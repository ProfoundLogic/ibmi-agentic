     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     h DFTACTGRP(*no) ACTGRP('CONNECT')
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR':'YAJL') DecEdit('0.')
     H*====================================================================
     H* Description:
     H*
     H*
     H*====================================================================
     H* inParms -
     H*   name:value
     H*
     H*====================================================================
     H* Created: 09/02/20 C:53 Project SY424
     H*                   C:33
     H*====================================================================
     H* Revisions:
     H*
     H* 02/28/25 C:062 Ticket: 118479(129)
     H*                Backout 08/06/23 change.
      *                                                               *
     H* 09/03/24 C:291 Project: WC2968
     H*                Add Failure Type: BIFFAILURE vs IVCNAMEADDR
     H*
      * 01/25/24 C:344 PROJECT: 133WTD                                *
      *               -Update subroutine to pass in the               *
      *                group# of the payment being processed.         *
      *               -Retrieve WMEXP record and verify if the group  *
      *                is flagged to separate 3rd party batches by    *
      *                group.                                         *
     H*                                                                    *
     H*  12/19/23 C:340 Project: WC2935                                    *
     H*                 Policy Rule - Failed BIF Notification              *
     H*
     H* 08/14/23 C:062 Ticket: 113384(129)
     H*                If invoice type is blank then determine type by
     H*                looking at the category.
     H*
     H* 08/06/23 C:062 Ticket: 113391(129)
     H*                Don't set online flag if CoveRisk
     H*
     H* 06/12/23 C:062 Project: WC2879
     H*                Add ProcessFailedBIFRequest Service.
     H*
     H* 11/15/22 C:079 Support: 104290
     H*                Check if called by S/R Online or IVC
     H*
     H* 07/29/22 C:340 Support: 103003
     H*                PayGo IVC : Seperate A/R Batches.
     H*
     H* 06/29/22 C:340 Support: WC2854
     H*                Invoice Cloud:  Retain Payment Ref#
     H*
     H* 10/19/21  C:33  Project: WC2663  98816
     H*                 Add processing for 3PRTPMTCX
     H*
     H* 05/21/21  c:62 Project WC2663
     H*                Add Invoice Cloud processing
     H*====================================================================

     Fwtabl     if   e           k disk    include(ptt01)
     Fwtc3l     if   e           k disk    include(pc301)
     F*** Begin Add *** 05/21/21 **********************************
     Fwdcdl     if   e           k disk    Rename(pcd01:pcd00)
     F*** End   Add *** 05/21/21 **********************************
      *** Beg Add ***** 01/25/24 *** 133WTD ********************************
     Fwmexl     if   e           k disk    rename(pex01:pex00)
      *** End Add ***** 01/25/24 *** 133WTD ********************************

     D* this size needs to change
     Darray            s               a   len(32767)
     Djson             s               a   len(8500000) varying
     DjsonSub          s               a   len(8500000) varying

     d* PgmStart variables
     d clientID        s              3a
     d intCo#          s              3  0
     d intUserCo#      s              3  0
     d strAuthority    s             10a
     d strCo#          s              3a
     d strEnv          s              4a
     d sysDate         s               d
     d sysDate7        s              7  0
     d sysTime6        s              6  0
     d isEnvQuote      s              1n
     d isEnvLive       s              1n
     d inPgm           s              1
     d rrnWDEAP_Hld    s              9  0
     D sqlStmtStr      s          32000a   varying inz

     d strErrorMsg     s            500a   Varying
     d i0              s             10i 0 inz
     d key             s             50a   varying
     d node0           s                   Like(yajl_val)
     d inJson          s               a   Len(2000000) varying
     d val             s                   Like(yajl_val)

     d inRequestAR     s            256
     d inUserAR        s             10
     d inParmsAR       s               a   len(8500000) varying
     d outJSONAR       s               a   len(8500000) varying
     d ioRtnCodeAR     s             10
     d*** Begin Add *** 06/12/23 *******************************************
     d clbBifData      s                   Sqltype(clob:64000)
     d*** End   Add *** 06/12/23 *******************************************
     d*** Beg Add *** 01/25/24 *** 133WTD *****************************
     d xxCo#           s                   Like(EXco#) inz
     d xxFnd           s                   Like(EXfnd) inz
     d flg3PYGrp       s              1    inz
     d*** End Add *** 01/25/24 *** 133WTD *****************************

     D PSDS          ESDS                  extname(SZQ1P)

     d CashDissolve    pr                  ExtPgm('WA055R')
     d  inPgm                         1
     d  rrnWDEAP_Hld                  9  0

     d GetClientID     pr                  ExtPgm('RTVCLTID')
     d  clientID                      3a

     dRtvEnv           pr                  extpgm('RTVENV')
     d outEnv                         4a

     dWISRPAY          pr                  extpgm('WISRPAY')
     d                              256    const
     d                               10    const
     d                                 a   const Len(8500000) Varying
     d                                 a         Len(8500000) Varying
     d                               10

     dPolicyAR         pr                  ExtPgm('WIAR')
     d inRequest                    256
     d inUser                        10
     d inParms                         a   len(8500000) varying
     d outJSON                         a   len(8500000) varying
     d ioRtnCode                     10


      /copy sPrAtrium
      /copy sPrDate
      /copy sPrFiles
      /Copy sPrJson
      /Copy sPrMessage
      /copy sPrSecure
      /copy sPrSql
      /copy sPrString     // 06/12/23
      /copy sPrTrigger
      /copy wPrAr
      /Copy wPrEmplr
      /copy wPrGroup
      /copy wPrUsrPrms
      //*** Beg Add *** 11/15/22 *******************************************
      /copy wPrPmtSvc
      //*** End Add *** 11/15/22 *******************************************
      /Include yajl_h

      *entry
     DRequestPgm       pr                  ExtPgm('WIPOLPMT')
     D                              256
     D                               10
     D                                 a   len(8500000) varying
     D                                 a   len(8500000) varying
     D                               10
     DRequestPgm       pi
     D inRequest                    256
     D inUser                        10
     D inParms                         a   len(8500000) varying
     D outJSON                         a   len(8500000) varying
     D ioRtnCode                     10

      /Free
       Exec Sql Set Option Commit = *None;
       Exsr PgmStart;

       Select;
       When inRequest = 'ListIncompleteInvoices';
          ListIncompleteInvoices();
       //*** Begin Add *** 06/12/23 ****************************************
       When inRequest = 'ProcessFailedBIFRequest';
          ProcessFailedBIFRequest();
       //*** End   Add *** 06/12/23 ****************************************
       When inRequest = 'ProcessPayment';
          ProcessPayment();
       When inRequest = 'ProcessThirdPartyPayment';
          ProcessThirdPartyPayment();
       EndSl;

       outJSON = json;
       *inlr = *on;

       //--------------------------------------------------------
       Begsr PgmStart;
       //--------------------------------------------------------
        strAuthority = ioRtnCode;
        Clear ioRtnCode;
        intCo# = UserCo#(inUser);
        intUserCo# = intCo#;
        strCo# = %Editc(intCo#:'X');
        GetClientID(clientID);
        sysDate  = %Date();
        sysDate7 = %Dec(sysDate:*Cymd);
        sysTime6 = %Dec(%Time():*hms);
        RtvEnv(strEnv);
        isEnvQuote = strEnv = '*QTE';
        isEnvLive = strEnv = '*LIV';

        CreateAliasSQL('WASRP_HLD':'WASRP':'HOLD':*on);
        CreateAliasSQL('WDEAP_HLD':'WDEAP':'HOLD':*on);

       Endsr;

      /End-free

      *=========================================================
      * Request: ListIncompleteInvoices
      * Description: Returns an array of Incomplete Invoices
      *=========================================================
     pListIncompleteInvoices...
     p                 b

     d first           s               n   Inz(*On)
     d intCo#          s              3  0
     d intGrp#         s              3  0
     d intEmp#         s              9  0
     d ix              s              9  0 Inz(0)
     d comma           s              1a   Inz
     d invoiceList     s           5000a   varying inz
     d strOrder        s            100a   varying inz
     d strSql          s           5000a   varying inz
     d strSql1         s           5000a   varying inz
     d strSql2         s           5000a   varying inz

     d dsIn            ds                  Qualified Inz
     d  co#                           3a
     d  group                         3a
     d  employer#                     9a

     D dsWDEAP         ds                  inz
     D   EAco#                        3  0
     D   EAfnd                        3  0
     D   EAemp#                       9  0
     D   EAdiv                        5  0
     D   EAinv#                      10s 0
     D   env                          1a

     D dsWASRP         ds                  inz
     D   SRco#                        3  0
     D   SRfnd                        3  0
     D   SRemp#                       9  0
     D   SRdiv                        5  0
     D   SRinv#                      10S 0
     D   SRefdt                       7S 0
     D   SRrpmy                       5S 0

      /Free
       Exsr GetParms;
       If ioRtnCode <> 'Error';
          Exsr BuildRunSql;
       Endif;
       json += ProcessErrorMessagesJson('*PRE');
       outJSON = json;
       Return;

       //==================================================
       // Build SQLs
       //==================================================
       Begsr BuildRunSql;

       // Build from A/R file
          json += ',"incompleteInvoices":[';
          strSQL1 = 'Select EAco#, EAfnd, EAemp#, EAdiv, EAinv#, ''Q'' ' +
             'From ' + %Trim(RtvQuoteLib('*QTE')) + '/WDEAP_HLD ' +
             'Where EAinv# <> 0 And EAeatp <> ''IP''';
          strSQL2 = 'Select EAco#, EAfnd, EAemp#, EAdiv, EAinv#, '' '' ' +
             'From ' + %Trim(RtvQuoteLib('*LIV')) + '/WDEAP_HLD ' +
             'Where EAinv# <> 0 ';
          If intGrp# <> 0;
             strSQl1 += ' And EAfnd = ' + %EditC(intGrp#:'X');
             strSQl2 += ' And EAfnd = ' + %EditC(intGrp#:'X');
          EndIf;
          If intEmp# <> 0;
             strSQl1 += ' And EAemp# = ' + %EditC(intEmp#:'X');
             strSQl2 += ' And EAemp# = ' + %EditC(intEmp#:'X');
          EndIf;
          strOrder = 'Order By EAco#, EAfnd, EAemp#, EAdiv, EAinv# ';
          strSql = %Trim(strSQL1) + ' union ' + %Trim(strSQL2) + ' ' +
                   %Trim(strOrder);

          Exec SQL close invCursor;

          // Prepare the SQL statement
          Exec SQL prepare invStmt from :strSQL;

          // Declare the SQL cursor
          Exec SQL declare invCursor cursor for invStmt;

          // Open cursor
          Exec SQL open invCursor;

          Reset dsWDEAP;

          DoU Not IsValidFetch(sqlstt);
             ix = ix + 1;
             If ix > 1;
                comma = ',';
             Endif;
             Exec SQL fetch next from invCursor into :dsWDEAP;
             If IsValidFetch(sqlstt);
                json += %Trim(comma) + '{' +
                BuildJsonNode('company':%EditC(EAco#:'X')) + ',';
                json += BuildJsonNode('group':%EditC(EAfnd:'X')) + ',';
                json += BuildJsonNode('employer':%EditC(EAemp#:'X')) + ',';
                json += BuildJsonNode('division':%EditC(EAdiv:'X')) + ',';
                json += BuildJsonNode('invoice#':%EditC(EAco#:'X') +
                %EditC(EAfnd:'X') + %EditC(EAemp#:'X') +
                %EditC(EAdiv:'X') +
                %EditC(EAinv#:'X')) + '}';
                //***%subst(%EditC(EAinv#:'X'):6)) + '}';
            Else;
               Leave;
            Endif;
          EndDo;
        //json = %scanrpl(',':'':json:%len(json)-1);
          json += ']';

       Endsr;

       //========================================================
       // Break parms out of JSON
       //========================================================
       Begsr GetParms;

          inJson = inParms;
          Reset i0;
          node0 = yajl_string_load_tree(inJson:strErrorMsg);
          If strErrorMsg = ' ';
             json = json + ' "requestParms" : {';
             Dow YAJL_object_loop(node0:i0:key:val);
                BuildGiantJSON(json:key:YAJL_get_string(val));
                Select;
                When key = 'company';
                   dsIn.co# = YAJL_get_string(val);
                When key = 'group';
                   dsIn.group = YAJL_get_string(val);
                When key = 'employer';
                   dsIn.employer# = YAJL_get_string(val);
                Endsl;
             Enddo;

             Yajl_Tree_Free(node0);
             json = json + '}';
             Monitor;
                intCo# = %int(dsIn.co#);
             On-error;
                intCo# = UserCo#(inUser);
             Endmon;
             dsIn.co# = %char(intCo#);

             Monitor;
                intGrp# = %int(dsIn.group);
             On-error;
                intGrp# = 0;
             Endmon;

             Monitor;
                intEmp# = %int(dsIn.employer#);
             On-error;
                intEmp# = 0;
             Endmon;

          Else;
             AddErrorMessageJson('JSON filter can''t be read.':ioRtnCode);
          Endif;

       Endsr;

     pListIncompleteInvoices...
     p                 e
     P*** Begin Add *** 06/12/23 ****************************************
      *=========================================================
      * Request: ProcessFailedBIFRequest
      * Description: Process Failed BIF Request
      *=========================================================
     PProcessFailedBIFRequest...
     P                 b

     d badData         s               n

     d chrDataRequest  s               a   Len(2000000) varying
     d chrMaintUser    s             10a
     d chrMaintPgm     s             10a
     d chrMessage      s            100a
     d chrResubmit     s              1a

     d intCo#          s              3  0
     d intMaintDate    s              7  0
     d intMaintTime    s              6  0
     d intTrDt7        s              7  0
     d intTrDt8        s              8  0
     d intTrTm6        s              6  0

     d end             s              7  0
     d len             s              7  0
     d str             s              7  0

     d dsIn            ds                  Qualified Inz
     d  inCo#                         3a
     d  inTransDate                   8a
     d  inTransTime                   8a
     d  inMessage                   100a
     d  inDataRequest                  a   Len(2000000) varying
      *** Beg Add ***** 09/03/24 ************************************
     d  inFailureTyp                 50a
      *** End Add ***** 09/03/24 ************************************
     d*** Beg Add *** 12/19/23 *** WC2935 ***************************
     d dsWIBIFP      e ds                  ExtName(WIBIFP) Inz qualified
     d chrTrgName      s             10a
     d chrCo#          s              3a
     d chrFnd          s              3a
     d wibifPTR        s               *
     d*** End Add *** 12/19/23 *** WC2935 ***************************
      *** Beg Add ***** 09/03/24 ************************************
     d chrFailureTyp   s             50a
      *** End Add ***** 09/03/24 ************************************

      /Free
       Exsr GetParms;
       If ioRtnCode <> 'Error';
          Exsr CreateFailedBIF;
       EndIf;

       json += ProcessErrorMessagesJson('*PRE');
       json = %scanrpl(',':'':json:1:1);
       outJSON = json;
       Return;

       //---------------------------------------------------
       Begsr CreateFailedBIF;
       //---------------------------------------------------

          //Add record to Failed BIF File
          chrResubmit    = 'E';
          If %Scan('CONNECT':upper(dsIn.inMessage)) <> 0;
             chrResubmit    = ' ';
          EndIf;

          chrDataRequest  = dsIn.inDataRequest;
          chrMessage      = dsIn.inMessage;
          intMaintDate    = DateTo7(systemDate());
          intMaintTime    = TimeTo6(systemTime());
          chrMaintUser    = Q1user;
          chrMaintPgm     = Q1pgm;
          Clear clbBifData;
          clbBifData_Data = %Trim(chrDataRequest);
          clbBifData_Len  = %Len(%Trim(chrDataRequest));
          //*** Beg Add ***** 09/03/24 ******************************
          chrFailureTyp = dsIn.inFailureTyp;
          //*** End Add ***** 09/03/24 ******************************

       // *** Beg Chg *** 12/19/23 *** WC2935 ***************************
        //Exec Sql Insert Into WIBIFP
        //  Values(:intCo#, :intTrDt7, :intTrTm6, :chrResubmit, :chrMessage,
        //         'E', :intMaintDate, :intMainttime, :chrMaintUser,
        //         :chrMaintPgm, :clbBifData);

          //*** Beg Chg ***** 09/03/24 ******************************
          //***Exec Sql Insert Into WIBIFP
          //***  Values(:intCo#, :intTrDt7, :intTrTm6, :chrResubmit, :chrMessage,
          //***         'E', :intMaintDate, :intMainttime, :chrMaintPgm,
          //***         :chrMaintUser, :clbBifData);
          //*** Begin Chg *** 10/06/25 *************************************
          //*** Exec Sql Insert Into WIBIFP
          //Values(:intCo#, :intTrDt7, :intTrTm6, :chrResubmit, :chrMessage,
          //***    'E', :intMaintDate, :intMainttime, :chrMaintPgm,
          //***    :chrMaintUser, :clbBifData, :chrFailureTyp);
          Exec Sql Insert Into WIBIFP
            Values(:intCo#, :intTrDt7, :intTrTm6, :chrResubmit, :chrMessage,
                   'E', :intMaintDate, :intMainttime, :chrMaintPgm,
                   :chrMaintUser, :chrFailureTyp, :clbBifData);
          //*** End   Chg *** 10/06/25 *************************************
          //*** End Chg ***** 09/03/24 ******************************
       // *** End Chg *** 12/19/23 *** WC2935 ***************************
       // *** Beg Add *** 12/19/23 *** WC2935 ***************************

          //*** Beg Chg ***** 09/03/24 *******************************
          //***Exec Sql Select * Into :dsWIBIFP
          //***         From WIBIFP
          //***         Where BICO#   = :intCo#       and
          //***               BITRDT  = :intTrDt7     and
          //***               BITRTM  = :intTrTm6     and
          //***               BIRESUB = :chrResubmit  and
          //***               BIMSG   = :chrMessage   and
          //***               BISTAT  = 'E'           and
          //***               BIDT    = :intMaintDate and
          //***               BITIME  = :intMainttime and
          //***               BIPGM   = :chrMaintPgm  and
          //***               BIUSER  = :chrMaintUser ;

          //***chrTrgName = 'BIFFAILURE';
          Exec Sql Select * Into :dsWIBIFP
                   From WIBIFP
                   Where BICO#   = :intCo#       and
                         BITRDT  = :intTrDt7     and
                         BITRTM  = :intTrTm6     and
                         BIRESUB = :chrResubmit  and
                         BIMSG   = :chrMessage   and
                         BISTAT  = 'E'           and
                         BIDT    = :intMaintDate and
                         BITIME  = :intMainttime and
                         BIPGM   = :chrMaintPgm  and
                         BIUSER  = :chrMaintUser and
                         BITYPE  = :chrFailureTyp;
          Select;
          When chrFailureTyp = 'Name Change';
             chrTrgName = 'IVCNAMADDR';
          Other;
             chrTrgName = 'BIFFAILURE';
          Endsl;
          //*** End Chg ***** 09/03/24 **********************************

          chrCo#     = %EditC(intCo#:'X');
          chrFnd     = '000';
          wibifPtr   = %Addr(dsWIBIFP);

          ProcessTrigger(chrTrgName:chrCo#:chrFnd:wibifPtr);

       // *** End Add *** 12/19/23 *** WC2935 ***************************

       Endsr;

       //---------------------------------------------------
       Begsr GetParms;
       //---------------------------------------------------
        inJson = inParms;
        Reset i0;
        node0 = yajl_string_load_tree(inJson:strErrorMsg);
        //*** Beg Add ***** 09/03/24 ******************************
        Clear dsIn.inFailureTyp;
        //*** End Add ***** 09/03/24 ******************************
        If strErrorMsg = ' ';
           json += ' "requestParms" : {';
           Dow YAJL_object_loop(node0:i0:key:val);
              If key <> 'WSDataQueueRequest';
                 BuildGiantJSON(json:key:YAJL_get_string(val));
              EndIf;
              Select;
              When key = 'company';
                 dsIn.inco# = YAJL_get_string(val);
              When key = 'transactionDate';
                 dsIn.inTransDate = YAJL_get_string(val);
              When key = 'transactionTime';
                 dsIn.inTransTime = YAJL_get_string(val);
              When key = 'message';
                 dsIn.inMessage = YAJL_get_string(val);
              When key = 'WSDataQueueRequest';
                 // get starting postion, end position and length
                 inJson = %scanrpl(X'25':'':inJson);
                 inJson = %scanrpl(X'0D':'':inJson);
                 str = %Scan('"WSDataQueueRequest":':inJson) + 21;
                 end = %Len(%Trim(inJson));
                 len = end - str;
                 dsIn.inDataRequest = %Trim(%Subst(inJson:str:len));
                 BuildGiantJSON(json:key:dsIn.inDataRequest);
              //*** Beg Add ***** 09/03/24 ******************************
              When key = 'failureType';
                 dsIn.inFailureTyp = YAJL_get_string(val);
              //*** End Add ***** 09/03/24 ******************************
              Endsl;
           Enddo;
           Yajl_Tree_Free(node0);
           json += '}';
        Else;
           AddErrorMessageJson('JSON filter can''t be read.':ioRtnCode);
        Endif;

        If strAuthority = '*View';
           AddErrorMessageJson('User, ' + %trim(inUser) +
            ', not authorized to process ' +
            'request (' + %trim(inRequest) + ').'
            :ioRtnCode);
        Endif;

        // Load Company#
        If dsIn.inCo# <> ' ';
           intCo# = %Int(dsIn.inCo#);
        Else;
           intCo# = UserCo#(inUser);
        Endif;

        // Load Transaction Date
        If dsIn.inTransDate = ' ';
           AddErrorMessageJson('Transaction Date  not passed in':ioRtnCode);
        Else;
           badData = *Off;
           Monitor;
              intTrDt8 = %Int(dsIn.inTransDate);
              intTrDt7 = dtCYMD(intTrDt8);
           On-Error;
              AddErrorMessageJson('Transaction Date is not a ' +
                                  'valid number':ioRtnCode);
              badData = *On;
           EndMon;

           If Not badData And intTrDt7 = 0;
              AddErrorMessageJson('Invalid Transaction Date':ioRtnCode);
           EndIf;
        Endif;

        // Load Transaction Time
        If dsIn.inTransTime = ' ';
           AddErrorMessageJson('Transaction Time not passed in':ioRtnCode);
        Else;
           Monitor;
              intTrTm6 = %Int(dsIn.inTransTime);
           On-Error;
              AddErrorMessageJson('Transaction Time is not a ' +
                                  'valid number':ioRtnCode);
           EndMon;
        Endif;

        // Load Message
        If dsIn.inMessage = ' ';
           AddErrorMessageJson('Message not passed in':ioRtnCode);
        Endif;

        // Load Data Request
        If dsIn.inDataRequest = ' ';
           AddErrorMessageJson('Data Request not passed in':ioRtnCode);
        Endif;

        If ioRtnCode = 'Error';
           LeaveSR;
        Endif;

       Endsr;

     PProcessFailedBIFRequest...
     P                 e
     P*** End   Add *** 06/12/23 ****************************************
      *=========================================================
      * Request: ProcessPayment
      * Description: Process payment
      *=========================================================
     PProcessPayment...
     P                 b

     d intCo#          s              3  0
     d intFnd          s              3  0
     d intEmp#         s              9  0
     d intDiv          s              5  0
     d intFyr          s              3  0
     d intAmt          s              9  2
     d intRRNWdelp     s              9  0
     d intBatch#       s              9  0
     d xxCxDate        s              7s 0
     d rcdCount        s             10i 0 inz
     d NextInv#        s             10  0
     d ChrPremARcode   s              2a
     d strBatchSysCd   s              2a
     d skipJSON        s              1n   Inz
     d badAmount       s              1n   Inz(*Off)
     d*** Begin Add *** 05/21/21 **********************************
     d inQte           s              1a
     d*** End   Add *** 05/21/21 **********************************

     d dsIn            ds                  Qualified Inz
     d  Inco#                         3a
     d  Infnd                         3a
     d  Inemp#                        9a
     d  Indiv                         5a
     d  Inyear                        3a
     d  invoice#                     10  0
     d  amount                       11a
       //*** Beg Add ***** 06/29/22 **********************************
     d  reference#                   40a
       //*** End Add ***** 06/29/22 **********************************

     D dsWDEAP_hld     ds                  inz
     D   EAco#                        3  0
     D   EABAT#                       9  0
     D   EABATA                      11  2
     D   EAFND                        3  0
     D   EAFYR                        3  0
     D   EAEMP#                       9  0
     D   EADIV                        5  0
     D   EAARCD                       2a
     D   EADESC                      30a
     D   EAPODT                       7  0
     D   EACHK#                       7  0
     D   EACKAM                       9  2
     D   EAAMT                        9  2
     D   EASYCD                       2a
     D   EATFAP                       1a
     D   EATFFL                       1a
     D   EATFDT                       7  0
     D   EATFAM                      11  2
     D   EASTAT                       1a
     D   EAUSER                      10a
     D   EAPGM                       10a
     D   EATIME                       6  0
     D   EADT                         7  0
     D   EAINV#                      10  0
     D   EATRDT                       7  0
     D   EALATE                       1a
     D   EABB#                       15a
     D   EAACHF                       1a
     D   EAACHD                       7  0
     D   EAACHT                       6  0
     D   EAACHU                      10a
     D   EABBDT                       7  0
     D   EAIUSR                      10a
     D   EAIPGM                      10a
     D   EAIDT                        7  0
     D   EACTYP                       1a
     D   EACASH                       1a
     D   EARDIV                       5  0
     D   EAAUTR                       1a
     D   EAONLI                       1a
     D   EAEATP                       2a
     D   EAFL01                       1a
     D   EACOLEFDT                    7  0
     D   EACOLEXDT                    7  0
     D   EACOLPERC                    5  2
     D   EACOLLFLG                    1a
     D   EASRRPTDT                    7  0
     D   EASRMANPR                   11  2
     D   EATRANSID                   32a
       //*** Beg Add ***** 06/29/22 **********************************
     D   EAINVTYPID                  15a
     D   EARef#                      40a
       //*** End Add ***** 06/29/22 **********************************
      //*** Beg Add ***** 09/03/24 **********************************
     D   EABAT2                       9  0
      //*** End Add ***** 09/03/24 **********************************

      /Free
       Exsr GetParms;
       If ioRtnCode <> 'Error';
          Exsr CreateHoldingBatch;
          If skipJSON = *Off;
             json += ProcessErrorMessagesJson('*PRE');
             outJSON = json;
          Endif;
          Exsr BuildOutput;
       Endif;
       Return;
       //---------------------------------------------------
       Begsr BuildOutput;
       //---------------------------------------------------
        json += '{' +
         BuildJsonNode('invoice#':%EditC(EAco#:'X') +
          %EditC(EAfnd:'X') + %EditC(EAemp#:'X') +
          %EditC(EAdiv:'X') +
          %subst(%EditC(EAinv#:'X'):6)) + '},';   // The invoice # is a 10A
       Endsr;

       //---------------------------------------------------
       Begsr CreateHoldingBatch;
       //---------------------------------------------------
       //Check for a pending payment
       //Check for rcd in WDEAP_HLD
       Exec sql
          select count(*) into :rcdCount
          from WDEAP_HLD
          where EAco# = :Intco# and EAfnd = :Intfnd and
             EAemp# = :Intemp# and EAdiv = :IntDiv and EAinv# = :NextInv#;
       // If found, pending
       If rcdCount <> 0;
          AddErrorMessageJson('Pending payments in WDEAP_HLD.':ioRtnCode);
          LeaveSr;
       Endif;

       //Get next invoice#
       NextInv# =
          GetNext3rdPartySeq#(
          %EditC(intco#:'X')  +
          %EditC(intfnd:'X')  +
          %EditC(intemp#:'X') +
          %EditC(intdiv:'X')
          );

        Exec Sql
         Select SYco#, SYkey
         Into :intCo#, :strBatchSysCd
         From WTSYP
         Join SMCOP
          On SYco# = COco#
         Where SYstat <> 'I'
          And SYtype = 'Y'
         Order by SYkey
         Limit 1;
        EAco#  = intCo#;
        EAsycd = strBatchSysCd;
        EAbat# = GetBatch#;
        intBatch# = EAbat#;
        EAfnd  = intFnd;
        EAemp# = intEmp#;
        EAdiv  = intDiv;
        EAfyr  = intFyr;
        EAamt  = intAmt;
        EAtrdt = %Dec(%Date():*cymd);
        EApodt = %Dec(%Date():*cymd);
        EAarcd = chrPremArCode;
        Chain (intCo#:chrPremArCode) ptt01;
        EAdesc = TTdesc;
        EActyp = 'P';
        EAchk# = 0;
        EAckam = 0;
        EAinv# = nextInv#;
        EArdiv = EAdiv;
        EAtfap = 'N';
        EAtffl = 'N';
        EAidt  = EAtrdt;
        EAdt   = EAidt;
        EAtime = %Dec(%Time():*hms);
        EAiusr = Inuser;
        EAuser = inuser;
        EAipgm = Q1pgm;
        EApgm  = Q1pgm;
       //*** Beg Add ***** 06/29/22 **********************************
        EAref# = dsIn.reference#;
       //*** End Add ***** 06/29/22 **********************************
       // Write pea0p; //wdeap_hld
        Exec Sql
        Insert into WDEAP_HLD values(:dsWDEAP_hld);

       // update batch total after a/r record is written
        If intBatch# <> 0;
           Exec Sql
           Update WDEAP_HLD
           Set EAbata =
              (Select Sum(EAamt)
              From WDEAP_HLD
              Where EAbat# = :intBatch#)
              Where EAbat# = :IntBatch#;
         Endif;
         If Not IsValidFetch(sqlStt);
            AddErrorMessageJson('SQL: Insert to WDEAP_HLD failed.':
            ioRtnCode);
            Return;
         Endif;

       Endsr;

       //---------------------------------------------------
       Begsr GetParms;
       //---------------------------------------------------
        inJson = inParms;
        Reset i0;
        node0 = yajl_string_load_tree(inJson:strErrorMsg);
        If strErrorMsg = ' ';
           json += ' "requestParms" : {';
           Dow YAJL_object_loop(node0:i0:key:val);
              BuildGiantJSON(json:key:YAJL_get_string(val));
              Select;
              When key = 'company';
                 dsIn.inco# = YAJL_get_string(val);
              When key = 'group';
                 dsIn.infnd = YAJL_get_string(val);
              When key = 'employer';
                 dsIn.inemp# = YAJL_get_string(val);
              When key = 'division';
                 dsIn.indiv = YAJL_get_string(val);
              When key = 'year';
                 dsIn.inyear = YAJL_get_string(val);
              When key = 'amount';
                 dsIn.amount = YAJL_get_string(val);
        //*** Beg Add ***** 06/29/22 **********************************
              When key = 'referenceNumber';
                 dsIn.reference#  = YAJL_get_string(val);
        //*** End Add ***** 06/29/22 **********************************
              Endsl;
           Enddo;
           Yajl_Tree_Free(node0);
           json += '}';
        Else;
           AddErrorMessageJson('JSON filter can''t be read.':ioRtnCode);
        Endif;
        If strAuthority = '*View';
           AddErrorMessageJson('User, ' + %trim(inUser) +
            ', not authorized to process ' +
            'request (' + %trim(inRequest) + ').'
            :ioRtnCode);
        Endif;

        // Load Company#
        If dsIn.inCo# <> ' ';
           intCo# = %Int(dsIn.inCo#);
        Endif;

        // Load Group #
        If dsIn.inFnd = ' ';
           AddErrorMessageJson('Group not passed in':ioRtnCode);
        Else;
           Monitor;
              intFnd = %Int(dsIn.inFnd);
           On-Error;
              AddErrorMessageJson('Group is not a valid number':ioRtnCode);
           EndMon;
        Endif;

        // Load Employer #
        If dsIn.inEmp# = ' ';
           AddErrorMessageJson('Employer# not passed in':ioRtnCode);
        Else;
           Monitor;
              intEmp# = %Int(dsIn.inEmp#);
           On-Error;
              AddErrorMessageJson('Employer# is not a valid number':ioRtnCode);
           EndMon;
        Endif;

        // Load Division
        If dsIn.inDiv = ' ';
           AddErrorMessageJson('Division not passed in':ioRtnCode);
        Else;
           Monitor;
              intDiv = %Int(dsIn.inDiv);
           On-Error;
              AddErrorMessageJson('Division is not a valid number':ioRtnCode);
           EndMon;
        Endif;

        // Load Year
        If dsIn.inYear = ' ';
           AddErrorMessageJson('Year not passed in':ioRtnCode);
        Else;
           Monitor;
              intFyr = %Int(dsIn.inYear);
           On-Error;
              AddErrorMessageJson('Year is not a valid number':ioRtnCode);
           EndMon;
        Endif;

        // Load Amount
        If dsIn.amount = ' ';
           AddErrorMessageJson('Amount not passed in':ioRtnCode);
        Else;
           badAmount = *Off;
           Monitor;
              intAmt = %Int(dsIn.amount);   //9,2
           On-Error;
              AddErrorMessageJson('Amount is not a valid number':ioRtnCode);
              badAmount = *On;
           EndMon;
           If Not badAmount and intAmt = 0;
              AddErrorMessageJson('Amount is zero':ioRtnCode);
           Endif;
        Endif;

        If ioRtnCode = 'Error';
           LeaveSR;
        Endif;

        //*** Begin Add *** 05/21/21 **********************************
        //  Is this Quote or Live
        inQte = ' ';
        If isEnvQuote;
           inQte = 'Q';
        EndIf;

        // If a new claim use new ar code
        If Not ChkRenewal(intCo#:intFnd:intEmp#:intFyr:inQte);
           Chain (intCo#:intFnd) pcd00;
           If Not %Found(wdcdl);
              CDnbCd = ' ';
           EndIf;
        EndIf;
        //*** End   Add *** 05/21/21 **********************************

       // Ensure cash is set up.
        Chain (intCo#:intFnd) pc301;
        chrPremArCode = C3cpcd;
        //*** Begin Add *** 05/21/21 **********************************
        If CDnbCd <> *Blanks;
           chrPremArCode = CDnbCd;
        EndIf;
        //*** End   Add *** 05/21/21 **********************************
        If chrPremArCode = *Blanks;
           AddErrorMessageJson('Cash dissolve premium A/R code is not set up');
        EndIf;

        If ValidEmpYr(intFyr:intEmp#:intDiv:intrrnWDELP:
         intFnd:intCo#) = *Off;
           AddErrorMessageJson('Policy cannot be found.');
           ioRtnCode = 'Error';
           Return;
        Endif;

       Endsr;

     PProcessPayment...
     P                 e
      *=========================================================
      * Request: ProcessThirdPartyPayment
      * Description: Process payment from 3RD PARTY
      *=========================================================
     PProcessThirdPartyPayment...
     P                 b

     d decCount        s              5  0 inz
     d decCountHld     s              5  0 inz
     d decPeriod       s              8  0 inz
     d arCode          s              2
     d dpCode          s              2
     d rpCode          s              2
     d dt5             s              5  0 inz
     d dt6             s              6  0 inz
     d dt7             s              7  0 inz
     d dt7From         s              7  0 inz
     d dt7To           s              7  0 inz
     d dt8From         s              8  0 inz
     d dt8To           s              8  0 inz(99999999)
     d intFnd          s              3  0 inz
     d intBat#         s              9  0 inz
     d isEofClassList  s              1n
     d polYear         s              3  0
     d skipJSON        s              1n   Inz
     d strBatchSysCd   s              2a
     d strFile         s             25a   varying inz
     d strNotificationType...
     d                 s            256    varying inz
     d strPeriod       s             10a   varying inz
     d strSql          s           5000a   varying inz
     d strSqlOrder     s            500a   varying inz
     d strSqlSelect    s           1000a   varying inz
     d strSqlWhere     s           5000a   varying inz
     d strSqlStatement...
     d                 s           5000a   varying inz
     d strStatus       s              1a   inz
     d decDate8        s              8s 0
     d arCount         s              5s 0
     d arAmount        s             15s 2
     d chrDate8        s              8a
     d chrTime6        s              6a
     d chrDateTime     s             14a
     d account#        s             30a
     d chrTrgName      s             10a
     d chrCo#          s              3a
     d chrFnd          s              3a
     d***  Begin Add  ***  10/21/19  *******************
     d ptrWMEAP        s               *
     d dsWMEAP       e ds                  extname(WMEAP) inz qualified
     d***  End   Add  ***  10/21/19  *******************
     d wdeap_HldPtr    s               *
     d dsWDEAP_HLD   e ds                  ExtName(WDEAP_HLD) inz
     d wdeap_hldFound...
     d                 s               n
     d decPaymentAmount...
     d                 s              9  2
     d*** Begin Add *** 06/29/22 *******************************************
     d chrRef#         s             40a
     d*** End   Add *** 06/29/22 *******************************************
     d*** Begin Add *** 08/14/23 *******************************************
     d holdEAcTyp      s              1a
     d*** End   Add *** 08/14/23 *******************************************

     d dsIn            ds                  Qualified Inz
     d  status                       25a   Varying
     d  invoiceNumber                30a
     d  co#                           3  0 Overlay(invoiceNumber)
     d  group                         3  0 Overlay(invoiceNumber:4)
     d  emp#                          9  0 Overlay(invoiceNumber:7)
     d  div                           5  0 Overlay(invoiceNumber:16)
     d  inv#                         10  0 Overlay(invoiceNumber:21)
     d  type                         20a
     d  paymentAmount                 9a
       //*** Beg Add ***** 06/29/22 **********************************
     d  reference#                   40a
       //*** End Add ***** 06/29/22 **********************************

      /Free

       Exsr GetParms;
       If ioRtnCode <> 'Error';
          Exsr ProcessInvoice;
       EndIf;

       If skipJSON = *Off;
          json += ProcessErrorMessagesJson('*PRE');
          outJSON = json;
       Endif;

       Return;

       //========================================================
       // Call Cash Dissolve (WA055R)
       //====================================================
       Begsr CallCashDissolve;
        //*** Begin Add *** 08/14/23 ***************************************
        //  If invoice type is blank,look at the category to determine the
        //  type by looking at the category
        If dsIn.type = ' ';
           Select;
           When holdEAcTyp = 'R';
              dsIn.type = 'deductible';
           When holdEAcTyp = 'C';
              dsIn.type = 'retro';
           Other;
              dsIn.type = 'billing';
           EndSl;
        EndIf;
        //*** End   Add *** 08/14/23 ***************************************
        Select;
        When dsIn.type = 'billing';
           inPgm = 'B';
        When dsIn.type = 'retro'
         Or dsIn.type = 'deductible';
           inPgm = 'I';
        Endsl;
        CashDissolve(inPgm:rrnWDEAP_Hld);
       Endsr;
       //========================================================
       // Generate auto diary
       //========================================================
       Begsr GenerateAutoDiary;

        If dsIn.type =  'quote billing';
           strSqlStatement = 'Select * from ' +
            %Trim(RtvQuoteLib('*QTE')) + '/WDEAP_HLD ' +
            'Where EAco# = ' + %Editc(dsIn.co#:'X') + ' ' +
            'And EAfnd = '   + %Editc(dsIn.group:'X') + ' ' +
            'And EAemp# = '  + %Editc(dsIn.emp#:'X') + ' ' +
            'And EAdiv = '   + %Editc(dsIn.div:'X')  + ' ' +
            'And EAinv# = '  + %Editc(dsIn.inv#:'X') + ' Limit 1';
           Exec Sql Close cursorA01;
           Exec Sql Prepare A01 From :strSqlStatement;
           Exec Sql Declare cursorA01 cursor for A01;
           Exec Sql Open cursorA01;
           Exec Sql fetch next from cursorA01 into :dsWDEAP_HLD;

           chrTrgName   = 'QTE3PRTPMT';
           chrCo#       = %EditC(dsIn.co#:'X');
           chrFnd       = %EditC(dsIn.group:'X');
           wdeap_HldPtr = %Addr(dsWDEAP_HLD);

           ProcessTrigger(chrTrgName:chrCo#:chrFnd:wdeap_HldPtr);
        EndIf;

        //***  Begin Add  ***  10/19/21  *************
        If ChkCxStatus(dsIn.co#:dsIn.group:dsIn.emp#:dsIn.div);
           Reset dsWMEAP;
           Exec sql
            select * into :dsWMEAP
             from WMEAP
             where EAco# = :dsIn.co# and EAfnd = :dsIn.group
              and EAemp# = :dsIn.emp# and EAdiv = :dsIn.div
              and EAnInv = :dsIn.inv#
             limit 1;
           chrCo#   = %editC(dsIn.co#:'X');
           chrFnd   = %editC(dsIn.group:'X');
           ptrWMEAP = %addr(dsWMEAP);
           ProcessTrigger('3PRTPMTCX':chrCo#:chrFnd:ptrWMEAP);
        Endif;
        //***  End   Add  ***  10/19/21  *************

       Endsr;

       //========================================================
       // Break parms out of JSON
       //========================================================
       Begsr GetParms;

        inJson = inParms;
        Reset i0;
        node0 = yajl_string_load_tree(inJson:strErrorMsg);

        If strErrorMsg <> *blanks;
           AddErrorMessageJson('JSON filter can''t be read.':ioRtnCode);
           Leavesr;
        Endif;

        json += ' "requestParms" : {';
        Dow YAJL_object_loop(node0:i0:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'status';
              dsIn.status = YAJL_get_string(val);
           When key = 'invoiceNumber';
              dsIn.invoiceNumber = YAJL_get_string(val);
           When key = 'invoiceType';
              dsIn.type = YAJL_get_string(val);
           When key = 'paymentAmount';
              dsIn.paymentAmount = YAJL_get_string(val);
              decPaymentAmount = %Dec(dsIn.paymentAmount:9:2);
           //*** Beg Add ***** 06/29/22 *******************************
           When key = 'referenceNumber';
              dsIn.reference#  = YAJL_get_string(val);
              chrRef#          = dsIn.reference#;
           //*** End Add ***** 06/29/22 ********************************
           Endsl;
        Enddo;

        json += '}';
        YAJL_tree_free(node0);

        Exsr ValidateParms;

       Endsr;

       //========================================================
       // Insert record into WDELP_HLD
       //========================================================
       Begsr InsertWDEAP_HLD;

          // Retieve the AR codes from Group auxiliary
          Exec SQL Select F2rpCd, F2dpCd Into :rpCode, :dpCode
             From WDF2P Where F2co# = :dsIn.co# And F2fnd = :dsIn.group;

          // Set the AR Code
          Select;
          When dsIn.type = 'billing';
            arCode = ' ';
          When dsIn.type = 'retro';
            arCode = rpCode;
          When dsIn.type = 'deductible';
            arCode = dpCode;
          EndSl;

          // Don't Copy to WDEAP_HLD without a AR code
          If (dsIn.type = 'retro' Or dsIn.type = 'deductible') And
             (arCode = ' ');
             LeaveSR;
          EndIf;

          // Read thru the aggregate of WMEAP by fyr and total amount
          sqlStmtStr = 'Select EAfyr, Sum(EAamt) From wmeap ' +
                  'Where EAco#='  + %EditC(dsIn.co#:'X')  +  ' And ' +
                        'EAfnd='  + %EditC(dsIn.group:'X')  +  ' And ' +
                        'EAemp#=' + %EditC(dsIn.emp#:'X') +  ' And ' +
                        'EAdiv='  + %EditC(dsIn.div:'X')  +  ' And ' +
                        'EAninv=' + %EditC(dsIn.inv#:'X') + ' ' +
                        'Group by EAfyr Order by EAfyr';
          Exec Sql Prepare sqlWMEAP From :sqlStmtStr;
          Exec SQL Declare cursorWMEAP Cursor For sqlWMEAP;
          Exec SQL Open cursorWMEAP;

          Dou Not IsValidFetch(sqlStt);
             Exec SQL fetch next from cursorWMEAP into :polYear, :arAmount;
             If IsValidFetch(sqlStt);
        //*** Beg Chg ***** 06/29/22 **********************************
        //***   // Create WDEAP_HLD Record
        //***   Exec Sql Insert Into WDEAP_HLD
        //***   (EAco#, EAfnd, EAfyr, EAemp#, EAdiv, EAarCd, EApoDt, EAamt,
        //***    EAuser, EApgm, EAtime, EAdt, EAinv#, EAtrDt, EAiUsr,
        //***    EAiPgm, EAiDt, EAsycd, EAbat#) Values
        //***   (:dsIn.co#, :dsIn.group, :polYear, :dsIn.emp#, :dsIn.div,
        //***    :arCode, :sysDate7, :arAmount, :Q1user, :Q1pgm, :sysTime6,
        //***    :sysDate7, :dsIn.inv#, :sysDate7, :Q1user, :Q1pgm,
        //***    :sysDate7, :strBatchSysCd, :intBat#);
                // Create WDEAP_HLD Record
                Exec Sql Insert Into WDEAP_HLD
                (EAco#, EAfnd, EAfyr, EAemp#, EAdiv, EAarCd, EApoDt, EAamt,
                 EAuser, EApgm, EAtime, EAdt, EAinv#, EAtrDt, EAiUsr,
                 EAiPgm, EAiDt, EAsycd, EAbat#, EAref#) Values
                (:dsIn.co#, :dsIn.group, :polYear, :dsIn.emp#, :dsIn.div,
                 :arCode, :sysDate7, :arAmount, :Q1user, :Q1pgm, :sysTime6,
                 :sysDate7, :dsIn.inv#, :sysDate7, :Q1user, :Q1pgm,
                 :sysDate7, :strBatchSysCd, :intBat#, :dsIn.reference#);
        //*** End Chg ***** 06/29/22 **********************************
                // Get the RRN
               Exec Sql Select rrn(wdeap_hld) Into :rrnWDEAP_HLD
                        From WDEAP_HLD
                        Where EAco#=:dsIn.co# And EAfnd=:dsIn.group And
                              EAemp#=:dsIn.emp# And EAdiv=:dsIn.div And
                              EAinv#=:dsIn.inv# And EAdt=:sysDate7 And
                              EAtime=:sysTime6 And EAuser=:Q1user and
                              EApgm=:Q1pgm And EAarCd=:arCode;
                // Run Cash Dissolve
                ExSr CallCashDissolve;
             EndIf;
          EndDo;

       Endsr;

       //========================================================
       // Look for WDEAP_HLD Record
       //========================================================
       Begsr LookForWDEAP_HLD;

        Clear rrnWDEAP_HLD;

        // Build Query to see if WDEAP_HLD record exists
        //*** Begin Chg *** 08/14/23 ***************************************
        //*** Select;
        //*** When dsIn.type = 'quote billing';
        //***    strSqlStatement = 'Select rrn(WDEAP_HLD) ' +
        //***     'From ' + %Trim(RtvQuoteLib('*QTE')) + '/WDEAP_HLD ' +
        //***     'Where EAco# = ' + %Editc(dsIn.co#:'X') + ' ' +
        //***     'And EAfnd = '   + %Editc(dsIn.group:'X') + ' ' +
        //***     'And EAemp# = '  + %Editc(dsIn.emp#:'X') + ' ' +
        //***     'And EAdiv = '   + %Editc(dsIn.div:'X')  + ' ' +
        //***     'And EAinv# = '  + %Editc(dsIn.inv#:'X');
        //*** Other;
        //***    strSqlStatement = 'Select rrn(WDEAP_HLD) ' +
        //***     'From ' + %Trim(RtvQuoteLib('*LIV')) + '/WDEAP_HLD ' +
        //***     'Where EAco# = ' + %Editc(dsIn.co#:'X') + ' ' +
        //***     'And EAfnd = '   + %Editc(dsIn.group:'X') + ' ' +
        //***     'And EAemp# = '  + %Editc(dsIn.emp#:'X') + ' ' +
        //***     'And EAdiv = '   + %Editc(dsIn.div:'X')  + ' ' +
        //***     'And EAinv# = '  + %Editc(dsIn.inv#:'X');
        //*** EndSl;
        Select;
        When dsIn.type = 'quote billing';
           strSqlStatement = 'Select rrn(WDEAP_HLD), EActyp  ' +
            'From ' + %Trim(RtvQuoteLib('*QTE')) + '/WDEAP_HLD ' +
            'Where EAco# = ' + %Editc(dsIn.co#:'X') + ' ' +
            'And EAfnd = '   + %Editc(dsIn.group:'X') + ' ' +
            'And EAemp# = '  + %Editc(dsIn.emp#:'X') + ' ' +
            'And EAdiv = '   + %Editc(dsIn.div:'X')  + ' ' +
            'And EAinv# = '  + %Editc(dsIn.inv#:'X');
        Other;
           strSqlStatement = 'Select rrn(WDEAP_HLD), EActyp ' +
            'From ' + %Trim(RtvQuoteLib('*LIV')) + '/WDEAP_HLD ' +
            'Where EAco# = ' + %Editc(dsIn.co#:'X') + ' ' +
            'And EAfnd = '   + %Editc(dsIn.group:'X') + ' ' +
            'And EAemp# = '  + %Editc(dsIn.emp#:'X') + ' ' +
            'And EAdiv = '   + %Editc(dsIn.div:'X')  + ' ' +
            'And EAinv# = '  + %Editc(dsIn.inv#:'X');
        EndSl;
        //*** End   Chg *** 08/14/23 ***************************************

        // See if record exists
        Exec Sql Close cursorInvoice;
        Exec Sql Prepare invoiceStmt From :strSqlStatement;
        Exec Sql Declare cursorInvoice cursor for invoiceStmt;
        Exec Sql Open cursorInvoice;
        //*** Begin Chg *** 08/14/23 ***************************************
        //*** Exec Sql fetch next from cursorInvoice into :rrnWDEAP_HLD;
        Exec Sql fetch next from cursorInvoice into :rrnWDEAP_HLD,
                                                    :holdEacTyp;
        //*** End   Chg *** 08/14/23 ***************************************
        Exec Sql Close cursorInvoice;

        If rrnWDEAP_HLD > 0;
           wdeap_hldFound = *On;
        Else;
           wdeap_hldFound = *Off;
        EndIf;

       Endsr;

       //=================================================================
       // Process invoice
       //=================================================================
       Begsr ProcessInvoice;

        // Update 3rd Party Setup
        //*** Begin Chg *** 02/28/25 ************************************
        //*** Begin Chg *** 08/06/23 ************************************
        //*** If dsIn.type <> ' ';
        //*** If dsIn.type <> ' ' And clientID <> '129';
        If dsIn.type <> ' ';
        //*** End   Chg *** 08/06/23 ************************************
        //*** End   Chg *** 02/28/25 ************************************
           ExSr Update3rdPartySetup;
        EndIf;

        // Get policy information from invoice#
        Reset decCount;
        Exec sql
         select count(*) into :decCount
          from WASRP_HLD
          where SRco# = :dsIn.co# and SRfnd = :dsIn.group
           and SRemp# = :dsIn.emp# and SRdiv = :dsIn.div
           and SRinv# = :dsIn.inv#;

        Select;
        // If a record exists in WASRP_HLD for the invoice, call ProcessSRPayments
        When decCount > 0;
           Exsr PaymentsSR;
        Other;
           Exsr Payments;
        Endsl;

       Endsr;

       //====================================================
       Begsr Payments;
       //*** Beg Add ***** 01/25/24 ****************************************
       xxCo# = %int(dsIn.co#);
       xxFnd = %int(dsIn.group);
       Chain (xxCo#:xxFnd) pex00;
       Clear flg3pyGrp;
       If %found(WMEXL);
          flg3pyGrp = EXiCpsGrp;
       Endif;
       //*** End Add ***** 01/25/24 ******************************     *

        // Call trigger process to issue diary
        ExSr GenerateAutoDiary;
       // For 'FAILURE', leave hold record alone.
       // For 'SUCCESS' for quote payment, mark the WDEAP_HLD
       //  as paid, (EAeatp = 'IP')
       // For 'SUCCESS', move A/R to WDEAP or WMEAP except for quote
       //  payments. Quote transfer (W8700R) will transfer quote payments.
        Select;
        When dsIn.status = 'FAILURE';
        When dsIn.status = 'SUCCESS' And dsIn.type = 'quote billing';
           strSqlStatement = 'Update ' +
            %Trim(RtvQuoteLib('*QTE')) + '/WDEAP_HLD ' +
            'Set EAeatp = ''IP'', ' +
            //*** Beg Add ***** 06/29/22 *******************************
            'EAref# = ''' + %Trim(chrRef#) + ''', ' +
            //*** End Add ***** 06/29/22 *******************************
            'EAamt = ' + %EditC(decPaymentAmount:'Q') +  ' ' +
            'Where EAco# = ' + %Editc(dsIn.co#:'X') + ' ' +
            'And EAfnd = '   + %Editc(dsIn.group:'X') + ' ' +
            'And EAemp# = '  + %Editc(dsIn.emp#:'X') + ' ' +
            'And EAdiv = '   + %Editc(dsIn.div:'X')  + ' ' +
            'And EAinv# = '  + %Editc(dsIn.inv#:'X');
            //*** End Chg ***** 06/29/22 *******************************
           Exec Sql Execute Immediate :strSqlStatement;
        When dsIn.status = 'SUCCESS' And dsIn.type <> 'quote billing';
          // Update WDEAP_HLD with batch # of current day unapproved third
          // party batch or next available batch# prior to cash dissolve.
           intBat# = 0;
           //*** Beg Del *** 01/25/24 *** 133WTD ********************* *****
           //***Exec Sql
           //*** End Del *** 01/25/24 *** 133WTD ********************* *****
        //  Beg Chg ***** 07/29/22 **********************************
          //*** Select EAbat#
          //*** Into :intBat#
          //*** From WDEAP
          //*** Where EAachf = 'T'
          //***  And EAtfap <> 'Y'
          //***  And EAidt = :sysDate7;
            //*** Beg Chg *** 01/25/24 *** 133WTD **************************
            //***Select EAbat#
            //***into :intBat#
            //***From WDEAP
            //***Where EAachf = 'T'
            //***And EAtfap <> 'Y'
            //***And EAonli <> 'Y'
            //*** And EAidt = :sysDate7;
            strSql = 'Select EAbat# ' +
            'From ' + %Trim(RtvQuoteLib('*LIV')) + '/WDEAP ' +
            'Where EAachF = ''T'' And EAtfAp <> ''Y'' And ' +
            'EAonli <> ''Y'' And EAidt = ' + %Editc(sysDate7:'3');
            Select;
            When flg3pyGrp = 'Y';
               strSql += ' And EAfnd = ' + %Editc(EXfnd:'X');
            Other;
               strSql += ' And EAfnd In (Select EXfnd ' +
               'From ' + %Trim(RtvQuoteLib('*LIV')) + '/WMEXP ' +
               'Where EXicpsgrp <> ''Y'')';
            Endsl;
            strSql += ' Limit 1';

            Exec SQL Prepare e1 From :strSQL;
            Exec SQL Declare e1Cursor Cursor For e1;
            Exec SQL Open e1Cursor;
            Exec SQL Fetch Next From e1Cursor
            Into :intBat#;
            Exec SQL Close e1Cursor;
            //*** End Chg *** 01/25/24 *** 133WTD **************************
        //  End Chg ***** 07/29/22 **********************************
            If intBat# = 0;
               Exec Sql
                Select Max(EAbat#) + 1
                Into :intBat#
                From WDEAP;
            Endif;
          // Update system code prior to cash dissolve
            Exec Sql
             Select SYkey
             Into :strBatchSysCd
             From WTSYP
             Where SYstat <> 'I'
              And SYtype = 'Y'
             Order by SYkey
             Limit 1;
           Exec Sql
            Update WDEAP_HLD
            Set EAbat# = :intBat#,
             EAsycd = :strBatchSysCd,
             //*** Begin Chg *** 06/29/22 *************************
             //*** EAamt = :decPaymentAmount
             EAamt  = :decPaymentAmount,
             EAref# = :chrRef#
             //*** End   Chg *** 06/29/22 *************************
            Where EAco# = :dsIn.co# And EAfnd = :dsIn.group
             And EAemp# = :dsIn.emp# And EAdiv = :dsIn.div
             And EAinv# = :dsIn.inv#;
           If isValidFetch(sqlStt);
              ExSr CallCashDissolve;
           Endif;
        EndSl;

       Endsr;

       //====================================================
       Begsr PaymentsSR;

       // ProcessSRPayments will provide request parms.
        Clear json;
        Callp WISRPAY ('ProcessSRPayments':inUser:inJson:json:ioRtnCode);
        skipJson = *on;

       Endsr;

       //====================================================
       Begsr Update3rdPartySetup;

          inRequestAR = 'SaveOptOutInAR';
          inUserAR    = inUser;
          chrDate8    = %EditC(dtMDYY(sysDate7):'X');
          chrTime6    = %Char(%Time():*hms0);
          chrDateTime = chrDate8 + chrTime6;
          account#    = %EditC(dsIn.co#:'X') +
                        %EditC(dsIn.group:'X') +
                        %EditC(dsIn.emp#:'X');
          inParmsAR   = '{ "servicer":"InvoiceCloud", ' +
                          '"transaction":"Opt In-Out", ' +
                          '"information":{' +
                              '"date":"' + chrDateTime + '",' +
                              '"accountNumber":"' + %Trim(account#) +
                              '","type":"In"}}';
          outJsonAR   = ' ';
          ioRtnCodeAR = ' ';
          policyAR(inRequestAR:inUserAR:inParmsAR:outJsonAR:
                   ioRtnCodeAR);

       Endsr;

       //====================================================
       Begsr ValidateParms;

        // Status Must be valid
        If dsIn.status <> 'SUCCESS' and dsIn.status <> 'FAILURE';
           AddErrorMessageJson('Status is not ''SUCCESS'' or ''FAILURE''.':
            ioRtnCode);
        Endif;
        //*** Beg Add *** 11/15/22 ********************************************
        // Determine if called from S/R Online or IVC
        Reset decCount;
        Exec sql
         select count(*) into :decCount
          from WASRP_HLD
          where SRco# = :dsIn.co# and SRfnd = :dsIn.group
           and SRemp# = :dsIn.emp# and SRdiv = :dsIn.div
           and SRinv# = :dsIn.inv#;
        Select;
        When decCount > 0;
        //*** End Add *** 11/15/22 ********************************************
           // Check to see if group is set for portal
           If UseGroupInPortal(dsIn.co#:dsIn.group:'S/R Online') = *Off;
              AddErrorMessageJson('Group ' + %char(dsIn.group) +
               ' is not included for service requested ' +
               '(' + %trim(inRequest) + ').':ioRtnCode);
           Endif;
        //*** Beg Add *** 11/15/22 ********************************************
        When decCount = 0;
           // Check to see if group is set for Invoice Cloud
           If ICGroupOpt(dsIn.co#:dsIn.group) = *Off;
              AddErrorMessageJson('Group ' + %char(dsIn.group) +
               ' is not included for service requested ' +
               '(' + %trim(inRequest) + ').':ioRtnCode);
           Endif;
        Endsl;
        //*** End Add *** 11/15/22 ********************************************

        // What kind of authority do you have?
        If strAuthority = '*View';
           AddErrorMessageJson('User, ' + %trim(inUser) +
            ', not authorized to process ' +
            'request (' + %trim(inRequest) + ').'
            :ioRtnCode);
        Endif;

        // Validate Invoice Type
        Select;
        When dsIn.Type = 'billing';
        When dsIn.Type = 'deductible';
        When dsIn.Type = 'quote billing';
        When dsIn.Type = 'retro';
        When dsIn.Type = *Blank;
        Other;
           AddErrorMessageJson('Invalid invoice type.':ioRtnCode);
        EndSl;

        // Is there a hold record?
        ExSr LookForWDEAP_HLD;
        If Not wdeap_hldFound;
           AddErrorMessageJson('WDEAP_HLD record not found.':
           ioRtnCode);
        EndIf;

       Endsr;

     PProcessThirdPartyPayment...
     P                 e

        //========================================
     PGetBatch#...
     P                 b
       //========================================
        Dcl-Pi *N Zoned(9:0);
        End-Pi;
        Dcl-S intBatch# Zoned(9:0);
        Select;
        When intBatch# = 0;
           Exec Sql
            Select Max(EAbat#)
            Into :intBatch#
            From WDEAP_HLD;
           intBatch# += 1;
        EndSl;
        Return intBatch#;
     P                 e
      /End-Free
