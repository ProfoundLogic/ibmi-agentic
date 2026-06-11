     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     h DFTACTGRP(*no) ACTGRP('CONNECT')
     h option(*nodebugio: *srcstmt)
     h BndDir('WTBNDDIR':'YAJL') DecEdit('0.')
     H*====================================================================
     H* Description:
     H*
     H*
     H*====================================================================
     H* inParms -
     H*   name:value
     H*
     H*====================================================================
     H* Revisions:
     H*
     H* 12/19/23 C:300 Support: 114684 (LUBA)
     H*                Premium notices are printing when quote Bill is
     H*                issued. The Premium notice is not saving to darts
     H*                but is printing out with the quote Bill.
     H*
     H* 10/18/23  C:062 Project: WC2663(114327(140))
     H*                 Get the max posting date when getting the to date.
     H*
     H* 10/06/23  C:062 Project: WC2663(114173(140))
     H*                 recreate hold record if payment disputed.
     H*
     H* 09/27/23  C:062 Project: WC2663(114061(129))
     H*                 Check quote for employer.
     H*
     H* 09/11/23  C:291 Project: WC2920
     H*                 Update 3rd party Payment History file with
     H*                 Transaction date and time.
     H*
     H* 04/19/22  C:33  Support: 96520
     H*                 Use the payment value for S/R invoices
     H*====================================================================
     Fwipsl     uf a e           k Disk
     Fwipslhist uf a e           k Disk    rename(pps01:ppsh01) Prefix(PSH:2)
     Fwmeml     if   e           k disk
     Fwdell1    if   e           k disk    rename(pel01:pel01)
     Fwmeal     if   e           k disk    rename(pea01:pea00)
     Fwximl     if   e           k disk    rename(pim01:pim00)

     D* this size needs to change
     D array           s               a   len(32767)
     d inJson          s               a   len(2000000) varying
     D json            s               a   len(8500000) varying
     D jsonSub         s               a   len(8500000) varying

     d i0              s             10i 0 inz
     d i1              s             10i 0 inz
     d i2              s             10i 0 inz
     d i3              s             10i 0 inz

     d key             s             50a   varying
     d key1            s             50a   varying
     d key2            s             50a   varying
     d key3            s             50a   varying

     d node0           s                   Like(yajl_val)
     d node1           s                   Like(yajl_val)
     d node2           s                   Like(yajl_val)
     d node3           s                   Like(yajl_val)

     d* PgmStart variables
     d clientID        s              3a
     d intCo#          s              3  0
     d intUserCo#      s              3  0
     d strAuthority    s             10a
     d strCo#          s              3a
     d strEnv          s              4a
     d strSql          s           5000a   varying inz
     D success         s               n
     d sysDate         s               d
     d sysDate7        s              7  0
     d sysTime6        s              6  0
     d isEnvQuote      s              1n
     d isEnvLive       s              1n
     D errMsg          s          99999a   Varying Inz(' ')
     D errMsg500       s            500a   Varying Inz(' ')
     D policyKeyBad    s               n   Inz(*Off)

     D docNode         s                   like(yajl_val)
     D list            s                   like(yajl_val)
     D node            s                   like(yajl_val)
     D val             s                   like(yajl_val)
     d intFnd          s                   Like(PSfnd)
     d intEmp#         s                   Like(PSEmp#)
     d*** Begin Add *** 10/06/23 *******************************************
     d intDiv          s              5  0 Inz
     d intInv#         s             10  0 Inz
     d intPyr          s              3  0 Inz
     d intPoDt         s              7  0 Inz
     d intInvTypID     s             15  0 Inz
     d intAmt          s              9  2 Inz
     d strSqlQte       s           5000a   varying inz
     d strSqlLiv       s           5000a   varying inz
     d strLibrary      s             10a   Inz
     d*** End   Add *** 10/06/23 *******************************************
     d dsWMEAP       e ds                  ExtName(WMEAP) inz
     d chrTrgName      s             10a
     d chrCo#          s              3a
     d chrFnd          s              3a
     d wmeapPtr        s               *
     d*====================================================================
     d* Data structures
     d dsAccount#      ds            15
     d  strAcctCo#                    3
     d  strAcctFnd                    3
     d  strAcctEmp#                   9

     D PSDS          ESDS                  extname(SZQ1P)

     d GetClientID     pr                  ExtPgm('RTVCLTID')
     d  clientID                      3a

     dRtvEnv           pr                  extpgm('RTVENV')
     d outEnv                         4a

     DChkDlo           pr                  extPgm('CHKIFSOBJ')
     D docName                      256    const
     D error                           n

     d sleep           pr            10i 0 extproc('sleep')
     d seconds                       10u 0 value

     D SVLDA           DS          1024    DTAARA(*LDA)
     D xxDart                         1    overlay(svlda:251)
     d xxRMCD                         5    overlay(svlda:252)
     d xxCopy                         1    overlay(svlda:257)
     d xxPrint                        1    overlay(svlda:258)
     d xxPrinter                     10    overlay(svlda:259)
     d xxTray                        50    overlay(svlda:269)
     d xx#CopiesA                     3    overlay(svlda:319)
     d xxEmail                        1    overlay(svlda:322)
     d xxToEmail                    200    overlay(svlda:323)
     d xxSubject                    150    overlay(svlda:523)
     d xxBody                       350    overlay(svlda:673)
     D xxPath                       255    overlay(svlda:673)

     * Spoolfile Cleanup
     d CallQteBillC    pr                  ExtPgm('WIQTEBILLC')
     d inspoolfile                   10

      *** Begin Add *** 12/19/23 *******************************************
     DQCMDEXC          pr                  ExtPgm('QCMDEXC')
     D command                             const like(strCommand)
     D length                        15  5 const
      *** End   Add *** 12/19/23 *******************************************

     * Premium Notice
     d CallW7321R      pr                  ExtPgm('W7321R')
     d inCompany                      3
     d inGroup                        3
     d inDate                         8
     d inDate2                        8
     d inAddu                         3
     d inExdf                         1
     d inCom1                         2
     d inCom2                         2
     d inCom3                         2
     d inFyr                          3
     d inEmployer#                    9
     d inDivision                     5
     d inFrdt                         8
     d inTodt                         8
     d inSrt1                         2
     d inSrt2                         2
     d inFrea                         2
     d inToea                         2
     d inFraa                         2
     d inToaa                         2
     d inFren                         9
     d inToen                         9
     d inFran                         5
     d inToan                         5
     d inQue                         10
     d inMode                         1
     d inDrmk                         5
     d inIdys                         3
     d inGrma                         1
     d inMinb                         7
     d inDep                          1
     d inExp                          1
     d inDres                         1
     d inPbf                          1
     d inMass                         1
     d inSupbalp                      1
     d inAll                          1
     d inInvoice#                    10

     D isScript        s              1n   inz
     D isUseXML        s              1n   inz
     D isUseSpoolOut   s              1n   inz
     d xxProcType      s            100
     d processType     c                   const('Premium Notice')
     d xxSubmit        s              1n
     d xxMass          s              1
      *** Begin Add *** 12/19/23 *******************************************
     DstrCommand       s            255
      *** End   Add *** 12/19/23 *******************************************

      * Submit Invoice Information
     d invoiceInfor    pr                  ExtPgm('WIINV')

      /copy sPrAtrium
      /copy sPrLetter
      /copy sPrXML
      /copy sPrDate
      /copy sPrFiles
      /copy sPrJson
      /copy sPrSecure
      /copy sPrSql
      /copy sPrTrigger
      /copy wPrAR
      /copy wPrUsrPrms
      /copy wPrEmplr
      /Include yajl_h

      *entry
     DRequestPgm       pr                  ExtPgm('WIAR')
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
       When inRequest = 'GetImageAR';
          GetImageAR();
       When inRequest = 'PutNotificationsAR';
          PutNotificationsAR();
       When inRequest = 'SaveOptOutInAR';
          SaveOptOutInAR();
       When inRequest = 'GetCurrentBalanceAR';
          GetCurrentBalanceAR();
       Endsl;

       outJSON = json;

       *inlr = *on;

       //--------------------------------------------------------
       Begsr PgmStart;
       //--------------------------------------------------------
        CreateAliasSQL('WDEAP_HLD':'WDEAP':'HOLD':*on);
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
       Endsr;

      /End-free

      *==========================================================
      * Request:  GetCurrentBalanceAR
      * Description: get Current A/r Balance
      *
      *==========================================================
     p GetCurrentBalanceAR...
     p                 b

     d account#        s             30a
     d balanceAmount   s             15  2
     d chrMode         s             25
     d chrIncBatch     s              1
     d chrExcludeDep   s              1
     d dateError       s               n
     d fromDate        s              8a
     d fromDt7         s              7  0
     d fromDt8         s              8  0
     d intCo#          s              3  0
     d intFnd          s              3  0
     d intFyr          s              3  0
     d intEmp#         s              9  0
     d intDiv          s              5  0
     d intInv#         s             10  0
     d invCount        s              9  0
     d invoicePassed   s               n
     d invoiceGood     s               n
     d isQuote         s              1n   inz
     d***  Begin Add  ***  04/19/22  ******************
     d isUseSR         s              1n   inz(*off)
     d***  Begin Add  ***  04/19/22  ******************
     d service         s             30a
     d strFromDt7      s              7
     d strToDt7        s              7
     d toDate          s              8a
     d toDt7           s              7  0
     d toDt8           s              8  0

     d* Account #
     d dsAccount#      ds            15
     d  strAcctCo#A                   3
     d  strAcctFndA                   3
     d  strAcctEmp#A                  9

     d* Invoice #
     d dsInvoice#      ds            30
     d  strAcctCo#I                   3
     d  strAcctFndI                   3
     d  strAcctEmp#I                  9
     d  strAcctDiv                    5
     d  strAcctInv#                  10

     ddsWDEAP        e ds                  extname(WDEAP) qualified inz

       ExSr GetParms;
       If ioRtnCode <> 'Error';
          ExSr RetrieveBalance;
          ExSr BuildOutput;
       EndIf;
       json += ProcessErrorMessagesJson('*PRE');

      /Free
       //=================================================================
       // Generate Output
       //=================================================================
       Begsr BuildOutput;

          BuildGiantJSON(json:'invoice#':%Trim(dsInvoice#));
          BuildGiantJSON(json:'amount':%EditC(balanceAmount:'3'));
          BuildGiantJSON(json:'accountNumber':%Trim(dsAccount#));

       Endsr;

       //=================================================================
       // Break parms out from inParms
       //=================================================================
       Begsr GetParms;

          // Initialize fields
          intFyr  = 0;
          intDiv  = -1;
          intInv# = 0;
          invoicePassed = *Off;
          invoiceGood   = *On;
          invCount      = 0;
          Clear dsAccount#;
          Clear dsInvoice#;

          // Load the json tree
          inJson = inParms;
          node0 = yajl_string_load_tree(inJson : errMsg500);
          json += ' "requestParms":{';
          If errMsg500 = ' ';
          // Parse the json into the input parms
             reset i0;
             Dow YAJL_object_loop(node0:i0:key:val);
                BuildGiantJSON(json:key:YAJL_get_string(val));
                Select;
                When key = 'servicer';
                    service  = YAJL_get_string(val);
                When key = 'account#';
                    dsAccount# = YAJL_get_string(val);
                When key = 'invoice#';
                    dsInvoice# = YAJL_get_string(val);
                When key = 'dates';
                   ExSr ProcessDateRange;
                Endsl;
             Enddo;

             //======= Validate the incoming parameters ====================

             // Service Must be passed
             If service = ' ';
                AddErrorMessageJson('Service not passed in':ioRtnCode);
             EndIf;

             Select;
             // Account Number Or Invoice # must be passed.
             When dsAccount# = ' ' or dsInvoice# = ' ';
                AddErrorMessageJson('Must pass account # or invoice #.'
                                    :ioRtnCode);
                invoiceGood  = *Off;
             // Elements of invoice# must be valid numeric data
             When dsInvoice# <> ' ';

                invoicePassed = *On;

                // Company #
                Monitor;
                   intCo# = %Dec(strAcctCo#I:3:0);
                On-Error;
                   intCo# = UserCo#(inUser);
                EndMon;

                // Group #
                Monitor;
                   intFnd = %Dec(strAcctFndI:3:0);
                On-Error;
                   AddErrorMessageJson('Invoice #: Invalid numeric group#.'
                                       :ioRtnCode);
                   invoiceGood = *Off;
                EndMon;

                // Employer #
                Monitor;
                   intEmp# = %Dec(strAcctEmp#I:9:0);
                On-Error;
                   AddErrorMessageJson('Invoice #: Invalid numeric emp#.'
                                       :ioRtnCode);
                   invoiceGood = *Off;
                EndMon;

                // Division
                Monitor;
                   intDiv = %Dec(strAcctDiv:5:0);
                On-Error;
                   AddErrorMessageJson('Invoice #: Invalid numeric div.'
                                       :ioRtnCode);
                   invoiceGood = *Off;
                EndMon;

                // Invoice #
                Monitor;
                   intInv# = %Dec(strAcctInv#:10:0);
                On-Error;
                   AddErrorMessageJson('Invoice #: Invalid numeric inv#.'
                                       :ioRtnCode);
                   invoiceGood = *Off;
                EndMon;

              //***  Begin Add  ***  04/19/22  **********
              // Check for invoice being processed by PAYG and return payment amount
                Exec sql
                 select EAbatA into :balanceAmount
                  from WDEAP_HLD
                  left join WTTTP on TTco#=EAco# and TTkey=EAarCd
                  where EAco#=:intCo# and EAfnd=:intFnd and EAemp#=:intEmp#
                   and EAdiv=:intDiv and EAinv#=:intInv# and TTtype='P';
                If IsValidFetch(sqlstt);
                   isUseSR = *on;
                   invoiceGood = *off;
                Endif;
              //***  End   Add  ***  04/19/22  **********

                // Is the invoice # valid
                If invoiceGood;
                   Exec Sql Select count(*) Into :invCount From WMEAP
                            Where EAco#=:intCo# And EAfnd=:intFnd And
                                  EAemp#=:intEmp# And EAdiv=:intDiv And
                                  EAninv=:intInv#;

                   //* Check A/R holding file in case of CX
                   If invCount = 0;
                      Exec Sql Select count(*) Into :invCount From WMXAP
                               Where XAco#=:intCo# And XAfnd=:intFnd And
                                     XAemp#=:intEmp# And XAdiv=:intDiv And
                                     XAninv=:intInv#;
                   Endif;

                   If invCount = 0;
                      strSql = 'select * '+
                       'from ' + %trim(RtvQuoteLib('*QTE')) + '/WDEAP_HLD' +
                       ' where EAco# = ' + %char(intCo#) +
                       ' and EAfnd = ' + %char(intFnd) +
                       ' and EAemp# = ' + %char(intEmp#) +
                       ' and EAdiv = ' + %char(intDiv) +
                       ' and EAinv# = ' + %char(intInv#) +
                       ' limit 1';
                      Exsr ResetCursor;
                      Exec sql fetch next from cursorGetCurrBal into :dsWDEAP;
                      Select;
                      When IsValidFetch(sqlstt);
                         isQuote = *on;
                      Other;
                         invoiceGood = *Off;
                      Endsl;
                   EndIf;
                EndIf;
             // Elements of account# must be valid numeric data
             When dsAccount# <> ' ';
                // Company #
                Monitor;
                   intCo# = %Dec(strAcctCo#A:3:0);
                On-Error;
                   intCo# = UserCo#(inUser);
                EndMon;

                // Group #
                Monitor;
                   intFnd = %Dec(strAcctFndA:3:0);
                On-Error;
                   AddErrorMessageJson('Account #: Invalid numeric group#.'
                                       :ioRtnCode);
                EndMon;

                // Employer #
                Monitor;
                   intEmp# = %Dec(strAcctEmp#A:9:0);
                On-Error;
                   AddErrorMessageJson('Account #: Invalid numeric emp#.'
                                       :ioRtnCode);
                EndMon;
             EndSl;

             // From Date Must be valid if passed
             If fromDate = ' ' Or fromDate = '00000000';
                fromDt7    = 0;
                fromDt8    = 0;
                strFromDt7 = '0000000';
             Else;
                If invoicePassed;
                   AddErrorMessageJson('From Date: Date range not allowed ' +
                                       'when invoice # passed.'
                                       :ioRtnCode);
                Else;
                   dateError= *Off;
                   Monitor;
                      fromDt8 = %Dec(fromDate:8:0);
                   On-Error;
                      AddErrorMessageJson('From Date: Invalid numeric data. '
                                          :ioRtnCode);
                      dateError= *On;
                   EndMon;

                   // From Date must be a valid date
                   If Not dateError;
                      fromDt7     = dtCYMD(fromDt8);
                      strFromDt7  = %EditC(fromDt7:'X');
                      If fromDt7 = 0;
                         AddErrorMessageJson('From Date: Invalid date.'
                                             :ioRtnCode);
                      EndIf;
                   EndIf;
                EndIf;
             EndIf;

             // To Date Must be valid if passed
             If toDate = ' ' Or toDate = '00000000';
                Select;
                When isQuote;
                   toDt8 = DtMDYY(dsWDEAP.EApoDt);
                   strToDt7 = %EditC(dsWDEAP.EApoDt:'X');
                When invoicePassed And invoiceGood;
                   //*** Begin Chg *** 10/18/23 ****************************
                   //***Exec Sql Select EApoDt Into :toDt7 From WMEAP
                   //***         Where EAco#=:intCo# And EAfnd=:intFnd And
                   //***               EAemp#=:intEmp# And EAdiv=:intDiv And
                   //***               EAninv=:intInv#;
                   Exec Sql Select Max(EApoDt) Into :toDt7 From WMEAP
                            Where EAco#=:intCo# And EAfnd=:intFnd And
                                  EAemp#=:intEmp# And EAdiv=:intDiv And
                                  EAninv=:intInv#;
                   //*** End   Chg *** 10/18/23 ****************************
                   If toDt7 = 0;
                      Exec Sql Select XApoDt Into :toDt7 From WMXAP
                               Where XAco#=:intCo# And XAfnd=:intFnd And
                                     XAemp#=:intEmp# And XAdiv=:intDiv And
                                     XAninv=:intInv#;
                   Endif;
                   toDt8    = dtMDYY(toDt7);
                   strToDt7 = %EditC(toDt7:'X');
                Other;
                   toDt7    = DateTo7(systemDate());
                   toDt8    = DateTo8(systemDate());
                   strToDt7 = %EditC(toDt7:'X');
                Endsl;
             Else;
                If invoicePassed;
                   AddErrorMessageJson('To Date: Date range not allowed ' +
                                       'when invoice # passed.'
                                       :ioRtnCode);
                Else;
                   dateError= *Off;
                   Monitor;
                      toDt8 = %Dec(toDate:8:0);
                   On-Error;
                      AddErrorMessageJson('To Date: Invalid numeric data.   '
                                          :ioRtnCode);
                      dateError= *On;
                   EndMon;

                   // To Date must be a valid date
                   If Not dateError;
                      toDt7     = dtCYMD(toDt8);
                      strToDt7  = %EditC(toDt7:'X');
                      If toDt7 = 0;
                         AddErrorMessageJson('To Date: Invalid date.'
                                             :ioRtnCode);
                      EndIf;
                   EndIf;
                EndIf;
             EndIf;

          Else;
             AddErrorMessageJson(%Trim(errMsg500):ioRtnCode);
          EndIf;

          json += '}';

          // Free up the space by removing the tree
          yajl_tree_free(node0);

       Endsr;

       //=================================================================
       // ProcessDateRange
       //=================================================================
       Begsr ProcessDateRange;

          // Load the json tree
          json = %scanrpl('""':'{':json:%len(json)-2);
          node1 = YAJL_object_find(node0:key);
          Reset i1;
          Dow YAJL_object_loop(node1:i1:key:val);
              BuildGiantJSON(json:key:YAJL_get_string(val));
              Select;
              When key = 'from';
                 fromDate  = YAJL_get_string(val);
              When key = 'to';
                 toDate    = YAJL_get_string(val);
              Endsl;
          Enddo;
          json += '}';

          // Free up the space by removing the tree
          yajl_tree_free(node1);

       Endsr;

       //=================================================================
       // Reset the cursor
       //=================================================================
       Begsr ResetCursor;
        Exec sql close cursorGetCurrBal;
        Exec sql prepare sqlGetCurrBal from :strSql;
        Exec sql declare cursorGetCurrBal cursor for sqlGetCurrBal;
        Exec sql open cursorGetCurrBal;
       Endsr;

       //=================================================================
       // Retrieve Balance
       //=================================================================
       Begsr RetrieveBalance;
        If isQuote;
           balanceAmount = dsWDEAP.EAamt;
           Leavesr;
        Endif;

        //***  Begin Add  ***  04/19/22  **********
        // S/R balance found during GetParms
        If isUseSR;
           Leavesr;
        Endif;
        //***  End   Add  ***  04/19/22  **********

          // Are we excluding Deposts?
          //*****Exec Sql Select EXindP Into :chrExcludeDep
          //*****   From wmexp Where EXco#=:intCo# And EXfnd=:intFnd;

          // Get the policy Year
          Exec Sql Select EAfyr Into :intFyr From WMEAP
                   Where EAco#=:intCo# And EAfnd=:intFnd And
                         EAemp#=:intEmp# And EAdiv=:intDiv And
                         EAninv=:intInv# Limit 1;

          If intFyr = 0;
          // Get the policy Year from holding file if not found in WMEAP
             Exec Sql Select XAfyr Into :intFyr From WMXAP
                      Where XAco#=:intCo# And XAfnd=:intFnd And
                            XAemp#=:intEmp# And XAdiv=:intDiv And
                            XAninv=:intInv# Limit 1;
          Endif;

          // Get the A/R Balance
          chrMode       = 'Invoice Cloud';
          chrIncBatch   = 'Y';
          chrExcludeDep = ' ';
          balanceAmount = ARBalance(fromDt7:toDt7:intCo#:intFnd:intFyr:
                          intEmp#:intDiv:chrMode:intInv#:chrIncBatch:
                          chrExcludeDep);

       Endsr;

      /End-Free

     p GetCurrentBalanceAR...
     p                 e

      *==========================================================
      * Request:  GetImageAR
      * Description: Retrieves Image A/R.
      *
      *==========================================================
     p GetImageAR      b

     d result          ds                  Qualified
     D   servicer                   100
     D   account                     15
     D   company                      3
     D   group                        3
     D   employer                     9
     D   division                     5
     D   year                         3
     D   invoice#                    10
     D   invoice                     30
     D   invoiceType                 50

     d company         s                   Like(ELco#)
     d group           s                   Like(ELfnd)
     d employer        s                   Like(ELemp#)
     d division        s                   Like(ELdiv)
     d year            s                   Like(ELfYr)
     d invoice#        s                   Like(EANInv)

     d* Data structure
     d dsInvoice#      ds            30
     d  strCo#                        3
     d  strFnd                        3
     d  strEmp#                       9
     d  strDiv                        5
     d  strInv#                      10

     d spoolFile       s             10
     d x               s              5  0
     d limit           s              5  0

     d inCompany       s              3
     d inGroup         s              3
     d inDate          s              8
     d inDate2         s              8
     d inAddu          s              3
     d inExdf          s              1
     d inCom1          s              2
     d inCom2          s              2
     d inCom3          s              2
     d inFyr           s              3
     d inEmployer#     s              9
     d inDivision      s              5
     d inFrdt          s              8
     d inTodt          s              8
     d inSrt1          s              2
     d inSrt2          s              2
     d inFrea          s              2
     d inToea          s              2
     d inFraa          s              2
     d inToaa          s              2
     d inFren          s              9
     d inToen          s              9
     d inFran          s              5
     d inToan          s              5
     d inQue           s             10
     d inMode          s              1
     d inDrmk          s              5
     d inIdys          s              3
     d inGrma          s              1
     d inMinb          s              7
     d inDep           s              1
     d inExp           s              1
     d inDres          s              1
     d inPbf           s              1
     d inMass          s              1
     d inSupbalp       s              1
     d inAll           s              1
     d inInvoice#      s             10

       ExSr GetParms;
       If ioRtnCode <> 'Error';
          ExSr FindImage;
          ExSr BuildOutput;
       EndIf;
       Exsr BldErrorsInfo;

      /Free

       //=================================================================
       // Break parms out from inParms
       //=================================================================
       Begsr GetParms;

          // Load the json tree
          policyKeyBad = *off;
          inJson = inParms;

          node0 = yajl_string_load_tree(inParms : errMsg500);
          If errMsg500 = ' ';
          // Parse the json into the input parms
             reset i0;
             Dow YAJL_object_loop(node0:i0:key:val);
                Select;
                When key = 'servicer';
                   BuildGiantJSON(json:key:YAJL_get_string(val));
                   result.servicer = %trim(YAJL_get_string(val));
                When key = 'accountNumber';
                   BuildGiantJSON(json:key:YAJL_get_string(val));
                   result.account = %trim(YAJL_get_string(val));
                // Parse information parm into subfields for node1
                When key = 'invoice';
                   BuildGiantJSON(json:key:YAJL_get_string(val));
                   json = %scanrpl('""':'{':json:%len(json)-2);
                   node1 = YAJL_object_find(node0:key);
                   Reset i1;
                   Dow YAJL_object_loop(node1:i1:key:val);
                      Select;
                      When key = 'number';
                         BuildGiantJSON(json:key:YAJL_get_string(val));
                         result.invoice = %trim(YAJL_get_string(val));
                      When key = 'type';
                         BuildGiantJSON(json:key:YAJL_get_string(val));
                         result.invoiceType = %trim(YAJL_get_string(val));
                      Endsl;
                   Enddo;
                   json += '}';
                // Free up the space by removing the tree
                   yajl_tree_free(node1);
                Endsl;
             Enddo;

             // Free up the space by removing the tree
             yajl_tree_free(node0);




             //======= Validate the incoming parameters ====================

             dsInvoice# = result.invoice;
             result.company  = strCo#;
             result.group    = strFnd;
             result.employer = strEmp#;
             result.division = strDiv;
             result.invoice# = strInv#;

             // Company Must be passed
             If result.company = ' ';
                AddErrorMessageJson('Company # not parsable':ioRtnCode);
                policyKeyBad = *On;
             Else;
                Monitor;
                   company = %Dec(%Trim(result.company):3:0);
                On-Error;
                   AddErrorMessageJson('Company # has invalid characters'
                    :ioRtnCode);
                   policyKeyBad = *On;
                EndMon;
             EndIf;

             // Group Must be passed
             If result.group = ' ';
                AddErrorMessageJson('Group # not parsable':ioRtnCode);
                policyKeyBad = *On;
             Else;
                Monitor;
                   group = %Dec(%Trim(result.group):3:0);
                On-Error;
                   AddErrorMessageJson('Group # has invalid characters'
                    :ioRtnCode);
                   policyKeyBad = *On;
                EndMon;
             EndIf;

             // Employer # Must be passed
             If result.employer = ' ';
                AddErrorMessageJson('Employer # not parsable':ioRtnCode);
                policyKeyBad = *On;
             Else;
                Monitor;
                   employer = %Dec(%Trim(result.employer):9:0);
                On-Error;
                   AddErrorMessageJson('Employer # has invalid characters'
                    :ioRtnCode);
                   policyKeyBad = *On;
                EndMon;
             EndIf;

             // Division # Must be passed
             If result.division = ' ';
                AddErrorMessageJson('Division # not parsable':ioRtnCode);
                policyKeyBad = *On;
             Else;
                Monitor;
                   division = %Dec(%Trim(result.division):5:0);
                On-Error;
                   AddErrorMessageJson('Division # has invalid characters'
                    :ioRtnCode);
                   policyKeyBad = *On;
                EndMon;
             EndIf;

             // Invoice # Must be passed
             If result.invoice# = ' ';
                AddErrorMessageJson('Invoice # not parsable':ioRtnCode);
                policyKeyBad = *On;
             Else;
                Monitor;
                   invoice# = %Dec(%Trim(result.invoice#):10:0);
                On-Error;
                   AddErrorMessageJson('Invoice # has invalid characters'
                    :ioRtnCode);
                   policyKeyBad = *On;
                EndMon;
             EndIf;

             //* Do not perform remaining edits if pulling quote image.
             If result.invoiceType = 'quote billing';
                Leavesr;
             Endif;

             //* Find the policy year
             Setll (company:group:employer:division) pea00;
             Reade (company:group:employer:division) pea00;
             Dow not %eof;
                If EANINV <> invoice#;
                   Reade (company:group:employer:division) pea00;
                Else;
                   If EAARTy = 'A' or EAARTy = 'B' or EAARTy = 'D' or
                    EAARty = 'F' or EAARTy = 'H';
                      result.year = %editc(EAFYR:'X');
                      Leave;
                   Else;
                      Reade (company:group:employer:division) pea00;
                   Endif;
                Endif;
             Enddo;

             // If invoice # not found.
             If %eof;
                AddErrorMessageJson('Invoice # not valid for Policy':
                 ioRtnCode);
                policyKeyBad = *On;
             Else;

                // Year Must be passed
                If result.year = ' ';
                   AddErrorMessageJson('Policy year not found':ioRtnCode);
                   policyKeyBad = *On;
                Else;
                   Monitor;
                      year = %Dec(%Trim(result.year):3:0);
                   On-Error;
                      AddErrorMessageJson('Policy year has invalid characters'
                       :ioRtnCode);
                      policyKeyBad = *On;
                   EndMon;
                EndIf;

             Endif;

             // Now let's validate the policy
             If Not policyKeyBad;
                Chain (company:group:year:employer:division) pel01;
                If Not %Found;
                   AddErrorMessageJson('Invalid Policy':ioRtnCode);
                EndIf;
             EndIf;

             // Now let's validate the invoice#
             If Not policyKeyBad;
                Setll (company:group:employer:division:year) pea00;
                Reade (company:group:employer:division:year) pea00;
                Dow not %eof;
                   If EANINV <> invoice#;
                      Reade (company:group:employer:division:year) pea00;
                   Else;
                      Leave;
                   Endif;
                Enddo;
                If %eof;
                   AddErrorMessageJson('Invoice # not valid for Policy':
                    ioRtnCode);
                EndIf;
             EndIf;

          Else;
             AddErrorMessageJson(%Trim(errMsg500):ioRtnCode);
          EndIf;

       Endsr;

       //=================================================================
       // Find Image
       //=================================================================
       Begsr FindImage;

         //* Clear out LDA for document name/path.
         In svLDA;
         Clear svLDA;
         Out svLDA;

         Chain (company:group:employer:division:invoice#) pim00;
         If %found and ((result.invoiceType = 'quote billing' and IMType =
          'Quote Premium Notice') or (result.invoiceType <> 'quote billing'
          and IMType <> 'Quote Premium Notice'));

            ChkDlo(IMUncn:success);
            If success;
               xxPath = %trim(IMUncn);
               Leavesr;
            Endif;
         Else;
            If result.invoiceType = 'quote billing';
               Leavesr;
            Endif;
         Endif;

       //* Generate premium notice for output.
         inCompany   = result.company;
         inGroup     = result.group;
         inEmployer# = result.employer;
         inDivision  = result.division;
         inFyr       = result.year;
         inInvoice#  = result.invoice#;

         inDate  = %EditC(dtMDYY(EAPodt):'X');
         inDate2 = %EditC(dtMDYY(EAPodt):'X');

         inFrdt  = '00000000';
         inTodt  = '00000000';

         inAddu = ' ';
         inExdf = ' ';
         inCom1 = ' ';
         inCom2 = ' ';
         inCom3 = ' ';
         inSrt1 = ' ';
         inSrt2 = ' ';
         inFrea = ' ';
         inToea = ' ';
         inFraa = ' ';
         inToaa = ' ';
         inFren = '000000000';
         inToen = '000000000';
         inFran = '00000';
         inToan = '00000';
         inQue  = 'QPRINT';
         inMode = 'D';
         inDrmk = ' ';
         inIdys = '000';
         inGrma = 'Y';
         inMinb = '0000000';
         inDep  = 'Y';
         inExp  = 'Y';
         inDres = ' ';
         inPbf  = 'P';
         inMass = ' ';
         inSupBalp = 'Y';
         inAll  = ' ';

         isUseSpoolOut = UseSpoolOut(processType);
         isUseXML = UseScriptura(processType)
          or Use3rdXml(processType);

         If isUseXML;
            isScript = *on;
         Else;
            isScript = *off;
         Endif;

         //* Setup up data area for scriptura call in notice program.
         If isScript = 'Y';
            xxMass = ' ';
            xxProcType = processType;
            In svLDA;
            Clear svLDA;

            xxDart = 'N';
            xxRMCD = ' ';

            Out svLDA;

            xxSubmit = OutputWindow(xxProcType:xxMass:'Y':'M');
         Endif;

         If isUseSpoolOut;
            spoolFile = 'W7321O';
            //* Cleanup potential spoolfile.
            CallQteBillC(spoolFile);
         Endif;

         //*** Begin Add *** 12/19/23 *******************************************
         strCommand = 'OVRPRTF FILE(spoolFile) HOLD(*YES)';
         Callp QCMDEXC(strCommand:%len(strCommand));
         //*** End   Add *** 12/19/23 *******************************************

         CallW7321R(inCompany:inGroup:inDate:inDate2:inAddu:inExdf:inCom1:
          inCom2:inCom3:inFyr:inEmployer#:inDivision:inFrdt:inTodt:inSrt1:
          inSrt2:inFrea:inToea:inFraa:inToaa:inFren:inToen:inFran:inToan:
          inQue:inMode:inDrmk:inIdys:inGrma:inMinb:inDep:inExp:inDres:
          inPbf:inMass:inSupBalp:inAll:inInvoice#);

         //*** Begin Add *** 12/19/23 *******************************************
         strCommand = 'DLTOVR FILE(spoolFile)';
         Callp QCMDEXC(strCommand:%len(strCommand));
         //*** End   Add *** 12/19/23 *******************************************

         //* Pull in document from local data area.
         In svLDA;
         Out svLDA;

         //* Verify if document exists/arrived before moving forward.
         x = 0;
         limit = 3;
         Dow x < limit;
            ChkDlo(xxPath:success);
            If success;
               Leave;
            Else;
               Sleep(10);
               x += 1;
            Endif;
         Enddo;
         If x = limit;
            Clear xxPath;
         Endif;

         If isUseSpoolOut;
            //* Cleanup spoolfile.
            CallQteBillC(spoolFile);
         Endif;

       Endsr;

       //=================================================================
       // Generate Output
       //=================================================================
       Begsr BuildOutput;

          If xxPath <> ' ';
             json = %trim(json) + ', ' +
               BuildJsonNode('file':%Trim(xxPath));
          Else;
             If isEnvQuote;
                AddErrorMessageJson('Cannot generate Premium Notice ' +
                 'in Quote.':ioRtnCode);
             Else;
                AddErrorMessageJson('Error Printing Premium Notice':ioRtnCode);
             Endif;
             json += ProcessErrorMessagesJson('*PRE');
             json = %scanrpl(',':'':json:1:1);
          Endif;

       Endsr;

       //=================================================================
       // Build Error Message Response
       //=================================================================
       Begsr BldErrorsInfo;

          // Finish by generating the errors
          json += ProcessErrorMessagesJson('*PRE');
          json = %scanrpl(',':'':json:1:1);

       Endsr;

      /End-Free

     p GetImageAR      e

      *==========================================================
      * Request:  PutNotificationsAR
      * Description: Sends A/R Notifications
      *
      *==========================================================
     p PutNotificationsAR...
     p                 b

     D sqlStmtStr      s          32000a   varying inz
     d inServicer      s            100a   varying
     d inAcctNumber    s            100a   varying
     d inType          s             20a   varying
     d inInformation   s            100a   varying
     d inService       s            100a   varying
     d inMessage       s            100a   varying
     d*** Begin Add *** 10/06/23 *******************************************
     d dsInvoice#      ds            30
     d  strAcctCo#I                   3
     d  strAcctFndI                   3
     d  strAcctEmp#I                  9
     d  strAcctDiv                    5
     d  strAcctInv#                  10

        dcl-ds dsResults qualified Inz;
            policyYear  packed(3);
            postDate    packed(7);
            invTypeID   zoned(15:0);
            totalAmount packed(31:2);
        end-ds;
     d*** End   Add *** 10/06/23 *******************************************

       ExSr GetParms;
       If ioRtnCode <> 'Error';
          ExSr ProcessNotification;
          ExSr BuildOutput;
       EndIf;
       json += ProcessErrorMessagesJson('*PRE');

       //=================================================================
       // Process Notifications
       //=================================================================
       Begsr ProcessNotification;

          Select;
          When inType = 'Autopay Enrollment';
             ExSr NoticeAutopayEnrollment;
          When inType = 'CCF Update';
             ExSr NoticeCCFUpdate;
          When inType = 'Disputed Charge';
             ExSr NoticeDisputedCharge;
          When inType = 'Error';
             ExSr NoticeError;
          When inType = 'Paperless';
             ExSr NoticePaperless;
          EndSl;

       Endsr;

       //=================================================================
       // Break parms out from inParms
       //=================================================================
       Begsr GetParms;

          json +=  '"requestParms" : {';
          // Load the json tree for node0
          policyKeybad = *off;
          //*** Begin Add *** 10/06/23 *************************************
          Clear dsInvoice#;
          //*** End   Add *** 10/06/23 *************************************
          node0 = yajl_string_load_tree(inParms : errMsg500);
          If errMsg500 = ' ';
          // Parse the json into the input parms
             reset i0;
             Dow YAJL_object_loop(node0:i0:key:val);
                BuildGiantJSON(json:key:YAJL_get_string(val));
                Select;
                When key = 'servicer';
                   inServicer = YAJL_get_string(val);
                When key = 'accountNumber';
                   dsAccount# = YAJL_get_string(val);
                When key = 'type';
                   inType = YAJL_get_string(val);
                //*** Begin Add *** 10/06/23 *******************************
                When key = 'invoice#';
                    dsInvoice# = YAJL_get_string(val);
                //*** End   Add *** 10/06/23 *******************************
                When key = 'information';
                   Select;
                   When inType = 'Error';
                      json = %scanrpl('""':'{':json:%len(json)-2);
                      node1 = YAJL_object_find(node0:key);
                      Reset i1;
                      Dow YAJL_object_loop(node1:i1:key:val);
                         BuildGiantJSON(json:key:YAJL_get_string(val));
                         Select;
                         When key = 'service';
                           inService = YAJL_get_string(val);
                         When key = 'message';
                           inMessage = YAJL_get_string(val);
                         Endsl;
                      Enddo;
                      json += '}';

                   Other;
                      inInformation = YAJL_get_string(val);
                      If inInformation = *blanks and inType <> 'CCF Update';
                         AddErrorMessageJson('Information not passed in'
                         :ioRtnCode);
                         policyKeyBad = *on;
                      Endif;
                   Endsl;
                Endsl;
             Enddo;

             //======= Validate the incoming parameters ====================
             // Servicer is required
             If inServicer = *blanks;
                AddErrorMessageJson('Servicer not passed in':ioRtnCode      );
             Endif;

             // Account is required
             If dsAccount# = *blanks;
                AddErrorMessageJson('Account number not passed in'
                :ioRtnCode);
             Endif;

             // Type is required
             If inType = *blanks;
                AddErrorMessageJson('Type not passed in'
                :ioRtnCode);
             Endif;

             // Service is required
             If inService = *blanks And inType = 'Error';
                AddErrorMessageJson('Service not passed in'
                :ioRtnCode);
             Endif;

             // Message is required
             If inMessage = *blanks And inType = 'Error';
                AddErrorMessageJson('Message not passed in'
                :ioRtnCode);
             Endif;

             // Close up
             json += '}';
          Else;
             AddErrorMessageJson(%Trim(errMsg500):ioRtnCode);
          Endif;

       Endsr;

       //=================================================================
       // Notice Autopay Enrollment
       //=================================================================
       Begsr NoticeAutoPayEnrollment;
         // Update WIPSP with Auto pay enrollment
         Monitor;
         intCo#  = %Dec(strAcctCo#:3:0);
         intFnd  = %Dec(strAcctFnd:3:0);
         intEmp# = %Dec(strAcctEmp#:9:0);
         On-error;
            AddErrorMessageJson('Invalid employer keys passed in'
             :ioRtnCode);
         Endmon;
         Chain (intCo#:intFnd:intEmp#) pem01;
         If not %Found(WMEML);
            //*** Begin Chg *** 09/27/23 ***********************************
            //*** AddErrorMessageJson('Employer does not exist'
            //***  :ioRtnCode);
            // Now check quote
            If Not QuoteExists(intCo#:intFnd:intEmp#);
               AddErrorMessageJson('Employer does not exist'
                :ioRtnCode);
            EndIf;
            //*** End   Chg *** 09/27/23 ***********************************
         Endif;
         If ioRtnCode <> 'Error';
            Chain (intCo#:intFnd:intEmp#) pps01;
            If %Found(wipsl);
               PSservicer = inServicer;
               If inInformation = 'true';
                  PSautoPay = sysDate7;
               Endif;
               If inInformation = 'false';
                  PSautoPay = 0;
               Endif;
               PSuser = Q1user;
               PSpgm = Q1pgm;
               PSdt = sysDate7;
               PStime = sysTime6;
               Update pps01;
            Else;
               Clear pps01;
               PSco# = intCo#;
               PSfnd = intFnd;
               PSemp# = intEmp#;
               PSservicer = inServicer;
               If inInformation = 'true';
                  PSautoPay = sysDate7;
               Endif;
               If inInformation = 'false';
                  PSautoPay = 0;
               Endif;
               PSstat = 'A';
               PSiUsr = Q1user;
               PSiPgm = Q1pgm;
               PSidt = sysDate7;
               PSitim = sysTime6;
               PSuser = Q1user;
               PSpgm = Q1pgm;
               PSdt = sysDate7;
               PStime = sysTime6;
               Write pps01;
            Endif;
            // Write a transaction history record to the history file
            PSHco#  = intCo#;
            PSHfnd  = intFnd;
            PSHemp# = intEmp#;
            PSHfield = 'PSAUTOPAY';
            If inInformation = 'true';
               PSHaction = 'Autopay Enrollment true';
            Else;
               PSHaction = 'Autopay Enrollment false';
            Endif;

            // If Transaction date is not being passed then set to 0
            Clear PSHtrDtDsp;
            PSHtrDt = 0;
            // Transaction time is not being passed
            PSHtrTm = 0;

            PSHstat = 'A';
            PSHiUsr = Q1user;
            PSHiPgm = Q1pgm;
            PSHiDt = sysDate7;
            PSHiTim = sysTime6;
            PSHuser = Q1user;
            PSHpgm = Q1pgm;
            PSHdt = sysDate7;
            PSHtime = sysTime6;

            Write ppsh01;
         Endif;

       Endsr;
       //=================================================================
       // Notice CCF Update
       //=================================================================
       Begsr NoticeCCFUpdate;

         // Update WIPSP for CCF with current timestamp
         Monitor;
         intCo#  = %Dec(strAcctCo#:3:0);
         intFnd  = %Dec(strAcctFnd:3:0);
         intEmp# = %Dec(strAcctEmp#:9:0);
         On-error;
            AddErrorMessageJson('Invalid employer keys passed in'
             :ioRtnCode);
         Endmon;
         Chain (intCo#:intFnd:intEmp#) pem01;
         If not %Found(WMEML);
            //*** Begin Add *** 09/27/23 ***********************************
            //*** AddErrorMessageJson('Employer does not exist'
            //***  :ioRtnCode);
            If Not QuoteExists(intCo#:intFnd:intEmp#);
               AddErrorMessageJson('Employer does not exist'
                :ioRtnCode);
            EndIf;
            //*** End   Add *** 09/27/23 ***********************************
         Endif;
         If ioRtnCode <> 'Error';
            Chain (intCo#:intFnd:intEmp#) pps01;
            If %Found(wipsl);
               PSservicer = inServicer;
               PS3PtyUpd = %Timestamp;
               PSuser = Q1user;
               PSpgm = Q1pgm;
               PSdt = sysDate7;
               PStime = sysTime6;
               Update pps01;
            Else;
               Clear pps01;
               PSco# = intCo#;
               PSfnd = intFnd;
               PSemp# = intEmp#;
               PSservicer = inServicer;
               PS3PtyUpd = %Timestamp;
               PSstat = 'A';
               PSiUsr = Q1user;
               PSiPgm = Q1pgm;
               PSidt = sysDate7;
               PSitim = sysTime6;
               PSuser = Q1user;
               PSpgm = Q1pgm;
               PSdt = sysDate7;
               PStime = sysTime6;
               Write pps01;
            Endif;
            invoiceInfor();
         Endif;
       Endsr;
       //=================================================================
       // Notice Disputed Charge
       //=================================================================
       Begsr NoticeDisputedCharge;

         intCo#  = %Dec(strAcctCo#:3:0);
         intFnd  = %Dec(strAcctFnd:3:0);
         intEmp# = %Dec(strAcctEmp#:9:0);
         //*** Begin Add *** 10/06/23 ************************************
         intDiv  = %Dec(strAcctDiv:5:0);
         intinv# = %Dec(strAcctInv#:10:0);
         //*** End   Add *** 10/06/23 ************************************

          // Populate data structure
          sqlStmtStr = 'Select * ' +
             'From WMEAP Where EAco#=' +
             %EditC(intCo#:'X') + ' And EAfnd=' + %EditC(intFnd:'X') +
             ' And EAemp#=' + %EditC(intEmp#:'X') + ' Limit 1';

          Exec Sql Prepare sqlWMEAP From :sqlStmtStr;
          Exec SQL Declare cursorWMEAP Cursor For sqlWMEAP;
          Exec SQL Open cursorWMEAP;

          Exec SQL fetch next from cursorWMEAP into :dsWMEAP;
          If IsValidFetch(sqlStt);
             // Call trigger process
             chrTrgName = '3PRTPMTDIS';
             chrCo#     = %EditC(intCo#:'X');
             chrFnd     = %EditC(intFnd:'X');
             wmeapPtr   = %Addr(dsWMEAP);
             ProcessTrigger(chrTrgName:chrCo#:chrFnd:wmeapPTR);
             //*** Begin Add *** 10/06/23 **********************************
             // Recreate the WDEAP_HLD Record
             ExSr RecreateHoldRecord;
             //*** End   Add *** 10/06/23 **********************************
          EndIf;

       Endsr;
       //=================================================================
       // Notice Error
       //=================================================================
       Begsr NoticeError;

         // Write to SIWEBSVCM
         If ioRtnCode <> 'Error';
            Exec SQL insert into SIWEBSVCM
             Values(:inService, :inMessage);
         Endif;

       Endsr;
       //=================================================================
       //  Notice Paperless
       //=================================================================
       Begsr NoticePaperless;

         // Update WIPSP with Paperless
         Monitor;
         intCo#  = %Dec(strAcctCo#:3:0);
         intFnd  = %Dec(strAcctFnd:3:0);
         intEmp# = %Dec(strAcctEmp#:9:0);
         On-error;
            AddErrorMessageJson('Invalid employer keys passed in'
            :ioRtnCode);
         Endmon;
         Chain (intCo#:intFnd:intEmp#) pem01;
         If not %Found(WMEML);
            //*** End   Add *** 09/27/23 ***********************************
            //*** AddErrorMessageJson('Employer does not exist'
            //***  :ioRtnCode);
            If Not QuoteExists(intCo#:intFnd:intEmp#);
               AddErrorMessageJson('Employer does not exist'
                :ioRtnCode);
            EndIf;
            //*** End   Add *** 09/27/23 ***********************************
         Endif;
         If ioRtnCode <> 'Error';
            Chain (intCo#:intFnd:intEmp#) pps01;
            If %Found(wipsl);
               PSservicer = inServicer;
               If inInformation = 'true';
                  PSpaper = sysDate7;
               Endif;
               If inInformation = 'false';
                  PSpaper = 0;
               Endif;
               PSuser = Q1user;
               PSpgm = Q1pgm;
               PSdt = sysDate7;
               PStime = sysTime6;
               Update pps01;
            Else;
               Clear pps01;
               PSco# = intCo#;
               PSfnd = intFnd;
               PSemp# = intEmp#;
               PSservicer = inServicer;
               If inInformation = 'true';
                  PSpaper = sysDate7;
               Endif;
               If inInformation = 'false';
                  PSpaper = 0;
               Endif;
               PSstat = 'A';
               PSiUsr = Q1user;
               PSiPgm = Q1pgm;
               PSidt = sysDate7;
               PSitim = sysTime6;
               PSuser = Q1user;
               PSpgm = Q1pgm;
               PSdt = sysDate7;
               PStime = sysTime6;
               Write pps01;
            Endif;
            // Write a transaction history record to the history file
            PSHco#  = intCo#;
            PSHfnd  = intFnd;
            PSHemp# = intEmp#;
            PSHfield = 'PSPAPER';
            If inInformation = 'true';
               PSHaction = 'Paperless true';
            Else;
               PSHaction = 'Paperless false';
            Endif;

            // If Transaction date is not being passed then set to 0
            //*** Beg Chg ***** 09/11/20 ************************
            //***Clear PSHtrDtDsp;
            //***PSHtrDt = 0;
            // Transaction time is not being passed
            //***PSHtrTm = 0;
            //*** Beg Chg ***** 09/11/23 ***************************
            //Update Transaction date/time with system date and time
            PSHtrDtDsp = sysDate;
            PSHtrDt = sysDate7;
            PSHtrTm = sysTime6;
            //*** End Chg ***** 09/11/23 ***************************

            PSHstat = 'A';
            PSHiUsr = Q1user;
            PSHiPgm = Q1pgm;
            PSHiDt = sysDate7;
            PSHiTim = sysTime6;
            PSHuser = Q1user;
            PSHpgm = Q1pgm;
            PSHdt = sysDate7;
            PSHtime = sysTime6;

            Write ppsh01;
         Endif;

       Endsr;

       //*** Begin Add *** 10/06/23 ****************************************
       //=================================================================
       // Recreate Hold Record
       //=================================================================
       Begsr RecreateHoldRecord;

          // Get Key Fields
          strLibrary = RtvQuoteLib('*LIV');
          strSqlLiv = 'Values' +
                   '(Select EAfyr, EApodt, EAinvTypID, sum(EAamt) '  +
                   'From '        + %Trim(strLibrary)   + '/wdeap '  +
                   'Where EAco#=' + %EditC(intCo#:'X')  + ' And '    +
                   'EAfnd='       + %EditC(intFnd:'X')  + ' And '    +
                   'EAemp#='      + %EditC(intEmp#:'X') + ' And '    +
                   'EAdiv='       + %EditC(intDiv:'X')  + ' And '    +
                   'EAinv#='      + %EditC(intInv#:'X') + ' '        +
                   'Group by EAfyr, EApoDt, EAinvTypID) '            +
                   'Into ?';

          exec sql PREPARE stmtKey FROM :strSqlLiv;
          exec sql EXECUTE stmtKey USING :dsResults;

          // If not found in live then look in quote
          If Not IsValidFetch(sqlstt);
             strLibrary = RtvQuoteLib('*QTE');
             strSqlLiv = 'Values' +
                         '(Select EAfyr, EApodt, EAinvTypID, sum(EAamt) '  +
                         'From '        + %Trim(strLibrary)   + '/wdeap '  +
                         'Where EAco#=' + %EditC(intCo#:'X')  + ' And '    +
                         'EAfnd='       + %EditC(intFnd:'X')  + ' And '    +
                         'EAemp#='      + %EditC(intEmp#:'X') + ' And '    +
                         'EAdiv='       + %EditC(intDiv:'X')  + ' And '    +
                         'EAinv#='      + %EditC(intInv#:'X') + ' '        +
                         'Group by EAfyr, EApoDt, EAinvTypID) '            +
                         'Into ?';

             exec sql PREPARE stmtKey FROM :strSqlQte;
             exec sql EXECUTE stmtKey USING :dsResults;
          EndIf;

          If IsValidFetch(sqlstt);
             intPyr      = dsResults.policyYear;
             intPoDt     = dsResults.postDate;
             intAmt      = dsResults.totalAmount;
             intInvTypID = dsResults.invTypeID;

             // Create hold record
             strSql = 'Insert Into ' + %Trim(strLibrary) + '/WDEAP_HLD ' +
              '(EAco#, EAfnd, EAemp#, EAdiv, EApodt, '                      +
              'EAamt, EAsyCd, EAuser, EApgm, EAdt, EAtime, EAinv#, EAtrDt, '+
              'EAiUsr, EAiPgm, EAiDt, EAinvTypId) Values('                  +
              %EditC(intCo#:'X')   + ', '   +  %EditC(intFnd:'X') + ', '   +
              %EditC(intPyr:'X')   + ', '   +
              %EditC(intEmp#:'X')  + ', '   +  %EditC(intDiv:'X') + ', '   +
              %EditC(intPoDt:'X')  + ', '   +  %EditC(intAmt:'4') + ', '   +
              '''SY'''             + ', ''' +  %Trim(Q1user)      + ''', ' +
              '''' + %Trim(Q1pgm)  + ''', ' +  %EditC(sysDate7:'X') + ', ' +
              %EditC(sysTime6:'X') + ', '   +  %EditC(intInv#:'X') + ', '  +
              %EditC(sysDate7:'X') + ', ''' +  %Trim(Q1user)      + ''', ' +
              '''' + %Trim(Q1pgm)  + ''', ' +  %EditC(sysDate7:'X') + ', ' +
              %EditC(intInvTypID:'X') + ')';

             Exec Sql EXECUTE IMMEDIATE :strSQL;
          EndIf;

       Endsr;
       //*** End   Add *** 10/06/23 ****************************************

       //=================================================================
       // Generate Output
       //=================================================================
       Begsr BuildOutput;
       Endsr;

      /End-Free

     p PutNotificationsAR...
     p                 e
      //*** Begin Add *** 09/27/23 ******************************
    P*====================================================================
    P* Looks to see if the Quote Exists
    P*
    P*  QuoteExists     - Returns *On if it finds the quote
    P*   co#            - accepts the company #     (required)
    P*   group          - accepts the group         (required)
    P*   employer       - accepts the employer      (required)
    P*====================================================================
     pQuoteExists      b                   export
     dQuoteExists      pi              n
     d inCo#                          3  0 const
     d inFnd                          3  0 const
     d inEmp#                         9  0 const

     d countWMEMP      s              5s 0 Inz
     d strSql          s           5000    Inz Varying

       strSql = 'Select count(*) from ' +
         %Trim(RtvQuoteLib('*QTE')) + '/wmemp ' +
         'Where EMco# = ' + %Editc(inCo#:'X') + ' ' +
         'And EMfnd = '   + %Editc(inFnd:'X') + ' ' +
         'And EMemp# = '  + %Editc(inEmp#:'X');

       Exec Sql Close cursorWMEMP_Qte;
       Exec Sql Prepare sqlWMEMP_Qte From :strSql;
       Exec Sql Declare cursorWMEMP_Qte cursor for sqlWMEMP_Qte;
       Exec Sql Open cursorWMEMP_Qte;
       Exec SQL fetch next from cursorWMEMP_Qte into :countWMEMP;

       If countWMEMP <> 0;
          Return *On;
       EndIf;

       Return *Off;

     PQuoteExists      e
      //*** End   Add *** 09/27/23 ******************************

      *==========================================================
      * Request:  SaveOptOutInAR
      * Description: Saves Option In A/R.
      *
      *==========================================================
     p SaveOptOutInAR...
     p                 b

     d countWMEMP      s              5s 0
     d sqlStmtStr      s          32000a   varying inz
     d inServicer      s            100a   varying
     d inTransaction   s            100a   varying
     d inInformation   s            100a   varying
      // information is broken into: date, accountNumber, type
     d  strDate        s             14    varying
     d  strDate8       s              8    varying
     d  strTime6       s              6    varying
     d  strAcctNumber  s            100    varying
     d  strType        s            100    varying
     d  isRedundant    s              1
     d  employer       s                   Like(EMemp#)
     d  isFound        s              1

       ExSr GetParms;
       If ioRtnCode <> 'Error';
          Exsr Validate;
          If ioRtnCode <> 'Error';
             ExSr ProcessOption;
             ExSr BuildOutput;
          Endif;
       EndIf;
       Exsr BldErrorsInfo;

      /Free
       //=================================================================
       // Break parms out from inParms
       //=================================================================
       Begsr GetParms;
          // Load the json tree for node0
          policyKeybad = *off;
          node0 = yajl_string_load_tree(inParms : errMsg500);
          If errMsg500 <> ' ';
             AddErrorMessageJson(%Trim(errMsg500):ioRtnCode);
          Else;
          // Parse the json into the input parms
             reset i0;
             Dow YAJL_object_loop(node0:i0:key:val);
                BuildGiantJSON(json:key:YAJL_get_string(val));
                Select;
                When key = 'servicer';
                   inServicer = YAJL_get_string(val);
                When key = 'transaction';
                   inTransaction = YAJL_get_string(val);
                // Parse information parm into subfields for node1
                When key = 'information';
                   json = %scanrpl('""':'{':json:%len(json)-2);
                   node1 = YAJL_object_find(node0:key);
                   Reset i1;
                   Dow YAJL_object_loop(node1:i1:key:val);
                      BuildGiantJSON(json:key:YAJL_get_string(val));
                      Select;
                      When key = 'date';
                         strDate = YAJL_get_string(val);
                         strDate8 = %Subst(strDate:1:8);
                         strTime6 = %Subst(strDate:9:6);
                      When key = 'accountNumber';
                         dsAccount# = YAJL_get_string(val);
                      When key = 'type';
                         strType = YAJL_get_string(val);
                      Endsl;
                   Enddo;
                   json += '}';
                Endsl;
             Enddo;
          Endif;

       Endsr;

       //=================================================================
       // Validate incoming JSON fields
       //==================================================================
       Begsr Validate;

          // Validate Servicer
          If inServicer = *blanks;
             AddErrorMessageJson('Servicer not passed in':ioRtnCode);
              policyKeyBad = *on;

          Endif;

          // Validate Transaction
          If inTransaction = *blanks;
             AddErrorMessageJson('Transaction not passed in'
              :ioRtnCode);
             policyKeyBad = *on;
          Endif;

          // Validate Transaction Date
          If strDate = *blanks;
             AddErrorMessageJson('Transaction date not passed in'
              :ioRtnCode);
             policyKeyBad = *on;
          Else;
             Monitor;
             If dtCheck(%Dec(strDate8:8:0)) = 0;
                AddErrorMessageJson('Transaction date is invalid'
                :ioRtnCode);
             Endif;
             On-error;
                AddErrorMessageJson('Transaction date is invalid'
                :ioRtnCode);
             Endmon;
          Endif;

          // Validate Account#
          If dsAccount# = *blanks;
             AddErrorMessageJson('Account number not passed in'
              :ioRtnCode);
             policyKeyBad = *on;
          Else;
             Monitor;
             intCo#  = %Dec(strAcctCo#:3:0);
             intFnd  = %Dec(strAcctFnd:3:0);
             intEmp# = %Dec(strAcctEmp#:9:0);
             On-error;
                AddErrorMessageJson('Invalid employer keys passed in'
                 :ioRtnCode);
             Endmon;
             Chain (intCo#:intFnd:intEmp#) pem01;
             If not %Found(WMEML);
                // Now check Quote
                countWMEMP = 0;
                sqlStmtStr = 'Select count(*) from ' +
                   %Trim(RtvQuoteLib('*QTE')) + '/wmemp ' +
                   'Where EMco# = ' + %Editc(intCo#:'X') + ' ' +
                   'And EMfnd = '   + %Editc(intFnd:'X') + ' ' +
                   'And EMemp# = '  + %Editc(intEmp#:'X');
                Exec Sql Close cursorWMEMP;
                Exec Sql Prepare sqlWMEMP From :sqlStmtStr;
                Exec Sql Declare cursorWMEMP cursor for sqlWMEMP;
                Exec Sql Open cursorWMEMP;
                Exec SQL fetch next from cursorWMEMP into :countWMEMP;
                If countWMEMP = 0;
                   AddErrorMessageJson('Employer does not exist'
                    :ioRtnCode);
                EndIf;
                Exec Sql Close cursorWMEMP;
             Endif;
          Endif;

          // Validate type
          If strType = *blanks;
             AddErrorMessageJson('Type not passed in'
              :ioRtnCode);
             policyKeyBad = *on;
          Else;
             If strType <> 'In' and strType <> 'Out';
                AddErrorMessageJson('Type is not valid':ioRtnCode);
             Endif;
          Endif;

       Endsr;

       //=================================================================
       // Process Option
       //==================================================================
       Begsr ProcessOption;

         // Update WIPSP with opt in/out
            isRedundant = 'N';
            Chain (intCo#:intFnd:intEmp#) pps01;
            If %Found(wipsl);
               If PSoptFlag = 'I' and strType = 'In';
                  isRedundant = 'Y';
               Endif;
               If PSoptFlag = 'O' and strType = 'Out';
                  isRedundant = 'Y';
               Endif;
               PSservicer = inServicer;
               If strType = 'In';
                  PSoptFlag = 'I';
               Endif;
               If strType = 'Out';
                  PSoptFlag = 'O';
               Endif;
               PSuser = Q1user;
               PSpgm = Q1pgm;
               PSdt = sysDate7;
               PStime = sysTime6;
               Update pps01;
            Else;
               PSco# = intCo#;
               PSfnd = intFnd;
               PSemp# = intEmp#;
               PSservicer = inServicer;
               PSpaper = 0;
               PSautoPay = 0;
               Clear PSqtExfer;
               Clear PS3ptyUpd;
               PSstat = 'A';
               If strType = 'In';
                  PSoptFlag = 'I';
               Endif;
               If strType = 'Out';
                  PSoptFlag = 'O';
               Endif;
               PSiUsr = Q1user;
               PSiPgm = Q1pgm;
               PSidt = sysDate7;
               PSitim = sysTime6;
               PSuser = Q1user;
               PSpgm = Q1pgm;
               PSdt = sysDate7;
               PStime = sysTime6;
               Write pps01;
            Endif;
            // Write a transaction history record to the history file
            If isRedundant = 'N';
               PSHco#  = intCo#;
               PSHfnd  = intFnd;
               PSHemp# = intEmp#;
               PSHfield = 'PSOPTFLAG';
               If PSoptFlag = 'I';
                  PSHaction = 'Opt in';
               Else;
                  PSHaction = 'Opt out';
               Endif;
               If strDate <> *blanks;
                  Monitor;
                  PSHtrDtDsp = CvtToDate(%Dec(strDate8:8:0));
                  PSHtrDt = DtCyMd(%Dec(strDate8:8:0));
                  On-error;
                  Endmon;
               Else;
               // If Transaction date is not being passed then set to 0
                  Clear PSHtrDtDsp;
                  PSHtrDt = 0;
               Endif;
               // Transaction time
               If strTime6 <> *blanks;
                  Monitor;
                  PSHtrTm = %Dec(strTime6:6:0);
                  On-error;
                  Endmon;
               Else;
               // If Transaction time is not being passed then set to 0
                  Clear PSHtrTm;
               Endif;

               PSHstat = 'A';
               PSHiUsr = Q1user;
               PSHiPgm = Q1pgm;
               PSHiDt = sysDate7;
               PSHiTim = sysTime6;
               PSHuser = Q1user;
               PSHpgm = Q1pgm;
               PSHdt = sysDate7;
               PSHtime = sysTime6;

               Write ppsh01;

            Endif;

       Endsr;

       //=================================================================
       // Generate Output
       //=================================================================
       Begsr BuildOutput;
       Endsr;
       //=================================================================
       // Build Error Message Response
       //=================================================================
       Begsr BldErrorsInfo;

          // Finish by generating the errors
          json += ProcessErrorMessagesJson('*PRE');
          json = %scanrpl(',':'':json:1:1);

       Endsr;

      /End-Free

     p SaveOptOutInAR...
     p                 e
