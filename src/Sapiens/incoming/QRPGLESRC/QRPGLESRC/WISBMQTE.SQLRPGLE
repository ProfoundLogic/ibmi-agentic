     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     h DFTACTGRP(*no) ACTGRP('CONNECT')
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR':'YAJL')
     H*====================================================================
     H* Description:  Submits a quote - updates status and removes other quotes.
     H*
     H* Project: SY406   Date: 11/09/18
     H* Pgmr:    C:65
     H*
     H*====================================================================
     H* inParms -
     H*   name:value
     H*
     H*====================================================================
     H* Revisions:
     H*
     H* 01/16/25  C:300 Support: 118313 (UBIC)
     H*                 This program writing junk data in all Numeric fields
     H*                 of WAQTP file, so initialize those fields
     H*
     H* 10/26/21  C:33  Support: 99007
     H*                 Only return code from email if not blank
     H*====================================================================
     Fwdell1    if   e           k disk    rename(pel01:pel01)
     Fwieml     uf   e           k disk    rename(pem01:pem00i)
     Fwieml1    if   e           k disk    rename(pem01:pem01i)
     Fwmupl     if   e           k disk    rename(pup01:pup00)
     Fwmahl     if   e           k disk    rename(pah01:pah00)
     Fwtqsl     if   e           k disk    rename(pqs01:pqs00)
     Fwaqtl7    uf a e           k disk    rename(pqt01:pqt07)
     Fwmeml     uf   e           k disk    rename(pem01:pem00)
     F                                     prefix(EM)
     F                                     infds(dbfds)
     fwtabl     if   e           k disk
     f                                     Ignore(psy01)
     fwmexl     if   e           k disk    Rename(pex01:pex00)

     D* this size needs to change
     Darray            s               a   len(32767)
     DinJson           s               a   len(2000000) varying
     Djson             s               a   len(8500000) varying
     DjsonSub          s               a   len(8500000) varying
     dcomma            s              1a
     D errMsg          s          99999a   Varying Inz(' ')
     D errMsg500       s            500a   Varying Inz(' ')

     D docNode         s                   like(yajl_val)
     D list            s                   like(yajl_val)
     D node            s                   like(yajl_val)
     D val             s                   like(yajl_val)

     DdsWAQTPnew       ds                  likerec(pqt07:*all)
     DdsWAQTPold       ds                  likerec(pqt07:*all)

     D result          ds                  Qualified
     D  rtnCode                      10a
     D  status                       30a
     D  mode                          1a
     D  co#                           3a
     D  app#                          9a
     D  group                         3a
     D  employer                      9a
     D  year                          3a

     dix               s              2  0
     d isEnvLive       s              1n
     d isEnvQuote      s              1n
     dmode             s              1a
     dqteStatusKey     s              1a
     dqteStatus        s             30a
     dkeyCo#           s              3s 0
     dkeyApp#          s                   like(EMAPP#)
     dkeyFnd           s                   like(ELFnd)
     dkeyEmp#          s                   like(ELEmp#)
     dkeyFyr           s                   like(ELFyr)
     d strEnv          s              4a
     D ##kc            s               n   inz(*off)
     D ##kl            s               n   inz(*off)
     D quoteMode       s              1    inz('Q')
     D process         s               n   inz(*off)
     D strType         s              1    inz
     D delMode         s              1    inz('E')
     d xxRrn           s              9  0
     d sysDate         s               d
     d sysDate7        s              7  0
     d sysTime6        s              6  0
     D printBill       s               n   inz(*off)
     d stRptDiv        s              5  0
     d strAuthority    s             10a
     d xxKey           s             10

     d ##CoC#          s              3
     d ##FndC          s              3
     d ##FyrC          s              3
     d ##EmpC#         s              9
     d ##DivC          s              5
     d ##PostDat       s              8
     d ##EstTrnCd      s              2
     d ##DueDate       s              8
     d ##PrtBal        s              1
     d ##SlfEstFl      s              1
     d ##Call          s              1
     d ##Override      s              1
     d ##SrCode        s              2
     d ##SrAdjCode     s              2
     d ##Quote         s              1
     d ##OutQ          s             10
     d ##NewRenew      s              1
     d ##DartsFlg      s              1
     d ##HardCopy      s              1
     d ##AgentOrder    s              1
     d ##FinalQuote    s              1
     d ##Rmkcode       s              5
     d ##ArCode        s              2
     d ##EffDate       s              8
     d ##Comm1         s              2
     d ##Comm2         s              2
     d ##Comm3         s              2
     d ##FreeComm1     s             30
     d ##FreeComm2     s             30
     d ##FreeComm3     s             30
     d ##ArCode2       s              2

     d* deletion program
     d empDelete       pr                  extpgm('W0050R02')
     d ##kc                                like(##kc)
     d ##kl                                like(##kl)
     d ##mode                              like(mode)
     d ##rrn                               like(xxRrn)
     d ##qte                               like(quoteMode)

     dRtvEnv           pr                  extpgm('RTVENV')
     d outEnv                         4a

     dProcessEmails    pr                  extpgm('WISRMAIL')
     d inRequest                    256    const
     d inUser                        10    const options(*nopass)
     d inParms                         a   len(2000000) varying options(*nopass)
     d outJSON                         a   len(2000000) varying options(*nopass)
     d outRtnCode                    10    options(*nopass)

     d CallW8001C      pr                  ExtPgm('W8001C')
     d ##CoC#                         3    Const
     d ##FndC                         3    Const
     d ##FyrC                         3
     d ##EmpC#                        9
     d ##DivC                         5
     d ##PostDat                      8
     d ##EstTrnCd                     2
     d ##Comm1                        2
     d ##Comm2                        2
     d ##Comm3                        2
     d ##DueDate                      8
     d ##PrtBal                       1
     d ##SlfEstFl                     1
     d ##Call                         1
     d ##Override                     1
     d ##SrCode                       2
     d ##SrAdjCode                    2
     d ##Quote                        1
     d ##OutQ                        10
     d ##NewRenew                     1
     d ##DartsFlg                     1
     d ##HardCopy                     1
     d ##AgentOrder                   1
     d ##FinalQuote                   1
     d ##Rmkcode                      5
     d ##ArCode                       2
     d ##EffDate                      8
     d ##FreeComm1                   30
     d ##FreeComm2                   30
     d ##FreeComm3                   30
     d ##ArCode2                      2

     d dbfds           ds
     d  dbrrn                397    400b 0

     D PSDS          ESDS                  extname(SZQ1P)

      /Free
       Dcl-S jsonEmailRequest VarChar(2000000) Inz(' ');
       Dcl-S jsonEmailReturn VarChar(2000000) Inz(' ');
       Dcl-S strNotificationType VarChar(10);
       Dcl-S strOutRequest Char(256);
       Dcl-S codeEmailReturn Char(10);
      /End-Free

      /copy sPrAtrium
      /copy sPrDate
      /copy sPrJSON
      /copy sPrSecure
      /copy sPrSql
      /copy sPrString
      /copy wPrGroup
      /copy wPrEmplr
      /include yajl_h

      *entry
     DRequestPgm       pr                  ExtPgm('WISBMQTE')
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

       Chain (inUser) pup00;

       Clear json;
       Exsr GetParms;
       If ioRtnCode <> 'Error';
          Exsr UpdateQuote;
       Endif;
       json += ProcessErrorMessagesJson('*PRE');
       json = %scanrpl(',':'':json:1:1);
       outJSON = json;
       *inlr = *on;

       //=================================================================
       // Build a example array
       //=================================================================
       Begsr BuildExampleArray;
        StartJSONarray(array);
        array = %trim(array) + 'arrayValue1,arrayValue2,arrayValue3';
        EndJSONarray(array);
        BuildGiantJSON(json:'myArray':array);
       Endsr;

       //=================================================================
       // Build a example sub-JSON
       //=================================================================
       Begsr BuildExampleSub;
        StartGiantElement(jsonSub);
        BuildGiantJSON(jsonSub:'first':'John');
        BuildGiantJSON(jsonSub:'last':'Smith');
        EndGiantElement(jsonSub);
        StartGiantElement(jsonSub);
        BuildGiantJSON(jsonSub:'first':'Bill');
        BuildGiantJSON(jsonSub:'last':'Jones');
        EndGiantElement(jsonSub);
        BuildGiantJSON(json:'names':jsonSub:'0');
       Endsr;

       //=================================================================
       // Verify the group is valid for Portal
       //=================================================================
       Begsr ChkGroupInPortal;
        Select;
        When isEnvQuote and not UseGroupInPortal(keyCo#:keyFnd:'Quote');
           AddErrorMessageJson('Group ' + %char(keyFnd) +
            ' is not included for service requested ' +
            '(' + %trim(inRequest) + ').':ioRtnCode);
        When isEnvLive and not UseGroupInPortal(keyCo#:keyFnd:'Policy');
           AddErrorMessageJson('Group ' + %char(keyFnd) +
            ' is not included for service requested ' +
            '(' + %trim(inRequest) + ').':ioRtnCode);
        Endsl;
       Endsr;

       //=================================================================
       // Call to WISRMAIL to create email
       //=================================================================
       Begsr CreateEmail;
        strType = Upper(dsWAQTPnew.QTType);
        Exec sql
         select GTkey into :strNotificationType
          from WTGTP
          where GTco# = :keyCo# and GTtbId = 'N003'
           and upper(GTfg10) = :strType;
        If not IsValidFetch(sqlstt);
           Leavesr;
        Endif;
        strOutRequest = 'ImmediateSRNotifications';
        jsonEmailRequest = '{'  +
         '"notificationCategory":"N003",' +
         '"notificationType":"' + %Trim(strNotificationType) + '",' +
         '"company":"' + %EditC(keyCo#:'X') + '",' +
         '"group":"' + %EditC(keyFnd:'X') + '",' +
         '"employer":"' + %EditC(keyEmp#:'X') + '",' +
         '"division":"' + %EditC(ELdiv:'X') + '",' +
         '"year":"' + %EditC(ELfyr:'X') + '",' +
         '"reportDate":"' + %EditC(%Dec(sysDate:*Usa):'X') + '"' +
         '}';
        ProcessEmails(strOutRequest:inUser:jsonEmailRequest:
         jsonEmailReturn:codeEmailReturn);
        //***  Begin Add  ***  10/26/21  ********
        //ioRtnCode = codeEmailReturn;
        If codeEmailReturn <> *blank;
           ioRtnCode = codeEmailReturn;
        Endif;
        //***  End   Add  ***  10/26/21  ********
       Endsr;

       //=================================================================
       // Delete the non-selected quote on application.
       //=================================================================
       Begsr DeleteQuote;

        Setll (EMCo#:EMFnd:EMEmp#) pem00;
        Reade(n) (EMCo#:EMFnd:EMEmp#) pem00;
        Dow Not %Eof(WMEML);
           xxRRN = dbRrn;
           empDelete(##kc:##kl:delMode:xxRRN:quoteMode);
           Reade(n) (EMCo#:EMFnd:EMEmp#) pem00;
        Enddo;

       Endsr;

       //=================================================================
       // Break parms out from inParms
       //=================================================================
       Begsr GetParms;

          Clear result;
          Clear errMsg;
          Clear errMsg500;
          Clear comma;

          Clear keyApp#;
          Clear keyCo#;
          Clear keyFnd;
          Clear keyEmp#;
          Clear keyFyr;

          // Load the json tree
          inJson = inParms;
          docNode = yajl_string_load_tree(inJson : errMsg500);
          errMsg = errMsg500;

          // Parse the parameters if no error from YAJL.
          If errMsg = ' ';

             json = %trim(json) + ' "requestParms" : { ';

             // Load the data structure with the parameter values
             // from the tree

             node = YAJL_object_find(docNode: 'company');
             result.co#       = YAJL_get_string(node);
             BuildGiantJSON(json :'company' :%trim(result.co#));

             node = YAJL_object_find(docNode: 'status');
             result.status    = YAJL_get_string(node);
             BuildGiantJSON(json :'status' :%trim(result.status));

             node = YAJL_object_find(docNode: 'mode');
             result.mode      = YAJL_get_string(node);
             BuildGiantJSON(json :'mode' :%trim(result.mode));

             node = YAJL_object_find(docNode: 'app#');
             result.app#     = YAJL_get_string(node);
             BuildGiantJSON(json :'app#' :%trim(result.app#));

             node = YAJL_object_find(docNode: 'group');
             result.group    = YAJL_get_string(node);
             BuildGiantJSON(json :'group' :%trim(result.group));

             node = YAJL_object_find(docNode: 'employer');
             result.employer = YAJL_get_string(node);
             BuildGiantJSON(json :'employer' :%trim(result.employer));

             node = YAJL_object_find(docNode: 'year');
             result.year     = YAJL_get_string(node);
             BuildGiantJSON(json :'year' :%trim(result.year));

             json = %trim(json) + ' }, ';

             // Free up the space by removing the tree
             yajl_tree_free(docNode);
          Else;
             AddErrorMessageJson('JSON filter can''t be read.':ioRtnCode);
             Leavesr;
          EndIf;

          If strAuthority = '*View';
             AddErrorMessageJson('User, ' + %trim(inUser) +
              ', not authorized to process ' +
              'request (' + %trim(inRequest) + ').'
              :ioRtnCode);
             Leavesr;
          Endif;

          //* Company
          If result.co# <> ' ';
             Monitor;
                keyCo# = %int(result.co#);
             On-error;
                AddErrorMessageJson('Invalid co# ' +
                 'received in incoming parameters: ' + result.co#:ioRtnCode);
             Endmon;
          Else;
             keyCo# = UPCo#;
          Endif;

          //* Update/Submit Mode
          If result.mode <> ' ';
             mode = %trim(result.mode);
          Else;
             Clear mode;
          Endif;

          //* Quote Status
          If result.status <> ' ';
             qteStatus = %trim(result.status);
             Setll (keyCo#:'Q') pqs00;
             Reade (keyCo#:'Q') pqs00;
             Dow not %eof and QSDesc <> qteStatus;
                Reade (keyCo#:'Q') pqs00;
             Enddo;
             If not %eof;
                qteStatusKey = QSKey;
             Else;
                Clear qteStatusKey;
             Endif;
          Else;
             Clear qteStatusKey;
          Endif;

          If qteStatusKey = ' ' and mode = ' ';
             AddErrorMessageJson('Status must be received in incoming ' +
              'parameters for updating just status.':ioRtnCode);
          Endif;

          If qteStatusKey = ' ';
             qteStatusKey = 'U';
          Endif;

          //* Group
          If result.group <> ' ';
             Monitor;
                keyFnd  = %int(result.group);
             On-error;
                AddErrorMessageJson('Invalid group ' +
                 'received in incoming parameters: ' + result.group:ioRtnCode);
             Endmon;
          Else;
             Clear keyFnd;
          Endif;

          //* Employer
          If result.employer <> ' ';
             Monitor;
                keyEmp# = %int(result.employer);
             On-error;
                AddErrorMessageJson('Invalid employer ' +
                 'received in incoming parameters: '+result.employer:ioRtnCode);
             Endmon;
          Else;
             Clear keyEmp#;
          Endif;

          //* Year
          If result.year <> ' ';
             Monitor;
                keyFyr = %int(result.year);
             On-error;
                AddErrorMessageJson('Invalid year ' +
                 'received in incoming parameters: ' + result.year:ioRtnCode);
             Endmon;
          Else;
             Clear keyFyr;
          Endif;

          //* Application #
          If result.app# <> ' ';
             Monitor;
                keyApp# = %int(result.app#);
             On-error;
                AddErrorMessageJson('Invalid app# ' +
                 'received in incoming parameters: ' + result.app#:ioRtnCode);
             Endmon;
          Else;
             Clear keyApp#;
             //* Check if single employer has been passed in, if not then
             //* App# is required.
             If keyFnd = 0 and keyEmp# = 0 and keyFyr = 0;
                AddErrorMessageJson('App# must be ' +
                 'received in incoming parameters: ' + result.app#:ioRtnCode);
             Endif;
          Endif;

          //* If App# is not received, check if one exists for employer.
          If keyApp# = 0;
             Chain(n) (keyCo#:keyFnd:keyEmp#:keyFyr) pem01i;
             If %found;
                keyApp# = EMApp#;
             Endif;
          Endif;

          //* Verify employer exists if passed in as single.
          If keyApp# = 0;
             Setll (keyCo#:keyFnd:keyFyr:keyEmp#) pel01;
             Reade (keyCo#:keyFnd:keyFyr:keyEmp#) pel01;
             If %eof;
                AddErrorMessageJson('Individual employer passed in ' +
                 'was not valid: ' + result.group + '/' + result.employer +
                  '/' + result.year:ioRtnCode);
             Endif;
          Endif;

       Endsr;

       //=================================================================
       Begsr PgmStart;
        strAuthority = ioRtnCode;
        Clear ioRtnCode;
        RtvEnv(strEnv);
        isEnvQuote = strEnv = '*QTE';
        isEnvLive = strEnv = '*LIV';
        sysDate  = %Date();
        sysDate7 = %Dec(sysDate:*Cymd);
        sysTime6 = %Dec(%Time():*hms);
       Endsr;

       //=================================================================
       // Call to submit the quote bill to iDarts
       //=================================================================
       Begsr PrintQteBill;

         Chain(n) (keyCo#:keyFnd) pex00;
         If %found and EXQte <> ' ';
            ##Rmkcode = EXQte;
         Else;
            Leavesr;
         Endif;

         Chain(n) (keyCo#:keyFnd:keyFyr:keyEmp#:stRptDiv) pel01;

         ##CoC#  =  %Editc(keyCo#:'X');
         ##FndC  =  %Editc(keyFnd:'X');
         ##FyrC  =  %Editc(keyFyr:'X');
         ##EmpC# =  %Editc(keyEmp#:'X');
         ##DivC  =  %Editc(stRptDiv:'X');

         If sysDate7 > ELBsdt;
            ##PostDat = %EditC(DtMDYY(sysDate7):'X');
         Else;
            ##PostDat = %EditC(DtMDYY(ELBsdt):'X');
         Endif;

         ##EstTrnCd   =  '  ';
         ##DueDate    =  '00000000';
         ##PrtBal     =  'Y';

         xxKey = %EditC(ELbsCd:'X');
         Chain (keyCo#:xxKey) pbs01;
         If %Found(wtabl);
            ##SlfEstFl = BSsefl;
         Else;
            ##SlfEstFl = 'E';
         EndIf;

         ##Call    = ' ';
         ##Override   = ' ';
         ##SrCode     = '  ';
         ##SrAdjCode  = '  ';
         ##Quote      = 'Q';
         ##OutQ       = UPoutQ;
         ##NewRenew = ' ';

         ##DartsFlg   = 'Y';
         ##HardCopy   = 'N';
         ##AgentOrder = 'N';
         ##FinalQuote = ' ';
         ##ArCode     = ' ';
         ##EffDate    = '00000000';
         ##Comm1      = ' ';
         ##Comm2      = ' ';
         ##Comm3      = ' ';
         ##FreeComm1  = ' ';
         ##FreeComm2  = ' ';
         ##FreeComm3  = ' ';
         ##ArCode2    = ' ';

         CallW8001C(##CoC#:##FndC:##FyrC:##EmpC#:##DivC:##PostDat:
          ##EstTrnCd:##Comm1:##Comm2:##Comm3:##DueDate:##PrtBal:##SlfEstFl:
          ##call:##Override:##SrCode:##SrAdjCode:##Quote:##OutQ:##NewRenew:
          ##DartsFlg:##HardCopy:##AgentOrder:##FinalQuote:##RmkCode:##ArCode:
          ##EffDate:##FreeComm1:##FreeComm2:##FreeComm3:##ArCode2);

       Endsr;

       //=================================================================
       // Release policy for underwriting evaluation
       //=================================================================
       Begsr ReleasePolicy;

        printBill = *on;

        // Remove lock
        Chain (keyCo#:keyFnd:keyEmp#:ELDiv) pem00;
        If %found;
           Clear EMEMFQLK;
           Update pem00;
           If EMEMStrp = 'Y';
              stRptDiv = EMEMDiv;
           Endif;
        Endif;

        // Delete waiver records
        Exec sql
         delete from WMWVP
          where WVco# = :ELco#
           and WVfnd = :ELfnd
           and WVemp# = :ELemp#
           and WVdiv = :ELdiv
           and WVfyr = :ELfyr
           and WVname = 'Delete and Rewrite';

       Endsr;

       //=================================================================
       // Read through data based parameters
       //=================================================================
       Begsr UpdateQuote;

        Clear comma;
        json = %trim(json) + ' "changeStatusResult" : {';

        //* Process application
        If keyApp# <> 0;
           Clear keyFnd;
           Clear keyEmp#;
           Clear keyFyr;

           Exec sql
            select EMfnd into :keyFnd
             from WIEMP
             where EMco# = :keyCo# and EMapp# = :keyApp# and EMcurrent = 'Y';
           Select;
           When keyFnd = *zero;
              AddErrorMessageJson('No ''currently selected'' group available ' +
               'for selected application.':ioRtnCode);
           Other;
              Exsr ChkGroupInPortal;
           Endsl;

           Setll (keyCo#:keyApp#) pem00i;
           Reade (keyCo#:keyApp#) pem00i;

           Dow not %eof and ioRtnCode <> 'Error';
              If mode = 'Y';
             //* If submitting quote, only update selected and delete rest.
                 If EMCurrent = 'Y';
                    keyFnd  = EMFnd;
                    keyEmp# = EMEmp#;
                    keyFyr  = EMFyr;
                    EMStat  = 'A';
                    Exsr UpdateQteStatus;
                 Else;
                    EMStat  = 'I';
                 Endif;
                 Update pem00i;
                 If EMStat = 'I';
                    Exsr DeleteQuote;
                 Endif;
              Else;
             //* If just changing status, update all quotes.
                 keyFnd  = EMFnd;
                 keyEmp# = EMEmp#;
                 keyFyr  = EMFyr;
                 Exsr UpdateQteStatus;
              Endif;
              Reade (keyCo#:keyApp#) pem00i;
           Enddo;
        Else;
           Exsr ChkGroupInPortal;
        //* Process single employer
           If ioRtnCode <> 'Error';
              Exsr UpdateQteStatus;
           Endif;
        Endif;

        If ioRtnCode <> 'Error';
           json = %trim(json) + ' "success" : true ';

           If mode = 'Y';
              json = %trim(json) + ', "submit" : true ';
           Endif;
        Else;
           json = %trim(json) + ' "success" : false ';
        Endif;

        json = %trim(json) + ' }';

       Endsr;

       //=================================================================
       // Update the quote status.
       //=================================================================
       Begsr UpdateQteStatus;

        Setll (keyCo#:keyFnd:keyFyr:keyEmp#) pel01;
        Reade (keyCo#:keyFnd:keyFyr:keyEmp#) pel01;

        printBill = *off;
        stRptDiv = ELDiv;

        Dow not %eof;

           //* Unlock the quote
           If  SetFQLockSts(ELco#:qteStatusKey:ELiUsr) = ' ';
              Exsr ReleasePolicy;
           Endif;

           Setgt (keyCo#:keyFnd:keyEmp#:ELDiv:keyFyr) pah00;
           Readpe (keyCo#:keyFnd:keyEmp#:ELDiv:keyFyr) pah00;
           If %eof;
              AddErrorMessageJson('Agent information not avaliable to update ' +
               'status.':ioRtnCode);
           Endif;
           If not %eof;
              Setll (keyCo#:keyFnd:keyEmp#:ELDiv:'Q':AHAgt#) pqt07;
              Reade (keyCo#:keyFnd:keyEmp#:ELDiv:'Q':AHAgt#) pqt07 dsWAQTPold;
              If %eof;
                 Chain(n) (keyCo#:keyFnd:keyEmp#:ELDiv) pem00;
                 If %found;
                    // Write a record
                    dsWAQTPnew.QTco#  = EMEMco#;
                    dsWAQTPnew.QTfnd  = EMEMfnd;
                    dsWAQTPnew.QTemp# = EMEMemp#;
                    dsWAQTPnew.QTdiv  = EMEMdiv;
                    dsWAQTPnew.QTagt# = AHagt#;
                    dsWAQTPnew.QTname = EMEMname;
                    dsWAQTPnew.QTchap = EMEMchap;
                    dsWAQTPnew.QTefdt = EMEMefd1;
                    dsWAQTPnew.QTwkdt = sysDate7;
                    dsWAQTPnew.QTordt = sysDate7;
                    dsWAQTPnew.QTtype = ' ';
                    dsWAQTPnew.QTstat = 'Q';
                    dsWAQTPnew.QTpgm  = Q1pgm;
                    dsWAQTPnew.QTuser = inUser;
                    dsWAQTPnew.QTdt   = sysDate7;
                    dsWAQTPnew.QTtime = sysTime6;
                    dsWAQTPnew.QTiPgm = dsWAQTPnew.QTpgm;
                    dsWAQTPnew.QTiUsr = dsWAQTPnew.QTuser;
                    dsWAQTPnew.QTiDt  = dsWAQTPnew.QTdt;
                    dsWAQTPnew.QTiTm  = dsWAQTPnew.QTtime;
                 //***  Begin Add  ***  01/16/25  ******************************
                    dsWAQTPnew.QTprem = 0;
                    dsWAQTPnew.QTmod  = 0;
                    dsWAQTPnew.QTarap = 0;
                    dsWAQTPnew.QThlr  = 0;
                    dsWAQTPnew.QTplr  = 0;
                    dsWAQTPnew.QTnbdt = 0;
                    dsWAQTPnew.QTrvdt = 0;
                    dsWAQTPnew.QTbfpr = 0;
                    dsWAQTPnew.QTfxdt = 0;
                    dsWAQTPnew.QTpprm = 0;
                 //***  End   Add  ***  01/16/25  ******************************
                    Write pqt07 dsWAQTPnew;
                 Endif;
                Setll (keyCo#:keyFnd:keyEmp#:ELDiv:'Q':AHAgt#) pqt07;
                Reade (keyCo#:keyFnd:keyEmp#:ELDiv:'Q':AHAgt#) pqt07 dsWAQTPold;
              Endif;
              If %eof;
                 AddErrorMessageJson('Employer information not avaliable to ' +
                  'update status.':ioRtnCode);
              Endif;
              Dow not %eof;
                 dsWAQTPnew = dsWAQTPold;
                 dsWAQTPnew.QTType = qteStatusKey;
                 dsWAQTPnew.QTPgm  = q1pgm;
                 dsWAQTPnew.QTUser = inUser;
                 dsWAQTPnew.QTDt   = sysDate7;
                 dsWAQTPnew.QTTime = sysTime6;
                 Update pqt07 dsWAQTPnew;
                 QuoteStsChgHst(dsWAQTPnew:dsWAQTPold);
                 If dsWAQTPold.QTType <> dsWAQTPnew.QTType;
                    Exsr CreateEmail;
                 Endif;
                 Reade (keyCo#:keyFnd:keyEmp#:ELDiv:'Q':AHAgt#) pqt07;
              Enddo;
           Endif;
           Reade (keyCo#:keyFnd:keyFyr:keyEmp#) pel01;
        Enddo;

        If printBill;
           Exsr PrintQteBill;
        Endif;

       Endsr;

      /End-free
