     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR':'YAJL')
     H*====================================================================
     H* Description:  Web Service program for returning A/R transactions,
     H*               list of policies with billing A/R, etc.
     H*
     H* Project: SY409   Date: 11/12/19
     H* Pgmr:    C:65
     H*
     H*====================================================================
     H* inParms -
     H*   name:value
     H*
     H*====================================================================
     H* Revisions:
     H*
     H* 02/25/22 C:65  Project: Phw#101112 (UBIC)
     H*                Change the edit code on billed/paid a/r records to
     H*                use edit code 'J' instead of '3' to include - sign.
     H*
     H*====================================================================
     Fwmagl     if   e           k disk    rename(pag01:pag00)
     Fwmupl     if   e           k disk    rename(pup01:pup00)
     Fwmfnl     if   e           k disk    rename(pfn01:pfn00)
     Fwmeml     if   e           k disk    rename(pem01:pem00)
     Fwmeml12   if   e           k disk    rename(pem01:pem12)
     Fwmrpl3    if   e           k disk    rename(prp01:prp03)
     Fwmeml19   if   e           k disk    rename(pem01:pem19) prefix(E2:2)
     Fwdell     if   e           k disk    rename(pel01:pel00)
     Fwmagp     if   e             disk    rename(pag01:pag0p)
     Fwmahp     if   e             disk    rename(pah01:pah0p)
     Fwmeal6    if   e           k disk    rename(pea01:pea06)

     D* this size needs to change
     Darray            s               a   len(32767)
     DinJson           s               a   len(2000000) varying
     Djson             s               a   len(8500000) varying
     DjsonSub          s               a   len(8500000) varying
     dcomma            s              1a
     D errMsg          s          99999a   Varying Inz(' ')
     D errMsg500       s            500a   Varying Inz(' ')

     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRST-
     D                                     UVWXYZ')
     D LO              C                   CONST('abcdefghijklmnopqrst-
     D                                     uvwxyz')

     D docNode         s                   like(yajl_val)
     D i0              s             10i 0 inz
     D i1              s             10i 0 inz
     D key             s             50a   varying
     D list            s                   like(yajl_val)
     D node            s                   like(yajl_val)
     D node0           s                   like(yajl_val)
     D node1           s                   like(yajl_val)
     D val             s                   like(yajl_val)

     d sysDate         s               d   datfmt(*iso)
     d sysDate7        s              7  0
     d sysTime6        s              6  0

     d clientID        s              3a
     d isProcess       s              1n   inz
     d intDate8        s              8  0
     D arrGrp          s              3  0 dim(999)
     d indexGrp        s              3  0
     D arrAgtFein      s              9  0 dim(9999)
     D arrAgtMod       s              3  0 dim(9999)
     d indexAgt        s              5  0
     d i               s              5  0

     D result          ds                  Qualified inz
     D  rtnCode                      10a
     D  fndName                      60a
     D  co#Fnd                        6a
     D  agtName                      40a
     D  agtFeinMod                   12a
     D  insName                     100a
     D  insFein                       9a
     D  insPol#                      25a
     D  insStatus                    25a
     D  fromPostDt                    8a
     D  toPostDt                      8a

     dkeyCo#           s              3s 0
     dkeyFnd           s              3s 0
     dkeyFndName       s             60a   varying inz
     dkeyAgtFein       s              9s 0
     dkeyAgtMod        s              3s 0
     dkeyAgtName       s             40a   varying inz
     dkeyEmpName       s            100a   varying inz
     dkeyEmpFein       s              9s 0
     dkeyStatus        s             25a
     dkeyPol#          s             25a
     dkeyFromDate      s              7s 0
     dkeyToDate        s              7s 0

     dintCo#           s              3  0
     dintFnd           s              3s 0
     dintFyr           s              3s 0
     dintEmp#          s              9s 0
     dintDiv           s              5s 0
     disEnvQuote       s              1n
     disEnvLive        s              1n
     d strAuthority    s             10a
     dstrCo#           s              3a
     dstrEnv           s              4a
     dstrSql           s           5000    varying inz
     dxagrrn           s              9  0
     dxaarrn           s              9  0
     dxahrrn           s              9  0
     dstrAgtName       s             40a
     dstrStatus        s             25a
     dintCxDt          s              7s 0
     dxxBilled         s             13s 2
     dxxPaid           s             13s 2

     D PSDS          ESDS                  extname(SZQ1P)

      /copy sPrAtrium
      /copy sPrDate
      /copy sPrJSON
      /copy sPrSecure
      /copy sPrSql
      /copy sPrString
      /copy wPrAgent
      /copy wPrAR
      /Copy wPrEmplr
      /copy wPrGroup
      /Copy wPrUsrPrms
      /include yajl_h

     dGetClientID      pr                  ExtPgm('RTVCLTID')
     d clientID                       3a

     dRtvEnv           pr                  extpgm('RTVENV')
     d outEnv                         4a

      *entry
     DRequestPgm       pr                  ExtPgm('WIBILLAR')
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
     D ioRtCd                        10

      /Free
       Exec Sql Set Option Commit = *None;
       Exsr PgmStart;

       Chain (inUser) pup00;

       Clear json;
       Select;
       When inRequest = 'ListBilling';
          Exsr GetParms;
          Exsr BuildOutputList;
       When inRequest = 'ListBillingDetail';
          ListBillingDetail();
       Endsl;
       json += ProcessErrorMessagesJson('*PRE');
       json = %scanrpl(',':'':json:1:1);
       outJSON = json;
       *inlr = *on;

       //=================================================================
       // Read through data based parameters
       //=================================================================
       Begsr BuildOutputList;
        If ioRtCd = 'Error';
           Leavesr;
        Endif;

        Clear comma;
        json += ' "billingList" : [';

        Select;
        When keyPol# <> ' ';
           Chain (keyCo#:keyPol#) prp03;
           If %found;
              intFnd = RPFnd;
              intEmp# = RPEmp#;
              intFyr = RPFyr;
           Else;
              Monitor;
                 intFnd  = %int(%subst(keyPol#:1:3));
                 intEmp# = %int(%subst(keyPol#:4:9));
                 intFyr  = %int(%subst(keyPol#:13:3));
              On-error;
                 json = %trim(json) + ' ]';
                 Leavesr;
              Endmon;
           Endif;
           Setll (keyCo#:intFnd:intEmp#) pem00;
           Reade (keyCo#:intFnd:intEmp#) pem00;
        When keyEmpFein <> 0;
           Setll (keyCo#:keyEmpFein) pem12;
           Reade (keyCo#:keyEmpFein) pem12;
        When keyFnd <> 0;
           Setll (keyCo#:keyFnd) pem00;
           Reade (keyCo#:keyFnd) pem00;
        Other;
           Setll (keyCo#) pem00;
           Reade (keyCo#) pem00;
        Endsl;

        Dow not %eof;

           //* Check Filters not in key list.
           isProcess = *on;

           //* Check if employer matches group name.
           If keyFndName <> ' ' and %lookup(EMFnd:arrGrp) = 0;
              isProcess = *off;
           Endif;

           //* Check if employer matches group name.
           If keyFnd = 0 and isProcess;
              Select;
              When isEnvQuote and not UseGroupInPortal(keyCo#:EMFnd:'Quote');
                 isProcess = *off;
              When isEnvLive and not UseGroupInPortal(keyCo#:EMFnd:'Policy');
                 isProcess = *off;
              Endsl;
           Endif;

           //* Check if employer matches policy name.
           If keyEmpName <> ' ' and isProcess;
              Setll (EMCo#:EMFnd:EMEmp#) pem19;
              Reade (EMCo#:EMFnd:EMEmp#) pem19;
              Dow not %eof;
                 If %scan(%trim(Upper(keyEmpName)):Upper(E2Name+E2Nam2)) <> 0 or
                  %scan(%trim(Upper(keyEmpName)):Upper(E2Fdnm+E2Fdn2)) <> 0;
                    Leave;
                 Endif;
                 Reade (EMCo#:EMFnd:EMEmp#) pem19;
              Enddo;
              If %eof;
                 isProcess = *off;
              Endif;
           Endif;

           //* Passed employer filters, check policy years.
           If isProcess;
              Exsr CheckEmployer;
           Endif;

           //* Read next employer
           Select;
           When keyPol# <> ' ';
              json = %trim(json) + ' ]';
              Leavesr;
           When keyEmpFein <> 0;
              intFnd = EMFnd;
              intEmp# = EMEmp#;
              Reade (keyCo#:keyEmpFein) pem12;
              Dow not %eof and (EMFnd = intFnd and EMEmp# = intEmp#);
                 Reade (keyCo#:keyEmpFein) pem12;
              Enddo;
           When keyFnd <> 0;
              SetGt (keyCo#:keyFnd:EMEmp#) pem00;
              Reade (keyCo#:keyFnd) pem00;
           Other;
              SetGt (keyCo#:EMFnd:EMEmp#) pem00;
              Reade (keyCo#) pem00;
           Endsl;

        Enddo;

        json = %trim(json) + ' ]';

       Endsr;

       //=================================================================
       // Check if any policy years on employer has matching A/R.
       //=================================================================
       Begsr CheckEmployer;

        intDiv = GetStRptDv(EMCo#:EMFnd:EMEmp#);

        Select;
        When intFyr <> 0;
           Setll (EMCo#:EMFnd:EMEmp#:intDiv:intFyr) pel00;
           Reade (EMCo#:EMFnd:EMEmp#:intDiv:intFyr) pel00;
        Other;
           Setll (EMCo#:EMFnd:EMEmp#:intDiv) pel00;
           Reade (EMCo#:EMFnd:EMEmp#:intDiv) pel00;
        Endsl;

        Dow not %eof;

           isProcess = *on;

           If keyAgtFein <> 0 or keyAgtName <> ' ';

              GetEmpAgt(EMCo#:EMFnd:EMEmp#:intDiv:ELFyr:0:
               strAgtName:xahrrn:xaarrn:xagrrn);

              Chain xagrrn pag0p;
              If not %found;
                 Clear AGName;
                 Clear AGFein;
                 Clear AGMod;
              Endif;

           Endif;

           //* Check if employer matches agency.
           If keyAgtFein <> 0 and isProcess;

              If AGFein <> keyAgtFein or AGMod <> keyAgtMod;
                 isProcess = *off;
              Endif;

           Endif;

           //* Check if employer matches agency name.
           If keyAgtName <> ' ' and isProcess;

              i = 1;
              isProcess = *off;

              Dow i <= indexAgt;
                 If AGFein = arrAgtFein(i) and AGMod = arrAgtMod(i);
                    isProcess = *on;
                    Leave;
                 Endif;
                 i = i + 1;
              Enddo;

           Endif;

           //* Determine policy status.
           intCxDt = GetCxDate(EMCo#:EMFnd:EMEmp#:intDiv);

           Select;
           When intCxDt <> 0 and intCxDt <= sysDate7 and intCxDt <= ELPTDT;
              strStatus = 'Cancelled';
           When intCxDt <> 0 and intCxDt > sysDate7 and intCxDt > ELPFDT and
            intCxDt <= ELPTDT;
              strStatus = 'Pending Cancellation';
           When ELPTDT <= sysDate7;
              strStatus = 'Expired';
           When ELPFDT > sysDate7;
              strStatus = 'Pending';
           Other;
              strStatus = 'Active';
           Endsl;

           //* Check if employer matches policy status.
           If keyStatus <> ' ' and isProcess;

              If strStatus <> keyStatus;
                 isProcess = *off;
              Endif;

           Endif;

           //* Check if employer has A/R in posting date range.
           If (keyFromDate <> 0 or keyToDate <> 9999999) and isProcess;

              Setgt (ELCo#:ELFnd:ELEmp#:ELDiv:ELFyr:keyToDate) pea06;
              Readpe (ELCo#:ELFnd:ELEmp#:ELDiv:ELFyr) pea06;
              If %eof or EAPodt < keyFromDate;
                 isProcess = *off;
              Endif;

           Endif;

           //* Passed year level filters, build record.
           If isProcess;
              Exsr BuildRecord;
           Endif;

           //* Process next year on employer.
           Select;
           When intFyr <> 0;
              Leavesr;
           Other;
              Reade (EMCo#:EMFnd:EMEmp#:intDiv) pel00;
           Endsl;
        Enddo;

       Endsr;

       //=================================================================
       // Build record results
       //=================================================================
       Begsr BuildRecord;

        If comma <> ' ';
           json = %trim(json) + comma;
        Else;
           comma = ',';
        Endif;

        //* Total policy A/R
        Exsr TotalAR;

        //* Write detail record for policy year
        json = %trim(json) + ' { ' +
         PolicyJSONData(ELCo#:ELFnd:ELEmp#:ELDiv:ELFyr) + ',' +
         BuildJsonNode('status':%trim(strStatus)) + ',' +
         BuildJsonNode('billed':%editc(xxBilled:'J')) + ',' +
         BuildJsonNode('paid':%editc(xxPaid:'J')) + ',' +
         BuildJsonNode('outstanding':%editc((xxBilled - xxPaid):'J')) +
         ' } ';

       Endsr;

       //=================================================================
       // Total A/R for policy year.
       //=================================================================
       Begsr TotalAR;

        Clear xxBilled;
        Clear xxPaid;

        Setgt (ELCo#:ELFnd:ELEmp#:ELDiv:ELFyr:sysDate7) pea06;
        Readpe (ELCo#:ELFnd:ELEmp#:ELDiv:ELFyr) pea06;
        Dow not %eof;
           Select;
           When EAArty = 'B' or EAArty = 'A' or EAArty = 'H' or EAArty = 'D'
            or EAArty = 'F';
              xxBilled = xxBilled + EAAmt;
           Other;
              If EAArty = 'N' or EAArty = 'T' or EAArty = 'R';
                 xxPaid = xxPaid - EAAmt;
              Else;
                 xxPaid = xxPaid + EAAmt;
              Endif;
           Endsl;

           Readpe (ELCo#:ELFnd:ELEmp#:ELDiv:ELFyr) pea06;
        Enddo;

       Endsr;

       //=================================================================
       // Break parms out from inParms
       //=================================================================
       Begsr GetParms;

          Reset result;
          Clear errMsg;
          Clear errMsg500;
          Clear comma;

          // Load the json tree
          inJson = inParms;
          docNode = yajl_string_load_tree(inJson : errMsg500);
          If errMsg500 <> ' ';
             AddErrorMessageJson(errMsg500:ioRtCd);
             Leavesr;
          Endif;

          // Parse the parameters if no error from YAJL.

          json = %trim(json) + ' "requestParms" : { ';

          // Load the data structure with the parameter values
          // from the tree
          Dow YAJL_object_loop(docNode:i1:key:val);
             BuildGiantJSON(json :key :%trim(YAJL_get_string(val)));
             Select;
             When key = 'rtnCode';
                result.rtnCode = YAJL_get_string(val);
             When key = 'groupName';
                result.fndName = YAJL_get_string(val);
             When key = 'groupCode';
                result.co#Fnd = YAJL_get_string(val);
             When key = 'agentName';
                result.agtName = YAJL_get_string(val);
             When key = 'agentCode';
                result.agtFeinMod = YAJL_get_string(val);
             When key = 'insuredName';
                result.insName = YAJL_get_string(val);
             When key = 'insuredCode';
                result.insFein = YAJL_get_string(val);
             When key = 'insuredPolicy#';
                result.insPol# = YAJL_get_string(val);
             When key = 'insuredStatus';
                result.insStatus = YAJL_get_string(val);
             When key = 'fromPostDate';
                result.fromPostDt = YAJL_get_string(val);
             When key = 'toPostDate';
                result.toPostDt = YAJL_get_string(val);
             Endsl;
          Enddo;

          json = %trim(json) + ' }, ';

          // Free up the space by removing the tree
          yajl_tree_free(docNode);

          Clear keyFnd;
          //* Group Name
          If result.fndName <> ' ';
             keyFndName = %trim(result.fndName);
             Setll (keyCo#) pfn00;
             Reade (keyCo#) pfn00;
             Dow not %eof;
                If %scan(%trim(Upper(keyFndName)):Upper(FNName)) <> 0;
                   indexGrp = indexGrp + 1;
                   arrGrp(indexGrp) = FNFnd;
                Endif;
                Reade (keyCo#) pfn00;
             Enddo;
             //* If only one group is found, then set key field intsead.
             If indexGrp = 1;
                keyFnd = arrGrp(1);
                Clear arrGrp;
                Clear indexGrp;
                Clear keyFndName;
             Endif;
          Else;
             Clear keyFndName;
          Endif;

          //* Group Code
          If result.co#Fnd <> ' ';
             Monitor;
                keyCo# = %int(%subst(result.co#Fnd:1:3));
                keyFnd = %int(%subst(result.co#Fnd:4:3));
             On-error;
                AddErrorMessageJson('Invalid group code received ' +
                 'in incoming parameters: ' + result.co#Fnd:ioRtCd);
                Leavesr;
             Endmon;
          Endif;

          If keyFnd <> 0;
             Select;
             When isEnvQuote and not UseGroupInPortal(keyCo#:keyFnd:'Quote');
                AddErrorMessageJson('Group ' + %char(keyFnd) +
                 ' is not included for service requested ' +
                 '(' + %trim(inRequest) + ').':ioRtCd);
             When isEnvLive and not UseGroupInPortal(keyCo#:keyFnd:'Policy');
                AddErrorMessageJson('Group ' + %char(keyFnd) +
                 ' is not included for service requested ' +
                 '(' + %trim(inRequest) + ').':ioRtCd);
             Endsl;
          Endif;

          If ioRtCd = 'Error';
             Leavesr;
          Endif;

          Clear keyAgtFein;
          Clear keyAgtMod;
          //* Agency Name
          If result.agtName <> ' ';
             keyAgtName = %trim(result.agtName);
             Setll (keyCo#) pag00;
             Reade (keyCo#) pag00;
             Dow not %eof;
                If %scan(%trim(Upper(keyAgtName)):Upper(AGName)) <> 0;
                   indexAgt = indexAgt + 1;
                   arrAgtFein(indexAgt) = AGFein;
                   arrAgtMod(indexAgt) = AGMod;
                Endif;
                Reade (keyCo#) pag00;
             Enddo;
             //* If only one agency is found, then set key field intsead.
             If indexAgt = 1;
                keyAgtFein = arrAgtFein(1);
                keyAgtMod = arrAgtMod(1);
                Clear arrAgtFein;
                Clear arrAgtMod;
                Clear indexAgt;
                Clear keyAgtName;
             Endif;
          Else;
             Clear keyAgtName;
          Endif;

          //* Agency Code
          If result.agtFeinMod <> ' ';
             Monitor;
                keyAgtFein = %int(%subst(result.agtFeinMod:1:9));
                keyAgtMod = %int(%subst(result.agtFeinMod:10:3));
             On-error;
                AddErrorMessageJson('Invalid agent fein/mod received ' +
                 'in incoming parameters: ' + result.agtFeinMod:ioRtCd);
             Endmon;
          Endif;

          //* Insured Name
          If result.insName <> ' ';
             keyEmpName = %trim(result.insName);
          Else;
             Clear keyEmpName;
          Endif;

          //* Insured Code(fein)
          If result.insFein <> ' ';
             Monitor;
                keyEmpFein = %int(result.insFein);
             On-error;
                AddErrorMessageJson('Invalid employer fein received ' +
                 'in incoming parameters: ' + result.insFein:ioRtCd);
             Endmon;
          Else;
             Clear keyEmpFein;
          Endif;

          //* Insured Policy #
          If result.insPol# <> ' ';
             keyPol# = %trim(result.insPol#);
          Else;
             Clear keyPol#;
          Endif;

          //* Policy Status
          If result.insStatus <> ' ';
             keyStatus = %trim(result.insStatus);
          Else;
             Clear keyStatus;
          Endif;

          //* Posting From Date
          If result.fromPostDt <> ' ';
             Monitor;
                intDate8 =  %Int(result.fromPostDt);
                intDate8 = dtCheck(intDate8);
                If intDate8 = 0;
                   AddErrorMessageJson('Invalid Posting From date received ' +
                    'in incoming parameters: ' + result.fromPostDt:ioRtCd);
                Else;
                   keyFromDate = dtCYMD(intDate8);
                Endif;
             On-error;
                AddErrorMessageJson('Invalid Posting From date received ' +
                 'in incoming parameters: ' + result.fromPostDt:ioRtCd);
             Endmon;
          Else;
             Clear keyFromDate;
          Endif;

          //* Posting To Date
          If result.toPostDt <> ' ';
             Monitor;
                intDate8 =  %Int(result.toPostDt);
                intDate8 = dtCheck(intDate8);
                If intDate8 = 0;
                   AddErrorMessageJson('Invalid Posting To date received ' +
                    'in incoming parameters: ' + result.toPostDt:ioRtCd);
                Else;
                   keyToDate = dtCYMD(intDate8);
                Endif;
             On-error;
                AddErrorMessageJson('Invalid Posting To date received ' +
                 'in incoming parameters: ' + result.toPostDt:ioRtCd);
             Endmon;
          Else;
             keyToDate = 9999999;
          Endif;

       Endsr;

       //--------------------------------------------------------
       Begsr PgmStart;
       //--------------------------------------------------------
        strAuthority = ioRtCd;
        Clear ioRtCd;
        keyCo# = UserCo#(inUser);
        intCo# = UserCo#(inUser);
        strCo# = %Editc(intCo#:'X');
        GetClientID(clientID);
        sysDate  = %Date();
        sysDate7 = %Dec(sysDate:*Cymd);
        sysTime6 = %Dec(%Time():*hms);
        RtvEnv(strEnv);
        isEnvQuote = strEnv = '*QTE';
        isEnvLive = strEnv = '*LIV';
        Clear arrGrp;
        Clear indexGrp;
        Clear arrAgtFein;
        Clear arrAgtMod;
        Clear indexAgt;
        Clear intFnd;
        Clear intEmp#;
        Clear intFyr;
        Clear intDiv;
       Endsr;

      /End-free
       //========================================
       // Build Billing Detail Information
       //========================================
     PListBillingDetail...
     P                 b

     ddecCo#           s              3  0
     ddecDiv           s              5  0
     ddecEmp#          s              9  0
     ddecFnd           s              3  0
     ddecFyr           s              3  0

     DjsonYear         s               a   len(150000) varying
     DjsonDetail       s               a   len(100000) varying

     ddecBalAsOf       s             15  2 inz
     ddecBalCurr       s             15  2 inz
     ddecBill          s             15  2 inz
     ddecBillAsOf      s             15  2 inz
     ddecBillCurr      s             15  2 inz
     ddecPaid          s             15  2 inz
     ddecPaidAsOf      s             15  2 inz
     ddecPaidCurr      s             15  2 inz
     ddt7AsOf          s              7  0 inz

     ddsWMEAP          ds                  qualified inz
     d EAco#                               like(EAco#)
     d EAfnd                               like(EAfnd)
     d EAemp#                              like(EAemp#)
     d EAdiv                               like(EAdiv)
     d EAfyr                               like(EAfyr)
     d EApoDt                              like(EApoDt)
     d EAtrDt                              like(EAtrDt)
     d EAarCd                              like(EAarCd)
     d EAarTy                              like(EAarTy)
     d EAdesc                              like(EAdesc)
     d EAamt                               like(EAamt)
     d EAchk#                              like(EAchk#)
     d type                          25    varying
     ddsWMEAPinz       ds                  likeds(dsWMEAP) inz
     ddsWMEAPsv        ds                  likeds(dsWMEAP) inz

     ddsIn             ds                  inz qualified
     d co#                            3
     d group                          3
     d employer                       9
     d division                       5
     d year                           3
      /Free
       Exsr ParmsGet;
       Exsr BuildOutput;
       If %len(json) > 0;
          json = %scanrpl(',':'':json:%len(json)-1);
       Endif;
       outJSON = json;
       *inlr = *on;

       //=================================================================
       // Build json results - Detail
       //=================================================================
       Begsr BuildJsonDetail;
        jsonDetail += '{' +
         '"dates":{' +
          BuildJsonNode('posted':%char(dsWMEAP.EApoDt):'date') + ',' +
          BuildJsonNode('transaction':%char(dsWMEAP.EAtrDt):'date') +
          '},' +
         '"transaction":{' +
          BuildJsonNode('code':dsWMEAP.EAarCd) + ',' +
          BuildJsonNode('type':dsWMEAP.EAarTy) + ',' +
          BuildJsonNode('description':dsWMEAP.EAdesc) +
          '},' +
         BuildJsonNode('amount'
          :%editc(ARValue(dsWMEAP.EAamt:dsWMEAP.EAarTy):'J')) +
          '},';
         Select;
         When dsWMEAP.EAarTy = 'P' or dsWMEAP.EAarTy = 'Q'
          or dsWMEAP.EAarTy = 'I' or dsWMEAP.EAarTy = 'T'
          or dsWMEAP.EAarTy = 'R';
            decPaid -= ARValue(dsWMEAP.EAamt:dsWMEAP.EAarTy);
         Other;
            decBill += ARValue(dsWMEAP.EAamt:dsWMEAP.EAarTy);
         Endsl;
       Endsr;

       //=================================================================
       // Build json results - Detail Summary
       //=================================================================
       Begsr BuildJsonDetailSummary;
        If jsonDetail = *blanks;
           Leavesr;
        Endif;
        jsonDetail = %scanrpl(',':'':jsonDetail:%len(jsonDetail)-1);
        json += ElementOpen('{') +
         '"dates":' + ElementOpen('{') +
          BuildJsonNode('posted':%char(dsWMEAPsv.EApoDt):'date') +
          ElementClose() + ',' +
         BuildJsonNode('type':dsWMEAPsv.type) + ',' +
         '"amount":' + ElementOpen('{') +
        //*** begin chg ***  02/25/22  *************************************
        //*****  BuildJsonNode('billed':%editc(decBill:'3')) + ',' +
        //*****  BuildJsonNode('paid':%editc(decPaid:'3')) +
          BuildJsonNode('billed':%editc(decBill:'J')) + ',' +
          BuildJsonNode('paid':%editc(decPaid:'J')) +
        //*** end chg *****  02/25/22  *************************************
          ElementClose() + ',' +
         BuildJsonNode('check':dsWMEAPsv.EAdesc) + ',' +
         '"detailList":' + ElementOpen('[') + jsonDetail + ElementClose()
          + ElementClose() + ',';
        Reset jsonDetail;
        Reset decBill;
        Reset decPaid;
       Endsr;

       //=================================================================
       // Build json results - Division
       //=================================================================
       Begsr BuildJsonDivision;
        json = %scanrpl(',':'':json:%len(json)-1);
        If dsWMEAPsv <> dsWMEAPinz;
           json += ElementClose() + ElementClose() + ElementClose() +
            ElementClose() + ',';
        Endif;
        json += ElementOpen('{') +
         BuildJsonNode('division':%char(dsWMEAP.EAdiv)) + ',' +
         '"years":' + ElementOpen('[');
        Reset dsWMEAPsv;
        Exsr BuildJsonYear;
       Endsr;

       //=================================================================
       // Build json results - Employer
       //=================================================================
       Begsr BuildJsonEmployer;
        json += BuildJsonNode('company':%char(dsWMEAP.EAco#)) + ', ' +
         BuildJsonNode('group':%char(dsWMEAP.EAfnd)) + ', ' +
         BuildJsonNode('employer':%char(dsWMEAP.EAemp#)) + ', ' +
         '"divisions":' + ElementOpen('[');
        Reset dsWMEAPsv;
        Exsr BuildJsonDivision;
       Endsr;

       //=================================================================
       // Build json results - Year
       //=================================================================
       Begsr BuildJsonYear;
        decBillCurr = ARBilled(0:sysDate7:dsWMEAP.EAco#:dsWMEAP.EAfnd
         :dsWMEAP.EAfyr:dsWMEAP.EAemp#:dsWMEAP.EAdiv);
        decPaidCurr = ARPaid(0:sysDate7:dsWMEAP.EAco#:dsWMEAP.EAfnd
         :dsWMEAP.EAfyr:dsWMEAP.EAemp#:dsWMEAP.EAdiv);
        decBalCurr = decBillCurr - decPaidCurr;
        decBillAsOf = ARBilled(0:dt7AsOf:dsWMEAP.EAco#:dsWMEAP.EAfnd
         :dsWMEAP.EAfyr:dsWMEAP.EAemp#:dsWMEAP.EAdiv);
        decPaidAsOf = ARPaid(0:dt7AsOf:dsWMEAP.EAco#:dsWMEAP.EAfnd
         :dsWMEAP.EAfyr:dsWMEAP.EAemp#:dsWMEAP.EAdiv);
        decBalAsOf = decBillAsOf - decPaidAsOf;
        json = %scanrpl(',':'':json:%len(json)-1);
        If dsWMEAPsv <> dsWMEAPinz;
           json += ElementClose() + ElementClose() + ',';
        Endif;
        json += ElementOpen('{') +
         BuildJsonNode('year':%char(dsWMEAP.EAfyr)) + ',' +
         PolicyJSONData(dsWMEAP.EAco#:dsWMEAP.EAfnd:dsWMEAP.EAemp#:
          dsWMEAP.EAdiv:dsWMEAP.EAfyr) + ',' +
         '"summary":' +
          '{"current":{' +
           BuildJsonNode('date':%char(sysDate7):'date') + ',' +
           BuildJsonNode('billed':%editc(decBillCurr:'J')) + ',' +
           BuildJsonNode('paid':%editc(decPaidCurr:'J')) + ',' +
           BuildJsonNode('balance':%editc(decBalCurr:'J')) + '},' +
          '"asOf":{' +
           BuildJsonNode('date':%char(dt7AsOf):'date') + ',' +
           BuildJsonNode('billed':%editc(decBillAsOf:'J')) + ',' +
           BuildJsonNode('paid':%editc(decPaidAsOf:'J')) + ',' +
           BuildJsonNode('balance':%editc(decBalAsOf:'J')) + '}},' +
         '"summaryList":' + ElementOpen('[');
        Exsr BuildJsonDetailSummary;
       Endsr;

       //=================================================================
       // Read through data based parameters
       //=================================================================
       Begsr BuildOutput;
        If ioRtCd = 'Error';
           Leavesr;
        Endif;
        //Exsr BuildStub;
        json += '"detail":' + ElementOpen('{');
        Exsr SqlBuild;
        Dou not IsValidFetch(sqlstt);
           Exec SQL fetch next from cursorBillDtl into :dsWMEAP;
           Select;
           When not IsValidFetch(sqlstt);
              Iter;
           When dsWMEAP.EAemp# <> dsWMEAPsv.EAemp#;
              Exsr BuildJsonEmployer;
           When dsWMEAP.EAdiv <> dsWMEAPsv.EAdiv;
              Exsr BuildJsonDivision;
           When dsWMEAP.EAfyr <> dsWMEAPsv.EAfyr;
              Exsr BuildJsonYear;
           When dsWMEAP.EApoDt <> dsWMEAPsv.EApoDt
            or dsWMEAP.type <> dsWMEAPsv.type;
              Exsr BuildJsonDetailSummary;
           Endsl;
           Exsr BuildJsonDetail;
           dsWMEAPsv = dsWMEAP;
        Enddo;
        Exsr BuildJsonDetailSummary;
        json = %scanrpl(',':'':json:%len(json)-1);
        json += ElementClose('*ALL');
       Endsr;

       //=================================================================
       // Build stub results - Leave this in the code for possible future requests
       //=================================================================
       Begsr BuildStub;
        json += BuildJsonNode('stub':%trim(inRequest) + ' (' +
         %trim(q1Pgm) + ') successfully executed. {ListBillingDetail}') + ',';
       Endsr;

       //=================================================================
       // Break parms out from inParms
       //=================================================================
       Begsr ParmsGet;
        inJson = inParms;
        node0 = yajl_string_load_tree(inJson : errMsg500);
        If errMsg500 <> ' ';
           AddErrorMessageJson(errMsg500:ioRtCd);
           Leavesr;
        Endif;

        json += ' "requestParms" : {';
        Reset i0;
        Dow YAJL_object_loop(node0:i0:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'company';
              dsIn.co# = YAJL_get_string(val);
           When key = 'group';
              dsIn.group = YAJL_get_string(val);
           When key = 'employer';
              dsIn.employer = YAJL_get_string(val);
           When key = 'division';
              dsIn.division = YAJL_get_string(val);
           When key = 'year';
              dsIn.year = YAJL_get_string(val);
           Endsl;
        Enddo;
        json += '},';

        Exsr ParmsValidate;
       Endsr;

       //=================================================================
       // Validate incoming parms
       //=================================================================
       Begsr ParmsValidate;
        If ioRtCd = 'Error';
           Leavesr;
        Endif;

        // Company # must be valid if not then default to user parms.
        Monitor;
           decCo# = %int(dsIn.co#);
        On-error;
        Endmon;
        If decCo# = *zero;
           decCo# = intCo#;
        Endif;

        // Group
        Monitor;
           decFnd = %int(dsIn.group);
        On-error;
        Endmon;
        Select;
        When dsIn.group = *blank;
        When isEnvQuote and not UseGroupInPortal(decCo#:decFnd:'Quote');
           AddErrorMessageJson('Group ' + %char(decFnd) +
            ' is not included for service requested ' +
            '(' + %trim(inRequest) + ').':ioRtCd);
        When isEnvLive and not UseGroupInPortal(decCo#:decFnd:'Policy');
           AddErrorMessageJson('Group ' + %char(decFnd) +
            ' is not included for service requested ' +
            '(' + %trim(inRequest) + ').':ioRtCd);
        Endsl;
        If ioRtCd = 'Error';
           Leavesr;
        Endif;
        dt7AsOf = sysDate7;
       Endsr;

       //=================================================================
       // Build sql based on request
       //=================================================================
       Begsr SqlBuild;
        strSql = 'select EAco#, EAfnd, EAemp#, EAdiv, EAfyr, EApoDt, EAtrDt,'+
         ' EAarCd, EAarTy, EAdesc, EAamt, EAchk#, ' +
         ' case EAarTy ' +
         '  when ''P'' then ''Payment''' +
         '  when ''I'' then ''Payment''' +
         '  when ''Q'' then ''Payment''' +
         '  when ''B'' then ''Billed''' +
         '  when ''F'' then ''Billed''' +
         '  when ''H'' then ''Billed''' +
         '  when ''A'' then ''Billed''' +
         '  when ''D'' then ''Billed''' +
         '  when ''R'' then ''Refund''' +
         '  when ''N'' then ''NSF Charge''' +
         '  when ''V'' then ''Void''' +
         '  when ''T'' then ''Return Premium Void''' +
         ' end as TYPE ' +
         'from WMEAP ' +
         ' where EAco# = ' + %char(decCo#) + ' and EAfnd = ' + dsIn.group +
         '  and EAemp# = ' + dsIn.employer;
        If dsIn.division <> *blank;
           strSql += ' and EAdiv = ' + dsIn.division;
        Endif;
        If dsIn.year <> *blank;
           strSql += ' and EAfyr = ' + dsIn.year;
        Endif;
        strSql += ' order by EAco#, EAfnd, EAemp#, EAdiv, EAfyr desc, ' +
         'EApoDt desc, TYPE, EAtrDt desc';
        Exsr SqlCursor;
       Endsr;

       //=================================================================
       // Build sql cursor
       //=================================================================
       Begsr SqlCursor;
        Exec sql close cursorBillDtl;
        Exec sql prepare sqlBillDtl from :strSql;
        Exec sql declare cursorBillDtl cursor for sqlBillDtl;
        Exec sql open cursorBillDtl;
       Endsr;
      /End-free
     PListBillingDetail...
     P                 e
