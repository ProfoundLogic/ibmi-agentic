     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR':'YAJL') DECEDIT('0.')
     H*====================================================================
     H* Create on 09/24/2018 by c:62 for Project SY406
     H*====================================================================
     H* Description: Retrieves Darts Information
     H*
     H*
     H*====================================================================
     H* inParms -
     H*   company
     H*   group
     H*   employer
     H*   year
     H*====================================================================
     H* Revisions:
     H*
     H* 10/24/24 C:291 Support: 117938
     H*                Clear LDA. Update the LDA with default iDarts and
     H*                Remark code. Set inHardCopy to 'Y' if Scriptura
     H*                enabled for Quote Bill.
     H*
     H* 08/03/23 C:340 Project: WC2670
     H*                Update the LDA usage for Scriptura
     H*====================================================================
     Fsmupl     if   e           k disk    rename(pup01:pup00)
     Fwdell1    if   e           k disk    rename(pel01:pel01)
     Fwtabl     if   e           k disk    rename(pbs01:pbs00)
     Fwmexl     if   e           k disk    rename(pex01:pex00)

     D PSDS          ESDS                  extname(SZQ1P)

     D SVLDA           DS          1024    DTAARA(*LDA)
     D***  Beg Chg  ***  08/03/23  WC2670  ****************
     d*xxPath***                    255    overlay(svlda:1)
     d xxDartsKey                    40    overlay(svlda:56)
     d xxDartsSeq#                    5    overlay(svlda:96)
      *** Beg Add ***** 10/24/24 ***************************************
     D xxDart                         1    overlay(svlda:251)
     d xxRMCD                         5    overlay(svlda:252)
     d xxPrint                        1    overlay(svlda:258)
     d xxPrinter                     10    overlay(svlda:259)
     d xxTray                        50    overlay(svlda:269)
     d xx#CopiesA                     3    overlay(svlda:319)
     d xxEmail                        1    overlay(svlda:322)
     d xxToEmail                    200    overlay(svlda:323)
     d xxSubject                    150    overlay(svlda:523)
     d xxBody                       350    overlay(svlda:673)
      *** End Add ***** 10/24/24 ***************************************

     d xxPath          S            256    inz
     D***  End Chg  ***  08/03/23  WC2670  ****************

     D* this size needs to change
     D array           s               a   len(32767)
     d inJson          s               a   len(2000000) varying
     D json            s               a   len(8500000) varying
     D jsont           s               a   len(8500000) varying
     D jsonSub         s               a   len(8500000) varying

     d group#          s              3  0
     d groupError      s               n   Inz(*Off)
     d strRmkDate      s              8
     d spoolFile       s             10
     d rmkDate7        s                   Like(UPdt)
     d rmkDate8        s              8  0
     d sequence#       s              5  0
     d dateError       s               n   Inz(*Off)
     d comma           s              1    Inz(' ')
     d i               s              5  0 Inz(0)
     d isEnvLive       s              1n   inz
     d isEnvQuote      s              1n   inz
     d quote           s              1
     d ##plan          s              1
     d ##anam          s             60
     d ##pnam          s             60
     d ##agt#          s              5  0 Inz(0)
     d ##prod          s              5  0 Inz(0)
     d ##area          s              3  0 Inz(0)
     d ##phon          s              7  0 Inz(0)
     d ##agdt          s              8  0 Inz(0)

     D errMsg          s          99999a   Varying Inz(' ')
     D errMsg500       s            500a   Varying Inz(' ')
     D sqlStmtStr      s            400a
     D moreRows        s               n   Inz(*On)
     D clientID        s              3    inz('000')
     D policyKeyBad    s               n   Inz(*Off)

     D docNode         s                   like(yajl_val)
     D list            s                   like(yajl_val)
     D node            s                   like(yajl_val)
     D val             s                   like(yajl_val)

     d inCompany       s              3
     d inGroup         s              3
     d inYear          s              3
     d inEmployer#     s              9
     d strAuthority    s             10a
     d strDivision     s              5
     d inPostDate      s              8
     d xxPostDate      s              8  0
     d intDays         s              3  0
     d inEstTrnCd      s              2    Inz(' ')
     d inComm1         s              2    Inz(' ')
     d inComm2         s              2    Inz(' ')
     d inComm3         s              2    Inz(' ')
     d inDueDate       s              8    Inz('00000000')
     d inPrtBal        s              1    Inz('Y')
     d inSlfEstFl      s              1    Inz('E')
     d inCall          s              1    Inz(' ')
     d inOverride      s              1    Inz(' ')
     d inSrCode        s              2    Inz(' ')
     d inSrAdjCode     s              2    Inz(' ')
     d inQuote         s              1    Inz(' ')
     d inOutQ          s             10    Inz('QPRINT')
     d inNewRenew      s              1    Inz(' ')
     d inDartsFlg      s              1    Inz('Y')
     d inHardCopy      s              1    Inz('N')
     d inAgentOrder    s              1    Inz('N')
     d inFinalQuote    s              1    Inz('N')
     d inRmkcode       s              5    Inz(' ')
     d inArCode        s              2    Inz(' ')
     d inEffDate       s              8    Inz('00000000')
     d inFreeComm1     s             30    Inz(' ')
     d inFreeComm2     s             30    Inz(' ')
     d inFreeComm3     s             30    Inz(' ')
     d inArCode2       s              2    Inz(' ')
     d outPath         s            256    Inz(' ')
      *** Beg Add ***** 10/24/24  ***************************
     d isUseScript     s              1n   inz
     d processType     c                   const('Quote Bill')
      *** End Add ***** 10/24/24  ***************************

      /copy sPrAtrium
      /copy sPrDate
      /copy sPrJSON
      /copy sPrSecure
      /copy sPrString
      /copy wPrGroup
      /copy wPrEmplr
      *** Beg Add ***** 10/24/24  ***************************
      /copy sPrXML
      *** End Add ***** 10/24/24  ***************************
      /include yajl_h

      // Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3

     d AgtInfo         pr                  extpgm('WA002R')
     d   ##CO#                        3  0
     d   ##FND                        3  0
     d   ##EMP#                       9  0
     d   ##DIV                        5  0
     d   ##FYR                        3  0
     d   ##AGT#                       5  0
     d   ##ANAM                      60
     d   ##PROD                       5  0
     d   ##PNAM                      60
     d   ##AREA                       3  0
     d   ##PHON                       7  0
     d   ##PLAN                       1
     d   ##DATE                       8  0

     * Spoolfile Cleanup
     d CallQteBillC    pr                  ExtPgm('WIQTEBILLC')
     d inspoolfile                   10

     * Billing Pgm
     d CallW8002C      pr                  ExtPgm('W8002C')
     d inCompany                      3
     d inGroup                        3
     d inYear                         3
     d inEmployer#                    9
     d inDivision                     5
     d inPostDate                     8
     d inEstTrnCd                     2
     d inComm1                        2
     d inComm2                        2
     d inComm3                        2
     d inDueDate                      8
     d inPrtBal                       1
     d inSlfEstFl                     1
     d inCall                         1
     d inOverride                     1
     d inSrCode                       2
     d inSrAdjCode                    2
     d inQuote                        1
     d inOutQ                        10
     d inNewRenew                     1
     d inDartsFlg                     1
     d inHardCopy                     1
     d inAgentOrder                   1
     d inFinalQuote                   1
     d inRmkcode                      5
     d inArCode                       2
     d inEffDate                      8
     d inFreeComm1                   30
     d inFreeComm2                   30
     d inFreeComm3                   30
     d inArCode2                      2

      *entry
     DRequestPgm       pr                  ExtPgm('WIQTEBILL')
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

       // Get the client ID
       GetClientID(clientID);

       Clear json;

       Select;
       When inRequest = 'PrintQuote';
          quote = 'Q';
          isEnvQuote = *on;
          ProcPrintQuote();
       When inRequest = 'PrintBill';
          quote = ' ';
          isEnvLive = *on;
          ProcPrintQuote();
       EndSl;

       outJSON = json;

       *inlr = *on;


      /End-free
       //===================================================================
       // Process Darts Listing by User
       //===================================================================
     pProcPrintQuote...
     p                 b
     dProcPrintQuote...
     d                 pi

     d result          ds                  Qualified
     D   company                      3
     D   group                        3
     D   employer                     9
     D   year                         3

     d company#        s                   Like(ELco#)
     d group#          s                   Like(ELfnd)
     d employer#       s                   Like(ELemp#)
     d division        s                   Like(ELdiv)
     d year            s                   Like(ELfYr)
     d tableKey        s                   Like(BSkey)

      /free
          Exsr PgmStart;
          Exsr GetParameters;
          ExSr BldReqInfo;
          If ioRtCd <> 'Error';
             Exsr PrintQuote;
             Exsr BldReturnInfo;
          EndIf;
          Exsr BldErrorsInfo;

       //=================================================================
       // Build Deductible Information                                      ques
       //=================================================================
       Begsr BldQuotePrintInfo;

        //***  Beg Add  ***  08/03/23  WC2670  *****************
          Reset xxPath;
          Exec sql
           select CMuncN into :xxPath
            from SDCMP
            where CMkey = :xxDartsKey and CMseq# = :xxDartsSeq#;
        //***  End Add  ***  08/03/23  WC2670  *****************

          If xxPath <> ' ';
             json = %trim(json) + ',"documentInfo":{ ' +
               BuildJsonNode('documentPath':%Trim(xxPath)) +
              ' }';
          Else;
             AddErrorMessageJson('Error Printing Bill':ioRtCd);
             json += ProcessErrorMessagesJson('*PRE');
             json = %scanrpl(',':'':json:1:1);
          Endif;

       EndSr;

       //=================================================================
       // Build Error Message Response                                      ques
       //=================================================================
       Begsr BldErrorsInfo;

          // Finish by generating the errors
          json += ProcessErrorMessagesJson('*PRE');
          json = %scanrpl(',':'':json:1:1);

       Endsr;

       //=================================================================
       // Build Request Information                                         ques
       //=================================================================
       Begsr BldReqInfo;

          json = '"requestParms" : {' +
                 BuildJsonNode('company':result.company) +
                 ',' +
                 BuildJsonNode('group':result.group) +
                 ',' +
                 BuildJsonNode('employer':result.employer) +
                 ',' +
                 BuildJsonNode('year':result.year) +
                 '}';

       Endsr;

       //=================================================================
       // Build Return Information                                          ques
       //=================================================================
       Begsr BldReturnInfo;

          // Build Darts Information
          ExSr BldQuotePrintInfo;

       Endsr;

       //=================================================================
       // Break parms out from inParms and validate
       //=================================================================
       Begsr GetParameters;

          // Load the json tree
          inJson = inParms;
          docNode = yajl_string_load_tree(inJson : errMsg500);
          If errMsg500 = ' ';

             // Load the data structure with the parameter values
             // from the tree
             node = YAJL_object_find(docNode: 'company');
             result.company  = YAJL_get_string(node);
             node = YAJL_object_find(docNode: 'group');
             result.group    = YAJL_get_string(node);
             node = YAJL_object_find(docNode: 'employer');
             result.employer = YAJL_get_string(node);
             node = YAJL_object_find(docNode: 'year');
             result.year     = YAJL_get_string(node);

             // Free up the space by removing the tree
             yajl_tree_free(docNode);

             //======= Validate the incoming parameters ====================

             // Company Must be passed
             If result.company = ' ';
                AddErrorMessageJson('Company # not passed in':ioRtCd);
                policyKeyBad = *On;
             Else;
                Monitor;
                   company# = %Dec(%Trim(result.company):3:0);
                On-Error;
                   AddErrorMessageJson('Company # has invalid characters'
                    :ioRtCd);
                   policyKeyBad = *On;
                EndMon;
             EndIf;

             // Group Must be passed
             If result.group = ' ';
                AddErrorMessageJson('Group # not passed in':ioRtCd);
                policyKeyBad = *On;
             Else;
                Monitor;
                   group# = %Dec(%Trim(result.group):3:0);
                   Select;
                   When isEnvQuote and
                    not UseGroupInPortal(company#:group#:'Quote');
                      AddErrorMessageJson('Group ' + %char(group#) +
                       ' is not included for service requested ' +
                       '(' + %trim(inRequest) + ').':ioRtCd);
                   When isEnvLive and
                    not UseGroupInPortal(company#:group#:'Policy');
                      AddErrorMessageJson('Group ' + %char(group#) +
                       ' is not included for service requested ' +
                       '(' + %trim(inRequest) + ').':ioRtCd);
                   Endsl;
                On-Error;
                   AddErrorMessageJson('Group # has invalid characters'
                    :ioRtCd);
                   policyKeyBad = *On;
                EndMon;
             EndIf;

             // Employer # Must be passed
             If result.employer = ' ';
                AddErrorMessageJson('Employer # not passed in':ioRtCd);
                policyKeyBad = *On;
             Else;
                Monitor;
                   employer# = %Dec(%Trim(result.employer):9:0);
                On-Error;
                   AddErrorMessageJson('Employer # has invalid characters'
                    :ioRtCd);
                   policyKeyBad = *On;
                EndMon;
             EndIf;

             // Year Must be passed
             If result.year = ' ';
                AddErrorMessageJson('Policy year not passed in':ioRtCd);
                policyKeyBad = *On;
             Else;
                Monitor;
                   year = %Dec(%Trim(result.year):3:0);
                On-Error;
                   AddErrorMessageJson('Policy year has invalid characters'
                    :ioRtCd);
                   policyKeyBad = *On;
                EndMon;
             EndIf;

             // Now let's validate the policy
             If Not policyKeyBad;
                Chain (company#:group#:year:employer#) pel01;
                If Not %Found;
                   AddErrorMessageJson('Invalid Policy':ioRtCd);
                EndIf;
             EndIf;

            //* Retrieve iDarts remark code, return error if none available.
            Clear inRmkCode;
            Chain(n) (ELCo#:ELFnd) pex00;
            If %found;
               If quote = 'Q';
                  inRmkCode = EXQte;
               Else;
                  inRmkCode = EXOrg;
                  If ELFAF = 'R' and EXRev <> ' ';
                     inRmkCode = EXRev;
                  Endif;
               Endif;
            Endif;

            If inRmkCode = ' ' and errMsg = ' ';
               AddErrorMessageJson('No iDarts remark code set up ' +
                'at Group to resolve bill.':ioRtCd);
            Endif;

          Else;
             AddErrorMessageJson(%Trim(errMsg500):ioRtCd);
          EndIf;

       Endsr;

       //--------------------------------------------------------
       Begsr PgmStart;
       //--------------------------------------------------------
        strAuthority = ioRtCd;
        Clear ioRtCd;
       Endsr;

       //=================================================================
       // Call the Print process.
       //=================================================================
       Begsr PrintQuote;

         division = GetStRptDv(ELCo#:ELFnd:ELEmp#);

         Chain (company#:group#:year:employer#:division) pel01;
         If not %found;
            Setll (company#:group#:year:employer#) pel01;
            Reade (company#:group#:year:employer#) pel01;
            If not %eof;
               division = ELDiv;
            Endif;
         Endif;

         strDivision = %editc(division:'X');

         //* Check account current status to determine posting date
         ##agdt = dtMDYY(ELptdt);
         ##plan = '*';

         AGTINFO(ELCO#:ELFND:ELEMP#:ELDIV:ELFYR:##AGT#
          :##ANAM:##PROD:##PNAM:##area:##phon:##plan:##agdt);

         If ##plan = 'A';
            intDays = EXaoid;
         Else;
            intDays = EXrvid;
         EndIf;

         // Posting Date (Interim Days Plus Current Date)
         xxPostDate = dtfuture(dateto8(systemDate()):intDays);
         If xxPostDate = *Zeros;
            xxPostDate = dateto8(systemDate());
         EndIf;

         //* Use billing start date if posting date is less than it.
         If dtCYMD(xxPostDate) < ELbsdt;
            inPostDate = %EditC(dtMDYY(ELbsDt):'X');
         Else;
            inPostDate = %EditC(xxPostDate:'X');
         Endif;

         tableKey = %EditC(ELbsCd:'X');
         Chain (ELco#:tableKey) pbs00;
         If %Found(wtabl);
            inSlfEstFl = BSseFl;
         Else;
            inSlfEstFl = 'E';
         EndIf;

         inQuote = quote;
         //***** Beg Add ***** 10/24/24 *******************************
         isUseScript = UseScriptura(processType);
         If isUseScript;
            inHardCopy = 'Y';
         Endif;
         //***** End Add ***** 10/24/24 *******************************

         //* Clear document path from local data area.
         In *Lock *dtaara;
         //***  Beg Chg  ***  08/03/23  WC2670  *************
         //xxPath = ' ';
         Clear xxDartsKey;
         Clear xxDartsSeq#;
         //***  End Chg  ***  08/03/23  WC2670  *************
         //***** Beg Add ***** 10/24/24 *******************************
         Clear svLDA;
         xxRMCD = inRmkCode;
         If xxRMCD <> ' ';
            xxDart = 'Y';
         Endif;
         //***** End Add ***** 10/24/24 *******************************
         Out *dtaara;

         spoolFile = 'B' + %editc(ELEmp#:'X');

         //* Cleanup potential spoolfile.
         CallQteBillC(spoolFile);

         CallW8002C(result.company:result.group:result.year:
            result.employer:strDivision:inPostDate:inEstTrnCd:inComm1:
            inComm2:inComm3:inDueDate:inPrtBal:inSlfEstFl:inCall:inOverride:
            inSrCode:inSrAdjCode:inQuote:inOutQ:inNewRenew:inDartsFlg:
            inHardCopy:inAgentOrder:inFinalQuote:inRmkCode:inArCode:
            inEffDate:inFreeComm1:inFreeComm2:inFreeComm3:inArCode2);

         //* Pull in document from local data area.
         In *Lock *dtaara;
         Out *dtaara;

         //* Cleanup spoolfile.
         CallQteBillC(spoolFile);

       Endsr;
      /End-free

     pProcPrintQuote...
     p                 e
