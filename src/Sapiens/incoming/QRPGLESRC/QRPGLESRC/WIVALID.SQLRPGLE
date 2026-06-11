     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     h DFTACTGRP(*no) ACTGRP('CONNECT')
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR':'YAJL')
     H*====================================================================
     H* Description:  Web Service to validate a quote for template and question
     H*               compliance.
     H*
     H* Project: SY406   Date: 11/20/18
     H* Pgmr:    C:65
     H*
     H* Modes:        ' ' - Perform validation and return any error conditions
     H*                     triggered in json.
     H*               'C' - Checks template to be used by quote and returns the
     H*                     template name in json.
     H*
     H*====================================================================
     H* inParms -
     H*   inMode: incoming mode
     H*   inCo# : incoming company
     H*   inFnd : incoming fund
     H*   inFyr : incoming year
     H*   inEmp#: incoming employer #
     H*
     H*====================================================================
     H* outParms -
     H*   outStatus: outgoing status
     H*   outJSON  : outgoing json/data
     H*
     H*====================================================================
     H* Revisions:
     H*
     H* 01/13/222 C:33  Support: 100385
     H*                 Account for all states for a class code to be included
     H*
     H* 10/08/21  C:33  Support: 98625
     H*                 Account for analytic scoring with template comparison
     H*====================================================================
     Fwdell1    if   e           k disk    rename(pel01:pel01) infds(elfds)
     Fwmepl7    if   e           k disk    rename(pep01:pep07)
     Fwmagp     if   e             disk    rename(pag01:pag0p)
     Fwmrsl     if   e           k disk    rename(prs01:prs00)
     Fwmecl     if   e           k disk    rename(pec01:pec00)
     Fwieml1    if   e           k disk    rename(pem01:pem01)
     Fwiapl     if   e           k disk    rename(pap01:pap00)
     Fwiapplosl if   e           k disk    rename(pap01:pal00) prefix(al:2)
     Fwiappansl if   e           k disk    rename(pap01:pan00) prefix(an:2)
     Fwital     if   e           k disk    rename(pta01:pta00)
     Fwital1    if   e           k disk    rename(pta01:pta01)
     Fwitml     if   e           k disk    rename(ptm01:ptm00)
     Fwitcl     if   e           k disk    rename(ptc01:ptc00)
     Fwitmpqstl if   e           k disk    rename(witmpqst:qst00) prefix(tq:2)

     D* this size needs to change
     Darray            s               a   len(32767)
     DinJson           s               a   len(2000000) varying
     Djson             s               a   len(8500000) varying
     dstatus           s              1a
     dcomma            s              1a
     dcomma1           s              1a
     derror            s              1a
     D errMsg          s          99999a   Varying Inz
     D errMsg500       s            500a   Varying Inz
     D errMsgQst       s          99999a   Varying Inz
     D errMsgSave      s          99999a   Varying Inz

     ddsPayroll        ds                  qualified
     d state                          2
     d class                          5
     d stClass                        7    overlay(dsPayroll)
     D arrPayroll      ds                  dim(9999) likeds(dsPayroll)
     d decLen          s             10i 0 inz
     d payroll         ds                            likeds(dsPayroll)
     d classSt         s              2
     d class           s              5s 0
     d index           s              4s 0

     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRST-
     D                                     UVWXYZ')
     D LO              C                   CONST('abcdefghijklmnopqrst-
     D                                     uvwxyz')

     dkeyCo#           s              3s 0
     dkeyFnd           s              3s 0
     dkeyEmp#          s              9s 0
     dkeyFyr           s              3s 0

     dstrQuestion      s            500a   varying
     dvalidTemplate    s              1n
     dxxExpMod         s              5s 4
     dxxLoss           s             15s 2
     dxxLossPrem       s             15s 2
     dxxLossRatio      s              9s 4
     dxxPremium        s             15s 2
     dxxType           s             10a
     dxxUserType       s                   like(TAuserType) inz

     drateSt           s              2
     dagtName          s             40
     dAH_RRN           s              9  0
     dAA_RRN           s              9  0
     dAG_RRN           s              9  0

     D PSDS          ESDS                  extname(SZQ1P)
     D elfds           ds
     D elrrn                 397    400b 0

      /copy sPrAtrium
      /copy sPrDate
      /copy sPrJSON
      /copy sPrSecure
      /copy sPrString
      /copy wPrAgent
      /copy wPrGroup
      /copy wPrRateSet
      /copy wPrRating
      /include yajl_h

      *entry
     DRequestPgm       pr                  ExtPgm('WIVALID')
     D                                1a
     D                                3s 0
     D                                3s 0
     D                                3s 0
     D                                9s 0
     D                               25a
     D                                1a
     D                                 a   len(8500000) varying
     DRequestPgm       pi
     D inMode                         1a
     D inCo#                          3s 0
     D inFnd                          3s 0
     D inFyr                          3s 0
     D inEmp#                         9s 0
     D inUserType                    25a
     D outStatus                      1a
     D outJSON                         a   len(8500000) varying


      /Free
       Exec Sql Set Option Commit = *None;
       Clear status;
       Clear json;
       Exsr PgmStart;
       Exsr GetParms;
       Select;
       When inMode = 'C';
          Exsr ReturnSetup;
       Other;
          Exsr BuildOutput;

          json = %trim(json) + ' { ';

          BuildGiantJSON(json:'template':%trim(TAname));

          Select;
          When errMsgSave <> ' ' and errMsg = ' ' and xxType = 'Pricing';
             BuildGiantJSON(json :'status' : 'success');
             BuildGiantJSON(json :'type' : %trim(xxType));
             json = %trim(json) + ', "errorConditions": [ ' +
              %trim(errMsgSave) + ' ] ';
             status = 'E';
          When errMsg <> ' ';
             BuildGiantJSON(json :'status' : 'fail');
             BuildGiantJSON(json :'type' : %trim(xxType));
             json = %trim(json) + ', "errorConditions": [ ' + %trim(errMsg) +
              ' ] ';
             status = 'E';
          Other;
             BuildGiantJSON(json :'status' : 'success');
             If validTemplate = *off;
                BuildGiantJSON(json :'type' : ' ');
             Else;
                BuildGiantJSON(json :'type' : %trim(xxType));
             Endif;
             status = ' ';
          Endsl;

          json = %trim(json) + ' } ';
       Endsl;

       outStatus = status;
       outJSON = json;
       //*inlr = *on;
       Return;

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
       // Read through data based parameters
       //=================================================================
       Begsr BuildOutput;

        //* Do template validation stuff here.
        errMsgSave = ' ';
        errMsg = ' ';
        errMsgQst = ' ';
        comma = ' ';
        comma1 = ' ';

        xxType = 'Quoting';
        Exsr CheckSetup;

        If errMsg <> ' ';
           Leavesr;
        Endif;

        //* Perform validation on quoting template.
        Exsr Validation;

        If errMsg <> ' ' or validTemplate = *off;
           errMsgSave = errMsg;
           errMsg = ' ';
           errMsgQst = ' ';
           comma = ' ';
           comma1 = ' ';

           xxType = 'Pricing';
           Exsr CheckSetup;

           If validTemplate = *off and errMsgSave <> ' ';
              errMsg = errMsgSave;
              xxType = 'Quoting';
           Endif;

           Exsr Validation;

        Endif;

       Endsr;

       //=================================================================
       // Build record results
       //=================================================================
       Begsr BuildRecord;

        If comma <> ' ';
           errMsg = %trim(errMsg) + comma;
        Else;
           comma = ',';
        Endif;

        errMsg = %trim(errMsg) + %trim(errMsg500);

       Endsr;

       //=================================================================
       // Determine Template Setup.
       //=================================================================
       Begsr CheckSetup;

        validTemplate = *off;

        Setll (keyCo#:keyFnd:keyFyr:keyEmp#) pel01;
        Reade(n) (keyCo#:keyFnd:keyFyr:keyEmp#) pel01;

        If %eof;
           If errMsg = *blank;
              errMsg = '"No Employer Detail."';
              AddErrorMessageJson(errMsg);
           Endif;
           Leavesr;
        Endif;

        Chain(n) (keyCO#:ELRTCD) prs00;
        If %found;
           rateSt = RSJRST;
        Else;
           Clear rateSt;
        Endif;

        Clear agtName;
        Clear AH_RRN;
        Clear AA_RRN;
        Clear AG_RRN;

        GetEmpAgt(keyCo#:keyFnd:keyEmp#:ELDiv:keyFyr:ELPFDT:agtName:
         AH_RRN:AA_RRN:AG_RRN);

        Chain(n) AG_RRN pag0p;

        Select;
        When inUserType = 'Agent';
           xxUserType = 'Agency';
        When inUserType = 'CSR';
           xxUserType = 'Agency';
        When inUserType = 'Company User';
           xxUserType = 'Company';
        Other;
           xxUserType = ' ';
        Endsl;

        //* Read the setup file.
        Exsr ReadSetup;

        //* If no valid template found yet, then reset and check again with blank/All.
        If validTemplate = *off and xxUserType <> ' ';
           xxUserType = ' ';
           Exsr ReadSetup;
        Endif;

       Endsr;

       //=================================================================
       // Break parms out from inParms
       //=================================================================
       Begsr GetParms;

        keyCo#  = inCo#;
        keyFnd  = inFnd;
        keyFyr  = inFyr;
        keyEmp# = inEmp#;

       Endsr;

       //=================================================================
       // Program entry
       //=================================================================
       Begsr PgmStart;
        // Convert depricated rate set code to rating state
        Exec sql
         update WITCP
          set TCRATEST = case
               when TCRATECD <> '' and TCRATEST = '' then
                (select RSJRST
                  from WMRSP
                  where RSCO#=TCCO# and TCRATECD=RSRTCD)
               else TCRATEST
               end;
       Endsr;

       //=================================================================
       // Read Template Setup.
       //=================================================================
       Begsr ReadSetup;

        //* Find template setup for agent levels.
        If %found(wmagp);
           Setgt (keyCo#:xxUserType:xxType:AGFein:AGMod:keyFnd:ELPFDT) pta01;
           Readpe(n) (keyCo#:xxUserType:xxType:AGFein:AGMod:keyFnd) pta01;
           Dow not %eof and TAStat <> 'A';
              Readpe(n) (keyCo#:xxUserType:xxType:AGFein:AGMod:keyFnd) pta01;
           Enddo;

           If %eof or (TAEXDT <= ELPFDT and TAEXDT <> 0);
              Setgt (keyCo#:xxUserType:xxType:AGFein:AGMod:0:ELPFDT) pta01;
              Readpe(n) (keyCo#:xxUserType:xxType:AGFein:AGMod:0) pta01;
              Dow not %eof and TAStat <> 'A';
                 Readpe(n) (keyCo#:xxUserType:xxType:AGFein:AGMod:0) pta01;
              Enddo;

              If %eof or (TAEXDT <= ELPFDT and TAEXDT <> 0);
              //* no template found.
              Else;
                 validTemplate = *on;
              Endif;
           Else;
              validTemplate = *on;
           Endif;
        Endif;

        //* Search for template at co/group/state levels.
        If validTemplate = *off;
           Setgt (keyCo#:xxUserType:xxType:keyFnd:rateSt:ELPFDT) pta00;
           Readpe(n) (keyCo#:xxUserType:xxType:keyFnd:rateSt) pta00;
           Dow not %eof and TAStat <> 'A';
              Readpe(n) (keyCo#:xxUserType:xxType:keyFnd:rateSt) pta00;
           Enddo;

           If %eof or (TAEXDT <= ELPFDT and TAEXDT <> 0);
              Setgt (keyCo#:xxUserType:xxType:keyFnd:'  ':ELPFDT) pta00;
              Readpe(n) (keyCo#:xxUserType:xxType:keyFnd:'  ') pta00;
              Dow not %eof and TAStat <> 'A';
                 Readpe(n) (keyCo#:xxUserType:xxType:keyFnd:'  ') pta00;
              Enddo;

              If %eof or (TAEXDT <= ELPFDT and TAEXDT <> 0);
                 Setgt (keyCo#:xxUserType:xxType:0:'  ':ELPFDT) pta00;
                 Readpe(n) (keyCo#:xxUserType:xxType:0:'  ') pta00;
                 Dow not %eof and TAStat <> 'A';
                    Readpe(n) (keyCo#:xxUserType:xxType:0:'  ') pta00;
                 Enddo;

                 If %eof or (TAEXDT <= ELPFDT and TAEXDT <> 0);
                 //* no template found.
                 Else;
                    validTemplate = *on;
                 Endif;
              Else;
                 validTemplate = *on;
              Endif;
           Else;
              validTemplate = *on;
           Endif;
        Endif;

       Endsr;

       //=================================================================
       // Find Template Setup to be used and return.
       //=================================================================
       Begsr ReturnSetup;

        json = '{"templateList":[';

        xxType = 'Quoting';
        Exsr CheckSetup;

        If validTemplate = *on;
         json += '{' + BuildJsonNode('template':%trim(TAname)) + ',' +
          BuildJsonNode('type':%trim(xxType)) + '}';
        Endif;

        xxType = 'Pricing';
        Exsr CheckSetup;

        If validTemplate = *on;
           If %subst(json:%len(json):1) = '}';
              json += ',';
           Endif;
           json += '{' + BuildJsonNode('template':%trim(TAname)) + ',' +
            BuildJsonNode('type':%trim(xxType)) + '}';
        Endif;

        json += ']}';

       Endsr;

       //=================================================================
       // Perform error checks based on template setup.
       //=================================================================
       Begsr Validation;

        // Get application number
        Clear EMapp#;
        Chain(n) (keyCo#:keyFnd:keyEmp#:keyFyr) pem01;

        //* Validate the quote with template settings.

        If validTemplate = *on;

           Chain(n) (keyCo#:TAName) ptm00;
           If %found;

              Clear xxPremium;
              Clear xxLoss;
              Clear xxExpMod;
              index = 1;
              Clear arrPayroll;

              //* Accumulate premium and other figures for quote.
              Setll (keyCo#:keyFnd:keyFyr:keyEmp#) pel01;
              Reade(n) (keyCo#:keyFnd:keyFyr:keyEmp#) pel01;

              Dow not %eof;
                 xxPremium = xxPremium + ELANPR;
                 If ELEXM1 > xxExpMod;
                    xxExpMod = ELEXM1;
                 Endif;

                 Setll (keyCo#:keyFnd:keyFyr:keyEmp#:ELDiv) pep07;
                 Reade(n) (keyCo#:keyFnd:keyFyr:keyEmp#:ELDiv) pep07;
                 Dow not %eof;
                    If index = 9999;
                       Leave;
                    Endif;
                    payroll.state = RatingSt(ELco#:ELrtCd);
                    payroll.class = %editc(EPCLAS:'X');
                    If %lookup(payroll.stClass:arrPayroll(*).stClass) = 0;
                       arrPayroll(index) = payroll;
                       index = index + 1;
                    Endif;
                    payroll.state = '**';
                    If %lookup(payroll.stClass:arrPayroll(*).stClass) = 0;
                       arrPayroll(index) = payroll;
                       index += 1;
                    Endif;
                    Reade(n) (keyCo#:keyFnd:keyFyr:keyEmp#:ELDiv) pep07;
                 Enddo;

                 Reade(n) (keyCo#:keyFnd:keyFyr:keyEmp#) pel01;
              Enddo;

           //* Verify Payroll Classes if setup is present.
              Chain(n) (keyCo#:TAName) ptc00;

              //* Process class exclusions.
              If %found and TMIECLCD = 'E';

                 Setll (keyCo#:TAName) ptc00;
                 Reade(n) (keyCo#:TAName) ptc00;
                 Dow not %eof;
                    payroll.state = TCrateSt;
                    payroll.class = %editc(TCclas:'X');

                 //* Class code is in error if its excluded and on quote
                    If (%lookup(payroll.stClass:arrPayroll(*).stClass) <> 0);
                       errMsg500 = '"Payroll class ' + %trim(%editc(TCClas:
                        '3')) + ' is not allowed, it is excluded from ' +
                        'quotes."';
                       Exsr BuildRecord;
                       LogRatingInfo(keyCo#:EMapp#:keyFnd:keyEmp#:keyFyr:
                        'Invalid':errMsg500);
                    Endif;

                    Reade(n) (keyCo#:TAName) ptc00;
                 Enddo;

              Endif;

              //* Process class inclusions.
              If %found and TMIECLCD = 'I';

                 index = 1;
                 Dow index <= 9999 and arrPayroll(index) <> ' ';

                  //***  Begin Chg  ***  01/13/22  ***********************
                  //classSt =  arrPayroll(index).state;
                  // Check for all states first
                    classSt = '**';
                  //***  End   Chg  ***  01/13/22  ***********************
                    class  =  %int(arrPayroll(index).class);
                    Chain(n) (keyCo#:TAName:classSt:class) ptc00;
                  //***  Begin Add  ***  01/13/22  ***********************
                    If not %found;
                       // Check for specific state
                       classSt =  arrPayroll(index).state;
                       Chain(n) (keyCo#:TAName:classSt:class) ptc00;
                    Endif;
                  //***  End   Add  ***  01/13/22  ***********************

                 //* Class code is in error if it has not been included for use.

                    If not %found;
                       errMsg500 = '"Payroll class ' + %trim(%editc(class:
                        '3')) + ' is not allowed, it is not included for ' +
                        'quotes."';
                       Exsr BuildRecord;
                       LogRatingInfo(keyCo#:EMapp#:keyFnd:keyEmp#:keyFyr:
                        'Invalid':errMsg500);
                    Endif;

                    index = index + 1;
                 Enddo;

              Endif;

           //* Maxiumum Exp Mod
              If xxExpMod > TMMaxMod and TMMaxMod <> 0;
                 errMsg500 = '"Experience Modifier exceeds maximum ' +
                  'allowed of ' + %trim(%editc(TMMaxMod:'3')) + '."';
                 Exsr BuildRecord;
                 LogRatingInfo(keyCo#:EMapp#:keyFnd:keyEmp#:keyFyr:
                  'Underwriting':errMsg500);
              Endif;

            //***  Begin Add  ***  10/08/21  *************
            // Analytic Scoring
              If ELscore < TMminScore or
               (ELscore > TMmaxScore and TMmaxScore <> *zero);
                 errMsg500 = '"Analytic Score (' + %editc(ELscore:'3') + ') ' +
                  'outside of allowable range (' +
                  %editc(TMminScore:'3') + ' - ' + %editc(TMmaxScore:'3')+')"';
                 Exsr BuildRecord;
                 LogRatingInfo(keyCo#:EMapp#:keyFnd:keyEmp#:keyFyr:
                  'Underwriting':errMsg500);
              Endif;
            //***  End   Add  ***  10/08/21  *************

           //* Minimum Years in Business
              If %found;
                 Chain(n) (keyCo#:EMApp#) pap00;
                 If %found and APYIB < TMMinYrs;
                    errMsg500 = '"Application does not meet minimum num' +
                     'ber of years in business of ' + %trim(%editc(TMMinYrs:
                     'Z')) + '."';
                    Exsr BuildRecord;
                    LogRatingInfo(keyCo#:EMapp#:keyFnd:keyEmp#:keyFyr:
                     'Underwriting':errMsg500);
                 Endif;
              Endif;

           //* Maximum Current Loss Ratio
              Setll (keyCo#:EMApp#) pal00;
              Reade(n) (keyCo#:EMApp#) pal00;
              If not %eof;
                 xxLossRatio = 0;
                 If ALAnnPrem <> 0;
                    xxLossRatio = (ALPaid + ALReserve) / ALAnnPrem;
                 Endif;

              //* Loss Ratio is available and greater than max.
                 If xxLossRatio <> 0 and xxLossRatio > TMMaxCLR;
                    errMsg500 = '"Current loss ratio of ' +
                     %editc(xxLossRatio:'3') + ' exceeds allowed maximum ' +
                     'of ' + %trim(%editc(TMMaxCLR:'3')) + '."';
                    Exsr BuildRecord;
                    LogRatingInfo(keyCo#:EMapp#:keyFnd:keyEmp#:keyFyr:
                     'Underwriting':errMsg500);
                 Endif;

              Endif;

           //* Maximum Loss Ratio
              xxLoss = 0;
              xxLossPrem = 0;
              xxLossRatio = 0;
              Setll (keyCo#:EMApp#) pal00;
              Reade(n) (keyCo#:EMApp#) pal00;
              Dow not %eof;
                 xxLoss = xxLoss + ALPaid + ALReserve;
                 xxLossPrem = xxLossPrem + ALAnnPrem;
                 Reade(n) (keyCo#:EMApp#) pal00;
              Enddo;
              If xxLossPrem <> 0;
                 xxLossRatio = xxLoss / xxLossPrem;
              Endif;

              //* Loss Ratio is available and greater than max.
              If xxLossRatio <> 0 and xxLossRatio > TMMaxLRA;
                 errMsg500 = '"Overall loss ratio of ' +
                  %editc(xxLossRatio:'3') + ' exceeds allowed maximum ' +
                  'of ' + %trim(%editc(TMMaxLRA:'3')) + '."';
                 Exsr BuildRecord;
                 LogRatingInfo(keyCo#:EMapp#:keyFnd:keyEmp#:keyFyr:
                  'Underwriting':errMsg500);
              Endif;

           //* Require Loss History
              Chain(n) (keyCo#:EMApp#) pal00;
              If not %found and TMReqLHS = 'Y';
                 errMsg500 = '"Loss History information is required ' +
                  'and has not been entered."';
                 Exsr BuildRecord;
                 LogRatingInfo(keyCo#:EMapp#:keyFnd:keyEmp#:keyFyr:
                  'Invalid':errMsg500);
              Endif;

           //* Require Contact Information
              Chain(n) (keyCo#:keyFnd:keyEmp#) pec00;
              If not %found and TMReqCon = 'Y';
                 errMsg500 = '"Contact information is required ' +
                  'and has not been entered."';
                 Exsr BuildRecord;
                 LogRatingInfo(keyCo#:EMapp#:keyFnd:keyEmp#:keyFyr:
                  'Invalid':errMsg500);
              Endif;

           //* Process required questions for template.
              Setll (keyCo#:TAName) qst00;
              Reade(n) (keyCo#:TAName) qst00;
              Dow not %eof;
                 Chain(n) (keyCo#:EMApp#:keyFnd:keyEmp#:keyFyr:
                  TQQue00001) pan00;
                 Reset errMsg500;
                 If %found;

                    error = ' ';

                    Monitor;
                       Select;
                       When TQOperator = 'Exists';
                          If ANResp = ' ';
                             error = 'Y';
                          Endif;
                       When TQOperator = 'Equals';
                          If ANResp <> TQOperand;
                             error = 'Y';
                          Endif;
                       When TQOperator = 'Not Equals';
                          If ANResp = TQOperand;
                             error = 'Y';
                          Endif;
                       When TQOperator = 'Less Than';
                          If %int(ANResp) >= %int(TQOperand);
                             error = 'Y';
                          Endif;
                       When TQOperator = 'Less Than or Equals';
                          If %int(ANResp) > %int(TQOperand);
                             error = 'Y';
                          Endif;
                       When TQOperator = 'Greater Than';
                          If %int(ANResp) <= %int(TQOperand);
                             error = 'Y';
                          Endif;
                       When TQOperator = 'Greater Than or Equals';
                          If %int(ANResp) < %int(TQOperand);
                             error = 'Y';
                          Endif;
                       Endsl;
                    On-error;
                          error = 'Y';
                    Endmon;

                    If error = 'Y';
                       errMsg500 = '"Question ' + %editc(ANID#:'Z') +
                        ' has an invalid answer.  The answer must be ' +
                        %trim(TQOperator) + ' ' + %trim(TQOperand) + '."';
                    Endif;
                 Else;
                    If IsClassCodeMatch();
                       errMsg500 = '"Question '+%trim(%editc(TQQue00001:'Z')) +
                        ' has not been answered.  This question is required."';
                    Endif;
                 Endif;
                 If errMsg500 <> *blanks;
                    Reset strQuestion;
                    Exec sql
                     select QUquestion into :strQuestion
                      from WIQUP
                      where QUco# = :keyCo#
                       and QUid# = :TQQue00001
                      limit 1;
                    If strQuestion <> *blank;
                       errMsg500 =
                        %scanrpl('"':' (' + %trim(RmvExceptJson(strQuestion)) +
                         ')"':errMsg500:%len(errMsg500)-1);
                    Endif;
                    LogRatingInfo(keyCo#:EMapp#:keyFnd:keyEmp#:keyFyr:
                     'Underwriting':errMsg500);
                    errMsgQst += errMsg500 + ',';
                 Endif;

                 Reade(n) (keyCo#:TAName) qst00;
              Enddo;

           //* Add question errors to error string.
              If errMsgQst <> ' ';
                 errMsgQst = %scanrpl(',':'':errMsgQst:%len(errMsgQst)-1);
                 If comma <> ' ';
                    errMsg = %trim(errMsg) + comma;
                 Else;
                    comma = ',';
                 Endif;
                 errMsg = %trim(errMsg) + %trim(errMsgQst);
              Endif;

           //* Minimum Premium
              If xxPremium < TMMinPrm;
                 errMsg500 = '"Quote premium of ' +
                  %trim(%editc(xxPremium:'3')) +
                  ' does not exceed minimum required of ' +
                  %trim(%editc(TMMinPrm:'3')) + '."';
                 Exsr BuildRecord;
                 LogRatingInfo(keyCo#:EMapp#:keyFnd:keyEmp#:keyFyr:
                  TAtype:errMsg500);
              Endif;

           //* Maximum Premium
              If xxPremium > TMMaxPrm;
                 errMsg500 = '"Quote premium of ' +
                  %trim(%editc(xxPremium:'3')) +
                  ' exceeds the maximum allowed of ' +
                  %trim(%editc(TMMaxPrm:'3')) + '."';
                 Exsr BuildRecord;
                 LogRatingInfo(keyCo#:EMapp#:keyFnd:keyEmp#:keyFyr:
                  TAtype:errMsg500);
              Endif;

           Endif;

        Endif;

       Endsr;
      /End-free
     P*======================================================
     P* Check employer for matching class code with question
     P*======================================================
     PIsClassCodeMatch...
     P                 b
     DIsClassCodeMatch...
     D                 pi             1n

     ddecExpEmp        s             10i 0 inz
     ddecExpMatch      s             10i 0 inz
     ddecExpQuest      s             10i 0 inz
      /Free
       Exec sql
        with
        empPayrolls as
        (select ELco#, ELfnd, ELemp#, ELdiv, ELfyr,
           ELRTCD || EPCLAS as empExposure
          from WDELP
          left join WMEPP
           on ELco#=EPco# and ELfnd=EPfnd and ELfyr=EPfyr
           and ELemp#=EPemp# and ELdiv=EPdiv
          where ELco#=:keyCo# and ELfnd=:keyFnd
           and ELemp#=:keyEmp# and ELfyr=:keyFyr),
        questionInfo as
        (select QUco#, QUid#,
           QUrateSet || QUexpCode as questExposure
          from WIQUPEXP
          where QUco#=:ANco# and QUid#=:ANid#)
        select
         (select count(*)
           from questionInfo q1
           join empPayrolls
            on q1.QUco#=ELco# and q1.questExposure=empExposure
           where q1.questExposure is not null) as expMatch,
         (select count(*) from empPayrolls) as expEmp,
         count(*) as expQuest
         into :decExpMatch, :decExpEmp, :decExpQuest
         from questionInfo;
        Return decExpQuest > 0 and decExpMatch > 0;
      /End-free
     PIsClassCodeMatch...
     P                 e

      /End-free
