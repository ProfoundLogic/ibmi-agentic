     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     h DFTACTGRP(*no) ACTGRP('CONNECT')
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR':'YAJL')
     H*====================================================================
     H* Description: Procedures for processing various reporting information
     H*====================================================================
     H* Revisions:
     H*
     H* 04/03/25 C:342 Ticket: 118517(140)
     H*                Change SQL to have correct result for Loss Summary Report
     H*
     H* 06/13/24 C:079 Ticket: 115739(140)
     H*                Change BookOfBusiness to handle negative premium
     H*
     H* 06/10/22 C:062 Ticket: 102585(140)
     H*                Expand field size to prevent overflow issue
     H*
     H* 12/23/21 C:062 Ticket: 100157(129)
     H*                Correct Cx Counts.
     H*
     H* 12/08/21 C:136 Project: CN028
     H*                Add AccidentYearLossRecapReport procedure
     H*
     H* 11/03/21 C:291 Project: C150WFK
     H*                Add procedure LossSummaryReport
     H*
     H* 11/18/20 C:180 Project WC2757
     H*                Recompiled only.
     H*
     H*====================================================================

     D* this size needs to change
     Darray            s               a   len(32767)
     Djson             s               a   len(8500000) varying
     DjsonSub          s               a   len(8500000) varying
     dclientID         s              3a
     disEnvQuote       s              1n
     disEnvLive        s              1n
     dintCo#           s              3  0
     dstrCo#           s              3a
     dstrEnv           s              4a
     dstrSql           s           5000    varying inz

     d i0              s             10i 0 inz
     d i1              s             10i 0 inz
     d i2              s             10i 0 inz
     d i3              s             10i 0 inz
     d i4              s             10i 0 inz
     d inJson          s               a   Len(2000000) varying
     d integer         s             15  0
     d strAuthority    s             10a
     d strErrorMsg     s            500a   varying
     d key             s             50a   varying
     d list            s                   Like(yajl_val)
     d node0           s                   Like(yajl_val)
     d node1           s                   Like(yajl_val)
     d node2           s                   Like(yajl_val)
     d node3           s                   Like(yajl_val)
     d node4           s                   Like(yajl_val)
     d val             s                   Like(yajl_val)

     d sysDate         s               d   datfmt(*iso)
     d sysDate7        s              7  0
     d sysDate8        s              8  0
     d sysTime6        s              6  0

     D PSDS          ESDS                  extname(SZQ1P)

     dRtvEnv           pr                  extpgm('RTVENV')
     d outEnv                         4a

     dGetClientID      pr                  ExtPgm('RTVCLTID')
     d clientID                       3a

      /copy sPrAtrium
      /copy sPrDate
      /copy sPrJson
      /copy sPrSecure
      /copy sPrSql
      /copy sPrString
      /copy wPrGroup
      /copy wPrEmplr
      /copy wPrUsrPrms
      /include yajl_h
      /Copy wPrOffice
      /Copy wPrClaims

      *entry
     DRequestPgm       pr                  ExtPgm('WIREPORTS')
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
       When inRequest = 'ReportBookOfBusiness';
          BookOfBusiness();
       When inRequest = 'ReportLossSummary';
          LossSummaryReport();
       When inRequest = 'ReportAccidentYearLossRecap';
          AccidentYearLossRecapReport();
       Other;
          Exsr BuildOutput;
       Endsl;
       json += ProcessErrorMessagesJson('*PRE');
       json = %scanrpl(',':'':json:1:1);
       outJSON = json;
       *inlr = *on;

       //=================================================================
       // Read through data based parameters
       //=================================================================
       Begsr BuildOutput;
        Exsr BuildStub;
       Endsr;

       //=================================================================
       // Build stub results - Leave this in the code for possible future requests
       //=================================================================
       Begsr BuildStub;
        BuildGiantJSON(json:'stub':%trim(inRequest) + ' (' +
         %trim(q1Pgm) + ') successfully executed.');
       Endsr;

       //=================================================================
       Begsr PgmStart;
        strAuthority = ioRtnCode;
        Clear ioRtnCode;
        intCo# = UserCo#(inUser);
        strCo# = %Editc(intCo#:'X');
        GetClientID(clientID);
        sysDate  = %Date();
        sysDate7 = %Dec(sysDate:*cymd);
        sysDate8 = %Dec(sysDate:*usa);
        sysTime6 = %Dec(%Time():*hms);
        RtvEnv(strEnv);
        isEnvQuote = strEnv = '*QTE';
        isEnvLive = strEnv = '*LIV';
       Endsr;
      /End-free
       //===================================================
       Dcl-Proc AccidentYearLossRecapReport;
       //===================================================
       // Use standard dsIn's for group, office, etc
       Dcl-Ds dsIn Inz Qualified;
        groupNumber VarChar(3);
        groupName VarChar(60);
        officeNumber VarChar(9);
        officeName VarChar(60);
        agentFEIN VarChar(9);
        agentMod VarChar(3);
        agentName VarChar(40);
        policyFEIN VarChar(9);
        policyName VarChar(100);
        policyStatus VarChar(10);
        claimStatus VarChar(10);
        injuryFromDate VarChar(10);
        injuryToDate VarChar(10);
        chrPolFEIN Char(9);
        policyFromEffDate VarChar(10);
        policyToEffDate VarChar(10);
        sort VarChar(20);
        asOfDate VarChar(10);
       End-Ds;
       Dcl-Ds tmpDual Template Qualified;
        name1 Char(60) Inz;
        name2 Char(225) Inz;
        amtType Char(3) Inz;
        accYr Zoned(3:0) Inz;
        bucket Char(3) Inz;
        rsvType Char(1) Inz;
        amount Zoned(13:2) Inz;
       End-Ds;
       Dcl-Ds dsDualSave LikeDs(tmpDual) Inz;
       Dcl-Ds dsDualSort LikeDs(tmpDual) Inz;
       Dcl-Ds tmpSing Template Qualified;
        name1 Char(60) Inz;
        amtType Char(3) Inz;
        accYr Zoned(3:0) Inz;
        bucket Char(3) Inz;
        rsvType Char(1) Inz;
        amount Zoned(13:2) Inz;
       End-Ds;
       Dcl-Ds dsSingSave LikeDs(tmpSing) Inz;
       Dcl-Ds dsSingSort LikeDs(tmpSing) Inz;
       Dcl-Ds sums Template Inz;
        rsvMtdCmp Zoned(14:2);
        rsvMtdMed Zoned(14:2);
        rsvMtdLeg Zoned(14:2);
        rsvMtdExp Zoned(14:2);
        rsvYtdCmp Zoned(14:2);
        rsvYtdMed Zoned(14:2);
        rsvYtdLeg Zoned(14:2);
        rsvYtdExp Zoned(14:2);
        rsvItdCmp Zoned(14:2);
        rsvItdMed Zoned(14:2);
        rsvItdLeg Zoned(14:2);
        rsvItdExp Zoned(14:2);
        pmtMtdCmp Zoned(14:2);
        pmtMtdMed Zoned(14:2);
        pmtMtdLeg Zoned(14:2);
        pmtMtdExp Zoned(14:2);
        pmtYtdCmp Zoned(14:2);
        pmtYtdMed Zoned(14:2);
        pmtYtdLeg Zoned(14:2);
        pmtYtdExp Zoned(14:2);
        pmtItdCmp Zoned(14:2);
        pmtItdMed Zoned(14:2);
        pmtItdLeg Zoned(14:2);
        pmtItdExp Zoned(14:2);
       End-Ds;
       Dcl-Ds dsYear Qualified Inz;
        accYr Zoned(4:0) Dim(99);
        totals LikeDs(sums) Dim(99);
       End-Ds;
       Dcl-Ds dsRprt Qualified Inz;
        totals LikeDs(sums);
       End-Ds;
       Dcl-Ds dsOut Qualified Inz;
        accYr Zoned(4:0);
        totals LikeDs(sums);
       End-Ds;
       Dcl-S chrAgentFEIN VarChar(9);
       Dcl-S chrAgentMod VarChar(3);
       Dcl-S chrAgentName VarChar(40);
       Dcl-S chrAmtType Char(3);
       Dcl-S chrAsOfCymd Char(7);
       Dcl-S chrBucket Char(3);
       Dcl-S chrClmStatus VarChar(10);
       Dcl-S chrCurrMonth01 Char(7);
       Dcl-S chrCurrYear01 Char(7);
       Dcl-S chrDebug Char(1);
       Dcl-S chrGrpNumber VarChar(3);
       Dcl-S chrInjFromCymd VarChar(7);
       Dcl-S chrInjToCymd VarChar(7);
       Dcl-S chrOfficeName VarChar(60);
       Dcl-S chrOfficeNumber VarChar(5);
       Dcl-S chrPolFEIN VarChar(9);
       Dcl-S chrPolName VarChar(100);
       Dcl-S chrPolFromCymd VarChar(7);
       Dcl-S chrPolStatus VarChar(10);
       Dcl-S chrPolToCymd VarChar(7);
       Dcl-S chrRsvType Char(1);
       Dcl-S chrSort VarChar(20);
       Dcl-S decAccYr Zoned(3:0);
       Dcl-S decAmount Zoned(14:2);
       Dcl-S dtAsOfDate Date;
       Dcl-S dtInjFromDate Date;
       Dcl-S dtInjToDate Date;
       Dcl-S dtPolFromEffDate Date;
       Dcl-S dtPolToEffDate Date;
       Dcl-S dtWorkDate Date;
       Dcl-S hasOutput Ind;
       Dcl-S idx Packed(3:0) Inz(1);
       Dcl-S idz Packed(3:0) Inz(1);
       Dcl-S intAgtFEIN Packed(9:0);
       Dcl-S intAgtMod Packed(3:0);
       Dcl-S intFnd Packed(3:0);
       Dcl-S intPolFEIN Packed(9:0);
       Dcl-S intPos Packed(5:0);
       Dcl-S isDualSort Ind;
       Dcl-S isFirstRead Ind Inz(*On);
       Dcl-S isSingSort Ind;
       Dcl-S strLevel1Hdg VarChar(30);
       Dcl-S strLevel1Name VarChar(60);
       Dcl-S strLevel1Number VarChar(10);
       Dcl-S strLevel1Prefix VarChar(10);
       Dcl-S strLevel2Hdg VarChar(30);
       Dcl-S strLevel2Name VarChar(225);
       Dcl-S strLevel2Number VarChar(10);
       Dcl-S strLevel2Prefix VarChar(10);
       Dcl-S strRptSortVbg VarChar(30);
       Dcl-S strSql VarChar(12000);
       Dcl-S strSqlWith VarChar(5000);
       Dcl-S strSqlWhere VarChar(500);
       Dcl-S strSqlPayments VarChar(2500);
       Dcl-S strSqlReserves VarChar(4000);
       Dcl-S strSumsPrefix VarChar(10);

       Exsr GetParms;
       Exsr ValidateParms;
       If ioRtnCode  <> 'Error';
          Exsr InitializePgmVariables;
          Exsr BuildOutput;
       Endif;
       json += ProcessErrorMessagesJson('*PRE');
       json = %scanrpl(',':'':json:%len(json)-1);
       //---------------------------------------------------
       Begsr AccumulateSums;
       //---------------------------------------------------
        If decAccYr <> 0;
           dsYear.accYr(idx) = decAccYr + 1900;
        Endif;
        Select;
        When chrAmtType = 'PMT';
           Select;
           When chrRsvType = 'C';
              Select;
              When chrBucket = 'MTD';
                 dsYear.totals(idx).pmtMtdCmp = decAmount;
                 dsRprt.totals.pmtMtdCmp += decAmount;
              When chrBucket = 'YTD';
                 dsYear.totals(idx).pmtYtdCmp = decAmount;
                 dsRprt.totals.pmtYtdCmp += decAmount;
              When chrBucket = 'ITD';
                 dsYear.totals(idx).pmtItdCmp = decAmount;
                 dsRprt.totals.pmtItdCmp += decAmount;
              EndSl;
           When chrRsvType = 'M';
              Select;
              When chrBucket = 'MTD';
                 dsYear.totals(idx).pmtMtdMed = decAmount;
                 dsRprt.totals.pmtMtdMed += decAmount;
              When chrBucket = 'YTD';
                 dsYear.totals(idx).pmtYtdMed = decAmount;
                 dsRprt.totals.pmtYtdMed += decAmount;
              When chrBucket = 'ITD';
                 dsYear.totals(idx).pmtItdMed = decAmount;
                 dsRprt.totals.pmtItdMed += decAmount;
              EndSl;
           When chrRsvType = 'L';
              Select;
              When chrBucket = 'MTD';
                 dsYear.totals(idx).pmtMtdLeg = decAmount;
                 dsRprt.totals.pmtMtdLeg += decAmount;
              When chrBucket = 'YTD';
                 dsYear.totals(idx).pmtYtdLeg = decAmount;
                 dsRprt.totals.pmtYtdLeg += decAmount;
              When chrBucket = 'ITD';
                 dsYear.totals(idx).pmtItdLeg = decAmount;
                 dsRprt.totals.pmtItdLeg += decAmount;
              EndSl;
           When chrRsvType = 'E';
              Select;
              When chrBucket = 'MTD';
                 dsYear.totals(idx).pmtMtdExp = decAmount;
                 dsRprt.totals.pmtMtdExp += decAmount;
              When chrBucket = 'YTD';
                 dsYear.totals(idx).pmtYtdExp = decAmount;
                 dsRprt.totals.pmtYtdExp += decAmount;
              When chrBucket = 'ITD';
                 dsYear.totals(idx).pmtItdExp = decAmount;
                 dsRprt.totals.pmtItdExp += decAmount;
              EndSl;
           EndSl;
        When chrAmtType = 'RSV';
           Select;
           When chrRsvType = 'C';
              Select;
              When chrBucket = 'MTD';
                 dsYear.totals(idx).rsvMtdCmp = decAmount;
                 dsRprt.totals.rsvMtdCmp += decAmount;
              When chrBucket = 'YTD';
                 dsYear.totals(idx).rsvYtdCmp = decAmount;
                 dsRprt.totals.rsvYtdCmp += decAmount;
              When chrBucket = 'ITD';
                 dsYear.totals(idx).rsvItdCmp = decAmount;
                 dsRprt.totals.rsvItdCmp += decAmount;
              EndSl;
           When chrRsvType = 'M';
              Select;
              When chrBucket = 'MTD';
                 dsYear.totals(idx).rsvMtdMed = decAmount;
                 dsRprt.totals.rsvMtdMed += decAmount;
              When chrBucket = 'YTD';
                 dsYear.totals(idx).rsvYtdMed = decAmount;
                 dsRprt.totals.rsvYtdMed += decAmount;
              When chrBucket = 'ITD';
                 dsYear.totals(idx).rsvItdMed = decAmount;
                 dsRprt.totals.rsvItdMed += decAmount;
              EndSl;
           When chrRsvType = 'L';
              Select;
              When chrBucket = 'MTD';
                 dsYear.totals(idx).rsvMtdLeg = decAmount;
                 dsRprt.totals.rsvMtdLeg += decAmount;
              When chrBucket = 'YTD';
                 dsYear.totals(idx).rsvYtdLeg = decAmount;
                 dsRprt.totals.rsvYtdLeg += decAmount;
              When chrBucket = 'ITD';
                 dsYear.totals(idx).rsvItdLeg = decAmount;
                 dsRprt.totals.rsvItdLeg += decAmount;
              EndSl;
           When chrRsvType = 'E';
              Select;
              When chrBucket = 'MTD';
                 dsYear.totals(idx).rsvMtdExp = decAmount;
                 dsRprt.totals.rsvMtdExp += decAmount;
              When chrBucket = 'YTD';
                 dsYear.totals(idx).rsvYtdExp = decAmount;
                 dsRprt.totals.rsvYtdExp += decAmount;
              When chrBucket = 'ITD';
                 dsYear.totals(idx).rsvItdExp = decAmount;
                 dsRprt.totals.rsvItdExp += decAmount;
              EndSl;
           Endsl;
        EndSl;
       Endsr;
       //---------------------------------------------------
       Begsr BuildOutput;
       //---------------------------------------------------
        If ioRtnCode = 'Error';
           Leavesr;
        Endif;
        Exsr BuildSqlWhere;
        Exsr BuildSqlWith;
        Exsr BuildSqlReserves;
        Exsr BuildSqlPayments;
        Exsr BuildSqlString;
        Exsr ProcessSql;
        json = %scanrpl(',':'':json:%len(json)-1);
       EndSr;
       //---------------------------------------------------
       Begsr BuildSqlPayments;
       //---------------------------------------------------
        strSqlPayments =
         'claimPayments as '  +
          '(Select PAco# co, ' +
            'Dec(CMacdt/10000,3,0) fyr, ' +
            'PApodt ef, ' +
            'PAretp rsvType, ' +
            'PAtype, ' +
            'PAamt, ' +
            '(AAname || AAagt#) agtName, ' +
            '(OFname || OFofc#) offName, ' +
            '(EMname || EMnam2 || EMemp#) empName ' +
           'From WDPAP ' +
           'Join claim_Info ' +
           'On CMco# = PAco# ' +
           'And CMfnd = PAfnd ' +
           'And CMcase = PAcase), ' +
          'payMTD as ' +
           '(Select ' + strRptSortVbg + ', ''PMT'' rsvOrPay, '  +
             'fyr, ''MTD'' bucket, rsvType, ' +
             'Sum(Case ' +
              'When PAtype In (''R'', ''S'', ''V'') ' +
              'Then PAamt * -1 ' +
              'Else PAamt ' +
              'End) amt ' +
            'From claimPayments ' +
            'Where ef >= ' + chrCurrMonth01 + ' ' +
           'Group By ' + strRptSortVbg + ', fyr, rsvType), ' +
          'payYTD as ' +
          '(Select ' + strRptSortVbg + ', ''PMT'' rsvOrPay, ' +
            'fyr, ''YTD'' bucket, rsvType, ' +
            'Sum(Case ' +
             'When PAtype In (''R'', ''S'', ''V'') ' +
             'Then PAamt * -1 ' +
             'Else PAamt ' +
             'End) amt ' +
            'From claimPayments ' +
            'Where ef >= ' + chrCurrYear01 + ' ' +
           'Group By ' + strRptSortVbg + ', fyr, rsvType), ' +
          'payITD as ' +
          '(Select ' + strRptSortVbg + ', ''PMT'' rsvOrPay, ' +
            'fyr, ''ITD'' bucket, rsvType, ' +
            'Sum(Case ' +
             'When PAtype In (''R'', ''S'', ''V'') ' +
             'Then PAamt * -1 ' +
             'Else PAamt ' +
             'End) amt ' +
            'From claimPayments ' +
            'Group By ' + strRptSortVbg + ', fyr, rsvType) ';
       Endsr;
       //---------------------------------------------------
       Begsr BuildSqlReserves;
       //---------------------------------------------------
        strSqlReserves =
         'claimReserves as ' +
          '(Select * ' +
           'From WMRHP ' +
           'Join claim_Info ' +
           'On CMco# = RHco# ' +
           'And CMfnd = RHfnd ' +
           'And CMcase = RHcase), ' +
         'splitReserves01 as ' +
          '(Select RHCO# co, Dec(CMacdt/10000,3,0) fyr, ' +
            'RHEFDT ef, RHRT01 rt, RHSU01 su, ' +
            '(AAname || AAagt#) agtName, ' +
            '(OFname || OFofc#) offName, ' +
            '(EMname || EMnam2 || EMemp#) empName ' +
           'From claimReserves), ' +
         'splitReserves02 as ' +
          '(Select RHCO# co, Dec(CMACDT/10000,3,0) fyr, ' +
            'RHEFDT ef, RHRT02 rt, RHSU02 su, ' +
            '(AAname || AAagt#) agtName, ' +
            '(OFname || OFofc#) offName, ' +
            '(EMname || EMnam2 || EMemp#) empName ' +
           'From claimReserves), ' +
         'splitReserves03 as ' +
          '(Select RHCO# co, Dec(CMACDT/10000,3,0) fyr, ' +
            'RHEFDT ef, RHRT03 rt, RHSU03 su, ' +
            '(AAname || AAagt#) agtName, ' +
            '(OFname || OFofc#) offName, ' +
            '(EMname || EMnam2 || EMemp#) empName ' +
           'From claimReserves), ' +
         'splitReserves04 as ' +
          '(Select RHCO# co, Dec(CMACDT/10000,3,0) fyr, ' +
            'RHEFDT ef, RHRT04 rt, RHSU04 su, ' +
            '(AAname || AAagt#) agtName, ' +
            '(OFname || OFofc#) offName, ' +
            '(EMname || EMnam2 || EMemp#) empName ' +
           'From claimReserves), ' +
         'splitReserves05 as ' +
          '(Select RHCO# co, Dec(CMACDT/10000,3,0) fyr, ' +
            'RHEFDT ef, RHRT05 rt, RHSU05 su, ' +
            '(AAname || AAagt#) agtName, ' +
            '(OFname || OFofc#) offName, ' +
            '(EMname || EMnam2 || EMemp#) empName ' +
           'From claimReserves), ' +
         'splitReserves06 as ' +
          '(Select RHCO# co, Dec(CMACDT/10000,3,0) fyr, ' +
            'RHEFDT ef, RHRT06 rt, RHSU06 su, ' +
            '(AAname || AAagt#) agtName, ' +
            '(OFname || OFofc#) offName, ' +
            '(EMname || EMnam2 || EMemp#) empName ' +
           'From claimReserves), ' +
         'splitReserves07 as ' +
          '(Select RHCO# co, Dec(CMACDT/10000,3,0) fyr, ' +
            'RHEFDT ef, RHRT07 rt, RHSU07 su, ' +
            '(AAname || AAagt#) agtName, ' +
            '(OFname || OFofc#) offName, ' +
            '(EMname || EMnam2 || EMemp#) empName ' +
           'From claimReserves), ' +
         'splitReserves08 as ' +
          '(Select RHCO# co, Dec(CMACDT/10000,3,0) fyr, ' +
            'RHEFDT ef, RHRT08 rt, RHSU08 su, ' +
            '(AAname || AAagt#) agtName, ' +
            '(OFname || OFofc#) offName, ' +
            '(EMname || EMnam2 || EMemp#) empName ' +
           'From claimReserves), ' +
         'splitReserves09 as ' +
          '(Select RHCO# co, Dec(CMACDT/10000,3,0) fyr, ' +
            'RHEFDT ef, RHRT09 rt, RHSU09 su, ' +
            '(AAname || AAagt#) agtName, ' +
            '(OFname || OFofc#) offName, ' +
            '(EMname || EMnam2 || EMemp#) empName ' +
           'From claimReserves), ' +
         'splitReserves10 as ' +
          '(Select RHCO# co, Dec(CMACDT/10000,3,0) fyr, ' +
            'RHEFDT ef, RHRT10 rt, RHSU10 su, ' +
            '(AAname || AAagt#) agtName, ' +
            '(OFname || OFofc#) offName, ' +
            '(EMname || EMnam2 || EMemp#) empName ' +
           'From claimReserves), ' +
         'splitReserves as ' +
          '(Select * From splitReserves01 ' +
           'Union Select * From splitReserves02 ' +
           'Union Select * From splitReserves03 ' +
           'Union Select * From splitReserves04 ' +
           'Union Select * From splitReserves05 ' +
           'Union Select * From splitReserves06 ' +
           'Union Select * From splitReserves07 ' +
           'Union Select * From splitReserves08 ' +
           'Union Select * From splitReserves09 ' +
           'Union Select * From splitReserves10), ' +
         'splitTypes as ' +
          '(Select sRsv.co, sRsv.fyr, sRsv.ef, sRsv.su, sRsv.rt, ' +
            'sRsv.agtName, sRsv.offName, sRsv.empName, ' +
            '(Select RTTYPE From WTRTP ' +
             'Where RTco# = sRsv.CO and RTkey = sRsv.RT) as rsvType ' +
             'From splitReserves sRsv ' +
             'Where sRsv.su <> 0), ' +
         'rsvMTD as ' +
          '(Select ' + strRptSortVbg + ', ''RSV'' rsvOrPay, ' +
            'fyr, ''MTD'' bucket, rsvType, sum(su) amt ' +
           'From splitTypes ' +
           'Where ef >= ' + chrCurrMonth01 + ' ' +
           'Group By ' + strRptSortVbg + ', fyr, rsvType), ' +
         'rsvYTD as ' +
          '(Select ' + strRptSortVbg + ', ''RSV'' rsvOrPay, ' +
            'fyr, ''YTD'' bucket, rsvType, sum(su) amt ' +
           'From splitTypes ' +
           'Where ef >= ' + chrCurrYear01 + ' ' +
           'Group By ' + strRptSortVbg + ', fyr, rsvType), ' +
         'rsvITD as ' +
          '(Select ' + strRptSortVbg + ', ''RSV'' rsvOrPay, ' +
            'fyr, ''ITD'' bucket, rsvType, sum(su) amt ' +
           'From splitTypes ' +
           'Group By ' + strRptSortVbg + ', fyr, rsvType), ';
       Endsr;
       //---------------------------------------------------
       Begsr BuildSqlString;
       //---------------------------------------------------
        json = %ScanRpl('""':'{':json:%Len(json)-1);
        strSql = strSqlWith + strSqlReserves + strSqlPayments;
        strSql +=
         'Select * From rsvMTD Union ' +
         'Select * From rsvYTD Union ' +
         'Select * From rsvITD Union ' +
         'Select * From payMTD Union ' +
         'Select * From payYTD Union ' +
         'Select * From payITD ' +
         'Order By ' + strRptSortVbg + ', ' +
         'rsvOrPay desc, fyr desc, bucket, rsvType';
        If chrDebug = 'Y';
           json += BuildJsonNode('sqlString':strSql) + ',';
        Endif;
       Endsr;
       //---------------------------------------------------
       Begsr BuildSqlWhere;
       //---------------------------------------------------
        strSqlWhere =
         ' Where CMco# = ' + strCo# +
          ' And CMfnd In ' +
           '(Select FIfnd From WMFIP ' +
            'Where CMfnd = FIfnd And FIclaim = ''Y'') ' +
          ' And CMacdt Between ' + chrInjFromCymd +
          ' And ' + chrInjToCymd +
          ' And ELpfdt Between ' + chrPolFromCymd +
          ' And ' + chrPolToCymd;
        If chrGrpNumber <> *Blanks;
           strSqlWhere +=
            ' And CMFND = ' + chrGrpNumber;
        EndIf;
        If chrOfficeName <> *Blanks;
           strSqlWhere +=
            ' And Upper(OFname) Like ''%'' || Upper(''' +
            chrOfficeName + ''') || ''%''';
        Endif;
        If chrOfficeNumber <> *Blanks;
           strSqlWhere +=
            ' And CMoff# = ' + chrOfficeNumber;
        EndIf;
        If chrAgentName <> *Blanks;
           strSqlWhere +=
            ' And Upper(AAname) Like ''%'' || Upper(''' +
            chrAgentName + ''') || ''%''';
        EndIf;
        If chrAgentFEIN <> *Blanks;
           strSqlWhere +=
            ' And AAfein = ' + chrAgentFEIN +
            ' And AAmod = ' + chrAgentMod;
        EndIf;
        If chrPolFEIN <> *Blanks;
           strSqlWhere +=
            ' And EMfein = ' + chrPolFEIN;
        EndIf;
        Select;
        When chrPolStatus = 'Cancelled';
           strSqlWhere +=
            ' And EMcad1 Between 0 And ' + chrAsOfCymd;
        When chrPolStatus = 'Expired';
           strSqlWhere +=
            ' And ELptdt <= ' + chrAsOfCymd;
        When chrPolStatus = 'Pending';
           strSqlWhere +=
            ' And ELpfdt > ' + chrAsOfCymd;
        When chrPolStatus = 'Active';
           strSqlWhere +=
            ' And ELpfdt <= ' + chrAsOfCymd +
            ' And ELptdt > ' + chrAsOfCymd +
            ' And (EMcad1 = 0 Or EMcad1 > ' + chrAsOfCymd + ')';
        EndSl;
        Select;
        When chrClmStatus = 'Open';
           strSqlWhere +=
            ' And CMstat = ''O''';
        When chrClmStatus = 'Closed';
           strSqlWhere +=
            ' And CMstat = ''C''';
        EndSl;
        If chrPolFEIN <> *Blanks;
           strSqlWhere +=
            ' And EMfein = ' + chrPolFEIN;
        EndIf;
        If chrPolName <> *Blanks;
           strSqlWhere +=
            ' And Upper(EMname || EMnam2) Like ''%'' || Upper(''' +
            chrPolName + ''') || ''%''';
        EndIf;
        strSqlWhere += '), ';
       Endsr;
       //---------------------------------------------------
       Begsr BuildSqlWith;
       //---------------------------------------------------
       // From WPIEMPLR/SqlWithPolicyInfo
       // called from SqlWithClaimInfo
       //  max_agt as
       //  agent_history as
       //  agent_info as
       //  office_name as
       //  policy_number as
       //  policy_info as
       // From WPICLAIMS:
       //  claim_info as
        strSqlWith = 'With ' +
         SqlWithClaimInfo(dtAsOfDate);
       // Remove ending ')' from strSqlWith to insert where clause
        intPos = %Len(%Trim(strSqlWith));
        If %Subst(strSqlWith:intPos:1) = ')';
           %Subst(strSqlWith:intPos:1) = *Blank;
        Endif;
        strSqlWith += strSqlWhere;
       Endsr;
       //---------------------------------------------------
       Begsr GetParms;
       //---------------------------------------------------
        inJson = inParms;
        Reset i0;
        node0 = yajl_string_load_tree(inJson:strErrorMsg);
        If strErrorMsg <> ' ';
           AddErrorMessageJson(strErrorMsg:ioRtnCode);
           Leavesr;
        Endif;
        json += ' "requestParms" : {';
        Dow YAJL_object_loop(node0:i0:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'debug';
              chrDebug = YAJL_get_string(val);  // Y triggers debug stuff
           When key = 'group';
              Exsr GetParmsGroup;
           When key = 'office';
              Exsr GetParmsOffice;
           When key = 'agent';
              Exsr GetParmsAgent;
           When key = 'policy';
              Exsr GetParmsPolicy;
           When key = 'policyStatus';
              dsIn.policyStatus = YAJL_get_string(val);
           When key = 'claimStatus';
              dsIn.claimStatus = YAJL_get_string(val);
           When key = 'injuryDate';
              Exsr GetParmsInjuryDate;
           When key = 'policyEffectiveDateRange';
              Exsr GetParmsPolicyEffDateRange;
           When key = 'sort';
              dsIn.sort = YAJL_get_string(val);
           When key = 'asOfDate';
              dsIn.asOfDate = YAJL_get_string(val);
           Endsl;
        Enddo;
        json = %scanrpl(',':'':json:%len(json)-1);
        json += '},';
       EndSr;
       //---------------------------------------------------
       Begsr GetParmsAgent;
       //---------------------------------------------------
        json = %scanrpl('""':'{':json:%len(json)-1);
        node1 = YAJL_object_find(node0:key);
        Reset i1;
        Dow YAJL_object_loop(node1:i1:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'fein';
              dsIn.agentFEIN = YAJL_get_string(val);
           When key = 'mod';
              dsIn.agentMod = YAJL_get_string(val);
           When key = 'name';
              dsIn.agentName = YAJL_get_string(val);
           Endsl;
        Enddo;
        json += '}';
       Endsr;
       //---------------------------------------------------
       Begsr GetParmsGroup;
       //---------------------------------------------------
        json = %scanrpl('""':'{':json:%len(json)-1);
        node1 = YAJL_object_find(node0:key);
        Reset i1;
        Dow YAJL_object_loop(node1:i1:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'number';
              dsIn.groupNumber = YAJL_get_string(val);
           When key = 'name';
              dsIn.groupName = YAJL_get_string(val);
           Endsl;
        Enddo;
        json += '}';
       Endsr;
       //---------------------------------------------------
       Begsr GetParmsInjuryDate;
       //---------------------------------------------------
        json = %scanrpl('""':'{':json:%len(json)-1);
        node1 = YAJL_object_find(node0:key);
        Reset i1;
        Dow YAJL_object_loop(node1:i1:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'from';
              dsIn.injuryFromDate = YAJL_get_string(val);
           When key = 'to';
              dsIn.injuryToDate = YAJL_get_string(val);
           Endsl;
        Enddo;
        json += '}';
       Endsr;
       //---------------------------------------------------
       Begsr GetParmsOffice;
       //---------------------------------------------------
        json = %scanrpl('""':'{':json:%len(json)-1);
        node1 = YAJL_object_find(node0:key);
        Reset i1;
        Dow YAJL_object_loop(node1:i1:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'number';
              dsIn.officeNumber = YAJL_get_string(val);
           When key = 'name';
              dsIn.officeName = YAJL_get_string(val);
           Endsl;
        Enddo;
        json += '}';
       EndSr;
       //---------------------------------------------------
       Begsr GetParmsPolicy;
       //---------------------------------------------------
        json = %scanrpl('""':'{':json:%len(json)-1);
        node1 = YAJL_object_find(node0:key);
        Reset i1;
        Dow YAJL_object_loop(node1:i1:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'fein';
              dsIn.policyFEIN = YAJL_get_string(val);
           When key = 'name';
              dsIn.policyName = YAJL_get_string(val);
           Endsl;
        Enddo;
        json += '}';
       EndSr;
       //---------------------------------------------------
       Begsr GetParmsPolicyEffDateRange;
       //---------------------------------------------------
        json = %scanrpl('""':'{':json:%len(json)-1);
        node1 = YAJL_object_find(node0:key);
        Reset i1;
        Dow YAJL_object_loop(node1:i1:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'from';
              dsIn.policyFromEffDate = YAJL_get_string(val);
           When key = 'to';
              dsIn.policyToEffDate = YAJL_get_string(val);
           Endsl;
        Enddo;
        json += '}';
       EndSr;
       //---------------------------------------------------
       Begsr InitializePgmVariables;
       //---------------------------------------------------
        chrGrpNumber = %Char(intFnd);
        chrAsOfCymd = %Char(dtAsOfDate:*Cymd0);
        chrCurrMonth01 = %Subst(chrAsOfCymd:1:5) + '01';
        chrCurrYear01 = %Subst(chrAsOfCymd:1:3) + '0101';
        chrInjFromCymd = %Char(dtInjFromDate:*Cymd0);
        chrInjToCymd = %Char(dtInjToDate:*Cymd0);
        chrPolFromCymd = %Char(dtPolFromEffDate:*Cymd0);
        chrPolToCymd = %Char(dtPolToEffDate:*Cymd0);
        chrOfficeName = dsIn.OfficeName;
        chrOfficeNumber = dsIn.OfficeNumber;
        chrAgentName = dsIn.agentName;
        chrAgentFEIN = dsIn.agentFEIN;
        chrAgentMod = dsIn.agentMod;
        chrPolName = dsIn.policyName;
        chrPolFEIN = dsIn.policyFEIN;
       EndSr;
       //---------------------------------------------------
       Begsr ProcessSql;
       //---------------------------------------------------
        If ioRtnCode = 'Error';
           Leavesr;
        Endif;
        Exec SQL Close cursorAccYearLossRecap;
        Exec SQL Prepare sqlAccYearLossRecap From :strSql;
        Exec SQL Declare cursorAccYearLossRecap
         Cursor For sqlAccYearLossRecap;
        Exec SQL Open cursorAccYearLossRecap;
        Dou isValidFetch(sqlStt) = *Off;
           If isDualSort = *On;
              Reset dsDualSort;
              Exec SQL Fetch Next From cursorAccYearLossRecap
               Into :dsDualSort;
           Else;
              Reset dsSingSort;
              Exec SQL Fetch Next From cursorAccYearLossRecap
               Into :dsSingSort;
           Endif;
           If isValidFetch(sqlStt) = *Off;
              If hasOutPut;
                 If isDualSort = *On;
                   // Write out year/s JSON
                    Exsr WriteJsonYear;
                   // Finish last name2 JSON
                    json += '}]';
                   // Finish last name1 JSON
                    json += '}]';
                   // Write out report totals
                    Reset dsOut;
                    dsOut.totals = dsRprt.totals;
                    Exsr WriteJsonSums;
                 Else;
                   // Write out last year/s JSON
                    Exsr WriteJsonYear;
                   // Finish last name1 JSON
                    json += '}]';
                   // Write out report totals
                    Reset dsOut;
                    dsOut.totals = dsRprt.totals;
                    Exsr WriteJsonSums;
                 Endif;
              Endif;
              Exec SQL Close cursorAccYearLossRecap;
              Leave;
           EndIf;
           hasOutput = *On;
          // Dual Sort
           Select;
           When isDualSort = *Off;
           When isDualSort = *On And isFirstRead = *On;
              isFirstRead = *Off;
              json += BuildJsonNode('sort':dsIn.sort) + ',';
              json += strLevel1Hdg;
              strLevel1Name = %Subst(dsDualSort.name1:1:40);
              strLevel1Number = %Subst(dsDualSort.name1:41:10);
              Exsr WriteJsonName1;
             // Start level 2 JSON
              json += strLevel2Hdg;
              If chrSort = 'AGENT/POLICY';
                 strLevel2Name = %Subst(dsDualSort.name2:1:100);
                 strLevel2Number = %Subst(dsDualSort.name2:101:10);
              Else;
                 strLevel2Name = %Subst(dsDualSort.name2:1:40);
                 strLevel2Number = %Subst(dsDualSort.name2:41:10);
              Endif;
              Exsr WriteJsonName2;
           When dsDualSort.name1 <> dsDualSave.name1;
             // Break level 1
              Exsr WriteJsonYear;                // Write out year/s sums for last name2
              json += '}]';                      // Finish previous name2's JSON
              json += '},';                      // Finish previous name1's JSON
              strLevel1Name = %Subst(dsDualSort.name1:1:40);  // Write out name1 to JSON
              strLevel1Number = %Subst(dsDualSort.name1:41:10);
              Exsr WriteJsonName1;
              json += strLevel2Hdg;              // Repeat level 2 heading
              If chrSort = 'AGENT/POLICY';
                 strLevel2Name = %Subst(dsDualSort.name2:1:100);
                 strLevel2Number = %Subst(dsDualSort.name2:101:10);
              Else;
                 strLevel2Name = %Subst(dsDualSort.name2:1:40);
                 strLevel2Number = %Subst(dsDualSort.name2:41:10);
              Endif;
              Exsr WriteJsonName2;               // Write out name2 to JSON
              Reset dsYear;                      // Clear and restart dsYear
              idx = 1;
           When dsDualSort.name2 <> dsDualSave.name2;
             // Break level 2
              Exsr WriteJsonYear;                // Write out year sums for last name2
              json += '},';                      // Finish previous name2's JSON
              If chrSort = 'AGENT/POLICY';
                 strLevel2Name = %Subst(dsDualSort.name2:1:100);
                 strLevel2Number = %Subst(dsDualSort.name2:101:10);
              Else;
                 strLevel2Name = %Subst(dsDualSort.name2:1:40);
                 strLevel2Number = %Subst(dsDualSort.name2:41:10);
              Endif;
              Exsr WriteJsonName2;               // Write out name2 to JSON
              Reset dsYear;                      //  Clear and restart dsYear
              idx = 1;
           When dsDualSort.accYr <> dsDualSave.accYr;
              idx += 1;
           EndSl;
          // Single Sort
           Select;
           When isSingSort = *Off;
           When isSingSort = *On And isFirstRead = *On;
              isFirstRead = *Off;
              json += strLevel1Hdg;
              strLevel1Name = %Subst(dsSingSort.name1:1:40);
              strLevel1Number = %Subst(dsSingSort.name1:41:10);
              Exsr WriteJsonName1;
           When dsSingSort.name1 <> dsSingSave.name1;
             // Break level 1
              Exsr WriteJsonYear;                // Write out year/s sums for last name1
              json += '},';                      // Finish previous name1's JSON
              strLevel1Name = %Subst(dsSingSort.name1:1:40);
              strLevel1Number = %Subst(dsSingSort.name1:41:10);
              Exsr WriteJsonName1;               // Write out name1 to JSON
              Reset dsYear;                      // Clear and restart dsYear
              idx = 1;
           When dsSingSort.accYr <> dsSingSave.accYr;
              idx += 1;
           EndSl;
           If isDualSort = *On;
              dsDualSave = dsDualSort;
              decAccYr = dsDualSort.accYr;
              decAmount = dsDualSort.amount;
              chrAmtType = dsDualSort.amtType;
              chrBucket = dsDualSort.bucket;
              chrRsvType = dsDualSort.rsvType;
           Else;
              dsSingSave = dsSingSort;
              decAccYr = dsSingSort.accYr;
              decAmount = dsSingSort.amount;
              chrAmtType = dsSingSort.amtType;
              chrBucket = dsSingSort.bucket;
              chrRsvType = dsSingSort.rsvType;
           Endif;
           Exsr AccumulateSums;
        Enddo;
       EndSr;
       //---------------------------------------------------
       Begsr ValidateParms;
       //---------------------------------------------------
       // Should we verify user access to the group?
        Monitor;
           If dsIn.groupNumber <> *Blanks;
              intFnd = %Int(dsIn.groupNumber);
           Endif;
        On-error;
           AddErrorMessageJson('A group number must be a number ' +
           'if provided.':ioRtnCode);
        Endmon;
        Monitor;
           If dsIn.AsOfDate <> *Blanks;
              dtAsOfDate = %Date(dsIn.asOfDate:*Usa0);
           Else;
              dtAsOfDate = %Date();
           Endif;
        On-Error;
           AddErrorMessageJson('The As Of Date must be a valid USA date ' +
           'if provided.':ioRtnCode);
        EndMon;
        Monitor;
           If dsIn.agentFEIN <> *Blanks;
              intAgtFEIN = %Int(dsIn.agentFEIN);
           Endif;
        On-error;
           AddErrorMessageJson('The agent FEIN must be a number ' +
            'if provided.':ioRtnCode);
        Endmon;
        Monitor;
           If dsIn.agentMod <> *Blanks;
              intAgtMod = %Int(dsIn.agentMod);
           Endif;
        On-error;
           AddErrorMessageJson('The agent mod must be a number ' +
            'if provided.':ioRtnCode);
        Endmon;
        Monitor;
           If dsIn.policyFEIN <> *Blanks;
              intPolFEIN = %Int(dsIn.policyFEIN);
           Endif;
        On-error;
           AddErrorMessageJson('The policy FEIN must be a number ' +
            'if provided.':ioRtnCode);
        Endmon;
        Monitor;
           If dsIn.injuryFromDate <> *Blanks;
              dtInjFromDate = %Date(dsIn.injuryFromDate:*Usa0);
           Else;
              dtInjFromDate = d'1980-01-01';
           Endif;
        On-Error;
           AddErrorMessageJson('The injury from date must be a valid USA ' +
            'date if provided.':ioRtnCode);
        EndMon;
        Monitor;
           If dsIn.injuryToDate <> *Blanks;
              dtInjToDate = %Date(dsIn.injuryToDate:*Usa0);
           Else;
              dtInjToDate = dtAsOfDate;
           Endif;
        On-Error;
           AddErrorMessageJson('The injury to date must be a valid USA ' +
            'date if provided.':ioRtnCode);
        EndMon;
        Monitor;
           If dsIn.policyFromEffDate <> *Blanks;
              dtPolFromEffDate = %Date(dsIn.injuryFromDate:*Usa0);
           Else;
              dtPolFromEffDate = d'1980-01-01';
           Endif;
        On-Error;
           AddErrorMessageJson('The policy effective from date must be a ' +
            'valid USA date if provided.':ioRtnCode);
        EndMon;
        Monitor;
           If dsIn.policyToEffDate <> *Blanks;
              dtPolToEffDate = %Date(dsIn.injuryToDate:*Usa0);
           Else;
              dtPolToEffDate = dtAsOfDate;
           Endif;
        On-Error;
           AddErrorMessageJson('The policy effective to date must be a ' +
            'valid USA date if provided.':ioRtnCode);
        EndMon;
        chrSort = Upper(dsIn.sort);
        Select;
        When chrSort = 'AGENT/POLICY';
           strLevel1Hdg = '"agent":[';
           strLevel1Prefix = 'agent';
           strLevel2Hdg = '"policies":[';
           strLevel2Prefix = 'policy';
           strSumsPrefix = strLevel2Prefix;
           strRptSortVbg = 'agtName, empName';
           isDualSort = *On;
        When chrSort = 'OFFICE/AGENT';
           strLevel1Hdg = '"office":[';
           strLevel1Prefix = 'office';
           strLevel2Hdg = '"agent":[';
           strLevel2Prefix = 'agent';
           strSumsPrefix = strLevel2Prefix;
           strRptSortVbg = 'offName, agtName';
           isDualSort = *On;
        When chrSort = 'AGENT/OFFICE';
           strLevel1Hdg = '"agent":[';
           strLevel1Prefix = 'agent';
           strLevel2Hdg = '"office":[';
           strLevel2Prefix = 'office';
           strSumsPrefix = strLevel2Prefix;
           strRptSortVbg = 'agtName, offName';
           isDualSort = *On;
        When chrSort = 'AGENT';
           strLevel1Hdg = '"agent":[';
           strLevel1Prefix = 'agent';
           strSumsPrefix = strLevel1Prefix;
           strRptSortVbg = 'agtName';
           isSingSort = *On;
        When chrSort = 'OFFICE';
           strLevel1Hdg = '"office":[';
           strLevel1Prefix = 'office';
           strSumsPrefix = strLevel1Prefix;
           strRptSortVbg = 'offName';
           isSingSort = *On;
        Other;
           AddErrorMessageJson('Report sort selection not valid.':
            ioRtnCode);
        Endsl;
        chrPolStatus = Upper(dsIn.policyStatus);
        Select;
        When chrPolStatus = *Blanks;
        When chrPolStatus = 'CANCELLED';
        When chrPolStatus = 'PENDING';
        When chrPolStatus = 'ACTIVE';
        When chrPolStatus = 'EXPIRED';
        Other;
           AddErrorMessageJson('Policy status selection not valid.':
            ioRtnCode);
        Endsl;
        chrClmStatus = dsIn.claimStatus;
        Select;
        When chrClmStatus = *Blanks;
        When chrClmStatus = 'OPEN';
        When chrClmStatus = 'CLOSED';
        Other;
           AddErrorMessageJson('Claim status selection not valid.':
            ioRtnCode);
        Endsl;
       Endsr;
      //====================================================
       Begsr WriteJsonName1;
      //====================================================
        json += '{';
        json += BuildJsonNode(strLevel1Prefix + 'Name':
         strLevel1Name) + ',';
        json += BuildJsonNode(strLevel1Prefix + 'Number':
         strLevel1Number) + ',';
        json += BuildJsonNode(strLevel1Prefix + 'Display':
         %Trim(strLevel1Name) + ' - ' + strLevel1Number) + ',';
        If isDualSort = *Off;
           json += '"' + strLevel1Prefix + 'Year":[';
        Endif;
       Endsr;
      //====================================================
       Begsr WriteJsonName2;
      //====================================================
        json += '{';
        json += BuildJsonNode(strLevel2Prefix + 'Name':
         strLevel2Name) + ',';
        json += BuildJsonNode(strLevel2Prefix + 'Number':
         strLevel2Number) + ',';
        json += BuildJsonNode(strLevel2Prefix + 'Display':
         %Trim(strLevel2Name) + ' - ' + strLevel2Number) + ',';
           json += '"' + strLevel2Prefix + 'Year":[';
       Endsr;
      //====================================================
       Begsr WriteJsonSums;
      //====================================================
       // Write out sums for year/s or report totals
        If dsOut.AccYr <> 0;
           json += '{' +
            BuildJsonNode('year':%Editc(dsOut.accYr:'X')) + ',';
        Else;
           json += ',';
           strSumsPrefix = 'report';
        Endif;
        json += BuildJsonNode(strSumsPrefix + 'ReservesMTDIndemnity':
         %EditC(dsOut.totals.rsvMtdCmp:'N')) + ',';
        json += BuildJsonNode(strSumsPrefix + 'ReservesMTDMedical':
         %EditC(dsOut.totals.rsvMtdMed:'N')) + ',';
        json += BuildJsonNode(strSumsPrefix + 'ReservesMTDLegal':
         %EditC(dsOut.totals.rsvMtdLeg:'N')) + ',';
        json += BuildJsonNode(strSumsPrefix + 'ReservesMTDExpense':
         %EditC(dsOut.totals.rsvMtdExp:'N')) + ',';
        json += BuildJsonNode(strSumsPrefix + 'ReservesYTDIndemnity':
         %EditC(dsOut.totals.rsvYtdCmp:'N')) + ',';
        json += BuildJsonNode(strSumsPrefix + 'ReservesYTDMedical':
         %EditC(dsOut.totals.rsvYtdMed:'N')) + ',';
        json += BuildJsonNode(strSumsPrefix + 'ReservesYTDLegal':
         %EditC(dsOut.totals.rsvYtdLeg:'N')) + ',';
        json += BuildJsonNode(strSumsPrefix + 'ReservesYTDExpense':
         %EditC(dsOut.totals.rsvYtdExp:'N')) + ',';
        json += BuildJsonNode(strSumsPrefix + 'ReservesITDIndemnity':
         %EditC(dsOut.totals.rsvItdCmp:'N')) + ',';
        json += BuildJsonNode(strSumsPrefix + 'ReservesITDMedical':
         %EditC(dsOut.totals.rsvItdMed:'N')) + ',';
        json += BuildJsonNode(strSumsPrefix + 'ReservesITDLegal':
         %EditC(dsOut.totals.rsvItdLeg:'N')) + ',';
        json += BuildJsonNode(strSumsPrefix + 'ReservesITDExpense':
         %EditC(dsOut.totals.rsvItdExp:'N')) + ',';
        json += BuildJsonNode(strSumsPrefix + 'PaidMTDIndemnity':
         %EditC(dsOut.totals.pmtMtdCmp:'N')) + ',';
        json += BuildJsonNode(strSumsPrefix + 'PaidMTDMedical':
         %EditC(dsOut.totals.pmtMtdMed:'N')) + ',';
        json += BuildJsonNode(strSumsPrefix + 'PaidMTDLegal':
         %EditC(dsOut.totals.pmtMtdLeg:'N')) + ',';
        json += BuildJsonNode(strSumsPrefix + 'PaidMTDExpense':
         %EditC(dsOut.totals.pmtMtdExp:'N')) + ',';
        json += BuildJsonNode(strSumsPrefix + 'PaidYTDIndemnity':
         %EditC(dsOut.totals.pmtYtdCmp:'N')) + ',';
        json += BuildJsonNode(strSumsPrefix + 'PaidYTDMedical':
         %EditC(dsOut.totals.pmtYtdMed:'N')) + ',';
        json += BuildJsonNode(strSumsPrefix + 'PaidYTDLegal':
         %EditC(dsOut.totals.pmtYtdLeg:'N')) + ',';
        json += BuildJsonNode(strSumsPrefix + 'PaidYTDExpense':
         %EditC(dsOut.totals.pmtYtdExp:'N')) + ',';
        json += BuildJsonNode(strSumsPrefix + 'PaidITDIndemnity':
         %EditC(dsOut.totals.pmtItdCmp:'N')) + ',';
        json += BuildJsonNode(strSumsPrefix + 'PaidITDMedical':
         %EditC(dsOut.totals.pmtItdMed:'N')) + ',';
        json += BuildJsonNode(strSumsPrefix + 'PaidITDLegal':
         %EditC(dsOut.totals.pmtItdLeg:'N')) + ',';
        json += BuildJsonNode(strSumsPrefix + 'PaidITDExpense':
         %EditC(dsOut.totals.pmtItdExp:'N'));
        If dsOut.AccYr <> 0;
           json += '},';
        Endif;
       Endsr;
      //====================================================
       Begsr WriteJsonYear;
      //====================================================
        For idz = 1 to 99;
           If dsYear.accYr(idz) = 0;
              Leave;
           Endif;
           dsOut.accYr = dsYear.accYr(idz);
           dsOut.totals = dsYear.totals(idz);
           Exsr WriteJsonSums;
        EndFor;
        json = %ScanRpl(',':'':json:%Len(json)-1);
        json += ']';
       Endsr;
       End-Proc;
      //=================================================================
      // Book of Business Reporting
      //=================================================================
     PBookOfBusiness   b

     ddecCo#           s              3  0 inz
     ddecCount         s             10i 0 inz
     ddecFnd           s              3  0 inz
     DdecLosses        s             15  2 inz
     DdecLossRatio     s             15  2 inz
     DdecPremEarned    s             15  2 inz
     DdecPremWritten   s             15  2 inz
     dstrSqlWhere      s           5000    inz varying
     dstrSqlWith       s           5000    inz varying

     DdsActive         ds                  inz qualified
     D count                         10i 0
     D premWritten                   15  2

     DdsCalcInfo       ds                  inz qualified
     D co#                            3s 0
     D fnd                            3s 0
     D fyr                            3s 0
     D emp#                           9s 0
     D div                            5s 0
     D premWritten                   11  2

     ddsAgency         ds                  qualified
     d fein                           9
     d mod                            3

     ddsEmployer       ds                  qualified
     d fein                           9
     d number                         9

     DdsIn             ds                  inz qualified
     D co#                            3
     D group                          3
     D agency                              likeds(dsAgency)
     D emp                                 likeds(dsEmployer)

      /Free
       Exsr GetParms;
       Exsr BuildOutput;
       json += ProcessErrorMessagesJson('*PRE');
       json = %scanrpl(',':'':json:1:1);

       //=================================================================
       // Build output
       //=================================================================
       Begsr BuildOutput;
        If ioRtnCode = 'Error';
           Leavesr;
        Endif;
        //Exsr BuildStub;
        Exsr BuildSqlWith;
        Exsr BuildOutputTotals;
        json = %scanrpl(',':'':json:%len(json)-1);
       Endsr;

       //=================================================================
       // Build output - Claims Info
       //=================================================================
       Begsr BuildOutputClaims;
        json += '"claims": {';

         // Claim count - All policies
         strSql = strSqlWith +
          'select count(*) ' +
          ' from claims_all ' +
          ' where CMstat = ''O'' ';
         Exsr BuildSql;
         Exec SQL fetch next from cursorBOB into :decCount;
         json += BuildJsonNode('open':%char(decCount)) + ',';

         // Claims reported - All policies
         json += '"claimsReported": {';
         // 7 days
         strSql = strSqlWith +
          'select count(*) ' +
          ' from claims_all ' +
          ' where CMentD between ' + %char(%Dec(sysDate-%days(7):*Cymd)) +
          '  and ' + %char(sysDate7);
         Exsr BuildSql;
         Exec SQL fetch next from cursorBOB into :decCount;
         json += BuildJsonNode('7days':%char(decCount)) + ',';
         json = %scanrpl(',':'},':json:%len(json)-1);

         // Claim count - Active policies
         strSql = strSqlWith +
          'select count(*) ' +
          ' from claims_active ' +
          ' where CMstat = ''O'' ';
         Exsr BuildSql;
         Exec SQL fetch next from cursorBOB into :decCount;
         //json += BuildJsonNode('open':%char(decCount)) + ',';

         // Claims reported - Active policies
         //json += '"claimsReported": {';
         // 7 days
         strSql = strSqlWith +
          'select count(*) ' +
          ' from claims_active ' +
          ' where CMentD between ' + %char(%Dec(sysDate-%days(7):*Cymd)) +
          '  and ' + %char(sysDate7);
         Exsr BuildSql;
         Exec SQL fetch next from cursorBOB into :decCount;
         //json += BuildJsonNode('7days':%char(decCount)) + ',';
         //json = %scanrpl(',':'},':json:%len(json)-1);

        json = %scanrpl(',':'':json:%len(json)-1);
        json += '},';
       Endsr;

       //=================================================================
       // Build output - Active Policy Info
       //=================================================================
       Begsr BuildOutputPoliciesActive;
        json += '"policesActive": {';
        strSql = strSqlWith +
         'select count(*), sum(ELanPr) ' +
         ' from policies_active ';
        Exsr BuildSql;
        Exec SQL fetch next from cursorBOB into :dsActive;
        json += BuildJsonNode('count':%char(dsActive.count)) + ',';
        json += BuildJsonNode('incurredLosses':%char(decLosses)) + ',';
        json += BuildJsonNode('lossRatio':%char(decLossRatio)) + ',';
        json += '"premium": {';
        json += BuildJsonNode('written':%char(dsActive.premWritten)) + ',';
        json += BuildJsonNode('earned':%char(decPremEarned)) + ',';
        json = %scanrpl(',':'},':json:%len(json)-1);

        json = %scanrpl(',':'':json:%len(json)-1);
        json += '},';
       Endsr;

       //=================================================================
       // Build output - Cancelled Policy Info
       //=================================================================
       Begsr BuildOutputPoliciesCancelled;
        json += '"policiesCancelled": {';
        // Pending
        strSql = strSqlWith +
         'select count(*) ' +
         ' from policies_cx ' +
         ' where cx_date > ' + %char(sysDate7) + ' And ' +   // 12/23/21
         %char(sysDate7) + ' between ELpfDt And ELptDt';     // 12/23/21
        Exsr BuildSql;
        Exec SQL fetch next from cursorBOB into :decCount;
        json += BuildJsonNode('pending':%char(decCount)) + ',';
        // 30 Days
        strSql = strSqlWith +
         'select count(*) ' +
         ' from policies_cx ' +
         ' where cx_date between ' + %char(%Dec(sysDate-%days(30):*Cymd)) +
         '  and ' + %char(sysDate7);
        Exsr BuildSql;
        Exec SQL fetch next from cursorBOB into :decCount;
        json += BuildJsonNode('30days':%char(decCount)) + ',';
        // 60 Days
        strSql = strSqlWith +
         'select count(*) ' +
         ' from policies_cx ' +
         ' where cx_date between ' + %char(%Dec(sysDate-%days(60):*Cymd)) +
         ' and ' + %char(%Dec(sysDate-%days(31):*Cymd));
        Exsr BuildSql;
        Exec SQL fetch next from cursorBOB into :decCount;
        json += BuildJsonNode('60days':%char(decCount)) + ',';
        // 90 Days
        strSql = strSqlWith +
         'select count(*) ' +
         ' from policies_cx ' +
         ' where cx_date between ' + %char(%Dec(sysDate-%days(90):*Cymd)) +
         ' and ' + %char(%Dec(sysDate-%days(61):*Cymd));
        Exsr BuildSql;
        Exec SQL fetch next from cursorBOB into :decCount;
        json += BuildJsonNode('90days':%char(decCount)) + ',';

        json = %scanrpl(',':'':json:%len(json)-1);
        json += '},';
       Endsr;

       //=================================================================
       // Build output - Expiring Policy Info
       //=================================================================
       Begsr BuildOutputPoliciesExpiring;
        json += '"policiesExpiring": {';
        // 30 Days
        strSql = strSqlWith +
         'select count(*) ' +
         ' from policies_active ' +
         ' where ELptDt between ' + %char(%Dec(sysDate+%days( 1):*Cymd)) +
         '  and ' + %char(%Dec(sysDate+%days(30):*Cymd));
        Exsr BuildSql;
        Exec SQL fetch next from cursorBOB into :decCount;
        json += BuildJsonNode('30days':%char(decCount)) + ',';
        // 60 Days
        strSql = strSqlWith +
         'select count(*) ' +
         ' from policies_active ' +
         ' where ELptDt between ' + %char(%Dec(sysDate+%days(31):*Cymd)) +
         ' and ' + %char(%Dec(sysDate+%days(60):*Cymd));
        Exsr BuildSql;
        Exec SQL fetch next from cursorBOB into :decCount;
        json += BuildJsonNode('60days':%char(decCount)) + ',';
        // 90 Days
        strSql = strSqlWith +
         'select count(*) ' +
         ' from policies_active ' +
         ' where ELptDt between ' + %char(%Dec(sysDate+%days(61):*Cymd)) +
         ' and ' + %char(%Dec(sysDate+%days(90):*Cymd));
        Exsr BuildSql;
        Exec SQL fetch next from cursorBOB into :decCount;
        json += BuildJsonNode('90days':%char(decCount)) + ',';

        json = %scanrpl(',':'':json:%len(json)-1);
        json += '},';
       Endsr;

       //=================================================================
       // Build output - Totals
       //=================================================================
       Begsr BuildOutputTotals;
        Exsr CalcPolicyInfo;
        json += '"totals":{';
        Exsr BuildOutputPoliciesActive;
        Exsr BuildOutputPoliciesExpiring;
        Exsr BuildOutputPoliciesCancelled;
        Exsr BuildOutputClaims;
        json = %scanrpl(',':'':json:%len(json)-1);
        json += '},';
       Endsr;

       //=================================================================
       // Build SQL
       //=================================================================
       Begsr BuildSql;
        Reset decCount;
        Reset dsActive;
        Reset dsCalcInfo;
        Exec sql close cursorBOB;
        Exec sql prepare sqlBOB from :strSql;
        Exec sql declare cursorBOB cursor for sqlBOB;
        Exec sql open cursorBOB;
       Endsr;

       //=================================================================
       // Build SQL
       //=================================================================
       Begsr BuildSqlWith;
        strSqlWith = 'with ' +
         'max_agt as ' +
         '(select AHco#, AHfnd, AHemp#, AHdiv, AHfyr, max(AHefDt) max_date ' +
         '  from WMAHP ' +
         '  where AHstat = ''A'' and AHefDt <= ' + %editc(sysDate7:'X') +
         '  group by AHco#, AHfnd, AHemp#, AHdiv, AHfyr ' +
         '), ' +
         'agt_num as ' +
         '(select a.AHco#, a.AHfnd, a.AHemp#, a.AHdiv, a.AHfyr, AHefDt, ' +
         '  AHagt#, AHstat ' +
         '  from WMAHP a ' +
         '  join max_agt m ' +
         '   on a.AHco# = m.AHco# and a.AHfnd = m.AHfnd ' +
         '    and a.AHemp# = m.AHemp# and a.AHdiv = m.AHdiv ' +
         '    and a.AHfyr = m.AHfyr and a.AHefDt = m.max_date ' +
         '  where AHstat = ''A'' ' +
         '), ' +
         'agt_fein as ' +
         '(select AHco#, AHfnd, AHemp#, AHdiv, AHfyr, AHefDt, AHagt#, AHstat,'+
         '  AAfein, AAmod ' +
         '  from agt_num ' +
         '  join WMAAP ' +
         '   on AAco# = AHco# and AAfnd = AHfnd and AAagt# = AHagt# ' +
         '),' +
         'policies_all as ' +
         '(select * ' +
         '  from WDELP ' +
         '  join agt_fein ' +
         '   on ELco# = AHco# and ELfnd = AHfnd ' +
         '    and ELemp# = AHemp# and ELdiv = AHdiv ' +
         '    and ELfyr = AHfyr ' +
         '   where ELco# = ' + %char(decCo#);
        strSqlWith = 'with ' + SqlWithPolicyInfo + ', ' +
         'policies_all as ' +
         '(select * ' +
         '  from policy_info ' +
         '   where ELco# = ' + %char(decCo#);
        If dsIn.group <> *blank;
           strSqlWith += ' and ELfnd = ' + dsIn.group;
        Else;
           strSqlWith +=
            ' and ELfnd in (select FIfnd from WMFIP where FIpolicy = ''Y'')';
        Endif;
        If dsIn.agency.fein <> *blank;
           strSqlWith += ' and AAfein = ' + dsIn.agency.fein;
        Endif;
        If dsIn.agency.mod <> *blank;
           strSqlWith += ' and AAmod = ' + dsIn.agency.mod;
        Endif;
        If dsIn.emp.fein <> *blank;
           strSqlWith += ' and digits(EMfnd)||digits(EMemp#) in ' +
            '(select digits(EMfnd)||digits(EMemp#) ' +
            '  from WMEMP where EMfein = ' + dsIn.emp.fein + ')';
        Endif;
        If dsIn.emp.number <> *blank;
           strSqlWith += ' and ELemp# = ' + dsIn.emp.number;
        Endif;
        strSqlWith += '), ' +
         'policies_active as ' +
         '(select * ' +
         '  from policies_all ' +
         ' where ' + %char(sysDate7) + ' between ELpfDt and ELptDt ' +
         '  and (EMcaD1 = 0 or EMcaD1 >= ' + %char(sysDate7) + ')' +
         '  and EMstRp = ''Y''' +
         '), ' +
         'cx_emp as ' +
         '(select EMco#, EMfnd, EMemp#, EMdiv, ' +
         '  EMcaD1, EMcaD2, EMcaD3, EMcaD4, EMcaD5, ' +
         '  case ' +
         '   when EMcaD5 <> 0 then EMcaD5 ' +
         '   when EMcaD4 <> 0 then EMcaD4 ' +
         '   when EMcaD3 <> 0 then EMcaD3 ' +
         '   when EMcaD2 <> 0 then EMcaD2 ' +
         '   when EMcaD1 <> 0 then EMcaD1 ' +
         '   else 0 ' +
         '  end as cx_date_emp ' +
         '  from WMEMP ' +
         '), ' +
         'cx as ' +
         '(select CXco#, CXfnd, CXemp#, CXdiv, max(CXfyr) cxfyr, ' +  //12/23/21
         '        max(CXcxDt) cx_date_cx  ' +  // 12/23/21
         '  from WDCXP ' +
         '  where CXreDt = 0 ' +
         '  group by CXco#, CXfnd, CXemp#, CXdiv, CXfyr ' +  // 12/23/21
         '), ' +
         'cx_combined as ' +
         '(select CXco#, CXfnd, CXemp#, CXdiv, CXfyr, ' +
         '  case ' +
         '   when cx_date_cx > cx_date_emp then cx_date_emp ' +  // 12/23/21
         '   else cx_date_emp ' +
         '  end as cx_date ' +
         '  from cx_emp ' +
         '  join cx ' +
         '   on CXco# = EMco# and CXfnd = EMfnd ' +
         '    and CXemp# = EMemp# and CXdiv = EMdiv ' +
         '), ' +
         'policies_cx as ' +
         '(select * ' +
         '  from cx_combined a ' +
         '  join policies_all ' +
         '   on a.CXco# = ELco# and a.CXfnd = ELfnd and a.CXfyr = ELfyr ' +
         '    and a.CXemp# = ELemp# and a.CXdiv = ELdiv ' +
         '  where EMstRp = ''Y''' +
         '), ' +
         'claims_all as ' +
         '(select * ' +
         '  from WMCMP ' +
         '  join policies_all ' +
         '   on CMco# = ELco# and CMfnd = ELfnd ' +
         '    and CMemp# = ELemp# and CMdiv = ELdiv ' +
         '    and CMfyr = ELfyr ' +
         '), ' +
         'claims_active as ' +
         '(select * ' +
         '  from WMCMP ' +
         '  join policies_active ' +
         '   on CMco# = ELco# and CMfnd = ELfnd ' +
         '    and CMemp# = ELemp# and CMdiv = ELdiv ' +
         '    and CMfyr = ELfyr ' +
         ') ';
       Endsr;

       //=================================================================
       // Build stub results - Leave this in the code for possible future requests
       //=================================================================
       Begsr BuildStub;
        json += BuildJsonNode('stub':%trim(inRequest) + ' (' +
         %trim(q1Pgm) + ') successfully executed. {BookOfBusiness}') + ',';
       Endsr;

       //=================================================================
       // Calculate information from active policies
       //=================================================================
       Begsr CalcPolicyInfo;
        Reset decLosses;
        Reset decLossRatio;
        Reset decPremEarned;
        strSql = strSqlWith +
         'select ' +
         ' sum(CMar01 + CMar02 + CMar03 + CMar04 + CMar05 + ' +
         '     CMar06 + CMar07 + CMar08 + CMar09 + CMar10 + ' +
         '     CMrr01 + CMrr02 + CMrr03 + CMrr04 + CMrr05 + ' +
         '     CMrr06 + CMrr07 + CMrr08 + CMrr09 + CMrr10) ' +
         ' from claims_active ';
        Exsr BuildSql;
        Exec SQL fetch next from cursorBOB into :decLosses;
        strSql = strSqlWith +
         'select ELco#, ELfnd, ELfyr, ELemp#, ELdiv, ELanPr ' +
         ' from policies_active ' +
         ' order by ELco#, ELfnd, ELfyr, ELemp#, ELdiv';
        Exsr BuildSql;
        Dou not IsValidFetch(sqlstt);
           Exec SQL fetch next from cursorBOB into :dsCalcInfo;
           Select;
           When not IsValidFetch(sqlstt);
              Iter;
           Endsl;
           decPremEarned +=
            CalcEarnedPrem(dsCalcInfo.co#:dsCalcInfo.fnd
             :dsCalcInfo.fyr:dsCalcInfo.emp#:dsCalcInfo.div);
           decPremWritten += dsCalcInfo.premWritten;
        Enddo;
        If decPremEarned <> *zero;
           //*** Begin Chg *** 06/10/22 *********************************
           //*** decLossRatio = %dech(decLosses / decPremEarned * 100:5:2);
           decLossRatio = %dech(decLosses / decPremEarned * 100:15:2);
           //*** End   Chg *** 06/10/22 *********************************
        Endif;
       Endsr;

       //=================================================================
       // Break parms out from inParms
       //=================================================================
       Begsr GetParms;
        inJson = inParms;
        Reset i0;
        node0 = yajl_string_load_tree(inJson:strErrorMsg);
        If strErrorMsg <> ' ';
           AddErrorMessageJson(strErrorMsg:ioRtnCode);
           Leavesr;
        Endif;

        json += ' "requestParms" : {';
        Dow YAJL_object_loop(node0:i0:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'company';
              dsIn.co# = YAJL_get_string(val);
           When key = 'group';
              dsIn.group = YAJL_get_string(val);
           When key = 'agency';
              Exsr GetParmsAgency;
           When key = 'employer';
              Exsr GetParmsEmployer;
           Endsl;
        Enddo;
        json = %scanrpl(',':'':json:%len(json)-1);
        json += '},';

        Exsr ValidateParms;
       Endsr;

       //=================================================================
       // Break parms out from inParms - Agency
       //=================================================================
       Begsr GetParmsAgency;
        json = %scanrpl('""':'{':json:%len(json)-1);
        node1 = YAJL_object_find(node0:key);
        Reset i1;
        Dow YAJL_object_loop(node1:i1:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'fein';
              dsIn.agency.fein = YAJL_get_string(val);
           When key = 'mod';
              dsIn.agency.mod = YAJL_get_string(val);
           Endsl;
        Enddo;
        json += '}';
       Endsr;

       //=================================================================
       // Break parms out from inParms - Employer
       //=================================================================
       Begsr GetParmsEmployer;
        json = %scanrpl('""':'{':json:%len(json)-1);
        node1 = YAJL_object_find(node0:key);
        Reset i1;
        Dow YAJL_object_loop(node1:i1:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'fein';
              dsIn.emp.fein = YAJL_get_string(val);
           When key = 'number';
              dsIn.emp.number = YAJL_get_string(val);
           Endsl;
        Enddo;
        json += '}';
       Endsr;

       //=================================================================
       // Validate incoming parameters
       //=================================================================
       Begsr ValidateParms;
        // Company # must be valid if not then default to user parms.
        Monitor;
           decCo# = %int(dsIn.co#);
        On-error;
        Endmon;
        If decCo# = *zero;
           decCo# = intCo#;
        Endif;

        Monitor;
           decFnd = %int(dsIn.group);
        On-error;
        Endmon;

        Select;
        When dsIn.group = *blank;
        When isEnvQuote and not UseGroupInPortal(decCo#:decFnd:'Quote');
           AddErrorMessageJson('Group ' + %char(decFnd) +
            ' is not included for service requested ' +
            '(' + %trim(inRequest) + ').':ioRtnCode);
        When isEnvLive and not UseGroupInPortal(decCo#:decFnd:'Policy');
           AddErrorMessageJson('Group ' + %char(decFnd) +
            ' is not included for service requested ' +
            '(' + %trim(inRequest) + ').':ioRtnCode);
        Endsl;

        // Validate agency information
        Select;
        When dsIn.agency.fein <> *blanks;
           Monitor;
              integer = %int(dsIn.agency.fein);
           On-Error;
              AddErrorMessageJson('Agent FEIN has character data.'
               :ioRtnCode);
           EndMon;
        Endsl;
        Select;
        When dsIn.agency.mod <> *blanks;
           Monitor;
              integer = %int(dsIn.agency.mod);
           On-Error;
              AddErrorMessageJson('Agent modifier has character data.'
               :ioRtnCode);
           EndMon;
        Endsl;

        // Validate employer information
        Select;
        When dsIn.emp.fein <> *blanks;
           Monitor;
              integer = %int(dsIn.emp.fein);
           On-Error;
              AddErrorMessageJson('Employer FEIN has character data.'
               :ioRtnCode);
           EndMon;
        Endsl;
        Select;
        When dsIn.emp.number <> *blanks;
           Monitor;
              integer = %int(dsIn.emp.number);
           On-Error;
              AddErrorMessageJson('Employer number has character data.'
               :ioRtnCode);
           EndMon;
        Endsl;

       Endsr;
      /End-free
     PBookOfBusiness   e

      //=================================================================
      // Calculate Earned Premium
      //=================================================================
     PCalcEarnedPrem   b
     DCalcEarnedPrem   pi            11  2
     D inCo#                          3  0 const
     D inFnd                          3  0 const
     D inFyr                          3  0 const
     D inEmp#                         9  0 const
     D inDiv                          5  0 const

     dCalcEarnPrem     pr                  extpgm('SRPRT1')
     d ##Co#                          3    const
     d ##Fnd                          3    const
     d ##Fyr                          3    const
     d ##Emp#                         9    const
     d ##Div                          5    const
     d ##Date                         8    const
     d ##Amount                      11
     d ##Amount2                     11
     d ##Term                         1    const

     DoutAmt           s             11
     DoutAmt2          s             11
     D*** Beg Add *** 06/13/24 *********************************************
     DChrEprmDS        DS            11
     D DecOutAmt                     11s 2
     D*** End Add *** 06/13/24 *********************************************
      /Free
       CalcEarnPrem(%editc(inCo#:'X'):%editc(inFnd:'X'):%editc(inFyr:'X')
        :%editc(inEmp#:'X'):%editc(inDiv:'X'):%editc(sysDate8:'X')
        :outAmt:outAmt2:'1');
       //*** Beg Chg *** 06/13/24 ******************************************
       //***Return %dec(outAmt:11:2)/100;
       ChrEprmDs = outAmt;
       Return DecOutAmt;
       //*** End Chg *** 06/13/24 ******************************************

      /End-free
     PCalcEarnedPrem   e

      //=================================================================
      // Loss Summary Reporting
      //=================================================================
     PLossSummaryReport...
     P                 b

     d strSql          s          10000    inz varying
     d strSqlPaid      s           5000    inz varying
     d dateEffDtTo     s               d   Inz datfmt(*usa)
     d dateEffDtFrom   s               d   Inz datfmt(*usa)
     d dateInjDtTo     s               d   Inz datfmt(*usa)
     d dateInjDtFrom   s               d   Inz datfmt(*usa)
     d dateZero        s               d   inz datfmt(*usa)
     d intRptDate8     s              8  0 Inz
     d isEofPaid       s              1n   Inz
     d strType         s              1a
     d agtFirst        s               n   Inz(*On)
     d ofcFirst        s               n   Inz(*On)
     d InsFirst        s               n   Inz(*On)
     d flgSortOrder    s               n   Inz(*On)
     d count           s              9s 0 Inz(0)
     d sysDate         s               d
     d sysDate7        s              7  0
     d decFnd          s              3  0 inz
     d holdAgent#      s              5  0
     d holdOffice#     s              5a
     d holdEmp#        s              9  0
     d holdDiv         s              5  0
     d moreRows        s               n   Inz(*On)
     d aryAR           s              9  2 dim(#ofFin)
     d aryRR           s              9  2 dim(#ofFin)
     d aryRT           s              2    dim(#ofFin)
     d strKey          s             10a
     d i2              s              5  0
     d subset          s               n   Inz(*Off)
     d offAgencySort   s               n   Inz(*Off)
     d agtInsuredSort  s               n   Inz(*Off)
     d agentSort       s               n   Inz(*Off)
     d strAgtDisplay   s             60a
     d strOfcDisplay   s             60a
     d strInsDisplay   s             60a

      //Agent Totals:
      //Payment this period
     ddecPtpIndAgt     s             15s 2 inz(0)
     ddecPtpMedAgt     s             15s 2 inz(0)
     ddecPtpLegAgt     s             15s 2 inz(0)
     ddecPtpExpAgt     s             15s 2 inz(0)
     ddecPtpTotAgt     s             15s 2 inz(0)
      //Paid to date
     ddecPtdIndAgt     s             15s 2 inz(0)
     ddecPtdMedAgt     s             15s 2 inz(0)
     ddecPtdLegAgt     s             15s 2 inz(0)
     ddecPtdExpAgt     s             15s 2 inz(0)
     ddecPtdTotAgt     s             15s 2 inz(0)
      //O/S Reserves
     ddecOsrIndAgt     s             15s 2 inz(0)
     ddecOsrMedAgt     s             15s 2 inz(0)
     ddecOsrLegAgt     s             15s 2 inz(0)
     ddecOsrExpAgt     s             15s 2 inz(0)
     ddecOsrTotAgt     s             15s 2 inz(0)
      //Total Incurred
     ddecTinIndAgt     s             15s 2 inz(0)
     ddecTinMedAgt     s             15s 2 inz(0)
     ddecTinLegAgt     s             15s 2 inz(0)
     ddecTinExpAgt     s             15s 2 inz(0)
     ddecTinTotAgt     s             15s 2 inz(0)

      //Report Totals:
      //Payment this period
     ddecPtpIndRpt     s             15s 2 inz(0)
     ddecPtpMedRpt     s             15s 2 inz(0)
     ddecPtpLegRpt     s             15s 2 inz(0)
     ddecPtpExpRpt     s             15s 2 inz(0)
     ddecPtpTotRpt     s             15s 2 inz(0)
      //Paid to date
     ddecPtdIndRpt     s             15s 2 inz(0)
     ddecPtdMedRpt     s             15s 2 inz(0)
     ddecPtdLegRpt     s             15s 2 inz(0)
     ddecPtdExpRpt     s             15s 2 inz(0)
     ddecPtdTotRpt     s             15s 2 inz(0)
      //O/S Reserves
     ddecOsrIndRpt     s             15s 2 inz(0)
     ddecOsrMedRpt     s             15s 2 inz(0)
     ddecOsrLegRpt     s             15s 2 inz(0)
     ddecOsrExpRpt     s             15s 2 inz(0)
     ddecOsrTotRpt     s             15s 2 inz(0)
      //Total Incurred
     ddecTinIndRpt     s             15s 2 inz(0)
     ddecTinMedRpt     s             15s 2 inz(0)
     ddecTinLegRpt     s             15s 2 inz(0)
     ddecTinExpRpt     s             15s 2 inz(0)
     ddecTinTotRpt     s             15s 2 inz(0)

      //Insured Totals:
      //Payment this period
     ddecPtpIndIns     s             15s 2 inz(0)
     ddecPtpMedIns     s             15s 2 inz(0)
     ddecPtpLegIns     s             15s 2 inz(0)
     ddecPtpExpIns     s             15s 2 inz(0)
     ddecPtpTotIns     s             15s 2 inz(0)
      //Paid to date
     ddecPtdIndIns     s             15s 2 inz(0)
     ddecPtdMedIns     s             15s 2 inz(0)
     ddecPtdLegIns     s             15s 2 inz(0)
     ddecPtdExpIns     s             15s 2 inz(0)
     ddecPtdTotIns     s             15s 2 inz(0)
      //O/S Reserves
     ddecOsrIndIns     s             15s 2 inz(0)
     ddecOsrMedIns     s             15s 2 inz(0)
     ddecOsrLegIns     s             15s 2 inz(0)
     ddecOsrExpIns     s             15s 2 inz(0)
     ddecOsrTotIns     s             15s 2 inz(0)
      //Total Incurred
     ddecTinIndIns     s             15s 2 inz(0)
     ddecTinMedIns     s             15s 2 inz(0)
     ddecTinLegIns     s             15s 2 inz(0)
     ddecTinExpIns     s             15s 2 inz(0)
     ddecTinTotIns     s             15s 2 inz(0)

      //Office Totals:
      //Payment this period
     ddecPtpIndOfc     s             15s 2 inz(0)
     ddecPtpMedOfc     s             15s 2 inz(0)
     ddecPtpLegOfc     s             15s 2 inz(0)
     ddecPtpExpOfc     s             15s 2 inz(0)
     ddecPtpTotOfc     s             15s 2 inz(0)
      //Paid to date
     ddecPtdIndOfc     s             15s 2 inz(0)
     ddecPtdMedOfc     s             15s 2 inz(0)
     ddecPtdLegOfc     s             15s 2 inz(0)
     ddecPtdExpOfc     s             15s 2 inz(0)
     ddecPtdTotOfc     s             15s 2 inz(0)
      //O/S Reserves
     ddecOsrIndOfc     s             15s 2 inz(0)
     ddecOsrMedOfc     s             15s 2 inz(0)
     ddecOsrLegOfc     s             15s 2 inz(0)
     ddecOsrExpOfc     s             15s 2 inz(0)
     ddecOsrTotOfc     s             15s 2 inz(0)
      //Total Incurred
     ddecTinIndOfc     s             15s 2 inz(0)
     ddecTinMedOfc     s             15s 2 inz(0)
     ddecTinLegOfc     s             15s 2 inz(0)
     ddecTinExpOfc     s             15s 2 inz(0)
     ddecTinTotOfc     s             15s 2 inz(0)

     ddsGroup          ds                  qualified
     d number                         3
     d name                          60

     ddsOffice         ds                  qualified
     d name                          60
     d number                         5

     ddsAgent          ds                  qualified
     d fein                           9
     d mod                            3
     d name                          40

     ddsEmployer       ds                  qualified
     d fein                           9
     d name                          40

     ddsInjuryDtRange  ds                  qualified inz
     d from                                likeds(dsDate)
     d to                                  likeds(dsDate)

     ddsEffDtRange     ds                  qualified inz
     d from                                likeds(dsDate)
     d to                                  likeds(dsDate)

     ddsDate           ds                  qualified inz
     d dt8                            8
     d dt7                            7  0

     DdsPaid           ds                  inz qualified
     D recType                        1a
     D amount                        15s 0

     DdsIn             ds                  inz qualified
     D group                               likeds(dsGroup)
     D office                              likeds(dsOffice)
     D agency                              likeds(dsAgent)
     D employer                            likeds(dsEmployer)
     D polStatus                     20a
     D claimStatus                   10a
     D effDtRange                          likeds(dsEffDtRange)
     D injDtRange                          likeds(dsInjuryDtRange)
     D sort                          40a

     d dsLossSumRpt    ds                  Inz Qualified
     d  CMCo#                         3  0
     d  CMFnd                         3  0
     d  FNname                       60
     d  CMFyr                         3  0
     d  CMCase                        7  0
     d  CMEmp#                        9  0
     d  CMDiv                         5  0
     d  EMName                       40a
     d  CMoff#                        5a
     d  OFname                       40a
     d  AAagt#                        5  0
     d  AAname                       40a
     d  CMrt01                        2a
     d  CMrt02                        2a
     d  CMrt03                        2a
     d  CMrt04                        2a
     d  CMrt05                        2a
     d  CMrt06                        2a
     d  CMrt07                        2a
     d  CMrt08                        2a
     d  CMrt09                        2a
     d  CMrt10                        2a
     d  CMrr01                        9  2
     d  CMrr02                        9  2
     d  CMrr03                        9  2
     d  CMrr04                        9  2
     d  CMrr05                        9  2
     d  CMrr06                        9  2
     d  CMrr07                        9  2
     d  CMrr08                        9  2
     d  CMrr09                        9  2
     d  CMrr10                        9  2
     d  CMar01                        9  2
     d  CMar02                        9  2
     d  CMar03                        9  2
     d  CMar04                        9  2
     d  CMar05                        9  2
     d  CMar06                        9  2
     d  CMar07                        9  2
     d  CMar08                        9  2
     d  CMar09                        9  2
     d  CMar10                        9  2
     d  CXdate                        7  0

     d#ofFin           c                   10

      /Free
       Exsr GetParms;
       Exsr ValidateParms;
       Exsr BuildSql;
       Exsr ProcessSql;

       //=================================================================
       // Build Report Record
       //=================================================================
       Begsr BldReportRcd;

        Select;
        When agentSort = *On;
           If dsLossSumRpt.AAagt# <> holdAgent#;
              If holdAgent# <> 0;
                 ExSr WriteAgentTotal;
              Endif;
              holdAgent# = dsLossSumRpt.AAagt#;
              Exsr WriteAgentInfo;
           Endif;
           ExSr SumAmounts;
        When agtInsuredSort = *On;
           If dsLossSumRpt.AAagt# <> holdAgent# or
            dsLossSumRpt.CMemp# <> holdEmp# or
            dsLossSumRpt.CMdiv <> holdDiv;
              Select;
              When dsLossSumRpt.AAagt# <> holdAgent#;
                 If holdAgent# <> 0;
                    ExSr WriteInsuredTotal;
                    json += '],';
                    ExSr WriteAgentTotal;
                 Endif;
                 Exsr WriteAgentInfo;
                 ExSr WriteInsuredInfo;
                 holdDiv = dsLossSumRpt.CMdiv;
                 holdEmp# = dsLossSumRpt.CMemp#;
                 holdAgent# = dsLossSumRpt.AAagt#;
              When dsLossSumRpt.CMemp# <> holdEmp# or
               dsLossSumRpt.CMdiv <> holdDiv;
                 If holdEmp# <> 0;
                    ExSr WriteInsuredTotal;
                 Endif;
                 Exsr WriteInsuredInfo;
                 holdEmp# = dsLossSumRpt.CMemp#;
                 holdDiv = dsLossSumRpt.CMdiv;
              Endsl;
           Endif;
           ExSr SumAmounts;
        When offAgencySort = *On;
           If dsLossSumRpt.CMoff# <> holdOffice# or
            dsLossSumRpt.AAagt# <> holdAgent#;
              Select;
              When dsLossSumRpt.CMoff# <> holdOffice#;
                 If holdOffice# <> ' ';
                    ExSr WriteAgentTotal;
                    json += '],';
                    ExSr WriteOfficeTotal;
                 Endif;
                 ExSr WriteOfficeInfo;
                 Exsr WriteAgentInfo;
                 holdOffice# = dsLossSumRpt.CMoff#;
                 holdAgent# = dsLossSumRpt.AAagt#;
              When dsLossSumRpt.AAagt# <> holdAgent#;
                 If holdAgent# <> 0;
                    ExSr WriteAgentTotal;
                 Endif;
                 Exsr WriteAgentInfo;
                 holdAgent# = dsLossSumRpt.AAagt#;
              Endsl;
           Endif;
           ExSr SumAmounts;
        Endsl;

       Endsr;
       //=================================================================
       // Build SQL
       //=================================================================
       Begsr BuildSql;
        If ioRtnCode = 'Error';
            json = %scanrpl(',':'':json:%len(json)-1);
           Leavesr;
        Endif;
        sysDate  = %Date();
        sysDate7 = %Dec(sysDate:*Cymd);

        strSql = 'with ' + SqlWithClaimInfo + ', ' +
         'cx_date as ' +
         '  (Select c.EMCO#  as CXco#,  ' +
         '          c.EMfnd  as CXfnd,  ' +
         '          c.EMemp# as CXemp#,' +
         '          c.EMdiv  as CXdiv,  ' +
         '   Max(case ' +
         '   when c.EMcaD5 <> 0 then c.EMcaD5 ' +
         '   when c.EMcaD4 <> 0 then c.EMcaD4 ' +
         '   when c.EMcaD3 <> 0 then c.EMcaD3 ' +
         '   when c.EMcaD2 <> 0 then c.EMcaD2 ' +
         '   when c.EMcaD1 <> 0 then c.EMcaD1 ' +
         '   else 0 end) as cxDate ' +
         '   From WMEMP c ' +
         '   Where c.EMstrp = ''Y'' ' +
         '   Group By c.EMco#, c.EMfnd, c.EMemp#, c.EMdiv) ' +
         'select CMco#, CMfnd, FNname, CMfyr, CMcase, CMemp#, CMdiv, EMname, '+
          //*** Beg Add ***** 04/03/25 *********************
          //***'CMoff#, OFname, AAagt#, AAname, CMrt01, CMrt02, CMrt03, CMrt04, ' +
         'CMoff#, x.OFname, AAagt#, AAname, CMrt01, CMrt02, CMrt03, CMrt04, ' +
          //*** End Add ***** 04/03/25 *********************
         'CMrt05, CMrt06, CMrt07, CMrt08, CMrt09, CMrt10, CMrr01, CMrr02, ' +
         'CMrr03, CMrr04, CMrr05, CMrr06, CMrr07, CMrr08, CMrr09, CMrr10, ' +
         'CMar01, CMar02, CMar03, CMar04, CMar05, CMar06, CMar07, ' +
         'CMar08, CMar09, CMar10, cxDate ' +
         ' from claim_info ' +
          //*** Beg Chg ***** 04/03/25 *********************
         //*** ' join WMOFP on CMco# = OFco# and CMoff# = OFofc# ' +
         ' join WMOFP x on CMco# = x.OFco# and CMoff# = x.OFofc# ' +
          //*** End Chg ***** 04/03/25 *********************
         ' join WMFNP on CMco# = FNco# And CMfnd = FNfnd ' +
         ' join cx_date on CMco# = CXco# And CMfnd = CXfnd ' +
         ' and CMemp# = CXemp# and CMdiv = CXdiv ';

        If dsIn.group.name    <> ' ' or dsIn.group.number    <> ' ' or
           dsIn.office.name   <> ' ' or dsIn.office.number   <> ' ' or
           dsIn.agency.fein   <> ' ' or dsIn.agency.name     <> ' ' or
           dsIn.agency.mod    <> ' ' or
           dsIn.employer.fein <> ' ' or dsIn.employer.name   <> ' ' or
           (dsIn.polStatus    <> ' ' and dsIn.polStatus   <> 'All') or
           (dsIn.claimStatus  <> ' ' and dsIn.claimStatus  <> 'All') or
           (dsIn.effDtRange.from.dt8  <> ' ' and
            dsIn.effDtRange.from.dt8 <> '00000000')  or
           (dsIn.effDtRange.to.dt8 <> ' ' and
            dsIn.effDtRange.to.dt8 <> '00000000') or
           (dsIn.injDtRange.from.dt8  <> ' ' and
            dsIn.injDtRange.from.dt8 <> '00000000')  or
           (dsIn.injDtRange.to.dt8 <> ' ' and
            dsIn.injDtRange.to.dt8 <> '00000000');

           strSql += ' Where ';

           //Group# Filter;
           If dsIn.group.number <> ' ';
              subset = *On;
              strSql += 'CMfnd = ' + %trim(dsIn.group.number);
           Endif;
           //Group Name Filter;
           If dsIn.group.name <> ' ';
              If subset = *Off;
                subset = *On;
                strSql += 'Upper(FNname) like ''%'' || Upper(''' +
                 %Trim(dsIn.group.name) + ''') || ''%''';
              Else;
                strSql += ' and Upper(FNname) like ''%'' || Upper(''' +
                 %Trim(dsIn.group.name) + ''') || ''%''';
              Endif;
           Endif;

           // Policy Effective Date Filters
           If (dsIn.effDtRange.from.dt8 <> ' ' and
              dsIn.effDtRange.from.dt8 <> '00000000') or
              (dsIn.effDtRange.to.dt8 <> ' ' and
              dsIn.effDtRange.to.dt8 <> '00000000');
              If subset = *Off;
                 subset = *On;
                 strSql += 'ELpfdt between ' +
                  %editc(dsIn.effDtRange.from.dt7:'X') +
                  ' and ' + %editc(dsIn.effDtRange.to.dt7:'X');
              Else;
                 strSql += ' and ELpfdt between ' +
                  %editc(dsIn.effDtRange.from.dt7:'X') +
                  ' and ' + %editc(dsIn.effDtRange.to.dt7:'X');
              EndIf;
           EndIf;

           // Injury Date Filters
           If (dsIn.injDtRange.from.dt8 <> ' ' and
              dsIn.injDtRange.from.dt8 <> '00000000') or
              (dsIn.injDtRange.to.dt8 <> ' ' and
              dsIn.injDtRange.to.dt8 <> '00000000');
              If subset = *Off;
                 subset = *On;
                 strSql += 'CMacdt between ' +
                  %editc(dsIn.injDtRange.from.dt7:'X') +
                  ' and ' + %editc(dsIn.injDtRange.to.dt7:'X');
              Else;
                 strSql += ' and CMacdt between ' +
                  %editc(dsIn.injDtRange.from.dt7:'X') +
                  ' and ' + %editc(dsIn.injDtRange.to.dt7:'X');
              EndIf;
           EndIf;

           //Office filter
           If dsIn.office.number <> *blank;
              If subset = *Off;
                 subset = *On;
                 strSql += 'CMoff#  = ' + dsIn.office.number;
              Else;
                 strSql += ' and CMoff#  = ' + dsIn.office.number;
              Endif;
           Endif;
           If dsIn.office.name <> *blank;
              If subset = *Off;
                 subset = *On;
                 strSql += 'Upper(OFname) like ''%'' || Upper(''' +
                   %Trim(dsIn.Agency.name) + ''') || ''%''';
              Else;
                 strSql += ' and Upper(OFname) like ''%'' || Upper(''' +
                   %Trim(dsIn.Agency.name) + ''') || ''%''';
              Endif;
           Endif;

           // Agency Fein Filter
           If dsIn.agency.fein <> *blank;
              If subset = *Off;
                 subset = *On;
                 strSql += 'AGfein  = ' + dsIn.agency.fein;
              Else;
                 strSql += ' and AGfein  = ' + dsIn.agency.fein;
              Endif;
           Endif;
           // Agency Modifier
           If dsIn.agency.mod <> *blank;
              If subset = *Off;
                 subset = *On;
                 strSql += 'AGmod  = ' + dsIn.agency.mod;
              Else;
                 strSql += ' and AGmod  = ' + dsIn.agency.mod;
              Endif;
           Endif;
           //Agency Name
           If dsIn.agency.name <> *blank;
              If subset = *Off;
                 subset = *On;
                 strSql += 'Upper(AAname) like ''%'' || Upper(''' +
                  %Trim(dsIn.Agency.name) + ''') || ''%''';
              Else;
                 strSql += ' and Upper(AAname) like ''%'' || Upper(''' +
                  %Trim(dsIn.Agency.name) + ''') || ''%''';
              Endif;
           Endif;

           //Employer filter
           If dsIn.employer.fein <> *blank;
              If subset = *Off;
                 subset = *On;
                 strSql += 'EMfein = ' + dsIn.employer.fein;
              Else;
                 strSql += ' and EMfein = ' + dsIn.employer.fein;
              Endif;
           Endif;
           If dsIn.employer.name <> *blank;
              If subset = *Off;
                 subset = *On;
                 strSql += 'Upper(EMname) like ''%'' || Upper(''' +
                      %Trim(dsIn.employer.name) + ''') || ''%''';
              Else;
                 strSql += ' and Upper(EMname) like ''%'' || Upper(''' +
                   %Trim(dsIn.employer.name) + ''') || ''%''';
              Endif;
           Endif;

           //Policy status filter
           If dsIn.polStatus <> *blank and dsIn.polStatus <> 'All';
              If subset = *Off;
                 subset = *On;
                 Select;
                 When Upper(dsIn.polStatus) = 'CANCELLED';
                    strSql += 'EMcad1 > 0 and EMcad1 <= ' +
                     %editc(sysDate7:'X');
                 When Upper(dsIn.polStatus) = 'EXPIRED';
                    strSql += 'ELptdt <= ' + %editc(sysDate7:'X');
                 When Upper(dsIn.polStatus) = 'PENDING';
                    strSql += 'ELpfdt > ' + %editc(sysDate7:'X');
                 When Upper(dsIn.polStatus) = 'ACTIVE';
                    strSql += 'ELptdt > ' + %editc(sysDate7:'X');
                 When Upper(dsIn.polStatus) = 'PENDING CANCELLATION';
                    strSql += 'cxDate > ' + %editc(sysDate7:'X');
                 Endsl;
              Else;
                 Select;
                 When Upper(dsIn.polStatus) = 'CANCELLED';
                    strSql += ' and EMcad1 > 0 and EMcad1 <= ' +
                     %editc(sysDate7:'X');
                 When Upper(dsIn.polStatus) = 'EXPIRED';
                    strSql += ' and ELptdt <= ' + %editc(sysDate7:'X');
                 When Upper(dsIn.polStatus) = 'PENDING';
                    strSql += ' and ELpfdt > ' + %editc(sysDate7:'X');
                 When Upper(dsIn.polStatus) = 'ACTIVE';
                    strSql += ' and ELptdt > ' + %editc(sysDate7:'X');
                 When Upper(dsIn.polStatus) = 'PENDING CANCELLATION';
                    strSql += ' and cxDate > ' + %editc(sysDate7:'X');
                 Endsl;
              Endif;
           Endif;
           //Claim status filter
           If dsIn.claimStatus <> *blank and dsIn.claimStatus <> 'All';
              If subset = *Off;
                 subset = *On;
                 Select;
                 When Upper(dsIn.claimStatus) = 'OPEN';
                    strSql += 'CMstat = ''O''';
                 When Upper(dsIn.claimStatus) = 'CLOSED';
                    strSql += 'CMstat = ''C''';
                 Endsl;
              Else;
                 Select;
                 When Upper(dsIn.claimStatus) = 'OPEN';
                    strSql += ' and CMstat = ''O''';
                 When Upper(dsIn.claimStatus) = 'CLOSED';
                    strSql += ' and CMstat = ''C''';
                 Endsl;
              Endif;
           Endif;
        Endif;

        //Order by
        strSql += ' Order by ';
        Select;
        When dsIn.sort = 'Agency/Insured';
            strSql += 'AANAME, EMNAME';
        When dsIn.sort = 'Office/Agency';
            strSql += 'OFNAME, AANAME';
        Other;
              strSql += 'AANAME';
        Endsl;
       Endsr;
       //=================================================================
       // Finish The Report
       //=================================================================
       Begsr FinishReport;
          //endReport = *On;
          If count > 0;
             Select;
             When agtInsuredSort = *On;
                ExSr WriteInsuredTotal;
                    json += '],';
                ExSr WriteAgentTotal;
             When offAgencySort = *On;
                ExSr WriteAgentTotal;
                    json += '],';
                ExSr WriteOfficeTotal;
             Other;
                ExSr WriteAgentTotal;
             EndSl;
          EndIf;
          ExSr WriteRptTotals;
       Endsr;
       //=================================================================
       // Break parms out from inParms
       //=================================================================
       Begsr GetParms;
        // Load the json tree
        inJson = inParms;
        node0 = yajl_string_load_tree(inJson:strErrorMsg);
        If strErrorMsg <> *blanks;
           AddErrorMessageJson('JSON filter can''t be read.':ioRtnCode);
           Leavesr;
        Endif;
        Reset i0;
        json = json + ' "requestParms" : {';
        Dow YAJL_object_loop(node0:i0:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'group';
              Exsr GetParmsGroup;
           When key = 'office';
              Exsr GetParmsOffice;
           When key = 'agent';
              Exsr GetParmsAgency;
           When key = 'policy';
              Exsr GetParmsEmployer;
           When key = 'policyStatus';
              dsIn.polStatus = YAJL_get_string(val);
           When key = 'claimStatus';
              dsIn.claimStatus = YAJL_get_string(val);
           When key = 'injuryDate';
              Exsr GetParmsInjDtRange;
           When key = 'policyEffectiveDateRange';
              Exsr GetParmsEffDtRange;
           When key = 'sort';
              dsIn.sort = YAJL_get_string(val);
           Endsl;
        Enddo;
        json = json + '}, ';
        //json = json + '{';
        // Free up the space by removing the tree
        yajl_tree_free(node0);
       Endsr;
       //=================================================================
       // Break parms out from inParms - Agency
       //=================================================================
       Begsr GetParmsAgency;
        json = %scanrpl('""':'{':json:%len(json)-1);
        node1 = YAJL_object_find(node0:key);
        Reset i1;
        Dow YAJL_object_loop(node1:i1:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'fein';
              dsIn.agency.fein = YAJL_get_string(val);
           When key = 'mod';
              dsIn.agency.mod = YAJL_get_string(val);
           When key = 'name';
              dsIn.agency.name = YAJL_get_string(val);
           Endsl;
        Enddo;
        json += '}';
       Endsr;
       //=================================================================
       // Break parms out from inParms - Policy Effective Date Range
       //=================================================================
       Begsr GetParmsEffDtRange;
        json = %scanrpl('""':'{':json:%len(json)-1);
        node1 = YAJL_object_find(node0:key);
        Reset i1;
        Dow YAJL_object_loop(node1:i1:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'from';
              dsIn.effDtRange.from.dt8 = YAJL_get_string(val);
              Monitor;
                 dsIn.effDtRange.from.dt7 =
                  DtCYMD(%int(dsIn.effDtRange.from.dt8));
              On-error;
              Endmon;
           When key = 'to';
              dsIn.effDtRange.to.dt8 = YAJL_get_string(val);
              Monitor;
                 dsIn.effDtRange.to.dt7 =
                  DtCYMD(%int(dsIn.effDtRange.to.dt8));
              On-error;
              Endmon;
           Endsl;
        Enddo;
        json = json + '}';
       Endsr;
       //=================================================================
       // Break parms out from inParms - Employer
       //=================================================================
       Begsr GetParmsEmployer;
        json = %scanrpl('""':'{':json:%len(json)-1);
        node1 = YAJL_object_find(node0:key);
        Reset i1;
        Dow YAJL_object_loop(node1:i1:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'fein';
              dsIn.employer.fein = YAJL_get_string(val);
           When key = 'name';
              dsIn.employer.name = YAJL_get_string(val);
           Endsl;
        Enddo;
        json += '}';
       Endsr;
       //=================================================================
       // Break parms out from inParms - Group
       //=================================================================
       Begsr GetParmsGroup;
        json = %scanrpl('""':'{':json:%len(json)-1);
        node1 = YAJL_object_find(node0:key);
        Reset i1;
        Dow YAJL_object_loop(node1:i1:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'number';
              dsIn.group.number = YAJL_get_string(val);
           When key = 'name';
              dsIn.group.name = YAJL_get_string(val);
           Endsl;
        Enddo;
        json = json + '}';
       Endsr;
       //=================================================================
       // Break parms out from inParms - Injury Date Range
       //=================================================================
       Begsr GetParmsInjDtRange;
        json = %scanrpl('""':'{':json:%len(json)-1);
        node1 = YAJL_object_find(node0:key);
        Reset i1;
        Dow YAJL_object_loop(node1:i1:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'from';
              dsIn.injDtRange.from.dt8 = YAJL_get_string(val);
              Monitor;
                 dsIn.injDtRange.from.dt7 =
                  DtCYMD(%int(dsIn.injDtRange.from.dt8));
              On-error;
              Endmon;
           When key = 'to';
              dsIn.injDtRange.to.dt8 = YAJL_get_string(val);
              Monitor;
                 dsIn.injDtRange.to.dt7 =
                  DtCYMD(%int(dsIn.injDtRange.to.dt8));
              On-error;
              Endmon;
           Endsl;
        Enddo;
        json = json + '}';
       Endsr;
       //=================================================================
       // Break parms out from inParms - Office
       //=================================================================
       Begsr GetParmsOffice;
        json = %scanrpl('""':'{':json:%len(json)-1);
        node1 = YAJL_object_find(node0:key);
        Reset i1;
        Dow YAJL_object_loop(node1:i1:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'number';
              dsIn.office.number = YAJL_get_string(val);
           When key = 'name';
              dsIn.office.name = YAJL_get_string(val);
           Endsl;
        Enddo;
        json = json + '}';
       Endsr;
       //=================================================================
       // Load Array
       //=================================================================
       Begsr LoadArraysCM;
        // AR array
        aryAR( 1) = dsLossSumRpt.CMAR01;
        aryAR( 2) = dsLossSumRpt.CMAR02;
        aryAR( 3) = dsLossSumRpt.CMAR03;
        aryAR( 4) = dsLossSumRpt.CMAR04;
        aryAR( 5) = dsLossSumRpt.CMAR05;
        aryAR( 6) = dsLossSumRpt.CMAR06;
        aryAR( 7) = dsLossSumRpt.CMAR07;
        aryAR( 8) = dsLossSumRpt.CMAR08;
        aryAR( 9) = dsLossSumRpt.CMAR09;
        aryAR(10) = dsLossSumRpt.CMAR10;
        // RR array
        aryRR( 1) = dsLossSumRpt.CMRR01;
        aryRR( 2) = dsLossSumRpt.CMRR02;
        aryRR( 3) = dsLossSumRpt.CMRR03;
        aryRR( 4) = dsLossSumRpt.CMRR04;
        aryRR( 5) = dsLossSumRpt.CMRR05;
        aryRR( 6) = dsLossSumRpt.CMRR06;
        aryRR( 7) = dsLossSumRpt.CMRR07;
        aryRR( 8) = dsLossSumRpt.CMRR08;
        aryRR( 9) = dsLossSumRpt.CMRR09;
        aryRR(10) = dsLossSumRpt.CMRR10;
        // RR array
        aryRT( 1) = dsLossSumRpt.CMRT01;
        aryRT( 2) = dsLossSumRpt.CMRT02;
        aryRT( 3) = dsLossSumRpt.CMRT03;
        aryRT( 4) = dsLossSumRpt.CMRT04;
        aryRT( 5) = dsLossSumRpt.CMRT05;
        aryRT( 6) = dsLossSumRpt.CMRT06;
        aryRT( 7) = dsLossSumRpt.CMRT07;
        aryRT( 8) = dsLossSumRpt.CMRT08;
        aryRT( 9) = dsLossSumRpt.CMRT09;
        aryRT(10) = dsLossSumRpt.CMRT10;
       Endsr;
       //=================================================================
       //  Read through SQL recrods
       //=================================================================
       Begsr ProcessSQL;
        If ioRtnCode = 'Error';
           Leavesr;
        Endif;

        //Close the SQL cursor
        Exec SQL close cursorLossSummary;

        //Prepare the SQL statement
        Exec SQL prepare sqlLossSummary from :strSql;

        //Declare the SQL cursor
        Exec SQL declare cursorLossSummary cursor for sqlLossSummary;

        //Run SQL
        Exec SQL open cursorLossSummary;

        Reset dsLossSumRpt;
        //Get first record if there is one
        Exec SQL fetch next from cursorLossSummary into :dsLossSumRpt;

       //Check to see if any records were fetched
        If sqlSTT <> '00000';
           ExSr FinishReport;
           moreRows = *off;
           Exec SQL close cursorLossSummary;
        EndIf;

        //If row was fetched then process
        DoW moreRows = *On;
           // Did anything get picked up
           count += 1;
           //Build Report Record
           Exsr BldReportRcd;
           //Fetch next record
           Exec SQL fetch next from cursorLossSummary into :dsLossSumRpt;
           //check to see if there was another record fetched.
           If sqlSTT <> '00000';
              ExSr FinishReport;
              moreRows = *off;
              Exec SQL close cursorLossSummary;
           EndIf;
        Enddo;
       Endsr;
       //=================================================================
       // Sum Agent Information
       //=================================================================
       Begsr SumAmounts;
        //Load CMAR, CMRR and CMRT arrays
        Exsr  LoadArraysCM;
        //Paid To date and Outstanding Reserves & Total Incurred
        For i2=1 to #ofFin;
           strKey = aryRT(i2);
           Exec sql
           select RTtype into :strType
           from WTRTP
           where RTco# = :dsLossSumRpt.CMco# and RTkey = :strKey
           limit 1;
           If IsValidFetch(sqlstt);
              If agentSort = *On or agtInsuredSort = *On or offAgencySort = *On;
                 Select;
                 When strType = 'C';
                    decPtdIndAgt += aryAR(i2);
                    decOsrIndAgt += aryRR(i2);
                    decTinIndAgt += aryAR(i2) + aryRR(i2);
                    decPtdTotAgt += aryAR(i2);
                    decOsrTotAgt += aryRR(i2);
                    decTinTotAgt += aryAR(i2) + aryRR(i2);
                 When strType = 'M';
                    decPtdMedAgt += aryAR(i2);
                    decOsrMedAgt += aryRR(i2);
                    decTinMedAgt += aryAR(i2) + aryRR(i2);
                    decPtdTotAgt += aryAR(i2);
                    decOsrTotAgt += aryRR(i2);
                    decTinTotAgt += aryAR(i2) + aryRR(i2);
                 When strType = 'L';
                    decPtdLegAgt += aryAR(i2);
                    decOsrLegAgt += aryRR(i2);
                    decTinLegAgt += aryAR(i2) + aryRR(i2);
                    decPtdTotAgt += aryAR(i2);
                    decOsrTotAgt += aryRR(i2);
                    decTinTotAgt += aryAR(i2) + aryRR(i2);
                 When strType = 'E';
                    decPtdExpAgt += aryAR(i2);
                    decOsrExpAgt += aryRR(i2);
                    decTinExpAgt += aryAR(i2) + aryRR(i2);
                    decPtdTotAgt += aryAR(i2);
                    decOsrTotAgt += aryRR(i2);
                    decTinTotAgt += aryAR(i2) + aryRR(i2);
                 Endsl;
              Endif;
              If agtInsuredSort = *On;
                 Select;
                 When strType = 'C';
                    decPtdIndIns += aryAR(i2);
                    decOsrIndIns += aryRR(i2);
                    decTinIndIns += aryAR(i2) + aryRR(i2);
                    decPtdTotIns += aryAR(i2);
                    decOsrTotIns += aryRR(i2);
                    decTinTotIns += aryAR(i2) + aryRR(i2);
                 When strType = 'M';
                    decPtdMedIns += aryAR(i2);
                    decOsrMedIns += aryRR(i2);
                    decTinMedIns += aryAR(i2) + aryRR(i2);
                    decPtdTotIns += aryAR(i2);
                    decOsrTotIns += aryRR(i2);
                    decTinTotIns += aryAR(i2) + aryRR(i2);
                 When strType = 'L';
                    decPtdLegIns += aryAR(i2);
                    decOsrLegIns += aryRR(i2);
                    decTinLegIns += aryAR(i2) + aryRR(i2);
                    decPtdTotIns += aryAR(i2);
                    decOsrTotIns += aryRR(i2);
                    decTinTotIns += aryAR(i2) + aryRR(i2);
                 When strType = 'E';
                    decPtdExpIns += aryAR(i2);
                    decOsrExpIns += aryRR(i2);
                    decTinExpIns += aryAR(i2) + aryRR(i2);
                    decPtdTotIns += aryAR(i2);
                    decOsrTotIns += aryRR(i2);
                    decTinTotIns += aryAR(i2) + aryRR(i2);
                 Endsl;
              Endif;
              If offAgencySort = *On;
                 Select;
                 When strType = 'C';
                    decPtdIndOfc += aryAR(i2);
                    decOsrIndOfc += aryRR(i2);
                    decTinIndOfc += aryAR(i2) + aryRR(i2);
                    decPtdTotOfc += aryAR(i2);
                    decOsrTotOfc += aryRR(i2);
                    decTinTotOfc += aryAR(i2) + aryRR(i2);
                 When strType = 'M';
                    decPtdMedOfc += aryAR(i2);
                    decOsrMedOfc += aryRR(i2);
                    decTinMedOfc += aryAR(i2) + aryRR(i2);
                    decPtdTotOfc += aryAR(i2);
                    decOsrTotOfc += aryRR(i2);
                    decTinTotOfc += aryAR(i2) + aryRR(i2);
                 When strType = 'L';
                    decPtdLegOfc += aryAR(i2);
                    decOsrLegOfc += aryRR(i2);
                    decTinLegOfc += aryAR(i2) + aryRR(i2);
                    decPtdTotOfc += aryAR(i2);
                    decOsrTotOfc += aryRR(i2);
                    decTinTotOfc += aryAR(i2) + aryRR(i2);
                 When strType = 'E';
                    decPtdExpOfc += aryAR(i2);
                    decOsrExpOfc += aryRR(i2);
                    decTinExpOfc += aryAR(i2) + aryRR(i2);
                    decPtdTotOfc += aryAR(i2);
                    decOsrTotOfc += aryRR(i2);
                    decTinTotOfc += aryAR(i2) + aryRR(i2);
                 Endsl;
              Endif;
           EndIf;
        EndFor;

        //Payments in Period
        strSqlPaid = 'select PARETP, sum(PAAMT) from WDPAP ' +
         'where PAco# = ' + %char(dsLossSumRpt.CMco#) +
         ' and PAfnd = ' + %char(dsLossSumRpt.CMfnd) +
         ' and PAfyr = ' + %char(dsLossSumRpt.CMfyr) +
         ' and PAcase = ' + %char(dsLossSumRpt.CMcase) +
         ' and (PAtrDt between ' +
           %char(DateTo7(GetFirstDay(%date))) + ' and ' +
           %char(DateTo7(%date)) + ')' +
         ' group by PAretp';
        Exec sql close cursorPaid;
        Exec sql prepare sqlPaid From :strSqlPaid;
        Exec sql declare cursorPaid cursor for sqlPaid;
        Exec sql open cursorPaid;
        Dou isEofPaid;
           Exec sql fetch next from cursorPaid into :dsPaid;
           Select;
           When not IsValidFetch(sqlstt);
              isEofPaid = *on;
              Iter;
           Endsl;
           If agentSort = *On or agtInsuredSort = *On or offAgencySort = *On;
              Select;
              When dsPaid.recType= 'C';
                 decPtpIndAgt += dsPaid.amount;
                 decPtpTotAgt += dsPaid.amount;
              When dsPaid.recType= 'M';
                 decPtpMedAgt += dsPaid.amount;
                 decPtpTotAgt += dsPaid.amount;
              When dsPaid.recType= 'L';
                 decPtpLegAgt += dsPaid.amount;
                 decPtpTotAgt += dsPaid.amount;
              When dsPaid.recType= 'E';
                 decPtdExpAgt += dsPaid.amount;
                 decPtpTotAgt += dsPaid.amount;
              Endsl;
           Endif;
           If agtInsuredSort = *On;
              Select;
              When dsPaid.recType= 'C';
                 decPtpIndIns += dsPaid.amount;
                 decPtpTotIns += dsPaid.amount;
              When dsPaid.recType= 'M';
                 decPtpMedIns += dsPaid.amount;
                 decPtpTotIns += dsPaid.amount;
              When dsPaid.recType= 'L';
                 decPtpLegIns += dsPaid.amount;
                 decPtpTotIns += dsPaid.amount;
              When dsPaid.recType= 'E';
                 decPtdExpIns += dsPaid.amount;
                 decPtpTotIns += dsPaid.amount;
              Endsl;
           Endif;
           If offAgencySort = *On;
              Select;
              When dsPaid.recType= 'C';
                 decPtpIndOfc += dsPaid.amount;
                 decPtpTotOfc += dsPaid.amount;
              When dsPaid.recType= 'M';
                 decPtpMedOfc += dsPaid.amount;
                 decPtpTotOfc += dsPaid.amount;
              When dsPaid.recType= 'L';
                 decPtpLegOfc += dsPaid.amount;
                 decPtpTotOfc += dsPaid.amount;
              When dsPaid.recType= 'E';
                 decPtdExpOfc += dsPaid.amount;
                 decPtpTotOfc += dsPaid.amount;
              Endsl;
           Endif;
        Enddo;
       Endsr;
       //=================================================================
       // Validate incoming parameters
       //=================================================================
       Begsr ValidateParms;
       //Group Number
        If dsIn.group.number <> *Blank;
           //Check for valid numeric
           Monitor;
              decFnd = %int(dsIn.group.number);
           On-error;
              AddErrorMessageJson('Group# has character data.':ioRtnCode);
              ioRtnCode = 'Error';
           Endmon;
        Endif;

       //Office Number
        If dsIn.office.number <> *Blank;
           If ValidOffice(dsIn.office.number) = *Off;
              AddErrorMessageJson('Invalid office#.':ioRtnCode);
              ioRtnCode = 'Error';
           Endif;
        EndIf;

        // Validate agency information
        Select;
        When dsIn.agency.fein <> *blanks;
           Monitor;
              integer = %int(dsIn.agency.fein);
           On-Error;
              AddErrorMessageJson('Agent FEIN has character data.':ioRtnCode);
              ioRtnCode = 'Error';
           EndMon;
        Endsl;
        Select;
        When dsIn.agency.mod <> *blanks;
           Monitor;
              integer = %int(dsIn.agency.mod);
           On-Error;
              AddErrorMessageJson('Agent modifier has character data.'
               :ioRtnCode);
              ioRtnCode = 'Error';
           EndMon;
        Endsl;

        // Validate employer information
        If dsIn.employer.fein <> *blanks;
           Monitor;
              integer = %int(dsIn.employer.fein);
           On-Error;
              AddErrorMessageJson('Employer FEIN has character data.'
               :ioRtnCode);
              ioRtnCode = 'Error';
           EndMon;
        Endif;

        // Effective Date from & To must be valid numeric
        If dsIn.effDtRange.from.dt8 <> *Blank;
           Monitor;
           intRptDate8 =  %Int(dsIn.effDtRange.from.dt8);
           On-error;
              AddErrorMessageJson('Effective Date From is not numeric.');
              ioRtnCode = 'Error';
           Endmon;
           intRptDate8 = dtCheck(intRptDate8);
           If intRptDate8 = 0;
              AddErrorMessageJson('Invalid Effective date From.');
              ioRtnCode = 'Error';
           Else;
              dateEffDtFrom = %Date(intRptDate8:*Usa);
           Endif;
        Else;
           dateEffDtFrom = *Loval;
           dsIn.effDtRange.from.dt7 = 0;
        Endif;

        // Effective Date To must be valid numeric
        If dsIn.effDtRange.to.dt8 <> *Blank;
           Monitor;
           intRptDate8 =  %Int(dsIn.effDtRange.to.dt8);
           On-error;
              AddErrorMessageJson('Effective Date To is not numeric.');
              ioRtnCode = 'Error';
           Endmon;
           intRptDate8 = dtCheck(intRptDate8);
           If intRptDate8 = 0;
              AddErrorMessageJson('Invalid Effective date To.');
              ioRtnCode = 'Error';
           Else;
              dateEffDtTo = %Date(intRptDate8:*Usa);
           Endif;
        Else;
           dateEffDtTo = *Hival;
           dsIn.effDtRange.to.dt7 = 9999999;
        Endif;

        // Injury Date from & To must be valid numeric
        If dsIn.injDtRange.from.dt8 <> *Blank;
           Monitor;
           intRptDate8 =  %Int(dsIn.injDtRange.from.dt8);
           On-error;
              AddErrorMessageJson('Injury Date To is not numeric.');
              ioRtnCode = 'Error';
           Endmon;
           intRptDate8 = dtCheck(intRptDate8);
           If intRptDate8 = 0;
              AddErrorMessageJson('Invalid Injury date From.');
              ioRtnCode = 'Error';
           Else;
              dateInjDtFrom = %Date(intRptDate8:*Usa);
           Endif;
        Else;
           dateInjDtFrom = *Loval;
           dsIn.injDtRange.from.dt7 = 0;
        Endif;

        // Injury Date To must be valid numeric
        If dsIn.injDtRange.to.dt8 <> *Blank;
           Monitor;
           intRptDate8 =  %Int(dsIn.injDtRange.to.dt8);
           On-error;
              AddErrorMessageJson('Injury Date To is not numeric.');
              ioRtnCode = 'Error';
           Endmon;
           intRptDate8 = dtCheck(intRptDate8);
           If intRptDate8 = 0;
              AddErrorMessageJson('Invalid Injury date To.');
              ioRtnCode = 'Error';
           Else;
              dateInjDtTo = %Date(intRptDate8:*Usa);
           Endif;
        Else;
           dateInjDtTo = *Hival;
             dsIn.injDtRange.to.dt7 = 9999999;
        Endif;

        //Valid Date Range
        If dateEffDtFrom <> dateZero and dateEffDtTo <> dateZero and
         dateEffDtTo < dateEffDtFrom;
           AddErrorMessageJson('Effective Date Range: Not in order');
           ioRtnCode = 'Error';
        Endif;
        If dateInjDtFrom <> dateZero and dateInjDtTo <> dateZero and
         dateInjDtTo < dateInjDtFrom;
           AddErrorMessageJson('Injury Date Range: Not in order');
           ioRtnCode = 'Error';
        Endif;

        //Sort order
        If dsIn.sort <> 'Agency' And
           dsIn.sort <> 'Agency/Insured' And
           dsIn.sort <> 'Office/Agency';
           AddErrorMessageJson('Invalid Sort Order.');
           ioRtnCode = 'Error';
        Endif;

        Select;
        When dsIn.sort = 'Office/Agency';
           offAgencySort = *On;
        When dsIn.sort = 'Agency/Insured';
           agtInsuredSort = *On;
        When dsIn.sort = 'Agency';
           agentSort   = *On;
        EndSl;
       Endsr;
       //=================================================================
       // Write Agent Information
       //=================================================================
       Begsr WriteAgentInfo;
        insFirst = *On;
        If agentSort = *On and flgSortOrder = *On;
           json += '"sortOrder": "Agent"' + ',';
           flgSortOrder = *Off;
        Endif;
        If agtFirst = *On;
           json += '"agent":[';
           agtFirst = *Off;
        Else;
           json += ',';
        EndIf;
        strAgtDisplay = %Editc(dsLossSumRpt.AAagt#:'X') + ' - ' +
         dsLossSumRpt.AAname;

        json += '{ ' +
        BuildJsonNode('number':%EditC(dsLossSumRpt.AAagt#:'X')) + ',' +
        BuildJsonNode('name':%trim(dsLossSumRpt.AAname)) + ',' +
        BuildJsonNode('display':%trim(strAgtDisplay)) + ',' ;
       Endsr;
       //=================================================================
       // Write Agent Total Record
       //=================================================================
       Begsr WriteAgentTotal;
        json +=
        BuildJsonNode('agentPaymentThisPeriodIndemnity':
        %EditC(decPtpIndAgt:'3')) + ',' +
        BuildJsonNode('agentPaymentThisPeriodMedical':
        %EditC(decPtpMedAgt:'3')) + ',' +
        BuildJsonNode('agentPaymentThisPeriodExpense':
        %EditC(decPtpExpAgt:'3')) + ',' +
        BuildJsonNode('agentPaymentThisPeriodLegal':
        %EditC(decPtpLegAgt:'3')) + ',' +
        BuildJsonNode('agentPaymentThisPeriodTotal':
        %EditC(decPtpTotAgt:'3')) + ',' +
        BuildJsonNode('agentPaidToDateIndemnity':
        %EditC(decPtdIndAgt:'3')) + ',' +
        BuildJsonNode('agentPaidToDateMedical':
        %EditC(decPtdMedAgt:'3')) + ',' +
        BuildJsonNode('agentPaidToDateExpense':
        %EditC(decPtdExpAgt:'3')) + ',' +
        BuildJsonNode('agentPaidToDateLegal':
        %EditC(decPtdLegAgt:'3')) + ',' +
        BuildJsonNode('agentPaidToDateTotal':
        %EditC(decPtdTotAgt:'3')) + ',' +
        BuildJsonNode('agentOutstandingReservesIndemnity':
        %EditC(decOsrIndAgt:'3')) + ',' +
        BuildJsonNode('agentOutstandingReservesMedical':
        %EditC(decOsrMedAgt:'3')) + ',' +
        BuildJsonNode('agentOutstandingReservesExpense':
        %EditC(decOsrExpAgt:'3')) + ',' +
        BuildJsonNode('agentOutstandingReservesLegal':
        %EditC(decOsrLegAgt:'3')) + ',' +
        BuildJsonNode('agentOutstandingReservesTotal':
        %EditC(decOsrTotAgt:'3')) + ',' +
        BuildJsonNode('agentTotalIncurredIndemnity':
        %EditC(decTinIndAgt:'3')) + ',' +
        BuildJsonNode('agentTotalIncurredMedical':
        %EditC(decTinMedAgt:'3')) + ',' +
        BuildJsonNode('agentTotalIncurredExpense':
        %EditC(decTinExpAgt:'3')) + ',' +
        BuildJsonNode('agentTotalIncurredLegal':
        %EditC(decTinLegAgt:'3')) + ',' +
        BuildJsonNode('agentTotalIncurredTotal':
        %EditC(decTinTotAgt:'3')) +
        '}';

        If agentSort = *On;
           decPtpIndRpt += decPtpIndAgt;
           decPtdIndRPt += decPtdIndAgt;
           decOsrIndRPt += decOsrIndAgt;
           decTinIndRpt += decTinIndAgt;
           decPtpMedRpt += decPtpMedAgt;
           decPtdMedRpt += decPtdMedAgt;
           decOsrMedRpt += decOsrMedAgt;
           decTinMedRpt += decTinMedAgt;
           decPtpLegRpt += decPtpLegAgt;
           decPtdLegRpt += decPtdLegAgt;
           decOsrLegRpt += decOsrLegAgt;
           decTinLegRpt += decTinLegAgt;
           decPtpExpRpt += decPtpExpAgt;
           decPtdExpRpt += decPtdExpAgt;
           decOsrExpRpt += decOsrExpAgt;
           decTinExpRpt += decTinExpAgt;
           decPtpTotRpt += decPtpTotAgt;
           decPtdTotRpt += decPtdTotAgt;
           decOsrTotRpt += decOsrTotAgt;
           decTinTotRpt += decTinTotAgt;
        Endif;

        // Clear Totals;
        decPtpIndAgt = 0;
        decPtdIndAgt = 0;
        decOsrIndAgt = 0;
        decTinIndAgt = 0;
        decPtpMedAgt = 0;
        decPtdMedAgt = 0;
        decOsrMedAgt = 0;
        decTinMedAgt = 0;
        decPtpLegAgt = 0;
        decPtdLegAgt = 0;
        decOsrLegAgt = 0;
        decTinLegAgt = 0;
        decPtpExpAgt = 0;
        decPtdExpAgt = 0;
        decOsrExpAgt = 0;
        decTinExpAgt = 0;
        decPtpTotAgt = 0;
        decPtdTotAgt = 0;
        decOsrTotAgt = 0;
        decTinTotAgt = 0;
       Endsr;
       //=================================================================
       // Write Insured Information
       //=================================================================
       Begsr WriteInsuredInfo;
        If insFirst = *On;
           json += '"insured":[';
           insFirst = *Off;
        Else;
           json += ',';
        EndIf;
        strInsDisplay = dsLossSumRpt.EMname;

        json += '{ ' +
        BuildJsonNode('insuredName':%trim(strInsDisplay)) + ',' ;
       Endsr;
       //=================================================================
       // Write Insured Total Record
       //=================================================================
       Begsr WriteInsuredTotal;
        json +=
        BuildJsonNode('insuredPaymentThisPeriodIndemnity':
        %EditC(decPtpIndIns:'3')) + ',' +
        BuildJsonNode('insuredPaymentThisPeriodMedical':
        %EditC(decPtpMedIns:'3')) + ',' +
        BuildJsonNode('insuredPaymentThisPeriodExpense':
        %EditC(decPtpExpIns:'3')) + ',' +
        BuildJsonNode('insuredPaymentThisPeriodLegal':
        %EditC(decPtpLegIns:'3')) + ',' +
        BuildJsonNode('insuredPaymentThisPeriodTotal':
        %EditC(decPtpTotIns:'3')) + ',' +
        BuildJsonNode('insuredPaidToDateIndemnity':
        %EditC(decPtdIndIns:'3')) + ',' +
        BuildJsonNode('insuredPaidToDateMedical':
        %EditC(decPtdMedIns:'3')) + ',' +
        BuildJsonNode('insuredPaidToDateExpense':
        %EditC(decPtdExpIns:'3')) + ',' +
        BuildJsonNode('insuredPaidToDateLegal':
        %EditC(decPtdLegIns:'3')) + ',' +
        BuildJsonNode('insuredPaidToDateTotal':
        %EditC(decPtdTotIns:'3')) + ',' +
        BuildJsonNode('insuredOutstandingReservesIndemnity':
        %EditC(decOsrIndIns:'3')) + ',' +
        BuildJsonNode('insuredOutstandingReservesMedical':
        %EditC(decOsrMedIns:'3')) + ',' +
        BuildJsonNode('insuredOutstandingReservesExpense':
        %EditC(decOsrExpIns:'3')) + ',' +
        BuildJsonNode('insuredOutstandingReservesLegal':
        %EditC(decOsrLegIns:'3')) + ',' +
        BuildJsonNode('insuredOutstandingReservesTotal':
        %EditC(decOsrTotIns:'3')) + ',' +
        BuildJsonNode('insuredTotalIncurredIndemnity':
        %EditC(decTinIndIns:'3')) + ',' +
        BuildJsonNode('insuredTotalIncurredMedical':
        %EditC(decTinMedIns:'3')) + ',' +
        BuildJsonNode('insuredTotalIncurredExpense':
        %EditC(decTinExpIns:'3')) + ',' +
        BuildJsonNode('insuredTotalIncurredLegal':
        %EditC(decTinLegIns:'3')) + ',' +
        BuildJsonNode('insuredTotalIncurredTotal':
        %EditC(decTinTotIns:'3')) +
        '}';

        decPtpIndRpt += decPtpIndIns;
        decPtdIndRPt += decPtdIndIns;
        decOsrIndRPt += decOsrIndIns;
        decTinIndRpt += decTinIndIns;
        decPtpMedRpt += decPtpMedIns;
        decPtdMedRpt += decPtdMedIns;
        decOsrMedRpt += decOsrMedIns;
        decTinMedRpt += decTinMedIns;
        decPtpLegRpt += decPtpLegIns;
        decPtdLegRpt += decPtdLegIns;
        decOsrLegRpt += decOsrLegIns;
        decTinLegRpt += decTinLegIns;
        decPtpExpRpt += decPtpExpIns;
        decPtdExpRpt += decPtdExpIns;
        decOsrExpRpt += decOsrExpIns;
        decTinExpRpt += decTinExpIns;
        decPtpTotRpt += decPtpTotIns;
        decPtdTotRpt += decPtdTotIns;
        decOsrTotRpt += decOsrTotIns;
        decTinTotRpt += decTinTotIns;

        // Clear Totals;
        decPtpIndIns = 0;
        decPtdIndIns = 0;
        decOsrIndIns = 0;
        decTinIndIns = 0;
        decPtpMedIns = 0;
        decPtdMedIns = 0;
        decOsrMedIns = 0;
        decTinMedIns = 0;
        decPtpLegIns = 0;
        decPtdLegIns = 0;
        decOsrLegIns = 0;
        decTinLegIns = 0;
        decPtpExpIns = 0;
        decPtdExpIns = 0;
        decOsrExpIns = 0;
        decTinExpIns = 0;
        decPtpTotIns = 0;
        decPtdTotIns = 0;
        decOsrTotIns = 0;
        decTinTotIns = 0;
       Endsr;
       //=================================================================
       // Write Office Information Record
       //=================================================================
       Begsr WriteOfficeInfo;

        agtFirst = *On;
        If offAgencySort = *On and flgSortOrder = *On;
           json += '"sortOrder": "Office/Agency"' + ',';
           flgSortOrder = *Off;
        Endif;
        strOfcDisplay = dsLossSumRpt.CMoff# + ' - '  + dsLossSumRpt.OFname;
        If ofcFirst = *On;
           json += '"office":[';
           ofcFirst = *Off;
        Else;
           json += ',';
        EndIf;

        json += '{ ' +
        BuildJsonNode('number':%trim(dsLossSumRpt.CMoff#)) + ',' +
        BuildJsonNode('name':%trim(dsLossSumRpt.OFname)) + ',' +
        BuildJsonNode('display':%trim(strOfcDisplay)) + ',' ;
       Endsr;
       //=================================================================
       // Write Office Total Record
       //=================================================================
       Begsr WriteOfficeTotal;
        json +=
        BuildJsonNode('officePaymentThisPeriodIndemnity':
        %EditC(decPtpIndOfc:'3')) + ',' +
        BuildJsonNode('officePaymentThisPeriodMedical':
        %EditC(decPtpMedOfc:'3')) + ',' +
        BuildJsonNode('officePaymentThisPeriodExpense':
        %EditC(decPtpExpOfc:'3')) + ',' +
        BuildJsonNode('officePaymentThisPeriodLegal':
        %EditC(decPtpLegOfc:'3')) + ',' +
        BuildJsonNode('officePaymentThisPeriodTotal':
        %EditC(decPtpTotOfc:'3')) + ',' +
        BuildJsonNode('officePaidToDateIndemnity':
        %EditC(decPtdIndOfc:'3')) + ',' +
        BuildJsonNode('officePaidToDateMedical':
        %EditC(decPtdMedOfc:'3')) + ',' +
        BuildJsonNode('officePaidToDateExpense':
        %EditC(decPtdExpOfc:'3')) + ',' +
        BuildJsonNode('officePaidToDateLegal':
        %EditC(decPtdLegOfc:'3')) + ',' +
        BuildJsonNode('officePaidToDateTotal':
        %EditC(decPtdTotOfc:'3')) + ',' +
        BuildJsonNode('officeOutstandingReservesIndemnity':
        %EditC(decOsrIndOfc:'3')) + ',' +
        BuildJsonNode('officeOutstandingReservesMedical':
        %EditC(decOsrMedOfc:'3')) + ',' +
        BuildJsonNode('officeOutstandingReservesExpense':
        %EditC(decOsrExpOfc:'3')) + ',' +
        BuildJsonNode('officeOutstandingReservesLegal':
        %EditC(decOsrLegOfc:'3')) + ',' +
        BuildJsonNode('officeOutstandingReservesTotal':
        %EditC(decOsrTotOfc:'3')) + ',' +
        BuildJsonNode('officeTotalIncurredIndemnity':
        %EditC(decTinIndOfc:'3')) + ',' +
        BuildJsonNode('officeTotalIncurredMedical':
        %EditC(decTinMedOfc:'3')) + ',' +
        BuildJsonNode('officeTotalIncurredExpense':
        %EditC(decTinExpOfc:'3')) + ',' +
        BuildJsonNode('officeTotalIncurredLegal':
        %EditC(decTinLegOfc:'3')) + ',' +
        BuildJsonNode('officeTotalIncurredTotal':
        %EditC(decTinTotOfc:'3')) +
        '}';

        decPtpIndRpt += decPtpIndOfc;
        decPtdIndRPt += decPtdIndOfc;
        decOsrIndRPt += decOsrIndOfc;
        decTinIndRpt += decTinIndOfc;
        decPtpMedRpt += decPtpMedOfc;
        decPtdMedRpt += decPtdMedOfc;
        decOsrMedRpt += decOsrMedOfc;
        decTinMedRpt += decTinMedOfc;
        decPtpLegRpt += decPtpLegOfc;
        decPtdLegRpt += decPtdLegOfc;
        decOsrLegRpt += decOsrLegOfc;
        decTinLegRpt += decTinLegOfc;
        decPtpExpRpt += decPtpExpOfc;
        decPtdExpRpt += decPtdExpOfc;
        decOsrExpRpt += decOsrExpOfc;
        decTinExpRpt += decTinExpOfc;
        decPtpTotRpt += decPtpTotOfc;
        decPtdTotRpt += decPtdTotOfc;
        decOsrTotRpt += decOsrTotOfc;
        decTinTotRpt += decTinTotOfc;

        //Clear Totals
        decPtpIndOfc = 0;
        decPtdIndOfc = 0;
        decOsrIndOfc = 0;
        decTinIndOfc = 0;
        decPtpMedOfc = 0;
        decPtdMedOfc = 0;
        decOsrMedOfc = 0;
        decTinMedOfc = 0;
        decPtpLegOfc = 0;
        decPtdLegOfc = 0;
        decOsrLegOfc = 0;
        decTinLegOfc = 0;
        decPtpExpOfc = 0;
        decPtdExpOfc = 0;
        decOsrExpOfc = 0;
        decTinExpOfc = 0;
        decPtpTotOfc = 0;
        decPtdTotOfc = 0;
        decOsrTotOfc = 0;
        decTinTotOfc = 0;
       Endsr;
       //=================================================================
       // Write Report Totals
       //=================================================================
       Begsr WriteRptTotals;
        If count > 0;
           json += '], ';
        EndIf;
        json +=
        BuildJsonNode('reportPaymentThisPeriodIndemnity':
        %EditC(decPtpIndRpt:'3')) + ',' +
        BuildJsonNode('reportPaymentThisPeriodMedical':
        %EditC(decPtpMedRpt:'3')) + ',' +
        BuildJsonNode('reportPaymentThisPeriodExpense':
        %EditC(decPtpExpRpt:'3')) + ',' +
        BuildJsonNode('reportPaymentThisPeriodLegal':
        %EditC(decPtpLegRpt:'3')) + ',' +
        BuildJsonNode('reportPaymentThisPeriodTotal':
        %EditC(decPtpTotRpt:'3')) + ',' +
        BuildJsonNode('reportPaidToDateIndemnity':
        %EditC(decPtdIndRpt:'3')) + ',' +
        BuildJsonNode('reportPaidToDateMedical':
        %EditC(decPtdMedRpt:'3')) + ',' +
        BuildJsonNode('reportPaidToDateExpense':
        %EditC(decPtdExpRpt:'3')) + ',' +
        BuildJsonNode('reportPaidToDateLegal':
        %EditC(decPtdLegRpt:'3')) + ',' +
        BuildJsonNode('reportPaidToDateTotal':
        %EditC(decPtdTotRpt:'3')) + ',' +
        BuildJsonNode('reportOutstandingReservesIndemnity':
        %EditC(decOsrIndRpt:'3')) + ',' +
        BuildJsonNode('reportOutstandingReservesMedical':
        %EditC(decOsrMedRpt:'3')) + ',' +
        BuildJsonNode('reportOutstandingReservesExpense':
        %EditC(decOsrExpRpt:'3')) + ',' +
        BuildJsonNode('reportOutstandingReservesLegal':
        %EditC(decOsrLegRpt:'3')) + ',' +
        BuildJsonNode('reportOutstandingReservesTotal':
        %EditC(decOsrTotRpt:'3')) + ',' +
        BuildJsonNode('reportTotalIncurredIndemnity':
        %EditC(decTinIndRpt:'3')) + ',' +
        BuildJsonNode('reportTotalIncurredMedical':
        %EditC(decTinMedRpt:'3')) + ',' +
        BuildJsonNode('reportTotalIncurredExpense':
        %EditC(decTinExpRpt:'3')) + ',' +
        BuildJsonNode('reportTotalIncurredLegal':
        %EditC(decTinLegRpt:'3')) + ',' +
        BuildJsonNode('reportTotalIncurredTotal':
        %EditC(decTinTotRpt:'3'));
        //json += '}';
       Endsr;

     PLossSummaryReport...
     P                 e
