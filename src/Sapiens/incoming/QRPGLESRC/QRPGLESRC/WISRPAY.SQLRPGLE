     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     h DftActGrp(*No) ACTGRP('CONNECT')
     h Option(*NoDebugIO: *SrcStmt)
     h BndDir('WTBNDDIR':'YAJL') DecEdit('0.')
     h/Title WISRPAY - Portal S/R Payroll Requests
      *==========================================================
      * Description: Stores procedures to process various
      *              portal requests for policy JSON returns
      *==========================================================
      * Revisions:
      *
      * 03/24/25 C:65  Project: Phw#118649 (UBIC)
      *                In CalculateSRPremium if the period has already
      *                been submitted to live (WASRP) and there are no
      *                changes or additions, remove hold records. This
      *                will prevent data from being in both files when
      *                a user clicks through screens to print report.
      *
      * 02/21/25 C:62  Ticket: 118339(102)
      *                Don't include inactive payrolls
      *
      * 09/04/24 C:62  Ticket: 117568(140)
      *                Reestablish condition.
      *
      * 06/17/24 C:344 Project: WC2952
      *                Increase rate fields to zone 8.3
      *
      * 03/12/24 C:65  Project: Phw#115228 (FMIC)
      *                Move the coding to print the worksheet for
      *                ProcessSRPayments to be last just before
      *                emailing in order to ensure payment processing
      *                does not get interrupted by Scriptura failures.
      *
      * 10/20/23 C:79  Support: 140/110407
      *                Make day a flexible value for ListSRpayroll*
      *
      * 10/31/22 C:65  Project: WC2663
      *                Fix batch assignment when no billing or
      *                payment batch exists yet for day - it is
      *                not seeing just inserted records yet.
      *
      * 10/13/22 C:62  Project: WC2854
      *                Update reference number
      *
      * 10/06/22 C:62  Project: WC2663
      *                -Correct SQL for accessing A/R Batch file
      *                -Pass Correct File name
      *
      * 09/21/22 C:65  Support: 103692 (FMIC)
      *                Show net waiver charge on net rated calculations.
      *
      * 09/19/22 C:65  Support: 103692  (FMIC)
      *                Replace first subtotal for net rate with net premium.
      *
      * 09/08/22 C:65  Support: 103465  (UBIC)
      *                Return the payroll rate from employer payroll file
      *                for ListSRPayrollClasses instead of base rate since
      *                base rate does not take into considertion LCM/etc.
      *
      * 08/10/22 C:65  Support: 103051  (InHouse - PayGo)
      *                Update ProcessSRPayments web service so if it gets
      *                called a subsequent time for a reporting period then
      *                it won't fail finding WASRP_HLD records with a zero
      *                invoice#.  Instead it will save that invoice # off
      *                and pass it along to SRONLI so it will be reused.
      *                Change the Transfer3rdPartyBatch procedure to find
      *                the daily batched flagged for S/R Online instead of
      *                any 3rd party payment batch.
      *
      * 07/22/22 C:79  Project: WC2663I (Priss #102970)
      *                Pass * in EFT parm when getting bank account. This
      *                will find the account if MICR or EFT.
      *
      * 07/19/22 C:33  Support: 96520
      *                Update service edits for 3rd Party Payments.
      *
      * 06/16/22 C:062 Ticket: 102615(146)
      *                Correct date conversion.
      *
      * 04/11/22 C:340 Project: WC2663
      *                Invoice Cloud - Add Invoice Type ID
      *
      * 04/05/22 C:33  Support: 96520
      *                Edit services based on reporting period status.
      *
      * 03/01/22 C:65  Project: WC2819 (Phw#101215)
      *                Remove/alter unnecessary coding from this project.
      *                Net rate should be calculated each reporting
      *                period and regular manual premium should be passed
      *                to SRPREM API.
      *
      * 01/19/22 C:33  Project: WC2837
      *                Extend net rate conditional
      *
      * 02/14/22 C:33  Support: 96520
      *               -Do not save payrolls if period is 'Completed'
      *
      * 02/07/22 C:33  Support: 100785
      *                Notify Worksheets what year to process
      *
      * 10/05/21 C:65  Project: WC2819
      *                Use payroll net rates for net rate groups and
      *                return subset of premium information.
      *
      * 05/26/21 C:062 Project WC2663
      *                Invoice cloud processing
      *
      * 09/02/20 C:053 Project SY424
      *                Rmv requests for payments via invoice cloud
      *
      * 11/18/20 C:180 Project WC2757
      *                Recompiled only.
      *
      *==========================================================
     f*** begin add ***  03/24/25  ****************************************
     fwasrl12   if   e           k disk    rename(psr01:psr12)
     fwasrl13   if   e           k disk    rename(psr01:psr13)
     f*** end add *****  03/24/25  ****************************************
      *Entry
     dRequestPgm       pr                  ExtPgm('WISRPAY')
     d                              256
     d                               10
     d                                 a   Len(8500000) Varying
     d                                 a   Len(8500000) Varying
     d                               10
     dRequestPgm       pi
     d inRequest                    256
     d inUser                        10
     d inParms                         a   Len(8500000) Varying
     d outJSON                         a   Len(8500000) Varying
     d ioRtnCode                     10

      * SR Online
     d SROnline        pr                  ExtPgm('SRONLIC2')
     d  strCall                       1    const
     d  strCompany#                   3    const options(*nopass)
     d  strGroup#                     3    const options(*nopass)
     d  strEmployer#                  9    const options(*nopass)
     d  strDivision                   5    const options(*nopass)
     d  strYear                       3    const options(*nopass)
     d  strEffDate                    8    const options(*nopass)
     d  strMode                       1    const options(*nopass)
     d  strError                      1    const options(*nopass)
     d  strData                     100    options(*nopass)
     d  strInv#                      10    options(*nopass)

     d CalcSrPrem      pr                  ExtPgm('SRPREM')
     d  rrn                           9  0
     d  rpmy                          6  0
     d  todt                          8  0
     d  mapr                         11  0
     d  adld                         11  0
     d  payr                         11  0
     d  prem                         11  0
     d  tax                          11  0
     d  pprm                         11  0
     d  ptax                         11  0
     d  fact                        143
     d  prev                         11  2
     d  sron                          1

     d CashDissolve    pr                  ExtPgm('WA055R')
     d  mode                          1    Const
     d  rrn                           9  0

     dW7221C           pr                  ExtPgm('W7221C')
     d outCo#                         3a   const
     d outFnd                         3a   const
     d outRpYM                        6a   const
     d outQ                          10a   const
     d outMode                        1a   const
     d outEmp#                        9a   const
     d outDiv                         5a   const
     d outRunDt                       8a   const
     d outDue                         9a   const
     d outSort                        1a   const
     d outAgtFr                       5a   const
     d outAgtTo                       5a   const
     d outDtFr                        8a   const
     d outDtTo                        8a   const
     d outEmpFr                       9a   const
     d outEmpTo                       9a   const
     d outPrmM                        1a   const
     d outDarts                       1a   const
     d outPrinDp                      1a   const
     d outAll                         1a   const
     d outFormComp                    1a   const
     d outUser                       10a   const

     dProcessEmails    pr                  extpgm('WISRMAIL')
     d inRequest                    256    const
     d inUser                        10    const options(*nopass)
     d inParms                         a   len(2000000) varying options(*nopass)
     d outJSON                         a   len(2000000) varying options(*nopass)
     d outRtnCode                    10    options(*nopass)

     dconSRBranding    c                   'Pay as You Go'

     d i0              s             10i 0 inz
     d i1              s             10i 0 inz
     d i2              s             10i 0 inz
     d i3              s             10i 0 inz
     d i4              s             10i 0 inz
     d i5              s             10i 0 inz
     d i6              s             10i 0 inz
     D key             s             50a   varying
     d list            s                   Like(yajl_val)
     d node            s                   Like(yajl_val)
     d node0           s                   Like(yajl_val)
     d node1           s                   Like(yajl_val)
     d node2           s                   Like(yajl_val)
     d node3           s                   Like(yajl_val)
     d node4           s                   Like(yajl_val)
     d node5           s                   Like(yajl_val)
     d node6           s                   Like(yajl_val)
     d val             s                   Like(yajl_val)
     d psds          esds                  ExtName(szq1p)
     d maxSize         c                   999
     d array           s               a   Len(32767)
     d clientID        s              3a
     d comma           s              1a
     d decMsg#         s             10i 0
     d docNode         s                   Like(yajl_val)
     d errMsgArray     s            500    dim(maxSize) varying inz
     d inJson          s               a   Len(2000000) varying
     d intCo#          s              3  0
     d json            s               a   len(8500000) varying
     d ordinal         s              2  0
     d strAuthority    s             10a
     d strCmd          s            500a   varying
     d strCo#          s              3a
     d strErrorMsg     s            500a   Varying
     d strSqlCursor    s               a   Len(32000) Varying
     d sysDate         s               d
     d sysDate7        s              7  0
     d sysTime6        s              6  0

     d strCall         s              1
     d strCompany#     s              3
     d strGroup#       s              3
     d strEmployer#    s              9
     d strDivision     s              5
     d strYear         s              3
     d strEffDate      s              8
     d strMode         s              1
     d strError        s              1
     d strData         s            100
     d sqlStmtStr      s           1000
     d arCode          s              2
     d arType          s              1
     d rrnWDEAP_HLD    s              9  0

     d decCount        s             10i 0 inz(0)
     d maxCount        s             10i 0 inz(500)
     d maxLen          s             10i 0 inz(32000)

     d w@Today         S              8S 0
     d w@BnkAcc        S             20A
     d w@InvTypId      S             15S 0
     d
     d @@co            s              3
     d @@fnd           s              3
     d @@appl          s              2
     d @@srdt          s              8
     d @@call          s              1
     d @@hdcy          s              1
     d @@chkq          s             10
     d @@ftyp          s             10
     d @@efdt          s              7
     d @@cpyq          s             10
     d @@cktp          s             10
     d @@eftc          s              1
     d @@coid          s             20
     d @@rtcd          s              1
     d***  Beg Add ***  10/20/23  **********************
     d strDay          s              2a
     d***  End Add ***  10/20/23  **********************

     d GetClientID     pr                  ExtPgm('RTVCLTID')
     d  clientID                      3a

     DdsBillingInfo    ds                  qualified inz
     D period                         8  0
     D periodDisplay                  8  0
     D periodAlpha                   10    varying
     D periodType                    25a   varying
     D periodDtFrom                   7  0
     D periodDtTo                     7  0

     D dbfds           ds
     D  dbrrn                397    400b 0

     d W7221R          ds                  dtaara('QTEMP/W7221R') qualified inz
     d  path                        250a
     d  fyr                           3a   overlay(W7221R:1)

     d prmNode         ds                  Inz Qualified
     d  comma                         1
     d  operand                       1
     d  heading                      22
     d  amount                       22

     d x#RRN           s              9  0 inz(0)

     D dsWINOTIP1    e ds                  extname(WINOTIP1) qualified inz

     d svLDA           ds          1024    DtaAra(*LDA)
     d  xxDart                        1    Overlay(svlda:251)
     d  xxRMCD                        5    Overlay(svlda:252)
     d  xxCopy                        1    Overlay(svlda:257)
     d  xxPrint                       1    Overlay(svlda:258)
     d  xxPrinter                    10    Overlay(svlda:259)
     d  xxTray                       50    Overlay(svlda:269)
     d  xx#CopiesA                    3    Overlay(svlda:319)
     d  xxEmail                       1    Overlay(svlda:322)
     d  xxToEmail                   200    Overlay(svlda:323)
     d  xxSubject                   150    Overlay(svlda:523)
     d  xxBody                      350    Overlay(svlda:673)

      /Copy sPrAtrium
      /Copy sPrDate
      /Copy sPrFiles
      /Copy sPrJson
      /Copy sPrMessage
      /Copy sPrRunCmd
      /Copy sPrSecure
      /Copy sPrSql
      /Copy sPrString
      /copy sPrLetter
      /copy sPrXML
      /Copy wPrAgent
      /Copy wPrAR
      /Copy wPrClaims
      /Copy wPrEmplr
      /Copy wPrGroup
      /copy wPrRateSet
      /copy wPrSecure
      /copy wPrSR
      /copy wPrTable
      /copy wPrUsrPrms
      /Include yajl_h
      /copy wPrBank

      *==========================================================
      * Main
      *==========================================================
      /Free
       Exec Sql Set Option Commit = *None;
       Exsr PgmStart;

       CreateAliasSQL('WASRP_HLD':'WASRP':'HOLD':*on);
       CreateAliasSQL('WDEAP_HLD':'WDEAP':'HOLD':*on);

       Select;
       When inRequest = 'CalculateSRBalance';
          CalculateSRBalance();
       When inRequest = 'CreateSRBatches';
          CreateSRBatches();
       When inRequest = 'ListIncompleteSRPayments';
          ListIncompleteSRPayments();
       When inRequest = 'ListSRPayrollClasses';
          ListSRPayrollClasses();
       When inRequest = 'ListSRPayrollPeriods';
          ListSRPayrollPeriods();
       When inRequest = 'PrintSRWorksheet';
          PrintSRWorksheet();
       When inRequest = 'SaveSRPayrolls';
          SaveSRPayrolls();
       When inRequest = 'ProcessSRPayments';
          ProcessSRPayments();
       Other;
          Exsr BuildStub;
       EndSl;

       *Inlr = *On;

       //=================================================================
       // Build stub results
       //=================================================================
       Begsr BuildStub;
        BuildGiantJSON(outJSON:'stub':%trim(inRequest) + ' (' +
         %trim(q1Pgm) + ') successfully executed.');
       Endsr;

       //--------------------------------------------------------
       Begsr PgmStart;
       //--------------------------------------------------------
        strAuthority = ioRtnCode;
        Clear ioRtnCode;
        intCo# = UserCo#(inUser);
        strCo# = %Editc(intCo#:'X');
        GetClientID(clientID);
        sysDate  = %Date();
        sysDate7 = %Dec(sysDate:*Cymd);
        sysTime6 = %Dec(%Time():*hms);
       Endsr;
      /End-free

      *=========================================================
      * Builds premium line items for Online Calculation
      *=========================================================
     pBuildPremiumJson...
     p                 b
     dBuildPremiumJson...
     d                 pi           999    Varying

      /Free
       ordinal += 1;
       Return %Trim(prmNode.comma) + '{' +
        %Trim(BuildJsonNode('ordinal':%Editc(ordinal:'1'))) + ',' +
        %Trim(BuildJsonNode('heading':%Trim(prmNode.heading))) + ',' +
        %Trim(BuildJsonNode('operand':%Trim(prmNode.operand))) + ',' +
        %Trim(BuildJsonNode('amount':%Trim(prmNode.amount))) + '}';
      /End-Free

     pBuildPremiumJson...
     p                 e
      *=========================================================
      * CalcManPrem - Called from SaveSrPayrolls
      *=========================================================
      /Free
       Dcl-Proc CalcManPrem;
       Dcl-Pi *N;
        inWASRP LikeDs(dsWASRP);
       End-Pi;
       Dcl-Ds dsWASRP ExtName('WASRP') Qualified;
       End-Ds dsWASRP;
       Dcl-S chrPerCapitaFlag Char(1);
       Dcl-S chrRateCode Char(3);
       //**** Beg Chg **** 06/17/24 ****************************************
       //***Dcl-S decPayrollRate Packed(7:2);
       Dcl-S decPayrollRate Zoned(8:3);
       //**** End Chg **** 06/17/24 ****************************************
       Dcl-S intManClassPrm Packed(11:0);
       Exec Sql
         Select EParat
         Into :decPayrollRate
         From WMEPP
          Where EPco# = :inWASRP.SRco#
           And EPfnd  = :inWASRP.SRfnd
           And EPemp# = :inWASRP.SRemp#
           And EPdiv  = :inWASRP.SRdiv
           And EPfyr  = :inWASRP.SRfyr
           And EPclas = :inWASRP.SRclas
           And EPspec = :inWASRP.SRspec
           And EPext  = :inWASRP.SRext
           And EPefdt = :inWASRP.SRefdt;
       Exec Sql
        Select ELrtcd
        Into :chrRateCode
        From WDELP
         Where ELco#  = :inWASRP.SRco#
           And ELfnd  = :inWASRP.SRfnd
           And ELemp# = :inWASRP.SRemp#
           And ELDiv  = :inWASRP.SRdiv
           And ELfyr  = :inWASRP.SRfyr;
       Exec Sql
        Select PRpcf Into :chrPerCapitaFlag
        From WMPRP
        Where PRco# = :inWASRP.SRco#
         And PRrtcd = :chrRateCode
         And PRstat = 'A'
         And PRclas = :inWASRP.SRclas
         And PRefdt <= :inWASRP.SRefdt
        Order by PRco#, PRrtcd, PRclas, PRefdt Desc
        Limit 1;
       Select;
       When Not IsValidFetch(sqlStt);
          AddErrorMessageJson('SQL: No payroll record for ' +
           %Char(inWASRP.SRclas):
           ioRtnCode);
       When chrPerCapitaFlag = 'P';
          inWASRP.SRmanPrm = %Inth(inWASRP.SRamtp*decPayrollRate/100);
       When chrPerCapitaFlag = 'C';
          inWASRP.SRmanPrm = %Inth(inWASRP.SRamtp*decPayrollRate);
       Endsl;
       End-Proc;
      /End-Free
      *=========================================================
      * Request: CalculatSRBalance
      * Description: Calls SRPREM to return premium line items
      *=========================================================
     pCalculateSRBalance...
     p                 b

     d dsIn            ds                  Qualified Inz
     d  co#                           3a
     d  group                         3a
     d  emp#                          9a
     d  division                      5a
     d  year                          3a
     d  reportDate                    8a

     d                 ds
     d q1Fact                  1    143
     d  q1Lmpc                 1      5  3
     d  q1Tot1                 6     16  0
     d  q1Dscb                17     27  9
     d  q1Tot2                28     38  0
     d  q1ExPm                39     43  4
     d  q1Tot3                44     54  0
     d  q1Dsca                55     65  9
     d  q1Tot4                66     76  0
     d  q1Terr                77     87  0
     d  q1Waiv                88     98  0
     d  q1Rein                99    109  0
     d  q1Netf               110    121  9
     d  q1Netp               122    132  0
     d  q1NWav               133    143  0

     dprmHeadings      ds
     d hdgManPrem                    22    Inz('Manual Premium')
     d hdgIncLimit                   22    Inz('Increased Limits')
     d underLine1                    22    Inz('________________')
     d hdgTot1                       22    Inz('*  Subtotal')
5    d hdgDscBfMod                   22    Inz('Factor Before Mod')
     d underLine2                    22    Inz('________________')
     d hdgTot2                       22    Inz('*  Subtotal')
     d hdgExpMod                     22    Inz('Experience Modifier')
     d underLine3                    22    Inz('________________')
10   d hdgTot3                       22    Inz('*  Subtotal')
     d hdgDscAfMod                   22    Inz('Factor After Mod')
     d underLine4                    22    Inz('________________')
     d hdgTot4                       22    Inz('*  Subtotal')
     d hdgTerrorism                  22    Inz('Terrorism')
15   d hdgWaiver                     22    Inz('Waiver Surcharge')
     d hdgReinsuranc                 22    Inz('Reinsurance Surcharge')
     d underLine5                    22    Inz('________________')
     d hdgCalcPrem                   22    Inz('Calculated Premium')
     d hdgCalcTax                    22    Inz('Calculated Tax')
20   d underLine6                    22    Inz('________________')
     d hdgTotalBilled                22    Inz('Total Billed')
     d hdgPriorBilled                22    Inz('Prior Billed')
     d hdgPrevBalance                22    Inz('Previous Balance')
     d underLine7                    22    Inz('________________')
25   d hdgFinalTotal                 22    Inz('Total Period Payment')
     dprmHeading                     22    Dim(maxSize) Overlay(prmHeadings:1)

     d chrDayOfWeek    s              2a   Inz
     d chrPayrollSpec  s              1    Inz
     d chrPerCapitaFlag...
     d                 s              1a   Inz
     d chrRateCode     s              3a   Inz
     d chrRptFreq      s              1a   Inz
     d chrSrOn         s              1a   Inz('S')
     d dateInRptDate   s               d   Inz
     d datePrRptDate   s               d   Inz
     d decPayRollAmt   s             11  2 Inz
     d**** Beg Chg **** 06/17/24 *******************************************
     d***decPayrollRate  s              7  2 Inz
     d decPayrollRate  s              8S 3 Inz
     d**** End Chg **** 06/17/24 *******************************************
     d dec119          s             11  9 Inz
     d dec132          s             13  2 Inz
     d dec54           s              5  4 Inz
     d intAdLd         s             11  0 Inz
     d intCalcPrem     s             11  0 Inz
     d intCalcTax      s             11  0 Inz
     d intCurrDoWk     s              1  0 Inz
     d intDueDate8     s              8  0 Inz
     d intEmp          s              9  0 Inz
     d intDiv          s              5  0 Inz
     d intFyr          s              3  0 Inz
     d intGrp          s              3  0 Inz
     d intManClassPrm  s             11  0 Inz
     d intManPrem      s             11  0 Inz
     d intPayrollAmt   s             11  0 Inz
     d intPayrollClass...
     d                 s              5  0 Inz
     d intPayrollEfDt  s              7  0 Inz
     d intPrPrem       s             11  0 Inz
     d intPrTax        s             11  0 Inz
     d intPrvBal       s             11  2 Inz
     d intRptDate5     s              5  0 Inz
     d intRptDate6     s              6  0 Inz
     d intRptDate7     s              7  0 Inz
     d intRptDate8     s              8  0 Inz
     d intSchdDoWk     s              1  0 Inz
     d ix              s              2  0 Inz
     d maxSize         c                   25
     d rrnWDELP        s              9  0 Inz
     d strPayrollExt   s              5    Inz
     DisNetRate        s              1n   inz('0')
     d*** begin add ***  03/24/25  ****************************************
     d payrollChanges  s               n
     d*** end add *****  03/24/25  ****************************************

      /Free
       Exsr GetParms;
       Exsr ValidateParms;
       Exsr CalculateManPrem;
       Exsr CalculateSRPrem;
       Exsr BuildReturnJson;
       //*** begin add ***  03/24/25  ****************************************
       Exsr RemovePayroll;
       //*** end add *****  03/24/25  ****************************************

       //---------------------------------------------------
       Begsr GetParms;
       //---------------------------------------------------
        inJson = inParms;
        node0 = yajl_string_load_tree(inJson:strErrorMsg);
        If strErrorMsg = ' ';
           json = json + ' "requestParms" : {';
           Dow YAJL_object_loop(node0:i0:key:val);
              Select;
              When key = 'company';
                 dsIn.co# = YAJL_get_string(val);
              When key = 'group';
                 dsIn.group = YAJL_get_string(val);
              When key = 'employer';
                 dsIn.emp# = YAJL_get_string(val);
              When key = 'division';
                 dsIn.division = YAJL_get_string(val);
              When key = 'year';
                 dsIn.year = YAJL_get_string(val);
              When key = 'reportDate';
                 dsIn.reportDate = YAJL_get_string(val);
              Endsl;
           Enddo;
           Yajl_Tree_Free(node0);
        Else;
           AddErrorMessageJson('Request JSON ill formed.');
           ioRtnCode = 'Error';
        Endif;
        If strAuthority = '*View';
           AddErrorMessageJson('User, ' + %trim(inUser) +
            ', not authorized to process ' +
            'request (' + %trim(inRequest) + ').'
            :ioRtnCode);
        Endif;
       EndSr;
       //---------------------------------------------------
       Begsr ValidateParms;
       //---------------------------------------------------
       // Company # must be valid numeric value
        If ioRtnCode = 'Error';
           LeaveSr;
        EndIf;
        Monitor;
           intCo# = %Int(dsIn.co#);
        On-error;
           intCo# = UserCo#(inUser);
           If intCo# = 0;
              AddErrorMessageJson('Company value cannot be defaulted.');
              ioRtnCode = 'Error';
           Endif;
        Endmon;
       // Group # must be valid numeric value
        Monitor;
           intGrp = %Int(dsIn.group);
        On-error;
           AddErrorMessageJson('Group value is not numeric.');
           ioRtnCode = 'Error';
        Endmon;
        If intGrp <> *zero and not UseGroupInPortal(intCo#:intGrp:'S/R Online');
           AddErrorMessageJson('Group ' + %char(intGrp) +
            ' is not included for service requested ' +
            '(' + %trim(inRequest) + ').':ioRtnCode);
        Endif;
       // Employer # must be valid numeric value
        Monitor;
           intEmp = %Int(dsIn.emp#);
        On-error;
           AddErrorMessageJson('Employer value is not numeric.');
           ioRtnCode = 'Error';
        Endmon;
       // Division must be valid numeric value
        Monitor;
           intDiv = %Int(dsIn.division);
        On-error;
           AddErrorMessageJson('Division value is not numeric.');
           ioRtnCode = 'Error';
        Endmon;
       // Year must be a valid numeric value
        Monitor;
           intFyr = %Int(dsIn.year);
        On-error;
           AddErrorMessageJson('Year value is not numeric.');
           ioRtnCode = 'Error';
        Endmon;
       // Policy must be valid
        If ioRtnCode <> 'Error';
           If ValidEmpYr(intFyr:intEmp:intDiv:rrnWDELP:
                         intGrp:intCo#) = *Off;
              AddErrorMessageJson('Policy cannot be found.');
              ioRtnCode = 'Error';
           Else;
             // Get frequency and day of week to calculate
             //  the prior report date for monthly or weekly.
              Exec Sql
               Select BSfreq, BSdowk, ELrtcd
               Into :chrRptFreq,
                    :chrDayOfWeek,
                    :chrRateCode
               From WDELP Join WTBSP
               On ELco# = BSco# And ELbscd = BSkey
               Where ELco# = :intCo# And ELfnd = :intGrp
                And ELemp# = :intEmp And ELdiv = :intDiv
                And ELfyr  = :intFyr;
             // Report date must be valid and be for the previous
             //  report period.
              Monitor;
                 intRptDate8 =  %Int(dsIn.reportDate);
                 intRptDate8 = dtCheck(intRptDate8);
                 If intRptDate8 = 0;
                    AddErrorMessageJson('Valid report date is required.');
                    ioRtnCode = 'Error';
                 Else;
                    dateInRptDate = %Date(intRptDate8:*Usa);
                    Select;
                    When chrRptFreq = 'W';
                      intSchdDoWk = GetWeekDayNumber(chrDayOfWeek);
                      intCurrDoWk =
                       %Rem(%Diff(%Date():d'1800-01-05':*Days):7);
                      Select;
                      When intCurrDoWk <= intSchdDoWk;
                         datePrRptDate =
                          %Date()-%Days(intCurrDoWk-intSchdDoWk+7);
                      When intCurrDoWk >  intSchdDoWk;
                         datePrRptDate =
                          %Date()-%Days(intCurrDoWk-intSchdDoWk);
                      EndSl;
                      intDueDate8 = DateTo8(dateInRptDate + %days(7));
                    Other;
                       datePrRptDate = (%Date())-%Days(%Subdt(%Date():*Days));
                       intDueDate8 =
                        DateTo8(GetLastDay(dateInRptDate + %Days(1)));
                    Endsl;
                    If datePrRptDate <> dateInRptDate;
                       AddErrorMessageJson('Report date is not' +
                        ' for prior reporting period.');
                       ioRtnCode = 'Error';
                    Endif;
                 Endif;
              On-error;
                 AddErrorMessageJson('Valid report date is required.');
                 ioRtnCode = 'Error';
              Endmon;
           Endif;
        Endif;
       EndSr;
       //---------------------------------------------------
       Begsr CalculateManPrem;
       //---------------------------------------------------
        If ioRtnCode = 'Error';
           LeaveSr;
        Endif;
        isNetRate = IsUseNetRate(intCo#:intGrp:intEmp:intDiv:intFyr);
        intRptDate7 = DateTo7(datePrRptDate);
        intRptDate5 = intRptDate7/100;
        //*** begin add ***  03/24/25  ****************************************
        payrollChanges = *off;
        //*** end add *****  03/24/25  ****************************************
        Exec Sql
         Declare s1 Cursor For
          Select SRclas, SRspec, SRext, SRefdt, SRamtp, EParat
          From WASRP_HLD
           Join WMEPP
            On  SRco#  = EPco#
            And SRfnd  = EPfnd
            And SRemp# = EPemp#
            And SRdiv  = EPdiv
            And SRfyr  = EPfyr
            And SRclas = EPclas
            And SRspec = EPspec
            And SRext  = EPext
            And SRefdt = EPefdt
          Where SRco# = :intCo#
            And SRfnd = :intGrp
            And SRemp# = :intEmp
            And SRDiv = :intDiv
            And SRfyr = :intFyr
            And (SRtodt = :intRptDate7 Or SRrpmy = :intRptDate5);
        Exec Sql
         Open s1;
        Dou Not IsValidFetch(sqlStt);
           Exec Sql
            Fetch Next
            From s1
            Into :intPayrollClass,
                 :chrPayrollSpec,
                 :strPayrollExt,
                 :intPayrollEfDt,
                 :decPayrollAmt,
                 :decPayrollRate;
           Select;
           When sqlStt = '02000';
              Leave;
           When Not IsValidFetch(sqlStt);
              AddErrorMessageJson('SQL: Cursor Fetch failed for ' +
               %Char(intPayrollClass):
               ioRtnCode);
              Iter;
           Endsl;
        //*** begin add ***  03/24/25  ****************************************
           If chrRptFreq = 'W';
              Chain(n) (intCo#:intGrp:intFyr:intEmp:intDiv:
               intRptDate7:intPayrollClass:chrPayrollSpec:
               intPayrollEfdt:strPayrollExt) psr13;
           Else;
              Chain(n) (intCo#:intGrp:intFyr:intEmp:intDiv:
               intPayrollClass:chrPayrollSpec:intPayrollEfdt:
               strPayrollExt:intRptDate5) psr12;
           Endif;
           If not %found or SRAmtp <> decPayrollAmt;
              payrollChanges = *on;
           Endif;
        //*** end add *****  03/24/25  ****************************************
           Exec Sql
            Select PRpcf Into :chrPerCapitaFlag
            From WMPRP
            Where PRco# = :intCo#
             And PRrtcd = :chrRateCode
             And PRstat = 'A'
             And PRclas = :intPayrollClass
             And PRefdt <= :intRptDate7
            Order by PRco#, PRrtcd, PRclas, PRefdt Desc
            Limit 1;
           Select;
           When Not IsValidFetch(sqlStt);
              AddErrorMessageJson('SQL: No payroll record for ' +
               %Char(intPayrollClass):
               ioRtnCode);
              Iter;
           When chrPerCapitaFlag = 'P';
              intManClassPrm = %Inth(decPayrollAmt*decPayrollRate/100);
           When chrPerCapitaFlag = 'C';
              intManClassPrm = %Inth(decPayrollAmt*decPayrollRate);
           Endsl;
           Exec Sql
            Update WASRP_HLD
            Set SRManPrm = :intManClassPrm
            Where SRco# = :intCo#
             And SRfnd = :intGrp
             And SRemp# = :intEmp
             And SRDiv = :intDiv
             And SRfyr = :intFyr
             And SRclas = :intPayrollClass
             And SRspec = :chrPayrollSpec
             And SRext = :strPayrollExt
             And SRefdt = :intPayrollEfDt
             And (SRtodt = :intRptDate7 Or SRrpmy = :intRptDate5);
           Select;
           When Not IsValidFetch(sqlStt);
              AddErrorMessageJson('SQL: Update failed for ' +
               %Char(intPayrollClass):
               ioRtnCode);
              Iter;
           Other;
              intManPrem += intManClassPrm;
              intPayrollAmt += %Inth(decPayrollAmt);
           EndSl;
        Enddo;
        Exec Sql
         Close s1;
       EndSr;
       //---------------------------------------------------
       Begsr CalculateSRPrem;
       //---------------------------------------------------
        If ioRtnCode = 'Error';
           LeaveSr;
        Endif;
        intRptDate6 = Dt6MDYY(intRptDate5);
        CalcSrPrem(rrnWDELP:
                   intRptDate6:
                   intRptDate8:
                   intManPrem:
                   intAdLd:
                   intPayrollAmt:
                   intCalcPrem:
                   intCalcTax:
                   intPrPrem:
                   intPrTax:
                   q1Fact:
                   intPrvBal:
                   chrSrOn);
       EndSr;
       //---------------------------------------------------
       Begsr BuildReturnJson;
       //---------------------------------------------------
        json = '"requestParms":{';
        BuildGiantJSON(json:'company':dsIn.co#);
        BuildGiantJSON(json:'group':dsIn.group);
        BuildGiantJSON(json:'employer':dsIn.emp#);
        BuildGiantJSON(json:'division':dsIn.division);
        BuildGiantJSON(json:'year':dsIn.year);
        BuildGiantJSON(json:'reportDate':dsIn.reportDate);
        json += '},';
        If ioRtnCode <> 'Error';
           json += '"paymentType":' + '"' +
            %Trim(GetPaymentType(intCo#:intGrp:intEmp:intDiv)) + '",';
           json += BuildJsonNode('previousBalance':%editc(intPrvBal:'N')) + ',';
           dec132 = intCalcPrem+intCalcTax-intPrPrem-intPrTax+intPrvBal;
           json += BuildJsonNode('amountDue':%editc(dec132:'N')) + ',';
           json += BuildJsonNode('paymentDueDate':%char(intDueDate8):'date')
            + ',';
           json += BuildJsonNode('paymentPlan':GetBillSch(rrnWDELP)) + ',';
           json += '"premiumLineItems":[';
           For ix = 1 to maxSize;
              Reset prmNode;
              If (ix > 1 and not isNetRate) or (ix > 13 and isNetRate);
                 prmNode.comma = ',';
              Endif;
             // Premium Breakdown
             // Underlines removed 10/16/19 per SS
              Select;
              When ix < 13 and isNetRate;
                 Iter;
              When ix = 1; // Manual Premium
                 prmNode.heading = prmHeading(ix);
                 dec132 = intManPrem;
                 prmNode.amount = %EditC(dec132:'1');
              When ix = 2; // Inc Limits
                 prmNode.heading = prmHeading(ix);
                 dec132 = %Dech(intManPrem*Q1Lmpc/100:13:0);
                 prmNode.amount = %EditC(dec132:'1');
                 prmNode.operand = '+';
              When ix = 3; // Underline
                 Iter;
              When ix = 4; // First subtotal
                 prmNode.heading = prmHeading(ix);
                 dec132 = Q1tot1;
                 prmNode.amount = %EditC(dec132:'1');
              When ix = 5; // Factor Before Mod
                 prmNode.heading = prmHeading(ix);
                 dec119 = Q1dscb;
                 If dec119 = 0;
                    dec119 = 1;
                 EndIf;
                 prmNode.amount = %EditC(dec119:'1');
                 prmNode.operand = 'x';
              When ix = 6; // Underline
                 Iter;
              When ix = 7; // 2nd subtotal
                 prmNode.heading = prmHeading(ix);
                 dec132 = Q1tot2;
                 prmNode.amount = %EditC(dec132:'1');
              When ix = 8; // Experience Mod
                 prmNode.heading = prmHeading(ix);
                 dec54 = Q1ExPm;
                 If dec54 = 0;
                    dec54 = 1;
                 EndIf;
                 prmNode.amount = %EditC(dec54:'1');
                 prmNode.operand = 'x';
              When ix = 9; // Underline
                 Iter;
              When ix = 10; // 3rd subtotal
                 prmNode.heading = prmHeading(ix);
                 dec132 = Q1tot3;
                 prmNode.amount = %EditC(dec132:'1');
              When ix = 11; // Factor After Mod
                 prmNode.heading = prmHeading(ix);
                 dec119 = Q1dsca;
                 If dec119 = 0;
                    dec119 = 1;
                 EndIf;
                 prmNode.amount = %EditC(dec119:'1');
                 prmNode.operand = 'x';
              When ix = 12; // Underline
                 Iter;
              When ix = 13; // 4th subtotal
                 prmNode.heading = prmHeading(ix);
                 dec132 = Q1tot4;
                 If isNetRate;
                    dec132 = q1netp;
                 Endif;
                 prmNode.amount = %EditC(dec132:'1');
              When ix = 14; // Terrorism
                 prmNode.heading = prmHeading(ix);
                 dec132 = Q1terr;
                 prmNode.amount = %EditC(dec132:'1');
                 prmNode.operand = '+';
              When ix = 15; // Waiver Surcharge
                 prmNode.heading = prmHeading(ix);
                 dec132 = Q1waiv;
                 If isNetRate;
                    dec132 = q1nwav;
                 Endif;
                 prmNode.amount = %EditC(dec132:'1');
                 prmNode.operand = '+';
              When ix = 16; // Reinsurance
                 prmNode.heading = prmHeading(ix);
                 dec132 = Q1rein;
                 prmNode.amount = %EditC(dec132:'1');
                 prmNode.operand = '+';
              When ix = 17; // Underline
                 Iter;
              When ix = 18; // Calculated Premium or Total Payment
                 If intCalcTax = 0 And intPrPrem = 0 And
                  intPrvBal = 0;
                    prmNode.heading = 'Total Period Payment';
                 Else;
                    prmNode.heading = prmHeading(ix);
                 Endif;
                 dec132 = intCalcPrem;
                 prmNode.amount = %EditC(dec132:'1');
              Endsl;
             // Add tax and total lines if needed.
              Select;
              When ix = 19; // Calculated Tax
                 prmNode.heading = prmHeading(ix);
                 dec132 = intCalcTax;
                 prmNode.amount = %EditC(dec132:'1');
                 prmNode.operand = '+';
              When ix = 20; // Underline
                 Iter;
              When ix = 21; // Total Billed
                 If intPrPrem <> 0 Or intPrvBal <> 0;
                    prmNode.heading = prmHeading(ix);
                 Else;
                    prmNode.heading = 'Total Period Payment';
                 Endif;
                 dec132 = intCalcPrem + intCalcTax;
                 prmNode.amount = %EditC(dec132:'1');
              Endsl;
             // Add prior billed and previous balance if needed
              Select;
              When ix = 22; // Prior billed
                 prmNode.heading = prmHeading(ix);
                 dec132 = intPrPrem + intPrTax;
                 prmNode.amount = %EditC(dec132:'1');
                 prmNode.operand = '-';
              When ix = 23; // Previous Balance
                 prmNode.heading = prmHeading(ix);
                 dec132 = intPrvBal;
                 prmNode.amount = %EditC(dec132:'N');
                 If dec132 >= 0;
                    prmNode.operand = '+';
                 Else;
                    prmNode.operand = '-';
                 Endif;
              When ix = 24; // Underline
                 Iter;
              When ix = 25; // Total Period Payment
                 prmNode.heading = prmHeading(ix);
                 dec132 = intCalcPrem + intCalcTax -
                 intPrPrem - intPrTax + intPrvBal;
                 If dec132 < 0;
                    prmNode.heading = 'Total Balance';
                 Endif;
                 prmNode.amount = %EditC(dec132:'N');
              EndSl;
              json += BuildPremiumJson();
           Endfor;
           json += ']';
        Else;
           json += ProcessErrorMessagesJson();
        Endif;
        outJson = json;
       EndSr;
       //*** begin add ***  03/24/25  ****************************************
       //---------------------------------------------------
       Begsr RemovePayroll;
       //---------------------------------------------------
       //* Remove payroll hold records only if no changes were found.
        If ioRtnCode = 'Error' or payrollChanges = *on;
           LeaveSr;
        Endif;

        Exec Sql
         Delete from WASRP_HLD
            Where SRco# = :intCo#
             And SRfnd = :intGrp
             And SRemp# = :intEmp
             And SRDiv = :intDiv
             And SRfyr = :intFyr
             And (SRtodt = :intRptDate7 Or SRrpmy = :intRptDate5);

       Endsr;
       //*** end add *****  03/24/25  ****************************************
       //---------------------------------------------------
      /End-Free
     pCalculateSRBalance...
     p                 e
      /Free
       //=========================================================
       // Changes Lock Status in WASRP_HLD
       //=========================================================
       Dcl-Proc ChangeSrLockStatus;
       Dcl-Pi *N;
        inCo#  Char(3);
        inFnd  Char(3);
        inEmp# Char(9);
        inDiv  Char(5);
        inFyr  Char(3);
        inRPMY Char(5);
        inToDt Char(7);
        inStatus Char(1);
       End-Pi;
       Exec Sql
        Update WASRP_HLD
        Set SRstat = :inStatus
        Where SRco#  = :inCo#
          And SRfnd  = :inFnd
          And SRemp# = :inEmp#
          And SRdiv  = :inDiv
          And SRfyr  = :inFyr
          And SRrpmy = :inRPMY
          And SRtodt = :inToDt;
       End-Proc ChangeSrLockStatus;
      /End-free
      *=========================================================
      * Request: CreateSRBatches
      * Description: Creates SR Batches when Pay button is hit
      *=========================================================
     pCreateSRBatches...
     p                 b

     D                 DS                  Inz
     D  data2                  1    100
     D  type1                  1      2
     D  amount1                3     13  2
     D  date1                 14     21  0
     D  type2                 22     23
     D  amount2               24     34  2
     D  date2                 35     42  0
     D  type3                 43     44
     D  amount3               45     55  2
     D  date3                 56     63  0
     D  type4                 64     65
     D  amount4               66     76  2
     D  date4                 77     84  0

     d i               s             10i 0 inz
     d x               s             10i 0 inz
     d errMsg          s          99999a   Varying Inz(' ')
     d rrnWDELP        s              9  0
     d batchCount      s              5  0 Inz(0)
     d amount          s             11  2
     d date            s              8  0
     d decimalError    s               n

     d intCo#          s              3  0
     d intGrp#         s              3  0
     d intEmp#         s              9  0
     d intDiv          s              5  0
     d intYear         s              3  0
     d intRpmy         s              6  0
     d intTodt         s              7  0
     d intInvoice#     s             10  0

     d strSql          s           5000a   varying inz
     d strSqlOrder     s            500a   varying inz
     d strSqlSelect    s           1000a   varying inz
     d strSqlWhere     s           5000a   varying inz

     d strCall         s              1
     d strMode         s              1    Inz('2')
     d strError        s              1    Inz(' ')
     d strData         s            100
     d strPayType      s              1

     d dsIn            ds                  Qualified Inz
     d  status                       25a
     d  co#                           3a
     d  group                         3a
     d  emp#                          9a
     d  division                      5a
     d  year                          3a
     d  reportDate                    8a
     d  invoiceNum                   10a

     d batchArray      ds                  Qualified Dim(9999)
     d  code                          2a
     d  amount                       12a
     d  effDate                       8a

      /Free
       Exsr GetParms;
       Exsr SaveBatches;
       json += ProcessErrorMessagesJson('*PRE');
       json = %scanrpl(',':'':json:1:1);
       outJSON = json;
       Return;
       //========================================================
       // Break parms out of JSON
       //========================================================
       Begsr GetParms;

          inJson = inParms;
          Reset i0;

          node0 = yajl_string_load_tree(inJson:strErrorMsg);
          If strErrorMsg = ' ';
            // CreateSRBatches is called from ProcessSRPayments.
            // No need to repeat the request parms except for
            // srBatches info.
             Dow YAJL_object_loop(node0:i0:key:val);
                Select;
                When key = 'status';
                   dsIn.status = YAJL_get_string(val);
                When key = 'company';
                   dsIn.co# = YAJL_get_string(val);
                When key = 'group';
                   dsIn.group = YAJL_get_string(val);
                When key = 'employer';
                   dsIn.emp# = YAJL_get_string(val);
                When key = 'division';
                   dsIn.division = YAJL_get_string(val);
                When key = 'year';
                   dsIn.year = YAJL_get_string(val);
                When key = 'reportDate';
                   dsIn.reportDate = YAJL_get_string(val);
                When key = 'invoice#';
                   dsIn.invoiceNum = YAJL_get_string(val);
                When key = 'srBatches';
                   ExSr GetParmsBatches;
                Endsl;
             Enddo;
             Yajl_Tree_Free(node0);
             Exsr ValidateData;
          Else;
             AddErrorMessageJson('JSON filter can''t be read.':ioRtnCode);
          Endif;
          If strAuthority = '*View';
             AddErrorMessageJson('User, ' + %trim(inUser) +
              ', not authorized to process ' +
              'request (' + %trim(inRequest) + ').'
              :ioRtnCode);
          Endif;

       Endsr;

       //========================================================
       // Break SR batches parms out of JSON
       //========================================================
       Begsr GetParmsBatches;

          node1 = YAJL_object_find(node0:key);
          json = json + ',"srBatches":[{';
          i1 = 0;
          i  = 0;
          Dow YAJL_array_loop(node1:i1:node2);
             i2 = 0;
             i  += 1;
             If i > 1;
                json = json + ',{';
             EndIf;
             Dow YAJL_object_loop(node2:i2:key:val);
                BuildGiantJSON(json:key:YAJL_get_string(val));
                Select;
                When key = 'code';
                   batchArray(i).code    = YAJL_get_string(val);
                   batchCount += 1;
                When key = 'amount';
                   batchArray(i).amount  = YAJL_get_string(val);
                When key = 'effectiveDate';
                   batchArray(i).effDate = YAJL_get_string(val);
                   json = json + '}';
                Endsl;
             Enddo;
          Enddo;
          json = json + ']';

       Endsr;
       //========================================================
       // Save SR Batches
       //========================================================
       Begsr SaveBatches;

          // Save Batches if no errors
          If ioRtnCode = 'Error';
             Leavesr;
          Endif;

          // Create SR Batch
          For x = 1 to batchCount;
             Select;
             When x = 1;   // Premium Billing (PB)
                type1   = batchArray(x).code;
                amount1 = %Dec(batchArray(x).amount:11:2);
                date1   = %Dec(batchArray(x).effDate:8:0);
             When x = 2;   // Premium Payment (PP)
                type2   = batchArray(x).code;
                amount2 = %Dec(batchArray(x).amount:11:2);
                date2   = %Dec(batchArray(x).effDate:8:0);
             When x = 3;   // Tax Billing (TB)
                type3   = batchArray(x).code;
                amount3 = %Dec(batchArray(x).amount:11:2);
                date3   = %Dec(batchArray(x).effDate:8:0);
             When x = 4;   // Tax Payment (TP)
                type4   = batchArray(x).code;
                amount4 = %Dec(batchArray(x).amount:11:2);
                date4   = %Dec(batchArray(x).effDate:8:0);
             Endsl;
          EndFor;

          strData = data2;
          Select;
          When dsIn.status = '3RD PARTY';
             strPayType = 'P';
          Other;
             strPayType = 'T';
          EndSl;
          Monitor;
             intInvoice# = %int(dsIn.invoiceNum);
          On-error;
             intInvoice# = 0;
             dsIn.InvoiceNum = '0000000000';
          Endmon;
          SROnline (strPayType:
           %EditC(intCo#:'X'):
           %EditC(intGrp#:'X'):
           %EditC(intEmp#:'X'):
           %EditC(intDiv:'X'):
           %EditC(intYear:'X'):
           dsIn.reportDate:
           strMode:
           strError:
           strData:dsIn.InvoiceNum);

          If strPayType = 'P';
          // Get invoice # assigned by SRBAT
             intInvoice# =
              GetLast3rdPartySeq#(
              %EditC(intCo#:'X') +
              %EditC(intGrp#:'X') +
              %EditC(intEmp#:'X') +
              %EditC(intDiv:'X'));
             Exec Sql
               Update WASRP_HLD
               Set SRinv# = :intInvoice#
               Where SRco#  = :intCo#  And SRfnd = :intGrp# And
                     SRemp# = :intEmp# And SRdiv = :intDiv  And
                     SRfyr = :intYear And
                    (SRrpmy = :intRpmy Or SRtodt = :intToDt);
             json += ', "invoiceNumber":' +
              '"' +
              %EditC(intCo#:'X') +
              %EditC(intGrp#:'X') +
              %EditC(intEmp#:'X') +
              %EditC(intDiv:'X') +
              %EditC(intInvoice#:'X') +
              '"';
        // Get the Bank Acount

             w@Today = %int(%Char(%Date() : *USA0)) ;

             @@co   = %EditC(intCo#:'X');
             @@fnd  = %EditC(intGrp#:'X');
             @@appl = 'WP';
             @@srDt = %EditC(DateTo8(systemDate()):'X');
             @@call = *Blanks;
             @@hdcy = *Blanks;
             @@chkq = *Blanks;
             @@ftyp = *Blanks;
             @@efdt = *Zeros ;
             @@cpyq = *Zeros ;
             @@cktp = *Blanks;
             // Passing * = wildcard so we look for MICR or EFT
             @@eftc = '*';
             @@coid = *Blanks;
             @@rtcd = *Blanks;

             w@BnkAcc = GetBankAcc(@@co : @@fnd: @@appl: @@srdt: @@call:
                        @@hdcy: @@chkq: @@ftyp: @@efdt: @@cpyq:
                        @@cktp: @@eftc: @@coid: @@rtcd);

             Exec Sql
               Select CkInvTypId Into :w@InvTypId
               From   WSCKP
               Where  CKCO# = :intCo# and CKACCT = :w@BnkAcc;

             If w@InvTypId <> 0 ;
                json += ', "invoiceTypeId":' +
                 '"' +
                 %EditC(w@InvTypId:'X') +
                 '"';
             Else;
                json += ', "invoiceTypeId": "164"';
             EndIf;
          Endif;

          json += ', "results" :"SR Batches Created"';

          // WDEAP
          Exec Sql
            Update WDEAP Set EAInvTypID = :w@InvTypId
            Where  EAco#  = :intCo#        and
                   EAfnd  = :intGrp#       and
                   EAemp# = :intEmp#       and
                   EAdiv  = :intDiv        and
                   EAinv# = :intInvoice#;

          // WDEAP_HLD
          Exec Sql
            Update WDEAP_HLD Set EAInvTypID = :w@InvTypId
            Where  EAco#  = :intCo#        and
                   EAfnd  = :intGrp#       and
                   EAemp# = :intEmp#       and
                   EAdiv  = :intDiv        and
                   EAinv# = :intInvoice#;

          // WMEAP
          Exec Sql
            Update WMEAP Set EAInvTypID = :w@InvTypId
            Where  EAco#  = :intCo#        and
                   EAfnd  = :intGrp#       and
                   EAemp# = :intEmp#       and
                   EAdiv  = :intDiv        and
                   EAninv = :intInvoice#;

       Endsr;

       //=================================================================
       // Validate Incoming Data
       //=================================================================
       Begsr ValidateData;

          //--------------------------------------------------------------
          // Company # must be valid numeric value
          Monitor;
             intCo# = %int(dsIn.co#);
          On-error;
             AddErrorMessageJson('Incoming company not valid.':ioRtnCode);
          Endmon;

          //--------------------------------------------------------------
          // Group # must be valid numeric value
          Monitor;
             intGrp# = %int(dsIn.group);
          On-error;
             AddErrorMessageJson('Incoming group # not valid.':ioRtnCode);
          Endmon;
          If intGrp# <> 0 and not UseGroupInPortal(intCo#:intGrp#:'S/R Online');
             AddErrorMessageJson('Group ' + %char(intGrp#) +
              ' is not included for service requested ' +
              '(' + %trim(inRequest) + ').':ioRtnCode);
          Endif;

          //--------------------------------------------------------------
          // Employer # must be valid numeric value
          Monitor;
             intEmp# = %int(dsIn.emp#);
          On-error;
             AddErrorMessageJson('Incoming employer not valid.':ioRtnCode);
          Endmon;

          //--------------------------------------------------------------
          // Division must be valid numeric value
          Monitor;
             intDiv = %int(dsIn.division);
          On-error;
             AddErrorMessageJson('Incoming division not valid.':ioRtnCode);
          Endmon;

          //--------------------------------------------------------------
          // Year must be a valid numeric value
          Monitor;
             intYear = %int(dsIn.year);
          On-error;
             AddErrorMessageJson('Incoming year not valid.':ioRtnCode);
          Endmon;

          //--------------------------------------------------------------
          // Policy must be valid
          If ioRtnCode <> 'Error';
             If ValidEmpYr(intYear:intEmp#:intDiv:rrnWDELP:
                           intGrp#:intCo#) = *Off;
                AddErrorMessageJson('Invalid Policy.':ioRtnCode);
             EndIf;
          EndIf;

          //--------------------------------------------------------------
          // Report date must be valid.
          Monitor;
             decimalError = *Off;
             date = %Dec(dsIn.reportDate:8:0);
          On-error;
             AddErrorMessageJson('reportDate(entry: ' + dsIn.reportDate +
              ') contains invalid numeric data.':ioRtnCode);
             decimalError = *On;
          Endmon;
          // Lets see if valid date
          If Not decimalError;
             intToDt = dtCymd(date);
             If intToDt = 0;
                AddErrorMessageJson('reportDate(entry: ' + dsIn.reportDate +
                 ') is invalid.':ioRtnCode);
             Else;
                intRpmy = intToDt/100;
             EndIf;
          EndIf;

          // Let's validate the A/R Batch info passed in
          For x=1 to batchCount;

             // AR type must be set in this order PB/PP/TB/TP if passed in
             Select;
             When x = 1 And batchArray(x).code = 'PB'
              And batchArray(x) <> *Blanks;
             When x = 2 And batchArray(x).code = 'PP'
              And batchArray(x) <> *Blanks;
             When x = 3 And batchArray(x).code = 'TB'
              And batchArray(x) <> *Blanks;
             When x = 4 And batchArray(x).code = 'TP'
              And batchArray(x) <> *Blanks;
             Other;
                AddErrorMessageJson('Invalid A/R information.':
                 ioRtnCode);
             EndSl;

             // AR amount must be numeric if passed
             If batchArray(x).amount = ' ';
                AddErrorMessageJson('AR Amount(entry:' + %Char(x) +
                 ') cannot be blank.':ioRtnCode);
             Else;
                Monitor;
                   amount = %Dec(batchArray(x).amount:11:2);
                On-error;
                   AddErrorMessageJson('AR Amount(entry:' + %Char(x) +
                            ') contains invalid numeric data."':ioRtnCode);
                Endmon;
             EndIf;

             // AR Effective Date is required
             If batchArray(x).effDate = ' ';
                AddErrorMessageJson('AR Effective Date(entry:' + %Char(x) +
                 ') cannot be blank.':ioRtnCode);
             Else;
                Monitor;
                   decimalError = *Off;
                   date = %Dec(batchArray(x).effDate:8:0);
                On-error;
                   AddErrorMessageJson('AR Effective Date(entry:' + %Char(x) +
                    ') contains invalid numeric data.':ioRtnCode);
                   decimalError = *On;
                Endmon;
                // Lets see if valid date
                If Not decimalError;
                   If dtCYMD(date) = 0;
                      AddErrorMessageJson('AR Effective Date(entry:' +%Char(x)+
                       ') is invalid.':ioRtnCode);
                   EndIf;
                EndIf;
             EndIf;
          EndFor;

       Endsr;
       /end-free

     pCreateSRBatches...
     p                 e
       //=========================================================
       // Deletes 3RD PARTY A/R Batch records from holding file
       //  to permanent storage
       //========================================================
       Dcl-Proc Delete3rdPartyBatch;
       Dcl-Pi *N;
        inCo# Packed(3:0) Const;
        inFnd Packed(3:0) Const;
        inEmp Packed(9:0) Const;
        inDiv Packed(5:0) Const;
        inInv Packed(10:0) Const;
       End-Pi;
       Exec Sql
        Delete From WDEAP_HLD
         Where EAco# = :inCo#
          And EAfnd  = :inFnd
          And EAemp# = :inEmp
          And EArdiv = :inDiv
          And EAinv# = :inInv;
       End-Proc;
      *===========================================
      * Get Auto Cash Dissolve Flag
      *===========================================
       Dcl-Proc GetAutoCash;
       Dcl-Pi *N Ind;
        intCo# Packed(3:0);
        intFnd Packed(3:0);
       End-Pi;
       Dcl-S chrYesOrNo char(1);
       Exec Sql
        Select F2ccyf Into :chrYesOrNo
         From WDF2P
         Where F2co# = :intCo# And F2fnd = :intFnd And F2cpmt = ' ';
       If chrYesOrNo = 'Y';
          Return *On;
       Endif;
       Return *Off;
       End-Proc GetAutoCash;
      *===========================================
      * Returns 'EFT' or '3RD PARTY'
      *===========================================
      /Free
       Dcl-Proc GetPaymentType;
       Dcl-Pi *N VarChar(10);
        inCo# Packed(3:0) Const;
        inFnd Packed(3:0) Const;
        inEmp Packed(9:0) Const;
        inDiv Packed(5:0) Const;
       End-Pi;
       Dcl-C EFT 'EFT       ';
       Dcl-C thirdParty '3RD PARTY';
       Dcl-S intCount Zoned(3:0);
       Dcl-S intDate7 Zoned(7:0);
       intDate7 = %Dec(%Char(%Date():*Cymd0):7:0);
       Exec Sql
        Select Count(*)
        Into :intCount
        From WMP3P
        Where P3co#  = :inCo#
          And P3fnd  = :inFnd
          And P3emp# = :inEmp
          And P3div  = :inDiv
          And P3enbld = 'Y'
          And P3efdt <= :intDate7
          And (P3exdt > :intDate7 Or P3exdt = 0)
          And P3PrType = 'S/R Online';
       If intCount > 0;
          Return thirdParty;
       EndIf;
       Exec Sql
        Select Count(*)
        Into :intCount
        From WMEFP
        Where EFco#  = :inCo#
          And EFfnd  = :inFnd
          And EFemp# = :inEmp
          And EFdiv  = :inDiv
          And EFacct <> ' '
          And EFefdt <= :intDate7
          And (EFexdt > :intDate7 Or EFexdt = 0);
       If intCount > 0;
          Return EFT;
       Endif;
       Return *Blanks;
       End-Proc GetPaymentType;
      /End-Free
      *===========================================
      * Returns 0 for Sun, 1 for Mon, etc
      *===========================================
     pGetWeekDayNumber...
     p                 b
     dGetWeekDayNumber...
     d                 pi             1s 0
     d dayOfWeek                      2a
      /Free
       Select;
       When dayOfWeek = 'SU';
          Return 0;
       When dayOfWeek = 'MO';
          Return 1;
       When dayOfWeek = 'TU';
          Return 2;
       When dayOfWeek = 'WE';
          Return 3;
       When dayOfWeek = 'TH';
          Return 4;
       When dayOfWeek = 'FR';
          Return 5;
       When dayOfWeek = 'SA';
          Return 6;
       EndSl;
      /End-Free
     pGetWeekDayNumber...
     p                 e
      *===========================================
      * Returns *On if WASRP record exists for
      *  invoice number. Invoice number will be
      *  zero for EFT transactions or .
      *===========================================
      /Free
       Dcl-Proc HasSelfReport;
       Dcl-Pi *N Ind;
        inCo# Packed(3:0) Const;
        inFnd Packed(3:0) Const;
        inEmp Packed(9:0) Const;
        inDiv Packed(5:0) Const;
        inFyr Packed(3:0) Const;
        inInv Packed(10:0) Const;
        outInv Packed(10:0);
        inRptDate Char(8) Const;
        inDate5 Packed(5:0);
        inDate7 Packed(7:0);
       End-Pi;
       Dcl-S intReportDate5 Packed(5:0);
       Dcl-S intReportDate7 Packed(7:0);
       Dcl-S intReportDate8 Packed(8:0);
        Monitor;
           intReportDate8 = %Int(inRptDate);
           intReportDate7 = DtCYMD(intReportDate8);
           intReportDate5 = intReportDate7/100;
        On-error;
           intReportDate8 = 0;
           intReportDate7 = 0;
           intReportDate5 = 0;
        Endmon;
        If intReportDate8 = 0;
        //* SQL for payments coming in.
           Exec Sql
           Select Max(SRrpmy), Max(SRtodt)
            Into :inDate5, :inDate7
            From WASRP_HLD
            Where SRco# = :inCo#
             And SRfnd  = :inFnd
             And SRemp# = :inEmp
             And SRdiv  = :inDiv
             And SRfyr  = :inFyr
             And SRinv# = :inInv;

        Else;
        //* SQL for creating payments on a reporting period.
           Exec Sql
            Select Max(SRrpmy), Max(SRtodt), Max(SRInv#)
            Into :inDate5, :inDate7, :outInv
            From WASRP_HLD
            Where SRco# = :inCo#
             And SRfnd  = :inFnd
             And SRemp# = :inEmp
             And SRdiv  = :inDiv
             And SRfyr  = :inFyr
             And (SRRPMY = :intReportDate5 or SRTODT = :intReportDate7);

        Endif;
        If inDate5 > 0 Or inDate7 > 0;
           Return *On;
        Endif;
        Return *Off;
       End-Proc HasSelfReport;
      /End-Free
      *=========================================================
      * Request: ListIncompleteSRPayments
      * Description: Returns an array of Incomplete Sr Payments
      *=========================================================
     pListIncompleteSRPayments...
     p                 b

     d first           s               n   Inz(*On)
     d intCo#          s              3  0
     d intGrp#         s              3  0
     d invoiceList     s           5000a   varying inz
     d strSql          s           5000a   varying inz

     d dsIn            ds                  Qualified Inz
     d  co#                           3a
     d  group                         3a

     D dsWASRP         ds                  inz
     D   SRco#                        3  0
     D   SRfnd                        3  0
     D   SRemp#                       9  0
     D   SRdiv                        5  0
     D   SRfYr                        3  0
     D   SRefDt                       7  0
     D   SRrpmy                       5  0
     D   SRinv#                      15S 0

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

          json += ',"incompleteInvoices":[';
          strSQL = 'Select SRco#, SRfnd, SRemp#, SRdiv, SRfYr, SRefdt, ' +
                   'SRrpmy, SRinv# ' +
                   'From WASRP_HLD Where SRinv# <> 0 ';
          If intGrp# <> 0;
             strSQl += ' And SRfnd = ' + %EditC(intGrp#:'X');
          EndIf;
          strSQL += ' Group By SRco#, SRfnd, SRemp#, SRdiv, SRfYr, SRefdt' +
                   ',SRrpmy, SRinv#';
          strSQL += ' Order By SRco#, SRfnd, SRemp#, SRdiv, SRfYr, SRefdt' +
                   ',SRrpmy, SRinv#';

          Exec SQL close invCursor;

          // Prepare the SQL statement
          Exec SQL prepare invStmt from :strSQL;

          // Declare the SQL cursor
          Exec SQL declare invCursor cursor for invStmt;

          // Open cursor
          Exec SQL open invCursor;

          Reset dsWASRP;

          DoU Not IsValidFetch(sqlstt);
            Exec SQL fetch next from invCursor into :dsWASRP;
            If IsValidFetch(sqlstt);
                json += '{' +
                 BuildJsonNode('company':%EditC(SRco#:'X')) + ',' +
                 BuildJsonNode('group':%EditC(SRfnd:'X')) + ',' +
                 BuildJsonNode('employer':%EditC(SRemp#:'X')) + ',' +
                 BuildJsonNode('division':%EditC(SRdiv:'X')) + ',' +
                 BuildJsonNode('year':%EditC(SRfYr:'X')) + ',' +
                 BuildJsonNode('reportDate':%EditC(SRefDt:'X')) + ',' +
                 BuildJsonNode('reportCYM':%EditC(SRrpmy:'X')) + ',' +
                 BuildJsonNode('invoice#':%EditC(SRco#:'X') +
                  %EditC(SRfnd:'X') + %EditC(SRemp#:'X') +
                  %EditC(SRdiv:'X') +
                  %subst(%EditC(SRinv#:'X'):6)) + '},';
            Else;
               Leave;
            Endif;
          EndDo;
          json = %scanrpl(',':'':json:%len(json)-1);
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

        Else;
           AddErrorMessageJson('JSON filter can''t be read.':ioRtnCode);
        Endif;

       Endsr;

     pListIncompleteSRPayments...
     p                 e
      *=========================================================
      * Request: ListSRPayrollClasses
      * Description: Returns an array of S/R Payroll classes
      *=========================================================
     pListSRPayrollClasses...
     p                 b

     d decAmountTotal  s             15  2 inz
     d decCount        s             10i 0 inz
     d decPeriod       s              8  0 inz
     d decYearCount    s             10i 0 inz
     d dt7             s              7  0 inz
     d dt              s               d   inz
     d intDiv          s              5  0 inz
     d intEmp#         s              9  0 inz
     d intFnd          s              3  0 inz
     d intFyr          s              3  0 inz
     d isEofClassList  s              1n
     d jsonClasses     s               a   len(500000)  varying inz
     d jsonPeriods     s               a   len(1000000) varying inz
     d jsonYears       s               a   len(1500000) varying inz
     d kkRtCd          s              3    inz
     d kkSt            s              2    inz
     d hazardGroup     s              5    inz
     d contClass       s              1    inz
     d**** Beg Chg **** 06/17/24 *******************************************
     d***classRate       s              7s 2 inz
     d classRate       s              8s 3 inz
     d**** End Chg **** 06/17/24 *******************************************
     d strClassDesc    s             40a   varying inz
     d strCodeBill     s              2a   inz
     d strCodePayment  s              2a   inz
     d strDtEf         s             10a   varying inz
     d strDtFr         s             10a   varying inz
     d strDtTo         s             10a   varying inz
     d strDt7          s              7a   inz
     d strFile         s             25a   varying inz
     d strFyr          s              3a   inz
     d strPeriod       s             10a   varying inz
     d strSql          s           5000a   varying inz
     d strSqlOrder     s            500a   varying inz
     d strSqlSelect    s           1000a   varying inz
     d strSqlWhere     s           5000a   varying inz
     d strSqlWhereEP   s           5000a   varying inz
     d strSqlWhereSR   s           5000a   varying inz
     d strStatus       s             25a   varying inz
     d strPolicyInfo   s          50000a   varying inz
     DisNetRate        s              1n   inz('0')
     d perCapita       s              1    inz
     d premium         s             11  0 inz
     d netRate         s              9  4 inz
     d netFactor       s              9  6 inz
     d rrnWDELP        s              9  0 Inz
     d intRptDate6     s              6  0 Inz
     d intRptDate8     s              8  0 Inz
     d intMapr         s             11  0 Inz
     d intAdld         s             11  0 Inz
     d intPayr         s             11  0 Inz
     d intPrem         s             11  0 Inz
     d intTax          s             11  0 Inz
     d intPPrem        s             11  0 Inz
     d intPTax         s             11  0 Inz
     d intPrev         s             11  2 Inz
     d chrSrOn         s              1a   Inz('S')
     d
     d                 ds
     d q1Fact                  1    143
     d  q1Lmpc                 1      5  3
     d  q1Tot1                 6     16  0
     d  q1Dscb                17     27  9
     d  q1Tot2                28     38  0
     d  q1ExPm                39     43  4
     d  q1Tot3                44     54  0
     d  q1Dsca                55     65  9
     d  q1Tot4                66     76  0
     d  q1Terr                77     87  0
     d  q1Waiv                88     98  0
     d  q1Rein                99    109  0
     d  q1Netf               110    121  9
     d  q1Netp               122    132  0
     d  q1NWav               133    143  0

     d dsIn            ds                  Qualified Inz
     d  type                         25a   varying
     d  co#                           3a
     d  group                         3a
     d  emp#                          9a
     d  div                           5a
     d  fyr                           3a
     d  period                        8a
     d  periodType                   25a   varying

     d dsSRIn          ds                  qualified inz
     d  co#                           3  0
     d  fnd                           3  0
     d  emp#                          9  0
     d  div                           5  0
     d  fyr                           3  0
     d  code                          5  0
     d  spec                          1
     d  ext                           5
     d  dtEf                          7  0
     d  dtCx                          7  0
     d**** Beg Chg **** 06/17/24 *******************************************
     d***rate                          7  2
     d  rate                          8S 3
     d**** End Chg **** 06/17/24 *******************************************
     d  netRate                       9  4
     d  premium                      11  2
     d  amount                       11  2
     d  rpMY                          5  0
     d  dtFr                          7  0
     d  dtTo                          7  0
     d  period                        8  0
     d  periodType                   25    varying
     d  status                       25    varying
     d dsSR            ds                  likeds(dsSRIn) inz
     d dsSRinz         ds                  likeds(dsSRIn) inz
     d dsSRsv          ds                  likeds(dsSRIn) inz
       //*** Beg Add *** 10/20/23 ***************************************
       dcl-s decPolToDt Zoned(7:0);
       //*** End Add *** 10/20/23 *****************************
      /Free
       Exsr GetParms;
       //Exsr BuildStub;
       If ioRtnCode <> 'Error';
          Exsr BuildSql;
          Exsr ProcessSql;
       Endif;
       json += ProcessErrorMessagesJson('*PRE');
       json = %scanrpl(',':'':json:1:1);
       outJSON = json;
       Return;

       //=================================================================
       // Build stub results
       //=================================================================
       Begsr BuildStub;
        If json <> *blanks;
           json += ',';
        Endif;
        json +=
         %trim(BuildJsonNode('stub':%trim(inRequest) + ' (' +
         %trim(q1Pgm) + ') successfully executed. {ListSRPayrollPeriods}'));
       Endsr;

       //========================================================
       // Builds Return JSON
       //========================================================
       Begsr BuildReturnJsonClass;
        decAmountTotal += dsSR.amount;
        strDtEf = %char(DtMDYY(dsSR.dtEf));
        kkRtCd = GetRateSet(GetEmpYrRR(dsSR.fyr:dsSR.emp#:dsSR.div:
         dsSR.fnd:dsSR.co#));
        kkSt = RatingSt(dsSR.co#:kkRtCd);
        strClassDesc = GetPayrollDesc(dsSR.co#:dsSR.dtEf:kkSt:
           kkRtCd:dsSR.code:dsSR.ext:hazardGroup:contClass:classRate);
        premium = %dech(dsSR.premium:11:0);
        If isNetRate;
        If netFactor <> 0;
           netRate = %dech(dsSR.rate * netFactor:9:4);
        Else;
           netRate = dsSR.NetRate;
        Endif;
           Exec Sql
            Select PRpcf Into :perCapita
            From WMPRP
            Where PRco# = :dsSR.co#
             And PRrtcd = :kkRtcd
             And PRstat = 'A'
             And PRclas = :dsSR.code
             And PRefdt <= :dsSR.dtEf
            Order by PRco#, PRrtcd, PRclas, PRefdt Desc
            Limit 1;
           Select;
           When Not IsValidFetch(sqlStt);
           When perCapita = 'P';
              premium = %dech((dsSR.amount * netRate/100):11:0);
           When perCapita = 'C';
              premium = %dech((dsSR.amount * netRate):11:0);
           Endsl;
        Endif;
        dsSR.status = %scanrpl('*':'':dsSR.status:1:1);
        jsonClasses += '{' +
         '"classInfo":{' +
          '"class":{' +
           BuildJsonNode('code':%char(dsSR.code)) + ',' +
           BuildJsonNode('specialDesignation':dsSR.spec) + ',' +
           BuildJsonNode('extension':dsSR.ext) + ',' +
           BuildJsonNode('effectiveDate':strDtEf:'date') + ',' +
           BuildJsonNode('cancelDate':%char(DtMDYY(dsSR.dtCx)):'date') + ',' +
           BuildJsonNode('description':strClassDesc) + '},' +
           BuildJsonNode('premium':%editc(premium:'J')) + ',';
          If not isNetRate;
             jsonClasses += BuildJsonNode('rate':%editc(dsSR.rate:'J')) + ',';
          Else;
             jsonClasses += BuildJsonNode('rate':%editc(netRate:'J')) +
              ',';
          Endif;
          jsonClasses +=
          BuildJsonNode('amount':%editc(dsSR.amount:'J')) + ',' +
          BuildJsonNode('status':dsSR.status) +
         '}},';
       Endsr;

       //========================================================
       // Builds Return JSON {employer info}
       //========================================================
       Begsr BuildReturnJsonEmp;
        Exsr BuildReturnJsonFyr;
        If %len(jsonYears) > 0;
           jsonYears = %scanrpl(',':'':jsonYears:%len(jsonYears)-1);
        Endif;
        json += '{' +
         BuildJsonNode('company':%char(dsSRsv.co#)) + ',' +
         BuildJsonNode('group':%char(dsSRsv.fnd)) + ',' +
         BuildJsonNode('employer':%char(dsSRsv.emp#)) + ',' +
         BuildJsonNode('division':%char(dsSRsv.div)) + ',' +
         '"years": [' + jsonYears + ']' +
         '},';
        Reset jsonYears;
       Endsr;

       //========================================================
       // Builds Return JSON {year info}
       //========================================================
       Begsr BuildReturnJsonFyr;
        Exsr BuildReturnJsonPeriod;
        decYearCount += 1;
        If %len(jsonPeriods) > 0;
           jsonPeriods = %scanrpl(',':'':jsonPeriods:%len(jsonPeriods)-1);
        Endif;
        jsonYears += '{' +
         BuildJsonNode('year':%char(dsSRsv.fyr)) + ',' +
         '"periods": [' + jsonPeriods + ']' +
         '},';
        Reset jsonPeriods;
       Endsr;

       //========================================================
       // Builds Return JSON {period info}
       //========================================================
       Begsr BuildReturnJsonPeriod;
        Clear strDtFr;
        Clear strDtTo;
        Select;
        When dsSRsv.periodType = 'WMEPP';
           LoadCurrentPeriod(dsSRsv.co#:dsSRsv.fnd:dsSRsv.fyr:dsSRsv.emp#:
            dsSRsv.div:%dec(dsSRsv.period:8:0));
           dsSRsv.period = dsBillingInfo.period;
           dsSRsv.periodType = dsBillingInfo.periodType;
        Endsl;
        Select;
        When dsSRsv.periodType = 'Other';
           strPeriod = %char(DtMDYY(dsSRsv.period));
           strDtTo = %char(DtMDYY(dsSRsv.period));
           strDtFr = %char(DateTo8(CvtToDate(DtMDYY(dsSRsv.period))-%days(7)));
        When dsSRsv.periodType = 'Monthly';
           strPeriod = %char(Dt6MDYY(dsSRsv.period));
           strDtFr = strPeriod + '01';
           //*** Beg Add *** 10/20/23 ***********************************
           // Split Month.  If the reporting month is the policy from date month
           // and the year is the same.

           If %Subst(%Editc(dsSR.dtEf:'X'):1:5) =
            %Subst(%Editc(dsSRsv.period:'X'):4:5);
              strDay = %Subst(%Char(dsSR.dtEf):6:2);
           Else;
              strDay = '01';
           Endif;
           //*** End Add *** 10/20/23 ***********************************
           //*** Beg Chg *** 10/20/23 ***********************************
           //***strDtFr = %char(DtMDYY(%dec(%char(dsSRsv.period)+'01':7:0)));
           strDtFr = %char(DtMDYY(%dec(%char(dsSRsv.period) + strday:7:0)));
           //*** End Chg *** 10/20/23 ***********************************
           //*** Beg Add *** 10/20/23 ***************************************
           // If the reporting month is the policy to date month
           Exec Sql
            Select ELptDt
            Into :decPolToDt
            From WDELP
            Where ELco#  = :dsSrSv.Co#
               And ELfnd  = :dsSrSv.fnd
               And ELemp# = :dsSrSv.emp#
               And ELDiv  = :dsSrSv.div
               And ELfyr  = :dsSrSv.fyr;
           If %Subst(%Editc(decPolToDt:'X'):1:5) =
            %Subst(%Editc(dsSRsv.period:'X'):4:5);
              strDay = %Subst(%Char(decPolToDt):6:2);
           Else;
              strDay = '99';
           Endif;
           //*** End Add *** 10/20/23 ***************************************
           //*** Beg Chg *** 10/20/23 ***************************************
           //***strDtTo = %char(DateTo8(GetLastDay(CvtToDate(%dec(strDtFr:8:0)))));
           If strDay = '99';
             strDtTo = %char(DateTo8(GetLastDay(CvtToDate(%dec(strDtFr:8:0)))));
           Else;
              strDtTo = %char(DtMDYY(%dec(%char(dsSRsv.period) + strday:7:0)));
           Endif;
           //*** End Chg *** 10/20/23 ***************************************
        Other;
           strPeriod = 'Invalid';
        Endsl;

        Exsr ChkPeriodStatus;

        //* Call wpiemplr here for policy info to insert below.
        strPolicyInfo = PolicyJSONData(dsSRsv.co#:dsSRsv.fnd:dsSRsv.emp#:
         dsSRsv.div:dsSRsv.fyr);

        If %len(jsonClasses) > 0;
           jsonClasses = %scanrpl(',':'':jsonClasses:%len(jsonClasses)-1);
        Endif;
        jsonPeriods += '{' +
         BuildJsonNode('date':strPeriod) + ',' +
         BuildJsonNode('startDate':strDtFr:'date') + ',' +
         BuildJsonNode('endDate':strDtTo:'date') + ',' +
         BuildJsonNode('type':dsSRsv.periodType) + ',' +
         %trim(strPolicyInfo) + ',' +
         '"reportingInfo" : { ' +
          BuildJsonNode('reportingPeriod':
           AlphaUsa(CvtToDate(%int(strDtFr))) + ' - ' +
           AlphaUsa(CvtToDate(%int(strDtTo)))) + ', ' +
          BuildJsonNode('amountTotal':%editc(decAmountTotal:'J')) +
          ' },' +
         BuildJsonNode('status':dsSRsv.status) + ',' +
         '"payrollClasses": [' + jsonClasses + ']' +
         '},';
        Reset jsonClasses;
        Reset decAmountTotal;
        LoadCurrentPeriod(dsSR.co#:dsSR.fnd:dsSR.fyr:dsSR.emp#:
         dsSR.div:%dec(dsSR.period:8:0));
        Exsr CallSRPREM;
       Endsr;

       //==================================================
       // Build SQL
       //==================================================
       Begsr BuildSql;
        strSqlWhereSR = ' where' +
         ' SRrpMY = ' + strPeriod;
        //*** Begin Chg *** 02/21/25 ***************************************
        //*** strSqlWhereEP = ' where' +
        //***  ' EPco# = ' + dsIn.co# + ' and EPfnd = ' + dsIn.group +
        //***  ' and EPemp# = ' + dsIn.emp#;
        strSqlWhereEP = ' where' +
         ' EPco# = ' + dsIn.co# + ' and EPfnd = ' + dsIn.group +
         ' and EPemp# = ' + dsIn.emp# + ' and EPstat <> ''I''';
        //*** End   Chg *** 02/21/25 ***************************************
        If dsIn.div <> *blank;
           strSqlWhereEP += ' and EPdiv = ' + dsIn.div;
        Endif;
        Select;
        When dsIn.fyr <> *blank;
           strSqlWhereEP += ' and EPfyr = ' + dsIn.fyr;
        Endsl;
        Select ;
        When Upper(dsIn.periodtype) = 'NEW';
           strSqlWhereEP += ' and (EPcaDt = 0 or EPcaDt > ' +
            %char(dsBillingInfo.periodDtFrom) + ')' +
            ' and EPefDt <= ' + %char(dt7);
        Endsl;
        strSqlOrder = ' order by EPCO#, EPFND, EPEMP#, EPDIV, EPFYR, ' +
         'Period desc, status, EPCLAS, EPSPEC, EPEXT, EPEFDT';
        strSqlSelect = ' select SRCO#, SRFND, SREMP#, SRDIV, SRFYR,' +
         '  SRCLAS, SRSPEC, SREXT, SREFDT, SRCADT,' +
         '  case when hex(SRMANPRM) = ''4040404040404040404040'' then 0' +
         '   else SRMANPRM end as SRMANPRM,' +
         '  SRAMTP, SRRPMY, SRFRDT, SRTODT,' +
         '  case when hex(SRINV#) = ''404040404040404040404040404040'' then 0'+
         '   else SRINV# end as SRINV#';
        strSql = ' with' +
         ' submitted as (' + strSqlSelect + ',' +
         '  ''Submitted'' as SR_status' +
         '  from WASRP ' + strSqlWhereSR + ')' +
         ',' +
         ' pending as (' + strSqlSelect + ',' +
         '  ''Pending'' as SR_status' +
         '  from WASRP_HLD ' + strSqlWhereSR + ')' +
         ',' +
         ' combined as (' +
         '  select * from submitted' +
         '   where not exists' +
         '   (select * from pending p' +
         '     where submitted.SRCO#=p.SRCO# and submitted.SRFND=p.SRFND' +
         '      and submitted.SREMP#=p.SREMP# and submitted.SRDIV=p.SRDIV' +
         '      and submitted.SRFYR=p.SRFYR and submitted.SRCLAS=p.SRCLAS' +
         '      and submitted.SRSPEC=p.SRSPEC and submitted.SREXT=p.SREXT' +
         '      and submitted.SREFDT=p.SREFDT)' +
         '  union select * from pending)';
        strSql += ' select EPCO#, EPFND, EPEMP#, EPDIV, EPFYR, ' +
         ' EPCLAS, EPSPEC, EPEXT, EPEFDT, EPCADT, ' +
         ' EPARAT, EPENTR, ' +
         ' case when SRMANPRM is null then 0 else SRMANPRM end as prem, ' +
         ' case when SRAMTP   is null then 0 else SRAMTP   end as amt, ' +
         ' case when SRRPMY   is null then 0 else SRRPMY   end as RPMY, ' +
         ' case when SRFRDT   is null then 0 else SRFRDT   end as frDt, ' +
         ' case when SRTODT   is null then 0 else SRTODT   end as toDt, ' +
         ' case when SRTODT is null and SRRPMY is null then ' +
          %char(dsBillingInfo.period) +
         '  when SRTODT <> 0 then SRTODT else SRRPMY end as Period, ' +
         ' case when SRTODT is null and SRRPMY is null then ''WMEPP'' ' +
         '      when SRTODT <> 0 then ''Other'' else ''Monthly'' ' +
         '  end as Period_Type, '+
         ' case ' +
         '  when SR_status is null then ''*Not Submitted''' +
         '  when SR_status = ''Pending'' and SRinv# <> 0 ' +
         '   then ''Pending Payment''' +
         '  else SR_status' +
         '  end as status ' +
         ' from WMEPP ' +
         '  left join combined on EPCO#=SRCO# and EPFND=SRFND and ' +
         '   EPFYR=SRFYR and EPEMP#=SREMP# and EPDIV=SRDIV and ' +
         '   EPCLAS=SRCLAS and EPSPEC=SRSPEC and EPEXT=SREXT' +
           strSqlWhereEP + strSqlOrder;
        Exsr CursorReset;
       Endsr;

       //==================================================
       // Build SQLs
       //==================================================
       Begsr BuildSqlOld;
        Exsr BuildSqlWASRP;
        Exsr CursorReset;
        Exec SQL fetch next from cursorSRClasses into :dsSR;
        Select;
        When IsValidFetch(sqlStt) and Upper(dsIn.periodType) = 'NEW';
           ioRtnCode = 'Exists';
        When (not IsValidFetch(sqlStt) and dsIn.period <> *blank)
         or Upper(dsIn.periodType) = 'NEW';
           Exsr BuildSqlWMEPP;
        Endsl;
        Exsr CursorReset;
       Endsr;

       //==================================================
       // Build SQLs for WASRP
       //==================================================
       Begsr BuildSqlWASRP;
        strSqlWhere = ' where' +
         ' SRco# = ' + dsIn.co# + ' and SRfnd = ' + dsIn.group +
         ' and SRemp# = ' + dsIn.emp#;
        If dsIn.div <> *blank;
           strSqlWhere += ' and SRdiv = ' + dsIn.div;
        Endif;
        Select;
        When dsIn.fyr <> *blank;
           strSqlWhere += ' and SRfyr = ' + dsIn.fyr;
        Endsl;
        strSqlOrder = ' order by SRCO#, SRFND, SREMP#, SRDIV, SRFYR, ' +
         'Period desc, SRCLAS, SRSPEC, SREXT, SREFDT';
        strSqlSelect = ' select SRCO#, SRFND, SREMP#, SRDIV, SRFYR,' +
         '  SRCLAS, SRSPEC, SREXT, SREFDT, SRCADT,' +
         '  case when hex(SRMANPRM) = ''4040404040404040404040'' then 0' +
         '   else SRMANPRM end as SRMANPRM,' +
         '  SRAMTP, SRRPMY, SRFRDT, SRTODT, ' +
         '  case when SRTODT <> 0 then SRTODT' +
           '       else SRRPMY' +
         '  end as Period,' +
         '  case when SRTODT <> 0 then ''Other''' +
         '       else ''Monthly''' +
         '  end as Period_Type';
        strSql = ' with' +
         ' submitted as (' + strSqlSelect + ',' +
         '  ''Submitted'' as SR_status' +
         '  from WASRP ' + strSqlWhere + ')' +
         ',' +
         ' pending as (' + strSqlSelect + ',' +
         '  ''Pending'' as SR_status' +
         '  from WASRP_HLD ' + strSqlWhere + ')' +
         ',' +
         ' combined as (' +
         '  select * from submitted' +
         '  union select * from pending)';
        Select;
        When strPeriod <> *blank;
           strSqlWhere = ' where period = ' + strPeriod;
        Other;
           Clear strSqlWhere;
        Endsl;
        strSql += ' select *' +
         ' from combined' + strSqlWhere + strSqlOrder;
       Endsr;

       //==================================================
       // Build SQLs for WMEPP
       //==================================================
       Begsr BuildSqlWMEPP;
        If strPeriod = *blanks;
           strPeriod = '0';
        Endif;
        Select;
        When dt7 = 0;
           strDt7 = %char(DateTo7(%date()));
        Other;
           strDt7 = %char(dt7);
        Endsl;
        strSqlWhere = ' where' +
         ' EPco# = ' + dsIn.co# + ' and EPfnd = ' + dsIn.group +
         ' and EPemp# = ' + dsIn.emp# + ' and EPefDt <= ' + strDt7 +
         ' and (EPcaDt = 0 or EPcaDt > '+%char(dsBillingInfo.periodDtFrom)+')';
        If dsIn.div <> *blank;
           strSqlWhere += ' and EPdiv = ' + dsIn.div;
        Endif;
        Select;
        When dsIn.fyr <> *blank;
           strSqlWhere += ' and EPfyr = ' + dsIn.fyr;
        When dsIn.period <> *blank;
           Exec SQL close cursorSRFyr;
           strSql = 'select distinct digits(ELFYR) ' +
            'from WDELP ' +
            'where ELco# = ' + dsIn.co# + ' and ELfnd = ' + dsIn.group +
            ' and ELemp# = ' + dsIn.emp# +
            ' and ' + strDt7 + ' between ELPFDT and ELPTDT ';
           If dsIn.div <> *blank;
              strSql += ' and ELdiv = ' + dsIn.div;
           Endif;
           Exec SQL prepare sqlSRFyr from :strSql;
           Exec SQL declare cursorSRFyr cursor for sqlSRFyr;
           Exec SQL open cursorSRFyr;
           Dou not IsValidFetch(sqlstt);
              Exec SQL fetch next from cursorSRFyr into :strFyr;
              Select;
              When not IsValidFetch(sqlstt);
                 Iter;
              When %scan('and EPfyr':strSqlWhere) <= 0;
                 strSqlWhere += ' and EPfyr in (';
              Endsl;
              strSqlWhere += strFyr + ',';
           Enddo;
           strSqlWhere = %scanrpl(',':')':strSqlWhere:%len(strSqlWhere)-1);
        Endsl;
        strSqlOrder = ' order by EPCO#, EPFND, EPEMP#, EPDIV, EPFYR, ' +
         'Period desc, EPCLAS, EPSPEC, EPEXT, EPEFDT';
        strSql = ' select EPCO#, EPFND, EPEMP#, EPDIV, EPFYR, ' +
         'EPCLAS, EPSPEC, EPEXT, EPEFDT, EPCADT, 0 as prem, ' +
         '0 as amt, 0 as RPMY, 0 as frDt, 0 as toDt, ' +
         strPeriod + ' as period, ' +
         '''WMEPP'' as Period_Type,' +
         '''Not Submitted'' as SR_status' +
         ' from WMEPP' + strSqlWhere + strSqlOrder;
        Exsr CursorReset;
       Endsr;
       //========================================================
       // Call SRPREM to retrieve net rate factor for reporting period
       //========================================================
       Begsr CallSRPREM;

       //* Establish net rate flag for current policy/reporting period.
        isNetRate = IsUseNetRate(dsSR.co#:dsSR.fnd:dsSR.emp#:dsSR.div:dsSR.fyr);

        ValidEmpYr(dsSR.fyr:dsSR.emp#:dsSR.div:rrnWDELP:dsSR.fnd:dsSR.co#);

        intRptDate6 = Dt6MDYY(dsSR.rpmy);
        intRptDate8 = DtMDYY(dsSR.dtTo);
        If intRptDate6 = 0 and intRptDate8 = 0;
           Select;
           When dsBillingInfo.periodType = 'Other';
              intRptDate8 = DtMDYY(dsBillingInfo.periodDtTo);
           When dsBillingInfo.periodType = 'Monthly';
              intRptDate6 = Dt6MDYY(dsBillingInfo.periodDtTo/100);
           Endsl;
        Endif;

        intMapr  = 0;
        intAdld  = 0;
        intPayr  = 0;
        intPrem  = 0;
        intTax   = 0;
        intPPrem = 0;
        intPTax  = 0;
        intPrev  = 0;
        Clear q1fact;
        netFactor = 0;

        If isNetRate;
           CalcSrPrem(rrnWDELP:
                      intRptDate6:
                      intRptDate8:
                      intMapr:
                      intAdld:
                      intPayr:
                      intPrem:
                      intTax:
                      intPPrem:
                      intPTax:
                      q1Fact:
                      intPrev:
                      chrSrOn);
           netFactor = %dec(q1netf:9:6);
        Endif;

       Endsr;

       //========================================================
       // Check period status
       //========================================================
       Begsr ChkPeriodStatus;
        dsSRSv.status = GetSRReportStatus(dsSRSv.co#:dsSRSv.fnd:dsSrSv.emp#
         :dsSRSv.div:dsSRSv.fyr:dsSRSv.period);
        If IsValidReportStatus(inRequest:dsSRSv.co#:dsSRSv.fnd:dsSrSv.emp#
         :dsSRSv.div:dsSRSv.fyr:dsSRSv.period);
           ioRtnCode = 'Error';
        Endif;
       Endsr;

       //========================================================
       // Reset cursor
       //========================================================
       Begsr CursorReset;
        Exec SQL close cursorSRClasses;
        Exec SQL prepare sqlSRClasses from :strSql;
        Exec SQL declare cursorSRClasses cursor for sqlSRClasses;
        Exec SQL open cursorSRClasses;
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
                 dsIn.emp# = YAJL_get_string(val);
              When key = 'division';
                 dsIn.div = YAJL_get_string(val);
              When key = 'year';
                 dsIn.fyr = YAJL_get_string(val);
              When key = 'period';
                 Exsr GetParmsPeriod;
              Endsl;
           Enddo;
           Yajl_Tree_Free(node0);
           json = json + '}';
           Monitor;
              intCo# = %int(dsIn.co#);
           On-error;
           Endmon;
           dsIn.co# = %char(intCo#);
           Exsr ValidateParms;
        Else;
           AddErrorMessageJson('JSON filter can''t be read.':ioRtnCode);
        Endif;
       Endsr;

       //========================================================
       // Break parms out of JSON - Period
       //========================================================
       Begsr GetParmsPeriod;
        Reset i1;
        node1 = YAJL_object_find(node0:key);
        json = %scanrpl('""':'{':json:%len(json)-1);
        Dow YAJL_object_loop(node1:i1:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'date';
              dsIn.period = YAJL_get_string(val);
           When key = 'type';
              dsIn.periodType = YAJL_get_string(val);
           Endsl;
        Enddo;
        json += '}';
        Reset decPeriod;
        Monitor;
           decPeriod = %int(dsIn.period);
        On-error;
        Endmon;
       Endsr;

       //========================================================
       // Read through SQL recrods
       //========================================================
       Begsr ProcessSQL;
        Reset decAmountTotal;
        Reset jsonClasses;
        json += ',"SRPayrollClassList":[ ';
        Reset dsSR;
        Reset isEofClassList;
        Dou isEofClassList;
           Exec SQL fetch next from cursorSRClasses into :dsSR;
           Select;
           When not IsValidFetch(sqlStt);
              If dsSRsv <> dsSRinz;
                 Exsr BuildReturnJsonEmp;
              Endif;
              isEofClassList = *on;
              Iter;
           When dsSRsv = dsSRinz;
              Exsr CallSRPREM;
           When dsSR.co# <> dsSRSv.co#
            or dsSR.fnd <> dsSRSv.fnd
            or dsSR.emp# <> dsSRSv.emp#
            or dsSR.div <> dsSRSv.div;
              Exsr BuildReturnJsonEmp;
           When dsSR.fyr <> dsSRSv.fyr;
              Exsr BuildReturnJsonFyr;
           When dsSR.period <> dsSRSv.period;
              Exsr BuildReturnJsonPeriod;
           Endsl;
           Exsr BuildReturnJsonClass;
           dsSRSv = dsSR;
        Enddo;
        json = %scanrpl(',':'':json:%len(json)-1);
        json += ']';
       Endsr;

       //========================================================
       // Validate parms
       //========================================================
       Begsr ValidateParms;
        Exec sql
         select * into :dsWINOTIP1
          from WINOTIP1
           where N1co# = :intCo#;

        Monitor;
           intFnd  = %int(dsIn.group);
           intFyr  = %int(dsIn.fyr);
           intEmp# = %int(dsIn.emp#);
           intDiv  = %int(dsIn.div);
        On-error;
           AddErrorMessageJson('Incoming information not valid for ' +
            'company, group, employer, division and/or year.'
            :ioRtnCode);
           Leavesr;
        Endmon;

        If intFnd <> *zero and not UseGroupInPortal(intCo#:intFnd:'S/R Online');
           AddErrorMessageJson('Group ' + %char(intFnd) +
            ' is not included for service requested ' +
            '(' + %trim(inRequest) + ').':ioRtnCode);
           Leavesr;
        Endif;

        Select;
        When Upper(dsIn.periodtype) = 'NEW' and decPeriod <> *zero;
           AddErrorMessageJson('Period date not allowed when creating a ' +
            'New report.':ioRtnCode);
        When Upper(dsIn.periodtype) = 'NEW';
           LoadCurrentPeriod(%int(dsIn.co#):%int(dsIn.group):%int(dsIn.fyr)
            :%int(dsIn.emp#):%int(dsIn.div));
           strPeriod = %char(dsBillingInfo.period);
           dt7 = dsBillingInfo.periodDtTo;
        When Upper(dsIn.periodtype) = 'MONTHLY'
         and Dt6Check(decPeriod) <> *zero;
           dt7 = Dt5CYMD(decPeriod);
           strPeriod = %char(dt7);
           dt7 = dt7 * 100 + 31;
        When Upper(dsIn.periodtype) <> 'MONTHLY'
         and DtCheck(decPeriod) <> *zero;
           dt7 = DtCYMD(decPeriod);
           strPeriod = %char(dt7);
        Other;
           AddErrorMessageJson('Invalid period.':ioRtnCode);
        Endsl;

        If Upper(dsIn.periodtype) = 'NEW';
           // Check year against period
           Exec sql
            select count(*) into :decYearCount
             from WDELP
             where ELco# = :intCo# and ELfnd = :intFnd and ELfyr = :intFyr and
              ELemp# = :intEmp# and ELdiv = :intDiv and
              (:dsBillingInfo.periodDtFrom between ELpfDt and ELptDt or
               :dsBillingInfo.periodDtTo between ELpfDt and ELptDt);
           If decYearCount <= 0;
              AddErrorMessageJson('Period (' + dsBillingInfo.periodAlpha + ')' +
               ' is not valid for the year (' + dsIn.fyr + ') requested.'
               :ioRtnCode);
           Endif;
        Endif;

       Endsr;
     pListSRPayrollClasses...
     p                 e

      *=========================================================
      * Request: ListSRPayrollPeriods
      * Description: Returns an array of S/R Payroll periods reported
      *=========================================================
     pListSRPayrollPeriods...
     p                 b

     d decBilled       s             15  2 inz
     d decPaid         s             15  2 inz
     d dt5From         s              7  0 inz
     d dt5To           s              7  0 inz(*hival)
     d dt7From         s              7  0 inz
     d dt7To           s              7  0 inz(*hival)
     d dt7             s              7  0 inz
     d intFnd          s              3  0
     d isEofSRPeriodList...
     d                 s              1n
     d isProcessed     s              1n   inz
     d strCodeBill     s              2a   inz
     d strCodePayment  s              2a   inz
     d strFile         s             25a   varying inz
     d strPeriod       s             10a   varying inz
     d strSql          s           5000a   varying inz
     d strSqlOrder     s            500a   varying inz
     d strSqlSelect    s           1000a   varying inz
     d strSqlWhere     s           5000a   varying inz
     d strStatus       s             25a   varying inz
     d strStsDesc      s             40a   inz

     d                 ds
     d  xxCmpDt                1      7  0
     d  xxCmpYM                1      5  0

     d dsIn            ds                  Qualified Inz
     d  type                         25a   varying
     d  co#                           3a
     d  group                         3a
     d  emp#                          9a
     d  div                           5a
     d  fyr                           3a
     d  dateFrom                      8a
     d  dateTo                        8a

     d dsSRIn          ds                  qualified inz
     d  co#                           3  0
     d  fnd                           3  0
     d  emp#                          9  0
     d  div                           5  0
     d  fyr                           3  0
     d  stat                          1
     d  inv#                         15  0
     d  periodType                   25    varying
     d  period                        7  0
     d  status                       25    varying
     d dsSR            ds                  likeds(dsSRIn) inz
     d dsSRSv          ds                  likeds(dsSRIn) inz
     d srNull          s              5i 0 dim(10)

      /Free
       Exsr GetParms;
       Exsr BuildSql;
       Exsr ProcessSql;
       json += ProcessErrorMessagesJson('*PRE');
       json = %scanrpl(',':'':json:1:1);
       outJSON = json;
       Return;

       //=================================================================
       // Build stub results
       //=================================================================
       Begsr BuildStub;
        If json <> *blanks;
           json += ',';
        Endif;
        json +=
         %trim(BuildJsonNode('stub':%trim(inRequest) + ' (' +
         %trim(q1Pgm) + ') successfully executed. {ListSRPayrollPeriods}'));
       Endsr;

       //========================================================
       // Builds Return JSON
       //========================================================
       Begsr BuildReturnJson;
        strStatus = GetSRReportStatus(dsSR.co#:dsSR.fnd:dsSr.emp#
         :dsSR.div:dsSR.fyr:dsSR.period);
        json += '{' +
         BuildJsonNode('ordinal':%char(dsSR.period)) + ',' +
         BuildJsonNode('period':strPeriod) + ',' +
         BuildJsonNode('billed':%editc(decBilled:'J')) + ',' +
         BuildJsonNode('paid':%editc(decPaid:'J')) + ',' +
         BuildJsonNode('status':%trim(strStatus)) +
         '},';
       Endsr;

       //========================================================
       // Builds Return JSON {employer info}
       //========================================================
       Begsr BuildReturnJsonEmp;
        json = %subst(json:1:%len(json)-1);
        Select;
        When %subst(json:%len(json):1) = '}';
           json = json + ']}]},';
        Endsl;
        json += '{' +
         BuildJsonNode('company':%char(dsSR.co#)) + ',' +
         BuildJsonNode('group':%char(dsSR.fnd)) + ',' +
         BuildJsonNode('employer':%char(dsSR.emp#)) + ',' +
         BuildJsonNode('division':%char(dsSR.div)) + ',' +
         '"years": [ ';
        Exsr BuildReturnJsonFyr;
       Endsr;

       //========================================================
       // Builds Return JSON {year info}
       //========================================================
       Begsr BuildReturnJsonFyr;
        json = %subst(json:1:%len(json)-1);
        Select;
        When %subst(json:%len(json):1) = '}';
           json = json + ']},';
        Endsl;
        json += '{' +
         BuildJsonNode('year':%char(dsSR.fyr)) + ',' +
         BuildJsonNode('type':%trim(dsSR.periodType)) + ',' +
         '"periods": [';
       Endsr;

       //==================================================
       // Build SQLs
       //==================================================
       Begsr BuildSQL;
        If ioRtnCode = 'Error';
           Leavesr;
        Endif;
        strSqlWhere = ' where' +
         ' SRco# = ' + dsIn.co# + ' and SRfnd = ' + dsIn.group +
         ' and SRemp# = ' + dsIn.emp#;
        If dsIn.div <> *blank;
           strSqlWhere += ' and SRdiv = ' + dsIn.div;
        Endif;
        If dsIn.fyr <> *blank;
           strSqlWhere += ' and SRfyr = ' + dsIn.fyr;
        Endif;
        strSqlOrder = ' order by SRCO#, SRFND, SREMP#, SRDIV, SRFYR, ' +
         'Period desc';
        strSqlSelect = ' select distinct SRCO#, SRFND, SREMP#, SRDIV, SRFYR,' +
         ' SRSTAT,' +
         '  case when hex(SRINV#) = ''404040404040404040404040404040'' then 0'+
         '   else SRINV# end as SRINV#,' +
         '  case when SRTODT <> 0 then ''Other''' +
         '       else ''Monthly''' +
         '  end as Period_Type,' +
         '  case when SRTODT <> 0 then SRTODT' +
         '       else SRRPMY' +
         '  end as Period';
        strSql = ' with' +
         ' submitted as (' + strSqlSelect + ',' +
         '  ''Submitted'' as SR_status' +
         '  from WASRP ' + strSqlWhere + strSqlOrder + ')' +
         ',' +
         ' pending as (' + strSqlSelect + ',' +
         '  ''Pending'' as SR_status' +
         '  from WASRP_HLD ' + strSqlWhere + strSqlOrder + ')';
        strSqlWhere = ' where ' +
         ' (Period_Type = ''Other'' and ' +
         '  Period between ' + %char(dt7From) + ' and ' + %char(dt7To) + ') ' +
         'or ' +
         ' (Period_Type = ''Monthly'' and ' +
         '  Period between ' + %char(dt5From) + ' and ' + %char(dt5To) + ') ';
        strSql += ' select *' +
         ' from submitted' +
         '  union select *' +
         ' from pending' +
         strSqlOrder;
        Exec SQL close cursorSRPayroll;
        Exec SQL prepare sqlSRPayroll from :strSql;
        Exec SQL declare cursorSRPayroll cursor for sqlSRPayroll;
        Exec SQL open cursorSRPayroll;
       Endsr;

       //========================================================
       // Get A/R Information {Billed/Paid)
       //========================================================
       Begsr GetARInfo;
        Reset decBilled;
        Reset decPaid;
        strCodeBill = GetARCode('billSR':dsSR.co#:dsSR.fnd:dsSR.fyr);
        strCodePayment = GetARCode('paymentSR':dsSR.co#:dsSR.fnd:dsSR.fyr);
        Select;
        When dsSR.periodType = 'Monthly';
           dt7 = DateTo7(GetLastDay(CvtToDate(dsSR.period * 100 + 1)));
           strPeriod = Alpha6USA(Dt6MDYY(dsSR.period));
        Other;
           dt7 = dsSR.period;
           strPeriod = AlphaUSA(CvtToDate(DtMDYY(dsSR.period)));
        Endsl;
        decBilled = AccumulateAR(strCodeBill:dt7:dt7:
         dsSR.co#:dsSR.fnd:dsSR.fyr:dsSR.emp#:dsSR.div);
        decPaid = AccumulateAR(strCodePayment:dt7:dt7:
         dsSR.co#:dsSR.fnd:dsSR.fyr:dsSR.emp#:dsSR.div);
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
                 dsIn.emp# = YAJL_get_string(val);
              When key = 'division';
                 dsIn.div = YAJL_get_string(val);
              When key = 'year';
                 dsIn.fyr = YAJL_get_string(val);
              When key = 'dateRange';
                 Exsr GetParmsDateRange;
              Endsl;
           Enddo;
           Yajl_Tree_Free(node0);
           json = json + '}';
           Monitor;
              intCo# = %int(dsIn.co#);
           On-error;
           Endmon;
           dsIn.co# = %char(intCo#);
           Exsr ValidateParms;
        Else;
           AddErrorMessageJson('JSON filter can''t be read.':ioRtnCode);
        Endif;
       Endsr;

       //========================================================
       // Break parms out of JSON - Date Range
       //========================================================
       Begsr GetParmsDateRange;
        Reset i1;
        node1 = yajl_object_find(node0:key);
        json = %scanrpl('""':'{':json:%len(json)-1);
        Dow YAJL_object_loop(node1:i1:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'from';
              dsIn.dateFrom = YAJL_get_string(val);
           When key = 'to';
              dsIn.dateTo = YAJL_get_string(val);
           Endsl;
        Enddo;
        json = json + '}';
       Endsr;

       //========================================================
       // Read through SQL recrods
       //========================================================
       Begsr ProcessSQL;
        If ioRtnCode = 'Error';
           Leavesr;
        Endif;
        json += ',"SRPayrollPeriodsList":[ ';
        strStatus = 'Pending';  // When records exist in holding file
        strStatus = 'Submitted';
        xxCmpDt = Dateto7(%date - %months(1));
        Reset dsSR;
        Reset isEofSRPeriodList;
        Dou isEofSRPeriodList;
           If isProcessed;
              dsSRSv = dsSR;
              Reset isProcessed;
           Endif;
           Exec SQL fetch next from cursorSRPayroll into :dsSR :srNull;
           Exsr GetARInfo;
           Select;
           When not IsValidFetch(sqlStt);
              isEofSRPeriodList = *on;
              Iter;
           When dt7From > dt7;
              Iter;
           When dt7 > dt7To;
              Iter;
           When dsSR.co# <> dsSRSv.co#
            or dsSR.fnd <> dsSRSv.fnd
            or dsSR.emp# <> dsSRSv.emp#
            or dsSR.div <> dsSRSv.div;
              Exsr BuildReturnJsonEmp;
           When dsSR.fyr <> dsSRSv.fyr;
              Exsr BuildReturnJsonFyr;
           Endsl;
           Select;
           Endsl;
           Exsr BuildReturnJson;
           isProcessed = *on;
        Enddo;
        Select;
        When %subst(json:%len(json):1) = ',';
           json = %subst(json:1:%len(json)-1);
           json += ']}]}]';
        Other;
           json += ']';
        Endsl;
       Endsr;

       //========================================================
       // Validate parms
       //========================================================
       Begsr ValidateParms;
        Monitor;
           intFnd = %int(dsIn.group);
        On-error;
        Endmon;
        If intFnd <> *zero and not UseGroupInPortal(intCo#:intFnd:'S/R Online');
           AddErrorMessageJson('Group ' + %char(intFnd) +
            ' is not included for service requested ' +
            '(' + %trim(inRequest) + ').':ioRtnCode);
        Endif;
        // From date
        If dsIn.dateFrom <> *blank and dsIn.dateFrom <> *zeros;
           If DtCheck(%int(dsIn.dateFrom)) = *zero;
              AddErrorMessageJson('Incoming from date not valid.'
               :ioRtnCode);
           Else;
              dt7From = DtCYMD(%int(dsIn.dateFrom));
           Endif;
        Else;
           dt7From = DateTo7(GetFirstDay(SystemDate()-%years(1)));
        Endif;
        dt5From = Dt5CYMD(Dt8to6(DtMDYY(dt7From)));
        // To date
        If dsIn.dateTo <> *blank and dsIn.dateTo <> *zeros;
           If DtCheck(%int(dsIn.dateTo)) = *zero;
              AddErrorMessageJson('Incoming to date not valid.'
               :ioRtnCode);
           Else;
              dt7To = DtCYMD(%int(dsIn.dateTo));
              dt5To = Dt5CYMD(Dt8to6(%int(dsIn.dateTo)));
           Endif;
        Endif;
       Endsr;
     pListSRPayrollPeriods...
     p                 e

      *=========================================================
      * Get the current payroll period
      *=========================================================
     pLoadCurrentPeriod...
     p                 b
     dLoadCurrentPeriod...
     d                 pi
     d inCo#                          3  0 const
     d inFnd                          3  0 const
     d inFyr                          3  0 const
     d inEmp#                         9  0 const
     d inDiv                          5  0 const
     d inPeriod                       8  0 const options(*nopass)
     d inPeriodFmt                   10    const options(*nopass) varying       *FILE/*DISPLAY

     dintDiff          s             10i 0 inz
     dintDOW           s             10i 0 inz
     dintDOWCurrent    s             10i 0 inz
     dintPeriod        s              8  0 inz
     dstrPeriodFmt     s             10    inz varying

     ddsWTBSP        e ds                  extname(WTBSP) qualified inz
      /Free
       If %parms >= 6;
          intPeriod = inPeriod;
       Endif;
       If %parms >= 7;
          strPeriodFmt = inPeriodFmt;
       Endif;
       // BSFREQ, BSDOWK
       Exec sql
        select * into :dsWTBSP
         from WTBSP
          where BSco# = :inCo# and
           BSkey = (select ELBSCD
                     from WDELP
                      where ELco# = :inCo# and ELfnd = :inFnd
                       and ELemp# = :inEmp# and ELdiv = :inDiv
                       and ELfyr = :inFyr);
       Select;
       When dsWTBSP.BSfreq = 'M';
          dsBillingInfo.periodType = 'Monthly';
       Other;
          dsBillingInfo.periodType = 'Other';
       Endsl;
       //Load dsBillingInfo.period in CYYMM or CYYMMDD format
       If intPeriod = *zero;
          Exsr GetCurrentPeriod;
       Endif;
       Select;
       When Upper(strPeriodFmt) = '*DISPLAY';
          // Period needs to be in *FILE format
          Select;
          When Upper(dsBillingInfo.periodType) = 'MONTHLY';
             intPeriod = Dt5CYMD(Dt8to6(intPeriod));
          Other;
             intPeriod = DtCYMD(intPeriod);
          Endsl;
       Endsl;
       dsBillingInfo.period = intPeriod;
       Select;
       When Upper(dsBillingInfo.periodType) = 'MONTHLY';
          dsBillingInfo.periodDtFrom =
           DtCYMD(Dt6To8(Dt6MDYY(dsBillingInfo.period)));
          dsBillingInfo.periodDtTo =
           DateTo7(GetLastDay(CvtToDate(dsBillingInfo.periodDtFrom)));
          dsBillingInfo.periodDisplay = Dt6MDYY(dsBillingInfo.period);
          dsBillingInfo.periodAlpha = Alpha6USA(dsBillingInfo.period);
       When Upper(dsBillingInfo.periodType) <> 'MONTHLY';
          dsBillingInfo.periodDtTo = dsBillingInfo.period;
          dsBillingInfo.periodDtFrom =
           DateTo7(CvtToDate(dsBillingInfo.period)-%days(7));
          dsBillingInfo.periodDisplay = DtMDYY(dsBillingInfo.period);
          dsBillingInfo.periodAlpha = AlphaUSA(CvtToDate(dsBillingInfo.period));
       Endsl;
       dsBillingInfo.periodAlpha = %trim(dsBillingInfo.periodAlpha);
       //----------------
       Begsr GetCurrentPeriod;
        Select;
        When dsWTBSP.BSfreq = 'M';
           intPeriod=Dt5CYMD(Dt8To6(DateTo8(GetLastDay(%date()-%months(1)))));
        Other;
           intDOW = DOWnum(dsWTBSP.BSdoWk);
           intDOWCurrent = DOW(DateTo8(%date()));
           intDiff = intDOWCurrent - ((7+intDOWCurrent)-(intDOW-intDOWCurrent));
           Select;
           When intDOWCurrent = 7;
              intDiff += 7;
           When intDOWCurrent > intDOW;
              intDiff += 7;
           Endsl;
           intPeriod = DateTo7(%date() + %days(intDiff));
        Endsl;
        strPeriodFmt = '*FILE';
       Endsr;
      /End-free
     pLoadCurrentPeriod...
     p                 e
      *=========================================================
      * Retrieve doc path for reports previously printed
      *=========================================================
     pRtvWorksheetPath...
     p                 b

     pRtvWorksheetPath...
     p                 e
      *=========================================================
      * Request: PrintSRWorksheet
      * Description: Returns the location of the S/R Worksheet(s)
      *=========================================================
     pPrintSRWorksheet...
     p                 b

     d charFreq        s              1a
     d charOnline      s              1a
     d datePrvMthBeg   s               d
     d datePrvMthEnd   s               d
     d decPeriod       s              8  0 inz
     d dt5             s              5  0 inz
     d dt6             s              6  0 inz
     d dt7             s              7  0 inz
     d dt7From         s              7  0 inz
     d dt7InRptDate    s              7  0 inz
     d dt7PrvMthEnd    s              7  0 inz
     d dt7To           s              7  0 inz
     d dt8From         s              8  0 inz
     d dt8To           s              8  0 inz(*hival)
     d intDiv          s              5  0 inz
     d intEmp#         s              9  0 inz
     d intGrp#         s              3  0 inz
     d intSRCnt        s              5  0 inz
     d intYear         s              3  0 inz
     d isEofClassList  s              1n
     d isPendingSubmission...
     d                 s               n   inz(*off)
     d isStub          s              1n   inz(*off)
     d kkRtCd          s              3    inz
     d kkSt            s              2    inz
     d hazardGroup     s              5    inz
     d contClass       s              1    inz
     d**** Beg Chg **** 06/17/24 *******************************************
     d***classRate       s              7s 2 inz
     d classRate       s              8s 3 inz
     d**** End Chg **** 06/17/24 *******************************************
     d outCompleteForm...
     d                 s              1a   inz('P')
     d outKey          s             60a   inz('SRWORKSHEET')
     d outType         s             15a   inz
     d rrnWDELP        s              9  0 inz
     d strClassDesc    s             40a   varying inz
     d strCodeBill     s              2a   inz
     d strCodePayment  s              2a   inz
     d strOutComplete  s              1a   inz
     d strDocPath      s            250a   varying inz
     d strFile         s             25a   varying inz
     d strMthIntDartCode...
     d                 s              5a
     d strPeriod       s             10a   varying inz
     d strPrintAll     s              1a   varying inz('N')
     d strSql          s           5000a   varying inz
     d strSqlOrder     s            500a   varying inz
     d strSqlSelect    s           1000a   varying inz
     d strSqlWhere     s           5000a   varying inz
     d strStatus       s             25a   varying inz
     d workDate        s               d   inz

     d dsIn            ds                  Qualified Inz
     d  type                         25a   varying
     d  co#                           3a
     d  group                         3a
     d  emp#                          9a
     d  div                           5a
     d  fyr                           3a
     d  period                       10a   varying
     d  periodFrom                   10a   varying
     d  periodTo                     10a   varying

      /Free
       Exsr GetParms;
       If isStub;
          Exsr BuildStub;
       Endif;
       Exsr ProcessDocument;
       json += ProcessErrorMessagesJson('*PRE');
       json = %scanrpl(',':'':json:1:1);
       outJSON = json;
       Return;

       //=================================================================
       // Build stub results
       //=================================================================
       Begsr BuildStub;
        If json <> *blanks;
           json += ',';
        Endif;
        json +=
         %trim(BuildJsonNode('stub':%trim(inRequest) + ' (' +
         %trim(q1Pgm) + ') successfully executed. {PrintSRWorksheet}'));
       Monitor;
          strCmd = 'CRTDTAARA DTAARA(QTEMP/W7221R) TYPE(*CHAR) LEN(250)';
          RunCmd(strCmd:%len(strCmd));
       On-error;
       Endmon;
       strCmd = 'CHGDTAARA DTAARA(QTEMP/W7221R *ALL)' +
        ' VALUE(''Stub Path Name'')';
       RunCmd(strCmd:%len(strCmd));
       Endsr;

       //========================================================
       // Break parms out of JSON
       //========================================================
       Begsr GetParms;
        inJson = inParms;
        Reset i0;
        node0 = yajl_string_load_tree(inJson:strErrorMsg);
        If strErrorMsg = ' ';
           json += ' "requestParms" : {';
           Dow YAJL_object_loop(node0:i0:key:val);
              BuildGiantJSON(json:key:YAJL_get_string(val));
              Select;
              When key = 'company';
                 dsIn.co# = YAJL_get_string(val);
              When key = 'group';
                 dsIn.group = YAJL_get_string(val);
              When key = 'employer';
                 dsIn.emp# = YAJL_get_string(val);
              When key = 'division';
                 dsIn.div = YAJL_get_string(val);
              When key = 'year';
                 dsIn.fyr = YAJL_get_string(val);
              When key = 'periodInfo';
                 Exsr GetParmsPeriodInfo;
              Endsl;
           Enddo;
           Yajl_Tree_Free(node0);
           json += '}';
           Monitor;
              intCo# = %int(dsIn.co#);
           On-error;
           Endmon;
           dsIn.co# = %char(intCo#);
           Exsr ValidateParms;
        Else;
           AddErrorMessageJson('JSON filter can''t be read.':ioRtnCode);
        Endif;
       Endsr;

       //========================================================
       // Break parms out of JSON - Period Info
       //========================================================
       Begsr GetParmsPeriodInfo;
        Reset i1;
        node1 = yajl_object_find(node0:key);
        json = %scanrpl('""':'{':json:%len(json)-1);
        Dow YAJL_object_loop(node1:i1:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'period';
              dsIn.period = YAJL_get_string(val);
           When key = 'range';
              Exsr GetParmsPeriodRange;
           Endsl;
        Enddo;
        json = json + '}';
        Select;
        When (%len(dsIn.period) < 7 and Dt6Check(%int(dsIn.period)) = *zero)
         or (%len(dsIn.period) >= 7 and DtCheck(%int(dsIn.period)) = *zero);
           AddErrorMessageJson('Incoming period not valid.':ioRtnCode);
        When %len(dsIn.period) < 7;
           dt5 = Dt5CYMD(%int(dsIn.period));
        Other;
           dt7 = DtCYMD(%int(dsIn.period));
           dt5 = dt7/100;
        Endsl;
       Endsr;

       //========================================================
       // Break parms out of JSON - Period Range
       //========================================================
       Begsr GetParmsPeriodRange;
        Reset i2;
        node2 = yajl_object_find(node1:key);
        json = %scanrpl('""':'{':json:%len(json)-1);
        Dow YAJL_object_loop(node2:i2:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'from';
              dsIn.periodFrom = YAJL_get_string(val);
           When key = 'to';
              dsIn.periodTo = YAJL_get_string(val);
           Endsl;
        Enddo;
        json = json + '}';
      // GW said that printing all worksheets won't be an option
      // from Connect. See DE717 documentation.
        If dsIn.periodFrom <> *blank and dsIn.periodFrom <> *zeros;
           AddErrorMessageJson('The S/R worksheet can not print' +
            ' for a date range.':ioRtnCode);
        Endif;
       Endsr;

       //========================================================
       // Reset cursor
       //========================================================
       Begsr ProcessDocument;
        If ioRtnCode = 'Error';
           Leavesr;
        Endif;
        Select;
        When isPendingSubmission = *On;
           dt8To = %Dec(datePrvMthEnd:*Usa);
           dt7To = %Dec(datePrvMthEnd:*Cymd);
           datePrvMthBeg = datePrvMthEnd + %Days(1) - %Months(1);
           dt8From = %Dec(datePrvMthBeg:*Usa);
           dt5 = dt7To/100;
           dt6 = Dt6MDYY(dt5);
        Other;
           dt7From = dt5 * 100 + 01;
           dt7To = DateTo7(GetLastDay(CvtToDate(dt7From)));
           dt6     = Dt6MDYY(dt5);
           dt8From = DtMDYY(dt7From);
           dt8To   = DtMDYY(dt7To);
        Endsl;
        Select;
        When isStub;
           json += ',"documentParms":{';
           BuildGiantJson(json:'reportingMMCCYY':%char(dt6));
           BuildGiantJson(json:'dateFrom':%char(dt8From):'date');
           BuildGiantJson(json:'dateTo':%char(dt8To):'date');
           BuildGiantJson(json:'printAll':strPrintAll);
           json += '}';
        When isPendingSubmission = *On;
           PrintWorkSheet(
            %editc(%dec(dsIn.co#:3:0):'X'):
            %editc(%dec(dsIn.group:3:0):'Z'):
            %editc(%dec(dsIn.emp#:9:0):'X'):
            RightJstfy(dsIn.div:5):
            %editc(%dec(dsIn.fyr:3:0):'X'):
            %editc(dt6:'X'):
            'D':
            %editc(DateTo8(SystemDate):'X'):
            '000000000':
            ' ':
            '00000':
            '99999':
            %editc(dt8From:'X'):
            %editc(dt8To:'X'):
            '000000000':
            '999999999':
            'Y':
            'Y':
            ' ':
            strPrintAll:
            outCompleteForm
            );
           If ioRtnCode <> 'Error';
              In W7221R;
              strDocPath = W7221R.path;
           Endif;
        Other;
           outType = 'SRWORKSHEET';
           outKey =
            %Editc(intCo#:'X') +
            %Editc(intGrp#:'X') +
            %Editc(intEmp#:'X') +
            %Editc(intDiv:'X') +
            %Editc(intYear:'X') +
            %Editc(dt7to:'X');
           strDocPath = RtvDocPath(outType:outKey);
        Endsl;
        If strDocPath <> *Blanks;
           json += ',"documentInfo":{ ' +
            BuildJsonNode('documentPath':strDocPath) +
            '}';
        Else;
           AddErrorMessageJson('Document information cannot be retrieved.':
            ioRtnCode);
        Endif;
       Endsr;

       //========================================================
       // Validate parms
       //========================================================
       Begsr ValidateParms;
        Monitor;
           intGrp# = %int(dsIn.group);
        On-error;
        Endmon;
        If intGrp# <> 0 and not UseGroupInPortal(intCo#:intGrp#:'S/R Online');
           AddErrorMessageJson('Group ' + %char(intGrp#) +
            ' is not included for service requested ' +
            '(' + %trim(inRequest) + ').':ioRtnCode);
        Endif;
        Monitor;
           intEmp# = %int(dsIn.emp#);
        On-error;
           AddErrorMessageJson('Incoming employer not valid.':ioRtnCode);
        Endmon;
        Monitor;
           intDiv = %int(dsIn.div);
        On-error;
           AddErrorMessageJson('Incoming division not valid.':ioRtnCode);
        Endmon;
        Monitor;
           intYear = %int(dsIn.fyr);
        On-error;
           AddErrorMessageJson('Incoming year not valid.':ioRtnCode);
        Endmon;

        If ioRtnCode <> 'Error';
           If ValidEmpYr(intYear:intEmp#:intDiv:rrnWDELP:intGrp#:intCo#) = *off;
              AddErrorMessageJson('Invalid Policy.':ioRtnCode);
              LeaveSr;
           EndIf;
        EndIf;
       // Verify policy is eligible for a worksheet. Must be S/R Online and
       //  monthly reporting.
        Exec Sql
         Select BSfreq, BSonli
         Into :charFreq, :charOnline
         From WDELP Join WTBSP
         On ELco# = BSco# And
            ELbscd = BSkey
        Where ELco#  = :intCo#
          And ELfnd  = :intGrp#
          And ELemp# = :intEmp#
          And ELdiv  = :intDiv
          And ELfyr  = :intYear;
        If Not(isValidFetch(sqlStt));
           AddErrorMessageJson('SQL: Fetch for billing plan failed.':
            ioRtnCode);
           LeaveSr;
        Else;
           If charFreq = 'W' Or charOnline <> 'Y';
              AddErrorMessageJson('Policy is not set for ' + conSRBranding +
               'or is not monthly reporting.':ioRtnCode);
              LeaveSr;
           Endif;
        Endif;
       // Can't print a worksheet without a DARTS code.
        Exec Sql
         Select EXmicd
         Into :strMthIntDartCode
         From WMEXP
         Where EXco# = :dsin.co#
          And EXfnd = :dsin.group;
        If strMthIntDartCode = *Blanks;
           AddErrorMessageJson('Worksheet can not be printed.':
            ioRtnCode);
        EndIf;
       // Check if policy is in a pending status.
       //  Must have records in WASRP_HLD for previous period.
       //  Records in WASRP_HLD can't be for 3rd Party Payment
       //   awaiting response from Invoice Cloud (SRinv# <> 0)
        datePrvMthEnd = %Date() - %Days(%Subdt(%Date():*Days));
        dt7PrvMthEnd = %Dec(datePrvMthEnd:*Cymd);
        dt7InRptDate = DateTo7(GetLastDay(CvtToDate(dt5 * 100 + 01)));
        If dt7PrvMthEnd = dt7InRptDate;
           Exec Sql
            Select Count(*)
            Into :intSrCnt
            From WASRP_HLD
            Where SRco# = :intCo#
             And SRfnd  = :intGrp#
             And SRemp# = :intEmp#
             And SRdiv  = :intDiv
             And SRfyr  = :intYear
             And SRrpmy = :dt5
             And SRinv# = 0;
           If intSrCnt <> 0;
              isPendingSubmission = *On;
           EndIf;
        Endif;
       Endsr;
     pPrintSRWorksheet...
     p                 e
      *=========================================================
      * Called from PrintSRWorksheet and ProcessSRPayments
      * Calls to W7221C
      *=========================================================
     pPrintWorksheet   b
     dPrintWorksheet   pi
     d inCo#                          3a   const
     d inFnd                          3a   const
     d inEmp#                         9a   const
     d inDiv                          5a   const
     d inFyr                          5a   const
     d outRpYM                        6a   const
     d outMode                        1a   const
     d outRunDt                       8a   const
     d outDue                         9a   const
     d outSort                        1a   const
     d outAgtFr                       5a   const
     d outAgtTo                       5a   const
     d outDtFr                        8a   const
     d outDtTo                        8a   const
     d outEmpFr                       9a   const
     d outEmpTo                       9a   const
     d outPrmM                        1a   const
     d outDarts                       1a   const
     d outPrinDp                      1a   const
     d outAll                         1a   const
     d inFormComp                     1a   const

     d chrNoCtgy       c                   const('N001')
     d chrNoType       s             10a
     d intCo#          s              3  0
     d intFnd          s              3  0
     d intEmp#         s              9  0
     d intDiv          s              5  0
     d intFyr          s              3  0
     d intRRNWdelp     s              9  0
     d isUseScript     s              1n   inz
     d message         s            250    inz(*blanks)
     d outQueue        s             10
     d processType     c                   const('Self-Reporting Worksheet')
     d string          s             10    inz(*blanks)
     d xxMass          s              1
     d xxPre           s              2    inz('MI')
     d xxProcType      s            100
     d xxSubmit        s              1n

     d StLetter        pr                  ExtPgm('STLETTER')
     d   letMode_                    10    const
     d   xmlFile_                    60
     d   message_                   250    options(*nopass)
     d   mode_                        1    const options(*nopass)
     d   rrn_                         9    const options(*nopass)
     d   prefix_                      2    const options(*nopass)
     d   string_                     10    const options(*nopass)
     d   co#_                         3    const options(*nopass)
     d   fnd_                         3    const options(*nopass)
     d   mass_                        1    const options(*nopass)
     d   link_                       60    const options(*nopass)

      /Free
       isUseScript = UseScriptura(processType);
       If isUseScript = *Off;
          AddErrorMessageJson(processType + ' must use Scriptura output.'
           :ioRtnCode);
          Return;
       Endif;
       // Get rrn of WDELP record
       intCo# = %Int(inCo#);
       intFnd = %Int(inFnd);
       intEmp# = %Int(inEmp#);
       intDiv = %Int(inDiv);
       intFyr = %Int(inFyr);
       Exec Sql
        Select Int(RRN(WDELP))
        Into :intRRNWdelp
       From WDELP
       Where ELco#  = :intCo#
        And  ELfnd  = :intFnd
        And  ELemp# = :intEmp#
        And  ELdiv  = :intDiv
        And  ELfyr  = :intFyr;
       If Not IsValidFetch(SqlStt);
          Return;
       Endif;
       // Pre-populate *LDA
       In svLDA;
       Clear svLDA;
       Select;
       When inFormComp = 'S';
          chrNoType = '3RDPTYCONF';
       When inFormComp = 'E';
          chrNoType = 'EFTPAYCONF';
       When inFormComp = 'P';
          ChrNoType = *Blanks;
       EndSl;
       If chrNoType <> *Blanks;
          Exec Sql
           Select NOdart
           Into :xxRmCd
           From WINOTIP
           Where NOctgy = :chrNoCtgy
            And NOtype = :chrNoType
            And NOstat = 'A';
       Endif;
       If xxRmCd <> *Blanks;
          xxDart = 'Y';
       Else;
          xxDart = 'N';
          xxRmCd = *Blanks;
       Endif;
       // Notify worksheet of the year to process
       Monitor;
          strCmd = 'CRTDTAARA DTAARA(QTEMP/W7221R) TYPE(*CHAR) LEN(250)';
          RunCmd(strCmd:%len(strCmd));
       On-error;
       Endmon;
       Monitor;
          In *Lock W7221R;
          Reset W7221R;
          W7221R.fyr = %char(intFyr);
          Out W7221R;
       On-error;
       Endmon;
       Out svLDA;
       // Populate *LDA
       xxProcType = processType;
       message = %trim(xxProcType);
       string = *Blank;
       xxMass = *Blank;
       // Populate LDA for process
       xxSubmit = OutputWindow(xxProcType:xxMass:'Y':'M');
       // Call S/R Worksheet Override
       OutQueue = GetUsrOutq(inUser);
       W7221C
        (
        inCo#:
        inFnd:
        outRpYM:
        outQueue:
        outMode:
        inEmp#:
        inDiv:
        outRunDt:
        outDue:
        outSort:
        outAgtFr:
        outAgtTo:
        outDtFr:
        outDtTo:
        outEmpFr:
        outEmpTo:
        outPrmM:
        outDarts:
        outPrinDp:
        outAll:
        inFormComp:
        inUser
        );

      /End-Free
     pPrintWorksheet   e
      *=========================================================
      * ProcessSRPayments
      *  If 3rdParty
      *   Lock Reporting Period
      *  If EFT
      *   Lock Reporting Period (if flagged to do so)
      *   Copy files from HOLD to LIVE
      *    WDEAP_HLD to WDEAP
      *    WASRP_HLD to WASRP
      * Request JSON:
      *  status (3RD PARTY/SUCCESS/FAILURE/' ')
      *    '3RD PARTY' writes to WDEAP_HLD and locks WASRP_HLD
      *    ' ' is EFT
      *    'SUCCESS' transfers from WASRP_HLD and WDEAP_HLD to live
      *    'FAILURE' unlocks WASRP_HLD and deletes WDEAP_HLD
      *  company
      *  group
      *  employer
      *  div
      *  year
      *  reportDate (MMDDYYYY)
      *  invoiceNumber
      *   'SUCCESS' and 'FAILURE' just pass in the invoiceNumber
      *   The policy info is pulled from it.
      *=========================================================
      /Free
       Dcl-Proc ProcessSRPayments;
       Dcl-S charLock Char(1) Inz('L');
       Dcl-S charUnlock Char(1) Inz(' ');
       Dcl-S charStatus Char(1) Inz(' ');
       Dcl-Ds dsIn Qualified Inz;
        status varchar(25);
        invoiceNumber Char(30);
         co# Char(3) Overlay(invoiceNumber);
         fnd Char(3) Overlay(invoiceNumber:4);
         emp# Char(9) Overlay(invoiceNumber:7);
         div Char(5) Overlay(invoiceNumber:16);
         sequence# Char(10) Overlay(invoiceNumber:21);
        fyr Char(3);
        reportDate Char(8);
        reference# Char(40);    // 10/13/22
       End-Ds;
       Dcl-S charFreq Char(1);
       Dcl-S charMode Char(13);
       Dcl-S charRPMY Char(5);
       Dcl-S chrSrOn Char(1) Inz('P');
       Dcl-S charToDate7 Char(7);
       Dcl-S charPreventResubmit Char(1);
       Dcl-S codeEmailReturn Char(10);
       Dcl-S dt6 Zoned(6:0);
       Dcl-S dt7From Zoned(7:0);
       Dcl-S dt7To Zoned(7:0);
       Dcl-S dt8From Zoned(8:0);
       Dcl-S dt8To Zoned(8:0);
       Dcl-S errMsg VarChar(200);
       Dcl-S isSelfReportTransaction Ind;
       Dcl-S intAdld Packed(11:0);
       Dcl-S intCalcPrem Packed(11:0);
       Dcl-S intCalcTax Packed(11:0);
       Dcl-S intDiv Packed(5:0);
       Dcl-S intEmp Packed(9:0);
       Dcl-S intFyr Packed(3:0);
       Dcl-S intGrp Packed(3:0);
       Dcl-S intDate5 Zoned(5:0);
       Dcl-S intDate6 Zoned(6:0);
       Dcl-S intDate7 Zoned(7:0);
       Dcl-S intDate8 Zoned(8:0);
       Dcl-S intInv# Packed(10:0);
       Dcl-S zonedInv# Zoned(10:0);
       Dcl-S intTotalManPrem Packed(11:0);
       Dcl-S intTotalPayroll Packed(11:0);
       Dcl-S intPremiumBilled Packed(11:0);
       Dcl-S intPrPrem Packed(11:0);
       Dcl-S intPrTax Packed(11:0);
       Dcl-S intPrvBal Packed(11:2);
       Dcl-S intReportDate5 Packed(5:0);
       Dcl-S intReportDate6 Packed(6:0);
       Dcl-S intReportDate7 Packed(7:0);
       Dcl-S intReportDate8 Packed(8:0);
       Dcl-S intTaxBilled Packed(11:0);
       Dcl-S intTotalPayment Packed(11:2);
       Dcl-S jsonEmailRequest VarChar(2000000) Inz(' ');
       Dcl-S jsonEmailReturn VarChar(2000000) Inz(' ');
       Dcl-S q1Fact Char(143);
       Dcl-S rrnWDELP Packed(9:0);
       Dcl-S strCompleteForm Char(1);
       Dcl-S strDocPath Char(250);
       Dcl-S strNotificationType VarChar(10);
       Dcl-S strOutRequest Char(256);
       Dcl-S strPaymentType VarChar(10);
       Dcl-S strPrintAll Char(1);
       Dcl-S workDate Date;
       Dcl-S saveInv# Packed(10:0) inz(0);
       Dcl-S chrReference# Char(40);
       Exsr GetParms;
       Exsr ValidateParms;
       Select;
       When isSelfReportTransaction = *On;
          Exsr ProcessSelfReportPayment;
       Other;
          Exsr ProcessCloudPayment;
       EndSl;
       json += ProcessErrorMessagesJson('*PRE');
       outJSON = json;
       Return;
       //---------------------------------------------------
       Begsr CalculateSRPrem;
       //---------------------------------------------------
        If ioRtnCode = 'Error';
           LeaveSr;
        Endif;
       // Manual Premium was written to WASRP_HLD
       //  in CalculateSRBalance
        Exec Sql
         Select Sum(SRamtp), Sum(SRmanprm)
         Into :intTotalPayroll, :intTotalManPrem
         From WASRP_HLD
         Where SRco# = :dsIn.co#
          And SRfnd = :dsIn.fnd
          And SRemp# = :dsIn.emp#
          And SRdiv = :dsIn.div
          And SRfyr = :dsIn.fyr
          And SRrpmy = :charRPMY
          And SRtodt = :charToDate7;
       // Run the calc again in case things have changed
       //  since CalculateSRBalance() was run.
        CalcSrPrem(rrnWDELP:
         intReportDate6:
         intReportDate8:
         intTotalManPrem:
         intAdLd:
         intTotalPayroll:
         intCalcPrem:
         intCalcTax:
         intPrPrem:
         intPrTax:
         q1Fact:
         intPrvBal:
         chrSrOn);
        intPremiumBilled = intCalcPrem - intPrPrem;
        intTaxBilled = intCalcTax - intPrTax;
        intTotalPayment = intPremiumBilled + intTaxBilled + intPrvBal;
       EndSr;
       //---------------------------------------------------
       Begsr ResetWASRP;
       //---------------------------------------------------
        Exec Sql
         Update WASRP_HLD
         Set SRinv# = 0, SRstat = ' '
         Where SRco# = :intCo#
          And SRfnd =  :intGrp
          And SRemp# = :intEmp
          And SRdiv = :intDiv
          And SRfyr = :intFyr
          And SRinv# = :zonedInv#;
       Endsr;
       //---------------------------------------------------
       Begsr CreateEmail;
       //---------------------------------------------------
        strOutRequest = 'ImmediateSRNotifications';
        Select;
        When Upper(dsIn.status) = 'SUCCESS';
           strNotificationType = '3RDPTYCONF';
        When Upper(dsIn.status) = 'FAILURE';
           strNotificationType = '3RDPTYFAIL';
        When strPaymentType = 'EFT';
           strNotificationType = 'EFTPAYCONF';
        When strPaymentType = '3RD PARTY';
           strNotificationType = '3RDPTYPEND';
        Endsl;
        Monitor;
           In W7221R;
           If W7221R.path = 'No output generated.';
              strDocPath = *Blanks;
           Else;
              strDocPath = W7221R.path;
           EndIf;
        On-Error;
           strDocPath = *Blanks;
        Endmon;
        jsonEmailRequest = '{'  +
         '"notificationCategory":"N001",' +
         '"notificationType":"' + %Trim(strNotificationType) + '",' +
         '"company":"' + %Trim(dsIn.co#) + '",' +
         '"group":"' + %Trim(dsIn.fnd) + '",' +
         '"employer":"' + %Trim(dsIn.emp#) + '",' +
         '"division":"' + %Trim(dsIn.div) + '",' +
         '"year":"' + %Trim(dsIn.fyr) + '",' +
         '"reportDate":"' + %Trim(dsIn.reportDate) + '",' +
         '"attachment":"' + %Trim(strDocPath) + '"' +
         '}';
        ProcessEmails(strOutRequest:inUser:jsonEmailRequest:
         jsonEmailReturn:codeEmailReturn);
        ioRtnCode = codeEmailReturn;
       EndSr;
       //---------------------------------------------------
       Begsr CreatePayments;
       //---------------------------------------------------
        inParms = '{' +
         '"status":"' + %Trim(strPaymentType) + '",' +
         '"company":"' + %Trim(dsIn.co#) + '",' +
         '"group":"' + %Trim(dsIn.fnd) + '",' +
         '"employer":"' + %Trim(dsIn.emp#) + '",' +
         '"division":"' + %Trim(dsIn.div) + '",' +
         '"year":"' + %Trim(dsIn.fyr) + '",' +
         '"reportDate":"' + dsIn.reportDate + '",' +
         BuildJsonNode('invoice#':%editc(saveInv#:'X')) + ',' +
         '"srBatches":[' +
          '{' +
          '"code":"' + 'PB' + '",' +
          '"amount":"' + %Trim(%EditC(intPremiumBilled:'P')) + '",' +
          '"effectiveDate":"' + dsIn.reportDate + '"' +
          '},' +
          '{' +
          '"code":"' + 'PP' + '",' +
          '"amount":"' + %Trim(%EditC(intTotalPayment:'P')) + '",' +
          '"effectiveDate":"' + dsIn.reportDate + '"' +
          '}';
        If intTaxBilled <> 0;
          inParms += ',{' +
          '"code":"' + 'TB' + '",' +
          '"amount":"' + %Trim(%EditC(intTaxBilled:'P')) + '",' +
          '"effectiveDate":"' + dsIn.reportDate + '"' +
          '}';
        Endif;
        inParms += ']}';
        // Create SRBatches will update ioRtnCode = 'Error'
        //  if it doesn't complete normally.
        CreateSrBatches();
       Endsr;
       //---------------------------------------------------
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
              When key = 'status';
                 dsIn.status = YAJL_get_string(val);
              When key = 'company';
                 dsIn.co# = YAJL_get_string(val);
              When key = 'group';
                 dsIn.fnd = YAJL_get_string(val);
              When key = 'employer';
                 dsIn.emp# = YAJL_get_string(val);
              When key = 'division';
                 dsIn.div = YAJL_get_string(val);
              When key = 'year';
                 dsIn.fyr = YAJL_get_string(val);
              When key = 'reportDate';
                 dsIn.reportDate = YAJL_get_string(val);
              When key = 'invoiceNumber';
                 dsIn.invoiceNumber = YAJL_get_string(val);
                 Exsr PopulateDsIn;
              When key = 'altRequest';
                 strOutRequest = YAJL_get_string(val);
              When key = 'referenceNumber';
                 dsIn.reference#  = YAJL_get_string(val);
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
        Select;
        When Upper(dsIn.status) = 'SUCCESS';
           strOutRequest = 'TransferPayment';
        Endsl;
       Endsr;
       //==================================================
       // Populate dsIn from invoice number if needed
       //==================================================
       Begsr PopulateDsIn;
        intCo# = %Int(dsIn.co#);
        intGrp = %Int(dsIn.fnd);
        intEmp = %Int(dsIn.emp#);
        intDiv = %Int(dsIn.div);
        intInv# = %Int(dsIn.sequence#);
        zonedInv# = %Int(dsIn.sequence#);
        // Get the policy year
        Exec Sql
         Select Max(EAfyr)
         Into :intFyr
         From WDEAP_HLD
         Where EAco# = :intCo#
          And EAfnd  = :intGrp
          And EAemp# = :intEmp
          And EArdiv  = :intDiv
          And EAinv# = :intInv#;
        If Not IsValidFetch(sqlStt);
           AddErrorMessageJson('Policy can not be found from invoice number.':
            ioRtnCode);
           LeaveSr;
        Endif;
        dsin.Fyr = %Editc(intFyr:'X');
       Endsr;
       //==================================================
       // Print S/R Worksheet
       //==================================================
       Begsr PrintSRInvoice;
       // S/R Worksheet can't handle weekly reporting yet.
        If charFreq = 'W';
           LeaveSr;
        Endif;
        dt7From = intReportDate5 * 100 + 1;
        dt7To = intReportDate7;
        dt6     = Dt6MDYY(intReportDate5);
        dt8From = DtMDYY(dt7From);
        dt8To   = DtMDYY(dt7To);
        Monitor;
           PrintWorksheet(
            %editc(%dec(dsIn.co#:3:0):'X'):
            %editc(%dec(dsIn.fnd:3:0):'Z'):
            %editc(%dec(dsIn.emp#:9:0):'X'):
            RightJstfy(dsIn.div:5):
            %editc(%dec(dsIn.fyr:3:0):'Z'):
            %editc(dt6:'X'):
            'D':
            %editc(DateTo8(SystemDate):'X'):
            '000000000':
            ' ':
            '00000':
            '99999':
            %editc(dt8From:'X'):
            %editc(dt8To:'X'):
            '000000000':
            '999999999':
            'Y':
            'Y':
            ' ':
            strPrintAll:
            strCompleteForm
            );
           If ioRtnCode <> 'Error';
              In W7221R;
              json = %Trim(json) + ',"documentInfo":{ ' +
               BuildJsonNode('documentPath':%trim(W7221R.path)) +
               '}';
           Endif;
        On-error;
           AddErrorMessageJson('Document information cannot be retrieved.':
            ioRtnCode);
        Endmon;
       Endsr;
       //==================================================
       // Process Cloud Payments
       //==================================================
       Begsr ProcessCloudPayment;
       If ioRtnCode = 'Error';
          LeaveSr;
       Endif;
        Select;
        When Upper(dsIn.status) = 'SUCCESS';
           Transfer3rdPartyBatch(%Int(dsIn.co#):%Int(dsIn.fnd):
            %Int(dsIn.emp#):%Int(dsIn.div):%Int(dsIn.sequence#));
        When Upper(dsIn.status) = 'FAILURE';
           Delete3rdPartyBatch(%Int(dsIn.co#):%Int(dsIn.fnd):
            %Int(dsIn.emp#):%Int(dsIn.div):%Int(dsIn.sequence#));
        Endsl;
       Endsr;
       //==================================================
       // Process Self Report Payments
       //==================================================
       Begsr ProcessSelfReportPayment;
       If ioRtnCode = 'Error';
          LeaveSr;
       Endif;
       charStatus = charUnlock;
       Exec Sql
        Select N1LckRsbmt
         Into :charPreventResubmit
         From WINOTIP1
        Where N1co# = :dsIn.co#;
       If charPreventResubmit = 'Y';
          charStatus = charLock;
       Endif;
       Select;
       When Upper(dsIn.status) = 'SUCCESS';
          If Not UpdatePayrollStatus(intCo#:intGrp:intFyr:
           intEmp:intDiv:charRPMY:charToDate7:charStatus);
             AddErrorMessageJson('SQL: UpdatePayrollStatus Failed':ioRtnCode);
             LeaveSr;
          Endif;
       //*** begin del ***  03/12/24  ****************************************
         // Passing an 'S' in this parm notifies W7221R that this is a
         //  3rd Party Success request.
       //****   strCompleteForm = 'S';
       //****   Exsr PrintSrInvoice;
       //****   If ioRtnCode = 'Error';
       //****      LeaveSr;
       //****   Endif;
       //*** end del *****  03/12/24  ****************************************
          If Not TransferPayroll(intCo#:intGrp:intFyr:
           intEmp:intDiv:charRPMY:charToDate7);
             AddErrorMessageJson('SQL: Transfer Payroll Failed':ioRtnCode);
             LeaveSr;
          Endif;
          chrReference# = dsIn.reference#;
          Exec Sql
            Update WDEAP_HLD Set EAref# = :chrReference#
            Where  EAco#  = :intCo#        and
                   EAfnd  = :intGrp        and
                   EAemp# = :intEmp        and
                   EAdiv  = :intDiv        and
                   EAinv# = :intInv#;
          Transfer3rdPartyBatch(intCo#:intGrp:intEmp:intDiv:intInv#);
          If ioRtnCode = 'Error';
             AddErrorMessageJson('SQL: Transfer3rdPartyBatches Failed':
              ioRtnCode);
             LeaveSr;
          Endif;
       //*** begin add ***  03/12/24  ****************************************
         // Passing an 'S' in this parm notifies W7221R that this is a
         //  3rd Party Success request.
          strCompleteForm = 'S';
          Exsr PrintSrInvoice;
          If ioRtnCode = 'Error';
             LeaveSr;
          Endif;
       //*** end add *****  03/12/24  ****************************************
          Exsr CreateEmail;
       When Upper(dsIn.status) = 'FAILURE';
          // Clear PrInt Worksheet data area if it exists.
          Monitor;
             In *Lock W7221R;
             W7221R.Path = *Blanks;
             Out W7221R;
          On-Error;
          Endmon;
          If Not UpdatePayrollStatus(intCo#:intGrp:intFyr:
           intEmp:intDiv:charRPMY:charToDate7:charStatus);
             AddErrorMessageJson('SQL: UpdatePayrollStatus Failed':ioRtnCode);
             LeaveSr;
          Endif;
          Delete3rdPartyBatch(intCo#:intGrp:intEmp:intDiv:intInv#);
          Exsr ResetWASRP;
          Exsr CreateEmail;
       When strPaymentType = '3RD PARTY';
       //*** begin add ***  03/12/24  ****************************************
       //*
       //* Note - it appears the coding in this section never gets called/executed.
       //* The intTotalPayment field is total billed figure sent to CreateSRBatches
       //* and does not represent a period being paid off, so this coding likely
       //* never executed.  If a S/R is paid off it calls this procedure from
       //* WIPOLPMT with SUCCESS instead of being called directly as webservice.
       //*
       //*** end add *****  03/12/24  ****************************************
          // Clear PrInt Worksheet data area if it exists.
          Monitor;
             In *Lock W7221R;
             W7221R.Path = *Blanks;
             Out W7221R;
          On-Error;
          Endmon;
          If UpdatePayrollStatus(intCo#:intGrp:intFyr:
           intEmp:intDiv:charRPMY:charToDate7:charStatus) = *Off;
             AddErrorMessageJson('SQL: UpdatePayrollStatus Failed':ioRtnCode);
             LeaveSr;
          Endif;
          //* Remove any WDEAP_HLD records as they will be recreated.
          If saveInv# <> 0;
             Delete3rdPartyBatch(intCo#:intGrp:intEmp:intDiv:saveInv#);
          Endif;
          Exsr CalculateSrPrem;
          Exsr CreatePayments;
          If ioRtnCode = 'Error';
             LeaveSr;
          Endif;
          // Transfer payrolls and prInt invoice if no payment is due
       //*** begin del ***  03/12/24  ****************************************
          //*** Begin Und *** 09/04/24 *************************************
          If intTotalPayment <= 0;
          //*** End   Und *** 09/04/24 *************************************
       //*** end del *****  03/12/24  ****************************************
             strCompleteForm = 'S';
             Exsr PrintSrInvoice;
             If Not TransferPayroll(intCo#:intGrp:intFyr:
              intEmp:intDiv:charRPMY:charToDate7);
                AddErrorMessageJson('SQL: Transfer Payroll Failed':ioRtnCode);
                LeaveSr;
             Endif;
             Exec Sql
              Select
              Max(case when hex(SRINV#)='404040404040404040404040404040' then 0
                   else SRINV# end)
              Into :intInv#
              From WASRP
               Where SRco# = :intCo#  And SRfnd = :intGrp
                And SRemp# = :intEmp And SRdiv = :intDiv
                And SRfyr  = :intFyr  And SRtodt = :charToDate7
                And SRrpmy = :charRPMY;
             Transfer3rdPartyBatch(intCo#:intGrp:intEmp:intDiv:intInv#);
             If ioRtnCode = 'Error';
                AddErrorMessageJson('SQL: Transfer3rdPartyBatches Failed':
                 ioRtnCode);
                LeaveSr;
             Endif;
             //*** Begin Add *** 09/04/24 **********************************
             Exsr CreateEmail;
             //*** End   Add *** 09/04/24 **********************************
       //*** begin chg ***  03/12/24  ****************************************
          //*** Begin Und *** 09/04/24 *************************************
          Endif;
          //*** End   Und *** 09/04/24 *************************************
          If ioRtnCode = 'Error';
             LeaveSr;
          Endif;
       //*** end chg *****  03/12/24  ****************************************
       //*** Begin Del *** 09/04/24 ****************************************
       //*Exsr CreateEmail;
       //*** End   Del *** 09/04/24 ****************************************
       Other;
          If Not UpdatePayrollStatus(intCo#:intGrp:intFyr:
           intEmp:intDiv:charRPMY:charToDate7:charStatus);
             AddErrorMessageJson('SQL: UpdatePayrollStatus Failed':ioRtnCode);
             LeaveSr;
          Endif;
          Exsr CalculateSrPrem;
       //*** begin del ***  03/12/24  ****************************************
         // Passing an 'E' in this parm notifies W7221R that this is a
         //  EFT Payment request.
       //****   strCompleteForm = 'E';
       //****   Exsr PrintSrInvoice;
       //*** end del *****  03/12/24  ****************************************
          If Not TransferPayroll(intCo#:intGrp:intFyr:
           intEmp:intDiv:charRPMY:charToDate7);
             AddErrorMessageJson('SQL: Transfer Payroll Failed':ioRtnCode);
             LeaveSr;
          Endif;
          Exsr CreatePayments;
          If ioRtnCode = 'Error';
             AddErrorMessageJson('SQL: CreateSRBatches Failed':ioRtnCode);
             LeaveSr;
          Endif;
       //*** begin add ***  03/12/24  ****************************************
         // Passing an 'E' in this parm notifies W7221R that this is a
         //  EFT Payment request.
          strCompleteForm = 'E';
          Exsr PrintSrInvoice;
          If ioRtnCode = 'Error';
             LeaveSr;
          Endif;
       //*** end add *****  03/12/24  ****************************************
          Exsr CreateEmail;
       EndSl;
       Endsr;
       //---------------------------------------------------
       Begsr ValidateParms;
       //---------------------------------------------------
        Monitor;
           intCo# = %Int(dsIn.co#);
        On-error;
           intCo# = UserCo#(inUser);
        Endmon;
        If intCo# = 0;
           AddErrorMessageJson('Company invalid.':ioRtnCode);
        Endif;
        If ioRtnCode = 'Error';
           LeaveSr;
        Endif;
       // Group # must be valid numeric value
        Monitor;
           intGrp = %Int(dsIn.fnd);
        On-error;
           AddErrorMessageJson('Group value is not numeric.':ioRtnCode);
        Endmon;
        If intGrp <> *zero and not UseGroupInPortal(intCo#:intGrp:'S/R Online');
           AddErrorMessageJson('Group ' + %char(intGrp) +
            ' is not included for service requested ' +
            '(' + %trim(inRequest) + ').':ioRtnCode);
        Endif;
       // Employer # must be valid numeric value
        Monitor;
           intEmp = %Int(dsIn.emp#);
        On-error;
           AddErrorMessageJson('Employer value is not numeric.':ioRtnCode);
        Endmon;
       // Division must be valid numeric value
        Monitor;
           intDiv = %Int(dsIn.div);
        On-error;
           AddErrorMessageJson('Division value is not numeric.':ioRtnCode);
        Endmon;
       // Year must be a valid numeric value
        Monitor;
           intFyr = %Int(dsIn.fyr);
        On-error;
           AddErrorMessageJson('Year value is not numeric.':ioRtnCode);
        Endmon;
       // Policy must be valid
        If ioRtnCode <> 'Error';
           If ValidEmpYr(intFyr:intEmp:intDiv:rrnWDELP:
                         intGrp:intCo#) = *Off;
              AddErrorMessageJson('Policy cannot be found.':ioRtnCode);
              LeaveSr;
           Endif;
        Endif;
        strPaymentType = GetPaymentType(intCo#:intGrp:intEmp:intDiv);
        If strPaymentType = *Blanks And ioRtnCode <> 'Error';
           AddErrorMessageJson('Policy not set up for 3RD PARTY or EFT.':
            ioRtnCode);
        Endif;
        If ioRtnCode <> 'Error';
           isSelfReportTransaction =
            HasSelfReport(intCo#:intGrp:intEmp:intDiv:intFyr:intInv#:saveInv#:
            dsIn.ReportDate:intReportDate5:intReportDate7);
        EndIf;
        If isSelfReportTransaction = *On;
           Exec Sql
            Select * Into :dsWINOTIP1
            From WINOTIP1
            Where N1co# = :intCo#;
         //// Check policy cancellation status against S/R flags
          // Validate the report date and populate the date variables. Report date
          //  is passed in for EFT and 3rd Party Request. HasSelfReport retrieves
          //  SRrpmy (intReportDate5) or SRtodt (intReportDate7) depending if monthly
          //  or weekly reporting for a 3rd Party SUCCESS message.
           If dsIn.reportDate = *Blanks;
              Select;
              When intReportDate7 <> 0;
                 intReportDate5 = intReportDate7/100;
                 intReportDate6 = Dt6MDYY(intReportDate5);
                 intReportDate8 = DtMDYY(intReportDate7);
              When intReportDate5 <> 0;
                 intReportDate6 = Dt6MDYY(intReportDate5);
                 workDate = %Date(intReportDate5 * 100 + 1:*Cymd);  //Set to 1st day of rpt mth
                 workDate = workDate + %Months(1) - %Days(1); //Find last day of mth
                 intReportDate7 = DateTo7(workDate);
                 intReportDate8 = DateTo8(workDate);
              EndSl;
              dsIn.reportDate = %EditC(intReportDate8:'X');
           Else;
              Monitor;
                 intReportDate8 = %Int(dsIn.reportDate);
                 intReportDate7 = DtCYMD(intReportDate8);
                 intReportDate5 = intReportDate7/100;
                 intReportDate6 = Dt6MDYY(intReportDate5);
              On-error;
                 AddErrorMessageJson('Report date is invalid.':ioRtnCode);
              Endmon;
           EndIf;
           // Check period status
           Select;
           When strOutRequest = *blanks;
              strOutRequest = inRequest;
           Endsl;
           If IsValidReportStatus(strOutRequest:intCo#:intGrp:intEmp:intDiv
            :intFyr:intReportDate7);
              ioRtnCode = 'Error';
           Endif;
           Clear strOutRequest;
           Exec Sql
            Select BSfreq
            Into :charFreq
            From WDELP Join WTBSP
            On ELco# = BSco# And
               ELbscd = BSkey
           Where ELco#  = :dsIn.co#
             And ELfnd  = :dsIn.fnd
             And ELemp# = :dsIn.emp#
             And ELdiv  = :dsIn.div
             And ELfyr  = :dsIn.fyr;
           If charFreq = 'W';
              charToDate7 = %EditC(intReportDate7:'X');
              charRPMY = *Zero;
           Else;
              charRPMY = %EditC(intReportDate5:'X');
              charToDate7 = *Zero;
           Endif;
        Endif;

       Endsr;
       End-Proc ProcessSRPayments;

      *=========================================================
      * Retrieve doc path from SMDPP
      *=========================================================
       Dcl-Proc RtvDocPath;
       Dcl-Pi *N Char(250);
        inType Char(15);
        inKey Char(60);
       End-Pi;
       Dcl-S strDocPath Char(250);
       Exec Sql
        Select DPpath
        Into :strDocPath
        From SMDPP
        Where DPtype = :inType
         And DPkey = :inKey;
       If IsValidFetch(sqlStt);
          Return strDocPath;
       Else;
          Return *Blanks;
       Endif;
       End-Proc RtvDocPath;
      /End-Free
      *=========================================================
      * Request: SaveSRPayrolls
      * Description: Save SR Payrolls
      *=========================================================
     pSaveSRPayrolls...
     p                 b

     d x1              s             10i 0 inz
     d x2              s             10i 0 inz
     d countPeriod     s             10i 0 inz
     d decCount        s             10i 0 inz
     d decPeriodSave   s              7  0 inz
     d rrnWDELP        s              9  0
     D totPremium      s             11  0 inz(0)
     d amount          s             11  2
     d date            s              8  0
     d date6           s              6  0
     d decDays         s              5  0 inz(7)
     d decimalError    s               n
     d dateError       s               n
     d dtCompareExp    s               d   inz
     d dtCompareRpt    s               d   inz
     d strPeriodSave   s              8    inz

     d intClas         s              5  0
     d intDiv          s              5  0
     d intEmp#         s              9  0
     d intGrp#         s              3  0
     d intYear         s              3  0

     d dsIn            ds                  Qualified Inz
     d  co#                           3a
     d  group                         3a
     d  emp#                          9a
     d  division                      5a
     d  year                          3a
     d  paymentType                  10a

     d dsPayroll       ds                  Qualified
     d  classCode                     5a
     d  extension                     5a
     d  specialDes                    1a
     d  effDate                       8a
     d  cxDate                        8a   inz('0')
     d  amount                       12a
     d  status                        1a
     d
     d dsPeriod        ds                  qualified dim(12) inz
     d  date                          8a   inz
     d  type                         25a   varying inz
     d  dt7                           7  0 inz
     d  payroll                            likeds(dsPayroll) dim(9999) inz
     d  countPayroll                 10i 0 inz

     D dsWASRP       e ds                  extname(WASRP) qualified inz
     D dsWASRPout    e ds                  extname(WASRP) qualified inz
     D dsWDELP       e ds                  extname(WDELP) qualified inz

      /Free

       Exsr GetParms;
       Exsr ValidateData;
       Exsr SavePayrolls;

       json += ProcessErrorMessagesJson('*PRE');
       json = %scanrpl(',':'':json:1:1);

       outJSON = json;

       Return;

       //========================================================
       // Break parms out of JSON
       //========================================================
       Begsr GetParms;

          inJson = inParms;
          Reset i0;

          node0 = yajl_string_load_tree(inJson:strErrorMsg);
          If strErrorMsg <> ' ';
             AddErrorMessageJson(strErrorMsg:ioRtnCode);
             Leavesr;
          Endif;

          json = json + ' "requestParms" : {';
          Dow YAJL_object_loop(node0:i0:key:val);
             BuildGiantJSON(json:key:YAJL_get_string(val));
             Select;
             When key = 'company#';
                dsIn.co# = YAJL_get_string(val);
             When key = 'group#';
                dsIn.group = YAJL_get_string(val);
             When key = 'employer#';
                dsIn.emp# = YAJL_get_string(val);
             When key = 'division';
                dsIn.division = YAJL_get_string(val);
             When key = 'year';
                dsIn.year = YAJL_get_string(val);
             When key = 'paymentType';
                dsIn.paymentType = YAJL_get_string(val);
             When key = 'periods';
                ExSr GetParmsPeriods;
             Endsl;

          Enddo;

          Yajl_Tree_Free(node0);
          json = json + '}';

        If strAuthority = '*View';
           AddErrorMessageJson('User, ' + %trim(inUser) +
            ', not authorized to process ' +
            'request (' + %trim(inRequest) + ').'
            :ioRtnCode);
        Endif;

       Endsr;

       //========================================================
       // Break class information parms out of JSON
       //========================================================
       Begsr GetParmsClass;
          json = %scanrpl('""':'{':json:%len(json)-1);
          node5 = YAJL_object_find(node4:key);
          Reset i5;
          Dow YAJL_object_loop(node5:i5:key:val);
             BuildGiantJSON(json:key:YAJL_get_string(val));
             Select;
             When key = 'amount';
                dsPeriod(i1).payroll(i3).amount = YAJL_get_string(val);
             When key = 'class';
                json = %scanrpl('""':'{':json:%len(json)-1);
                node6 = YAJL_object_find(node5:key);
                Reset i6;
                Dow YAJL_object_loop(node6:i6:key:val);
                   BuildGiantJSON(json:key:YAJL_get_string(val));
                   Select;
                   When key = 'code';
                      dsPeriod(i1).payroll(i3).classCode = YAJL_get_string(val);
                   When key = 'specialDesignation';
                      dsPeriod(i1).payroll(i3).specialDes =YAJL_get_string(val);
                   When key = 'extension';
                      dsPeriod(i1).payroll(i3).extension = YAJL_get_string(val);
                   When key = 'effectiveDate';
                      dsPeriod(i1).payroll(i3).effDate = YAJL_get_string(val);
                   When key = 'cancelDate';
                      dsPeriod(i1).payroll(i3).cxDate = YAJL_get_string(val);
                   When key = 'status';
                      dsPeriod(i1).payroll(i3).status = YAJL_get_string(val);
                   Endsl;
                Enddo;
                json += '}';
             Endsl;
          Enddo;
          json = %scanrpl(',':'':json:%len(json)-1);
          json += '}';
       Endsr;

       //========================================================
       // Break payroll information parms out of JSON
       //========================================================
       Begsr GetParmsPayroll;
          json = %scanrpl('""':'[':json:%len(json)-1);
          node3 = YAJL_object_find(node2:key);
          Reset i3;
          Dow YAJL_array_loop(node3:i3:node4);
             json += '{';
             Reset i4;
             Dow YAJL_object_loop(node4:i4:key:val);
                BuildGiantJSON(json:key:YAJL_get_string(val));
                Select;
                When key = 'classInfo';
                   Exsr GetParmsClass;
                Endsl;
             Enddo;
             json += '},';
          Enddo;
          json = %scanrpl(',':'':json:%len(json)-1);
          json += ']';
          dsPeriod(i1).countPayroll = i3 - 1;
       Endsr;

       //========================================================
       // Break period information parms out of JSON
       //========================================================
       Begsr GetParmsPeriods;
          json = %scanrpl('""':'[':json:%len(json)-1);
          Reset i1;
          node1 = YAJL_object_find(node0:key);
          Dow YAJL_array_loop(node1:i1:node2);
             json += '{';
             Reset i2;
             Dow YAJL_object_loop(node2:i2:key:val);
                BuildGiantJSON(json:key:YAJL_get_string(val));
                Select;
                When key = 'date';
                   dsPeriod(i1).date = YAJL_get_string(val);
                When key = 'type';
                   dsPeriod(i1).type = YAJL_get_string(val);
                When key = 'payrollClasses';
                   Exsr GetParmsPayroll;
                Endsl;
             Enddo;
             json += '},';
          Enddo;
          json = %scanrpl(',':'':json:%len(json)-1);
          json += ']';
          countPeriod = i1 - 1;
       Endsr;

       //========================================================
       // Save Payroll Data
       //========================================================
       Begsr SavePayrolls;
        If ioRtnCode = 'Error';
           Leavesr;
        Endif;

        For x1 = 1 to countPeriod;
           Reset dsWASRPout;
           dsWASRPout.SRco#  = intCo#;
           dsWASRPout.SRfnd  = intGrp#;
           dsWASRPout.SRfyr  = intYear;
           dsWASRPout.SRemp# = intEmp#;
           dsWASRPout.SRdiv  = intDiv;
           Select;
           When dsPeriod(x1).type = 'Monthly';
              dsWASRPout.SRrpMY = dsPeriod(x1).dt7;
           Other;
              dsWASRPout.SRtoDt = dsPeriod(x1).dt7;
           Endsl;
           For x2 = 1 to dsPeriod(x1).countPayroll;
              If dsPeriod(x1).payroll(x2).classCode = *blank;
                 Iter;
              Endif;
              dsPayroll = dsPeriod(x1).payroll(x2);
              dsWASRPout.SRspec = dsPayroll.specialDes;
              dsWASRPout.SRext  = dsPayroll.extension;
              dsWASRPout.SRclas = %int(dsPayroll.classCode);
              dsWASRPout.SRefDt = DtCYMD(%int(dsPayroll.effDate));
              dsWASRPout.SRcaDt = DtCYMD(%int(dsPayroll.cxDate));
              Exec SQL
               select * into :dsWASRPout From WASRP_HLD
                where SRco# = :dsWASRPout.SRco# and SRfnd = :dsWASRPout.SRfnd
                and SRemp# = :dsWASRPout.SRemp# and SRdiv = :dsWASRPout.SRdiv
                and SRfyr = :dsWASRPout.SRfyr and SRclas = :dsWASRPout.SRclas
                and SRext = :dsWASRPout.SRext and SRspec = :dsWASRPout.SRspec
                and SRrpMY = :dsWASRPout.SRrpMy and SRtoDt = :dsWASRPout.SRtoDt
                fetch first row only;
              dsWASRPout.SRamtP = %dec(dsPayroll.amount:11:2);
              dsWASRPout.SRuser = q1user;
              dsWASRPout.SRpgm  = q1pgm;
              dsWASRPout.SRdt   = sysDate7;
              dsWASRPout.SRtime = sysTime6;
              If IsValidFetch(sqlstt);
                 CalcManPrem(dsWASRPout);
                 Exec SQL
                  update WASRP_HLD
                   set row = :dsWASRPout
                   where SRco# = :dsWASRPout.SRco# and SRfnd = :dsWASRPout.SRfnd
                   and SRemp# = :dsWASRPout.SRemp# and SRdiv = :dsWASRPout.SRdiv
                   and SRfyr = :dsWASRPout.SRfyr and SRclas = :dsWASRPout.SRclas
                   and SRext = :dsWASRPout.SRext and SRspec = :dsWASRPout.SRspec
                   and SRrpMY = :dsWASRPout.SRrpMy
                   and SRtoDt = :dsWASRPout.SRtoDt;
              Else;
                 CalcManPrem(dsWASRPout);
                 dsWASRPout.SRiUsr = dsWASRPout.SRuser;
                 dsWASRPout.SRiPgm = dsWASRPout.SRpgm;
                 dsWASRPout.SRiDt  = dsWASRPout.SRdt;
                 dsWASRPout.SRiTm  = dsWASRPout.SRtime;
                 Exec SQL
                  insert into WASRP_HLD
                   values (:dsWASRPout);
              EndIf;
              If Not IsValidFetch(sqlStt);
                 AddErrorMessageJson('SQL - SaveSRPayrolls Failed.':ioRtnCode);
              Endif;
           EndFor;
        EndFor;

        If ioRtnCode <> 'Error';
           json += ', "results" :"SR Payrolls Saved"';
        Endif;

       Endsr;

       //=================================================================
       // Validate Incoming Data
       //=================================================================
       Begsr ValidateData;
        If ioRtnCode = 'Error';
           Leavesr;
        Endif;

          //--------------------------------------------------------------
          // Company # must be valid numeric value
          Monitor;
             intCo# = %int(dsIn.co#);
          On-error;
             // If not passed in, will be defaulted to user parms
          Endmon;

          Exec sql
           select * into :dsWINOTIP1
            from WINOTIP1
            where N1co# = :intCo#;

          //--------------------------------------------------------------
          // Group # must be valid numeric value
          Monitor;
             intGrp# = %int(dsIn.group);
          On-error;
             AddErrorMessageJson('Incoming group # not valid.':ioRtnCode);
          Endmon;
           If intGrp#<>0 and not UseGroupInPortal(intCo#:intGrp#:'S/R Online');
              AddErrorMessageJson('Group ' + %char(intGrp#) +
               ' is not included for service requested ' +
               '(' + %trim(inRequest) + ').':ioRtnCode);
           Endif;

          //--------------------------------------------------------------
          // Employer # must be valid numeric value
          Monitor;
             intEmp# = %int(dsIn.emp#);
          On-error;
             AddErrorMessageJson('Incoming employer not valid.':ioRtnCode);
          Endmon;

          //--------------------------------------------------------------
          // Division must be valid numeric value
          Monitor;
             intDiv = %int(dsIn.division);
          On-error;
             AddErrorMessageJson('Incoming division not valid.':ioRtnCode);
          Endmon;

          //--------------------------------------------------------------
          // Year must be a valid numeric value
          Monitor;
             intYear = %int(dsIn.year);
          On-error;
             AddErrorMessageJson('Incoming year not valid.':ioRtnCode);
          Endmon;

          //--------------------------------------------------------------
          // Policy must be valid
          If ioRtnCode <> 'Error';
             If ValidEmpYr(intYear:intEmp#:intDiv:rrnWDELP:
                           intGrp#:intCo#) = *Off;
                AddErrorMessageJson('Invalid Policy.':ioRtnCode);
             EndIf;
          EndIf;
          Reset dsWDELP;
          Exec sql
           select * into :dsWDELP
            from WDELP
            where rrn(WDELP) = :rrnWDELP;

        ////--------------------------------------------------------------
        //// Check policy cancellation status against S/R flags
        //
        ////--------------------------------------------------------------
        //// Check status of last period

          // Let's evaluate the payroll entries
          For x1 = 1 to countPeriod;
             If dsPeriod(x1).type = *blank;
                LoadCurrentPeriod(intCo#:intGrp#:intYear:intEmp#:intDiv:
                 %dec(dsPeriod(x1).date:8:0):'*DISPLAY');
                dsPeriod(x1).type = dsBillingInfo.periodType;
             Endif;
             //----------------------------------------------------------
             // Validate period date
             If dsPeriod(x1).date = ' ';
                AddErrorMessageJson('Report period date (entry:' +
                 %Char(x1) + ') must be entered.':ioRtnCode);
                Iter;
             Endif;
             Select;
             When Upper(dsPeriod(x1).type) = 'MONTHLY';
                date6     = 0;
                dateError = *Off;
                Monitor;
                   decimalError = *Off;
                   date6 = %Dec(dsPeriod(x1).date:6:0);
                   dtCompareRpt=GetLastDay(CvtToDate(Dt6To8(date6))+%months(1));
                   dtCompareExp =
                    GetLastDay(CvtToDate(dsWDELP.ELptDt)-%days(1)+%months(1));
                On-error;
                   AddErrorMessageJson('Report period date (entry:' +
                    %Char(x1) + ') contains invalid numeric data.':ioRtnCode);
                   decimalError = *On;
                   dateError = *On;
                Endmon;
                // Lets see if report month/year
                If Not decimalError And date6 <> 0;
                   dsPeriod(x1).dt7 = dt5CYMD(date6);
                   If dsPeriod(x1).dt7 = 0;
                      AddErrorMessageJson('Report period date (entry:' +
                       %Char(x1)+') is invalid for monthly report.':ioRtnCode);
                   EndIf;
                EndIf;
             Other;
                date = 0;
                Monitor;
                   decimalError = *Off;
                   date = %Dec(dsPeriod(x1).date:8:0);
                   decDays = 7;
                   dtCompareRpt = CvtToDate(date)+%days(decDays);
                   dtCompareExp = CvtToDate(dsWDELP.ELptDt)+%days(decDays-1);
                   // Allow for reporting after expiration date for the normal amount of time;
                   //  the policy could expire mid-period
                   If dtCompareRpt > dtCompareExp
                    and %diff(dtCompareRpt:dtCompareExp:*days) < decDays;
                      dtCompareExp = dtCompareRpt;
                   Endif;
                On-error;
                   AddErrorMessageJson('Report period date (entry:' +
                    %Char(x1) + ') contains invalid numeric data.':ioRtnCode);
                   dateError = *On;
                   decimalError = *On;
                Endmon;
                // Lets see if valid date
                If Not decimalError And date <> 0;
                   dsPeriod(x1).dt7 = dtCYMD(date);
                   If dsPeriod(x1).dt7 = 0;
                      AddErrorMessageJson('Report period date (entry:' +
                       %Char(x1) + ') is invalid for report.':ioRtnCode);
                   EndIf;
                EndIf;
             Endsl;
             // Check period status
             If decPeriodSave <> dsPeriod(x1).dt7;
                If IsValidReportStatus(inRequest:intCo#:intGrp#:intEmp#:intDiv
                 :intYear:dsPeriod(x1).dt7);
                   ioRtnCode = 'Error';
                   Leavesr;
                Endif;
                decPeriodSave = dsPeriod(x1).dt7;
             Endif;
             For x2 = 1 to dsPeriod(x1).countPayroll;
                If dsPeriod(x1).payroll(x2).cxDate = ' ';
                   dsPeriod(x1).payroll(x2).cxDate = '0';
                Endif;
                dsPayroll = dsPeriod(x1).payroll(x2);
                //----------------------------------------------------------
                // Class Code is required
                If dsPayroll.classCode = *blank;
                   AddErrorMessageJson('Period: ' + %trim(dsPeriod(x1).date)+
                    ' - Class Code (entry:' + %Char(x2) +
                    ') cannot be blank.':ioRtnCode);
                Else;
                   Monitor;
                      intClas = %Dec(dsPayroll.classCode:5:0);
                   On-error;
                      AddErrorMessageJson('Period: ' + %trim(dsPeriod(x1).date)+
                      ' - Payroll Amount(entry:' + %Char(x2) +
                       ') contains invalid numeric data.':ioRtnCode);
                   Endmon;
                EndIf;
                //----------------------------------------------------------
                // Payroll Effective Date is required
                If dsPayroll.effDate = ' ';
                   AddErrorMessageJson('Period: ' + %trim(dsPeriod(x1).date)+
                    ' - Payroll Effective Date(entry:' +
                    %Char(x2) + ') cannot be blank.':ioRtnCode);
                Else;
                   Monitor;
                      decimalError = *Off;
                      date = %Dec(dsPayroll.effDate:8:0);
                   On-error;
                      AddErrorMessageJson('Period: ' + %trim(dsPeriod(x1).date)+
                       ' - Payroll Effective Date(entry:' +
                       %Char(x2)+') contains invalid numeric data.':ioRtnCode);
                      decimalError = *On;
                   Endmon;
                   // Lets see if valid date
                   If Not decimalError;
                      If dtCYMD(date) = 0;
                         AddErrorMessageJson('Period: ' +
                          %trim(dsPeriod(x1).date)+
                          ' - Payoll Effective Date(entry:' +
                          %Char(x2) + ') is invalid.':ioRtnCode);
                      EndIf;
                   EndIf;
                EndIf;
                //----------------------------------------------------------
                // Payroll Cancel Date must be valid, if entered
                If dsPayroll.cxDate <> '0';
                   Monitor;
                      decimalError = *Off;
                      date = %Dec(dsPayroll.cxDate:8:0);
                   On-error;
                      AddErrorMessageJson('Period: ' + %trim(dsPeriod(x1).date)+
                       ' - Payroll Cancel Date(entry:' +
                       %Char(x2)+') contains invalid numeric data.':ioRtnCode);
                      decimalError = *On;
                   Endmon;
                   // Lets see if valid date
                   If Not decimalError;
                      If date <> 0 and dtCYMD(date) = 0;
                         AddErrorMessageJson('Period: ' +
                          %trim(dsPeriod(x1).date)+
                          ' - Payoll Cancel Date(entry:' +
                          %Char(x2) + ') is invalid.':ioRtnCode);
                      EndIf;
                   EndIf;
                EndIf;
                //----------------------------------------------------------
                // Payroll amount is required
                If dsPayroll.amount = ' ';
                   AddErrorMessageJson('Period: ' + %trim(dsPeriod(x1).date)+
                   ' - Payroll Amount(entry:' + %Char(x2) +
                    ') cannot be blank.':ioRtnCode);
                Else;
                   Monitor;
                      amount = %Dec(dsPayroll.amount:11:2);
                   On-error;
                      AddErrorMessageJson('Period: ' + %trim(dsPeriod(x1).date)+
                      ' - Payroll Amount(entry:' + %Char(x2) +
                       ') contains invalid numeric data.':ioRtnCode);
                   Endmon;
                EndIf;
             EndFor;
          EndFor;

       Endsr;

     pSaveSRPayrolls...
     p                 e

      *=========================================================
      * Transfer S/R payroll from holding file to permanent storage
      * - Requires Co#/Fnd/Fyr/Emp#/Div and RPMY or ToDt
      *=========================================================
     pTransferPayroll  b
     dTransferPayroll  pi             1n
     d inCo#                          3  0 Const
     d inFnd                          3  0 Const
     d inFyr                          3  0 Const
     d inEmp#                         9  0 Const
     d inDiv                          5  0 Const
     d inRPMY                         5    Const
     d inToDt                         7    Const

      /Free
       // Delete current records if existing
        Exec Sql
         Delete From WASRP
          Where SRco# = :inCo# And SRfnd = :inFnd
           And SRemp# = :inEmp# And SRdiv = :inDiv
           And SRfyr = :inFyr And SRtodt = :inToDt
           And SRrpmy = :inRPMY;

        // Copy SR Payroll Records from hold to live
        Exec SQL
        Insert into WASRP
         (Select * from WASRP_HLD
           Where SRco# = :inCo# And SRfnd = :inFnd And
            SRemp# = :inEmp# And SRdiv = :inDiv And
            SRfYr = :inFyr And SRtodt = :inToDt And
            SRrpmy = :inRPMY);

        If not IsValidFetch(sqlstt);
           AddErrorMessageJson('SQL error encountered on inserting WASRP ' +
            'records. SqlStt:' + SqlStt + ' SqlCode: ' + %EditC(SqlCode:'X'):
            ioRtnCode);
           Return *off;
        Endif;

        // Delete SR Payroll Records in the hold file.
        Exec SQL
         Delete From WASRP_HLD
          Where SRco# = :inCo# And SRfnd = :inFnd
           And SRemp# = :inEmp# And SRdiv = :inDiv
           And SRfyr = :inFyr And SRtodt = :inToDt
           And SRrpmy = :inRPMY;

        If not IsValidFetch(sqlstt);
           AddErrorMessageJson('SQL error encountered on deleting WASRP_HLD ' +
            'records. SqlStt:' + SqlStt + ' SqlCode: ' + %EditC(SqlCode:'X'):
            ioRtnCode);
           Return *off;
        Endif;

       // Update total self reported payroll in WMEPP
       Exec Sql
        Update WMEPP
         Set EPSAPR =
          (Select Sum(SRamtp)
           From WASRP
           Where EPco# = SRco#
            And EPfnd = SRfnd
            And EPemp# = SRemp#
            And EPdiv = SRdiv
            And EPfyr = SRfyr
            And EPclas = SRclas
            And EPspec = SRspec
            And EPext = SRext
            And EPefdt = SRefdt)
         Where EPco# = :inCo#
          And EPfnd = :inFnd
          And EPemp# = :inEmp#
          And EPdiv = :inDiv
          And EPfyr = :inFyr
          And Exists(Select *
           From WASRP
           Where EPco# = SRco#
            And EPfnd = SRfnd
            And EPemp# = SRemp#
            And EPdiv = SRdiv
            And EPfyr = SRfyr
            And EPclas = SRclas
            And EPspec = SRspec
            And EPext = SRext
            And EPefdt = SRefdt);

        If not IsValidFetch(sqlstt);
           AddErrorMessageJson('SQL error encountered updating WMEPP ' +
            'records. SqlStt:' + SqlStt + ' SqlCode: ' + %EditC(SqlCode:'X'):
            ioRtnCode);
           Return *Off;
        Endif;

        Return *On;
      /End-Free
     pTransferPayroll  e
      /Free
       //=========================================================
       // Transfers 3RD PARTY A/R Batch records from holding file
       //  to permanent storage
       //========================================================
       Dcl-Proc Transfer3rdPartyBatch;
       Dcl-Pi *N;
        inCo# Packed(3:0) Const;
        inFnd Packed(3:0) Const;
        inEmp Packed(9:0) Const;
        inDiv Packed(5:0) Const;
        inInv Packed(10:0) Const;
       End-Pi;
       Dcl-S chrAchF Char(2);
       Dcl-S chrARType Char(1);
       Dcl-S intBatch# Packed(9:0);
       Dcl-S intCo# Packed(3:0);
       Dcl-S intFnd Packed(3:0);
       Dcl-S intBillingBatch Packed(9:0);
       Dcl-S intPaymentBatch Packed(9:0);
       Dcl-S intBillingHoldBatch Packed(9:0);
       Dcl-S intPaymentHoldBatch Packed(9:0);
       Dcl-S rrnWDEAP Packed(9:0);
       Dcl-S payAR Char(1);
       Dcl-S saveBatch Packed(9:0);
       // Convert to variables that can pass by reference
       intCo# = inCo#;
       intFnd = inFnd;
       // Find the billing and payment batch numbers
       //  for the policy in WDEAP_HLD
       Exec Sql
        Declare e30 Cursor For
        Select Distinct EAbat#, EAachF
        From WDEAP_HLD
        Where EAco# = :inCo#
         And EAfnd  = :inFnd
         And EAemp# = :inEmp
         And EArdiv = :inDiv
         And EAinv# = :inInv
        Order by EAbat# Desc;
       Exec Sql
        Open e30;
       Dou Not IsValidFetch(sqlStt);
          Clear intBatch#;
          Clear chrAchF;
          Exec Sql
           Fetch Next
           From e30
           Into :intBatch#,:chrAchF;
          Select;
          When chrAchF = 'T' And intPaymentHoldBatch = 0;
             intPaymentHoldBatch = intBatch#;
          When chrAchF = 'U' And intBillingHoldBatch = 0;
             intBillingHoldBatch = intBatch#;
          EndSl;
       Enddo;
       Exec Sql
        Close e30;
       If intPaymentHoldBatch = 0 And intBillingHoldBatch = 0;
          AddErrorMessageJson('No records exist in WDEAP_HLD to transfer.':
           ioRtnCode);
          Return;
       Endif;

       // Are we moving batch to live
       Exec Sql Select EXicP2Gar Into :payAR From WMEXP
                Where EXco#=:inCo# And EXfnd=:inFnd;

       // Write to Live or Live Batch
       If payAR = 'Y';
          ExSr WriteToLiveAR;
       Else;
          ExSr WriteToBatchAR;
       EndIf;

       //===================================================================
       // WriteToBatchAR - Write from WDEAP_HLD to WDEAP
       //===================================================================
       BegSr WriteToBatchAR;

          // Find existing 3RD Party Batches in WDEAP
          //  created on system date
          Exec Sql
           Declare e31 Cursor For
           Select Distinct EAbat#, EAachf
           From WDEAP
           Where EAstat = ' '
            And EAachf In ('T','U')
            And EAidt = :sysDate7
            And EAOnli = 'Y'
            Order by EAbat# Desc;
          Exec Sql
           Open e31;
          Dou Not IsValidFetch(sqlStt);
             Clear intBatch#;
             Clear chrAchF;
             Exec Sql
              Fetch Next
              From e31
              Into :intBatch#, :chrAchF;
             Select;
             When chrAchF = 'T' And intPaymentBatch = 0;
                intPaymentBatch = intBatch#;
             When chrAchF = 'U' And intBillingBatch = 0;
                intBillingBatch = intBatch#;
             EndSl;
          Enddo;
          Exec Sql
           Close e31;
          Exec Sql
           Select Max(EAbat#)
           Into :saveBatch
           From WDEAP;

          // Billing:
          If intBillingHoldBatch <> 0;
             // Get next available batch # if billing batch not found
             If intBillingBatch = 0;
                If saveBatch = 0;
                   saveBatch = 1;
                   intBillingBatch = 1;
                Else;
                   saveBatch += 1;
                   intBillingBatch = saveBatch;
                Endif;
             Endif;
             //  Update batch# and maint fields in billing hold batch
             //  to live batch # before copying
             Exec Sql
              Update WDEAP_HLD
              Set EAbat# = :intBillingBatch,
               EApgm = :Q1pgm,
               EAuser = :inUser,
               EAtime = :sysTime6,
               EAdt = :sysDate7
              Where EAco# = :inCo#
               And EAfnd  = :inFnd
               And EAemp# = :inEmp
               And EArdiv = :inDiv
               And EAinv# = :inInv
               And EAbat# = :intBillingHoldBatch;
             If Not IsValidFetch(sqlStt);
                AddErrorMessageJson('SQL: Update to WDEAP_HLD failed.':
                 ioRtnCode);
                Return;
             Endif;
             // Copy from HOLD to LIVE
             Exec Sql
              Insert Into WDEAP
              Select * from WDEAP_HLD
              Where EAco# = :inCo#
               And EAfnd  = :inFnd
               And EAemp# = :inEmp
               And EArdiv = :inDiv
               And EAinv# = :inInv
               And EAbat# = :intBillingBatch;
             If Not IsValidFetch(sqlStt);
                AddErrorMessageJson('SQL: Insert to WDEAP failed.':
                ioRtnCode);
                Return;
             Endif;
             // Update batch total in WDEAP
             Exec Sql
              Update WDEAP
              Set EAbata =
               (Select Sum(EAamt)
                 From WDEAP
                 Where EAbat# = :intBillingBatch)
                 Where EAbat# = :intBillingBatch;
             If Not IsValidFetch(sqlStt);
                AddErrorMessageJson('SQL: Update total in WDEAP failed.':
                ioRtnCode);
                Return;
             Endif;
             // Delete billings from WDEAP_HLD
             Exec Sql
             Delete From WDEAP_HLD
              Where EAco# = :inCo#
               And EAfnd  = :inFnd
               And EAemp# = :inEmp
               And EArdiv = :inDiv
               And EAinv# = :inInv
               And EAbat# = :intBillingBatch;
          Endif;

          // Payments:
          //  Return if no payments in WDEAP_HLD.
          If intPaymentHoldBatch = 0;
             Return;
          Endif;
          //  Get next available batch # if payment batch not found.
          If intPaymentBatch = 0;
              If saveBatch = 0;
                 intPaymentBatch = 1;
              Else;
                 intPaymentBatch = saveBatch + 1;
              Endif;
          Endif;

          // Update maint fields and batch # in HOLD before copying.
          Exec Sql
           Update WDEAP_HLD
           Set EAbat# = :intPaymentBatch,
            EApgm = :Q1pgm,
            EAuser = :inUser,
            EAdt = :sysDate7,
            EAtime = :sysTime6,
            EAtrdt = :sysDate7
           Where EAco# = :inCo#
            And EAfnd  = :inFnd
            And EAemp# = :inEmp
            And EArdiv  = :inDiv
            And EAinv# = :inInv
            And EAbat# = :intPaymentHoldBatch;
          If Not IsValidFetch(sqlStt);
             AddErrorMessageJson('SQL: Update to WDEAP_HLD failed.':
              ioRtnCode);
             Return;
          Endif;

          // Copy from HOLD to LIVE
          Exec Sql
           Insert Into WDEAP
           Select * from WDEAP_HLD
           Where EAco# = :inCo#
            And EAfnd  = :inFnd
            And EAemp# = :inEmp
            And EArdiv  = :inDiv
            And EAinv# = :inInv
            And EAbat# = :intPaymentBatch;
          If Not IsValidFetch(sqlStt);
             AddErrorMessageJson('SQL: Insert to WDEAP failed.':
             ioRtnCode);
             Return;
          Endif;

          // Run through Cash Dissolve if flagged to do so.
          If GetAutoCash(intCo#:intFnd) = *On;
             Exec Sql
              Select Max(rrn(WDEAP)) Into :rrnWDEAP
               From WDEAP
               Where EAco# = :inCo#
               And EAfnd  = :inFnd
               And EAemp# = :inEmp
               And EArdiv = :inDiv
               And EAinv# = :inInv
               And EAbat# = :intPaymentBatch;
               If Not IsValidFetch(sqlStt);
                  AddErrorMessageJson('SQL: No RRN for Cash Dissolve.':
                  ioRtnCode);
                  Return;
               Endif;
               CashDissolve('S':rrnWDEAP);
          Endif;

          // Update batch total in WDEAP
          Exec Sql
          Update WDEAP
          Set EAbata =
           (Select Sum(EAamt)
             From WDEAP
             Where EAbat# = :intPaymentBatch)
          Where EAbat# = :intPaymentBatch;
          If Not IsValidFetch(sqlStt);
             AddErrorMessageJson('SQL: Update total in WDEAP failed.':
             ioRtnCode);
             Return;
          Endif;

          // Delete payments from WDEAP_HLD
          Exec Sql
           Delete From WDEAP_HLD
           Where EAco# = :inCo#
            And EAfnd  = :inFnd
            And EAemp# = :inEmp
            And EArdiv = :inDiv
            And EAinv# = :inInv
            And EAbat# = :intPaymentBatch;

       EndSr;
       //===================================================================
       // WriteToLiveAR - Write from WDEAP_HLD to WMEAP
       //===================================================================
       BegSr WriteToLiveAR;

          // Read thru WDEAP_HLD and transfer by fyr and total amount
          sqlStmtStr = 'Select EAarCd, rrn(wdeap_hld) From wdeap_hld ' +
                  'Where EAco#='  + %EditC(inCo#:'X')  +  ' And ' +
                        'EAfnd='  + %EditC(inFnd:'X')  +  ' And ' +
                        'EAemp#=' + %EditC(inEmp:'X')  +  ' And ' +
                        'EAdiv='  + %EditC(inDiv:'X')  +  ' And ' +
                        'EAinv#=' + %EditC(inInv:'X') + ' ' +
                        'Order by EAfyr';

          Exec Sql Prepare sqlWDEAP_HLD From :sqlStmtStr;
          Exec SQL Declare cursorWDEAP_HLD Cursor For sqlWDEAP_HLD;
          Exec SQL Open cursorWDEAP_HLD;

          Dou Not IsValidFetch(sqlStt);
             Exec SQL fetch next from cursorWDEAP_HLD into :arCode,
             :rrnWDEAP_HLD;
             If IsValidFetch(sqlStt);

                // Let's get the ar type since ony billing records
                // are being transferred
                arType = GetArType(arCode);

                // Process billing records
                Select;
                When arType = 'B' Or arType = 'F';
                   // Transfer Billing Records
                   TransferARToLive('WDEAP_HLD':rrnWDEAP_HLD);
                When arType = 'P';
                   // Call Cash Dissolve
                   CashDissolve('T':rrnWDEAP_HLD);
                EndSl;

             EndIf;
          EndDo;

       EndSr;

       End-Proc Transfer3rdPartyBatch;
      /End-Free
      *=========================================================
      * Request: UpdatePayrollStatus
      * Change the payroll period status
      *=========================================================
     pUpdatePayrollStatus...
     p                 b
     d                 pi             1n
     dinCo#                           3  0 Const
     dinFnd                           3  0 Const
     dinFyr                           3  0 Const
     dinEmp#                          9  0 Const
     dinDiv                           5  0 Const
     dinRPMY                          5    Const
     dinToDt                          7    Const
     dinStatus                        1    Const

     dintPeriod        s              8  0 Inz
     disUpdateLive     s              1n   Inz
     disUpdateHold     s              1n   Inz

      /Free
       Exec Sql
        Update WASRP_HLD
        Set SRstat = :inStatus
        Where SRco# = :inCo# And SRfnd = :inFnd And SRfyr = :inFyr
         And SRemp# = :inEmp# And SRdiv = :inDiv
         And SRrpmy = :inRPMY And SRtoDt = :inToDt;
       isUpdateHold = IsValidFetch(sqlStt);
       Exec sql
        Update WASRP
         Set SRstat = :inStatus
         Where SRco# = :inCo# And SRfnd = :inFnd And SRfyr = :inFyr
          And SRemp# = :inEmp# And SRdiv = :inDiv
         And SRrpmy = :inRPMY And SRtoDt = :inToDt;
       isUpdateLive = IsValidFetch(sqlStt);
       Return isUpdateLive Or isUpdateHold;
      /End-free

     pUpdatePayrollStatus...
     p                 e
