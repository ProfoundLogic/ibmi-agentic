     hCopyright('(c) 2024 Sapiens. All Rights Reserved.')
    h/TITLE W8362R - Create upload S/R payroll A/R
     h option(*nodebugio:*srcstmt)
     //========================================================================
     //  DESCRIPTION:  This program reads WXSRL and writes to A/R Batch Detail
     //
      //  Called from:  W8361C
      //
      //  Rewritten: 07/28/17 by C:136 for project: 150WAN
     //========================================================================
     // Revisions:
      *                                                               *
      * 06/24/24 C:300 Project: 146WKR (Prescient)                    *
      *                Collateral Processing                          *
      *                                                               *
      * 06/12/24 C:062 Project 146WKQ                                 *
      *                Add assessment processing.                     *
      *                                                               *
      * 11/18/20 C:180 Project WC2757                                 *
      *                Recompiled only.                               *
      *                                                               *
      * 09/24/20 C:136 Support: 91640                                 *
      *                Populate EATRDT for the payment batches so     *
      *                that the record passes the EFT check in        *
      *                W/W A/R Batches (W1200R -> WA023R3)            *
      *                                                               *
      * 05/31/19 C:65  Support: 79861 (FMIC) (Replicate 64432)        *
      *                Save Output from OutputWindow in SPILETTER     *
      *                uses position 190 of LDA.  If set, it          *
      *                causes Scriptura to fail when processing a mass*
      *                run. Adjusted ldMassFile in LDA to use         *
      *                different positions.                           *
      //
      // 03/29/19 C:136 Project: 150WDW
      //                Remove 150WBV per client request
      //
      // 11/29/18 C:136 Project: 150WDI
      //                Calculate S/R premium to the penny
      //
      // 11/07/18 C:136 Project: 150WDK
      //                Sunz wants terrorism calculated on non-large
      //                deductible policies only.
      //
      // 06/08/18 C:136 Project: 150WAQ
      //                Add collateral processing for Sunz
      //
      // 03/28/18 C:291 Project: 150WBV
      //                Add call to Billing Invoice for SUNZ
      //
      // 03/21/18 C:136 Support: 69830
      //                Correct writing of A/R records
      //
     //========================================================================
     fwdeal4    uf a e           k disk    rename(pea01:pea0l4)
     fwdrrl     if   e           k disk    rename(prr01:prr00)
     fwdell     if   e           k disk    rename(pel01:pel0l)
     f                                     infds(dbWdell)
     fwmexl     if   e           k disk    rename(pex01:pex00)
     fwmfnl     if   e           k disk    rename(pfn01:pfn00)
     fwtabl     if   e           k disk    rename(psy01:psy0l)
     fwtttl1    if   e           k disk    rename(ptt01:ptt0l1)
     fwxsrl     if   e           k disk    rename(psr01:psr00)
     fxmlprel1  uf   e           k disk
     //========================================================================
      // Program described variables
     //========================================================================
     d cltID           s              3
     d elrrn           s              9  0
     d expModFactor    s              9  6
     d flag            s               n
     d kkco#           s                   like(SRCO#)
     d kkefdt          s                   like(SRTODT)
     d isMassReq       s               n
     d isUseScript     s               n
     d isLdaWritten    s               n
     d isPeo           s               n
     d massFlag        s              1    inz('Y')
     d message         s            250    inz(*blanks)
     d notTransferFl   s               n
     d numberOfDays    s              5  0
     d prmNotDartsCd   s                   like(EXpnrm)
     d processType     c                   const('Premium Notice')
     d proWavChg       s             11  0
     d qqRrn           s                   like(ELRRN)
     d qqRpMY          s              6  0
     d qqToDt          s              8  0
     d qqMaPr          s                   like(qqTotPremium)
     d qqAdLd          s                   like(qqTotPremium)
     d qqPayr          s                   like(qqTotPremium)
     d qqTotPremium    s             11  0 inz(0)
     d qqTotTax        s                   like(qqTotPremium)
     d qqPrBillPrm     s                   like(qqTotPremium)
     d qqPrBillTax     s                   like(qqTotPremium)
     d qqFactors       s            143
      *** beg add ***** 11/29/18 **************************
     d qqTotPremium2   s             11  2 inz(0)
     d qqTotTax2       s                   like(qqTotPremium2)
     d qqPrBillPrm2    s                   like(qqTotPremium2)
     d qqPrBillTax2    s                   like(qqTotPremium2)
     d qqFactors2      s             48
      *** end add ***** 11/29/18 **************************
     d qqPrevBal       s             11  2
     d qqSrOnline      s              1
     d qqInfo          s             39
     d qqHeading       s            270
     d qqPro           s            100
     d ratingState     s              2
     d stateRptDiv     s                   like(ELDIV)
     d strUsrOutQ      s             10
     d supressPrYear   s                   like(EXspyb)
     d svfnd           s                   like(SRFND)
     d svemp#          s                   like(SREMP#)
     d svdiv           s                   like(SRDIV)
     d svfyr           s                   like(SRFYR)
      *** beg add ***** 03/21/18 ***********************
     d svClas          s                   like(SRclas)
     d svSpec          s                   like(SRspec)
     d svExt           s                   like(SRext)
      *** end add ***** 03/21/18 ***********************
     d lastFyr         s                   like(SRFYR)
     d lastRpDt        s                   like(EApodt)
     d svtodt          s                   like(SRTODT)
     d svrpmy          s                   like(SRRPMY)
     d svfreq          s                   like(SRFREQ)
     d svArSm          s                   like(ELarsm)
     d svautr          s                   like(SRAUTR)
     d svvdr#          s                   like(SRVDR#)
     d svppcd          s                   like(SRPPCD)
     d svpbcd          s                   like(SRPBCD)
     d svGenPrm        s                   like(SRGenPrm)
     d svNetRate       s             12  9
     d svTranPay       s                   like(SRTranPay)
     d sys_date        s               d   datfmt(*usa)
     d sys_time        s               t   timfmt(*hms:)
     d sysCode         s                   like(EASYCD)
     d sysDate7        s              7  0
     d sysDate8        s              8  0
     d sysTime6        s              6  0
     d taxBillCode     s                   like(EAARCD)
     d taxBillCodeDes  s                   like(EAdesc)
     d taxPayCode      s                   like(EAARCD)
     d taxPayCodeDesc  s                   like(EAdesc)
     d totalManPrem    s             +2    like(SRmanprm)
     d totalTerPayR    s             +2    like(SRamtp)
     d xmlFile         s                   like(XPXML)
     d xmlFileName     s                   like(XPxml)
     d xmlFlowName     s                   like(XPXML)
     d xmlLink         s                   like(XPLink)
     d xxMass          s              1
     d xxProcType      s            100
     d xxSubmit        s              1n
     d*** Begin Add *** 06/12/24 *******************************************
     d hldTime         s               t    TimFmt(*usa)
     d*** End   Add *** 06/12/24 *******************************************
     //========================================================================
      // Data Structures and Arrays
     //========================================================================
     d b#a             s                   dim(4) like(EABAT#)                  batch # assignment
     d bInd            s               n   dim(4)                               type of batch ind
     d bamt            s                   dim(4) like(EABATA)                  batch # assignment
     d dbWdell         ds
     d  rrnWdell             397    400b 0
     d psds          esds                  extname(szq1p)
     d svLDA           ds          1024    dtaara(*lda)
     d ldend#                         5  0 overlay(svlda:1)
     d ldenda                         5    overlay(svlda:1)
     d ldmyrr                         9  0 overlay(svlda:6)
     d ldeedt                         8  0 overlay(svlda:15)
     d ldfyr                          3  0 overlay(svlda:23)
     d ldpyrl                         1  0 overlay(svlda:26)
     d ldSrPayImpFlg                  1    overlay(svlda:29)
     d*** begin chg ***  05/31/19  *******************************************
     D****ldMassFile                    60    Overlay(svlda:131)
     D ldMassFile                    60    Overlay(svlda:129)
     d*** end chg *****  05/31/19  *******************************************
     d ldOutputFile                  60    Overlay(svlda:191)
     d xxDart                         1    overlay(svlda:251)
     d xxRmCd                         5    overlay(svlda:252)
     d xxCopy                         1    overlay(svlda:257)
     d xxPrint                        1    overlay(svlda:258)
     d xxPrinter                     10    overlay(svlda:259)
     d xxTray                        50    overlay(svlda:269)
     d xx#CopiesA                     3    overlay(svlda:319)
     d xxEmail                        1    overlay(svlda:322)
     d xxToEmail                    200    overlay(svlda:323)
     d xxSubject                    150    overlay(svlda:523)
     d xxBody                       350    overlay(svlda:673)
     d*** Begin Add *** 06/12/24 *******************************************
     d dsAssessment    ds                  qualified inz
     d  chrPrintAss                   1a
     d  chrAssessment#...
     d                                1    dim(7)
     d  decAssessmentHdg...
     d                               20    dim(7)
     d  decAssessmentPct...
     d                                6  4 dim(7)
     d  decAssessmentAmt...
     d                               11  0 dim(7)

     d dsPriorBilled   ds                  qualified inz
     d  decPriorBilledAmt...
     d                               11  0 dim(7)

     d chrAssIndex     s              1    Inz
     d idx             s              1  0 Inz(0)
     d outIdx          s              1  0 Inz(0)
     d rtnAssessment   s            267    Inz
     d rtnPreviousAssessment...
     d                 s             77    Inz
     d*** End   Add *** 06/12/24 *******************************************
     d*** Begin Add *** 06/24/24 *******************************************
     d rtnColB         s             11  0 Inz(0)
     d rtnPrColB       s             11  0 Inz(0)
     d*** End   Add *** 06/24/24 *******************************************
     //========================================================================
      // Procedure prototypes
     //========================================================================
     d/copy sprDate
     d/copy srInv#Pr
     d/copy sprLetter
     d/copy sprXML
     d/copy wprAR
     d/copy wprEmplr
     d/copy wprRateSet
     d/copy wprUsrPrms

     d CalcPremium     pr                  extpgm('SRPREM')
     d  outELrrn                      9  0
     d  outRPMY                       6  0
     d  outToDt                       8  0
     d  outManPrem                   11  0
     d  outAddlLoad                  11  0
     d  outTerrPayrol                11  0
     d  rtnTotPrem                   11  0
     d  rtnTaxAmt                    11  0
     d  rtnPriorPrem                 11  0
     d  rtnPriorTax                  11  0
     d  rtnFactors                  143
     d  rtnPrevBal                   11  2
     d  rtnSrOnline                   1
     d  rtnInfo                      39
     d  rtnHeading                  270
     d  rtnPro                      100
      *** beg add ***** 11/29/18 **************
     d  rtnTotPrem2                  11  2
     d  rtnTaxAmt2                   11  2
     d  rtnPriorPrem2                11  2
     d  rtnPriorTax2                 11  2
     d  rtnFactors2                  48
      *** end add ***** 11/29/18 **************
     d*** Begin Add *** 06/12/24 *******************************************
     d  rtnAssessment               267
     d  rtnPreviousAssessment...
     d                               77
     d*** End   Add *** 06/12/24 *******************************************
     d*** Begin Add *** 06/24/24 *******************************************
     d  rtnColB                      11  0
     d  rtnPrColB                    11  0
     d*** End   Add *** 06/24/24 *******************************************
     d GetNxtBatch     pr                  like(EAbat#)
     d GetBatches      pr
     d GetClientID     pr                  extpgm('RTVCLTID')
     d  ReturnCltID                   3
     d GetInitValues   pr
     d GetEmpDetail    pr
     d GetPrmNotDfts   pr
     d GetReportDate   pr                  like(EAPODT)
     d GetSysCode      pr
     d GetTaxCodes     pr
     d PrepScriptura   pr
     d SetSaveFields   pr
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
     d S4400C          pr                  ExtPgm('S4400C')
     d  fileNameParm                 61
     d UpdateBatchTot  pr
     d WriteAR         pr
      *** beg add ***** 06/08/18 ****************
     d WrtCollateral   pr
      *** end add ***** 06/08/18 ****************
     //========================================================================
      // Main
     //========================================================================
      /Free
       GetInitValues();
       Setll *Loval psr00;
       Read psr00;
       SetSaveFields();
       Dow Not(%Eof(WXSRL));
        //*** beg chg ***** 03/21/18 ***************
        //*** Select;
        //*** When svArSm <> 'Y' and (svFnd <> SRfnd or
        //***  svEmp# <> SRemp# or svFyr <> SRfyr or
        //***  svDiv <> SRdiv);
        //***    WriteAR();
        //***    SetSaveFields();
        //*** When svArSm = 'Y' and (svFnd = SRfnd and
        //***  svEmp# = SRemp# and svFyr = SRfyr and
        //***  svDiv <> SRdiv);
        //***    WriteAR();
        //***    SetSaveFields();
        //*** When svArSm = 'Y' and (svFnd <> SRfnd or
        //***   svEmp# <> SRemp# or svFyr <> SRfyr);
        //***    WriteAR();
        //***    SetSaveFields();
        //*** Other;
        //***    Eval(h) totalTerPayR += SRamtp;
        //***    Eval(h) totalManPrem += SRManPrm;
        //*** Endsl;
          Select;
          When svFnd <> SRfnd or svEmp# <> SRemp#
           or svFyr <> SRfyr or svDiv <> SRdiv
           or svRPMY <> SRrpmy or svTodt <> SRtodt;
             WriteAR();
             SetSaveFields();
             If SRRepFlg <> 'Y';
                Eval(h) totalTerPayR += SRamtp;
                Eval(h) totalManPrem += SRManPrm;
             Endif;
          Other;
             If SRRepFlg <> 'Y';
                Eval(h) totalTerPayR += SRamtp;
                Eval(h) totalManPrem += SRManPrm;
             Endif;
          Endsl;
        //*** end chg ***** 03/21/18 ***************
       Read psr00;
          If %Eof(Wxsrl);
             WriteAR();
          Endif;
       Enddo;
       UpdateBatchTot();
        //*** beg add ***** 06/08/18 ***************
       WrtCollateral();
        //*** end add ***** 06/08/18 ***************
        //*** beg del ***** 03/21/18 ***************
        //*** SbmScriptura();
        //*** end del ***** 03/21/18 ***************
       *Inlr = *On;
      /End-Free
     //========================================================================
      // Sets batch indicators 1 - autoTransferBillings 2 - DontTransferBillings
      //                       3 - autoTransferPayments 4 - DontTransferPayments
     //========================================================================
     p GetBatches      b
     d GetBatches      pi

      /Free
       Read psr00;
       Dow Not(%Eof);
          If bInd(1) = *off and SRautr = 'Y';    // Billing batch auto transferred
             bInd(1) = *on;
             b#a(1) = GetNxtBatch();
          Endif;
          If bInd(2) = *off and SRautr <> 'Y';    // Billing batch not auto transferred
             bInd(2) = *on;
             b#a(2) = GetNxtBatch();
          Endif;
          If bInd(3) = *off and SRtranPay = 'Y';  // Payment batch auto transferred
             bInd(3) = *on;
             b#a(3) = GetNxtBatch();
          Endif;
          If bInd(4) = *off and SRtranPay <> 'Y';  // Payment batch not auto transferred
             bInd(4) = *on;
             b#a(4) = GetNxtBatch();
          Endif;
       Read psr00;
       Enddo;
      /End-Free

     p GetBatches      e
     //========================================================================
      // Get employer detail record
     //========================================================================
     p GetEmpDetail    b
     d GetEmpDetail    pi

     c     elky          klist
     c                   kfld                    SRCO#
     c                   kfld                    SRFND
     c                   kfld                    SREMP#
     c                   kfld                    SRDIV
     c                   kfld                    SRFYR

     c     rrky          klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    ratingState

     c     rrky1         klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    ratingState
     c                   kfld                    ELPFDT

      /Free
       Chain elky pel0l;
       ratingState = RatingSt(ELCO#:ELRTCD);
       Setgt rrky1 prr00;
       Readpe rrky prr00;
       //*** beg chg ***** 03/21/18 *****************
       //***If ELARSM = 'Y';
       //***   stateRptDiv = GetStRptDv(ELCO#:ELFND:ELEMP#);
       //***Else;
       //***   stateRptDiv = 0;
       //***Endif;
       stateRptDiv = GetStRptDv(ELCO#:ELFND:ELEMP#);
       //*** end chg ***** 03/21/18 *****************
      /End-Free

     p GetEmpDetail    e
     //========================================================================
      // Gets miscellaneous initial values
     //========================================================================
     p GetInitValues   b
     d GetInitValues   pi

      /Free
       GetClientID(cltID);
       kkco# = UserCo#(Q1user);
       strUsrOutQ = GetUsrOutQ(Q1user);
       sys_Date = %date();
       sys_Time = %time();
       sysDate7 = %Dec(sys_Date:*cymd);
       sysDate8 = %Dec(sys_Date:*usa);
       sysTime6 = %Dec(sys_Time:*hms);
       GetBatches();
       GetPrmNotDfts();
       GetSysCode();
       GetTaxCodes();
       //*** beg del ***** 03/21/18 ***************
       //*** PrepScriptura();
       //*** end del ***** 03/21/18 ***************
      /End-Free

     p GetInitValues   e
     //========================================================================
       // Sets up batch numbers for WDEAP
     //========================================================================
     p GetNxtBatch     b
     d GetNxtBatch     pi                  like(EAbat#)

     d kkBat#          s                   like(EABAT#)
     d nxBat#          s                   like(EABAT#)
     d idx             s              1  0 inz(1)

     c     eaky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkBat#

      /Free
       // Get next available batch number
       kkbat# = *hival;
       Setgt eaky wdeal4;
       Readpe(n) kkco# wdeal4;
       If not(%Eof(wdeal4));
          nxBat# = EAbat# + 1;
       Else;
          nxBat# = 1;
       Endif;
       // Save it off
       //*** beg add ***** 03/21/18 **********
       Clear pea0l4;
       //*** end add ***** 03/21/18 **********
       EAco#  = kkCo#;
       EAchk# = *Hival;
       EAckam = *HIval;
       EAbat# = nxBat#;
       EAtfap = 'N';
       EAtffl = 'N';
       EApgm  = Q1pgm;
       EAuser = Q1user;
       EAipgm = Q1pgm;
       EAiusr = Q1user;
       EAtime = sysTime6;
       EAidt  = sysDate7;
       EAdt   = sysDate7;
       Write pea0l4;
       Feod wdeal4;
       Return nxBat#;
      /End-Free

     p GetNxtBatch     e
     //========================================================
     // Chains to Admin/Darts for premium notice remark code
      // and suppress prior year flag
     //========================================================
     p GetPrmNotDfts   b

      /Free
       Chain (kkCo#:svFnd) pex00;
       prmNotDartsCd = EXpnrm;
       supressPrYear = EXspyb;
      /End-Free

     p GetPrmNotDfts   e
     //========================================================
      // Gets the report date for EAPODT (A/R posting date)
     //========================================================
     p GetReportDate   b
     d GetReportDate   pi                  like(EAPODT)

     d  reportDate     s              7  0
     d  workDate       s              7  0

      /Free
       Select;
       When svfreq = 'W';
          reportDate = svtodt;
       When svfreq = 'M';
          workDate  = SRrpmy * 100 + 1;
          reportDate =
          DateTo7(GetLastDay(CvtToDate(DtMDYY(workDate))));
          If reportDate > ELPTDT;
             reportDate = ELPTDT;
          Endif;
       Endsl;
       Return reportDate;
      /End-Free

     p getReportDate   e
      //====================================================
      // Get A/R Sytem Code
      //====================================================
     p GetSysCode      b
     d GetSysCode      pi

      /Free
       Reade kkco# psy0l;
       Dow not(%eof);
          If SYTYPE = 'Y' and
                 SYSTAT = 'A';
             sysCode = SYKEY;
             Leave;
          Endif;
          Reade kkco# psy0l;
       Enddo;
      /End-Free

     p GetSysCode      e
      //====================================================
      // Gets tax bill and payment A/R codes
      //====================================================
     p GetTaxCodes     b
     d GetTaxCodes     pi

     d kktype          s                    like(TTTYPE)
     d kkctyp          s                    like(TTCTYP) inz('T')

     c     txky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkctyp
     c                   kfld                    kktype
     c
      /Free
       kktype = 'F';
       Setll txky ptt0l1;
       Reade txky ptt0l1;
       Dow Not(%Eof(wtttl1));
          If TTstat = 'A';
             taxBillCode = TTkey;
             taxBillCodeDes = getArDesc(TTkey);
             Leave;
          Endif;
       Reade txky ptt0l1;
       Enddo;

       kktype = 'Q';
       Setll txky ptt0l1;
       Reade txky ptt0l1;
       Dow Not(%Eof(wtttl1));
          If TTstat = 'A';
             taxPayCode = TTKEY;
             taxPayCodeDesc = getArDesc(TTkey);
             Leave;
          Endif;
       Reade txky ptt0l1;
       Enddo;
      /End-Free

     p GetTaxCodes     e
      //====================================================
      // Sets Scriptura indicator
      //====================================================
     p PrepScriptura   b

      /Free
       //*** beg chg ***** 03/21/18 *****************
       //*** isUseScript = UseScriptura(processType);
       If cltID = '150';
          isUseScript = UseScriptura(processType);
       Endif;
       //*** end chg ***** 03/21/18 *****************
      /End-Free

     p PrepScriptura   e
     //=====================================================
     // Submits job for Scriptura documents to print
     //=====================================================
     p SbmScriptura    b

     d fileNameParm    s             61

      /Free
       //*** beg chg ***** 03/21/18 ************
       //*** fileNameParm = xmlFileName + '@';
       //*** S4400C(fileNameParm);
       If isUseScript and svGenPrm = 'Y';
          fileNameParm = xmlFileName + '@';
          S4400C(fileNameParm);
       Endif;
       //*** end chg ***** 03/21/18 ************
      /End-Free

     p SbmScriptura    e
     //=====================================================
      // Sets the save fields for writing, gets Employer
      //  Detail record, and net rate for policy
     //=====================================================
     p SetSaveFields   b
     d SetSaveFields   pi

      /Free
       svfnd  = SRFND;
       Chain (SRco#:SRfnd) pfn00;
       If FNpeof <> *Blank;
          isPeo = *On;
       Else;
          isPeo = *Off;
       Endif;
       svemp# = SREMP#;
       svdiv  = SRDIV;
       svfyr  = SRFYR;
       //*** beg add ***** 03/21/18 ****************
       svClas = SRclas;
       svSpec = SRspec;
       svExt  = SRext;
       //*** end add ***** 03/21/18 ****************
       // Ensure premium notice runs for latest year encountered
       If SRfyr > lastFyr;
          lastFyr = SRfyr;
       Endif;
       svArSm = SRarsm;
       svtodt = SRTODT;
       svrpmy = SRRPMY;
       svautr = SRAUTR;
       svTranPay = SRtranPay;
       svpbcd = SRPBCD;
       svppcd = SRPPCD;
       svfreq = SRFREQ;
       svvdr# = SRVDR#;
       // Run a premium notice on master PEO policy if any transaction
       //  is flagged to do so
       If SRGenPrm = 'Y';
          svGenPrm = SRGenPrm;
       Endif;
       GetEmpDetail();
      /End-Free

     p SetSaveFields   e
      //===========================================================================
      // Updates batch total and deletes the A/R Batch Detail place holding records
      //===========================================================================
     p UpdateBatchTot  b
     d UpdateBatchTot  pi

     d z               s              1  0
     d kkbat#          s                   like(EAbat#)

     c     eaky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkBat#

      /Free
       For z = 1 by 1 to 4;
          If b#a(z) <> 0;
             kkbat# = b#a(z);
             Setll eaky pea0l4;
             Reade eaky pea0l4;
             Dow Not(%Eof);
                If EAchk# = *hival and
                   EAckam = *hival;
                   Delete pea0l4;
                Else;
                   EAbata = bamt(z);
                   Update pea0l4;
                Endif;
             Reade eaky pea0l4;
             Enddo;
          Endif;
       EndFor;
      /End-Free

     p UpdateBatchTot  e
     //========================================================================
      // Write AR
     //========================================================================
     p WriteAR         b
     d WriteAR         pi

      /Free
       qqRrn  = rrnWdell;
       qqToDt = dtMDYY(svtodt);
       qqRpMY = dt6MDYY(svrpmy);
       qqAdLd = 0;
       Eval(h) qqMaPr = totalManPrem;
       Eval(h) qqPayR = totalTerPayR;
       totalManPrem = 0;
       totalTerPayR = 0;

       //*** beg chg ***** 11/29/18 ************************
       //*** CalcPremium(qqRrn:qqRPMY:qqToDt:qqMaPr:qqAdLd:qqPayR:
       //***  qqTotPremium:qqTotTax:qqPrBillPrm:qqPrBillTax:
       //***  qqFactors:qqPrevBal:qqSrOnline:qqInfo:qqHeading:qqPro);
       //*** Begin Chg *** 06/12/24 ****************************************
       //*** CalcPremium(qqRrn:qqRPMY:qqToDt:qqMaPr:qqAdLd:qqPayR:
       //***  qqTotPremium:qqTotTax:qqPrBillPrm:qqPrBillTax:
       //***  qqFactors:qqPrevBal:qqSrOnline:qqInfo:qqHeading:qqPro:
       //***  qqTotPremium2:qqTotTax2:qqPrBillPrm2:qqPrBillTax2:qqFactors2);
       Clear dsAssessment;
       Clear dsPriorBilled;
       rtnAssessment         = dsAssessment;
       rtnPreviousAssessment = dsPriorBilled;

       //*** Begin Chg *** 06/24/24 ****************************************
       //*** CalcPremium(qqRrn:qqRPMY:qqToDt:qqMaPr:qqAdLd:qqPayR:
       //***  qqTotPremium:qqTotTax:qqPrBillPrm:qqPrBillTax:
       //***  qqFactors:qqPrevBal:qqSrOnline:qqInfo:qqHeading:qqPro:
       //***  qqTotPremium2:qqTotTax2:qqPrBillPrm2:qqPrBillTax2:qqFactors2:
       //***  rtnAssessment:rtnPreviousAssessment);
       CalcPremium(qqRrn:qqRPMY:qqToDt:qqMaPr:qqAdLd:qqPayR:
        qqTotPremium:qqTotTax:qqPrBillPrm:qqPrBillTax:
        qqFactors:qqPrevBal:qqSrOnline:qqInfo:qqHeading:qqPro:
        qqTotPremium2:qqTotTax2:qqPrBillPrm2:qqPrBillTax2:qqFactors2:
        rtnAssessment:rtnPreviousAssessment:rtnColB:rtnPrColB);
       //*** End   Chg *** 06/24/24 ****************************************

       dsAssessment  = rtnAssessment;
       dsPriorBilled = rtnPreviousAssessment;
       //*** End   Chg *** 06/12/24 ****************************************
       //*** end chg ***** 11/29/18 ************************

       Clear pea0l4;
       EACO#  = kkco#;
       EAFND  = svfnd;
       EAFYR  = svfyr;
       EAEMP# = svemp#;
       EADIV  = svdiv;
       EASTAT = ' ';
       If cltId = '150';
          EAACHF = 'L';
       Else;
          EAACHF = 'S';
       Endif;
       EASYCD = sysCode;
       If ELARSM = 'Y';
          EARDIV = svDiv;
          EADIV  = stateRptDiv;
       Else;
          EARDIV = svdiv;
       Endif;
       EAAMT  = qqTotPremium - qqPrBillPrm;
       //*** beg add ***** 11/29/18 ***********************
       If cltID = '150';
          EAAMT  = qqTotPremium2 - qqPrBillPrm2;
       EndIf;
       //*** end add ***** 11/29/18 ***********************
       EAPODT = getReportDate;
       // Ensure premium notice runs with most recent report date
       If EApodt > lastRpDt;
          lastRpDt = EApodt;
       Endif;
       EAUSER = Q1USER;
       EAPGM  = Q1PGM;
       EATIME = TimeTo6(SystemTime);
       EADT   = dateTo7(SystemDate);
       EAIUSR = Q1USER;
       EAIPGM = Q1PGM;
       EAIDT  = EADT;
       //*** Begin Add *** 06/24/24 ****************************************
       Chain (kkCo#:svFnd) pex00;        // Chain to Admin Darts file
       //*** End   Add *** 06/24/24 ****************************************
       // Write Auto Transfer Billing Batch
       If svAutR  = 'Y';
          EAarcd  = svPbCd;
          EAdesc  = getARDesc(EAarcd);
          EAcTyp  = 'P';
          EAbat#  = b#a(1);
          bAmt(1) = bAmt(1) + EAamt;
          EAtfap  = 'Y';
          EAtffl  = 'Y';
          EAautr  = 'Y';
          Write pea0l4;
          If RRtxpc <> 0;
             EAamt  = qqTotTax - qqPrBillTax;
             //*** beg add ***** 11/29/18 ***********************
             If cltID = '150';
                EAamt  = qqTotTax2 - qqPrBillTax2;
             EndIf;
             //*** end add ***** 11/29/18 ***********************
             EAarcd = taxBillCode;
             EAdesc = taxBillCodeDes;
             EActyp = 'T';
             EAbat# = b#a(1);
             bAmt(1) = bAmt(1) + EAamt;
             Write pea0l4;
          Endif;
          //*** Begin Add *** 06/12/24 *************************************
          // Write Assessments if supposed to
          If dsAssessment.chrPrintAss = 'Y';
             For idx=1 to 7;
                chrAssIndex = dsAssessment.chrAssessment#(idx);
                If chrAssIndex <> ' ';
                   EAamt   = dsAssessment.decAssessmentAmt(idx) -
                             dsPriorBilled.decPriorBilledAmt(idx);
                   EAarcd  = GetDefaultArCode('F':chrAssIndex);
                   EAdesc  = GetARDesc(EAarCd);
                   EActyp  = chrAssIndex;
                   EAbat#  = b#a(1);
                   bAmt(1) = bAmt(1) + EAamt;
                   sys_Time = sys_Time + %Seconds(1);
                   EAtime  = %Dec(sys_Time:*hms);
                   Write pea0l4;
                EndIf;
             EndFor;
          EndIf;
          //*** End   Add *** 06/12/24 *************************************
          //*** Begin Add *** 06/24/24 *************************************
          // Write Collateral Billed Amount
          If rtnColB <> 0 and EXcolb <> *blanks;
             EAamt   = rtnColB - rtnPrColB;
             EAarcd  = EXcolb;
             EAdesc  = GetARDesc(EAarCd);
             EActyp  = 'G';
             EAbat#  = b#a(1);
             bAmt(1) = bAmt(1) + EAamt;
             Write pea0l4;
          EndIf;
          //*** End   Add *** 06/24/24 *************************************
       Endif;
       // Write Billing Batch that doesn't auto transfer
       If svAutR <> 'Y';
          EAarcd  = svPbCd;
          EAdesc  = getARDesc(EAarcd);
          EAcTyp  = 'P';
          EAbat#  = b#a(2);
          bAmt(2) = bAmt(2) + EAamt;
          EAtfap  = 'N';
          EAtffl  = 'N';
          EAautr  = 'N';
          Write pea0l4;
          If RRtxpc <> 0;
             EAamt   = qqTotTax - qqPrBillTax;
             //*** beg add ***** 11/29/18 ***********************
             If cltID = '150';
                EAamt  = qqTotTax2 - qqPrBillTax2;
             EndIf;
             //*** end add ***** 11/29/18 ***********************
             EAarcd  = taxBillCode;
             EAdesc  = taxBillCodeDes;
             EActyp  = 'T';
             EAbat#  = b#a(2);
             bAmt(2) = bAmt(2) + EAamt;
             Write pea0l4;
          Endif;
          //*** Begin Add *** 06/12/24 *************************************
          // Write Assessments if supposed to
          If dsAssessment.chrPrintAss = 'Y';
             For idx=1 to 7;
                chrAssIndex = dsAssessment.chrAssessment#(idx);
                If chrAssIndex <> ' ';
                   EAamt   = dsAssessment.decAssessmentAmt(idx) -
                             dsPriorBilled.decPriorBilledAmt(idx);
                   EAarcd  = GetDefaultArCode('F':chrAssIndex);
                   EAdesc  = GetARDesc(EAarCd);
                   EActyp  = chrAssIndex;
                   EAbat#  = b#a(2);
                   bAmt(2) = bAmt(2) + EAamt;
                   sys_Time = sys_Time + %Seconds(1);
                   EAtime  = %Dec(sys_Time:*hms);
                   Write pea0l4;
                EndIf;
             EndFor;
          EndIf;
          //*** End   Add *** 06/12/24 *************************************
          //*** Begin Add *** 06/24/24 *************************************
          // Write Collateral Billed Amount
          If rtnColB <> 0 and EXcolb <> *blanks;
             EAamt   = rtnColB - rtnPrColB;
             EAarcd  = EXcolb;
             EAdesc  = GetARDesc(EAarCd);
             EActyp  = 'G';
             EAbat#  = b#a(2);
             bAmt(2) = bAmt(2) + EAamt;
             Write pea0l4;
          EndIf;
          //*** End   Add *** 06/24/24 *************************************
       Endif;
       // Write Auto Transfer Payment Batch
       //*** beg add ***** 11/29/18 ***********************
       If cltID = '150';
          EAAMT  = qqTotPremium2 - qqPrBillPrm2;
       Else;
          EAAMT  = qqTotPremium - qqPrBillPrm;
       EndIf;
       //*** end add ***** 11/29/18 ***********************
       If svTranPay = 'Y' and svPpCd <> *Blanks;
       //*** beg add ***** 09/24/20 ***********************
          EAtrDt  = EAdt;
       //*** end add ***** 09/24/20 ***********************
          EAarCd  = svPpCd;
          //*** Begin Add *** 06/12/24 *************************************
          EAcTyp  = 'P';
          //*** End   Add *** 06/12/24 *************************************
          EAdesc  = getARDesc(EAarcd);
          EAbat#  = b#a(3);
          bAmt(3) = bAmt(3) + EAamt;
          EAtfap  = 'Y';
          EAtffl  = 'Y';
          EAautr  = 'Y';
          Write pea0l4;
          If RRtxpc <> 0;
             EAamt   = qqTotTax - qqPrBillTax;
             //*** beg add ***** 11/29/18 ***********************
             If cltID = '150';
                EAamt  = qqTotTax2 - qqPrBillTax2;
             EndIf;
             //*** end add ***** 11/29/18 ***********************
             EAarcd  = taxPayCode;
             EAdesc  = taxPayCodeDesc;
             EAbat#  = b#a(3);
             bAmt(3) = bamt(3) + EAamt;
             Write pea0l4;
          Endif;
          //*** Begin Add *** 06/12/24 *************************************
          // Write Assessments if supposed to
          If dsAssessment.chrPrintAss = 'Y';
             For idx=1 to 7;
                chrAssIndex = dsAssessment.chrAssessment#(idx);
                If chrAssIndex <> ' ';
                   EAamt   = dsAssessment.decAssessmentAmt(idx) -
                             dsPriorBilled.decPriorBilledAmt(idx);
                   EAarcd  = GetDefaultArCode('Q':chrAssIndex);
                   EAdesc  = GetARDesc(EAarCd);
                   EActyp  = chrAssIndex;
                   EAbat#  = b#a(3);
                   bAmt(3) = bAmt(3) + EAamt;
                   sys_Time = sys_Time + %Seconds(1);
                   EAtime  = %Dec(sys_Time:*hms);
                   Write pea0l4;
                EndIf;
             EndFor;
          EndIf;
          //*** End   Add *** 06/12/24 *************************************
          //*** Begin Add *** 06/24/24 *************************************
          // Write Collateral Payment Amount
          If rtnColB <> 0 and EXcolp <> *blanks;
             EAamt   = rtnColB - rtnPrColB;
             EAarcd  = EXcolp;
             EAdesc  = GetARDesc(EAarCd);
             EActyp  = 'G';
             EAbat#  = b#a(3);
             bAmt(3) = bAmt(3) + EAamt;
             Write pea0l4;
          EndIf;
          //*** End   Add *** 06/24/24 *************************************
       Endif;
       // Write Payment Batch that doesn't auto transfer
       If svTranPay <> 'Y' and svPpCd <> *Blanks;
       //*** beg add ***** 09/24/20 ***********************
          EAtrDt  = EAdt;
       //*** end add ***** 09/24/20 ***********************
          EAarCd  = svPpCd;
          //*** Begin Add *** 06/12/24 *************************************
          EAcTyp  = 'P';
          //*** End   Add *** 06/12/24 *************************************
          EAdesc  = getARDesc(EAarcd);
          EAbat#  = b#a(4);
          bAmt(4) = bAmt(4) + EAamt;
          EAtfap  = 'N';
          EAtffl  = 'N';
          EAautr  = 'N';
          Write pea0l4;
          If RRtxpc <> 0;
             EAamt   = qqTotTax - qqPrBillTax;
             //*** beg add ***** 11/29/18 ***********************
             If cltID = '150';
                EAamt  = qqTotTax2 - qqPrBillTax2;
             EndIf;
             //*** end add ***** 11/29/18 ***********************
             EAarcd  = taxPayCode;
             EAdesc  = taxPayCodeDesc;
             EAbat#  = b#a(4);
             bAmt(4) = bamt(4) + EAamt;
             Write pea0l4;
          Endif;
          //*** Begin Add *** 06/12/24 *************************************
          // Write Assessments if supposed to
          If dsAssessment.chrPrintAss = 'Y';
             For idx=1 to 7;
                chrAssIndex = dsAssessment.chrAssessment#(idx);
                If chrAssIndex <> ' ';
                   EAamt   = dsAssessment.decAssessmentAmt(idx) -
                             dsPriorBilled.decPriorBilledAmt(idx);
                   EAarcd  = GetDefaultArCode('Q':chrAssIndex);
                   EAdesc  = GetARDesc(EAarCd);
                   EActyp  = chrAssIndex;
                   EAbat#  = b#a(4);
                   bAmt(4) = bAmt(4) + EAamt;
                   sys_Time = sys_Time + %Seconds(1);
                   EAtime  = %Dec(sys_Time:*hms);
                   Write pea0l4;
                EndIf;
             EndFor;
          EndIf;
          //*** End   Add *** 06/12/24 *************************************
          //*** Begin Add *** 06/24/24 *************************************
          // Write Collateral Payment Amount
          If rtnColB <> 0 and EXcolp <> *blanks;
             EAamt   = rtnColB - rtnPrColB;
             EAarcd  = EXcolp;
             EAdesc  = GetARDesc(EAarCd);
             EActyp  = 'G';
             EAbat#  = b#a(4);
             bAmt(4) = bAmt(4) + EAamt;
             Write pea0l4;
          EndIf;
          //*** End   Add *** 06/24/24 *************************************
       Endif;
      /End-Free

     p WriteAR         e
     //========================================================================
     p WrtCollateral   b
     //========================================================================
     d WrtCollateral   pr

     d iBilBat#        s                   like(EAbat#)
     d iPayBat#        s                   like(EAbat#)
     d jBilBat#        s                   like(EAbat#)
     d jPayBat#        s                   like(EAbat#)
     d pBilBat#        s                   like(EAbat#)
     d pPayBat#        s                   like(EAbat#)
     d pAmt            s              9  0
     d pBat#           s                   like(EAbat#)
     d pDate           s                   like(EApodt)
     d pEmp#           s                   like(EAemp#)
     d pFnd            s                   like(EAfnd)
     d pFyr            s                   like(EAfyr)
     d pMode           s              1a
     d pBat#1          s                   like(EAbat#)
     d pBat#2          s                   like(EAbat#)

      /Free
       If SRWrtColl <> 'Y';
          Return;
       Endif;
       pBat#1 = b#a(1);
       pBat#2 = b#a(2);
       Exec SQL Declare C1 Cursor For
        Select Distinct EAbat#, EAfnd, EAfyr, EAemp#, EApodt
        From WDEAP
        Where EAbat# in (:pBat#1, :pBat#2);
       Exec SQL Open C1;
       Dou SqlStt = '00000';
          Exec Sql Fetch Next From C1 Into :pBat#, :pFnd, :pFyr, :pEmp#, :pDate;
          If SqlStt <> '00000';
             Leave;
          Endif;
          Select;
          When pBat# = b#a(1);
             pMode = 'J';
             pBilBat# = jBilBat#;
             pPayBat# = jPayBat#;
          When pBat# = b#a(2);
             pMode = 'I';
             pBilBat# = iBilBat#;
             pPayBat# = iPayBat#;
          Endsl;
          If CollateralSR(kkCo#:pFnd:pEmp#:pFyr:pMode:
           pAmt:pDate:pBilBat#:pPayBat#) <> 0;
             Select;
             When pBat# = b#a(1);
                jBilBat# = pBilBat#;
                jPayBat# = pPayBat#;
             When pBat# = b#a(2);
                iBilBat# = pBilBat#;
                iPayBat# = pPayBat#;
             Endsl;
          Endif;
       Enddo;
      /End-Free

     p WrtCollateral   e
