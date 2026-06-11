     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     h*****************************************************************
      /TITLE WTPOLPFDT - Modify Policy Effective Date
     H DFTACTGRP(*NO) ACTGRP(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
      *****************************************************************
      * Revisions
      *
      * 06/11/25 C:344 Support: 119139
      *                Assign the user and program maintenance fields
      *                with the correct field entity.
      *
      * 10/13/23 C:344 Project: WC2857
      *                Remove Experience Modifier Updates and add the
      *                Effective Division grid.
      *
      * 09/15/23 C:342 Support: 107348 (VS)
      *                Update Large Deductible Effective Date/Year if
      *                user modify effective date of policy under
      *                policy review.
      *
      * 04/25/23 C:342 Support: 109161
      *                delete old effective date from file
      *                WAELP, WAEPP, WAILP, WADSP,WAMPP, WALDP, WAPPP
      *
      * 03/07/22 C:300 Support: 98489 (In-House)
      *                Quote Review --> Modify Effective Date Error
      *
      * 10/04/21 C:300 Support: 98489 (In-House)
      *                Correct Tabbing Order When Editing Effective Date
      *                in Quote Review
      *
      * 05/18/21 C:136 Project: WC1986
      *                Add EFT and 3rd Party files
      *
      * Project: WC2231
      *****************************************************************

     fWTPOLPFDT cf   e             workstn extfile('WTPOLPFDT')
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(PayrollSFL:rrnPayroll)
      *** Beg Add ***** 10/13/23 ***** WC2857 *****************************
     f                                     Sfile(EffDivSFL:rrnEffDiv)
      *** End Add ***** 10/13/23 ***** WC2857 *****************************
     f*
     f*
     fWmupl     if   e           k disk    Rename(PUP01:PUP00)
     fsmcol     if   e           k disk    Rename(PCO01:PCO00)
     fWmemp     if   e             disk    Rename(pem01:pem0p)
     fWmeml     uf   e           k disk    Rename(pem01:pem00)
     f*                                    INFDS(DBFDSD)
     fWdelp     if   e             disk    Rename(pel01:pel0p)
     FWdell     uf   e           k disk    Rename(pel01:pel00)
     FWxell     uf   e           k disk    Rename(pel01:pel02)
     FWdillq    uf   e           k disk    Rename(pil01:pil00)
     FWddslq    uf   e           k disk    Rename(pds01:pds00)
     FWdmplq    uf   e           k disk    Rename(pmp01:pmp00)
     FWdppl1    uf   e           k disk    Rename(ppp01:ppp00)
     FWmepl     uf   e           k disk    Rename(pep01:pep00)
     FWxmpp     o    e             disk    Rename(pmp01:pmp0p)
     FWmepl1e   uf   e           k disk    Rename(pep01:pep01)
     FWmahl     uf   e           k disk    Rename(pah01:pah00)
     FWabll1    uf   e           k disk    Rename(pbl01:pbl00)
     FWaqtl7    uf   e           k disk    Rename(pqt01:pqt00)
     fWmfnl     if   e           k disk    Rename(Pfn01:Pfn00)
     FWdf1l     if   e           k disk    Rename(Pf101:Pf100)
     FWmeal     uf   e           k disk    Rename(pea01:Pea00)
     FWmcml     if   e           k disk    Rename(pcm01:Pcm00)
     FWmall     uf   e           k disk    Rename(Pal01:Pal00)
     FWmool     uf   e           k disk    Rename(Poo01:Poo00)
     FWdvhl     uf   e           k disk    Rename(Pvh01:Pvh00)
     FWmdbl     uf   e           k disk    Rename(Pdb01:Pdb00)
     FWmanl     uf   e           k disk    Rename(Pan01:Pan00)
     FWdldl1    uf   e           k disk    Rename(Pld01:Pld00)
     FWtabl     if   e           k disk
     FWtbsp     if   e             disk    Rename(Pbs01:Pbs0p)
     FWmbxp     if   e             disk    Rename(Pbx01:Pbx0p)
      *** beg add ***** 05/18/21 **************************
     fWmefl6    uf   e           k disk    Rename(pef01:pef06)
     fWmp3l     uf   e           k disk    Rename(pp301:pp300)
      *** end add ***** 05/18/21 **************************
      *** beg add ***** 04/25/23 **************************
     FWaell     uf   e           k disk    Rename(pel01:pel01)
     FWaepl     uf   e           k disk    Rename(pep01:pep02)
     FWaill     uf   e           k disk    Rename(pil01:pil01)
     FWadsl     uf   e           k disk    Rename(pds01:pds01)
     FWampl     uf   e           k disk    Rename(pmp01:pmp01)
     FWaldl1    uf   e           k disk    Rename(pld01:pld01)
     FWappl     uf   e           k disk    Rename(ppp01:ppp01)
      *** end add ***** 04/25/23 **************************
      *** beg add ***** 09/15/23 **************************
     FWmldl1    uf   e           k disk    Rename(pld01:pld02)
      *** end add ***** 09/15/23 **************************
      *** Beg Add ***** 10/13/23 ***** WC2857 *****************************
     FWdell1    uf   e           k disk    Rename(pel01:pel03)
      *** End Add ***** 10/13/23 ***** WC2857 *****************************

     dpsds           esds                  extname(szq1p)
     dsysdat           s              7  0
     d xxCo#           s              3  0
     d xxFnd           s              3  0
     d xxEmp#          s              9  0
     d xxDiv           s              5  0
     d xxFyr           s              3  0
     d xxNefd          s              7  0
     d xxBgdt          s              7  0
     d xxTodt          s              7  0
     d svPfdt          s              7  0
     d svPtdt          s              7  0
     d xxfold          s              7  0
     d SVAGT#          s              5  0
     d svadld          s             11  2
     d sveapr          s             11  2
     d svempr          s             11  2
     d svsapr          s             11  2
     d svDiv           s              5  0
     d svFyr           s              3  0
     d svExt           s              5
     d svEcnt          s              7  0
     d svEfDt          s              7  0
     d agTeFd          s              7  0
     d svaedt          s              7  0
     d keyBlSch        s              3
     d outCltId        s              3
     d adCfLg          s              1
     d divCnt          s              5  0
     d srdto           s             10
     d isValid         s               n   Inz(*off)
     d isFound         s               n   Inz(*off)
      *** Beg Del *** 10/13/23 *** WC2857 **********************************
     d*updAllDiv       s               n   Inz(*off)
      *** End Del *** 10/13/23 *** WC2857 **********************************
     d message         s            100
     d charmsg         s            100
     d x               s              2  0
     d xxAgt#          s              5  0
     d xxEfDt          s              7  0
     d xxOeDt          s              7  0
     d xxOxDt          s              7  0
     d xxSpec          s              1
     d xxClas          s              5  0
     d xxstat          s              1
     d xxToYr          s              3  0
      *** Beg Add ***** 10/13/23 ***** WC2857 *****************************
     d xxBilCd         s              3    Inz
     d scUpdSkip       s              1    Inz
     d wcxdate         s              7  0 Inz
     d xxPfDt          s              7  0 Inz
     d chgPFDT         s               n   Inz
     d i1              s              6s 0 Inz
     d i2              s              6s 0 Inz
     d i3              s              6s 0 Inz
     d div1Max         s              6s 0 Inz
     d div2Max         s              6s 0 Inz
      *** End Add ***** 10/13/23 ***** WC2857 *****************************
     D dmsg            C                   CONST('Check all divisions -
     D                                     for error.')
     D xxSt            s              2
     D                 DS
     D  xxDT                   1      7  0
     D  xxYY                   2      3  0
     D  xxMM                   4      5  0
     D  xxDD                   6      7  0
     D  xxMDD                  4      7  0
     D  xxMMDD                 4      7  0
     D                 DS
     D  Cred                   1    154  0
     D                                     DIM(22)
     D  ELcref                 1      7  0
     D  ELcrex                 8     14  0
     D  ELdvef                15     21  0
     D  ELdvex                22     28  0
     D  ELd2ef                29     35  0
     D  ELd2ex                36     42  0
     D  ELilef                43     49  0
     D  ELilex                50     56  0
     D  ELi2ef                57     63  0
     D  ELi2ex                64     70  0
     D  ELdcef                71     77  0
     D  ELdcex                78     84  0
     D  ELmcef                85     91  0
     D  ELmcex                92     98  0
     D  ELscef                99    105  0
     D  ELscex               106    112  0
     D  ELsdef               113    119  0
     D  ELsdex               120    126  0
     D  ELd1d2               127    133  0
     D  ELd1x2               134    140  0
     D  ELd2d2               141    147  0
     D  ELd2x2               148    154  0

      *** Beg Add ***** 10/13/23 ***** WC2857 *****************************
     D  Div1                               Like(ELdiv)
     D                                     DIM(999)
     D  Div2                               Like(ELdiv)
     D                                     DIM(999)
      *** End Add ***** 10/13/23 ***** WC2857 *****************************
     D                 DS
     D  X7MXDT                 1      7  0
     D  X7MXCY                 1      3  0
     D  X7MXMM                 4      5  0

     D DBFDS3          DS
     D  DBRRN3               397    400B 0

     d WtPolPfDte      pr                  Extpgm('WTPOLPFDT')
     d  inPrds                       40A   const
     d  inCall                        1A
     d  inKC                          1
     d  inKL                          1
     d  inCo#                         3  0
     d  inFnd                         3  0
     d  inEmp#                        9  0
     d  inDiv                         5  0
     d  inFyr                         3  0
     d
     d WtPolPfDte      pi
     d  inPrds                       40A   const
     d  inCall                        1A
     d  inKC                          1
     d  inKL                          1
     d  inCo#                         3  0
     d  inFnd                         3  0
     d  inEmp#                        9  0
     d  inDiv                         5  0
     d  inFyr                         3  0

      * File Updation Proc.
     d UpdFiles        pr
      //*** Beg Add *** 10/13/23 *** WC2857 ********************************
     d @div                                like(ELdiv)
      //*** End Add *** 10/13/23 *** WC2857 ********************************

     d/copy sPrAtrium
     d/copy sPrDate
     d/copy sPrString
     d/copy sPrCompany
     d/copy wPrPolErr
     d/copy wPrEmplr

      * Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d outCltId                       3

      * Intialize the Screen fields
     d LoadInVal       pr

      * Main Processing
     d Main            pr

      * Program Start
     d ProgramStart    pr

      * Validate Panel Sreen Values
     d ValidPnl        pr

     D RcdPayrollDsp   pr
     D  indValue                      1n   const

      // Load Payroll Grid
     d LoadPayrollGrd  pr

      //*** Beg Add *** 10/13/23 *** WC2857 ********************************
      // Load Effective Divisions Grid
     d LoadEffDivGrd   pr

      // Update Policy Effective Date
     d UpdPolEffDt     pr

      // Update Policy Expiration Date
     d UpdPolExpDt     pr
      //*** End Add *** 10/13/23 *** WC2857 ********************************

     C*****Dtcymdp       Plist
     C*****              Parm                    Srdti             8 0
     C*****              Parm      *BLANKS       Srffmt            8
     C*****              Parm      '*CYMD'       Srtfmt            8
     C*****              Parm                    Srdto            10
     C*****              Parm                    Srerr             1
      /Free
       // Start Of Program
       ProgramStart();

       // Main Procesing
       Main();

       *InLr = *on;

      /End-Free

       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /Free

       // Intialize the Screen fields
       LoadInVal();

       divCnt = 0;

       Setll (xxCo#:xxFnd:xxEmp#) pem00;
       Reade (xxCo#:xxFnd:xxEmp#) pem00;

       Dow not %eof(wmeml);
          divCnt = divCnt + 1;
          Reade (xxCo#:xxFnd:xxEmp#) pem00;
       Enddo;

       scFnd = xxFnd;
       scEmp# = xxEmp#;
       scDiv = xxDiv;
       scPolYr = xxFyr;
       scToYr = xxFyr;

       Chain(n) (xxCo#:xxFnd) pfn00;
       If %found (wmfnl);
          scName = FNname;
       Endif;

       scEmpName = EMname;

       Chain(n) (xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr) pel00;
       If %found(Wdell);
          svPfDt = ELpfdt;
          svPtDt = ELptdt;
          scPfDt = dtMDYY(ELPFDT);
          scPtdt = dtMDYY(ELPTDT);
          scAndt = dtMDYY(ELANDT);
          scBsdt = dtMDYY(ELBSDT);
          //*** Beg Del ***** 10/13/23 *** WC2857 ************************
          //***scExm1 = ELexm1;
          //***scEM1Date =  dtMDYY(ELEMD1);
          //***scEXM2 = ELEXM2;
          //***scEM2Date =  dtMDYY(ELEMD2);
          //***scEXM3 = ELEXM3;
          //***scEM3Date =  dtMDYY(ELEMD3);
          //***scARP1 = ELARP1;
          //***scARP2 = ELARP2;
          //***scARP3 = ELARP3;
          //***scExMd1Nf = ELexMd1Nf;
          //***scExMd2Nf = ELexMd2Nf;
          //***scExMd3Nf = ELexMd3Nf;
          //*** End Del ***** 10/13/23 *** WC2857 ************************
          scBsCd  = ELbscd;
       Endif;

       LoadPayrollGrd();
       //*** Beg Add ***** 10/13/23 *** WC2857 ****************************
       visEffDiv = *off;
       visUpdSkip = *off;
       //*** End Add ***** 10/13/23 *** WC2857 ****************************

       //*** Beg Del ***** 10/13/23 *** WC2857 ****************************
       // ARAP
       //***arap = *off;

       //***xxSt = GetRatingState(ELco#:ELfnd:ELemp#:ELdiv);
       //***If xxSt = 'MA';
          //***arap = *on;
       //***Endif;
       //*** End Del ***** 10/13/23 *** WC2857 ****************************

       Return;

      /End-Free

     p ProgramStart    e

       //***************************************************************************//
       //===================================================================
       // Load Payroll Grid
       //===================================================================
     P LoadPayrollGrd  B
     D LoadPayrollGrd  PI
      /free

       // Clear the subfile.
       RcdPayrollDsp(*off);
       clrPayroll = *On;
       Write RcdPolEfDt;
       clrPayroll = *Off;
       RcdPayrollDsp(*on);
       rrnPayroll = 0;
       //*** Beg Add *** 10/13/23 *** WC2857 *******************************
       pnlHeight = 300;
       //*** End Add *** 10/13/23 *** WC2857 *******************************

       // Populate the subfile
       Setll (xxCo#:xxFnd:scEmp#:scDiv:scPolYr) pep00;
       Reade (xxCo#:xxFnd:scEmp#:scDiv:scPolYr) pep00;

       Dow not %eof;
          scEPEFDT = CvtToDate(EPEFDT);
          scEPCLAS = EPCLAS;
          scEPType = EPSAFL;
          scEPEAPR = EPEAPR;
          If EPSTAT = 'A';
             scEPStatus = 'Active';
          Else;
             scEPStatus = 'Inactive';
          Endif;

          rrnPayroll =  rrnPayroll + 1;
          If rrnPayroll = 1;
            scEPsel = *On;
          Else;
            scEPsel = *Off;
          Endif;
          Write PayrollSFL;

          Reade (xxCo#:xxFnd:scEmp#:scDiv:scPolYr) pep00;
       Enddo;
       //* Position payroll grid at top at all times.
       rrnPayroll = 1;

      /end-free
     P LoadPayrollGrd  E
       //***************************************************************************//
       //*** Beg Add ***** 10/13/23 *** WC2857 ****************************
       //===================================================================
       // Load Effective Divisions Grid
       //===================================================================
     P LoadEffDivGrd   B
     D LoadEffDivGrd   PI

       // Clear the subfile.
       dspEffDSfl = *Off;
       dspEffDCtl = *Off;
       clrEffDiv = *On;
       Write RcdPolEfDt;

       // Reset indicators
       dspEffDSfl = *On;
       dspEffDCtl = *On;
       clrEffDiv = *Off;
       rrnEffDiv = 0;
       visEffDiv = *Off;
       visUpdSkip = *Off;
       pnlHeight = 300;
       scSavVer = 'Verify';
       Clear i1;
       Clear i2;
       Clear div1Max;
       Clear div2Max;

       // Convert New Policy Effective to CYYMMDD Format
       xxPfDt = DateTo7(%Date(scPfDt:*usa));

       // Verify the new Policy Effective against each division in the
       // Employer Detail file.
       // If divisions have a Policy From Date that is greater than the
       // newly entered Policy Effective Date then add rec to the Effective
       // Division grid. Also don't include division if the Policy Effective
       // Date is the same date as the newly entered Policy From Date.

       Setll (xxCo#:xxFnd:xxFyr:xxEmp#) pel03;
       Dou %eof(WDELL1);
          Reade(n) (xxCo#:xxFnd:xxFyr:xxEmp#) pel03;
          If %eof(WDELL1);
             Leave;
          Endif;

          Select;
          // Skip when division is the same
          When ELdiv = scDiv;
             i1 += 1;
             Div1(i1) = ELdiv;
             Iter;

          // Moving policy effective date forward
          When xxPfDt > svPfDt and
             ELpfdt > xxPfDt and ELpfdt <> svPfDt;
             i2 += 1;
             Div2(i2) = ELdiv;

          // Moving policy effective date backward
          When xxPfDt < svPfDt and
             ELpfdt > xxPfDt and ELpfdt <> svPfDt;
             i2 += 1;
             Div2(i2) = ELdiv;

          Other;
             i1 += 1;
             Div1(i1) = ELdiv;
             Iter;
          Endsl;

          visEffDiv = *On;
          visUpdSkip = *On;
          pnlHeight = 500;
          scEffDiv =  ELdiv;
          scPolEfDt = CvtToDate(ELpfdt);
          scPolExpDt = CvtToDate(ELptdt);
          scBilStrDt = CvtToDate(ELbsdt);
          scAnnRatDt = CvtToDate(ELandt);

          // Construct Payment Billing Schedule Code
          xxBilCd = %trim(%Editc(ELbscd:'X'));
          Chain(n) (xxCo#:xxBilCd) Pbs01;
             If %found(WTABL);
                scPayPlan = %trim(%Editc(ELbscd:'X')) + ' ' +
                %trim(BSdesc);
             Endif;

          // Retrieve Cancellation Date
          wcxdate = getCxDate(ELco#:ELfnd:ELemp#:ELdiv);
          scCnclDt = CvtToDate(wcxdate);
          rrnEffDiv += 1;
          Write EffDivSFL;
       Enddo;

       div1Max = i1;
       div2Max = i2;

       // Position grid at top at all times.
       rrnEffDiv = 1;
     P LoadEffDivGrd   E
       //***************************************************************************//
       //===================================================================
       // Update Policy Effective Date
       //===================================================================
     P UpdPolEffDt     B
     D UpdPolEffDt     PI

       // Update Employer Detail recs where the division is the same as the
       // entered division or policy effective date is less than or equal
       // to the entered policy effective date.
       Setll (xxCo#:xxFnd:xxFyr:xxEmp#) pel03;
       Reade (xxCo#:xxFnd:xxFyr:xxEmp#) pel03;
       Dow not %eof(WDELL1);
          xxPfDt = DateTo7(%Date(scPfdt:*usa));
          If ELdiv = scDiv or ELpfdt <= xxPfDt or ELpfdt = svPfDt;
             ELpfdt = xxPfDt;
             ELpgm   = Q1pgm;
             ELuser  = Q1user;
             ELdt    = Dateto7(SystemDate());
             ELtime  = Timeto6(SystemTime());
             Update pel03;
          Endif;
          Reade (xxCo#:xxFnd:xxFyr:xxEmp#) pel03;
       Enddo;
       Unlock WDELL1;

     P UpdPolEffDt     E
       //***************************************************************************//
       //===================================================================
       // Update Policy Expiration Date
       //===================================================================
     P UpdPolExpDt     B
     D UpdPolExpDt     PI

       // Regardless of the skip/update radio button chosen, if the policy
       // expiration date on the prompt screen was changed then update
       // all Employer Detail division records with the new policy expiration
       // date value.
       Setll (xxCo#:xxFnd:xxFyr:xxEmp#) pel03;
       Reade (xxCo#:xxFnd:xxFyr:xxEmp#) pel03;

       Dow not %eof(WDELL1);
          If ELptdt <> DateTo7(%Date(scPtdt:*usa));
             ELptdt = DateTo7(%Date(scPtdt:*usa));
             ELpgm   = Q1pgm;
             ELuser  = Q1user;
             ELdt    = Dateto7(SystemDate());
             ELtime  = Timeto6(SystemTime());
             Update pel03;
          Endif;
          Reade (xxCo#:xxFnd:xxFyr:xxEmp#) pel03;
       Enddo;
       Unlock WDELL1;

     P UpdPolExpDt     E
       //*** End Add ***** 10/13/23 *** WC2857 ****************************

       //===================================================================
       // Load display indicators for sub-files in rcdPayroll
       //===================================================================
     P RcdPayrollDsp   b
     D RcdPayrollDsp   pi
     D  indValue                      1n   const
      /Free
       dspPaySfl  = indValue;
       dspPayCtl  = indValue;

      /End-Free
     P RcdPayrollDsp   e

       //===================================================================
       // LoadInVal - Intialize the Screen fields
       //===================================================================
     p LoadInVal       b
     d LoadInVal       pi

      /Free

       Clear RcdPolEfDt;

       //*** Beg Del ***** 10/13/23 *** WC2857 ****************************
       //***scUpdExp = 'Y';
       //*** End Del ***** 10/13/23 *** WC2857 ****************************
       //*** Beg Add ***** 10/13/23 *** WC2857 ****************************
       // Initially set the submit button value to 'Save'
       scSavVer = 'Save';
       //*** End Add ***** 10/13/23 *** WC2857 ****************************

       // Get the client id
       GetClientID(outCltId);

       // Retrieve Output Queue of the User.
       Chain(n) Q1User pup00;
       If %Found(Wmupl);
          xxCo# = UPco#;
          xxFnd = UPfnd;
          xxEmp# = UPemp#;
          xxDiv = UPdiv;
       EndIf;

       If Q1prms > 3;

          xxCo# = inCo#;
          xxFnd = inFnd;
          xxEmp# = inEmp#;
          xxDiv = inDiv;
          xxFyr = inFyr;

       Endif;

      /End-Free
     p LoadInVal       e

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

      /Free

       // Process until exit button is pressed.
       Dou BtnCancel = *On ;

          // Display Screen
          Exsr ExfmtScreens;

          Select;
          When BtnSubmit = *on;
             // Validate Edits
             svDiv = scDiv;
             xxDiv = scDiv;
             //*** Beg Del ***** 10/13/23 *** WC2857 **********************
             //***If scUpdExp <> 'Y';
                //***Chain(n) (xxCo#:xxFnd:xxEmp#:xxDiv) pem00;
             //***Else;
             //*** End Del ***** 10/13/23 *** WC2857 **********************
                Chain(n) (xxCo#:xxFnd:xxEmp#) pem00;
             //*** Beg Del ***** 10/13/23 *** WC2857 **********************
             //***Endif;
             //*** End Del ***** 10/13/23 *** WC2857 **********************
             If %found(wmeml);
                Dow not %eof(wmeml);
                   xxDiv = EMdiv;
                   ValidPnl();
                   Select;
                   When isValid = *on;
                      Leave;
                   //*** Beg Del ***** 10/13/23 *** WC2857 ****************
                   //***When scUpdExp <> 'Y';
                      //***Leave;
                   //*** End Del ***** 10/13/23 *** WC2857 ****************
                   Endsl;
                   Reade (xxCo#:xxFnd:xxEmp#) pem00;
                Enddo;
             Endif;

             If isValid = *Off;
                //*** Beg Del *** 10/13/23 *** WC2857 *********************
                //***UpdFiles();
                //*** End Del *** 10/13/23 *** WC2857 *********************
                //*** Beg Add *** 10/13/23 *** WC2857 *********************
                // Process until Cancel Button is selected.
                Clear erUpdRad;
                Clear mgUpdRad;
                Clear erSkipRad;
                Clear mgSkipRad;

                Dou BtnCancel = *On ;

                   // Create the Effective Division Grid/ Subfile by
                   // verifying the Policy From Date against each division
                   // and create the 2nd subfile accordingly.
                   If isValid = *Off;
                      LoadEffDivGrd();
                   Endif;

                   Exsr ExfmtScreens;
                   ValidPnl();

                   Select;
                   // Errors while validating panel then redisplay.
                   When isValid = *On;
                      // Clear the subfile.
                      dspEffDSfl = *Off;
                      dspEffDCtl = *Off;
                      clrEffDiv = *On;
                      Write RcdPolEfDt;

                   // If Policy Effective was changed then redisplay.
                   When chgPFDT;

                   // If 'Update' and 'Skip' button not selected then
                   // display message that a choice must be made.
                   When visUpdSkip = *On and scUpdSkip = ' ';
                      erUpdRad = *on;
                      mgUpdRad = 'A Choice Must Be Made.';
                      erSkipRad = *on;
                      mgSkipRad = 'A Choice Must Be Made.';
                      Iter;

                   // If 'Skip' button selected, check each div to
                   // determine if not listed in the Policy Effective
                   // Grid. If div not listed in grid then execute the
                   // UPDFILE procedure to fully update the files.
                   When visUpdSkip = *On and scUpdSkip = 'S';

                      For i3 = 1 To div1Max;
                         UpdFiles(Div1(i3));
                      Endfor;
                      UpdPolExpDt();
                      Leave;

                   // If 'Update' button selected, update files
                   // and update policy expiration dates.
                   When visUpdSkip = *On and scUpdSkip = 'U';

                      For i3 = 1 To div1Max;
                         UpdFiles(Div1(i3));
                      Endfor;

                      For i3 = 1 To div2Max;
                         UpdFiles(Div2(i3));
                      Endfor;
                      UpdPolExpDt();
                      Leave;

                   // If 'Update' button selected, update files
                   When visUpdSkip = *Off and btnSubmit = *On;
                      For i3 = 1 To div1Max;
                         UpdFiles(Div1(i3));
                      Endfor;
                      Leave;

                   Endsl;

                   Clear erUpdRad;
                   Clear mgUpdRad;
                   Clear erSkipRad;
                   Clear mgSkipRad;
                Enddo;
                //*** End Add *** 10/13/23 *** WC2857 **********************
                Leave;
             Endif;

          When BtnCancel = *on;
             Leave;

          // do nothing - catch tab click event
          When tabClick = *on;
             tabClick = *off;

          When BtnRefresh = *On;
             BtnRefresh =*Off;

             ProgramStart();

          EndSl;

       Enddo;

       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

          Exfmt RcdPolEfDt;

       Endsr;

      /End-Free
     p Main            e


       //===================================================================
       // UpdFiles - Update Files Procedure
       //===================================================================
     p UpdFiles        b
     d UpdFiles        pi
      //*** Beg Add *** 10/13/23 *** WC2857 ********************************
     d @div                                like(ELdiv)
      //*** End Add *** 10/13/23 *** WC2857 ********************************
      /Free
                //***Beg Del*** 10/13/23 *** WC2857 ************************
                //***xxDiv = svDiv;
                //***If scUpdExp <> 'Y';
                   //***Chain (xxCo#:xxFnd:xxEmp#:xxDiv) pem00;
                //***Else;
                   //***Chain (xxCo#:xxFnd:xxEmp#) pem00;
                //***EndIf;
                //***End Del*** 10/13/23 *** WC2857 ************************

                //***Beg Add*** 10/13/23 *** WC2857 ************************
                Chain (xxCo#:xxFnd:xxEmp#:@div) pem00;
                //***Beg End*** 10/13/23 *** WC2857 ************************
                If %found(wmeml);
                   //***Beg Del*** 10/13/23 *** WC2857 *********************
                   //*** Dow not %eof(wmeml);
                   //***End Del*** 10/13/23 *** WC2857 *********************
                      xxDiv = EMdiv;
                      Chain (xxCo#:xxFnd:xxEmp#:EMdiv:xxFyr) pel00;
                      //***Beg Del*** 10/13/23 *** WC2857 ******************
                      //***If not %found(wdell);
                         //***Update pem00;
                         //***Reade (xxCo#:xxFnd:xxEmp#) pem00;
                         //***Iter;
                      //***Endif;
                      //***End Del*** 10/13/23 *** WC2857 ******************

                      //*** Beg Del *** 10/13/23 *** WC2857 **************
                      //***If ELdiv = svDiv;
                         //***updAllDiv = *off;
                      //***Else;
                         //***updAllDiv = *on;
                      //***Endif;
                      //*** End Del *** 10/13/23 *** WC2857 **************

                      xxoedt = ELpfdt;
                      xxoxdt = ELptdt;

                      //Update Modify Policy Effective Date Log File
                      MPco# = xxCo#;
                      MPfnd =   xxFnd;
                      MPemp# =  xxEmp#;
                      MPdiv   = xxDiv;
                      MPfyr   = scToYr;
                      MPlogdt = Dateto7(SystemDate());
                      MPlogtm = Timeto6(SystemTime());
                      MPpfyr  = ELfyr;
                      MPppfdt = ELpfdt;
                      MPpptdt = ELptdt;
                      MPpandt = ELandt;
                      MPpbsdt = ELbsdt;
                      MPpbscd = ELbscd;
                      MPpprom = ELprom;
                      MPpexm1 = ELexm1;
                      MPpemd1 = ELemd1;
                      MPpenf1 = ELexmd1nf;
                      MPpexm2 = ELexm2;
                      MPpemd2 = ELemd2;
                      MPpenf2 = ELexmd2nf;
                      MPpexm3 = ELexm3;
                      MPpemd3 = ELemd3;
                      MPpenf3 = ELexmd3nf;
                      MPparp1 = ELarp1;
                      MPparp2 = ELarp2;
                      MPparp3 = ELarp3;
                      MPpfdt  = dtcymd(scPfDt);
                      MPptdt  = dtcymd(scPtDt);
                      MPandt  = dtcymd(scPtDt);
                      MPbsdt  = dtcymd(scBsDt);
                      MPprom  = ELprom;
                      //*** Beg Del ***** 10/13/23 *** WC2857 **************
                      //***MPexm1  = scExM1;
                      //***MPemd1  = dtcymd(SCEM1Date);
                      //***MPenf1  = scExMd1Nf;
                      //***MPexm2  = scExM2;
                      //***MPemd2  = dtcymd(SCEM2Date);
                      //***MPenf2  = scExMd2Nf;
                      //***MPexm3  = scExM3;
                      //***MPemd3  = dtcymd(SCEM3Date);
                      //***MPenf3  = scExMd3Nf;
                      //***MParp1  = scArP1;
                      //***MParp2  = scArP2;
                      //***MParp3  = scArP3;
                      //*** End Del ***** 10/13/23 *** WC2857 **************
                      MPuser  = Q1user;
                      MPpgm   = Q1pgm;
                      MPtime  = Timeto6(SystemTime());
                      MPdt    = Dateto7(SystemDate());
                      MPiusr  = Q1user;
                      MPipgm  = Q1pgm;
                      MPitim  = Timeto6(SystemTime());
                      MPidt   = Dateto7(SystemDate());

                      Write  pmp0p;

                      If ELandt <> dtCYMD(scAnDt) and scAndt <> 0;
                         xxEfdt = ELandt + 10000;
                         Chain (xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr:xxEfDt) pep00;
                         If %found(wmepl);
                            Dow not %eof(wmepl);

                               EPefdt = dtCYMD(scAnDt) + 10000;
                               EPfyr = xxToYr;
                               xxclas = EPclas;
                               xxSpec = EPspec;
                               svEfDt = EPefdt;
                               svExt  = EPext;
                               svFyr  = xxToYr;
                               svEaPr = EPeapr;
                               svEmPr = EPempr;
                               svEcNt = EPecnt;
                               svAdLd = EPadld;
                               svSaPr = EPsapr;

                               Chain (xxCo#:xxFnd:svFyr:xxEmp#:xxDiv:
                                xxClas:xxSpec:svExt:svEfDt) pep01;
                               If %found(wmepl1e);
                                  EPeapr = EPeapr + svEaPr;
                                  EPempr = EPempr + svEmPr;
                                  EPecnt = EPecnt + svEcNt;
                                  EPadld = EPadld + svAdLd;
                                  EPsapr = EPsapr + svSaPr;
                                  Delete pep01;
                               //*** Beg Add *** 10/13/23 *** WC2857 ***********************
                               Else;
                                  Unlock WMEPL1E;
                               //*** End Add *** 10/13/23 *** WC2857 ***********************
                               Endif;
                               EPpgm  = Q1pgm;
                               EPuser = Q1user;
                               EPdt   = Dateto7(SystemDate());
                               EPtime = Timeto6(SystemTime());

                               Update pep00;
                               Reade (xxCo#:xxFnd:xxEmp#:xxDiv:
                                xxFyr:xxEfDt) pep00;
                            Enddo;
                            //*** Beg Add *** 10/13/24 *** WC2857 *******   ****************
                            Unlock WMEPL1E;
                            //*** End Add *** 10/13/24 *** WC2857 *******   ****************
                         Endif;
                      Endif;

                      If ELpfdt <> Dtcymd(scPfDt);
                         xxEfDt = ELpfdt;
                         Chain (xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr:xxEfDt)
                          pep00;
                         If %found(wmepl);
                            Dow not %eof(wmepl);
                               EPefdt = Dtcymd(scPfdt);
                               EPFyr   = xxToYr;
                               xxClas  = EPCLAS;
                               xxSpec  = EPSPEC;
                               svEfDt  = EPEFDT;
                               svExt   = EPEXT ;
                               svFyr   = xxToYr;
                               svEaPr  = EPEAPR;
                               svEmPr  = EPEMPR;
                               svEcNt  = EPECNT;
                               svAdLd  = EPADLD;
                               svSaPr  = EPSAPR;
                               Chain (xxCo#:xxFnd:svFyr:xxEmp#:xxDiv:
                                xxClas:xxSpec:svExt:svEfDt) pep01;
                               If %found(wmepl1e);
                                  EPeapr = EPeapr + svEaPr;
                                  EPempr = EPempr + svEmPr;
                                  EPecnt = EPecnt + svEcNt;
                                  EPadld = EPadld + svAdLd;
                                  EPsapr = EPsapr + svSaPr;
                                  Delete pep01;
                               //*** Beg Add *** 10/13/23 *** WC2857 ***********************
                               Else;
                                  Unlock WMEPL1E;
                               //*** End Add *** 10/13/23 *** WC2857 ***********************
                               Endif;
                               EPpgm  = Q1pgm;
                               EPuser = Q1user;
                               EPdt   = Dateto7(SystemDate());
                               EPtime = Timeto6(SystemTime());
                               //*** End Add *** 10/13/23 *** WC2857 ***********************
                               EPpgm  = Q1pgm;
                               EPuser = Q1user;
                               EPdt   = Dateto7(SystemDate());
                               EPtime = Timeto6(SystemTime());

                               Update pep00;
                               Reade (xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr:
                                xxEfDt) pep00;
                            Enddo;
                            Unlock WMEPL1E;
                         Endif;

                         Chain (xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr) pah00;
                         If %found(wmahl);
                            Dow not %eof(wmahl) and AHstat = 'I';
                               Reade (xxCo#:xxFnd:xxEmp#:xxDiv
                                :xxFyr) pah00;
                            Enddo;
                         Endif;

                         If not %eof(wmahl);
                            xxStat = 'Q';
                            xxAgt# = AHagt#;
                            Chain (xxCo#:xxFnd:xxEmp#:xxDiv:xxStat:
                             xxAgt#) pqt00;
                            If %found(WAQTL7);
                               QTefdt = dtcymd(scPfDt);
                               QTpgm  = Q1pgm;
                               QTuser = Q1user;
                               QTdt   = Dateto7(SystemDate());
                               QTtime = Timeto6(SystemTime());
                               Update pqt00;
                            Endif;
                         Endif;
                      Endif;

                      EMefd1  = Dtcymd(scPfdt);
                      EMefd2  = 0;
                      EMefd3  = 0;
                      EMefd4  = 0;
                      EMefd5  = 0;
                      EMcad1  = 0;
                      EMcad2  = 0;
                      EMcad3  = 0;
                      EMcad4  = 0;
                      EMcad5  = 0;
                      EMuser  = Q1pgm;
                      EMpgm   = Q1user;
                      EMdt    = Dateto7(SystemDate());
                      EMtime  = Timeto6(SystemTime());

                      Update pem00;

                      //*** Beg Del ***** 10/13/23 *** WC2857 *************
                      //***If updAllDiv  = *off;
                         //***ELexM1 = scExm1;
                         //***ELEMD1 = dtcymd(SCEM1Date);
                         //***ELexM2 = scExm2;
                         //***ELEMD2 = dtcymd(SCEM2Date);
                         //***ELexM3 = scExm3;
                         //***ELEMD3 = dtcymd(SCEM3Date);
                         //***ELexmd1nf = scExMd1Nf;
                         //***ELexmd1nf = scExMd2Nf;
                         //***ELexmd1nf = scExMd2Nf;
                         //***ELarp1 = scARP1;
                         //***ELarp2 = scARP2;
                         //***ELarp3 = scARP3;
                      //***Endif;
                      //*** End Del ***** 10/13/23 *** WC2857 *************

                      If ELandt <> dtcymd(scAnDt);
                         ELd1d2 = 0;
                         ELd1x2 = 0;
                         ELd2d2 = 0;
                         ELd2x2 = 0;
                      Endif;

                      ELandt = dtcymd(scAnDt);
                      ELpfdt = dtcymd(scPfDt);
                      ELptdt = dtcymd(scPtDt);
                      ELbsdt = dtcymd(scBsDt);

                      Exsr srcred;

                      ELbscd = scBsCd;
                      //*** Beg Chg *** 10/13/23 *** WC2857*****************
                      //***ELuser  = Q1pgm;
                      //***ELpgm   = Q1user;
                      ELuser  = Q1user;
                      ELpgm   = Q1pgm;
                      //*** End Chg *** 10/13/23 *** WC2857*****************
                      ELdt    = Dateto7(SystemDate());
                      ELtime  = Timeto6(SystemTime());
                      ELfyr = xxToYr;
                      Update pel00;

                      //Update agent history record with AHefdt = saved AHefdt from
                      //AHefdt edit. Update all records with new year.
                      If scToYr <> scPolYr or Adcflg = *on;
                         Setll (xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr) pah00;
                         Reade (xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr) pah00;
                         Dow not %eof(wmahl);
                            If adcflg = *on and AHagt# = svAgt# and
                             AHefdt = svAeDt and AHstat = 'A';
                               AHefdt = agTeFd;
                            Endif;

                            If scToYr <> scPolYr;
                               AHfyr = scToYr;
                            Endif;

                            Update pah00;
                            Reade (xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr) pah00;
                         Enddo;
                      Endif;

                      //Delete Billing Transaction records
                      Chain (ELco#:ELfnd:ELemp#:ELdiv:scPolYr) pbl00;
                      If %found(wabll1);
                         Dow not %eof(Wabll1);
                            Delete pbl00;
                            Reade (ELco#:ELfnd:ELemp#:ELdiv:scPolYr) pbl00;
                         Enddo;
                      Endif;

                      //Update Additional Location Effective Date.
                      Chain (xxCo#:xxFnd:xxEmp#:xxDiv) pal00;
                      If %found(Wmall);
                         Dow not %eof(Wmall );
                            ALefdt = ELpfdt;
                            ALuser= Q1pgm;
                            ALpgm = Q1user;
                            ALdt  = Dateto7(SystemDate());
                            ALtime= Timeto6(SystemTime());
                            Update pal00;
                            Reade (xxCo#:xxFnd:xxEmp#:xxDiv) pal00;
                         Enddo;
                      Endif;

                      //Update Additional Named Insured Effective Date.
                      Chain (xxCo#:xxFnd:xxEmp#:xxDiv) pan00;
                      If %found(Wmanl);
                         Dow not %eof(Wmanl);
                            ANefdt = ELpfdt;
                            ANuser= Q1pgm;
                            ANpgm = Q1user;
                            ANdt  = Dateto7(SystemDate());
                            ANtime= Timeto6(SystemTime());
                            Update pan00;
                            Reade (xxCo#:xxFnd:xxEmp#:xxDiv) pan00;
                         Enddo;
                      Endif;


                      //Update Owners/Officers Effective Date.
                      Chain (xxCo#:xxFnd:xxEmp#:xxDiv) poo00;
                      If %found(Wmool);
                         Dow not %eof(Wmool);
                            OOefdt = ELpfdt;
                            OOuser= Q1pgm;
                            OOpgm = Q1user;
                            OOdt  = Dateto7(SystemDate());
                            OOtime= Timeto6(SystemTime());
                            Update poo00;
                            Reade (xxCo#:xxFnd:xxEmp#:xxDiv) poo00;
                         Enddo;
                      Endif;

                      //Update Direct Debit Effective Date.
                      Chain (xxCo#:xxFnd:xxEmp#:xxDiv) pdb00;
                      If %found(Wmdbl);
                         Dow not %eof(Wmdbl);
                            DBefdt = ELpfdt;
                            DBuser= Q1pgm;
                            DBpgm = Q1user;
                            DBdt  = Dateto7(SystemDate());
                            DBtime= Timeto6(SystemTime());
                            Update pdb00;
                            Reade (xxCo#:xxFnd:xxEmp#:xxDiv) pdb00;
                         Enddo;
                      Endif;

                      //Update Vendor History Efective Date and Year.
                      Chain (xxCo#:xxFnd:xxEmp#:xxDiv:scPolYr) pvh00;
                      If %found(Wdvhl);
                         Dow not %eof(Wdvhl);
                            VHfyr = scToYr;
                            VHefdt = ELpfdt;
                            VHuser= Q1pgm;
                            VHpgm = Q1user;
                            VHdt  = Dateto7(SystemDate());
                            VHtime= Timeto6(SystemTime());
                            Update pvh00;
                            Reade (xxCo#:xxFnd:xxEmp#:xxDiv:scPolYr) pvh00;
                         Enddo;
                      Endif;

       //*** beg add ***** 05/18/21 *****************************
        Setll (xxCo#:xxFnd:xxEmp#:xxDiv) pef06;
        Dou %Eof(WMEFL6);
           Reade (xxCo#:xxFnd:xxEmp#:xxDiv) pef06;
           If %Eof;
              Leave;
           EndIf;
           If EFefdt = EFexdt;
              Iter;
           EndIf;
           Select;
           When EFEnvFlg <> 'Q';
              Iter;
           When ELpfdt < EFefdt;
              EFefdt = ELpfdt;
              EFsfyr = scToYr;
           When ELpfdt > EFefdt;
              EFefdt = Dateto7(SystemDate());
              EFsfyr = scToYr;
           Other;
              Iter;
           EndSl;
           EFuser = Q1pgm;
           EFpgm  = Q1user;
           EFdt   = Dateto7(SystemDate());
           EFtime = Timeto6(SystemTime());
           Update pef06;
        Enddo;
        Setll (xxCo#:xxFnd:xxEmp#:xxDiv) pp300;
        Dou %Eof(WMP3L);
           Reade (xxCo#:xxFnd:xxEmp#:xxDiv) pp300;
           If %Eof;
              Leave;
           EndIf;
           If P3efdt = P3exdt;
              Iter;
           EndIf;
           Select;
           When P3EnvFlg <> 'Q';
           When ELpfdt < P3efdt;
              P3efdt = ELpfdt;
              P3sfyr = scToYr;
           When ELpfdt > P3efdt;
              P3efdt = Dateto7(SystemDate());
              P3sfyr = scToYr;
           Other;
              Iter;
           Endsl;
           P3user = Q1pgm;
           P3pgm  = Q1user;
           P3dt   = Dateto7(SystemDate());
           P3time = Timeto6(SystemTime());
           Update pp300;
        Enddo;
       //*** end add ***** 05/18/21 *****************************

                      //Update Location Year Detail Year.
                      Chain (xxCo#:xxFnd:xxEmp#:xxDiv:scPolYr) pld00;
                      If %found(Wdldl1);
                         Dow not %eof(Wdldl1);
                            LDfyr = scToYr;
                            //*** Beg Chg **** 06/11/25 ********************
                            //***LDuser= Q1pgm;
                            //***LDpgm = Q1user;
                            LDuser = Q1user;
                            LDpgm  = Q1pgm;
                            //*** End Chg **** 06/11/25 ********************
                            LDdt  = Dateto7(SystemDate());
                            LDtime= Timeto6(SystemTime());
                            Update pld00;
                            Reade (xxCo#:xxFnd:xxEmp#:xxDiv:scPolYr) pld00;
                         Enddo;
                      Endif;

                     //*** beg add ***** 09/15/23 *****************************
                      //Update large deductible effective date and year
                      Chain (xxCo#:xxFnd:xxEmp#:xxDiv:scPolYr) pld02;
                      If %found(Wmldl1);
                         Dow not %eof(Wmldl1);
                            LDefd1 = ELpfdt;
                            LDfyr = scToYr;
                            //*** Beg Chg **** 06/11/25 ********************
                            //***LDuser= Q1pgm;
                            //***LDpgm = Q1user;
                            LDuser = Q1user;
                            LDpgm  = Q1pgm;
                            //*** End Chg **** 06/11/25 ********************
                            LDdt  = Dateto7(SystemDate());
                            LDtime= Timeto6(SystemTime());
                            Update pld02;
                            Reade (xxCo#:xxFnd:xxEmp#:xxDiv:scPolYr) pld02;
                         Enddo;
                      Endif;
                     //*** end add ***** 09/15/23 *****************************

                      //Delete prior A/R records
                      Chain (xxCo#:xxFnd:xxEmp#:xxDiv:scPolYr) pea00;
                      If %found(wmeal);
                         Dow not %eof(Wmeal);
                            If EAarty = 'A' or EAarty = 'B' or EAarty = 'D'
                             or EAarty = 'F' or EAarty = 'H';
                               Delete pea00;
                            Endif;
                            Reade (xxCo#:xxFnd:xxEmp#:xxDiv:scPolYr) pea00;
                         Enddo;
                      Endif;

                      //Delete unbilled premium data
                      Chain (xxCo#:xxFnd:xxEmp#:xxDiv:scPolYr) pel02;
                      If %found(wxell);
                         Dow not %eof(Wxell);
                            Delete pel02;
                            Reade (xxCo#:xxFnd:xxEmp#:xxDiv:scPolYr) pel02;
                         Enddo;
                      Endif;

                      //Delete Employer Increased Limits records
                      Chain (xxCo#:xxFnd:xxEmp#:xxDiv:scPolYr) pil00;
                      If %found(wdillq);
                         Dow not %eof(wdillq);
                            Delete pil00;
                            Reade (xxCo#:xxFnd:xxEmp#:xxDiv:scPolYr) pil00;
                         Enddo;
                      Endif;

                      //Delete Employer Discount Detail File records
                      Chain (xxCo#:xxFnd:xxEmp#:xxDiv:scPolYr) pds00;
                      If %found(wddslq);
                         Dow not %eof(wddslq);
                            Delete pds00;
                            Reade (xxCo#:xxFnd:xxEmp#:xxDiv:scPolYr) pds00;
                         Enddo;
                      Endif;

                      //Delete Employer Premium File records
                      Chain (xxCo#:xxFnd:xxEmp#:xxDiv:scPolYr) pmp00;
                      If %found(wdmplq);
                         Dow not %eof(wdmplq);
                            Delete pmp00;
                            Reade (xxCo#:xxFnd:xxEmp#:xxDiv:scPolYr) pmp00;
                         Enddo;
                      Endif;

                      //Delete Employer Detail Premium Periods records
                      Chain (xxCo#:xxFnd:xxEmp#:xxDiv:scPolYr) ppp00;
                      If %found(wdppl1);
                         Dow not %eof(wdppl1);
                            Delete ppp00;
                            Reade (xxCo#:xxFnd:xxEmp#:xxDiv:scPolYr) ppp00;
                         Enddo;
                      Endif;

                      //*** Beg Del *** 10/13/23 *** WC2857 ****************
                      //***If scUpdExp <> 'Y';
                         //***Leave;
                      //***Else;
                         //***Reade (xxCo#:xxFnd:xxEmp#) pem00;
                      //***Endif;
                      //*** End Del *** 10/13/23 *** WC2857 ****************
                      //*** beg add ***** 04/25/23 *****************************
                      //Delete Employer Detail Changed Record
                      // Mahesh
                      Chain (xxCo#:xxFnd:xxEmp#:xxDiv:scPolYr) pel01;
                      If %found(waell);
                         Dow not %eof(waell);
                            Delete pel01;
                            Reade (xxCo#:xxFnd:xxEmp#:xxDiv:scPolYr) pel01;
                         Enddo;
                      Endif;

                      //Delete Employer Payroll Changed Record
                      Chain (xxCo#:xxFnd:xxEmp#:xxDiv:scPolYr) pep02;
                      If %found(Waepl);
                         Dow not %eof(Waepl);
                            Delete pep02;
                            Reade (xxCo#:xxFnd:xxEmp#:xxDiv:scPolYr) pep02;
                         Enddo;
                      Endif;

                      //Delete Employer Increased Limits Record
                      Chain (xxCo#:xxFnd:scPolYr:xxEmp#:xxDiv) pil01;
                      If %found(waill);
                         Dow not %eof(waill);
                            Delete pil01;
                            Reade (xxCo#:xxFnd:scPolYr:xxEmp#:xxDiv) pil01;
                         Enddo;
                      Endif;

                      //Delete Employer Discount Detail Record
                      Chain (xxCo#:xxFnd:scPolYr:xxEmp#:xxDiv) pds01;
                      If %found(wadsl);
                         Dow not %eof(wadsl);
                            Delete pds01;
                            Reade (xxCo#:xxFnd:scPolYr:xxEmp#:xxDiv) pds01;
                         Enddo;
                      Endif;

                      //Delete Employer Premiumt Record
                      Chain (xxCo#:xxFnd:scPolYr:xxEmp#:xxDiv) pmp01;
                      If %found(wampl);
                         Dow not %eof(wampl);
                            Delete pmp01;
                            Reade (xxCo#:xxFnd:scPolYr:xxEmp#:xxDiv) pmp01;
                         Enddo;
                      Endif;

                      //Delete Large Deductible Detail Record
                      Chain (xxCo#:xxFnd:xxEmp#:xxDiv:scPolYr) pld01;
                      If %found(waldl1);
                         Dow not %eof(waldl1);
                            Delete pld01;
                            Reade (xxCo#:xxFnd:xxEmp#:xxDiv:scPolYr) pld01;
                         Enddo;
                      Endif;

                      //Delete Employer Detail Premium Periods
                      Chain (xxCo#:xxFnd:scPolYr:xxEmp#:xxDiv) ppp01;
                      If %found(wappl);
                         Dow not %eof(wappl);
                            Delete ppp01;
                            Reade (xxCo#:xxFnd:scPolYr:xxEmp#:xxDiv) ppp01;
                         Enddo;
                      Endif;
                      //*** end add ***** 04/25/23 *****************************

                   //*** Beg Del *** 10/13/24 *** WC2857 ******************
                   //***Enddo;
                   //*** End Del *** 10/13/24 *** WC2857 ******************
                Endif;
                //*** Beg Add *** 10/13/24 *** WC2857 **********************
                Unlock WDELL;
                //*** End Add *** 10/13/24 *** WC2857 **********************
      /End-free
     C*===============================================================
     C* SRCRED - SUBROUTINE TO MODIFY CREDIT EFFECTIVE DATES
     C*===============================================================
     C*
     C     SRCRED        BEGSR
     C*
     C                   Z-ADD     1             X
     C                   Z-ADD     *ZERO         Y                 2 0
     C*
     C*  PROCESS EACH CREDIT DATE FIELD
     C*
     C     X             DOWLE     21
     C     X             ADD       1             Y
     C*
     C     CRED(X)       IFNE      *ZERO
     C*
     C*  IF CREDIT EXP. DATE IS NOT ZERO AND LE NEW EFFECTIVE DATE
     C*  THEN CLEAR EFFECTIVE AND EXPIRATION DATES
     C*
     C     CRED(Y)       IFNE      *ZERO
     C     CRED(Y)       ANDLE     ELPFDT
     C                   Z-ADD     *ZERO         CRED(X)
     C                   Z-ADD     *ZERO         CRED(Y)
     C*
     C                   ELSE                                                               RO
     C*
     C*  IF CREDIT EFFECTIVE DATE IS GT POLICY "TO" DATE THEN CLEAR THE
     C*  EFFECTIVE AND EXPIRATION DATES
     C*
     C     CRED(X)       IFGT      ELPTDT
     C                   Z-ADD     *ZERO         CRED(X)
     C                   Z-ADD     *ZERO         CRED(Y)
     C*
     C                   ELSE                                                               RO
     C*
     C*  IF CREDIT EFFECTIVE DATE EQUALS OLD POLICY EFFECTIVE DATE OR
     C*  IS LESS THAN NEW POLICY EFFECTIVE DATE THEN ASSIGN NEW DATE
     C*
     C     CRED(X)       IFEQ      XXOEDT
     C     CRED(X)       ORLT      ELPFDT
     C                   MOVE      ELPFDT        CRED(X)
     C                   ENDIF                                                  CRED,X EQ ELPFDT
      ***** BEG ADD ******* 04/29/02 **********************************
      *
      *  If credit expiration date equals old policy expiration date OR
      *  is less than new policy expiration date then assign new date.
      *
     C     CRED(Y)       IFEQ      XXOXDT
     C     CRED(Y)       ORGT      ELPTDT
     C                   MOVE      ELPTDT        CRED(Y)
     C                   ENDIF                                                  CRED,Y EQ ELPTDT
      *
     C*
     C                   ENDIF                                                  CRED,X GT XXPTDT
     C*
     C                   ENDIF                                                  CRED,Y NE*ZERO
     C*
     C                   ENDIF                                                  CRED,X NE*ZERO
     C*
     C                   ADD       2             X
     C                   ENDDO                                                  X DOWLE 19
     C*
     C*  ZERO OR BLANK CREDIT RATE OR CODE AS NECESSARY
     C*
     C     ELCREF        IFEQ      *ZERO
     C                   MOVE      *ZERO         ELASP1
     C                   ENDIF                                                  ELCREF=0
     C*
     C     ELDVEF        IFEQ      *ZERO
     C                   MOVE      *ZERO         ELASP2
     C                   Z-ADD     *ZERO         ELD1D2
     C                   Z-ADD     *ZERO         ELD1X2
     C                   ENDIF                                                  ELDVEF=0
     C*
     C     ELD2EF        IFEQ      *ZERO
     C                   MOVE      *ZERO         ELDEV2
     C                   Z-ADD     *ZERO         ELD2D2
     C                   Z-ADD     *ZERO         ELD2X2
     C                   ENDIF                                                  ELD2EF=0
     C     ELD1D2        IFEQ      *ZERO
     C                   MOVE      *ZERO         ELD1S2
     C                   ENDIF                                                  ELD2EF=0
     C*
     C     ELD2D2        IFEQ      *ZERO
     C                   MOVE      *ZERO         ELD2S2
     C                   ENDIF                                                  ELD2EF=0
     C*  IF INCR. LIMITS IS ZERO THEN INCR. LIMITS 2 MUST BE ZERO
     C*
     C     ELILEF        IFEQ      *ZERO
     C                   MOVE      *BLANKS       ELILCD
     C                   MOVE      *ZERO         ELI2EF
     C                   MOVE      *ZERO         ELI2EX
     C                   ENDIF                                                  ELILEF=0
     C*
     C     ELI2EF        IFEQ      *ZERO
     C                   MOVE      *BLANKS       ELINL2
     C                   ENDIF                                                  ELI2EF=0
     C*
     C     ELDCEF        IFEQ      *ZERO
     C                   MOVE      *BLANKS       ELDCDE
     C                   MOVE      *BLANK        ELCXBL
     C                   ENDIF                                                  ELDCEF=0
     C*
     C     ELMCEF        IFEQ      *ZERO
     C                   MOVE      *ZERO         ELASPM
     C                   MOVE      *BLANKS       ELMCCD
     C                   ENDIF                                                  ELMCEF=0
     C*
     C     ELSCEF        IFEQ      *ZERO
     C                   MOVE      *ZERO         ELASPS
     C                   ENDIF                                                  ELSCEF=0
     C*
     C     ELSDEF        IFEQ      *ZERO
     C                   MOVE      *ZERO         ELDBCR
     C                   ENDIF                                                  ELSDEF=0
     C*
     C                   ENDSR
     p UpdFiles        e

       //===================================================================
       // ValidPnl - Validate Panel Screen Fields
       //===================================================================
     p ValidPnl        b
     d ValidPnl        pi

      /Free

       // initialize the error indicator
       //===================================================================
       isValid = *Off;
       msToYr     = *blanks;
       erToYr     = *off;
       msPfDt     = *blanks;
       erPfDt     = *off;
       msPtDt     = *blanks;
       erPtDt     = *off;
       msAnDt     = *blanks;
       erAnDt     = *off;
       msBsDt     = *blanks;
       erBsDt     = *off;
       //*** Beg Del ***** 10/13/23 *** WC2857 ****************************
       //***msExm1     = *blanks;
       //***msExm2     = *blanks;
       //***msExm3     = *blanks;
       //***erExm1     = *off;
       //***erExm2     = *off;
       //***erExm3     = *off;
       //***msEM1Date  = *blanks;
       //***msEM2Date  = *blanks;
       //***msEM3Date  = *blanks;
       //***erEM1Date  = *off;
       //***erEM2Date  = *off;
       //***erEM3Date  = *off;
       //***msARP1     = *blanks;
       //***msARP2     = *blanks;
       //***msARP3     = *blanks;
       //***erARP1     = *off;
       //***erARP2     = *off;
       //***erARP3     = *off;
       //***msExMd1Nf  = *blanks;
       //***msExMd2Nf  = *blanks;
       //***msExMd3Nf  = *blanks;
       //***erExMd1Nf  = *off;
       //***erExMd2Nf  = *off;
       //***erExMd3Nf  = *off;
       //***erExMd1Nf  = *blanks;
       //*** End Del ***** 10/13/23 *** WC2857 ****************************
       msBSCD     = *blanks;
       erBSCD     = *off;
       //*** Beg Del ***** 10/13/23 *** WC2857 ****************************
       //***msUpdExp   = *blanks;
       //***erUpdExp   = *off;

       //Validate 'Update all divisions'
       //***If scUpdExp <> 'Y' and scUpdExp <> ' ';
          //***erUpdExp = *on;
          //***msUpdExp = 'WCR0521';
          //***isValid = *on;
       //***Endif;
       //*** End Del ***** 10/13/23 *** WC2857 ****************************

       xxToYr = scToYr;

       Chain(n) (xxCo#:xxFnd:xxToYr) pf100;
       If not %found(wdf1l);
          erToYr  = *on;
          msToYr  = 'WCR0517';
          isValid = *on;
       Endif;

       //Validate that policy year has not been used previously
       If scPolYr <> scToYr;
          Chain(n) (xxCo#:xxFnd:xxEmp#:xxDiv:xxToYr) pel00;
          If %found(wdell);
             erToYr  = *on;
             msToYr  = 'WC30004';
             isValid = *on;
          Endif;
       Endif;

       Chain(n) (xxCo#:xxFnd:xxToYr) pf100;
       If %found(Wdf1l);
          If FNcgfl = 'N';
             If dtCYMD(scPtDt) <> F1enpo;
                scPtdt = dtMDYY(F1enpo);
                isValid = *on;
             Endif;
          Endif;
       Endif;

       //Validate no payment type records exist if moved to new policy yr
         Chain(n) (xxCo#:xxFnd:xxEmp#:xxDiv:scPolYr) pea00;
         If %found(wmeal);
            If EAarty = 'P' or EAarty = 'I' or EAarty = 'Q' or EAarty = 'N'
             or EAarty = 'V' or EAarty = 'R' or EAarty = 'T';
               erToYr = *on;
               msToYr = 'This policy has payment A/R records and ' +
                         'cannot be moved to new policy year.';
               isValid = *on;
            Endif;
         Endif;

       //Validate no claims outside of the new policy eff date period
         Chain(n) (xxCo#:xxFnd:xxEmp#:xxDiv:scToYr) pcm00;
         If %found(wmcml);
            If CMACDT < dtCYMD(scPfdt) or CMACDT > dtCYMD(scPtdt);
               erPfdt2 = *on;
               erPtdt2 = *on;
               mgPfdt = 'This policy has claims posted outside of ' +
                         'the new policy effective date period.';
               mgPtdt = mgPfDt;
               isValid = *on;
            Endif;
         Endif;

       //Validate new policy From Date
       erPfDt = ValidDate(scPfDt:charMsg);
       If erPFDt = *on;
          msPFDt = charMsg;
          isValid  = *on;
       Endif;

       //Validate new policy To Date
       erPtDt = ValidDate(scPtDt:charMsg);
       If erPtDt = *on;
          msPtDt = charMsg;
          isValid  = *on;
       Endif;

       //Dates must be Ascending Order
       If dtCYMD(scPtDt) < dtCYMD(scPfDt);
          msPtdt = 'WCR1112';
          msPfdt = 'WCR1112';
          erPtdt = *on;
          erPfdt = *on;
          isValid  = *on;
       Endif;

       //Both Dates must be between group policy start and end dates
       If FNcgfl <> 'Y';
          If (dtCYMD(scPfDt) < F1stpo) or (dtCYMD(scPfDt) > F1enpo)
           or (dtCYMD(scPtDt) < F1stpo) or(dtCYMD(scPtDt) > F1enpo);
             msPtdt = 'WCR0723';
             msPfdt = 'WCR0723';
             erPtdt = *on;
             erPfdt = *on;
             isValid  = *on;
          Endif;
       Else;
          If EMstrp = 'Y';
             If (dtCYMD(scPfDt) < F1stpo) or (dtCYMD(scPfDt) >= F1enpo);
                msPtdt = 'WCR0723';
                msPfdt = 'WCR0723';
                erPtdt = *on;
                erPfdt = *on;
                isValid  = *on;
             Endif;
          Endif;
       Endif;

       //Dates cannot overlap date ranges for that employer
         Chain(n) (xxCo#:xxFnd:xxEmp#:xxDiv) pel00;
         If %found(wdell);
            Dow not %eof(wdell);
               If ELfyr <> scPolYr;
                  If (dtCYMD(scPfDt) >= ELpfdt) and (dtCYMD(scPfDt) < ELptdt);
                     msPfdt = 'WC30673';
                     erPfdt = *on;
                     isValid  = *on;
                  Endif;

                  If (dtCYMD(scPtDt) >  ELpfdt) and (dtCYMD(scPtDt) < ELptdt);
                     msPtdt = 'WC30673';
                     erPtdt = *on;
                     isValid  = *on;
                  Endif;
               Endif;
               Reade (xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr) pel00;
            Enddo;
         Endif;

       //Policy end date cannot be > policy end date of the state
       // reporting division
       Chain(n) (xxCo#:xxFnd:xxEmp#) pem00;
       //*** Beg Chg *** 10/13/23 *** WC2857 *******************************
       //***If %found(wmeml) and scUpdExp <> 'Y';
       If %found(wmeml);
       //*** End Chg *** 10/13/23 *** WC2857 *******************************
          Dow not %eof(wmeml);
             If EMstrp = 'Y';
                If EMdiv <> scDiv;
                   Chain(n) (xxCo#:xxFnd:xxEmp#:EMdiv:xxFyr) pel00;
                   If %found(wdell);
                      If dtCYMD(scPtDt) > ELptdt;
                         msPtdt = 'WCR1280';
                         erPtdt = *on;
                         isValid  = *on;
                         Leave;
                      Endif;
                   Endif;
                Endif;
             Endif;
             Reade(n) (xxCo#:xxFnd:xxEmp#) pem00;
          Enddo;
       Endif;

       Chain(n) (xxCo#:xxFnd:xxEmp#:xxDiv) pem00;
       Chain(n) (xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr) pel00;

       //Policy end date cannot be more than 365(366) days past policy
       //start date
       If erPTDT = *off;
          erPTDT = PolMaxLength(scPFDT:scPTDT:xxCo#:xxFnd:message);
          If erPTDT = *on;
             msPTDT = message;
             isValid = *on;
          Endif;
       Endif;

       //Edit Anniversary rating date
       If scAnDt <> 0;
          erAnDt = ValidDate(scAnDt:charMsg);
          If erAnDt = *on;
             msAnDt = charMsg;
             isValid  = *on;
          Endif;
       Endif;

       //Anniv date must be less than or equal to Pol from date
       If dtCYMD(scAnDt) > dtCYMD(scPfDt);
          erAnDt = *on;
          msAnDt = 'WCR0577';
          isValid = *on;
       Endif;

       //Edit new billing start date
       erBsDt = ValidDate(scBsDt:charMsg);
       If erBsDt = *on;
          msBsDt = charMsg;
          isValid  = *on;
       Endif;

       //*** Beg Del ***** 10/13/23 *** WC2857 ****************************
       //Mod required if effective Date Entered
       //***If scExM1 > 0 and SCEM1Date = 0;
          //***erExM1 = *on;
          //***msExM1 = 'WCR0528';
          //***isValid = *on;
       //***Endif;

       //Edit Effective Date
       //***If SCEM1Date <> 0;
          //***erEM1Date = ValidDate(SCEM1Date:charMsg);
          //***If erEM1Date = *on;
             //***msEM1Date = charMsg;
             //***isValid  = *on;
          //***Endif;
       //***Endif;

       //First Effect date must be less than or equal to Pol from date
       //***If dtCYMD(SCEM1Date) > dtCYMD(scPfDt);
          //***erEM1Date = *on;
          //***msEM1Date = 'WCR0577';
          //***isValid  = *on;
       //***Endif;

       //Mod required if effective Date Entered
       //***If scExM2 > 0 and SCEM2Date = 0;
          //***erExM2 = *on;
          //***msExM2 = 'WCR0528';
          //***isValid = *on;
       //***Endif;

       //Edit Effective Date
       //***If SCEM2Date <> 0;
          //***erEM2Date = ValidDate(SCEM2Date:charMsg);
          //***If erEM2Date = *on;
             //***msEM2Date = charMsg;
             //***isValid  = *on;
          //***Endif;

          //***If dtCYMD(SCEM2Date) < dtCYMD(SCEM1Date);
             //***erEM1Date = *on;
             //***erEM2Date = *on;
             //***msEM1Date = 'WCR0529';
             //***msEM2Date = 'WCR0529';
             //***isValid  = *on;
          //***Endif;
       //***Endif;

       //Mod Required If Effective Date Entered
       //***If scExM3 > 0 and SCEM3Date = 0;
          //***erExM3 = *on;
          //***msExM3 = 'WCR0528';
          //***isValid = *on;
       //***Endif;

       //Edit Effective Date
       //***If SCEM3Date <> 0;
          //***erEM3Date = ValidDate(SCEM3Date:charMsg);
          //***If erEM2Date = *on;
             //***msEM2Date = charMsg;
             //***isValid  = *on;
          //***Endif;

          //***If dtCYMD(SCEM3Date) < dtCYMD(SCEM2Date);
             //***erEM2Date = *on;
             //***erEM3Date = *on;
             //***msEM2Date = 'WCR0529';
             //***msEM3Date = 'WCR0529';
             //***isValid  = *on;
          //***Endif;
       //***Endif;
       //*** End Del ***** 10/13/23 *** WC2857 ****************************
       //*** Beg Add ***** 10/13/23 *** WC2857 ****************************
       //Validate if Billing Start Date > Policy Effective Date
       If dtCYMD(scBsdt) > dtCYMD(scPfdt);
          erBsDt = *on;
          msBsDt = 'WCR1284';
          isValid = *on;
       Endif;
       //*** End Add ***** 10/13/23 *** WC2857 ****************************
       //Validate the Billing Schedule Code
         keyBlSch = %trim(%Editc(scBsCd:'X'));
         Chain(n) (xxCo#:keyBlSch) pbs01;
         If not %found or BSstat <> 'A';
            erBsCd = *on;
            msBsCd = 'WCR0513';
            isValid = *on;
         Endif;

         If erBscd = *off;
            Exsr Srbill;
         Endif;

      /End-Free

      * AGENT HISTORY
      *  INVOKE EDIT IF MORE THAN ONE ACTIVE AGENT HISTORY RCD
      *
      *  IF POLICY EFF DATE IS CHANGING TO A PRIOR DATE,
      *  LOOK FOR RECORD THAT CURRENTLY SETS RATE ON POLICY AS
      *  OF THE CURRENT POLICY EFDT. IF NONE, LOOK FOR FUTURE
      *  RECORD. IF EFDT NEEDS UPDATE, ENSURE THAT AGENT ASSIGNMENT
      *  IS VALID FOR THE NEW EFDT.
      *
     C     AHKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    KKEFDT            7 0

     C     AHKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR

     C     AHKY2         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    SCPOLYR

     C     *LIKE         DEFINE    AHAGT#        SVAGT#
     C     *LIKE         DEFINE    AHEFDT        AGTEFD
     C     *LIKE         DEFINE    AHEFDT        SVAEDT
     C     isValid       IFEQ      *OFF
     C                   MOVE      *OFF          ADCFLG
     C                   Z-ADD     0             AGTEFD
     C                   Z-ADD     0             SVAEDT
     C                   MOVE      *OFF          NOAGT             1
     C                   MOVE      *OFF          FUTAGT            1
     C                   Z-ADD     0             SVAGT#
      *
     C                   Eval      xxnefd = dtcymd(scPfDt)
     C                   Eval      xxfold = svPfDt
     C                   EVAL      SYSDAT = Dateto7(SystemDate())
     C     XXNEFD        IFLT      XXFOLD
      /Free
       Exec sql
         SELECT COUNT(*) INTO :XXCNT
           FROM WMAHP
            WHERE  AHCO#   =  :XXCO#  AND
                   AHFND   =  :XXFND  AND
                   AHEMP#  =  :XXEMP# AND
                   AHDIV   =  :XXDIV  AND
                   AHFYR   =  :XXFYR  AND
                   AHSTAT  =  'A'     AND AHEFDT <=  :XXFOLD;

      /End-free
      *
     C     XXCNT         IFGT      1
     C*** Beg Del *** 10/13/23 *** WC2857 **********************************
     C***  scUpdExp      IFEQ      'Y'
     C***  DIVCNT        ANDGT     1
     C***                MOVE      DMSG          SCDMSG                         CHK ALL DIV MSG
     C***                WRITE     RcdPolEfDt
     C***                ELSE
     C***                MOVE      *BLANKS       SCDMSG                         CHK ALL DIV MSG
     C*** End Del *** 10/13/23 *** WC2857 **********************************
     C                   WRITE     RcdPolEfDt
     C*** Beg Del *** 10/13/23 *** WC2857 **********************************
     C***                ENDIF
     C*** End Del *** 10/13/23 *** WC2857 **********************************
     C                   MOVE      *ON           erPfDt
      *** beg chg ***** 03/07/22 *****************************
     C***                MOVE      'WC31000'     msPfDt
     C                   MOVEL     'WC31000'     msPfDt
      *** end chg ***** 03/07/22 *****************************
     C                   MOVE      *ON           isValid
     C                   ENDIF
      *
     C                   Z-ADD     XXFOLD        KKEFDT
     C     AHKY3         SETGT     PAH00
     C     AHKY1         READPE(N) PAH00                                  60
     C     *IN60         IFEQ      *ON
     C     AHKY3         SETLL     PAH00
     C     AHKY1         READE(N)  PAH00                                  60
     C     *IN60         IFEQ      *OFF
     C     AHSTAT        ANDEQ     'A'
     C                   Z-ADD     AHEFDT        KKEFDT
     C     AHKY3         SETGT     PAH00
     C     AHKY1         READPE(N) PAH00                                  60
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  *IN60=*ON
      *
     C                   SELECT
     C     *IN60         WHENEQ    *ON
     C                   MOVE      *ON           NOAGT
     C     *IN60         WHENEQ    *OFF
     C     KKEFDT        ANDNE     XXFOLD
     C                   MOVE      *ON           FUTAGT            1
     C                   ENDSL
      *
     C     *IN60         DOWEQ     *OFF
     C                   SELECT
     C     AHSTAT        WHENNE    'A'
     C     AHEFDT        WHENLE    XXNEFD
     C                   Z-ADD     AHEFDT        AGTEFD
     C                   Z-ADD     0             SVAEDT
     C                   Z-ADD     0             SVAGT#
     C                   LEAVE
     C     AHEFDT        WHENGT    XXNEFD
     C     FUTAGT        OREQ      *ON
     C                   Z-ADD     AHEFDT        SVAEDT
     C                   Z-ADD     AHAGT#        SVAGT#
     C                   ENDSL
     C     AHKY2         READPE(N) PAH00                                  60
     C                   ENDDO
      *
      * IF NO AHEFDT EXISTS EARLIER THAN NEW EFF DT, ASSIGN ONE
      *
     C                   SELECT
     C     NOAGT         WHENEQ    *ON
     C                   MOVE      *OFF          ADCFLG
     C     AGTEFD        WHENNE    *ZERO
     C                   MOVE      *OFF          ADCFLG
     C     XXNEFD        WHENLE    SYSDAT
     C                   Z-ADD     XXNEFD        AGTEFD
     C                   MOVE      *ON           ADCFLG            1            AGT EFDT CHG FL
     C     XXNEFD        WHENGT    SYSDAT
     C                   Z-ADD     SYSDAT        AGTEFD
     C                   MOVE      *ON           ADCFLG            1            AGT EFDT CHG FL
     C                   ENDSL
     C                   ENDIF                                                  XXNEFD<XXFOLD
      *
      * IF MOVING POLICY EFF DATE FORWARDS AND AGENT HISTORY RECORD
      * DOESN'T EXIST PRIOR, CHECK IF AGENT HISTORY RECORD EXISTS IN
      * FUTURE THAT CAN BE BACKED UP TO NEW EFDT.
      *
     C     XXNEFD        IFGT      XXFOLD
      /Free
       EXEC SQL
         SELECT COUNT(*) INTO :XXCNT
           FROM WMAHP
            WHERE  AHCO#   =  :XXCO#  AND
                   AHFND   =  :XXFND  AND
                   AHEMP#  =  :XXEMP# AND
                   AHDIV   =  :XXDIV  AND
                   AHFYR   =  :XXFYR  AND
                   AHSTAT  =  'A'     AND
                   AHEFDT <=  :XXNEFD;
      /End-Free
     C     XXCNT         IFEQ      0
     C                   Z-ADD     XXNEFD        KKEFDT
     C     AHKY3         SETLL     PAH00
     C     AHKY1         READE(N)  PAH00                                  60
     C     *IN60         DOWEQ     *OFF
     C     AHSTAT        IFEQ      'A'
     C                   Z-ADD     SYSDAT        AGTEFD
     C                   Z-ADD     AHEFDT        SVAEDT
     C                   Z-ADD     AHAGT#        SVAGT#
     C                   MOVE      *ON           ADCFLG            1
     C                   LEAVE
     C                   ENDIF
     C     AHKY1         READE(N)  PAH00                                  60
     C                   ENDDO
     C                   ENDIF                                                  XXCNT = 0
     C                   ENDIF                                                  XXNEFD<XXFOLD
      *
      * ENSURE AGENT ASSIGNMENT RECORD EXISTS FOR NEW AHEFDT
      *
     C     ADCFLG        IFEQ      *ON
     C                   Z-ADD     0             XXCNT             5 0
      /Free
       EXEC SQL
         SELECT COUNT(*) INTO :XXCNT
           FROM WMAAP
            WHERE  AACO#   =  :XXCO#  AND
                   AAFND   =  :XXFND  AND
                   AAAGT#  =  :SVAGT# AND
                   AASTAT  =  'A'     AND
                  (:AGTEFD >= AAEFDT  AND
                   AACNDT  =  0       OR
                   :AGTEFD >= AAEFDT  AND
                   :AGTEFD <  AACNDT  AND
                   AACNDT  <> 0);
      /END-Free
      *
     C     XXCNT         IFEQ      *ZERO
      *** Beg Del **** 10/13/23 *** WC2857 *********************************
     C***  scUpdExp      IFEQ      'Y'
     C***  DIVCNT        ANDGT     1
     C***                MOVE      DMSG          SCDMSG                         CHK ALL DIV MSG
     C***                WRITE     RcdPolEfDt
     C***                ELSE
     C***                MOVE      *BLANKS       SCDMSG                         CHK ALL DIV MSG
     C***                ENDIF
      *** End Del **** 10/13/23 *** WC2857 *********************************
     C     XXNEFD        IFLT      SYSDAT
     C                   MOVE      *ON           ERPFDT
      *** beg chg ***** 03/07/22 *****************************
     C***                MOVE      'WCR1045'     ERPFDT
     C                   MOVEL     'WCR1045'     MSPFDT
      *** end chg ***** 03/07/22 *****************************
     C                   ELSE
      *** beg chg ***** 03/07/22 *****************************
     C***                MOVE      'WCR0637'     MSPFDT
     C                   MOVEL     'WCR0637'     MSPFDT
      *** end chg ***** 03/07/22 *****************************
     C                   MOVE      *ON           ERPFDT
     C                   ENDIF                                                  XXNEFD < SYSDAT
     C                   MOVE      *ON           ISVALID
     C                   ENDIF                                                  XXCNT = 0
     C                   ENDIF                                                  ADCFLG = *ON
     C                   ENDIF

      *===============================================================
      * SRBILL - CHECK NO BILLING AFTER POL EFF DT
      *===============================================================
      *
     C     SRBILL        BEGSR
      *
      *  Only perform edit if Installment ranges in given Policy dates
      *
      *  Get number of billing periods
      *
     C                   SELECT
     C     BSAB12        WHENNE    *ZERO
     C                   Z-ADD     12            XXHIBS            2 0
     C     BSAB11        WHENNE    *ZERO
     C                   Z-ADD     11            XXHIBS            2 0
     C     BSAB10        WHENNE    *ZERO
     C                   Z-ADD     10            XXHIBS            2 0
     C     BSAB09        WHENNE    *ZERO
     C                   Z-ADD     9             XXHIBS            2 0
     C     BSAB08        WHENNE    *ZERO
     C                   Z-ADD     8             XXHIBS            2 0
     C     BSAB07        WHENNE    *ZERO
     C                   Z-ADD     7             XXHIBS            2 0
     C     BSAB06        WHENNE    *ZERO
     C                   Z-ADD     6             XXHIBS            2 0
     C     BSAB05        WHENNE    *ZERO
     C                   Z-ADD     5             XXHIBS            2 0
     C     BSAB04        WHENNE    *ZERO
     C                   Z-ADD     4             XXHIBS            2 0
     C     BSAB03        WHENNE    *ZERO
     C                   Z-ADD     3             XXHIBS            2 0
     C     BSAB02        WHENNE    *ZERO
     C                   Z-ADD     2             XXHIBS            2 0
     C     BSAB01        WHENNE    *ZERO
     C                   Z-ADD     1             XXHIBS            2 0
     C                   OTHER
     C                   Z-ADD     0             XXHIBS            2 0
     C                   ENDSL
      *
     C     SCBSDT        IFNE      *ZERO
      /Free
       xxBgdt = dtCYMD(scBsDt);
      /End-Free
     C                   ELSE
      /Free
       xxBgdt = dtCYMD(scPfDt);
      /End-Free
     C                   ENDIF
      /Free
       xxTodt = dtCYMD(scPtDt);
      /End-Free
      *
      * Checks how many billings for the 1st will fall within the
      * effective dates
      *
     C                   Z-ADD     XXBGDT        XXDT
      *
     C     BSON15        IFEQ      'Y'
     C                   Z-ADD     15            XXDD
     C                   ELSE
     C                   Z-ADD     1             XXDD
     C                   ENDIF
     C*
     C                   Z-ADD     XXDT          XXBGDT
     C                   Z-ADD     0             XXINCT            2 0
     C     XXDT          DOWLE     XXTODT
     C     XXDT          IFGE      XXBGDT
     C     XXDT          ANDLT     XXTODT
     C                   ADD       1             XXINCT
     C                   ENDIF
     C                   ADD       1             XXMM
     C     XXMM          IFEQ      13
     C                   Z-ADD     1             XXMM
     C                   ADD       1             XXYY
     C                   ENDIF
     C                   ENDDO
      *
     C     XXHIBS        IFGT      XXINCT
     C                   EVAL      ERBSCD = *ON
     C                   EVAL      MSBSCD = 'WC31347'
     C                   EVAL      ISVALID = *ON
     C                   ENDIF
      *
     C                   ENDSR
      *
     p ValidPnl        e

       //***************************************************************************//

