     hcopyright('(c) 2025 Sapiens, Inc. All rights reserved.')
      //*********************************************************************
     h/title WTSRPAYROL - S/R Payroll Entry - CS Standards
     h dftname(WTSRPAYROL)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h ALWNULL(*UsrCtl)
     h/define   profoundUI
     h bnddir('WTBNDDIR')
     h bnddir('WTGRPCFG')
      //*********************************************************************
      //==============================================================*
      //  Created   : 01/07/25                                         *
      //  Programmer: C:300                                            *
      //  Project   : WC2996 - S/R Payroll Entry Menu Version          *
      //                                                               *
      //===============================================================*
      //  Revisions:                                                   *
      //                                                               *
      //  08/28/25 C:300 Spport: 119589 (Prescient)                    *
      //                 Allow editing of Monthly S/R Payroll          *
      //                 (if found, like old screen allows)            *
      //                                                               *
      //  02/20/25 C:300 Project: WC2996, Support: 118556              *
      //                 DSPF: Add Policy Effective Date Range and     *
      //                       Cancellation Date on Reporting Period   *
      //                       window.                                 *
      //                       Also Bind Enter key to Apply Button     *
      //                                                               *
      // ------------------------------------------------------------- *
      //  NOTE: Please add comments to the top of Revisions.           *
      // ------------------------------------------------------------- *
      //===============================================================*

     fWTSRPAYROLcf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)
      * User Parameter File
     fsmupl     if   e           k disk    rename(pup01:pup00)
      * Fund Master File
     fwmfnl     if   e           k Disk    Rename(pfn01:pfn00)
      * Employer Details File
     fwdelp     if   e             disk
     fwdell     if   e           k Disk    Rename(pel01:pel00)
     F                                     infds(dbfds)
      * Employer Master File
     fwmeml     if   e           k Disk    Rename(pem01:pem00)
      * Employer Payroll File
     fwasrl3    if   e           k Disk    Rename(psr01:psr03)
      *** Beg Add *** 08/28/25 *******************************************
     F                                     infds(dbfds3)
      *** End Add *** 08/28/25 *******************************************
     fwasrl9    if   e           k Disk    Rename(psr01:psr09)
      *** Beg Add *** 08/28/25 *******************************************
     F                                     infds(dbfds9)
      *** End Add *** 08/28/25 *******************************************
      * Logical over all table files
     fwtabl     if   e           k disk
     f                                     include(pbs01)

     d psds          esds                  extname(szq1p)
     D dbfds           ds
     D  dbrrn                397    400b 0
      *** Beg Add *** 08/28/25 *******************************************
     D dbfds3          ds
     D  dbrrn3               397    400b 0
     D dbfds9          ds
     D  dbrrn9               397    400b 0
      *** End Add *** 08/28/25 *******************************************

      // work variables
     d intCo#          s              3P 0
     d xxRpmy          s              5P 0
     d chkDate         s                   like(scMyDt)
     d xxCxDt7         s              7  0 inz
     d xxCxMYD         s              7  0 inz
     d xxWeDt          s              7  0 inz
     d xxDowk          s              9    inz
     d xxKey           s                   like(BSkey)
     d charMsg         s            100
     D schdTyp         s              1                                         Bill Schedule Type
     D clientID        s              3    inz('000')
     d process         s               n   inz(*off)
     d isError         s               n   inz
     d elRRN           s              9  0
     d srRRN           s              9  0
     d srFltDt         s              8  0
     d srRtCd          s                   like(ELrtcd)
      *** Beg Add *** 08/28/25 *******************************************
     d savSRrrn        s              9  0
      *** End Add *** 08/28/25 *******************************************

      // calling parameter for WtPolARInq
     d @@rrn           s              9
     d @@mode          s              1
     d @@next          s             10
     d @@tabind        s              5
     d @@Qte           s              1
     d @@PostDt        s              8  0
     d @@PostAmt       s             11  2

      // Employer search DS
     d outSelect       ds                  likeds(dsSrchCrit)
     d intEmpRrn       s              9  0
     d outCall         s              1
     d outRrn#         s              9
     d outMode         s              1    inz

     d                 ds
     d  ELpfdt                 1      7  0
     d  xxStym                 1      5  0
     d                 ds
     d  ELptdt                 1      7  0
     d  xxEnym                 1      5  0
     d                 ds
     d  xxCurdat               1      7  0
     d  xxCudt                 1      5  0

      * Clear Error/Message fields
     d ClrErr          pr
 
      // search employer number
     d EmployerSrch    pr

      // Employer search
     d EmpSearch       pr                  ExtPgm('WTSEARCH')
     d outMode                        1
     d outCall                        1
     d outRrn#                        9
     d outSelect                           likeds(dsSrchCrit)

      // Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3

      * Main Processing
     d Main            pr

      * Process S/R payroll
     d ProcSRPayRol    pr

      * Program Start
     d ProgramStart    pr

      * Validate Reporting Period Date
     d ValidatePeriod  pr              n

      * Validate S/R payroll Entry
     d ValidateSRPayR  pr              n

      * Policy A/R Inquiry
     d WtPolARInq      pr                  ExtPgm('WTPOLARINQ')
     d @@rrn                          9
     d @@mode                         1
     d @@next                        10
     d @@tabind                       5
     d @@Qte                          1
     d @@PostDt                       8  0
     d @@PostAmt                     11  2

     d/copy sPrDate
     d/copy sPrGenPop
     d/copy sPrCompany
     d/copy wPrUsrPrms
     d/copy wPrPolErr
     d/copy wPrPolPop2
     d/copy WprEmpLr
     d/copy WTSearchDs
     d/copy sPrString

     c     keyWASRL3     Klist
     c                   Kfld                    intCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scYear
     c                   Kfld                    scEmpNo
     c                   Kfld                    scDiv
     c                   KFLD                    xxRpmy

     c     keyWASRL9     Klist
     c                   Kfld                    intCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scYear
     c                   Kfld                    scEmpNo
     c                   Kfld                    scDiv
     c                   KFLD                    xxWeDt

     c     keyWTABL      klist
     c                   kfld                    intCo#
     c                   kfld                    xxKey

      /Free

       ProgramStart();

       If process;
          Main();
       Endif;

       *InLr = *On;

      /End-Free

        //===================================================================
        // ClrErr - Clear Error/Message fields
        //===================================================================
     p ClrErr          b
     d ClrErr          pi

      /Free
       // Initialize all the messageId and error indicators.
       isError = *off;
       erFnd   = *off;
       msFnd   = *blanks;
       erEmpNo = *off;
       msEmpNo = *blanks;
       erDiv   = *off;
       msDiv   = *blanks;
       erYear  = *off;
       msYear  = *blanks;

      /End-Free

     p ClrErr          e

       //===================================================================
       // EmployerSrch  - Employer/Div Search
       //===================================================================
     p EmployerSrch    b
     d EmployerSrch    pi

      /Free
       outMode  = 'E';
       outCall  = '2';
       clear dsSrchCrit;
       clear outSelect;
       dsEmpGrp  = scFnd;
       outSelect  = dsSrchCrit;
       outRrn#   = '000000000';
       EmpSearch(outMode:outCall:outRrn#:outSelect);

       If outRrn# <> '000000000'
        and outRrn# <> '999999999';
          IntEmpRrn = %Dec(outRrn#:9:0);
          Chain intEmpRrn pel01;
          If %Found(Wdelp);
             Chain (ELco#:ELfnd:ELemp#:ELdiv) pem00;
             If %Found(Wmeml);
                scEmpNo   = EMemp#;
                scDiv     = EMdiv;
                scFnd     = EMfnd;
             EndIf;
          EndIf;
       EndIf;

       // Employer Detail Record does not exist in WDELP file for selected
       // Fund, Employer, division, then show error --
       // 'Policy Years information has not been established for this employer/'
       If outRrn# = '999999999';
          scEmpNo   = *zeros;
          scDiv     = *zeros;
          scYear    = *zeros;
          scFnd     = *zeros;

          PopOneBtn('Policy Years Information has not been established for the-
           selected Employer/Division. Please select a different-
           Employer/Division.':'Employer Search Error');

       EndIf;

      /End-Free
     p EmployerSrch    e


       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

      /Free

       // Process until exit button is pressed.
       BtnCancel = *off;

       // Process until exit button is pressed.
       DoU BtnCancel = *On;

          // Display Screen
          Exsr ExfmtScreens;

          // Perform actions on screen.
          Select;

          // Employer Search
          When selEmpSrch = *on;
             erFnd = *off;
             msFnd = *blanks;
             erEmpNo = *off;
             msEmpNo = *blanks;
             EmployerSrch();

          // Year Search
          When selPolYr = *on;
             erYear = *off;
             msYear = *blanks;
             @@rrn = *blanks;
             @@next = 'PostYear';
             If intco# <> 0 and scFnd <> 0 and scEmpNo <> 0;
                Chain(n) (intco#:scFnd:scEmpNo:scDiv) pel00;
                If %found(wdell);
                   @@rrn = %char(dbrrn);
                Endif;
             Endif;

             @@mode = 'D';
             @@TabInd = *blanks;
             @@PostDt = *zeros;
             @@PostAmt = *zeros;

             // Call WtPolARInq for Policy Year
             If @@rrn <> *blanks;
                WtPolARInq(@@rrn:@@mode:@@next:@@TabInd:@@Qte:
                           @@PostDt:@@PostAmt);
                If @@TabInd <> *blanks and @@next = 'PostYear';
                   scYear = %int(@@TabInd);
                Endif;
             Endif;

          // Submit
          When BtnSubmit = *On;
             BtnSubmit = *Off;
             // Validate S/R payroll Entry
             If ValidateSRPayR();
                // Check for Self-Report or Estimate billing type Flag
                xxKey = %editc(ELbscd:'X');
                Chain(n) keyWTABL PBS01;
                If BSsefl <> 'E';
                   ProcSRPayRol();
                Else;
                   PopOneBtn('This Policy do not has Self-Reporting Billing +
                    Plan, Please Select another Policy.':'Error':'Ok');
                Endif;
             EndIf;

          // Refresh
          When btnRefresh = *On;
             btnRefresh = *Off;
             Clear rcdSRPayR;
             scCo#  = UPco#;
             scFnd  = UPfnd;
             scYear = UPfyr;
             ClrErr();

          // Save
          When btnSave = *On;
             btnSave = *Off;

          EndSl;

       Enddo;

       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

        Select;

        When TabIndex = 0;
           PnlHeight = 400;
           APnlHeight = PnlHeight + 60;

           Write rcdComPnl;
           Write rcdPanel;

           Exfmt rcdSRPayR;

           Read rcdComPnl;
           Read rcdPanel;
        EndSl;

       Endsr;

      /End-Free

     p Main            e

       //===================================================================
       //ProcSRPayRol  - Process S/R Payroll
       //===================================================================
     p ProcSRPayRol    b
     d ProcSRPayRol    pi
       //*** Beg Add *** 02/20/25 *******************************************
     d xxCxDt          s              7  0 inz
       //*** End Add *** 02/20/25 *******************************************

      /Free
       Clear rcdSrrFilt;

       // Set Employer information
       scGrpNum  = scfnd;

       Chain (intCo#:scfnd) pfn00;
       If %found(wmfnl);
          scGrpName = FNname;
       Endif;

       scEmpNum  = %editc(scfnd:'X') + ' - ' + %editc(scEmpNo:'X') +
                     ' - ' + %editc(scDiv:'X');

       Chain (intCo#:scfnd:scEmpNo:scdiv) pem00;
       If %found(wmeml);
          scEmpName = %trim(EMname) + ' ' + %trim(EMnam2);
       Endif;

       scPolFyr  = scYear;

       scBilPlan = %trim(BSkey) + ' - ' + %trim(BSdesc);

       //*** Beg Add *** 02/20/25 *******************************************
       scPolEffDt = alphaUSA(cvtToDate(dtMDYY(ELpfdt)));
       scPolExpDt = alphaUSA(cvtToDate(dtMDYY(ELptdt)));

       xxCxDt = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
       scPolCanDt = alphaUSA(cvtToDate(dtMDYY(xxCxDt)));
       //*** End Add *** 02/20/25 *******************************************

       scFreq = *Blanks;
       Select;
       When BSfreq = 'W';
          scFreq = 'Weekly';
          monthlyDt = *off;
          weeklyDt = *on;
          weeklyLeft = 270;
          schdTyp = 'W';                  // Billing Schedule Type
       When BSfreq = 'M';
          scFreq = 'Monthly';
          monthlyDt = *on;
          weeklyDt = *off;
          schdTyp = ' ';                  // Billing Schedule Type
       Endsl;


       visible046 = *off;            // Set visibility for Late Report Checkbox
       If clientID = '046';
          visible046 = *on;
       endif;

       // Display window for Reporting Period Date

       BtnFiltCnl = *Off;
       DoU BtnFiltCnl = *On;
          Exfmt rcdSrrFilt;

          // Apply
          If BtnFiltApl = *On;
             BtnFiltApl = *Off;
             // Validate Reporting Period Date
             If ValidatePeriod();
                srRRN = 0;
                srRtCd = ELrtcd;
                elRRN = dbRRN;
                //*** Beg Add *** 08/28/25 *******************************************
                // Call S/R Payroll Procedure in Edit Mode
                If savSRrrn <> 0;
                   srRRN = savSRrrn;
                   elRRN = 0;
                Endif;
                //*** End Add *** 08/28/25 *******************************************
                If scMyDt <> 0;
                   srFltDt = scMyDt;
                Else;
                   srFltDt = scWeDt;
                Endif;

                // Call S/R Payroll Procedure in WPIPOLPOP2
                SelfRptEdit(srRRN:srRtCd:elRRN:srFltDt:schdTyp);
                Leave;
             Endif;
          Endif;
       EndDo;

      /End-Free
     p ProcSRPayRol    e

       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /Free

       // Load Action panel with the Company Name.
       intCo# = userCo#(q1user);
       scCompName = GetCompNam(intCo#);

       // Retrieve user parameter
       Chain q1user pup00;
       If %Found(Smupl);
          scFnd  = UPfnd;
          scYear = UPfyr;
          scCo#  = UPco#;
          process = *on;
       Else;
          process = *off;
       Endif;

       GetClientID(clientID);

       Return;

      /End-Free

     p ProgramStart    e

      //===================================================================
      // Validate Reporting Period Date
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidatePeriod  b
     d ValidatePeriod  pi              n
      /Free

       // Initialize all the messageId and error indicators.
       isError = *off;
       erMyDt  = *off;
       msMyDt  = *blanks;
       erMyDt2 = *off;
       mgMyDt2 = *blanks;
       erWeDt  = *off;
       msWeDt  = *blanks;
       erWeDt2 = *off;
       mgWeDt2 = *blanks;

       Select;
       // Validate Monthly Reporting Date MM/YYYY
       When BSfreq = 'M';
          xxRpmy = dt5CYMD(scMyDt);
          xxCurdat = Dateto7(SystemDate());

          // Check valid month/year entered
          chkDate = dt6Check(scMyDt);
          If chkDate = *zero;
             erMyDt  = *on;
             msMyDt  = 'WCR0500';
             isError = *on;
          Endif;

          // Month/Year must be within employer policy effective date range
          If scMyDt <> *zeros and erMyDt = *off;
             If xxRpmy < xxStym  or xxRpmy > xxEnym;
                erMyDt = *on;
                msMyDt = 'WC31373';
                isError = *on;
             Endif;
             If xxRpmy > xxCudt;
                erMyDt = *on;
                msMyDt = 'WC40162';
                isError = *on;
             Endif;
          Endif;

          // Check for duplicate month/year (Cannot added more than once)
          If scMyDt <> *zeros and erMyDt = *off;
             Chain keyWASRL3 psr03;
             If %found(wasrl3);
                //*** Beg Chg *** 08/28/25 *******************************************
                //*** erMyDt = *on;
                //*** msMyDt = 'WC30005';
                //*** isError = *on;
                // If Monthly S/R Payroll record found, then Allow to Edit it
                savSRrrn = dbrrn3;
                //*** End Chg *** 08/28/25 *******************************************
             Endif;
          Endif;

          // Check for Policy Cancellation Date
          If scMyDt <> *zeros and erMyDt = *off;
             xxCxDt7 = getCxDate(intCo#:scFnd:scEmpNo:scDiv);
             If xxCxDt7 <> 0;
                xxCxMYD = (xxRpmy * 100) + 01;
                xxCxMYD = DateTo7(getLastDay(cvtToDate(xxCxMYD)));
                If xxCxMYD > xxCxDt7;
                   erMyDt2 = *on;
                   mgMyDt2 = 'Self-report not allowed after Cancellation Date';
                   isError = *on;
                Endif;
             Endif;
          Endif;

       // Validate Weekly Reporting Date MM/DD/YYYY
       When BSfreq = 'W';
          xxWeDt = dtCYMD(scWeDt);
          xxCurdat = Dateto7(SystemDate());

          // Validate Weekly Date entered
           erWeDt = ValidDate(scWeDt:charMsg);
           If erWeDt = *on;
              msWeDt = charMsg;
             isError = *on;
          Endif;

          // Weekly Reporting Date must be within employer policy effective date range
          If scWeDt <> *zeros and erWeDt = *off;
             If xxWeDt < ELpfdt  or xxWeDt >= ELptdt;
                erWeDt = *on;
                msWeDt = 'WC31315';
                isError = *on;
             Endif;
          Endif;

          // in Weekly Report Date - day of week must match billing schedule code day of week
          If scWeDt <> *zeros and erWeDt = *off;
             xxDowk = *blanks;
             xxDowk = DowLong(scWeDt);
             If BSdowk <> Upper(%subst(xxDowk:1:2));
                erWeDt = *on;
                msWeDt = 'WC30263';
                isError = *on;
             Endif;
          Endif;

          // Check for duplicate month/year (Cannot added more than once)
          If scWeDt <> *zeros and erWeDt = *off;
             Chain keyWASRL9 psr09;
             If %found(wasrl9);
                //*** Beg Chg *** 08/28/25 *******************************************
                //*** erWeDt = *on;
                //*** msWeDt = 'WC30005';
                //*** isError = *on;
                // If Weekly S/R Payroll record found, then Allow to Edit it
                savSRrrn = dbrrn9;
                //*** End Chg *** 08/28/25 *******************************************
             Endif;
          Endif;

          // Check for Policy Cancellation Date
          If scWeDt <> *zeros and erWeDt = *off;
             xxCxDt7 = getCxDate(intCo#:scFnd:scEmpNo:scDiv);
             If xxCxDt7 <> 0;
                If xxWeDt > xxCxDt7;
                   erWeDt2 = *on;
                   mgWeDt2 = 'Self-report not allowed after Cancellation Date';
                   isError = *on;
                Endif;
             Endif;
          Endif;

       EndSl;




      // Return *on indicator when No error exists.
        If isError = *on;
           return *off;
        Else;
           return *on;
        Endif;

      /End-Free
     p ValidatePeriod  e

      //===================================================================
      // Validate S/R payroll Entry
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateSRPayR  b
     d ValidateSRPayR  pi              n
      /Free

       // Initialize all the messageId and error indicators.
       ClrErr();

       // Group cannot be zero
       If scFnd = *zeros;
          erFnd = *on;
          msFnd = 'WCR0502';
          isError = *on;
       Endif;

       // Employer cannot be Zero
       If scEmpNo = *zeros;
          erEmpNo = *on;
          msEmpNo = 'WCR0502';
          isError = *on;
       Endif;
       // Validate Employer
       If scEmpNo <> *zero;
           Chain (intco#:scFnd:scEmpNo:scDiv) wmeml;
           If not %Found(wmeml);
              erEmpNo = *on;
              msEmpNo = 'WCR0538';
              erDiv = *on;
              msDiv = 'WCR0538';
              erYear = *on;
              msYear = 'WCR0538';
              isError = *on;
           EndIf;
       Endif;

       // Year cannot be zero
       If scYear = *zeros;
          erYear = *on;
          msYear = 'WCR0502';
          isError = *on;
       Endif;
       // check policy year valid for this Employer
       If erYear = *off and erFnd = *off and erEmpNo = *off;
          If scYear <> *zero or scEmpNo <> *zero or scDiv <> *zero;
             Chain (intco#:scFnd:scEmpNo:scDiv:scYear) wdell;
             If not %Found(wdell);
                erYear = *on;
                msYear = 'WCR0639';
                isError = *on;
             EndIf;
          EndIf;
       Endif;


      // Return *on indicator when No error exists.
        If isError = *on;
           return *off;
        Else;
           return *on;
        Endif;

      /End-Free
     p ValidateSRPayR  e

