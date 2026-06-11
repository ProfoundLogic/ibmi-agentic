     hcopyright('(c) 2024 Sapiens, Inc. All rights reserved.')
      //*********************************************************************
     h/title WTENDFRM - Work with Policy Endorsement Forms - old screen
     h dftname(WTDRCRRPT)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h ALWNULL(*UsrCtl)
     h/define   profoundUI
     h bnddir('WTBNDDIR')
      //*********************************************************************
      //==============================================================*
      //  Created   : 05/09/24                                         *
      //  Programmer: C:300                                            *
      //  Project   : WC2433 - Work with Policy Endorsement Forms -    *
      //                       Prompt to access old screen             *
      //                                                               *
      //===============================================================*
      //  Revisions:                                                   *
      //                                                               *
      //                                                               *
      //                                                               *
      // ------------------------------------------------------------- *
      //  NOTE: Please add comments to the top of Revisions.           *
      // ------------------------------------------------------------- *
      //===============================================================*

     fWTENDFRM  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)
      * Employer Details File
     fwdelp     if   e             disk
     FWDELL5    iF   E           K DISK    rename(PEL01:PEL05)
     F                                     infds(dbfds)
     fwdell     if   e           k disk    rename(pel01:pel00)
     f                                     infds(dbfds3)
      * Employer Master file
     fwmeml     if   e           k disk    rename(pem01:pem00)
     f                                     infds(dbfds4)
      * User Parameter File
     fsmupl     if   e           k disk    rename(pup01:pup00)

     d psds          esds                  extname(szq1p)
     D dbfds           ds
     D  dbrrn                397    400b 0
     d dbfds3          ds
     d  dbrrn3               397    400b 0
     d dbfds4          ds
     d  dbrrn4               397    400b 0

      // work variables
     d intCo#          s              3P 0
     d isError         s               n   inz
     d intStRptDiv     s                   like(ELdiv)
     d ##stdivrrn      s              9  0 inz

      // calling parameter for WtPolARInq
     d @@rrn           s              9
     d @@mode          s              1
     d @@next          s             10
     d @@tabind        s              5
     d @@Qte           s              1
     d @@PostDt        s              8  0
     d @@PostAmt       s             11  2

      // calling parameter for W1054R
     d ##Kc            s              1    inz
     d ##Kl            s              1    inz
     d ##Call          s              1    inz
     d ##Rrn           s              9  0 inz
     d ##Qte           s              1    inz

      // Employer search DS
     d outSelect       ds                  likeds(dsSrchCrit)
     d intEmpRrn       s              9  0
     d outCall         s              1
     d outRrn#         s              9
     d outMode         s              1    inz

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

      * Main Processing
     d Main            pr

      * Process to Call old Policy Endorsement Screen
     d ProcEndFrm      pr

      * Program Start
     d ProgramStart    pr

     d PolEndForm      pr                  extpgm('W1054R')
     d ##kc                           1
     d ##kl                           1
     d ##call                         1
     d ##rrn                          9  0
     d ##qte                          1

      * Validate Employer Information
     d ValidateEmpInf  pr              n

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
     d/copy WprEmpLr
     d/copy WTSearchDs

      /Free

       ProgramStart();

       Main();

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
       erGroupE  = *off;
       msGroupE  = *blanks;
       erEmpNoE  = *off;
       msEmpNoE  = *blanks;
       erDivE = *off;
       msDivE = *blanks;
       erYearE = *off;
       msYearE = *blanks;

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
       dsEmpGrp  = scGroupE;
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
                scEmpNoE   = EMemp#;
                scDivE     = EMdiv;
                scGroupE   = EMfnd;
             EndIf;
          EndIf;
       EndIf;

       // Employer Detail Record does not exist in WDELP file for selected
       // Fund, Employer, division, then show error --
       // 'Policy Years information has not been established for this employer/'
       If outRrn# = '999999999';
          scEmpNoE   = *zeros;
          scDivE     = *zeros;
          scYearE    = *zeros;
          scGroupE   = *zeros;

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
             erGroupE = *off;
             msGroupE = *blanks;
             erEmpNoE = *off;
             msEmpNoE = *blanks;
             EmployerSrch();

          // Year Search
          When selPolYr = *on;
             erYearE = *off;
             msYearE = *blanks;
             @@rrn = *blanks;
             @@next = 'PostYear';
             If intco# <> 0 and scGroupE <> 0 and scEmpNoE <> 0;
                Chain(n) (intco#:scGroupE:scEmpNoE:scDivE) pel00;
                If %found(wdell);
                   @@rrn = %char(dbrrn3);
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
                   scYearE = %int(@@TabInd);
                Endif;
             Endif;

          // Submit
          When BtnSubmit = *On;
             BtnSubmit = *Off;
             // Validate Edits
             If ValidateEmpInf();
                ProcEndFrm();
                Leave;
             EndIf;

          // Refresh
          When btnRefresh = *On;
             btnRefresh = *Off;
             Clear rcdEndFrm;
             scGroupE = UPfnd;
             scCo#1 = UPco#;
             ClrErr();

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

           Exfmt rcdEndFrm;

           Read rcdComPnl;
           Read rcdPanel;
        EndSl;

       Endsr;

      /End-Free

     p Main            e

       //===================================================================
       //ProcEndFrm - Call old Policy Endorsement Screen
       //===================================================================
     p ProcEndFrm      b
     d ProcEndFrm      pi

      /Free

       // Get WDELP rrn of State Reporting Division
       intStRptDiv = GetStRptDv(intco#:scGroupE:scEmpNoE);
       Chain (intco#:scGroupE:scEmpNoE:intStRptDiv:scYearE) pel05;
       If %found(WDELL5);
          ##stdivrrn = dbrrn;
       Endif;

       ##kc   = '0';
       ##kl   = '0';
       ##call = ' ';
       If scInquiryE = 'Y';
          ##call = '1';                           // Inquiry Mode
       Endif;
       ##rrn  = ##stdivrrn;
       ##qte  = ' ';
       // Call old Policy Endorsement Screen (W1054R)
       PolEndForm(##kc:##kl:##call:##rrn:##qte);

      /End-Free
     p ProcEndFrm      e

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
          scGroupE = UPfnd;
          scCo#1 = UPco#;
       Endif;

       Return;

      /End-Free

     p ProgramStart    e

      //===================================================================
      // Validate Employer Information
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateEmpInf  b
     d ValidateEmpInf  pi              n
      /Free

      // Initialize all the messageId and error indicators.
       ClrErr();

       // Group cannot be Zero
       If scGroupE = *zeros;
          erGroupE = *on;
          msGroupE = 'WCR0502';
          isError = *on;
       Endif;

       // Employer cannot be Zero
       If scEmpNoE = *zeros;
          erEmpNoE = *on;
          msEmpNoE = 'WCR0502';
          isError = *on;
       Endif;
       // Validate Employer
       If scEmpNoE <> *zero;
           Chain (intco#:scGroupE:scEmpNoE:scDivE) wmeml;
           If not %Found(wmeml);
              erEmpNoE = *on;
              msEmpNoE = 'WCR0538';
              erDivE = *on;
              msDivE = 'WCR0538';
              erYearE = *on;
              msYearE = 'WCR0538';
              isError = *on;
           EndIf;
       Endif;

       // Year cannot be Zero
       If scYearE = *zeros;
          erYearE = *on;
          msYearE = 'WCR0502';
          isError = *on;
       Endif;
       // check policy year valid for this Employer
       If erYearE = *off and erGroupE = *off and erEmpNoE = *off;
          If scYearE <> *zero or scEmpNoE <> *zero or scDivE <> *zero;
             Chain (intco#:scGroupE:scEmpNoE:scDivE:scYearE) wdell;
             If not %Found(wdell);
                erYearE = *on;
                msYearE = 'WCR0639';
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
     p ValidateEmpInf  e
