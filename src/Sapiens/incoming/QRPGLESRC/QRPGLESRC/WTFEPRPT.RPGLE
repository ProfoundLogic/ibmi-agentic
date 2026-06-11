     hcopyright('(c) 2024 Sapiens, Inc. All rights reserved.')
      //*********************************************************************
     h/title WTFEPRPT - Financial Earned Premium Report
     h dftname(WTFEPRPT)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h ALWNULL(*UsrCtl)
     h/define   profoundUI
     h bnddir('WTBNDDIR')
     h bnddir('WTGRPCFG')
      //*********************************************************************
      //==============================================================*
      //  Created   : 02/26/24                                         *
      //  Programmer: C:342                                            *
      //  Project   : WC2948 - Financial Earned Premium Report         *
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

     fWtFEPRpt  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflFEPSum:scSflRRN)
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)
      * User Parameter File
     fsmupl     if   e           k disk    rename(pup01:pup00)
      * financial earned premium work file
     fwafpl     if   e           k disk    rename(pfp01:pfp00)
      * Fund detail by year
     fwdf1l     if   e           k disk    rename(pf101:pf100)

     d psds          esds                  extname(szq1p)

      // work variables
     d intCo#          s              3P 0
     d intEmp#         s                   like(FPemp#) inz
     d intDiv          s                   like(FPdiv) inz
     d xxyrmo          s                   like(FPym01)
     d chkDate         s                   like(scDate)
     d x               s              3P 0 inz
     d process         s               n   inz(*off)
     d isError         s               n   inz
     d isOverlaid      s               n   inz

     d xxco#           s              3    inz
     d xxFnd           s              3    inz
     d xxYear          s              3    inz
     d xxDate          s              6    inz
     d xxoutq          s             10    inz
     d xxmode          s              1    inz

     d                 ds
     d  F1stpo                 1      7  0
     d  xxstym                 1      5  0
     d                 ds
     d  F1enpo                 1      7  0
     d  xxenym                 1      5  0

      * Clear Error/Message fields
     d ClrErr          pr

      * Load Financial Earned Premium Summary Grid
     d LoadFEPSum      pr

      * Main Processing
     d Main            pr

      * Process Financial Earned Premium Report
     d ProcFEPrmRpt    pr

      * Program Start
     d ProgramStart    pr

      // submit - submit financial earned premium report
     d SubFEPrmRpt     pr                  extpgm('W4090C')
     d xxco#                          3
     d xxFnd                          3
     d xxYear                         3
     d xxDate                         6
     d xxoutq                        10
     d xxmode                         1

      * Validate Financial Earned Premium Report
     d ValidateFEPRpt  pr              n

     d/copy sPrDate
     d/copy sPrGenPop
     d/copy sPrCompany
     d/copy wPrUsrPrms
     d/copy wPrPolErr

     c     keyWafpl      klist
     c                   kfld                    intCo#
     c                   kfld                    scFnd
     c                   kfld                    scYear
     c                   kfld                    intEmp#
     c                   kfld                    intDiv

     c     keyWdf1l      klist
     c                   kfld                    intCo#
     c                   kfld                    scFnd
     c                   kfld                    scYear

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
       erYear  = *off;
       erDate  = *off;
       msFnd   = *blanks;
       msYear  = *blanks;
       msDate  = *blanks;

      /End-Free

     p ClrErr          e

       //===================================================================
       // Load Financial Earned Premium Summary Grid
       //===================================================================
     p LoadFEPSum      b
     d LoadFEPSum      pi

      /free
       // Clear the subfile
       scSflRRN = 0;
       scSflDsp = *off;
       scSflClr = *on;
       Write rcdFEPRpt;
       scSflClr = *off;
       scSflDsp = *on;

       // Populate the subfile
       intEmp# = *zeros;
       intDiv  = *zeros;
       Chain Keywafpl wafpl;
       If %found(wafpl);
          x = 1;
          Dow x < 13;
             scSflRRN += 1;
             Select;
             When x = 1 and FPym01 <> 0;
                scMMYYYY = dt6MDYY(FPym01);
                scFEPAmt = FPam01;
                Write sflFEPSum;
             When x = 2 and FPym02 <> 0;
                scMMYYYY = dt6MDYY(FPym02);
                scFEPAmt = FPam02;
                Write sflFEPSum;
             When x = 3 and FPym03 <> 0;
                scMMYYYY = dt6MDYY(FPym03);
                scFEPAmt = FPam03;
                Write sflFEPSum;
             When x = 4 and FPym04 <> 0;
                scMMYYYY = dt6MDYY(FPym04);
                scFEPAmt = FPam04;
                Write sflFEPSum;
             When x = 5 and FPym05 <> 0;
                scMMYYYY = dt6MDYY(FPym05);
                scFEPAmt = FPam05;
                Write sflFEPSum;
             When x = 6 and FPym06 <> 0;
                scMMYYYY = dt6MDYY(FPym06);
                scFEPAmt = FPam06;
                Write sflFEPSum;
             When x = 7 and FPym07 <> 0;
                scMMYYYY = dt6MDYY(FPym07);
                scFEPAmt = FPam07;
                Write sflFEPSum;
             When x = 8 and FPym08 <> 0;
                scMMYYYY = dt6MDYY(FPym08);
                scFEPAmt = FPam08;
                Write sflFEPSum;
             When x = 9 and FPym09 <> 0;
                scMMYYYY = dt6MDYY(FPym09);
                scFEPAmt = FPam09;
                Write sflFEPSum;
             When x = 10 and FPym10 <> 0;
                scMMYYYY = dt6MDYY(FPym10);
                scFEPAmt = FPam10;
                Write sflFEPSum;
             When x = 11 and FPym11 <> 0;
                scMMYYYY = dt6MDYY(FPym11);
                scFEPAmt = FPam11;
                Write sflFEPSum;
             When x = 12 and FPym12 <> 0;
                scMMYYYY = dt6MDYY(FPym12);
                scFEPAmt = FPam12;
                Write sflFEPSum;
             Endsl;
             x = x + 1;
          Enddo;
       Endif;
       scSflRRN = 1;

      /end-free
     p LoadFEPSum      e

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

          // Load Financial Earned Premium Summary Grid
          LoadFEPSum();

          // Display Screen
          Exsr ExfmtScreens;
          // Clear error for Onchange property of Policy year
          ClrErr();

          // Perform actions on screen.
          Select;

          // Submit
          When BtnSubmit = *On;
             BtnSubmit = *Off;
             // Validate Edits
             If ValidateFEPRpt();
                ProcFEPrmRpt();
                Leave;
             EndIf;

          // Refresh
          When btnRefresh = *On;
             btnRefresh = *Off;
             scFnd  = UPfnd;
             scYear = UPfyr;
             scDate = *zeros;
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

           Exfmt rcdFEPRpt;

           Read rcdComPnl;
           Read rcdPanel;
        EndSl;

       Endsr;

      /End-Free

     p Main            e

       //===================================================================
       //ProcFEPrmRpt  - Process Financial Earned Premium Report
       //===================================================================
     p ProcFEPrmRpt    b
     d ProcFEPrmRpt    pi

      /Free

       xxco#  = %editc(UPco#:'X');
       xxFnd  = %editc(scFnd:'X');
       xxYear = %editc(scYear:'X');
       xxDate = %editc(scDate:'X');
       xxoutq = UPoutq;
       xxmode = 'D';
       // Financial Earned Premium Report - submit
       SubFEPrmRpt(xxco#:xxFnd:xxYear:xxDate:xxoutq:xxmode);

      /End-Free
     p ProcFEPrmRpt    e

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

       Return;

      /End-Free

     p ProgramStart    e

      //===================================================================
      // Validate the Financial Earned Premium Report
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateFEPRpt  b
     d ValidateFEPRpt  pi              n
      /Free

       // Initialize all the messageId and error indicators.
        isError = *off;
        erFnd  = *off;
        msFnd  = *blanks;
        erYear = *off;
        msYear = *blanks;
        erDate = *off;
        msDate = *blanks;

        // Group cannot be zero
        If scFnd = *zeros;
           erFnd = *on;
           msFnd = 'WCR0502';
           isError = *on;
        Endif;

        // Year cannot be zero
        If scYear = *zeros;
           erYear = *on;
           msYear = 'WCR0502';
           isError = *on;
        Endif;

        // Check valid month/year entered
        chkDate = dt6Check(scDate);
        If chkDate = *zero;
           erDate  = *on;
           msDate  = 'WCR0500';
           isError = *on;
        Endif;

        // Check valid month/year entered for year
        xxyrmo = dt5CYMD(scDate);
        If scDate <> *zeros and erDate = *off;
           Chain keywdf1l wdf1l;
           If %found(wdf1l);
              If xxyrmo < xxstym  or xxyrmo > xxenym;
                 erDate = *on;
                 msDate = 'WCR0722';
                 isError = *on;
              Endif;
           Endif;
        Endif;

        // Send a message if entered month/year has already been processed
        If scDate <> *zeros and isError = *off;
           If xxyrmo = FPym01 or xxyrmo = FPym02 or xxyrmo = FPym03 or
              xxyrmo = FPym04 or xxyrmo = FPym05 or xxyrmo = FPym06 or
              xxyrmo = FPym07 or xxyrmo = FPym08 or xxyrmo = FPym09 or
              xxyrmo = FPym10 or xxyrmo = FPym11 or xxyrmo = FPym12;

              isOverlaid = PopTwoBtn('Month/Year has already been Processed +
              for this Fund and Fund Year, and using this Date will Cause +
              the Data to be Overlaid.':'Confirm Overlaid':'Yes':'No');
              Select;
              When isOverlaid = *On;
              When isOverlaid = *Off;
                 isError = *on;
              EndSl;
           Endif;
        Endif;

      // Return *on indicator when No error exists.
        If isError = *on;
           return *off;
        Else;
           return *on;
        Endif;

      /End-Free
     p ValidateFEPRpt  e
