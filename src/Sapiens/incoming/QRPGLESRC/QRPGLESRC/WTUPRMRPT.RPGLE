     hcopyright('(c) 2024 Sapiens, Inc. All rights reserved.')
      //*********************************************************************
     h/title WTUPRMRPT - Unearned Premium Report
     h dftname(WTUPRMRPT)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h ALWNULL(*UsrCtl)
     h/define   profoundUI
     h bnddir('WTBNDDIR')
     h bnddir('WTGRPCFG')
      //*********************************************************************
      //==============================================================*
      //  Created   : 01/22/24                                         *
      //  Programmer: C:342                                            *
      //  Project   : WC2946 - Unearned Premium Report                 *
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

     fWtUPrmRpt cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)
      * User Parameter File
     fsmupl     if   e           k disk    rename(pup01:pup00)

     d psds          esds                  extname(szq1p)

      // work variables

     d intCo#          s              3P 0
     d process         s               n   inz(*off)
     d charMsg         s            100    inz
     d isError         s               n   inz

     d xxco#           s              3    inz
     d xxFnd           s              3    inz
     d xxYear          s              3    inz
     d xxDate          s              8    inz
     d xxoutq          s             10    inz

      * Clear Error/Message fields
     d ClrErr          pr

      * Main Processing
     d Main            pr

      * Process Unearned Premium Report
     d ProcUPrmRpt     pr

      * Program Start
     d ProgramStart    pr

      // submit - unearned premium report
     d SubUPrmRpt      pr                  extpgm('W4075C')
     d xxco#                          3
     d xxFnd                          3
     d xxYear                         3
     d xxDate                         8
     d xxoutq                        10

      * Validate Unearned Premium Report
     d ValidateUPRpt   pr              n

     d/copy sPrDate
     d/copy sPrGenPop
     d/copy sPrCompany
     d/copy wPrUsrPrms
     d/copy wPrPolErr

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
       erRunDate  = *off;
       erFnd   = *off;
       erYear  = *off;
       msRunDate  = *blanks;
       msFnd   = *blanks;
       msYear  = *blanks;
       charMsg = *blanks;

      /End-Free

     p ClrErr          e

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

          // Submit
          When BtnSubmit = *On;
             BtnSubmit = *Off;
             // Validate Edits
             If ValidateUPRpt();
                ProcUPrmRpt();
                Leave;
             EndIf;

          // Refresh
          When btnRefresh = *On;
             btnRefresh = *Off;
             scFnd  = UPfnd;
             scYear = UPfyr;
             scRunDate = DateTo8(systemDate());
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

           Exfmt rcdUPrmRpt;

           Read rcdComPnl;
           Read rcdPanel;
        EndSl;

       Endsr;

      /End-Free

     p Main            e

       //===================================================================
       //ProcDrCrRpt - Process Unearned Premium Report
       //===================================================================
     p ProcUPrmRpt     b
     d ProcUPrmRpt     pi

      /Free

       xxco#  = %editc(UPco#:'X');
       xxFnd  = %editc(scFnd:'X');
       xxYear = %editc(scYear:'X');
       xxDate = %editc(scRunDate:'X');
       xxoutq = UPoutq;
       // submit - unearned premium report
       SubUPrmRpt(xxco#:xxFnd:xxYear:xxDate:xxoutq);

      /End-Free
     p ProcUPrmRpt     e

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
          scRunDate = DateTo8(systemDate());
          scCo#  = UPco#;
          process = *on;
       Else;
          process = *off;
       Endif;

       Return;

      /End-Free

     p ProgramStart    e

      //===================================================================
      // Validate the Unearned Premium Report
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateUPRpt   b
     d ValidateUPRpt   pi              n
      /Free

       // Initialize all the messageId and error indicators.
        isError = *off;
        erRunDate = *off;
        msRunDate = *blanks;
        erFnd  = *off;
        msFnd  = *blanks;
        erYear = *off;
        msYear = *blanks;
        charMsg = *blanks;

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

        // Validate Date
        erRunDate  = ValidDate(scRunDate:charMsg);
        If erRunDate   = *on;
           msRunDate   = charMsg;
           isError = *on;
        Endif;

      // Return *on indicator when No error exists.
        If isError = *on;
           return *off;
        Else;
           return *on;
        Endif;

      /End-Free
     p ValidateUPRpt   e
