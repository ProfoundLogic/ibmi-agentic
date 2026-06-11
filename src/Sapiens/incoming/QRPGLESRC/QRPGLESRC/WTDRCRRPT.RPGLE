     hcopyright('(c) 2024 Sapiens, Inc. All rights reserved.')
      //*********************************************************************
     h/title WTDRCRRPT - Debit/Credit Report
     h dftname(WTDRCRRPT)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h ALWNULL(*UsrCtl)
     h/define   profoundUI
     h bnddir('WTBNDDIR')
     h bnddir('WTGRPCFG')
      //*********************************************************************
      //==============================================================*
      //  Created   : 01/17/24                                         *
      //  Programmer: C:342                                            *
      //  Project   : WC2945 - Redesign of Debit/Credit Report         *
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

     fWtDrCrRpt cf   e             workstn
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
     d isError         s               n   inz
     d charMsg         s            100    inz
     d suffix          s              3    inz
     d RptLib          s             10a   inz

     d xxco#           s              3    inz
     d xxFnd           s              3    inz
     d xxYear          s              3    inz
     d xxDate          s              8    inz
     d xxoutq          s             10    inz
     d xxmode          s              1    inz

      * Clear Error/Message fields
     d ClrErr          pr

      * Get suffix
     d GetSuffix       PR                  extpgm('SA030R')
     d   suffix                       3

      * Main Processing
     d Main            pr

      * Process Debit/Credit Report
     d ProcDrCrRpt     pr

      * Program Start
     d ProgramStart    pr

     d SubDrCrRpt      pr                  extpgm('W4050C')
     d xxco#                          3
     d xxFnd                          3
     d xxYear                         3
     d xxDate                         8
     d xxoutq                        10
     d xxMode                         1

      * Validate Debit/Credit Report
     d ValidateDCRpt   pr              n

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
       erBalDate  = *off;
       erFnd   = *off;
       erYear  = *off;
       msBalDate  = *blanks;
       msFnd   = *blanks;
       msYear  = *blanks;

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
             If ValidateDCRpt();
                ProcDrCrRpt();
                Leave;
             EndIf;

          // Refresh
          When btnRefresh = *On;
             btnRefresh = *Off;
             scFnd  = UPfnd;
             scYear = UPfyr;
             scBalDate = DateTo8(systemDate());
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

           Exfmt rcdDrCrRpt;

           Read rcdComPnl;
           Read rcdPanel;
        EndSl;

       Endsr;

      /End-Free

     p Main            e

       //===================================================================
       //ProcDrCrRpt - Process Debit/Credit Report
       //===================================================================
     p ProcDrCrRpt     b
     d ProcDrCrRpt     pi

      /Free

       xxco#  = %editc(UPco#:'X');
       xxFnd  = %editc(scFnd:'X');
       xxYear = %editc(scYear:'X');
       xxDate = %editc(scBalDate:'X');
       xxoutq = UPoutq;
       xxmode = 'D';
       // submit - debit / credit report
       SubDrCrRpt(xxco#:xxFnd:xxYear:xxDate:xxoutq:xxmode);

      /End-Free
     p ProcDrCrRpt     e

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
          scBalDate = DateTo8(systemDate());
          scCo#  = UPco#;
          process = *on;
       Else;
          process = *off;
       Endif;

       // Retrieve library suffix and build Report library name
       GetSuffix(suffix);
       If suffix <> *blanks;
          RptLib = 'PDSRPT' + %Trim(suffix);
       Else;
          RptLib = 'PDSRPT';
       Endif;
       scRptMsg = 'This Report will Produce Output File WFDCP in Library ' +
                  %trim(RptLib);

       Return;

      /End-Free

     p ProgramStart    e

      //===================================================================
      // Validate the Debit/Credit Report
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateDCRpt   b
     d ValidateDCRpt   pi              n
      /Free

       // Initialize all the messageId and error indicators.
        isError = *off;
        erBalDate = *off;
        msBalDate = *blanks;
        erFnd  = *off;
        msFnd  = *blanks;
        erYear = *off;
        msYear = *blanks;

        // Fund Cannot be zero
        If scFnd = *zeros;
           erFnd = *on;
           msFnd = 'WCR0502';
           isError = *on;
        Endif;

        // Year Cannot be zero
        If scYear = *zeros;
           erYear = *on;
           msYear = 'WCR0502';
           isError = *on;
        Endif;

        // Validate Date
        erBalDate  = ValidDate(scBalDate:charMsg);
        If erBalDate   = *on;
           msBalDate   = charMsg;
           isError = *on;
        Endif;

      // Return *on indicator when No error exists.
        If isError = *on;
           return *off;
        Else;
           return *on;
        Endif;

      /End-Free
     p ValidateDCRpt   e
