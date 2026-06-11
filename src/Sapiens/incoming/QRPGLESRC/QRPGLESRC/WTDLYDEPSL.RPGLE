     hcopyright('(c) 2024 Sapiens, Inc. All rights reserved.')
      //*********************************************************************
     h/title WTDLYDEPSL - Daily Deposit Slip
     h dftname(WTDLYDEPSL)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h ALWNULL(*UsrCtl)
     h/define   profoundUI
     h bnddir('WTBNDDIR')
     h bnddir('WTGRPCFG')
      //*********************************************************************
      //==============================================================*
      //  Created   : 01/18/24                                         *
      //  Programmer: C:342                                            *
      //  Project   : WC2943 - Redesign of Daily Deposit Slip          *
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

     fWtDlyDepSlcf   e             workstn
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
     d xxDate1         s              8  0
     d isError         s               n   inz
     d charMsg         s            100

     d                 ds
     d  xxData                 1     80
     d  xxType                 1      1
     d  UPco#                  2      4  0
     d  scFnd                  5      7  0
     d  xxDate                 8     15  0
     d  UPoutq                16     25
     d  UPuser                26     35

      * Clear Error/Message fields
     d ClrErr          pr

     d LoadDataQ       pr                  extpgm('QSNDDTAQ')
     d ##DataQ                       10    const
     d ##DataQLib                    10    const
     d ##DataQLen                     5  0 const
     d ##Data                        80    const

      * Main Processing
     d Main            pr

      * Process Daily Deposit Slip
     d ProcDDSlip      pr

      * Program Start
     d ProgramStart    pr

      * Validate Daily Deposit Slip
     d ValidateDDSlip  pr              n

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
       erDate  = *off;
       erFnd   = *off;
       charMsg = *blanks;
       msDate  = *blanks;
       msFnd   = *blanks;

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
             If ValidateDDSlip();
                ProcDDSlip();
                Leave;
             EndIf;

          // Refresh
          When btnRefresh = *On;
             btnRefresh = *Off;
             scFnd  = userGrp(q1user);
             scDate = DateTo8(systemDate());
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

           Exfmt rcdDDslip;

           Read rcdComPnl;
           Read rcdPanel;
        EndSl;

       Endsr;

      /End-Free

     p Main            e

       //===================================================================
       //ProcDDSlip - Process Daily Deposit Slip
       //===================================================================
     p ProcDDSlip      b
     d ProcDDSlip      pi

      /Free

       //Send Request to data queue to run report
       xxType = 'E';
       xxDate = scDate;
       LoadDataQ ('WEDITQ' :'*LIBL':80:xxData);

      /End-Free
     p ProcDDSlip      e

       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /Free

       // Load Action panel with the Company Name.
       intCo# = userCo#(q1user);
       scFnd  = userGrp(q1user);
       scDate = DateTo8(systemDate());
       scCompName = GetCompNam(intCo#);

       // Retrieve user parameter
       Chain q1user pup00;
       If %Found(Smupl);
          process = *on;
       Else;
          process = *off;
       Endif;

       Return;

      /End-Free

     p ProgramStart    e

      //===================================================================
      // Validate the Daily Deposit Slip
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateDDSlip  b
     d ValidateDDSlip  pi              n
      /Free

       // Initialize all the messageId and error indicators.
        isError = *off;
        erDate = *off;
        msDate = *blanks;
        erFnd  = *off;
        msFnd  = *blanks;
        charMsg = *blanks;

        // Validate Fund
        If scFnd = *zeros;
           erFnd = *on;
           msFnd = 'WCR0502';
           isError = *on;
        Endif;

        // Validate Date
        erDate  = ValidDate(scDate:charMsg);
        If erDate   = *on;
           msDate   = charMsg;
           isError = *on;
        Endif;

      // Return *on indicator when No error exists.
        If isError = *on;
           return *off;
        Else;
           return *on;
        Endif;

      /End-Free
     p ValidateDDSlip  e

