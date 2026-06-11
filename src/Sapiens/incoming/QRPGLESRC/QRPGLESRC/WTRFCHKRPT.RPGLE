     hcopyright('(c) 2024 Sapiens, Inc. All rights reserved.')
      //*********************************************************************
     h/title WTRFCHKRPT - Work with Return Funds Check Request Report
     h dftname(WTRFCHKRPT)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h ALWNULL(*UsrCtl)
     h/define   profoundUI
     h bnddir('STBNDDIR')
     h bnddir('WTGRPCFG')
      //*********************************************************************
      //==============================================================*
      //  Created   : 01/09/24                                         *
      //  Programmer: C:342                                            *
      //  Project   : WC2676 - Redesign of Work with Return Funds Check*
      //                       Request Report                          *
      //===============================================================*
      //  Revisions:                                                   *
      //                                                               *
      //                                                               *
      //                                                               *
      // ------------------------------------------------------------- *
      //  NOTE: Please add comments to the top of Revisions.           *
      // ------------------------------------------------------------- *
      //===============================================================*

     fWtRfChkRptcf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)
      * Company file
     fsmcol     if   e           k disk    rename(pco01:pco00)
      * Fund master File
     fwmfnl     if   e           k disk
      * User Parameter File
     fsmupl     if   e           k disk    rename(pup01:pup00)
      * Application System Control file
     fssylf     if   e           k disk
     f                                     include(SWC01)

     d intCo#          s              3P 0
     d clientID        s              3    inz('000')
     d isError         s               n   inz
     d process         s               n   inz(*off)

      // For W0503C21
     d inco#           s              3
     d inPrtRpt        s              1
     d inOutQ          s             10
     d inBtypPrtR      s              1

     d psds          esds                  extname(szq1p)

      * Clear Error/Message fields
     d ClrErr          pr

      * Process Print Report for Return Fund
     d ProcPrtRpt      pr

      * Main Processing
     d Main            pr

      * Program Start
     d ProgramStart    pr

      * Validate return fund check requests
     d ValidatePrtRpt  pr              n

      // Get client ID
     d GetClientID     pr                  ExtPgm('RTVCLTID')
     d   outCltID                     3

     d w0503c21        pr                  Extpgm('W0503C21')
     d inco#                          3
     d inPrtRpt                       1
     d inOutQ                        10
     d inBtypPrtR                     1

     d/copy sPrDate
     d/copy sPrGenPop
     d/copy sPrCompany
     d/copy sPrUsrPrms

      /Free

       GetClientID(clientID);

       ProgramStart();

       If process;
          Main();
       Endif;

       *InLr = *On;

      /End-Free


       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

      /Free

       // Process until exit button is pressed.
       Dou BtnPRCan = *On;

          // Display Screen
          Exsr ExfmtScreens;

          Select;
          When BtnPRSub = *on;
             BtnPRSub = *off;
             // Validate Edits
             If ValidatePrtRpt();
                Exsr ProcSubmit;
                Leave;
             EndIf;

          When BtnRefresh = *On;
             BtnRefresh =*Off;
             // Clear Errors
             ClrErr();
             scBtypPrtR = *blanks;
             scPrtRpt   = *blanks;

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

           Exfmt rcdRFCRpt;

           Read rcdComPnl;
           Read rcdPanel;
        EndSl;

       Endsr;

       //===================================================================
       //Submitter - Checks to be Written Report
       //===================================================================
       Begsr ProcSubmit;
        inco# = %char(UPCo#);
        inPrtRpt = scPrtRpt;
        inoutq = UPoutq;
        inBtypPrtR = scBtypPrtR;
        W0503C21(inco#:inPrtRpt:inoutq:inBtypPrtR);
       Endsr;

      /End-Free

     p Main            e

       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /Free
       intCo# = GetUserInfo(q1user);
       Chain intCo# pco00;
       If %found(smcol) and COname <> ' ';
          scCompName = COname;
       Endif;
       scCo# = intCo#;

       // Retrieve user parameter
       Chain q1user pup00;

       // Get Flags value from Workers Comp control file (SWCPF)
       Chain 'WC' SWC01;

       // Check if Return Fund Module is Installed or Not
       If WCfl25 <> 'Y';
          PopOneBtn('The Return Fund Module has not been Installed. Please -
           contact Sapiens for more Information.':'Return Fund Process');

          process = *off;
          Return;
       Endif;

       If WCfl36 = 'Y';
          SelChoices = ',Account Current,Deposit,+
                        Independent Cash Distribution,Premium';
          SelValues  = ',A,D,I,P';
       Else;
          SelChoices = ',Account Current,Premium';
          SelValues  = ',A,P';
       Endif;

       process = *on;
       Return;

      /End-Free

     p ProgramStart    e

      //===================================================================
      // Validate the Return Fund Check Print Report
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidatePrtRpt  b
     d ValidatePrtRpt  pi              n
      /Free

       // Initialize all the messageId and error indicators.
        isError = *off;
        erPrtRpt = *off;
        msPrtRpt = *blanks;

        If scPrtRpt = *blanks;
           erPrtRpt = *on;
           msPrtRpt = 'WCR0503';
           isError = *on;
        Endif;

      // Return *on indicator when No error exists.
        If isError = *on;
           return *off;
        Else;
           return *on;
        Endif;

      /End-Free
     p ValidatePrtRpt  e

       //===================================================================
       // ClrErr - Clear Error/Message fields
       //===================================================================
     p ClrErr          b
     d ClrErr          pi

      /Free

       // Initialize all the messageId and error indicators.
       isError = *off;
       erPrtRpt = *off;
       msPrtRpt = *blanks;

      /End-Free

     p ClrErr          e

