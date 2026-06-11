     hcopyright('(c) 2024 Sapiens, Inc. All rights reserved.')
      //*********************************************************************
     h/title WTAR4YRCAP - A/R 4 Year Recap
     h dftname(WTAR4YRCAP)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h ALWNULL(*UsrCtl)
     h/define   profoundUI
     h bnddir('WTBNDDIR')
      //*********************************************************************
      //==============================================================*
      //  Created   : 04/22/24                                         *
      //  Programmer: C:342                                            *
      //  Project   : WC2949 - A/R 4 Year Recap                        *
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

     fWtAr4YRCapcf   e             workstn
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
     d xxYear1         s              3    inz
     d xxYear2         s              3    inz
     d xxYear3         s              3    inz
     d xxYear4         s              3    inz
     d xxDate          s              8    inz
     d xxoutq          s             10    inz
     d xxmode          s              1    inz
     d xxFeeTrx        s              1    inz

      * Clear Error/Message fields
     d ClrErr          pr

      * Main Processing
     d Main            pr

      * Process A/R 4 Year Recap
     d ProcAR4YrRRpt   pr

      * Program Start
     d ProgramStart    pr

      // submit - A/R 4 year recap report
     d SubAR4YrRRpt    pr                  extpgm('W4060C')
     d xxco#                          3
     d xxFnd                          3
     d xxYear1                        3
     d xxYear2                        3
     d xxYear3                        3
     d xxYear4                        3
     d xxDate                         8
     d xxoutq                        10
     d xxmode                         1
     d xxFeeTrx                       1

      * Validate A/R 4 year recap report
     dValidateAR4YRpt  pr              n

     d/copy sPrDate
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
       erCurDate  = *off;
       erFnd   = *off;
       erYear1 = *off;
       msCurDate  = *blanks;
       msFnd   = *blanks;
       msYear1 = *blanks;
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
             If ValidateAR4YRpt();
                ProcAR4YrRRpt();
                Leave;
             EndIf;

          // Refresh
          When btnRefresh = *On;
             btnRefresh = *Off;
             clear rcdAR4YRcp;
             scCo#   = UPco#;
             scFnd   = UPfnd;
             scYear1 = UPfyr;
             scInFeeTrx =  'N';
             scCurDate = DateTo8(systemDate());
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

           Exfmt rcdAR4YRcp;

           Read rcdComPnl;
           Read rcdPanel;
        EndSl;

       Endsr;

      /End-Free

     p Main            e

       //===================================================================
       //ProcAR4YrRRpt - Process A/R 4 year recap report
       //===================================================================
     p ProcAR4YrRRpt   b
     d ProcAR4YrRRpt   pi

      /Free

       xxco#   = %editc(UPco#:'X');
       xxFnd   = %editc(scFnd:'X');
       xxYear1 = %editc(scYear1:'X');
       xxYear2 = %editc(scYear2:'X');
       xxYear3 = %editc(scYear3:'X');
       xxYear4 = %editc(scYear4:'X');
       xxDate  = %editc(scCurDate:'X');
       xxoutq  = UPoutq;
       xxmode  = 'D';
       If scInFeeTrx = *blanks;
          scInFeeTrx = 'N';
       Endif;
       xxFeeTrx= scInFeeTrx;
       // submit - A/R 4 year recap report
       SubAR4YrRRpt(xxco#:xxFnd:xxYear1:xxYear2:xxYear3:xxYear4:xxDate:
                    xxoutq:xxmode:xxFeeTrx);

      /End-Free
     p ProcAR4YrRRpt   e

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
          scYear1 = UPfyr;
          scCurDate = DateTo8(systemDate());
          scCo#  = UPco#;
          process = *on;
       Else;
          process = *off;
       Endif;

       Return;

      /End-Free

     p ProgramStart    e

      //===================================================================
      // Validate the A/R 4 Year Recap Report
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     pValidateAR4YRpt  b
     dValidateAR4YRpt  pi              n
      /Free

       // Initialize all the messageId and error indicators.
        ClrErr();

        // Group cannot be zero
        If scFnd = *zeros;
           erFnd = *on;
           msFnd = 'WCR0502';
           isError = *on;
        Endif;

        // Year cannot be zero
        If scYear1 = *zeros;
           erYear1 = *on;
           msYear1 = 'WCR0502';
           isError = *on;
        Endif;

        // Validate Date
        erCurDate  = ValidDate(scCurDate:charMsg);
        If erCurDate   = *on;
           msCurDate   = charMsg;
           isError = *on;
        Endif;

      // Return *on indicator when No error exists.
        If isError = *on;
           return *off;
        Else;
           return *on;
        Endif;

      /End-Free
     pValidateAR4YRpt  e
