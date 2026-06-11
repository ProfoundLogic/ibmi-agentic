     hcopyright('(c) 2024 Sapiens, Inc. All rights reserved.')
      //*********************************************************************
     h/title WTRFCHKREG - Work with Return Funds Check Register
     h dftname(WTRFCHKREG)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h ALWNULL(*UsrCtl)
     h/define   profoundUI
     h bnddir('WTBNDDIR')
     h bnddir('WTGRPCFG')
      //*********************************************************************
      //==============================================================*
      //  Created   : 11/23/23                                         *
      //  Programmer: C:333                                            *
      //  Project   : WC2676 - Redesign of Work with Return Funds      *
      //===============================================================*
      //  Revisions:                                                   *
      //                                                               *
      //                                                               *
      //                                                               *
      // ------------------------------------------------------------- *
      //  NOTE: Please add comments to the top of Revisions.           *
      // ------------------------------------------------------------- *
      //===============================================================*

     fWTRFCHKREGcf   e             workstn extfile('WTRFCHKREG')
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     infds(fids01)
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)
      * Fund master File
     fwmfnl     if   e           k disk
      * User Parameter File
     fwmupl     if   e           k disk
      * Workers Compensation Control File
     fswcpf     if   e           k disk

     d ##kc            s              1A
     d ##kl            s              1A
     d ##call          s              1A
     d #1co#           s              3A
     d ##acct          s             20A
     d intCo#          s              3P 0
     d ##ffnd          s              3A
     d ##tfnd          s              3A
     d ##fdat          s              8A
     d ##tdat          s              8A
     d ##ttyp          s              1A
     d ##sort          s              1A
     d ##outq          s             10A
     d ##btyp          s              1A
     d charMsg         s            100

     d fids01        e ds                  extname(szq2p)

     d psds          esds                  extname(szq1p)

      * Search Bank Account
     d BankAccount     pr                  Extpgm('W2422R')
     d  @@p1                          1A
     d  @@p2                          1A
     d  @@P3                          1A
     d  @@P4                          3A
     d  @@P5                         20A

      * Clear Error/Message fields
     d ClrErr          pr

      * Intialize the Screen fields
     d LoadInVal       pr

      * Main Processing
     d Main            pr

      * Program Start
     d ProgramStart    pr

     d Submitter       pr                  Extpgm('W7514C')
     d #1co#                          3A
     d ##ffnd                         3A
     d ##tfnd                         3A
     d ##fdat                         8A
     d ##tdat                         8A
     d ##acct                        20A
     d ##ttyp                         1A
     d ##sort                         1A
     d ##outq                        10A
     d ##btyp                         1A
      *
     * Validate edits
     d ValidSel        pr              n
      *
     d WTRFCHKR        pr                  Extpgm('WTRFCHKREG')
     d  inPrds                       40A   const
     d  inCall                        1A
     d
     d WTRFCHKR        pi
     d  inPrds                       40A   const
     d  inCall                        1A

      *
     d/copy sPrDate
     d/copy sPrGenPop
     d/copy sPrCompany
     d/copy wPrPolErr
     d/copy wPrUsrPrms

      /Free
       //Display window if return premium module not installed
       Read SWCPF;
       If %eof or WCFL25 <> 'Y';
          PopOneBtn('The Return Fund Module has not been Installed. Please -
           contact Sapiens for more Information.':'Return Fund Process');
          *InLr = *on;
          return;
       Endif;

       // Start Of Program
       ProgramStart();

       // Main Procesing
       Main();

       *InLr = *on;

      /End-Free

       //===================================================================
       // ClrErr - Clear Error/Message fields
       //===================================================================
     p ClrErr          b
     d ClrErr          pi

      /Free
       // Initialize all the messageId and error indicators.
       msSortOpt = *Blanks;
       erSortOpt = *off;
       msFrmCkDt = *Blanks;
       erFrmCkDt = *off;
       msToCkDt  = *Blanks;
       erToCkDt  = *off;
       msFGrpRng = *Blanks;
       erFGrpRng = *off;
       msTGrpRng = *Blanks;
       erTGrpRng = *off;

      /End-Free

     p ClrErr          e

       //===================================================================
       // LoadInVal - Intialize the Screen fields
       //===================================================================
     p LoadInVal       b
     d LoadInVal       pi

      /Free
       scSortOpt  = *Blanks;
       scBtypreg  = *Blanks;
       scFGrpRng  = 0;
       scTGrpRng  = 0;
       scFrmCkDt  = 0;
       scToCkDt   = 0;
       scAcct     = *Blanks;
       scTtype    = *Blanks;

      /End-Free
     p LoadInVal       e

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

      /Free

       // Process until exit button is pressed.
       Dou BtnScrCan = *On;

          // Set Choices and Choices values for Batch Type
          scBtypCH = *Blanks;
          scBtypVal = *Blanks;
          scBtypCH = ',Account Current,Premium';
          scBtypVal = ',A,P';
          If WCfl36 = 'Y';
             scBtypCH = ',Account Current,Premium,Deposit,+
              Independent Cash Distribution';
             scBtypVal = ',A,P,D,I';
          Endif;

          // Display Screen
          Exsr ExfmtScreens;

          Select;
          When BtnSubmit = *on;
             ClrErr();
             // Validate Edits
             If ValidSel();
                Exsr Process;
                Leave;
             EndIf;

          When BtnScrCan = *on;
             Leave;

          When BtnRefresh = *On;
             BtnRefresh =*Off;
             // Intialize the Screen fields
             LoadInVal();
             // Clear Errors
             ClrErr();
             // Load Action panel with the Company Name.
             intCo# = userCo#(q1user);
             scCompName = GetCompNam(intCo#);

          When selAccSrch = *On;
             selAccSrch = *Off;
             ##kc = *Blanks;
             ##kl = *Blanks;
             ##call = '2';
             #1co# = *Blanks;
             ##acct = *Blanks;
             BankAccount(##kc:##kl:##call:#1co#:##acct);
             If ##acct <> *Blank;
                scacct = ##acct;
             Endif;
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
           Write RcdPanel;

           Exfmt RcdRFCReg;

           Read rcdComPnl;
           Read RcdPanel;

        EndSl;

       Endsr;
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr Process;
        Chain(n) (q1user) pup01;
        #1co#  = %editc(intCo#:'X');
        ##ffnd = %editc(scFGrpRng:'X');
        ##tfnd = %editc(scTGrpRng:'X');
        ##fdat = %editc(scFrmCkDt:'X');
        ##tdat = %editc(scToCkDt:'X');
        Submitter(#1co#:##ffnd:##tfnd:##fdat:##tdat:
           scAcct:scTtype:scSortOpt:upoutq:scBtypreg);

       EndSr;

      /End-Free

     p Main            e

       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /Free

       // Intialize the Screen fields
       LoadInVal();

       // Load Action panel with the Company Name.
       intCo# = userCo#(q1user);
       scCompName = GetCompNam(intCo#);

       Return;

      /End-Free

     p ProgramStart    e

      //===================================================================
      // Validate the Return Fund Checks Register
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidSel        b
     d ValidSel        pi              n
     d isError         s               n   inz
      /Free

        isError = *off;
        If scSortOpt = ' ';
           erSortOpt = *on;
           msSortOpt = 'WCR0503';
           isError = *on;
        Endif;

        If scFrmCkDt <> 0;
           erFrmCkDt = ValidDate(scFrmCkDt:charMsg);
           If erFrmCkDt = *on;
              msFrmCkDt = charMsg;
              isError = *on;
           Endif;
        Endif;

        If scToCkDt <> 0;
           erToCkDt = ValidDate(scToCkDt:charMsg);
           If erToCkDt = *on;
              msToCkDt = charMsg;
              isError = *on;
           Endif;
        Endif;

        If (scTGrpRng < scFGrpRng);
           erFGrpRng = *on;
           erTGrpRng = *on;
           msFGrpRng = 'WCR0886';
           msTGrpRng = 'WCR0886';
           isError = *on;
        Endif;

        If (erFrmCkDt <> *on) and (erToCkDt <> *on);
           If AscendDates(scFrmCkDt:scToCkDt:charMsg) = *on;
              erFrmCkDt = *on;
              erToCkDt = *on;
              msFrmCkDt = charMsg;
              msToCkDt = charMsg;
              isError = *on;
           Endif;
        Endif;

       // Return *on indicator when No error exists.
        If isError = *on;
           return *off;
        Else;
           return *on;
        Endif;

      /End-Free
     p ValidSel        e
