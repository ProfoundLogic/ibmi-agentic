     hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h*****************************************************************
     h/title WC2473 - Prompt for Active Agent Assignment List
     h/define CONVERT
     h dftactgrp(*no) actgrp(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     H*****************************************************************
     H/TITLE WTRPTAGTAL - Prompt for Active Agent Assignment List
     h*****************************************************************
     h* Revisions                                                     *
     h*                                                               *
     h*                                                               *

     fWTRPTAGTALcf   e             workstn extfile('WTRPTAGTAL')
     f                                     handler('PROFOUNDUI(HANDLER)')
     fWtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdPolPnl)
     f                                     include(rcdQtePnl)
     f                                     include(rcdComPnl)
     f*
     f* User Parameter Master File
     fWmupl     IF   E           K DISK    Rename(pup01:pup00)

     dpsds           esds                  extname(szq1p)

      *
     d WtAgtList       pr                  Extpgm('WTRPTAGTAL')
     d  inPrds                       40A   const
     d  inCall                        1A
     d
     d WtAgtList       pi
     d  inPrds                       40A   const
     d  inCall                        1A

     d intCo#          s              3  0
     d strOutq         s                   Like(Upoutq)
      *
      *
     d inCo#           s              3
     d inOutq          s             10
      *****************************************************************
      * Prototype Declaration
      *****************************************************************

     d/copy wPrUsrPrms
     d/copy sPrCompany

      * Main Processing
     d Main            pr

      * Program Start
     d ProgramStart    pr

      * Active Agent Assignment List Report Proc
     d AgtLstproc      pr

      * Active Agent Assignment List Report Submit
     d AgtLstSbm       pr                  ExtPgm('W4070C')
     d inCo#                          3
     d inOutq                        10

      /Free
       // Start Of Program
       ProgramStart();

       // Main Procesing
       Main();

       *InLr = *on;

      /End-Free
       //===================================================================
       // AgtLstproc - Recovery Report Submit
       //===================================================================
     p AgtLstproc      b
     d AgtLstproc      pi

      /Free

       inCo#     = %EditC(intCo#:'X');
       inOutq    = strOutq;

        AgtLstSbm(inCo#:inOutq);

      /End-Free

     p AgtLstproc      e
       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

      /Free

       // Process until exit button is pressed.
       Dou ExitNow = *On ;

          // Display Screen
          Exsr ExfmtScreens;

          Select;
          When BtnSubmit = *on;
             BtnSubmit = *off;
             AgtLstproc();
             Leave;
          When BtnCancel = *on;
             Leave;

          // do nothing - catch tab click event
          When tabClick = *on;
             tabClick = *off;
          When BtnRefresh = *On;
             BtnRefresh =*Off;
             // Load Action panel with the Company Name.
             intCo# = userCo#(q1user);
             scCompName = GetCompNam(intCo#);
          EndSl;

       Enddo;
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       Select;

       When TabIndex = 0;
          TabHeight = 770;
          APnlHeight = TabHeight + 60;

          Write rcdComPnl;
          Write RcdAgtLR;

          Exfmt RcdAgtLRpt;

          Read rcdComPnl;
          Read RcdAgtLR;

       EndSl;

       Endsr;

      /End-Free

     p Main            e
       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /Free

       // Load Action panel with the Company Name.
       intCo# = userCo#(q1user);
       scCompName = GetCompNam(intCo#);

       // Retrieve Output Queue of the User.
       Chain(n) q1user pup00;
       If %Found(Wmupl);
          strOutq = Upoutq;
       EndIf;

       tabClick = *Off;
       Return;
      /End-Free

     p ProgramStart    e
