     hcopyright('(c) 2022 Sapiens, Inc. All rights reserved.')

      //*********************************************************************
     h/title WTAGTSRVRT - Agency Service Request Tracking
     h dftname(WTAGTSRVRT)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h ALWNULL(*UsrCtl)
     h/define   profoundUI
     h bnddir('STBNDDIR')
     h bnddir('WTGRPCFG')
      //*********************************************************************
      //==============================================================*
      //  Created   : 12/29/21                                         *
      //  Programmer: C:300                                            *
      //  Project   : WC2395 - Redesign of Agency Configuration program*
      //===============================================================*
      //  Revisions:                                                   *
      //                                                               *
      //                                                               *
      // ------------------------------------------------------------- *
      //  NOTE: Please add comments to the top of Revisions.           *
      // ------------------------------------------------------------- *
      //===============================================================*

     fWTAGTSRVRTcf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdAgtPnl)
      * Agent Master file
     fwmagp     if   e             disk    rename(pag01:pag0p)
     f                                     infds(dbfds)
      * Company file
     fsmcol     if   e           k disk    rename(pco01:pco00)

     d psds          esds                  extname(szq1p)
     d dbfds           ds
     d  dbrrn                397    400b 0


      // work variables
     d clientID        s              3    inz('000')
     d intCo#          s              3  0
     d inCo#           s              3  0
     d xxRrn           s              9  0
     D indexTab        s              5  0
     d chgMod          s               n   inz(*off)
     d dspMode         s               n   inz(*off)
     d error           s               n   inz(*off)
     d process         s               n   inz(*off)
     d charMsg         s            100
     d xxst            s              2    inz
     d ##Category      s             10    inz
     d ##Assign        s             10    inz
     d ##VstFrDt       s              8    inz
     d ##VstToDt       s              8    inz

      // Proto-types

      // Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3

      // Service Request
     d Serv_Req        pr                  extpgm('WTVSTTRK')
     d   @@rrn                        9
     d   @@mode                       1
     d   ##Category                  10
     d   ##Assign                    10
     d   ##VstFrDt                    8
     d   ##VstToDt                    8

      * Main Processing
     d Main            pr

      * Program Start
     d ProgramStart    pr
     d processMe                       n


     d/copy sprUsrPrms
     d/copy sPrGenPop
     d/copy sPrDate
     d/copy sPrString
     d/copy sPrSql
     d/copy sPrMessage
     d/copy sPrModule
     d/copy sCopyCompr
     d/copy sprCompany
     d/copy sPrTable
     d/copy sPrTrigger
     d/copy sPrTrig
     d/copy wPrTable
     d/copy sPrAtrium
     d/copy wPrPolErr
     d/copy wPrGroup
     d/copy sPrGenErr
     d/copy sCopyGrpPr

      //
      //   Key List for the Documents Table File
      //

      // Entry of Program.

     c     *entry        plist
     c                   parm                    @@rrn             9            rrn
     c                   parm                    @@mode            1            mod
     c                   parm                    @@next           10            next pgm
     c                   parm                    @@TabInd          5            tab index

      /free
       Exec Sql Set Option Commit = *None;

       If %parms >= 1;
          xxRrn = %int(@@rrn);
       Endif;

       Chain xxRrn pag0p;
       inCo# = AGco#;
       scCo# = AGco#;

       If (@@next = *Blanks or @@next = 'WTAGTSRVRT');
          GetClientID(clientID);

          ProgramStart(process);

          If process;

             Main();

          Endif;
       Endif;


       *InLr = *On;
      /end-free

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi
      /free

       // Process until exit button is pressed.
       DoU BtnCancel = *On;

          // display screen
          Exsr ExfmtScreens;

          error = *off;

          // Perform actions on screen.
          Select;

          // do nothing - catch tab click event
          When TabClick = *on;
             TabClick = *off;

          // Refresh
          When btnRefresh = *On;
             btnRefresh = *Off;

          // For "Change FEIN" option in More Action of Agency Toolbar
          When btnShowWin = *on;
             PopupWindow(txtShowWin:scActRRN:' ':txtWinData);
             btnShowWin = *off;


          // Menu Choices
          When scSelOpt <> *blanks;
             @@next = scSelOpt;
             Leave;


          Endsl;
       Enddo;

       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       indexTab = TabIndex + 1;

       Select;
       When indexTab = 1;
          pnlHeight = 750;
          APnlHeight = pnlHeight + 60;

          ##Category = 'Agency';
          ##Assign   = ' ';
          ##VstFrDt  = ' ';
          ##VstToDt  = ' ';

          Write rcdAgtPnl;
        //Write rcdPanel;
          Serv_Req(@@rrn:@@Mode:##Category:##Assign:##VstFrDt:##VstToDt);
          Read rcdAgtPnl;
        //Read rcdPanel;

       EndSl;

       Endsr;

      /end-free
     p Main            e

       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi
     d processMe                       n
      /free

       // Program Mode.
       If q1prms >= 2;
          Select;
          When @@mode = 'D';
             dspMode = *on;
          When @@mode = 'C';
             chgMod  = *on;
          Endsl;
       Endif;

       apShowMenu = *on;

       aMenHeight = 145;

       apFootHght = aMenHeight + 125;

       // Set Menu options
       scMenuChoi = 'Primary Agency Information';
       scMenuValu = 'WTAGTPRI';

       scMenuChoi = %trim(scMenuChoi) + ',Group Assignments';
       scMenuValu = %trim(scMenuValu) + ',WTAGTASN';

       scMenuChoi = %trim(scMenuChoi) + ',Producers';
       scMenuValu = %trim(scMenuValu) + ',WTAGTPROD';

       scMenuChoi = %trim(scMenuChoi) + ',Contacts';
       scMenuValu = %trim(scMenuValu) + ',WTAGTCON';

       scMenuChoi = %trim(scMenuChoi) + ',Service Request';
       scMenuValu = %trim(scMenuValu) + ',WTAGTSRVRT';

       scMenuChoi = %trim(scMenuChoi) + ',Return Funds';
       scMenuValu = %trim(scMenuValu) + ',WTAGTRTFND';

       // Tab Index
       tabIndex = 0;

       If q1prms >= 4;
          If @@TabInd <> ' ';
             tabIndex = %int(@@TabInd);
             If tabIndex < 0 or tabIndex > 2;
                TabIndex = 0;
             Endif;
          Endif;
          @@TabInd = '00000';
       Endif;

       intCo# = GetUserInfo(q1user);
      // Update Action Panel Info (Rrn, Agency Name)
       scActRrn = xxRrn;
       scCompName = %trim(AGname);
       If scCompName = ' ';
          Chain intCo# pco00;
          If COname <> ' ';
             scCompName = COname;
          Endif;
       Endif;

       scCo# = intCo#;

       processMe = *on;

       Return;

      /end-free
     p ProgramStart    e


      // utility procedure definitions
      /COPY SCOPYCOMPI

