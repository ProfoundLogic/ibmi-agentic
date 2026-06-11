     Hcopyright('(c) 2020 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H**********************************************************************
     H/TITLE FTPBDTL - Provider Bills Detail
     H DFTNAME(FTPBDTL)
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H/DEFINE   PROFOUNDUI
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     h**********************************************************************
      *===============================================================*
      *  Created   : 05/22/19                                         *
      *  Programmer: C:212                                            *
      *  Project   : MC430                                            *
      *===============================================================*
      *  Revisions:                                                   *
      *                                                               *
     H*  06/09/20 C:295  Project : SY420
     H*                  Mask SSN
     H*
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: Please add comments to the top of Revisions.           *
      * ------------------------------------------------------------- *
      *===============================================================*

     fftpbdtl   cf   e             workstn extfile('FTPBDTL')
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflStsHist:rrnStsHist)
     fstactpnl  cf   e             workstn handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdMCPnl)
     ffaesl1    if   e           k disk    rename(pes01:pes01)
     f                                     infds(dbfds1)
     ffaeop     if   e             disk    rename(peo01:peo0p)
     ffaeolg    if   e           k disk    rename(peo01:peo01)
     ffmcml1    if   e           k disk    rename(pcm01:pcm01)
     ffmfnl     if   e           k disk    rename(pfn01:pfn01)
     ffaeal1    if   e           k disk    rename(pea01:pea01)
     ffmpdl     if   e           k disk    rename(ppd01:ppd01)
     fststl1    if   e           k disk    rename(pst01:pst01)

     d* procedure definitions
     d*

     d RunCommand      pr                  extPgm('QCMDEXC')
     d inCmd                       2750    const
     d inCmdLen                      15  5 const

     d* data structures
     d*
     d psds          esds                  extname(szq1p)

     d dbfds1          ds
     d  rrnfaesp             397    400b 0

     d* array definitions
     d*

     d* standalone variables
     d*

     D inCmd           s           2750
     D inCmdLen        s             15  5

     d addMode         s               n   inz(*off)
     d chgMode         s               n   inz(*off)
     d dspMode         s               n   inz(*off)
     d mode            s              1    inz
     d process         s               n   inz(*off)
     d xxRrn           s              9  0 inz
     d @@mode          s              1

     d* Clear Subfiles
     d clrSubFiles     pr

     d* load rightcoding grid
     d loadStsHist     pr

     d* main Processing
     d main            pr

     d* program Start
     d programStart    pr
     d processMe                       n

     d/copy SCopyMCPr
      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************

     c* Entry of Program.
     c     *Entry        Plist
     c                   Parm                    @@rrn             9            faeop
     c                   Parm                    @@mode                         mode
     c                   Parm                    @@TabInd          5            Tab index

      /free

       // main program processing
       If %parms >= 1;
          xxRrn = %int(@@rrn);
       Else;
          xxRrn = 1;
       Endif;

       // Program Mode
       If %parms >= 2;
          mode = @@mode;
       Endif;

       // Tab Index
       If %parms >= 3;
          tabindex = %int(@@tabind);
       Else;
          tabindex = 0;
       Endif;

       Select;
       When mode = 'D';
          dspMode = *On;
       When mode = 'A';
          addMode = *On;
       When mode = 'C';
          chgMode= *On;
       Other;
          dspMode = *On;
       Endsl;

       ProgramStart(process);

       If process;
          Main();
       Endif;

       *inlr = *on;
       Return;

      /end-free
       //===================================================================
       // ClrSubFiles - Clears SubFiles
       //===================================================================
     P ClrSubFiles     B
     D ClrSubFiles     PI
      /Free

       rrnStsHist = 0;
       clrStsHSub = *On;
       dspStsHSub = *Off;
       dspStsHCtl = *Off;
       Write rcdPBSts;

      /End-Free
     P ClrSubfiles     E
       //===================================================================
       // Load PB Status History Information
       //===================================================================
     P LoadStsHist     B
     d LoadStsHist     PI
      /free

       // set subfile indicators
       clrStsHSub = *Off;
       dspStsHSub = *On;
       dspStsHCtl = *On;

       // read file and load the subfile
       Setll (EOco#:EOsub#:EOfnd:EOfyr:EOcase:EOeob#) pes01;
       ReadE (EOco#:EOsub#:EOfnd:EOfyr:EOcase:EOeob#) pes01;
       dow not %eof(faesl1);
          clear sflStsHist;

          scSHDate   = %date(ESidt:*cymd);
          scSHTime   = %int(%subst(%EditC(ESitim:'X'):1:6));
          scSHStat   = ESblst;
          scSHType   = ESstcd;
          scSHReason = %Trim(ESreas);
          scSHUser   = ESiusr;
          scSHRcn    = ESrcn;
          If ESltr# <> 0;
             scSHLtr = 'Y';
          Else;
             scSHLtr = 'N';
          EndIf;

          scSHRRN# = rrnfaesp;
          rrnStsHist = rrnStsHist + 1;

          Write sflStsHist;

          ReadE (EOco#:EOsub#:EOfnd:EOfyr:EOcase:EOeob#) pes01;
       Enddo;

       rrnStsHist = 1;

      /end-free
     P LoadStsHist     E
      //===================================================================
       // Main - Main processing
      //===================================================================
     P Main            B
     d Main            PI

      /free

       // Process until exit button is pressed.
       Dou ExitNow = *On;

          // If User pressed Cancel - F12, leave
          If CancelNow = *On;
             Leave;
          EndIf;

          // Load the MC action panel (this proc is defined through SCOPYMCPI)
          LoadMCActPan();

          // Clear Sub-files
          ClrSubfiles();

          // Load PB status history information
          LoadStsHist();

          // Process PB status history screen
          Exsr ExfmtScreens;

          Select;
          // do nothing - catch tab click event
          When tabClick = *on;
             tabClick = *off;

          When btnRefresh = *On;
             btnRefresh = *off;

          When btnSave = *On;
             btnSave = *off;

          // Other options processing
          Other;

          Endsl;

       Enddo;
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       Select;

       When tabIndex = 0;
          tabHeight = 600;
          APnlHeight = tabHeight + 50;
          tabHeader = 'Bill Status History';
          Write rcdMCPnl;
          Write rcdPBDtl;
          Exfmt rcdPBSts;
          Read rcdPBDtl;
          Read rcdMCPnl;
       EndSl;

       Endsr;

      /end-free
     P Main            E
       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     P ProgramStart    B
     d ProgramStart    PI
     d processMe                       n

      /free

       // If Rrn is passed, verify
       If xxRrn > *zeros;

         // Chain to EOB file
         Chain xxRrn peo0p;

         // Process only if valid record found.
         If not %found(faeop);
            processme = *Off;
            Return;
         Endif;

       Endif;

       processMe = *on;

       Return;

      /end-free
     P ProgramStart    E
       //===================================================================

     P/copy SCopyMCPi
       //===================================================================
