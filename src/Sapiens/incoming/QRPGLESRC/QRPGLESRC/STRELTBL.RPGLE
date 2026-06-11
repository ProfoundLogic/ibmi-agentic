     hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h/title STRELTBL - Relay Messenger Message Table Information
     h dftname(STRELTBL)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h bnddir('STBNDDIR')
     h/define   profoundUI
      *===============================================================*
      *  Created   : 07/27/16                                         *
      *  Programmer: C:136                                            *
      *  Project   : 114WYQ- Create Relay Messenger Message Process   *
      *===============================================================*
      *  Revisions:                                                   *
      *                                                               *
     H*  03/21/22 C:333 Project: SY434                                *
     H*                - Update Grids for Sorting Filtering Upgrade   *
     H*                                                               *
      *===============================================================*
     fstreltbl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflMsgGrid:rrnSfl)
     fstactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdSysTbl)
     fsmcol     if   e           k disk    rename(pco01:pco00)
     fstgtl     if   e           k disk    rename(pgt01:pgt00)
     fstrml     if   e           k disk    rename(prm01:prm00)
     f                                     infds(dbfdsSTRML)
     fstrmp     uf a e             disk    rename(prm01:prm0p)

     d DeleteMessage   pr
     d  rrnSTRMP                      9  0 const
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3
     d LoadMsgGrid     pr
     d Main            pr
     d MsgInfoWdw      pr
     d  rrnSTRMP                      9  0 const options(*nopass)
     d ProgramStart    pr
     d  processMe                      n
     d SaveDetail      pr
     d  inRRN                         9  0

     d/copy sprUsrPrms
     d/copy sPrGenPop
     d/copy sPrDate
     d/copy sPrString
     d/copy sPrModule
     d/copy sCopyCompr
     d/copy sprCompany

     d clientID        s              3
     d error           s               n   inz(*off)
     d process         s               n   inz(*off)

     d psds          esds                  extname(szq1p)
     d dbfdsSTRML      ds
     d  dbrrn                397    400b 0

      /free
       GetClientID(clientID);
       ProgramStart(process);
       If process;
          Main();
       Endif;
       *InLr = *On;
       Return;
      /end-free

      /copy sCopyCompi
       //=========================================
       // Delete Message
       //=========================================
     p DeleteMessage   b
     d DeleteMessage   pi
     d  inRRN                         9  0 const
      /free
       Chain inRRN prm0p;
       If %found;
          Delete prm0p;
       Endif;
      /end-free
     p DeleteMessage   e
       //=========================================
       // Load Message Grid
       //=========================================
     p LoadMsgGrid     b
      /free
       dspSfl = *off;
       dspSflCtl = *off;
       clrSfl = *on;
       Write rcdPanel;
       dspSfl = *on;
       dspSflCtl = *on;
       clrSfl = *off;
       rrnSfl = 0;
       Setll scCO# prm00;
       Reade scCO# prm00;
       Dow Not(%Eof);
          sfGrp     = RMfnd;
          sfMsgID   = RMmsgid;
          sfCampID  = RMcmpid;
          sfMsgDesc = RMdesc;
          Chain (scCO#:'RELY':RMpurs) pgt00;
          If %found;
             sfPurpose = GTdesc;
          Endif;
          rrnSfl   += 1;
          If rrnSfl = 1;
             sfGridSel = *on;
          Else;
             sfGridSel = *off;
          Endif;
          sfMsgRRN = dbRRN;
          Write sflMsgGrid;
       Reade scCO# prm00;
       Enddo;
       rrnSfl = 1;
      /end-free
     p LoadMsgGrid     e
       //=========================================
       // Main - Main processing
       //=========================================
     p Main            b
     d Main            pi
      /free
       LoadActionBar();
       LoadMsgGrid();
       DoU exitNow = *On;
          Exsr ExfmtScreens;
          Select;
          When exitNow = *on;
             Leave;
          When tabClick = *On;
             tabClick = *Off;
          When btnRefresh = *On;
             btnRefresh =*Off;
             scCo# = GetUserInfo(q1user);
             Chain scCo# pco00;
             scCompName = COName;
             LoadActionBar();
             LoadMsgGrid();
          When addMsg = *on;
             MsgInfoWdw();
             LoadMsgGrid();
          Other;
             Readc sflMsgGrid;
             If Not(%Eof);
                If editMsg = *on;
                   editMsg = *off;
                   Update sflMsgGrid;
                   MsgInfoWdw(sfMsgRRN);
                   LoadMsgGrid();
                Endif;
                If deleteMsg = *on;
                   deleteMsg = *off;
                   Update sflMsgGrid;
                   DeleteMessage(sfMsgRRN);
                   LoadMsgGrid();
                Endif;
             Endif;
          Endsl;
       Enddo;

       BegSr ExfmtScreens;
          pnlHeight = 770;
          APnlHeight = pnlHeight + 60;
          Write rcdSysTbl;
          Write rcdPanel;
          Exfmt rcdPanel;
          Read  rcdSysTbl;
       Endsr;

      /end-free
     p Main            e
      *==========================================
      * MsgInfoWdw - Maint window processing
      *==========================================
     p MsgInfoWdw      b
     d MsgInfoWdw      pi
     d  inRRN                         9  0 const options(*nopass)

     d addMsgMode      s               n
     d svFnd           s              3  0
     d svPurpose       s             10
     d xxRRN           s              9  0

      /free
       If %parms >= 1;
          xxRRN = inRRN;
       Endif;
       addMsgMode = *off;
       If xxRRN <> 0;
          Chain(n) xxRRN prm0p;
       Else;
          addMsgMode  = *on;
       Endif;
       Dou addMsgMode = *off;
          If addMsgMode = *on;
             Clear scFnd;
             Clear scMsgId;
             Clear scCampId;
             Clear scMsgDesc;
             Clear scPurpose;
             dspAddNext = *on;
             posCancel  = 430;
             posAddNext = 545;
             dspAddNext = *on;
          Else;
             scFnd      = RMfnd;
             svFnd      = RMfnd;
             scMsgID    = RMMsgId;
             scCampID   = RMCmpId;
             scMsgDesc  = RMdesc;
             scPurpose  = RMpurs;
             svPurpose  = RMpurs;
             Chain (scCO#:'RELY':RMpurs) pgt00;
             If %found;
                sfPurpose = GTdesc;
             Endif;
             posCancel  = 545;
             dspAddNext = *off;
          Endif;
             Exsr ClearErrors;
          Dou btnSave = *on or btnCancel = *on or btnAddNext = *on;
             Dou error = *off or btnCancel = *on;
                Exfmt rcdMsgInfo;
                Exsr ClearErrors;
                If btnCancel = *off;
                   Exsr EditWindow;
                Endif;
             Enddo;
          Enddo;

          If btnSave = *on or btnAddNext = *on;
             SaveDetail(xxRRN);
          Endif;

          If btnCancel  = *on or btnSave = *on;
             addMsgMode = *off;
          Endif;

       Enddo;

       Begsr ClearErrors;
       Clear error;
       Clear errBlnkMsg;
       Clear msBlankMsg;
       Clear errBlnkDsc;
       Clear msBlankDsc;
       Clear errBlnkPrp;
       Clear msBlankPrp;
       Clear errDupeRcd;
       Clear msgDupeRcd;
       Endsr;

       Begsr EditWindow;
       If scMsgId = *blanks;
          msBlankMsg = 'This field is required.';
          errBlnkMsg = *on;
          error = *on;
       Endif;
       If scMsgDesc = *blanks;
          msBlankDsc = 'This field is required.';
          errBlnkDsc = *on;
          error = *on;
       Endif;
       If scPurpose = *blanks;
          msBlankPrp = 'This field is required.';
          errBlnkPrp = *on;
          error = *on;
       Endif;
       If xxRRN = 0;
          Chain (scCO#:scFnd:scPurpose) prm00;
          If %found;
             msgDupeRcd = 'Multiple records with the same purpose -
              cannot be assigned to the same group.';
             errDupeRcd = *on;
          error = *on;
          Endif;
       Endif;
       If xxRRN <> 0 and (svFnd <> scFnd or
        svPurpose <> scPurpose);
          Chain (scCO#:scFnd:scPurpose) prm00;
          If %found;
             msgDupeRcd = 'Multiple records with the same purpose -
              cannot be assigned to the same group.';
             errDupeRcd = *on;
          error = *on;
          Endif;
       Endif;
       Endsr;

       BegSr ExfmtScreens;
          LoadMsgGrid();
          pnlHeight = 770;
          APnlHeight = pnlHeight + 60;
          Write rcdSysTbl;
          Write rcdPanel;
          Exfmt rcdPanel;
          Read  rcdSysTbl;
       Endsr;

      /end-free

     p MsgInfoWdw      e
       //=========================================
       // ProgramStart - Initial Program tasks
       //=========================================
     p ProgramStart    b
     d ProgramStart    pi
     d processMe                       n
      /free
       apShowMenu = *off;
       // scMenuChoi = ' ';
       scMenuValu = ' ';
       aMenHeight = 50;
       apFootHght = aMenHeight + 125;
       processMe = *on;
       scCO# = GetUserInfo(q1user);
       Chain scCO# pco00;
       scCompName = COName;
      /end-free
     p ProgramStart    e
       //=========================================
       // Save Detail -
       //=========================================
     p SaveDetail      b
     d SaveDetail      pi
     d  inRRN                         9  0
      /free
       If inRRN <> 0;
          Chain inRRN prm0p;
       Endif;
       RMco#   = scCO#;
       RMfnd   = scFnd;
       RMmsgId = scMsgId;
       RMcmpId = scCampID;
       RMdesc  = scMsgDesc;
       RMpurs  = scPurpose;
       If inRRN = 0;
          Exsr AddMaintInfo;
          Write prm0p;
       Else;
          Exsr UpdMaintInfo;
          Update prm0p;
       Endif;

       Begsr UpdMaintInfo;
       RMuser = Q1user;
       RMpgm  = Q1pgm;
       RMtime = TimeTo6(SystemTime);
       RMdate = DateTo7(SystemDate);
       Endsr;

       Begsr AddMaintInfo;
       RMiusr = Q1user;
       RMipgm = Q1pgm;
       RMitim = TimeTo6(SystemTime);
       RMidat = DateTo7(SystemDate);
       RMuser = RMiusr;
       RMpgm  = RMipgm;
       RMtime = RMitim;
       RMdate = RMidat;
       Endsr;
      /end-free

     p SaveDetail      e
