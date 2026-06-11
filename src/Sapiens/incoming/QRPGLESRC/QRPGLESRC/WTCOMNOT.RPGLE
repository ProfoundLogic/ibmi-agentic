     H Copyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H**********************************************************************
     H/TITLE WTCOMNOT - Notifications Release
     H DFTNAME(WTCOMNOT)
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO) ACTGRP(*caller)
     H/DEFINE   PROFOUNDUI
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
    *===============================================================*
    *  Created    : 05/28/14                                        *
    *  Programmer : C:65                                            *
    *  Project    : WC2277                                          *
    *  Description: The notification release screen is used to      *
    *               release pending emails to the intended target.  *
    *               If called from the menu then it will display    *
    *               all pending records.  If called from a policy   *
    *               or a claim via toolbar then it will show the    *
    *               pending notifications for that policy/claim.    *
    *                                                               *
    *===============================================================*
    *  Revisions:                                                   *
      *                                                               *
      *  03/21/22 C:333 Project: SY434                                *
      *                 - Update Grids for Sorting Filtering Upgrade  *
      *                                                               *
      *  01/07/16 C:65  Project: WC2401                               *
      *                Change the coding done under WC2254 to call an *
    *                API instead of directly calling the various    *
      *                programs for windows here.                     *
      *                                                               *
      *  09/17/15 C:302 Project: wc2254                               *
      *                 Recompile only                                *
      *                                                               *
    *                                                               *
    * ------------------------------------------------------------- *
    *  NOTE: Please add comments to the top of Revisions.           *
    * ------------------------------------------------------------- *
    *===============================================================*

     fwtcomnot  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflNote:rrnNote)
     f                                     Sfile(sflDoc:rrnDoc)
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)

     FWMUPL     IF   E           K DISK    rename(PUP01:PUP00)
     FWMNEL     IF   E           K DISK    rename(PNE01:PNE00)
     F                                     infds(dbfds)
     FWMNEP     UF   E             DISK    rename(PNE01:PNE0P)
     FWDELP     IF   E             DISK    rename(PEL01:PEL0P)
     FWDELL     IF   E           K DISK    rename(PEL01:PEL00)
     F                                     infds(dbfds2)
     FWMCMP     IF   E             DISK    rename(PCM01:PCM0P)
     FWMCML     IF   E           K DISK    rename(PCM01:PCM00)
     F                                     infds(dbfds3)
     FWDNDL     IF   E           K DISK    Rename(PND01:PND00)
     FWDNDL1    IF   E           K DISK    Rename(PND01:PND01)

     D PSDS          ESDS                  extname(SZQ1P)

     d dbfds           ds
     d  dbrrn                397    400b 0
     d dbfds2          ds
     d  dbrrn2               397    400b 0
     d dbfds3          ds
     d  dbrrn3               397    400b 0

     d systemFlag      s              1    inz(' ')
     D process         s               n   inz(*off)
     D dspMode         s               n   inz(*off)
     D clientID        s              3    inz('000')
     d xxRrn           s              9  0 inz(0)
     d x#Rrn           s              9  0 inz(0)
     d*** begin add ***  01/07/16  ****************************************
     D xxType          s              2    inz('  ')
     d*** end add *****  01/07/16  ****************************************
     D xxCO#           s                   like(CMCO#)
     D xxFnd           s                   like(CMFnd)
     D xxEmp#          s                   like(CMEmp#)
     D xxDiv           s                   like(CMDiv)
     D xxFyr           s                   like(CMFyr)
     D xxCase          s                   like(CMCase)
     D xxCall          s              1    inz(' ')
     D xxStatus        s              1    inz(' ')
     D svFiltStat      s            100    inz(' ')
     D svFiltUser      s             10    inz(' ')
     D svFiltFnd       s              3  0 inz(0)
     D svFiltBgDt      s              8  0 inz(0)
     D svFiltEnDt      s              8  0 inz(0)
     D svFiltType      s              1    inz(' ')
     D svFiltRenw      s              1    inz(' ')
     D userEmail       s            100    inz(' ')
     D beginDate       s              7  0 inz(0)
     D endDate         s              7  0 inz(0)
     D saveRow         s              5  0 inz(0)
     D urlRRN          s              9
     D urlName         s             50
     d xxFold          s            243
     d xxDcid          s             12
     d policyLevel     s              1    inz(' ')
     d y               s              3  0
     d saveNoteRow     s              5  0
     d fold            s              1    DIM(255)
     D*** DATA STRUCTURES FOR DARTS REMARK
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D  QCQTE                 40     40
     d

     d* Proto-types

     d* Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3

     d* System Flag API
     d GetSystemFlag   pr                  extpgm('SA046R')
     d   outSysFlag                   1

     d**** begin add ***  01/07/16  ****************************************
     d PopupWindow     pr                  extpgm('WTPOPUP')
     d outProgram_                   40    const
     d outRRN_                        9  0 const
     d outMode_                       2    const
     d outData_                     500    const
     d**** end add *****  01/07/16  ****************************************

     d* Program Start
     d ProgramStart    pr
     D processMe                       n

     d* Main Processing
     d Main            pr

     d/copy sCopyCompr
     d/copy sprCompany
     d/copy sprDate
     d/copy sprGenPop
     d/copy sprNotice
     d/copy sPrAtrium
     d/copy wPrUsrPrms
     d/copy wPrEmpLr

     C*
     C*   Key List for the User Parameter File
     C*
     C     keyWMUPL      Klist
     C                   Kfld                    Q1USER
     C*
     C     key1WMUPL     Klist
     C                   Kfld                    scFiltUser
     C*
     C*   Key List for the Notification Master File
     C*
     C     keyWMNEL      Klist
     C                   Kfld                    xxCo#
     C*
     C     keyWMNEL_E    Klist
     C                   Kfld                    xxCo#
     C                   Kfld                    xxFnd
     C                   Kfld                    xxEmp#
     C                   Kfld                    xxDiv
     C*
     C     keyWMNEL_C    Klist
     C                   Kfld                    xxCo#
     C                   Kfld                    xxFnd
     C                   Kfld                    xxEmp#
     C                   Kfld                    xxDiv
     C                   Kfld                    xxFyr
     C                   Kfld                    xxCase
     C*
     C*   Key List for the Employer Detail File
     C*
     C     keyWDELL      Klist
     C                   Kfld                    NECo#
     C                   Kfld                    NEFnd
     C                   Kfld                    NEEmp#
     C                   Kfld                    NEDiv
     C                   Kfld                    NEFyr
     C*
     C*   Key List for the Claim Master File
     C*
     C     keyWMCML      Klist
     C                   Kfld                    NECo#
     C                   Kfld                    NEFnd
     C                   Kfld                    NEEmp#
     C                   Kfld                    NEDiv
     C                   Kfld                    NEFyr
     C                   Kfld                    NECase
     C*
     C* Notification Detail File Key List
     C*
     C     keyWDNDL      Klist
     C                   Kfld                    NECo#
     C                   Kfld                    NEFnd
     C                   Kfld                    NEEmp#
     C                   Kfld                    NEDiv
     C                   Kfld                    NEFyr
     C                   Kfld                    NECase
     C                   Kfld                    NEType
     C*
     C     keyWDNDL1     Klist
     C                   Kfld                    NECo#
     C                   Kfld                    NEFnd
     C                   Kfld                    NEEmp#
     C                   Kfld                    NEFyr
     C                   Kfld                    NECase
     C                   Kfld                    NEType
     C*
     C*
     c* Entry of Program.
     c*
     C     *ENTRY        PLIST
     C                   PARM                    @@mode            1            mode
     C                   PARM                    @@rrn             9            wdelp/wmcmp
     C                   PARM                    @@call            1            call (E/C)
     c
      /free

       GetSystemFlag(systemFlag);

       If systemFlag = 'Y';

          ProgramStart(process);

          If process;
             Main();
          Else;
             PopOneBtn('User is not properly setup in User Parameters.':
              'Notifications':'Exit');
          Endif;

       Else;
          PopOneBtn('System is not enabled for Notifications.':
           'Notifications':'Exit');
       Endif;

       *InLr = *On;
      /end-free
      *
       //***************************************************************************//
       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     P ProgramStart    B
     D ProgramStart    PI
     D processMe                       n
      /free

       GetClientID(clientID);

       Chain(n) keyWMUPL PUP00;
       If not %found or UPCO# = *zero;
          processMe = *off;
          Return;
       Endif;

       xxCO# = UPCO#;
       scCO# = UPCO#;

       scFiltStat = 'P';
       scFiltUser = ' ';
       dspClrFilt = *on;
       scFiltType = ' ';
       scFiltRenw = ' ';
       scFiltFnd = 0;
       scFiltBgDt = 0;
       scFiltEnDt = 0;
       disbType = *off;
       disbRenew = *off;
       disbFnd = *off;

       // Relative Record # for specific employer detail or claim detail call.
       If q1prms >= 3;
          xxRrn = %int(@@rrn);
          xxCall = @@call;
          Select;
          When xxCall = 'E';
             Chain xxRRN pel0p;
             xxCo#  = ELCo#;
             xxFnd  = ELFnd;
             xxEmp# = ELEmp#;
             xxDiv  = ELDiv;
             xxFyr  = ELFyr;
             //*** begin add ***  01/07/16  ****************************************
             xxType = 'PS';
             //*** end add *****  01/07/16  ****************************************
             scFiltType = 'P';
             disbType = *on;
             scFiltFnd = ELFnd;
             disbFnd = *on;
          When xxCall = 'C';
             Chain xxRRN pcm0p;
             xxCo#  = CMCo#;
             xxFnd  = CMFnd;
             xxEmp# = CMEmp#;
             xxDiv  = CMDiv;
             xxFyr  = CMFyr;
             xxCase = CMCase;
             //*** begin add ***  01/07/16  ****************************************
             xxType = 'CS';
             //*** end add *****  01/07/16  ****************************************
             scFiltType = 'C';
             disbType = *on;
             scFiltFnd = CMFnd;
             disbFnd = *on;
             disbRenew = *on;
          Endsl;
       Endif;

       TabHeight  = 850;
       APnlHeight = TabHeight + 60;
       inquiry=*off;

       Select;
       When @@mode='D';
          dspMode=*on;
          inquiry=*on;
       Endsl;

       scActMode = @@mode;
       scActRrn = xxRrn;
       scActCltid = clientId;
       scEnv = ' ';
       scInvokeUP = 'Y';

       processMe = *on;
       Return;

      /end-free
     P ProgramStart    E

       //***************************************************************************//
       //===================================================================
       // Main - Main processing
       //===================================================================
     P Main            B
     D Main            PI
      /free

       //*  Load screen fields
       LoadActionBar();
       Exsr LoadRecords;

       //*  Process until exit button is pressed.

       DoU exitNow = *On;

          //*  Process policy detail screen.

          Exsr ExfmtScreens;

          //Perform actions on screen.
          Select;

          //* Exit the program
          When exitNow = *on;
             Leave;

       //*** begin add ***  01/07/16  ****************************************
          When btnShowWin = *on;
             PopupWindow(txtShowWin:scActRRN:xxType:txtWinData);
             btnShowWin = *off;
       //*** end add *****  01/07/16  ****************************************

          //* Refresh the screen
          When BtnRefresh = *on;
             btnRefresh = *off;
             Chain(n) keyWMUPL PUP00;
             xxCO# = UPCO#;
             scCO# = UPCO#;
             LoadActionBar();
             Exsr LoadRecords;

          //* Refresh the screen
          When filter = *on;
             filter = *off;
             Exsr FilterPopup;
             If scFiltStat <> svFiltStat or svFiltFnd <> scFiltFnd or
              svFiltUser <> scFiltUser or svFiltType <> scFiltType or
              svFiltBgDt <> scFiltBgDt or svFiltEnDt <> scFiltEnDt or
              svFiltRenw <> scFiltRenw;
                Exsr LoadRecords;
             Endif;

          //* Refresh the screen
          When clrFilter = *on;
             clrFilter = *off;
             dspClrFilt = *off;
             Clear scFiltStat;
             Clear scFiltUser;
             Clear scFiltBgDt;
             Clear scFiltEnDt;
             Clear scFiltRenw;

             If disbType = *off;
                Clear scFiltType;
             Endif;

             If disbFnd = *off;
                Clear scFiltFnd;
             Endif;

             Exsr LoadRecords;

          Other;

             //*  Check for clicked links on grid.
             Readc sflNote;
             If not %EOF() and not dspMode;

                //*  Release the notification.
                If noteReleas = *on;
                   noteReleas = *off;
                   Exsr ReleaseNote;
                Endif;

                //*  Update the emails.
                If noteUpdate = *on;
                   noteUpdate = *off;
                   Exsr UpdateEmail;
                Endif;

                selNote = *on;

                Update sflNote;

             Endif;

             If saveRow <> rrnNoteRow;
                Chain saveRow sflNote;
                selNote = *off;
                Update sflNote;
                saveRow = rrnNoteRow;
             Endif;

             //* If saved row differs from cursor row reload document sub-grid
             If rrnNoteRow <> saveNoteRow;
                saveNoteRow = rrnNoteRow;
                Exsr LoadDocs;
             Endif;

          Endsl;

       Enddo;

       //***************************************************************************//
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       Write rcdComPnl;
       Exfmt rcdPanel;
       Read rcdComPnl;

       Endsr;

       //***************************************************************************//

       //***************************************************************************//
       //===================================================================
       // Release the Notification.
       //===================================================================
       BegSr ReleaseNote;

       Select;
       When scNtStatus <> 'Pending';
          PopOneBtn('This notification is not in a pending status t+
           o release.':'Release Error':'Enter');
       When scNtMail = *blanks or scNtUSRM = *blanks;
          PopOneBtn('The from or to email for this notification i+
           s not populated.':'Release Error':'Enter');
       Other;
          xxStatus = SendNotice(scNtKeyRRN:scNtMode:scNtType);
          Select;
          When xxStatus = 'S';
             scNtStatus = 'Released';
          Other;
             scNtStatus = 'Error';
          Endsl;
       Endsl;

       Endsr;

       //***************************************************************************//

       //***************************************************************************//
       //===================================================================
       // Update the emails.
       //===================================================================
       BegSr UpdateEmail;

       Chain(n) scNtRRN pne0p;
       If %found;

          scFrmEmail = NEUsrm;
          scToEmail  = NEMail;

          Clear btnCancel;
          Clear btnConfirm;
          Clear erFrmEmail;
          Clear erToEmail;
          Clear msFrmEmail;
          Clear msToEmail;

          Dow btnCancel <> *on and btnConfirm <> *on;
             Exfmt rcdEmail;

             Clear erFrmEmail;
             Clear erToEmail;
             Clear msFrmEmail;
             Clear msToEmail;

             If %scan(';':scFrmEmail) <> 0 or %scan(':':scFrmEmail) <> 0 or
              %scan(',':scFrmEmail) <> 0;
                erFrmEmail = *on;
                msFrmEmail = 'Only one email address is allowed.';
                btnConfirm = *off;
             Endif;

             If %scan(';':scToEmail) <> 0 or %scan(':':scToEmail) <> 0 or
              %scan(',':scToEmail) <> 0;
                erToEmail = *on;
                msToEmail = 'Only one email address is allowed.';
                btnConfirm = *off;
             Endif;

          Enddo;

          If btnConfirm = *on;
             Chain scNtRRN pne0p;
             If %found;
                NEUsrm = scFrmEmail;
                NEMail = scToEmail;
                NEPgm = Q1PGM;
                NEUser = NEUser;
                NEDt   = Dateto7(SystemDate());
                NETime = Timeto6(SystemTime());
                Update pne0p;
             Endif;

             scNtUsrm = scFrmEmail;
             scNtMail = scToEmail;
             scDtlUsrm = scFrmEmail;
             scDtlMail = scToEmail;
          Endif;

       Endif;

       Endsr;

       //***************************************************************************//

       //***************************************************************************//
       //===================================================================
       // Load Notification Grid
       //===================================================================
       BegSr LoadRecords;

       // Clear the subfile.
       dspNote = *off;
       dspNoteCtl = *off;
       dspDoc = *off;
       dspDocCtl = *off;
       clrNote = *on;
       clrDoc = *on;
       Write rcdPanel;
       clrNote = *off;
       clrDoc = *off;
       dspNote = *on;
       dspNoteCtl = *on;
       dspDoc = *on;
       dspDocCtl = *on;
       rrnNote = 0;

       Clear scDtlKey;
       Clear scDtlType;
       Clear scDtlStatu;
       Clear scDtlDate;
       Clear scDtlUSRM;
       Clear scDtlMail;
       Clear scDtlName;
       Clear scDtlFDNM;

       Clear userEmail;
       If scFiltUser <> ' ';
          Chain(n) key1WMUPL PUP00;
          If %found;
             userEmail = UPMail;
          Endif;
          Chain(n) keyWMUPL PUP00;
       Endif;

       // Load the subfile.

       Select;
       When xxCall = ' ';
          Setll keyWMNEL PNE00;
          Reade keyWMNEL PNE00;
       When xxCall = 'E';
          Setll keyWMNEL_E PNE00;
          Reade keyWMNEL_E PNE00;
       When xxCall = 'C';
          Setll keyWMNEL_C PNE00;
          Reade keyWMNEL_C PNE00;
       Endsl;

       Dow not %eof;

          If NECase <> 0;
             Chain(n) keyWMCML pcm00;
          Else;
             Chain(n) keyWDELL pel00;
          Endif;

          If (scFiltStat = ' ' or %scan(NEStat:scFiltStat) <> 0)
           and (scFiltFnd = 0 or scFiltFnd = NEFnd)
           and (scFiltUser = ' ' or userEmail = NEUsrm)
           and (scFiltType = ' ' or (scFiltType = 'P' and NECase = 0)
                or (scFiltType = 'C' and NeCase <> 0))
           and (scFiltBgDt = 0 or (NECase <> 0 and CMAcdt >= beginDate and
                CMAcdt <= endDate) or (NECase = 0 and ELPfdt >= beginDate
                and ELPfdt <= endDate))
           and (scFiltRenw = ' ' or (NECase = 0 and scFiltRenw = ELNWRW)
                or NECase <> 0)
           and NEStat <> ' ';

             If NECase <> 0;
                scNtKey = 'Clm# ' + %editc(NEFnd : 'X') + '-' +
                 %editc(NeCase : 'X') + '-' + %editc(NEFyr : 'X');
                scNtMode =  'C';
                scNtKeyRRN = dbrrn3;
                urlRRN = %editc(dbrrn3:'X');
                urlName = %trim(CMLsn) + ', ' + %trim(CMFsn) +
                 ' ' + %trim(CMMid);
                scNtKeyLnk = CreateURL('STDRVPGM':urlName:urlRRN:@@mode:
                 'WTCLMSUM':'00001':' ');
                scDtlKeyLk = scNtKeyLnk;
             Else;
                scNtKey = 'Pol# ' + %editc(NEFnd : 'X') + '-' +
                 %editc(NEEmp# : 'X') + '-' + %editc(NEFyr : 'X');
                scNtMode =  'E';
                scNtKeyRRN = dbrrn2;
                urlRRN = %editc(dbrrn2:'X');
                urlName = GetEmpFullName(NEEmp#:NEDiv:x#RRN:NEFnd:NECo#:'DBA');
                scNtKeyLnk = CreateURL('STDRVPGM':urlName:urlRRN:@@mode:
                 'WTPOLSUM':'00001':' ');
                scDtlKeyLk = scNtKeyLnk;
             Endif;

             scNtType =  NEType;

             Select;
             When NESTAT = 'P';
                scNtStatus = 'Pending';
             When NESTAT = 'S';
                scNtStatus = 'Sent';
             When NESTAT = 'E';
                scNtStatus = 'Error';
             Other;
                Clear scNtStatus;
             Endsl;

             scNtDate =  CvtToDate(NESTDT);
             scNtUsrM =  NEUsrM;
             scNtMail =  NEMail;
             scNtRRN  = dbrrn;

             rrnNote  =  rrnNote + 1;

             If rrnNote = 1;
                selNote = *On;
                saveRow = rrnNote;
             Else;
                selNote = *Off;
             Endif;

             Write sflNote;

             If rrnNote = 1;
                saveNoteRow = rrnNote;
                Exsr LoadDocs;
             Endif;

          Endif;

          Select;
          When xxCall = ' ';
             Reade keyWMNEL PNE00;
          When xxCall = 'E';
             Reade keyWMNEL_E PNE00;
          When xxCall = 'C';
             Reade keyWMNEL_C PNE00;
          Endsl;
       Enddo;

       //* Position grid at top at all times.
       If rrnNote <> 0;
          rrnNote = 1;
       Endif;

       Endsr;

       //***************************************************************************//
       //===================================================================
       // Load Documents to sub-grid.
       //===================================================================
       BegSr LoadDocs;

       // Clear the subfile.
       dspDoc = *off;
       dspDocCtl = *off;
       dspNote = *off;
       dspNoteCtl = *off;
       clrDoc = *on;
       Write rcdPanel;
       clrDoc = *off;
       dspDoc = *on;
       dspDocCtl = *on;
       dspNote = *on;
       dspNoteCtl = *on;
       rrnDoc = 0;

       // Load the subfile.

       Chain saveNoteRow sflNote;
       If %found;

          Chain(n) scNtRRN pne0p;
          If %found;

             scDtlKey = scNtKey;
             scDtlType = scNtType;
             scDtlStatu = scNtStatus;
             scDtlDate = scNtDate;
             scDtlUSRM = scNtUSRM;
             scDtlMail = scNtMail;
             scDtlName = GetEmpFullName(NEEmp#:NEDiv:x#RRN:NEFnd:NECo#:'DBA');
             scDtlFDNM = GetEmpFullName(NEEmp#:NEDiv:x#RRN:NEFnd:NECo#:'FED');

             Clear qcKey;
             qcCo#  = NECo#;
             qcFnd  = NEFnd;
             qcEmp# = NEEmp#;
             qcDiv  = NEDiv;
             qcQte  = ' ';

             policyLevel = PolicyLevCheck(NEType);

             If policyLevel = 'Y';
                Setll keyWDNDL1 pnd01;
                Reade(n) keyWDNDL1 pnd01;
             Else;
                Setll keyWDNDL pnd00;
                Reade(n) keyWDNDL pnd00;
             Endif;

             Dow not %eof;
                Clear scDocLnch;
                scDtlDocTy = NDKey;
                xxFold = NDFold;
                xxDcid = NDDcid;
                If NDUNCN <> *blanks;
                   Exsr movearr;
                   y = 255;
                   Dow y > 1;
                      If fold(y) = '/';
                         xxfold = %trim(%subst(NDUncn:1:y-1));
                         xxDcid = %trim(%subst(NDUncn:y+1));
                         Leave;
                      Endif;
                      y = y - 1;
                   Enddo;
                Endif;

                scDtlDoc = xxDCID;

                scDocLnch = qcKey + ',' + %editc(NDSeq#:'Z') + ',' +
                 %trim(xxDcid) + ',' + %trim(xxFold) + '^P,' + %trim(Q1User);

                rrnDoc = rrnDoc + 1;
                Write sflDoc;

                If policyLevel = 'Y';
                   Reade(n) keyWDNDL1 pnd01;
                Else;
                   Reade(n) keyWDNDL pnd00;
                Endif;
             Enddo;

          Endif;

       Endif;

       Endsr;

       //***************************************************************************//
       //===================================================================
       // Filter Popup;
       //===================================================================
       BegSr FilterPopup;

       svFiltStat = scFiltStat;
       svFiltUser = scFiltUser;
       svFiltFnd  = scFiltFnd;
       svFiltType = scFiltType;
       svFiltBgDt = scFiltBgDt;
       svFiltEnDt = scFiltEnDt;
       svFiltRenw = scFiltRenw;
       erFiltStat = *off;
       erFiltBgDt = *off;
       erFiltEnDt = *off;
       msFiltBgDt = ' ';
       msFiltEnDt = ' ';

       selFndFilt = 'FNCO# = ' + %char(xxCO#);
       selUsrFilt = 'UPCO# = ' + %char(xxCO#);

       Dou filtCancel = *on or filtApply = *on;

          Exfmt rcdFilter;

          erFiltStat = *off;
          erFiltBgDt = *off;
          erFiltEnDt = *off;
          msFiltBgDt = ' ';
          msFiltEnDt = ' ';

          If scFiltStat <> *blanks and %subst(scFiltStat:1:1) = ' ' and
           filtCancel <> *on;
             erFiltStat = *on;
             filtApply = *off;
          Endif;

          If filtCancel <> *on and (scFiltBgDt <> 0 or scFiltEnDt <> 0);
             If dtCheck(scFiltBgDt) = 0;
                erFiltBgDt = *on;
                msFiltBgDt = 'Invalid Date';
             Else;
                scFiltBgDt = dtCheck(scFiltBgDt);
             Endif;

             If dtCheck(scFiltEnDt) = 0;
                erFiltEnDt = *on;
                msFiltEnDt = 'Invalid Date';
             Else;
                scFiltEnDt = dtCheck(scFiltEnDt);
             Endif;

             If erFiltBgDt <> *on and erFiltEnDt <> *on;
                beginDate =  DtCYMD(scFiltBgDt);
                endDate   =  DtCYMD(scFiltEnDt);
                If endDate < beginDate;
                   erFiltBgDt = *on;
                   msFiltBgDt = 'Dates must be in ascending order.';
                   erFiltEnDt = *on;
                   msFiltEnDt = 'Dates must be in ascending order.';
                Endif;
             Endif;

             If erFiltBgDt = *on or erFiltEnDt = *on;
                filtApply = *off;
             Endif;

          Endif;

       Enddo;

       If FiltCancel = *on;
          scFiltStat = svFiltStat;
          scFiltUser = svFiltUser;
          scFiltFnd  = svFiltFnd;
          scFiltType = svFiltType;
          scFiltBgDt = svFiltBgDt;
          scFiltEnDt = svFiltEnDt;
       Endif;

       If scFiltBgDt <> 0;
          beginDate =  DtCYMD(scFiltBgDt);
          endDate   =  DtCYMD(scFiltEnDt);
       Endif;

       If scFiltStat <> *blanks or scFiltUser <> ' ' or
        (disbFnd = *off and scFiltFnd <> 0) or
        (disbType = *off and scFiltType <> ' ') or
        scFiltBgDt <> 0 or scFiltEnDt <> 0 or scFiltRenw <> ' ';
          dspClrFilt = *on;
       Else;
          dspClrFilt = *off;
       Endif;

       Endsr;

      /end-free
     C**************************************************************************************
     C     movearr       BEGSR
     C                   Movea     NDUncn        fold
     C                   ENDSR                                                  movearr subr
     C**************************************************************************************
     P Main            E

       //***************************************************************************//

      * utility procedure definitions
      /COPY SCOPYCOMPI
