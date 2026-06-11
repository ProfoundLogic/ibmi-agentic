     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     H**********************************************************************
     H/TITLE WTGRPMEND - Midterm Endorsement Master Configuration table
     H DFTNAME(WTGRPMEND)
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO) ACTGRP(*CALLER)
     H/DEFINE   PROFOUNDUI
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
      *===============================================================*
     * Program Description:                                          *
     *   This program will display a list of Midterm Endorsements    *
     *   for a selected group.  Each Endorsement can be accessed     *
     *   for update, display or selection depending on the calling   *
     *   program.                                                    *
     *                                                               *
      *  Created   : 06/16/18                                         *
      *  Programmer: C:267                                            *
      *  Project   : WC2584  clone from WTPOLCON                      *
      *===============================================================*
      *  Revisions:                                                   *
      *                                                               *
      *  06/09/21 C:300 Project: WC2196, Support: 96185               *
      *                 Subfile positions to first record in the grid *
      *                 When "Edit" link is pressed and press "Cancel"*
      *                 or "Save" on the Edit screen                  *
      *                                                               *
      *  05/10/21 C:300 Project: WC2196                               *
      *                 Check If record has been updated by another   *
      *                 for EDIT Mode                                 *
      *                 Display Record lock message, ask Confirmation *
      *                 and Delete status window for DELETE mode      *
      *                                                               *
      *  03/25/21 C:300 Project: WC2196                               *
      *                 Load this screen on Endorsement/Policy Form   *
      *                 tab of WTGRPPOLCG, and added visibility       *
      *                 indicator on TabPanel & Cancel Button of      *
      *                 RcdMENDMst record format                      *
      *                                                               *
      *  02/02/21 C:136 Project: WC2757                               *
      *                 Disable state specific functionality for      *
      *                 waivers.                                      *
      *                                                               *
      *  01/20/20 C:298 Project: WC2703                               *
      *                 Mid-term Endorsement Configuration for        *
      *                 Printing of Schedule.                         *
      *                                                               *
      *  06/17/19 C:295 Project: WC2584                               *
      *                 Correct the checkbox-state reporting division *
      *                 to handle checked unchecked issue,tabbing     *
      *                 correcton, Processing program feild correction*
      *                                                               *
      *  06/03/19 C:291 Project: WC2584                               *
      *                 Add Typeahead on State specific From/To form. *
      *                 Move "Copy All" button above first grid.      *
      *                 Add "Copy To" group on the Copy screen,       *
      *                 Allow to copy single midterm endorsement      *
      *                 record to another group and allow to copy any *
      *                 updates to a new group.                       *
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: Please add comments to the top of Revisions.           *
      * ------------------------------------------------------------- *
      *===============================================================*

     fwtgrpmend cf   e             workstn extfile('WTGRPMEND')
     f                                     HANDLER('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflGrpMEND:rrnGrpMED)
     f                                     Sfile(sflStaMED1:rrnStaMED1)
     f                                     Sfile(sflStaMED2:rrnStaMED2)
      *
     ** Midterm Endorsement file
     FWTMYP     UF A E             DISK    RENAME(PMY01:PMY0P)
      *** Beg Add ***** 05/10/21 *************************
     F                                     INFDS(MYPDS)
      *** End Add ***** 05/10/21 *************************
      *
     FWTMYL     IF   E           K DISK    INFDS(MYLDS)
      *
     FWTMYL1    IF   E           K DISK    INFDS(MYL1DS)
     F                                     RENAME(PMY01:PMYL1)
      *
     ** State Specific Midterm Endorsement file
     FWTM2P     UF A E             DISK    RENAME(PM201:PM20P)
      *
     FWTM2L     IF   E           K DISK    INFDS(M2LDS)
      *
     FWTM2L1    IF   E           K DISK    RENAME(PM201:PM2L1)

     ** Group Master File
     FWMFNP     IF   E             DISK    RENAME(PFN01:PFN0P)

     FWMFNL     IF   E           K DISK

     ** State table
     FSTSTL3    IF   E           K DISK

     FWTENL     IF   E           K DISK    rename(pen01:pen00)

      //***/copy sCopyPolPr

     d psds          esds                  extname(szq1p)

      *** Beg Add ***** 05/10/21 *************************
     d MYPDS           ds
     d  rrnWTMYP             397    400b 0
      *** End Add ***** 05/10/21 *************************
     d MYLDS           ds
     d  rrnWTMYL             397    400b 0
     d MYL1DS          ds
     d  rrnWTMYL1            397    400b 0
     d M2LDS           ds
     d  rrnWTM2L             397    400b 0

      * Add Midterm Endorsement State Specific forms arrays
     D arrME2Co#       S              3p 0 dim(99)
     D arrME2Grp       S              3p 0 dim(99)
     D arrME2Sort      S              3p 0 dim(99)
     D arrME2St        S              2    dim(99)
     D arrME2Form      S             10    dim(99)
     D arrME2Sts       S              1    dim(99)
     D arrME2Rrn       S              9  0 dim(99)

     D arrIdx          S              3  0
     D xxIndex         S              3  0
     D y               S              3  0

     D MENDTxt         C                   CONST('Midterm Endorsements')
     D CpyATxt         C                   CONST('Copy All')

     D StateTxt        C                   CONST('State Specific')
     D CpyStaTxt       C                   CONST('Copy')
     D AddStaTxt       C                   CONST('Add')
     D EdtStaTxt       C                   CONST('Edit')

     d MENDTitle       s            100    inz
     d StateTitle      s            100    inz
     d tlCpyAllME      s            100    inz
     d ##Co#           s                   like(FNco#)
     d ##fnd           s                   like(FNfnd)
     d isMode          s              1    inz
     d isStaMode       s              1    inz
     d isStateGrid     s              1    inz
     d ##Rrn           s                    like(rrnWTMYL)
     d svFnd           s                   like(MYfnd)
     d svSort          s                   like(MYSort)
      *** Beg Add ***** 06/03/19 *************************
     d svState         s                   like(M2St)
     d flgWaiver       s              1    inz(' ')
     d flgFormExist    s              1    inz('N')
      *** End Add ***** 06/03/19 *************************
     d FlgWrtSSRec     s              1    inz

     d doLoad          s               n   inz(*On)
     d FirstTime       s               n   inz(*on)
     d strSt           s                   like(m2st)
     d strState        s                   like(stkey)
     d intSort         s                   like(mysort)
     d intGroup        s                   like(myfnd)
     d SavRrnGrpMED    s                   like(CrrnGrpMED)

     d addMode         s               n   inz(*off)
     d chgMode         s               n   inz(*off)
     d dspMode         s               n   inz(*off)
     D qteMode         s               n   inz(*off)
     D quoteMode       s              1    inz(' ')
     d isValid         s               n   inz(*off)
     d clientID        s              3    inz('000')
     d mode            s              1
     d process         s               n   inz(*off)
     d xxRrn           s              9  0
      **@@mode          s              1
      **@@next          s             10
     d flgRcdUpdated   s              1
     d SVdt            s              7  0
     d SVtime          s              6  0
      ***** Beg Add ***** 05/10/21 ****************************
     d isexit          s               n   Inz(*off)
     d isDelResp       s               n   inz(*off)
      ***** End Add ***** 05/10/21 ****************************

      ***** Beg Add ***** 03/25/21 ****************************
     d Callpgm         s              1
      ***** End Add ***** 03/25/21 ****************************

     d* Add Midterm Endorsements
     d AddMEND         pr

     d* Add State Specific Midterm Endorsements
     d AddState        pr

      ***** Beg Add ***** 03/25/21 ****************************
     d ChkStack        pr                  extpgm('SRCHKPGM')
     d  outPgmName                   10    const
     d  isFound                       1
      ***** End Add ***** 03/25/21 ****************************

     d* Clear screen fields - ADD mode
     d ClrScrnFlds     pr

     d* Clear State Specific array fields
     d ClrStaArray     pr

     d* CopyALL Midterm Endorsements
     d CopyAllMEND     pr

     d* CopyALL Midterm Endorsement States
     d CopyAllMESta    pr

     d* Copy Midterm Endorsement States
     d CopyMendSta     pr

     d* Copy State Specific Midterm Endorsements
     d CopyState       pr

     d* Clear all Error indicators
     d ClearErrors     pr

     d* Clear Midterm Endorsement master subfile
     d ClrSubFilMst    pr

     d* Clear State Specific Midterm Endorsement subfile
     d ClrSubFilSta    pr

     d* Add State Specific Midterm Endorsements
     d EditAddState    pr

     d* Delete Midterm Endorsement
     d DeleteMEND      pr

     d* Delete Midterm Endorsement State Specific
     d*DeleteMENDSta   pr

     d* get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d outCltID                       3

     d* load Midterm Endorsement Detail information below grid
     d LoadGrpDetail   pr

     d* load Midterm Endorsement Master Configuration grid for group
     d LoadGrpMEND     pr

     d* load Selection fields
     d LoadSelection   pr

     d* load State Specific Midterm Endorsement grid - ADD screen
     d LoadStaArray    pr

     d* load State Specific Midterm Endorsement grid
     d LoadStaMEND     pr

     d* load State Specific Midterm Endorsement grid
     d LoadStaMEND2    pr

     d* main Processing
     d Main            pr

     d* Move database record to screen
     d MoveDbSc        pr

     d* Move State Specific database record to screen
     d MoveStaDbSc     pr

     d* process Add/Edit for Midterm Endorsements
     d ProcessAddEdit  pr
     d isMode                         1A

     d* process Add/Edit for State Specific Midterm Endorsements
     d ProcStaAddEdit  pr
     d isStaMode                      1A

     d* process Copy ALL Midterm Endorsements
     d ProcessCopyALL  pr

     d* program Start
     d ProgramStart    pr
     d processMe                       n

     d* Save state info to arrays
     d SaveState       pr

     d* Process screen actions
     d ScreenActions   pr

     d* Validate Add/Edit screen
     d Validate        pr
     d  isMode                        1A

     d* Validate State Specific Add/Edit screen
     d ValidSta        pr
     d  isStaMode                     1A

     d/copy sPrAtrium
     d/copy sPrDate
     d/copy sPrGenPop
     d/copy sPrString
     d/copy sPrGenErr

     d/copy wPrGroup
     d/copy wPrRateSet
     d/copy wPrUsrPrms

     c* Midterm Endorsement Key List
     c     keyWTMYP      klist
     c                   kfld                    ##Co#
     c                   kfld                    ##fnd

     c     keyWTMYL      klist
     c                   kfld                    ##Co#
     c                   kfld                    ##fnd

     c     keyWTMY2      klist
     c                   kfld                    ##Co#
     c                   kfld                    ##fnd
     c                   kfld                    intSort

     c* State Specific Midterm Endorsement Key List
     c     keyWTM2L      klist
     c                   kfld                    ##Co#
     c                   kfld                    ##fnd

     c     keyWTM22      klist
     c                   kfld                    ##Co#
     c                   kfld                    ##fnd
     c                   kfld                    intSort

     c     keyWTM23      klist
     c                   kfld                    ##Co#
     c                   kfld                    ##fnd
     c                   kfld                    intSort
     c                   kfld                    strSt

     c     keyWTM24      klist
     c                   kfld                    ##Co#
     c                   kfld                    ##fnd
     c                   kfld                    strSt


     c* Entry of Program.
     c     *Entry        Plist
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@NEXT           10

      /free

       // main program processing
       xxRrn = @@rrn;

       // Program Mode.
       mode = @@mode;

       If mode = *blank;
          mode = 'C';
       Endif;

       Select;
       // Display mode
       When mode  = 'D';
          dspMode = *On;
          dspVisb = *Off;
          //Set output status field to visible
          //dspModeS = *Off;
          dspVisbS = *On;
       Other;
          dspMode = *Off;
          dspVisb = *On;
          //Set output status field to disabled
          //dspModeS = *On;
          dspVisbS = *Off;
       Endsl;

       GetClientID(ClientID);

       ProgramStart(process);

       If process;
          Dou btnCancel = *on;
             Main();
          EndDo;
       Endif;

       Unlock WTMYP;
       *INKL = '1';
       *InLr = *On;
       Return;

      /end-free
       //===================================================================
       // AddMEND  - Add Midterm Endorsements
       //===================================================================
     P AddMEND         B
     d AddMEND         PI
     c
      /free

       MYFND  = scGroup#;
       MYDESC = %trim(scAddMenDs);
       MYSORT = scAddSort;
       MYSRFL = scAddSRDiv;
       MYPGM1 = scAddPgm;
       MYDSC1 = %trim(scAddForm);
       MYSTAT = scStatus;
       //*** Beg Add ***** 06/03/19 *********************
       MYWavr = scWaiver;
       //*** End Add ***** 06/03/19 *********************
       //***** Beg Add ***** 01/20/20 *****************************************
       // The below list of program names will also be present in AJAX program
       // WTPOLMEN01 and any change to this program to add a new entry
       // should be duplicated there as well.

       If scAddPgm = 'W0051R11' or scAddPgm = 'W0051R12' or
        scAddPgm = 'W0051R13' or scAddPgm = 'W0051R15' or
        scAddPgm = 'W0051R19' or scAddPgm = 'W0051R20' or
        scAddPgm = 'W0051R21' or scAddPgm = 'W0051R40' or
        scAddPgm = 'W0051R55' or scAddPgm = 'W0051R65' or
        scAddPgm = 'ADDNAMEINS';
          MYSCHD = scSuppSch;
       Else;
          MYSCHD = ' ';
       Endif;
       //***** End Add ***** 01/20/20 *****************************************

       //*** Beg Chg ***** 06/03/19 *********************
       //If isMode = 'A' or isMode = 'C';
       //   MYCO#  = ##co#;
       //   MYUSER = q1user;
       //   MYPGM  = q1pgm;
       //   MYDT   = Dateto7(SystemDate());
       //   MYTIME = Timeto6(SystemTime());
       //   Write pmy0p;
       //Else;
       //   MYUSER = q1user;
       //   MYPGM  = q1pgm;
       //   MYDT   = Dateto7(SystemDate());
       //   MYTIME = Timeto6(SystemTime());
       //   Update pmy0p;
       //Endif;
       Select;
       When isMode = 'A';
          MYCO#  = ##co#;
          MYFND  = scGroup#;
          MYUSER = q1user;
          MYPGM  = q1pgm;
          MYDT   = Dateto7(SystemDate());
          MYTIME = Timeto6(SystemTime());
          Write pmy0p;
       When isMode = 'E';
          MYUSER = q1user;
          MYPGM  = q1pgm;
          MYDT   = Dateto7(SystemDate());
          MYTIME = Timeto6(SystemTime());
          //*** Beg Add ***** 05/10/21 ***************************
          SVdt   = MYdt;
          SVtime = MYtime;
          //*** End Add ***** 05/10/21 ***************************
          Update pmy0p;
       When isMode = 'C';
          Chain (##Co#:scToGroup#:scAddSort) pmy01;     //Check if midterm End exist in To group
          If not %found(WTMYL);                         //Only add midterm End record if not found
             MYCO#  = ##co#;
             MYFND  = scToGroup#;
             MYUSER = q1user;
             MYPGM  = q1pgm;
             MYDT   = Dateto7(SystemDate());
             MYTIME = Timeto6(SystemTime());
             Write pmy0p;
          Endif;
       Endsl;
       //*** End Chg ***** 06/03/19 *********************
       Unlock WTMYP;

      /end-free
     P AddMEND         E
       //===================================================================
       // AddState - Add State Specific Midterm Endorsements to database
       //===================================================================
     P AddState        B
     d AddState        PI
     c
      /free
       Select;
       // State form can only be added when a Midterm Endorsement is added
       // the following process is for State grid 2
       When isStateGrid  = '2';
          ##Rrn = 1;
          Chain ##Rrn sflStaMED2;

          Dow %found;
             If scME2State = *blanks;
                Leave;
             EndIf;

             // Check if State record already exists
             Chain (scME2Co#:scME2Grp:scME2Sort:scME2State) pm201;
             If not %found(WTM2L);

                M2Co#  = scME2Co#;
                M2Fnd  = scME2Grp;
                M2Sort = scME2Sort;
                M2St   = scME2State;

                M2Form = %trim(scME2Form);
                M2STAT = 'A';
                M2USER = q1user;
                M2PGM  = q1pgm;
                M2DT   = Dateto7(SystemDate());
                M2TIME = Timeto6(SystemTime());

                Write pm20p;
             Endif;

             ##Rrn = ##Rrn + 1;
             Chain ##Rrn sflStaMED2;
          EndDo;
          Unlock WTM2P;
       EndSl;

      /end-free
     P AddState        E
       //===================================================================
       // ClrScrnFlds - clear screen fields - State Specific ADD mode
       //===================================================================
     P ClrScrnFlds     B
     d ClrScrnFlds     PI
     c
      /free

       Clear scAddSort;
       Clear scAddMenDs;
       Clear scAddPgm;
       Clear scAddForm;
       //*** Beg Add ***** 06/03/19 *********************
       Clear scWaiver;
       //*** End Add ***** 06/03/19 *********************

      /end-free
     P ClrScrnFlds     E
       //===================================================================
       // ClrStaArray - clear array fields - State Specific ADD mode
       //===================================================================
     P ClrStaArray     B
     d ClrStaArray     PI
     c
      /free

       y = 1;
       DoW y <= 99;
          Clear  arrME2Co#(y);
          Clear  arrME2Grp(y);
          Clear  arrME2Sort(y);
          Clear  arrME2St(y);
          Clear  arrME2Form(y);
          Clear  arrME2Sts(y);
          y = y + 1;
       Enddo;

      /end-free
     P ClrStaArray     E
       //===================================================================
       // CopyAllMEND- Copy all Midterm Endorsements to another group or fund
       //===================================================================
     P CopyAllMEND     B
     d CopyAllMEND     PI
     c
      /free
       SetLL (##Co#:##fnd) pmy01;
       ReadE (##Co#:##fnd) pmy01;

       Dow not %eof(WTMYL);
          If MYFND <> ##fnd or MYCO# <> ##Co#;
             Leave;
           Endif;

          MYFND  = scToGrp;
          MYSTAT = 'A';
          MYUSER = q1user;
          MYPGM  = q1pgm;
          MYDT   = Dateto7(SystemDate());
          MYTIME = Timeto6(SystemTime());

          //---------
          Write pmy0p;
          //---------

          ReadE (##Co#:##fnd) pmy01;

       EndDo;

       Unlock WTMYP;

      /end-free
     P CopyAllMEND     E
       //===================================================================
       // CopyAllMESta- Copy Midterm Endorsements state records
       //===================================================================
     P CopyAllMESta    B
     d CopyAllMESta    PI
     c
      /free
          //*----------------------------------------------------------
          // copy ALL state specific records for ALL endorsement forms
          //*----------------------------------------------------------
          SetLL (##Co#:##fnd) pm201;
          ReadE (##Co#:##fnd) pm201;

          Dow not %eof;
             If M2FND <> ##fnd or M2CO# <> ##Co#;
                Leave;
             EndIf;

             M2FND  = scToGrp;
             M2STAT = 'A';
             M2USER = q1user;
             M2PGM  = q1pgm;
             M2DT   = Dateto7(SystemDate());
             M2TIME = Timeto6(SystemTime());

             //---------
             Write pm20p;
             //---------

             ReadE (##Co#:##fnd) pm201;
          EndDo;

          Unlock WTM2P;

      /end-free
     P CopyAllMESta    E
       //===================================================================
       // CopyMENDSta- Copy Midterm Endorsements state records
       //===================================================================
     P CopyMENDSta     B
     d CopyMENDSta     PI
     c
      /free
          //*----------------------------------------------------------
          // copy ALL state specific records for ALL endorsement forms
          //*----------------------------------------------------------
          SetLL (##Co#:svfnd:svSort) pm201;
          ReadE (##Co#:svfnd:svSort) pm201;

          Dow not %eof;
             If M2CO# <> ##Co# or M2FND <> svfnd or M2SORT <> svSort;
                Leave;
             EndIf;

             //*** Beg Chg ***** 06/03/19 **********************
             //M2FND  = scGroup#;
             svState = M2St;
             Chain (M2Co#:scToGroup#:M2Sort:M2St) pm201;
             If not %found(WTM2L);
                M2FND  = scToGroup#;
             //*** End Chg ***** 06/03/19 **********************
                M2SORT = scAddSort;

                M2STAT = 'A';
                M2USER = q1user;
                M2PGM  = q1pgm;
                M2DT   = Dateto7(SystemDate());
                M2TIME = Timeto6(SystemTime());

                //---------
                Write pm20p;
                //---------
             //*** Beg Add ***** 06/03/19 **********************
             Endif;
             //*** End Add ***** 06/03/19 **********************

             SetGt (##Co#:svfnd:svSort:svState) pm201;
             ReadE (##Co#:svfnd:svSort) pm201;
          EndDo;

          Unlock WTM2P;

      /end-free
     P CopyMENDSta     E
       //===================================================================
       // CopyState - Copy State Specific Midterm Endorsements
       //===================================================================
     P CopyState       B
     d CopyState       PI
     c
      /free

       M2St   = scTSSState;
       M2Form = %trim(scTSSForm);

       M2co#  = ##co#;
       M2fnd  = scSSGrp;
       M2Sort = scSSSort;
       M2STAT = 'A';
       M2USER = q1user;
       M2PGM  = q1pgm;
       M2DT   = Dateto7(SystemDate());
       M2TIME = Timeto6(SystemTime());

       Write pm20p;

       Unlock WTM2P;

      /end-free
     P CopyState       E
       //===================================================================
       // Clear all errors indicators
       //===================================================================
     P ClearErrors     B
     D ClearErrors     PI
      /free

       erSSSta  = *off;
       erSSForm  = *off;
       erTSSSta  = *off;
       erTSSForm  = *off;
       Clear msSSSta;
       Clear msSSForm;
       Clear msTSSSta;
       Clear msTSSForm;

       erGroup# = *off;
       erSRDiv = *off;
       erSort  = *off;
       erDesc  = *off;
       erPgm   = *off;
       erForm  = *off;
       Clear msGroup#;
       Clear msSort;
       Clear msSRDiv;
       Clear msDesc;
       Clear msPgm;
       Clear msForm;
       //*** Beg Add ***** 06/03/19 **************************
       erToGroup# = *off;
       Clear msToGroup#;
       //*** End Add ***** 06/03/19 **************************

       erCGroup# = *off;
       Clear msCGroup#;

      /end-free
     P ClearErrors     E
       //===================================================================
       // ClrSubFilMst - Clears SubFile - Midterm Endorsement Master
       //===================================================================
     P ClrSubFilMst    B
     D ClrSubFilMst    PI
      /Free

       rrnGrpMED = 0;
       clrGrpMED = *On;
       dspMEDSub = *Off;
       dspMEDCtl = *Off;

       rrnStaMED1 = 0;
       clrStaMED1 = *On;
       dspStaSub1 = *Off;
       dspStaCtl1 = *Off;

       //***************//
       Write RcdMENDMst;
       //***************//

       clrGrpMed = *Off;
       dspMEDSub = *On;
       dspMEDCtl = *On;

       clrStaMED1 = *Off;
       dspStaSub1 = *On;
       dspStaCtl1 = *On;

      /End-Free
     P ClrSubfilMst    E
       //===================================================================
       // ClrSubFilSta - Clears State Specific subfile
       //===================================================================
     P ClrSubFilSta    B
     D ClrSubFilSta    PI
      /Free

       Select;
       When isStateGrid = '1';
          rrnStaMED1 = 0;
          clrStaMED1 = *On;
          dspStaSub1 = *Off;
          dspStaCtl1 = *Off;

          //SavRrnGrpMED = CrrnGrpMED;

          //***************//
          Write RcdMENDMst;
          //***************//

          //CrrnGrpMED = SavRrnGrpMED;

          clrStaMED1 = *Off;
          dspStaSub1 = *On;
          dspStaCtl1 = *On;

       When isStateGrid = '2';

          rrnStaMED2 = 0;
          clrStaMED2 = *On;
          dspStaSub2 = *Off;
          dspStaClt2 = *Off;

          //***************//
          Write RcdAddMEND;
          //***************//

          clrStaMED2 = *Off;
          dspStaSub2 = *On;
          dspStaClt2 = *On;

          //turn on ADD button
          dspMODE3 = *on;

       EndSl;

      /End-Free
     P ClrSubfilSta    E
       //===================================================================
       // EditAddState - Add State Specific Midterm Endorsements
       //===================================================================
     P EditAddState    B
     d EditAddState    PI
     c
      /free
       Select;
       When isStateGrid  = '1';
          M2St   = scSSState;
          M2Form = %trim(scSSForm);

          If isStaMode = 'A';
             M2co#  = ##co#;
             M2fnd  = scSSGrp;
             M2Sort = scSSSort;
             M2STAT = 'A';
             M2USER = q1user;
             M2PGM  = q1pgm;
             M2DT   = Dateto7(SystemDate());
             M2TIME = Timeto6(SystemTime());
             Write pm20p;
          Else;
             M2USER = q1user;
             M2PGM  = q1pgm;
             M2DT   = Dateto7(SystemDate());
             M2TIME = Timeto6(SystemTime());
             Update pm20p;
          Endif;

          Unlock WTM2P;
       EndSl;

      /end-free
     P EditAddState    E
       //====================================================================
       // DeleteMEND - Delete Midterm Endorsement - Delete State Specific too
       //====================================================================
     P DeleteMEND      B
     d DeleteMEND      PI
     c
      /free
       Chain scMERRn# pmy0p;
       If %found;
          //*------------------------------------------------------------------
          // Delete ALL state specific records for this company, fund and sort
          //*-----------------------------------------------------------------
          SetLL (MYCo#:MYfnd:MYsort) pm201;
          ReadE (MYCo#:MYfnd:MYsort) pm201;
          Dow not %eof(WTM2L);
             If M2FND <> MYfnd or M2CO# <> MYCo# or M2Sort <> MYSort;
                Leave;
             EndIf;

             //*-----------------*
             Chain rrnWTM2L pm20p;
             If %found(WTM2P);
                delete pm20p;
             Else;
                unlock WTM2P;
             Endif;
             //*-----------------*
             ReadE (MYCo#:MYfnd:MYsort) pm201;
          EndDo;

       //*----------*
       delete pmy0p;
       //*----------*

       unlock WTM2P;
       unlock WTMYP;

       FirstTime = *on;     //refresh mid-end detail & State Specific subfile

       EndIf;

      /end-free
     P DeleteMEND      E
       //===================================================================
       // Load Midterm Endorsements Detail for group
       //===================================================================
     P LoadGrpDetail   B
     d LoadGrpDetail   PI
      /free

       clear scDtlDesc;
       clear scDtlSort;
       clear scDtlStRp;
       clear scDtlSts;
       clear scDtlPgm;
       clear scDtlForm;
       //*** Beg Add ***** 06/03/19 *********************
       clear scDtlWavr;
       //*** End Add ***** 06/03/19 *********************

       Chain(n) scMERrn# pmy0p;

       If %found(WTMYP);
          //Description
          scDtlDesc = %trim(MYDesc);
          //*** Beg Add ***** 06/03/19 *********************
          Clear flgWaiver;
          If MYWavr = 'Y' ;
             flgWaiver = 'Y';
          Endif;
          //*** End Add ***** 06/03/19 *********************

          //Program
          scDtlPgm = %trim(MYpgm1);

          //Form
          scdtlForm  = %trim(MYDsc1);

          //Sort - move to alpha
          scDtlSort = MYSort;

          //***** Beg Add *****  01/20/20 ***************************

          If scDtlPgm = 'W0051R11' or scDtlPgm = 'W0051R12' or
           scDtlPgm = 'W0051R13' or scDtlPgm = 'W0051R15' or
           scDtlPgm = 'W0051R19' or scDtlPgm = 'W0051R20' or
           scDtlPgm = 'W0051R21' or scDtlPgm = 'W0051R40' or
           scDtlPgm = 'W0051R55' or scDtlPgm = 'W0051R65' or
           scDtlPgm = 'ADDNAMEINS';
             scSupSch = MYschd;
             isMidTrm = *on;
          Else;
             scSupSch = ' ';
             isMidTrm = *off;
          Endif;

          //***** End Add *****  01/20/20 ***************************

          // Status
          Select;
             When MYstat = 'A';
                scDtlSts  = 'Active';
             When MYstat = 'I';
                scDtlSts  = 'inActive';
          Endsl;

          // State Reporting Division
          Select;
             When MYsrfl = 'Y';
                scDtlStRp = 'YES';
             When MYsrfl = 'N';
                scDtlStRp = 'NO ';
          Endsl;

          //*** Beg Add ***** 06/03/19 *********************
          // Waiver Y/N
          If MYWavr  = 'Y';
             scDtlWavr = 'YES';
          Else;
             scDtlWavr = 'NO ';
          Endif;
          //*** End Add ***** 06/03/19 *********************

       EndIf;
      /end-free
     P LoadGrpDetail   E
       //===================================================================
       // Load Midterm Endorsements for Group
       //===================================================================
     P LoadGrpMEND     B
     d LoadGrpMEND     PI
      /free

       // clear the subfile.
       Clear sflGrpMEND;

       // ADD button visibility
       //*** beg chg ***** 02/02/21 ************
       //If mode = 'D';
       //   VisbAdd = *Off;
       //   dsaAdd  = *on;
       //Else;
       //   VisbAdd = *On;
       //   dsaAdd  = *off;
       //Endif;
       VisbAdd = *Off;
       dsaAdd  = *on;
       //*** end chg ***** 02/02/21 ************

       Setll (##co#:##fnd) pmy01;
       Reade (##co#:##fnd) pmy01;

       Dow not %eof(WTMYL);
          // Write Midterm Endorsement subfile
          // description
          scMEDesc = %trim(MYDESC);

          // sort
          scMESort  = MYSORT;
          intSort = MYSORT;        //key field for state midterm endorsement

          // state reporting division only
          Select;
          When MYSRFL = 'Y';
             scMEStRp = 'Yes';
          When MYSRFL = 'N';
             scMEStRp = 'No';
          EndSl;

          // status
          If mode = 'D';
             //display output fld status
             Select;
             When MYSTAT = 'A';
                scM1StatsO = 'Active';
             When MYSTAT = 'I';
                scM1StatsO = 'inActive';
             EndSl;
          Else;
             //display hyperlink status
             Select;
             When MYSTAT = 'A';
                scM1Status = 'Active';
             When MYSTAT = 'I';
                scM1Status = 'inActive';
             EndSl;
          EndIf;

          // update hidden fields
          scMERrn# = rrnWTMYL;

          rrnGrpMED = rrnGrpMED + 1;

          // Load State specific midterm endorsement file first time
          If FirstTime or rrnGrpMED = scHdnRrn#;
             LoadGrpDetail();
             LoadStaMEND();
             FirstTime = *off;
             scMEDRowSl = 'A';
          Else;
             scMEDRowSl = ' ';
          Endif;

          //Edit/Copy hyperlinks visiability
          If mode = 'D';
             dspmode = *on;
             dspVisb = *Off;
             //Set output status field to visible
             dspVisbS = *On;
          Else;
             dspmode = *Off;
             dspVisb = *On;
             //Set output status field to disabled
             dspVisbS = *Off;
          Endif;

          Write sflGrpMEND;

          Reade (##co#:##fnd) pmy01;
       Enddo;

       If rrnGrpMED > 0;
          dspMEDSub = *On;
          dspMEDCtl = *On;
          If CrrnGrpMED=0;
             rrnGrpMED=1;
          Else;
             rrnGrpMED = CrrnGrpMED;
          EndIf;
       EndIf;
       If scHdnRrn# = 0;
          scHdnRrn# =1;
       EndIf;

      /end-free
     P LoadGrpMEND     E
       //===================================================================
       // Load Selection - Load the criteria fields for FYR/DIV drop down boxes.
       //===================================================================
     P LoadSelection   B
     D LoadSelection   PI

     dfirst            s               n   inz(*on)

      /free

       //Midterm Endorsement screen
       clear grpChoices;
       grpChoices = 'FNco# = ' + %char(FNco#);

       //Copy ALL screen
       clear grpChoiceC;
       grpChoiceC = 'FNco# = ' + %char(FNco#);

       //clear divChoices;
       //divChoices = 'EMco# = ' + %char(ELco#) + ' AND EMfnd = '
       // + %char(ELfnd) + ' AND EMemp# = ' + %char(ELemp#);

      /end-free
     P LoadSelection   E
       //===================================================================
       // Load State Specific Midterm Endorsements for group - ADD screen
       //===================================================================
     P LoadStaArray    B
     d LoadStaArray    PI
      /free
       //Visibility ADD button
       dsaAdd3  = *off;
       //*** Beg Del ***** 06/03/19 **********************
       //VisbAdd3 = *on;
       //*** End Del ***** 06/03/19 **********************

       Clear scME2Co#;
       Clear scME2Grp;
       Clear scME2Sort;
       Clear scME2State;
       Clear scME2Form;
       Clear scME2Sts;

       y = 1;
       DoW y <= 99;
          If arrME2St(y) = ' ';
            Leave;
          Endif;

          scME2Co#   = arrME2Co#(y);
          scME2Grp   = arrME2Grp(y);
          scME2Sort  = arrME2Sort(y);
          scME2State = arrME2St(y);
          scME2Form  = arrME2Form(y);
          Select;
          When arrME2Sts(y) = 'A';
             scME2Sts = 'Active';
          When arrME2Sts(y) = 'I';
             scME2Sts = 'inActive';
          EndSl;

          //In ADD mode, data is saved from arrays - no rrn#
          //scSta2Rrn   = arrME2Rrn(y);
          scSta2Rrn   = *zeros;

          y = y + 1;
          rrnStaMED2 = rrnStaMED2 + 1;
          If rrnStaMED2 = 1;
             scStaRowS2 = 'A';
          Else;
             clear scStaRowS2;
          Endif;

          // visiability for Edit/Copy
          dspMODE3 = *on;
          dspVisb3 = *off;

          Write sflStaMED2;
       Enddo;

       rrnStaMED2 = 1;

      /end-free
     P LoadStaArray    E
       //===================================================================
       // Load State Specific Midterm Endorsements for group
       //===================================================================
     P LoadStaMEND     B
     d LoadStaMEND     PI
      /free

       Clear rrnStaMED1;

       // visiability for ADD botton
       //*** beg chg ***** 02/02/21 ************
       //If mode = 'D';
       //   dsaAdd2  = *on;
       //   VisbAdd2 = *Off;
       //Else;
       //   dsaAdd2  = *off;
       //   VisbAdd2 = *on;
       //Endif;
       //*** Beg Add ***** 06/03/19 *********************
       //If flgWaiver = 'Y';
       //   VisbAdd2 = *On;
       //Else;
       //   VisbAdd2 = *Off;
       //Endif;
       //*** End Add ***** 06/03/19 *********************
       dsaAdd2  = *on;
       VisbAdd2 = *Off;
       //*** end chg ***** 02/02/21 ************

       Setll (##co#:##fnd:intSort) pm201;
       Reade (##co#:##fnd:intSort) pm201;

       Dow not %eof(WTM2L);

          // Sort field
          scME1Sort = M2Sort;

          // State (abreviation for now)
          scME1State = %trim(M2St);

          // Form Description
          scME1Form = %trim(M2Form);

          // status
          If mode = 'D';
             //display output fld status
             Select;
             When M2STAT  = 'A';
                scME1StsO = 'Active';
             When M2STAT  = 'I';
                scME1StsO = 'inActive';
             EndSl;
          Else;
             //display hyperlink status
             Select;
             When M2STAT = 'A';
                scME1Sts = 'Active';
             When M2STAT = 'I';
                scME1Sts = 'inActive';
             EndSl;
          EndIf;

          scSta1Rrn   = rrnWTM2L;

          rrnStaMED1 = rrnStaMED1 + 1;
          If rrnStaMED1 = 1;
             scStaRowS1 = 'A';
          Else;
             clear scStaRowS1;
          Endif;

          // Visiability for Edit/copy
          If mode = 'D';
             dspmode2 = *On;
             dspVisb2 = *Off;
             dspVisbS = *On;
          Else;
             dspmode2 = *Off;
             dspVisb2 = *On;
             dspVisbS = *Off;
          Endif;

          Write sflStaMED1;

          Reade (##co#:##fnd:intSort) pm201;
       Enddo;

       rrnStaMED1 = 1;

      /end-free
     P LoadStaMEND     E
       //===================================================================
       // Load State Specific Midterm Endorsements for group - edit/add screen
       //===================================================================
     P LoadStaMEND2    B
     d LoadStaMEND2    PI
      /free
       // visiability for ADD botton
       dsaAdd3  = *On;
       VisbAdd3 = *Off;

       Setll (##co#:##fnd:intSort) pm201;
       Reade (##co#:##fnd:intSort) pm201;

       Dow not %eof(WTM2L);

          // Sort field
          scME2Sort = M2Sort;

          // State (abreviation for now)
          scME2State = %trim(M2St);

          // Form Description
          scME2Form = %trim(M2Form);

          // status
          Select;
          When M2STAT = 'A';
             scME2Sts = 'Active';
          When M2STAT = 'I';
             scME2Sts = 'inActive';
          EndSl;

          scSta2Rrn   = rrnWTM2L;

          rrnStaMED2 = rrnStaMED2 + 1;
          If rrnStaMED2 = 1;
             scStaRowS2 = 'A';
          Else;
             clear scStaRowS2;
          Endif;

          // visiability for Edit/copy
          dspMode3 = *On;
          dspVisb3 = *Off;

          Write sflStaMED2;

          Reade (##co#:##fnd:intSort) pm201;
       Enddo;

       rrnStaMED2 = 1;

      /end-free
     P LoadStaMEND2    E
       //===================================================================
       // Main - Main processing
      //===================================================================
     P Main            B
     d Main            PI

      /free
       If doLoad;

          // Clear Sub-files
          ClrSubfilMst();

          // Load Group Midterm Endorsement Configuration
          LoadGrpMEND();
          isGrpFocus = *on;
          doLoad = *off;
       EndIf;

       // Process policy summary screen.
       Exsr ExfmtScreens;

       // Process screen actions
       ScreenActions();

       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;
          addPop01 = *on;     //show as Window

          //***** Beg Add ***** 03/25/21 ****************************
          visGrpCfg = *on;    // Show Window Panel and Cancel Button
          callpgm = *off;
          ChkStack('WTGRPPOLCG':CallPgm);
          If callpgm = *on;
             visGrpCfg = *off;
             addPop01 = *off;
          Endif;
          //***** End Add ***** 03/25/21 ****************************

          Exfmt RcdMENDMst;   //record formats  - grid 1 and grid 2

       Endsr;

      /end-free
     P Main            E
       //===================================================================
       // MoveDbSc - Move database record to screen
       //===================================================================
     P MoveDbSc        B
     d MoveDbSc        PI
     c
      /free

       scGroup#   = MYfnd;
       grpChoices = 'FNco# = ' + %char(##co#) + ' AND FNfnd = '
        + %char(MYfnd);

       scAddSort  = MYsort;
       scAddMenDs = MYdesc;
       scAddPgm   = MYpgm1;
       scAddForm  = MYdsc1;
       scAddSRDiv = MYSRFl;
       scStatus   = MYstat;
       //*** Beg Add ***** 06/03/19 *********************
       scWaiver   = MYWavr;
       //*** End Add ***** 06/03/19 *********************

       //*** Beg Add ***** 01/20/20 *********************
       scSuppSch = MYSCHD;
       //*** End Add ***** 01/20/20 *********************
       svFND      = MYFND;
       svSort     = MYSORT;
       //*** Beg Add ***** 05/10/21 ***************************
       SVdt   = MYdt;
       SVtime = MYtime;
       //*** End Add ***** 05/10/21 ***************************

      /end-free
     P MoveDbSc        E
       //===================================================================
       // MoveStaDbSc - Move State Specific database record to screen
       //===================================================================
     P MoveStaDbSc     B
     d MoveStaDbSc     PI
     c
      /free
       // only for Edit and Copy
       scSSState  = M2St;
       scSSForm   = M2Form;

      /end-free
     P MoveStaDbSc     E
       //===================================================================
       // processAddEdit - process add/Edit of Midterm Endorsements
       //===================================================================
     P ProcessAddEdit  B
     d ProcessAddEdit  PI
     d  isMode                        1a
      /free
       //Clear all buttons
       btnM1Add  = *off;
       btnSavAdd = *off;
       btnCanAdd = *off;
       btnSavNxt = *off;

       // turn visibility off all SPLATS - used for copy mode only
       VisCopyRcd = *off;
       //*** Beg Add ***** 06/03/19 ***************************
       visToGrp = *off;
       //*** End Add ***** 06/03/19 ***************************

       If CrrnGrpMed <> 0;
          Chain CrrnGrpMED sflGrpMEND;
          scHdnRrn# = CrrnGrpMED;
       Else;
          Chain scHdnRrn# sflGrpMEND;
       EndIf;

       //Set visibility off for SAVE/ADD_NEXT button
       LeftMEDCan = 835;
       dspVisNxt = *off;   //hidden
       dspAddNxt = *on;    //disabled
       dsaGroup = *on;     //group - read only - key field
       dsaSort  = *on;     //sort  - read only - key field

       // processing for Edit mode
       If isMode = 'E';
          //*** Beg Chg ***** 05/10/21 ***************************
          //***** Chain scMERrn# pmy0p;
          Chain(n) scMERrn# pmy0p;
          //*** End Chg ***** 05/10/21 ***************************
          intGroup = MYfnd;
          intSort  = MYsort;
          scAECTitl = 'Edit Midterm Endorsement record';
          MoveDbSc();
          //*** Beg Add ***** 06/03/19 ***************************
          scMDMode = 'E';
          //*** End Add ***** 06/03/19 ***************************
       Endif;

       // processing for COPY mode
       If isMode = 'C';
          Chain(n) scMERrn# pmy0p;
          intGroup = MYfnd;
          intSort  = MYsort;
          VisCopyRcd = *on;
          //*** Beg Chg ***** 06/03/19 ***************************
          //dsaGroup = *off;
          //dsaSort  = *off;
          grpTChoice = 'FNco# = ' + %char(FNco#);
          visToGrp = *on;
          scMDMode = 'E';
          //*** End Chg ***** 06/03/19 ***************************
          scAECTitl = 'Copy Midterm Endorsement record';
          MoveDbSc();
          scCpySSRcd = 'Y';       //clear check box value - copy state specific records
        //dspVisb3 = *off;        //Edit/copy/delete hperlinks - State spcific
        //dspMode3  = *on;        //disabled
       Endif;

       // processing for Add mode
       If isMode = 'A';
          isStateGrid  = '2';
          intGroup = ##fnd;
          //Default group status value to 'Active'
          scStatus = 'A';
          //Set visibility off for SAVE/ADD_NEXT button
          LeftMEDCan = 720;
          dspVisNxt = *On;    //visiable
          dspAddNxt = *Off;   //enabled
          dsaGroup = *off;
          dsaSort  = *off;
          scAECTitl = 'Add Midterm Endorsement record';
          //*** Beg Add ***** 06/03/19 ***************************
          scMDMode = 'A';
          //*** End Add ***** 06/03/19 ***************************
       Endif;

       // Prefill fund/group
       scGroup# = intGroup;
       grpChoices = 'FNco# = ' + %char(##co#) + ' AND FNfnd = '
        + %char(intGroup);

       //Load State Specific grid and display the format screen
       //This is available for Edit or Copy functions; clear the grid for Add function
       ClrSubFilSta();

       If isMode = 'E' or isMode = 'C';
          LoadStaMEND2();
       Else;
          Clear sflStaMED2;
          //ADD mode enabled
          dsaAdd3  = *Off;
          //*** Beg Del ***** 06/03/19 **********************
          //VisbAdd3 = *On;    //Add button only visible if Wiaver check box is checked in Add mode
          //*** End Del ***** 06/03/19 **********************
       EndIf;

       ClearErrors();

       Dou BtnCanAdd = *on or (BtnSavAdd = *on and isValid = *off);

          Exfmt RcdAddMEND;
          ClearErrors();
          Readc sflStaMED2;

          isValid = *off;
          Select;
          //ADD state grid
          When btnSt2Add = *on;
             isValid = *off;
             Validate(isMode);
             If isValid = *off;
                clear RcdAState;
                isStaMode = 'A';
                //process State grid 2
                isStateGrid  = '2';
                intGroup = scGroup#;
                intSort   = scAddSort;
                procStaAddEdit(isStaMode);
                FlgWrtSSRec = 'Y';
                btnSt2Add  = *off;
             EndIf;

          //Save or Save/Add-next buttons
          When BtnSavAdd = *on or BtnSavNxt = *on;
             isValid = *off;
             Validate(isMode);
             If isValid = *off;
                //*** Beg Chg ***** 05/10/21 ***************************
                If isMode = 'E';
                   // Check if WTMYP updated since screen displayed (Midterm Endorsement)
                   flgRcdUpdated=RcdUpdatedErr('WTMYP':rrnWTMYP:'MYdt':'MYtime':
                    'MYuser':SVdt:SVtime:'1');
                   if flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scMERrn# pmy0p;
                      MoveDbSc();
                      isValid = *on;
                   EndIf;
                   if flgRcdUpdated = '1'; // Error without Refresh Button
                      isValid = *on;
                   EndIf;
                   if flgRcdUpdated = '0'; // No error
                      Chain scMERrn# pmy0p;
                      AddMEND();
                   EndIf;
                EndIf;

                If isMode <> 'E';
                //*****   AddMEND();
                   AddMEND();
                EndIf;
                //*** End Chg ***** 05/10/21 ***************************
                If FlgWrtSSRec = 'Y';
                   AddState();
                   FlgWrtSSRec = 'N';
                EndIf;
                If scCpySSRcd = 'Y';
                   CopyMendSta();
                   scCpySSRcd = 'N';
                EndIf;
                //if adding more records, then clear all fields
                If BtnSavNxt = *on;
                   ClrScrnFlds();
                   ClrSubFilSta();
                EndIf;
             Endif;

          //Cancel button
          When BtnCanADD = *on;
             BtnCanADD = *off;
             Leave;
          Endsl;
       Enddo;

       unlock WTMYP;
      /end-free
     P ProcessAddEdit  E
       //=========================================================================
       // procStaAddEdit - process add/Edit of State Specific Midterm Endorsements
       //=========================================================================
     P ProcStaAddEdit  B
     d ProcStaAddEdit  PI
     d  isStaMode                     1a
      /free

       btnM2Add  = *off;
       btnSavSta = *off;
       btnCanSta = *off;
       btnSavASta = *off;

       If isStateGrid = '1';
          Chain scHdnRrn# sflGrpMEND;
       EndIf;

       //disable SAVE/ADD_NEXT button
       LeftStaCan = 635;
       visStaNxt = *off;   //hidden
       dspStaNxt = *on;    //disabled

       //Set disabled and visiability indicators for Edit/Add
       dspStaCpy = *on;
       visStaCpy = *off;

       dspStaAdd = *off;
       visStaAdd = *on;
       //*** Beg Add ***** 06/03/19 **************************
       dspStaAEdt = *off;
       //*** Beg Add ***** 06/03/19 **************************

       Select;
       // processing for Edit modes
       When isStaMode = 'E';
          //*** Beg Add ***** 06/03/19 **************************
          dspStaAEdt = *on;
          //*** Beg Add ***** 06/03/19 **************************
          Select;
          When isStateGrid = '1';
             Chain scSta1Rrn pm20p;
          When isStateGrid = '2';
             Chain scSta2Rrn pm20p;
          EndSl;

          intGroup = M2fnd;
          intSort   = M2sort;
          scM21Edit= *off;
          //group - disabled - read only - key field
          //Sort  - disabled - read only - key field
          MoveStaDbSc();

          // setup screen title State Specific Edit screen
          Clear StateTitle;
          StateTitle = %trim(EdtStaTxt) + ' ' +
                       %trim(StateTxt)   + ' ' +
                       %trim(MENDTxt);
          tlStaAEC   = %trim(StateTitle);

       // processing for Add modes
       When isStaMode = 'A';
          Select;
          When isStateGrid = '1';
           //Chain(n) scSta1Rrn pm20p;
             intGroup  = scFnd;
             intSort   = scMESort;
          When isStateGrid = '2';
             intGroup  = scGroup#;
             intSort   = scAddSort;
          EndSl;

          btnM2Add  = *off;
          Clear scSSState;
          Clear scSSForm;

          //display SAVE/ADD_NEXT button
          LeftStaCan = 520;
          visStaNxt = *On;    //hidden
          dspStaNxt = *Off;   //disabled

          // setup screen title State Specific Add screen
          Clear StateTitle;
          StateTitle = %trim(AddStaTxt) + ' ' +
                       %trim(StateTxt)   + ' ' +
                       %trim(MENDTxt);
          tlStaAEC   = %trim(StateTitle);

       // processing for COPY mode
       When isStaMode = 'C';
          Select;
          When isStateGrid = '1';
             Chain(n) scSta1Rrn pm20p;
          When isStateGrid = '2';
             Chain(n) scSta2Rrn pm20p;
          EndSl;

          intGroup = M2fnd;
          intSort   = M2sort;
          Clear scTSSState;
          Clear scTSSForm;
          MoveStaDbSc();

          //Set disabled and visiability indicators for Copy
          dspStaCpy = *off;
          visStaCpy = *on;
          dspStaAdd = *on;
          visStaAdd = *off;
          //*** Beg Add ***** 06/03/19 **************************
          dspStaAEdt = *on;
          //*** Beg Add ***** 06/03/19 **************************

          // setup screen title State Specific Copy screen
          Clear StateTitle;
          StateTitle = %trim(CpyStaTxt) + ' ' +
                       %trim(StateTxt)   + ' ' +
                       %trim(MENDTxt);
          tlStaAEC   = %trim(StateTitle);
       EndSl;

       // Prefill fund/sort
       scSSGrp  = intGroup;
       scSSSort = intSort;

       //update screen output fields
       Chain (##Co#:scSSGrp) pfn01;
       If %found(WMFNL);
          scSSGrpDs = %editc(scSSGrp:'X') + ' - ' + %trim(FNname);
       EndIf;
       Chain (##Co#:scSSGrp:scSSSort) pmy01;
       If %found(WTMYL);
          scSSSortDs = %editc(scSSSort:'X') + ' - ' + %trim(MYdesc);
       Else;
          scSSSortDs = %editc(scSSSort:'X');
       EndIf;

       //*** Beg Add ***** 06/03/19 **************************
       //State Specific Form Selection
       clear SSFChoices;
       //SSFChoices = 'ENco# = ' + %char(FNco#) + ' AND ENfnd = '
       // + %char(FNfnd) + ' AND SUBSTR(ENFORM,9,1) = '' ''';
       SSFChoices = 'ENco# = ' + %char(FNco#) + ' AND ENfnd = '
        + %char(FNfnd) + '';

       clear SSTChoices;
       //SSTChoices = 'ENco# = ' + %char(FNco#) + ' AND ENfnd = '
       // + %char(FNfnd) + ' AND SUBSTR(ENFORM,9,1) = '' ''';
       SSTChoices = 'ENco# = ' + %char(FNco#) + ' AND ENfnd = '
        + %char(FNfnd) + '';
       //*** End Add ***** 06/03/19 **************************

       ClearErrors();

       Dou (BtnSavSta = *on or BtnCanSta = *on or BtnSavASta = *off)
            and isValid = *off;

          Exfmt RcdAState;

          ClearErrors();
          isValid = *off;

          Select;
          When BtnSavSta = *on or BtnSavASta = *on;
             ValidSta(isStaMode);
             If isValid = *off;
                Select;
                When isStaMode = 'A' or isStaMode = 'E';
                   If isStateGrid = '1';
                      EditAddState();
                   ElseIf isStateGrid = '2';
                      SaveState();
                   EndIf;
                When isStaMode = 'C';
                   CopyState();
                When isStaMode = 'D';
                   //DltState();
                EndSl;

                //clear fields
                If BtnSavASta = *on;
                   If isStaMode = 'C';
                      scTSSState = *blanks;
                      scTSSForm = *blanks;
                   Else;
                      scSSState = *blanks;
                      scSSForm = *blanks;
                   Endif;
                Endif;
             Endif;

          When BtnCanSta = *on;
             BtnCanSta = *off;
             Leave;
          Endsl;

       Enddo;

       //Load state arrays to screen fields and clear arrays
       If BtnSavSta = *on or BtnSavASta = *on;
          If isStaMode = 'A' and isStateGrid = '2';
             LoadStaArray();
             ClrStaArray();
          Endif;
       Endif;

       unlock WTM2P;
      /end-free
     P ProcStaAddEdit  E
       //===================================================================
       // processCopyALL - process Copy All of Midterm Endorsements
       //===================================================================
     P ProcessCopyAll  B
     d ProcessCopyAll  PI
      /free

       btnCpyAll = *off;
       btnSavCopy = *off;
       btnCanCopy = *off;

       Clear BtnSavCopy;
       Clear BtnCanCopy;

       // processing for COPY ALL
       // isMode = 'P';
          dsaGroup = *off;
          intGroup = ##fnd;

       //Update screen title with group#
       Clear scCpyAllM;
       scCpyAllM  = tlCpyAllME;

       Clear scToGrp;
       //scCpySRDiv = 'Y';
       scCpySSInf = 'Y';

       Dou (BtnCanCopy = *on or BtnSavCopy = *on) and isValid = *off;
          Exfmt RcdCopyAll;
          isValid = *off;
          Select;
          When BtnSavCopy = *on;
             Validate(isMode);
             If isValid = *off;
                CopyAllMEND();
                If scCpySSInf = 'Y';
                   CopyAllMESta();
                EndIf;
             EndIf;
          When BtnCanCopy = *on;
             BtnCanCopy = *off;
             Leave;
          Endsl;
       Enddo;

       // unlock WTMYP;
      /end-free
     P ProcessCopyAll  E
       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     P ProgramStart    B
     d ProgramStart    PI
     d processMe                       n

      /free

       // Chain to Group (fund) Master File
       Chain xxRrn PFN0P;
       //scActRrn=xxRrn;

       // Process only if Group Master file record is found
       If not %found;
          processme = *Off;
          Return;
       Endif;

       // Populate hidden fields
       scJob  = q1job;
       scJobN = q1jobN;
       scCusr = q1CUSR;
       scCO#  = fnco#;
       scFnd  = fnfnd;
       scMod  = mode;

       // setup screen title for ADD/EDIT/COPY/DELETE screen
       Clear MENDTitle;
       MENDTitle = %trim(MENDTxt) + ' - Group: ' +
                   %Editc(fnfnd:'X') + ' - ' +
                   %trim(fnname);
       tlMENDMst = %trim(MENDTitle);

       // setup screen title for COPYALL screen
       Clear MENDTitle;
       MENDTitle = %trim(CpyATxt) + ' ' +
                   %trim(MENDTxt) + ' ' +
                   %Editc(fnfnd:'X') + ' - ' +
                   %trim(fnname);
       tlCpyAllME = %trim(MENDTitle);

       //TabIndex = 0;
       //APnlHeight = 1100;

       ##Co#   = FNco#;
       ##Fnd   = FNfnd;
       FirstTime = *on;
       ##Rrn   = 0;

       LoadSelection();    //load group selection
        isGrpFocus = *off;
       processMe = *on;
       Return;

      /end-free
     P ProgramStart    E
       //===================================================================
       // SaveState - Save State Specific Midterm Endorsements
       //===================================================================
     P SaveState       B
     d SaveState       PI
     c
      /free

          Select;
          When isStaMode = 'A';
             xxIndex = %Lookup(scSSState : arrME2St);
             If xxIndex = 0;
                arrIdx = arrIdx + 1;
                arrME2Co#(arrIdx)  = ##co#;
                arrME2Grp(arrIdx)  = scSSGrp;
                arrME2Sort(arrIdx) = scSSSort;
                arrME2St(arrIdx)   = scSSState;
                arrME2Form(arrIdx) = %trim(scSSForm);
                arrME2Sts(arrIdx)  = 'A';
             Endif;
          When isStaMode = 'E';
             xxIndex = %Lookup(scSSState : arrME2St);
             If xxIndex <> 0;
                arrME2Co#(xxIndex)  = ##co#;
                arrME2Grp(xxIndex)  = scSSGrp;
                arrME2Sort(xxIndex) = scSSSort;
                arrME2St(xxIndex)   = scSSState;
                arrME2Form(xxIndex) = %trim(scSSForm);
                arrME2Sts(xxIndex)  = 'A';
             Endif;
          EndSl;
      /end-free
     P SaveState       E
       //===================================================================
       // ScreenActions - process all screen actions
      //================== hmk ============================================
     P ScreenActions   B
     d ScreenActions   PI

      /free

       Select;

          // Refresh
          When btnRefresh = *On;
             btnRefresh =*Off;
             //Turn off row click event
             btnMENDClk =*Off;
             //doLoad = *on;

          // Midterm Endorsement Row click - update state specific subfile
          When btnMENDClk = *On;
             isStateGrid = '1';
             btnMENDClk =*Off;
             rrnGrpMED = CrrnGrpMED;
             scHdnRrn# = CrrnGrpMED;
             SavRrnGrpMed = CrrnGrpMED;
             doLoad = *on;

          // Add Midterm Endorsements for Group
          When btnM1Add = *on;
             clear RcdAddMEND;
             isMode = 'A';
             isStateGrid = '1';
             ProcessAddEdit(isMode);
             doLoad = *on;

          // Add State Specific Midterm Endorsements
          When btnM2Add = *on;
             clear RcdAState;
             isStaMode = 'A';
             isStateGrid = '1';
             procStaAddEdit(isStaMode);
             doLoad = *on;

          // copy ALL
          When btnCpyAll = *on;
             clear RcdCopyAll;
             isMode = 'P';
             isStateGrid = '1';
             ProcessCopyAll();
             doLoad = *on;

          // Cancel
          When BtnCancel = *on;
           //BtnCancel = *off;
           //ExitNow = *on;
           // exit the program

          Other;

             //***** Beg Add ***** 03/25/21 ****************************
             callpgm = *off;
             ChkStack('WTGRPPOLCG':CallPgm);
             If callpgm = *on;
                btnCancel = *on;
             Endif;
             //***** End Add ***** 03/25/21 ****************************

             Readc sflGrpMEND;
             DoU %EOF;
                If CrrnGrpMED <> 0;
                   Select;
                   // processing for Edit mode
                   When scM1Edit  = *on;
                      scM1Edit = *off;
                      //***** Beg Add ***** 06/09/21 ****************************
                      btnCancel = *off;
                      //***** End Add ***** 06/09/21 ****************************
                      clear RcdAddMEND;
                      isMode = 'E';
                      isStateGrid = '2';
                      ProcessAddEdit(isMode);
                      doLoad = *on;

                   // processing for COPY
                   When scM1Copy = *on;
                      scM1Copy = *off;
                      //***** Beg Add ***** 06/09/21 ****************************
                      btnCancel = *off;
                      //***** End Add ***** 06/09/21 ****************************
                      clear RcdAddMEND;
                      isMode = 'C';
                      isStateGrid = '2';
                      ProcessAddEdit(isMode);
                      doLoad = *on;

                   // processing for DELETE
                   //   Delete both Medterm Endorsement record & State Specific
                   When scM1Del = *on;
                      scM1Del = *off;
                      //***** Beg Add ***** 06/09/21 ****************************
                      btnCancel = *off;
                      //***** End Add ***** 06/09/21 ****************************
                      isMode = 'D';
                      isStateGrid = '1';

                      //*** Beg Chg ***** 05/10/21 ***************************
                      //***  DeleteMEND();

                      Chain(e) scMERRn# pmy0p;
                      If %status = 1218;
                      PopOneBtn('Record locked by another user. Please try-
                       again later.':'Record Lock');
                      Return;
                      Endif;

                      Chain scMERRn# pmy0p;

                      Dow isexit = *off;
                         isDelresp = PopTwoBtn('Do you want to delete this Mid+
                         term Endorsement Info?':'Confirm Delete':'Yes':'No');
                         Select;
                         When isDelResp = *On;
                            DeleteMEND();
                            PopOneBtn('Midterm Endorsement Information +
                            Deleted!':'Delete':'Ok');
                            isexit = *on;
                         When isDelResp = *Off;
                            isexit = *on;
                            unlock WTMYP;
                         EndSl;
                      EndDo;
                      //*** End Chg ***** 05/10/21 ***************************
                      doLoad = *on;
                   Endsl;
                EndIf;
                Readc sflGrpMEND;
             EndDo;


             Readc sflStaMED1;
             DoU %EOF;
                If CrrnMEDSt1 <> 0;
                   Select;
                   // processing for State Specific EDIT mode
                   When scM21Edit = *on;
                      scM21Edit= *off;
                      //***** Beg Add ***** 06/09/21 ****************************
                      btnCancel = *off;
                      //***** End Add ***** 06/09/21 ****************************
                      clear RcdMENDSta;
                      isStaMode = 'E';
                      isStateGrid = '1';
                      procStaAddEdit(isStaMode);
                      doLoad = *on;

                   // processing for State Specific COPY mode
                   When scM21Copy= *on;
                      scM21Copy= *off;
                      //***** Beg Add ***** 06/09/21 ****************************
                      btnCancel = *off;
                      //***** End Add ***** 06/09/21 ****************************
                      clear RcdMENDSta;
                      isStaMode = 'C';
                      isStateGrid = '1';
                      procStaAddEdit(isStaMode);
                      doLoad = *on;

                   // processing for DELETE
                   When scM21Del= *on;
                      scM21Del= *off;
                      //***** Beg Add ***** 06/09/21 ****************************
                      btnCancel = *off;
                      //***** End Add ***** 06/09/21 ****************************
                      isMode = 'D';
                      isStateGrid = '1';
                      Chain scSta1Rrn pm20p;
                      delete pm20p;
                      unlock WTM2P;
                      doLoad = *on;
                   EndSl;
                EndIf;
                Readc sflStaMED1;
             EndDo;
       EndSl;

      /end-free
     P ScreenActions   E
       //===================================================================
       // ValidSta - Validate Add/Edit screen for State Specific Midterm End.
       //===================================================================
     P ValidSta        B
     d ValidSta        PI
     d   isStaMode                    1a
      /free

       isValid  = *off;

       If isStaMode <> 'C';
          // State cannot be blanks
          If scSSState = *blanks;
             msSSSta   = 'WCR0503';
             erSSSta   = *on;
             isValid = *on;
          Else;
             // State code should be valid
             strState = scSSState;
             Chain strState pst01;
             If not %found(STSTL3);
                msSSSta = 'WCR3163';
                erSSSta = *on;
                isValid = *on;
             Endif;
          Endif;

          // State Form cannot be blanks
          If scSSForm  = *blanks;
             msSSForm  = 'WCR0503';
             erSSForm  = *on;
             isValid = *on;
          //*** Beg Add ***** 06/03/19 *********************
          Else;
             // State Form should be valid
             flgFormExist = 'N';
             Setll (##co#:scSSGrp:scSSForm) pen00;
             Reade (##co#:scSSGrp) pen00;
             Dow not %eof(WTENL);
                If %subst(ENform:1:8) = scSSForm;
                   flgFormExist = 'Y';
                   Leave;
                Endif;
                Reade (##co#:scSSGrp) pen00;
             Enddo;
             If flgFormExist = 'N';
                msSSForm  = 'WCR3186';
                erSSForm  = *on;
                isValid = *on;
             Endif;
          //*** End Add ***** 06/03/19 *********************
          Endif;

          If isStaMode = 'A';
             // Check if State form already added
             Chain (##co#:scSSGrp:scSSSort:scSSState) pm201;
             If %found(WTM2L);
                msSSSta = 'WC30005';
                erSSSta = *on;
                isValid = *on;
             Endif;
          EndIf;

       Else;

          // State cannot be blanks
          If scTSSState = *blanks;
             msTSSSta   = 'WCR0503';
             erTSSSta   = *on;
             isValid = *on;
          Else;
             // State code should be valid
             strState = scTSSState;
             Chain strState pst01;
             If not %found(STSTL3);
                msTSSSta = 'WCR3163';
                erTSSSta = *on;
                isValid = *on;
             Endif;
             // Check if State form already added
             Chain (##co#:scSSGrp:scSSSort:scTSSState) pm201;
             If %found(WTM2L);
                msTSSSta = 'WC30005';
                erTSSSta = *on;
                isValid = *on;
             Endif;
          Endif;

          // State Form cannot be blanks
          If scTSSForm  = *blanks;
             msTSSForm  = 'WCR0503';
             erTSSForm  = *on;
             isValid = *on;
          //*** Beg Add ***** 06/03/19 *********************
          Else;
             // State Form should be valid
             flgFormExist = 'N';
             Setll (##co#:scSSGrp:scTSSForm) pen00;
             Reade (##co#:scSSGrp) pen00;
             Dow not %eof(WTENL);
                If %subst(ENform:1:8) = scTSSForm;
                   flgFormExist = 'Y';
                   Leave;
                Endif;
                Reade (##co#:scSSGrp) pen00;
             Enddo;
             If flgFormExist = 'N';
                msTSSForm  = 'WCR3186';
                erTSSForm  = *on;
                isValid = *on;
             Endif;
          //*** End Add ***** 06/03/19 *********************
          Endif;
       Endif;

     P ValidSta        E
       //===================================================================
       // Validate - Validate add/edit screen
       //===================================================================
     P Validate        B
     d Validate        PI
     d   isMode                       1a
      /free

       isValid  = *off;

       Select;
       When isMode = 'E' or isMode = 'A' or isMode = 'C';

          // fund (group#) cannot be zero - invalid
          If scGroup#   = *zeros;
             msGroup# = 'WCR0508';
             erGroup# = *on;
             isValid = *on;
          Endif;
          //*** Beg Add ***** 06/03/19 *********************
          // Copy To fund (group#) cannot be zero - invalid
          If isMode = 'C';
             If scToGroup# = *zeros;
                msToGroup# = 'WCR0508';
                erToGroup# = *on;
                isValid = *on;
             Endif;
          Endif;
          //*** End Add ***** 06/03/19 *********************

          // Sort cannot be zero
          If scAddSort  = *zeros;
             msSort  = 'WCR0502';
             erSort  = *on;
             isValid = *on;
          Else;
             //*** Beg Chg ***** 06/03/19 *********************
             //If isMode = 'A' or isMode = 'C';
             If isMode = 'A';
             //*** End Chg ***** 06/03/19 *********************
                // Sort already exists for co,grp,sort - Add or Copy mode
                intSort = scAddSort;
                Chain keyWTMY2 pmy01;
                If %found(WTMYL);
                   msSort  = 'WC30005';
                   erSort  = *on;
                   isValid = *on;
                Endif;
             Endif;
             //*** Beg Add ***** 06/03/19 *********************
             If isMode = 'C';
                // If Sort already exists in To group, Check if State specific endorsements
                // exist in From Group. If yes, Allow to copy to a new group
                // If Sort already exists in To group, and No State specific records found
                // in From Group, Sent error, NO state specific updates to copy to a new group...
                intSort = scAddSort;
                Chain (##Co#:scToGroup#:intSort) pmy01;
                If %found(WTMYL);
                   //Check if State specific records exist in From Group
                   Chain (##co#:scGroup#:intSort) pm201;
                   If not %found(WTM2L);
                      msSort  = 'WC30005';
                      erSort  = *on;
                      isValid = *on;
                   Endif;
                Endif;
             Endif;
             //*** End Add ***** 06/03/19 *********************
          Endif;

          // State report div only flag must be 'Y' or 'N'
          If scAddSRDiv = *blanks;
          //*** Beg Add ***** 06/17/19 *********************
             scAddSRDiv = 'N';
          Endif;
          If scAddSRDiv <> 'Y' And scAddSRDiv <> 'N';
          //*** End Add ***** 06/17/19 *********************
             msSRDiv = 'WCR0501';
             erSRDIV = *on;
             isValid = *on;
          Endif;

          // Endorsement Description cannot be blanks
          If scAddMenDs = *blanks;
             msDesc  = 'WCR0503';
             erDesc  = *on;
             isValid = *on;
          Endif;

          // Program to be called cannot be blanks
          If scAddPgm   = *blanks;
             msPgm   = 'WCR0503';
             erPgm   = *on;
             isValid = *on;
          Endif;

          // Form Description cannot be blanks
          If scAddForm  = *blanks;
             msForm  = 'WCR0503';
             erForm  = *on;
             isValid = *on;
          Endif;

       //**--------------------------------------**

       // validate for CopyAll button
       When isMode = 'P';
          erCGroup# = *off;
          msGroup#  = *blanks;
          //---------------------------------------------------------
          // Endorsement forms already exists for co and grp in WTMYP
          // cannot COPY ALL to this group
          //---------------------------------------------------------
          intGroup = scToGrp;
          Chain (##co#:intGroup) pmy01;
          If %found(WTMYL);
             msCGroup# = 'WCR0964';             //New msg
             erCGroup# = *on;
             isValid = *on;
          EndIf;

       EndSL;

      /end-free
     P Validate        E
       //===================================================================

      * utility procedure definitions
      //**** /COPY SCOPYPOLPI
