     hcopyright('(c) 2022 Sapiens, Inc. All rights reserved.')

     h/title WTGRPDATA - Data Reporting
     h dftname(WTGRPDATA)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h ALWNULL(*UsrCtl)
     h/define   profoundUI
     h bnddir('STBNDDIR')
     h bnddir('WTGRPCFG')
      //*********************************************************************
      //==============================================================*
      //  Created   : 01/22/20                                         *
      //  Programmer: C:300                                            *
      //  Project   : WC2196- Redesign of Group Configuration program  *
      //===============================================================*
      //  Revisions:                                                   *
      //                                                               *
      //  05/23/24 C:300 Support: 115634 (SUNZ)                        *
      //                 Refresh Button should clear all errors and    *
      //                 Re-load screen with latest file information   *
      //                                                               *
      //  08/28/23 C:300 Support: 113191                               *
      //                 Action Hyperlinks like Activate/Inactivate    *
      //                 should not be included as last column of excel*
      //                 spreadsheet export                            *
      //                 DSPF: Removed hyperlink scActivate and created*
      //                 2 new hyperlinks for Activate/Inactivate      *
      //                                                               *
      //  06/27/23 C:300 Project: WC2910                               *
      //                 Check User Security from Application Configuration
      //                 instead of User Parameters file               *
      //                                                               *
      //  06/23/23 C:300 Project: WC2910, Support: 111868              *
      //                 Display all screens in Inquiry mode, even if  *
      //                 user option security level does not allow them*
      //                 to Edit group information.                    *
      //                                                               *
      //  05/23/23 C:300 Project: WC2910                               *
      //                 - Add User Access Security                    *
      //                                                               *
      //  05/10/23 C:300 Project:W2196 Support:109128                  *
      //                 Do not reload when user re-visit same Tab, if *
      //                 screen is pending with un-saved changes       *
      //                                                               *
      // 01/17/22 C:291  Project: WC2788                               *
      //                 Add "Copy Configuration" option to Group      *
      //                 Toolbar.                                      *
      //                                                               *
      // ------------------------------------------------------------- *
      //  NOTE: Please add comments to the top of Revisions.           *
      // ------------------------------------------------------------- *
      //===============================================================*

     fWTGRPDATA cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflEDSM:rrnEDSM)
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdGrpPnl)
      * Company file
     fsmcol     if   e           k disk    rename(pco01:pco00)
      * Fund Master file
     fwmfnp     if   e             disk    rename(pfn01:pfn0p)
      * Policy Export Control File
     fwmpel     uf a e           k disk    rename(ppe01:ppe00)
     f                                     infds(dbfds2)
      * Group EDI Information File
     fwdf3p     uf a e             disk    rename(pf301:pf30p)
     f                                     infds(dbfds3)
     fwdf3l     if   e           k disk    rename(pf301:pf300)
     f                                     infds(dbfds)
      * Application System Control file
     fssylf     if   e           k disk
     f                                     ignore(SSY01)
     f                                     ignore(SPC01)
     f                                     ignore(SRC01)
     f                                     ignore(SBC01)
     f                                     ignore(SCS01)
     f*** Beg Add *** 05/23/23 *************************************************
     fsmupl     if   e           k disk
     fsdtbl1    if   e           k disk
     f*** End Add *** 05/23/23 *************************************************

     d psds          esds                  extname(szq1p)

     d dbfds           ds
     d  dbrrn                397    400b 0

     d dbfds2          ds
     d  dbrrn2               397    400b 0

     d dbfds3          ds
     d  dbrrn3               397    400b 0


      // work variables
     d saveDT          s                   like(FNdt)
     d saveTM          s                   like(FNtime)
     d saveDT2         s                   like(FNdt)
     d saveTM2         s                   like(FNtime)
     d flgRcdUpdated   s              1    inz
     d clientID        s              3    inz('000')
     d intCo#          s              3  0
     d inCo#           s              3  0
     d inFnd           s              3  0
     D indexTab        s              5  0
     d svrrnEDSM       s              9  0 inz
     D xxRrn           s              9  0 inz
     d chgMod          s               n   inz(*off)
     d dspMode         s               n   inz(*off)
     d error           s               n   inz(*off)
     d process         s               n   inz(*off)
     d charMsg         s            100
     d xxDelResp       s               n   inz
      ***  Begin Add  ***  05/10/23  *****************
     d t2              s              2  0 inz(*zeros)
     d t3              s              2  0 inz(*zeros)
     d* Array to store Tab index numbers, which is clicked by user before SAVE button
     d ClickTab        s              5  0 dim(4)
      ***  End   Add  ***  05/10/23  *****************
      *** Beg Add *** 05/23/23 *************************************************
       dcl-s  inDspFileName char(10);
       dcl-s  inPgmName     char(50);
       dcl-s  inTabName     char(50);
      *
     DarrTabNames      s            100    varying inz dim(20)
     dn1               s             10i 0 inz(1)
     dstrTabNames      s           2000    varying inz
      *** End Add *** 05/23/23 *************************************************
      //
     D ##Wcif          s              1
     D ##Mcif          s              1
     D ##Wcin          s              1
      //

      // Proto-types

      // Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3

     d GetSystem       pr                  extpgm('SA001R')
     d ##Wcif                         1
     d ##Mcif                         1
     d ##Wcin                         1

      * Initialize the Screen fields for EDI submission Tab
     d InzEDSMScVal    pr

      * Initialize the Screen fields for Policy Export Tab
     d InzPolExpScVal  pr

      // Load EDI submission Grid
     d LoadEDSMGrid    pr

      // Load Policy Export Tab
     d LoadPolExp      pr

      * Main Processing
     d Main            pr

      * Process Add EDI submission information
     d ProcessAddEDSM  pr

      * Process other Options for EDI submission information Grid
     d ProcOther       pr

      * Program Start
     d ProgramStart    pr
     d processMe                       n

     * Validate EDI submission info procedure.
     d ValidateEDSM    pr              n
     d strmode                        1

     * Validate Policy Export info procedure.
     d ValidatePolExp  pr              n


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
      //*** Beg Add ***** 01/17/22 *****************************
     d/copy sCopyGrpPr
      //*** End Add ***** 01/17/22 *****************************
      *** Beg Add *** 05/23/23 ************************************************
     d/copy sPrSecure
      *** End Add *** 05/23/23 ************************************************


      // Entry of Program.

     c     *entry        plist
     c                   parm                    @@rrn             9
     c                   parm                    @@mode            1
     c                   parm                    @@next           10
     c                   parm                    @@TabInd          5

     c     keyWdf3l      klist
     c                   kfld                    FNco#
     c                   kfld                    FNfnd
     c                   kfld                    scKstE

      /free
       If %parms >= 1;
          xxRrn = %int(@@rrn);
       Endif;

       Chain xxRrn pfn0p;
       inCo# = FNco#;
       inFnd = FNfnd;
       scCo# = FNco#;
       scFnd = FNfnd;

       GetClientID(clientID);
       scCltid = clientID;

       ProgramStart(process);

       If process;
          //*** Beg Add *** 05/23/23 ******************************************
          If TbNmRcdPnl <> *Blanks;
          //*** End Add *** 05/23/23 ******************************************

          Main();

          //*** Beg Add *** 05/23/23 ******************************************
          Else;
             LoadActionBar();
             isNoAccess = *on;
             TbNmRcdPnl = 'Security';
             pnlHeight = 560;
             aPnlHeight = 620;
             Write rcdGrpPnl;
             Exfmt rcdPanel;
             Read rcdGrpPnl;
             If scSelOpt <> *blanks;
                @@next = scSelOpt;
             Endif;
          Endif;
          //*** End Add *** 05/23/23 ******************************************
       Endif;


       *InLr = *On;
      /end-free

       //*** Beg Add *** 05/23/23 ******************************************
       //===================================================================
       // Add information to list
       //===================================================================
     PAddToList        b
     DAddToList        pi           101a   varying
     D inString                     100a   varying const
     D inArray                      100a   varying dim(20)
     D inIndex                       10i 0

     DoutString        s            101a   varying inz
      /Free
       If inIndex > 1;
          outString = ',';
       Endif;
       outString += inString;
       inArray(inIndex) = inString;
       inIndex += 1;
       Return outString;
      /End-free
     PAddToList        e
       //*** End Add *** 05/23/23 ******************************************

       //===================================================================
       // InzEDSMScVal - Initialize the Screen fields and indicators
       //===================================================================
     p InzEDSMScVal    b
     d InzEDSMScVal    pi

      /Free
       scCo# = intCo#;
       scFnd = FNfnd;
       scCltid = clientID;

       visFl09 = *Off;

      // Set visibility for Insurer Code, TPA Code, EDI Start Date Override
       Chain 'WC' SWC01;
       If %found() and WCfl09 = 'Y';
          visFl09 = *On;
       Endif;


       //=====================Below visibility codes need to be removed=====
       // visFl09 = *On;
       //=====================Above visibility codes need to be removed=====
      /End-Free
     p InzEDSMScVal    e

       //===================================================================
       // InzPolExpScVal - Initialize the Screen fields and indicators
       //===================================================================
     p InzPolExpScVal  b
     d InzPolExpScVal  pi

      /Free
       scCo# = intCo#;
       scFnd = FNfnd;
       scCltid = clientID;

      // Set visibility for "Default to Include in Policy Export Group Select"
       visbDftGrp = *Off;
       If clientID = '150';
          visbDftGrp = *On;
       Endif;


       //=====================Below visibility codes need to be removed=====
       // visbDftGrp = *On;
       //=====================Above visibility codes need to be removed=====

      /End-Free
     p InzPolExpScVal  e

       //===================================================================
       // Load EDI submission Grid
       //===================================================================
     p LoadEDSMGrid    b
     d LoadEDSMGrid    pi

      /free
        // Clear the subfile
        rrnEDSM = 0;
        dspEDSM = *off;
        clrEDSM = *on;
        Write rcdEDIInf;
        clrEDSM = *off;
        dspEDSM = *on;
        scRowSel = 1;

        If dspMode;
           inquiry_1 = *on;
        Endif;

        // Populate the subfile
        Setll (inCo#:inFnd) wdf3l;
        Reade (inCo#:inFnd) wdf3l;
        Dow not %eof(wdf3l);
           rrnEDSM += 1;
           scEDSMSel = *off;

           // set the selection on 1st record for first load
           If svrrnEDSM = *zeros;
              If rrnEDSM = 1;
                 scEDSMSel = *on;
              Endif;
           Else;
              If svrrnEDSM = rrnEDSM;
                 scEDSMSel = *on;
              Endif;
           Endif;

           scKst = F3kst;
           scNameES = F3name;
           scEDSMSts = *Blanks;

           //*** Begin Chg ***** 08/28/23 *****************************
           // scActivate = *Blanks;
           // If F3stat = 'A';
           //    scEDSMSts = 'Active';
           //    scActivate = 'Inactivate';
           // Endif;
           // If F3stat = 'I';
           //    scEDSMSts = 'Inactive';
           //    scActivate = 'Activate';
           // Endif;

           visActSts = *off;                // New Hyperlink for Activate
           visIActSts = *off;               // New Hyperlink for Inactivate
           If F3stat = 'A';
              scEDSMSts = 'Active';
              visIActSts = *on;
           Endif;
           If F3stat = 'I';
              scEDSMSts = 'Inactive';
              visActSts = *on;
           Endif;

           If dspMode;
              visActSts = *off;
              visIActSts = *off;
           Endif;
           //*** End Chg ***** 08/28/23 *****************************

           scRrnEDSM = dbrrn;

           Write sflEDSM;
        Reade (inCo#:inFnd) wdf3l;
        Enddo;

        rrnEDSM = 1;
        If svrrnEDSM <> 0;
           rrnEDSM = svrrnEDSM;
           scRowSel = svrrnEDSM;
           svrrnEDSM = 0;
        Endif;

      /end-free
     p LoadEDSMGrid    e

       //===================================================================
       // Load Policy Export Tab
       //===================================================================
     p LoadPolExp      b
     d LoadPolExp      pi

      /free

       Chain(n) (inCo#:inFnd) ppe00;
       If %found(wmpel);
          scEfDt  = dtMDYY(PEefdt);
          scExDt  = dtMDYY(PEexdt);
          scNet   = PEnet;
          scBnet  = PEbnet;
          scPtyc  = PEptyc;
          scEndo  = PEendo;
          scAdda  = PEadda;
          If PEdftGrpSl = *blank;
             PEdftGrpSl = 'E';
          Endif;
          scDftGrpSl = PEdftGrpSl;

          saveDT = PEdt;
          saveTM = PEtime;
       Endif;

      /end-free
     p LoadPolExp      e

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi
     d*
     d  isexit         s               n   Inz(*off)
      /free

       //LoadActionBar();

       BtnCancel = *off;

       // Process until exit button is pressed.
       DoW BtnCancel = *off;

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
             //***  Begin Del  ***  05/23/24  *****************
             //***  btnRefresh = *Off;
             //***  End   Del  ***  05/23/24  *****************

          // (Enter key or Save button)
          When btnSave = *on;
             btnSave = *off;
             If @@mode <> 'D';
                // Check if WMPEP updated since screen displayed (Policy Export)
                flgRcdUpdated = RcdUpdatedErr('WMPEP':dbrrn2:'PEdt':'PEtime':
                 'PEuser':saveDT:saveTM);

                if flgRcdUpdated = '0'; // No error

                   // Validate Policy Export (Tab 1)
                   If ValidatePolExp();
                      Exsr UpdPolExp;
                   Else;
                      error = *on;
                   EndIf;

                Else;
                   error = *on;
                EndIf;

                // Show Data Saved Sucessfully Message
                If error = *off;
                   PopDataSaved();

                   //*** Beg Add ***** 05/10/23 *****************************
                   Clear ClickTab;
                   Clear t2;
                   //*** End Add ***** 05/10/23 *****************************
                EndIf;
             Endif;

          // Add EDI Submission Info Button
          When BtnEDSMAdd = *On;
             BtnEDSMAdd = *Off;
             ProcessAddEDSM();

          //*** Beg Add ***** 01/17/22 *******************************
          // Copy Configuration
          When btnShowWin = *on;
             PopupWindow(txtShowWin:scActRRN:'GS':txtWinData);
             btnShowWin = *off;
          //*** End Add ***** 01/17/22 *******************************

          //*** Beg Add *** 05/23/23 ******************************************
          // Add User Access Window Logic
          When btnUsrAcc = *on;
             inDspFileName = 'WTGRPDATA';
             inPgmName     = 'Group Configuration - Data Reporting';
             inTabName     =  arrTabNames(TabIndex + 1);
             UserAccessWdw(inDspFileName:inPgmName:inTabName);
             btnUsrAcc = *off;
          //*** End Add *** 05/23/23 ******************************************

          // Menu Choices
          When scSelOpt <> *blanks;
             @@next = scSelOpt;
             Leave;


          Other;
             procOther();                   // EDI Submission Edit

          Endsl;
       Enddo;

       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       indexTab = TabIndex + 1;

       Select;
       //*** Beg Chg *** 05/23/23 ******************************************
       //*** When indexTab = 1;
       When arrTabNames(TabIndex+1) = 'Policy Export';
       //*** End Chg *** 05/23/23 ******************************************
          pnlHeight = 500;
          APnlHeight = pnlHeight + 60;

          //***  Begin Add  ***  05/10/23  *****************
          t3 = 0;
          t3 = %lookup(indexTab:ClickTab);
          //***  Begin Chg  ***  05/23/24  *****************
          //***  If t3 = 0;
          //***     t2 = t2 + 1;
          //***     ClickTab(t2) = indexTab;
          If t3 = 0 or btnRefresh = *On;
             btnRefresh = *Off;
             If t3 = 0;
                t2 = t2 + 1;
                ClickTab(t2) = indexTab;
             EndIf;
          //***  End   Chg  ***  05/23/24  *****************
          //***  End   Add  ***  05/10/23  *****************

          If error = *off;
             Clear rcdPolExp;

            // Load initial values
             InzPolExpScVal();

            // Load Policy Export
             LoadPolExp();
          Endif;

          //***  Begin Add  ***  05/10/23  *****************
          Endif;
          //***  End   Add  ***  05/10/23  *****************

          If dspMode;
             inquiry = *on;
          Endif;

          Write rcdGrpPnl;
          Write rcdPanel;
          Exfmt rcdPolExp;
          Read  rcdGrpPnl;
          Read rcdPanel;

       //*** Beg Chg *** 05/23/23 ******************************************
       //*** When indexTab = 2;
       When arrTabNames(TabIndex+1) = 'EDI Submission Info';
       //*** End Chg *** 05/23/23 ******************************************
          pnlHeight = 500;
          APnlHeight = pnlHeight + 60;
          //***  Begin Add  ***  05/23/24  *****************
          btnRefresh = *Off;
          //***  End   Add  ***  05/23/24  *****************

          Clear rcdEDIInf;

          // Load initial values
          InzEDSMScVal();

          // Load EDI submission Grid
          LoadEDSMGrid();

          If dspMode;
             inquiry_1 = *on;
          Endif;

          Write rcdGrpPnl;
          Write rcdPanel;
          Exfmt rcdEDIInf;
          Read  rcdGrpPnl;
          Read rcdPanel;

       EndSl;

       Endsr;

       // Subroutine to move Screen fields into database fields.
       BegSr MoveScDb;
        PEefDt  = dtCYMD(scEfDt);
        PEexDt  = dtCYMD(scExDt);
        PEnet   = scNet;
        PEbnet  = scBnet;
        PEptyc  = scPtyc;
        PEendo  = scEndo;
        PEadda  = scAdda;
        PEdftGrpSl = scDftGrpSl;
       EndSr;

       // Update Policy Export Tab subroutine
       BegSr UpdPolExp;
        // Load Policy Export Info
        Chain (inCo#:inFnd) ppe00;
        Exsr MoveScDb;
        PEuser  = q1user;                // User
        PEpgm   = q1pgm;                 // Program
        PEtime  = Timeto6(SystemTime()); // Time
        PEdt    = Dateto7(SystemDate()); // Date Mintained
        saveDT = PEdt;
        saveTM = PEtime;
        If %found(wmpel);
           Update ppe00;
        Else;
           PEco#   = FNco#;
           PEfnd   = FNfnd;
           PEiusr  = q1user;                // Initial User
           PEipgm  = q1pgm;                 // Initial Program
           PEidt   = Dateto7(SystemDate()); // Initial Date
           Write ppe00;
        EndIf;
       EndSr;


      /end-free
     p Main            e

       //===================================================================
       // ProcessAddEDSM - process addition of Reserve Cushion
       //===================================================================
     P ProcessAddEDSM  B
     D ProcessAddEDSM  PI
     d*
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)

      /free

       Clear rcdAddEDSM;
       mode = 'A';
       isexit = *off;
       BtnEDSMCan = *off;
       EDSMEdt = *off;
       scEDSMStE = 'A';

       Dow BtnEDSMCan = *off and isexit = *off;

          // Loading initial values
          InzEDSMScVal();

          scEDSMHdg = 'Add EDI Submission Information';
          visEDISvAd = *On;                           // Show Save/Add next Button

          Exfmt rcdAddEDSM;

          Select;
          // Save/Add Next Button
          When BtnEDISvAd = *on;
             BtnEDISvAd = *off;
             If ValidateEDSM(mode);
                Exsr WrtEDSM;
               // Initialize Screen fields for re-load
                Clear rcdAddEDSM;
                BtnEDSMCan = *off;
                EDSMEdt = *off;
                scEDSMStE = 'A';
             EndIf;

          // Save Button
          When BtnEDSMSav = *on;
             BtnEDSMSav = *off;
             If ValidateEDSM(mode);
                Exsr WrtEDSM;
                isexit = *on;
             EndIf;

          // Cacel Button
          When BtnEDSMCan= *on;
             BtnEDSMCan= *off;
             isexit = *on;

          EndSl;
       Enddo;

       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        F3co#  = scCo#;                 // Company Number
        F3fnd  = FNFnd;                  // Group Number
        F3Kst  = scKstE;               // State
        F3fein = scFEINE;              // FEIN
        F3name = scNameE;               // Name
        F3adr1 = scAdr1E;              //Address1
        F3adr2 = scAdr2E;              //Address2
        F3cty  = scCtyE;              // city
        F3st   = scstEES;               // State
        F3zip  =  scZipE;               // Zip
        If scPh#E <> *Zeros;
           F3ph# = %editc(scPh#E:'X');
        Else;
           F3ph# = *Blanks;
        Endif;
        If scFaxE <> *Zeros;
           F3fax = %editc(scFaxE:'X');
        Else;
           F3fax = *Blanks;
        Endif;

        F3azp  =  scAzpE;               // Alternate Zip
        F3info = scInfoE;               // claim admin attention
        F3ifei = scIfeiE;               // insurer fein
        F3inam = scInamE;              // insurer name
        F3icod = scIcodE;              // insurer code
        F3tcod = scTcodE;              // TPA code
        F3stat = scEDSMStE;           // Status
        F3ovdt = dtCYMD(scOvdtE);       // Edi statrt override date

        F3user = q1user;
        F3pgm  = q1pgm;
        F3time = Timeto6(SystemTime());
        F3dt   = Dateto7(SystemDate());
        F3iUsr = q1user;
        F3iPgm = q1pgm;
        F3iTim = Timeto6(SystemTime());
        F3iDt  = Dateto7(SystemDate());
       EndSr;

       // Writing EDI submission details subroutine
       BegSr WrtEDSM;
        Clear pf30p;
        Exsr MoveScDb;
        Write pf30p;
       EndSr;

      /end-free
     P ProcessAddEDSM  E

       //===================================================================
       // ProcOther - Process other Options for EDI Submission Grid
       //===================================================================
     P ProcOther       B
     D ProcOther       PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdAddEDSM;
       isexit = *off;
       ReadC sflEDSM;

       Dow not %eof;

          Select;
          When ScEDSMEdt = *On; // Processing for Edit EDI submission Link

             // Loading initial values
             InzEDSMScVal();

             ScEDSMEdt = *Off;
             mode = 'C';
             EDSMEdt = *on ;   //disable non-editable field
             svrrnEDSM = rrnEDSM;     //Save Rrn value for edited record

             Chain(n) scRrnEDSM WDF3P;

             Exsr MoveDbSc;

             scEDSMHdg = 'Edit EDI Submission Information';

             Dow isexit = *off;
                Exfmt rcdAddEDSM;

                Select;
                // Savd Button
                When BtnEDSMSav = *on;
                   BtnEDSMSav = *off;
                   // Check if WDF3P updated since screen displayed (EDI Submission Info)
                   flgRcdUpdated=RcdUpdatedErr('WDF3P':dbrrn3:'F3dt':'F3time':
                    'F3user':saveDT2:saveTM2:'1');
                   if flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scRrnEDSM WDF3P;
                      Exsr MoveDbSc;
                   EndIf;
                   if flgRcdUpdated = '0'; // No error
                      If ValidateEDSM(mode);
                         Chain scRrnEDSM WDF3P;
                         Exsr MoveScDb;
                         Update pf30p;
                         isexit = *on;
                      EndIf;
                   EndIf;

                // Cacel Button
                When BtnEDSMCan= *on;
                   BtnEDSMCan = *off;
                   isexit = *on;
                EndSl;
             EndDo;

          When ScEDSMDlt = *On; // Processing for Delete EDI submission Link

             ScEDSMDlt = *Off;

             Chain(e) scRrnEDSM WDF3P;
             If %status = 1218;
                PopOneBtn('Record locked by another user.  Please try again-
                 later.':'Record Lock');
                Return;
             Endif;

             Chain scRrnEDSM WDF3P;
             Exsr MoveDbSc;

             scEDSMHdg = 'Delete EDI Submission Info';

             Dow isexit = *off;

                Write rcdAddEDSM;
                xxDelresp = PopTwoBtn('Do you want to delete this EDI +
                Submission Information?':'Confirm Delete':'Yes':'No');
                Select;
                When xxDelResp = *On;
                   Delete pf30p;
                   PopOneBtn('EDI submission Information +
                   Deleted!':'Delete':'Ok');
                   isexit = *on;
                When xxDelResp = *Off;
                   svrrnEDSM = rrnEDSM;     //Save Rrn value
                   isexit = *on;
                EndSl;
             EndDo;
          Endsl;

          Unlock WDF3P;

          ReadC sflEDSM;
       Enddo;


       // Subrotine to move database fields into screen fields
       BegSr MoveDbSc;
        scKstE = F3kst;
        scFEINE = F3fein;
        scNameE= F3name;
        scAdr1E = F3adr1;
        scAdr2E = F3adr2;
        scCtyE = F3cty;
        scStEES = F3st;
        scZipE = F3zip;
        If F3ph# <> *Blanks;
           scPh#E = %dec(F3ph#:15:0);
        Else;
           scPh#E = *Zeros;
        Endif;
        If F3fax <> *Blanks;
           scFaxE = %dec(F3fax:10:0);
        Else;
           scFaxE = *Zeros;
        Endif;
        scAzpE = F3azp;
        scInfoE = F3info;
        scIfeiE = F3ifei;
        scInamE = F3inam;
        scIcodE = F3icod;
        scTcodE = F3tcod;
        scOvdtE = dtMDYY(F3ovdt);
        scEDSMStE = F3stat;
        saveDT2 = F3dt;
        saveTM2 = F3time;
       EndSr;

       // Subrotine to move Screen fields into database fields
       BegSr MoveScDb;
        F3fein = scFEINE;              // FEIN
        F3name = scNameE;               // Name
        F3adr1 = scAdr1E;              //Address1
        F3adr2 = scAdr2E;              //Address2
        F3cty = scCtyE;              // city
        F3st = ScstEES;               // State
        F3zip = ScZipE;               // Zip
        If scPh#E <> *Zeros;
           F3ph# = %editc(scPh#E:'X');
        Else;
           F3ph# = *Blanks;
        Endif;
        If scFaxE <> *Zeros;
           F3fax = %editc(scFaxE:'X');
        Else;
           F3fax = *Blanks;
        Endif;
        F3azp = scAzpE;               // Alternate Zip
        F3info = scInfoE;               // claim admin attention
        F3ifei = scIfeiE;               // insurer fein
        F3inam = scInamE;              // insurer name
        F3icod = scIcodE;              // insurer code
        F3tcod = scTcodE;              // TPA code
        F3stat = scEDSMStE;           // Status
        F3ovdt = dtCYMD(scOvdtE);       // Edi statrt override date

        F3user = q1user;
        F3pgm  = q1pgm;
        F3time = Timeto6(SystemTime());
        F3dt   = Dateto7(SystemDate());
        saveDT2 = F3dt;
        saveTM2 = F3time;
       EndSr;

      /end-free
     P ProcOther       E

       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi
     d processMe                       n
      /free

       // Program Mode.
       If q1prms >= 2;
          //*** Beg Add ***** 01/17/22 *****************************
          scActMode = @@mode;
          //*** End Add ***** 01/17/22 *****************************
          Select;
          When @@mode = 'D';
             dspMode = *on;
          When @@mode = 'C';
             chgMod  = *on;
          Endsl;
       Endif;

       apShowMenu = *on;

       // Set Menu options
       scMenuChoi = 'Primary Group Information';
       scMenuValu = 'WTGRPPRI';

       scMenuChoi = %trim(scMenuChoi) + ',Administration / DARTS Configuration';
       scMenuValu = %trim(scMenuValu) + ',WTGRPADMIN';

       scMenuChoi = %trim(scMenuChoi) + ',Policy Configuration';
       scMenuValu = %trim(scMenuValu) + ',WTGRPPOLCG';

       scMenuChoi = %trim(scMenuChoi) + ',Claims Configuration';
       scMenuValu = %trim(scMenuValu) + ',WTGRPCLMCG';

       scMenuChoi = %trim(scMenuChoi) + ',State Specific Information';
       scMenuValu = %trim(scMenuValu) + ',WTGRPSSI';

       scMenuChoi = %trim(scMenuChoi) + ',Data Reporting';
       scMenuValu = %trim(scMenuValu) + ',WTGRPDATA';

       aMenHeight = 145;

       apFootHght = aMenHeight + 125;

       // get the system values
       GetSystem(##Wcif:##Mcif:##Wcin);


       // Tab Index
       tabIndex = 0;
       If q1prms >= 4;
          If @@TabInd <> ' ';
             tabIndex = %int(@@TabInd);
             If tabIndex < 0 or tabIndex > 1;
                TabIndex = 0;
             Endif;
          Endif;
          @@TabInd = '00000';
       Endif;

       intCo# = GetUserInfo(q1user);
     // Update Action Panel Info (Rrn, Group Name, Group Number)
       scActRrn = xxRrn;
       scCompName = %trim(FNname);
       scGrpNum = FNfnd;
       If scCompName = ' ';
          Chain intCo# pco00;
          If COname <> ' ';
             scCompName = COname;
          Endif;
       Endif;

       scCo# = intCo#;

       processMe = *on;

       //*** Beg Add *** 05/23/23 ******************************************
       isNoAccess = *off;

       // Set User Access button visibility indicator based on Users Security
       Chain (Q1user) pup01;
       //*** Beg Chg *** 06/27/23 ******************************************
       //*** If %Found(SMUPL) and UPlvl = 999 and UPtype = '*';
       // based on Users Security Type and Level defined in Application Confguration
       If CheckSecLevel('GrpCfgAut':UPtype:UPlvl);
       //*** End Chg *** 06/27/23 ******************************************
          isUsrAcc = *on;
       Else;
          isUsrAcc = *off;
       Endif;

       //*** Beg Add *** 06/23/23 ******************************************
       // Display Group Information in Inquiry Mode even if Users Security
       // level does not allow them to Edit Group Information.
       If dspMode = *on;
          isUsrAcc = *on;
       Endif;
       //*** End Add *** 06/23/23 ******************************************

       // Determine if user is in the Group Config Tab file for each tab.  This
       // will determine which tabs are displayed.  If the user has 999 * then
       // all tabs are displayed

       // Policy Export tab
       Chain (intCo#:'WTGRPDATA':'Group Configuration - Data Reporting':
        'Policy Export':Q1user) pTB01;
       If %Found(SDTBL1) or isUsrAcc = *on;
          strTabNames += AddToList('Policy Export':arrTabNames:n1);
       Endif;

       // EDI Submission Info Tab
       Chain (intCo#:'WTGRPDATA':'Group Configuration - Data Reporting':
        'EDI Submission Info':Q1user) pTB01;
       If %Found(SDTBL1) or isUsrAcc = *on;
          strTabNames += AddToList('EDI Submission Info':arrTabNames:n1);
       Endif;

       TbNmRcdPnl = strTabNames;
       //*** End Add *** 05/23/23 ******************************************

       Return;


      /end-free
     p ProgramStart    e

      //===================================================================
      // Validate EDI Submission Details addition information.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateEDSM    b
     d ValidateEDSM    pi              n
     d strmode                        1
     d isError         s               n   inz
      /Free

       isError = *off;
      // Initialize all the messageId and error indicators.
       erKstE= *off;
       msKstE = *blanks;
       erOvdtE= *off;
       msOvdtE = *blanks;
       erAdr1E = *off;
       msAdr1E = *blanks;
       erCtyE = *off;
       msCtyE = *blanks;
       erZipE = *off;
       msZipE = *blanks;
       erStEES = *off;
       msStEES = *blanks;

       If strmode <> 'C';
         // Validate State
          Chain(n) keyWdf3l pf300;
          If %found(wdf3l);
             erKstE = *on;
             msKstE = 'WC30005';
             isError = *on;
          Endif;
       Endif;

      // Validation Check for EDI override date
       If scOvdtE <> *Zeros;
          erOvdtE = ValidDate(scOvdtE:charMsg);
          If erOvdtE = *on;
             msOvdtE = charMsg;
             isError = *on;
          Endif;
       Endif;

      // Enter complete info for Mailing address, if any of the field is entered
       If scAdr1E <> *blanks or scAdr2E <> *blanks or
        scCtyE <> *blanks or scStEES <> *blanks or scZipE <> 0;
          If scAdr1E = *Blanks;
             erAdr1E = *on;
             msAdr1E = 'WCR0503';
             isError = *on;
          Endif;
          If scCtyE = *blanks;
             erCtyE = *on;
             msCtyE = 'WCR0503';
             isError = *on;
          Endif;
          If scStEES = *blanks;
             erStEES = *on;
             msStEES = 'WCR0503';
             isError = *on;
          Endif;
          If scZipE = *zeros;
             erZipE = *on;
             msZipE = 'WCR0556';
             isError = *on;
          Endif;
       Endif;


      // Return *on indicator when No error exists.
       If isError = *on;
          return *off;
       Else;
          return *on;
       Endif;

      /End-Free
     p ValidateEDSM    e

      //===================================================================
      // Validate Policy Export information.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidatePolExp  b
     d ValidatePolExp  pi              n
     d isError2        s               n   inz
      /Free

       isError2 = *off;
      // Initialize all the messageId and error indicators.
       erEfDt = *off;
       msEfDt = *blanks;
       erExDt = *off;
       msExDt = *blanks;


      // Validation Check for Effective Date
       If scEfDt <> 0;
          erEfDt = ValidDate(scEfDt:charMsg);
          If erEfDt = *on;
             msEfDt = charMsg;
             isError2 = *on;
          Endif;
       Endif;

      // Validation Check for Expiration Date
       If scExDt <> 0;
          erExDt = ValidDate(scExDt:charMsg);
          If erExDt = *on;
             msExDt = charMsg;
             isError2 = *on;
          Endif;
       Endif;

      // Dates must be in Ascending order
       If scEfDt <> 0 and scExDt <> 0;
          If erExDt = *off and erEfDt = *off;
             erExDt = AscendDates(scEfDt:scExDt:charMsg);
             If erExDt = *on;
                erEfDt = *on;
                msExDt = charMsg;
                msEfDt = charMsg;
                isError2 = *on;
             Endif;
          Endif;
       Endif;

      // Return *on indicator when No error exists.
       If isError2 = *on;
          return *off;
       Else;
          return *on;
       Endif;

      /End-Free
     p ValidatePolExp  e


      // utility procedure definitions
      /COPY SCOPYCOMPI

