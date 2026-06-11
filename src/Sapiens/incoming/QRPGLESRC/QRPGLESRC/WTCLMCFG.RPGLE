     Hcopyright('(c) 2023 Sapiens. All rights reserved.')
     H**********************************************************************
     H/TITLE WTCLMCFG - Claims Configuration.
     H DFTNAME(WTCLMCFG)
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO) ACTGRP(*caller)
     H/DEFINE   PROFOUNDUI
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
      *** Check for 'Continue here'
      *====================================================================*
      *  Created   : 12/01/14                                              *
      *  Programmer: C:279                                                 *
      *  Project   : 114WLF                                                *
      *====================================================================*
      *  Revisions:                                                        *
      *                                                                    *
      *  03/30/23 C:340 Project: WC2430                                    *
      *                 Changes to General Tabs                            *
      *                 # Add PV2 Code for Payment Entry                   *
      *                 # Add Payment Section for Security Type/Level Sets *
      *                                                                    *
      *  03/02/23 C:079 Project: 146WKK                                    *
      *                 Add Import/Export Settings tab                     *
      *                                                                    *
      *  08/31/22 C:291 Project: WC2852                                    *
      *                 Fix Multiple Claim headers to not controlled by    *
      *                 Reserve Worksheet module flag. Recompile only.     *
      *                 Display file Changed: Remove visibility flag       *
      *                 ShowClmCfg from the line, Multiple claim section   *
      *                 header and Add/Edit a Multi-claim header.          *
      *                                                                    *
      *  06/22/22 C:079 Support: 102638                                    *
      *                 Load Action Panel Items using service pgm          *
      *                                                                    *
      *  05/09/22 C:079 Project: 129WWV                                    *
      *                 Application Configuration Security                 *
      *                                                                    *
      *  03/15/22 C:333 Project: SY434                                     *
      *                 Update Grids for Sorting Filtering Upgrade         *
      *                                                                    *
      *  05/27/21 C:318 Support:95270                                      *
      *                 Changes for inquiry mode to make fields disable    *
      *                 and links invisible                                *
      *                                                                    *
      *  01/22/21  C:180 ProjectL WC2732                                   *
      *                  Multiple claims accident screen/CS standards      *
      *                                                                    *
      *  03/12/19  C:295 Support: 77377                                    *
      *                  Removed disable functionality from Override       *
      *                  Inquiry mode check box to make it independent of  *
      *                  SSYPF/SYFL12 flag.                                *
      *                                                                    *
      *  03/04/19  C:33  Project SY390                                     *
      *                  Soft-code 'Portal'                                *
      *                                                                    *
      * 08/23/18 C:062 Project: SY390                                      *
      *                Add Portal Configuration option to the menu.        *
      *                                                                    *
      * 05/14/18 C:291 Project: 150WCR                                     *
      *                Add Policy Configuration option to the menu.        *
      *                                                                    *
      * 01/23/18 C:062 Project: 129WUV                                     *
      *                Add Inquiry Authority Override processing           *
      *                                                                    *
      * 04/19/17 C:180 Support: 46840                                      *
      *                Use Application Configuration Action Panel.         *
      *                                                                    *
      * 02/08/16 C:306 Project: WC2403                                     *
      *                Added a WMCNP file population.                      *
      *                                                                    *
      * 01/07/16 C:65  Project: WC2401                                     *
      *                Recompile.                                          *
      *                                                                    *
      *  10/09/15 C:306 Project: 114WVN                                    *
      *                 Load the benifit Editable fields value from Data-  *
      *                 base.                                              *
      *                                                                    *
      *  09/17/15 C:302 Project: wc2254                                    *
      *                 Recompile only                                     *
      *                                                                    *
      *  03/31/15 C:300 Project: 114WLF Priss #37                     *
      *                 Maintain Require Reserve Worksheet data, when *
      *                 we add/edit in State Heading Grid without save*
      *                                                                    *
      *  03/13/15 C:300 Project: 114WLF Priss #35                     *
      *                 In case of Validation error, do not load      *
      *                 screen and subfile                            *
      *                                                                    *
      * -------------------------------------------------------------------*
      *  NOTE: Please add comments to the top of Revisions.                *
      * -------------------------------------------------------------------*
      *====================================================================*

     fwtclmcfg  cf   e             workstn extfile('WTCLMCFG')
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflStateHd:rrnHdgSt)
     fstactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdSysTbl)
     fwmcwl     uf a e           k disk    rename(pcw01:pcw0c)
     f                                     infds(dbfds1)
     fwdcwl     uf a e           k disk    rename(pcw01:pcw0s)
     f                                     infds(dbfds2)
     fsmcol     uf   e           k disk    rename(pco01:pco0s)
     fsdacp     uf a e             disk    rename(pac01:pac0p)
     fsdacl     if   e           k disk    rename(pac01:pac00)
     f                                     infds(dbfds)
     fsmupl     if   e           k disk
     fsdtbl1    if   e           k disk
     f*** Beg Add *** 03/02/23 ************************************************
     fwdimexl1  uf a e           k disk
     f*** End Add *** 03/02/23 ************************************************
      *
     d dbfds           ds
     d  dbrrn                397    400b 0

      * system data structure
     d psds          esds                  extname(szq1p)

     d dbfds1          ds
     d  rrnwmcwl             397    400b 0

     d dbfds2          ds
     d  rrnwdcwl             397    400b 0

      * work variables
     d intco#          s                   like(CWco#)
     d mode            s              1
     d strmode         s              1    inz
     d isValid         s              1n   inz(*off)
     d isLoadSfl       s              1n   inz(*off)
     D flgRcdUpdated   s              1
     d saveDT          s                   like(CWdt)
     d saveTM          s                   like(CWtime)
     d strKey          s             10a   inz
     d intRrn          s              9  0
     d intRrn1         s              9  0
     d intRrn2         s              9  0
      *** Beg Add *** 03/30/23 *** WC2430 ***********************************
     d intRrn3         s              9  0
     d intRrn4         s              9  0
     d intRrn5         s              9  0
     d intRrn6         s              9  0
     d intRrn7         s              9  0
     d intRrn8         s              9  0
      *** End Add *** 03/30/23 *** WC2430 ***********************************
       dcl-s  inDspFileName char(10);
       dcl-s  inPgmName     char(50);
       dcl-s  inTabName     char(50);
       dcl-s  index         zoned(3:0);
     d namesArr        s            100    DIM(20)
     d strMenuChoi     s            500    Varying
     d strMenuValu     s            100    Varying

      * Add Reserve Worksheet Setup
     d AddReserve      pr

      * Clear Subfiles
     d ClrSubFiles     pr

      * Load Require Reserve Worksheet Information
     d LoadWorksheet   pr

      * Load State Heading Setup Information
     d LoadStateGrid   pr

     d*** Beg Add *** 03/02/23 ************************************************
      * Load Import/Export Settings
     d LoadImpExpInfo  pr
     d*** End Add *** 03/02/23 ************************************************

      * main Processing
     d Main            pr

      * Move database record to screen
     d MoveDbSc        pr

      * process addition of contact
     d ProcessAdd      pr

      * Process Other (Edit)
     d ProcOther       pr

      * Update Reserve Worksheet Company Wide
     d UpdReserve      pr

      * Validate Require Reserve Worksheet screen
     d Validate        pr

     d/copy sPrDate
     d/copy wPrUsrPrms
     d/copy sPrModule
     d/copy sPrCompany
     d/copy sPrGenPop
     d/copy sPrSecure
     d/copy sPrActPnl
     d*** Beg Add *** 03/02/23 ************************************************
     d/copy sPrGenErr
     d*** End Add *** 03/02/23 ************************************************
     d Rtvsysval       pr                  ExtPgm('SA042R')
     d  ##CMSf                        1

     d  ##CMSf         s              1

      * Entry of Program.
     dwtClmCfgPgm      pr                  extpgm('WTCLMCFG')
     d                                9
     d                                1
     d                               10
     d                                5
     dwtClmCfgPgm      pi
     d@@rrn                           9
     d@@mode                          1
     d@@next                         10
     d@@TabInd                        5

      /free

       // start program processing

       // Program Mode.
       If %parms >= 2;
          mode = @@mode;
          Select;
          When @@mode = 'D';
             dspMode = *on;
          Endsl;
       Endif;

       // Tab Index Mode.
       TabIndex = 0;

       // Get Company no.
       intCo# = UserCo#(q1user);

       // Get Company name
       scCompName = getCompNam(intCo#);

       // Panel Heading
       scPnlHdg = 'Claims Configuration';

       // Check if Reserve Worksheet Module is not installed or not
       DspHdgAdd  = *off;

       ShowClmCfg = *off;
       ShowClmCfg = IsModEnabled('CLAIMS WORKSHEET');

       //Check for applicable options

       //  Applicable options


       GetActPnlItems(strMenuChoi:strMenuvalu);
       scMenuChoi = strMenuChoi;
       scMenuValu = strMenuValu;

       ShowErrmsg = *off;
       If ShowClmCfg = *off;
          ShowErrmsg = *on;
       EndIf;

       DsblClmCfg = *off;
       If mode = 'D';
          DsblClmCfg = *on;
       EndIf;

       RtvSysVal(##CMSf);

       isDsblGen  = *off;
       ShowGnlCfg = *on;

       If ##CMSf <> 'Y';
          isDsblGen  = *on;
          strHelp    = 'The CMS module is not installed for your use at this ' +
                       'time for more information on this module, '  +
                        'please contact your StoneRiver representative.';
       EndIf;
       If mode = 'D';
          isDsblGen  = *on;
          dsbMC = *on;
       Endif;
       LoadTabInfo();
       // main processing
       If index <> 0;
          Main();
       Else;
          scCompName = getCompNam(intCo#);
          isNoAccess = *on;
          TabNames = 'Security';
          TabHeight = 560;
          APnlHeight = TabHeight + 60;
          apShowMenu = *on;
          Write rcdSysTbl;
          Exfmt rcdPanel;
          Read rcdSysTbl;
          If scSelOpt <> *blanks;
             @@next = scSelOpt;
          Endif;
       Endif;

       Unlock WMCWL;
       Unlock WDCWL;
       *InLr = *On;
       Return;

      /end-free
       //===================================================================
       // ClrSubFiles - Clears SubFiles
       //===================================================================
     P ClrSubFiles     B
     D ClrSubFiles     PI
      /Free

       rrnHdgSt  = 0;
       clrHdgSt  = *On;
       dspHdgSub = *Off;
       dspHdgCtl = *Off;
       Write RcdReqRsvW;

      /End-Free
     P ClrSubfiles     E
       //===================================================================
       // Load Require Reserve Worksheet Information
       //===================================================================
     P LoadWorksheet   B
     D LoadWorksheet   PI
      /free

       Chain(n) intCo# pcw0c;
       If %found(WMCWL);

          // Write Require Reserve Worksheet Information
          scMedClm   = CWmofl;
          scIndClm   = CWinfl;
          scMedAmt   = CWmoth;
          scIndAmt   = CWinth;
          scUserFlag = CWdisa;
          scAppCode  = CWracd;
          scRmkCode  = CWrmcd;

          saveDT = CWdt;
          saveTM = CWtime;

       Endif;

      /end-free
     P LoadWorksheet   E
       //*** Beg Add *** 03/02/23 *********************************************
       //===================================================================
       // Load Import/Export Settings Information
       //===================================================================
     P LoadImpExpInfo  B
     D LoadImpExpInfo  PI

       scFimEmAd = *blanks;
       scTimEmAd = *blanks;
       scImSub   = *blanks;
       scImbody  = *blanks;

       Chain(n) intCo# pime01;
       If %Found(WDIMEXL1);
          scFimEmAd = IMEfImEmAd;
          scTimEmAd = IMEtImEmAd;
          scImSub   = IMEImSub;
          scImbody  = IMEImBody;
       Endif;

     P LoadImpExpInfo  E
       //*** End Add *** 03/02/23 *********************************************
       //===================================================================
       // Load Tab Information
       //===================================================================
     P LoadTabInfo     B
     D LoadTabInfo     PI
       // Set User Access button visibility indicator based on Users Security
       Chain (Q1user) pup01;
       If %Found(SMUPL) and UPlvl = 999 and UPtype = '*';
          isUsrAcc = *on;
       Else;
          isUsrAcc = *off;
       Endif;

       // Determine if user is in the App Config Tab file for each tab.  This
       // will determine which tabs are displayed.  If the user has 999 * then
       // all tabs are displayed

       // Reserve Worksheet tab
       Chain (intCo#:'WTCLMCFG':'Claims Configuration':'Reserve Worksheet'
        :Q1user) pTB01;
       If %Found(SDTBL1) or isUsrAcc = *on;
          TabNames = 'Reserve Worksheet';
          index = 1;
          namesArr(index) = 'Reserve Worksheet';
       Endif;

       // General tab
       Chain (intCo#:'WTCLMCFG':'Claims Configuration':'General'
        :Q1user) pTB01;
       If %Found(SDTBL1) or isUsrAcc = *on;
          If index = 0;
             TabNames = 'General';
          Else;
             TabNames = %Trim(TabNames) + ', General';
          Endif;
          index = index + 1;
          namesArr(index) = 'General';
       Endif;
       //*** Beg Add *** 03/02/23 *********************************************
       // Import/Export Tab
       Chain (intCo#:'WTCLMCFG':'Claims Configuration':'Import/Export'
        :Q1user) pTB01;
       If %Found(SDTBL1) or isUsrAcc = *on;
          If index = 0;
             TabNames = 'Import/Export';
          Else;
             TabNames = %Trim(TabNames) + ', Import/Export';
          Endif;
          index = index + 1;
          namesArr(index) = 'Import/Export';
       Endif;
       //*** Endif *** 03/02/23 *********************************************

     P LoadTabInfo     E
       //*** End Add *** 05/09/22 **********************************************
       //===================================================================
       // Load State Heading Setup Information
       //===================================================================
     P LoadStateGrid   B
     D LoadStateGrid   PI
      /free

       // clear the subfile.
       clrHdgSt  = *Off;
       dspHdgSub = *On;
       dspHdgCtl = *On;

       If ShowClmCfg = *on;
          Select;
          When mode = 'D';
             dspHdgEdt = *Off;
             dsbedit   = *On;
             dspHdgAct = *On;
          When mode = 'A' or mode = 'C';
             dspHdgEdt = *On;
             dsbedit   = *Off;
             dspHdgAdd = *On;
             dsbadd   = *Off;
          Endsl;
       Endif;

       Setll intCo# pcw0s;
       Reade(n) intCo# pcw0s;
       Dow not %eof(WDCWL);

          // State
          scHdgState = CWst;
          If scHdgState = *blanks;
             scHdgState = 'Default';
          EndIf;
          // Status
          If CWstat = 'A';
             scActivate = 'Inactivate';
             scActivat1 = 'Active';
          Else;
             scActivate = 'Activate';
             scActivat1 = 'Inactive';
          Endif;
          scResRRN# = rrnwdcwl;

          rrnHdgSt = rrnHdgSt + 1;

          If rrnHdgSt = 1;
             scAgnRowSl = 'A';
          Else;
             clear scAgnRowSl;
          Endif;

          Write sflStateHd;
          Reade(n) intco# pcw0s;
       Enddo;

       rrnHdgSt = 1;

     P LoadStateGrid   E
       //===================================================================
       // Main - Main processing
      //===================================================================
     P Main            B
     D Main            PI

      /free

       ExitNow = *Off;
       isValid = *Off;
       isLoadSfl = *Off;

       // Process until exit button is pressed.
       Dow ExitNow = *Off;

             If isValid = *Off or isLoadSfl = *On;

                // Clear Sub-files
                ClrSubfiles();

                If isValid = *Off and isLoadSfl = *Off;
                   // Load Require Reserve Worksheet Information
                   LoadWorksheet();
                Endif;

                // Load State Heading Setup Information
                LoadStateGrid();
                //*** Beg Add *** 03/02/23 *************************************
                // Load Import/Export Settings
                LoadImpExpInfo();
                //*** End Add *** 03/02/23 *************************************

                isLoadSfl = *Off;
             Endif;

          // Process policy summary screen.
          Exsr ExfmtScreens;

          Select;
          // do nothing - catch tab click event
          When tabClick = *on;
       //*** Beg Chg *** 03/30/23 *** WC2430 ***********************************
             // Multi-Claim configuration
             // UpdAddMcConfig('MultAddEdt':intRrn);
             // UpdAddMcConfig('MultAddClm':intRrn1);
             // UpdAddMcConfig('MultDelClm':intRrn2);
       //*** End Chg *** 03/30/23 *** WC2430 ***********************************
             tabClick = *off;
          When btnRefresh = *On;
             btnRefresh = *Off;
             isValid = *Off;
             erAppCode = *off;
             erRmkCode = *off;
             //*** Beg Add *** 03/02/23 ***************************************
             erFimEmAd = *off;
             erTimEmAd = *off;
             erImSub   = *off;
             erImBody  = *off;
             erFimEmAd2 = *off;
             erTimEmAd2 = *off;
             //*** End Add *** 03/02/23 ***************************************

          // Edit Reserve Worksheet Set Up Company Wide
          // (Enter key or Save button)
          When btnSave = *on;
             btnSave = *off;
             isValid = *off;
             erAppCode = *off;
             erRmkCode = *off;
             //*** Beg Add *** 03/02/23 ***************************************
             erFimEmAd  = *off;
             erTimEmAd  = *off;
             erImSub    = *off;
             erImBody   = *off;
             erFimEmAd2 = *off;
             erTimEmAd2 = *off;
             //*** End Add *** 03/02/23 ***************************************

             If mode <> 'D';
                //*** Beg Add *** 03/02/23 ************************************
                Select;
                When namesArr(TabIndex + 1) = 'Reserve Worksheet'
                   Or namesArr(TabIndex + 1) = 'General';
                //*** End Add *** 03/02/23 ************************************
                   Validate();
                   If isValid = *off;
                      UpdReserve();
                      //*** Beg Add *** 03/02/23 ******************************
                      Exfmt rcdsave;
                      //*** End Add *** 03/02/23 ******************************
                   Endif;
                //*** Beg Add *** 03/02/23 ************************************
                When namesArr(TabIndex + 1) = 'Import/Export';
                   ValidateImpExp();
                   If isValid = *off;
                      UpdImpExpInfo();
                      Exfmt rcdsave;
                   Endif;
                Endsl;
                //*** End Add *** 03/02/23 ************************************
             Endif;
             // Multi-Claim configuration
             UpdAddMcConfig('MultAddEdt':intRrn);
             UpdAddMcConfig('MultAddClm':intRrn1);
             UpdAddMcConfig('MultDelClm':intRrn2);
       //*** Beg Add *** 03/30/23 *** WC2430 ***********************************
             UpdAddMcConfig('PaymentRqs':intRrn3);
             UpdAddMcConfig('ManualPmnt':intRrn4);
             UpdAddMcConfig('Adjustment':intRrn5);
             UpdAddMcConfig('RecovTrack':intRrn6);
             UpdAddMcConfig('PostRecvry':intRrn7);
             UpdAddMcConfig('OvrDupPmnt':intRrn8);
       //*** End Add *** 03/30/23 *** WC2430 ***********************************

          // Menu Choices
          When scSelOpt <> *blanks;
             @@next = scSelOpt;
             Leave;

          // processing for Add mode
          When ScHdgAdd = *on;
             clear RcdStateHd;
             strmode   = 'A';
             BtnAddRcd = *off;
             BtnCanRcd = *off;
             isLoadSfl = *On;
             processAdd();

          // Save the General Tab setting
          // (Enter key or Checked or Unchecked ChkBox.
          When btnGenSav = *on;
              btnGenSav = *off;
              chain (intCo#) pco0s;
              If %found(smcol);
                 COrcdI = scGenClm;
                 COinqA = scInqAutO;
          //*** Beg Add *** 03/30/23 *** WC2430 ***********************************
                 COdPV2 = scDfltPV2;
          //*** End Add *** 03/30/23 *** WC2430 ***********************************
                 update pco0s;
              EndIf;
             // Multi-Claim configuration
              UpdAddMcConfig('MultAddEdt':intRrn);
              UpdAddMcConfig('MultAddClm':intRrn1);
              UpdAddMcConfig('MultDelClm':intRrn2);
          //*** Beg Add *** 03/30/23 *** WC2430 ***********************************
              UpdAddMcConfig('PaymentRqs':intRrn3);
              UpdAddMcConfig('ManualPmnt':intRrn4);
              UpdAddMcConfig('Adjustment':intRrn5);
              UpdAddMcConfig('RecovTrack':intRrn6);
              UpdAddMcConfig('PostRecvry':intRrn7);
              UpdAddMcConfig('OvrDupPmnt':intRrn8);
          //*** End Add *** 03/30/23 *** WC2430 ***********************************
          // Add User Access Window Logic
          When btnUsrAcc = *on;
             inDspFileName = 'WTCLMCFG';
             inPgmName     = 'Claims Configuration';
             inTabName     =  namesArr(TabIndex + 1);
             UserAccessWdw(inDspFileName:inPgmName:inTabName);
             btnUsrAcc = *off;

          Other;

             procOther();

          Endsl;

       Enddo;
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       Select;

       When namesArr(TabIndex + 1) = 'Reserve Worksheet';
          TabHeight = 800;
          APnlHeight = TabHeight + 60;
          aMenHeight = 120;
          apFootHght = aMenHeight + 125;
          apShowMenu = *on;
          Exsr WriteAction;
          Write rcdPanel;
          Exfmt RcdReqRsvW;
          Exsr ReadAction;
          Read rcdPanel;
       When namesArr(TabIndex + 1) = 'General';
          Exsr LoadGeneral;
          TabHeight = 800;
       //*** Beg Chg *** 03/30/23 *** WC2430 ***********************************
         // TabHeight = 800;
          TabHeight =  965;
       //*** End Chg *** 03/30/23 *** WC2430 ***********************************
          APnlHeight = TabHeight + 60;
          //*** Beg Chg *** 03/02/23 *********************************************
          aMenHeight = 125;
          //*** End Chg *** 03/02/23 *********************************************
          apFootHght = aMenHeight + 125;
          apShowMenu = *on;
          Exsr WriteAction;
          Write rcdPanel;
          Exfmt RcdGenrl;
          Exsr ReadAction;
          Read rcdPanel;
       //*** Beg Add *** 03/02/23 *********************************************
       When namesArr(TabIndex + 1) = 'Import/Export';
          TabHeight = 800;
          APnlHeight = TabHeight + 60;
          aMenHeight = 120;
          apFootHght = aMenHeight + 125;
          apShowMenu = *on;
          Exsr WriteAction;
          Write rcdPanel;
          Exfmt RcdImpExp;
          Exsr ReadAction;
          Read rcdPanel;
       //*** End Add *** 03/02/23 *********************************************
       EndSl;

       Endsr;

       //===================================================================
       // Write action panel;
       //===================================================================
       BegSr WriteAction;
       Write rcdSysTbl;
       Endsr;
       //===================================================================
       // Read action panel;
       //===================================================================
       BegSr ReadAction;

       Read rcdSysTbl;
       Endsr;

       //===================================================================
       // Load General Tab
       //===================================================================
       BegSr LoadGeneral;
       chain(n) (intCo#) pco0s;
       If %found(smcol);
          scGenClm = COrcdI;
          scInqAutO = COinqA;
       //*** Beg Add *** 03/30/23 *** WC2430 ***********************************
          scDfltPV2 = COdPV2;
       //*** End Add *** 03/30/23 *** WC2430 ***********************************
       Else;
          scGenClm  = 'N';
          scInqAutO = 'N';
       //*** Beg Add *** 03/30/23 *** WC2430 ***********************************
          scDfltPV2 = 'N';
       //*** End Add *** 03/30/23 *** WC2430 ***********************************
       EndIf;

       // Multi-Claim configuration
       LoadMcConfig('MultAddEdt');
       LoadMcConfig('MultAddClm');
       LoadMcConfig('MultDelClm');
       //*** Beg Add *** 03/30/23 *** WC2430 ***********************************
       // Payment Configuration
       LoadMcConfig('PaymentRqs');
       LoadMcConfig('ManualPmnt');
       LoadMcConfig('Adjustment');
       LoadMcConfig('RecovTrack');
       LoadMcConfig('PostRecvry');
       LoadMcConfig('OvrDupPmnt');
       //*** End Add *** 03/30/23 *** WC2430 ***********************************
       EndSr;

     P Main            E
       //===================================================================
       // Validate - Validate Require Reserve Worksheet Screen
       //===================================================================
     p Validate        b
     D Validate        pi

      /free

       // Check if Worksheet Approval Diary Code is Blanks
       If scAppCode = *blanks;
          erAppCode = *on;
          isValid   = *on;
       Endif;

       // Check if Worksheet Remark Code is Blanks
       If scRmkCode = *blanks;
          erRmkCode = *on;
          isValid   = *on;
       Endif;

       // Check to be sure screen has not been updated since entry
       Chain(n) intCo# pcw0c;

       // Check if WMCWP updated since screen displayed
       flgRcdUpdated = RcdUpdatedErr('WMCWP':rrnwmcwl:'CWdt':'CWtime':
        'CWuser':saveDT:saveTM);
       Select;
       When flgRcdUpdated = '0'; // No error
       When flgRcdUpdated = '1'; // Error
          isValid   = *on;
       When flgRcdUpdated = '2'; // Error with refresh
       Endsl;

      /end-free
     p Validate        e
       //*** Beg Add *** 03/02/23 *********************************************
       //===================================================================
       // ValidateImpExp- Validate Import/Export Settings
       //===================================================================
     P ValidateImpExp  B
     D ValidateImpExp  PI

       mgMutIncl = 'This field is required when any of the other fields +
        have entries';
       If scFimEmAd <> *blank or scTimEmAd <> *blanks or scImSub <> *blanks
        Or scImBody <> *blanks;
          If scFimEmAd = *blanks;
             isValid = *on;
             erFimEmAd = *on;
          Endif;
          If scTimEmAd = *blanks;
             isValid  = *on;
             erTimEmAd = *on;
          Endif;
          If scImSub = *blanks;
             isValid = *on;
             erImSub = *on;
          Endif;
          If scImBody = *blanks;
             isValid = *on;
             erImBody = *on;
          Endif;
       Endif;
       If scFimEmAd <> *blanks;
          erFimEmAd2 = Check_Email(scFimEmAd:mgFimEmAd);
          If erFimEmAd2 = *on;
             isValid = *on;
          Endif;
       Endif;
       If scTimEmAd <> *blanks;
          erTimEmAd2 = Check_Email(scTimEmAd:mgTImEmAd);
          If erTimEmAd2 = *on;
             isValid = *on;
          Endif;
       Endif;

     p ValidateImpExp  e
       //*** End Add *** 03/02/23 *********************************************
       //===================================================================
       // processAdd - process addition of Reserve Worksheet Setup
       //===================================================================
     P ProcessAdd      B
     D ProcessAdd      PI

      /free

       errState = *off;

       // processing for Edit mode
       If strmode = 'C';
          MoveDbSc();
       Endif;

       // processing for Add mode
       If ScHdgAdd = *on;
          scRsvStat = 'Active';
       Endif;

       Dou BtnCanRcd = *on or (BtnAddRcd = *on and errState = *off);
          Exfmt RcdStateHd;
          errState = *off;

          Select;
          When BtnAddRcd = *on;

             // Check for adding Dupliacte State record
             If strmode <> 'C';
                Chain (intco#:scAddSt) pcw0s;
                If %found(wdcwl);
                   errState = *on;
                Endif;
             Endif;

             If errState = *off;
                AddReserve();
             Endif;

          When BtnCanRcd = *on;
             BtnCanRcd = *off;
             Leave;
          Endsl;
       Enddo;
       unlock WDCWL;
      /end-free
     P ProcessAdd      E
       //===================================================================
       // procOther - process other options
       //===================================================================
     P ProcOther       B
     D ProcOther       PI
      /free

       ReadC sflStateHd;

       Dow not %eof;

          If scHdgState = 'Default';
             scHdgState = *blanks;
          Endif;

          Select;
          // processing for Status Inactivate/Activate link
          When scStatus = *on;
             scStatus = *off;

             isLoadSfl = *On;

             Chain (intco#:scHdgState) pcw0s;
             If %found(wdcwl);

                If CWstat     = 'A';
                   scActivate = 'Activate';
                   scActivat1 = 'Inactive';
                   CWstat     = 'I';
                Else;
                   scActivate = 'Inactivate';
                   scActivat1 = 'Active';
                   CWstat     = 'A';
                Endif;

                CWuser = q1user;
                CWpgm  = q1pgm;
                CWdt   = Dateto7(SystemDate());
                CWtime = Timeto6(SystemTime());
                Update pcw0s;
                Endif;

             btnRefresh = *On;

          // processing for Edit mode
          When scHdgEdt = *on;
             scHdgEdt = *off;

             isLoadSfl = *On;

             Chain (intco#:scHdgState) pcw0s;
             If %found(wdcwl);
                clear RcdStateHd;
                strmode   = 'C';
                BtnAddRcd = *off;
                BtnCanRcd = *off;
                scDisstate = *on;
                processAdd();
             Endif;
          Endsl;

       ReadC sflStateHd;
       Enddo;

      /end-free
     P procOther       E
       //===================================================================
       // addReserve - Add Reserve Worksheet Setup
       //===================================================================
     P AddReserve      B
     D AddReserve      PI
     c
      /free

       If scAddSt <> 'Default';
          CWst = scAddSt;
       Endif;
       CWstat = scRsvStat;
       CWbfh1 = scAddIBH1;
       CWbfh2 = scAddIBH2;
       CWbfh3 = scAddIBH3;
       CWbfh4 = scAddIBH4;
       CWbfh5 = scAddIBH5;
       CWbfhP = scAddIBH6;
       CWifh1 = scAddIFH1;
       CWifh2 = scAddIFH2;
       CWifh3 = scAddIFH3;
       CWifh4 = scAddIFH4;
       CWmfh1 = scAddMFH1;
       CWmfh2 = scAddMFH2;
       CWmfh3 = scAddMFH3;
       CWmfh4 = scAddMFH4;
       CWefh1 = scAddEFH1;
       CWefh2 = scAddEFH2;
       CWefh3 = scAddEFH3;
       CWefh4 = scAddEFH4;
       CWlfh1 = scAddLFH1;
       CWlfh2 = scAddLFH2;
       CWlfh3 = scAddLFH3;
       CWlfh4 = scAddLFH4;

       CWbf1Edit = scIndEdt1;
       CWbf2Edit = scIndEdt2;
       CWbf3Edit = scIndEdt3;
       CWbf4Edit = scIndEdt4;
       CWbf5Edit = scIndEdt5;
       CWbfpEdit = scIndEdt6;

       If strmode = 'A';
          CWco#   = intco#;
          CWiusr  = q1user;
          CWipgm  = q1pgm;
          CWidt   = Dateto7(SystemDate());
          CWitim  = Timeto6(SystemTime());

          CWuser = *blanks;
          CWpgm  = *blanks;
          CWdt   = *zeros;
          CWtime = *zeros;
          Write pcw0s;
       Else;
          CWuser = q1user;
          CWpgm  = q1pgm;
          CWdt   = Dateto7(SystemDate());
          CWtime = Timeto6(SystemTime());
          Update pcw0s;
       Endif;

      /end-free
     P addReserve      E
       //===================================================================
       // UpdReserve - Update Reserve Worksheet Company Wide
       //===================================================================
     P UpdReserve      B
     D UpdReserve      PI
     c
      /free

       Chain intco# pcw0c;
       If %found(WMCWL);

          CWco#  = intco#;
          CWmofl = scMedClm;
          CWmoth = scMedAmt;
          CWinfl = scIndClm;
          CWinth = scIndAmt;
          CWdisa = scUserFlag;
          CWracd = scAppCode;
          CWrmcd = scRmkCode;

          CWuser = q1user;
          CWpgm  = q1pgm;
          CWdt   = Dateto7(SystemDate());
          CWtime = Timeto6(SystemTime());

          saveDT = CWdt;
          saveTM = CWtime;

          Update pcw0c;

       Else;

          CWco#  = intco#;
          CWmofl = scMedClm;
          CWmoth = scMedAmt;
          CWinfl = scIndClm;
          CWinth = scIndAmt;
          CWdisa = scUserFlag;
          CWracd = scAppCode;
          CWrmcd = scRmkCode;

          CWiusr  = q1user;
          CWipgm  = q1pgm;
          CWidt   = Dateto7(SystemDate());
          CWitim  = Timeto6(SystemTime());

          CWuser = *blanks;
          CWpgm  = *blanks;
          CWdt   = *zeros;
          CWtime = *zeros;
          Write pcw0c;
       Endif;

      /end-free
     P updReserve      E
       //*** Beg Add *** 03/02/23 *********************************************
       //===================================================================
       // Update Import/Export Settings Information
       //===================================================================
     P UpdImpExpInfo   B
     D UpdImpExpInfo   PI


       Chain intCo# pime01;
       If %Found(WDIMEXL1);
          IMEfImEmAd = scFimEmAd;
          IMEtImEmAd = scTimEmAd;
          IMEImSub = scImSub;
          IMEImBody = scImBody;
          IMEuser = q1user;
          IMEpgm  = q1pgm;
          IMEdt   = Dateto7(SystemDate());
          IMEtime = Timeto6(SystemTime());
          Update Pime01;
       Else;
          IMECo#     = intCo#;
          IMEfImEmAd = scFimEmAd;
          IMEtImEmAd = scTimEmAd;
          IMEImSub = scImSub;
          IMEImBody = scImBody;
          IMEiUsr = q1user;
          IMEiPgm = q1pgm;
          IMEiDt  = Dateto7(SystemDate());
          IMEiTim = Timeto6(SystemTime());
          IMEuser = q1user;
          IMEpgm  = q1pgm;
          IMEdt   = Dateto7(SystemDate());
          IMEtime = Timeto6(SystemTime());
          Write Pime01;
       Endif;

     P UpdImpExpInfo   E
       //*** End Add *** 03/02/23 *********************************************
       //===================================================================
       // MoveDbSc - Move database record to screen
       //===================================================================
     P MoveDbSc        B
     D MoveDbSc        PI
     c
      /free

       // State
       scAddSt   = CWst;
       // Status
       If CWstat = 'A';
          scRsvStat = 'Active';
       Else;
          scRsvStat = 'Inactive';
       Endif;
       scAddIBH1 = CWbfh1;
       scAddIBH2 = CWbfh2;
       scAddIBH3 = CWbfh3;
       scAddIBH4 = CWbfh4;
       scAddIBH5 = CWbfh5;
       scAddIBH6 = CWbfhP;
       scAddIFH1 = CWifh1;
       scAddIFH2 = CWifh2;
       scAddIFH3 = CWifh3;
       scAddIFH4 = CWifh4;
       scAddMFH1 = CWmfh1;
       scAddMFH2 = CWmfh2;
       scAddMFH3 = CWmfh3;
       scAddMFH4 = CWmfh4;
       scAddEFH1 = CWefh1;
       scAddEFH2 = CWefh2;
       scAddEFH3 = CWefh3;
       scAddEFH4 = CWefh4;
       scAddLFH1 = CWlfh1;
       scAddLFH2 = CWlfh2;
       scAddLFH3 = CWlfh3;
       scAddLFH4 = CWlfh4;
       scIndEdt1 = CWbf1Edit;
       scIndEdt2 = CWbf2Edit;
       scIndEdt3 = CWbf3Edit;
       scIndEdt4 = CWbf4Edit;
       scIndEdt5 = CWbf5Edit;
       scIndEdt6 = CWbfpEdit;

      /end-free
     P MoveDbSc        E
       //===================================================================
       //*------------------------------------------------------------------
       //* Load Multi-Claim Configuration
       //*------------------------------------------------------------------
       Dcl-Proc LoadMcConfig;
        Dcl-Pi *N;
         @Key char(10) Const;
        End-Pi;

       strKey = @key;
       Chain strKey pac00;
       If not %found;
          clear *all pac00;
          Select;
          When strKey = 'MultAddEdt';
             intRrn = 0;
             scAEmcTyp1 = *blanks;
             scAEmcLvl1 = *zeros;
             scAEmcTyp2 = *blanks;
             scAEmcLvl2 = *zeros;
             scAEmcTyp3 = *blanks;
             scAEmcLvl3 = *zeros;
             scAEmcTyp4 = *blanks;
             scAEmcLvl4 = *zeros;
             scAEmcTyp5 = *blanks;
             scAEmcLvl5 = *zeros;
             scAEmcTyp6 = *blanks;
             scAEmcLvl6 = *zeros;
             scAEmcTyp7 = *blanks;
             scAEmcLvl7 = *zeros;
             scAEmcTyp8 = *blanks;
             scAEmcLvl8 = *zeros;
             scAEmcTyp9 = *blanks;
             scAEmcLvl9 = *zeros;
             scAEmcTy10 = *blanks;
             scAEmcLv10 = *zeros;
          When strKey = 'MultAddClm';
             intRrn1 = 0;
             scAddCtyp1 = *blanks;
             scAddClvl1 = *zeros;
             scAddCtyp2 = *blanks;
             scAddClvl2 = *zeros;
             scAddCtyp3 = *blanks;
             scAddClvl3 = *zeros;
             scAddCtyp4 = *blanks;
             scAddClvl4 = *zeros;
             scAddCtyp5 = *blanks;
             scAddClvl5 = *zeros;
             scAddCtyp6 = *blanks;
             scAddClvl6 = *zeros;
             scAddCtyp7 = *blanks;
             scAddClvl7 = *zeros;
             scAddCtyp8 = *blanks;
             scAddClvl8 = *zeros;
             scAddCtyp9 = *blanks;
             scAddClvl9 = *zeros;
             scAddCty10 = *blanks;
             scAddClv10 = *zeros;
          When strKey = 'MultDelClm';
             intRrn2 = 0;
             scDelCtyp1 = *blanks;
             scDelClvl1 = *zeros;
             scDelCtyp2 = *blanks;
             scDelClvl2 = *zeros;
             scDelCtyp3 = *blanks;
             scDelClvl3 = *zeros;
             scDelCtyp4 = *blanks;
             scDelClvl4 = *zeros;
             scDelCtyp5 = *blanks;
             scDelClvl5 = *zeros;
             scDelCtyp6 = *blanks;
             scDelClvl6 = *zeros;
             scDelCtyp7 = *blanks;
             scDelClvl7 = *zeros;
             scDelCtyp8 = *blanks;
             scDelClvl8 = *zeros;
             scDelCtyp9 = *blanks;
             scDelClvl9 = *zeros;
             scDelCty10 = *blanks;
             scDelClv10 = *zeros;
       //***  Beg Add  ***  03/30/23  *** WC2430 **************************************
          When strKey = 'PaymentRqs';
             intRrn3 = 0;
             scPayRTyp1 = *blanks;
             scPayRLvl1 = *zeros;
             scPayRTyp2 = *blanks;
             scPayRLvl2 = *zeros;
             scPayRTyp3 = *blanks;
             scPayRLvl3 = *zeros;
             scPayRTyp4 = *blanks;
             scPayRLvl4 = *zeros;
             scPayRTyp5 = *blanks;
             scPayRLvl5 = *zeros;
             scPayRTyp6 = *blanks;
             scPayRLvl6 = *zeros;
             scPayRTyp7 = *blanks;
             scPayRLvl7 = *zeros;
             scPayRTyp8 = *blanks;
             scPayRLvl8 = *zeros;
             scPayRTyp9 = *blanks;
             scPayRLvl9 = *zeros;
             scPayRTy10 = *blanks;
             scPayRLv10 = *zeros;
          When strKey = 'ManualPmnt';
             intRrn4 = 0;
             scManPTyp1 = *blanks;
             scManPLvl1 = *zeros;
             scManPTyp2 = *blanks;
             scManPLvl2 = *zeros;
             scManPTyp3 = *blanks;
             scManPLvl3 = *zeros;
             scManPTyp4 = *blanks;
             scManPLvl4 = *zeros;
             scManPTyp5 = *blanks;
             scManPLvl5 = *zeros;
             scManPTyp6 = *blanks;
             scManPLvl6 = *zeros;
             scManPTyp7 = *blanks;
             scManPLvl7 = *zeros;
             scManPTyp8 = *blanks;
             scManPLvl8 = *zeros;
             scManPTyp9 = *blanks;
             scManPLvl9 = *zeros;
             scManPTy10 = *blanks;
             scManPLv10 = *zeros;
          When strKey = 'Adjustment';
             intRrn5 = 0;
             scAdstTyp1 = *blanks;
             scAdstLvl1 = *zeros;
             scAdstTyp2 = *blanks;
             scAdstLvl2 = *zeros;
             scAdstTyp3 = *blanks;
             scAdstLvl3 = *zeros;
             scAdstTyp4 = *blanks;
             scAdstLvl4 = *zeros;
             scAdstTyp5 = *blanks;
             scAdstLvl5 = *zeros;
             scAdstTyp6 = *blanks;
             scAdstLvl6 = *zeros;
             scAdstTyp7 = *blanks;
             scAdstLvl7 = *zeros;
             scAdstTyp8 = *blanks;
             scAdstLvl8 = *zeros;
             scAdstTyp9 = *blanks;
             scAdstLvl9 = *zeros;
             scAdstTy10 = *blanks;
             scAdstLv10 = *zeros;
          When strKey = 'RecovTrack';
             intRrn6 = 0;
             scRecTTyp1 = *blanks;
             scRecTLvl1 = *zeros;
             scRecTTyp2 = *blanks;
             scRecTLvl2 = *zeros;
             scRecTTyp3 = *blanks;
             scRecTLvl3 = *zeros;
             scRecTTyp4 = *blanks;
             scRecTLvl4 = *zeros;
             scRecTTyp5 = *blanks;
             scRecTLvl5 = *zeros;
             scRecTTyp6 = *blanks;
             scRecTLvl6 = *zeros;
             scRecTTyp7 = *blanks;
             scRecTLvl7 = *zeros;
             scRecTTyp8 = *blanks;
             scRecTLvl8 = *zeros;
             scRecTTyp9 = *blanks;
             scRecTLvl9 = *zeros;
             scRecTTy10 = *blanks;
             scRecTLv10 = *zeros;
          When strKey = 'PostRecvry';
             intRrn7 = 0;
             scPstRTyp1 = *blanks;
             scPstRLvl1 = *zeros;
             scPstRTyp2 = *blanks;
             scPstRLvl2 = *zeros;
             scPstRTyp3 = *blanks;
             scPstRLvl3 = *zeros;
             scPstRTyp4 = *blanks;
             scPstRLvl4 = *zeros;
             scPstRTyp5 = *blanks;
             scPstRLvl5 = *zeros;
             scPstRTyp6 = *blanks;
             scPstRLvl6 = *zeros;
             scPstRTyp7 = *blanks;
             scPstRLvl7 = *zeros;
             scPstRTyp8 = *blanks;
             scPstRLvl8 = *zeros;
             scPstRTyp9 = *blanks;
             scPstRLvl9 = *zeros;
             scPstRTy10 = *blanks;
             scPstRLv10 = *zeros;
          When strKey = 'OvrDupPmnt';
             intRrn8 = 0;
             scODPtTyp1 = *blanks;
             scODPtLvl1 = *zeros;
             scODPtTyp2 = *blanks;
             scODPtLvl2 = *zeros;
             scODPtTyp3 = *blanks;
             scODPtLvl3 = *zeros;
             scODPtTyp4 = *blanks;
             scODPtLvl4 = *zeros;
             scODPtTyp5 = *blanks;
             scODPtLvl5 = *zeros;
             scODPtTyp6 = *blanks;
             scODPtLvl6 = *zeros;
             scODPtTyp7 = *blanks;
             scODPtLvl7 = *zeros;
             scODPtTyp8 = *blanks;
             scODPtLvl8 = *zeros;
             scODPtTyp9 = *blanks;
             scODPtLvl9 = *zeros;
             scODPtTy10 = *blanks;
             scODPtLv10 = *zeros;
       //***  End Add  ***  03/30/23  *************************************************
          Endsl;
       Else;
          Select;
          When strKey = 'MultAddEdt';
             intRrn = dbrrn;
             scAEmcTyp1 = ACsTp1;
             scAEmcTyp2 = ACsTp2;
             scAEmcTyp3 = ACsTp3;
             scAEmcTyp4 = ACsTp4;
             scAEmcTyp5 = ACsTp5;
             scAEmcTyp6 = ACsTp6;
             scAEmcTyp7 = ACsTp7;
             scAEmcTyp8 = ACsTp8;
             scAEmcTyp9 = ACsTp9;
             scAEmcTy10 = ACsTp0;
             scAEmcLvl1 = ACsLv1;
             scAEmcLvl2 = ACsLv2;
             scAEmcLvl3 = ACsLv3;
             scAEmcLvl4 = ACsLv4;
             scAEmcLvl5 = ACsLv5;
             scAEmcLvl6 = ACsLv6;
             scAEmcLvl7 = ACsLv7;
             scAEmcLvl8 = ACsLv8;
             scAEmcLvl9 = ACsLv9;
             scAEmcLv10 = ACsLv0;
          When strKey = 'MultAddClm';
             intRrn1 = dbrrn;
             scAddCtyp1 = ACsTp1;
             scAddCtyp2 = ACsTp2;
             scAddCtyp3 = ACsTp3;
             scAddCtyp4 = ACsTp4;
             scAddCtyp5 = ACsTp5;
             scAddCtyp6 = ACsTp6;
             scAddCtyp7 = ACsTp7;
             scAddCtyp8 = ACsTp8;
             scAddCtyp9 = ACsTp9;
             scAddCty10 = ACsTp0;
             scAddClvl1 = ACsLv1;
             scAddClvl2 = ACsLv2;
             scAddClvl3 = ACsLv3;
             scAddClvl4 = ACsLv4;
             scAddClvl5 = ACsLv5;
             scAddClvl6 = ACsLv6;
             scAddClvl7 = ACsLv7;
             scAddClvl8 = ACsLv8;
             scAddClvl9 = ACsLv9;
             scAddClv10 = ACsLv0;
          When strKey = 'MultDelClm';
             intRrn2 = dbrrn;
             scDelCtyp1 = ACsTp1;
             scDelCtyp2 = ACsTp2;
             scDelCtyp3 = ACsTp3;
             scDelCtyp4 = ACsTp4;
             scDelCtyp5 = ACsTp5;
             scDelCtyp6 = ACsTp6;
             scDelCtyp7 = ACsTp7;
             scDelCtyp8 = ACsTp8;
             scDelCtyp9 = ACsTp9;
             scDelCty10 = ACsTp0;
             scDelClvl1 = ACsLv1;
             scDelClvl2 = ACsLv2;
             scDelClvl3 = ACsLv3;
             scDelClvl4 = ACsLv4;
             scDelClvl5 = ACsLv5;
             scDelClvl6 = ACsLv6;
             scDelClvl7 = ACsLv7;
             scDelClvl8 = ACsLv8;
             scDelClvl9 = ACsLv9;
             scDelClv10 = ACsLv0;
       //***  Beg Add  ***  03/30/23  *** WC2430 **************************************
          When strKey = 'PaymentRqs';
             intRrn3 = dbrrn;
             scPayRTyp1 = ACsTp1;
             scPayRTyp2 = ACsTp2;
             scPayRTyp3 = ACsTp3;
             scPayRTyp4 = ACsTp4;
             scPayRTyp5 = ACsTp5;
             scPayRTyp6 = ACsTp6;
             scPayRTyp7 = ACsTp7;
             scPayRTyp8 = ACsTp8;
             scPayRTyp9 = ACsTp9;
             scPayRTy10 = ACsTp0;
             scPayRLvl1 = ACsLv1;
             scPayRLvl2 = ACsLv2;
             scPayRLvl3 = ACsLv3;
             scPayRLvl4 = ACsLv4;
             scPayRLvl5 = ACsLv5;
             scPayRLvl6 = ACsLv6;
             scPayRLvl7 = ACsLv7;
             scPayRLvl8 = ACsLv8;
             scPayRLvl9 = ACsLv9;
             scPayRLv10 = ACsLv0;
          When strKey = 'ManualPmnt';
             intRrn4 = dbrrn;
             scManPTyp1 = ACsTp1;
             scManPTyp2 = ACsTp2;
             scManPTyp3 = ACsTp3;
             scManPTyp4 = ACsTp4;
             scManPTyp5 = ACsTp5;
             scManPTyp6 = ACsTp6;
             scManPTyp7 = ACsTp7;
             scManPTyp8 = ACsTp8;
             scManPTyp9 = ACsTp9;
             scManPTy10 = ACsTp0;
             scManPLvl1 = ACsLv1;
             scManPLvl2 = ACsLv2;
             scManPLvl3 = ACsLv3;
             scManPLvl4 = ACsLv4;
             scManPLvl5 = ACsLv5;
             scManPLvl6 = ACsLv6;
             scManPLvl7 = ACsLv7;
             scManPLvl8 = ACsLv8;
             scManPLvl9 = ACsLv9;
             scManPLv10 = ACsLv0;
          When strKey = 'Adjustment';
             intRrn5 = dbrrn;
             scAdstTyp1 = ACsTp1;
             scAdstTyp2 = ACsTp2;
             scAdstTyp3 = ACsTp3;
             scAdstTyp4 = ACsTp4;
             scAdstTyp5 = ACsTp5;
             scAdstTyp6 = ACsTp6;
             scAdstTyp7 = ACsTp7;
             scAdstTyp8 = ACsTp8;
             scAdstTyp9 = ACsTp9;
             scAdstTy10 = ACsTp0;
             scAdstLvl1 = ACsLv1;
             scAdstLvl2 = ACsLv2;
             scAdstLvl3 = ACsLv3;
             scAdstLvl4 = ACsLv4;
             scAdstLvl5 = ACsLv5;
             scAdstLvl6 = ACsLv6;
             scAdstLvl7 = ACsLv7;
             scAdstLvl8 = ACsLv8;
             scAdstLvl9 = ACsLv9;
             scAdstLv10 = ACsLv0;
          When strKey = 'RecovTrack';
             intRrn6 = dbrrn;
             scRecTTyp1 = ACsTp1;
             scRecTTyp2 = ACsTp2;
             scRecTTyp3 = ACsTp3;
             scRecTTyp4 = ACsTp4;
             scRecTTyp5 = ACsTp5;
             scRecTTyp6 = ACsTp6;
             scRecTTyp7 = ACsTp7;
             scRecTTyp8 = ACsTp8;
             scRecTTyp9 = ACsTp9;
             scRecTTy10 = ACsTp0;
             scRecTLvl1 = ACsLv1;
             scRecTLvl2 = ACsLv2;
             scRecTLvl3 = ACsLv3;
             scRecTLvl4 = ACsLv4;
             scRecTLvl5 = ACsLv5;
             scRecTLvl6 = ACsLv6;
             scRecTLvl7 = ACsLv7;
             scRecTLvl8 = ACsLv8;
             scRecTLvl9 = ACsLv9;
             scRecTLv10 = ACsLv0;
          When strKey = 'PostRecvry';
             intRrn7 = dbrrn;
             scPstRTyp1 = ACsTp1;
             scPstRTyp2 = ACsTp2;
             scPstRTyp3 = ACsTp3;
             scPstRTyp4 = ACsTp4;
             scPstRTyp5 = ACsTp5;
             scPstRTyp6 = ACsTp6;
             scPstRTyp7 = ACsTp7;
             scPstRTyp8 = ACsTp8;
             scPstRTyp9 = ACsTp9;
             scPstRTy10 = ACsTp0;
             scPstRLvl1 = ACsLv1;
             scPstRLvl2 = ACsLv2;
             scPstRLvl3 = ACsLv3;
             scPstRLvl4 = ACsLv4;
             scPstRLvl5 = ACsLv5;
             scPstRLvl6 = ACsLv6;
             scPstRLvl7 = ACsLv7;
             scPstRLvl8 = ACsLv8;
             scPstRLvl9 = ACsLv9;
             scPstRLv10 = ACsLv0;
          When strKey = 'OvrDupPmnt';
             intRrn8 = dbrrn;
             scODPtTyp1 = ACsTp1;
             scODPtTyp2 = ACsTp2;
             scODPtTyp3 = ACsTp3;
             scODPtTyp4 = ACsTp4;
             scODPtTyp5 = ACsTp5;
             scODPtTyp6 = ACsTp6;
             scODPtTyp7 = ACsTp7;
             scODPtTyp8 = ACsTp8;
             scODPtTyp9 = ACsTp9;
             scODPtTy10 = ACsTp0;
             scODPtLvl1 = ACsLv1;
             scODPtLvl2 = ACsLv2;
             scODPtLvl3 = ACsLv3;
             scODPtLvl4 = ACsLv4;
             scODPtLvl5 = ACsLv5;
             scODPtLvl6 = ACsLv6;
             scODPtLvl7 = ACsLv7;
             scODPtLvl8 = ACsLv8;
             scODPtLvl9 = ACsLv9;
             scODPtLv10 = ACsLv0;
       //***  End Add  ***  03/30/23  *** WC2430 **************************************
          Endsl;
       Endif;
       End-Proc;
       //*-------------------------------------------------------------------
       //* Update/Add Multi-Claim Configuration                                -
       //*-------------------------------------------------------------------
       Dcl-Proc UpdAddMcConfig;
        Dcl-Pi *N;
         @keyOut char(10) Const ;
         @Rrn packed(9:0);
        End-Pi;

        Dcl-S intRrn3 packed(9:0);
        intRrn3 = *Zero;

        //Load incoming parms
        intRrn3 = @Rrn;
        strKey = @keyOut;

        //Process parms
        Chain intRrn3 Pac0p;
        Select;
        When strKey = 'MultAddEdt';
           ACsTp1 = scAEmcTyp1;
           ACsTp2 = scAEmcTyp2;
           ACsTp3 = scAEmcTyp3;
           ACsTp4 = scAEmcTyp4;
           ACsTp5 = scAEmcTyp5;
           ACsTp6 = scAEmcTyp6;
           ACsTp7 = scAEmcTyp7;
           ACsTp8 = scAEmcTyp8;
           ACsTp9 = scAEmcTyp9;
           ACsTp0 = scAEmcTy10;
           ACsLv1 = scAEmcLvl1;
           ACsLv2 = scAEmcLvl2;
           ACsLv3 = scAEmcLvl3;
           ACsLv4 = scAEmcLvl4;
           ACsLv5 = scAEmcLvl5;
           ACsLv6 = scAEmcLvl6;
           ACsLv7 = scAEmcLvl7;
           ACsLv8 = scAEmcLvl8;
           ACsLv9 = scAEmcLvl9;
           ACsLv0 = scAEmcLv10;
        When strKey = 'MultAddClm';
           ACsTp1 = scAddCtyp1;
           ACsTp2 = scAddCtyp2;
           ACsTp3 = scAddCtyp3;
           ACsTp4 = scAddCtyp4;
           ACsTp5 = scAddCtyp5;
           ACsTp6 = scAddCtyp6;
           ACsTp7 = scAddCtyp7;
           ACsTp8 = scAddCtyp8;
           ACsTp9 = scAddCtyp9;
           ACsTp0 = scAddCty10;
           ACsLv1 = scAddClvl1;
           ACsLv2 = scAddClvl2;
           ACsLv3 = scAddClvl3;
           ACsLv4 = scAddClvl4;
           ACsLv5 = scAddClvl5;
           ACsLv6 = scAddClvl6;
           ACsLv7 = scAddClvl7;
           ACsLv8 = scAddClvl8;
           ACsLv9 = scAddClvl9;
           ACsLv0 = scAddClv10;
        When strKey = 'MultDelClm';
           ACsTp1 = scDelCtyp1;
           ACsTp2 = scDelCtyp2;
           ACsTp3 = scDelCtyp3;
           ACsTp4 = scDelCtyp4;
           ACsTp5 = scDelCtyp5;
           ACsTp6 = scDelCtyp6;
           ACsTp7 = scDelCtyp7;
           ACsTp8 = scDelCtyp8;
           ACsTp9 = scDelCtyp9;
           ACsTp0 = scDelCty10;
           ACsLv1 = scDelClvl1;
           ACsLv2 = scDelClvl2;
           ACsLv3 = scDelClvl3;
           ACsLv4 = scDelClvl4;
           ACsLv5 = scDelClvl5;
           ACsLv6 = scDelClvl6;
           ACsLv7 = scDelClvl7;
           ACsLv8 = scDelClvl8;
           ACsLv9 = scDelClvl9;
           ACsLv0 = scDelClv10;
       //***  Beg Add  ***  03/30/23  *** WC2430 **************************************
        When strKey = 'PaymentRqs';
           ACsTp1 = scPayRtyp1;
           ACsTp2 = scPayRtyp2;
           ACsTp3 = scPayRtyp3;
           ACsTp4 = scPayRtyp4;
           ACsTp5 = scPayRtyp5;
           ACsTp6 = scPayRtyp6;
           ACsTp7 = scPayRtyp7;
           ACsTp8 = scPayRtyp8;
           ACsTp9 = scPayRtyp9;
           ACsTp0 = scPayRty10;
           ACsLv1 = scPayRlvl1;
           ACsLv2 = scPayRlvl2;
           ACsLv3 = scPayRlvl3;
           ACsLv4 = scPayRlvl4;
           ACsLv5 = scPayRlvl5;
           ACsLv6 = scPayRlvl6;
           ACsLv7 = scPayRlvl7;
           ACsLv8 = scPayRlvl8;
           ACsLv9 = scPayRlvl9;
           ACsLv0 = scPayRlv10;
        When strKey = 'ManualPmnt';
           ACsTp1 = scManPtyp1;
           ACsTp2 = scManPtyp2;
           ACsTp3 = scManPtyp3;
           ACsTp4 = scManPtyp4;
           ACsTp5 = scManPtyp5;
           ACsTp6 = scManPtyp6;
           ACsTp7 = scManPtyp7;
           ACsTp8 = scManPtyp8;
           ACsTp9 = scManPtyp9;
           ACsTp0 = scManPty10;
           ACsLv1 = scManPlvl1;
           ACsLv2 = scManPlvl2;
           ACsLv3 = scManPlvl3;
           ACsLv4 = scManPlvl4;
           ACsLv5 = scManPlvl5;
           ACsLv6 = scManPlvl6;
           ACsLv7 = scManPlvl7;
           ACsLv8 = scManPlvl8;
           ACsLv9 = scManPlvl9;
           ACsLv0 = scManPlv10;
        When strKey = 'Adjustment';
           ACsTp1 = scAdsttyp1;
           ACsTp2 = scAdsttyp2;
           ACsTp3 = scAdsttyp3;
           ACsTp4 = scAdsttyp4;
           ACsTp5 = scAdsttyp5;
           ACsTp6 = scAdsttyp6;
           ACsTp7 = scAdsttyp7;
           ACsTp8 = scAdsttyp8;
           ACsTp9 = scAdsttyp9;
           ACsTp0 = scAdstty10;
           ACsLv1 = scAdstlvl1;
           ACsLv2 = scAdstlvl2;
           ACsLv3 = scAdstlvl3;
           ACsLv4 = scAdstlvl4;
           ACsLv5 = scAdstlvl5;
           ACsLv6 = scAdstlvl6;
           ACsLv7 = scAdstlvl7;
           ACsLv8 = scAdstlvl8;
           ACsLv9 = scAdstlvl9;
           ACsLv0 = scAdstlv10;
        When strKey = 'RecovTrack';
           ACsTp1 = scRecTtyp1;
           ACsTp2 = scRecTtyp2;
           ACsTp3 = scRecTtyp3;
           ACsTp4 = scRecTtyp4;
           ACsTp5 = scRecTtyp5;
           ACsTp6 = scRecTtyp6;
           ACsTp7 = scRecTtyp7;
           ACsTp8 = scRecTtyp8;
           ACsTp9 = scRecTtyp9;
           ACsTp0 = scRecTty10;
           ACsLv1 = scRecTlvl1;
           ACsLv2 = scRecTlvl2;
           ACsLv3 = scRecTlvl3;
           ACsLv4 = scRecTlvl4;
           ACsLv5 = scRecTlvl5;
           ACsLv6 = scRecTlvl6;
           ACsLv7 = scRecTlvl7;
           ACsLv8 = scRecTlvl8;
           ACsLv9 = scRecTlvl9;
           ACsLv0 = scRecTlv10;
        When strKey = 'PostRecvry';
           ACsTp1 = scPstRtyp1;
           ACsTp2 = scPstRtyp2;
           ACsTp3 = scPstRtyp3;
           ACsTp4 = scPstRtyp4;
           ACsTp5 = scPstRtyp5;
           ACsTp6 = scPstRtyp6;
           ACsTp7 = scPstRtyp7;
           ACsTp8 = scPstRtyp8;
           ACsTp9 = scPstRtyp9;
           ACsTp0 = scPstRty10;
           ACsLv1 = scPstRlvl1;
           ACsLv2 = scPstRlvl2;
           ACsLv3 = scPstRlvl3;
           ACsLv4 = scPstRlvl4;
           ACsLv5 = scPstRlvl5;
           ACsLv6 = scPstRlvl6;
           ACsLv7 = scPstRlvl7;
           ACsLv8 = scPstRlvl8;
           ACsLv9 = scPstRlvl9;
           ACsLv0 = scPstRlv10;
        When strKey = 'OvrDupPmnt';
           ACsTp1 = scODPttyp1;
           ACsTp2 = scODPttyp2;
           ACsTp3 = scODPttyp3;
           ACsTp4 = scODPttyp4;
           ACsTp5 = scODPttyp5;
           ACsTp6 = scODPttyp6;
           ACsTp7 = scODPttyp7;
           ACsTp8 = scODPttyp8;
           ACsTp9 = scODPttyp9;
           ACsTp0 = scODPtty10;
           ACsLv1 = scODPtlvl1;
           ACsLv2 = scODPtlvl2;
           ACsLv3 = scODPtlvl3;
           ACsLv4 = scODPtlvl4;
           ACsLv5 = scODPtlvl5;
           ACsLv6 = scODPtlvl6;
           ACsLv7 = scODPtlvl7;
           ACsLv8 = scODPtlvl8;
           ACsLv9 = scODPtlvl9;
           ACsLv0 = scODPtlv10;
       //***  End Add  ***  03/30/23  *** WC2430 **************************************
        Endsl;

        ACpgm  = Q1pgm;
        ACuser = Q1user;
        ACtime = Timeto6(SystemTime());
        ACdt   = Dateto7(SystemDate());

        If not %found;
           ACapid = strKey;
           ACipgm = Q1pgm;
           ACiusr = Q1user;
           ACitim = Timeto6(SystemTime());
           ACidt  = Dateto7(SystemDate());
           Write pac0p;
        Else;
           Update pac0p;
        Endif;
        unlock SDACP;

       End-Proc;
