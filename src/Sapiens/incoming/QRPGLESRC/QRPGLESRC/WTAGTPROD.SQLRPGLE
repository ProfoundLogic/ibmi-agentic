     hcopyright('(c) 2022 Sapiens, Inc. All rights reserved.')

      //*********************************************************************
     h/title WTAGTPROD - Agency Producers
     h dftname(WTAGTPROD)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h ALWNULL(*UsrCtl)
     h/define   profoundUI
     h bnddir('STBNDDIR')
     h bnddir('WTGRPCFG')
      //*********************************************************************
      //==============================================================*
      //  Created   : 01/27/22                                         *
      //  Programmer: C:300                                            *
      //  Project   : WC2395- Redesign of Agency Configuration program *
      //===============================================================*
      //  Revisions:                                                   *
      //                                                               *
      //  08/28/23 C:300 Project: WC2395, Support: 113191              *
      //                 Action Hyperlinks like Activate/Inactivate    *
      //                 should not be included as last column of excel*
      //                 spreadsheet export                            *
      //                 DSPF: Removed 2 hyperlinks scActProd & scActivate
      //                 and added 4 new hyperlinks for Activate/Inactivate
      //                                                               *
      //  08/21/23 C:300 Project: WC2395, Support: 113191              *
      //                 DSPF: Correct Export to Excel File Name       *
      //                 Also Include Producer Info in Licensing Info  *
      //                 Export File Name                              *
      //                                                               *
      //  08/17/23 C:300 Project: WC2395, Support: 113196              *
      //                 DSPF: Added Activate/Inactivate Producers Link*
      //                                                               *
      //  08/07/23 C:300 Project: WC2395, Support: 113138              *
      //                 Require Save-Add Next button on Add Producers *
      //                                                               *
      //  08/07/23 C:300 Project: WC2395, Support: 113225              *
      //                 Allow entry of data in Upper/Lower case in all*
      //                 'Comment' or 'Notes' type fields              *
      //                                                               *
      //                                                               *
      // ------------------------------------------------------------- *
      //  NOTE: Please add comments to the top of Revisions.           *
      // ------------------------------------------------------------- *
      //===============================================================*

     fwtagtprod cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflAgtProd:rrnAgtProd)
     f                                     Sfile(sflNonRes:rrnNonRes)
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdAgtPnl)
      * Company file
     fsmcol     if   e           k disk    rename(pco01:pco00)
      * Agent Master file
     fwmagp     if   e             disk    rename(pag01:pag0p)
      * Agent Producer File
     fwmapp     uf a e             disk    rename(pap01:pap0p)
     f                                     infds(dbfds4)
     fwmapl     if   e           k disk    rename(pap01:pap00)
     f                                     infds(dbfds)
      * Agent Producer File
     fwmappcrt  uf a e           k disk    rename(pap01:pap0pcr)
     f                                     prefix(CR_)
      * Producers Non-Resindent States Info File
     fwmpsp     uf a e             disk    rename(pps01:pps0p)
     f                                     infds(dbfds3)
     fwmpsl     if   e           k disk    rename(pps01:pps00)
     f                                     infds(dbfds2)
      *
     FWTABL     IF   E           K DISK    ignore(PRT01)


     d psds          esds                  extname(szq1p)
     d dbfds           ds
     d  dbrrn                397    400b 0
     d dbfds2          ds
     d  dbrrn2               397    400b 0
     d dbfds3          ds
     d  dbrrn3               397    400b 0
     d dbfds4          ds
     d  dbrrn4               397    400b 0

      // work variables
     d saveDT          s                   like(APdt)
     d saveTM          s                   like(APtime)
     d saveDT2         s                   like(APdt)
     d saveTM2         s                   like(APtime)
     d flgRcdUpdated   s              1    inz
     d intCo#          s              3  0
     d xxRrn           s              9  0 inz
     d indexTab        s              5  0
     d chgMod          s               n   inz(*off)
     d dspMode         s               n   inz(*off)
     d charMsg         s            100
     d process         s               n   inz(*off)
     d reload          s               n   inz(*off)
     d xxDelResp       s               n   inz
     d maxseq#         s             15  0 inz
     d infein          s              9  0 inz
     d inco#           s              3  0 inz
     d inmod           s              5  0 inz
     d @@co#           s              3  0 inz
     d @@fnd           s              3  0 inz
     d @@emp#          s              9  0 inz
     d @@div           s              5  0 inz
     d @@fyr           s              3  0 inz
     d @@agt#          s              5  0 inz
     d @@anam          s             60    inz
     d u1pdsc          s             40
     d u2pdsc          s             40
     d str9999Msg      s            256
     d xxKey           s             10
     d svrrnAgtProd    s              5  0
     d svrrnNonRes     s              5  0 inz
     d charSS#         s              9
     d co#_sav         s                   Like(APco#)
     d fein_sav        s                   Like(APfein)
     d mod_sav         s                   Like(APmod)
     d prod_sav        s                   Like(APprod)
     d fsn_sav         s                   Like(APfsn)
     d lsn_sav         s                   Like(APlsn)
     d rst_sav         s                   Like(APrst)                          Resident Lic State
      *** Begin Add ***** 08/17/23 *****************************
     d svStat          s             10    inz
     d svActProd       s             10    inz
      *** End   Add ***** 08/17/23 *****************************
     d*
      //

      // Proto-types

      // Load Producers Grid
     d LoadAgtProd     pr

      // Load Non-Resident States Grid
     d LoadNonRes      pr

      * Main Processing
     d Main            pr

      * Process Add for Non-Resident States Info
     d ProcAddNonR     pr

      * Process Add for Producers Info
     d ProcAddProd     pr

      * Process other Options for Non-Resident States
     d ProcOthrNonR    pr

      * Process other Options for Producers
     d ProcOthrProd    pr

      * Process the Row Click in Producers Grid
     d ProcRoWClk      pr

      * Program Start
     d ProgramStart    pr
     d processMe                       n

     * On/Off Subfile Display and Display Control Indicators for rcdAgtProd
     d RcdAgtProdDsp   pr
     d  indValue                      1n   const

     * Validate Non-Resident States info procedure.
     d ValidateNonR    pr              n
     d strmode                        1

     * Validate Producers info procedure.
     d ValidateProd    pr              n
     d strmode                        1


     d/copy sprUsrPrms
     d/copy sPrGenPop
     d/copy sPrDate
     d/copy sprCompany
     d/copy wPrPolErr
     d/copy sPrString
     d/copy sPrMask
     d/copy sPrGenErr
     d/copy sCopyGrpPr
    

     c     keyTable      klist
     c                   kfld                    scco#
     c                   kfld                    xxKey

     c     keyWMAPL      klist
     c                   kfld                    AGco#
     c                   kfld                    AGfein
     c                   kfld                    AGmod

     c     keyWMPSL      klist
     c                   kfld                    co#_sav
     c                   kfld                    fein_sav
     c                   kfld                    mod_sav
     c                   kfld                    prod_sav
    
     c     key2WMPSL     klist
     c                   kfld                    co#_sav
     c                   kfld                    fein_sav
     c                   kfld                    mod_sav
     c                   kfld                    prod_sav
     c                   kfld                    scStENR
    
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
       inFein = AGfein;
       inMod = AGmod;

       If (@@next = *Blanks or @@next = 'WTAGTPROD');

          ProgramStart(process);

          str9999msg = 'Your request returned too many records. ' +
           'Click Continue to see the first 9999 records. ' +
           'Click Filter on the display screen to narrow your results.';

          If process;

             Main();

          Endif;
       Endif;


       *InLr = *On;
      /end-free

       //===================================================================
       // Load Producers Grid
       //===================================================================
     P LoadAgtProd     B
     D LoadAgtProd     PI
      /free

       // Clear the subfile.
       clrAgtProd = *On;
       RcdAgtProdDsp(*off);
       Write rcdAgtProd;
       clrAgtProd = *Off;
       RcdAgtProdDsp(*on);
       rrnAgtProd = 0;
       scRowSel = 1;
       fein_sav = 0;

       If dspMode;
          inquiry = *on;
       Endif;

       Setll keyWMAPL pap00;
       Reade keyWMAPL pap00;

       DoW not %eof(WMAPL);
          scProd# = APprod;
          scName = %Trim(APlsn) + ', ' + %Trim(APfsn);
          scCxDt = CvtToDate(APcxdt);
          scStat = *Blanks;

          //*** Begin Chg ***** 08/28/23 *****************************
          //*** Begin Add ***** 08/17/23 *****************************
          // scActProd = *Blanks;
          //*** End   Add ***** 08/17/23 *****************************
          // If APstat = 'A';
          //    scStat = 'Active';
             //*** Begin Add ***** 08/17/23 *****************************
          //    scActProd = 'Inactivate';
             //*** End   Add ***** 08/17/23 *****************************
          // Endif;
          // If APstat = 'I';
          //    scStat = 'Inactive';
             //*** Begin Add ***** 08/17/23 *****************************
          //    scActProd = 'Activate';
             //*** End   Add ***** 08/17/23 *****************************
          // Endif;

          visActSts = *off;                // New Hyperlink for Activate
          visIActSts = *off;               // New Hyperlink for Inactivate
          If APstat = 'A';
             scStat = 'Active';
             visIActSts = *on;
          Endif;
          If APstat = 'I';
             scStat = 'Inactive';
             visActSts = *on;
          Endif;

          If dspMode;
             visActSts = *off;
             visIActSts = *off;
          Endif;
          //*** End Chg ***** 08/28/23 *****************************


          scProdrrn = dbrrn;
          rrnAgtProd = rrnAgtProd + 1;
          scProdSel = *off;

          // set the selection on 1st record for first load
          If svrrnAgtProd = *zeros;
             If rrnAgtProd = 1;
                scProdSel = *on;
             Endif;
          Else;
             If svrrnAgtProd = rrnAgtProd;
                scProdSel = *on;
             Endif;
          Endif;

          Write sflAgtProd;

          // Save 1st record information to load in A/R Transaction details Grid
          If fein_sav = 0;
             co#_sav  = APco#;
             fein_sav = APfein;
             mod_sav  = APmod;
             prod_sav = APprod;
             fsn_sav  = APfsn;
             lsn_sav  = APlsn;
             rst_sav  = APrst;                 // Resident Lic State
          Endif;

          Reade keyWMAPL pap00;
       Enddo;

       rrnAgtProd = 1;
       If svrrnAgtProd <> 0;
          rrnAgtProd = svrrnAgtProd;
          scRowSel = svrrnAgtProd;
          svrrnAgtProd = 0;
       Endif;

      /end-free
     P LoadAgtProd     E

       //===================================================================
       // Load Non-Resident States Grid
       //===================================================================
       //===================================================================
     P LoadNonRes      B
     D LoadNonRes      PI

      // Instructions::
      // Row 1 in Grid will show Resident State Info from Producer WMAPP file
      // and it will use WTAGTPRO05 Ajax program to populate detail box
      // Remaining rows in Grid will show Non-Resident State Info from WMPSP file
      // and it will use WTAGTPRO02 Ajax program to populate detail box

      /free

       // Clear the subfile.
       clrNonRes = *On;
       RcdAgtProdDsp(*off);
       Write rcdAgtProd;
       clrNonRes = *Off;
       RcdAgtProdDsp(*on);
       rrnNonRes = 0;
       scRowSel2 = 1;

       //*** Begin Add ***** 08/17/23 *****************************
       scExpFile = *Blanks;
       scExpFile = 'Licensing Info for Producer# ' + %Char(prod_sav) + ' - ' +
        %Trim(lsn_sav) + ', ' + %Trim(fsn_sav);
       //*** End   Add ***** 08/17/23 *****************************

      // Load 1st Row
       Chain keyWMPSL pap00;
       If %found(WMAPL);
          inquiry = *on;        // Don't show Edit/Delete Links
          scResi = 'Y';         // Resident State Flag
          scNRst = APrst;
          scLflg = APlflg;
          scLic# = APlic#;
          scLtyp = APltyp;
          scRwDt = CvtToDate(APrwdt);
          scExDt = CvtToDate(APexdt);
          scNRStat = *Blanks;
          If APstat = 'A';
             scNRStat = 'Active';
             //*** Begin Del ***** 08/28/23 *****************************
             // scActivate = 'Inactivate';
             //*** End   Del ***** 08/28/23 *****************************
          Endif;
          If APstat = 'I';
             scNRStat = 'Inactive';
             //*** Begin Del ***** 08/28/23 *****************************
             // scActivate = 'Activate';
             //*** End   Del ***** 08/28/23 *****************************
          Endif;

          //*** Begin Add ***** 08/28/23 *****************************
          // Like Edit/Delete Links, Don't show Activate/Inactivate links
          visActNR = *off;                // New Hyperlink for Activate
          visIActNR = *off;               // New Hyperlink for Inactivate
          //*** End Add ***** 08/28/23 *****************************

          scNonRRRN = dbrrn;
          rrnNonRes = rrnNonRes + 1;
          scNonReSel = *off;

          // set the selection on 1st record for first load
          If svrrnNonRes = *zeros;
             If rrnNonRes = 1;
                scNonReSel = *on;
             Endif;
          Else;
             If svrrnNonRes = rrnNonRes;
                scNonReSel = *on;
             Endif;
          Endif;

          Write sflNonRes;
       Endif;

      // Load Other Rows
       scResi = ' ';         // Resident State Flag
       inquiry = *off;
       If dspMode;
          inquiry = *on;
       Endif;

       Setll keyWMPSL pps00;
       Reade keyWMPSL pps00;
       Dow not %eof(WMPSL);
          scNRst = PSst;
          scLflg = PSlflg;
          scLic# = PSlic#;
          scLtyp = PSltyp;
          scRwDt = CvtToDate(PSrwdt);
          scExDt = CvtToDate(PSexdt);
          scNRStat = *Blanks;

          //*** Begin Chg ***** 08/28/23 *****************************
          // If PSstat = 'A';
          //    scNRStat = 'Active';
          //    scActivate = 'Inactivate';
          // Endif;
          // If PSstat = 'I';
          //    scNRStat = 'Inactive';
          //    scActivate = 'Activate';
          // Endif;

          visActNR = *off;                // New Hyperlink for Activate
          visIActNR = *off;               // New Hyperlink for Inactivate
          If PSstat = 'A';
             scNRStat = 'Active';
             visIActNR = *on;
          Endif;
          If PSstat = 'I';
             scNRStat = 'Inactive';
             visActNR = *on;
          Endif;

          If dspMode;
             visActNR = *off;
             visIActNR = *off;
          Endif;
          //*** End Chg ***** 08/28/23 *****************************

          scNonRRRN = dbrrn2;
          rrnNonRes = rrnNonRes + 1;
          scNonReSel = *off;

          // set the selection on 1st record for first load
          If svrrnNonRes = *zeros;
             If rrnNonRes = 1;
                scNonReSel = *on;
             Endif;
          Else;
             If svrrnNonRes = rrnNonRes;
                scNonReSel = *on;
             Endif;
          Endif;

          Select;
          When rrnNonRes > 9999;
            PopOneBtn(str9999msg:'Message-Transaction Detail':'Continue');
             Leave;
          Endsl;

          Write sflNonRes;

       Reade keyWMPSL pps00;
       Enddo;
       rrnNonRes = 1;
       If svrrnNonRes <> 0;
          rrnNonRes = svrrnNonRes;
          scRowSel2 = svrrnNonRes;
          svrrnNonRes = 0;
       Endif;

      /end-free
     P LoadNonRes      E

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi
     d*
      /free

       BtnCancel = *Off;
       reload = *on;

       // Process until exit button is pressed.
       DoW BtnCancel = *Off;

          // display screen
          Exsr ExfmtScreens;

          // Perform actions on screen.
          Select;
          // do nothing - catch tab click event
          When TabClick = *on;
             TabClick = *off;
             reload = *on;

          // Catch Row click event
          When RowClick = *on;
             RowClick = *off;
             ProcRoWClk();           // Process the Row Click in Producers Grid
             reload = *off;

          // Refresh
          When btnRefresh = *On;
             btnRefresh = *Off;
             reload = *on;

          // Add Producers info
          When BtnProdAdd = *On;
             BtnProdAdd = *Off;
             ProcAddProd();
             reload = *on;

          // Add Non-Resident States info
          When BtnNRAdd = *On;
             BtnNRAdd = *Off;
             ProcAddNonR();

          // For "Change FEIN" option in More Action of Agency Toolbar
          When btnShowWin = *on;
             PopupWindow(txtShowWin:scActRRN:' ':txtWinData);
             btnShowWin = *off;


          // Menu Choices
          When scSelOpt <> *blanks;
             @@next = scSelOpt;
             Leave;


          Other;
             ProcOthrNonR();         //other option for Non-Resident States info
             ProcOthrProd();         //other option for Producers info

          Endsl;
       Enddo;

       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       indexTab = TabIndex + 1;

       Select;

       When indexTab = 1;
          pnlHeight = 960;
          APnlHeight = pnlHeight + 60;

          If reload = *on;
             // Load Producers grid
             LoadAgtProd();
          Endif;

          // Load Non-Resident States Grid
          LoadNonRes();

          If dspMode;
             inquiry = *on;
          Endif;

          Write rcdAgtPnl;
          Write rcdPanel;
          Exfmt rcdAgtProd;
          Read rcdAgtPnl;
          Read rcdPanel;

       EndSl;

       Endsr;

      /end-free
     p Main            e

       //===================================================================
       // ProcAddNonR  - process addition of Non-Resident States Info
       //===================================================================
     P ProcAddNonR     B
     D ProcAddNonR     PI
     d*
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)

      /free

       Clear rcdAddNonR;
       mode = 'A';
       isexit = *off;
       BtnNonRCan = *off;

       Exsr MovDftVal;

       Dow BtnNonRCan = *off and isexit = *off;

          scNonRHdg = 'Add Non-Resident States Information';

          Exfmt rcdAddNonR;

          Select;

          // Save Button
          When BtnNonRSav = *on;
             BtnNonRSav = *off;
             If ValidateNonR(mode);
                Exsr WrtNonR;
                isexit = *on;
             EndIf;

          // Cancel Button
          When BtnNonRCan = *on;
             BtnNonRCan = *off;
             isexit = *on;

          EndSl;
       Enddo;

       // Writing Non-Resident States Details Subroutine
       BegSr WrtNonR;
        Clear pps0p;
        Exsr MoveScDb;
        Write pps0p;
       EndSr;

       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        PSco#   = inCo#;                 // Company Number
        PSfein  = scFeinE;               // Agency Fein
        PSmod   = scModE;                // Division
        PSprod  = scprodE;               // Producer
        PSst    = scStENR;               // State
        PSlflg  = scLflgE;               // License Flag
        PSlic#  = scLic#E;               // License Number
        PSrwdt  = dtCYMD(scRwDtE);       // License Renewal Date
        PSexdt  = dtCYMD(scExDtE);       // License Expiry Date
        PSltyp  = scLtypE;               // License Type 1
        PSltp2  = scLtp2E;               // License Type 2
        PSltp3  = scLtp3E;               // License Type 3
        PSltp4  = scLtp4E;               // License Type 4
        PSltp5  = scLtp5E;               // License Type 5
        PSfidt  = dtCYMD(scFiDtE);       // Filed with Carrier Date
        PSmldt  = dtCYMD(scMlDtE);       // Mailed to State Date
        PScfdt  = dtCYMD(scCfDtE);       // Confirmed Date
        PSdndt  = dtCYMD(scDnDtE);       // Denied Date
        PScom1  = scCom1E;               // Comments 1
        PScom2  = scCom2E;               // Comments 2
        PSstat  = scNonRStE;             // Status
        PSuser = q1user;
        PSpgm  = q1pgm;
        PStime = Timeto6(SystemTime());
        PSdt   = Dateto7(SystemDate());
        PSitm  = Timeto6(SystemTime());
        PSidt  = Dateto7(SystemDate());
        PSiusr = q1user;
        PSipgm = q1pgm;
       EndSr;

       BegSr MovDftVal;
        scNonRStE = 'A';
        // Agency FEIN, Mod, Name
        scFeinE = APfein;
        scModE = APmod;
        scAnamE = %trim(AGname);
        // Producer Number, Name
        scProdE = prod_sav;
        scPnamE = %Trim(lsn_sav) + ', ' + %Trim(fsn_sav);
       EndSr;

      /end-free
     P ProcAddNonR     E

       //===================================================================
       // ProcAddNonR  - process addition of Producers Info
       //===================================================================
     P ProcAddProd     B
     D ProcAddProd     PI
     d*
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)

      /free

       Clear rcdAddProd;
       mode = 'A';
       isexit = *off;
       BtnProdCan = *off;

       Exsr MovDftVal;

       Dow BtnProdCan = *off and isexit = *off;

          scProdHdg = 'Add Producers Information';
          visProdAdS = *On;                   // Show Save/Add next Button

          Exfmt rcdAddProd;

          Select;

          // Save-Add Next
          When BtnProdAds = *on;
             BtnProdAds = *off;
             If ValidateProd(mode);
                Exsr WrtProd;
                //Initialize fields for next add screen if save/addnext is pressed
                Clear rcdAddProd;
                Exsr MovDftVal;
                BtnProdCan = *off;
             EndIf;

          // Save Button
          When BtnProdSav = *on;
             BtnProdSav = *off;
             If ValidateProd(mode);
                Exsr WrtProd;
                isexit = *on;
             EndIf;

          // Cancel Button
          When BtnProdCan = *on;
             BtnProdCan = *off;
             isexit = *on;

          EndSl;
       Enddo;

       // Writing Producers Details Subroutine
       BegSr WrtProd;
        Clear pap0p;
        Exsr MoveScDb;
        Write pap0p;
       EndSr;

       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        APco#   = inCo#;                 // Company Number
        APfein  = inFein;                // Agency Fein
        APmod   = inMod;                 // Division
        APprod  = scprodPR;              // Producer
        APlsn   = scLsnPR;               // Last Name
        APfsn   = scFsnPR;               // First Name
        APmad1  = scMad1PR;              // Home Address
        APmcty  = scMctyPR;              // City
        APmst   = scMstPR;               // State
        APmzip  = scMzipPR;              // Zip
        APemal  = scEmalPR;              // Email
        // Social Security Number
        If scSs#PR <> *Blanks;
           clear charSS#;
           charSS# = UnMaskData('SSN':'APss#':scSs#PR);
           APss# = %dec(charSS#:9:0);
        Endif;

        APdob   = dtCYMD(scDobPR);       // Date of Birth
        APcxdt  = dtCYMD(scCxDtPR);      // Cancellation Date
        APcomm  = scCommPR;              // Comments 1
        APcom2  = scCom2PR;              // Comments 2
        APstat  = scProdStat;            // Status

        APrst   = scRstPR;               // State
        APlflg  = scLflgPR;              // License Flag
        APlic#  = scLic#PR;              // License Number
        APnpn   = scNpnPR;               // NPN Number
        APrwdt  = dtCYMD(scRwDtPR);      // License Renewal Date
        APexdt  = dtCYMD(scExDtPR);      // License Expiry Date
        APltyp  = scLtypPR;              // License Type 1
        APltp2  = scLtp2PR;              // License Type 2
        APltp3  = scLtp3PR;              // License Type 3
        APltp4  = scLtp4PR;              // License Type 4
        APltp5  = scLtp5PR;              // License Type 5
        APfidt  = dtCYMD(scFiDtPR);      // Filed with Carrier Date
        APmldt  = dtCYMD(scMlDtPR);      // Mailed to State Date
        APcfdt  = dtCYMD(scCfDtPR);      // Confirmed Date
        APdndt  = dtCYMD(scDnDtPR);      // Denied Date

        Exec SQL Select Max(APseq#) Into :maxseq#  From Wmapl
         Where APco# = :inco# And APfein = :infein And APmod = :inmod;
        APseq# = maxseq# + 1;

        Dow scDisTyp <> *blank;
           CR_APcritID  = 'Distribution';
           CR_APcritVal = ParseString(scDisTyp:',':1:'1');
           Exsr Write_CritRec;
        Enddo;
        Dow scSubCat <> *blank;
           CR_APcritID  = 'Sub Category';
           CR_APcritVal = ParseString(scSubCat:',':1:'1');
           Exsr Write_CritRec;
        Enddo;
        Dow scOutCnt <> *blank;
           CR_APcritID  = 'Process Output Control';
           CR_APcritVal = ParseString(scOutCnt:',':1:'1');
           Exsr Write_CritRec;
        Enddo;

        APuser = q1user;
        APpgm  = q1pgm;
        APtime = Timeto6(SystemTime());
        APdt   = Dateto7(SystemDate());
        APitm  = Timeto6(SystemTime());
        APidt  = Dateto7(SystemDate());
        APiusr = q1user;
        APipgm = q1pgm;
       EndSr;

       BegSr MovDftVal;
        scProdStat = 'A';
        Clear APseq#;
       EndSr;

       Begsr Write_CritRec;
        CR_APco#   = inco#;
        CR_APfein  = infein;
        CR_APmod   = inmod;
        CR_APprod  = APprod;
        CR_APseq#  = APseq#;
        CR_APstat  = APstat;
        CR_APuser  = q1user;
        CR_APpgm   = q1pgm;
        CR_APtime  = Timeto6(SystemTime());
        CR_APdt    = Dateto7(SystemDate());
        CR_APiusr  = q1user;
        CR_APipgm  = q1pgm;
        CR_APitm   = Timeto6(SystemTime());
        CR_APidt   = Dateto7(SystemDate());
        Write pap0pcr;
        Unlock wmappcrt;
       Endsr;

      /end-free
     P ProcAddProd     E

       //===================================================================
       // ProcOthrNonR  - Process other Options for Non-Resident States
       //===================================================================
     P ProcOthrNonR    B
     D ProcOthrNonR    PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdAddNonR;
       isexit = *off;
       ReadC sflNonRes;

       Exsr MovDftVal;

       Dow not %eof;

          Select;
          When scNREdt = *On; // Processing for Edit Non-Resident States Info

             scNREdt = *Off;
             mode = 'C';
             svrrnNonRes = rrnNonRes;   //Save Rrn value for edited record
             NREdt = *On;               //disable non-editable field

             Chain(n) scNonRRRN WMPSP;

             Exsr MovDbSc;

             scNonRHdg = 'Edit Non-Resident States Information';

             Dow isexit = *off;
                Exfmt rcdAddNonR;

                Select;
                // Save Button
                When BtnNonRSav = *on;
                   BtnNonRSav = *off;
                   // Check if WMPSP updated since screen displayed (Non-Resident States Info)
                   flgRcdUpdated=RcdUpdatedErr('WMPSP':dbrrn3:'PSdt':'PStime':
                    'PSuser':saveDT:saveTM:'1');
                   if flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scNonRRRN WMPSP;
                      Exsr MovDbSc;
                   EndIf;
                   if flgRcdUpdated = '0'; // No error
                      If ValidateNonR(mode);
                         Chain scNonRRRN WMPSP;
                         Exsr MoveScDb;
                         Update pps0p;
                         isexit = *on;
                      EndIf;
                   EndIf;

                // Cancel Button
                When BtnNonRCan = *on;
                   BtnNonRCan = *off;
                   isexit = *on;
                EndSl;
             EndDo;

             Unlock WMPSP;

          When scNRDlt = *On; // Processing for Delete Non-Resident States Link

             scNRDlt = *Off;

             Chain(e) scNonRRRN WMPSP;
             If %status = 1218;
                PopOneBtn('Record locked by another user. Please try again lat+
                 er.':'Record Lock');
                Return;
             Endif;

             Chain scNonRRRN WMPSP;
             Exsr MovDbSc;

             scNonRHdg = 'Delete Non-Resident States Information';

             Dow isexit = *off;

                Write rcdAddNonR;
                xxDelresp = PopTwoBtn('Do you want to delete this Non-Resident-
                            State?':'Confirm Delete':'Yes':'No':540:790);
                Select;
                When xxDelResp = *On;

                   Delete pps0p;
                   PopOneBtn('Non-Resident State Deleted!':'Delete':'Ok');
                   isexit = *on;
                When xxDelResp = *Off;
                   svrrnNonRes = rrnNonRes;   //Save Rrn value for edited record
                   isexit = *on;
                EndSl;
             EndDo;
          Endsl;

          ReadC sflNonRes;
       Enddo;

       // Subrotine to move database fields into screen fields
       BegSr MovDbSc;
        scStENR = PSst;                  // State
        scLflgE = PSlflg;                // License Flag
        scLic#E = PSlic#;                // License Number
        scRwDtE = dtMDYY(PSrwdt);        // License Renewal Date
        scExDtE = dtMDYY(PSexdt);        // License Expiry Date
        scLtypE = PSltyp;                // License Type 1
        scLtp2E = PSltp2;                // License Type 2
        scLtp3E = PSltp3;                // License Type 3
        scLtp4E = PSltp4;                // License Type 4
        scLtp5E = PSltp5;                // License Type 5
        scFiDtE = dtMDYY(PSfidt);        // Filed with Carrier Date
        scMlDtE = dtMDYY(PSmldt);        // Mailed to State Date
        scCfDtE = dtMDYY(PScfdt);        // Confirmed Date
        scDnDtE = dtMDYY(PSdndt);        // Denied Date
        scCom1E = PScom1;                // Comments 1
        scCom2E = PScom2;                // Comments 2
        scNonRStE = PSstat;              // Status

        saveDT = PSdt;
        saveTM = PStime;
       EndSr;

       // Subrotine to move Screen fields into database fields
       BegSr MoveScDb;
        PSlflg  = scLflgE;               // License Flag
        PSlic#  = scLic#E;               // License Number
        PSrwdt  = dtCYMD(scRwDtE);       // License Renewal Date
        PSexdt  = dtCYMD(scExDtE);       // License Expiry Date
        PSltyp  = scLtypE;               // License Type 1
        PSltp2  = scLtp2E;               // License Type 2
        PSltp3  = scLtp3E;               // License Type 3
        PSltp4  = scLtp4E;               // License Type 4
        PSltp5  = scLtp5E;               // License Type 5
        PSfidt  = dtCYMD(scFiDtE);       // Filed with Carrier Date
        PSmldt  = dtCYMD(scMlDtE);       // Mailed to State Date
        PScfdt  = dtCYMD(scCfDtE);       // Confirmed Date
        PSdndt  = dtCYMD(scDnDtE);       // Denied Date
        PScom1  = scCom1E;               // Comments 1
        PScom2  = scCom2E;               // Comments 2
        PSstat  = scNonRStE;             // Status

        PSuser = q1user;
        PSpgm  = q1pgm;
        PStime = Timeto6(SystemTime());
        PSdt   = Dateto7(SystemDate());
        saveDT = PSdt;
        saveTM = PStime;
       EndSr;

       BegSr MovDftVal;
        // Agency FEIN, Mod, Name
        scFeinE = APfein;
        scModE = APmod;
        scAnamE = %trim(AGname);
        // Producer Number, Name
        scProdE = prod_sav;
        scPnamE = %Trim(lsn_sav) + ', ' + %Trim(fsn_sav);
       EndSr;

      /end-free
     P ProcOthrNonR    E

       //===================================================================
       // ProcOthrProd  - Process other Options for Producers
       //===================================================================
     P ProcOthrProd    B
     D ProcOthrProd    PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdAddProd;
       isexit = *off;
       ReadC sflAgtProd;

       Dow not %eof;

          Select;
          When scProdEdt = *On; // Processing for Edit Producers Info

             reload = *on;
             scProdEdt = *Off;
             mode = 'C';
             svrrnAgtProd = rrnAgtProd;   //Save Rrn value for edited record
             PREdt = *On;                 //disable non-editable field

             Chain(n) scProdRRN WMAPP;

             Exsr MovDbSc;

             scProdHdg = 'Edit Producers Information';

             Dow isexit = *off;
                Exfmt rcdAddProd;

                Select;
                // Save Button
                When BtnProdSav = *on;
                   BtnProdSav = *off;
                   // Check if WMAPP updated since screen displayed (Producers Info)
                   flgRcdUpdated=RcdUpdatedErr('WMAPP':dbrrn4:'APdt':'APtime':
                    'APuser':saveDT2:saveTM2:'1');
                   if flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scProdRRN WMAPP;
                      Exsr MovDbSc;
                   EndIf;
                   if flgRcdUpdated = '0'; // No error
                      If ValidateProd(mode);
                         Chain scProdRRN WMAPP;
                         Exsr MoveScDb;
                         Update pap0p;
                         isexit = *on;
                      EndIf;
                   EndIf;

                // Cancel Button
                When BtnProdCan = *on;
                   BtnProdCan = *off;
                   isexit = *on;
                EndSl;
             EndDo;

             Unlock WMAPP;

          Endsl;

          ReadC sflAgtProd;
       Enddo;

       // Subrotine to move database fields into screen fields
       BegSr MovDbSc;
        scProdPR = APprod;               // Producer
        scLsnPR  = APlsn;                // Last Name
        scFsnPR  = APfsn;                // First Name
        scMad1PR = APmad1;               // Home Address
        scMctyPR = APmcty;               // City
        scMstPR  = APmst;                // State
        scMzipPR = APmzip;               // Zip
        scEmalPR = APemal;               // Email
        // Social Security Number
        If APss# <> 0;
           clear charSS#;
           charSS# = %EditC(APss#:'X');
           scSs#PR = MaskData('SSN':'APss#':charSS#);
        Endif;

        scDobPR = dtMDYY(APdob);         // License Renewal Date
        scCxDtPR = dtMDYY(APcxdt);       // License Renewal Date
        scCommPR = APcomm;               // Comments 1
        scCom2PR = APcom2;               // Comments 2
        scProdStat = APstat;             // Status

        scRstPR  = APrst;                // State
        scLflgPR = APlflg;               // License Flag
        scLic#PR = APlic#;               // License Number
        scNpnPR = APnpn;                 // NPN Number
        scRwDtPR = dtMDYY(APrwdt);       // License Renewal Date
        scExDtPR = dtMDYY(APexdt);       // License Expiry Date
        scLtypPR = APltyp;               // License Type 1
        scLtp2PR = APltp2;               // License Type 2
        scLtp3PR = APltp3;               // License Type 3
        scLtp4PR = APltp4;               // License Type 4
        scLtp5PR = APltp5;               // License Type 5
        scFiDtPR = dtMDYY(APfidt);       // Filed with Carrier Date
        scMlDtPR = dtMDYY(APmldt);       // Mailed to State Date
        scCfDtPR = dtMDYY(APcfdt);       // Confirmed Date
        scDnDtPR = dtMDYY(APdndt);       // Denied Date


        Clear scDisTyp;
        Clear scSubCat;
        Clear scOutCnt;
        Setll (inco#:inFein:inMod:scProdPR:APseq#) pap0pcr;
        Reade(n) (inco#:inFein:inMod:scProdPR:APseq#) pap0pcr;
        Dow not %eof(wmappcrt);
           Select;
           When CR_APcritID = 'Distribution';
              scDisTyp = %trim(scDisTyp) + ',' + %trim(CR_APcritVal);
           When CR_APcritID = 'Sub Category';
              scSubCat = %trim(scSubCat) + ',' + %trim(CR_APcritVal);
           When CR_APcritID = 'Process Output Control';
              scOutCnt = %trim(scOutCnt) + ',' + %trim(CR_APcritVal);
           Endsl;
        Reade(n) (inco#:inFein:inMod:scProdPR:APseq#) pap0pcr;
        Enddo;

        If scDisTyp <> *blank;
           scDisTyp = %subst(scDisTyp:2);
        Endif;
        If scSubCat <> *blank;
           scSubCat = %subst(scSubCat:2);
        Endif;
        If scOutCnt <> *blank;
           scOutCnt = %subst(scOutCnt:2);
        Endif;

        saveDT2 = APdt;
        saveTM2 = APtime;
       EndSr;

       // Subrotine to move Screen fields into database fields
       BegSr MoveScDb;
        APlsn   = scLsnPR;               // Last Name
        APfsn   = scFsnPR;               // First Name
        APmad1  = scMad1PR;              // Home Address
        APmcty  = scMctyPR;              // City
        APmst   = scMstPR;               // State
        APmzip  = scMzipPR;              // Zip
        APemal  = scEmalPR;              // Email
        // Social Security Number
        If scSs#PR <> *Blanks;
           clear charSS#;
           charSS# = UnMaskData('SSN':'APss#':scSs#PR);
           APss# = %dec(charSS#:9:0);
        Endif;

        APdob   = dtCYMD(scDobPR);       // Date of Birth
        APcxdt  = dtCYMD(scCxDtPR);      // Cancellation Date
        APcomm  = scCommPR;              // Comments 1
        APcom2  = scCom2PR;              // Comments 2
        APstat  = scProdStat;            // Status

        APrst   = scRstPR;               // State
        APlflg  = scLflgPR;              // License Flag
        APlic#  = scLic#PR;              // License Number
        APnpn   = scNpnPR;               // NPN Number
        APrwdt  = dtCYMD(scRwDtPR);      // License Renewal Date
        APexdt  = dtCYMD(scExDtPR);      // License Expiry Date
        APltyp  = scLtypPR;              // License Type 1
        APltp2  = scLtp2PR;              // License Type 2
        APltp3  = scLtp3PR;              // License Type 3
        APltp4  = scLtp4PR;              // License Type 4
        APltp5  = scLtp5PR;              // License Type 5
        APfidt  = dtCYMD(scFiDtPR);      // Filed with Carrier Date
        APmldt  = dtCYMD(scMlDtPR);      // Mailed to State Date
        APcfdt  = dtCYMD(scCfDtPR);      // Confirmed Date
        APdndt  = dtCYMD(scDnDtPR);      // Denied Date

        APuser = q1user;
        APpgm  = q1pgm;
        APtime = Timeto6(SystemTime());
        APdt   = Dateto7(SystemDate());

        Setll (inco#:infein:inmod:scProdPR:APseq#) pap0pcr;
        Reade (inco#:infein:inmod:scProdPR:APseq#) pap0pcr;
        Dow not %eof(wmappcrt);
           Delete wmappcrt;
           Reade (inco#:infein:inmod:scProdPR:APseq#) pap0pcr;
        Enddo;
        Unlock wmappcrt;

        Dow scDisTyp <> *blank;
           CR_APcritID  = 'Distribution';
           CR_APcritVal = ParseString(scDisTyp:',':1:'1');
           Exsr Write_CritRec;
        Enddo;
        Dow scSubCat <> *blank;
           CR_APcritID  = 'Sub Category';
           CR_APcritVal = ParseString(scSubCat:',':1:'1');
           Exsr Write_CritRec;
        Enddo;
        Dow scOutCnt <> *blank;
           CR_APcritID  = 'Process Output Control';
           CR_APcritVal = ParseString(scOutCnt:',':1:'1');
           Exsr Write_CritRec;
        Enddo;

        saveDT2 = APdt;
        saveTM2 = APtime;
       EndSr;

       Begsr Write_CritRec;
        CR_APco#   = inco#;
        CR_APfein  = infein;
        CR_APmod   = inmod;
        CR_APprod  = APprod;
        CR_APseq#  = APseq#;
        CR_APstat  = APstat;
        CR_APuser  = q1user;
        CR_APpgm   = q1pgm;
        CR_APtime  = Timeto6(SystemTime());
        CR_APdt    = Dateto7(SystemDate());
        Write pap0pcr;
        Unlock wmappcrt;
       Endsr;

      /end-free
     P ProcOthrProd    E

       //===================================================================
       // ProcRoWClk - Process the Row Click in Producers Grid
       //===================================================================
     P ProcRoWClk      B
     D ProcRoWClk      PI

      /free

       // process to load Non-Resident States Grid for selected row
       Chain rrnAgtProd sflAgtProd;
       //*** Begin Chg ***** 08/17/23 *****************************
       //*** Chain scProdrrn pap0p;
       Chain(n) scProdrrn pap0p;
       //*** End   Chg ***** 08/17/23 *****************************

       //*** Begin Add ***** 08/17/23 *****************************
       // Check if user changed the Producer Status using Activate/Inactivate link
       // Then in SFL, just update the selected row with new status
       svStat = scStat;

       //*** Begin Del ***** 08/28/23 *****************************
       // svActProd = scActProd;
       //*** End   Del ***** 08/28/23 *****************************
       if %found(WMAPP);
          if APstat <> %subst(svStat:1);
             If APstat = 'A';
                scStat = 'Active';
                //*** Begin Chg ***** 08/28/23 *****************************
                // scActProd = 'Inactivate';
                visActSts = *off;                // New Hyperlink for Activate
                visIActSts = *on;
                //*** End   Chg ***** 08/28/23 *****************************
             Endif;
             If APstat = 'I';
                scStat = 'Inactive';
                //*** Begin Chg ***** 08/28/23 *****************************
                // scActProd = 'Activate';
                visActSts = *on;
                visIActSts = *off;               // New Hyperlink for Inactivate
                //*** End   Chg ***** 08/28/23 *****************************
             Endif;

             // Update new values in SFL selected row
             Update sflAgtProd;
          endif;
       endif;
       //*** End   Add ***** 08/17/23 *****************************

       scRowSel = rrnAgtProd;

       co#_sav  = APco#;
       fein_sav = APfein;
       mod_sav  = APmod;
       prod_sav = APprod;
       fsn_sav  = APfsn;
       lsn_sav  = APlsn;
       rst_sav  = APrst;                        // Resident Lic State

      /end-free

     P ProcRoWClk      E

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

       //===================================================================
       // Load display indicators for sub-files in rcdAgtProd Record Format
       //===================================================================
     P RcdAgtProdDsp   b
     D RcdAgtProdDsp   pi
     D  indValue                      1n   const
      /Free
       dspAgtProd = indValue;
       dspNonRes = indValue;
      /End-Free
     P RcdAgtProdDsp   e

      //===================================================================
      // Validate Non-Resident States Info
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateNonR    b
     d ValidateNonR    pi              n
     d strmode                        1
     d isErrNonREdt    s               n   inz
      /Free

       isErrNonREdt = *off;
      // Intialize all the messageId and error indicators.
       erStENR = *off;
       msStENR = *blanks;
       erRwDtE = *off;
       msRwDtE = *blanks;
       erExDtE = *off;
       msExDtE = *blanks;
       erFiDtE = *off;
       msFiDtE = *blanks;
       erMlDtE = *off;
       msMlDtE = *blanks;
       erCfDtE = *off;
       msCfDtE = *blanks;
       erDnDtE = *off;
       msDnDtE = *blanks;

       If strmode= 'A';                //Add Mode
          // State should not be blank
          If scStENR = *Blanks;
             erStENR = *on;
             msStENR = 'WCR0503';
             isErrNonREdt = *on;
          Endif;

          // Validation check for Duplicate record
          If isErrNonREdt = *off;
             Chain key2WMPSL pps00;
             If %found(wmpsl);
                erStENR = *on;
                msStENR = 'WC30005';
                isErrNonREdt = *on;
             Endif;
          Endif;

          // Non-Resident State cannot be same as the Resident state (in WMAPP)
          If isErrNonREdt = *off;
             If scStENR = rst_sav;          // Resident Lic State
                erStENR = *on;
                msStENR = 'WC30093';
                isErrNonREdt = *on;
             Endif;
          Endif;
       Endif;

       // Validation Check for Renewwal Date
       If scRwDtE <> *Zeros;
          erRwDtE = ValidDate(scRwDtE:charMsg);
          If erRwDtE = *on;
             msRwDtE = charMsg;
             isErrNonREdt = *on;
          Endif;
       Endif;

       // Validation Check for Expiration Date
       If scExDtE <> *Zeros;
          erExDtE = ValidDate(scExDtE:charMsg);
          If erExDtE = *on;
             msExDtE = charMsg;
             isErrNonREdt = *on;
          Endif;
       Endif;

       // Validation Check for Filed with Carrier Date
       If scFiDtE <> *Zeros;
          erFiDtE = ValidDate(scFiDtE:charMsg);
          If erFiDtE = *on;
             msFiDtE = charMsg;
             isErrNonREdt = *on;
          Endif;
       Endif;

       // Validation Check for Mailed to State Date
       If scMlDtE <> *Zeros;
          erMlDtE = ValidDate(scMlDtE:charMsg);
          If erMlDtE = *on;
             msMlDtE = charMsg;
             isErrNonREdt = *on;
          Endif;
       Endif;

       // Validation Check for Confirmed Date
       If scCfDtE <> *Zeros;
          erCfDtE = ValidDate(scCfDtE:charMsg);
          If erCfDtE = *on;
             msCfDtE = charMsg;
             isErrNonREdt = *on;
          Endif;
       Endif;

       // Validation Check for Denied Date
       If scDnDtE <> *Zeros;
          erDnDtE = ValidDate(scDnDtE:charMsg);
          If erDnDtE = *on;
             msDnDtE = charMsg;
             isErrNonREdt = *on;
          Endif;
       Endif;


      // Return *on indicator when No error exists.
       If isErrNonREdt = *on;
          return *off;
       Else;
          return *on;
       Endif;

      /End-Free
     p ValidateNonR    e

      //===================================================================
      // Validate Producers Info
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateProd    b
     d ValidateProd    pi              n
     d strmode                        1
     d isErrProdEdt    s               n   inz
      /Free

       isErrProdEdt = *off;
      // Intialize all the messageId and error indicators.
       erProdPR = *off;
       msProdPR = *blanks;
       erLsnPR = *off;
       msLsnPR = *blanks;
       erMad1PR = *off;
       msMad1PR = *blanks;
       erMctyPR = *off;
       msMctyPR = *blanks;
       erMStPR = *off;
       msMStPR = *blanks;
       erMzipPR = *off;
       msMzipPR = *blanks;
       erEmalPR = *off;
       mgEmalPR = *blanks;
       erRstPR = *off;
       msRstPR = *blanks;
       erDobPR = *off;
       msDobPR = *blanks;
       erCxDtPR = *off;
       msCxDtPR = *blanks;
       erRwDtPR = *off;
       msRwDtPR = *blanks;
       erExDtPR = *off;
       msExDtPR = *blanks;
       erFiDtPR = *off;
       msFiDtPR = *blanks;
       erMlDtPR = *off;
       msMlDtPR = *blanks;
       erCfDtPR = *off;
       msCfDtPR = *blanks;
       erDnDtPR = *off;
       msDnDtPR = *blanks;

       If strmode = 'A';                //Add Mode
          // Producer cannot be Zero
          If scProdPR = 0;
             erProdPR = *on;
             msProdPR = 'WCR0502';
             isErrProdEdt = *on;
          Endif;

          // Validation check for Duplicate record
          If isErrProdEdt = *off;
             Chain (inCo#:inFein:inMod:scProdPR) pap00;
             If %found(wmapl);
                erProdPR = *on;
                msProdPR = 'WC30005';
                isErrProdEdt = *on;
             Endif;
          Endif;
       Endif;

       // Last Name should not be blank
       If scLsnPR = *Blanks;
          erLsnPR = *on;
          msLsnPR = 'WCR0503';
          isErrProdEdt = *on;
       Endif;

      // Enter complete info for Address, if any of the field is entered
       If scMad1PR <> *blanks or scMctyPR <> *blanks or
        scMstPR <> *blanks or scMzipPR <> 0;
          If scMad1PR = *Blanks;
             erMad1PR = *on;
             msMad1PR = 'WCR0503';
             isErrProdEdt = *on;
          Endif;
          If scMctyPR = *blanks;
             erMctyPR = *on;
             msMctyPR = 'WCR0503';
             isErrProdEdt = *on;
          Endif;
          If scMstPR = *blanks;
             erMstPR = *on;
             msMstPR = 'WCR0503';
             isErrProdEdt = *on;
          Endif;
          If scMzipPR = *zeros;
             erMzipPR = *on;
             msMzipPR = 'WCR0556';
             isErrProdEdt = *on;
          Endif;
       Endif;

       // Validate E-Mail Address
       If scEmalPR <> *Blanks;
          erEmalPR = Check_Email(scEmalPR:mgEmalPR);
          If erEmalPR = *on;
             isErrProdEdt = *on;
          Endif;
       Endif;

       // Resident State should not be blank
       If scRstPR = *Blanks;
          erRstPR = *on;
          msRstPR = 'WCR0503';
          isErrProdEdt = *on;
       Endif;

       // Resident State cannot be in the Non-Resident state File (in WMPSP)
       If isErrProdEdt = *off;
          Chain (inCo#:inFein:inMod:scProdPR:scRstPR) pps00;
          If %found(wmpsl);
             erRstPR = *on;
             msRstPR = 'WC30094';
             isErrProdEdt = *on;
          Endif;
       Endif;

       // Validation Check for Date of Birth
       If scDobPR <> *Zeros;
          erDobPR = ValidDate(scDobPR:charMsg);
          If erDobPR = *on;
             msDobPR = charMsg;
             isErrProdEdt = *on;
          Endif;
       Endif;

       // Validation Check for Cancellation Date
       If scCxDtPR <> *Zeros;
          erCxDtPR = ValidDate(scCxDtPR:charMsg);
          If erCxDtPR = *on;
             msCxDtPR = charMsg;
             isErrProdEdt = *on;
          Endif;
       Endif;

       // Validation Check for Renewwal Date
       If scRwDtPR <> *Zeros;
          erRwDtPR = ValidDate(scRwDtPR:charMsg);
          If erRwDtPR = *on;
             msRwDtPR = charMsg;
             isErrProdEdt = *on;
          Endif;
       Endif;

       // Validation Check for Expiration Date
       If scExDtPR <> *Zeros;
          erExDtPR = ValidDate(scExDtPR:charMsg);
          If erExDtPR = *on;
             msExDtPR = charMsg;
             isErrProdEdt = *on;
          Endif;
       Endif;

       // Validation Check for Filed with Carrier Date
       If scFiDtPR <> *Zeros;
          erFiDtPR = ValidDate(scFiDtPR:charMsg);
          If erFiDtPR = *on;
             msFiDtPR = charMsg;
             isErrProdEdt = *on;
          Endif;
       Endif;

       // Validation Check for Mailed to State Date
       If scMlDtPR <> *Zeros;
          erMlDtPR = ValidDate(scMlDtPR:charMsg);
          If erMlDtPR = *on;
             msMlDtPR = charMsg;
             isErrProdEdt = *on;
          Endif;
       Endif;

       // Validation Check for Confirmed Date
       If scCfDtPR <> *Zeros;
          erCfDtPR = ValidDate(scCfDtPR:charMsg);
          If erCfDtPR = *on;
             msCfDtPR = charMsg;
             isErrProdEdt = *on;
          Endif;
       Endif;

       // Validation Check for Denied Date
       If scDnDtPR <> *Zeros;
          erDnDtPR = ValidDate(scDnDtPR:charMsg);
          If erDnDtPR = *on;
             msDnDtPR = charMsg;
             isErrProdEdt = *on;
          Endif;
       Endif;


      // Return *on indicator when No error exists.
       If isErrProdEdt = *on;
          return *off;
       Else;
          return *on;
       Endif;

      /End-Free
     p ValidateProd    e

      // utility procedure definitions
      /COPY SCOPYCOMPI

