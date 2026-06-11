     hcopyright('(c) 2024 Sapiens, Inc. All rights reserved.')
      //*********************************************************************
     h/title WTOFFICE - Office Maintenance
     h dftname(WTOFFICE)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h ALWNULL(*UsrCtl)
     h/define   profoundUI
     h bnddir('STBNDDIR')
     h bnddir('WTGRPCFG')
      //*********************************************************************
      //==============================================================*
      //  Created   : 01/10/24                                         *
      //  Programmer: C:342                                            *
      //  Project   : WC2940 - Redesign of Work with Office information*
      //                                                               *
      //===============================================================*
      //  Revisions:                                                   *
      //                                                               *
      //  04/03/24 C:342 Project: WC2940, Support: 115342              *
      //                 1. Loaded grid with Office# order.            *
      //                 2. Added Horizontal line in display file      *
      //                    below Office#.
      //                 3. Change Label heading at bottom in detail   *
      //                    box.                                       *
      //                 4. Allow upper case and lower case input for  *
      //                    Premium Notices, Monthly Interim and Late  *
      //                    Notices.                                   *
      //                                                               *
      // ------------------------------------------------------------- *
      //  NOTE: Please add comments to the top of Revisions.           *
      // ------------------------------------------------------------- *
      //===============================================================*

     fWtOffice  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflOffice:scSflRRN)
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)
      * Company file
     fsmcol     if   e           k disk    rename(pco01:pco00)
      * User Parameter File
     fsmupl     if   e           k disk    rename(pup01:pup00)
      * Office Master File
     fwmofl     if   e           k disk    rename(pof01:pof00)
     f                                     infds(dbfds)
     fwmofp     uf a e             disk    rename(pof01:pof0p)
     f                                     infds(dbfds1)
      * Application System Control File
     fssylf     if   e           k disk    include(ssy01)
     f                                     include(swc01)

     d dbfds           ds
     d  dbrrn                397    400b 0
     d dbfds1          ds
     d  dbrrn1               397    400b 0

     d psds          esds                  extname(szq1p)

      // work variables
     d saveDT          s                   like(OFdt)
     d saveTM          s                   like(OFtime)
     d flgRcdUpdated   s              1    inz
     d svsflrrn        s              5  0 inz
     d kkOfc#          s                   like(OFofc#) inz(*loval)

     d intCo#          s              3P 0
     d chgMod          s               n   inz(*off)
     d dspMode         s               n   inz(*off)
     d process         s               n   inz(*off)
     d isError         s               n   inz
     d xxClmPhnE       s              6  0
     d valNum          c                   const('0123456789')
     d isEFTVrb        s               n   inz(*off)
     d is3rdVrb        s               n   inz(*off)

      // work variables for CMS check
     d                 ds
     d  syfl12                 1     10
     d  xxCms                  2      2

      * Load Office Information
     d LoadOffice      pr

      * Main Processing
     d Main            pr

      * Process Addition of Office Information
     d ProcessAdd      pr

      * Process Edit option for Office Information
     d ProcOther       pr

      * Program Start
     d ProgramStart    pr

      * Validate Office Information
     d ValidateOffice  pr              n
     d strmode                        1

     d/copy sPrDate
     d/copy sPrGenPop
     d/copy sPrCompany
     d/copy sPrUsrPrms
     d/copy sPrGenErr
     d/copy sPrModule

      // Entry of Program.

     c     *entry        plist
     c                   parm                    @@rrn             9            rrn
     c                   parm                    @@mode            1            mod
     c                   parm                    @@next           10            next pgm
     c                   parm                    @@TabInd          5            tab index


      /Free

       ProgramStart();

       If process;
          Main();
       Endif;

       *InLr = *On;

      /End-Free

       //===================================================================
       // Load Office Information
       //===================================================================
     p LoadOffice      b
     d LoadOffice      pi

      /free
       // Clear the subfile
       scSflRRN = 0;
       scSflDsp = *off;
       scSflClr = *on;
       Write rcdOffice;
       scSflClr = *off;
       scSflDsp = *on;
       scRowSel = 1;

       // Populate the subfile
       //*** Beg Chg **** 04/03/24 *******************************
       //*** Setll *loval Wmofl;
       //*** Read Wmofl;
       Setll (intCo#:kkOfc#) Wmofl;
       Reade intCo# Wmofl;
       //***  End Chg **** 04/03/24 *******************************
       Dow not %eof(Wmofl);

          scSflRRN += 1;
          scSflSel = *off;
 
          If svsflrrn = 0;
             If scSflRRN = 1;
                scSflSel = *on;
             Endif;
          Else;
             If scSflRRN = svsflrrn;
                scSflSel = *on;
             Endif;
          Endif;
 
          scOfc#   = OFofc#;
          scName   = OFname;
          visActSts = *off;                // New Hyperlink for Activate
          visIActSts = *off;               // New Hyperlink for Inactivate
          If OFstat = 'A';
             scStatus = 'Active';
             visIActSts = *on;
          Endif;
          If OFstat = 'I';
             scStatus = 'Inactive';
             visActSts = *on;
          Endif;

          If dspMode;
             visActSts = *off;
             visIActSts = *off;
          Endif;

          scRRN = dbrrn;
          Write sflOffice;
          //*** Beg Chg **** 04/03/24 *******************************
          //*** Read Wmofl;
          Reade intCo# Wmofl;
          //*** End Chg **** 04/03/24 *******************************
       Enddo;

       scSflRRN = 1;
       If svSflRRN <> 0;
          scSflRRN = svSflRRN;
          scRowSel = svSflRRN;
          svSflRRN = 0;
       Endif;

      /end-free
     p LoadOffice      e

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

      /Free

       // Process until exit button is pressed.
       BtnCancel = *off;

       // Process until exit button is pressed.
       DoU BtnCancel = *On;

          // Load Office Information
          LoadOffice();

          // Display Screen
          Exsr ExfmtScreens;

          // Perform actions on screen.
          Select;

          // Add Office Information
          When BtnOfcAdd  = *On;
             BtnOfcAdd = *Off;
             ProcessAdd();

          // Refresh
          When btnRefresh = *On;
             btnRefresh = *Off;

          // Save
          When btnSave = *On;
             btnSave = *Off;

          Other;
             ProcOther();             // Process other Options Office Information

          EndSl;

       Enddo;

       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

        Select;

        When TabIndex = 0;
           PnlHeight = 540;
           APnlHeight = PnlHeight + 60;

           Write rcdComPnl;
           Write rcdPanel;

           Exfmt rcdOffice;

           Read rcdComPnl;
           Read rcdPanel;
        EndSl;

       Endsr;

      /End-Free

     p Main            e

       //===================================================================
       // ProcessAdd - Process Addition of Office Information
       //===================================================================
     P ProcessAdd      B
     D ProcessAdd      PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdOfcAdd;
       visEFTVrb = isEFTVrb;
       vis3rdVrb = is3rdVrb;
       mode = 'A';
       isexit = *off;
       BtnCanOfc = *off;
       scStatusE = 'A';
       scFocus = *off;

       Dow BtnCanOfc = *off and isexit = *off;

          scOfcHdg = 'Add Office Information';

          Exfmt rcdOfcAdd;
          Select;
          // Save Button
          When BtnSaveOfc = *on;
             BtnSaveOfc = *off;
             If ValidateOffice(mode);
                Exsr WrtOffice;
                isexit = *on;
             EndIf;

          // Cacel Button
          When BtnCanOfc = *on;
             BtnCanOfc = *off;
             isexit = *on;
          EndSl;
       Enddo;

       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        //CBdate  = dtCYMD(scBlkDateE);
        OFco#   = intCo#;                // Company Number
        OFstat  = scStatusE;             // Status
        OFofc#  = scOfc#E;               // Office #
        OFemail = scmailE;               // Office Email

        // Office Information
        OFname  = scOfcNameE;            // Office name
        OFadr1  = scOfcAdr1E;            // Office Address1
        OFadr2  = scOfcAdr2E;            // Office Address1
        OFcty   = scOfcCtyE;             // Office City
        OFst    = scOfcStE;              // Office State
        OFzip   = scOfcZipE;             // Office Zip
        If scOfcPhnE> *zeros;
           OFarea  = %dec(%subst(%editc(scOfcPhnE:'X'):1:3):3:0); // Phone  Area
           OFphn#  = %dec(%subst(%editc(scOfcPhnE:'X'):4):7:0);   // Phone  Num
        EndIf;
        If scOfcFaxE> *zeros;
           OFfara  = %dec(%subst(%editc(scOfcFaxE:'X'):1:3):3:0); // Phone  Area
           OFfax#  = %dec(%subst(%editc(scOfcFaxE:'X'):4):7:0);   // Phone  Num
        EndIf;

        // Accounting Company information
        OFacnm  = scAccNameE;            // Acct Compnay Name
        OFaad1  = scAccAdr1E;            // Acct Compnay Address1
        OFaad2  = scAccAdr2E;            // Acct Compnay Address1
        OFacty  = scAccCtyE;             // Acct Compnay City
        OFast   = scAccStE;              // Acct Compnay State
        OFazip  = scAccZipE;             // Acct Compnay Zip
        If scAccPhnE> *zeros;
           OFaara  = %dec(%subst(%editc(scAccPhnE:'X'):1:3):3:0); // Phone  Area
           OFaphn  = %dec(%subst(%editc(scAccPhnE:'X'):4):7:0);   // Phone  Num
        EndIf;
        If scAccFaxE> *zeros;
           OFafxa  = %dec(%subst(%editc(scAccFaxE:'X'):1:3):3:0); // Phone  Area
           OFafx#  = %dec(%subst(%editc(scAccFaxE:'X'):4):7:0);   // Phone  Num
        EndIf;

        // Claim Office information
        OFcnam  = scClmNameE;            // Claim Office Name
        OFcad1  = scClmAdr1E;            // Claim Office Address1
        OFcad2  = scClmAdr2E;            // Claim Office Address1
        OFccty  = scClmCtyE;             // Claim Office City
        OFcst   = scClmStE;              // Claim Office State
        OFczp   = scClmZipE;             // Claim Office Zip
        If scClmPhnE> *zeros;
           OFcara  = %dec(%subst(%editc(scClmPhnE:'X'):1:3):3:0); // Phone  Area
           OFcph1  = %dec(%subst(%editc(scClmPhnE:'X'):4:3):3:0);   // Phone  Num
           OFcph2  = %dec(%subst(%editc(scClmPhnE:'X'):7:4):4:0);   // Phone  Num
        Endif;

        // Move other Additonal fields
        OFcknm  = scCkNm;                // Make checks payable to name
        OFeftv  = scEFTV;                // EFT Verbiage
        OFlnnm  = scLnnm;                // Late notices inquiry direction
        OFprnm  = scPrnm;                // Premium notices inquiry direction
        OFminm  = scMinm;                // Monthly interim inquiry direction
        OF3rdvrb= sc3rdVrb;              // 3rd-Party Payment Verbiage

        OFuser  = q1user;                // User
        OFpgm   = q1pgm;                 // Program
        OFtime  = Timeto6(SystemTime()); // Time
        OFdt    = Dateto7(SystemDate()); // Date Mintained
        OFiusr  = q1user;                // Initial User
        OFipgm  = q1pgm;                 // Initial Program
        OFitm   = Timeto6(SystemTime()); // Initial Time
        OFidt   = Dateto7(SystemDate()); // Initial Date

       EndSr;

       // Writing new Office Information
       BegSr WrtOffice;
        Clear pof0p;
        Exsr MoveScDb;
        Write pof0p;
       EndSr;

      /end-free
     P ProcessAdd      E

       //===================================================================
       // ProcOther - Process other Options for Office Information
       //===================================================================
     P ProcOther       B
     D ProcOther       PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdOfcAdd;
       visEFTVrb = isEFTVrb;
       vis3rdVrb = is3rdVrb;
       isexit = *off;

       ReadC sflOffice;
       Dow not %eof;

          Select;
          When scOfcEdt = *On; // Processing for Edit
             scOfcEdt = *Off;
             Chain(e) scRRN pof0p;
             If %status = 1218;
                PopOneBtn('Record locked by another user. Please try again-
                 later.':'Record Lock');
                Return;
             Endif;

             mode = 'C';
             svsflrrn = scSflrrn;  //save rrn value
             Chain(n) scRRN pof0p;
             Exsr Movdbsc;
             scOfcHdg = 'Edit Office Information';
             dsbOfc#  = *on;
             scFocus = *on;
             Dow isexit = *off;
                Exfmt rcdOfcAdd;

                Select;
                When BtnSaveOfc = *on;
                   BtnSaveOfc = *off;
                   // Check if Wmofp updated since screen displayed
                   flgRcdUpdated=RcdUpdatedErr('WMOFP':dbrrn1:'OFdt':'OFtime':
                    'OFuser':saveDT:saveTM:'1');
                   if flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scRRN pof0p;
                      Exsr Movdbsc;
                   EndIf;
                   If flgRcdUpdated = '0'; // No error
                      // Validate the Office Information
                      If ValidateOffice(mode);
                         Chain scRRN pof0p;
                         Exsr Movscdb;
                         update pof0p;
                         isExit = *on;
                      Endif;
                   Endif;
                When BtnCanOfc = *on;
                   BtnCanOfc = *off;
                   isExit = *on;
                EndSl;
             Enddo;

          Endsl;

          Unlock Wmofp;

          ReadC sflOffice;
       Enddo;

       // Subrotine to move database fields into screen fields
       BegSr MovDbsc;
        scOfc#E = OFofc#;
        scmailE = OFemail;
        scStatusE = OFstat;

        // Office Information
        scOfcNameE  = OFname;
        scOfcAdr1E  = OFadr1;
        scOfcAdr2E  = OFadr2;
        scOfcCtyE   = OFcty;
        scOfcStE    = OFst;
        scOfcZipE   = OFzip;
        scOfcPhnE   = OFarea * 10000000 + OFphn#;
        scOfcFaxE   = OFfara * 10000000 + OFfax#;

        // Acct Company Information
        scAccNameE  = OFacnm;
        scAccAdr1E  = OFaad1;
        scAccAdr2E  = OFaad2;
        scAccCtyE   = OFacty;
        scAccStE    = OFast;
        scAccZipE   = OFazip;
        scAccPhnE   = OFaara * 10000000 + OFaphn;
        scAccFaxE   = OFafxa * 10000000 + OFafx#;

        // Claim Office Information
        scClmNameE  = OFcnam;
        scClmAdr1E  = OFcad1;
        scClmAdr2E  = OFcad2;
        scClmCtyE   = OFccty;
        scClmStE    = OFcst;
        scClmZipE   = OFczp;
        xxClmPhnE   = OFcara * 1000 + OFcph1;
        scClmPhnE   = xxClmPhnE * 10000 + OFcph2;

        // Move other Additional Fields
        scCkNm  = OFcknm;      // Make checks payable to name
        scEFTV  = OFeftv;      // EFT Verbiage
        scLnnm  = OFlnnm;      // Late notices inquiry direction
        scPrnm  = OFprnm;      // Premium notices inquiry direction
        scMinm  = OFminm;      // Monthly interim inquiry direction
        sc3rdVrb= OF3rdvrb;    // 3rd-Party Payment Verbiage

        saveDT = OFdt;
        saveTM = OFtime;
       EndSr;


       // Subrotine to move screen field to database file
       BegSr Movscdb;

        // Office Information
        OFemail = scmailE;               // Office Email
        OFstat  = scStatusE;             // Status
        OFname  = scOfcNameE;            // Office name
        OFadr1  = scOfcAdr1E;            // Office Address1
        OFadr2  = scOfcAdr2E;            // Office Address1
        OFcty   = scOfcCtyE;             // Office City
        OFst    = scOfcStE;              // Office State
        OFzip   = scOfcZipE;             // Office Zip
        If scOfcPhnE> *zeros;
           OFarea  = %dec(%subst(%editc(scOfcPhnE:'X'):1:3):3:0); // Phone  Area
           OFphn#  = %dec(%subst(%editc(scOfcPhnE:'X'):4):7:0);   // Phone  Num
        EndIf;
        If scOfcFaxE> *zeros;
           OFfara  = %dec(%subst(%editc(scOfcFaxE:'X'):1:3):3:0); // Phone  Area
           OFfax#  = %dec(%subst(%editc(scOfcFaxE:'X'):4):7:0);   // Phone  Num
        EndIf;

        // Accounting Company information
        OFacnm  = scAccNameE;            // Acct Compnay Name
        OFaad1  = scAccAdr1E;            // Acct Compnay Address1
        OFaad2  = scAccAdr2E;            // Acct Compnay Address1
        OFacty  = scAccCtyE;             // Acct Compnay City
        OFast   = scAccStE;              // Acct Compnay State
        OFazip  = scAccZipE;             // Acct Compnay Zip
        If scAccPhnE> *zeros;
           OFaara  = %dec(%subst(%editc(scAccPhnE:'X'):1:3):3:0); // Phone  Area
           OFaphn  = %dec(%subst(%editc(scAccPhnE:'X'):4):7:0);   // Phone  Num
        EndIf;
        If scAccFaxE> *zeros;
           OFafxa  = %dec(%subst(%editc(scAccFaxE:'X'):1:3):3:0); // Phone  Area
           OFafx#  = %dec(%subst(%editc(scAccFaxE:'X'):4):7:0);   // Phone  Num
        EndIf;

        // Claim Office information
        OFcnam  = scClmNameE;            // Claim Office Name
        OFcad1  = scClmAdr1E;            // Claim Office Address1
        OFcad2  = scClmAdr2E;            // Claim Office Address1
        OFccty  = scClmCtyE;             // Claim Office City
        OFcst   = scClmStE;              // Claim Office State
        OFczp   = scClmZipE;             // Claim Office Zip
        If scClmPhnE> *zeros;
           OFcara  = %dec(%subst(%editc(scClmPhnE:'X'):1:3):3:0); // Phone  Area
           OFcph1  = %dec(%subst(%editc(scClmPhnE:'X'):4:3):3:0);   // Phone  Num
           OFcph2  = %dec(%subst(%editc(scClmPhnE:'X'):7:4):4:0);   // Phone  Num
        Endif;

        // Move other Additonal fields
        OFcknm  = scCkNm;                // Make checks payable to name
        OFeftv  = scEFTV;                // EFT Verbiage
        OFlnnm  = scLnnm;                // Late notices inquiry direction
        OFprnm  = scPrnm;                // Premium notices inquiry direction
        OFminm  = scMinm;                // Monthly interim inquiry direction
        OF3rdvrb= sc3rdVrb;              // 3rd-Party Payment Verbiage

        OFuser  = q1user;                // User
        OFpgm   = q1pgm;                 // Program
        OFtime  = Timeto6(SystemTime()); // Time
        OFdt    = Dateto7(SystemDate()); // Date Mintained

        saveDT = OFdt;
        saveTM = OFtime;
       EndSr;

      /end-free
     P ProcOther       E

       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /Free
       // Program Mode.
       If q1prms >= 2;
          Select;
          When @@mode = 'D';
             dspMode = *on;
          When @@mode = 'C';
             chgMod  = *on;
          Endsl;
       Endif;

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
       Chain intCo# pco00;
       If %found(smcol) and COname <> ' ';
          scCompName = COname;
       Endif;

       // Retrieve user parameter
       Chain q1user pup00;

       // Get Flags value from Workers Comp control file swcpf
       Chain 'WC' swc01;
       If %found(ssylf) and WCfl24 = 'Y';
          visEFTVrb = *on;
          isEFTVrb = *on;
       Else;
          visEFTVrb = *off;
          isEFTVrb = *off;
       Endif;

       // Let's see if Invoice Cloud is Installed
       is3rdVrb = isModEnabled('Invoice Cloud');
       vis3rdVrb = is3rdVrb;

       process = *on;

       Return;

      /End-Free

     p ProgramStart    e

      //===================================================================
      // Validate Office Information
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateOffice  b
     d ValidateOffice  pi              n
     d strmode                        1
     d xxnumber        s              1P 0

      /Free

       // Initialize all the messageId and error indicators.
        isError = *off;
        erOfc#E    = *off;
        msOfc#E    = *blanks;
        ermailE    = *off;
        msgmailE   = *blanks;
        erOfcNameE = *off;
        msOfcNameE = *blanks;
        erOfcAdr1E = *off;
        msOfcAdr1E = *blanks;
        erOfcCtyE  = *off;
        msOfcCtyE  = *blanks;
        erOfcStE   = *off;
        msOfcStE   = *blanks;
        erOfcZipE  = *off;
        msOfcZipE  = *blanks;

        erAccNameE = *off;
        msAccNameE = *blanks;
        erAccAdr1E = *off;
        msAccAdr1E = *blanks;
        erAccCtyE  = *off;
        msAccCtyE  = *blanks;
        erAccStE   = *off;
        msAccStE   = *blanks;
        erAccZipE  = *off;
        msAccZipE  = *blanks;

        erClmNameE = *off;
        msClmNameE = *blanks;
        erClmAdr1E = *off;
        msClmAdr1E = *blanks;
        erClmCtyE  = *off;
        msClmCtyE  = *blanks;
        erClmStE   = *off;
        msClmStE   = *blanks;
        erClmZipE  = *off;
        msClmZipE  = *blanks;

        erCkNm     = *off;
        msCkNm     = *blanks;

        If strmode = 'A';
           //Office# cannot be blank
           If scOfc#E = *blanks;
              erOfc#E = *on;
              msOfc#E = 'WCR0503';
              isError = *on;
           Endif;

           //if CMS module is installed then office number can not have character
           //and it is going to be right justified.
           If erOfc#E = *off;
              Chain 'SY' ssy01;
              If %found(ssylf);
                 If xxCms = 'Y';
                    xxnumber = *zeros;
                    xxnumber = %check(ValNum:scOFc#E);
                    If xxnumber > 0;
                       erOfc#E = *on;
                       msOfc#E = 'WC40103';
                       isError = *on;
                    Endif;
                 Endif;
              Endif;
           Endif;

           //Office# duplicate entry not allowed
           If erOfc#E = *off;
              Chain (intco#:scOfc#E) pof00;
              If %found(wmofl);
                 erOfc#E = *on;
                 msOfc#E = 'WC30005';
                 isError = *on;
              Endif;
           Endif;

        Endif;

        //Office mail should be valid mail
        If scmailE <> *blanks;
           ermailE = Check_Email(scmailE:msgmailE);
           If ermailE  = *on;
              msgmailE = 'Please Enter Valid Email.';
              isError = *on;
           Endif;
        Endif;

        // Check if Office information is entered
        If scOfcNameE = *blanks;
           erOfcNameE = *on;
           msOfcNameE = 'WCR0503';
           isError = *on;
        Endif;

        //Office address cannot be blank
        If scOfcAdr1E = *blanks;
           erOfcAdr1E = *on;
           msOfcAdr1E = 'WCR0503';
           isError = *on;
        Endif;

        //Office City cannot be blank
        If scOfcCtyE = *blanks;
           erOfcCtyE = *on;
           msOfcCtyE = 'WCR0503';
           isError = *on;
        Endif;

        //Office State cannot be blank
        If scOfcStE = *blanks;
           erOfcStE = *on;
           msOfcStE = 'WCR0503';
           isError = *on;
        Endif;

        //Office zip cannot be zero
        If scOfcZipE = *zeros;
           erOfcZipE = *on;
           msOfcZipE = 'WCR0556';
           isError = *on;
        Endif;

        // Check if Acct Company is entered
        If scAccNameE = *blanks;
           erAccNameE = *on;
           msAccNameE = 'WCR0503';
           isError = *on;
        Endif;

        // Check if Acct Company address is blank
        If scAccAdr1E = *blanks;
           erAccAdr1E = *on;
           msAccAdr1E = 'WCR0503';
           isError = *on;
        Endif;

        // Check if Acct Company city is blank
        If scAccCtyE = *blanks;
           erAccCtyE = *on;
           msAccCtyE = 'WCR0503';
           isError = *on;
        Endif;

        // Check if Acct Company State is blank
        If scAccStE = *blanks;
           erAccStE = *on;
           msAccStE = 'WCR0503';
           isError = *on;
        Endif;

        // Check if Acct Company zip is zero
        If scAccZipE = *zeros;
           erAccZipE = *on;
           msAccZipE = 'WCR0556';
           isError = *on;
        Endif;

        // Check Other info entered
        If scCkNm = *blanks;
           erCkNm = *on;
           msCkNm = 'WCR0503';
           isError = *on;
        Endif;

        // Enter complete info for Physical address, if any of the field is entered
        If scClmAdr1E <> *blanks or scClmAdr2E <> *blanks or
         scClmCtyE <> *blanks or scClmStE <> *blanks or scClmZipE <> 0 or
         scClmNameE <> *blanks;
           If scClmNameE = *blanks;
              erClmNameE = *on;
              msClmNameE = 'WCR0503';
              isError = *on;
           Endif;

           // Check if Claim Office address is blank
           If scClmAdr1E = *blanks;
              erClmAdr1E = *on;
              msClmAdr1E = 'WCR0503';
              isError = *on;
           Endif;

           // Check if Claim Office City is blank
           If scClmCtyE = *blanks;
              erClmCtyE = *on;
              msClmCtyE = 'WCR0503';
              isError = *on;
           Endif;

           // Check if Claim Office State is blank
           If scClmStE = *blanks;
              erClmStE = *on;
              msClmStE = 'WCR0503';
              isError = *on;
           Endif;

           // Check if Claim Office zip is zero
           If scClmZipE = *zeros;
              erClmZipE = *on;
              msClmZipE = 'WCR0556';
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
     p ValidateOffice  e

