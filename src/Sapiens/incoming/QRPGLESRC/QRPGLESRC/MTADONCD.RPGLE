     Hcopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H**********************************************************************
     H/TITLE MTADONCD - CMS Addon Code Maintenance
     H DFTNAME(MTADONCD)
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO)
     H/DEFINE   PROFOUNDUI
     h**********************************************************************
      *===============================================================*
      *  Created   : 06/03/19                                         *
      *  Programmer: C:212                                            *
      *  Project   : NY046                                            *
      *===============================================================*
      *  Revisions:                                                   *
      *                                                               *
      *   01/30/23 C:342  Project : FS664                             *
      *                   2023 Medicare Physician Fee Schedule Update *
      *                                                               *
      *   09/02/19 C:212  Support : 82493                             *
      *                   ** Change DSPF and program for below :      *
      *                    * Fix record lock when records > 9999      *
      *                    * Load maximum 999 records instead of 9999 *
      *                    * Show message in DSPF when > 999 records  *
      *                    * Change DSPF filter to show 'From/To Code'*
      *                    * Show screen when no records in file      *
      *                                                               *
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: Please add comments to the top of Revisions.           *
      * ------------------------------------------------------------- *
      *===============================================================*

     fMTADONCD  cf   e             workstn extfile('MTADONCD')
     f                                     HANDLER('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflAddOnCd:rrnAddOnCd)
     fstactpnl  cf   e             workstn handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdSysTbl)
     ffmaop     uf a e             disk    rename(pao01:pao0p)
     f                                     infds(dbfds1)
     ffmaol1    if   e           k disk    rename(pao01:pao01)

     d* procedure definitions
     d*

     d*
     d RunCommand      pr                  extPgm('QCMDEXC')
     d inCmd                       2750    const
     d inCmdLen                      15  5 const

     d* data structures
     d*
     d psds          esds                  extname(szq1p)

     d dspFilter       ds                  likerec(RcdAOFltr)

     d dbfds1          ds
     d  rrnfmaop             397    400b 0

     d                 ds
     d  @@cmd                  1     56
     d  @@st                  38     39

     d* array definitions
     d*

     d* constants
     d*
     d AlphaNum        c                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ+
     d                                     abcdefghijklmnopqrstuvwxyz+
     d                                     0123456789'
     d* standalone variables
     d*

     D inCmd           s           2750
     D inCmdLen        s             15  5

     d @@mode          s              1
     d ##mode          s              1    inz
     d mode            s              1    inz
     d addMode         s               n   inz(*off)
     d chgMode         s               n   inz(*off)
     d dspMode         s               n   inz(*off)
     d isValid         s               n   inz(*off)
     d process         s               n   inz(*off)
     d xxRrn           s              9  0 inz
      ***** Beg Add *** 01/30/23 ***************************************
     d sv_scAOTypeE    s                   like(AOeditType) inz
      ***** End Add *** 01/30/23 ***************************************
     d sv_scAddAOCd    s                   like(AOaddon) inz
     d sv_scAddPriCd   s                   like(AOprim) inz
     d sv_scAddEffDt   s                   like(AOefdt) inz
     d sv_scAddDelDt   s                   like(AOdldt) inz
     d isFirstLoad     s               n   inz(*off)
     d meetFltr        s               n   inz(*off)

     d* Clear Subfiles
     d ClrSubFiles     pr

     d* load addon code grid
     d loadAddOnCd     pr

     d* main Processing
     d main            pr

     d* program Start
     d programStart    pr
     d processMe                       n

     d* process addition/edit
     d processAddEdit  pr

     d* process other options
     d procOther       pr

     d* Validate Add/Edit screen
     d validate        pr

     d* Add/Edit details
     d addEditDetails  pr

     d* Move database record to Add screen
     d MoveDbScAdd     pr

     c* Entry of Program.
     c     *Entry        Plist
     c                   Parm                    @@rrn             9            fmaop
     c                   Parm                    @@mode                         mode
     c                   Parm                    @@TabInd          5            Tab index

      /free

       // main program processing
       If %parms >= 1;
          xxRrn = %int(@@rrn);
       //***** Beg Dlt *** 09/02/19 ***************************************
       //*** Else;
       //***    xxRrn = 1;
       //***** End Dlt *** 09/02/19 ***************************************
       Endif;

       // Program Mode.
       If %parms >= 2;
          mode = @@mode;
       Endif;

       // Tab Index Mode.
       TabIndex = 0;

       Select;
       When mode = 'D';
          dspMode = *On;
          dspVisb = *Off;
       When mode = 'A';
          addMode = *On;
          dspVisb = *On;
       When mode = 'C';
          chgMode= *On;
          dspVisb = *On;
       Endsl;

       ProgramStart(process);

       If process;
          Main();
       Endif;

       Unlock FMAOP;
       *inlr = *on;
       Return;

      /end-free
       //===================================================================
       // AddEditDetails - Add/Edit Details
       //===================================================================
     P addEditDetails  B
     d addEditDetails  PI
     c
      /free

       Chain scAORrn# pao0p;
       //***** Beg Add *** 01/30/23 ***************************************
       AOedittype = scAOTypeE;
       //***** End Add *** 01/30/23 ***************************************
       AOaddon = scAddAOCd;
       AOprim = scAddPriCd;
       Monitor;
        AOefdt = %dec(%char(scAddEffDt:*cymd0):7:0);
       On-error;
        AOefdt = *zeros;
       EndMon;
       Monitor;
        AOdldt = %dec(%char(scAddDelDt:*cymd0):7:0);
       On-error;
        AOdldt = *zeros;
       EndMon;
       AOstat = scAOStat;

       If ##mode = 'A';
          Monitor;
           AOefdt = %dec(%char(scAddEffDt:*cymd0):7:0);
          On-error;
           AOefdt = *zeros;
          EndMon;
          Monitor;
           AOdldt = %dec(%char(scAddDelDt:*cymd0):7:0);
          On-error;
           AOdldt = *zeros;
          EndMon;
          AOstat = 'A';
          AOiusr = q1user;
          AOipgm = q1pgm;
          AOidt  = %dec(%char(%date():*cymd0):7:0);
          AOitim = %dec(%char(%time():*iso0):6:0);
          AOuser = q1user;
          AOpgm  = q1pgm;
          AOdt   = %dec(%char(%date():*cymd0):7:0);
          AOtime = %dec(%char(%time():*iso0):6:0);
          Write pao0p;
       Else;
          AOuser = q1user;
          AOpgm  = q1pgm;
          AOdt   = %dec(%char(%date():*cymd0):7:0);
          AOtime = %dec(%char(%time():*iso0):6:0);
          Update pao0p;
       Endif;
       Unlock FMAOP;

      /end-free
     P addEditDetails  E

       //==================================================================
       // CheckFilter - Determine if the records meet the Filter criteria
       //==================================================================

     p checkFilter     b
     d checkFilter     pi
      /free

       // Check for AddOn code
       //***** Beg Dlt *** 09/02/19 ****************************************
       //*** If scFltrAOCd <> *blanks and scFltrAOCd <> AOaddon;
       //***   meetFltr = *off;
       //*** Endif;
       //***** End Dlt *** 09/02/19 ****************************************

       //***** Beg Add *** 09/02/19 ****************************************
       // Check for AddOn From code
       If scFltrFrCd <> *blanks and AOaddon < scFltrFrCd;
          meetFltr = *off;
       Endif;

       // Check for AddOn To code
       If scFltrToCd <> *blanks and AOaddon > scFltrToCd;
          meetFltr = *off;
       Endif;
       //***** End Add *** 09/02/19 ****************************************

       // Check for status
       If meetFltr = *on;
          If scFltrSts <> ' ' and scFltrSts <> AOstat;
             meetFltr = *off;
          Endif;
       Endif;

      /end-free
     p checkFilter     e

       //===================================================================
       // ClrSubFiles - Clears SubFiles
       //===================================================================
     p clrSubFiles     B
     D clrSubFiles     PI
      /Free

       rrnAddOnCd = 0;
       clrAOsub = *On;
       dspAOsub = *Off;
       dspAOCtl = *Off;
       Write rcdAddOnCd;

      /End-Free
     p clrSubfiles     E

       //===================================================================
       // LoadClrFlt - Clears the Filter
       //===================================================================

     p loadClrFlt      b
     d loadClrFlt      pi

      /free

       // if first time screen load, set only Active records and display Filter button
       If isFirstLoad = *on;
          isFirstLoad = *off;
          dspFilter.scFltrSts = 'A';
          scFltrSts = 'A';
       EndIf;

       // display clear filter button
       If dspFilter.scFltrSts = *blanks and
       //***** Beg Chg *** 09/02/19 ****************************************
          dspFilter.scFltrFrCd = *blanks;
          dspFilter.scFltrToCd = *blanks;

        //*** dspFilter.scFltrAOCd = *blanks;
       //***** End Chg *** 09/02/19 ****************************************
          dspClrFltr = *off;
       Else;
          dspClrFltr = *on;
       Endif;

      /end-free
     p LoadClrFlt      e

       //===================================================================
       // MoveDbSc - Move database record to Add screen
       //===================================================================
     p moveDbScAdd     B
     d moveDbScAdd     PI
     c
      /free

       //***** Beg Add *** 01/30/23 ***************************************
       scAOTypeE = AOedittype;
       //***** End Add *** 01/30/23 ***************************************
       scAddAOCd = AOaddon;
       scAddPriCd = AOprim;
       Monitor;
        scAddEffDt = %date(AOefdt:*cymd);
       On-error;
        scAddEffDt = *loval;
       EndMon;
       Monitor;
        scAddDelDt = %date(AOdldt:*cymd);
       On-error;
        scAddDelDt = *loval;
       EndMon;

       Monitor;
        sv_scAddEffDt = %dec(%char(scAddEffDt:*cymd0):7:0);
       On-error;
        sv_scAddEffDt = *loval;
       EndMon;
       Monitor;
        sv_scAddDelDt = %dec(%char(scAddDelDt:*cymd0):7:0);
       On-error;
        sv_scAddDelDt = *loval;
       EndMon;
       //***** Beg Add *** 01/30/23 ***************************************
       sv_scAOTypeE = scAOTypeE;
       //***** End Add *** 01/30/23 ***************************************
       sv_scAddAOCd = scAddAOCd;
       sv_scAddPriCd  = scAddPriCd;

      /end-free
     p moveDbScAdd     E

       //===================================================================
       // LoadAddOnCd : Loads Add-On Code Information
       //===================================================================
     p loadAddOnCd     B
     d loadAddOnCd     PI
      /free

       // set subfile indicators
       clrAOSub = *Off;
       dspAOSub = *On;
       dspAOCtl = *On;

       //***** Beg Add *** 09/02/19 ***************************************
       scAOMsg = *blanks;
       vsbAOMsg = *Off;
       //***** End Add *** 09/02/19 ***************************************

       // read file and load the subfile
       Setll *start fmaop;
       Read pao0p;
       dow not %eof(fmaop);

          // Validate for filter
          meetFltr = *on;
          checkFilter();
          If meetFltr = *on;

             clear sflAddOnCd;

             //***** Beg Add *** 01/30/23 ***************************************
             scAOType = AOedittype;
             //***** End Add *** 01/30/23 ***************************************
             scAOAdOnCd = AOaddon;
             scAOPriCd = AOprim;
             Monitor;
              scAOEffDt = %date(AOefdt:*cymd);
             On-error;
              scAOEffDt = *loval;
             EndMon;
             Monitor;
              scAODelDt = %date(AOdldt:*cymd);
             On-error;
              scAODelDt = *loval;
             EndMon;
             scAOStat  = AOstat;

             scAORRN# = rrnfmaop;

             rrnAddOnCd = rrnAddOnCd + 1;
             If rrnAddOnCd = 1;
                scAORowSl = 'A';
             Else;
                clear scAORowSl;
             Endif;
           //***** Beg Dlt *** 09/02/19 *************************************
           //***  If mode = 'D';
           //***     dspVisb = *Off;
           //***  Else;
           //***     dspVisb = *On;
           //***  Endif;
           //***** End Dlt *** 09/02/19 *************************************

       //***** Beg Dlt *** 09/02/19 ***************************************
       //*** If rrnAddOnCd >= 9999;
       //***** End Dlt *** 09/02/19 ***************************************

       //***** Beg Add *** 09/02/19 ***************************************
             If rrnAddOnCd >= 9999;
               scAOMsg = 'Too many records returned; first 9999 are shown. ' +
                         'Please use filter to select the records';
               vsbAOMsg = *On;
       //***** End Add *** 09/02/19 ***************************************
               rrnAddOnCd = 1;
               Leave;
             EndIf;

             Write sflAddOnCd;

          Endif;
          Read pao0p;
       Enddo;

       rrnAddOnCd = 1;

       //***** Beg Add *** 09/02/19 ***************************************
       If mode = 'D';
          dspVisb = *Off;
       Else;
          dspVisb = *On;
       Endif;

       Unlock FMAOP;
       //***** End Add *** 09/02/19 ***************************************

      /end-free
     p loadAddOnCd     E

      //===================================================================
       // Main - Main processing
      //===================================================================
     p main            B
     d main            PI

      /free

       // Process until exit button is pressed.
       Dou ExitNow = *On;

          // Load Clear Filter Button.
          LoadClrFlt();

          // Clear Sub-files
          ClrSubfiles();

          // Load Add-On Code Information
          LoadAddonCd();

          // Process Add-On Code screen.
          Exsr ExfmtScreens;

          Select;

          // do nothing - catch tab click event
          When tabClick = *on;
             tabClick = *off;

          // Filter Processing.
          When BtnFilter = *on;
             Dou BtnFltrCan = *on or BtnFltrApp = *on;

                ExFmt RcdAOFltr;
                If BtnFltrApp = *on;
                   dspFilter.scFltrSts = scFltrSts;
              //***** Beg Add *** 09/02/19 *******************************
                   dspFilter.scFltrFrCd = scFltrFrCd;
                   dspFilter.scFltrToCd = scFltrToCd;
              //***** End Add *** 09/02/19 *******************************

              //***** Beg Dlt *** 09/02/19 *******************************
              //***  dspFilter.scFltrAOCd = scFltrAOCd;
              //***** End Dlt *** 09/02/19 *******************************
                Endif;

                If BtnFltrCan = *on;
                   scFltrSts = dspFilter.scFltrSts;
              //***** Beg Add *** 09/02/19 *******************************
                   scFltrFrCd = dspFilter.scFltrFrCd;
                   scFltrToCd = dspFilter.scFltrToCd;
              //***** End Add *** 09/02/19 *******************************

              //***** Beg Dlt *** 09/02/19 *******************************
              //***  scFltrAOCd = dspFilter.scFltrAOCd;
              //***** End Dlt *** 09/02/19 *******************************
                Endif;
             Enddo;

          // Filter clear processing
          When BtnClrFltr = *on;
             Clear dspFilter;
             Clear rcdAOFltr;
             dspClrFltr = *off;

          When btnRefresh = *On;
             btnRefresh =*Off;

          // Add button processing
          When scAOAdd = *on;
             clear RcdAddAO;
             ##mode = 'A';
             BtnAddSave = *off;
             BtnAddCan = *off;
             processAddEdit();

          // Other options processing
          Other;
             procOther();

          Endsl;

       Enddo;
       //===================================================================
       // SR ExfmtScreens : Write-Read Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       Select;

       When TabIndex = 0;
          TabHeight = 600;
          APnlHeight = tabHeight + 60;
          Write rcdSysTbl;
          Write rcdTabPan;
          Exfmt rcdAddOnCd;
          Read rcdTabPan;
          Read rcdSysTbl;
       EndSl;

       Endsr;

      /end-free
     p main            E

       //===================================================================
       // ProcessAddEdit - process addition through Add or Edit
       //===================================================================
     p processAddEdit  B
     d processAddEdit  PI
      /free

       isValid  = *on;

       // processing for Edit mode
       If ##mode = 'C';

        //***** Beg Add *** 01/30/23 ***************************************
        // Edit Type should be disabled in edit mode
          dsbAOTypeE = *on;
        //***** End Add *** 01/30/23 ***************************************
        // add-on code should be disabled in edit mode
          dsbAddAOCd = *on;

        // primary code should be disabled in edit mode
          dsbAddPrCd = *on;

        // effective date should be disabled in edit mode
          dsbAddEfDt = *on;

        // deletion date can be changed in edit mode
          dsbAddDlDt = *off;

          Chain(n) scAORrn# pao0p;
          MoveDbScAdd();

       Endif;

       // processing for Add mode
       If ##mode = 'A';

        //***** Beg Add *** 01/30/23 ***************************************
        // Edit Type should not be disabled in add mode
          dsbAOTypeE = *off;
        //***** End Add *** 01/30/23 ***************************************
        // add-on code should not be disabled in add mode
          dsbAddAOCd = *off;

        // primary code should not be disabled in add mode
          dsbAddPrCd = *off;

        // effective date should not be disabled in add mode
          dsbAddEfDt = *off;

        // deletion date should not be disabled in add mode
          dsbAddDlDt = *off;

       Endif;

       Dou BtnAddCan = *on or (BtnAddSave= *on and isValid = *on);
          Exfmt rcdAddAO;
          isValid = *on;
          Select;

          When BtnAddSave= *on;
             Validate();
             If isValid = *on;
                addEditDetails();
             Endif;

          When BtnAddCan = *on;
             BtnAddCan = *off;
             Leave;
       EndSl;
       Enddo;
       unlock FMAOP;
      /end-free
     P processAddEdit  E

       //===================================================================
       // ProcOther - process other options
       //===================================================================
     p procOther       B
     d procOther       PI
      /free

       ReadC sflAddOnCd;

       Select;

       // processing for Edit link
       When scAOEdt = *on;
          scAOEdt = *off;
          clear rcdAddAO;
          ##mode = 'C';
          BtnAddSave = *off;
          BtnAddCan = *off;
          processAddEdit();

       // processing for Change Status link
       When scAOChgSts = *on;
          scAOChgSts = *off;
          Chain scAORrn# pao0p;
          if AOstat = 'A';
             AOstat = 'I';
          else;
             AOstat = 'A';
          endif;
          update pao0p;
          unlock FMAOP;
       Endsl;

      /end-free
     p procOther       E

       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     p programStart    B
     d programStart    PI
     d processMe                       n

      /free

       // If Rrn is passed, verify
       If xxRrn > *zeros;

         // Chain to Add-On Code File
         Chain xxRrn pao0p;

         // Process only if valid record found.
         If not %found(fmaop);
          processme = *Off;
          Return;
         Endif;

       //***** Beg Add *** 09/02/19 ***************************************
       // If Rrn is not passed, screen opened through menu
       Else;

          xxRrn = 1;
          processMe = *on;
       //***** End Add *** 09/02/19 ***************************************

       Endif;

       processMe = *on;

       TabIndex = 0;
       Clear dspFilter;
       // Default to On for first load of screen
       isFirstLoad = *on;

       // get company of the user
       //company = UserCo#(q1user);

       Return;

      /end-free
     p programStart    E

       //===================================================================
       // Validate - Validate Add/Edit screen
       //===================================================================
     p validate        B
     d validate        PI
     c
      /free

       isValid  = *on;
       erAddAOCd = *off;
       erAddPriCd = *off;
       erAddEffDt = *off;
       erAddDelDt = *off;

       If ##mode = 'A';

          // Add-On Code is a required entry
          If scAddAOCd = *blanks;
             msAddAOCd = 'This is required';
             erAddAOCd = *on;
             isValid  = *off;
             return;
          Endif;

          // Add-On Code must be valid
          If scAddAOCd <> *blanks;
             if %check(AlphaNum:scAddAOCd) <> 0;
                msAddAOCd = 'Invalid entry';
                erAddAOCd = *on;
                isValid  = *off;
                return;
             endif;
          Endif;

          // Primary Code is a required entry
          If scAddPriCd = *blanks;
             msAddPriCd = 'This is required';
             erAddPriCd = *on;
             isValid  = *off;
             return;
          Endif;

          // Primary Code must be valid
          If scAddPriCd <> *blanks;
             if %check(AlphaNum:scAddPriCd) <> 0;
                msAddPriCd = 'Invalid entry';
                erAddPriCd = *on;
                isValid  = *off;
                return;
             endif;
          Endif;

          // Effective Date is a required entry
          If %char(scAddEffDt) = *blanks or scAddEffDt = *loval
             or %char(scAddEffDt) = *zeros;
             msAddEffDt = 'This is required';
             erAddEffDt = *on;
             isValid  = *off;
             return;
          Endif;

          // Effective Date must be valid
          // the screen field is a date field that enforces it

          // Deletion Date must be valid
          // the screen field is a date field that enforces it

       EndIf;

       // Verify if the combination entered is not already present
       If isValid  = *on;

          // if any of the unique combination value has changed,
          // verify the new combination does not already exist

       Endif;

      /end-free
     p validate        E

      ***===================================================================

