     Hcopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H**********************************************************************
     H/TITLE MTPROCCODE - Procedure Codes Maintenance
     H DFTNAME(MTPROCCODE)
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO)
     H/DEFINE   PROFOUNDUI
     h**********************************************************************
      *===============================================================*
      *  Created   : 07/09/19                                         *
      *  Programmer: C:079                                            *
      *  Project   : MC433                                            *
      *===============================================================*
      *  Revisions:                                                   *
      *                                                               *
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: Please add comments to the top of Revisions.           *
      * ------------------------------------------------------------- *
      *===============================================================*

     fMTPROCCODEcf   e             workstn extfile('MTPROCCODE')
     f                                     HANDLER('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflProcCd:rrnProcCd)
     fstactpnl  cf   e             workstn handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdSysTbl)
     ffmprp     uf a e             disk    rename(ppr01:ppr0p)
     f                                     infds(dbfds1)
     ffmprl     uf a e             disk    rename(ppr01:ppr0l)
     ffmprl1    uf a e             disk    rename(ppr01:ppr0l1)
     ffdcfl     if a e           k disk

     d* procedure definitions
     d*

     d*
     d RunCommand      pr                  extPgm('QCMDEXC')
     d inCmd                       2750    const
     d inCmdLen                      15  5 const

     d* data structures
     d*
     d psds          esds                  extname(szq1p)

     d dspFilter       ds                  likerec(RcdFltr)

     d dbfds1          ds
     d  rrnfmprp             397    400b 0

     d                 ds
     d  @@cmd                  1     56
     d  @@st                  38     39
     d*

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
     d srchMode        s               n   inz(*off)
     d dspMode         s               n   inz(*off)
     d isValid         s               n   inz(*off)
     d process         s               n   inz(*off)
     d xxRrn           s              9  0 inz
     d sv_scProcCd     s                   like(PRcpt4) inz
     d sv_scEfDt       s                   like(PRefdt) inz
     d isFirstLoad     s               n   inz(*off)
     d meetFltr        s               n   inz(*off)
     Dstr9999Msg       s            256
     Dxx9999rsp        s               n
     D isMaxRcds       s               n
     D xxEfDt7         s                   Like(PRefDt)

     d* /Copy source members
     d/copy sprCompany
     d/copy sprUsrPrms
     d/copy sprGenPop
     d/copy sprDate

     d* Clear Subfiles
     d ClrSubFiles     pr

     d* load flexible heading
     d loadFlexHdg     pr

     d* load proc code grid
     d loadProcCd      pr

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
     c                   Parm                    @@State           2            Tab index

      /free

       // main program processing
       If %parms >= 1;
          xxRrn = %int(@@rrn);
       Else;
          xxRrn = 1;
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
       When mode = '1';
          srchMode= *On;
          dspVisb = *Off;
       Endsl;

       ProgramStart(process);

       If process;
          Main();
       Endif;

       Unlock FMPRP;
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

       Chain scRrn# ppr0p;
       PRcpt4 = scProcCd;
       PRdsc1 = scPrCdDesc;
       Monitor;
        PRefdt = %dec(%char(scEfDt:*cymd0):7:0);
       On-error;
        PRefDt = *zeros;
       EndMon;
       PRstat = scStat;

       If ##mode = 'A';
          Monitor;
           PRefdt = %dec(%char(scEfDt:*cymd0):7:0);
          On-error;
           PRefdt = *zeros;
          EndMon;
          PRstat = 'A';
          PRiusr = q1user;
          PRidt  = %dec(%char(%date():*cymd0):7:0);
          PRuser = q1user;
          PRpgm  = q1pgm;
          PRdt   = %dec(%char(%date():*cymd0):7:0);
          PRtime = %dec(%char(%time():*iso0):6:0);
          Write ppr0p;
       Else;
          PRuser = q1user;
          PRpgm  = q1pgm;
          PRdt   = %dec(%char(%date():*cymd0):7:0);
          PRtime = %dec(%char(%time():*iso0):6:0);
          Update ppr0p;
       Endif;
       Unlock FMPRP;

      /end-free
     P addEditDetails  E

       //==================================================================
       // CheckFilter - Determine if the records meet the Filter criteria
       //==================================================================

     p checkFilter     b
     d checkFilter     pi
      /free

       // Check for Procedure code
       If scFltrFrCd <> *blanks or  scFltrToCd <> *blanks;
          If PRcpt4 < scFltrFrCd or PRcpt4 > scFltrToCd;
             meetFltr = *off;
          Endif;
       Endif;

       // Check for status
       If meetFltr = *on;
          If scFltrSts <> ' ' and scFltrSts <> PRstat;
             meetFltr = *off;
          Endif;
       Endif;

       // Check for Effective Date
       If meetFltr = *on;
          If scFltrEfDt <> *zeros;
             xxEfDt7 = DtCyMd(scFltrEfDt);
             If PRefDt <> xxEfDt7;
                meetFltr = *off;
             Endif;
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

       rrnProcCd = 0;
       clrsub   = *On;
       dspSub   = *Off;
       dspctl   = *Off;
       Write rcdProcCd;

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
          scfltrEfdt = 0;
          isFirstLoad = *off;
          If Mode = '2';
             dspFilter.scFltrSts = 'A';
             scFltrSts = 'A';
          Else;
             dspFilter.scFltrSts = ' ';
             scFltrSts = ' ';
          Endif;
       EndIf;

       // display clear filter button
       If dspFilter.scFltrSts = *blanks and
        dspFilter.scFltrFrCd = *blanks and
        dspFilter.scFltrToCd = *blanks and
        dspFilter.scFltrEfDt = *zeros;
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

       scAddPrCd = PRcpt4;
       Monitor;
        scAddEffDt = %date(PRefDt:*cymd);
       On-error;
        scAddEffDt = *loval;
       EndMon;

       Monitor;
        sv_scEfDt = %dec(%char(scAddEffDt:*cymd0):7:0);
       On-error;
        sv_scEfDt = *zeros;
       EndMon;
       sv_scProcCd = scAddPrCd;

      /end-free
     p moveDbScAdd     E

       //===================================================================
       // LoadFlexHdg : Loads Flexible screen headings
       //===================================================================
     p loadFlexHdg     B
     d                 PI
      /free

       // Get flexible headings based on state
       Chain (@@State) pcf01;
       If %Found(fdcfl);
          scFlxMRA = CFhMra;
          scFlxTech = CF92H1;
       Endif;

      /end-free
     p loadFlexHdg     E
       //===================================================================
       // LoadProcCd : Loads Procedure Code Information
       //===================================================================
     p loadProcCd      B
     d                 PI
      /free

       // set subfile indicators
       clrsub   = *Off;
       dspSub   = *On;
       dspctl   = *On;
       isMaxRcds = *Off;

       // Load state
       scState = @@State;

       // read file and load the subfile
       Setll *start fmprp;
       Read ppr0p;
       dow not %eof(fmprp);

          // Validate for filter
          meetFltr = *on;
          checkFilter();
          If meetFltr = *on;

             clear sflProcCd;

             scProcCd   = PRcpt4;
             scPrCdDesc= PRdsc1;
             Monitor;
              scEfDt = %date(PRefdt:*cymd);
             On-error;
              scEfDt = *loval;
             EndMon;
             scStat  = PRstat;
             scMRA   = PRmra;
             scProf  = PRprof;

             scRRN# = rrnfmprp;

             rrnProcCd = rrnProcCd + 1;
             If rrnProcCd = 1;
                scRowSl   = 'A';
             Else;
                clear scRowSl  ;
             Endif;
             If mode = 'D';
                dspVisb = *Off;
             Else;
                dspVisb = *On;
             Endif;

             If rrnProcCd >= 9999;
               isMaxRcds = *on;
               Leave;
             EndIf;

             Write sflProcCd;

          Endif;
          Read ppr0p;
       Enddo;

       rrnProcCd = 1;

      /end-free
     p loadProcCd      E

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

          // Load Flexible headings
          LoadFlexHdg();

          // Load Procedure Code Information
          LoadProcCd();

          // Process Add-On Code screen.
          Exsr ExfmtScreens;

          Select;

          // do nothing - catch tab click event
          When tabClick = *on;
             tabClick = *off;

          // Filter Processing.
          When BtnFilter = *on;
             Dou BtnFltrCan = *on or BtnFltrApp = *on;
                isFltrWdw = *On;
                ExFmt RcdFltr;
                If BtnFltrApp = *on;
                   dspFilter.scFltrSts = scFltrSts;
                   dspFilter.scFltrFrCd = scFltrFrCd;
                   dspFilter.scFltrToCd = scFltrToCd;
                   dspFilter.scFltrEfDt = scFltrEfDt;
                Endif;

                If BtnFltrCan = *on;
                   scFltrSts = dspFilter.scFltrSts;
                   scFltrFrCd = dspFilter.scFltrFrCd;
                   scFltrToCd = dspFilter.scFltrToCd;
                   scFltrEfDt = dspFilter.scFltrEfDt;
                Endif;
             Enddo;

          // Filter clear processing
          When BtnClrFltr = *on;
             Clear dspFilter;
             Clear rcdFltr;
             dspClrFltr = *off;

          When btnRefresh = *On;
             btnRefresh =*Off;

          // Add button processing
          When scAdd = *on;
             clear RcdAdd;
             ##mode = 'A';
             BtnAddSave = *off;
             BtnAddCan = *off;
             processAddEdit();

          // Cancel button processing
          When scCancel = *on;
             scCancel = *Off;
             Unlock FMPRP;
             *inlr = *on;
             Return;

          // Other options processing
          Other;
             procOther();

          Endsl;

       Enddo;
       //===================================================================
       // SR ExfmtScreens : Write-Read Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;
          If isMaxRcds = *on;
             str9999msg = 'Your search returned too many records. ' +
             'Click on Results to see first 9999 records Or, click ' +
             'Filter to narrow down your search results';
             BtnFilter = PopTwoBtn(str9999msg:'Search':'Filter':
                     'Results');
             Dow BtnFilter = *on;
                BtnFilter = *off;
                isMaxRcds = *off;


                Dou BtnFltrCan = *on or BtnFltrApp = *on;

                   TabHeight = 600;
                   APnlHeight = tabHeight + 60;
                   Write rcdSysTbl;
                   Write rcdTabPan;
                   isFltrWdw = *Off;
                   ExFmt RcdFltr;
                   Read rcdTabPan;
                   Read rcdSysTbl;


                   If BtnFltrApp = *on;
                      dspFilter.scFltrSts = scFltrSts;
                      dspFilter.scFltrFrCd = scFltrFrCd;
                      dspFilter.scFltrToCd = scFltrToCd;
                      dspFilter.scFltrEfDt = scFltrEfDt;
                   Endif;

                   If BtnFltrCan = *on;
                      scFltrSts = dspFilter.scFltrSts;
                      scFltrFrCd = dspFilter.scFltrFrCd;
                      scFltrToCd = dspFilter.scFltrToCd;
                      scFltrEfDt = dspFilter.scFltrEfDt;
                   Endif;
                Enddo;
                ClrSubFiles();
                LoadProcCd();
                If isMaxRcds = *on;
                   BtnFilter = PopTwoBtn(str9999msg:'Search':'Filter':
                        'Results');
                Endif;
             Enddo;
          Endif;

       Select;

       When TabIndex = 0;
          TabHeight = 600;
          APnlHeight = tabHeight + 60;
          Write rcdSysTbl;
          Write rcdTabPan;
          Exfmt rcdProcCd;
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

        // Procedure code should be disabled in edit mode
          dsbAddCd   = *on;

        // effective date should be disabled in edit mode
          dsbAddEfDt = *on;

          Chain(n) scRrn# ppr0p;
          MoveDbScAdd();

       Endif;

       // processing for Add mode
       If ##mode = 'A';

        // Procedure code should not be disabled in add mode
          dsbAddCd = *off;

        // effective date should not be disabled in add mode
          dsbAddEfDt = *off;

       Endif;

       Dou BtnAddCan = *on or (BtnAddSave= *on and isValid = *on);
          Exfmt rcdAdd;
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
       unlock FMPRP;
      /end-free
     P processAddEdit  E

       //===================================================================
       // ProcOther - process other options
       //===================================================================
     p procOther       B
     d procOther       PI
      /free

       ReadC sflProcCd;

       Select;

       // processing for Edit link
       When scEdit  = *on;
          scEdit  = *off;
          clear rcdAdd;
          ##mode = 'C';
          BtnAddSave = *off;
          BtnAddCan = *off;
          processAddEdit();

       // processing for Change Status link
       When scChgSts = *on;
          scChgSts = *off;
          Chain scRrn# ppr0p;
          if PRstat = 'A';
             PRstat = 'I';
          else;
             PRstat = 'A';
          endif;
          update ppr0p;
          unlock FMPRP;
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

       // Load Action panel with the Company Name.
       scCompName = GetCompNam(GetUserInfo(q1user));

       // If Rrn is passed, verify
       If xxRrn > *zeros;

         // Chain to Procedure Code File
         Chain xxRrn ppr0p;

         // Process only if valid record found.
         If not %found(fmprp);
          processme = *Off;
          Return;
         Endif;

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
       erAddPrCd = *off;
       erAddEffDt = *off;

       If ##mode = 'A';

          // Add-On Code is a required entry
          If scAddPrCd = *blanks;
             msAddPrCd = 'This is required';
             erAddPrCd = *on;
             isValid  = *off;
             return;
          Endif;

          // Add-On Code must be valid
          If scAddPrCd <> *blanks;
             if %check(AlphaNum:scAddPrCd) <> 0;
                msAddPrCd = 'Invalid entry';
                erAddPrCd = *on;
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

