     hcopyright('(c) 2024 Sapiens, Inc. All rights reserved.')

     h/title WTCNTCFG - County Codes Configuration
     h dftname(WTCNTCFG)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h ALWNULL(*UsrCtl)
     h/define   profoundUI
     h bnddir('STBNDDIR')
     h bnddir('WTGRPCFG')
      //*********************************************************************
      //==============================================================*
      //  Created   : 09/23/24                                         *
      //  Programmer: C:300                                            *
      //  Project   : WC2477 - Redesign of County Codes Configuration  *
      //===============================================================*
      //  Revisions:                                                   *
      //                                                               *
      //                                                               *
      // ------------------------------------------------------------- *
      //  NOTE: Please add comments to the top of Revisions.           *
      // ------------------------------------------------------------- *
      //===============================================================*

     fWTCNTCFG  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflCntCd:rrnCntCd)
     f                                     Sfile(sflZipCd:rrnZipCd)
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)
      * Company file
     fsmcol     if   e           k disk    rename(pco01:pco00)
      * County Codes File
     fwtcnp     uf a e             disk    rename(pcn01:pcn0p)
     f                                     infds(dbfds2)
     fwtcnl1    if   e           k disk    rename(pcn01:pcn01)
     f                                     infds(dbfds)
      * Zip Codes File
     fwmczp     uf a e             disk    rename(pcz01:pcz0p)
     f                                     infds(dbfds4)
     fwmczl1    if   e           k disk    rename(pcz01:pcz01)
     f                                     infds(dbfds3)
      * Application System Control file
     fssylf     if   e           k disk
     f                                     ignore(SSY01)
     f                                     ignore(SPC01)
     f                                     ignore(SRC01)
     f                                     ignore(SBC01)
     f                                     ignore(SCS01)
      * User Parameter File
     fsmupl     if   e           k disk    rename(pup01:pup00)

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
     d saveDT          s                   like(CNdt)
     d saveTM          s                   like(CNtime)
     d saveDT2         s                   like(CNdt)
     d saveTM2         s                   like(CNtime)
     d flgRcdUpdated   s              1    inz
     d intCo#          s              3  0
     D indexTab        s              5  0
     d svrrnCntCd      s              9  0 inz
     d svrrnZipCd      s              9  0 inz
     D xxRrn           s              9  0 inz
     d chgMod          s               n   inz(*off)
     d dspMode         s               n   inz(*off)
     d error           s               n   inz(*off)
     d process         s               n   inz(*off)
     d charMsg         s            100
     d xxDelResp       s               n   inz
     d reload          s               n   inz(*off)
     d keyStCnt        s             10    inz

      // save details of 1st record
     d co#_sav         s                   like(CNco#)
     d state_sav       s              2    inz
     d cnty_sav        s              2    inz

      // Proto-types

      // Load County Codes Grid
     dLoadCntCdGrid    pr

      // Load Zip Codes Grid
     dLoadZipCdGrid    pr

      * Main Processing
     d Main            pr

      * Process Add County Codes
     d ProcAddCntCd    pr

      * Process Add Zip Codes
     d ProcAddZipCd    pr

      * Process other Options for County Codes Grid
     d ProcOther       pr

      * Process other Options for Zip Codes Grid
     d ProcOther2      pr

      * Process the Row Click in Producers Grid
     d ProcRoWClk      pr

      * Program Start
     d ProgramStart    pr
     d processMe                       n

     * Validate County Codes procedure.
     d ValidateCntCd   pr              n
     d strmode                        1

     * Validate Zip Codes procedure.
     d ValidateZipCd   pr              n
     d strmode                        1


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


      //
      //   Key List for the Zip Codes Table File
      //
     C     keyWMCZL      klist
     C                   kfld                    co#_sav
     C                   kfld                    state_sav
     C                   kfld                    cnty_sav

      // Entry of Program.

     c     *entry        plist
     c                   parm                    @@rrn             9
     c                   parm                    @@mode            1
     c                   parm                    @@next           10
     c                   parm                    @@TabInd          5

      /free
       ProgramStart(process);

       If process;
          Main();
       Endif;

       *InLr = *On;
      /end-free


       //===================================================================
       // Load County Codes Grid
       //===================================================================
     p LoadCntCdGrid   b
     d LoadCntCdGrid   pi

      /free
        // Clear the subfile
        rrnCntCd = 0;
        dspCntCd = *off;
        dspZipCd = *off;
        clrCntCd = *on;
        Write rcdCntCd;
        clrCntCd = *off;
        dspCntCd = *on;
        dspZipCd = *on;
        scRowSel = 1;

        If dspMode;
           inquiry = *on;
        Endif;

        // Populate the subfile
        Setll intCo# wtcnl1;
        Reade intCo# wtcnl1;
        Dow not %eof(wtcnl1);
           rrnCntCd += 1;
           scCntCdSel = *off;

           // set the selection on 1st record for first load
           If svrrnCntCd = *zeros;
              If rrnCntCd = 1;
                 scCntCdSel = *on;
              Endif;
           Else;
              If svrrnCntCd = rrnCntCd;
                 scCntCdSel = *on;
              Endif;
           Endif;

           scKst = %subst(CNkey:1:2);
           scCntcd = %subst(CNkey:3:2);
           scDesc = CNdesc;
           scCntSts = *Blanks;

           visActSts = *off;                // New Hyperlink for Activate
           visIActSts = *off;               // New Hyperlink for Inactivate
           If CNstat = 'A';
              scCntSts = 'Active';
              visIActSts = *on;
           Endif;
           If CNstat = 'I';
              scCntSts = 'Inactive';
              visActSts = *on;
           Endif;

           If dspMode;
              visActSts = *off;
              visIActSts = *off;
           Endif;

           scRrnCnt = dbrrn;

           Write sflCntCd;

           // Save 1st record information to load in Zip Codes Grid
           If co#_sav = 0;
              co#_sav  = CNco#;
              state_sav = %subst(CNkey:1:2);
              cnty_sav  = %subst(CNkey:3:2);
           Endif;
        Reade intCo# wtcnl1;
        Enddo;

        rrnCntCd = 1;
        If svrrnCntCd <> 0;
           rrnCntCd = svrrnCntCd;
           scRowSel = svrrnCntCd;
           svrrnCntCd = 0;
        Endif;

      /end-free
     p LoadCntCdGrid   e

       //===================================================================
       // Load Zip Codes Grid
       //===================================================================
     p LoadZipCdGrid   b
     d LoadZipCdGrid   pi

      /free
        // Clear the subfile
        rrnZipCd = 0;
        dspCntCd = *off;
        dspZipCd = *off;
        clrZipCd = *on;
        Write rcdCntCd;
        clrZipCd = *off;
        dspCntCd = *on;
        dspZipCd = *on;
        scRowSel2 = 1;

        If dspMode;
           inquiry = *on;
        Endif;

        // Populate the subfile
        Setll keyWMCZL wmczl1;
        Reade keyWMCZL wmczl1;
        Dow not %eof(wmczl1);
           rrnZipCd += 1;
           scZipCdSel = *off;

           // set the selection on 1st record for first load
           If svrrnZipCd = *zeros;
              If rrnZipCd = 1;
                 scZipCdSel = *on;
              Endif;
           Else;
              If svrrnZipCd = rrnZipCd;
                 scZipCdSel = *on;
              Endif;
           Endif;

           scZst = CZst;
           scZCntcd = CZcnty;
           scZipCd = CZzip;
           scZipSts = *Blanks;

           visActSts2 = *off;                // New Hyperlink for Activate
           visIActSt2 = *off;                // New Hyperlink for Inactivate
           If CZstat = 'A';
              scZipSts = 'Active';
              visIActSt2 = *on;
           Endif;
           If CZstat = 'I';
              scZipSts = 'Inactive';
              visActSts2 = *on;
           Endif;

           If dspMode;
              visActSts2 = *off;
              visIActSt2 = *off;
           Endif;

           scRrnZip = dbrrn3;

           Write sflZipCd;

        Reade keyWMCZL wmczl1;
        Enddo;

        rrnZipCd = 1;
        If svrrnZipCd <> 0;
           rrnZipCd = svrrnZipCd;
           scRowSel2 = svrrnZipCd;
           svrrnZipCd = 0;
        Endif;

      /end-free
     p LoadZipCdGrid   e

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi
     d*
     d isexit          s               n   Inz(*off)
      /free

       LoadActionBar();
       BtnCancel = *off;
       reload = *on;

       // Process until exit button is pressed.
       DoW BtnCancel = *off;

          scCo# = intCo#;
          // Load County Codes Grid
          If reload = *on;
             LoadCntCdGrid();
             reload = *off;
          endif;

          // Load Zip Codes Grid
          LoadZipCdGrid();

          // display screen
          Exsr ExfmtScreens;

          // Perform actions on screen.
          Select;
          // do nothing - catch tab click event
          When TabClick = *on;
             TabClick = *off;

          // Refresh
          When btnRefresh = *On;
             btnRefresh = *Off;
             co#_sav = 0;
             state_sav = *blanks;
             cnty_sav = *blanks;
             svrrnCntCd = 0;
             reload = *on;

          // Catch Row click event
          When RowClick = *on;
             RowClick = *off;
             ProcRoWClk();           // Process the Row Click in County Codes Grid
             reload = *off;

          // Add County Codes Button
          When BtnCntAdd = *On;
             BtnCntAdd = *Off;
             ProcAddCntCd();

          // Add Zip Codes Button
          When BtnZipAdd = *On;
             BtnZipAdd = *Off;
             ProcAddZipCd();


          Other;
             procOther();                   // County Codes Edit
             procOther2();                  // Zip Codes Edit

          Endsl;
       Enddo;

       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       indexTab = TabIndex + 1;

       Select;
       When indexTab = 1;
          pnlHeight = 500;
          APnlHeight = pnlHeight + 60;

          If dspMode;
             inquiry = *on;
          Endif;

          Write rcdComPnl;
          Write rcdPanel;
          Exfmt rcdCntCd;
          Read  rcdComPnl;
          Read rcdPanel;

       EndSl;

       Endsr;


      /end-free
     p Main            e

       //===================================================================
       // ProcAddCntCd - process addition of County Codes
       //===================================================================
     P ProcAddCntCd    B
     D ProcAddCntCd    PI
     d*
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)

      /free

       Clear rcdAddCnt;
       mode = 'A';
       isexit = *off;
       BtnCntCan = *off;
       CntCdEdt = *off;
       scCntCdStE = 'A';

       Dow BtnCntCan = *off and isexit = *off;

          scCo# = intCo#;

          scCntCdHdg = 'Add County Codes Information';
          visCntSvAd = *On;                           // Show Save/Add next Button

          Exfmt rcdAddCnt;

          Select;
          // Save/Add Next Button
          When BtnCntSvAd = *on;
             BtnCntSvAd = *off;
             If ValidateCntCd(mode);
                Exsr WrtCntCd;
               // Initialize Screen fields for re-load
                Clear rcdAddCnt;
                BtnCntCan = *off;
                CntCdEdt = *off;
                scCntCdStE = 'A';
                reload = *on;
                co#_sav = 0;              // Reload the County Codes grid from top
                state_sav = *blanks;
                cnty_sav = *blanks;
                svrrnCntCd = 0;
             EndIf;

          // Save Button
          When BtnCntSav = *on;
             BtnCntSav = *off;
             If ValidateCntCd(mode);
                Exsr WrtCntCd;
                isexit = *on;
                reload = *on;
                co#_sav = 0;              // Reload the County Codes grid from top
                state_sav = *blanks;
                cnty_sav = *blanks;
                svrrnCntCd = 0;
             EndIf;

          // Cacel Button
          When BtnCntCan = *on;
             BtnCntCan = *off;
             isexit = *on;

          EndSl;
       Enddo;

       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        CNco#  = scCo#;                 // Company Number
        CNkey  = *blanks;              // State + County Code
        CNkey  = scKstE + scCntCdE;    // State + County Code
        CNdesc = scDescE;               // Description
        CNstat = scCntCdStE;           // Status

        CNuser = q1user;
        CNpgm  = q1pgm;
        CNtime = Timeto6(SystemTime());
        CNdt   = Dateto7(SystemDate());
        CNiUsr = q1user;
        CNiPgm = q1pgm;
        CNitm  = Timeto6(SystemTime());
        CNiDt  = Dateto7(SystemDate());
       EndSr;

       // Writing County Codes subroutine
       BegSr WrtCntCd;
        Clear pcn0p;
        Exsr MoveScDb;
        Write pcn0p;
       EndSr;

      /end-free
     P ProcAddCntCd    E

       //===================================================================
       // ProcAddZipCd - process addition of Zip Codes
       //===================================================================
     P ProcAddZipCd    B
     D ProcAddZipCd    PI
     d*
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)

      /free

       Clear rcdAddZip;
       mode = 'A';
       isexit = *off;
       BtnZipCan = *off;
       scZipCdStE = 'A';

       Dow BtnZipCan = *off and isexit = *off;

          scCo# = intCo#;
          scZstE = state_sav;
          scZCntyE = cnty_sav;

          scZipCdHdg = 'Add Zip Codes Information';
          visZipSvAd = *On;                           // Show Save/Add next Button

          Exfmt rcdAddZip;

          Select;
          // Save/Add Next Button
          When BtnZipSvAd = *on;
             BtnZipSvAd = *off;
             If ValidateZipCd(mode);
                Exsr WrtZipCd;
               // Initialize Screen fields for re-load
                Clear rcdAddZip;
                BtnZipCan = *off;
                scZipCdStE = 'A';
                scZstE = state_sav;
                scZCntyE = cnty_sav;
             EndIf;

          // Save Button
          When BtnZipSav = *on;
             BtnZipSav = *off;
             If ValidateZipCd(mode);
                Exsr WrtZipCd;
                isexit = *on;
             EndIf;

          // Cacel Button
          When BtnZipCan = *on;
             BtnZipCan = *off;
             isexit = *on;

          EndSl;
       Enddo;

       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        CZco#  = scCo#;                 // Company Number
        CZst   = scZstE;               // State
        CZcnty = scZCntyE;             // County Code
        CZzip  = scZipE;               // Description
        CZstat = scZipCdStE;           // Status

        CZuser = q1user;
        CZpgm  = q1pgm;
        CZtime = Timeto6(SystemTime());
        CZdt   = Dateto7(SystemDate());
       EndSr;

       // Writing Zip Codes subroutine
       BegSr WrtZipCd;
        Clear pcz0p;
        Exsr MoveScDb;
        Write pcz0p;
       EndSr;

      /end-free
     P ProcAddZipCd    E

       //===================================================================
       // ProcOther - Process other Options for County Codes Grid
       //===================================================================
     P ProcOther       B
     D ProcOther       PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdAddCnt;
       isexit = *off;
       ReadC sflCntCd;

       Dow not %eof;

          Select;
          When scCntEdt = *On; // Processing for Edit County Codes Link
             scCntEdt = *Off;
             Update sflCntCd;           // Update Indicator in sfl

             scCo# = intCo#;
             mode = 'C';
             CntCdEdt = *on ;   //disable non-editable field
             svrrnCntCd = rrnCntCd;     //Save Rrn value for edited record

             Chain(n) scRrnCnt WTCNP;

             Exsr MoveDbSc;

             scCntCdHdg = 'Edit County Codes Information';

             Dow isexit = *off;
                Exfmt rcdAddCnt;

                Select;
                // Save Button
                When BtnCntSav = *on;
                   BtnCntSav = *off;
                   // Check if WTCNP updated since screen displayed (County Codes)
                   flgRcdUpdated=RcdUpdatedErr('WTCNP':dbrrn2:'CNdt':'CNtime':
                    'CNuser':saveDT:saveTM:'1');
                   if flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scRrnCnt WTCNP;
                      Exsr MoveDbSc;
                   EndIf;
                   if flgRcdUpdated = '0'; // No error
                      If ValidateCntCd(mode);
                         Chain scRrnCnt WTCNP;
                         Exsr MoveScDb;
                         Update pcn0p;
                         isexit = *on;
                         reload = *on;
                         state_sav = scKstE;
                         cnty_sav = scCntCdE;
                      EndIf;
                   EndIf;

                // Cacel Button
                When BtnCntCan= *on;
                   BtnCntCan = *off;
                   isexit = *on;
                EndSl;
             EndDo;

          Endsl;

          Unlock WTCNP;

          ReadC sflCntCd;
       Enddo;


       // Subrotine to move database fields into screen fields
       BegSr MoveDbSc;
        scKstE = %subst(CNkey:1:2);
        scCntCdE = %subst(CNkey:3:2);
        scDescE = CNdesc;
        scCntCdStE = CNstat;
        saveDT = CNdt;
        saveTM = CNtime;
       EndSr;

       // Subrotine to move Screen fields into database fields
       BegSr MoveScDb;
        CNdesc = scDescE;               // Description
        CNstat = scCntCdStE;           // Status

        CNuser = q1user;
        CNpgm  = q1pgm;
        CNtime = Timeto6(SystemTime());
        CNdt   = Dateto7(SystemDate());
        saveDT = CNdt;
        saveTM = CNtime;
       EndSr;

      /end-free
     P ProcOther       E

       //===================================================================
       // ProcOther2 - Process other Options for Zip Codes Grid
       //===================================================================
     P ProcOther2      B
     D ProcOther2      PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdAddZip;
       isexit = *off;
       ReadC sflZipCd;

       Dow not %eof;

          Select;
          When scZipEdt = *On; // Processing for Edit Zip Codes Link

             scCo# = intCo#;
             scZipEdt = *Off;
             mode = 'C';
             svrrnZipCd = rrnZipCd;     //Save Rrn value for edited record

             Chain(n) scRrnZip WMCZP;

             Exsr MoveDbSc;

             scZipCdHdg = 'Edit Zip Codes Information';

             Dow isexit = *off;
                Exfmt rcdAddZip;

                Select;
                // Save Button
                When BtnZipSav = *on;
                   BtnZipSav = *off;
                   // Check if WMCZP updated since screen displayed (Zip Codes)
                   flgRcdUpdated=RcdUpdatedErr('WMCZP':dbrrn4:'CZdt':'CZtime':
                    'CZuser':saveDT2:saveTM2:'1');
                   if flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scRrnZip WMCZP;
                      Exsr MoveDbSc;
                   EndIf;
                   if flgRcdUpdated = '0'; // No error
                      If ValidateZipCd(mode);
                         Chain scRrnZip WMCZP;
                         Exsr MoveScDb;
                         Update pcz0p;
                         isexit = *on;
                      EndIf;
                   EndIf;

                // Cacel Button
                When BtnZipCan= *on;
                   BtnZipCan = *off;
                   isexit = *on;
                EndSl;
             EndDo;

          When ScZipDlt = *On; // Processing for Delete Zip Codes Link

             scZipDlt = *Off;

             Chain(e) scRrnZip WMCZP;
             If %status = 1218;
                PopOneBtn('Record locked by another user.  Please try again-
                 later.':'Record Lock');
                Return;
             Endif;

             Chain scRrnZip WMCZP;
             Exsr MoveDbSc;

             scZipCdHdg = 'Delete Zip Codes Information';

             Dow isexit = *off;

                Write rcdAddZip;
                xxDelResp = PopTwoBtn('Do you want to delete this Zip +
                Codes Information?':'Confirm Delete':'Yes':'No');
                Select;
                When xxDelResp = *On;
                   Delete pcz0p;
                   PopOneBtn('Zip Codes Information +
                   Deleted!':'Delete':'Ok');
                   isexit = *on;
                When xxDelResp = *Off;
                   svrrnZipCd= rrnZipCd;     //Save Rrn value
                   isexit = *on;
                EndSl;
             EndDo;
          Endsl;

          Unlock WMCZP;

          ReadC sflZipCd;
       Enddo;


       // Subrotine to move database fields into screen fields
       BegSr MoveDbSc;
        scZstE = CZst;
        scZCntyE = CZcnty;
        scZipE = CZzip;
        scZipCdStE = CZstat;
        saveDT2 = CZdt;
        saveTM2 = CZtime;
       EndSr;

       // Subrotine to move Screen fields into database fields
       BegSr MoveScDb;
        CZzip  = scZipE;               // Zip Code
        CZstat = scZipCdStE;           // Status

        CZuser = q1user;
        CZpgm  = q1pgm;
        CZtime = Timeto6(SystemTime());
        CZdt   = Dateto7(SystemDate());
        saveDT2 = CZdt;
        saveTM2 = CZtime;
       EndSr;

      /end-free
     P ProcOther2      E

       //===================================================================
       // ProcRoWClk - Process the Row Click in County Codes Grid
       //===================================================================
     P ProcRoWClk      B
     D ProcRoWClk      PI

      /free

       // process to load Zip Codes grid for selected row.
       Chain rrnCntCd sflCntCd;
       Chain(n) scRrnCnt wtcnp;

       svrrnCntCd = rrnCntCd;
       scrowsel = rrnCntCd;

       co#_sav  = CNco#;
       state_sav = %subst(CNkey:1:2);
       cnty_sav  = %subst(CNkey:3:2);

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

          scActMode = @@mode;
       Endif;

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
       Chain intCo# pco00;
       If %found(smcol) and COname <> ' ';
          scCompName = COname;
       Endif;
       scCo# = intCo#;

       // Retrieve user parameter
       Chain q1user pup00;

       processMe = *on;

       Return;


      /end-free
     p ProgramStart    e

      //===================================================================
      // Validate County Codes addition information.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateCntCd   b
     d ValidateCntCd   pi              n
     d strmode                        1
     d isError         s               n   inz
      /Free

       isError = *off;
      // Initialize all the messageId and error indicators.
       erKstE= *off;
       msKstE = *blanks;
       erCntCdE = *off;
       msCntCdE = *blanks;
       erDescE = *off;
       msDescE = *blanks;

       If strmode <> 'C';
         // State should not be blank
          If scKstE = *Blanks;
             erKstE = *on;
             msKstE = 'WCR0503';
             isError = *on;
          Endif;

         // County Code should not be blank
          If scCntCdE = *Blanks;
             erCntCdE = *on;
             msCntCdE = 'WCR0503';
             isError = *on;
          Endif;

         // Validate check for Duplicate record
          If isError = *off;
             keyStCnt = scKstE + scCntCdE;
             Chain (scCo#:keyStCnt) wtcnl1;
             If %found(wtcnl1);
                erKstE = *on;
                msKstE = 'WC30005';
                erCntCdE = *on;
                msCntCdE = 'WC30005';
                isError = *on;
             Endif;
          Endif;
       Endif;

      // Description should not be blank
       If scDescE = *Blanks;
          erDescE = *on;
          msDescE = 'WCR0503';
          isError = *on;
       Endif;


      // Return *on indicator when No error exists.
       If isError = *on;
          return *off;
       Else;
          return *on;
       Endif;

      /End-Free
     p ValidateCntCd   e

      //===================================================================
      // Validate Zip Codes addition information.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateZipCd   b
     d ValidateZipCd   pi              n
     d strmode                        1
     d isError2        s               n   inz
      /Free

       isError2 = *off;
      // Initialize all the messageId and error indicators.
       erZipE = *off;
       msZipE = *blanks;

      // Zip Code should not be zero
       If scZipE = *Zeros;
          erZipE = *on;
          msZipE = 'WCR0502';
          isError2 = *on;
       Endif;

       If strmode <> 'C';
         // Validate check for Duplicate record
          If isError2 = *off;
             Chain (scCo#:scZstE:scZCntyE:scZipE) wmczl1;
             If %found(wmczl1);
                erZipE = *on;
                msZipE = 'WC30005';
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
     p ValidateZipCd   e

      // utility procedure definitions
      /COPY SCOPYCOMPI

