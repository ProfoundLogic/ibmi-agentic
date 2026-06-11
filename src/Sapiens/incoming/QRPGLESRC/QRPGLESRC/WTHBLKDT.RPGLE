     hcopyright('(c) 2024 Sapiens, Inc. All rights reserved.')
      //*********************************************************************
     h/title WTHBLKDT - Holiday Blackout Dates
     h dftname(WTHBLKDT)
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
      //  Project   : WC2939 - Redesign of Work with Holiday Blackout  *
      //                       Dates                                   *
      //===============================================================*
      //  Revisions:                                                   *
      //                                                               *
      //                                                               *
      //                                                               *
      // ------------------------------------------------------------- *
      //  NOTE: Please add comments to the top of Revisions.           *
      // ------------------------------------------------------------- *
      //===============================================================*

     fWtHBlkDt  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflHBlkDt:scSflRRN)
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)
      * Company file
     fsmcol     if   e           k disk    rename(pco01:pco00)
      * User Parameter File
     fsmupl     if   e           k disk    rename(pup01:pup00)
      * Holiday blackout date file
     fwtcbl     if   e           k disk    rename(pcb01:pcb00)
     f                                     infds(dbfds)
     fwtcbp     uf a e             disk    rename(pcb01:pcb0p)
     f                                     infds(dbfds1)

     d dbfds           ds
     d  dbrrn                397    400b 0
     d dbfds1          ds
     d  dbrrn1               397    400b 0

     d psds          esds                  extname(szq1p)

      // work variables
     d saveDT          s                   like(CBdate)
     d saveTM          s                   like(CBitm)
     d flgRcdUpdated   s              1    inz
     d svsflrrn        s              5  0 inz
     d charmsg         s            200    inz(*blanks)

     d intCo#          s              3P 0
     d chgMod          s               n   inz(*off)
     d dspMode         s               n   inz(*off)
     d process         s               n   inz(*off)
     d isError         s               n   inz

      // work variables for GetWeekendDt
     d outCo#          s              3
     d outDti          s              8
     d outSrd          s              5
     d outMod          s              1
     d outDto          s              8
     d outRtcd         s              1

      * Check Weekend Date
     d GetWeekendDt    pr                  extpgm('WA072R')
     d   outco#                       3
     d   outdti                       8
     d   outsrd                       5
     d   outmod                       1
     d   outdto                       8
     d   outrtcd                      1

      // Load Holiday Blackout Dates
     d LoadHBlkDate    pr

      * Main Processing
     d Main            pr

      * Process Addition of Holiday Blackout Dates
     d ProcessAdd      pr

      * Process Edit option for Holiday Blackout Dates
     d ProcOther       pr

      * Program Start
     d ProgramStart    pr

      * Validate Holiday Blackout Dates
     d ValidateBlkDt   pr              n
     d strmode                        1

     d/copy sPrDate
     d/copy sPrGenPop
     d/copy sPrCompany
     d/copy sPrUsrPrms
     d/copy wPrPolerr

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
       // Load Holiday Blackout Dates
       //===================================================================
     p LoadHBlkDates   b
     d LoadHBlkDates   pi

      /free
       // Clear the subfile
       scSflRRN = 0;
       scSflDsp = *off;
       scSflClr = *on;
       Write rcdHBlkDt;
       scSflClr = *off;
       scSflDsp = *on;
       scRowSel = 1;

       // Populate the subfile
       Setll *loval Wtcbl;
       Read Wtcbl;
       Dow not %eof(Wtcbl);

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
          scHBlkDt = CvtToDate(CBdate);
          scDesc   = CBdesc;

          visActSts = *off;                // New Hyperlink for Activate
          visIActSts = *off;               // New Hyperlink for Inactivate
          If CBstat = 'A';
             scStatus = 'Active';
             visIActSts = *on;
          Endif;
          If CBstat = 'I';
             scStatus = 'Inactive';
             visActSts = *on;
          Endif;

          If dspMode;
             visActSts = *off;
             visIActSts = *off;
          Endif;

          scRRN = dbrrn;

          Write sflHBlkDt;
          Read Wtcbl;
       Enddo;

       scSflRRN = 1;
       If svSflRRN <> 0;
          scSflRRN = svSflRRN;
          scRowSel = svSflRRN;
          svSflRRN = 0;
       Endif;

      /end-free
     p LoadHBlkDates   e

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

          // Load Holiday Blackout Dates
          LoadHBlkDates();

          // Display Screen
          Exsr ExfmtScreens;

          // Perform actions on screen.
          Select;

          // Add Blackout Date
          When BtnBlkAdd  = *On;
             BtnBlkAdd = *Off;
             ProcessAdd();

          // Refresh
          When btnRefresh = *On;
             btnRefresh = *Off;

          // Save
          When btnSave = *On;
             btnSave = *Off;

          Other;
             ProcOther();             // Process other Options Holiday Blackout Dates

          EndSl;

       Enddo;

       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

        Select;

        When tabIndex = 0;
           PnlHeight = 320;
           APnlHeight = PnlHeight + 60;

           Write rcdComPnl;
           Write rcdPanel;

           Exfmt rcdHBlkDt;

           Read rcdComPnl;
           Read rcdPanel;
        EndSl;

       Endsr;

      /End-Free

     p Main            e

       //===================================================================
       // ProcessAdd - Process Addition of Holiday Blackout Dates
       //===================================================================
     P ProcessAdd      B
     D ProcessAdd      PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdBlkDAdd;
       mode = 'A';
       isexit = *off;
       visSvAd = *on;
       scStatusE   = 'A';

       Dow isexit = *off;

          scBlkDtHdg = 'Add Holiday Blackout Date';

          Exfmt rcdBlkDAdd;
          Select;

          // Save Button
          When BtnSvAdBlk = *on;
             BtnSvAdBlk = *off;
             If ValidateBlkDt(mode);
                Exsr WrtBlkDt;
                //Initialize fields for next add screen if save/addnext is pressed
                Clear rcdBlkDAdd;
                visSvAd = *on;
                scStatusE = 'A';
             EndIf;

          // Save Button
          When BtnSaveBlk = *on;
             BtnSaveBlk = *off;
             If ValidateBlkDt(mode);
                Exsr WrtBlkDt;
                isexit = *on;
             EndIf;

          // Cacel Button
          When BtnCanBlk = *on;
               BtnCanBlk = *off;
               isexit = *on;
          EndSl;
       Enddo;

       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        CBco#   = intCo#;                // Company Number
        CBdate  = dtCYMD(scBlkDateE);    // Blackout Date
        CBdesc  = scDescE;               // Description
        CBstat  = scStatusE;             // Status

        CBusr   = q1user;                // User
        CBpgm   = q1pgm;                 // Program
        CBtm    = Timeto6(SystemTime()); // Time
        CBdat   = Dateto7(SystemDate()); // Date Mintained
        CBiusr  = q1user;                // Initial User
        CBipgm  = q1pgm;                 // Initial Program
        CBitm   = Timeto6(SystemTime()); // Initial Time
        CBidat  = Dateto7(SystemDate()); // Initial Date

       EndSr;

       // Writing new Holiday Blackout Date
       BegSr WrtBlkDt;
        Clear pcb0p;
        Exsr MoveScDb;
        Write pcb0p;
       EndSr;

      /end-free
     P ProcessAdd      E

       //===================================================================
       // ProcOther - Process other Options for Holiday Blackout Dates
       //===================================================================
     P ProcOther       B
     D ProcOther       PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdBlkDAdd;
       isexit = *off;
       visSvAd = *off;

       ReadC sflHBlkDt;
       Dow not %eof;

          Select;
          When scBlkDtEdt = *On; // Processing for Edit
             scBlkDtEdt = *Off;
             mode = 'C';
             svsflrrn = scSflrrn;  //save rrn value
             Chain(n) scRRN pcb0p;
             Exsr Movdbsc;
             scBlkDtHdg = 'Edit Holiday Blackout Date';
             dsbBlkDt = *on;
             Dow isexit = *off;
                Exfmt rcdBlkDAdd;

                Select;
                When BtnSaveBlk = *on;
                   BtnSaveBlk = *off;
                   // Check if Wtcbp updated since screen displayed
                   flgRcdUpdated=RcdUpdatedErr('WTCBP':dbrrn1:'CBdat':'CBtm':
                    'CBusr':saveDT:saveTM:'1');
                   if flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scRRN pcb0p;
                      Exsr Movdbsc;
                   EndIf;
                   If flgRcdUpdated = '0'; // No error
                      Chain scRRN pcb0p;
                      Exsr Movscdb;
                      Update pcb0p;
                      isExit = *on;
                   Endif;
                When BtnCanBlk = *on;
                   BtnCanBlk = *off;
                   isExit = *on;
                EndSl;
             Enddo;

          Endsl;

          Unlock Wtcbp;

          ReadC sflHBlkDt;
       Enddo;

       // Subrotine to move database fields into screen fields
       BegSr MovDbsc;
        scDescE     = CBdesc;
        scBlkDateE  = dtMDYY(CBdate);
        scStatusE   = CBstat;
        saveDT = CBdat;
        saveTM = CBtm;
       EndSr;

       // Subrotine to move screen field to database file
       BegSr Movscdb;
        CBdesc = scDescE;
        CBusr   = q1user;                // User
        CBpgm   = q1pgm;                 // Program
        CBtm    = Timeto6(SystemTime()); // Time
        CBdat   = Dateto7(SystemDate()); // Date Mintained
        saveDT = CBdat;
        saveTM = CBtm;
       EndSr;

      /end-free
     P ProcOther       E

       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

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
             If tabIndex < 0;
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

       process = *on;
       Return;

      /End-Free

     p ProgramStart    e

      //===================================================================
      // Validate the Holiday Blackout Date
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateBlkDt   b
     d ValidateBlkDt   pi              n
     d strmode                        1
      /Free

       // Initialize all the messageId and error indicators.
        isError = *off;
        erBlkDateE = *off;
        msBlkDateE = *blanks;

        If strmode = 'A';

           // Valid Holiday date should be entered
           If scBlkDateE = 0;
              erBlkDateE = *on;
              msBlkDateE = 'WCR3309';
              isError = *on;
           Endif;

           // Check date is valid or not
           If erBlkDateE = *off;
              erBlkDateE = ValidDate(scBlkDateE:charMsg);
              If erBlkDateE = *on;
                 msBlkDateE = charMsg;
                 isError    = *on;
              Endif;
           Endif;

           // Holiday Date should be > than today's date
           If erBlkDateE = *off;
              If dtCYMD(scBlkDateE) <= Dateto7(SystemDate());
                 erBlkDateE = *on;
                 msBlkDateE = 'WCR3308';
                 isError = *on;
              Endif;
           Endif;

           // Check if date is Weekend date
           If erBlkDateE = *off;
              outCo#  = %editc(intco#:'X');
              outDti  = %editc(scBlkDateE:'X');
              outSrd  = *blanks;
              outMod  = '1';
              outDto  = *blanks;
              outRtcd = *blank;

              GetWeekendDt(outCo#:outDti:outSrd:outMod:outDto:outRtcd);
              If outRtcd = '0';
                 erBlkDateE = *on;
                 msBlkDateE = 'WCR3310';
                 isError = *on;
              Endif;
           Endif;

           // Check if duplicate records exist
           If erBlkDateE = *off;
              chain (intco#:dtCYMD(scBlkDateE)) pcb00;
              If %found(wtcbl);
                 erBlkDateE = *on;
                 msBlkDateE = 'WC30661';
                 isError = *on;
              Endif;
           Endif;
        Endif;

      // Return *on indicator when No error exists.
        If isError = *on;
           return *off;
        Else;
           return *on;
        Endif;

      /End-Free
     p ValidateBlkDt   e

