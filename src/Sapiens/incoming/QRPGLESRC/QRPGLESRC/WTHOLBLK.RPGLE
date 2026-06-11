     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H BNDDIR('STBNDDIR')
     H BNDDIR('WTGRPCFG')
     H/define CONVERT
     H*
     H***********************************************************************
     H/TITLE WTHOLBLK - HOLIDAY BLACKOUT DATES                             *
     H***********************************************************************
     H*                                                                    *
     H* PROGRAM DESCRIPTION:                                               *
     H*   THIS PROGRAM ENABLES USER TO ENTER OR EDIT BLACKOUT DATES AND    *
     H*   SPECIFY A DESCRIPTION. THE USER CAN ALSO ACTIVATE OR DEACTIVATE  *
     H*   THE BLACKOUT DATE RECORD.                                        *
     H*                                                                    *
     H*  CALLED FROM:                                                      *
     H*   NONE                                                             *
     H*  CALLS TO:                                                         *
     H*                                                                    *
     H*   Created   : 01/10/24                                         *
     H*   Programmer: C:342                                            *
     H*   Project   : WC2939 - Redesign of Work with Holiday Blackout  *
     H*                        Dates                                   *
     H*                                                                    *
     H***********************************************************************
     H* REVISIONS:                                                         *
     H*                                                                    *
     H* 04/30/25 C:344 Project: WC2939                                     *
     H*                Revise to add UX Updates.                           *
     H***********************************************************************
     fwtholblk  cf   e             workstn extfile('WTHOLBLK')
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
     d svSflRRN        s              5  0 inz
     d charMsg         s            200    inz(*blanks)

     d intCo#          s              3P 0
     d chgMod          s               n   inz(*Off)
     d dspMode         s               n   inz(*Off)
     d process         s               n   inz(*Off)
     d isError         s               n   inz
      //**** Beg Add ***** 04/30/25 ****************************************
     d isActivated     s               n   inz
     d @@mode          s              1
      //**** End Add ***** 04/30/25 ****************************************

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
     c                   parm                    @@mode                         mod

       ProgramStart();

       If process;
          Main();
       Endif;

       *InLr = *On;

       //===================================================================
       // Load Holiday Blackout Dates
       //===================================================================
     p LoadHBlkDates   b
     d LoadHBlkDates   pi

       // Clear the subfile
       scSflRRN = 0;
       scSflDsp = *Off;
       scSflClr = *On;
       Write rcdHBlkDt;
       scSflClr = *Off;
       scSflDsp = *On;
       scRowSel = 1;

       // Populate the subfile
       Setll *loval pcb00;
       Dou %eof(WTCBL);
          Read pcb00;
          If %eof(WTCBL);
             Leave;
          Endif;

          //**** Beg Add **** 04/30/25 *************************************
          isActivated = *On;

          If isActivated = *On;
          //**** End Add **** 04/30/25 *************************************

             scSflRRN += 1;
             scSflSel = *Off;

            If svSflRRN = 0;
                If scSflRRN = 1;
                   scSflSel = *On;
                Endif;
             Else;
                If scSflRRN = svSflRRN;
                   scSflSel = *On;
                Endif;
             Endif;

            scHBlkDt = CvtToDate(CBdate);
             scDesc   = CBdesc;
             visActSts = *Off;       // New Hyperlink for Activate
             visIActSts = *Off;      // New Hyperlink for Inactivate

             If CBstat = 'A';
                scStatus = 'Active';
                visIActSts = *On;
             Endif;

             If CBstat = 'I';
                scStatus = 'Inactive';
                visActSts = *On;
             Endif;

             If dspMode;
                visActSts = *Off;
                visIActSts = *Off;
             Endif;

             scRRN = dbrrn;

             Write sflHBlkDt;
          Endif;
       Enddo;

       scSflRRN = 1;
       If svSflRRN <> 0;
          scSflRRN = svSflRRN;
          scRowSel = svSflRRN;
          svSflRRN = 0;
       Endif;

     p LoadHBlkDates   e

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

       btnCancel = *Off;

       // Process until exit button is pressed.
       DoU btnCancel = *On;

          // Load Holiday Blackout Dates
          LoadHBlkDates();

          // Display Screen
          Exsr ExfmtScreens;

          // Perform actions on screen.
          Select;

          // Add Blackout Date
          When btnBlkAdd  = *On;
             btnBlkAdd = *Off;
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
           PnlHeight = 455;
           APnlHeight = PnlHeight + 60;

           Write rcdComPnl;
           Write rcdPanel;

           Exfmt rcdHBlkDt;

           Read rcdComPnl;
           Read rcdPanel;
        EndSl;

       Endsr;

     p Main            e

       //===================================================================
       // ProcessAdd - Process Addition of Holiday Blackout Dates
       //===================================================================
     P ProcessAdd      B
     D ProcessAdd      PI
     d  mode           s              1    Inz
     d  isExit         s               n   Inz(*Off)

       Clear rcdBlkDAdd;
       mode = 'A';
       isExit = *Off;
       visSvAd = *On;
       scStatusE   = 'A';

       Dow isExit = *Off;

          scBlkDtHdg = 'Add Holiday Blackout Date';

          Exfmt rcdBlkDAdd;
          Select;

          // Save Button
          When btnSvAdBlk = *On;
             btnSvAdBlk = *Off;
             If ValidateBlkDt(mode);
                Exsr WrtBlkDt;
                //Initialize fields for next add screen if save/addnext is pressed
                Clear rcdBlkDAdd;
                visSvAd = *On;
                scStatusE = 'A';
             EndIf;

          // Save Button
          When btnSaveBlk = *On;
             btnSaveBlk = *Off;
             If ValidateBlkDt(mode);
                Exsr WrtBlkDt;
                isExit = *On;
             EndIf;

          // Cancel Button
          When btnCanBlk = *On;
             btnCanBlk = *Off;
             isExit = *On;
          EndSl;
       Enddo;

       // Subroutine to move Screen fields into database fields.
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

     P ProcessAdd      E
       //===================================================================
       // ProcOther - Process other Options for Holiday Blackout Dates
       //===================================================================
     P ProcOther       B
     D ProcOther       PI
     d  mode           s              1    Inz
     d  isExit         s               n   Inz(*Off)

       Clear rcdBlkDAdd;
       isExit = *Off;
       visSvAd = *Off;

       ReadC sflHBlkDt;
       Dow not %eof;

          Select;
          When scBlkDtEdt = *On; // Processing for Edit
             scBlkDtEdt = *Off;
             mode = 'C';
             svSflRRN = scSflRRN;  //save rrn value
             Chain(n) scRRN pcb0p;
             Exsr Movdbsc;
             scBlkDtHdg = 'Edit Holiday Blackout Date';
             dsbBlkDt = *On;
             Dow isExit = *Off;
                Exfmt rcdBlkDAdd;

                Select;
                When btnSaveBlk = *On;
                   btnSaveBlk = *Off;

                   // Check if Wtcbp updated since screen displayed
                   flgRcdUpdated=rcdUpdatedErr('WTCBP':dbrrn1:'CBdat':'CBtm':
                    'CBusr':saveDT:saveTM:'1');

                   //**** Beg Add **** 04/30/25 ****************************
                   // Check the Holiday Date Description, a blank
                   // Description is not valid.
                   If scDescE = ' ';
                      erDescE = *On;
                      msDescE = 'WCR0520';
                      flgRcdUpdated = '1'; // Error
                   EndIf;
                   //**** End Add **** 04/30/25 ****************************

                   If flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scRRN pcb0p;
                      Exsr Movdbsc;
                   EndIf;

                   If flgRcdUpdated = '0'; // No error
                      Chain scRRN pcb0p;
                      Exsr Movscdb;
                      Update pcb0p;
                      isExit = *On;
                   Endif;

                When btnCanBlk = *On;
                   btnCanBlk = *Off;
                   isExit = *On;
                EndSl;

             Enddo;

          Endsl;

          Unlock Wtcbp;

          ReadC sflHBlkDt;
       Enddo;

       // Subroutine to move database fields into screen fields
       BegSr MovDbsc;
          scDescE     = CBdesc;
          scBlkDateE  = dtMDYY(CBdate);
          scStatusE   = CBstat;
          saveDT = CBdat;
          saveTM = CBtm;
       EndSr;

       // Subroutine to move screen field to database file
       BegSr Movscdb;
          CBdesc = scDescE;
        //**** Beg Add **** 04/30/25 *********************************
          CBstat = scStatusE;
        //**** End Add **** 04/30/25 *********************************
          CBusr   = q1user;                // User
          CBpgm   = q1pgm;                 // Program
          CBtm    = Timeto6(SystemTime()); // Time
          CBdat   = Dateto7(SystemDate()); // Date Mintained
          saveDT = CBdat;
          saveTM = CBtm;
       EndSr;

     P ProcOther       E

       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

       //**** Beg Add **** 04/30/25 ****************************************
       isFirstLod = 'Y';

       If @@mode = '1';
          dspMode = *On;
       Else;
          dspMode = *Off;
       Endif;
       //**** End Add **** 04/30/25 ****************************************

       // Tab Index
       tabIndex = 0;

       intCo# = GetUserInfo(q1user);
       Chain intCo# pco00;
       If %found(SMCOL) and COname <> ' ';
          scCompName = COname;
       Endif;

       // Retrieve user parameter
       Chain q1user pup00;

       process = *On;

       Return;

     p ProgramStart    e

      //===================================================================
      // Validate the Holiday Blackout Date
      // Returns *On  ----> No errors
      // Returns *Off ----> Any errors
      //===================================================================
     p ValidateBlkDt   b
     d ValidateBlkDt   pi              n
     d strMode                        1

       // Initialize all the messageId and error indicators.
        isError = *Off;
        erBlkDateE = *Off;
        msBlkDateE = *blanks;
        //**** Beg Add **** 04/30/25 **********************************
        erDescE    = *Off;
        msDescE    = *blanks;
        //**** End Add **** 04/30/25 **********************************

        If strMode = 'A';

           // Valid Holiday date should be entered
           If scBlkDateE = 0;
              erBlkDateE = *On;
              msBlkDateE = 'WCR3309';
              isError = *On;
           Endif;

           // Check date is valid or not
           If erBlkDateE = *Off;
              erBlkDateE = ValidDate(scBlkDateE:charMsg);
              If erBlkDateE = *On;
                 msBlkDateE = charMsg;
                 isError    = *On;
              Endif;
           Endif;

           // Holiday Date should be > than today's date
           If erBlkDateE = *Off;
              If dtCYMD(scBlkDateE) <= Dateto7(SystemDate());
                 erBlkDateE = *On;
                 msBlkDateE = 'WCR3308';
                 isError = *On;
              Endif;
           Endif;

           // Check if date is Weekend date
           If erBlkDateE = *Off;
              outCo#  = %editc(intco#:'X');
              outDti  = %editc(scBlkDateE:'X');
              outSrd  = *blanks;
              outMod  = '1';
              outDto  = *blanks;
              outRtcd = *blank;

              GetWeekendDt(outCo#:outDti:outSrd:outMod:outDto:outRtcd);
              If outRtcd = '0';
                 erBlkDateE = *On;
                 msBlkDateE = 'WCR3310';
                 isError = *On;
              Endif;
           Endif;

           // Check if duplicate records exist
           If erBlkDateE = *Off;
              Chain (intco#:dtCYMD(scBlkDateE)) pcb00;
              If %found(WTCBL);
                 erBlkDateE = *On;
                 msBlkDateE = 'WC30661';
                 isError = *On;
              Endif;
           Endif;
        Endif;

        //**** Beg Add **** 04/30/25 ***************************************
        // Holiday Date Description cannot be blank.
        If scDescE = ' ';
           erDescE = *On;
           msDescE = 'WCR0520';
           isError = *On;
        EndIf;
        //**** End Add **** 04/30/25 ***************************************

       // Return *On indicator when No error exists.
        If isError = *On;
           Return *Off;
        Else;
           Return *On;
        Endif;

     p ValidateBlkDt   e
