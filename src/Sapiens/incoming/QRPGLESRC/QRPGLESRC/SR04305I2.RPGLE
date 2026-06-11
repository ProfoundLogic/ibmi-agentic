     hcopyright('(c) 2025 Sapiens, Inc. All rights reserved.')
      //*********************************************************************
     h/title SR04305I2 - Addition Entry pgm for WC040305
     h dftname(SR04305I2)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h ALWNULL(*UsrCtl)
     h/define   profoundUI
     h bnddir('WTBNDDIR')
      //*********************************************************************
      //==============================================================*
      //  Created   : 12/27/24                                         *
      //  Programmer: C:300                                            *
      //  Project   : WC2994 - Redesign of Addition Entry pgm for WC040305
      //                                                               *
      //===============================================================*
      //  Revisions:                                                   *
      //                                                               *
      //                                                               *
      // ------------------------------------------------------------- *
      //  NOTE: Please add comments to the top of Revisions.           *
      // ------------------------------------------------------------- *
      //===============================================================*

     fsr04305f2 cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflAddInfo:scSflRRN)
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdPolPnl)
     f                                     include(rcdQtePnl)
     f                                     include(rcdComPnl)
      * Company file
     fsmcol     if   e           k disk    rename(pco01:pco00)
      * User Parameter File
     fsmupl     if   e           k disk    rename(pup01:pup00)
      * Group Master File
     fwmfnl     if   e           k disk    rename(pfn01:pfn00)
      * Employer Master File
     fwmeml     if   e           k disk    rename(pem01:pem00)
      * Employer Detail File
     fWdell     if   e           k disk    rename(pel01:pel00)
      * Additional Information File
     fwxfmp1    uf a e           k disk    rename(pfm01:pfm0p)
     f                                     infds(dbfds)
     fwxfml11   if   e           k disk    rename(pfm01:pfm00)
     f                                     prefix(L1_)
     fwxfml12   uf   e             disk    rename(pfm01:pfm02)
     f                                     prefix(L2_)
     f                                     infds(dbfds2)

     d dbfds           ds
     d  dbrrn                397    400b 0
     d dbfds2          ds
     d  dbrrn2               397    400b 0

     d psds          esds                  extname(szq1p)

      // work variables
     d saveDT          s                   like(F3dt)
     d saveTM          s                   like(F3time)
     d flgRcdUpdated   s              1    inz
     d svsflrrn        s              5  0 inz
     d isDelresp       s               n   inz
     d isMenuAvbl      s               n   inz(*on)

     D @@call          s              1
     D @@prds          s             40
     D @@co#           s              3
     D @@emp#          s              9
     D @@div           s              5
     D @@fnd           s              3
     D @@fyr           s              3
     D xxForm          s             15    inz('WC040305  ')
     D xxco#           s                   like(F3co#)
     D xxfnd           s                   like(F3fnd)
     D xxemp#          s                   like(F3emp#)
     D xxdiv           s                   like(F3div)
     D xxfyr           s                   like(F3fyr)
     d scfnd           s                   like(F3fnd)
     d scfyr           s                   like(F3fyr)
     d scemp#          s                   like(F3emp#)
     d scdiv           s                   like(F3div)
     d xxRrn           s              9  0

     d intCo#          s              3P 0
     d chgMod          s               n   inz(*off)
     d dspMode         s               n   inz(*off)
     d process         s               n   inz(*off)
     d isError         s               n   inz

      * Load Additional Information
     d LoadAddInfo     pr

      * Main Processing
     d Main            pr

      * Process Addition of Office Information
     d ProcessAdd      pr

      * Process Edit option for Office Information
     d ProcOther       pr

      * Program Start
     d ProgramStart    pr

      * Validate Additional Information
     d ValidateAddInf  pr              n
     d strmode                        1

     d/copy sPrDate
     d/copy sPrGenPop
     d/copy sPrCompany
     d/copy sPrUsrPrms
     d/copy sPrGenErr
     d/copy sPrModule
     d/copy sCopyPolPr
     d/copy sPrEmail
     d/copy sPrAtrium
     d/copy sPrString
     d/copy wPrAgent
     d/copy wPrProduce
     d/copy wPrGroup
     d/copy wPrRateSet
     d/copy wPrEmpLr
     d/copy wPrCarrier

      // Entry of Program.

     c     *entry        plist
     C                   parm                    @@prds
     C                   parm                    @@call
     C                   parm                    @@co#
     C                   parm                    @@fnd
     C                   parm                    @@emp#
     C                   parm                    @@div
     C                   parm                    @@fyr


      ** Additional Information file
     C     fmky1         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
     C                   kfld                    xxform
      *
     C     fmky2         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
     C                   kfld                    xxfyr
     C                   kfld                    xxform
      *
     c     keyWdell      Klist
     c                   Kfld                    xxCo#
     c                   Kfld                    xxFnd
     c                   Kfld                    xxEmp#
     c                   Kfld                    xxDiv
     c                   Kfld                    xxFyr

      /Free

       ProgramStart();

       If process;
          Main();
       Endif;

       *InLr = *On;

      /End-Free

       //===================================================================
       // Load Additional Information
       //===================================================================
     p LoadAddInfo     b
     d LoadAddInfo     pi

      /free
       // Clear the subfile
       scSflRRN = 0;
       scSflDsp = *off;
       scSflClr = *on;
       Write rcdAddInfo;
       scSflClr = *off;
       scSflDsp = *on;
       scRowSel = 1;

       // Populate the subfile
       Setll fmky1 pfm0p;
       Reade fmky1 pfm0p;
       Dow not %eof(wxfmp1);

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
          scSeq# = F3seq#;
          scName = %subst(F3500a1:1:60);

          scRRN = dbrrn;
          Write sflAddinfo;
          Reade fmky1 pfm0p;
       Enddo;

       scSflRRN = 1;
       If svSflRRN <> 0;
          scSflRRN = svSflRRN;
          scRowSel = svSflRRN;
          svSflRRN = 0;
       Endif;

      /end-free
     p LoadAddInfo     e

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

      /Free

       // Process until exit button is pressed.
       BtnCanInfo = *off;

       // Read the Employer Detail File
       Chain keyWdell pel00;

       // Load Policy Action Bar
       LoadActionBar(isMenuAvbl);

       // Process until exit button is pressed.
       DoU BtnCanInfo = *On;

          // Load Office Information
          LoadAddInfo();

          // Display Screen
          Exsr ExfmtScreens;

          // Perform actions on screen.
          Select;

          // Add Additional Information
          When BtnAddInfo = *On;
             BtnAddInfo = *Off;
             ProcessAdd();

          // Refresh
          When btnRefresh = *On;
             btnRefresh = *Off;

          // Save
          When btnSave = *On;
             btnSave = *Off;

          Other;
             ProcOther();             // Process other Options

          EndSl;

       Enddo;

       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

        Select;

        When TabIndex = 0;
           PnlHeight = 900;
           APnlHeight = PnlHeight + 60;

           Write rcdPolPnl;
           Write rcdPanel;

           Exfmt rcdAddInfo;

           Read rcdPolPnl;
           Read rcdPanel;
        EndSl;

       Endsr;

      /End-Free

     p Main            e

       //===================================================================
       // ProcessAdd - Process Addition of Additional Information
       //===================================================================
     P ProcessAdd      B
     D ProcessAdd      PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdAddEdt;
       mode = 'A';
       isexit = *off;
       BtnCan = *off;
       visSvAdNx = *on;

       Dow BtnCan = *off and isexit = *off;

          scAddHdg = 'Voluntary Comp and Employers Liability Coverage Add';
          scSeq#E = 1;                      // Sequence #

          // Get Next Sequence #
          Setgt fmky2 pfm00;
          Readpe fmky2 pfm00;
          If not %eof(wxfml11);
             scSeq#E = L1_F3seq# + 1;       // Sequence #
          Endif;

          Exfmt rcdAddEdt;

          Select;
          // save/Add Next Button
          When BtnSvAdNx = *on;
             BtnSvAdNx = *off;
             If ValidateAddInf(mode);
                Exsr WrtAddInfo;
                Clear rcdAddEdt;
                visSvAdNx = *on;
                BtnCan = *off;
             EndIf;

          // Save Button
          When BtnSaveInf = *on;
             BtnSaveInf = *off;
             If ValidateAddInf(mode);
                Exsr WrtAddInfo;
                isexit = *on;
             EndIf;

          // Cacel Button
          When BtnCan = *on;
             BtnCan = *off;
             isexit = *on;
          EndSl;
       Enddo;

       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        F3co#   = xxCo#;                 // Company Number
        F3fnd   = xxfnd;                 // Fund
        F3emp#  = xxemp#;                // Employer number
        F3fyr   = xxfyr;                 // Year
        F3div   = xxdiv;                 // Division
        F3form  = xxform;                // Endorsement Form
        F3seq#  = scSeq#E;               // Sequence #

        %subst(F3500a1:1:60) = scNameE;  // Name in Generic 500 Chr 1

        F3stat  = 'A';                   // Status
        F3user  = q1user;                // User
        F3pgm   = q1pgm;                 // Program
        F3time  = Timeto6(SystemTime()); // Time
        F3dt    = Dateto7(SystemDate()); // Date Mintained
        F3iusr  = q1user;                // Initial User
        F3ipgm  = q1pgm;                 // Initial Program
        F3idt   = Dateto7(SystemDate()); // Initial Date

       EndSr;

       // Writing new Office Information
       BegSr WrtAddInfo;
        Clear pfm0p;
        Exsr MoveScDb;
        Write pfm0p;
       EndSr;

      /end-free
     P ProcessAdd      E

       //===================================================================
       // ProcOther - Process other Options for Additional Information
       //===================================================================
     P ProcOther       B
     D ProcOther       PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdAddEdt;
       visSvAdNx = *off;
       isexit = *off;

       ReadC sflAddInfo;
       Dow not %eof;

          Select;
          When scEdt = *On;                       // Processing for Edit
             scEdt = *Off;
             Chain(e) scRRN pfm02;
             If %status = 1218;
                PopOneBtn('Record locked by another user. Please try again-
                 later.':'Record Lock');
                Return;
             Endif;

             mode = 'C';
             svsflrrn = scSflrrn;  //save rrn value
             Chain(n) scRRN pfm02;
             Exsr Movdbsc;
             scAddHdg = 'Voluntary Comp and Employers Liability Coverage Edit';
             Dow isexit = *off;
                Exfmt rcdAddEdt;

                Select;
                When BtnSaveInf = *on;
                   BtnSaveInf = *off;
                   // Check if WXFMP1 updated since screen displayed
                   flgRcdUpdated=RcdUpdatedErr('WXFMP1':dbrrn2:'F3dt':'F3time':
                    'F3user':saveDT:saveTM:'1');
                   if flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scRRN pfm02;
                      Exsr Movdbsc;
                   EndIf;
                   If flgRcdUpdated = '0'; // No error
                      // Validate the Additional Information
                      If ValidateAddInf(mode);
                         Chain scRRN pfm02;
                         Exsr Movscdb;
                         update pfm02;
                         isExit = *on;
                      Endif;
                   Endif;
                When BtnCan = *on;
                   BtnCan = *off;
                   isExit = *on;
                EndSl;
             Enddo;

          When scDlt = *On;                       // Processing for Delete
             scDlt = *Off;
             Chain(e) scRRN pfm02;
             If %status = 1218;
                PopOneBtn('Record locked by another user. Please try again-
                 later.':'Record Lock');
                Return;
             Endif;

             Chain scRRN pfm02;
             Exsr Movdbsc;
             scAddHdg='Voluntary Comp and Employers Liability Coverage Delete';
             Dow isexit = *off;
                Write rcdAddEdt;
                isDelresp = PopTwoBtn('Do you want to Delete this Additional +
                 Information?':'Confirm Delete':'Yes':'No');
                Select;
                When isDelresp = *On;
                   Delete pfm02;
                   PopOneBtn('Additional Information Deleted!':'Delete':'Ok');
                   isexit = *on;
                   svsflrrn = 0;         // Reload the master grid from top
                When isDelresp = *Off;
                   svsflrrn = scSflrrn;  //save rrn value
                   isexit = *on;
                EndSl;
             EndDo;

          Endsl;

          Unlock Wxfml12;

          ReadC sflAddInfo;
       Enddo;

       // Subrotine to move database fields into screen fields
       BegSr MovDbsc;
        scSeq#E = L2_F3seq#;
        scNameE = %subst(L2_F3500a1:1:60);

        saveDT = L2_F3dt;
        saveTM = L2_F3time;
       EndSr;


       // Subrotine to move screen field to database file
       BegSr Movscdb;
        %subst(L2_F3500a1:1:60) = scNameE;  // Name in Generic 500 Chr 1

        L2_F3user  = q1user;                // User
        L2_F3pgm   = q1pgm;                 // Program
        L2_F3dt    = Dateto7(SystemDate()); // Date Mintained
        L2_F3time  = Timeto6(SystemTime()); // Time

        saveDT = L2_F3dt;
        saveTM = L2_F3time;
       EndSr;

      /end-free
     P ProcOther       E

       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /Free

       // Retrieve user parameter
       Chain q1user pup00;

       xxco#  = %int(@@co#);
       xxfnd  = %int(@@fnd);
       xxemp# = %int(@@emp#);
       xxdiv  = %int(@@div);
       xxfyr  = %int(@@fyr);

       process = *on;

       Return;

      /End-Free

     p ProgramStart    e

      //===================================================================
      // Validate Additional Information
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateAddInf  b
     d ValidateAddInf  pi              n
     d strmode                        1

      /Free

       // Initialize all the messageId and error indicators.
        isError = *off;
        erNameE = *off;
        msNameE = *blanks;

        // Name can not be Blank
        If scNameE = *blanks;
           erNameE = *on;
           msNameE = 'WCR0503';
           isError = *on;
        Endif;

      // Return *on indicator when No error exists.
        If isError = *on;
           return *off;
        Else;
           return *on;
        Endif;

      /End-Free
     p ValidateAddInf  e

       //===================================================================
      * Utility policy procedure definitions
      /COPY SCOPYPOLPI

