     hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h/title WTPOLST - State Code file for Reporting
     h/define   PROFOUNDUI
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description:  This Program Will Used to Add State code for States.
     h*
     h*  Called from: Menu
     h*  Calls to:
     h*
     h*  Created on 1/14/2015 by C:282 for Project: WC2334
     h**********************************************************************
     h*  Revisions:
      *                                                               *
      *    03/24/22 C:333 Project: SY434                              *
      *                   - Update Grids for Sorting Filtering Upgrade*
      *                                                               *
      *    01/07/16 C:65  Project: WC2401                             *
      *                   Recompile Only                              *
      *                                                               *
      *    09/17/15 C:302 Project: WC2254                             *
      *                   Recompile Only                              *
     h*
     h**********************************************************************
     fwtpolst   cf   e             workstn extfile('WTPOLST')
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflStCd:rrnStCd)
     fWtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)

     fwtstp     uf a e             disk    rename(pst01:pst0p)
     f                                     Infds(dbfds1)
     fsmcop     if   e             disk    usropn
     f                                     rename(pco01:pco0p)
     fststl3    if   e           k disk    rename(pst01:pst00)

     d psds          esds                  extname(szq1p)

     d dbfds1          ds
     d  dbrrn1               397    400b 0

     d @@mode          s              1
     d @@TabIndex      s              5
     d inmode          s              1
     d inTabIndex      s              5  0
     d intCo#          s              3  0
     d ##rrn           s              9  0
     d intrrn          s              9  0
     d clientID        s              3    inz('000')
     d isValid         s              1n   inz(*off)
     d charState       s             10

      * Add Records
     d AddStCd         pr

      * Clear Subfiles
     d ClrSubFiles     pr

      * Edit/Add State Code
     d EditAddStCd     pr
     d inRRN                          9  0 options(*nopass)

      * Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d ##CltId                        3

      * Load State Code value grid
     d LoadStCdGrid    pr

      * Main Processing
     d Main            pr

      * Program Start
     d ProgramStart    pr

      * Process other options
     d ProcOther       pr

      * Add/Edit State value code
     d ProcessAdd      pr

      * Validate Add/Edit screen
     d Validate        pr

     d  ##CltId        s              3

     d/copy sPrDate
     d/copy sPrString
     d/copy SPrCompany
     d/copy wPrUsrPrms
    
      * Entry of Program.
     c     *entry        plist
     c                   parm                    @@Mode
     c                   parm                    @@TabIndex

      /free
       // Start Of Program
       ProgramStart();

       // Main Procesing
       Main();

       *inlr = *on;
       return;
      /end-free
       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /free

       // Program Mode.
       If %parms() >= 1;
          inmode = @@Mode;
       Endif;

       Select;
       When inmode = 'D';
          dspMode = *On;
          dspVisb = *Off;
       When inmode = 'A';
          dspVisb = *On;
       When inmode = 'C';
          dspVisb = *On;
       Endsl;

       // Tab Index Mode.
       If %parms() >= 2;
          intabIndex = %dec(@@TabIndex:5:0);
       Else;
          intabIndex = 0;
       EndIf;

       // Load Action panel with the Company Name.
       clear scCompName;
       intCo# = userCo#(q1user);
       // Get company name
       scCompName = GetCompNam(intCo#);
       aPnlHeight = 610;

       // Get the client id
       GetClientID(##CltId);

       tabClick = *Off;
       ##rrn = 0;
       Return;

      /end-free
     p ProgramStart    e
       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

      /free

       // Process until exit button is pressed.
       Dou ExitNow = *On ;

          // Clear Sub-files
          ClrSubfiles();

          // Load State Code Grid
          LoadStCdGrid();

          // Process State Code Screen
          Exsr ExfmtScreens;

          Select;
          // do nothing - catch tab click event
          When tabClick = *on;
             tabClick = *off;

          When BtnRefresh = *On;
             BtnRefresh =*Off;

          // Add Payment value
          When scStAdd = *on;
             EditAddStCd();

          Other;

             ProcOther();

          Endsl;

       Enddo;
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       Select;

       When TabIndex = 0;
          TabHeight = 550;
          Write rcdComPnl;
          Write rcdTabStcd;
          Exfmt rcdStCd;
          Read rcdComPnl;
          Read rcdTabStcd;
       EndSl;

       Endsr;

      /end-free
     p Main            e
       //===================================================================
       // ClrSubFiles - Clears SubFiles
       //===================================================================
     p ClrSubFiles     b
     d ClrSubFiles     pi
      /Free

       rrnStCd = 0;
       clrStCd = *On;
       dspStSub  = *Off;
       dspStCtl  = *Off;
       Write rcdStCd;

      /End-Free
     p ClrSubfiles     e
       //===================================================================
       // Load Payment Value 1 code grid
       //===================================================================
     p LoadStCdGrid    b
     d LoadStCdGrid    pi

      /free

       // clear the subfile.
       clear sflStCd;
       clrStCd = *Off;
       dspStSub = *On;
       dspStCtl = *On;
       clear dbrrn1;

       Setll 1 pst0p;
       Read  pst0p;
       Dow Not %Eof(wtstp);
          // State
          scState = STstate;
          // State Code
          scStCd   = STcode;
          // Report for zero amount Flag
          scPrtZrFlg = STflag;
          // Status
          If STstat = 'A';
             scStatus  = 'Active';
          Else;
             scStatus  = 'Inactive';
          Endif;

          scStrrn# = dbrrn1;
          rrnStCd = rrnStCd + 1;

          If inmode = 'D';
             dspVisb = *Off;
          Else;
             dspVisb = *On;
          Endif;
          Write sflStCd;

          Read pst0p;
       Enddo;

       rrnStCd = 1;

      /end-free
     p LoadStCdGrid    e
       //===================================================================
       // ProcOther - process other options
       //===================================================================
     p ProcOther       b
     d ProcOther       pi
      /free
       // Edit State Code
       Readc sflStCd;
       Dow not %eof;
          If scStCdEdt = *on;
             scStCdEdt = *off;
             ##rrn = scStRRN#;
             EditAddStCd(##rrn);
          Endif;
          Readc sflStCd;
       Enddo;
      /End-Free
     p ProcOther       e
       //===================================================================
       // EditAddStCd  - Edit/Add State Code
       //===================================================================
     p EditAddStCd     b
     d EditAddStCd     pi
     d inRRN                          9  0 options(*nopass)

      /Free
       // To get the company number
       intCo# = UserCo#(q1user);

       // Program Mode.
       If %parms() >  0;
          inmode  = 'C';
          intRRN  = inRRN;
       Else;
          inmode  = 'A';
          intRRN  = *zeros;
       Endif;

       ProcessAdd();

       intRRN  = *zeros;
       return;
      /End-Free
     p EditAddStCd     e
       //===================================================================
       // ProcessAdd - process addition of Payment Value 1 Code
       //===================================================================
     p ProcessAdd      b
     d ProcessAdd      pi

      /free
       clear RcdAdStCd;

       btnAddRcd = *off;
       btnCanRcd = *off;

       // processing for Edit mode
       If inmode = 'C' and intRrn <> 0;
          Chain intRrn pst0p;
          If %Found(wtstp);
             indAdStCd = *on;
             scState     = STstate;
             scStCd      = STcode;
             scPrtZrFlg  = STflag;
             scStatus    = STstat;
          Endif;
       Endif;

       // Processing for Add mode
       If inmode = 'A';
       // indStsDis  = *on;
          scState    = *blanks;
          scStCd     = *zero;
          scPrtZrFlg = *blanks;
          scStatus   = *blanks;
       Endif;

       Dou BtnCanRcd = *On or (BtnAddRcd = *on and isValid = *off);
          Exfmt rcdAdStCd;
          isValid = *off;

          erState    = *off;
          erPrtZrFlg = *off;
          erStatus   = *off;

          Select;
             When BtnAddRcd = *on;
                Validate() ;
                If isValid = *off;
                   AddStCd();
                Endif;
             When BtnCanRcd = *on;
                BtnCanRcd = *off;
                Leave;
          Endsl;
       Enddo;
      /end-free
     p ProcessAdd      e
       //===================================================================
       // Validate - Edit/Add State Code
       //===================================================================
     p Validate        b
     d Validate        pi
      /Free

       charState = *blanks;
       If inmode = 'A';
          // Check if Sate is valid.
          charState = Upper(scState);
          Chain(n) charState pst00;
          If Not %Found(ststl3);
             erState = *On;
             isValid = *On;
          EndIf;

          If scStatus = *blank;
             erStatus = *On;
             isValid  = *On;
          EndIf;

       EndIf;

       If (Upper(scPrtZrFlg) = 'N' Or Upper(scPrtZrFlg) = 'Y');

       Else;
          erPrtZrFlg = *On;
          isValid = *On;
       EndIf;

      /End-Free
     p Validate        e
       //===================================================================
       // AddStCd -  Add/Edit State Codes Values
       //===================================================================
     p AddStCd         b
     d AddStCd         pi

      /free
        STstate = Upper(scState);
        STcode  = scStCd;
        STflag  = Upper(scPrtZrFlg);
        STstat  = scStatus;
        STpgm  = q1pgm;
        STuser = q1user;
        STdt   = DateTo7(SystemDate);
        STtime = TimeTo6(SystemTime);

        If inmode = 'A';

           Open smcop;
           Setll 1 pco0p;
           Read pco0p;
           STCo# = COco#;
           Close smcop;

           STipgm = q1pgm;
           STiusr = q1user;
           STidt  = DateTo7(SystemDate);
           STitm  = TimeTo6(SystemTime);
           Write pst0p;
        Else;
           Update pst0p;
        Endif;

      /end-free
     p AddStCd         e
