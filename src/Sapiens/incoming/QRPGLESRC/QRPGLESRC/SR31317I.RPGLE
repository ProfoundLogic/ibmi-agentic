     hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
     h*****************************************************************
     h/title SR31317I- NY - Labor Contractor Endorsement
     h**********************************************************************
     h*
     h*  Description:  Collects additional information for form WC310317
     h*
     h*  Called from: SR31317
     h*  Calls to:    None
     h*
    H* Created: 10/25/17   C:33   Project: WC2570
     h**********************************************************************
     H*                                                               *
     H*  THIS PROGRAM COLLECTS CUSTOM DATA FOR ENDORSEMENTS AND       *
     H*  STORES IT IN FILE WDL4P, USING GENERIC DATA FIELDS.          *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     fSR31317F  cf   e             workstn extfile('SR31317F')
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflEndr:rrnEndr)
     fWtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdPolPnl)
     f                                     include(rcdQtePnl)
     f                                     include(rcdComPnl)

     fwmeml     if   e           k disk    rename(pem01:pem00)
     fwdell     if   e           k disk    rename(pel01:pel00)
     Fwdl4p     uf a e             disk    rename(pL401:pl40p)
     Fwdl4l     if   e           k disk    rename(pL401:pl400)
     F                                     infds(dbfds1)

     d psds          esds                  extname(szq1p)

     d dbfds1          ds
     d  dbrrn1               397    400b 0

     d @@prds          s             40
     d @@Mode          s              1
     d @@Co#           s              3
     d @@Fnd           s              3
     d @@Emp#          s              9
     d @@Div           s              5
     d @@Fyr           s              3
     d @@TabIndex      s              5
     d xxCo#           s                   like(ELco#)
     d xxFnd           s                   like(ELfnd)
     d xxEmp#          s                   like(ELemp#)
     d xxDiv           s                   like(ELdiv)
     d xxFyr           s                   like(ELfyr)
     d scfnd           s                   like(ELfnd)
     d scfyr           s                   like(ELfyr)
     d scemp#          s                   like(ELemp#)
     d scdiv           s                   like(ELdiv)
     d svSeq#          s                   like(L4Seq#)
     d clientID        s              3    inz('000')
     d inmode          s              1
     d inProcMod       s              1
     d inTabIndex      s              5  0
     d intCo#          s              3  0
     d intRrn          s              9  0
     d xxRrn           s              9  0
     d ##Rrn#          s              9  0
     d ##CltId         s              3
     d isMenuAvbl      s               n   inz(*on)
     d isValid         s              1n   inz(*off)
     d isError         s              1n   inz(*off)
     d xxForm          s             15    inz('WC310317  ')
      *****************************************************************
      * Prototype Declaration
      *****************************************************************
      * Clear Subfiles
     d ClrSubFiles     pr

      * Edit/Add Endorse Record
     d EdtAddEndr      pr

      * Edit/Del/Add Endorse Record
     d EditDelAddEndr  pr
     d inRRN                          9  0 options(*nopass)

      * Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d ##CltId                        3

      * Load Endorse Value grid
     d LoadEndrGrid    pr

      * Main Processing
     d Main            pr

      * Add/Del/Edit Endorse Value record
     d ProcEdtDelAdd   pr

      * Process other options
     d ProcOther       pr

      * Program Start
     d ProgramStart    pr

      * Validate Add/Edit screen
     d Validate        pr

     d/copy sPrAtrium
     d/copy sPrDate
     d/copy sPrString
     d/copy sPrCompany
     d/copy sCopyPolPr
     d/copy wPrAgent
     d/copy wPrProduce
     d/copy wPrGroup
     d/copy wPrRateSet
     d/copy wPrEmpLr
     d/copy wPrCarrier
     d/copy wPrUsrPrms
    
      * Entry of Program.
     c     *Entry        Plist
     c                   Parm                    @@Prds
     c                   Parm                    @@Mode
     c                   Parm                    @@Co#
     c                   Parm                    @@Fnd
     c                   Parm                    @@Emp#
     c                   Parm                    @@Div
     c                   Parm                    @@Fyr

      /Free

       // Start Of Program
       ProgramStart();

       // Main Procesing
       Main();

       *InLr = *on;

      /End-Free
       //===================================================================
       // ClrSubFiles - Clears SubFiles
       //===================================================================
     p ClrSubFiles     b
     d ClrSubFiles     pi

      /Free

       rrnEndr = 0;
       clrEndr = *On;
       dspEndrSub  = *Off;
       dspEndrCtl  = *Off;
       Write rcdEndr;

      /End-Free

     p ClrSubfiles     e
       //===================================================================
       // EdtAddEndr -  Upd/Add Endorse Record in WXFMP1 Endorse File.
       //===================================================================
     p EdtAddEndr      b
     d EdtAddEndr      pi

      /Free

       L4co#  = xxcO#;
       L4fnd  = xxFND;
       L4fyr  = xxFYR;
       L4emp# = xxEMP#;
       L4div  = xxDIV;
       L4form = xxForm;

       L4seq# = scSeq#;
       L4nam1 = scName;

       //Address
       L4adr1 = scAddr1;
       L4adr2 = scAddr2;
       L4cty  = scCity;
       L4st   = scSt;
       L4zip  = scZip;

       L4pgm  = q1pgm;
       L4user = q1user;
       L4dt   = DateTo7(SystemDate);
       L4time = TimeTo6(SystemTime);

        If inProcMod = 'A';
           L4iUsr = L4user;
           L4iPgm = L4pgm;
           L4iDt  = L4dt;
           L4iTm  = L4time;
           Write pl40p;
        Else;
           Update pl40p;
        Endif;

      /End-Free

     p EdtAddEndr      e
       //===================================================================
       // EditDelAddEndr  - Edit/Del/Add Endorse Record
       //===================================================================
     p EditDelAddEndr  b
     d EditDelAddEndr  pi
     d inRRN                          9  0 options(*nopass)

      /Free

       // To get the company number
       intCo# = UserCo#(q1user);

       // Program Mode.
       If %parms() > 0;
          intRrn = inRrn;
       Else;
          inProcMod = 'A';
          intRrn = *zeros;
       Endif;

       ProcEdtDelAdd();

       intRrn  = *zeros;
       Return;

      /End-Free

     p EditDelAddEndr  e
       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

      /Free
       xxCo#  =  %Int(@@Co#);
       xxFnd  =  %Int(@@Fnd);
       xxEmp# =  %Int(@@Emp#);
       xxDiv  =  %Int(@@Div);
       xxFyr  =  %Int(@@Fyr);

       // Read the Employer Detail File
       Chain (xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr) pel00;

       // Load Policy Action Bar
       LoadActionBar(isMenuAvbl);

       // Process until exit button is pressed.
       Dou ExitNow = *On ;

          // Clear Sub-files
          ClrSubfiles();

          // Load Endorse Grid
          LoadEndrGrid();

          // Process Endorse Screen
          Exsr ExfmtScreens;

          Select;
          // do nothing - catch tab click event
          When tabClick = *on;
             tabClick = *off;
          When BtnRefresh = *On;
             BtnRefresh =*Off;
          // Add/Edit Detail Record
          When btnEndrAdd = *on;
             EditDelAddEndr();
          When btnEndrCan = *on;
             Leave;
          Other;
             ProcOther();
          EndSl;

       Enddo;
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       Select;

       When TabIndex = 0;
          TabHeight = 1050;
          APnlHeight = TabHeight + 50;
          Write rcdPolPnl;
          Write rcdTabEndr;
          Exfmt rcdEndr;
          Read rcdPolPnl;
          Read rcdTabEndr;
       EndSl;

       Endsr;

      /End-Free

     p Main            e
       //===================================================================
       // Load Endorse Value Detail Grid
       //===================================================================
     p LoadEndrGrid    b
     d LoadEndrGrid    pi

      /Free

       // Clear the subfile.
       Clear sflEndr;
       clrEndr = *Off;

       If inMode = 'D';
          dspVisb = *Off;
       Else;
          dspVisb = *On;
       Endif;

       dspEndrSub = *On;
       dspEndrCtl = *On;
       clear dbrrn1;

       Setll (xxCo#:xxFnd:xxFyr:xxEmp#:xxDiv:xxForm) pl400;
       Dou %eof(wdL4l);
          Reade (xxCo#:xxFnd:xxFyr:xxEmp#:xxDiv:xxForm) pl400;
          Select;
          When %eof;
             Iter;
          Endsl;

          scSeq# = L4seq#;
          scName = L4nam1;
          scAddr = %trim(L4adr1) + ' ' + %trim(L4adr2) + ' ' +
           %trim(L4cty)  + ', ' + %trim(L4st) + ' ' +
           %editw(L4zip:'      -    ');

          scEndrRrn# = dbrrn1;
          rrnEndr = rrnEndr + 1;

          Write sflEndr;
       Enddo;

       rrnEndr = 1;

      /End-Free

     p LoadEndrGrid    e
       //===================================================================
       // ProcEdtDelAdd - Edit/Del/Add Endorse Record
       //===================================================================
     p ProcEdtDelAdd   b
     d ProcEdtDelAdd   pi

      /Free

       Clear RcdAdEndr;

       btnAddRcd = *off;
       btnCanRcd = *off;

       // Process to Delete Endorse Record
       If inProcMod = 'D' and intRrn <> 0;
          Delete(e) intRrn pl40p;
       Endif;

       // Processing to Edit Endorse Record
       If inProcMod = 'C' and intRrn <> 0;
          Clear pl40p;
          Chain intRrn pl40p;
          scSeq#  = L4Seq#;
          scName  = L4nam1;
          scAddr1 = L4adr1;
          scAddr2 = L4adr2;
          scCity  = L4cty;
          scSt    = L4st;
          scZip   = L4zip;

          // Intialize the left alignment of Cancel Button.
          scCanLft = 680;

          // Save/Add Next Button will not get displyed.
          indSvAdNx = *Off;

       Endif;

       Dou (BtnCanRcd  = *on) or (BtnAddRcd = *on and not isValid)
            or (BtnAdNxRcd  = *off and not isValid);
          // Processing to Add Endorse Record
          If inProcMod = 'A' and not isValid;
             Clear RcdAdEndr;
             //Get Next sequence number
             Clear L4seq#;
             Setgt  (xxCo#:xxFnd:xxFyr:xxEmp#:xxDiv:xxForm:*hival) pl400;
             Readpe (xxCo#:xxFnd:xxFyr:xxEmp#:xxDiv:xxForm) pl400;
             scSeq# = L4seq# + 1;
             // Intialize the left alignment of Cancel Button.
             scCanLft = 570;
             // Save/Add Next Button will get displyed.
             indSvAdNx = *On;
          Endif;

          Exfmt rcdAdEndr;
          isValid = *off;

          Select;
             When BtnAddRcd =  *on or BtnAdNxRcd = *on or scEndrEdt = *On;
                Validate() ;
                If isValid = *off;
                   EdtAddEndr();
                Endif;
             When BtnCanRcd = *on;
                BtnCanRcd = *off;
                Leave;
          Endsl;
       Enddo;

      /End-Free

     p ProcEdtDelAdd   e
       //===================================================================
       // ProcOther - Process Other Options
       //===================================================================
     p ProcOther       b
     d ProcOther       pi

      /Free

       // Edit Endorse Record

       ReadC sflEndr;
       Dow Not %Eof;

          If scEndrEdt  = *On;
           //scEndrEdt  = *Off;
             inProcMod = 'C';
             ##Rrn# = scEndrRRN#;
             EditDelAddEndr(##Rrn#);
             scEndrEdt  = *Off;
          Endif;

          If scEndrDel = *On;
             scEndrDel = *Off;
             inProcMod = 'D';
             ##Rrn#  = scEndrRRN#;
             EditDelAddEndr(##Rrn#);
          Endif;

          ReadC sflEndr;

       Enddo;

      /End-Free
     p ProcOther       e
       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /Free

       // Program Mode.
       If q1prms >= 2;
          inmode = 'A';
       Endif;

       Select;
       When inmode = 'D';
          dspVisb = *Off;
       When inmode = 'A';
          dspVisb = *On;
       When inmode = 'C';
          dspVisb = *On;
       Endsl;

       // Load Action panel with the Company Name.
       intCo# = userCo#(q1user);

       // Get the client id
       GetClientID(##CltId);

       tabClick = *Off;
       ##Rrn# = 0;
       Return;

      /End-Free

     p ProgramStart    e
       //===================================================================
       // Validate - Edit/Add State Code
       //===================================================================
     p Validate        b
     d Validate        pi
      /Free

         isError = *Off;
         mgSeq#  = ' ';
         erSeq#  = *off;
         mgName  = ' ';
         erName  = *off;
         mgAddr1 = ' ';
         erAddr1 = *off;
         mgCity  = ' ';
         erCity  = *off;
         mgSt    = ' ';
         erSt    = *off;
         mgZip   = ' ';
         erZip   = *off;

         Select;
         When scSeq# = *Zeros;
            mgSeq# = 'Contractor number is required';
            erSeq# = *on;
            isError = *On;
         When inProcMod = 'A' or
          (inProcMod = 'C' and scSeq# <> *zero and scSeq# <> svSeq#);
            Chain (xxCo#:xxFnd:xxFyr:xxEmp#:xxDiv:xxForm:scSeq#) pl400;
            If %found;
               mgSeq# = 'Contractor number already exist';
               erSeq# = *on;
               isError = *On;
            Endif;
         Endsl;

         If scName = *Blank;
            mgName = 'Contractor Name is required';
            erName = *on;
            isError = *On;
         Endif;

         If scAddr1 = *Blank;
            mgAddr1 = 'Address is required';
            erAddr1 = *on;
            isError = *On;
         Endif;

         If scCity = *Blank;
            mgCity = 'City is required';
            erCity = *on;
            isError = *On;
         Endif;

         If scSt = *Blank;
            mgSt = 'State is required';
            erSt = *on;
            isError = *On;
         Endif;

         If scZip = *Zeros;
            mgZip = 'Zip Code is required';
            erZip = *on;
            isError = *On;
         Endif;

         If isError = *on;
            isValid = *on;
         Endif;

      /End-Free

     p Validate        e
       //===================================================================
      * Utility policy procedure definitions
      /COPY SCOPYPOLPI
