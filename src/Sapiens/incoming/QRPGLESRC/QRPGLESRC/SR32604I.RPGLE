     H Copyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio: *srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
     H/TITLE SR32604I - ADDITIONAL ENTRY PROGRAM FOR WC320604
      *===================================================================*
      *  DESCRIPTION: Collects additional information for form WC320604   *
      *                                                                   *
      *  CALLED FROM: SR32604                                             *
      *                                                                   *
      *  CALLS TO:    none                                                *
      *                                                                   *
      *  CREATED: 12/09/20 C:180  PROJECT: 148WCE                         *
      *===================================================================*
     FSR32604F  cf   e             WORKSTN HANDLER('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflEndr:RrnEndr)
     fWtactpnl  cf   e             WORKSTN
     f                                     HANDLER('PROFOUNDUI(HANDLER)')
     f                                     include(RcdPolPnl)
     f                                     include(rcdQtePnl)
     f                                     include(rcdComPnl)

     fWledl     if   e           k disk    rename(ped01:ped00)
     f                                     Infds(dbfds1) prefix(x)
     fWledp     uf a e             disk    rename(ped01:ped0p)
     fwmfnl     if   e           k Disk
     fwmeml     if   e           k Disk
     fWdell     if   e           k disk    infds(dbfds)
      *
      * Procedure Prototypes
      *

      * Entry of Program.

     d SR32604I        pr
     d                               40
     d                                1
     d                                3
     d                                3
     d                                9
     d                                5
     d                                3

     d SR32604I        pi
     d inprds                        40
     d incall                         1
     d inco#                          3
     d infnd                          3
     d inemp#                         9
     d indiv                          5
     d infyr                          3

      * Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d                                3

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

      *
      * Data Structures
      *
     d psds          esds                  extname(szq1p)

     d dbfds           ds
     d  dbrrn                397    400b 0
      *
     d dbfds1          ds
     d  dbrrn1               397    400b 0

      *
      * Stand Alone
      *
     d @@TabIndex      s              5
     d xxCo#           s                   like(EMco#)
     d xxFnd           s                   like(EMfnd)
     d xxEmp#          s                   like(EMemp#)
     d xxDiv           s                   like(EMdiv)
     d xxFyr           s              3  0
     d xxFrm1          s             15
     d scfnd           s                   like(ELfnd)
     d scfyr           s                   like(ELfyr)
     d scemp#          s                   like(ELemp#)
     d scdiv           s                   like(ELdiv)
     d clientID        s              3    inz('000')
     d InMode          s              1
     d inProcMod       s              1
     d intCo#          s              3  0
     d intRrn          s              9  0
     d xxRrn           s              9  0
     d ##CltId         s              3
     d isMenuAvbl      s               n   inz(*on)
     d isValid         s              1n   inz(*on)
     d ##Rrn#          s              9  0
     d MaxSflRrn       s              5  0  Inz

      *
      * Constants
      *
     d xxForm          c                   Const('WC320604  ')
    
      /Free

       // Start Of Program
       ProgramStart();

       // Main Procesing
       Main();

       *InLr = *on;
      /End-Free
       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi
      /Free
       xxCo#  =  %Int(inCo#);
       xxFnd  =  %Int(inFnd);
       xxEmp# =  %Int(inEmp#);
       xxDiv  =  %Int(inDiv);
       xxFyr  =  %Int(inFyr);
       xxFrm1= xxForm;

       // Read the Employer Detail File
       Chain (xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr) pel01;

       xxRRN = dbRRN;
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
          When tabClick = *on;
             tabClick = *off;
          When BtnRefresh = *On;
             BtnRefresh =*Off;
          When BtnAdd = *on;
             ProcEdtDelAdd();
             BtnAdd = *Off;
          When BtnCancel = *on;
             Leave;
          When Save = *On;
             Validate('D');
             If isValid = *On;
                SaveNoticeDays();
             Endif;
             Save = *Off;
          Other;
             If InMode = 'C';
                ProcOther();
             Endif;
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
          Write RcdPolPnl;
          Write RcdPanel;
          Exfmt RcdEndr;
          Read RcdPolPnl;
          Read RcdPanel;
       EndSl;
       Endsr;
     p Main            e
       //===================================================================
       // ClrSubFiles - Clears SubFiles
       //===================================================================
     p ClrSubFiles     b
     d ClrSubFiles     pi

      /Free

       RrnEndr = 0;
       clrEndr = *On;
       dspEndrSub  = *Off;
       dspEndrCtl  = *Off;
       MaxSflRrn = *Zero;
       Write rcdEndr;

      /End-Free

     p ClrSubfiles     e
       //===================================================================
       // Load Endorse Value Detail Grid
       //===================================================================
     p LoadEndrGrid    b
     d LoadEndrGrid    pi

      /Free

       // Clear the subfile.
       Clear sflEndr;
       clrEndr = *Off;

       If InMode = 'D';
         dspVisb = *Off;
         inqMode = *On;
         scInMode = '1';
         DisSav1 = *on;
       Else;
         dspVisb = *On;
         inqMode = *Off;
         scInMode = '0';
         DisSav1 = *Off;
       Endif;

       dspEndrSub = *On;
       dspEndrCtl = *On;
       clear dbrrn1;
       scGovEname = *Blanks;

       xxFrm1 =  xxForm;

       Setll (xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr:xxFrm1) ped00;
       Reade (xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr:xxFrm1) ped00;

       RrnEndr = *Zero;

       Dow Not %Eof(wledl);
          scDaysNtc  = xFMa701;                   //Notice Days
          scGovEname = %Trim(xFM2001);            //Gov entity name
          scEndrRrn# = dbrrn1;
          RrnEndr = RrnEndr + 1;
          Write sflEndr;
          Reade (xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr:xxFrm1) ped00;
       Enddo;

       MaxSflRrn = RrnEndr;
      /End-Free

     p LoadEndrGrid    e
       //===================================================================
       // ProcOther - Process Other Options
       //===================================================================
     p ProcOther       b
     d ProcOther       pi

      /Free
       // Validate Notice Days
       Validate('D');                  //Validate Days

       // Edit Endorse Record
       If isValid = *On;
          ReadC sflEndr;

          Dow Not %Eof;
             If scEndrEdt  = *On;
                inProcMod = 'C';
                ##Rrn# = scEndrRRN#;
                ProcEdtDelAdd(##Rrn#);
                scEndrEdt  = *Off;
             Endif;

             If scEndrDel = *On;
                scEndrDel = *Off;
                inProcMod = 'D';
                ##Rrn#  = scEndrRRN#;
                ProcEdtDelAdd(##Rrn#);
                scEndrDel  = *Off;
             Endif;
             ReadC sflEndr;
          Enddo;
       Endif;

      /End-Free
     p ProcOther       e
       //===================================================================
       // ProcEdtDelAdd - Edit/Del/Add Endorse Record
       //===================================================================
     p ProcEdtDelAdd   b
     d ProcEdtDelAdd   pi
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
       Clear RcdAddInfo;

       BtnCancel = *off;
       leftCan = 550;
       BtnSave  = *off;
       BtnAddNext = *off;
       VisAddNext = *Off;
       DisAddNext = *On;
       scGovEntNm = *Blanks;

       Select;
       When inProcMod = 'A' ;                           //Add
          VisAddNext = *On;
          DisAddNext = *Off;
          Validate('D');
          If isValid = *Off;
             Return;
          Endif;

       When inProcMod = 'C' and intRrn <> 0;            //Edit
          LeftCan = 660;

          Chain(n) intRrn ped0p;
          If %Found(wledp);
             scGovEntNm= FM2001;
          Endif;

       When inProcMod = 'D' and intRrn <> 0;            //Delete
          Chain intRrn ped0p;
          If %Found(wledp);
             Delete ped0p;
             Return;
          Endif;
       Endsl;

       Dou (BtnCancel  = *On) or (BtnSave = *on and isValid = *on);

          Exfmt RcdAddInfo;

          Select;
          When BtnSave =  *on or BtnAddNext = *on;
             Validate('E') ;                     //Validate Entity
             If isValid = *on;
                 Exsr UpdAddEntity;
             Endif;
             BtnAddNext = *Off;
          When BtnCancel = *on;
             BtnCancel = *off;
             Leave;
          Endsl;
       Enddo;

       intRrn  = *zeros;
       Return;
       //====================================================================
       // UpdAddEntity - Update/Add Government Entity
       //====================================================================
       Begsr UpdAddEntity;
       If inProcMod = 'A';
          Clear Ped0p;
       Else;
          Chain intRrn ped0p;
       Endif;

       FMa701 = scDaysNtc;
       FM2001 = scGovEntNm;

       FMpgm  = q1pgm;
       FMuser = q1user;
       FMdt   = DateTo7(%Date);
       FMtime = %Dec(%Time);

       // Move the additional information fields to the file
       If intRrn > *Zero;
          If %found (wledp);
             Update ped0p;
          Endif;
       Else;
          FMipgm = q1pgm;
          FMiusr = q1user;
          FMidt  = DateTo7(%Date);
          FMco#  = xxCo#;
          FMfnd  = xxFnd;
          FMfyr  = xxFyr;
          FMemp# = xxEmp#;
          FMdiv  = xxDiv;
          FMform = xxForm;
          Write ped0p;
       Endif;

       If inProcMod = 'A';
          scGovEntNm = *blanks;
       Endif;
       Endsr;

      /End-Free

     p ProcEdtDelAdd   e
       //===================================================================
       // SaveNoticeDays - Save Notice Days
       //===================================================================
     p SaveNoticeDays  b
     d SaveNoticeDays  pi
     d MaxRrn          s              5  0  Inz
      /Free
       MaxRrn = 1;
       Dow MaxRrn <= MaxSflRrn;
          chain MaxRrn sflEndr;
          If %found;
             intRrn = scEndrRRN#;
             Chain intRrn ped0p;
             If %found (wledp);
                FMa701 = scDaysNtc;
                FMpgm  = q1pgm;
                FMuser = q1user;
                FMdt   = DateTo7(%Date);
                FMtime = %Dec(%Time);
                Update ped0p;
             Endif;
          Endif;
          MaxRrn = MaxRrn + 1;
       Enddo;
      /End-Free
     p SaveNoticeDays  e
       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi
      /Free

       // Program Mode.
       If q1prms >= 2 and InCall = '1';
          InMode = 'D';
          dspVisb = *Off;
          inqMode = *On;
          scInMode = '1';
          DisSav1 = *on;
       Else;
          InMode = 'C';
          dspVisB = *On;
          inqMode = *Off;
          scInMode = '0';
          DisSav1 = *Off;
       Endif;

       // Load Action panel with the Company Name.
       intCo# = userCo#(q1user);

       // Get the client id
       GetClientID(##CltId);

       tabClick = *Off;
       ##Rrn# = 0;
       scDaysNtc = *zeros;
       Return;

      /End-Free
     p ProgramStart    e
       //===================================================================
       // Validate - Edit/Add State Code
       //===================================================================
     p Validate        b
     d Validate        pi
     d inscr                          1A   Const
      /Free
       isValid = *on;
       mgDaysNtc = *blanks;
       erDaysNtc = *off;
       mgGovEntNm = *blanks;
       erGovEntNm = *off;
       mgGovEname = *blanks;
       erGovEname = *off;

       If scDaysNtc = *zeros;
          mgDaysNtc = 'Number of days Notice must be greater than zero.';
          erDaysNtc = *on;
          isValid = *off;
          return;
       Endif;

       Select;
       When inScr = 'D';
          If scDaysNtc > *zeros and (RrnEndr = *Zero and BtnSave = *On);
                mgGovEName = 'Government entity name is required.';
                erGovEName = *on;
                isValid = *off;
          Endif;
       When inScr = 'E';
          If scGovEntNm = *Blanks;
             mgGovEntNm = 'Government entity name is required.';
             erGovEntNm = *on;
             isValid = *off;
          Endif;
       Endsl;

      /End-Free
     p Validate        e
       //===================================================================
      * Utility policy procedure definitions
      /COPY SCOPYPOLPI
