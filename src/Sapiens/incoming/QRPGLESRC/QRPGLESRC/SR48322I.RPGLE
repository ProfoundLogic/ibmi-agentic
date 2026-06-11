     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     h*****************************************************************
     h/title SR48322I - WI Emp Leasing Inclusion Add Info Program
     h/define   PROFOUNDUI
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h***********************************************************************
     h*
     h* Description: Collects additional information for Alternate Employer
     h*              Endorsements
     h*
     h* Called from: SR48322
     h* Calls to:    None
     h*
     h* Created   : 08/05/21
     h* Programmer: C:079
     h* Project   : WC2814
     h**********************************************************************
     h*                                                               *
     h*  This program will display a list of all Labor Contractor     *
     h*  for a selected employer policy year. Each Labor Contractor   *
     h*  can be accessed for update, display or delete.               *
     h*                                                               *
     h*****************************************************************
     h*  REVISIONS:                                                   *
     h*                                                               *
     h*                                                               *
     h*****************************************************************
     fSR48322F  cf   e             workstn extfile('SR48322F  ')
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflEndr:rrnEndr)
     fWtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdPolPnl)
     f                                     include(rcdQtePnl)
     f                                     include(rcdComPnl)

     fWmeml     if   e           k disk
     fWdell     if   e           k disk    rename(pel01:pel00)
     fWxfml11   if   e           k disk    rename(pfm01:pfm11)
     f                                     Infds(dbfds1)
     fWxfmp1    uf a e             disk    rename(pfm01:pfm0p)
     FWdf2l     if   e           k disk    rename(pf201:pf200)
     FWmfnl     if   e           k disk    rename(pfn01:pfn00)

     d psds          esds                  extname(szq1p)

     d dbfds1          ds
     d  dbrrn1               397    400b 0
     d                 ds
     d @@prds          s             40
     d @@Mode          s              1
     d @@Mode2         s              1
     d @@Co#           s              3
     d @@Fnd           s              3
     d @@Emp#          s              9
     d @@Div           s              5
     d @@Fyr           s              3
     d @@Form          s             15    inz('WC480322 ')
     d xxCo#           s                   like(F3co#)
     d xxFnd           s                   like(F3fnd)
     d xxEmp#          s                   like(F3emp#)
     d xxDiv           s                   like(F3div)
     d xxFyr           s                   like(F3fyr)
     d xxForm          s                   like(F3form)
     d scfnd           s                   like(F3fnd)
     d scfyr           s                   like(F3fyr)
     d scemp#          s                   like(F3emp#)
     d scdiv           s                   like(F3div)
     d clientID        s              3    inz('000')
     d inmode          s              1
     d inProcMod       s              1
     d inTabIndex      s              5  0
     d intCo#          s              3  0
     d intRrn          s              9  0
     d xxRrn           s              9  0
     d ##Rrn#          s              9  0
     d @@Rrn           s              9  0
     d ##CltId         s              3
     d isMenuAvbl      s               n   inz(*on)
     d isValid         s              1n   inz(*off)
     d isError         s              1n   inz(*off)
     d I               s              3  0
     d svLbrCon        s                   like(F3500A1)
     d svCarPol#       s                   like(F3cPl#)
     d svFein          s                   like(F3fein)
     d sveAd1          s                   like(F3eAd1)
     d sveAd2          s                   like(F3eAd2)
     d sveAd3          s                   like(F3eAd3)
     d svCity          s                   like(EMmCty)
     d svState         s                   like(EMmSt)
     d svZipCd         s                   like(EMmZip)
     d xxDelResp       s               n   inz
     d chrZip          s             12
      ******************************************AEEmst*****************
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
     d/copy wprGenPop
     d/copy wPrRateSet
     d/copy wPrEmpLr
     d/copy wPrCarrier
     d/copy wPrUsrPrms
    
      * Entry of Program.
     c     *Entry        Plist
     c                   Parm                    @@Co#
     c                   Parm                    @@Fnd
     c                   Parm                    @@Emp#
     c                   Parm                    @@Div
     c                   Parm                    @@Fyr
     c                   Parm                    @@Mode
     c                   Parm                    @@Rrn
     c                   Parm                    @@Mode2
      *
     c     keyWxfml11    Klist
     c                   Kfld                    xxCo#
     c                   Kfld                    xxFnd
     c                   Kfld                    xxEmp#
     c                   Kfld                    xxDiv
     c                   Kfld                    xxFyr
     c                   Kfld                    xxform
      *
     c     keyWdell      Klist
     c                   Kfld                    xxCo#
     c                   Kfld                    xxFnd
     c                   Kfld                    xxEmp#
     c                   Kfld                    xxDiv
     c                   Kfld                    xxFyr

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

       F3co#  =  xxcO#;
       F3fnd  =  xxFND;
       F3fyr  =  xxFYR;
       F3emp# =  xxEMP#;
       F3div  =  xxDIV;
       F3form =  xxForm;

       //Contractor Name/Address/City/State/Zip
       F3500A1 = scLbrCon;
       F3FEIN  = scFein;
       F3cPl#  = scCarPol#;
       F3eAd1 = scAddr1;
       F3eAd2 = scAddr2;
       F3c301 = scCity;
       F3emSt = scState;
       F3am01 = scZipCd;
       If F3am01 <> 0;
          chrZip = %Char(F3am01*100);
          chrZip = %Subst(chrZip:1:5) + '-' + %Subst(chrZip:6:4);
       Else;
          chrZip = *blanks;
       Endif;
       Select;
       When F3c301 <> *blanks and F3eMst <> *blanks and chrZip <> *blanks;
          F3eAd3 = %Trim(F3c301) + ', ' + %Trim(F3eMst) + ' ' + %Trim(chrZip);
       When F3c301 <> *blanks and F3eMst <> *blanks and chrZip = *blanks;
          F3eAd3 = %Trim(F3c301) + ', ' + %Trim(F3eMst);
       When F3c301 <> *blanks and F3eMst = *blanks and chrZip = *blanks;
          F3eAd3 = %Trim(F3c301);
       When F3c301 <> *blanks and F3eMst = *blanks and chrZip <> *blanks;
          F3eAd3 = %Trim(F3c301) + %Trim(chrZip);
       When F3c301 = *blanks and F3eMst <> *blanks and chrZip <> *blanks;
          F3eAd3 = %Trim(F3eMst) + %Trim(chrZip);
       When F3c301 = *blanks and F3eMst = *blanks and chrZip <> *blanks;
          F3eAd3 = %Trim(chrZip);
       When F3c301 = *blanks and F3eMst <> *blanks and chrZip = *blanks;
          F3eAd3 = %Trim(F3eMst);
       Endsl;

       F3pgm  = q1pgm;
       F3user = q1user;
       F3dt   = DateTo7(SystemDate);
       F3time = TimeTo6(SystemTime);
       F3stat = 'A';

        If inProcMod = 'A';
           F3ipgm = q1pgm;
           F3iusr = q1user;
           F3idt  = DateTo7(SystemDate);
           Write pfm0p;
        Else;
           Update pfm0p;
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
       xxForm =  @@Form;

       Chain (xxco#:xxFnd) pf200;
       Chain (xxco#:xxFnd) pfn00;

       // Read the Employer Detail File
       Chain keyWdell pel00;

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
       Clear dbrrn1;

       Setll keyWxfml11 pfm11;
       Reade keyWxfml11 pfm11;
       Dow Not %Eof(wxfml11);

          // Labor Contractor Name
          scLbrCon = %Trim(F3500A1);

          // Carrier Policy Number
          scCarPol# = %Trim(F3cPl#);

          // FEIN
          scFein = F3fein;

          // Address

          If F3am01 <> 0;
              chrZip = %Char(F3am01*100);
              chrZip = %Subst(chrZip:1:5) + '-' + %Subst(chrZip:6:4);
          Else;
             chrZip = *blanks;
          Endif;
          scAddress = *blanks;
          Select;
          When F3eAd1 <> *Blanks and F3eAd2 <> *Blanks;
             scAddress = %Trim(F3eAd1) + ', ' + %Trim(F3eAd2) + ', ' +
              %Trim(F3eAd3);
          When F3eAd1 <> *blanks and F3eAd2 = *Blanks;
             scAddress = %Trim(F3eAd1) + ', ' + %Trim(F3eAd3);
          When F3eAd1 = *Blanks and F3eAd2 <> *Blanks;
             scAddress = %Trim(F3eAd2) + ', ' +
              %Trim(F3eAd3);
          When F3eAd1 = *Blanks and F3eAd2 = *Blanks;
             scAddress = %Trim(F3eAd3);
          Endsl;

          If inMode = 'D';
             dspLnk = *Off;
          Else;
            dspLnk = *On;
          Endif;

          scEndrRrn# = dbrrn1;
          rrnEndr = rrnEndr + 1;

          Write sflEndr;
          Reade keyWxfml11 pfm11;

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

       Clear svLbrCon;
       Clear svCarPol#;
       Clear svFein;
       Clear sveAd1;
       Clear sveAd2;
       Clear sveAd3;
       Clear svCity;
       Clear svState;
       Clear svZipCd;

       // Process to Delete Endorse Record
       If inProcMod = 'D' and intRrn <> 0;
          Chain intRrn pfm0p;
          If %Found(wxfmp1);
             svLbrCon  = F3500A1;
             svCarPol# = F3cPl#;
             svFein    = F3fein;
             sveAd1    = F3eAd1;
             sveAd2    = F3eAd2;
             sveAd3    = F3eAd3;
             svCity    = F3C301;
             svState   = F3emSt;
             svZipCd   = F3am01;
             Clear svLbrCon;
             Clear svCarPol#;
             Clear svFein;
             Clear sveAd1;
             Clear sveAd2;
             Clear sveAd3;
             Clear svCity;
             Clear svState;
             Clear svZipCd;

             xxDelresp = PopTwoBtn('Do you want to delete this Labor +
              Contractor?':'Confirm Delete':'Yes':'No':0:200);
             Select;
             When xxDelResp = *On;
                Delete pfm0p;
                PopOneBtn('Labor Contractor deleted!':'Delete':'Ok':
                 0:200);
                Return;
             When xxDelResp = *Off;
                Return;
             EndSl;

          Endif;
       Endif;

       // Processing to Edit Endorse Record
       If inProcMod = 'C' and intRrn <> 0;
          Chain intRrn pfm0p;
          If %Found(wxfmp1);
             scLbrCon  = F3500A1;
             scCarPol# = F3cPl#;
             scFein    = F3fein;
             scAddr1   = F3eAd1;
             scAddr2   = F3eAd2;
             scCity    = F3C301;
             scState   = F3emSt;
             scZipCd   = F3am01;

             svLbrCon  = F3500A1;
             svCarPol# = F3cPl#;
             svFein    = F3fein;
             svEad1    = F3eAd1;
             svEad2    = F3eAd2;
             svCity    = F3C301;
             svState   = F3emSt;
             svZipCd   = F3am01;

          Endif;

          // Intialize the left alignment of Cancel Button.
          scCanLft = 640;

          // Save/Add Next Button will not get displyed.
          indSvAdNx = *Off;

       Endif;

       Dou (BtnCanRcd  = *On) or (BtnAddRcd = *on and isValid = *off)
            or (BtnAdNxRcd  = *Off and isvalid = *off);
          // Processing to Add Endorse Record
          If inProcMod = 'A' and isValid = *Off;


             scLbrCon  = *Blanks;
             scCarPol# = *Blanks;
             scFein    = *Zeros;
             scAddr1   = *Blanks;
             scAddr2   = *Blanks;
             scCity    = *Blanks;
             scState   = *Blanks;
             scZipCd   = *Zeros;

             // Intialize the left alignment of Cancel Button.
             scCanLft = 535;

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
       // Validate - Edit/Add
       //===================================================================
     p Validate        b
     d Validate        pi
      /Free

       isError = *Off;
       mgLbrCon   = ' ';
       erLbrCon   = *off;

       If scLbrCon = *Blanks;
          mgLbrCon   = 'Labor Contractor is required';
          erLbrCon   = *on;
          isError    = *On;
       Endif;

       If isError = *on;
          isValid = *on;
       Endif;

      /End-Free

     p Validate        e
       //===================================================================
      * Utility policy procedure definitions
      /COPY SCOPYPOLPI
