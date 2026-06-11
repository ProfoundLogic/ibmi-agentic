     hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h*****************************************************************
     h/title SR04339BI- Designated Operations Coverage
     h/define   PROFOUNDUI
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description:  Collects additional information for form WC040339B
     h*
     h*  Called from: SR04339B
     h*  Calls to:    None
     h*
     h* Created   : 04/28/18
     h* Programmer: C:33
     h* Project   : WC2603
     h**********************************************************************
     H*                                                               *
     H*  THIS PROGRAM COLLECTS CUSTOM DATA FOR ENDORSEMENTS AND       *
     H*  STORES IT IN FILE WLEDP, USING GENERIC DATA FIELDS.          *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     fSR04339BF cf   e             workstn extfile('SR04339BF ')
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflEndr:rrnEndr)
     fWtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdPolPnl)
     f                                     include(rcdQtePnl)
     f                                     include(rcdComPnl)

     fWmeml     if   e           k disk
     fWdell     if   e           k disk
     fWledl     if   e           k disk    rename(ped01:ped00)
     f                                     Infds(dbfds1) prefix(x)
     fWledp     uf a e             disk    rename(ped01:ped0p)

     d psds          esds                  extname(szq1p)

     d dbfds1          ds
     d  dbrrn1               397    400b 0
     d                 ds
     d intZip9                 1      9  0
     d  intZip5                1      5  0
     d  intZip4                6      9  0

     d @@prds          s             40
     d @@Mode          s              1
     d @@Co#           s              3
     d @@Fnd           s              3
     d @@Emp#          s              9
     d @@Div           s              5
     d @@Fyr           s              3
     d @@TabIndex      s              5
     d xxCo#           s                   like(FMco#)
     d xxFnd           s                   like(FMfnd)
     d xxEmp#          s                   like(FMemp#)
     d xxDiv           s                   like(FMdiv)
     d xxFyr           s                   like(FMfyr)
     d xxFrm1          s                   like(FMform)
     d scfnd           s                   like(ELfnd)
     d scfyr           s                   like(ELfyr)
     d scemp#          s                   like(ELemp#)
     d scdiv           s                   like(ELdiv)
     d svLocNoA        s                   like(FMSeq#)
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
     d xxSeq#          s                   like(FMSeq#)
     d xxForm          c                   Const('WC040339B ')
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
      *
     c     keyWledl      Klist
     c                   Kfld                    xxCo#
     c                   Kfld                    xxFnd
     c                   Kfld                    xxEmp#
     c                   Kfld                    xxDiv
     c                   Kfld                    xxFyr
     c                   Kfld                    xxfrm1
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

       FMco#  =  xxcO#;
       FMfnd  =  xxFND;
       FMfyr  =  xxFYR;
       FMemp# =  xxEMP#;
       FMdiv  =  xxDIV;
       FMform =  xxForm;

       //Location/Operation name/Title
       FMSeq# =  scLocNoA;
       FMc401 =  scOprNmA;
       FMc402 =  scClassNoA;
       FMc406 =  scAbbrvA;

       //Alternate coverage information
       FMc404 =  scInsdName;
       FMc405 =  scInsurer;
       FMc301 =  scPol#;
       FMDt01 =  scEffDt;
       FMDt02 =  scExpDt;

       //Affirming information
       FMFL01 = scAffirWrt;
       FMFL02 = scLiabUnis;

       FMpgm  = q1pgm;
       FMuser = q1user;
       FMdt   = DateTo7(SystemDate);
       FMtime = TimeTo6(SystemTime);

        If inProcMod = 'A';
           FMipgm = q1pgm;
           FMiusr = q1user;
           FMidt  = DateTo7(SystemDate);
           Write ped0p;
        Else;
           Update ped0p;
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
       Chain keyWdell pel01;

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

       xxFrm1 =  xxForm;

       Setll keyWledl ped00;
       Reade keyWledl ped00;
       Dow Not %Eof(wledl);

          // Location Number
          scLocNo = xFMSeq#;

          // Operation Name
          scOprNm = %Trim(xFMc401);

          // Standard Classification Number
          scClassNo = %Trim(xFMc402);

          // Abbreviated Phraseology
          scAbbrv = %Trim(xFMc406);

          // Location Address
          //scLocAddr = %Trim(xFMC403) + ' ' +
          //            %Trim(xFMC201)  + ',' + ' ' + %Trim(xFMC101) +
          //            ' ' + %Trim(xFMC102)  + ' ' + '-' + ' ' + xFMC103;


          scEndrRrn# = dbrrn1;
          rrnEndr = rrnEndr + 1;

          Write sflEndr;
          Reade keyWledl ped00;

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
          Chain intRrn ped0p;
          If %Found(wledp);
             Delete ped0p;
             Return;
          Endif;
       Endif;

       // Processing to Edit Endorse Record
       If inProcMod = 'C' and intRrn <> 0;
          Chain intRrn ped0p;
          If %Found(wledp);
             scLocNoA  = FMSeq#;
             svLocNoA  = FMSeq#;
             scOprNmA  = FMc401;
             scClassNoA = FMc402;
             scAbbrvA  = FMc406;
             //scCty     = FMc201;
             //scSt      = FMc101;
             //intZip5   = %Int(FMc102);
             //intZip4   = %Int(FMc103);
             //scZip     = intZip9;
             scInsdName= FMc404;
             scInsurer = FMc405;
             scPol#    = FMc301;
             scEffDt   = FMDt01;
             scExpDt   = FMDt02;
             scAffirWrt= FMFL01;
             scLiabUnis= FMFL02;
          Endif;

          // Intialize the left alignment of Cancel Button.
          scCanLft = 680;

          // Save/Add Next Button will not get displyed.
          indSvAdNx = *Off;

       Endif;

       Dou (BtnCanRcd  = *On) or (BtnAddRcd = *on and isValid = *off)
            or (BtnAdNxRcd  = *Off and isvalid = *off);
          // Processing to Add Endorse Record
          If inProcMod = 'A' and isValid = *Off;

             //Get Next Location number
             xxFrm1 =  xxForm;
             xxseq# =  *hival;
             Setgt  (xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr:xxfrm1:xxSeq#) ped00;
             Readpe (xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr:xxfrm1) ped00;
             If Not %Eof(Wledl);
                eval scLocNoA = xFMseq# + 1;
             Else;
                eval scLocNoA = 1;
             Endif;

             scOprNmA  = *Blanks;
             scClassNoA = *Blanks;
             scAbbrvA  = *Blanks;
             //scAddress = *Blanks;
             //scCty     = *Blanks;
             //scSt      = *Blanks;
             //scZip     = *Zeros;
             scInsdName= *Blanks;
             scInsurer = *Blanks;
             scPol#    = *Blanks;
             scEffDt   = *Zeros;
             scExpDt   = *Zeros;
             scAffirWrt= *Blanks;
             scLiabUnis= *Blanks;

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
         mgLocNoA = ' ';
         erLocNoA = *off;
         mgOprNmA = ' ';
         erOprNmA = *off;
         mgClassNoA = ' ';
         erClassNoA = *off;
         mgAbbrvA = ' ';
         erAbbrvA = *off;
         //mgAddress= ' ';
         //erAddress= *off;
         //mgCty    = ' ';
         //erCty    = *off;
         //mgST     = ' ';
         //erST     = *off;
         //mgZip    = ' ';
         //erZip    = *off;
         mgInsdName = ' ';
         erInsdName = *off;
         mgInsurer = ' ';
         erInsurer = *off;
         mgPol# = ' ';
         erPol# = *off;
         mgEffDt = ' ';
         erEffdt = *off;
         mgExpDt = ' ';
         erExpdt = *off;

         If scLocNoA   = *Zeros;
            mgLocNoA   = 'Location number is required';
            erLocNoA   = *on;
            isError = *On;
         Else;
          If inProcMod = 'A' or (inProcMod = 'C' and scLocNoA <> *Zero and
                                 scLocNoA <> svLocNoA);
            Setll (xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr:xxfrm1:scLocNoA) ped00;
            Reade (xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr:xxfrm1:scLocNoA) ped00;
              If Not %Eof(Wledl);
                mgLocNoA   = 'Location number already exist';
                erLocNoA   = *on;
                isError = *On;
              Endif;
          Endif;
         Endif;

         If scOprNmA   = *Blank;
            mgOprNmA   = 'Operation Name is required';
            erOprNmA   = *on;
            isError = *On;
         Endif;

         If scClassNoA   = *Blank;
            mgClassNoA   = 'Standard Classification Number is required';
            erClassNoA   = *on;
            isError = *On;
         Endif;

         If scAbbrvA   = *Blank;
            mgAbbrvA   = 'Abbreviated Phraseology is required';
            erAbbrvA   = *on;
            isError = *On;
         Endif;

         //If scAddress  = *Blank;
         //   mgAddress  = 'Address is required';
         //   erAddress  = *on;
         //   isError = *On;
         //Endif;

         //If scCty  = *Blank;
         //   mgCty  = 'City is required';
         //   erCty  = *on;
         //   isError = *On;
         //Endif;

         //If scST  = *Blank;
         //   mgST  = 'State is required';
         //   erST  = *on;
         //   isError = *On;
         //Endif;

         //If scZip = *Zeros;
         //   mgZip = 'Zip Code is required';
         //   erZip = *on;
         //   isError = *On;
         //Endif;

         If scInsdName <> *Blank or scInsurer <> *Blank or
            scPol# <> *Blank or scEffdt <> *Zero or scExpdt <> *Zero;

            If scInsdName = *Blank;
               mgInsdName = 'Insured name is required';
               erInsdName = *on;
               isError = *On;
            Endif;
            If scInsurer = *Blank;
               mgInsurer = 'Insurer name is required';
               erInsurer = *on;
               isError = *On;
            Endif;
            If scPol# = *Blanks;
               mgPol# = 'Policy # is required';
               erPol# = *on;
               isError = *On;
            Endif;
            If scEffDt = *Zeros;
               mgEffDt = 'Effective Date is required';
               erEffDt = *on;
               isError = *On;
            Endif;
            If scExpDt = *Zeros;
               mgExpDt = 'Expiration Date is required';
               erExpDt = *on;
               isError = *On;
            Endif;

         Endif;

         If isError = *on;
            isValid = *on;
         Endif;

      /End-Free

     p Validate        e
       //===================================================================
      * Utility policy procedure definitions
      /COPY SCOPYPOLPI
