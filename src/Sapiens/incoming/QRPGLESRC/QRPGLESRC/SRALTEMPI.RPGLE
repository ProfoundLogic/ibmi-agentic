     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     h*****************************************************************
     h/title SRALTEMPI- Alternate Employer Endorsement
     h/define   PROFOUNDUI
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h***********************************************************************
     h*
     h* Description: Collects additional information for Alternate Employer
     h*              Endorsements
     h*
     h* Called from: SRALTEMP
     h* Calls to:    None
     h*
     h* Created   : 10/15/18
     h* Programmer: C:291
     h* Project   : WC2549
     h**********************************************************************
     H*                                                               *
     H*  This program will display a list of all alternate employers  *
     H*  for a selected employer policy year. Each alternate employer *
     H*  can be accessed for update, display or delete.               *
     H*                                                               *
      *****************************************************************
      *  REVISIONS:                                                   *
      *                                                               *
      * 02/14/20 C:136 Support: 80310                                 *
      *!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*
      *!!! ALL THE OBJECTS FOR THIS SUPPORT ITEM MUST GO TOGETHER  !!!*
      *!!! AND FIX80310 MUST BE RUN.                               !!!*
      *!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*
      *                Move form name to different position in LDA    *
      *                to not conflict with Scriptura processing.     *
      *                                                               *
     H*****************************************************************
     fSRALTEMPF cf   e             workstn extfile('SRALTEMPF ')
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflEndr:rrnEndr)
     fWtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdPolPnl)
     f                                     include(rcdQtePnl)
     f                                     include(rcdComPnl)

     fWmeml     if   e           k disk
     fWdell     if   e           k disk    rename(pel01:pel00)
     fWdael4    if   e           k disk    rename(pae01:pae04)
     f                                     Infds(dbfds1) prefix(x)
     fWdaep     uf a e             disk    rename(pae01:pae0p)
     fWmrsl     if   e           k disk    rename(prs01:prs00)
     FWdf2l     if   e           k disk    rename(pf201:pf200)
     FWmfnl     if   e           k disk    rename(pfn01:pfn00)
     FWmenl3    if a e           k disk    rename(pen01:pen03)
     FWtmyp     if   e             disk    rename(pmy01:pmy0p)
     FWmdxl     if   e           k disk    rename(pdx01:pdx00)

     D*  Midterm Endorsment data area
     D svLda           DS          1024    dtaara(*LDA)
     D  ldEnd#                 1      5  0
     D  ldEnda                 1      5
     D  ldMyrr                 6     14  0
     D  ldEedt                15     22  0
     D  ldFyr                 23     25  0
     D  ldPyrl                26     26
      *** beg chg ***** 02/14/20 *****************
     D***ldForm               261    275
     d  ldStSpcfFlg           40     40
     d  ldForm                41     55
      *** end chg ***** 02/14/20 *****************

     D xxBfLn          s             50    Dim(5)                               Before line
     D xxAfLn          s             50    Dim(5)                               After line

     d psds          esds                  extname(szq1p)

     d dbfds1          ds
     d  dbrrn1               397    400b 0
     d                 ds
     d intZip9                 1      9  0
     d  intZip5                1      5  0
     d  intZip4                6      9  0

     d* Get Email Address for Producer
     dAgtInfo          pr                  extpgm('WA002R')
     d inCo#                          3  0 const
     d inFnd                          3  0 const
     d inEmp#                         9  0 const
     d inDiv                          5  0 const
     d inFyr                          3  0 const
     d outAgt#                        5  0
     d outANam                       60
     d*outProd                        5  0
     d*outPNam                       60

     d intAgt#         s              5  0
     d strANam         s             60
     d @@prds          s             40
     d @@Mode          s              1
     d @@Co#           s              3
     d @@Fnd           s              3
     d @@Emp#          s              9
     d @@Div           s              5
     d @@Fyr           s              3
     d @@Form          s             15    inz('WC220306 ')
     d @@TabIndex      s              5
     d xxCo#           s                   like(AEco#)
     d xxFnd           s                   like(AEfnd)
     d xxEmp#          s                   like(AEemp#)
     d xxDiv           s                   like(AEdiv)
     d xxFyr           s                   like(AEfyr)
     d xxFrm1          s                   like(AEform)
     d xxForm          s                   like(AEform)
     d scfnd           s                   like(ELfnd)
     d scfyr           s                   like(ELfyr)
     d scemp#          s                   like(ELemp#)
     d scdiv           s                   like(ELdiv)
     d svAltENoA       s                   like(AESeq#)
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
     d xxSeq#          s                   like(AESeq#)
     d kkSt            s                   like(RSJrSt)
     d intClassNo      s              4  0
     d I               s              3  0
     d svSeq#          s                   like(AESeq#)
     d svName          s                   like(AENam1)
     d svAdr1          s                   like(AEAdr1)
     d svAdr2          s                   like(AEAdr2)
     d svCty           s                   like(AECty)
     d svSt            s                   like(AESt)
     d svZip           s                   like(AEZip)
     d svProj          s                   like(AEProj)
     d svEmst          s                   like(AEEmst)
     d xxDelResp       s               n   inz
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
     c                   Parm                    @@Prds
     c                   Parm                    @@Mode
     c                   Parm                    @@Co#
     c                   Parm                    @@Fnd
     c                   Parm                    @@Emp#
     c                   Parm                    @@Div
     c                   Parm                    @@Fyr
     c***                Parm                    @@Form
      *
     c     keyWdael4     Klist
     c                   Kfld                    xxCo#
     c                   Kfld                    xxFnd
     c                   Kfld                    xxEmp#
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

       AEco#  =  xxcO#;
       AEfnd  =  xxFND;
       AEfyr  =  xxFYR;
       AEemp# =  xxEMP#;
       AEdiv  =  xxDIV;
       AEform =  xxForm;

       //Alternate Employer Name/Address
       AESeq# = scAltENoA;
       AENam1 = scAltENmA;
       AEAdr1 = scAddr1;
       AEAdr2 = scAddr2;
       AECty  = scCity;
       AESt   = scState;
       AEZip  = scZipCd;

       //Alternate Employer state
       AEEmSt =  scEmplStA;

       //Contract or Project
       AEProj =  scConOrPrj;

       AEefdt = dtCYMD(LDeedt);

       AEpgm  = q1pgm;
       AEuser = q1user;
       AEdt   = DateTo7(SystemDate);
       AEtime = TimeTo6(SystemTime);

        If inProcMod = 'A';
           AEipgm = q1pgm;
           AEiusr = q1user;
           AEidt  = DateTo7(SystemDate);
           AEitm  = TimeTo6(SystemTime);
           Write pae0p;
        Else;
           Update pae0p;
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
       //xxForm =  @@Form;
       xxForm =  ldForm;

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

       xxFrm1 =  xxForm;

       Setll keyWdael4 pae04;
       Reade keyWdael4 pae04;
       Dow Not %Eof(wdael4);

          // Alternate Employer Number
          scAltEmp# = xAESeq#;

          // Alternate Employer Name
          scAltEmpNm = %Trim(xAENam1);

          // State of Employment
          scStOfEmpl = xAEEmSt;

          // Address
          scAlEmpAdr = %Trim(xAEAdr1);
          If xAEAdr2 <> *blank;
             scAlEmpAdr = %Trim(scAlEmpAdr) + ', ' + %Trim(xAEAdr2);
          Endif;
          scAlEmpAdr = %Trim(scAlEmpAdr) + ', ' + %Trim(xAECty) + ', ' +
           %Trim(xAEst) + '  ' + %Subst(%Editc(xAEZip:'X'):1:5);
          If %Subst(%Editc(xAEZip:'X'):6:4) <> '0000';
             scAlEmpAdr = %trim(scAlEmpAdr) + '-' +
              %Subst(%Editc(xAEZip:'X'):6:4);
          Endif;

          If inMode = 'D';
             dspLnk = *Off;
          Else;
            dspLnk = *On;
          Endif;

          scEndrRrn# = dbrrn1;
          rrnEndr = rrnEndr + 1;

          Write sflEndr;
          Reade keyWdael4 pae04;

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

       Clear svSeq#;
       Clear svName;
       Clear svAdr1;
       Clear svAdr2;
       Clear svCty;
       Clear svSt;
       Clear svZip;
       Clear svProj;
       Clear svEmst;

       // Process to Delete Endorse Record
       If inProcMod = 'D' and intRrn <> 0;
          Chain intRrn pae0p;
          If %Found(wdaep);
             svSeq# = AESeq#;
             svName = AENam1;
             svAdr1 = AEAdr1;
             svAdr2 = AEAdr2;
             svCty  = AECty;
             svSt   = AESt;
             svZip  = AEZip;
             svProj = AEProj;
             svEmst = AEEmst;
             Clear AESeq#;                 //Blank out AE fields to write After fields blank for Dlt
             Clear AENam1;
             Clear AEAdr1;
             Clear AEAdr2;
             Clear AECty;
             Clear AESt;
             Clear AEZip;
             Clear AEProj;
             Clear AEEmst;

             xxDelresp = PopTwoBtn('Do you want to delete this Alternate +
              Employer?':'Confirm Delete':'Yes':'No':0:200);
             Select;
             When xxDelResp = *On;
                Exsr WrtMdTrmEnd;
                Delete pae0p;
                PopOneBtn('Alternate Employer deleted!':'Delete':'Ok':
                 0:200);
                Return;
             When xxDelResp = *Off;
                Return;
             EndSl;

          Endif;
       Endif;

       // Processing to Edit Endorse Record
       If inProcMod = 'C' and intRrn <> 0;
          Chain intRrn pae0p;
          If %Found(wdaep);
             scAltENoA = AESeq#;
             svAltENoA = AESeq#;
             scEmplStA = AEEmSt;
             scConOrPrj= AEProj;
             scAltENmA = AENam1;
             scAddr1   = AEAdr1;
             scAddr2   = AEAdr2;
             scCity    = AECty;
             scState   = AESt;
             scZipCd   = AEZip;

             svSeq#    = AESeq#;
             svName    = AENam1;
             svAdr1    = AEAdr1;
             svAdr2    = AEAdr2;
             svCty     = AECty;
             svSt      = AESt;
             svZip     = AEZip;
             svProj    = AEProj;
             svEmst    = AEEmst;
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

             //Get Next Alternate Employer number
             xxFrm1 =  xxForm;
             xxseq# =  *hival;
             Setgt  (xxCo#:xxFnd:xxEmp#:xxFyr:xxfrm1:xxSeq#) pae04;
             Readpe (xxCo#:xxFnd:xxEmp#:xxFyr:xxfrm1) pae04;
             If Not %Eof(Wdael4);
                eval scAltENoA = xAEseq# + 1;
             Else;
                eval scAltENoA = 1;
             Endif;

             scEmplStA = *Blanks;
             scConOrPrj= *Blanks;
             scAltENmA = *Blanks;
             scAddr1   = *Blanks;
             scAddr2   = *Blanks;
             scCity    = *Blanks;
             scState   = *Blanks;
             scZipCd   = *Zeros;

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
                Exsr WrtMdTrmEnd;
             Endif;
          When BtnCanRcd = *on;
             BtnCanRcd = *off;
             Leave;
          Endsl;
       Enddo;


       //Write Mid-term endorsement records
       Begsr WrtMdTrmEnd;

        If inProcMod = 'A' or inProcMod = 'D' or
           (inProcMod = 'C' and
           (AEseq# <> svSeq# or AEnam1 <> svName or
           AEAdr1 <> svAdr1 or AEAdr2 <> svAdr2 or
           AECty <> svCty or AEst <> svSt or
           AEZip <> svZip or AEProj <> svProj or
           AEEmst <> svEmst));

           Chain (xxCo#:xxFnd) pdx00;
           Chain (xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr) pel00;
           Chain (xxCo#:xxFnd:xxEmp#:xxDiv) pem01;
           Chain LDmyrr pmy0p;

           //Get Agent Info
           AgtInfo(xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr:intAgt#:strAnam);

           Setgt (EMco#:EMFnd:EMEmp#:EMDiv:LDfyr:Q1Jobn:LDEnd#) pen03;
           Readpe (EMco#:EMFnd:EMEmp#:EMDiv:LDfyr:Q1Jobn:LDEnd#) pen03;
           If %Eof(wmenl3);
              ENReq# = 0;
           Endif;

           ENReq# = ENReq# + 1;
           ENCo#  = EMCo#;
           ENFnd  = EMFnd;
           ENEmp# = EMEmp#;
           ENDiv  = EMDiv;
           ENFyr  = LDFyr;
           ENJob  = Q1Jobn;
           ENEnd# = LDEnd#;
           ENeedt = dtCYMD(LDeedt);
           ENNam1 = EMName;
           ENFdn1 = EMFdnm;
           ENENm1 = EMName + %subst(EMNam2:1:10);
           ENENm2 = %subst(EMNam2:11:40);
           ENEFn1 = EMFdNm + %Subst(EMFdN2:1:10);
           ENEFn2 = %Subst(EMFdN2:11:40);
           ENCon1 = EMCont;
           ENAtt1 = EMAttn;
           ENCn31 = EMCnt3;
           ENFen1 = EMFein;
           ENTob1 = EMTpob;
           ENMa11 = EMMAd1;
           ENMa21 = EMMAd2;
           ENMct1 = EMMCty;
           ENMst1 = EMMst;
           ENMZp1 = EMMZip;
           ENPa11 = EMPAd1;
           ENPa21 = EMPAd2;
           ENPcT1 = EMPCty;
           ENPst1 = EMPst;
           ENPZp1 = EMPZip;

           ENofi1 = svSEQ#;
           ENoen1 = svNAME;
           ENom11 = svADR1;
           ENoM21 = svADR2;
           ENomC1 = svCTY;
           ENomS1 = svST;
           ENomZ1 = svZIP;
           ENoc11 = svPROJ;

           ENofi2 = AESeq#;
           ENoen2 = AENam1;
           ENom12 = AEAdr1;
           ENom22 = AEAdr2;
           ENomC2 = AECty;
           ENomS2 = AEst;
           ENomZ2 = AEZip;
           ENoc12 = AEProj;

           Clear ENncfl;
           Clear ENaddf;
           Clear ENdldf;
           Clear ENfifl;
           Clear ENcnfl;
           Clear ENatfl;
           Clear ENc3fl;
           Clear ENtbfl;
           Clear ENridf;
           Clear ENnobf;
           Clear ENnofl;

           ENmsfl = 'A';

           ENrmcd = DXMisc;
           ENfnnm = FNName;
           ENAgt# = intAgt#;
           ENAgNm = strANam;
           ENRusr = Q1user;

           ENDsc1 = MYDsc1;
           ENDsc2 = MYDsc2;
           ENDsc3 = MYDsc3;

           //Before Line
           Clear xxBfLn;
           If inProcMod <> 'A';
              I = 1;
              xxBfLn(I) = %Editc(svSeq#:'X');
              xxBfLn(I) = %Trim(xxBfLn(I)) + ' ' + svName;
              I = I + 1;
              If svProj <> *Blank;
                 xxBfLn(I) = %Trim(svProj);
                 I = I + 1;
              Endif;
              If svAdr1 <> *Blank;
                 xxBfLn(I) = %Trim(svAdr1);
                 I = I + 1;
              Endif;
              If svAdr2 <> *Blank;
                 xxBfLn(I) = %Trim(svAdr2);
                 I = I + 1;
              Endif;
              If svCty <> *Blank or svSt <> *Blank or svZip <> *Zeros;
                 xxBfLn(I) = %Trim(svCty) + ', ' + svSt + ' ' +
                  %subst(%Editc(svZip:'X'):1:5);
                 If %subst(%Editc(svZip:'X'):6:4) <> '0000';
                    xxBfLn(I) = %Trim(xxBfLn(I)) + '-' +
                     %subst(%Editc(svZip:'X'):6:4);
                 Endif;
              Endif;
           Endif;
           //After Line
           Clear xxAfLn;
           I = 1;
           xxAfLn(I) = %Editc(AESeq#:'X');
           xxAfLn(I) = %Trim(xxAfLn(I)) + ' ' + AENam1;
           I = I + 1;
           If AEProj <> *Blank;
              xxAfLn(I) = %Trim(AEProj);
              I = I + 1;
           Endif;
           If AEAdr1 <> *Blank;
              xxAfLn(I) = %Trim(AEAdr1);
              I = I + 1;
           Endif;
           If AEAdr2 <> *Blank;
              xxAfLn(I) = %Trim(AEAdr2);
              I = I + 1;
           Endif;
           If AECty <> *Blank or AESt <> *Blank or AEZip <> *Zeros;
              xxAfLn(I) = %Trim(AECty) + ', ' + AESt + ' ' +
               %Subst(%Editc(AEZip:'X'):1:5);
              If %subst(%Editc(AEZip:'X'):6:4) <> '0000';
                 xxAfLn(I) = %Trim(xxAfLn(I)) + '-' +
                  %Subst(%Editc(AEZip:'X'):6:4);
              Endif;
           Endif;

           ENuser = Q1user;
           ENPgm  = Q1pgm;
           ENStat = 'A';
           ENRusr = Q1user;
           ENCar1 = F2Car#;
           ENPfD1 = Elpfdt;

           For I = 1 to 5;
              If xxBfLn(I) <> *Blanks or xxAfLn(I) <> *Blanks;
                 ENBlin = xxBfLn(I);
                 ENAlin = xxAfLn(I);
                 Write pen03;
                 ENtime = ENtime + 1;
                 ENSeq# = ENSeq# + 1;
              Endif;
           Endfor;

        Endif;
       Endsr;

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

       In *Lock *dtaara;
       Out *dtaara;

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
       mgAltEmp#A = ' ';
       erAltEmp#A = *off;
       mgAltENmA  = ' ';
       erAltENmA  = *off;
       mgAddr1    = ' ';
       erAddr1    = *off;
       mgAddr2    = ' ';
       erAddr2    = *off;
       mgCity     = ' ';
       erCity     = *off;
       mgState    = ' ';
       erState    = *off;
       mgZipCd    = ' ';
       erZipCd    = *off;
       mgEmplStA  = ' ';
       erEmplStA  = *off;

       If scAltENoA  = *Zeros;
          mgAltEmp#A = 'Alternate Employer number is required';
          erAltEmp#A = *on;
          isError = *On;
       Else;
          If inProcMod = 'A' or (inProcMod = 'C' and scAltENoA <> *Zero and
           scAltENoA <> svAltENoA);
             Setll (xxCo#:xxFnd:xxEmp#:xxDiv:xxfrm1:scAltENoA) pae04;
             Reade (xxCo#:xxFnd:xxEmp#:xxDiv:xxfrm1:scAltENoA) pae04;
             If Not %Eof(Wdael4);
                mgAltEmp#A = 'Alternate Employer number already exist';
                erAltEmp#A = *on;
                isError = *On;
             Endif;
          Endif;
       Endif;

       If scAltENmA  = *Blank;
          mgAltENmA  = 'Alternate Employer name is required';
          erAltENmA  = *on;
          isError = *On;
       Endif;

       If scAddr1  = *Blank;
          mgAddr1  = 'Address is required';
          erAddr1  = *on;
          isError = *On;
       Endif;

       If scCity  = *Blank;
          mgCity  = 'City is required';
          erCity  = *on;
          isError = *On;
       Endif;

       If scState  = *Blank;
          mgState  = 'State is required';
          erState  = *on;
          isError = *On;
       Endif;

       If scZipCd = *Zeros;
          mgZipCD = 'Zip Code is required';
          erZipCd = *on;
          isError = *On;
       Endif;

       If scEmplStA = *Blank;
          mgEmplStA = 'State of Special or Temporary Employment is required';
          erEmplStA = *on;
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
