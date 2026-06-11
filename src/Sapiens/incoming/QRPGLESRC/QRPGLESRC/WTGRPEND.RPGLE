     h Copyright('(c) 2024 Sapiens. All Rights Reserved.')
     h*****************************************************************
     h/title WTGRPEND - Group Endorsement Table setup
     h/define   PROFOUNDUI
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h********************************************************************
     h*
     h* Description: Group Endorsement Table setup
     h*
     h* Called from: W0010R
     h* Calls to:    None
     h*
     h* Created   : 03/12/20
     h* Programmer: C:291
     h* Project   : WC2608
     H*****************************************************************
     H*                                                               *
     H*  This program will process Endorsement table setup in         *
     H*  Group maintenance.                                           *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  06/07/24 C:65  Support: 115720 (Amwins/Inhouse)              *
     H*                 Add tooltip info to display file for the      *
     H*                 optional and state/national form # fields.    *
     H*                                                               *
     H*  12/27/23 C:342 Project:W2932                                 *
     H*                 1.Added Copy hyperlink for Policy Page Endors-*
     H*                 ement form grid                               *
     H*                 2.Issue fixed while using Delete option in    *
     H*                 Policy Page Endorsement forms grid.           *
     H*                 3.Hide Copy button of Approved/Excluded State *
     H*                 grid when open in Inquiry mode.               *
     H*                 4.Do not call GroupCopySetup If No record is  *
     H*                 selected for copy from Policy Page Endorsement*
     H*                 Form.i.e. call only when index >=1            *
    H*                                                               *
     H*  04/18/23 C:300 Project:W2196 Support:109064                  *
     H*                 Show 'Confirm Delete' window having Allign to *
     H*                 Approved/Excluded States grid                 *
    H*                                                               *
     H*  03/27/23 C:300 Support: 107761 (SUNZ)                        *
     H*                 Issue with Copy Endorsements                  *
     H*                 - Filter on the form number column to select  *
     H*                   multiple form for COPY, it also selects very*
     H*                   first endorsement that had loaded into grid *
     H*
     H*  01/24/23 C:291 Project: WC2863                               *
     H*                 Add functionality to Copy Approved/Excluded   *
     H*                 State to other states.                        *
     H*                 Add "Copy" Link to Approved/Excluded State    *
     H*                 Grid.                                         *
     H*                                                               *
     H*  11/28/22 C:300 Support: 101201 (WC2196)
     H*                 Right Align Edition and Adoption dates in Grid*
     H*                 Status Changed to left aligned and            *
     H*                 Width adjusted for these 3 columns            *
     H*                                                               *
     H*  10/25/22 C:079 Project: WC2856                               *
     H*                 Alternate Employer Single vs. Multiple.       *
     H*                 Changes in Display File.  Recompile only.     *
     H*                                                               *
     H*  01/17/22 C:291 Project: WC2788                               *
     H*                 Move Endorsement copy process (ProcessCopy)   *
     H*                 to WPIGRPPOP.                                 *
     H*                 Add "Copy Configuration" option to Group      *
     H*                 Toolbar.                                      *
     H*                                                               *
     H*  10/05/21 C:300 Support: 97521 (In-House)                     *
     H*                 In DSPF, Edit Endorsement Form window, change *
     H*                 field heading "Client Template" to "Client    *
     H*                 Template path".                               *
     H*                                                               *
     H*  08/04/21 C:300 Project: WC2196, Support: 96731               *
     H*                 Show Group Name and Group Number in Action Panel*
     H*                                                               *
     H*  07/09/21 C:65  Support: 95868 (Amwins/Inhouse)               *
     H*                 Add leasing type descriptions to the peo      *
     H*                 relationship dropdown choices.                *
     H*                                                               *
     H*  07/02/21 C:180  Support 96490                                *
     H*                  Do not deactivate more records than          *
     H*                  requested.                                   *
     H*                                                               *
     H*  07/01/21  C:180  Support: 96506                              *
     H*                   Prevent record lock error when retrieving   *
     H*                   endorsement to load screen.                 *
     H*                                                               *
     H*  06/16/21 C:298 Support: 96022                                *
     H*                 Correct edit for Replacement Date to allow    *
     H*                 withdrawl of form by Date.                    *
     H*
     H*  06/09/21 C:300 Project: WC2196, Support: 96185               *
     H*                 Subfile positions to first record in the grid *
     H*                 When "Edit" link is pressed and press "Cancel"*
     H*                 or "Save" on the Edit screen                  *
     H*                                                               *
     H*  05/11/21 C:300 Project: WC2196                               *
     H*                 Check If record has been updated by another   *
     H*                 for EDIT Mode                                 *
     H*                 Display 'Record not found for delete' message *
     H*                 for DELETE mode                               *
     H*                                                               *
     H*  04/05/21 C:291 Project: WC2196                               *
     H*                 Clear nxchgEndr indicator when Endorsement    *
     H*                 grid is loaded for "Exclude All" button.      *
     H*                 Fixed endorsement copy option, when multiple  *
     H*                 endorsements were selected to copy, it only   *
     H*                 copied first endorsement to destination group *
     H*                                                               *
     H*  03/25/21 C:300 Project: WC2196                               *
     H*                 Load this screen on Endorsement/Policy Form   *
     H*                 tab of WTGRPPOLCG, and added visibility       *
     H*                 DSPF changes on RcdEndrFrm record format:     *
     H*                 1) Added visibility indicator on Cancel Button*
     H*                    , LblGroup, OutGroup and OutName           *
     H*                 2) Added HdgPolPageEndorse with visibility ind*
     H*                                                               *
     H*  02/02/21 C:136 Project: WC2757                               *
     H*                 Add blanket waiver default description.       *
     H*                 Fix for unexpired date edit.                  *
     H*                                                               *
     H*  12/28/20 C:180  Project WC2786                               *
     H*                  Add unexpired date                            *
     H*                                                               *
     H*  11/24/20 C:180  Project WC2770                               *
     H*                  Add destination group subfile                *
     H*****************************************************************
     fWtGrpEnd  cf   e             workstn extfile('WTGRPEND')
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflEndrFrm:rrnEndr)
      *** Beg Del ***** 01/17/22 ******************************************
     f***                                  Sfile(sflDupEnd:rrnDEnd)
      *** End Del ***** 01/17/22 ******************************************
     f                                     Sfile(sflBusType:rrnBusTyp)
     f                                     Sfile(sflApExSt:rrnApEx)
      *** Beg Del ***** 01/17/22 ******************************************
     f***                                  Sfile(sflToGrp:rrnToGrp)
      *** End Del ***** 01/17/22 ******************************************
      *** Beg Add ***** 01/24/23 ******************************************
     f                                     Sfile(sflDupSt:rrnDSt)
      *** End Add ***** 01/24/23 ******************************************
     fWtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdGrpPnl)

     fWtenl     uf   e           k disk    rename(pen01:pen00)
     f                                     Infds(dbfds1)
     fWtenp     uf a e             disk    rename(pen01:pen0p)
     fWtesl     uf   e           k disk    rename(pes01:pes00)
     f                                     Infds(dbfds2)
     fWtesp     uf a e             disk    rename(pes01:pes0p)
     fwmfnl     if   e           k disk
     fWdell     if   e           k disk
     fWtgtl     if   e           k disk    rename(pgt01:pgt00)
     fWdf2l     if   e           k disk
      *** Beg Add ***** 01/24/23 ******************************************
     Fststl     if   e           k disk
      *** End Add ***** 01/24/23 ******************************************

     d psds          esds                  extname(szq1p)

     d dbfds1          ds
     d  dbrrn1               397    400b 0

     d dbfds2          ds
     d  dbrrn2               397    400b 0

      *** Beg Add ***** 01/17/22 ********************************************
     d index           s              5  0
     d inData          s            500      inz
     d ptrEndFrm       s               *   inz
     ddsEndrs          ds                  qualified
     d EndrFrm                       15
     d EndrRrn                        9  0
     D arrEndrs        ds                  dim(9999) likeds(dsEndrs)
      *** End Add ***** 01/17/22 ********************************************

      //Standalone fields
     d @@prds          s             40
     d @@TabIndex      s              5
     d xxCo#           s                   like(FNco#)
     d xxFnd           s                   like(FNfnd)
     d clientID        s              3    inz('000')
     d inTabIndex      s              5  0
     d intCo#          s              3  0
     d intRrn          s              9  0
     d xxRrn           s              9  0
     d ##Rrn#          s              9  0
     d ##CltId         s              3
     d isMenuAvbl      s               n   inz(*on)
     d isValid         s              1n   inz(*off)
     d strSlash        s              1
     d strSlash2       s              1
     d skipDocEdit     s               n   inz(*Off)
     d isScriptUsed    s              1A   inz('N')
     d isUseSpoolOut   s               n   inz(*Off)
     d isUseXML        s               n   inz(*Off)
     d procType        s            100
     d outFolder1      s             80
     d outRtcd1        s              1
     d outDocId2       s             12
     d outFolder2      s             80
     d outRtcd2        s              1
     d outName         s             10    inz
     d outType         s              7    inz
     d outRtcd         s              1    inz
     d saveENDt        s                   inz like(ENdt)
     d saveENTm        s                   inz like(ENtime)
     d saveESDt        s                   inz like(ESdt)
     d saveESTm        s                   inz like(EStim)
     d FormWC301       s               n
     d strSelection    s            525
     d inInclude       s             10
     d xxKey           s                   like(GTKey)
     d xxTbid          s                   like(GTTbid)
     d saveRrn         s                   like(rrnEndrRow)
     d doLoad          s               n   inz(*on)

     d Callpgm         s              1
      *** Beg Add ***** 12/27/23 ********************************************
     d svAEndFrm#      s                   like(ENform)
      *** End Add ***** 12/27/23 ********************************************

      * Prototype Declaration
      *****************************************************************
     d ChkStack        pr                  extpgm('SRCHKPGM')
     d  outPgmName                   10    const
     d  isFound                       1

      * Clear Approved/Excluded state data
     d ClrApExdata     pr

      * Clear Old data
     d Clrdata         pr

      * Clear Error/Message fields
     d ClrError        pr

      * Clear Subfiles
     d ClrSubFiles     pr

      * Copy Endorsement Forms to another group

      * Check object
     d ChkObject       pr                  Extpgm('SRCHKOBJ')
     d  outName                      10
     d  outType                       7
     d  outRtcd                       1

      * Edit/Del/Add Endorse State Approved/Excluded
     d EditAddApExSt   pr
     d inProcMode                     1    const
     d inRRN                          9  0 options(*nopass)

      * Edit/Del/Add Endorse Record
     d EditAddEndr     pr
     d inProcMode                     1    const
     d inRRN                          9  0 options(*nopass)

      * Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d ##CltId                        3

      * Load Endorsement Form grid detail
     d LoadEndrDtl     pr
     d inClrSfl                       1    const

      * Load Endorsement Form grid
     d LoadEndrGrid    pr
     d inInclude                     10    const

      * Main Processing
     d Main            pr

     d* Process Change Status
     d  ProcChgSts     pr

     d* Process Copy Endorsements
     d  ProcessCopy    pr

      * Process other options
     d ProcOther       pr

      * Program Start
     d ProgramStart    pr

      * Validate Document
     d ValidateDoc     pr                  extpgm('SVALDOC')
     d outDocId2                     12
     d outFolder2                    80
     d outRtcd2                       1

      * Validate Folder
     d ValidateFolder  pr                  extpgm('SVALFLR')
     d outFolder1                    80
     d outRtcd1                       1

      * Validate Add/Edit screen
     d ValidApExSt     pr              n
     d inMode                         1    const
     d inFrmRrn                       9  0

      * Validate Add/Edit screen
     d ValidEndFrm     pr              n
     d inMode                         1    const
     d inFrmRrn                       9  0

      *entry parameters
     D WTGrpEndors     pr                  Extpgm('WTGRPEND')
     D  @@Co#                         3  0
     D  @@Fnd                         3  0
     D  @@Mode                        1
     D  @@Rrn                         9  0

     D WTGrpEndors     pi
     D  @@Co#                         3  0
     D  @@Fnd                         3  0
     D  @@Mode                        1
     D  @@Rrn                         9  0

     d/copy sPrAtrium
     d/copy sPrDate
     d/copy sPrCompany
     d/copy wPrGroup
     d/copy wPrUsrPrms
     d/copy sPrXML
     d/copy wprGenPop
      *** Beg Add ***** 01/17/22 *********************************
     d/copy wprGrpPop
     d/copy sCopyGrpPr
      *** End Add ***** 01/17/22 *********************************
    
      /Free

       // Start Of Program
       ProgramStart();

       // Main Procesing
       Main();

       If %open(wtgrpend);
          Close WTGRPEND;
       Endif;

       *InLr = *on;

      /End-Free
       //===================================================================
       // ClrApExData - Clear Approved/Excluded State data
       //===================================================================
     p ClrApExData     b
     d ClrApExData     pi
      /Free
         Clear scApExSt;
         Clear scApExEfDt;
         Clear scApExExDt;
         Clear scApExFlg;

         Chain(n) (xxCo#:scApExEndr) pes00;
         If %found(wtesl);
            scApExFlg = ESapex;
            indApEx = *on;
         Else;
            Clear scApExFlg;
            indApEx = *off;
         Endif;

      /End-Free

     p ClrApExData     e
       //===================================================================
       // ClrData - Clear Old data
       //===================================================================
     p ClrData         b
     d ClrData         pi

      /Free
       scAEndFrm# = *Blanks;
       scAEndFDsc = *Blanks;
       scAEdtnDt  = *Zeros;
       SCAEndrSt  = *Blanks;
       SCAStatus  = 'A';
       scOptForm# = *Blanks;
       scNatForm# = *Blanks;
       scEndType  = *Blanks;
       scCltTpPth = *Blanks;
       scFolder   = *Blanks;
       scEndPos   = *Blanks;
       scManAuto  = *Blanks;
       scSingMult = *Blanks;
       SCPrntFrm  = 'N';
       scExFrSchd = 'N';
       scExFrWCPL = 'N';
       scGnFrPDF  = 'N';
       scAtchPgm  =  *Blanks;
       scPrntPgm  =  *Blanks;
       scAdInfPgm =  *Blanks;
       scOffEndrs = *Blanks;
       scReplForm =  *Blanks;
       scReplDate =  *Zeros;
       scOffEndrs = *Blanks;
       scBusTypF = *Blanks;
       scBusTypV = *Blanks;
       scUnExpDt =  *Zeros;

      /End-Free

     p ClrData         e
       //===================================================================
       // ClrError - Clear Error/Message fields
       //===================================================================
     p ClrError        b
     d ClrError        pi

      /Free

        msAEndFrm# = *blanks;
        erAEndFrm# = *off;
        msAEndFDsc = *blanks;
        erAEndFDsc = *off;
        msAEdtnDt  = *blanks;
        erAEdtnDt  = *off;
        msAAdptDt  = *blanks;
        erAAdptDt  = *off;
        msCltTpPth = *blanks;
        erCltTpPth = *off;
        msAtchPgm  = *blanks;
        erAtchPgm  = *off;
        msPrntPgm  = *blanks;
        erPrntPgm  = *off;
        msAdInfPgm = *blanks;
        erAdInfPgm = *off;
        msReplForm = *blanks;
        erReplForm = *off;
        msReplDate = *blanks;
        erReplDate = *off;
        erEndFrm   = *blanks;
        msEndFrm   = *off;
        msPrntFrm  = *blanks;
        erPrntFrm  = *off;
        msAManAuto = *blanks;
        erAManAuto = *off;
        msSingMult = *blanks;
        erSingMult = *off;
        msBusTypF  = *blanks;
        erBusTypF  = *off;
        msUnExpDt = *blanks;
        erUnExpDt = *off;
      /End-Free

     p ClrError        e
       //===================================================================
       // ClrSubFiles - Clears SubFiles
       //===================================================================
     p ClrSubFiles     b
     d ClrSubFiles     pi

      /Free
       //Endorsement form subfile
       rrnEndr = 0;
       clrEndr = *On;
       dspEndrSub  = *Off;
       dspEndrCtl  = *Off;

       //Business Types subfile
       rrnBusTyp = 0;
       clrBusTyp = *on;
       dspBusTyp = *off;
       dspBusCtl = *off;

       //Approved/Excluded States subfile
       rrnApEx = 0;
       clrApEx = *on;
       dspApEx = *off;
       dspApExCtl = *off;

       Write rcdEndrFrm;

       clrEndr = *Off;
       dspEndrSub = *On;
       dspEndrCtl = *On;

       clrBusTyp = *off;
       dspBusTyp = *on;
       dspBusCtl = *on;

       clrApEx = *off;
       dspApEx = *on;
       dspApExCtl = *on;

      /End-Free

     p ClrSubfiles     e
       //*** Beg Add ***** 01/24/23 ****************************************
       //===================================================================
       // CopyApExSt - Copy Approved/Excluded State to other States
       //===================================================================
     p CopyApExSt      b
     d CopyApExSt      pi
     d inRRN                          9  0
      /Free
      *
       Dcl-S xxkey Like(STkey) inz;
       Dcl-S xxState Char(2) inz;
       Dcl-S ExitNow Ind inz(*Off);
       Dcl-S isDupSt Ind inz(*Off);

       Clear RcdCpyAESt;
       Exsr ClrSflDupSt;

       Clear scSkipRpl;
       BtnCpyAESt = *off;
       BtnTStClk =  *off;
       BtnCpyAESt =  *off;
       VisCpyAESt =  *off;
       visDupSt = *off;
       scCpyPnTop = 230;
       scCpyPnlHt = 270;
       TopDupSt = 500;
       TopToSt = 255;
       TopNoDupSt = TopToSt + 245;
       visDupSkRp = *Off;
       VisCpyAESt = *Off;
       Dou ExitNow = *on;
          Chain(n) inRrn pes0p;
          If %Found(wtesp);
             If ESapex = 'A';
                scApExTitl = 'Approved State Copy for ' + %Trim(ESform);
             Else;
                scApExTitl = 'Excluded State Copy for ' + %Trim(ESform);
             EndIf;
          Endif;
          xxkey = ESst;
          Clear stChoices;
          stChoices = 'STKEY <> ''' + xxKey + ''' AND STSTAT <> ''I''';
          Chain xxkey pst01;
          scApExStC = ESst + ' - ' + %Trim(STname);
          scAEEfDtC = dtmdyy(ESadte);
          scAEExDtC = dtmdyy(ESexdt);

          Exfmt RcdCpyAESt;
          Select;
          When btnTStClk = *on;
             Clear scSkipRpl;
             Clear sflDupSt;
             isDupSt = *Off;
             Exsr ClrSflDupSt;
             strSelection = scAEStC;
             Dow strSelection <> ' ';
                xxState = %Subst(strSelection:1:2);
                Chain (xxCo#:ESForm:xxState) pes00;
                If %Found(wtesl);
                   ScStateD = ESst;
                   scAEEfDtD = CvtToDate(ESadte);
                   scAEExDtD = CvtToDate(ESexdt);
                   rrnDSt = rrnDSt + 1;
                   Write sflDupSt;
                Endif;
                strSelection = %subst(strSelection:4);
             Enddo;

             If rrnDSt > 0;
                isDupSt = *On;
                visDupSt = *On;
                visDupSkRp = *On;
                scCpyPnlHt = 540;
                TopNoDupSt = TopDupSt + 230;
                visCpyAESt = *On;
                disCpyAESt = *On;
             Else;
                visCpyAESt = *On;
                visDupSt = *Off;
                visDupSkRp = *Off;
                scCpyPnlHt = 310;
                TopNoDupSt = TopToSt + 245;
                disCpyAESt = *Off;
             Endif;
             rrnDSt = 1;
          When BtnCancel = *On;
             ExitNow = *on;
          When BtnCpyAESt = *On;
             strSelection = scAEStC;
             Dow strSelection <> ' ';
                xxState = %Subst(strSelection:1:2);
                Select;
                When isDupSt = *On;
                   Select;
                   When scSkipRpl = 'S';
                      Chain(n) (xxCo#:ESForm:xxState) pes00;
                      If Not %Found(wtesl);
                         Exsr WriteWTesp;
                      Endif;
                   When scSkipRpl = 'R';
                      Chain (xxCo#:ESForm:xxState) pes00;
                      If %Found(wtesl);
                         Delete pes00;
                         Exsr WriteWTesp;
                      Else;
                         Exsr WriteWTesp;
                      Endif;
                   Endsl;
                When isDupSt = *Off;
                   Exsr WriteWTesp;
                Endsl;
                strSelection = %subst(strSelection:4);
             Enddo;
             ExitNow = *on;
          Endsl;
       Enddo;

       Begsr ClrSflDupSt;
        RrnDSt = 0;
        dspDSt = *Off;
        dspDStCtl = *Off;
        clrDSt = *On;
        Write rcdCpyAESt;
        dspDSt = *On;
        dspDStCtl = *On;
        clrDSt = *Off;
       Endsr;

       Begsr WriteWTesp;
        Chain(n) inRrn pes0p;
        If %Found(wtesp);
           ESst  = xxState;
           ESipgm = Q1pgm;
           ESiusr = Q1user;
           ESitim = Timeto6(SystemTime());
           ESidt  = Dateto7(SystemDate());
           ESpgm = Q1pgm;
           ESusr = Q1user;
           EStim = Timeto6(SystemTime());
           ESdt  = Dateto7(SystemDate());
           Write pes0p;
        Endif;
       Endsr;


      /End-Free
     p CopyApExSt      e
       //*** End Add ***** 01/24/23 ***********************************
       //*** Beg Del ***** 01/17/22 ***********************************
       //===================================================================
       // CopyEndrsForm - Copy selected endorsements to To Group
       //===================================================================
     p* CopyEndrsForm   b
     d* CopyEndrsForm   pi
     d* inSkipRepl                     1    const
      *
      */Free
      *
       //ReadC sflEndrFrm;
       //Dow Not %Eof;
       //   If scSelect = 'Y';
       //      ReadC sflToGrp;
       //         Dow Not %Eof;
       //            If scSelToGrp = 'Y';
       //               inToGroup = scToGrp;
       //                  nxChgToGrp = *On;
       //                  update sflToGrp;
       //               Select;
       //               //Replace when duplicates found in To Group
       //               When inSkipRepl = 'R';
       //                  //Check if form exist in To Group
       //                  chain (@@co#:inToGroup:scEndForm) pen00;
       //                  If %found(wtenl);
       //                     //Delete the existing endorsement form and write new form in To Group
       //                     Delete pen00;
       //                     Exsr WriteWTenp;
       //                  Else;
       //                     Exsr WriteWTenp;
       //                  Endif;
       //               //Skip duplicate forms When duplicate found in To Group
       //               When inSkipRepl = 'S';
       //                  //Check if form exist in To Group
       //                  chain(n) (@@co#:inToGroup:scEndForm) pen00;
       //                  If not %found(wtenl);
       //                     //Write to To Group if form does not exist in To Group
       //                     Exsr WriteWTenp;
       //                  Endif;
       //               //No dupllicate forms found in To Group, Copy all the forms
       //               When inSkipRepl = 'A';
       //                  Exsr WriteWTenp;
       //               Endsl;
       //            Endif;
       //            ReadC sflToGrp;
       //         Enddo;
       //   Endif;
       //   ReadC sflEndrFrm;
       //Enddo;
       //
       //Begsr WriteWTenp;
       //  //Write to To Group if form does not exist in To Group
       //  Chain(n) scEndrRrn# pen0p;
       //  If %found(wtenp);
       //     ENfnd = inToGroup;
       //     ENpgm  = Q1pgm;
       //     ENuser = Q1user;
       //     ENtime = Timeto6(SystemTime());
       //     ENdt   = Dateto7(SystemDate());
       //     Write pen0p;
       //  Endif;
       //Endsr;
       //
      */End-Free
     p* CopyEndrsForm   e
       //*** End Del ***** 01/17/22 ****************************************
       //===================================================================
       // EditAddApExSt  - Edit/Del/Add Approved/Excluded States
       //===================================================================
     p EditAddApExSt   b
     d EditAddApExSt   pi
     d inProcMode                     1    const
     d inRRN                          9  0 options(*nopass)

     d  ExitNow        s               n   inz
     d  xxDelResp      s               n   inz
      //*** Beg Add ***** 04/18/23 *****************************
     d  xxDelMsg       s            256    inz
      //*** End Add ***** 04/18/23 *****************************

      /Free

       If %parms() > 1;
          intRrn = inRrn;
       Else;
          intRrn = *zeros;
       Endif;

       saveESdt = Dateto7(systemDate());
       saveEStm = Timeto6(SystemTime());

       Clear RcdAddApEx;

       Select;
       When inProcMode = 'A';
          scApExTitl = 'Add Approved/Excluded Information';
       When inProcMode = 'C';
          scApExTitl = 'Edit Approved/Excluded Information';
       Endsl;
       BtnApExCan = *off;
       BtnApExNxt= *off;
       BtnApExSav = *off;
       //Initialize Group name and description
       scApExEndr = scEndForm;
       scApExFDsc = scEndDesc;

       //Processing to Edit Endorse Record
       If inProcMode = 'C' and intRrn <> 0;
          Chain(n) intRrn pes0p;
          If %Found(wtesp);
             Exsr LoadFilToScn;
          Endif;
          //Intialize the left alignment of Cancel Button.
          scAECanLft = 630;
          //Save/Add Next Button will not get displyed.
          indASvAdNx = *Off;
          //Protect fields in Edit mode
          indEdtApEx = *On;
          indApEx = *On;
       Endif;

       // Processing to Add Endorse Record
       If inProcMode = 'A';
          Clear pes0p;
          ClrApExData();
         //Initialize the left alignment of Cancel Button.
          scAECanLft = 520;
         //Save/Add Next Button will get displyed.
         indASvAdNx = *On;
         indEdtApEx = *Off;
       Endif;

       Dou ExitNow = *on;
          Select;
          When inProcMode = 'D';
             //*** Beg Chg ***** 04/18/23 *****************************
             //*** xxDelresp = PopTwoBtn('Do you want to delete this Approved/-
             //***  Excluded State?': 'Confirm Delete':'Yes':'No':0:140);
             xxDelMsg = 'Do you want to delete this Approved/Excluded State - '+
              scAEState + '?';
             xxDelresp = PopTwoBtn(xxDelMsg: 'Confirm Delete':'Yes':'No':0:380);
             //*** End Chg ***** 04/18/23 *****************************
             Select;
             When xxDelResp = *On;
                Chain intRrn pes0p;
                If %found(wtesp);
                   Exsr WrtUpdApExSt;
                   //*** Beg Chg ***** 04/18/23 *****************************
                   //*** PopOneBtn('Approved/Exclude State deleted!':'Delete':'Ok':
                   //*** 0:140);
                   PopOneBtn('Approved/Exclude State deleted!':'Delete':'Ok':
                   0:380);
                   //*** End Chg ***** 04/18/23 *****************************
                Else;
                   //*** Beg Chg ***** 04/18/23 *****************************
                   //*** PopOneBtn('Record not found for Delete.':'Delete Error':'Ok':
                   //*** 0:140);
                   PopOneBtn('Record not found for Delete.':'Delete Error':'Ok':
                   0:380);
                   //*** End Chg ***** 04/18/23 *****************************
                Endif;
                ExitNow = *On;
             When xxDelResp = *Off;
                ExitNow = *On;
             Endsl;
          Other;
             Exfmt rcdAddApEx;
             Select;
             When btnApExCan = *On;
                ExitNow = *On;
             When btnApExSav = *On;
                ExSr ProcessSave;
             When btnApExNxt = *On;
                ExSr ProcessSave;
                If ExitNow = *On;
                   ExitNow = *Off;
                   clear pes0p;
                   ClrApExData();
                Endif;
             Endsl;
          Endsl;
       Enddo;

       Return;

       //**********************************
       // Load File fields to screen
       //**********************************
       Begsr LoadFilToScn;
         scApExEndr = ESForm;
         scApExFDsc = scEndDesc;
         scApExSt   = ESst;
         scApExFlg  = ESapex;
         scApExEfDt = dtmdyy(ESadte);
         scApExExDt = dtmdyy(ESexdt);
       Endsr;

       //**********************************
       // Load Screen Fields to File
       //**********************************
       Begsr LoadScnToFil;
         ESco#   = @@co#;
         ESForm  = scApExEndr;
         ESst    = scApExSt;
         ESapex  = scApExFlg;
         ESadte  = DtCymd(scApExEfDt);
         ESexdt  = DtCymd(scApExExDt);
       Endsr;

       //**********************************
       // ProcessSave
       //**********************************
       Begsr ProcessSave;
          If ValidApExSt(inProcMode:intRrn);
             If inProcMode <> 'A';
                Chain intRrn pes0p;
             Endif;
             Exsr LoadScnToFil;
             Exsr WrtUpdApExSt;
             ExitNow = *On;
          EndIf;
       EndSr;

       //********************************************************
       //*   Write/Update Endorsement Form
       //********************************************************
       BegSr WrtUpdApExSt;

         ESpgm = Q1pgm;
         ESusr = Q1user;
         EStim = Timeto6(SystemTime());
         ESdt  = Dateto7(SystemDate());
         saveESdt = ESdt;
         saveEStm = EStim;
         Select;
         When inProcMode = 'A';
            ESipgm  = ESpgm;
            ESiusr  = ESusr;
            ESitim  = EStim;
            ESidt   = ESdt;
            Write pes0p;
         When inProcMode = 'C';
            Update pes0p;
         When inProcMode = 'D';
            Delete pes0p;
         EndSl;
       Endsr;
      /End-Free

     p EditAddApExSt   e
       //===================================================================
       // EditAddEndr  - Edit/Del/Add Endorsement form Record
       //===================================================================
     p EditAddEndr     b
     d EditAddEndr     pi
     d inProcMode                     1    const
     d inRRN                          9  0 options(*nopass)

     d  ExitNow        s               n   inz
     d  xxDelResp      s               n   inz

      /Free

       If %parms() > 1;
          intRrn = inRrn;
       Else;
          intRrn = *zeros;
       Endif;

       saveENdt = Dateto7(systemDate());
       saveENtm = Timeto6(SystemTime());

       Clear RcdAddEndr;

       Select;
       When inProcMode = 'A';
          scEFrmTitl = 'Add Endorsement Form';
       When inProcMode = 'C';
          scEFrmTitl = 'Edit Endorsement Form';
       //*** Beg Add ***** 12/27/23 *****************************
       When inProcMode = 'P';
          scEFrmTitl = 'Copy Endorsement Form';
       //*** End Add ***** 12/27/23 *****************************
       Endsl;
       BtnEndCan = *off;
       BtnEndsNxt= *off;
       BtnEndSav = *off;
       //Load selection criteria
       Exsr LoadEndSel;
       //Initialize Group name and description
       scAGrp   = @@Fnd;
       scAGrpDs = GetGrpName(@@Fnd);

       //Processing to Edit Endorse Record
       //*** Beg Chg ***** 12/27/23 *****************************
       //*** If inProcMode = 'C' and intRrn <> 0;
       If (inProcMode = 'C' or inProcMode = 'P') and intRrn <> 0;
       //*** End Chg ***** 12/27/23 *****************************
          Chain(n) intRrn pen0p;
          If %Found(wtenp);
             Exsr LoadFilToScn;
          Endif;
          //Intialize the left alignment of Cancel Button.
          scCanLft = 870;
          //Save/Add Next Button will not get displyed.
          indSvAdNx = *Off;
          //Protect fields in Edit mode
          indEdtEFrm = *On;
          //*** Beg Add ***** 12/27/23 *****************************
          If inProcMode = 'P';
             //do not protect fields in Copy mode
             indEdtEFrm = *Off;
          Endif;
          //*** End Add ***** 12/27/23 *****************************
       Endif;

       // Processing to Add Endorse Record
       If inProcMode = 'A';
          Clear pen0p;
          ClrData();
         //Initialize the left alignment of Cancel Button.
          scCanLft = 760;
         //Save/Add Next Button will get displyed.
         indSvAdNx = *On;
         indEdtEFrm = *Off;
       Endif;

       Dou ExitNow = *on;
          Select;
          When inProcMode = 'D';
           //Write rcdAddEndr;
             xxDelresp = PopTwoBtn('Do you want to delete this Endorsement -
              Form?': 'Confirm Delete':'Yes':'No':0:140);
             Select;
             When xxDelResp = *On;
                Chain intRrn pen0p;
                If %found(wtenp);
                   //*** Beg Add ***** 12/27/23 *****************************
                   Exsr DltApExcSt;
                   //*** End Add ***** 12/27/23 *****************************
                   Exsr WrtUpdEndFrm;
                   PopOneBtn('Endorsement Form deleted!':'Delete':'Ok':0:140);
                Else;
                   PopOneBtn('Record not found for Delete.':'Delete Error':'Ok':
                   0:140);
                Endif;
                ExitNow = *On;
             When xxDelResp = *Off;
                ExitNow = *On;
             Endsl;
          Other;
             Exfmt rcdAddEndr;
             Select;
             When btnEndCan = *On;
                ExitNow = *On;
             When btnEndSav = *On;
                ExSr ProcessSave;
             When btnEndSNxt = *On;
                ExSr ProcessSave;
                If ExitNow = *On;
                   ExitNow = *Off;
                   clear pen0p;
                   Exsr LoadFilToScn;
                Endif;
             Endsl;
          Endsl;
       Enddo;

       Return;

       //**********************************
       //Load Selection criteria
       //**********************************
       Begsr LoadEndSel;

         //Endorsement Type
         slEndTyp = 'GTCO# = ' + %char(@@CO#) +
          ' AND GTTBID = ''ENTP'' AND GTSTAT = ''A''';

         //PEO selection criteria
         dspPEO = *off;
         slPEORel = ' ';
         chPEORel = ' ';
         If FNPEOF <> ' ';
            dspPEO = *on;
            slPEORel = ' ,Master Policy (Type 2),MCP PEO (Type 3),Master or ' +
             'MCP PEO (Type 2 or 3),MCP Client (Type 4 or 5),' +
             'MCP PEO or MCP Client (MCP Only Type 3 or 4 or 5)';
            chPEORel = ' ,1,2,3,4,5';
         Endif;
       Endsr;

       //**********************************
       // Load File fields to screen
       //**********************************
       Begsr LoadFilToScn;
         scAGrp     = @@Fnd;
         scAGrpDs   = GetGrpName(@@Fnd);
         scAEndFrm# = ENform;
         scAEndFDsc = ENdsc2;
         scAEdtnDt  = dtmdyy(ENeddt);
         scAAdptDt  = dtmdyy(ENadte);
         SCAEndrSt  = ENst;
         SCAStatus  = ENstat;
         //*** Beg Add ***** 12/27/23 *****************************
         If inProcMode = 'P';
            SCAStatus  = 'A';
            svAEndFrm# = ENform;
         Endif;
         //*** End Add ***** 12/27/23 *****************************
         scOptForm# = ENformo;
         scNatForm# = ENnatfm;
         scEndType  = ENtype;
         scCltTpPth = ENpathc;
         scFolder   = ENfldr;
         scEndPos   = ENdpos;
         scAManAuto = ENmnau;
         scSingMult = ENsngl;
         SCPrntFrm  = ENprnt;
         scExFrSchd = ENexfl;
         scExFrWCPL = ENexcwc;
         scGnFrPDF  = ENgpdf;
         scAtchPgm  = ENpgm1;
         scPrntPgm  = ENpgm2;
         scAdInfPgm = ENpgm3;
         scOffEndrs = *Blanks;
         scReplForm = ENrpfm;
         scReplDate = dtmdyy(ENrpdt);
         scPEORel   = ENPEOF;
         scOffEndrs = ENoef;
         scBusTypF  = ENotobf;
         scBusTypV  = ENotobv;
         isOffEndrs = *off;
         If ENoef <> *blank;
            isOffEndrs = *on;
         Endif;
         scBusTypV = *blank;
         strSelection = ENotobv;
         Dow strSelection <> ' ';
            scBusTypV = %trim(scBusTypV) + ',' + %subst(strSelection:1:1);
            strSelection = %subst(strSelection:2);
         Enddo;
         scBusTypV = %subst(scBusTypV:2);
         scBlkWvDs  = ENblkWvDs;
         scUnExpDt = dtmdyy(ENunExpDt);

       Endsr;

       //**********************************
       // Load Screen Fields to File
       //**********************************
       Begsr LoadScnToFil;
         ENco#   = @@co#;
         ENFnd   = @@Fnd;
         ENForm  = scAEndFrm#;
         ENdesc  = scAEndFDsc;
         ENdsc2  = scAEndFDsc;
         ENeddt  = DtCymd(scAEdtnDt);
         ENadte  = DtCymd(scAAdptDt);
         ENst    = SCAEndrSt;
         ENstat  = SCAStatus;
         ENformo = scOptForm#;
         ENnatfm = scNatForm#;
         ENtype  = scEndType;
         ENpathc = scCltTpPth;
         ENfldr  = scFolder;
         ENdpos  = scEndPos;
         ENmnau  = scAManAuto;
         ENsngl  = scSingMult;
         ENprnt  = SCPrntFrm;
         ENexfl  = scExFrSchd;
         ENexcwc = scExFrWCPL;
         ENgpdf  = scGnFrPDF;
         ENpgm1  = scAtchPgm;
         ENpgm2  = scPrntPgm;
         ENpgm3  = scAdInfPgm;
         ENrpfm  = scReplForm;
         ENrpdt  = DtCymd(scReplDate);
         ENpeof  = scPEORel;
         ENoef   = scOffEndrs;
         ENotobf = scBusTypF;
         //Load Officer Type of Business flag/values
         ENotobf = *blank;
         ENotobv = *blank;
         If scOffEndrs <> *blank;
            strSelection = scBusTypV;
            Dow strSelection <> ' ';
               ENotobv = %trim(ENotobv) + %subst(strSelection:1:1);
               strSelection = %subst(strSelection:3);
            Enddo;
            ENotobf = scBusTypF;
         Endif;
         ENblkWvDs  = scBlkWvDs;
         ENunExpDt  = DtCymd(scUnExpDt);
       Endsr;

       //**********************************
       // ProcessSave
       //**********************************
       Begsr ProcessSave;
          ClrError();
          If ValidEndFrm(inProcMode:intRrn);
             If inProcMode <> 'A';
                Chain intRrn pen0p;
             Endif;
             Exsr LoadScnToFil;
             Exsr WrtUpdEndFrm;
             ExitNow = *On;
          EndIf;
       EndSr;

       //********************************************************
       //*   Write/Update Endorsement Form
       //********************************************************
       BegSr WrtUpdEndFrm;

         ENpgm  = Q1pgm;
         ENuser = Q1user;
         ENtime = Timeto6(SystemTime());
         ENdt   = Dateto7(SystemDate());
         saveENdt = ENdt;
         saveENtm = ENtime;
         Select;
         When inProcMode = 'A';
            Write pen0p;
         When inProcMode = 'C';
            Update pen0p;
         When inProcMode = 'D';
            Delete pen0p;
         //*** Beg Add ***** 12/27/23 *****************************
         When inProcMode = 'P';
            Write pen0p;
            Exsr WrtApExcSt;
            rrnEndrRow = 0;     //Select 1st row after sucessful copy operation
         //*** End Add ***** 12/27/23 *****************************
         EndSl;
       Endsr;

       //*** Beg Add ***** 12/27/23 *****************************
       //*************************************
       // DltApExcSt -  Delete Wtesp records
       //*************************************
       Begsr DltApExcSt;
        Setll (ENco#: ENform) pes00;
        Reade (ENco#: ENform) pes00;
        Dow not %eof(Wtesl);
           delete pes00;
           Reade (ENco#: ENform) pes00;
        Enddo;
       EndSr;
       //*************************************
       // WrtApExcSt -  Write Wtesp records
       //*************************************
       Begsr WrtApExcSt;
        Setll (ENco#: svAEndFrm#) pes00;
        Reade (ENco#: svAEndFrm#) pes00;
        Dow not %eof(Wtesl);
           ESForm = scAEndFrm#;
           ESpgm = Q1pgm;
           ESusr = Q1user;
           EStim = Timeto6(SystemTime());
           ESdt  = Dateto7(SystemDate());
           ESipgm = Q1pgm;
           ESiusr = Q1user;
           ESitim = Timeto6(SystemTime());
           ESidt  = Dateto7(SystemDate());
           write pes0p;
           Reade (ENco#: svAEndFrm#) pes00;
        Enddo;
       EndSr;
       //*** End Add ***** 12/27/23 *****************************
      /End-Free

     p EditAddEndr     e
       //===================================================================
       // LoadEndrDtl - Load Endorsement form Detail
       //===================================================================
     p LoadEndrDtl     b
     d LoadEndrDtl     pi
     d inClrSfl                       1    const

      /Free

       If rrnEndrRow = 0;
          rrnEndrRow = 1;
       Endif;

       // Read from subfile
       Chain rrnEndrRow sflEndrFrm;

       If %found;
          Chain(n) scEndrRrn# pen0p;
          If not %found(wtenp);
             clear pen0p;
          Endif;
          If scSelect = 'Y';
             nxchgEndr = *on;
          Else;
             nxchgEndr = *off;
          Endif;
          update sflEndrFrm;
       Else;
          clear pen0p;
       Endif;

       //Clear Endorsement detail fields
       Exsr ClrEndrDtl;

       //Endorsement Form
       scDEndFrm# = ENform;

       //Form Description
       scDEndFDsc = ENdsc2;

       //Edition Date
       scDEdtnDt  = %EditW(dtMDYY(ENeddt):'  /  /    ');

       //Adoption Date
       scDAdptDt  = %EditW(dtMDYY(ENadte):'  /  /    ');

       //Endorsement State
       SCDEndrSt  = ENst;

       //Endorsement status
       Select;
       When ENstat = 'I';
          SCDEndrSts = 'Inactive';
       When ENstat = 'A';
          SCDEndrSts = 'Active';
       Endsl;

       //Optional Form Number
       scDOptForm = ENformo;

       //National/State Form Number
       scDNatForm = ENnatfm;

       //Endorsement Type
       scDEndType = ' ';
       If EnType <> *blank;
          Chain (@@Co#:'ENTP':EnType) pgt00;
          scDEndType = %trim(gtkey) + ' - ' + %trim(gtdesc);
       Endif;

       // Display blanket waiver description if waiver type
       If ENtype = 'Waiver';
          scdBlkWvrD = ENblkWvDs;
          visBlkWvr = *On;
       Else;
          scdBlkWvrD = *Blanks;
          visBlkWvr = *Off;
       Endif;

       //Client Template
       scDCltTPth = ENpathc;

       //Folder
       scDFolder = ENfldr;

       //Form Position
       Select;
       When ENdpos = 'D';
          scDEndPos = 'Before Dec Page';
       When ENdpos = 'B';
          scDEndPos = 'Before Endorsements';
       When ENdpos = 'A';
          scDEndPos = 'After Endorsements';
       Endsl;

       //Manual/Auto
       Select;
       When ENmnau = 'M';
          scDManAuto = 'Manual';
       When ENmnau = 'A';
          scDManAuto = 'Auto';
       Endsl;

       //Single/Multiple Form
       Select;
       When ENsngl = 'S';
          scDSingMlt = 'Single';
       When ENsngl = 'M';
          scDSingMlt = 'Multiple';
       Endsl;

       //Print Form
       scDPrntFrm = Enprnt;

       //Exclude from Schedule
       scDExFrSch = Enexfl;

       //Exclude from WCPOLS
       scDExFWCPL = ENexcwc;

       //Generate From PDF
       scDGnFrPDF = ENgpdf;

       //Attachment Program
       scDAtchPgm = ENpgm1;

       //Print Program
       scDPrntPgm = ENpgm2;

       //Additional Information Program
       scDAdInPgm = ENpgm3;

       //Replacement Form
       scDReplFrm = ENrpfm;

       //Replacement Date
       scDReplDte = %EditW(dtMDYY(ENrpdt):'  /  /    ');

       //PEO Relationship
       Select;
       When ENpeof = '1';
          scDPEORel = 'Master PEO';
       When ENpeof = '2';
          scDPEORel = 'MCP PEO';
       When ENpeof = '3';
          scDPEORel = 'Master or MCP PEO';
       When ENpeof = '4';
          scDPEORel = 'MCP Client';
       When ENpeof = '5';
          scDPEORel = 'MCP PEO or MCP Client';
       Endsl;

       //Officer Endorsement
       Select;
       When ENoef = 'I';
          scDOffEndr = 'Inclusion';
       When ENoef = 'E';
          scDOffEndr = 'Exclusion';
       Endsl;

       //Business Types
       scDBusTypF = ENotobf;

       If ENoef <> *blank;
          isDOffEndr = *on;
          scHLAPgm = 260;
          scHTAPgm = 555;
          scDLApgm = 260;
          scDTApgm = 570;
          scHLPPgm = 480;
          scHTPPgm = 555;
          scDLPPgm = 480;
          scDTPPgm = 570;
          scHLAIPgm = 625;
          scHTAIPgm = 555;
          scDLAIPgm = 625;
          scDTAIPgm = 570;
          scHLRplFrm = 260;
          scHTRplFrm = 590;
          scDLRplFrm = 260;
          scDTRplFrm = 605;
          scHLRepDte = 480;
          scHTRepDte = 590;
          scDLRepDte = 480;
          scDTRepDte = 605;
          scHLPEORel = 625;
          scHTPEORel = 590;
          scDLPEORel = 625;
          scDTPEORel = 605;
          scLine1Top = 630;
          scCont1Top = 645;
       Else;
          isDOffEndr = *off;
          scHLAPgm = 960;
          scHTAPgm = 465;
          scDLApgm = 960;
          scDTApgm = 480;
          scHLPPgm = 1100;
          scHTPPgm = 465;
          scDLPPgm = 1100;
          scDTPPgm = 480;
          scHLAIPgm = 960;
          scHTAIPgm = 495;
          scDLAIPgm = 960;
          scDTAIPgm = 510;
          scHLRplFrm = 960;
          scHTRplFrm = 430;
          scDLRplFrm = 960;
          scDTRplFrm = 445;
          scHLRepDte = 1100;
          scHTRepDte = 430;
          scDLRepDte = 1100;
          scDTRepDte = 445;
          scHLPEORel = 810;
          scHTPEORel = 495;
          scDLPEORel = 810;
          scDTPEORel = 510;
          scLine1Top = 555;
          scCont1Top = 570;
          If visBlkWvr = *On;
             scLine1Top = 630;
             scCont1Top = 645;
          Endif;
       Endif;

       If isDOffEndr = *on;
          Exsr LoadBusType;
       Endif;

       If scEndrSt = *Blank;
          isApExSt = *on;
       Else;
          isApExSt = *off;
       Endif;
       Select;
       When ENoef = *blank And visBlkWvr = *Off;
          If scEndrSt =  *Blank;
             topCxBtn = 790;
          Else;
             topCxBtn = 580;
          Endif;
       Other;
          If scEndrSt =  *Blank;
             topCxBtn = 865;
          Else;
             topCxBtn = 655;
          Endif;
       Endsl;

       dspAEVisb = *Off;
       If isApExSt = *on;
          Exsr LoadApExState;
          If @@Mode = '1';
             dspAEVisb = *Off;
          Else;
             dspAEVisb = *On;
          Endif;
       Endif;

       //--------------------------------------------
       // Subroutines
       //--------------------------------------------
       Begsr ClrEndrDtl;
         Clear scDEndFrm#;
         Clear scDEndFDsc;
         Clear scDEdtnDt;
         Clear scDAdptDt;
         Clear SCDEndrSt;
         Clear SCDEndrSts;
         Clear scDOptForm;
         Clear scDNatForm;
         Clear scDCltTPth;
         Clear scDEndType;
         Clear scDFolder;
         Clear scDEndPos;
         Clear scDManAuto;
         Clear scDSingMlt;
         Clear scDPrntFrm;
         Clear scDExFrSch;
         Clear scDExFWCPL;
         Clear scDGnFrPDF;
         Clear scDAtchPgm;
         Clear scDPrntPgm;
         Clear scDAdInPgm;
         Clear scDReplFrm;
         Clear scDReplDte;
         Clear scDOffEndr;
         Clear scDPEORel;
         Clear scDBusTypF;
       Endsr;

       Begsr LoadApExState;
         If inClrSfl = 'Y';
            dspApEx = *off;
            dspApExCtl = *off;
            clrApEx = *on;
            Write rcdEndrFrm;
            clrApEx = *off;
            dspApEx = *on;
            dspApExCtl = *on;
            rrnApEx = 0;
         Endif;

         clear dbrrn2;

         Setll (xxCo#:scEndForm) pes00;
         Reade(n) (xxCo#:scEndForm) pes00;
         Dow Not %Eof(wtesl);

            //Endorsement Form
            ScAEEndFrm = ESform;

            //Endorsement Form Description
            scAEEndDsc = scEndDesc;

            //Endorsement Form State
            scAEState = ESst;

            //Effective Date
            scAEEffDt = CvtToDate(ESadte);

            //Expiration Date
            scAEExpDt = CvtToDate(ESexdt);

            //Approved/Excluded flag
            Select;
            When ESapex  = 'A';
               ScAprExcl = 'Approved';
            When ESapex  = 'E';
               ScAprExcl = 'Excluded';
            Endsl;

            Select;
            When @@mode = '1';
              isSLnkVisb = *Off;
            Other;
              isSLnkVisb = *On;
            Endsl;

            scAERrn# = dbrrn2;
            rrnApEx = rrnApEx + 1;

            clear scApExRwSl;
            If (rrnApEx = rrnApExCsr) or (rrnApEx = 1 and rrnApExCsr = 0);
               scApExRwSl = 'A';
            Endif;

            Write sflApExSt;
            Reade(n) (xxCo#:scEndForm) pes00;

         Enddo;

         rrnApEx = 1;

       Endsr;

       Begsr LoadBusType;
       //Business Types subfile

         If inClrSfl = 'Y';
            dspBusTyp = *off;
            dspBusCtl = *off;
            clrBusTyp = *on;
            Write rcdEndrFrm;
            clrBusTyp = *off;
            dspBusTyp = *on;
            dspBusCtl = *on;
            rrnBusTyp = 0;
         Endif;

         strSelection = ENotobv;
         Dow strSelection <> ' ';
            xxTbId = 'BUSI';
            xxKey  = %subst(strSelection:1:1);
            Chain (@@Co#:xxTbId:xxKey) pgt00;
            If %found;
               scDBusTyp = %trim(GTDesc);
            Else;
               scDBusTyp = %trim(xxKey);
            Endif;

            rrnBusTyp = rrnBusTyp + 1;
            Write sflBusType;

            strSelection = %subst(strSelection:2);
         Enddo;
         rrnBusTyp = 1;
       Endsr;

      /End-Free

     p LoadEndrDtl     e
       //===================================================================
       // Load Endorse Value Detail Grid
       //===================================================================
     p LoadEndrGrid    b
     d LoadEndrGrid    pi
     d inIncludeFrm                  10    const

      /Free

       If @@Mode = '1';
          dspVisb = *Off;
       Else;
          dspVisb = *On;
       Endif;

       scInMode = @@Mode;

       dspEndrSub = *On;
       clear dbrrn1;

       Setll (xxCo#:xxFnd) pen00;
       Reade(n) (xxCo#:xxFnd) pen00;
       Dow Not %Eof(wtenl);

          //Endorsement Form
          ScEndForm = ENform;

          //Endorsement Form Description
          scEndDesc = %trim(ENdsc2);

          //Edition Date
          scEdtnDt = CvtToDate(ENeddt);

          //Adoption Date
          scAdptDt = CvtToDate(ENadte);

          //Status
          Select;
          When ENstat = 'I';
             scStatus = 'Inactive';
          When ENstat = 'A';
             scStatus = 'Active';
          Endsl;

          //Endorsement Form State
          scEndrSt = ENst;

          //Manual/Auto
          Select;
          When ENmnau = 'M';
             scManAuto = 'Manual';
          When ENmnau = 'A';
             scManAuto = 'Auto';
          Endsl;

          //Replacement Form
          scReplFrm = ENrpfm;

          //Replacement Date
          scReplDt = CvtToDate(ENrpdt);

          //Generate From PDF File
          If ENgpdf = 'Y';
             scGnrFPDF = 'Yes';
          Else;
             scGnrFPDF = 'No';
          Endif;

          //Select Checkbox
          //If inIncludeFrm = 'Include' or scSelect = 'Y';
          If inIncludeFrm = 'Include';
             scSelect = 'Y';
             nxchgEndr = *on;
          Else;
             scSelect = 'N';
             nxchgEndr = *off;
          Endif;

          Select;
          When @@mode = '1';
            isLnkVisb = *Off;
          Other;
            isLnkVisb = *On;
          Endsl;

          scEndrRrn# = dbrrn1;
          rrnEndr = rrnEndr + 1;

          clear scEndRowSl;
          If (rrnEndr = rrnEndrRow) or (rrnEndr = 1 and rrnEndrRow = 0);
             scEndRowSl = 'A';
          Endif;

          Write sflEndrFrm;
          Reade(n) (xxCo#:xxFnd) pen00;

       Enddo;

       rrnEndr = 1;
       Clear inInclude;

      /End-Free

     p LoadEndrGrid    e

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

      /Free
       xxCo#  =  @@Co#;
       xxFnd  =  @@Fnd;

       // Read Fund Master File
       Chain (xxCo#:xxFnd) pfn01;

     // Update Action Panel Info (Group Name, Group Number)
       scCompName = %trim(FNname);
       scGrpNum = FNfnd;

       // Process until exit button is pressed.
       Dou ExitNow = *On ;
          If doLoad = *on;
             ClrSubfiles();                   //Clear Sub-files
             LoadEndrGrid(inInclude);         //Load Endorsement Form Grid
             LoadEndrDtl('N');
          Endif;
          If rrnPosEndr <> 0 and rrnPosEndr <= rrnEndrRow;
             rrnEndr = rrnPosEndr;
          Else;
             rrnEndr = 1;
          Endif;
          saveRrn = rrnEndrRow;

          // Process Endorse Screen
          Exsr ExfmtScreens;
          If rrnEndrRow = 0 and saveRrn <> 0;
             rrnEndrRow = saveRrn;
          Endif;

          doLoad = *On;
          Select;
          // do nothing - catch tab click event
          When tabClick = *on;
             tabClick = *off;
          When BtnRefresh = *On;
             BtnRefresh =*Off;
             inInclude = ' ';
          When btnEndrAdd = *on;
             EditAddEndr('A');
          When btnEndIncl = *on;
             inInclude = 'Include';
          When btnEndExcl = *on;
             inInclude = ' ';
          When btnPerAct = *on;
             ProcPerAct();
          When btnApExAdd = *on;
             EditAddApExSt('A');
          When btnSfRowCl = *on;
             LoadEndrDtl('Y');
             doLoad = *Off;
          When btnEndrCan = *on;
             Leave;
          Other;

             If callpgm = *on;
                ExitNow = *On;
             Endif;

             ProcOther();
          EndSl;

       Enddo;
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       // Initialize Screen fields
       scSelAct = '--- Select List Action ---';

       visGrpCfg = *on;    // Show Cancel Button
       callpgm = *off;
       ChkStack('WTGRPPOLCG':CallPgm);
       If callpgm = *on;
          visGrpCfg = *off;
       Endif;

       Select;
       When TabIndex = 0;
          TabHeight = 850;
          APnlHeight = TabHeight + 50;
          If callpgm = *off;
          Write rcdGrpPnl;
          Write rcdTabEndr;
          EndIf;
          Exfmt rcdEndrFrm;
          If callpgm = *off;
          Read rcdGrpPnl;
          Read rcdTabEndr;
          //*** Beg Add ***** 01/17/22 *******************************
          // Copy Configuration
          If btnShowWin = *on;
             PopupWindow(txtShowWin:scActRRN:'GS':txtWinData);
             btnShowWin = *off;
          Endif;
          //*** End Add ***** 01/17/22 *******************************
          EndIf;
       EndSl;

       Endsr;

      /End-Free

     p Main            e
       //===================================================================
       // ProcChgSts - Change status Option
       //===================================================================
     p ProcChgSts      b
     d ProcChgSts      pi
      /Free

       ReadC sflEndrFrm;
       Dow Not %Eof;
          If scSelect = 'Y';
             Chain scEndrRrn# pen0p;
             If %found(wtenp);
                If ENstat = 'I';
                   ENstat = 'A';
                Else;
                   ENstat = 'I';
                EndIf;
                Update pen0p %fields(ENstat);
             Endif;
          Endif;
          ReadC sflEndrFrm;
       Enddo;

      /End-Free
     p ProcChgSts      e
       //===================================================================
       // ProcessCopy - Process Copy Option
       //===================================================================
     p ProcessCopy     b
     d ProcessCopy     pi
      /Free
        index = 0;
        clear arrEndrs;
        ReadC sflEndrFrm;
        Dow Not %Eof;
          If scSelect = 'Y';
             nxchgEndr = *on;
             update sflEndrFrm;
          //*** Beg Del ***** 03/27/23 *****************************************
          //*** Endif;
          //*** End Del ***** 03/27/23 *****************************************
          dsEndrs.EndrFrm = ScEndForm;
          dsEndrs.EndrRrn = scEndrRrn#;
          index += 1;
          arrEndrs(index) = dsEndrs;
          //*** Beg Add ***** 03/27/23 *****************************************
          Endif;
          //*** End Add ***** 03/27/23 *****************************************
          ReadC sflEndrFrm;
        Enddo;

        //*** Beg Chg ***** 12/27/23 *****************************************
        If index >= 1;
           ptrEndFrm = %Addr(arrEndrs);
           inData = 'Copy Endorsements';
           GroupCopySetup(@@rrn:inData:ptrEndFrm);
        Endif;
        //*** End Chg ***** 12/27/23 *****************************************

       //*** Beg Del ***** 01/17/22 ***********************************
       //Following code for Endorsement copy process is moved to WPIGRPPOP.GroupCopySetup
     d*  ExitNow        s               n   inz
      *
       // Dcl-S scToGroup# packed(3:0) inz;

       //Clear RcdCopyEnd;
       //Exsr ClrSflDupEnd;
       //
       //scFrGroup# = scGroup;
       //visDupEnd = *off;
       //visNDupEnd = *off;
       //BtnReplEnd = *off;
       //BtnSkipDup = *off;
       //BtnCpyEnd  = *off;
       //Dou ExitNow = *on;
       //
       //   Exfmt rcdCopyEnd;
       //   Select;
       //   When btnTGrpClk = *on;
       //      Clear sflDupEnd;
       //      Exsr ClrSflDupEnd;
       //      ReadC sflEndrFrm;
       //      Dow Not %Eof;
       //         If scSelect = 'Y';
       //            nxchgEndr = *on;
       //            update sflEndrFrm;
       //            //scError1 = '9';
       //            ReadC sflToGrp;
       //            Dow Not %Eof;
       //               If scSelToGrp = 'Y';
       //                  //scError1 = *blank;
       //                  scToGroup# = scToGrp;
       //                  nxChgToGrp = *On;
       //                  update sflToGrp;
       //                  chain(n) (@@co#:scToGroup#:scEndForm) pen00;
       //                  If %found(wtenl);
       //                     ScDEndForm = ENForm;
       //                     ScDEndDesc = ENdsc2;
       //                     ScDEndrSt  = ENSt;
       //                     scDEndRrn# = dbrrn1;
       //                     scDEndGrp =  ENfnd;
       //                     rrnDEnd = rrnDEnd + 1;
       //                     Write sflDupEnd;
       //                  Endif;
       //               Endif;
       //               ReadC sflToGrp;
       //            Enddo;
       //         Endif;
       //         ReadC sflEndrFrm;
       //      Enddo;

       //      If rrnDEnd > 0;
       //         visDupEnd = *on;
       //         visNDupEnd = *off;
       //         visNDupEnd = *on;
       //         visDupEnd = *off;
       //      Endif;
       //      rrnDEnd = 1;
       //   Endsl;
       //Enddo;
       //
       //Begsr ClrSflDupEnd;
       //   RrnDEnd = 0;
       //   dspDEnd = *Off;
       //   dspDEndCtl = *Off;
       //   clrDEnd = *On;
       //   Write rcdCopyEnd;
       //   clrDEnd = *Off;
       //Endsr;

       //Begsr ClrSflToGrp;
       //   RrnToGrp = *Zeros;
       //   dspToGrpSf = *Off;
       //   dspToGrpCt = *Off;
       //   clrToGrp = *On;
       //   Write rcdCopyEnd;
       //   dspToGrpSf = *On;
       //   dspToGrpCt = *On;
       //   clrToGrp = *Off;
       //Endsr;
       //
       //Begsr LoadSflToGrp;
       //  Setll *loval Wmfnl;
       //  Read Wmfnl;
       //  Dow not %Eof(Wmfnl);
       //     If FNfnd <> scFrGroup#;
       //        chain (FNco#:FNfnd) Wdf2l;
       //        scSelToGrp = 'N';
       //        scToGrp = FNfnd;
       //        scToGrpDes = %Editc(FNfnd:'X') + ' - ' + FNname;
       //        If %found(Wdf2l);
       //           scCar# = F2Car#;
       //        Else;
       //           scCar# = *Zeros;
       //        Endif;
       //        RrnToGrp = RrnToGrp + 1;
       //        Write SflToGrp;
       //     Endif;
       //     Read Wmfnl;
       //  Enddo;
       //
       //  If RrnToGrp = *Zeros;
       //     DspToGrpSf = *Off;
       //     VisToGrp = *Off;
       //  Else;
       //     RrnToGrp = 1;
       //     VisToGrp = *On;
       //     TopToGrp = 185;
       //  Endif;
       //Endsr;
       //*** End Del ***** 01/17/22 ***********************************
      /End-Free
     p ProcessCopy     e

       //===================================================================
       // ProcOther - Process Other Options
       //===================================================================
     p ProcOther       b
     d ProcOther       pi

      /Free

       //Edit Endorse Record
       ReadC sflEndrFrm;
       Dow not %Eof;
          //process Edit
          Select;
          When scEndrEdt  = *On;
             scEndrEdt  = *Off;
             ExitNow = *Off;
             ##Rrn# = scEndrRRN#;
             EditAddEndr('C':##Rrn#);
          //process Delete
          When scEndrDel = *On;
             scEndrDel  = *Off;
             ExitNow = *Off;
             ##Rrn#  = scEndrRRN#;
             EditAddEndr('D':##Rrn#);
          //*** Beg Add ***** 12/27/23 *****************************
          //process Copy option
          When scEndrCpy  = *On;
             scEndrCpy  = *Off;
             ExitNow = *Off;
             ##Rrn# = scEndrRRN#;
             EditAddEndr('P':##Rrn#);
          //*** End Add ***** 12/27/23 *****************************
          Endsl;
          ReadC sflEndrFrm;
       Enddo;

       //Edit Approved/Excluded States Record
       ReadC sflApExSt;
       Dow not %Eof;
          //process Edit
          Select;
          When scAEStEdt = *On;
             scAEStEdt  = *Off;
             ExitNow = *Off;
             ##Rrn# = scAERRN#;
             EditAddApExSt('C':##Rrn#);
          //process Delete
          When scAEStDel = *On;
             scAEStDel  = *Off;
             ExitNow = *Off;
             ##Rrn#  = scAERRN#;
             EditAddApExSt('D':##Rrn#);
          //*** Beg Add ***** 01/24/23 ******************************
          //process Copy
          When scAEStCopy = *On;
             scAEStCopy  = *Off;
             ExitNow = *Off;
             ##Rrn#  = scAERRN#;
             CopyApExSt(##Rrn#);
          //*** End Add ***** 01/24/23 ******************************
          Endsl;
          ReadC sflApExSt;
       Enddo;

      /End-Free
     p ProcOther       e
       //===================================================================
       // ProcPerAct - Process Performed Action from the action dropdown
       //===================================================================
     p ProcPerAct      b
     d ProcPerAct      pi
      /Free
         Select;
         When scSelAct = 'Change Status';
            ProcChgSts();
         When scSelAct = 'Copy';
            ProcessCopy();
         Endsl;

      /End-Free
     p ProcPerAct      e
       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /Free

       Select;
       When @@mode = '1';
          dspVisb = *Off;
       Other;
          dspVisb = *On;
       Endsl;

       Select;
       When @@mode = '1';
          scActMode = 'D';
       Other;
          scActMode = 'C';
       Endsl;

       // Load Action panel with RRN
       xxRrn = %int(@@rrn);
       scActRrn=xxRrn;

       // Load Action panel with the Company Name.
       intCo# = @@Co#;

       // Load Fund Name
       scGroup = @@Fnd;
       scName = GetGrpName(ScGroup);

       // Get the client id
       GetClientID(##CltId);

       // Get Company name
       scCompName = GetCompNam(intCo#);

       If @@Mode = '1';
         ChoSelAct = ' ';
       Else;
         ChoSelAct = '--- Select List Action ---,Copy,Change Status';
       Endif;
       scSelAct = '--- Select List Action ---';

       // Determine if this module get spool file and/or XML output
       procType = 'Policy Page';
       isUseSpoolOut = UseSpoolOut(procType);
       isUseXML = UseScriptura(procType) Or Use3rdXml(procType);
       if isUseXML;
         isScriptUsed = 'Y';
       else;
         isScriptUsed = 'N';
       Endif;

       visEndrFrm = *on;
       If isScriptUsed='Y' And isUseSpoolOut = *Off;
          skipDocEdit = *On;
          visEndrFrm = *off;
       EndIf;

       tabClick = *Off;
       ##Rrn# = 0;
       Return;

      /End-Free

     p ProgramStart    e
       //===================================================================
       // Validate Approved/Excluded State - Edit/Add State
       //===================================================================
     p ValidApExSt     b
     d ValidApExSt     pi              n
     d  inMode                        1    const
     d  inApExRrn                     9  0

     disError          s               n   inz

      /Free

       isError = *Off;
       msApExSt   = *blanks;
       erApExSt   = *off;
       msApExEfDt = *blanks;
       erApExEfDt = *off;
       msApExExDt = *blanks;
       erApExExDt = *off;
       msApExFlg  = *blanks;
       erApExFlg  = *off;
       msAprExSt  = *blanks;
       erAprExSt  = *off;

       //State field cannot be blank
       If SCApExSt = ' ';
          msApExSt = 'State is required';
          erApExSt = *on;
          isError = *on;
       Endif;

       //Check to see if Approved/Excluded State already exist
       If isError = *off and inMode = 'A';
          Setll (xxCo#:scApExEndr:SCApExSt) pes00;
          If %Equal(wtesl);
             msApExSt = 'Approved/Excluded State already Exist';
             erApExSt = *on;
             isError = *on;
          Endif;
       Endif;

       //Approved/Excluded flag cannot be blank
       If SCApExFlg = ' ';
          msApExFlg = 'Approved/Excluded flag is required';
          erApExFlg = *on;
          isError = *on;
       Endif;

       //Effective date must be valid if entered
       If scApExEfDt <> *zero;
          scApExEfDt = DtCheck(scApExEfDt);
          If scApExEfDt = *zero;
             msApExEfDt = 'Invalid date.';
             erApExEfDt = *on;
             isError = *on;
          Else;
             //Effective date must be greater than or equal to the edition date
             If dtCYMD(scApExEfDt) < ENeddt;
                msApExEfDt = 'Effective date cannot be less '  +
                 'than the Edition date.';
                erApExEfDt = *on;
                isError = *on;
             Endif;
          Endif;
       Endif;

       //Expiration date must be valid if entered
       If scApExExDt <> *zero;
          scApExExDt = DtCheck(scApExExDt);
          If scApExExDt = *zero;
             msApExExDt = 'Invalid date.';
             erApExExDt = *on;
             isError = *on;
          Else;
             //Expiration date must be greater than or equal to the effective date
             If isError = *off and scApExExDt <> 0 and scApExEfDt <> 0 and
              dtCYMD(scApExExDt) < dtCYMD(scApExEfDt);
                msApExExDt = 'Expiration date cannot be less '  +
                 'than the Effective date.';
                erApExExDt = *on;
                isError = *on;
             Endif;
          Endif;
       Endif;

       //Check to be sure screen has NOT been updated since entry
       If isError = *off;
          Chain(n) inApExRrn pes0p;
          If ESdt > saveESdt Or (ESdt = saveESdt AND EStim > saveEStm);
             isError = *On;
             erAprExSt = *On;
             msAprExSt = 'WC30000';
          Endif;
       EndIf;

       Return not isError;

      /End-Free

     p ValidApExSt     e
       //===================================================================
       // Validate Endorsement Form - Edit/Add Endorsement Form
       //===================================================================
     p ValidEndFrm     b
     d ValidEndFrm     pi              n
     d  inMode                        1    const
     d  inFrmRrn                      9  0

     disError          s               n   inz

      /Free

       isError = *Off;

       //Form field cannot be blank
       If scAEndFrm# = ' ' and skipDocEdit = *off;
          msAEndFrm# = 'Endorsement Form# is required';
          erAEndFrm# = *on;
          isError = *on;
       Endif;

       //Check to see if form already exist
       //*** Beg Chg ***** 12/27/23 *****************************
       //*** If isError = *off and inMode = 'A';
       If isError = *off and (inMode = 'A' or inMode = 'P');
       //*** End Chg ***** 12/27/23 *****************************
          Setll (xxCo#:xxFnd:scAEndFrm#) pen00;
          If %Equal(wtenl);
             msAEndFrm#= 'Endorsement Form# already Exist';
             erAEndFrm# = *on;
             isError = *on;
          Endif;
       Endif;

       //Description cannot be blank
       If scAEndFDsc = ' ';
          msAEndFDsc = 'Endorsement Description is required';
          erAEndFDsc = *on;
          isError = *on;
       Endif;

       //Edition date must be valid if entered
       If scAEdtnDt = *zero;
          msAEdtnDt = 'Edition date is required.';
          erAEdtnDt = *on;
          isError = *on;
       Else;
          scAEdtnDt = DtCheck(scAEdtnDt);
          If scAEdtnDt = *zero;
             msAEdtnDt = 'Invalid date.';
             erAEdtnDt = *on;
             isError = *on;
          Endif;
       Endif;

       //Adoption date must be valid if entered
       If scAAdptDt <> *zero;
          scAAdptDt = DtCheck(scAAdptDt);
          If scAAdptDt = *zero;
             msAAdptDt = 'Invalid date.';
             erAAdptDt = *on;
             isError = *on;
          Else;
             //Adoption date must be greater than or equal to the edition date
             If dtCYMD(scAAdptDt) < dtCYMD(scAEdtnDt);
             msAAdptDt = 'Adoption date can''t be less '  +
                         'than the Edition date.';
             erAAdptDt = *on;
             isError = *on;
             Endif;
          Endif;
       Endif;

       // Document path must begin and end with a '/' character.
       If isError = *off and scCltTpPth <> ' ';
          strSlash  = %subst(scCltTpPth:1:1);
          strSlash2 = %subst(scCltTpPth:%len(%trim(scCltTpPth)):1);
          If strSlash <> '/' or strSlash2 <> '/';
             msCltTpPth = 'Document path must begin and end with a ''/''.';
             erCltTpPth = *on;
          Endif;
       Endif;

       //Manual/Auto cannot be blank
       If scAManAuto = ' ';
          msAManAuto = 'Manual/Auto is required';
          erAManAuto = *on;
          isError = *on;
       Endif;

       //Single/Multiple form cannot be blank
       If scSingMult = ' ';
          msSingMult = 'Single/Multple Form is required';
          erSingMult = *on;
          isError = *on;
       Endif;

       //Manual/Auto Attach and Print form Edits
       FormWC301 = *off;
       If %subst(scAEndFrm#:1:8) = 'WC000301';
          FormWC301 = *on;
       Endif;

       Select;
       //If Manual Form and printing
       When scAManAuto = 'M' and SCPrntFrm = 'Y';
          Select;
          When scFolder = *Blank and skipDocEdit = *off;
             msFolder = 'Folder is required.';
             erFolder = *on;
             isError = *on;
          When scAtchPgm <> *Blank and FormWC301 <> *on;
             msAtchPgm = 'Attachment program must be blank.';
             erAtchPgm = *on;
             isError = *on;
          When scPrntPgm = *Blank;
             msPrntPgm = 'Print program is required.';
             erPrntPgm = *on;
             isError = *on;
          Endsl;
          If skipDocEdit = *off and isError = *off;
             Exsr ValidDoc;
          Endif;

       //If Auto form and printing
       When scAManAuto = 'A' and SCPrntFrm = 'Y';
          Select;
          When scFolder = *Blank and skipDocEdit = *off;
             msFolder = 'Folder is required.';
             erFolder = *on;
             isError = *on;
          When scAtchPgm = *Blank;
             msAtchPgm = 'Attachment program is required.';
             erAtchPgm = *on;
             isError = *on;
          When scPrntPgm = *Blank;
             msPrntPgm = 'Print program is required.';
             erPrntPgm = *on;
             isError = *on;
          When scAdInfPgm <> *Blank and FormWC301 <> *on;
             msAdInfPgm= 'Additional Information program must be blank.';
             erAdInfPgm= *on;
             isError = *on;
          Endsl;
          If skipDocEdit = *off and isError = *off;
             Exsr ValidDoc;
          Endif;

       //If Manual form and not printing
       When scAManAuto = 'M' and SCPrntFrm = 'N';
          Select;
          When scAtchPgm <> *Blank and FormWC301 <> *on;
             msAtchPgm = 'Attachment program must be blank.';
             erAtchPgm = *on;
             isError = *on;
          When scPrntPgm <> *Blank;
             msPrntPgm = 'Print program must be blank.';
             erPrntPgm = *on;
             isError = *on;
          Endsl;

       //If Auto form and not printing
       When scAManAuto = 'A' and SCPrntFrm = 'N';
          Select;
          When scAtchPgm = *Blank;
             msAtchPgm = 'Attachment program is required.';
             erAtchPgm = *on;
             isError = *on;
          When scPrntPgm <> *Blank;
             msPrntPgm = 'Print program must be blank.';
             erPrntPgm = *on;
             isError = *on;
          When scAdInfPgm <> *Blank and FormWC301 <> *on;
             msAdInfPgm= 'Additional Information program must be blank.';
             erAdInfPgm= *on;
             isError = *on;
          Endsl;
       Endsl;

       //Folder Name must be valid if Entered
       If scFolder <> *blank and skipDocEdit = *off;
          Exsr ValidFolder;
       Endif;

       If isError = *off;
          //Attachment program must be valid.
          If scAtchPgm <> *blank;
             outName = scAtchPgm;
             outType = '*PGM';
             outRtcd = ' ';
             ChkObject(outName:outType:outRtcd);
             If outRtcd = '1';
                msAtchPgm = 'Program ' + %trim(scAtchPgm) +
                 ' was not found within your current library list.';
                erAtchPgm = *on;
                isError = *On;
             Endif;
          Endif;

          //Print program must be valid.
          If scPrntPgm <> *blank;
             outName = scPrntPgm;
             outType = '*PGM';
             outRtcd = ' ';
             ChkObject(outName:outType:outRtcd);
             If outRtcd = '1';
                msPrntPgm = 'Program ' + %trim(scPrntPgm) +
                 ' was not found within your current library list.';
                erPrntPgm = *on;
                isError = *On;
             Endif;
          Endif;

          //Additional input program must be valid.
          If scAdInfPgm <> *blank;
             outName = scAdInfPgm;
             outType = '*PGM';
             outRtcd = ' ';
             ChkObject(outName:outType:outRtcd);
             If outRtcd = '1';
                msAdInfPgm = 'Program ' + %trim(scAdInfPgm) +
                 ' was not found within your current library list.';
                erAdInfPgm = *on;
                isError = *On;
             Endif;
          Endif;

          //Business Type Flag is required if Business Type value is selected
          If scBusTypV <> *blank and scBusTypF = *Blank;
             msBusTypF = 'Include or Exclude is required when Business type -
              is selected.';
             erBusTypF = *on;
             isError = *On;
          Endif;

          //Check if Replacement form is a valid form
          If scReplForm <> *blank;
             Setll (xxCo#:xxFnd:scReplForm) pen00;
             If not %Equal(wtenl);
                msReplForm= 'Replacement form ' + %trim(scReplForm) +
                 ' not found in Endorsement form table for this group.';
                erReplForm = *on;
                isError = *on;
             Endif;
          Endif;

          //Replacement date must be valid if entered
          If scReplDate <> *zero;
             scReplDate = DtCheck(scReplDate);
             If scReplDate = *zero;
                msReplDate = 'Invalid date.';
                erReplDate = *on;
                isError = *on;
             Endif;
          Endif;

          If isError = *off;
             If scReplForm <> *blank and scReplDate = *zero;
                msReplDate= 'Replacement date is required if ' +
                 'Replacement form is entered.';
                erReplDate = *on;
                isError = *on;
             Endif;

          Endif;         //isError = *off



          Select;
          When scUnExpDt = 0;
          Other;
             scUnExpDt = DtCheck(scUnExpDt);
             If scUnExpDt = *zero;
                msUnExpDt = 'Invalid date.';
                erUnExpDt = *on;
                isError = *on;
             Endif;
             If DtCymd(scUnExpDt) < DtCymd(scAEdtnDt) and isError = *off;
                msUnExpDt = 'Unexpired date can be prior to the edition date.';
                erUnExpDt = *on;
                isError = *on;
             Endif;
          Endsl;
       Endif;                //isError = *off

       //Check to be sure screen has NOT been updated since entry
       If isError = *off;
          Chain(n) inFrmRrn pen0p;
          If ENdt > saveENdt Or (ENdt = saveENdt AND ENtime > saveENtm);
             isError = *On;
             erEndFrm = *On;
             msEndFrm = 'WC30000';
          Endif;
       EndIf;

       Return not isError;

       //--------------------------------------------
       // Subroutines
       //--------------------------------------------

       //Validate Document
       Begsr ValidDoc;
          Exsr ValidFolder;
          If isError = *off;
             outRtcd1 = *blank;
             outFolder2 = scFolder;
             If %subst(scAEndFrm#:9:7) = *blank;
                outDocId2 = scAEndFrm#;
             Else;
                outDocId2 = %subst(scAEndFrm#:1:8) + '.' +
                 %trim(%subst(scAEndFrm#:9:7));
             Endif;
          Endif;
          ValidateDoc(outDocId2:outFolder2:outRtcd2);
          If outRtcd2 = '1' and outDocId2 <> 'MID-YEAR';
             //*** Beg Chg ***** 12/27/23 *****************************
             //*** If inMode = 'A';
             If inMode = 'A' or inMode = 'P';
             //*** End Chg ***** 12/27/23 *****************************
                msFolder = 'Document not found in specified folder.';
                erFolder = *on;
                isError = *on;
             Else;
                msFolder = 'Form not found in folder. Change folder -
                 or change to NOT print.';
                erFolder = *on;
                msPrntFrm = 'Form not found in folder. Change folder -
                 or change to NOT print.';
                erPrntFrm = *on;
                isError = *on;
             Endif;
          Endif;

       Endsr;

       //Validate Folder
       Begsr ValidFolder;
          outFolder1 = scAEndFrm#;
          outRtcd1 = *blank;
          ValidateFolder(outFolder1:outRtcd1);
          If outRtcd1 = '1' or outRtcd1 = '2';
             msFolder = 'Folder Name is Invalid';
             erFolder = *on;
             isError = *on;
          Endif;

       Endsr;

      /End-Free

     p ValidEndFrm     e

       //===================================================================
