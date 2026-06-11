     hCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     h*****************************************************************
     h/title SR48323I- WI Designated Named Insured Cancellation Input
     h/define   PROFOUNDUI
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description:  Collects additional information for form WC480323
     h*
     h*  Called from: SR48323
     h*  Calls to:    None
     h*
     h* Created   : 03/07/23
     h* Programmer: C:291
     h* Project   : WC2896
     h*****************************************************************
     H*                                                               *
     H* This program will display a grid of the Additional Named      *
     H* Insured on the policy with an expire link.                    *
     H* The expire link will allow a deletion date to be entered and  *
     H* a cancellation reason to be selected and update the Additional*
     H* Named Insured record.                                         *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     fSR48323F  cf   e             workstn extfile('SR48323F ')
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflNmIns:rrnNmIn)
     fWtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdPolPnl)
     f                                     include(rcdQtePnl)
     f                                     include(rcdComPnl)

     fWmeml     if   e           k disk
     fWdell     if   e           k disk
     fWmrsl     if   e           k disk
     fWmanl     if   e           k disk    rename(pan01:pan00)
     f                                     Infds(dbfds1)
     fWmanp     uf a e             disk    rename(pan01:pan0p)
     fwtgtl     if   e           k disk

     d psds          esds                  extname(szq1p)

     d dbfds1          ds
     d  dbrrn1               397    400b 0
     d                 ds

     d @@prds          s             40
     d @@Mode          s              1
     d @@Co#           s              3
     d @@Fnd           s              3
     d @@Emp#          s              9
     d @@Div           s              5
     d @@Fyr           s              3
     d xxCo#           s                   like(ANco#)
     d xxFnd           s                   like(ANfnd)
     d xxEmp#          s                   like(ANemp#)
     d xxDiv           s                   like(ANdiv)
     d xxFyr           s                   like(ELfyr)
     d scfnd           s                   like(ELfnd)
     d scfyr           s                   like(ELfyr)
     d scemp#          s                   like(ELemp#)
     d scdiv           s                   like(ELdiv)
     d inmode          s              1
     d intCo#          s              3  0
     d intRrn          s              9  0
     d xxRrn           s              9  0
     d isMenuAvbl      s               n   inz(*on)
     d isValid         s              1n   inz(*off)
     d isError         s              1n   inz(*off)
     d xxForm          c                   Const('WC480323  ')
     d xxKey           s                   like(GTkey) inz
     d xxTbId          s                   like(GTtbid) inz
     d xxCxDate7       s              7  0
      *****************************************************************
      * Prototype Declaration
      *****************************************************************
      * Clear Subfiles
     d ClrSubFiles     pr

      * Edit/Del/Add Endorse Record
     d ExpireNamedIns  pr
     d inRRN                          9  0 options(*nopass)

      * Load Endorse Value grid
     d LoadNmInsGrid   pr

      * Main Processing
     d Main            pr

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

       rrnNmIn = 0;
       clrNmIn = *On;
       dspNmInSub  = *Off;
       dspNmInCtl  = *Off;
       Write rcdEndr;

      /End-Free

     p ClrSubfiles     e
       //=======================================================================
       // ExpireNamedIns  - Expire Named Insured Record with Cancel Reason code
       //=======================================================================
     p ExpireNamedIns  b
     d ExpireNamedIns  pi
     d inRRN                          9  0 options(*nopass)

      /Free

       // To get the company number
       intCo# = UserCo#(q1user);

       // Program Mode.
       If %parms() > 0;
          intRrn = inRrn;
       Endif;

       Clear RcdAdEndr;

       BtnSaveRcd = *off;
       btnCanRcd = *off;

       scTobSel = 'GTCO# = ' + %char(xxCO#) +
        ' AND GTTBID = ''WICX'' AND GTSTAT = ''A''';

       // Processing to Edit Endorse Record
       If intRrn <> 0;
          Chain(n) intRrn pan0p;
          If %Found(wmanp);
             scANCxDtE = dtMDYY(ANdldt);
             scANCxRsnE = ANcxRsn;
          Endif;
       Endif;

       Dou (BtnCanRcd  = *On) or (BtnSaveRcd = *on and isValid = *off);
          // Processing to Add Endorse Record
          Exfmt rcdAdEndr;
          isValid = *off;
          Select;
          When BtnSaveRcd = *On;
             Validate() ;
             If isValid = *off;
                Chain intRrn pan0p;
                If %Found(wmanp);
                   ANdldt = dtCYMD(scANCxDtE);
                   ANCxRsn = scANCxRsnE;
                   ANCxFyr = xxFyr;
                   If ANdldt = 0;
                      Clear ANCxRsn;
                      Clear ANCxFyr;
                   Endif;
                   If ANCxRsn = *Blanks;
                      Clear ANCxFyr;
                   Endif;
                   ANpgm  = q1pgm;
                   ANuser = q1user;
                   ANdt   = DateTo7(SystemDate);
                   ANtime = TimeTo6(SystemTime);
                   Update pan0p;
                Endif;
             Endif;
          When BtnCanRcd = *on;
             BtnCanRcd = *off;
             Leave;
          Endsl;
       Enddo;

       intRrn  = *zeros;
       Return;

      /End-Free

     p ExpireNamedIns  e
       //===================================================================
       // Load Additional Named Insured Grid
       //===================================================================
     p LoadNmInsGrid   b
     d LoadNmInsGrid   pi

      /Free

       // Clear the subfile.
       Clear sflNmIns;
       clrNmIn = *Off;

       If inMode = 'D';
          dspVisb = *Off;
       Else;
          dspVisb = *On;
       Endif;

       dspNmInSub = *On;
       dspNmInCtl = *On;
       clear dbrrn1;

       Setll (xxCo#:xxFnd:xxEmp#) pan00;
       Dow Not %Eof(wmanl);
          Reade (xxCo#:xxFnd:xxEmp#) pan00;
          If %Eof;
             Leave;
          Endif;
          Chain (xxCo#:xxFnd:xxEmp#:ANDiv:xxFyr) pel01;
          If ANefdt >= ELptdt;
             Iter;
          Endif;
          If ANdldt <> 0 and ANdldt < ELpfdt;
             Iter;
          Endif;
          Chain (Elco#:ELrtcd) prs01;
          Select;
          When Not(%Found);
             Iter;
          When RSjrst <> 'WI';
             Iter;
          Endsl;

          scANname  = ANname + ANnam2;
          scANfein  = ANfein;
          scANefdt  = CvtToDate(ANefdt);
          scANexdt  = CvtToDate(ANdldt);
          scANCxRsn = ANCxRsn;
          xxTbId = 'WICX';
          xxKey  = ANCxRsn;
          Chain (xxCo#:xxTbiD:xxKey) WTGTL;
          If %found(WTGTL);
             scANCxRsD = %Trim(GTdesc);
          Else;
             scANCxRsD = *blanks;
          Endif;

          scNmInRrn# = dbrrn1;
          rrnNmIn = rrnNmIn + 1;

          clear scEndRowSl;
          If (rrnNmIn = rrnNmInRow) or (rrnNmIn = 1 and rrnNmInRow = 0);
             scEndRowSl = 'A';
          Endif;

          Write sflNmIns;
       Enddo;

       rrnNmIn = 1;

      /End-Free

     p LoadNmInsGrid   e
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
       Chain (xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr) pel01;

       // Load Policy Action Bar
       LoadActionBar(isMenuAvbl);

       // Process until exit button is pressed.
       Dou ExitNow = *On ;

          // Clear Sub-files
          ClrSubfiles();

          // Load Endorse Grid
          LoadNmInsGrid();
          If rrnNmInPos <> 0 and rrnNmInPos <= rrnNmInRow;
             rrnNmIn = rrnNmInPos;
          Else;
             rrnNmIn = 1;
          Endif;
          If rrnNmInRow <> 0;
             scInRow = rrnNmInRow;
          Endif;

          // Process Endorse Screen
          Exsr ExfmtScreens;

          Select;
          When tabClick = *On;
             tabClick = *Off;
          When BtnRefresh = *On;
             BtnRefresh =*Off;
          When btnEndrCan = *On;
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
       // ProcOther - Process Other Options
       //===================================================================
     p ProcOther       b
     d ProcOther       pi

      /Free

       // Edit Endorse Record
       ReadC sflNmIns;
       Dow Not %Eof;
          If scNmInExp  = *On;
             xxRrn = scNmInRrn#;
             ExpireNamedIns(xxRrn);
             scNmInExp = *Off;
          Endif;
          ReadC sflNmIns;
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

       tabClick = *Off;
       xxRrn = 0;
       scInRow = 1;
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
         mgANCxDtE  = ' ';
         erANCxDtE  = *off;
         mgANCxRsnE = ' ';
         erANCxRsnE = *off;

         If scANCxDtE <> 0;
            scANCxDtE = DtCheck(scANCxDtE);
            If scANCxDtE = *zero;
               mgANCxDtE  = 'Invalid Cancellation Date.';
               erANCxDtE  = *on;
               isError = *On;
            Else;
               xxCxDate7 = dtCymd(scANCxDtE);
            Endif;
         Endif;

         Select;
         When isError = *On;
         When scANCxDtE = 0 and scANCxRsnE <> *Blank;
            mgANCxDtE = 'Cancellation Date Must be Entered.';
            erANCxDtE = *on;
            isError = *On;
         When scANCxDtE <> 0 and dtCymd(scANCxDtE) < ANefdt;
            mgANCxDtE  = 'Cancellation Date must be greater than or ' +
             'equal to the Effective date of the Additional Named Insured.';
            erANCxDtE  = *on;
            isError = *On;
         When scANCxDtE <> 0 and
          (dtCymd(scANCxDtE) < ELpfdt or dtCymd(scANCxDtE) > ELptdt);
            mgANCxDtE  = 'Named Insured must be cancelled within ' +
             'the current Policy year.';
            erANCxDtE  = *on;
            isError = *On;
         When scANCxDtE <> 0 and scANCxRsnE = *Blank;
            mgANCxRsnE = 'Cancellation Reason Must be Entered.';
            erANCxRsnE = *on;
            isError = *On;
         Endsl;

         If isError = *On;
            isValid = *On;
         Endif;

      /End-Free

     p Validate        e
       //===================================================================
      * Utility policy procedure definitions
      /COPY SCOPYPOLPI
