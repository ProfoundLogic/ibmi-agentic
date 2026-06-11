     hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h*****************************************************************
     h/TITLE 150WBV - Prompt for Billing Invoices
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     H*****************************************************************
     H/TITLE WTRPTBLIN - Prompt for Billing Invoices
     h*****************************************************************
     h* Revisions                                                     *
     H*                                                               *
     h* 02/13/18  C:267  Project: 150WBV                              *
     H*                  Call WTRPTBLINC to submit the batch job      *
     H*                                                               *
     h*****************************************************************

     fWTRPTBLIN cf   e             workstn extfile('WTRPTBLIN')
     f                                     handler('PROFOUNDUI(HANDLER)')
     fWtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)
     f*
     fsmcol     IF   E           k DISK    Rename(PCO01:PCO00)
     fWdelp     if   e             disk    Rename(pel01:pel0p)

     D*LO              C                   CONST('abcdefghijklmnopqrs-
     D*                                    tuvwxyz')
     D*UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D*                                    TUVWXYZ')

     dpsds           esds                  extname(szq1p)

      *
     d WtBillInvoice   pr                  Extpgm('WTRPTBLIN')
     d  inPrds                       40A   const
     d  inCall                        1A
     d  inrrn                         9A
     d
     d WtBillInvoice   pi
     d  inPrds                       40A   const
     d  inCall                        1A
     d  inrrn                         9A

     d outCltId        s              3
     d EL_rrn          s              9  0

     d charMsg         s            100
     d isValid         s               n   Inz(*off)
     d intCo#          s                   Like(ELco#)
     d intFnd          s                   Like(ELfnd)
     d intEmp#         s                   Like(ELemp#)
     d intDiv          s                   Like(ELdiv)
     d intFyr          s                   Like(ELFyr)
      *
     d outCo#          s              3
     d outFnd          s              3
     d outEmp#         s              9
     d outDiv          s              5
     d outFyr          s              3
     d outFromDt       s              8
     d outToDt         s              8
     d outSBMT         s              1    inz(' ')
     d sVLDA           DS          1024    DTAARA(*LDA)
     d xxDart                         1    overlay(svlda:251)
     d xxRMCD                         5    overlay(svlda:252)
     d xxCopy                         1    overlay(svlda:257)
     d xxPrint                        1    overlay(svlda:258)
     d xxPrinter                     10    overlay(svlda:259)
     d xxTray                        50    overlay(svlda:269)
     d xx#CopiesA                     3    overlay(svlda:319)
     d xxEmail                        1    overlay(svlda:322)
     d xxToEmail                    200    overlay(svlda:323)
     d xxSubject                    150    overlay(svlda:523)
     d xxBody                       350    overlay(svlda:673)
     d outDocPath      s            300
     d outMessage      s            256
     d outTitle        s             50
     d outCnlHide      s              1    inz(' ')
     d OutModeOut      s              1    inz(' ')
     d isScript        s              1A   inz('N')
     D isUseSpoolOut   s              1n   inz
     D isUseXML        S              1n   inz
     d massFlag        s              1
     d xxSubmit        s              1n
     d xxProcType      s            100
     d processType     c                   const('Billing Invoice')
      *****************************************************************
      * Prototype Declaration
      *****************************************************************

     d/copy sPrAtrium
     d/copy sPrDate
     d/copy sPrCompany
     d/copy wPrUsrPrms
     d/copy wPrPolErr
     d/copy sPrLetter
     d/copy sPrModule
     d/copy sPrXML

      * Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d outCltId                       3


      * Main Processing
     d Main            pr

      * Program Start
     d ProgramStart    pr

      * Set Criteria
     d SetCriteria     pr

      * Billing Invoice Report Submit
     d BillInvoice     pr                  ExtPgm('WTRPTBLINC')
     d outCo#                         3
     d outFnd                         3
     d outEmp#                        9
     d outDiv                         5
     d outFyr                         3
     d outFromDt                      8
     d outToDt                        8
     d outSBMT                        1

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

       // Process until exit button is pressed.
       Dou BtnCancel = *On ;

          SetCriteria();

          // Display Screen
          Exsr ExfmtScreens;

          Select;

          When BtnSubmit = *on;
             // Validate Edits
             Exsr Validate;
             If isValid = *Off;
                Exsr OutControl;
                If xxSubmit = *off;
                   Leave;
                Endif;
                Exsr Submit;
                Leave;
             EndIf;

          When BtnCancel = *on;
             Leave;

          // do nothing - catch tab click event
          When tabClick = *on;
             tabClick = *off;

          When BtnRefresh = *On;
             BtnRefresh =*Off;
             ProgramStart();

          EndSl;

       Enddo;

       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       Select;

       When TabIndex = 0;
          TabHeight = 770;
          APnlHeight = TabHeight + 60;

          If Q1Prms >=3;
             Exfmt RcdBillInv;
          Else;
             Write rcdComPnl;
             Write RcdBillTab;

             Exfmt RcdBillInv;

             Read rcdComPnl;
             Read RcdBillTab;
          Endif;

       EndSl;

       Endsr;

       //===================================================================
       // Validate the Screen;
       //===================================================================
       BegSr Validate;

       // initialize the error indicator
       isValid = *Off;
       msFnd      = *blanks;
       erFnd      = *off;
       msFrmDt    = *blanks;
       erFrmDt    = *off;
       msToDate   = *blanks;
       erToDate   = *off;

       // Validation Check for Fund
       If scFnd   = *Zeros;
          erFnd   = *On;
          msFnd   = 'WCR0503';
          isValid  = *on;
       EndIf;


       // Validation the Installment (Notice) date entered
          erFrmDt  = ValidDate(scFrmDt:charMsg);
          If erfrmDt  = *on;
             msFrmDt  = charMsg;
             isValid  = *on;
          Endif;

       // Validation the Installmet 'to' date entered
          erToDate = ValidDate(scToDate:charMsg);
          If erToDate = *on;
             msToDate = charMsg;
             isValid  = *on;
          Endif;

       //Installment "from date" cannot be greater than "to date"
          erToDate = AscendDates(scFrmDt:scToDate:charMsg);
          If erToDate = *on;
             erfrmDt  = *on;
             msToDate = charMsg;
             msFrmDT  = charMsg;
             isValid  = *on;
          Endif;


       Endsr;

       //===================================================================
       // Write Output Control Screen
       //===================================================================
       Begsr OutControl;

           xxSubmit = *on;

           if isScript = 'Y';

             xxProcType = processType;
             In svLDA;
             Clear svLDA;


           //if scDartRmk <> *blanks;
                xxDart = 'Y';
                xxRMCD = 'PSPN';
           //Else;
           //   xxDart = 'N';
           //   xxRMCD = ' ';
           //Endif;

             Out svLDA;

             xxSubmit = OutputWindow(xxProcType:massFlag:outCnlHide);

           endif;
       Endsr;

       //===================================================================
       // Submit the Screen;
       //===================================================================
       BegSr Submit;

          intFnd = scFnd;
          outCo# = %editc(intCo#:'X');
          outFnd = %editc(intFnd:'X');
          outEmp# = %editc(intEmp#:'X');
          outDiv = %editc(intDiv:'X');
          outFyr = %editc(intFyr:'X');
          outFromDt = %editc(scFrmDt:'X');
          outToDt   = %editc(scToDate:'X');
          If inCall = 'E';
             outSBMT   = inCall;                 // 'E' = Billing Invoice called from Toolbar
          Else;
             outSBMT   = 'G';                    // 'G' = Billing Invoice called from Menu
          Endif;

          BillInvoice(outCo#:outFnd:outEmp#:outDiv:outFyr:outFromDt:outToDt:
                      outSBMT);

       Endsr;

      /End-Free

     p Main            e

       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /Free

       // Intialize the Screen fields
       Clear RcdBillInv;

       // Load Action panel with the Company Name.
       intCo#     = UserCo#(Q1User);
       scCompName = GetCompNam(intCo#);
       scFnd      = UserGrp(Q1User);

       // When screen is called as pop up
       If Q1Prms >= 3;
          isPopUp  = '1';
          EL_Rrn = %Dec(inRrn:9:0);
       Endif;

       If EL_Rrn <> 0;
          Chain(n) EL_Rrn pel0p;
          If %Found(Wdelp);
             intCo#  = ELco#;
             intFnd  = ELfnd;
             scFnd   = ELfnd;
             intEmp# = ELEmp#;
             intDiv  = ELdiv;
             intFyr  = ELFyr;
          Endif;
       Else;
          isPopUp = '0';
       EndIf;

       intFnd     = scFnd;

       // Get the client id
       GetClientID(outCltId);

       tabClick = *Off;
       // Determine if using spool output and/or Scriptura
       // Set field indicator to determine if fields are displayed
       isUseSpoolOut = UseSpoolOut(processType);
       isUseXML = UseScriptura(processType)
        or Use3rdXml(processType);
       If isUseXML;
          isScript = 'Y';
       Else;
          isScript = 'N';
       Endif;

       Return;

      /End-Free

     p ProgramStart    e

       //===================================================================
       // SetCriteria
       //===================================================================
     p SetCriteria     b
     d SetCriteria     pi

      /Free

       GetGrpAuthSql(q1User:grpSelect:grpFrom:grpWhere:'A');

      /End-Free

     p SetCriteria     e
