     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H*****************************************************************
     H/title WTRPTCERTP - Certificate of Insurance - Mass Print
     H/define   PROFOUNDUI
     H dftactgrp(*no) actgrp(*caller)
     H option(*nodebugio: *srcstmt)
     H bnddir('WTBNDDIR')
     H*****************************************************************
     H/TITLE WTRPTCERTP - Promt : Certificate of Insurance - Mass Print
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*  This is the Prompt Program For Certificate of Insurance      *
     H*       This Program Prompts For                                *
     H*       1) Policy Effective Date Range                          *
     H*       2) Policy Type
     H*****************************************************************
     H* Revisions:                                                    *
     H*
     H* 11/20/24 C:340 Project: WC2988
     H*                Create
     H*
     H*****************************************************************
     FWTRPTCERTPCF   E             Workstn ExtFile('WTRPTCERTP')
     F                                     Handler('PROFOUNDUI(HANDLER)')
     FWTACTPNL  CF   E             Workstn
     F                                     Handler('PROFOUNDUI(HANDLER)')
     F                                     include(rcdPolPnl)
     F                                     include(rcdQtePnl)
     F                                     include(rcdComPnl)
     F*
     F* User Parameter Master File
     F*
     FWDELL     IF   E           K Disk

     dpsds           esds                  extname(szq1p)

     d xxRRN           s              9  0
      *
     d WtPrnCert       pr                  Extpgm('WTRPTPRNTC')
     d  inPrds                       40A   const
     d  inCall                        1A
     d  inrrn                         9A
     d
     d WtPrnCert       pi
     d  inPrds                       40A   const
     d  inCall                        1A
     d  inrrn                         9A

     d outCnlHide      s              1    inz(' ')
     d outCo#          S              3
     d outFnd          S              3
     d outYear         S              3
     d outEmp#         S              9
     d outDiv          S              5
     d outCer          S              5
     d outCert         S              5
     d outBlnk         S              1
     d outDate         S              8
     d outCopy         S              3    Inz('001')
     d outQue          S             10
     d outRSFL         S              1
     d outRMCD         S              5
     d outFrDt         S              8
     d outToDt         S              8
     d outRNB          S              1
     d outCltId        s              3
     d charMsg         s            100
     d isValid         s               N   Inz(*off)
     d intCo#          s                   Like(ELco#)
      *
     d xxFrDt          s              8
     d xxToDt          s              8
      *
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
      *
     d isScript        S              1A   inz('0')
     D isUseSpoolOut   S              1n   inz
     D isUseXML        S              1n   inz
     d massFlag        S              1
     d xxCopies        S              3  0
     d xxSubmit        S              1n
     d xxProcType      S            100
     d processType     C                   const('Certificate of Insurance')
      *****************************************************************
      * Prototype Declaration
      *****************************************************************

     d/copy sPrAtrium
     d/copy sPrDate
     d/copy sPrString
     d/copy sPrCompany
     d/include sCopyPolPr
     d/copy wPrAgent
     d/copy wPrProduce
     d/copy wPrGroup
     d/copy wPrRateSet
     d/copy wPrEmpLr
     d/copy wPrCarrier
     d/copy wPrUsrPrms
     d/copy wPrPolErr
     d/copy WTSearchDs
     d/copy sPrLetter
     d/copy sPrModule
     d/copy sPrXML

      * Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d outCltId                       3

      * Intialize the Screen fields
     d LoadInVal       pr

      * Main Processing
     d Main            pr

      * Program Start
     d ProgramStart    pr

      * Validate Panel Sreen Values
     d ValidPnl        pr

      * Print Certificate Proc.
     d PrnCertProc     pr

      * New/Renewal Business Report Submit
     d PrnCertSbm      pr                  ExtPgm('W7510C')
     d outCo#                         3
     d outFnd                         3
     d outYear                        3
     d outEmp#                        9
     d outDiv                         5
     d outCer                         5
     d outCert                        5
     d outBlnk                        1
     d outDate                        8
     d outCopy                        3
     d outQue                        10
     d outRSFL                        1
     d outRMCD                        5
     d outFrDt                        8
     d outToDt                        8
     d outRNB                         1

      /Free
       // Start Of Program
       ProgramStart();

       // Main Procesing
       Main();

       *InLr = *On;

      /End-Free

      *===================================================================
      * PrnCertProc - Print Certificate Process Submit
      *===================================================================
     p PrnCertProc     b
     d PrnCertProc     pi

       outco# = %EditC(intCo#:'X');

       // Set form/To date to scFrmDt
       xxFrdt = %editc(scFDt:'X');
       xxTodt = %editc(scTDt:'X');

       outFrDt  = xxFrdt;
       outToDt  = xxTodt;
       outRNB   = scPolType;

       PrnCertSbm(outCo#:outFnd:outYear:outEmp#:outDiv:outCer:outCert:
                  outBlnk:outDate:outCopy:outQue:outRSFL:outRMCD:
                  outFrDt:outToDt:outRNB);

      /End-Free

     p PrnCertProc     e

       //===================================================================
       // LoadInVal - Intialize the Screen fields
       //===================================================================
     p LoadInVal       b
     d LoadInVal       pi

      /Free

       Clear RcdPrnCert;

       // Load Action panel with the Company Name.
       intCo#     = UserCo#(Q1User);
       scCompName = GetCompNam(intCo#);
       scCo#     = intCo#;

     p LoadInVal       e

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

      /Free

       // Process until Cancel button is pressed.
       Dou BtnCancel = *On ;

          // Display Screen
          Exsr ExfmtScreens;

          Select;
          When BtnSubmit = *on;
             // Validate Edits
             ValidPnl();
             If isValid = *Off;
                Exsr OutControl;
                If xxSubmit = *off;
                   Leave;
                Endif;
                PrnCertProc();
                Leave;
             EndIf;

          When BtnCancel = *on;
             Leave;

          // do nothing - catch tab click event
          When tabClick = *on;
             tabClick = *off;

          EndSl;

       Enddo;

       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       Select;
          When TabIndex = 0;
             TabHeight = 650;
             APnlHeight = TabHeight + 60;

             If Q1Prms >=3;
                Exfmt RcdPrnCert;
             Else;
                Write rcdComPnl;
                Write RcdTabCert;

                Exfmt RcdPrnCert;

                Read rcdComPnl;
                Read RcdTabCert;
             Endif;
       EndSl;

       Endsr;
       //===================================================================
       // Write Output Control Screen
       //===================================================================
       Begsr OutControl;

           xxSubmit = *on;

           If isScript = '1';

             xxProcType = processType;
             In svLDA;
             Clear svLDA;

             xxDart = 'Y';
             xxRMCD = 'PSCI';
             xxCopies = 1;

             Out svLDA;

             xxSubmit = OutputWindow(xxProcType:massFlag:outCnlHide);

           Endif;
       Endsr;

      /End-Free

     p Main            e

       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

       // Intialize the Screen fields
       LoadInVal();

       Eval isPopUp  = '0';
       Eval isPop    = '1';

       // Get the client id
       GetClientID(outCltId);

       // Determine if using Scriptura
       isScript = UseScriptura(processType);

       Return;

     p ProgramStart    e
       //===================================================================
       // ValidPnl - Validate Panel Screen Fields
       //===================================================================
     p ValidPnl        b
     d ValidPnl        pi

         // Initialize the Error Indicator
         isValid = *Off;
         msFDt      = *Blanks;
         erFDt      = *Off;
         msTDt      = *Blanks;
         erTDt      = *Off;
         msPolType  = *Blanks;
         erPolType  = *Off;

         // Validate Policy effective date
         If scFDt = *Zero or scTDt = *Zero;
               erFDt    = *On;
               msFDt    = 'Select From Effective Date.';
               erTDt    = *On;
               msTDt    = 'Select To Effective Date.';
               isValid  = *on;
         Else;
         // Check From and To date for valid values
         // Validate the Policy effective 'from' date
            erFDt = ValidDate(scFDt:charMsg);
            If erFDt = *on;
               msFDt = 'Enter Valid Date';
               isValid  = *on;
            Endif;

         // Validate the Policy effective 'To' date
            erTDt = ValidDate(scTDt:charMsg);
            If erTDt = *on;
               msTDt = 'Enter Valid Date';
               isValid  = *on;
            Endif;

         // "From Date" cannot be greater than "To Date"
            erTDt = AscendDates(scFDt:scTDt:charMsg);
            If erTDt = *on;
               msTDt = 'From Date cannot be greater than To Date';
               isValid  = *on;
            Endif;
         Endif;

         // Validation Check for Generate
         If scPolType = *Blanks;
            erPolType = *On;
            msPolType = 'Please Select the Policy Type.';
            isValid  = *on;
         EndIf;

     p ValidPnl        e

