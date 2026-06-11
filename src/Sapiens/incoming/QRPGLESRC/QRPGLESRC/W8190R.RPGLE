     hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h*****************************************************************
     h/title Prompt for Carrier Policy # Report
     h/define CONVERT
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     H*****************************************************************
     H/TITLE W8190R - Prompt for Carrier Policy # Report
     h*****************************************************************
     h* Revisions                                                     *
     h*                                                               *
     h*****************************************************************
     fW8190F    cf   e             workstn extfile('W8190F')
     f                                     handler('PROFOUNDUI(HANDLER)')
     fWtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)
     f*
     f* User Parameter Master File
     f*
     fsmcol     IF   E           k DISK    Rename(pco01:pco00)
     fWmupl     IF   E           K DISK    Rename(pup01:pup00)

     dpsds           esds                  extname(szq1p)

     d*****************************************************************
     d* Copy Books
     d*****************************************************************
     d/copy sPrAtrium
     d/copy sPrDate
     d/copy sPrString
     d/copy sPrCompany
     d/copy wPrUsrPrms
     d/copy sPrLetter
     D/copy sPrXML

     d*****************************************************************
     d* Prototype Declaration
     d*****************************************************************

     d* Carrier Policy # Proc.
     d CarPol#Proc     pr

     d* Carrier Policy # Report Submit
     d CarPol#Sbm      pr                  ExtPgm('W8190C')
     d outCo#                         3
     d outFrmDt                       8
     d outToDt                        8
     d outOutq                       10

     d* Clear Error/Message fields
     d ClearErrors     pr

     d* Validate Panel Screen Values
     d ErrorCheck      pr              n

     d* Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d outCltId                       3

     d* Intialize the Screen fields
     d LoadInVal       pr

     d* Main Processing
     d Main            pr

     d* Output Control
     d OutputControl   pr

      * Program Start
     d ProgramStart    pr

     d* Entry
     d W8190R          pr                  Extpgm('W8190R')
     d
     d W8190R          pi

     d*****************************************************************
     d* Work Fields
     d*****************************************************************
     d charMsg         s            100
     d isValid         s               n   Inz(*off)
     d intCo#          s                   Like(UPco#)
     d strOutq         s                   Like(Upoutq)
     d fromDate        s               d   DatFmt(*usa)
     d toDate          s               d   DatFmt(*usa)
      *
     d outCltId        s              3
     d outCo#          s              3
     d outFrmDt        s              8
     d outToDt         s              8
     d outOutq         s             10
     D isUseScript     S              1n   inz
     d processType     c                   const('Carrier Policy# Report')
     d xxProcType      s            100
     d xxSubmit        s              1n
     d xxMass          s              1
     d
     D SVLDA           DS          1024    DTAARA(*LDA)
     D xxDart                         1    overlay(svlda:251)
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

      /Free
       //***************************************************************
       // Main Procesing
       //***************************************************************
       Main();

       *InLr = *on;

      /End-Free

       //===================================================================
       // CarPol#Proc - Carrier Policy # Report Process Submit
       //===================================================================
     p CarPol#Proc     b
     d CarPol#Proc     pi

      /Free

       outCo# = %EditC(intCo#:'X');
       outFrmDt= %editc(scFrmDt:'X');
       outToDt = %editc(scToDt:'X');
       outOutq = strOutq;

       CarPol#Sbm(outCo#:outFrmDT:outToDt:outOutq);

      /End-Free

     p CarPol#Proc     e

       //===================================================================
       // ClearErrors - Clear Error/Message fields
       //===================================================================
     p ClearErrors     b
     d ClearErrors     pi

      /Free

       mgFrmDt    = *blanks;
       erFrmDt    = *off;
       mgToDt     = *blanks;
       erToDt     = *off;

      /End-Free

     p ClearErrors     e

       //===================================================================
       // ErrorCheck - Validate Panel Screen Fields
       //===================================================================
     p ErrorCheck      b
     d ErrorCheck      pi              n

     d isValid         s               n   Inz(*On)
      /Free

       // Clear Errors
       ClearErrors();

       // Validate the from Installment date is valid
       If scFrmDt <> 0;
          If DtCheck(scFrmDt) = *Zeros;
             erfrmDt  = *on;
             mgFrmDt  = 'Invalid Date.';
             isValid  = *Off;
          Else;
            scFrmDt = DtCheck(scFrmDt);
          Endif;
       Endif;

    /  // Validate the To Installment date is valid
       If scToDt <> 0;
          If DtCheck(scToDt) = *Zeros;
             erToDt  = *on;
             mgToDt  = 'Invalid Date.';
             isValid = *Off;
          Else;
            scToDt = DtCheck(scToDt);
          Endif;
       EndIf;

       //"from date" cannot be greater than "to date"
       // If "from date" is left blank then it will be treated as loval
       // If "to date" is left blank then it will be treated as hival
       If erToDt  = *Off and erfrmDt = *Off And scFrmDt <> 0 And scToDt <> 0;
          If CvtToDate(scToDt) < CvtToDate(scFrmDt);
             erFrmDt  = *on;
             mgFrmDT  = 'Dates must be in Ascending order.';
             erToDt   = *on;
             mgToDt   = 'Dates must be in Ascending order.';
             isValid  = *Off;
          Endif;
       Endif;

       Return isValid;

      /End-Free
     p ErrorCheck      e

       //===================================================================
       // LoadInVal - Intialize the Screen fields
       //===================================================================
     p LoadInVal       b
     d LoadInVal       pi

      /Free

       Clear RcdCarPol#;

      /End-Free

     p LoadInVal       e

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

      /Free

       // Start Of Program
       ProgramStart();

       // Process until exit button is pressed.
       Dou BtnCancel = *On ;

          // Display Screen
          Exsr ExfmtScreens;

          Select;
          // Submit
          When BtnSubmit = *On;
             // Validate Edits
             If ErrorCheck();
                OutputControl();
                CarPol#Proc();
                Leave;
             EndIf;
          // Cancel
          When BtnCancel = *On;
             BtnCancel = *Off;
             Leave;
          // do nothing - catch tab click event
          When tabClick = *On;
             tabClick = *Off;
          // Refresh Screen
          When BtnRefresh = *On;
             BtnRefresh =*Off;
             LoadInVal();
          EndSl;

       Enddo;

       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

          Select;
          When TabIndex = 0;
             TabHeight = 570;
             APnlHeight = TabHeight + 50;

             Write rcdComPnl;
             Write RcdTabCPol;

             Exfmt RcdCarPol#;

             Read rcdComPnl;
             Read RcdTabCPol;
          EndSl;

       Endsr;

      /End-Free

     p Main            e

       //===================================================================
       // OutputControl - Output Control Processing
       //===================================================================
     p OutputControl   b
     d OutputControl   pi

      /Free

       If isUseScript;

          xxProcType = processType;

          xxMass = ' ';

          In *Lock *dtaara;

          Clear svLDA;

          xxDart = 'N';
          xxRMCD = ' ';

          Out *dtaara;

          xxSubmit = OutputWindow(xxProcType:xxMass);

       Endif;

      /End-Free

     p OutputControl   e

       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /Free
       // Check to se of Scriptura is on
       isUseScript = UseScriptura(processType);

       // Load Action panel with the Company Name.
       intCo# = userCo#(q1user);
       scCompName = GetCompNam(intCo#);

       // Get the client id
       GetClientID(outCltId);

       // Intialize the Screen fields
       LoadInVal();

       // Retrieve Output Queue of the User.
       Chain q1user pup00;
       If %Found(Wmupl);
          strOutq = Upoutq;
       EndIf;

       tabClick = *Off;

       Return;

      /End-Free

     p ProgramStart    e
