     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H BNDDIR('WTBNDDIR')
     H/define CONVERT
     H*
     H***********************************************************************
     H/TITLE WTDEDBLREP - Deductible Invoice Reprint Prompt                *
     H***********************************************************************
     H*                                                                    *
     H* PROGRAM DESCRIPTION:                                               *
     H*                                                                    *
     H*  Provides the ability to reproduce the deductible invoices         *
     H*  and detail report as of a specific loss date.  The posting        *
     H*  date is a required field and will be edited to verify that        *
     H*  the deductible was posted on that date.  If no record is          *
     H*  found for the entered posting date, an error message is           *
     H*  displayed.  The deductible invoice flag is a required field       *
     H*  (Y/N).  If the deductible invoice flag is 'Y' then the print      *
     H*  date is required.  The deductible detail flag is a required       *
     H*  filed (Y/N).                                                      *
     H*                                                                    *
     H*  CALLED FROM:                                                      *
     H*   NONE                                                             *
     H*  CALLS TO:                                                         *
     H*                                                                    *
     H*  CREATED: 02/14/25  PROGRAMMER- C:344 PROJECT: WC2989              *
     H*                                                                    *
     H***********************************************************************
     H* REVISIONS:                                                         *
     H*                                                                    *
     H* 12/12/25 C:300 Support: 120231                                     *
     H*                Deductible Invoice Submission Error                 *
     H*                1. Psss all dates into USA format to W7325R         *
     H*                                                                    *
     H* 06/23/25 C:300 Project: WC2494, Support: 119263                    *
     H*                DSPF Changes:                                       *
     H*                1) Change 'Print SSN' dropdown values as per JAD    *
     H*                   and corrected its tabbing order                  *
     H*                2) Commented 'Onload' changes from RcdTabDed and    *
     H*                   RcdDedRep record formats for buildFKeyMenu()     *
     H*                                                                    *
     H* 04/18/25 C:291 Project: WC2494                                     *
     H*                Add Output Control Window for Scriptura Output.     *
     H*                                                                    *
     H*                                                                    *
     H***********************************************************************
     fwtdedblrepcf   e             workstn extfile('WTDEDBLREP')
     f                                     handler('PROFOUNDUI(HANDLER)')
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)
     fwdddl2    if   e           k disk    Rename(pdd01:pdd00)
     fwxgsl     if   e           k disk    Rename(pgs01:pgs00)
     fwdelp     if   e             disk
     f                                     Rename(pel01:pel0p)
     f                                     Infds(dbfds1)
     fwmeml     if   e           k disk    Rename(pem01:pem00)
     fssylf     if   e           k disk
     f                                     ignore(ssy01)
     f                                     ignore(spc01)
     f                                     ignore(src01)
     f                                     ignore(sbc01)
     f                                     ignore(scs01)
      //*** Beg Add ***** 04/18/25 **************************************
     FWMEXL     IF   E           K DISK    RENAME(PEX01:PEX00)
      //*** End Add ***** 04/18/25 **************************************

     dpsds           esds                  extname(szq1p)

      * Prototype Declaration
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
     d/copy wPrPolErr
     d/copy WTSearchDs
     d/copy sPrLetter
     d/copy sPrXML

      *** Beg Add ***** 04/18/25 *****************************
     D isUseScript     S              1n   inz
     D processType     c                   const('Deductible Invoice')
     D xxProcType      s            100
     D xxSubmit        s              1n
     D xxMass          s              1
      *** End Add ***** 04/18/25 *****************************

     d SVLDA           ds          1024    DTAARA(*LDA)
     d xxDart                         1    overlay(svlda:251)
      *** Beg Add ***** 04/18/25 *****************************
     D xxRMCD                         5    overlay(svlda:252)
      *** End Add ***** 04/18/25 *****************************
     d xxCopy                         1    overlay(svlda:257)
     d xxPrint                        1    overlay(svlda:258)
     d xxPrinter                     10    overlay(svlda:259)
     d xxTray                        50    overlay(svlda:269)
     d xx#CopiesA                     3    overlay(svlda:319)
     d xxEmail                        1    overlay(svlda:322)
     d xxToEmail                    200    overlay(svlda:323)
     d xxSubject                    150    overlay(svlda:523)
     d xxBody                       350    overlay(svlda:673)

     d dsActSts        ds
     d dsActSts1                      2
     d dsActSts2                      2
     d dsActSts3                      2
     d dsActSts4                      2
     d dsActSts5                      2

     d dbfds1          ds
     d  dbrrn1               397    400b 0

     d dsInvRmk        ds
     d dsInvRmk1               1      2
     d dsInvRmk2               4      5
     d dsInvRmk3               7      8
     d dsInvRmk4              10     11
     d dsInvRmk5              13     14

      // Proto-types
      // Intialize the Screen fields
     d LoadInitVal     pr

      // Main Processing
     d Main            pr

      // Program Start
     d ProgramStart    pr

      // Process Screen input values
     d ProcessScreen   pr

      // Validate Panel Sreen Values
     d ValidPnl        pr              n

      // Retrieve Suffix
     d GetSuffix       pr                  Extpgm('SA030R')
     d outSuffix                      3

      // Report Submit Program CL
     d DedInvRepPr     pr                  ExtPgm('W7324C')
     d  outCo#                        3A
     d  outFnd                        3A
     d  outLosDt                      8A
     d  outPosDt                      8A
     d  outPrtDt                      8A
     d  outInvRmk1                    2A
     d  outInvRmk2                    2A
     d  outInvRmk3                    2A
     d  outInvRmk4                    2A
     d  outOutq                      10A
     d  outDcIn                       1A
     d  outDDrp                       1A
     d  outMode                       1A
     d  outExCl                       1A
     d  outDedBil                     1A
     d  outPrtSsn                     1A

      * Extract Loss Date
     d GetLossDate     pr                  Extpgm('SRPAST8')
     d outDt                          8  0
     d outDay                         5  0
     d inErr                          1

      * Verify Print SSN
     d ChkPrtSsn       pr                  ExtPgm('WA097R')
     d  outCo#                        3A
     d  outRptId                     10A
     d  inPrtSsn                      1A
     d  inLockSsn                     1A

      // Retrieve Text
     d GetText         pr                  ExtPgm('SRCENTER')
     d  outData                      80A
     d  outLen                        3  0

      // Program entry parameters
     d WtDedBlRepR     pr                  Extpgm('WTDEDBLREP')
     d  inPrDs                       40A
     d  inCall                        1A

     d WtDedBlRepR     pi
     d  inPrDs                       40A
     d  inCall                        1A

     d @@Mode          s                   Like(inCall) Inz
     d outAppSfx       s              1A   Inz
     d inFlag          s              1A   Inz
     d outSuffix       s              3    Inz
     d intGrpCnt       s              2  0 Inz
     d intCo#          s                   Like(ELco#) Inz
     d intFnd          s                   Like(ELfnd) Inz
     d inPosDt         s              7  0 Inz
     d outRrn#         s              9
     d inSelect        s           1024
     d outCo#          s              3A   Inz
     d outFnd          s              3A   Inz
     d outLosDt        s              8A   Inz
     d outPosDt        s              8A   Inz
     d outPrtDt        s              8A   Inz
     d outInvRmk1      s              2A   Inz
     d outInvRmk2      s              2A   Inz
     d outInvRmk3      s              2A   Inz
     d outInvRmk4      s              2A   Inz
     d outOutq         s             10A   Inz
     d outDcIn         s              1A   Inz
     d outDDrp         s              1A   Inz
     d outExCl         s              1A   Inz
     d outDedBil       s              1A   Inz
     d outPrtSsn       s              1A   Inz
     d outMode         s              1A   inz
     d outData         s             80A   inz
     d outLen          s              3  0 inz
     d indexTab        s              5  0 inz
     d outDt           s              8  0 Inz
     d outDay          s              5  0 Inz
     d inErr           s              1    Inz
     d outRptId        s             10A   Inz
     d inPrtSsn        s              1A   Inz
     d inLockSsn       s              1A   Inz
     d isError         s               n   inz
     d isSglMod        s               n   inz
     d isARexist       s               n   inz

       // Start Of Program
       ProgramStart();

          // Main Procesing
          Main();

       *InLr = *On;

       //===================================================================
       // Validate the screen fields for error
       //===================================================================
     p ValidPnl        b
     d ValidPnl        pi              n

     d intPostYear     s              4  0 Inz
     d intSysYear      s              4  0 Inz
     d intUsaDate      s              8  0 Inz

       // Initialize error indicators
       Clear isError;
       Clear erPosDt;
       Clear erDedBil;
       Clear erPrtDt;
       Clear erInvRmk;
       Clear erPrtSsn;

       // Initialize error messages
       Clear msPosDt;
       Clear msDedBil;
       Clear msPrtDt;
       Clear msInvRmk;
       Clear msPrtSsn;

       // Validate A/R Posting Date
       intUsaDate = dtCheck(scPosDt);
       If intUsaDate = 0;
          erPosDt = *On;
          msPosDt = 'WCR0500';
          isError = *On;
       Else;

          // Determine if A/R Posting Date is valid for 1 group.
          Clear isARexist;
          If isSglMod;
             // Convert Post Date to 7 digits
             inPosDt = dtCymd(scPosDt);
             // Determine if Deductible recs exist for given posting date
             Chain (scCo#:scFnd:inPosDt) pdd00;
             If %found(WDDDL2);
                isARexist = *On;
             Endif;
          Else;
             // Determine if Deductible recs exist for given posting date
             Setll (scCo#) pgs00;
             Dou %Eof(WXGSL);
                Reade (scCo#) pgs00;
                If %Eof(WXGSL);
                   Leave;
                Endif;

                // Convert Post Date to 7 digits
                inPosDt = dtCymd(scPosDt);
                // Determine if Deductible recs exist for given posting date
                Chain (GSco#:GSFnd:inPosDt) pdd00;
                If %found(WDDDL2);
                   isARexist = *On;
                   Leave;
                Endif;
             Enddo;
          Endif;

          // If AR Records don't exist then flag error.
          If isARexist = *Off;
             erPosDt = *On;
             msPosDt = 'WCR3149';
             isError = *On;
          Endif;

       EndIf; // If intUsaDate = 0

       // Validate Print Date
       intUsaDate = dtCheck(scPrtDt);
       If intUsaDate = 0;
          erPrtDt = *On;
          msPrtDt = 'WCR0500';
          isError = *On;
       EndIf;

       // Validate Deductible Billing Type
       If isLrgDed = *On and scDedBil <> 'B' and scDedBil <> 'L' and
        scDedBil <> 'S';
          erDedBil = *On;
          msDedBil = 'WCR0503';
          isError = *On;
       Endif;

       // Validate Max of 4 Invoice Remarks can be selected
       Clear dsInvRmk;
       dsInvRmk = scInvRmk;

       If dsInvRmk5 <> ' ';
          erInvRmk = *On;
          msInvRmk = 'WCR4825';
          isError = *On;
       Endif;

       // Validate Print SSN
       If isSsnLock = *Off;
          If scDDrp = 'Y' and scPrtSsn = ' ';
             erPrtSsn = *On;
             msPrtSsn = 'WCR0503';
             isError = *On;
          Endif;
       Endif;

       Return not isError;

     p ValidPnl        e
       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

       // Intialize the Screen fields
       LoadInitVal();

     p ProgramStart    e

       //===================================================================
       // LoadInitVal - Initializing variables
       //===================================================================
     p LoadInitVal     b

       Clear RcdDedRep;

       // Get Company
       scCo# = userCo#(q1user);

       // Retrieve Company Name
       scCompName = GetCompNam(scCo#);

       // Extract text for PRDS
       If Q1prms >= 1 and inPRDS <> ' ';
          outData = inPRDS;
       Else;
          outData = 'Deductible Billing Reprint';
       Endif;

       outLen = 40;
       GetText(outData:outLen);

       scPrDs = outData;

       Clear @@Mode;

       If Q1Prms >= 2;
          @@Mode = inCall;
       Endif;

       // GET client suffix; set up workfile caption.
       GetSuffix(outSuffix);
       scOutFile = 'This option will produce output file WFD1P in Library +
        PDSRPT' + outSuffix + ' for the Deductible Detail report.';
       Clear WCfl43;

       // Check for Large Deductible Module
       Chain ('WC') swc01;
       If WCfl43 = 'Y';
          isLrgDed = *On;
       Else;
          isLrgDed = *Off;
          scDedBil = 'S';
       Endif;

       // Determine Company and Group Count
       Dou %Eof(WXGSL);
          Read pgs00;
          If %Eof(WXGSL) or intGrpCnt > 1;
             Leave;
          Endif;

          // Verify if group rec was included
          If GSiefl = 'I';
             intGrpCnt += 1;
             intCO# = GSco#;
             intFnd = GSfnd;
             scFndNm  = GSname;
          Endif;
       Enddo;

       // Determine if a single group was selected
       If intGrpCnt = 1;
          isSglMod = *On;
          scFnd = intFnd;
       Else;
          Clear scFnd;
       Endif;

       // Extract SSN info
       outCo# = %Char(intCo#);
       outRptId = 'W7324R'; // Use old program name
       Clear inPrtSsn;
       Clear inLockSsn;

       // Verify SSN/FEIN User Authority
       ChkPrtSsn(outCo#:outRptId:inPrtSsn:inLockSsn);
       scPrtSsn = inPrtSsn;
       If inLockSsn = '1';
          isSsnLock = *On;
       Else;
          isSsnLock = *Off;
       Endif;
       //*** Beg Add ***** 04/18/25 ************************************
       isUseScript = UseScriptura(processType);
       //*** End Add ***** 04/18/25 ************************************

     p LoadInitVal     e
       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

       // Process until exit button is pressed.
       Dou ExitNow = *On;

          // Display Screen
          Exsr ExfmtScreens;

          Select;
             When BtnSubmit = *On;
                If ValidPnl() = *On;
                   ProcessScreen();
                   Return;
                EndIf;

             //Do nothing - inPolExpDt click event
             When tabClick = *On;
                tabClick = *Off;

          EndSl;

       Enddo;
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       Select;

       When TabIndex = 0;
          Tabheight = 770;
          APnlHeight = 830;

          Write rcdComPnl;
          Write rcdTabRep;
          Exfmt rcdDedRep;
          Read rcdComPnl;
          Read rcdTabRep;

       EndSl;

       Endsr;

     p Main            e

       //===================================================================
       // Process user inputs on screen
       //===================================================================
     p ProcessScreen   b
     d ProcessScreen   pi
       //*** Beg Add ***** 12/12/25 ****************************************
     d intUsaDate      s              8  0 Inz
       //*** End Add ***** 12/12/25 ****************************************

        //*** Beg Add ***** 04/18/25 ************************************
        // Display Output Control Window
        xxSubmit = *on;
        If isUseScript;
           xxMass = 'Y';
           xxProcType = processType;

           In *Lock *dtaara;
           xxDart = 'N';
           xxRMCD = ' ';
           Out *dtaara;
           xxSubmit = OutputWindow(xxProcType:xxMass);

           If xxSubmit = *Off;
              Return;
           Endif;
        Endif;
        //*** End Add ***** 04/18/25 ************************************

        // Move the parameter values to call the report CL program
        outCo#     = %editC(scCo#:'X');
        outFnd     = %editC(scFnd:'X');

        // Get Loss Date
        outDt = %dec(%char(%date(DDidt:*cymd):*usa0):8:0);
        outDay = %subdt(%Date(outDt:*usa):*days);

        // Populate Loss Date with the last day of the previous month.
        GetLossDate(outDt:outDay:inErr);
        outLosDt   = %editC(outDt:'X');
        //*** Beg Chg ***** 12/12/25 ****************************************
        //*** outPosDt   = %editC(scPosDt:'X');
        //*** outPrtDt   = %editC(scPrtDt:'X');
        intUsaDate = dtCheck(scPosDt);
        outPosDt = %editC(intUsaDate:'X');
        intUsaDate = dtCheck(scPrtDt);
        outPrtDt = %editC(intUsaDate:'X');
        //*** End Chg ***** 12/12/25 ****************************************
        dsInvRmk   = scInvRmk;
        outInvRmk1 = dsInvRmk1;
        outInvRmk2 = dsInvRmk2;
        outInvRmk3 = dsInvRmk3;
        outInvRmk4 = dsInvRmk4;
        outOutq    = GetUsrOutq(q1user);
        outDcIn    = scDcIn;
        outDDrp    = scDDrp;
        outMode    = 'D';
        outExCl    = scExCl;
        outDedBil  = scDedBil;
        outPrtSsn  = scPrtSsn;

        DedInvRepPr(outCo#:outFnd:outLosDt:outPosDt:outPrtDt:outInvRmk1:
        outInvRmk2:outInvRmk3:outInvRmk4:outOutq:outDcIn:outDDrp:outMode:
        outExCl:outDedBil:outPrtSsn);

     p ProcessScreen   e
