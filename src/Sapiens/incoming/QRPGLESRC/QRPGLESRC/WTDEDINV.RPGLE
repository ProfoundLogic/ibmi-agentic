     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H BNDDIR('WTBNDDIR')
     H/define CONVERT
     H*
     H***********************************************************************
     H/TITLE WTDEDINV - PROMPT FOR DEDUCTIBLE BILLING REPORT & INVOICING.
     H***********************************************************************
     H*                                                                    *
     H* PROGRAM DESCRIPTION:                                               *
     H*   THIS PROGRAM PROMPTS THE USER FOR THE BILLING FUND YEAR,         *
     H*   A DATE TO CALCULATE LOSSES, THE A/R POSTING DATE, THE            *
     H*   INVOICE DATE, THE PRINT DATE, AND REMARKS FOR THE INVOICES.      *
     H*                                                                    *
     H*  CALLED FROM:                                                      *
     H*   NONE                                                        *
     H*  CALLS TO:                                                         *
     H*                                                                    *
     H*  CREATED: 01/13/25  PROGRAMMER- C:344 PROJECT: WC2989              *
     H*                                                                    *
     H***********************************************************************
     H* REVISIONS:                                                         *
     H*                                                                    *
     H* 12/12/25 C:300 Support: 120231                                     *
     H*                Deductible Invoice Submission Error                 *
     H*                1. Pass Group Number to W7325R when Employer# is 0  *
     H*                2. Psss all dates into USA format to W7325R         *
     H*                                                                    *
     H* 06/23/25 C:300 Project: WC2494, Support: 119263                    *
     H*                1) Show Blank Losses As Of, AR Posting and Print    *
     H*                   Dates on screen load. Do not intialize them from *
     H*                   user processing date                             *
     H*                2) Default 'All Divisions' checkbox to "Checked"    *
     H*                3) Employer Name getting clear on changing Division *
     H*                   field or 'All Divisions' checkbox                *
     H*                DSPF Changes:                                       *
     H*                1) Change 'Print SSN' dropdown values as per JAD    *
     H*                2) Commented 'Onload' changes from RcdTabDed rcdfmt *
     H*                3) Changes in 'onchange' events of Employer Number, *
     H*                   Division and 'All Divisions' Checkbox -- To not  *
     H*                   clear Division, 'All Divisions' and Employer Name*
     H*                4) Changed Value attributes of Employer Number and  *
     H*                   Division Number fields                           *
     H*                5) Moved Search icon from out of tabbing order      *
     H*                                                                    *
     H* 04/21/25 C:300 Project: WC2494                                     *
     H*                Validate DARTS Remark code, if it is visible        *
     H*                                                                    *
     H* 04/18/25 C:344 Project: WC2989                                     *
     H*                Populate Group parm only if Employer# is has been   *
     H*                entered.                                            *
     H*                                                                    *
     H* 04/11/25 C:291 Project: WC2494                                     *
     H*                Fix Division parm passed to W8007C. Blank value is  *
     H*                passed in Div parameter to run Ded Invoice for all  *
     H*                the divisions.                                      *
     H*                Added visibility indicator isRsvDarts on Remaek code*
     H*                field in display file.                              *
     H***********************************************************************
     fwtdedinv  cf   e             workstn extfile('WTDEDINV')
     f                                     handler('PROFOUNDUI(HANDLER)')
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)
     fstctl1    if   e           k disk    Rename(pct01:pct00)
     fwxgsl     if   e           k disk    Rename(pgs01:pgs00)
     fwtavl     if   e           k disk    Rename(pav01:pav00)
     fwtabl     if   e           k disk    include(pbc01)
     fwdelp     if   e             disk
     f                                     Rename(pel01:pel0p)
     f                                     Infds(dbfds1)
     fwdell     if   e           k disk
     f                                     Rename(pel01:pel00)
     f                                     Infds(dbfds2)
     fwmeml     if   e           k disk    Rename(pem01:pem00)
     fwmupl     if   e           k disk    Rename(pup01:pup00)
     fwmexl     if   e           k disk    Rename(pex01:pex00)
     fwmfnl     if   e           k disk    Rename(pfn01:pfn00)
     fssylf     if   e           k disk
     f                                     ignore(ssy01)
     f                                     ignore(spc01)
     f                                     ignore(src01)
     f                                     ignore(sbc01)
     f                                     ignore(scs01)

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

     d isUseScriptura  s              1n   inz
     d isUseSpool      s              1n   inz
     d processType     s            100    inz('Deductible Invoice')
     d xxProcType      s            100
     d xxSubmit        s              1n
     d xxMass          s              1

     d SVLDA           ds          1024    DTAARA(*LDA)
     d xxDart                         1    overlay(svlda:251)
     d xxRMKCD                        5    overlay(svlda:252)
     d xxCopy                         1    overlay(svlda:257)
     d xxPrint                        1    overlay(svlda:258)
     d xxPrinter                     10    overlay(svlda:259)
     d xxTray                        50    overlay(svlda:269)
     d xx#CopiesA                     3    overlay(svlda:319)
     d xxEmail                        1    overlay(svlda:322)
     d xxToEmail                    200    overlay(svlda:323)
     d xxSubject                    150    overlay(svlda:523)
     d xxBody                       350    overlay(svlda:673)
     d isError         s               n   inz

     d dsActSts        ds
     d dsActSts1                      2
     d dsActSts2                      2
     d dsActSts3                      2
     d dsActSts4                      2
     d dsActSts5                      2

     d dbfds1          ds
     d  dbrrn1               397    400b 0

     d dbfds2          ds
     d  dbrrn2               397    400b 0

     d dsInvRmk        ds
     d dsInvRmk1               1      2
     d dsInvRmk2               4      5
     d dsInvRmk3               7      8
     d dsInvRmk4              10     11
     d dsInvRmk5              13     14

      * Intialize the Screen fields
     d LoadInitVal     pr

      * Main Processing
     d Main            pr

      * Program Start
     d ProgramStart    pr

      * Process Screen input values
     d ProcessScreen   pr

      * Validate Panel Sreen Values
     d ValidPnl        pr              n

     d CheckLib        pr                  Extpgm('SRCHKLIB')
     d outLib                        10A
     d outLibl                        4A
     d outAppSfx                      1A
     d inFlag                         1A

     d CheckPgm        pr                  Extpgm('SRCHKPGM')
     d outPgm                        10A
     d inFlag2                        1A

     d CheckGrp        pr                  Extpgm('W7944R1')
     d outCall                        1A
     d outType                       10A
     d inGrpErr                       1A

     d GetSuffix       pr                  Extpgm('SA030R')
     d outSuffix                      3

      * Report Submit Program CL
     d DedInvPr        pr                  ExtPgm('W8007C')
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
     d  outToFnd                      3A
     d  outExCl                       1A
     d  outFRgn                       3A
     d  outTRgn                       3A
     d  outResBil                     1A
     d  outDrtRmk                     5A
     d  outDedBil                     1A
     d  outEmp#                       9A
     d  outDiv                        5A
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

      * Retrieve Text
     d GetText         pr                  ExtPgm('SRCENTER')
     d  outData                      80A
     d  outLen                        3  0

      * Employer Search Proc.
     d EmployerSrch    pr

      * Employer Search
     d EmpSearch       pr                  ExtPgm('WTSEARCH')
     d inMode                         1
     d inCall2                        1
     d outRrn#                        9
     d inSelect                            likeds(dsSrchCrit)

     d WtDedInvR       pr                  Extpgm('WTDEDINV')
     d  inPrDs                       40A
     d  inCall                        1A

     d WtDedInvR       pi
     d  inPrDs                       40A
     d  inCall                        1A

     d intPos          s              2  0 Inz
     d strMsg          s            100    Inz
     d strrptl         s            200    Inz
     d strARCdDsc      s             30    Inz
     d strARTyDsc      s             30    Inz
     d strAVTDsc       s             30    Inz
     d outLib          s             10A   Inz
     d inFlag          s              1A   Inz
     d outPgm          s             10A   Inz
     d inFlag2         s              1A   Inz
     d outCall         s              1A   Inz
     d outType         s             10A   Inz
     d inErr           s              1A   Inz
     d outMultGr       s              1A   Inz
     d outLibl         s              4A   Inz
     d outAppSfx       s              1A   Inz
     d outSuffix       s              3    Inz
     d intGrpCnt       s              2  0 Inz
     d intCo#          s                   Like(ELco#) Inz
     d intFnd          s                   Like(ELfnd) Inz
     d intEmp#         s                   Like(ELemp#) Inz
     d intDiv          s                   Like(ELdiv) Inz
     d intFyr          s                   Like(ELFyr) Inz(*hival)
     d isVisbEmp       s              1n   Inz
     d inMode          s              1    Inz
     d outRrn#         s              9    Inz
     d inSelect        s           1024    Inz
     d rrnEmp          s              9  0 Inz
     d rrnEl           s              9  0 Inz
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
     d outToFnd        s              3A   Inz
     d outExCl         s              1A   Inz
     d outFRgn         s              3A   Inz
     d outTRgn         s              3A   Inz
     d outDFlg         s              1A   Inz
     d outDrtRmk       s              5A   Inz
     d outDedBil       s              1A   Inz
     d outEmp#         s              9A   Inz
     d outDiv          s              5A   Inz
     d outPrtSsn       s              1A   Inz
     d outRptId        s             10A   Inz
     d inPrtSsn        s              1A   Inz
     d inLockSsn       s              1A   Inz
     d outMthEnd       s              1n   Inz
     d lrGDed          s              1n   Inz
     d outDt           s              8  0 Inz
     d outDay          s              5  0 Inz
     d inGrpErr        s              1    Inz
     d outData         s             80A   Inz
     d outLen          s              3  0 Inz
     d empRrn          s              9  0 Inz
     d inCall2         s              1    Inz
     d inDiv           s              5    Inz

       // Start Of Program
       ProgramStart();

       If WCfl06 = 'Y';
          // Main Procesing
          Main();
       Endif;

       *InLr = *On;

       //===================================================================
       // Validate the screen fields for error
       //===================================================================
     p ValidPnl        b
     d ValidPnl        pi              n

     d intPostYear     s              4  0 Inz
     d intPostMonth    s              2  0 Inz
     d intSysYear      s              4  0 Inz
     d intSysMonth     s              2  0 Inz
     d intUsaDate      s              8  0 Inz

       // Initialize error indicators
       Clear isError;
       Clear erEmp#;
       Clear erAllDiv;
       Clear erLosDt;
       Clear erPosDt;
       Clear erPrtDt;
       Clear erInvRmk;
       Clear erDedBil;
       Clear erDFlg  ;
       Clear erPrtSsn;
       Clear erDrtRmk;

       // Initialize error messages
       Clear msEmp#;
       Clear msAllDiv;
       Clear msLosDt;
       Clear msPosDt;
       Clear msPrtDt;
       Clear msInvRmk;
       Clear msDedBil;
       Clear msDFlg  ;
       Clear msPrtSsn;
       Clear msDrtRmk;

      // If single-mode, validate the associated field(s)
       If isSglMod = *On;
         // Validate Employer/Division
          Select;
          When scEmp# <> 0;
             If ValidEmp(scEmp#:scDiv:rrnEmp:scFnd:scCo#) = *Off;
                erEmp#    =  *On;
                msEmp#    = 'WCR0538';
                isError = *On;
             Else;
               // Both Division and 'All Divisions' selected
                If scDiv <> 0 and scAllDiv <> ' ';
                   erAllDiv  =  *On;
                   msAllDiv  = 'WC40013';
                   isError = *On;
                Endif;
             Endif;
          // Clear Division when all Employers selected (scEmp# = 0)
          Other;
            Clear scDiv;
          Endsl;
       Endif;

       // Validate Losses as of Date
       intUsaDate = dtCheck(scLosDt);
       If intUsaDate = 0;
          erLosDt = *On;
          msLosDt = 'WCR0500';
          isError = *On;
       Endif;

       // Validate A/R Posting Date
       intUsaDate = dtCheck(scPosDt);
       If intUsaDate = 0;
          erPosDt = *On;
          msPosDt = 'WCR0500';
          isError = *On;
       Else;
          // Post date must be valid and not in prior month
          intUsaDate = dtCheck(scPosDt);
          intPostYear = %Subdt(%Date(intUsaDate:*usa):*y);
          intPostMonth = %Subdt(%Date(intUsaDate:*usa):*m);
          intSysYear = %Subdt(%Date():*y);
          intSysMonth = %Subdt(%Date():*m);

          If intPostYear < intSysYear Or
           (intPostYear = intSysYear And
           intPostMonth < intSysMonth);
             erPosDt = *On;
             msPosDt = 'WCR0924';
             isError = *On;
          Endif;
       Endif;

       // Validate Print Date
       intUsaDate = dtCheck(scPrtDt);
       If intUsaDate = 0;
          erPrtDt = *On;
          msPrtDt = 'WCR0500';
          isError = *On;
       Endif;

       Clear dsInvRmk;
       dsInvRmk = scInvRmk;

       // Validate Max of 4 Invoice Remarks can be selected
       If dsInvRmk5 <> ' ';
          erInvRmk = *On;
          msInvRmk = 'WCR4825';
          isError = *On;
       Endif;

       // Validate Deductible Billing Type
       If lrGDed = *On;
          If scDedBil <> 'B' and scDedBil <> 'L' and scDedBil <> 'S';
             erDedBil = *On;
             msDedBil = 'WCR0503';
             isError = *On;
          Endif;
       Endif;

       // Validate Print SSN
       If isSsnLock = *Off;
          If scPrtSsn = ' ';
             erPrtSsn = *On;
             msPrtSsn = 'WCR0503';
             isError = *On;
          Endif;
       Endif;

       // Validate Darts Remark Code
       //*** Beg Chg ***** 04/21/25 ********************
       // If scDrtRmk = ' ';
       If scDrtRmk = ' ' and isRsvDarts = *On;
       //*** End Chg ***** 04/21/25 ********************
          erDrtRmk = *On;
          msDrtRmk = 'WCR0503';
          isError = *On;
       Endif;

       // Don't edit DARTS code unless resolving bills to DARTS.
       //*** Beg Chg ***** 04/21/25 ********************
       // If scDfLg = 'Y';
       If scDfLg = 'Y' and isRsvDarts = *On;
       //*** End Chg ***** 04/21/25 ********************
          If outMultGr = *On and scDrtRmk = ' ';
             // Perform Group Setup Checking
             outCall = '1';
             outType = 'EXDDRM';
             inGrpErr = '0';
             CheckGrp(outCall:outType:inGrpErr);
             If inGrpErr = *On;
                erDrtRmk = *On;
                msDrtRmk = 'WCR0503';
                isError = *On;
             Endif;
          Endif;
          If outMultGr = *Off;
             Chain (UPco#:scFnd) pex00;
             If not %Found(WMEXL);
                Clear EXddrm;
             Endif;
             If scDrtRmk = ' ' and EXddrm = ' ';
                erDrtRmk = *On;
                msDrtRmk = 'WCR0503';
                isError = *On;
             Endif;
             If scDrtRmk <> ' ';
                Chain (scDrtRmk) pct00;
                If %Found(STCTL1) and CTstat = 'A';
                Else;
                   erDrtRmk = *On;
                   msDrtRmk = 'WCR0827';
                   isError = *On;
                Endif;
             Endif;
          Endif;
       Endif;

       Return not isError;

     p ValidPnl        e
       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

       // Set Use Scriptura and Spool Flags
       isUseScriptura = UseScriptura(processType);
       isUseSpool = UseSpoolOut(processType);

       // Intialize the Screen fields
       LoadInitVal();


     p ProgramStart    e
       //===================================================================
       // LoadInitVal - Initializing variables
       //===================================================================
     p LoadInitVal     b

       Clear RcdDedInv;

       // Get Company
       scCo# = userCo#(q1user);

       // Retrieve Company Name
       scCompName = GetCompNam(scCo#);

       // Determine Company and Group Count
       Dou %Eof(WXGSL);
          Read pgs00;
          If %Eof(WXGSL) or intGrpCnt > 1;
             Leave;
          Endif;

          // Verify if group rec was included
          If GSiefl = 'I';
             intGrpCnt += 1;
             intFnd = GSfnd;
          Endif;
       Enddo;

       // Determine Single-Mode
       If intGrpCnt = 1;
          isSglMod = *On;
          // Get Group
          scFnd = intFnd;
          // Retrieve Group Name
          scFndNm = GetGrpName(scFnd:fnRrn:scCo#);
       Endif;

       outLib = 'PDSMTH';
       outLibl = '*USR';
       outAppSfx = '1';
       inFlag = '0';


       // Retrieve Monthend Flag
       CheckLib(outLib:outLibl:outAppSfx:inFlag);

       outMthEnd = inFlag;

       // Extract System Flag
       Chain ('WC') swc01;
       If %Found(ssylf) and WCfl06 <> 'Y';
          Return;
       Endif;

       // Extract User Parameters
       Chain (Q1user) pup00;
       If Not %Found(wmupl);
          Clear UPco#;
          Clear UPfnd;
          Clear UPprDt;
       Endif;

       // Check for Large Deductible Module
       If WCfl43 = 'Y';
          lrGDed = *On;
          isLrgDed = *On;
       Else;
          lrGDed = *Off;
          isLrgDed = *Off;
          scDedBil = 'S';
       Endif;

       //*** Beg Del ***** 06/23/25 ********************
       // Initialize Losses as of Date to user processing date
       // scLosDt = %Dec(%Char(%Date(UPprDt:*CYMD):*usa0):8:0);

       // If Monthend, determine Losses as of Date
       //If outMthEnd;
       //   outDt = %Dec(%Char(%Date():*usa0):8:0); // Today's Date
       //   outDay = %subdt(%Date():*DAYS); // Today's day value
       //   // Extract Past Number of Days
       //   GetLossDate(outDt:outDay:inErr);
       //   If inErr <> '1';
       //      scLosDt = outDt;
       //   Else;
       //      Clear scLosDt;
       //   Endif;
       //Endif;

       // Initialize A/R Posting Date to user processing date
       // scPosDt = %Dec(%Char(%Date(UPprDt:*CYMD):*usa0):8:0);

       // Initialize Print Date to user processing date
       // scPrtDt = %Dec(%Char(%Date(UPprDt:*CYMD):*usa0):8:0);
       //*** End Del ***** 06/23/25 ********************

       //*** Beg Add ***** 06/23/25 ********************
       // Default 'All Divisions' checkbox to "Checked"
       scAllDiv = 'Y';
       //*** End Add ***** 06/23/25 ********************

       // Clear Invoice Remark Codes
       Clear scInvRmk;
       scDFlg = 'Y';

       // Extract SSN info
       outCo# = %Char(UPco#);
       outRptId = 'W8007R'; // Use old program name
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

       // Retrieve Multi Group Flag
       outPgm = 'W7944R';
       inFlag2 = *Off;
       CheckPgm(outPgm:inFlag2);
       outMultGr = inFlag2;

       // Extract DARTS Remark Code
       Setll (*Loval) pgs00;
       Dou %Eof(WXGSL);
          Read pgs00;
          If %Eof(WXGSL);
             Leave;
          Endif;
       If GSieFl = 'I';
          Chain (GSco#:GSfnd) pex00;
          Select;
          When %found(WMEXL) and EXddrm <> ' ';
             scDrtRmk = EXddrm;
             Leave;
          Endsl;
       EndIf;
       Enddo;

       // Extract text for PRDS
       If Q1prms >= 1;
          outData = inPRDS;
       Else;
          outData = 'Deductible Invoices';
       Endif;

       outLen = 40;
       GetText(outData:outLen);
       scPrDs = outData;

       // Extract Library for the Deductible Detail Report Work File
       GetSuffix(outSuffix);
       scOutFile = 'This option will produce output file WFD1P in Library +
        PDSRPT' + outSuffix + ' for the Deductible Detail report.';

       // If USESPOOL is turned on then make the Resolve Bills to Darts
       // and Darts Remark Code visible on the prompt screen.
       If isUseSpool;
          isRsvDarts = *On;
       Else;
          isRsvDarts = *Off;
       Endif;

     p LoadInitVal     e
       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

       // Process until exit button is pressed.
       Dou ExitNow = *On;

          //*** Beg Del ***** 06/23/25 ********************
          // Retrieve Employer Name
          // Chain (scCo#:scFnd:scEmp#:scDiv) pem00;
          // If %Found(wmeml);
          //    scEmpName  = EMname;
          // Else;
          //    Clear scEmpName;
          // Endif;
          //*** End Del ***** 06/23/25 ********************

          // Display Screen
          Exsr ExfmtScreens;

          //*** Beg Add ***** 06/23/25 ********************
          // Retrieve Employer Name
          Clear scEmpName;
          If scAllDiv = 'Y';
             scDiv = 0;
          Endif;
          If scCo# <> 0 and scFnd <> 0 and scEmp# <> 0;
             Chain (scCo#:scFnd:scEmp#:scDiv) pem00;
             If %Found(wmeml);
                scEmpName  = EMname;
             Endif;
          Endif;
          //*** End Add ***** 06/23/25 ********************

          // Check if Search Criteria was selected
          If selEmpSrch=*On;
             EmployerSrch();
          Endif;

          Select;
          When BtnSubmit = *On;
             If ValidPnl() = *On;
                ProcessScreen();
                Return;
             Endif;
          When tabClick = *On;
             tabClick = *Off;
          When BtnRefresh = *On;
             BtnRefresh = *Off;
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
          APnlHeight = 830;

          Write rcdComPnl;
          Write rcdTabDed;
          Exfmt rcdDedInv;
          Read rcdComPnl;
          Read rcdTabDed;
       EndSl;

       Endsr;

     p Main            e

       //===================================================================
       // EmployerSrch  - Employer/Div Search
       //===================================================================
     p EmployerSrch    b
     d EmployerSrch    pi

       inMode  = 'E';
       inCall2  = '2';
       clear dsSrchCrit;
       clear inSelect;
       dsEmpGrp  = scFnd;
       inSelect  = dsSrchCrit;
       outRrn#   = '000000000';
       EmpSearch(inMode:inCall2:outRrn#:inSelect);

       If outRrn# <> '000000000';
          empRrn=%Dec(outRrn#:9:0);
          Chain empRrn pel0p;
          If %Found(Wdelp);
             intCo#  = ELco#;
             intFnd  = ELfnd;
             intEmp# = ELEmp#;
             intDiv  = ELdiv;

             Chain (intCo#:intFnd:intEmp#:intDiv) pem00;
             If %Found(WMEML);
                scFnd = EMfnd;
                scEmp# = EMemp#;
                scDiv = EMdiv;
             Endif;
          Endif;
       Endif;

     p EmployerSrch    e

       //===================================================================
       // Process user inputs on screen
       //===================================================================
     p ProcessScreen   b
     d ProcessScreen   pi
       //*** Beg Add ***** 12/12/25 ****************************************
     d intUsaDate      s              8  0 Inz
       //*** End Add ***** 12/12/25 ****************************************
       // Display Output Control Window
       xxSubmit = *on;
       If isUseScriptura;
          xxProcType = processType;
          If isSglMod = *On and scEmp# <> 0;
             xxMass = ' ';
          Else;
             xxMass = 'Y';
          Endif;

          In *Lock *dtaara;
          Clear svLDA;
          xxDart = 'N';
          Clear xxRMKCD;
          If scDrtRmk <> ' ';
             xxDart = 'Y';
             xxRMKCD = scDrtRmk;
          Endif;

          Out *dtaara;

          Clear rrnEl;
          If scEmp# <> 0;
             Setgt (scCo#:scFnd:scEmp#:scDiv:intFyr) pel00;
             Readpe (scCo#:scFnd:scEmp#:scDiv) pel00;
             rrnEl = dbrrn2;
          Endif;

          If rrnEl <> 0;
             xxSubmit = OutputWindow(xxProcType:xxMass:' ':' ':rrnEl);
          Else;
             xxSubmit = OutputWindow(xxProcType:xxMass);
          Endif;

          If xxSubmit = *off;
             Return;
          Else;
             In *Lock *dtaara;

             Out *dtaara;
             scDFlg = xxDart;
             scDrtRmk = xxRMKCD;
          Endif;
       Endif;

       // Move the parameter values to call the report CL program
       outCo#     = %editC(scCo#:'X');
       //*** Beg Chg ***** 04/18/25 ****************************************
       If scEmp# <> 0;
          outFnd  = %editC(scFnd:'X');
       Endif;
       //*** End Chg ***** 04/18/25 ****************************************
       outFnd     = %editC(scFnd:'X');
       //*** Beg Chg ***** 12/12/25 ****************************************
       //*** outLosDt   = %editC(scLosDt:'X');
       //*** outPosDt   = %editC(scPosDt:'X');
       //*** outPrtDt   = %editC(scPrtDt:'X');
       intUsaDate = dtCheck(scLosDt);
       outLosDt = %editC(intUsaDate:'X');
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
       Clear outToFnd;
       outExCl    = scExCl;
       Clear outFRgn;
       Clear outTRgn;
       outDFlg    = scDFlg  ;
       outDrtRmk  = scDrtRmk;
       outDedBil  = scDedBil;
       outEmp#    = %editC(scEmp#:'X');
       outDiv     = %editC(scDiv:'X');
       //*** Beg Add ***** 04/11/25 ********************
       outFRgn= '000';
       outTRgn= '000';
       If scEmp# <> 0;
          If scAllDiv = 'Y' or scDiv = 0;
             outDiv = *Blank;
          Endif;
       Else;
          //*** Beg Del ***** 12/12/25 ****************************************
          //*** outFnd = '000';
          //*** End Del ***** 12/12/25 ****************************************
          outDiv = *Blank;
       Endif;
       //*** Beg Add ***** 04/11/25 ********************
       outPrtSsn  = scPrtSsn;

       DedInvpr(outCo#:outFnd:outLosDt:outPosDt:outPrtDt:outInvRmk1:
       outInvRmk2:outInvRmk3:outInvRmk4:outOutq:outToFnd:outExCl:outFRgn:
       outTRgn:outDFlg:outDrtRmk:outDedBil:outEmp#:outDiv:outPrtSsn);
     p ProcessScreen   e
