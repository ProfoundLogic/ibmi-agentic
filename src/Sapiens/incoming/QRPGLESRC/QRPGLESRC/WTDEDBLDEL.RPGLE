     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H BNDDIR('WTBNDDIR')
     H/define CONVERT
     H*
     H***********************************************************************
     H/TITLE WTDEDBLDEL - PROMPT FOR DEDUCTIBLE BILLING DELETION.          *
     H***********************************************************************
     H*                                                                    *
     H*  PROGRAM DESCRIPTION:                                              *
     H*  THIS PGM PROMPTS THE USER FOR THE DEDUCTIBLE BILLING DELETION.    *
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
     H*                1. Psss all dates into CYMD 7A format to W7323CA    *
     H*                                                                    *
     H* 11/23/25 C:300 Project: WC2494, Support: 120137                    *
     H*                Deductible Billing Deletion Error                   *
     H*                Problem - Its not passing Group Number to W7323CA   *
     H*                Fix - Revert 04/21/25 change to pass Group Number   *
     H*                                                                    *
     H* 06/23/25 C:300 Project: WC2494, Support: 119263                    *
     H*                1) Default 'All Divisions' checkbox to "Checked"    *
     H*                2) Employer Name getting clear on changing Division *
     H*                   field or 'All Divisions' checkbox                *
     H*                DSPF Changes:                                       *
     H*                1) Commented 'Onload' changes from RcdTabDed rcdfmt *
     H*                2) Changes in 'onchange' events of Employer Number, *
     H*                   Division and 'All Divisions' Checkbox -- To not  *
     H*                   clear Division, 'All Divisions' and Employer Name*
     H*                3) Changed Value attributes for Group Number,       *
     H*                   Employer Number and Division Number fields       *
     H*                4) Moved Search icon from out of tabbing order      *
     H*                                                                    *
     H* 04/21/25 C:344 Project: WC2989                                     *
     H*                Populate Group parm only if Employer# is has been   *
     H*                entered.                                            *
     H*                                                                    *
     H***********************************************************************
     fwtdedbldelcf   e             workstn extfile('WTDEDBLDEL')
     f                                     handler('PROFOUNDUI(HANDLER)')
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)
     fwxgsl     if   e           k disk    Rename(pgs01:pgs00)
     fwdelp     if   e             disk
     f                                     Rename(pel01:pel0p)
     f                                     Infds(dbfds1)
     fwmeml     if   e           k disk    Rename(pem01:pem00)
     fwdddl9    if   e           k disk    Rename(pdd01:pdd00)

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

     d xxProcType      s            100
     d xxSubmit        s              1n
     d xxMass          s              1

     d SVLDA           ds          1024    DTAARA(*LDA)
     d xxDart                         1    overlay(svlda:251)
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
     d inFLAG                         1A

      // Verify Deductible Detail Record
     d GetDdpCnt       pr
     d  xxPosDt                       7  0
     d  xxDiv                         5  0
     d  xxCount                        n
     d  xxTransfer                     n

      *** Beg Del ***** 12/12/25 ****************************************
      * Report Submit Program CL
     d***** DedDelPr        pr                  ExtPgm('W7323CA')
     d*****  outCo#                        3A
     d*****  outFnd                        3A
     d*****  outEmp#                       9A
     d*****  outDiv                        5A
     d*****  outPosDt                      8A
      *** End Del ***** 12/12/25 ****************************************


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

      * Deductible Invoice Deletion Submit
     d DedInvDelPr     pr                  ExtPgm('W7323CA')
     d  outCo#                        3A
     d  outFnd                        3A
     d  outEmp#                       9A
     d  outDiv                        5A
      *** Beg Chg ***** 12/12/25 ****************************************
     d*****  outPosDt                      8A
     d  outPosDt                      7A
      *** End Chg ***** 12/12/25 ****************************************

     d WtDedBlDelR     pr                  Extpgm('WTDEDBLDEL')
     d  inPrDs                       40A
     d  inCall                        1A

     d WtDedBlDelR     pi
     d  inPrDs                       40A
     d  inCall                        1A

     d outLib          s             10A   Inz
     d outLibl         s              4A   Inz
     d outAppSfx       s              1A   Inz
     d inFlag          s              1A   Inz
     d intGrpCnt       s              2  0 Inz
     d intCo#          s                   Like(ELco#) Inz
     d intFnd          s                   Like(ELfnd) Inz
     d intEmp#         s                   Like(ELemp#) Inz
     d intDiv          s                   Like(ELdiv) Inz
     d isVisbEmp       s              1n   Inz
     d inMode          s              1    Inz
     d outRrn#         s              9    Inz
     d inSelect        s           1024    Inz
     d rrnEmp          s              9  0 Inz
     d outCo#          s              3A   Inz
     d outFnd          s              3A   Inz
     d outLosDt        s              8A   Inz
     d outPosDt        s              8A   Inz
     d outPrtDt        s              8A   Inz
     d outOutq         s             10A   Inz
     d outToFnd        s              3A   Inz
     d outExCl         s              1A   Inz
     d outDFlg         s              1A   Inz
     d outEmp#         s              9A   Inz
     d outDiv          s              5A   Inz
     d outRptId        s             10A   Inz
     d outDt           s              8  0 Inz
     d outDay          s              5  0 Inz
     d inErr           s              1    Inz
     d outData         s             80A   Inz
     d outLen          s              3  0 Inz
     d empRrn          s              9  0 Inz
     d inCall2         s              1    Inz
     d inDiv           s              5    Inz
     d indexTab        s              5  0 Inz
     d xxPosDt         s              7  0 Inz
     d xxDiv           s              5  0 Inz
     d xxBat#          s              9  0 Inz
     d xxcount         s               n   inz
     d xxtransfer      s               n   inz
     d isError         s               n   inz

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
       Clear erFnd;
       Clear erEmp#;
       Clear erAllDiv;
       Clear erPosDt;

       // Initialize error messages
       Clear msFnd;
       Clear msEmp#;
       Clear msAllDiv;
       Clear msPosDt;

      // Validate Group
       If scFnd   = *Zeros;
          erFnd   =  *On;
          msFnd   = 'WCR0503';
          isError = *On;
       Else;

          Select;
          When scEmp# <> 0;
             // Validate Employer/Division
             If ValidEmp(scEmp#:scDiv:EM_rrn:scFnd:scCo#) = *Off;
               erEmp#  =  *On;
                msEmp#  = 'WCR0538';
                isError = *On;
             Else;

               // Both Division and 'All Divisions' selected
               If scDiv <> 0 and scAllDiv <> ' ';
                 erAllDiv =  *On;
                  msAllDiv = 'WC40013';
                  isError  = *On;
               Endif;
             Endif;
          Other;
             Clear scDiv;
          Endsl;
       Endif;

       // Validate A/R Posting Date
       intUsaDate = dtCheck(scPosDt);
       If intUsaDate = 0;
          erPosDt = *On;
          msPosDt = 'WCR0500';
          isError = *On;
       Else;
          // Convert Post Date to 7 digits
          xxPosDt = dtCymd(scPosDt);

          // Invoice cannot be deleted because payments have posted
          Setll (scCo#:scFnd:xxPosDt:scEmp#:scDiv) pdd00;
          If %equal(WDDDL9);
             erPosDt = *On;
             msPosDt = 'WCR2265';
             isError = *On;
          Endif;

          // Verify Deductible Detail records for given A/R Posting Date
          xxDiv = scDiv;
          GetDdpCnt(xxPosDt:xxDiv:xxCount:xxTransfer);

          // No recs found for the posting date requested
          If xxCount = *Off;
             erPosDt = *On;
             msPosDt = 'WCR3149';
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

       Clear RcdDedDel;
       scCo# = userCo#(q1user);

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
          Endif;
       Enddo;

       // Retrieve Company Name
       scCompName = GetCompNam(scCo#);

       // Initialize Employer Name
       Clear scEmpName;

       // Initialize A/R Posting Date
       Clear scPosDt;

       //*** Beg Add ***** 06/23/25 ********************
       // Default 'All Divisions' checkbox to "Checked"
       scAllDiv = 'Y';
       //*** End Add ***** 06/23/25 ********************

       // Extract text for PRDS
       If Q1prms >= 1 and inPRDS <> ' ';
          outData = inPRDS;
       Else;
          outData = 'Deductible Billing Deletion';
       Endif;

       outLen = 40;
       GetText(outData:outLen);
       scPrDs = outData;

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

             When TabClick = *On;
                tabClick = *Off;

          EndSl;

       Enddo;
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       Select;
       When indexTab = 0;
          Tabheight = 770;
          APnlHeight = 830;

          Write rcdComPnl;
          Write rcdTabDel;
          Exfmt rcdDedDel;
          Read rcdComPnl;
          Read rcdTabDel;

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
       // Get count of Deductible Detail records
       //===================================================================
     p GetDdpCnt       B
     d
     d GetDdpCnt       PI
     d   x1PosDt                      7  0
     d   x1Div                        5  0
     d   x1Count                       n
     d   x1Transfer                    n
     d inputSrchCnd    s            256a
     d SqlStmtStr      s            256a
     d openArrChar     s              3a   inz
     d openArray       s               n   dim(3)
     d i               s              1  0 inz

       if scFnd  <> 0;
          inputSrchCnd = 'DDFND = ? AND DDPODT = ?';
          openArray(1) = *on;
       Endif;

       if scEmp# <> 0;
          inputSrchCnd = %trim(inputSrchCnd) +
          ' and ' + 'DDEMP# = ?';
          openArray(2) = *on;
       Endif;

       if scDiv <> 0;
          inputSrchCnd = %trim(inputSrchCnd) +
          ' and ' + 'DDDIV  = ?';
          openArray(3) = *on;
       Endif;

       Exec Sql declare DARTScursor cursor with Hold for dynSqlStmt;

       sqlStmtStr = 'Select DISTINCT(DDBAT#)' +
        ' from  WDDDP where ' + inputSrchCnd;

       Exec SQL prepare dynSqlStmt from :sqlStmtStr;

       // OPEN THE CURSOR DEPENDING ON SCREEN PARAMETERS
       For i = 1 to %len(openArrChar);
          %subst(openArrChar:i:1) = openArray(i);
       Endfor;

       Select;
       When openArrChar = '100';
          Exec SQL open DARTScursor using :scfnd,:x1PosDt;
       When openArrChar = '110';
          Exec SQL open DARTScursor using :scfnd,:x1PosDt,:scemp#;
       When openArrChar = '111';
          Exec SQL open DARTScursor using :scfnd,:x1PosDt,:scemp#,:x1div;
       Endsl;

       // First time fetch the cursor
       Exec SQL fetch next from DARTScursor into :xxBAT#;

       Select;
       When SQLCODE = 0;
          x1Count = *On;
       Other;
          x1Count = *Off;
       EndSl;

       Exec SQL close DARTScursor;

     p GetDdpCnt       E

       //===================================================================
       // Process user inputs on screen
       //===================================================================
     p ProcessScreen   b
     d ProcessScreen   pi
       //*** Beg Add ***** 12/12/25 ****************************************
     d intUsaDate      s              8  0 Inz
       //*** End Add ***** 12/12/25 ****************************************

        // Move the parameter values to call the report CL program
        outCo#     = %editC(scCo#:'X');
       //*** Beg Chg ***** 04/21/25 ****************************************
       //*** Beg Chg ***** 11/23/25 ****************************************
       //*** If scEmp# <> 0;
       //***    outFnd     = %editC(scFnd:'X');
       //*** Endif;
       outFnd     = %editC(scFnd:'X');
       //*** End Chg ***** 11/23/25 ****************************************
       //*** End Chg ***** 04/21/25 ****************************************
       outEmp#    = %editC(scEmp#:'X');
       outDiv     = %editC(scDiv:'X');

       // Check if all divisions were selected
       If scEmp# <> 0;
          If scAllDiv = 'Y' or scDiv = 0;
             outDiv = *Blank;
          Endif;
       Else;
          //*** Beg Del ***** 11/23/25 ****************************************
          //*** outFnd = '000';
          //*** End Del ***** 11/23/25 ****************************************
          outDiv = *Blank;
       Endif;

       //*** Beg Chg ***** 12/12/25 ****************************************
       //*** outPosDt = %char(%date(scPosDt:*usa) :*cymd0);
       outPosDt = %editC(xxPosDt:'X');
       //*** End Chg ***** 12/12/25 ****************************************

       DedInvDelpr(outCo#:outFnd:outEmp#:outDiv:outPosDt);

     p ProcessScreen   e
