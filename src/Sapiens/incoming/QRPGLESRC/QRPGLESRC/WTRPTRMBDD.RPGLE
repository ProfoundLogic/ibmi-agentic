     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H BNDDIR('WTBNDDIR')
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE WTREMDED - PROMPT FOR REIMBURSABLE DED DETAIL REPORT
     H*****************************************************************    **
     H*  DESCRIPTION:  This program prompts for selection criteria to run *
     H*                the Reimbursable Deductible Report.                *
     H*                                                                   *
     H*  CALLED FROM:                                                     *
     H*    MENU                                                           *
     H*                                                                   *
     H*  CALLS TO:                                                        *
     H*                                                                   *
     H*                                                                   *
     H*  REWRITE:               C:298     PROJECT: WC2416                 *
     H*********************************************************************
     H* REVISIONS:
     H*                                                                    *
     H* 01/20/25 C:333 Project: WC2494                                     *
     H*                Deductible Invoices and Detail Report in Scriptura  *
     H*                                                                    *
     H***********************************************************************
     H* INDICATORS:
     H***********************************************************************
      *                                                                    *
     fWTRPTRMBDDcf   e             workstn extfile('WTRPTRMBDD')
     f                                     handler('PROFOUNDUI(HANDLER)')
     fWtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)
     fwpigenpop cf   e             workstn extfile('WPIGENPOP')
     f                                     HANDLER('PROFOUNDUI(HANDLER)')
     fWmfnl     if   e           k disk    Rename(pfn01:pfn00)
     fWmdbl     if   e           k disk    Rename(pdb01:pdb00)
     fWmdcl     if   e           k disk    Rename(pdc01:pdc00)
     fWmupl     if   e           k disk    Rename(pup01:pup00)
      *** Beg Add ***** 01/20/25 **************************************
     fWXGSL     IF   E           K DISK
     f                                     RENAME(PGS01:PGS00)
      * Admin/Darts Control File
     fWMEXL     IF   E           K DISK    RENAME(PEX01:PEX00)
      *** End Add ***** 01/20/25 **************************************

     d/copy scopypr01d
     d/copy sPrCompany
     d/copy wPrEmpLr
     d/copy wPrUsrprms
     d/copy sPrDate
     d/copy wPrPolErr
      *** Beg Add ***** 01/20/25 *****************************
     D/copy sPrLetter
     D/copy sPrXML
     D isUseScript     S              1n   inz
     D processType     c                   const('Deductible Invoice')
     D xxProcType      s            100
     D xxSubmit        s              1n
     D xxMass          s              1
     D
     D SVLDA           DS          1024    DTAARA(*LDA)
     D xxDart                         1    overlay(svlda:251)
     D xxRMCD                         5    overlay(svlda:252)
     D xxCopy                         1    overlay(svlda:257)
     D xxPrint                        1    overlay(svlda:258)
     D xxPrinter                     10    overlay(svlda:259)
     D xxTray                        50    overlay(svlda:269)
     D xx#CopiesA                     3    overlay(svlda:319)
     D xxEmail                        1    overlay(svlda:322)
     D xxToEmail                    200    overlay(svlda:323)
     D xxSubject                    150    overlay(svlda:523)
     D xxBody                       350    overlay(svlda:673)
      *** End Add ***** 01/20/25 *****************************

     d UsrSecVer       pr                  Extpgm('WA097R')
     d  outCo#                        3
     d  outPgmId                     10
     d  inDft                         1
     d  inChg                         1

     d RemDedSubm      pr                  Extpgm('W7935C1')
     d  outCo#                        3
     d  outDate                       8
     d  outType                       1
     d  outOutq                      10
     d  outDdTyp                      1
     d  outPrtSSN                     1
     d  outLossDt                     8
     d  outExdflg                     1

      * Intialize the Screen fields
     d LoadInitVal     pr

      * Main Processing
     d Main            pr

      * Program Start
     d ProgramStart    pr

      * Process other options
     d*ProcOther       pr

      * Set Criteria
     d SetCriteria     pr

      * Validate Panel Sreen Values
     d ValidPnlPH      pr

      * Reimbursable Deductible Report
     d RemDedRptPr     pr

      **
     d outCo#          s              3
     d intCo#          s              3  0
     d outPgmId        s             10
     d inDft           s              1
     d inChg           s              1
     d  outDate        s              8
     d  outType        s              1
     d  outOutq        s             10
     d  outDdTyp       s              1
     d  outPrtSSN      s              1
     d  outLossDt      s              8
     d  outExdflg      s              1
     d strMsg          s            100
     d isValidS        s               n   Inz(*off)
      /Free
       // Start Of Program
       ProgramStart();

       // Main Procesing
       Main();

       *InLr = *on;

      /End-Free

       //===================================================================
       // LoadInitVal - Intialize the Screen fields
       //===================================================================
     p LoadInitVal     b
     d LoadInitVal     pi

      /Free
       scPrEndDt = *Zero;
       scLossDt = *Zero;
       scRunType = *Blanks;
       scDedBlTyp = *Blanks;
       scPrintSSN = *Blanks;
       scExPolAct = *Blanks;
       erPrEndDt = *off;
       erLossDt  = *off;
       erRnType  = *off;
       erDedBlTyp = *off;
       erPrtSSN = *off;
       erExPolAct = *off;
       erExclude  = *off;
       msPrEndDt = *blanks;
       msLossDt = *blanks;
       msRnType = *blanks;
       msDedBl = *blanks;
       msPrtSSN = *blanks;
       msExc = *blanks;
       isValidS = *off;
      /End-Free
     p LoadInitVal     e

       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /Free

       // Intialize the Screen fields
       LoadInitVal();

       // Load Action panel with the Company Name.
       intCo# = userCo#(q1user);
       scCompName = GetCompNam(intCo#);
       inDft = *Blanks;
       inChg = *Blanks;
       outPgmId = 'W7935R1';
       outCo# = %EditC(intCo#:'X');
       UsrSecVer(outCo#:outPgmId:inDft:inChg);
       scPrintSSN = inDft;
       //*** Beg Add ***** 01/20/25 ***************************************
       isUseScript = UseScriptura(processType);
       //*** End Add ***** 01/20/25 ***************************************
      /End-Free
     p ProgramStart    e

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

      /Free

       // Process until exit button is pressed.
       Dou BtnCancel  = *On ;

           // Display Screen
           Exsr ExfmtScreens;

           Select;

           When BtnSubmit = *On;
              ValidPnlPH();
              If isValidS = *Off;
              //*** Beg Chg ***** 01/20/25 ***********************
              //***   RemDedRptPr();
              //***   Leave;
                 xxSubmit = *on;
                 If isUseScript;
                    xxMass = 'Y';
                    xxProcType = processType;
                    In *Lock *dtaara;
                     xxDart = 'N';
                     xxRMCD = ' ';
                    Out *dtaara;
                    xxSubmit = OutputWindow(xxProcType:xxMass);
                    If xxSubmit = *on;
                       RemDedRptPr();
                       Leave;
                    Endif;
                 Else;
                    RemDedRptPr();
                    Leave;
                 Endif;
              //*** End Chg ***** 01/20/25 ***********************
              Endif;

           When BtnRefresh = *On;
              BtnRefresh =*Off;
              // Intialize the Screen fields
              LoadInitVal();
              // Reload the program start values gain.
              ProgramStart();
           Other;
           Endsl;
       Enddo;
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

          APnlHeight = 830;

          Write rcdComPnl;
          Write RcdTabRemD;
          Exfmt RcdRemDedR;
          Read rcdComPnl;
          Read RcdTabRemD;

       Endsr;

      /End-Free

     p Main            e
       //===================================================================
       // ValidPnlPH - Validate Panel 1 Screen Fields
       //===================================================================
     p ValidPnlPH      b
     d ValidPnlPH      pi

      /Free

       erPrEndDt = *off;
       erLossDt  = *off;
       erRnType  = *off;
       erDedBlTyp = *off;
       erPrtSSN = *off;
       erExPolAct = *off;
       erExclude  = *off;
       msPrEndDt = *blanks;
       msLossDt = *blanks;
       msRnType = *blanks;
       msDedBl = *blanks;
       msPrtSSN = *blanks;
       msExc = *blanks;
       isValidS = *off;
       scPleft = 360;
       scPtop = 215;
       scMLeft = 370;
       scMTop  = 260;
       scOkLeft = 515;
       scOkTop  = 340;
       scCnLeft = 625;
       scCnTop  = 340;

         //Date Validation
              erPrEndDt = ValidDate(scPrEndDt:strMsg);
              If erPrEndDt = *On;
                 msPrEndDt = strMsg;
                 isValidS = *On;
              Endif;

              erLossDt = ValidDate(scLossDt:strMsg);
              If erLossDt = *On;
                 msLossDt = strMsg;
                 isValidS = *On;
              Endif;

              If scRunType = *Blanks;
                 erRnType = *On;
                 msRnType = 'WCR0503';
                 isValidS = *On;
              Endif;

              If scDedBlTyp = *Blanks;
                 erDedBlTyp = *On;
                 msDedBl = 'WCR0503';
                 isValidS = *On;
              Endif;

              If inChg = '0';
                 If scPrintSSN = *Blanks;
                    erPrtSSN = *On;
                    msPrtSSN = 'WCR0503';
                    isValidS = *on;
                 Endif;

                 If (scPrintSSN <> inChg) and (scPrintSSN = 'Y');
                    clear scYNCMsg;
                    clear scBtn1Txt;
                    clear scBtn2Txt;
                    scBtn1Txt = 'Enter';
                    scBtn2Txt = 'Cancel';
                    scOkCnTtl  = 'Information';
                    scOkCnMsg  = 'Social Security Number will be visible on ' +
                     'report. Press enter to continue. ';
                    btnOk = *off;
                    btnCancel = *off;
                    dou btnOk = *on or btnCancel = *on;
                       Exfmt rcdTwoBtn;
                       Select;
                          When btnCancel = *on;
                             btnCancel = *off;
                             isValidS = *on;
                             Leave;
                       Endsl;
                    Enddo;
                 Endif;
              Endif;

              If scExPolAct <> *blank and scExPolAct <> 'Y';
                 erExPolAct = *On;
                 msExc = 'WCR0571';
                 isValidS = *On;
              Endif;

              If scExPolAct = 'Y' and (scRunType = 'A' or scRunType = 'N');
                 erExclude = *On;
                 isValidS = *On;
              Endif;
      /End-free

     p ValidPnlPH      e

     p RemDedRptPr     b
     d RemDedRptPr     pi

      /Free
       Chain q1user pup00;
       outCo# = %EditC(intCo#:'X');
       outDate = %EditC(scPrEndDt:'X');
       outLossDt = %EditC(scLossDt:'X');
       RemDedSubm(outCo#:outDate:scRunType:UPoutq:scDedBlTyp:
        scPrintSSN:outLossDt:scExPolAct);

       BtnCancel = *on;
      /End-free

     p RemDedRptPr     e
