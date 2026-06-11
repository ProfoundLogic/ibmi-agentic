     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H BNDDIR('WTBNDDIR')
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H***********************************************************************
     H/TITLE WTRPTMBREX - Member Experience Report
     H***********************************************************************
     H*                                                                    *
     H*  DESCRIPTION:                                                      *
     H*                                                                    *
     H*  CALLED FROM:                                                      *
     H*    Menu                                                            *
     H*  CALLS TO:                                                         *
     H*                                                                    *
     H*  CREATED: 10/27/16  PROGRAMMER- C:308 PROJECT: WC2464              *
     H*                                                                    *
     H***********************************************************************
     H* REVISIONS:                                                         *
     H*
     H* 10/07/24 C:300 Project: WC2477
     H*                DSPF: Remove County Code selection fields/links
     H*                                                                    *
     H***********************************************************************
     fWTRPTMBREXcf   e             workstn extfile('WTRPTMBREX')
     f                                     handler('PROFOUNDUI(HANDLER)')
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)

     fWdelp     if   e             disk    Rename(pel01:pel0p)
     fWmaap     if   e             disk    Rename(paa01:paa0p)
     fWmeml     if   e           k disk    Rename(pem01:pem00)
     fWmrel     if   e           k disk    Rename(pre01:pre00)
     fWtabl     if   e           k disk    Rename(pcn01:pcn00)
     fWmfnl     if   e           k disk    Rename(pfn01:pfn00)
     fWtcnl1    if   e           k disk
     dpsds           esds                  extname(szq1p)

     d outsuffix                      3
     d intCo#                         3  0
     d intEmpRrn       s              9  0
     d outOutQ         s             10
     d outCall         s              1
     d outMode         s              1
     d outRrn#         s              9
     d strRgnSel       s           1000
     d strRgnVal       s            300
     d strRgnDsc       s             30
     d strCtyDsc       s             30
     d strMsg          s            100
     d outSelect       ds                  likeds(dsSrchCrit)
     d outExit         s              1
     d outCancel       s              1
     d isEmpNbr        s              1
     d outAgtRrn#      s              9  0
     d outCtExit       s              1
     d outCtCncl       s              1
     d outCtCall       s              1
     d outCtRRN        s              9  0
     d outCtKey        s             10
     d outCtstate      s              2
     d outCompany      s              3
     d outFund         s              3
     d outAcdntDatF    s              8
     d outAcdntDatT    s              8
     d outRunDate      s              8
     d outEmployer     s              9
     d outDivision     s              5
     d outRegionF      s              3
     d outCountyF      s              2
     d outAgentF       s              5
     d outZip          s              5
     d outOpnClaim     s              1
     d outCloseClm     s              1
     d outMonth        s              3
     d outSequence     s              1
     d outMbrName      s             10    Inz(' ')
     d outActAccount   s              1
     d outPremiumF     s             11
     d outPremiumT     s             11
     d outQueue        s             10
     d outDupPrint     s              1
     d outOutPut       s              1
     d outPolFrmDate   s              8
     d outPolTodate    s              8
     d outEmpNoClaim   s              1
     d outPrintSSN     s              1
     d outRegionT      s              3
     d outCrtFile      s              1    Inz('0')
     d rrnWMAGP        s              9  0 Inz(0)
     d rrnWMAAP        s              9  0 Inz(0)
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

      * Agent search procedure
     d AgentSearch     pr

      *** Beg Del *** 10/07/24 *************************************************
      * County Search for Single County procedure
     d***** CountySearch    pr

      * County Select for Multiple County procedure
     d***** CountySelect    pr
     d*****  createFile                    1    Const

      * County Search
     d***** CtySearch       pr                  ExtPgm('W2215R')
     d***** outCtExit                      1
     d***** outCtCncl                      1
     d***** outCtcall                      1
     d***** outCtRRN                       9  0
     d***** outCtKey                       2
     d*****tstate                     2

      * County Search for Multiple County select
     d***** CtySelect       pr                  ExtPgm('W2217C')
     d***** outCtExit                      1
     d***** outCtCncl                      1
     d***** outCtcall                      1
     d***** outCtstate                     2
     d***** outMbrName                    10
     d***** outCrtFile                     1
      *** End Del *** 10/07/24 *************************************************

      * Employer Search Proc.
     d EmployerSrch    pr

      * Employer Search
     d EmpSearch       pr                  ExtPgm('WTSEARCH')
     d outMode                        1
     d outCall                        1
     d outRrn#                        9
     d outSelect                           likeds(dsSrchCrit)

      * Intialize the Screen fields
     d LoadInitVal     pr

      * Main Processing
     d Main            pr

      * Program Start
     d ProcessScreen   pr

      * Program Start
     d ProgramStart    pr

     d SA030R          pr                  extpgm('SA030R')
     d outsuffix                      3

      * Set Criteria
     d SetCriteria     pr

      * Submit CL program
     d SbtCLProc       pr                  extpgm('W7195C')
     d outCompany                     3
     d outFund                        3
     d outAcdntDatF                   8
     d outAcdntDatT                   8
     d outRunDate                     8
     d outEmployer                    9
     d outDivision                    5
     d outRegionF                     3
     d outCountyF                     2
     d outAgentF                      5
     d outZip                         5
     d outOpnClaim                    1
     d outCloseClm                    1
     d outMonth                       3
     d outSequence                    1
     d outMbrName                    10
     d outActAccount                  1
     d outPremiumF                   11
     d outPremiumT                   11
     d outQueue                      10
     d outDupPrint                    1
     d outOutPut                      1
     d outPolFrmDate                  8
     d outPolTodate                   8
     d outEmpNoClaim                  1
     d outPrintSSN                    1
     d outRegionT                     3

      * Validate Panel Sreen Values
     d ValidPnl        pr              n

     d WtRptMbrE       pr                  Extpgm('WTRPTMBREX')
     d  inExt                         1A
     d  inCnl                         1A

     d WtRptMbrE       pi
     d  inExt                         1A
     d  inCnl                         1A

      /Free
       // Start Of Program
       ProgramStart();

       // Main Procesing
       Main();

       *InLr = *on;

      /End-Free
      //*** Beg Del *** 10/07/24 *************************************************
       //===================================================================
       // CountySearch  - To search County on screen
       //===================================================================
      //p CountySearch    b
      //d CountySearch    pi

      ///Free

      // Chain (intCo#:scGrpDsr) pfn00;
      // If %Found (wmfnl);
      //    outCtState = FNMst;
      // Else;
      //    outCtState = *Blanks;
      // EndIf;

      // outCtExit  = *Blanks;
      // outCtCncl  = *Blanks;
      // outCtCall  = '2';
      // outCtRRN   = *zeros;
      // outCtKey   = *Blanks;
      // CtySearch(outCtExit:outCtCncl:outCtCall:outCtRRN:outCtKey:outCtState);

      // scCountyCd = %subst(outCtKey:3:2);

      // Chain (intCo#:outCtKey) pcn01;
      // If %Found(wtcnl1);
      //    scCtyDesc = CNdesc;
      // Else;
      //    scCtyDesc = ' ';
      // EndIf;

      // selCtySrch = *Off;

      ///End-Free
      //p CountySearch    e
       //===================================================================
       // CountySelect  - To select Counties
       //===================================================================
      //p CountySelect    b
      //d CountySelect    pi
      //d  createFile                    1    Const

      //d outCtyExit      s              1    Inz('0')
      //d outCtyCncl      s              1    Inz('0')
      //d outCtyCall      s              1    Inz('2')
      //d error           s               n

      ///Free

          // Get the Group Mailing State
      //    Chain (intCo#:scGrpDsr) pfn00;
      //    If %Found (wmfnl);
      //       outCtState = FNMst;
      //    Else;
      //       outCtState = *Blanks;
      //    EndIf;

      //    outCrtFile = createFile;
      //    error      = *Off;

      //    Monitor;
      //       CtySelect(outCtyExit:outCtyCncl:outCtyCall:outCtState:outMbrName:
      //          outCrtFile);
      //    On-Error;
      //       error = *On;
      //    EndMon;

      //    *inKC = outCtyExit;
      //    *inKL = outCtyCncl;

      //    If outCrtFile = '1' And error = *Off;
      //       scCntySel  = 'Y';
      //       scCountyCd = ' ';
      //       scCtyDesc  = ' ';
      //    Else;
      //       scCntySel  = ' ';
      //    EndIf;

      //    selCtySlct = *Off;

      ///End-Free

      // p CountySelect    e
      //*** End Del *** 10/07/24 *************************************************
       //===================================================================
       // Agent Search  - To search Agent on screen
       //===================================================================
     p AgentSearch     b
     d AgentSearch     pi

     d AArrn           s              9  0
     d error           s              1
     d inKL            s                   like(*inKL)

      /Free

       AArrn = AgtSrchAA(*inkc:inkl:error:scGrpDsr);
       If AArrn <> *zero;
          Chain AArrn paa0p;
          scAgentNbr = AAAgt#;
          scAgtName = GetAgtName(AAagt#:rrnWMAGP:rrnWMAAP:scGrpDsr);
       Else;
          If scAgentNbr = 0;
             scAgtName  = ' ';
          EndIf;
       Endif;

       selAgtSrch = *Off;

      /End-Free
     p AgentSearch     e

       //===================================================================
       // EmployerSrch  - Employer/Div Search
       //===================================================================
     p EmployerSrch    b
     d EmployerSrch    pi

      /Free
       isEmpNbr = '0';
       outMode  = 'E';
       outCall  = '2';
       clear dsSrchCrit;
       clear outSelect;
       dsEmpGrp  = scGrpDsr;
       dsEmpName = scEmpName;
       outSelect  = dsSrchCrit;
       outRrn#   = '000000000';
       EmpSearch(outMode:outCall:outRrn#:outSelect);

       If outRrn# <> '000000000';
          IntEmpRrn=%Dec(outRrn#:9:0);
          Chain  intEmpRrn pel0p;
          If %Found(Wdelp);
             Chain (ELco#:ELfnd:ELemp#:ELdiv) pem00;
             If %Found(Wmeml);
                scEmpNbr   = EMemp#;
                scEmpName  = EMname;
                scDivNbr   = EMdiv;
                isEmpNbr = '1';
             EndIf;
          EndIf;
       EndIf;

       selEmpSrch = *Off;

      /End-Free
     p EmployerSrch    e
       //===================================================================
       // LoadInitVal - Initializing variables
       //===================================================================
     p LoadInitVal     b

      /Free
       clear RcdMbrExpR;
       //initialize all check boxes
       scActAccOn = 'N';
       scOpenClms = 'N';
       scCloseClm = 'N';
       scEmpNoClm = 'N';
       scDupPrint = 'N';
       scPrintSSN = 'N';
       scPrmRngT  = 99999999999;
      /End-Free

     p LoadInitVal     e
       //===================================================================
       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

      /Free

       // Process until exit button is pressed.
       Dou ExitNow = *On;

         // Display Screen
          Exsr ExfmtScreens;

          Select;
             When selEmpSrch = *On;
                EmployerSrch();

             //*** Beg Del *** 10/07/24 *************************************************
             //When selCtySrch = *On;
             //   CountySearch();
             //*** End Del *** 10/07/24 *************************************************

             When selAgtSrch = *On;
                AgentSearch();

             //*** Beg Del *** 10/07/24 *************************************************
             //When selCtySlct = *On;
             //   CountySelect(outCrtFile);
             //*** End Del *** 10/07/24 *************************************************

             When BtnSubmit = *On;
                If ValidPnl() = *On;
                   ProcessScreen();
                   ExitNow = *On;
                EndIf;

             //Do nothing
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
          APnlHeight = 830;

          Write rcdComPnl;
          Write RcdTabMbrE;
          Exfmt RcdMbrExpR;
          Read rcdComPnl;
          Read RcdTabMbrE;

       EndSl;

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
       LoadInitVal();

       // Load Action panel with the Company Name.
       clear scCompName;
       intCo# = userCo#(q1user);
       scCompName = GetCompNam(intCo#);

       // Load Default Group on screen
       SetCriteria();
       scGrpDsr =  userGrp(q1user);

       // Load Output Queue for user.
       outOutQ  = GetUsrOutq(q1user);

       // Load current date
       scAsOfDate = DateTo8(%Date());

       // Determine the library for output
       Sa030r(outSuffix);

       scRptlib = 'This report will produce output file WFE2P in library '+
                  'PDSRPT'+ outSuffix + '.';

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
       //===================================================================
       //===================================================================
       // Validate the screen fields for error
       //===================================================================
     p ValidPnl        b
     d ValidPnl        pi              n
     d isValid         s               n
      /Free
       //Clear Error flag and fields
         BtnSubmit  = *Off;
         msGrpDsr   = *Blanks;
         erGrpDsr   = *Off;
         msPolEftDt = *Blanks;
         erPolEftDt = *Off;
         msPolExpDt = *Blanks;
         erPolExpDt = *Off;
         msAccDtRnf = *Blanks;
         erAccDtRnf = *Off;
         msAccDtRnT = *Blanks;
         erAccDtRnT = *Off;
         msAsOfdate = *Blanks;
         erAsOfdate = *Off;
         msPrmRngF  = *Blanks;
         erPrmRngF  = *Off;
         msPrmRngT  = *Blanks;
         erPrmRngT  = *Off;
         msEmpNbr   = *Blanks;
         erEmpNbr   = *Off;
         msDivNbr   = *Blanks;
         erDivNbr   = *Off;
         msRegionF  = *Blanks;
         erRegionF  = *Off;
         msRegionT  = *Blanks;
         erRegionT  = *Off;
         //*** Beg Del *** 10/07/24 *************************************************
         //msCountyCd = *Blanks;
         //erCountyCd = *Off;
         //msCountyC1 = *Blanks;
         //erCountyC1 = *Off;
         //*** End Del *** 10/07/24 *************************************************
         msAgentNbr = *Blanks;
         erAgentNbr = *Off;
         msActAccOn = *Blanks;
         erActAccOn = *Off;
         msOpenClms = *Blanks;
         erOpenClms = *Off;
         msEmpNoClm = *Blanks;
         erEmpNoClm = *Off;
         msCloseClm = *Blanks;
         erCloseClm = *Off;
         msNbrMonth = *Blanks;
         erNbrMonth = *Off;
         msDupPrint = *Blanks;
         erDupPrint = *Off;
         msPrintSSN = *Blanks;
         erPrintSSN = *Off;
         msOutPut   = *Blanks;
         erOutPut   = *Off;
         msSortSeq  = *Blanks;
         erSortSeq  = *Off;
         msRptLib   = *Blanks;
         erRptLib   = *Off;
         //*** Beg Del *** 10/07/24 *************************************************
         //erCountyC1 = *Off;
         //msCountyC1 = *blanks;
         //*** End Del *** 10/07/24 *************************************************


         // initialize the error indicator
         isValid = *On;

         // validate group on screen
         If scGrpDsr = *zeros;
            erGrpDsr = *On;
            msGrpDsr = 'WCR0503';
            isValid = *Off;
         EndIf;

       // validate Accident from date with Policy Eff date
         If scPolEftDt = *zero ;
            If DtCheck(scAccDtRnF) = 0;
               erAccDtRnF = *On;
               msAccDtRnF  =  'WCR0500';
               isValid = *Off;
            EndIf;
         Else;
         // validate Policy effective date
            If DtCheck(scPolEftDt) = 0;
               erPolEftDt = *On;
               msPolEftDt  =  'WCR0500';
               isValid = *Off;
            EndIf;
            If scAccDtRnF <> *zero and DtCheck(scAccDtRnF) = 0;
               erAccDtRnF = *On;
               msAccDtRnF  =  'WCR0500';
               isValid = *Off;
            Endif;
         EndIf;

       // validate Accident To date with Policy Eff date
         If scPolExpDt = *zero;
            If DtCheck(scAccDtRnT) = 0;
               erAccDtRnT = *On;
               msAccDtRnT = 'WCR0500';
               isValid = *Off;
            EndIf;
         Else;

           // validate Policy Expiry date
            If DtCheck(scPolExpDt) = 0;
               erPolExpDt = *On;
               msPolExpDt  =  'WCR0500';
               isValid = *Off;
            EndIf;

            If scAccDtRnT <> *zero and DtCheck(scAccDtRnT) = 0;
               erAccDtRnT = *On;
               msAccDtRnT = 'WCR0500';
               isValid = *Off;
            EndIf;
         EndIf;

       // Validate Accident date with Policy Eff date
         If erAccDtRnF <> *On   and erAccDtRnT <> *On;
            erAccDtRnF = AscendDates(scAccDtRnF:scAccDtRnT:strMsg);
            If erAccDtRnF = *On;
               erAccDtRnT = erAccDtRnF;
               msAccDtRnT = 'WCR0702';
               msAccDtRnF = 'WCR0702';
               isValid = *Off;
            EndIf;
         EndIf;

         // Validate Policy Eftdate is less than Exp date
         If erPolExpDt = *off and erPolEftDt = *off;
            erPolEftDt = AscendDates(scPolEftDt:scPolExpDt:strMsg);
            If erPolEftDt = *On;
               erPolExpDt = erPolEftDt;
               msPolEftDt = 'WCR0702';
               msPolExpDt = 'WCR0702';
               isValid = *Off;
            EndIf;
         EndIf;

        //Validate As of date or run Date
         If DtCheck(scAsOfDate) = 0;
            erAsOfDate = *On;
            msAsOfDate  =  'WCR0500';
            isValid = *Off;
         EndIf;

        //Validate Premium range
         If scPrmRngT < 1 ;
            erPrmRngT = *On;
            msPrmRngT = 'WCR0502';
            isValid = *Off;
         Else;
            If scPrmRngT < scPrmRngF;
               erPrmRngT = *On;
               msPrmRngT = 'WCR0807';
               isValid = *Off;
            EndIf;
         EndIf;

        //Validate Employer and Division
         If scEmpNbr <> *zero or scDivNbr <> *zero;
             Chain (intCo#:scGrpDsr:scEmpNbr:scDivNbr) pem00;
             If not %Found(wmeml);
                erEmpNbr = *On;
                msEmpNbr = 'WCR0538';
                isValid = *Off;
             EndIf;
         Endif;

        // Region Range must be in ascending order
        If scRegionF > scRegionT;
            erRegionF = *On;
            erRegionT = *On;
            msRegionF = 'WCR0886';
            msRegionT = 'WCR0886';
            isValid = *Off;
        EndIf;

        //*** Beg Del *** 10/07/24 *************************************************
        // County must be blank if county select chosen
        //If scCountyCd <> ' ' and scCntySel = 'Y';
        //    erCountyC1 = *On;
        //    msCountyC1 = 'County Code must be blank, if county select' +
        //                 ' has been chosen.';
        //    isValid = *Off;
        //EndIf;

        // Clear county name if county code is blank.
        //If scCountyCd = ' ';
        //   scCtyDesc = ' ';
        //EndIf;
        //*** End Del *** 10/07/24 *************************************************

        //Validate Type of Output
         If scOutPut = *Blanks;
            erOutPut = *On;
            msOutPut = 'WCR0503';
            isValid = *Off;
         EndIf;

        //Validate SortSequence type
         If scSortSeq# = *Blanks;
            erSortSeq  = *On;
            msSortSeq  = 'WCR0503';
            isValid = *Off;
         EndIf;

         If isValid = *Off;
            Return *Off;
         Else;
            Return *On;
         EndIf;

      /End-Free
     p ValidPnl        e
       //===================================================================
       // Process screen
       //===================================================================
     p ProcessScreen   b
     d ProcessScreen   pi

      /Free

       //*** Beg Del *** 10/07/24 *************************************************
       // Call the County Selection Program
       //If scCntySel = 'Y' And outCrtFile = '0';
       //   CountySelect(outCrtFile);
       //   If outCrtFile <> '0';
       //      outCountyF  = '*S';
       //   EndIf;
       //Else;
       //   If scCntySel = 'Y';
       //      outCountyF = '*S';
       //   Else;
       //      outCountyF  = scCountyCd;
       //   EndIf;
       //EndIf;
       //*** End Del *** 10/07/24 *************************************************
       //*** Beg Add *** 10/07/24 *************************************************
       outCountyF = *Blanks;
       //*** End Add *** 10/07/24 *************************************************

       // Clear error indicator

       outCompany     = %EditC(intCo#:'X');
       outFund        = %EditC(scGrpDsr:'X');
       outAcdntDatF   = %EditC(scAccDtRnF:'X');
       outAcdntDatT   = %EditC(scAccDtRnT:'X');
       outRunDate     = %EditC(scAsOfDate:'X');
       outEmployer    = %EditC(scEmpNbr:'X');
       outDivision    = %EditC(scDivNbr:'X');
       outRegionF     = %EditC(scRegionF:'X');
       outAgentF      = %EditC(scAgentNbr:'X');
       outZip         = %EditC(scZipCode:'X');
       outOpnClaim    = scOpenClms;
       outCloseClm    = scCloseClm;
       outMonth       = %EditC(scNbrMonth:'X');
       outSequence    = scSortSeq#;
       outActAccount  = scActAccOn;
       outPremiumF    = %EditC(scPrmRngF:'X');
       outPremiumT    = %EditC(scPrmRngT:'X');
       outQueue       = outOutQ;
       outDupPrint    = scDupPrint;
       outOutPut      = scOutPut;
       outPolFrmDate  = %EditC(scPolEftDt:'X');
       outPolToDate   = %EditC(scPolExpDt:'X');
       outEmpNoClaim  = scEmpNoClm;
       outPrintSSN    = scPrintSSN;
       outRegionT     = %EditC(scRegionT:'X');

       SbtCLProc(outCompany:outFund:outAcdntDatF:outAcdntDatT:outRunDate:
        outEmployer:outDivision:outRegionF:outCountyF:outAgentF:outZip:
        outOpnClaim:outCloseClm:outMonth:outSequence:outMbrName:outActAccount:
        outPremiumF:outPremiumT:outQueue:outDupPrint:outOutPut:outPolFrmDate:
        outPolToDate:outEmpNoClaim:outPrintSSN:outRegionT);

       //*** Beg Del *** 10/07/24 *************************************************
       // Remove County Select if not used
       //If outCrtFile = '1' And scCntySel <> 'Y';
       //   CountySelect('D');
       //EndIf;
       //*** End Del *** 10/07/24 *************************************************

      /End-Free
     p ProcessScreen   e
