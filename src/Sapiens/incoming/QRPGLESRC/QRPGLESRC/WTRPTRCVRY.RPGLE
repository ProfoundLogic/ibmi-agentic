     hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h*****************************************************************
     h dftactgrp(*no) actgrp(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     H*****************************************************************
     H/TITLE WTRPTRCVRY - Prompt for Recovery Report
     h*****************************************************************
     H*                                                                    *
     H*  CALLED FROM:                                                      *
     H*    Menu                                                            *
     H*  CALLS TO:                                                         *
     H*    W7240C                                                          *
     H*                                                                    *
     H*  CREATED: 10/28/16  PROGRAMMER- C:300                              *
     H*  PROJECT: WC2462 - Prompt for Recovery Report                      *
     H*                                                                    *
     H***********************************************************************
     h* Revisions                                                     *
     H*
     H* 10/07/24 C:300 Project: WC2477
     H*                DSPF: Remove County Code range selection fields
     h*                                                               *
     h*                                                               *
     H***********************************************************************

     fWTRPTRCVRYcf   e             workstn extfile('WTRPTRCVRY')
     f                                     handler('PROFOUNDUI(HANDLER)')
     fWtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdPolPnl)
     f                                     include(rcdQtePnl)
     f                                     include(rcdComPnl)
     f*
     f* User Parameter Master File
     fWmeml     IF   E           k DISK    Rename(pem01:pem00)
     fWmupl     IF   E           K DISK    Rename(pup01:pup00)
     fWdelp     if   e             disk    Rename(pel01:pel0p)

     dpsds           esds                  extname(szq1p)

      *
     d WtRcvrRpt       pr                  Extpgm('WTRPTRCVRY')
     d
     d WtRcvrRpt       pi

     d intCo#          s                   Like(EMco#)
     d intEmp#         s                   Like(EMemp#)
     d intDiv          s                   Like(EMdiv)
     d intFnd          s                   Like(ELfnd)
     d strOutq         s                   Like(Upoutq)
      *
     d outRrn#         s              9
     d intEmpRrn       s              9  0
     d outSelect       s           1024
     d isValid         s               n   Inz(*off)
     d isValid2        s               n   Inz(*off)
      *
     d outCo#          s              3
     d outFnd          s              3
     d outData         s             70
     d outOutq         s             10
     d outMode         s              1

     d outKc           s              1
     d outKl           s              1
     d outCall         s              1
     d outCall2        s              1
     d outRrn          s              9  0
     d outCnt          s             10
     d OutSt           s              2
     d outView         s              1  0
     d outEmpNbr       s              9  0
     d outDivNbr       s              5  0
       // outDataDS
     D                 DS
     D  DATA                   1     70
     D  scSort                 1      1
     D  scEmpNbrF              2     10  0
     D  scEmpNbrT             11     19  0
     D  scDivNbrF             20     24  0
     D  scDivNbrT             25     29  0
      *** Beg Chg *** 10/07/24 *************************************************
     D***** scFCnt                30     33
     D***** scTCnt                34     37
     D  strFCnt               30     33
     D  strTCnt               34     37
      *** End Chg *** 10/07/24 *************************************************
     D  scFChp                38     40  0
     D  scTChp                41     43  0
     D  scAgtNoF              44     48  0
     D  scAgtNoT              49     53  0
     D  inSTAT                54     55
     D  inFEAD                56     58
     D  inATTY                59     67  0
     D  inMOD                 68     70  0
      *****************************************************************
      * Prototype Declaration
      *****************************************************************

     d/copy sPrString
     d/copy sPrCompany
     d/copy wPrAgent
     d/copy wPrUsrPrms
     d/copy wPrPolErr
     d/copy WTSearchDs
     d/copy wPrEmpLr

      * Clear Error/Message fields
     d ClrErr          pr

      *** Beg Del *** 10/07/24 *************************************************
      * County Code Search
     d***** CntCdSrch       pr                  ExtPgm('W2215R')
     d***** outKc                          1
     d***** outKl                          1
     d***** outCall2                       1
     d***** outRrn                         9  0
     d***** outCnt                        10
     d***** OutSt                          2
     d***** outView                        1  0

      * County Code Search Proc.
     d***** CountyCdSrch    pr            10
      *** End Del *** 10/07/24 *************************************************

      * Employer Search Proc.
     d EmployerSrch    pr
     d outEmpNum                      9  0
     d outDivNbr                      5  0

      * Employer Search
     d EmpSearch       pr                  ExtPgm('WTSEARCH')
     d outMode                        1
     d outCall                        1
     d outRrn#                        9
     d outSelect                           likeds(dsSrchCrit)

      * Get client ID
      * Intialize the Screen fields
     d LoadInVal       pr

      * Main Processing
     d Main            pr

      * Program Start
     d ProgramStart    pr

      * Recovery Report proc.
     d RcvRptproc      pr

      * Recovery Report Submit
     d RcvRptSbm       pr                  ExtPgm('W7240C')
     d outCo#                         3
     d outFnd                         3
     d outData                       70
     d outOutq                       10
     d outMode                        1

      * Set Criteria
     d SetCriteria     pr

      * Validate Panel Sreen Values
     d ValidPnl        pr              n

      /Free
       // Start Of Program
       ProgramStart();

       // Main Procesing
       Main();

       *InLr = *on;
      /End-Free
       //===================================================================
       // ClrErr - Clear Error/Message fields
       //===================================================================
     p ClrErr          b
     d ClrErr          pi
      /Free

       isValid   = *off;
       isValid2  = *off;
       msFnd     = *blanks;
       erFnd     = *off;
       msSort    = *blanks;
       erSort    = *off;
       msEmpNbrF = *blanks;
       erEmpNbrF = *off;
       msDivNbrF = *blanks;
       erDivNbrF = *off;
       msEmpNbrT = *blanks;
       erEmpNbrT = *off;
       msDivNbrT = *blanks;
       erDivNbrT = *off;
       //*** Beg Del *** 10/07/24 *************************************************
       //msFCnt    = *blanks;
       //erFCnt    = *off;
       //msTCnt    = *blanks;
       //erTCnt    = *off;
       //*** End Del *** 10/07/24 *************************************************
       msFChp    = *blanks;
       erFChp    = *off;
       msTChp    = *blanks;
       erTChp    = *off;
       msAgtNoF  = *blanks;
       erAgtNoF  = *off;
       msAgtNoT  = *blanks;
       erAgtNoT  = *off;

      /End-Free
     p ClrErr          e
       //*** Beg Del *** 10/07/24 *************************************************
       //===================================================================
       // CountyCdSrch   - County Code Search Proc
       //===================================================================
       //p CountyCdSrch    b
       //d CountyCdSrch    pi            10
       //
       ///Free
       //outKc   = ' ';
       //outKl   = ' ';
       //outCall2 = '2';
       //outRrn   = *Zeros;
       //outCnt   = *Blanks;
       //OutSt    = *Blanks;
       //outView = 2;
       //CntCdSrch(outKc:outKl:outCall2:outRrn:outCnt:outSt:outView);
       //
       //Return outCnt;
       ///End-Free
       //p CountyCdSrch    e
       //*** End Del *** 10/07/24 *************************************************
       //===================================================================
       // EmployerSrch  - Employer/Div Search
       //===================================================================
     p EmployerSrch    b
     d EmployerSrch    pi
     d  outEmpNbr                     9  0
     d  outDivNbr                     5  0

      /Free
       outEmpNbr = 0;
       outDivNbr = 0;

       outMode = 'E';
       outCall = '2';
       clear dsSrchCrit;
       clear outSelect;
       dsEmpGrp  = UserGrp(q1user);
       dsEmpName = *Blanks;
       outSelect = dsSrchCrit;
       outRrn#   = '000000000';
       EmpSearch(outMode:outCall:outRrn#:outSelect);

       If outRrn# <> '000000000';
          intEmpRrn=%Dec(outRrn#:9:0);
          Chain(n) intEmpRrn pel0p;
          If %Found(Wdelp);
             intCo#  = ELco#;
             intFnd  = ELfnd;
             intEmp# = ELEmp#;
             intDiv  = ELdiv;

             Chain(n)(intCo#:intFnd:intEmp#:intDiv) pem00;
             If %Found(Wmeml);
                outEmpNbr  = EMemp#;
                outDivNbr  = EMdiv;
             EndIf;
          EndIf;

       EndIf;

      /End-Free

     p EmployerSrch    e
       //===================================================================
       // LoadInVal - Intialize the Screen fields
       //===================================================================
     p LoadInVal       b
     d LoadInVal       pi
      /Free
       Clear RcdRcvrRpt;
      /End-Free
     p LoadInVal       e
       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

      /Free

       // Process until exit button is pressed.
       Dou ExitNow = *On ;

          SetCriteria();

          // Display Screen
          Exsr ExfmtScreens;

          Select;
             When selEmpSrcF = *On;
                selEmpSrcF = *Off;
                EmployerSrch(outEmpNbr:outDivNbr);
                scEmpNbrF = outEmpNbr;
                scDivNbrF = outDivNbr;

             When selEmpSrcT = *On;
                selEmpSrcT = *Off;
                EmployerSrch(outEmpNbr:outDivNbr);
                scEmpNbrT = outEmpNbr;
                scDivNbrT = outDivNbr;

             //*** Beg Del *** 10/07/24 *************************************************
             //When selFCnt = *On;
             //   selFCnt = *Off;
             //   scFCnt = CountyCdSrch();
             //
             //When selTCnt = *On;
             //   selTCnt = *Off;
             //   scTCnt = CountyCdSrch();
             //*** End Del *** 10/07/24 *************************************************

             When BtnSubmit = *on;
                BtnSubmit = *off;
                // Clear Errors
                ClrErr();
                // Validate Edits
                If ValidPnl() = *On;
                   RcvRptproc();
                   Leave;
                EndIf;
             When BtnCancel = *on;
                Leave;

             // do nothing - catch tab click event
             When tabClick = *on;
                tabClick = *off;
             When BtnRefresh = *On;
                BtnRefresh =*Off;
                // Intialize the Screen fields
                LoadInVal();
                ClrErr();
                // Load Action panel with the Company Name.
                intCo# = userCo#(q1user);
                scFnd  = userGrp(q1user);
                scCompName = GetCompNam(intCo#);
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

             Write rcdComPnl;
             Write RcdRcvryR;
             Exfmt RcdRcvrRpt;
             Read rcdComPnl;
             Read RcdRcvryR;
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
       LoadInVal();

       // Load Action panel with the Company Name.
       intCo# = UserCo#(q1user);
       scFnd  = UserGrp(q1user);
       scCompName = GetCompNam(intCo#);

       // Retrieve Output Queue of the User.
       Chain(n) q1user pup00;
       If %Found(Wmupl);
          strOutq = Upoutq;
       EndIf;

       tabClick = *Off;
       Return;
      /End-Free
     p ProgramStart    e
       //===================================================================
       // RcvRptproc - Recovery Report Submit
       //===================================================================
     p RcvRptproc      b
     d RcvRptproc      pi

      /Free

       outCo#    = %EditC(intCo#:'X');

       If scFnd <> 0;
          outFnd  = %EditC(scFnd:'X');
       EndIf;

       inSTAT    = *Blanks;
       inFEAD    = *Blanks;
       inATTY    = *Zeros;
       inMOD     = *Zeros;
       //*** Beg Chg *** 10/07/24 *************************************************
       //scFCnt    = %trim(Upper(scFCnt));
       //scTCnt    = %trim(Upper(scTCnt));
       strFCnt   = *Blanks;
       strTCnt   = *Blanks;
       //*** End Chg *** 10/07/24 *************************************************
       outData   = Data;
       outOutq   = strOutq;
       outMode   = 'D';

       RcvRptSbm(outCo#:outFnd:outData:outOutq:outMode);

      /End-Free

     p RcvRptproc      e
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
       // ValidPnl - Validate Panel Screen Fields
       //===================================================================
     p ValidPnl        b
     d ValidPnl        pi              n

      /Free

       // Validation Check for Group
       If scfnd = *Zeros;
          erFnd      = *On;
          msFnd      = 'WCR0503';
          isValid2   = *On;
       EndIf;

       // Validation Check for Sort.
       If scSort   = *Blanks;
          erSort   = *On;
          msSort   = 'WCR0503';
          isValid2 = *On;
       EndIf;



       // Validation Check for Not allowing more than 1 range
       //*** Beg Del *** 10/07/24 *************************************************
       // Employer Number and County code
       //If (scEmpNbrF <> 0 or scEmpNbrT <> 0) and
       //   (scFCnt <> ' ' or scTCnt <> ' ');
       //   erEmpNbrF = *On;
       //   msEmpNbrF = 'WCR0590';
       //   erEmpNbrT = *On;
       //   msEmpNbrT = 'WCR0590';
       //   erFCnt = *On;
       //   msFCnt = 'WCR0590';
       //   erTCnt = *On;
       //   msTCnt = 'WCR0590';
       //   isValid  = *On;
       //EndIf;
       //*** End Del *** 10/07/24 *************************************************

       // Employer Number and Chapter Number
       If (scEmpNbrF <> 0 or scEmpNbrT <> 0) and
          (scFChp <> 0 or scTChp <> 0);
          erEmpNbrF = *On;
          msEmpNbrF = 'WCR0590';
          erEmpNbrT = *On;
          msEmpNbrT = 'WCR0590';
          erFChp = *On;
          msFChp = 'WCR0590';
          erTChp = *On;
          msTChp = 'WCR0590';
          isValid  = *On;
       EndIf;

       // Employer Number and Agent Number
       If (scEmpNbrF <> 0 or scEmpNbrT <> 0) and
          (scAgtNoF <> 0 or scAgtNoT <> 0);
          erEmpNbrF = *On;
          msEmpNbrF = 'WCR0590';
          erEmpNbrT = *On;
          msEmpNbrT = 'WCR0590';
          erAgtNoF = *On;
          msAgtNoF = 'WCR0590';
          erAgtNoT = *On;
          msAgtNoT = 'WCR0590';
          isValid  = *On;
       EndIf;

       //*** Beg Del *** 10/07/24 *************************************************
       // County code and Chapter Number
       //If (scFCnt <> ' ' or scTCnt <> ' ') and
       //   (scFChp <> 0 or scTChp <> 0);
       //   erFCnt = *On;
       //   msFCnt = 'WCR0590';
       //   erTCnt = *On;
       //   msTCnt = 'WCR0590';
       //   erFChp = *On;
       //   msFChp = 'WCR0590';
       //   erTChp = *On;
       //   msTChp = 'WCR0590';
       //   isValid  = *On;
       //EndIf;

       // County code and Agent Number
       //If (scFCnt <> ' ' or scTCnt <> ' ') and
       //   (scAgtNoF <> 0 or scAgtNoT <> 0);
       //   erFCnt = *On;
       //   msFCnt = 'WCR0590';
       //   erTCnt = *On;
       //   msTCnt = 'WCR0590';
       //   erAgtNoF = *On;
       //   msAgtNoF = 'WCR0590';
       //   erAgtNoT = *On;
       //   msAgtNoT = 'WCR0590';
       //   isValid  = *On;
       //EndIf;
       //*** End Del *** 10/07/24 *************************************************

       // Chapter Number and Agent Number
       If (scFChp <> 0 or scTChp <> 0) and
          (scAgtNoF <> 0 or scAgtNoT <> 0);
          erFChp = *On;
          msFChp = 'WCR0590';
          erTChp = *On;
          msTChp = 'WCR0590';
          erAgtNoF = *On;
          msAgtNoF = 'WCR0590';
          erAgtNoT = *On;
          msAgtNoT = 'WCR0590';
          isValid  = *On;
       EndIf;



       // FROM Value is greater than TO Value
       If isValid = *Off;
          // Validation Check for Employer Number Range
          If scEmpNbrF > scEmpNbrT;
             erEmpNbrF = *On;
             msEmpNbrF = 'WCR0750';
             isValid  = *On;
          EndIf;

          //*** Beg Del *** 10/07/24 *************************************************
          // Validation Check for County Code Range
          //If scFCnt > scTCnt;
          //   erFCnt = *On;
          //   msFCnt = 'WCR0750';
          //   isValid  = *On;
          //EndIf;
          //*** End Del *** 10/07/24 *************************************************

          // Validation Check for Chapter Number Range
          If scFChp > scTChp;
             erFChp = *On;
             msFChp = 'WCR0750';
          isValid  = *On;
          EndIf;

          // Validation Check for Agent Number Range
          If scAgtNoF > scAgtNoT;
             erAgtNoF  = *On;
             msAgtNoF  = 'WCR0750';
             isValid  = *On;
          EndIf;
       EndIf;



       // Check for Valid Range Entered
       If isValid = *Off;
          Select;
             When scSort = 'E' and scEmpNbrT = *zeros;
                erEmpNbrF = *On;
                msEmpNbrF = 'WCR0807';
                erEmpNbrT = *On;
                msEmpNbrT = 'WCR0807';
                isValid  = *On;
             //*** Beg Del *** 10/07/24 *************************************************
             //When scSort = 'C' and scTCnt = *blanks;
             //   erFCnt = *On;
             //   msFCnt = 'WCR0807';
             //   erTCnt = *On;
             //   msTCnt = 'WCR0807';
             //   isValid  = *On;
             //*** End Del *** 10/07/24 *************************************************
             When scSort = 'H' and scTChp = *zeros;
                erFChp = *On;
                msFChp = 'WCR0807';
                erTChp = *On;
                msTChp = 'WCR0807';
                isValid  = *On;
             When scSort = 'A' and scAgtNoT = *zeros;
                erAgtNoF = *On;
                msAgtNoF = 'WCR0807';
                erAgtNoT = *On;
                msAgtNoT = 'WCR0807';
                isValid  = *On;
          EndSl;
       EndIf;

       If isValid = *Off and isValid2 = *Off;
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free
     p ValidPnl        e
