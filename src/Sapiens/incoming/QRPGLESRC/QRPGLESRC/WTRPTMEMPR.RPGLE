     hcopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h*****************************************************************
     h dftactgrp(*no) actgrp(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     H*****************************************************************
     H/TITLE WTRPTMEMPR - Prompt for Member Profile Report
     h*****************************************************************
     H*                                                                    *
     H*  CALLED FROM:                                                      *
     H*    Menu                                                            *
     H*  CALLS TO:                                                         *
     H*    W7730C                                                          *
     H*                                                                    *
     H*  CREATED: 12/06/16  PROGRAMMER- C:300                              *
     H*  PROJECT: WC2470 - Prompt for Member Profile Report                *
     H*                                                                    *
     H***********************************************************************
     h* Revisions:                                                         *
     h*                                                                    *
     h* 02/01/19  C:295 Support: 76705                                     *
     h*                 Date check and correction to pass correctly to call*
     h*                 -ing program (outFrDt,outToDt)                     *
     h*                                                                    *
     h* 11/29/17  C:33  Support: 51312                                     *
     h*                 Allow for access to groups with inquiry access     *
     H***********************************************************************

     fWTRPTMEMPRcf   e             workstn extfile('WTRPTMEMPR')
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
     fWmupl1    IF   E           K DISK    Rename(pup01:pup01)
     f                                     Prefix(l1)
     fSmupp     IF   E             DISK    Rename(pup01:pup0p)
     fWmfnl     IF   E           K DISK    Rename(pfn01:pfn00)
     f                                     INFDS(dbfds)
     fWdelp     if   e             disk    Rename(pel01:pel0p)
     fWmaap     if   e             disk    Rename(paa01:paa0p)
     fWmaal1    if   e           k disk    Rename(paa01:paa01)
     fWmcpl     if   e           k disk    Rename(pcp01:pcp00)

     dpsds           esds                  extname(szq1p)

      *
     d WtMemPrf        pr                  Extpgm('WTRPTMEMPR')
     d
     d WtMemPrf        pi

     D dbfds           DS
     D  dbrrn                397    400B 0

     d intCo#          s                   Like(EMco#)
     d intEmp#         s                   Like(EMemp#)
     d intDiv          s                   Like(EMdiv)
     d intFnd          s                   Like(ELfnd)
     d strOutq         s                   Like(Upoutq)
     d outsufx         s              3
     d strrptl         s             23
     d strRunDt        s               d   inz
      *
     d outRrn#         s              9
     d inRrn           s              9  0
     d intEmpRrn       s              9  0
     d intRrn          s              9  0
     d outSelect       s           1024
     d charMsg         s            100
     d isValid         s               n   Inz(*off)
     d isValid2        s               n   Inz(*off)
      *
     d outCo#          s              3
     d outFnd          s              3
     d outFyr          s              3
     d outFyr1         s              3
     d outFyr2         s              3
     d outFyr3         s              3
     d outFyr4         s              3
     d outRunDt        s              8
     d outEmp#         s              9
     d outDiv          s              5
     d outFrDt         s              8
     d outToDt         s              8
     d outExpr         s              1
     d outOutT         s              1
     d outAgt          s              5
     d outChap         s              3
     d outUndwr        s              3
     d outPrSt         s              2
     d outPSort        s              2
     d outSSort        s              2
     d outStat         s              1
     d outOutq         s             10
     d outMode         s              1
     d outSyn          s              1
     d outPrSSN        s              1

     d outKc           s              1
     d outKl           s              1
     d outErr          s              1
     d outCall         s              1
     d outCall2        s              1
     d outCall3        s              1
     d outRrn          s              9  0
     d outRrn2         s              9  0
     d outChp#         s              3  0
     d outView         s              1  0
     d outEmpNbr       s              9  0
     d outDivNbr       s              5  0
     d outEmpName      s             40
       // Loss History Policy Years DS
     d dsLHFyr         DS
     d dsLHFyr1                       3
     d dsLHFyr2                       3
     d dsLHFyr3                       3
     d dsLHFyr4                       3
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
     d/copy sPrDate

      * Agent Number Search Proc.
     d*** AgtNumSrch      pr             5  0
     d AgtNumSrch      pr

      * Chapter Search Proc.
     d ChapterSrch     pr             3  0

      * Chapter Search
     d ChpSrch         pr                  ExtPgm('W0720R')
     d outKc                          1
     d outKl                          1
     d outCall2                       1
     d outRrn                         9  0
     d outRrn2                        9  0
     d outChp#                        3  0

      * Clear Error/Message fields
     d ClrErr          pr

      * Employer Search Proc.
     d EmployerSrch    pr
     d outEmpNum                      9  0
     d outDivNbr                      5  0
     d outEmpName                    40

      * Employer Search
     d EmpSearch       pr                  ExtPgm('WTSEARCH')
     d outMode                        1
     d outCall                        1
     d outRrn#                        9
     d outSelect                           likeds(dsSrchCrit)

      * Intialize the Screen fields
     d LoadInVal       pr

      * Main Processing
     d Main            pr

      * Member Profile Reprot proc.
     d MemPrfproc      pr

      * Member Profile Reprot Submit
     d MemPrfSbm       pr                  ExtPgm('W7730C')
     d outCo#                         3
     d outFnd                         3
     d outFyr                         3
     d outFyr1                        3
     d outFyr2                        3
     d outFyr3                        3
     d outFyr4                        3
     d outRunDt                       8
     d outEmp#                        9
     d outDiv                         5
     d outFrDt                        8
     d outToDt                        8
     d outExpr                        1
     d outOutT                        1
     d outAgt                         5
     d outChap                        3
     d outUndwr                       3
     d outPrSt                        2
     d outPSort                       2
     d outSSort                       2
     d outStat                        1
     d outOutq                       10
     d outMode                        1
     d outSyn                         1
     d outPrSSN                       1

      * Program Start
     d ProgramStart    pr

      * Output Suffix.
     d SA030R          PR                  extpgm('SA030R')
     d outsufx                        3

      * Set Criteria
     d SetCriteria     pr

      * Underwriter Search
     d UndrSrch        pr                  ExtPgm('S0005R')
     d outCall3                       1
     d outRrn                         9  0
     d outView                        1  0

      * Underwriter Search Proc.
     d UndwrSrch       pr             3

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
       // AgtNumSrch  - Agent Number Search
       //===================================================================
     p AgtNumSrch      b
     d*** AgtNumSrch      pi             5  0
      /Free

       outKc   = '0';
       outKl   = '0';
       outErr  = ' ';

       inRrn = AgtSrchAA(outKc:outKl:outErr:scFnd);

       If inRrn <> 0;
          Chain(n) inRrn paa0p;
          If %Found(Wmaap);
             scAgntName = AAname;
             scAgtNo = AAagt#;
             //Return AAagt#;
          EndIf;
       EndIf;

      /End-Free
     p AgtNumSrch      e
       //===================================================================
       // ChapterSrch   - Chapter Search Proc
       //===================================================================
     p ChapterSrch     b
     d ChapterSrch     pi             3  0

      /Free
      // Retrieve RRN of WMFNL file for Chapter Search
       Chain(n) (UPco#:scfnd) pfn00;

       outKc   = ' ';
       outKl   = ' ';
       outCall2 = '2';
       outRrn   = *Zeros;
       outRrn2  = dbrrn;
       outChp#  = *Zeros;
       ChpSrch(outKc:outKl:outCall2:outRrn:outRrn2:outChp#);

       Chain(n)(intCo#:scFnd:outChp#) pcp00;
       If %Found(Wmcpl);
          scChapter = CPdesc;
       Endif;

       Return outChp#;
      /End-Free
     p ChapterSrch     e
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
       msFyr     = *blanks;
       erFyr     = *off;
       msLHFyr   = *blanks;
       erLHFyr   = *off;
       msEmpNbr  = *blanks;
       erEmpNbr  = *off;
       msDivNbr  = *blanks;
       erDivNbr  = *off;
       msPXDtF   = *blanks;
       erPXDtF   = *off;
       msPXDtT   = *blanks;
       erPXDtT   = *off;
       msRunDt   = *blanks;
       erRunDt   = *off;
       msAgtNo   = *blanks;
       erAgtNo   = *off;
       msChap    = *blanks;
       erChap    = *off;
       msUndwr   = *blanks;
       erUndwr   = *off;
       msPrSt    = *blanks;
       erPrSt    = *off;
       msPSort   = *blanks;
       erPSort   = *off;
       msSSort   = *blanks;
       erSSort   = *off;
       msSyn     = *blanks;
       erSyn     = *off;
       msOutput  = *blanks;
       erOutput  = *off;
       msPSort1  = *blanks;
       erPSort1  = *off;
       msSSort1  = *blanks;
       erSSort1  = *off;


      /End-Free
     p ClrErr          e
       //===================================================================
       // EmployerSrch  - Employer/Div Search
       //===================================================================
     p EmployerSrch    b
     d EmployerSrch    pi
     d  outEmpNbr                     9  0
     d  outDivNbr                     5  0
     d  outEmpName                   40

      /Free
       outEmpNbr = 0;
       outDivNbr = 0;

       outMode = 'E';
       outCall = '2';
       clear dsSrchCrit;
       clear outSelect;
       //****Beg Chg *********/
       //dsEmpGrp  = UserGrp(q1user);
       dsEmpGrp  = scFnd;
       //****End Chg *********/
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
                outEmpName = EMname;
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
       Clear RcdMemPrf;
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
             When selEmpSrch = *On;
                selEmpSrch = *Off;
                EmployerSrch(outEmpNbr:outDivNbr:outEmpName);
                scEmpNbr = outEmpNbr;
                scDivNbr = outDivNbr;
                scEmpName = outEmpName;

             When selAgtNo = *On;
                selAgtNo = *Off;
              //scAgtNo = AgtNumSrch();
               AgtNumSrch();

             When selChapNo = *On;
                selChapNo = *Off;
                scChap = ChapterSrch();

             When selUndwr = *On;
                selUndwr = *Off;
                scUndwr = UndwrSrch();

             When BtnSubmit = *on;
                BtnSubmit = *off;
                // Clear Errors
                ClrErr();
                // Validate Edits
                If ValidPnl() = *On;
                   MemPrfproc();
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
                scRunDt = DateTo8(strRunDt);
                scOutput = 'B';
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
             Write RcdMemPR;
             Exfmt RcdMemPrf;
             Read rcdComPnl;
             Read RcdMemPR;
       EndSl;
       Endsr;
      /End-Free
     p Main            e
       //===================================================================
       // MemPrfproc - Member Profile Reprot Submit
       //===================================================================
     p MemPrfproc      b
     d MemPrfproc      pi

      /Free

       outCo#    = %EditC(intCo#:'X');
       outFnd    = %EditC(scFnd:'X');
       outFyr    = %EditC(scFyr:'X');
       evalr outFyr1 = '000' + %trim(dsLHFyr1);
       evalr outFyr2 = '000' + %trim(dsLHFyr2);
       evalr outFyr3 = '000' + %trim(dsLHFyr3);
       evalr outFyr4 = '000' + %trim(dsLHFyr4);
       outRunDt  = %EditC(scRunDt:'X');
       outEmp#   = %EditC(scEmpNbr:'X');
       outDiv    = %EditC(scDivNbr:'X');
       //***  Begin Chg  ***  02/01/19  ***********************************
       //*** outFrDt   = %EditC(scPXDtF:'X');
       //*** outToDt   = %EditC(scPXDtT:'X');
       outFrDt   = %EditC(DateTo8(cvtToDate(scPXDtF)):'X');
       outToDt   = %EditC(DateTo8(cvtToDate(scPXDtT)):'X');
       //***  End   Chg  ***  02/01/19  ***********************************
       outExpr   = scExpR;
       If outExpr = *Blanks;
          outExpr = 'N';
       EndIf;
       outOutT   = scOutput;
       outAgt    = %EditC(scAgtNo:'X');
       outChap   = %EditC(scChap:'X');
       outUndwr  = %trim(Upper(scUndwr));
       outPrSt   = scPrSt;

       Select;
          When scPSort = '1';
             outPSort  = 'EX';
          When scPSort = '2';
             outPSort  = 'AN';
          When scPSort = '3';
             outPSort  = 'EC';
          When scPSort = '4';
             outPSort  = 'UW';
          When scPSort = '5';
             outPSort  = 'ST';
          When scPSort = ' ';
             outPSort  = 'EN';
       EndSl;

       Select;
          When scSSort = '1';
             outSSort  = 'EX';
          When scSSort = '2';
             outSSort  = 'AN';
          When scSSort = '3';
             outSSort  = 'EC';
          When scSSort = '4';
             outSSort  = 'UW';
          When scSSort = '5';
             outSSort  = 'ST';
          When scSSort = ' ';
             outSSort  = '  ';
       EndSl;

       If scFyr = 0;
          If scPXDtF <> 0 and scPXDtT <> 0;
             outStat   = 'X';
          Else;
             outStat   = 'B';
             outFrDt   = '01011950';
             outToDt   = '12312099';
          EndIf;
       Else;
          outStat   = 'E';
          outFrDt   = %EditC(dtMDYY(%int(%char(scFyr)+'0101')):'X');
          outToDt   = %EditC(dtMDYY(%int(%char(scFyr)+'1231')):'X');
       EndIf;

       outOutq   = strOutq;
       outMode   = 'D';
       outSyn    = scSyn;
       If outSyn = *Blanks;
          outSyn = 'N';
       EndIf;
       outPrSSN  = scPrSSN;

       MemPrfSbm(outCo#:outFnd:outFyr:outFyr1:outFyr2:outFyr3:
        outFyr4:outRunDt:outEmp#:outDiv:outFrDt:outToDt:outExpr:
        outOutT:outAgt:outChap:outUndwr:outPrSt:outPSort:outSSort:
        outStat:outOutq:outMode:outSyn:outPrSSN);

      /End-Free

     p MemPrfproc      e
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
          strOutq = UPoutq;

       EndIf;

       // Retrieve Output Suffix.
       SA030r(outSufx);
       If outSufx = *blanks;
         strrptl = 'PDSRPT' + ', if selected.';
       Else;
         strrptl = 'PDSRPT' + outSufx + ', if selected.';
       EndIf;

       scrpt = 'This Report will produce output file WFMRP in library '+
        strrptl;

       // Load system date into Run Date.
       strRunDt = %Date();
       scRunDt = DateTo8(strRunDt);
       scOutput = 'B';

       tabClick = *Off;
       Return;
      /End-Free
     p ProgramStart    e
       //===================================================================
       // SetCriteria
       //===================================================================
     p SetCriteria     b
     d SetCriteria     pi
      /Free
       //***  Begin Chg  ***  11/29/17  ***********************************
       //GetGrpAuthSql(q1User:grpSelect:grpFrom:grpWhere:'A');
       GetGrpAuthSql(q1User:grpSelect:grpFrom:grpWhere:'I');
       //***  End   Chg  ***  11/29/17  ***********************************
      /End-Free
     p SetCriteria     e
       //===================================================================
       // UndwrSrch  - Underwriter Search
       //===================================================================
     p UndwrSrch       b
     d UndwrSrch       pi             3
      /Free

       outCall3 = '2';
       outRrn   = *zeros;
       outView  = 3;

       UndrSrch(outCall3:outRrn:OutView);

       If outRrn <> 0;
          Chain(n) outRrn pup0p;
          If %Found(Smupp);
             scUndWrter = UPusNm;
             Return UPstaf;
          EndIf;
       EndIf;

       Return *blanks;

      /End-Free
     p UndwrSrch       e
       //===================================================================
       // ValidPnl - Validate Panel Screen Fields
       //===================================================================
     p ValidPnl        b
     d ValidPnl        pi              n

      /Free

       // Validation Check for Group
       If scFnd = *Zeros;
          erFnd      = *On;
          msFnd      = 'WCR0503';
          isValid    = *On;
       EndIf;

       // None of the (simgle/multiple) employers selected.
       // Can't be Blank
       If (scFyr = *Zeros and scEmpNbr = *Zeros and scDivNbr = *Zeros)
        and (scPXDtF = *Zeros and scPXDtT = *Zeros);
          erFyr      = *On;
          msFyr      = 'WCR1391';
          erEmpNbr   = *On;
          msEmpNbr   = 'WCR1391';
          erDivNbr   = *On;
          msDivNbr   = 'WCR1391';
          erPXDtF    = *On;
          msPXDtF    = 'WCR1391';
          erPXDtT    = *On;
          msPXDtT    = 'WCR1391';
          isValid2   = *On;
       EndIf;

       // Both (simgle/multiple) employers can't be selected together.
       // Mutually Exclusive
       If (scFyr <> *Zeros or scEmpNbr <> *Zeros or scDivNbr <> *Zeros)
        and (scPXDtF <> *Zeros or scPXDtT <> *Zeros);
          erFyr      = *On;
          msFyr      = 'WCR1393';
          erEmpNbr   = *On;
          msEmpNbr   = 'WCR1393';
          erDivNbr   = *On;
          msDivNbr   = 'WCR1393';
          erPXDtF    = *On;
          msPXDtF    = 'WCR1393';
          erPXDtT    = *On;
          msPXDtT    = 'WCR1393';
          isValid2   = *On;
       EndIf;

       If isValid2 = *Off;
          // Validation Check for policy Year
          If scFyr = *Zeros
           and (scPXDtF = *Zeros and scPXDtT = *Zeros);
             erFyr      = *On;
             msFyr      = 'WCR0503';
             isValid2   = *On;
          EndIf;

          // Validation Check for Employer Number
          If scEmpNbr  <> *Zeros;
             Chain(n)(UPco#:scfnd:scEmpNbr:scDivNbr) pem00;
             If %Found(Wmeml);
             Else;
                erEmpNbr   = *On;
                msEmpNbr   = 'WCR0538';
                erDivNbr   = *On;
                msDivNbr   = 'WCR0538';
                isValid2   = *On;
             EndIf;
          EndIf;

          // Validation Check for Policy expiration date, To
          If scPXDtT <> 0;
             If DtCheck(scPXDtT) = 0;
                erPXDtT  = *on;
                msPXDtT  = 'WCR0500';
                isValid2 = *On;
             Endif;
          Endif;

          // Validation Check for Policy expiration date, From
          If scPXDtF <> 0;
             If DtCheck(scPXDtF) =0;
                erPXDtF  = *on;
                msPXDtF  = 'WCR0500';
                isValid2 = *On;
             Endif;
          Endif;

          If erPXDtT = *off and erPXDtF = *off;
             erPXDtT = AscendDates(scPXDtF:scPXDtT:charMsg);
             If erPXDtT = *on;
                erPXDtF = *on;
                msPXDtT = charMsg;
                msPXDtF = charMsg;
                isValid2 = *On;
             Endif;
          Endif;

       Endif;

       // Validation Check for Loss History Policy Years
       If scLHFyr <> *Blanks;
          If %len(%trim(scLHFyr)) >= 16;
             erLHFyr = *on;
             msLHFyr = 'WCR4825';
             isValid  = *On;
          Else;
             dsLHFyr1 = ParseString(scLHFyr:',':1);
             dsLHFyr2 = ParseString(scLHFyr:',':2);
             dsLHFyr3 = ParseString(scLHFyr:',':3);
             dsLHFyr4 = ParseString(scLHFyr:',':4);
             If ParseString(scLHFyr:',':5) <> *Blanks;
                erLHFyr  = *on;
                msLHFyr  = 'WCR4825';
                isValid  = *On;
             EndIf;
          EndIf;
       EndIf;

       // Validation Check for Run Date
       If scRunDt = *Zeros;
          erRunDt    = *On;
          msRunDt    = 'WCR0503';
          isValid    = *On;
       EndIf;
       If erRunDt = *Off;
          If DtCheck(scRunDt) =0;
             erRunDt  = *on;
             msRunDt  = 'WCR0500';
             isValid  = *On;
          Endif;
       Endif;

       // Validation Check for Agent Number
       If scAgtNo  <> *Zeros;
          Chain(n)(UPco#:scfnd:scAgtNo) paa01;
          If %Found(Wmaal1);
          Else;
             erAgtNo    = *On;
             msAgtNo    = 'WCR3161';
             isValid    = *On;
          EndIf;
       EndIf;

       // Validation Check for Chapter
       If scChap   <> *Zeros;
          Chain(n)(UPco#:scfnd:scChap) pcp00;
          If %Found(Wmcpl);
          Else;
             erChap     = *On;
             msChap     = 'WCR3160';
             isValid    = *On;
          EndIf;
       EndIf;

       // Validation Check for Underwriter
       If scUndwr  <> *Blanks;
          Chain(n) scUndwr pup01;
          If %Found(Wmupl1);
          Else;
             erUndwr    = *On;
             msUndwr    = 'WCR3162';
             isValid    = *On;
          EndIf;
       EndIf;

       // Validation Check for Primary Sort
       If scPSort  <> *Blanks;
          If scEmpNbr <> *Zeros or scSyn = 'Y';
             erPSort    = *On;
             msPSort    = 'WCR3174';
             isValid    = *On;
          EndIf;
       EndIf;

       // Validation Check for Secondary Sort
       If scSSort  <> *Blanks;
          If scEmpNbr <> *Zeros or scSyn = 'Y';
             erSSort    = *On;
             msSSort    = 'WCR3174';
             isValid    = *On;
          EndIf;
       EndIf;

       // Validation Check for Summarize by Employer/Division
       If scSyn  = 'Y';
          If scPSort <> *Blanks and scSSort <> *Blanks;
             erSyn      = *On;
             msSyn      = 'WCR3176';
             isValid    = *On;
          EndIf;
       EndIf;

       // Primary sort and Secondary Sort should not be same
       If (scPSort <> *Blanks and scSSort <> *Blanks and scPSort = scSSort);
          msPSort1 = 'Primary Sort and Secondary Sort should be different.';
          msSSort1 = 'Primary Sort and Secondary Sort should be different.';
          erPSort1 = *On;
          erSSort1 = *On;
          isValid  = *On;
       EndIf;


       // Validation Check for Output Type
       If scOutput = *Blanks;
          erOutput   = *On;
          msOutput   = 'WCR0503';
          isValid    = *On;
       EndIf;


       If isValid = *Off and isValid2 = *Off;
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free
     p ValidPnl        e
