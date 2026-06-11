     hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h*****************************************************************
     h/title WC2460 - Prompt for Agent Premium Loss By State
     h dftactgrp(*no) actgrp(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     H*****************************************************************
     H/TITLE WTRPTNRWB - Prompt for Agent Premium Loss By State
     h*****************************************************************
     h* Revisions                                                     *
     h*****************************************************************
     fWTRPTAGPRMcf   e             workstn extfile('WTRPTAGPRM')
     f                                     handler('PROFOUNDUI(HANDLER)')
     fWtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdPolPnl)
     f                                     include(rcdQtePnl)
     f                                     include(rcdComPnl)
     f*
     f* User Parameter Master File
     fWmfnl     IF   E           K DISK    Rename(PFN01:PFN00)
     fWmeml     IF   E           k DISK    Rename(pem01:pem00)
     fsmcol     IF   E           k DISK    Rename(PCO01:PCO00)
     fqfeul     IF   E           K DISK    Rename(PEU01:PEU00)
     fqfe2l     IF   E           K DISK    Rename(PE201:PE200)
     fWmupl     IF   E           K DISK    Rename(PUP01:PUP00)

     dpsds           esds                  extname(szq1p)
      *
     d WtAgtPrmR       pr                  Extpgm('WTRPTAGPRM')
     d  inPrds                       40A   const
     d  inCall                        1A
     d
     d WtAgtPrmR       pi
     d  inPrds                       40A   const
     d  inCall                        1A

     d inOutq          s             10
     d outOutq         s             10
     d inMode          s              1
     d outMode         s              1

     d charMsg         s            100
     d isValid         s               n   Inz(*off)
     d intCo#          s                   Like(EMco#)
     d intFnd          s                   Like(EMfnd)
     d intEmp#         s                   Like(EMemp#)
     d intDiv          s                   Like(EMdiv)
     d strOutq         s                   Like(Upoutq)
      *
     d inCo#           s              3
     d outCo#          s              3
     d inFnd           s              3
     d outFnd          s              3
     d inFyr           s              3
     d outFyr          s              3
     d inEmp#          s              9
     d outEmp#         s              9
     d inDiv           s              5
     d outDiv          s              5
     d inDte           s              8
     d outDte          s              8
     d inFrDt          s              8
     d outFrDt         s              8
     d inDate1         s               d   datfmt(*iso)
     d inToDt          s              8
     d outToDt         s              8
     d inToDt1         s              8
     d inToDt2         s             10
     d inDtYY          s              4
     d inDtYY1         s              4
     d inDtDD          s              2
     d inDtDD1         s              2
     d inDtMM          s              2
     d inDtMM1         s              2
     d inAgt           s              5
     d outAgt          s              5
     d inSort          s              2
     d outSort         s              2
     d outSuffix       s              3
     d strrptlib       s             23
     d inCYYMM         s              5  0
     d inFld           s              4  0
     d indatechk       s              6
     d inasof          s              6
     d outasof         s              6
     d intypo          s              1
     d outtypo         s              1

     D dsAsOf          DS
     D  Scmm                   1      2  0
     D  Sccc                   3      4  0
     D  Scyy                   5      6  0
     D  Scccyy                 3      6  0

     D dsAsof2         DS                  inz
     D  Scc2                          1  0
     D  Scyy2                         2  0
     D  Scmm2                         2  0

      *****************************************************************
      * Prototype Declaration
      *****************************************************************

     d/copy sPrString
     d/copy sPrDate
     d/copy sPrCompany
     d/copy wPrAgent
     d/copy wPrUsrPrms
     d/copy wPrPolErr
     d/copy WTSearchDs

     d GetSuffix       PR                  extpgm('SA030R')
     d outSuffix                      3

      * Get client ID
      * Intialize the Screen fields
     d LoadInVal       pr

      * Main Processing
     d Main            pr

      * Program Start
     d ProgramStart    pr

      * Set Criteria
     d SetCriteria     pr

      * Clear Error/Message fields
     d ClrErr          pr

      * Validate Panel Sreen Values
     d ValidPnl        pr

      * Agent Premium Loss By State Report Proc.
     d AgtPrmProc      pr

      * Agent Premium Loss By State Report Submit
     d AgtPrmSbm       pr                  ExtPgm('W9760C')
     d outCo#                         3
     d outFnd                         3
     d outFrDt                        8
     d outToDt                        8
     d outasof                        6
     d outsort                        1
     d outtypo                        1
     d outOutQ                       10
     d outMode                        1

      /Free
       // Start Of Program
       ProgramStart();

       // Main Procesing
       Main();

       *InLr = *on;

      /End-Free
       //===================================================================
       // AgtPrmProc - Agent Premium Loss By State Report  Submit
       //===================================================================
     p AgtPrmProc      b
     d AgtPrmProc      pi

      /Free

       inCo# = %Editc(intCo#:'X');
       If scFND <> '0';
          outFnd = scFND;
       Else;
          outFnd = *Blanks;
       EndIf;

       If scASOF <> 0;
          outfrdt = '0101' + %Editc(Scccyy:'X');
       Else;
          outFrDt = *Zeros;
       EndIf;

       If scASOF <> 0;
          indatechk = %Editc(scASOF:'X');
          inDtYY =   %Subst(indatechk:3:4);
          inDtDD =   '01';
          inDtMM =   %Subst(indatechk:1:2);
          inToDt1 =   inDtYY + inDtMM + inDtDD;
          inDate1 = %Date(intodt1:*ISO0);
          intodt2 = %Char(getLastDay(inDate1));
          inDtYY1 =   %Subst(intodt2:1:4);
          inDtMM1 =   %Subst(intodt2:6:2);
          inDtDD1 =   %Subst(intodt2:9:2);
          outToDt  =   inDtMM1 + inDtDD1 + inDtYY1;
       Else;
          outToDt = *Zeros;
       EndIf;

       outasof    = %Editc(scASOF:'X');
       outSort    = scPSRT;
       outtypo    = scPFB;
       outOutQ    = strOutQ;
       outMode    = 'D';

       AgtPrmSbm(outCo#:outFnd:outFrDt:outToDt:outAsOf:outSort:outTypo:
           outOutQ:outMode);
      /End-Free

     p AgtPrmproc      e
       //===================================================================
       // LoadInVal - Intialize the Screen fields
       //===================================================================
     p LoadInVal       b
     d LoadInVal       pi

      /Free

       Clear RcdAgPrRpt;

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
          When BtnSubmit = *on;
             // Clear Errors
             ClrErr();
             // Validate Edits
             ValidPnl();
             If isValid = *Off;
                AgtPrmproc();
                Leave;
             EndIf;

          When BtnCancel = *on;
             ExitNow = *On ;

          // do nothing - catch tab click event
          When tabClick = *on;
             tabClick = *off;
          When BtnRefresh = *On;
             BtnRefresh =*Off;
             // Intialize the Screen fields
             LoadInVal();
             SetCriteria();
             ClrErr();
             // Load Action panel with the Company Name.
             intCo#   = userCo#(q1user);
             scCOMPNAME = GetCompNam(intCo#);
             scFND = %editc(userGrp(q1user):'X');
          EndSl;

       Enddo;
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       Select;

       When TabIndex = 0;
          TabHeight = 750;
          APnlHeight = TabHeight + 60;

          Write rcdComPnl;
          Write RcdTabAgPr;

          Exfmt RcdAgPrRpt;

          Read rcdComPnl;
          Read RcdTabAgPr;

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
       intCo#     = userCo#(q1user);
       scCOMPNAME = GetCompNam(intCo#);
       scFND = %Editc(userGrp(q1user):'X');

       // Retrieve Output Queue of the User.
       Chain q1user pup00;
       If %Found(Wmupl);
          strOutq = Upoutq;
       EndIf;


       // Retrieve Output Suffix.
       GetSuffix(outSuffix);
       strrptlib = 'PDSRPT' + outSuffix;

       scRPTLIB = 'This Report will produce output file WFXLP in library '+
        strrptlib + '.';

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

       GetGrpAuthSql(q1User:grpSelect:grpFrom:grpWhere:'A');

      /End-Free

     p SetCriteria     e
       //===================================================================
       // ClrErr - Clear Error/Message fields
       //===================================================================
     p ClrErr          b
     d ClrErr          pi

      /Free
       msAsOf    = *Blanks;
       msprisort = *Blanks;
       mstypop   = *Blanks;
       erAsOf    = *off;
       erPriSort = *off;
       erTyPop   = *off;
       isValid   = *off;
      /End-Free

     p ClrErr          e
       //===================================================================
       // ValidPnl - Validate Panel Screen Fields
       //===================================================================
     p ValidPnl        b
     d ValidPnl        pi

      /Free

       // Validation Check for Date entered.
       dsAsOf = %Editc(scASOF:'X');
       If Scmm < 01 or Scmm > 12 or Scccyy  < 1960 or Scccyy  > 2060;
          erAsOf = *on;
          msAsOf = 'WCR0500';
          isValid    = *On;
       EndIf;

       If Sccc > 19;
          Scc2 = 1;
       Else;
          Scc2 = 0;
       EndIf;
       Scmm2 = Scmm;
       Scyy2 = Scyy;
       inCYYMM = %Dec(dsAsOf2:5:0);
       Chain (intco#:inCYYMM) PEU00;
       If Not %Found(Qfeul);
          erAsOf = *on;
          msAsOf = 'WCR3181';
          isValid = *on;
       EndIf;

       Chain (intco#:inCYYMM) PE200;
       If not %found(Qfe2l);
          erAsOf = *on;
          msAsOf = 'WCR3181';
          isValid = *on;
       EndIf;

       // Validation Check for Sort Options.
         If scPSRT     = *Blanks;
            erPriSort  = *On;
            MSPriSort  = 'WCR0503';
            isValid    = *On;
         EndIf;


       // Validation Check for type of Output.
       If scPFB   = *Blanks;
          erTypOp = *On;
          msTypOp = 'WCR0503';
          isValid = *On;
       EndIf;

      /End-Free
     p ValidPnl        e
