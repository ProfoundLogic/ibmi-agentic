     hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h*****************************************************************
     h/title WC2443 - Prompt for New/Renewal Business Report
     H dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     H*****************************************************************
     H/TITLE WTRPTNRWB - Prompt for New/Renewal Business Report
     h*****************************************************************
     h* Revisions                                                     *
     h*                                                               *
     h*                                                               *

     fWTRPTNRWB cf   e             workstn extfile('WTRPTNRWB')
     f                                     handler('PROFOUNDUI(HANDLER)')
     fWtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdPolPnl)
     f                                     include(rcdQtePnl)
     f                                     include(rcdComPnl)
     f*
     f* User Parameter Master File
     fWmfnl     IF   E           K DISK    Rename(PFN01:PFN00)
     fWmeml     IF   E           k DISK    Rename(PEM01:PEM00)
     fWmupl     IF   E           K DISK    Rename(PUP01:PUP00)

     dpsds           esds                  extname(szq1p)

      *
     d WtNwRwBuR       pr                  Extpgm('WTRPTNRWB')
     d  inPrds                       40A   const
     d  inCall                        1A
     d
     d WtNwRwBuR       pi
     d  inPrds                       40A   const
     d  inCall                        1A

     d outOutq         s             10
     d outmode         s              1

     d charMsg         s            100
     d*isValid         s               n   Inz(*off)
     d intCo#          s                   Like(EMCO#)
     d intFnd          s                   Like(EMFND)
     d intEmp#         s                   Like(EMEMP#)
     d intDiv          s                   Like(EMDIV)
     d strOutq         s                   Like(UPOUTQ)
      *
     d outCo#          s              3
     d outFndFr        s              3
     d outFndTo        s              3
     d outFyr          s              3
     d outEmp#         s              9
     d outDiv          s              5
     d outDte          s              8
     d outFrDt         s              8
     d outToDt         s              8
     d outAgt          s              5
     d outPrst         s              2
     d outExpr         s              1
     d outPsrt         s              2A
     d outSsrt         s              2A
     d outStat         s              1A
     d outDtSl         s              1A
     d outNRB          s              1A
     d outAPB          s              1A
     d outexpC         s              1A
     d outPsA          s              1A
     d outSsA          s              1A
     d outPFB          s              1A
     d outRLB          s              1A
     d outGrpSlct      s              1A
     d strrptl         s             23
     d outsufx         s              3
      *****************************************************************
      * Prototype Declaration
      *****************************************************************

     d/copy sPrString
     d/copy sPrCompany
     d/copy wPrAgent
     d/copy wPrUsrPrms
     d/copy wPrPolErr
     d/copy WTSearchDs

     d GetSuffix       PR                  extpgm('SA030R')
     d outsufx                        3

      * Get client ID
      * Intialize the Screen fields
     d LoadInVal       pr

      * Main Processing
     d Main            pr

      * Program Start
     d ProgramStart    pr

      * Validate Panel Sreen Values
     d ValidPnl        pr              n

      * New/Renewal Business Report Proc.
     d NwRwBuproc      pr

      * New/Renewal Business Report Submit
     d NwRwBuSbm       pr                  ExtPgm('W7940C')
     d outCo#                         3
     d outFndFr                       3
     d outFndTo                       3
     d outFyr                         3
     d outDte                         8
     d outEmp#                        9
     d outDiv                         5
     d outFrDt                        8
     d outToDt                        8
     d outExpr                        1
     d outAgt                         5
     d outPrst                        2
     d outPsrt                        2
     d outSsrt                        2
     d outStat                        1
     d outOutq                       10
     d outMode                        1
     d outNRB                         1
     d outAPB                         1
     d outDtSl                        1
     d outexpC                        1
     d outPsA                         1
     d outPFB                         1
     d outRLB                         1
     d outSsA                         1
     d outGrpSlct                     1

      /Free
       // Start Of Program
       ProgramStart();

       // Main Procesing
       Main();

       *InLr = *on;

      /End-Free

       //===================================================================
       // NwRwBuproc - New/Renewal Business Report Process Submit
       //===================================================================
     p NwRwBuproc      b
     d NwRwBuproc      pi

      /Free

       outCo# = %EditC(intCo#:'X');

       If scFRMDT <> 0;
          outFrDt  = %EditC(scFRMDT:'X');
       Else;
          outFrDt  = *Blanks;
       EndIf;

       If scTODT  <> 0;
          outToDt  = %EditC(scTODT:'X');
       Else;
          outToDt  = *Blanks;
       EndIf;

       If scISSEFFDT = 'I';
          outDtSl  = 'I';
       Endif;

       If scISSEFFDT = 'E';
          outDtSl  = 'E';
       EndIf;

       outFndFr   = ' ';
       outFndTo   = ' ';
       outNRB     = scNEWRWB;
       outAPB     = scACTPNDBO;
       outRLB     = scRENQUOB;
       outPsrt    = scPRISORT;
       outSsrt    = scSECSORT;
       outStat    = scISSEFFDT;
       outPsA     = scPRIAGENT;
       outSsA     = scSECAGENT;
       outPFB     = scPRNTFILB;
       outexpC    = scINCEXPCO;
       outEmp#    = *blanks;
       outDiv     = *blanks;
       outFyr     = '000';
       outDte     = *Zeros;
       outAgt     = *Zeros;
       outPrst    = *Zeros;
       outExpr    = *Zeros;
       outOutq     = strOutq;
       outMode     = 'D';
       outGrpSlct  = 'Y';

       NwRwBuSbm(outCo#:outFndFr:outFndTo:outFyr:outDte:outEmp#:outDiv:outFrDt:
        outToDt:outExpr:outAgt:outPrst:outPsrt:outSsrt:outStat:outOutq:outMode:
        outNRB:outAPB:outDtsl:outexpC:outPsA:outPFB:outRLB:outSsA:outGrpSlct);

      /End-Free
     p NwRwBuproc      e

       //===================================================================
       // LoadInVal - Intialize the Screen fields
       //===================================================================
     p LoadInVal       b
     d LoadInVal       pi

      /Free

       Clear RcdNRBRpt;
       scISSEFFDT ='I';
       scINCEXPCO = 'N';

       // Retrieve Output Suffix.
       GetSuffix(outSufx);
       strrptl = 'PDSRPT' + outSufx;
       scRptLib = 'This report will produce output file WFNBP in library '+
                  %trim(strrptl) + '.';

      /End-Free
     p  LoadInVal      e

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

      /Free

       // Process until cancel button is pressed.
       Dou BtnCancel = *On ;

          // Display Screen
          Exsr ExfmtScreens;

          Select;
          When BtnSubmit = *on;
             // Validate Edits
             If ValidPnl() = *On;
                NwRwBuproc();
                Leave;
             EndIf;

          When BtnCancel = *on;
             Leave;

          // do nothing - catch tab click event
          When tabClick = *on;
             tabClick = *off;

          When BtnRefresh = *On;
             BtnRefresh =*Off;
             Programstart();

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
          Write RcdTabNRBR;

          Exfmt RcdNRBRpt;

          Read rcdComPnl;
          Read RcdTabNRBR;

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
       scCompName = GetCompNam(intCo#);

       // Retrieve Output Queue of the User.
       Chain(n) q1user PUP00;
       If %Found(Wmupl);
          strOutq = UPOUTQ;
       EndIf;

       tabClick = *Off;

       Return;

      /End-Free

     p ProgramStart    e

       //===================================================================
       // ValidPnl - Validate Panel Screen Fields
       //===================================================================
     p ValidPnl        b
     d ValidPnl        pi              n
     d isError         s               n   inz(*off)
      /Free

       // initialize the error indicator
       msNewRwB   = *blanks;
       erNewRwB   = *off;
       msACTPNDBO = *blanks;
       erACTPNDBO = *off;
       msRENQUOB  = *blanks;
       erRENQUOB  = *off;
       msFrmDt    = *blanks;
       erfrmDt    = *off;
       msToDt     = *blanks;
       erToDt     = *off;
       msPriSort  = *blanks;
       erPriSort  = *off;
       msPRIAGENT = *blanks;
       erPRIAGENT = *off;
       msSecSort  = *blanks;
       erSecSort  = *off;
       msSECAGENT = *blanks;
       erSECAGENT = *off;
       msPRNTFILB = *blanks;
       erPRNTFILB = *off;
       msINCEXPCO = *blanks;
       erINCEXPCO = *off;
       msIssEffDt = *blanks;
       erIssEffDt = *off;
       isError  = *off;

       // Validation Check for from Date
       erfrmDt = ValidDate(scFRMDT:charMsg);
       If erfrmDt  = *on;
          msFrmDt  = charMsg;
          isError  = *On;
       Endif;

       // Validation Check for To Date
       erToDt = ValidDate(scTODT:charMsg);
       If erToDt  = *on;
          msToDt  = charMsg;
          isError  = *On;
       Endif;

       erToDt = AscendDates(scFRMDT:scTODT:charMsg);
       If erToDt  = *on;
          erfrmDt = *on;
          msToDt  = charMsg;
          msFrmDT = charMsg;
          isError  = *On;
       Endif;

       // Validation Check for New Business/Renewal/Both.
       If scNEWRWB = *Blanks;
          erNewRwB = *On;
          msNewRwB = 'WCR0503';
          isError  = *On;
       EndIf;

       // Validation Check for Account status Active/Pending/Both.
       If scACTPNDBO = *Blanks;
          erACTPNDBO = *On;
          msACTPNDBO = 'WCR0503';
          isError  = *On;
       EndIf;

       // Validation Check for Renewal Quote/Live Policy/Both.
       If scRENQUOB = *Blanks;
          erRENQUOB  = *On;
          msRENQUOB  = 'WCR0503';
          isError  = *On;
       EndIf;

       // Validation Check for Date based on Issue date/Eff date.
       If scISSEFFDT = 'I' and scACTPNDBO <>'A';
          erIssEffDt = *On;
          msIssEffDt = 'WCR3172';
          isError  = *On;
       Endif;

       // Validation Check for Primary Sort.
       If scPRISORT  = *Blanks;
          erPriSort  = *On;
          MSPriSort  = 'WCR0503';
          isError  = *On;
       EndIf;

       // Validation Check for Primary Sort
       If scPRISORT ='AA' or scPRISORT ='AN';
          If scPRIAGENT= *Blanks;
             erPRIAGENT = *On;
             msPRIAGENT = 'WCR3169';
             isError  = *On;
          EndIf;
       EndIf;

       // Validation Check for Secondary sort.
       If scSECSORT ='AA' or scSECSORT = 'AN';
          If scSECAGENT <> 'P' and scSECAGENT <> 'S';
             erSECAGENT = *On;
             msSECAGENT = 'WCR3169';
             isError  = *On;
          EndIf;
       EndIf;

       // Validation Check for type of Output.
       If scPRNTFILB = *Blanks;
          erPRNTFILB = *On;
          msPRNTFILB = 'WCR0503';
          isError  = *On;
       EndIf;

       // Validation Check for expense constant Y/N.
       If scINCEXPCO = *Blanks;
          erINCEXPCO = *On;
          msINCEXPCO = 'WCR3171';
          isError  = *On;
       EndIf;


       If isError = *off;
          Return *On;
       else;
          Return *Off;
       EndIf;

      /End-Free
     p ValidPnl        e
