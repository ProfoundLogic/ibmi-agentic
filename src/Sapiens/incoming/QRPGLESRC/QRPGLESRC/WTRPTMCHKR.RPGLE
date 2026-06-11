     hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h*****************************************************************
     h/title WC2461 - Monthly Check Register
     h dftactgrp(*no) actgrp(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     H*****************************************************************
     H/TITLE WTRPTMCHKR - Monthly Check Register
     h*****************************************************************
     h* Revisions                                                     *
     h*****************************************************************
     fWTRPTMCHKRcf   e             workstn extfile('WTRPTMCHKR')
     f                                     handler('PROFOUNDUI(HANDLER)')
     fWtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdPolPnl)
     f                                     include(rcdQtePnl)
     f                                     include(rcdComPnl)
     f*
     f* User Parameter Master File
     f                                     INFDS(FIDS01)
     fWmupl     IF   E           K DISK    Rename(pup01:pup00)
     fWmeml     IF   E           k DISK    Rename(pem01:pem00)

     dFIDS01         E DS                  EXTNAME(SZQ2P)
     dpsds           esds                  extname(szq1p)
      *
     d WtMonChkR       pr                  Extpgm('WTRPTMCHKR')
     d  inPrds                       40A   const
     d  inCall                        1A
     d
     d WtMonChkR       pi
     d  inPrds                       40A   const
     d  inCall                        1A

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
     d intFyr          s              3
     d inEmp#          s              9
     d inDiv           s              5
     d inDte           s              8
     d outDte          s              8
     d outsuffix       s              3
     d strMsg          s            100
     d inLr            s              1
     d inOutq          s             10
     d outOutq         s             10
     d inRRN           s              9  0

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

      * Clear Error/Message fields
     d ClrErr          pr

     d GetSuffix       PR                  extpgm('SA030R')
     d outsuffix                      3

      * Get client ID
      * Intialize the Screen fields
     d LoadInVal       pr

      * Main Processing
     d Main            pr

      * Monthly Check Register Proc.
     d Mchkrproc       pr

      * Program Start
     d ProgramStart    pr

      * Set Criteria
     d SetCriteria     pr

      * Validate Panel Sreen Values
     d ValidPnl        pr

      * Monthly Check Register Submit
     d MchkrSbm        pr                  ExtPgm('W7033C')
     d outCo#                         3
     d outFnd                         3
     d outDte                         8
     d outFyr                         3
     d outOutq                       10

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
         ergrpDsr   = *off;
         erPolYr    = *off;
         erRunDt    = *off;
         msgrpDsr   = *blanks;
         msPolYr    = *blanks;
         msRunDt    = *blanks;
         isValid   = *off;

      /End-Free

     p ClrErr          e
       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

      /Free

       // Process until exit button is pressed.
       Dou ExitNow = *On ;

          setCriteria();

          // Display Screen
          Exsr ExfmtScreens;

          Select;
          When BtnSubmit = *on;
             // Clear Errors
             ClrErr();
             // Validate Edits
             ValidPnl();
             If isValid = *Off;
                Mchkrproc();
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
             SetCriteria();
             ClrErr();
             // Load Action panel with the Company Name.
             intCo#   = userCo#(q1user);
             intFnd   = userGrp(q1user);
             scCOMPNAME = GetCompNam(intCo#);
             scGRPDSR = userGrp(q1user);
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
          Write RcdTabMchk;

          Exfmt RcdMchk;

          Read rcdComPnl;
          Read RcdTabMchk;

       EndSl;

       Endsr;

      /End-Free

     p Main            e
       //===================================================================
       // Mchkrproc - Monthly Check Register Submit
       //===================================================================
     p Mchkrproc       b
     d Mchkrproc       pi

      /Free

       outCo#   = %editc(intCo#:'X');
       //*** Begin Chg *** 03/20/17 ********************************
       //***outFnd   = %char(scGRPDSR);
       //***outDte   = %char(scRUNDT);
       //***outFyr   = scPOLYR;
       outFnd   = %EditC(scGRPDSR:'X');
       outDte   = %EditC(scRUNDT:'X');

       If scPolYr = ' ';
          outFyr = '000';
       Else;
          outFyr   = scPOLYR;
       EndIf;
       //*** End   Chg *** 03/20/17 ********************************
       outOutQ  = strOUTQ;

        MchkrSbm (outCo#:outFnd:outDte:outFyr:outOutQ);

      /End-Free

     p Mchkrproc       e
       //===================================================================
       // LoadInVal - Intialize the Screen fields
       //===================================================================
     p LoadInVal       b
     d LoadInVal       pi

      /Free

             Chain q1user pup00;
             If %Found(Wmupl);
                //*** Begin Chg *** 03/20/17 **************************
                //***scPOLYR = %char(UPFYR);
                scPOLYR = %EditC(UPFYR:'X');
                //*** End   Chg *** 03/20/17 **************************
             EndIf;
             scGRPDSR = userGrp(q1user);
             scRUNDT    = Dateto8(%date());

      /End-Free

     p LoadInVal       e
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
       intFnd     = userGrp(q1user);
       scCOMPNAME = GetCompNam(intCo#);
       scGRPDSR = userGrp(q1user);

       // Retrieve Output Queue of the User.
       Chain q1user pup00;
       If %Found(Wmupl);
          strOutq = UPOUTQ;
          //*** Begin Chg *** 03/20/17 **************************
          //***scPOLYR = %char(UPFYR);
          scPOLYR = %EditC(UPFYR:'X');
          //*** End   Chg *** 03/20/17 **************************
       EndIf;

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
       // ValidPnl - Validate Panel Screen Fields
       //===================================================================
     p ValidPnl        b
     d ValidPnl        pi

      /Free
         // Validation Check for Fund
        If scgrpDsr = *Zeros;
         ergrpDsr = *On;
         msgrpDsr = 'WCR0503';
         isValid = *on;
        EndIf;

         //Run Date Validation
       If scRunDt    = *Zeros;
          erRunDt    = *On;
          msRunDt    = 'WCR1391';
          isValid = *on;
       Endif;

       If scRunDt <> *Zeros;
          erRunDt = ValidDate(scRunDt:strMsg);
          If erRunDt = *On;
             msRunDt = StrMsg;
             isValid = *on;
          Endif;
       Endif;

      /End-Free
     p ValidPnl        e
