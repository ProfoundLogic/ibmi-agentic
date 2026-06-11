     hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h*****************************************************************
     h/title WTRPTDPRCP - Promt for Deposit Recap Report
     h/define CONVERT
     h dftactgrp(*no) actgrp('QILE')
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     H*****************************************************************
     H/TITLE WTRPTDPRCP - Promt for Deposit Recap Report
     h*****************************************************************
     h* Revisions:                                                    *
     h*                                                               *
     h*                                                               *

     fWtrptdprcpcf   e             workstn extfile('WTRPTDPRCP')
     f                                     handler('PROFOUNDUI(HANDLER)')
     fWtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdPolPnl)
     f                                     include(rcdQtePnl)
     f                                     include(rcdComPnl)
     f*
     f* User Parameter Master File
     fWmupl     IF   E           K DISK
     f*
     fWmeml     IF   E           k DISK
     fWdelp     if   e             disk
     f                                     Rename(pel01:pel0p)
     fWmaap     IF   E             DISK

     dpsds           esds                  extname(szq1p)

      *
     d WtDpRcpR        pr                  Extpgm('WTRPTDPRCP')
     d  inPrds                       40A   const
     d  inCall                        1A
     d
     d WtDpRcpR        pi
     d  inPrds                       40A   const
     d  inCall                        1A

     d strEmpName      s             40
     d inEmpNum        s              9  0
     d strAgtName      s             40
     d inAgtNum        s              5  0
     d inPEDtF         s              8
     d inPEDtT         s              8
     d inBalDt         s              8
     d inAccSts        s              1
     d inAccType       s              1
     d inAcSt          s              1
     d inPriSort       s              2
     d inSecSort       s              2
     d inEmFg          s              1
     d inAgFg          s              1
     d inPrem          s              1
     d inRgTp          s              2
     d in1lRg          s             15
     d in1hRg          s             15
     d inRgT2          s              2
     d in2lRg          s             15
     d in2hRg          s             15
     d inOutq          s             10
     d inMode          s              1

     d charMsg         s            100
     d outRrn#         s              9
     d inRrn           s              9  0
     d empRrn          s              9  0
     d isValid         s               n   Inz(*off)
     d intCo#          s                   Like(ELco#)
     d intFnd          s                   Like(ELfnd)
     d intEmp#         s                   Like(ELemp#)
     d intDiv          s                   Like(ELdiv)
     d strOutq         s                   Like(Upoutq)
     d inSelect        s           1024
      *
     d inKc            s              1
     d inKl            s              1
     d inErr           s              1
     d inCo#           s              3
     d inFnd           s              3
     d inFyr           s              3
     d inEmp#          s              9
     d inDiv           s              5
      *****************************************************************
      * Prototype Declaration
      *****************************************************************

     d/copy sPrString
     d/copy sPrCompany
     d/copy wPrAgent
     d/copy wPrUsrPrms
     d/copy wPrPolErr

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

      * Deposit Report Proc.
     d DpRcpProc       pr

      * Deposit Recap Report Submit
     d DpRcpSbm        pr                  ExtPgm('W4080C')
     d inCo#                          3
     d inFnd                          3
     d inPEDtF                        8
     d inPEDtT                        8
     d inBalDt                        8
     d inAccSts                       1
     d inAccType                      1
     d inAcSt                         1
     d inPriSort                      2
     d inSecSort                      2
     d inEmFg                         1
     d inAgFg                         1
     d inPrem                         1
     d inEmp#                         9
     d inDiv                          5
     d inFyr                          3
     d inRgTp                         2
     d in1lRg                        15
     d in1hRg                        15
     d inRgT2                         2
     d in2lRg                        15
     d in2hRg                        15
     d inOutq                        10
     d inMode                         1

      /Free
       // Start Of Program
       ProgramStart();

       // Main Procesing
       Main();

       *InLr = *on;

      /End-Free
       //===================================================================
       // DpRcpProc - Deposit Report Process Submit
       //===================================================================
     p DpRcpProc       b
     d DpRcpProc       pi

      /Free

       inCo# = %EditC(intCo#:'X');

       If scFnd <> 0;
          inFnd   = %EditC(scFnd:'X');
       EndIf;

       If scPEDtF <> 0;
          inPEDtF = %EditC(scPEDtF:'X');
       Else;
          inPEDtF = *Blanks;
       EndIf;

       If scPEDtT <> 0;
          inPEDtT = %EditC(scPEDtT:'X');
       Else;
          inPEDtT = *Blanks;
       EndIf;

       If scBalDt <> 0;
          inBalDt = %EditC(scBalDt:'X');
       Else;
          inBalDt = *Blanks;
       EndIf;

       inAccSts  = scAccSts;
       inAccType = scAccType;
       inAcSt    = 'E';
       inPriSort = scPriSort;
       inSecSort = scSecSort;
       inEmFg    = 'M';
       inAgFg    = 'M';
       inPrem    = ' ';
       inEmp#    = *blanks;
       inDiv     = *blanks;
       inFyr     = '000';

       If scEmpNmF <> *blanks;
          inRgTp  = 'EA';
          in1lRg  = Upper(scEmpNmF);
          in1hRg  = Upper(scEmpNmT);
       EndIf;

       If scEmpNoF <> 0;
          inRgTp  = 'EN';
          in1lRg  = ' ' + %EditC(scEmpNoF:'X') + '00000';
          in1hRg  = ' ' + %EditC(scEmpNoT:'X') + '99999';
       EndIf;

       If scAgtNmF <> *blanks;
          inRgT2  = 'AA';
          in2lRg  = Upper(scAgtNmF);
          in2hRg  = Upper(scAgtNmT);
       EndIf;

       If scAgtNoF <> 0;
          inRgT2  = 'AN';
          in2lRg  = '          ' + %EditC(scAgtNoF:'X');
          in2hRg  = '          ' + %EditC(scAgtNoT:'X');
       EndIf;

       inOutq     = strOutq;
       inMode     = 'D';

       DpRcpSbm(inCo#:inFnd:inPEDtF:inPEDtT:inBalDt:inAccSts:
          inAccType:inAcSt:inPriSort:inSecSort:inEmFg:inAgFg:
          inPrem:inEmp#:inDiv:inFyr:inRgTp:in1lRg:in1hRg:
          inRgT2:in2lRg:in2hRg:inOutq:inMode);

      /End-Free

     p DpRcpProc       e
       //===================================================================
       // LoadInVal - Intialize the Screen fields
       //===================================================================
     p LoadInVal       b
     d LoadInVal       pi

      /Free
       scFnd      = 0;
       scPEDtF    = 0;
       scPEDtT    = 0;
       scBalDt    = 0;
       scAccType  = *Blanks;
       scAccSts   = *Blanks;
       scPriSort  = *Blanks;
       scSecSort  = *Blanks;
       scEmpNmF   = *Blanks;
       scEmpNmT   = *Blanks;
       scEmpNoF   = 0;
       scEmpNoT   = 0;
       scAgtNmF   = *Blanks;
       scAgtNmT   = *Blanks;
       scAgtNoF   = 0;
       scAgtNoT   = 0;

       sccompany# = %Char(userCo#(q1user));

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
                DpRcpProc();
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
             // Clear Errors
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
          Write RcdTabDepR;

          Exfmt RcdDepRcp;

          Read rcdComPnl;
          Read RcdTabDepR;

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
       intCo# = userCo#(q1user);
       scFnd  = userGrp(q1user);
       scCompName = GetCompNam(intCo#);

       // Retrieve Output Queue of the User.
       Chain(n) q1user pup01;
       If %Found(Wmupl);
          strOutq = Upoutq;
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
       // ClrErr - Clear Error/Message fields
       //===================================================================
     p ClrErr          b
     d ClrErr          pi

      /Free

       isValid    = *off;
       msFnd      = *blanks;
       erFnd      = *off;
       msPEffToDt = *blanks;
       erPEffToDt = *off;
       msPEffFrDt = *blanks;
       erPEffFrDt = *off;
       msBalDt    = *blanks;
       erBalDt    = *off;
       msAccTyp   = *blanks;
       erAccTyp   = *off;
       msAccSts   = *blanks;
       erAccSts   = *off;
       msPriSort  = *blanks;
       erPriSort  = *off;
       msSecSort  = *blanks;
       erSecSort  = *off;

       msEmpNmF1  = *blanks;
       msEmpNmF2  = *blanks;
       msEmpNmF3  = *blanks;
       erEmpNmF1  = *off;
       erEmpNmF2  = *off;
       erEmpNmF3  = *off;
       msEmpNmT1  = *blanks;
       msEmpNmT2  = *blanks;
       erEmpNmT1  = *off;
       erEmpNmT2  = *off;
       msEmpNoF1  = *blanks;
       msEmpNoF2  = *blanks;
       erEmpNoF1  = *off;
       erEmpNoF2  = *off;
       msEmpNoT   = *blanks;
       erEmpNoT   = *off;

       msAgtNmF1  = *blanks;
       msAgtNmF2  = *blanks;
       msAgtNmF3  = *blanks;
       erAgtNmF1  = *off;
       erAgtNmF2  = *off;
       erAgtNmF3  = *off;
       msAgtNmT1  = *blanks;
       msAgtNmT2  = *blanks;
       erAgtNmT1  = *off;
       erAgtNmT2  = *off;
       msAgtNoF1  = *blanks;
       msAgtNoF2  = *blanks;
       erAgtNoF1  = *off;
       erAgtNoF2  = *off;
       msAgtNoT   = *blanks;
       erAgtNoT   = *off;

      /End-Free

     p ClrErr          e
       //===================================================================
       // ValidPnl - Validate Panel Screen Fields
       //===================================================================
     p ValidPnl        b
     d ValidPnl        pi

      /Free

       // Validation Check for Group
       If scfnd = *Zeros;
          erFnd      = *On;
          msFnd      = 'WCR0503';
          isValid    = *On;
       EndIf;

       // Validation Check for Effective To Date.
       erPEffToDt = ValidDate(scPEDtT:charMsg);
       If erPEffToDt = *on;
          msPEffToDt = charMsg;
          isValid    = *on;
       Endif;

       // Validation Check for Effective From Date.
       erPEffFrDt = ValidDate(scPEDtF:charMsg);
       If erPEffFrDt = *on;
          msPEffFrDt = charMsg;
          isValid    = *on;
       Endif;

       If erPEffToDt = *off and erPEffFrDt = *off;
          erPEffToDt = AscendDates(scPEDtF:scPEDtT:charMsg);
          If erPEffToDt = *on;
             erPEffFrDt = *on;
             msPEffToDt = charMsg;
             msPEffFrDt = charMsg;
             isValid    = *on;
          Endif;
       Endif;

       // Validation Check for Balance Date.
       erBalDt = ValidDate(scBalDt:charMsg);
       If erBalDt = *on;
          msBalDt = charMsg;
          isValid    = *on;
       Endif;

       // Validation Check for Account Type.
       If scAccType = *Blanks;
          erAccTyp   = *On;
          msAccTyp   = 'WCR0503';
          isValid    = *On;
       EndIf;

       // Validation Check for Account Status.
       If scAccSts  = *Blanks;
          erAccSts   = *On;
          MSAccSts   = 'WCR0503';
          isValid    = *On;
       EndIf;

       // Validation Check for Primary Sort.
       If scPriSort = *Blanks;
          erPriSort  = *On;
          MSPriSort  = 'WCR0503';
          isValid    = *On;
       EndIf;

       // Validation Check for Secondary Sort.
       If scPriSort = 'AA' or scPriSort = 'AN';
          If scSecSort = *Blanks;
             erSecSort  = *On;
             MSSecSort  = 'WCR3133';
             isValid    = *On;
          EndIf;
       EndIf;

       // Validation Check Employer Name & Number Range
       // (1) Both Name Range and Number range are Mutually Exclusive
       If (scEmpNmF <> *Blanks or scEmpNmT <> *Blanks)
        and (scEmpNoF <> *Zeros or scEmpNoT <> *Zeros);
          erEmpNmF1 = *On;
          msEmpNmF1 = 'WCR0918';
          erEmpNmT1 = *On;
          msEmpNmT1 = 'WCR0918';
          erEmpNoF1 = *On;
          msEmpNoF1 = 'WCR0918';
          erEmpNoT  = *On;
          msEmpNoT  = 'WCR0918';
          isValid   = *On;
       EndIf;

       // (2) Both FROM Name and To Name not enterted
       If scEmpNmF = *Blanks and scEmpNmT <> *Blanks;
          erEmpNmF2 = *On;
          msEmpNmF2 = 'WCR0503';
          isValid   = *On;
       EndIf;
       If scEmpNmF <> *Blanks and scEmpNmT = *Blanks;
          erEmpNmT2 = *On;
          msEmpNmT2 = 'WCR0503';
          isValid   = *On;
       EndIf;

       // (3) FROM Name/Number is greater than TO Name/Number
       If scEmpNmF > scEmpNmT;
          erEmpNmF3 = *On;
          msEmpNmF3 = 'WCR0750';
          isValid   = *On;
       EndIf;
       If scEmpNoF > scEmpNoT;
          erEmpNoF2 = *On;
          msEmpNoF2 = 'WCR0750';
          isValid   = *On;
       EndIf;

       // Validation Check Agent Name & Number Range
       // (1) Both Name Range and Number range are Mutually Exclusive
       If (scAgtNmF <> *Blanks or scAgtNmT <> *Blanks)
        and (scAgtNoF <> *Zeros or scAgtNoT <> *Zeros);
          erAgtNmF1 = *On;
          msAgtNmF1 = 'WCR0918';
          erAgtNmT1 = *On;
          msAgtNmT1 = 'WCR0918';
          erAgtNoF1 = *On;
          msAgtNoF1 = 'WCR0918';
          erAgtNoT  = *On;
          msAgtNoT  = 'WCR0918';
          isValid   = *On;
       EndIf;

       // (2) Both FROM Name and To Name not enterted
       If scAgtNmF = *Blanks and scAgtNmT <> *Blanks;
          erAgtNmF2 = *On;
          msAgtNmF2 = 'WCR0503';
          isValid   = *On;
       EndIf;
       If scAgtNmF <> *Blanks and scAgtNmT = *Blanks;
          erAgtNmT2 = *On;
          msAgtNmT2 = 'WCR0503';
          isValid   = *On;
       EndIf;

       // (3) FROM Name/Number is greater than TO Name/Number
       If scAgtNmF > scAgtNmT;
          erAgtNmF3 = *On;
          msAgtNmF3 = 'WCR0750';
          isValid   = *On;
       EndIf;
       If scAgtNoF > scAgtNoT;
          erAgtNoF2 = *On;
          msAgtNoF2 = 'WCR0750';
          isValid   = *On;
       EndIf;


      /End-Free
     p ValidPnl        e
