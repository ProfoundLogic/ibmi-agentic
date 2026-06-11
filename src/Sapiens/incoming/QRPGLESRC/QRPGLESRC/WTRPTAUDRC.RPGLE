     hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h*****************************************************************
     h/title WTRPTAUDRC - Promt for Final Audit Recap (Prem/Pmt Comparison) Report
     h/define CONVERT
     h dftactgrp(*no) actgrp('QILE')
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     H*****************************************************************
     H/TITLE WTRPTAUDRC - Promt for Final Audit Recap (Prem/Pmt Comparison) Report
     h*****************************************************************
     h* Revisions:                                                    *
     h*                                                               *
     h* 06/19/17 C:65  Project: Phw#46743 (RMLA)                      *
     h*                Fix defaulted parameters for status and        *
     h*                employer/agent mailing address flags.          *
     h*                                                               *
     h*****************************************************************

     fWtrptaudrccf   e             workstn extfile('WTRPTAUDRC')
     f                                     handler('PROFOUNDUI(HANDLER)')
     fWtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdPolPnl)
     f                                     include(rcdQtePnl)
     f                                     include(rcdComPnl)
     f*
     f* User Parameter Master File
     fWmupl     IF   E           K DISK
     f* Transaction Type Code file for AR Payment codes
     fwtabl     if   e           k Disk    include(ptt01)

     dpsds           esds                  extname(szq1p)

      *
     d WtAudRcR        pr                  Extpgm('WTRPTAUDRC')
     d  inPrds                       40A   const
     d  inCall                        1A
     d
     d WtAudRcR        pi
     d  inPrds                       40A   const
     d  inCall                        1A

     d inCo#           s              3
     d inFnd           s              3
     d inPEDtF         s              8
     d inPEDtT         s              8
     d inPayDT         s              8
     d inRdSt          s              1
     d inAccType       s              1
     d inBilSts        s              1
     d inStat          s              1
     d inPriSort       s              2
     d inSecSort       s              2
     d inEmpF          s              1
     d inAmpF          s              1
     d inPrty          s              2
     d inEmp#          s              9
     d inDiv           s              5
     d inFyr           s              3
     d inSel1          s              2
     d inS1Fr          s             15
     d inS1To          s             15
     d inSel2          s              2
     d inS2Fr          s             15
     d inS2To          s             15
     d inARcds         s             60
     d inArCodIE       s              1
     d inSort          s              2
     d inOutq          s             10

     d charMsg         s            100
     d isValid         s               n   Inz(*off)
     d intCo#          s              3  0
     d strOutq         s                   Like(Upoutq)
     d inARcdsTmp      s            200    inz(*Blank)
     d intpos          s              3  0 inz(0)
     d strARdesc       s             30
     d strPayDt        s               d   inz
      *
      *****************************************************************
      * Prototype Declaration
      *****************************************************************

     d/copy sPrCompany
     d/copy wPrUsrPrms
     d/copy wPrPolErr
     d/copy sPrString
     d/copy sPrDate

      * Get client ID
      * Intialize the Screen fields
     d LoadInVal       pr

      * Main Processing
     d Main            pr

      * Program Start
     d ProgramStart    pr

      * Set Criteria
     d SetCriteria     pr

      * Load AR Payment Codes select box
     d LoadARCodes     pr

      * Clear Error/Message fields
     d ClrErr          pr

      * Validate Panel Sreen Values
     d ValidPnl        pr

      * Final Audit Recap Report Proc.
     d AudRcProc       pr

      * Final Audit Recap Report Submit
     d AudRcSbm        pr                  ExtPgm('W4130C')
     d inCo#                          3
     d inFnd                          3
     d inPEDtF                        8
     d inPEDtT                        8
     d inPayDT                        8
     d inRdSt                         1
     d inAccType                      1
     d inBilSts                       1
     d inStat                         1
     d inPriSort                      2
     d inSecSort                      2
     d inEmpF                         1
     d inAmpF                         1
     d inPrty                         2
     d inEmp#                         9
     d inDiv                          5
     d inFyr                          3
     d inSel1                         2
     d inS1Fr                        15
     d inS1To                        15
     d inSel2                         2
     d inS2Fr                        15
     d inS2To                        15
     d inARcds                       60
     d inArCodIE                      1
     d inSort                         2
     d inOutq                        10

      /Free
       // Start Of Program
       ProgramStart();

       // Main Procesing
       Main();

       *InLr = *on;

      /End-Free
       //===================================================================
       // AudRcProc - Final Audit Recap Report Submit
       //===================================================================
     p AudRcProc       b
     d AudRcProc       pi

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

       If scPayDt <> 0;
          inPayDt = %EditC(scPayDt:'X');
       Else;
          inPayDt = *Blanks;
       EndIf;

       inRdSt    = 'B';
       inAccType = scAccType;
       inBilSts  = scBilSts;
       inStat    = 'E';
       inPriSort = scPriSort;
       inSecSort = *Blanks;
       //*** begin chg ***  06/19/17  ******************************************
       //*****inStat    = 'M';
       //*****inStat    = 'M';
       inEmpF    = 'M';
       inAmpF    = 'M';
       //*** end chg *****  06/19/17  ******************************************
       inPrty    = *Blanks;
       inEmp#    = '000000000';
       inDiv     = '00000';
       inFyr     = '000';
       inSel1    = *Blanks;
       inS1Fr    = *Blanks;
       inS1To    = *Blanks;
       inSel2    = *Blanks;
       inS2Fr    = *Blanks;
       inS2To    = *Blanks;
       inARcds   = inARcdsTmp;
       inArCodIE = scArCodIE;
       inSort    = scPriSort;
       inOutq    = strOutq;

       AudRcSbm(inCo#:inFnd:inPEDtF:inPEDtT:inPayDT:inRdSt:inAccType:
          inBilSts:inStat:inPriSort:inSecSort:inEmpF:inAmpF:inPrty:inEmp#:
          inDiv:inFyr:inSel1:inS1Fr:inS1To:inSel2:inS2Fr:inS2To:
          inARcds:inArCodIE:inSort:inOutq);

      /End-Free

     p AudRcProc       e
       //===================================================================
       // LoadInVal - Intialize the Screen fields
       //===================================================================
     p LoadInVal       b
     d LoadInVal       pi

      /Free
       scFnd      = 0;
       scPEDtF    = 0;
       scPEDtT    = 0;
       scPayDt    = 0;
       scAccType  = *Blanks;
       scBilSts   = *Blanks;
       scPriSort  = *Blanks;
       scArCodIE  = *Blanks;
       scARcodes  = *Blanks;

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
          LoadARCodes();

          // Display Screen
          Exsr ExfmtScreens;

          Select;
          When BtnSubmit = *on;
             // Clear Errors
             ClrErr();
             // Validate Edits
             ValidPnl();
             If isValid = *Off;
                AudRcProc();
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
             scPayDt = DateTo8(strPayDt);
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
          Write RcdTabFAR;

          Exfmt RcdAudRcR;

          Read rcdComPnl;
          Read RcdTabFAR;

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

       // Load system date into Payments as of Date.
       strPayDt = %Date();
       scPayDt = DateTo8(strPayDt);

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
       // LoadARCodes - Load AR Payment Codes Select Box
       //===================================================================
     p LoadARCodes     b
     d LoadARCodes     pi

      /Free
       scARSel = *Blanks;
       scARVal = *Blanks;
       strARdesc = *Blanks;

       // Read WTTTP file from starting
       Chain(n) intCo# ptt01;
       If %found(wtabl);
          Dow not %Eof(wtabl);
             If TTstat = 'A';
                strARdesc = TitleCase(TTdesc);
                If scARSel = *Blanks;
                   scARSel = %trim(TTkey)+'-'+%trim(strARdesc);
                   scARVal = %trim(TTkey);
                else;
                   scARSel=%trim(scARSel)+','+%trim(TTkey)+'-'+%trim(strARdesc);
                   scARVal = %trim(scARVal)+','+%trim(TTkey);
                endif;
             endif;
          ReadE intCo# ptt01;
          enddo;
       endif;

      /End-Free

     p LoadARCodes     e
       //===================================================================
       // ClrErr - Clear Error/Message fields
       //===================================================================
     p ClrErr          b
     d ClrErr          pi

      /Free

       isValid    = *off;
       msFnd      = *blanks;
       erFnd      = *off;
       msPEffFrDt = *blanks;
       erPEffFrDt = *off;
       msPEffToDt = *blanks;
       erPEffToDt = *off;
       msPayDt    = *blanks;
       erPayDt    = *off;
       msAccTyp   = *blanks;
       erAccTyp   = *off;
       msBilSts   = *blanks;
       erBilSts   = *off;
       msPriSort  = *blanks;
       erPriSort  = *off;
       msArCodIE1 = *blanks;
       erArCodIE1 = *off;
       msArCodIE2 = *blanks;
       erArCodIE2 = *off;
       msARcd     = *blanks;
       erARcd     = *off;

       inarcdsTmp = *blanks;
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

       // Validation Check for Effective From Date.
       erPEffFrDt = ValidDate(scPEDtF:charMsg);
       If erPEffFrDt = *on;
          msPEffFrDt = charMsg;
          isValid    = *on;
       Endif;

       // Validation Check for Effective To Date.
       erPEffToDt = ValidDate(scPEDtT:charMsg);
       If erPEffToDt = *on;
          msPEffToDt = charMsg;
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

       // Validation Check for Payments as of Date.
       erPayDt = ValidDate(scPayDt:charMsg);
       If erPayDt = *on;
          msPayDt = charMsg;
          isValid    = *on;
       Endif;

       // Validation Check for Account Type.
       If scAccType = *Blanks;
         erAccTyp   = *On;
         msAccTyp   = 'WCR0503';
         isValid    = *On;
       EndIf;

       // Validation Check for Billing status.
       If scBilSts  = *Blanks;
         erBilSts   = *On;
         msBilSts   = 'WCR0503';
         isValid    = *On;
       EndIf;

       // Validation Check for Primary Sort.
       If scPriSort = *Blanks;
         erPriSort  = *On;
         MSPriSort  = 'WCR0503';
         isValid    = *On;
       EndIf;

       // Validation Check for A/R Payment Codes Inclusion/Exclusion.
       If scARcodes = *Blanks and scArCodIE <> ' ';
          erArCodIE1 = *on;
          msArCodIE1 = 'WC31098';
          isValid    = *On;
       EndIf;

       If scARcodes <> *Blanks and scArCodIE <> 'I' and scArCodIE <> 'E';
          erArCodIE2 = *on;
          msArCodIE2 = 'WCR4817';
          isValid    = *On;
       EndIf;

       // Validation Check for AR Codes.
       If scARcodes <> *Blanks;
          inarcdsTmp = ' ';
          inarcdsTmp =  %trim(%xlate(',':' ':scARcodes));
          intpos = 0;
          Dou intpos = 0;
             intpos = %scan(' ':%trim(inarcdsTmp));
             If intpos <> 0;
                inarcdsTmp = %replace('':inarcdsTmp:intpos:1);
             EndiF;
          EndDo;
          If %len(%trim(inarcdsTmp)) > 60;
             erARcd  = *On;
             msARcd  = 'WCR4816';
             isValid = *On;
          EndIf;
       EndIf;

      /End-Free
     p ValidPnl        e
