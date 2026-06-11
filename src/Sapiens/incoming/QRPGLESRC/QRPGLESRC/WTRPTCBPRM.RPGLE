     hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h*****************************************************************
     h/title WTRPTCBPRM - Promt for Calculated vs Billed Premium Report
     h/define CONVERT
     h dftactgrp(*no) actgrp('QILE')
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     H*****************************************************************
     H/TITLE WTRPTCBPRM - Promt for Calculated vs Billed Premium Report
     h*****************************************************************
     h* Revisions:                                                    *
     h*                                                               *
     h*                                                               *

     fWtrptcbprmcf   e             workstn extfile('WTRPTCBPRM')
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
     d WtCBPrmR        pr                  Extpgm('WTRPTCBPRM')
     d  inPrds                       40A   const
     d  inCall                        1A
     d
     d WtCBPrmR        pi
     d  inPrds                       40A   const
     d  inCall                        1A

     d inCo#           s              3
     d inFnd           s              3
     d inPEDtF         s              8
     d inPEDtT         s              8
     d inSRAC          s              1
     d inARcds         s             14
     d inOutput        s              1
     d inOutq          s             10
     d inSort          s              1

     d charMsg         s            100
     d isValid         s               n   Inz(*off)
     d intCo#          s              3  0
     d strOutq         s                   Like(Upoutq)
     d inARcdsTmp      s            200    inz(*Blank)
     d intpos          s              3  0 inz(0)
     d outsufx         s              3
     d strrptl         s             23
     d strARdesc       s             30
      *
      *****************************************************************
      * Prototype Declaration
      *****************************************************************

     d/copy sPrCompany
     d/copy wPrUsrPrms
     d/copy wPrPolErr
     d/copy sPrString

     dSA030R           PR                  extpgm('SA030R')
     d outsufx                        3

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

      * Calculated vs Billed Premium Report Proc.
     d CBPrmProc       pr

      * Calculated vs Billed Premium Report Submit
     d CBPrmSbm        pr                  ExtPgm('W7660C')
     d inCo#                          3
     d inFnd                          3
     d inPEDtF                        8
     d inPEDtT                        8
     d inSRAC                         1
     d inARcds                       14
     d inOutput                       1
     d inOutq                        10
     d inSort                         1

      /Free
       // Start Of Program
       ProgramStart();

       // Main Procesing
       Main();

       *InLr = *on;

      /End-Free
       //===================================================================
       // CBPrmProc - Calculated vs Billed Premium Report Submit
       //===================================================================
     p CBPrmProc       b
     d CBPrmProc       pi

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

       inSRAC    = scSRAC;
       inARcds   = inARcdsTmp;
       inOutput  = scOutput;
       inOutq    = strOutq;
       inSort    = scSort;

       CBPrmSbm(inCo#:inFnd:inPEDtF:inPEDtT:inSRAC:inARcds:
          inOutput:inOutq:inSort);

      /End-Free

     p CBPrmProc       e
       //===================================================================
       // LoadInVal - Intialize the Screen fields
       //===================================================================
     p LoadInVal       b
     d LoadInVal       pi

      /Free
       scFnd      = 0;
       scPEDtF    = 0;
       scPEDtT    = 0;
       scSRAC     = *Blanks;
       scARcodes  = *Blanks;
       scOutput   = *Blanks;
       scSort     = *Blanks;

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
                CBPrmProc();
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
          Write RcdTabCBR;

          Exfmt RcdCBPrmR;

          Read rcdComPnl;
          Read RcdTabCBR;

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

       scSRAC = 'Y';

       // Retrieve Output Queue of the User.
       Chain(n) q1user pup01;
       If %Found(Wmupl);
          strOutq = Upoutq;
       EndIf;

       // Retrieve Output Suffix.
       SA030r(outSufx);
       If outSufx = *blanks;
         strrptl = 'PDSRPT' + ', if selected.';
       Else;
         strrptl = 'PDSRPT' + outSufx + ', if selected.';
       EndIf;

       scrpt = 'This Report will produce output file WFARP in library '+
        strrptl;

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
                scARSel = %trim(scARSel)+','+%trim(TTkey)+'-'+%trim(strARdesc);
                scARVal = %trim(scARVal)+','+%trim(TTkey);
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
       msPEffToDt = *blanks;
       erPEffToDt = *off;
       msPEffFrDt = *blanks;
       erPEffFrDt = *off;
       msOutput   = *blanks;
       erOutput   = *off;
       msSort     = *blanks;
       erSort     = *off;
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

       // Validation Check for Output Type
       If scOutput  = *Blanks;
         erOutput   = *On;
         msOutput   = 'WCR0503';
         isValid    = *On;
       EndIf;

       // Validation Check for Sort.
       If scSort = *Blanks;
         erSort  = *On;
         MSSort  = 'WCR0503';
         isValid = *On;
       EndIf;

       // Validation AR Codes.
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
          If %len(%trim(inarcdsTmp)) > 14;
             erARcd  = *On;
             msARcd  = 'WCR4815';
             isValid = *On;
          EndIf;
       EndIf;

      /End-Free
     p ValidPnl        e
