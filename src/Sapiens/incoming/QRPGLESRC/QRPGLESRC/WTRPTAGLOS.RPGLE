     hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h*****************************************************************
     h/title WC2459 - Prompt for Agent Loss Ratio (PPR Report)
     h/define CONVERT
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     H*****************************************************************
     H/TITLE WTRPTAGLOS - Prompt for Agent Loss Ratio (PPR Report)
     h*****************************************************************
     h* Revisions                                                     *
     H*                                                               *
     H* 08/05/24  C:291  Support: 117251                              *
     H*                  Removed edit on Agent # to validate against  *
     H*                  User's default Group when Producer # search  *
     H*                  icon is clicked.                             *
     H*                                                               *
     H* 11/30/21  C:333  Support: 97363 (UBIC)                        *
     H*                  Blank report is generating for explicit      *
     H*                  selection of Agent/Producer from the UI.     *
     H*                                                               *
     H* 09/26/18  C:72   Support: 140/73282                           *
     H*                  Edit Agent against group select file         *
     h*****************************************************************
     fWTRPTAGLOScf   e             workstn extfile('WTRPTAGLOS')
     f                                     handler('PROFOUNDUI(HANDLER)')
     fWtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdPolPnl)
     f                                     include(rcdQtePnl)
     f                                     include(rcdComPnl)
     f*
     f* User Parameter Master File
     fWmfnl     IF   E           K DISK    INFDS(FNFDS)
     f                                     Rename(pfn01:pfn00)
     fWmeml     IF   E           k DISK    Rename(pem01:pem00)
     fsmcol     IF   E           k DISK    Rename(pco01:pco00)
     fqfeul     IF   E           K DISK    Rename(peu01:peu00)
     fqfe2l     IF   E           K DISK    Rename(pe201:pe200)
     fWmupl     IF   E           K DISK    Rename(pup01:pup00)
     fwmaap     IF   E             DISK    RENAME(PAA01:PAA0P)
     fwmaal1    IF   E           K DISK    RENAME(PAA01:PAA02)
     f                                     INFDS(AAFDS1)
      *** Beg Add ***** 08/05/24 *****************************************
     fwmaal5    IF   E           K DISK    RENAME(PAA01:PAA05)
     f                                     INFDS(AAFDS5)
      *** End Add ***** 08/05/24 *****************************************
     fwmagl     IF   E           K DISK    RENAME(PAG01:PAG00)
     f                                     INFDS(AGFDS)
     fwmapl     IF   E           K DISK    RENAME(PAP01:PAP00)
     fwmapp     IF   E             DISK    RENAME(PAP01:PAP0P)
     f*** Beg Add *** 09/26/18 ******************************
     fwxgsl     if   e           k disk
     f*** End Add *** 09/26/18 ******************************

     D FNFDS           DS
     D  FNRRN                397    400B 0
     D AAFDS1          DS
     D  AARRN1               397    400B 0
      *** Beg Add ***** 08/05/24 *****************************************
     D AAFDS5          DS
     D  AARRN5               397    400B 0
      *** Beg Add ***** 08/05/24 *****************************************
     D AGFDS           DS
     D  AGRRN                397    400B 0

     dpsds           esds                  extname(szq1p)
      *
     d WtAgtLosR       pr                  Extpgm('WTRPTAGLOS')
     d  inPrds                       40A   const
     d  inCall                        1A
     d
     d WtAgtLosR       pi
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
     d inFnd           s              3
     d inFyr           s              3
     d inEmp#          s              9
     d inDiv           s              5
     d inDte           s              8
     d inFrDt          s              8
     d inToDt          s              8
     d inAgt           s              5
     d outsufx         s              3
     d strrptl         s             23
     d strMsg          s            100
     d inLr            s              1
     d inAgPr          s              1
     d inSort          s              1
     d inAgt#          s              5
     d inProd          s              5
     d inTyp           s              1
     d inOutP          s             10
     d inOutq          s             10
     d inMode          s              1
     d inKc            s              1
     d inKl            s              1
     d inRRN           s              9  0
     d inCallS         s              1
      *** Beg Add ***** 08/05/24 *****************************************
     d flgAgtFnd       s               n   inz(*Off)
      *** End Add ***** 08/05/24 *****************************************

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

     d SA030R          PR                  extpgm('SA030R')
     d outsufx                        3

     d GetAgtNum       PR                  extpgm('W0040R')
     d inKc                           1
     d inKl                           1
     d inCallS                        1
     d inRRN                          9  0

     d GetPrdNum       PR                  extpgm('W0042R')
     d inKc                           1
     d inKl                           1
     d inCallS                        1
     d inRRN                          9  0

      * Agent Loss Ratio (PPR Report) Proc.
     d AgtLosproc      pr

      * Clear Error/Message fields
     d ClrErr          pr

      * Get client ID

      * Intialize the Screen fields
     d LoadInVal       pr

      * Main Processing
     d Main            pr

      * Program Start
     d ProgramStart    pr

      * Validate Panel Sreen Values
     d ValidPnl        pr

      * Agent Loss Ratio (PPR Report) Submit
     d AgtLosSbm       pr                  ExtPgm('W7300C')
     d inCo#                          3
     d inFnd                          3
     d inFrDt                         7
     d inToDt                         7
     d inDte                          8
     d inOutq                        10
     d inMode                         1
     d inLr                           1
     d inAgPr                         1
     d insort                         1
     d inoutP                         1
     d inAgt#                         5
     d inprod                         5
     d intyp                          1

      /Free
       // Start Of Program
       ProgramStart();

       // Main Procesing
       Main();

       *InLr = *on;

      /End-Free
       //===================================================================
       // AgtLosproc - Agent Loss Ratio (PPR Report) Submit
       //===================================================================
     p AgtLosproc      b
     d AgtLosproc      pi

      /Free

        inCo# = %Editc(intCo#:'X');
        inFnd = *Zeros;
        inFrdt = %Char(%Date(scPolEftDt:*usa):*cymd0);
        inTodt = %Char(%Date(scPolExpDt:*usa):*cymd0);
        //*** Begin Chg *** 06/23/17 ****************************
        //**********inDte   = %Char(scRunDt);
        inDte   = %EditC(scRunDt:'X');
        //*** End   Chg *** 06/23/17 ****************************
        inOutQ  = strOutQ;
        inMode  = 'D';
        If scLossR  = 'Y';
           inLr    = 'Y';
        Else;
           inLr    = ' ';
        EndIf;
        If scPPR = 'Y';
           inAgPr  = 'Y';
        Else;
           inAgPr  = ' ';
        EndIf;
        inSort  = scSort;
        inOutP  = scOutP;
      //*** Beg Chg *** 11/30/21 *****************************************
      //*** inAgt#  = %Char(scAgt#);
      //*** inProd  = %Char(scProd);
        inAgt#  = %EditC(scAgt#:'X');
        inProd  = %EditC(scProd:'X');
      //*** End Chg *** 11/30/21 *****************************************
        inTyp   = scRptTyp;

        AgtLosSbm (inCo#:inFnd:inFrDt:inToDt:inDte:inOutQ:inMode:
         inLr:inAgPr:inSort:inoutP:inAgt#:inProd:inTyp);

      /End-Free

     p AgtLosproc      e
       //===================================================================
       // ClrErr - Clear Error/Message fields
       //===================================================================
     p ClrErr          b
     d ClrErr          pi

      /Free
         isValid    = *off;
         erLossR    = *off;
         erPPR      = *off;
         erPolEftDt = *off;
         erPolExpDt = *off;
         erPriSort  = *off;
         erRptTyp   = *off;
         erRunDt    = *off;
         erTypOp    = *off;
         erProd     = *off;
         erAgt#     = *off;
         msLossR    = *Blanks;
         msPPR      = *Blanks;
         msPolEftDt = *Blanks;
         msPolExpDt = *Blanks;
         msPriSort  = *Blanks;
         msRptTyp   = *Blanks;
         msRunDt    = *Blanks;
         msTypOp    = *Blanks;
         msAgt#     = *Blanks;
         msProd     = *Blanks;

      /End-Free

     p ClrErr          e
       //===================================================================
       // LoadInVal - Intialize the Screen fields
       //===================================================================
     p LoadInVal       b
     d LoadInVal       pi

      /Free

         scLossR    = 'N';
         scPPR      = 'N';
         scOutP     = *Blanks;
         scPolEftDt = *Zeros;
         scPolExpDt = *Zeros;
         scAgt#     = *Zeros;
         scAgtName  = *Blanks;
         scProd     = *Zeros;
         scProdName = *Blanks;
         scRptTyp   = *Blanks;
         scSort     = *Blanks;
         scRpt      = *Blanks;
         scRunDt    = Dateto8(%date());

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

          // Display Screen
          Exsr ExfmtScreens;

          If selAgtSrch = *on;
             Chain (intCo#:intFnd) pfn00;
             inKc = ' ';
             inKl = ' ';
             inCallS = '2';
             inRrn = FnRrn;
             GetAgtNum ( inKc:inKl:inCallS:inRrn );
             If inRRN <> *Zeros;
                Chain inRrn PAA0p;
                If  %found(wmaap);
                   scAgt# = AAAGT#;
                   scAgtName = AANAME;
                   isFocusPr = *on;
                Else;
                   isFocusAg = *on;
                EndIf;
             EndIf;
          EndIf;

          If selPrdSrch = *on;
             //*** Beg Chg ***** 08/05/24 **********************************
             //***Chain (fnCo#:fnFnd:scAgt#) PAA02;
             //***If %found(WMAAL1);
             flgAgtFnd = *Off;
             Setll (FNco#:scAgt#) PAA05;
             Reade (FNco#:scAgt#) PAA05;
             Dow Not %Eof(WMAAL5);
                If scAgtName = AANAME;
                   flgAgtFnd = *On;
                   Leave;
                Endif;
                Reade (FNco#:scAgt#) PAA05;
             Enddo;
             If flgAgtFnd = *Off;
                Chain (FNco#:scAgt#) PAA05;
                scAgtName = AANAME;
             Endif;
             If %Found(WMAAL5) or flgAgtFnd = *On;
             //*** End Chg ***** 08/05/24 **********************************
                Chain (FNCO#:AAFEIN:AAMOD) pag00;
                inKc = '0';
                inKl = '0';
                inCallS = '2';
                inRrn = AgRrn;
                GetPrdNum ( inKc:inKl:inCallS:inRrn );
                If inRRN <> *Zeros;
                   Chain inRrn PAP0P;
                   If  %found (wmapp);
                      scProd = APPROD;
                      scProdName = APFSN + ',' + APLSN;
                      isFocusRp = *on;
                   Else;
                      isFocusPr = *on;
                   EndIf;
                EndIf;
             Else;
                erAgt# = *on;
                msAgt# = 'WCR0530';
             EndIf;
          EndIf;

          Select;
          When BtnSubmit = *on;
             // Clear Errors
             ClrErr();
             // Validate Edits
                ValidPnl();
                If isValid = *Off;
                   AgtLosProc();
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
             intCo#   = userCo#(q1user);
             intFnd   = userGrp(q1user);
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
          Write RcdTabAgLs;

          Exfmt RcdAgLsRpt;

          Read rcdComPnl;
          Read RcdTabAgLs;

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
       intFnd     = userGrp(q1user);
       scCompName = GetCompNam(intCo#);

       // Retrieve Output Queue of the User.
       Chain q1user pup00;
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

       scrpt = 'This Report will produce output file WFALP (Loss Ratio) +
         , WFPPP (PPR) in library '+ strrptl;

       tabClick = *Off;

       Return;

      /End-Free

     p ProgramStart    e
       //===================================================================
       // ValidPnl - Validate Panel Screen Fields
       //===================================================================
     p ValidPnl        b
     d ValidPnl        pi

      /Free
         //Date Validation
       If scPolEftDt = *Zeros;
          erPolEftDt = *On;
          msPolEftDt = 'WCR1391';
          isValid = *On;
       Endif;

       If scPolExpDt = *Zeros;
          erPolExpDt = *On;
          msPolExpDt = 'WCR1391';
          isValid = *On;
       Endif;

       If scPolEftDt <> *Zeros;
          erPolEftDt = ValidDate(scPolEftDt:strMsg);
          If erPolEftDt = *On;
             msPolEftDt = StrMsg;
             isValid = *On;
          Endif;
       Endif;

       If scPolExpDt <> *Zeros;
          erPolExpDt = ValidDate(scPolExpDt:strMsg);
          If erPolExpDt = *On;
             msPolExpDt = StrMsg;
             isValid = *On;
          Endif;
       Endif;

       //"From Date" cannot be greater than "To date"
       If erPolEftDt = *off and erPolExpDt = *off;
          erPolEftDt = AscendDates(scPolEftDt:scPolExpDt:strMsg);
          If erPolEftDt = *On;
             msPolEftDt = strMsg;
             erPolExpDt = *On;
             msPolExpDt = strMsg;
             isValid = *On;
          Endif;
       Endif;

         //Run Date Validation
       If scRunDt    = *Zeros;
          erRunDt    = *On;
          msRunDt    = 'WCR1391';
          isValid = *On;
       Endif;

       If scRunDt <> *Zeros;
          erRunDt = ValidDate(scRunDt:strMsg);
          If erRunDt = *On;
             msRunDt = StrMsg;
             isValid = *On;
          Endif;
       Endif;

       // Validation Select Report Flag.
         If scLossR = 'N' and scPPR <> 'Y';
            erPPR     = *On;
            msPPR     = 'WC40064';
            isValid = *On;
         EndIf;

       // Validate Agent #, if entered
       //*** Beg Del *** 09/26/18 *****************************************
       //***  If scAgt# <> *Zeros;
       //***     Chain (fnCo#:fnFnd:scAgt#) PAA02;
       //***     If not %found(wmaal1) or AAstat = 'Y';
       //***        erAgt#   = *On;
       //***        msAgt#   = 'WCR0530';
       //***        isValid = *On;
       //***     EndIf;
       //***  EndIf;
       //*** End Del *** 09/26/18 *****************************************
       //*** Beg Add *** 09/26/18 *****************************************
         If scAgt# <> *zeros;
            erAgt#   = *On;
            msAgt#   = 'WCR0530';
            isValid = *On;
            Setll *loval pgs01;
            Read pgs01;
            Dow Not %Eof(wxgsl);
               If GSieFl = 'I';
                  Chain (upco#:GSfnd:scAgt#) paa02;
                  If %Found(wmaal1) and AAstat <> 'I';
                     erAgt#   = *Off;
                     msAgt#   = *Blanks;
                     isValid = *Off;
                     Leave;
                  Endif;
               Endif;
               Read pgs01;
            Enddo;
         Endif;
       //*** End Add *** 09/26/18 *****************************************

       // Validate Producer #, if entered
         If scProd <> *Zeros;
            Chain (fnCo#:AAFein:AAmod:scProd) PAP00;
            If not %found(wmapl) or APstat = 'I';
               erProd   = *On;
               msProd   = 'WCR0531';
               isValid = *On;
            EndIf;
         EndIf;

       // Validate Report Type.
         If ScLossR  = 'Y' ;
            If scRptTyp = *Blanks;
               erRptTyp = *On;
               msRptTyp = 'WCR0503';
               isValid = *On;
            EndIf;
         EndIf;

       // Validate sort flag for producer performance select
         If scPPR = 'Y';
            If scSort    = *Blanks;
               erPriSort = *On;
               msPriSort = 'WCR0503';
               isValid = *On;
            EndIf;
         EndIf;

       // Validation Check for type of Output.
         If scOutP  = *Blanks;
            erTypOp = *On;
            msTypOp = 'WCR0503';
            isValid = *On;
         EndIf;

      /End-Free
     p ValidPnl        e
