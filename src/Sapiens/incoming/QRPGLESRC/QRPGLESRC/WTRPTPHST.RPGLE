     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     H
     H option(*nodebugio:*srcstmt)
     H BNDDIR('WTBNDDIR')
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE WTRPTPHST - RPROMPT FOR PAYMENT HISTORY REPORT
     H***********************************************************************
     H* REVISIONS:                                                         *
     H*                                                                    *
     H* 02/08/23 C:340 project 140WBY                                      *
     H*                Add Lost Time Indicator                             *
     H*                                                                    *
     H* 11/18/16 C:180 Support 44307                                       *
     H*                Fix To-Case number.                                 *
     H*                                                                    *
     H* 11/10/16 C:308 Phaseware 44178                                     *
     H*                Correct group in toolbar call                       *
     H*                                                                    *
     H* 06/23/16 C:298 Phaseware 42154                                     *
     H*                Added handling of Cancel Button.                    *
     H***********************************************************************
      *                                                                    *
     fWTRPTPHST cf   e             workstn extfile('WTRPTPHST')
     f                                     handler('PROFOUNDUI(HANDLER)')
     fWtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)
     fWmupl     if   e           k disk    Rename(pup01:pup02)
     fWmfnl     if   e           k disk    Rename(pfn01:pfn02)
     fWmcmp     if   e             disk    Rename(pcm01:pcm02)
     fWmeml     if   e           k disk    Rename(pem01:pem02)
     fWtabl     if   e           k disk
     fwmcml1    if   e           k disk    Rename(pcm01:pcm03)
     f                                     Infds(dbfds1)
     fWdf1l     if   e           k disk    Rename(pf101:pf102)
     fWdelp     if   e             disk    Rename(pel01:pel02)
     fWdell     if   e           k disk    Rename(pel01:pel03)

     dpsds           esds                  extname(szq1p)

     d dbfds1          ds
     d  dbrrn1               397    400b 0

     d outCltId        s              3
      ** Screen variables
     d
     d strOutq         s                   Like(Upoutq)
     d strMsg          s            100
     d fyrChoices      s            500
     d outRrn#         s              9
     d empRrn          s              9  0
     d isValidS        s               n   Inz(*off)
     d intCo#          s                   Like(ELco#)
     d intFnd          s                   Like(ELfnd)
     d intEmp#         s                   Like(ELemp#)
     d intDiv          s                   Like(ELdiv)
     d intFyr          s                   Like(ELfyr)
     d inSelect        s           1024
     d intCount        s              2  0
     d intPosst        s              2  0
     d intPosl         s              2  0
     d intPos          s              2  0
     d intPos1         s              2  0
     d intPos2         s              2  0
     d intStart        s              5  0 inz(1)
     d strRCdDsc       s             30    inz(*Blanks)
     d strPmtTpDsc     s             30    inz(*Blanks)
     d strPmtCdeDs     s             30    inz(*Blanks)
     d strExtVar       s             50    inz(*Blanks)
     d strrptl         s            200    inz(*Blanks)
      *
     d WtRpthstR       pr                  Extpgm('WTRPTPHST')
     d  inPrds                       40A
     d  inCall                        1A
     d  inRrn                         9A
     d
     d WtRpthstR       pi
     d  inPrds                       40A
     d  inCall                        1A
     d  inRrn                         9A

     d intclaimRrn     S              9  0
     d intEmpNbr       S                   like(EMEMP#)
     d intDivNbr       S                   like(EMDIV)
     d inCo#           S              3
     d inFnd           S              3
     d inFyr           S              3
     d inClm#Fr        S              7
     d in@Out          S              1
     d inDateFr        S              8
     d inDateTh        S              8
     d inPmtCode       S              2
     d inTranType      S              1
     d inReC1          S              2
     d inReC2          S              2
     d inReC3          S              2
     d inReC4          S              2
     d inReC5          S              2
     d inReC6          S              2
     d inReC7          S              2
     d inReC8          S              2
     d inReC9          S              2
     d inReC10         S              2
     d inReC11         S              2
     d inResvFlg1      S              1
     d inResvFlg2      S              1
     d inResvFlg3      S              1
     d inResvFlg4      S              1
     d inOutQ          S             10
     d inEmp#          S              9
     d inDiv#          S              5
     d inPmt1          S              2
     d inPmt2          S              2
     d inPmt3          S              2
     d inPmt4          S              2
     d inPmt5          S              2
     d inPmt6          S              2
     d inClm#To        S              7
     d inSortOrd       S              1
     d inYear          S              3  0
     d
     d*scOutType       S              1
     d inMode          S              1
     d inEnd           S              1
     d inStart         S              1
     d OutPmtType      S              2
     d inExit          S              1
     d inSort          S              2
     d inRrn1          S              9  0
     d outRrn1         S              9  0
      * Prototypes
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

      * Employer Search Proc.
     d EmployerSrch    pr

      * Employer Search
     d EmpSearch       pr                  ExtPgm('WTSEARCH')
     d inMode                         1
     d inCall                         1
     d outRrn#                        9
     d inSelect                            likeds(dsSrchCrit)

      * Payment Type Search
     d PmtSearch       pr                  ExtPgm('W2146R')
     d inCall                         1
     d inEnd                          1
     d inStart                        1
     d outPmtType                     2

      * Claim Number Search
     d ClaimNbrSrch    pr
      *
     d ClmSearch       pr                  ExtPgm('W0070R')
     d inCall                         1
     d inExit                         1
     d inEnd                          1
     d inMode                         1
     d OutRrn1                        9  0
     d inSort                         2
     d inRrn1                         9  0
     d inYear                         3  0

      * Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d outCltId                       3

      * Intialize the Screen fields
     d LoadInitVal     pr

      * Main Processing
     d Main            pr

      * Program Start
     d ProgramStart    pr

      * Set Criteria
     d SetCriteria     pr

      * Polpulate Payment code Dropdown Box.
     d PmtCdeLod       pr

      * Polpulate Reserve Code multi-select Box.
     d Rescvlod        pr

      * Polpulate Reserve Code multi-select Box.
     d PmtTypLod       pr

      * Validate Panel Sreen Values
     d ValidPnlPH      pr

     dSA030R           PR                  extpgm('SA030R')
     d outsufx                        3

      * Payment History Reort Procedure
     d PmtHisRptpr     pr

      * Payment History Reort Submit
     d PmtHisRptSbm    pr                  ExtPgm('W7060C')
     d inCo#                          3
     d inFnd                          3
     d inFyr                          3
     d inClm#Fr                       7
     d in@Out                         1
     d inDateFr                       8
     d inDateTh                       8
     d inPmtCode                      2
     d inTranType                     1
     d inReC1                         2
     d inReC2                         2
     d inReC3                         2
     d inReC4                         2
     d inReC5                         2
      *Extra codes added to screen
     d*inReC6                         2
     d*inReC7                         2
     d*inReC8                         2
     d*inReC9                         2
     d*inReC10                        2
     d inResvFlg1                     1
     d inResvFlg2                     1
     d inResvFlg3                     1
     d inResvFlg4                     1
     d inOutQ                        10
     d inEmp#                         9
     d inDiv#                         5
     d inPmt1                         2
      *Extra codes added in this screen
     d*inPmtT2                        2
     d*inPmtT3                        2
     d*inPmtT4                        2
     d*inPmtT5                        2
     d inClm#To                       7
     d inSortOrd                      1
     ***** Beg Add ***** 02/08/23 **************************************
     d inClmTyp                       1
     d inClmTyp        S              1
     ***** End Add ***** 02/08/23 **************************************
     d outsufx         s              3
      /Free
       // Start Of Program
       ProgramStart();

       // Main Procesing
       Main();

       *InLr = *on;

      /End-Free

       //===================================================================
       // Claim Number Search for the year
       //===================================================================
     p ClaimNbrSrch    b
     d ClaimNbrSrch    pi

      /Free
         inCall   = '2';
         inEnd    = '0';
         inMode   = *Blank;
         outRrn1  = *Zeros;
         inRrn1   = *Zeros;
         inYear   = scYear;
         ClmSearch(inCall:inexit:inEnd:inMode:outRrn1:inSort:inRrn1:inYear);
            if inEnd <> '1';
            If outRrn1 <> *zeros;
               Chain(n) outRrn1 pcm02;
                  If %found(Wmcmp);
                     scYear = CMfyr;
                        If selClmfrSh = *On;
                           scClmNbrFr = CMcase;
                        Else;
                           scClmNbrTo = CMcase;
                        EndIf;
                  EndIf;
            EndIf;
         EndIf;
      /End-Free

     p ClaimNbrSrch    e
       //===================================================================
       // EmployerSrch  - Employer/Div Search
       //===================================================================
     p EmployerSrch    b
     d EmployerSrch    pi

      /Free
       isEMPnbr = '0';
       inMode  = 'E';
       inCall  = '2';
       clear dsSrchCrit;
       clear inSelect;
       dsEmpGrp  = scGrpDsr;
       dsEmpName = scEmpName;
       inSelect  = dsSrchCrit;
       outRrn#   = '000000000';
       EmpSearch(inMode:inCall:outRrn#:inSelect);

       If outRrn# <> '000000000';
          empRrn=%Dec(outRrn#:9:0);
          Chain(n) empRrn pel02;
          If %Found(Wdelp);
             intCo#  = ELco#;
             intFnd  = ELfnd;
             intEmp# = ELEmp#;
             intDiv  = ELdiv;

             Chain(n)(intCo#:intFnd:intEmp#:intDiv) pem02;
             If %Found(Wmeml);
                scEmpNbr   = EMemp#;
                scEmpName  = EMname;
                scDivNbr   = EMdiv;
                scGrpDsr   = EMfnd;
                isEMPnbr = '1';
             EndIf;
          EndIf;

       EndIf;

      /End-Free

     p EmployerSrch    e
       //===================================================================
       // PmtHisRptPr      - Payment history report
       //===================================================================
     p PmtHisRptPr     b
     d PmtHisRptPr     pi

      /Free

       inCo# = %EditC(intCo#:'X');

       If intClaimRrn <> *zero;
          inFnd = scGrp;
       Else;
          inFnd = %EditC(scGrpDsr:'X');
       EndIf;

       If scYear <> *Zero;
          inFyr = %EditC(scYear:'X');
       Else;
          inFyr = *Blanks;
       EndIf;

       If scEmpNbr <> *Zero;
          inEmp# = %EditC(scEmpNbr:'X');
       Else;
          inEmp# = *Blanks;
       EndIf;

       If scDivNbr <> *Zero;
          inDiv#  = %EditC(scDivNbr:'X');
       Else;
          inDiv#  = *Blanks;
       EndIf;

       If scClmNbrFr <> *Zero;
          inClm#Fr = %EditC(scClmNbrFr:'X');
       Else;
          inClm#Fr = *Blanks;
       EndIf;

       //*** Begin Change *** 11/18/16 *****************************************
         //If scClmNbrTo <> *Zero;
         //   inClm#To = %EditC(scClmNbrTo:'X');
         //Else;
         //   inClm#To = '9999999';
         //EndIf;

       Select;
       When scClmNbrTo <> *Zero;
          inClm#To = %EditC(scClmNbrTo:'X');

       When isMenu = '0';
          inClm#To = inClm#Fr;

       Other;
          inClm#To = '9999999';
       Endsl;

       //*** End   Change *** 11/18/16 *****************************************

       If scFromDate <> *Zeros;
          inDateFr = %EditC(scFromDate:'X');
       Else;
          inDateFr = *Blanks;
       EndIf;

       If scThruDate <> *Zeros;
          inDateTh = %EditC(scThruDate:'X');
       Else;
          inDateTh = *Blanks;
       EndIf;

       If scTranType <> *Blanks;
          inTranType = scTranType;
       Else;
          inTranType = *Blanks;
       EndIf;

       If scpmtCode  <> *Blanks;
          inPmtCode = scPmtCode;
       Else;
          inPmtCode = *Blanks;
       EndIf;


       in@Out  = scOutType;
       inOutQ  = strOutQ;

       If scSortOrd = *Blanks and outCltId = '139';
          scSortOrd = 'C';
       Else;
          scSortOrd = 'T';
       EndIf;
       inSortOrd = scSortOrd;

        //    PmtHisRptSbm(inCo#:inFnd:inFyr:inClm#Fr:in@Out:inDateFr:
        //           inDateTh:inPmtCode:inTranType:inReC1:inReC2:
        //           inReC3:inReC4:inReC5:inReC6:inReC7:inReC8:
        //           inReC9:inReC10:inResvFlg1:inResvFlg2:inResvFlg3:
        //           inResvFlg4:inOutQ:inEmp#:inDiv#:inPmt1:inPmt2:
        //           inPmt3:inPmt4:inPmt5:inClm#To:inSortOrd);

       //***** Beg Chg ***** 02/08/23 **************************************
        //    PmtHisRptSbm(inCo#:inFnd:inFyr:inClm#Fr:in@Out:inDateFr:
        //             inDateTh:inPmtCode:inTranType:inReC1:inReC2:
        //             inReC3:inReC4:inReC5:inResvFlg1:inResvFlg2:
        //             inResvFlg3:inResvFlg4:inOutQ:inEmp#:inDiv#:inPmt1:
        //             inClm#To:inSortOrd);

       inClmTyp = scClmTyp;
       PmtHisRptSbm(inCo#:inFnd:inFyr:inClm#Fr:in@Out:inDateFr:
                inDateTh:inPmtCode:inTranType:inReC1:inReC2:
                inReC3:inReC4:inReC5:inResvFlg1:inResvFlg2:
                inResvFlg3:inResvFlg4:inOutQ:inEmp#:inDiv#:inPmt1:
                inClm#To:inSortOrd:inClmTyp);
       //***** End Chg ***** 02/08/23 **************************************
       //***** Beg Chg ***** 06/23/16 **************************************
       //*****ExitNow = *On;
       BtnCancel = *On;
       //***** End Chg ***** 06/23/16 **************************************

      /End-Free
     p PmtHisRptPr     e
       //===================================================================
       // LoadInitVal - Intialize the Screen fields
       //===================================================================
     p LoadInitVal     b
     d LoadInitVal     pi

      /Free
       isPop      = '0';
       isMenu     = '0';
       scYear     = 0;
       scClmNbrFr = 0;
       scClmNbrTo = 0;
       scEmpNbr   = 0;
       scDivNbr   = 0;
       scFromDate = 0;
       scThruDate = 0;
       scGrpDsr   = 0;
       scCompName = *Blanks;
       scEmpName  = *Blanks;
       scTranType = *Blanks;
       scResvCd1  = *Blanks;
       scResvFlg1 = *Blanks;
       scResvFlg2 = *Blanks;
       scResvFlg3 = *Blanks;
       scResvFlg4 = *Blanks;
       scPmtCode  = *Blanks;
       scPmtType1 = *Blanks;
       scSortOrd  = *Blanks;
       scOutType  = *Blanks;
       //***** Beg Add ***** 02/08/23 **************************************
       scClmTyp   = *Blanks;
       //***** End Add ***** 02/08/23 **************************************
       inReC1     = *Blanks;
       inReC2     = *Blanks;
       inReC3     = *Blanks;
       inReC4     = *Blanks;
       inReC5     = *Blanks;
       inReC6     = *Blanks;
       inReC7     = *Blanks;
       inReC8     = *Blanks;
       inReC9     = *Blanks;
       inReC10    = *Blanks;
       inReC11    = *Blanks;
       inPmt1     = *Blanks;
       inPmt2     = *Blanks;
       inPmt3     = *Blanks;
       inPmt4     = *Blanks;
       inPmt5     = *Blanks;
       inPmt6     = *Blanks;
       Sa030r(outSufx);
       If outSufx = *blanks;
          strrptl = 'in library PDSRPT' + ', if selected.';
       Else;
          strrptl = 'in library PDSRPT' + outSufx + ', if selected.';
       EndIf;
       scrptl = 'This report will produce output file WFPHP '+
                   strrptl;
      /End-Free
     p LoadInitVal     e

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

      /Free

          SetCriteria();

          //scGrpDsr = userGrp(q1user);
          scGrpDsr = UPfnd;
          // To load Payment Code values.
          PmtCdeLod();

          // To load Reserve code values.
          Rescvlod();

          // To load Payment Values.
          PmtTypLod();
       // Process until exit button is pressed.
       //***** Beg Chg ***** 06/23/16 **************************************
       //*****Dou ExitNow = *On ;
       Dou BtnCancel = *On ;
       //***** End Chg ***** 06/23/16 **************************************


          // Display Screen
          Exsr ExfmtScreens;

          Select;
             When selClmFrSh = *On or selClmToSh = *On;
                ClaimNbrSrch();
                selClmFrSh = *Off;
                selClmToSh = *Off;

             When selEmpSrch = *On and isMenu = '1';
                EmployerSrch();


             When BtnSubmit = *On;
                ValidPnlPH();
                If isValidS = *Off;
                   PmtHisRptpr();
                EndIf;

             //Do nothing - catch tab click event
             When tabClick = *on;
                tabClick = *off;

             When BtnRefresh = *On;
                BtnRefresh =*Off;
                ProgramStart();
                SetCriteria();
                scGrpDsr = UPfnd;
          // To load Payment Code values.
                PmtCdeLod();

          // To load Reserve code values.
                Rescvlod();

          // To load Payment Values.
                PmtTypLod();

          EndSl;

       Enddo;
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       Select;

       When TabIndex = 0;
          TabHeight =  710;
          APnlHeight = 770;

          If inPrds = 'PaymentHistory';
             Exfmt RcdRpthst;
          Else;
          Write rcdComPnl;
          Write RcdTabPmtR;
          Exfmt RcdRptPhst;
          Read rcdComPnl;
          Read RcdTabPmtR;
          EndIf;




       EndSl;
       erGrpNbr   = *off;
       erYear     = *off;
       erClmNbrFr = *off;
       erClmNbrTo = *off;
       erEmpNbr   = *off;
       erYear     = *off;
       erFromDate = *off;
       erThruDate = *off;
       erOutType  = *off;
       msGrpNbr   = *Blanks;
       msYear     = *Blanks;
       msClmNbrFr = *Blanks;
       msClmNbrTo = *Blanks;
       msEmpNbr   = *Blanks;
       msYear     = *Blanks;
       msFromDate = *Blanks;
       msThruDate = *Blanks;
       msOutType  = *Blanks;
       isValidS   = *Off;
       inReC1     = *Blanks;
       inReC2     = *Blanks;
       inReC3     = *Blanks;
       inReC4     = *Blanks;
       inReC5     = *Blanks;
       inReC6     = *Blanks;
       inReC7     = *Blanks;
       inReC8     = *Blanks;
       inReC9     = *Blanks;
       inReC10    = *Blanks;
       inReC11    = *Blanks;
       inPmt1     = *Blanks;
       inPmt2     = *Blanks;
       inPmt3     = *Blanks;
       inPmt4     = *Blanks;
       inPmt5     = *Blanks;
       inPmt6     = *Blanks;

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

       If q1prms >= 3;
          Eval isPop  = '1';
          Eval isMenu = '0';
          intClaimRrn=%Dec(inRrn:9:0);
       Else;
          intClaimRrn=0;
       EndIf;

       // Load Action panel with the Company Name.
       intCo# = userCo#(q1user);
       scCompName = GetCompNam(intCo#);

        // Retrieve Output Queue of the User.
       Chain(n) q1user pup02;
       If %Found(Wmupl);
          strOutq = UPoutq;
          scYear  = UPfYr;
       EndIf;

       // Get the client id
       GetClientID(outCltId);
       If outCltId = '139';
          scSortOrd = 'C';
       Else;
          scSortOrd = 'T';
       EndIf;

       If intClaimRrn <> 0;
          Chain(n) intClaimRrn pcm02;
          If %Found(Wmcmp);
             // Employer,Division, claim, Year from WMCMP file
             scClmNbrFr = CMcase;
             scYear = CMfYr;
             scEmpNbr = CMemp#;
             isEMPnbr = '1';
             scDivNbr = CMDiv;
             Chain(n) (CMco#:CMfnd) pfn02;
             If %Found(Wmfnl) ;
         //*** Beg Chg ***** 11/10/16 *****************************
            //  scGrp = %Char(FNfnd) + '-' + FNname;
                scGrp = %EditC(FNfnd:'X') + '-' + FNname;
         //*** End Chg ***** 11/10/16 *****************************
             EndIf;
             Chain(n) (CMco#:CMfnd:CMemp#:CMdiv) pem02;
             If %Found(WMEML) ;
                scEmpName = EMname;
             EndIf;
          EndIf;
       ELse;
          isPop  = '0';
          isMenu = '1';
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
       // ValidPnlPH - Validate Panel 1 Screen Fields
       //===================================================================
     p ValidPnlPH      b
     d ValidPnlPH      pi

      /Free

       If (scOutType <> 'F' and scOutType <> 'P' and  scOutType <> 'B');
          erOutType = *On;
          msOutType = 'WCR0503';
          isValidS  = *On;
       Endif;

       //If isPop = '0' and isValidS  = *Off;
       If isPop = '0';

          If scGrpDsr = *zeros;
             erGrpNbr =  *On;
             msGrpNbr = 'WCR0503';
             isValidS = *On;
          Endif;

          If scYear <> *Zeros;
             Chain(n) (UPco#:scGrpDsr:scYear)pf102;
             If not %found(Wdf1l);
                erYear = *On;
                msYear = 'WCR0517';
                isvalidS  = *On;
             EndIf;

             If scClmNbrFr = *zeros and scEmpNbr = *zero;
                erEmpNbr   = *On;
                msEmpNbr   = 'WCR0503';
                isValidS   = *on;
             EndIf;
          ElseIf scClmNbrTo <> *zeros;
             erClmNbrFr = *On;
             msClmNbrFr = 'WCR1179';
             isValidS   = *on;
          Endif;

          If scClmNbrFr > *zeros;
             Chain(n) (UPco#:scGrpDsr:scYear:scClmNbrFr) pcm03;
             If not %found(wmcml1);
                erClmNbrFr = *On;
                msClmNbrFr = 'WCR0505';
                isValidS   = *on;
             Endif;

             If scClmNbrTo > *zero and scClmNbrTo < 9999999;
                Chain(n) (UPco#:scGrpDsr:scYear:scClmNbrTo) pcm03;
                If not %found(wmcml1);
                   erClmNbrTo = *On;
                   msClmNbrTo = 'WCR0505';
                   isValidS   = *on;
                Endif;
             Endif;
             If scClmNbrTo <> *zero and scClmNbrTo < scClmNbrFr;
                erClmNbrTo   = *On;
                msClmNbrTo   = 'WCR0807';
                isValidS     = *on;
             Endif;
             If scClmNbrTo =  *zeros;
                erClmNbrTo = *On;
                msClmNbrTo = 'WCR0502';
                isValidS   = *on;
             Endif;
          Endif;

          If scEmpNbr > *zeros;
             Chain(n) (UPco#:scGrpDsr:scEmpNbr:scDivNbr) pem02;
             If not %Found(wmeml);
                erEmpNbr  = *On;
                msEmpNbr  = 'WCR0538';
                isValidS  = *on;
             EndIf;

             If scYear <> *zeros;
                Chain(n) (UPco#:scGrpDsr:scEmpNbr:scDivNbr:scYear) pel03;
                If not %Found(wdell);
                   erYear = *On;
                   msYear = 'WCR0694';
                   isValidS = *on;
                EndIf;
             Endif;
          EndIf;

       EndIf;

         //Date Validation
       If scFromDate <> *Zeros;
          erFromDate = ValidDate(scFromDate:strMsg);
          If erFromDate = *On;
             msFromDate = 'WCR0500';
             isValidS   = *on;
          Endif;
       Endif;

       If scThruDate <> *Zeros;
          erThruDate = ValidDate(scThruDate:strMsg);
          If erThruDate = *On;
             msThruDate = 'WCR0500';
             isValidS   = *on;
          Endif;
       Endif;

       If scFromDate <> *Zeros and scThruDate <> *Zeros;
          If erThruDate = *off and erFromDate = *off and isValidS  = *Off;
             erThruDate = AscendDates(scFromDate:scThruDate:strMsg);
             If erThruDate = *On;
                erFromDate = *On;
                msThruDate = 'WCR0529';
                msFromDate = 'WCR0529';
                isValidS   = *on;
             Endif;
          Endif;
       Endif;

         //Reserve Codes validation
       If scResvCd1 <> *Blanks;
          intPos = %Scan(',':scResvCd1);
          If intPos = 01;
         //  intPos = %Scan(',':scResvCd1:2);
             inReC1 = %SubSt(scResvCd1:2:2);
          If inReC1 <> *Blanks;
             erResvCd = '1';
             msResvCd = 'WCR4820';
             isValidS   = *on;
          EndIf;
          EndIf;
          If intPos <> 01;
             inReC1 = %SubSt(scResvCd1:1:2);
             If intPos = 03;
                intPos = intPos + 1;
             Else;
                intPos = intPos + 3;
             EndIf;
             inReC2 = %SubSt(scResvCd1:intPos:2);
             If inReC2  <> *Blanks;
                intPos = %Scan(',':scResvCd1:6);
                If intPos = 06;
                   intPos = intPos + 1;
                   inReC3 = %SubSt(scResvCd1:intPos:2);
                   If inReC3  <> *Blanks;
                      intPos = %Scan(',':scResvCd1:9);
                      intPos = intPos + 1;
                      inReC4 = %SubSt(scResvCd1:intPos:2);
                      If inReC4  <> *Blanks;
                         intPos = %Scan(',':scResvCd1:12);
                         intPos = intPos + 1;
                         inReC5 = %SubSt(scResvCd1:intPos:2);
                         If inReC5  <> *Blanks;
                            intPos = %Scan(',':scResvCd1:15);
                            intPos = intPos + 1;
                            inReC6 = %SubSt(scResvCd1:intPos:2);
                            If inReC6  <> *Blanks;
                               intPos = %Scan(',':scResvCd1:18);
                               intPos = intPos + 1;
                               inReC7 = %SubSt(scResvCd1:intPos:2);
                               If inReC7  <> *Blanks;
                                  intPos = %Scan(',':scResvCd1:21);
                                  intPos = intPos + 1;
                                  inReC8 = %SubSt(scResvCd1:intPos:2);
                                  If inReC8  <> *Blanks;
                                     intPos = %Scan(',':scResvCd1:24);
                                     intPos = intPos + 1;
                                     inReC9 = %SubSt(scResvCd1:intPos:2);
                                     If inReC9  <> *Blanks;
                                        intPos = %Scan(',':scResvCd1:27);
                                        intPos = intPos + 1;
                                        inReC10 = %SubSt(scResvCd1:intPos:2);
                                        If inReC10 <> *Blanks;
                                           intPos = %Scan(',':scResvCd1:intPos);
                                           If intPos <> 0;
                                              erRsv10 = '1';
                                              msRsv10 = 'WCR4818';
                                              isValidS   = *on;
                                           EndIf;
                                        EndIf;
                                     EndIf;
                                  EndIf;
                               EndIf;
                            EndIf;
                         EndIf;
                      EndIf;
                   EndIf;
                EndIf;
             EndIf;
          EndIf;
       EndIf;

         //Payment Types Validation

       If scpmtType1 <> *Blanks;
          intPos = %Scan(',':scpmtType1);
          If intPos = 1;
           //intPos = %Scan(',':scpmtType1:2);
             inPmt1 = %SubSt(scpmtType1:2:2);
             If inPmt1 <> *Blanks;
                erPmtTp1 = '1';
                msPmtTp1 = 'WCR4820';
                isValidS   = *on;
             EndIf;
          EndIf;
          If intPos <> 01;
             inPmt1 = %SubSt(scpmtType1:1:2);
             If intPos = 03;
                intPos = intPos + 1;
             Else;
                intPos = intPos + 3;
             EndIf;
             inPmt2 = %SubSt(scpmtType1:intPos:2);
             If inPmt2  <> *Blanks;
                intPos = %Scan(',':scpmtType1:6);
                If intPos = 06;
                   intPos = intPos + 1;
                   inPmt3 = %SubSt(scpmtType1:intPos:2);
                   If inPmt3  <> *Blanks;
                      intPos = %Scan(',':scpmtType1:9);
                      intPos = intPos + 1;
                      inPmt4 = %SubSt(scpmtType1:intPos:2);
                      If inPmt4  <> *Blanks;
                         intPos = %Scan(',':scpmtType1:12);
                         intPos = intPos + 1;
                         inPmt5 = %SubSt(scpmtType1:intPos:2);
                         If inPmt5  <> *Blanks;
                            intPos = %Scan(',':scpmtType1:intPos);
                            If intPos <> 00;
                               erPmtTp5 = '1';
                               msPmtTp5 = 'WCR4819';
                               isValidS   = *on;
                            EndIf;
                         EndIf;
                      EndIf;
                   EndIf;
                EndIf;
             EndIf;
          EndIf;
       EndIf;

      /End-Free

     p ValidPnlPH      e
       //===================================================================
       // ResvCodLod - Load reserve code multiselect box.
       //===================================================================
     p Rescvlod        b
      /Free

       scRCodSel = 'All';
       scRCodVal = *Blanks;
       strRCdDsc = *Blanks;

       // Read WTRTP file from starting
       Setll intCo# prt01;
       If %Equal(wtabl);
          ReadE intCo# prt01;
          Dow not %Eof(wtabl);
             If RTstat = 'A';
                strRCdDsc = TitleCase(RTdesc);
                scRCodSel = %trim(scRCodSel)+','+%trim(RTkey)+'-'+
                             %trim(strRCdDsc);
                scRCodVal = %trim(scRCodVal)+','+%trim(RTkey);
             endif;
          ReadE intCo# prt01;
          enddo;
       endif;

      /End-Free
     p Rescvlod        e
       //===================================================================
       // PmtTypLod - Load Payment type to multiselect box.
       //===================================================================
     p PmtTypLod       b
      /Free

       scPmtTpSel = 'All';
       scPmtTpVal = *Blanks;
       strPmtTpDsc = *Blanks;

       // Read WTRTP file from starting
       Setll intCo# pb201;
       If %Equal(wtabl);
          ReadE intCo# pb201;
          Dow not %Eof(wtabl);
             If B2stat = 'A';
                strPmtTpDsc = TitleCase(B2desc);
                scPmtTpSel = %trim(scPmtTpSel)+','+%trim(B2key)+'-'+
                             %trim(strPmtTpDsc);
                scPmtTpVal = %trim(scPmtTpVal)+','+%trim(B2key);
             endif;
          ReadE intCo# pb201;
          enddo;
       endif;

      /End-Free
     p PmtTypLod       e
       //===================================================================
       // PmtCdeLod - Load Payment Code to Dropdown    box.
       //===================================================================
     p PmtCdeLod       b
      /Free

       scPmtCdeSl = 'All';
       scPmtCdeVl = *Blanks;
       strPmtCdeDs = *Blanks;

       // Read WTPCP file from starting
       Setll intCo# ppc01;
       If %Equal(wtabl);
          ReadE intCo# ppc01;
          Dow not %Eof(wtabl);
             If PCstat = 'A';
                strPmtCdeDs = TitleCase(PCdesc);
                scPmtCdeSl = %trim(scPmtCdeSl)+','+%trim(PCkey)+'-'+
                             %trim(strPmtCdeDs);
                scPmtCdeVl = %trim(scPmtCdeVl)+','+%trim(PCkey);
             endif;
          ReadE intCo# ppc01;
          enddo;
       endif;

      /End-Free
     p PmtCdeLod       e
