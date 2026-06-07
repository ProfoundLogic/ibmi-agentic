     hcopyright('(c) 2025 Sapiens, Inc. All rights reserved.')

      //*********************************************************************
     h/title WTAGTRTFND - Agency Return Fund Checks
     h dftname(WTAGTRTFND)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h ALWNULL(*UsrCtl)
     h/define   profoundUI
     h bnddir('STBNDDIR')
     h bnddir('WTGRPCFG')
      //*********************************************************************
      //==============================================================*
      //  Created   : 12/31/21                                         *
      //  Programmer: C:300                                            *
      //  Project   : WC2395- Redesign of Agency Configuration program *
      //===============================================================*
      //  Revisions:                                                   *
      //                                                               *
      //  08/26/25 C:300 Support: 117250                               *
      //                 Return Funds Grid is showing same Return fund *
      //                 records for all Agents                        *
      //                 Fix: Add missing filter conditions from old   *
      //                      Return Funds screen (W0505R)             *
      //                                                               *
      //  11/28/23 C:300 Project: WC2395                               *
      //                 Correct Title of PopUp Window                 *
      //                                                               *
      //  11/17/23 C:300 Support: 113191 (WC2395)                      *
      //                 DSPF: Change Export to Excel File Name to     *
      //                 AR Transaction Detail                         *
      //                                                               *
      //  08/17/23 C:300 Support: 113188                               *
      //                 On 'Return Funds' Grid, Set default filter to *
      //                 'Agent' Payee Type records, since it is being *
      //                 accessed from the Agency Configuration screen *
      //                                                               *
      // ------------------------------------------------------------- *
      //  NOTE: Please add comments to the top of Revisions.           *
      // ------------------------------------------------------------- *
      //===============================================================*

     fwtagtrtfndcf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflRtnFund:rrnRtnFund)
     f                                     Sfile(sflTrnDtl:rrnTrDtl)
     f                                     Sfile(sflCurStm:rrnCurStm)
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdAgtPnl)
      * Company file
     fsmcol     if   e           k disk    rename(pco01:pco00)
      * Agent Master file
     fwmagp     if   e             disk    rename(pag01:pag0p)
      * Return Premium Check Request File
     fwar4p     if   e             disk    rename(pr401:pr40p)
     fwar4l6    if   e           k disk    rename(pr401:pr406)
     f                                     infds(dbfds)
      * Employers Accounts Receivable File
     fwmeal31   if   e           k disk    rename(pea01:pea31)
     f                                     infds(dbfds2)
      * Accounts Current Summary File
     fwsasl     if   e           k disk    rename(pas01:pas00)
      * Agency Billing Detail File (or Accounts Current Detail File)
     fwdabl     if   e           k disk    rename(pab01:pab00)
     f                                     infds(dbfds3)
      *
     FWTABL     IF   E           K DISK    ignore(PRT01)


     d psds          esds                  extname(szq1p)
     d dbfds           ds
     d  dbrrn                397    400b 0
     d dbfds2          ds
     d  dbrrn2               397    400b 0
     d dbfds3          ds
     d  dbrrn3               397    400b 0

      // work variables
     d intCo#          s              3  0
     d xxRrn           s              9  0 inz
     d indexTab        s              5  0
     d chgMod          s               n   inz(*off)
     d dspMode         s               n   inz(*off)
     d process         s               n   inz(*off)
     d reload          s               n   inz(*off)
     d infein          s              9  0 inz
     d inco#           s              3  0 inz
     d inmod           s              5  0 inz
     d meetSS1         s              1
     d @r4Cust         s                   like(R4cust)
     d @ssCus1         s                   like(R4cust)
     d @ssCus2         s                   like(R4cust)
     d ssFDat7         s                   Like(R4ckdt)
     d ssTDat7         s                   Like(R4ckdt)
     d ssFnd           s                   Like(R4fnd)
     d @@co#           s              3  0 inz
     d @@fnd           s              3  0 inz
     d @@emp#          s              9  0 inz
     d @@div           s              5  0 inz
     d @@fyr           s              3  0 inz
     d @@agt#          s              5  0 inz
     d @@anam          s             60    inz
     d u1pdsc          s             40
     d u2pdsc          s             40
     d str9999Msg      s            256
     d flgPaidBil      s              1
     d typeAmount2     s             11  2
     d xxKey           s             10
     d xxBalanceDt     s              7  0
     d svrrnRtnFund    s              7  0
     d sstnd           s                   Like(R4fnd)
     d co#_sav         s                   Like(R4co#)
     d fnd_sav         s                   Like(R4fnd)
     d fyr_sav         s                   Like(R4fyr)
     d emp#_sav        s                   Like(R4emp#)
     d agt#_sav        s                   Like(R4agt#)
     d stdt_sav        s                   Like(R4stdt)
     d*
     d dsRtnFilter     ds                  likerec(rcdRtnFltr)
      //

      // Proto-types
     d AR_value        pr             9  2
     d ar_Amount                      9  2  const
     d ar_Type                        1     const
     d outPaidBil                     1     options(*nopass)

      // Get Agent Name
     d GetAgtName      pr                  extpgm('WA002R')
     d @@co#                          3  0
     d @@fnd                          3  0
     d @@emp#                         9  0
     d @@div                          5  0
     d @@fyr                          3  0
     d @@agt#                         5  0
     d @@anam                        60

      // Load Account Current Statement Grid
     d LoadCurStmGrid  pr

      // Load Return Funds Grid
     d LoadReturnFund  pr

      // Load A/R Transaction Detail Grid
     d LoadTrDtlGrid   pr

      * Main Processing
     d Main            pr

      * Process the Row Click in Return Funds Grid
     d ProcRoWClk      pr

      * Program Start
     d ProgramStart    pr
     d processMe                       n

      * On/Off Subfile Display and Display Control Indicators for rcdRtnFnd
     d RcdRtnFndDsp    pr
     d  indValue                      1n   const

     d RtnFndFilter    pr


     d/copy sprUsrPrms
     d/copy sPrGenPop
     d/copy sPrDate
     d/copy sprCompany
     d/copy wPrPolErr
     d/copy sPrString
     d/copy sCopyGrpPr
     

     c     keyTable      klist
     c                   kfld                    scco#
     c                   kfld                    xxKey

     c     key1WMEAL30   klist
     c                   kfld                    co#_sav
     c                   kfld                    fnd_sav
     c                   kfld                    fyr_sav
     c                   kfld                    emp#_sav

     c     asky          klist
     c                   kfld                    scco#
     c                   kfld                    stdt_sav
     c                   kfld                    agt#_sav
     
      // Entry of Program.

     c     *entry        plist
     c                   parm                    @@rrn             9            rrn
     c                   parm                    @@mode            1            mod
     c                   parm                    @@next           10            next pgm
     c                   parm                    @@TabInd          5            tab index

      /free
       Exec Sql Set Option Commit = *None;

       If %parms >= 1;
          xxRrn = %int(@@rrn);
       Endif;

       Chain xxRrn pag0p;
       inCo# = AGco#;
       inFein = AGfein;
       inMod = AGmod;

       If (@@next = *Blanks or @@next = 'WTAGTRTFND');

          ProgramStart(process);

          //Initialize the Filter fields
          Clear dsRtnFilter;
          Clear rcdRtnFltr;

          //*** Begin Add ***** 08/17/23 *****************************
          // Set Payee Type filter to 'Agent' by default
          ssPtFl = 'A';
          dsRtnFilter.ssPtFl  = ssPtFl;
          //*** End   Add ***** 08/17/23 *****************************

          str9999msg = 'Your request returned too many records. ' +
           'Click Continue to see the first 9999 records. ' +
           'Click Filter on the display screen to narrow your results.';

          If process;

             Main();

          Endif;
       Endif;


       *InLr = *On;
      /end-free

       //==================================================================
        //AR_value - Determine if A/R amount is positive or negative
       //==================================================================
     P AR_value        b
     
     d AR_value        PI             9  2
     d ar_Amount                      9  2  const
     d ar_Type                        1     const
     d outPaidBil                     1     options(*nopass)

     d amount          s              9  2

      /free

       Select;
       When ar_Type = 'P' or ar_Type = 'Q' or
         ar_Type = 'I' or ar_Type = 'T';
         amount = ar_Amount * -1;
         If %parms >=3;
           outPaidBil = 'P';
         Endif;
       When ar_Type = 'R';
         amount = ar_Amount;
         If %parms >=3;
           outPaidBil = 'P';
         Endif;
       Other;
         amount = ar_Amount;
         If %parms >=3;
           outPaidBil = 'B';
         Endif;
       Endsl;

       Return amount;

      /end-free
     P AR_value        e

       //===================================================================
       // Load Account Current Statement Grid
       //===================================================================
       //===================================================================
     P LoadCurStmGrid  B
     D LoadCurStmGrid  PI

      /free

       // Clear the subfile.
       clrCurStm = *On;
       RcdRtnFndDsp(*off);
       Write rcdRtnFnd;
       clrCurStm = *Off;
       RcdRtnFndDsp(*on);
       rrnCurStm = 0;

       Clear scAgt#;
       Clear scAnam;
       Clear scStdt;
       Clear scDue;
       Clear scBill;
       Clear scPaid;
       Clear scBal;

       Chain asky pas00;
       If %found(wsasl);

          // Load Header fields from wsasl
          scAgt# = ASagt#;
          GetAgtName(R4co#:R4fnd:@@emp#:@@div:@@fyr:ASagt#:@@anam);
          scAnam = @@anam;
          scStdt = dt6MDYY(ASstdt);
          scDue  = dtMDYY(ASdue);
          scBill = ASbill;
          scPaid = ASpaid;
          scBal  = ASbal;

          Setll asky pab00;
          Reade asky pab00;
          Dow not %eof(wdabl);
             scARcode   = ABarcd;
             scARTranDt = CvtToDate(ABtrdt);
             scARPostDt = CvtToDate(ABpodt);

             scAmount  = ABamt;
             scCommRate  = ABrate;
             scCommAmt  = ABcomm;
             scNetAmt  = ABnet;

             scManEnt = *Blanks;
             If ABmadj = 'Y';
                scManEnt = 'Yes';
             Endif;

             scCurStRRN = dbrrn3;

             rrnCurStm = rrnCurStm + 1;
             If rrnCurStm = 1;
                scCurStSel = *on;
             Else;
                clear scCurStSel;
             Endif;

             Select;
             When rrnCurStm > 9999;
               //*** Begin Chg ***** 11/28/23 *****************************
               //*** PopOneBtn(str9999msg:'Message-Transaction Detail':'Continue');
               PopOneBtn(str9999msg:'Message-Account Current State-
                ment':'Continue');
               //*** End   Chg ***** 11/28/23 *****************************
                Leave;
             Endsl;

             Write sflCurStm;

             Reade asky pab00;
          Enddo;
          rrnCurStm = 1;


          xxBalanceDt = DateTo7(SystemDate);
          sccurr = scTdBalanc;
          scbal_to = '  Balances through: ' +
           %trim(alphaUSA(cvtToDate(xxBalanceDt)));

       Endif;


      /end-free
     P LoadCurStmGrid  E

       //===================================================================
       // Load Return Fund
       //===================================================================
     P LoadReturnFund  B
     D LoadReturnFund  PI
      /free

       // Clear the subfile.
       clrRtnFund = *On;
       RcdRtnFndDsp(*off);
       Write rcdRtnFnd;
       clrRtnFund = *Off;
       RcdRtnFndDsp(*on);
       rrnRtnFund = 0;
       scRowSel = 1;
       emp#_sav = 0;


       Setll scCO# PR406;
       Reade scCO# PR406;

       DoW not %eof;
          meetSS1  = *on;
          RtnFndFilter();
          If meetss1 = *on;
             scRFName   = R4ptnm;
             scRFCkDt   = CvtToDate(r4ckdt);
             scRFChk#   = R4chk#;
             scRFBnKAct = R4acct;
             scRFAmount = R4amt;

             Select;
             When R4BTYP = 'A';
                scRFCkType = 'Account Current';
             When R4BTYP = 'D';
                scRFCkType = 'Deposit';
             When R4BTYP = 'P';
                scRFCkType = 'Premium';
             When R4BTYP = 'I';
                scRFCkType = 'Independent Cash Distribution';
             When R4BTYP = ' ';
                scRFCkType = ' ';
             Endsl;

             Select;
             When R4vdsp = 'V';
                scRFVoSt = 'Void';
             When R4vdsp = 'S';
                scRFVoSt = 'Stop';
             When R4vdsp = ' ';
                scRFVoSt = ' ';
             Endsl;

             scRtFndrrn = dbrrn;
             rrnRtnFund = rrnRtnFund + 1;
             scRtnFSel = *off;

             // set the selection on 1st record for first load
             If svrrnRtnFund = *zeros;
                If rrnRtnFund = 1;
                   scRtnFSel = *on;
                Endif;
             Else;
                If svrrnRtnFund = rrnRtnFund;
                   scRtnFSel = *on;
                Endif;
             Endif;

             Write sflRtnFund;

       // Save 1st record information to load in A/R Transaction details Grid
             If emp#_sav = 0;
                co#_sav = R4co#;
                fnd_sav = R4fnd;
                fyr_sav = R4fyr;
                emp#_sav = R4emp#;
                agt#_sav = R4agt#;
                stdt_sav = R4stdt;
             Endif;

          Endif;
          Reade scCo# PR406;
       Enddo;

       rrnRtnFund = 1;
       If svrrnRtnFund <> 0;
          rrnRtnFund = svrrnRtnFund;
          scRowSel = svrrnRtnFund;
          svrrnRtnFund = 0;
       Endif;

      /end-free
     P LoadReturnFund  E

       //===================================================================
       // Load A/R Transaction Detail Grid
       //===================================================================
       //===================================================================
     P LoadTrDtlGrid   B
     D LoadTrDtlGrid   PI

      /free

       // Clear the subfile.
       clrTrDtl = *On;
       RcdRtnFndDsp(*off);
       Write rcdRtnFnd;
       clrTrDtl = *Off;
       RcdRtnFndDsp(*on);
       rrnTrDtl = 0;

       Setll key1WMEAL30 pea31;
       Reade key1WMEAL30 pea31;
       Dow not %eof(WMEAL31);
       // If EAFyr = R4fyr;
                scTdPostDt = CvtToDate(EApodt);
                scTdcode   = EAarcd;
                scTdtype   = EAarty;

                Select;
                When  EAarty = 'B' or EAarty = 'F'
                   or EAarty = 'H' or EAarty = 'A'
                   or EAarty = 'D';
                   scTdchk# = *blanks;
                Other;
                   If EAchk# = 0;
                      scTdchk# = '0';
                   Else;
                      scTdchk# = %char(Eachk#);
                   Endif;
                Endsl;

                typeAmount2 = AR_value(EAAMT:EAARTY:
                              flgPaidBil);
                scTdAmount  = typeAmount2;

                scTdDesc = EADesc;

                If EActyp = ' ' and EAarcd <> ' ';
                   xxKey = EAarcd;
                   Chain keyTable ptt01;
                   If %found;
                      EActyp = TTctyp;
                   Endif;
                Endif;

                scTdBalanc  = scTdBalanc + scTdAmount;

                scTrDtlRRN = dbrrn2;

                rrnTrDtl = rrnTrDtl + 1;
                If rrnTrDtl = 1;
                   scTrDtlSel = *on;
                Else;
                   clear scTrDtlSel;
                Endif;

                Select;
                When rrnTrDtl > 9999;
                  PopOneBtn(str9999msg:'Message-Transaction Detail':'Continue');
                   Leave;
                Endsl;

                Write sflTrnDtl;

       // Endif;

          Reade key1WMEAL30 pea31;
       Enddo;
       rrnTrDtl = 1;


       xxBalanceDt = DateTo7(SystemDate);
       sccurr = scTdBalanc;
       scbal_to = '  Balances through: ' +
        %trim(alphaUSA(cvtToDate(xxBalanceDt)));


      /end-free
     P LoadTrDtlGrid   E

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi
     d*
      /free

       BtnCancel = *Off;
       reload = *on;

       // Process until exit button is pressed.
       DoW BtnCancel = *Off;

          // display screen
          Exsr ExfmtScreens;

          // Perform actions on screen.
          Select;
          // do nothing - catch tab click event
          When TabClick = *on;
             TabClick = *off;

          // Catch Row click event
          When RowClick = *on;
             RowClick = *off;
             ProcRoWClk();           // Process the Row Click in Return Funds Grid
             reload = *off;

          // Refresh
          When btnRefresh = *On;
             btnRefresh = *Off;

          // Return Funds Filter Processing.
          When BtnRtnFltr = *on;
             DoU BtnRtnAppf = *On or BtnRtnCan = *On;
                ExFmt rcdRtnFltr;
                If BtnRtnAppf = *On;
                   dsRtnFilter.ssAcct  = ssAcct;
                   dsRtnFilter.ssPtFl  = ssPtFl;
                   dsRtnFilter.ssBTyp  = ssBTyp;
                   dsRtnFilter.ssPtNm  = ssPtNm;
                   dsRtnFilter.ssFAmt  = ssFAmt;
                   dsRtnFilter.ssTAmt  = ssTAmt;
                   dsRtnFilter.ssFDat  = ssFDat;
                   dsRtnFilter.ssTDat  = ssTDat;
                   dsRtnFilter.ssCus1  = ssCus1;
                   dsRtnFilter.ssCus2  = ssCus2;
                   dsRtnFilter.ssAgt1  = ssAgt1;
                   dsRtnFilter.ssAgt2  = ssAgt2;
                Endif;
                If BtnRtnCan  = *On;
                   ssAcct  = dsRtnFilter.ssAcct;
                   ssPtFl  = dsRtnFilter.ssPtFl;
                   ssBTyp  = dsRtnFilter.ssBTyp;
                   ssPtNm  = dsRtnFilter.ssPtNm;
                   ssFAmt  = dsRtnFilter.ssFAmt;
                   ssTAmt  = dsRtnFilter.ssTAmt;
                   ssFDat  = dsRtnFilter.ssFDat;
                   ssTDat  = dsRtnFilter.ssTDat;
                   ssCus1  = dsRtnFilter.ssCus1;
                   ssCus2  = dsRtnFilter.ssCus2;
                   ssAgt1  = dsRtnFilter.ssAgt1;
                   ssAgt2  = dsRtnFilter.ssAgt2;
                Endif;
             EndDo;

          // For "Change FEIN" option in More Action of Agency Toolbar
          When btnShowWin = *on;
             PopupWindow(txtShowWin:scActRRN:' ':txtWinData);
             btnShowWin = *off;


          // Menu Choices
          When scSelOpt <> *blanks;
             @@next = scSelOpt;
             Leave;


          Other;

          Endsl;
       Enddo;

       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       indexTab = TabIndex + 1;

       Select;

       When indexTab = 1;
          pnlHeight = 1220;
          APnlHeight = pnlHeight + 60;

          If reload = *on;
             // Load Return Finds grid
             LoadReturnFund();
          Endif;

          // Load A/R Transaction Detail Grid
          LoadTrDtlGrid();

          // Load Account Current Statement Grid
          LoadCurStmGrid();

          Write rcdAgtPnl;
          Write rcdPanel;
          Exfmt rcdRtnFnd;
          Read rcdAgtPnl;
          Read rcdPanel;

       EndSl;

       Endsr;

      /end-free
     p Main            e

       //===================================================================
       // ProcRoWClk - Process the Row Click in Return Funds Grid
       //===================================================================
     P ProcRoWClk      B
     D ProcRoWClk      PI

      /free

       // process to load A/R Transaction Detail for selected row
       Chain rrnRtnFund sflRtnFund;
       Chain scRtFndrrn pr40p;
       scRowSel = rrnRtnFund;

       co#_sav = R4co#;
       fnd_sav = R4fnd;
       fyr_sav = R4fyr;
       emp#_sav = R4emp#;
       agt#_sav = R4agt#;
       stdt_sav = R4stdt;

      /end-free

     P ProcRoWClk      E

       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi
     d processMe                       n
      /free

       // Program Mode.
       If q1prms >= 2;
          Select;
          When @@mode = 'D';
             dspMode = *on;
          When @@mode = 'C';
             chgMod  = *on;
          Endsl;
       Endif;

       apShowMenu = *on;

       aMenHeight = 145;

       apFootHght = aMenHeight + 125;

       // Set Menu options
       scMenuChoi = 'Primary Agency Information';
       scMenuValu = 'WTAGTPRI';

       scMenuChoi = %trim(scMenuChoi) + ',Group Assignments';
       scMenuValu = %trim(scMenuValu) + ',WTAGTASN';

       scMenuChoi = %trim(scMenuChoi) + ',Producers';
       scMenuValu = %trim(scMenuValu) + ',WTAGTPROD';

       scMenuChoi = %trim(scMenuChoi) + ',Contacts';
       scMenuValu = %trim(scMenuValu) + ',WTAGTCON';

       scMenuChoi = %trim(scMenuChoi) + ',Service Request';
       scMenuValu = %trim(scMenuValu) + ',WTAGTSRVRT';

       scMenuChoi = %trim(scMenuChoi) + ',Return Funds';
       scMenuValu = %trim(scMenuValu) + ',WTAGTRTFND';

       // Tab Index
       tabIndex = 0;

       If q1prms >= 4;
          If @@TabInd <> ' ';
             tabIndex = %int(@@TabInd);
             If tabIndex < 0 or tabIndex > 2;
                TabIndex = 0;
             Endif;
          Endif;
          @@TabInd = '00000';
       Endif;

       intCo# = GetUserInfo(q1user);
       // Update Action Panel Info (Rrn, Agency Name)
        scActRrn = xxRrn;
        scCompName = %trim(AGname);
        If scCompName = ' ';
           Chain intCo# pco00;
           If COname <> ' ';
              scCompName = COname;
           Endif;
        Endif;

       scCo# = intCo#;

       processMe = *on;

       Return;

      /end-free
     p ProgramStart    e

       //===================================================================
       // Load display indicators for sub-files in rcdRtnFnd Record Format
       //===================================================================
     P RcdRtnFndDsp    b
     D RcdRtnFndDsp    pi
     D  indValue                      1n   const
      /Free
       dspRtnFdSb = indValue;
       dspTrDtlSb = indValue;
       dspCurStm = indValue;
      /End-Free
     P RcdRtnFndDsp    e

       //==================================================================
        // RtnFndFilter -  Determine if the records meet the Filter criteria
       //==================================================================
     P RtnFndFilter    b
     d RtnFndFilter    PI

      /free

       If R4chk# = 0;
         meetss1 = *off;
       Endif;

       If ssAcct <> *Blanks And ssAcct <> R4acct;
         meetss1 = *Off;
       Endif;

       If ssFnd <> *Zeros And ssFnd <> R4fnd;
         meetss1 = *Off;
       Endif;

       If ssPtFl <> *Blanks And ssptfl <> R4ptfl;
         meetss1 = *Off;
       Endif;

       //If ssEmp# <> *Zeros And ssEmp# <> R4emp# ;
       //  meetss1 = *Off;
       //Endif;

       //If ssDiv <> *Zeros And ssDiv <> R4div;
       //  meetss1 = *Off;
       //Endif;

       //*** Begin Add ***** 08/26/25 *****************************
       If inFein <> 0 And inFein <> R4fein;
          meetss1 = *Off;
       Endif;

       If inMod <> 0 And inMod <> R4fein;
          meetss1 = *Off;
       Endif;
       //*** End   Add ***** 08/26/25 *****************************

       If ssbtyp <> *blanks And ssbtyp <> R4btyp;
         meetss1 = *off;
       Endif;

       // Check Payee against search string
       If ssptnm <> *Blanks;
         U1pdsc = Upper(R4ptnm);
         U2pdsc = Upper(ssptnm);
         Exsr String;
       Endif;

       If (ssvdsp = 'V' And R4vdsp <> 'V');
          meetss1 = *Off;
       Endif;

       If (ssvdsp = 'S' And R4vdsp <> 'S');
         meetss1 = *Off;
       Endif;

       If ((ssvdsp = 'B' And R4vdsp <> 'V') and
           (ssvdsp = 'B' And R4vdsp <> 'S'));
          meetss1 = *Off;
       Endif;

       If ssfamt <> *Zeros And R4amt < SSfamt;
          meetss1 = *Off;
       Endif;

       If sstamt <> *Zeros And R4amt > SStamt;
         meetss1 = *Off;
       Endif;

       If ssfdat <> *zeros;
         ssfdat7 = DtCYMD(ssfdat);
         If r4ckdt < ssfdat7;
           meetss1 = *off;
         Endif;
       Endif;

       If sstDat <> *zeros;
         sstDat7 = DtCYMD(sstdat);
         If R4ckdt > sstdat7;
           meetss1 = *off;
         Endif;
       Endif;

       If ssCus1 <> *Blanks And ssCus2 <> *Blanks;
         @r4cust = Upper(R4cust);

         If @R4cust < @ssCus1 Or @R4cust > @ssCus2;
            meetss1 = *Off;
         Endif;

       Endif;

       If ssAgt1 <> *Zero And ssAgt2 <> *Zero and
           (R4agt# < ssAgt1 Or R4agt# > ssAgt2);
       meetss1 = *Off;
       Endif;

      /end-free
       //=============================================================
        //STRING - SUBROUTINE TO SEARCH STRINGS
       //=============================================================
     c     String        Begsr

     c                   Call      'QCLSCAN'
     c                   parm                    U1PDSC
     c                   parm      40            STRLEN            3 0
     c                   parm      1             START             3 0
     c                   parm                    U2PDSC
     c                   parm      40            PATLEN            3 0
     c                   parm      '0'           TRANS             1
     c                   parm      '1'           TRIM              1
     c                   parm      ' '           WILD              1
     c                   parm                    SCNRES            3 0

     c     scnres        ifeq      0
     c                   move      *off          meetss1
     c                   endif

     c                   Endsr
     P RtnFndFilter    E


      // utility procedure definitions
      /COPY SCOPYCOMPI

