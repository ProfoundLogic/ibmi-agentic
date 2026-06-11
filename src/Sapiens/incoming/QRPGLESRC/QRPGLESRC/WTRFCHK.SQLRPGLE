     hcopyright('(c) 2025 Sapiens, Inc. All rights reserved.')

      //*********************************************************************
     h/title WTRFCHK - Return Fund Checks
     h dftname(WTRFCHK)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h ALWNULL(*UsrCtl)
     h/define   profoundUI
     h bnddir('STBNDDIR')
     h bnddir('WTGRPCFG')
     h bnddir('WPIAGENT')
      //*********************************************************************
      //  Created   : 10/16/23                                         *
      //  Programmer: C:342                                            *
      //  Project   : WC2676 - Redesign of Work with Return Funds Check*
      //===============================================================*
      //  Revisions:                                                   *
      //                                                               *
      //  05/13/25 C:333 Support: 118669 (FCC)                         *
      //                 Return Funds Screen Request.                  *
      //                 DSPF Changes:                                 *
      //                 - Renamed rcdPanel to rcdPanelCk.             *
      //                 - Added visibility for TabRFCheck.            *
      //                 - Changed overlay format for rcdRFChks.       *
      //                 - Added visibility for HdgRFCheck.            *
      //                                                               *
      //  01/28/25 C:300 Support: 118389 (CoveRisk)                    *
      //                 DSPF: Apply Filter to Return Funds Check Grid *
      //                 Check for 9999 records in the Grid            *
      //                                                               *
      //                                                               *
      // ------------------------------------------------------------- *
      //  NOTE: Please add comments to the top of Revisions.           *
      // ------------------------------------------------------------- *
      //===============================================================*

     fwtrfchk   cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflRFChks:scSflRRN)
     f                                     Sfile(sflCurStm:rrnCurStm)
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)
      * Return Premium Check Request File
     fwar4l6    if   e           k disk    rename(pr401:pr406)
     f                                     infds(dbfds)
     fwar4p     uf a e             disk    rename(pr401:pr40p)
     f                                     infds(dbfds3)
      * Company file
     fsmcol     if   e           k disk    rename(pco01:pco00)
      * User Parameter File
     fsmupl     if   e           k disk    rename(pup01:pup00)
      * Employer Master file
     fwmeml     if   e           k disk    rename(pem01:pem00)
     f                                     infds(dbfds1)
      * Employer Detail file
     fwdell     if   e           k disk    rename(pel01:pel00)
     f                                     infds(dbfds2)
      * Account Current Summary File
     fwsasl     uf   e           k disk    rename(pas01:pas00)
      * Application System Control file
     fssylf     if   e           k disk
     f                                     ignore(SSY01)
     f                                     ignore(SPC01)
     f                                     ignore(SRC01)
     f                                     ignore(SBC01)
     f                                     ignore(SCS01)
      * Logical over all table files
     fwtabl     if   e           k disk
     f                                     include(ptt01)
      * Bank Account Master file
     fwsckl     if   e           k disk    rename(pck01:pck00)
      * Agency Billing Detail File
     fwdabp     o    e             disk    rename(pab01:pab0p)
     fwdabl     if   e           k disk    rename(pab01:pab00)
     f                                     infds(dbfds4)
      * employer accounts receivable file
     fwmeap     o    e           k disk    rename(pea01:pea0p)
      * Mass Return Premium Batch Control File
     fwxrnl     if a e           k disk    rename(prn01:prn00)
      * Return Premium Batch Detail File
     fwxrtp     o    e           k disk    rename(prt01:prt0p)
     d
     d psds          esds                  extname(szq1p)
     d dbfds           ds
     d  dbrrn                397    400b 0
     d dbfds1          ds
     d  dbrrn1               397    400b 0
     d dbfds2          ds
     d  dbrrn2               397    400b 0
     d dbfds3          ds
     d  dbrrn3               397    400b 0
     d dbfds4          ds
     d  dbrrn4               397    400b 0

      // work variables
     d saveDT          s                   like(R4date)
     d saveTM          s                   like(R4time)
     d flgRcdUpdated   s              1    inz
     d intCo#          s              3  0
     d @@mode          s              1
     D indexTab        s              5  0
     d chgMod          s               n   inz(*off)
     d dspMode         s               n   inz(*off)
     d process         s               n   inz(*off)
     d svsflrrn        s              5  0 inz
     d kkkey           s                   like(ttkey)
     d xxbill          s                   like(ASBILL)
     d xxpaid          s                   like(ASPAID)
     d kkbat#          s                   like(rnbat#)
     d str9999Msg      s            256
      *** Begin Add ***** 01/28/25 *****************************
     d charMsg         s            100
      // Filter screen variable
     d meetFltr        s               n
     d dsFilter        ds                  likerec(rcdRFCFltr) Inz
      *** End   Add ***** 01/28/25 *****************************

      // calling parameter for W0050R     (Policy Inquiry)
     d ##kc            s              1
     d ##kl            s              1
     d ##mode          s              1
     d ##rrn           s              9  0
     d ##rrn2          s              9  0
     d ##qte           s              1
     d ##view          s              1  0
     d ##rrn3          s              9  0
     d
      // calling parameter for WtPolARInq (Policy A/R Inquiry)
     d #1rrn           s              9
     d #1mode          s              1
     d #1next          s             10
     d #1tabind        s              5
     d #1Qte           s              1

      // Emp info
     d GetEmpInfo      pr                  ExtPgm('W0050R')
     d ##kc                           1
     d ##kl                           1
     d ##mode                         1
     d ##rrn                          9  0
     d ##rrn2                         9  0
     d ##qte                          1
     d ##view                         1  0
     d ##rrn3                         9  0

      * Load Return Fund Checks
     d LoadRFCheck     pr

      * Load Account Current Statement Grid
     d LoadCurStmGrid  pr

      * Main Processing
     d Main            pr

      *** Begin Add ***** 01/28/25 *****************************
      // Meet Filter for Return Funds Check Grid
     d MeetFlt         pr
      *** End   Add ***** 01/28/25 *****************************

      * Process other Options for Return Funds checks
     d ProcOther       pr

      * Program Start
     d ProgramStart    pr
     d processMe                       n

     * Validate return fund checks - Void/Stopre.
     d ValidateAR      pr              n

      *** Begin Add ***** 01/28/25 *****************************
     * Validate value entered on filter screen for Return Funds Check Grid
     d ValidateFlt     pr              n
      *** End   Add ***** 01/28/25 *****************************

     d WtPolARInq      pr                  ExtPgm('WTPOLARINQ')
     d #1rrn                          9
     d #1mode                         1
     d #1next                        10
     d #1tabind                       5
     d #1Qte                          1

     d/copy sprUsrPrms
     d/copy sPrGenPop
     d/copy sPrDate
     d/copy sprCompany
     d/copy wprAgent
      *** Begin Add ***** 01/28/25 *****************************
     d/copy wPrPolErr
      *** End   Add ***** 01/28/25 *****************************

      //   Key List for the Documents Table File
     c     keyWdabl      klist
     c                   kfld                    abco#
     c                   kfld                    abstdt
     c                   kfld                    abagt#

     c     keyWsasl      klist
     c                   kfld                    abco#
     c                   kfld                    abstdt
     c                   kfld                    abagt#

     c     keyWxrnl      klist
     c                   kfld                    R4co#
     c                   kfld                    kkbat#

     c     asky          klist
     c                   kfld                    R4co#
     c                   kfld                    R4stdt
     c                   kfld                    R4agt#
      *================================================================

      // Entry of Program.

     c     *entry        plist
     c                   parm                    @@rrn             9            rrn
     c                   parm                    @@mode            1            mod
     c                   parm                    @@next           10            next pgm
     c                   parm                    @@TabInd          5            tab index


      /free
       Exec Sql Set Option Commit = *None;

       ProgramStart(process);

       str9999msg = 'Your request returned too many records. ' +
        'Click Continue to see the first 9999 records. ' +
        'Click Filter on the display screen to narrow your results.';

       //*** Begin Add ***** 01/28/25 *****************************
       // Set Current Date and Current Date less 2 year date
       scDate2 = DateTo8(SystemDate());
       scDate1 = DateTo8(CvtToDate(scDate2) - %years(2));
       // Initialize Filter to load 2 years old data
       scFrCkDt = scDate1;
       scToCkDt = scDate2;
       dsFilter.scFrCkDt = scDate1;
       dsFilter.scToCkDt = scDate2;
       //*** End   Add ***** 01/28/25 *****************************

       If process;
          Main();
       Endif;

       *InLr = *On;
      /end-free

       //===================================================================
       // Load Account Current Statement Grid
       //===================================================================
       //===================================================================
     P LoadCurStmGrid  B
     D LoadCurStmGrid  PI

      /free

       rrnCurStm = 0;
       dspCurStm = *off;
       clrCurStm = *on;
       Write RcdRFCAcSt;
       clrCurStm = *off;
       dspCurStm = *on;
       scRowSel2 = 1;

       Clear scAgt#;
       Clear scAnam;
       Clear scStdt;
       Clear scDue;
       Clear scBill;
       Clear scPaid;
       Clear scBal;

       Chain(n) asky pas00;
       If %found(wsasl);

          // Load Header fields from wsasl
          scAgt# = ASagt#;
          scAnam = GetAgtName(R4agt#);
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

             scCurStRRN = dbrrn4;

             rrnCurStm = rrnCurStm + 1;
             If rrnCurStm = 1;
                scCurStSel = *on;
             Else;
                clear scCurStSel;
             Endif;

             Select;
             When rrnCurStm > 9999;
               PopOneBtn(str9999msg:'Message-Account Current State-
                ment':'Continue');
                Leave;
             Endsl;

             Write sflCurStm;

             Reade asky pab00;
          Enddo;
          rrnCurStm = 1;

       Endif;

      /end-free
     P LoadCurStmGrid  E

       //===================================================================
       // Load Return Fund Checks Grid
       //===================================================================
     p LoadRFCheck     b
     d LoadRFCheck     pi

      /free
       // Clear the subfile
       scSflRRN = 0;
       scSflDsp = *off;
       scSflClr = *on;
       Write rcdRFChks;
       scSflClr = *off;
       scSflDsp = *on;
       scRowSel = 1;

       // Populate the subfile
       Setll *loval war4l6;
       Read war4l6;
       Dow not %eof(war4l6);
          If R4chk# <> *zeros;
             //*** Begin Add ***** 01/28/25 *****************************
             // Validate filter for Return Funds Check Grid
             meetFltr = *on;
             If scFrCkDt <> *zeros or scToCkDt <> *zeros;
                MeetFlt();
             Endif;

             If meetFltr = *on;
             //*** End   Add ***** 01/28/25 *****************************

             scSflRRN += 1;
             scSflSel = *off;
 
             If svsflrrn = 0;
                If scSflRRN = 1;
                   scSflSel = *on;
                Endif;
             Else;
                If scSflRRN = svsflrrn;
                   scSflSel = *on;
                Endif;
             Endif;
 
             scPayNm  = R4ptnm;
             scChkDt  = CvtToDate(R4ckdt);
             scChk#   = R4chk#;
             scBkAcct = R4acct;
             scBType  = R4btyp;
             scChkAmt = R4amt;
             select;
             when R4vdsp = 'V';
                scVS = 'Void';
             when R4vdsp = 'S';
                scVS = 'Stop';
             other;
                scVS = *blanks;
             endsl;

             scRRNChk  = dbrrn;

             // Set visibility for Account Current Statement.
             visAccCur = *off;
             Chain(n) (R4co#:R4stdt:R4agt#) pas00;
             If %found(wsasl);
                visAccCur = *on;
             Endif;

             //*** Begin Add ***** 01/28/25 *****************************
             If scSflRRN > 9999;
                PopOneBtn(str9999msg:'Return Funds Check':'Continue');
                Leave;
             Endif;
             //*** End   Add ***** 01/28/25 *****************************

             Write sflRFChks;

             //*** Begin Add ***** 01/28/25 *****************************
             Endif;                                // If meetFltr = *on
             //*** End   Add ***** 01/28/25 *****************************
          Endif;
          Read war4l6;
       Enddo;

       scSflRRN = 1;
       If svSflRRN <> 0;
          scSflRRN = svSflRRN;
          scRowSel = svSflRRN;
          svSflRRN = 0;
       Endif;

      /end-free
     p LoadRFCheck     e

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi
       //*** Begin Add ***** 01/28/25 *****************************
     d isexit          s               n   Inz(*off)
       //*** End   Add ***** 01/28/25 *****************************
      /free

       LoadActionBar();
       BtnCancel = *off;

       // Process until exit button is pressed.
       DoU BtnCancel = *On;

          // Load Return Funds Check Grid
          LoadRFCheck();

          // display screen
          Exsr ExfmtScreens;

          // Perform actions on screen.
          Select;

          // Refresh
          When btnRefresh = *On;
             btnRefresh = *Off;

          // Save
          When btnSave = *On;
             btnSave = *Off;

          //*** Begin Add ***** 01/28/25 *****************************
          // Filter Processing for Return Funds Check Grid
          When BtnFilter = *on;
             BtnFilter = *off;
             isExit = *off;
             Dow isExit = *off;
                ExFmt rcdRFCFltr;
                Select;
                When BtnFltrApp = *on;
                   BtnFltrApp = *off;
                   If ValidateFlt();
                      dsFilter.scFrCkDt = scFrCkDt;
                      dsFilter.scToCkDt = scToCkDt;
                      isExit = *on;
                      dspClrFltr = *on;
                   Endif;

                When BtnFltrCan = *on;
                   BtnFltrCan = *off;
                   scFrCkDt = dsFilter.scFrCkDt;
                   scToCkDt = dsFilter.scToCkDt;
                   isExit = *on;

                   msFrCkDt = *blanks;
                   erFrCkDt = *off;
                   msToCkDt = *blanks;
                   erToCkDt = *off;
                Endsl;
             Enddo;

          // Filter clear processing
          When BtnClrFltr = *on;
             BtnClrFltr = *off;
             Clear rcdRFCFltr;
             // Set Current Date and Current Date less 2 year date
             scDate2 = DateTo8(SystemDate());
             scDate1 = DateTo8(CvtToDate(scDate2) - %years(2));
             // Initialize Filter to load 2 years old data
             scFrCkDt = scDate1;
             scToCkDt = scDate2;
             Clear dsFilter;
             dsFilter.scFrCkDt = scDate1;
             dsFilter.scToCkDt = scDate2;
             dspClrFltr = *off;
          //*** End   Add ***** 01/28/25 *****************************

          Other;
             ProcOther();             // Process other Options for Return Funds check

          Endsl;
       Enddo;

       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       indexTab = TabIndex + 1;

       Select;
       When indexTab = 1;
          pnlHeight = 460;
          APnlHeight = pnlHeight + 60;
          Write rcdComPnl;
          //*** Beg Chg ***** 05/13/25 ************************************
          // Write rcdPanel;
          // Exfmt rcdRFChks;
          // Read rcdComPnl;
          // Read rcdPanel;
          tabVisInd = *On;
          Write rcdPanelCk;
          Exfmt rcdRFChks;
          Read rcdComPnl;
          Read rcdPanelCk;
          //*** End Chg ***** 05/13/25 ************************************

       EndSl;

       Endsr;

      /end-free
     p Main            e

      *** Begin Add ***** 01/28/25 *****************************
       //==================================================================
       // MeetFlt -  Determine if the records meet the Filter criteria for Return Funds Check Grid
       //==================================================================
     p MeetFlt         b
     d MeetFlt         pi
      /free

       // Check for Check Date Range
       If scFrCkDt <> *zeros and R4ckdt < dtCYMD(scFrCkDt);
          meetFltr = *off;
       Endif;
       If scToCkDt <> *zeros and R4ckdt > dtCYMD(scToCkDt);
          meetFltr = *off;
       Endif;

      /end-free
     p MeetFlt         e
      *** End   Add ***** 01/28/25 *****************************

       //===================================================================
       // ProcOther - Process other option for Return Fund Checks
       //===================================================================
     p ProcOther       b
     d ProcOther       pi
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
     d  flgVdsp        s              1    Inz

      /Free

       Clear rcdRFCVS;
       isexit = *off;
       ReadC sflRFChks;
       Dow not %eof;
          Select;

          // Process Emp Info option for Return Fund Checks
          When scRFCEInf = *on;
             scRFCEInf = *off;
             svsflrrn = scSflrrn;  //save rrn value
             Chain scRRNChk pr40p;
             Chain (R4co#:R4fnd:R4emp#:R4div) pem00;
             If %found(wmeml);
                ##kc = *inkc;
                ##kl = *inkl;
                ##mode ='Z';
                ##rrn  = 0;
                ##rrn2 = 0;
                ##qte  = 'W';
                ##view = 0;
                ##rrn3 = dbrrn1;
                GetEmpInfo(##kc:##kl:##mode:##rrn:##rrn2:##qte:##view:##rrn3);
             Endif;

          // Process A/R inquiry information.
          When scRFCARInq = *on;
             //*** Beg Add ***** 05/13/25 ************************
             tabVisInd  = *Off;
             Write rcdPanelCk;
             //*** End Add ***** 05/13/25 ************************
             scRFCARInq = *off;
             svsflrrn = scSflrrn;  //save rrn value
             Chain(n) scRRNChk pr40p;
             Chain (R4co#:R4fnd:R4emp#:R4div:R4fyr) pel00;
             #1rrn = %char(dbrrn2);
             #1mode = 'D';
             #1next = 'ARInq';
             #1TabInd = *blanks;
             WtPolARInq(#1rrn:#1mode:#1next:#1TabInd:#1Qte);

          // Process Void option for return fund checks
          When scRFCVoid = *on;
             scRFCVoid = *off;
             scRFCVSHdg = 'Process Void';
             svsflrrn = scSflrrn;  //save rrn value
             Chain(n) scRRNChk pr40p;
             If R4vdsp = 'V' or R4vdsp = 'S';
                PopOneBtn('Cannot Issue a Duplicate Void or Stop.'
                   :'Error':'Ok');
                Leave;
             Endif;
             Exsr MVdbsc;
             Dow isexit = *off;
                Exfmt rcdRFCVS;
                Select;
                When BtnSaveVS = *on;
                   BtnSaveVS = *off;
                   // Check if WAR4P updated since screen displayed
                   flgRcdUpdated=RcdUpdatedErr('WAR4P':dbrrn3:'R4date':'R4time':
                    'R4user':saveDT:saveTM:'1');
                   if flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scRRNChk pr40p;
                      Exsr Mvdbsc;
                   EndIf;
                   If flgRcdUpdated = '0'; // No error
                      // Validate AR Code.
                      If ValidateAR();
                         Chain scRRNChk pr40p;
                         flgVdsp = 'V';
                         Exsr WriteVS;
                         isExit = *on;
                      Endif;
                   Endif;

                When BtnCanVS = *on;
                   BtnCanVS = *off;
                   isExit = *on;
                Endsl;
             Enddo;

          // Process Stop option for return fund checks
          When scRFCStop = *on;
             scRFCStop = *off;
             scRFCVSHdg = 'Process Stop';
             svsflrrn = scSflrrn;  //save rrn value
             Chain(n) scRRNChk pr40p;
             If R4vdsp = 'V' or R4vdsp = 'S';
                PopOneBtn('Cannot Issue a Duplicate Void or Stop.'
                   :'Error':'Ok');
                Leave;
             Endif;
             Exsr MVdbsc;
             Dow isexit = *off;
                Exfmt rcdRFCVS;
                Select;
                When BtnSaveVS = *on;
                   BtnSaveVS = *off;
                   // Check if WAR4P updated since screen displayed
                   flgRcdUpdated=RcdUpdatedErr('WAR4P':dbrrn3:'R4date':'R4time':
                    'R4user':saveDT:saveTM:'1');
                   if flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scRRNChk pr40p;
                      Exsr Mvdbsc;
                   EndIf;
                   If flgRcdUpdated = '0'; // No error
                      // Validate AR Code.
                      If ValidateAR();
                         Chain scRRNChk pr40p;
                         flgVdsp = 'S';
                         Exsr WriteVS;
                         isExit = *on;
                      Endif;
                   Endif;
                When BtnCanVS = *on;
                   BtnCanVS = *off;
                   isExit = *on;
                Endsl;
             Enddo;

          // Process inquiry for Agency Billing Detail
          When scRFCStmt = *on;
             scRFCStmt = *off;
             svsflrrn = scSflrrn;  //save rrn value
             Chain(n) scRRNChk pr40p;
             Chain(n) asky pas00;
             If %found(wsasl);
                LoadCurStmGrid();
                Dow isexit = *off;
                   Exfmt RcdRFCAcSt;
                   Select;
                   When BtnCanStMt = *on;
                      BtnCanStMt = *off;
                      isexit = *on;
                   Endsl;
                Enddo;
             Endif;

          Endsl;

          Unlock War4p;

          ReadC sflRFChks;
       Enddo;

       // Move data base fields to screen fields
       Begsr MVdbsc;
        Chain (R4co#: R4acct) pck00;
        If %found(wsckl);
           scARCodeE = CKrpvs;
        Endif;
        saveDT = R4date;
        saveTM = R4time;
       Endsr;

       // Move data base fields to screen fields
       Begsr WriteVS;
        R4vdsp = flgVdsp;
        R4pgm  = q1pgm;
        R4user = q1user;
        R4date = Dateto7(SystemDate());
        R4time = Timeto6(SystemTime());
        update pr40p;

        Select;
        When R4acfl = 'Y';
           Exsr writeAB;
        When R4acfl <> 'Y' and R4BTYP <> 'I';
           Exsr writeAR;
        Endsl;
        If scChkReq = 'Y';
           Exsr genRequest;
        Endif;
       Endsr;

       // genRequest - Regenerate check request subroutine
       Begsr genRequest;
        //Get next batch # and write return premium batch control file
        kkbat# = *hival;
        Setgt keyWxrnl prn00;
        Readpe R4co# prn00;
        If %eof(wxrnl);
           RNbat# = 1;
        Else;
           RNbat# = RNbat# + 1;
        Endif;

        RNco#  = R4co#;
        RNfgrp = R4fnd;
        RNtgrp = R4fnd;
        clear RNcom1;
        clear RNcom2;
        clear RNcom3;
        clear RNmin;

        RNfaud = R4ckdt;
        RNtaud = R4ckdt;
        RNbpf = 'Y';

        RNuser  = q1user;                // Maint User
        RNpgm   = q1pgm;                 // Maint Program
        RNtime  = Timeto6(SystemTime()); // Maint Time
        RNdate  = Dateto7(SystemDate()); // Maint Date
        RNiusr  = q1user;                // Initial User
        RNipgm  = q1pgm;                 // Initial Program
        RNitim  = Timeto6(SystemTime()); // Initial Time
        RNidt   = Dateto7(SystemDate()); // Initial Date

        RNptyp = R4ptfl;
        RNmtyp = 'M';

        select;
        when R4acfl = 'Y';
           RNbtyp = 'A';
        when R4btyp <> *blanks;
           RNbtyp = R4btyp;
           RNptyp = R4ptfl;
        other;
          RNbtyp = 'P';
        endsl;
        write prn00;

        // write to wxrtp (return premium batch detail)
        Clear prt0p;
        RTco#  = R4co#;
        RTbat# = Rnbat#;
        RTfnd  = R4fnd;
        RTemp# = R4emp#;
        RTdiv  = R4div;
        Chain (R4co#:R4fnd:R4emp#:R4div) pem00;
        If %found(wmeml);
           RTempn = emname;
        Endif;

        RTagt# = R4agt#;
        RTagtn = GetAgtName(R4agt#);
        RTfein = R4fein;
        RTmod  = R4mod;
        RTfyr  = R4fyr;
        RTamt  = R4amt;
        RTselc = 'Y';
        RTacfl= R4acfl;
        RTbtyp = RNbtyp;
        RTarcd = R4arcd;
        RTcom1 = 'Regenerated check request';
        RTptfl = R4ptfl;
        RTpfin = R4pfin;

        RTuser  = q1user;                // Maint User
        RTpgm   = q1pgm;                 // Maint Program
        RTtime  = Timeto6(SystemTime()); // Maint Time
        RTdate  = Dateto7(SystemDate()); // Maint Date
        RTiusr  = q1user;                // Initial User
        RTipgm  = q1pgm;                 // Initial Program
        RTitim  = Timeto6(SystemTime()); // Initial Time
        RTidt   = Dateto7(SystemDate()); // Initial Date

        write prt0p;
       Endsr;


       //Write Agency Billing for void/stop payment
       Begsr WriteAB;
        clear pab0p;
        ABco#  = R4co#;
        ABfein = R4fein;
        ABmod  = R4mod;
        ABagt# = GetAgent#(R4co#:R4fein:R4mod);
        ABstdt = R4stdt;
        ABfnd  = R4fnd;
        ABemp# = R4emp#;
        ABdiv  = R4div;
        ABfyr  = R4fyr;
        ABamt  = R4amt * 1;
        ABchk# = R4chk#;
        ABarcd = scARCodeE;
        kkkey  = ABarcd;
        chain (R4co#: kkkey) ptt01;
        If %found(wtabl) and ttstat <>'I';
           ABarty = TTtype;
           ABdesc = TTdesc;
        Endif;
        ABtrdt = Dateto7(SystemDate());
        ABpodt = Dateto7(SystemDate());

        ABuser  = q1user;                // Maint User
        ABpgm   = q1pgm;                 // Maint Program
        ABtime  = Timeto6(SystemTime()); // Maint Time
        ABdt    = Dateto7(SystemDate()); // Maint Date
        ABiusr  = q1user;                // Initial User
        ABipgm  = q1pgm;                 // Initial Program
        ABitim  = Timeto6(SystemTime()); // Initial Time
        ABidt   = Dateto7(SystemDate()); // Initial Date
        write pab0p;

        // Update WSASP record
        exsr updateWSASP;
       Endsr;

       // writeAR - write AR for void/stop payment
       Begsr WriteAR;
        EAco#  = R4co#;
        EAfnd  = R4fnd;
        EAemp# = R4emp#;
        EAdiv  = R4div;
        EAfyr  = R4fyr;
        EAamt  = R4amt * 1;
        EAchk# = R4chk#;
        EAarcd = scARCodeE;
        kkkey  = EAarcd;

        chain (R4co#: kkkey) ptt01;
        If %found(wtabl) and ttstat <>'I';
           EAarty = TTtype;
           EActyp = TTctyp;
           EAdesc = TTdesc;
        Endif;

        EAtrdt  = Dateto7(SystemDate());
        EApodt  = Dateto7(SystemDate());

        EAuser  = q1user;
        EApgm   = q1pgm;
        EAtime  = Timeto6(SystemTime());
        EAdt    = Dateto7(SystemDate());
        write pea0p;

       Endsr;

       // updateWSASP - Update WSASP record
       Begsr updateWSASP;
        clear xxbill;
        clear xxpaid;
        setll keyWdabl pab00;
        reade keyWdabl pab00;
        Dow not %eof(wdabl);
           select;
           when abarty = 'A' or abarty = 'B' or abarty = 'D'
              or abarty = 'F' or abarty = 'H' or abarty = 'N';
              xxbill = xxbill + abnet;
           when abarty = 'P' or abarty = 'I' or abarty = 'Q'
              or abarty = 'T';
              xxpaid = xxpaid + abamt;
           when abarty = 'R' or abarty = 'V';
              xxpaid = xxpaid - abamt;
           endsl;
           reade keyWdabl pab00;
        Enddo;

        Chain keyWsasl pas00;
        If %found(wsasl);
           ASbill = xxbill;
           ASpaid = xxpaid;
           ASbal = ASbill - ASpaid;

           ASpgm  = q1user;                // Maint User
           ASuser = q1pgm;                 // Maint Program
           ASdt   = Timeto6(SystemTime()); // Maint Time
           AStime = Dateto7(SystemDate()); // Maint Date
           update pas00;
        Endif;

       Endsr;

      /End-Free
     p ProcOther       e

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

          scActMode = @@mode;
       Endif;

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
       Chain intCo# pco00;
       If %found(smcol) and COname <> ' ';
          scCompName = COname;
       Endif;
       scCo# = intCo#;

       // Retrieve user parameter
       Chain q1user pup00;

       // Get Flags value from Workers Comp control file (SWCPF)
       Chain 'WC' SWC01;

       processMe = *on;

       Return;

      /end-free
     p ProgramStart    e

      //===================================================================
      // Validate the Return Fund Checks
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateAR      b
     d ValidateAR      pi              n
     d isError         s               n   inz
      /Free

        isError = *off;
       // Initialize all the messageId and error indicators.
        erARCodeE = *off;
        msARCodeE = *blanks;

       // AR code cannot be blank
        If scARCodeE  = *blanks;
           erARCodeE = *on;
           msARCodeE = 'WCR0503';
           isError   = *on;
        Endif;

      // Return *on indicator when No error exists.
        If isError = *on;
           return *off;
        Else;
           return *on;
        Endif;

      /End-Free
     p ValidateAR      e

      *** Begin Add ***** 01/28/25 *****************************
      //===================================================================
      // Validate Filter criteria for AR Batch Master Grid
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateFlt     b
     d ValidateFlt     pi              n
     d isError         s               n   inz
      /Free

       isError = *off;

       msFrCkDt = *blanks;
       erFrCkDt = *off;
       msToCkDt = *blanks;
       erToCkDt = *off;

      // Validate Check Date Range
       If scFrCkDt <> *zeros;
          erFrCkDt = ValidDate(scFrCkDt:charMsg);
          if erFrCkDt = *on;
             msFrCkDt = charMsg;
             isError = *on;
          Endif;
       Endif;
       If scToCkDt <> *zeros;
          erToCkDt = ValidDate(scToCkDt:charMsg);
          if erToCkDt = *on;
             msToCkDt = charMsg;
             isError  = *on;
          Endif;
       Endif;
      // Dates must be in Ascending order
       If scFrCkDt <> 0 or scToCkDt <> 0;
          If erToCkDt = *off and erFrCkDt = *off;
             erToCkDt = AscendDates(scFrCkDt:scToCkDt:charMsg);
             If erToCkDt = *on;
                erFrCkDt = *on;
                msToCkDt = charMsg;
                msFrCkDt = charMsg;
                isError = *on;
             Endif;
          Endif;
       Endif;

      // Return *on indicator when No error exists.
       If isError = *on;
          return *off;
       Else;
          return *on;
       Endif;

      /End-Free
     p ValidateFlt     e
      *** End   Add ***** 01/28/25 *****************************

      // utility procedure definitions
      /COPY SCOPYCOMPI

