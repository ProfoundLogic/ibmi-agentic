     hcopyright('(c) 2025 Sapiens, Inc. All rights reserved.')

     h/title WTGRPADMIN - Administration / DARTS Configuration
     h dftname(WTGRPADMIN)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h ALWNULL(*UsrCtl)
     h/define   profoundUI
     h bnddir('STBNDDIR')
     h bnddir('WTGRPCFG')
      //*********************************************************************
      //==============================================================*
      //  Created   : 01/22/20                                         *
      //  Programmer: C:300                                            *
      //  Project   : WC2196- Redesign of Group Configuration program  *
      //===============================================================*
      //  Revisions:                                                   *
      *                                                                *
      *   07/01/25 C:349 Support: 119301 (AmWINS)                      *
      *                  Unchecked the "Right Justify and Blank Fill"  *
      *                  checkbox property of the DrpDefaultOffice#    *
      *                  because it is a 5 alpha field & it must match *
      *                  the WMOFP table value.                        *
      *                                                                *
      *   10/16/24 C:300 Support: 117877                               *
      *                  Audit Activity showing incorrect value on Policy Tab
      *                  DSPF: Corrected Audit Activity field name from*
      *                        scAacd to acA1cd                        *
      *                                                                *
      *   08/06/24 C:300 Support: 117247                               *
      *                  Saving info on 1st Tab Clears info on other Tabs
      *                                                                *
      *   07/15/24 C:333 Support: 115805 (SUNZ)                        *
      *                  Error when saving on the Group Admin/Darts    *
      *                  Config screen                                 *
      *                                                                *
      *   06/24/24 C:300 Project: 146WKR (Prescient)                   *
      *                  Collateral Processing                         *
      *                  Re-complie over new fields in WMEXP file      *
      *                                                                *
      *   05/23/24 C:300 Support: 115634 (SUNZ)                        *
      *                  Refresh Button should clear all errors and    *
      *                  Re-load screen with latest file information   *
      *                                                                *
      *   03/04/24 C:342 Project: WC2826                               *
      *                  Claims Contacts                               *
      *                  Update new value in stsppcrt1 file if changed *
      *                  and found.                                    *
      *                                                                *
      *   10/21/23 C:300 Project: WC2196                               *
      *                  ASE not working for 'Print FROI' checkbox     *
      *                  DSPF: Corrected the 'Print FROI' checkbox ID  *
      *                                                                *
      *   07/03/23 C:300 Support: 107702 (SUNZ)                        *
      *                  Add Delete from Quote Sec lvl and type        *
      *                                                                *
      *   06/27/23 C:300 Project: WC2910                               *
      *                  Check User Security from Application Configuration
      *                  instead of User Parameters file               *
      *                                                                *
      *   06/23/23 C:300 Project: WC2910, Support: 111868              *
      *                  Display all screens in Inquiry mode, even if  *
      *                  user option security level does not allow them*
      *                  to Edit group information.                    *
      *
      *  05/23/23 C:300 Project: WC2910                               *
      *                 - Add User Access Security                    *
      *                                                               *
      *  05/10/23 C:300 Project:W2196 Support:109128                  *
      *                 Do not reload when user re-visit same Tab, if *
      *                 screen is pending with un-saved changes       *
      *
      *  08/29/22 C:340  Project WC2823
      *                  FROI Data Field Updates
      *
      *  03/02/22 C:136 Project: WC2818
      *                 Add flag to include CSV with Agent
      *                 Commission Statement
      *
      *  01/17/22 C:291 Project: WC2788
      *                 Add "Copy Configuration" option to Group
      *                 Toolbar.
      *
      *  04/19/21 C:53   Project WC2278
      *                  Changes for contact enhancement
      *                                                                *
      // ------------------------------------------------------------- *
      //  NOTE: Please add comments to the top of Revisions.           *
      // ------------------------------------------------------------- *
      //===============================================================*

     fWTGRPADMINcf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdGrpPnl)
      * Company file
     fsmcol     if   e           k disk    rename(pco01:pco00)
      * Fund Master file
     fwmfnp     if   e             disk    rename(pfn01:pfn0p)
      * Admin/Darts Control File
     fwmexl     uf a e           k disk    rename(pex01:pex00)
     f                                     infds(dbfds)
      * Endors/Darts Control File
     fwmdxl     uf a e           k disk    rename(pdx01:pdx00)
     f                                     infds(dbfds2)
      * Audit Tracking Control File
     fwmaxl     uf a e           k disk    rename(pax01:pax00)
     f                                     infds(dbfds3)
      * Multiple Darts Dairy file
     fwmdrl     if   e           k disk
      * Claim/DARTS control definition file
     fwmcxl     uf a e           k disk    rename(pcx01:pcx00)
     f                                     infds(dbfds4)
      * WCMC control file
     ffmcxl     uf   e           k disk    rename(pcx01:pfx00) prefix(f_)
     f                                     usropn
      * Claim/DARTS control definition file Extension
     fwmcxp2    uf a e           k disk    prefix(x2_)
     f                                     infds(dbfds5)
      * Remark Type Table Record
     fstctl1    if   e           k disk
      * system table file
     fsfbpf     if   e             disk    rename(sfb01:sfb02)
      * Application System Control file
     fssylf     if   e           k disk
     f*                                    ignore(SSY01)
     f                                     ignore(SPC01)
     f                                     ignore(SRC01)
     f                                     ignore(SBC01)
     f                                     ignore(SCS01)
     f*** Beg Add *** 05/23/23 *************************************************
     fsmupl     if   e           k disk
     fsdtbl1    if   e           k disk
     f*** End Add *** 05/23/23 *************************************************
     f*** Beg Add *** 03/04/24 *************************************************
      * Scripture Processes file 1 criteria
     fstsppcrt1 uf   e           k disk    rename(ps101:ps10p)
     f*** End Add *** 03/04/24 *************************************************

     d psds          esds                  extname(szq1p)
     d dbfds           ds
     d  dbrrn                397    400b 0
     D
     d dbfds2          ds
     d  dbrrn2               397    400b 0
     D
     d dbfds3          ds
     d  dbrrn3               397    400b 0
     D
     d dbfds4          ds
     d  dbrrn4               397    400b 0
     D
     d dbfds5          ds
     d  dbrrn5               397    400b 0


      // work variables
     d saveDT          s                   like(FNdt)
     d saveTM          s                   like(FNtime)
     d saveDT2         s                   like(FNdt)
     d saveTM2         s                   like(FNtime)
     d saveDT3         s                   like(FNdt)
     d saveTM3         s                   like(FNtime)
     d saveDT4         s                   like(FNdt)
     d saveTM4         s                   like(FNtime)
     d saveDT5         s                   like(FNdt)
     d saveTM5         s                   like(FNtime)
     d flgRcdUpdated   s              1    inz
     d clientID        s              3    inz('000')
     d intCo#          s              3  0
     d inCo#           s              3  0
     d inFnd           s              3  0
     D indexTab        s              5  0
     d xxRrn           s              9  0 inz
     d chgMod          s               n   inz(*off)
     d dspMode         s               n   inz(*off)
     d error           s               n   inz(*off)
     d error1          s               n   inz(*off)
     d process         s               n   inz(*off)

     d ##RtCd          s              1    inz
     d ##Co            s              3    inz
     d ##Wcf           s              3    inz
     d ##Sub           s              5    inz
     d ##Fnd           s              3    inz

     d isWCFl46        s               n   inz(*off)
     d isFL12          s              1    inz
     d xxSub#          s              3  0 inz


     d @@objname       s             10
     d @@objtype       s              7
     d @@err           s              1
     d dupflg          s               n   inz(*off)
     d i               s              1  0 inz
     d j               s              1  0 inz
     d outSysFlag      s              1    inz
     d Cnt1            C                   Const('Contact')
     d Cnt2            C                   Const('Attention')
     d Cnt3            C                   Const('Contact 3')
      ***  Begin Add  ***  05/10/23  *****************
     d t2              s              2  0 inz(*zeros)
     d t3              s              2  0 inz(*zeros)
     d* Array to store Tab index numbers, which is clicked by user before SAV
     d ClickTab        s              5  0 dim(4)
      ***  End   Add  ***  05/10/23  *****************
      *** Beg Add *** 05/23/23 **********************************************
       dcl-s  inDspFileName char(10);
       dcl-s  inPgmName     char(50);
       dcl-s  inTabName     char(50);
      *
     DarrTabNames      s            100    varying inz dim(20)
     dn1               s             10i 0 inz(1)
     dstrTabNames      s           2000    varying inz
      *** End Add *** 05/23/23 **********************************************
      //
     D ##Wcif          s              1
     D ##Mcif          s              1
     D ##Wcin          s              1
     d*** Beg Add *** 03/04/24 *************************************************
     d conStaf         c                   CONST('Staff Roles')
     d strSqlCursor    s           1000
     d strProcType     s            100
     d svAhlc          s             50
     d svAdj           s             50
     d*** End Add *** 03/04/24 *************************************************

     d                 ds
     d fq                      1      6    dim(3) ascend
     d  scFqc1                 1      2
     d  scFqc2                 3      4
     d  scFqc3                 5      6
     d*
     d                 DS
     d  scNewCover             1     97
     d  xxNewDept              1      5
     d  xxNewDoc               6     17
     d  xxNewFold             18     97
     d                 DS
     d  scRnwCover             1     97
     d  xxRnwDept              1      5
     d  xxRnwDoc               6     17
     d  xxRnwFold             18     97

      // Proto-types

      // Check Object
     d CheckObj        pr                  extpgm('SRCHKOBJ')
     d @@objname                     10
     d @@objtype                      7
     d @@err                          1

     d FA700R          pr                  extPgm('FA700R')
     d ##RtCd                         1
     d ##Co                           3
     d ##Wcf                          3
     d ##Sub                          5
     d ##Fnd                          3

      // Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3

     d GetSystem       pr                  extpgm('SA001R')
     d ##Wcif                         1
     d ##Mcif                         1
     d ##Wcin                         1

      * System Flag API
     d GetSystemFlag   pr                  extpgm('SA046R')
     d   outSysFlag                   1

      // initialize the screen fields for claim Darts Configuration
     d InzClmDrtScVal  pr

      * Initialize the Screen fields for Policy Darts Configuration
     d InzPolDrtScVal  pr

      // Load Admin/Darts Control info
     d LoadAdmInfo     pr

      // Load Audit Tracking Control info
     d LoadAudInfo     pr

      // load claim info
     d LoadClmInfo     pr

      // Load Endorsment/Darts Control info
     d LoadEndInfo     pr

      * Main Processing
     d Main            pr

      * Program Start
     d ProgramStart    pr
     d processMe                       n

     * Validate Admin Darts Info.
     d ValidAdmDrt     pr              n

     * Validate Claim Darts Info.
     d ValidClmDrt     pr              n


     d/copy sprUsrPrms
     d/copy sPrGenPop
     d/copy sPrDate
     d/copy sPrString
     d/copy sPrSql
     d/copy sPrMessage
     d/copy sPrModule
     d/copy sCopyCompr
     d/copy sprCompany
     d/copy sPrTable
     d/copy sPrTrigger
     d/copy sPrTrig
     d/copy wPrTable
     d/copy sPrAtrium
     d/copy wPrPolErr
     d/copy wPrGroup
      //*** Beg Add ***** 01/17/22 *****************************
     d/copy sCopyGrpPr
      //*** End Add ***** 01/17/22 *****************************
      *** Beg Add *** 05/23/23 *********************************
     d/copy sPrSecure
      *** End Add *** 05/23/23 *********************************


      // Entry of Program.

     c     *entry        plist
     c                   parm                    @@rrn             9
     c                   parm                    @@mode            1
     c                   parm                    @@next           10
     c                   parm                    @@TabInd          5

     c     keyStctl1     klist
     C                   kfld                    keyDrtCd          5

      /free
       If %parms >= 1;
          xxRrn = %int(@@rrn);
       Endif;

       Chain xxRrn pfn0p;
       inCo# = FNco#;
       inFnd = FNfnd;
       scCo# = FNco#;
       scFnd = FNfnd;

       GetClientID(clientID);
       scCltid = clientID;

       ProgramStart(process);

       If process;
          //*** Beg Add *** 05/23/23 ******************************************
          If TbNmRcdPnl <> *Blanks;
          //*** End Add *** 05/23/23 ******************************************

          Main();

          //*** Beg Add *** 05/23/23 ******************************************
          Else;
             LoadActionBar();
             isNoAccess = *on;
             TbNmRcdPnl = 'Security';
             pnlHeight = 560;
             aPnlHeight = 620;
             Write rcdGrpPnl;
             Exfmt rcdPanel;
             Read rcdGrpPnl;
             If scSelOpt <> *blanks;
                @@next = scSelOpt;
             Endif;
          Endif;
          //*** End Add *** 05/23/23 ******************************************
       Endif;


       *InLr = *On;
      /end-free

       //*** Beg Add *** 05/23/23 ******************************************
       //===================================================================
       // Add information to list
       //===================================================================
     PAddToList        b
     DAddToList        pi           101a   varying
     D inString                     100a   varying const
     D inArray                      100a   varying dim(20)
     D inIndex                       10i 0

     DoutString        s            101a   varying inz
      /Free
       If inIndex > 1;
          outString = ',';
       Endif;
       outString += inString;
       inArray(inIndex) = inString;
       inIndex += 1;
       Return outString;
      /End-free
     PAddToList        e
       //*** End Add *** 05/23/23 ******************************************

       //===================================================================
       // InzClmDrtScVal - Initialize Claim Info screen values
       //===================================================================
     P InzClmDrtScVal  B
     d InzClmDrtScVal  PI

      /free
       visDevf = *Off;
       visFl01 = *Off;
       visWcr = *Off;
       VisApv = *Off;
       visLgrh = *Off;
       visAprm = *Off;
       visPbme = *Off;
       visCdsm = *Off;
       visMedDry = *Off;
       visExnm  = *Off;
       isWCFl46 = *Off;
       isFL12 = ' ';

       scCo# = intCo#;
       scFnd = FNfnd;
       scCltid = clientID;

      // Set Selection criteria of Major Peril Code Default
       scMjrCH = 'GTco# = ' + %char(scCo#) + ' and GTTBID = ''MAJP''' +
       ' and GTstat <> ''I''';

      // Set Selection criteria of Loss Condition Code
       scLossCdCH = 'GTco# = ' + %char(scCo#) + ' and GTTBID = ''  AC''' +
       ' and GTstat <> ''I''';

      // Set Selection criteria of Loss Recovery
       scLsReCdCH = 'GTco# = ' + %char(scCo#) + ' and GTTBID = ''LSRV''' +
       ' and GTstat <> ''I''';

      // Set Selection criteria of Type of Coverage
       scCovtCH = 'GTco# = ' + %char(scCo#) + ' and GTTBID = ''  CT''' +
       ' and GTstat <> ''I''';

      // Set Selection criteria of Loss Coverage Code
       scLsCvCH = 'GTco# = ' + %char(scCo#) + ' and GTTBID = ''  LC''' +
       ' and GTstat <> ''I''';

      // Set Selection criteria of Diary User (3 fields in CMS Medicare Section)
       scUserCH = 'GTco# = ' + %char(scCo#) + ' and GTTBID = ''AXDU''' +
       ' and GTstat <> ''I''';


       // Get Flags value from Workers Comp control file
       Chain 'WC' SWC01;
       If %found();
          // Set visibility for Claim diary access summary screen
          If WCfl33 = 'Y';
             visCdsm = *On;
          Endif;
          If WCfl46 = 'Y';
             isWCFl46 = *On;
          Endif;
       Endif;

       // Get value from system file
       Chain 'SY' SSY01;
       If %found();
          isFL12 = %subst(SYfl12:2:1);
       Endif;

       // Set visibility for CMS Medicare diary section
       If isFL12 = 'Y' and isWCFL46 = *On;
          visMedDry = *on;
       EndIf;

       // Set Visibility for Exclude Group From Claims Export
       // Set Visibility for Devereux Group
       If clientID = '114';
          visFl01 = *On;
          visDevf = *On;
       EndIf;

       // Set Visibility for Wage Calculation Review DARTS Diary
       // Set Visibility for Attorney Order PV1 Codes (5 fields)
       If clientID = '025';
          visWcr = *On;
          visApv = *On;
       EndIf;

       // Set Visibility for Heading for Legal Reserve
       // Set Visibility for Apportionment Remark Code
       If clientID = '129';
          visLgrh = *On;
          visAprm = *On;
       EndIf;

       // Set visibilty for PBM Eligible (7 fields)
       If clientID = '090';
          visPbme = *On;
       EndIf;

       // Set visibilty for Exclude from NCCI Medical Data Call
       Read sfb02;
       If Not %eof(sfbpf);
          If %Subst(FBst07:1:1) <> 'Y';
             visExnm = *off;  // not visible
          Else;
             visExnm = *on;   // visible
          Endif;
       Else;
          visExnm = *on;   // visible
       Endif;

       //=====================Below visibility codes need to be removed=====
       // visDevf = *On;
       // visFl01 = *On;
       // visWcr = *On;
       // VisApv = *On;
       // visLgrh = *On;
       // visAprm = *On;
       // visPbme = *On;
       // visCdsm = *On;
       // visMedDry = *On;
       // visExnm  = *On;
       //=====================Above visibility codes need to be removed=====
      /end-free
     P InzClmDrtScVal  E

       //===================================================================
       // InzPolDrtScVal - Initialize the Screen fields and indicators
       //===================================================================
     p InzPolDrtScVal  b
     d InzPolDrtScVal  pi

      /Free
       visNotify = *off;
       visFirm = *off;
       visApcd = *off;
       visBrlt = *off;
       visLdal = *off;
       visEcnt = *off;
       visUpda = *off;

       scCo# = intCo#;
       scFnd = FNfnd;
       scCltid = clientID;

      // Set selection criteria for Default Office #
        scDoffCH = 'OFco# = ' + %EditC(scCo#:'X') + ' and ' +
         'OFstat<>' + '''I''';

      // Set selection criteria for Default Region #
        scDrgnCH = 'REco# = ' + %EditC(scCo#:'X') + ' and ' +
           'REfnd = ' + %EditC(scFnd:'X') + ' and ' + 'REstat <> ' + '''I''';

      // Set selection criteria for FastQuote Cross-Reference
        scFqcCH = 'GTco# = ' + %EditC(scCo#:'X') + ' and '  +
           'GTtbid =  ' + '''FQCT'''+' and '+ 'GTstat <> ' + '''I''';

      // Set selection criteria for Field # 1 Generic Table ID
        scFlxtCH = 'GIco# = ' + %EditC(scCo#:'X') + ' and ' +
         'GIstat <> ' + '''I''';

      // Set visibilty indicators
       GetSystemFlag(outSysFlag);
       If outSysFlag  = 'Y';
          visNotify = *on;
       Endif;
       //If FNpeof = 'M';
       //    visFirm = *on;
       //Endif;
       If FNpeof <> ' ';
          visApcd = *on;
       Endif;

       If clientID = '129';
          visBrlt = *on;
       Endif;


       Chain 'WC' swc01;
       If %found(ssylf);
          If wcfl43 = 'Y';
            visLdal = *on;
          Endif;
          If wcfl44 = 'Y';
             visFirm = *on;
          Endif;
          If WCfl09 = 'Y';
            visEcnt = *on;
          Endif;
          If WCFL41 = 'Y';
             visUpda = *on;                   // set Visibility for this checkbox
            // Set Label for Checkbox scUpda
             If fnpeof = 'M' or fnpeof = 'C';
                lblUpda = 'Prompt for Changes Across All Policies/Divisions';
             Else;
                lblUpda = 'Prompt for Changes Across All Divisions';
             Endif;
          Endif;
       Endif;


       //These lines of visibility indicators below needs to be removed
       // visNotify = *on;
       // visFirm = *on;
       // visApcd = *on;
       // visBrlt = *on;
       // visLdal = *on;
       // visEcnt = *on;
       // visUpda = *on;
       //These lines of visibility indicators above needs to be removed
      /End-Free
     p InzPolDrtScVal  e

       //===================================================================
       // Load Audit Tracking Control Info
       //===================================================================
     p LoadAudInfo     b
     d LoadAudInfo     pi

      /free

       Chain(n) (inCo#:inFnd) pax00;
       If %found(wmaxl);
          scAacd  = AXaacd;                // AUDIT ATT DARTS CODE
          scFmcd  = AXfmcd;                // FIN AUD MAILED DARTS CD
          scAccd  = AXaccd;                // AUDIT CONTESTED CODE
          scRccd  = AXrccd;                // FINAL AUD RCVD DARTS CD
          scCccd  = AXcccd;                // CLR CNTSTD INFO DARTS CD
          scRear  = AXrear;                // REASSIGN  AUDIT REP
          scIadc  = AXiadc;                // INIT AUD DARTS CODE
          scRadc  = AXradc;                // REV AUD DARTS CODE
          scDasc  = AXdasc;                // DFLT AUD STAF CODE

          saveDT3 = AXdt;
          saveTM3 = AXtime;
       Endif;

      /end-free
     p LoadAudInfo     e

       //===================================================================
       // LoadClmInfo - Load Claim Info screen values
       //===================================================================
     P LoadClmInfo     B
     d LoadClmInfo     PI

      /free

      // Claim/ Darts Control Info
       Chain(n) (inCo#:inFnd) pcx00;
       If %found(wmcxl);

          // Security section

          scSecl = CXsecl;
          scCdt  = CXcdt ;
          scCdt2 = CXcdt2;
          scCdt3 = CXcdt3;
          scCdt4 = CXcdt4;
          scCdt5 = CXcdt5;

          scSecc = CXsecc;
          scOcat = CXocat;
          scOca2 = CXoca2;
          scOca3 = CXoca3;
          scOca4 = CXoca4;
          scOca5 = CXoca5;

          scCkdL = CXckdl;
          scCkdT = CXckdt;
          scCkd2 = CXckd2;
          scCkd3 = CXckd3;
          scCkd4 = CXckd4;
          scCkd5 = CXckd5;

          scDltd = CXdltd;
          scDppt = CXdppt;
          scDpp2 = CXdpp2;
          scDpp3 = CXdpp3;
          scDpp4 = CXdpp4;
          scDpp5 = CXdpp5;

          scPyod = CXpyod;
          scPdct = CXpdct;
          scPdc2 = CXpdc2;
          scPdc3 = CXpdc3;
          scPdc4 = CXpdc4;
          scPdc5 = CXpdc5;

          scVszl = CXvszl;
          scVszt = CXvszt;
          scVsz2 = CXvsz2;
          scVsz3 = CXvsz3;
          scVsz4 = CXvsz4;
          scVsz5 = CXvsz5;

          scFsvl = CXfsvl;
          scFsvt = CXfsvt;
          scFsv2 = CXfsv2;
          scFsv3 = CXfsv3;
          scFsv4 = CXfsv4;
          scFsv5 = CXfsv5;

          scAusl = CXausl;
          scAut1 = CXaut1;
          scAut2 = CXaut2;
          scAut3 = CXaut3;
          scAut4 = CXaut4;
          scAut5 = CXaut5;

          scDslv = CXdslv;
          scDs01 = CXds01;
          scDs02 = CXds02;
          scDs03 = CXds03;
          scDs04 = CXds04;
          scDs05 = CXds05;

          scDplv = CXdplv;
          scDpt1 = CXdpt1;
          scDpt2 = CXdpt2;
          scDpt3 = CXdpt3;
          scDpt4 = CXdpt4;
          scDpt5 = CXdpt5;

          scDpl2 = CXdpl2;
          scDpt6 = CXdpt6;
          scDpt7 = CXdpt7;
          scDpt8 = CXdpt8;
          scDpt9 = CXdpt9;
          scDpt0 = CXdpt0;

          scMbs  = CXmbs;
          scMbs1 = CXmbs1;
          scMbs2 = CXmbs2;
          scMbs3 = CXmbs3;
          scMbs4 = CXmbs4;
          scMbs5 = CXmbs5;

          scRbs  = CXrbs;
          scRbs1 = CXrbs1;
          scRbs2 = CXrbs2;
          scRbs3 = CXrbs3;
          scRbs4 = CXrbs4;
          scRbs5 = CXrbs5;

          scPbmel = CXpbmel;
          scPbme1 = CXpbme1;
          scPbme2 = CXpbme2;
          scPbme3 = CXpbme3;
          scPbme4 = CXpbme4;
          scPbme5 = CXpbme5;

          // Miscellaneous section

          scLoct = CXloct;
          scPmts = CXpmts;
          scPsc2 = CXpsc2;
          scAccp = CXaccp;
          scRt01 = CXrt01;
          scRt02 = CXrt02;
          scRt03 = CXrt03;
          scRt04 = CXrt04;
          scMjr  = CXmjr;
          scCat2 = CXcat2;
          scAdj  = CXadj;
          //*** Beg Add *** 03/04/24 *******************************************
          svAdj  = CXadj;
          //*** End Add *** 03/04/24 *******************************************
          scRchg = CXrchg;
          scRlbl = CXrlbl;
          scDppc = CXdppc;
          scEdim = CXedim;
          scLgrh = CXlgrh;

          // Automatic DARTS Remark Creation section

          scRffa = CXrffa;
          scRtfa = CXrtfa;
          scRcnc = CXrcnc;
          scRopn = CXropn;
          scRody = CXrody;
          scDsrc = CXdsrc;
          scNmad = CXnmad;
          scAadp = CXaadp;
          scAadr = CXaadr;
          scWcr  = CXwcr;
          scCapd = CXcapd;

          // Benefit Diary Default (creation of a claim) section

          scRads = CXrads;
          scBadj = CXbadj;
          scRcad = CXrcad;
          scRsrd = CXrsrd;
          scRssd = CXrssd;
          scBspv = CXbspv;
          scRumd = CXrumd;
          scBmgr = CXbmgr;
          scCdsm = CXcdsm;
          scDevf = CXdevf;
          scFl01 = CXfl01;
          scAprm = CXaprm;

          // Claim Entry Screens to Appear in Add Mode section

          scClminst = CXclmInst;
          If scClminst = ' ';
             scClminst = 'N';
          EndIf;
          scMedi = CXmedi;
          scBeni = CXbeni;
          scRps  = CXrps;
          scStsp = CXstsp;
          scDben = CXdben;
          scDart = CXdart;
      //*** Beg Add ***** 08/29/22 *****************************
          scPFro = CXPrtFROI;
      //*** End Add ***** 08/29/22 *****************************

          // Auto Claim Closing Criteria section

          scMoc = CXmoc;
          scAccm = CXaccm;
          scNfam = CXnfam;
          scAdrm = CXadrm;
          scInc  = CXinc;
          scAcci = CXacci;
          scNfai = CXnfai;
          scAdri = CXadri;

          // CMS Medicare Diaries section

          scCerd = CXcerd;
          scCedu = CXcedu;
          scCesu = CXcesu;

          scQrsd = CXqrsd;
          scQrdu = CXqrdu;
          scQrsu = CXqrsu;

          scMbqf = CXmbqf;
          scRcsc = CXrcsc;
          scRcdu = CXrcdu;
          scRcsu = CXrcsu;

          // Attorney order PV1 Codes/section

          scApv1 = CXapv1;
          scApv2 = CXapv2;
          scApv3 = CXapv3;
          scApv4 = CXapv4;
          scApv5 = CXapv5;

          // Miscellaneous continued section

          scOpcl = CXopcl;
          scDohr = CXdohr;
          scDobr = CXdobr;
          scGdrr = CXgdrr;
          scExnm = CXexnm;
          scLsrv = CXlsrv;
          scWmth = CXwmth;
          If CXawwr = ' ';
             scAwwr = 'Y';
          Else;
             scAwwr = CXawwr;
          EndIf;
          If CXpbme = *blanks;
             scPbme = 'Y';
          Else;
             scPbme  = CXpbme;
          EndIf;
          scCvt# = CXcvt#;

          saveDT4 = CXdt;
          saveTM4 = CXtime;

       EndIf;

      // Claim/ Darts Extension Control Info
       Chain(n) (inCo#:inFnd) pcx02;
       If %found(wmcxp2);

          // Claim Add Default Values

          scLossCd = x2_CXlossCd;
          scLsReCd = x2_CXlsReCd;
          scCovt = x2_CXcovt;
          scLsCv = x2_CXlsCv;
          scLoadAww = x2_CXloadAww;

          saveDT5 = x2_CXdt;
          saveTM5 = x2_CXtime;

       EndIf;

      /end-free
     P LoadClmInfo     E

       //===================================================================
       // Load Admin/Darts Control info
       //===================================================================
     p LoadAdmInfo     b
     d LoadAdmInfo     pi

      /free

       Chain(n) (inCo#:inFnd) pex00;
       If %found(wmexl);
          scDltl  = EXdltl;
          scDltt  = EXdltt;
          //*** Beg Add ***** 07/03/23 ***********************
          scDltLQt = EXdltLQt;
          scDltTQt = EXdltTQt;
          //*** End Add ***** 07/03/23 ***********************
          scOvrl  = EXovrl;
          scOvrt  = EXovrt;
          scUpdl  = EXupdl;
          scUpdt  = EXupdt;
          scSubl  = EXsubl;
          scSubt  = EXsubt;
          scPdcl  = EXpdcl;
          scPdct  = EXpdct;
          scAdll  = EXadll;
          scAdlt  = EXadlt;
          scAdql  = EXadql;
          scAdqt  = EXadqt;
          scPlnl  = EXplnl;
          scPlnt  = EXplnt;
          scPrsl  = EXprsl;
          scPrst  = EXprst;
          scLdal  = EXldal;
          scLdat  = EXldat;
          scQtil  = EXqtil;
          scQtit  = EXqtit;
          scQtbl  = EXqtbl;
          scQtbt  = EXqtbt;
          scDlar  = EXdlar;
          scDrgn  = EXdrgn;
          scDoff  = EXdoff;
          scQtit  = EXqtit;
          scEdfl  = EXedfl;
          scRafl  = EXrafl;
          scAafl  = EXaafl;
          scTrfl  = EXtrfl;
          sccxstrp= EXcxstrp;
          scAhdr  = EXahdr;
          scA2fl  = EXa2fl;
          If Excnt1 = *blanks;
             ScCnt1 = Cnt1;
          Else;
             scCnt1 = EXcnt1;
          Endif;
          If Excnt2 = *blanks;
             ScCnt2 = Cnt2;
          Else;
             scCnt2 = EXcnt2;
          Endif;
          If Excnt3 = *blanks;
             ScCnt3 = Cnt3;
          Else;
             scCnt3 = EXcnt3;
          Endif;
          scFqc1  = EXfqc1;
          scFqc2  = EXfqc2;
          scFqc3  = EXfqc3;
          scEcnt  = EXecnt;
          scSicr  = EXsicr;
          ScCust  = EXcust;
          scFifg  = Exfifg;
          scReqLnkCd = EXreqlnkcd;
          scPrtl  = EXprtl;
          scSrfl  = EXsrfl;
          scPppc  = EXpppc;
          scMout  = EXmout;
          scAout  = EXaout;
          scSout  = EXsout;
          scCroq  = EXcroq;
          scOfrm  = EXofrm;
          scLnrm  = EXlnrm;
          scPnrm  = EXpnrm;
          scAurq  = EXaurq;
          scCxcd  = EXcxcd;
          scRccd  = EXrccd;
          scRecd  = EXrecd;
          scRrcd  = EXrrcd;
          scPpcd  = EXppcd;
          scMicd  = EXmicd;
          scPldr  = EXpldr;
          scAudl  = EXaudl;
          scRvrn  = EXrvrn;
          scCndr  = EXcndr;
          scCnst  = EXcnst;
          scBrlt  = EXbrlt;
          scAsrm  = EXasrm;
          scDdrm  = EXddrm;
          scCicd  = EXcicd;
          //*** Beg Chg *** 10/16/24 *******************************************
          //*** scAacd  = EXaacd;
          scA1cd  = EXaacd;
          //*** End Chg *** 10/16/24 *******************************************
          scAa#m  = EXaa#m;
          scFirm  = EXfirm;
          scApcd  = EXapcd;
          scAcsr  = EXacsr;
          scAcord = EXacord;
          scDocUplod = EXdocuplod;
          scAhlc  = EXahlc;
          //*** Beg Add *** 03/04/24 *******************************************
          svAhlc  = EXahlc;
          //*** End Add *** 03/04/24 *******************************************
          scAhwl  = EXahwl;
          scAhwl  = EXahwl;
          scAhvp  = EXahvp;
          scAhsa  = EXahsa;
          scUpda = EXupda;
          scFlxh  = EXflxh;
          scFlxt  = EXflxt;
          scNewCover = EXncdp + EXncdc + EXncfd;
          scRnwCover = EXrcdp + EXrcdc + EXrcfd;
         //*** beg add ***** 03/02/22 *********************
          scAcSrCSV = EXacSrCSV;
         //*** end add ***** 03/02/22 *********************
          saveDT = EXdt;
          saveTM = EXtime;
        Endif;

      /end-free
     p LoadAdmInfo     e

       //===================================================================
       // Load Endors/Darts Control Info
       //===================================================================
     p LoadEndInfo     b
     d LoadEndInfo     pi

      /free

       Chain(n) (inCo#:inFnd) pdx00;
       If %found(wmdxl);
          scEmnc  = DXemnc;
          scEmma  = DXemma;
          scEmpa  = DXempa;
          scEmfi  = DXemfi;
          scPanc  = DXpanc;
          scEmls  = DXemls;
          scDvad  = DXdvad;
          scDvdl  = DXdvdl;
          scPexm  = DXpexm;
          scPinl  = DXpinl;
          scPcro  = DXpcro;
          scPiap  = DXpiap;
          scCexm  = DXcexm;
          scEnad  = DXenad;
          scEndl  = DXendl;
          scEnnc  = DXennc;
          scEnma  = DXenma;
          scEnpa  = DXenpa;
          scLcad  = DXlcad;
          scLcdl  = DXlcdl;
          scLcnc  = DXlcnc;
          scLcma  = DXlcma;
          scLcpa  = DXlcpa;
          scOoad  = DXooad;
          scOonc  = DXoonc;
          scWvar  = DXwvar;
          scWvcr  = DXwvcr;
          scWvdr  = DXWvdr;
          scCert  = DXCert;
          scMisc  = DXMisc;

          saveDT2 = DXdt;
          saveTM2 = DXtime;
       Endif;

      /end-free
     p LoadEndInfo     e

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi
     d*
     d  isexit         s               n   Inz(*off)
      /free

       //LoadActionBar();

       BtnCancel = *off;

       // Process until exit button is pressed.
       DoW BtnCancel = *Off;

          // display screen
          Exsr ExfmtScreens;

          error = *off;
          error1 = *off;

          // Perform actions on screen.
          Select;
          // do nothing - catch tab click event
          When TabClick = *on;
             TabClick = *off;

          // Refresh
          When btnRefresh = *On;
             //***  Begin Del  ***  05/23/24  *****************
             //***  btnRefresh = *Off;
             //***  End   Del  ***  05/23/24  *****************

          // (Enter key or Save button)
          When btnSave = *on;
             btnSave = *off;
             If @@mode <> 'D';
                // Check if WMEXP updated since screen displayed (Admin/Darts)
                flgRcdUpdated = RcdUpdatedErr('WMEXP':dbrrn:'EXdt':'EXtime':
                 'EXuser':saveDT:saveTM);
                if flgRcdUpdated = '0'; // No error
                   // Check if WMDXP updated since screen displayed (Endors/Darts)
                   flgRcdUpdated = RcdUpdatedErr('WMDXP':dbrrn2:'DXdt':'DXtime':
                    'DXuser':saveDT2:saveTM2);
                EndIf;
                if flgRcdUpdated = '0'; // No error
                   // Check if WMAXP updated since screen displayed (Audit Track)
                   flgRcdUpdated = RcdUpdatedErr('WMAXP':dbrrn3:'AXdt':
                    'AXtime':'AXuser':saveDT3:saveTM3);
                EndIf;
                if flgRcdUpdated = '0' and saveDT4 <> 0; // No error
                   // Check if WMCXP updated since screen displayed (Claim/DARTS)
                   flgRcdUpdated = RcdUpdatedErr('WMCXP':dbrrn4:'CXdt':
                    'CXtime':'CXuser':saveDT4:saveTM4);
                EndIf;
                if flgRcdUpdated = '0' and saveDT5 <> 0; // No error
                   // Check if WMCXP2 updated since screen displayed (Claim/DARTS 2)
                   flgRcdUpdated=RcdUpdatedErr('WMCXP2':dbrrn5:'CXdt':
                    'CXtime':'CXuser':saveDT5:saveTM5);
                EndIf;

                if flgRcdUpdated = '0'; // No error

                   // Validate Admin Darts Info (Tab 1)
                   If ValidAdmDrt();
                      Exsr UpdPolInfo;
                   Else;
                      error = *on;
                   EndIf;
                   //***  Begin Add  ***  08/06/24  *****************
                   // Validate only if user has visited Tab 2
                   t3 = 0;
                   t3 = %lookup(2:ClickTab);
                   If t3 <> 0;
                   //***  End   Add  ***  08/06/24  *****************
                   // Validate Claim Darts Info (Tab 2)
                   If ValidClmDrt();
                      Exsr UpdClmInfo;
                   Else;
                      error1 = *on;
                   EndIf;
                   //***  Begin Add  ***  08/06/24  *****************
                   Endif;
                   //***  End   Add  ***  08/06/24  *****************

                Else;
                   error = *on;
                   error1 = *on;
                EndIf;

                // Show Data Saved Sucessfully Message
                If error = *off and error1 = *off;
                   PopDataSaved();

                   //*** Beg Add ***** 05/10/23 *****************************
                   Clear ClickTab;
                   Clear t2;
                   //*** End Add ***** 05/10/23 *****************************
                EndIf;
             Endif;

          //*** Beg Add ***** 01/17/22 *******************************
          // Copy Configuration
          When btnShowWin = *on;
             PopupWindow(txtShowWin:scActRRN:'GS':txtWinData);
             btnShowWin = *off;
          //*** End Add ***** 01/17/22 *******************************

          //*** Beg Add *** 05/23/23 ******************************************
          // Add User Access Window Logic
          When btnUsrAcc = *on;
             inDspFileName = 'WTGRPADMIN';
             inPgmName     = 'Group Administration / DARTS Configuration';
             inTabName     =  arrTabNames(TabIndex + 1);
             UserAccessWdw(inDspFileName:inPgmName:inTabName);
             btnUsrAcc = *off;
          //*** End Add *** 05/23/23 ******************************************

          // Menu Choices
          When scSelOpt <> *blanks;
             @@next = scSelOpt;
             Leave;

          Endsl;
       Enddo;

       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       indexTab = TabIndex + 1;

       Select;
       //*** Beg Chg *** 05/23/23 ******************************************
       //*** When indexTab = 1;
       When arrTabNames(TabIndex+1) = 'Policy';
       //*** End Chg *** 05/23/23 ******************************************
          //*** Beg Chg ***** 07/03/23 ***********************
          //*** pnlHeight = 2400;
          pnlHeight = 2425;
          //*** Beg Chg ***** 07/03/23 ***********************
          APnlHeight = pnlHeight + 60;

          //***  Begin Add  ***  05/10/23  *****************
          t3 = 0;
          t3 = %lookup(indexTab:ClickTab);
          //***  Begin Chg  ***  05/23/24  *****************
          //***  If t3 = 0;
          //***     t2 = t2 + 1;
          //***     ClickTab(t2) = indexTab;
          If t3 = 0 or btnRefresh = *On;
             btnRefresh = *Off;
             If t3 = 0;
                t2 = t2 + 1;
                ClickTab(t2) = indexTab;
             EndIf;
          //***  End   Chg  ***  05/23/24  *****************
          //***  End   Add  ***  05/10/23  *****************

          If error = *off;
             Clear rcdPolInf;

            // Loading initial values
             InzPolDrtScVal();

            // Load Admin/Darts Control Info
             LoadAdmInfo();

            // Load Endors/Darts Control Info
             LoadEndInfo();

            // Load Audit Tracking Control Info
             LoadAudInfo();
          Endif;

          //***  Begin Add  ***  05/10/23  *****************
          Endif;
          //***  End   Add  ***  05/10/23  *****************

          If dspMode;
             inquiry = *on;
          Endif;

          Write rcdGrpPnl;
          Write rcdPanel;
          Exfmt rcdPolInf;
          Read  rcdGrpPnl;
          Read rcdPanel;

       //*** Beg Chg *** 05/23/23 ******************************************
       //*** When indexTab = 2;
       When arrTabNames(TabIndex+1) = 'Claim';
       //*** End Chg *** 05/23/23 ******************************************
          pnlHeight = 1695;
          APnlHeight = pnlHeight + 60;

          //***  Begin Add  ***  05/10/23  *****************
          t3 = 0;
          t3 = %lookup(indexTab:ClickTab);
          //***  Begin Chg  ***  05/23/24  *****************
          //***  If t3 = 0;
          //***     t2 = t2 + 1;
          //***     ClickTab(t2) = indexTab;
          If t3 = 0 or btnRefresh = *On;
             btnRefresh = *Off;
             If t3 = 0;
                t2 = t2 + 1;
                ClickTab(t2) = indexTab;
             EndIf;
          //***  End   Chg  ***  05/23/24  *****************
          //***  End   Add  ***  05/10/23  *****************

          If error1 = *off;
             Clear rcdClmInf;

             // Load initial values
             InzClmDrtScVal();

             // Set top position of CMS Medicare Diaries (section)
             topMedDry = 1400;
             If VisApv = *on;
                // Attorney Order PV1 Codes (section) is present for client 025
                topMedDry = 1485;
             Endif;

             // Load initial values
             LoadClmInfo();
          Endif;

          //***  Begin Add  ***  05/10/23  *****************
          Endif;
          //***  End   Add  ***  05/10/23  *****************

          If dspMode;
             inquiry_1 = *on;
          Endif;

          Write rcdGrpPnl;
          Write rcdPanel;
          Exfmt rcdClmInf;
          Read  rcdGrpPnl;
          Read rcdPanel;

       EndSl;

       EndSr;

       // Subroutine to move Screen fields into database fields.
       BegSr MoveScDb;
        EXdltl  = scDltl;
        EXdltt  = scDltt;
        //*** Beg Add ***** 07/03/23 ***********************
        EXdltLQt = scDltLQt;
        EXdltTQt = scDltTQt;
        //*** End Add ***** 07/03/23 ***********************
        EXovrl  = scOvrl;
        EXovrt  = scOvrt;
        EXupdl  = scUpdl;
        EXupdt  = scUpdt;
        EXsubl  = scSubl;
        EXsubt  = scSubt;
        EXpdcl  = scPdcl;
        EXpdct  = scPdct;
        EXadll  = scAdll;
        EXadlt  = scAdlt;
        EXadql  = scAdql;
        EXadqt  = scAdqt;
        EXplnl  = scPlnl;
        EXplnt  = scPlnt;
        EXprsl  = scPrsl;
        EXprst  = scPrst;
        EXldal  = scLdal;
        EXldat  = scLdat;
        EXqtil  = scQtil;
        EXqtit  = scQtit;
        EXqtbl  = scQtbl;
        EXqtbt  = scQtbt;
        EXdlar  = scDlar;
        EXdrgn  = scDrgn;
        EXdoff  = scDoff;
        EXqtit  = scQtit;
        EXedfl  = scEdfl;
        EXrafl  = scRafl;
        EXaafl  = scAafl;
        EXtrfl  = scTrfl;
        EXcxstrp= scCxStrp;
        EXahdr  = scAhdr;
        EXa2fl  = scA2fl;
        EXcnt1  = scCnt1;
        EXcnt2  = scCnt2;
        EXcnt3  = scCnt3;
        EXfqc1  = scFqc1;
        EXfqc2  = scFqc2;
        EXfqc3  = scFqc3;
        EXecnt  = scEcnt;
        EXsicr  = scSicr;
        Excust  = Sccust;
        Exfifg  = scFifg;
        EXreqlnkcd = scReqLnkCd;
        EXprtl  = scPrtl;
        EXsrfl  = scSrfl;
        EXpppc  = scPppc;
        EXmout  = scMout;
        EXaout  = scAout;
        EXsout  = scSout;
        EXcroq  = scCroq;
        EXofrm  = scOfrm;
        EXlnrm  = scLnrm;
        EXpnrm  = scPnrm;
        EXaurq  = scAurq;
        EXcxcd  = scCxcd;
        EXrccd  = scRccd;
        EXrecd  = scRecd;
        EXrrcd  = scRrcd;
        EXppcd  = scPpcd;
        EXmicd  = scMicd;
        EXpldr  = scPldr;
        EXaudl  = scAudl;
        EXrvrn  = scRvrn;
        EXcndr  = scCndr;
        EXcnst  = scCnst;
        EXbrlt  = scBrlt;
        EXasrm  = scAsrm;
        EXddrm  = scDdrm;
        EXcicd  = scCicd;
        //*** Beg Chg *** 10/16/24 *******************************************
        //*** EXaacd  = scAacd;
        EXaacd  = scA1cd;
        //*** End Chg *** 10/16/24 *******************************************
        EXaa#m  = scAa#m;
        EXfirm  = scFirm;
        EXapcd  = scApcd;
        EXacsr  = scAcsr;
        EXacord = scAcord;
        EXdocuplod = scDocUplod;
        EXahlc  = scAhlc;
        //*** Beg Add *** 03/04/24 *******************************************
        //Update stsppcrt1 file if scahlc is change and old value found in stsppcrt1
        If svAhlc <> scAhlc;
           strSqlCursor = 'Select Distinct SPtype from STSPP  +
                           where SPCAT = ''PS'' and SPtype <> '' '' +
                           ORDER BY SPtype ASC';

           Exec Sql Prepare sqlDB1 From :strSqlCursor;
           Exec Sql Declare PolServices Cursor For sqlDB1;
           Exec Sql Open PolServices;
           Dow sqlStt = '00000';
              Exec Sql Fetch Next From PolServices Into :strProcType;
              If sqlStt = '00000';
                 chain (strProcType:conStaf:svAhlc) ps10p;
                 If %found(stsppcrt1);
                    S1critVal = scAhlc;
                    S1pgm  = q1Pgm;
                    S1user = q1User;
                    S1time = Timeto6(SystemTime());
                    S1dt   = Dateto7(SystemDate());
                    update ps10p;
                 Endif;
              Endif;
           Enddo;
           Exec Sql Close PolServices;
        Endif;
        //*** End Add *** 03/04/24 *******************************************
        EXahwl  = scAhwl;
        EXahwl  = scAhwl;
        EXahvp  = scAhvp;
        EXahsa  = scAhsa;
        EXupda  = scUpda;
        EXflxh  = scFlxh;
        Exflxt  = scFlxt;
        EXncdp  = xxNewDept;
        EXncdc  = xxNewDoc ;
        EXncfd  = xxNewFold;
        EXrcdp  = xxRnwDept;
        EXrcdc  = xxRnwDoc ;
        EXrcfd  = xxRnwFold;
       //*** beg add ***** 03/02/22 *********************
        EXacSrCSV = scacSrCSV;
       //*** end add ***** 03/02/22 *********************
       EndSr;

       // Subroutine to move Screen fields into database fields.
       BegSr MoveScDb2;
        DXemnc  = scEmnc;
        DXemma  = scEmma;
        DXempa  = scEmpa;
        DXemfi  = scEmfi;
        DXpanc  = scPanc;
        DXemls  = scEmls;
        DXdvad  = scDvad;
        DXdvdl  = scDvdl;
        DXpexm  = scPexm;
        DXpinl  = scPinl;
        DXpcro  = scPcro;
        DXpiap  = scPiap;
        DXcexm  = scCexm;
        DXenad  = scEnad;
        DXendl  = scEndl;
        DXennc  = scEnnc;
        DXenma  = scEnma;
        DXenpa  = scEnpa;
        DXlcad  = scLcad;
        DXLcdl  = scLcdl;
        DXlcnc  = scLcnc;
        DXlcma  = scLcma;
        DXlcpa  = scLcpa;
        DXooad  = scOoad;
        DXoonc  = scOonc;
        DXwvar  = scWvar;
        DXwvcr  = scWvcr;
        DXwvdr  = scWvdr;
        DXcert  = scCert;
        DXmisc  = scMisc;
       EndSr;

       // Subroutine to move Screen fields into database fields.
       BegSr MoveScDb3;
        AXaacd  = scAacd;                // AUDIT ATT DARTS CODE
        AXfmcd  = scFmcd;                // FIN AUD MAILED DARTS CD
        AXaccd  = scAccd;                // AUDIT CONTESTED CODE
        AXrccd  = scRccd;                // FINAL AUD RCVD DARTS CD
        AXcccd  = scCccd;                // CLR CNTSTD INFO DARTS CD
        AXrear  = scRear;                // REASSIGN  AUDIT REP
        AXiadc  = scIadc;                // INIT AUD DARTS CODE
        AXradc  = scRadc;                // REV AUD DARTS CODE
        AXdasc  = scDasc;                // DFLT AUD STAF CODE
       EndSr;

       // Subroutine to move Screen fields into database fields.
       BegSr MoveScDb4;
        // Security section

        CXsecl = scSecl;
        CXcdt  = scCdt;
        CXcdt2 = scCdt2;
        CXcdt3 = scCdt3;
        CXcdt4 = scCdt4;
        CXcdt5 = scCdt5;

        CXsecc = scSecc;
        CXocat = scOcat;
        CXoca2 = scOca2;
        CXoca3 = scOca3;
        CXoca4 = scOca4;
        CXoca5 = scOca5;

        CXckdl = scCkdL;
        CXckdt = scCkdt;
        CXckd2 = scCkd2;
        CXckd3 = scCkd3;
        CXckd4 = scCkd4;
        CXckd5 = scCkd5;

        CXdltd = scDltd;
        CXdppt = scDppt;
        CXdpp2 = scDpp2;
        CXdpp3 = scDpp3;
        CXdpp4 = scDpp4;
        CXdpp5 =  scDpp5;

        CXpyod = scPyod;
        CXpdct = scPdct;
        CXpdc2 = scPdc2;
        CXpdc3 = scPdc3;
        CXpdc4 = scPdc4;
        CXpdc5 = scPdc5;

        CXvszl = scVszl;
        CXvszt = scVszt;
        CXvsz2 = scVsz2;
        CXvsz3 = scVsz3;
        CXvsz4 = scVsz4;
        CXvsz5 = scVsz5;

        CXfsvl = scFsvl;
        CXfsvt = scFsvt;
        CXfsv2 = scFsv2;
        CXfsv3 = scFsv3;
        CXfsv4 = scFsv4;
        CXfsv5 = scFsv5;

        CXausl = scAusl;
        CXaut1 = scAut1;
        CXaut2 = scAut2;
        CXaut3 = scAut3;
        CXaut4 = scAut4;
        CXaut5 = scAut5;

        CXdslv = scDslv;
        CXds01 = scDs01;
        CXds02 = scDs02;
        CXds03 = scDs03;
        CXds04 = scDs04;
        CXds05 = scDs05;

        CXdplv = scDplv;
        CXdpt1 = scDpt1;
        CXdpt2 = scDpt2;
        CXdpt3 = scDpt3;
        CXdpt4 = scDpt4;
        CXdpt5 = scDpt5;

        CXdpl2 = scDpl2;
        CXdpt6 = scDpt6;
        CXdpt7 = scDpt7;
        CXdpt8 = scDpt8;
        CXdpt9 = scDpt9;
        CXdpt0 = scDpt0;

        CXmbs  = scMbs ;
        CXmbs1 = scMbs1;
        CXmbs2 = scMbs2;
        CXmbs3 = scMbs3;
        CXmbs4 = scMbs4;
        CXmbs5 = scMbs5;

        CXrbs  = scRbs ;
        CXrbs1 = scRbs1;
        CXrbs2 = scRbs2;
        CXrbs3 = scRbs3;
        CXrbs4 = scRbs4;
        CXrbs5 = scRbs5;

        CXpbmel = scPbmel;
        CXpbme1 = scPbme1;
        CXpbme2 = scPbme2;
        CXpbme3 = scPbme3;
        CXpbme4 = scPbme4;
        CXpbme5 = scPbme5;

         // Miscellaneous section

        CXloct = scLoct;
        CXpmts = scPmts;
        CXpsc2 = scPsc2;
        CXaccp = scAccp;
        CXRT01 = scRt01;
        CXRT02 = scRt02;
        CXRT03 = scRt03;
        CXRT04 = scRt04;
        CXmjr  = scMjr;
        CXcat2 = scCat2;
        CXadj  = scAdj;
        //*** Beg Add *** 03/04/24 *******************************************
        //Update stsppcrt1 file if scadj is change and old value found in stsppcrt1
        If svAdj <> scAdj;
           strSqlCursor = 'Select Distinct SPtype from STSPP  +
                           where SPCAT = ''CS'' and SPtype <> '' '' +
                           ORDER BY SPtype ASC';

           Exec Sql Prepare sqlDB2 From :strSqlCursor;
           Exec Sql Declare ClmServices Cursor For sqlDB2;
           Exec Sql Open ClmServices;
           Dow sqlStt = '00000';
              Exec Sql Fetch Next From ClmServices Into :strProcType;
              If sqlStt = '00000';
                 chain (strProcType:conStaf:svAdj) ps10p;
                 If %found(stsppcrt1);
                    S1critVal = scAdj;
                    S1pgm  = q1Pgm;
                    S1user = q1User;
                    S1time = Timeto6(SystemTime());
                    S1dt   = Dateto7(SystemDate());
                    update ps10p;
                 Endif;
              Endif;
           Enddo;
           Exec Sql Close ClmServices;
        Endif;
        //*** End Add *** 03/04/24 *******************************************
        CXrchg = scRchg;
        CXrlbl = scRlbl;
        CXdppc = scDppc;
        CXedim = scEdim;
        CXlgrh = scLgrh;

        // Automatic DARTS Remark Creation section

        CXrffa = scRffa;
        CXrtfa = scRtfa;
        CXrcnc = scRcnc;
        CXropn = scRopn;
        CXrody = scRody;
        CXdsrc = scDsrc;
        CXnmad = scNmad;
        CXaadp = scAadp;
        CXaadr = scAadr;
        CXwcr  = scWcr;
        CXcapd = scCapd;

        // Benefit Diary Default (creation of a claim) section

        CXrads = scRads;
        CXbadj = scBadj;
        CXrcad = scRcad;
        CXrsrd = scRsrd;
        CXrssd = scRssd;
        CXbspv = scBspv;
        CXrumd = scRumd;
        CXbmgr = scBmgr;
        CXcdsm = scCdsm;
        CXdevf = scDevf;
        CXfl01 = scFl01;
        CXaprm = scAprm;

        // Claim Entry Screens to Appear in Add Mode section

        CXclmInst = scClminst;
        CXmedi = scMedi;
        CXbeni = scBeni;
        CXrps  = scRps;
        CXstsp = scStsp;
        CXdben = scDben;
        CXdart = scDart;
      //*** Beg Add ***** 08/29/22 *****************************
        CXPrtFROI = scPFro;
      //*** End Add ***** 08/29/22 *****************************

        // Auto Claim Closing Criteria section

        CXmoc  = scMoc;
        CXaccm = scAccm;
        CXnfam = scNfam;
        CXadrm = scAdrm;
        CXinc  = scInc;
        CXacci = scAcci;
        CXnfai = scNfai;
        CXadri = scAdri;

        // CMS Medicare Diaries section

        CXcerd = scCerd;
        CXcedu = scCedu;
        CXcesu = scCesu;

        CXqrsd = scQrsd;
        CXqrdu = scQrdu;
        CXqrsu = scQrsu;

        CXmbqf = scMbqf;
        CXrcsc = scRcsc;
        CXrcdu = scRcdu;
        CXrcsu = scRcsu;

        // Attorney order/section

        CXapv1 = scApv1;
        CXapv2 = scApv2;
        CXapv3 = scApv3;
        CXapv4 = scApv4;
        CXapv5 = scApv5;

        // Miscellaneous continued section

        CXopcl = scOpcl;
        CXdohr = scDohr;
        CXdobr = scDobr;
        CXgdrr = scGdrr;
        CXexnm = scExnm;
        CXlsrv = scLsrv;
        CXwmth = scWmth;
        CXawwr = scAwwr;
        CXpbme = scPbme;
        CXcvt# = scCvt#;

        ##RtCd = *blanks;
        ##Co   = %editc(inCo#:'X');
        ##Wcf  = %editc(infnd:'X');
        ##Sub  = *zeros;
        ##Fnd  = *zeros;
        FA700R(##RtCd:##Co:##Wcf:##Sub:##Fnd);
        If ##RtCd <> 'F';
           If not %Open(fmcxl);
              Open fmcxl;
           Endif;
           xxSub# = %int(##Sub);
           Chain (inCo#:xxSub#:inFnd) fmcxl;

           If %found(fmcxl);
              F_CXcvt# = scCvt#;
              Update pfx00;
           Endif;
        EndIf;

       Endsr;

       // Subroutine to move Screen fields into database fields.
       BegSr MoveScDb5;
        x2_CXlossCd  = scLossCd;
        x2_CXlsReCd  = scLsReCd;
        x2_CXcoVt    = scCoVt;
        x2_CXlsCv    = scLsCv;
        x2_CXloadAww = scLoadAww;
       EndSr;

       // Update Policy Claim/Darts Control Info subroutine
       BegSr UpdClmInfo;
        // Update Claim/Darts Control info
        Chain (inCo#:inFnd) pcx00;
        Exsr MoveScDb4;
        CXuser  = q1user;                // User
        CXpgm   = q1pgm;                 // Program
        CXtime  = Timeto6(SystemTime()); // Time
        CXdt    = Dateto7(SystemDate()); // Date Mintained
        saveDT4 = CXdt;
        saveTM4 = CXtime;
        If %found(wmcxl);
          Update pcx00;
        Else;
           CXco# = FNco#;
           CXfnd = FNfnd;
           CXiusr = q1user;
           CXidt =  %dec(%char(%date():*cymd0):7:0);
           Write pcx00;
        EndIf;
        //*** Beg Add *** 03/04/24 *******************************************
        svAdj = CXadj;
        //*** End Add *** 03/04/24 *******************************************

        // Update Claim/Darts Extension Control info
        Chain (inCo#:inFnd) pcx02;
        Exsr MoveScDb5;
        x2_CXuser  = q1user;                // User
        x2_CXpgm   = q1pgm;                 // Program
        x2_CXtime  = Timeto6(SystemTime()); // Time
        x2_CXdt    = Dateto7(SystemDate()); // Date Mintained
        saveDT5 = x2_CXdt;
        saveTM5 = x2_CXtime;
        If %found(wmcxp2);
          Update pcx02;
        Else;
           x2_CXco# = FNco#;
           x2_CXfnd = FNfnd;
           x2_CXiusr = q1user;
           x2_CXidt =  %dec(%char(%date():*cymd0):7:0);
           Write pcx02;
        EndIf;
       Endsr;

       // Update Policy Admin/Darts Control Info subroutine
       BegSr UpdPolInfo;
        // Update Admin/Darts Control info
        Chain (inCo#:inFnd) pex00;
        Exsr MoveScDb;
        EXuser  = q1user;                // User
        EXpgm   = q1pgm;                 // Program
        EXtime  = Timeto6(SystemTime()); // Time
        EXdt    = Dateto7(SystemDate()); // Date Mintained
        saveDT = EXdt;
        saveTM = EXtime;
        If %found(wmexl);
           Update pex00;
        Else;
           EXco#   = FNco#;
           EXfnd   = FNfnd;
           EXiusr  = q1user;                // Initial User
           EXipgm  = q1pgm;                 // Initial Program
           EXitm   = Timeto6(SystemTime()); // Initial Time
           EXidt   = Dateto7(SystemDate()); // Initial Date
           Write pex00;
        EndIf;
        //*** Beg Add *** 03/04/24 *******************************************
        svahlc = EXahlc;
        //*** End Add *** 03/04/24 *******************************************

        // Update Endors/Darts Control Info
        Chain (inCo#:inFnd) pdx00;
        Exsr MoveScDb2;
        DXuser  = q1user;                // User
        DXpgm   = q1pgm;                 // Program
        DXtime  = Timeto6(SystemTime()); // Time
        DXdt    = Dateto7(SystemDate()); // Date Mintained
        saveDT2 = DXdt;
        saveTM2 = DXtime;
        If %found(wmdxl);
           Update pdx00;
        Else;
           DXco#   = FNco#;
           DXfnd   = FNfnd;
           DXiusr  = q1user;                // Initial User
           DXipgm  = q1pgm;                 // Initial Program
           DXitm   = Timeto6(SystemTime()); // Initial Time
           DXidt   = Dateto7(SystemDate()); // Initial Date
           Write pdx00;
        EndIf;

        // Update Audit Tracking Control Info
        Chain (inCo#:inFnd) pax00;
        Exsr MoveScDb3;
        AXuser  = q1user;                // User
        AXpgm   = q1pgm;                 // Program
        AXtime  = Timeto6(SystemTime()); // Time
        AXdt    = Dateto7(SystemDate()); // Date Mintained
        saveDT3 = AXdt;
        saveTM3 = AXtime;
        //*** Beg Chg ***** 07/15/24 ******************************
        //***If %found(wmdxl);
        If %found(wmaxl);
        //*** End Chg ***** 07/15/24 ******************************
           Update pax00;
        Else;
           AXco#   = FNco#;
           AXfnd   = FNfnd;
           AXiusr  = q1user;                // Initial User
           AXidt   = Dateto7(SystemDate()); // Initial Date
           Write pax00;
        EndIf;
       EndSr;



      /end-free
     p Main            e

       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi
     d processMe                       n
      /free

       // Program Mode.
       If q1prms >= 2;
          //*** Beg Add ***** 01/17/22 *****************************
          scActMode = @@mode;
          //*** End Add ***** 01/17/22 *****************************
          Select;
          When @@mode = 'D';
             dspMode = *on;
          When @@mode = 'C';
             chgMod  = *on;
          Endsl;
       Endif;

       apShowMenu = *on;

       // Set Menu options
       scMenuChoi = 'Primary Group Information';
       scMenuValu = 'WTGRPPRI';

       scMenuChoi = %trim(scMenuChoi) + ',Administration / DARTS Configuration';
       scMenuValu = %trim(scMenuValu) + ',WTGRPADMIN';

       scMenuChoi = %trim(scMenuChoi) + ',Policy Configuration';
       scMenuValu = %trim(scMenuValu) + ',WTGRPPOLCG';

       scMenuChoi = %trim(scMenuChoi) + ',Claims Configuration';
       scMenuValu = %trim(scMenuValu) + ',WTGRPCLMCG';

       scMenuChoi = %trim(scMenuChoi) + ',State Specific Information';
       scMenuValu = %trim(scMenuValu) + ',WTGRPSSI';

       scMenuChoi = %trim(scMenuChoi) + ',Data Reporting';
       scMenuValu = %trim(scMenuValu) + ',WTGRPDATA';

       aMenHeight = 145;

       apFootHght = aMenHeight + 125;

       // get the system values
       GetSystem(##Wcif:##Mcif:##Wcin);


       // Tab Index
       tabIndex = 0;
       If q1prms >= 4;
          If @@TabInd <> ' ';
             tabIndex = %int(@@TabInd);
             If tabIndex < 0 or tabIndex > 1;
                TabIndex = 0;
             Endif;
          Endif;
          @@TabInd = '00000';
       Endif;

       intCo# = GetUserInfo(q1user);
     // Update Action Panel Info (Rrn, Group Name, Group Number)
       scActRrn = xxRrn;
       scCompName = %trim(FNname);
       scGrpNum = FNfnd;
       If scCompName = ' ';
          Chain intCo# pco00;
          If COname <> ' ';
             scCompName = COname;
          Endif;
       Endif;

       scCo# = intCo#;

       processMe = *on;

       //*** Beg Add *** 05/23/23 ******************************************
       isNoAccess = *off;

       // Set User Access button visibility indicator based on Users Security
       Chain (Q1user) pup01;
       //*** Beg Chg *** 06/27/23 ******************************************
       //*** If %Found(SMUPL) and UPlvl = 999 and UPtype = '*';
       // based on Users Security Type and Level defined in Application Confguration
       If CheckSecLevel('GrpCfgAut':UPtype:UPlvl);
       //*** End Chg *** 06/27/23 ******************************************
          isUsrAcc = *on;
       Else;
          isUsrAcc = *off;
       Endif;

       //*** Beg Add *** 06/23/23 ******************************************
       // Display Group Information in Inquiry Mode even if Users Security
       // level does not allow them to Edit Group Information.
       If dspMode = *on;
          isUsrAcc = *on;
       Endif;
       //*** End Add *** 06/23/23 ******************************************

       // Determine if user is in the Group Config Tab file for each tab.  This
       // will determine which tabs are displayed.  If the user has 999 * then
       // all tabs are displayed

       // Policy tab
       Chain (intCo#:'WTGRPADMIN':'Group Administration / DARTS Configuration':
        'Policy':Q1user) pTB01;
       If %Found(SDTBL1) or isUsrAcc = *on;
          strTabNames += AddToList('Policy':arrTabNames:n1);
       Endif;

       // Claim Tab
       Chain (intCo#:'WTGRPADMIN':'Group Administration / DARTS Configuration':
        'Claim':Q1user) pTB01;
       If %Found(SDTBL1) or isUsrAcc = *on;
          strTabNames += AddToList('Claim':arrTabNames:n1);
       Endif;

       TbNmRcdPnl = strTabNames;
       //*** End Add *** 05/23/23 ******************************************

       Return;


      /end-free
     p ProgramStart    e

      //===================================================================
      // Validate Admin/Darts Configuration Information.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidAdmDrt     b
     d ValidAdmDrt     pi              n
     d isErrAdmDrt     s               n   inz
      /Free

        isErrAdmDrt=*off;
       // Intialize all the messageId and error indicators.
        erDltl= *off;
        msDltl  = *blanks;
        //*** Beg Add ***** 07/03/23 ***********************
        erDltlQt = *off;
        msDltlQt  = *blanks;
        //*** End Add ***** 07/03/23 ***********************
        erOvrl= *off;
        msOvrl = *blanks;
        erUpdl = *off;
        msUpdl  = *blanks;
        erSubl= *off;
        msSubl  = *blanks;
        erPdcl= *off;
        msPdcl = *blanks;
        erCnt1 =*off;
        msCnt1=*blanks;
        erCnt2 =*off;
        msCnt2=*blanks;
        erCnt3 =*off;
        msCnt3=*blanks;
        Dupflg = *off;
        erFqc  =*off;
        msFqc =*blanks;
        erEcnt =*off;
        msEcnt =*blanks;
        erPrtl=*off;
        msPrtl=*blanks;
        erMout=*off;
        msMout=*blanks;
        erAout=*off;
        msAout=*blanks;
        erSout=*off;
        msSout=*blanks;
        erCroq=*off;
        msCroq=*blanks;
        erOfrm=*off;
        msOfrm=*blanks;
        erCndr=*off;
        msCndr=*blanks;
        erCnst=*off;
        msCnst=*blanks;
        erAa#m=*off;
        msAa#m=*blanks;
        //*** Beg Chg *** 10/16/24 *******************************************
        //*** erAacd=*off;
        //*** msAacd=*blanks;
        erA1cd=*off;
        msA1cd=*blanks;
        //*** End Chg *** 10/16/24 *******************************************
        erFlxt=*off;
        msFlxt=*blanks;


       // Employer Deletion from LIVE must be > 0
        If scDltl <= 0;
           erDltl = *on;
           msDltl = 'WCR0502';
           isErrAdmDrt = *on;
        Endif;
        //*** Beg Add ***** 07/03/23 ***********************
       // Employer Deletion from QUOTE must be > 0
        If scDltlQt <= 0;
           erDltlQt = *on;
           msDltlQt = 'WCR0502';
           isErrAdmDrt = *on;
        Endif;
        //*** End Add ***** 07/03/23 ***********************
       // Update Next Available Employer # must be > 0
        If scUpdl <= 0;
           erUpdl = *on;
           msUpdl = 'WCR0502';
           isErrAdmDrt = *on;
        Endif;
       // Override Cancellation Date < Accident Date must be > 0
        If scOvrl <= 0;
           erOvrl = *on;
           msOvrl = 'WCR0502';
           isErrAdmDrt = *on;
        Endif;
       // Override Accident Date Outside of Policy Date Range must be > 0
        If scPdcl <= 0;
           erPdcl = *on;
           msPdcl = 'WCR0502';
           isErrAdmDrt = *on;
        Endif;
       // Allow Submission of Mass Bills must be > 0
        If scSubl <= 0;
           erSubl = *on;
           msSubl = 'WCR0502';
           isErrAdmDrt = *on;
        Endif;

       // Employer Contact Headings can't be Blank
        If scCnt1 = *blanks;
           erCnt1 = *on;
           msCnt1 = 'WCR0503';
           isErrAdmDrt = *on;
        Endif;
        If scCnt2 = *blanks;
           erCnt2 = *on;
           msCnt2 = 'WCR0503';
           isErrAdmDrt = *on;
        Endif;
        If scCnt3 = *blanks;
           erCnt3 = *on;
           msCnt3 = 'WCR0503';
           isErrAdmDrt = *on;
        Endif;

       // FastQuote Cross-Reference must be Mutually Exclusive
        For i=1 to 3;
           If FQ(i) <> *blanks;
              For j=1 to 3;
                 If fq(i) = fq(j) and i <> j;
                    Dupflg = *on;
                    Leave;
                 Endif;
              Endfor;
           Endif;
        Endfor;
        If Dupflg = *on;
           erFqc = *on;
           msFqc = 'WCR1022';
           isErrAdmDrt = *on;
        Endif;

       // EDI Contact can't be Blank
        If ScEcnt = ' ' and visEcnt = *on;
           erEcnt = *on;
           msEcnt='Please Select One EDI Contact';
           isErrAdmDrt = *on;
        Endif;

       // Validate Print Employer Link Code on Schedule
        If ScPrtl = 'Y' and ScPppc <> 'D' ;
           msPrtl   = 'Print Employer Link Code on Schedule is only +
                       Allowed if Payroll Printing is set to D.';
           erPrtl = *on;
           isErrAdmDrt = *on;
        Endif;

       // Member Output Queue must exist if entered
        If ScMout <> *Blanks;
           @@objname = ScMout;
           @@objtype = '*OUTQ  ';
           @@err = '0';
           CheckObj(@@objname:@@objtype:@@err);
           If @@err = '1';
              erMout = *on;
              msMout = 'WC32386';
              isErrAdmDrt = *on;
           Endif;
        Endif;

       // Agent Output Queue must exist if entered
        If ScAout <> *Blanks;
           @@objname = ScAout;
           @@objtype = '*OUTQ  ';
           @@err = '0';
           CheckObj(@@objname:@@objtype:@@err);
           If @@err = '1';
              erAout = *on;
              msAout = 'WC32386';
              isErrAdmDrt = *on;
           Endif;
        Endif;

       // State Output Queue must exist if entered
        If ScSout <> *Blanks;
           @@objname = ScSout;
           @@objtype = '*OUTQ  ';
           @@err = '0';
           CheckObj(@@objname:@@objtype:@@err);
           If @@err = '1';
              erSout = *on;
              msSout = 'WC32386';
              isErrAdmDrt = *on;
           Endif;
        Endif;

       // Cancellation Certificate-Holder Letter Output Queue must exist if entered
        If scCroq <> *Blanks;
           @@objname = scCroq;
           @@objtype = '*OUTQ  ';
           @@err = '0';
           CheckObj(@@objname:@@objtype:@@err);
           If @@err = '1';
             erCroq = *on;
             msCroq = 'WC32386';
             isErrAdmDrt = *on;
           Endif;
        Endif;

       // Validate Office # Change Remark
        If scOfrm = *blanks;
           erOfrm = *on;
           msOfrm = 'WCR0827';
           isErrAdmDrt = *on;
        Endif;

       // Validate Company Name Change Remark and Staff (Both are Mutually Inclusive)
        If (scCndr <> ' ' and scCnst = ' ') OR
           (scCndr = ' ' and scCnst <> ' ');
           erCnst = *on;
           msCnst = 'WCR1083';
           erCndr = *on;
           msCndr = 'WCR1083';
           isErrAdmDrt = *on;
        Endif;

       // Validate Audit Activity and Number of Months
        //*** Beg Chg *** 10/16/24 *******************************************
        //*** If scAacd <> ' ' and scAa#m = *zeros;
        //***    erAa#m = *on;
        //***    msAa#m = 'WCR0503';
        //***    isErrAdmDrt = *on;
        //*** Endif;
        //*** If scAacd = ' ' and scAa#m <> *zeros;
        //***    erAacd = *on;
        //***    msAacd = 'WCR0503';
        //***    isErrAdmDrt = *on;
        //*** Endif;
        //*** If scAacd <> ' ';
        //***    Chain ScAacd Pdr01;
        //***    If Not %Found(Wmdrl);
        //***       erAacd = *on;
        //***       msAacd = 'WC40051';
        //***       isErrAdmDrt = *on;
        //***    Endif;
        //*** Endif;
        If scA1cd <> ' ' and scAa#m = *zeros;
           erAa#m = *on;
           msAa#m = 'WCR0502';
           isErrAdmDrt = *on;
        Endif;
        If scA1cd = ' ' and scAa#m <> *zeros;
           erA1cd = *on;
           msA1cd = 'WCR0503';
           isErrAdmDrt = *on;
        Endif;
        If scA1cd <> ' ';
           Chain ScA1cd Pdr01;
           If Not %Found(Wmdrl);
              erA1cd = *on;
              msA1cd = 'WC40051';
              isErrAdmDrt = *on;
           Endif;
        Endif;
        //*** End Chg *** 10/16/24 *******************************************

       // Validate Flexible Heading and Table ID
        If scFlxh = *blanks and scFlxt <> *blanks;
           erFlxt = *on;
           msFlxt = 'Do Not Enter a Flex Table ID without a Flex Heading.';
           isErrAdmDrt = *on;
        Endif;
        If scFlxt = *blanks and scFlxh <> *blanks;
           erFlxt = *on;
           msFlxt = 'Flex Table ID must be Entered if Flex Heading Entered.';
           isErrAdmDrt = *on;
        Endif;


      // Return *on indicator when No error exists.
        If isErrAdmDrt = *on;
           return *off;
        Else;
           return *on;
        Endif;

      /End-Free
     p ValidAdmDrt     e

      //===================================================================
      // Validate Claim/Darts Configuration Information.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     P ValidClmDrt     B
     d ValidClmDrt     PI              n
     d isError         s               n   inz(*off)
      /free


       // initialize all the error and message variables
       isError = *off;

       // security section
       erSecl = *off;
       msSecl = *blanks;
       erSecc = *off;
       msSecc = *blanks;
       erCkdl = *off;
       msCkdl = *blanks;
       erDltd = *off;
       msDltd = *blanks;
       erPyod = *off;
       msPyod = *blanks;
       erVszl = *off;
       msVszl = *blanks;
       erFsvl = *off;
       msFsvl = *blanks;
       erAusl = *off;
       msAusl = *blanks;
       erDslv = *off;
       msDslv = *blanks;
       erMjr  = *off;
       msMjr  = *blanks;
       erPmts = *off;
       msPmts = *blanks;
       erCat2 = *off;
       msCat2 = *blanks;
       erAdj  = *off;
       msAdj  = *blanks;
       erNfam = *off;
       msNfam = *blanks;
       erAdrm = *off;
       msAdrm = *blanks;
       erNfai = *off;
       msNfai = *blanks;
       erAdri = *off;
       msAdri = *blanks;
       erCedu = *off;
       msCedu = *blanks;
       erCesu = *off;
       msCesu = *blanks;
       erQrsu = *off;
       msQrsu = *blanks;
       erQrdu = *off;
       msQrdu = *blanks;
       erRcsu = *off;
       msRcsu = *blanks;
       erRcdu = *off;
       msRcdu = *blanks;
       erRcnc = *off;
       msRcnc = *blanks;
       erRffa = *off;
       msRffa = *blanks;
       erRtfa = *off;
       msRtfa = *blanks;
       erAadp = *off;
       msAadp = *blanks;
       erAadr = *off;
       msAadr = *blanks;



      // validation for security section

    // security levels below should be > 0

       If scSecl <= 0;
          erSecl = *on;
          msSecl = 'WCR0502';
          isError = *on;
       Endif;

       If scSecc <= 0;
          erSecc = *on;
          msSecc = 'WCR0502';
          isError = *on;
       Endif;

       If scCkdl <= 0;
          erCkdl = *on;
          msCkdl = 'WCR0502';
          isError = *on;
       Endif;

       If scDltd <= 0;
          erDltd = *on;
          msDltd = 'WCR0502';
          isError = *on;
       Endif;

       If scPyod <= 0;
          erPyod = *on;
          msPyod = 'WCR0502';
          isError = *on;
       Endif;

       If scVszl <= 0;
          erVszl = *on;
          msVszl = 'WCR0502';
          isError = *on;
       Endif;

       If scFsvl <= 0;
          erFsvl = *on;
          msFsvl = 'WCR0502';
          isError = *on;
       Endif;

       If scAusl <= 0;
          erAusl = *on;
          msAusl = 'WCR0502';
          isError = *on;
       Endif;

       If scDslv <= 0;
          erDslv = *on;
          msDslv = 'WCR0502';
          isError = *on;
       Endif;

     // validation for Miscellaneous section

       If scMjr = *blanks;
          erMjr = *on;
          msMjr = 'WCR0513';
          isError = *on;
       Endif;

       If scPmts = *blanks;
          erPmts = *on;
          msPmts = 'WCR0503';
          isError = *on;
       Endif;

       If scCat2 = *blanks;
          erCat2 = *on;
          msCat2 = 'WCR0503';
          isError = *on;
       Endif;

       If scAdj = *blanks;
          erAdj = *on;
          msAdj = 'WCR0503';
          isError = *on;
       Endif;

     // Validation for Auto Claim Closing Criteria Section

      // Validate only if 'Medical Only Claims' Flag is 'Y'
       If scMoc = 'Y';
          If scAccm = 'Y';
             If scNfam = *zeros;
                erNfam = *on;
                msNfam = 'WCR0502';
                isError = *on;
             Endif;
             If scAdrm <> *blanks;
                erAdrm = *on;
                msAdrm = 'WCR0450';
                isError = *on;
             EndIf;
          Endif;

          If scAccm = 'N';
             If scNfam = *zeros;
                erNfam = *on;
                msNfam = 'WCR0502';
                isError = *on;
             Endif;
             If scAdrm = *blanks;
                 erAdrm = *on;
                 msAdrm = 'WCR0503';
                 isError = *on;
              EndIf;
          Endif;
       Endif;

      // Validate only if 'Indemnity Claims' Flag is 'Y'
       If scInc = 'Y';
          If scAcci = 'Y';
             If scNfai = *zeros;
                erNfai = *on;
                msNfai = 'WCR0502';
                isError = *on;
             Endif;
             If scAdri <> *blanks ;
                erAdri = *on;
                msAdri = 'WCR0450';
                isError = *on;
             EndIf;
          Endif;

          If scAcci = 'N';
             If scNfai = *zeros;
                erNfai = *on;
                msNfai = 'WCR0502';
                isError = *on;
             Endif;
             If scAdri = *blanks;
                erAdri = *on;
                msAdri = 'WCR0503';
                isError = *on;
             EndIf;
          Endif;
       Endif;

     // Validation for CMS Medicare Diaries Section
      // Claim Input Error Diary Code Validation
       If scCerd <> *Blanks and scCedu = *Blanks and scCesu = *Blanks;
          erCedu = *on;
          msCedu = 'WCR0975';
          erCesu = *on;
          msCesu = 'WCR0975';
          isError = *on;
       EndIf;
       If scCerd <> *Blanks and scCedu <> *Blanks and scCesu <> *Blanks;
          erCedu = *on;
          msCedu = 'WCR0978';
          erCesu = *on;
          msCesu = 'WCR0978';
          isError = *on;
       EndIf;

      // Quick Response Diary Code Validation
       If scQrsd <> *Blanks and scQrsu = *Blanks and scQrdu = *Blanks;
          erQrsu = *on;
          msQrsu = 'WCR0975';
          erQrdu = *on;
          msQrdu = 'WCR0975';
          isError = *on;
       EndIf;
       If scQrdu <> *Blanks and scQrsu <> *Blanks and scQrdu <> *Blanks;
          erQrsu = *on;
          msQrsu = 'WCR0978';
          erQrdu = *on;
          msQrdu = 'WCR0978';
          isError = *on;
       EndIf;

      // Removal of CMS Submission Flag Diary Code Validation
       If scRcsc <> *Blanks and scRcsu = *Blanks and scRcdu = *Blanks;
          erRcsu = *on;
          msRcsu = 'WCR0975';
          erRcdu = *on;
          msRcdu = 'WCR0975';
          isError = *on;
       EndIf;
       If scRcdu <> *Blanks and scRcsu <> *Blanks and scRcdu <> *Blanks;
          erRcsu = *on;
          msRcsu = 'WCR0978';
          erRcdu = *on;
          msRcdu = 'WCR0978';
          isError = *on;
       EndIf;

     // Validation for Automatic DARTS Remark Creation Section

      // Name Change Can't be Blank
       If scRcnc = *blanks;
          erRcnc = *on;
          msRcnc = 'WCR0503';
          isError = *on;
       EndIf;

      // Change To Adjuster Can't be Blank if Change From Adjuster Given
       If scRtfa = *blanks and scRffa <> *Blanks;
          erRffa = *on;
          msRffa = 'WCR1083';
          erRtfa = *on;
          msRtfa = 'WCR1083';
          isError = *on;
       EndIf;

      // Additional Address/Dependent Changes Default Can't be Blank
      // If Generate on Add Checkbox is 'Y'
       If scAadp = *blanks and scAadr = 'Y';
          erAadp = *on;
          msAadp = 'WCR1083';
          erAadr = *on;
          msAadr = 'WCR1083';
          isError = *on;
       EndIf;





       // Return *off (validation fail) if there is error
       If isError = *on;
          return *off;
       Else;
          return *on;
       Endif;

      /end-free
     P ValidClmDrt     E


      // utility procedure definitions
      /COPY SCOPYCOMPI

