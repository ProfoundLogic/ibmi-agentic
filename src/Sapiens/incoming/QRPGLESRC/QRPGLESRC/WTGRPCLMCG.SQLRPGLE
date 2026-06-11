     hcopyright('(c) 2022 Sapiens, Inc. All rights reserved.')

     h/title WTGRPCLMCG - Claims Configuration
     h dftname(WTGRPCLMCG)
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
      //                                                               *
      //  08/06/24 C:300 Support: 117247                               *
      //                 Saving info on 1st Tab Clears info on other Tabs
      //                                                               *
      //  05/23/24 C:300 Support: 115634 (SUNZ)                        *
      //                 Refresh Button should clear all errors and    *
      //                 Re-load screen with latest file information   *
      //                                                               *
      //  08/28/23 C:300 Support: 113191                               *
      //                 Action Hyperlinks like Activate/Inactivate    *
      //                 should not be included as last column of excel*
      //                 spreadsheet export                            *
      //                 DSPF: Removed 2 hyperlinks scActSCB & scActSCBR
      //                 and added 4 new hyperlinks for Activate/Inactivate
      //                                                               *
      //  06/27/23 C:300 Project: WC2910                               *
      //                 Check User Security from Application Configuration
      //                 instead of User Parameters file               *
      //                                                               *
      //  06/23/23 C:300 Project: WC2910, Support: 111868              *
      //                 Display all screens in Inquiry mode, even if  *
      //                 user option security level does not allow them*
      //                 to Edit group information.                    *
      //                                                               *
      // 05/23/23 C:300  Project: WC2910                               *
      //                 - Add User Access Security                    *
      //                                                               *
      // 05/17/23 C:300  Support: 101193                               *
      //                 -"Recurring Compensation Payment" Tab renamed *
      //                  as "Compensation Benefits".                  *
      //                                                               *
      //  05/10/23 C:300 Project:W2196 Support:109128                  *
      //                 Do not reload when user re-visit same Tab, if *
      //                 screen is pending with un-saved changes       *
      //                                                               *
      // 09/26/22 C:342  Support: 101193                               *
      //                 -Add State Compensation Benefit configuration *
      //                  under claims configuration.                  *
      //                 -"Compensation Payment" Tab renamed as        *
      //                  "Recurring Compensation Payment".            *
      //                 -State Compensation Rate by Benefit type      *
      //                  grid is created below State Compensation     *
      //                  Benefit grid.                                *
      //                 -State Compensation Benefit grid has given    *
      //                  option for copy.                             *
      //                 -State Compensation Rate By Benefit grid has  *
      //                  given option for copy.                       *
      //                                                               *
      // 01/17/22 C:291  Project: WC2788                               *
      //                 Add "Copy Configuration" option to Group      *
      //                 Toolbar.                                      *
      //                                                               *
      // ------------------------------------------------------------- *
      //  NOTE: Please add comments to the top of Revisions.           *
      // ------------------------------------------------------------- *
      //===============================================================*

     fWTGRPCLMCGcf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflRsvCus:rrnRsvCus)
     f                                     Sfile(sflMcoCde:rrnMcoCde)
     f                                     Sfile(sflACL:rrnACL)
      //*** Begin Add *** 09/26/22 *******************************************
     f                                     Sfile(sflSCB:rrnSCB)
     f                                     Sfile(sflSCBR:rrnSCBR)
      //*** End   Add *** 09/26/22 *******************************************
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdGrpPnl)
      * Company file
     fsmcol     if   e           k disk    rename(pco01:pco00)
      * Fund Master file
     fwmfnp     if   e             disk    rename(pfn01:pfn0p)
      * Reserve Cushion Definition File
     fwdrcp     uf a e             disk    rename(prc01:prc0p)
     f                                     infds(dbfds4)
     fwdrcl     if   e           k disk    rename(prc01:pre00)
     f                                     infds(dbfds)
      * MCO Group Code File
     fwmmgp     uf a e             disk    rename(pmg01:pmg0p)
     f                                     infds(dbfds5)
     fwmmgl     if   e           k disk    rename(pmg01:pmg00)
     f                                     infds(dbfds2)
      * Compensation Payment Control file
     fwmpcl     uf a e           k disk    rename(ppc01:ppc00)
     f                                     infds(dbfds3)
      * Auto Claim Letters File
     fwaclp     uf a e             disk    rename(pcl01:pcl0p)
     f                                     infds(dbfds6)
     fwacll     if   e           k disk    rename(pcl01:pcl00)
     f                                     infds(dbfds7)
      * Department/Document ID files
     fwmdol     if   e           k disk
      * Logical over all table files
     fwtabl     if   e           k disk    ignore(prc01)
      * Application System Control file
     fssylf     if   e           k disk
     f                                     ignore(SSY01)
     f                                     ignore(SPC01)
     f                                     ignore(SRC01)
     f                                     ignore(SBC01)
     f                                     ignore(SCS01)

      //*** Begin Add *** 09/26/22 *******************************************
      * Compensation Benefit/Reimbursement File
     fwdrbp     uf a e             disk    rename(prb01:prb0p)
     f                                     infds(dbfds25)
     fwdrbl1    if   e           k disk    rename(prb01:prb00)
     f                                     infds(dbfds13)
      * State Compensation Rates by Benefit Type
     fwdr1p     uf a e             disk    rename(pr101:pr10p)
     f                                     infds(dbfds26)
     fwdr1l     if   e           k disk    rename(pr101:pr100)
     f                                     infds(dbfds27)
      //*** End   Add *** 09/26/22 *******************************************
     f*** Beg Add *** 05/23/23 *************************************************
     fsmupl     if   e           k disk
     fsdtbl1    if   e           k disk
     f*** End Add *** 05/23/23 *************************************************

     d psds          esds                  extname(szq1p)

     d dbfds           ds
     d  dbrrn                397    400b 0

     d dbfds2          ds
     d  dbrrn2               397    400b 0

     d dbfds3          ds
     d  dbrrn3               397    400b 0

     d dbfds4          ds
     d  dbrrn4               397    400b 0

     d dbfds5          ds
     d  dbrrn5               397    400b 0

     d dbfds6          ds
     d  dbrrn6               397    400b 0

     d dbfds7          ds
     d  dbrrn7               397    400b 0
     D
      //*** Begin Add *** 09/26/22 *******************************************
     d dbfds13         ds
     d  dbrrn13              397    400b 0

     d dbfds25         ds
     d  dbrrn25              397    400b 0

     d dbfds26         ds
     d  dbrrn26              397    400b 0

     d dbfds27         ds
     d  dbrrn27              397    400b 0

     d isDelResp       s               n   inz
     d saveDT9         s                   like(FNdt)
     d saveTM9         s                   like(FNtime)
     d svrrnSCB        s              9  0 inz
     d svrrnSCBR       s              9  0 inz

     d index           s              5  0
     d inData          s            500      inz
     d ptrSCBRcd       s               *   inz
     ddsSCB            ds                  qualified
     d SCBState                       2
     d SCBRrn                         9  0
     d SCBEfdt                        7  0
     D arrSCB          ds                  dim(9999) likeds(dsSCB)

     d xxRrn1          s              9  0 inz

      // Variable for State comp Rate By Benefits ******************

     d ptrSCBRRcd      s               *   inz
     ddsSCBR           ds                  qualified
     d SCBRState                      2
     d SCBRRrn                        9  0
     d SCBREfdt                       7  0
     d SCBRbr2                        2
     D arrSCBR         ds                  dim(9999) likeds(dsSCBR)

     d kkkey           s                   like(b2key)
     d saveDT10        s                   like(FNdt)
     d saveTM10        s                   like(FNtime)
     d inInclude       s             10
     d inInclude1      s             10
      //*** End   Add *** 09/26/22 *******************************************

      // work variables
     d saveDT          s                   like(FNdt)
     d saveTM          s                   like(FNtime)
     d saveDT2         s                   like(FNdt)
     d saveTM2         s                   like(FNtime)
     d saveDT3         s                   like(FNdt)
     d saveTM3         s                   like(FNtime)
     d saveDT4         s                   like(FNdt)
     d saveTM4         s                   like(FNtime)
     d flgRcdUpdated   s              1    inz
     d clientID        s              3    inz('000')
     d intCo#          s              3  0
     d inCo#           s              3  0
     d inFnd           s              3  0
     D indexTab        s              5  0
     d svrrnRsvCus     s              9  0 inz
     d svrrnMcoCde     s              9  0 inz
     d svrrnACL        s              9  0 inz
     D xxRrn           s              9  0 inz
     d kyrvcd          s                   like(RTkey)inz
     d chgMod          s               n   inz(*off)
     d dspMode         s               n   inz(*off)
     d error1          s               n   inz(*off)
     d process         s               n   inz(*off)
     d KeyEfDt         s                   like(MGefdt)inz
     d xxDelResp       s               n   inz(*off)
     d charMsg         s            100
     d KeyDept         s                   like(DOdept)
     d outDoc          s             12    inz
     d outFld          s             80    inz
     d outFold         s             80    inz
     d outQ            s             10
     d outRtcd         s              1    inz
     D isScriptura     s              1
      ***  Begin Add  ***  05/10/23  *****************
     d t2              s              2  0 inz(*zeros)
     d t3              s              2  0 inz(*zeros)
     d* Array to store Tab index numbers, which is clicked by user before SAVE button
     d ClickTab        s              5  0 dim(4)
      ***  End   Add  ***  05/10/23  *****************
      *** Beg Add *** 05/23/23 *************************************************
       dcl-s  inDspFileName char(10);
       dcl-s  inPgmName     char(50);
       dcl-s  inTabName     char(50);
      *
     DarrTabNames      s            100    varying inz dim(20)
     dn1               s             10i 0 inz(1)
     dstrTabNames      s           2000    varying inz
      *** End Add *** 05/23/23 *************************************************
     d
     d DptDocFld       DS            97
     d  xxDpcdE                1      5
     d  xxFldrE                6     85
     d  xxDcidE               86     97
     d*
      //
     D ##Wcif          s              1
     D ##Mcif          s              1
     D ##Wcin          s              1
      //

      // Proto-types

      * Check Folder
     dChkFolder        pr                  extpgm('SVALFLR')
     d outFold                             like(outFold)
     d outRtcd                             like(outRtcd)

      * Check Document
     dChkDoc           pr                  extpgm('SVALDOC')
     d outDoc                              like(outDoc)
     d outFld                              like(outFld)
     d outRtcd                             like(outRtcd)

     dChkOutq          pr                  extpgm('S0003C')
     d outQ                                like(outQ)
     d outRtcd                             like(outRtcd)

     dChkScriptura     pr                  extpgm('SA047R')
     d isScriptura                         like(outRtcd)

      // Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3

     d GetSystem       pr                  extpgm('SA001R')
     d ##Wcif                         1
     d ##Mcif                         1
     d ##Wcin                         1

      // Load Auto Claim Letters Grid
     d LoadACLGrid     pr

      // Load Compensation Payment
     d LoadCompPay     pr

      // Load MCO Code Grid
     d LoadMcoGrid     pr

      // Load Reserve Cushion Grid
     d LoadRsvCusGrid  pr

      //*** Begin Add *** 09/26/22 *******************************************
      // Load State Compensation Benefit Grid
     d LoadSCBGrid     pr
     d inInclude                     10    const

      // Load State Compensation Rates By Benefit Grid
     d LoadSCBRGrid    pr
     d inInclude1                    10    const

      //*** End   Add *** 09/26/22 *******************************************
      * Main Processing
     d Main            pr

      * Process Add Auto Claim Letters Info
     d ProcAddACL      pr

      * Process MCO Code addition
     d ProcAddMcoCd    pr

      //*** Begin Add *** 09/26/22 *******************************************
      * Process Add State Compensation Benefit Info
     d ProcAddSCB      pr

      * Process Add State Compensation Rates By Benefit Info
     d ProcAddSCBR     pr

      * Process Copy State Compensation Benefit
     d  ProcCopySCB    pr

      * Process Copy State Compensation Rate By Benefit
     d  ProcCopySCBR   pr

      //*** End   Add *** 09/26/22 *******************************************
      * Process Reserve Cushion Addition
     d ProcessAddRsv   pr

      * Process other Options for Auto Claim Letters
     d ProcOthrACL     pr

      * Process other Options for MCO Code
     d ProcOthrMcoCd   pr

      * Process other Options for Reserve Cushion Grid
     d ProcOthrRsv     pr

      //*** Begin Add *** 09/26/22 *******************************************
      * Process other Options for State Compensation Benefit
     d ProcOthrSCB     pr

      * Process other Options for State Compensation Rates By Benefit
     d ProcOthrSCBR    pr

      * Process Performed Action from the action dropdown
     d ProcPerAct      pr

      //*** End   Add *** 09/26/22 *******************************************

      * Program Start
     d ProgramStart    pr
     d processMe                       n

      //*** Begin Add *** 09/26/22 *******************************************
     * On/Off Subfile Display and Display Control Indicators for rcdCompPay
     d RcdCompPayDsp   pr
     d  indValue                      1n   const
      //*** End   Add *** 09/26/22 *******************************************
     * Validate Auto Claim Letters Info
     d ValidateACL     pr              n
     d strmode                        1

     * Validate Compensation Payment
     d ValidCompPay    pr              n

     * Validate MCO Group code info procedure.
     d ValidateMcoCd   pr              n
     d strmode                        1

     * Validate Reserve Cushion Details info. procedure.
     d ValidateRsvCus  pr              n
     d strmode                        1

      //*** Begin Add *** 09/26/22 *******************************************
     * Validate State Compensation Benefit info
     d ValidateSCB     pr              n
     d strmode                        1

     * Validate State Compensation Rates By Benefit info
     d ValidateSCBR    pr              n
     d strmode                        1

      //*** End   Add *** 09/26/22 *******************************************

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

      //*** Beg Add ***** 09/26/22 *****************************
     d/copy wprGrpPop
      //*** End Add ***** 09/26/22 *****************************
      *** Beg Add *** 05/23/23 ************************************************
     d/copy sPrSecure
      *** End Add *** 05/23/23 ************************************************

      // Entry of Program.

     c     *entry        plist
     c                   parm                    @@rrn             9
     c                   parm                    @@mode            1
     c                   parm                    @@next           10
     c                   parm                    @@TabInd          5

     c     keyWMMGL      klist
     c                   kfld                    FNco#
     c                   kfld                    FNfnd
     c                   kfld                    scMcoe

     c     keyWMMGL2     klist
     c                   kfld                    FNco#
     c                   kfld                    FNfnd
     c                   kfld                    scMcoe
     c                   kfld                    KeyEfDt

     c     keyWmdol      klist
     c                   kfld                    CLCo#
     c                   kfld                    KeyDept
     c                   kfld                    CLdpcd
     c                   kfld                    CLfldr
     c                   kfld                    CLdcid

      //*** Begin Add *** 09/26/22 *******************************************
     c     keyWdrbl1     klist
     c                   kfld                    inCo#
     c                   kfld                    inFnd
     c                   kfld                    scStESCB
     c                   kfld                    KeyEfdt

     c     keyWdr1l      klist
     c                   kfld                    inCo#
     c                   kfld                    inFnd
     c                   kfld                    scStESCBR
     c                   kfld                    scBenTypE
     c                   kfld                    KeyEfdt

     c     keyWtabl      klist
     c                   kfld                    inCo#
     c                   kfld                    kkkey
      //*** End   Add *** 09/26/22 *******************************************

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
       // Load Auto Claim Letters Grid
       //===================================================================
     p LoadACLGrid     b
     d LoadACLGrid     pi

      /free

        rrnACL = 0;
        dspACL = *off;
        clrACL = *on;
        Write rcdLetter;
        clrACL = *off;
        dspACL = *on;
        scRowSel = 1;

        If dspMode;
           inquiry_3 = *on;
        Endif;
        If isScriptura = 'Y';
           visbscript = *on ;
           inquiry_3 = *on;
        Endif;

        // Populate the subfile
        Setll (inCo#:inFnd) wacll;
        Reade (inCo#:inFnd) wacll;
        Dow not %eof(wacll);
           rrnACL += 1;
           scACLSel = *off;

           // set the selection on 1st record for first load
           If svrrnACL = *zeros;
              If rrnACL = 1;
                 scACLSel = *on;
              Endif;
           Else;
              If rrnACL = svrrnACL;
                 scACLSel = *on;
              Endif;
           Endif;

           scStACL = CLst;
           scOfc# = CLofc#;
           scDpcd = CLdpcd;
           scDcid = CLdcid;
           KeyDept = 'CS';
           scDescACL = *Blanks;
           chain keyWmdol wmdol;
           If %found(wmdol);
              scDescACL = DOdesc;
           Endif;
           scOutq = CLoutq;
           sc#cpy = CL#cpy;

           scACLRRN = dbrrn7;

           Write sflACL ;
           Reade (inCo#:inFnd) wacll;
        Enddo;

        rrnACL = 1;
        If svrrnACL <> 0;
           rrnACL = svrrnACL;
           scRowSel = svrrnACL;
           svrrnACL = 0;
        Endif;

      /end-free
     p LoadACLGrid     e

       //===================================================================
       // Load Compensation Payment
       //===================================================================
     p LoadCompPay     b
     d LoadCompPay     pi

      /free

       Chain(n) (inCo#:inFnd) ppc00;
       If %found(wmpcl);
          scArfl  = PCarfl;
          scDidy  = PCdidy;
          scRcep  = PCrcep;
          scDidy  = PCdidy;
          scRclp  = PCrclp;
          scEdtd  = PCedtd;
          scPrtd  = PCprtd;
          scMax#  = PCmax#;

          saveDT  = PCdt;
          saveTM  = PCtime;
       Endif;

      /end-free
     p LoadCompPay     e

       //===================================================================
       // Load MCO Group Code Grid
       //===================================================================
     p LoadMcoGrid     b
     d LoadMcoGrid     pi

      /free
        // Clear the subfile
        rrnMcoCde = 0;
        dspMcoCde = *off;
        clrMcoCde = *on;
        Write rcdMCOGrp;
        clrMcoCde = *off;
        dspMcoCde = *on;
        scRowSel = 1;

        If dspMode;
           inquiry_2 = *on;
        Endif;

        // Populate the subfile
        Setll (inCo#:inFnd) wmmgl;
        Reade (inCo#:inFnd) wmmgl;
        Dow not %eof(wmmgl);
           rrnMcoCde += 1;
           scMcoSel = *off;

           // set the selection on 1st record for first load
           If svrrnMcoCde = *zeros;
              If rrnMcoCde = 1;
                 scMcoSel = *on;
              Endif;
           Else;
              If rrnMcoCde = svrrnMcoCde;
                 scMcoSel = *on;
              Endif;
           Endif;

           scMco  = MGmco;
           scEfdt = CvtToDate(MGefdt);
           scExdt = CvtToDate(MGexdt);
           scName = MGname;
           scPhn# = %int(%editc(MGarea:'X') + %editc(MGphn#:'X'));

           scMcoRRN = dbrrn2;

           Write sflMcoCde;
        Reade (inCo#:inFnd) wmmgl;
        Enddo;

        rrnMcoCde = 1;
        If svrrnMcoCde <> 0;
           rrnMcoCde = svrrnMcoCde;
           scRowSel = svrrnMcoCde;
           svrrnMcoCde = 0;
        Endif;

      /end-free
     p LoadMcoGrid     e

       //===================================================================
       // Load Reserve Cushion Grid
       //===================================================================
     p LoadRsvCusGrid  b
     d LoadRsvCusGrid  pi

      /free
        // Clear the subfile
        rrnRsvCus = 0;
        dspRsvCus = *off;
        clrRsvCus = *on;
        Write rcdRsvCus;
        clrRsvCus = *off;
        dspRsvCus = *on;

        If dspMode;
           inquiry = *on;
        Endif;

        // Populate the subfile
        Setll (inCo#:inFnd) wdrcl;
        Reade (inCo#:inFnd) wdrcl;
        Dow not %eof(wdrcl);
           rrnRsvCus += 1;
           scRsvSel = *off;

           // set the selection on 1st record for first load
           If svrrnRsvCus = *zeros;
              If rrnRsvCus = 1;
                 scRsvSel = *on;
              Endif;
           Else;
              If rrnRsvCus = svrrnRsvCus;
                 scRsvSel= *on;
              Endif;
           Endif;

           scRvcd = RCrvcd;
           scEfDt = CvtToDate(RCefdt);
           scMamt = RCmamt;
           scLamt = RCLamt;

           kyrvcd = RCrvcd;
           chain (inCo#:kyrvcd) prt01;
           If %found(wtabl);
              scDesc = RTdesc;
           Endif;

           scRsvRRN = dbrrn;

           Write sflRsvCus;
        Reade (inCo#:inFnd) wdrcl;
        Enddo;

        rrnRsvCus = 1;
        If svrrnRsvCus <> 0;
           rrnRsvCus = svrrnRsvCus;
           svrrnRsvCus = 0;
        Endif;

      /end-free
     p LoadRsvCusGrid  e

      //*** Begin Add *** 09/26/22 *******************************************
       //===================================================================
       // Load State Compensatin Benefit Grid
       //===================================================================
     p LoadSCBGrid     b
     d LoadSCBGrid     pi
     d inIncludeSCB                  10    const

      /free

       rrnSCB = 0;
       RcdCompPayDsp(*off);
       clrSCB = *on;
       Write rcdCompPay ;
       clrSCB = *off;
       RcdCompPayDsp(*on);
       scRowSel = 1;

       If dspMode;
          inquiry_6 = *on;
       Endif;

       // Populate the subfile
       Setll (inCo#:inFnd) wdrbl1;
       Reade (inCo#:inFnd) wdrbl1;
       Dow not %eof(wdrbl1);
          rrnSCB += 1;
          scSCBSel = *off;

          // set the selection on 1st record for first load
          If svrrnSCB = *zeros;
             If rrnSCB = 1;
                scSCBSel = *on;
             Endif;
          Else;
             If rrnSCB = svrrnSCB;
                scSCBSel = *on;
             Endif;
          Endif;

          scStSCB = RBst;
          scEfDtSCB = CvtToDate(RBefdt);
          scCmpr = RBcmpr;
          scPacp = RBpacp;
          scSCBSts = *Blanks;

          //*** Begin Chg ***** 08/28/23 *****************************
          // scActSCB = *Blanks;
          // If RBstat = 'A';
          //    scSCBSts = 'Active';
          //    scActSCB = 'Inactivate';
          // Endif;
          // If RBstat = 'I';
          //    scSCBSts = 'Inactive';
          //    scActSCB = 'Activate';
          // Endif;

          visActSCB = *off;                // New Hyperlink for Activate
          visIActSCB = *off;               // New Hyperlink for Inactivate
          If RBstat = 'A';
             scSCBSts = 'Active';
             visIActSCB = *on;
          Endif;
          If RBstat = 'I';
             scSCBSts = 'Inactive';
             visActSCB = *on;
          Endif;

          If dspMode;
             visActSCB = *off;
             visIActSCB = *off;
          Endif;
          //*** End Chg ***** 08/28/23 *****************************

          //Select Checkbox
          If inIncludeSCB = 'Include';
             scSelect = 'Y';
             nxchgSCB  = *on;
          Else;
             scSelect = 'N';
             nxchgSCB  = *off;
          Endif;

          scSCBRRN = dbrrn13;

          Write sflSCB;
           Reade (inCo#:inFnd) wdrbl1;
        Enddo;

        rrnSCB  = 1;
        If svrrnSCB <> 0;
           rrnSCB = svrrnSCB;
           scRowSel = svrrnSCB;
           svrrnSCB = 0;
        Endif;
        Clear inInclude;

      /end-free
     p LoadSCBGrid     e

       //===================================================================
       // Load State Compensatin Rate By Benefit Grid
       //===================================================================
     p LoadSCBRGrid    b
     d LoadSCBRGrid    pi
     d inIncludeSCBR                 10    const

      /free

       rrnSCBR = 0;
       RcdCompPayDsp(*off);
       clrSCBR = *on;
       Write rcdCompPay ;
       clrSCBR = *off;
       RcdCompPayDsp(*on);

       If dspMode;
          inquiry_6 = *on;
       Endif;

       // Populate the subfile
       Setll (inCo#:inFnd) wdr1l ;
       Reade (inCo#:inFnd) wdr1l ;
       Dow not %eof(wdr1l);
          rrnSCBR += 1;
          scSCBRSel = *off;

          // set the selection on 1st record for first load
          If svrrnSCBR = *zeros;
             If rrnSCBR = 1;
                scSCBRSel = *on;
             Endif;
          Else;
             If rrnSCBR = svrrnSCBR;
                scSCBRSel = *on;
             Endif;
          Endif;

          scStSCBR = R1st;
          scEfDtSCBR = CvtToDate(R1efdt);
          scBenTyp = R1br2 ;
          kkkey    = R1br2 ;
          chain KeyWtabl pb201;
          if %found(wtabl);
             scBenDesc = b2desc ;
          else  ;
             scBenDesc = ' ' ;
          endif ;
          scRateSCBR = R1rate ;
          scSCBRSts = *Blanks;

          //*** Begin Chg ***** 08/28/23 *****************************
          // scActSCBR = *Blanks;
          // If R1stat = 'A';
          //    scSCBRSts = 'Active';
          //    scActSCBR = 'Inactivate';
          // Endif;
          // If R1stat = 'I';
          //    scSCBRSts = 'Inactive';
          //    scActSCBR = 'Activate';
          // Endif;

          visActSCR = *off;                // New Hyperlink for Activate
          visIActSCR = *off;               // New Hyperlink for Inactivate
          If R1stat = 'A';
             scSCBRSts = 'Active';
             visIActSCR = *on;
          Endif;
          If R1stat = 'I';
             scSCBRSts = 'Inactive';
             visActSCR = *on;
          Endif;

          If dspMode;
             visActSCR = *off;
             visIActSCR = *off;
          Endif;
          //*** End Chg ***** 08/28/23 *****************************

          //Select Checkbox
          If inIncludeSCBR = 'Include';
             scSelectR = 'Y';
             nxchgSCBR  = *on;
          Else;
             scSelectR = 'N';
             nxchgSCBR  = *off;
          Endif;

          scSCBRRRN = dbrrn27;

          Write sflSCBR;
           Reade (inCo#:inFnd) wdr1l;
        Enddo;

        rrnSCBR  = 1;
        If svrrnSCBR <> 0;
           rrnSCBR = svrrnSCBR;
           svrrnSCBR = 0;
        Endif;
        Clear inInclude1;

      /end-free
     p LoadSCBRGrid    e

      //*** End   Add *** 09/26/22 *******************************************
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
       DoW BtnCancel = *off;

          // display screen
          Exsr ExfmtScreens;

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
             //*** Begin Add *** 09/26/22 *************************************
             inInclude = ' ';
             //*** End   Add *** 09/26/22 *************************************

          // (Enter key or Save button)
          When btnSave = *on;
             btnSave = *off;
             If @@mode <> 'D';
                //***  Begin Add  ***  08/06/24  *****************
                // Initialize flgRcdUpdated
                flgRcdUpdated = '0';
                //***  End   Add  ***  08/06/24  *****************
                if saveDT <> 0; // User Pressed on Tab 2
                   // Check if WMPCP updated since screen displayed (Compensation Payment)
                   flgRcdUpdated = RcdUpdatedErr('WMPCP':dbrrn3:'PCdt':'PCtime':
                    'PCuser':saveDT:saveTM);
                EndIf;

                if flgRcdUpdated = '0'; // No error

                   //***  Begin Add  ***  08/06/24  *****************
                   // Validate only if user has visited Tab 2
                   t3 = 0;
                   t3 = %lookup(2:ClickTab);
                   If t3 <> 0;
                   //***  End   Add  ***  08/06/24  *****************
                   // Validate Compensation Payment (Tab 2)
                   If ValidCompPay();
                      Exsr UpdCompPay;
                   Else;
                      error1 = *on;
                   EndIf;
                   //***  Begin Add  ***  08/06/24  *****************
                   Endif;
                   //***  End   Add  ***  08/06/24  *****************

                Else;
                   error1 = *on;
                EndIf;

                // Show Data Saved Sucessfully Message
                If error1 = *off;
                   PopDataSaved();

                   //*** Beg Add ***** 05/10/23 *****************************
                   Clear ClickTab;
                   Clear t2;
                   //*** End Add ***** 05/10/23 *****************************
                EndIf;
             Endif;

          // Add Reserve Cushion Button
          When BtnRsvAdd = *On;
             BtnRsvAdd =*Off;
             ProcessAddRsv();

          // Add MCO Group Code.
          When BtnMcoAdd = *On;
             BtnMcoAdd = *Off;
             ProcAddMcoCd();

          // Add Auto Claim Letters Info
          When BtnACLAdd = *On;
             BtnACLAdd = *Off;
             ProcAddACL();

          //*** Begin Add *** 09/26/22 *******************************
          // Add State Compensation Benefit Info
          When BtnSCBAdd = *On;
             BtnSCBAdd = *Off;
             ProcAddSCB();

          // Add State Compensation Rate By Benefit Info
          When BtnSCBRAdd = *On;
             BtnSCBRAdd = *Off;
             ProcAddSCBR();
          //*** End   Add *** 09/26/22 *******************************

          //*** Beg Add ***** 01/17/22 *******************************
          // Copy Configuration
          When btnShowWin = *on;
             PopupWindow(txtShowWin:scActRRN:'GS':txtWinData);
             btnShowWin = *off;
          //*** End Add ***** 01/17/22 *******************************

          //*** Beg Add *** 05/23/23 ******************************************
          // Add User Access Window Logic
          When btnUsrAcc = *on;
             inDspFileName = 'WTGRPCLMCG';
             inPgmName     = 'Group Claims Configuration';
             inTabName     =  arrTabNames(TabIndex + 1);
             UserAccessWdw(inDspFileName:inPgmName:inTabName);
             btnUsrAcc = *off;
          //*** End Add *** 05/23/23 ******************************************

          // Menu Choices
          When scSelOpt <> *blanks;
             @@next = scSelOpt;
             Leave;

          //*** Begin Add *** 09/26/22 ********************************
          When btnSCBIncl = *on;
             btnSCBIncl = *off ;
             inInclude = 'Include';
          When btnSCBExcl = *on;
             btnSCBExcl = *off;
             inInclude = ' ';

          When btnSCBRInc = *on;
             btnSCBRInc = *off;
             inInclude1 = 'Include';
          When btnSCBRExc = *on;
             btnSCBRExc = *off;
             inInclude1 = ' ';

          When btnPerAct = *on;
             ProcPerAct();

          // perform action for State comp Rate By Benefit grid
          When btnPerAct1 = *on;
             ProcPerAct();
          //*** End   Add *** 09/26/22 ********************************

          Other;
             ProcOthrRsv();                 // Reserve Cushion Edit
             ProcOthrMcocd();               // MCO Group Code Edit
             ProcOthrACL();                 //other option for Auto Claim Letters Edit
             //*** Begin Add *** 09/26/22 ******************************
             ProcOthrSCB();                 //other option for State Compensation Benefit
             ProcOthrSCBR();        //other option for State Compensation Rate by Benefit
             //*** End   Add *** 09/26/22 ******************************

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
       When arrTabNames(TabIndex+1) = 'Reserve Cushion';
       //*** End Chg *** 05/23/23 ******************************************
          pnlHeight = 500;
          APnlHeight = pnlHeight + 60;
          //***  Begin Add  ***  05/23/24  *****************
          btnRefresh = *Off;
          //***  End   Add  ***  05/23/24  *****************
          Clear rcdRsvCus;

         // Load Reserve Cushion Grid
          LoadRsvCusGrid();

          If dspMode;
             inquiry = *on;
          Endif;

          Write rcdGrpPnl;
          Write rcdPanel;
          Exfmt rcdRsvCus;
          Read  rcdGrpPnl;
          Read rcdPanel;

       //*** Beg Chg *** 05/23/23 ******************************************
       //*** When indexTab = 2;
       When arrTabNames(TabIndex+1) = 'Compensation Benefits';
       //*** End Chg *** 05/23/23 ******************************************
          //*** Begin Chg *** 09/26/22 ***********************************
          //*******   pnlHeight = 500;
          pnlHeight = 950 ;
          //*** End   Chg *** 09/26/22 ***********************************
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
             Clear rcdCompPay;

             //*** Begin Add *** 09/26/22 *************************************
             // Initialize Screen fields
             ChoSelAct = '--- Select List Action ---,Copy';
             scSelAct  = '--- Select List Action ---';
             scSelAct1 = '--- Select List Action ---';
             scSelect  = *blanks ;
             scSelectR = *blanks ;
             //*** End   Add *** 09/26/22 *************************************
            // Load Compensation Payment Info
             LoadCompPay();
             //*** Begin Add *** 09/26/22 **************************************
             //***  Begin Del  ***  05/10/23  *****************
             // Load State Compensation Benefit Grid
             //*** LoadSCBGrid(inInclude);
             // Load State Compensation Rates By Benefit Grid
             //*** LoadSCBRGrid(inInclude1);
             //***  End Del  ***  05/10/23  *****************
             //*** End   Add *** 09/26/22 **************************************
          Endif;

          //***  Begin Add  ***  05/10/23  *****************
          Endif;
          // Load State Compensation Benefit Grid
          LoadSCBGrid(inInclude);
          // Load State Compensation Rates By Benefit Grid
          LoadSCBRGrid(inInclude1);
          //***  End   Add  ***  05/10/23  *****************

          If dspMode;
             inquiry_1 = *on;
             //*** Begin Add *** 09/26/22 **************************************
             inquiry_6 = *on;
             //*** End   Add *** 09/26/22 **************************************
          Endif;

          Write rcdGrpPnl;
          Write rcdPanel;
          Exfmt rcdCompPay;
          Read  rcdGrpPnl;
          Read rcdPanel;

       //*** Beg Chg *** 05/23/23 ******************************************
       //*** When indexTab = 3;
       When arrTabNames(TabIndex+1) = 'MCO Group Codes';
       //*** End Chg *** 05/23/23 ******************************************
          pnlHeight = 500;
          APnlHeight = pnlHeight + 60;
          //***  Begin Add  ***  05/23/24  *****************
          btnRefresh = *Off;
          //***  End   Add  ***  05/23/24  *****************
          Clear rcdMCOGrp;

          // Load MCO Group Code Grid
          LoadMcoGrid();

          If dspMode;
             inquiry_2 = *on;
          Endif;

          Write rcdGrpPnl;
          Write rcdPanel;
          Exfmt rcdMCOGrp;
          Read  rcdGrpPnl;
          Read rcdPanel;

       //*** Beg Chg *** 05/23/23 ******************************************
       //*** When indexTab = 4;
       When arrTabNames(TabIndex+1) = 'Letters';
       //*** End Chg *** 05/23/23 ******************************************
          pnlHeight = 500;
          APnlHeight = pnlHeight + 60;
          //***  Begin Add  ***  05/23/24  *****************
          btnRefresh = *Off;
          //***  End   Add  ***  05/23/24  *****************
          Clear rcdLetter;

          // Load Auto Claim Letters Grid
          LoadACLGrid();

          If dspMode;
             inquiry_3 = *on;
          Endif;
          If isScriptura = 'Y';
             visbscript = *on ;
             inquiry_3 = *on;
          Endif;

          Write rcdGrpPnl;
          Write rcdPanel;
          Exfmt rcdLetter;
          Read  rcdGrpPnl;
          Read rcdPanel;


       EndSl;

       Endsr;

       // Subroutine to move Screen fields into database fields.
       BegSr MoveScDb;
        PCarfl  = scArfl;
        PCdidy  = scDidy;
        PCrcep  = scRcep;
        PCdidy  = scDidy;
        PCrclp  = scRclp;
        PCedtd  = scEdtd;
        PCprtd  = scPrtd;
        PCmax#  = scMax#;
       EndSr;

       // Update Compensation Payment Info Tab subroutine
       BegSr UpdCompPay;
        // Load Compensation Payment Info
        Chain (inCo#:inFnd) ppc00;
        Exsr MoveScDb;
        PCuser  = q1user;                // User
        PCpgm   = q1pgm;                 // Program
        PCtime  = Timeto6(SystemTime()); // Time
        PCdt    = Dateto7(SystemDate()); // Date Mintained
        saveDT  = PCdt;
        saveTM  = PCtime;
        If %found(wmpcl);
           Update ppc00;
        Else;
           PCco#   = FNco#;
           PCfnd   = FNfnd;
           Write ppc00;
        EndIf;
       EndSr;



      /end-free
     p Main            e

       //===================================================================
       // ProcAddACL - process addition of Auto Claim Letters
       //===================================================================
     P ProcAddACL      B
     D ProcAddACL      PI
     d*
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)

      /free

       Clear rcdACLDtl;
       mode = 'A';
       isexit = *off;
       BtnACLCan = *off;

       // Set selection criteria for Office
       scOfc#CH = 'OFco# = ' + %EditC(inCo#:'X') +' and OFstat <> ' + '''I''';

       // Set selection criteria for Department Code
       scDpcdCH = 'DOco# = ' + %EditC(inCo#:'X') + ' and DOdept = '
        +'''CS'''+' and DOstat <> ' + '''I''';

       Dow BtnACLCan = *off and isexit = *off;

          scACLHdg = 'Add Auto Claim Letters Information';

          Exfmt rcdACLDtl ;

          Select;
          // Save Button
          When BtnACLSav = *on;
             BtnACLSav = *off;
             DptDocFld = scDptDcFlE;
             If ValidateACL(mode);
                Exsr WrtACL;
                isexit = *on;
             EndIf;

          // Cancel Button
          When BtnACLCan = *on;
             BtnACLCan = *off;
             isexit = *on;

          EndSl;
       Enddo;

       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        CLco#  = inCo#;                 // Company Number
        CLfnd  = inFnd;                 // Group Number
        CLst   = scStEACL;             // State
        CLofc# = scOfc#E;             // Office
        CLdpcd = xxDpcdE;              // Department Code
        CLdcid = xxDcidE;              // Document Id
        CLfldr = xxFldrE;              // Folder
        CL#cpy = sc#CpyE;              // Number Of copies
        CLoutq = scOutqE;              // Output Queue
        CLltfl = scLtflE;              //Lost time flag Begining value
        CLltto = scLttoE;              //Lost time flag End value
        CLcsfr = scCsfrE;             // Claim Status Begining Value
        CLcsto = scCstoE;             // Claim Status Ending Value
        CLandr = scAndrE;             //And Or
        CLuser = q1user;
        CLpgm  = q1pgm;
        CLtime = Timeto6(SystemTime());
        CLdt   = Dateto7(SystemDate());
        CLitm = Timeto6(SystemTime());
        CLidt  = Dateto7(SystemDate());
        CLiusr = q1user;
        CLipgm = q1pgm;
       EndSr;

       // Writing Auto Claim Letters Info
       BegSr WrtACL;
        Clear pcl0p;
        Exsr MoveScDb;
        Write pcl0p ;
       EndSr;

      /end-free
     P ProcAddACL      E

       //===================================================================
       // ProcAddMcoCd  - process addition of MCO Group Code
       //===================================================================
     P ProcAddMcoCd    B
     D ProcAddMcoCd    PI
     d*
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)

      /free

       Clear rcdMcoDtl;
       mode = 'A';
       isexit = *off;
       BtnMcoCan = *off;
       McoCdeEdit = *off;

       Dow BtnMcoCan = *off and isexit = *off;

          scMcoHdg = 'Add MCO Group Code';
          visMcoSvAd = *On;                           // Show Save/Add next Button

          Exfmt rcdMcoDtl;

          Select;
          // Save/Add Next Button
          When BtnMcoSvAd = *on;
             BtnMcoSvAd = *off;
             If ValidateMcoCd(mode);
                Exsr WrtMcoCd;
               // Initialize Screen fields for re-load
                Clear rcdMcoDtl;
                BtnMcoCan = *off;
                McoCdeEdit = *off;
             EndIf;

          // Process Save Button
          When BtnMcoSav = *on;
             BtnMcoSav = *off;
             If ValidateMcoCd(mode);
                Exsr WrtMcoCd;
                isexit = *on;
             EndIf;

          // Process Cancel Button
          When BtnMcoCan = *on;
             BtnMcoCan = *off;
             isexit = *on;

          EndSl;
       Enddo;

       // Subroutine to move Screen fields into database fields.
       BegSr MoveScDb;
        MGco#  = inCo#;
        MGfnd  = inFnd;
        MGmco  = scMcoe;
        MGefdt = dtCYMD(scEfDte);
        MGexdt = dtCYMD(scExDte);
        MGname = scNamee;
        MGadr1 = scAdr1e;
        MGadr2 = scAdr2e;
        MGcty  = scCtye;
        MGst   = scSte;
        MGzip  = scZipe;
        MGrep  = scRepe;

        If scPhn#e > *zeros;
           MGarea  = %dec(%subst(%editc(scPhn#e:'X'):1:3):3:0);
           MGphn#  = %dec(%subst(%editc(scPhn#e:'X'):4):7:0);
        EndIf;
        If scPhn#e = *zeros;
           MGarea  = *zeros;
           MGphn#  = *zeros;
        EndIf;

        MGuser = q1user;
        MGpgm  = q1pgm;
        MGtime = Timeto6(SystemTime());
        MGdt   = Dateto7(SystemDate());
        MGiUsr = q1user;
        MGiPgm = q1pgm;
        MGiTim = Timeto6(SystemTime());
        MGiDt  = Dateto7(SystemDate());
        MGid#  = *Zeros;
       EndSr;

       // Writing MCO Group Code subroutine
       BegSr WrtMcoCd;
        Clear pmg0p;
        Exsr MoveScDb;
        Write pmg0p;
       EndSr;

      /end-free
     P ProcAddMcoCd    E

      //*** Begin Add *** 09/26/22 *******************************************
       //===================================================================
       // ProcAddSCB - process addition of State Compensation Benefit
       //===================================================================
     P ProcAddSCB      B
     D ProcAddSCB      PI
     d*
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)

      /free

       Clear rcdSCBDtl;
       mode = 'A';
       isexit = *off;
       BtnSCBCan = *off;
       SCBEdt = *off;
       scSCBStE = 'A';

       Dow BtnSCBCan = *off and isexit = *off;

          scSCBHdg = 'Add State Compensation Benefit Information';

          Exfmt rcdSCBDtl;

          Select;

          // Save Button
          When BtnSCBSav = *on;
             BtnSCBSav = *off;
             If ValidateSCB(mode);
                Exsr WrtSCB;
                isexit = *on;
             EndIf;

          // Cancel Button
          When BtnSCBCan = *on;
             BtnSCBCan = *off;
             isexit = *on;

          EndSl;
       Enddo;

       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        RBco#  = inCo#;                 // Company Number
        RBfnd  = inFnd;                 // Group Number
        RBst  = scStESCB;             // State
        RBefdt = dtCYMD(scEfdtESCB);       // Effective Date
        RBcmpr = scCmprE;              // New (7,4) Compensation Reimbursement Percentage
        RBrepc = %dech(scCmprE:7:2);   // Old (7,2) Compensation Reimbursement Percentage
        RBstat = scSCBStE;              // Status
        RBmnrt = scMnrtE;              // Minimum Compensation Rate
        RBpacp = scPacpE;              //Maximum Compensation Rate
        RBwks = scWksE;              //Waiting Period(Days)
        RBlhmn = scLhmnE;             // USL&H Minimum Compensation Rate
        RBlhmx = scLhmxE;             // USL&H Maximum Compensation Rate
        RBlhwp = scLhwpE;             // USL&H Waiting Period (Days)
        RBdys  = scDysE;               // Full salary Continuance, Number of Days
        RBicpd = scIcpdE;               // Number of Days Initial Compensation Payment Due
        RBuser = q1user;
        RBpgm  = q1pgm;
        RBtime = Timeto6(SystemTime());
        RBdt   = Dateto7(SystemDate());
        RBitim = Timeto6(SystemTime());
        RBidt  = Dateto7(SystemDate());
        RBiusr = q1user;
        RBipgm = q1pgm;
       EndSr;

       // Writing State Compensation Benefit Info
       BegSr WrtSCB;
        Clear prb0p;
        Exsr MoveScDb;
        Write prb0p;
       EndSr;

      /end-free
     P ProcAddSCB      E

       //=====================================================================
       // ProcAddSCBR - process addition of State Compensation Rate By Benefit
       //=====================================================================
     P ProcAddSCBR     B
     D ProcAddSCBR     PI
     d*
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)

      /free

       Clear rcdSCBRDtl;
       mode = 'A';
       isexit = *off;
       BtnSCBRCan = *off;
       SCBREdt = *off;
       scSCBRStE = 'A';

       Dow BtnSCBRCan = *off and isexit = *off;

          scSCBRHdg = 'Add State Compensation Rate By Benefit Information';

          Exfmt rcdSCBRDtl;

          Select;

          // Save Button
          When BtnSCBRSav = *on;
             BtnSCBRSav = *off;
             If ValidateSCBR(mode);
                Exsr WrtSCBR;
                isexit = *on;
             EndIf;

          // Cancel Button
          When BtnSCBRCan = *on;
             BtnSCBRCan = *off;
             isexit = *on;

          EndSl;
       Enddo;

       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        R1co#  = inCo#;                 // Company Number
        R1fnd  = inFnd;                 // Group Number
        R1st  = scStESCBR;              // State
        R1efdt = dtCYMD(scDtESCBR) ;    // Effective Date
        R1rate = %dech(scERtSCBR:7:4);  // Benfit Rate
        R1stat = scSCBRStE ;            // Status
        R1br2  = scBenTypE ;            // Benefit type
        R1user = q1user;
        R1pgm  = q1pgm;
        R1time = Timeto6(SystemTime());
        R1dt   = Dateto7(SystemDate());
        R1itim = Timeto6(SystemTime());
        R1idt  = Dateto7(SystemDate());
        R1iusr = q1user;
        R1ipgm = q1pgm;
       EndSr;

       // Writing State Compensation Rate By Benefit Info
       BegSr WrtSCBR;
        Clear pr10p;
        Exsr MoveScDb;
        Write pr10p;
       EndSr;

      /end-free
     P ProcAddSCBR     E

       //===================================================================
       // ProcCopySCB - Copy State Compensation Benefits
       //===================================================================
     p ProcCopySCB     b
     d ProcCopySCB     pi
      /Free
        index = 0;
        clear arrSCB;
        ReadC sflSCB;
        Dow Not %Eof;
          If scSelect = 'Y';
             nxchgSCB  = *on;
             update sflSCB;
             dsSCB.SCBState = scStSCB;
             dsSCB.SCBRrn = scSCBRRN;
             dsSCB.SCBEfdt = DateTo7(scEfdtSCB);
             index += 1;
             arrSCB(index) = dsSCB;
          Endif;
          ReadC sflSCB;
        Enddo;

        If index > 0 ;
           ptrSCBRcd = %Addr(arrSCB);
           inData = 'Copy SCB';
           xxRrn1= %int(@@rrn);
           GroupCopySetup(xxrrn1:inData:ptrSCBRcd);
        Endif;

     p ProcCopySCB     e

       //===================================================================
       // ProcCopySCBR - Copy State Compensation Rate By Benefits
       //===================================================================
     p ProcCopySCBR    b
     d ProcCopySCBR    pi
      /Free
        index = 0;
        clear arrSCBR;
        ReadC sflSCBR;
        Dow Not %Eof;
          If scSelectR = 'Y';
             nxchgSCBR  = *on;
             update sflSCBR;
             dsSCBR.SCBRState = scStSCBR ;
             dsSCBR.SCBRRrn   = scSCBRRRN ;
             dsSCBR.SCBREfdt  = DateTo7(scEfdtSCBR) ;
             dsSCBR.SCBRbr2   = scBenTyp ;
             index += 1;
             arrSCBR(index) = dsSCBR;
          Endif;
          ReadC sflSCBR;
        Enddo;

        If index > 0 ;
           ptrSCBRRcd = %Addr(arrSCBR);
           inData = 'Copy SCBR';
           xxRrn1= %int(@@rrn);
           GroupCopySetup(xxrrn1:inData:ptrSCBRRcd);
        Endif;

     p ProcCopySCBR    e

      //*** End   Add *** 09/26/22 *******************************************
       //===================================================================
       // ProcessAddRsv - process addition of Reserve Cushion
       //===================================================================
     P ProcessAddRsv   B
     D ProcessAddRsv   PI
     d*
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)

      /free

       Clear rcdRsvDtl;
       mode = 'A';
       isexit = *off;
       BtnRsvCan = *off;

       Dow BtnRsvCan = *off and isexit = *off;

          scRsvHdg = 'Add Reserve Cushion Information';

          Exfmt rcdRsvDtl;

          Select;
          // Save Button
          When BtnRsvSave = *on;
             BtnRsvSave = *off;
             If ValidateRsvCus(mode);
                Exsr WrtRsvCus;
                isexit = *on;
             EndIf;

          // Cacel Button
          When BtnRsvCan = *on;
               BtnRsvCan = *off;
               isexit = *on;

          EndSl;
       Enddo;

       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        RCco#   = inCo#;                  // Company Number
        RCfnd   = inFnd;                  // Group Number
        RCrvcd  = scRvcdAE;               // Reserve Code
        RCefdt  = dtCYMD(scEfDtAE);       // Effective date
        RCmamt  = scMamtAE;               // Med- only amount
        RClamt  = scLamtAE;               // Lost Time amount

        RCuser = q1user;
        RCpgm  = q1pgm;
        RCtime = Timeto6(SystemTime());
        RCdt   = Dateto7(SystemDate());
       EndSr;

       // Writing Reserve Cushions Subroutine
       BegSr WrtRsvCus;
        Clear prc0p;
        Exsr MoveScDb;
        Write prc0p;
       EndSr;

      /end-free
     P ProcessAddRsv   E

       //===================================================================
       // ProcOthrACL - Process other Options for Auto Claim Letters Info
       //===================================================================
     P ProcOthrACL     B
     D ProcOthrACL     PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdACLDtl;
       isexit = *off;
       ReadC sflACL;

       Dow not %eof;

          Select;
          When scACLEdt = *On; // Processing for Edit Auto Claim Letters Info
             scACLEdt = *Off;
             mode = 'C';

             Chain(n) scACLRRN WACLP;

             Exsr MovDbSc;

             // Set selection criteria for Office
             scOfc#CH='OFco# = ' + %EditC(inCo#:'X')+' and OFstat <> ' +'''I''';

             // Set selection criteria for Department Code
             scDpcdCH = 'DOco# = ' + %EditC(inCo#:'X') + ' and DOdept = '
              +'''CS'''+' and DOstat <> ' + '''I''';

             scACLHdg = 'Edit Auto Claim Letters Information';
             svrrnACL = rrnACL;   //Save Rrn value for edited record

             Dow isexit = *off;
                Exfmt rcdACLDtl;

                Select;
                // Process Save Button
                When BtnACLSav = *on;
                   BtnACLSav = *off;
                   // Check if WACLP updated since screen displayed (Auto Claim Letters)
                   flgRcdUpdated = RcdUpdatedErr('WACLP':dbrrn6:'CLdt':'CLtime':
                    'CLuser':saveDT4:saveTM4:'1');
                   if flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scACLRRN WACLP;
                      Exsr MovDbSc;
                   EndIf;
                   if flgRcdUpdated = '0'; // No error
                      DptDocFld = scDptDcFlE;
                      If ValidateACL(mode);
                         Chain scACLRRN WACLP;
                         Exsr MoveScDb;
                         Update pcl0p;
                         isexit = *on;
                      EndIf;
                   EndIf;

                // Process Cancel Button
                When BtnACLCan = *on;
                   BtnACLCan = *off;
                   isexit = *on;
                EndSl;
             EndDo;

          When scACLDlt = *On; // Processing for Delete Auto Claim Letters Info
             scACLDlt = *Off;

             Chain(e) scACLRRN WACLP;
             If %status = 1218;
                PopOneBtn('Record locked by another user. Please try again-
                 later.':'Record Lock');
                Return;
             Endif;

             Chain scACLRRN WACLP;
             Exsr MovDbSc;

             scACLHdg = 'Delete Auto Claim Letters Info';

             Dow isexit = *off;
                Write rcdACLDtl;
                xxDelresp = PopTwoBtn('Do you want to delete this Auto +
                 Claim Letters Info?':'Confirm Delete':'Yes':'No');

                Select;
                When xxDelResp = *On;
                    Delete pcl0p;
                    PopOneBtn('Auto Claim Letters Info +
                     Deleted!':'Delete':'Ok');
                    isexit = *on;
                When xxDelResp = *Off;
                   svrrnACL = rrnACL;   //Save Rrn value
                   isexit = *on;
                EndSl;
             EndDo;
          Endsl;

          Unlock WACLP;

          ReadC sflACL;
       Enddo;

       // Subrotine to move database fields into screen fields
       BegSr MovDbSc;
        scStEACL =CLst;
        scOfc#E = CLofc#;
        xxDpcdE = CLdpcd;
        xxDcidE = CLdcid;
        xxFldrE = CLfldr;
        scDptDcFlE = DptDocFld;
        sc#CpyE = CL#cpy;
        scOutqE = CLoutq;
        scLtflE = CLltfl;
        scLttoE = CLltto;
        scCsfrE = CLcsfr;
        scCstoE = CLcsto;
        scAndrE = CLandr;
        saveDT4 = CLdt;
        saveTM4 = CLtime;
       EndSr;

       // Subrotine to move Screen fields into database fields
       BegSr MoveScDb;
        CLofc# = scOfc#E;             // Office
        CLdpcd = xxDpcdE;              // Department Code
        CLdcid = xxDcidE;              // Document Id
        CLfldr = xxFldrE;              // Folder
        CL#cpy = sc#CpyE;              // Number Of copies
        CLoutq = scOutqE;              // Output Queue
        CLltfl = scLtflE;              //Lost time flag Begining value
        CLltto = scLttoE;              //Lost time flag End value
        CLcsfr = scCsfrE;             // Claim Status Begining Value
        CLcsto = scCstoE;             // Claim Status Ending Value
        CLandr = scAndrE;             //And Or
        CLuser = q1user;
        CLpgm  = q1pgm;
        CLtime = Timeto6(SystemTime());
        CLdt   = Dateto7(SystemDate());
        saveDT4 = CLdt;
        saveTM4 = CLtime;
       EndSr;

      /end-free
     P ProcOthrACL     E

       //===================================================================
       // ProcOthrMcoCd - Process other Options for MCO Code
       //===================================================================
     P ProcOthrMcoCd   B
     D ProcOthrMcoCd   PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdMcoDtl;
       isexit = *off;
       ReadC sflMcoCde;

       Dow not %eof;

          Select;
          When scMcoEdit = *On;   // Processing for Edit MCO Group Code
             scMcoEdit = *Off;
             mode = 'C';

             Chain(n) scMcoRRN wmmgp;

             Exsr MovDbSc;

             scMcoHdg = 'Edit MCO Group Code Information';
             McoCdeEdit = *on ;           //disable non-editable field
             svrrnMcoCde = rrnMcoCde;     //Save Rrn value

             Dow isExit = *off;
                Exfmt rcdMcoDtl;

                Select;
                // Process Save Button
                When BtnMcoSav = *on;
                   BtnMcoSav = *off;
                   // Check if WMMGP updated since screen displayed (MCO Group Code)
                   flgRcdUpdated=RcdUpdatedErr('WMMGP':dbrrn5:'MGdt':'MGtime':
                    'MGuser':saveDT3:saveTM3:'1');
                   if flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scMcoRRN wmmgp;
                      Exsr MovDbSc;
                   EndIf;
                   if flgRcdUpdated = '0'; // No error
                      If ValidateMcoCd(mode);
                         Chain scMcoRRN wmmgp;
                         Exsr MoveScDb;
                         Update pmg0p;
                         isexit = *on;
                      EndIf;
                   EndIf;

                // Process Cancel Button
                When BtnMcoCan = *on;
                   BtnMcoCan = *off;
                   isexit = *on;
                EndSl;
             Enddo;

          When scMcoDlt = *on;  // Processing for Delete
             scMcoDlt = *off;

             Chain(e) scMcoRRN wmmgp;
             If %status = 1218;
                PopOneBtn('Record locked by another user. Please try again-
                 later.':'Record Lock');
                Return;
             Endif;

             Chain scMcoRRN wmmgp;
             Exsr MovDbSc;

             Dow isexit = *off;
                write rcdMcoDtl;
                xxDelresp = PopTwoBtn('Do you want to delete this MCO +
                             Group Code?':'Confirm Delete':'Yes':'No');

                Select;
                When xxDelResp = *On;
                   Delete pmg0p;
                   PopOneBtn('MCO Group Code deleted!':'Delete':'Ok');
                   isexit = *on;
                When xxDelResp = *Off;
                   svrrnMcoCde = rrnMcoCde;     //Save Rrn value
                   isexit = *on;
                EndSl;
             EndDo;
          Endsl;

          Unlock wmmgp;

          ReadC sflMcoCde;
       Enddo;

       // Subroutine to move database fields into screen fields
       BegSr MovDbSc;
        scMCoe  = MGmco;
        scEfDte = dtMDYY(MGefdt);
        scExDte = dtMDYY(MGexdt);
        scNamee = MGname;
        scAdr1e = MGadr1;
        scAdr2e = MGadr2;
        scCtye  = MGcty;
        scSte   = MGst;
        scRepe  = MGrep;
        scZipe  = MGzip;
        If MGarea <> *zeros or MGphn# <> *zeros;
           scPhn#e = %int(%editc(MGarea:'X') + %editc(MGphn#:'X'));
        Endif;
        saveDT3 = MGdt;
        saveTM3 = MGtime;
       Endsr;

       // Subroutine to move Screen fields into database fields
       BegSr MoveScDb;
        MGmco  = scMCoe ;
        MGefdt = dtCYMD(scEfDte);
        MGexdt = dtCYMD(scExDte);
        MGname = scNamee;
        MGadr1 = scAdr1e;
        MGadr2 = scAdr2e;
        MGCty  = scCtye;
        MGSt   = scSte ;
        MGrep  = scRepe;
        MGzip  = scZipe;

        If scPhn#e > *zeros;
           MGarea  = %dec(%subst(%editc(scPhn#e:'X'):1:3):3:0);
           MGphn#  = %dec(%subst(%editc(scPhn#e:'X'):4):7:0);
        EndIf;
        If scPhn#e = *zeros;
           MGarea  = *zeros;
           MGphn#  = *zeros;
        EndIf;

        MGuser = q1user;
        MGpgm  = q1pgm;
        MGtime = Timeto6(SystemTime());
        MGdt   = Dateto7(SystemDate());
        MGid#  = *Zeros;
        saveDT3 = MGdt;
        saveTM3 = MGtime;

       EndSr;

      /end-free
     P ProcOthrMcoCd   E

       //===================================================================
       // ProcOthrRsv - Process other Options for Reserve Cushion Grid
       //===================================================================
     P ProcOthrRsv     B
     D ProcOthrRsv     PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdRsvDtl;
       isexit = *off;
       ReadC sflRsvCus;

       Dow not %eof;

          // Processing for Edit Reserve Cushion Link
          If scRsvEdt = *On;
             scRsvEdt = *Off;
             mode = 'C';

             Chain(n) scRsvRRN wdrcP;
             scRvcdAE = RCrvcd;
             scEfDtAE = dtMDYY(RCefdt);
             scMamtAE = RCmamt;
             scLamtAE = RClamt;
             saveDT2 = RCdt;
             saveTM2 = RCtime;

             scRsvHdg = 'Edit Reserve Cushion Information';
             rdRsvCusAE = *on;                        // Read only Reserve Cushion
             svrrnRsvCus = rrnRsvCus;                 //Save RRN value

             Dow isexit = *off;
                Exfmt rcdRsvDtl;

                Select;
                // Save Button
                When BtnRsvSave = *on;
                   BtnRsvSave = *off;
                   // Check if WDRCP updated since screen displayed (Reserve Cushion)
                   flgRcdUpdated=RcdUpdatedErr('WDRCP':dbrrn4:'RCdt':'RCtime':
                    'RCuser':saveDT2:saveTM2:'1');
                   if flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scRsvRRN wdrcP;
                      scRvcdAE = RCrvcd;
                      scEfDtAE = dtMDYY(RCefdt);
                      scMamtAE = RCmamt;
                      scLamtAE = RClamt;
                      saveDT2 = RCdt;
                      saveTM2 = RCtime;
                   EndIf;
                   if flgRcdUpdated = '0'; // No error
                      If ValidateRsvCus(mode);
                         Chain scRsvRRN wdrcP;
                         Exsr MoveScDb;
                         Update prc0p;
                         isexit = *on;
                      EndIf;
                   EndIf;

                // Cacel Button
                When BtnRsvCan = *on;
                     BtnRsvCan = *off;
                      isexit = *on;
                EndSl;
             EndDo;

             Unlock wdrcp;
          EndIf;

          ReadC sflRsvCus;
       Enddo;


       // Subrotine to move Screen fields into database fields
       BegSr MoveScDb;
        RCmamt = scMamtAE;
        RCLamt = scLamtAE;

        RCuser = q1user;
        RCpgm  = q1pgm;
        RCtime = Timeto6(SystemTime());
        RCdt   = Dateto7(SystemDate());
        saveDT2 = RCdt;
        saveTM2 = RCtime;
       EndSr;

      /end-free
     P ProcOthrRsv     E

      //*** Begin Add *** 09/26/22 *******************************************
       //===================================================================
       // ProcOthrSCB  - Process other Options for State Compensation Benefit
       //===================================================================
     P ProcOthrSCB     B
     D ProcOthrSCB     PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdSCBDtl;
       isexit = *off;
       ReadC sflSCB;

       Dow not %eof;

          Select;
          When scSCBEdt = *On; // Processing for Edit State Compensation Benefit
             scSCBEdt = *Off;
             mode = 'C';

             Chain(n) scSCBRRN WDRBP;

             Exsr MovDbSc;

             scSCBHdg = 'Edit State Compensation Benefit Information';
             SCBEdt = *on;        //disable non-editable field
             svrrnSCB = rrnSCB;   //save rrn value

             Dow isexit = *off;
                Exfmt rcdSCBDtl;

                Select;
                // Save Button
                When BtnSCBSav = *on;
                   BtnSCBSav = *off;
                   // Check if WDRBP updated since screen displayed (State Comp Benefit)
                   flgRcdUpdated=RcdUpdatedErr('WDRBP':dbrrn25:'RBdt':'RBtime':
                    'RBuser':saveDT9:saveTM9:'1');
                   if flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scSCBRRN WDRBP;
                      Exsr MovDbSc;
                   EndIf;
                   if flgRcdUpdated = '0'; // No error
                      If ValidateSCB(mode);
                         Chain scSCBRRN WDRBP;
                         Exsr MoveScDb;
                         Update prb0p;
                         isexit = *on;
                      EndIf;
                   EndIf;

                // Cancel Button
                When BtnSCBCan = *on;
                   BtnSCBCan = *off;
                   isexit = *on;
                EndSl;
             EndDo;

          When scSCBDlt = *On; // Processing for Delete State Compensation Benefit Link
             scSCBDlt = *Off;

             Chain(e) scSCBRRN WDRBP;
             If %status = 1218;
                PopOneBtn('Record locked by another user. Please try again-
                 later.':'Record Lock');
                Return;
             Endif;

             Chain scSCBRRN WDRBP;
             Exsr MovDbSc;

             scSCBHdg = 'Delete State Compensation Benefit Info';

             Dow isexit = *off;

                Write rcdSCBDtl;
                isDelresp = PopTwoBtn('Do you want to delete this State +
                Compenation Benefit Information?':'Confirm Delete':'Yes':'No');
                Select;
                When isDelResp = *On;
                    Delete prb0p;
                    PopOneBtn('State Compensation Benefit Information +
                    Deleted!':'Delete':'Ok');
                    isexit = *on;
                When isDelResp = *Off;
                   svrrnSCB = rrnSCB;   //save rrn value
                   isexit = *on;
                EndSl;
             EndDo;
          Endsl;

          Unlock WDRBP;

          ReadC sflSCB;
       Enddo;

       // Subrotine to move database fields into screen fields
       BegSr MovDbSc;
        scStESCB = RBst;
        scEfdtESCB = dtMDYY(RBefdt);
        scCmprE = RBcmpr;
        scSCBStE = RBstat;
        scMnrtE = RBmnrt;
        scPacpE = RBpacp;
        scWksE  = RBwks;
        scLhmnE = RBlhmn;
        scLhmxE = RBlhmx;
        scLhwpE = RBlhwp;
        scDysE  = RBdys;
        scIcpdE = RBicpd;
        saveDT9 = RBdt;
        saveTM9 = RBtime;
       EndSr;

       // Subrotine to move Screen fields into database fields
       BegSr MoveScDb;
        RBcmpr = scCmprE;              // New (7,4) Compensation Reimbursement Percentage
        RBrepc = %dech(scCmprE:7:2);   // Old (7,2) Compensation Reimbursement Percentage
        RBstat = scSCBStE;              // Status
        RBmnrt = scMnrtE;              // Minimum Compensation Rate
        RBpacp = scPacpE;              //Maximum Compensation Rate
        RBwks = scWksE;              //Waiting Period(Days)
        RBlhmn = scLhmnE;             // USL&H Minimum Compensation Rate
        RBlhmx = scLhmxE;             // USL&H Maximum Compensation Rate
        RBlhwp = scLhwpE;             // USL&H Waiting Period (Days)
        RBdys  = scDysE;               // Full salary Continuance, Number of Days
        RBicpd = scIcpdE;               // Number of Days Initial Compensation Payment Due
        RBuser = q1user;
        RBpgm  = q1pgm;
        RBtime = Timeto6(SystemTime());
        RBdt   = Dateto7(SystemDate());
        saveDT9 = RBdt;
        saveTM9 = RBtime;
       EndSr;

      /end-free
     P ProcOthrSCB     E

       //============================================================================
       // ProcOthrSCBR - Process other Options for State Compensation Rate By Benefit
       //============================================================================
     P ProcOthrSCBR    B
     D ProcOthrSCBR    PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdSCBRDtl;
       isexit = *off;
       ReadC sflSCBR;

       Dow not %eof;

          Select;
          When scSCBREdt = *On; // Processing for Edit State Compensation Rate By Benefit
             scSCBREdt = *Off;
             mode = 'C';

             Chain(n) scSCBRRRN WDR1P;

             Exsr MovDbSc;

             scSCBRHdg = 'Edit State Compensation Rate By Benefit Information';
             SCBREdt = *on;        //disable non-editable field
             svrrnSCBR = rrnSCBR;   //save rrn value

             Dow isexit = *off;
                Exfmt rcdSCBRDtl;

                Select;
                // Save Button
                When BtnSCBRSav = *on;
                   BtnSCBRSav = *off;
                   // Check if WDR1P updated since screen displayed (State Comp Rate Benefit)
                   flgRcdUpdated=RcdUpdatedErr('WDR1P':dbrrn26:'R1dt':'R1time':
                    'R1user':saveDT10:saveTM10:'1');
                   if flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scSCBRRRN WDR1P;
                      Exsr MovDbSc;
                   EndIf;
                   if flgRcdUpdated = '0'; // No error
                      If ValidateSCBR(mode);
                         Chain scSCBRRRN WDR1P;
                         Exsr MoveScDb;
                         Update pr10p;
                         isexit = *on;
                      EndIf;
                   EndIf;

                // Cancel Button
                When BtnSCBRCan = *on;
                   BtnSCBRCan = *off;
                   isexit = *on;
                EndSl;
             EndDo;

          When scSCBRDlt = *On; // Processing for Delete State Compensation Rate By Benefit Link
             scSCBRDlt = *Off;

             Chain(e) scSCBRRRN WDR1P;
             If %status = 1218;
                PopOneBtn('Record locked by another user. Please try again-
                 later.':'Record Lock');
                Return;
             Endif;

             Chain scSCBRRRN WDR1P;
             Exsr MovDbSc;

             scSCBRHdg = 'Delete State Compensation Rate By Benefit Info';

             Dow isexit = *off;

                Write rcdSCBRDtl;
                isDelresp = PopTwoBtn('Do you want to delete this State +
                Compenation Rate By Benefit Information?':'Confirm Delete'
                :'Yes':'No');
                Select;
                When isDelResp = *On;
                    Delete pr10p;
                    PopOneBtn('State Compensation Rate By Benefit Information +
                    Deleted!':'Delete':'Ok');
                    isexit = *on;
                When isDelResp = *Off;
                   svrrnSCBR = rrnSCBR;   //save rrn value
                   isexit = *on;
                EndSl;
             EndDo;
          Endsl;

          Unlock WDR1P;

          ReadC sflSCBR;
       Enddo;

       // Subrotine to move database fields into screen fields
       BegSr MovDbSc;
        scStESCBR = R1st;
        scDtESCBR = dtMDYY(R1efdt);
        scERtSCBR = R1rate;
        scSCBRStE = R1stat;
        scBenTypE = R1br2 ;
        saveDT10 = R1dt;
        saveTM10 = R1time;
       EndSr;

       // Subrotine to move Screen fields into database fields
       BegSr MoveScDb;
        R1rate = scERtSCBR ;           // Benefit rate
        R1stat = scSCBRStE ;           // Status
        R1user = q1user;
        R1pgm  = q1pgm;
        R1time = Timeto6(SystemTime());
        R1dt   = Dateto7(SystemDate());
        saveDT10 = R1dt;
        saveTM10 = R1time;
       EndSr;

      /end-free
     P ProcOthrSCBR    E

       //===================================================================
       // ProcPerAct - Process Performed Action from the action dropdown
       //===================================================================
     p ProcPerAct      b
     d ProcPerAct      pi
      /Free
         Select;
         When scSelAct = 'Copy' and btnPerAct = *on;
            btnPerAct = *off ;
            ProcCopySCB();
         When scSelAct1 = 'Copy' and btnPerAct1 = *on;
            btnPerAct1 = *off ;
            ProcCopySCBR();
         Endsl;

      /End-Free
     p ProcPerAct      e
      //*** End   Add *** 09/26/22 *******************************************
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
             If tabIndex < 0 or tabIndex > 3;
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

       // check if scriptura enabled
       isScriptura = *Blanks;
       ChkScriptura(isScriptura);

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

       // Reserve Cushion tab
       Chain (intCo#:'WTGRPCLMCG':'Group Claims Configuration':
        'Reserve Cushion':Q1user) pTB01;
       If %Found(SDTBL1) or isUsrAcc = *on;
          strTabNames += AddToList('Reserve Cushion':arrTabNames:n1);
       Endif;

       // Compensation Benefits tab
       Chain (intCo#:'WTGRPCLMCG':'Group Claims Configuration':
        'Compensation Benefits':Q1user) pTB01;
       If %Found(SDTBL1) or isUsrAcc = *on;
          strTabNames += AddToList('Compensation Benefits':arrTabNames:n1);
       Endif;

       // MCO Group Codes tab
       Chain (intCo#:'WTGRPCLMCG':'Group Claims Configuration':
        'MCO Group Codes':Q1user) pTB01;
       If %Found(SDTBL1) or isUsrAcc = *on;
          strTabNames += AddToList('MCO Group Codes':arrTabNames:n1);
       Endif;

       // Letters tab
       Chain (intCo#:'WTGRPCLMCG':'Group Claims Configuration':
        'Letters':Q1user) pTB01;
       If %Found(SDTBL1) or isUsrAcc = *on;
          strTabNames += AddToList('Letters':arrTabNames:n1);
       Endif;

       TbNmRcdPnl = strTabNames;
       //*** End Add *** 05/23/23 ******************************************

       Return;


      /end-free
     p ProgramStart    e
      //*** Begin Add *** 09/26/22 *******************************************
       //===================================================================
       // Load display indicators for sub-files in rcdCompPay Record Format
       //===================================================================
     P RcdCompPayDsp   b
     D RcdCompPayDsp   pi
     D  indValue                      1n   const
      /Free
       dspSCB  = indValue;
       dspSCBR = indValue;
      /End-Free
     P RcdCompPayDsp   e

      //*** End   Add *** 09/26/22 *******************************************

      //===================================================================
      // Validate Auto Claim Letters Info
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateACL     b
     d ValidateACL     pi              n
     d strmode                        1
     d isErrACLEdt     s               n   inz
      /Free

        isErrACLEdt = *off;
       // Intialize all the messageId and error indicators.
        erStEACL = *off;
        msStEACL = *blanks;
        erOfc#E = *off;
        msOfc#E = *blanks;
        erDptDcFlE = *off;
        msDptDcFlE = *blanks;
        flDptDcFlE = *blanks;
        er#cpyE = *off;
        ms#cpyE = *blanks;
        erOutqE = *off;
        msOutqE = *blanks;
        erCsfrE = *off;

        // State can not be blank
        If scStEACL = *Blanks;
           erStEACL = *on;
           msStEACL = 'WCR0504';
           isErrACLEdt = *on;
        Endif;

        // Office can not be blank
        If scOfc#E = *Blanks;
           erOfc#E = *on;
           msOfc#E = 'WCR0503';
           isErrACLEdt = *on;
        Endif;

        // Department code, document id and folder should not be blank
        If scDptDcFlE = *Blanks;
           erDptDcFlE = *on;
           msDptDcFlE = 'WCR0503';
           flDptDcFlE = 'WMESGG';
           isErrACLEdt = *on;
        Endif;
        If xxFldrE <> *Blanks;
           outFold = xxFldrE;
           outRtcd = ' ';
           ChkFolder(outFold:outRtcd);
         //Folder must exist
           If outRtcd = '1';
              erDptDcFlE = *On;
              msDptDcFlE = 'WCR0826';
              flDptDcFlE = 'WMESGG';
              isErrACLEdt = *on;
           Endif;
        // User must have authority to folder
           If outRtcd = '2';
              erDptDcFlE = *On;
              msDptDcFlE = 'SCM0517';
              flDptDcFlE = 'SCMESG';
              isErrACLEdt = *on;
           Endif;
        Endif;
        // Docuement must be valid
        If xxDcidE <> *Blanks;
           outFld = xxFldrE;
           outDoc = xxDcidE;
           outRtcd = *Blanks;
           ChkDoc(outDoc:outFld:outRtcd);
           If outRtcd = '1';
              erDptDcFlE = *On;
              msDptDcFlE = 'WCR0825';
              flDptDcFlE = 'WMESGG';
              isErrACLEdt = *on;
           Endif;
        Endif;

        // Number of copies can not be zero.
        If sc#cpyE = *zeros;
           er#cpyE = *on;
           ms#cpyE = 'WCR0502';
           isErrACLEdt = *on;
        Endif;

        //Outq should not be blank
        If scOutqE = *Blanks;
           erOutqE = *on;
           msOutqE = 'WCR0503';
           isErrACLEdt = *on;
        Else;
           //Out queue should be valid
           outRtcd = *Blanks;
           OutQ = scOutqE;
           ChkOutq(outQ:outRtcd);
           If outRtcd = '1';
              erOutqE = *On;
              msOutqE = 'WCR0509';
              isErrACLEdt = *on;
           Endif;
        Endif;

        // If claim status Ending value if not open then begining value should not be new.
        If scCsfrE = 'N' and scCstoE <> 'O';
           erCsfrE = *On;
           isErrACLEdt = *on;
        Endif;



      // Return *on indicator when No error exists.
       If isErrACLEdt = *on;
          return *off;
       Else;
          return *on;
       Endif;

      /End-Free
     p ValidateACL     e

      //===================================================================
      // Validate Compensation Payment
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidCompPay    b
     d ValidCompPay    pi              n
     d isError2        s               n   inz

      /Free
       isError2 = *off;
       erDidy   = *off;
       msDidy   = *blanks;

      // If Either Remark Code Entered
      // '# of Days in Advance to Diary' must be > 0
       If scRcep <> *Blanks or scRclp <> *Blanks;
          If scDidy <= *zero;
             erDidy = *on;
             msDidy = 'WCR0502';
             isError2 = *on;
          Endif;
       Endif;


      // Return *on indicator when No error exists.
       If isError2 = *on;
          return *off;
       Else;
          return *on;
       Endif;

      /End-Free

     p ValidCompPay    e

      //===================================================================
      // Validate MCO Group Code Information.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateMcoCd   b
     d ValidateMcoCd   pi              n
     d strmode                        1
     d isErrMcoEdt     s               n   inz
      /Free

        isErrMcoEdt = *off;
       //Intialize all the messageId and error indicators.
        erMcoe = *off;
        msMcoe = *blanks;
        erEfdte = *off;
        msEfdte = *blanks;
        erExdte = *off;
        msExdte = *blanks;
        erNamee = *off;
        msNamee = *blanks;
        erAdr1e = *off;
        msAdr1e = *blanks;
        erCtye = *off;
        msCtye = *blanks;
        erSte = *off;
        msSte = *blanks;
        erZipe = *off;
        msZipe = *blanks;

        // Validation Check for Effective Date
        erEfdte = ValidDate(scEfdte:charMsg);
        If erEfdte = *on;
           msEfdte = charMsg;
           isErrMcoEdt  = *on;
        Endif;

        If strmode <> 'C';                       //Add Mode
          // MCO Group Code can't be blank
           If scMcoe = *blanks;
              erMcoe = *on;
              msMcoe = 'WC31320';
              isErrMcoEdt = *on;
           Endif;

          // Check for Duplicat Record (MCO Group Code must be unique for eff date)
           If scMcoe <> *Blanks and erEfdte = *off;
              KeyEfDt = dtCYMD(scEfdte);
              Chain keyWMMGL2 wmmgl;
              If %found(wmmgl);
                 erMcoe = *on;
                 msMcoe = 'WC30005';
                 erEfdte = *on;
                 msEfdte = 'WC30005';
                 isErrMcoEdt = *on;
              Endif;
           Endif;
        Endif;

        // Validation Check for Expiration Date
        If scExdte <> 0;
           erExdte = ValidDate(scExdte:charMsg);
           If erExdte = *on;
              msExdte = charMsg;
              isErrMcoEdt = *on;
           Endif;
        Endif;

        // Dates must be in Ascending order
        If scEfdte <> 0 and scExdte <> 0;
           If erEfdte = *off and erExdte = *off;
              erExdte = AscendDates(scEfdte:scExdte:charMsg);
              If erExdte = *on;
                 erEfdte = *on;
                 msEfDte = charMsg;
                 msExDte = charMsg;
                 isErrMcoEdt  = *on;
              Endif;
           Endif;
        Endif;


        // Validation Check for effective Date overlap with another entry of same code.
        If scMcoe <> *Blanks and erEfdte = *off and erExdte = *off;
           Setll keyWMMGL wmmgl;
           Reade keyWMMGL wmmgl;
           Dow not %eof(wmmgl);
              If dtCYMD(scEfDte) <> MGefdt;    // Prior record with same MCO code and different date
                 If (MGexdt = 0 and scExDte = 0)
                  OR ((MGexdt=0 and scExDte<>0) and dtCYMD(scExDte) >= MGefdt)
                  OR ((MGexdt<>0 and scExDte=0) and dtCYMD(scEfDte) <= MGexdt);
                    erExdte = *on;
                    erEfdte = *on;
                    msEfDte = 'WC31321';
                    msExDte = 'WC31321';
                    isErrMcoEdt = *on;
                    Leave;
                 Endif;
                 If (MGexdt <> 0 and scExDte <> 0);
                    If (dtCYMD(scEfDte) > MGexdt OR dtCYMD(scExDte) < MGefdt);

                    Else;
                       erExdte = *on;
                       erEfdte = *on;
                       msEfDte = 'WC31321';
                       msExDte = 'WC31321';
                       isErrMcoEdt = *on;
                       Leave;
                    Endif;
                 Endif;
              Endif;
              Reade keyWMMGL wmmgl;
           Enddo;
        Endif;


        // Name can not be blank
        If scNamee = *blanks;
           erNamee = *on;
           msNamee = 'WCR0503';
           isErrMcoEdt = *on;
        Endif;

        // Address can not be blank
        If scAdr1e = *blanks;
           erAdr1e = *on;
           msAdr1e = 'WCR0503';
           isErrMcoEdt = *on;
        Endif;

        If scCtye = *blanks;
           erCtye = *on;
           msCtye = 'WCR0503';
           isErrMcoEdt = *on;
        Endif;

        If scSte = *blanks;
           erSte = *on;
           msSte = 'WCR0503';
           isErrMcoEdt = *on;
        Endif;

        If scZipe= *zeros;
           erZipe= *on;
           msZipe= 'WCR0556';
           isErrMcoEdt = *on;
        Endif;


      // Return *on indicator when No error exists.
        If isErrMcoEdt  = *on;
           return *off;
        Else;
           return *on;
        Endif;

      /End-Free
     p ValidateMcoCd   e

      //===================================================================
      // Validate Reserve Cushion Details addition information.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateRsvCus  b
     d ValidateRsvCus  pi              n
     d strmode                        1
     d isError         s               n   inz
      /Free

        isError = *off;
       // Initialize all the messageId and error indicators.
        erEfdtAE= *off;
        msEfdtAE = *blanks;
        erRvcdAE= *off;
        msRvcdAE = *blanks;

        If strmode <> 'C';
          // Validate Payments Mode on Reserve Code
           If scRvcdAE = *blanks;
              msRvcdAE = 'WCR0513';
              erRvcdAE = *on;
              isError = *on;
           Endif;

          // Validation Check for Effective Date
           erEfdtAE = ValidDate(scEfdtAE:charMsg);
           If erEfdtAE = *on;
              msEfdtAE = charMsg;
              isError = *on;
           Endif;
        Endif;


      // Return *on indicator when No error exists.
        If isError = *on;
           return *off;
        Else;
           return *on;
        Endif;

      /End-Free
     p ValidateRsvCus  e


      //*** Begin Add *** 09/26/22 *******************************************
      //===================================================================
      // Validate State Compensation Benefit Info.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateSCB     b
     d ValidateSCB     pi              n
     d strmode                        1
     d isErrSCBEdt     s               n   inz
      /Free

       isErrSCBEdt = *off;
      // Intialize all the messageId and error indicators.
       erEfdtESCB = *off;
       msEfdtESCB = *blanks;
       erStESCB = *off;
       msStESCB = *blanks;
       erCmprE = *off;
       msCmprE = *blanks;
       erPacpE = *off;
       msPacpE = *blanks;

       If strmode = 'A';      //Add Mode
         // State can not be blank
          If scStESCB = *Blanks;
             erStESCB = *on;
             msStESCB = 'WCR0503';
             isErrSCBEdt = *on;
          Endif;
         // Validation Check for Effective Date
          erEfdtESCB = ValidDate(scEfdtESCB:charMsg);
          If erEfdtESCB = *on;
             msEfdtESCB = charMsg;
             isErrSCBEdt = *on;
          Endif;
         // Record Must Not Already Exist for st/efdt
          If isErrSCBEdt = *off;
             KeyEfdt = dtCYMD(scEfDtESCB);
             Chain keyWdrbl1 prb00;
             If %found(wdrbl1);
                erStESCB = *on;
                msStESCB = 'WC30005';
                erEfDtESCB = *on;
                msEfDtESCB = 'WC30005';
                isErrSCBEdt = *on;
             Endif;
          Endif;
       Endif;                      //Add mode

       // Compensation Reimbursement Percentage should not be zero.
       If scCmprE = *zeros;
          erCmprE = *on;
          msCmprE = 'WCR0502';
          isErrSCBEdt = *on;
       Endif;
       // Maximum compensation rate should not be zero.
       If scPacpE = *zeros;
          erPacpE = *on;
          msPacpE = 'WCR0502';
          isErrSCBEdt = *on;
       Endif;


      // Return *on indicator when No error exists.
       If isErrSCBEdt = *on;
          return *off;
       Else;
          return *on;
       Endif;

      /End-Free
     p ValidateSCB     e

      //===================================================================
      // Validate State Compensation Rate By Benefit Info.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateSCBR    b
     d ValidateSCBR    pi              n
     d strmode                        1
     d isErrSCBREdt    s               n   inz
      /Free

       isErrSCBREdt = *off;
      // Intialize all the messageId and error indicators.
       erDtESCBR  = *off;
       msDtESCBR  = *blanks;
       erStESCBR = *off;
       msStESCBR = *blanks;
       erCmpRate = *off;
       msCmpRate = *blanks;
       erBenTyp  = *off;
       msBenTyp  = *blanks;

       If strmode = 'A';      //Add Mode
         // State can not be blank
          If scStESCBR = *Blanks;
             erStESCBR = *on;
             msStESCBR = 'WCR0503';
             isErrSCBREdt = *on;
          Endif;
         // Validation Check for Effective Date
          erDtESCBR  = ValidDate(scdtESCBR:charMsg);
          If erDtESCBR  = *on;
             msDtESCBR  = charMsg;
             isErrSCBREdt = *on;
          Endif;
         // Benefit type cannot be blank
          If scBenTypE = *Blanks;
             erBenTyp  = *on;
             msBenTyp  = 'WCR0503';
             isErrSCBREdt = *on;
          Endif;
         // Record Must Not Already Exist for st/efdt/bentyp
          If isErrSCBREdt = *off;
             KeyEfdt = dtCYMD(scDtESCBR);
             Chain keyWdr1l pr100;
             If %found(wdr1l);
                erStESCBR = *on;
                msStESCBR = 'WC30005';
                erDtESCBR = *on;
                msDtESCBR = 'WC30005';
                erBenTyp  = *on;
                msBenTyp  = 'WC30005';
                isErrSCBREdt = *on;
             Endif;
          Endif;
       Endif;                      //Add mode

       // Benefit Rate should not be zero.
       If scERtSCBR = *zeros;
          erCmpRate = *on;
          msCmpRate = 'WCR0502';
          isErrSCBREdt = *on;
       Endif;

      // Return *on indicator when No error exists.
       If isErrSCBREdt = *on;
          return *off;
       Else;
          return *on;
       Endif;

      /End-Free
     p ValidateSCBR    e

      //*** End   Add *** 09/26/22 *******************************************
      // utility procedure definitions
      /COPY SCOPYCOMPI

