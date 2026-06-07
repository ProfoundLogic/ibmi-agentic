      * (c) 2023 Sapiens. All Rights Reserved.                             *
     h/TITLE WTVSTTRK - Profound Visit Tracking
     h/DEFINE   PROFOUNDUI
     h DFTACTGRP(*no) ACTGRP(*caller)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h/TITLE WTVSTTRK - Visit Tracking - All
     h**********************************************************************
     h* Programmers Notes:
     h*
     h* Very Important:  When you save the DSPF  in Designer it will rename
     h* these subfile control records: QSFLCTL006, QSFLCTL007, QSFLCTL008.
     h* You will need to use another editor to name them back to the way
     h* they were.
     h*
     h**********************************************************************
     h*
     h*  Description:  This Program Will Display Visits
     h*
     h*  Called from: Profound UI
     h*  Calls to: RTVCLTID
     h*    RTVCLTID
     h*
     h*  Created on 1/29/2015 by C:062 for Project 114WGU
     h**********************************************************************
     h* Revisions:
     h*
     h*  09/28/23 C:333 Support: 113925 (re-compilation)
     h*                 Changes in WTVSTTRKSP copybook.
     h*
     h*  08/21/23 C:300 Project: WC2395, Support: 113191
     h*                 DSPF: Correct Export to Excel File Name
     h*                 from "Visit Tracking" to "Service Request Tracking"
     h*
     h*  07/28/23 C:300 Support: 113102 (WC2395)
     h*                 Apply Persist state property in sflVstTrk, sflAgtUndw
     h*                 and sflAgtProd grids
     h*
     h*  06/08/23 C:342 Project: WC2747
     h*                 For Recompilation, changes in WTVSTTRKSP copybook.
     h*
     h*  04/06/23 C:333 Support: 107461 (re-compilation)
     h*                 - Changing Tab Name from 'Visit Track Report' to
     h*                   'Service Request Tracking Report'.
     h*
     h*
     h*  10/17/22 C:291 Support: 103956
     h*                 Stop loading Service Request Tracking grid when
     h*                 it reaches 9999 records and display message to
     h*                 use filter to display other records.
     h*                 Also moved some definitions to WTVSTTRKSD.
     h*
     h*  06/24/22 C:300 Project: WC2395
     h*                 1) Agency links in Service Request Tracking Grid should not be
     h*                 visible when called from Work with Agent or Agency Maintenace
     h*                 2) Check If record has been updated by another user in Edit Mode
     h*                Made changes in WTVSTTRKSD and WTVSTTRKSP copybooks
     h*
     h*  12/29/21 C:300 Project: WC2395
     h*                 Call this program from new Agency Configuration program
     h*                 Enable Find and Filter options in the Grid in DSPF
     h*
     h*  06/21/21 C:298 Project: SY426
     h*                 Re-Brand and Config for Visit Tracking
     h*
     h*  09/26/19 C:291 Project: 129WVN
     h*                 Add LF WMVIL7 for multiple visit tracking per day.
     h*
     h*  08/08/19 C:212 Support: 81206
     h*                 ReCompile only (for WTVstTrkSP changes to fix Visit Traking)
     h*
     h*  07/10/19 C:300 Support: 80203 (LUBA)
     h*                Do not show an Add button on the Visit Tracking grid, When
     h*                called from the Menu for any type of Visit Tracking called.
     h*
     h* 06/18/19 C:300 Support: 80204 (LUBA)
     h*                Policy/Claim links in Visit Tracking menu call should
     h*                use the security available in Application Configuration.
     h*                If user does not have any access then link should not appear.
     h*                If user have access, select the link then they should go to
     h*                policy or claim in inquiry or edit depending on app config.
     h*                Made change in WTVSTTRKSD and WTVSTTRKSP copybooks
     h*
     h*  06/10/19 C:295 Support: 79465
     h*                 Recompile over change to WTVSTTRK screen file.
     h*                 Changed the drop down feilds to hold the full list
     h*                 set up in respective files.
     h*
      * 01/22/19 C:180 PHW76434                                            *
      *                Prevent compile error because of file added to      *
      *                WTVSTTRKSP.                                         *
     h*  10/17/18 C:079 Support: 146/73955
     h*                 Recompile due to increasing drop down field size
     h*
     h*  01/05/18 C:291 Support: 63337
     h*                 Recompile over change to WTVSTTRK screen file.
     h*                 Changed scEdtDiv Value property to "Show
     h*                 Zero Balance".
     h*
     h*  03/22/17 C:053 Support: 46241
     h*                 Recompile over changed copy module
      *                                                               *
      * 02/10/16 C:242 PW37116                                        *
      *                DBA/Fed Names and Links for Quote not working  *
      *                correctly.                                     *
      *                                                               *
      *    01/07/16 C:65  Project: WC2401                             *
      *                   Recompile Only                              *
      *                                                               *
      *    09/17/15 C:302 Project: WC2254                             *
      *                   Recompile Only                              *
     h*
     h**********************************************************************
     fwtVstTrk  cf   e             WorkStn Handler('PROFOUNDUI(HANDLER)')
     f                                     sfile(SflVstTrk:RrnVst)
     f                                     sfile(sflAgtUndw:RrnAgtUnd)
     f                                     sfile(sflAgtProd:RrnAgtPrd)
     fwtActPnl  cf   e             WorkStn
     f                                     Handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)
     fwmvip     uf a e             disk    rename(pvi01:pvi0p)
      ***** Beg Add ***** 06/24/22 ****************************
     f                                     infds(vimds8)
      ***** End Add ***** 06/24/22 ****************************
     fwmvil6    if   e           k disk    rename(pvi01:pvi06)
     f                                     infds(vimds6)
     fwmvil1    if   e           k disk    rename(pvi01:pvi01)
     f                                     infds(vimds1)
     fwmvil2    if   e           k disk    rename(pvi01:pvi02)
     f                                     infds(vimds2)
     fwmvil3    if   e           k disk    rename(pvi01:pvi03)
     f                                     infds(vimds3)
     fwmvil4    if   e           k disk    rename(pvi01:pvi04)
     f                                     infds(vimds4)
      *** Beg Add ***** 09/26/19 *************************************
     fwmvil7    if   e           k disk    rename(pvi01:pvi07)
     f                                     infds(vimds7)
      *** End Add ***** 09/26/19 *************************************
     fwtvil     if   e           k disk    rename(pvi01:ptvil)
     fwmeml     if   e           k disk    rename(pem01:pem00) usropn
     fwmrpl     if   e           k disk    rename(prp01:prp00)
     fwmall     if   e           k disk    rename(pal01:pal00) usropn
     fwtabl     if   e           k disk
     f                                     include(pcn01)
     f                                     rename(pcn01:pcn00)
     f                                     include(pna01)
     f                                     rename(pna01:pna00)
     f                                     include(ppa01)
     f                                     rename(ppa01:ppa00)
     f                                     include(pca01)
     f                                     rename(pca01:pca00)
     f                                     include(pat01)
     f                                     rename(pat01:pat00)
     fwtagl     if   e           k disk    rename(pag01:pag01)
     fwmael     if   e           k disk    rename(pae01:pae01)
     fstabl     if   e           k disk    rename(pst01:pst00)
     fwtgtl     if   e           k disk    rename(pgt01:pgt01)
     fsmupl1    if   e           k disk    rename(pup01:pup01)
     fsmupl     if   e           k disk    rename(pup01:pup00)
     fwmahl     if   e           k disk    rename(pah01:pah00)
     fwmagl     if   e           k disk    rename(pag01:pag00)
     fwmaal1    if   e           k disk    rename(paa01:paa01)
     fwmaal     if   e           k disk    rename(paa01:paa00)
     fwmapl     if   e           k disk    rename(pap01:pap00)
     fwmcml     if   e           k disk    rename(pcm01:pcm00)
     fwmcdl     if   e           k disk    rename(pcd01:pcd00)
     fwdelp     if   e             disk    rename(pel01:pel0p) usropn
      ********* Begin Chg *** 02/10/16 **********************************
     f****wdell     if   e           k disk    rename(pel01:pel00)
     f****                                     infds(ELfds)
     fwdellq    if   e           k disk    rename(pel01:pel00q) usropn
     f                                     infds(ELfdsQ)
     f                                     prefix(E1:2)
      ********* End   Chg *** 02/10/16 **********************************
      ********* Begin Add *** 01/22/19 **********************************
     Fwdell     if   e           k disk    rename(pel01:pel00)
     F                                     infds(ELfds)
      ********* End   Add *** 01/22/19 **********************************
     fwmcmp     if   e             disk    rename(pcm01:pcm0p)
     fwmagp     if   e             disk    rename(pag01:pag0p)
      ********* Begin Add *** 06/18/19 **********************************
     Fsdacl     if   e           k disk    rename(pac01:pac04)
      ********* End   Add *** 06/18/19 **********************************
      ********* Beg Add *** 06/21/21 **********************************
     fwtvipcfg  if   e           k disk    rename(pvi01:pvi0p_cfg)
     f                                     prefix(cfg_)
      ********* End Add *** 06/21/21 **********************************

     d psds          esds                  extname(SZQ1P)

     d/copy wtVstTrkSD
     d/copy wPrGroup
      * Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3

     d* Main Processing
     d Main            pr
     d  inCategory                   10a   const

      * Program Start
     d ProgramStart    pr

     d* Claim Search
     d ClmSearch       pr                  ExtPgm('WTSEARCH')
     D @@mode                         1
     D @@call                         1
     D @@rrn                          9
     D @@criteria                          likeds(dsSrchCrit)

     d QCmdExc         pr                  ExtPgm('QCMDEXC')
     d  command                            const like(strCommand)
     d  length                       15  5 const

      ***** Beg Add ***** 12/29/21 ****************************
      ***** Beg Del ***** 10/17/22 ****************************
     d*** ChkStack        pr                  extpgm('SRCHKPGM')
     d***  outPgmName                   10    const
     d***  isFound                       1
      ***** End Del ***** 10/17/22 ****************************
      ***** End Add ***** 12/29/21 ****************************

     d/copy SPrAtrium
     d/copy SPrCompany
     d/copy sPrDate
     d/copy sPrString
     d/copy sPrModule
     d/copy sprTeam
     d/copy wprUsrPrms
     d/copy wprEmplr
     d/copy wprClaims
     d/copy wprAgent
     d/copy wtSearchDs
      ***** Beg Add ***** 06/24/22 ****************************
     d/copy sPrGenPop
      ***** End Add ***** 06/24/22 ****************************

     d* Time Delay
     d TimeDelay       pr                  ExtPgm('TIMEDELAY')
     D @@minute                       3    Const
     D @@seconds                      3    Const

     D clientID        s              3    Inz('000')
     d @@Next          s             10
     D @@Qte           s              1
     D intCo#          s              3  0
     D strCommand      s            255
      ***** Beg Del ***** 10/17/22 ****************************
      ***** Beg Add ***** 12/29/21 ****************************
     d*** Callpgm         s              1
      ***** End Add ***** 12/29/21 ****************************
      ***** Beg Add ***** 06/24/22 ****************************
     d*** Callpgm2        s              1
     d*** saveDT          s                   like(VIdt)
     d*** saveTM          s                   like(VItime)
     d*** flgRcdUpdated   s              1    inz
      ***** End Add ***** 06/24/22 ****************************
      ***** End Del ***** 10/17/22 ****************************

      * Entry of Program.

     c     *entry        plist
     C                   PARM                    @@rrn             9            wdelp
     C                   PARM                    @@mode            1            mode
     C                   PARM                    @@Category                     category
     C                   PARM                    @@Assign                       assignment
     C                   PARM                    @@VstFrDt                      visit from date
     C                   PARM                    @@VstToDt                      visit to date
     c                   PARM                    @@isTeam                       isTeam
      /free

       // TimeDelay('001':'000');
       Main(@@category);

       *InLr = *On;
      /end-free

       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi
      /free

       empUsrOpn = *On;

       GetClientID(clientID);

       If q1prms >= 1;
         $$RRN = %dec(@@Rrn:9:0);
         scActMode = @@mode;
         $$Mode = @@Mode;
         if ($$Mode = 'C') or ($$Mode = 'A');
           isEditable = *on;
         endif;
       Endif;

       // Load Action panel with the Company Name.
       intCo# = userCo#(q1user);
       scCO# = intCO#;
       // Get company name
       scCompName = GetCompNam(intCo#);
       today = DateTo7(%date());

       // Load Agent Information
       If $$RRN <> 0 and @@Category = 'Agency';
          Chain $$RRN pag0p;
          If %Found(WMAGP);
             scCompName = AGname;
             Chain (AGco#:AGfein:AGmod) paa00;
          EndIf;
       EndIf;

       // Set the coordinates
       vstScreenX = 250;
       fSetPolDtY = 320;
       fSetClmDtY = 320;
       fSetAgtDtY = 320;
       fSetVstDtY = 320;

       IF (q1prms >= 7);
          vstClndrCall = 'Y';
          IF (@@isTeam = 'Y');
             isTeam = *on;
             Clear arrTeamList;
             ptrTeamList = %Addr(arrTeamList);
             GetUserTeamList(@@Assign:' ' :ptrTeamList :intCount :'Y');
          ENDIF;
       ENDIF;
       // Initialize Setup
       InzSetup();

       Return;

      /end-free
     p ProgramStart    e
       //===================================================================
       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b                   Export
     d Main            pi
     d  inCategory                   10a   const
      *
      /free

       @@Category = inCategory;

       ProgramStart();

       btnRefresh = *on;

       DoU ExitNow = *On;

          if btnRefresh = *on;
             ClrVstSubFiles();
             IF (isTeam = *off);
                LoadVisitGrid();
             ELSE;
                LoadVisitGrid(ptrTeamList:intCount);
             ENDIF;
             if (onLoadRrn <> 0);
                LoadVisitDtl(OnLoadRrn);
             else;
                ClearVisitDtl();
                fSetVstDtX = vstScreenX;
                fSetVstDtH = 50;
             endif;
             btnRefresh = *off;
          endif;
          Exsr ExfmtScreens;
          //*** Beg Add ***** 10/17/22 ******************************
          If isFilter = *On;
             vstTypFlt = *On;
          Endif;
          ///*** End Add ***** 10/17/22 ******************************

          Select;

          // Exit the program
          When exitNow = *on;
             Leave;

          // Do nothing - Catch tab click event
          When tabClick = *On;
             tabClick = *Off;

          When vstTypFlt = *On;
             SetVisitFilter();
             InzSubset();
             btnRefresh = *on;

          When clrVstFlt = *On;
             ClrVisitFilter();
             btnRefresh = *on;

          When btnRefresh = *On;
             //Loop will Refresh

          When BtnVstAdd  = *on;
             BtnVstAdd = *off;
             AddVisitDtl();
             btnRefresh = *on;

          When RRNVstRow <> 0;
             ProcessSFL();

          Other;

          //***** Beg Add ***** 12/29/21 ****************************
          If callpgm = *on;
             ExitNow = *On;
          endif;
          //***** End Add ***** 12/29/21 ****************************

          Endsl;
       Enddo;

       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

          if fSetVstDtY < 320;
             fSetVstDty = 320;
          endif;
          //****** Begin Add *** 07/10/19 **********************************
          // When Visit Tracking is called from Menu
          If @@rrn = '000000000';
             visVstAdd  = *off;
          EndIf;
          //****** End   Add *** 07/10/19 **********************************
          tabHeight =  fSetVstDtH + fSetVstDty - 55;
          if tabHeight < 800;
             tabHeight = 800;
          endif;
          aPnlHeight = tabHeight + 60;

          //***** Beg Add ***** 12/29/21 ****************************
          callpgm = *off;
          ChkStack('WTAGTSRVRT':CallPgm);
          //***** End Add ***** 12/29/21 ****************************

          //***** Beg Chg ***** 12/29/21 ****************************
          If callpgm = *off;
          //Write rcdComPnl;
             Write rcdComPnl;
          Endif;
          //***** End Chg ***** 12/29/21 ****************************
          Write rcdVstPnl;
          //*** Beg Chg ***** 10/17/22 ****************************
          //***Exfmt rcdVstTrk;
          //***** Beg Chg ***** 12/29/21 ****************************
          //***If callpgm = *off;
          //***//Read  rcdComPnl;
          //***  Read  rcdComPnl;
          //***Endif;
          //***** End Chg ***** 12/29/21 ****************************
          //***Read  rcdVstPnl;
          If isFilter = *On;
             Write rcdVstTrk;
          Else;
             Exfmt rcdVstTrk;
          Endif;
          If isFilter = *Off;
             If callpgm = *off;
                Read  rcdComPnl;
             Endif;
             Read  rcdVstPnl;
          Endif;
          //*** End Chg ***** 10/17/22 ****************************

       Endsr;

      /end-free
     P Main            E
       //===================================================================
       // * General Visit Tracking Procedures
       //===================================================================
      /Copy wtVstTrkSP
