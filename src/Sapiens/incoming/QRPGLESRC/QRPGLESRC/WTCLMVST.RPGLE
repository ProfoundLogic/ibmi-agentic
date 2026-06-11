     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     h/TITLE WTCLMVST - Claim Visit Tracking
     h/DEFINE   PROFOUNDUI
     h DFTACTGRP(*no) ACTGRP(*caller)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h/TITLE WTCLMVST - Visit Tracking - Claims
     h**********************************************************************
     h*
     h*  Description:  This Program Will Display Visits for Claims
     h*
     h*  Called from: Profound UI
     h*  Calls to: RTVCLTID
     h*    RTVCLTID
     h*
     h*  Created on 1/29/2015 by C:062 for Project 114WGU
     h**********************************************************************
     h* Revisions:
      *
      * 03/06/25 C:291 Project: SY450
      *                Add button on Service Tracking grid should not
      *                be available in Inquiry mode.
      *
      * 08/21/23 C:300 Support: 113191 (WC2395)
      *                Recompile over changes in WTVSTTRK DSPF
      *
      * 07/28/23 C:300 Support: 113102 (WC2395)
      *                Recompile over changes in WTVSTTRK DSPF
      *
      * 06/08/23 C:342 Project: WC2747
      *                For Recompilation, changes in WTVSTTRKSP copybook.
      *
      * 10/17/22 C:291 Support: 103956
      *                Moved some definitions to WTVSTTRKSD.
      *
      * 09/08/22 C:062 Ticket: 103606(099)
      *                Add data structure to WMVIP.
      *
      * 06/21/21 C:298 Project: SY426
      *                For Recompilation, changes in WTVSTTRKSP copybook.
      *
      * 09/26/19 C:291 Project: 129WVN
      *                Add LF WMVIL7 for multiple visit tracking per day.  *
      *                                                                    *
      * 08/08/19 C:212  Support: 81206
      *                 ReCompile only (for WTVstTrkSP changes to fix Visit Traking)
      *                                                                    *
      * 07/10/19 C:300 Support: 80203 (LUBA)                               *
      *                Allow Add/update when accessing Visit Tracking      *
      *                Grid in inquiry mode                                *
      *                                                                    *
      * 06/18/19 C:300 Support: 80204 (LUBA)                               *
      *                Add WTVSTTRK program changes to recompile over      *
      *                WTVSTTRKSD and WTVSTTRKSP copybook changes          *
      *                                                                    *
      * 01/22/19 C:180 PHW76434                                            *
      *                Prevent compile error because of file added to      *
      *                WTVSTTRKSP.                                         *
      *                                                                    *
      * 10/17/18 C:079 Support: 146/73955                                  *
      *                Recompile due to increasing drop down field size    *
      *                                                                    *
      * 01/05/18 C:291 Support: 63337                                      *
      *                Recompile over change to WTVSTTRK screen file.      *
      *                Changed scEdtDiv Value property to "Show            *
      *                Zero Balance".                                      *
      *                                                                    *
      * 07/18/16 C:308 Project: 114WXY                                     *
      *              Removed sPrModule.                                    *
      *                                                                    *
      * 03/23/16 C:308 Project: ELEVATE                                    *
      *                Recompile Only                                      *
      *                                                                    *
      * 02/10/16 C:242 PW37116                                        *
      *                DBA/Fed Names and Links for Quote not working  *
      *                correctly.                                     *
      *                                                               *
      * 01/07/16 C:65  Project: WC2401                                     *
      *                Change the coding done under WC2254 to call an      *
    *                API instead of directly calling the various         *
      *                programs for windows here.                          *
     h*
     h*  06/24/15 C:62  Support: 35406(025)
     h*                 Populate the client id and rrn.
     h*
     h*  04/16/15 C:65  Project: SY251
     h*                 Add API call to launch new Department letters.
     h*                 Populate the RRN for the action panel as well.
     h*
     h**********************************************************************
     fwtVstTrk  cf   e             WorkStn Handler('PROFOUNDUI(HANDLER)')
     f                                     sfile(SflVstTrk:RrnVst)
     f                                     sfile(sflAgtUndw:RrnAgtUnd)
     f                                     sfile(sflAgtProd:RrnAgtPrd)
     fwtActPnl  cf   e             WorkStn
     f                                     Handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdClmPnl)
     fwmvip     uf a e             disk    rename(pvi01:pvi0p)
     f*** Begin Add *** 09/08/22 *******************************************
     f                                     infds(vimds8)
     f*** End   Add *** 09/08/22 *******************************************
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
     fwmeml     if   e           k disk    rename(pem01:pem00)
     fwmrpl     if   e           k disk    rename(prp01:prp00)
     fwmall     if   e           k disk    rename(pal01:pal00)
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
     fwmcmp     if   e             disk    rename(pcm01:pcm0p)
     fwmcdl     if   e           k disk    rename(pcd01:pcd00)
     fwdelp     if   e             disk    rename(pel01:pel0p)
      ********* Begin Chg *** 02/10/16 **********************************
     F****wdell     if   e           k disk    rename(pel01:pel00)
     f****                                     infds(ELfds)
     fwdellq    if   e           k disk    rename(pel01:pel00q)
     f                                     infds(ELfdsQ)
     f                                     prefix(E1:2)
      ********* End   Chg *** 02/10/16 **********************************
      ********* Begin Add *** 01/22/19 **********************************
     Fwdell     if   e           k disk    rename(pel01:pel00)
     F                                     infds(ELfds)
      ********* End   Add *** 01/22/19 **********************************
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
     d*** Begin Add *** 09/08/22 *******************************************
     d/copy sPrGenPop
     d*** End   Add *** 09/08/22 *******************************************

     d* Main Processing
     d Main            pr
     d  inCategory                   10a   const

      * Program Start
     d ProgramStart    pr

     d QCmdExc         pr                  ExtPgm('QCMDEXC')
     d  command                            const like(strCommand)
     d  length                       15  5 const
      ***** Beg Add ***** 09/08/22 ****************************
      ***** Beg Del ***** 10/17/22 ****************************
     d*** ChkStack        pr                  extpgm('SRCHKPGM')
     d***  outPgmName                   10    const
     d***  isFound                       1
      ***** End Del ***** 10/17/22 ****************************
      ***** End Add ***** 09/08/22 ****************************

     d/copy sPrAtrium
     d/copy SPrCompany
     d/copy sPrDate
     d/copy sPrString
      ***** Beg Del ***** 07/18/16 **************************************************
     d****/copy sPrModule
      ***** End Del ***** 07/18/16 **************************************************
     d/copy wprUsrPrms
     d/copy wprEmplr
     d/copy wprClaims
     d/copy wprAgent
     d/copy wprLoc
     d/copy wprGroup
     d/copy wPrContact

     d* Load Action Panel procedure Prototype and variables
     d/Copy sCopyClmPr
       //*** begin add ***  04/16/15  ****************************************
     d/copy sPrLetter
       //*** begin add ***  04/16/15  ****************************************

     D clientID        s              3    Inz('000')
     d @@Next          s             10
     D @@Qte           s              1
     D intCo#          s              3  0
     D strCommand      s            255
      ***** Beg Add ***** 09/08/22 ****************************
      ***** Beg Del ***** 10/17/22 ****************************
     d*** Callpgm         s              1
     d*** Callpgm2        s              1
     d*** saveDT          s                   like(VIdt)
     d*** saveTM          s                   like(VItime)
     d*** flgRcdUpdated   s              1
      ***** End Del ***** 10/17/22 ****************************
      ***** End Add ***** 09/08/22 ****************************

      * Key Lists
      *
     C     keyWMEML      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMemp#
     C                   Kfld                    CMdiv

      * Entry of Program.

     c     *entry        plist
     C                   PARM                    @@rrn             9            wdelp
     C                   PARM                    @@mode            1            mode
     C                   PARM                    @@Next           10            next pgm call
     C                   PARM                    @@TabInd          5            tab index
      /free

       Main('Claims');

       *InLr = *On;

      /end-free

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b                   Export
     d Main            pi
     d  inCategory                   10a   const

      /free
       @@Category = inCategory;

       ProgramStart();

       btnRefresh = *on;

       DoU ExitNow = *On;

          if btnRefresh = *on;
             ClrVstSubFiles();
             //****** Begin Add *** 07/10/19 **********************************
             visVstAdd  = *on;
             //*** Beg Add ***** 03/06/25 ***********************
             If $$Mode = 'D';
                visVstAdd = *Off;
             Endif;
             //*** End Add ***** 03/06/25 ***********************
             visVstEdit  = *on;
             visVstDel  = *on;
             //****** End   Add *** 07/10/19 **********************************
             LoadVisitGrid();
             if (onLoadRrn <> 0);
               LoadVisitDtl(OnLoadRrn);
             else;
               ClearVisitDtl();
               fSetVstDtX = vstScreenX;
               fSetVstDtH = 1000;
             endif;
             btnRefresh = *off;
          endif;
          Exsr ExfmtScreens;

          Select;

       //*** begin chg ***  01/07/16  ****************************************
       //*** begin add ***  04/16/15  ****************************************
       //*****   When btnDocWndw = *on;
       //*****      btnDocWndw = *off;
       //*****      DocWindow(scActRRN:'CS');
       //*** end add *****  04/16/15  ****************************************
          When btnShowWin = *on;
             PopupWindow(txtShowWin:scActRRN:'CS':txtWinData);
             btnShowWin = *off;
       //*** end chg *****  01/07/16  ****************************************

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
             If scMenuRsp <> ' ';
                Leave;
             EndIf;

          Endsl;
       Enddo;

       If %Parms()>=1;
          @@Next=scMenuRsp;
       EndIf;

       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

          if fSetVstDtY < 320;
             fSetVstDty = 320;
          endif;
          tabHeight =  fSetVstDtH + fSetVstDty - 55;
          if tabHeight < 800;
             tabHeight = 800;
          endif;
          aPnlHeight = tabHeight + 60;
          Write rcdClmPnl;
          Write rcdVstPnl;
          Exfmt rcdVstTrk;
          Read  rcdClmPnl;
          Read  rcdVstPnl;

       Endsr;

      /end-free
     P Main            E
       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi
      /free

       //Load default values or passed in values
       If q1Prms > 0;
          $$RRN = %int(@@RRN);
          $$Mode = @@Mode;
          If ($$Mode = 'C') or ($$Mode = 'A');
             isEditable = *on;
          endif;
       Else;
          $$RRN = 41;
          $$Mode = 'C';
       Endif;

       // Access the claim record
       Chain(n) $$RRN pcm0p;

       //*** Begin Add *** 06/24/15 ******************************
       // Get client ID
       RtvClientID(clientID);

       // Populate hidden fields
       scActCltId=clientID;
       scActRrn=$$rrn;
       //*** End   Add *** 06/24/15 ******************************

       // Subset to this claim
       ssGroup  = CMfnd;
       ssClaim# = %EditC(CMfnd:'X') + %EditC(CMfyr:'X') + %EditC(CMcase:'X');

       If q1prms >= 1;
       Endif;

       // Load Action panel with the Company Name.
       intCo# = userCo#(q1user);
       scCO# = intCO#;
       // Get company name
       //scCompName = GetCompNam(intCo#);
       today = DateTo7(%date());

       //*** begin add ***  04/16/15  ****************************************
       scActRrn= $$RRN;
       //*** end add *****  04/16/15  ****************************************

       // Set the x-coordinate
       vstScreenX = 250;
       fSetPolDtY = 320;
       fSetClmDtY = 320;
       fSetAgtDtY = 320;
       fSetVstDtY = 320;

       // Load the action panel for both tabs
       LoadActPan();

       // Initialize Setup
       InzSetup();

      /end-free
     p ProgramStart    e
       //===================================================================
       //===================================================================
       // * utility procedure definitions
       //===================================================================
      /Copy sCopyClmPi
       //===================================================================
       // * General Visit Tracking Procedures
       //===================================================================
      /Copy wtVstTrkSP
