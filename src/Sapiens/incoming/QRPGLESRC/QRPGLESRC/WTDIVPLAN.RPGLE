     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H/TITLE WTDIVPLAN - Dividend Plan Information
     H DFTNAME(WTPOLCONT)
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO) ACTGRP('QILE')
     H/DEFINE   PROFOUNDUI
     H BNDDIR('WTBNDDIR')
      *===============================================================*
      *  Created   : 05/27/13                                         *
      *  Programmer: C:190                                            *
      *  Project   : 147WAI                                           *
      *===============================================================*
      *  Revisions:                                                   *
     h*                                                                    *
     h*  03/21/22 C:333 Project: SY434                                     *
     h*                 - Update Grids for Sorting Filtering Upgrade       *
     h*                                                                    *
     h*    01/07/16 C:65  Project: WC2401                                  *
     h*                   Recompile Only                                   *
      *                                                                    *
      *  09/17/15 C:302 Project: wc2254                                    *
      *                 Recompile only                                     *
     h*
     h*  03/06/15 C:65  Project: WC2247
     h*                 Change program to use WCMS action panel, not system,
     h*                 since this is a WCMS project.
      *                                                               *
      *  09/09/14 C:295 Project: WC2247 Priss: 12                     *
      *                 The action panel should have a same           *
      *                 colour through out in the new PV1 Code screen.*
      *                 (For Compilation Only)                        *
      *                                                               *
      *  07/26/13 C:279 Project: WC2197                               *
      *                 Modified to use varibale field for Action     *
      *                 panel heading.                                *
      *                                                               *
      *===============================================================*
     fwtdivplan cf   e             workstn extfile('WTDIVPLAN')
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflPlnCode:rrnPlnCode)
     f                                     Sfile(sflPremRng:rrnPremRng)
     f*** begin chg ***  03/06/15  *****************************************
     f*****stactpnl  cf   e             workstn
     f*****                                     handler('PROFOUNDUI(HANDLER)')
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)
     f*** end chg *****  03/06/15  *****************************************
     fwtdvp1    if   e             disk    rename(pdv01:pdv00)
     F                                     infds(dbfds)
     fwddvl     if   e           k disk    rename(pdv01:pdv01)
     F                                     infds(dbfds1)
     fsmupl     if   e           k disk    rename(pup01:pup00)

     d psds          esds                  extname(szq1p)
     D dbfds           ds
     D  dbrrn                397    400b 0
     D dbfds1          ds
     D  dbrrn1               397    400b 0

     d ##mode          s              1    inz
     d addMode         s               n   inz(*off)
     d chgMode         s               n   inz(*off)
     d dspMode         s               n   inz(*off)
     d reloadFlg       s               n   inz(*on)
     D qteMode         s               n   inz(*off)
     D quoteMode       s              1    inz(' ')
     d clientID        s              3    inz('000')
     d mode            s              1
     d process         s               n   inz(*off)
     d xxRrn           s              9  0
     d @@mode          s              1
     d @@next          s             10
     d inCO#           s              3  0
     d inRRN           s              9  0
     d inPlanID        s             10
     d inqteMode       s              1
     d svrrnPlnIDCr    s                   like(rrnPlnIDCr)

     d* Clear Subfiles
     d ClrSubFiles     pr
     d* get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d outCltID                       3
     d* load Plan Code grid
     d loadPlanCode    pr
     d* load Premium Range grid
     d loadPremRange   pr
     d* main Processing
     d main            pr
     d* program Start
     d programStart    pr
     d processMe                       n
     d* process addition of contact
     d processAdd      pr
     d* process other options
     d procOther       pr
     d RcdDivdPlnDsp   pr
     d  indValue                      1n   const

     d/copy sPrAtrium
     d/copy sPrDate
     d/copy sPrGenPop
     d/copy sPrGenErr
     d/copy wprDivPop
      *** begin add ***  03/06/15  *****************************************
     d/copy sPrCompany
      *** end add *****  03/06/15  *****************************************

     d/copy wPrUsrPrms
    
     c* Entry of Program.
     c     *Entry        Plist
     c                   Parm                    @@mode                         mode
     C                   PARM                    @@Qte             1            quote

      /free

       // main program processing
       rrnPlnIDCr  = 1;
       // Program Mode.
       If %parms >= 1;
          mode = @@mode;
       Endif;
       If q1prms >= 2;
          quoteMode = @@qte;
          Select;
             When @@qte='Q';
                qteMode=*on;
          Endsl;
       Endif;
       scEnv = quoteMode;

       Chain Q1user PUP00;
       inCO# = UPco#;
       //*** begin chg **** 03/06/15 ***********************************
       // Get company name
       clear scCompName;
       scCompName = GetCompNam(inCo#);
       aPnlHeight = 500;
       //*** end chg ****** 03/06/15 ***********************************

       Select;
       When mode = 'D';
          dspMode = *On;
          dspVisb = *Off;
       When mode = 'A';
          addMode = *On;
          dspVisb = *On;
       When mode = 'C';
          chgMode= *On;
          dspVisb = *On;
       Endsl;
       scActMode = mode;

       GetClientID(ClientID);
       scActCltId=ClientID;

       ProgramStart(process);

       If process;
          Main();
       Endif;

       *InLr = *On;
       Return;

      /end-free
       //===================================================================
       // ClrSubFiles - Clears SubFiles
       //===================================================================
     P ClrSubFiles     B
     D ClrSubFiles     PI
      /Free

       rrnPlnCode = 0;
       rrnPremRng = 0;
       clrPlnCode = *On;
       clrPremRng = *On;
       RcdDivdPlnDsp(*off);
       Write RcdPlnCode;

      /End-Free
     P ClrSubfiles     E
       //===================================================================
       // Load Dividend Plan Grid
       //===================================================================
     P LoadPlanCode    B
     d LoadPlanCode    PI
      /free

       // clear the subfile.
       RcdDivdPlnDsp(*off);
       clrPlnCode = *On;
       Write RcdPlnCode;
       clrPlnCode = *Off;
       RcdDivdPlnDsp(*on);
       rrnPlnCode = 0;
       plnCdeVal = *Off;

       setll 1 pdv00;
       read pdv00;
       dow not %eof(Wtdvp1);

          clear sflPlnCode;
          scPlanID   = DVplid;
          scPlanDesc = DVpdsc;

          Select;
          When DVptyp = 'F';
             scPlanType = 'Flat';
          When DVptyp = 'S';
             scPlanType = 'Sliding Scale';
          When DVptyp = 'C';
             scPlanType = 'Combo';
          Endsl;

          Select;
          When DVgdiv = 'Y';
             scGrouped  = 'Yes';
          Other;
             scGrouped  = ' ';
          Endsl;

          scEffDate  = dtMDYY(DVefdt);
          scExpDate  = dtMDYY(DVexdt);

          Select;
          When DVauto = 'Y';
            scAutoRnw  = 'Yes';
          Other;
            scAutoRnw  = ' ';
          Endsl;

          scPlnCRRN# = dbrrn;
          rrnPlnCode = rrnPlnCode + 1;
          Select;
          When rrnPlnIDCr = rrnPlnCode;
             scPlnRowSl = 'A';
             nxtChgDvPl = *on;
          Other;
             Clear scPlnRowSl;
          Endsl;

          If mode = 'D';
             dspVisb = *Off;
          Else;
             dspVisb = *On;
          Endif;
          Write sflPlnCode;
          Read pdv00;
       Enddo;

       // Load Premium Range Information
       Chain rrnPlnIDCr sflPlnCode;
       If rrnPlnIDCr <> 1 and rrnPlnIDCr = 0;
          Chain svrrnPlnIDCr sflPlnCode;
       Endif;
       If %found;
          LoadPremRange();
       Endif;

       rrnPlnCode = 1;
       If rrnPlnIDCr <> 0;
          svrrnPlnIDCr = rrnPlnIDCr;
          rrnPlnCode   = rrnPlnIDCr;
       Endif;

      /end-free
     P LoadPlanCode    E
       //===================================================================
       // Load Premium Range Grid
       //===================================================================
     P LoadPremRange   B
     D LoadPremRange   PI
      /free

       // clear the subfile.
       RcdDivdPlnDsp(*off);
       clrPremRng = *On;
       Write RcdPlnCode;
       clrPremRng = *Off;
       RcdDivdPlnDsp(*on);
       rrnPremRng = 0;
       plnCdeVal = *On;

       Setll (inCO#:scPlanId) PDV01;
       ReadE (inCO#:scPlanId) PDV01;
       dow not %eof;
          clear sflPremRng;
          scMinLevel = DVmnlv;
          scMaxLevel = DVmxlv;
          scPrmCRRN# = dbrrn1;
          rrnPremRng = rrnPremRng + 1;
          If rrnPremRng = 1;
             scPrmRowSl = 'A';
          Else;
             clear scPrmRowSl;
          Endif;
          If mode = 'D';
             dspVisb = *Off;
          Else;
             dspVisb = *On;
          Endif;
          Write sflPremRng;
          ReadE (inCO#:scPlanId) PDV01;
       Enddo;
       rrnPremRng = 1;

      /end-free
     P LoadPremRange   E
       //===================================================================
       // Main - Main processing
      //===================================================================
     P Main            B
     d Main            PI

      /free
       // Process until exit button is pressed.
       Dou ExitNow = *On;

          // Don't reload if editing premium range.
          If reloadFlg = *off;
             reloadFlg = *on;
          Else;
             ClrSubfiles();
             LoadPlanCode();
          Endif;

          // Process Dividend screen.
          Exsr ExfmtScreens;

          Select;
          // do nothing - catch tab click event
          When tabClick = *on;
             tabClick = *off;
          When btnRefresh = *On;
             btnRefresh =*Off;
          //*  Add new dividend record.
          When BtnPnCdAdd = *on;
             processAdd();
          Other;
          procOther();
          Endsl;

       Enddo;
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;
          Exsr WriteAction;
          Write rcdTabPan;
          Exfmt RcdPlnCode;
          Exsr ReadAction;
          Read rcdTabPan;
       Endsr;
       //===================================================================
       // Write action panel;
       //===================================================================
       BegSr WriteAction;
          //*** begin chg **** 03/06/15 ***********************************
          //****** Beg Add ********* 07/26/13 *************************
          //*****scActPnHdg = 'Dividend Plan Codes';
          //****** End Add ********* 07/26/13 *************************
          //*****Write rcdSysTbl;
          Write rcdComPnl;
          //*** end chg ****** 03/06/15 ***********************************
       Endsr;
       //===================================================================
       // Read action panel;
       //===================================================================
       BegSr ReadAction;
          //*** begin chg **** 03/06/15 ***********************************
          //*****Read rcdSysTbl;
          Read rcdComPnl;
          //*** end chg ****** 03/06/15 ***********************************
       Endsr;
      /end-free
     P Main            E
       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     P ProgramStart    B
     d ProgramStart    PI
     d processMe                       n
      /free
       // scActRrn=xxRrn;
       processMe = *on;
       Return;
      /end-free
     P ProgramStart    E
       //===================================================================
       // processAdd - process addition of contact
       //===================================================================
     P processAdd      B
     d processAdd      PI
      /free
       Select;
       When BtnPnCdAdd = *On;
          inRRN = 0;
          DivdPlanEdit(inCO#:inRRN);
          BtnPnCdAdd = *off;
          loadPlanCode();
       Endsl;

      /end-free
     P processAdd      E
       //===================================================================
       // procOther - process other options
       //===================================================================
     P procOther       B
     d procOther       PI
      /free

       // processing for Dividend Edit mode/link
       Readc sflPlnCode;
       Select;
       When lnkPnCdEdt = *on;
          inRRN = scPlnCRRN#;
          DivdPlanEdit(inCO#:inRRN);
          lnkPnCdEdt = *off;
          loadPlanCode();
       // processing for Premium Range Add
       When lnkPremAdd = *On;
          inRRN = 0;
          inPlanID = scPlanID;
          DivPremLevel(inCO#:inRRN:inPlanID:inQteMode);
          lnkPremAdd = *off;
       Endsl;

       // processing for Premium Range Edit
       Readc sflPremRng;
       If lnkPremEdt = *on;
          inRRN = scPrmCRRN#;
          inPlanID = scPlanID;
          DivPremLevel(inCO#:inRRN:inPlanID:inQteMode);
          lnkPremEdt = *off;
          loadPremRange();
          reloadFlg = *off;
       Endif;

      /end-free
     P procOther       E
       //===================================================================
       // Load display indicators for sub-files in RcdPlnCode
       //===================================================================
     P RcdDivdPlnDsp   b
     D RcdDivdPlnDsp   pi
     D  indValue                      1n   const
      /Free
       dspPlnSub  = indValue;
       dspPlnCode = indValue;
       dspPremSub = indValue;
       dspPremRng = indValue;
      /End-Free
     P RcdDivdPlnDsp   e
       //===================================================================
