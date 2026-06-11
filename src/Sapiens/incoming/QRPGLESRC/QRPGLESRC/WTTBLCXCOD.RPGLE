     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h/title WTTBLCXCOD - Cancellation Reason Codes
     h/define   PROFOUNDUI
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description:  This Program process the Cancellation Reason Codes
     h*
     h*  Called from: Menu
     h*  Calls to:
     h*
     h*  Created on 11/25/2015 by C:212 for Project: WC2399
     h**********************************************************************
     h*  Revisions:
     h*
     h* 09/18/25 C:342  Project: WC3014
     h*                          -Added Do Not Prorate Min column
     h*                          -Added Do Not Prorate Min prem checkbox RcdCxCdAdd
     h*
     h* 07/28/25 C:062  Project: 153WBJ
     h*                          -Added Late Fee                             edit issue
     h*                          -Added Late Fee A/R Code                    edit issue
     h*                          -Added Reinstatement A/R Code               edit issue
     h*
     h* 09/18/24 C:333  Support: 117544
     h*                          Cancellation reason code configuration date edit issue
     h*                          - DSPF changes:
     h*                           -Changed datatype for scAdEfdt and scCxCdEfDt.
     h*                           -Adding new error message for scAdEfdt.
     h*                           -Adding text transform for scAdCode (UpperCase)
     h*                            & adding text transform property for TxbAddCxCdCode.
     h*                           -Changed ID: OutDtlCxCdCode to DtlOutCxCdCode &
     h*                            OutDtlCxCdEfDt to DtlOutCxCdEfDt.
     h*
     h* 03/16/20 C:180  Project: WC2700 (Override Renewal Cancellations)
     h*                         -Add Override Code and Canc Notice day columns.
     h*          C:136          -Remove filter logic.
     h**********************************************************************
     f*
     fwttblcxcodcf   e             workstn extfile('WTTBLCXCOD')
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflCxCd:rrnCxCd)
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)

     fwtcrp     uf a e             disk    rename(pcr01:pcr0p)
     f                                     InfDs(dbfds1)
     fwtcrl     if   e           k disk    rename(pcr01:pcr01)
     fsmcol     if   e           k disk    rename(pco01:pco00)
     fwtgtl     if   e           k disk    rename(pgt01:pgt00)
     fststl3    if   e           k disk    rename(pst01:pst03)
     fssylf     if   e           K disk
     f                                     IGNORE(SBC01)
     f                                     IGNORE(SCS01)
     f                                     IGNORE(SDR01)
     f                                     IGNORE(SFB01)
     f                                     IGNORE(SGL01)
     f                                     IGNORE(SPC01)
     f                                     IGNORE(SP201)
     f                                     IGNORE(SRC01)
     f                                     IGNORE(SSY01)
     d* procedure definitions
     d*

     d* data structures
     d*

     d psds          esds                  extname(szq1p)

     d dbfds1          ds
     d  dbrrn1               397    400b 0

     d dspFilter       ds                  likerec(RcdCxCdFlt)

     d* constant declarations
     d*

     d* standalone variables
     d*

     d @@mode          s              1
     d @@TabIndex      s              5

     d ##CltId         s              3    inz

     d inmode          s              1    inz
     d inTabIndex      s              5  0 inz

     d isAddorEdit     s              1    inz
     d isValid         s               n   inz(*off)
     d isAutoReIns     s               n   inz(*off)
     d isFirstLoad     s               n   inz(*off)
     d kkCo#           s              3  0 inz
     d meetPay         s               n   inz(*off)
     d kkefdt          s                   like(CRefdt) inz
     d kkst            s                   like(STkey) inz
     d kkcode          s                   like(CRkey) inz
     d kktbid          s                   like(GTtbid) inz
     d kkkey           s                   like(GTkey) inz
     d kkwc            s                   like(WCkey) inz
      *** Beg Add ***** 09/18/24 ***********************************
     d charMsg         s            100
      *** End Add ***** 09/18/24 ***********************************
     d*** Begin Add *** 07/28/25 ***********************************
     d hld153          s                   Like(is153)
     d*** End   Add *** 07/28/25 ***********************************

     d* Clear Subfiles
     d ClrSubFiles     pr

     d* Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d ##CltId                        3

     d* Clear Filter - Visibility of button for Payment Value 1 Grid
     d LoadClrFlt      pr

     d* Load Cancellation Reason Codes Grid
     d LoadCnlRsnCd    pr

     d* Main Processing
     d Main            pr

      *** beg del ***** 03/16/20 **************
     d* Filter for Payment Value
     d*** MeetFlt         pr
      *** end del ***** 03/16/20 **************

     d* Program Start
     d ProgramStart    pr

     d* process addition
     d processAdd      pr

     d* Process other options
     d ProcOther       pr

     d* Validate Add/Edit screen
     d Validate        pr

     d* Add details
     d AddDetails      pr

     d* Move database record to screen
     d MoveDbSc        pr


     d/Copy sPrDate
     d/Copy sPrString
     d/Copy SPrCompany

     d/Copy wPrGroup
     d/Copy wPrRateSet
     d/Copy wPrUsrPrms
     d/Copy wTclmTblPr
      *** Beg Add ***** 09/18/24 ***********************************
     d/copy wPrPolErr
      *** End Add ***** 09/18/24 ***********************************
    
     c* Entry of Program.
     c     *Entry        Plist
     c                   Parm                    @@Mode
     c                   Parm                    @@TabIndex

      /free
       // Start Of Program
       ProgramStart();

       // Main Procesing
       Main();

       *inlr = *on;
       Return;
      /end-free

       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================

     p ProgramStart    b
     d ProgramStart    pi

      /free

       // Program Mode.
       If %Parms() >= 1;
          inmode = @@Mode;
       Endif;

       Select;
       When inmode = 'D';
          dspMode = *On;
          dspVisb = *Off;
       When inmode = 'A';
          dspMode = *Off;
          dspVisb = *On;
       When inmode = 'C';
          dspMode = *Off;
          dspVisb = *On;
       Endsl;

       // Tab Index Mode.
       If %Parms() >= 2;
          intabIndex = %dec(@@TabIndex:5:0);
       Else;
          intabIndex = 0;
       EndIf;

       // Load Action panel with the Company Name.
       // Get company number
       Clear scCompName;
       kkco# = userCo#(q1user);
       // Get company name
       scCompName = GetCompNam(kkco#);
       //*** Begin Add *** 07/28/25 ****************************************
       //*** aPnlHeight = 610;
       aPnlHeight = 670;
       //*** End   Add *** 07/28/25 ****************************************

       // Get the client id
       GetClientID(##CltId);

       Clear dspFilter;
       tabClick = *Off;
       // Default to On for first load of screen
       isFirstLoad = *on;

       // Check if Auto Reinstall is installed
       kkwc = 'WC';
       Chain(n) kkwc swc01;
       If %found (ssylf);
          If WCfl29 <> *blanks;
             isAutoReIns = *on;
          Else;
             isAutoReIns = *off;
          Endif;
       Endif;

       //*** Begin Add *** 07/28/25 ****************************************
       //
       // Check to see client ID
       Select;
       When ##CltId = '153';
          is153  = *On;
          hld153 = *On;
       Other;
          is153  = *Off;
          hld153 = *Off;
       EndSL;
       //*** End   Add *** 07/28/25 ****************************************

       Return;

      /end-free
     p ProgramStart    e

       //===================================================================
       // Main - Main processing
       //===================================================================

     p Main            b
     d Main            pi

      /free

       // Process until exit button is pressed.
       Dou ExitNow = *On ;

          // Load Filter Clear Button.
          LoadClrFlt();

          // Clear Sub-files
          ClrSubfiles();

          // Load Cancellation Reason Codes Grid
          LoadCnlRsnCd();

          // Process Cancellation Reason Code Screens
          Exsr ExfmtScreens;

          Select;
          // do nothing - catch tab click event
          When tabClick = *on;
             tabClick = *off;

          // Filter Processing.
          //*** Begin Delete *** 03/16/20 *******************************
          //When BtnFilter = *on;
          //   Dou BtnFltrCan = *on or BtnFltrApp = *on;
          //
          //      ExFmt RcdCxCdFlt;
          //      If BtnFltrApp = *on;
          //         dspFilter.scFltrSts = scFltrSts;
          //         dspFilter.scFltrSt = scFltrSt;
          //      Endif;
          //
          //      If BtnFltrCan = *on;
          //         scFltrSts = dspFilter.scFltrSts;
          //         scFltrSt = dspFilter.scFltrSt;
          //      Endif;
          //   Enddo;

          // Filter clear processing
          //When BtnClrFltr = *on;
          //   Clear dspFilter;
          //   Clear rcdCxCdFlt;
          //   dspClrFltr = *off;
          //*** End    Delete *** 03/16/20 *******************************

          When BtnRefresh = *On;
             BtnRefresh =*Off;

          // Add Cancellation Reason Code
          When scCxCdAdd = *on;
             Clear RcdCxCdAdd;
             //*** Begin Add *** 07/28/25 **********************************
             is153 = hld153;
             //*** End   Add *** 07/28/25 **********************************
             isAddorEdit = 'A';
             BtnAddSav = *off;
             BtnAddCan = *off;
             ProcessAdd();

          Other;
             ProcOther();

          Endsl;

       Enddo;

       //===================================================================
       // Write Screen Formats;
       //===================================================================

       BegSr ExfmtScreens;

       Select;

       When TabIndex = 0;
       //*** Begin Change *** 03/16/20 ************************************
       // TabHeight = 550;
       //*** Begin Chg *** 07/28/25 ***************************************
       //*TabHeight = 560;
          TabHeight = 620;
       //*** End   Chg *** 07/28/25 ***************************************
       //*** End   Change *** 03/16/20 ************************************
          Write rcdComPnl;
          Write rcdTabCxCd;
          Exfmt rcdCxCd;
          Read rcdComPnl;
          Read rcdTabCxCd;
       EndSl;

       Endsr;

      /end-free
     p Main            e

       //===================================================================
       // ClrSubFiles - Clears SubFiles
       //===================================================================

     p ClrSubFiles     b
     d ClrSubFiles     pi
      /Free

       rrnCxCd = 0;
       clrCxCd = *On;
       dspCxCdSub = *Off;
       dspCxCdCtl = *Off;
       Write rcdCxCd;

      /End-Free
     p ClrSubfiles     e

       //===================================================================
       // Clear Filter - Clears the Filter
       //===================================================================

     p LoadClrFlt      b
     d LoadClrFlt      pi

      /free

       // if first time screen load, set only Active records and display Filter button
       If isFirstLoad = *on;
          isFirstLoad = *off;
          dspFilter.scFltrSts = 'A';
          scFltrSts = 'A';
       EndIf;

       //*** Begin Del ***** 03/16/20 *************************
       // display clear filter button
       //If dspFilter.scFltrSt = *blanks and
       // dspFilter.scFltrSts = *blanks;
       //   dspClrFltr = *off;
       //Else;
       //   dspClrFltr = *on;
       //Endif;
       //*** End Del ***** 03/16/20 ****************************

      /end-free
     p LoadClrFlt      e

       //===================================================================
       // Load Cancellation Reason Codes Grid
       //===================================================================

     p LoadCnlRsnCd    b
     d LoadCnlRsnCd    pi

      /free

       // clear the subfile.
       Clear sflCxCd;
       clrCxCd = *Off;
       dspCxCdSub = *On;
       dspCxCdCtl = *On;
       Clear dbRrn1;

       Setll *start wtcrp;
       Read pcr0p;
       Dow Not %Eof(wtcrp);

         //*** beg del ***** 03/16/20 **************
         //***meetPay = *on;
         //*** MeetFlt();
         //*** If meetPay = *on;
         //*** end del ***** 03/16/20 **************

          // Write Cancellation Reason Code

          // State
          scCxCdSt = CRst;

          // Code
          //*** Beg Chg ***** 09/18/24 ***********************************
          //***scCxCdCd = CRkey;
          scCxCdCd = Upper(CRkey);
          //*** End Chg ***** 09/18/24 ***********************************

          // NCCI Code
          scCxCdNCCI = CRNCCI;

          // Description
          scCxCdDesc = CRdesc;

          // Effective Date
          //*** Beg Chg ***** 09/18/24 *************************************
          //***Monitor;
          //***   scCxCdEfDt = %date(CRefdt:*cymd);
          //***On-Error;
          //***   scCxCdEfDt = %date(01010001);
          //***EndMon;
          scCxCdEfDt = dtMDYY(CRefdt);
          //*** End Chg ***** 09/18/24 *************************************

          // Status
          If CRstat = 'A';
             scCxCdStat = 'Active';
          Else;
             scCxCdStat = 'Inactive';
          Endif;

          // Begin Add *** 03/16/20 *****************************************
          // Override Code, Description

          scCxOvrDsc = CRidov;
          kktbid = 'CXID';
          kkkey = scCxOvrDsc;
          chain (kkco#:kktbid:kkkey) wtgtl;
          If %found(wtgtl);
             scCxOvrDsc = CRidov + '-' + %Trim(GTdesc);
          Else;
             scCxOvrDsc = *blanks;
          Endif;

          //Notice Days
          scCxNDays = CRnotc;
          If CRnotc > *Zeros;
             visCxNdays = *On;
          Else;
             visCxNdays = *Off;
          Endif;

          // End   Add *** 03/16/20 *****************************************
          //*** Beg Add ***** 09/18/25 ***********************************
          If CRnopro = 'Y';
             scNoPro = 'Yes';
          Else;
             scNoPro = *blanks;
          Endif;
          //*** End Add ***** 09/18/25 ***********************************

          scCxCdRRN# = dbRrn1;

          rrnCxCd = rrnCxCd + 1;

       //   If rrnCxCd = 1;
       //      scCxCdRwSl = 'A';
       //   Else;
       //      Clear scCxCdRwSl;
       //   Endif;

          If inmode = 'D';
             dspVisb = *Off;
          Else;
             dspVisb = *On;
          Endif;

          Write sflCxCd;
         //*** beg del ***** 03/16/20 **************
         //*** Endif;
         //*** end del ***** 03/16/20 **************
          Read pcr0p;
       Enddo;

       rrnCxCd  = 1;

      /end-free
     p LoadCnlRsnCd    e

       //==================================================================
       // MeetFlt - Determine if the records meet the Filter criteria
       //==================================================================

       //*** beg del ***** 03/16/20 **************
     p*** MeetFlt         b
     d*** MeetFlt         pi
     d*** meetCode        s              1n
      ***/free
      ***
       //////// Check for State
       //////If scFltrSt <> ' ' and scFltrSt <> CRst;
       //////   meetPay = *off;
       //////Endif;
       //////
       //////// Check for status
       //////If meetPay = *on;
       //////   If scFltrSts <> ' ' and scFltrSts <> CRstat;
       //////   meetPay = *off;
       //////   Endif;
       //////Endif;
       //////
      ***/end-free
     p*** MeetFlt         e
       //*** end del ***** 03/16/20 **************

       //===================================================================
       // ProcessAdd - process addition
       //===================================================================
     P ProcessAdd      B
     d ProcessAdd      PI
      /free

       isValid  = *on;

       // processing for Edit mode
       If isAddorEdit = 'C';

        // reason code should be disabled in edit mode
          dsbAdCode = *on;

        // effective date should be disabled in edit mode
          dsbAdEfDt = *on;

        // State should be disabled in edit mode
          dsbAdState = *on;

        // Status should be disabled in edit mode
          dsbAdSts = *on;

          Chain(n) scCxCdRrn# pcr0p;
          MoveDbSc();

       Endif;

       // processing for Add mode
       If isAddorEdit = 'A';

        // reason code should not be disabled in add mode
          dsbAdCode = *off;

        // effective date should not be disabled in add mode
          dsbAdEfDt = *off;

        // State should not be disabled in add mode
          dsbAdState = *off;

        // Status should be disabled in add mode
          dsbAdSts = *on;

        // Status is defaulted
          scAdSts = 'A';

       Endif;

       Dou BtnAddCan = *on or (BtnAddSav = *on and isValid = *on);
          //*** Begin Add *** 07/28/25 *************************************
          is153 = hld153;
          //*** End   Add *** 07/28/25 *************************************
          Exfmt rcdCxCdAdd;
          isValid = *on;
          Select;

          When BtnAddSav = *on;
             Validate();
             If isValid = *on;
                AddDetails();
             Endif;

          When BtnAddCan = *on;
             BtnAddCan = *off;
             Leave;

          Other;
          Endsl;
       Enddo;
       Unlock WTCRP;
      /end-free
     P processAdd      E

       //===================================================================
       // ProcOther - process other options
       //===================================================================

     p ProcOther       b
     d ProcOther       pi
      /free

       // Edit Cancellation Reason Codes

       Readc sflCxCd;

       Dow not %eof;

          Select;

          // processing for Edit link
          When scCxCdEdt = *on;
             scCxCdEdt = *off;
             Clear RcdCxCdAdd;
             //*** Begin Add *** 07/28/25 **********************************
             is153 = hld153;
             //*** End   Add *** 07/28/25 **********************************
             isAddorEdit = 'C';
             BtnAddCan = *off;
             BtnAddSav = *off;
             ProcessAdd();

          // processing for Change Status link
          When scCxCdChgS = *on;
             scCxCdChgS = *off;
             Chain scCxCdRrn# pcr0p;
             If CRstat = 'A';
                CRstat = 'I';
             Else;
                CRstat = 'A';
             Endif;
             Update pcr0p;
             Unlock WTCRP;
          Endsl;

          Readc sflCxCd;
       EndDo;

      /end-free
     p ProcOther       E

       //===================================================================
       // Validate - Validate Add/Edit screen
       //===================================================================
     P Validate        B
     d Validate        PI
     c
      /free

       isValid  = *on;
       erAdState = *off;
       erAdEfdt = *off;
       erAdCode = *off;
       erAdDesc = *off;
       erAdSts = *off;
       erAdNCCI = *off;
       erAdSurPct = *off;
       erAdCxlOvr = *off;
       erAdNotDay = *off;
       erAdAdlDay = *off;
       erAdAutRei = *off;
       erAdAudTrk = *off;
       erAdCerLtr = *off;
       erAdNwCxDy = *off;
       erAdNwAdDy = *off;
       erAdReiFee = *off;
       erAdInNwDy = *off;
       erAdBThrDt = *off;
       erAdPctBal = *off;
       erAdCxCnf = *off;
       erAdCxReDy = *off;
       erAdCnfDay = *off;

       If isAddorEdit = 'A';

          // State is required and should be valid
          If isValid = *on;
             If scAdState = *blanks;
                msAdState = 'This is a required field';
                erAdState = *on;
                isValid = *off;
                return;
             Else;
                kkst = scAdState;
                Setll kkst pst03;
                If not %equal(ststl3);
                   msAdState = 'WCR0513';
                   erAdState = *on;
                   isValid = *off;
                   return;
                Endif;
             Endif;
          Endif;

          // Effective Date is a required entry
          If isValid = *on;
             //*** Beg Chg ***** 09/18/24 ********************************
             //***If %char(scAdEfDt) = *blanks or scAdEfDt = *loval;
             If scAdEfDt = *zeros;
             //*** End Chg ***** 09/18/24 ********************************
                msAdEfDt = 'This is a required field';
                erAdEfDt = *on;
                isValid = *off;
                return;
             Endif;
          Endif;

          // Effective Date must be valid
          // the screen field is a date field that enforces it

          //*** Beg Add ***** 09/18/24 ***********************************
          // Validate Effective date entered
          If scAdEfDt <> 0;
             MsAdEfdt1 = *blanks;
             erAdEfdt1 = *off;
             erAdEfdt1 = ValidDate(scAdEfDt:charMsg);
             If erAdEfdt1 = *on;
                msAdEfdt1 = charMsg;
                isValid = *off;
                return;
             Endif;
          Endif;
          //*** End Add ***** 09/18/24 ***********************************

          // Reason Code is a required entry
          If isValid = *on and scAdCode = *blanks;
             msAdCode = 'This is a required field';
             erAdCode = *on;
             isValid = *off;
             return;
          Endif;

          // The combination entered is unique
          //*** Beg Chg ***** 09/18/24 ***********************************
          //***kkefdt = Dateto7(scAdEfDt);
          kkefdt = dtCYMD(scAdEfDt);
          //*** End Chg ***** 09/18/24 ***********************************
          kkcode = scAdCode;
          kkst = scAdState;
          Setll (kkco#:kkst:kkcode:kkefdt) pcr01;
          If %equal(wtcrl);
             msAdEfDt = 'This combination already exists';
             erAdEfDt = *on;
             isValid = *off;
             Return;
          Endif;

       EndIf;

       // Description is a required entry
       If isValid = *on and scAdDesc = *blanks;
          msAdDesc = 'This is a required field';
          erAdDesc = *on;
          isValid = *off;
          Return;
       Endif;

       // NCCI code is required and should be valid
       If isValid = *on and scAdNCCI = *blanks;
          msAdNCCI = 'This is a required field';
          erAdNCCI = *on;
          isValid = *off;
          Return;
       Else;
          kktbid = 'CXCD';
          kkkey = scAdNCCI;
          Setll (kkco#:kktbid:kkkey) pgt00;
          If not %equal(wtgtl);
             msAdNCCI = 'Invalid value';
             erAdNCCI = *on;
             isValid = *off;
             Return;
          Endif;
       Endif;

       // Validate Cancellation Notice Days
       If isValid = *on and scAdNotDay < *zeros;
          msAdNotDay = 'This cannot be less than zero';
          erAdNotDay = *on;
          isValid = *off;
          Return;
       Endif;

       // Validate Additional Days for Mailing
       If isValid = *on and scAdAdlDay < *zeros;
          msAdAdlDay = 'This cannot be less than zero';
          erAdAdlDay = *on;
          isValid = *off;
          Return;
       Endif;

       // Validate New Policy Cancellation Days
       If isValid = *on and scAdNwCxDy < *zeros;
          msAdNwCxDy = 'This cannot be less than zero';
          erAdNwCxDy = *on;
          isValid = *off;
          Return;
       Endif;

       // Validate No. of days from Inception to consider Policy New
       If isValid = *on and scAdInNwDy < *zeros;
          msAdInNwDy = 'This cannot be less than zero';
          erAdInNwDy = *on;
          isValid = *off;
          Return;
       Endif;

       // Validate New Policy Additional Mailing Days
       If isValid = *on and scAdNwAdDy < *zeros;
          msAdNwAdDy = 'This cannot be less than zero';
          erAdNwAdDy = *on;
          isValid = *off;
          Return;
       Endif;

       // No. of days from Inception to consider Policy New and
       // New Policy Cancellation days are mutually inclusive
       If isValid = *on and
        (
         (scAdNwCxDy <> *zeros and scAdInNwDy = *zeros)
         or
         (scAdNwCxDy = *zeros and scAdInNwDy <> *zeros)
        );
          msAdNwCxDy = 'The indicated information is mutually inclusive';
          erAdNwCxDy = *on;
          msAdInNwDy = 'The indicated information is mutually inclusive';
          erAdInNwDy = *on;
          isValid = *off;
          Return;
       Endif;

       // New Policy Additional Mailing days is dependent on
       // No. of days from Inception to consider Policy New
       If isValid = *on and
        (
         (scAdNwAdDy <> *zeros and scAdInNwDy = *zeros)
        );
          msAdNwAdDy = 'The indicated information is mutually inclusive';
          erAdNwAdDy = *on;
          msAdInNwDy = 'The indicated information is mutually inclusive';
          erAdInNwDy = *on;
          isValid = *off;
          Return;
       Endif;

       // Validate Reinstatement Fee
       If isValid = *on and scAdReiFee < *zeros;
          msAdReiFee = 'This cannot be less than zero';
          erAdReiFee = *on;
          isValid = *off;
          Return;
       Endif;

       //*** Begin Add *** 07/28/25 ****************************************
       //
       // VS Edits for Reinstatement Code and Late Fee/Code
       If is153 = *On;

          // If NCCI code is 05 edit reinstatement and late fee codes
          If scAdNCCI = '05';

             // Reinstatement Billing A/R Code
             If scAdReiFee = 0;
                If scAdReinCd <> ' ';
                   msAdReinCd = 'Reinstatement A/R Code must be blank.';
                   erAdReinCd = *On;
                   isValid    = *Off;
                   Return;
                EndIf;
             Else;
                If scAdReinCd = ' ';
                   msAdReinCd = 'Reinstatement A/R Code is required.';
                   erAdReinCd = *On;
                   isValid    = *Off;
                   Return;
                EndIf;
             EndIf;

             // Late Fee Billing A/R Code
             If scAdLatFee = 0;
                If scAdLFCode <> ' ';
                   msAdLFCode = 'Late Fee A/R Code must be blank.';
                   erAdLFCode = *On;
                   isValid    = *Off;
                   Return;
                EndIf;
             Else;
                If scAdLFCode = ' ';
                   msAdLFCode = 'Late Fee A/R Code is required.';
                   erAdLFCode = *On;
                   isValid    = *Off;
                   Return;
                EndIf;
             EndIf;

          // Only allow Late fees and A/R Codes if NCCI code is 05
          Else;

             // do not allow reinstatement code if not NCCI code 05
             If scAdReinCd <> ' ';
                msAdReinCd = 'Reinstatement A/R Code must be blank.';
                erAdReinCd = *On;
                isValid    = *Off;
                Return;
             EndIf;

             // do not allow late fee if not NCCI code 05
             If scAdLatFee <> 0;
                msAdLatFee = 'Late Fee must be zero.';
                erAdLatFee = *On;
                isValid    = *Off;
                Return;
             EndIf;

             // do not allow late fee billing code if not NCCI code 05
             If scAdLFCode <> ' ';
                msAdLFCode = 'Late Fee A/R Code must be blank.';
                erAdLFCode = *On;
                isValid    = *Off;
                Return;
             EndIf;
          EndIf;
       EndIf;
       //*** End   Add *** 07/28/25 ****************************************

       // If Auto Reinstatement Flag is on do edits
       If isAutoReIns = *on;

          // If Auto Reinstatement Flag is on, percentage should be there
          If isValid = *on and
           scAdAutRei = 'Y' and scAdPctBal <= *zeros;
             msAdPctBal = 'This cannot be less than or equal to zero';
             erAdPctBal = *on;
             isValid = *off;
             Return;
          EndIf;

          // Auto Reinstatement Flag should be on for Percentage of
          // Balance to be entered
          If isValid = *on and
           scAdAutRei <> 'Y' and scAdPctBal <> *zeros;
             msAdPctBal = 'If this is entered, Auto Reinstatement ' +
              'flag must be "Y"';
             erAdPctBal = *on;
             isValid = *off;
             Return;
          EndIf;

          // Auto Reinstatement Flag should be on for No. of days from
          // Cancellation Eff. Date to Auto Reinstate to be entered
          If isValid = *on and
           scAdAutRei <> 'Y' and scAdCxReDy <> *zeros;
             msAdCxReDy = 'If this is entered, Auto Reinstatement ' +
              'flag must be "Y"';
             erAdCxReDy = *on;
             isValid = *off;
             Return;
          EndIf;

          // Billings Through Date to be Used is required if
          // Auto Reinstatement Flag is on
          If isValid = *on and
           scAdAutRei = 'Y' and scAdBThrDt = *blanks;
             msAdBThrDt = 'This is a required field';
             erAdBThrDt = *on;
             isValid = *off;
             Return;
          EndIf;

          // Validate Billings Through Date to be used
          If isValid = *on and scAdAutRei = 'Y' and
           scAdBThrDt <> 'I' and scAdBThrDt <> 'C' and
           scAdBThrDt <> 'E';
             msAdBThrDt = 'Invalid value';
             erAdBThrDt = *on;
             isValid = *off;
             Return;
          EndIf;

          // Auto Reinstatement Flag should be on for Billings
          // Through Date to be Used to be entered
          If isValid = *on and
           scAdAutRei <> 'Y' and scAdBThrDt <> *blanks;
             msAdBThrDt = 'If this is entered, Auto Reinstatement ' +
              'flag must be "Y"';
             erAdBThrDt = *on;
             isValid = *off;
             Return;
          EndIf;

       EndIf;

       // If Auto Reinstatement Flag is on, No. of days from Cancel Eff.
       // Date to Auto Reinstate should be >= Number of days from Cancel
       // Effective Date to Auto Confirm (grace)
       If isValid = *on and
        scAdAutRei = 'Y' and scAdCxCnf = 'Y' and
        scAdCnfDay < scAdCxReDy;
          msAdCnfDay = 'The No. of days to Auto Confirm must be greater or ' +
           'equal to number of days to Auto Reinstate';
          erAdCnfDay = *on;
          isValid = *off;
          Return;
       EndIf;

       // Cancel ID Code Override should be valid
       If isValid = *on and scAdCxlOvr <> *blanks;
          kktbid = 'CXID';
          kkkey = scAdCxlOvr;
          Setll (kkco#:kktbid:kkkey) pgt00;
          If not %equal(wtgtl);
             msAdCxlOvr = 'Invalid value';
             erAdCxlOvr = *on;
             isValid = *off;
             Return;
          Endif;
       Endif;

      /end-free
     P Validate        E

       //===================================================================
       // AddDetails - Add Details
       //===================================================================
     P AddDetails      B
     d AddDetails      PI
     c
      /free

       Chain scCxCdRrn# pcr0p;
       CRdesc = scAdDesc;
       CRNCCI = scAdNCCI;
       CRupsp = scAdSurPct;
       CRupsc = scAdSurCd;
       CRidov = scAdCxlOvr;
       CRnotc = scAdNotDay;
       CRaddm = scAdAdlDay;
       CRarei = scAdAutRei;
       CRatrk = scAdAudTrk;
       CRpchl = scAdCerLtr;
       CRcert = scAdCerNot;
       CRntnp = scAdNwCxDy;
       CRmdnp = scAdNwAdDy;
       CRrfee = scAdReiFee;
       //*** Begin Add *** 07/28/25 ****************************************
       CRrFCd = scAdReinCd;
       CRlFee = scAdLatFee;
       CRlFCd = scAdLFCode;
       //*** End   Add *** 07/28/25 ****************************************
       CRndnw = scAdInNwDy;
       CRice = scAdBThrDt;
       CRpcnt = scAdPctBal;
       CRacnf = scAdCxCnf;
       CRrday = scAdCxReDy;
       CRcday = scAdCnfDay;
       //*** Beg Add *** 09/18/25 ****************************************
       CRnopro = scNoProE;
       //*** End Add *** 09/18/25 ****************************************
       If isAddorEdit = 'A';
          CRst = scAdState;
          //*** Beg Chg ***** 09/18/24 ***********************************
          //***CRefdt = Dateto7(scAdEfDt);
          CRefdt = dtCYMD(scAdEfDt);
          //*** End Chg ***** 09/18/24 ***********************************
          CRkey = scAdCode;
          CRstat = 'A';
          CRuser = q1user;
          CRpgm  = q1pgm;
          CRdt   = Dateto7(SystemDate());
          CRtime = Timeto6(SystemTime());
          Write pcr0p;
       Else;
          CRuser = q1user;
          CRpgm  = q1pgm;
          CRdt   = Dateto7(SystemDate());
          CRtime = Timeto6(SystemTime());
          Update pcr0p;
       Endif;
       Unlock WTCRP;

      /end-free
     P AddDetails      E

       //===================================================================
       // MoveDbSc - Move database record to screen
       //===================================================================
     P MoveDbSc        B
     d MoveDbSc        PI
     c
      /free

       scAdState = CRst;
       //*** Beg Chg ***** 09/18/24 ***********************************
       //***scAdEfDt = CvtToDate(CRefdt);
       scAdEfDt = dtMDYY(CRefdt);
       //*** End Chg ***** 09/18/24 ***********************************
       scAdCode = CRkey;
       scAdDesc = CRdesc;
       scAdSts = CRstat;
       scAdNCCI = CRNCCI;
       scAdSurPct = CRupsp;
       scAdSurCd = CRupsc;
       scAdCxlOvr = CRidov;
       scAdNotDay = CRnotc;
       scAdAdlDay = CRaddm;
       scAdAutRei = CRarei;
       scAdAudTrk = CRatrk;
       scAdCerLtr = CRpchl;
       scAdCerNot = CRcert;
       scAdNwCxDy = CRntnp;
       scAdNwAdDy = CRmdnp;
       scAdReiFee = CRrfee;
       //*** Begin Add *** 07/28/25 ****************************************
       scAdReinCd = CRrFCd;
       scAdLatFee = CRlFee;
       scAdLFCode = CRlFCd;
       //*** End   Add *** 07/28/25 ****************************************
       scAdInNwDy = CRndnw;
       scAdBThrDt = CRice;
       scAdPctBal = CRpcnt;
       scAdCxCnf = CRacnf;
       scAdCxReDy = CRrday;
       scAdCnfDay = CRcday;
       //*** Beg Add *** 09/18/25 ****************************************
       scNoProE   = CRnopro;
       //*** End Add *** 09/18/25 ****************************************

      /end-free
     P MoveDbSc        E
       //===================================================================
