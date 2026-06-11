     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H BNDDIR('WTBNDDIR')
     H dftactgrp(*no) actgrp(*CALLER)
     H*
     H/TITLE WTBILLPLN  - Profound Billing Plan Maintenance.
     H*********************************************************************
    H*  DESCRIPTION:  This program prompts for Billing Plan maintenance  *
    H*                                                                   *
    H*  CALLED FROM:                                                     *
    H*    MENU     : StoneRiver New menu items >> Billing Plan Maintena- *
    H*               ance                                                *
    H*  CALLS TO:                                                        *
    H*                                                                   *
    H*                                                                   *
    H*  REWRITE:               C:306     PROJECT: WC2474/102WHD          *
     H*********************************************************************
    H* REVISIONS:                                                        *
     H*                                                                   *
     H* 09/17/21 c:79 Project CN024                                       *
     H*               Add Connect column to grid                          *
     H*                                                                   *
     H* 11/11/17 c:62 Project 148WAN                                      *
     H*               Add payment report check box                        *
     H*                                                                   *
     H*********************************************************************
    H* INDICATORS:
    H* 1.) IsError :- Any Error condition meets, this is setting to *On  *
    H* 2.) IsmeetBill:This flag is *on when record satisfied the filter  *
    H*                criteria.                                          *
     H*********************************************************************
     * Files Declaration                                                 *
     fwtbillpln cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     sfile(sflBillPln:RrnBillpln)
     f
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)
    f*** Billing Schedule Code Table File
     fwtbsp     uf a e             disk    rename(pbs01:pbs0p)
     f
    f*** Billing Schedule Code Table File
     fwdella    if   e           k disk    usropn
     f
    f*** Logical over all table files
     fwtabl     if   e           k disk    include(pbs01)
     f                                     prefix(bs_)
     f                                     infds(dbfds1)
     f
    f*** System & Application Control File Logical
     fssylf     if   e           k disk
     f                                     include(swc01)
     f
     ***********************************************************
     * Copybook having Prototypes.
     ***********************************************************
     d
     d/copy sCopyPr01d
     d/copy sPrCompany
     d/copy sPrDate
     d/copy sPrString
     d/copy wPrEmplr
     d/copy wPrUsrprms
     d*** Beg Add *** 09/17/21**************************************************
     d/copy sPrModule
     d*** End Add *** 09/17/21**************************************************
     d

     *---------------------------------------------------------------------
     **** Prototype Declarations *******************************
     *---------------------------------------------------------------------
     * Clear the subfile
     d ClrSubFile      pr
     d
     * Execute Screen record Formats.
     d Exfmtscr        pr
     d
     * Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d ##CltId                        3
     d
      * Check if BSCD is assigned to policy
     d IfAssigned      pr              n
     d
     * Loading the action panel.
     d LoadActPnl      pr
     d
    d* Billing Plan Grid Loading procedure.
     d LoadBillPln     pr
     d
    d* Clear Bill Plan Filter procedure.
     d LoadClrFlt      pr
     d
     * Main Processing
     d Main            pr
     d
    d* Meet Billing Plan Filter procedure.
     d MeetFlt         pr              n
     d
    d* Process Billing Plan ADD/EDIT/Change Status procedure.
     d PrcBillPlan     pr
     d inrrn                          9  0
     d inMode                         1    const
     d
    d* Billing Plan Filter procedure.
     d ProcBillFlt     pr
     d
    d* Validate Billing Plan screen information
     d ValidBillPln    pr              n
     d inMode                         1    const
     d inBillType                     1
     d
     * Execute Override command.
     d QCmdExc         pr                  ExtPgm('QCMDEXC')
     d  command                            const like(strCommand)
     d  length                       15  5 const
    d*---------------------------------------------------------------------
    d* Stand Alone Fields - TOP
    d*---------------------------------------------------------------------
     d intCo#          s              3  0 inz(0)
     d
     d inRrn           s              9  0
     d inMode          s              1
     d inBillType      s              1
     d ##CltId         s              3
     d strCommand      s            255
     d svRrnBillPln    s              9  0
     d*** Beg Add *** 09/17/21 *************************************************
     d strPortalHead   s             25
     d*** End Add *** 09/17/21 *************************************************
     d
     d dbfds1          ds
     d  bsrrn                397    400b 0
     d
     d dsPFilter       ds                  likerec(RcdBFilter)
     d
     * Entry parameter list.

     **************************************************************************
     *                       P R O G R A M   M A I N L I N E
     **************************************************************************
      /Free

      // Get the Client Id for further processing on basis of Client number.
       GetClientID(##CltId);
       //*** Beg Add *** 09/17/21 **********************************************
       // Determine if Portal is installed
       isPortal = isModEnabled('Portal Module');
       If isPortal = *on;
          scIsPortal = 'Y';
       Else;
          scIsPortal = 'N';
       Endif;
       //*** End Add *** 09/17/21 **********************************************

      // Main Procesing
       Main();

       *Inlr = *on;

      /End-Free

      //===================================================================
      // ClrSubFile - Clearing the Billing plan grid.
      //===================================================================
     p ClrSubFile      b
     d ClrSubFile      pi
      /Free

        clrBillPln = *on;
        dspBillPln = *off;

        Write RcdBilPlan;

        clrBillPln = *off;
        dspBillPln = *on;
        RrnBillPln = *zeros;

      /End-Free
     p ClrSubfile      e

      //===================================================================
      // Execute Screen Record Formats.
      //===================================================================
     p Exfmtscr        b
      /Free
        //*** Beg Add *** 09/17/21 ********************************************
        //If isPortal = *on;                // Tab Width adjustment
           tabWidth   = 1040;
        //Else;
        //   tabWidth   = 970;
        //Endif;
        //*** End Add *** 09/17/21 ********************************************
        tabHeight  = 770;              // Tab Height adjustment
        aPnlHeight = tabHeight + 60;  // Action panel Height adjustment

        Write RcdComPnl;
        Write RcdTabBill;
        Exfmt RcdBilPlan;
        Read RcdTabBill;
        Read RcdComPnl;
      /End-Free
     p Exfmtscr        e
     //===================================================================
     // IfAssigned - Returns *On if BSCD is assigned to policy
     //===================================================================
     p IfAssigned      b
     d IfAssigned      pi              n

      /Free
       Open wdella;
       Chain (intCo#:scBillCode) pel01;
       If %found(wdella);
          Close wdella;
          Return *On;
       EndIf;
       Close wdella;
       strCommand = 'OVRDBF FILE(wdella) TOFILE(' +
        %trim(RtvQuoteLib('*QTE')) + '/wdella)';
       Callp QCMDEXC(strCommand:%len(strCommand));
       Open wdella;
       Chain (intCo#:scBillCode) pel01;
       Close wdella;
       strCommand = 'DLTOVR FILE(wdella)';
       Callp QCMDEXC(strCommand:%len(strCommand));
       Return %Found(wdella);
      /End-Free

     p IfAssigned      e
     //===================================================================
     // Load Action panel Procedure.
     //===================================================================
     p LoadActPnl      b
      /Free
        TabHeight = 1800;
        aPnlHeight = TabHeight + 60;

       // Load Action panel with the Company Name.
        Clear scCompName;
        intCo# = userCo#(q1user);
        scCompName = GetCompNam(intCo#);

      /End-Free
     p LoadActPnl      e

      //===================================================================
      // Billing Plan Grid loading
      //===================================================================
     p LoadBillPln     b
      /Free

        //*** Beg Add *** 09/17/21 *********************************************
        // Load the grid headings based on whethor Portal is installed.  Load a
        // blank heading if portal not install and client is not on 6.0.0 or higher
        // and therefore does not have the hideColumn function.
        strPortalHead = GetModuleProperty('Portal Module':'Name');
        scHdBplan = 'Code,Description,Est/Self-Rpt,Plan Type,EFT Only, +
         S/R Online,' + %Trim(strPortalHead) + ',Frequency,Status,';
        scPortHd2     = 'Include in ' + %Trim(strPortalHead);
        //*** End Add *** 09/17/21 *********************************************

        Setll (intCo#:*loval) pbs01;
        Reade intCo# pbs01;

        Dow not %eof(wtabl);

          // Validate for filter
           If MeetFlt();

              scBillCd = %trim(bs_BSkey);  // Billing plan code.
              scBillDs = %trim(bs_BSdesc); // Billing plan description.

             // Self-Rept/ Estimate Flag
              Select;
               When bs_BSseFl = 'E';
                  scSelfEsFl = 'Estimating';
               When bs_BSseFl = 'S';
                  scSelfEsFl = 'Self-Reporting';
               Other;
                  scSelfEsFl = *blanks;
              Endsl;

             // Plan Type.
              Select;
               When bs_BSlPgm = 'I';
                  scBillPnTy = 'Incurred Loss Retro';
               When bs_BSlPgm = 'P';
                  scBillPnTy = 'Paid Loss Retro';
               When bs_BSlPgm = 'L';
                  scBillPnTy = 'Large Deductible';
               When bs_BSlPgm = 'R';
                  scBillPnTy = 'Retro Paid';
               Other;
                  scBillPnTy = *blanks;
              Endsl;

             // EFT Flag.
              Select;
               When bs_BSeftF = 'Y';
                  scEftFlag = 'Yes';
               When bs_BSeftF = 'N';
                  scEftFlag = 'No';
               Other;
                  scEftFlag = 'No';
              Endsl;

             // S/R Online.
              Select;
               When bs_BSonli = 'Y';
                  scSROnFlag = 'Yes';
               When bs_BSonli = 'N';
                  scSROnFlag = 'No';
               Other;
                  scSROnFlag = 'No';
              Endsl;

              // Frequency
              Select;
              When bs_BSfreq = 'M';
                 scBFreq = 'Monthly';
              When bs_BSfreq = 'W';
                 scBFreq = 'Weekly';
              When bs_BSfreq = 'T';
                 scBFreq = 'Twice Monthly';
              Other;
                 scBFreq = *blanks;
              Endsl;

             // Status.
              Select;
               When bs_BSstat = 'A';
                  scBillStat = 'Active';
               When bs_BSstat = 'I';
                  scBillStat = 'Inactive';
              Endsl;
              //*** Beg Add *** 09/17/21 ***************************************
              // Portal
              Select;
               When bs_BSportal = 'Y';
                  scPortFlag = 'Yes';
               When bs_BSportal = 'N' or bs_BSportal = ' ';
                  scPortFlag = 'No';
              Endsl;
              //*** End Add *** 09/17/21 ***************************************

              scBillRrn = bsrrn;
              rrnBillpln = rrnBillpln + 1;

              If rrnBillpln = 1;
                 scBilRowSl = 'A';
              Else;
                 Clear scBilRowSl;
              EndIf;

              Write sflBillPln;

          EndIf;  // MeetFlt()

          Reade intCo# pbs01;

         EndDo;

       If svRrnBillPln = 0;
          rrnBillpln = 1;
       Else;
          rrnBillpln = svRrnBillPln;
       Endif;

      /End-Free
     p LoadBillPln     e

     ***----------------------------------------------------***
     * Procedure for Clearing Filter                        ***
     ***----------------------------------------------------***
     p LoadClrFlt      b
     d LoadClrFlt      pi

      /free

      // display clear filter button
       If dspFilter.scFlStat  = *blanks and
          dspFilter.scFlPlTyp = *blanks and
          dspFilter.scFlBType = *blanks and
          dspFilter.scFlDesc  = *blanks;
          dspClrFltr = *off;
       Else;
          dspClrFltr = *on;
       Endif;

      /end-free
     p LoadClrFlt      e

      //===================================================================
      // Main - Main processing
      //===================================================================
     p Main            b
      /free

      // Clearing the filter datastructure.
       Clear dspFilter;

      //*  Process until cancel/exit button is pressed.
       Dou ExitNow = *On;

         // Load Filter Clear Button.
          LoadClrFlt();

         // Clearing the Billing Plan subfile.
          ClrSubFile();

         // Load action panel values.
          LoadActPnl();

         // Load the Billing Plan Grid information.
          LoadBillPln();

         // Execute all the record formats.
          Exfmtscr();

          Select;

            // Add Button click
             When BtnBlPlAdd = *on;
                BtnBlPlAdd = *off;
                inrrn = 0;
                PrcBillPlan(inrrn :'A');

            // Billing Plan Filter.
             When BtnBFilter = *on;
                ProcBillFlt();

            // Filter clear processing
             When BtnClear = *on;
                Clear dspFilter;
                Clear rcdBFilter;
                dspClrFltr = *off;


            // Other Processing.
             Other;

                svRRnBillPln = 0;

              // Billing plan Grid subfile change record logic
                Dou %Eof;
                  Readc sflBillPln;
                  If %eof();
                     Leave;
                  Endif;

                  svRrnBillPln = rrnBillpln;

              // Billing Plan Edit hyperlink
                  If lnkBillEdt = *on;
                     lnkBillEdt = *off;

                     inrrn = scBillRrn;
                     PrcBillPlan(inrrn :'C');
                     Leave;
                  EndIf;

              // Change Status hyperlink
                  If LnkBillSts = *on;
                     LnkBillSts = *off;

                     inrrn = scBillRrn;
                     PrcBillPlan(inrrn :'S');
                     Leave;
                  EndIf;

               EndDo;    // Billing plan grid ReadC

          EndSl;

       Enddo;    // ExitNow = *On;

      /End-Free
     p Main            e

     ***----------------------------------------------------***
     * Procedure for Meet Filter                            ***
     ***----------------------------------------------------***
     p MeetFlt         b
     d MeetFlt         pi              n
     d
     d isMeetBill      s               n   inz(*off)

      /free
       isMeetBill = *On;

       If scFlBType <> ' ' and scFlBType <> bs_BSsefl;
          isMeetBill = *off;
       EndIf;

       If scFlStat <> ' ' and scFlStat <> bs_BSstat;
          isMeetBill = *off;
       EndIf;

       If scFlPlTyp <> ' ' and scFlPlTyp <> bs_BSlPgm;
          isMeetBill = *off;
       EndIf;

       If scFlDesc  <> ' ' and Not(StringFound(bs_BSdesc:scFlDesc:*On));
          isMeetBill = *off;
       EndIf;

       Return isMeetBill;
      /end-free

     p MeetFlt         e
      //===================================================================
      // ADD/EDIT/CHANGE-STATUS Process.
      //===================================================================
     p PrcBillPlan     b
     d PrcBillPlan     pi
     d  inRrn                         9  0
     d  inMode                        1    const
      // Local variables.
     d  isexit         s               n   inz(*off)
      /Free
       Clear RcdBillAdd;
       BtnCancel  = *off;

       isAmWins = *Off;
       If ##CltId = '148';
          isAmWins = *On;
       EndIf;

       If ##CltId = '129';
          isBillMnty = *On;  // Bill monthly installments on the 15th Cbx visibility
       else;
          isBillMnty = *Off;
       EndIf;

       Exsr ModuleChk;

      //**********************************
       Select;
      // ADD Mode
       When inmode = 'A';
          isBillCdDs = *off;
          isDisable  = *on;
          isEstDisb  = *on;
      // EDIT Mode.
       When inmode = 'C';
          ExSr MoveDbSc;
          ExSr CrtBillFreqCh;   // Creating Bill Plan Frequecy.
          isBillCdDs = IfAssigned();
          isDisable  = isBillCdDs;
          isEstDisb  = isBillCdDs;
      // Change Status
       When inmode = 'S';
          ExSr ChgStatus;
       EndSl;

      // Looping for the popup window untill cancel button and exit is Off.
       Dow BtnCancel = *off And isExit = *off;
          Exfmt RcdBillAdd;     // Displaying the screen.
          Select;
            // Estimating Radio button Click event.
             When BtnEstimat = *on;
                BtnEstimat = *off;
                Clear scBillFreq;  // Clearing Billing Frequency
                Clear scDayOfWk;   // Clearing Day of week.
                Clear scDepPerd1;  // Clearing Deposit % 1
                Clear scDepPerd2;  // Clearing Reporting Deposit % 2
                Exsr CrtBillFreqCh;
            // Self-Reporting Radio button Click event.
             When BtnSelfRep = *on;
                BtnSelfRep = *off;
                Clear scBillFreq;  // Clearing Billing Frequency
                Clear scDayOfWk;   // Clearing Day of week.
                Clear scDepPerd1;  // Clearing Deposit % 1
                Clear scDepPerd2;  // Clearing Deposit % 2
                Clear scEftStart;  // Clearing EFT Start Period
                Clear scEstBlPd1;  // Clearing Estimating Billing % per Period 1
                Clear scEstBlPd2;  // Clearing Estimating Billing % per Period 2
                Clear scEstBlPd3;  // Clearing Estimating Billing % per Period 3
                Clear scEstBlPd4;  // Clearing Estimating Billing % per Period 4
                Clear scEstBlPd5;  // Clearing Estimating Billing % per Period 5
                Clear scEstBlPd6;  // Clearing Estimating Billing % per Period 6
                Clear scEstBlPd7;  // Clearing Estimating Billing % per Period 7
                Clear scEstBlPd8;  // Clearing Estimating Billing % per Period 8
                Clear scEstBlPd9;  // Clearing Estimating Billing % per Period 9
                Clear scEstBlP10;  // Clearing Estimating Billing % per Period 10
                Clear scEstBlP11;  // Clearing Estimating Billing % per Period 11
                Clear scEstBlP12;  // Clearing Estimating Billing % per Period 12
               // Building the Frequency and Day of week Choices.
                Exsr CrtBillFreqCh;
            // Save Button Click Event. (ADD Mode)
             When BtnSave = *On And inmode = 'A' ;
                BtnSave = *off;
                If ValidBillPln('A':inBillType);
                   Exsr WriteBillPln;
                   isexit = *on;
                EndIf;
            // Save Button Click Event. (EDIT Mode)
             When BtnSave = *On And inmode = 'C' ;
                BtnSave = *off;
                If ValidBillPln('C':inBillType);
                   Exsr UpdBillPlan;
                   isexit = *on;
                EndIf;
           EndSl;

       EndDo;

      //**************************************************//
      // Subroutine to create the Frequency and Day of week
      // choices
      //**************************************************//
       BegSr CrtBillFreqCh;
        Clear scFreqChc;
        Clear scFreqChVl;
        Clear scDayWkCh;
        Clear scDyWkCVa;

        If scBType = 'E';
           inBillType = 'E';
           isDisable = isBillCdDs;
           isEstDisb = isBillCdDs;
           isEstHid  = *off;

           scFreqChc  = 'Monthly,Twice a Month,Weekly';
           scFreqChVl = 'M,T,W';
           scDayWkCh  = 'Monday,Tuesday,Wednesday,Thursday,Friday';
           scDyWkCVa  = 'MO,TU,WE,TH,FR,';

        EndIf;

        If scBType = 'S';
           isDisable = isBillCdDs;
           isEstHid  = *on;
           inBillType = 'S';

          // Vendor Processing module flag.
           If WCfl23 = 'Y';
              scFreqChc  = 'Monthly,Weekly';
              scFreqChVl = 'M,W';
           Else;
              scFreqChc  = 'Monthly';
              scFreqChVl = 'M';
           EndIf;

           scDayWkCh  = 'Monday,Tuesday,Wednesday,Thursday,Friday,+
             Saturday,Sunday';
           scDyWkCVa  = 'MO,TU,WE,TH,FR,SA,SU';
        EndIf;

       EndSr;

      //*********************************************************//
      // Subroutine to check whether Modules are installed or not.
      //*********************************************************//

       BegSr ModuleChk;
        isEftVisb = *off;
        isSRVisb  = *off;

        Chain 'WC' swc01;
        If %found(ssylf);

          // EFT Module flag.
           If WCfl24 = 'Y';
              isEftVisb = *on;
           EndIf;

          // S/R Online Module flag.
           If WCfl38 = 'Y';
              isSRVisb = *on;
           EndIf;

          // Paid/Incurred Loss Retro Module
           If WCfl11 = 'Y';
              If ##CltId = '114';
                 scPlTyChc  = ',Incurred Loss Retro,Paid Loss Retro,Retro Plan';
                 scPlTyChVl =  ',I,P,R';
              Else;
                 scPlTyChc  = ',Incurred Loss Retro,Paid Loss Retro';
                 scPlTyChVl =  ',I,P';
              EndIf;
           EndIf;

          // Large Deductible Module
           If WCfl43 = 'Y';
              scPlTyChc  = %trim(scPlTyChc) + ',Large Deductible' ;
              scPlTyChVl = %trim(scPlTyChVl) + ',L';
           EndIf;

        EndIf;   //ssylf
       //*** Beg Add *** 09/17/21 **********************************************
       // Determine if Portal is installed
       isPortal = isModEnabled('Portal Module');
       If isPortal = *on;
          scPortHd2 = 'Include in ' + %Trim(strPortalHead);
       Endif;
       //*** End Add *** 09/17/21 **********************************************
       EndSr;

      //******************************//
      // Writing a new Billing plan.
      //*****************************//
       BegSr WriteBillPln;

        ExSr MoveScDb;
        BSiUsr  = q1user;                // Intial user
        BSiPgm  = q1pgm;                 // Intial pgm
        BSiTm   = Timeto6(SystemTime()); // Intial time
        BSiDt   = Dateto7(SystemDate()); // Intial date

        Write pbs0p;
       EndSr;

      //*****************************************//
      // Updating/Editing Billing Plan subroutine.
      //*****************************************//
       BegSr UpdBillPlan;
          Chain inrrn pbs0p;  // to get exclusive record lock.
          ExSr MoveScDb;
          Update pbs0p;
          Unlock wtbsp;
       EndSr;

      //****************************************************//
      // Subrotine to move Screen fields into database fields.
      //****************************************************//
       BegSr MoveScDb;

        BSco#  = intCo#;                  // COMPANY NUMBER
        BSkey  = %editc(scBillCode:'X');  // TABLE FILE KEY
        BSdesc = scBillDesc;              // DESCRIPTION
        BSseFl = scBType;                 // SELF-REPT/ ESTIMATE FLAG
        BSrlCd = %editc(scBillCdRl:'X');  // ROLLOVER CODE

        BSab01 = *zeros;
        BSab02 = *zeros;
        BSab03 = *zeros;             // Est Deposit % per Period 3
        BSab04 = *zeros;             // Est Deposit % per Period 4
        BSab05 = *zeros;             // Est Deposit % per Period 5
        BSab06 = *zeros;             // Est Deposit % per Period 6
        BSab07 = *zeros;             // Est Deposit % per Period 7
        BSab08 = *zeros;             // Est Deposit % per Period 8
        BSab09 = *zeros;             // Est Deposit % per Period 9
        BSab10 = *zeros;             // Est Deposit % per Period 10
        BSab11 = *zeros;             // Est Deposit % per Period 11
        BSab12 = *zeros;             // Est Deposit % per Period 12
        BSesDp = *zeros;            // Est Deposit % Period 1
        BSesD2 = *zeros;            // Est Deposit % Period 2

        If scBType = 'S';  //S/R reporting.
           If scDepPerd1 > *zeros;
              BSab01  = scDepPerd1/100;    // SR deposit % period 1
           EndIf;

           If scDepPerd2 > *zeros;
              BSab02  = scDepPerd2/100;    // SR deposit % period 2
           EndIf;
        EndIf;     // scBType EndIf;


        If scBType = 'E';  //Estimating

           If scEstBlPd1 > *zeros;
              BSab01 = scEstBlPd1/100;     // Est Deposit % per Period 1
           EndIf;

           If scEstBlPd2 > *zeros;
              BSab02 = scEstBlPd2/100;     // Est Deposit % per Period 2
           EndIf;

           If scEstBlPd3 > *zeros;
              BSab03 = scEstBlPd3/100;     // Est Deposit % per Period 3
           EndIf;

           If scEstBlPd4 > *zeros;
              BSab04 = scEstBlPd4/100;     // Est Deposit % per Period 4
           EndIf;

           If scEstBlPd5 > *zeros;
              BSab05 = scEstBlPd5/100;     // Est Deposit % per Period 5
           EndIf;

           If scEstBlPd6 > *zeros;
              BSab06 = scEstBlPd6/100;     // Est Deposit % per Period 6
           EndIf;

           If scEstBlPd7 > *zeros;
              BSab07 = scEstBlPd7/100;     // Est Deposit % per Period 7
           EndIf;

           If scEstBlPd8 > *zeros;
              BSab08 = scEstBlPd8/100;     // Est Deposit % per Period 8
           EndIf;

           If scEstBlPd9 > *zeros;
              BSab09 = scEstBlPd9/100;     // Est Deposit % per Period 9
           EndIf;

           If scEstBlP10 > *zeros;
              BSab10 = scEstBlP10/100;     // Est Deposit % per Period 10
           EndIf;

           If scEstBlP11 > *zeros;
              BSab11 = scEstBlP11/100;     // Est Deposit % per Period 11
           EndIf;

           If scEstBlP12 > *zeros;
              BSab12 = scEstBlP12/100;         // Est Deposit % per Period 12
           EndIf;

           BSesDp  = scDepPerd1;               // Est Deposit % Period 1
           BSesD2  = scDepPerd2;               // Est Deposit % Period 2

        EndIf;  // estimating flag.


        If inMode = 'A';
           BSstat  = 'A';                  // STATUS
        Endif;
        BSuser  = q1user;                  // USER
        BSpgm   = q1pgm;                   // PROGRAM
        BStime  = Timeto6(SystemTime());   // TIME
        BSdt    = Dateto7(SystemDate());   // DATE

        BSlpgm  = scPlanType;              // PLAN TYPE (I/P/ )
        BSdbsd  = scEftStart;              // DIRECT DEBIT TRANSFER START

        BSfreq  = scBillFreq;           // BILLING FREQUENCY

        BSdoWk  = scDayOfWk;               // DAY OF WEEK

        BSon15  = scBillMt15;              // BILLING ON 15 DAY FLAG
        BSpayR  = scPayRpt;                // Payment Report
        BSonli  = scOnlineSR;              // ONLINE S/R PROCESSING
        BSeftF  = scEFTOnly;               // EFT only flag
        BSmaxDep = scMxDpAmt;              // Max Deposit Amt

        BSMSFL  = ' ';                     // SPECIAL S/R FLAG (Obselete now)
        BS#MTH  = *zeros;                  // # OF MONTHS REPORTED (Obselete now)
        BStMth  = *zeros;                  // MNTH DEPOSIT TRANSFERRED (Obselete now)
        BSbDay  = ' ';                     // INSTALLMENT BILLING DAY(Obselete now)
        //*** Beg Add *** 09/17/21 *********************************************
        BSportal = scPort;
        //*** End Add *** 09/17/21 *********************************************

       EndSr;

      //****************************************************//
      // Subroutine to move database fields into Screen fields.
      //****************************************************//
       BegSr MoveDbSc;
        Chain(n) inrrn pbs0p; // Reading the bill plan record.

        scBillCode = %int(BSkey);          // TABLE FILE KEY
        scBillDesc = BSdesc;               // DESCRIPTION
        scBType    = BSseFl;               // SELF-REPT/ ESTIMATE FLAG
        scBillCdRl = %int(BSrlCd);         // ROLLOVER CODE

        If BSseFl = 'S';
           scDepPerd1 = BSab01*100;         // SR deposit % period 1
           scDepPerd2 = BSab02*100;         // SR deposit % period 2
        EndIf;

        If BSseFl = 'E';
           scEstBlPd1 = BSab01*100;        // Est Deposit % per Period 1
           scEstBlPd2 = BSab02*100;        // Est Deposit % per Period 2
           scEstBlPd3 = BSab03*100;        // Est Deposit % per Period 3
           scEstBlPd4 = BSab04*100;        // Est Deposit % per Period 4
           scEstBlPd5 = BSab05*100;        // Est Deposit % per Period 5
           scEstBlPd6 = BSab06*100;        // Est Deposit % per Period 6
           scEstBlPd7 = BSab07*100;        // Est Deposit % per Period 7
           scEstBlPd8 = BSab08*100;        // Est Deposit % per Period 8
           scEstBlPd9 = BSab09*100;        // Est Deposit % per Period 9
           scEstBlP10 = BSab10*100;        // Est Deposit % per Period 10
           scEstBlP11 = BSab11*100;        // Est Deposit % per Period 11
           scEstBlP12 = BSab12*100;        // Est Deposit % per Period 12

           scDepPerd1 = BSesDp;            // Est Deposit % Period 1
           scDepPerd2 = BSesD2;            // Est Deposit % Period 2
        EndIf;

        scPlanType = BSlpgm;               // PLAN TYPE (I/P/ )
        scEftStart = BSdbsd;               // DIRECT DEBIT TRANSFER START

        scBillFreq = BSfreq;               // BILLING FREQUENCY
        scDayOfWk  = BSdoWk;               // DAY OF WEEK

        scBillMt15 = BSon15;               // BILLING ON 15 DAY FLAG
        scPayRpt   = BSpayR;               // Payment Report
        scOnlineSR = BSonli;               // ONLINE S/R PROCESSING
        scEFTOnly  = BSeftF;               // EFT only flag
        scMxDpAmt  = BSmaxDep;             // Max Deposit Amt
        //*** Beg Add *** 09/17/21 *********************************************
        scPort = BSportal;
        //*** End Add *** 09/17/21 *********************************************

       EndSr;

      //****************************************************//
      // Subroutine to Change the status of the Billing plan.
      //****************************************************//
       BegSr ChgStatus;
        Chain inrrn pbs0p; // Reading the bill plan record.

        If BSstat = 'A';
           BSstat  = 'I';                  // STATUS
        Else;
           BSstat  = 'A';                  // STATUS
        EndIf;

        Update pbs0p %fields(BsStat);
        isExit = *on;
       EndSr;

      /End-Free
     p PrcBillPlan     e

     ***----------------------------------------------------***
     * Procedure for Billing Plan filtering                 ***
     ***----------------------------------------------------***
     p ProcBillFlt     b
      /Free
       Dou BtnFilCan  = *on or BtnApply = *on;

          ExFmt RcdBFilter;
          If BtnApply = *on;
             dspFilter.scFlStat  = scFlStat;
             dspFilter.scFlPlTyp = scFlPlTyp;
             dspFilter.scFlBType = scFlBtype;
             dspFilter.scFlDesc  = scFlDesc;
          Endif;

          If BtnFilCan  = *on;
             scFlStat   = dspFilter.scFlStat;
             scFlPlTyp  = dspFilter.scFlPlTyp;
             scFlBtype  = dspFilter.scFlBType;
             scFlDesc   = dspFilter.scFlDesc;
          Endif;
       Enddo;

      /End-Free
     p ProcBillFlt     e

     ***----------------------------------------------------***
     * Procedure to validate the Billing Plan screen values.***
     ***----------------------------------------------------***
     p ValidBillPln    b
     d ValidBillPln    pi              n
     d inMode                         1    const
     d inBillType                     1
     d
     d isError         s               n   Inz(*off)
     d inttotal        s             10  6 Inz(*zeros)
     d strBillcode     s             10    Inz(' ')
      /Free

       isError    = *off;
       msBillDesc = *blanks;
       erBillDesc = *off;
       msBillCode = *blanks;
       erBillCode = *off;
       msBType    = *blanks;
       erBType    = *off;
       msBillCdRl = *blanks;
       erBillCdRl = *off;
       msPlanType = *blanks;
       erPlanType = *off;
       mgPayRpt   = *blanks;
       erPayRpt   = *off;

       msEstBlPd1 = *blanks;
       erEstBlPd1 = *off;

       msDepPerd1  = *blanks;    // Deposit% Period 1
       erDepPerd1  = *off;
       msDepPerd2  = *blanks;    // Deposit% Period 2
       erDepPerd2  = *off;

       msOnlineSR = *blanks;
       erOnlineSR = *off;
       msBillFreq = *blanks;
       erBillFreq = *off;
       msBillMt15 = *blanks;
       erBillMt15 = *off;
       msDayOfWk  = *blanks;
       erDayOfWk  = *off;
       msEftStart = *blanks;
       erEftStart = *off;
       msMxDpAmt  = *blanks;
       erMxDpAmt  = *off;

       Chain 'WC' swc01;  // Reading the system control table.

      // Billing Plan Code
       If scBillCode <= *zeros;
          msBillCode = 'WCR0502';
          erBillCode = *On;
          isError    = *On;
       EndIf;

      // Billing Plan Code already exists. Only in ADD mode.
       If inMode = 'A' and scBillCode > *Zeros and isError = *off;
          strBillcode = %editc(scBillCode:'X');
          Chain (intCo#:strBillcode) pbs01;
          If %found(wtabl);
             msBillCode = 'WC30005';
             erBillCode = *On;
             isError    = *On;
          EndIf;
       EndIf;

      // Description;
       If scBillDesc = *blanks;
          msBillDesc = 'WCR0503';
          erBillDesc = *On;
          isError    = *on;
       EndIf;

      // Billing Type.
       If scBType <> 'S' and scBType <> 'E';
          msBType = 'WCR0516';
          erBType = *On;
          isError = *On;
       EndIf;

      //*** If estimating and not EFT, then Total percentage is 100/
       If scBType = 'E' and WCfl24 <> 'Y';
          inttotal = (scEstBlPd1 + scEstBlPd2 + scEstBlPd3 + scEstBlPd4
           + scEstBlPd5 + scEstBlPd6 + scEstBlPd7 + scEstBlPd8 + scEstBlPd9
           + scEstBlP10 + scEstBlP11 + scEstBlP12);
          If inttotal <> 100;
             msEstBlPd1 = 'WCR0570';
             erEstBlPd1 = *On;
             isError = *On;
          EndIf;
       EndIf;

      //*** If Self reporting/estimating flag not 'E' then both estimating deposit
      //*** fields should be zero.

      //*** If Estimating  Deposit fields should not be less than 0.
       If scBType = 'E' and (scDepPerd1 < 0 or scDepPerd2 < 0);
          msDepPerd1 = 'WCR0547';
          erDepPerd1 = *on;
          msDepPerd2 = 'WCR0547';
          erDepPerd2 = *on;
          isError   = *On;
       Endif;

      //** If Estimating, Deposit periods are mutually exclusive.
       If scBType = 'E' and scDepPerd1 <> 0 and scDepPerd2 <> 0;
          msDepPerd1 = 'WCR0918';
          erDepPerd1 = *On;
          msDepPerd2 = 'WCR0918';
          erDepPerd2 = *On;
          isError   = *On;
       Endif;

      // Value must be entered in Period1 or 2 to allow a max.
      // deposit amount for Estimating Billing type.
       If (scBType = 'E' and scMxDpAmt <> *Zeros and scDepPerd1 = *zeros
        and scDepPerd2 = *Zeros);
            msDepPerd1 = 'WC40164';
            erDepPerd1 = *on;
            isError = *On;
       EndIf;

      //** If Estimating, S/R online cannot be checked.
       If scBType = 'E' and scOnlineSR <> *blanks;
          msOnlineSR = 'WCR0450';
          erOnlineSR = *On;
          isError   = *On;
       EndIf;

       // Error check2
      //** IF Self Reporting, Frequency must be Monthly Or Weekly.
       If scBType = 'S' and scBillFreq <> 'M' and scBillFreq <> 'W';
          msBillFreq = 'WC30249';
          erBillFreq = *On;
          isError   = *On;
       EndIf;

      //** IF EFT Enabled And Estimating, Freq Must Be M,T, OR W..
       If WCfl24 = 'Y' and scBType = 'E';
          If scBillFreq <> 'M' and scBillFreq <> 'W' and
           scBillFreq <> 'T';
             msBillFreq = 'WC30271';
             erBillFreq = *On;
             isError   = *On;
          EndIf;
       EndIf;

       If isBillMnty = *on;
          If scBillMt15 <> 'Y' and scBillMt15 <> ' ';
             msBillMt15 = 'WC31401';
             erBillMt15 = *On;
             isError    = *On;
          EndIf;

         // If billing on the 15th, then Frequency MUST BE = (M)ontly
          If scBillMt15 = 'Y' and scBillFreq <> 'M';
             msBillFreq = 'WC40041';
             erBillFreq = *On;
             isError    = *On;
          EndIf;

       EndIf;
       //
       // Paymet Report check box can only be checked if estimated bill type
       If scBType <> 'E' And scPayRpt = 'Y';
          mgPayRpt = 'Pay report cannot be checked if self-reporting type.';
          erPayRpt = *On;
          isError  = *On;
       EndIf;

      // If Self Reporting, Validate day of week.
       If scBType = 'S' and scBillFreq = 'W' and
        scDayOfWk <> 'MO' and scDayOfWk <> 'TU' and
        scDayOfWk <> 'WE' and scDayOfWk <> 'TH' and
        scDayOfWk <> 'FR' and scDayOfWk <> 'SA' and
        scDayOfWk <> 'SU';
          msDayOfWk = 'WC30250';
          erDayOfWk = *on;
          isError  = *On;
       EndIf;

      // If Self Reporting, day of week must be blank if freq is Monthly.
       If scBType = 'S' and scBillFreq = 'M' and scDayOfWk <> *blanks;
          msDayOfWk = 'WC30251';
          erDayOfWk = *on;
          isError  = *On;
       EndIf;

      // Self reporting % are mutually exclusive.
       If scBType = 'S';
          If scDepPerd1 <> *zeros and scDepPerd2 <> *zeros;
             msDepPerd1 = 'WCR0590';
             erDepPerd1 = *On;
             msDepPerd2 = 'WCR0590';
             erDepPerd2 = *On;
             isError = *On;
          EndIf;
       EndIf;

      // Value must be entered in Period1 or 2 to allow a max.
      // deposit amount for Self Reporting type.
       If (scBType = 'S' and scMxDpAmt <> *Zeros and scDepPerd1 = *zeros
        and scDepPerd2 = *Zeros);
            msDepPerd1 = 'WC40164';
            erDepPerd1 = *on;
            isError = *On;
       EndIf;

      // If Estimating, Validate day of week.
       If scBType = 'E' and scBillFreq = 'W' and
        scDayOfWk <> 'MO' and scDayOfWk <> 'TU' and
        scDayOfWk <> 'WE' and scDayOfWk <> 'TH' and
        scDayOfWk <> 'FR';
          msDayOfWk = 'WC30253';
          erDayOfWk = *on;
          isError  = *On;
       EndIf;

      // Estimate Billing % cannot be greater than 100 %.
       If scBType = 'E';
          inttotal = (scEstBlPd1 + scEstBlPd2 + scEstBlPd3 + scEstBlPd4
           + scEstBlPd5 + scEstBlPd6 + scEstBlPd7 + scEstBlPd8 + scEstBlPd9
           + scEstBlP10 + scEstBlP11 + scEstBlP12);

          If (scBillFreq = 'M' and WCfl24 = 'Y' and inttotal <> 100) Or
             (scDayOfWk <> *blanks and WCfl24 <> 'Y' and inttotal <> 100);
             msEstBlPd1 = 'WCR0570';
             erEstBlPd1 = *On;
             isError = *On;
           EndIf;

       EndIf;

      //* If Estimating, Day Of Week and Period Buckets are mutually Exclusive
       If scBType = 'E' and scDayOfWk <> *blanks and inttotal <> *zeros;
          msDayOfWk  = 'WC30252';
          erDayOfWk  = *On;
          msEstBlPd1 = 'WC30252';
          erEstBlPd1 = *on;
          isError = *On;
       EndIf;

      //** If EFT Enabled and Estimating, Freq and Buckets are mutually inclusive
      //** If Freq = M, And Mutually Exclusive.
       If scBType = 'E' and WCfl24 = 'Y' and isError = *off;

          If scBillFreq = 'M' and inttotal = *zeros;
             msBillFreq = 'WCR1083';
             erBillFreq = *On;
             msEstBlPd1 = 'WCR1083';
             erEstBlPd1 = *on;
             isError = *On;
          EndIf;

          If (scBillFreq = 'T' and inttotal <> *zeros) Or
             (scBillFreq = 'W' and inttotal <> *zeros);
             msBillFreq = 'WCR0590';
             erBillFreq = *On;
             msEstBlPd1 = 'WCR0590';
             erEstBlPd1 = *On;
             isError = *On;
          EndIf;

          If (scBillFreq = 'M' and scDayOfWk <> *blanks) Or
             (scBillFreq = 'T' and scDayOfWk <> *blanks);
             msBillFreq = 'WCR0590';
             erBillFreq = *On;
             msDayOfWk  = 'WCR0590';
             erDayOfWk  = *On;
             isError = *On;
          EndIf;

         // EFT starting period.
          If scEftStart <= *zeros and isEftVisb = *On;
             msEftStart = 'WCR0502';
             erEftStart = *On;
             isError = *On;
          EndIf;

       EndIf; // Estimating and EFT module.

       If isError = *off;
          return *on;
       Else;
          return *off;
       EndIf;

      /End-Free
     p ValidBillPln    e
