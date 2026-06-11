     Hcopyright('(c) 2023 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H BNDDIR('WTBNDDIR')
     H dftactgrp(*no) actgrp(*caller)
     H*
     H/TITLE Policy Renewal - Prompt
     H*********************************************************************
   H*  DESCRIPTION:  This program prompts for selection criteria to run *
    H*                the Policy Renewal prompt.                         *
     H*                                                                   *
    H*  CALLED FROM:  StoneRiver New Menu items >> Policy Renewal.       *
    H*    MENU                                                           *
     H*                                                                   *
    H*  CALLS TO:                                                        *
     H*                                                                   *
     H*                                                                   *
    H*  REWRITE:               C:306     PROJECT: WC2436                 *
     H*********************************************************************
     H***********************************************************************
    H* REVISIONS:                                                         *
     H*                                                                    *
     H* 03/30/23 C:344 Support: 108826                                     *
     H*                Change display file record format RcdPolRen so that *
     H*                State Code entries in the select box are ordered/   *
     H*                sorted by state code.                               *
     H*                                                                    *
     H* 10/20/22 C:291 Project: 148WDV                                     *
     H*                Add New Payroll rollover option 'F' (Rollover Type  *
     H*                F will not roll any payrolls) to the Rollover type  *
     H*                dropdown. Recompile only.                           *
     H*                                                                    *
     H* 04/11/22 C:333 Project: SY434                                      *
     H*                - Update Grids for Sorting Filtering Upgrade        *
     H*                                                                    *
     H* 01/25/22 C:136 Support: 98291                                      *
     H*                Add division parm for call to renewal processing    *
     H*                to prevent parm mismatch issue.                     *
     H*                                                                    *
     H* 05/13/19  C:180 Project:150WDY                                     *
     H*                 Add renewal option E.                              *
     H*                                                                    *
     H*                 Show renewing year (allow change).                 *
     H* 12/06/17  C:33  Support: 51426                                     *
     H*                 Show renewing year (allow change).                 *
     H***********************************************************************
    H* INDICATORS:
     H***********************************************************************
     * Files Declaration                                                   *
     fwtpolren  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     sfile(sflAgentRt:rrnAgntRt)
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)

    f* Employer Master File
     fwmeml     if   e           k disk    rename(pem01:pem00)
    f* Employer Detail File
     fwdelp     if   e             disk    rename(pel01:pel0p)
     f***  Begin Add  ***  12/06/17  **************************************
     fwdell     if   e           k disk    rename(pel01:pel00)
     f                                     prefix(e0_)
     f***  End   Add  ***  12/06/17  **************************************
    f* User Parameters File
     fwmupl     if   e           k disk    rename(pup01:pup00)
    f* Work file - deposit recap report in agent order
     fwxrxp     uf a e             disk    rename(prx01:prx0p)
     f
     fwxrxl     if   e           k disk    rename(prx01:prx00)
     f                                     prefix(ag_)
     f                                     infds(dbfds)
     f
    f* Agent Rate Master File
     fwmarl     if   e           k disk    rename(par01:par00)
     f
     f***  Begin Add  ***  05/13/19  **************************************
     fwmfnl     if   e           k disk
     f***  End   Add  ***  05/13/19  **************************************
     * Prototypes

     d/copy sPrDate
     d/copy sPrModule
     d/copy sPrCompany
     d/copy sPrGenPop
     d/copy wPrEmpLr
     d/copy wPrUsrprms
     d/copy wPrgroup
     d/copy wPrPolErr
     d
     d/copy scopypr01d
    d*---------------------------------------------------------------------
    d* Stand Alone Fields - TOP
    d*---------------------------------------------------------------------
     d***  Begin Add  ***  12/06/17  **************************************
     ddecStRptDiv      s                   like(ELdiv)
     d***  End   Add  ***  12/06/17  **************************************
     d strMsg          s            256    inz
     d isExit          s               n   inz(*off)
     d ismenuFlg       s               n
     d isEnter         s               n
     d
     d intdayDiff      s              3  0 inz(0)
     d intRrn          s              9  0 inz(0)
     d intTime         s              6  0 inz
     d
     d inMode          s              1
     d inCall          s              1
     d intCo#          s                   Like(ELco#)
     d intFnd          s                   Like(ELfnd)

     d inAgtrrn        s              9  0 inz(0)
     d
     d inCompName      s             40    inz
     d inco#           s              3    inz
     d inFnd           s              3    inz
     d inFyr           s              3    inz
     d inType          s              1    inz
     d inPct           s              5    inz
     d inOpt           s              1    inz
     d inIdfl          s              1    inz
     d inFtDt          s              8    inz
     d inTtDt          s              8    inz
     d inTime          s              6    inz
     d inOutQ          s             10    inz
     d inRtoQ          s              1    inz
     d inStIn          s             21    inz
     d inStrp          s              1    inz
     d inEmp#          s              9    inz
      *** beg add ***** 01/25/22 ******************
     d inDiv           s              5    inz
      *** end add ***** 01/25/22 ******************

    d*---------------------------------------------------------------------
    d* State codes DataStructure.
    d*---------------------------------------------------------------------
     d dbfds           ds
     d  agtrrn               397    400b 0
     d**
     D                 DS
     D  state1                 1      2
     D  state2                 3      4
     D  state3                 5      6
     D  state4                 7      8
     D  state5                 9     10
     D  state6                11     12
     D  state7                13     14
     D  state8                15     16
     D  state9                17     18
     D  state10               19     20
     D  stateSts              21     21
     d  dsStates               1     21
     d
     *---------------------------------------------------------------------
     * Internal subprocedure Prototypes.
     *---------------------------------------------------------------------

     * Main Processing
     d Main            pr

     * Program Start
     d ProgramStart    pr

     * SetCriteria
     d SetCriteria     pr

     * Clear the subfiles
     d ClrSubFiles     pr

     * Loading Rate code defination Grid.
     d LoadRtCdGrd     pr

     * Loading Rate code as a Dropdown choices.
     d LoadRtCdDrp     pr

     * Process Add rate code.
     d PrcAddRate      pr
     d inAgtRrn                       9  0
     d inMode                         1    const

     * Screen validation procedure.
     d ValidateScr     pr              n

     * Validate Old rate and new rate code.
     d ValidateRate    pr              n
     d inMode                         1    const

     * Rollover - Submitter.
     d W7810C          pr                   extpgm('W7810C')
     d inco#                          3
     d inFnd                          3
     d inFyr                          3
     d inType                         1
     d inPct                          5
     d inOpt                          1
     d inIdfl                         1
     d inFtDt                         8
     d inTtDt                         8
     d inTime                         6
     d inOutQ                        10
     d inRtoQ                         1
     d inStIn                        21
     d inStrp                         1
     d inEmp#                         9
     d inDiv                          5
     ***===================================================================
     ** Recieving entry parameters.
     ***===================================================================
     c     *entry        plist
     c                   parm                    @@Prds           40
     c                   parm                    @@Call            1
     c                   parm                    @@Rrn             9
     c
      /Free

      // Start Of Program
       ProgramStart();

      // Making all the selection criterias.
       SetCriteria();

      // Main Procesing
       Main();

       *InLr = *on;

      /End-Free

      //===================================================================
      // Main - Main logic is here.
      //===================================================================
     p Main            b
      /Free

       Dow btnCancel = *off and isExit = *off;

         // Executing the screen record format.
          Exsr ExfmtScreens;

          Select;

         // Submit button
          When btnSubmit = *on;
             btnSubmit = *off;
             If ValidateScr();
                ExSr SubmitProcess;
                isExit = *on;
             Endif;

         // Add button for Rate code description.
          When btnAddAgRt = *on;
             btnAddAgRt = *off;
             inAgtrrn = 0;
             PrcAddRate(inAgtrrn:'A');
            // Clearing the Subfile before loading.
             ClrSubfiles();
            // Loading the Rate Code Grid.
             LoadRtCdGrd();


         // Refresh button
          When btnRefresh = *On;
             btnRefresh = *Off;
            // Clearing the record format
             Clear RcdPolRen;
            // Intailize all that indicator stuff.
             ProgramStart();
            // Making all the selection criterias.
             SetCriteria();

          Other;
            // Agent Rate Description subfile change record logic
             Dou %Eof;
               Readc sflAgentRt;
               If %eof();
                  Leave;
               Endif;

              // Agent Rate Descrption Edit hyperlink
               If lnkAgntEdt = *on;
                  lnkAgntEdt = *off;

                  inAgtrrn = scAgntRrn;
                  PrcAddRate(inAgtrrn:'E');
                 // Clearing the Subfile before loading.
                  ClrSubfiles();
                 // Loading the Rate Code Grid.
                  LoadRtCdGrd();

                  Leave;
               EndIf;

              // Agent Rate Descrption Delete hyperlink
               If lnkAgntDel = *on;
                  lnkAgntDel = *off;

                  inAgtrrn = scAgntRrn;
                  PrcAddRate(inAgtrrn:'D');
                 // Clearing the Subfile before loading.
                  ClrSubfiles();
                 // Loading the Rate Code Grid.
                  LoadRtCdGrd();

                  Leave;
               EndIf;

             EndDo;

          EndSl;

       EndDo;

      // Executing all the screen Record Format.
       BegSr ExfmtScreens;

        tabHeight  = 770;              // Tab Height adjustment
        aPnlHeight = tabHeight + 60;  // Action panel Height adjustment

        If q1prms >= 3;
           visPolDate = *off;    // Hide the Policy Date range.
           Exfmt RcdPolRen;      // Only popup window.
        Else;
           visPolDate = *on;     // Display the Policy Date range

           Write RcdComPnl;
           Write RcdTabPolR;
           Exfmt RcdPolRen;
           Read RcdComPnl;
           Read RcdTabPolR;
        EndIf;

       EndSr;

      // Submitter job subroutine.
       BegSr SubmitProcess;
        inco#  =  %editc(intCo#:'X');
        inFnd  =  %editc(scFnd:'X');
       //***  Begin Chg  ***  12/06/17  **********************************
       // inFyr  =  '000';
        inFyr  =  %editc(scFyr:'X');
       //***  End   Chg  ***  12/06/17  **********************************
        inType =  scType;
        inPct  =  %editc(scPct:'X');
        inOpt  =  *blanks;
        inIdfl =  scIdFl;
        inFtDt =  %editc(scFtDt:'X');
        inTtDt =  %editc(scTtDt:'X');

        inRtoQ =  scRtoQ;
 
        inStIn =  dsStates;

        inStrp =  scStrp;
        inEmp# =  %editc(scEmp#:'X');

        W7810C(inco#:inFnd:inFyr:inType:inPct:inOpt:inIdfl:
         inFtDt:inTtDt:inTime:inOutq:inRtoQ:inStIn:inStrp:inEmp#:inDiv);

       EndSr;

      /End-Free
     p Main            e

      //===================================================================
      // ProgramStart - Initial Program
      //===================================================================
     p ProgramStart    b
     d ProgramStart    pi
      /Free

      // Intialize the Screen indicators and fields.
       btnCancel = *off;
       scRtoQ    = 'N';
       scStrp    = 'N';

      // Load Action panel with the Company Name.
       clear scCompName;
       intCo# = userCo#(q1user);
       scCompName = GetCompNam(intCo#);


       If q1prms >= 3;
          isPopup    = *on;         // Open popup as window.
          ismenuFlg  = *off;        // Is the call from menu option or Policy toolbar.
          intRrn     = %int(@@Rrn);
       Else;
          isPopup   = *off;         // Open screen as Tab not as a pop up window.
          ismenuFlg = *on;
       Endif;

      // If screen is called from policy toolbar, then pre-populate the
      // Employer number and Fund field.
       If ismenuFlg = *on;          // Menu Call.
        //***  Begin Add  ***  12/06/17  **********************************
          scCo#  = UserCo#(q1user);
          scFyr  = *zeros;
        //***  End   Add  ***  12/06/17  **********************************
          scFnd  = userGrp(q1user);
          isGrpDisb = *off;         // Employer field enabled
          isEmp#Visb = *off;        // Donot display the employer field.
          isStateVis = *on;         // Display the all State code related fields.

         // Screen fields and other element adjustments
          scRenewAdj = 250;         // Left adjustment (Renew to quote field).
          scGrdLblAd = 395;         // Grid label adjustment
          scGridAdj  = 325;         // Grid adjustment
          scAddBtnAd = 450;         // Grid ADD button adjustment.
          scBtnTop   = 490;         // Button positioning.
          scPnlHght  = 570;         // screen panel height adjustment

       Else;                        // Policy toolbar Call. as a popup screen.

          Chain(n) intRrn pel0p;
          scEmp# = ELemp#;
          scFnd  = ELfnd;
        //***  Begin Add  ***  12/06/17  **********************************
          decStRptDiv = GetStRptDv(ELco#:ELfnd:ELemp#);
          Setgt (ELco#:ELfnd:ELemp#:decStRptDiv) wdell;
          Readpe (ELco#:ELfnd:ELemp#:decStRptDiv) wdell;
          scFyr  = e0_ELfyr;
          scCo#  = ELco#;
          isYearChg = *on;
        //isYearChg = *off;
        //***  End   Add  ***  12/06/17  **********************************

          isGrpDisb  = *on;         // Employer field Disabled
          isEmp#Visb = *on;         // Display the Employer field.
          isStateVis = *off;        // Donot display all State code related fields.


         // Screen fields and other element adjustements
          scRenewAdj = 195;         // Left adjustment (Renew to quote field).
          scEmp#Adj  = 20;          // Left adjustment (Employer field).
          scGrdLblAd = 235;         // Grid label adjustment
          scGridAdj  = 165;         // Grid adjustment
          scAddBtnAd = 290;         // Grid ADD button adjustment.
          scBtnTop   = 320;         // Button positioning.
          scPnlHght  = 385;         // screen panel height adjustment

       EndIf;


      // Read the user parameter file to retrive the output queue.
       Chain q1User pup00;
       If %found(wmupl);
          inOutq = UPoutQ;
       EndIf;

      // Get the current time.
       inTime =  %char(%Time():*ISO0);
       intTime = %int(inTime);


      // Clearing the Subfile before loading.
       ClrSubfiles();
      // Loading the Rate Code Grid.
       LoadRtCdGrd();

      /End-Free
     p ProgramStart    e

      //===================================================================
      // SetCriteria
      //===================================================================
     p SetCriteria     b
     d SetCriteria     pi

      /Free

       GetGrpAuthSql(q1User:grpSelect:grpFrom:grpWhere:'A');

      /End-Free

     p SetCriteria     e


      //===================================================================
      // Validate Procedure. All the screen field validation
      //===================================================================
     p ValidateScr     b
     d ValidateScr     pi              n
     d**
     disError          s               n   inz(*off)

      /Free
       isError    = *off;

       strmsg     = *blanks;
       msFnd      = *blanks;
       erFnd      = *off;
       msFtDt     = *blanks;
       erFtDt     = *off;
       msTtDt     = *blanks;
       erTtDt     = *off;
       msIdFl     = *blanks;
       erIdFl     = *off;
       msType     = *blanks;
       erType     = *off;
       msstate    = *blanks;
       erstate    = *off;
       mssts      = *blanks;
       ersts      = *off;

     // Group cannnot be blanks.
       If scFnd = *zeros;
          msFnd = 'WCR0503';
          erFnd = *on;
          isError = *On;
       EndIf;


      // Date range Validations
       If visPolDate = *on and ismenuFlg = *On;

         // From date validation.
          If scftdt = 0;
             msftdt = 'WCR0500';
             erftdt = *on;
             isError = *On;
          Else;
             erftdt = ValidDate(scftdt:strMsg);
             If erftdt = *on;
                msftdt = 'WCR0500';
                isError = *On;
             EndIf;
          EndIf;

         // To date validation.
          If scttdt = 0;
             msttdt = 'WCR0500';
             erttdt = *on;
             isError = *On;
          Else;
             erftdt = ValidDate(scttdt:strMsg);
             If erttdt = *on;
                msttdt = 'WCR0500';
                isError = *On;
             EndIf;
          EndIf;

         // From and To date should be ascending order.
          If cvtToDate(scttdt) < cvtToDate(scftdt) and erttdt = *off
             and erftdt = *off;
             msttdt = 'WCR0529';
             erttdt = *on;
             msftdt = 'WCR0529';
             erftdt = *on;
             isError = *On;
          EndIf;

       EndIf;

      // Payroll % and Increase/Decrease both are related.
       If scPct <> 0 and scIdFl = *blanks;
          msIdFl = 'WCR0864';
          erIdFl = *on;
          isError = *On;
       EndIf;

      // Rollover Type must be 'A', 'B', 'C' or 'D'.
       If scType = *blanks;
          msType = 'WCR0503';
          erType = *on;
          isError = *On;
       EndIf;

      //*** Begin Add *** 05/13/19 **************************************************
      // Rollover Type E only allowed for PEOs
       If scType = 'E';
          chain (scCo#:scFnd) wmfnl;
          If not %found(wmfnl) or FNpeof = *blanks;
             msType = 'WC31538';
             erType = *On;
             isError = *On;
          Endif;
       Endif;
      //*** End   Add *** 05/13/19 **************************************************

      // At least one state must be entered if "I" or "E" has been selected.
       If (scSts = 'I' or scSts = 'E') and scstate = ' ';
          msState = 'WCR3178';
          erState = *on;
          isError = *On;
       EndIf;

      // Selection must be "I" or "E" if a state has been selected.
       If scSts = ' ' and scstate <> ' ';
          msSts = 'WCR3177';
          erSts = *on;
          isError = *On;
       EndIf;

      // Maximum 10 State codes can be selected
       If %len(%trim(scState)) > 30  and erState = *off;
             msState = 'WCR4824';
             erState = *On;
             isError = *On;
       EndIf;

      //Extract State code in a DS.
       Dow %scan(',':%trimR(scState)) > *zeros;
          scState   = %Replace ('':scState:%scan(',':%trimR(scState)):1);
       Enddo;
       dsStates = scState;
       statests = scSts;

      // Policy date range difference is greater than 31 days .
      // Display the warning popup window.
       If ismenuFlg = *on and scTtdt <> 0 and scFtdt <> 0 ;

          Monitor;
             intdayDiff = %Diff(%date(scTtdt:*USA):%date(scFtdt:*USA):*DAYS);
          On-error *all;
             intdayDiff = 0;
          EndMon;

          If intdayDiff > 31;

             strmsg = 'Warning:  The Policy expiration date range is greater' +
              ' than 31 days, press ENTER to continue.';

             isEnter = PopTwoBtn(strmsg:'Warning Message':'Enter':'Exit');

             If isEnter = *off;
                isError = *On;
             EndIf;

          EndIf;  // 31 days Check.
       EndIf;  // Ismenuflg check.

       If isError = *On;
          Return *off;
       Else;
          Return *on;
       EndIf;

      /End-Free
     p ValidateScr     e

      //===================================================================
      // ClrSubFiles - Clears SubFile
      //===================================================================
     p ClrSubFiles     b
     d ClrSubFiles     pi
      /Free
       clrAgntSfl = *on;
       dspAgent   = *off;
       Write RcdPolRen;
       clrAgntSfl = *off;
       dspAgent   = *on;
       rrnAgntRt  = 0;
      /End-Free
     p ClrSubFiles     e

     //===================================================================
     // Rate Code Description Addition/Edit/Delete Procdure.
     //===================================================================
     p PrcAddRate      b
     d PrcAddRate      pi
     d  inAgtRrn                      9  0
     d  inMode                        1    const
     d*
     d isEnd           s               n   inz(*off)
     d intpos          s              3  0
      /Free

      // Intialize the screen format/fields.
       Clear RcdAgntRt;
       btnAgntCan = *off;
       isOldRtDsb = *off;
       isNewRtDsb = *off;
       isVisSvNxt = *off;


      // loading the rate code dropdown choices.
       LoadRtCdDrp();

      // Handling Visiblity indicators.
       Select;
       When inMode ='A';      // In case ADD mode.
          isVisSvNxt = *On;   //Save/Add Next Button visiblity.
          scAgtBtnDs = 'Save';
          scAgtHdg   = 'Add Agent Rate Description';
          scCanBtAdj = 675;   // Cancel button Adjustment.

       When inmode = 'E';     // In case EDIT mode.
          ExSr MoveDbSc;
          isOldRtDsb = *on;
          scAgtBtnDs = 'Save';
          scAgtHdg   = 'Edit Agent Rate Description';
          scCanBtAdj = 785;   // Cancel button Adjustment.

       When inmode = 'D';     // In case DELETE mode.
          ExSr MoveDbSc;
          isOldRtDsb = *on;
          isNewRtDsb = *on;
          scAgtBtnDs = 'Delete';
          scAgtHdg   = 'Delete Agent Rate Description';
          scCanBtAdj = 785;   // Cancel button Adjustment.
       EndSl;

      // Looping for the popup window untill cancel button and exit is Off.
       Dow btnAgntCan = *off and isEnd = *off;

          Exfmt RcdAgntRt;
          Exsr ProcessRate;

       EndDo;

       BegSr ProcessRate;
         Select;
        // ADD Mode. (Save button)
         When btnAgntSav = *On and inmode = 'A';
            btnAgntSav = *off;
            If ValidateRate('A');
               ExSr MoveScDb;
               write prx0p;
               isEnd = *on;
            EndIf;


        // ADD Mode. (Save Next Button)
         When btnAgSvNxt = *On and inmode = 'A';
            btnAgSvNxt = *On;
            If ValidateRate('A');
               ExSr MoveScDb;
               write prx0p;
            EndIf;
            Clear scOldR;
            Clear scNewR;

        // EDIT Mode.
         When btnAgntSav = *On and inmode = 'E';
            btnAgntSav = *off;
            If ValidateRate('E');
               ExSr MoveScDb;
               Update prx0p;
               isEnd = *on;
            EndIf;

        // Delete Mode.
         When btnAgntSav = *On and inmode = 'D';
            btnAgntSav = *off;
            Delete prx0p;
            isEnd = *on;

         Other;
         EndSl;

       EndSr;

       BegSr MoveScDb;
          RXtime = intTime;

          clear intpos;
          intpos     = %scan('-':scOldR);
          RXoldR     = %int(%subst(scOldR:1:intpos-1));
          RXoldREfDt = %int(%subst(scOldR:intpos+1:7));

          clear intpos;
          intpos     = %scan('-':scNewR);
          RXnewR     = %int(%subst(scNewR:1:intpos-1));
          RXNewREfDt = %int(%subst(scNewR:intpos+1:7));

       EndSr;

       BegSr MoveDbSc;
         chain inAgtRrn prx0p;

         scOldR = %char(RXoldR) + '-' + %char(RXOldREfDt);
         scNewR = %char(RXnewR) + '-' + %char(RXNewREfDt);

       EndSr;


      /End-Free
     p PrcAddRate      e

     //===================================================================
     // Validate the Rate Code Description.
     //===================================================================
     p ValidateRate    b
     d ValidateRate    pi              n
     d  inMode                        1    const
     d**
     d intpos          s              3  0 inz
     d intOldR         s              3  0 inz
     d intNewR         s              3  0 inz
     d isError         s               n   inz(*off)

      /Free
       isError = *Off;
       msNewR = *blanks;
       erNewR = *Off;
       msOldR = *blanks;
       erOldR = *Off;

       If scOldR = *blanks;
          msOldR = 'WCR0503';
          erOldR = *on;
          isError = *On;
       EndIf;

       If scNewR = *blanks;
          msNewR = 'WCR0503';
          erNewR = *on;
          isError = *On;
       EndIf;

       If scOldR <> *blanks and inmode = 'A';
          clear intpos;
          intpos = %scan('-':scOldR);
          intoldR = %int(%subst(scOldR:1:intpos-1));

          Chain (intTime:intoldR) prx00;
          If %found(wxrxl);
             msOldR = 'WC30005';
             erOldR = *on;
             isError = *On;
          EndIf;
       EndIf;


       If scNewR <> *Blanks and scOldR <> *Blanks;
          clear intpos;
          intpos = %scan('-':scOldR);
          intOldR = %int(%subst(scOldR:1:intpos-1));

          clear intpos;
          intpos = %scan('-':scNewR);
          intNewR = %int(%subst(scNewR:1:intpos-1));

          If intoldR = intnewR;
             msOldR = 'WC30021';
             erOldR = *on;
             isError = *On;
          EndIf;
       EndIf;

       If isError = *On;
          Return *off;
       Else;
          Return *on;
       EndIf;

      /End-Free
     p ValidateRate    e

     //===================================================================
     // Load Rate Code Grid procedure.
     //===================================================================
     p LoadRtCdGrd     b
      /Free
       setll (intTime:*loval) prx00;
       reade (intTime) prx00;
       Dow not %eof(wxrxl);

          scOldRate = ag_RXoldR;
          scNewRate = ag_RXnewR;

         // Retrieving the Old Rate code plan Type.
          chain (intCo#:scFnd:ag_RXoldR:ag_RXoldRefDt) par00;
          If %found(wmarl);
             Select;
             When ARplan = 'A';
                scOldRPlTy = 'Account Current';
             When ARplan = 'W';
                scOldRPlTy = 'Direct Written';
             Other;
                scOldRPlTy = 'Direct Cash';
             Endsl;
             scOldNwRt  = ARnwRt;
             scOldRnRt  = ARrnRt;

          else;
             scOldRPlTy = *blanks;
             scOldNwRt  = *zeros;
             scOldRnRt  = *zeros;
          EndIf;

         // Retrieving the New Rate code Plan type.
          chain (intCo#:scFnd:ag_RXNewR:ag_RXnewRefDt) par00;
          If %found(wmarl);
             Select;
             When ARplan = 'A';
                scNewRPlTy = 'Account Current';
             When ARplan = 'W';
                scNewRPlTy = 'Direct Written';
             Other;
                scNewRPlTy = 'Direct Cash';
             Endsl;
             scNewNwRt  = ARnwRt;
             scNewRnRt  = ARrnRt;
          else;
             scNewRPlTy = *blanks;
             scNewNwRt  = *zeros;
             scNewRnRt  = *zeros;
          EndIf;

          scAgntRrn = agtRrn;

          rrnAgntRt = rrnAgntRt + 1;

          clear scAgtRowSl;
          If rrnAgntRt = 1;
             scAgtRowSl = 'A';
          EndIf;

          write sflAgentRt;

          reade (intTime) prx00;
       EndDo;
      /End-Free
     p LoadRtCdGrd     e

     //===================================================================
     // Loading Rate Codes in the dropdown as a choices.
     //===================================================================
     p LoadRtCdDrp     b
     d strPlanType     s             30    inz(' ')
      /Free

       clear scOldRtSel;
       clear scOldRtVal;
       clear scNewRtSel;
       clear scNewRtVal;

       setll (intCo#:scFnd:*loval:*loval) par00;
       reade (intCo#:scFnd) par00;
       Dow not %eof(wmarl);
          If ARstat = 'A';  // Only Active records.


            // Making the OLD RATE dropdwon choices and value.
             clear strPlanType;
             Select;
             When ARplan = 'A';
                strPlanType = 'Account Current';
             When ARplan = 'W';
                strPlanType = 'Direct Written';
             Other;
                strPlanType = 'Direct Cash';
             Endsl;

             If scOldRtSel = *blanks;

                scOldRtSel = %char(ARrt#) + ' - ' +
                %trim(strPlanType) + ' - ' + %char(cvtToDate(dtMDYY(ARefDt)))
                + ' ' + 'New Rate : ' + %char(ARnwRt) + '% ' + 'Renew Rate : '
                + %char(ARrnRt) + '%';


                scOldRtVal = %char(ARrt#) + '-' +%char(ARefDt);

             Else;
                scOldRtSel =  %trim(scOldRtSel) + ',' + %char(ARrt#) + ' - ' +
                %trim(strPlanType) + ' - ' + %char(cvtToDate(dtMDYY(ARefDt)))
                + ' ' + 'New Rate : ' + %char(ARnwRt) + '% ' + 'Renew Rate : '
                + %char(ARrnRt) + '%' ;

                scOldRtVal = %trim(scOldRtVal) + ','  + %char(ARrt#) +
                 '-' +%char(ARefDt) ;

             Endif;

            // Making the NEW RATE dropdwon choices and value.
             clear strPlanType;
             Select;
             When ARplan = 'A';
                strPlanType = 'Account Current';
             When ARplan = 'W';
                strPlanType = 'Direct Written';
             Other;
                strPlanType = 'Direct Cash';
             Endsl;

             If scNewRtSel = *blanks;

                scNewRtSel = %char(ARrt#) + ' - ' +
                %trim(strPlanType) + ' - ' + %char(cvtToDate(dtMDYY(ARefDt)))
                + ' ' + 'New Rate : ' + %char(ARnwRt) + '% ' + 'Renew Rate : '
                + %char(ARrnRt) + '%';

                scNewRtVal = %char(ARrt#) + '-' + %char(ARefDt);

             Else;
                scNewRtSel =  %trim(scNewRtSel) + ',' + %char(ARrt#) + ' - ' +
                %trim(strPlanType) + ' - ' + %char(cvtToDate(dtMDYY(ARefDt)))
                + ' ' + 'New Rate : ' + %char(ARnwRt) + '% ' + 'Renew Rate : '
                + %char(ARrnRt) + '%';

                scNewRtVal = %trim(scNewRtVal) + ',' + %char(ARrt#) +
                 '-' +%char(ARefDt);

             Endif;

          EndIf;

          reade (intCo#:scFnd) par00;
       EndDo;
      /End-Free
     p LoadRtCdDrp     e
