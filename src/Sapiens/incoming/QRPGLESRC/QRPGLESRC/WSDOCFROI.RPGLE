     H Copyright('(c) 2023 Sapiens. All Rights Reserved.')
     H**********************************************************************
     H/TITLE WSDOCFROI - Scriptura FROI Document Setup
     H DFTNAME(WSDOCFROI)
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO) ACTGRP(*caller)
     H/DEFINE   PROFOUNDUI
      *** Beg Add ***** 08/21/23 *************************************
     H*BNDDIR('WTBNDDIR')
     H BNDDIR('STBNDDIR')
      *** End Add ***** 08/21/23 *************************************
     h**********************************************************************
    *===============================================================*
    *  Created    : 07/19/17
    *  Programmer : C:53
    *  Project    : WC2499
    *  Description: This is the new scriptura document setup scren
    *               program for FROI
    *               It is accessed as an additional setup screen
    *               from the Output Control tab of Application
    *               Configuration.
    *
    *  Note:        State defaults are coded both in the RPG here
    *               and in the display file to trigger the form
    *               names to be updated when a state is selected
    *               that has state specific forms (via state drop
    *               down box).
    *
    *===============================================================*
    *  Revisions:                                                   *
      *                                                               *
      * 08/21/23 C:333 Project: WC2876                                *
      *                Output Control Functionality Enhancements      *
      *                                                               *
      * 04/28/22 C:291 Project: WC2670                                *
      *                Add filter option on State Form Setup grid.    *
      *                                                               *
      * 02/17/20 C:318 Support: 86503                                 *
      *                1. Corrected Default FROI form name and also   *
      *                   corrected FROI form name for selected states*
      *                2.Customized the dropdown to show only states  *
      *                  those exist in file WSFFP.                   *
      *                3.Changed Onchange syntax and choices for      *
      *                  state dropdown in WSDOCFROI display file     *
      *                                                               *
    * ------------------------------------------------------------- *
    *  NOTE: Please add comments to the top of Revisions.           *
    * ------------------------------------------------------------- *
    *===============================================================*

     fwsdocfroi cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflState:rrnSt)

     FWMUPL     IF   E           K Disk    Rename(PUP01:PUP00)
     FWSFFL     IF   E           K Disk    Rename(PFF01:PFF00)
     F                                     infds(dbfds)
     FWSFFP     UF A E             Disk    Rename(PFF01:PFF0P)
      //*****Beg Add*****02/17/20********************************
     FSTSTL3    IF   E           K Disk
      //*****End Add*****02/17/20********************************
      *** Beg Add ***** 08/21/23 ***************************************
     fwmfnl     if   e           k disk    rename(pfn01:pfn00)
     f                                     usropn
     f                                     infds(dbfd14)
      *** End Add ***** 08/21/23 ***************************************

     D PSDS          ESDS                  Extname(SZQ1P)

     d dbfds           ds
     d  dbrrn                397    400b 0
     d*** Beg Add *** 08/21/23 ***************************************
     d dbfd14          ds
     d  dbrrn14              397    400b 0
     d*** End Add *** 08/21/23 ***************************************

     D process         s               n   inz(*off)
     D clientID        s              3    inz('000')
     d xxCo#           s              3  0 inz(0)
     d xxStEffDt       s              7  0 inz(0)
      *** Beg Add *** 08/21/23 ***************************************
     d inData          s            500a   inz
     d intCo#          s              3  0
     d inLeft          s              5  0 inz(290)
     d inTop           s              5  0 inz(210)
      *** End Add *** 08/21/23 ***************************************

     d* Proto-types

     d* Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3

     d* Program Start
     d ProgramStart    pr
     D processMe                       n

     d* Main Processing
     d Main            pr

      * Add/Edit/Copy State Setup record
     d SetupWindow     pr
     D inStateMode                    1    const
     D inStateRRN                     9  0 const options(*nopass)
      //*****Beg Add*****02/17/20********************************
     D StateCodes      S             10    Dim(999)
     D indx            S              4  0 inz(0)
      //*****End Add*****02/17/20********************************

     d/copy sprDate
     d/copy sprGenPop
     d/copy sprGenErr
      //*** Beg Chg ***** 08/21/23 ***********************************
     d*/copy wPrUsrPrms
     d/copy sPrModule
     d/copy sPrUsrPrms
      //*** End Chg ***** 08/21/23 ***********************************

     C*
     C*   Key List for the User Parameter File
     C*
     C     keyWMUPL      Klist
     C                   Kfld                    Q1USER
     C*
     C*   Key List for the Notification Master File
     C*
     C     keyWSFFL      Klist
     C                   Kfld                    scStGrp
     C                   Kfld                    scStState
     C                   Kfld                    xxStEffDt
     C*
     c* Entry of Program.
     c*
     C     *ENTRY        PLIST
     C                   PARM                    @@cancel          1            cancel indicator
     c
      /free

       ProgramStart(process);

       If process;
          Main();
       Else;
          PopOneBtn('You must set up a company in your User Parameters.':
           'Forms Setup':'Exit');
          @@cancel = *on;
       Endif;

       *InLr = *On;
      /end-free
      *
       //***************************************************************************//
       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     P ProgramStart    B
     D ProgramStart    PI
     D processMe                       n
      /free

       //*** Beg Add *** 08/21/23 ************************************
       intCo# = GetUserInfo(q1user);
       //*** End Add *** 08/21/23 ************************************
       GetClientID(clientID);

       Chain(n) keyWMUPL PUP00;
       If not %found or UPCO# = *zero;
          processMe = *off;
          Return;
       Endif;

       xxCO# = UPCO#;

       processMe = *on;
       Return;

      /end-free
     P ProgramStart    E

       //***************************************************************************//
       //===================================================================
       // Main - Main processing
       //===================================================================
     P Main            B
     D Main            PI
      /free

       Exsr LoadRecords;

       //*  Process until exit button is pressed.

       DoU btnCancel = *on or btnSave = *on;

          //*  Process screen.

          //*** Beg Add ***** 08/21/23 ******************************
          scOutType = 'First Report of Injury';
          write rcdSetup;
          //*** End Add ***** 08/21/23 ******************************
          Exfmt rcdSetup;

          //Perform actions on screen.
          Select;

          //* Exit the program
          When btnCancel = *on;
             Leave;

          //* Add a new state setup record.
          When btnStAdd = *on;
             btnStAdd = *off;
             Exsr AddStateRcd;
             Exsr LoadRecords;

          Other;

             //*  Check for clicked links on grid.
             Readc sflState;
             If not %EOF();

                //*  Edit the state setup record.
                If stateEdit = *on;
                   stateEdit = *off;
                   Exsr EditStateRcd;
                Endif;

                //*  Delete the state setup record.
                If stateDelt = *on;
                   stateDelt = *off;
                   Exsr DeleteStateRcd;
                Endif;

                //*  Copy the state setup record.
                If stateCopy = *on;
                   stateCopy = *off;
                   Exsr CopyStateRcd;
                Endif;

                Update sflState;

                Exsr LoadRecords;

             Endif;

          Endsl;

       Enddo;


       //***************************************************************************//
       //===================================================================
       // Load State Setup Grid
       //===================================================================
       Begsr LoadRecords;

       // Clear the subfile.
       dspSt = *off;
       dspStCtl = *off;
       clrSt = *on;
       Write rcdSetup;
       clrSt = *off;
       dspSt = *on;
       dspStCtl = *on;
       rrnSt = 0;

       // Load the subfile.

       Setll *loval pFF00;
       Read  pFF00;

       Dow not %eof;

       //*** Beg Add *** 08/21/23 ***********************************
          If FFFnd = 0;
             outStGrp = 'All';
          Else;
             outStGrp = %editc(FFFnd : 'X');
          Endif;
       //*** End Add *** 08/21/23 ***********************************

          outStState = FFSt;
          outStEffDt = dtMDYY(FFEFDT);
          outStFForm = %trim(FFPATH) + %trim(FFDOC);
          outStCForm = %trim(FFPATHC) + %trim(FFDOCC);
          outStRRN  = dbrrn;

       //  outStRForm = %trim(FFREPATH) + %trim(FFREDOC);
       //  outStReClt = %trim(FFREPATHC) + %trim(FFREDOCC);

          rrnSt  =  rrnSt + 1;

          Write sflState;

          Read pFF00;
       Enddo;

       rrnSt = 1;

       Endsr;

       //***************************************************************************//
       //===================================================================
       // Add State Setup Record
       //===================================================================
       Begsr AddStateRcd;

       SetupWindow('A');

       Endsr;

       //***************************************************************************//
       //===================================================================
       // Edit State Setup Record
       //===================================================================
       Begsr EditStateRcd;

       SetupWindow('E':outStRRN);

       Endsr;

       //***************************************************************************//
       //===================================================================
       // Delete State Setup Record
       //===================================================================
       Begsr DeleteStateRcd;

       SetupWindow('D':outStRRN);

       Endsr;

       //***************************************************************************//
       //===================================================================
       // Copy State Setup Record
       //===================================================================
       Begsr CopyStateRcd;

       SetupWindow('C':outStRRN);

       Endsr;

       //***************************************************************************//

      /end-free
     P Main            E

       //===================================================================
       // Add/Edit/Copy State Setup to file.
       //===================================================================
     p SetupWindow     b
     d SetupWindow     pi
     D inStateMode                    1    const
     D inStateRRN                     9  0 const options(*nopass)

     d xxRRN           s              9  0 inz(0)
     d error           s               n   inz(*off)
     d steAddMode      s               n   inz(*off)
     d message         s            100
     d strMsg          s            200

      /free

       If %parms >= 2;
          xxRRN = inStateRRN;
       Endif;

       fndChoices = 'FNCO# = ' + %char(xxCO#);
       steAddMode = *off;
       steEditMde = *off;
       //*** Beg Add ***** 08/21/23 **********************************
       steEditM1 = *off;
       //*** End Add ***** 08/21/23 **********************************
       dspStAdd   = *off;

       If xxRRN <> 0;
          Chain(n) xxRRN pFF0p;
       //*** Beg Chg ***** 08/21/23 **********************************
          //***If inStateMode <> 'C';
          //***   steEditMde = *on;
          //***Endif;
          If inStateMode = 'C';
             steAddMode = *on;
             steEditMde = *on;
             btnName = 'Proceed To Copy';
          Else;
             steEditMde = *on;
             steEditM1 = *on;
             btnName = 'Save';
          Endif;
       //*** End Chg ***** 08/21/23 **********************************
       Else;
          dspStAdd   = *on;
          steAddMode = *on;
       //*** Beg Add ***** 08/21/23 **********************************
          btnName = 'Save';
       //*** End Add ***** 08/21/23 **********************************
       Endif;

       //* Keep bringing up new popup windows in Add mode until cancel is hit.
       Dou steAddMode = *off;

          //* Clear screen fields prior to display if in Add Mode.
       //*** Beg Chg ***** 08/21/23 **********************************
          //***If steAddMode = *on;
          If steAddMode = *on and inStateMode <> 'C';
       //*** End Chg ***** 08/21/23 **********************************
             Clear scStGrp;
             Clear scStState;
             Clear scStEffDt;
      //*****Beg Chg*****02/17/20********************************
      //**** scStCXTem = 'First Report of Injury.scd';
             scStCXTem = 'FROI IA-1.scd';
      //*****End Chg*****02/17/20********************************
      //*****Beg Add*****02/17/20********************************
             scCXForm  = 'FROI';
      //*** Beg Del ***** 08/21/23 ***********************************
             //***ScStDrpdw ='All States';
             //***ScStDrpVal=*blanks;
              //***Indx=1;
              //***Clear StateCodes;
      //*Storing state list in array  and showing customized dropdown
              //***Setll *loval pFF00;
              //***Read  pFF00;
              //***Dow not %eof;
              //***  If %Lookup(Ffst:StateCodes)=0 and Ffst<>' ' ;
              //***    StateCodes(Indx)=Ffst;
              //***    Chain StateCodes(Indx) pST01;
              //***       If  %found(STSTL3) and Ststat<> 'I' and Stnst<>' ' ;
              //***        ScStDrpdw  =%trim(ScStDrpdw)+','+%trim(STkey)+'-'+
              //***                    %trim(STname);
              //***        ScStDrpVal=%trim(ScStDrpVal)+','+%trim(Stkey);
              //***       Endif;
              //***    Indx=Indx+1;
              //***  Endif;
              //***Read pFF00;
              //***EndDo;
      //*****End Add*****02/17/20********************************
      //*** End Del ***** 08/21/23 ***********************************
             scStCXPth = '/Compliance/Base/Claim/';
             Clear scStCxTemC;
             Clear scStCxPthC;
      //*** Beg Add ***** 08/21/23 ***********************************
             stCancLeft = 700;
      //*** End Add ***** 08/21/23 ***********************************
          Else;
             scStGrp = FFFND;
             scStState = FFST;
             scStEffDt = dtMDYY(FFEFDT);
             scStCxTem = FFDOC;
             scStCxPth = FFPATH;
             scStCxTemC = FFDOCC;
             scStCxPthC = FFPATHC;
      //*** Beg Add ***** 08/21/23 ***********************************
             stCancLeft = 830;
      //*** End Add ***** 08/21/23 ***********************************
          Endif;

          Exsr ClearErrors;
          Clear btnStSave;
          Clear btnStAdd;
          Clear btnStCanc;

       //* Load the screen for display and then request confirmation of deletion.
       If inStateMode = 'D';
          Write rcdStWin;
          strMsg = 'Do you want to continue with the deletion?';
          If not PopTwoBtn(strmsg:'Confirm':'Cancel':'Continue');
             Chain xxRRN pFF0p;
             Delete pFF0p;
          Endif;
          Return;
       Endif;

       //* Display popup window for the State Setup record.

          Dou btnStSave = *on or btnStCanc = *on or btnStAdd = *on;

             Dou error = *off or btnStCanc = *on;

                Exfmt rcdStWin;

                Exsr ClearErrors;

       //*** Beg Chg ***** 08/21/23 **********************************
                //***If btnStCanc = *off;
                If btnStCanc = *off and inStateMode <> 'C';
       //*** End Chg ***** 08/21/23 **********************************
                   Exsr SetupCheck;
                Endif;

             Enddo;

          Enddo;

          If btnStSave = *on or btnStAdd = *on;
             //*** Beg Chg ***** 08/21/23 ****************************
             //***Exsr SetupSave;
             If inStateMode <> 'C';
                Exsr SetupSave;
             Else;
                inData = 'Copy FROI Forms';
                xxrrn = 0;
                If outStGrp <> 'All';
                   Open WMFNL;
                   Chain(n) (intCo#:%dec(outStGrp:3:0)) pfn00;
                   If %found;
                      xxrrn = dbrrn14;
                   Endif;
                   Close WMFNL;
                Endif;
                ProceedToCopy(xxrrn:inData:scOutType:scStEffDt:outStRRN:
                              intCo#:outStState:*omit:*omit:*omit:inLeft:inTop);
             Endif;
             //*** End Chg ***** 08/21/23 ****************************
          Endif;

          If btnStCanc = *on or btnStSave = *on;
             steAddMode = *off;
          Endif;

       Enddo;

       //===================================================================
       // Clear the error fields for window.
       //===================================================================
       BegSr ClearErrors;

       error = *off;

       Clear erStGrp;
       Clear erStState;
       Clear erStEffDt;
       Clear erStCXTem;
       Clear erStCXPth;
       Clear erStCXTemC;
       Clear erStCXPthC;

       Clear msStGrp;
       Clear msStState;
       Clear msStEffDt;
       Clear msStCXTem;
       Clear msStCXPth;
       Clear msStCXTemC;

       Endsr;
       //===================================================================

       //===================================================================
       // Edit the state setup fields.
       //===================================================================
       Begsr SetupCheck;

       If inStateMode = 'A' or inStateMode = 'C';
          Exsr StateDefaults;
       Endif;

       // Effective date must be entered and valid.
       erStEffDt = Valid_Date(scStEffDt:msStEffDt);
       If erStEffDt = *on;
          msStEffDt = 'Effective Date must be a valid date.';
          error = *on;
       Endif;

       xxStEffDt = dtCYMD(scStEffDt);

       // Check for state setup record already exists.
       If error = *off and (steAddMode = *on or inStateMode = 'C');

          Chain(n) keyWSFFL pFF00;

          If %found;
             msStGrp = 'A record already exists for this Group/State/Effectiv' +
              'e Date.';
             erStGrp = *on;
             error = *on;
          Endif;

       Endif;

       If error = *on;
          LeaveSR;
       Endif;

       // State FROI template.
       If scStCXTem <> ' ';

          // Validate the FROI template name.
          erStCXTem = Valid_Document(xxCo#:'N':scStCXTem:message);
          If erStCXTem = *on;
             msStCXTem = message;
             error = *on;
          Endif;

          // Validate the FROI template path.
          erStCXPth = Valid_Path(scStCXPth:message);
          If erStCXPth = *on;
             msStCXPth = message;
             error = *on;
          Endif;

       Endif;

       // Client FROI template.
       If scStCXTemC <> ' ';

          // Validate the client FROI template name.
          erStCXTemC = Valid_Document(xxCo#:'N':scStCXTemC:message);
          If erStCXTemC = *on;
             msStCXTemC = message;
             error = *on;
          Endif;

          // Validate the client FROI template path.
          erStCXPthC = Valid_Path(scStCXPthC:message);
          If erStCXPthC = *on;
             msStCXPthC = message;
             error = *on;
          Endif;

       Endif;

       Endsr;
       //===================================================================

       //===================================================================
       // Save the state setup record.
       //===================================================================
       Begsr SetupSave;

       If xxRRN <> 0 and inStateMode <> 'C';
          Chain xxRRN pFF0p;
       Else;
          Clear pFF0p;
          FFFnd  = scStGrp;
          FFSt   = scStState;
          FFEfdt = dtCYMD(scStEffDt);
          FFIPgm = Q1PGM;
          FFIUsr = Q1USER;
          FFITm  = Timeto6(SystemTime());
          FFIDt  = Dateto7(SystemDate());
       Endif;

       FFDOC   = scStCxTem;
       FFPATH  = scStCxPth;
       FFDOCC  = scStCxTemC;
       FFPATHC = scStCxPthC;


       FFPgm  = Q1PGM;
       FFUser = Q1USER;
       FFTime = Timeto6(SystemTime());
       FFDt   = Dateto7(SystemDate());

       If xxRRN <> 0 and inStateMode <> 'C';
          Update pFF0p;
       Else;
          Write pFF0p;
       Endif;

       Endsr;
       //===================================================================
       // Set the default template fields for state.
       //===================================================================
       Begsr StateDefaults;

       Endsr;
       //===================================================================

      /end-free

     p SetupWindow     e
