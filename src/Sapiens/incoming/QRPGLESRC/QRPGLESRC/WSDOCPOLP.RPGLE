     H Copyright('(c) 2023 Sapiens. All Rights Reserved.')
     H**********************************************************************
     H/TITLE WSDOCPOLP - Scriptura Policy Page Document Setup
     H DFTNAME(WSDOCPOLP)
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO) ACTGRP(*caller)
     H/DEFINE   PROFOUNDUI
      *** Beg Add ***** 08/21/23 *******************************************
     H*BNDDIR('WTBNDDIR')
     H BNDDIR('STBNDDIR')
      *** End Add ***** 08/21/23 *******************************************
     h**********************************************************************
    *===============================================================*
    *  Created    : 09/16/16                                        *
    *  Programmer : C:65                                            *
    *  Project    : SY371                                           *
    *  Description: This is the new scriptura document setup scren  *
    *               program for policy pages.                       *
    *               It is accessed as an additional setup screen    *
    *               from the Output Control tab of Application      *
    *               Configuration.                                  *
    *===============================================================*
    *  Revisions:                                                   *
      *                                                               *
      * 09/25/23 C:65  Project: 153WAB    (Vital Services)            *
      *                Display file change - display the new dec page *
      *                checkbox when adding a record for Endorsement  *
      *                schedule setup.                                *
      *                                                               *
      * 08/21/23 C:333 Project: WC2876                                *
      *                Output Control Functionality Enhancements      *
      *                                                               *
      * 03/27/23 C:65  Project: 153WAB    (Vital Services)            *
      *                Add checkbox for listing the declaration page  *
      *                on the schedule of forms.                      *
      *                                                               *
      * 04/28/22 C:291 Project: WC2670                                *
      *                Add filter option on all the grids.            *
      *                                                               *
      * 09/16/21 C:65  Project: 148WDC    (Amwins)                    *
      *                Add entry for national/state form to allow the *
      *                form setup to be reported as a carrier form,   *
      *                replacing the entered national/state form.     *
      *                                                               *
      * 08/30/19 C:65  Project: Phw#82385 (LUBA)                      *
      *                Ensure schedule type gets populated for form   *
      *                template records as values are present for     *
      *                different form types.                          *
      *                                                               *
      * 12/11/17 C:65  Project: 148WAN                                *
      *                Add fields for originating transaction form:   *
      *                template, path, form#, edition date, and desc. *
      *                                                               *
      * 11/11/17 C:062 Project: 148WAN                                *
      *                Add AmWins Schedule Types                      *
      *                                                               *
      * 10/30/17 C:062 Project: 148WAN                                *
      *                Add the following fields:                      *
      *                List schedules on schedule of endorsement forms*
      *                Client template form                           *
      *                Edition date                                   *
      *                Form Description                               *
      *                                                               *
      * 10/24/17 C:136 Project: 150WBW                                *
      *                Add endorsement signature grid                 *
      *                                                               *
      * 08/18/17 C:291 PROJECT: SY371                                 *
      *                Removed Form type/state combination edit from  *
      *                Policy Page Template Forms. Form Type table    *
      *                setup is not required for schedules.           *
      *                                                               *
      * 08/17/17 C:291 PROJECT: C150WAO                               *
      *                Add new field Always produce schedule of       *
      *                Endorsements.                                  *
      *                                                               *
      * 07/19/17 C:65  PROJECT: WC2492                                *
      *                Add POC notice type for notice grid.           *
      *                                                               *
      * 01/27/17 C:65  PROJECT: SY371                                 *
      *                Add types for notice grid.                     *
    *                                                               *
    * ------------------------------------------------------------- *
    *  NOTE: Please add comments to the top of Revisions.           *
    * ------------------------------------------------------------- *
    *===============================================================*

     fwsdocpolp cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflSched:rrnSt)
     f                                     Sfile(sflNotice:rrnNt)
      *** beg add ***** 10/24/17 *******************************
     f                                     Sfile(sflEndSig:rrnESGrid)
      *** end add ***** 10/24/17 *******************************

     FWMUPL     IF   E           K Disk    Rename(PUP01:PUP00)
     FWSPPL     IF   E           K Disk    Rename(PPP01:PPP00)
     F                                     infds(dbfds)
      *** beg add ***** 10/24/17 ***************************
     FWSPPL1    IF   E           K Disk    Rename(PPP01:PPP01)
     F                                     infds(dbfds1)
      *** end add ***** 10/24/17 ***************************
     FWSPPP     UF A E             Disk    Rename(PPP01:PPP0P)
     F*** begin add ***  01/27/17  **************************************
     fstgtl     if   e           k disk    rename(pgt01:pgt00)

     D xxTbid          s                   like(GTTbid)
     D xxKey           s                   like(GTKey)
     F*** end add *****  01/27/17  **************************************
      *** Beg Add ***** 08/21/23 ***************************************
     fwmfnl     if   e           k disk    rename(pfn01:pfn00)
     f                                     usropn
     f                                     infds(dbfd14)
     fwdf2l     if   e           k disk    rename(pf201:pf200)
     f                                     usropn

     D PSDS          ESDS                  Extname(SZQ1P)

     d dbfds           ds
     d  dbrrn                397    400b 0
      *** End Add ***** 08/21/23 ***************************************
      *** beg add ***** 10/24/17 ***************************
     d dbfds1          ds
     d  dbrrn1               397    400b 0
      *** end add ***** 10/24/17 ***************************
     d*** Beg Add *** 08/21/23 ***************************************
     d dbfd14          ds
     d  dbrrn14              397    400b 0
     d*** End Add *** 08/21/23 ***************************************

     D process         s               n   inz(*off)
     D clientID        s              3    inz('000')
     d xxCo#           s              3  0 inz(0)
     d xxType          s              1    inz(' ')
     d xxSelRRN        s              9  0 inz(0)
     d xxSchdType      s              1    inz(' ')
     d xxStEffDt       s              7  0 inz(0)
      *** beg add ***** 10/24/17 ********************
     d xxESEfDt        s              7  0 inz(0)
      *** end add ***** 10/24/17 ********************
      *** Begin Add *** 11/11/17 *****************************
     d AmWinsSchdTyp   s            100    Inz(',Estimated Payment, -
     d                                     Self-Reporting,Self-Reporting -
     d                                      with Payment')
     d AmWinsSchdVal   s             10    Inz(',1,2,3')
      *** End   Add *** 11/11/17 *****************************
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

      * Add/Edit/Copy Setup record
     d SetupWindow     pr
     D inSetupMode                    1    const
     D inSetupType                    1    const
     D inSetupRRN                     9  0 const options(*nopass)
      *** beg add ***** 10/24/17 **********************
     d EndSigWindow    pr
      *** end add ***** 10/24/17 **********************
     D inSetupMode                    1    const
     D inSetupType                    1    const
     D inSetupRRN                     9  0 const options(*nopass)
      //*** Beg Add ***** 08/21/23 *******************************
      *
     d GetCarrier#     pr             5s 0
     d infnd                               like(f2fnd)
      *
      //*** End Add ***** 08/21/23 *******************************

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
     C     keyWSPPL      Klist
     C                   Kfld                    xxType
     C*
     C     key1WSPPL     Klist
     C                   Kfld                    xxType
     C                   Kfld                    scStType
     C                   Kfld                    scStGrp
     C                   Kfld                    scStState
     C                   Kfld                    xxStEffDt

      *** beg add ***** 10/24/17 *************************
     C     key1WSPPL1    Klist
     C                   Kfld                    xxType
     C                   Kfld                    scESGrp
     C                   Kfld                    scESForm
     C                   Kfld                    scESState
     C                   Kfld                    xxESEfDt
      *** end add ***** 10/24/17 *************************
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
     D*
      /free

       Exsr LoadRecords;

       //*  Process until exit button is pressed.

       DoU btnCancel = *on or btnSave = *on;

          //*  Process screen.

          //*** Beg Add ***** 08/21/23 ******************************
          scOutType = 'Policy Page';
          write rcdSetup;
          //*** End Add ***** 08/21/23 ******************************
          Exfmt rcdSetup;

          //Perform actions on screen.
          Select;

          //* Exit the program
          When btnCancel = *on;
             Leave;

          //* Add a new schedule setup record.
          When btnStAdd = *on;
             btnStAdd = *off;
             xxType = 'S';
             Exsr AddSetupRcd;
             Exsr LoadRecords;

          //* Add a new loss notice setup record.
          When btnNtAdd = *on;
             btnNtAdd = *off;
             xxType = 'N';
             Exsr AddSetupRcd;
             Exsr LoadRecords;

          //*** beg add ***** 10/24/17 **************
          // Add a new endorsement signature record.
          When btnEsAdd = *on;
           btnEsAdd = *off;
             xxType = 'E';
             Exsr AddEndSigRcd;
             Exsr LoadRecords;
          //*** end add ***** 10/24/17 **************

          Other;

             //*  Check for clicked links on grid.
             Readc sflSched;
             If not %EOF();

                //*  Edit the schedule setup record.
                If schedEdit = *on;
                   schedEdit = *off;
                   xxType = 'S';
                   xxSelRRN = outStRRN;
                   Exsr EditSetupRcd;
                Endif;

                //*  Delete the schedule setup record.
                If schedDelt = *on;
                   schedDelt = *off;
                   If outStEffDt = 01011999;
                      PopOneBtn('The original 1/1/1999 setup records cannot'+
                       ' be deleted.':'Deletion':'Ok');
                   Else;
                      xxType = 'S';
                      xxSelRRN = outStRRN;
                      Exsr DeleteSetupRcd;
                   Endif;
                Endif;

                //*  Copy the schedule setup record.
                If schedCopy = *on;
                   schedCopy = *off;
                   xxType = 'S';
                   xxSelRRN = outStRRN;
                   Exsr CopySetupRcd;
                Endif;

                Update sflSched;

                Exsr LoadRecords;

             Endif;

             //*  Check for clicked links on grid.
             Readc sflNotice;
             If not %EOF();

                //*  Edit the notice setup record.
                If noteEdit = *on;
                   noteEdit = *off;
                   xxType = 'N';
                   xxSelRRN = outNtRRN;
                   Exsr EditSetupRcd;
                Endif;

                //*  Delete the notice setup record.
                If noteDelt = *on;
                   noteDelt = *off;
                   If outNtEffDt = 01011999;
                      PopOneBtn('The original 1/1/1999 setup records cannot'+
                       ' be deleted.':'Deletion':'Ok');
                   Else;
                      xxType = 'N';
                      xxSelRRN = outNtRRN;
                      Exsr DeleteSetupRcd;
                   Endif;
                Endif;

                //*  Copy the notice setup record.
                If noteCopy = *on;
                   noteCopy = *off;
                   xxType = 'N';
                   xxSelRRN = outNtRRN;
                   Exsr CopySetupRcd;
                Endif;

                Update sflNotice;

                Exsr LoadRecords;

             Endif;

          //*** beg add ***** 10/24/17 *******************
             //*  Check for clicked links on grid.
             ReadC sflEndSig;
             If Not %Eof();

                //*  Edit the schedule setup record.
                If endSigEdit = *on;
                   endSigEdit = *off;
                   xxType = 'E';
                   xxSelRRN = outESRRN;
                   Exsr EditEndSigRcd;
                Endif;

                //*  Delete the schedule setup record.
                If endSigDlt = *on;
                   endSigDlt = *off;
                   If outESEfDt = 01011999;
                      PopOneBtn('The original 1/1/1999 setup records cannot'+
                       ' be deleted.':'Deletion':'Ok');
                   Else;
                      xxType = 'E';
                      xxSelRRN = outESRrn;
                      Exsr DeleteEndSigRcd;
                   Endif;
                Endif;

                //*  Copy the schedule setup record.
                If endSigCopy = *on;
                   endSigCopy = *off;
                   xxType = 'E';
                   xxSelRRN = outESRRN;
                   Exsr CopyEndSigRcd;
                Endif;

                Update sflEndSig;

                Exsr LoadRecords;

             Endif;
          //*** end add ***** 10/24/17 *******************

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
       dspNt = *off;
       dspNtCtl = *off;
       clrNt = *on;
       //*** beg add ***** 10/24/17 *************
       dspESGRid = *Off;
       ctlESGrid = *Off;
       clrESGrid = *On;
       //*** end add ***** 10/24/17 *************
       Write rcdSetup;
       clrSt = *off;
       dspSt = *on;
       dspStCtl = *on;
       rrnSt = 0;
       clrNt = *off;
       dspNt = *on;
       dspNtCtl = *on;
       rrnNt = 0;
       //*** beg add ***** 10/24/17 *************
       dspESGRid = *On;
       clrESGrid = *Off;
       ctlESGrid = *On;
       rrnESGrid = 0;
       //*** end add ***** 10/24/17 *************

       // Load the schedule subfile.

       xxType = 'S';

       Setll keyWSPPL ppp00;
       Reade keyWSPPL ppp00;

       Dow not %eof;

          outSchdTyp = ' ';

          Select;
          When PPSType = 'D';
             outSchdTyp = 'Declaration';
          When PPSType = 'W';
             outSchdTyp = 'Workplaces';
          When PPSType = 'N';
             outSchdTyp = 'Named Insured';
          When PPSType = 'P';
             outSchdTyp = 'Premium';
          When PPSType = 'E';
             outSchdTyp = 'Endorsements';
          //*** Begin Add *** 11/11/17 *************************
          When PPSType = '1' And clientID = '148';
             outSchdTyp = 'Estimated Payment';
          When PPSType = '2' And clientID = '148';
             outSchdTyp = 'Self-Reporting';
          When PPSType = '3' And clientID = '148';
             outSchdTyp = 'Self-Reporting with Payment';
          //*** End   Add *** 11/11/17 *************************
          Endsl;

          If PPFnd = 0;
             outStGrp = 'All';
          //***** Beg Chg *** 08/21/23 ***************************
          //***Else;
          //***   outStGrp = %editc(PPFnd : 'X');
             outPPCarr = ' ';
          Else;
             outStGrp = %editc(PPFnd : 'X');
             outPPCarr = %editc(GetCarrier#(PPFnd) : 'X');
          //***** End Chg *** 08/21/23 ***************************
          Endif;

          outStState = PPSt;
          outStEffDt = dtMDYY(PPEFDT);
          outSchdDef = %trim(PPPOLPTH) + %trim(PPPOLDOC);
          outSchdClt = %trim(PPPOLPTHC) + %trim(PPPOLDOCC);
          outStRRN  = dbrrn;

          rrnSt  =  rrnSt + 1;

          Write sflSched;

          Reade keyWSPPL ppp00;
       Enddo;

       rrnSt = 1;

       // Load the notice subfile.

       xxType = 'N';

       Setll keyWSPPL ppp00;
       Reade keyWSPPL ppp00;

       Dow not %eof;

          //*** begin add ***  01/27/17  *******************************************
          outNtType = ' ';

          Select;
          When PPSType = 'L';
             outNtType = 'Loss Prevention';
          When PPSType = 'D';
             outNtType = 'Deductible Opt In';
          When PPSType = 'P';
             outNtType = 'Policy Notice';
          //*** begin add ***  07/19/17  *******************************************
          When PPSType = 'C';
             outNtType = 'Proof of Coverage Notice';
          //*** end add *****  07/19/17  *******************************************
          Endsl;
          //*** end add *****  01/27/17  *******************************************

          If PPFnd = 0;
             outNtGrp = 'All';
          //***** Beg Chg *** 08/21/23 ***************************
          //***Else;
          //***   outNtGrp = %editc(PPFnd : 'X');
             outPFCarr = ' ';
          Else;
             outNtGrp = %editc(PPFnd : 'X');
             outPFCarr = %editc(GetCarrier#(PPFnd) : 'X');
          //***** End Chg *** 08/21/23 ***************************
          Endif;

          outNtState = PPSt;
          outNtEffDt = dtMDYY(PPEFDT);
          outNoteDef = %trim(PPPOLPTH) + %trim(PPPOLDOC);
          outNoteClt = %trim(PPPOLPTHC) + %trim(PPPOLDOCC);
          outNtRRN  = dbrrn;

          rrnNt  =  rrnNt + 1;

          Write sflNotice;

          Reade keyWSPPL ppp00;
       Enddo;

       rrnNt = 1;

       //*** beg add ***** 10/24/17 **************

       // Load the endorsement signature subfile.
       xxType = 'E';

       Setll keyWSPPL ppp01;
       Reade keyWSPPL ppp01;
       Dow Not %Eof;
          If PPFnd = 0;
             outESGrp = 'All';
          //***** Beg Chg *** 08/21/23 ***************************
          //***Else;
          //***   outESGrp = %editc(PPFnd : 'X');
             outESCarr = ' ';
          Else;
             outESGrp = %editc(PPFnd : 'X');
             outESCarr = %editc(GetCarrier#(PPFnd) : 'X');
          //***** End Chg *** 08/21/23 ***************************
          Endif;
          outESForm = PPEndFrm;
          outESState = PPSt;
          outESEfDt  = dtMDYY(PPefdt);
          out1stSig  = %Trim(PP1stSPth) + PP1stSig;
          out2ndSig  = %Trim(PP2ndSPth) + PP2ndSig;
          outESRrn   = dbRrn1;
          rrnESGrid  =  rrnESGrid + 1;
          Write sflEndSig;
          Reade keyWSPPL ppp01;
       Enddo;
       rrnESGrid = 1;
       //*** end add ***** 10/24/17 **************

       Endsr;

       //*** beg add ***** 10/24/17 **************
        //-----------------------------------------
       Begsr AddEndSigRcd;
       //-----------------------------------------
       EndSigWindow('A':xxType);
       Endsr;
       //-----------------------------------------
       Begsr EditEndSigRcd;
       //-----------------------------------------
       EndSigWindow('E':xxType:xxSelRRN);
       Endsr;
       //-----------------------------------------
       Begsr CopyEndSigRcd;
       //-----------------------------------------
       EndSigWindow('C':xxType:xxSelRRN);
       Endsr;
       //-----------------------------------------
       Begsr DeleteEndSigRcd;
       //-----------------------------------------
       EndSigWindow('D':xxType:xxSelRRN);
       Endsr;
       //-----------------------------------------
       //*** end add ***** 10/24/17 **************

       //=============================================================
       // Add State Setup Record
       //=============================================================
       Begsr AddSetupRcd;

       SetupWindow('A':xxType);

       Endsr;


       //===================================================================
       // Edit State Setup Record
       //===================================================================
       Begsr EditSetupRcd;

       SetupWindow('E':xxType:xxSelRRN);

       Endsr;


       //===================================================================
       // Delete State Setup Record
       //===================================================================
       Begsr DeleteSetupRcd;

       SetupWindow('D':xxType:xxSelRRN);

       Endsr;


       //===================================================================
       // Copy State Setup Record
       //===================================================================
       Begsr CopySetupRcd;

       SetupWindow('C':xxType:xxSelRRN);

       Endsr;

      /end-free
     P Main            E

       //===================================================================
       // Add/Edit/Copy Endorsement Signature.
       //===================================================================
     p EndSigWindow    b
     d EndSigWindow    pi
     D inSetupMode                    1    const
     D inSetupType                    1    const
     D inSetupRRN                     9  0 const options(*nopass)

     d error           s               n   inz(*off)
     d isAddMode       s               n   inz(*off)
     d message         s            100
     d strMsg          s            200
     d strSlashBeg     s              1
     d strSlashEnd     s              1
     d xxRRN           s              9  0 inz(0)

      /free
       If %parms >= 3;
          xxRRN = inSetupRRN;
       Endif;

       fndChoices = 'FNCO# = ' + %char(xxCO#);
       isAddMode  = *off;
       dsbEsGrpSt = *off;
       //*** Beg Add ***** 08/21/23 *********************************
       dsbEsGrpS1 = *off;
       //*** End Add ***** 08/21/23 *********************************
       dspEsAdd   = *off;

       If xxRRN <> 0;
          Chain(n) xxRRN ppp0p;
       //*** Beg Chg ***** 08/21/23 **********************************
          //***If inSetupMode <> 'C';
          //***   dsbEsGrpSt = *on;
          //***   dspESAdd   = *off;
          //***Endif;
          If inSetupMode = 'C';
             isAddMode = *on;
             dspESAdd = *on;
             dsbEsGrpSt = *on;
             btnName = 'Proceed To Copy';
          Else;
             dsbEsGrpSt = *on;
             dsbEsGrpS1 = *on;
             dspESAdd   = *off;
             btnName = 'Save';
          Endif;
       //*** End Chg ***** 08/21/23 **********************************
       Else;
          dspESAdd   = *on;
          isAddMode = *on;
       //*** Beg Add ***** 08/21/23 **********************************
          btnName = 'Save';
       //*** End Add ***** 08/21/23 **********************************
       Endif;

       //* Keep bringing up new popup windows in Add mode until cancel is hit.
       DoU isAddMode = *Off;

          //* Clear screen fields prior to display if in Add Mode.
       //*** Beg Chg ***** 08/21/23 **********************************
          //***If isAddMode = *on;
          If isAddMode = *on and inSetupMode <> 'C';
       //*** End Chg ***** 08/21/23 **********************************
             Clear scESForm;
             Clear scESGrp;
             Clear scESState;
             Clear scESEfDt;
             Clear scES1stSig;
             Clear scES1stPth;
             Clear scES2ndSig;
             Clear scES2ndPth;
             posESCxBtn = 630;
          Else;
             scESGrp    = PPfnd;
             scESState  = PPst;
             scESEfDt   = dtMDYY(PPefdt);
             scESForm   = PPEndFrm;
             scES1stSig = PP1stSig;
             scES1stPth = PP1stSPth;
             scES2ndSig = PP2ndSig;
             scES2ndPth = PP2ndSPth;
             posEsCxBtn = 755;
          Endif;

          Exsr ClearErrors;
          Clear btnESCx;
          Clear btnESAdd;
          Clear btnESSave;

       //* Load the screen for display and then request confirmation of deletion.
       If inSetupMode = 'D';
          Write rcdEndSig;
          strMsg = 'Do you want to continue with the deletion?';
          If not PopTwoBtn(strmsg:'Confirm':'Cancel':'Continue');
             Chain xxRRN ppp0p;
             Delete ppp0p;
          Endif;
          Return;
       Endif;

       //* Display popup window for the Endorsement Signature Setup record.

          Dou btnESSave = *on or btnESCx = *on or btnESAdd = *on;
             Dou error = *off or btnESCx = *on;
                Exfmt rcdEndSig;
                Exsr ClearErrors;
       //*** Beg Chg ***** 08/21/23 **********************************
                //***If btnESCx = *off;
                If btnESCx = *off and inSetupMode <> 'C';
       //*** End Chg ***** 08/21/23 **********************************
                   Exsr SetupCheck;
                Endif;
             Enddo;
          Enddo;

          If btnESSave = *on or btnESAdd = *on;
             //*** Beg Chg ***** 08/21/23 ****************************
             //***Exsr SetupSave;
             If inSetupMode <> 'C';
                Exsr SetupSave;
             Else;
                inData = 'Copy Endorsement Signatures';
                xxrrn = 0;
                If outESGrp <> 'All';
                   Open WMFNL;
                   Chain(n) (intCo#:%dec(outESGrp:3:0)) pfn00;
                   If %found;
                      xxrrn = dbrrn14;
                   Endif;
                   Close WMFNL;
                Endif;
                ProceedToCopy(xxrrn:inData:scOutType:scESEfDt:outESRRN:
                     intCo#:outESState:PPtype:PPStype:outESForm:inLeft:inTop);
             Endif;
             //*** End Chg ***** 08/21/23 ****************************
          Endif;

          If btnESCx = *on or btnESSave = *on;
             isAddMode = *off;
          Endif;

       Enddo;
       //===================================================================
       // Clear the error fields for window.
       //===================================================================
       BegSr ClearErrors;

       error = *off;
       Clear msNoEsForm;
       Clear erNoEsForm;
       Clear msDupEsRec;
       Clear erDupEsRec;
       Clear msEsStGrp;
       Clear erEsStGrp;
       Clear msEsEfDt;
       Clear erEsEfDt;
       Clear msEs1stSig;
       Clear erEs1stSig;
       Clear msEs2ndSig;
       Clear erEs2ndSig;
       Clear msEs1stPth;
       Clear erEs1stPth;
       Clear msEs2ndPth;
       Clear erEs2ndPth;

       Endsr;
       //===================================================================
       // Edit the endorsement signature fields.
       //===================================================================
       Begsr SetupCheck;

       // Endorsement form is required.
       If scEsForm = *Blanks;
          erNoEsForm = *On;
          msNoEsForm = 'An endorsement form entry is required.';
          error = *On;
       Endif;

       // Effective date must be entered and valid.
       erESEfDt = Valid_Date(scEsEfDt:msStEffDt);
       If erEsEfDt  = *On;
          msEsEfDt  = 'A valid effective date is a required entry.';
          error = *On;
       Endif;

       xxEsEfDt = dtCYMD(scEsEfDt);

       // Check for state setup record already exists.
       If error = *Off and (isAddMode = *on or inSetupMode = 'C');
          Chain(n) key1WSPPL1 ppp01;
          If %Found;
             msDupEsRec = 'A record already exists for this ' +
              'Group/Endorsement/State/Effective Date.';
             erDupEsRec = *On;
             error   = *On;
          Endif;
       Endif;

       If error = *On;
          LeaveSR;
       Endif;

       // Primary signature and signature path edits.
       If scEs1stSig <> *Blank;
         // Look for a '.' in the signature file name
          If %Scan('.':scEs1stSig) = 0;
             msES1stSig = 'Signature file name must have a file extension.';
             erES1stSig = *On;
             error      = *On;
          Endif;
       Else;
          erES1stSig = *On;
          msES1stSig = 'The primary signature is a required entry.';
          error = *On;
       Endif;

       // Look for a slash at beginning and end of path
       If scEs1stPth <> *Blanks;
          strSlashBeg = %Subst(scEs1stPth:1:1);
          strSlashEnd = %Subst(scEs1stPth:%Len(%Trim(scEs1stPth)):1);
          If strSlashBeg <> '/' or strSlashEnd <> '/';
             msES1stPth = 'Path name must begin and end with a ''/''.';
             erES1stPth = *On;
             error      = *On;
          Endif;
       Else;
          erES1stPth = *On;
          msES1stPth = 'The primary path name is a required entry.';
          error = *On;
       Endif;

       // Secondary signature and signature path edits.
       If scES2ndSig <> ' ';
         // Look for a '.' in the signature file name
          If %Scan('.':scEs2ndSig) = 0;
             msES2ndSig = 'Signature file name must have a file extension.';
             erES2ndSig = *On;
             error      = *On;
          Endif;
         // Look for a slash at beginning and end of path
          If scEs2ndPth <> *Blanks;
             strSlashBeg = %Subst(scEs2ndPth:1:1);
             strSlashEnd = %Subst(scEs2ndPth:%Len(%Trim(scEs1stPth)):1);
             If strSlashBeg <> '/' or strSlashEnd <> '/';
                msES2ndPth = 'Path name must begin and end with a ''/''.';
                erES2ndPth = *On;
                error      = *On;
             Endif;
          Else;
             msES2ndPth = 'Secondary path name is required entry.';
             erES2ndPth = *On;
             error      = *On;
          Endif;
       Endif;

       Endsr;
       //===================================================================
       // Save the state setup record.
       //===================================================================
       Begsr SetupSave;

       If xxRRN <> 0 and inSetupMode <> 'C';
          Chain xxRRN ppp0p;
       Else;
          Clear ppp0p;
          PPType  = inSetupType;
          PPSType = *Blank;
          PPFnd   = scESGrp;
          PPSt    = scESState;
          PPEfdt  = dtCYMD(scESEfDt);
          PPIPgm  = Q1PGM;
          PPIUsr  = Q1USER;
          PPITim  = Timeto6(SystemTime());
          PPIDt   = Dateto7(SystemDate());
       Endif;

       PPEndFrm  = scESForm;
       PP1stSig  = scEs1stSig;
       PP1stSPth = scEs1stPth;
       PP2ndSig  = scEs2ndSig;
       PP2ndSPth = scEs2ndPth;
       PPPgm     = Q1pgm;
       PPUser    = Q1user;
       PPTime    = Timeto6(SystemTime());
       PPDt      = Dateto7(SystemDate());

       If xxRRN <> 0 and inSetupMode <> 'C';
          Update ppp0p;
       Else;
          Write ppp0p;
       Endif;

       Endsr;
      /end-free

     p EndSigWindow    e
       //===================================================================
       // Add/Edit/Copy Setup to file.
       //===================================================================
     p SetupWindow     b
     d SetupWindow     pi
     D inSetupMode                    1    const
     D inSetupType                    1    const
     D inSetupRRN                     9  0 const options(*nopass)

     d xxRRN           s              9  0 inz(0)
     d error           s               n   inz(*off)
     d steAddMode      s               n   inz(*off)
     d message         s            100
     d strMsg          s            200

      /free

       If %parms >= 3;
          xxRRN = inSetupRRN;
       Endif;


       Clear scWndTitle;
       Clear scTypeChoi;
       Clear scTypeVal;

       //*** Begin Add *** 10/30/17 *******************************
       visSchd = *off;
       //*** End Add ***** 10/30/17 *******************************

       Select;
       When inSetupType = 'S';
          //*** begin chg ***  01/27/17  *****************************************
          //*****scWndTitle = 'Policy Page Form';
          scWndTitle = 'Policy Page Templates';
          //*** end chg *****  01/27/17  *****************************************
          scTypeChoi = ' ,Declaration,Workplaces,Named Insured,Premium,' +
           'Endorsements';
          scTypeVal  = ' ,D,W,N,P,E';
          //*** Begin Add *** 11/11/17 *************************
          // Add AmWins specific schedule types
          If clientId = '148';
             scTypeChoi = %Trim(scTypeChoi) + AmWinsSchdTyp;
             scTypeVal  = %Trim(scTypeVal)  + AmWinsSchdVal;
          EndIf;
          //*** End   Add *** 11/11/17 *************************
          disbType = *off;
       //*** Begin Add *** 10/30/17 *******************************
          visSchd = *on;
       //*** End Add ***** 10/30/17 *******************************
       When inSetupType = 'N';
          //*** begin chg ***  01/27/17  *****************************************
          //*****scWndTitle = 'Loss Prevention Notice Form';
          //*****scTypeChoi = ' ,Loss Prevention Notice';
          //*****scTypeVal  = ' ,X';
          //*****disbType = *on;
          scWndTitle = 'Policy Form Templates';
          //*** begin chg ***  07/19/17  *******************************************
          //*****scTypeChoi = ' ,Deductible Opt In,Loss Prevention,Policy Notice';
          //*****scTypeVal  = ' ,D,L,P';
          scTypeChoi = ' ,Deductible Opt In,Loss Prevention,Policy Notice,'+
           'Proof of Coverage Notice';
          scTypeVal  = ' ,D,L,P,C';
          //*** end chg *****  07/19/17  *******************************************
          disbType = *off;
          //*** end chg *****  01/27/17  *****************************************
       Endsl;

       fndChoices = 'FNCO# = ' + %char(xxCO#);
       steAddMode = *off;
       steEditMde = *off;
       //*** Beg Add ***** 08/21/23 *********************************
       steEditM1 = *off;
       //*** End Add ***** 08/21/23 *********************************
       dspStAdd   = *off;

       If xxRRN <> 0;
          Chain(n) xxRRN ppp0p;
       //*** Beg Chg ***** 08/21/23 **********************************
          //***If inSetupMode <> 'C';
          //***   steEditMde = *on;
          //***   disbType = *on;
          //***Endif;
          If inSetupMode = 'C';
             steAddMode = *on;
             steEditMde = *on;
             disbType = *on;
             btnName = 'Proceed To Copy';
          Else;
             steEditMde = *on;
             steEditM1 = *on;
             disbType = *on;
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
          If steAddMode = *on and inSetupMode <> 'C';
       //*** End Chg ***** 08/21/23 **********************************
             Clear scStType;
          //*** begin del ***  01/27/17  *****************************************
          //*****   If inSetupType = 'N';
          //*****      scStType = 'X';
          //*****   Endif;
          //*** end del *****  01/27/17  *****************************************
             Clear scStGrp;
             Clear scStState;
             Clear scStEffDt;
             Clear scStCltTem;
             Clear scStCltPth;
             Clear scStDefTem;
             Clear scStDefPth;
             //*** Beg Add *****  08/17/17  *************************************
             Clear scStAlPsen;
             visPsen = *off;
             //*** End Add *****  08/17/17  *************************************
             //*** Begin Add *** 10/30/17 *******************************
             Clear scStCltFm#;
             Clear scStEdDt;
             Clear scStLstScd;
             Clear scStDesc;
             //*** End   Add *** 10/30/17 *******************************
             //*** begin add *** 12/11/17 *******************************
             Clear scStOrgTem;
             Clear scStOrgPth;
             Clear scStOrgFm#;
             Clear scStOrgEdD;
             Clear scStOrgDes;
             //*** end add ***** 12/11/17 *******************************
             //*** begin add *** 03/27/23 *******************************
             Clear scStLstDec;
             //*** end add ***** 03/27/23 *******************************
             stCancLeft = 630;
          Else;
             scStType = PPSTYPE;
          //*** begin del ***  01/27/17  *****************************************
          //*****   If inSetupType = 'N';
          //*****      scStType = 'X';
          //*****   Endif;
          //*** end del *****  01/27/17  *****************************************
             scStGrp = PPFND;
             scStState = PPST;
             scStEffDt = dtMDYY(PPEFDT);
             scStCltTem = PPPOLDOCC;
             scStCLTPth = PPPOLPTHC;
             scStDefTem = PPPOLDOC;
             //*** Beg Add *****  08/17/17  *************************************
             scStAlPsen = PPPSEN;
             If PPSTYPE = 'E';
               visPsen = *on;
             Else;
               visPsen = *off;
             Endif;
             //*** End Add *****  08/17/17  *************************************
             //*** Begin Add *** 10/30/17 *******************************
             scStCltFm# = PPcltFrm#;
             scStEdDt   = DTmdyy(PPeditDt);
             scStLstScd = PPlstSchd;
             scStDesc = PPDesc;
             //*** End   Add *** 10/30/17 *******************************
             scStDefPth = PPPOLPTH;
             //*** begin add *** 12/11/17 *******************************
             scStOrgTem = PPPOLDOCO;
             scStOrgPth = PPPOLPTHO;
             scStOrgFm# = PPORGFRM#;
             scStOrgEdD = PPEDITDTO;
             scStOrgDes = PPORGDESC;
             //*** end add ***** 12/11/17 *******************************
             //*** begin add ***  09/16/21  *****************************
             scStNatFm  = PPNatFm;
             //*** end add *****  09/16/21  *****************************
             //*** begin add *** 03/27/23 *******************************
             scStLstDec = PPlstDec;
             //*** end add ***** 03/27/23 *******************************
             stCancLeft = 745;
          Endif;

          Exsr ClearErrors;
          Clear btnStSave;
          Clear btnStAdd;
          Clear btnStCanc;

       //* Load the screen for display and then request confirmation of deletion.
       If inSetupMode = 'D';
          Write rcdWindow;
          strMsg = 'Do you want to continue with the deletion?';
          If not PopTwoBtn(strmsg:'Confirm':'Cancel':'Continue');
             Chain xxRRN ppp0p;
             Delete ppp0p;
          Endif;
          Return;
       Endif;

       //* Display popup window for the Schedule Setup record.

          Dou btnStSave = *on or btnStCanc = *on or btnStAdd = *on;

             Dou error = *off or btnStCanc = *on;

                Exfmt rcdWindow;

                Exsr ClearErrors;

       //*** Beg Chg ***** 08/21/23 **********************************
                //***If btnStCanc = *off;
                If btnStCanc = *off and inSetupMode <> 'C';
       //*** End Chg ***** 08/21/23 **********************************
                   Exsr SetupCheck;
                Endif;

             Enddo;

          Enddo;

          If btnStSave = *on or btnStAdd = *on;
             //*** Beg Chg ***** 08/21/23 ****************************
             //***Exsr SetupSave;
             If inSetupMode <> 'C';
                Exsr SetupSave;
             Else;
                xxrrn = 0;
                Select;
                 When inSetupType = 'S';
                    inData = 'Copy Policy Page Forms';
                    If outStGrp <> 'All';
                       Open WMFNL;
                       Chain(n) (intCo#:%dec(outStGrp:3:0)) pfn00;
                       If %found;
                          xxrrn = dbrrn14;
                       Endif;
                       Close WMFNL;
                    Endif;
                    ProceedToCopy(xxrrn:inData:scOutType:scStEffDt:outStRRN:
                           intCo#:outStState:PPtype:PPStype:*omit:inLeft:inTop);

                 When inSetupType = 'N';
                    inData = 'Copy Policy Form Templates';
                    If outNtGrp <> 'All';
                       Open WMFNL;
                       Chain(n) (intCo#:%dec(outNtGrp:3:0)) pfn00;
                       If %found;
                          xxrrn = dbrrn14;
                       Endif;
                       Close WMFNL;
                    Endif;
                    ProceedToCopy(xxrrn:inData:scOutType:scStEffDt:outNtRRN:
                           intCo#:outNtState:PPtype:PPStype:*omit:inLeft:inTop);

                Endsl;
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

       Clear erStType;
       Clear erStGrp;
       Clear erStState;
       Clear erStEffDt;
       Clear erStCltTem;
       Clear erStCltPth;
       Clear erStDefTem;
       Clear erStDefPth;
       //*** Begin Add *** 10/30/17 ************************************
       Clear erStCltFm#;
       Clear erStEdDt;
       Clear erStLstScd;
       //*** End   Add *** 10/30/17 ************************************
       //*** begin add *** 12/11/17 *******************************
       Clear erStOrgTem;
       Clear erStOrgPth;
       Clear erStOrgFm#;
       Clear erStOrgEdD;
       //*** end add ***** 12/11/17 *******************************
       //*** begin add ***  09/16/21  *****************************
       Clear erStNatFm;
       //*** end add *****  09/16/21  *****************************
       //*** begin add *** 03/27/23 *******************************
       Clear erStLstDec;
       //*** end add ***** 03/27/23 *******************************

       Clear msStType;
       Clear msStGrp;
       Clear msStState;
       Clear msStEffDt;
       Clear msStCltTem;
       Clear msStCltPth;
       Clear msStDefTem;
       Clear msStDefPth;
       //*** Begin Add *** 10/30/17 ************************************
       Clear msStCltFm#;
       Clear msStEdDt;
       Clear msStLstScd;
       //*** End   Add *** 10/30/17 ************************************
       //*** begin add *** 12/11/17 *******************************
       Clear msStOrgTem;
       Clear msStOrgPth;
       Clear msStOrgFm#;
       Clear msStOrgEdD;
       //*** end add ***** 12/11/17 *******************************
       //*** begin add ***  09/16/21  *****************************
       Clear msStNatFm;
       //*** end add *****  09/16/21  *****************************
       //*** begin add *** 03/27/23 *******************************
       Clear msStLstDec;
       //*** end add ***** 03/27/23 *******************************

       //*** Beg Add *****  08/17/17  *************************************
       // Set Indicatiors
       If scStType = 'E';
          visPsen = *on;
       Else;
          visPsen = *off;
       Endif;
       //*** End Add *****  08/17/17  *************************************

       Endsr;
       //===================================================================

       //===================================================================
       // Edit the state setup fields.
       //===================================================================
       Begsr SetupCheck;

       // Type must be selected.
       If scStType = '  ';
          msStType = 'A type must be selected.';
          erStType = *on;
          error = *on;
       Endif;

       // State must be selected for loss prevention notice.
       If scStState = '  ' and inSetupType = 'N';
          msStState = 'A state must be selected.';
          erStState = *on;
          error = *on;
       Endif;

       //*** begin add ***  01/27/17  *****************************************
       // State/Type combination must be valid for notice/form setup.
       //*** Beg Add *****  08/18/17  *************************************
       If inSetupType = 'N';
       //*** End Add *****  08/18/17  *************************************
         xxTBID = 'POLF';
         xxKey  = scStState;

         Setll (xxCo#:xxTbid:xxKey) pgt00;
         Reade(n) (xxCo#:xxTbid:xxKey) pgt00;
         Dow not %eof;
            If GTFG01 = scStType;
               Leave;
            Endif;
            Reade(n) (xxCo#:xxTbid:xxKey) pgt00;
         Enddo;

         If %eof and erStState <> *on and erStType <> *on;
            msStType = 'Form type is not set up for the state.';
            erStType = *on;
            error = *on;
         Endif;
       //*** Beg add *****  08/18/17  *************************************
       Endif;
       //*** End add *****  08/18/17  *************************************
       //*** end add *****  01/27/17  *****************************************

       // Effective date must be entered and valid.
       erStEffDt = Valid_Date(scStEffDt:msStEffDt);
       If erStEffDt = *on;
          msStEffDt = 'Effective Date must be a valid date.';
          error = *on;
       Endif;

       xxStEffDt = dtCYMD(scStEffDt);

       // Check for state setup record already exists.
       If error = *off and (steAddMode = *on or inSetupMode = 'C');

          Chain(n) key1WSPPL ppp00;

          If %found;
             msStGrp = 'A record already exists for this Type/Group/State/Eff' +
              'ective Date.';
             erStGrp = *on;
             error = *on;
          Endif;

       Endif;

       If error = *on;
          LeaveSR;
       Endif;

       // Client template.
       If scStCltTem <> ' ';

          // Validate the cx template name.
          erStCltTem = Valid_Document(xxCo#:'N':scStCltTem:message);
          If erStCltTem = *on;
             msStCltTem = message;
             error = *on;
          Endif;

          // Validate the client template path.
          erStCltPth = Valid_Path(scStCltPth:message);
          If erStCltPth = *on;
             msStCltPth = message;
             error = *on;
          Endif;


       //*** Begin Add *** 11/11/17 ********************************
       Else;
          // If AmWins And Schedule Type 1, 2 or 3 then Client Template
          // is required
          If clientId = '148' And (scStType = '1' Or scStType = '2' Or
             scStType = '3');
             msStCltTem = 'Client template information must be populated' +
                ' for installment schedules.';
             erStCltTem = *On;
             error      = *On;
          EndIf;
       //*** End   Add *** 11/11/17 ********************************
       Endif;

       //*** Begin Add *** 10/30/17 *******************************
       // Effective date must be valid if entered.
       If scStEdDt <> 0;
          erStEdDt = Valid_Date(scStEdDt:msStEdDt);
          If erStEdDt = *On;
             msStEdDt = 'Edition Date must be a valid date.';
             Error = *on;
          Endif;
       Endif;

       // List Schedule can only be checked if Always produce is checked
       If scStAlPsen <> 'Y' And scStlstScd = 'Y';
       //*** begin chg *** 03/27/23 *******************************
       //*****   msStLstScd = 'Edition Date must be a valid date.';
          msStLstScd = 'This box can only be checked if Always produce'+
           ' schedule is checked.';
       //*** end chg ***** 03/27/23 *******************************
          erStLstScd = *On;
          Error = *on;
       EndIf;
       //*** End   Add *** 10/30/17 *******************************
       //*** begin add *** 03/27/23 *******************************
       // List dec page can only be checked if Always produce is checked
       If scStAlPsen <> 'Y' And scStlstDec = 'Y';
          msStLstDec = 'This box can only be checked if Always produce'+
           ' schedule is checked.';
          erStLstDec = *On;
          error = *on;
       EndIf;
       //*** end add ***** 03/27/23 *******************************

       //*** begin add ***  09/16/21  *****************************
       // Client Template Form # must be entered if national/state
       // Form # has been entered.
       If scStCltFm# = ' ' and scStNatFm <> ' ';
          msStCltFm# = 'Client Template Form # must be entered ' +
           'to report carrier approved form information when ' +
           'national/state form # is populated.';
          erStCltFm# = *On;
          error = *on;
       Endif;

       // Do not allow blanks in national/state form # entry.
       If scStNatFm <> ' ';
          If %scan(' ':%trim(scStNatFm)) <> 0;
             msStNatFm = 'Do not enter spaces in the national/state ' +
              'form #.';
             erStNatFm = *On;
             error = *on;
          Endif;
       Endif;
       //*** end add *****  09/16/21  *****************************

       //*** begin add *** 12/11/17 *******************************

       // Originating template.
       If scStOrgTem <> ' ';

          // Validate the org template name.
          erStOrgTem = Valid_Document(xxCo#:'N':scStOrgTem:message);
          If erStOrgTem = *on;
             msStOrgTem = message;
             error = *on;
          Endif;

          // Validate the originating template path.
          erStOrgPth = Valid_Path(scStOrgPth:message);
          If erStOrgPth = *on;
             msStOrgPth = message;
             error = *on;
          Endif;

       Endif;

       // Effective date must be valid if entered.
       If scStOrgEdD <> 0;
          erStOrgEdD = Valid_Date(scStOrgEdD:msStOrgEdD);
          If erStOrgEdD = *On;
             msStOrgEdD = 'Originating Form Edition Date must be a valid date.';
             error = *on;
          Endif;
       Endif;
       //*** end add ***** 12/11/17 *******************************

       Endsr;
       //===================================================================

       //===================================================================
       // Save the state setup record.
       //===================================================================
       Begsr SetupSave;

       If xxRRN <> 0 and inSetupMode <> 'C';
          Chain xxRRN ppp0p;
       Else;

       //* Set the default template when writing new records.
          Setgt (inSetupType:scStType:scStGrp:scStState:xxStEffDt) ppp00;
          Readpe(n) (inSetupType:scStType:scStGrp:scStState) ppp00;
          If not %eof;
             scStDefTem = PPPolDoc;
             scStDefPth = PPPolPth;
          Else;
             Setgt (inSetupType:scStType:scStGrp:'  ':xxStEffDt) ppp00;
             Readpe(n) (inSetupType:scStType:scStGrp:'  ') ppp00;
             If not %eof;
                scStDefTem = PPPolDoc;
                scStDefPth = PPPolPth;
             Else;
                Setgt (inSetupType:scStType:000:scStState:xxStEffDt) ppp00;
                Readpe(n) (inSetupType:scStType:000:scStState) ppp00;
                If not %eof;
                   scStDefTem = PPPolDoc;
                   scStDefPth = PPPolPth;
                Else;
                   Setgt (inSetupType:scStType:000:'  ':xxStEffDt) ppp00;
                   Readpe(n) (inSetupType:scStType:000:'  ') ppp00;
                   If not %eof;
                      scStDefTem = PPPolDoc;
                      scStDefPth = PPPolPth;
                   Endif;
                Endif;
             Endif;
          Endif;

          Clear ppp0p;
          PPType = inSetupType;
          PPSType = scStType;
          //*** begin del ***  08/30/19  **************************************
          //*****If inSetupType = 'N';
          //*****   PPSType = ' ';
          //*****Endif;
          //*** end del *****  08/30/19  **************************************
          PPFnd  = scStGrp;
          PPSt   = scStState;
          PPEfdt = dtCYMD(scStEffDt);
          PPIPgm = Q1PGM;
          PPIUsr = Q1USER;
          PPITim = Timeto6(SystemTime());
          PPIDt  = Dateto7(SystemDate());
       Endif;

       PPPOLDOCC = scStCltTem;
       PPPOLPTHC = scStCltPth;
       PPPOLDOC  = scStDefTem;
       PPPOLPTH  = scStDefPth;
       //*** Beg Add *****  08/17/17  *************************************
       PPPSEN    = scStAlPsen;
       //*** End Add *****  08/17/17  *************************************
       //*** Begin Add *** 10/30/17 *******************************
       PPcltFrm# = scStCltFm#;
       PPeditDt  = dtCYMD(scStEdDt);
       PPlstSchd = scStLstScd;
       PPDesc = scStDesc;
       //*** End   Add *** 10/30/17 *******************************
       //*** begin add *** 12/11/17 *******************************
       PPPOLDOCO = scStOrgTem;
       PPPOLPTHO = scStOrgPth;
       PPORGFRM# = scStOrgFm#;
       PPEDITDTO = scStOrgEdD;
       PPORGDESC = scStOrgDes;
       //*** end add ***** 12/11/17 *******************************
       //*** begin add ***  09/16/21  *****************************
       PPNatFm   = scStNatFm;
       //*** end add *****  09/16/21  *****************************
       //*** begin add *** 03/27/23 *******************************
       PPlstDec  = scStLstDec;
       //*** end add ***** 03/27/23 *******************************

       PPPgm  = Q1PGM;
       PPUser = Q1USER;
       PPTime = Timeto6(SystemTime());
       PPDt   = Dateto7(SystemDate());

       If xxRRN <> 0 and inSetupMode <> 'C';
          Update ppp0p;
       Else;
          Write ppp0p;
       Endif;

       Endsr;
       //===================================================================

      /end-free

     p SetupWindow     e
       //*** Beg Add ***** 08/21/23 **************************************
      //------------------------------------------
      // GetCarrier# - Getting Carrier
      //------------------------------------------
     p GetCarrier#     b
     d GetCarrier#     pi             5s 0
     d xxfnd                          3  0

      /Free
       If not %open(Wdf2l);
          Open Wdf2l;
       Endif;

       Chain (intCo#:xxfnd) pf200;
       Return F2car#;

       If %open(Wdf2l);
          Close Wdf2l;
       Endif;
      /End-Free

     p GetCarrier#     e
       //*** End Add ***** 08/21/23 **************************************
