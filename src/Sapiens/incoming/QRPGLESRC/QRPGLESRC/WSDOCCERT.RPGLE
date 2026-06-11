     H Copyright('(c) 2023 Sapiens. All Rights Reserved.')
     H**********************************************************************
     H/TITLE WSDOCCERT - Scriptura Certificate of Insurance Document Setup
     H DFTNAME(WSDOCCERT)
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO) ACTGRP(*caller)
     H/DEFINE   PROFOUNDUI
      *** Beg Chg ***** 08/21/23 ******************************************
     H*BNDDIR('WTBNDDIR')
     H BNDDIR('STBNDDIR')
      *** End Chg ***** 08/21/23 ******************************************
     h**********************************************************************
    *===============================================================*
    *  Created    : 03/08/17                                        *
    *  Programmer : C:65                                            *
    *  Project    : SY371                                           *
    *  Description: This is the new scriptura document setup scren  *
    *               program for certificates of insurance.          *
    *               It is accessed as an additional setup screen    *
    *               from the Output Control tab of Application      *
    *               Configuration.                                  *
    *                                                               *
    *===============================================================*
    *  Revisions:                                                   *
      *                                                               *
      * 08/21/23 C:333 Project: WC2876                                *
      *                Output Control Functionality Enhancements      *
      *                                                               *
      * 04/28/22 C:291 Project: WC2670                                *
      *                Add filter option on State Form Setup grid.    *
      *                                                               *
      * 10/04/21 C:300 Support: 98371 (In-House)                      *
      *                popup screen position/flow error               *
      *                Changed top position of Panel from 150 to 1060 *
      *                                                               *
    *                                                               *
    * ------------------------------------------------------------- *
    *  NOTE: Please add comments to the top of Revisions.           *
    * ------------------------------------------------------------- *
    *===============================================================*

     fwsdoccert cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflState:rrnSt)

     FWMUPL     IF   E           K Disk    Rename(PUP01:PUP00)
     FWSCIL     IF   E           K Disk    Rename(PCI01:PCI00)
     F                                     infds(dbfds)
     FWSCIP     UF A E             Disk    Rename(PCI01:PCI0P)
      *** Beg Add ***** 08/21/23 ***************************************
     fwmfnl     if   e           k disk    rename(pfn01:pfn00)
     f                                     usropn
     f                                     infds(dbfd14)
     fwdf2l     if   e           k disk    rename(pf201:pf200)
     f                                     usropn
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
     C     keyWSCIL      Klist
     C                   Kfld                    xxCo#
     C*
     C     key1WSCIL     Klist
     C                   Kfld                    xxCo#
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
          scOutType = 'Certificate of Insurance';
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

       Setll keyWSCIL pci00;
       Reade keyWSCIL pci00;

       Dow not %eof;

          If CIFnd = 0;
             outStGrp = 'All';
          //***** Beg Chg *** 08/21/23 ***************************
          //***Else;
          //***   outStGrp = %editc(CIFnd : 'X');
             outTICarr = ' ';
          Else;
             outStGrp = %editc(CIFnd : 'X');
             outTICarr = %editc(GetCarrier#(CIFnd) : 'X');
          //***** End Chg *** 08/21/23 ***************************
          Endif;

          outStState = CISt;
          outStEffDt = dtMDYY(CIEFDT);
          outStForm  = %trim(CIPATH) + %trim(CIDOC);
          outStFormC = %trim(CIPATHC) + %trim(CIDOCC);
          outStRRN   = dbrrn;

          rrnSt  =  rrnSt + 1;

          Write sflState;

          Reade keyWSCIL pci00;
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
       //*** Beg Add ***** 08/21/23 *****************************
       steEditM1 = *off;
       //*** End Add ***** 08/21/23 *****************************
       dspStAdd   = *off;

       If xxRRN <> 0;
          Chain(n) xxRRN pci0p;
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
             scStDoc = 'Certificate of Insurance.scd';
             scForm  = 'Certificate of Insurance.scd';
             scStPath = '/Compliance/Base/Policy/';
             Clear scStDocC;
             Clear scStPathC;
             stCancLeft = 715;
          Else;
             scStGrp = CIFND;
             scStState = CIST;
             scStEffDt = dtMDYY(CIEFDT);
             scStDoc   = CIDOC;
             scStPath  = CIPATH;
             scStDocC  = CIDOCC;
             scStPathC = CIPATHC;
             stCancLeft = 830;
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
             Chain xxRRN pci0p;
             Delete pci0p;
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
                inData = 'Copy Certificate of Insurance Forms';
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
       Clear erStDoc;
       Clear erStPath;
       Clear erStDocC;
       Clear erStPathC;

       Clear msStGrp;
       Clear msStState;
       Clear msStEffDt;
       Clear msStDoc;
       Clear msStPath;
       Clear msStDocC;
       Clear msStPathC;

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

          Chain(n) key1WSCIL pci00;

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

       // State default template.
       If scStDoc <> ' ';

          // Validate the default template name.
          erStDoc = Valid_Document(xxCo#:'N':scStDoc:message);
          If erStDoc = *on;
             msStDoc = message;
             error = *on;
          Endif;

          // Validate the default template path.
          erStPath = Valid_Path(scStPath:message);
          If erStPath = *on;
             msStPath = message;
             error = *on;
          Endif;

       Endif;

       // State client template.
       If scStDocC <> ' ';

          // Validate the client template name.
          erStDocC = Valid_Document(xxCo#:'N':scStDocC:message);
          If erStDocC = *on;
             msStDocC = message;
             error = *on;
          Endif;

          // Validate the client template path.
          erStPathC = Valid_Path(scStPathC:message);
          If erStPathC = *on;
             msStPathC = message;
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
          Chain xxRRN pci0p;
       Else;
          Clear pci0p;
          CICo#  = xxCo#;
          CIFnd  = scStGrp;
          CISt   = scStState;
          CIEfdt = dtCYMD(scStEffDt);
          CIIPgm = Q1PGM;
          CIIUsr = Q1USER;
          CIITim = Timeto6(SystemTime());
          CIIDt  = Dateto7(SystemDate());
       Endif;

       CIDoc     = scStDoc;
       CIPath    = scStPath;
       CIDocC    = scStDocC;
       CIPathC   = scStPathC;

       CIPgm  = Q1PGM;
       CIUser = Q1USER;
       CITime = Timeto6(SystemTime());
       CIDt   = Dateto7(SystemDate());

       If xxRRN <> 0 and inStateMode <> 'C';
          Update pci0p;
       Else;
          Write pci0p;
       Endif;

       Endsr;
       //===================================================================
       // Set the default template fields for state.
       //===================================================================
       Begsr StateDefaults;

          scStDoc   = 'Certificate of Insurance.scd';
          scStPath  = '/Compliance/Base/Policy/';

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
