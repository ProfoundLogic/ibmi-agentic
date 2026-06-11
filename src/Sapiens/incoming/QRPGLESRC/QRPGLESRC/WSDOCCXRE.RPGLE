     H Copyright('(c) 2023 Sapiens. All Rights Reserved.')
     H**********************************************************************
     H/TITLE WSDOCCXRE - Scriptura CX/RE Document Setup
     H DFTNAME(WSDOCCXRE)
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO) ACTGRP(*caller)
     H/DEFINE   PROFOUNDUI
      *** Beg Chg ***** 08/21/23 *************************************
     H*BNDDIR('WTBNDDIR')
     H BNDDIR('STBNDDIR')
      *** End Chg ***** 08/21/23 *************************************
     h**********************************************************************
    *===============================================================*
    *  Created    : 07/20/16                                        *
    *  Programmer : C:65                                            *
    *  Project    : SY371                                           *
    *  Description: This is the new scriptura document setup scren  *
    *               program for cancellations and reinstatements.   *
    *               It is accessed as an additional setup screen    *
    *               from the Output Control tab of Application      *
    *               Configuration.                                  *
    *                                                               *
    *  Note:        State defaults are coded both in the RPG here   *
    *               and in the display file to trigger the form     *
    *               names to be updated when a state is selected    *
    *               that has state specific forms (via state drop   *
    *               down box).                                      *
    *                                                               *
    *===============================================================*
    *  Revisions:                                                   *
      *                                                               *
      * 08/21/23 C:333 Project: WC2876                                *
      *                Output Control Functionality Enhancements      *
      *                                                               *
      * 05/10/23 C:062 Project: 148WEF                                *
      *                Add NJ specific CX and RE forms for defaults.  *
      *                                                               *
      * 04/28/22 C:291 Project: WC2670                                *
      *                Add filter option on State Form Setup grid.    *
      *                                                               *
    *  03/22/17 C:65  Project: SY371                                *
      *                 Add state setup for GA with just a state      *
    *                 specific reinstatement form.                  *
    *                                                               *
    * ------------------------------------------------------------- *
    *  NOTE: Please add comments to the top of Revisions.           *
    * ------------------------------------------------------------- *
    *===============================================================*

     fwsdoccxre cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflState:rrnSt)
      *** Beg Add ***** 08/21/23 ***************************************
     fwmfnl     if   e           k disk    rename(pfn01:pfn00)
     f                                     usropn
     f                                     infds(dbfd14)
     fwdf2l     if   e           k disk    rename(pf201:pf200)
     f                                     usropn
      *** End Add ***** 08/21/23 ***************************************

     FWMUPL     IF   E           K Disk    Rename(PUP01:PUP00)
     FWSCRL     IF   E           K Disk    Rename(PCR01:PCR00)
     F                                     infds(dbfds)
     FWSCRP     UF A E             Disk    Rename(PCR01:PCR0P)

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
     C     keyWSCRL      Klist
     C                   Kfld                    xxCo#
     C*
     C     key1WSCRL     Klist
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

          //*** Beg Add ***** 08/21/23 *******************************
          scOutType = 'Cancellations';
          write rcdSetup;
          //*** End Add ***** 08/21/23 *******************************
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

       Setll keyWSCRL pcr00;
       Reade keyWSCRL pcr00;

       Dow not %eof;

          If CRFnd = 0;
             outStGrp = 'All';
          //***** Beg Chg *** 08/21/23 ***************************
          //***Else;
          //***   outStGrp = %editc(CRFnd : 'X');
             outTICarr = ' ';
          Else;
             outStGrp = %editc(CRFnd : 'X');
             outTICarr = %editc(GetCarrier#(CRFnd) : 'X');
          //***** End Chg *** 08/21/23 ***************************
          Endif;

          outStState = CRSt;
          outStEffDt = dtMDYY(CREFDT);
          outStCForm = %trim(CRCXPATH) + %trim(CRCXDOC);
          outStRForm = %trim(CRREPATH) + %trim(CRREDOC);
          outStCxClt = %trim(CRCXPATHC) + %trim(CRCXDOCC);
          outStReClt = %trim(CRREPATHC) + %trim(CRREDOCC);
          outStRRN  = dbrrn;

          rrnSt  =  rrnSt + 1;

          Write sflState;

          Reade keyWSCRL pcr00;
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
       //*** Beg Add ***** 08/21/23 *********************************
       steEditM1 = *off;
       //*** End Add ***** 08/21/23 *********************************
       dspStAdd   = *off;

       If xxRRN <> 0;
          Chain(n) xxRRN pcr0p;
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
             scStCXTem = 'Cancel Notice.scd';
             scCXForm  = 'Cancel Notice.scd';
             scStCXPth = '/Compliance/Base/Policy/';
             scStRETem = 'Reinstate Notice.scd';
             scREForm  = 'Reinstate Notice.scd';
             scStREPth = '/Compliance/Base/Policy/';
             Clear scStCxTemC;
             Clear scStCxPthC;
             Clear scStReTemC;
             Clear scStRePthC;
             Clear scStCCvTem;
             Clear scStCCvPth;
             Clear scStRCvTem;
             Clear scStRCvPth;
             Clear scStMail;
             Clear scStMailP;
             Clear scSt1stTem;
             Clear scSt1stPth;
             Clear scSt2ndTem;
             Clear scSt2ndPth;
             Clear scSt3rdTem;
             Clear scSt3rdPth;
             Clear scSt4thTem;
             Clear scSt4thPth;
             scCXCrTem = 'CX Cert Letter.scd';
             scCXCrPth = '/Compliance/Base/Policy/';
             scCFCrTem = 'CX Confirmation Cert Letter.scd';
             scCFCrPth = '/Compliance/Base/Policy/';
             scRECrTem = 'Rein Cert Letter.scd';
             scRECrPth = '/Compliance/Base/Policy/';
             Clear scCxCrTemC;
             Clear scCxCrPthC;
             Clear scCfCrTemC;
             Clear scCfCrPthC;
             Clear scReCrTemC;
             Clear scReCrPthC;
             stCancLeft = 715;
          Else;
             scStGrp = CRFND;
             scStState = CRST;
             scStEffDt = dtMDYY(CREFDT);
             scStCxTem = CRCXDOC;
             scStCxPth = CRCXPATH;
             scStCxTemC = CRCXDOCC;
             scStCxPthC = CRCXPATHC;
             scStReTem = CRREDOC;
             scStRePth = CRREPATH;
             scStReTemC = CRREDOCC;
             scStRePthC = CRREPATHC;
             scStCCvTem = CRCCov;
             scStCCvPth = CRCCovP;
             scStCCvCde = CRCCRC;
             scStRCvTem = CRRCov;
             scStRCvPth = CRRCovP;
             scStMail  = CRMail;
             scStMailP = CRMailP;
             scSt1stTem = CR1stC;
             scSt1stPth = CR1stCP;
             scSt2ndTem = CR2ndC;
             scSt2ndPth = CR2ndCP;
             scSt3rdTem = CR3rdC;
             scSt3rdPth = CR3rdCP;
             scSt4thTem = CR4thC;
             scSt4thPth = CR4thCP;
             scCxCrTem = CRCXCR;
             scCxCrPth = CRCXCRP;
             scCxCrTemC = CRCXCRC;
             scCxCrPthC = CRCXCRPC;
             scCfCrTem = CRCFCR;
             scCfCrPth = CRCFCRP;
             scCfCrTemC = CRCFCRC;
             scCfCrPthC = CRCFCRPC;
             scReCrTem = CRRECR;
             scReCrPth = CRRECRP;
             scReCrTemC = CRRECRC;
             scReCrPthC = CRRECRPC;
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
             Chain xxRRN pcr0p;
             Delete pcr0p;
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
                inData = 'Copy Cancellation/Reinstatement';
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
       Clear erStRETem;
       Clear erStREPth;
       Clear erStRETemC;
       Clear erStREPthC;
       Clear erStCCvTem;
       Clear erStCCvPth;
       Clear erStRCvTem;
       Clear erStRCvPth;
       Clear erStMail;
       Clear erStMailP;
       Clear erSt1stTem;
       Clear erSt1stPth;
       Clear erSt2ndTem;
       Clear erSt2ndPth;
       Clear erSt3rdTem;
       Clear erSt3rdPth;
       Clear erSt4thTem;
       Clear erSt4thPth;
       Clear erCXCrTem;
       Clear erCXCrPth;
       Clear erCXCrTemC;
       Clear erCXCrPthC;
       Clear erCFCrTem;
       Clear erCFCrPth;
       Clear erCFCrTemC;
       Clear erCFCrPthC;
       Clear erRECrTem;
       Clear erRECrPth;
       Clear erRECrTemC;
       Clear erRECrPthC;

       Clear msStGrp;
       Clear msStState;
       Clear msStEffDt;
       Clear msStCXTem;
       Clear msStCXPth;
       Clear msStCXTemC;
       Clear msStCXPthC;
       Clear msStRETem;
       Clear msStREPth;
       Clear msStRETemC;
       Clear msStREPthC;
       Clear msStCCvTem;
       Clear msStCCvPth;
       Clear msStRCvTem;
       Clear msStRCvPth;
       Clear msStMail;
       Clear msStMailP;
       Clear msSt1stTem;
       Clear msSt1stPth;
       Clear msSt2ndTem;
       Clear msSt2ndPth;
       Clear msSt3rdTem;
       Clear msSt3rdPth;
       Clear msSt4thTem;
       Clear msSt4thPth;
       Clear msCXCrTem;
       Clear msCXCrPth;
       Clear msCXCrTemC;
       Clear msCXCrPthC;
       Clear msCFCrTem;
       Clear msCFCrPth;
       Clear msCFCrTemC;
       Clear msCFCrPthC;
       Clear msRECrTem;
       Clear msRECrPth;
       Clear msRECrTemC;
       Clear msRECrPthC;

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

          Chain(n) key1WSCRL pcr00;

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

       // State Cancellation template.
       If scStCXTem <> ' ';

          // Validate the cx template name.
          erStCXTem = Valid_Document(xxCo#:'N':scStCXTem:message);
          If erStCXTem = *on;
             msStCXTem = message;
             error = *on;
          Endif;

          // Validate the cx template path.
          erStCXPth = Valid_Path(scStCXPth:message);
          If erStCXPth = *on;
             msStCXPth = message;
             error = *on;
          Endif;

       Endif;

       // State Reinstatement template.
       If scStRETem <> ' ';

          // Validate the rein template name.
          erStRETem = Valid_Document(xxCo#:'N':scStRETem:message);
          If erStRETem = *on;
             msStRETem = message;
             error = *on;
          Endif;

          // Validate the rein template path.
          erStREPth = Valid_Path(scStREPth:message);
          If erStREPth = *on;
             msStREPth = message;
             error = *on;
          Endif;

       Endif;

       // Client Cancellation template.
       If scStCXTemC <> ' ';

          // Validate the client cx template name.
          erStCXTemC = Valid_Document(xxCo#:'N':scStCXTemC:message);
          If erStCXTemC = *on;
             msStCXTemC = message;
             error = *on;
          Endif;

          // Validate the client cx template path.
          erStCXPthC = Valid_Path(scStCXPthC:message);
          If erStCXPthC = *on;
             msStCXPthC = message;
             error = *on;
          Endif;

       Endif;

       // Client Reinstatement template.
       If scStRETemC <> ' ';

          // Validate the client rein template name.
          erStRETemC = Valid_Document(xxCo#:'N':scStRETemC:message);
          If erStRETemC = *on;
             msStRETemC = message;
             error = *on;
          Endif;

          // Validate the client rein template path.
          erStREPthC = Valid_Path(scStREPthC:message);
          If erStREPthC = *on;
             msStREPthC = message;
             error = *on;
          Endif;

       Endif;

       // Cancellation Cover Letter template.
       If scStCCvTem <> ' ';

          // Validate the template name.
          erStCCvTem = Valid_Document(xxCo#:'N':scStCCvTem:message);
          If erStCCvTem = *on;
             msStCCvTem = message;
             error = *on;
          Endif;

          // Validate the template path.
          erStCCvPth = Valid_Path(scStCCvPth:message);
          If erStCCvPth = *on;
             msStCCvPth = message;
             error = *on;
          Endif;

       Endif;

       // Reinstatement Cover Letter template.
       If scStRCvTem <> ' ';

          // Validate the template name.
          erStRCvTem = Valid_Document(xxCo#:'N':scStRCvTem:message);
          If erStRCvTem = *on;
             msStRCvTem = message;
             error = *on;
          Endif;

          // Validate the template path.
          erStRCvPth = Valid_Path(scStRCvPth:message);
          If erStRCvPth = *on;
             msStRCvPth = message;
             error = *on;
          Endif;

       Endif;

       // Certified Mailing Letter template.
       If scStMail <> ' ';

          // Validate the template name.
          erStMail = Valid_Document(xxCo#:'N':scStMail:message);
          If erStMail = *on;
             msStMail = message;
             error = *on;
          Endif;

          // Validate the template path.
          erStMailP = Valid_Path(scStMailP:message);
          If erStMailP = *on;
             msStMailP = message;
             error = *on;
          Endif;

       Endif;

       // 1st Cancellation Letter template.
       If scSt1stTem <> ' ';

          // Validate the template name.
          erSt1stTem = Valid_Document(xxCo#:'N':scSt1stTem:message);
          If erSt1stTem = *on;
             msSt1stTem = message;
             error = *on;
          Endif;

          // Validate the template path.
          erSt1stPth = Valid_Path(scSt1stPth:message);
          If erSt1stPth = *on;
             msSt1stPth = message;
             error = *on;
          Endif;

       Endif;

       // 2nd Cancellation Letter template.
       If scSt2ndTem <> ' ';

          // Validate the template name.
          erSt2ndTem = Valid_Document(xxCo#:'N':scSt2ndTem:message);
          If erSt2ndTem = *on;
             msSt2ndTem = message;
             error = *on;
          Endif;

          // Validate the template path.
          erSt2ndPth = Valid_Path(scSt2ndPth:message);
          If erSt2ndPth = *on;
             msSt2ndPth = message;
             error = *on;
          Endif;

       Endif;

       // 3rd Cancellation Letter template.
       If scSt3rdTem <> ' ';

          // Validate the template name.
          erSt3rdTem = Valid_Document(xxCo#:'N':scSt3rdTem:message);
          If erSt3rdTem = *on;
             msSt3rdTem = message;
             error = *on;
          Endif;

          // Validate the template path.
          erSt3rdPth = Valid_Path(scSt3rdPth:message);
          If erSt3rdPth = *on;
             msSt3rdPth = message;
             error = *on;
          Endif;

       Endif;

       // 4th Cancellation Letter template.
       If scSt4thTem <> ' ';

          // Validate the template name.
          erSt4thTem = Valid_Document(xxCo#:'N':scSt4thTem:message);
          If erSt4thTem = *on;
             msSt4thTem = message;
             error = *on;
          Endif;

          // Validate the template path.
          erSt4thPth = Valid_Path(scSt4thPth:message);
          If erSt4thPth = *on;
             msSt4thPth = message;
             error = *on;
          Endif;

       Endif;

       // Default Cancellation Cert Holder template.
       If scCXCrTem <> ' ';

          // Validate the template name.
          erCXCrTem = Valid_Document(xxCo#:'N':scCXCrTem:message);
          If erCXCrTem = *on;
             msCXCrTem = message;
             error = *on;
          Endif;

          // Validate the template path.
          erCXCrPth = Valid_Path(scCXCrPth:message);
          If erCXCrPth = *on;
             msCXCrPth = message;
             error = *on;
          Endif;

       Endif;

       // Client Cancellation Cert Holder template.
       If scCXCrTemC <> ' ';

          // Validate the template name.
          erCXCrTemC = Valid_Document(xxCo#:'N':scCXCrTemC:message);
          If erCXCrTemC = *on;
             msCXCrTemC = message;
             error = *on;
          Endif;

          // Validate the template path.
          erCXCrPthC = Valid_Path(scCXCrPthC:message);
          If erCXCrPthC = *on;
             msCXCrPthC = message;
             error = *on;
          Endif;

       Endif;

       // Default Confirmation Cert Holder template.
       If scCFCrTem <> ' ';

          // Validate the template name.
          erCFCrTem = Valid_Document(xxCo#:'N':scCFCrTem:message);
          If erCFCrTem = *on;
             msCFCrTem = message;
             error = *on;
          Endif;

          // Validate the template path.
          erCFCrPth = Valid_Path(scCFCrPth:message);
          If erCFCrPth = *on;
             msCFCrPth = message;
             error = *on;
          Endif;

       Endif;

       // Client Confirmation Cert Holder template.
       If scCFCrTemC <> ' ';

          // Validate the template name.
          erCFCrTemC = Valid_Document(xxCo#:'N':scCFCrTemC:message);
          If erCFCrTemC = *on;
             msCFCrTemC = message;
             error = *on;
          Endif;

          // Validate the template path.
          erCFCrPthC = Valid_Path(scCFCrPthC:message);
          If erCFCrPthC = *on;
             msCFCrPthC = message;
             error = *on;
          Endif;

       Endif;

       // Default Reinstatement Cert Holder template.
       If scRECrTem <> ' ';

          // Validate the template name.
          erRECrTem = Valid_Document(xxCo#:'N':scRECrTem:message);
          If erRECrTem = *on;
             msRECrTem = message;
             error = *on;
          Endif;

          // Validate the template path.
          erRECrPth = Valid_Path(scRECrPth:message);
          If erRECrPth = *on;
             msRECrPth = message;
             error = *on;
          Endif;

       Endif;

       // Client Reinstatement Cert Holder template.
       If scRECrTemC <> ' ';

          // Validate the template name.
          erRECrTemC = Valid_Document(xxCo#:'N':scRECrTemC:message);
          If erRECrTemC = *on;
             msRECrTemC = message;
             error = *on;
          Endif;

          // Validate the template path.
          erRECrPthC = Valid_Path(scRECrPthC:message);
          If erRECrPthC = *on;
             msRECrPthC = message;
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
          Chain xxRRN pcr0p;
       Else;
          Clear pcr0p;
          CRCo#  = xxCo#;
          CRFnd  = scStGrp;
          CRSt   = scStState;
          CREfdt = dtCYMD(scStEffDt);
          CRIPgm = Q1PGM;
          CRIUsr = Q1USER;
          CRITim = Timeto6(SystemTime());
          CRIDt  = Dateto7(SystemDate());
       Endif;

       CRCXDOC   = scStCxTem;
       CRCXPATH  = scStCxPth;
       CRCXDOCC  = scStCxTemC;
       CRCXPATHC = scStCxPthC;
       CRREDOC   = scStReTem;
       CRREPATH  = scStRePth;
       CRREDOCC  = scStReTemC;
       CRREPATHC = scStRePthC;

       CRCCov   =  scStCCvTem;
       CRCCovP  =  scStCCvPth;
       CRCCRC   =  %trim(scStCCvCde);
       CRRCov   =  scStRCvTem;
       CRRCovP  =  scStRCvPth;
       CRMail   =  scStMail;
       CRMailP  =  scStMailP;
       CR1stC   =  scSt1stTem;
       CR1stCP  =  scSt1stPth;
       CR2ndC   =  scSt2ndTem;
       CR2ndCP  =  scSt2ndPth;
       CR3rdC   =  scSt3rdTem;
       CR3rdCP  =  scSt3rdPth;
       CR4thC   =  scSt4thTem;
       CR4thCP  =  scSt4thPth;
       CRCXCR   =  scCxCrTem;
       CRCXCRP  =  scCxCrPth;
       CRCXCRC  =  scCxCrTemC;
       CRCXCRPC =  scCxCrPthC;
       CRCFCR   =  scCfCrTem;
       CRCFCRP  =  scCfCrPth;
       CRCFCRC  =  scCfCrTemC;
       CRCFCRPC =  scCfCrPthC;
       CRRECR   =  scReCrTem;
       CRRECRP  =  scReCrPth;
       CRRECRC  =  scReCrTemC;
       CRRECRPC =  scReCrPthC;

       CRPgm  = Q1PGM;
       CRUser = Q1USER;
       CRTime = Timeto6(SystemTime());
       CRDt   = Dateto7(SystemDate());

       If xxRRN <> 0 and inStateMode <> 'C';
          Update pcr0p;
       Else;
          Write pcr0p;
       Endif;

       Endsr;
       //===================================================================
       // Set the default template fields for state.
       //===================================================================
       Begsr StateDefaults;

       scCXCrTem = 'CX Cert Letter.scd';
       scCXCrPth = '/Compliance/Base/Policy/';

       scCFCrTem = 'CX Confirmation Cert Letter.scd';
       scCFCrPth = '/Compliance/Base/Policy/';

       scRECrTem = 'Rein Cert Letter.scd';
       scRECrPth = '/Compliance/Base/Policy/';

       Select;
       When scStState = 'FL';
          scStCXTem = 'FL Cancel Notice.scd';
          scStCXPth = '/Compliance/Base/Policy/';
          scStRETem = 'FL Reinstate Notice.scd';
          scStREPth = '/Compliance/Base/Policy/';
       //*** begin add ***  03/22/17  *****************************************
       When scStState = 'GA';
          scStCXTem = 'Cancel Notice.scd';
          scStCXPth = '/Compliance/Base/Policy/';
          scStRETem = 'GA Reinstate Notice.scd';
          scStREPth = '/Compliance/Base/Policy/';
       //*** end add *****  03/22/17  *****************************************
       When scStState = 'MA';
          scStCXTem = 'MA Cancel Notice.scd';
          scStCXPth = '/Compliance/Base/Policy/';
          scStRETem = 'MA Reinstate Notice.scd';
          scStREPth = '/Compliance/Base/Policy/';
       //*** Begin Add ***  05/10/23  *****************************************
       When scStState = 'NJ';
          scStCXTem = 'NJ Cancel Notice.scd';
          scStCXPth = '/Compliance/Base/Policy/';
          scStRETem = 'NJ Reinstate Notice.scd';
          scStREPth = '/Compliance/Base/Policy/';
       //*** End   Add ***  05/10/23  *****************************************
       When scStState = 'NY';
          scStCXTem = 'NY Cancel Notice.scd';
          scStCXPth = '/Compliance/Base/Policy/';
          scStRETem = 'NY Reinstate Notice.scd';
          scStREPth = '/Compliance/Base/Policy/';
       When scStState = 'VA';
          scStCXTem = 'VA Cancel Notice.scd';
          scStCXPth = '/Compliance/Base/Policy/';
          scStRETem = 'VA Reinstate Notice.scd';
          scStREPth = '/Compliance/Base/Policy/';
       When scStState = 'WI';
          scStCXTem = 'WI Cancel Notice.scd';
          scStCXPth = '/Compliance/Base/Policy/';
          scStRETem = 'WI Reinstate Notice.scd';
          scStREPth = '/Compliance/Base/Policy/';
       Other;
          scStCXTem = 'Cancel Notice.scd';
          scStCXPth = '/Compliance/Base/Policy/';
          scStRETem = 'Reinstate Notice.scd';
          scStREPth = '/Compliance/Base/Policy/';
       Endsl;

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
