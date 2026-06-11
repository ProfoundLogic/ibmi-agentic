     H Copyright('(c) 2024 Sapiens. All Rights Reserved.')
     H**********************************************************************
     H/TITLE WSDOCBILL - Scriptura Bill Document Setup
     H DFTNAME(WSDOCBILL)
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO) ACTGRP(*caller)
     H/DEFINE   PROFOUNDUI
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
    *===============================================================*
    *  Created    : 04/28/22                                        *
    *  Programmer : C:291                                           *
    *  Project    : WC2670                                          *
    *  Description: This is the new scriptura document setup scren  *
    *               program for Bills.                              *
    *               It is accessed as an additional setup screen    *
    *               from the Output Control tab of Application      *
    *               Configuration.                                  *
    *                                                               *
    *===============================================================*
    *  Revisions:                                                   *
      *                                                               *
      *                                                               *
    * ------------------------------------------------------------- *
    *  NOTE: Please add comments to the top of Revisions.           *
    * ------------------------------------------------------------- *
    *===============================================================*

     fwsdocbill cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflBillTmp:rrnBill)

     FWMUPL     IF   E           K Disk    Rename(PUP01:PUP00)
     FWSBL1L    IF   E           K Disk    Rename(PBL01:PBL00)
     F                                     infds(dbfds)
     FWSBLP1    UF A E             Disk    Rename(PBL01:PBL0P)
     FWSBLL     UF A E           K Disk
     FSMORL     IF   E           K DISK

     D PSDS          ESDS                  Extname(SZQ1P)

     d dbfds           ds
     d  dbrrn                397    400b 0

     D process         s               n   inz(*off)
     D clientID        s              3    inz('000')
     d xxCo#           s              3  0 inz(0)
     d xxBlEffDt       s              7  0 inz(0)

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
     D inBillMode                     1    const
     D inBillRRN                      9  0 const options(*nopass)

     d/copy sprDate
     d/copy sprGenPop
     d/copy sprGenErr
     d/copy wPrUsrPrms

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
       //===================================================================
       // Main - Main processing
       //===================================================================
     P Main            B
     D Main            PI
     d errorM          s               n   inz(*off)
     d ExitNow         s               n   inz(*off)
      /free

       Clear btnSave;
       Exsr LoadRecords;

       //*  Process until exit button is pressed.
       DoU ExitNow = *On;
          //*  Process screen.
          Exfmt rcdBlSetup;
          //Perform actions on screen.
          Select;
          //* Exit the program
          When btnCancel = *on;
             ExitNow = *On;
          //* Add a new state setup record.
          When btnBlAdd = *on;
             btnBlAdd = *off;
             Exsr AddBillRcd;
             Exsr LoadRecords;
          When btnSave = *on;
             Exsr ErrChkBillDoc;
             If errorM = *Off;
                Exsr SaveBillDoc;
                ExitNow = *On;
             Endif;
          Other;
             //*  Check for clicked links on grid.
             Readc sflBillTmp;
             If not %EOF();
                //*  Edit the state setup record.
                If billEdit = *on;
                   billEdit = *off;
                   Exsr EditBillRcd;
                Endif;
                //*  Delete the state setup record.
                If billDelt = *on;
                   billDelt = *off;
                   Exsr DeleteBillRcd;
                Endif;
                //*  Copy the state setup record.
                If billCopy = *on;
                   billCopy = *off;
                   Exsr CopyBillRcd;
                Endif;
                Update sflBillTmp;
                Exsr LoadRecords;
             Endif;
          Endsl;
       Enddo;

       //***************************************************************************//
       //===================================================================
       // Add State Setup Record
       //===================================================================
       Begsr AddBillRcd;
       SetupWindow('A');
       Endsr;

       //===================================================================
       // Copy State Setup Record
       //===================================================================
       Begsr CopyBillRcd;
       SetupWindow('C':outBlRRN);
       Endsr;

       //===================================================================
       // Delete State Setup Record
       //===================================================================
       Begsr DeleteBillRcd;
       SetupWindow('D':outBlRRN);
       Endsr;

       //===================================================================
       // Edit State Setup Record
       //===================================================================
       Begsr EditBillRcd;
       SetupWindow('E':outBlRRN);
       Endsr;

       //===================================================================
       // Save Bill Document Setup record
       //===================================================================
       Begsr ErrChkBillDoc;
        errorM = *Off;
        Clear msMrgPrmNt;
        Clear erMrgPrmNt;
        If scMrgPrmNt <> *Blank and
         (scCmbQteBl = *Blank and scCmbEstBl = *Blank and
         scCmbRevBl = *Blank and scCmbRevBl = *Blank);
           msMrgPrmNt = 'This value can only be chosen when ' +
            'Bills are being combined.';
           erMrgPrmNt = *on;
           errorM = *on;
        Endif;
       Endsr;

       //===================================================================
       // Load State Setup Grid
       //===================================================================
       Begsr LoadRecords;

       // Load Bill Document setup fields
       Chain(n) (xxCo#) PBL01;
       scCmbQteBl = BLCOMBQ;
       scCmbEstBl = BLCOMBE;
       scCmbRevBl = BLCOMBR;
       scCmbAudBl = BLCOMBA;
       scAutQteBl = BLPRMNQ;
       scAutEstBl = BLPRMNE;
       scAutRevBl = BLPRMNR;
       scAutAudBl = BLPRMNA;
       scMrgPrmNt = BLMRGPRN;
       If BLMRGPRN = ' ';
          scMrgPrmNt = 'N';
       Endif;

       // Load the subfile.
       dspBill = *off;
       dspBillCtl = *off;
       clrBill= *on;
       Write rcdBlSetup;
       clrBill = *off;
       dspBill = *on;
       dspBillCtl = *on;
       rrnBill = 0;

       Setll keyWSCIL pbl00;
       Reade keyWSCIL pbl00;

       Dow not %eof;
          If BLFnd = 0;
             outBlGrp = 'All';
          Else;
             outBlGrp = %editc(BLFnd : 'X');
          Endif;
          outBlState = BLSt;
          outBlEffDt = dtMDYY(BLEFDT);
          outBlType = BLTYPE;
          outBlTmp  = %trim(BLPATH) + %trim(BLDOC);
          outBlTmpC = %trim(BLPATHC) + %trim(BLDOCC);
          outBlTmpS  = %trim(BLSUMP) + %trim(BLSUMD);
          outBlTmpCS = %trim(BLSUMPC) + %trim(BLSUMDC);
          Clear outBlTmpN;
          Clear outBlTmpCN;
          Clear outBlTmpNH;
          Clear outBlTmpCH;
          If BLType = 'Q';
             outBlTmpN  = %trim(BLNOTEP) + %trim(BLNOTED);
             outBlTmpCN  = %trim(BLNOTEPC) + %trim(BLNOTEDC);
             outBlTmpNH = 'Notice of Election of Retrospective Rating ' +
              'Default Template';
             outBlTmpCH = 'Notice of Election of Retrospective Rating ' +
              'Client Template';
          Endif;
          Select;
          When BLTYPE = 'Q';
             outBlType = 'Quote';
          When BLTYPE = 'E';
             outBlType = 'Estimated';
          When BLTYPE = 'R';
             outBlType = 'Revised';
          When BLTYPE = 'A';
             outBlType = 'Audit';
          Endsl;
          If BLEFDT = 990101 and BLFND = 0 and BLST = *Blank;
             visDltBil = *Off;
          Else;
             visDltBil = *On;
          Endif;
          outBlRRN   = dbrrn;
          rrnBill =  rrnBill + 1;
          Write sflBillTmp;
          Reade keyWSCIL pbl00;
       Enddo;

       rrnBill = 1;

       Endsr;

       //===================================================================
       // Save Bill Document Setup record
       //===================================================================
       Begsr SaveBillDoc;
        Chain (xxCo#) PBL01;
        If %Found(WSBLL);
           BLCOMBQ = scCmbQteBl;
           BLCOMBE = scCmbEstBl;
           BLCOMBR = scCmbRevBl;
           BLCOMBA = scCmbAudBl;
           BLPRMNQ = scAutQteBl;
           BLPRMNE = scAutEstBl;
           BLPRMNR = scAutRevBl;
           BLPRMNA = scAutAudBl;
           BLMRGPRN = scMrgPrmNt;
           If scMrgPrmNt = 'N';
              BLMRGPRN = ' ';
           Endif;
           BLPgm  = Q1PGM;
           BLUser = Q1USER;
           BLTime = Timeto6(SystemTime());
           BLDt   = Dateto7(SystemDate());
           Update PBL01;
        Endif;
       Endsr;
      /end-free
     P Main            E

       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     P ProgramStart    B
     D ProgramStart    PI
     D processMe                       n
     D strPgmName      s             10
     D strFile#        s              1
      /free

       GetClientID(clientID);

       Chain(n) keyWMUPL PUP00;
       If not %found or UPCO# = *zero;
          processMe = *off;
          Return;
       Endif;

       xxCO# = UPCO#;

       //Write Bill Document setup record if No record exist in WSBLP for company being processed
       Chain(n) (xxCo#) PBL01;
       If not %Found(WSBLL);
          Clear PBL01;
          strPgmName = 'W7615R';
          strFile# = '6';
          Chain (strPgmName:strFile#) Por01;
          If %Found(SMORL) and ORstat <> 'I';
             BLCOMBQ = 'Y';
             BLCOMBE = 'Y';
             BLCOMBR = 'Y';
          Endif;
          strPgmName = 'W7700R';
          Chain (strPgmName:strFile#) Por01;
          If %Found(SMORL) and ORstat <> 'I';
             BLCOMBA = 'Y';
          Endif;
          BLco# = UPCO#;
          BLIPgm = Q1PGM;
          BLIUsr = Q1USER;
          BLITim = Timeto6(SystemTime());
          BLIDt  = Dateto7(SystemDate());
          BLPgm  = Q1PGM;
          BLUser = Q1USER;
          BLTime = Timeto6(SystemTime());
          BLDt   = Dateto7(SystemDate());
          Write PBL01;
       Endif;

       processMe = *on;
       Return;

      /end-free
     P ProgramStart    E

       //===================================================================
       // Add/Edit/Copy State Setup to file.
       //===================================================================
     p SetupWindow     b
     d SetupWindow     pi
     D inBillMode                     1    const
     D inBillRRN                      9  0 const options(*nopass)

     d xxRRN           s              9  0 inz(0)
     d error           s               n   inz(*off)
     d bilAddMode      s               n   inz(*off)
     d message         s            100
     d strMsg          s            200
      /free

       If %parms >= 2;
          xxRRN = inBillRRN;
       Endif;

       fndChoices = 'FNCO# = ' + %char(xxCO#);
       bilAddMode = *off;
       bilEdtMode = *off;
       dspBlAdd   = *off;

       If xxRRN <> 0;
          Chain(n) xxRRN pbl0p;
          If inBillMode <> 'C';
             bilEdtMode = *on;
          Endif;
       Else;
          dspBlAdd   = *on;
          bilAddMode = *on;
       Endif;

       If BLType = 'Q';
          visNtOfRet = *On;
       Else;
          visNtOfRet = *Off;
       Endif;

       //* Keep bringing up new popup windows in Add mode until cancel is hit.
       Dou bilAddMode = *off;

          //* Clear screen fields prior to display if in Add Mode.
          If bilAddMode = *on;
             Clear scBlGrp;
             Clear scBlState;
             Clear scBlEffDt;
             Clear scBillType;
             Clear scBlDoc;
             Clear scBlPath;
             Clear scBlDocC;
             Clear scBlPathC;
             Clear scBlDocS;
             Clear scBlPathS;
             Clear scBlDocSC;
             Clear scBlPathSC;
             Clear scBLNoteD;
             Clear scBLNoteP;
             Clear scBLNoteDC;
             Clear scBLNotePC;
             stCancLeft = 715;
          Else;
             scBlGrp = BLFND;
             scBlState = BLST;
             scBlEffDt = dtMDYY(BLEFDT);
             scBillType = BLTYPE;
             scBlDoc   = BLDOC;
             scBlPath  = BLPATH;
             scBlDocC  = BLDOCC;
             scBlPathC = BLPATHC;
             scBlDocS  = BLSUMD;
             scBlPathS = BLSUMP;
             scBlDocSC = BLSUMDC;
             scBlPathSC= BLSUMPC;
             scBLNoteD  = BLNOTED;
             scBLNoteP = BLNOTEP;
             scBLNoteDC = BLNOTEDC;
             scBLNotePC= BLNOTEPC;
             stCancLeft = 830;
          Endif;

          Exsr ClearErrors;
          Clear btnBlSave;
          Clear btnBlAdd;
          Clear btnBlCanc;

       //* Load the screen for display and then request confirmation of deletion.
       If inBillMode = 'D';
          Write rcdBillWin;
          strMsg = 'Do you want to continue with the deletion?';
          If not PopTwoBtn(strmsg:'Confirm Delete':'Cancel':
           'Continue':270:1120);
             Chain xxRRN pbl0p;
             Delete pbl0p;
          Endif;
          Return;
       Endif;

       //* Display popup window for the State Setup record.
          Dou btnBlAdd = *on or btnBlCanc = *on or btnBlSave = *on;
             Dou error = *off or btnBlCanc = *on;
                Exfmt rcdBillWin;
                Exsr ClearErrors;
                If btnBlCanc = *off;
                   Exsr SetupCheck;
                Endif;
             Enddo;
          Enddo;

          If btnBlSave = *on or btnBlAdd = *on;
             Exsr SetupSave;
          Endif;

          If btnBlCanc = *on or btnBlSave = *on;
             bilAddMode = *off;
          Endif;

       Enddo;

       //********************************************************************
       //===================================================================
       // Clear the error fields for window.
       //===================================================================
       BegSr ClearErrors;
       error = *off;
       Clear erBlGrp;
       Clear erBlState;
       Clear erBlEffDt;
       Clear erBillType;
       Clear erBlDoc;
       Clear erBlPath;
       Clear erBlDocC;
       Clear erBlPathC;
       Clear msBlGrp;
       Clear msBlState;
       Clear msBlEffDt;
       Clear msBillType;
       Clear msBlDoc;
       Clear msBlPath;
       Clear msBlDocC;
       Clear msBlPathC;
       Clear msBLNoteD;
       Clear msBLNoteP;
       Clear msBLNoteDC;
       Clear msBLNotePC;
       Endsr;

       //===================================================================
       // Edit the state setup fields.
       //===================================================================
       Begsr SetupCheck;

       // Effective date must be entered and valid.
       erBlEffDt = Valid_Date(scBlEffDt:msBlEffDt);
       If erBlEffDt = *on;
          msBlEffDt = 'Effective Date must be a valid date.';
          error = *on;
       Endif;

       //Type of Bill must be entered
       If scBillType = *Blank;
          msBillType = 'Type of Bill must be entered.';
          erBillType = *On;
          error = *On;
       Endif;

       xxBlEffDt = dtCYMD(scBlEffDt);
       // Check for state setup record already exists.
       If error = *off and (bilAddMode = *on or inBillMode = 'C');
          Chain(n) (xxCo#:scBillType:scBlGrp:scBlState:xxBlEffDt) pbl00;
          If %found;
             msBlGrp = 'A record already exists for this ' +
              'Bill Type/Group/State/Effective Date.';
             erBlGrp = *on;
             error = *on;
          Endif;
       Endif;

       If error = *on;
          LeaveSR;
       Endif;

       // Default template.
       If scBlDoc <> ' ';
          // Validate the default template name.
          erBlDoc = Valid_Document(xxCo#:'N':scBlDoc:message);
          If erBlDoc = *on;
             msBlDoc = message;
             error = *on;
          Endif;
          // Validate the default template path.
          erBlPath = Valid_Path(scBlPath:message);
          If erBlPath = *on;
             msBlPath = message;
             error = *on;
          Endif;
       Endif;

       // Client template.
       If scBlDocC <> ' ';
          // Validate the client template name.
          erBlDocC = Valid_Document(xxCo#:'N':scBlDocC:message);
          If erBlDocC = *on;
             msBlDocC = message;
             error = *on;
          Endif;
          // Validate the client template path.
          erBlPathC = Valid_Path(scBlPathC:message);
          If erBlPathC = *on;
             msBlPathC = message;
             error = *on;
          Endif;
       Endif;

       // Default template Summary.
       If scBlDocS <> ' ';
          // Validate the default template Summary name.
          erBlDocS = Valid_Document(xxCo#:'N':scBlDocS:message);
          If erBlDocS = *on;
             msBlDocS = message;
             error = *on;
          Endif;
          // Validate the default template Summary path.
          erBlPathS = Valid_Path(scBlPathS:message);
          If erBlPathS = *on;
             msBlPathS = message;
             error = *on;
          Endif;
       Endif;

       // Client template Summary.
       If scBlDocSC <> ' ';
          // Validate the client template Snummary ame.
          erBlDocSC = Valid_Document(xxCo#:'N':scBlDocSC:message);
          If erBlDocSC = *on;
             msBlDocSC = message;
             error = *on;
          Endif;
          // Validate the client template Spummary Path.
          erBlPathSC = Valid_Path(scBlPathSC:message);
          If erBlPathSC = *on;
             msBlPathSC = message;
             error = *on;
          Endif;
       Endif;

       // Default Notice of Retrospective template.
       If scBLNoteD <> ' ';
          // Validate the default Notice of Retrospective template name.
          erBLNoteD = Valid_Document(xxCo#:'N':scBLNoteD:message);
          If erBLNoteD = *on;
             msBLNoteD = message;
             error = *on;
          Endif;
          // Validate the default Notice of Retrospective template path.
          erBLNoteP = Valid_Path(scBLNoteP:message);
          If erBLNoteP = *on;
             msBLNoteP = message;
             error = *on;
          Endif;
       Endif;

       // Client Notice of Retrospective template.
       If scBLNoteDC <> ' ';
          // Validate the client Notice of Retrospective template name.
          erBLNoteDC = Valid_Document(xxCo#:'N':scBLNoteDC:message);
          If erBLNoteDC = *on;
             msBLNoteDC = message;
             error = *on;
          Endif;
          // Validate the client Notice of Retrospective template path.
          erBLNotePC = Valid_Path(scBLNotePC:message);
          If erBLNotePC = *on;
             msBLNotePC = message;
             error = *on;
          Endif;
       Endif;


       Endsr;

       //===================================================================
       // Save the Billing Information setup record.
       //===================================================================
       Begsr SetupSave;

       If xxRRN <> 0 and inBillMode <> 'C';
          Chain xxRRN pbl0p;
       Else;
          Clear pbl0p;
          BLCo#  = xxCo#;
          BLFnd  = scBlGrp;
          BLSt   = scBlState;
          BLEfdt = dtCYMD(scBlEffDt);
          BLTYPE = scBillType;
          BLIPgm = Q1PGM;
          BLIUsr = Q1USER;
          BLITim = Timeto6(SystemTime());
          BLIDt  = Dateto7(SystemDate());
       Endif;

       BLDoc     = scBlDoc;
       BLPath    = scBlPath;
       BLDocC    = scBlDocC;
       BLPathC   = scBlPathC;
       BLSumD    = scBlDocS;
       BLSumP    = scBlPathS;
       BLSumDC   = scBlDocSC;
       BLSumPC   = scBlPathSC;
       BLNoteD     = scBLNoteD;
       BLNoteP    = scBLNoteP;
       BLNoteDC    = scBLNoteDC;
       BLNotePC   = scBLNotePC;

       BLPgm  = Q1PGM;
       BLUser = Q1USER;
       BLTime = Timeto6(SystemTime());
       BLDt   = Dateto7(SystemDate());

       If xxRRN <> 0 and inbILLMode <> 'C';
          Update pbl0p;
       Else;
          Write pbl0p;
       Endif;

       Endsr;

      /end-free

     p SetupWindow     e
