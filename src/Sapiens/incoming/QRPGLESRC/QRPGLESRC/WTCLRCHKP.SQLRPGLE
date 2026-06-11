     H Copyright('(c) 2024 Sapiens. All Rights Reserved.')
     H Option(*NoDebugio:*SrcStmt)
     H DftActGrp(*No) ActGrp(*Caller)
     H/Title WTCLRCHKP - Cleared Check Import Prompt
     H Bnddir('WTBNDDIR')
     //===============================================================*
     // WTCLRCHKP                                                     *
     //                                                               *
     //   This program processes a bank file and update the check     *
     //   history file with the Check Cashed date provided.           *
     //                                                               *
     // Created   06/11/24  C:340 Project: 152WCB                     *
     //===============================================================*
     // REVISIONS:                                                    *
     //                                                               *
     //===============================================================*
     // Notes:                                                        *
     //                                                               *
     // # Load the file from the IFS.                                 *
     // # Run edits interactively and display in a subfile grid.      *
     // # Submit a job to import the data when no edits are found     *
     //   in the import document.                                     *
     // # Process the file and update the payment history file with   *
     //   the Cashed Date.                                            *
     // # Display the date on the Check Detail.                       *
     //                                                               *
     //===============================================================*
     H*===============================================================
     H* REVISIONS
     H*
     H*
     H*
     H*
     H*===============================================================
     fWTCLRCHKP cf   e             workstn extfile('WTCLRCHKP')
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     sfile(sflClCkErr:rrnClCkErr)
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)

    d*===================================================================
    d* Prototypes
    d*===================================================================
     *Entry
     d ClrChkImport    pr                  Extpgm('WTCLRCHKP')
     d ClrChkImport    pi

     d ChkDlo          pr                  extPgm('CHKIFSOBJ')
     d  docName                     256    const
     d  error                          n

     d RtvCltId        pr                  ExtPgm('RTVCLTID')
     d  strCltId                      3

     d RtvDocRoot      pr                  ExtPgm('RTVDOCROOT')
     d   root                        10

     d RtvSuffix       pr                  ExtPgm('SA030R')
     d   suffix                       3

     d SubmitImport    pr                  ExtPgm('WTCLRCHKC')
     d   path                       257

     d/copy sCopyPr01d
     d/copy sPrCompany
     d/copy sPrDate
     d/copy sPrGenPop
     d/copy sPrLetter
     d/copy sPrMessage
     d/copy sPrRunCmd
     d/copy sPrSQL
     d/copy sPrXML
     d/copy wPrEmplr
     d/copy wPrUsrPrms

    d*===================================================================
    d* Work Fields
    d*===================================================================
     d arcLibrary      s             10a
     d chrChk#         s              7a
     d chrInv#         s              7a
     d chrFEIN         s              9a
     d chrAmt          s              9a
     d errorFound      s               n
     d fullPathName    s            256a   Varying
     d importRow       s              9s 0 Inz(1)
     d inPathName      s            257a
     d intCo#          s              3s 0
     d intChk#         s              7s 0
     d intInv#         s              7s 0
     d intFEIN         s              9s 0
     d intAmt          s              9s 2
     d intCase         s              7s 0 Inz
     d intFnd          s              3s 0 Inz
     d intFyr          s              3s 0 Inz
     d intPos          s              2s 0 Inz
     d isErrorClrChk   s              1n
     d length          s              3s 0
     d loopSqlStt      s                   Like(SqlStt)
     d nullInds        s              5i 0 dim(10)
     d strCltId        s              3
     d strReturn       s             10
     d strRunCmd       s            500
     d strSql          s           2700a   inz
     d suffix          s              3
     d tempLibrary     s             10a
     d strConfirmMsg   s             80
    d*===================================================================
    d* Data Structures
    d*===================================================================
     d dsMsgInfo       ds                  Likeds(dsRtnMsgInfo)
     d dsInMsgInfo     ds                  Likeds(dsRtvMsgInfo)

     d dsClrChkArap    ds                  Inz  Qualified
     d  Chk#                         07a
     d  Inv#                         07a
     d  Cashed                       10a
     d  ChkDt                        10a
     d  Claim#                       15a
     d  Loc                          50a
     d  Name                         45a
     d  DOI                          10a
     d  Payee                        40a
     d  FEIN                         09a
     d  SvFr                         10a
     d  SvTo                         10a
     d  Amt                          09a
     d  PayCd                        01a
     d  RsvType                      01a

     d dsClaim#        ds                  Inz  Qualified
     d  ClmFnd                 1      3
     d  ClmFyr                 5      7
     d  ClmCas                 9     15

     d dsWDELP       e ds                  extName(WDELP)
     d dsWDRRP       e ds                  extName(WDRRP)

     d pathNameDta     ds                  dtaara('WTCLRCHKP')
     d  lastPathName           1    256

    d*===================================================================
    d* Main
    d*===================================================================
      /Free

       ProgramStart();
       MainProcess();
       *InLr = *on;

      /End-Free

      //===================================================================
      // ClearError - Clear Error/Message fields
      //===================================================================
     p ClearError      b
     d ClearError      pi

      /Free
        msgPathNam = ' ';
        errPathNam = *Off;
      /End-Free

     p ClearError      e

      //===================================================================
      //  ClrClCkError  - Clear the Experience Mod Error Subfile
      //===================================================================
     p ClrClCkError    b
     d ClrClCkError    pi

      /Free
        // Clear the subfile
        rrnClCkErr  = 0;
        dspClCkErr  = *off;
        clrClCkErr  = *on;
        Write rcdClCkImp;
        clrClCkErr  = *off;
        dspClCkErr  = *On;
      /End-Free

     p ClrClCkError    e

      //===================================================================
      // CrtTblExpMod - Create the Experience Mod Table using SQL
      //===================================================================
     p CrtTblClrChk    b
     d CrtTblClrChk    pi

      /Free
       Exec SQL create table QTEMP/ClrChk
         (CHK#     CHAR (07) not null,
          INV#     CHAR (07) not null,
          CASHED   CHAR (10) not null,
          CHKDT    CHAR (10) not null,
          CLAIM#   CHAR (15) not null,
          LOC      CHAR (50) not null,
          NAME     CHAR (45) not null,
          DOI      CHAR (10) not null,
          PAYEE    CHAR (40) not null,
          FEIN     CHAR (09) not null,
          SVFR     CHAR (10) not null,
          SVTO     CHAR (10) not null,
          AMT      CHAR (09) not null,
          PAYCD    CHAR (01) not null,
          RSVTYPE  CHAR (01) not null);
      /End-Free

     p CrtTblClrChk    e

       //==================================================================
       // EdtTblClrChk - Edit the table ClrChk
       //==================================================================
     p EdtTblClrChk    b
     d EdtTblClrChk    pi
     d   inMode                       1    Const

     d badData         s               n   Inz(*Off)
     d header          s              1    Inz
     d rateState       s              2    Inz
     d rcdCount        s              9  0 Inz
     d trailer         s              1    Inz

      /Free

       errorFound = *Off;

       Select;
       When inMode = '1'; // File edits
          // File must have records
          Exec Sql
             Select Count(*)
             Into :rcdCount
             From ClrChk ;
          If rcdCount = 0;
             LoadImpErrGrd('No records found.':0);
          EndIf;

       When inMode = '2'; // Record Edits
          // Increment the row #
          importRow += 1;
          badData    = *Off;

          // Does Check# field have valid numeric data?
          Monitor;
             chrChk# = dsClrchkArap.Chk#;
             If dsClrchkArap.Chk# = ' ';
                intChk# = 0;
             Else;
                intChk# = %Dec(%Trim(dsClrchkArap.Chk#):7:0);
             EndIf;
          On-Error;
             badData = *On;
             LoadImpErrGrd('Non-numeric data found in Check # field.':
                           importRow);
          EndMon;

          // Does Invoice# field have valid numeric data?
          Monitor;
             chrInv# = dsClrchkArap.Inv#;
             If dsClrchkArap.Inv# = ' ';
                intInv# = 0;
             Else;
                intInv# = %Dec(%Trim(dsClrchkArap.Inv#):7:0);
             EndIf;
          On-Error;
             badData = *On;
             LoadImpErrGrd('Non-numeric data found in Invoice # field.':
                           importRow);
          EndMon;

          // Does Amount field have valid numeric data?
          Monitor;
             If dsClrchkArap.Amt <> ' ';
                intPos = %Scan(',':dsClrchkArap.Amt);
                Dow intPos <> 0;
                    dsClrchkArap.Amt = %replace('':dsClrchkArap.Amt:intPos:1);
                    intPos = %Scan(',':dsClrchkArap.Amt);
                EndDo;
                intPos = %Scan('$':dsClrchkArap.Amt);
                Dow intPos <> 0;
                    dsClrchkArap.Amt = %replace('':dsClrchkArap.Amt:intPos:1);
                    intPos = %Scan('$':dsClrchkArap.Amt);
                EndDo;
                If dsClrchkArap.Amt <> ' ';
                   intAmt = %Dec(dsClrchkArap.Amt:9:2);
                EndIf;
             Else;
                intAmt = 0;
             EndIf;
          On-Error;
             badData = *On;
             LoadImpErrGrd('Non-numeric data found in Amount field.':
                           importRow);
          EndMon;

          // Does Claim# field have valid numeric data?
          dsClaim# = dsClrchkArap.Claim#;
          Monitor;

             If dsClaim#.ClmFnd <> ' ';
                intFnd = %Dec(%Trim(dsClaim#.ClmFnd):3:0);
             Else;
                intFnd = 0;
             EndIf;

             If dsClaim#.ClmFyr <> ' ';
                intFyr = %Dec(%Trim(dsClaim#.ClmFyr):3:0);
             Else;
                intFyr = 0;
             EndIf;

             If dsClaim#.ClmCas <> ' ';
                intCase = %Dec(%Trim(dsClaim#.ClmCas):7:0);
             Else;
                intCase = 0;
             EndIf;

          On-Error;
             badData = *On;
             LoadImpErrGrd('Non-numeric data found in Claim # field.':
                           importRow);
          EndMon;

          // No need to check further if there is bad data
          If badData;
             UpdateClrChkErr(importRow);
             Return;
          EndIf;

       EndSl;

       // Update record if error found
       If errorFound;
          UpdateClrChkErr(importRow);
       EndIf;

      /End-Free

     p EdtTblClrChk    e

      //===================================================================
      // ExfmtScreens- display screen
      //===================================================================
     p ExfmtScreens    b
     d ExfmtScreens    pi

      /Free
       Select;
       When TabIndex = 0;
          TabHeight = 620;
          APnlHeight = TabHeight + 60;
          If isErrorClrChk;
             isImpError = '1';
          EndIf;
          If scPathName = *blanks;
             In pathNameDta;
             scPathName = lastPathName;
          Endif;
          Write rcdComPnl;
          Exfmt RcdClCkImp;
          Read rcdComPnl;
       EndSl;
      /End-Free

     p ExfmtScreens    e

      //===================================================================
     *// ImpFiletoTable - Import the Experience Mod File to table EXPMODS
      //===================================================================
     p ImpFileToTable  b
     d ImpFileToTable  pi              n

      /Free
        strRunCmd = 'CPYFRMIMPF FROMSTMF(';
        strRunCmd = %Trim(strRunCmd) + '''' + %Trim(scPathName) + '''' + ')';
        strRunCmd = %Trim(strRunCmd) + ' TOFILE(QTEMP/CLRCHK)';
        strRunCmd = %Trim(strRunCmd) + ' MBROPT(*REPLACE)';
        strRunCmd = %Trim(strRunCmd) + ' RCDDLM(*CRLF)';
        strRunCmd = %Trim(strRunCmd) + ' STRDLM(*DBLQUOTE)';
        strRunCmd = %Trim(strRunCmd) + ' RMVBLANK(*BOTH)';
        strRunCmd = %Trim(strRunCmd) + ' ERRRCDOPT(*REPLACE)';
        strRunCmd = %Trim(strRunCmd) + ' FROMRCD(2)';
        strRunCmd = %Trim(strRunCmd) + ' RPLNULLVAL(*FLDDFT)';

        Monitor;
           RunCmd(strRunCmd:500);
        On-Error;
           dsInMsgInfo = dsRtvMsgInfo;
           dsInMsgInfo.type = '*LAST';
           dsMsgInfo = RtvMsg('*':dsInMsgInfo);
           dsInMsgInfo.type = '*PRV';

           If dsMsgInfo.MsgID <> *blanks;
             Return *Off;
           EndIf;
        Endmon;

        Return *On;

      /End-Free

     p ImpFileToTable  e

      //===================================================================
      // Initialize - Initialization procedure
      //===================================================================
     p Initialize      b
     d Initialize      pi

      /Free
      /End-Free

     p Initialize      e

      //===================================================================
      // LoadImpErrGrd - Load the Experience Mod Import Error Grid
      //===================================================================
     p LoadImpErrGrd   b
     d LoadImpErrGrd   pi
     d   inError                    110a   Const
     d   inRow                        9  0 Const

      /Free

        If inRow <> 0;
           errorFound    = *On;
        EndIf;

        isErrorClrChk = *On;
        scChk#        = dsClrchkArap.Chk#;
        scInv#        = dsClrchkArap.Inv#;
        scCashed      = dsClrchkArap.Cashed;
        scChkDt       = dsClrchkArap.ChkDt;
        scClaim#      = dsClrchkArap.Claim#;
        scLoc         = dsClrchkArap.Loc;
        scName        = dsClrchkArap.Name;
        scRow         = inRow;
        scErr         = inError;
        rrnClCkErr   += 1;
        outClCkRRN    = rrnClCkErr;
        Write sflClCkErr;

      /End-Free

     p LoadImpErrGrd   e

      //===================================================================
      // ProcTblExpMod - Read the table ClrChk and call edit routine
      //===================================================================
     p ProcTblClrChk   b
     d ProcTblClrChk   pi
      /Free

        // Initialize
        importRow     = 1;
        isImpError    = *Off;
        isErrorClrChk = *Off;

        // Process file level errors
        EdtTblClrChk('1');

        Exec Sql Declare C1 Cursor for
          Select * from QTEMP/ClrChk;

        Exec Sql Open C1;
        // Set Subfile indicators
        // Fetch records from Clear Check file and check for error conditions
        Reset dsClrchkArap;
        loopSqlStt = SqlStt;
        Dou Not IsValidFetch(LoopSqlStt);
           Exec SQL
           Fetch Next From C1 into :dsClrchkArap :NullInds;
           loopSqlStt = SqlStt;
           If isValidFetch(loopSqlStt);
              // Perform edits
              EdtTblClrChk('2');
           // Exit If Max Error records are 9999
              If outClCkRRN >= 9999;
                 Leave;
              Endif;
           // Invalid Fetch.  Leave the loop.
           Else;
              Leave;
           Endif;
           Reset dsClrchkArap;
        Enddo;

        Exec Sql Close C1;

      /End-Free

     p ProcTblClrChk   e

      //===================================================================
      // MainProcess - Main process procedure
      //===================================================================
     p MainProcess     b
     d MainProcess     pi

      /Free

      // Process until exit button is pressed.
       Dou ExitNow = *On;
          // Reset Import Error Grid
          ClrClCkError();
          // Initially the error grid is hidden
          If BtnSubmit <> *on;
             ExfmtScreens();
          Endif;
          Select;
          When BtnSubmit = *On ;
             BtnSubmit = *Off;
             ClearError();
            // Validate the screen (folder path and document name exists)
             If Validate();
                // If the file imported succsessfully then read through the table and
                // Reset Import Error Grid
                ClrClCkError();
                // perform edits on the field values and display the grid if there are errors
                ProcTblClrChk();
                If isErrorClrChk;
                   isImpError = *On;
                   ExfmtScreens();
                   Dou ExitNow = *On;
                      Select;
                      When BtnSubmit = *On;
                         ClearError();
                         Leave;
                      When BtnCancel = *On;
                         Leave;
                      Endsl;
                   Enddo;
                   If BtnSubmit = *On;
                      Iter;
                   Else;
                     Leave;
                   Endif;
                Else;
                   // Submit the import job
                   strConFirmMsg = 'You are uploading Clear Check Recon File.';
                   If PopTwoBtn(strConfirmMsg:'Confirmation':
                    'Cancel':'Confirm') = *Off;
                      inPathName = scPathName + '$';
                      SubmitImport(inPathName);
                      PopOneBtn('Job Check Cased Date Submitted -
                      successfully':'Info');
                      Leave;
                   Endif;
                Endif;
             EndIf;
          When BtnCancel = *On;
             BtnCancel = *Off;
             Leave;
          When BtnRefresh= *On;
             BtnRefresh= *Off;
             Initialize();
             ClearError();
          EndSl;

       EndDo;
      /End-Free

     p MainProcess     e

      //===================================================================
      // ProgramEnd - End Program
      //===================================================================
     p ProgramEnd      b
     d ProgramEnd      pi
      /Free
        *inlr = *on;
        Return;
      /End-free
     p ProgramEnd      e

      //===================================================================
      // ProgramStart - Initial Program
      //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /Free
        // Exec Sql Set Option commit = *None;
        Initialize();
        scCompName = GetCompNam(userCo#(q1user));
        intCo# = UserCo#(Q1User);
        strReturn = 'ONDEMAND  ';
        // Retrieve library suffix and build archive library name
        RtvSuffix(suffix);
        If suffix = *blanks;
           TempLibrary = 'PDSTEMP';
           ArcLibrary  = 'PDSFILARC';
        Else;
           ArcLibrary  = 'PDSFIL' + %Trim(suffix) + 'A';
           TempLibrary = 'PDSTEMP' + %Trim(suffix);
        Endif;
        // Create Data Area to store folder path
        strRunCmd = 'CRTDTAARA DTAARA(PDSOBJ';
        strRunCmd = %Trim(strRunCmd) + %Trim(Suffix) + '/';
        strRunCmd = %Trim(strRunCmd) + 'WTCLRCHKP)';
        strRunCmd = %Trim(strRunCmd) + ' ' + 'TYPE(*CHAR) LEN(256)';
        strRunCmd = %Trim(strRunCmd) + ' ' + 'TEXT(''Clear Check Path'') ';
        Monitor;
           RunCmd(strRunCmd:98);
        On-Error;
        Endmon;

        //Retrieve Client-ID
        RtvCltID(strCltId);

      /End-Free

     p ProgramStart    e

      //===================================================================
      //  UpdateClrChkErr - Update workfile to indicate error found
      //===================================================================
     p UpdateClrChkErr...
     p                 b
     d UpdateClrChkErr...
     d                 pi
     d inRow                          9s 0

      /Free

       Exec Sql
          Update qtemp/ClrChk
          set error = 'Y'
          Where rrn(ClrChk) = :inRow;

      /End-Free

     p UpdateClrChkErr...
     p                 e

      //===================================================================
      //  UpdDataArea - Update Data Area with Path Name selected
      //===================================================================
     p UpdDataArea     b
     d UpdDataArea     pi

      /Free
        strRunCmd = 'CHGDTAARA DTAARA(';
        strRunCmd = %Trim(strRunCmd) + 'WTCLRCHKP';
        strRunCmd = %Trim(strRunCmd) + ' ' + '(1 256))';
        strRunCmd = %Trim(strRunCmd) + ' ' + 'VALUE(''';
        strRunCmd = %Trim(strRunCmd) + %Trim(scPathName) + '''' + ')';
        Monitor;
           RunCmd(strRunCmd:500);
        On-Error;
        Endmon;
      /End-Free

     p UpdDataArea     e

      //===================================================================
      // Validate - Validate screen fields
      //===================================================================
     p Validate        b
     d Validate        pi              n
     d docExists       s               n
     d Lo              c                   const('abcdefghijklmnopqrstuvwxyz')
     d root            s             10
     d rootPlus        s             20
     d sFldPos         s              2  0
     d suffix          s              3
     d Up              c                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
     d x               s              3  0 inz(1)
     d y               s              3  0
     d intCharPos      s              3  0

      /Free
       isErrorClrChk = *Off;
       isImpError    = *Off;

       // Must not be blank
       If scPathName = ' ';
          errPathNam = *On;
          msgPathNam = 'The path name cannot be blank.';
          Return *Off;
       Endif;

       // Must start with a '/'
       If %subst(scPathName:1:1) <> '/';
          scPathName = '/' + scPathName;
       Endif;

       fullPathName = %Xlate(lo:up:scPathName);
       // Must be a .CSV file
       length = %Len(%Trim(fullPathName));
       If length < 4;
          errPathNam = *On;
          msgPathNam = 'The file extension must be .CSV.';
          Return *Off;
       Else;
          If %Subst(fullPathName:length-3:4) <> '.CSV';
             errPathNam = *On;
             msgPathNam = 'The file extension must be .CSV.';
             Return *Off;
          Endif;
       Endif;

       // Make sure doc exists (also checks authority);
       ChkDlo(fullPathName:docExists);
       If Not(docExists);
          errPathNam = *On;
          msgPathNam = 'Document does not exist or authority is restricted.';
          Return *Off;
       Endif;

       // Update the data area with the folder path selected
       UpdDataArea();
       // Create temporary SQL table called Factors in QTEMP librar         y
       CrtTblClrChk();

       // Did the data copy?
       If Not ImpFiletoTable();
          errPathNam = *On;

          Select;
          When dsMsgInfo.msgId = 'CPF2817';
             msgPathNam = 'Upload failed. CSV possibly open in another ' +
                          'application.';
          Other;
             msgPathNam = dsMsgInfo.MsgText;
          EndSl;

          Return *Off;
       Endif;

       Return *On;
      /End-Free

     P Validate        e
