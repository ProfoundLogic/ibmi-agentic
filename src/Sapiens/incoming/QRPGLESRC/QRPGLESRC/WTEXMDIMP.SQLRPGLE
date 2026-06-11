     hCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H*Option(*NoDebugio:*SrcStmt)
     H DftActGrp(*No) ActGrp(*Caller)
     H/Title WTEXMDIMP - Experience Mod/ARAP Import - Prompt
     H Bnddir('WTBNDDIR')
     H*===============================================================
     H* Created on 04/04/22 by c:62 for Project 129WWZ
     H*===============================================================
     H* Revisions
     H*
     H*===============================================================
     fwtexmdimp cf   e             workstn extfile('WTEXMDIMP')
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     sfile(sflExMdErr:rrnExMdErr)
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)

    d*===================================================================
    d* Prototypes
    d*===================================================================
     *Entry
     d wtexmdimport    pr                  Extpgm('WTEXMDIMP')
     d wtexmdimport    pi

     d ChkDlo          pr                  extPgm('CHKIFSOBJ')
     d  docName                     256    const
     d  error                          n

     d RtvCltId        pr                  ExtPgm('RTVCLTID')
     d  strCltId                      3

     d RtvDocRoot      pr                  ExtPgm('RTVDOCROOT')
     d   root                        10

     d RtvSuffix       pr                  ExtPgm('SA030R')
     d   suffix                       3

     d SubmitImport    pr                  ExtPgm('WTEXMDIMC')
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
     d chrFnd          s              3a
     d chrEffDate      s              8a
     d chrEmp#         s              9a
     d chrDiv          s              5a
     d chrFyr          s              3a
     d charNum         s              1a
     d charRecNotCopy  s              7a
     d chr#OfRecs      s              9a
     d chrFormat       s              1a
     d errorFound      s               n
     d fullPathName    s            256a   Varying
     d importRow       s              9s 0 Inz(1)
     d inPathName      s            257a
     d int#OfRecs      s              9s 0
     d intARAP         s              5s 4
     d intCo#          s              3s 0
     d intCount        s              5s 0
     d intFnd          s              3s 0
     d intEmp#         s              9s 0
     d intExpCnt       s              5s 0
     d intExpMod       s              5s 4
     d intDiv          s              5s 0
     d intFyr          s              3s 0
     d intEffDate7     s              7s 0
     d intEffDate8     s              8s 0
     d isErrorExpMod   s              1n
     d length          s              3s 0
     d loopSqlStt      s                   Like(SqlStt)
     d msg2ndLevel     s           4096a
     d nullInds        s              5i 0 dim(10)
     d pattern         s             25a
     d popMsg          s            256
     d popTitle        s             50
     d result          s              3s 0
     d strCltId        s              3
     d strConfirmMsg   s             80
     d strReturn       s             10
     d strRunCmd       s            500
     d strSql          s           2700a   inz
     d suffix          s              3
     d tempLibrary     s             10a

    d*===================================================================
    d* Data Structures
    d*===================================================================
     d dsMsgInfo       ds                  Likeds(dsRtnMsgInfo)
     d dsInMsgInfo     ds                  Likeds(dsRtvMsgInfo)

     d dsExpModArap    ds                  Inz  Qualified
     d  format                        1a
     d  #ofRecs                       9a
     d  fnd                           3a
     d  emp#                          9a
     d  div                           5a
     d  year                          3a
     d  mod                           6a
     d  effDate                       8a
     d  arap                          6a
     d  notFinal                      1a
     d  filler                       59a

     d dsWDELP       e ds                  extName(WDELP)
     d dsWDRRP       e ds                  extName(WDRRP)

     d pathNameDta     ds                  dtaara('WTEXMDIMP')
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
      //  ClrExMdError - Clear the Experience Mod Error Subfile
      //===================================================================
     p ClrExMdError    b
     d ClrExMdError    pi

      /Free
        // Clear the subfile
        rrnExMdErr  = 0;
        dspExMdErr  = *off;
        clrExmdErr  = *on;
        Write rcdExMdImp;
        clrExMdErr  = *off;
        dspExMdErr  = *On;
      /End-Free

     p ClrExmdError    e

      //===================================================================
      // CrtTblExpMod - Create the Experience Mod Table using SQL
      //===================================================================
     p CrtTblExpMod    b
     d CrtTblExpMod    pi

      /Free
       Exec SQL create table QTEMP/EXPMODS
         (FORMAT   CHAR ( 1 ) not null,
          #OFRECS  CHAR ( 9 ) not null,
          FND      CHAR ( 3 ) not null,
          EMP#     CHAR ( 9 ) not null,
          DIV      CHAR ( 5 ) not null,
          YEAR     CHAR ( 3 ) not null,
          MOD      CHAR ( 6 ) not null,
          EFFDT    CHAR ( 8 ) not null,
          ARAP     CHAR ( 6 ) not null,
          NOTFINAL CHAR ( 1 ) not null,
          FILLER   CHAR ( 59 ) not null,
          ERROR    CHAR ( 1 ) not null);
      /End-Free

     p CrtTblExpMod    e

       //==================================================================
       // EdtTblExpMod - Edit the table ExpMods
       //==================================================================
     p EdtTblExpMod    b
     d EdtTblExpMod    pi
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
             From ExpMods;
          If rcdCount = 0;
             LoadImpErrGrd('No records found.':0);
          EndIf;

          // File must begin with a header record
          Exec Sql
             Select format, #ofrecs
             Into :chrFormat, :chr#OfRecs
             From ExpMods a
             Where rrn(a) = (Select min(rrn(ExpMods))
                             From ExpMods);
          If chrFormat <> 'H';
             LoadImpErrGrd('The first record must be a header record.':0);
          Else;
             Monitor;
                int#OfRecs = %Dec(%Trim(chr#OfRecs):9:0);
             On-Error;
                int#OfRecs = 0;
             EndMon;
             If int#OfRecs <> rcdCount;
                LoadImpErrGrd('Record count does not match.':1);
             EndIf;
          EndIf;

          // File must end with a trailer record
          Exec Sql
             Select format
             Into :trailer
             From ExpMods a
             Where rrn(a) = (Select max(rrn(ExpMods))
                             From ExpMods);
          If trailer <> 'T';
             LoadImpErrGrd('The last record must be a trailer record.':0);
          EndIf;

       When inMode = '2'; // Record Edits
          // Increment the row #
          importRow += 1;
          badData    = *Off;

          // No need to check errors if (H)eader or (T)trailer

          // Does group field have valid numeric data?
          Monitor;
             chrFnd = dsExpModArap.fnd;
             If dsExpModArap.fnd = ' ';
                intFnd = 0;
             Else;
                intFnd = %Dec(%Trim(dsExpModArap.fnd):3:0);
             EndIf;
          On-Error;
             badData = *On;
             LoadImpErrGrd('Non-numeric data found in group # field.':
                           importRow);
          EndMon;

          // Does employer# field have valid numeric data?
          Monitor;
             chrEmp# = dsExpModArap.emp#;
             If dsExpModArap.emp# = ' ';
                intEmp# = 0;
             Else;
                intEmp# = %Dec(%Trim(dsExpModArap.emp#):9:0);
             EndIf;
          On-Error;
             badData = *On;
             LoadImpErrGrd('Non-numeric data found in employer # field.':
                           importRow);
          EndMon;

          // Does division field have valid numeric data?
          Monitor;
             chrDiv = dsExpModArap.div;
             If dsExpModArap.div = ' ';
                intDiv = 0;
             Else;
                intDiv = %Dec(%Trim(dsExpModArap.div):5:0);
             EndIf;
          On-Error;
             badData = *On;
             LoadImpErrGrd('Non-numeric data found in division field.':
                           importRow);
          EndMon;

          // Does policy year have valid numeric data?
          Monitor;
             chrFyr = dsExpModArap.year;
             If dsExpModArap.year = ' ';
                intFyr = 0;
             Else;
                intFyr = %Dec(%Trim(dsExpModArap.year):3:0);
             EndIf;
          On-Error;
             badData = *On;
             LoadImpErrGrd('Non-numeric data found in policy year field.':
                           importRow);
          EndMon;

          // Does experience mod have valid numeric data?
          Monitor;
             If dsExpModArap.mod = ' ';
                intExpMod = 0;
             Else;
                intExpMod = %Dec(%Trim(dsExpModArap.mod):5:4);
             EndIf;
             If intExpMod < 0;
                LoadImpErrGrd('Experience mod cannot be negative.':
                              importRow);
             EndIf;
          On-Error;
             badData    = *On;
             LoadImpErrGrd('Non-numeric data found in experience mod field.':
                           importRow);
          EndMon;

          // Does effective date have valid numeric data?
          Monitor;
             chrEffDate = dsExpModArap.effDate;
             If dsExpModArap.effDate = ' ';
                intEffDate7 = 0;
                intEffDate8 = 0;
             Else;
                intEffDate8 = %Dec(%Trim(dsExpModArap.effDate):8:0);
             EndIf;
          On-Error;
             badData    = *On;
             LoadImpErrGrd('Non-numeric data found in effective date field.':
                           importRow);
          EndMon;

          // Does ARAP factor have valid numeric data?
          Monitor;
             If dsExpModArap.arap = ' ';
                intArap = 0;
             Else;
                intArap = %Dec(%Trim(dsExpModArap.arap):5:4);
             EndIf;
          On-Error;
             badData    = *On;
             LoadImpErrGrd('Non-numeric data found in ARAP factor field.':
                           importRow);
          EndMon;

          // Is the effective date a valid date?
          intEffDate7 = dtCYMD(intEffDate8);
          If intEffDate7 = 0;
             LoadImpErrGrd('Invalid effective date.':importRow);
          EndIf;

          // No need to check further if there is bad data
          If badData;
             UpdateExpModErr(importRow);
             Return;
          EndIf;

          // Not Final Flag must be 'Y' or blank.
          If dsExpModArap.notFinal <> 'Y' And dsExpModArap.notFinal <> ' ';
             LoadImpErrGrd('"Not Final flag" must be Y or Blank.':importRow);
          Endif;

          // Is this a valid policy?
          Clear dsWDELP;
          Exec Sql
             Select *
             Into :dsWDELP
             From WDELP
             Where ELco# = :intCo# And ELfnd = :intFnd And ELemp# = :intEmp# And
                   ELdiv = :intDiv And ELfyr = :intFyr;

          If Not isValidFetch(SqlStt);
             LoadImpErrGrd('Policy # does not exist.':importRow);
             UpdateExpModErr(importRow);
             Return;
          EndIf;

          // Cannot use the same date more than once per policy
          Exec Sql
             Select count(*)
             Into :intCount
             from ExpMods
             Where fnd = :chrFnd And emp# = :chrEmp# And div = :chrDiv And
                   year = :chrFyr And effDt = :chrEffDate;

          If intCount > 1;
             LoadImpErrGrd('This policy has duplicate experience mods by date.':
                           importRow);
             UpdateExpModErr(importRow);
          EndIf;

          // Not enough experience mod slots available
          // How many slots are available
          Select;
          When ELemd3 <> 0;
             intExpCnt = 0;
          When ELemd2 <> 0;
             intExpCnt = 1;
          When ELemd1 <> 0;
             intExpCnt = 2;
          Other;
             intExpCnt = 3;
          EndSl;

          // How many slots would be filled?
          Exec Sql
             Select count(*)
             Into :intCount
             from ExpMods
             Where fnd = :chrFnd And emp# = :chrEmp# And div = :chrDiv And
                   year = :chrFyr;

          // If the policy count exceeds the remaining slots then error.
          If intCount > intExpCnt;
             LoadImpErrGrd('Not enough experience mod slots.':importRow);
             UpdateExpModErr(importRow);
          EndIf;

          // Exp mod date cannot be prior or equal to an existing mod date.
          If (ELemd1 <> 0 And intEffDate7 <= ELemd1) Or
             (ELemd2 <> 0 And intEffDate7 <= ELemd2) Or
             (ELemd3 <> 0 And intEffDate7 <= ELemd3);
             LoadImpErrGrd('Exp Mod Date cannot be prior or equal to an ' +
                           'existing mod date.':importRow);
          EndIf;

          // 2nd and 3rd exp mod date cannot be imported with an effective
          // prior or equal to the policy effective date.
          If ELemd1 <> 0 and intEffDate7 > ELemd1 And intEffDate7 <= ELpfdt;
             LoadImpErrGrd('2nd and 3rd exp mod date cannot be imported ' +
                           'with an effective date prior or equal to the ' +
                           'policy effective date.':importRow);
          EndIf;

          // Exp mod date cannot be after policy expiration date.
          If intEffDate7 > ELptDt;
             LoadImpErrGrd('Exp mod date cannot be after the policy ' +
                           'expiration date.':importRow);
          EndIf;

          // No exp mod slot is available.
          If ELexm1 <> 0 And ELexm2 <> 0 And ELexm3 <> 0;
             LoadImpErrGrd('No exp mod slot is availalbe.':importRow);
          EndIf;

          // ARAP Factor is valid for a rating state of MA only.
          If intARAP <> 0;
             If GetRatingState(intCo#:intFnd:intEmp#:intDiv:intFyr) <> 'MA';
                LoadImpErrGrd('ARAP factor is valid for a rating state ' +
                              'of MA only':importRow);
             Else;
                // ARAP Factor must have a value between 1.00 and 1.49.
                If intARAP < 1 Or intARAP > 1.49;
                   LoadImpErrGrd('ARAP factor must have a value between '   +
                                 '1.00 And 1.49':importRow);
                Endif;

                // ARAP Factor cannot be applied when scheduled debit/credit
                If ELdbcr <> 0;
                   LoadImpErrGrd('ARAP factor cannot be applied when '      +
                        'scheduled debit/credit factors exists.':importRow);
                EndIf;
             EndIf;
          EndIf;
       EndSl;

       // Update record if error found
       If errorFound;
          UpdateExpModErr(importRow);
       EndIf;

      /End-Free

     p EdtTblExpMod    e

      //===================================================================
      // ExfmtScreens- display screen
      //===================================================================
     p ExfmtScreens    b
     d ExfmtScreens    pi

      /Free
       Select;
       When TabIndex = 0;
          TabHeight = 620;
          APnlHeight = TabHeight + 40;
          If isErrorExpMod;
             isImpError = '1';
          EndIf;
          If scPathName = *blanks;
             In pathNameDta;
             scPathName = lastPathName;
          Endif;
          Write rcdComPnl;
          Exfmt RcdExMdImp;
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
        strRunCmd = %Trim(strRunCmd) + ' TOFILE(QTEMP/EXPMODS)';
        strRunCmd = %Trim(strRunCmd) + ' MBROPT(*REPLACE)';
        strRunCmd = %Trim(strRunCmd) + ' RCDDLM(*CRLF)';
        strRunCmd = %Trim(strRunCmd) + ' FLDDLM(*TAB)';
        strRunCmd = %Trim(strRunCmd) + ' ERRRCDOPT(*REPLACE)';
        strRunCmd = %Trim(strRunCmd) + ' RPLNULLVAL(*FLDDFT)';

        Monitor;
           RunCmd(strRunCmd:500);
        On-Error;
           dsInMsgInfo = dsRtvMsgInfo;
           dsInMsgInfo.type = '*LAST';
           dsMsgInfo = RtvMsg('*':dsInMsgInfo:'ESCAPE');
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

        isErrorExpMod = *On;
        scFnd         = dsExpModArap.fnd;
        scEmp#        = dsExpModArap.emp#;
        scDiv         = dsExpModArap.div;
        scYear        = dsExpModArap.year;
        scExMd        = dsExpModArap.mod;
        scEffDt       = dsExpModArap.effDate;
        scArap        = dsExpModArap.arap;
        scNotFinal    = dsExpModArap.notFinal;
        scRow         = inRow;
        scErr         = inError;
        rrnExMdErr   += 1;
        outExMdRRN    = rrnExMdErr;
        Write sflExMdErr;

      /End-Free

     p LoadImpErrGrd   e

      //===================================================================
      // ProcTblExpMod - Read the table EXPMODS and call edit routine
      //===================================================================
     p ProcTblExpMod   b
     d ProcTblExpMod   pi
      /Free

        // Initialize
        importRow     = 1;
        isImpError    = *Off;
        isErrorExpMod = *Off;

        // Process file level errors
        EdtTblExpMod('1');

        Exec Sql Declare C1 Cursor for
          Select * from QTEMP/EXPMODS
          Where format = 'D';

        Exec Sql Open C1;
        // Set Subfile indicators
        // Fetch records from FACTORS file and check for error conditions
        Reset dsExpModArap;
        loopSqlStt = SqlStt;
        Dou Not IsValidFetch(LoopSqlStt);
           Exec SQL
           Fetch Next From C1 into :dsExpModArap :NullInds;
           loopSqlStt = SqlStt;
           If isValidFetch(loopSqlStt);
              // Perform edits
              EdtTblExpMod('2');
           // Invalid Fetch.  Leave the loop.
           Else;
              Leave;
           Endif;
           Reset dsExpModArap;
        Enddo;

        Exec Sql Close C1;

      /End-Free

     p ProcTblExpMod   e

      //===================================================================
      // MainProcess - Main process procedure
      //===================================================================
     p MainProcess     b
     d MainProcess     pi

      /Free

      // Process until exit button is pressed.
       Dou ExitNow = *On;
          // Reset Import Error Grid
          ClrExMdError();
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
                ClrExMdError();
                // perform edits on the field values and display the grid if there are errors
                ProcTblExpMod();
                If isErrorExpMod;
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
                   // Submit the import job                                 t file
                   inPathName = scPathName + '$';
                   SubmitImport(inPathName);
                   Leave;
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
        strRunCmd = %Trim(strRunCmd) + 'WTEXMDIMP)';
        strRunCmd = %Trim(strRunCmd) + ' ' + 'TYPE(*CHAR) LEN(256)';
        strRunCmd = %Trim(strRunCmd) + ' ' + 'TEXT(''WTEXMDIMP Data Area'') ';
        Monitor;
           RunCmd(strRunCmd:98);
        On-Error;
        Endmon;

        //Retrieve Client-ID
        RtvCltID(strCltId);

      /End-Free

     p ProgramStart    e

      //===================================================================
      //  UpddateExpModErr - Update workfile to indicate error found
      //===================================================================
     p UpdateExpModErr...
     p                 b
     d UpdateExpModErr...
     d                 pi
     d inRow                          9s 0

      /Free

       Exec Sql
          Update qtemp/expmods
          set error = 'Y'
          Where rrn(expmods) = :inRow;

      /End-Free

     p UpdateExpModErr...
     p                 e

      //===================================================================
      //  UpdDataArea - Update Data Area with Path Name selected
      //===================================================================
     p UpdDataArea     b
     d UpdDataArea     pi

      /Free
        strRunCmd = 'CHGDTAARA DTAARA(';
        strRunCmd = %Trim(strRunCmd) + 'WTEXMDIMP';
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
       isErrorExpMod = *Off;
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
       // Must be a .TXT file
       length = %Len(%Trim(fullPathName));
       If length < 4;
          errPathNam = *On;
          msgPathNam = 'The file extension must be .TXT.';
          Return *Off;
       Else;
          If %Subst(fullPathName:length-3:4) <> '.TXT';
             errPathNam = *On;
             msgPathNam = 'The file extension must be .TXT.';
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
       CrtTblExpMod();

       // Did the data copy?
       If Not ImpFiletoTable();
          errPathNam = *On;

          Select;
          When dsMsgInfo.msgId = 'CPF2817';
             msgPathNam = 'Upload failed.  CSV possibly open in another ' +
                          'application.';
          Other;
             msgPathNam = dsMsgInfo.MsgText;
          EndSl;

          Return *Off;
       Endif;

       Return *On;
      /End-Free

     P Validate        e
