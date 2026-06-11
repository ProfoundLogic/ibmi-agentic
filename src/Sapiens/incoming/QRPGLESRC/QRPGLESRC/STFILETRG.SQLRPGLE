     hcopyright('(c) 2024 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')

     h/title STFILETRGR - Build File Triggers
     h nomain
     h option(*nodebugio: *srcstmt) bnddir('STBNDDIR')
      **********************************************************************
      *  Created: 05/03/17 C:33    Project: WC2405
      **********************************************************************
      * Revisions:
      *
      *  09/06/17 C:33  Project: WC2405
      *                -Account for Inactive triggers
      *
      *  06/22/17 C:33  Project: WC2405
      *                -Account for Profound environments
      *
      * 05/10/17  C:33  Project: WC2405 (no comments)
      *                -Include 'WHEN' in the trigger
      *                -Include inactive fields/frequency
      **********************************************************************
     fsmtrl     uf   e           k disk    rename(ptr01:ptr00)    usropn
     fsmtrpgrp  if   e           k disk    rename(ptr01:ptr0grp)  usropn
     f                                     prefix(g)
     fsmtrpclm  if   e           k disk    rename(ptr01:ptr0clm)  usropn
     f                                     prefix(c)
     fsmtrppol  if   e           k disk    rename(ptr01:ptr0pol)  usropn
     f                                     prefix(p)
     fsmtrlrec  if   e           k disk    rename(ptr01:ptr0rec)  usropn
     f                                     prefix(r)
     fsmtrptbl  if   e           k disk    rename(ptr01:ptr0tbl)  usropn
     f                                     prefix(t)
     fsmtrplog  if   e           k disk    rename(ptr01:ptr0log)  usropn
     f                                     prefix(l)
     fsmtrpdrt  uf a e           k disk    rename(ptr01:ptr0drt)  usropn
     f                                     prefix(m)
     fsmtrpact  if   e           k disk    rename(ptr01:ptr0act)  usropn
     f                                     prefix(a)
     fsmtrpfil  if   e           k disk    rename(ptr01:ptr0fil)  usropn
     fsmtrpfld  uf   e           k disk    rename(ptr01:ptr0fld)  usropn
     fsmtrlfrq  if   e           k disk    rename(ptr01:ptr0frq)  usropn
     fsmtrpfrq  if   e           k disk    rename(ptr01:ptrpfrq)  usropn
     fsmtrpwhn  if   e           k disk    rename(ptr01:ptr0whn)  usropn

     d psds          esds                  extname(szq1p)

      * work variables
     d intCo#          s              3  0
     d intFnd          s              3  0
     d intCase         s              7  0
     d intLastRun      s              7  0 inz
     d isTrgSet        s               n   inz
     d isOpen          s               n   inz
     d isClaim         s               n   inz
     d Ptr             s               *   procptr
     d PtrBfrAddr      s               *
     d strTrgName      s             10
     d strCommand      s            255
     d strCmd          s            255
     d strQry          s            255
     d strFile         s             10    inz
     d strMsg          s            700

     DBuildFFD         pr
     D inLib                         10
     D inFile                        10

     DCheckFFDExists   pr              n

     DCheckFileLock    PR             1n
     D inLib                         10    const
     D inFile                        10    const
     D inMbr                         10    const options(*nopass)

     d ChkObjwLib      pr                  extpgm('SRCHKOB2')
     d  outLib                       10    const
     d  outFile                      10    const
     d  outType                       7    const
     d  inError                       1

     DCreateFileTrg    pr             1n
     D inTrgName                    128

     DFileAction       pr
     D inAction                      10    const

     d GetObjLib       pr                  extpgm('RTVOBJLIB')
     d Object                        10    const
     d Type                           7    const
     d Library                       10

     d QCmdExc         pr                  extpgm('QCMDEXC')
     d command                             const like(strCommand)
     d length                        15  5 const

     d RtvSuffix       pr                  extpgm('RTVSUFFIX')
     d  inSuffix                      3

     dTriggerExists    pr             1n
     d inCo#                          3  0
     d inName                        10
     d inSeq#                         5  0

     d/copy sprdate
     d*copy sprfiles
     d/copy sprruncmd
     d/copy sprstring

     P* --------------------------------------------------
     P* Build QADSPFFD
     P* --------------------------------------------------
     PBuildFFD         b
     DBuildFFD         pi
     D inLib                         10
     D inFile                        10

     dstrCmd           s            255
      /free
       strCmd = 'DSPFFD FILE(' + %trim(inLib) + '/' + %trim(inFile) +
        ') OUTPUT(*OUTFILE) OUTFILE(QTEMP/QADSPFFD) OUTMBR(*FIRST *REPLACE)';
        RunCmd(strCmd:%len(strCmd));
       Return;
      /end-free
     PBuildFFD         e

     P* --------------------------------------------------
     P* Check for the existence of QADSPFFD
     P* --------------------------------------------------
     PCheckFFDExists   b
     DCheckFFDExists   pi              n

     dstrError         s              1
      /free
       ChkObjwLib('QTEMP':'QADSPFFD':'*FILE':strError);
       Return strError = '0';
      /end-free
     PCheckFFDExists   e

     P*=====================================================================
     P* Check to see if the file is locked
     P*=====================================================================
     PCheckFileLock    b
     DCheckFileLock    pi             1n
     D inLib                         10    const
     D inFile                        10    const
     D inMbr                         10    const options(*nopass)

     DdsLock           ds                  qualified
     D lib                           10
     D file                          10
     D mbr                           10
     D state                          9
     D status                        11
     D scope                         12
     D job                           30

     DisLocked         s              1n   inz('0')
     DstrSqlCursor     s           5000
      /Free
       Exec SQL set option COMMIT=*NONE;
       strSqlCursor = 'select SYS_DNAME, SYS_ONAME, SYS_MNAME, LOCK_STATE, ' +
        'LOCK_STATUS, LOCK_SCOPE, JOB_NAME ' +
        'from OBJ_LOCK ' +
        'where SYS_DNAME = ''' + %trim(inLib) + '''' +
        '  and SYS_ONAME = ''' + %trim(inFile) + '''';
       If %parms >= 3 and inMbr <> *blanks;
          strSqlCursor = %trim(strSqlCursor) +
           ' and SYS_MNAME = ''' + %trim(inMbr) + '''';
       Endif;
       Exec SQL Prepare sq1Lock From :strSqlCursor;
       Exec SQL Declare cursorLock Cursor For sq1Lock;
       Exec SQL Open cursorLock;
       Exec SQL Fetch next From cursorLock into :dsLock;
       //strCmd = 'SNDMSG MSG(''' + %trim(strSqlCursor) + ' - ' +
       // sqlstt + ':' + %editc(sqlcod:'X') + ' - ' + %trim(dsLock) +
       // ''') TOUSR(' + q1User + ')';
       //RunCmd(strCmd:%len(strCmd));
       Select;
       When SQLSTT = '00000';
          isLocked = *on;
       Endsl;
       Exec SQL Close cursorLock;
       Return isLocked;
      /End-free
     PCheckFileLock    e

     P* --------------------------------------------------
     P* Create file trigger
     P* --------------------------------------------------
     PCreateFileTrg    b                   export
     DCreateFileTrg    pi             1n
     D inTrgName                    128

     D flgNewStatus    s                   like(TRstat)
     D isDevMachine    s               n
     D isError         s               n
     D strField        s             20
     D strFieldChg     s           5000    inz
     D strFieldChgN    s           5000    inz
     D strFieldChgO    s           5000    inz
     D strFieldRpt     s           5000    inz
     D strFieldRptO    s           5000    inz
     D strFieldRptN    s           5000    inz
       //***  Begin Add  ***  06/22/17  ***********************************
     D strLibEnv       s             10    inz
       //***  End   Add  ***  06/22/17  ***********************************
     D strLibFile      s             10    inz
     D strLibObj       s             10    inz
     D strPDSOBJ       s             10
     D strPgm          s             10
     D strSql          s          32000
     D strSqlCursor    s          32000
     D strSqlWhen      s           1005    inz
     D strTrgEvent     s                   like(TRtrgEvent)
     D strTrgTime      s                   like(TRtrgTime)
     D strTrigger      s            150
     D strSuffix       s              3
     D strWhen         s                   like(TRwhen)
     D decPos          s              5  0
     D decPosChg       s              5  0 inz(1)
     D decPosRpt       s              5  0 inz(1)

     D dsFFD           ds                  inz
     D  fldB                          5  0
     D  fldD                          2  0
     D  fldP                          2  0
     D  fldT                          1
     D dsFil           ds                  likeRec(ptr0fil:*input)
     D dsFld           ds                  likeRec(ptr0fld:*input)
     D dsFrq           ds                  likeRec(ptr0frq:*input)
     D dsWhen          ds                  likeRec(ptr0whn:*input)

     DChkDevMachine    pr                  ExtPgm('SACHKPDS')
     D dev                            1n
     dChkObj           pr                  extpgm('SRCHKOBJ')
     d outObj                        10    const
     d outType                        7    const
     d inError                        1
     DSetDevEnv        pr                  ExtPgm('LODWC2405')

      /free
       RtvSuffix(strSuffix);
       strPDSOBJ = 'PDSOBJ' + strSuffix;
       strLibObj = strPDSOBJ;
       GetObjLib('SMTRP':'*FILE':strLibFile);
       ChkDevMachine(isDevMachine);
       If (isDevMachine);
          strPDSOBJ = %subst(strLibFile:1:6);
          ChkObj(strPDSOBJ:'*LIB':isError);
          If isError;
             strPDSOBJ = %trim(strPDSOBJ) + 'OBJ';
          Endif;
          GetObjLib('M0002C':'*PGM':strLibObj);
          SetDevEnv();
       Endif;
       //***  Begin Add  ***  06/22/17  ***********************************
       GetObjLib('ATNAVP':'*FILE':strLibEnv);
       //***  End   Add  ***  06/22/17  ***********************************

       FileAction('*OPEN');
       Chain inTrgName ptr0frq dsFrq;
       If not %found;
          Return *off;
       Endif;
       //***  Begin Add  ***  09/06/17  ***********************************
       // Get associated record in SMTRP
       Clear TRstat;
       Chain(n) (dsFrq.TRco#:dsFrq.TRname:dsFrq.TRseq#) ptr00;
       //***  End   Add  ***  09/06/17  ***********************************
       // Get associated record in SMTRPFIL
       Chain (dsFrq.TRco#:dsFrq.TRname:dsFrq.TRseq#) ptr0fil dsFil;
       //If ChkFileLock(dsFil.TRlibrary:dsFil.TRfile);
       If CheckFileLock(dsFil.TRlibrary:dsFil.TRfile);
          Return *on;
       Endif;
       If not CheckFFDExists;
          BuildFFD(dsFil.TRlibrary:dsFil.TRfile);
       Endif;
       strTrigger = %trim(strPDSOBJ) + '.' + %trim(inTrgName);
       //***  Begin Add  ***  05/10/17  ***********************************
       //***  Begin Chg  ***  09/06/17  ****************************
       //**********If dsFrq.TRstat = 'I';
       If dsFrq.TRstat = 'I' or Upper(TRstat) = 'I';
          flgNewStatus = TRstat;
       //***  End   Chg  ***  09/06/17  ****************************
          Monitor;
             strSql = 'DROP TRIGGER ' + %trim(strTrigger);
             Exec SQL execute immediate :strSQL;
             //***  Begin Add  ***  09/06/17  ****************************
             Select;
             When sqlStt = *zero;
                flgNewStatus = 'I';
             Endsl;
             FileAction('*OPEN');
             Chain(n) (dsFrq.TRco#:dsFrq.TRname:dsFrq.TRseq#) ptr00;
             If %found(smtrl);
                Select;
                When TRstat = 'i' and not TriggerExists(TRco#:TRname:TRseq#);
                   Exsr UpdateTrigSts;
                When TRstat <> flgNewStatus;
                   Exsr UpdateTrigSts;
                Endsl;
                Unlock smtrl;
             Endif;
             FileAction('*CLOSE');
             //***  End   Add  ***  09/06/17  ****************************
          On-Error;
           //strCmd = 'SNDMSG MSG('''+%trim(strSql)+''') TOUSR('+q1User+')';
           //RunCmd(strCmd:%len(strCmd));
          Endmon;
          FileAction('*CLOSE');
          Return *off;
       Endif;
       //***  End   Add  ***  05/10/17  ***********************************

       // Read through SMTRPFLD
       Setll (dsFrq.TRco#:dsFrq.TRname:dsFrq.TRseq#) ptr0fld;
       Dou %eof(smtrpfld);
          Reade (dsFrq.TRco#:dsFrq.TRname:dsFrq.TRseq#) ptr0fld dsFld;
          Select;
          When %eof;
             Iter;
          //***  Begin Add  ***  05/10/17  ********************************
          When dsFld.TRstat = 'I';
             Iter;
          //***  End   Add  ***  05/10/17  ********************************
          Endsl;
          Clear dsFld.TRposition;
          Clear dsFld.TRfldB;
          Clear dsFld.TRfldD;
          Clear dsFld.TRfldP;
          strSqlCursor = 'select WHFLDB, WHFLDD, WHFLDP, WHFLDT' +
           ' from QTEMP/QADSPFFD' +
           ' where WHLIB  = ''' + %trim(dsFil.TRlibrary) + '''' +
           '   and WHFILE = ''' + %trim(dsFil.TRfile) + '''' +
           '   and (WHFLDI = ''' + %trim(dsFld.TRfield) + '''' +
           '     or WHFLDE = ''' + %trim(dsFld.TRfield) + ''')';
          Exec SQL Prepare sql1 From :strSqlCursor;
          Exec SQL Declare cursor1 Cursor For sql1;
          Exec SQL Open cursor1;
          Exec SQL Fetch next From cursor1 into :dsFFD;
          If SQLSTT = '00000';
             Select;
             When fldD > fldB;
                decPos = fldD;
             Other;
                decPos = fldB;
             Endsl;
             Select;
             When dsFld.TRfldT = 'P' or dsFld.TRfldT = 'S' or
              dsFld.TRfldT = 'L' or dsFld.TRfldT = 'T' or dsFld.TRfldT = 'Z';
                strField = 'DIGITS(*.' + %trim(dsFld.TRfield) + ')';
             Other;
                strField = '*.' + dsFld.TRfield;
             Endsl;
             Select;
             When dsFld.TRtype = 'Change';
                If strFieldChg <> *blanks;
                   strFieldChg = %trim(strFieldChg) + ' ||';
                Endif;
                strFieldChg = %trim(strFieldChg) + ' ' + strField;
                dsFld.TRposition = decPosChg;
                decPosChg += decPos;
             When dsFld.TRtype = 'Report';
                If strFieldRpt <> *blanks;
                   strFieldRpt = %trim(strFieldRpt) + ' ||';
                Endif;
                strFieldRpt = %trim(strFieldRpt) + ' ' + strField;
                dsFld.TRposition = decPosRpt;
                decPosRpt += decPos;
             Endsl;
             dsFld.TRfldB = fldB;
             dsFld.TRfldD = fldD;
             dsFld.TRfldP = fldP;
             dsFld.TRfldT = fldT;
          Endif;
          Exec SQL Close cursor1;
          // Update SMTRPFLD
          dsFld.TRuser = q1User;
          dsFld.TRpgm  = q1Pgm;
          dsFld.TRtime = TimeTo6(SystemTime);
          dsFld.TRdt   = DateTo7(SystemDate);
          Update ptr0fld dsFld;
       Enddo;

       Clear dsWhen;
       Chain (dsFrq.TRco#:dsFrq.TRname:dsFrq.TRseq#) ptr0whn dsWhen;
       strSqlWhen = ' SET OPTION COMMIT=*NONE';
       If dsWhen.TRwhen <> *blank;
          strWhen = ScanReplace('WHEN':'':%trim(Upper(dsWhen.TRwhen)):1:1);
          strSqlWhen = ' WHEN (' + %trim(strWhen) + ')';
       Endif;
       //strCmd = 'SNDMSG MSG('''+%trim(strSqlWhen)+''') TOUSR('+q1User+')';
       //RunCmd(strCmd:%len(strCmd));

       FileAction('*CLOSE');

       Select;
       When dsFrq.TRtrgEvent = '*INSERT';
          strPgm = 'QDBPUT';
          strFieldChgN = ' || ' + %trim(ScanReplace('*.':'N.':strFieldChg));
          strFieldChgO = *blanks;
          strFieldRptN = ' || ' + %trim(ScanReplace('*.':'N.':strFieldRpt));
          strFieldRptO = *blanks;
       When dsFrq.TRtrgEvent = '*DELETE';
          strPgm = 'QDBUDR';
          strFieldChgN = *blanks;
          strFieldChgO = ' || ' + %trim(ScanReplace('*.':'O.':strFieldChg));
          strFieldRptN = *blanks;
          strFieldRptO = ' || ' + %trim(ScanReplace('*.':'O.':strFieldRpt));
       When dsFrq.TRtrgEvent = '*UPDATE';
          strPgm = 'QDBUDR';
          strFieldChgN = ' || ' + %trim(ScanReplace('*.':'N.':strFieldChg));
          strFieldChgO = ' || ' + %trim(ScanReplace('*.':'O.':strFieldChg));
          strFieldRptN = ' || ' + %trim(ScanReplace('*.':'N.':strFieldRpt));
          strFieldRptO = ' || ' + %trim(ScanReplace('*.':'O.':strFieldRpt));
       Endsl;

       strTrgTime = %trim(ScanReplace('*':' ':dsFrq.TRtrgTime));
       strTrgEvent = %trim(ScanReplace('*':' ':dsFrq.TRtrgEvent));
       strSql = 'CREATE or REPLACE TRIGGER ' + %trim(strTrigger) + ' ' +
        %trim(strTrgTime) + ' ' + %trim(strTrgEvent) +
        ' on ' + %trim(dsFil.TRlibrary) + '.' + %trim(dsFil.TRfile) +
        ' REFERENCING NEW ROW AS N OLD ROW AS O' +
        ' FOR EACH ROW MODE DB2SQL' +
        %trimr(strSqlWhen) +
        ' BEGIN' +
        '  declare UpdatePgm  char(10)  default ''' + strPgm + ''';' +
        '  declare CalledPgm  char(10) default '''';' +
        '  declare outTrgName char(128) default ''' + inTrgName + ''';' +
        '  declare outLibFile char(10) default ''' + strLibFile + ''';' +
        '  declare outLibObj  char(10) default ''' + strLibObj + ''';' +
        '  declare outLibEnv  char(10) default ''' + strLibEnv + ''';' +
        '  declare outPgm    char(10);' +
        '  declare outUser   char(10);' +
        '  declare outChgO char(32765);' +
        '  declare outChgN char(32765);' +
        '  declare outRptO char(32765);' +
        '  declare outRptN char(32765);' +
        '  call ' + %trim(strPDSOBJ) + '.GetPrvCall(CalledPgm,UpdatePgm);' +
        '  set outPgm = CalledPgm;' +
        '  set outUser = User;' +
        '  set outChgO = (''O''' + %trimr(strFieldChgO) + ');' +
        '  set outChgN = (''N''' + %trimr(strFieldChgN) + ');' +
        '  set outRptO = (''O''' + %trimr(strFieldRptO) + ');' +
        '  set outRptN = (''N''' + %trimr(strFieldRptN) + ');' +
        '  call ' + %trim(strPDSOBJ) + '.PROCFILECHG' +
        '  (outTrgName, outUser, outPgm, outLibFile, outLibObj, outLibEnv,' +
        '   outChgO, outChgN, outRptO, outRptN);' +
        ' END';
       Exec SQL execute immediate :strSQL;
       Select;
       When sqlStt <> *zero;
          flgNewStatus = 'X';
       Other;
          flgNewStatus = 'A';
       Endsl;
       FileAction('*OPEN');
       Chain(n) (dsFrq.TRco#:dsFrq.TRname:dsFrq.TRseq#) ptr00;
       If %found(smtrl);
          Select;
          When TRstat = 'P' and TriggerExists(TRco#:TRname:TRseq#);
             Exsr UpdateTrigSts;
          When TRstat <> flgNewStatus;
             Exsr UpdateTrigSts;
          Endsl;
          Unlock smtrl;
       Endif;
       FileAction('*CLOSE');
       Return *off;
       //==================================================================
       Begsr UpdateTrigSts;
        Chain (dsFrq.TRco#:dsFrq.TRname:dsFrq.TRseq#) ptr00;
        TRstat = flgNewStatus;
        TRpgm  = q1Pgm;
        TRuser = q1User;
        TRtime = Timeto6(SystemTime());
        TRdt   = Dateto7(SystemDate());
        Update ptr00;
       Endsr;
      /end-free
     PCreateFileTrg    e

     P* --------------------------------------------------
     P* File action
     P* --------------------------------------------------
     PFileAction       b
     DFileAction       pi
     D inAction                      10    const

      /free
       Select;
       When inAction = '*OPEN';
          If %open(SMTRL);
             Return;
          Endif;
          Exsr FileOpen;
       When inAction = '*CLOSE';
          If not %open(SMTRL);
             Return;
          Endif;
          Exsr FileClose;
       Endsl;
       Return;
       //==================================================================
       Begsr FileClose;
        Close smtrl;
        Close smtrpgrp;
        Close smtrpclm;
        Close smtrppol;
        Close smtrlrec;
        Close smtrptbl;
        Close smtrplog;
        Close smtrpdrt;
        Close smtrpact;
        Close smtrpfil;
        Close smtrpfld;
        Close smtrpfrq;
        Close smtrlfrq;
        Close smtrpwhn;
       Endsr;
       //==================================================================
       Begsr FileOpen;
        Open smtrl;
        Open smtrpgrp;
        Open smtrpclm;
        Open smtrppol;
        Open smtrlrec;
        Open smtrptbl;
        Open smtrplog;
        Open smtrpdrt;
        Open smtrpact;
        Open smtrpfil;
        Open smtrpfld;
        Open smtrpfrq;
        Open smtrlfrq;
        Open smtrpwhn;
       Endsr;
      /end-free
     PFileAction       e

       //===================================================================
       // Check for trigger existence
       //===================================================================
     pTriggerExists    b                   export
     dTriggerExists    pi             1n
     d inCo#                          3  0
     d inName                        10
     d inSeq#                         5  0

     D outExists       s              1n   inz(*on)
     D strSqlCursor    s           5000
     D strSql          s           5000

     D dsTrigger       ds                  inz
     D  name                        130
      /Free
       FileAction('*OPEN');
       Chain(n) (inCo#:inName:inSeq#) smtrl;
       Select;
       When TRname <> 'FILE RULES';
          Return *on;
       Endsl;
       Setll (inCo#:inName:inSeq#) smtrpfrq;
       Dou %eof;
          Reade (inCo#:inName:inSeq#) smtrpfrq;
          Select;
          When %eof;
             Iter;
          When TRstat <> 'A';
             Iter;
          Endsl;
          strSqlCursor = 'select TRIGNAME ' +
           'from SYSTRIGGERS ' +
           'where TRIGNAME = ''' + %trim(TRtrgName) + '''';
          Exec SQL Prepare sql2 From :strSqlCursor;
          Exec SQL Declare cursor2 Cursor For sql2;
          Exec SQL Open cursor2;
          Exec SQL Fetch next From cursor2 into :dsTrigger;
          If SQLSTT <> '00000';
             outExists = *off;
          Endif;
          Exec SQL Close cursor2;
       Enddo;
       Return outExists;
      /End-free
     pTriggerExists    e

