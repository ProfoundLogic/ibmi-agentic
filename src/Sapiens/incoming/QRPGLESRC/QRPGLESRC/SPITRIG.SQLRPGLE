     hcopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')

     h/title SPITRIGGER - API - Auto Diary Trigger Processing
     h nomain
     h DFTNAME(SPITRIG)
     h option(*nodebugio: *srcstmt)
      **********************************************************************
      *  Description:  This copybook will be used for auto diary trigger
      *  processing
      *
      *  Created: 05/22/19 C:33    Support: 79577
      **********************************************************************
      * Revisions:
      *
      * 10/17/2024 C: 344 Ticket: 150-115584
      *                   Qualify %found for SMTRLFRQ chain
      **********************************************************************
     fsmtrl     uf   e           k disk    rename(ptr01:ptr00)
     fsmtrpfil  if   e           k disk    rename(ptr01:ptr0fil)
     fsmtrpfld  uf   e           k disk    rename(ptr01:ptr0fld)
     fsmtrlfrq  if   e           k disk    rename(ptr01:ptr0frq)
     fsmtrpfrq  if   e           k disk    rename(ptr01:ptrpfrq)

     d psds          esds                  extname(szq1p)

     dChkObjwLib       pr                  extpgm('SRCHKOB2')
     d outLib                        10    const
     d outFile                       10    const
     d outType                        7    const
     d inError                        1

     dGetObjLib        pr                  extpgm('RTVOBJLIB')
     d outObject                     10    const
     d outType                        7    const
     d inLib                         10

     dQCmdExc          pr                  extpgm('QCMDEXC')
     d command                             const like(strCommand)
     d length                        15  5 const

     dRtvSuffix        pr                  extpgm('RTVSUFFIX')
     d inSuffix                       3

     d strCommand      s            255

      * copy books
      /copy sPrTrig
      /copy sPrDate
      /copy sPrString
      /copy sPrModule
      /copy sPrRunCmd

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

     P* --------------------------------------------------
     P* Create file triggers
     P* --------------------------------------------------
     PCreateFileTrg    b                   export
     DCreateFileTrg    pi             1n
     D inTrgName                    128

     D isDevMachine    s               n
     D isError         s               n
     D strField        s             20
     D strFieldChg     s           5000    inz
     D strFieldChgN    s           5000    inz
     D strFieldChgO    s           5000    inz
     D strFieldRpt     s           5000    inz
     D strFieldRptO    s           5000    inz
     D strFieldRptN    s           5000    inz
     D strLibFile      s             10    inz
     D strLibObj       s             10    inz
     D strLibEnv       s             10    inz
     D strPDSOBJ       s             10
     D strPgm          s             10
     D strSqlCursor    s           5000
     D strSql          s           5000
     D strTrgEvent     s                   like(TRtrgEvent)
     D strTrgTime      s                   like(TRtrgTime)
     D strTrigger      s            150
     D strSuffix       s              3
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

     DChkDevMachine    pr                  ExtPgm('SACHKPDS')
     D dev                            1n
     dChkObj           pr                  extpgm('SRCHKOBJ')
     d outObj                        10    const
     d outType                        7    const
     d inError                        1
     DSetDevEnv        pr                  ExtPgm('LODWC2405')

      /free
       Exec SQL set option COMMIT=*NONE;
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
       GetObjLib('ATNAVP':'*FILE':strLibEnv);

       Chain inTrgName ptr0frq dsFrq;
       //*** Beg Chg *** 10/17/24 ******************************************
       //***If not %found;
       If not %found(SMTRLFRQ);
       //*** End Chg *** 10/17/24 ******************************************
          Return *off;
       Endif;
       // Get associated record in SMTRPFIL
       Chain (dsFrq.TRco#:dsFrq.TRname:dsFrq.TRseq#) ptr0fil dsFil;
       If not CheckFFDExists;
          BuildFFD(dsFil.TRlibrary:dsFil.TRfile);
       Endif;
       strTrigger = %trim(strPDSOBJ) + '.' + %trim(inTrgName);
       Monitor;
       // strSql = 'DROP TRIGGER ' + %trim(strTrigger);
       // Exec SQL execute immediate :strSQL;
       On-Error;
       Endmon;

       // Read through SMTRPFLD
       Setll (dsFrq.TRco#:dsFrq.TRname:dsFrq.TRseq#) ptr0fld;
       Dou %eof(smtrpfld);
          Reade (dsFrq.TRco#:dsFrq.TRname:dsFrq.TRseq#) ptr0fld dsFld;
          Select;
          When %eof;
             Iter;
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
        ' SET OPTION COMMIT=*NONE' +
        ' BEGIN' +
        '  declare UpdatePgm  char(10)  default ''' + strPgm + ''';'        +
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
          Return *on;
       Other;
          Chain (dsFrq.TRco#:dsFrq.TRname:dsFrq.TRseq#) ptr00;
          If %found;
             Select;
             When TRstat = 'P' and TriggerExists(TRco#:TRname:TRseq#);
                TRstat = 'A';
                TRpgm  = q1Pgm;
                TRuser = q1User;
                TRtime = Timeto6(SystemTime());
                TRdt   = Dateto7(SystemDate());
             Endsl;
             Update ptr00;
          Endif;
          Return *off;
       Endsl;
      /end-free
     PCreateFileTrg    e

     P* --------------------------------------------------
     P* Drop file triggers
     P* --------------------------------------------------
     PDropFileTrg      b                   export
     DDropFileTrg      pi             1n
     D inTrgName                    128

     D isDevMachine    s               n
     D isError         s               n
     D strLibFile      s             10    inz
     D strLibObj       s             10    inz
     D strPDSOBJ       s             10
     D strSql          s           5000
     D strTrigger      s            150
     D strSuffix       s              3
     D dsFrq           ds                  likeRec(ptr0frq:*input)

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

       Chain inTrgName ptr0frq dsFrq;
       //*** Beg Chg *** 10/17/24 ******************************************
       //***If not %found;
       If not %found(SMTRLFRQ);
       //*** End Chg *** 10/17/24 ******************************************
          Return *off;
       Endif;
       strTrigger = %trim(strPDSOBJ) + '.' + %trim(inTrgName);
       Monitor;
          strSql = 'DROP TRIGGER ' + %trim(strTrigger);
          Exec SQL execute immediate :strSQL;
       On-Error;
          Return *on;
       Endmon;

       Select;
       When sqlStt <> *zero;
          Return *on;
       Other;
          Return *off;
       Endsl;
      /end-free
     PDropFileTrg      e

     P* --------------------------------------------------
     P* Get trigger name
     P* --------------------------------------------------
     PGetTriggerName   b                   export
     DGetTriggerName   pi           128
     D inLib                         10
     D inFile                        10
     D inEvent                        8

     ddecIndex         s              5  0 inz(0)
     dstrEvent         s             10
     dstrIndex         s              5
     dstrName          s            128
     dstrNamePrefix    s            128

      /free
       strEvent = %trim(ScanReplace('*':' ':inEvent));
       strNamePrefix = %trim(inLib)+'_'+%trim(inFile)+'_'+%trim(strEvent)+'_';
       Dou not %found;
          decIndex += 1;
          strName = %trim(strNamePrefix) + %editc(decIndex:'X');
          Chain strName ptr0frq;
       Enddo;
       Return strName;

       strName = %trim(inLib)+'_'+%trim(inFile)+'_'+%trim(inEvent)+'_';
       Setgt strName ptr0frq;
       Readp ptr0frq;
       //*** Beg Chg *** 10/17/24 ******************************************
       //***If not %eof and
       If not %eof(SMTRLFRQ) and
       //*** End Chg *** 10/17/24 ******************************************
        %subst(TRtrgName:1:%len(%trim(strName))) = %trim(strName);
          strIndex = ParseString(TRtrgName:'_':4);
          decIndex = %int(strIndex);
       Endif;
       decIndex += 1;
       strName = %trim(strName) + %editc(decIndex:'X');
       Return strName;
      /end-free
     PGetTriggerName   e

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

