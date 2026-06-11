     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H BNDDIR('STBNDDIR')
     H dftactgrp(*no) actgrp(*caller)
     H*
     H/TITLE Masking Processing
     H*********************************************************************
     H*  DESCRIPTION:  This program will process masking.                 *
     H*                                                                   *
     H*  Valid types: SSN, BANK                                           *
     H*********************************************************************
     H* Revisions:                                                        *
     H*                                                                   *
     H*                                                                   *
     H*********************************************************************
     F
     ddecCo#           s              3  0 inz
     ddecSizeShow      s             10i 0 inz(4)
     di1               s             10i 0 inz
     disActive         s              1n   inz('0')
     disEofSMCOP       s              1n   inz('1')
     disLockExists     s              1n   inz
     dstrField         s             10    inz varying
     dstrFile          s             10    inz varying
     dstrFileField     s             21    inz varying
     dstrFileIn        s             10    inz varying
     dstrLib           s             10    inz varying
     dstrLibIn         s             10    inz varying
     dstrLibFile       s             21    inz varying
     dstrMask          s           1000    varying inz
     dstrMasking       s            500    varying inz(*ALL'*')
     dstrNameMask      s            100    inz varying
     dstrNameConst     s            100    inz varying
     dstrRtnCode       s             25    inz varying
     dstrShowSize      s              5    varying inz('4')
     dstrSql           s           5000    inz varying

     ddsSMCOP        e ds                  extname('SMCOP')

     d#ofMasks         c                   10
     daryMasks         s             25    dim(#ofMasks) ctdata perrcd(1)
     d aryFileField    s             20    dim(#ofMasks) alt(aryMasks)
     d

     d#ofTypes         c                   10
     d dsMasking       ds            35    qualified
     d  flag                   1      1
     d  user1                  6     15
     d  user2                 16     25
     d  user3                 26     35
     d aryMaskCO       ds                  likeds(dsMasking) dim(#ofMasks)

     d/copy sPrFiles
     d/copy sPrRunCmd
     d/copy sPrSql
     d/copy sPrString

     dRtvObjLib        pr                  extpgm('RTVOBJLIB')
     d inObj                         10    const
     d inType                         7    const
     d outLibrary                    10

      *Entry
     dSYMASK           pr                  ExtPgm('SYMASK')
     d                                4    const
     d                               25    const varying
     d                               10    const
     d                               20    const
     d                               25          options(*nopass)
     dSYMASK           pi
     d inCo#                          4    const                                *ALL
     d inType                        25    const varying                        *ALL
     d inProcess                     10    const                                *ADD,*REMOVE,
     d                                                                          *TESTLOCK
     d inLibFile                     20    const
     d outRtnCode                    25          options(*nopass)
      /Free
       Select;
       When inProcess = '*ACTIVE' and isActive;
          outRtnCode = inProcess;
          *inlr = *on;
          Return;
       Endsl;
       strSql = 'select * from SMCOP';
       If Upper(inCo#) <> '*ALL';
          strSql += ' where COco# = ' + inCo#;
       Endif;
       strFileIn = %trim(%subst(inLibFile: 1:10));
       strLibIn  = %trim(%subst(inLibFile:11:10));
       Exec sql close cursorSMCOP;
       Exec sql prepare sqlSMCOP from :strSql;
       Exec sql declare cursorSMCOP cursor for sqlSMCOP;
       Exec sql open cursorSMCOP;
       Dou not isEofSMCOP or isLockExists;
          Exec sql fetch next from cursorSMCOP into :dsSMCOP;
          Select;
          When not IsValidFetch(sqlstt);
             isEofSMCOP = *off;
             Iter;
          Endsl;
          Exsr ProcessConstraints;
       Enddo;
       If isLockExists;
          strRtnCode = '*LOCKEXISTS';
       Endif;
       If %parms >= 5;
          outRtnCode = strRtnCode;
       Endif;
       *inlr = *on;
       //==================
       Begsr Process;
        strFile  = %trim(%subst(aryFileField(i1): 1:10));
        strField = %trim(%subst(aryFileField(i1):11:10));
        strLib = GetFileLib(strFile:strLibIn); // Return *blank if object not found
        Select;
        When strFileIn <> '*ALL' and strFile <> strFileIn;
           Leavesr;
        When strLib = *blanks;
            Leavesr;
        Endsl;
        strLibFile = strLib + '.' + strFile;
        strFileField = strFile + '_' + strField;
        strNameMask = 'SYMASK_' + strFileField + '_masking';
        strNameConst = 'Check_' + strNameMask;
        Select;
        When inProcess = '*ADD';
           Exsr ProcessAdd;
        When inProcess = '*REMOVE';
           Exsr ProcessRemove;
        When inProcess = '*TESTLOCK';
           Exsr ProcessTestLock;
        Endsl;

       Endsr;
       //==================
       Begsr ProcessConstraints;
        For i1 = 1 to %elem(aryMasks);
           Select;
           When inType <> '*ALL' and aryMasks(i1) <> inType;
              Iter;
           When aryMasks(i1) = 'SSN'
            and inProcess <> ValidProcess(COmk01);
              Iter;
           When aryMasks(i1) = 'BANK'
            and inProcess <> ValidProcess(COmk02);
              Iter;
           Endsl;
           Exsr Process;
        Endfor;
       Endsr;
       //==================
       Begsr ProcessAdd;
        Exsr ProcessRemove;
        // substr('********************',1,Length(trim(BIACCT))-4) concat
        //  trim(substr(BIACCT,length(trim(BIACCT))-3,4)) As masked
        strMask = 'substring(''' + strMasking + ''',1,length(trim(' + strField +
         '))-' + %char(decSizeShow) + ') concat trim(substring(' + strField +
         ',length(trim(' + strField + '))-' + %char(decSizeShow-1) + ',' +
         %char(decSizeShow) + '))';
        strSql = 'create or replace mask ' + strNameMask +
         ' on ' + strLibFile +
         ' for column ' + strField + ' return' +
         '  case' +
         '  when VERIFY_GROUP_FOR_USER(CURRENT_USER,''' +
         %trim(dsMasking.user1) + ''') = 1' +
         '   then ' + strField;
        If dsMasking.user2 <> *blank;
           strSql += '  when VERIFY_GROUP_FOR_USER(CURRENT_USER,''' +
            %trim(dsMasking.user2) + ''') = 1' +
            '   then ' + strField;
        Endif;
        If dsMasking.user3 <> *blank;
           strSql += '  when VERIFY_GROUP_FOR_USER(CURRENT_USER,''' +
            %trim(dsMasking.user3) + ''') = 1' +
            '   then ' + strField;
        Endif;
        strSql += ' else ' + strMask + 'end enable';
        Exec sql execute immediate :strSql;
        strSql = 'alter table ' + strLibFile +
         ' add constraint ' + strNameConst +
         ' check (substring(' + strField + ',1,' +
          %char(%len(strField)-decSizeShow) + ') = ''' +
          %subst(strMasking:1:%len(strField)-decSizeShow) + '''))' +
         ' on update violation preserve ' + strField;
        Exec sql execute immediate :strSql;
       Endsr;
       //==================
       Begsr ProcessRemove;
        strSql = 'alter table ' + strLibFile +
         ' deactivate column access control';
        Exec sql execute immediate :strSql;
        strSql = 'alter table ' + strLibFile +
         ' drop constraint ' + strNameConst;
        Exec sql execute immediate :strSql;
       Endsr;
       //==================
       Begsr ProcessTestLock;
        If ChkFileLock(strLib:strFile) = *on;
           isLockExists = *on;
        Endif;
       Endsr;
       //==================
       Begsr Stub;

       Endsr;
      /End-free
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
      //=============================================================
      // Return library of requested file
      //=============================================================
     pGetFileLib       b
     dGetFileLib       pi            10
     d inFile                        10    const
     d inLib                         10    const

     dstrLib           s             10
      /Free
       Select;
       When inLib = '*LIBL';
          RtvObjLib(inFile:'*FILE':strLib);
       Other;
          strLib = inLib;
       Endsl;
       Return strLib;
      /End-free
     pGetFileLib       e
      //============================================================
      // Check for valid processing based on settings
      //=============================================================
     pValidProcess     b
     dValidProcess     pi            10
     d inInfo                              like(dsMasking)
      /Free
       dsMasking = inInfo;
       Select;
       When inProcess = '*TESTLOCK';
          Return inProcess;
       When dsMasking.flag = 'Y';
          Return '*ADD';
       Endsl;
       Return '*REMOVE';
      /End-free
     pValidProcess     e

** files/fields
Type                     File      Field
SSN                      WMCMP     CMSS#
SSN                      WMCDP     CDSS#
