     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H nomain
      *
      *********************************************************************
      * REVISIONS:
     *
      *  08/04/21  C:180 Support: 97167
      *                  -CreateAliasSql - Allow option not to check for
      *                   file lock.
      *
     *  08/04/20  C:33  Support: 90576
     *                  -AddFileMbr - return error, if so
     *
     *  04/20/20  C:33    Project: SY420
     *                   -ChkFileLock - allow for null values retrieved
     *
     *  02/01/19  C:33    Project: SY407
     *                   -LeaveFileRead
     *
     *  01/14/19  C:33    Project: SY406
     *                   -CreateAliasSQL
     *
     *  12/06/18  C:33    Project: SY406
     *                   -ChkFileLayouts
     *                   -DspFileLayout
     *
     *  05/10/17  C:33    Project: WC2405
     *                    Check file lock
     *
     *  04/04/17  C:33    Project: SY388
     *                    Get file/field description(s)
     *
     *  04/12/16  C:212   Project : C114MXO
     *                    Added BldMember
      *********************************************************************
      *
     D/copy sprfiles
     D/copy sprruncmd
     D***  Begin Add  ***  01/14/19  *************************************
     dGetObjLib        pr                  extpgm('RTVOBJLIB')
     d inObject                      10    const
     d inType                         7    const
     d outLibrary                    10
     D***  End   Add  ***  01/14/19  *************************************

     DdsField          ds                  qualified
     D field                         10
     D file                          10
     D lib                           10
     D desc                         102

     DdsFile           ds                  qualified
     D file                          10
     D lib                           10
     D desc                         102


     P*=====================================================================
     P* addFileMbr- Add a member to a file
     P*  Parameter 1 - file name   (10a)
     P*  Parameter 2 - member name (10a)
     P*  Returns - *on if an error occurred, otherwise *off
     P*=====================================================================
     PaddFileMbr       B                   Export
     D
     DaddFileMbr       PI             1
     D fileName                      10    const
     D memberName                    10    const
     D
     D error           s              1    inz('0')

       //***  Begin Add  ***  08/04/20  ********************
      /Free
       Monitor;
      /End-free
       //***  End   Add  ***  08/04/20  ********************
     C                   callp     runCmd('ADDPFM FILE('
     C                                  + %trim(fileName) + ') MBR('
     C                                  + %trim(memberName) + ')':255)
       //***  Begin Add  ***  08/04/20  ********************
      /Free
       On-error;
          error = *on;
       Endmon;
      /End-free
       //***  End   Add  ***  08/04/20  ********************
     C*
     C                   return    error

     PaddFileMbr       E





      //***  Begin Add  ***  04/04/17  ************************************
     P*=====================================================================
     P* Append information
     P*  Parameter 1 - string 1    (150a)
     P*  Parameter 2 - string 2    (150a)
     P*  Parameter 3 - delimiter    ( 1a)
     P*  Returns - description appended with string
     P*=====================================================================
     PAppendInfo       b
     DAppendInfo       pi           300
     D inString1                    150    const
     D inString2                    150    const
     D inDelimiter                    1    const options(*nopass)

     DoutDesc          s            300
     DstrDelimiter     s              1    inz('-')
      /Free
       If %parms >= 3 and inDelimiter <> *blanks;
          strDelimiter = inDelimiter;
       Endif;
       Select;
       When strDelimiter = '-';
          outDesc = %trim(inString1) + ' - ' + %trim(inString2);
       When strDelimiter = '(' or strDelimiter = ')';
          outDesc = %trim(inString1) + ' (' + %trim(inString2) + ')';
       Other;
          outDesc = %trim(inString1) + ' - ' + %trim(inString2);
       Endsl;
       Return outDesc;
      /End-free
     PAppendInfo       e
      //***  End   Add  ***  04/04/17  ************************************


      //***  Begin Add  ***  12/06/18  ***********************************
     P*=====================================================================
     P* ChkFileLayouts - Check to see if the file layouts are the same
     P*  Parameter 1 - current file (21a) (file only or lib/file)
     P*  Parameter 2 - compare file (21a) (file only or lib/file)
     P*  Returns - *on if the file layouts are the same, otherwise *off
     P*   {This routine assumes the files exists.  If one or the other doesn't exist, returns *off}
     P*=====================================================================
     PChkFileLayouts   b                   export
     DChkFileLayouts   pi             1n
     D inFileCur                     21    const varying
     D inFileCmp                     21    const varying
     D*
     DdecCount         s             10i 0 inz
     DstrCmd           s            500    inz varying
      /Free
       Monitor;
          DspFileLayout(inFileCur:'*OUTFILE':'QTEMP/CMPFFD1');
          DspFileLayout(inFileCmp:'*OUTFILE':'QTEMP/CMPFFD2');
          strCmd = 'STRQMQRY QMQRY(CMPFFD) OUTPUT(*OUTFILE) ' +
           ' OUTFILE(QTEMP/CMPFFD3)';
          RunCmd(strCmd:%len(strCmd));
          Exec sql
           select count(*)
            into :decCount
            from QTEMP/CMPFFD3;
          Return decCount = 0;
       On-error;
          Return *off;
       Endmon;
      /End-free
     PChkFileLayouts   e
      //***  End   Add  ***  12/06/18  ***********************************



      //***  Begin Add  ***  05/10/17  ************************************
     P*=====================================================================
     P* ChkFileLock - Check to see if the file is locked
     P*  Parameter 1 - library name  (10a)
     P*  Parameter 2 - file name     (10a)
     P*  Parameter 3 - member name   (10a) {optional}
     P*  Returns - *on if the file(member) is locked, otherwise *off
     P*=====================================================================
     PChkFileLock      b                   export
     DChkFileLock      pi             1n
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
     D***  Begin Add  ***  04/20/20  *****************
     DlockNullInds     s              5i 0 dim(7)
     D***  End   Add  ***  04/20/20  *****************

     DisLocked         s              1n   inz('0')
     DstrSqlCursor     s           5000
      /Free
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
       Exec SQL Fetch next From cursorLock into :dsLock :lockNullInds;
       Select;
       When SQLSTT = '00000';
          isLocked = *on;
       Endsl;
       Exec SQL Close cursorLock;
       Return isLocked;
      /End-free
     PChkFileLock      e
      //***  End   Add  ***  05/10/17  ************************************





     P*=====================================================================
     P* checkMbr - Check to see if the member exists
     P*  Parameter 1 - object name   (10a)
     P*  Parameter 2 - object type   ( 7a)
     P*  Parameter 3 - object member (10a)
     P*  Returns - *on if the member exist, otherwise *off
     P*=====================================================================
     PchkFileMbr       B                   Export
     D
     DchkFileMbr       PI             1
     D objectName                    10    const
     D objectType                     7    const
     D objectMbr                     10    const
     D
     DsrChkMbr         PR                  extpgm('SRCHKMBR')
     D objName                       10    const
     D objType                        7    const
     D objMbr                        10    const
     D error                          1
     D
     D error           s                   like(*in60) inz('1')

     C                   callp     srChkMbr(objectName:objectType:objectMbr:
     C                                      error)
     C*
     C                   eval      *in60 = error
     C*
     C                   return    not *in60

     PchkFileMbr       E





     P*=====================================================================
     P* clrFileMbr - Clear file
     P*  Parameter 1 - file name   (10a)
     P*  Parameter 2 - member name (10a) {optional}
     P*  Returns - *on if an error occurred, otherwise *off
     P*=====================================================================
     PclrFileMbr       B                   Export
     D
     DclrFileMbr       PI             1
     D fileName                      10    const
     D memberName                    10    const options(*nopass)
     D
     D member          s             10    inz('*FIRST')
     D error           s              1    inz('0')

     C                   if        %parms >= 2 and memberName <> *blanks
     C                   eval      member = memberName
     C                   endif
     C*
     C                   callp     runCmd('CLRPFM FILE('
     C                                   + %trim(fileName) + ') MBR('
     C                                   + %trim(member)   + ')':255)
     C*
     C                   return    error

     PclrFileMbr       E


      //***  Begin Add  ***  01/14/19  ***********************************
     P*=====================================================================
     P* CreateAliasSQL - Create an alias over physical file members to be
     P*                   used by SQL
     P*  Parameter 1 - alias (21a) (name only or lib/file)
     P*  Parameter 2 - file (21a) (file only or lib/file)
     P*  Parameter 3 - member (10a) (member name)
     P*  Parameter 4 - create member (1n) (*ON, *OFF) {optional, *ON default}
     P*  Parameter 5 - Check file lock (1n) (*ON, *OFF) {optional, *ON default}
     P*  Returns - *on if the command is successful, otherwise *off
     P*=====================================================================
     PCreateAliasSQL...
     P                 b                   export
     DCreateAliasSQL...
     D                 pi             1n
     D inAlias                       21    const varying
     D inFile                        21    const varying
     D inMember                      10    const varying
     D inCrtMbr                       1n   const         options(*nopass)
      *** Begin Add *** 08/04/21 ****************************************
     D inChkLock                      1n   const         options(*nopass)
      *** End   Add *** 08/04/21 ****************************************

     DdecPos           s              5  0 inz
     DinFileLib        s             10    inz
     DisCrtMbr         s              1n   inz('1')
      //*** Begin Add *** 08/04/21 ***************************************
     DisChkLock        s              1n   inz('1')
      //*** End   Add *** 08/04/21 ***************************************
     DoutFile          s             10    inz
     DstrAlias         s             10    inz varying
     DstrAliasLib      s             10    inz varying
     DstrDesc          s             40    inz varying
     DstrFile          s             10    inz varying
     DstrFileLib       s             10    inz varying
     DstrSql           s            500    inz varying
      /Free
      *** Begin Add *** 08/04/21 ****************************************
       If %parms >= 5;
          isChkLock = inChkLock;
       Endif;
      //*** End   Add *** 08/04/21 ***************************************
       If %parms >= 4;
          isCrtMbr = inCrtMbr;
       Endif;

       // Break out file information
       decPos = %scan('/':inFile:1);
       strFile = %subst(inFile:decPos+1);
       Select;
       When decPos <> *zero;
          strFileLib = %subst(inFile:1:decPos-1);
       Other;
          outFile = strFile;
          GetObjLib(outFile:'*FILE':inFileLib);
          strFileLib = %trim(inFileLib);
       Endsl;

       // Break out alias information
       decPos = %scan('/':inAlias:1);
       strAlias = %subst(inAlias:decPos+1);
       Select;
       When decPos <> *zero;
          strAliasLib = %subst(inAlias:1:decPos-1);
       Other;
          strAliasLib = strFileLib;
       Endsl;

       //*** Begin Change *** 08/04/21 ********************************
       //If ChkFileLock(strFileLib:strFile) = *on;
       If isChkLock = *On and ChkFileLock(strFileLib:strFile) = *on;
       //*** End   Change *** 08/04/21 ********************************
          Return *off;
       Endif;

       strDesc = 'SQL alias over ' + strFileLib + '/' + strFile +
        '(' + inMember + ')';

       strSql = 'CREATE ALIAS ' + strAliasLib + '/' + strAlias +
        ' FOR ' + strFileLib + '/' + strFile + ' ('+ inMember + ')';
       Exec sql execute immediate :strSql;
       // Only update the desciption if it was just created.
       Select;
       When sqlstt = '42710';
          isCrtMbr = *off;
       When sqlstt = '00000';
          strSql = 'LABEL ON ALIAS ' + strAliasLib + '/' + strAlias +
           ' IS ''' + strDesc + '''';
          Exec sql execute immediate :strSql;
       Endsl;

       Monitor;
          If isCrtMbr;
             AddFileMbr(strFile:inMember);
          Endif;
       On-error;
          Return *off;
       Endmon;

       Return *on;
      /End-free
     PCreateAliasSQL...
     P                 e


      //***  Begin Add  ***  12/06/18  ***********************************
     P*=====================================================================
     P* DspFileLayout - Display file layout (DSPFFD)
     P*  Parameter 1 - file (21a) (file only or lib/file)
     P*  Parameter 2 - output (10a) (*PRINT, *OUTFILE) {optional, *PRINT default}
     P*  Parameter 3 - output file (21a) (file only or lib/file)
     P*  Parameter 4 - output file option (10a) (*REPLACE, *ADD) {optional, *REPLACE default}
     P*  Returns - *on if the command is successful, otherwise *off
     P*=====================================================================
     PDspFileLayout    b                   export
     DDspFileLayout    pi             1n
     D inFile                        21    const varying
     D inOutType                     10    const varying options(*nopass)
     D inOutFile                     21    const varying options(*nopass)
     D inOutFileOpt                  10    const varying options(*nopass)
     D*
     DstrCmd           s            500    varying inz
     DstrOutFile       s             21    varying inz
     DstrOutFileOpt    s             10    varying inz('*REPLACE')
     DstrOutType       s             10    varying inz('*PRINT')
      /Free
       If %parms >= 2 and inOutType <> *blanks;
          strOutType = inOutType;
       Endif;
       If %parms >= 3 and inOutFile <> *blanks;
          strOutFile = inOutFile;
       Endif;
       If %parms >= 4 and inOutFileOpt <> *blanks;
          strOutFileOpt = inOutFileOpt;
       Endif;
       strCmd = 'DSPFFD FILE(' + inFile + ')' +
        ' OUTPUT(' + strOutType + ')';
       If strOutType = '*OUTFILE';
          strCmd += ' OUTFILE(' + strOutFile + ')' +
           ' OUTMBR(*FIRST ' + strOutFileOpt + ')';
       Endif;
       Monitor;
          RunCmd(strCmd:%len(strCmd));
          Return *on;
       On-error;
          Return *off;
       Endmon;
      /End-free
     PDspFileLayout    e
      //***  End   Add  ***  12/06/18  ***********************************



      //***  Begin Add  ***  04/04/17  ************************************
     P*=====================================================================
     P* Get file field description
     P*  Parameter 1 - field       (10a)
     P*  Parameter 2 - file name   (10a)
     P*  Parameter 3 - library name   (10a)
     P*  Parameter 4 - include field in description (10a) {*BEFORE,*AFTER}
     P*  Parameter 5 - delimiter (1a) {-,(,)}
     P*  Returns - description of field (with field name)
     P*=====================================================================
     PGetFieldDesc     b                   export
     DGetFieldDesc     pi           150
     D inField                       10    const
     D inFile                        10    const
     D inLib                         10    const
     D inInclude                     10    const options(*nopass)
     D inDelimiter                    1    const options(*nopass)

     DoutDesc          s            150
     DstrDelimiter     s              1    inz('-')
     DstrInclude       s             10
     DstrSqlCursor     s           5000
      /Free
       If %parms >= 4 and inInclude <> *blanks;
          strInclude = inInclude;
       Endif;
       If %parms >= 5 and inDelimiter <> *blanks;
          strDelimiter = inDelimiter;
       Endif;
       outDesc = %trim(inField);
       strSqlCursor = 'select SYS_CNAME, SYS_TNAME, SYS_DNAME, LABELTEXT ' +
        'from SYSCOLUMNS ' +
        'where SYS_CNAME = ''' + %trim(inField) + '''' +
        '  and SYS_TNAME = ''' + %trim(inFile)  + '''' +
        '  and SYS_DNAME = ''' + %trim(inLib)   + '''';
       Exec SQL Prepare sq1Field From :strSqlCursor;
       Exec SQL Declare cursorField Cursor For sq1Field;
       Exec SQL Open cursorField;
       Exec SQL Fetch next From cursorField into :dsField;
       Select;
       When SQLSTT <> '00000';
          Return outDesc;
       Endsl;
       outDesc = dsField.desc;
       Exec SQL Close cursorField;
       Select;
       When strInclude = '*BEFORE';
          outDesc = AppendInfo(inField:outDesc:strDelimiter);
       When strInclude = '*AFTER';
          outDesc = AppendInfo(outDesc:inField:strDelimiter);
       Endsl;
       Return outDesc;
      /End-free
     PGetFieldDesc     e





     P*=====================================================================
     P* Get file description
     P*  Parameter 1 - file name   (10a)
     P*  Parameter 2 - library name   (10a)
     P*  Parameter 3 - include file in description (10a) {*BEFORE,*AFTER}
     P*  Parameter 4 - delimiter (1a) {-,(,)}
     P*  Returns - description of file (with file name)
     P*=====================================================================
     PGetFileDesc      b                   export
     DGetFileDesc      pi           150
     D inFile                        10    const
     D inLib                         10    const
     D inInclude                     10    const options(*nopass)
     D inDelimiter                    1    const options(*nopass)

     DoutDesc          s            150
     DstrDelimiter     s              1    inz('-')
     DstrFile          s             21
     DstrInclude       s             10
     DstrSqlCursor     s           5000
      /Free
       If %parms >= 3 and inInclude <> *blanks;
          strInclude = inInclude;
       Endif;
       If %parms >= 4 and inDelimiter <> *blanks;
          strDelimiter = inDelimiter;
       Endif;
       outDesc = %trim(inFile);
       strSqlCursor = 'select SYS_TNAME, SYS_DNAME, LABEL ' +
        'from SYSTABLES ' +
        'where SYS_TNAME = ''' + %trim(inFile)  + '''' +
        '  and SYS_DNAME = ''' + %trim(inLib)   + '''';
       Exec SQL Prepare sq1File From :strSqlCursor;
       Exec SQL Declare cursorFile Cursor For sq1File;
       Exec SQL Open cursorFile;
       Exec SQL Fetch next From cursorFile into :dsFile;
       Select;
       When SQLSTT <> '00000';
          Return outDesc;
       Endsl;
       outDesc = dsFile.desc;
       strFile = %trim(dsFile.lib) + '/' + %trim(dsFile.file);
       Exec SQL Close cursorFile;
       Select;
       When strInclude = '*BEFORE';
          outDesc = AppendInfo(strFile:outDesc:strDelimiter);
       When strInclude = '*AFTER';
          outDesc = AppendInfo(outDesc:strFile:strDelimiter);
       Endsl;
       Return outDesc;
      /End-free
     PGetFileDesc      e
      //***  End   Add  ***  04/04/17  ************************************





     P*=====================================================================
     P* dltOvr - Delete file override
     P*  Parameter 1 - file name   (10a)
     P*  Returns - *on if an error occurred, otherwise *off
     P*=====================================================================
     PdltOvr           B                   Export
     D
     DdltOvr           PI             1
     D fileName                      10    const
     D
     D error           s              1    inz('0')

     C                   callp     runCmd('DLTOVR FILE('
     C                                   + %trim(fileName) + ')':255)
     C*
     C                   return    error

     PdltOvr           E


      //***  Begin Add  ***  02/01/19  ********************************
     P*=====================================================================
     P* Deterimine if the record meets criteria for exiting or EOF
     P*  Parameter 1 - eof (1n)
     P*  Parameter 2 - status (1a) {optional}
     P*  Parameter 3 - expiration date (7.0) {optional}
     P*  Parameter 4 - as of date (7.0) {optional}
     P*  Returns - *on no more records should be read
     P*=====================================================================
     PLeaveFileRead    b                   export
     D
     DLeaveFileRead    pi             1n
     D inEof                          1n   const
     D inStatus                       1a   const options(*nopass)
     D inExpDate                      7  0 const options(*nopass)
     D inAsOfDate                     7  0 const options(*nopass)

     DisLeaveRead      s              1n   inz('0')

      /Free
       Select;
       When inEof;
          Return *on;
       When %parms >= 2 and inStatus = 'I';
          Return *off;
       When %parms >= 3 and inExpDate = *zero;
          Return *on;
       When %parms >= 4 and inAsOfDate <= inExpDate;
          Return *on;
       Endsl;
       Return *off;
      /End-free

     PLeaveFileRead    e
      //***  End   Add  ***  02/01/19  ********************************





     P*=====================================================================
     P* ovrFile  - Override file
     P*  Parameter 1 - file name               (10a)
     P*  Parameter 2 - overriding member name  (10a)
     P*  Parameter 3 - overriding file name    (10a) {optional}
     P*  Parameter 4 - overriding file library (10a) {optional}
     P*  Returns - *on if an error occurred, otherwise *off
     P*=====================================================================
     PovrFile          B                   Export
     D
     DovrFile          PI             1
     D fileName                      10    const
     D memberName                    10    const
     D ovrFile                       10    const options(*nopass)
     D ovrLib                        10    const options(*nopass)
     D
     D newLib          s             10    inz('*LIBL')
     D member          s             10    inz('*FIRST')
     D toFile          s             21    inz('*FILE')
     D error           s              1    inz('0')

     C                   if        %parms >= 4 and ovrLib <> *blanks
     C                   eval      newLib = ovrLib
     C                   endif
     C*
     C                   if        %parms >= 3 and ovrFile <> *blanks
     C                   eval      toFile = %trim(newLib)  + '/'
     C                                    + %trim(ovrFile)
     C                   endif
     C*
     C                   if        %parms >= 2 and memberName <> *blanks
     C                   eval      member = %trim(memberName)
     C                   endif
     C*
     C                   callp     runCmd('OVRDBF FILE('
     C                                  + %trim(fileName) + ') TOFILE('
     C                                  + %trim(toFile)   + ') MBR('
     C                                  + %trim(member)   + ')':255)
     C*
     C                   return    error

     PovrFile          E


     P*=====================================================================
     P* bldMember - returns name for a member
     P*  Parameter 1 - prefix                  (1a)
     P*  Parameter 2 - format                  (1a)
     P*  Parameter 3 - suffix                  (1a)
     P*  Returns - a member name of 10 char
     P*  P = prefix, M = middle, S = suffix
     P* example : PMMMMMMMSS
     P*=====================================================================
     PbldMember        B                   Export
     D
     DbldMember        PI            10
     D prefix                         1    const
     D format                         1    const
     D suffix                         1    const

     D mbrname         s             10    inz

     D prfx            s              1    inz
     D frmt            s              1    inz
     D sfx             s              1    inz

     D xxprfx          s              1    inz
     D xxfrmt          s              7    inz
     D xxsfx           s              2    inz

      /free
        if %parms >= 3 and suffix <> *blanks;
          sfx = suffix;
        else;
          sfx = *blanks;
        endif;

        if %parms >= 2 and format <> *blanks;
          frmt = format;
        else;
          frmt = *blanks;
        endif;

        if %parms >= 1 and prefix <> *blanks;
          prfx = prefix;
        else;
          prfx = *blanks;
        endif;

        // default values in case they are not passed

        if sfx = *blanks;
          xxsfx = '00';
        endif;

        if prfx = *blanks;
          xxprfx = 'B';
        endif;

        if frmt = *blanks;
          frmt = 'J'; // julian
        endif;

        // build format
        select;

         when frmt = 'J';
          xxfrmt = %char(%Date() : *LongJul0);

        other;
        endsl;

        mbrname = xxprfx + xxfrmt + xxsfx;

        return mbrname;
      /end-free

     Pbldmember        E
