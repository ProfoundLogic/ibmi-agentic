     hCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     h/Title SyRgzPfm - Remove Deleted Records
     h DftActGrp(*No) ActGrp(*Caller)
     h Option(*nodebugio: *srcstmt)
     h BndDir('STBNDDIR')
      //===============================================================
      // Identifies and reorganizes files to clear deleted records
      //  Called from SYRGZPFM command.
      //  SYRGZPFM is called from Daily Batch.
      //===============================================================
      /Copy sPrRunCmd
      /Copy sPrSql
      /Free
       // *Entry
       Dcl-Pr SyRgzPfm ExtPgm('SYRGZPFM');
        inLib Char(10);
        inFile Char(10);
        inTime Char(10);
       End-Pr;
       Dcl-Pi SyRgzPfm;
        inLib Char(10);
        inFile Char(10);
        inTime Char(10);
       End-Pi;

       Dcl-Pr RtvSuffix ExtPgm('RTVSUFFIX');
        chrSuffix Char(3);
       End-Pr;

       Dcl-S curTime TimeStamp;
       Dcl-S endTime TimeStamp;
       Dcl-S intCount Zoned(1:0);
       Dcl-S sqlString Char(1000);
       Dcl-S chrSuffix Char(3);
       Dcl-S strFile Char(10);
       Dcl-S strFileLib Char(10);
       Dcl-S strDltRLib Char(10);

       Dcl-Ds dsDltRecList;
        dsFile Char(10);
        dsMember Char(10);
        dsLib  Char(10);
        dsDltSpace Zoned(15:0);
       End-Ds;

      // Calculate the ending time
       If inTime = '*DEFAULT';
          endTime = %TimeStamp() + %Minutes(60);
       Else;
          Monitor;
             endTime = %TimeStamp() + %Minutes(%Int(inTime));
          On-Error;
             Return;
          EndMon;
       Endif;

      // Create the outfile library
       RtvSuffix (chrSuffix);
       strDltRLib = 'PDSDLT' + chrSuffix;
       Monitor;
          CallP RunCmd('DltLib Lib(' + %Trim(strDltRLib) + ')':255);
       On-Error;
       EndMon;
       CallP RunCmd('CrtLib Lib(' + %Trim(strDltRLib) + ')':255);

      // Build Outfiles from DSPFD command
       If inFile <> '*ALL';
          strFile = inFile;
       Else;
          strFile = '*ALL';
       EndIf;

       For intCount = 1 to 2;
          Select;
          When inLib <> '*DEFAULT';
             strFileLib = inLib;
          When intCount = 1;
             strFileLib = 'PDSFIL' + chrSuffix;
          When intCount = 2;
             strFileLib = 'PDSQTE' + chrSuffix;
          EndSl;
          Monitor;
             CallP RunCmd('Dspfd File(' + %Trim(strFileLib) + '/' +
              %Trim(strFile) + ') Type(*MBR) +
              Output(*Outfile) FileAtr(*PF) Outfile(' + %Trim(strDltRLib) +
              '/DSPFD_MBR) Outmbr(*FIRST *ADD)':255);
             CallP RunCmd('Dspfd File(' + %Trim(strFileLib) + '/' +
              %Trim(strFile) + ') Type(*ATR) +
              Output(*Outfile) FileAtr(*PF) Outfile(' + %Trim(strDltRLib) +
              '/DSPFD_ATR) OutMbr(*FIRST *ADD)':255);
          On-Error;
             Return;
          EndMon;
         // Leave if processing a specific library
          If inLib <> '*DEFAULT';
             Leave;
          Endif;
       EndFor;

       // Query Outfiles
       sqlString = 'Create Index ' +
        %Trim(strDltRLib) + '.DSPFDATRIX On ' +
        %Trim(strDltRLib) + '.DSPFD_ATR(PHFILE)';
       Exec Sql
        Prepare DynSqlStmt from :sqlString;
       If SqlStt = '00000';
          Exec Sql Execute DynSqlStmt;
       Else;
          Return;
       Endif;
       sqlString = 'Create Index ' +
        %Trim(strDltRLib) + '.DSPFDMBRIX On ' +
        %Trim(strDltRLib) + '.DSPFD_MBR(MBFTYP,MBFILE,MBNDTR)';
       Exec Sql
        Prepare DynSqlStmt from :sqlString;
       If SqlStt = '00000';
          Exec Sql Execute DynSqlStmt;
       Else;
          Return;
       Endif;
       sqlString = 'Call QSYS2.DUMP_PLAN_CACHE(''' +
        %Trim(strDltRLib) + ''', ''PCSS'')';
       Exec Sql
        Prepare DynSqlStmt from :sqlString;
       If SqlStt = '00000';
          Exec Sql Execute DynSqlStmt;
       Else;
          Return;
       Endif;
       sqlString =
        'Select Distinct +
          F.MBFILE As Filename, +
          F.MBNAME As Member, +
          MBLIB AS Library, +
          Integer(Case when MBDSSZ > 0 Then MBDSSZ Else MBDSZ2 End * +
          (MBNDTR/(Case When MBNRCD + MBNDTR = 0 Then 1 +
          Else MBNRCD + MBNDTR End))) as "Deleted Space" +
         From ' + %Trim(strDltRLib) + '.DSPFD_MBR As F +
          Join ' + %Trim(strDltRLib) + '.DSPFD_ATR As A +
          On F.MBFILE = A.PHFILE And F.MBLIB = A.PHLIB +
         Where MBFTYP = ''P'' And MBNDTR > 0 +
         Order By "Deleted Space" Desc';
       Exec SQL
        Prepare sqlDltRecList From :sqlString;
       If SqlStt <> '00000';
          Return;
       Endif;
       Exec SQL
        Declare cursorDltRecList Cursor For sqlDltRecList;
       If SqlStt <> '00000';
          Return;
       Endif;
       Exec Sql
        Open cursorDltRecList;
       Dou Not IsValidFetch(sqlStt);
          Exec Sql
           Fetch Next From cursorDltRecList into :dsDltRecList;
          If Not IsValidFetch(sqlStt);
             Leave;
          Endif;
          CallP RunCmd('RgzPfm File(' +
           %Trim(dsLib) + '/' + %Trim(dsFile) + ') ' +
           'Mbr(' + %Trim(dsMember) + ')':255);
          curTime = %TimeStamp();
          If curTime > endTime;
             Leave;
          EndIf;
       Enddo;
       Return;
      /End-Free
