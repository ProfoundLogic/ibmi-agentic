     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H BndDir('STBNDDIR')
    H*
    H/TITLE   SYAOCVTR Convert files and selected funds
    H*=========================================================================
    H*                   = WORKERS COMPENSATION =
    H*=========================================================================
    H*
    H*                       DESCRIPTION:
    H*  Called by SYAOCVT
    H*
    H*  If AtriumUsersPDS does not run, at least one PDS* user (you) will need
     H*   to be added to ATUSERSP and ATUIDSP needs to be incremented.
    H* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    H*=========================================================================
      * Revisions:
      *
      * 01/10/22  C:33  Support: 97725
      *                 -Skip Scriptura files
      *
      * 09/20/21  C:33  Support: 97862
      *                 -Always set to process "DTM"
      *
      * 08/16/21  C:33  Project: 129WWJ
      *                 -Do not process SXFAP
      *
      * 03/01/19  C:62  Project: 152WAD
      *                 -Add flag to determine if going to copy Q Files
      *                 -Add flag to determine run type
      *                 -Add flag to determine copy all payees
      *                 -Add flag to determine copy all ss#s
      *
      * 10/02/18  C:79  Support: 99/70482
      *                 Do not duplicate triggers on crtdupobj
      *
      * 09/15/17  C:33  Project: 146WGV
      *                -Update AO flag (SYFL11 pos 2)
      *                -Inactive user parm records with no x-ref
      *                -Copy PDS* user parms
      *                -Clear production files
      *
      * 08/28/17  C:33  Project: 146WGV
      *                 Convert SDCMP records
      *
      * 07/31/17  C:33  Project: 146WGV
      *                 Convert users in USER and SMSLP
    H*=========================================================================
    H*
    F*=========================================================================
    F*        F i l e s
    F*=========================================================================
     FQAFDPHY   IF   E             DISK    extfile(@QAFDPHY) usropn
     f
    F*
     FSXMBRLSTL IF   E           K DISK
    F*
    F* Files With Members
    F*
     FWXGSL     IF   E           K DISK
    F*
    F* Approved File List
    F*
     FSXFAL     IF   E           K DISK
    F*
    F* Groups to Convert
    F*
     FSXFIPL    IF   E           K DISK
    F*
    F* Files to Copy With Group
    F*
     FWMUPP     UF   E             DISK    rename(pup01:wpup) usropn
    F*
    F* User Parameters File
    F*
     FSMUPP     UF   E             DISK    rename(pup01:spup) usropn
     F                                     prefix('S')
    F*
    F* User Parameters File
    F*
     F*SDCMP     UF   E             DISK                       usropn
    F*
    F* Remark Detail File
    F*
     FFAEAP     UF   E             DISK                       usropn
    F*
    F* EOMB Header Amount Transaction File
    F*
     FFAEOP     UF   E             DISK                       usropn
    F*
    F* EOMB Header Amount Transaction File
    F*
     FSXUPL     IF   E           K DISK                       usropn
    F*
    F* User Parameter Transition Table by User
    F*
     FWMBIP     UF   E             DISK                       usropn
    F*
    F* MICR Bank Information File
    F*
     FSXORL     IF   E           K DISK                       usropn
    F*
    F* OutQ Transition Table by old outq new outq
    F*
     FSSYPF     UF   E             DISK                       usropn
    F*
    F* Application System Control File
    F*
     FSSYPFSAVE IF   E             DISK    rename(ssy01:ssy) prefix('Y')
     F                                     usropn
    F*
    F* WCMS User File
    F*
     Fsmwup     UF   E             DISK    Rename(pwu01:pwu0p) usropn
    F*
    F* Employer A/R File
    F*
     Fwmeap     UF   E             DISK    Rename(pea01:pea0p) usropn
     F                                     Prefix(E2:2)
    F*
    F* Application System Control File Saved Version
    F*
    D*
     D@QAFDPHY         c                   'QTEMP/QAFDPHY'
     D appFileLst      s               n   Inz(*Off)
     D approvedFile    s               n   Inz(*Off)
     d fileKey         s             10    Inz(' ')
     D xxloc           s              3  0
     D sfx             s              3
     D q               s              1
     D fund            s              3
     D name            s             10
     D type            s              7    inz('*FILE')
     D error           s              1
     D inCmd           s           2750
     D inCmd2          s           2750
     D inCmdLen        s             15  5
      //***  Begin Add  ***  08/13/21  **************
     D isEofSMUPP      s              1n   inz
      //***  End   Add  ***  08/13/21  **************
     D isProcess       s              1n
     D nextClm#        s              7  0
     D nextEmp#        s              9  0
     D nextQte#        s              9  0
     D nextInv#        s             10  0
     D strCopy         s           2750
     D strFlrPrefix    s              8    inz
     D strIncRel       s             50
     D strFmtOpt       s             50
     D strMbr          s            100
     D strPrefixChk    s             10    inz
     D strRoot         s             10    inz('/DOCROOT')
     D strRootChk      s             12    inz
      //***  Begin Add  ***  08/13/21  **************
     D strSkipFile     s             10    inz
      //***  End   Add  ***  08/13/21  **************
     D strSql          s            500
     d psds          esds                  extname(szq1p)
    D*
     D                 DS
     D  suffix                 1      3
     D  l08                    1      1
     D  l09                    2      2
     D  l10                    3      3

      //***  Begin Add  ***  08/13/21  **************
     ddsSMUPP        e ds                  extname(SMUPP) inz qualified
      //***  End   Add  ***  08/13/21  **************

     DdsLDA            ds                  dtaara(*LDA)
     D inSuffix                       3
     D inFiles                       10
     D inPDSFIL                      10
     D inPDSMFIL                     10
     D inPDSQTE                      10
     D inPDSMQTE                     10
     D inPDSCLT                      10
     D inPDSMTH                      10
     D inCopy                         4
     D inFromObjLib                  10
     D inFromInstance                10
     D inToInstance                  10
     D inBrowserTitle               130
     D inCopyPdsLib                  10
     D inSavfLib                     10
     D inClrFiles                     4
     D inSkipQFiles                   4
     D inRunType                      4
     D inAllPayees                    4
     D inAllSS#                       4
     D inSkipFiles                  100
    D*
     DRunCmd           pr                  extPgm('QCMDEXC')
     D inCmd                       2750    const
     D inCmdLen                      15  5 const
    D*
     DCheckObj         pr                  extPgm('SRCHKOB2')
     D lib                           10
     D name                          10
     D type                           7
     D error                          1
     D*
     D RtvDocRoot      pr                  ExtPgm('RTVDOCROOT')
     D  rootDir                      10

      //***  Begin Add  ***  01/10/22  **************
     d/copy sPrFiles
      //***  End   Add  ***  01/10/22  **************
      //***  Begin Add  ***  08/13/21  **************
     d/copy sPrSql
      //***  End   Add  ***  08/13/21  **************
    D*
    C*=========================================================
     C     *LIKE         DEFINE    mblib         tolib
     C     *LIKE         DEFINE    mblib         fmlib
     C     *LIKE         DEFINE    mbfile        file
    C*=========================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@SUFFIX          3
    C*=========================================================

     /free
       Exec SQL set option COMMIT=*NONE;

       //***  Begin Add  ***  01/10/22  **************
       CreateAliasSQL('QTEMP/SYAOCVTSKP':'QTXTSRC':'SYAOCVTSKP':*off);
       //***  End   Add  ***  01/10/22  **************

       // Check to see if we are going to use the approved file list
       appFileLst = *Off;
       SetLL fileKey fap01;
       Read fap01;
       If Not %Eof(sxfal);
          appFileLst = *On;
       EndIf;

       In dsLDA;
       suffix = @@suffix;

       If inClrFiles = '*YES';
          Exsr ClearFiles;
       Endif;

       // read contents of file/member list
       Setll *loval SXMBRLSTL;
       Read  SXMBRLSTL;

       Dow Not %EOF(SXMBRLSTL);

          // is this file to be processed ?
          Clear FIgFld;
          Clear FIstat;
          Chain MBFILE SXFIPL;
          // If we are using the approved file list then check to see
          // if the file is on the approved list.
          approvedFile = *Off;
          If appFileLst = *On;
             Chain MBFILE fap01;
             If %Found(sxfal);
                approvedFile = *On;
             EndIf;
          EndIf;
          //***  Begin Add  ***  01/10/22  **************
          Reset strSkipFile;
          Exec sql
           select SRCDTA into :strSkipFile
            from QTEMP/SYAOCVTSKP
            where SRCDTA = :MBfile;
          //***  End   Add  ***  01/10/22  **************
          Select;
          When appFileLst = *On And approvedFile = *Off;
             isProcess = *off;
          When %Scan(MBfile:inSkipFiles) <> 0; //Skip file list
             isProcess = *off;
        //***  Begin Add  ***  08/13/21  **********************
          When mbfile = 'SXFAP';
             isProcess = *off;
        //***  End   Add  ***  08/13/21  **********************
          When mbfile = 'SXFIP';
             isProcess = *off;
          When mbfile = 'SXORP';
             isProcess = *off;
          When mbfile = 'SXUPP';
             isProcess = *off;
          When mbfile = 'SSYPFSAVE';
             isProcess = *off;
          When mbfile = 'SXMBRLST';
             isProcess = *off;
          When mbfile = 'WXGSP';
             isProcess = *off;
          When mbfile = 'WXGSP';
             isProcess = *off;
          // process only physical files and not "Q" file types
          When %subst(mbfile:1:1) = 'Q' And inSkipQFiles = '*YES';
             isProcess = *off;
          //***  Begin Add  ***  01/10/22  **************
          When MBfile = strSkipFile;
             isProcess = *off;
          //***  End   Add  ***  01/10/22  **************
          When FIstat = 'I';
             isProcess = *off;
          When inFiles = '*ALL';
             isProcess = *on;
          When %found(SXFIPL);
             isProcess = *on;
          Other;
             isProcess = *off;
          Endsl;

          If isProcess;
             // determine to library name
             Select;
             When %scan('CLT':mblib) <> 0;
                tolib = ('PDSCLT' + %trim(suffix));
                fmlib = inPDSCLT;
             When %scan('MFIL':mblib) <> 0;
                tolib = ('PDSMFIL' + %trim(suffix));
                fmlib = inPDSMFIL;
             When %scan('MQTE':mblib) <> 0;
                tolib = ('PDSMQTE' + %trim(suffix));
                fmlib = inPDSMQTE;
             When %scan('FIL':mblib) <> 0;
                tolib = ('PDSFIL' + %trim(suffix));
                fmlib = inPDSFIL;
             When %scan('QTE':mblib) <> 0;
                tolib = ('PDSQTE' + %trim(suffix));
                fmlib = inPDSQTE;
             When %scan('MTH':mblib) <> 0;
                tolib = ('PDSMTH' + %trim(suffix));
                fmlib = inPDSMTH;
             Endsl;

             // does file existence ?
             Callp CheckObj(tolib:mbfile:type:error);
             If error = '2';
                // create file
                incmdlen = 68;
                If inRunType = '*IMP';
                   incmd = 'CRTDUPOBJ OBJ(' + %trim(mbfile) + ') ' +
                    'FROMLIB(' + %trim(fmlib) + ') ' +
                    'OBJTYPE(*FILE) TRG(*NO) ' +
                    'TOLIB(' + %TRIM(tolib) + ')';
                Else;
                   incmd = 'CRTDUPOBJ OBJ(' + %trim(mbfile) + ') ' +
                    'FROMLIB(' + %trim(fmlib) + ') ' +
                    'OBJTYPE(*FILE) CST(*NO) TRG(*NO) ' +
                    'TOLIB(' + %TRIM(tolib) + ')';
                EndIf;
                RunCmd(inCmd:%len(%trim(inCmd)));
             Else;
                // clear file
                incmdlen = 45;
                incmd = 'CLRPFM FILE(' + %trim(tolib) + '/'  +
                 %trim(mbfile) + ')' +
                 ' MBR(*ALL)';
                Monitor;
                   RunCmd(inCmd:%len(%trim(inCmd)));
                On-error;
                   If q1ExTp + q1ExNm = 'CPF3134';
                      incmd2 = 'RMVPFCST FILE(' + %trim(tolib) + '/'  +
                       %trim(mbfile) + ')' +
                       ' CST(*all) RMVCST(*REMOVE)';
                      RunCmd(inCmd2:%len(%trim(inCmd2)));
                      RunCmd(inCmd:%len(%trim(inCmd)));
                   Endif;
                Endmon;
             Endif;

             strIncRel = ' ';
             // is this file by fund ?
             If FIgFld <> *blanks;

                // proces funds
                Setll *loval WXGSL;
                Dou %eof(WXGSL);
                   Read WXGSL;
                   Select;
                   When %eof;
                      Iter;
                   // process this fund ?
                   When GSiEFl <> 'I';
                      Iter;
                   Endsl;

                   // does file have members ?
                   If MBnoMb > 1;
                      // process each member
                      file = mbfile;
                      Dow not %eof(sxmbrlstl);
                         // build command and run
                         fund = %editc(GSfnd:'X');
                         strCopy = 'CPYF FROMFILE(' +
                          %trim(mblib) +
                          '/'  +
                          %trim(mbfile) +
                          ')' +
                          ' TOFILE(' +
                          %trim(tolib) +
                          '/'  +
                          %trim(mbfile) +
                          ')' +
                          ' FROMMBR(' +
                          %trim(mbname) + ')' +
                          ' TOMBR(' +
                          %trim(mbname) + ')' +
                          ' MBROPT(*REPLACE)';
                         strIncRel =
                          'INCREL((*IF ' +
                          %trim(figfld) +
                          ' *EQ ' +
                          %trim(FUND) +
                          '))';
                         Exsr CopyFile;
                         Reade file sxmbrlstl;
                      Enddo;
                      // position to next file
                      Chain (file:mbname) sxmbrlstl;
                    Else;
                       // build command and run
                       fund = %editc(gsfnd:'X');
                       strCopy = 'CPYF FROMFILE(' +
                        %trim(mblib) +
                        '/'  +
                        %trim(mbfile) +
                        ')' +
                        ' TOFILE(' +
                        %trim(tolib) +
                        '/'  +
                        %trim(mbfile) +
                        ')' +
                        ' MBROPT(*REPLACE)';
                       strIncRel =
                        'INCREL((*IF ' +
                        %trim(figfld) +
                        ' *EQ ' +
                        %trim(FUND) +
                        '))';
                       Exsr CopyFile;
                    Endif;

                 Enddo;

             Else;

                // does file have members ?
                If mbnomb > 1;
                   // process each member
                   file = mbfile;
                   Dow not %eof(sxmbrlstl);
                      // build command and run
                      strCopy = 'CPYF FROMFILE(' +
                       %trim(mblib) +
                       '/'  +
                       %trim(mbfile) +
                       ')' +
                       ' TOFILE(' +
                       %trim(tolib) +
                       '/'  +
                       %trim(mbfile) +
                       ')' +
                       ' FROMMBR(' +
                       %trim(mbname) + ')' +
                       ' TOMBR(' +
                       %trim(mbname) + ')' +
                       ' MBROPT(*ADD)';
                      Exsr CopyFile;
                      Reade file sxmbrlstl;
                   Enddo;
                   // position to next file
                   Chain (file:mbname) sxmbrlstl;
                Else;
                   // build command and run
                   fund = %editc(gsfnd:'X');
                   strCopy = 'CPYF FROMFILE(' +
                    %trim(mblib) +
                    '/'  +
                    %trim(mbfile) +
                    ')' +
                    ' TOFILE(' +
                    %trim(tolib) +
                    '/'  +
                    %trim(mbfile) +
                    ')' +
                    ' MBROPT(*REPLACE)';
                   Exsr CopyFile;
                Endif;

             Endif;

          Endif;

          Read  SXMBRLSTL;

       Enddo;

      // Process User Profiles
       If inRunType = '*IMP';
          Open sxupl;
          Open sxorl;
          Open wmupp;
          Read wmupp;
b01       Dow not %eof(wmupp);
             // wmupp User Profiles
             Chain upusr sxupl;
b02          If %found(sxupl);
                upusr = upnewusr;
             Else;
                UPstat = 'I';
e02          Endif;
             // Get New Output Queue
             Chain upoutq sxorl;
b02          If %found(sxorl);
                upoutq = ornewoutq;
e02          Endif;
             Update  wpup;
             Read wmupp;
e01       Enddo;

          // smupp User Profiles
          Open Smupp;
          Read smupp;
b01       Dow not %eof(smupp);
            // Get New User Profiles
             Chain supusr sxupl;
b02          If %found(sxupl);
                supusr = upnewusr;
             Else;
                sUPstat = 'I';
e02          Endif;
            // Get New Output Queue
             Chain supoutq sxorl;
b02          If %found(sxorl);
                supoutq = ornewoutq;
e02          Endif;
             update  spup;
             Read smupp;
e01       Enddo;

         // faeap user profiles
          Open faeap;
          Read faeap;
          Dow not %eof(faeap);
             // Get New User
             Chain eauser sxupl;
b02          If %found(sxupl);
                eauser = upnewusr;
                update  pea01;
e02          Endif;
             Read faeap;
          Enddo;

          // faeop user profiles
          Open faeop;
          Read faeop;
b01       Dow not %eof(faeop);
             // Get New User Profiles
             Chain eouser sxupl;
b02          If %found(sxupl);
                eouser = upnewusr;
                update  peo01;
e03          Endif;
             read faeop;
e01       Enddo;

          // wmbip output queue
          Open wmbip;
          Read wmbip;
          Dow not %eof(wmbip);
             // Get New output queue
             Chain bioutq sxorl;
b02          If %found(sxorl);
                bioutq = ornewoutq;
e02          Endif;
             Chain bicpyq sxorl;
b02          If %found(sxorl);
                bicpyq = ornewoutq;
e02          Endif;
             update  pbi01;
             read wmbip;
e01       Enddo;

         // smwup user profiles
          Open smwup;
          Read smwup;
          Dow not %eof(smwup);
             // Get New User
             Chain wuusr sxupl;
b02          If %found(sxupl);
                wuusr = upnewusr;
                update  pwu0p;
e02          Endif;
             Read smwup;
          Enddo;

         // wmeap user profiles
          Open wmeap;
          Read wmeap;
          Dow not %eof(wmeap);
             // Get New User
             Chain eauser sxupl;
b02          If %found(sxupl);
                e2user = upnewusr;
                update  pea0p;
e02          Endif;
             Read wmeap;
          Enddo;

          // Update WCMS User File with new user profiles
          Setll *loval sxupl;
          Dou %eof;
             Read sxupl;
             Select;
             When %eof;
                Iter;
             Endsl;
             strSQL = 'update SDRCP' +
              ' set RCUSR = ''' + UPnewUsr + '''' +
              ' where RCUSR = ''' + UPoldUsr + '''';
             Exec SQL execute immediate :strSQL;
             strSQL = 'update SMAZP' +
              ' set AZUSR = ''' + UPnewUsr + '''' +
              ' where AZUSR = ''' + UPoldUsr + '''';
             Exec SQL execute immediate :strSQL;
             strSQL = 'update WMCUP' +
              ' set CUAUSR = ''' + UPnewUsr + '''' +
              ' where CUAUSR = ''' + UPoldUsr + '''';
             Exec SQL execute immediate :strSQL;
             strSQL = 'update USER' +
              ' set ISERIES_PROFILE = ''' + UPnewUsr + '''' +
              ' where ISERIES_PROFILE = ''' + UPoldUsr + '''';
             Exec SQL execute immediate :strSQL;
             strSQL = 'update SMSLP' +
              ' set SLUSRP = ''' + UPnewUsr + '''' +
              ' where SLUSRP = ''' + UPoldUsr + '''';
             Exec SQL execute immediate :strSQL;
             strSQL = 'update SDCMP' +
              ' set CMUSER = ''' + %trim(UPnewUsr) + '''' +
              ' where CMUSER =''' + %trim(UPoldUsr) + '''' +
              ' and CMSTAT <> ''I''';
             Exec SQL execute immediate :strSQL;
          Enddo;

          If inCopyPdsLib <> '*NONE';
             strSQL = 'insert into SMUPP' +
              ' select a.* ' +
              '  from ' + %trim(inCopyPdsLib) + '/SMUPP a ' +
              '  Exception Join SMUPP b On a.UPusr = b.UPusr ' +
              '  where a.UPUSR like ''PDS%'' and a.UPSTAT <> ''I''' +
              '   and a.UPUSR <> ''PDSPGMR''';
             Exec SQL execute immediate :strSQL;
             strSQL = 'insert into WMUPP' +
              ' select a.* ' +
              '  from ' + %trim(inCopyPdsLib) + '/WMUPP a ' +
              '  Exception Join WMUPP b On a.UPusr = b.UPusr ' +
              '  where a.UPUSR like ''PDS%'' and a.UPSTAT <> ''I''' +
              '   and a.UPUSR <> ''PDSPGMR''';
             Exec SQL execute immediate :strSQL;
             strSQL = 'insert into SMSLP' +
              ' select a.* ' +
              '  from ' + %trim(inCopyPdsLib) + '/SMSLP a ' +
              '  Exception Join WMSLP b On a.SLusrP = b.SLusrP ' +
              '  where a.SLUSRP like ''PDS%'' and a.SLSTAT <> ''I''' +
              '   and a.SLUSRP <> ''PDSPGMR''';
             Exec SQL execute immediate :strSQL;
             strSQL = 'insert into USER' +
              ' select a.* ' +
              '  from ' + %trim(inCopyPdsLib) + '/USER a ' +
              '  Exception Join USER b On ' +
              '  a.ISERIES_PROFILE = b.ISERIES_PROFILE ' +
              '  where a.ISERIES_PROFILE like ''PDS%'' and a.STATUS <> ''I''' +
              '   and ISERIES_PROFILE <> ''PDSPGMR''';
             Exec SQL execute immediate :strSQL;
           //***  Begin Add  ***  08/13/21  *******************
             Exsr AtriumUsersPDS;
           //***  End   Add  ***  08/13/21  *******************
          Endif;

          strFlrPrefix = '/PDS/' + suffix;
          strPrefixChk = '%' + %trim(strFlrPrefix) + '%';
          strRootChk = '%' + %trim(strRoot) + '%';
          strSQL = 'update SDCMP ' +
           ' set CMUNCN = replace(CMUNCN, ''' + %trim(strRoot) + '''' +
           '  , ''' + %trim(strRoot) + %trim(strFlrPrefix) + ''')' +
           ' where CMUNCN <> '' ''' +
           '  and CMUNCN not like(''' + %trim(strPrefixChk) + ''')' +
           '  and CMUNCN like (''' + %trim(strRootChk) + ''')' +
           ' with NC';
          Exec SQL execute immediate :strSQL;
          strSQL = 'update SMMDP ' +
           ' set MDUNCP = replace(MDUNCP, ''' + %trim(strRoot) + '''' +
           '  , ''' + %trim(strRoot) + %trim(strFlrPrefix) + ''')' +
           ' where MDUNCP <> '' ''' +
           '  and MDUNCP not like(''' + %trim(strPrefixChk) + ''')' +
           '  and MDUNCP like (''' + %trim(strRootChk) + ''')' +
           ' with NC';
          Exec SQL execute immediate :strSQL;
          strSQL = 'update IMDOP ' +
           ' set DOUNCN = replace(DOUNCN, ''' + %trim(strRoot) + '''' +
           '  , ''' + %trim(strRoot) + %trim(strFlrPrefix) + ''')' +
           ' where DOUNCN <> '' ''' +
           '  and DOUNCN not like(''' + %trim(strPrefixChk) + ''')' +
           '  and DOUNCN like (''' + %trim(strRootChk) + ''')' +
           ' with NC';
          Exec SQL execute immediate :strSQL;

          // Update Application System Control File
          Open ssypf;
          Open ssypfsave;
          Read ssypf;
          Read ssypfsave;
          syal01 = ysyal01;
          syal02 = ysyal02;
          syal03 = ysyal03;
        //***  Begin Chg  ***  09/20/21  *************
        //syfl07 = ysyfl07;
          syfl07 = 'D';
        //***  End   Chg  ***  09/20/21  *************
          syfl08 = %subst(suffix:1:1);
          syfl09 = %subst(suffix:2:1);
          syfl10 = %subst(suffix:3:1);
          %subst(syfl11:2:1) = 'Y';
          update  ssy01;
       EndIf;

       // Update WMFNP
       tolib = ('PDSFIL' + %trim(suffix));
       fmlib = inPDSFIL;
       Setll *loval WXGSL;
       Read WXGSL;
       DoW Not %EOF(WXGSL);
          Read WXGSL;
          If GSieFL = 'I';
             // Create Alias
             strSQL = 'Create Alias QTEMP/WMFNP_FRM ' +
                'For ' + %trim(fmLIb) + '/WMFNP(WMFNP)';
             // Let's get the next available counters from convert library
             Exec SQL Select FNnacs, FNnaen, FNnaqn, FNninv
                Into :nextClm#, :nextEmp#, :nextQte#, :nextInv#
                From WMFNP_FRM
                Where FNfnd = :GSfnd;
             // Now let's apply the counter values to the production library
             strSQL = 'Update ' + %trim(toLIb) + '/WMFNP ' +
                'Set FNnacs = ' + %EditC(nextClm#:'X') + ', ' +
                '    FNnaen = ' + %EditC(nextEmp#:'X') + ', ' +
                '    FNnaqn = ' + %EditC(nextQte#:'X') + ', ' +
                '    FNninv = ' + %EditC(nextInv#:'X') + ' ' +
                'Where FNfnd = ' + %EditC(GSfnd:'X');
             Exec SQL execute immediate :strSQL;
          EndIf;
          Read WXGSL;
       EndDo;

       // Delete all SS# not used by a claim
       // WCMS
       If inAllSS# = '*NO';
          strSQL = 'Delete From ' + %Trim(toLib) + '/WMCDP ' +
                   'Where CDss# not in (Select CMss# ' +
                   'From ' + %trim(toLib) + '/WMCMP ' +
                   'Group by CMss#)';
          Exec SQL execute immediate :strSQL;
          // WCMC
          strSQL = 'Delete From ' + %Trim(toLib) + '/FMCDP ' +
                   'Where CDss# not in (Select CMss# ' +
                   'From ' + %trim(toLib) + '/FMCMP ' +
                   'Group by CMss#)';
          Exec SQL execute immediate :strSQL;
       EndIf;

       // Delete all Payee's that aren't being used as payment or eob
       If inAllPayees = '*NO';
          // WCMS
          strSQL =  'delete ' +
                    'from ' + %trim(toLib) + '/wmpdp a ' +
                    'where Not ((a.pdfein in (select b.pafein ' +
                    'from ' + %trim(toLib) + '/wdpap b ' +
                    'where a.pdmod=b.pamod)) Or ' +
                    '(a.pdfein in (select c.eafein ' +
                    'from ' + %trim(toLib) + '/faeap c ' +
                    'where a.pdmod=c.eamod)) Or ' +
                    '(a.pdfein in (select d.pafein ' +
                    'from ' + %trim(toLib) + '/wdpap d ' +
                    'join ' + %trim(toLib) + '/wmpdp f ' +
                    'on d.pafein = f.pdfein and d.pamod = f.pdmod ' +
                    'where a.pdmod=f.pd109m)) Or ' +
                    '(a.pdfein in (select e.eafein ' +
                    'from ' + %trim(toLib) + '/faeap e ' +
                    'join ' + %trim(toLib) + '/wmpdp g ' +
                    'on e.eafein = g.pdfein and e.eamod = g.pdmod ' +
                    'where a.pdmod=g.pd109m)) Or ' +
                    '(substr(a.pdname,1,4) =''SBBC''))';
          Exec SQL execute immediate :strSQL;
          // WCMC
          strSQL = 'Delete from ' + %trim(toLib) + '/FMPDP a ' +
                   'Where a.PDfein not In (Select b.PDfein ' +
                   'From ' + %trim(toLib) + '/WMPDP b ' +
                   'Where a.PDmod = b.PDmod)';
          Exec SQL execute immediate :strSQL;
          strSQL = 'Delete from ' + %trim(toLib) + '/FMP1P a ' +
                   'Where a.P1fein not In (Select b.PDfein ' +
                   'From ' + %trim(toLib) + '/WMPDP b ' +
                   'Where a.P1mod = b.PDmod)';
          Exec SQL execute immediate :strSQL;
       EndIf;

       *inlr = *on;

       //***  Begin Add  ***  08/13/21  **************
       //===============================================================
       Begsr AtriumUsersPDS;
        // This needs work/consideration before removing LEAVESR
        Leavesr;
        strSql = 'select * from SMUPP where UPusr like (''PDS%'')';
        Exec sql close cursorSMUPP;
        Exec sql prepare sqlSMUPP from :strSql;
        Exec sql declare cursorSMUPP cursor for sqlSMUPP;
        Exec sql open cursorSMUPP;
        Reset isEofSMUPP;
        Dou isEofSMUPP;
           Reset dsSMUPP;
           Exec sql fetch next from cursorSMUPP into :dsSMUPP;
           Select;
           When not IsValidFetch(sqlstt);
              isEofSMUPP = *on;
              Iter;
           Endsl;
           strSQL = 'insert into ' + %trim(inToInstance) + '.ATUSERSP' +
            ' (AUUSER, AUNAME, AUPROF)' +
            ' select (select max(AUUSER)+1' +
            '  from ' + %trim(inToInstance) + '.ATUSERSP),' +
            ' trim(UPUSNM)||'' (Go)'', UPUSR from '+%trim(inPDSFIL)+'.SMUPP' +
            ' where UPusr = ''' + %trim(dsSMUPP.UPusr) + '''';
           Exec SQL execute immediate :strSQL;
           If sqlstt = '00000';
              strSQL = 'update ' + %trim(inToInstance) + '.ATUSERSP t' +
               ' set (t.AUGROUP, t.AUROLE, t.AUEDIT, t.AULANG) = ' +
               ' (select f.AUGROUP, f.AUROLE, f.AUEDIT, f.AULANG' +
               '   from ' + %trim(inToInstance) + '.ATUSERSP f' +
               '   where f.AUPROF = ''PDSPGMR'')' +
               '  where t.AUPROF = ''GRPUSRID''';
              Exec SQL execute immediate :strSQL;
              strSQL = 'update ' + %trim(inToInstance) + '.ATUIDSP' +
               ' set AUINext = AUINext +1 ' +
               '  where AUIField = ''GRPUSRID''';
              Exec SQL execute immediate :strSQL;
           Endif;
        Enddo;
       Endsr;
       //***  End   Add  ***  08/13/21  **************

       //===============================================================
       Begsr ClearFiles;
        Open QAFDPHY;
        Dou %eof;
           Read QAFDPHY;
           Clear FIstat;
           Chain PHfile SXFIPL;
           Select;
           When %eof;
              Iter;
           When FIstat = 'I';
              Iter;
           Endsl;
           inCmd = 'CLRPFM FILE(' + %trim(PHlib) + '/' + %trim(PHfile) +
            ') MBR(*ALL)';
           RunCmd(inCmd:%len(%trim(inCmd)));
        Enddo;
       Endsr;

       //===============================================================
       Begsr CopyFile;
        strFmtOpt = 'FMTOPT(*MAP *DROP)';
        If strIncRel = ' ';
           inCmd = %trim(strCopy) + ' ' + %trim(strFmtOpt);
        Else;
           inCmd = %trim(strCopy) + ' ' + %trim(strIncRel) +
                   ' ' + %trim(strFmtOpt);
        EndIf;
        Monitor;
           RunCmd(inCmd:%len(%trim(inCmd)));
        On-error;
           strFmtOpt = 'FMTOPT(*NOCHK)';
           inCmd = %trim(strCopy) + ' ' + %trim(strFmtOpt);
           Monitor;
              RunCmd(inCmd:%len(%trim(inCmd)));
           On-error;
              inCmd = %trim(strCopy) + ' ' + %trim(strFmtOpt);
              Monitor;
                 RunCmd(inCmd:%len(%trim(inCmd)));
              On-Error;
                 // Keep going
              EndMon;
           Endmon;
        Endmon;
       Endsr;

     /end-free
