     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('STBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h*
     h*  Description: Response program to update available libraries for file triggers
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*
     h*  Called from: STAPPCFG
     h*  Calls to:
     h*
     h*  Created on 01/31/2017 by C:33  for WC2405
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
      /copy sPrRunCmd
      /copy sPrAtrium
      /copy sPrDate
      /copy sPrString

      *entry
     DOutLibraries     pr                  ExtPgm('STAPPCFG06')
     D                                9  0
     D                              512
     D                            32767
     D                                1
      *
     DOutLibraries     pi
     D inRRN                          9  0
     D inGenParm                    512
     D outJSON                    32767
     D RtnCode                        1

     DAddChoice        pr
     D inValue                      250    const
     DAddChoiceValue   pr
     D inValue                      250    const

     DRtvObjLib        pr                  ExtPgm('RTVOBJLIB')
     D                               10    const
     D                                7    const
     D                               10

     DRtvSuffix        pr                  ExtPgm('RTVSUFFIX')
     D                                3

     D#ofLibs          c                   5
     DaryLibs          s             10    dim(#ofLibs) ctdata perrcd(1)
     DdecCount         s             15  0 inz(0)
     ddecLength        s             15  5 inz
     dinDelimiter      s              1    inz('|')
     dsuffix           s              3
     dstrChoices       s          32767    inz
     dstrChoiceValues  s          32767    inz
     dstrCmd           s           3000    inz
     dstrCurrentLib    s             10    inz
     dstrSqlCursor     s           1000    inz
     dstrFile          s             10    inz
     dstrLast          s             10    inz
     dstrLibrary       s             10    inz
     dstrText          s            100    inz
     dstrType          s             10    inz

     D i1              s              5  0 inz
     D startPos        s              5  0 inz
     D endPos          s              5  0 inz
     D len             s              5  0 inz
     D value           s            100    inz

     ddsFile           ds
     D ATfile                        10
     D ATtxt                         50
     ddsField          ds
     D WHfldI                        10
     D WHfldE                        10
     D*WHfldB                         5  0
     D*WHfldD                         2  0
     D*WHfldP                         2  0
     D WHfTxt                        50
     D WHcHd1                        20
     D WHcHd2                        20
     D WHcHd3                        20

      /Free
       //                                                                              52
       // input data is in format: |type|library|file|

       Exsr ProcessGenParm;
       If RtnCode = 'X';
          RtnCode = ' ';
          BuildBigJSON(outJSON :'RtnCode' :'GenParm scan error');
          Return;
       Endif;

       RtvSuffix(suffix);
       Select;
       When strType = 'library';
          Exsr LoadLibraries;
       When strType = 'file';
          Exsr LoadFiles;
       When strType = 'field';
          Exsr LoadFields;
       When strType = 'clear';
          Exsr Clear;
       Endsl;
       If strChoices <> *blanks;
          BuildBigJSON(outJSON :'Choices' :%trim(strChoices));
       Endif;
       decLength = %len(%trim(outJSON));
       If strChoiceValues <> *blanks;
          BuildBigJSON(outJSON :'ChoiceValues' :%trim(strChoiceValues));
       Endif;
       decLength = %len(%trim(outJSON));

       RtnCode = ' ';
       Return;

       //=================================================================
       // Check for errors
       //=================================================================
       BegSr ErrorCheck;


       EndSr;
       //=================================================================
       // Clear information
       //=================================================================
       BegSr Clear;
        BuildBigJSON(outJSON:'Choices':'');
        BuildBigJSON(outJSON:'ChoiceValues':'');
        //BuildBigJSON(outJSON:'Value':'');
       EndSr;
       //=================================================================
       // extract values from genparm
       //=================================================================
       BegSr ExtractValue;
        value = *blanks;
        If startPos > 0;
           endPos = %Scan(inDelimiter:inGenParm:startPos);
           If endPos > 0;
              len = endPos-startPos;
              If len > 0;
                 value = %Subst(inGenParm:startPos:len);
              Endif;
           Endif;
        Endif;
       EndSr;

       //=================================================================
       // Load field choices
       //=================================================================
       BegSr LoadFields;
        strCmd = 'DSPFFD FILE(' + %trim(strLibrary)+'/'+%trim(strFile) + ') ' +
         'OUTPUT(*OUTFILE) OUTFILE(QTEMP/APPCFGFLD)';
        RunCmd(strCmd:%len(strCmd));
        strSqlCursor = 'select WHfldI, WHfldE, WHfTxt, WHcHd1, WHcHd2, WHcHd3 '+
         'from QTEMP/APPCFGFLD ' +
         ' ';
        Exec SQL Prepare sqlField From :strSqlCursor;
        Exec SQL Declare cursorField Cursor For sqlField;
        Exec SQL Open cursorField;
        Dou SQLSTT <> '00000';
           Exec SQL Fetch next From cursorField into :dsField;
           Select;
           When SQLSTT <> '00000';
              Iter;
           Endsl;
           If WHfldI = *blanks;
              WHfldI = WHfldE;
           Endif;
           strText = WHfTxt;
           If WHfTxt = *blanks;
              strText = %trim(WHcHd1) + ' ' + %trim(WHcHd2) + ' '
               + %trim(WHcHd3);
           Endif;
           AddChoice(%trim(WHfldI) + ' - ' +
            %trim(ScanReplace(',':' ':strText)));
           AddChoiceValue(%trim(WHfldI));
        Enddo;
        Exec SQL Close cursorField;
        Select;
        When strChoices = *blanks;
           AddChoice('No files found.');
        When %scan(',':strChoices) <= 0;
           BuildBigJSON(outJSON:'Value':%trim(WHfldI));
        When %scan(',':strChoices) > 0;
         //strChoices = %replace('*ALL - All Fields,':strChoices:1:0);
         //strChoiceValues = %replace('*ALL,':strChoiceValues:1:0);
        Endsl;
       EndSr;

       //=================================================================
       // Load file choices
       //=================================================================
       BegSr LoadFiles;
        For i1 = 1 to #ofLibs;
           If strLibrary = %trim(aryLibs(1)) + suffix;
              BuildBigJSON(outJSON:'ChoicesSelectionCriteria':
               'ATLIB = '''+%trim(strLibrary)+'''');
            //Leavesr;
              BuildBigJSON(outJSON:'ChoicesDatabaseFile':
               'PDSFIL'+%trim(suffix)+'/STAPPCFGF');
              BuildBigJSON(outJSON:'ChoicesOptionsField':
               'trim(ATFILE) || '' - '' || ATTXT');
              BuildBigJSON(outJSON:'ChoiceValuesField':'ATFILE');
            //BuildBigJSON(outJSON:'MaxChoices':'500');
              Leavesr;
           Endif;
        Endfor;
        strSqlCursor = 'select ATfile, ATtxt ' +
         'from PDSFIL' + %trim(suffix) + '/STAPPCFGF ' +
         'where ATLIB = ''' + %trim(strLibrary) + '''';
        Exec SQL Prepare sqlFile From :strSqlCursor;
        Exec SQL Declare cursorFile Cursor For sqlFile;
        Exec SQL Open cursorFile;
        Exec SQL Fetch next From cursorFile into :dsFile;
        If SQLSTT <> '00000';
           Exec SQL Close cursorFile;
           strCmd = 'DSPFD FILE(' + %trim(strLibrary) + '/*ALL) ' +
            'FILEATR(*PF) TYPE(*BASATR) ' +
            'OUTMBR(*FIRST *ADD) ' +
            'OUTPUT(*OUTFILE) ' +
            'OUTFILE(PDSFIL' + %trim(suffix) + '/STAPPCFGF)';
           RunCmd(strCmd:%len(strCmd));
        Endif;
        //strSqlCursor = 'select ATfile, ATtxt ' +
        // 'from QTEMP/APPCFGFILE ';
        //If strLast <> *blanks;
        //   strSqlCursor = %trim(strSqlCursor) +
        //    ' where ATfile > ''' + %trim(strLast) + '''';
        //Endif;
        //Exec SQL Prepare sqlFile From :strSqlCursor;
        //Exec SQL Declare cursorFile Cursor For sqlFile;
        Exec SQL Open cursorFile;
        Dou SQLSTT <> '00000';
           Exec SQL Fetch next From cursorFile into :dsFile;
           Select;
           When SQLSTT <> '00000';
              Iter;
           When ATfile <= strLast;
              Iter;
           Endsl;
         //AddChoice(%trim(ATfile) + ' - ' +
         // %trim(ScanReplace(',':' ':ATtxt)));
         //AddChoiceValue(%trim(ATfile));
           decCount += 1;
           If decCount > 1;
              Leave;
           Endif;
        Enddo;
        Exec SQL Close cursorFile;
        Select;
        When decCount = *zero;
           BuildBigJSON(outJSON:'Value':'No files found.');
           strChoices = %replace('No files found.':strChoices:1:0);
           strChoiceValues = %replace('No files found.':strChoiceValues:1:0);
        When decCount = 1;
           BuildBigJSON(outJSON:'Value':%trim(ATfile));
        When decCount >= 50;
           BuildBigJSON(outJSON:'LastChoice':%trim(ATfile));
        Endsl;
              BuildBigJSON(outJSON:'ChoicesDatabaseFile':
               'PDSFIL'+%trim(suffix)+'/STAPPCFGF');
              BuildBigJSON(outJSON:'ChoicesOptionsField':
               'trim(ATFILE) || '' - '' || ATTXT');
              BuildBigJSON(outJSON:'ChoiceValuesField':'ATFILE');
              BuildBigJSON(outJSON:'ChoicesSelectionCriteria':
               'ATLIB = '''+%trim(strLibrary)+'''');
       EndSr;

       //=================================================================
       // Load library choices
       //=================================================================
       BegSr LoadLibraries;
        AddChoice('*Other');
        RtvObjLib('SSYPF':'*FILE':strCurrentLib);
        For i1 = 1 to #ofLibs;
           If strCurrentLib = %trim(aryLibs(1)) + suffix;
              Clear strCurrentLib;
           Endif;
        Endfor;
        If strCurrentLib <> *blanks;
           AddChoice(%trim(strCurrentLib));
        Endif;
        For i1 = 1 to #ofLibs;
           AddChoice(%trim(aryLibs(i1)) + suffix);
        Endfor;
       EndSr;

       //=================================================================
       // Process genparm
       //=================================================================
       BegSr ProcessGenParm;
        BuildBigJSON(outJSON :'inParms' : %trim(inGenParm));
        startPos = 0;
        endPos = 0;
        Monitor;
           // extract type
           startPos = %Scan(inDelimiter:inGenParm)+1;
           If startPos <= 1;
              LeaveSr;
           Endif;
           Exsr ExtractValue;
           strType = %trim(value);
           // extract library
           startPos = %Scan(inDelimiter:inGenParm:startPos)+1;
           If startPos <= 1;
              LeaveSr;
           Endif;
           Exsr ExtractValue;
           strLibrary = %trim(Upper(value));
           // extract file
           startPos = %Scan(inDelimiter:inGenParm:startPos)+1;
           If startPos <= 1;
              LeaveSr;
           Endif;
           Exsr ExtractValue;
           strFile = %trim(Upper(value));
           // extract last choice
           startPos = %Scan(inDelimiter:inGenParm:startPos)+1;
           If startPos <= 1;
              LeaveSr;
           Endif;
           Exsr ExtractValue;
           strLast = %trim(value);
        On-Error;
           Rtncode = 'X';
        EndMon;
       EndSr;

      /End-Free
       //=================================================================
       // Add value to list of choices
       //=================================================================
     PAddChoice        b
     DAddChoice        pi
     D inValue                      250    const

     D delimiter       s              1    inz
      /Free
       If strChoices <> *blanks;
          delimiter = ',';
       Endif;
       strChoices = %trim(strChoices) + delimiter + %trim(inValue);
       Return;
      /End-free
     PAddChoice        e
       //=================================================================
       // Add value to list of choice values
       //=================================================================
     PAddChoiceValue   b
     DAddChoiceValue   pi
     D inValue                      250    const

     D delimiter       s              1    inz
      /Free
       If strChoiceValues <> *blanks;
          delimiter = ',';
       Endif;
       strChoiceValues = %trim(strChoiceValues) + delimiter + %trim(inValue);
       Return;
      /End-free
     PAddChoiceValue   e
** Standard libraries
PDSFIL
PDSMFIL
PDSQTE
PDSMQTE
PDSCLT
