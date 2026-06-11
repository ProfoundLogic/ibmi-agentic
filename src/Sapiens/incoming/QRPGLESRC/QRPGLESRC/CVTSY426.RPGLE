     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H BNDDIR('WTBNDDIR')

      *===============================================================*
      *  Created   : 06/21/21                                         *
      *  Programmer: C:298                                            *
      *  Project   : SY426                                            *
      *===============================================================*
      *  Revisions:                                                   *
      *                                                               *
      *                                                               *
      *===============================================================*

     d strRoot         s            128A   inz
     d str1            s            128A   inz
     d str2            s            128A   inz
     d strChk          s            128A   inz
     d strSql          s            900
     d pos             s              3  0 inz
     d len             s              3  0 inz
     d strHldLib       s             10    inz
     d isLibNtExt      s              1n   inz
     d clientID        s              3    inz('000')

      * Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3

     d RunCmd          pr                  extPgm('QCMDEXC')
     d inCmd                       2750    const
     d inCmdLen                      15  5 const

     d CheckObj        pr                  extPgm('SRCHKOBJ')
     d outObject                     10    const
     d outObjectType                  7    const
     d inError                        1
      /copy sPrPrprty
      /copy sPrString

      /free

       // get client ID
       GetClientID(clientID);

       // get instance library name
       strHldLib = SysProperty('InstanceName');

       isLibNtExt = *off;
       If strHldLib <> *blanks;
          // verify if library exist
          CheckObj(strHldLib:'*LIB':isLibNtExt);
       Endif;

       // check if QA env
       If isLibNtExt or strHldLib = ' ';
          isLibNtExt = *off;
          strHldLib = 'PF' + clientID + 'QA';

          // verify if library exist
          CheckObj(strHldLib:'*LIB':isLibNtExt);
          // check if PDN env
          If isLibNtExt;
             isLibNtExt = *off;
             strHldLib = 'PF' + clientID + 'PN';

             // verify if library exist
             CheckObj(strHldLib:'*LIB':isLibNtExt);

             If isLibNtExt;
                strHldLib = ' ';
             Endif;
          Endif;
       Endif;

       If strHldLib = ' ';
          Return;
       Endif;

       Monitor;
          RunCmd('ADDLIBLE ' + %trim(strHldLib) +
             ' POSITION(*LAST)':45);
       On-error;
       Endmon;

       // process ATNAVP file

       //reqStr = '%Visit Tracking%';
       //Exec sql
       // select ANtext, rrn(ATNAVP) into :strText, :decRRN
       //  from ATNAVP
       //  where ANtext like trim(:reqStr);

       strRoot = 'Visit';
       str1 = 'Service';
       str2 = 'Request';
       strChk = '%' + %trim(strRoot) + '%';
       strsql = 'update ATNAVP' +
       ' set ANtext = replace(ANTEXT, ''' + %trim(strRoot) + '''' +
       '  , ''' + %trim(str1) + ' ' + %trim(str2) + ''')' +
       ' where ANTEXT <> '' ''' +
       '  and ANTEXT like (''' + %trim(strChk) + ''')';

       Exec SQL
       set option commit = *none;
       Exec SQL execute immediate :strSQL;
       *inlr = *on;
      /End-Free
