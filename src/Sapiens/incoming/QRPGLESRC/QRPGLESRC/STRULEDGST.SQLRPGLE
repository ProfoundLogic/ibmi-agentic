     hCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H Option(*SrcStmt : *NoDebugIO)
     H BndDir('STBNDDIR':'QC2LE')
     H dftactgrp(*no) actgrp(*CALLER)
      //===================================================================
     h/Title STRULEDGST - Send digets emails for rules
      //===================================================================
      //  Created: 07/20/17   C:33   Project: WC2405
      //===================================================================
      // Revisions:
      //
      *  02/01/22 C:33  Support: 97790
      *                 Encopass email addresses in ''
      //===================================================================
      *entry
     dStRuleDgst       pr                  Extpgm('STRULEDGST')
     d inDigestDate                  10
     d inDigestTime                   6
     dStRuleDgst       pi
     d inDigestDate                  10
     d inDigestTime                   6

     d dsRec         e ds                  extname(SXTRPREC) qualified inz
     d dsXml         e ds                  extname(SXTRPXML) qualified inz
     D dsFileChgInfo   ds          1000    dtaara(STFILECHG) inz
     D  strDirLoc                   100
     D  strExtPrefix                100
     D  strSmsPrefix                100
     d dsFileInfo      ds                  qualified inz
     d  file                         10a
     d  library                      10a
     d strAttach       s            300a   inz
     d strBody         s          10000a   inz
     d strCmd          s          15000a
     d strDesc         s             60a   inz
     d strDescFile     s             75a   inz('File Changes')
      //***  Begin Add  ***  02/01/22  *************
     d strEmailFrom    s             80a   inz varying
     d strEmailTo      s             80a   inz varying
      //***  End   Add  ***  02/01/22  *************
     d strOutput       s            250a   inz
     d strSqlCursor    s           5000a   inz
     d strSubject      s            200a   inz('Rule Digest')
     d strSms          s            200a   inz

     DDltRuleXML       pr
     D inCo#                          3  0 const
     D inName                        10    const
     D inSeq#                         5  0 const
     D inDigestDt                      d   const
     D inDigest                       6    const

     d/copy sPrRunCmd
     d/copy sPrXml
      /Free
       Clear strSms;
       strBody = ('See attachment for file changes.');
       Exec SQL Set option COMMIT = *NONE;
       strSqlCursor = 'select * from SXTRPXML ' +
        'where ' +
        ' TRDIGEST = ''' + inDigestTime + ''' and ' +
        ' TRDGSTDT = date(''' + inDigestDate + ''') ' +
        'order by TRCO#, TRNAME, TRSEQ#';
       Exec SQL Prepare sqlXml From :strSqlCursor;
       Exec SQL Declare cursorXml Cursor For sqlXml;
       Exec SQL Open cursorXml;
       Dou SQLSTT <> '00000';
          Exec SQL Fetch next From cursorXml into :dsXml;
          If SQLSTT <> '00000';
             Iter;
          Endif;
          Exsr GetFileDesc;
          Monitor;
             CreateXML(dsXml.TRxml:*off);
             In dsXmlMsg;
          On-error;
             strXmlMsg = '*Failure';
          Endmon;
          Clear strAttach;
          If strXmlMsg = *blanks or %subst(strXmlMsg:1:7) = 'Success';
             strOutput = GetXMLOutputName(dsXml.TRxml:*on);
             If strOutput <> *blank;
                strAttach = 'ATTACH((''' + %subst(strDescFile:1:30) + ''' ''' +
                 %trim(strOutput) + '''))';
             Endif;
          Endif;
          In dsFileChgInfo;
          Select;
          When strExtPrefix <> *blanks;
             strSms = GetXMLOutputName(dsXml.TRxml:*off:2);
             If strSms <> *blank;
                strSms = %trim(strExtPrefix) + %trim(strSms);
             Endif;
          Other;
             strSms = GetXMLOutputName(dsXml.TRxml:*on:2);
          Endsl;
          If strAttach = *blanks;
             strBody = %trim(strBody) + '<br><br><br><br><br><br>' +
              'XML: ' + %trim(dsXml.TRxml) + '<br>' +
              'Date: ' + %trim(inDigestDate) + '<br><br>';
             strSms = 'Unable to generate link. '                            +
              %trim(strDescFile) + '  ' + %trim(strDesc) + '  ' +
              'XML: ' + %trim(dsXml.TRxml);
          Endif;
          Exsr ProcessRecipients;
          DltRuleXML(dsXml.TRco#:dsXml.TRname:dsXml.TRseq#:dsXml.TRdgstDt
           :dsXml.TRdigest);
       Enddo;
       Exec SQL Close cursorXml;
       *inlr = *on;
       //===================================================================
       Begsr GetFileDesc;
        Clear dsFileInfo;
        Exec SQL Select TRfile, TRlibrary into :dsFileInfo from SMTRPFIL
         where TRco# = :dsXml.TRco# and TRname = :dsXml.TRname
          and TRseq# = :dsXml.TRseq#
         fetch first row only;
        If dsFileInfo.library <> '*LIBL' and dsFileInfo.library <> *blank;
           strDescFile = %trim(dsFileInfo.library) + '/';
        Endif;
        If dsFileInfo.file <> *blank;
           strDescFile = 'Changes: ' +
            %trim(strDescFile) + %trim(dsFileInfo.file);
        Endif;
       Endsr;
       //===================================================================
       Begsr ProcessRecipients;
        Clear strDesc;
        Exec SQL Select TRdesc into :strDesc from SMTRP
         where TRco# = :dsXml.TRco# and TRname = :dsXml.TRname
          and TRseq# = :dsXml.TRseq#
         fetch first row only;
        // Read through SXTRPREC records by Co#/Name/Seq#/DgstDt/Digest
        Exec SQL Declare cursorRec Cursor For
         Select * from SXTRPREC
          where TRco# = :dsXml.TRco# and TRname = :dsXml.TRname
           and TRseq# = :dsXml.TRseq# and TRdgstDt = :dsXml.TRdgstDt
           and TRdigest = :dsXml.TRdigest;
        Exec SQL Open cursorRec;
        // Send email for each recipient (including attachment)
        Dou SQLSTT <> '00000';
           Clear dsRec;
           Exec SQL Fetch next From cursorRec into :dsRec;
           Select;
           When SQLSTT <> '00000';
              Iter;
           Endsl;
           If dsRec.TRemail <> *blank;
            //***  Begin Chg  ***  02/01/22  ************* +
            //strCmd = 'SNDEMAIL FROM(' + %trim(dsRec.TRemail) + ') ' +
            // 'TO(' + %trim(dsRec.TRemail) + ') ' +
              strEmailFrom = '''' + %trim(dsRec.TRemail) + '''';
              strEmailTo = '''' + %trim(dsRec.TRemail) + '''';
              strCmd = 'SNDEMAIL FROM(' + strEmailFrom + ') ' +
               'TO(' + strEmailTo + ') ' +
            //***  End   Chg  ***  02/01/22  ************* +
               'SUBJECT(''' + %trim(strSubject) + ''') ' +
               'BODY(''' + %trim(strBody) + ''') ' +
               %trim(strAttach);
              RunCmd(strCmd:%len(%trim(strCmd)));
           Endif;
           If dsRec.TRtext <> *zeros;
              strSms = 'Digest for ' + %trim(inDigestDate) +
               ' - ' + strSms;
              If strSmsPrefix <> *blanks;
                 strSms = %trim(strSmsPrefix) + ' ' + strSms;
              Endif;
              strCmd = 'SNDSMS TO(' + %trim(%editc(dsRec.TRtext:'X')) +
               ') ' + 'MESSAGE(''' + %trim(strSms) + ''')';
              RunCmd(strCmd:%len(%trim(strCmd)));
           Endif;
        Enddo;
        Exec SQL Close cursorRec;
       Endsr;
      /End-free

      //======================================================================
      // Delete XML record name
      //======================================================================
     PDltRuleXML       b
     DDltRuleXML       pi
     D inCo#                          3  0 const
     D inName                        10    const
     D inSeq#                         5  0 const
     D inDigestDt                      d   const
     D inDigest                       6    const

     disDelete         s              1n   inz('1')
     dstrXml           s            250
      /Free
       // Used for testing to change the value in debug
       If not isDelete;
          Return;
       Endif;
       Exec Sql
        delete from SXTRPXML
         where TRCO# = :inCo# and TRNAME = :inName and TRSEQ# = :inSeq#
          and TRDGSTDT = :inDigestDt and TRDIGEST = :inDigest;
       Return;
      /End-free
     PDltRuleXML       e
