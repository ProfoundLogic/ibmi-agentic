     Hcopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H BNDDIR('STBNDDIR')
     H*=============================================================
     H/TITLE STFILECHN - File Rule Change Notification
     H*=============================================================
     H*  Description:
     H*
     H*  This program is called by STLETTER via the RunDataPgm subroutine.
     H*   It is used to send email/sms with the created document attached.
     H*
     H*  Created:  07/01/18  C:33  Support: 78471
     H*=============================================================

     disError          s              1n
     dstrAttach        s            300a   inz
     dstrCmd           s          15000a
     dstrOutput        s            250a   inz
     dstrSuffix        s              3

     ddsOut3         e ds                  extname(SXTRPOUT3) qualified inz

     d/copy sPrRunCmd
     d/copy sPrSql
     d/copy sPrString
     d/copy sPrXml

     dChkObj           pr                  extpgm('SRCHKOBJ')
     d outObj                        10    const
     d outType                        7    const
     d inError                        1

     DRtvSuffix        PR                  extpgm('RTVSUFFIX')
     D suffix                         3

      *entry
     DStFileChN        pr                  extpgm('STFILECHN')
     D inXML                         60
     DStFileChN        pi
     D inXML                         60

      /Free
       Exec Sql Set Option Commit = *None;

       RtvSuffix(strSuffix);

       ChkObj('SXTRPOUT3':'*FILE':isError);
       If not isError;
          strOutput = GetXMLOutputName(inXML:*on);
          Exsr ProcessOut3;
       Endif;

       *inlr = *on;
       Return;

       //-------------
       Begsr ProcessOut3;
        Exec sql close cursorOut3;
        Exec sql declare cursorOut3 cursor for
         select * from SXTRPOUT3
          where TRxml = :inXML;
        Exec sql open cursorOut3;
        Dou not IsValidFetch(sqlstt);
           Exec sql fetch next from cursorOut3 into :dsOut3;
           Reset strAttach;
           If strOutput <> *blank;
              strAttach = 'ATTACH((''' + %subst(dsOut3.TRdescFile:1:30)
               + ''' ''' + %trim(strOutput) + '''))';
           Endif;
           Select;
           When not IsValidFetch(sqlstt);
              Iter;
           When Upper(dsOut3.TRnotify) = 'EMAIL';
              Exsr ProcessOutEmail;
           When Upper(dsOut3.TRnotify) = 'SMS';
              Exsr ProcessOutSMS;
           Endsl;
             If strSuffix <> *blank and strSuffix <> '000';
                strCmd = %trim(strCmd) + ' SUFFIX(' + strSuffix + ')';
             Endif;
           RunCmd(strCmd:%len(%trim(strCmd)));
        Enddo;
        // Clear file
        Exec sql
         delete from SXTRPOUT3
          where TRxml = :inXML;
       Endsr;

       //-------------
       Begsr ProcessOutEmail;
        strCmd = 'SNDEMAIL FROM(' + %trim(dsOut3.TRrecip) + ') ' +
         'TO(' + %trim(dsOut3.TRrecip) + ') ' +
         'SUBJECT(''' + %trim(dsOut3.TRsubject) + ''') ' +
         'BODY(''' +
          %trim(%subst(dsOut3.TRbody:1:5000)) + ''' ''' +
          %trim(%subst(dsOut3.TRbody:5001:5000)) + ''') ' +
         %trim(strAttach);
       Endsr;

       //-------------
       Begsr ProcessOutSMS;
        strCmd = 'SNDSMS TO(' + %trim(dsOut3.TRrecip) + ') ' +
         'MESSAGE(''' + %trim(dsOut3.TRsubject) + ''')';
       Endsr;
      /End-free
