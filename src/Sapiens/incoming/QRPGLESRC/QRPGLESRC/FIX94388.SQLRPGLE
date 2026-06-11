     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     h DftActGrp(*No)
     h Option(*NoDebugIO: *SrcStmt)
     h BndDir('WTBNDDIR')
     h/Title FIX94388 - Update Renew to Quote Flag
      *==========================================================
      * Description: Updates the Renew to Quote flag for policies that
      *              should still have the flag set.
      *==========================================================
      * Revisions:
      *
      *==========================================================
     disUpdate         s               n   inz('1')
     dstrSql           s               a   Len(32000) Varying
     dstrSqlCursor     s               a   Len(32000) Varying
     ddsQuote          ds                  qualified inz
     d co#                            3  0
     d fnd                            3  0
     d emp#                           9  0
     d div                            5  0
     d fyr                            3  0
     D/copy SPRSQL
     D/copy WPREMPLR
      /Free
       Exec Sql Set Option Commit = *None;
       // This is the statement needed to determine employer master records to be updated
       strSqlCursor =
       'with ' +
       'policy as ' +
       '(select ' +
       '  digits(EMco#)||digits(EMfnd)||digits(EMemp#)||digits(EMdiv) as pol#,'+
       '  EMRQFL as qteFlg, EMDt as qteDt, EMPgm as qtePgm, EMQTFR as qteTrDt' +
       '  from ' + %trim(RtvQuoteLib('*QTE')) + '.WMEMP' +
       '  where EMRQFL = '' ''), ' +
       'issues as ' +
       '(select pol#, EMco#, EMfnd, EMemp#, EMdiv, EMQTFR, qteTrDt, qteFlg, ' +
       'EMDt, qteDt, EMPgm, qtePgm' +
       '  from ' + %trim(RtvQuoteLib('*LIV')) + '.WMEMP' +
       '  join policy on' +
       '   pol#=digits(EMco#)||digits(EMfnd)||digits(EMemp#)||digits(EMdiv)),'+
       'quoteYear as ' +
       '(select ELco#, ELfnd, ELemp#, ELdiv, max(ELfyr) as year' +
       '  from ' + %trim(RtvQuoteLib('*QTE')) + '.WDELP' +
       '  where digits(ELco#)||digits(ELfnd)||digits(ELemp#)||digits(ELdiv)' +
       '   in (select pol# from issues)' +
       '  group by ELco#, ELfnd, ELemp#, ELdiv), ' +
       'liveYear as ' +
       '(select ELco#, ELfnd, ELemp#, ELdiv, max(ELfyr) as year' +
       '  from ' + %trim(RtvQuoteLib('*LIV')) + '.WDELP' +
       '  where digits(ELco#)||digits(ELfnd)||digits(ELemp#)||digits(ELdiv)' +
       '   in (select pol# from issues)' +
       '  group by ELco#, ELfnd, ELemp#, ELdiv) ' +
       'select *' +
       ' from quoteYear q' +
       ' join liveYear l on l.ELco#=q.ELco# and l.ELfnd=q.ELfnd' +
       '  and l.ELemp#=q.ELemp# and l.ELdiv=q.ELdiv' +
       ' where l.year=q.year-1' +
       ' order by q.ELco#, q.ELfnd, q.ELemp#, q.ELdiv';
       // Read from above and update PDSQTE*.WMEMP/EMRQFL = 'Y'
        Exec sql close cursorQuoteUpdates;
        Exec sql prepare sqlQuoteUpdates from :strSqlCursor;
        Exec sql declare cursorQuoteUpdates cursor for sqlQuoteUpdates;
        Exec sql open cursorQuoteUpdates;
        Dou not IsValidFetch(sqlstt);
           Exec sql fetch next from cursorQuoteUpdates into :dsQuote;
           Select;
           When not IsValidFetch(sqlstt);
              Iter;
           When not isUpdate;
              Iter;
           Endsl;
           strSql = 'update ' + %trim(RtvQuoteLib('*QTE')) + '.WMEMP ' +
            'set EMRQFL = ''Y'', EMQTFR = 0' +
            ' where EMco# = ' + %char(dsQuote.co#) +
            '  and EMfnd = ' + %char(dsQuote.fnd) +
            '  and EMemp# = ' + %char(dsQuote.emp#) +
            '  and EMdiv = ' + %char(dsQuote.div);
           Exec sql execute immediate :strSql;
           // This is to allow the job to continue if the update fails
           sqlstt='00000';
        Enddo;
        *inlr = *on;
      /End-free
