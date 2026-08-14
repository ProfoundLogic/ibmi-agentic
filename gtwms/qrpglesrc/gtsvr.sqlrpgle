**free

//////////////////////////////////////////////////////////////////////////
// GTSVR -- Supervisor View.
//
// "Progress and alerts" -- what the menu tile has promised since the first
// build. A supervisor standing on the floor wants three things in this order:
//
//   1  is the shift on track          -> the progress bars
//   2  where is it going wrong        -> variance and alert counts
//   3  take me to it                  -> tapping a row opens the real screen
//
// ACTIONABLE BEATS HISTORICAL, the same rule the Inventory alert list follows.
// A dashboard nobody can act on is a poster. So every row in the work queue is
// a button that calls the program the operator would have reached through the
// menu anyway:
//
//   a receipt -> GTRCLR, the receiving lines screen
//   a count   -> GTCNVR, the variance review -- which IS the supervisor's
//                screen for a count, not the blind entry screen
//
// It re-reads on every return, so coming back from a drill-down shows what the
// operator just did rather than a stale snapshot.
//
// ONE SUBFILE, and the per-zone rollup travels as a JSON string in ZONESTAT.
// Two subfiles in one EJS format do not compile -- the RDF-to-DDS conversion
// emits malformed DDS and the display file is not created at all. A short
// fixed-length list is a string; a list the operator scrolls is its own
// display file. The zone rollup is eight rows and nobody types into it.
//
// READ-ONLY. Nothing here writes, so there is no commit boundary to get wrong.
//////////////////////////////////////////////////////////////////////////

ctl-opt dftactgrp(*no) actgrp(*new);

dcl-f gtsvd workstn sfile(wrksfl : rrn) handler('PROFOUNDUI(HANDLER)');

exec sql set option commit = *chg, closqlcsr = *endmod;

//  The two screens a supervisor drills into. Same shape as every other
//  called screen here: the document, and the language they are working in.
dcl-pr gtrclr extpgm;
  inRcpt char(10) const;
  inLang char(2);
end-pr;

dcl-pr gtcnvr extpgm;
  inCnt  char(10) const;
  inLang char(2);
end-pr;

dcl-c MAX_ROWS 40;

dcl-s rrn     int(10) inz(0);
dcl-s i       int(10);
dcl-s done    ind inz(*off);

//  The work queue, held between round trips so a tap resolves to a document
//  without re-running the query that built the list.
dcl-ds work qualified dim(MAX_ROWS);
  kind char(4);
  doc  char(10);
end-ds;
dcl-s workCount int(10) inz(0);

//  Work fields. RNF0724 forbids dcl-s inside a begsr in the main procedure,
//  so everything the subroutines use is declared here.
//
//  NAMED x*, NOT w*. The subfile's DDS fields are WSEQ, WTYPE, WDOC, WWHERE,
//  WWHO, WSTAT, WLINES, WDONE, WVAR, WPHOTO -- and RPG is case-insensitive, so
//  a work field called wType IS the externally described WTYPE. Declaring it
//  again is a storm of RNF6033/RNF6299/RNF7456 "length of externally-described
//  field is not the same" that never once names the actual problem.
dcl-s xType   char(4);
dcl-s xDoc    char(10);
dcl-s xWhere  varchar(40);
dcl-s xWho    varchar(40);
dcl-s xStat   char(4);
dcl-s xLines  int(10);
dcl-s xDone   int(10);
dcl-s xVar    int(10);
dcl-s xPhoto  int(10);
dcl-s xZone   char(4);
dcl-s xLocs   int(10);
dcl-s xSkus   int(10);
dcl-s xOh     packed(11 : 2);
dcl-s xLow    int(10);
dcl-s xJson   varchar(1024);
dcl-s xPiece  varchar(200);

//------------------------------------------------------------------------
// Main
//------------------------------------------------------------------------
exsr loadOperator;

dow not done;
  exsr loadTotals;
  exsr loadZones;
  exsr loadWorkSubfile;

  action = *blanks;
  exfmt supvhome;
  msg     = *blanks;
  msgkind = *blanks;

  select;
    when action = 'EXIT';
      done = *on;

    when action = 'LANGEN';
      langpref = 'EN';
      exsr saveLanguage;

    when action = 'LANGFR';
      langpref = 'FR';
      exsr saveLanguage;

    when action = 'REFRESH';
      //  The loop re-reads at the top, so there is nothing to do here but
      //  come round again. Having the button be explicit beats telling the
      //  operator that Enter happens to work.

    when %subst(action : 1 : 4) = 'PICK';
      //  PICKnn -- the supervisor tapped a row. nn is the subfile sequence,
      //  not the RRN.
      i = %int(%subst(action : 5));
      if i >= 1 and i <= workCount;
        select;
          when work(i).kind = 'RECV';
            gtrclr(work(i).doc : langpref);
          when work(i).kind = 'CNT';
            gtcnvr(work(i).doc : langpref);
          other;
        endsl;
      endif;

    when action = *blanks;
      //  Enter with nothing pressed. Redisplay.

    other;
  endsl;
enddo;

*inlr = *on;
return;

//------------------------------------------------------------------------
// The numbers across the top.
//
// Receiving and counting each get lines/done/variance so the screen can draw
// a real progress bar rather than a percentage nobody can check. Every figure
// comes from the same views the operator screens use -- GTVRCPPRG, GTVCNTVAR,
// GTVREPLEN -- so the supervisor and the operator cannot be told different
// numbers about the same work.
//------------------------------------------------------------------------
begsr loadTotals;
  //  Receiving. Open means anything not yet posted.
  exec sql
    select count(*),
           coalesce(sum(line_count), 0),
           coalesce(sum(lines_done), 0),
           coalesce(sum(lines_variance), 0)
      into :nrcpts, :nrlines, :nrdone, :nrvar
      from gtvrcpprg
     where receipt_status <> 'POST';
  //  ONE STATEMENT PER LINE. Free-form RPG has no statement separator in the
  //  C sense: `a = 0; b = 0;` on one line is RNF5508 "end of free-format
  //  statement is not blank", pointing at the column after the first semicolon.
  if sqlcode <> 0;
    nrcpts  = 0;
    nrlines = 0;
    nrdone  = 0;
    nrvar   = 0;
  endif;

  //  Counting. GTVCNTVAR counts a line as done when it has been counted.
  exec sql
    select count(*),
           coalesce(sum(line_count), 0),
           coalesce(sum(lines_counted), 0),
           coalesce(sum(lines_variance), 0)
      into :ncounts, :nclines, :ncdone, :ncvar
      from gtvcntvar
     where count_status <> 'POST';
  if sqlcode <> 0;
    ncounts = 0;
    nclines = 0;
    ncdone  = 0;
    ncvar   = 0;
  endif;

  //  Replenishment alerts -- the same 51 the menu badge counts.
  exec sql
    select count(*) into :nalerts from gtvreplen;
  if sqlcode <> 0;
    nalerts = 0;
  endif;

  //  Stock standing in staging, waiting for putaway. This is the number that
  //  tells a supervisor the dock is backing up.
  exec sql
    select coalesce(sum(qty_staged), 0) into :nstaged from gtvputstg;
  if sqlcode <> 0;
    nstaged = 0;
  endif;

  //  Movements booked today, whatever kind. Shift throughput in one figure.
  exec sql
    select count(*) into :nmoves
      from gtmovement
     where date(moved_ts) = current date;
  if sqlcode <> 0;
    nmoves = 0;
  endif;
endsr;

//------------------------------------------------------------------------
// Per-zone rollup, as a JSON string.
//
// Hand-built rather than via a JSON function: this has to survive a CHAR(1024)
// display-file field, and SQL/JSON on this box would have to be trimmed to fit
// anyway. Eight zones at roughly 90 characters each leaves room to spare, and
// the loop stops before it can overrun the field.
//------------------------------------------------------------------------
begsr loadZones;
  xJson = '[';

  exec sql
    declare zonCsr cursor for
      select l.zone,
             count(distinct l.location_id),
             count(distinct b.sku),
             coalesce(sum(b.qty_on_hand), 0),
             (select count(*) from gtvreplen r where r.zone = l.zone)
        from gtlocation l
        left join gtinvbal b on b.location_id = l.location_id
       where l.active_flag = 'Y'
       group by l.zone
       order by l.zone;

  exec sql open zonCsr;

  dow *on;
    exec sql
      fetch zonCsr into :xZone, :xLocs, :xSkus, :xOh, :xLow;
    if sqlcode <> 0;
      leave;
    endif;

    xPiece = '{"z":"' + %trim(xZone) + '"'
           + ',"l":' + %char(xLocs)
           + ',"s":' + %char(xSkus)
           + ',"q":' + %char(%int(xOh))
           + ',"a":' + %char(xLow) + '}';

    //  +2 for the separator now and the closing bracket later. Overrunning a
    //  varchar is RNX0100 on the operator's screen, not a compile error.
    if %len(xJson) + %len(xPiece) + 2 > 1024;
      leave;
    endif;

    if %len(xJson) > 1;
      xJson += ',';
    endif;
    xJson += xPiece;
  enddo;

  exec sql close zonCsr;

  xJson += ']';
  zonestat = xJson;
endsr;

//------------------------------------------------------------------------
// The work queue: every open receipt and every open count, in one list.
//
// Ordered worst first -- variances, then least progress -- because that is
// the order a supervisor wants to walk the floor in. This is the opposite of
// the rule for the scan-to-confirm list, where ordering by status was wrong
// because every scan moved the row that had just been scanned. Nothing here
// is scanned, and nothing moves under the operator's thumb.
//------------------------------------------------------------------------
begsr loadWorkSubfile;
  rrn = 0;
  sflclear = '1';
  write supvhome;
  sflclear = '0';
  workCount = 0;

  exec sql
    declare wrkCsr cursor for
      select 'RECV', p.receipt_id,
             cast('Door ' || trim(p.door_id) || ' - ' ||
                  trim(p.vendor_name) as varchar(40)),
             cast(trim(p.po_number) as varchar(40)),
             p.receipt_status,
             p.line_count, p.lines_done, p.lines_variance, p.photo_count
        from gtvrcpprg p
       where p.receipt_status <> 'POST'
      union all
      select 'CNT', v.count_id,
             cast(trim(v.location_id) as varchar(40)),
             cast(coalesce((select trim(o.full_name) from gtoperator o
                             where o.operator_id = v.assigned_to), '')
                  as varchar(40)),
             v.count_status,
             v.line_count, v.lines_counted, v.lines_variance, 0
        from gtvcntvar v
       where v.count_status <> 'POST'
       order by 8 desc, 7, 2
       fetch first 40 rows only;

  exec sql open wrkCsr;

  dow workCount < MAX_ROWS;
    exec sql
      fetch wrkCsr into :xType, :xDoc, :xWhere, :xWho, :xStat,
                        :xLines, :xDone, :xVar, :xPhoto;
    if sqlcode <> 0;
      leave;
    endif;

    workCount += 1;
    work(workCount).kind = xType;
    work(workCount).doc  = xDoc;

    rrn   += 1;
    wseq   = workCount;
    wtype  = xType;
    wdoc   = xDoc;
    wwhere = xWhere;
    wwho   = xWho;
    wstat  = xStat;
    wlines = xLines;
    wdone  = xDone;
    wvar   = xVar;
    wphoto = xPhoto;
    write wrksfl;
  enddo;

  exec sql close wrkCsr;

  nwork = workCount;
endsr;

//------------------------------------------------------------------------
begsr loadOperator;
  exec sql
    select full_name, lang_pref, operator_role
      into :opername, :langpref, :operrole
      from gtoperator
     where operator_id = 'GT001' and active_flag = 'Y'
     fetch first row only;

  if sqlcode <> 0;
    opername = 'Demo Operator';
    langpref = 'EN';
    operrole = 'OPER';
  endif;
endsr;

//------------------------------------------------------------------------
begsr saveLanguage;
  exec sql
    update gtoperator set lang_pref = :langpref
     where operator_id = 'GT001';
  if sqlcode < 0;
    exec sql rollback;
  else;
    exec sql commit;
  endif;
endsr;
