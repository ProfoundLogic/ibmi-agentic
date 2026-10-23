**free

// ---------------------------------------------------------------------
// Program: rcnbrwr  (Reconciliation Log Browse & Inquiry)
// Purpose: Filterable subfile of reconciliation_log rows (item,
//          reconciled_by, run_timestamp date range). Option 5 drills to
//          a plain-record detail showing before/after values + notes.
//          No F6=Add -- this table is populated exclusively by the
//          lotrecon service (PERP-44), never by hand, same reasoning
//          pobrwd already applies (browse-only screens in this module
//          omit F6).
// Epic:    PERP-8 (PERP-45)
// ---------------------------------------------------------------------

// datfmt(*iso) is REQUIRED here (not decorative) -- filter defaults use
// 1940-01-01 / 2039-12-31 as sentinels, and the job DATFMT on this
// environment is *MDY (2-digit year, 1940-2039). Without this ctl-opt,
// every Date variable in this program is capped at *MDY's range and
// RNQ0114 fires at runtime the first time the DSPF WRITEs the FFRDT/
// FTODT fields or the SQL fetches one. Same fix pobrwr needed.
ctl-opt dftactgrp(*no) actgrp(*new) datfmt(*iso);

dcl-f rcnbrwd workstn sfile(lsfl:rrn) sfile(rmsgsfl:msgrrn);

dcl-ds ldaDS dtaara(*lda) len(1024) qualified;
  compcd char(3) pos(1);
end-ds;

dcl-pr QMHSNDPM extpgm;
  msgId       char(7)   const;
  msgF        char(20)  const;
  msgData     char(256) const;
  msgDataLen  int(10)   const;
  msgType     char(10)  const;
  stackEntry  char(10)  const;
  stackCntr   int(10)   const;
  msgKey      char(4);
  errorCode   char(8)   const;
end-pr;

dcl-ds statusDS psds qualified;
  programName char(10) pos(334);
end-ds;

dcl-ds reconRow qualified;
  id    int(10);
  item  varchar(25);
  qbef  packed(11:4);
  qaft  packed(11:4);
  rcnby varchar(18);
end-ds;

dcl-ds rows likeds(reconRow) dim(500);
dcl-s numRows int(10);
dcl-s rrn     int(10);
dcl-s msgrrn  int(10);
dcl-s selRrn  int(10);
dcl-s selOpt  char(1);
dcl-s selId   int(10);
dcl-s i       int(10);
dcl-s compcd  char(3);

// Cursor scalars.
dcl-s cId    int(10);
dcl-s cItem  varchar(25);
dcl-s cQbef  packed(11:4);
dcl-s cQaft  packed(11:4);
dcl-s cRcnby varchar(18);

in ldaDS;
compcd = ldaDS.compcd;

if compcd = '';
  exsr clearMsgs;
  writeMsg('No company selected - run Select Company (PERPSELR) first.');
  *in40 = *on;
  lcompdsp = '';
  write rmsgctl;
  exfmt lfoot;
  *inlr = *on;
  return;
endif;

lcompdsp = compcd;
fitem  = '';
frcnby = '';
// Filter sentinels must stay within the *MDY 1940-2039 range -- see
// ctl-opt comment above and pobrwr's own identical fix.
ffrdt = %date('1940-01-01' : *ISO);
ftodt = %date('2039-12-31' : *ISO);

// -----------------------------------------------------------------------
// Browse loop.
// -----------------------------------------------------------------------
dow '1';
  exsr loadRows;

  if numRows = 0;
    *in30 = *off;
    write lnorows;
  else;
    exsr fillSubfile;
    *in30 = *on;
  endif;

  write lfoot;
  *in40 = *off;
  if msgrrn > 0;
    *in40 = *on;
  endif;
  write rmsgctl;
  exfmt lctl;

  if *in03 or *in12;
    leave;
  endif;

  exsr clearMsgs;

  if numRows > 0;
    selRrn = 0;
    selOpt = ' ';
    readc lsfl;
    dow not %eof(rcnbrwd);
      if lsopt <> '';
        if selRrn = 0;
          selRrn = rrn;
          selOpt = lsopt;
        else;
          writeMsg('Only one selection per Enter.');
        endif;
      endif;
      readc lsfl;
    enddo;

    if selRrn > 0 and msgrrn = 0;
      select;
        when selOpt = '5';
          selId = rows(selRrn).id;
          exsr showDetail;
        other;
          writeMsg('Option ' + selOpt + ' not valid.');
      endsl;
    endif;
  endif;
enddo;

*inlr = *on;
return;

// ---------------------------------------------------------------------
// loadRows -- pull reconciliation_log rows matching the current filter
// values. Empty filter fields skip that filter (fitem='' means "any
// item", etc.).
begsr loadRows;
  numRows = 0;

  exec sql declare lc1 cursor for
    select reconciliation_id, item_number, item_qty_before, qty_after,
           reconciled_by
      from perpdemo.reconciliation_log
     where company_code = :compcd
       and (:fitem  = '' or item_number  = :fitem)
       and (:frcnby = '' or reconciled_by = :frcnby)
       and cast(run_timestamp as date) between :ffrdt and :ftodt
     order by reconciliation_id desc;
  exec sql open lc1;
  if sqlcode < 0;
    writeMsg('SQL open failed: SQLCODE=' + %char(sqlcode)
           + ' STATE=' + sqlstate);
    return;
  endif;

  dow numRows < %elem(rows);
    exec sql fetch lc1 into :cId, :cItem, :cQbef, :cQaft, :cRcnby;
    if sqlcode = 100 or sqlcode < 0;
      leave;
    endif;
    numRows += 1;
    rows(numRows).id    = cId;
    rows(numRows).item  = cItem;
    rows(numRows).qbef  = cQbef;
    rows(numRows).qaft  = cQaft;
    rows(numRows).rcnby = cRcnby;
  enddo;
  exec sql close lc1;
endsr;

// ---------------------------------------------------------------------
begsr fillSubfile;
  rrn = 0;
  *in31 = *on;
  write lctl;
  *in31 = *off;
  for i = 1 to numRows;
    lsopt   = '';
    lsid    = rows(i).id;
    lsitem  = rows(i).item;
    lsqbef  = rows(i).qbef;
    lsqaft  = rows(i).qaft;
    // Direct assign VARCHAR -> fixed CHAR: RPG right-pads/truncates
    // automatically. %subst is strict about CURRENT length, not
    // declared max (DDL_STYLE_GUIDE Sec.13) -- don't use it here.
    lsrcnby = rows(i).rcnby;
    rrn += 1;
    write lsfl;
  endfor;
endsr;

// ---------------------------------------------------------------------
// showDetail -- populate LDETAIL fields for the selected reconciliation
// row. EXFMT once; F12 returns to the browse.
begsr showDetail;
  exec sql
    select reconciliation_id, item_number, char(run_timestamp),
           item_qty_before, lot_sum_before, qty_after, reconciled_by,
           notes
      into :ddid, :dditem, :ddrunts, :ddqbef, :ddlsum, :ddqaft,
           :ddrcnby, :ddnotes
      from perpdemo.reconciliation_log
     where company_code = :compcd and reconciliation_id = :selId;
  if sqlcode <> 0;
    writeMsg('Reconciliation ' + %char(selId) + ' lookup failed: SQLCODE='
           + %char(sqlcode));
    return;
  endif;

  *in40 = *off;
  if msgrrn > 0;
    *in40 = *on;
  endif;
  write rmsgctl;
  exfmt ldetail;
endsr;

// ---------------------------------------------------------------------
begsr clearMsgs;
  msgrrn = 0;
  *in41 = *on;
  write rmsgctl;
  *in41 = *off;
endsr;

// ---------------------------------------------------------------------
dcl-proc writeMsg;
  dcl-pi *n;
    text varchar(256) const;
  end-pi;
  dcl-s data char(256);
  data = text;
  QMHSNDPM(
    'CPF9897' :
    'QCPFMSG   QSYS      ' :
    data :
    %len(text) :
    '*INFO     ' :
    '*         ' :
    1 :
    smsgkey :
    x'0000000000000000');
  msgrrn += 1;
  spgmq = statusDS.programName;
  write rmsgsfl;
end-proc;
