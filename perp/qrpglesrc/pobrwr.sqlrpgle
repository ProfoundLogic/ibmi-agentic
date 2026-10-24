**free

// ---------------------------------------------------------------------
// Program: pobrwr  (PO Browse & Inquiry)
// Purpose: Filterable subfile of PO headers with drill-down to detail.
//          Filters: status_code, vendor_code, buyer_code, order-date
//          range. Enter reloads the subfile with the current filter
//          values.
//
//          Option 5 on a PO switches to plain-record BDETAIL showing
//          header + up to 6 lines joined to po_line_open for open_qty
//          and extended_price + a src-req display + a schedule
//          summary line. Follows reqaprr's "one subfile + plain
//          detail" shape (DDL_STYLE_GUIDE.md Sec.14) to avoid the
//          CPF5006 two-subfile crash.
//
//          Option 9 calls poschr for the selected PO (line 1) so the
//          user can drill into blanket schedules without leaving the
//          browse -- if the PO has no lines, that call falls through
//          to the schedule program's own not-found message.
// Epic:    PERP-7 (PERP-41)
// ---------------------------------------------------------------------

// datfmt(*iso) is REQUIRED here (not decorative). FFRDT/FTODT are
// DATFMT(*MDY) on pobrwd.dspf, which binds their underlying variables
// to *MDY's 1940-2039 year range regardless of this ctl-opt (which
// only governs Date variables not tied to a display-file field) --
// see the PERP-89 clamp below, which uses 1940-01-01/2039-12-31 as
// in-range sentinels for exactly this reason (an out-of-range sentinel
// like 0001-01-01 crashes with RNQ0114 the moment it's assigned to
// one of these fields -- confirmed live in poentr's EEXPDT).
ctl-opt dftactgrp(*no) actgrp(*new) datfmt(*iso);

dcl-f pobrwd workstn sfile(bsfl:rrn) sfile(rmsgsfl:msgrrn);

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

dcl-pr poschCall extpgm('POSCHR');
  in_ponbr char(10) const;
  in_line  packed(3:0) const;
end-pr;

dcl-ds statusDS psds qualified;
  programName char(10) pos(334);
end-ds;

dcl-ds poRow qualified;
  ponbr   int(20);
  vndcd   char(10);
  vndnm   varchar(60);
  buyer   char(10);
  orddt   date;
  stat    varchar(20);
end-ds;

dcl-ds rows likeds(poRow) dim(500);
dcl-s numRows int(10);
dcl-s rrn      int(10);
dcl-s msgrrn   int(10);
dcl-s selRrn   int(10);
dcl-s i        int(10);
dcl-s compcd   char(3);
dcl-s selOpt   char(1);
dcl-s selPo    int(20);
dcl-s firstLine int(10);
dcl-s cnt       int(10);
dcl-s doneAll   ind;

// Cursor scalars.
dcl-s cPo      int(20);
dcl-s cVnd     char(10);
dcl-s cVndNm   varchar(60);
dcl-s cBuyer   char(10);
dcl-s cOrdDt   date;
dcl-s cStat    varchar(20);
dcl-s cCurr    varchar(20);
dcl-s cTot     packed(15:2);
dcl-s cLine    int(10);
dcl-s cItem    varchar(25);
dcl-s cOrd     packed(15:4);
dcl-s cRcv     packed(15:4);
dcl-s cOpen    packed(15:4);
dcl-s cPrice   packed(15:4);
dcl-s cSrcReq  int(20);
dcl-s cSrcLn   int(10);
dcl-s cSchCnt  int(10);

// Locals used by showDetail/putLine -- hoisted to main scope; dcl-s
// inside a begsr triggers RNF0724 (DDL_STYLE_GUIDE.md Sec.13).
dcl-s ln       int(10);
dcl-s schedTot packed(15:4);
dcl-s schedCnt int(10);
dcl-s srcTxt   char(8);

in ldaDS;
compcd = ldaDS.compcd;

if compcd = '';
  exsr clearMsgs;
  writeMsg('No company selected - run Select Company (PERPSELR) first.');
  *in40 = *on;
  lcompdsp = '';
  write rmsgctl;
  exfmt bfoot;
  *inlr = *on;
  return;
endif;

lcompdsp = compcd;
// Initial filter state -- everything.
fstat  = '';
fvnd   = '';
fbuy   = '';
// Filter sentinels must be within the *MDY 1940-2039 range: the SQL
// precompiler generates its intermediate host variables (SQL_00020 /
// SQL_00021 for :ffrdt / :ftodt) with the JOB DATFMT (*MDY on this
// env), IGNORING the ctl-opt datfmt(*iso). Assigning any date outside
// 1940-2039 into those generated vars crashes with RNQ0114. 1940 and
// 2039 both safely bracket every realistic PO order_date. Also parse
// with :*ISO in case a future ctl-opt change drops the datfmt override.
ffrdt  = %date('1940-01-01' : *ISO);
ftodt  = %date('2039-12-31' : *ISO);
doneAll = *off;

// -----------------------------------------------------------------------
// Browse loop.
// -----------------------------------------------------------------------
dow not doneAll;
  exsr loadPOs;

  if numRows = 0;
    *in30 = *off;
    write bnopo;
  else;
    exsr fillBSFL;
    *in30 = *on;
  endif;

  write bfoot;
  *in40 = *off;
  if msgrrn > 0;
    *in40 = *on;
  endif;
  write rmsgctl;
  exfmt bsctl;

  if *in03 or *in12;
    leave;
  endif;

  // PERP-89: blanking (or otherwise invalidating) either date filter
  // produces a value outside the *MDY-safe 1940-2039 range, which then
  // crashes with RNQ0114 the moment loadPOs' embedded SQL touches it
  // (the SQL precompiler's own intermediate host variable for a date
  // parameter is bound to the JOB's *MDY format regardless of this
  // program's ctl-opt datfmt(*iso) override -- see the header comment
  // and DDL_STYLE_GUIDE.md). %subdt is a plain RPG built-in, not an
  // SQL host variable, so it's safe to test the *year* of whatever
  // came back from the screen -- including an out-of-range value --
  // before it ever reaches loadPOs. Clamping back to the sentinel is
  // exactly "blank means no filter on that side" per the ticket.
  if %subdt(ffrdt : *years) < 1940 or %subdt(ffrdt : *years) > 2039;
    ffrdt = %date('1940-01-01' : *ISO);
  endif;
  if %subdt(ftodt : *years) < 1940 or %subdt(ftodt : *years) > 2039;
    ftodt = %date('2039-12-31' : *ISO);
  endif;

  exsr clearMsgs;

  // Handle selections (Opt 5 = detail, Opt 9 = schedule maintenance).
  if numRows > 0;
    selRrn = 0;
    selOpt = ' ';
    selPo  = 0;
    readc bsfl;
    dow not %eof(pobrwd);
      if bsopt <> '';
        if selRrn = 0;
          selRrn = rrn;
          selOpt = bsopt;
          selPo  = rows(rrn).ponbr;
        else;
          writeMsg('Only one selection per Enter.');
        endif;
      endif;
      readc bsfl;
    enddo;

    if selRrn > 0 and msgrrn = 0;
      select;
        when selOpt = '5';
          exsr showDetail;
          // PERP-96: F3 on the detail popup should exit the whole
          // program like everywhere else, not just fall through to
          // redisplaying the browse list (which is already correct
          // for F12/Enter -- no change needed there).
          if doneAll;
            leave;
          endif;
        when selOpt = '9';
          // Find the first (lowest-numbered) line to hand to poschr.
          exec sql
            select coalesce(min(line_number), 0)
              into :firstLine
              from perpdemo.po_line
             where company_code = :compcd
               and po_number    = :selPo;
          if firstLine = 0;
            writeMsg('PO ' + %char(selPo) + ' has no lines yet.');
          else;
            poschCall(%char(selPo) : firstLine);
          endif;
        other;
          writeMsg('Option ' + selOpt + ' not valid.');
      endsl;
    endif;
  endif;
enddo;

*inlr = *on;
return;

// ---------------------------------------------------------------------
// loadPOs -- pull PO headers matching the current filter values.
// Empty filter fields skip that filter (fstat='' means "any status",
// etc.).
// ---------------------------------------------------------------------
begsr loadPOs;
  numRows = 0;

  exec sql declare bc1 cursor for
    select h.po_number, h.vendor_code, v.vendor_name, h.buyer_code,
           h.order_date, h.status_code
      from perpdemo.po_header h
      join perpdemo.vendor v
        on v.company_code = h.company_code and v.vendor_code = h.vendor_code
     where h.company_code = :compcd
       and (:fstat = '' or h.status_code = :fstat)
       and (:fvnd  = '' or h.vendor_code = :fvnd)
       and (:fbuy  = '' or h.buyer_code  = :fbuy)
       and h.order_date between :ffrdt and :ftodt
     order by h.po_number desc;
  exec sql open bc1;
  if sqlcode < 0;
    writeMsg('SQL open failed: SQLCODE=' + %char(sqlcode)
           + ' STATE=' + sqlstate);
    leavesr;
  endif;

  dow numRows < %elem(rows);
    exec sql fetch bc1 into :cPo, :cVnd, :cVndNm, :cBuyer,
                             :cOrdDt, :cStat;
    if sqlcode = 100 or sqlcode < 0;
      leave;
    endif;
    numRows += 1;
    rows(numRows).ponbr = cPo;
    rows(numRows).vndcd = cVnd;
    rows(numRows).vndnm = cVndNm;
    rows(numRows).buyer = cBuyer;
    rows(numRows).orddt = cOrdDt;
    rows(numRows).stat  = cStat;
  enddo;
  exec sql close bc1;
endsr;

// ---------------------------------------------------------------------
begsr fillBSFL;
  rrn = 0;
  *in31 = *on;
  write bsctl;
  *in31 = *off;
  for i = 1 to numRows;
    bsopt   = '';
    bsponbr = %char(rows(i).ponbr);
    bsvndr  = rows(i).vndcd;
    // Direct assign VARCHAR -> fixed CHAR: RPG right-pads or truncates
    // automatically. %subst is strict about the CURRENT length of a
    // VARCHAR (not the declared max) and raises RNQ0100 when a value
    // like 'DRAFT' (5 chars) is asked for 10 chars.
    bsvndnm = rows(i).vndnm;
    bsbuyer = rows(i).buyer;
    bsorddt = rows(i).orddt;
    bsstat  = rows(i).stat;
    rrn += 1;
    write bsfl;
  endfor;
endsr;

// ---------------------------------------------------------------------
// showDetail -- populate BDETAIL fields for the selected PO and up
// to 6 lines from po_line_open. EXFMT once; Enter or F12 returns
// to the browse.
// ---------------------------------------------------------------------
begsr showDetail;
  exec sql
    select h.vendor_code, v.vendor_name, h.buyer_code, h.order_date,
           h.status_code, h.total_amount, h.currency_code
      into :cVnd, :cVndNm, :cBuyer, :cOrdDt, :cStat, :cTot, :cCurr
      from perpdemo.po_header h
      join perpdemo.vendor v
        on v.company_code = h.company_code and v.vendor_code = h.vendor_code
     where h.company_code = :compcd and h.po_number = :selPo;
  if sqlcode <> 0;
    writeMsg('PO ' + %char(selPo) + ' lookup failed: SQLCODE='
           + %char(sqlcode));
    leavesr;
  endif;

  ddponbr = %char(selPo);
  ddvndr  = cVnd;
  ddvndnm = cVndNm;
  ddbuyr  = cBuyer;
  ddorddt = cOrdDt;
  ddstat  = cStat;
  ddtot   = cTot;
  ddcurr  = cCurr;

  // Turn off all line indicators.
  *in60 = *off;
  *in61 = *off;
  *in62 = *off;
  *in63 = *off;
  *in64 = *off;
  *in65 = *off;
  *in34 = *off;

  ln = 0;
  exec sql declare bc2 cursor for
    select line_number, item_number, ordered_qty, received_qty, open_qty,
           unit_price,
           coalesce(source_requisition_number, 0),
           coalesce(source_requisition_line_number, 0)
      from perpdemo.po_line_open
     where company_code = :compcd and po_number = :selPo
     order by line_number;
  exec sql open bc2;

  dow '1';
    exec sql fetch bc2 into :cLine, :cItem, :cOrd, :cRcv, :cOpen,
                             :cPrice, :cSrcReq, :cSrcLn;
    if sqlcode = 100 or sqlcode < 0;
      leave;
    endif;
    ln += 1;
    if ln > 6;
      *in34 = *on;
      leave;
    endif;
    exsr putLine;
  enddo;
  exec sql close bc2;

  // Schedule summary: count + total scheduled qty across ALL lines
  // of this PO.
  exec sql
    select count(*), coalesce(sum(scheduled_qty), 0)
      into :schedCnt, :schedTot
      from perpdemo.po_line_schedule
     where company_code = :compcd and po_number = :selPo;
  if schedCnt = 0;
    ddsch = 'None';
  else;
    ddsch = %char(schedCnt) + ' schedule(s), total qty '
          + %char(schedTot);
  endif;

  *in40 = *off;
  if msgrrn > 0;
    *in40 = *on;
  endif;
  write rmsgctl;
  exfmt bdetail;

  // PERP-96: F3 here should exit the whole program (consistent with
  // every other PERP screen), not just fall through and return to the
  // browse list the way F12/Enter already correctly do.
  if *in03;
    doneAll = *on;
  endif;
endsr;

// putLine -- ln, cItem, cOrd, cRcv, cOpen, cPrice, cSrcReq, cSrcLn
// already staged; fan out to L<n>* fields by row.
begsr putLine;
  if cSrcReq = 0;
    srcTxt = '';
  else;
    srcTxt = %char(cSrcReq) + '/' + %char(cSrcLn);
  endif;
  select;
    when ln = 1;
      *in60 = *on;
      l1ln = cLine;
      l1item = cItem;
      l1ord = cOrd;
      l1rcv = cRcv;
      l1open = cOpen;
      l1prc = cPrice;
      l1src = srcTxt;
    when ln = 2;
      *in61 = *on;
      l2ln = cLine;
      l2item = cItem;
      l2ord = cOrd;
      l2rcv = cRcv;
      l2open = cOpen;
      l2prc = cPrice;
      l2src = srcTxt;
    when ln = 3;
      *in62 = *on;
      l3ln = cLine;
      l3item = cItem;
      l3ord = cOrd;
      l3rcv = cRcv;
      l3open = cOpen;
      l3prc = cPrice;
      l3src = srcTxt;
    when ln = 4;
      *in63 = *on;
      l4ln = cLine;
      l4item = cItem;
      l4ord = cOrd;
      l4rcv = cRcv;
      l4open = cOpen;
      l4prc = cPrice;
      l4src = srcTxt;
    when ln = 5;
      *in64 = *on;
      l5ln = cLine;
      l5item = cItem;
      l5ord = cOrd;
      l5rcv = cRcv;
      l5open = cOpen;
      l5prc = cPrice;
      l5src = srcTxt;
    when ln = 6;
      *in65 = *on;
      l6ln = cLine;
      l6item = cItem;
      l6ord = cOrd;
      l6rcv = cRcv;
      l6open = cOpen;
      l6prc = cPrice;
      l6src = srcTxt;
  endsl;
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
