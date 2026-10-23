**free

// ---------------------------------------------------------------------
// Program: poreqr  (Create POs from Approved Requisitions)
// Purpose: Convert one or more APPROVED requisitions into POs, grouping
//          lines by the preferred vendor per item (one PO per vendor ->
//          "splitting"), consolidating multiple selected reqs going to
//          the same vendor into a single PO ("consolidation").
//
//          Line-level linkage: each generated po_line carries
//          source_requisition_number + source_requisition_line_number
//          so "converted" status is derivable by joining po_line back
//          to requisition_line's PK (no redundant status column, per
//          the PERP-37 DDL design note).
//
//          Requisitions with no preferred vendor rows for their items,
//          or already fully converted, are skipped with a message.
//
//          Follows reqaprr's "one subfile + plain-record detail" shape
//          (see DDL_STYLE_GUIDE.md Sec.14) -- SSFL (approved-req
//          subfile) plus a plain SPREV record for the vendor-grouped
//          preview (up to 5 vendors shown; the count message covers
//          any overflow). No second real subfile.
// Epic:    PERP-7 (PERP-39)
// ---------------------------------------------------------------------

// datfmt(*iso) for consistency with the other PO programs -- the job
// DATFMT here is *MDY (2-digit year, 1940-2039) and we want every
// PERP program to accept the full RPG Date range.
ctl-opt dftactgrp(*no) actgrp(*new) bnddir('PERP') datfmt(*iso);

dcl-f poreqd workstn sfile(ssfl:rrn) sfile(rmsgsfl:msgrrn);

/copy docseq_pr.rpgle

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

// A row of the approved-req subfile.
dcl-ds reqRow qualified;
  reqnbr  int(20);
  reqby   char(10);
  needby  date;
  totest  packed(15:2);
  isSel   ind;
  // Snapshot of the dominant vendor for this req (first line's
  // preferred vendor) -- purely for display.
  vndcd   char(10);
  vndnm   varchar(60);
end-ds;

// A plan row: vendor + a list of (reqnbr, reqline, item, qty, uom, price).
dcl-ds planLine qualified;
  reqnbr  int(20);
  reqline int(10);
  item    varchar(25);
  qty     packed(15:4);
  uom     varchar(5);
  price   packed(15:4);
end-ds;
dcl-ds vendorPlan qualified;
  vndcd     char(10);
  vndnm     varchar(60);
  lineCount int(10);
  totAmt    packed(15:2);
  lines     likeds(planLine) dim(500);
end-ds;

dcl-ds reqs likeds(reqRow) dim(500);
dcl-ds plan likeds(vendorPlan) dim(50);
dcl-s numReqs   int(10);
dcl-s numPlan   int(10);
dcl-s rrn       int(10);
dcl-s msgrrn    int(10);
dcl-s selCnt    int(10);
dcl-s lineCnt   int(10);
dcl-s i         int(10);
dcl-s j         int(10);
dcl-s k         int(10);
dcl-s compcd    char(3);
dcl-s docerrmsg varchar(80);
dcl-s newPo     int(20);
dcl-s vidx      int(10);
dcl-s prevMode  ind;

// Cursor host vars.
dcl-s cReqNbr   int(20);
dcl-s cReqBy    char(10);
dcl-s cNeedBy   date;
dcl-s cTotEst   packed(15:2);
dcl-s cVndCd    char(10);
dcl-s cVndNm    varchar(60);
dcl-s cReqLine  int(10);
dcl-s cItem     varchar(25);
dcl-s cQty      packed(15:4);
dcl-s cUom      varchar(5);
dcl-s cPrice    packed(15:4);

// Staging scalars for SQL statements that need array-element data.
// The SQLRPGLE precompiler does not accept :ds(i).field as a host
// variable, so we copy into these plain scalars first.
dcl-s xReqNbr   int(20);
dcl-s xReqLine  int(10);
dcl-s xVndCd    char(10);
dcl-s xItem     varchar(25);
dcl-s xQty      packed(15:4);
dcl-s xUom      varchar(5);
dcl-s xPrice    packed(15:4);
dcl-s xTotAmt   packed(15:2);
dcl-s xBuyer    char(10);

// Locals used by createPOs -- dcl-s inside a begsr triggers RNF0724
// ("statement type out of sequence"), so they live up here.
dcl-s poCount int(10);
dcl-s lineNbr int(10);
dcl-s v       int(10);
dcl-s L       int(10);

in ldaDS;
compcd = ldaDS.compcd;

if compcd = '';
  exsr clearMsgs;
  writeMsg('No company selected - run Select Company (PERPSELR) first.');
  *in40 = *on;
  lcompdsp = '';
  write rmsgctl;
  exfmt sfoot;
  *inlr = *on;
  return;
endif;

lcompdsp = compcd;
pcompdsp = compcd;

// -----------------------------------------------------------------------
// Selection loop.
// -----------------------------------------------------------------------
prevMode = *off;

dow '1';
  exsr loadReqs;

  if numReqs = 0;
    *in30 = *off;
    write snosub;
  else;
    exsr fillSSFL;
    *in30 = *on;
  endif;

  write sfoot;
  *in40 = *off;
  if msgrrn > 0;
    *in40 = *on;
  endif;
  write rmsgctl;
  exfmt ssctl;

  if *in03 or *in12;
    leave;
  endif;

  exsr clearMsgs;

  // Read selections. Guard READC against an unloaded subfile
  // (DDL_STYLE_GUIDE.md Sec.14 -- CPF5006 otherwise).
  selCnt = 0;
  if numReqs > 0;
    readc ssfl;
    dow not %eof(poreqd);
      if ssopt = '1';
        reqs(rrn).isSel = *on;
        selCnt += 1;
      endif;
      readc ssfl;
    enddo;
  endif;

  if *in06;
    // *in06 is a response indicator: the workstation turns it on when
    // F6 is pressed but never turns it off on its own, so it must be
    // cleared here or it stays "stuck on" and every later Enter/F5/F12
    // press on this screen is misread as another F6 (PERP-97 follow-up
    // bug found during live testing -- distinct from the DDS overflow).
    *in06 = *off;
    if selCnt = 0;
      writeMsg('Select at least one requisition (option 1) before F6.');
      iter;
    endif;

    exsr buildPlan;

    if numPlan = 0;
      writeMsg('Selected requisitions produced no PO lines -- check that '
             + 'items have a preferred vendor with a current price row.');
      // Clear selections and iter.
      for i = 1 to numReqs;
        reqs(i).isSel = *off;
      endfor;
      iter;
    endif;

    exsr showPreview;

    if *in06;
      // Same stuck-indicator concern as above -- clear it once consumed.
      *in06 = *off;
      // Confirmed -- create the POs.
      exsr createPOs;
      // After creation, refresh the list (converted lines drop out
      // of the "unconverted approved" query) and clear selections.
      exec sql commit;
      // Clear selection flags on the in-memory copy for safety.
      for i = 1 to numReqs;
        reqs(i).isSel = *off;
      endfor;
    else;
      // F12 back to selection -- keep selections in the array; refill
      // the subfile from reqs() so they still show.
      writeMsg('Conversion cancelled.');
      // NB: subfile is rebuilt from perpdemo on next iteration, so the
      // in-memory isSel flags are effectively wiped; that's fine --
      // the user re-selects.
    endif;
    iter;
  endif;
enddo;

*inlr = *on;
return;

// ---------------------------------------------------------------------
// loadReqs -- pull APPROVED reqs whose lines are not yet all linked
// to a po_line. A req with SOME lines converted still shows (its
// remaining lines get placed on new POs -- classic "splitting").
// ---------------------------------------------------------------------
begsr loadReqs;
  numReqs = 0;

  exec sql declare rc1 cursor for
    select h.requisition_number, h.requested_by, h.need_by_date,
           h.total_estimated_cost,
           coalesce(v.vendor_code, ''),
           coalesce(v.vendor_name, '')
      from perpdemo.requisition_header h
      left join lateral (
        select iv.vendor_code, ven.vendor_name
          from perpdemo.requisition_line rl
          join perpdemo.item_vendor iv
            on iv.company_code = rl.company_code
           and iv.item_number  = rl.item_number
           and iv.is_preferred = 'Y'
          join perpdemo.vendor ven
            on ven.company_code = iv.company_code
           and ven.vendor_code  = iv.vendor_code
         where rl.company_code       = h.company_code
           and rl.requisition_number = h.requisition_number
         order by rl.line_number
         fetch first 1 row only
      ) v on 1=1
     where h.company_code = :compcd
       and h.status_code  = 'APPROVED'
       and exists (
         select 1
           from perpdemo.requisition_line rl2
          where rl2.company_code       = h.company_code
            and rl2.requisition_number = h.requisition_number
            and not exists (
              select 1 from perpdemo.po_line pl
               where pl.company_code                    = rl2.company_code
                 and pl.source_requisition_number       = rl2.requisition_number
                 and pl.source_requisition_line_number  = rl2.line_number
            )
       )
     order by h.requisition_number;
  exec sql open rc1;
  if sqlcode < 0;
    writeMsg('SQL open failed: SQLCODE=' + %char(sqlcode)
           + ' STATE=' + sqlstate);
    leavesr;
  endif;

  dow numReqs < %elem(reqs);
    exec sql fetch rc1 into :cReqNbr, :cReqBy, :cNeedBy, :cTotEst,
                             :cVndCd, :cVndNm;
    if sqlcode = 100 or sqlcode < 0;
      leave;
    endif;
    numReqs += 1;
    reqs(numReqs).reqnbr = cReqNbr;
    reqs(numReqs).reqby  = cReqBy;
    reqs(numReqs).needby = cNeedBy;
    reqs(numReqs).totest = cTotEst;
    reqs(numReqs).vndcd  = cVndCd;
    reqs(numReqs).vndnm  = cVndNm;
    reqs(numReqs).isSel  = *off;
  enddo;
  exec sql close rc1;
endsr;

// ---------------------------------------------------------------------
begsr fillSSFL;
  rrn = 0;
  *in31 = *on;
  write ssctl;
  *in31 = *off;
  for i = 1 to numReqs;
    ssopt    = '';
    ssreqnbr = %char(reqs(i).reqnbr);
    ssreqby  = reqs(i).reqby;
    ssneedby = reqs(i).needby;
    ssvndr   = reqs(i).vndcd;
    ssvndnm  = reqs(i).vndnm;
    sstotest = reqs(i).totest;
    rrn += 1;
    write ssfl;
  endfor;
endsr;

// ---------------------------------------------------------------------
// buildPlan -- for each selected req, iterate its unconverted lines,
// look up the preferred vendor + current price, add to the vendor's
// plan bucket. Multiple selected reqs -> same vendor -> same bucket
// (consolidation). Different items on same req -> different vendors
// -> different buckets (splitting).
// ---------------------------------------------------------------------
begsr buildPlan;
  numPlan = 0;
  lineCnt = 0;

  for i = 1 to numReqs;
    if not reqs(i).isSel;
      iter;
    endif;

    xReqNbr = reqs(i).reqnbr;

    exec sql declare rc2 cursor for
      select rl.line_number, rl.item_number, rl.quantity,
             rl.uom_code,
             coalesce(iv.vendor_code, ''),
             coalesce(ven.vendor_name, ''),
             coalesce(ivp.unit_price, rl.est_unit_cost)
        from perpdemo.requisition_line rl
        left join perpdemo.item_vendor iv
          on iv.company_code = rl.company_code
         and iv.item_number  = rl.item_number
         and iv.is_preferred = 'Y'
        left join perpdemo.vendor ven
          on ven.company_code = iv.company_code
         and ven.vendor_code  = iv.vendor_code
        left join perpdemo.item_vendor_price ivp
          on ivp.company_code = iv.company_code
         and ivp.item_number  = iv.item_number
         and ivp.vendor_code  = iv.vendor_code
         and ivp.effective_to is null
       where rl.company_code       = :compcd
         and rl.requisition_number = :xReqNbr
         and not exists (
           select 1 from perpdemo.po_line pl
            where pl.company_code                    = rl.company_code
              and pl.source_requisition_number       = rl.requisition_number
              and pl.source_requisition_line_number  = rl.line_number
         )
       order by rl.line_number;
    exec sql open rc2;
    if sqlcode < 0;
      writeMsg('buildPlan open failed: SQLCODE=' + %char(sqlcode));
      leavesr;
    endif;

    dow '1';
      exec sql fetch rc2 into :cReqLine, :cItem, :cQty, :cUom,
                               :cVndCd, :cVndNm, :cPrice;
      if sqlcode = 100 or sqlcode < 0;
        leave;
      endif;

      if cVndCd = '';
        // No preferred vendor -- skip this line with a warning.
        writeMsg('Skipped req ' + %char(reqs(i).reqnbr)
               + ' line ' + %char(cReqLine)
               + ' (' + %trim(cItem) + '): no preferred vendor.');
        iter;
      endif;

      // Find or add the vendor bucket.
      vidx = 0;
      for j = 1 to numPlan;
        if plan(j).vndcd = cVndCd;
          vidx = j;
          leave;
        endif;
      endfor;
      if vidx = 0;
        if numPlan >= %elem(plan);
          writeMsg('Too many distinct vendors -- capping preview.');
          leave;
        endif;
        numPlan += 1;
        vidx = numPlan;
        plan(vidx).vndcd     = cVndCd;
        plan(vidx).vndnm     = cVndNm;
        plan(vidx).lineCount = 0;
        plan(vidx).totAmt    = 0;
      endif;

      if plan(vidx).lineCount >= %elem(plan(vidx).lines);
        writeMsg('Too many lines for vendor ' + %trim(cVndCd)
               + ' -- capping.');
        iter;
      endif;

      plan(vidx).lineCount += 1;
      k = plan(vidx).lineCount;
      plan(vidx).lines(k).reqnbr  = reqs(i).reqnbr;
      plan(vidx).lines(k).reqline = cReqLine;
      plan(vidx).lines(k).item    = cItem;
      plan(vidx).lines(k).qty     = cQty;
      plan(vidx).lines(k).uom     = cUom;
      plan(vidx).lines(k).price   = cPrice;
      plan(vidx).totAmt          += cQty * cPrice;
      lineCnt += 1;
    enddo;
    exec sql close rc2;
  endfor;
endsr;

// ---------------------------------------------------------------------
// showPreview -- populate SPREV plain-record fields for up to 5
// vendor buckets, EXFMT, wait for F6 (confirm) or F12 (back).
// ---------------------------------------------------------------------
begsr showPreview;
  pselcnt = selCnt;
  plncnt  = lineCnt;
  ppocnt  = numPlan;

  *in60 = *off;
  *in61 = *off;
  *in62 = *off;
  *in63 = *off;
  *in64 = *off;
  *in34 = *off;

  if numPlan >= 1;
    *in60 = *on;
    p1vndr  = plan(1).vndcd;
    p1vndnm = plan(1).vndnm;
    p1lns   = plan(1).lineCount;
    p1tot   = plan(1).totAmt;
  endif;
  if numPlan >= 2;
    *in61 = *on;
    p2vndr  = plan(2).vndcd;
    p2vndnm = plan(2).vndnm;
    p2lns   = plan(2).lineCount;
    p2tot   = plan(2).totAmt;
  endif;
  if numPlan >= 3;
    *in62 = *on;
    p3vndr  = plan(3).vndcd;
    p3vndnm = plan(3).vndnm;
    p3lns   = plan(3).lineCount;
    p3tot   = plan(3).totAmt;
  endif;
  if numPlan >= 4;
    *in63 = *on;
    p4vndr  = plan(4).vndcd;
    p4vndnm = plan(4).vndnm;
    p4lns   = plan(4).lineCount;
    p4tot   = plan(4).totAmt;
  endif;
  if numPlan >= 5;
    *in64 = *on;
    p5vndr  = plan(5).vndcd;
    p5vndnm = plan(5).vndnm;
    p5lns   = plan(5).lineCount;
    p5tot   = plan(5).totAmt;
  endif;
  if numPlan > 5;
    *in34 = *on;
  endif;

  *in40 = *off;
  if msgrrn > 0;
    *in40 = *on;
  endif;
  write rmsgctl;
  exfmt sprev;
endsr;

// ---------------------------------------------------------------------
// createPOs -- one PO per vendor bucket. Snapshots buyer_code from
// vendor; allocates po_number from docseq_next('PO'); inserts po_header
// + po_line rows; each po_line carries source_requisition_number +
// source_requisition_line_number for the derived converted-status
// lookup.
// ---------------------------------------------------------------------
begsr createPOs;
  poCount = 0;

  for v = 1 to numPlan;
    if plan(v).lineCount = 0;
      iter;
    endif;

    // Copy vendor bucket into scalars (SQLRPGLE precompiler doesn't
    // accept :plan(v).field as a host variable -- SQL0312).
    xVndCd  = plan(v).vndcd;
    xTotAmt = plan(v).totAmt;

    // Snapshot buyer from vendor.
    exec sql
      select buyer_code
        into :xBuyer
        from perpdemo.vendor
       where company_code = :compcd
         and vendor_code  = :xVndCd;
    if sqlcode <> 0;
      writeMsg('Vendor ' + %trim(xVndCd)
             + ' lookup failed: SQLCODE=' + %char(sqlcode));
      exec sql rollback;
      leavesr;
    endif;

    newPo = docseq_next(compcd : 'PO' : docerrmsg);
    if newPo = 0;
      writeMsg('docseq_next failed: ' + docerrmsg);
      exec sql rollback;
      leavesr;
    endif;

    exec sql
      insert into perpdemo.po_header
        (company_code, po_number, vendor_code, buyer_code,
         status_code, total_amount)
        values (:compcd, :newPo, :xVndCd, :xBuyer,
                'OPEN', :xTotAmt);
    if sqlcode < 0;
      writeMsg('po_header insert failed: SQLCODE=' + %char(sqlcode)
             + ' STATE=' + sqlstate);
      exec sql rollback;
      leavesr;
    endif;

    lineNbr = 0;
    for L = 1 to plan(v).lineCount;
      lineNbr += 1;
      xItem    = plan(v).lines(L).item;
      xQty     = plan(v).lines(L).qty;
      xUom     = plan(v).lines(L).uom;
      xPrice   = plan(v).lines(L).price;
      xReqNbr  = plan(v).lines(L).reqnbr;
      xReqLine = plan(v).lines(L).reqline;
      exec sql
        insert into perpdemo.po_line
          (company_code, po_number, line_number, item_number,
           ordered_qty, uom_code, unit_price,
           source_requisition_number, source_requisition_line_number)
          values (:compcd, :newPo, :lineNbr,
                  :xItem, :xQty, :xUom, :xPrice,
                  :xReqNbr, :xReqLine);
      if sqlcode < 0;
        writeMsg('po_line insert failed for vendor '
               + %trim(plan(v).vndcd) + ': SQLCODE='
               + %char(sqlcode) + ' STATE=' + sqlstate);
        exec sql rollback;
        leavesr;
      endif;
    endfor;

    poCount += 1;
  endfor;

  writeMsg('Created ' + %char(poCount) + ' PO(s) from '
         + %char(selCnt) + ' requisition(s) ('
         + %char(lineCnt) + ' line(s)).');
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
