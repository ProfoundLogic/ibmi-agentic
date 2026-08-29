**free

// ---------------------------------------------------------------------
// Program: poentr  (Manual Purchase Order Entry)
// Purpose: DSPF-based manual PO entry, scoped by the company selected
//          via perpselr (*LDA positions 1-3). Header screen collects
//          vendor / order date / notes; buyer_code is snapshotted from
//          vendor at header commit. Currency is hard-coded to USD --
//          the code_master CURRENCY lookup only has 'USD' in it today
//          and there's no per-company override yet. Allocates the doc
//          number via docseq_next('PO'), then inserts a DRAFT header.
//          Line screen is a subfile of po_line rows -- F6=Add opens an
//          edit panel that defaults UOM from item and unit_price from
//          the preferred vendor's current item_vendor_price row.
//          F8=Submit requires >=1 line and flips status to OPEN;
//          no further changes are allowed once submitted.
// Epic:    PERP-7 (PERP-38)
// ---------------------------------------------------------------------

// eexpdt (PERP-92/PERP-78) uses 1940-01-01 as its "no expected receipt
// date" sentinel. This field is DATFMT(*MDY) on poentd.dspf (so the
// screen renders MM/DD/YY per PERP-78) -- that DDS-level format binds
// the underlying variable's valid year range to 1940-2039 REGARDLESS
// of this program's own datfmt(*iso) ctl-opt, which only governs Date
// variables NOT tied to a display-file field. An earlier attempt used
// 0001-01-01 as the sentinel and crashed with RNQ0114 the moment F6=Add
// assigned it to eexpdt -- confirmed live. 1940-01-01 is both a real,
// in-range *MDY date and obviously never a genuine business date, so
// it works as a sentinel without fighting the field's own DDS format.
ctl-opt dftactgrp(*no) actgrp(*new) bnddir('PERP') datfmt(*iso);

dcl-f poentd workstn sfile(plsfl:rrn) sfile(pmsgsfl:msgrrn);

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

// Standard, reusable Item/Vendor Number prompts (PERP-51/PERP-56/PERP-70).
// Same dynamic CALL idiom as wrkitmr's callWrkcnvr/callWrklotr.
dcl-pr callItmprmt extpgm('ITMPRMT');
  pCompcd char(3)     const;
  pItem   varchar(25);
end-pr;

dcl-pr callVndprmt extpgm('VNDPRMT');
  pCompcd char(3)     const;
  pVendor varchar(10);
end-pr;

dcl-ds statusDS psds qualified;
  programName char(10) pos(334);
end-ds;

dcl-ds lineRow qualified;
  lnbr   int(10);
  item   varchar(25);
  qty    packed(15:4);
  uom    varchar(5);
  price  packed(15:4);
end-ds;

dcl-ds rows likeds(lineRow) dim(500);
dcl-s numRows   int(10);
dcl-s i         int(10);
dcl-s rrn       int(10);
dcl-s msgrrn    int(10);
dcl-s selRrn    int(10);
dcl-s changeRrn int(10);
dcl-s selOpt    char(1);
dcl-s compcd    char(3);
dcl-s ponbr     int(20);
dcl-s docerrmsg varchar(80);
dcl-s poStatus  varchar(20);
dcl-s nextLine  int(10);
dcl-s chgLnbr   int(10);
dcl-s cnt       int(10);
dcl-s edefuom   varchar(5);
dcl-s edefprice packed(15:4);
dcl-s edesc     varchar(60);
dcl-s hbuyer    char(10);
dcl-s hvname    varchar(60);
dcl-s promptItem varchar(25);
dcl-s promptVendor varchar(10);
dcl-s doneAll   ind;
dcl-s holdMsg   ind;

in ldaDS;
compcd = ldaDS.compcd;

if compcd = '';
  exsr clearMsgs;
  writeMsg('No company selected - run Select Company (PERPSELR) first.');
  *in40 = *on;
  write pmsgctl;
  hcompdsp = '';
  exfmt phead;
  *inlr = *on;
  return;
endif;

hcompdsp = compcd;

// -----------------------------------------------------------------------
// Header entry -- collect vendor / order_date / notes, validate,
// snapshot buyer from vendor, allocate the doc number, insert the
// DRAFT header. Currency is hard-coded to USD (see program header).
// Wrapped in an outer loop (PERP-93) so a successful F8=Submit on the
// line screen below returns here for the NEXT PO instead of leaving
// the user stranded on the now-read-only submitted line list -- same
// pattern as reqentr.sqlrpgle (PERP-86).
// -----------------------------------------------------------------------
doneAll = *off;

dow not doneAll;
  hvndcd  = '';
  hvndnm  = '';
  hbuycd  = '';
  horddt  = %date();
  hnotes  = '';

  // PERP-93: a submit confirmation queued just before restarting this
  // loop must survive one pass before clearMsgs wipes it -- same
  // holdMsg pattern as reqentr.sqlrpgle (PERP-86).
  if holdMsg;
    holdMsg = *off;
  else;
    exsr clearMsgs;
  endif;

  dow '1';
    *in40 = *off;
    if msgrrn > 0;
      *in40 = *on;
    endif;
    write pmsgctl;
    exfmt phead;

    if *in03 or *in12;
      *inlr = *on;
      return;
    endif;

    exsr clearMsgs;

    if %trim(hvndcd) = '?';
      promptVendor = hvndcd;
      callVndprmt(compcd : promptVendor);
      hvndcd = promptVendor;
      iter;
    endif;

    if %trim(hvndcd) = '';
      writeMsg('Vendor Code is required.');
      iter;
    endif;

    exec sql
      select vendor_name, buyer_code
        into :hvname, :hbuyer
        from perpdemo.vendor
       where company_code = :compcd and vendor_code = :hvndcd
         and is_active = 'Y';
    if sqlcode <> 0;
      writeMsg('Vendor ' + %trim(hvndcd) + ' not found for this company.');
      iter;
    endif;

    hvndnm = hvname;
    hbuycd = hbuyer;

    leave;
  enddo;

ponbr = docseq_next(compcd : 'PO' : docerrmsg);
if ponbr = 0;
  writeMsg('Could not allocate PO number: ' + docerrmsg);
  *inlr = *on;
  return;
endif;

// currency_code defaults to 'USD' via the po_header DDL default; leave
// it out of the column list so any future default change lands here
// too.
exec sql
  insert into perpdemo.po_header
    (company_code, po_number, vendor_code, buyer_code, order_date,
     notes)
    values (:compcd, :ponbr, :hvndcd, :hbuycd, :horddt, :hnotes);
if sqlcode < 0;
  writeMsg('Could not create PO: SQLCODE=' + %char(sqlcode));
  *inlr = *on;
  return;
endif;

exec sql commit;

// -----------------------------------------------------------------------
// Line entry -- subfile of po_line rows.
// -----------------------------------------------------------------------
exsr clearMsgs;
writeMsg('PO ' + %char(ponbr) + ' created (DRAFT) for vendor '
       + %trim(hvndcd) + '. Add lines, then F8=Submit.');

dow '1';
  exsr loadHeader;
  exsr loadLines;

  if numRows = 0;
    *in30 = *off;
    write pnolin;
  else;
    exsr fillSubfile;
    *in30 = *on;
  endif;

  write plfoot;
  *in40 = *off;
  if msgrrn > 0;
    *in40 = *on;
  endif;
  write pmsgctl;
  exfmt plctl;

  // PERP-94: F3 exits the program; F12 must NOT -- it should just step
  // back to (redisplay) this line list rather than ending the whole
  // flow. The PO header is already committed at this point, so
  // there's no earlier screen to unwind into.
  if *in03;
    doneAll = *on;
    leave;
  endif;
  if *in12;
    iter;
  endif;

  exsr clearMsgs;

  if *in06;
    if poStatus <> 'DRAFT';
      writeMsg('PO already submitted - no further changes allowed.');
    else;
      exsr addLine;
    endif;
    iter;
  endif;

  if *in08;
    if numRows = 0;
      writeMsg('At least one line is required before submitting.');
    elseif poStatus <> 'DRAFT';
      writeMsg('PO already submitted.');
    else;
      exec sql
        update perpdemo.po_header
           set status_code = 'OPEN',
               updated_at  = current_timestamp,
               updated_by  = user
         where company_code = :compcd and po_number = :ponbr;
      if sqlcode < 0;
        writeMsg('Submit failed: SQLCODE=' + %char(sqlcode));
      else;
        exec sql commit;
        // PERP-93: return to a fresh header entry screen for the next
        // PO instead of staying parked on this now read-only line
        // list. holdMsg carries this confirmation through to the
        // restarted header loop above.
        writeMsg('PO ' + %char(ponbr) + ' submitted (OPEN).');
        holdMsg = *on;
        leave;
      endif;
    endif;
    iter;
  endif;

  // Drain the READC loop before switching to PLEDIT for a change
  // (option 2). Same fix pattern as reqentr.sqlrpgle / DDL_STYLE_GUIDE
  // Sec.14 -- collect the row to change while draining, act on it after
  // the loop finishes.
  if numRows > 0;
    selRrn    = 0;
    selOpt    = ' ';
    changeRrn = 0;
    readc plsfl;
    dow not %eof(poentd);
      if slopt <> '';
        selRrn = rrn;
        selOpt = slopt;
        exsr handleOpt;
        selRrn = 0;
      endif;
      readc plsfl;
    enddo;

    if changeRrn > 0 and msgrrn = 0;
      selRrn = changeRrn;
      exsr changeLine;
    endif;
  endif;

  enddo;
enddo;

*inlr = *on;
return;

// ---------------------------------------------------------------------
begsr loadHeader;
  exec sql
    select h.status_code, h.vendor_code, v.vendor_name, h.buyer_code,
           h.order_date, h.total_amount
      into :poStatus, :dvndcd, :dvndnm, :dbuycd, :dorddt, :dtotamt
      from perpdemo.po_header h
      join perpdemo.vendor v
        on v.company_code = h.company_code and v.vendor_code = h.vendor_code
     where h.company_code = :compcd and h.po_number = :ponbr;
  dponbr  = %char(ponbr);
  dstatus = poStatus;
endsr;

// ---------------------------------------------------------------------
begsr loadLines;
  numRows = 0;
  exec sql declare pc1 cursor for
    select line_number, item_number, ordered_qty, uom_code, unit_price
      from perpdemo.po_line
     where company_code = :compcd and po_number = :ponbr
     order by line_number;
  exec sql open pc1;
  if sqlcode < 0;
    writeMsg('SQL open failed: SQLCODE=' + %char(sqlcode));
    leavesr;
  endif;

  dow numRows < %elem(rows);
    exec sql fetch pc1 into :lineRow.lnbr, :lineRow.item, :lineRow.qty,
                            :lineRow.uom, :lineRow.price;
    if sqlcode = 100 or sqlcode < 0;
      leave;
    endif;
    numRows += 1;
    rows(numRows) = lineRow;
  enddo;
  exec sql close pc1;
endsr;

// ---------------------------------------------------------------------
begsr fillSubfile;
  rrn = 0;
  *in31 = *on;
  write plctl;
  *in31 = *off;
  for i = 1 to numRows;
    slopt   = '';
    slline  = rows(i).lnbr;
    slitem  = rows(i).item;
    slqty   = rows(i).qty;
    sluom   = rows(i).uom;
    slprice = rows(i).price;
    rrn += 1;
    write plsfl;
  endfor;
endsr;

// ---------------------------------------------------------------------
begsr handleOpt;
  chain selRrn plsfl;
  if %found(poentd);
    if poStatus <> 'DRAFT';
      writeMsg('PO already submitted - no further changes allowed.');
      leavesr;
    endif;
    select;
      when selOpt = '2';
        if changeRrn = 0;
          changeRrn = selRrn;
        else;
          writeMsg('Only one line may be changed per Enter.');
        endif;
      when selOpt = '4';
        exsr deleteLine;
      other;
        writeMsg('Option ' + selOpt + ' not valid.');
    endsl;
  endif;
endsr;

// ---------------------------------------------------------------------
begsr addLine;
  emode   = 'A';
  eitem   = '';
  eitmdsc = '';
  eqty    = 0;
  euom    = '';
  eprice  = 0;
  // Sentinel "no expected receipt date". %date() with no format uses
  // the job DATFMT which on this environment is *MDY (year range
  // 1940-2039), so '0001' triggers RNQ0114. Force *ISO.
  eexpdt  = %date('1940-01-01' : *ISO);

  dow '1';
    exfmt pledit;
    if *in12;
      leavesr;
    endif;

    // Item Number prompt (PERP-62): '?' + Enter invokes the standard
    // reusable Item Number lookup (PERP-56) and returns the selection.
    if %trim(eitem) = '?';
      promptItem = eitem;
      callItmprmt(compcd : promptItem);
      eitem = promptItem;
      iter;
    endif;

    if %trim(eitem) = '';
      writeMsg('Item Number is required.');
      iter;
    endif;

    exec sql
      select item_description, inventory_uom
        into :edesc, :edefuom
        from perpdemo.item
       where company_code = :compcd and item_number = :eitem;
    if sqlcode <> 0;
      writeMsg('Item ' + %trim(eitem) + ' not found for this company.');
      iter;
    endif;

    eitmdsc = edesc;

    if eqty <= 0;
      writeMsg('Quantity must be greater than zero.');
      iter;
    endif;

    if %trim(euom) = '';
      euom = edefuom;
    endif;

    if eprice = 0;
      // Default from this vendor's current price row for the item.
      exec sql
        select unit_price
          into :edefprice
          from perpdemo.item_vendor_price
         where company_code = :compcd
           and item_number  = :eitem
           and vendor_code  = :hvndcd
           and effective_to is null
         fetch first 1 row only;
      if sqlcode = 0;
        eprice = edefprice;
      endif;
    endif;

    leave;
  enddo;

  exec sql
    select coalesce(max(line_number), 0) + 1
      into :nextLine
      from perpdemo.po_line
     where company_code = :compcd and po_number = :ponbr;

  if eexpdt = %date('1940-01-01' : *ISO);
    exec sql
      insert into perpdemo.po_line
        (company_code, po_number, line_number, item_number,
         ordered_qty, uom_code, unit_price)
        values (:compcd, :ponbr, :nextLine, :eitem, :eqty, :euom, :eprice);
  else;
    exec sql
      insert into perpdemo.po_line
        (company_code, po_number, line_number, item_number,
         ordered_qty, uom_code, unit_price, expected_receipt_date)
        values (:compcd, :ponbr, :nextLine, :eitem, :eqty, :euom, :eprice,
                :eexpdt);
  endif;
  if sqlcode < 0;
    writeMsg('Add line failed: SQLCODE=' + %char(sqlcode)
           + ' SQLSTATE=' + sqlstate);
  else;
    exec sql commit;
    writeMsg('Added line ' + %char(nextLine) + ': ' + %trim(edesc)
           + ' (' + %trim(euom) + ' @ ' + %char(eprice) + ').');
    exsr recalcTotal;
  endif;
endsr;

// ---------------------------------------------------------------------
begsr changeLine;
  chgLnbr = rows(selRrn).lnbr;
  emode   = 'C';
  eitem   = rows(selRrn).item;
  eqty    = rows(selRrn).qty;
  euom    = rows(selRrn).uom;
  eprice  = rows(selRrn).price;

  exec sql
    select item_description
      into :edesc
      from perpdemo.item
     where company_code = :compcd and item_number = :eitem;
  if sqlcode = 0;
    eitmdsc = edesc;
  else;
    eitmdsc = '';
  endif;

  dow '1';
    exfmt pledit;
    if *in12;
      leavesr;
    endif;

    // Item Number prompt (PERP-62): '?' + Enter invokes the standard
    // reusable Item Number lookup (PERP-56) and returns the selection.
    if %trim(eitem) = '?';
      promptItem = eitem;
      callItmprmt(compcd : promptItem);
      eitem = promptItem;
      exec sql
        select item_description
          into :edesc
          from perpdemo.item
         where company_code = :compcd and item_number = :eitem;
      if sqlcode = 0;
        eitmdsc = edesc;
      else;
        eitmdsc = '';
      endif;
      iter;
    endif;

    leave;
  enddo;

  if eqty <= 0;
    writeMsg('Quantity must be greater than zero.');
    leavesr;
  endif;

  if %trim(euom) = '';
    writeMsg('UOM is required.');
    leavesr;
  endif;

  exec sql
    update perpdemo.po_line
       set ordered_qty  = :eqty,
           uom_code     = :euom,
           unit_price   = :eprice,
           updated_at   = current_timestamp,
           updated_by   = user
     where company_code = :compcd and po_number = :ponbr
       and line_number  = :chgLnbr;
  if sqlcode < 0;
    writeMsg('Change failed: SQLCODE=' + %char(sqlcode));
  else;
    exec sql commit;
    writeMsg('Updated line ' + %char(chgLnbr) + '.');
    exsr recalcTotal;
  endif;
endsr;

// ---------------------------------------------------------------------
begsr deleteLine;
  exec sql
    delete from perpdemo.po_line
     where company_code = :compcd and po_number = :ponbr
       and line_number  = :slline;
  if sqlcode < 0;
    writeMsg('Delete failed: SQLSTATE=' + sqlstate);
  else;
    exec sql commit;
    writeMsg('Deleted line ' + %char(slline) + '.');
    exsr recalcTotal;
  endif;
endsr;

// ---------------------------------------------------------------------
begsr recalcTotal;
  exec sql
    update perpdemo.po_header
       set total_amount =
             (select coalesce(sum(ordered_qty * unit_price), 0)
                from perpdemo.po_line
               where company_code = :compcd
                 and po_number   = :ponbr),
           updated_at = current_timestamp,
           updated_by = user
     where company_code = :compcd and po_number = :ponbr;
  exec sql commit;
endsr;

// ---------------------------------------------------------------------
begsr clearMsgs;
  msgrrn = 0;
  *in41 = *on;
  write pmsgctl;
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
  write pmsgsfl;
end-proc;
