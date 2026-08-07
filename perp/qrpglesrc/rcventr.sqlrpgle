**free

// ---------------------------------------------------------------------
// Program: rcventr  (PO Receipt Entry)
// Purpose: DSPF-based receipt entry, scoped by the company selected via
//          perpselr (*LDA positions 1-3). Header screen picks a PO
//          (must be OPEN or PARTIAL) and a receiver; allocates the doc
//          number via docseq_next('RCP') and inserts a POSTED header --
//          there is no DRAFT workflow here, unlike poentr/reqentr:
//          creating a receipt line IS the act of receiving.
//
//          Line screen is a subfile of the PO's still-open lines
//          (status_code <> CLOSED), joined to po_line_open for open_qty.
//          Per-line action is subfile Option 1=Receive, not F6=Add --
//          every receipt line originates from an existing open po_line,
//          unlike poentr where F6 creates a brand-new row from nothing.
//          Receiving a line: converts vendor UOM -> inventory UOM via
//          item_uom_conversion, upserts item_lot for lot-controlled
//          items, and rolls po_line.received_qty/status_code and
//          po_header.status_code forward -- the PO-status-transition-
//          from-receipts item PERP-7's recap deferred to this epic.
// Epic:    PERP-8 (PERP-43)
// ---------------------------------------------------------------------

// datfmt(*iso) kept for consistency with every other PERP program that
// touches Date fields (DDS DATFMT(*ISO) fields) -- no out-of-range
// sentinel date is used here (receipt_date always defaults to a real
// %date()), so the *MDY host-variable trap (DDL_STYLE_GUIDE Sec.13)
// does not apply to this program the way it did to poentr/pobrwr.
ctl-opt dftactgrp(*no) actgrp(*new) bnddir('PERP') datfmt(*iso);

dcl-f rcventd workstn sfile(rlsfl:rrn) sfile(rmsgsfl:msgrrn);

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

dcl-ds lineRow qualified;
  lnbr    int(10);
  item    varchar(25);
  desc    varchar(30);
  openqty packed(11:4);
  uom     varchar(5);
end-ds;

dcl-ds rows likeds(lineRow) dim(500);
dcl-s numRows   int(10);
dcl-s i         int(10);
dcl-s rrn       int(10);
dcl-s msgrrn    int(10);
dcl-s selRrn    int(10);
dcl-s selOpt    char(1);
dcl-s compcd    char(3);
dcl-s rcpnbr    int(20);
dcl-s docerrmsg varchar(80);
dcl-s hvndnmW   varchar(60);
dcl-s hrcvnmW   varchar(60);
dcl-s dvndnmW   varchar(60);
dcl-s invuom    varchar(5);
dcl-s convFactor packed(15:6);
dcl-s qtyInv     packed(15:4);
dcl-s nextRLine  int(10);
dcl-s newHdrStat varchar(20);

in ldaDS;
compcd = ldaDS.compcd;

if compcd = '';
  exsr clearMsgs;
  writeMsg('No company selected - run Select Company (PERPSELR) first.');
  *in40 = *on;
  write rmsgctl;
  hcompdsp = '';
  exfmt rhead;
  *inlr = *on;
  return;
endif;

hcompdsp = compcd;

// -----------------------------------------------------------------------
// Header entry -- pick PO, receipt date, receiver, notes. Validate the
// PO exists for this company and is still open to receive against.
// -----------------------------------------------------------------------
hponbr = 0;
hrcpdt = %date();
hrcvby = '';
hnotes = '';

exsr clearMsgs;

dow '1';
  *in40 = *off;
  if msgrrn > 0;
    *in40 = *on;
  endif;
  write rmsgctl;
  exfmt rhead;

  if *in03 or *in12;
    *inlr = *on;
    return;
  endif;

  exsr clearMsgs;

  if hponbr <= 0;
    writeMsg('PO Number is required.');
    iter;
  endif;

  exec sql
    select h.status_code, v.vendor_name
      into :hpostat, :hvndnmW
      from perpdemo.po_header h
      join perpdemo.vendor v
        on v.company_code = h.company_code and v.vendor_code = h.vendor_code
     where h.company_code = :compcd and h.po_number = :hponbr;
  if sqlcode <> 0;
    writeMsg('PO ' + %char(hponbr) + ' not found for this company.');
    iter;
  endif;
  hvndnm = hvndnmW;

  if hpostat <> 'OPEN' and hpostat <> 'PARTIAL';
    writeMsg('PO ' + %char(hponbr) + ' is ' + %trim(hpostat)
           + ' - not open for receiving.');
    iter;
  endif;

  if %trim(hrcvby) = '';
    writeMsg('Received By is required.');
    iter;
  endif;

  exec sql
    select display_name
      into :hrcvnmW
      from perpdemo.perp_user
     where user_code = :hrcvby;
  if sqlcode <> 0;
    writeMsg('Receiver ' + %trim(hrcvby) + ' not found.');
    iter;
  endif;
  hrcvnm = hrcvnmW;

  leave;
enddo;

rcpnbr = docseq_next(compcd : 'RCP' : docerrmsg);
if rcpnbr = 0;
  writeMsg('Could not allocate receipt number: ' + docerrmsg);
  *inlr = *on;
  return;
endif;

// status_code/status_type default to POSTED/RCPSTATUS via the DDL
// default -- left out of the column list so a future default change
// lands here too (same idiom poentr uses for po_header.currency_code).
exec sql
  insert into perpdemo.po_receipt
    (company_code, receipt_number, po_number, receipt_date, received_by,
     notes)
    values (:compcd, :rcpnbr, :hponbr, :hrcpdt, :hrcvby, :hnotes);
if sqlcode < 0;
  writeMsg('Could not create receipt: SQLCODE=' + %char(sqlcode));
  *inlr = *on;
  return;
endif;

exec sql commit;

// -----------------------------------------------------------------------
// Line entry -- subfile of the PO's still-open lines.
// -----------------------------------------------------------------------
exsr clearMsgs;
writeMsg('Receipt ' + %char(rcpnbr) + ' created for PO ' + %char(hponbr)
       + '. Type 1=Receive next to a line.');

dow '1';
  exsr loadHeaderRecap;
  exsr loadOpenLines;

  if numRows = 0;
    *in30 = *off;
    write rnolin;
  else;
    exsr fillSubfile;
    *in30 = *on;
  endif;

  write rlfoot;
  *in40 = *off;
  if msgrrn > 0;
    *in40 = *on;
  endif;
  write rmsgctl;
  exfmt rlctl;

  if *in03 or *in12;
    leave;
  endif;

  exsr clearMsgs;

  if numRows > 0;
    selRrn = 0;
    selOpt = ' ';
    readc rlsfl;
    dow not %eof(rcventd);
      if slopt <> '';
        if selRrn = 0;
          selRrn = rrn;
          selOpt = slopt;
        else;
          writeMsg('Only one selection per Enter.');
        endif;
      endif;
      readc rlsfl;
    enddo;

    if selRrn > 0 and msgrrn = 0;
      select;
        when selOpt = '1';
          exsr receiveLine;
        other;
          writeMsg('Option ' + selOpt + ' not valid.');
      endsl;
    endif;
  endif;
enddo;

*inlr = *on;
return;

// ---------------------------------------------------------------------
begsr loadHeaderRecap;
  exec sql
    select h.status_code, v.vendor_code, v.vendor_name
      into :hpostat, :dvndr, :dvndnmW
      from perpdemo.po_header h
      join perpdemo.vendor v
        on v.company_code = h.company_code and v.vendor_code = h.vendor_code
     where h.company_code = :compcd and h.po_number = :hponbr;
  dvndnm  = dvndnmW;
  dponbr  = %char(hponbr);
  drcvby  = hrcvby;
  drcvnm  = hrcvnm;
  drcpdt  = hrcpdt;
  dpostat = hpostat;
endsr;

// ---------------------------------------------------------------------
// loadOpenLines -- po_line rows still open (status_code <> CLOSED),
// joined to po_line_open for open_qty and to item for the description.
begsr loadOpenLines;
  numRows = 0;
  exec sql declare rc1 cursor for
    select l.line_number, l.item_number, i.item_description,
           o.open_qty, l.uom_code
      from perpdemo.po_line l
      join perpdemo.po_line_open o
        on o.company_code = l.company_code and o.po_number = l.po_number
       and o.line_number  = l.line_number
      join perpdemo.item i
        on i.company_code = l.company_code and i.item_number = l.item_number
     where l.company_code = :compcd and l.po_number = :hponbr
       and l.status_code <> 'CLOSED' and l.is_active = 'Y'
     order by l.line_number;
  exec sql open rc1;
  if sqlcode < 0;
    writeMsg('SQL open failed: SQLCODE=' + %char(sqlcode));
    return;
  endif;

  dow numRows < %elem(rows);
    exec sql fetch rc1 into :lineRow.lnbr, :lineRow.item, :lineRow.desc,
                            :lineRow.openqty, :lineRow.uom;
    if sqlcode = 100 or sqlcode < 0;
      leave;
    endif;
    numRows += 1;
    rows(numRows) = lineRow;
  enddo;
  exec sql close rc1;
endsr;

// ---------------------------------------------------------------------
begsr fillSubfile;
  rrn = 0;
  *in31 = *on;
  write rlctl;
  *in31 = *off;
  for i = 1 to numRows;
    slopt   = '';
    slline  = rows(i).lnbr;
    slitem  = rows(i).item;
    sldesc  = rows(i).desc;
    slopen  = rows(i).openqty;
    sluom   = rows(i).uom;
    rrn += 1;
    write rlsfl;
  endfor;
endsr;

// ---------------------------------------------------------------------
// receiveLine -- prompt for qty received (vendor UOM) + lot number
// (if lot-controlled), then post everywhere a receipt touches:
// po_receipt_line insert, item_lot upsert, po_line roll-up, item
// qty_on_hand bump, po_header status recalc.
begsr receiveLine;
  eline   = rows(selRrn).lnbr;
  eitem   = rows(selRrn).item;
  edesc   = rows(selRrn).desc;
  eopen   = rows(selRrn).openqty;
  euom    = rows(selRrn).uom;

  exec sql
    select lot_controlled, inventory_uom
      into :elotctl, :invuom
      from perpdemo.item
     where company_code = :compcd and item_number = :eitem;

  eqty    = 0;
  elot    = '';
  enotes  = '';

  dow '1';
    *in40 = *off;
    if msgrrn > 0;
      *in40 = *on;
    endif;
    write rmsgctl;
    exfmt rledit;
    if *in12;
      return;
    endif;

    exsr clearMsgs;

    if eqty <= 0;
      writeMsg('Quantity must be greater than zero.');
      iter;
    endif;

    if eqty > eopen;
      writeMsg('Quantity exceeds open quantity of ' + %char(eopen) + '.');
      iter;
    endif;

    if elotctl = 'Y' and %trim(elot) = '';
      writeMsg('Lot number is required for this lot-controlled item.');
      iter;
    endif;

    leave;
  enddo;

  if %trim(euom) = %trim(invuom);
    convFactor = 1;
  else;
    exec sql
      select conversion_factor
        into :convFactor
        from perpdemo.item_uom_conversion
       where company_code = :compcd and item_number = :eitem
         and from_uom = :euom and to_uom = :invuom;
    if sqlcode <> 0;
      writeMsg('No UOM conversion from ' + %trim(euom) + ' to '
             + %trim(invuom) + ' defined for this item.');
      return;
    endif;
  endif;

  qtyInv = eqty * convFactor;

  exec sql
    select coalesce(max(line_number), 0) + 1
      into :nextRLine
      from perpdemo.po_receipt_line
     where company_code = :compcd and receipt_number = :rcpnbr;

  if %trim(elot) = '';
    exec sql
      insert into perpdemo.po_receipt_line
        (company_code, receipt_number, line_number, po_number,
         po_line_number, item_number, qty_received_vendor_uom,
         qty_received_inventory_uom, uom_conversion_factor, notes)
        values (:compcd, :rcpnbr, :nextRLine, :hponbr, :eline,
                :eitem, :eqty, :qtyInv, :convFactor, :enotes);
  else;
    exec sql
      insert into perpdemo.po_receipt_line
        (company_code, receipt_number, line_number, po_number,
         po_line_number, item_number, qty_received_vendor_uom,
         qty_received_inventory_uom, uom_conversion_factor, lot_number,
         notes)
        values (:compcd, :rcpnbr, :nextRLine, :hponbr, :eline,
                :eitem, :eqty, :qtyInv, :convFactor, :elot, :enotes);
  endif;
  if sqlcode < 0;
    writeMsg('Receipt line insert failed: SQLCODE=' + %char(sqlcode)
           + ' SQLSTATE=' + sqlstate);
    return;
  endif;

  if elotctl = 'Y';
    exec sql
      update perpdemo.item_lot
         set qty_on_hand = qty_on_hand + :qtyInv,
             updated_at  = current_timestamp,
             updated_by  = user
       where company_code = :compcd and item_number = :eitem
         and lot_number = :elot;
    if sqlcode = 100;
      exec sql
        insert into perpdemo.item_lot
          (company_code, item_number, lot_number, qty_on_hand)
          values (:compcd, :eitem, :elot, :qtyInv);
    endif;
  endif;

  exec sql
    update perpdemo.po_line
       set received_qty = received_qty + :qtyInv,
           status_code  = case
             when received_qty + :qtyInv >= ordered_qty then 'CLOSED'
             when received_qty + :qtyInv > 0 then 'PARTIAL'
             else 'OPEN'
           end,
           updated_at = current_timestamp,
           updated_by = user
     where company_code = :compcd and po_number = :hponbr
       and line_number = :eline;
  if sqlcode < 0;
    writeMsg('po_line update failed: SQLCODE=' + %char(sqlcode));
    return;
  endif;

  exec sql
    update perpdemo.item
       set qty_on_hand = qty_on_hand + :qtyInv,
           updated_at  = current_timestamp,
           updated_by  = user
     where company_code = :compcd and item_number = :eitem;

  exec sql
    select case
             when count(*) = sum(case when status_code = 'CLOSED' then 1 else 0 end)
               then 'RECEIVED'
             when sum(case when received_qty > 0 then 1 else 0 end) > 0
               then 'PARTIAL'
             else 'OPEN'
           end
      into :newHdrStat
      from perpdemo.po_line
     where company_code = :compcd and po_number = :hponbr and is_active = 'Y';

  exec sql
    update perpdemo.po_header
       set status_code = :newHdrStat,
           updated_at  = current_timestamp,
           updated_by  = user
     where company_code = :compcd and po_number = :hponbr;

  exec sql commit;

  writeMsg('Received ' + %char(eqty) + ' ' + %trim(euom) + ' ('
         + %char(qtyInv) + ' ' + %trim(invuom) + ') on line '
         + %char(eline) + '.');
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
