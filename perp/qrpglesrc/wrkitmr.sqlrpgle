**free

// ---------------------------------------------------------------------
// Program: wrkitmr  (Work with Items -- item master maintenance)
// Purpose: DSPF-based CRUD for item, scoped by the company selected via
//          perpselr (*LDA positions 1-3). Subfile filters by class,
//          active-only, and low-stock-only (qty_on_hand <= reorder_point).
//          The edit panel covers descriptions/class/UOMs/lot flag,
//          read-only balances, reorder parameters, and location.
//          Balances are never written here -- per the epic, they're
//          adjusted via receipt/reconciliation programs (later epics).
//          Option 6 on the subfile calls wrkcnvr pre-scoped to the
//          selected item, per PERP-21's "called from item maintenance"
//          integration point.
// Epic:    PERP-3 (PERP-23)
// ---------------------------------------------------------------------

ctl-opt dftactgrp(*no) actgrp(*new);

dcl-f wrkitmd workstn sfile(wisfl:rrn) sfile(wimsgsfl:msgrrn);

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

dcl-pr callWrkcnvr extpgm('WRKCNVR');
  pCompcd char(3)     const;
  pItem   varchar(25) const;
end-pr;

dcl-pr callWrklotr extpgm('WRKLOTR');
  pCompcd char(3)     const;
  pItem   varchar(25) const;
end-pr;

// Standard, reusable Item Number prompt (PERP-51/PERP-56). Same dynamic
// CALL idiom as callWrkcnvr/callWrklotr above.
dcl-pr callItmprmt extpgm('ITMPRMT');
  pCompcd char(3)     const;
  pItem   varchar(25);
end-pr;

dcl-ds statusDS psds qualified;
  programName char(10) pos(334);
end-ds;

dcl-ds itemRow qualified;
  item    varchar(25);
  desc    varchar(60);
  class   varchar(10);
  lotctl  char(1);
  onhand  packed(15:4);
  rordpt  packed(15:4);
end-ds;

dcl-ds rows likeds(itemRow) dim(500);
dcl-s numRows  int(10);
dcl-s i        int(10);
dcl-s rrn      int(10);
dcl-s msgrrn   int(10);
dcl-s msgkey   char(4);
dcl-s selRrn   int(10);
dcl-s selOpt   char(1);
dcl-s compcd   char(3);
dcl-s fClass   varchar(10);
dcl-s fActOnly char(1);
dcl-s fLowOnly char(1);
dcl-s fPosTo   varchar(30);
dcl-s promptItem varchar(25);
dcl-s holdMsg   ind;
dcl-s validationFailed ind;

in ldaDS;
compcd = ldaDS.compcd;
scompdsp = compcd;
fClass   = '';
fActOnly = 'N';
fLowOnly = 'N';
fPosTo   = '';
sfclass  = '';
sfact    = 'N';
sflow    = 'N';
sposto   = '';

if compcd = '';
  exsr clearMsgs;
  writeMsg('No company selected - run Select Company (PERPSELR) first.');
endif;

dow not *in03 and not *in12;
  if compcd = '';
    *in30 = *off;
    write winoco;
    write wifoot;
    if msgrrn > 0;
      *in40 = *on;
      write wimsgctl;
    else;
      *in40 = *off;
    endif;
    exfmt wictl;
    leave;
  endif;

  // A message queued by an action handler below (2=Change, etc.) must
  // survive one full loop pass before being cleared, or it never
  // reaches the screen -- clearMsgs wipes msgrrn back to 0 on the very
  // next pass, before this pass's own exfmt ever shows it. holdMsg
  // skips exactly one clearMsgs call right after such a message was
  // queued. Same pattern as wrkivpr (PERP-74).
  if holdMsg;
    holdMsg = *off;
  else;
    exsr clearMsgs;
  endif;
  exsr loadRows;

  if numRows = 0;
    *in30 = *off;
    write winone;
  else;
    exsr fillSubfile;
    *in30 = *on;
  endif;

  write wifoot;
  if msgrrn > 0;
    *in40 = *on;
    write wimsgctl;
  else;
    *in40 = *off;
  endif;
  exfmt wictl;

  if *in03 or *in12;
    leave;
  endif;

  if *in05;
    fClass   = sfclass;
    fActOnly = sfact;
    fLowOnly = sflow;
    fPosTo   = %trim(sposto);
    iter;
  endif;

  if *in06;
    exsr addRow;
    iter;
  endif;

  // Refresh filters from screen entry
  if sfclass <> fClass or sfact <> fActOnly or sflow <> fLowOnly
     or sposto <> fPosTo;
    fClass   = sfclass;
    fActOnly = sfact;
    fLowOnly = sflow;
    fPosTo   = %trim(sposto);
    iter;
  endif;

  // Process subfile options. Guard on numRows: READC against a subfile
  // that was never written to this cycle (0 rows loaded) raises a
  // "Session or device error" (CPF5006-class) runtime error instead of
  // just returning *EOF.
  if numRows > 0;
    selRrn = 0;
    selOpt = ' ';
    readc wisfl;
    dow not %eof(wrkitmd);
      if sopt <> '';
        selRrn = rrn;
        selOpt = sopt;
        exsr handleOpt;
        selRrn = 0;
      endif;
      readc wisfl;
    enddo;
  endif;

enddo;

*inlr = *on;
return;

// ---------------------------------------------------------------------
begsr loadRows;
  numRows = 0;
  exec sql declare c1 cursor for
    select item_number, item_description, class_code, lot_controlled,
           qty_on_hand, reorder_point
      from perpdemo.item
     where company_code = :compcd
       and (:fClass   = '' or class_code = :fClass)
       and (:fActOnly = 'N' or is_active = 'Y')
       and (:fLowOnly = 'N' or qty_on_hand <= reorder_point)
       and (:fPosTo = ''
            or upper(item_number) like upper(:fPosTo) || '%'
            or upper(item_description) like '%' || upper(:fPosTo) || '%')
     order by item_number;
  exec sql open c1;
  if sqlcode < 0;
    writeMsg('SQL open failed: SQLCODE=' + %char(sqlcode));
    leavesr;
  endif;

  dow numRows < %elem(rows);
    exec sql fetch c1 into :itemRow;
    if sqlcode = 100 or sqlcode < 0;
      leave;
    endif;
    numRows += 1;
    rows(numRows) = itemRow;
  enddo;
  exec sql close c1;
endsr;

// ---------------------------------------------------------------------
begsr fillSubfile;
  rrn = 0;
  *in31 = *on;
  write wictl;
  *in31 = *off;
  for i = 1 to numRows;
    *in50 = *off;
    *in51 = *off;
    sopt    = '';
    siitem  = rows(i).item;
    sidesc  = rows(i).desc;
    siclass = rows(i).class;
    silot   = rows(i).lotctl;
    if rows(i).onhand <= rows(i).rordpt;
      silow = '*';
    else;
      silow = '';
    endif;
    rrn += 1;
    write wisfl;
  endfor;
endsr;

// ---------------------------------------------------------------------
begsr handleOpt;
  chain selRrn wisfl;
  if %found(wrkitmd);
    select;
      when selOpt = '2';
        exsr changeRow;
      when selOpt = '4';
        exsr deleteRow;
      when selOpt = '5';
        exsr displayRow;
      when selOpt = '6';
        callWrkcnvr(compcd : siitem);
      when selOpt = '7';
        callWrklotr(compcd : siitem);
      other;
        writeMsg('Option ' + selOpt + ' not valid.');
    endsl;
  endif;
endsr;

// ---------------------------------------------------------------------
begsr addRow;
  // Clear any message left over from a PRIOR action before this one queues
  // its own -- without this, pressing F6 again right after seeing a message
  // (before the outer loop's own clearMsgs ever runs) stacks a second
  // message behind the first, and the stale one shows instead of this
  // action's real result.
  exsr clearMsgs;
  emode   = 'A';
  // PERP-57: the item prompt makes no sense while adding a brand-new item
  // (there's nothing to look up yet) -- *in60 on hides the "(? = prompt)"
  // hint and skips the prompt-invocation check in editLoop below.
  *in60   = *on;
  eitem   = '';
  edesc   = '';
  eshort  = '';
  eclass  = fClass;
  einvuom = '';
  estkuom = '';
  elotctl = 'N';
  eqoh    = 0;
  eqavl   = 0;
  eqfrz   = 0;
  eqoo    = 0;
  erordpt = 0;
  ecritlv = 0;
  eminqty = 0;
  emaxqty = 0;
  esafstk = 0;
  eleadtm = 0;
  eaisle  = '';
  ebay    = '';
  eshelf  = '';
  // Loop so a failed insert redisplays THIS SAME panel with the error and
  // the user's own entries intact, instead of bouncing back to the list --
  // only a successful add or an explicit Cancel (F12) leaves the loop.
  dow *on;
    exsr editLoop;
    if *in12;
      leave;
    endif;
    // Clear the message the user just saw (if any) before queuing THIS
    // iteration's own result -- otherwise a retry within this same loop
    // stacks its message behind the previous iteration's, same as the
    // across-actions case clearMsgs at the top of this subroutine guards.
    exsr clearMsgs;
    exsr validateEdit;
    if validationFailed;
      holdMsg = *on;
      iter;
    endif;
    exec sql
      insert into perpdemo.item
        (company_code, item_number, item_description, short_description,
         class_code, inventory_uom, stocking_uom, lot_controlled,
         reorder_point, critical_level, min_qty, max_qty, safety_stock,
         lead_time_days, aisle_code, bay_code, shelf_code)
        values (:compcd, :eitem, :edesc, :eshort,
                :eclass, :einvuom, :estkuom, :elotctl,
                :erordpt, :ecritlv, :eminqty, :emaxqty, :esafstk,
                :eleadtm, :eaisle, :ebay, :eshelf);
    if sqlcode < 0;
      writeMsg('Add failed: SQLCODE=' + %char(sqlcode)
             + ' SQLSTATE=' + sqlstate);
      holdMsg = *on;
      iter;
    else;
      writeMsg('Added ' + %trim(eitem) + '.');
      holdMsg = *on;
      leave;
    endif;
  enddo;
endsr;

// ---------------------------------------------------------------------
begsr changeRow;
  exsr clearMsgs;
  emode = 'C';
  *in60 = *off;
  eitem = siitem;
  exec sql
    select item_description, short_description, class_code,
           inventory_uom, stocking_uom, lot_controlled,
           qty_on_hand, qty_available, qty_frozen, qty_on_order,
           reorder_point, critical_level, min_qty, max_qty, safety_stock,
           lead_time_days, aisle_code, bay_code, shelf_code
      into :edesc, :eshort, :eclass,
           :einvuom, :estkuom, :elotctl,
           :eqoh, :eqavl, :eqfrz, :eqoo,
           :erordpt, :ecritlv, :eminqty, :emaxqty, :esafstk,
           :eleadtm, :eaisle, :ebay, :eshelf
      from perpdemo.item
     where company_code = :compcd and item_number = :eitem;
  if sqlcode <> 0;
    writeMsg('Row disappeared before change.');
    holdMsg = *on;
    leavesr;
  endif;
  // Same retry-in-place shape as addRow: a failed update redisplays this
  // panel with the error and the user's entries intact.
  dow *on;
    exsr editLoop;
    if *in12;
      leave;
    endif;
    exsr clearMsgs;
    exsr validateEdit;
    if validationFailed;
      holdMsg = *on;
      iter;
    endif;
    exec sql
      update perpdemo.item
         set item_description  = :edesc,
             short_description = :eshort,
             class_code        = :eclass,
             inventory_uom     = :einvuom,
             stocking_uom      = :estkuom,
             lot_controlled    = :elotctl,
             reorder_point     = :erordpt,
             critical_level    = :ecritlv,
             min_qty           = :eminqty,
             max_qty           = :emaxqty,
             safety_stock      = :esafstk,
             lead_time_days    = :eleadtm,
             aisle_code        = :eaisle,
             bay_code          = :ebay,
             shelf_code        = :eshelf,
             updated_at        = current_timestamp,
             updated_by        = user
       where company_code = :compcd and item_number = :eitem;
    if sqlcode < 0;
      writeMsg('Change failed: SQLCODE=' + %char(sqlcode));
      holdMsg = *on;
      iter;
    else;
      writeMsg('Updated ' + %trim(eitem) + '.');
      holdMsg = *on;
      leave;
    endif;
  enddo;
endsr;

// ---------------------------------------------------------------------
begsr deleteRow;
  exsr clearMsgs;
  exec sql
    delete from perpdemo.item
     where company_code = :compcd and item_number = :siitem;
  if sqlcode < 0;
    writeMsg('Delete failed (in use?): SQLSTATE=' + sqlstate);
  else;
    writeMsg('Deleted ' + %trim(siitem) + '.');
  endif;
  holdMsg = *on;
endsr;

// ---------------------------------------------------------------------
begsr displayRow;
  emode = 'D';
  *in60 = *off;
  eitem = siitem;
  exec sql
    select item_description, short_description, class_code,
           inventory_uom, stocking_uom, lot_controlled,
           qty_on_hand, qty_available, qty_frozen, qty_on_order,
           reorder_point, critical_level, min_qty, max_qty, safety_stock,
           lead_time_days, aisle_code, bay_code, shelf_code
      into :edesc, :eshort, :eclass,
           :einvuom, :estkuom, :elotctl,
           :eqoh, :eqavl, :eqfrz, :eqoo,
           :erordpt, :ecritlv, :eminqty, :emaxqty, :esafstk,
           :eleadtm, :eaisle, :ebay, :eshelf
      from perpdemo.item
     where company_code = :compcd and item_number = :eitem;
  exsr editLoop;
endsr;

// ---------------------------------------------------------------------
begsr editLoop;
  dow not *in12;
    // Re-issue the message SFLCTL record before every exfmt on this format,
    // exactly like the outer loop does for wictl -- otherwise a message
    // queued by a failed insert/update (with the caller looping back into
    // this same edit panel to show it) never reaches the screen, and any
    // stale message left over from the calling list screen can bleed
    // through WIEDIT's OVERLAY instead of being explicitly cleared.
    *in40 = (msgrrn > 0);
    write wimsgctl;
    exfmt wiedit;
    if *in12;
      leave;
    endif;
    if not *in60 and %trim(eitem) = '?';
      promptItem = eitem;
      callItmprmt(compcd : promptItem);
      eitem = promptItem;
      iter;
    endif;
    leave;
  enddo;
endsr;

// ---------------------------------------------------------------------
// Required-field validation for the Add/Change panel, done here in RPG
// instead of letting a blank required field surface as a raw FK violation
// (e.g. ITEM_STKUOM_FK) from the database. Checked in screen order so the
// first message matches the first blank field the user would fix.
begsr validateEdit;
  validationFailed = *off;
  if %trim(eitem) = '';
    writeMsg('Item Number is required.');
    validationFailed = *on;
  elseif %trim(edesc) = '';
    writeMsg('Description is required.');
    validationFailed = *on;
  elseif %trim(eclass) = '';
    writeMsg('Class is required.');
    validationFailed = *on;
  elseif %trim(einvuom) = '';
    writeMsg('Inventory UOM is required.');
    validationFailed = *on;
  elseif %trim(estkuom) = '';
    writeMsg('Stocking UOM is required.');
    validationFailed = *on;
  endif;
endsr;

// ---------------------------------------------------------------------
begsr clearMsgs;
  msgrrn = 0;
  *in41 = *on;
  write wimsgctl;
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
  write wimsgsfl;
end-proc;
