**free

// ---------------------------------------------------------------------
// Program: wrklotr  (Work with Item Lots)
// Purpose: DSPF-based CRUD for item_lot, scoped by the company selected
//          via perpselr (*LDA positions 1-3) and an item number entered
//          on screen (same scoping idiom as wrkcnvr). Shows item.on_hand
//          alongside SUM(item_lot.qty_on_hand) and flags a discrepancy
//          -- the entry point for the reconciliation demo (Option C:
//          balances denormalized on both item and item_lot by design).
//          Callable standalone or pre-scoped by passing company/item
//          (mirrors wrkcnvr's PERP-23 integration).
// Epic:    PERP-3 (PERP-24)
// ---------------------------------------------------------------------

// PERP-84: datfmt(*iso) is required now that this program declares
// Date-typed variables (parsedRecv/parsedExpd below) to validate the
// MM/DD/YY Received/Expiry Date entry fields. Without this override the
// job's *MDY (1940-2039) DATFMT becomes the Date variables' storage
// format, per perp/AGENTS.md's RNQ0114 gotcha.
ctl-opt dftactgrp(*no) actgrp(*new) datfmt(*iso);

dcl-pi *n;
  pCompcd char(3)      const options(*nopass);
  pItem   varchar(25)  const options(*nopass);
end-pi;

dcl-f wrklotd workstn sfile(ltsfl:rrn) sfile(ltmsgsfl:msgrrn);

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

// Standard, reusable Item Number prompt (PERP-51/PERP-56). Same dynamic
// CALL idiom as wrkitmr's callWrkcnvr/callWrklotr.
dcl-pr callItmprmt extpgm('ITMPRMT');
  pCompcd char(3)     const;
  pItem   varchar(25);
end-pr;

dcl-ds statusDS psds qualified;
  programName char(10) pos(334);
end-ds;

dcl-ds ltRow qualified;
  lot     varchar(20);
  qty     packed(15:4);
  recv    varchar(10);
  expd    varchar(10);
end-ds;

dcl-ds rows likeds(ltRow) dim(500);
dcl-s numRows  int(10);
dcl-s i        int(10);
dcl-s rrn      int(10);
dcl-s msgrrn   int(10);
dcl-s msgkey   char(4);
dcl-s selRrn   int(10);
dcl-s selOpt   char(1);
dcl-s filter   varchar(25);
dcl-s compcd   char(3);
dcl-s itemOh   packed(15:4);
dcl-s lotTotal packed(15:4);
dcl-s promptItem varchar(25);

// PERP-84: MM/DD/YY entry validation for ERECV/EEXPD (see editLoop).
// parsedRecv/parsedExpd are Date-typed working vars used only to
// validate/reformat the typed text -- they are never bound directly to
// an SQL host variable (erecv/eexpd stay char(10) for that), so the
// SQL-precompiler-intermediate-host-variable *MDY cap documented in
// perp/AGENTS.md #13 does not come into play here.
dcl-s parsedRecv date;
dcl-s parsedExpd date;
dcl-s validRecv  ind;
dcl-s validExpd  ind;

in ldaDS;
compcd = ldaDS.compcd;

if %parms >= 1 and pCompcd <> '';
  compcd = pCompcd;
endif;
scompdsp = compcd;
filter = '';
if %parms >= 2 and pItem <> '';
  filter = pItem;
  sfitem = pItem;
else;
  sfitem = '';
endif;

if compcd = '';
  exsr clearMsgs;
  writeMsg('No company selected - run Select Company (PERPSELR) first.');
endif;

dow not *in03 and not *in12;
  if compcd = '';
    *in30 = *off;
    write ltnoitem;
    write ltfoot;
    if msgrrn > 0;
      *in40 = *on;
      write ltmsgctl;
    else;
      *in40 = *off;
    endif;
    exfmt ltctl;
    leave;
  endif;

  exsr clearMsgs;

  if filter = '';
    *in30 = *off;
    *in60 = *off;
    numRows = 0;
    sioh = 0;
    slottot = 0;
    write ltnoitem;
  else;
    exsr loadBalances;
    exsr loadRows;
    if numRows = 0;
      *in30 = *off;
      write ltnone;
    else;
      exsr fillSubfile;
      *in30 = *on;
    endif;
  endif;

  write ltfoot;
  if msgrrn > 0;
    *in40 = *on;
    write ltmsgctl;
  else;
    *in40 = *off;
  endif;
  exfmt ltctl;

  if *in03 or *in12;
    leave;
  endif;

  if *in05;
    filter = sfitem;
    iter;
  endif;

  if *in06;
    if sfitem = '';
      writeMsg('Enter an item number before adding a lot.');
    else;
      filter = sfitem;
      exsr addRow;
    endif;
    iter;
  endif;

  // Item Number prompt (PERP-60): '?' + Enter invokes the standard
  // reusable Item Number lookup (PERP-56) and returns the selection.
  if %trim(sfitem) = '?';
    promptItem = sfitem;
    callItmprmt(compcd : promptItem);
    sfitem = promptItem;
    filter = promptItem;
    iter;
  endif;

  // Refresh scope from screen entry
  if sfitem <> filter;
    filter = sfitem;
    iter;
  endif;

  // Process subfile options. Guard on numRows: READC against a subfile
  // that was never written to this cycle (0 rows loaded) raises a
  // "Session or device error" (CPF5006-class) runtime error instead of
  // just returning *EOF.
  if numRows > 0;
    selRrn = 0;
    selOpt = ' ';
    readc ltsfl;
    dow not %eof(wrklotd);
      if sopt <> '';
        selRrn = rrn;
        selOpt = sopt;
        exsr handleOpt;
        selRrn = 0;
      endif;
      readc ltsfl;
    enddo;
  endif;

enddo;

*inlr = *on;
return;

// ---------------------------------------------------------------------
begsr loadBalances;
  exec sql
    select qty_on_hand into :itemOh
      from perpdemo.item
     where company_code = :compcd and item_number = :filter;
  if sqlcode <> 0;
    itemOh = 0;
  endif;
  exec sql
    select coalesce(sum(qty_on_hand), 0) into :lotTotal
      from perpdemo.item_lot
     where company_code = :compcd and item_number = :filter;
  sioh    = itemOh;
  slottot = lotTotal;
  if itemOh <> lotTotal;
    *in60 = *on;
  else;
    *in60 = *off;
  endif;
endsr;

// ---------------------------------------------------------------------
begsr loadRows;
  numRows = 0;
  // PERP-84: display as MM/DD/YY. received_date/expiry_date are stored
  // as native DATE columns but rendered here as plain strings (SRECV/
  // SEXPD carry no DATFMT keyword), so the format has to be built by
  // hand from the ISO string -- DB2 for i's CHAR(date,fmt) built-in
  // formats (ISO/USA/EUR/JIS) all use a 4-digit year, none produce a
  // 2-digit year directly. Same technique as wrkivpr.sqlrpgle (PERP-83).
  exec sql declare c1 cursor for
    select lot_number, qty_on_hand,
           substr(char(received_date, iso), 6, 2) || '/'
             || substr(char(received_date, iso), 9, 2) || '/'
             || substr(char(received_date, iso), 3, 2),
           case when expiry_date is null then ''
                else substr(char(expiry_date, iso), 6, 2) || '/'
                       || substr(char(expiry_date, iso), 9, 2) || '/'
                       || substr(char(expiry_date, iso), 3, 2)
           end
      from perpdemo.item_lot
     where company_code = :compcd and item_number = :filter
     order by lot_number;
  exec sql open c1;
  if sqlcode < 0;
    writeMsg('SQL open failed: SQLCODE=' + %char(sqlcode));
    leavesr;
  endif;

  dow numRows < %elem(rows);
    exec sql fetch c1 into :ltRow;
    if sqlcode = 100 or sqlcode < 0;
      leave;
    endif;
    numRows += 1;
    rows(numRows) = ltRow;
  enddo;
  exec sql close c1;
endsr;

// ---------------------------------------------------------------------
begsr fillSubfile;
  rrn = 0;
  *in31 = *on;
  write ltctl;
  *in31 = *off;
  for i = 1 to numRows;
    *in50 = *off;
    *in51 = *off;
    sopt  = '';
    slot  = rows(i).lot;
    sqty  = rows(i).qty;
    srecv = rows(i).recv;
    sexpd = rows(i).expd;
    rrn += 1;
    write ltsfl;
  endfor;
endsr;

// ---------------------------------------------------------------------
begsr handleOpt;
  chain selRrn ltsfl;
  if %found(wrklotd);
    select;
      when selOpt = '2';
        exsr changeRow;
      when selOpt = '4';
        exsr deleteRow;
      other;
        writeMsg('Option ' + selOpt + ' not valid.');
    endsl;
  endif;
endsr;

// ---------------------------------------------------------------------
begsr addRow;
  emode  = 'A';
  elot   = '';
  eqty   = 0;
  erecv  = %char(%date():*mdy);
  eexpd  = '';
  eactive = 'Y';
  exsr editLoop;
  if not *in12 and elot <> '';
    exec sql
      insert into perpdemo.item_lot
        (company_code, item_number, lot_number, qty_on_hand, received_date, expiry_date)
        values (:compcd, :filter, :elot, :eqty, date(:erecv),
                case when :eexpd = '' then null else date(:eexpd) end);
    if sqlcode < 0;
      writeMsg('Add failed: SQLCODE=' + %char(sqlcode)
             + ' SQLSTATE=' + sqlstate);
    else;
      writeMsg('Added lot ' + %trim(elot) + '.');
    endif;
  endif;
endsr;

// ---------------------------------------------------------------------
begsr changeRow;
  emode = 'C';
  elot  = slot;
  eqty  = sqty;
  erecv = srecv;
  eexpd = sexpd;
  eactive = 'Y';
  exsr editLoop;
  if not *in12;
    exec sql
      update perpdemo.item_lot
         set qty_on_hand   = :eqty,
             received_date = date(:erecv),
             expiry_date   = case when :eexpd = '' then null else date(:eexpd) end,
             updated_at    = current_timestamp,
             updated_by    = user
       where company_code = :compcd and item_number = :filter and lot_number = :elot;
    if sqlcode < 0;
      writeMsg('Change failed: SQLCODE=' + %char(sqlcode));
    else;
      writeMsg('Updated lot ' + %trim(elot) + '.');
    endif;
  endif;
endsr;

// ---------------------------------------------------------------------
begsr deleteRow;
  exec sql
    delete from perpdemo.item_lot
     where company_code = :compcd and item_number = :filter and lot_number = :slot;
  if sqlcode < 0;
    writeMsg('Delete failed: SQLSTATE=' + sqlstate);
  else;
    writeMsg('Deleted lot ' + %trim(slot) + '.');
  endif;
endsr;

// ---------------------------------------------------------------------
// PERP-84: erecv/eexpd are typed by the user as MM/DD/YY on LTEDIT.
// Validate on every Enter; on failure, show the error in the message
// subfile alongside LTEDIT and let the user retry (never crash into
// the SQL date(:erecv) cast in addRow/changeRow with unparsed text).
// On success, erecv/eexpd are normalized to ISO ('yyyy-mm-dd') text so
// the existing date(:erecv)/date(:eexpd) SQL casts in addRow/changeRow
// keep working unchanged.
begsr editLoop;
  exsr clearMsgs;
  dow *on;
    if msgrrn > 0;
      *in40 = *on;
      write ltmsgctl;
    else;
      *in40 = *off;
    endif;
    exfmt ltedit;

    if *in12;
      leave;
    endif;

    exsr clearMsgs;

    if %trim(erecv) = '';
      writeMsg('Received Date is required (MM/DD/YY).');
      iter;
    endif;

    validRecv = *on;
    monitor;
      parsedRecv = %date(%trim(erecv):*mdy);
    on-error;
      validRecv = *off;
    endmon;
    if not validRecv;
      writeMsg('Invalid Received Date - enter as MM/DD/YY.');
      iter;
    endif;

    validExpd = *on;
    if %trim(eexpd) <> '';
      monitor;
        parsedExpd = %date(%trim(eexpd):*mdy);
      on-error;
        validExpd = *off;
      endmon;
    endif;
    if not validExpd;
      writeMsg('Invalid Expiry Date - enter as MM/DD/YY, or blank for none.');
      iter;
    endif;

    erecv = %char(parsedRecv:*iso);
    if %trim(eexpd) <> '';
      eexpd = %char(parsedExpd:*iso);
    endif;

    leave;
  enddo;
endsr;

// ---------------------------------------------------------------------
begsr clearMsgs;
  msgrrn = 0;
  *in41 = *on;
  write ltmsgctl;
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
  write ltmsgsfl;
end-proc;
