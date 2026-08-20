**free

// ---------------------------------------------------------------------
// Program: wrkivpr  (Item-Vendor Price History / Add New Price)
// Purpose: Read-only history list for item_vendor_price scoped by an
//          item + vendor entered on screen, plus F6=Add to enter a new
//          current price. On add: the current row (effective_to IS
//          NULL) is closed (effective_to = CURRENT_DATE) and a new row
//          is inserted (effective_from = CURRENT_DATE, effective_to =
//          NULL, unit_price = entered value). Historical rows are
//          never updated or deleted here -- no subfile options exist.
//          A second price entered the same day hits the table's own
//          PRIMARY KEY (company_code, item_number, vendor_code,
//          effective_from) and is rejected by DB2 (SQLCODE -803) --
//          "one price change per day" falls out of the PK for free.
// Epic:    PERP-5 (PERP-31)
// ---------------------------------------------------------------------

ctl-opt dftactgrp(*no) actgrp(*new);

dcl-f wrkivpd workstn sfile(ipsfl:rrn) sfile(ipmsgsfl:msgrrn);

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

dcl-ds prcRow qualified;
  efffrm char(10);
  effto  char(10);
  price  packed(15:4);
  curr   varchar(20);
  src    varchar(20);
end-ds;

dcl-ds rows likeds(prcRow) dim(500);
dcl-s numRows  int(10);
dcl-s i        int(10);
dcl-s rrn      int(10);
dcl-s msgrrn   int(10);
dcl-s msgkey   char(4);
dcl-s compcd   char(3);
dcl-s fItem    varchar(25);
dcl-s fVendor  varchar(10);
dcl-s promptItem varchar(25);
dcl-s promptVendor varchar(10);
dcl-s holdMsg   ind;

in ldaDS;
compcd = ldaDS.compcd;
scompdsp = compcd;
fItem   = '';
fVendor = '';
sfitem   = '';
sfvendor = '';

if compcd = '';
  exsr clearMsgs;
  writeMsg('No company selected - run Select Company (PERPSELR) first.');
endif;

dow not *in03 and not *in12;
  if compcd = '';
    *in30 = *off;
    write ipnoscp;
    write ipfoot;
    if msgrrn > 0;
      *in40 = *on;
      write ipmsgctl;
    else;
      *in40 = *off;
    endif;
    exfmt ipctl;
    leave;
  endif;

  // PERP-74: a message queued by an action handler below (F6, etc.)
  // must survive one full loop pass before being cleared, or it never
  // reaches the screen -- clearMsgs wipes msgrrn back to 0 on the very
  // next pass, before the "if msgrrn > 0" check further down ever sees
  // it. holdMsg skips exactly one clearMsgs call right after such a
  // message was queued.
  if holdMsg;
    holdMsg = *off;
  else;
    exsr clearMsgs;
  endif;

  if fItem = '' or fVendor = '';
    *in30 = *off;
    numRows = 0;
    write ipnoscp;
  else;
    exsr loadRows;
    if numRows = 0;
      *in30 = *off;
      write ipnone;
    else;
      exsr fillSubfile;
      *in30 = *on;
    endif;
  endif;

  write ipfoot;
  if msgrrn > 0;
    *in40 = *on;
    write ipmsgctl;
  else;
    *in40 = *off;
  endif;
  exfmt ipctl;

  if *in03 or *in12;
    leave;
  endif;

  if *in05;
    fItem   = sfitem;
    fVendor = sfvendor;
    iter;
  endif;

  if *in06;
    if fItem = '' or fVendor = '';
      writeMsg('Enter an item and vendor before adding a price.');
    else;
      exsr addPrice;
    endif;
    if msgrrn > 0;
      holdMsg = *on;
    endif;
    iter;
  endif;

  // Item Number prompt (PERP-59): '?' + Enter invokes the standard
  // reusable Item Number lookup (PERP-56) and returns the selection.
  if %trim(sfitem) = '?';
    promptItem = sfitem;
    callItmprmt(compcd : promptItem);
    sfitem = promptItem;
    fItem = promptItem;
    iter;
  endif;

  // Vendor Number prompt (PERP-75): '?' + Enter invokes the standard
  // reusable Vendor Number lookup (PERP-70) and returns the selection.
  if %trim(sfvendor) = '?';
    promptVendor = sfvendor;
    callVndprmt(compcd : promptVendor);
    sfvendor = promptVendor;
    fVendor = promptVendor;
    iter;
  endif;

  // Refresh scope from screen entry
  if sfitem <> fItem or sfvendor <> fVendor;
    fItem   = sfitem;
    fVendor = sfvendor;
    iter;
  endif;
enddo;

*inlr = *on;
return;

// ---------------------------------------------------------------------
begsr loadRows;
  numRows = 0;
  // PERP-83: display as MM/DD/YY. effective_from/to are stored as
  // native DATE columns but rendered here as plain strings (SEFFFRM/
  // SEFFTO carry no DATFMT keyword), so the format has to be built by
  // hand from the ISO string -- DB2 for i's CHAR(date,fmt) built-in
  // formats (ISO/USA/EUR/JIS) all use a 4-digit year, none produce a
  // 2-digit year directly.
  exec sql declare p1 cursor for
    select substr(char(effective_from, iso), 6, 2) || '/'
             || substr(char(effective_from, iso), 9, 2) || '/'
             || substr(char(effective_from, iso), 3, 2),
           case when effective_to is null then ''
                else substr(char(effective_to, iso), 6, 2) || '/'
                       || substr(char(effective_to, iso), 9, 2) || '/'
                       || substr(char(effective_to, iso), 3, 2)
           end,
           unit_price, currency_code, price_source
      from perpdemo.item_vendor_price
     where company_code = :compcd and item_number = :fItem
       and vendor_code = :fVendor
     order by effective_from desc;
  exec sql open p1;
  if sqlcode < 0;
    writeMsg('SQL open failed: SQLCODE=' + %char(sqlcode));
    leavesr;
  endif;

  dow numRows < %elem(rows);
    exec sql fetch p1 into :prcRow;
    if sqlcode = 100 or sqlcode < 0;
      leave;
    endif;
    numRows += 1;
    rows(numRows) = prcRow;
  enddo;
  exec sql close p1;
endsr;

// ---------------------------------------------------------------------
begsr fillSubfile;
  rrn = 0;
  *in31 = *on;
  write ipctl;
  *in31 = *off;
  for i = 1 to numRows;
    sefffrm = rows(i).efffrm;
    seffto  = rows(i).effto;
    sprice  = rows(i).price;
    scurr   = rows(i).curr;
    ssrc    = rows(i).src;
    rrn += 1;
    write ipsfl;
  endfor;
endsr;

// ---------------------------------------------------------------------
begsr addPrice;
  eitem   = fItem;
  evendor = fVendor;
  enewprc = 0;
  ecurr   = 'USD';
  eprcsrc = 'MANUAL';
  exfmt ipadd;
  if *in12 or enewprc <= 0;
    leavesr;
  endif;

  // Close the current row, if one exists (no current row is fine --
  // this is the very first price for this item-vendor).
  exec sql
    update perpdemo.item_vendor_price
       set effective_to = current_date,
           updated_at   = current_timestamp,
           updated_by   = user
     where company_code = :compcd and item_number = :fItem
       and vendor_code = :fVendor and effective_to is null;
  if sqlcode < 0;
    writeMsg('Close of prior price failed: SQLCODE=' + %char(sqlcode));
    leavesr;
  endif;

  exec sql
    insert into perpdemo.item_vendor_price
      (company_code, item_number, vendor_code, effective_from,
       unit_price, currency_code, price_source)
      values (:compcd, :fItem, :fVendor, current_date,
              :enewprc, :ecurr, :eprcsrc);
  if sqlcode = -803;
    writeMsg('Add failed: a price was already entered today for this'
           + ' item-vendor - only one price change per day.');
  elseif sqlcode < 0;
    writeMsg('Add failed: SQLCODE=' + %char(sqlcode)
           + ' SQLSTATE=' + sqlstate);
  else;
    writeMsg('New price ' + %char(enewprc) + ' ' + %trim(ecurr)
           + ' effective today.');
  endif;
endsr;

// ---------------------------------------------------------------------
begsr clearMsgs;
  msgrrn = 0;
  *in41 = *on;
  write ipmsgctl;
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
  write ipmsgsfl;
end-proc;
