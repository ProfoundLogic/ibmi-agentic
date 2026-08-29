**free

// ---------------------------------------------------------------------
// Program: wrkcnvr  (Work with Item UOM Conversions)
// Purpose: DSPF-based CRUD for item_uom_conversion, scoped to the
//          company selected via perpselr (*LDA positions 1-3) and an
//          item number entered on screen. Callable standalone from the
//          PERP menu (PERP-21) or pre-scoped by passing company/item
//          (PERP-23 item master maintenance calls it this way, option 6
//          on the item subfile).
//          Example: for item WIDGET1, 1 CS = 12 EA.
// Epic:    PERP-3 (PERP-21 / PERP-23)
// ---------------------------------------------------------------------

ctl-opt dftactgrp(*no) actgrp(*new);

dcl-pi *n;
  pCompcd char(3)      const options(*nopass);
  pItem   varchar(25)  const options(*nopass);
end-pi;

dcl-f wrkcnvd workstn sfile(cvsfl:rrn) sfile(cvmsgsfl:msgrrn);

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

dcl-ds cvRow qualified;
  cfrom   varchar(5);
  cto     varchar(5);
  cfact   packed(15:6);
  cactive char(1);
end-ds;

dcl-ds rows likeds(cvRow) dim(500);
dcl-s numRows int(10);
dcl-s i       int(10);
dcl-s rrn     int(10);
dcl-s msgrrn  int(10);
dcl-s msgkey  char(4);
dcl-s selRrn  int(10);
dcl-s selOpt  char(1);
dcl-s filter  varchar(25);
dcl-s compcd  char(3);
dcl-s promptItem varchar(25);
dcl-s holdMsg   ind;
dcl-s validationFailed ind;

in ldaDS;
compcd = ldaDS.compcd;
filter = '';

if %parms >= 1 and pCompcd <> '';
  compcd = pCompcd;
endif;
scompdsp = compcd;
sfitem = '';
if %parms >= 2 and pItem <> '';
  filter = pItem;
  sfitem = pItem;
endif;

if compcd = '';
  exsr clearMsgs;
  writeMsg('No company selected - run Select Company (PERPSELR) first.');
endif;

dow not *in03 and not *in12;
  if compcd = '';
    *in30 = *off;
    write cvnoitem;
    write cvfoot;
    if msgrrn > 0;
      *in40 = *on;
      write cvmsgctl;
    else;
      *in40 = *off;
    endif;
    exfmt cvctl;
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

  if filter = '';
    *in30 = *off;
    numRows = 0;
    write cvnoitem;
  else;
    exsr loadRows;
    if numRows = 0;
      *in30 = *off;
      write cvnone;
    else;
      exsr fillSubfile;
      *in30 = *on;
    endif;
  endif;

  write cvfoot;
  if msgrrn > 0;
    *in40 = *on;
    write cvmsgctl;
  else;
    *in40 = *off;
  endif;
  exfmt cvctl;

  if *in03 or *in12;
    leave;
  endif;

  if *in05;
    filter = sfitem;
    iter;
  endif;

  if *in06;
    if sfitem = '';
      writeMsg('Enter an item number before adding a conversion.');
      holdMsg = *on;
    else;
      filter = sfitem;
      exsr addRow;
    endif;
    iter;
  endif;

  // Item Number prompt (PERP-54): '?' + Enter invokes the standard
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
    readc cvsfl;
    dow not %eof(wrkcnvd);
      if sopt <> '';
        selRrn = rrn;
        selOpt = sopt;
        exsr handleOpt;
        selRrn = 0;
      endif;
      readc cvsfl;
    enddo;
  endif;

enddo;

*inlr = *on;
return;

// ---------------------------------------------------------------------
begsr loadRows;
  numRows = 0;
  exec sql declare c1 cursor for
    select from_uom, to_uom, conversion_factor, is_active
      from perpdemo.item_uom_conversion
     where company_code = :compcd and item_number = :filter
     order by from_uom, to_uom;
  exec sql open c1;
  if sqlcode < 0;
    writeMsg('SQL open failed: SQLCODE=' + %char(sqlcode));
    leavesr;
  endif;

  dow numRows < %elem(rows);
    exec sql fetch c1 into :cvRow;
    if sqlcode = 100 or sqlcode < 0;
      leave;
    endif;
    numRows += 1;
    rows(numRows) = cvRow;
  enddo;
  exec sql close c1;
endsr;

// ---------------------------------------------------------------------
begsr fillSubfile;
  rrn = 0;
  *in31 = *on;
  write cvctl;
  *in31 = *off;
  for i = 1 to numRows;
    *in50 = *off;
    *in51 = *off;
    sopt  = '';
    sfrom = rows(i).cfrom;
    sto   = rows(i).cto;
    sfact = rows(i).cfact;
    rrn += 1;
    write cvsfl;
  endfor;
endsr;

// ---------------------------------------------------------------------
begsr handleOpt;
  chain selRrn cvsfl;
  if %found(wrkcnvd);
    select;
      when selOpt = '2';
        exsr changeRow;
      when selOpt = '4';
        exsr deleteRow;
      other;
        writeMsg('Option ' + selOpt + ' not valid.');
        holdMsg = *on;
    endsl;
  endif;
endsr;

// ---------------------------------------------------------------------
begsr addRow;
  exsr clearMsgs;
  emode   = 'A';
  efrom   = '';
  eto     = '';
  efact   = 0;
  eactive = 'Y';
  exsr editLoop;
  if not *in12;
    exsr validateConv;
    if validationFailed;
      holdMsg = *on;
    else;
      exec sql
        insert into perpdemo.item_uom_conversion
          (company_code, item_number, from_uom, to_uom, conversion_factor, is_active)
          values (:compcd, :filter, :efrom, :eto, :efact, :eactive);
      if sqlcode < 0;
        writeMsg('Add failed: SQLCODE=' + %char(sqlcode)
               + ' SQLSTATE=' + sqlstate);
      else;
        writeMsg('Added ' + %trim(efrom) + ' -> ' + %trim(eto) + '.');
      endif;
      holdMsg = *on;
    endif;
  endif;
endsr;

// ---------------------------------------------------------------------
begsr changeRow;
  exsr clearMsgs;
  emode = 'C';
  efrom = sfrom;
  eto   = sto;
  exec sql
    select conversion_factor, is_active
      into :efact, :eactive
      from perpdemo.item_uom_conversion
     where company_code = :compcd and item_number = :filter
       and from_uom = :efrom and to_uom = :eto;
  if sqlcode <> 0;
    writeMsg('Row disappeared before change.');
    holdMsg = *on;
    leavesr;
  endif;
  exsr editLoop;
  if not *in12;
    exsr validateConv;
    if validationFailed;
      holdMsg = *on;
    else;
      exec sql
        update perpdemo.item_uom_conversion
           set conversion_factor = :efact,
               is_active         = :eactive,
               updated_at        = current_timestamp,
               updated_by        = user
         where company_code = :compcd and item_number = :filter
           and from_uom = :efrom and to_uom = :eto;
      if sqlcode < 0;
        writeMsg('Change failed: SQLCODE=' + %char(sqlcode));
      else;
        writeMsg('Updated ' + %trim(efrom) + ' -> ' + %trim(eto) + '.');
      endif;
      holdMsg = *on;
    endif;
  endif;
endsr;

// ---------------------------------------------------------------------
begsr deleteRow;
  exsr clearMsgs;
  exec sql
    delete from perpdemo.item_uom_conversion
     where company_code = :compcd and item_number = :filter
       and from_uom = :sfrom and to_uom = :sto;
  if sqlcode < 0;
    writeMsg('Delete failed: SQLSTATE=' + sqlstate);
  else;
    writeMsg('Deleted ' + %trim(sfrom) + ' -> ' + %trim(sto) + '.');
  endif;
  holdMsg = *on;
endsr;

// ---------------------------------------------------------------------
begsr editLoop;
  exfmt cvedit;
endsr;

// ---------------------------------------------------------------------
// Required-field validation for the Add/Change panel, done here in RPG
// instead of letting a blank From/To UOM or zero Factor surface as a raw
// FK/CHECK-constraint violation from the database.
begsr validateConv;
  validationFailed = *off;
  if %trim(efrom) = '';
    writeMsg('From UOM is required.');
    validationFailed = *on;
  elseif %trim(eto) = '';
    writeMsg('To UOM is required.');
    validationFailed = *on;
  elseif efact <= 0;
    writeMsg('Factor is required.');
    validationFailed = *on;
  endif;
endsr;

// ---------------------------------------------------------------------
begsr clearMsgs;
  msgrrn = 0;
  *in41 = *on;
  write cvmsgctl;
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
  write cvmsgsfl;
end-proc;
