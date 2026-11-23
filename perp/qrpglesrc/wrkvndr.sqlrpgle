**free

// ---------------------------------------------------------------------
// Program: wrkvndr  (Work with Vendors -- vendor master maintenance)
// Purpose: DSPF-based CRUD for vendor, scoped by the company selected
//          via perpselr (*LDA positions 1-3). Subfile filters by
//          active-only and buyer_code. buyer_code FK is enforced by
//          DB2 against perp_user; payment_terms_code FK against
//          code_master (PAYTERMS) -- typos surface as SQLSTATE 23503
//          on the message subfile.
// Epic:    PERP-5 (PERP-29)
// ---------------------------------------------------------------------

ctl-opt dftactgrp(*no) actgrp(*new);

dcl-f wrkvndd workstn sfile(vsfl:rrn) sfile(vmsgsfl:msgrrn);

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

dcl-ds vndRow qualified;
  vcode  varchar(10);
  vname  varchar(60);
  vbuyer char(10);
  vact   char(1);
end-ds;

dcl-ds rows likeds(vndRow) dim(500);
dcl-s numRows  int(10);
dcl-s i        int(10);
dcl-s rrn      int(10);
dcl-s msgrrn   int(10);
dcl-s msgkey   char(4);
dcl-s selRrn   int(10);
dcl-s selOpt   char(1);
dcl-s compcd   char(3);
dcl-s fActOnly char(1);
dcl-s fBuyer   char(10);
dcl-s holdMsg  ind;
dcl-s validationFailed ind;

in ldaDS;
compcd = ldaDS.compcd;
scompdsp = compcd;
fActOnly = 'N';
fBuyer   = '';
sfact    = 'N';
sfbuyer  = '';

if compcd = '';
  exsr clearMsgs;
  writeMsg('No company selected - run Select Company (PERPSELR) first.');
endif;

dow not *in03 and not *in12;
  if compcd = '';
    *in30 = *off;
    write vnoco;
    write vfoot;
    if msgrrn > 0;
      *in40 = *on;
      write vmsgctl;
    else;
      *in40 = *off;
    endif;
    exfmt vctl;
    leave;
  endif;

  // A message queued by an action handler below (addRow, etc.) must
  // survive one full loop pass before being cleared, or it never
  // reaches the screen -- clearMsgs wipes msgrrn back to 0 on the very
  // next pass, before this pass's own exfmt ever shows it. holdMsg
  // skips exactly one clearMsgs call right after such a message was
  // queued. Same pattern as wrkitmr.
  if holdMsg;
    holdMsg = *off;
  else;
    exsr clearMsgs;
  endif;
  exsr loadRows;

  if numRows = 0;
    *in30 = *off;
    write vnone;
  else;
    exsr fillSubfile;
    *in30 = *on;
  endif;

  write vfoot;
  if msgrrn > 0;
    *in40 = *on;
    write vmsgctl;
  else;
    *in40 = *off;
  endif;
  exfmt vctl;

  if *in03 or *in12;
    leave;
  endif;

  if *in05;
    fActOnly = sfact;
    fBuyer   = sfbuyer;
    iter;
  endif;

  if *in06;
    exsr addRow;
    iter;
  endif;

  // Refresh filters from screen entry
  if sfact <> fActOnly or sfbuyer <> fBuyer;
    fActOnly = sfact;
    fBuyer   = sfbuyer;
    iter;
  endif;

  // Process subfile options. Guard on numRows: READC against a subfile
  // that was never written to this cycle (0 rows loaded) raises a
  // "Session or device error" (CPF5006-class) runtime error instead of
  // just returning *EOF.
  if numRows > 0;
    selRrn = 0;
    selOpt = ' ';
    readc vsfl;
    dow not %eof(wrkvndd);
      if sopt <> '';
        selRrn = rrn;
        selOpt = sopt;
        exsr handleOpt;
        selRrn = 0;
      endif;
      readc vsfl;
    enddo;
  endif;

enddo;

*inlr = *on;
return;

// ---------------------------------------------------------------------
begsr loadRows;
  numRows = 0;
  exec sql declare v1 cursor for
    select vendor_code, vendor_name, buyer_code, is_active
      from perpdemo.vendor
     where company_code = :compcd
       and (:fActOnly = 'N' or is_active = 'Y')
       and (:fBuyer   = '' or buyer_code = :fBuyer)
     order by vendor_code;
  exec sql open v1;
  if sqlcode < 0;
    writeMsg('SQL open failed: SQLCODE=' + %char(sqlcode));
    holdMsg = *on;
    leavesr;
  endif;

  dow numRows < %elem(rows);
    exec sql fetch v1 into :vndRow;
    if sqlcode = 100 or sqlcode < 0;
      leave;
    endif;
    numRows += 1;
    rows(numRows) = vndRow;
  enddo;
  exec sql close v1;
endsr;

// ---------------------------------------------------------------------
begsr fillSubfile;
  rrn = 0;
  *in31 = *on;
  write vctl;
  *in31 = *off;
  for i = 1 to numRows;
    *in50 = *off;
    *in51 = *off;
    sopt    = '';
    svcode  = rows(i).vcode;
    svname  = rows(i).vname;
    svbuyer = rows(i).vbuyer;
    svact   = rows(i).vact;
    rrn += 1;
    write vsfl;
  endfor;
endsr;

// ---------------------------------------------------------------------
begsr handleOpt;
  chain selRrn vsfl;
  if %found(wrkvndd);
    select;
      when selOpt = '2';
        exsr changeRow;
      when selOpt = '4';
        exsr deleteRow;
      when selOpt = '5';
        exsr displayRow;
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
  evcode  = '';
  evname  = '';
  eaddr1  = '';
  eaddr2  = '';
  ecity   = '';
  estate  = '';
  epostcd = '';
  ecntry  = 'US';
  ephone  = '';
  eemail  = '';
  ecntct  = '';
  ebuyer  = fBuyer;
  epterms = 'NET30';
  etaxid  = '';
  eactive = 'Y';
  exsr editLoop;
  if not *in12;
    exsr validateVendor;
    if validationFailed;
      holdMsg = *on;
    else;
      exec sql
        insert into perpdemo.vendor
          (company_code, vendor_code, vendor_name, address_line1, address_line2,
           city_name, state_code, postal_code, country_code, phone_number,
           email_address, contact_name, buyer_code, payment_terms_code, tax_id,
           is_active)
          values (:compcd, :evcode, :evname, :eaddr1, :eaddr2,
                  :ecity, :estate, :epostcd, :ecntry, :ephone,
                  :eemail, :ecntct, :ebuyer, :epterms, :etaxid,
                  :eactive);
      if sqlcode < 0;
        writeMsg('Add failed: SQLCODE=' + %char(sqlcode)
               + ' SQLSTATE=' + sqlstate);
      else;
        writeMsg('Added ' + %trim(evcode) + '.');
      endif;
      holdMsg = *on;
    endif;
  endif;
endsr;

// ---------------------------------------------------------------------
begsr changeRow;
  exsr clearMsgs;
  emode  = 'C';
  evcode = svcode;
  exec sql
    select vendor_name, address_line1, address_line2, city_name, state_code,
           postal_code, country_code, phone_number, email_address,
           contact_name, buyer_code, payment_terms_code, tax_id, is_active
      into :evname, :eaddr1, :eaddr2, :ecity, :estate,
           :epostcd, :ecntry, :ephone, :eemail,
           :ecntct, :ebuyer, :epterms, :etaxid, :eactive
      from perpdemo.vendor
     where company_code = :compcd and vendor_code = :evcode;
  if sqlcode <> 0;
    writeMsg('Row disappeared before change.');
    holdMsg = *on;
    leavesr;
  endif;
  exsr editLoop;
  if not *in12;
    exsr validateVendor;
    if validationFailed;
      holdMsg = *on;
    else;
      exec sql
        update perpdemo.vendor
           set vendor_name         = :evname,
               address_line1       = :eaddr1,
               address_line2       = :eaddr2,
               city_name           = :ecity,
               state_code          = :estate,
               postal_code         = :epostcd,
               country_code        = :ecntry,
               phone_number        = :ephone,
               email_address       = :eemail,
               contact_name        = :ecntct,
               buyer_code          = :ebuyer,
               payment_terms_code  = :epterms,
               tax_id              = :etaxid,
               is_active           = :eactive,
               updated_at          = current_timestamp,
               updated_by          = user
         where company_code = :compcd and vendor_code = :evcode;
      if sqlcode < 0;
        writeMsg('Change failed: SQLCODE=' + %char(sqlcode)
               + ' SQLSTATE=' + sqlstate);
      else;
        writeMsg('Updated ' + %trim(evcode) + '.');
      endif;
      holdMsg = *on;
    endif;
  endif;
endsr;

// ---------------------------------------------------------------------
begsr deleteRow;
  exsr clearMsgs;
  exec sql
    delete from perpdemo.vendor
     where company_code = :compcd and vendor_code = :svcode;
  if sqlcode < 0;
    writeMsg('Delete failed (in use?): SQLSTATE=' + sqlstate);
  else;
    writeMsg('Deleted ' + %trim(svcode) + '.');
  endif;
  holdMsg = *on;
endsr;

// ---------------------------------------------------------------------
begsr displayRow;
  emode  = 'D';
  evcode = svcode;
  exec sql
    select vendor_name, address_line1, address_line2, city_name, state_code,
           postal_code, country_code, phone_number, email_address,
           contact_name, buyer_code, payment_terms_code, tax_id, is_active
      into :evname, :eaddr1, :eaddr2, :ecity, :estate,
           :epostcd, :ecntry, :ephone, :eemail,
           :ecntct, :ebuyer, :epterms, :etaxid, :eactive
      from perpdemo.vendor
     where company_code = :compcd and vendor_code = :evcode;
  exsr editLoop;
endsr;

// ---------------------------------------------------------------------
// *in60 conditions DSPATR(PR) on every entry field in VEDIT -- protect
// them in Display mode so 5=Display can't be mistaken for an editable
// screen (it never saves regardless, but the fields must not look
// enterable).
begsr editLoop;
  if emode = 'D';
    *in60 = *on;
  else;
    *in60 = *off;
  endif;
  exfmt vedit;
endsr;

// ---------------------------------------------------------------------
// Required-field validation for the Add/Change panel, done here in RPG
// instead of letting a blank required field surface as a raw FK/NOT-NULL
// violation from the database.
begsr validateVendor;
  validationFailed = *off;
  if %trim(evcode) = '';
    writeMsg('Vendor Code is required.');
    validationFailed = *on;
  elseif %trim(evname) = '';
    writeMsg('Vendor Name is required.');
    validationFailed = *on;
  elseif %trim(ebuyer) = '';
    writeMsg('Buyer is required.');
    validationFailed = *on;
  endif;
endsr;

// ---------------------------------------------------------------------
begsr clearMsgs;
  msgrrn = 0;
  *in41 = *on;
  write vmsgctl;
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
  write vmsgsfl;
end-proc;
