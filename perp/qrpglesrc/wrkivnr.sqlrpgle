**free

// ---------------------------------------------------------------------
// Program: wrkivnr  (Work with Item-Vendor Profiles)
// Purpose: DSPF-based CRUD for item_vendor, scoped by the company
//          selected via perpselr (*LDA positions 1-3) plus EITHER an
//          item number OR a vendor code entered on screen (item wins
//          if both are entered). Setting is_preferred = 'Y' lets DB2
//          reject the change via the item_vendor_preferred_ak partial
//          unique index (SQLSTATE 23505) rather than pre-clearing the
//          previous preferred row -- per the epic's "or lets DB reject
//          the change with a clear error" option.
// Epic:    PERP-5 (PERP-30)
// ---------------------------------------------------------------------

ctl-opt dftactgrp(*no) actgrp(*new);

dcl-f wrkivnd workstn sfile(ivsfl:rrn) sfile(ivmsgsfl:msgrrn);

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

dcl-ds ivRow qualified;
  iitem   varchar(25);
  ivendor varchar(10);
  ipartn  varchar(25);
  ilead   int(10);
  imoq    packed(15:4);
  ipack   packed(15:4);
  ipref   char(1);
end-ds;

dcl-ds rows likeds(ivRow) dim(500);
dcl-s numRows  int(10);
dcl-s i        int(10);
dcl-s rrn      int(10);
dcl-s msgrrn   int(10);
dcl-s msgkey   char(4);
dcl-s selRrn   int(10);
dcl-s selOpt   char(1);
dcl-s compcd   char(3);
dcl-s fItem    varchar(25);
dcl-s fVendor  varchar(10);

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
    write ivnoscp;
    write ivfoot;
    if msgrrn > 0;
      *in40 = *on;
      write ivmsgctl;
    else;
      *in40 = *off;
    endif;
    exfmt ivctl;
    leave;
  endif;

  exsr clearMsgs;

  if fItem = '' and fVendor = '';
    *in30 = *off;
    numRows = 0;
    write ivnoscp;
  else;
    exsr loadRows;
    if numRows = 0;
      *in30 = *off;
      write ivnone;
    else;
      exsr fillSubfile;
      *in30 = *on;
    endif;
  endif;

  write ivfoot;
  if msgrrn > 0;
    *in40 = *on;
    write ivmsgctl;
  else;
    *in40 = *off;
  endif;
  exfmt ivctl;

  if *in03 or *in12;
    leave;
  endif;

  if *in05;
    fItem   = sfitem;
    fVendor = sfvendor;
    iter;
  endif;

  if *in06;
    exsr addRow;
    iter;
  endif;

  // Refresh scope from screen entry
  if sfitem <> fItem or sfvendor <> fVendor;
    fItem   = sfitem;
    fVendor = sfvendor;
    iter;
  endif;

  // Process subfile options. Guard on numRows: READC against a subfile
  // that was never written to this cycle (0 rows loaded) raises a
  // "Session or device error" (CPF5006-class) runtime error instead of
  // just returning *EOF.
  if numRows > 0;
    selRrn = 0;
    selOpt = ' ';
    readc ivsfl;
    dow not %eof(wrkivnd);
      if sopt <> '';
        selRrn = rrn;
        selOpt = sopt;
        exsr handleOpt;
        selRrn = 0;
      endif;
      readc ivsfl;
    enddo;
  endif;

enddo;

*inlr = *on;
return;

// ---------------------------------------------------------------------
begsr loadRows;
  numRows = 0;
  exec sql declare iv1 cursor for
    select item_number, vendor_code, vendor_part_number, lead_time_days,
           moq, pack_size, is_preferred
      from perpdemo.item_vendor
     where company_code = :compcd
       and (:fItem   = '' or item_number = :fItem)
       and (:fVendor = '' or vendor_code = :fVendor)
     order by item_number, vendor_code;
  exec sql open iv1;
  if sqlcode < 0;
    writeMsg('SQL open failed: SQLCODE=' + %char(sqlcode));
    return;
  endif;

  dow numRows < %elem(rows);
    exec sql fetch iv1 into :ivRow;
    if sqlcode = 100 or sqlcode < 0;
      leave;
    endif;
    numRows += 1;
    rows(numRows) = ivRow;
  enddo;
  exec sql close iv1;
endsr;

// ---------------------------------------------------------------------
begsr fillSubfile;
  rrn = 0;
  *in31 = *on;
  write ivctl;
  *in31 = *off;
  for i = 1 to numRows;
    *in50 = *off;
    *in51 = *off;
    sopt     = '';
    siitem   = rows(i).iitem;
    sivendor = rows(i).ivendor;
    sipartn  = rows(i).ipartn;
    silead   = rows(i).ilead;
    simoq    = rows(i).imoq;
    sipack   = rows(i).ipack;
    sipref   = rows(i).ipref;
    rrn += 1;
    write ivsfl;
  endfor;
endsr;

// ---------------------------------------------------------------------
begsr handleOpt;
  chain selRrn ivsfl;
  if %found(wrkivnd);
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
  if fItem = '' and fVendor = '';
    writeMsg('Enter an item or vendor before adding a profile.');
    return;
  endif;
  emode   = 'A';
  eitem   = fItem;
  evendor = fVendor;
  epartn  = '';
  eleadtm = 0;
  emoq    = 0;
  epacksz = 1;
  epref   = 'N';
  eactive = 'Y';
  exsr editLoop;
  if not *in12 and eitem <> '' and evendor <> '';
    exec sql
      insert into perpdemo.item_vendor
        (company_code, item_number, vendor_code, vendor_part_number,
         lead_time_days, moq, pack_size, is_preferred, is_active)
        values (:compcd, :eitem, :evendor, :epartn,
                :eleadtm, :emoq, :epacksz, :epref, :eactive);
    if sqlcode = -803;
      writeMsg('Add failed: another vendor is already preferred for'
             + ' this item - clear it first.');
    elseif sqlcode < 0;
      writeMsg('Add failed: SQLCODE=' + %char(sqlcode)
             + ' SQLSTATE=' + sqlstate);
    else;
      writeMsg('Added ' + %trim(eitem) + '/' + %trim(evendor) + '.');
    endif;
  endif;
endsr;

// ---------------------------------------------------------------------
begsr changeRow;
  emode   = 'C';
  eitem   = siitem;
  evendor = sivendor;
  exec sql
    select vendor_part_number, lead_time_days, moq, pack_size,
           is_preferred, is_active
      into :epartn, :eleadtm, :emoq, :epacksz,
           :epref, :eactive
      from perpdemo.item_vendor
     where company_code = :compcd and item_number = :eitem
       and vendor_code = :evendor;
  if sqlcode <> 0;
    writeMsg('Row disappeared before change.');
    return;
  endif;
  exsr editLoop;
  if not *in12;
    exec sql
      update perpdemo.item_vendor
         set vendor_part_number = :epartn,
             lead_time_days     = :eleadtm,
             moq                = :emoq,
             pack_size          = :epacksz,
             is_preferred       = :epref,
             is_active          = :eactive,
             updated_at         = current_timestamp,
             updated_by         = user
       where company_code = :compcd and item_number = :eitem
         and vendor_code = :evendor;
    if sqlcode = -803;
      writeMsg('Change failed: another vendor is already preferred for'
             + ' this item - clear it first.');
    elseif sqlcode < 0;
      writeMsg('Change failed: SQLCODE=' + %char(sqlcode)
             + ' SQLSTATE=' + sqlstate);
    else;
      writeMsg('Updated ' + %trim(eitem) + '/' + %trim(evendor) + '.');
    endif;
  endif;
endsr;

// ---------------------------------------------------------------------
begsr deleteRow;
  exec sql
    delete from perpdemo.item_vendor
     where company_code = :compcd and item_number = :siitem
       and vendor_code = :sivendor;
  if sqlcode < 0;
    writeMsg('Delete failed (in use?): SQLSTATE=' + sqlstate);
  else;
    writeMsg('Deleted ' + %trim(siitem) + '/' + %trim(sivendor) + '.');
  endif;
endsr;

// ---------------------------------------------------------------------
begsr editLoop;
  exfmt ivedit;
endsr;

// ---------------------------------------------------------------------
begsr clearMsgs;
  msgrrn = 0;
  *in41 = *on;
  write ivmsgctl;
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
  write ivmsgsfl;
end-proc;
