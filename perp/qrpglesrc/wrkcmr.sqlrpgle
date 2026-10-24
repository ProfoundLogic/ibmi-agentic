**free

// ---------------------------------------------------------------------
// Program: wrkcmr  (Work with Code Master)
// Purpose: DSPF-based CRUD for the code_master table. One screen shows
//          all system lookups regardless of code_type; a filter field
//          narrows the subfile to a specific type. Options 2/4/5
//          change/delete/display; F6 adds. Delete falls through to a
//          DB2 FK violation (SQL0532/SQL0531) if the code is in use —
//          RPG surfaces the SQLSTATE cleanly rather than pre-checking.
// Epic:    PERP-2 (PERP-17)
// ---------------------------------------------------------------------

ctl-opt dftactgrp(*no) actgrp(*new);

dcl-f wrkcmd workstn sfile(cmsfl:rrn) sfile(cmmsgsfl:msgrrn);

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

dcl-ds cmRow qualified;
  ctype       varchar(20);
  cvalue      varchar(20);
  cdesc       varchar(60);
  cshort      varchar(20);
  csort       int(10);
  cactive     char(1);
end-ds;

dcl-ds rows likeds(cmRow) dim(500);
dcl-s numRows int(10);
dcl-s i       int(10);
dcl-s rrn     int(10);
dcl-s msgrrn  int(10);
dcl-s msgkey  char(4);
dcl-s selRrn  int(10);
dcl-s selOpt  char(1);
dcl-s filter  varchar(20);
dcl-s holdMsg ind;
dcl-s validationFailed ind;

filter = '';
sftype = '';

dow not *in03 and not *in12;
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
    write cmnone;
  else;
    exsr fillSubfile;
    *in30 = *on;
  endif;

  write cmfoot;
  if msgrrn > 0;
    *in40 = *on;
    write cmmsgctl;
  else;
    *in40 = *off;
  endif;
  exfmt cmctl;

  if *in03 or *in12;
    leave;
  endif;

  if *in05;
    filter = sftype;
    iter;
  endif;

  if *in06;
    exsr addRow;
    iter;
  endif;

  // Refresh filter from screen entry
  if sftype <> filter;
    filter = sftype;
    iter;
  endif;

  // Process subfile options. Guard on numRows: READC against a subfile
  // that was never written to this cycle (0 rows loaded) raises a
  // "Session or device error" (CPF5006-class) runtime error instead of
  // just returning *EOF.
  if numRows > 0;
    selRrn = 0;
    selOpt = ' ';
    readc cmsfl;
    dow not %eof(wrkcmd);
      if sopt <> '';
        selRrn = rrn;
        selOpt = sopt;
        exsr handleOpt;
        selRrn = 0;
      endif;
      readc cmsfl;
    enddo;
  endif;

enddo;

*inlr = *on;
return;

// ---------------------------------------------------------------------
begsr loadRows;
  numRows = 0;
  if filter = '';
    exec sql declare c1 cursor for
      select code_type, code_value, description, short_desc,
             sort_order, is_active
        from perpdemo.code_master
       order by code_type, sort_order, code_value;
  else;
    exec sql declare c2 cursor for
      select code_type, code_value, description, short_desc,
             sort_order, is_active
        from perpdemo.code_master
       where code_type = :filter
       order by sort_order, code_value;
  endif;

  if filter = '';
    exec sql open c1;
  else;
    exec sql open c2;
  endif;
  if sqlcode < 0;
    writeMsg('SQL open failed: SQLCODE=' + %char(sqlcode));
    holdMsg = *on;
    leavesr;
  endif;

  dow numRows < %elem(rows);
    if filter = '';
      exec sql fetch c1 into :cmRow;
    else;
      exec sql fetch c2 into :cmRow;
    endif;
    if sqlcode = 100 or sqlcode < 0;
      leave;
    endif;
    numRows += 1;
    rows(numRows) = cmRow;
  enddo;

  if filter = '';
    exec sql close c1;
  else;
    exec sql close c2;
  endif;
endsr;

// ---------------------------------------------------------------------
begsr fillSubfile;
  rrn = 0;
  *in31 = *on;
  write cmctl;
  *in31 = *off;
  for i = 1 to numRows;
    *in50 = *off;
    *in51 = *off;
    sopt   = '';
    stype  = rows(i).ctype;
    svalue = rows(i).cvalue;
    sdesc  = rows(i).cdesc;
    rrn += 1;
    write cmsfl;
  endfor;
endsr;

// ---------------------------------------------------------------------
begsr handleOpt;
  chain selRrn cmsfl;
  if %found(wrkcmd);
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
  etype   = filter;
  evalue  = '';
  edesc   = '';
  eshort  = '';
  esort   = 0;
  eactive = 'Y';
  exsr editLoop;
  if not *in12;
    exsr validateCode;
    if validationFailed;
      holdMsg = *on;
    else;
      exec sql
        insert into perpdemo.code_master
          (code_type, code_value, description, short_desc,
           sort_order, is_active)
          values (:etype, :evalue, :edesc, :eshort, :esort, :eactive);
      if sqlcode < 0;
        writeMsg('Add failed: SQLCODE=' + %char(sqlcode)
               + ' SQLSTATE=' + sqlstate);
      else;
        writeMsg('Added ' + %trim(etype) + '/' + %trim(evalue) + '.');
      endif;
      holdMsg = *on;
    endif;
  endif;
endsr;

// ---------------------------------------------------------------------
begsr changeRow;
  exsr clearMsgs;
  emode   = 'C';
  etype   = stype;
  evalue  = svalue;
  exec sql
    select description, short_desc, sort_order, is_active
      into :edesc, :eshort, :esort, :eactive
      from perpdemo.code_master
     where code_type = :etype and code_value = :evalue;
  if sqlcode <> 0;
    writeMsg('Row disappeared before change.');
    holdMsg = *on;
    leavesr;
  endif;
  exsr editLoop;
  if not *in12;
    exsr validateCode;
    if validationFailed;
      holdMsg = *on;
    else;
      exec sql
        update perpdemo.code_master
           set description = :edesc,
               short_desc  = :eshort,
               sort_order  = :esort,
               is_active   = :eactive,
               updated_at  = current_timestamp,
               updated_by  = user
         where code_type = :etype and code_value = :evalue;
      if sqlcode < 0;
        writeMsg('Change failed: SQLCODE=' + %char(sqlcode));
      else;
        writeMsg('Updated ' + %trim(etype) + '/' + %trim(evalue) + '.');
      endif;
      holdMsg = *on;
    endif;
  endif;
endsr;

// ---------------------------------------------------------------------
begsr deleteRow;
  exsr clearMsgs;
  exec sql
    delete from perpdemo.code_master
     where code_type = :stype and code_value = :svalue;
  if sqlcode < 0;
    writeMsg('Delete failed (in use?): SQLSTATE=' + sqlstate);
  else;
    writeMsg('Deleted ' + %trim(stype) + '/' + %trim(svalue) + '.');
  endif;
  holdMsg = *on;
endsr;

// ---------------------------------------------------------------------
begsr displayRow;
  emode   = 'D';
  etype   = stype;
  evalue  = svalue;
  exec sql
    select description, short_desc, sort_order, is_active
      into :edesc, :eshort, :esort, :eactive
      from perpdemo.code_master
     where code_type = :etype and code_value = :evalue;
  exsr editLoop;
endsr;

// ---------------------------------------------------------------------
begsr editLoop;
  exfmt cmedit;
endsr;

// ---------------------------------------------------------------------
// Required-field validation for the Add/Change panel, done here in RPG
// instead of letting a blank required field surface as a raw NOT-NULL
// violation from the database.
begsr validateCode;
  validationFailed = *off;
  if %trim(etype) = '';
    writeMsg('Code Type is required.');
    validationFailed = *on;
  elseif %trim(evalue) = '';
    writeMsg('Code Value is required.');
    validationFailed = *on;
  elseif %trim(edesc) = '';
    writeMsg('Description is required.');
    validationFailed = *on;
  endif;
endsr;

// ---------------------------------------------------------------------
begsr clearMsgs;
  msgrrn = 0;
  *in41 = *on;
  write cmmsgctl;
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
  write cmmsgsfl;
end-proc;
