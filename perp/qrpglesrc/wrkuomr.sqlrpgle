**free

// ---------------------------------------------------------------------
// Program: wrkuomr  (Work with Units of Measure)
// Purpose: DSPF-based CRUD for the uom table. Global (not company-
//          scoped) master data. Options 2/4/5 change/delete/display;
//          F6 adds. Delete falls through to a DB2 FK violation
//          (SQL0532/SQL0531) if the UOM is referenced by item or
//          item_uom_conversion -- RPG surfaces the SQLSTATE cleanly
//          rather than pre-checking.
// Epic:    PERP-3 (PERP-21)
// ---------------------------------------------------------------------

ctl-opt dftactgrp(*no) actgrp(*new);

dcl-f wrkuomd workstn sfile(uosfl:rrn) sfile(uomsgsfl:msgrrn);

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

dcl-ds uomRow qualified;
  code    varchar(5);
  desc    varchar(60);
  cat     varchar(20);
  active  char(1);
end-ds;

dcl-ds rows likeds(uomRow) dim(500);
dcl-s numRows int(10);
dcl-s i       int(10);
dcl-s rrn     int(10);
dcl-s msgrrn  int(10);
dcl-s msgkey  char(4);
dcl-s selRrn  int(10);
dcl-s selOpt  char(1);

dow not *in03 and not *in12;
  exsr clearMsgs;
  exsr loadRows;

  if numRows = 0;
    *in30 = *off;
    write uonone;
  else;
    exsr fillSubfile;
    *in30 = *on;
  endif;

  write uofoot;
  if msgrrn > 0;
    *in40 = *on;
    write uomsgctl;
  else;
    *in40 = *off;
  endif;
  exfmt uoctl;

  if *in03 or *in12;
    leave;
  endif;

  if *in05;
    iter;
  endif;

  if *in06;
    exsr addRow;
    iter;
  endif;

  // Process subfile options. Guard on numRows: READC against a subfile
  // that was never written to this cycle (0 rows loaded) raises a
  // "Session or device error" (CPF5006-class) runtime error instead of
  // just returning *EOF.
  if numRows > 0;
    selRrn = 0;
    selOpt = ' ';
    readc uosfl;
    dow not %eof(wrkuomd);
      if sopt <> '';
        selRrn = rrn;
        selOpt = sopt;
        exsr handleOpt;
        selRrn = 0;
      endif;
      readc uosfl;
    enddo;
  endif;

enddo;

*inlr = *on;
return;

// ---------------------------------------------------------------------
begsr loadRows;
  numRows = 0;
  exec sql declare c1 cursor for
    select uom_code, description, uom_category, is_active
      from perpdemo.uom
     order by uom_code;
  exec sql open c1;
  if sqlcode < 0;
    writeMsg('SQL open failed: SQLCODE=' + %char(sqlcode));
    return;
  endif;

  dow numRows < %elem(rows);
    exec sql fetch c1 into :uomRow;
    if sqlcode = 100 or sqlcode < 0;
      leave;
    endif;
    numRows += 1;
    rows(numRows) = uomRow;
  enddo;
  exec sql close c1;
endsr;

// ---------------------------------------------------------------------
begsr fillSubfile;
  rrn = 0;
  *in31 = *on;
  write uoctl;
  *in31 = *off;
  for i = 1 to numRows;
    *in50 = *off;
    *in51 = *off;
    sopt  = '';
    scode = rows(i).code;
    sdesc = rows(i).desc;
    scat  = rows(i).cat;
    rrn += 1;
    write uosfl;
  endfor;
endsr;

// ---------------------------------------------------------------------
begsr handleOpt;
  chain selRrn uosfl;
  if %found(wrkuomd);
    select;
      when selOpt = '2';
        exsr changeRow;
      when selOpt = '4';
        exsr deleteRow;
      when selOpt = '5';
        exsr displayRow;
      other;
        writeMsg('Option ' + selOpt + ' not valid.');
    endsl;
  endif;
endsr;

// ---------------------------------------------------------------------
begsr addRow;
  emode   = 'A';
  ecode   = '';
  edesc   = '';
  ecat    = '';
  eactive = 'Y';
  exsr editLoop;
  if not *in12 and ecode <> '';
    exec sql
      insert into perpdemo.uom (uom_code, description, uom_category, is_active)
        values (:ecode, :edesc, :ecat, :eactive);
    if sqlcode < 0;
      writeMsg('Add failed: SQLCODE=' + %char(sqlcode)
             + ' SQLSTATE=' + sqlstate);
    else;
      writeMsg('Added ' + %trim(ecode) + '.');
    endif;
  endif;
endsr;

// ---------------------------------------------------------------------
begsr changeRow;
  emode = 'C';
  ecode = scode;
  exec sql
    select description, uom_category, is_active
      into :edesc, :ecat, :eactive
      from perpdemo.uom
     where uom_code = :ecode;
  if sqlcode <> 0;
    writeMsg('Row disappeared before change.');
    return;
  endif;
  exsr editLoop;
  if not *in12;
    exec sql
      update perpdemo.uom
         set description  = :edesc,
             uom_category = :ecat,
             is_active    = :eactive,
             updated_at   = current_timestamp,
             updated_by   = user
       where uom_code = :ecode;
    if sqlcode < 0;
      writeMsg('Change failed: SQLCODE=' + %char(sqlcode));
    else;
      writeMsg('Updated ' + %trim(ecode) + '.');
    endif;
  endif;
endsr;

// ---------------------------------------------------------------------
begsr deleteRow;
  exec sql
    delete from perpdemo.uom
     where uom_code = :scode;
  if sqlcode < 0;
    writeMsg('Delete failed (in use?): SQLSTATE=' + sqlstate);
  else;
    writeMsg('Deleted ' + %trim(scode) + '.');
  endif;
endsr;

// ---------------------------------------------------------------------
begsr displayRow;
  emode = 'D';
  ecode = scode;
  exec sql
    select description, uom_category, is_active
      into :edesc, :ecat, :eactive
      from perpdemo.uom
     where uom_code = :ecode;
  exsr editLoop;
endsr;

// ---------------------------------------------------------------------
begsr editLoop;
  exfmt uoedit;
endsr;

// ---------------------------------------------------------------------
begsr clearMsgs;
  msgrrn = 0;
  *in41 = *on;
  write uomsgctl;
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
  write uomsgsfl;
end-proc;
