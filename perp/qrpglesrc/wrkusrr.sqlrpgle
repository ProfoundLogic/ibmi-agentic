**free

// ---------------------------------------------------------------------
// Program: wrkusrr  (Work with PERP Users)
// Purpose: DSPF-based CRUD for the perp_user table. Same pattern as
//          wrkcmr — subfile list + edit format. Role FK is enforced
//          by DB2; if the caller types a role_code that is not in
//          code_master (USERROLE), INSERT/UPDATE returns SQLSTATE
//          23503 which the RPG surfaces to the message subfile.
// Epic:    PERP-2 (PERP-18)
// ---------------------------------------------------------------------

ctl-opt dftactgrp(*no) actgrp(*new);

dcl-f wrkusrd workstn sfile(usfl:rrn) sfile(umsgsfl:msgrrn);

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

dcl-ds uRow qualified;
  ucode   char(10);
  uname   varchar(60);
  uemail  varchar(120);
  urole   varchar(20);
  uact    char(1);
end-ds;

dcl-ds rows likeds(uRow) dim(500);
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
    write unone;
  else;
    exsr fillSubfile;
    *in30 = *on;
  endif;

  write ufoot;
  if msgrrn > 0;
    *in40 = *on;
    write umsgctl;
  else;
    *in40 = *off;
  endif;
  exfmt uctl;

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

  selRrn = 0;
  selOpt = ' ';
  readc usfl;
  dow not %eof(wrkusrd);
    if sopt <> '';
      selRrn = rrn;
      selOpt = sopt;
      exsr handleOpt;
      selRrn = 0;
    endif;
    readc usfl;
  enddo;
enddo;

*inlr = *on;
return;

// ---------------------------------------------------------------------
begsr loadRows;
  numRows = 0;
  exec sql declare u1 cursor for
    select user_code, display_name, email_address, role_code, is_active
      from perpdemo.perp_user
     order by user_code;
  exec sql open u1;
  if sqlcode < 0;
    writeMsg('SQL open failed: SQLCODE=' + %char(sqlcode));
    return;
  endif;

  dow numRows < %elem(rows);
    exec sql fetch u1 into :uRow;
    if sqlcode = 100 or sqlcode < 0;
      leave;
    endif;
    numRows += 1;
    rows(numRows) = uRow;
  enddo;
  exec sql close u1;
endsr;

// ---------------------------------------------------------------------
begsr fillSubfile;
  rrn = 0;
  *in31 = *on;
  write uctl;
  *in31 = *off;
  for i = 1 to numRows;
    *in50 = *off;
    *in51 = *off;
    sopt   = '';
    sucode = rows(i).ucode;
    suname = rows(i).uname;
    surole = rows(i).urole;
    suact  = rows(i).uact;
    rrn += 1;
    write usfl;
  endfor;
endsr;

// ---------------------------------------------------------------------
begsr handleOpt;
  chain selRrn usfl;
  if %found(wrkusrd);
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
  eucode  = '';
  euname  = '';
  euemail = '';
  eurole  = 'REQUESTER';
  euact   = 'Y';
  exsr editLoop;
  if not *in12 and eucode <> '';
    exec sql
      insert into perpdemo.perp_user
        (user_code, display_name, email_address, role_code, is_active)
        values (:eucode, :euname, :euemail, :eurole, :euact);
    if sqlcode < 0;
      writeMsg('Add failed: SQLCODE=' + %char(sqlcode)
             + ' SQLSTATE=' + sqlstate);
    else;
      writeMsg('Added ' + %trim(eucode) + '.');
    endif;
  endif;
endsr;

// ---------------------------------------------------------------------
begsr changeRow;
  emode  = 'C';
  eucode = sucode;
  exec sql
    select display_name, email_address, role_code, is_active
      into :euname, :euemail, :eurole, :euact
      from perpdemo.perp_user
     where user_code = :eucode;
  if sqlcode <> 0;
    writeMsg('Row disappeared before change.');
    return;
  endif;
  exsr editLoop;
  if not *in12;
    exec sql
      update perpdemo.perp_user
         set display_name  = :euname,
             email_address = :euemail,
             role_code     = :eurole,
             is_active     = :euact,
             updated_at    = current_timestamp,
             updated_by    = user
       where user_code = :eucode;
    if sqlcode < 0;
      writeMsg('Change failed: SQLCODE=' + %char(sqlcode)
             + ' SQLSTATE=' + sqlstate);
    else;
      writeMsg('Updated ' + %trim(eucode) + '.');
    endif;
  endif;
endsr;

// ---------------------------------------------------------------------
begsr deleteRow;
  exec sql
    delete from perpdemo.perp_user
     where user_code = :sucode;
  if sqlcode < 0;
    writeMsg('Delete failed: SQLSTATE=' + sqlstate);
  else;
    writeMsg('Deleted ' + %trim(sucode) + '.');
  endif;
endsr;

// ---------------------------------------------------------------------
begsr displayRow;
  emode  = 'D';
  eucode = sucode;
  exec sql
    select display_name, email_address, role_code, is_active
      into :euname, :euemail, :eurole, :euact
      from perpdemo.perp_user
     where user_code = :eucode;
  exsr editLoop;
endsr;

// ---------------------------------------------------------------------
begsr editLoop;
  exfmt uedit;
endsr;

// ---------------------------------------------------------------------
begsr clearMsgs;
  msgrrn = 0;
  *in41 = *on;
  write umsgctl;
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
  write umsgsfl;
end-proc;
