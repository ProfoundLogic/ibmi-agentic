**free

// ---------------------------------------------------------------------
// Program: wrkiclr  (Work with Item Classes)
// Purpose: DSPF-based CRUD for item_class, scoped by the company
//          selected via perpselr (*LDA positions 1-3). Options 2/4/5
//          change/delete/display; F6 adds. Delete falls through to a
//          DB2 FK violation (SQL0532/SQL0531) if the class is
//          referenced by item -- RPG surfaces the SQLSTATE cleanly
//          rather than pre-checking.
// Epic:    PERP-3 (PERP-22)
// ---------------------------------------------------------------------

ctl-opt dftactgrp(*no) actgrp(*new);

dcl-f wrkicld workstn sfile(icsfl:rrn) sfile(icmsgsfl:msgrrn);

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

dcl-ds icRow qualified;
  class   varchar(10);
  desc    varchar(60);
  active  char(1);
end-ds;

dcl-ds rows likeds(icRow) dim(500);
dcl-s numRows int(10);
dcl-s i       int(10);
dcl-s rrn     int(10);
dcl-s msgrrn  int(10);
dcl-s msgkey  char(4);
dcl-s selRrn  int(10);
dcl-s selOpt  char(1);
dcl-s compcd  char(3);
dcl-s holdMsg ind;
dcl-s validationFailed ind;

in ldaDS;
compcd = ldaDS.compcd;
scompdsp = compcd;

if compcd = '';
  exsr clearMsgs;
  writeMsg('No company selected - run Select Company (PERPSELR) first.');
endif;

dow not *in03 and not *in12;
  if compcd = '';
    *in30 = *off;
    write icnoco;
    write icfoot;
    if msgrrn > 0;
      *in40 = *on;
      write icmsgctl;
    else;
      *in40 = *off;
    endif;
    exfmt icctl;
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
    write icnone;
  else;
    exsr fillSubfile;
    *in30 = *on;
  endif;

  write icfoot;
  if msgrrn > 0;
    *in40 = *on;
    write icmsgctl;
  else;
    *in40 = *off;
  endif;
  exfmt icctl;

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
    readc icsfl;
    dow not %eof(wrkicld);
      if sopt <> '';
        selRrn = rrn;
        selOpt = sopt;
        exsr handleOpt;
        selRrn = 0;
      endif;
      readc icsfl;
    enddo;
  endif;

enddo;

*inlr = *on;
return;

// ---------------------------------------------------------------------
begsr loadRows;
  numRows = 0;
  exec sql declare c1 cursor for
    select class_code, description, is_active
      from perpdemo.item_class
     where company_code = :compcd
     order by class_code;
  exec sql open c1;
  if sqlcode < 0;
    writeMsg('SQL open failed: SQLCODE=' + %char(sqlcode));
    leavesr;
  endif;

  dow numRows < %elem(rows);
    exec sql fetch c1 into :icRow;
    if sqlcode = 100 or sqlcode < 0;
      leave;
    endif;
    numRows += 1;
    rows(numRows) = icRow;
  enddo;
  exec sql close c1;
endsr;

// ---------------------------------------------------------------------
begsr fillSubfile;
  rrn = 0;
  *in31 = *on;
  write icctl;
  *in31 = *off;
  for i = 1 to numRows;
    *in50 = *off;
    *in51 = *off;
    sopt   = '';
    sclass = rows(i).class;
    sdesc  = rows(i).desc;
    rrn += 1;
    write icsfl;
  endfor;
endsr;

// ---------------------------------------------------------------------
begsr handleOpt;
  chain selRrn icsfl;
  if %found(wrkicld);
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
  eclass  = '';
  edesc   = '';
  eactive = 'Y';
  exsr editLoop;
  if not *in12;
    exsr validateClass;
    if validationFailed;
      holdMsg = *on;
    else;
      exec sql
        insert into perpdemo.item_class (company_code, class_code, description, is_active)
          values (:compcd, :eclass, :edesc, :eactive);
      if sqlcode < 0;
        writeMsg('Add failed: SQLCODE=' + %char(sqlcode)
               + ' SQLSTATE=' + sqlstate);
      else;
        writeMsg('Added ' + %trim(eclass) + '.');
      endif;
      holdMsg = *on;
    endif;
  endif;
endsr;

// ---------------------------------------------------------------------
begsr changeRow;
  exsr clearMsgs;
  emode  = 'C';
  eclass = sclass;
  exec sql
    select description, is_active
      into :edesc, :eactive
      from perpdemo.item_class
     where company_code = :compcd and class_code = :eclass;
  if sqlcode <> 0;
    writeMsg('Row disappeared before change.');
    holdMsg = *on;
    leavesr;
  endif;
  exsr editLoop;
  if not *in12;
    exsr validateClass;
    if validationFailed;
      holdMsg = *on;
    else;
      exec sql
        update perpdemo.item_class
           set description = :edesc,
               is_active   = :eactive,
               updated_at  = current_timestamp,
               updated_by  = user
         where company_code = :compcd and class_code = :eclass;
      if sqlcode < 0;
        writeMsg('Change failed: SQLCODE=' + %char(sqlcode));
      else;
        writeMsg('Updated ' + %trim(eclass) + '.');
      endif;
      holdMsg = *on;
    endif;
  endif;
endsr;

// ---------------------------------------------------------------------
begsr deleteRow;
  exsr clearMsgs;
  exec sql
    delete from perpdemo.item_class
     where company_code = :compcd and class_code = :sclass;
  if sqlcode < 0;
    writeMsg('Delete failed (in use?): SQLSTATE=' + sqlstate);
  else;
    writeMsg('Deleted ' + %trim(sclass) + '.');
  endif;
  holdMsg = *on;
endsr;

// ---------------------------------------------------------------------
begsr displayRow;
  emode  = 'D';
  eclass = sclass;
  exec sql
    select description, is_active
      into :edesc, :eactive
      from perpdemo.item_class
     where company_code = :compcd and class_code = :eclass;
  exsr editLoop;
endsr;

// ---------------------------------------------------------------------
begsr editLoop;
  exfmt icedit;
endsr;

// ---------------------------------------------------------------------
// Required-field validation for the Add/Change panel, done here in RPG
// instead of letting a blank required field surface as a raw NOT-NULL
// violation from the database.
begsr validateClass;
  validationFailed = *off;
  if %trim(eclass) = '';
    writeMsg('Class Code is required.');
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
  write icmsgctl;
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
  write icmsgsfl;
end-proc;
