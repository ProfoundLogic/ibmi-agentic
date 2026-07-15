**free

// ---------------------------------------------------------------------
// Program: wlmr  (Warehouse Layout Maintenance)
// Purpose: Single-record display + edit of warehouse_layout -- one row
//          per company, scoped by *LDA company_code (set by PERPSELR).
//          Enter=Save (update if the row exists), F6=Add (insert the
//          row the first time), F5=Refresh (discard in-progress edits
//          and reload from DB), F12=Cancel (same as Refresh), F3=Exit.
// Epic:    PERP-4 (3D Warehouse Map) / PERP-26
// ---------------------------------------------------------------------

ctl-opt dftactgrp(*no) actgrp(*new);

dcl-f wlmd workstn sfile(wlmsgsfl:msgrrn);

// Local Data Area -- perpselr stamps the selected company at pos 1-3.
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

dcl-s compcd  char(3);
dcl-s exists  ind;
dcl-s msgrrn  int(10);

in ldaDS;
compcd = ldaDS.compcd;

if compcd = '';
  exsr clearMsgs;
  writeMsg('No company selected - run Select Company (PERPSELR) first.');
endif;

dow not *in03 and not *in12;
  if compcd = '';
    write wlnoco;
    write wlfoot;
    if msgrrn > 0;
      *in40 = *on;
      write wlmsgctl;
    else;
      *in40 = *off;
    endif;
    exfmt wledit;
    leave;
  endif;

  exsr clearMsgs;
  exsr loadRow;

  write wlfoot;
  if msgrrn > 0;
    *in40 = *on;
    write wlmsgctl;
  else;
    *in40 = *off;
  endif;
  exfmt wledit;

  if *in03 or *in12;
    leave;
  endif;

  if *in05;
    iter;  // refresh -- discard in-progress edits
  endif;

  if *in06;
    exsr addRow;
    iter;
  endif;

  // Enter -- save changes to the existing row.
  if exists;
    exsr updateRow;
  else;
    writeMsg('No row yet for company ' + %trim(compcd)
           + ' - press F6=Add to create it.');
  endif;
enddo;

*inlr = *on;
return;

// ---------------------------------------------------------------------
begsr loadRow;
  ecompc = compcd;
  exec sql
    select warehouse_name, aisle_count, bays_per_aisle, shelves_per_bay,
           bin_width_m, bin_depth_m, bin_height_m, aisle_spacing_m, notes
      into :ewhsnm, :easlcnt, :ebayspa, :eshlfsb,
           :ebinwid, :ebindep, :ebinhgt, :easlspc, :enotes
      from perpdemo.warehouse_layout
     where company_code = :compcd;

  if sqlcode = 0;
    exists = *on;
    estat  = 'EXISTING';
  else;
    exists  = *off;
    estat   = 'NEW';
    ewhsnm  = '';
    easlcnt = 1;
    ebayspa = 1;
    eshlfsb = 1;
    ebinwid = 1;
    ebindep = 1;
    ebinhgt = 1;
    easlspc = 2;
    enotes  = '';
  endif;
endsr;

// ---------------------------------------------------------------------
begsr addRow;
  if exists;
    writeMsg('Row already exists for company ' + %trim(compcd)
           + ' - press Enter to save changes.');
    return;
  endif;

  exec sql
    insert into perpdemo.warehouse_layout
      (company_code, warehouse_name, aisle_count, bays_per_aisle,
       shelves_per_bay, bin_width_m, bin_depth_m, bin_height_m,
       aisle_spacing_m, notes)
      values (:compcd, :ewhsnm, :easlcnt, :ebayspa, :eshlfsb,
              :ebinwid, :ebindep, :ebinhgt, :easlspc, :enotes);
  if sqlcode < 0;
    writeMsg('Add failed: SQLCODE=' + %char(sqlcode)
           + ' SQLSTATE=' + sqlstate);
  else;
    writeMsg('Warehouse layout created for company ' + %trim(compcd) + '.');
  endif;
endsr;

// ---------------------------------------------------------------------
begsr updateRow;
  exec sql
    update perpdemo.warehouse_layout
       set warehouse_name  = :ewhsnm,
           aisle_count     = :easlcnt,
           bays_per_aisle  = :ebayspa,
           shelves_per_bay = :eshlfsb,
           bin_width_m     = :ebinwid,
           bin_depth_m     = :ebindep,
           bin_height_m    = :ebinhgt,
           aisle_spacing_m = :easlspc,
           notes           = :enotes,
           updated_at      = current_timestamp,
           updated_by      = user
     where company_code = :compcd;
  if sqlcode < 0;
    writeMsg('Update failed: SQLCODE=' + %char(sqlcode)
           + ' SQLSTATE=' + sqlstate);
  else;
    writeMsg('Warehouse layout updated for company ' + %trim(compcd) + '.');
  endif;
endsr;

// ---------------------------------------------------------------------
begsr clearMsgs;
  msgrrn = 0;
  *in41 = *on;
  write wlmsgctl;
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
  write wlmsgsfl;
end-proc;
