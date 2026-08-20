**free

// ---------------------------------------------------------------------
// Program: reqentr  (Requisition Entry)
// Purpose: DSPF-based requisition entry, scoped by the company selected
//          via perpselr (*LDA positions 1-3). Header screen collects
//          requested_by/need_by/priority/notes, allocates the doc
//          number via docseq_next('REQ'), then inserts a DRAFT header.
//          Line screen is a subfile of requisition_line rows -- F6=Add
//          opens an edit panel that defaults UOM from item and
//          est_unit_cost from the item's preferred vendor price.
//          F8=Submit requires >=1 line and flips status to SUBMITTED;
//          no further changes are allowed once submitted.
// Epic:    PERP-6 (PERP-34)
// ---------------------------------------------------------------------

ctl-opt dftactgrp(*no) actgrp(*new) bnddir('PERP');

dcl-f reqentd workstn sfile(rlsfl:rrn) sfile(rmsgsfl:msgrrn);

/copy docseq_pr.rpgle

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

dcl-ds lineRow qualified;
  lnbr   int(10);
  item   varchar(25);
  qty    packed(15:4);
  uom    varchar(5);
  cost   packed(15:4);
end-ds;

dcl-ds rows likeds(lineRow) dim(500);
dcl-s numRows   int(10);
dcl-s i         int(10);
dcl-s rrn       int(10);
dcl-s msgrrn    int(10);
dcl-s selRrn    int(10);
dcl-s changeRrn int(10);
dcl-s selOpt    char(1);
dcl-s compcd    char(3);
dcl-s reqnbr    int(20);
dcl-s docerrmsg varchar(80);
dcl-s reqStatus varchar(20);
dcl-s nextLine  int(10);
dcl-s chgLnbr   int(10);
dcl-s cnt       int(10);
dcl-s edefuom   varchar(5);
dcl-s edefcost  packed(15:4);
dcl-s edesc     varchar(60);
dcl-s promptItem varchar(25);
dcl-s doneAll   ind;
dcl-s holdMsg   ind;

in ldaDS;
compcd = ldaDS.compcd;

if compcd = '';
  exsr clearMsgs;
  writeMsg('No company selected - run Select Company (PERPSELR) first.');
  *in40 = *on;
  write rmsgctl;
  hcompdsp = '';
  exfmt rhead;
  *inlr = *on;
  return;
endif;

hcompdsp = compcd;

// -----------------------------------------------------------------------
// Header entry -- collect requested_by/need_by/priority/notes, validate,
// allocate the doc number, insert the DRAFT header. Wrapped in an outer
// loop (PERP-86) so a successful F8=Submit on the line screen below
// returns here for the NEXT requisition instead of leaving the user
// stranded on the now-read-only submitted line list.
// -----------------------------------------------------------------------
doneAll = *off;

dow not doneAll;
  // PERP-76: default Requested By to the current job user, but only when
  // that job user is actually a known perp_user -- an interactive/SSH job
  // user (e.g. AIDEMO) will almost never be one, and pre-filling with a
  // value that then fails the "not found" check below just traded a blank
  // required field for a confusing default the human has to notice and
  // overwrite anyway. Leaving it blank keeps the existing, already-clear
  // "Requested By is required" prompt as the fallback.
  exec sql values(user) into :hreqby;
  exec sql
    select count(*) into :cnt
      from perpdemo.perp_user
     where user_code = :hreqby;
  if cnt = 0;
    hreqby = '';
  endif;
  hneedby = %date() + %days(7);
  hpricd  = 'NORMAL';
  hnotes  = '';

  // PERP-86: a submit confirmation queued just before restarting this
  // loop must survive one pass before clearMsgs wipes it -- same
  // holdMsg pattern as PERP-74/PERP-87.
  if holdMsg;
    holdMsg = *off;
  else;
    exsr clearMsgs;
  endif;

  dow '1';
  *in40 = *off;
  if msgrrn > 0;
    *in40 = *on;
  endif;
  write rmsgctl;
  exfmt rhead;

  if *in03 or *in12;
    *inlr = *on;
    return;
  endif;

  exsr clearMsgs;

  if %trim(hreqby) = '';
    writeMsg('Requested By is required.');
    iter;
  endif;

  exec sql
    select count(*) into :cnt
      from perpdemo.perp_user
     where user_code = :hreqby;
  if cnt = 0;
    writeMsg('Requested By ' + %trim(hreqby) + ' not found.');
    iter;
  endif;

  if %trim(hpricd) = '';
    hpricd = 'NORMAL';
  endif;

  exec sql
    select count(*) into :cnt
      from perpdemo.code_master
     where code_type = 'PRIORITY' and code_value = :hpricd;
  if cnt = 0;
    writeMsg('Priority ' + %trim(hpricd) + ' not valid.');
    iter;
  endif;

  leave;
enddo;

reqnbr = docseq_next(compcd : 'REQ' : docerrmsg);
if reqnbr = 0;
  writeMsg('Could not allocate requisition number: ' + docerrmsg);
  *inlr = *on;
  return;
endif;

exec sql
  insert into perpdemo.requisition_header
    (company_code, requisition_number, requested_by, need_by_date,
     priority_code, notes)
    values (:compcd, :reqnbr, :hreqby, :hneedby, :hpricd, :hnotes);
if sqlcode < 0;
  writeMsg('Could not create requisition: SQLCODE=' + %char(sqlcode));
  *inlr = *on;
  return;
endif;

// -----------------------------------------------------------------------
// Line entry -- subfile of requisition_line rows.
// -----------------------------------------------------------------------
exsr clearMsgs;
writeMsg('Requisition ' + %char(reqnbr) + ' created (DRAFT). Add lines, '
       + 'then F8=Submit.');

dow '1';
  exsr loadHeader;
  exsr loadLines;

  if numRows = 0;
    *in30 = *off;
    write rnolin;
  else;
    exsr fillSubfile;
    *in30 = *on;
  endif;

  write rlfoot;
  *in40 = *off;
  if msgrrn > 0;
    *in40 = *on;
  endif;
  write rmsgctl;
  exfmt rlctl;

  // PERP-85: F3 exits the program; F12 must NOT -- it should just step
  // back to (redisplay) this line list rather than ending the whole
  // flow. The requisition header is already committed at this point,
  // so there's no earlier screen to unwind into.
  if *in03;
    doneAll = *on;
    leave;
  endif;
  if *in12;
    iter;
  endif;

  exsr clearMsgs;

  if *in06;
    if reqStatus <> 'DRAFT';
      writeMsg('Requisition already submitted - no further changes allowed.');
    else;
      exsr addLine;
    endif;
    iter;
  endif;

  if *in08;
    if numRows = 0;
      writeMsg('At least one line is required before submitting.');
    elseif reqStatus <> 'DRAFT';
      writeMsg('Requisition already submitted.');
    else;
      exec sql
        update perpdemo.requisition_header
           set status_code = 'SUBMITTED',
               updated_at  = current_timestamp,
               updated_by  = user
         where company_code = :compcd and requisition_number = :reqnbr;
      if sqlcode < 0;
        writeMsg('Submit failed: SQLCODE=' + %char(sqlcode));
      else;
        // PERP-86: return to a fresh header entry screen for the next
        // requisition instead of staying parked on this now read-only
        // line list. holdMsg carries this confirmation through to the
        // restarted header loop above.
        writeMsg('Requisition ' + %char(reqnbr) + ' submitted.');
        holdMsg = *on;
        leave;
      endif;
    endif;
    iter;
  endif;

  // Fully drain the READC loop before switching to RLEDIT for a change
  // (option 2). Calling changeLine in-line here -- while a READC cursor
  // is still active on RLSFL -- disturbs the subfile's pending read
  // sequence on this same device file, and the next readc raises
  // CPF5006 ("Session or device error occurred in file REQENTD"). Same
  // fix as PERPSELR/REQAPRR: collect the row to change while draining
  // (deletes and invalid-option messages don't switch formats, so they
  // still run in-line), act on the change only after the loop finishes
  // (DDL_STYLE_GUIDE.md Sec.14).
  if numRows > 0;
    selRrn    = 0;
    selOpt    = ' ';
    changeRrn = 0;
    readc rlsfl;
    dow not %eof(reqentd);
      if slopt <> '';
        selRrn = rrn;
        selOpt = slopt;
        exsr handleOpt;
        selRrn = 0;
      endif;
      readc rlsfl;
    enddo;

    if changeRrn > 0 and msgrrn = 0;
      selRrn = changeRrn;
      exsr changeLine;
    endif;
  endif;

  enddo;
enddo;

*inlr = *on;
return;

// ---------------------------------------------------------------------
begsr loadHeader;
  exec sql
    select status_code, requested_by, need_by_date, priority_code,
           total_estimated_cost
      into :reqStatus, :dreqby, :dneedby, :dpricd, :dtotest
      from perpdemo.requisition_header
     where company_code = :compcd and requisition_number = :reqnbr;
  dreqnbr  = %char(reqnbr);
  dstatus  = reqStatus;
endsr;

// ---------------------------------------------------------------------
begsr loadLines;
  numRows = 0;
  exec sql declare c1 cursor for
    select line_number, item_number, quantity, uom_code, est_unit_cost
      from perpdemo.requisition_line
     where company_code = :compcd and requisition_number = :reqnbr
     order by line_number;
  exec sql open c1;
  if sqlcode < 0;
    writeMsg('SQL open failed: SQLCODE=' + %char(sqlcode));
    leavesr;
  endif;

  dow numRows < %elem(rows);
    exec sql fetch c1 into :lineRow.lnbr, :lineRow.item, :lineRow.qty,
                            :lineRow.uom, :lineRow.cost;
    if sqlcode = 100 or sqlcode < 0;
      leave;
    endif;
    numRows += 1;
    rows(numRows) = lineRow;
  enddo;
  exec sql close c1;
endsr;

// ---------------------------------------------------------------------
begsr fillSubfile;
  rrn = 0;
  *in31 = *on;
  write rlctl;
  *in31 = *off;
  for i = 1 to numRows;
    slopt  = '';
    slline = rows(i).lnbr;
    slitem = rows(i).item;
    slqty  = rows(i).qty;
    sluom  = rows(i).uom;
    slcost = rows(i).cost;
    rrn += 1;
    write rlsfl;
  endfor;
endsr;

// ---------------------------------------------------------------------
begsr handleOpt;
  chain selRrn rlsfl;
  if %found(reqentd);
    if reqStatus <> 'DRAFT';
      writeMsg('Requisition already submitted - no further changes allowed.');
      leavesr;
    endif;
    select;
      when selOpt = '2';
        if changeRrn = 0;
          changeRrn = selRrn;
        else;
          writeMsg('Only one line may be changed per Enter.');
        endif;
      when selOpt = '4';
        exsr deleteLine;
      other;
        writeMsg('Option ' + selOpt + ' not valid.');
    endsl;
  endif;
endsr;

// ---------------------------------------------------------------------
begsr addLine;
  emode = 'A';
  eitem = '';
  eqty  = 0;
  euom  = '';
  ecost = 0;

  dow '1';
    exfmt rledit;
    if *in12;
      leavesr;
    endif;

    // Item Number prompt (PERP-61): '?' + Enter invokes the standard
    // reusable Item Number lookup (PERP-56) and returns the selection.
    if %trim(eitem) = '?';
      promptItem = eitem;
      callItmprmt(compcd : promptItem);
      eitem = promptItem;
      iter;
    endif;

    if %trim(eitem) = '';
      writeMsg('Item Number is required.');
      iter;
    endif;

    exec sql
      select item_description, inventory_uom
        into :edesc, :edefuom
        from perpdemo.item
       where company_code = :compcd and item_number = :eitem;
    if sqlcode <> 0;
      writeMsg('Item ' + %trim(eitem) + ' not found for this company.');
      iter;
    endif;

    if eqty <= 0;
      writeMsg('Quantity must be greater than zero.');
      iter;
    endif;

    if %trim(euom) = '';
      euom = edefuom;
    endif;

    if ecost = 0;
      exec sql
        select ivp.unit_price
          into :edefcost
          from perpdemo.item_vendor_price ivp
          join perpdemo.item_vendor iv
            on iv.company_code = ivp.company_code
           and iv.item_number  = ivp.item_number
           and iv.vendor_code  = ivp.vendor_code
         where ivp.company_code = :compcd
           and ivp.item_number  = :eitem
           and iv.is_preferred  = 'Y'
           and ivp.effective_to is null
         fetch first 1 row only;
      if sqlcode = 0;
        ecost = edefcost;
      endif;
    endif;

    leave;
  enddo;

  exec sql
    select coalesce(max(line_number), 0) + 1
      into :nextLine
      from perpdemo.requisition_line
     where company_code = :compcd and requisition_number = :reqnbr;

  exec sql
    insert into perpdemo.requisition_line
      (company_code, requisition_number, line_number, item_number,
       quantity, uom_code, est_unit_cost)
      values (:compcd, :reqnbr, :nextLine, :eitem, :eqty, :euom, :ecost);
  if sqlcode < 0;
    writeMsg('Add line failed: SQLCODE=' + %char(sqlcode)
           + ' SQLSTATE=' + sqlstate);
  else;
    writeMsg('Added line ' + %char(nextLine) + ': ' + %trim(edesc)
           + ' (' + %trim(euom) + ' @ ' + %char(ecost) + ').');
    exsr recalcTotal;
  endif;
endsr;

// ---------------------------------------------------------------------
begsr changeLine;
  // Read from the rows() snapshot (populated by loadLines), not the live
  // RLSFL buffer -- changeLine now runs after the READC loop has fully
  // drained (see the fix note above the readc loop), by which point the
  // subfile buffer holds whatever row READC last visited, not
  // necessarily this one.
  chgLnbr = rows(selRrn).lnbr;
  emode   = 'C';
  eitem   = rows(selRrn).item;
  eqty    = rows(selRrn).qty;
  euom    = rows(selRrn).uom;
  ecost   = rows(selRrn).cost;

  dow '1';
    exfmt rledit;
    if *in12;
      leavesr;
    endif;

    // Item Number prompt (PERP-61): '?' + Enter invokes the standard
    // reusable Item Number lookup (PERP-56) and returns the selection.
    if %trim(eitem) = '?';
      promptItem = eitem;
      callItmprmt(compcd : promptItem);
      eitem = promptItem;
      iter;
    endif;

    leave;
  enddo;

  if eqty <= 0;
    writeMsg('Quantity must be greater than zero.');
    leavesr;
  endif;

  exec sql
    update perpdemo.requisition_line
       set quantity      = :eqty,
           uom_code       = :euom,
           est_unit_cost  = :ecost,
           updated_at     = current_timestamp,
           updated_by     = user
     where company_code = :compcd and requisition_number = :reqnbr
       and line_number   = :chgLnbr;
  if sqlcode < 0;
    writeMsg('Change failed: SQLCODE=' + %char(sqlcode));
  else;
    writeMsg('Updated line ' + %char(chgLnbr) + '.');
    exsr recalcTotal;
  endif;
endsr;

// ---------------------------------------------------------------------
begsr deleteLine;
  exec sql
    delete from perpdemo.requisition_line
     where company_code = :compcd and requisition_number = :reqnbr
       and line_number   = :slline;
  if sqlcode < 0;
    writeMsg('Delete failed: SQLSTATE=' + sqlstate);
  else;
    writeMsg('Deleted line ' + %char(slline) + '.');
    exsr recalcTotal;
  endif;
endsr;

// ---------------------------------------------------------------------
begsr recalcTotal;
  exec sql
    update perpdemo.requisition_header
       set total_estimated_cost =
             (select coalesce(sum(quantity * est_unit_cost), 0)
                from perpdemo.requisition_line
               where company_code = :compcd
                 and requisition_number = :reqnbr),
           updated_at = current_timestamp,
           updated_by = user
     where company_code = :compcd and requisition_number = :reqnbr;
endsr;

// ---------------------------------------------------------------------
begsr clearMsgs;
  msgrrn = 0;
  *in41 = *on;
  write rmsgctl;
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
  write rmsgsfl;
end-proc;
