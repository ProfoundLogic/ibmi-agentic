**free

// ---------------------------------------------------------------------
// Program: poschr  (Blanket PO Schedule Maintenance)
// Purpose: Maintain po_line_schedule rows for a given (company,
//          po_number, line_number). Blanket POs use a schedule row per
//          delivery; a non-blanket line simply has zero schedule rows.
//          Company comes from *LDA[1:3] via perpselr. PO and line are
//          entered on the header screen (or arrive via optional call
//          parameters -- see the parm list on the entry procedure).
//
//          Total scheduled qty is compared against po_line.ordered_qty
//          on every refresh. Excess is highlighted with indicator 70
//          (red warning line) and a message -- soft, not a hard block,
//          per the PERP-40 requirement.
// Epic:    PERP-7 (PERP-40)
// ---------------------------------------------------------------------

// datfmt(*iso) for consistency with the other PO programs -- the job
// DATFMT here is *MDY (2-digit year, 1940-2039).
ctl-opt dftactgrp(*no) actgrp(*new) datfmt(*iso);

dcl-f poschd workstn sfile(hsfl:rrn) sfile(hmsgsfl:msgrrn);

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

// Optional entry parameters. When both parms carry non-blank data the
// header screen is skipped and the schedule is loaded immediately. A
// call with no parms drops the user on the header entry screen.
// Optional parms via OPTIONS(*NOPASS) -- menu option calls with none,
// PO detail calls with both (PERP-41 wires this).
dcl-pi *n;
  in_ponbr char(10) const options(*nopass);
  in_line  packed(3:0) const options(*nopass);
end-pi;

dcl-ds schedRow qualified;
  seq     int(10);
  schdt   date;
  qty     packed(15:4);
  rcvqty  packed(15:4);
  notes   varchar(240);
end-ds;

dcl-ds rows likeds(schedRow) dim(500);
dcl-s numRows  int(10);
dcl-s rrn      int(10);
dcl-s msgrrn   int(10);
dcl-s selRrn   int(10);
dcl-s changeRrn int(10);
dcl-s i        int(10);
dcl-s compcd   char(3);
dcl-s ponbr    int(20);
dcl-s lineNbr  int(10);
dcl-s chgSeq   int(10);
dcl-s nextSeq  int(10);
dcl-s selOpt   char(1);
dcl-s cnt      int(10);
dcl-s parmed   ind;
// po_line context for the header.
dcl-s itemNbr  varchar(25);
dcl-s vndCd    varchar(10);
dcl-s ordQty   packed(15:4);
dcl-s uomCd    varchar(5);
dcl-s schTot   packed(15:4);
dcl-s rcvTot   packed(15:4);

in ldaDS;
compcd = ldaDS.compcd;

if compcd = '';
  exsr clearMsgs;
  writeMsg('No company selected - run Select Company (PERPSELR) first.');
  *in40 = *on;
  hcompdsp = '';
  hponbr = '';
  hline = 0;
  write hmsgctl;
  exfmt hhead;
  *inlr = *on;
  return;
endif;

hcompdsp = compcd;

// Prefill from parms when supplied. Guard reads with %parms() because
// the parms are OPTIONS(*NOPASS) -- menu callers pass none.
parmed = *off;
if %parms() >= 2;
  if %trim(in_ponbr) <> '' and in_line > 0;
    hponbr = in_ponbr;
    hline  = in_line;
    parmed = *on;
  endif;
endif;

// -----------------------------------------------------------------------
// Header entry loop -- collect (or accept from parms) po_number + line;
// look them up on po_line to prove they exist and grab item / vendor /
// ordered_qty / uom for the detail screen.
// -----------------------------------------------------------------------
dow '1';
  if not parmed;
    *in40 = *off;
    if msgrrn > 0;
      *in40 = *on;
    endif;
    exfmt hhead;

    if *in03 or *in12;
      *inlr = *on;
      return;
    endif;
  endif;

  exsr clearMsgs;

  if %trim(hponbr) = '';
    writeMsg('PO Number is required.');
    parmed = *off;
    iter;
  endif;

  monitor;
    ponbr = %int(%trim(hponbr));
  on-error;
    writeMsg('PO Number ' + %trim(hponbr) + ' must be numeric.');
    parmed = *off;
    iter;
  endmon;

  if hline <= 0;
    writeMsg('Line Number must be greater than zero.');
    parmed = *off;
    iter;
  endif;
  lineNbr = hline;

  exec sql
    select item_number,
           (select vendor_code from perpdemo.po_header
             where company_code = :compcd and po_number = :ponbr),
           ordered_qty, uom_code, received_qty
      into :itemNbr, :vndCd, :ordQty, :uomCd, :rcvTot
      from perpdemo.po_line
     where company_code = :compcd
       and po_number    = :ponbr
       and line_number  = :lineNbr;
  if sqlcode = 100;
    writeMsg('PO ' + %char(ponbr) + ' line ' + %char(lineNbr)
           + ' not found.');
    parmed = *off;
    iter;
  endif;
  if sqlcode < 0;
    writeMsg('Lookup failed: SQLCODE=' + %char(sqlcode));
    parmed = *off;
    iter;
  endif;

  leave;
enddo;

// -----------------------------------------------------------------------
// Schedule maintenance loop.
// -----------------------------------------------------------------------
dow '1';
  exsr loadSched;

  if numRows = 0;
    *in30 = *off;
    write hnosch;
  else;
    exsr fillSubfile;
    *in30 = *on;
  endif;

  // Header context.
  dponbr  = hponbr;
  dline   = lineNbr;
  ditem   = itemNbr;
  dvndcd  = vndCd;
  dordqty = ordQty;
  duom    = uomCd;
  dschtot = schTot;
  drcvtot = rcvTot;
  *in70 = *off;
  if schTot > ordQty;
    *in70 = *on;
  endif;

  write hfoot;
  *in40 = *off;
  if msgrrn > 0;
    *in40 = *on;
  endif;
  write hmsgctl;
  exfmt hsctl;

  if *in03 or *in12;
    leave;
  endif;

  exsr clearMsgs;

  if *in06;
    exsr addSched;
    iter;
  endif;

  // Drain READC loop before switching to HEDIT for a change
  // (DDL_STYLE_GUIDE.md Sec.14 pattern).
  if numRows > 0;
    selRrn    = 0;
    selOpt    = ' ';
    changeRrn = 0;
    readc hsfl;
    dow not %eof(poschd);
      if slopt <> '';
        selRrn = rrn;
        selOpt = slopt;
        exsr handleOpt;
        selRrn = 0;
      endif;
      readc hsfl;
    enddo;

    if changeRrn > 0 and msgrrn = 0;
      selRrn = changeRrn;
      exsr changeSched;
    endif;
  endif;

enddo;

*inlr = *on;
return;

// ---------------------------------------------------------------------
begsr loadSched;
  numRows = 0;
  schTot  = 0;

  exec sql declare sc1 cursor for
    select schedule_seq, scheduled_date, scheduled_qty, received_qty,
           notes
      from perpdemo.po_line_schedule
     where company_code = :compcd
       and po_number    = :ponbr
       and line_number  = :lineNbr
     order by schedule_seq;
  exec sql open sc1;
  if sqlcode < 0;
    writeMsg('SQL open failed: SQLCODE=' + %char(sqlcode));
    return;
  endif;

  dow numRows < %elem(rows);
    exec sql fetch sc1 into :schedRow.seq, :schedRow.schdt,
                             :schedRow.qty, :schedRow.rcvqty,
                             :schedRow.notes;
    if sqlcode = 100 or sqlcode < 0;
      leave;
    endif;
    numRows += 1;
    rows(numRows) = schedRow;
    schTot += schedRow.qty;
  enddo;
  exec sql close sc1;

  // Refresh po_line.received_qty (a receipt program may have updated it).
  exec sql
    select received_qty
      into :rcvTot
      from perpdemo.po_line
     where company_code = :compcd
       and po_number    = :ponbr
       and line_number  = :lineNbr;
endsr;

// ---------------------------------------------------------------------
begsr fillSubfile;
  rrn = 0;
  *in31 = *on;
  write hsctl;
  *in31 = *off;
  for i = 1 to numRows;
    slopt    = '';
    slseq    = rows(i).seq;
    slschdt  = rows(i).schdt;
    slschqty = rows(i).qty;
    slrcvqty = rows(i).rcvqty;
    slschnot = rows(i).notes;
    rrn += 1;
    write hsfl;
  endfor;
endsr;

// ---------------------------------------------------------------------
begsr handleOpt;
  chain selRrn hsfl;
  if %found(poschd);
    select;
      when selOpt = '2';
        if changeRrn = 0;
          changeRrn = selRrn;
        else;
          writeMsg('Only one row may be changed per Enter.');
        endif;
      when selOpt = '4';
        exsr deleteSched;
      other;
        writeMsg('Option ' + selOpt + ' not valid.');
    endsl;
  endif;
endsr;

// ---------------------------------------------------------------------
begsr addSched;
  emode  = 'A';
  eseq   = 0;
  eschdt = %date();
  eschqty = 0;
  ercvqty = 0;
  enotes = '';

  dow '1';
    exfmt hedit;
    if *in12;
      return;
    endif;

    if eschqty <= 0;
      writeMsg('Scheduled Qty must be greater than zero.');
      iter;
    endif;

    if ercvqty < 0;
      writeMsg('Received Qty cannot be negative.');
      iter;
    endif;

    if ercvqty > eschqty;
      writeMsg('Received Qty cannot exceed Scheduled Qty.');
      iter;
    endif;

    leave;
  enddo;

  exec sql
    select coalesce(max(schedule_seq), 0) + 1
      into :nextSeq
      from perpdemo.po_line_schedule
     where company_code = :compcd
       and po_number    = :ponbr
       and line_number  = :lineNbr;

  exec sql
    insert into perpdemo.po_line_schedule
      (company_code, po_number, line_number, schedule_seq,
       scheduled_date, scheduled_qty, received_qty, notes)
      values (:compcd, :ponbr, :lineNbr, :nextSeq,
              :eschdt, :eschqty, :ercvqty, :enotes);
  if sqlcode < 0;
    writeMsg('Add failed: SQLCODE=' + %char(sqlcode)
           + ' STATE=' + sqlstate);
    return;
  endif;

  exec sql commit;
  writeMsg('Added schedule seq ' + %char(nextSeq) + ' ('
         + %char(eschqty) + ' on ' + %char(eschdt) + ').');
endsr;

// ---------------------------------------------------------------------
begsr changeSched;
  chgSeq = rows(selRrn).seq;
  emode  = 'C';
  eseq   = chgSeq;
  eschdt = rows(selRrn).schdt;
  eschqty = rows(selRrn).qty;
  ercvqty = rows(selRrn).rcvqty;
  enotes = rows(selRrn).notes;

  exfmt hedit;
  if *in12;
    return;
  endif;

  if eschqty <= 0;
    writeMsg('Scheduled Qty must be greater than zero.');
    return;
  endif;

  if ercvqty < 0 or ercvqty > eschqty;
    writeMsg('Received Qty must be between 0 and Scheduled Qty.');
    return;
  endif;

  exec sql
    update perpdemo.po_line_schedule
       set scheduled_date = :eschdt,
           scheduled_qty  = :eschqty,
           received_qty   = :ercvqty,
           notes          = :enotes,
           updated_at     = current_timestamp,
           updated_by     = user
     where company_code = :compcd
       and po_number    = :ponbr
       and line_number  = :lineNbr
       and schedule_seq = :chgSeq;
  if sqlcode < 0;
    writeMsg('Change failed: SQLCODE=' + %char(sqlcode));
    return;
  endif;

  exec sql commit;
  writeMsg('Updated schedule seq ' + %char(chgSeq) + '.');
endsr;

// ---------------------------------------------------------------------
begsr deleteSched;
  exec sql
    delete from perpdemo.po_line_schedule
     where company_code = :compcd
       and po_number    = :ponbr
       and line_number  = :lineNbr
       and schedule_seq = :slseq;
  if sqlcode < 0;
    writeMsg('Delete failed: SQLSTATE=' + sqlstate);
    return;
  endif;
  exec sql commit;
  writeMsg('Deleted schedule seq ' + %char(slseq) + '.');
endsr;

// ---------------------------------------------------------------------
begsr clearMsgs;
  msgrrn = 0;
  *in41 = *on;
  write hmsgctl;
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
  write hmsgsfl;
end-proc;
