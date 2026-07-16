**free

// ---------------------------------------------------------------------
// Program: reqaprr  (Requisition Approval)
// Purpose: Lists SUBMITTED requisitions for the selected company
//          (*LDA positions 1-3, via perpselr). Selecting a row (option
//          5=Review) shows a detail screen (header summary + up to 6
//          lines as plain fields + a confidence_pct badge) with
//          Approve/Reject actions. Both actions stamp approved_by (job
//          USER), approved_at, approval_source=HUMAN, and the entered
//          approval_notes; the requisition leaves the SUBMITTED list
//          either way.
//
//          The detail screen is a single plain (non-subfile) record in
//          this SAME program/file, showing up to 6 lines as
//          individually-named fields, each conditioned on its own
//          indicator (*in60..*in65) so an unused row is genuinely
//          blank rather than displaying "0.0000".
//
//          This design exists after extensive, reproducible live
//          testing on this specific environment (Profound UI Genie,
//          "classic" skin) ruled out a second business subfile in this
//          program:
//            1. ASFL (list) + ALSFL/ALCTL (review-detail) in one
//               reqaprd.dspf, entered via EXFMT ALCTL from inside the
//               "5=Review" handler -- crashed with RNX1255 at EXFMT
//               ALCTL, "Session or device error occurred in file
//               REQAPRD".
//            2. The same design with the format switch deferred past
//               the driving READC loop -- crashed identically.
//            3. Splitting the detail screen into a separate called
//               program (reqapdtl) with its own device file, the same
//               pattern wrkitmr uses calling wrkcnvr/wrklotr -- also
//               crashed, cascading across both files.
//            4. The same two-subfile design again, this time with
//               OVERLAY added everywhere (a real and independently
//               confirmed DDS rule -- missing OVERLAY on a WRITE-only
//               footer really can cause exactly this class of error)
//               and separate SFLCLR/SFLDSP indicators matching ASFL's
//               own proven-safe pattern exactly -- STILL crashed at
//               the identical EXFMT ALCTL statement, confirmed via a
//               genuine native-session retest (not a stale load).
//          Every one of those is textbook-correct RPG/DDS and matches
//          patterns already used successfully elsewhere in this
//          module (or, for #3, in wrkitmr) -- yet all four crash
//          identically and reproducibly on this environment, while
//          this plain-fields design is the ONLY one confirmed to
//          render without crashing. Treat that as an environment-
//          specific limitation of this particular setup (possibly the
//          Genie web-based 5250 renderer, not the RPG/DDS itself,
//          though the exact reason remains unconfirmed), not a
//          contradiction of standard subfile practice -- and don't
//          re-attempt a second business subfile in this program
//          without a real interactive test proving it actually works
//          here first.
// Epic:    PERP-6 (PERP-35, redesigned four times as a runtime-crash
//          fix; see DDL_STYLE_GUIDE.md Sec.14 for the full history)
// ---------------------------------------------------------------------

ctl-opt dftactgrp(*no) actgrp(*new);

dcl-f reqaprd workstn sfile(asfl:rrn) sfile(rmsgsfl:msgrrn);

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

dcl-ds reqRow qualified;
  reqnbr int(20);
  reqby  varchar(10);
  needby date;
  pricd  varchar(20);
  totest packed(15:2);
  conf   varchar(8);
end-ds;

dcl-ds lineRow2 qualified;
  item varchar(25);
  qty  packed(15:4);
  uom  varchar(5);
  cost packed(15:4);
end-ds;

dcl-c MAXDTLLINES 6;

dcl-ds reqs likeds(reqRow) dim(500);
dcl-ds detailLines likeds(lineRow2) dim(500);

dcl-s numReqs   int(10);
dcl-s numLines  int(10);
dcl-s i         int(10);
dcl-s rrn       int(10);
dcl-s msgrrn    int(10);
dcl-s selRrn    int(10);
dcl-s reviewRrn int(10);
dcl-s selOpt    char(1);
dcl-s compcd    char(3);
dcl-s selReqnbr int(20);
dcl-s confPct   packed(5:2);
dcl-s confInd   int(5);

in ldaDS;
compcd = ldaDS.compcd;

if compcd = '';
  exsr clearMsgs;
  writeMsg('No company selected - run Select Company (PERPSELR) first.');
  *in40 = *on;
  write rmsgctl;
  lcompdsp = '';
  write rlfoot;
  exfmt asctl;
  *inlr = *on;
  return;
endif;

lcompdsp = compcd;

dow '1';
  exsr clearMsgs;
  exsr loadReqs;

  if numReqs = 0;
    *in30 = *off;
    write rnosub;
  else;
    exsr fillList;
    *in30 = *on;
  endif;

  write rlfoot;
  *in40 = *off;
  if msgrrn > 0;
    *in40 = *on;
  endif;
  write rmsgctl;
  exfmt asctl;

  if *in03 or *in12;
    leave;
  endif;

  if *in05;
    iter;
  endif;

  // Fully drain the READC loop before switching to the detail record --
  // matches perpselr.sqlrpgle's "collect then act" pattern for one
  // action per Enter.
  if numReqs > 0;
    selRrn    = 0;
    selOpt    = ' ';
    reviewRrn = 0;
    readc asfl;
    dow not %eof(reqaprd);
      if asopt <> '';
        selRrn = rrn;
        selOpt = asopt;
        if selOpt = '5';
          if reviewRrn = 0;
            reviewRrn = selRrn;
          else;
            writeMsg('Only one requisition may be reviewed per Enter.');
          endif;
        else;
          writeMsg('Option ' + selOpt + ' not valid - use 5.');
        endif;
        selRrn = 0;
      endif;
      readc asfl;
    enddo;

    if reviewRrn > 0 and msgrrn = 0;
      selReqnbr = reqs(reviewRrn).reqnbr;
      exsr reviewReq;
    endif;
  endif;

enddo;

*inlr = *on;
return;

// ---------------------------------------------------------------------
begsr loadReqs;
  numReqs = 0;
  exec sql declare c1 cursor for
    select requisition_number, requested_by, need_by_date, priority_code,
           total_estimated_cost, confidence_pct
      from perpdemo.requisition_header
     where company_code = :compcd and status_code = 'SUBMITTED'
     order by requisition_number;
  exec sql open c1;
  if sqlcode < 0;
    writeMsg('SQL open failed: SQLCODE=' + %char(sqlcode));
    return;
  endif;

  dow numReqs < %elem(reqs);
    exec sql fetch c1 into :reqRow.reqnbr, :reqRow.reqby, :reqRow.needby,
                           :reqRow.pricd, :reqRow.totest,
                           :confPct :confInd;
    if sqlcode = 100 or sqlcode < 0;
      leave;
    endif;
    if confInd < 0;
      reqRow.conf = 'N/A';
    else;
      reqRow.conf = %char(confPct) + '%';
    endif;
    numReqs += 1;
    reqs(numReqs) = reqRow;
  enddo;
  exec sql close c1;
endsr;

// ---------------------------------------------------------------------
begsr fillList;
  rrn = 0;
  *in31 = *on;
  write asctl;
  *in31 = *off;
  for i = 1 to numReqs;
    asopt    = '';
    asreqnbr = %char(reqs(i).reqnbr);
    asreqby  = reqs(i).reqby;
    asneedby = reqs(i).needby;
    aspricd  = reqs(i).pricd;
    astotest = reqs(i).totest;
    asconf   = reqs(i).conf;
    rrn += 1;
    write asfl;
  endfor;
endsr;

// ---------------------------------------------------------------------
begsr reviewReq;
  ddreqnbr = %char(selReqnbr);
  ddreqby  = reqs(reviewRrn).reqby;
  ddneedby = reqs(reviewRrn).needby;
  ddpricd  = reqs(reviewRrn).pricd;
  ddtotest = reqs(reviewRrn).totest;
  ddconf   = reqs(reviewRrn).conf;
  enotes2  = '';

  exsr loadLines2;
  exsr fillDetailLines;

  dow '1';
    exsr clearMsgs;
    if msgrrn > 0;
      *in40 = *on;
    else;
      *in40 = *off;
    endif;
    write rmsgctl;
    exfmt rdetail;

    if *in12;
      return;
    endif;

    if *in06;
      exec sql
        update perpdemo.requisition_header
           set status_code           = 'APPROVED',
               approved_by           = user,
               approved_at           = current_timestamp,
               approval_source_code  = 'HUMAN',
               approval_source_type  = 'APPRSRC',
               approval_notes        = :enotes2,
               updated_at            = current_timestamp,
               updated_by            = user
         where company_code = :compcd and requisition_number = :selReqnbr;
      if sqlcode < 0;
        writeMsg('Approve failed: SQLCODE=' + %char(sqlcode));
        iter;
      endif;
      writeMsg('Requisition ' + %trim(ddreqnbr) + ' approved.');
      return;
    endif;

    if *in07;
      exec sql
        update perpdemo.requisition_header
           set status_code           = 'REJECTED',
               approved_by           = user,
               approved_at           = current_timestamp,
               approval_source_code  = 'HUMAN',
               approval_source_type  = 'APPRSRC',
               approval_notes        = :enotes2,
               updated_at            = current_timestamp,
               updated_by            = user
         where company_code = :compcd and requisition_number = :selReqnbr;
      if sqlcode < 0;
        writeMsg('Reject failed: SQLCODE=' + %char(sqlcode));
        iter;
      endif;
      writeMsg('Requisition ' + %trim(ddreqnbr) + ' rejected.');
      return;
    endif;

    iter;
  enddo;
endsr;

// ---------------------------------------------------------------------
begsr loadLines2;
  numLines = 0;
  exec sql declare c2 cursor for
    select item_number, quantity, uom_code, est_unit_cost
      from perpdemo.requisition_line
     where company_code = :compcd and requisition_number = :selReqnbr
     order by line_number;
  exec sql open c2;
  if sqlcode < 0;
    writeMsg('SQL open failed: SQLCODE=' + %char(sqlcode));
    return;
  endif;

  dow numLines < %elem(detailLines);
    exec sql fetch c2 into :lineRow2;
    if sqlcode = 100 or sqlcode < 0;
      leave;
    endif;
    numLines += 1;
    detailLines(numLines) = lineRow2;
  enddo;
  exec sql close c2;
endsr;

// ---------------------------------------------------------------------
// Fills the fixed L1..L6 (item/qty/uom/cost) plain fields on RDETAIL from
// detailLines(). Each row's 4 fields are conditioned in the DDS on its
// own indicator (*in60..*in65) so an unused row is truly blank on screen
// -- not just zeroed-out numerics, which display as "0.0000" per
// EDTCDE(3) and read as confusing garbage data rather than "no line
// here." Also turns on *in34 (conditions the "more lines exist" message)
// when numLines exceeds what the screen can show.
begsr fillDetailLines;
  if numLines > MAXDTLLINES;
    *in34 = *on;
  else;
    *in34 = *off;
  endif;

  *in60 = (numLines >= 1);
  *in61 = (numLines >= 2);
  *in62 = (numLines >= 3);
  *in63 = (numLines >= 4);
  *in64 = (numLines >= 5);
  *in65 = (numLines >= 6);

  if numLines >= 1;
    l1item = detailLines(1).item;
    l1qty  = detailLines(1).qty;
    l1uom  = detailLines(1).uom;
    l1cost = detailLines(1).cost;
  endif;

  if numLines >= 2;
    l2item = detailLines(2).item;
    l2qty  = detailLines(2).qty;
    l2uom  = detailLines(2).uom;
    l2cost = detailLines(2).cost;
  endif;

  if numLines >= 3;
    l3item = detailLines(3).item;
    l3qty  = detailLines(3).qty;
    l3uom  = detailLines(3).uom;
    l3cost = detailLines(3).cost;
  endif;

  if numLines >= 4;
    l4item = detailLines(4).item;
    l4qty  = detailLines(4).qty;
    l4uom  = detailLines(4).uom;
    l4cost = detailLines(4).cost;
  endif;

  if numLines >= 5;
    l5item = detailLines(5).item;
    l5qty  = detailLines(5).qty;
    l5uom  = detailLines(5).uom;
    l5cost = detailLines(5).cost;
  endif;

  if numLines >= 6;
    l6item = detailLines(6).item;
    l6qty  = detailLines(6).qty;
    l6uom  = detailLines(6).uom;
    l6cost = detailLines(6).cost;
  endif;
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
