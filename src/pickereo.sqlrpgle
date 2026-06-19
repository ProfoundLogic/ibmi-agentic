**free

/////////////////////
// Picker Workflow (EJS Screen Mode)
//
// Hornady-branded EJS clone of PICKERR option 5. Mobile-style picker
// workflow with three phases driven by the `phase` field:
//
//   L  Login         -- pick a picker code + warehouse
//   B  My Batches    -- batch cards filtered to the logged-in picker
//   D  Batch Detail  -- pick lines with simulated scan (increments
//                       PICKBATDP.PICKQTYP toward PICKNEED)
//
// Actions:
//   LOGIN     -- empcode + warehouse -> phase B
//   LOGOUT    -- back to phase L
//   OPEN      -- selBatch -> phase D, load detLines JSON
//   BACK      -- phase D -> phase B
//   START     -- Open|Paused -> Active for selBatch (stamps PICKSTART)
//   COMPLETE -- Active|Paused -> Completed, stamps PICKEND + PICKDUR
//   SCAN      -- adds :qty pieces toward PICKBATDP.PICKQTYP for
//                (selBatch, selSeq), capped at PICKNEED
//   SCANFULL  -- shortcut: set PICKQTYP = PICKNEED for that line
//   EXIT      -- leave the program
/////////////////////

ctl-opt dftactgrp(*no) actgrp(*new);
ctl-opt option(*srcstmt : *nodebugio);

dcl-f pickereo workstn sfile(batsfl : rrn) handler('PROFOUNDUI(HANDLER)');

// Non-journaled mutations -- without this the SCAN updates roll back
// when the program exits. (see memory sqlrpgle-commit-default)
exec sql
  set option
    naming    = *sys,
    commit    = *none,
    datfmt    = *iso,
    dynusrprf = *user,
    closqlcsr = *endmod;

dcl-ds batch_t qualified template;
  batch   zoned(8 : 0);
  loc     char(4);
  stat    char(1);
  items   zoned(5 : 0);
  pieces  zoned(5 : 0);
  picked  zoned(5 : 0);
  start   timestamp;
  ismega  char(1);
  kidct   zoned(3 : 0);
end-ds;

dcl-ds batches likeds(batch_t) dim(9999);
dcl-ds bRow likeds(batch_t);
dcl-s numBatches int(10);

dcl-ds det_t qualified template;
  seq   zoned(3 : 0);
  item  char(15);
  aisle char(4);
  loc   char(8);
  ord   zoned(8 : 0);
  need  zoned(7 : 0);
  qtyp  zoned(7 : 0);
end-ds;

dcl-ds detRow likeds(det_t);
dcl-s numDet int(10);

// scratch fields for mega-aware loadDetail (extra columns fetched out
// of the join with PICKMBATDP -- tote ID and child batch number)
dcl-s detRowChild   zoned(8 : 0);
dcl-s detRowTotePos zoned(3 : 0);
dcl-s toteStr       char(5);
// warehouse coordinate (joined from WHLOC, 0 if no row matched)
dcl-s detRowWhRow   zoned(3 : 0);
dcl-s detRowWhBay   zoned(3 : 0);
dcl-s detRowWhShelf zoned(2 : 0);

dcl-s rrn  int(10);
dcl-s i    int(10);

dcl-s curEmp char(10);
dcl-s curWh  char(4);
dcl-s curStat char(1);
dcl-s curStart timestamp;
dcl-s scanTarget zoned(8 : 0);

// Start on the login phase.
phase = 'L';

dow action <> 'EXIT';

  exsr processAction;

  // Default phase if not set / cleared.
  if phase = '' or phase = ' ';
    phase = 'L';
  endif;

  // Load data appropriate to the current phase BEFORE EXFMT.
  select;
    when phase = 'L';
      exsr clearBatches;
    when phase = 'B';
      exsr loadMyBatches;
    when phase = 'D';
      exsr clearBatches;
      exsr loadDetail;
  endsl;

  empname = nameFor(curEmp);

  // Reset transient fields for the next round.
  action   = '';
  selBatch = 0;
  selChild = 0;
  selSeq   = 0;
  qty      = 0;

  exfmt wfctl;

enddo;

*inlr = *on;

//////////////////////////////////////////////////////////////////////
// processAction -- branch on action / phase combinations.
//////////////////////////////////////////////////////////////////////
begsr processAction;
  msg = '';
  msgType = '';

  select;
    when action = '' or action = 'EXIT';
      // no-op
    when action = 'LOGIN';
      if %trim(empcode) = '';
        msg = 'Picker code is required.';
        msgType = 'error';
        phase = 'L';
      else;
        curEmp = %xlate('abcdefghijklmnopqrstuvwxyz' :
                        'ABCDEFGHIJKLMNOPQRSTUVWXYZ' : %trim(empcode));
        empcode = curEmp;
        if %trim(warehouse) = '';
          warehouse = 'WEST';
        endif;
        curWh = warehouse;
        phase = 'B';
        msg = 'Signed in as ' + %trim(curEmp) + ' (' +
              %trim(nameFor(curEmp)) + ') at ' + %trim(curWh) + '.';
        msgType = 'success';
      endif;

    when action = 'LOGOUT';
      curEmp  = '';
      empcode = '';
      curWh   = '';
      warehouse = '';
      phase = 'L';
      msg = 'Signed out.';
      msgType = 'info';

    when action = 'OPEN' and selBatch > 0;
      detBatch = selBatch;
      phase = 'D';

    when action = 'BACK';
      detBatch = 0;
      phase = 'B';

    when action = 'START' and selBatch > 0;
      exec sql
        SELECT PICKSTAT INTO :curStat
          FROM PICKBATHP WHERE PICKBAT = :selBatch;
      if sqlcode = 0;
        if curStat = 'O';
          exec sql
            UPDATE PICKBATHP
               SET PICKSTAT = 'A',
                   PICKSTART = CURRENT_TIMESTAMP
             WHERE PICKBAT = :selBatch;
        else;
          exec sql
            UPDATE PICKBATHP
               SET PICKSTAT = 'A'
             WHERE PICKBAT = :selBatch;
        endif;
        msg = 'Batch ' + %char(selBatch) + ' started.';
        msgType = 'success';
      else;
        msg = 'Batch ' + %char(selBatch) + ' not found.';
        msgType = 'error';
      endif;

    when action = 'COMPLETE' and selBatch > 0;
      exec sql
        SELECT PICKSTART INTO :curStart
          FROM PICKBATHP WHERE PICKBAT = :selBatch;
      if sqlcode = 0;
        exec sql
          UPDATE PICKBATHP
             SET PICKSTAT  = 'Z',
                 PICKEND   = CURRENT_TIMESTAMP,
                 PICKDUR   = COALESCE(BIGINT(
                              TIMESTAMPDIFF(2, CHAR(CURRENT_TIMESTAMP -
                                                    PICKSTART))), 0)
           WHERE PICKBAT = :selBatch;
        msg = 'Batch ' + %char(selBatch) + ' completed.';
        msgType = 'success';
        // After completing, drop back to the batch list.
        phase = 'B';
        detBatch = 0;
      endif;

    when (action = 'SCAN' or action = 'SCANFULL') and
         selSeq > 0 and (selChild > 0 or selBatch > 0);
      // In a mega context, selChild carries the constituent batch
      // number (the actual PICKBATDP owner).  Otherwise the line is
      // a regular-batch pick and selBatch IS the owner.
      if selChild > 0;
        scanTarget = selChild;
      else;
        scanTarget = selBatch;
      endif;

      // Auto-start the target child batch on first scan.
      exec sql
        SELECT PICKSTAT INTO :curStat
          FROM PICKBATHP WHERE PICKBAT = :scanTarget;
      if sqlcode = 0 and curStat = 'O';
        exec sql
          UPDATE PICKBATHP
             SET PICKSTAT = 'A',
                 PICKSTART = CURRENT_TIMESTAMP
           WHERE PICKBAT = :scanTarget;
      endif;

      if action = 'SCANFULL';
        exec sql
          UPDATE PICKBATDP
             SET PICKQTYP = PICKNEED
           WHERE PICKBAT = :scanTarget AND PICKSEQ = :selSeq;
      else;
        if qty <= 0;
          qty = 1;
        endif;
        exec sql
          UPDATE PICKBATDP
             SET PICKQTYP = LEAST(PICKQTYP + :qty, PICKNEED)
           WHERE PICKBAT = :scanTarget AND PICKSEQ = :selSeq;
      endif;
      if sqlcode = 0 and sqlerrd(3) > 0;
        msg = 'Scanned line ' + %char(selSeq) + ' of batch ' +
              %char(scanTarget) + '.';
        msgType = 'success';
      else;
        msg = 'Scan failed for batch ' + %char(scanTarget) +
              ' line ' + %char(selSeq) + '.';
        msgType = 'error';
      endif;

    other;
      // fall through silently for unrecognized non-empty actions
  endsl;
endsr;

//////////////////////////////////////////////////////////////////////
// loadMyBatches -- batch list filtered to the logged-in picker.
//////////////////////////////////////////////////////////////////////
begsr loadMyBatches;
  numBatches = 0;
  myBatches  = 0;
  myTotItm   = 0;
  myTotPcs   = 0;
  myOpenCt   = 0;
  myActCt    = 0;
  myPauCt    = 0;
  myDoneCt   = 0;

  // Filter children of any mega umbrella out of the regular list.  When
  // a mega exists, the umbrella row stands in for its children -- the
  // picker should see the consolidated bundle, not the constituents.
  // Picked = sum of PICKQTYP across the batch's own lines AND (when this
  // row is a mega umbrella) all its children's lines.
  exec sql
    DECLARE C_MY CURSOR FOR
      SELECT H.PICKBAT, H.PICKINVLOC, H.PICKSTAT,
             H.PICKNUMITM, H.PICKNUMPCS,
             COALESCE((SELECT SUM(D.PICKQTYP) FROM PICKBATDP D
                        WHERE D.PICKBAT = H.PICKBAT
                           OR D.PICKBAT IN
                              (SELECT M.PICKBAT FROM PICKMBATDP M
                                WHERE M.PICKMBAT = H.PICKBAT)),
                       0) AS PICKED,
             H.PICKSTART,
             CASE WHEN COALESCE(MK.KIDCT, 0) > 0 THEN 'Y' ELSE 'N' END,
             COALESCE(MK.KIDCT, 0)
        FROM PICKBATHP H
        LEFT JOIN (SELECT PICKMBAT, COUNT(*) AS KIDCT
                     FROM PICKMBATDP GROUP BY PICKMBAT) MK
          ON MK.PICKMBAT = H.PICKBAT
       WHERE H.PICKBAT NOT IN (SELECT M2.PICKBAT FROM PICKMBATDP M2)
         AND H.PICKER = :curEmp
         AND (:curWh = '' OR H.PICKINVLOC = :curWh)
       ORDER BY H.PICKSTAT, H.PICKSEQNO, H.PICKBAT;

  exec sql OPEN C_MY;

  dow numBatches < %elem(batches);
    exec sql FETCH FROM C_MY INTO :bRow;
    if sqlcode <> 0;
      leave;
    endif;
    numBatches += 1;
    batches(numBatches) = bRow;
  enddo;

  exec sql CLOSE C_MY;

  myBatches = numBatches;
  for i = 1 to numBatches;
    myTotItm += batches(i).items;
    myTotPcs += batches(i).pieces;
    select;
      when batches(i).stat = 'O';
        myOpenCt += 1;
      when batches(i).stat = 'A';
        myActCt  += 1;
      when batches(i).stat = 'P';
        myPauCt  += 1;
      when batches(i).stat = 'Z';
        myDoneCt += 1;
    endsl;
  endfor;

  exsr loadSubfile;
endsr;

//////////////////////////////////////////////////////////////////////
// loadSubfile -- write batches[] to BATSFL.
//////////////////////////////////////////////////////////////////////
begsr loadSubfile;
  rrn = 0;
  sflclear = '1';
  write wfctl;
  sflclear = '0';
  for i = 1 to numBatches;
    sbatch    = batches(i).batch;
    sloc      = batches(i).loc;
    sstat     = batches(i).stat;
    sstatdesc = statusDesc(batches(i).stat);
    sitems    = batches(i).items;
    spieces   = batches(i).pieces;
    spicked   = batches(i).picked;
    sstart    = fmtTs(batches(i).start);
    sismega   = batches(i).ismega;
    skidct    = batches(i).kidct;
    rrn += 1;
    write batsfl;
  endfor;
endsr;

//////////////////////////////////////////////////////////////////////
// clearBatches -- empty BATSFL (used when leaving phase B).
//////////////////////////////////////////////////////////////////////
begsr clearBatches;
  rrn = 0;
  sflclear = '1';
  write wfctl;
  sflclear = '0';
  numBatches = 0;
  myBatches  = 0;
endsr;

//////////////////////////////////////////////////////////////////////
// loadDetail -- pick lines for the selected batch, plus a small set
// of header fields (status, location, totals).
//
// Mega awareness: when detBatch is a mega umbrella, the cursor walks
// the children's PICKBATDP rows in (child, seq) order and assigns a
// tote ID per child (T01..T0N where N is the child's ordinal in the
// mega, ordered by PICKBAT).  Regular batches get tote "T01" and
// "child" = the batch itself.
//////////////////////////////////////////////////////////////////////
begsr loadDetail;
  detStat   = '';
  detLoc    = '';
  detItems  = 0;
  detPieces = 0;
  detIsMega = 'N';
  detKidCt  = 0;

  // Header lookup including mega flag and kid count.
  exec sql
    SELECT H.PICKSTAT, H.PICKINVLOC, H.PICKNUMITM, H.PICKNUMPCS,
           CASE WHEN COALESCE(MK.KIDCT, 0) > 0 THEN 'Y' ELSE 'N' END,
           COALESCE(MK.KIDCT, 0)
      INTO :curStat, :detLoc, :detItems, :detPieces,
           :detIsMega, :detKidCt
      FROM PICKBATHP H
      LEFT JOIN (SELECT PICKMBAT, COUNT(*) AS KIDCT
                   FROM PICKMBATDP GROUP BY PICKMBAT) MK
        ON MK.PICKMBAT = H.PICKBAT
     WHERE H.PICKBAT = :detBatch;
  if sqlcode = 0;
    detStat = statusDesc(curStat);
  endif;

  // Detail cursor:
  //   - Regular batch -> just that batch's PICKBATDP rows, tote = T01
  //   - Mega umbrella  -> children's PICKBATDP rows ordered by child
  //                      then PICKSEQ, with tote = 'T0' + child position
  //                      (1..N).
  numDet = 0;
  exec sql
    DECLARE C_DET CURSOR FOR
      SELECT D.PICKSEQ, D.PICKITEM, D.PICKAISLE, D.PICKLOC,
             D.PICKORD, D.PICKNEED, D.PICKQTYP,
             D.PICKBAT                AS CHILD_BAT,
             COALESCE(K.POS, 1)       AS TOTE_POS,
             COALESCE(W.WHROW,   0)   AS WH_ROW,
             COALESCE(W.WHBAY,   0)   AS WH_BAY,
             COALESCE(W.WHSHELF, 0)   AS WH_SHELF
        FROM PICKBATDP D
        LEFT JOIN (SELECT PICKMBAT, PICKBAT,
                          ROW_NUMBER() OVER (PARTITION BY PICKMBAT
                                             ORDER BY PICKBAT) AS POS
                     FROM PICKMBATDP) K
          ON K.PICKMBAT = :detBatch AND K.PICKBAT = D.PICKBAT
        LEFT JOIN PICKBATHP H
          ON H.PICKBAT = D.PICKBAT
        LEFT JOIN WHLOC W
          ON W.WHITEM = D.PICKITEM AND W.WHDC = H.PICKINVLOC
       WHERE D.PICKBAT = :detBatch
          OR D.PICKBAT IN (SELECT M.PICKBAT FROM PICKMBATDP M
                            WHERE M.PICKMBAT = :detBatch)
       ORDER BY COALESCE(K.POS, 0), D.PICKBAT, D.PICKSEQ;
  exec sql OPEN C_DET;

  detLines = '[';
  dow numDet < 500;
    exec sql FETCH FROM C_DET
      INTO :detRow, :detRowChild, :detRowTotePos,
           :detRowWhRow, :detRowWhBay, :detRowWhShelf;
    if sqlcode <> 0;
      leave;
    endif;
    if numDet > 0;
      detLines = %trimr(detLines) + ',';
    endif;
    numDet += 1;
    // Format tote as "T01", "T02", ...
    if detRowTotePos < 10;
      toteStr = 'T0' + %char(detRowTotePos);
    else;
      toteStr = 'T' + %char(detRowTotePos);
    endif;
    detLines = %trimr(detLines) +
               '{"seq":'   + %char(detRow.seq) +
               ',"item":"' + %trimr(detRow.item) + '"' +
               ',"aisle":"' + %trimr(detRow.aisle) + '"' +
               ',"loc":"' + %trimr(detRow.loc) + '"' +
               ',"ord":'  + %char(detRow.ord) +
               ',"need":' + %char(detRow.need) +
               ',"qtyp":' + %char(detRow.qtyp) +
               ',"child":' + %char(detRowChild) +
               ',"tote":"' + %trimr(toteStr) + '"' +
               ',"whRow":' + %char(detRowWhRow) +
               ',"whBay":' + %char(detRowWhBay) +
               ',"whShelf":' + %char(detRowWhShelf) + '}';
  enddo;
  detLines = %trimr(detLines) + ']';
  exec sql CLOSE C_DET;
endsr;

//////////////////////////////////////////////////////////////////////
// statusDesc -- single-char status code -> human-readable label.
//////////////////////////////////////////////////////////////////////
dcl-proc statusDesc;
  dcl-pi *n char(12);
    stat char(1) const;
  end-pi;
  select;
    when stat = 'O';
      return 'Open';
    when stat = 'A';
      return 'Active';
    when stat = 'P';
      return 'Paused';
    when stat = 'Z';
      return 'Completed';
    other;
      return stat;
  endsl;
end-proc;

//////////////////////////////////////////////////////////////////////
// nameFor -- friendly display name for the seeded demo pickers.
//////////////////////////////////////////////////////////////////////
dcl-proc nameFor;
  dcl-pi *n char(30);
    code char(10) const;
  end-pi;
  dcl-s c char(10);
  c = code;
  select;
    when c = 'PICKER01';
      return 'John Smith';
    when c = 'PICKER02';
      return 'Jane Doe';
    when c = 'PICKER03';
      return 'Bob Jones';
    when c = 'PICKER04';
      return 'Sarah Wilson';
    when c = 'PICKER05';
      return 'Mike Brown';
    when c = 'PICKER06';
      return 'Lisa Garcia';
    when c = 'PICKER07';
      return 'Tom Anderson';
    when c = 'PICKER08';
      return 'Emily Taylor';
    other;
      return %trimr(c);
  endsl;
end-proc;

//////////////////////////////////////////////////////////////////////
// fmtTs -- format an IBM i timestamp as YYYY-MM-DD HH:MM:SS,
// returning '' for *LOVAL.
//////////////////////////////////////////////////////////////////////
dcl-proc fmtTs;
  dcl-pi *n char(19);
    ts timestamp const;
  end-pi;
  dcl-s s char(26);
  if ts = z'0001-01-01-00.00.00.000000';
    return '';
  endif;
  s = %char(ts : *iso);
  return %subst(s:1:10) + ' ' +
         %subst(s:12:2) + ':' + %subst(s:15:2) + ':' + %subst(s:18:2);
end-proc;
