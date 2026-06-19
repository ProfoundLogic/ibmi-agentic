**free

/////////////////////
// Pick Batch Dashboard (EJS Screen Mode)
//
// Hornady-branded EJS clone of PICKBATR option 4.
//
// Top of screen: filterable + sortable dashboard over PICKBATHP with
// stat cards (Open / Active / Paused / Completed counts and totals).
//
// Per-row actions (icons in the leftmost grid column):
//   START    -- Open|Paused -> Active, stamps PICKSTART if first start
//   PAUSE    -- Active -> Paused
//   COMPLETE -- Active|Paused -> Completed, stamps PICKEND + PICKDUR
//   VIEW     -- loads PICKBATDP detail rows into DETSFL + sets viewBatch
//   ASSIGN   -- updates PICKER from newPicker
//   DELETE   -- SQL DELETE the batch (PICKBATHP and PICKBATDP)
// Plus modal-control actions: CLOSE_VIEW.
/////////////////////

ctl-opt dftactgrp(*no) actgrp(*new);
ctl-opt option(*srcstmt : *nodebugio);

dcl-f pickbateo workstn sfile(batsfl : rrn) handler('PROFOUNDUI(HANDLER)');

exec sql
  set option
    naming    = *sys,
    commit    = *none,
    datfmt    = *iso,
    dynusrprf = *user,
    closqlcsr = *endmod;

// Lengths must match the pickbateo.json field definitions.  Used by the
// EJS template via the regular EXFMT-of-batctl output buffer.
dcl-c DET_JSON_LEN const(3000);

dcl-ds batch_t qualified template;
  batch   zoned(8 : 0);
  picker  char(10);
  loc     char(4);
  stat    char(1);
  items   zoned(5 : 0);
  pieces  zoned(5 : 0);
  start   timestamp;
  finish  timestamp;
  dur     zoned(6 : 0);
  ismega  char(1);
  kidct   zoned(3 : 0);
end-ds;

dcl-ds batches likeds(batch_t) dim(9999);
dcl-ds row likeds(batch_t);
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

dcl-s rrn int(10);
dcl-s i int(10);

dcl-s curStatus char(1);
dcl-s curLoc    char(4);
dcl-s curFilter char(30);
dcl-s curStat   char(1);

// ---- mega-batch scratch variables ----
dcl-s sRowItems  zoned(5 : 0);
dcl-s sRowPieces zoned(5 : 0);
dcl-s kidBatch   zoned(8 : 0);
dcl-s kidPicker  char(10);
dcl-s kidLoc4    char(4);
dcl-s kidStat    char(1);
dcl-s kidItems   zoned(5 : 0);
dcl-s kidPieces  zoned(5 : 0);
dcl-s firstKid   int(5);
dcl-s buf        varchar(200);
dcl-s tok        char(10);
dcl-s tokStart   int(10);
dcl-s tokPos     int(10);
dcl-s tokLen     int(10);
dcl-s kidCount   int(10);
dcl-s sumItems   packed(7 : 0);
dcl-s sumPieces  packed(9 : 0);
dcl-s newMega    zoned(8 : 0);
dcl-s newPicker2 char(10);
dcl-s newLoc4    char(4);
dcl-s megaFlag   char(1);

dow action <> 'EXIT';

  // ---- apply per-row action against selBatch BEFORE re-loading list ----
  exsr processAction;

  // ---- reload main list with current filters ----
  exsr loadBatches;

  if numBatches = 0 and msg = '';
    msg = 'No pick batches match the current filter.';
    msgType = 'info';
  endif;

  exsr loadSubfile;

  // ---- if VIEW is in progress, viewBatch carries the batch id and
  //      DETSFL has been populated. Otherwise clear it. ----
  if action = 'VIEW' and selBatch > 0;
    viewBatch = selBatch;
    exsr loadDetail;
  elseif action = 'CLOSE_VIEW';
    viewBatch = 0;
    exsr clearDetail;
  elseif viewBatch > 0;
    // viewBatch came in from prior round; refresh details in case the
    // user clicked another action while modal was open.
    exsr loadDetail;
  else;
    exsr clearDetail;
  endif;

  // Echo filter values so the EJS UI reflects current state.
  ffilter = curFilter;
  fstatus = curStatus;
  floc    = curLoc;

  // Reset transient action / selBatch / selBatches / newPicker.
  action     = '';
  selBatch   = 0;
  selBatches = '';
  newPicker  = '';
  exfmt batctl;

  // Capture filters before next iteration. Most actions reuse the
  // existing filter; CLEAR explicitly wipes it.
  if action = 'CLEAR';
    curFilter = '';
    curStatus = '';
    curLoc    = '';
  else;
    curFilter = ffilter;
    curStatus = fstatus;
    curLoc    = floc;
  endif;

enddo;

*inlr = *on;

//////////////////////////////////////////////////////////////////////
// processAction -- apply any per-row action submitted by the EJS.
// Reads:  action, selBatch, newPicker
// Writes: msg, msgType (success|warn|error|info)
//////////////////////////////////////////////////////////////////////
begsr processAction;
  msg = '';
  msgType = '';

  select;
    // ---- SEARCH / CLEAR / EXIT are filter actions only -- no row work
    when action = 'SEARCH' or action = 'CLEAR' or action = 'EXIT' or
         action = '';
      // no-op
    when action = 'VIEW' or action = 'CLOSE_VIEW';
      // handled in mainline

    when action = 'CREATE_MEGA';
      // selBatches is a comma-separated batch list; needs at least 2
      // entries.  Helper subroutine handles parse + SQL.
      exsr doCreateMega;

    when selBatch = 0;
      msg = 'No batch selected for action.';
      msgType = 'error';

    when action = 'DISSOLVE_MEGA';
      // selBatch is the mega umbrella; release children + delete umbrella.
      exsr doDissolveMega;

    when action = 'START';
      // First-time start stamps PICKSTART; resume after Pause leaves it.
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

    when action = 'PAUSE';
      exec sql
        UPDATE PICKBATHP
           SET PICKSTAT = 'P'
         WHERE PICKBAT = :selBatch AND PICKSTAT = 'A';
      if sqlcode = 0 and sqlerrd(3) > 0;
        msg = 'Batch ' + %char(selBatch) + ' paused.';
        msgType = 'success';
      else;
        msg = 'Batch ' + %char(selBatch) +
              ' must be Active before it can be paused.';
        msgType = 'warn';
      endif;

    when action = 'COMPLETE';
      exec sql
        SELECT PICKSTART INTO :row.start
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
      else;
        msg = 'Batch ' + %char(selBatch) + ' not found.';
        msgType = 'error';
      endif;

    when action = 'ASSIGN';
      if %trim(newPicker) = '';
        msg = 'Picker code is required.';
        msgType = 'error';
      else;
        exec sql
          UPDATE PICKBATHP
             SET PICKER = :newPicker
           WHERE PICKBAT = :selBatch;
        if sqlcode = 0 and sqlerrd(3) > 0;
          msg = 'Batch ' + %char(selBatch) + ' assigned to ' +
                %trim(newPicker) + '.';
          msgType = 'success';
        else;
          msg = 'Batch ' + %char(selBatch) + ' not found.';
          msgType = 'error';
        endif;
      endif;

    when action = 'DELETE';
      exec sql
        DELETE FROM PICKBATDP WHERE PICKBAT = :selBatch;
      exec sql
        DELETE FROM PICKBATHP WHERE PICKBAT = :selBatch;
      if sqlcode = 0;
        msg = 'Batch ' + %char(selBatch) + ' deleted.';
        msgType = 'success';
      else;
        msg = 'Could not delete batch ' + %char(selBatch) + '.';
        msgType = 'error';
      endif;

    other;
      msg = 'Unknown action: ' + %trim(action);
      msgType = 'error';
  endsl;
endsr;

//////////////////////////////////////////////////////////////////////
// loadBatches -- read PICKBATHP into batches[] honouring current
// filters. Recomputes totals + per-status counts.
//////////////////////////////////////////////////////////////////////
begsr loadBatches;
  numBatches = 0;
  totBatch   = 0;
  totItems   = 0;
  totPieces  = 0;
  openCt     = 0;
  activeCt   = 0;
  pausedCt   = 0;
  doneCt     = 0;
  megaCt     = 0;

  // Exclude child batches that already roll up under a mega umbrella;
  // the mega's own PICKBATHP row stays visible and is flagged as mega.
  exec sql
    DECLARE C1 CURSOR FOR
      SELECT H.PICKBAT, H.PICKER, H.PICKINVLOC, H.PICKSTAT,
             H.PICKNUMITM, H.PICKNUMPCS,
             H.PICKSTART, H.PICKEND, H.PICKDUR,
             CASE WHEN COALESCE(KM.KIDCT, 0) > 0
                  THEN 'Y' ELSE 'N' END,
             COALESCE(KM.KIDCT, 0)
        FROM PICKBATHP H
        LEFT JOIN (SELECT PICKMBAT, COUNT(*) AS KIDCT
                     FROM PICKMBATDP GROUP BY PICKMBAT) KM
          ON KM.PICKMBAT = H.PICKBAT
       WHERE H.PICKBAT NOT IN
             (SELECT M.PICKBAT FROM PICKMBATDP M)
         AND (:curStatus = '' OR H.PICKSTAT = :curStatus)
         AND (:curLoc    = '' OR H.PICKINVLOC = :curLoc)
         AND (:curFilter = '' OR
              UPPER(H.PICKER) LIKE '%' CONCAT UPPER(RTRIM(:curFilter)) CONCAT '%' OR
              CHAR(H.PICKBAT) LIKE '%' CONCAT RTRIM(:curFilter) CONCAT '%')
       ORDER BY H.PICKSEQNO, H.PICKBAT;

  exec sql OPEN C1;

  dow numBatches < %elem(batches);
    exec sql
      FETCH FROM C1 INTO :row;
    if sqlcode <> 0;
      leave;
    endif;
    numBatches += 1;
    batches(numBatches) = row;
  enddo;

  exec sql CLOSE C1;

  totBatch = numBatches;
  for i = 1 to numBatches;
    totItems  += batches(i).items;
    totPieces += batches(i).pieces;
    if batches(i).ismega = 'Y';
      megaCt += 1;
    endif;
    select;
      when batches(i).stat = 'O';
        openCt += 1;
      when batches(i).stat = 'A';
        activeCt += 1;
      when batches(i).stat = 'P';
        pausedCt += 1;
      when batches(i).stat = 'Z';
        doneCt += 1;
    endsl;
  endfor;
endsr;

//////////////////////////////////////////////////////////////////////
// loadSubfile -- push batches[] into BATSFL records.
//////////////////////////////////////////////////////////////////////
begsr loadSubfile;
  rrn = 0;
  sflclear = '1';
  write batctl;
  sflclear = '0';
  for i = 1 to numBatches;
    sbatch    = batches(i).batch;
    spicker   = batches(i).picker;
    sloc      = batches(i).loc;
    sstat     = batches(i).stat;
    sstatdesc = statusDesc(batches(i).stat);
    sitems    = batches(i).items;
    spieces   = batches(i).pieces;
    sstart    = fmtTs(batches(i).start);
    send      = fmtTs(batches(i).finish);
    sdurmins  = batches(i).dur / 60;
    sismega   = batches(i).ismega;
    skidct    = batches(i).kidct;
    rrn += 1;
    write batsfl;
  endfor;
endsr;

//////////////////////////////////////////////////////////////////////
// loadDetail -- populate DETJSON for the View modal.  For a mega
// umbrella, also populates MEGAKIDS with its constituent batches.
//////////////////////////////////////////////////////////////////////
begsr loadDetail;
  exsr clearDetail;
  viewIsMega = 'N';

  // header info for the modal
  // Single-row lookup of the batch -- then a separate count to set ismega.
  exec sql
    SELECT PICKER, PICKSTAT INTO :viewPick, :curStat
      FROM PICKBATHP WHERE PICKBAT = :viewBatch;
  if sqlcode = 0;
    viewStat = statusDesc(curStat);
  endif;
  exec sql
    SELECT CASE WHEN COUNT(*) > 0 THEN 'Y' ELSE 'N' END
      INTO :viewIsMega
      FROM PICKMBATDP WHERE PICKMBAT = :viewBatch;

  numDet = 0;
  exec sql
    DECLARE C2 CURSOR FOR
      SELECT PICKSEQ, PICKITEM, PICKAISLE, PICKLOC,
             PICKORD, PICKNEED, PICKQTYP
        FROM PICKBATDP
       WHERE PICKBAT = :viewBatch
          OR PICKBAT IN (SELECT M.PICKBAT FROM PICKMBATDP M
                          WHERE M.PICKMBAT = :viewBatch)
       ORDER BY PICKBAT, PICKSEQ;
  exec sql OPEN C2;
  detJson = '[';
  dow numDet < 200;
    exec sql FETCH FROM C2 INTO :detRow;
    if sqlcode <> 0;
      leave;
    endif;
    if numDet > 0;
      detJson = %trimr(detJson) + ',';
    endif;
    numDet += 1;
    detJson = %trimr(detJson) +
              '{"seq":'   + %char(detRow.seq) +
              ',"item":"' + jsonEscape(detRow.item) + '"' +
              ',"aisle":"' + jsonEscape(detRow.aisle) + '"' +
              ',"loc":"' + jsonEscape(detRow.loc) + '"' +
              ',"ord":'  + %char(detRow.ord) +
              ',"need":' + %char(detRow.need) +
              ',"qtyp":' + %char(detRow.qtyp) + '}';
  enddo;
  detJson = %trimr(detJson) + ']';
  exec sql CLOSE C2;

  // If we're viewing a mega umbrella, also surface the child batches.
  megaKids = '[]';
  if viewIsMega = 'Y';
    exsr loadMegaKids;
  endif;
endsr;

//////////////////////////////////////////////////////////////////////
// loadMegaKids -- read PICKBATHP rows linked to viewBatch through
// PICKMBATDP and serialise them into megaKids as a JSON array.
//////////////////////////////////////////////////////////////////////
begsr loadMegaKids;
  megaKids = '[';
  exec sql
    DECLARE C3 CURSOR FOR
      SELECT H.PICKBAT, H.PICKER, H.PICKINVLOC,
             H.PICKSTAT, H.PICKNUMITM, H.PICKNUMPCS
        FROM PICKBATHP H
        JOIN PICKMBATDP M ON M.PICKBAT = H.PICKBAT
       WHERE M.PICKMBAT = :viewBatch
       ORDER BY H.PICKBAT;
  exec sql OPEN C3;
  firstKid = 1;
  dou sqlcode <> 0;
    exec sql FETCH FROM C3
      INTO :kidBatch, :kidPicker, :kidLoc4, :kidStat,
           :kidItems, :kidPieces;
    if sqlcode <> 0;
      leave;
    endif;
    if firstKid = 0;
      megaKids = %trimr(megaKids) + ',';
    endif;
    firstKid = 0;
    megaKids = %trimr(megaKids) +
               '{"batch":' + %char(kidBatch) +
               ',"picker":"' + jsonEscape(kidPicker) + '"' +
               ',"loc":"'  + jsonEscape(kidLoc4)    + '"' +
               ',"stat":"' + %trimr(statusDesc(kidStat)) + '"' +
               ',"items":' + %char(kidItems) +
               ',"pieces":' + %char(kidPieces) + '}';
  enddo;
  exec sql CLOSE C3;
  megaKids = %trimr(megaKids) + ']';
endsr;

//////////////////////////////////////////////////////////////////////
// clearDetail -- empty the detail payload.
//////////////////////////////////////////////////////////////////////
begsr clearDetail;
  detJson  = '[]';
  megaKids = '[]';
  viewPick = '';
  viewStat = '';
  viewIsMega = 'N';
endsr;

//////////////////////////////////////////////////////////////////////
// doCreateMega -- parse selBatches (comma-separated batch numbers)
// and bundle them into a new umbrella batch.  The umbrella gets a
// PICKBATHP row of its own with PICKSTAT='O', summed items/pieces,
// and the picker of the first selected batch.  Children get linked
// via PICKMBATDP.
//////////////////////////////////////////////////////////////////////
begsr doCreateMega;
  buf = %trim(selBatches);
  if buf = '';
    msg = 'Select at least two batches to create a mega.';
    msgType = 'error';
    leavesr;
  endif;

  kidCount   = 0;
  sumItems   = 0;
  sumPieces  = 0;
  newPicker2 = '';
  newLoc4    = '';

  // Allocate the next mega number above the existing max; start at
  // 9001 if the table is small enough.
  newMega = 0;
  exec sql
    SELECT COALESCE(GREATEST(MAX(PICKBAT) + 1, 9001), 9001)
      INTO :newMega
      FROM PICKBATHP;
  if newMega = 0;
    newMega = 9001;
  endif;

  // Parse comma-separated list of batch numbers from selBatches.
  tokStart = 1;
  dow tokStart <= %len(buf);
    tokPos = %scan(',' : buf : tokStart);
    if tokPos = 0;
      tokLen = %len(buf) - tokStart + 1;
    else;
      tokLen = tokPos - tokStart;
    endif;
    if tokLen > 0;
      tok = %subst(buf : tokStart : %min(tokLen : 10));
      kidBatch = numericOrZero(tok);
      if kidBatch > 0;
        // Look up child + capture umbrella picker / loc from the first.
        exec sql
          SELECT PICKER, PICKINVLOC, PICKNUMITM, PICKNUMPCS
            INTO :newPicker2, :newLoc4, :sRowItems, :sRowPieces
            FROM PICKBATHP WHERE PICKBAT = :kidBatch;
        if sqlcode = 0;
          sumItems  += sRowItems;
          sumPieces += sRowPieces;
          if kidCount = 0;
            exec sql
              INSERT INTO PICKBATHP
                (PICKBAT, PICKSEQNO, PICKER, PICKINVLOC, PICKSTAT,
                 PICKNUMITM, PICKNUMPCS, PICKCRTUSR, PICKCRTTZ)
              VALUES (:newMega, :newMega, :newPicker2, :newLoc4, 'O',
                      0, 0, 'AIDEMO', CURRENT_TIMESTAMP);
          endif;
          exec sql
            INSERT INTO PICKMBATDP (PICKMBAT, PICKBAT)
              VALUES (:newMega, :kidBatch);
          if sqlcode = 0;
            kidCount += 1;
          endif;
        endif;
      endif;
    endif;
    if tokPos = 0;
      leave;
    endif;
    tokStart = tokPos + 1;
  enddo;

  if kidCount < 2;
    exec sql DELETE FROM PICKMBATDP WHERE PICKMBAT = :newMega;
    exec sql DELETE FROM PICKBATHP  WHERE PICKBAT  = :newMega;
    msg = 'Select at least two batches to create a mega.';
    msgType = 'error';
    leavesr;
  endif;

  exec sql
    UPDATE PICKBATHP
       SET PICKNUMITM = :sumItems,
           PICKNUMPCS = :sumPieces
     WHERE PICKBAT = :newMega;

  msg = 'Mega batch ' + %char(newMega) + ' created from ' +
        %char(kidCount) + ' batches.';
  msgType = 'success';
endsr;

//////////////////////////////////////////////////////////////////////
// doDissolveMega -- selBatch IS the mega umbrella PICKBAT.  Children
// are released back to the regular grid and the umbrella row is
// removed.  No data is lost from PICKBATDP since children own their
// own detail rows.
//////////////////////////////////////////////////////////////////////
begsr doDissolveMega;
  megaFlag = 'N';
  exec sql
    SELECT CASE WHEN COUNT(*) > 0 THEN 'Y' ELSE 'N' END
      INTO :megaFlag
      FROM PICKMBATDP WHERE PICKMBAT = :selBatch;
  if megaFlag <> 'Y';
    msg = 'Batch ' + %char(selBatch) + ' is not a mega batch.';
    msgType = 'error';
    leavesr;
  endif;
  exec sql DELETE FROM PICKMBATDP WHERE PICKMBAT = :selBatch;
  exec sql DELETE FROM PICKBATHP  WHERE PICKBAT  = :selBatch;
  msg = 'Mega batch ' + %char(selBatch) + ' dissolved.';
  msgType = 'success';
  if viewBatch = selBatch;
    viewBatch = 0;
  endif;
endsr;

//////////////////////////////////////////////////////////////////////
// jsonEscape -- trim trailing blanks. Detail values are internal
// SKU / aisle / location codes that never contain "/\ control chars,
// so a plain trim suffices.  Kept as a procedure so the call sites
// document intent.
//////////////////////////////////////////////////////////////////////
dcl-proc jsonEscape;
  dcl-pi *n varchar(64);
    val char(64) const;
  end-pi;
  return %trimr(val);
end-proc;

//////////////////////////////////////////////////////////////////////
// numericOrZero -- parse a trimmed digit string to a zoned(8) batch
// number.  Returns 0 if the input is empty or has any non-digit
// characters (defensive: comma-list parser may pass partial tokens).
//////////////////////////////////////////////////////////////////////
dcl-proc numericOrZero;
  dcl-pi *n zoned(8 : 0);
    val char(10) const;
  end-pi;
  dcl-s s varchar(10);
  s = %trim(val);
  if s = '' or %check('0123456789' : s) > 0;
    return 0;
  endif;
  monitor;
    return %dec(s : 8 : 0);
  on-error;
    return 0;
  endmon;
end-proc;

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
// fmtTs -- format an IBM i timestamp as YYYY-MM-DD HH:MM:SS,
// returning '' for *LOVAL so the dashboard shows '-'.
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
