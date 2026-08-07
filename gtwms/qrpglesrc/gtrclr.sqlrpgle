**free

//////////////////////////////////////////////////////////////////////////
// GTRCLR -- Receiving, the line list.
//
// "Show all the items in that receipt for us to confirm numbers or adjust
// numbers down." This is the screen that has to be perfect, because subfiles
// are where mobile RPG demos usually fall apart.
//
// SCAN-TO-CONFIRM IS THE PRIMARY INTERACTION. The operator does not tap; they
// scan each carton and the matching line ticks up. Tapping is the exception
// path -- an inline quantity box on every card, so adjusting down never needs
// another screen.
//
// A scanned carton is matched against the receipt's own lines, by SKU, and
// GTBAR resolves any barcode level to a SKU: a consumer UPC, a case ITF-14, or
// a GS1-128 with the GTIN in AI 01. Case codes increment by the case pack,
// eaches by one, which is what the barcode actually means.
//
// POSTING IS ONE UNIT OF WORK (design doc §7.3). Header status, every line's
// received quantity and status, one GTMOVEMENT row per line, and the inventory
// balance upsert into the receiving staging location, all under commitment
// control. Any failure rolls the whole receipt back -- nothing half-received.
//
// NOT YET BUILT, and deliberately so: the per-line detail screen (reason-code
// picker and damage photographs) and the separate confirm screen with driver
// signature. Both are additive -- see the design doc §7.2.
//////////////////////////////////////////////////////////////////////////

ctl-opt dftactgrp(*no) actgrp(*new);
ctl-opt bnddir('GTBAR');

dcl-f gtrcld workstn sfile(linsfl : rrn) handler('PROFOUNDUI(HANDLER)');

exec sql set option commit = *chg, closqlcsr = *endmod;

/copy gtbar_pr.rpgle

dcl-pi *n;
  inRcpt char(10) const;
  inLang char(2);
end-pi;

dcl-ds r likeds(gtbar_result_t);

dcl-c MAX_ROWS 60;

// Where a receipt lands before putaway. Putaway moves it onward; until then
// the stock is real, it is on site, and it is findable -- which is the whole
// point of receiving it into a staging location rather than nowhere.
dcl-c STAGE_LOC 'STAGE01';

dcl-s rrn      int(10) inz(0);
dcl-s i        int(10);
dcl-s done     ind inz(*off);
dcl-s payload  varchar(120);
dcl-s posted   ind inz(*off);
dcl-s bumpQty  packed(11 : 2);
dcl-s hitIdx   int(10);

// The receipt's lines, held between round trips. Holding them means a scan
// can be matched without re-querying, and the subfile rebuild after a change
// reads the database once rather than once per row.
dcl-ds lines qualified dim(MAX_ROWS);
  lineNo  int(10);
  sku     char(15);
  desc    varchar(60);
  dept    char(4);
  uom     char(3);
  casePk  int(10);
  img     int(10);
  qtyExp  packed(11 : 2);
  qtyRcv  packed(11 : 2);
  stat    char(4);
  reason  char(4);
  photos  int(10);
end-ds;
dcl-s lineCount int(10) inz(0);

// Work fields -- RNF0724 forbids dcl-s inside a begsr here.
dcl-s wLine   int(10);
dcl-s wSku    char(15);
dcl-s wDesc   varchar(60);
dcl-s wDescEn varchar(60);
dcl-s wDescFr varchar(60);
dcl-s wDept  char(4);
dcl-s wUom   char(3);
dcl-s wCase  int(10);
dcl-s wImg   int(10);
dcl-s wExp   packed(11 : 2);
dcl-s wRcv   packed(11 : 2);
dcl-s wVar   packed(11 : 2);
dcl-s wStat  char(4);
dcl-s wRsn   char(4);
dcl-s wPho   int(10);
dcl-s wNewSt char(4);
dcl-s wQty   packed(11 : 2);

// The audit columns are VARCHAR(18) and opername is char(40), so the operator
// ID is what gets stamped -- which is the right value anyway.
dcl-s wOper  varchar(18);

// Parser state for the QTYEDITS payload, and the barcode stamped on a line.
// wBar is separate from `payload` so a typed change does not inherit whatever
// was last scanned.
dcl-s wRest  varchar(1024);
dcl-s wTok   varchar(32);
dcl-s wIdx   int(10);
dcl-s wPos   int(10);
dcl-s wCount int(10);
dcl-s wBar   varchar(20);

//------------------------------------------------------------------------
// Main
//------------------------------------------------------------------------
langpref = inLang;
hrcpt    = inRcpt;

exsr loadOperator;
exsr loadHeader;

dow not done;
  exsr loadLineSubfile;
  action   = *blanks;
  scanval  = *blanks;
  qtyedits = *blanks;
  exfmt rcvlines;
  msg     = *blanks;
  msgkind = *blanks;
  hitline = 0;

  select;
    when action = 'EXIT';
      done = *on;

    when action = 'LANGEN';
      langpref = 'EN';
      inLang   = 'EN';
      exsr saveLanguage;

    when action = 'LANGFR';
      langpref = 'FR';
      inLang   = 'FR';
      exsr saveLanguage;

    when action = 'SCAN';
      exsr doScan;

    when action = 'APPLY';
      // The operator typed quantities into the cards and pressed Apply.
      exsr applyTyped;

    when action = 'CONFALL';
      exsr confirmAll;

    when action = 'POST';
      exsr postReceipt;
      if posted;
        done = *on;
      endif;

    when action = *blanks;
      // Enter with nothing pressed. Redisplay.

    other;
  endsl;
enddo;

*inlr = *on;
return;

//------------------------------------------------------------------------
// A carton was scanned. Resolve it to a SKU, find that SKU on THIS receipt,
// and tick the line up by what the barcode actually represents.
//------------------------------------------------------------------------
begsr doScan;
  payload = %trim(scanval);

  if payload = *blanks;
    exsr msgNothingScanned;
    leavesr;
  endif;

  if hstat = 'POST';
    exsr msgAlreadyPosted;
    leavesr;
  endif;

  clear r;
  gtbar_parse(payload : r);

  if r.sku = *blanks;
    exsr msgUnknownBarcode;
    leavesr;
  endif;

  hitIdx = 0;
  for i = 1 to lineCount;
    if lines(i).sku = r.sku;
      hitIdx = i;
      leave;
    endif;
  endfor;

  if hitIdx = 0;
    // On the receipt's paperwork this item does not exist. Overage handling
    // (add as unexpected, or reject) is the detail screen's job; for now say
    // so plainly rather than silently doing nothing.
    exsr msgNotOnReceipt;
    leavesr;
  endif;

  // A case barcode means a case: increment by the case pack, not by one.
  // This is the difference between a receiver trusting the app and not.
  if r.barLevel = 'CS' and lines(hitIdx).casePk > 0;
    bumpQty = lines(hitIdx).casePk;
  elseif r.qtyCount > 0;
    // A GS1-128 carrying AI 30/37 states its own quantity.
    bumpQty = r.qtyCount;
  else;
    bumpQty = 1;
  endif;

  //  %subst past the end of a varchar is RNX0100, so the length is checked
  //  rather than assumed: a UPC-A payload is 12 characters, not 20.
  if %len(payload) > 20;
    wBar = %subst(payload : 1 : 20);
  else;
    wBar = payload;
  endif;
  wQty = lines(hitIdx).qtyRcv + bumpQty;
  exsr writeLineQty;

  // Tell the screen which line moved so it can flash and scroll to it.
  hitline = lines(hitIdx).lineNo;
  exsr msgScanned;
endsr;

//------------------------------------------------------------------------
// Quantities typed into the cards. Only lines whose box differs from the
// stored value are written, so pressing Apply with nothing changed is free
// and does not stamp updated_by across the whole receipt.
//------------------------------------------------------------------------
begsr applyTyped;
  if hstat = 'POST';
    exsr msgAlreadyPosted;
    leavesr;
  endif;

  wCount = 0;
  wBar   = *blanks;
  wRest  = %trim(qtyedits);

  //  "seq:qty;seq:qty;" -- one entry per row the browser saw change.
  //
  //  NOT a READC. In an EJS screen the subfile rows are rendered by the
  //  TEMPLATE rather than by Profound UI grid widgets, so every quantity box
  //  carries the same field name with no record number attached and the
  //  runtime has nothing to build its {SUBFILE}.rrn changed-record marker
  //  from. READC therefore returned nothing however the value was changed,
  //  and the screen reported "no quantities were changed" every time.
  dow wRest <> *blanks;
    wPos = %scan(';' : wRest);
    if wPos > 0;
      if wPos > 1;
        wTok = %subst(wRest : 1 : wPos - 1);
      else;
        wTok = *blanks;
      endif;
      //  The payload ends with a separator, so the last token leaves the
      //  scan position ON the final character. Advancing past it is
      //  RNX0100 "length or start position out of range" -- which arrives as
      //  an inquiry message on the operator's screen, not as an error the
      //  program can report.
      if wPos >= %len(wRest);
        wRest = *blanks;
      else;
        wRest = %subst(wRest : wPos + 1);
      endif;
    else;
      wTok  = wRest;
      wRest = *blanks;
    endif;

    if %trim(wTok) <> *blanks;
      wPos = %scan(':' : wTok);
      if wPos > 1 and wPos < %len(%trim(wTok));
        //  A malformed entry must not take the screen down with RNX0105.
        //  Skip it and carry on: the operator's other edits still land.
        monitor;
          wIdx = %int(%subst(wTok : 1 : wPos - 1));
          wQty = %dec(%subst(%trim(wTok) : wPos + 1) : 11 : 2);

          if wIdx >= 1 and wIdx <= lineCount and wQty <> lines(wIdx).qtyRcv;
            hitIdx = wIdx;
            exsr writeLineQty;
            wCount += 1;
          endif;
        on-error;
        endmon;
      endif;
    endif;
  enddo;

  if wCount = 0;
    exsr msgNothingChanged;
  else;
    exsr msgApplied;
  endif;
endsr;

//------------------------------------------------------------------------
// "Everything arrived as expected" -- the common case for a clean pallet,
// and the one that must not cost 34 taps.
//------------------------------------------------------------------------
begsr confirmAll;
  if hstat = 'POST';
    exsr msgAlreadyPosted;
    leavesr;
  endif;

  exec sql
    update gtrcptdtl
       set qty_received = qty_expected,
           line_status  = 'MTCH',
           reason_code  = '',
           updated_by   = :wOper,
           updated_ts   = current timestamp
     where receipt_id = :hrcpt
       and line_status = 'OPEN';

  if sqlcode < 0;
    exec sql rollback;
    exsr msgUpdateFailed;
    leavesr;
  endif;

  exsr markInProgress;

  exec sql commit;
  exsr msgConfirmedAll;
endsr;

//------------------------------------------------------------------------
// One line's received quantity. Status is derived from the numbers, except
// that a damaged line keeps DAMG -- a reason the operator gave outranks
// arithmetic.
//------------------------------------------------------------------------
begsr writeLineQty;
  if wQty < 0;
    wQty = 0;
  endif;

  if lines(hitIdx).reason <> *blanks and lines(hitIdx).stat = 'DAMG';
    wNewSt = 'DAMG';
  elseif wQty = lines(hitIdx).qtyExp;
    wNewSt = 'MTCH';
  elseif wQty > lines(hitIdx).qtyExp;
    wNewSt = 'OVER';
  else;
    wNewSt = 'SHRT';
  endif;

  wLine = lines(hitIdx).lineNo;

  exec sql
    update gtrcptdtl
       set qty_received    = :wQty,
           line_status     = :wNewSt,
           scanned_barcode = :wBar,
           updated_by      = :wOper,
           updated_ts      = current timestamp
     where receipt_id = :hrcpt
       and line_no    = :wLine;

  if sqlcode < 0;
    exec sql rollback;
    exsr msgUpdateFailed;
    leavesr;
  endif;

  exsr markInProgress;

  exec sql commit;

  lines(hitIdx).qtyRcv = wQty;
  lines(hitIdx).stat   = wNewSt;
endsr;

//------------------------------------------------------------------------
// A receipt somebody has started counting is no longer "not started".
//
// Nothing lifted the header out of OPEN, so it read "Not started" on the
// screen and in the open-receipt list however many lines had been confirmed --
// only POST ever changed it. Runs inside the caller's commit boundary, so the
// header and the line move together or not at all.
//------------------------------------------------------------------------
begsr markInProgress;
  if hstat <> 'OPEN';
    leavesr;
  endif;

  exec sql
    update gtrcpthdr
       set receipt_status = 'PART'
     where receipt_id = :hrcpt
       and receipt_status = 'OPEN';

  if sqlcode >= 0;
    hstat = 'PART';
  endif;
endsr;

//------------------------------------------------------------------------
// POST -- one unit of work. Either the whole receipt is received or none of
// it is.
//
// COMMIT and ROLLBACK are the only two exits. Every statement is checked,
// because a failure that is noticed only at the end has already written rows
// somebody will later have to explain.
//------------------------------------------------------------------------
begsr postReceipt;
  posted = *off;

  if hstat = 'POST';
    exsr msgAlreadyPosted;
    leavesr;
  endif;

  if lineCount = 0;
    exsr msgNothingToPost;
    leavesr;
  endif;

  // Refuse to post a receipt nobody has actually looked at. An all-zero
  // receipt is almost always somebody pressing Post on the wrong pallet.
  if ndone = 0;
    exsr msgNothingConfirmed;
    leavesr;
  endif;

  // ---- 1. one movement row per received line -----------------------
  exec sql
    insert into gtmovement
           (movement_type, sku, from_location, to_location, qty,
            reason_code, source_doc, moved_by)
    select 'RECV', d.sku, '', :STAGE_LOC, d.qty_received,
           d.reason_code, d.receipt_id, :wOper
      from gtrcptdtl d
     where d.receipt_id = :hrcpt
       and d.qty_received > 0;

  if sqlcode < 0;
    exec sql rollback;
    exsr msgPostFailed;
    leavesr;
  endif;

  // ---- 2. balances: update what exists, insert what does not -------
  // Two statements rather than a MERGE: the UPDATE covers a SKU already in
  // staging from an earlier pallet, the INSERT covers the rest. Order
  // matters -- insert first and the update would double-count it.
  exec sql
    update gtinvbal b
       set qty_on_hand = b.qty_on_hand +
             (select sum(d.qty_received)
                from gtrcptdtl d
               where d.receipt_id = :hrcpt
                 and d.sku = b.sku
                 and d.qty_received > 0)
     where b.location_id = :STAGE_LOC
       and exists (select 1
                     from gtrcptdtl d
                    where d.receipt_id = :hrcpt
                      and d.sku = b.sku
                      and d.qty_received > 0);

  if sqlcode < 0;
    exec sql rollback;
    exsr msgPostFailed;
    leavesr;
  endif;

  exec sql
    insert into gtinvbal (sku, location_id, qty_on_hand, qty_allocated)
    select d.sku, :STAGE_LOC, sum(d.qty_received), 0
      from gtrcptdtl d
     where d.receipt_id = :hrcpt
       and d.qty_received > 0
       and not exists (select 1
                         from gtinvbal b
                        where b.sku = d.sku
                          and b.location_id = :STAGE_LOC)
     group by d.sku;

  if sqlcode < 0;
    exec sql rollback;
    exsr msgPostFailed;
    leavesr;
  endif;

  // ---- 3. lines nobody touched are received at zero, not at expected
  exec sql
    update gtrcptdtl
       set line_status = 'SHRT',
           updated_by  = :wOper,
           updated_ts  = current timestamp
     where receipt_id = :hrcpt
       and line_status = 'OPEN';

  if sqlcode < 0;
    exec sql rollback;
    exsr msgPostFailed;
    leavesr;
  endif;

  // ---- 4. header ---------------------------------------------------
  exec sql
    update gtrcpthdr
       set receipt_status = 'POST',
           received_by    = :wOper,
           received_ts    = current timestamp
     where receipt_id = :hrcpt;

  if sqlcode < 0;
    exec sql rollback;
    exsr msgPostFailed;
    leavesr;
  endif;

  // ---- 5. the pallet ------------------------------------------------
  if hsscc <> *blanks;
    exec sql
      update gtpallet
         set pallet_status = 'RECV',
             location_id   = :STAGE_LOC
       where sscc = :hsscc;

    // A receipt with no pallet row on file is not a reason to fail the
    // post; the stock is still received. SQLCODE 100 is that case.
    if sqlcode < 0;
      exec sql rollback;
      exsr msgPostFailed;
      leavesr;
    endif;
  endif;

  exec sql commit;

  // A COMMIT can itself fail. Checking it is the difference between "posted"
  // and "believed to be posted".
  if sqlcode < 0;
    exec sql rollback;
    exsr msgPostFailed;
    leavesr;
  endif;

  posted = *on;
  hstat  = 'POST';
endsr;

//------------------------------------------------------------------------
begsr loadHeader;
  exec sql
    select po_number, vendor_name, door_id, receipt_status, sscc
      into :hpo, :hvend, :hdoor, :hstat, :hsscc
      from gtrcpthdr
     where receipt_id = :hrcpt
     fetch first row only;

  if sqlcode <> 0;
    // Reached only if the receipt vanished between screens. Nothing to
    // show, so do not pretend otherwise.
    hstat = *blanks;
    done  = *on;
  endif;
endsr;

//------------------------------------------------------------------------
// The lines, and the summary counters that sit above them. Both come from
// the same read, so the bar and the cards cannot disagree.
//------------------------------------------------------------------------
begsr loadLineSubfile;
  // The CLEAR writes the RECORD FORMAT, not the subfile record. Writing the
  // subfile with rrn still 0 gives CPF5021 "incorrect relative record
  // number" -- an inquiry message on the operator's screen, not a
  // compile error.
  rrn = 0;
  sflclear = '1';
  write rcvlines;
  sflclear = '0';
  lineCount = 0;
  ndone  = 0;
  nvar   = 0;
  nphoto = 0;

  exec sql
    declare linCsr cursor for
      select line_no, sku, description_en, description_fr, department, uom,
             case_pack, thumb_id, qty_expected, qty_received, qty_variance,
             line_status, reason_code, photo_count
        from gtvrcvlin
       where receipt_id = :hrcpt
       --  LINE ORDER, always. GTVRCVLIN also offers sort_group (untouched
       --  first, then variances), and ordering by it was the obvious choice
       --  until it was tried: every scan moved the line that had just been
       --  scanned to a different place in the list, so the flash-and-scroll
       --  landed on a row that had jumped and the operator lost their place.
       --  Scan-to-confirm needs a list that holds still.
       order by line_no
       fetch first 60 rows only;

  exec sql open linCsr;

  dow lineCount < MAX_ROWS;
    // Both descriptions are fetched and the language chosen afterwards.
    // One FETCH, one host variable per column -- reusing a variable for two
    // columns to "save a declaration" silently mixes the two languages up.
    exec sql
      fetch linCsr into :wLine, :wSku, :wDescEn, :wDescFr, :wDept, :wUom,
                        :wCase, :wImg, :wExp, :wRcv, :wVar,
                        :wStat, :wRsn, :wPho;
    if sqlcode <> 0;
      leave;
    endif;

    if langpref = 'FR' and wDescFr <> *blanks;
      wDesc = wDescFr;
    else;
      wDesc = wDescEn;
    endif;

    lineCount += 1;
    lines(lineCount).lineNo = wLine;
    lines(lineCount).sku    = wSku;
    lines(lineCount).desc   = wDesc;
    lines(lineCount).dept   = wDept;
    lines(lineCount).uom    = wUom;
    lines(lineCount).casePk = wCase;
    lines(lineCount).img    = wImg;
    lines(lineCount).qtyExp = wExp;
    lines(lineCount).qtyRcv = wRcv;
    lines(lineCount).stat   = wStat;
    lines(lineCount).reason = wRsn;
    lines(lineCount).photos = wPho;

    if wStat <> 'OPEN';
      ndone += 1;
    endif;
    if wStat = 'SHRT' or wStat = 'OVER' or wStat = 'DAMG';
      nvar += 1;
    endif;
    nphoto += wPho;

    rrn    += 1;
    lseq    = lineCount;
    lline   = wLine;
    lsku    = wSku;
    ldesc   = wDesc;
    ldept   = wDept;
    luom    = wUom;
    limg    = wImg;
    lexp    = wExp;
    lrcv    = wRcv;
    lvar    = wVar;
    lstat   = wStat;
    lreason = wRsn;
    lphoto  = wPho;
    lqty    = wRcv;
    //  The stepper on the card moves by a CASE, not by one: a line expecting
    //  864 eaches is not counted up in single taps. Sent to the screen so the
    //  step is the item's real case pack rather than a number invented in the
    //  template.
    lcase   = wCase;
    write linsfl;
  enddo;

  exec sql close linCsr;

  nlines = lineCount;
endsr;

//------------------------------------------------------------------------
begsr loadOperator;
  exec sql
    select full_name, operator_id into :opername, :wOper
      from gtoperator
     where operator_id = 'GT001' and active_flag = 'Y'
     fetch first row only;

  if sqlcode <> 0;
    opername = 'Demo Operator';
    wOper    = 'GT001';
  endif;
endsr;

//------------------------------------------------------------------------
begsr saveLanguage;
  exec sql
    update gtoperator set lang_pref = :langpref
     where operator_id = 'GT001';
  if sqlcode < 0;
    exec sql rollback;
  else;
    exec sql commit;
  endif;
endsr;

//------------------------------------------------------------------------
// Messages. msgkind drives the colour and the haptic on the screen, so a
// receiver working by feel knows a scan landed without reading anything.
//------------------------------------------------------------------------
begsr msgScanned;
  msgkind = 'GOOD';
  if langpref = 'FR';
    msg = %trim(lines(hitIdx).sku) + ' : ' +
          %trim(%editc(lines(hitIdx).qtyRcv : '3')) + ' sur ' +
          %trim(%editc(lines(hitIdx).qtyExp : '3')) + ' reçus.';
  else;
    msg = %trim(lines(hitIdx).sku) + ': ' +
          %trim(%editc(lines(hitIdx).qtyRcv : '3')) + ' of ' +
          %trim(%editc(lines(hitIdx).qtyExp : '3')) + ' received.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgNothingScanned;
  msgkind = 'WARN';
  if langpref = 'FR';
    msg = 'Scannez un carton, ou saisissez les quantités.';
  else;
    msg = 'Scan a carton, or type the quantities in.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgUnknownBarcode;
  msgkind = 'BAD';
  if langpref = 'FR';
    msg = 'Code-barres inconnu : ' + %trim(payload);
  else;
    msg = 'Unrecognised barcode: ' + %trim(payload);
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgNotOnReceipt;
  msgkind = 'BAD';
  if langpref = 'FR';
    msg = %trim(r.sku) + ' ne figure pas sur cette réception.';
  else;
    msg = %trim(r.sku) + ' is not on this receipt.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgApplied;
  msgkind = 'GOOD';
  if langpref = 'FR';
    msg = 'Quantités enregistrées.';
  else;
    msg = 'Quantities saved.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgNothingChanged;
  msgkind = 'WARN';
  if langpref = 'FR';
    msg = 'Aucune quantité modifiée.';
  else;
    msg = 'No quantities were changed.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgConfirmedAll;
  msgkind = 'GOOD';
  if langpref = 'FR';
    msg = 'Toutes les lignes confirmées comme prévu.';
  else;
    msg = 'All remaining lines confirmed as expected.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgNothingToPost;
  msgkind = 'WARN';
  if langpref = 'FR';
    msg = 'Cette réception n''a aucune ligne.';
  else;
    msg = 'This receipt has no lines.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgNothingConfirmed;
  msgkind = 'WARN';
  if langpref = 'FR';
    msg = 'Confirmez au moins une ligne avant de valider.';
  else;
    msg = 'Confirm at least one line before posting.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgAlreadyPosted;
  msgkind = 'WARN';
  if langpref = 'FR';
    msg = 'Réception déjà validée : lecture seule.';
  else;
    msg = 'This receipt is already posted -- read only.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgUpdateFailed;
  msgkind = 'BAD';
  if langpref = 'FR';
    msg = 'Échec de la mise à jour (SQL ' + %trim(%char(sqlcode)) + ').';
  else;
    msg = 'Update failed (SQL ' + %trim(%char(sqlcode)) + ').';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgPostFailed;
  msgkind = 'BAD';
  if langpref = 'FR';
    msg = 'Validation annulée, rien reçu (SQL ' + %trim(%char(sqlcode)) + ').';
  else;
    msg = 'Post rolled back, nothing received (SQL ' +
          %trim(%char(sqlcode)) + ').';
  endif;
endsr;
