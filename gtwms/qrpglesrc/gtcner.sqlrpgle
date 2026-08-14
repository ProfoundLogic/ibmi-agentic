**free

//////////////////////////////////////////////////////////////////////////
// GTCNER -- Cycle Count, BLIND entry.
//
// THE EXPECTED QUANTITY IS NEVER SENT TO THIS SCREEN. Not hidden by CSS, not
// greyed out -- absent. GTCNED has no field to hold it, and the cursor below
// does not select it. A counter who can see the system's number writes down the
// system's number, and the count is then worth nothing.
//
// That is also why this is a separate program from the variance review: the
// review needs qty_expected, so it has its own display file that carries it.
// One screen doing both would have had the value in the datastream throughout.
//
// Scan an item on the count -> its line ticks up by a case, or by one.
// Scan an item NOT on the count -> a line is added, expected zero. Extra stock
// found in a location is a real finding, and refusing to record it is how it
// ends up unrecorded.
//////////////////////////////////////////////////////////////////////////

ctl-opt dftactgrp(*no) actgrp(*new);
ctl-opt bnddir('GTBAR');

dcl-f gtcned workstn sfile(cesfl : rrn) handler('PROFOUNDUI(HANDLER)');

exec sql set option commit = *chg, closqlcsr = *endmod;

/copy gtbar_pr.rpgle

dcl-pr gtcnvr extpgm;
  countId char(10) const;
  lang    char(2);
end-pr;

dcl-pi *n;
  inCnt  char(10) const;
  inLang char(2);
end-pi;

dcl-ds r likeds(gtbar_result_t);

dcl-c MAX_ROWS 60;

dcl-s rrn     int(10) inz(0);
dcl-s i       int(10);
dcl-s done    ind inz(*off);
dcl-s payload varchar(120);
dcl-s bumpQty packed(11 : 2);
dcl-s hitIdx  int(10);

dcl-ds lines qualified dim(MAX_ROWS);
  lineNo int(10);
  sku    char(15);
  casePk int(10);
  cnt    packed(11 : 2);
  flag   char(1);
end-ds;
dcl-s lineCount int(10) inz(0);

//  Work fields.
dcl-s wLine   int(10);
dcl-s wSku    char(15);
dcl-s wDesc   varchar(60);
dcl-s wDescEn varchar(60);
dcl-s wDescFr varchar(60);
dcl-s wDept  char(4);
dcl-s wUom   char(3);
dcl-s wCase  int(10);
dcl-s wImg   int(10);
dcl-s wCnt   packed(11 : 2);
dcl-s wFlag  char(1);
dcl-s wQty   packed(11 : 2);
dcl-s wOper  varchar(18);
dcl-s wNext  int(10);
dcl-s wRest  varchar(1024);
dcl-s wTok   varchar(32);
dcl-s wIdx   int(10);
dcl-s wPos   int(10);
dcl-s wCount int(10);

//------------------------------------------------------------------------
langpref = inLang;
hcnt     = inCnt;

exsr loadOperator;
exsr loadHeader;

dow not done;
  exsr loadLineSubfile;
  action   = *blanks;
  scanval  = *blanks;
  cntedits = *blanks;
  exfmt cntentry;
  msg      = *blanks;
  msgkind  = *blanks;
  hitline  = 0;

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
      exsr applyTyped;

    when action = 'REVIEW';
      //  Done counting -- on to the variance review, which is the only place
      //  the expected quantities appear.
      exsr goReview;

    when action = *blanks;
      //  Enter with nothing pressed. Redisplay.

    other;
  endsl;
enddo;

*inlr = *on;
return;

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
    //  Not on the count: extra stock found. Add it rather than refuse it.
    exsr addLine;
    leavesr;
  endif;

  //  A case barcode means a case.
  if r.barLevel = 'CS' and lines(hitIdx).casePk > 0;
    bumpQty = lines(hitIdx).casePk;
  elseif r.qtyCount > 0;
    bumpQty = r.qtyCount;
  else;
    bumpQty = 1;
  endif;

  wQty = lines(hitIdx).cnt + bumpQty;
  exsr writeLineQty;

  hitline = lines(hitIdx).lineNo;
  exsr msgScanned;
endsr;

//------------------------------------------------------------------------
// Extra stock: a new line at expected zero, counted with what was scanned.
//------------------------------------------------------------------------
begsr addLine;
  wSku = r.sku;

  exec sql
    select coalesce(max(line_no), 0) + 1 into :wNext
      from gtcountdtl
     where count_id = :hcnt;

  if sqlcode <> 0;
    exsr msgAddFailed;
    leavesr;
  endif;

  if r.barLevel = 'CS' and r.packQty > 0;
    wQty = r.packQty;
  elseif r.qtyCount > 0;
    wQty = r.qtyCount;
  else;
    wQty = 1;
  endif;

  exec sql
    insert into gtcountdtl (count_id, line_no, sku, qty_expected,
                            qty_counted, counted_flag, reason_code)
    values (:hcnt, :wNext, :wSku, 0, :wQty, 'Y', 'CEXT');

  if sqlcode < 0;
    exec sql rollback;
    exsr msgAddFailed;
    leavesr;
  endif;

  exsr markCounting;
  exec sql commit;

  hitline = wNext;
  exsr msgAdded;
endsr;

//------------------------------------------------------------------------
// Typed quantities, carried in ONE field as "seq:qty;".
//
// NOT a READC: in an EJS screen the subfile rows are rendered by the template,
// so every box carries the same field name with no record number and the
// runtime has nothing to build its changed-record marker from. See §34.7.
//
// A line appearing in the payload IS the counted flag -- which is how a
// deliberate count of zero is distinguishable from a line nobody has reached.
//------------------------------------------------------------------------
begsr applyTyped;
  if hstat = 'POST';
    exsr msgAlreadyPosted;
    leavesr;
  endif;

  wCount = 0;
  wRest  = %trim(cntedits);

  dow wRest <> *blanks;
    wPos = %scan(';' : wRest);
    if wPos > 0;
      if wPos > 1;
        wTok = %subst(wRest : 1 : wPos - 1);
      else;
        wTok = *blanks;
      endif;
      //  Advancing past the final character is RNX0100, and it arrives as an
      //  inquiry message on the operator's screen.
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
        monitor;
          wIdx = %int(%subst(wTok : 1 : wPos - 1));
          wQty = %dec(%subst(%trim(wTok) : wPos + 1) : 11 : 2);

          if wIdx >= 1 and wIdx <= lineCount;
            //  An unchanged value on an ALREADY counted line is not a change.
            //  On an uncounted line it is: it means "I counted that, and the
            //  answer happens to be what was showing".
            if wQty <> lines(wIdx).cnt or lines(wIdx).flag <> 'Y';
              hitIdx = wIdx;
              exsr writeLineQty;
              wCount += 1;
            endif;
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
begsr writeLineQty;
  if wQty < 0;
    wQty = 0;
  endif;

  wLine = lines(hitIdx).lineNo;

  exec sql
    update gtcountdtl
       set qty_counted  = :wQty,
           counted_flag = 'Y'
     where count_id = :hcnt
       and line_no  = :wLine;

  if sqlcode < 0;
    exec sql rollback;
    exsr msgUpdateFailed;
    leavesr;
  endif;

  exsr markCounting;
  exec sql commit;

  lines(hitIdx).cnt  = wQty;
  lines(hitIdx).flag = 'Y';
endsr;

//------------------------------------------------------------------------
// A count somebody has started is no longer OPEN. Inside the caller's commit
// boundary, so the header and the line move together.
//------------------------------------------------------------------------
begsr markCounting;
  if hstat <> 'OPEN';
    leavesr;
  endif;

  exec sql
    update gtcounthdr
       set count_status = 'CNTD',
           counted_by   = :wOper,
           counted_ts   = current timestamp
     where count_id = :hcnt
       and count_status = 'OPEN';

  if sqlcode >= 0;
    hstat = 'CNTD';
  endif;
endsr;

//------------------------------------------------------------------------
begsr goReview;
  if ndone = 0;
    exsr msgNothingCounted;
    leavesr;
  endif;

  gtcnvr(hcnt : langpref);
  exsr loadHeader;

  //  A posted count has nothing left to enter.
  if hstat = 'POST';
    done = *on;
  endif;
endsr;

//------------------------------------------------------------------------
begsr loadHeader;
  exec sql
    select h.location_id, l.zone, l.location_type, h.count_status
      into :hloc, :hzone, :htype, :hstat
      from gtcounthdr h
      join gtlocation l on l.location_id = h.location_id
     where h.count_id = :hcnt
     fetch first row only;

  if sqlcode <> 0;
    done = *on;
  endif;
endsr;

//------------------------------------------------------------------------
// The lines. NOTE WHAT IS NOT SELECTED: qty_expected. This is the blind
// count; the expected quantity has no business in this datastream.
//------------------------------------------------------------------------
begsr loadLineSubfile;
  rrn = 0;
  sflclear = '1';
  write cntentry;
  sflclear = '0';
  lineCount = 0;
  ndone = 0;

  exec sql
    declare linCsr cursor for
      select line_no, sku, description_en, description_fr, department, uom,
             case_pack, thumb_id, qty_counted, counted_flag
        from gtvcntlin
       where count_id = :hcnt
       order by line_no
       fetch first 60 rows only;

  exec sql open linCsr;

  dow lineCount < MAX_ROWS;
    exec sql
      fetch linCsr into :wLine, :wSku, :wDescEn, :wDescFr, :wDept, :wUom,
                        :wCase, :wImg, :wCnt, :wFlag;
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
    lines(lineCount).casePk = wCase;
    lines(lineCount).cnt    = wCnt;
    lines(lineCount).flag   = wFlag;

    if wFlag = 'Y';
      ndone += 1;
    endif;

    rrn  += 1;
    eseq  = lineCount;
    eline = wLine;
    esku  = wSku;
    edesc = wDesc;
    edept = wDept;
    euom  = wUom;
    eimg  = wImg;
    ecnt  = wCnt;
    eflag = wFlag;
    ecase = wCase;
    eqty  = wCnt;
    write cesfl;
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
begsr msgScanned;
  msgkind = 'GOOD';
  if langpref = 'FR';
    msg = %trim(lines(hitIdx).sku) + ' : ' +
          %trim(%editc(lines(hitIdx).cnt : '3')) + ' dénombré.';
  else;
    msg = %trim(lines(hitIdx).sku) + ': counted ' +
          %trim(%editc(lines(hitIdx).cnt : '3')) + '.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgAdded;
  msgkind = 'GOOD';
  if langpref = 'FR';
    msg = %trim(wSku) + ' ajouté : stock excédentaire trouvé.';
  else;
    msg = %trim(wSku) + ' added -- extra stock found.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgNothingScanned;
  msgkind = 'WARN';
  if langpref = 'FR';
    msg = 'Scannez un article, ou saisissez les quantités.';
  else;
    msg = 'Scan an item, or type the quantities in.';
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
begsr msgApplied;
  msgkind = 'GOOD';
  if langpref = 'FR';
    msg = 'Quantités dénombrées enregistrées.';
  else;
    msg = 'Counted quantities saved.';
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
begsr msgNothingCounted;
  msgkind = 'WARN';
  if langpref = 'FR';
    msg = 'Dénombrez au moins une ligne avant la révision.';
  else;
    msg = 'Count at least one line before reviewing.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgAlreadyPosted;
  msgkind = 'WARN';
  if langpref = 'FR';
    msg = 'Dénombrement déjà validé : lecture seule.';
  else;
    msg = 'This count is already posted -- read only.';
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
begsr msgAddFailed;
  msgkind = 'BAD';
  if langpref = 'FR';
    msg = 'Impossible d''ajouter la ligne (SQL ' + %trim(%char(sqlcode)) + ').';
  else;
    msg = 'Could not add the line (SQL ' + %trim(%char(sqlcode)) + ').';
  endif;
endsr;
