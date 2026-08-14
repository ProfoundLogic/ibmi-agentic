**free

//////////////////////////////////////////////////////////////////////////
// GTPUR -- Putaway, home screen.
//
// "Staging to storage", which is what the menu tile has always said. Receiving
// posts everything it accepts into STAGE01 and parks the pallet there; until
// somebody puts that stock away it cannot be picked and it is standing in the
// way of the next truck. This screen is that queue.
//
// ONE SCAN BOX, SEVERAL MEANINGS -- the same contract as Inventory and
// Receiving, because an operator holding a scanner should never have to tell
// the application what kind of label they are pointing at:
//
//   a pallet SSCC        -> the staged lines that arrived on it
//   an item barcode      -> straight to that item's putaway confirm screen
//   a staging location   -> everything staged in it (the default list anyway)
//
// The list is GTVPUTSTG and the suggestion is GTVPUTSUG, so the destination
// offered here and the destination offered on the confirm screen are the same
// answer to the same question. Putting that ranking in two places is how the
// list and the confirm screen come to disagree.
//
// WORK LIST, NOT HISTORY. There is deliberately no "recently put away" list:
// it would tell an operator where they have been rather than where the work is.
//////////////////////////////////////////////////////////////////////////

ctl-opt dftactgrp(*no) actgrp(*new);
ctl-opt bnddir('GTBAR');

dcl-f gtpud workstn sfile(stgsfl : rrn) handler('PROFOUNDUI(HANDLER)');

exec sql set option commit = *chg, closqlcsr = *endmod;

/copy gtbar_pr.rpgle

//  The confirm-and-book screen. inSku plus the staging location it is sitting
//  in, because the same SKU can be staged in more than one place.
dcl-pr gtpdr extpgm;
  inSku   char(15) const;
  inStage char(10) const;
  inLang  char(2);
end-pr;

dcl-ds r likeds(gtbar_result_t);

dcl-c MAX_ROWS 40;

dcl-s rrn      int(10) inz(0);
dcl-s i        int(10);
dcl-s done     ind inz(*off);
dcl-s payload  varchar(120);

//  The staged list, held between round trips so a tap resolves without
//  re-running the query that drew the card.
dcl-ds staged qualified dim(MAX_ROWS);
  sku   char(15);
  stage char(10);
end-ds;
dcl-s stagedCount int(10) inz(0);

//  Work fields -- RNF0724 forbids dcl-s inside a begsr in the main procedure.
dcl-s wSku     char(15);
dcl-s wDesc    varchar(60);
dcl-s wDescEn  varchar(60);
dcl-s wDescFr  varchar(60);
dcl-s wDept    char(4);
dcl-s wUom     char(3);
dcl-s wCase    int(10);
dcl-s wImg     int(10);
dcl-s wQty     packed(11 : 2);
dcl-s wStage   char(10);
dcl-s wSscc    char(18);
dcl-s wSugg    char(10);
dcl-s wSugTyp  char(4);
dcl-s wSugRoom packed(11 : 2);
dcl-s wSugRank int(10);
dcl-s wCount   int(10);
dcl-s wUnits   packed(11 : 2);
dcl-s wKey     char(10);
dcl-s wFound   ind;

//------------------------------------------------------------------------
// Main
//------------------------------------------------------------------------
exsr loadOperator;

dow not done;
  exsr loadStagedSubfile;

  action  = *blanks;
  scanval = *blanks;
  exfmt puthome;
  msg     = *blanks;
  msgkind = *blanks;
  hitline = 0;

  select;
    when action = 'EXIT';
      done = *on;

    when action = 'LANGEN';
      langpref = 'EN';
      exsr saveLanguage;

    when action = 'LANGFR';
      langpref = 'FR';
      exsr saveLanguage;

    when action = 'SCAN';
      exsr doScan;

    when %subst(action : 1 : 4) = 'PICK';
      //  PICKnn -- the operator tapped a staged card. nn is the subfile
      //  sequence, not the RRN.
      i = %int(%subst(action : 5));
      if i >= 1 and i <= stagedCount;
        gtpdr(staged(i).sku : staged(i).stage : langpref);
      endif;

    when action = *blanks;
      //  Enter with nothing pressed. Redisplay -- which re-reads, so the list
      //  reflects anything just put away.

    other;
  endsl;
enddo;

*inlr = *on;
return;

//------------------------------------------------------------------------
// The scan. GTBAR identifies; what it MEANS is decided here.
//
// Order matters, most specific first. A pallet is the common case on a dock:
// the operator is standing in front of one.
//------------------------------------------------------------------------
begsr doScan;
  payload = %trim(scanval);

  if payload = *blanks;
    exsr msgNothingScanned;
    leavesr;
  endif;

  clear r;
  gtbar_parse(payload : r);

  select;
    //  ---- a pallet: flash its staged lines ------------------------
    when r.sscc <> *blanks;
      exsr findByPallet;

    //  ---- an item: straight to its confirm screen -----------------
    when r.sku <> *blanks;
      exsr findBySku;

    //  ---- a staging location: it is already the list --------------
    when r.locationId <> *blanks;
      exsr msgLocationIsList;

    //  ---- hand-keyed. A typed SKU or pallet has no symbology, so try
    //       both before giving up.
    other;
      wKey = %upper(%trim(payload));
      exec sql
        select sku into :wSku
          from gtvputstg
         where sku = :wKey
         fetch first row only;

      if sqlcode = 0;
        r.sku = wSku;
        exsr findBySku;
      else;
        exec sql
          select max(sscc) into :wSscc
            from gtvputstg
           where sscc = :payload;

        if sqlcode = 0 and wSscc <> *blanks;
          r.sscc = wSscc;
          exsr findByPallet;
        else;
          exsr msgUnknownScan;
        endif;
      endif;
  endsl;
endsr;

//------------------------------------------------------------------------
// A scanned SKU. Resolve it against the list already on screen so the
// operator lands on the row they can see, and so a SKU that is not staged
// says so rather than opening an empty confirm screen.
//------------------------------------------------------------------------
begsr findBySku;
  wFound = *off;

  for i = 1 to stagedCount;
    if staged(i).sku = r.sku;
      wFound = *on;
      gtpdr(staged(i).sku : staged(i).stage : langpref);
      leave;
    endif;
  endfor;

  if not wFound;
    exsr msgSkuNotStaged;
  endif;
endsr;

//------------------------------------------------------------------------
// A scanned pallet. Everything that arrived on it is staged in one place, so
// this marks the first of its rows -- the screen scrolls to it and flashes it.
//------------------------------------------------------------------------
begsr findByPallet;
  wFound = *off;

  exec sql
    select min(location_id) into :wStage
      from gtvputstg
     where sscc = :r.sscc;

  if sqlcode = 0 and wStage <> *blanks;
    for i = 1 to stagedCount;
      if staged(i).stage = wStage;
        hitline = i;
        wFound  = *on;
        leave;
      endif;
    endfor;
  endif;

  if wFound;
    exsr msgPalletFound;
  else;
    exsr msgPalletNothingStaged;
  endif;
endsr;

//------------------------------------------------------------------------
// The staged list, newest arrivals last so the dock is worked in order.
//
// The suggestion comes from GTVPUTSUG rank 1. A staged SKU with no candidate
// at all still appears -- with a blank suggestion -- because hiding work the
// application cannot advise on is how stock goes missing.
//------------------------------------------------------------------------
begsr loadStagedSubfile;
  rrn = 0;
  sflclear = '1';
  write puthome;
  sflclear = '0';
  stagedCount = 0;

  exec sql
    declare stgCsr cursor for
      select s.sku, s.description_en, s.description_fr, s.department,
             s.uom, s.case_pack, s.image_id, s.qty_staged,
             s.stage_location, s.sscc
        from gtvputstg s
       order by s.stage_location, s.sku
       fetch first 40 rows only;

  exec sql open stgCsr;

  dow stagedCount < MAX_ROWS;
    exec sql
      fetch stgCsr into :wSku, :wDescEn, :wDescFr, :wDept, :wUom, :wCase,
                        :wImg, :wQty, :wStage, :wSscc;
    if sqlcode <> 0;
      leave;
    endif;

    if langpref = 'FR' and wDescFr <> *blanks;
      wDesc = wDescFr;
    else;
      wDesc = wDescEn;
    endif;

    //  The winner of the shared suggestion. SORT_KEY carries the whole
    //  opinion -- does it FIT first, then where it BELONGS -- so this ORDER BY
    //  and the confirm screen's are the same one line and cannot drift. Most
    //  room breaks a tie, so a half-empty face beats one with a carton's worth
    //  left in it.
    wSugg    = *blanks;
    wSugTyp  = *blanks;
    wSugRoom = 0;
    wSugRank = 0;

    exec sql
      select location_id, location_type, room, rank_no
        into :wSugg, :wSugTyp, :wSugRoom, :wSugRank
        from gtvputsug
       where sku = :wSku
       order by sort_key, room desc
       fetch first row only;

    if sqlcode <> 0;
      wSugg    = *blanks;
      wSugTyp  = *blanks;
      wSugRoom = 0;
      wSugRank = 0;
    endif;

    stagedCount += 1;
    staged(stagedCount).sku   = wSku;
    staged(stagedCount).stage = wStage;

    rrn     += 1;
    sseq     = stagedCount;
    ssku     = wSku;
    sdesc    = wDesc;
    sdept    = wDept;
    suom     = wUom;
    simg     = wImg;
    sqty     = wQty;
    scase    = wCase;
    sloc     = wStage;
    ssugg    = wSugg;
    ssugtyp  = wSugTyp;
    ssugrm   = wSugRoom;
    ssugrnk  = wSugRank;
    ssscc    = wSscc;
    write stgsfl;
  enddo;

  exec sql close stgCsr;

  //  The real totals, not the 40 shown, so the screen never claims there are
  //  40 lines staged when there are more.
  exec sql
    select count(*), coalesce(sum(qty_staged), 0)
      into :wCount, :wUnits
      from gtvputstg;

  if sqlcode = 0;
    nrows  = wCount;
    nunits = wUnits;
  else;
    nrows  = stagedCount;
    nunits = 0;
  endif;
endsr;

//------------------------------------------------------------------------
begsr loadOperator;
  exec sql
    select full_name, lang_pref
      into :opername, :langpref
      from gtoperator
     where operator_id = 'GT001' and active_flag = 'Y'
     fetch first row only;

  if sqlcode <> 0;
    opername = 'Demo Operator';
    langpref = 'EN';
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
// Messages. Specific about what happened: "not found" is no use to somebody
// standing on a dock holding a scanner.
//------------------------------------------------------------------------
begsr msgNothingScanned;
  msgkind = 'WARN';
  if langpref = 'FR';
    msg = 'Scannez une palette ou un article en zone de réception.';
  else;
    msg = 'Scan a pallet or an item in staging.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgPalletFound;
  msgkind = 'GOOD';
  if langpref = 'FR';
    msg = 'Palette ' + %trim(r.sscc) + ' : lignes en zone ' + %trim(wStage) + '.';
  else;
    msg = 'Pallet ' + %trim(r.sscc) + ' is staged in ' + %trim(wStage) + '.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgPalletNothingStaged;
  msgkind = 'WARN';
  if langpref = 'FR';
    msg = 'Palette ' + %trim(r.sscc) + ' : rien à ranger.';
  else;
    msg = 'Pallet ' + %trim(r.sscc) + ' has nothing waiting to be put away.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgSkuNotStaged;
  msgkind = 'WARN';
  if langpref = 'FR';
    msg = %trim(r.sku) + ' n’est pas en zone de réception.';
  else;
    msg = %trim(r.sku) + ' is not in staging.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgLocationIsList;
  msgkind = 'WARN';
  if langpref = 'FR';
    msg = 'Emplacement ' + %trim(r.locationId) +
          ' : la liste ci-dessous est déjà tout ce qui est en zone.';
  else;
    msg = 'Location ' + %trim(r.locationId) +
          ': the list below is already everything staged.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgUnknownScan;
  msgkind = 'BAD';
  if langpref = 'FR';
    msg = %trim(payload) + ' : ni palette, ni article en zone.';
  else;
    msg = %trim(payload) + ' is not a pallet or a staged item.';
  endif;
endsr;
