**free

//////////////////////////////////////////////////////////////////////////
// GTINLR -- Inventory, what is in this location.
//
// THE BULK VERSUS PICK-FACE STORY (design doc §8.3). For every item in the
// scanned location, two numbers side by side:
//
//   in this location   -- what you are standing in front of
//   available in bulk  -- on hand across all BULK locations, less allocations
//
// and a REPLENISH shortcut when a pick face is below its minimum and bulk has
// stock to pull from. The shortcut pre-fills the move screen -- bulk as FROM,
// this location as TO, the suggested quantity, reason REPL -- turning an
// inquiry into a one-tap action. That is the operational value in a single tap.
//
// Both numbers and the replenish condition come from GTVINVLOC, so this screen,
// the alert list on the home screen and the move screen cannot disagree about
// what needs replenishing or by how much.
//
// A pick face holds one SKU and a bulk location holds up to 26, so this screen
// has to read well as a one-row list and as a 26-row one.
//////////////////////////////////////////////////////////////////////////

ctl-opt dftactgrp(*no) actgrp(*new);
ctl-opt bnddir('GTBAR');

dcl-f gtinld workstn sfile(itmsfl : rrn) handler('PROFOUNDUI(HANDLER)');

exec sql set option commit = *chg, closqlcsr = *endmod;

/copy gtbar_pr.rpgle

//  The move screen. Everything is pre-fillable, so the same program serves a
//  plain move (TO blank, the operator scans it) and a replenishment (all four
//  values supplied).
dcl-pr gtimvr extpgm;
  fromLoc char(10) const;
  sku     char(15) const;
  toLoc   char(10) const;
  qty     packed(11 : 2) const;
  reason  char(4) const;
  lang    char(2);
end-pr;

dcl-ds r likeds(gtbar_result_t);

dcl-c MAX_ROWS 40;

dcl-s rrn      int(10) inz(0);
dcl-s i        int(10);
dcl-s done     ind inz(*off);
dcl-s payload  varchar(120);

//  The location's items, held between round trips.
dcl-ds items qualified dim(MAX_ROWS);
  sku    char(15);
  oh     packed(11 : 2);
  avail  packed(11 : 2);
  bulk   packed(11 : 2);
  sugg   packed(11 : 2);
  repl   char(1);
end-ds;
dcl-s itemCount int(10) inz(0);

//  Work fields.
dcl-s wSku    char(15);
dcl-s wDesc   varchar(60);
dcl-s wDescEn varchar(60);
dcl-s wDescFr varchar(60);
dcl-s wDept  char(4);
dcl-s wUom   char(3);
dcl-s wCase  int(10);
dcl-s wImg   int(10);
dcl-s wOh    packed(11 : 2);
dcl-s wAlloc packed(11 : 2);
dcl-s wAvail packed(11 : 2);
dcl-s wBulk  packed(11 : 2);
dcl-s wRepl  char(1);
dcl-s wSugg  packed(11 : 2);
dcl-s wFrom  char(10);
dcl-s wQty   packed(11 : 2);

//------------------------------------------------------------------------
dcl-pi *n;
  inLoc  char(10) const;
  inLang char(2);
end-pi;

//------------------------------------------------------------------------
// Main
//------------------------------------------------------------------------
langpref = inLang;
hloc     = inLoc;

exsr loadOperator;
exsr loadHeader;

dow not done;
  exsr loadItemSubfile;
  action  = *blanks;
  scanval = *blanks;
  exfmt invloc;
  msg     = *blanks;
  msgkind = *blanks;

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

    when %subst(action : 1 : 4) = 'MOVE';
      //  MOVEnn -- move this item out of here. The operator scans the TO
      //  location on the next screen.
      i = %int(%subst(action : 5));
      if i >= 1 and i <= itemCount;
        gtimvr(hloc : items(i).sku : *blanks : 0 : *blanks : langpref);
        exsr loadHeader;
      endif;

    when %subst(action : 1 : 4) = 'REPL';
      //  REPLnn -- replenish this pick face from bulk. Everything is
      //  pre-filled; the operator confirms.
      i = %int(%subst(action : 5));
      if i >= 1 and i <= itemCount;
        exsr startReplen;
      endif;

    when action = *blanks;
      //  Enter with nothing pressed. Redisplay.

    other;
  endsl;
enddo;

*inlr = *on;
return;

//------------------------------------------------------------------------
// A scan on this screen means "show me that instead": another location
// replaces what is displayed, an item jumps to its own detail.
//------------------------------------------------------------------------
begsr doScan;
  payload = %trim(scanval);

  if payload = *blanks;
    exsr msgNothingScanned;
    leavesr;
  endif;

  clear r;
  gtbar_parse(payload : r);

  if r.locationId <> *blanks;
    hloc = r.locationId;
    exsr loadHeader;
    leavesr;
  endif;

  //  Hand-keyed location ids carry no symbology, so try the literal too.
  exec sql
    select location_id into :wFrom
      from gtlocation
     where location_id = :payload
     fetch first row only;
  if sqlcode = 0;
    hloc = wFrom;
    exsr loadHeader;
    leavesr;
  endif;

  //  An item scanned HERE means "move this one", which is more useful than
  //  leaving the location: the operator is standing at the rack.
  if r.sku <> *blanks;
    for i = 1 to itemCount;
      if items(i).sku = r.sku;
        gtimvr(hloc : items(i).sku : *blanks : 0 : *blanks : langpref);
        exsr loadHeader;
        leavesr;
      endif;
    endfor;
    exsr msgItemNotHere;
    leavesr;
  endif;

  exsr msgUnknownScan;
endsr;

//------------------------------------------------------------------------
// Replenish: pull from the bulk location with the most available, so one
// move does as much of the job as possible rather than emptying the
// smallest pallet first.
//------------------------------------------------------------------------
begsr startReplen;
  wFrom = *blanks;
  //  The SQL precompiler will not accept an ARRAY ELEMENT as a host variable
  //  (SQL0312 "variable ITEMS not defined or not usable", then SQL0104 on the
  //  subscript), so the value is copied to a scalar first. Same restriction as
  //  the subfields of a likeds() parameter.
  wSku = items(i).sku;

  exec sql
    select b.location_id into :wFrom
      from gtinvbal   b
      join gtlocation l on l.location_id = b.location_id
     where b.sku = :wSku
       and l.location_type = 'BULK'
       and l.active_flag   = 'Y'
       and b.qty_on_hand - b.qty_allocated > 0
     order by b.qty_on_hand - b.qty_allocated desc
     fetch first row only;

  if sqlcode <> 0 or wFrom = *blanks;
    exsr msgNoBulk;
    leavesr;
  endif;

  wQty = items(i).sugg;
  if wQty <= 0;
    wQty = items(i).bulk;
  endif;

  gtimvr(wFrom : items(i).sku : hloc : wQty : 'REPL' : langpref);
  exsr loadHeader;
endsr;

//------------------------------------------------------------------------
begsr loadHeader;
  exec sql
    select zone, location_type, aisle, bay, level_id, capacity_units,
           active_flag, item_count, qty_on_hand, pct_full
      into :hzone, :htype, :haisle, :hbay, :hlevel, :hcap,
           :hactive, :hitems, :hqty, :hpct
      from gtvlocsum
     where location_id = :hloc
     fetch first row only;

  if sqlcode <> 0;
    //  Reached only if the location vanished between screens.
    exsr msgNoSuchLocation;
    done = *on;
  endif;
endsr;

//------------------------------------------------------------------------
// The items. GTVINVLOC already carries the bulk figure, the replenish flag
// and the suggested quantity, so this is one read and no per-row query.
//------------------------------------------------------------------------
begsr loadItemSubfile;
  rrn = 0;
  sflclear = '1';
  write invloc;
  sflclear = '0';
  itemCount = 0;

  exec sql
    declare itmCsr cursor for
      select sku, description_en, description_fr, department, uom, case_pack,
             thumb_id, qty_on_hand, qty_allocated, qty_available,
             qty_bulk_avail, needs_replen, qty_suggested
        from gtvinvloc
       where location_id = :hloc
       order by needs_replen desc, sku
       fetch first 40 rows only;

  exec sql open itmCsr;

  dow itemCount < MAX_ROWS;
    exec sql
      fetch itmCsr into :wSku, :wDescEn, :wDescFr, :wDept, :wUom, :wCase,
                        :wImg, :wOh, :wAlloc, :wAvail, :wBulk, :wRepl, :wSugg;
    if sqlcode <> 0;
      leave;
    endif;

    if langpref = 'FR' and wDescFr <> *blanks;
      wDesc = wDescFr;
    else;
      wDesc = wDescEn;
    endif;

    itemCount += 1;
    items(itemCount).sku   = wSku;
    items(itemCount).oh    = wOh;
    items(itemCount).avail = wAvail;
    items(itemCount).bulk  = wBulk;
    items(itemCount).sugg  = wSugg;
    items(itemCount).repl  = wRepl;

    rrn   += 1;
    iseq   = itemCount;
    isku   = wSku;
    idesc  = wDesc;
    idept  = wDept;
    iuom   = wUom;
    iimg   = wImg;
    ioh    = wOh;
    ialloc = wAlloc;
    iavail = wAvail;
    ibulk  = wBulk;
    irepl  = wRepl;
    isugg  = wSugg;
    icase  = wCase;
    write itmsfl;
  enddo;

  exec sql close itmCsr;
endsr;

//------------------------------------------------------------------------
begsr loadOperator;
  exec sql
    select full_name into :opername
      from gtoperator
     where operator_id = 'GT001' and active_flag = 'Y'
     fetch first row only;

  if sqlcode <> 0;
    opername = 'Demo Operator';
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
begsr msgNothingScanned;
  msgkind = 'WARN';
  if langpref = 'FR';
    msg = 'Scannez un autre emplacement, ou un article d''ici.';
  else;
    msg = 'Scan another location, or an item from this one.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgItemNotHere;
  msgkind = 'WARN';
  if langpref = 'FR';
    msg = %trim(r.sku) + ' ne se trouve pas dans ' + %trim(hloc) + '.';
  else;
    msg = %trim(r.sku) + ' is not in ' + %trim(hloc) + '.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgNoBulk;
  msgkind = 'WARN';
  if langpref = 'FR';
    msg = 'Aucun stock en vrac disponible pour ' + %trim(items(i).sku) + '.';
  else;
    msg = 'No bulk stock available for ' + %trim(items(i).sku) + '.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgUnknownScan;
  msgkind = 'BAD';
  if langpref = 'FR';
    msg = %trim(payload) + ' : ni emplacement, ni article.';
  else;
    msg = %trim(payload) + ' is not a location or an item.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgNoSuchLocation;
  msgkind = 'BAD';
  if langpref = 'FR';
    msg = 'Emplacement ' + %trim(hloc) + ' inconnu.';
  else;
    msg = 'Location ' + %trim(hloc) + ' is not on file.';
  endif;
endsr;
