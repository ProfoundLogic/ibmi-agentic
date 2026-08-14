**free

//////////////////////////////////////////////////////////////////////////
// GTINHR -- Inventory & Movement, home screen.
//
// "Scan a barcode on an inventory location, show me the on-hand balance in
// that location and the available balance in bulk inventory, with the option
// to do an inventory movement."
//
// ONE SCAN BOX, SEVERAL MEANINGS (design doc §8.2). Rather than making the
// operator answer "am I scanning a location or an item?", there is one scan
// target and GTBAR classifies what arrived:
//
//   a location label -> GTINLR, what is in this location
//   an item barcode  -> the Item Lookup DETAIL screen that already exists
//   a pallet SSCC    -> the location that pallet is sitting in
//
// The item branch calls GTITDR rather than a second screen of its own. Item
// Lookup already shows the image carousel, the facts and every location the
// item is in -- which is exactly what "where is this item" means. A separate
// invitm screen would have been the same query and the same layout under a
// different name.
//
// The list underneath is REPLENISHMENT ALERTS, not the "recent locations" of
// the original sketch. Recent locations tell an operator where they have just
// been; alerts tell them where the work is, they are the same 51 the menu
// badge already counts, and tapping one goes straight to the location with the
// move pre-filled. Actionable beats historical.
//////////////////////////////////////////////////////////////////////////

ctl-opt dftactgrp(*no) actgrp(*new);
ctl-opt bnddir('GTBAR');

dcl-f gtinhd workstn sfile(alrsfl : rrn) handler('PROFOUNDUI(HANDLER)');

exec sql set option commit = *chg, closqlcsr = *endmod;

/copy gtbar_pr.rpgle

dcl-pr gtinlr extpgm;
  location char(10) const;
  lang     char(2);
end-pr;

//  The Item Lookup detail screen. Its third parameter is how it reports the
//  way the operator left it, which this screen does not need to act on.
dcl-pr gtitdr extpgm;
  sku    char(15) const;
  lang   char(2);
  outAct char(10);
end-pr;

dcl-ds r likeds(gtbar_result_t);

dcl-c MAX_ROWS 40;

dcl-s rrn      int(10) inz(0);
dcl-s i        int(10);
dcl-s done     ind inz(*off);
dcl-s payload  varchar(120);
dcl-s curLoc   char(10);
dcl-s curSku   char(15);
dcl-s detlAct  char(10);

//  The alert list, held between round trips so a tap resolves to a location
//  without re-querying.
dcl-ds alerts qualified dim(MAX_ROWS);
  loc    char(10);
  sku    char(15);
end-ds;
dcl-s alertCount int(10) inz(0);

//  Work fields -- RNF0724 forbids dcl-s inside a begsr in the main procedure.
dcl-s wLoc   char(10);
dcl-s wZone  char(4);
dcl-s wSku   char(15);
dcl-s wDesc  varchar(60);
dcl-s wDescEn varchar(60);
dcl-s wDescFr varchar(60);
dcl-s wImg   int(10);
dcl-s wOh    packed(11 : 2);
dcl-s wMin   packed(11 : 2);
dcl-s wSugg  packed(11 : 2);
dcl-s wBulk  packed(11 : 2);
dcl-s wKey   char(10);
dcl-s wCount int(10);

//------------------------------------------------------------------------
// Main
//------------------------------------------------------------------------
exsr loadOperator;

dow not done;
  exsr loadAlertSubfile;
  action  = *blanks;
  scanval = *blanks;
  exfmt invhome;
  msg     = *blanks;
  msgkind = *blanks;

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
      //  PICKnn -- the operator tapped an alert card.
      i = %int(%subst(action : 5));
      if i >= 1 and i <= alertCount;
        curLoc = alerts(i).loc;
        gtinlr(curLoc : langpref);
      endif;

    when action = *blanks;
      //  Enter with nothing pressed. Redisplay.

    other;
  endsl;
enddo;

*inlr = *on;
return;

//------------------------------------------------------------------------
// The scan. GTBAR classifies; what it MEANS is decided here, because the
// service program identifies barcodes and knows nothing about screens.
//
// Order matters: most specific interpretation first.
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
    //  ---- a location label ----------------------------------------
    when r.locationId <> *blanks;
      curLoc = r.locationId;
      gtinlr(curLoc : langpref);

    //  ---- a pallet: show the location it is standing in -----------
    when r.sscc <> *blanks;
      wLoc = *blanks;
      exec sql
        select location_id into :wLoc
          from gtpallet
         where sscc = :r.sscc
         fetch first row only;

      if sqlcode = 0 and wLoc <> *blanks;
        curLoc = wLoc;
        gtinlr(curLoc : langpref);
      else;
        exsr msgPalletNowhere;
      endif;

    //  ---- an item: straight to the Item Lookup detail screen ------
    when r.sku <> *blanks;
      curSku  = r.sku;
      detlAct = *blanks;
      gtitdr(curSku : langpref : detlAct);
      //  GTITDR reports MENU when the operator asked for the menu rather
      //  than to come back here. Honour it.
      if detlAct = 'MENU';
        done = *on;
      endif;

    //  ---- not a location, an item or a pallet. Try it as a plain
    //       location id, because a hand-keyed A01041 has no symbology.
    other;
      wKey = %upper(%trim(payload));
      exec sql
        select location_id into :wLoc
          from gtlocation
         where location_id = :wKey
         fetch first row only;

      if sqlcode = 0;
        curLoc = wLoc;
        gtinlr(curLoc : langpref);
      else;
        exsr msgUnknownScan;
      endif;
  endsl;
endsr;

//------------------------------------------------------------------------
// Replenishment alerts: pick faces below minimum with bulk stock to pull.
// GTVREPLEN owns that condition, so this screen and the Supervisor View
// cannot disagree about what counts as an alert.
//------------------------------------------------------------------------
begsr loadAlertSubfile;
  rrn = 0;
  sflclear = '1';
  write invhome;
  sflclear = '0';
  alertCount = 0;

  exec sql
    declare alrCsr cursor for
      select p.location_id, p.zone, p.sku,
             i.description_en, i.description_fr,
             coalesce((select min(v.image_id) from gtvitemimg v
                        where v.sku = p.sku and v.img_group = 0), 0),
             p.qty_on_hand, p.min_qty, p.qty_suggested, p.qty_bulk_avail
        from gtvreplen p
        join gtitem i on i.sku = p.sku
       order by p.qty_on_hand - p.min_qty, p.location_id
       fetch first 40 rows only;

  exec sql open alrCsr;

  dow alertCount < MAX_ROWS;
    exec sql
      fetch alrCsr into :wLoc, :wZone, :wSku, :wDescEn, :wDescFr,
                        :wImg, :wOh, :wMin, :wSugg, :wBulk;
    if sqlcode <> 0;
      leave;
    endif;

    if langpref = 'FR' and wDescFr <> *blanks;
      wDesc = wDescFr;
    else;
      wDesc = wDescEn;
    endif;

    alertCount += 1;
    alerts(alertCount).loc = wLoc;
    alerts(alertCount).sku = wSku;

    rrn  += 1;
    aseq  = alertCount;
    aloc  = wLoc;
    azone = wZone;
    asku  = wSku;
    adesc = wDesc;
    aimg  = wImg;
    aoh   = wOh;
    amin  = wMin;
    asugg = wSugg;
    abulk = wBulk;
    write alrsfl;
  enddo;

  exec sql close alrCsr;

  //  The count is the REAL total, not the 40 shown, so the screen never
  //  claims there are 40 alerts when there are 51.
  exec sql
    select count(*) into :wCount from gtvreplen;
  if sqlcode = 0;
    nalerts = wCount;
  else;
    nalerts = alertCount;
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
// Messages. Specific about what happened -- "not found" is no use to
// somebody holding a scanner in front of a rack.
//------------------------------------------------------------------------
begsr msgNothingScanned;
  msgkind = 'WARN';
  if langpref = 'FR';
    msg = 'Scannez un emplacement, un article ou une palette.';
  else;
    msg = 'Scan a location, an item or a pallet.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgPalletNowhere;
  msgkind = 'WARN';
  if langpref = 'FR';
    msg = 'Palette ' + %trim(r.sscc) + ' : aucun emplacement enregistré.';
  else;
    msg = 'Pallet ' + %trim(r.sscc) + ' is not in a recorded location.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgUnknownScan;
  msgkind = 'BAD';
  if langpref = 'FR';
    msg = %trim(payload) + ' : ni emplacement, ni article, ni palette.';
  else;
    msg = %trim(payload) + ' is not a location, an item or a pallet.';
  endif;
endsr;
