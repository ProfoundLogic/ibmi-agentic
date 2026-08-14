**free

//////////////////////////////////////////////////////////////////////////
// GTPDR -- Putaway, confirm the destination and book it.
//
// The application SUGGESTS, the operator DECIDES. That distinction is the whole
// design of this screen:
//
//   - the suggested destination is filled in, with the reason it was chosen and
//     how much room is left there, so it can be accepted with one tap;
//   - the alternatives are shown, so the suggestion reads as a choice rather
//     than as an instruction from a system that cannot see the floor;
//   - any location can be scanned instead, and a scanned location wins.
//
// A putaway is ONE UNIT OF WORK. Stock leaving staging, stock arriving in the
// destination, the audit row and the pallet's status either all happen or none
// of them do -- otherwise a failure halfway through invents or destroys
// inventory. Same shape as the receipt post and the inventory move.
//
// The suggestion arithmetic is NOT here. It is in GTVPUTSUG, which the work
// list also reads, so this screen and that list cannot disagree about where a
// carton should go.
//////////////////////////////////////////////////////////////////////////

ctl-opt dftactgrp(*no) actgrp(*new);
ctl-opt bnddir('GTBAR');

dcl-f gtpdd workstn handler('PROFOUNDUI(HANDLER)');

exec sql set option commit = *chg, closqlcsr = *endmod;

/copy gtbar_pr.rpgle

dcl-pi *n;
  inSku   char(15) const;
  inStage char(10) const;
  inLang  char(2);
end-pi;

dcl-ds r likeds(gtbar_result_t);

dcl-s done    ind inz(*off);
dcl-s booked  ind inz(*off);
dcl-s payload varchar(120);

//  Work fields -- RNF0724 forbids dcl-s inside a begsr in the main procedure.
dcl-s wOper    varchar(18);
dcl-s wDescEn  varchar(60);
dcl-s wDescFr  varchar(60);
dcl-s wLoc     char(10);
dcl-s wTyp     char(4);
dcl-s wZone    char(4);
dcl-s wAct     char(1);
dcl-s wRoom    packed(11 : 2);
dcl-s wThere   packed(11 : 2);
dcl-s wRank    int(10);
dcl-s wAvail   packed(11 : 2);
dcl-s wLeft    packed(11 : 2);
dcl-s wJson    varchar(512);
dcl-s wPiece   varchar(120);
dcl-s wKey     char(10);
dcl-s wCap     packed(11 : 2);
dcl-s wUsed    packed(11 : 2);

//------------------------------------------------------------------------
// Main
//------------------------------------------------------------------------
psku     = inSku;
pfrom    = inStage;
langpref = inLang;

exsr loadOperator;
exsr loadItem;
exsr loadAvail;
exsr loadSuggestion;
exsr loadAlternatives;

//  Default the quantity to what will actually FIT in the chosen destination.
//
//  Defaulting to everything staged read well -- a whole pallet is the normal
//  case -- and was a trap: 864 shirts staged, a suggested pick face with room
//  for 36, and ONE TAP on "Put away" would have booked all 864 into it. The
//  default is now always bookable, and "All" is still one tap away for the
//  operator who means it.
exsr defaultQty;
pdone = 'N';

dow not done;
  action  = *blanks;
  scanval = *blanks;
  exfmt putdest;
  msg     = *blanks;
  msgkind = *blanks;

  select;
    when action = 'EXIT';
      done = *on;

    when action = 'LANGEN';
      langpref = 'EN';
      exsr saveLanguage;
      exsr loadItem;

    when action = 'LANGFR';
      langpref = 'FR';
      exsr saveLanguage;
      exsr loadItem;

    when action = 'SCAN';
      exsr doScanDest;

    when action = 'ALL';
      //  "Put it all away" -- everything staged, room or no room. A deliberate
      //  overfill is the operator's call; an accidental one is not.
      exsr loadAvail;
      pqty = pavail;

    when action = 'SUGG';
      //  Go back to what the application suggested, after an override.
      exsr loadSuggestion;
      exsr loadAlternatives;
      exsr defaultQty;

    when action = 'BOOK';
      exsr doPutaway;
      if booked;
        //  Nothing left staged here means the job is finished; go back to the
        //  list rather than sit on a screen with nothing to do.
        exsr loadAvail;
        if pavail <= 0;
          done = *on;
        else;
          //  Some still staged: re-suggest for what is LEFT and offer a
          //  quantity that fits it, the same as on entry.
          exsr loadSuggestion;
          exsr loadAlternatives;
          exsr defaultQty;
        endif;
      endif;

    when action = *blanks;
      //  Enter with nothing pressed. Redisplay.

    other;
  endsl;
enddo;

*inlr = *on;
return;

//------------------------------------------------------------------------
// A scanned destination. Any location can be scanned; the rules are only that
// it has to exist, be active, and not be another staging bay -- putting stock
// from staging into staging is not putaway, it is moving the problem.
//------------------------------------------------------------------------
begsr doScanDest;
  payload = %trim(scanval);

  if payload = *blanks;
    exsr msgScanSomething;
    leavesr;
  endif;

  clear r;
  gtbar_parse(payload : r);

  if r.locationId <> *blanks;
    wKey = r.locationId;
  else;
    //  A hand-keyed location has no symbology, so take the text as typed.
    wKey = %upper(%trim(payload));
  endif;

  wLoc = *blanks;
  exec sql
    select location_id, location_type, zone, active_flag
      into :wLoc, :wTyp, :wZone, :wAct
      from gtlocation
     where location_id = :wKey
     fetch first row only;

  if sqlcode <> 0 or wLoc = *blanks;
    exsr msgNotALocation;
    leavesr;
  endif;

  if wAct <> 'Y';
    exsr msgLocationInactive;
    leavesr;
  endif;

  if wTyp = 'STAG';
    exsr msgDestIsStaging;
    leavesr;
  endif;

  pto     = wLoc;
  ptotyp  = wTyp;
  ptozone = wZone;
  ptook   = 'Y';
  prank   = 0;

  exsr loadDestFigures;
  exsr defaultQty;
  exsr msgDestTaken;
endsr;

//------------------------------------------------------------------------
// The suggestion: the winner from the shared view.
//
// SORT_KEY carries the ordering -- does it FIT before where it BELONGS -- so
// this and the work list ask the identical question. Most room breaks a tie.
//------------------------------------------------------------------------
begsr loadSuggestion;
  pto     = *blanks;
  ptotyp  = *blanks;
  ptozone = *blanks;
  ptoroom = 0;
  ptoqty  = 0;
  prank   = 0;
  ptook   = 'N';

  exec sql
    select location_id, location_type, zone, room, qty_there, rank_no
      into :wLoc, :wTyp, :wZone, :wRoom, :wThere, :wRank
      from gtvputsug
     where sku = :psku
     order by sort_key, room desc
     fetch first row only;

  if sqlcode = 0;
    pto     = wLoc;
    ptotyp  = wTyp;
    ptozone = wZone;
    ptoroom = wRoom;
    ptoqty  = wThere;
    prank   = wRank;
  endif;
endsr;

//------------------------------------------------------------------------
// The alternatives, as a JSON string.
//
// A string rather than a second subfile: two subfiles in one EJS format do not
// compile -- the conversion emits malformed DDS and the display file is not
// created at all. This list is four rows and nobody types into it, which is
// exactly the case a string is for.
//------------------------------------------------------------------------
begsr loadAlternatives;
  wJson = '[';

  exec sql
    declare altCsr cursor for
      select location_id, location_type, zone, room, rank_no
        from gtvputsug
       where sku = :psku
       order by sort_key, room desc
       fetch first 5 rows only;

  exec sql open altCsr;

  dow *on;
    exec sql
      fetch altCsr into :wLoc, :wTyp, :wZone, :wRoom, :wRank;
    if sqlcode <> 0;
      leave;
    endif;

    wPiece = '{"l":"' + %trim(wLoc) + '"'
           + ',"t":"' + %trim(wTyp) + '"'
           + ',"z":"' + %trim(wZone) + '"'
           + ',"r":' + %char(%int(wRoom))
           + ',"k":' + %char(wRank) + '}';

    //  +2 for the separator now and the closing bracket later. Overrunning the
    //  varchar is RNX0100 on the operator's screen, not a compile error.
    if %len(wJson) + %len(wPiece) + 2 > 512;
      leave;
    endif;

    if %len(wJson) > 1;
      wJson += ',';
    endif;
    wJson += wPiece;
  enddo;

  exec sql close altCsr;

  wJson += ']';
  palts = wJson;
endsr;

//------------------------------------------------------------------------
// Room and current contents of whatever destination is now selected. Measured
// per type, because a pick face has a maximum for one SKU and a bulk bay has a
// capacity shared by everything standing in it.
//------------------------------------------------------------------------
begsr loadDestFigures;
  ptoroom = 0;
  ptoqty  = 0;

  exec sql
    select coalesce(qty_on_hand, 0) into :ptoqty
      from gtinvbal
     where sku = :psku and location_id = :pto
     fetch first row only;

  if sqlcode <> 0;
    ptoqty = 0;
  endif;

  if ptotyp = 'PICK';
    exec sql
      select coalesce(max_qty, 0) into :wCap
        from gtlocation
       where location_id = :pto
       fetch first row only;
    if sqlcode = 0;
      ptoroom = wCap - ptoqty;
    endif;
  else;
    exec sql
      select coalesce(capacity_units, 0),
             coalesce((select sum(x.qty_on_hand) from gtinvbal x
                        where x.location_id = :pto), 0)
        into :wCap, :wUsed
        from gtlocation
       where location_id = :pto
       fetch first row only;
    if sqlcode = 0;
      ptoroom = wCap - wUsed;
    endif;
  endif;

  if ptoroom < 0;
    ptoroom = 0;
  endif;
endsr;

//------------------------------------------------------------------------
// The transaction. One unit of work: stock out of staging, stock into the
// destination, the audit row, and the pallet's status.
//------------------------------------------------------------------------
begsr doPutaway;
  booked = *off;

  if pto = *blanks;
    exsr msgNoDest;
    leavesr;
  endif;

  if pqty <= 0;
    exsr msgQtyZero;
    leavesr;
  endif;

  //  Re-read what is actually staged rather than trusting what was displayed:
  //  somebody else may have put this stock away while the screen sat there.
  exsr loadAvail;

  if pqty > pavail;
    exsr msgQtyTooMuch;
    leavesr;
  endif;

  pfrombef = pavail;
  ptobef   = ptoqty;

  //  ---- 1. off the staging location -------------------------------
  exec sql
    update gtinvbal
       set qty_on_hand = qty_on_hand - :pqty,
           updated_by  = :wOper,
           updated_ts  = current timestamp
     where sku = :psku and location_id = :pfrom;

  if sqlcode < 0 or sqlcode = 100;
    exec sql rollback;
    exsr msgFailed;
    leavesr;
  endif;

  //  ---- 2. onto the destination -----------------------------------
  //  UPDATE then INSERT, and only insert when the update found nothing.
  //  Inserting first would double-count a location that already held it.
  exec sql
    update gtinvbal
       set qty_on_hand = qty_on_hand + :pqty,
           updated_by  = :wOper,
           updated_ts  = current timestamp
     where sku = :psku and location_id = :pto;

  if sqlcode < 0;
    exec sql rollback;
    exsr msgFailed;
    leavesr;
  endif;

  if sqlcode = 100;
    exec sql
      insert into gtinvbal (sku, location_id, qty_on_hand, qty_allocated,
                            updated_by)
      values (:psku, :pto, :pqty, 0, :wOper);

    if sqlcode < 0;
      exec sql rollback;
      exsr msgFailed;
      leavesr;
    endif;
  endif;

  //  ---- 3. the audit row ------------------------------------------
  //  source_doc stays blank: it is CHAR(10) and an SSCC is 18 digits, so the
  //  pallet cannot be recorded there without truncating it into something
  //  that looks like a different pallet. from_location carries the staging
  //  bay, which is what a putaway is actually about.
  exec sql
    insert into gtmovement (movement_type, sku, from_location, to_location,
                            qty, reason_code, source_doc, moved_by)
    values ('PUTA', :psku, :pfrom, :pto, :pqty, '', '', :wOper);

  if sqlcode < 0;
    exec sql rollback;
    exsr msgFailed;
    leavesr;
  endif;

  //  ---- 4. the pallet, once its staging bay is empty --------------
  //  Only when nothing is left standing there. Flipping the pallet while some
  //  of its stock is still staged would report the dock clear when it is not.
  exec sql
    select coalesce(sum(qty_on_hand), 0) into :wLeft
      from gtinvbal
     where location_id = :pfrom;

  if sqlcode = 0 and wLeft <= 0;
    exec sql
      update gtpallet
         set pallet_status = 'PUTA'
       where location_id = :pfrom and pallet_status = 'RECV';

    if sqlcode < 0;
      exec sql rollback;
      exsr msgFailed;
      leavesr;
    endif;
  endif;

  exec sql commit;

  //  A COMMIT can fail too. Checking it is the difference between "put away"
  //  and "believed to be put away".
  if sqlcode < 0;
    exec sql rollback;
    exsr msgFailed;
    leavesr;
  endif;

  booked   = *on;
  pdone    = 'Y';
  pfromaft = pfrombef - pqty;
  ptoaft   = ptobef + pqty;
  exsr msgBooked;
endsr;

//------------------------------------------------------------------------
// The quantity to offer: everything staged, capped at what the destination can
// actually take. A destination with no room recorded at all is not capped --
// zero capacity in the demo data would otherwise offer zero and look broken.
//------------------------------------------------------------------------
begsr defaultQty;
  pqty = pavail;

  if ptoroom > 0 and pqty > ptoroom;
    pqty = ptoroom;
  endif;
endsr;

//------------------------------------------------------------------------
begsr loadAvail;
  pavail = 0;
  exec sql
    select coalesce(qty_on_hand, 0) into :pavail
      from gtinvbal
     where sku = :psku and location_id = :pfrom
     fetch first row only;

  if sqlcode <> 0;
    pavail = 0;
  endif;
endsr;

//------------------------------------------------------------------------
begsr loadItem;
  exec sql
    select i.description_en, i.description_fr, i.department, i.uom,
           i.case_pack,
           coalesce((select min(v.image_id) from gtvitemimg v
                      where v.sku = i.sku and v.img_group = 0), 0)
      into :wDescEn, :wDescFr, :pdept, :puom, :pcase, :pimg
      from gtitem i
     where i.sku = :psku
     fetch first row only;

  if sqlcode <> 0;
    pdesc = psku;
  elseif langpref = 'FR' and wDescFr <> *blanks;
    pdesc = wDescFr;
  else;
    pdesc = wDescEn;
  endif;

  //  The pallet this stock arrived on, for the header. Blank when the stock
  //  was staged without one.
  psscc = *blanks;
  exec sql
    select coalesce(min(sscc), '') into :psscc
      from gtvputstg
     where sku = :psku and stage_location = :pfrom;
endsr;

//------------------------------------------------------------------------
begsr loadOperator;
  exec sql
    select full_name, operator_id
      into :opername, :wOper
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
// Messages.
//------------------------------------------------------------------------
begsr msgScanSomething;
  msgkind = 'WARN';
  if langpref = 'FR';
    msg = 'Scannez un emplacement de destination.';
  else;
    msg = 'Scan a destination location.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgNotALocation;
  msgkind = 'BAD';
  if langpref = 'FR';
    msg = %trim(wKey) + ' : emplacement inconnu.';
  else;
    msg = %trim(wKey) + ' is not a known location.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgLocationInactive;
  msgkind = 'BAD';
  if langpref = 'FR';
    msg = 'Emplacement ' + %trim(wLoc) + ' inactif.';
  else;
    msg = 'Location ' + %trim(wLoc) + ' is not active.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgDestIsStaging;
  msgkind = 'WARN';
  if langpref = 'FR';
    msg = %trim(wLoc) + ' est une zone de réception, pas un stockage.';
  else;
    msg = %trim(wLoc) + ' is staging, not storage.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgDestTaken;
  msgkind = 'GOOD';
  if langpref = 'FR';
    msg = 'Destination ' + %trim(pto) + ' retenue.';
  else;
    msg = 'Destination set to ' + %trim(pto) + '.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgNoDest;
  msgkind = 'WARN';
  if langpref = 'FR';
    msg = 'Aucune destination. Scannez ou choisissez un emplacement.';
  else;
    msg = 'No destination. Scan or choose a location.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgQtyZero;
  msgkind = 'WARN';
  if langpref = 'FR';
    msg = 'Indiquez une quantité supérieure à zéro.';
  else;
    msg = 'Enter a quantity greater than zero.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgQtyTooMuch;
  msgkind = 'BAD';
  if langpref = 'FR';
    msg = 'Seulement ' + %char(%int(pavail)) + ' en zone de réception.';
  else;
    msg = 'Only ' + %char(%int(pavail)) + ' staged.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgBooked;
  msgkind = 'GOOD';
  if langpref = 'FR';
    msg = %char(%int(pqty)) + ' rangé en ' + %trim(pto) + '.';
  else;
    msg = %char(%int(pqty)) + ' put away in ' + %trim(pto) + '.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgFailed;
  msgkind = 'BAD';
  if langpref = 'FR';
    msg = 'Rangement annulé. Rien n’a été modifié.';
  else;
    msg = 'Putaway rolled back. Nothing was changed.';
  endif;
endsr;
