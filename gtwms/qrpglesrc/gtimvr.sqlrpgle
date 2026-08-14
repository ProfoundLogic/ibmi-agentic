**free

//////////////////////////////////////////////////////////////////////////
// GTIMVR -- Inventory movement.
//
// FROM is prefilled and locked -- the operator is standing at it, and letting
// them change it is how stock ends up moved out of the wrong rack. The TO
// location is SCANNED, which is the point: a keyed location is a typo waiting
// to happen and a scanned one is proof the operator was actually there.
//
// Everything is pre-fillable, so one program serves both entry paths:
//   a plain move   -- FROM and SKU supplied, TO and quantity blank
//   a replenishment -- all four supplied, reason REPL, operator just confirms
//
// THE MOVE IS ONE UNIT OF WORK: decrement the from-balance, increment (or
// create) the to-balance, write the audit row. Any failure rolls all three
// back. A half-applied move is stock that exists in two places or none.
//
// VALIDATION HAPPENS BEFORE THE COMMIT BOUNDARY OPENS, not inside it, so a
// rejected move never starts a transaction it has to unwind:
//   - the to-location must exist and be active
//   - it must not be the from-location
//   - the quantity must be positive, and within on-hand less allocated
//////////////////////////////////////////////////////////////////////////

ctl-opt dftactgrp(*no) actgrp(*new);
ctl-opt bnddir('GTBAR');

dcl-f gtimvd workstn sfile(rsnsfl : rrn) handler('PROFOUNDUI(HANDLER)');

exec sql set option commit = *chg, closqlcsr = *endmod;

/copy gtbar_pr.rpgle

dcl-pi *n;
  inFrom   char(10) const;
  inSku    char(15) const;
  inTo     char(10) const;
  inQty    packed(11 : 2) const;
  inReason char(4) const;
  inLang   char(2);
end-pi;

dcl-ds r likeds(gtbar_result_t);

dcl-c MAX_RSN 12;

dcl-s rrn      int(10) inz(0);
dcl-s i        int(10);
dcl-s done     ind inz(*off);
dcl-s payload  varchar(120);
dcl-s moved    ind inz(*off);

dcl-ds reasons qualified dim(MAX_RSN);
  code char(4);
end-ds;
dcl-s rsnCount int(10) inz(0);

//  Work fields.
dcl-s wCode   char(4);
dcl-s wDesc   varchar(40);
dcl-s wDescEn varchar(40);
dcl-s wDescFr varchar(40);
dcl-s wLoc    char(10);
dcl-s wType   char(4);
dcl-s wZone   char(4);
dcl-s wAct    char(1);
dcl-s wQty    packed(11 : 2);
dcl-s wOh     packed(11 : 2);
dcl-s wAlloc  packed(11 : 2);
dcl-s wExists int(10);
dcl-s wOper   varchar(18);
dcl-s wDescEnL varchar(60);
dcl-s wDescFrL varchar(60);

//------------------------------------------------------------------------
// Main
//------------------------------------------------------------------------
langpref = inLang;
mfrom    = inFrom;
msku     = inSku;
mto      = inTo;
mqty     = inQty;
mreason  = inReason;
mdone    = 'N';
mtook    = 'N';

exsr loadOperator;
exsr loadItem;
exsr loadFromBalance;

//  A pre-filled TO (the replenish path) is validated straight away so the
//  operator sees its zone and current contents before confirming.
if mto <> *blanks;
  exsr checkTo;
endif;

//  Default reason: a replenishment says so, anything else the operator picks.
if mreason = *blanks;
  mreason = 'RELO';
endif;

dow not done;
  exsr loadReasonSubfile;
  action  = *blanks;
  scanval = *blanks;
  exfmt invmove;
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
      //  Scanning here means the TO location.
      exsr doScanTo;

    when action = 'MOVE';
      exsr doMove;

    when action = 'AGAIN';
      //  Move something else out of the same location.
      mdone = 'N';
      mto   = *blanks;
      mtook = 'N';
      mqty  = 0;
      exsr loadFromBalance;

    when action = *blanks;
      //  Enter with nothing pressed. Redisplay.

    other;
  endsl;
enddo;

*inlr = *on;
return;

//------------------------------------------------------------------------
// The TO location, scanned. A keyed value is accepted too, because a label
// that has been run over by a forklift still has to be dealt with.
//------------------------------------------------------------------------
begsr doScanTo;
  payload = %trim(scanval);

  if payload = *blanks;
    exsr msgScanTo;
    leavesr;
  endif;

  clear r;
  gtbar_parse(payload : r);

  if r.locationId <> *blanks;
    mto = r.locationId;
  else;
    //  %trim, not %subst(payload : 1 : 10): a fixed-length substring is RNX0100
    //  when the payload is shorter, and a location id is six characters. Same
    //  defect as gtcnhr, found in the same audit.
    mto = %upper(%trim(payload));
  endif;

  exsr checkTo;
endsr;

//------------------------------------------------------------------------
// Validate the destination and show what is already in it. Sets mtook.
//------------------------------------------------------------------------
begsr checkTo;
  mtook    = 'N';
  mtotyp   = *blanks;
  mtozon   = *blanks;
  mtoqty   = 0;

  if mto = *blanks;
    leavesr;
  endif;

  exec sql
    select location_type, zone, active_flag
      into :wType, :wZone, :wAct
      from gtlocation
     where location_id = :mto
     fetch first row only;

  if sqlcode <> 0;
    exsr msgToUnknown;
    mto = *blanks;
    leavesr;
  endif;

  if wAct <> 'Y';
    exsr msgToInactive;
    mto = *blanks;
    leavesr;
  endif;

  if mto = mfrom;
    exsr msgToSameAsFrom;
    mto = *blanks;
    leavesr;
  endif;

  mtotyp = wType;
  mtozon = wZone;

  //  What this item already has in the destination -- so the operator can see
  //  they are topping up rather than creating a second home for it.
  exec sql
    select qty_on_hand into :mtoqty
      from gtinvbal
     where sku = :msku and location_id = :mto
     fetch first row only;
  if sqlcode <> 0;
    mtoqty = 0;
  endif;

  mtook = 'Y';
  exsr msgToOk;
endsr;

//------------------------------------------------------------------------
// THE MOVE. Validate first, then one commit boundary around all three
// statements.
//------------------------------------------------------------------------
begsr doMove;
  moved = *off;

  if mtook <> 'Y' or mto = *blanks;
    exsr msgScanTo;
    leavesr;
  endif;

  if mqty <= 0;
    exsr msgQtyZero;
    leavesr;
  endif;

  //  Re-read the balance rather than trusting what was displayed: somebody
  //  else may have moved this stock while the screen sat there.
  exsr loadFromBalance;

  if mqty > mavail;
    exsr msgQtyTooMuch;
    leavesr;
  endif;

  mfrombef = monhand;
  mtobef   = mtoqty;

  //  ---- 1. take it off the from-location --------------------------
  exec sql
    update gtinvbal
       set qty_on_hand = qty_on_hand - :mqty,
           updated_by  = :wOper,
           updated_ts  = current timestamp
     where sku = :msku and location_id = :mfrom;

  if sqlcode < 0 or sqlcode = 100;
    exec sql rollback;
    exsr msgMoveFailed;
    leavesr;
  endif;

  //  ---- 2. put it on the to-location ------------------------------
  //  UPDATE then INSERT, in that order, and only insert when the update
  //  found nothing -- the same shape the receipt post uses. Inserting first
  //  would double-count a location that already held the item.
  exec sql
    update gtinvbal
       set qty_on_hand = qty_on_hand + :mqty,
           updated_by  = :wOper,
           updated_ts  = current timestamp
     where sku = :msku and location_id = :mto;

  if sqlcode < 0;
    exec sql rollback;
    exsr msgMoveFailed;
    leavesr;
  endif;

  if sqlcode = 100;
    exec sql
      insert into gtinvbal (sku, location_id, qty_on_hand, qty_allocated,
                            updated_by)
      values (:msku, :mto, :mqty, 0, :wOper);

    if sqlcode < 0;
      exec sql rollback;
      exsr msgMoveFailed;
      leavesr;
    endif;
  endif;

  //  ---- 3. the audit row ------------------------------------------
  exec sql
    insert into gtmovement (movement_type, sku, from_location, to_location,
                            qty, reason_code, source_doc, moved_by)
    values ('MOVE', :msku, :mfrom, :mto, :mqty, :mreason, '', :wOper);

  if sqlcode < 0;
    exec sql rollback;
    exsr msgMoveFailed;
    leavesr;
  endif;

  exec sql commit;

  //  A COMMIT can fail too. Checking it is the difference between "moved"
  //  and "believed to be moved".
  if sqlcode < 0;
    exec sql rollback;
    exsr msgMoveFailed;
    leavesr;
  endif;

  moved    = *on;
  mdone    = 'Y';
  mfromaft = mfrombef - mqty;
  mtoaft   = mtobef + mqty;
  exsr msgMoved;

  //  Refresh so "move again" starts from the truth.
  exsr loadFromBalance;
endsr;

//------------------------------------------------------------------------
begsr loadItem;
  exec sql
    select i.description_en, i.description_fr, i.uom, i.case_pack,
           coalesce((select min(v.image_id) from gtvitemimg v
                      where v.sku = i.sku and v.img_group = 0), 0)
      into :wDescEnL, :wDescFrL, :muom, :mcase, :mimg
      from gtitem i
     where i.sku = :msku
     fetch first row only;

  if sqlcode <> 0;
    mdesc = msku;
  elseif langpref = 'FR' and wDescFrL <> *blanks;
    mdesc = wDescFrL;
  else;
    mdesc = wDescEnL;
  endif;

  exec sql
    select location_type, zone into :mfromtyp, :mfromzon
      from gtlocation
     where location_id = :mfrom
     fetch first row only;
endsr;

//------------------------------------------------------------------------
begsr loadFromBalance;
  monhand = 0;
  mavail  = 0;

  exec sql
    select qty_on_hand, qty_allocated
      into :wOh, :wAlloc
      from gtinvbal
     where sku = :msku and location_id = :mfrom
     fetch first row only;

  if sqlcode = 0;
    monhand = wOh;
    mavail  = wOh - wAlloc;
    if mavail < 0;
      mavail = 0;
    endif;
  endif;

  //  A quantity nobody has set yet defaults to everything that can move --
  //  the common case is "take the whole lot to the new home".
  if mqty <= 0 and mdone <> 'Y';
    mqty = mavail;
  endif;

  //  And never offer more than is actually there.
  if mqty > mavail;
    mqty = mavail;
  endif;
endsr;

//------------------------------------------------------------------------
// Move reason codes, bilingual, from GTREASON. Not hardcoded here: the table
// owns them and the count screen will use the same mechanism.
//------------------------------------------------------------------------
begsr loadReasonSubfile;
  rrn = 0;
  sflclear = '1';
  write invmove;
  sflclear = '0';
  rsnCount = 0;

  exec sql
    declare rsnCsr cursor for
      select reason_code, reason_desc_en, reason_desc_fr
        from gtreason
       where reason_type = 'MOVE'
       order by sort_order, reason_code
       fetch first 12 rows only;

  exec sql open rsnCsr;

  dow rsnCount < MAX_RSN;
    exec sql fetch rsnCsr into :wCode, :wDescEn, :wDescFr;
    if sqlcode <> 0;
      leave;
    endif;

    if langpref = 'FR' and wDescFr <> *blanks;
      wDesc = wDescFr;
    else;
      wDesc = wDescEn;
    endif;

    rsnCount += 1;
    reasons(rsnCount).code = wCode;

    rrn  += 1;
    rseq  = rsnCount;
    rcode = wCode;
    rdesc = wDesc;
    write rsnsfl;
  enddo;

  exec sql close rsnCsr;
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
  //  The item description and the reason list are language-dependent.
  exsr loadItem;
endsr;

//------------------------------------------------------------------------
// Messages
//------------------------------------------------------------------------
begsr msgScanTo;
  msgkind = 'WARN';
  if langpref = 'FR';
    msg = 'Scannez l''emplacement de destination.';
  else;
    msg = 'Scan the destination location.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgToOk;
  msgkind = 'GOOD';
  if langpref = 'FR';
    msg = 'Destination ' + %trim(mto) + ' (' + %trim(mtozon) + ') confirmée.';
  else;
    msg = 'Destination ' + %trim(mto) + ' (' + %trim(mtozon) + ') confirmed.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgToUnknown;
  msgkind = 'BAD';
  if langpref = 'FR';
    msg = 'Emplacement ' + %trim(mto) + ' inconnu.';
  else;
    msg = 'Location ' + %trim(mto) + ' is not on file.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgToInactive;
  msgkind = 'BAD';
  if langpref = 'FR';
    msg = 'Emplacement ' + %trim(mto) + ' inactif.';
  else;
    msg = 'Location ' + %trim(mto) + ' is not active.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgToSameAsFrom;
  msgkind = 'BAD';
  if langpref = 'FR';
    msg = 'La destination doit différer de l''origine.';
  else;
    msg = 'The destination must be a different location.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgQtyZero;
  msgkind = 'WARN';
  if langpref = 'FR';
    msg = 'Indiquez une quantité à déplacer.';
  else;
    msg = 'Enter a quantity to move.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgQtyTooMuch;
  msgkind = 'BAD';
  if langpref = 'FR';
    msg = 'Seulement ' + %trim(%editc(mavail : '3')) + ' disponible ici.';
  else;
    msg = 'Only ' + %trim(%editc(mavail : '3')) + ' available here.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgMoved;
  msgkind = 'GOOD';
  if langpref = 'FR';
    msg = %trim(%editc(mqty : '3')) + ' déplacé de ' + %trim(mfrom) +
          ' vers ' + %trim(mto) + '.';
  else;
    msg = %trim(%editc(mqty : '3')) + ' moved from ' + %trim(mfrom) +
          ' to ' + %trim(mto) + '.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgMoveFailed;
  msgkind = 'BAD';
  if langpref = 'FR';
    msg = 'Déplacement annulé, rien changé (SQL ' +
          %trim(%char(sqlcode)) + ').';
  else;
    msg = 'Move rolled back, nothing changed (SQL ' +
          %trim(%char(sqlcode)) + ').';
  endif;
endsr;
