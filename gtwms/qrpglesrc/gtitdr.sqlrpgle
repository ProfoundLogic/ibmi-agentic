**free

//////////////////////////////////////////////////////////////////////////
// GTITDR -- Item Lookup, detail screen.
//
// A SEPARATE PROGRAM with its OWN display file carrying ONE format.
//
// It started life as a second format inside GTITMD, and that was wrong: with
// two formats in one display file both stayed rendered at once, so the detail
// screen sat on top of the search screen. "Scan another" looked dead and the
// search screen's own footer was unreachable -- only F3 worked, because that
// goes straight to the program rather than through a button.
//
// One format per display file, and a called program for the next screen, is
// the pattern the existing cfdemo screens in this repo already use.
//
// CAMERA CAPTURE
// --------------
// The operator can add photographs of the actual stock from the device
// camera. They arrive base64-encoded in IMGDATA, go to GTIMG which stores the
// BLOB and publishes the servable file, and then the whole detail is simply
// re-read -- so the new photograph comes back through GTVITEMIMG on the very
// same round trip and appears in the carousel beside the catalogue imagery.
// No special case in the render path: to the screen it is just another image.
//////////////////////////////////////////////////////////////////////////

ctl-opt dftactgrp(*no) actgrp(*new);
ctl-opt bnddir('GTIMG');

dcl-f gtitdd workstn sfile(locsfl : rrn2) handler('PROFOUNDUI(HANDLER)');

exec sql set option commit = *chg, closqlcsr = *endmod;

/copy gtimg_pr.rpgle

dcl-pi *n;
  inSku   char(15) const;
  inLang  char(2);
  outAct  char(10);
end-pi;

dcl-c MAX_ROWS 60;

//  Carousel slots. Catalogue imagery runs to two per family today, so eight
//  leaves real room for operator photographs before anything is dropped.
dcl-c MAX_IMGS 8;

dcl-s rrn2     int(10) inz(0);
dcl-s i        int(10);
dcl-s done     ind inz(*off);
dcl-s curSku   char(15);

dcl-ds locs qualified dim(MAX_ROWS);
  loc    char(10);
  ltype  char(4);
  zone   char(4);
  qty    packed(11 : 2);
  alloc  packed(11 : 2);
end-ds;
dcl-s locCount int(10) inz(0);

dcl-s wAlloc packed(11 : 2);
dcl-s wCap   varchar(60);
dcl-s wImgId int(10);
dcl-s wSrc   char(1);
dcl-s newImg int(10);
dcl-s capEn  varchar(60);
dcl-s capFr  varchar(60);
dcl-s today  varchar(10);
dcl-s wLoc   char(10);
dcl-s wQty   packed(11 : 2);
dcl-s wType  char(4);
dcl-s wZone  char(4);

//------------------------------------------------------------------------
curSku = inSku;
langpref = inLang;
outAct = *blanks;

exsr loadOperator;
exsr loadDetail;

dow not done;
  exsr loadLocSubfile;
  action = *blanks;
  exfmt itmdetl;
  msg = *blanks;

  select;
    when action = 'BACK';
      done = *on;

    when action = 'MENU';
      // Straight out of Item Lookup entirely. The caller reads outAct and
      // ends too, so the operator lands back on the main menu.
      outAct = 'MENU';
      done = *on;

    when action = 'ADDIMG';
      exsr addPhoto;

    when action = 'LANGEN';
      langpref = 'EN';
      exsr saveLanguage;
      exsr loadDetail;

    when action = 'LANGFR';
      langpref = 'FR';
      exsr saveLanguage;
      exsr loadDetail;

    other;
  endsl;
enddo;

inLang = langpref;
*inlr = *on;
return;

//------------------------------------------------------------------------
begsr loadDetail;
  exec sql
    select i.sku,
           case when :langpref = 'FR' then i.description_fr
                else i.description_en end,
           i.department, i.uom, i.case_pack, i.weight_kg,
           i.length_cm, i.width_cm, i.height_cm, i.retail_price
      into :dsku, :ddesc, :ddept, :duom, :dcasepk, :dweight,
           :dlen, :dwid, :dhgt, :dretail
      from gtitem i
     where i.sku = :curSku
     fetch first row only;

  dupc = *blanks;
  dcase = *blanks;
  exec sql
    select barcode into :dupc from gtbarcode
     where sku = :curSku and barcode_level = 'EA'
     order by barcode fetch first row only;
  exec sql
    select barcode into :dcase from gtbarcode
     where sku = :curSku and barcode_level = 'CS'
     order by barcode fetch first row only;

  exec sql
    select coalesce(sum(qty_on_hand), 0),
           coalesce(sum(qty_on_hand - qty_allocated), 0),
           count(*)
      into :dtotoh, :dtotav, :dnlocs
      from gtinvbal where sku = :curSku;

  dbulk = 0;
  exec sql
    select coalesce(qty_bulk_avail, 0) into :dbulk
      from gtvbulk where sku = :curSku;

  for i = 1 to MAX_IMGS;
    exsr clearSlot;
  endfor;
  i = 0;

  //  IMG_GROUP puts the catalogue shot first and the operator's photographs
  //  after it, so the carousel always opens on the clean product image.
  exec sql
    declare cimg cursor for
      select v.image_id,
             case when :langpref = 'FR' then v.caption_fr
                  else v.caption_en end,
             v.img_source
        from gtvitemimg v
       where v.sku = :curSku
       order by v.img_group, v.seq_no, v.image_id
       fetch first 8 rows only;

  exec sql open cimg;
  exec sql fetch cimg into :wImgId, :wCap, :wSrc;
  dow sqlcode = 0 and i < MAX_IMGS;
    i += 1;
    exsr fillSlot;
    exec sql fetch cimg into :wImgId, :wCap, :wSrc;
  enddo;
  exec sql close cimg;

  locCount = 0;
  exec sql
    declare cloc cursor for
      select b.location_id, l.location_type, l.zone,
             b.qty_on_hand, b.qty_allocated
        from gtinvbal b
        join gtlocation l on l.location_id = b.location_id
       where b.sku = :curSku
       order by l.location_type, b.location_id
       fetch first 60 rows only;

  exec sql open cloc;
  exec sql fetch cloc into :wLoc, :wType, :wZone, :wQty, :wAlloc;
  dow sqlcode = 0 and locCount < MAX_ROWS;
    locCount += 1;
    locs(locCount).loc   = wLoc;
    locs(locCount).ltype = wType;
    locs(locCount).zone  = wZone;
    locs(locCount).qty   = wQty;
    locs(locCount).alloc = wAlloc;
    exec sql fetch cloc into :wLoc, :wType, :wZone, :wQty, :wAlloc;
  enddo;
  exec sql close cloc;
endsr;

//------------------------------------------------------------------------
// Slot assignment. The display file exposes eight discrete fields rather than
// a second subfile, because a carousel is a small fixed list and a subfile
// would cost another record format for no gain -- but that does mean the
// mapping has to be spelled out. It is spelled out ONCE, here.
//------------------------------------------------------------------------
begsr clearSlot;
  select;
    when i = 1;
      dimg1 = 0;
      dcap1 = *blanks;
      dsrc1 = *blanks;
    when i = 2;
      dimg2 = 0;
      dcap2 = *blanks;
      dsrc2 = *blanks;
    when i = 3;
      dimg3 = 0;
      dcap3 = *blanks;
      dsrc3 = *blanks;
    when i = 4;
      dimg4 = 0;
      dcap4 = *blanks;
      dsrc4 = *blanks;
    when i = 5;
      dimg5 = 0;
      dcap5 = *blanks;
      dsrc5 = *blanks;
    when i = 6;
      dimg6 = 0;
      dcap6 = *blanks;
      dsrc6 = *blanks;
    when i = 7;
      dimg7 = 0;
      dcap7 = *blanks;
      dsrc7 = *blanks;
    when i = 8;
      dimg8 = 0;
      dcap8 = *blanks;
      dsrc8 = *blanks;
  endsl;
endsr;

//------------------------------------------------------------------------
begsr fillSlot;
  select;
    when i = 1;
      dimg1 = wImgId;
      dcap1 = wCap;
      dsrc1 = wSrc;
    when i = 2;
      dimg2 = wImgId;
      dcap2 = wCap;
      dsrc2 = wSrc;
    when i = 3;
      dimg3 = wImgId;
      dcap3 = wCap;
      dsrc3 = wSrc;
    when i = 4;
      dimg4 = wImgId;
      dcap4 = wCap;
      dsrc4 = wSrc;
    when i = 5;
      dimg5 = wImgId;
      dcap5 = wCap;
      dsrc5 = wSrc;
    when i = 6;
      dimg6 = wImgId;
      dcap6 = wCap;
      dsrc6 = wSrc;
    when i = 7;
      dimg7 = wImgId;
      dcap7 = wCap;
      dsrc7 = wSrc;
    when i = 8;
      dimg8 = wImgId;
      dcap8 = wCap;
      dsrc8 = wSrc;
  endsl;
endsr;

//------------------------------------------------------------------------
// addPhoto -- store what the camera sent, then reload.
//
// IMGDATA is cleared unconditionally. It is a 24 KB field and leaving it
// populated would ship the whole photograph back to the browser on every
// subsequent screen write, and re-store it on the next unrelated action.
//------------------------------------------------------------------------
begsr addPhoto;
  if %len(%trim(imgdata)) < 64;
    //  Nothing usable arrived. Far more likely a truncated payload than a
    //  genuinely tiny photograph, so say so rather than storing a stub.
    if langpref = 'FR';
      msg = 'Aucune photo reçue. Veuillez réessayer.';
    else;
      msg = 'No photo was received. Please try again.';
    endif;
  else;
    today = %char(%date());
    capEn = 'Warehouse photo ' + today;
    capFr = 'Photo d''entrepôt ' + today;

    newImg = gtimg_add('ITEM' : curSku : %trim(imgdata) : capEn : capFr);

    if newImg > 0;
      if langpref = 'FR';
        msg = 'Photo ajoutée à cet article.';
      else;
        msg = 'Photo added to this item.';
      endif;
    else;
      if langpref = 'FR';
        msg = 'Échec de l''enregistrement de la photo.';
      else;
        msg = 'The photo could not be saved.';
      endif;
    endif;
  endif;

  imgdata = *blanks;
  exsr loadDetail;
endsr;

//------------------------------------------------------------------------
begsr loadLocSubfile;
  rrn2 = 0;
  sflclr2 = '1';
  write itmdetl;
  sflclr2 = '0';

  for i = 1 to locCount;
    lseq   = i;
    lloc   = locs(i).loc;
    ltype  = locs(i).ltype;
    lzone  = locs(i).zone;
    lqty   = locs(i).qty;
    lalloc = locs(i).alloc;
    rrn2 += 1;
    write locsfl;
  endfor;
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
