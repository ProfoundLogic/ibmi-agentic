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
//////////////////////////////////////////////////////////////////////////

ctl-opt dftactgrp(*no) actgrp(*new);

dcl-f gtitdd workstn sfile(locsfl : rrn2) handler('PROFOUNDUI(HANDLER)');

exec sql set option commit = *chg, closqlcsr = *endmod;

dcl-pi *n;
  inSku   char(15) const;
  inLang  char(2);
  outAct  char(10);
end-pi;

dcl-c MAX_ROWS 60;

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

  dimg1 = 0;
  dimg2 = 0;
  dimg3 = 0;
  dimg4 = 0;
  dcap1 = *blanks;
  dcap2 = *blanks;
  dcap3 = *blanks;
  dcap4 = *blanks;
  i = 0;

  exec sql
    declare cimg cursor for
      select v.image_id,
             case when :langpref = 'FR' then v.caption_fr
                  else v.caption_en end
        from gtvitemimg v
       where v.sku = :curSku
       order by v.seq_no
       fetch first 4 rows only;

  exec sql open cimg;
  exec sql fetch cimg into :wImgId, :wCap;
  dow sqlcode = 0 and i < 4;
    i += 1;
    select;
      when i = 1;
        dimg1 = wImgId;
        dcap1 = wCap;
      when i = 2;
        dimg2 = wImgId;
        dcap2 = wCap;
      when i = 3;
        dimg3 = wImgId;
        dcap3 = wCap;
      when i = 4;
        dimg4 = wImgId;
        dcap4 = wCap;
    endsl;
    exec sql fetch cimg into :wImgId, :wCap;
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
