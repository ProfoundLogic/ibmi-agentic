**free

//////////////////////////////////////////////////////////////////////////
// GTITMR -- Item Lookup.
//
// The demo's opener and the brand showpiece: scan a UPC, get the product
// with a swipeable image carousel, its facts, and where every unit of it is
// in the building.
//
// Two formats rather than the three in the design: the location list folds
// into the detail screen as a subfile. Fewer taps for the operator, and one
// less screen to keep consistent.
//
// Resolution goes through GTBAR like every other scan in the application, so
// a UPC, a case ITF-14, a GS1 pallet label carrying a GTIN and a bare SKU all
// arrive here the same way.
//////////////////////////////////////////////////////////////////////////

ctl-opt dftactgrp(*no) actgrp(*new);
ctl-opt bnddir('GTBAR');

dcl-f gtitmd workstn sfile(srchsfl : rrn) handler('PROFOUNDUI(HANDLER)');

exec sql set option commit = *chg, closqlcsr = *endmod;

/copy gtbar_pr.rpgle

// The detail screen is a SEPARATE program with its own single-format display
// file. Two formats in one display file both stayed rendered at once, which
// left the detail screen covering the search screen's footer.
dcl-pr gtitdr extpgm;
  sku    char(15) const;
  lang   char(2);
  outAct char(10);
end-pr;

dcl-ds r likeds(gtbar_result_t);

dcl-c MAX_ROWS 60;

dcl-s rrn      int(10) inz(0);
dcl-s i        int(10);
dcl-s done     ind inz(*off);
dcl-s payload  varchar(120);
dcl-s curSku   char(15);
dcl-s detlAct  char(10);

// Search results held between round trips so the subfile can be rebuilt
// without re-querying on every redisplay.
dcl-ds hits qualified dim(MAX_ROWS);
  sku   char(15);
  desc  varchar(60);
  dept  char(4);
  img   int(10);
  oh    packed(11 : 2);
  uom   char(3);
end-ds;
dcl-s hitCount int(10) inz(0);


// Work fields for the subroutines below. RPG will not accept a dcl-s
// inside a begsr in the main procedure (RNF0724), so they live here.
dcl-s pattern varchar(60);
dcl-s wDept  char(4);
dcl-s wDesc  varchar(60);
dcl-s wImg   int(10);
dcl-s wOh    packed(11 : 2);
dcl-s wSku   char(15);
dcl-s wUom   char(3);

//------------------------------------------------------------------------
// Main
//------------------------------------------------------------------------
exsr loadOperator;

dow not done;
  exsr loadSearchSubfile;
  action = *blanks;
  exfmt itmscan;
  msg = *blanks;

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

    when action = 'SEARCH';
      exsr doSearch;

    when action = 'CLEAR';
      hitCount = 0;
      search = *blanks;
      scanval = *blanks;

    when %subst(action : 1 : 4) = 'PICK';
      // PICKnn -- the operator tapped a result row.
      i = %int(%subst(action : 5));
      if i >= 1 and i <= hitCount;
        curSku = hits(i).sku;
        exsr showDetail;
      endif;

    other;
  endsl;
enddo;

*inlr = *on;
return;

//------------------------------------------------------------------------
// doScan -- one scan box, resolved by GTBAR.
//------------------------------------------------------------------------
begsr doScan;
  payload = %trim(scanval);
  scanval = *blanks;

  if payload = '';
    exsr sayNothingScanned;
  else;
    gtbar_parse(payload : r);

    if r.kind = 'ITEM' and r.sku <> *blanks;
      curSku = r.sku;
      exsr showDetail;
    else;
      // Not an item. Say what it actually was rather than "not found" --
      // scanning a location here is a mistake worth naming.
      exsr sayNotAnItem;
    endif;
  endif;
endsr;

//------------------------------------------------------------------------
// doSearch -- description search in the operator's language.
//------------------------------------------------------------------------
begsr doSearch;
  if %trim(search) = '';
    hitCount = 0;
  else;
    pattern = '%' + %xlate('abcdefghijklmnopqrstuvwxyz' :
                           'ABCDEFGHIJKLMNOPQRSTUVWXYZ' : %trim(search)) + '%';
    exsr runSearch;
  endif;
endsr;

//------------------------------------------------------------------------
begsr runSearch;
  hitCount = 0;

  // Search whichever description matches the operator's language, and pull
  // the first image and the total on-hand in the same pass so the result
  // list can show a thumbnail and a quantity without N+1 queries.
  exec sql
    declare csrch cursor for
      select i.sku,
             case when :langpref = 'FR' then i.description_fr
                  else i.description_en end,
             i.department,
             coalesce((select min(v.image_id) from gtvitemimg v
                        where v.sku = i.sku), 0),
             coalesce((select sum(b.qty_on_hand) from gtinvbal b
                        where b.sku = i.sku), 0),
             i.uom
        from gtitem i
       where i.item_status = 'A'
         and (upper(i.description_en) like :pattern
              or upper(i.description_fr) like :pattern
              or upper(i.sku) like :pattern)
       order by i.description_en
       fetch first 60 rows only;

  exec sql open csrch;
  exec sql fetch csrch into :wSku, :wDesc, :wDept, :wImg, :wOh, :wUom;

  dow sqlcode = 0 and hitCount < MAX_ROWS;
    hitCount += 1;
    hits(hitCount).sku  = wSku;
    hits(hitCount).desc = wDesc;
    hits(hitCount).dept = wDept;
    hits(hitCount).img  = wImg;
    hits(hitCount).oh   = wOh;
    hits(hitCount).uom  = wUom;
    exec sql fetch csrch into :wSku, :wDesc, :wDept, :wImg, :wOh, :wUom;
  enddo;

  exec sql close csrch;

  if hitCount = 0;
    exsr sayNoMatch;
  endif;
endsr;

//------------------------------------------------------------------------
// showDetail -- hand off to the detail program and honour what it returns.
//------------------------------------------------------------------------
begsr showDetail;
  detlAct = *blanks;
  gtitdr(curSku : langpref : detlAct);

  // The detail screen offers a way straight back to the menu, so it can ask
  // this program to end too rather than dropping the operator back into a
  // search they had already finished with.
  if detlAct = 'MENU';
    done = *on;
  endif;
endsr;

//------------------------------------------------------------------------
begsr loadSearchSubfile;
  rrn = 0;
  sflclear = '1';
  write itmscan;
  sflclear = '0';

  for i = 1 to hitCount;
    sseq  = i;
    ssku  = hits(i).sku;
    sdesc = hits(i).desc;
    sdept = hits(i).dept;
    simg  = hits(i).img;
    soh   = hits(i).oh;
    suom  = hits(i).uom;
    rrn += 1;
    write srchsfl;
  endfor;

  nfound = hitCount;
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
// Messages. Bilingual, and specific about what actually happened.
//------------------------------------------------------------------------
begsr sayNothingScanned;
  if langpref = 'FR';
    msg = 'Rien à chercher. Balayez un code ou saisissez une description.';
  else;
    msg = 'Nothing to look up. Scan a barcode or type a description.';
  endif;
endsr;

begsr sayNotAnItem;
  if langpref = 'FR';
    select;
      when r.kind = 'LOC';
        msg = 'Ceci est un emplacement, pas un article : ' + %trim(r.locationId);
      when r.kind = 'PALLET' or r.kind = 'RCPT';
        msg = 'Ceci est une palette, pas un article.';
      when r.kind = 'OPER';
        msg = 'Ceci est un badge d''opérateur, pas un article.';
      other;
        msg = 'Code inconnu : ' + %subst(r.raw : 1 : %min(40 : r.rawLen));
    endsl;
  else;
    select;
      when r.kind = 'LOC';
        msg = 'That is a location, not an item: ' + %trim(r.locationId);
      when r.kind = 'PALLET' or r.kind = 'RCPT';
        msg = 'That is a pallet, not an item.';
      when r.kind = 'OPER';
        msg = 'That is an operator badge, not an item.';
      other;
        msg = 'Unknown barcode: ' + %subst(r.raw : 1 : %min(40 : r.rawLen));
    endsl;
  endif;
endsr;

begsr sayNoMatch;
  if langpref = 'FR';
    msg = 'Aucun article ne correspond.';
  else;
    msg = 'No items match.';
  endif;
endsr;
