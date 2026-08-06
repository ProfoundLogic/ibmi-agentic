**free

//////////////////////////////////////////////////////////////////////////
// GTSCNR -- Scan Lab.
//
// Scans anything and shows exactly what it found: symbology, raw payload,
// parsed GS1 application identifiers, and whether it resolved to anything
// in the database.
//
// It costs almost nothing -- it is GTBAR plus a result card -- and it is
// the only screen where an executive can be handed the phone and told to
// scan whatever is on the table. "Not one of ours" is a first-class result
// here, not an error: showing a correctly decoded payload for a stranger's
// conference badge is the point.
//
// It is also the proving ground for gt-scan, which every other screen in
// the application depends on.
//////////////////////////////////////////////////////////////////////////

ctl-opt dftactgrp(*no) actgrp(*new);
ctl-opt bnddir('GTBAR');

dcl-f gtscnd workstn sfile(scnsfl : rrn) handler('PROFOUNDUI(HANDLER)');

// Commitment control is used deliberately across this project; this screen
// only reads, but the setting has to agree with the rest of the module set.
exec sql set option commit = *chg, closqlcsr = *endmod;

/copy gtbar_pr.rpgle

dcl-c HISTORY_MAX 20;

dcl-ds r likeds(gtbar_result_t);

// Scan history, newest first. Held in the program rather than a table --
// it is a diagnostic view of this session, not a business record.
dcl-ds history qualified dim(HISTORY_MAX);
  kind  char(6);
  symb  char(12);
  raw   varchar(120);
  desc  varchar(60);
end-ds;

dcl-s histCount int(10) inz(0);
dcl-s rrn       int(10) inz(0);
dcl-s i         int(10);
dcl-s done      ind inz(*off);
dcl-s payload   varchar(120);

//------------------------------------------------------------------------
// Main
//------------------------------------------------------------------------
exsr loadOperator;
exsr clearResult;

dow not done;
  exsr loadSubfile;

  action = *blanks;
  exfmt scnhome;
  msg = *blanks;

  select;
    when action = 'EXIT';
      done = *on;

    when action = 'CLEAR';
      histCount = 0;
      exsr clearResult;

    when action = 'LANGEN';
      langpref = 'EN';
      exsr saveLanguage;

    when action = 'LANGFR';
      langpref = 'FR';
      exsr saveLanguage;

    when action = 'SCAN';
      exsr doScan;

    other;
      // Enter with nothing pressed -- redisplay as-is.
  endsl;
enddo;

*inlr = *on;
return;

//------------------------------------------------------------------------
// doScan -- hand the payload to GTBAR and unpack the answer.
//------------------------------------------------------------------------
begsr doScan;
  payload = %trim(scanval);

  if payload = '';
    if langpref = 'FR';
      msg = 'Rien à analyser. Balayez un code ou saisissez-en un.';
    else;
      msg = 'Nothing to decode. Scan a barcode or type one in.';
    endif;
    exsr clearResult;
  else;
    gtbar_parse(payload : r);

    hasres  = 'Y';
    rkind   = r.kind;
    rsymb   = r.symbology;
    rraw    = r.raw;
    rrawlen = r.rawLen;
    rsku    = r.sku;
    rdesc   = descriptionForLang();
    rloc    = r.locationId;
    rsscc   = r.sscc;
    rrcpt   = r.receiptId;
    rgtin   = r.gtin;
    rlot    = r.lot;
    rexpiry = r.expiry;
    rqty    = r.qtyCount;
    rlevel  = r.barLevel;
    rpack   = r.packQty;

    risgs1 = 'N';
    if r.isGs1;
      risgs1 = 'Y';
    endif;

    rcheck = 'N';
    if r.checkOk;
      rcheck = 'Y';
    endif;

    exsr pushHistory;
  endif;

  // Always clear the input so the next scan starts from empty. An operator
  // scanning continuously must never append to the previous payload.
  scanval = *blanks;
endsr;

//------------------------------------------------------------------------
// clearResult -- blank the result card.
//------------------------------------------------------------------------
begsr clearResult;
  hasres  = 'N';
  rkind   = *blanks;
  rsymb   = *blanks;
  risgs1  = 'N';
  rcheck  = 'N';
  rraw    = *blanks;
  rrawlen = 0;
  rsku    = *blanks;
  rdesc   = *blanks;
  rloc    = *blanks;
  rsscc   = *blanks;
  rrcpt   = *blanks;
  rgtin   = *blanks;
  rlot    = *blanks;
  rexpiry = *blanks;
  rqty    = 0;
  rlevel  = *blanks;
  rpack   = 0;
  scanval = *blanks;
endsr;

//------------------------------------------------------------------------
// pushHistory -- newest first, oldest falls off the end.
//------------------------------------------------------------------------
begsr pushHistory;
  if histCount < HISTORY_MAX;
    histCount += 1;
  endif;

  for i = histCount downto 2;
    history(i) = history(i - 1);
  endfor;

  history(1).kind = r.kind;
  history(1).symb = r.symbology;
  history(1).raw  = r.raw;
  history(1).desc = descriptionForLang();
endsr;

//------------------------------------------------------------------------
// loadSubfile
//------------------------------------------------------------------------
begsr loadSubfile;
  // The control record IS the format; sflclear is a character flag the
  // compiler generates from the "clear" property in the JSON source.
  rrn = 0;
  sflclear = '1';
  write scnhome;
  sflclear = '0';

  for i = 1 to histCount;
    sseq  = i;
    skind = history(i).kind;
    ssymb = history(i).symb;
    sraw  = %subst(history(i).raw : 1 : %min(40 : %len(history(i).raw)));
    sdesc = history(i).desc;
    rrn += 1;
    write scnsfl;
  endfor;

  nscans = histCount;
endsr;

//------------------------------------------------------------------------
begsr loadOperator;
  exec sql
    select full_name, lang_pref
      into :opername, :langpref
      from gtoperator
     where operator_id = 'GT001'
       and active_flag = 'Y'
     fetch first row only;

  if sqlcode <> 0;
    opername = 'Demo Operator';
    langpref = 'EN';
  endif;
endsr;

//------------------------------------------------------------------------
begsr saveLanguage;
  exec sql
    update gtoperator
       set lang_pref = :langpref
     where operator_id = 'GT001';

  if sqlcode < 0;
    exec sql rollback;
  else;
    exec sql commit;
  endif;
endsr;

//------------------------------------------------------------------------
dcl-proc descriptionForLang;
  dcl-pi *n varchar(60);
  end-pi;

  if langpref = 'FR' and r.descFr <> '';
    return r.descFr;
  endif;
  return r.descEn;
end-proc;
