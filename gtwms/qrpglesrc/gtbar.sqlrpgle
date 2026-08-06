**free

//////////////////////////////////////////////////////////////////////////
// GTBAR -- barcode identification, GS1 parsing and resolution.
//
// The single place in the application that answers "what did we just scan?".
// Every screen with a scan target calls gtbar_parse and routes on the result,
// so an operator never has to tell the system what kind of barcode they are
// holding.
//
// Three stages:
//   1. Normalise -- strip the AIM symbology identifier and FNC1/GS separators
//      that scanners and BarcodeDetector add in different ways.
//   2. Structure -- if it is GS1, walk the application identifiers. One scan
//      of a pallet label yields SSCC, GTIN, lot and expiry.
//   3. Resolve   -- look the keys up in DB2 and say what they are.
//
// Symbology is inferred from length and content rather than trusted from the
// client, because a keyboard-wedge scan arrives with no symbology at all.
//////////////////////////////////////////////////////////////////////////

ctl-opt nomain;

exec sql set option commit = *chg, closqlcsr = *endmod;

/copy gtbar_pr.rpgle

dcl-c GS x'1D';          // FNC1 / group separator inside a GS1 element string

//------------------------------------------------------------------------
// Fixed-length GS1 application identifiers we support. Anything not listed
// is treated as variable length and runs to the next separator.
//------------------------------------------------------------------------
dcl-ds aiFixed qualified dim(9) inz;
  ai  char(2);
  len int(10);
end-ds;

//========================================================================
// gtbar_checkOk -- verify a trailing GS1 mod-10 check digit.
//========================================================================
dcl-proc gtbar_checkOk export;
  dcl-pi *n ind;
    barcode varchar(120) const;
  end-pi;

  dcl-s work varchar(120);
  dcl-s len  int(10);

  work = %trim(barcode);
  len = %len(work);

  if len < 8 or len > 18 or not isAllDigits(work);
    return *off;
  endif;

  return gtbar_checkDigit(%subst(work : 1 : len - 1)) = %subst(work : len : 1);
end-proc;

//========================================================================
// gtbar_checkDigit -- GS1 mod-10.
//
// Weights alternate 3,1 from the RIGHTMOST payload digit, which is the same
// rule for UPC-A (11 digits), EAN-13 (12), ITF-14 (13) and SSCC-18 (17).
//========================================================================
dcl-proc gtbar_checkDigit export;
  dcl-pi *n char(1);
    payload varchar(120) const;
  end-pi;

  dcl-s work varchar(120);
  dcl-s len  int(10);
  dcl-s i    int(10);
  dcl-s d    int(10);
  dcl-s sum  int(10) inz(0);

  work = %trim(payload);
  len = %len(work);

  if len = 0 or not isAllDigits(work);
    return '?';
  endif;

  for i = 1 to len;
    d = %int(%subst(work : len - i + 1 : 1));
    if %rem(i - 1 : 2) = 0;
      sum += d * 3;
    else;
      sum += d;
    endif;
  endfor;

  return %char(%rem(10 - %rem(sum : 10) : 10));
end-proc;

//========================================================================
// gtbar_parse -- the entry point every scan goes through.
//========================================================================
dcl-proc gtbar_parse export;
  dcl-pi *n;
    payload varchar(120) const;
    result  likeds(gtbar_result_t);
  end-pi;

  dcl-s work varchar(120);
  dcl-s len  int(10);

  clear result;

  work = normalise(payload);
  result.raw = work;
  result.rawLen = %len(work);
  result.kind = 'UNKNWN';
  len = result.rawLen;

  if len = 0;
    return;
  endif;

  // ---- Stage 2: GS1 element string? ----------------------------------
  // A leading AI of 00 or 01 on a long numeric payload, or an embedded
  // separator, means this is a GS1-128 rather than a plain barcode.
  if len > 16 and isAllDigits(%subst(work : 1 : 2)) and
     (%subst(work : 1 : 2) = '00' or %subst(work : 1 : 2) = '01' or
      %scan(GS : work) > 0);
    parseGs1(work : result);
    result.isGs1 = *on;
    result.symbology = 'GS1-128';
  else;
    result.checkOk = gtbar_checkOk(work);
    result.symbology = guessSymbology(work);
  endif;

  // ---- Stage 3: resolve against the database -------------------------
  resolveKeys(work : result);
end-proc;

//========================================================================
// normalise -- strip what scanners bolt on.
//
// BarcodeDetector, keyboard wedges and camera libraries disagree about the
// AIM identifier (]C1, ]d2, ]E0 ...) and about whether FNC1 arrives as GS,
// as a literal "{GS}", or as nothing at all. Everything downstream assumes
// one clean form.
//========================================================================
dcl-proc normalise;
  dcl-pi *n varchar(120);
    payload varchar(120) const;
  end-pi;

  dcl-s work varchar(120);
  dcl-s p    int(10);

  work = %trim(payload);

  // AIM symbology identifier: "]" + 2 characters.
  if %len(work) > 3 and %subst(work : 1 : 1) = ']';
    work = %subst(work : 4);
  endif;

  // Some scanners emit FNC1 as the literal text {GS}.
  p = %scan('{GS}' : work);
  dow p > 0;
    work = %replace(GS : work : p : 4);
    p = %scan('{GS}' : work);
  enddo;

  return %trim(work);
end-proc;

//========================================================================
// parseGs1 -- walk the application identifiers.
//========================================================================
dcl-proc parseGs1;
  dcl-pi *n;
    payload varchar(120) const;
    result  likeds(gtbar_result_t);
  end-pi;

  dcl-s work varchar(120);
  dcl-s pos  int(10) inz(1);
  dcl-s ai   char(4);
  dcl-s vlen int(10);
  dcl-s val  varchar(120);
  dcl-s sep  int(10);
  dcl-s rest varchar(120);

  work = payload;

  dow pos + 1 <= %len(work);
    ai = %subst(work : pos : 2);
    pos += 2;

    vlen = fixedLenFor(ai);

    if vlen > 0;
      if pos + vlen - 1 > %len(work);
        leave;
      endif;
      val = %subst(work : pos : vlen);
      pos += vlen;
      // A fixed-length element may still be followed by a stray separator.
      if pos <= %len(work) and %subst(work : pos : 1) = GS;
        pos += 1;
      endif;
    else;
      // Variable length: runs to the next separator or the end.
      rest = %subst(work : pos);
      sep = %scan(GS : rest);
      if sep > 0;
        val = %subst(rest : 1 : sep - 1);
        pos += sep;
      else;
        val = rest;
        pos = %len(work) + 1;
      endif;
    endif;

    select;
      when ai = '00';
        result.sscc = val;
        result.checkOk = gtbar_checkOk(val);
      when ai = '01';
        result.gtin = val;
        if not result.checkOk;
          result.checkOk = gtbar_checkOk(val);
        endif;
      when ai = '10';
        result.lot = val;
      when ai = '17';
        result.expiry = val;
      when ai = '21';
        result.serial = val;
      when ai = '30' or ai = '37';
        if isAllDigits(val) and %len(val) <= 8;
          result.qtyCount = %int(val);
        endif;
      other;
        // Unsupported AI -- skipped rather than treated as an error, which
        // is what a real label with extra data should do.
    endsl;
  enddo;
end-proc;

//========================================================================
// fixedLenFor -- length of a fixed-length AI, or 0 if variable.
//========================================================================
dcl-proc fixedLenFor;
  dcl-pi *n int(10);
    ai char(4) const;
  end-pi;

  dcl-s code char(2);

  code = %subst(ai : 1 : 2);

  select;
    when code = '00';
      return 18;
    when code = '01';
      return 14;
    when code = '02';
      return 14;
    when code = '11' or code = '12' or code = '13' or
         code = '15' or code = '17';
      return 6;
    when code = '20';
      return 2;
  endsl;

  // Everything else is variable length and runs to the next separator.
  // Also satisfies the compiler, which cannot prove a SELECT covers
  // every path (RNF5415).
  return 0;
end-proc;

//========================================================================
// guessSymbology -- infer from length and content.
//
// A keyboard-wedge scan arrives as bare keystrokes with no symbology, and
// BarcodeDetector's format names vary, so the server decides.
//========================================================================
dcl-proc guessSymbology;
  dcl-pi *n char(12);
    work varchar(120) const;
  end-pi;

  dcl-s len int(10);

  len = %len(%trim(work));

  if not isAllDigits(work);
    // Location labels and licence plates in this demo are alphanumeric.
    if len <= 10;
      return 'CODE-39';
    endif;
    return 'CODE-128';
  endif;

  select;
    when len = 8;
      return 'EAN-8';
    when len = 12;
      return 'UPC-A';
    when len = 13;
      return 'EAN-13';
    when len = 14;
      return 'ITF-14';
    when len = 18;
      return 'SSCC-18';
  endsl;

  return 'CODE-128';
end-proc;

//========================================================================
// resolveKeys -- turn whatever we parsed into something the app knows.
//
// Order matters: the most specific interpretation wins. A GS1 label that
// carried a GTIN is resolved on the GTIN, not on the raw string.
//========================================================================
dcl-proc resolveKeys;
  dcl-pi *n;
    work   varchar(120) const;
    result likeds(gtbar_result_t);
  end-pi;

  // The SQL precompiler will not accept subfields of a likeds() procedure
  // PARAMETER as host variables (SQL0312), so every INTO target here is a
  // local scalar that is copied into the result afterwards.
  dcl-s lookup   varchar(20);
  dcl-s found    int(10) inz(0);
  dcl-s wSscc    char(18);
  dcl-s wRcpt    char(10);
  dcl-s wSku     char(15);
  dcl-s wPackQty packed(7 : 0);
  dcl-s wLevel   char(2);
  dcl-s wDescEn  varchar(60);
  dcl-s wDescFr  varchar(60);
  dcl-s wLoc     char(10);
  dcl-s wName    varchar(60);

  // ---- A pallet SSCC ------------------------------------------------
  if result.sscc <> *blanks;
    wSscc = result.sscc;

    exec sql
      select count(*) into :found
        from gtpallet
       where sscc = :wSscc;

    if found > 0;
      result.kind = 'PALLET';

      // A pallet sitting on an open receipt should take the operator
      // straight there -- that is the whole receiving flow.
      exec sql
        select receipt_id into :wRcpt
          from gtrcpthdr
         where sscc = :wSscc
           and receipt_status in ('OPEN', 'PART')
         fetch first row only;

      if sqlcode = 0;
        result.receiptId = wRcpt;
        result.kind = 'RCPT';
      endif;
      return;
    endif;
  endif;

  // ---- An item barcode ----------------------------------------------
  // Prefer the GS1 GTIN when one was present, otherwise the raw payload.
  if result.gtin <> *blanks;
    lookup = %trim(result.gtin);
  else;
    lookup = %subst(work : 1 : %min(20 : %len(work)));
  endif;

  exec sql
    select b.sku, b.pack_qty, b.barcode_level, i.description_en, i.description_fr
      into :wSku, :wPackQty, :wLevel, :wDescEn, :wDescFr
      from gtbarcode b
      join gtitem i on i.sku = b.sku
     where b.barcode = :lookup
     fetch first row only;

  if sqlcode = 0;
    exsr assignItem;
    return;
  endif;

  // A UPC-A very often arrives as a 13-digit EAN-13 with a leading zero:
  // that is what ZXing returns, and what a phone's BarcodeDetector returns
  // on most builds. Without this retry a perfectly good item scans as
  // "not one of ours", which is the worst possible failure on stage.
  if %len(lookup) = 13 and %subst(lookup : 1 : 1) = '0';
    lookup = %subst(lookup : 2);

    exec sql
      select b.sku, b.pack_qty, b.barcode_level, i.description_en, i.description_fr
        into :wSku, :wPackQty, :wLevel, :wDescEn, :wDescFr
        from gtbarcode b
        join gtitem i on i.sku = b.sku
       where b.barcode = :lookup
       fetch first row only;

    if sqlcode = 0;
      exsr assignItem;
      return;
    endif;
  endif;

  // A GTIN-14 case code is the same item as its UPC-A with the packaging
  // indicator and check digit stripped -- try that before giving up.
  if %len(lookup) = 14;
    lookup = %subst(lookup : 2 : 12);

    exec sql
      select b.sku, b.pack_qty, b.barcode_level, i.description_en, i.description_fr
        into :wSku, :wPackQty, :wLevel, :wDescEn, :wDescFr
        from gtbarcode b
        join gtitem i on i.sku = b.sku
       where b.barcode = :lookup
       fetch first row only;

    if sqlcode = 0;
      exsr assignItem;
      return;
    endif;
  endif;

  // ---- A location label ---------------------------------------------
  if %len(work) <= 10;
    lookup = work;

    exec sql
      select location_id into :wLoc
        from gtlocation
       where location_id = :lookup
         and active_flag = 'Y'
       fetch first row only;

    if sqlcode = 0;
      result.locationId = wLoc;
      result.kind = 'LOC';
      return;
    endif;
  endif;

  // ---- An operator badge --------------------------------------------
  if %len(work) <= 20;
    lookup = work;

    exec sql
      select full_name into :wName
        from gtoperator
       where badge_barcode = :lookup
         and active_flag = 'Y'
       fetch first row only;

    if sqlcode = 0;
      result.descEn = wName;
      result.descFr = wName;
      result.kind = 'OPER';
      return;
    endif;
  endif;

  // Unknown is a legitimate answer, not a failure. Scan Lab is built around
  // showing exactly this for a barcode that is not ours.
  result.kind = 'UNKNWN';

  begsr assignItem;
    result.sku      = wSku;
    result.packQty  = wPackQty;
    result.barLevel = wLevel;
    result.descEn   = wDescEn;
    result.descFr   = wDescFr;
    result.kind     = 'ITEM';
  endsr;
end-proc;

//========================================================================
// isAllDigits
//========================================================================
dcl-proc isAllDigits;
  dcl-pi *n ind;
    work varchar(120) const;
  end-pi;

  dcl-s trimmed varchar(120);

  trimmed = %trim(work);
  if %len(trimmed) = 0;
    return *off;
  endif;

  return %check('0123456789' : trimmed) = 0;
end-proc;
