**free

//  FLDATA - J.H. Fletcher demo data access service program (GJA-911)
//
//  Duplicate detection
//  -------------------
//  Part descriptions are normalised into a canonical signature:
//    * upper-cased, punctuation replaced by spaces
//    * era-specific abbreviations expanded  (HYD -> HYDRAULIC, ASSY -> ASSEMBLY)
//    * number words converted to digits     (FOUR -> 4, TWELVE -> 12)
//    * noise words dropped                  (ASSEMBLY, STANDARD, SERIES, INCH)
//    * remaining tokens split into words and sizes, de-duplicated and sorted
//  Two parts are duplicates when their WORD signatures are identical (>= 2
//  words) and their SIZE signatures are compatible - equal, or one is empty.
//  That clusters "HYDRAULIC PUMP ASSY" / "HYD PUMP ASSEMBLY" / "PUMP,
//  HYDRAULIC, ASSY" / "HYDRAULIC PUMP (STANDARD)" while keeping genuinely
//  different sizes apart ("HOSE 25FT" vs "HOSE 40FT").

ctl-opt nomain;

exec sql set option commit = *none;
exec sql set option closqlcsr = *endmod;

/copy fldata_pr.rpgle

dcl-c ABBREV 'HYD:HYDRAULIC PMP:PUMP CYL:CYLINDER VLV:VALVE CTL:CONTROL +
              CNP:CANOPY DST:DUST FLT:FILTER FLTR:FILTER DRL:DRILL +
              CHK:CHUCK CHN:CHAIN WTR:WATER BM:BOOM FD:FEED ASSY:ASSEMBLY +
              HD:HEAD PWDR:POWDER MANIF:MANIFOLD ';
dcl-c NUMWORD 'ONE:1 TWO:2 THREE:3 FOUR:4 FIVE:5 SIX:6 SEVEN:7 EIGHT:8 +
               NINE:9 TEN:10 ELEVEN:11 TWELVE:12 ';
dcl-c STOPW ' ASSEMBLY STANDARD STD SERIES SER INCH IN FT GPM MM CM LB KIT +
              NEW THE FOR AND WITH ';
dcl-c THIS_YEAR 2026;
dcl-c FULL_MOD_YEARS 15;
dcl-c PART_MOD_YEARS 7;

// part-signature cache, loaded once per activation
dcl-ds sigCache qualified dim(2000);
  part char(15);
  words varchar(120);
  sizes varchar(40);
  descr char(40);
  line char(3);
  super char(15);
  status char(1);
  price packed(11:2);
  stock packed(7:0);
  created packed(8:0);
end-ds;
dcl-s sigCount int(10) inz(0);
dcl-s sigLoaded ind inz(*off);

// scan bookkeeping - global because free-form RPG requires declarations
// before the first procedure, not between them (RNF0256)
dcl-s used ind dim(2000);

// ---- A4 schedule board caches -----------------------------------------
// Loaded once per activation. The board and its two detail screens are
// read-only inquiries, so nothing invalidates these mid-run; if a
// maintenance screen is ever added it must reset schedLoaded.
dcl-ds wcCache qualified dim(50);
  code char(6);
  descr char(40);
  seq packed(3:0);
  cap packed(5:1);
  crew packed(3:0);
  shifts packed(1:0);
  status char(1);
  wip int(10);
  queued int(10);
  loadhrs packed(7:1);
  late int(10);
end-ds;
dcl-s wcCount int(10) inz(0);

dcl-ds woCache likeds(fl_wo_t) dim(300);
dcl-s woCount int(10) inz(0);
dcl-s woModelOk ind dim(300);

// Stored dates, kept alongside the formatted display copies in woCache.
// The projection walk and the confidence scan both need real dates; parsing
// them back out of char(10) display text would be absurd.
dcl-ds woRaw qualified dim(300);
  opened packed(8:0);
  promised packed(8:0);
  started packed(8:0);
  compl packed(8:0);
  projected packed(8:0);
end-ds;

dcl-ds opCache qualified dim(1200);
  wo packed(8:0);
  seq packed(3:0);
  wctr char(6);
  descr char(40);
  std packed(5:1);
  act packed(5:1);
  status char(1);
  started packed(8:0);
  compl packed(8:0);
end-ds;
dcl-s opCount int(10) inz(0);

dcl-ds shCache qualified dim(300);
  wo packed(8:0);
  part char(15);
  seq packed(3:0);
  reqd packed(5:0);
  avail packed(5:0);
  due packed(8:0);
  status char(1);
  po char(8);
  vend char(30);
  price packed(11:2);
end-ds;
dcl-s shCount int(10) inz(0);

dcl-ds dqCache likeds(fl_dq_t) dim(400);
dcl-s dqCount int(10) inz(0);

dcl-s schedLoaded ind inz(*off);
dcl-s schedToday packed(8:0) inz(0);

// ------------------------------------------------------------------ helpers
dcl-proc fl_lookup;
  dcl-pi *n varchar(30);
    map varchar(400) const;
    token varchar(30) const;
  end-pi;
  dcl-s p int(10);
  dcl-s e int(10);
  p = %scan(' ' + %trim(token) + ':' : ' ' + map);
  if p = 0;
    return '';
  endif;
  // p is the leading space: +1 token start, +len end of token,
  // +1 colon, +1 first char of the value
  p = p + %len(%trim(token)) + 2;
  e = %scan(' ' : ' ' + map : p);
  if e = 0;
    e = %len(map) + 2;
  endif;
  return %subst(' ' + map : p : e - p);
end-proc;

dcl-proc fl_norm;
  dcl-pi *n;
    descr char(60) const;
    words varchar(120);
    sizes varchar(40);
  end-pi;
  dcl-s t varchar(80);
  dcl-s tok varchar(30);
  dcl-s mapped varchar(30);
  dcl-s p int(10);
  dcl-s i int(10);
  dcl-s j int(10);
  dcl-s n int(10);
  dcl-s isSize ind;
  dcl-s w char(30) dim(20);
  dcl-s nw int(10) inz(0);
  dcl-s s char(10) dim(20);
  dcl-s ns int(10) inz(0);
  dcl-s tmp char(30);

  words = '';
  sizes = '';
  t = %upper(%trimr(descr));
  t = %xlate(',.()/-*"''&+#' : '             ' : t);

  dow %len(%trim(t)) > 0;
    t = %triml(t);
    p = %scan(' ' : t);
    if p = 0;
      tok = t;
      t = '';
    else;
      tok = %subst(t : 1 : p - 1);
      t = %subst(t : p);
    endif;
    if tok = '';
      iter;
    endif;

    // number word -> digits
    mapped = fl_lookup(NUMWORD : tok);
    if mapped <> '';
      tok = mapped;
    else;
      mapped = fl_lookup(ABBREV : tok);
      if mapped <> '';
        tok = mapped;
      endif;
    endif;

    // Any token containing a digit is a size or variant designation, not a
    // descriptive word: 4IN, 200, A1, B3. Treating short ones like A1 as
    // words-too-short-to-keep made A1, A2 and B3 pumps look identical.
    isSize = %check('ABCDEFGHIJKLMNOPQRSTUVWXYZ' : %trimr(tok)) <> 0
             and %len(%trim(tok)) > 0;
    if isSize;
      // if it starts with digits, drop a trailing unit: 4IN -> 4
      if %check('0123456789' : tok) <> 1;
        n = %check('0123456789' : tok);
        if n > 0;
          tok = %subst(tok : 1 : n - 1);
        endif;
      endif;
      if tok = '';
        iter;
      endif;
      if %lookup(%trimr(tok) : s : 1 : ns) = 0;
        ns += 1;
        s(ns) = tok;
      endif;
      iter;
    endif;

    // noise word, or too short to be meaningful
    if %scan(' ' + %trim(tok) + ' ' : STOPW) > 0 or %len(%trim(tok)) < 3;
      iter;
    endif;
    if %lookup(%trimr(tok) : w : 1 : nw) = 0;
      nw += 1;
      w(nw) = tok;
    endif;
  enddo;

  // sort both lists so the signature is order-independent
  for i = 1 to nw - 1;
    for j = i + 1 to nw;
      if w(j) < w(i);
        tmp = w(i);
        w(i) = w(j);
        w(j) = tmp;
      endif;
    endfor;
  endfor;
  for i = 1 to ns - 1;
    for j = i + 1 to ns;
      if s(j) < s(i);
        tmp = s(i);
        s(i) = s(j);
        s(j) = tmp;
      endif;
    endfor;
  endfor;

  for i = 1 to nw;
    words += %trimr(w(i)) + ' ';
  endfor;
  for i = 1 to ns;
    sizes += %trimr(s(i)) + ' ';
  endfor;
end-proc;

dcl-proc fl_loadSigs;
  dcl-ds r qualified;
    part char(15);
    descr char(40);
    line char(3);
    super char(15);
    status char(1);
    price packed(11:2);
    stock packed(7:0);
    created packed(8:0);
  end-ds;
  if sigLoaded;
    return;
  endif;
  exec sql declare cSig cursor for
    select flpart, flpdesc, flpline, flpsuper, flpstat, flpprice, flpstock,
           flpcreat
      from flpartp order by flpart;
  exec sql open cSig;
  dow sigCount < %elem(sigCache);
    exec sql fetch cSig into :r;
    if sqlcode <> 0;
      leave;
    endif;
    sigCount += 1;
    sigCache(sigCount).part    = r.part;
    sigCache(sigCount).descr   = r.descr;
    sigCache(sigCount).line    = r.line;
    sigCache(sigCount).super   = r.super;
    sigCache(sigCount).status  = r.status;
    sigCache(sigCount).price   = r.price;
    sigCache(sigCount).stock   = r.stock;
    sigCache(sigCount).created = r.created;
    fl_norm(r.descr : sigCache(sigCount).words : sigCache(sigCount).sizes);
  enddo;
  exec sql close cSig;
  sigLoaded = *on;
end-proc;

dcl-proc fl_sizesOk;
  dcl-pi *n ind;
    a varchar(40) const;
    b varchar(40) const;
  end-pi;
  return a = b or a = '' or b = '';
end-proc;

dcl-proc fl_countDupes;
  dcl-pi *n int(10);
    part char(15) const;
  end-pi;
  dcl-s i int(10);
  dcl-s me int(10);
  dcl-s n int(10) inz(0);
  fl_loadSigs();
  me = 0;
  for i = 1 to sigCount;
    if sigCache(i).part = part;
      me = i;
      leave;
    endif;
  endfor;
  if me = 0 or %len(%trim(sigCache(me).words)) = 0;
    return 0;
  endif;
  // a single word is too weak a signal to call a duplicate
  if %scan(' ' : %trim(sigCache(me).words)) = 0;
    return 0;
  endif;
  for i = 1 to sigCount;
    if i <> me
       and sigCache(i).words = sigCache(me).words
       and fl_sizesOk(sigCache(i).sizes : sigCache(me).sizes);
      n += 1;
    endif;
  endfor;
  return n;
end-proc;

dcl-proc fl_statusDesc;
  dcl-pi *n char(10);
    code char(1) const;
  end-pi;
  select;
    when code = 'A';
      return 'Active';
    when code = 'P';
      return 'Parked';
    when code = 'S';
      return 'Scrapped';
    other;
      return code;
  endsl;
end-proc;

dcl-proc fl_modTypeDesc;
  dcl-pi *n char(10);
    code char(1) const;
  end-pi;
  select;
    when code = 'F';
      return 'Full';
    when code = 'P';
      return 'Partial';
    when code = 'N';
      return 'None';
    other;
      return code;
  endsl;
end-proc;

dcl-proc fl_svcTypeDesc;
  dcl-pi *n char(12);
    code char(1) const;
  end-pi;
  select;
    when code = 'A';
      return 'Audit';
    when code = 'R';
      return 'Repair';
    when code = 'P';
      return 'Maintenance';
    when code = 'W';
      return 'Warranty';
    other;
      return code;
  endsl;
end-proc;

dcl-proc fl_partStatusDesc;
  dcl-pi *n char(12);
    code char(1) const;
  end-pi;
  select;
    when code = 'A';
      return 'Active';
    when code = 'S';
      return 'Superseded';
    when code = 'O';
      return 'Obsolete';
    other;
      return code;
  endsl;
end-proc;

// The single place a date is turned into display text. Every screen-facing
// date goes through here, so the format is changed in one edit.
// Standard: MM/DD/YYYY, blank when the stored value is zero.
dcl-proc fl_fmtDate;
  dcl-pi *n char(10);
    ymd packed(8:0) const;
  end-pi;
  dcl-s t char(8);
  if ymd = 0;
    return *blanks;
  endif;
  t = %char(%int(ymd));
  if %len(%trim(t)) <> 8;
    return *blanks;
  endif;
  return %subst(t : 5 : 2) + '/' + %subst(t : 7 : 2) + '/' + %subst(t : 1 : 4);
end-proc;

// Single place the modernization rule lives, so the list and the summary agree.
dcl-proc fl_modFlag;
  dcl-pi *n char(12);
    builtYmd packed(8:0) const;
    lastModYmd packed(8:0) const;
    modType char(1) const;
  end-pi;
  dcl-s builtYr int(10);
  dcl-s baseYr int(10);
  if builtYmd = 0;
    return '';
  endif;
  builtYr = %int(builtYmd / 10000);
  if THIS_YEAR - builtYr >= FULL_MOD_YEARS and modType <> 'F';
    return 'FULL DUE';
  endif;
  baseYr = builtYr;
  if lastModYmd > 0;
    baseYr = %int(lastModYmd / 10000);
  endif;
  if THIS_YEAR - baseYr >= PART_MOD_YEARS;
    return 'PARTIAL DUE';
  endif;
  return '';
end-proc;

// ------------------------------------------------------------ exported API
dcl-proc fl_getCustomer export;
  dcl-pi *n varchar(80);
    p_custno like(fl_cust_t.custno) const;
    customer likeds(fl_cust_t);
    found ind;
  end-pi;
  found = *off;
  clear customer;
  exec sql
    select flcust, flcname, flccity, flcstate, flcregn, flcmgr, flcstat
      into :customer
      from flcustp
     where flcust = :p_custno;
  if sqlcode < 0;
    return 'Error reading customer. SQLCODE=' + %char(sqlcode);
  endif;
  found = sqlcode <> 100;
  return '';
end-proc;

dcl-proc fl_getMachine export;
  dcl-pi *n varchar(80);
    serial char(8) const;
    machine likeds(fl_mach_t);
    found ind;
  end-pi;
  dcl-ds r qualified;
    ser char(8);
    model char(10);
    modeldsc char(40);
    custno packed(6:0);
    custname char(40);
    siteno packed(6:0);
    sitename char(40);
    sitetype char(1);
    built packed(8:0);
    shipped packed(8:0);
    hours packed(9:0);
    status char(1);
    lastsvc packed(8:0);
    lastmod packed(8:0);
    modtype char(1);
  end-ds;
  found = *off;
  clear machine;
  exec sql
    select m.flser, m.flmmodl, coalesce(d.flmdesc,''), m.flmcust,
           coalesce(c.flcname,''), m.flmsite, coalesce(s.flsname,''),
           coalesce(s.flstype,''),
           m.flmbuild, m.flmship, m.flmhours, m.flmstat,
           m.flmlsvc, m.flmlmod, m.flmmodt
      into :r
      from flmachp m
      left join flmodlp d on d.flmodl = m.flmmodl
      left join flcustp c on c.flcust  = m.flmcust
      left join flsitep s on s.flsite  = m.flmsite
     where m.flser = :serial;
  if sqlcode < 0;
    return 'Error reading machine. SQLCODE=' + %char(sqlcode);
  endif;
  if sqlcode = 100;
    return '';
  endif;
  found = *on;
  machine.ser = r.ser;
  machine.model = r.model;
  machine.modeldsc = r.modeldsc;
  machine.custno = r.custno;
  machine.custname = r.custname;
  machine.siteno = r.siteno;
  machine.sitename = r.sitename;
  machine.sitetype = r.sitetype;
  machine.built = fl_fmtDate(r.built);
  machine.shipped = fl_fmtDate(r.shipped);
  machine.hours = r.hours;
  machine.status = r.status;
  machine.statusd = fl_statusDesc(r.status);
  machine.lastsvc = fl_fmtDate(r.lastsvc);
  machine.lastmod = fl_fmtDate(r.lastmod);
  machine.modtype = r.modtype;
  machine.modtyped = fl_modTypeDesc(r.modtype);
  machine.modflag = fl_modFlag(r.built : r.lastmod : r.modtype);
  return '';
end-proc;

dcl-proc fl_listMachines export;
  dcl-pi *n varchar(80);
    p_custno like(fl_cust_t.custno) const;
    machines likeds(fl_mach_t) dim(500);
    limit int(10) const;
    returned int(10);
  end-pi;
  dcl-ds r qualified;
    ser char(8);
    model char(10);
    modeldsc char(40);
    siteno packed(6:0);
    sitename char(40);
    built packed(8:0);
    hours packed(9:0);
    status char(1);
    lastsvc packed(8:0);
    modtype char(1);
    lastmod packed(8:0);
  end-ds;
  returned = 0;
  exec sql declare cMach cursor for
    select m.flser, m.flmmodl, coalesce(d.flmdesc,''), m.flmsite,
           coalesce(s.flsname,''), m.flmbuild, m.flmhours, m.flmstat,
           m.flmlsvc, m.flmmodt, m.flmlmod
      from flmachp m
      left join flmodlp d on d.flmodl = m.flmmodl
      left join flsitep s on s.flsite = m.flmsite
     where m.flmcust = :p_custno
     order by m.flser;
  exec sql open cMach;
  if sqlcode < 0;
    return 'Error listing machines. SQLCODE=' + %char(sqlcode);
  endif;
  dow returned < limit and returned < %elem(machines);
    exec sql fetch cMach into :r;
    if sqlcode <> 0;
      leave;
    endif;
    returned += 1;
    clear machines(returned);
    machines(returned).ser = r.ser;
    machines(returned).model = r.model;
    machines(returned).modeldsc = r.modeldsc;
    machines(returned).custno = p_custno;
    machines(returned).siteno = r.siteno;
    machines(returned).sitename = r.sitename;
    machines(returned).built = fl_fmtDate(r.built);
    machines(returned).hours = r.hours;
    machines(returned).status = r.status;
    machines(returned).statusd = fl_statusDesc(r.status);
    machines(returned).lastsvc = fl_fmtDate(r.lastsvc);
    machines(returned).modtype = r.modtype;
    machines(returned).modtyped = fl_modTypeDesc(r.modtype);
    machines(returned).modflag = fl_modFlag(r.built : r.lastmod : r.modtype);
  enddo;
  exec sql close cMach;
  return '';
end-proc;

dcl-proc fl_listOptions export;
  dcl-pi *n varchar(80);
    serial char(8) const;
    options_ likeds(fl_optn_t) dim(50);
    limit int(10) const;
    returned int(10);
  end-pi;
  dcl-ds r likeds(fl_optn_t);
  returned = 0;
  exec sql declare cOpt cursor for
    select flocode, flodesc from floptnp where floser = :serial order by flocode;
  exec sql open cOpt;
  if sqlcode < 0;
    return 'Error listing options. SQLCODE=' + %char(sqlcode);
  endif;
  dow returned < limit and returned < %elem(options_);
    exec sql fetch cOpt into :r;
    if sqlcode <> 0;
      leave;
    endif;
    returned += 1;
    options_(returned) = r;
  enddo;
  exec sql close cOpt;
  return '';
end-proc;

// Parts that fit a specific machine: BOM for its model, restricted to
// options actually fitted to that serial.
dcl-proc fl_listPartsForMachine export;
  dcl-pi *n varchar(80);
    serial char(8) const;
    search varchar(40) const;
    parts likeds(fl_part_t) dim(500);
    limit int(10) const;
    returned int(10);
  end-pi;
  dcl-ds r qualified;
    part char(15);
    descr char(40);
    assy char(20);
    qty packed(5:0);
    stock packed(7:0);
    price packed(11:2);
    lead packed(3:0);
    status char(1);
    superby char(15);
    optn char(10);
  end-ds;
  dcl-s model char(10);
  dcl-s pat varchar(44);

  returned = 0;
  exec sql select flmmodl into :model from flmachp where flser = :serial;
  if sqlcode <> 0;
    return 'Machine ' + %trimr(serial) + ' not found.';
  endif;

  // trim before building the pattern: a fixed-length field carries trailing
  // blanks into the varchar host variable and LIKE then matches nothing
  if %len(%trim(search)) = 0;
    pat = '%';
  else;
    pat = '%' + %upper(%trim(search)) + '%';
  endif;

  exec sql declare cPart cursor for
    select b.flbpart, coalesce(p.flpdesc,''), b.flbassy, b.flbqty,
           coalesce(p.flpstock,0), coalesce(p.flpprice,0), coalesce(p.flplead,0),
           coalesce(p.flpstat,''), coalesce(p.flpsuper,''), b.flboptn
      from flbomp b
      left join flpartp p on p.flpart = b.flbpart
     where b.flbmodl = :model
       and (b.flboptn = '' or exists (select 1 from floptnp o
                                       where o.floser = :serial
                                         and o.flocode = b.flboptn))
       and (upper(coalesce(p.flpdesc,'')) like :pat or upper(b.flbpart) like :pat)
     order by b.flbassy, b.flbpos;
  exec sql open cPart;
  if sqlcode < 0;
    return 'Error listing parts. SQLCODE=' + %char(sqlcode);
  endif;
  dow returned < limit and returned < %elem(parts);
    exec sql fetch cPart into :r;
    if sqlcode <> 0;
      leave;
    endif;
    returned += 1;
    clear parts(returned);
    parts(returned).part = r.part;
    parts(returned).descr = r.descr;
    parts(returned).assy = r.assy;
    parts(returned).qty = r.qty;
    parts(returned).stock = r.stock;
    parts(returned).price = r.price;
    parts(returned).lead = r.lead;
    parts(returned).status = r.status;
    parts(returned).statusd = fl_partStatusDesc(r.status);
    parts(returned).superby = r.superby;
    parts(returned).optn = r.optn;
    parts(returned).dupes = fl_countDupes(r.part);
  enddo;
  exec sql close cPart;
  return '';
end-proc;

dcl-proc fl_findDuplicates export;
  dcl-pi *n varchar(80);
    part char(15) const;
    parts likeds(fl_part_t) dim(50);
    limit int(10) const;
    returned int(10);
  end-pi;
  dcl-ds r qualified;
    part char(15);
    descr char(40);
    stock packed(7:0);
    price packed(11:2);
    lead packed(3:0);
    status char(1);
    superby char(15);
    created packed(8:0);
  end-ds;
  dcl-s i int(10);
  dcl-s me int(10);
  dcl-s pn char(15);

  returned = 0;
  fl_loadSigs();
  me = 0;
  for i = 1 to sigCount;
    if sigCache(i).part = part;
      me = i;
      leave;
    endif;
  endfor;
  if me = 0;
    return '';
  endif;
  // one word alone is too weak a signal
  if %scan(' ' : %trim(sigCache(me).words)) = 0;
    return '';
  endif;

  for i = 1 to sigCount;
    if returned >= limit or returned >= %elem(parts);
      leave;
    endif;
    if sigCache(i).words <> sigCache(me).words
       or not fl_sizesOk(sigCache(i).sizes : sigCache(me).sizes);
      iter;
    endif;
    pn = sigCache(i).part;
    exec sql
      select flpart, flpdesc, flpstock, flpprice, flplead, flpstat,
             flpsuper, flpcreat
        into :r
        from flpartp
       where flpart = :pn;
    if sqlcode <> 0;
      iter;
    endif;
    returned += 1;
    clear parts(returned);
    parts(returned).part = r.part;
    parts(returned).descr = r.descr;
    parts(returned).stock = r.stock;
    parts(returned).price = r.price;
    parts(returned).lead = r.lead;
    parts(returned).status = r.status;
    parts(returned).statusd = fl_partStatusDesc(r.status);
    parts(returned).superby = r.superby;
    // created year is useful context in the cluster panel
    parts(returned).assy = %char(%int(r.created / 10000));
  endfor;
  return '';
end-proc;

dcl-proc fl_listServiceEvents export;
  dcl-pi *n varchar(80);
    serial char(8) const;
    events likeds(fl_svc_t) dim(200);
    limit int(10) const;
    returned int(10);
  end-pi;
  dcl-ds r qualified;
    ser char(8);
    svcdate packed(8:0);
    svctype char(1);
    tech char(30);
    descr char(60);
    downhrs packed(5:1);
    cost packed(11:2);
  end-ds;
  returned = 0;
  exec sql declare cSvc cursor for
    select flvser, flvdate, flvtype, flvtech, flvdesc, flvdown, flvcost
      from flsvcp where flvser = :serial order by flvdate desc;
  exec sql open cSvc;
  if sqlcode < 0;
    return 'Error listing service events. SQLCODE=' + %char(sqlcode);
  endif;
  dow returned < limit and returned < %elem(events);
    exec sql fetch cSvc into :r;
    if sqlcode <> 0;
      leave;
    endif;
    returned += 1;
    clear events(returned);
    events(returned).ser = r.ser;
    events(returned).svcdate = fl_fmtDate(r.svcdate);
    events(returned).svctype = r.svctype;
    events(returned).svctyped = fl_svcTypeDesc(r.svctype);
    events(returned).tech = r.tech;
    events(returned).descr = r.descr;
    events(returned).downhrs = r.downhrs;
    events(returned).cost = r.cost;
  enddo;
  exec sql close cSvc;
  return '';
end-proc;

// Free-text customer lookup. Returns fleet size and parts spend alongside the
// name so the list is worth looking at in its own right - it doubles as an
// installed-base-by-account view.
dcl-proc fl_findCustomers export;
  dcl-pi *n varchar(80);
    search varchar(40) const;
    customers likeds(fl_clkp_t) dim(200);
    limit int(10) const;
    returned int(10);
  end-pi;
  dcl-ds r qualified;
    custno   packed(6:0);
    name     char(40);
    city     char(30);
    state    char(2);
    region   char(10);
    mgr      char(30);
    machines int(10);
    sites    int(10);
    spendcur packed(13:2);
    spendpri packed(13:2);
  end-ds;
  dcl-s pat varchar(44);
  dcl-s curFrom packed(8:0);
  dcl-s curTo packed(8:0);
  dcl-s priFrom packed(8:0);
  dcl-s priTo packed(8:0);

  returned = 0;
  if %len(%trim(search)) = 0;
    pat = '%';
  else;
    pat = '%' + %upper(%trim(search)) + '%';
  endif;
  curFrom = (THIS_YEAR - 1) * 10000 + 101;
  curTo   = (THIS_YEAR - 1) * 10000 + 1231;
  priFrom = (THIS_YEAR - 2) * 10000 + 101;
  priTo   = (THIS_YEAR - 2) * 10000 + 1231;

  exec sql declare cCust cursor for
    select c.flcust, c.flcname, c.flccity, c.flcstate, c.flcregn, c.flcmgr,
           coalesce(m.mc, 0), coalesce(m.sc, 0),
           coalesce(p.cur, 0), coalesce(p.pri, 0)
      from flcustp c
      left join (select flmcust, count(*) as mc, count(distinct flmsite) as sc
                   from flmachp group by flmcust) m on m.flmcust = c.flcust
      left join (select flocust,
                        sum(case when flodate between :curFrom and :curTo
                                 then floamt else 0 end) as cur,
                        sum(case when flodate between :priFrom and :priTo
                                 then floamt else 0 end) as pri
                   from flpordp group by flocust) p on p.flocust = c.flcust
     where :pat = '%'
        or upper(c.flcname)  like :pat
        or upper(c.flccity)  like :pat
        or upper(c.flcstate) like :pat
        or upper(c.flcregn)  like :pat
        or upper(c.flcmgr)   like :pat
        or char(c.flcust)    like :pat
     order by c.flcname;
  exec sql open cCust;
  if sqlcode < 0;
    return 'Error searching customers. SQLCODE=' + %char(sqlcode);
  endif;
  dow returned < limit and returned < %elem(customers);
    exec sql fetch cCust into :r;
    if sqlcode <> 0;
      leave;
    endif;
    returned += 1;
    clear customers(returned);
    customers(returned).custno   = r.custno;
    customers(returned).name     = r.name;
    customers(returned).city     = r.city;
    customers(returned).state    = r.state;
    customers(returned).region   = r.region;
    customers(returned).mgr      = r.mgr;
    customers(returned).machines = r.machines;
    customers(returned).sites    = r.sites;
    customers(returned).spendcur = r.spendcur;
    customers(returned).spendpri = r.spendpri;
    // computed in RPG, not SQL: a DEC() cast on the ratio can overflow, and
    // DECIMAL division truncates the scale to zero (see fl_defaultCustomer)
    if r.spendpri > 0;
      customers(returned).trendpct = (r.spendcur - r.spendpri) * 100 / r.spendpri;
    endif;
  enddo;
  exec sql close cCust;
  return '';
end-proc;

// Free-text machine lookup. Deliberately searches the things a parts rep
// actually has to hand - the caller's company, the mine site, the model -
// not just the serial number they are unlikely to know.
dcl-proc fl_findMachines export;
  dcl-pi *n varchar(80);
    search varchar(40) const;
    machines likeds(fl_mach_t) dim(500);
    limit int(10) const;
    returned int(10);
  end-pi;
  dcl-ds r qualified;
    ser char(8);
    model char(10);
    modeldsc char(40);
    custname char(40);
    sitename char(40);
    built packed(8:0);
    status char(1);
  end-ds;
  dcl-s pat varchar(44);

  returned = 0;
  if %len(%trim(search)) = 0;
    pat = '%';
  else;
    pat = '%' + %upper(%trim(search)) + '%';
  endif;

  exec sql declare cFind cursor for
    select m.flser, m.flmmodl, coalesce(d.flmdesc,''), coalesce(c.flcname,''),
           coalesce(s.flsname,''), m.flmbuild, m.flmstat
      from flmachp m
      left join flmodlp d on d.flmodl = m.flmmodl
      left join flcustp c on c.flcust  = m.flmcust
      left join flsitep s on s.flsite  = m.flmsite
     where :pat = '%'
        or upper(m.flser)               like :pat
        or upper(coalesce(c.flcname,''))  like :pat
        or upper(coalesce(s.flsname,''))  like :pat
        or upper(m.flmmodl)             like :pat
        or upper(coalesce(d.flmdesc,'')) like :pat
     order by coalesce(c.flcname,''), m.flser;
  exec sql open cFind;
  if sqlcode < 0;
    return 'Error searching machines. SQLCODE=' + %char(sqlcode);
  endif;
  dow returned < limit and returned < %elem(machines);
    exec sql fetch cFind into :r;
    if sqlcode <> 0;
      leave;
    endif;
    returned += 1;
    clear machines(returned);
    machines(returned).ser      = r.ser;
    machines(returned).model    = r.model;
    machines(returned).modeldsc = r.modeldsc;
    machines(returned).custname = r.custname;
    machines(returned).sitename = r.sitename;
    machines(returned).built    = fl_fmtDate(r.built);
    machines(returned).status   = r.status;
    machines(returned).statusd  = fl_statusDesc(r.status);
  enddo;
  exec sql close cFind;
  return '';
end-proc;

// Opens the fleet screen on the account with the steepest year-over-year
// parts-spend decline - the record worth talking about, chosen by query
// rather than hardcoded.
//
// The DOUBLE() casts matter. Dividing two DECIMAL sums yields a result whose
// scale is truncated to 0, so every ratio came back as 0, the ORDER BY became
// a no-op and this silently returned whichever group happened to be first.
dcl-proc fl_defaultCustomer export;
  dcl-pi *n varchar(80);
    custno like(fl_cust_t.custno);
  end-pi;
  dcl-s curFrom packed(8:0);
  dcl-s curTo packed(8:0);
  dcl-s priFrom packed(8:0);
  dcl-s priTo packed(8:0);
  custno = 0;
  curFrom = (THIS_YEAR - 1) * 10000 + 101;
  curTo   = (THIS_YEAR - 1) * 10000 + 1231;
  priFrom = (THIS_YEAR - 2) * 10000 + 101;
  priTo   = (THIS_YEAR - 2) * 10000 + 1231;
  exec sql
    select flocust into :custno from (
      select flocust,
             sum(case when flodate between :curFrom and :curTo then floamt else 0 end) cur,
             sum(case when flodate between :priFrom and :priTo then floamt else 0 end) pri
        from flpordp group by flocust) t
     where pri > 0
     order by double(cur - pri) / double(pri) asc
     fetch first 1 row only;
  if sqlcode < 0 or custno = 0;
    exec sql select min(flcust) into :custno from flcustp;
  endif;
  return '';
end-proc;

// Opens the parts screen on a machine with a rich fitment list, so the first
// thing on screen is a populated parts table.
dcl-proc fl_defaultMachine export;
  dcl-pi *n varchar(80);
    serial char(8);
  end-pi;
  serial = *blanks;
  exec sql
    select m.flser into :serial
      from flmachp m
      join (select flbmodl, count(*) n from flbomp group by flbmodl) b
        on b.flbmodl = m.flmmodl
     where m.flmstat = 'A'
     order by b.n desc, m.flser
     fetch first 1 row only;
  if sqlcode < 0 or serial = *blanks;
    exec sql select min(flser) into :serial from flmachp;
  endif;
  return '';
end-proc;

// Every number in the insight text is computed here, so anything shown on
// screen can be traced back to a query rather than to generated prose.
dcl-proc fl_getFleetSummary export;
  dcl-pi *n varchar(80);
    p_custno like(fl_cust_t.custno) const;
    summary likeds(fl_sum_t);
  end-pi;
  dcl-ds cust likeds(fl_cust_t);
  dcl-ds machines likeds(fl_mach_t) dim(500);
  dcl-s found ind;
  dcl-s err varchar(80);
  dcl-s n int(10);
  dcl-s i int(10);
  dcl-s curFrom packed(8:0);
  dcl-s curTo packed(8:0);
  dcl-s priFrom packed(8:0);
  dcl-s priTo packed(8:0);
  dcl-s txt varchar(240);

  clear summary;
  summary.custno = p_custno;

  err = fl_getCustomer(p_custno : cust : found);
  if err <> '';
    return err;
  endif;
  if not found;
    return 'Customer not found.';
  endif;
  summary.custname = cust.name;
  summary.region = cust.region;

  exec sql select count(*), count(distinct flmsite)
             into :summary.machines, :summary.sites
             from flmachp where flmcust = :p_custno;

  // two most recent complete years in the data
  curFrom = (THIS_YEAR - 1) * 10000 + 101;
  curTo   = (THIS_YEAR - 1) * 10000 + 1231;
  priFrom = (THIS_YEAR - 2) * 10000 + 101;
  priTo   = (THIS_YEAR - 2) * 10000 + 1231;
  exec sql
    select coalesce(sum(case when flodate between :curFrom and :curTo
                             then floamt else 0 end),0),
           coalesce(sum(case when flodate between :priFrom and :priTo
                             then floamt else 0 end),0)
      into :summary.spendcur, :summary.spendpri
      from flpordp where flocust = :p_custno;

  if summary.spendpri > 0;
    summary.trendpct = (summary.spendcur - summary.spendpri) * 100 / summary.spendpri;
  endif;

  err = fl_listMachines(p_custno : machines : %elem(machines) : n);
  if err <> '';
    return err;
  endif;
  for i = 1 to n;
    if machines(i).modflag <> '';
      summary.modcand += 1;
    endif;
  endfor;

  txt = %char(summary.machines) + ' machines across ' + %char(summary.sites)
      + ' sites. ';
  if summary.modcand > 0;
    txt += %char(summary.modcand) + ' due for modernization review. ';
  else;
    txt += 'No machines currently due for modernization. ';
  endif;
  if summary.spendpri > 0;
    if summary.trendpct < 0;
      txt += 'Parts spend down ' + %char(%abs(%int(summary.trendpct)))
           + '% year over year - possible service gap.';
    else;
      txt += 'Parts spend up ' + %char(%int(summary.trendpct)) + '% year over year.';
    endif;
  else;
    txt += 'No prior-year parts spend to compare.';
  endif;
  summary.insight = txt;
  return '';
end-proc;

// ==================================================================
//  Duplicate review (A7)
//
//  fl_scanDuplicates groups the item master by normalised description
//  signature and writes each multi-member group to FLDUPC/FLDUPM with an
//  explainable confidence score, a plain-language reason, and the blast
//  radius per member. The review utility then applies a decision.
//
//  Confidence is built from named signals so it can be defended:
//     identical description text        40
//     same normalised word set          30   (always true by construction)
//     same size / rating                15
//     already linked by supersession    10
//     same product line                 5
//     prices within 15%                 5
//  Capped at 100. A size mismatch scores nothing and says so - those are
//  usually genuine variants (HOSE 25FT vs 40FT) and belong low in the queue
//  rather than hidden, because only a human can settle them.
// ==================================================================


dcl-proc fl_dupRoleDesc export;
  dcl-pi *n char(12);
    code char(1) const;
  end-pi;
  select;
    when code = 'S';
      return 'Kept';
    when code = 'L';
      return 'Merged in';
    when code = 'X';
      return 'Left alone';
    other;
      return '';
  endsl;
end-proc;

dcl-proc fl_dupStatusDesc export;
  dcl-pi *n char(14);
    code char(1) const;
  end-pi;
  select;
    when code = 'N';
      return 'Not reviewed';
    when code = 'M';
      return 'Merged';
    when code = 'R';
      return 'Not duplicate';
    when code = 'D';
      return 'Deferred';
    other;
      return code;
  endsl;
end-proc;

// Names the era convention a part number follows. Useful to an engineer:
// it explains WHY the duplicate exists rather than just that it does.
dcl-proc fl_styleOf;
  dcl-pi *n char(40);
    part char(15) const;
    descr char(40) const;
  end-pi;
  dcl-s p varchar(15);
  dcl-s hyph int(10);
  dcl-s i int(10);
  p = %trimr(part);
  hyph = 0;
  for i = 1 to %len(p);
    if %subst(p : i : 1) = '-';
      hyph += 1;
    endif;
  endfor;
  if %scan('-STD' : p) > 0 or %scan('(STANDARD)' : %upper(descr)) > 0;
    return 'Short code + STD suffix (recent)';
  endif;
  if %scan(',' : descr) > 0;
    return 'Comma-inverted description';
  endif;
  if hyph >= 2;
    return 'Hyphenated abbreviations (older)';
  endif;
  if hyph = 0;
    return 'Run-together abbreviations';
  endif;
  return 'Mixed convention';
end-proc;

dcl-proc fl_scanDuplicates export;
  dcl-pi *n varchar(80);
    run packed(9:0);
    created int(10);
    skipped int(10);
  end-pi;
  dcl-s i int(10);
  dcl-s j int(10);
  dcl-s m int(10);
  dcl-s n int(10);
  dcl-s mem int(10) dim(50);
  dcl-s cnt int(10);
  dcl-s conf int(10);
  dcl-s reason varchar(250);
  dcl-s reasonOut char(150);
  dcl-s sig char(100);
  dcl-s fullSig varchar(200);
  dcl-s newid packed(9:0);
  dcl-s judged int(10);
  dcl-s descIdent ind;
  dcl-s sizesOk ind;
  dcl-s superLink ind;
  dcl-s sameLine ind;
  dcl-s priceNear ind;
  dcl-s loPrice packed(11:2);
  dcl-s hiPrice packed(11:2);
  dcl-s pn char(15);
  dcl-s nbom int(10);
  dcl-s nord int(10);
  dcl-s totVal packed(13:2);
  dcl-s totBom int(10);
  dcl-s mval packed(11:2);
  dcl-s pctDiff int(10);
  // SQL host variables must be plain scalars - an expression, a procedure
  // call or an array-indexed DS subfield is rejected (SQL0104 / SQL0312),
  // so stage every value into a scalar first.
  dcl-s mwhy char(80);
  dcl-s mstk packed(7:0);

  created = 0;
  skipped = 0;
  fl_loadSigs();
  clear used;

  // Sweep orphaned members. Cluster ids restart if the header file is ever
  // recreated, so leftover member rows would attach themselves to the wrong
  // cluster - which is exactly what happened during development.
  exec sql delete from fldupm
    where fldmid not in (select fldcid from fldupc);

  for i = 1 to sigCount;
    if used(i) or sigCache(i).status = 'O';
      iter;
    endif;
    // a single word is too weak a signal to cluster on
    if %scan(' ' : %trim(sigCache(i).words)) = 0;
      iter;
    endif;

    cnt = 0;
    for j = i to sigCount;
      if used(j) or sigCache(j).status = 'O';
        iter;
      endif;
      // full signature, not just words - see the note above fl_scanDuplicates
      if sigCache(j).words = sigCache(i).words
         and sigCache(j).sizes = sigCache(i).sizes
         and cnt < %elem(mem);
        cnt += 1;
        mem(cnt) = j;
        used(j) = *on;
      endif;
    endfor;
    if cnt < 2;
      iter;
    endif;

    // %subst on a VARCHAR shorter than the requested length raises RNX0100.
    // Plain assignment to a fixed-length field pads or truncates safely.
    fullSig = %trim(sigCache(i).words) + '|' + %trim(sigCache(i).sizes);
    if %len(fullSig) > 100;
      sig = %subst(fullSig : 1 : 100);
    else;
      sig = fullSig;
    endif;

    // already decided? do not raise it again - a tool that re-asks gets
    // abandoned. Deferred counts as decided until the membership changes.
    exec sql
      select count(*) into :judged from fldupc
       where fldckey = :sig and fldccnt = :cnt
         and fldcstat in ('M', 'R', 'D');
    if judged > 0;
      skipped += 1;
      iter;
    endif;
    // clear any prior un-reviewed row for this group before re-inserting
    exec sql delete from fldupm
      where fldmid in (select fldcid from fldupc
                        where fldckey = :sig and fldcstat = 'N');
    exec sql delete from fldupc where fldckey = :sig and fldcstat = 'N';

    // ---- signals
    descIdent = *on;
    sizesOk = *on;
    superLink = *off;
    sameLine = *on;
    loPrice = sigCache(mem(1)).price;
    hiPrice = sigCache(mem(1)).price;
    for m = 2 to cnt;
      if %upper(%trimr(sigCache(mem(m)).descr))
         <> %upper(%trimr(sigCache(mem(1)).descr));
        descIdent = *off;
      endif;
      if sigCache(mem(m)).sizes <> sigCache(mem(1)).sizes;
        sizesOk = *off;
      endif;
      if sigCache(mem(m)).line <> sigCache(mem(1)).line;
        sameLine = *off;
      endif;
      if sigCache(mem(m)).price < loPrice;
        loPrice = sigCache(mem(m)).price;
      endif;
      if sigCache(mem(m)).price > hiPrice;
        hiPrice = sigCache(mem(m)).price;
      endif;
    endfor;
    for m = 1 to cnt;
      for n = 1 to cnt;
        if m <> n and %trimr(sigCache(mem(m)).super) <> ''
           and %trimr(sigCache(mem(m)).super) = %trimr(sigCache(mem(n)).part);
          superLink = *on;
        endif;
      endfor;
    endfor;
    priceNear = *off;
    pctDiff = 0;
    if hiPrice > 0;
      pctDiff = %int((hiPrice - loPrice) * 100 / hiPrice);
      priceNear = pctDiff <= 15;
    endif;

    // Matching normalised descriptions IS the core signal, so it carries the
    // weight. A size/rating mismatch is a penalty rather than a missing
    // bonus - those are usually genuine variants and belong low in the queue.
    // Clusters are grouped on the full signature, so same size/rating is a
    // precondition rather than a signal. Confidence now ranks how well
    // corroborated a cluster is, not whether it is real.
    conf = 60;
    reason = 'Same description and size after expanding abbreviations';
    if superLink;
      conf += 15;
      reason += '; linked by supersession';
    endif;
    if sameLine;
      conf += 10;
    endif;
    if descIdent;
      conf += 10;
      reason += '; identical description text';
    endif;
    if priceNear;
      conf += 5;
      reason += '; prices within 15%';
    else;
      reason += '; prices differ ' + %char(pctDiff) + '%';
    endif;
    if conf > 100;
      conf = 100;
    endif;
    if conf < 5;
      conf = 5;
    endif;
    // the column is char(150); never let the buffer decide
    if %len(reason) > 150;
      reasonOut = %subst(reason : 1 : 150);
    else;
      reasonOut = reason;
    endif;

    // ---- write the cluster
    exec sql select coalesce(max(fldcid), 0) + 1 into :newid from fldupc;
    totVal = 0;
    totBom = 0;
    for m = 1 to cnt;
      pn = sigCache(mem(m)).part;
      exec sql select count(*) into :nbom from flbomp where flbpart = :pn;
      exec sql select count(*) into :nord from flpordp where flopart = :pn;
      mval = sigCache(mem(m)).stock * sigCache(mem(m)).price;
      totVal += mval;
      totBom += nbom;
    endfor;

    exec sql insert into fldupc
      (fldcid, fldcrun, fldckey, fldccnt, fldcconf, fldcreas, fldcstat,
       fldcsurv, fldcuser, fldcdate, fldcnote, fldcval, fldcbom)
      values (:newid, :run, :sig, :cnt, :conf, :reasonOut, 'N',
              '', '', 0, '', :totVal, :totBom);
    if sqlcode < 0;
      return 'Error writing cluster. SQLCODE=' + %char(sqlcode);
    endif;

    for m = 1 to cnt;
      pn = sigCache(mem(m)).part;
      exec sql select count(*) into :nbom from flbomp where flbpart = :pn;
      exec sql select count(*) into :nord from flpordp where flopart = :pn;
      mstk = sigCache(mem(m)).stock;
      mval = mstk * sigCache(mem(m)).price;
      mwhy = fl_styleOf(sigCache(mem(m)).part : sigCache(mem(m)).descr);
      exec sql insert into fldupm
        (fldmid, fldmpart, fldmrole, fldmwhy, fldmbom, fldmord, fldmstk, fldmval)
        values (:newid, :pn, 'U', :mwhy, :nbom, :nord, :mstk, :mval);
    endfor;
    created += 1;
  endfor;
  return '';
end-proc;

dcl-proc fl_dupScorecard export;
  dcl-pi *n varchar(80);
    sc likeds(fl_dupsc_t);
  end-pi;
  clear sc;
  exec sql select count(*) into :sc.parts from flpartp;
  exec sql
    select count(*),
           coalesce(sum(fldccnt), 0),
           coalesce(sum(fldcval), 0),
           coalesce(sum(fldcbom), 0),
           coalesce(sum(case when fldcconf >= 80 then 1 else 0 end), 0),
           coalesce(sum(case when fldcstat = 'N' then 1 else 0 end), 0),
           coalesce(sum(case when fldcstat = 'M' then 1 else 0 end), 0),
           coalesce(sum(case when fldcstat = 'R' then 1 else 0 end), 0),
           coalesce(sum(case when fldcstat = 'D' then 1 else 0 end), 0)
      into :sc.clusters, :sc.involved, :sc.val, :sc.bomlines, :sc.c80,
           :sc.cnew, :sc.cmerged, :sc.crej, :sc.cdef
      from fldupc;
  if sqlcode < 0;
    return 'Error reading scorecard. SQLCODE=' + %char(sqlcode);
  endif;
  return '';
end-proc;

dcl-proc fl_listClusters export;
  dcl-pi *n varchar(80);
    statusFilter char(1) const;
    clusters likeds(fl_dupc_t) dim(500);
    limit int(10) const;
    returned int(10);
  end-pi;
  dcl-ds r qualified;
    id packed(9:0);
    run packed(9:0);
    sig char(60);
    cnt packed(3:0);
    conf packed(3:0);
    reason char(150);
    status char(1);
    survivor char(15);
    user char(18);
    revdate packed(8:0);
    note char(120);
    val packed(13:2);
    bom packed(5:0);
    topdesc char(40);
  end-ds;
  dcl-s f char(1);
  f = statusFilter;
  returned = 0;
  exec sql declare cClu cursor for
    select c.fldcid, c.fldcrun, c.fldckey, c.fldccnt, c.fldcconf, c.fldcreas,
           c.fldcstat, c.fldcsurv, c.fldcuser, c.fldcdate, c.fldcnote,
           c.fldcval, c.fldcbom,
           coalesce((select min(p.flpdesc) from fldupm m
                       join flpartp p on p.flpart = m.fldmpart
                      where m.fldmid = c.fldcid), '')
      from fldupc c
     where :f = '*' or c.fldcstat = :f
     order by c.fldcconf desc, c.fldcval desc, c.fldcid;
  exec sql open cClu;
  if sqlcode < 0;
    return 'Error listing clusters. SQLCODE=' + %char(sqlcode);
  endif;
  dow returned < limit and returned < %elem(clusters);
    exec sql fetch cClu into :r;
    if sqlcode <> 0;
      leave;
    endif;
    returned += 1;
    clear clusters(returned);
    clusters(returned).id       = r.id;
    clusters(returned).run      = r.run;
    clusters(returned).sig      = r.sig;
    clusters(returned).cnt      = r.cnt;
    clusters(returned).conf     = r.conf;
    clusters(returned).reason   = r.reason;
    clusters(returned).status   = r.status;
    clusters(returned).statusd  = fl_dupStatusDesc(r.status);
    clusters(returned).survivor = r.survivor;
    clusters(returned).user     = r.user;
    clusters(returned).revdate  = fl_fmtDate(r.revdate);
    clusters(returned).note     = r.note;
    clusters(returned).val      = r.val;
    clusters(returned).bom      = r.bom;
    clusters(returned).topdesc  = r.topdesc;
  enddo;
  exec sql close cClu;
  return '';
end-proc;

dcl-proc fl_getCluster export;
  dcl-pi *n varchar(80);
    id packed(9:0) const;
    hdr likeds(fl_dupc_t);
    found ind;
  end-pi;
  dcl-ds r qualified;
    id packed(9:0);
    run packed(9:0);
    sig char(60);
    cnt packed(3:0);
    conf packed(3:0);
    reason char(150);
    status char(1);
    survivor char(15);
    user char(18);
    revdate packed(8:0);
    note char(120);
    val packed(13:2);
    bom packed(5:0);
  end-ds;
  found = *off;
  clear hdr;
  exec sql
    select fldcid, fldcrun, fldckey, fldccnt, fldcconf, fldcreas, fldcstat,
           fldcsurv, fldcuser, fldcdate, fldcnote, fldcval, fldcbom
      into :r from fldupc where fldcid = :id;
  if sqlcode < 0;
    return 'Error reading cluster. SQLCODE=' + %char(sqlcode);
  endif;
  if sqlcode = 100;
    return '';
  endif;
  found = *on;
  hdr.id = r.id;
  hdr.run = r.run;
  hdr.sig = r.sig;
  hdr.cnt = r.cnt;
  hdr.conf = r.conf;
  hdr.reason = r.reason;
  hdr.status = r.status;
  hdr.statusd = fl_dupStatusDesc(r.status);
  hdr.survivor = r.survivor;
  hdr.user = r.user;
  hdr.revdate = fl_fmtDate(r.revdate);
  hdr.note = r.note;
  hdr.val = r.val;
  hdr.bom = r.bom;
  return '';
end-proc;

dcl-proc fl_listClusterMembers export;
  dcl-pi *n varchar(80);
    id packed(9:0) const;
    members likeds(fl_dupm_t) dim(50);
    limit int(10) const;
    returned int(10);
  end-pi;
  dcl-ds r qualified;
    part char(15);
    role char(1);
    why char(80);
    bom packed(5:0);
    ord packed(5:0);
    stock packed(7:0);
    val packed(11:2);
    descr char(40);
    created packed(8:0);
    status char(1);
    super char(15);
    price packed(11:2);
  end-ds;
  returned = 0;
  exec sql declare cMem cursor for
    select m.fldmpart, m.fldmrole, m.fldmwhy, m.fldmbom, m.fldmord,
           m.fldmstk, m.fldmval,
           coalesce(p.flpdesc, ''), coalesce(p.flpcreat, 0),
           coalesce(p.flpstat, ''), coalesce(p.flpsuper, ''),
           coalesce(p.flpprice, 0)
      from fldupm m
      left join flpartp p on p.flpart = m.fldmpart
     where m.fldmid = :id
     order by p.flpcreat, m.fldmpart;
  exec sql open cMem;
  if sqlcode < 0;
    return 'Error listing members. SQLCODE=' + %char(sqlcode);
  endif;
  dow returned < limit and returned < %elem(members);
    exec sql fetch cMem into :r;
    if sqlcode <> 0;
      leave;
    endif;
    returned += 1;
    clear members(returned);
    members(returned).part    = r.part;
    members(returned).descr   = r.descr;
    members(returned).created = fl_fmtDate(r.created);
    members(returned).status  = r.status;
    members(returned).statusd = fl_partStatusDesc(r.status);
    members(returned).super   = r.super;
    members(returned).stock   = r.stock;
    members(returned).price   = r.price;
    members(returned).val     = r.val;
    members(returned).bom     = r.bom;
    members(returned).ord     = r.ord;
    members(returned).role    = r.role;
    members(returned).roled   = fl_dupRoleDesc(r.role);
    members(returned).why     = r.why;
  enddo;
  exec sql close cMem;
  return '';
end-proc;

// Applies a review decision. On a merge the losers are marked superseded and
// pointed at the survivor; the cluster records who decided and why. Nothing
// is deleted - supersession is reversible, deletion is not.
dcl-proc fl_resolveCluster export;
  dcl-pi *n varchar(80);
    id packed(9:0) const;
    survivor char(15) const;
    mergeList char(15) dim(50) const options(*varsize);
    mergeCount int(10) const;
    decision char(1) const;
    note char(120) const;
    user char(18) const;
  end-pi;
  dcl-s today packed(8:0);
  dcl-s surv char(15);
  dcl-s d char(1);
  dcl-s n char(120);
  dcl-s u char(18);
  dcl-s i int(10);
  dcl-s pn char(15);

  d = decision;
  surv = survivor;
  n = note;
  u = user;
  today = %dec(%char(%date() : *iso0) : 8 : 0);

  if d = 'M';
    if %trimr(surv) = '';
      return 'Type 1 against the part you want to keep.';
    endif;
    if mergeCount < 1;
      return 'Type 2 against at least one part to merge into the kept part.';
    endif;
  endif;

  if d = 'M';
    // everything starts excluded; only the marked members become losers
    exec sql update fldupm set fldmrole = 'X' where fldmid = :id;
    exec sql update fldupm set fldmrole = 'S'
      where fldmid = :id and fldmpart = :surv;
    for i = 1 to mergeCount;
      pn = mergeList(i);
      if %trimr(pn) = '' or %trimr(pn) = %trimr(surv);
        iter;
      endif;
      exec sql update fldupm set fldmrole = 'L'
        where fldmid = :id and fldmpart = :pn;
      exec sql update flpartp set flpstat = 'S', flpsuper = :surv
        where flpart = :pn;
      if sqlcode < 0;
        return 'Error updating part ' + %trimr(pn) + '. SQLCODE='
             + %char(sqlcode);
      endif;
    endfor;
    // the survivor stays active and points at nothing
    exec sql update flpartp set flpstat = 'A', flpsuper = ''
      where flpart = :surv;
  endif;

  exec sql update fldupc
     set fldcstat = :d, fldcsurv = :surv, fldcuser = :u,
         fldcdate = :today, fldcnote = :n
   where fldcid = :id;
  if sqlcode < 0;
    return 'Error updating cluster. SQLCODE=' + %char(sqlcode);
  endif;
  sigLoaded = *off;
  sigCount = 0;
  return '';
end-proc;

// ==========================================================================
// A4 production schedule board                                     (GJA-917)
//
// Everything the board shows is derived here, so the board, the work order
// detail and the data-confidence screen cannot disagree with each other.
//
// The one thing this code will not do is guess. Where the data does not
// support a projection - no routing, no promised date, material with no
// arrival date - it returns no projection and the scan reports why, rather
// than producing a plausible-looking date. A schedule that quietly invents
// its inputs is worse than no schedule, and the whole point of the
// confidence panel is to say so out loud.
// ==========================================================================

// ------------------------------------------------------------ date helpers
dcl-proc fl_dateOk;
  dcl-pi *n ind;
    ymd packed(8:0) const;
  end-pi;
  dcl-s d date(*iso);
  if ymd < 19000101 or ymd > 29991231;
    return *off;
  endif;
  monitor;
    d = %date(ymd : *iso);
  on-error;
    return *off;
  endmon;
  return *on;
end-proc;

dcl-proc fl_today;
  dcl-pi *n packed(8:0);
  end-pi;
  return %dec(%char(%date() : *iso0) : 8 : 0);
end-proc;

dcl-proc fl_daysBetween;
  dcl-pi *n int(10);
    fromYmd packed(8:0) const;
    toYmd packed(8:0) const;
  end-pi;
  if not fl_dateOk(fromYmd) or not fl_dateOk(toYmd);
    return 0;
  endif;
  return %diff(%date(toYmd : *iso) : %date(fromYmd : *iso) : *days);
end-proc;

// RPG has no weekday BIF, so weekday comes from the distance to a known
// Monday. 2000-01-03 was a Monday; 0 = Monday through 6 = Sunday.
dcl-proc fl_isWeekend;
  dcl-pi *n ind;
    d date(*iso) const;
  end-pi;
  dcl-s n int(10);
  n = %rem(%diff(d : d'2000-01-03' : *days) : 7);
  if n < 0;
    n += 7;
  endif;
  return n >= 5;
end-proc;

// Shop days only. A capacity plan that counts weekends ships every order
// two days early and is wrong in the customer's favour, which is worse.
dcl-proc fl_addWorkDays;
  dcl-pi *n packed(8:0);
    fromYmd packed(8:0) const;
    days int(10) const;
  end-pi;
  dcl-s d date(*iso);
  dcl-s n int(10) inz(0);
  dcl-s guard int(10) inz(0);
  if not fl_dateOk(fromYmd);
    return 0;
  endif;
  d = %date(fromYmd : *iso);
  dow n < days and guard < 5000;
    d += %days(1);
    guard += 1;
    if not fl_isWeekend(d);
      n += 1;
    endif;
  enddo;
  return %dec(%char(d : *iso0) : 8 : 0);
end-proc;

// ------------------------------------------------------- coded value text
dcl-proc fl_woTypeDesc;
  dcl-pi *n char(16);
    code char(1) const;
  end-pi;
  select;
    when code = 'N';
      return 'New build';
    when code = 'M';
      return 'Modernization';
    when code = 'R';
      return 'Rebuild';
    when code = 'E';
      return 'Eng Solutions';
    other;
      return code;
  endsl;
end-proc;

dcl-proc fl_woStatusDesc;
  dcl-pi *n char(12);
    code char(1) const;
  end-pi;
  select;
    when code = 'P';
      return 'Planned';
    when code = 'R';
      return 'Released';
    when code = 'I';
      return 'In process';
    when code = 'H';
      return 'On hold';
    when code = 'C';
      return 'Complete';
    other;
      return code;
  endsl;
end-proc;

dcl-proc fl_prioDesc;
  dcl-pi *n char(8);
    code char(1) const;
  end-pi;
  select;
    when code = 'H';
      return 'High';
    when code = 'M';
      return 'Medium';
    when code = 'L';
      return 'Low';
    other;
      return code;
  endsl;
end-proc;

dcl-proc fl_opStatusDesc;
  dcl-pi *n char(12);
    code char(1) const;
  end-pi;
  select;
    when code = 'W';
      return 'Waiting';
    when code = 'R';
      return 'Running';
    when code = 'C';
      return 'Complete';
    when code = 'H';
      return 'On hold';
    other;
      return code;
  endsl;
end-proc;

dcl-proc fl_wcStatusDesc;
  dcl-pi *n char(10);
    code char(1) const;
  end-pi;
  select;
    when code = 'A';
      return 'Active';
    when code = 'I';
      return 'Inactive';
    other;
      return code;
  endsl;
end-proc;

dcl-proc fl_sevDesc;
  dcl-pi *n char(8);
    code char(1) const;
  end-pi;
  select;
    when code = 'H';
      return 'High';
    when code = 'M';
      return 'Medium';
    when code = 'L';
      return 'Low';
    other;
      return code;
  endsl;
end-proc;

// Hours left on an operation. One rule, used by the order KPI, the work
// centre load and the projection, so the three cannot drift apart: a
// completed operation has none left, one that has reported time has its
// standard less what it has booked, anything else has its full standard.
dcl-proc fl_opRemaining;
  dcl-pi *n packed(5:1);
    std packed(5:1) const;
    act packed(5:1) const;
    status char(1) const;
  end-pi;
  if status = 'C';
    return 0;
  endif;
  if act > 0;
    if act >= std;
      return 0;
    endif;
    return std - act;
  endif;
  return std;
end-proc;

dcl-proc fl_prioRank;
  dcl-pi *n int(10);
    prio char(1) const;
  end-pi;
  select;
    when prio = 'H';
      return 1;
    when prio = 'M';
      return 2;
    other;
      return 3;
  endsl;
end-proc;

dcl-proc fl_wcIndex;
  dcl-pi *n int(10);
    code char(6) const;
  end-pi;
  dcl-s i int(10);
  for i = 1 to wcCount;
    if wcCache(i).code = code;
      return i;
    endif;
  endfor;
  return 0;
end-proc;

dcl-proc fl_woIndex;
  dcl-pi *n int(10);
    wo packed(8:0) const;
  end-pi;
  dcl-s i int(10);
  for i = 1 to woCount;
    if woCache(i).wo = wo;
      return i;
    endif;
  endfor;
  return 0;
end-proc;

// -------------------------------------------------------- confidence scan
// Fourteen checks, every one of them a condition that a real scheduling
// application trips over. They read the loaded data, never a list of known
// problems, so they keep finding things after the data changes - which is
// the only version of this that is worth showing anybody.
//
// Severity is the honest distinction between "this projection is degraded"
// and "this projection is meaningless": only High counts against an order in
// the trust percentage.
dcl-proc fl_dqAdd;
  dcl-pi *n;
    sev char(1) const;
    cat char(26) const;
    wo packed(8:0) const;
    subject char(22) const;
    finding char(110) const;
    impact char(90) const;
  end-pi;
  if dqCount >= %elem(dqCache);
    return;
  endif;
  dqCount += 1;
  clear dqCache(dqCount);
  dqCache(dqCount).sev = sev;
  dqCache(dqCount).sevd = fl_sevDesc(sev);
  dqCache(dqCount).cat = cat;
  dqCache(dqCount).wo = wo;
  dqCache(dqCount).subject = subject;
  dqCache(dqCount).finding = finding;
  dqCache(dqCount).impact = impact;
end-proc;

dcl-proc fl_dqScan;
  dcl-pi *n;
  end-pi;
  dcl-s i int(10);
  dcl-s j int(10);
  dcl-s wc int(10);
  dcl-s t varchar(200);
  dcl-s subj varchar(60);
  dcl-s pctDiff int(10);

  dqCount = 0;

  // ---------------------------------------------------------- high severity
  for i = 1 to woCount;
    // 1. a promised date that precedes the order it belongs to
    if woRaw(i).promised > 0 and woRaw(i).opened > 0
       and woRaw(i).promised < woRaw(i).opened;
      t = 'Promised ship ' + %trim(woCache(i).promised)
        + ' is earlier than the order date ' + %trim(woCache(i).opened) + '.';
      fl_dqAdd('H' : 'Impossible date' : woCache(i).wo : 'Order header' : t
             : 'Order reads late from the day it was entered.');
    endif;

    // 2. in process with nothing to measure elapsed time from
    if woCache(i).status = 'I' and woRaw(i).started = 0;
      fl_dqAdd('H' : 'Missing start date' : woCache(i).wo : 'Order header'
             : 'Work order is in process but carries no actual start date.'
             : 'Cycle time and elapsed time cannot be computed.');
    endif;

    // 3. released or running with no promised ship date at all
    if (woCache(i).status = 'R' or woCache(i).status = 'I')
       and woRaw(i).promised = 0;
      fl_dqAdd('H' : 'No promised ship date' : woCache(i).wo : 'Order header'
             : 'Order is released to the floor with no promised ship date.'
             : 'Nothing to schedule backwards from or measure against.');
    endif;

    // 4. no routing: the order consumes capacity no plan can see
    if woCache(i).ops = 0 and woCache(i).status <> 'C';
      fl_dqAdd('H' : 'No routing operations' : woCache(i).wo : 'Order header'
             : 'Work order has no routing operations, so it carries no hours.'
             : 'Consumes shop capacity that no schedule can account for.');
    endif;

    // 5. current work centre that is not in the work centre file
    if woCache(i).wctr <> '' and fl_wcIndex(woCache(i).wctr) = 0;
      t = 'Current work centre ' + %trim(woCache(i).wctr)
        + ' does not exist in the work centre file.';
      fl_dqAdd('H' : 'Unknown work centre' : woCache(i).wo
             : %trim(woCache(i).wctr) : t
             : 'Hours on this order roll up to no work centre at all.');
    endif;

    // 6. in process but nobody can say where
    if woCache(i).status = 'I' and woCache(i).wctr = '';
      fl_dqAdd('H' : 'No current work centre' : woCache(i).wo : 'Order header'
             : 'Work order is in process but no current work centre is set.'
             : 'Order does not appear on any shop floor view.');
    endif;
  endfor;

  // 7. operations queued at a work centre that has been stood down
  for i = 1 to opCount;
    if opCache(i).status = 'C';
      iter;
    endif;
    wc = fl_wcIndex(opCache(i).wctr);
    if wc > 0 and wcCache(wc).status <> 'A';
      subj = 'Op ' + %char(opCache(i).seq) + ' / ' + %trim(opCache(i).wctr);
      t = 'Operation ' + %char(opCache(i).seq) + ' is routed to '
        + %trim(opCache(i).wctr) + ', which is marked inactive.';
      fl_dqAdd('H' : 'Inactive work centre' : opCache(i).wo : subj : t
             : 'Work is queued at a centre with no crew assigned.');
    elseif wc = 0;
      subj = 'Op ' + %char(opCache(i).seq) + ' / ' + %trim(opCache(i).wctr);
      t = 'Operation ' + %char(opCache(i).seq) + ' is routed to '
        + %trim(opCache(i).wctr) + ', which is not a known work centre.';
      fl_dqAdd('H' : 'Unknown work centre' : opCache(i).wo : subj : t
             : 'These hours are missing from every capacity figure.');
    endif;
  endfor;

  // -------------------------------------------------------- medium severity
  for i = 1 to woCount;
    // 8. complete, but on-time delivery cannot be measured
    if woCache(i).status = 'C' and woRaw(i).compl = 0;
      fl_dqAdd('M' : 'Missing completion date' : woCache(i).wo : 'Order header'
             : 'Work order is complete but carries no completion date.'
             : 'On-time delivery cannot be measured for this order.');
    endif;

    // 9. a model code with no master record behind it
    if not woModelOk(i);
      t = 'Model ' + %trim(woCache(i).model) + ' is not in the model file.';
      fl_dqAdd('M' : 'Unknown model' : woCache(i).wo : %trim(woCache(i).model)
             : t : 'No product line or standard routing for this build.');
    endif;

    // 10. the board and the floor disagree about the same order
    if woCache(i).ops > 0;
      pctDiff = woCache(i).pctrptd - woCache(i).pctroute;
      if pctDiff < 0;
        pctDiff = -pctDiff;
      endif;
      if pctDiff > 20;
        t = 'Reported ' + %char(woCache(i).pctrptd) + '% complete; routing '
          + 'shows ' + %char(woCache(i).pctroute) + '%.';
        fl_dqAdd('M' : 'Reported percent conflict' : woCache(i).wo
               : 'Order header' : t
               : 'Board and shop floor disagree on this order.');
      endif;
    endif;
  endfor;

  // 11. completed work with no labour behind it
  for i = 1 to opCount;
    if opCache(i).status = 'C' and opCache(i).act = 0 and opCache(i).std > 0;
      subj = 'Op ' + %char(opCache(i).seq) + ' / ' + %trim(opCache(i).wctr);
      t = 'Operation ' + %char(opCache(i).seq) + ' at '
        + %trim(opCache(i).wctr) + ' is complete with no actual hours.';
      fl_dqAdd('M' : 'Zero actual hours' : opCache(i).wo : subj : t
             : 'Standard hours cannot be validated at this work centre.');
    endif;
    // 12. work the capacity plan is treating as free
    if opCache(i).std = 0;
      subj = 'Op ' + %char(opCache(i).seq) + ' / ' + %trim(opCache(i).wctr);
      t = 'Operation ' + %char(opCache(i).seq) + ' at '
        + %trim(opCache(i).wctr) + ' carries no standard hours.';
      fl_dqAdd('M' : 'No standard hours' : opCache(i).wo : subj : t
             : 'Capacity plan treats this operation as taking no time.');
    endif;
  endfor;

  for i = 1 to shCount;
    if shCache(i).status <> 'O';
      iter;
    endif;
    // 13. material that was due and never landed
    if shCache(i).due > 0 and shCache(i).due < schedToday;
      t = 'Part ' + %trim(shCache(i).part) + ' was expected '
        + %trim(fl_fmtDate(shCache(i).due)) + ' and has not arrived.';
      fl_dqAdd('M' : 'Shortage overdue' : shCache(i).wo
             : %trim(shCache(i).part) : t
             : 'Projection cannot be pushed out - no arrival date exists.');
    endif;
    // 14. a shortage with nothing on order behind it
    if shCache(i).po = '';
      t = 'Part ' + %trim(shCache(i).part)
        + ' is short with no purchase order raised.';
      fl_dqAdd('M' : 'Shortage with no PO' : shCache(i).wo
             : %trim(shCache(i).part) : t
             : 'No expected date exists for this material at all.');
    endif;
  endfor;

  // --- tally back onto the orders, so the board can mark the rows whose
  //     projection should not be relied on
  for i = 1 to woCount;
    woCache(i).dqcount = 0;
    woCache(i).dqhigh = 0;
  endfor;
  for i = 1 to dqCount;
    j = fl_woIndex(dqCache(i).wo);
    if j = 0;
      iter;
    endif;
    woCache(j).dqcount += 1;
    if dqCache(i).sev = 'H';
      woCache(j).dqhigh += 1;
    endif;
  endfor;
end-proc;

// ------------------------------------------------------------- cache load
// One pass over the four schedule files, then everything the screens need
// is computed in memory. Fourteen separate confidence checks over 44 orders
// and 357 operations is exactly the shape of query that is cheap in an array
// and expensive as fourteen round trips.
dcl-proc fl_loadSchedule;
  dcl-pi *n varchar(80);
  end-pi;
  dcl-ds w qualified;
    wo packed(8:0);
    serial char(8);
    model char(10);
    custno packed(6:0);
    custname char(40);
    siteno packed(6:0);
    sitename char(40);
    otype char(1);
    status char(1);
    prio char(1);
    opened packed(8:0);
    promised packed(8:0);
    started packed(8:0);
    compl packed(8:0);
    wctr char(6);
    pctrptd packed(3:0);
    value packed(11:2);
    descr char(40);
    note char(60);
    modeldsc char(40);
    modelok char(1);
  end-ds;
  dcl-ds c qualified;
    code char(6);
    descr char(40);
    seq packed(3:0);
    cap packed(5:1);
    crew packed(3:0);
    shifts packed(1:0);
    status char(1);
  end-ds;
  dcl-ds o qualified;
    wo packed(8:0);
    seq packed(3:0);
    wctr char(6);
    descr char(40);
    std packed(5:1);
    act packed(5:1);
    status char(1);
    started packed(8:0);
    compl packed(8:0);
  end-ds;
  dcl-ds h qualified;
    wo packed(8:0);
    part char(15);
    seq packed(3:0);
    reqd packed(5:0);
    avail packed(5:0);
    due packed(8:0);
    status char(1);
    po char(8);
    vend char(30);
    price packed(11:2);
  end-ds;
  dcl-s i int(10);
  dcl-s j int(10);
  dcl-s k int(10);
  dcl-s wc int(10);
  dcl-s cursor packed(8:0);
  dcl-s share float(8);
  dcl-s dayf float(8);
  dcl-s days int(10);
  dcl-s counted ind dim(50);
  dcl-s wcClock packed(8:0) dim(50);
  dcl-s ord int(10) dim(300);
  dcl-s nord int(10) inz(0);
  dcl-s p int(10);
  dcl-s best int(10);
  dcl-s tmp int(10);
  dcl-s ra int(10);
  dcl-s rb int(10);
  dcl-s da packed(8:0);
  dcl-s db packed(8:0);
  dcl-s cap packed(5:1);
  dcl-s rem packed(5:1);

  if schedLoaded;
    return '';
  endif;
  schedToday = fl_today();
  wcCount = 0;
  woCount = 0;
  opCount = 0;
  shCount = 0;

  // --- work centres
  exec sql declare cWc cursor for
    select flwctr, flwcdesc, flwcseq, flwccap, flwccrew, flwcshft, flwcstat
      from flwcp order by flwcseq, flwctr;
  exec sql open cWc;
  if sqlcode < 0;
    return 'Error reading work centres. SQLCODE=' + %char(sqlcode);
  endif;
  dow wcCount < %elem(wcCache);
    exec sql fetch cWc into :c;
    if sqlcode <> 0;
      leave;
    endif;
    wcCount += 1;
    clear wcCache(wcCount);
    wcCache(wcCount).code = c.code;
    wcCache(wcCount).descr = c.descr;
    wcCache(wcCount).seq = c.seq;
    wcCache(wcCount).cap = c.cap;
    wcCache(wcCount).crew = c.crew;
    wcCache(wcCount).shifts = c.shifts;
    wcCache(wcCount).status = c.status;
  enddo;
  exec sql close cWc;

  // --- work orders. modelok comes back from the join so an unknown model
  //     is a fact about the data rather than a second query per row.
  exec sql declare cWo cursor for
    select w.flwo, w.flwser, w.flwmodl, w.flwcust, coalesce(cu.flcname,''),
           w.flwsite, coalesce(si.flsname,''), w.flwtype, w.flwstat,
           w.flwprio, w.flwopen, w.flwprom, w.flwstrt, w.flwcomp, w.flwwctr,
           w.flwpct, w.flwvalue, w.flwdesc, w.flwnote,
           coalesce(mo.flmdesc,''),
           case when mo.flmodl is null then 'N' else 'Y' end
      from flwop w
      left join flcustp cu on cu.flcust = w.flwcust
      left join flsitep si on si.flsite = w.flwsite
      left join flmodlp mo on mo.flmodl = w.flwmodl
     order by w.flwo;
  exec sql open cWo;
  if sqlcode < 0;
    return 'Error reading work orders. SQLCODE=' + %char(sqlcode);
  endif;
  dow woCount < %elem(woCache);
    exec sql fetch cWo into :w;
    if sqlcode <> 0;
      leave;
    endif;
    woCount += 1;
    clear woCache(woCount);
    woCache(woCount).wo = w.wo;
    woCache(woCount).serial = w.serial;
    woCache(woCount).model = w.model;
    woCache(woCount).modeldsc = w.modeldsc;
    woCache(woCount).custno = w.custno;
    woCache(woCount).custname = w.custname;
    woCache(woCount).siteno = w.siteno;
    woCache(woCount).sitename = w.sitename;
    woCache(woCount).otype = w.otype;
    woCache(woCount).otyped = fl_woTypeDesc(w.otype);
    woCache(woCount).status = w.status;
    woCache(woCount).statusd = fl_woStatusDesc(w.status);
    woCache(woCount).prio = w.prio;
    woCache(woCount).priod = fl_prioDesc(w.prio);
    woCache(woCount).opened = fl_fmtDate(w.opened);
    woCache(woCount).promised = fl_fmtDate(w.promised);
    woCache(woCount).started = fl_fmtDate(w.started);
    woCache(woCount).compl = fl_fmtDate(w.compl);
    woCache(woCount).wctr = w.wctr;
    woCache(woCount).pctrptd = w.pctrptd;
    woCache(woCount).value = w.value;
    woCache(woCount).descr = w.descr;
    woCache(woCount).note = w.note;
    woModelOk(woCount) = (w.modelok = 'Y');
    // The projection walk and the confidence scan both need the stored
    // dates, not the formatted display copies, so they are kept separately
    // rather than parsed back out of char(10) text.
    woRaw(woCount).opened = w.opened;
    woRaw(woCount).promised = w.promised;
    woRaw(woCount).started = w.started;
    woRaw(woCount).compl = w.compl;
    wc = fl_wcIndex(w.wctr);
    if wc > 0;
      woCache(woCount).wctrd = wcCache(wc).descr;
    elseif w.wctr <> '';
      woCache(woCount).wctrd = '(not a known work centre)';
    endif;
  enddo;
  exec sql close cWo;

  // --- routing
  exec sql declare cOps cursor for
    select flrwo, flrseq, flrwctr, flrdesc, flrsthr, flrachr, flrstat,
           flrstrt, flrcomp
      from flroutp order by flrwo, flrseq;
  exec sql open cOps;
  if sqlcode < 0;
    return 'Error reading routing. SQLCODE=' + %char(sqlcode);
  endif;
  dow opCount < %elem(opCache);
    exec sql fetch cOps into :o;
    if sqlcode <> 0;
      leave;
    endif;
    opCount += 1;
    opCache(opCount).wo = o.wo;
    opCache(opCount).seq = o.seq;
    opCache(opCount).wctr = o.wctr;
    opCache(opCount).descr = o.descr;
    opCache(opCount).std = o.std;
    opCache(opCount).act = o.act;
    opCache(opCount).status = o.status;
    opCache(opCount).started = o.started;
    opCache(opCount).compl = o.compl;
  enddo;
  exec sql close cOps;

  // --- shortages
  exec sql declare cSh cursor for
    select s.flshwo, s.flshpart, s.flshseq, s.flshqtyr, s.flshqtya,
           s.flshdue, s.flshstat, s.flshpo, s.flshvend,
           coalesce(p.flpprice, 0)
      from flshrtp s
      left join flpartp p on p.flpart = s.flshpart
     order by s.flshwo, s.flshseq, s.flshpart;
  exec sql open cSh;
  if sqlcode < 0;
    return 'Error reading shortages. SQLCODE=' + %char(sqlcode);
  endif;
  dow shCount < %elem(shCache);
    exec sql fetch cSh into :h;
    if sqlcode <> 0;
      leave;
    endif;
    shCount += 1;
    shCache(shCount).wo = h.wo;
    shCache(shCount).part = h.part;
    shCache(shCount).seq = h.seq;
    shCache(shCount).reqd = h.reqd;
    shCache(shCount).avail = h.avail;
    shCache(shCount).due = h.due;
    shCache(shCount).status = h.status;
    shCache(shCount).po = h.po;
    shCache(shCount).vend = h.vend;
    shCache(shCount).price = h.price;
  enddo;
  exec sql close cSh;

  // --- routed hours per order, and load per work centre
  for i = 1 to opCount;
    j = fl_woIndex(opCache(i).wo);
    if j = 0;
      iter;
    endif;
    woCache(j).ops += 1;
    woCache(j).stdhrs += opCache(i).std;
    woCache(j).acthrs += opCache(i).act;
    if opCache(i).status = 'C';
      woCache(j).opsdone += 1;
    endif;
    woCache(j).remhrs +=
      fl_opRemaining(opCache(i).std : opCache(i).act : opCache(i).status);
  endfor;
  for i = 1 to woCount;
    if woCache(i).stdhrs > 0;
      woCache(i).pctroute =
        %int((woCache(i).stdhrs - woCache(i).remhrs) * 100 / woCache(i).stdhrs);
    endif;
  endfor;

  // Queue depth: how many live orders still have work routed at each centre.
  // The projection divides a centre's daily hours by this, which is the
  // honest reading - a crew of nine is not nine crews.
  for i = 1 to woCount;
    if woCache(i).status = 'C';
      iter;
    endif;
    clear counted;
    for j = 1 to opCount;
      if opCache(j).wo <> woCache(i).wo or opCache(j).status = 'C';
        iter;
      endif;
      wc = fl_wcIndex(opCache(j).wctr);
      if wc = 0;
        iter;
      endif;
      wcCache(wc).loadhrs +=
        fl_opRemaining(opCache(j).std : opCache(j).act : opCache(j).status);
      if not counted(wc);
        counted(wc) = *on;
        wcCache(wc).queued += 1;
      endif;
    endfor;
    wc = fl_wcIndex(woCache(i).wctr);
    if wc > 0 and woCache(i).status = 'I';
      wcCache(wc).wip += 1;
    endif;
  endfor;

  // --- open shortages per order
  for i = 1 to shCount;
    if shCache(i).status <> 'O';
      iter;
    endif;
    j = fl_woIndex(shCache(i).wo);
    if j = 0;
      iter;
    endif;
    woCache(j).shorts += 1;
    if shCache(i).due > 0 and shCache(i).due < schedToday;
      woCache(j).shortlat += 1;
    endif;
  endfor;

  // --- projected ship: a forward finite-capacity load
  //
  // The first version of this divided each work centre's daily hours by the
  // number of orders queued on it and walked every order independently. That
  // double-counts contention - an order only competes for a centre while it
  // is actually there - and it put 27 of 36 orders at risk, which is not a
  // schedule, it is an artefact.
  //
  // This is the textbook version instead, and it says in one sentence what it
  // did: the orders are loaded onto the work centres in promised-date order,
  // each centre working one order at a time at its own daily capacity, and an
  // operation cannot start before its material has landed.
  clear wcClock;
  for i = 1 to wcCount;
    wcClock(i) = schedToday;
  endfor;

  // Load sequence: promised date, then priority, then work order number.
  // Undated orders load last - they have nothing to be early or late for.
  nord = 0;
  for i = 1 to woCount;
    if woCache(i).status = 'C' or woCache(i).ops = 0;
      iter;
    endif;
    nord += 1;
    ord(nord) = i;
  endfor;
  for i = 1 to nord - 1;
    best = i;
    for j = i + 1 to nord;
      da = woRaw(ord(best)).promised;
      db = woRaw(ord(j)).promised;
      if da = 0;
        da = 99999999;
      endif;
      if db = 0;
        db = 99999999;
      endif;
      ra = fl_prioRank(woCache(ord(best)).prio);
      rb = fl_prioRank(woCache(ord(j)).prio);
      if db < da
         or (db = da and rb < ra)
         or (db = da and rb = ra and woCache(ord(j)).wo < woCache(ord(best)).wo);
        best = j;
      endif;
    endfor;
    if best <> i;
      tmp = ord(i);
      ord(i) = ord(best);
      ord(best) = tmp;
    endif;
  endfor;

  for p = 1 to nord;
    i = ord(p);
    cursor = schedToday;
    for j = 1 to opCount;
      if opCache(j).wo <> woCache(i).wo or opCache(j).status = 'C';
        iter;
      endif;
      // Material first: an operation cannot start before its parts land.
      // A shortage already past its expected date is NOT pushed forward -
      // there is no arrival date to push to. The scan reports it instead,
      // and the board marks the row, rather than inventing an estimate.
      for k = 1 to shCount;
        if shCache(k).wo = woCache(i).wo and shCache(k).seq = opCache(j).seq
           and shCache(k).status = 'O' and shCache(k).due > cursor;
          cursor = shCache(k).due;
        endif;
      endfor;
      wc = fl_wcIndex(opCache(j).wctr);
      cap = 8;
      if wc > 0 and wcCache(wc).cap > 0;
        cap = wcCache(wc).cap;
        // Queue: the centre is busy with the orders loaded ahead of this one.
        if wcClock(wc) > cursor;
          cursor = wcClock(wc);
        endif;
      endif;
      rem = fl_opRemaining(opCache(j).std : opCache(j).act : opCache(j).status);
      days = 0;
      if rem > 0;
        dayf = %float(rem) / %float(cap);
        days = %int(dayf);
        if dayf > days;
          days += 1;
        endif;
        if days < 1;
          days = 1;
        endif;
      endif;
      if days > 0;
        cursor = fl_addWorkDays(cursor : days);
      endif;
      if wc > 0;
        wcClock(wc) = cursor;
      endif;
    endfor;
    woRaw(i).projected = cursor;
    woCache(i).projectd = fl_fmtDate(cursor);
  endfor;

  // --- day counts and the flag
  for i = 1 to woCount;
    if woCache(i).status = 'C';
      woRaw(i).projected = woRaw(i).compl;
      woCache(i).projectd = woCache(i).compl;
    elseif woCache(i).ops = 0;
      // No routing means no projection. Refusing to invent one, and saying
      // why on the screen, is the entire point of the confidence panel.
      woRaw(i).projected = 0;
      woCache(i).projectd = *blanks;
    endif;

    woCache(i).promdays = fl_daysBetween(schedToday : woRaw(i).promised);
    woCache(i).projdays = fl_daysBetween(schedToday : woRaw(i).projected);
    woCache(i).slipdays = 0;
    if woRaw(i).promised > 0 and woRaw(i).projected > 0;
      woCache(i).slipdays =
        fl_daysBetween(woRaw(i).promised : woRaw(i).projected);
    endif;

    // Flag precedence: a fact about the data outranks a fact about the
    // schedule, because the second one is only as good as the first.
    select;
      when woCache(i).status = 'C';
        woCache(i).flag = 'SHIPPED';
      when woCache(i).status = 'H';
        woCache(i).flag = 'HOLD';
      when woCache(i).ops = 0;
        woCache(i).flag = 'NO DATA';
      when woRaw(i).promised = 0;
        woCache(i).flag = 'NO DATE';
      when woRaw(i).promised < schedToday;
        woCache(i).flag = 'LATE';
      when woCache(i).slipdays > 0;
        woCache(i).flag = 'AT RISK';
      other;
        woCache(i).flag = 'ON TRACK';
    endsl;
  endfor;

  // Orders already past their promised ship date, queued at each work centre.
  // This counted AT RISK too at first, which made every downstream centre
  // show a large number and the chip carry no signal at all.
  for i = 1 to woCount;
    if woCache(i).flag <> 'LATE';
      iter;
    endif;
    clear counted;
    for j = 1 to opCount;
      if opCache(j).wo <> woCache(i).wo or opCache(j).status = 'C';
        iter;
      endif;
      wc = fl_wcIndex(opCache(j).wctr);
      if wc > 0 and not counted(wc);
        counted(wc) = *on;
        wcCache(wc).late += 1;
      endif;
    endfor;
  endfor;

  schedLoaded = *on;
  fl_dqScan();
  return '';
end-proc;

// ---------------------------------------------------------- exported board
dcl-proc fl_listWorkCentres export;
  dcl-pi *n varchar(80);
    lanes likeds(fl_wctr_t) dim(50);
    limit int(10) const;
    returned int(10);
  end-pi;
  dcl-s i int(10);
  dcl-s err varchar(80);
  returned = 0;
  err = fl_loadSchedule();
  if err <> '';
    return err;
  endif;
  for i = 1 to wcCount;
    if returned >= limit or returned >= %elem(lanes);
      leave;
    endif;
    returned += 1;
    clear lanes(returned);
    lanes(returned).code = wcCache(i).code;
    lanes(returned).descr = wcCache(i).descr;
    lanes(returned).seq = wcCache(i).seq;
    lanes(returned).cap = wcCache(i).cap;
    lanes(returned).crew = wcCache(i).crew;
    lanes(returned).shifts = wcCache(i).shifts;
    lanes(returned).status = wcCache(i).status;
    lanes(returned).statusd = fl_wcStatusDesc(wcCache(i).status);
    lanes(returned).wip = wcCache(i).wip;
    lanes(returned).queued = wcCache(i).queued;
    lanes(returned).loadhrs = wcCache(i).loadhrs;
    lanes(returned).late = wcCache(i).late;
    // Load as a share of the next month of capacity, which is the horizon a
    // scheduler actually plans over. A stood-down centre reports zero rather
    // than infinity; the confidence scan is what explains the queue on it.
    if wcCache(i).cap > 0;
      lanes(returned).loaddays = wcCache(i).loadhrs / wcCache(i).cap;
      lanes(returned).loadpct =
        %int(wcCache(i).loadhrs * 100 / (wcCache(i).cap * 20));
      lanes(returned).weeks = wcCache(i).loadhrs / (wcCache(i).cap * 5);
    endif;
  endfor;
  return '';
end-proc;

// Does this order still have work routed at this centre?
dcl-proc fl_atCentre;
  dcl-pi *n ind;
    wo packed(8:0) const;
    wctr char(6) const;
  end-pi;
  dcl-s i int(10);
  for i = 1 to opCount;
    if opCache(i).wo = wo and opCache(i).wctr = wctr
       and opCache(i).status <> 'C';
      return *on;
    endif;
  endfor;
  return *off;
end-proc;

// Flag order for the board. A scheduler wants the trouble at the top, and
// "we cannot tell" ranks above "on track" because it is not the same thing.
dcl-proc fl_flagRank;
  dcl-pi *n int(10);
    flag char(10) const;
  end-pi;
  select;
    when flag = 'LATE';
      return 1;
    when flag = 'AT RISK';
      return 2;
    when flag = 'NO DATE';
      return 3;
    when flag = 'NO DATA';
      return 4;
    when flag = 'HOLD';
      return 5;
    when flag = 'ON TRACK';
      return 6;
    other;
      return 7;
  endsl;
end-proc;

dcl-proc fl_listWorkOrders export;
  dcl-pi *n varchar(80);
    view char(1) const;
    wctr char(6) const;
    orders likeds(fl_wo_t) dim(300);
    limit int(10) const;
    returned int(10);
  end-pi;
  dcl-s i int(10);
  dcl-s j int(10);
  dcl-s p int(10);
  dcl-s best int(10);
  dcl-s keep ind;
  dcl-s err varchar(80);
  dcl-s pick int(10) dim(300);
  dcl-s n int(10) inz(0);
  dcl-s ra int(10);
  dcl-s rb int(10);
  dcl-s da int(10);
  dcl-s db int(10);
  dcl-s tmp int(10);

  returned = 0;
  err = fl_loadSchedule();
  if err <> '';
    return err;
  endif;

  for i = 1 to woCount;
    select;
      when view = '*';
        keep = *on;
      when view = 'C';
        keep = (woCache(i).status = 'C');
      when view = 'L';
        keep = (woCache(i).flag = 'LATE');
      when view = 'R';
        keep = (woCache(i).flag = 'AT RISK');
      when view = 'S';
        keep = (woCache(i).shorts > 0);
      when view = 'H';
        keep = (woCache(i).status = 'H');
      when view = 'D';
        keep = (woCache(i).dqcount > 0);
      other;
        keep = (woCache(i).status <> 'C');
    endsl;
    if keep and wctr <> '';
      keep = fl_atCentre(woCache(i).wo : wctr);
    endif;
    if keep and n < %elem(pick);
      n += 1;
      pick(n) = i;
    endif;
  endfor;

  // Selection sort on the index array: flag rank, then promised date with
  // blanks last, then work order number. n is at most a few hundred.
  for i = 1 to n - 1;
    best = i;
    for j = i + 1 to n;
      ra = fl_flagRank(woCache(pick(best)).flag);
      rb = fl_flagRank(woCache(pick(j)).flag);
      da = woRaw(pick(best)).promised;
      db = woRaw(pick(j)).promised;
      if da = 0;
        da = 99999999;
      endif;
      if db = 0;
        db = 99999999;
      endif;
      if rb < ra
         or (rb = ra and db < da)
         or (rb = ra and db = da and woCache(pick(j)).wo
                                     < woCache(pick(best)).wo);
        best = j;
      endif;
    endfor;
    if best <> i;
      tmp = pick(i);
      pick(i) = pick(best);
      pick(best) = tmp;
    endif;
  endfor;

  for p = 1 to n;
    if returned >= limit or returned >= %elem(orders);
      leave;
    endif;
    returned += 1;
    orders(returned) = woCache(pick(p));
  endfor;
  return '';
end-proc;

dcl-proc fl_getWorkOrder export;
  dcl-pi *n varchar(80);
    wo packed(8:0) const;
    order likeds(fl_wo_t);
    found ind;
  end-pi;
  dcl-s i int(10);
  dcl-s err varchar(80);
  found = *off;
  clear order;
  err = fl_loadSchedule();
  if err <> '';
    return err;
  endif;
  i = fl_woIndex(wo);
  if i > 0;
    order = woCache(i);
    found = *on;
  endif;
  return '';
end-proc;

dcl-proc fl_listOperations export;
  dcl-pi *n varchar(80);
    wo packed(8:0) const;
    ops likeds(fl_op_t) dim(100);
    limit int(10) const;
    returned int(10);
  end-pi;
  dcl-s i int(10);
  dcl-s k int(10);
  dcl-s wc int(10);
  dcl-s err varchar(80);
  dcl-s extra int(10);
  dcl-s t varchar(120);
  returned = 0;
  err = fl_loadSchedule();
  if err <> '';
    return err;
  endif;
  for i = 1 to opCount;
    if opCache(i).wo <> wo;
      iter;
    endif;
    if returned >= limit or returned >= %elem(ops);
      leave;
    endif;
    returned += 1;
    clear ops(returned);
    ops(returned).wo = opCache(i).wo;
    ops(returned).seq = opCache(i).seq;
    ops(returned).wctr = opCache(i).wctr;
    wc = fl_wcIndex(opCache(i).wctr);
    if wc > 0;
      ops(returned).wctrd = wcCache(wc).descr;
    else;
      ops(returned).wctrd = '(not a known work centre)';
    endif;
    ops(returned).descr = opCache(i).descr;
    ops(returned).stdhrs = opCache(i).std;
    ops(returned).acthrs = opCache(i).act;
    ops(returned).status = opCache(i).status;
    ops(returned).statusd = fl_opStatusDesc(opCache(i).status);
    ops(returned).started = fl_fmtDate(opCache(i).started);
    ops(returned).compl = fl_fmtDate(opCache(i).compl);
    // Variance is computed here rather than in SQL: DECIMAL / DECIMAL comes
    // back with the scale truncated to zero, so every ratio is 0.
    if opCache(i).std > 0 and opCache(i).act > 0;
      ops(returned).varpct =
        %int((opCache(i).act - opCache(i).std) * 100 / opCache(i).std);
    endif;

    // Shortages hang off the operation they block, so the screen can say
    // which step is stalled rather than only that the order is short.
    extra = 0;
    for k = 1 to shCount;
      if shCache(k).wo <> wo or shCache(k).seq <> opCache(i).seq
         or shCache(k).status <> 'O';
        iter;
      endif;
      ops(returned).shorts += 1;
      if ops(returned).shorts = 1;
        ops(returned).shortpar = shCache(k).part;
        ops(returned).shortqty = shCache(k).reqd - shCache(k).avail;
        ops(returned).shortdue = fl_fmtDate(shCache(k).due);
        ops(returned).shortpo = shCache(k).po;
        ops(returned).shortven = shCache(k).vend;
      else;
        extra += 1;
      endif;
    endfor;
    if ops(returned).shorts > 0;
      t = %trim(%char(ops(returned).shortqty)) + ' short of '
        + %trim(ops(returned).shortpar);
      if ops(returned).shortdue = *blanks;
        t += ' - no expected date';
      elseif ops(returned).shortpo = *blanks;
        t += ' - no purchase order';
      else;
        t += ' - due ' + %trim(ops(returned).shortdue);
      endif;
      if extra > 0;
        t += ' (+' + %trim(%char(extra)) + ' more)';
      endif;
      ops(returned).shortdsc = t;
    endif;
  endfor;
  return '';
end-proc;

dcl-proc fl_shopSummary export;
  dcl-pi *n varchar(80);
    summary likeds(fl_shop_t);
  end-pi;
  dcl-s i int(10);
  dcl-s err varchar(80);
  clear summary;
  err = fl_loadSchedule();
  if err <> '';
    return err;
  endif;
  for i = 1 to wcCount;
    if wcCache(i).status = 'A';
      summary.capday += wcCache(i).cap;
    endif;
  endfor;
  for i = 1 to woCount;
    if woCache(i).status = 'C';
      iter;
    endif;
    summary.orders += 1;
    summary.value += woCache(i).value;
    summary.remhrs += woCache(i).remhrs;
    if woCache(i).status = 'I';
      summary.wip += 1;
    endif;
    if woCache(i).shorts > 0;
      summary.shorts += 1;
    endif;
    if woCache(i).dqcount > 0;
      summary.dqorders += 1;
    endif;
    if woCache(i).dqhigh = 0;
      summary.dqtrust += 1;
    endif;
    select;
      when woCache(i).flag = 'LATE';
        summary.late += 1;
      when woCache(i).flag = 'AT RISK';
        summary.risk += 1;
      when woCache(i).flag = 'HOLD';
        summary.hold += 1;
      when woCache(i).flag = 'ON TRACK';
        summary.ontrack += 1;
    endsl;
  endfor;
  for i = 1 to shCount;
    if shCache(i).status = 'O' and shCache(i).reqd > shCache(i).avail;
      summary.shortval +=
        (shCache(i).reqd - shCache(i).avail) * shCache(i).price;
    endif;
  endfor;
  summary.dqfind = dqCount;
  for i = 1 to dqCount;
    if dqCache(i).sev = 'H';
      summary.dqhigh += 1;
    endif;
  endfor;
  if summary.capday > 0;
    summary.weeks = summary.remhrs / (summary.capday * 5);
  endif;
  if summary.orders > 0;
    summary.dqpct = %int(summary.dqtrust * 100 / summary.orders);
  endif;
  return '';
end-proc;

dcl-proc fl_scanSchedule export;
  dcl-pi *n varchar(80);
    findings likeds(fl_dq_t) dim(400);
    limit int(10) const;
    returned int(10);
  end-pi;
  dcl-s i int(10);
  dcl-s err varchar(80);
  returned = 0;
  err = fl_loadSchedule();
  if err <> '';
    return err;
  endif;
  for i = 1 to dqCount;
    if returned >= limit or returned >= %elem(findings);
      leave;
    endif;
    returned += 1;
    findings(returned) = dqCache(i);
  endfor;
  return '';
end-proc;

// The board opens on a real order rather than an empty prompt: the worst
// slip that also has material missing, because that is the order somebody
// would actually be chasing. Chosen by data, never hardcoded.
dcl-proc fl_defaultWorkOrder export;
  dcl-pi *n varchar(80);
    wo packed(8:0);
  end-pi;
  dcl-s i int(10);
  dcl-s best int(10) inz(0);
  dcl-s bestScore int(10) inz(-99999);
  dcl-s score int(10);
  dcl-s err varchar(80);
  wo = 0;
  err = fl_loadSchedule();
  if err <> '';
    return err;
  endif;
  for i = 1 to woCount;
    if woCache(i).status = 'C' or woCache(i).ops = 0;
      iter;
    endif;
    score = woCache(i).slipdays + woCache(i).shorts * 12
          + woCache(i).dqcount * 5;
    if score > bestScore;
      bestScore = score;
      best = i;
    endif;
  endfor;
  if best = 0 and woCount > 0;
    best = 1;
  endif;
  if best > 0;
    wo = woCache(best).wo;
  endif;
  return '';
end-proc;

dcl-proc fl_jsonStr export;
  dcl-pi *n varchar(300);
    v varchar(250) const;
  end-pi;
  // NB: not named "out" - IN and OUT are free-form RPG opcodes, so a
  // statement starting with `out =` is parsed as a data-area operation and
  // fails with RNF7064 "not a data area", which names nothing recognisable.
  dcl-s buf varchar(300);
  dcl-s c char(1);
  dcl-s i int(10);
  buf = '"';
  for i = 1 to %len(v);
    // Stop well short of the declared length: appending past a VARCHAR's
    // size is RNX0100 at runtime, and an escape can cost two characters.
    if %len(buf) > 292;
      leave;
    endif;
    c = %subst(v : i : 1);
    select;
      when c = '"';
        buf += '\"';
      when c = '\';
        buf += '\\';
      when c < ' ';
        buf += ' ';
      other;
        buf += c;
    endsl;
  endfor;
  return buf + '"';
end-proc;

dcl-proc fl_jsonNum export;
  dcl-pi *n varchar(24);
    numText varchar(24) const;
  end-pi;
  dcl-s t varchar(24);
  t = %trim(numText);
  if t = '';
    return '0';
  endif;
  if %subst(t : 1 : 1) = '.';
    return '0' + t;
  endif;
  if %len(t) > 1 and %subst(t : 1 : 2) = '-.';
    return '-0' + %subst(t : 2);
  endif;
  return t;
end-proc;
