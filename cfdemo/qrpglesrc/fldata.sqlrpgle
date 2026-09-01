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
