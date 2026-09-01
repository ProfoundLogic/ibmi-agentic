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
end-ds;
dcl-s sigCount int(10) inz(0);
dcl-s sigLoaded ind inz(*off);

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

    // a token starting with a digit is a size; strip any trailing unit
    isSize = %check('0123456789' : tok) <> 1 and %len(tok) > 0;
    if isSize;
      n = %check('0123456789' : tok);
      if n > 0;
        tok = %subst(tok : 1 : n - 1);
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
  dcl-s pn char(15);
  dcl-s pd char(40);
  if sigLoaded;
    return;
  endif;
  exec sql declare cSig cursor for
    select flpart, flpdesc from flpartp order by flpart;
  exec sql open cSig;
  dow sigCount < %elem(sigCache);
    exec sql fetch cSig into :pn, :pd;
    if sqlcode <> 0;
      leave;
    endif;
    sigCount += 1;
    sigCache(sigCount).part = pn;
    fl_norm(pd : sigCache(sigCount).words : sigCache(sigCount).sizes);
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
