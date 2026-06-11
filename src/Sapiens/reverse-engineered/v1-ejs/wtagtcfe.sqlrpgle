**free

// =================================================================
// WTAGTCFE - Agency Configuration (EJS rendition).
//
// Companion to WTAGTCFG. Reads the same data (WMAGP via WMAGL1)
// but renders through a Profound UI EJS Rich Display File so we can
// apply modern Sapiens branding (navy/orange/cream, DM Sans/Inter).
// No /COPY procedure calls, so no SAPSTUBS dependency.
// =================================================================

ctl-opt dftname(WTAGTCFE) option(*nodebugio:*srcstmt)
        dftactgrp(*no) actgrp(*new) alwnull(*usrctl);

dcl-f wtagtcfe workstn sfile(agtsfl:rrn)
               handler('PROFOUNDUI(HANDLER)');
dcl-f wmagl1   disk(*ext) keyed usage(*input) rename(pag01:pag01);

dcl-c maxRows 500;

dcl-s rrn      int(5);
dcl-s i        int(5);
dcl-s selSrrn  int(10);
dcl-s numAgts  int(5);
dcl-s agentCnt packed(5 : 0);

dcl-ds dsAgency qualified dim(maxRows);
  rrnPF   packed(9 : 0);
  name    char(40);
  shnm    char(40);
  fein    packed(9 : 0);
  mod     packed(3 : 0);
  agent   packed(5 : 0);
  contact char(40);
  phone   char(24);
  city    char(25);
  state   char(2);
  status  char(1);
  mailAdd char(40);
  email   char(60);
  county  char(20);
end-ds;

// Prototypes for the helpers defined below.
dcl-pr formatPhone char(24);
  area  packed(3 : 0) const;
  line7 packed(7 : 0) const;
end-pr;

dcl-pr matchesFilter ind;
  pname   char(40)   const;
  pcont   char(40)   const;
  pcity   char(25)   const;
  pfein   packed(9 : 0) const;
  pfilt   varchar(50) const;
end-pr;

dcl-pr loadAgencies end-pr;
dcl-pr loadSubfile  end-pr;
dcl-pr loadDetail   end-pr;
dcl-pr clearDetail  end-pr;

// ----- main program flow -----------------------------------------------

loadAgencies();

// Default selection: first row if any.
if selrrn = 0 and numAgts > 0;
  selrrn = 1;
endif;

dow action <> 'EXIT';

  loadSubfile();
  numagts = numAgts;
  msg = '';
  if numAgts = 0;
    msg = 'No agencies found. Try clearing the search filter.';
  endif;

  if selrrn >= 1 and selrrn <= numAgts;
    loadDetail();
  else;
    clearDetail();
  endif;

  action = '';
  exfmt agtctl;

  select;
    when action = 'EXIT';
      leave;
    when action = 'CLEAR_FILTER';
      sfilter = ' ';
      selrrn  = 0;
      loadAgencies();
      if numAgts > 0;
        selrrn = 1;
      endif;
      iter;
    when action = 'SEARCH';
      selrrn = 0;
      loadAgencies();
      if numAgts > 0;
        selrrn = 1;
      endif;
      iter;
    when action = 'SELECT';
      // selrrn was set by the client; nothing else to do
      iter;
    when action = 'NEW';
      msg = '"New Agency" is stubbed in the EJS demo. Use option 4 for the real add flow.';
      iter;
    when action = 'SAVE';
      msg = '"Save" is stubbed in the EJS demo. No persistence.';
      iter;
  endsl;

  // Fallback selection: pick the first subfile row with a non-blank opt.
  selSrrn = 0;
  sopt = ' ';
  readc agtsfl;
  dow not %eof(wtagtcfe);
    if sopt <> ' ' and selSrrn = 0;
      selSrrn = rrn;
    endif;
    readc agtsfl;
  enddo;
  if selSrrn > 0;
    chain selSrrn agtsfl;
    selrrn = srrn;
  endif;

enddo;

*inlr = *on;
return;

// ----- helpers --------------------------------------------------------

dcl-proc formatPhone;
  dcl-pi *n char(24);
    area  packed(3 : 0) const;
    line7 packed(7 : 0) const;
  end-pi;
  dcl-s area3   char(3);
  dcl-s pref    char(3);
  dcl-s last4   char(4);
  if line7 = 0 and area = 0;
    return ' ';
  endif;
  area3 = %editc(area  : 'X');
  pref  = %subst(%editc(line7 : 'X') : 1 : 3);
  last4 = %subst(%editc(line7 : 'X') : 4 : 4);
  return '(' + area3 + ') ' + pref + '-' + last4;
end-proc;

dcl-proc matchesFilter;
  dcl-pi *n ind;
    pname   char(40)   const;
    pcont   char(40)   const;
    pcity   char(25)   const;
    pfein   packed(9 : 0) const;
    pfilt   varchar(50) const;
  end-pi;
  dcl-s hay    varchar(200);
  dcl-s needle varchar(50);
  if pfilt = ' ';
    return *on;
  endif;
  needle = %lower(%trim(pfilt));
  hay    = %lower(%trim(pname)) + ' ' +
           %lower(%trim(pcont)) + ' ' +
           %lower(%trim(pcity)) + ' ' +
           %editc(pfein : 'X');
  return %scan(needle : hay) > 0;
end-proc;

dcl-proc loadAgencies;
  numAgts = 0;
  clear dsAgency;
  setll *loval wmagl1;
  read wmagl1;
  dow not %eof(wmagl1) and numAgts < maxRows;
    if matchesFilter(AGname : AGcont : AGmcty : AGfein : sfilter);
      numAgts += 1;
      dsAgency(numAgts).rrnPF   = numAgts;
      dsAgency(numAgts).name    = AGname;
      dsAgency(numAgts).shnm    = AGshnm;
      dsAgency(numAgts).fein    = AGfein;
      dsAgency(numAgts).mod     = AGmod;
      // Agent number isn't on WMAGP — each agency has many agents
      // in WMAAP, keyed by (AACO#, AAFEIN, AAMOD). Count them.
      // SQL precompiler can't bind directly to a subscripted DS field,
      // so count into a scalar then copy.
      exec sql
        select count(*) into :agentCnt
        from   wmaap
        where  AACO#  = :AGCO#
          and  AAFEIN = :AGFEIN
          and  AAMOD  = :AGMOD;
      dsAgency(numAgts).agent = agentCnt;
      dsAgency(numAgts).contact = AGcont;
      dsAgency(numAgts).phone   = formatPhone(AGARE1 : AGPHN1);
      dsAgency(numAgts).city    = AGmcty;
      dsAgency(numAgts).state   = AGmst;
      dsAgency(numAgts).status  = AGstat;
      dsAgency(numAgts).mailAdd = AGmad1;
      dsAgency(numAgts).email   = AGemal;
      dsAgency(numAgts).county  = AGcnty;
    endif;
    read wmagl1;
  enddo;
end-proc;

dcl-proc loadSubfile;
  rrn = 0;
  sflclear = '1';
  write agtctl;
  sflclear = '0';
  for i = 1 to numAgts;
    sopt     = ' ';
    srrn     = dsAgency(i).rrnPF;
    sname    = dsAgency(i).name;
    sfein    = dsAgency(i).fein;
    smod     = dsAgency(i).mod;
    sagent   = dsAgency(i).agent;
    scontact = dsAgency(i).contact;
    sphone   = dsAgency(i).phone;
    scity    = dsAgency(i).city;
    sstate   = dsAgency(i).state;
    sstatus  = dsAgency(i).status;
    rrn += 1;
    write agtsfl;
  endfor;
end-proc;

dcl-proc loadDetail;
  i = selrrn;
  if i < 1 or i > numAgts;
    clearDetail();
    return;
  endif;
  dname    = dsAgency(i).name;
  dshnm    = dsAgency(i).shnm;
  dfein    = dsAgency(i).fein;
  dmod     = dsAgency(i).mod;
  dagent   = dsAgency(i).agent;
  dcontact = dsAgency(i).contact;
  dphone   = dsAgency(i).phone;
  demail   = dsAgency(i).email;
  daddr    = %trim(dsAgency(i).mailAdd) + ', ' +
             %trim(dsAgency(i).city) + ' ' +
             dsAgency(i).state;
  dcounty  = dsAgency(i).county;
  dstatus  = dsAgency(i).status;
end-proc;

dcl-proc clearDetail;
  dname    = ' ';
  dshnm    = ' ';
  dfein    = 0;
  dmod     = 0;
  dagent   = 0;
  dcontact = ' ';
  dphone   = ' ';
  demail   = ' ';
  daddr    = ' ';
  dcounty  = ' ';
  dstatus  = ' ';
end-proc;
