**free

// =================================================================
// WTAGTCFE - Agency Configuration (EJS rendition, v2)
//
// Reads the real Sapiens WMAGP via WMAGL1, joins WMAAP for agent
// counts, and serves a Rich Display File (EJS template) with a
// Sapiens-branded UX (sapiens.com design system).
//
// Independent of WTAGTCFG: no /COPY procedure calls, no binding-
// directory dependency, no shared display file.  Pure RPG + SQL.
// =================================================================

ctl-opt dftname(WTAGTCFE) option(*nodebugio:*srcstmt)
        dftactgrp(*no) actgrp(*new) alwnull(*usrctl);

dcl-f wtagtcfe workstn sfile(agtsfl:rrn)
               handler('PROFOUNDUI(HANDLER)');
dcl-f wmagl1   disk(*ext) keyed usage(*input) rename(pag01:pag01);

dcl-c maxRows 500;

dcl-s rrn       int(5);
dcl-s i         int(5);
dcl-s selSrrn   int(10);
dcl-s numAgts   int(5);
dcl-s numActive int(5);
dcl-s numMaster int(5);
dcl-s numProd   int(10);
dcl-s agentCnt  packed(5 : 0);
dcl-s scratch   varchar(120);
dcl-s pendingMsg varchar(100);

// One slot per agency we've loaded into the subfile this cycle.
dcl-ds dsAgency qualified dim(maxRows);
  rrnPF    packed(9 : 0);
  // grid-level (subfile columns)
  name     char(40);
  shnm     char(20);
  fein     char(11);                // formatted XX-XXXXXXX
  mod      packed(3 : 0);
  agent    packed(5 : 0);
  contact  char(40);
  phone    char(24);
  city     char(25);
  state    char(2);
  status   char(1);
  master   char(1);
  spa      char(1);
  bkup     char(1);
  // detail-only (only filled for the selected row)
  fdnm     char(40);
  type     char(3);
  group    char(3);
  external packed(9 : 0);
  secy     char(40);
  hcon     char(40);
  email    char(60);
  phone2   char(24);
  fax      char(24);
  mailAdd  char(120);
  physAdd  char(120);
  intAdd   char(80);
  county   char(30);
  w9sent   char(10);
  w9recv   char(10);
  bgchk    char(10);
  f1099    char(1);
  fexp     char(1);
  mktrep   char(3);
  mktstf   char(3);
  mktrg    char(10);
  svcr     char(3);
  eftp     char(250);
  eftpd    char(250);
  lastdt   char(10);
  lastusr  char(10);
  initdt   char(10);
  initusr  char(10);
end-ds;

// Prototypes
dcl-pr formatPhone char(24);
  area  packed(3 : 0) const;
  line7 packed(7 : 0) const;
end-pr;

dcl-pr formatFEIN char(11);
  fein  packed(9 : 0) const;
end-pr;

dcl-pr formatDate char(10);
  cymd  packed(7 : 0) const;
end-pr;

dcl-pr matchesFilter ind;
  pname   char(40)   const;
  pcont   char(40)   const;
  pcity   char(25)   const;
  pfein   packed(9 : 0) const;
  pfilt   varchar(50) const;
  pstatus char(1)    const;
  psfilt  char(1)    const;
end-pr;

dcl-pr loadAgencies end-pr;
dcl-pr loadAggregates end-pr;
dcl-pr loadSubfile  end-pr;
dcl-pr loadDetail   end-pr;
dcl-pr clearDetail  end-pr;
dcl-pr saveEdit     end-pr;
dcl-pr createAgency end-pr;

// ----- main program flow ----------------------------------------------

loadAgencies();
loadAggregates();

// Default selection: first row if any.
if selrrn = 0 and numAgts > 0;
  selrrn = 1;
endif;

dow action <> 'EXIT';

  loadSubfile();
  numagts = numAgts;
  numactive = numActive;
  nummaster = numMaster;
  numprods = numProd;
  // Carry any message produced by the previous action (CREATE / SAVE /
  // validation failure) into this cycle's display, then clear it.
  if pendingMsg <> '';
    msg = pendingMsg;
    pendingMsg = '';
  else;
    msg = '';
    if numAgts = 0;
      msg = 'No agencies match. Try clearing the search filter or status.';
    endif;
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
      sfilter     = ' ';
      sstatfltr = ' ';
      selrrn      = 0;
      loadAgencies();
      loadAggregates();
      if numAgts > 0;
        selrrn = 1;
      endif;
      iter;
    when action = 'SEARCH';
      selrrn = 0;
      loadAgencies();
      loadAggregates();
      if numAgts > 0;
        selrrn = 1;
      endif;
      iter;
    when action = 'SELECT';
      iter;
    when action = 'NEW';
      // No-op on server; the client opens the new-agency modal locally.
      iter;
    when action = 'CREATE';
      createAgency();
      loadAgencies();
      loadAggregates();
      iter;
    when action = 'EDIT';
      // No-op on server; the client opens the edit modal locally.
      iter;
    when action = 'SAVE';
      saveEdit();
      loadAgencies();
      loadAggregates();
      iter;
  endsl;

  // Fallback: scan subfile for any flagged row
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

dcl-proc formatFEIN;
  dcl-pi *n char(11);
    fein  packed(9 : 0) const;
  end-pi;
  dcl-s s9 char(9);
  if fein = 0;
    return ' ';
  endif;
  s9 = %editc(fein : 'X');
  return %subst(s9 : 1 : 2) + '-' + %subst(s9 : 3 : 7);
end-proc;

dcl-proc formatDate;
  // Input: CYYMMDD (7 packed, century 0=19xx, 1=20xx).
  // Output: 'MM/DD/YYYY' or blank if zero/invalid.
  dcl-pi *n char(10);
    cymd  packed(7 : 0) const;
  end-pi;
  dcl-s s7   char(7);
  dcl-s cc   int(5);
  dcl-s yyyy int(5);
  dcl-s mm   char(2);
  dcl-s dd   char(2);
  if cymd = 0;
    return ' ';
  endif;
  s7 = %editc(cymd : 'X');
  cc = %int(%subst(s7 : 1 : 1));
  if cc = 0;
    yyyy = 1900 + %int(%subst(s7 : 2 : 2));
  else;
    yyyy = 2000 + %int(%subst(s7 : 2 : 2));
  endif;
  mm = %subst(s7 : 4 : 2);
  dd = %subst(s7 : 6 : 2);
  if mm = '00' or dd = '00';
    return ' ';
  endif;
  return mm + '/' + dd + '/' + %char(yyyy);
end-proc;

dcl-proc matchesFilter;
  dcl-pi *n ind;
    pname   char(40)   const;
    pcont   char(40)   const;
    pcity   char(25)   const;
    pfein   packed(9 : 0) const;
    pfilt   varchar(50) const;
    pstatus char(1)    const;
    psfilt  char(1)    const;
  end-pi;
  dcl-s hay    varchar(200);
  dcl-s needle varchar(50);

  // Status filter
  if psfilt <> ' ' and psfilt <> pstatus;
    return *off;
  endif;

  // Text filter
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
    if matchesFilter(AGname : AGcont : AGmcty : AGfein : sfilter :
                     AGstat : sstatfltr);
      numAgts += 1;

      // Count active producers from WMAAP for this agency.
      exec sql
        select count(*) into :agentCnt
        from   wmaap
        where  AACO#  = :AGCO#
          and  AAFEIN = :AGFEIN
          and  AAMOD  = :AGMOD;

      dsAgency(numAgts).rrnPF    = numAgts;
      // grid-level
      dsAgency(numAgts).name     = AGname;
      dsAgency(numAgts).shnm     = AGshnm;
      dsAgency(numAgts).fein     = formatFEIN(AGfein);
      dsAgency(numAgts).mod      = AGmod;
      dsAgency(numAgts).agent    = agentCnt;
      dsAgency(numAgts).contact  = AGcont;
      dsAgency(numAgts).phone    = formatPhone(AGARE1 : AGPHN1);
      dsAgency(numAgts).city     = AGmcty;
      dsAgency(numAgts).state    = AGmst;
      dsAgency(numAgts).status   = AGstat;
      dsAgency(numAgts).master   = AGmstr;
      dsAgency(numAgts).spa      = AGspa;
      dsAgency(numAgts).bkup     = AGbkup;

      // detail-only fields
      dsAgency(numAgts).fdnm     = AGfdnm;
      dsAgency(numAgts).type     = AGtpc2;
      dsAgency(numAgts).group    = AGgrcd;
      dsAgency(numAgts).external = AGnchb;
      dsAgency(numAgts).secy     = AGsecy;
      dsAgency(numAgts).hcon     = AGhcon;
      dsAgency(numAgts).email    = AGemal;
      dsAgency(numAgts).phone2   = formatPhone(AGARE2 : AGPHN2);
      dsAgency(numAgts).fax      = formatPhone(AGfaxa : AGfax#);
      dsAgency(numAgts).mailAdd  = %trim(AGmad1) + ', ' +
                                   %trim(AGmcty) + ' ' + AGmst;
      dsAgency(numAgts).physAdd  = %trim(AGpad1) + ', ' +
                                   %trim(AGpcty) + ' ' + AGpst;
      dsAgency(numAgts).intAdd   = AGiadr;
      dsAgency(numAgts).county   = AGcnty;
      dsAgency(numAgts).w9sent   = formatDate(AGw9se);
      dsAgency(numAgts).w9recv   = formatDate(AGw9rc);
      dsAgency(numAgts).bgchk    = formatDate(AGbkch);
      dsAgency(numAgts).f1099    = AG1099;
      dsAgency(numAgts).fexp     = AGeexp;
      dsAgency(numAgts).mktrep   = AGmkrp;
      dsAgency(numAgts).mktstf   = AGmktr;
      dsAgency(numAgts).mktrg    = AGmkrg;
      dsAgency(numAgts).svcr     = AGsvcr;
      dsAgency(numAgts).eftp     = AGeftp;
      dsAgency(numAgts).eftpd    = AGeftpd;
      dsAgency(numAgts).lastdt   = formatDate(AGdt);
      dsAgency(numAgts).lastusr  = AGuser;
      dsAgency(numAgts).initdt   = formatDate(AGidt);
      dsAgency(numAgts).initusr  = AGiusr;
    endif;
    read wmagl1;
  enddo;
end-proc;

dcl-proc loadAggregates;
  // Total = numAgts (already set by loadAgencies after filter)
  // Active = count of filtered agencies with status A
  numActive = 0;
  numMaster = 0;
  for i = 1 to numAgts;
    if dsAgency(i).status = 'A';
      numActive += 1;
    endif;
    if dsAgency(i).master = 'Y';
      numMaster += 1;
    endif;
  endfor;

  // Total producers across ALL agencies (not just filtered)
  exec sql
    select count(*) into :numProd
    from   wmaap;
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
    sshnm    = dsAgency(i).shnm;
    sfein    = dsAgency(i).fein;
    smod     = dsAgency(i).mod;
    sagent   = dsAgency(i).agent;
    scontact = dsAgency(i).contact;
    sphone   = dsAgency(i).phone;
    scity    = dsAgency(i).city;
    sstate   = dsAgency(i).state;
    sstatus  = dsAgency(i).status;
    smaster  = dsAgency(i).master;
    sspa     = dsAgency(i).spa;
    sbkup    = dsAgency(i).bkup;
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
  dname     = dsAgency(i).name;
  dshnm     = dsAgency(i).shnm;
  dfdnm     = dsAgency(i).fdnm;
  dfein     = dsAgency(i).fein;
  dmod      = dsAgency(i).mod;
  dagent    = dsAgency(i).agent;
  dtype     = dsAgency(i).type;
  dgroup    = dsAgency(i).group;
  dexternal = dsAgency(i).external;
  dcontact  = dsAgency(i).contact;
  dsecy     = dsAgency(i).secy;
  dhcon     = dsAgency(i).hcon;
  demail    = dsAgency(i).email;
  dphone    = dsAgency(i).phone;
  dphone2   = dsAgency(i).phone2;
  dfax      = dsAgency(i).fax;
  daddr     = dsAgency(i).mailAdd;
  dpaddr    = dsAgency(i).physAdd;
  diaddr    = dsAgency(i).intAdd;
  dcounty   = dsAgency(i).county;
  dcity     = dsAgency(i).city;
  dstate    = dsAgency(i).state;
  dw9sent   = dsAgency(i).w9sent;
  dw9recv   = dsAgency(i).w9recv;
  dbgchk    = dsAgency(i).bgchk;
  d1099f    = dsAgency(i).f1099;
  dbkupf    = dsAgency(i).bkup;
  dexpf     = dsAgency(i).fexp;
  dmasterf  = dsAgency(i).master;
  dspaf     = dsAgency(i).spa;
  dmktrep   = dsAgency(i).mktrep;
  dmktstf   = dsAgency(i).mktstf;
  dmktrg    = dsAgency(i).mktrg;
  dsvcr     = dsAgency(i).svcr;
  deftp     = dsAgency(i).eftp;
  deftpd    = dsAgency(i).eftpd;
  dlastdt   = dsAgency(i).lastdt;
  dlastusr  = dsAgency(i).lastusr;
  dinitdt   = dsAgency(i).initdt;
  dinitusr  = dsAgency(i).initusr;
  dstatus   = dsAgency(i).status;
end-proc;

dcl-proc clearDetail;
  dname     = ' ';
  dshnm     = ' ';
  dfdnm     = ' ';
  dfein     = ' ';
  dmod      = 0;
  dagent    = 0;
  dtype     = ' ';
  dgroup    = ' ';
  dexternal = 0;
  dcontact  = ' ';
  dsecy     = ' ';
  dhcon     = ' ';
  demail    = ' ';
  dphone    = ' ';
  dphone2   = ' ';
  dfax      = ' ';
  daddr     = ' ';
  dpaddr    = ' ';
  diaddr    = ' ';
  dcounty   = ' ';
  dcity     = ' ';
  dstate    = ' ';
  dw9sent   = ' ';
  dw9recv   = ' ';
  dbgchk    = ' ';
  d1099f    = ' ';
  dbkupf    = ' ';
  dexpf     = ' ';
  dmasterf  = ' ';
  dspaf     = ' ';
  dmktrep   = ' ';
  dmktstf   = ' ';
  dmktrg    = ' ';
  dsvcr     = ' ';
  deftp     = ' ';
  deftpd    = ' ';
  dlastdt   = ' ';
  dlastusr  = ' ';
  dinitdt   = ' ';
  dinitusr  = ' ';
  dstatus   = ' ';
end-proc;

dcl-proc saveEdit;
  // Apply the edits from the modal to WMAGP. Uses the agency identified
  // by selrrn (its keys cached in dsAgency).
  dcl-s sCo#    packed(3 : 0);
  dcl-s sFEIN   packed(9 : 0);
  dcl-s sMod    packed(3 : 0);
  dcl-s rows    int(10);
  dcl-s sDigits varchar(11);
  dcl-s sCh     char(1);
  dcl-s sk      int(5);

  if selrrn < 1 or selrrn > numAgts;
    pendingMsg = 'Select an agency first.';
    return;
  endif;

  sCo#  = dsAgency(selrrn).rrnPF;
  i = selrrn;

  // dsAgency(i).fein is FORMATTED (XX-XXXXXXX) — extract the 9 digits.
  scratch = dsAgency(i).fein;
  sDigits = '';
  for sk = 1 to %len(%trim(scratch));
    sCh = %subst(%trim(scratch) : sk : 1);
    if sCh >= '0' and sCh <= '9';
      sDigits = sDigits + sCh;
    endif;
  endfor;
  if %len(sDigits) = 9;
    sFEIN = %dec(sDigits : 9 : 0);
  else;
    sFEIN = 0;
  endif;
  sMod  = dsAgency(i).mod;

  if sFEIN = 0;
    pendingMsg = 'Cannot save: FEIN is missing on the selected agency.';
    return;
  endif;

  // Refuse the save if the agency name didn't make it through. This is the
  // signature of PUI's form binding failing to pick up DOM .value mutations
  // on inputs that originally had server-rendered value="" attributes.
  // Better to ask for a refresh than to blank the row.
  if %trim(ename) = '';
    pendingMsg = 'Save refused: agency name was blank on submit. ' +
                 'Refresh the page (Ctrl+Shift+R) and try again.';
    return;
  endif;
  if %trim(estatus) = '';
    estatus = 'A';
  endif;

  exec sql
    update wmagp
       set AGNAME  = :ename,
           AGCONT  = :econtact,
           AGEMAL  = :eemail,
           AGMCTY  = :ecity,
           AGMST   = :estate,
           AGSTAT  = :estatus,
           AGUSER  = 'EJSDEMO',
           AGPGM   = 'WTAGTCFE'
     where AGFEIN = :sFEIN
       and AGMOD  = :sMod;

  rows = sqler3;
  if rows > 0;
    pendingMsg = 'Saved ' + %char(rows) + ' row(s) — agency updated.';
    dsAgency(i).name    = ename;
    dsAgency(i).contact = econtact;
    dsAgency(i).email   = eemail;
    dsAgency(i).city    = ecity;
    dsAgency(i).state   = estate;
    dsAgency(i).status  = estatus;
  else;
    pendingMsg = 'No row updated — FEIN/Mod did not match (SQLSTATE='
                 + sqlstt + ').';
  endif;
end-proc;

dcl-proc createAgency;
  // Insert a new row into WMAGP using values from the New-Agency modal.
  dcl-s nFEIN packed(9 : 0);
  dcl-s nMod  packed(3 : 0);
  dcl-s nCo#  packed(3 : 0);
  dcl-s digits varchar(11);
  dcl-s src    varchar(11);
  dcl-s ch     char(1);
  dcl-s k      int(5);
  dcl-s exists int(5);
  dcl-s rows   int(10);
  dcl-s sqlState char(5);

  // Basic validation
  if %trim(efein) = '' or %trim(ename) = '';
    pendingMsg = 'Federal EIN and Agency name are required.';
    return;
  endif;

  // FEIN: extract digits only → 9 packed digits.
  src = %trim(efein);
  digits = '';
  for k = 1 to %len(src);
    ch = %subst(src : k : 1);
    if ch >= '0' and ch <= '9';
      digits = digits + ch;
    endif;
  endfor;
  if %len(digits) <> 9;
    pendingMsg = 'Federal EIN must be 9 digits (XX-XXXXXXX). Got ' +
                 %char(%len(digits)) + '.';
    return;
  endif;
  nFEIN = %dec(digits : 9 : 0);
  nMod  = emod;

  // Pick AGCO# from any existing agency for this client.
  // The data uses AGCO#=777; fall back to 79 only if the table is empty.
  setll *loval wmagl1;
  read wmagl1;
  if not %eof(wmagl1);
    nCo# = AGCO#;
  else;
    nCo# = 79;
  endif;

  // Duplicate-key check
  exec sql
    select count(*) into :exists
    from   wmagp
    where  AGCO#  = :nCo#
      and  AGFEIN = :nFEIN
      and  AGMOD  = :nMod;
  if exists > 0;
    pendingMsg = 'An agency with that FEIN/Mod already exists ' +
                 '(Co# ' + %char(nCo#) + ').';
    return;
  endif;

  exec sql
    insert into wmagp
      (AGCO#, AGFEIN, AGMOD,
       AGNAME, AGCONT, AGEMAL,
       AGMCTY, AGMST,
       AGSTAT, AGUSER, AGPGM)
    values
      (:nCo#, :nFEIN, :nMod,
       :ename, :econtact, :eemail,
       :ecity, :estate,
       :estatus, 'EJSDEMO', 'WTAGTCFE');

  sqlState = sqlstt;
  rows = sqler3;
  if sqlState = '00000' and rows = 1;
    pendingMsg = 'Created ' + %trim(ename) +
                 ' (FEIN ' + %trim(efein) +
                 ', Mod ' + %char(nMod) +
                 ', Co# ' + %char(nCo#) + ').';
  else;
    pendingMsg = 'Insert failed (SQLSTATE=' + sqlState +
                 ', rows=' + %char(rows) + ').';
  endif;
end-proc;
