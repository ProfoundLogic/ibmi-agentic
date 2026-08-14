**free

//////////////////////////////////////////////////////////////////////////
// GTCNHR -- Cycle Count, home screen.
//
// The counts waiting to be done, and a scan box to start one by standing in
// front of the rack instead of picking it off a list. Both routes end in the
// same place: GTCNER, the blind count.
//
// A count is shown whatever its status short of posted, so a counter can go
// back into one they left half-finished -- which is what actually happens on a
// floor when a forklift needs the aisle.
//////////////////////////////////////////////////////////////////////////

ctl-opt dftactgrp(*no) actgrp(*new);
ctl-opt bnddir('GTBAR');

dcl-f gtcnhd workstn sfile(cntsfl : rrn) handler('PROFOUNDUI(HANDLER)');

exec sql set option commit = *chg, closqlcsr = *endmod;

/copy gtbar_pr.rpgle

dcl-pr gtcner extpgm;
  countId char(10) const;
  lang    char(2);
end-pr;

dcl-ds r likeds(gtbar_result_t);

dcl-c MAX_ROWS 40;

dcl-s rrn     int(10) inz(0);
dcl-s i       int(10);
dcl-s done    ind inz(*off);
dcl-s payload varchar(120);
dcl-s curCnt  char(10);

dcl-ds counts qualified dim(MAX_ROWS);
  cnt char(10);
end-ds;
dcl-s cntCount int(10) inz(0);

//  Work fields -- RNF0724 forbids dcl-s inside a begsr here.
dcl-s wCnt    char(10);
dcl-s wLoc    char(10);
dcl-s wZone   char(4);
dcl-s wType   char(4);
dcl-s wStat   char(4);
dcl-s wAsgn   char(8);
dcl-s wAName  varchar(40);
dcl-s wLines  int(10);
dcl-s wDone   int(10);
dcl-s wVar    int(10);
dcl-s wPho    int(10);
dcl-s wKey    char(10);

//------------------------------------------------------------------------
exsr loadOperator;

dow not done;
  exsr loadCountSubfile;
  action  = *blanks;
  scanval = *blanks;
  exfmt cnthome;
  msg     = *blanks;
  msgkind = *blanks;

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

    when %subst(action : 1 : 4) = 'PICK';
      i = %int(%subst(action : 5));
      if i >= 1 and i <= cntCount;
        gtcner(counts(i).cnt : langpref);
      endif;

    when action = *blanks;
      //  Enter with nothing pressed. Redisplay.

    other;
  endsl;
enddo;

*inlr = *on;
return;

//------------------------------------------------------------------------
// A scan means "start the count for this location". A location with no open
// count says so rather than silently doing nothing -- generating counts is a
// supervisor's job, not something a scan should invent.
//------------------------------------------------------------------------
begsr doScan;
  payload = %trim(scanval);

  if payload = *blanks;
    exsr msgNothingScanned;
    leavesr;
  endif;

  clear r;
  gtbar_parse(payload : r);

  wLoc = r.locationId;
  if wLoc = *blanks;
    //  A hand-keyed or unrecognised label carries no symbology, so take it as
    //  typed.
    //
    //  %trim, NOT %subst(payload : 1 : 10). A fixed-length substring demands the
    //  payload be at least that long, and EVERY location id in this warehouse is
    //  six characters -- so this line was RNX0100 for any payload it was actually
    //  reached with. It survived because it is only reached when gtbar_parse did
    //  NOT resolve the scan, and every test typed a location that resolved.
    //  Enabling the camera on this screen is what finally reached it: the first
    //  unresolved short payload took the program down and the operator got an
    //  empty screen.
    //
    //  Assigning a longer payload to char(10) truncates on the right, which is
    //  exactly what the four scan handlers that already used %trim rely on.
    wKey = %upper(%trim(payload));
    exec sql
      select location_id into :wLoc
        from gtlocation
       where location_id = :wKey
       fetch first row only;
    if sqlcode <> 0;
      exsr msgUnknownScan;
      leavesr;
    endif;
  endif;

  curCnt = *blanks;
  exec sql
    select count_id into :curCnt
      from gtvcntopen
     where location_id = :wLoc
     order by count_id
     fetch first row only;

  if sqlcode <> 0 or curCnt = *blanks;
    exsr msgNoCount;
    leavesr;
  endif;

  gtcner(curCnt : langpref);
endsr;

//------------------------------------------------------------------------
begsr loadCountSubfile;
  rrn = 0;
  sflclear = '1';
  write cnthome;
  sflclear = '0';
  cntCount = 0;

  exec sql
    declare cntCsr cursor for
      select count_id, location_id, zone, location_type, count_status,
             assigned_to, coalesce(assigned_name, ''),
             line_count, lines_counted, lines_variance, photo_count
        from gtvcntopen
       order by count_status, count_id
       fetch first 40 rows only;

  exec sql open cntCsr;

  dow cntCount < MAX_ROWS;
    exec sql
      fetch cntCsr into :wCnt, :wLoc, :wZone, :wType, :wStat,
                        :wAsgn, :wAName, :wLines, :wDone, :wVar, :wPho;
    if sqlcode <> 0;
      leave;
    endif;

    cntCount += 1;
    counts(cntCount).cnt = wCnt;

    rrn    += 1;
    cseq    = cntCount;
    ccnt    = wCnt;
    cloc    = wLoc;
    czone   = wZone;
    ctype   = wType;
    cstat   = wStat;
    casgn   = wAsgn;
    caname  = wAName;
    clines  = wLines;
    cdone   = wDone;
    cvar    = wVar;
    cphoto  = wPho;
    write cntsfl;
  enddo;

  exec sql close cntCsr;
  ncounts = cntCount;
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
begsr msgNothingScanned;
  msgkind = 'WARN';
  if langpref = 'FR';
    msg = 'Scannez l''emplacement à dénombrer.';
  else;
    msg = 'Scan the location you are counting.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgNoCount;
  msgkind = 'WARN';
  if langpref = 'FR';
    msg = 'Aucun dénombrement ouvert pour ' + %trim(wLoc) + '.';
  else;
    msg = 'No open count for ' + %trim(wLoc) + '.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgUnknownScan;
  msgkind = 'BAD';
  if langpref = 'FR';
    msg = %trim(payload) + ' : emplacement inconnu.';
  else;
    msg = %trim(payload) + ' is not a known location.';
  endif;
endsr;
