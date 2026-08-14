**free

//////////////////////////////////////////////////////////////////////////
// GTSIMR -- Simple camera test.
//
// WHY THIS EXISTS
//
// The camera works on Android and on the desk, and on one iPad it opens,
// shows a live picture and never reads a barcode. Seven rounds of fixes went
// into the main application without settling it, and each round had to be
// diagnosed through the whole stack: eleven screens, a shared scan component,
// a service program, commitment control, a cache stamp and a Genie snapshot.
// Too many moving parts to isolate one browser's behaviour.
//
// So this program is deliberately the smallest thing that can still fail the
// same way. It is RPG plus a Rich Display File and nothing else:
//
//   - no SQL, no tables, no journalling, no commitment control
//   - no GTBAR, no service program, no binding directory
//   - no shared scan component -- the screen carries its own JavaScript
//   - no images, no navigation, one format, one subfile
//
// Whatever the screen reads, this program echoes back. That makes the round
// trip provable: a value in the frame below came from the device, through
// Profound UI, into RPG and back out again. If the value appears, the camera
// and the decoder both work and the fault in the main application is
// somewhere in its own plumbing. If it never appears, the fault is in the
// browser, and DIAG says which part.
//
// Reached from option 5 of the sign-on menu via GTSIMST. Throw it away once
// the iPad question is closed -- nothing in the application depends on it.
//////////////////////////////////////////////////////////////////////////

ctl-opt dftactgrp(*no) actgrp(*new);

dcl-f gtsimd workstn sfile(simsfl : rrn) handler('PROFOUNDUI(HANDLER)');

dcl-c HISTORY_MAX 40;

// Everything read this session, newest first. Held in the program: this is a
// diagnostic session, not a business record, and adding a table would drag in
// the journalling and commitment control this program exists to avoid.
dcl-ds history qualified dim(HISTORY_MAX);
  val   varchar(120);
  symb  varchar(20);
  tim   char(8);
end-ds;

dcl-s histCount int(10) inz(0);
dcl-s rrn       int(10) inz(0);
dcl-s i         int(10);
dcl-s done      ind inz(*off);
dcl-s payload   varchar(120);
dcl-s symbol    varchar(20);

//------------------------------------------------------------------------
// Main
//------------------------------------------------------------------------
langpref = 'EN';
exsr clearResult;

dow not done;
  exsr loadSubfile;

  action = *blanks;
  exfmt simtest;
  msg = *blanks;

  select;
    when action = 'EXIT';
      done = *on;

    when action = 'SCAN';
      exsr doScan;

    when action = 'CLEAR';
      histCount = 0;
      exsr clearResult;

    when action = 'LANGEN';
      langpref = 'EN';

    when action = 'LANGFR';
      langpref = 'FR';

    when action = 'DIAG';
      // The screen has pushed its diagnostics into DIAG. Nothing to do but
      // redisplay: the value is now in the screen data, which is the one
      // channel that has reached us from the device every single time.
      if langpref = 'FR';
        msg = 'Diagnostics envoyés au serveur.';
      else;
        msg = 'Diagnostics sent to the server.';
      endif;

    other;
      // Enter with nothing pressed, or a keyed value with no action.
  endsl;
enddo;

*inlr = *on;
return;

//------------------------------------------------------------------------
// doScan -- echo whatever the screen read, and remember it.
//
// No validation and no lookup. "Any barcode" means any barcode: a payload
// this warehouse has never seen is a successful read, not an error.
//------------------------------------------------------------------------
begsr doScan;
  payload = %trim(scanval);
  symbol  = %trim(lastsymb);

  if payload = '';
    if langpref = 'FR';
      msg = 'Rien à lire. Ouvrez la caméra ou saisissez une valeur.';
    else;
      msg = 'Nothing to read. Open the camera or type a value in.';
    endif;
  else;
    if symbol = '';
      symbol = 'UNKNOWN';
    endif;

    lastval  = payload;
    lastsymb = symbol;
    lastlen  = %len(payload);
    lasttime = %char(%time() : *hms);

    // Newest first, so the operator reads the top line without scrolling.
    if histCount < HISTORY_MAX;
      histCount += 1;
    endif;
    for i = histCount downto 2;
      history(i) = history(i - 1);
    endfor;
    history(1).val  = payload;
    history(1).symb = symbol;
    history(1).tim  = lasttime;

    if langpref = 'FR';
      msg = 'Lu : ' + payload;
    else;
      msg = 'Read: ' + payload;
    endif;
  endif;

  // Never leave the inbound value on the screen -- otherwise the next scan of
  // the same barcode looks like a fresh read of a field that was never cleared.
  scanval = *blanks;
endsr;

//------------------------------------------------------------------------
// clearResult
//------------------------------------------------------------------------
begsr clearResult;
  lastval  = *blanks;
  lastsymb = *blanks;
  lastlen  = 0;
  lasttime = *blanks;
  scanval  = *blanks;
endsr;

//------------------------------------------------------------------------
// loadSubfile -- the control record IS the format, exactly as every other
// screen in this project does it. sflclear is a character flag the compiler
// generates from the "clear" property in the JSON source.
//------------------------------------------------------------------------
begsr loadSubfile;
  rrn = 0;
  sflclear = '1';
  write simtest;
  sflclear = '0';

  for i = 1 to histCount;
    sseq  = i;
    sval  = %subst(history(i).val : 1 : %min(60 : %len(history(i).val)));
    ssymb = history(i).symb;
    stime = history(i).tim;
    rrn += 1;
    write simsfl;
  endfor;
endsr;
