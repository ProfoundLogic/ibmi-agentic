**free

//////////////////////////////////////////////////////////////////////////
// GTMNUR -- GT Warehouse Mobile main menu.
//
// The first screen an operator sees after option 4 on the sign-on menu.
// Presents the application as large touch tiles with live counts pulled
// straight from DB2, so the menu is never a static list.
//
// Language handling is deliberately server-side. The EJS template resolves
// its own strings from langpref, so the EN/FR toggle is a round trip: this
// program flips the operator's stored preference and the screen re-renders.
// That keeps the screen fully readable with no client-side JavaScript.
//////////////////////////////////////////////////////////////////////////

ctl-opt dftactgrp(*no) actgrp(*new);

dcl-f gtmnud workstn handler('PROFOUNDUI(HANDLER)');

// Commitment control is deliberate across this project -- every table is
// journaled. This is a considered deviation from the house default of
// commit = *none, which suits non-journaled files.
exec sql set option commit = *chg, closqlcsr = *endmod;

dcl-pr gtscnr extpgm;
end-pr;

dcl-pr gtitmr extpgm;
end-pr;

dcl-pr gtrchr extpgm;
end-pr;

dcl-s done ind inz(*off);

// Until GTSGNR (badge sign-on) lands, the menu runs as a known operator.
dcl-c DEFAULT_OPERATOR 'GT001';

//------------------------------------------------------------------------
// Main
//------------------------------------------------------------------------
operid = DEFAULT_OPERATOR;
exsr loadOperator;

dow not done;
  exsr loadCounts;

  action = *blanks;
  exfmt gtmenu;
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

    when action = 'ITEM';
      // Item Lookup. Called, not chained, so exit lands back on the menu.
      gtitmr();

    when action = 'RECV';
      // Receiving. Scan the pallet label, confirm the lines, post.
      gtrchr();

    when action = 'SCAN';
      // Scan Lab. Called rather than chained so the operator lands back
      // here on exit, which is what a menu should do.
      gtscnr();

    when action = *blanks;
      // Enter with no tile pressed -- just redisplay.

    other;
      // Every other tile is a real action code that its application will
      // claim as it is built. Until then, say so honestly rather than
      // failing silently.
      exsr notBuiltYet;
  endsl;
enddo;

*inlr = *on;
return;

//------------------------------------------------------------------------
// Read the operator record. Preference drives the whole screen's language.
//------------------------------------------------------------------------
begsr loadOperator;
  exec sql
    select full_name, lang_pref, operator_role
      into :opername, :langpref, :operrole
      from gtoperator
     where operator_id = :operid
       and active_flag = 'Y'
     fetch first row only;

  if sqlcode <> 0;
    // No operator row: still usable, just unnamed. A missing preference
    // must never stop the menu from displaying.
    opername = 'Demo Operator';
    langpref = 'EN';
    operrole = 'OPER';
  endif;
endsr;

//------------------------------------------------------------------------
// Persist the language choice so it survives to the next screen and the
// next session.
//------------------------------------------------------------------------
begsr saveLanguage;
  exec sql
    update gtoperator
       set lang_pref = :langpref
     where operator_id = :operid;

  if sqlcode < 0;
    exec sql rollback;
  else;
    exec sql commit;
  endif;
endsr;

//------------------------------------------------------------------------
// Live badge counts. One round trip per value keeps the SQL readable; the
// tables are small and this screen is not in any hot path.
//------------------------------------------------------------------------
begsr loadCounts;
  exec sql
    select count(*) into :nrcpts
      from gtrcpthdr
     where receipt_status in ('OPEN', 'PART');

  exec sql
    select count(*) into :ncounts
      from gtcounthdr
     where count_status = 'OPEN';

  exec sql
    select count(*) into :nreplen
      from gtvreplen;

  exec sql
    select count(*) into :nitems
      from gtitem
     where item_status = 'A';
endsr;

//------------------------------------------------------------------------
// Placeholder routing. Replaced application by application as Wave 1 lands.
//------------------------------------------------------------------------
begsr notBuiltYet;
  if langpref = 'FR';
    msg = 'Cette application n''est pas encore construite : ' + %trim(action);
  else;
    msg = 'That application is not built yet: ' + %trim(action);
  endif;
endsr;
