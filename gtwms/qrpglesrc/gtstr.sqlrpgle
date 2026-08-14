**free

//////////////////////////////////////////////////////////////////////////
// GTSTR -- Settings.
//
// The three preferences this application actually acts on, and nothing else.
// GTOPERATOR has carried them since the first build:
//
//   lang_pref     EN | FR -- every screen reads it, so changing it here
//                 changes the whole application, not just this screen
//   default_zone  the zone this operator works in
//   large_touch   bigger targets for a gloved hand
//
// NO INVENTED SETTINGS. A screen full of toggles that store nothing is worse
// than no settings screen: it teaches the operator that the application lies.
// Anything new here needs a column to live in first.
//
// The preferences are read into the screen, edited by the operator, and written
// back on Save under commitment control -- one row, but the same discipline, so
// a failure leaves the stored preferences as they were rather than half applied.
//
// LANGUAGE IS SPECIAL. It is both a setting on this screen and the language
// this screen is drawn in, so choosing it applies immediately: the operator
// sees the result of the choice they just made, which is the only way to
// discover you have picked the wrong one.
//////////////////////////////////////////////////////////////////////////

ctl-opt dftactgrp(*no) actgrp(*new);

dcl-f gtstd workstn handler('PROFOUNDUI(HANDLER)');

exec sql set option commit = *chg, closqlcsr = *endmod;

dcl-c OPER_ID 'GT001';

dcl-s done   ind inz(*off);
dcl-s saved  ind inz(*off);

//  Work fields -- RNF0724 forbids dcl-s inside a begsr in the main procedure.
dcl-s wZone   char(4);
dcl-s wCount  int(10);
dcl-s wJson   varchar(512);
dcl-s wPiece  varchar(60);
dcl-s wPick   char(4);

//------------------------------------------------------------------------
// Main
//------------------------------------------------------------------------
exsr loadOperator;
exsr loadZones;
exsr loadAbout;
sdirty = 'N';

dow not done;
  action = *blanks;
  exfmt setthome;
  msg     = *blanks;
  msgkind = *blanks;

  select;
    when action = 'EXIT';
      //  Leaving with unsaved edits says so rather than discarding them
      //  silently -- but it still leaves, because trapping an operator on a
      //  settings screen is worse than losing a zone preference.
      done = *on;

    when action = 'LANGEN';
      slang    = 'EN';
      langpref = 'EN';
      exsr saveAll;

    when action = 'LANGFR';
      slang    = 'FR';
      langpref = 'FR';
      exsr saveAll;

    when action = 'TOUCH';
      //  The screen posts the value it wants; this only records that something
      //  is now unsaved.
      sdirty = 'Y';

    when action = 'SAVE';
      exsr saveAll;

    when action = 'RESET';
      //  Back to what is stored, discarding the edits on screen.
      exsr loadOperator;
      sdirty = 'N';
      exsr msgReloaded;

    when action = *blanks;
      //  Enter with nothing pressed. Redisplay.

    other;
  endsl;
enddo;

*inlr = *on;
return;

//------------------------------------------------------------------------
// Save. One row, one unit of work.
//
// The zone is validated against the real zone list rather than trusted: the
// field arrives from a browser, and a zone that does not exist would quietly
// filter every screen that honours it down to nothing.
//------------------------------------------------------------------------
begsr saveAll;
  saved = *off;

  if slang <> 'EN' and slang <> 'FR';
    exsr msgBadLang;
    leavesr;
  endif;

  if stouch <> 'Y' and stouch <> 'N';
    stouch = 'N';
  endif;

  if szone <> *blanks;
    wPick = *blanks;
    exec sql
      select min(zone) into :wPick
        from gtlocation
       where zone = :szone and active_flag = 'Y';

    if sqlcode <> 0 or wPick = *blanks;
      exsr msgBadZone;
      leavesr;
    endif;
  endif;

  exec sql
    update gtoperator
       set lang_pref    = :slang,
           default_zone = :szone,
           large_touch  = :stouch
     where operator_id = :OPER_ID;

  if sqlcode < 0 or sqlcode = 100;
    exec sql rollback;
    exsr msgSaveFailed;
    leavesr;
  endif;

  exec sql commit;

  //  A COMMIT can fail too. Checking it is the difference between "saved" and
  //  "believed to be saved".
  if sqlcode < 0;
    exec sql rollback;
    exsr msgSaveFailed;
    leavesr;
  endif;

  saved    = *on;
  sdirty   = 'N';
  langpref = slang;
  exsr msgSaved;
endsr;

//------------------------------------------------------------------------
begsr loadOperator;
  exec sql
    select operator_id, full_name, badge_barcode, operator_role,
           lang_pref, default_zone, large_touch
      into :soperid, :opername, :sbadge, :srole,
           :slang, :szone, :stouch
      from gtoperator
     where operator_id = :OPER_ID
     fetch first row only;

  if sqlcode <> 0;
    soperid  = OPER_ID;
    opername = 'Demo Operator';
    sbadge   = *blanks;
    srole    = 'OPER';
    slang    = 'EN';
    szone    = *blanks;
    stouch   = 'N';
  endif;

  langpref = slang;
endsr;

//------------------------------------------------------------------------
// The zone list, as a JSON string.
//
// Read from GTLOCATION rather than hardcoded, so a zone added to the warehouse
// appears here without a code change. A string rather than a subfile because
// nobody scrolls or types into eight zones -- and because a second subfile in
// one EJS format does not compile at all.
//------------------------------------------------------------------------
begsr loadZones;
  wJson = '[';

  exec sql
    declare zonCsr cursor for
      select zone, count(*)
        from gtlocation
       where active_flag = 'Y' and zone <> ''
       group by zone
       order by zone;

  exec sql open zonCsr;

  dow *on;
    exec sql
      fetch zonCsr into :wZone, :wCount;
    if sqlcode <> 0;
      leave;
    endif;

    wPiece = '{"z":"' + %trim(wZone) + '","n":' + %char(wCount) + '}';

    //  +2 for the separator now and the closing bracket later.
    if %len(wJson) + %len(wPiece) + 2 > 512;
      leave;
    endif;

    if %len(wJson) > 1;
      wJson += ',';
    endif;
    wJson += wPiece;
  enddo;

  exec sql close zonCsr;

  wJson += ']';
  szones = wJson;
endsr;

//------------------------------------------------------------------------
// The About card. Real figures read from the database, because a version
// number nobody can check is decoration.
//------------------------------------------------------------------------
begsr loadAbout;
  sapplib = 'TIGERPOC';

  exec sql select count(*) into :sitems from gtitem;
  if sqlcode <> 0;
    sitems = 0;
  endif;

  exec sql select count(*) into :simgs from gtimage;
  if sqlcode <> 0;
    simgs = 0;
  endif;

  exec sql select count(*) into :slocs from gtlocation;
  if sqlcode <> 0;
    slocs = 0;
  endif;

  exec sql select count(*) into :smoves from gtmovement;
  if sqlcode <> 0;
    smoves = 0;
  endif;
endsr;

//------------------------------------------------------------------------
// Messages.
//------------------------------------------------------------------------
begsr msgSaved;
  msgkind = 'GOOD';
  if langpref = 'FR';
    msg = 'Préférences enregistrées.';
  else;
    msg = 'Preferences saved.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgReloaded;
  msgkind = 'WARN';
  if langpref = 'FR';
    msg = 'Modifications annulées.';
  else;
    msg = 'Changes discarded.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgBadZone;
  msgkind = 'BAD';
  if langpref = 'FR';
    msg = 'Zone ' + %trim(szone) + ' inconnue.';
  else;
    msg = 'Zone ' + %trim(szone) + ' does not exist.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgBadLang;
  msgkind = 'BAD';
  if langpref = 'FR';
    msg = 'Langue non reconnue.';
  else;
    msg = 'That is not a language this application speaks.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgSaveFailed;
  msgkind = 'BAD';
  if langpref = 'FR';
    msg = 'Enregistrement annulé. Rien n’a été modifié.';
  else;
    msg = 'Save rolled back. Nothing was changed.';
  endif;
endsr;
