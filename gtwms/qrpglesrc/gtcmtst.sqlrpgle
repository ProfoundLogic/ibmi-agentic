**free

//////////////////////////////////////////////////////////////////////////
// GTCMTST -- Wave 0 spike: prove commitment control actually behaves.
//
// Not part of the application. It exists because every posting path in this
// project depends on ROLLBACK discarding work and COMMIT keeping it, and that
// is worth proving before eight programs are written on top of the assumption.
//
// Two rows are written to GTMOVEMENT. The first is rolled back, the second is
// committed. Verification is external and deliberately blunt: after this runs
// against an empty GTMOVEMENT, exactly ONE row may survive, and its reason
// code must be COMMIT-ME.
//
// Note the deliberate deviation from the project's usual RPG standard, which
// sets `commit = *none`. That default suits non-journaled files; here every
// table is journaled and commitment control is the whole point.
//////////////////////////////////////////////////////////////////////////

ctl-opt dftactgrp(*no) actgrp(*new);

exec sql set option commit = *chg, closqlcsr = *endmod;

dcl-pr runCmd extpgm('QCMDEXC');
  cmd char(3000) const options(*varsize);
  len packed(15:5) const;
end-pr;

dcl-s cmd     varchar(300);
dcl-s rowsNow int(10);

// Unqualified SQL resolves through the library list at run time, so nothing in
// this project ever names its own library. In the real application GTSTART
// does this once on entry; here we do it inline so the spike is self-contained.
cmd = 'ADDLIBLE LIB(TIGERPOC) POSITION(*FIRST)';
monitor;
  runCmd(cmd : %len(cmd));
on-error;
  // CPF2103 -- already on the library list. Nothing to do.
endmon;

// Start from a known state so the external check is unambiguous.
exec sql delete from gtmovement;
exec sql commit;

// ---- 1. Write, then throw it away. ----------------------------------
exec sql
  insert into gtmovement
         (movement_type, sku, from_location, to_location,
          qty, reason_code, source_doc)
  values ('ADJC', 'GROC-000001', 'A01011', '', 5, 'ADJD', 'ROLLBACKME');

exec sql rollback;

// ---- 2. Write, then keep it. ----------------------------------------
exec sql
  insert into gtmovement
         (movement_type, sku, from_location, to_location,
          qty, reason_code, source_doc)
  values ('ADJC', 'GROC-000001', 'A01011', '', 7, 'ADJU', 'COMMIT-ME');

exec sql commit;

// Report into the joblog as well, so a failure is diagnosable from the log
// rather than only from the external query.
exec sql select count(*) into :rowsNow from gtmovement;

cmd = 'SNDPGMMSG MSG(''GTCMTST rows surviving: ' + %char(rowsNow) + ''')';
monitor;
  runCmd(cmd : %len(cmd));
on-error;
endmon;

*inlr = *on;
return;
