#!/usr/bin/env node
/*
 * GT Warehouse Mobile -- the scanned-payload lint.
 *
 *   node gtwms/tools/lint-scan-handlers.js
 *
 * ONE RULE: never take a fixed-length substring of a scanned payload without
 * checking its length first.
 *
 *     wKey = %upper(%subst(payload : 1 : 10));      // RNX0100 waiting to happen
 *     wKey = %upper(%trim(payload));                // fine
 *
 * `%subst(x : 1 : n)` requires the string to be at least n characters. A varchar
 * payload is whatever the scanner handed over, and every location id in this
 * warehouse is SIX characters -- so `%subst(payload : 1 : 10)` is an escape
 * message, not a truncation.
 *
 * WHY A LINT AND NOT A TEST. The crash is only reachable when gtbar_parse did
 * NOT resolve the scan, which is the branch no happy-path test takes: every test
 * typed a location that resolved. It shipped in GTCNHR and GTIMVR and took the
 * Cycle Count screen down in the operator's hands -- an empty screen with no
 * fields, which is what an unmonitored RNX0100 looks like from the browser.
 *
 * The same trap was ALREADY guarded in GTRCLR, with a comment explaining it.
 * Knowing it and applying it are different things, so this checks instead.
 *
 * A guarded use passes: GTRCLR wraps its %subst in `if %len(payload) > 20`.
 */

'use strict';

const fs = require('fs');
const path = require('path');

const SRC = path.resolve(__dirname, '../qrpglesrc');
const LOOKBACK = 8;          /* lines above, in which a length guard counts */

let pass = 0, fail = 0;
const ok = (m, d) => { pass++; console.log('  PASS  ' + m + (d ? '  -- ' + d : '')); };
const bad = (m, d) => { fail++; console.log('  FAIL  ' + m + (d ? '  -- ' + d : '')); };

/* %subst(<var> : <start> : <literal length>) -- the fixed-length form. A
 * computed length (%len(x) - 1) is the caller doing its own arithmetic and is
 * not what this rule is about. */
const FIXED_SUBST = /%subst\s*\(\s*([A-Za-z_][\w]*)\s*:\s*\d+\s*:\s*(\d+)\s*\)/g;

/* Variables that hold something a scanner handed us. Anything read from a
 * display file field or a barcode is unbounded in practice. */
const SCANNED = ['payload', 'scanval', 'work', 'raw', 'barcode'];

const findings = [];

for (const f of fs.readdirSync(SRC).filter((x) => x.endsWith('.sqlrpgle') ||
                                                  x.endsWith('.rpgle'))) {
  const lines = fs.readFileSync(path.join(SRC, f), 'utf8').split('\n');

  lines.forEach((line, i) => {
    const code = line.split('//')[0];              /* ignore comments */
    if (!code.trim()) return;

    let m;
    FIXED_SUBST.lastIndex = 0;
    while ((m = FIXED_SUBST.exec(code)) !== null) {
      const varName = m[1].toLowerCase();
      const want = Number(m[2]);
      if (SCANNED.indexOf(varName) === -1) continue;
      if (want <= 1) continue;                     /* a 1-char peek is guarded by
                                                      the len > 0 tests already */

      /* Is there a length guard just above, or on this line?
       *
       * Two forms count, because both are real in this codebase:
       *
       *   1. `%len(payload)` named directly -- GTRCLR's `if %len(payload) > 20`.
       *   2. a `len` variable compared to a number -- GTBAR assigns
       *      `len = result.rawLen` (which IS %len(work)) and then writes
       *      `if len > 16 and isAllDigits(%subst(work : 1 : 2))`. Free-form RPG
       *      short-circuits `and` left to right, so the %subst is never reached
       *      on a short payload. Flagging that would be a false positive, and a
       *      lint that cries wolf gets switched off.
       */
      const from = Math.max(0, i - LOOKBACK);
      const window = lines.slice(from, i + 1).join('\n');
      const guarded =
        new RegExp('%len\\s*\\(\\s*' + varName + '\\s*\\)', 'i').test(window) ||
        /\blen\s*>=?\s*\d+/i.test(window);

      if (!guarded) {
        findings.push({ file: f, line: i + 1, varName, want,
                        text: code.trim().slice(0, 72) });
      }
    }
  });
}

console.log('=== fixed-length substrings of scanned payloads ===');
console.log('    ' + fs.readdirSync(SRC).filter((x) => /\.(sql)?rpgle$/.test(x)).length +
            ' source members checked\n');

if (findings.length) {
  for (const f of findings) {
    bad(f.file + ':' + f.line,
        '%subst(' + f.varName + ' : 1 : ' + f.want + ') with no %len() guard  ->  ' +
        f.text);
  }
  console.log('\n  Use %trim() and let the assignment truncate, or guard with');
  console.log('  `if %len(' + findings[0].varName + ') > n`, the way GTRCLR does.');
} else {
  ok('no unguarded fixed-length substring of a scanned payload',
     'checked ' + SCANNED.join(', '));
}

console.log('\n' + (fail ? 'FAILED' : 'Clean') + ': ' + pass + ' passed, ' + fail + ' failed');
process.exit(fail ? 1 : 0);
