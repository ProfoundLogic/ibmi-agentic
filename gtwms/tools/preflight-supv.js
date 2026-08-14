/* ==================================================================
 * Offline pre-flight for the Supervisor View.
 *
 *   node gtwms/tools/preflight-supv.js
 *
 * Two renders, because the interesting states are different shapes: a busy
 * floor with variances and alerts, and an empty one. The empty case matters --
 * "nothing open" is the state a demo starts in.
 *
 * ZONESTAT and the subfile both go in as the real thing: a JSON string that
 * RPG built and a subfile array with mixed RECV/CNT rows, because the two card
 * layouts differ and only one of them was designed first.
 * ================================================================== */

'use strict';

const { run } = require('./preflight-lib.js');

const DECLARED = ['action', 'langpref', 'opername', 'operrole', 'msg', 'msgkind',
                  'nrcpts', 'nrlines', 'nrdone', 'nrvar',
                  'ncounts', 'nclines', 'ncdone', 'ncvar',
                  'nalerts', 'nstaged', 'nmoves', 'nwork',
                  'zonestat', 'wrksfl'];

const COMMON = (lang) => ({
  action: '', langpref: lang, msg: '', msgkind: '',
  opername: lang === 'FR' ? 'Marie Tremblay' : 'James Okonkwo',
  operrole: 'SUPV'
});

/* Eight zones, exactly what loadZones emits, at the length it emits it. */
const ZONESTAT = JSON.stringify([
  { z: 'A', l: 432, s: 408, q: 24770, a: 51 },
  { z: 'B', l: 144, s: 130, q: 41022, a: 0 },
  { z: 'C', l: 144, s: 141, q: 38914, a: 3 },
  { z: 'D', l: 576, s: 680, q: 245107, a: 0 },
  { z: 'E', l: 96, s: 88, q: 12040, a: 7 },
  { z: 'F', l: 96, s: 71, q: 9330, a: 0 },
  { z: 'G', l: 48, s: 40, q: 5120, a: 2 },
  { z: 'H', l: 12, s: 0, q: 0, a: 0 }
]);

/* Mixed types, and a vendor name long enough to test wrapping -- the header
 * title splitting mid-word was found exactly this way. */
const WORK = (lang) => [
  { _rrn: 1, wseq: 1, wtype: 'RECV', wdoc: 'RC00000004',
    wwhere: 'Door D004 - Maritime Home Goods Distribution',
    wwho: 'PO0000048261', wstat: 'PART',
    wlines: 31, wdone: 12, wvar: 4, wphoto: 2 },
  { _rrn: 2, wseq: 2, wtype: 'CNT', wdoc: 'CN00000001',
    wwhere: 'D01011', wwho: lang === 'FR' ? 'Marie Tremblay' : 'Marie Tremblay',
    wstat: 'CNTD', wlines: 26, wdone: 26, wvar: 3, wphoto: 0 },
  { _rrn: 3, wseq: 3, wtype: 'RECV', wdoc: 'RC00000001',
    wwhere: 'Door D001 - Northbridge Wholesale Ltd.',
    wwho: 'PO0000031884', wstat: 'OPEN',
    wlines: 60, wdone: 0, wvar: 0, wphoto: 0 },
  { _rrn: 4, wseq: 4, wtype: 'CNT', wdoc: 'CN00000004',
    wwhere: 'A01061', wwho: 'Luc Bergeron', wstat: 'OPEN',
    wlines: 1, wdone: 0, wvar: 0, wphoto: 0 }
];

const SCREENS = [
  {
    name: 'supv-busy',
    tpl: ['gtsvd', 'supv.ejs'],
    css: ['gtcommon/gt-theme.css', 'gtsvd/supv.css'],
    declared: DECLARED,
    /* Both footer buttons must survive the narrowest phone: a supervisor with
     * no way back to the menu is trapped. */
    controls: ['.gt-btn-primary', '.gt-btn-ghost', '.gt-sv-hit'],
    minTarget: ['.gt-btn-primary', '.gt-btn-ghost'],
    data: (lang) => Object.assign(COMMON(lang), {
      nrcpts: 8, nrlines: 196, nrdone: 74, nrvar: 11,
      ncounts: 4, nclines: 76, ncdone: 27, ncvar: 5,
      nalerts: 51, nstaged: 1240, nmoves: 37, nwork: 4,
      zonestat: ZONESTAT, wrksfl: WORK(lang)
    })
  },
  {
    /* The empty floor. Everything divides by zero here if the percentage guard
     * is wrong, and the "nothing open" card is the state a demo opens in. */
    name: 'supv-clear',
    tpl: ['gtsvd', 'supv.ejs'],
    css: ['gtcommon/gt-theme.css', 'gtsvd/supv.css'],
    declared: DECLARED,
    controls: ['.gt-btn-primary', '.gt-btn-ghost'],
    minTarget: ['.gt-btn-primary'],
    data: (lang) => Object.assign(COMMON(lang), {
      nrcpts: 0, nrlines: 0, nrdone: 0, nrvar: 0,
      ncounts: 0, nclines: 0, ncdone: 0, ncvar: 0,
      nalerts: 0, nstaged: 0, nmoves: 0, nwork: 0,
      /* An empty zone list, and an empty string, are both real: the cursor can
       * return nothing and RPG still sends '[]'. */
      zonestat: '[]', wrksfl: []
    })
  },
  {
    /* A truncated ZONESTAT. RPG stops before overrunning CHAR(1024), so the
     * template must survive a string that is not valid JSON rather than throw
     * and take the whole screen with it. */
    name: 'supv-badjson',
    tpl: ['gtsvd', 'supv.ejs'],
    css: ['gtcommon/gt-theme.css', 'gtsvd/supv.css'],
    declared: DECLARED,
    controls: ['.gt-btn-primary'],
    data: (lang) => Object.assign(COMMON(lang), {
      nrcpts: 1, nrlines: 10, nrdone: 5, nrvar: 0,
      ncounts: 0, nclines: 0, ncdone: 0, ncvar: 0,
      nalerts: 0, nstaged: 0, nmoves: 0, nwork: 0,
      zonestat: '[{"z":"A","l":432,"s":408,"q":247',
      wrksfl: []
    })
  }
];

run('Supervisor View pre-flight', SCREENS);
