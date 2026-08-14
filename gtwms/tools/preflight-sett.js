/* ==================================================================
 * Offline pre-flight for Settings.
 *
 *   node gtwms/tools/preflight-sett.js
 *
 * Four renders. The two that matter most are the LARGE TOUCH pair: the whole
 * point of that option is that the controls get bigger, so the check is that
 * they measurably do -- and that nothing overflows once they have. A preference
 * that makes the screen unusable at 360px is worse than not offering it.
 * ================================================================== */

'use strict';

const { run } = require('./preflight-lib.js');

const DECLARED = ['action', 'langpref', 'opername', 'msg', 'msgkind',
                  'soperid', 'sbadge', 'srole', 'slang', 'szone', 'stouch',
                  'sdirty', 'szones', 'sapplib', 'sitems', 'simgs', 'slocs',
                  'smoves'];

const COMMON = (lang) => ({
  action: '', langpref: lang, msg: '', msgkind: '',
  opername: 'Marie Tremblay',
  soperid: 'GT001', sbadge: 'GT-BADGE-0001', srole: 'OPER',
  sapplib: 'TIGERPOC', sitems: 408, simgs: 102, slocs: 1032, smoves: 37
});

/* Eight zones, which is what GTLOCATION actually has -- and the count that has
 * to wrap onto three columns at 360px without spilling. */
const ZONES = JSON.stringify([
  { z: 'A', n: 432 }, { z: 'B', n: 144 }, { z: 'C', n: 144 },
  { z: 'D', n: 576 }, { z: 'E', n: 96 },  { z: 'F', n: 96 },
  { z: 'G', n: 48 },  { z: 'H', n: 12 }
]);

const BASE = (lang) => Object.assign(COMMON(lang), {
  slang: lang, szone: 'D', stouch: 'N', sdirty: 'N', szones: ZONES
});

const SCREENS = [
  {
    name: 'settings',
    tpl: ['gtstd', 'settings.ejs'],
    css: ['gtcommon/gt-theme.css', 'gtstd/settings.css'],
    declared: DECLARED,
    controls: ['.gt-btn-primary', '.gt-btn-ghost', '.gt-st-opt'],
    minTarget: ['.gt-btn-primary', '.gt-st-opt'],
    data: (lang) => BASE(lang)
  },
  {
    /* Unsaved edits: an extra banner and a third footer button, which is the
     * case that crowds a 360px footer. */
    name: 'settings-dirty',
    tpl: ['gtstd', 'settings.ejs'],
    css: ['gtcommon/gt-theme.css', 'gtstd/settings.css'],
    declared: DECLARED,
    controls: ['.gt-btn-primary', '.gt-btn-ghost'],
    minTarget: ['.gt-btn-primary'],
    data: (lang) => Object.assign(BASE(lang), { sdirty: 'Y', szone: 'B' })
  },
  {
    /* LARGE TOUCH on. Bigger controls in the longer language at the narrowest
     * width is the combination that overflows if it is going to. */
    name: 'settings-largetouch',
    tpl: ['gtstd', 'settings.ejs'],
    css: ['gtcommon/gt-theme.css', 'gtstd/settings.css'],
    declared: DECLARED,
    controls: ['.gt-btn-primary', '.gt-st-opt'],
    minTarget: ['.gt-btn-primary', '.gt-st-opt'],
    data: (lang) => Object.assign(BASE(lang), { stouch: 'Y', sdirty: 'Y' })
  },
  {
    /* A truncated zone list. RPG stops before overrunning CHAR(512), so the
     * template must degrade to no zone buttons rather than throw and take the
     * whole screen with it -- leaving the operator no way out. */
    name: 'settings-badjson',
    tpl: ['gtstd', 'settings.ejs'],
    css: ['gtcommon/gt-theme.css', 'gtstd/settings.css'],
    declared: DECLARED,
    controls: ['.gt-btn-primary', '.gt-btn-ghost'],
    data: (lang) => Object.assign(BASE(lang), { szones: '[{"z":"A","n":43' })
  }
];

run('Settings pre-flight', SCREENS);
