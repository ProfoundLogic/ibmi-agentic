/* ==================================================================
 * Offline pre-flight for the three Inventory & Movement screens.
 *
 * Renders the real templates with the real CSS and the display files' declared
 * field lists, under Genie's hostile globals, at 360/412/1280 in both
 * languages. Audits every <%= identifier %> against the declared fields --
 * one undefined reference aborts the whole render and gives a blank white
 * screen with no error anywhere.
 *
 * Also hit-tests the controls that must survive the narrowest phone.
 *
 * Run: node gtwms/tools/preflight-inv.js
 * ================================================================== */

const ejs = require('ejs');
const fs = require('fs');
const path = require('path');
const { chromium } = require('playwright-core');

const UI = '/workspace/workspace/ibmi-agentic/docs/htdocs/profoundui/userdata/ui';
const CH = '/home/coder/.cache/ms-playwright/chromium-1228/chrome-linux64/chrome';
const SHOT = '/tmp/ejspre';

const ALERTS = (lang) => [1,2,3,4,5,6].map((n) => ({
  _rrn: n, aseq: n, aloc: 'A0' + (10 + n) + '41', azone: 'A',
  asku: 'GROC-0000' + (10 + n),
  adesc: lang === 'FR' ? 'Riz blanc à grain long - 1,5 kg' : 'Long Grain White Rice - 1.5 kg',
  aimg: 0, aoh: n * 2, amin: 12, asugg: 100 + n, abulk: 273
}));

/* A bulk location with several items, one of them flagged for replenishment,
   one with an allocation, and one description long enough to wrap. */
const LOCITEMS = (lang) => [
  { _rrn: 1, iseq: 1, isku: 'GROC-000004',
    idesc: lang === 'FR' ? 'Riz blanc à grain long - 1 kg' : 'Long Grain White Rice - 1 kg',
    idept: 'GROC', iuom: 'EA', iimg: 0, ioh: 4, ialloc: 0, iavail: 4,
    ibulk: 273, irepl: 'Y', isugg: 112, icase: 12 },
  { _rrn: 2, iseq: 2, isku: 'SEAS-000512',
    idesc: lang === 'FR'
      ? 'Ensemble de meubles de patio quatre pièces avec coussins'
      : 'Four Piece Patio Furniture Set with Weather Resistant Cushions',
    idept: 'SEAS', iuom: 'EA', iimg: 0, ioh: 312, ialloc: 24, iavail: 288,
    ibulk: 0, irepl: 'N', isugg: 0, icase: 4 },
  { _rrn: 3, iseq: 3, isku: 'HLTH-000141',
    idesc: lang === 'FR' ? 'Dentifrice - paquet de 12' : 'Toothpaste - 12 Pack',
    idept: 'HLTH', iuom: 'EA', iimg: 0, ioh: 1073, ialloc: 0, iavail: 1073,
    ibulk: 665, irepl: 'N', isugg: 0, icase: 24 }
];

const REASONS = (lang) => [
  { _rrn: 1, rseq: 1, rcode: 'REPL', rdesc: lang === 'FR' ? 'Réapprovisionnement' : 'Replenishment' },
  { _rrn: 2, rseq: 2, rcode: 'PUTA', rdesc: lang === 'FR' ? 'Rangement depuis la zone' : 'Putaway from staging' },
  { _rrn: 3, rseq: 3, rcode: 'RELO', rdesc: lang === 'FR' ? 'Changement d\u2019emplacement' : 'Location change' }
];

const COMMON = (lang) => ({
  action: '', scanval: '', langpref: lang,
  opername: lang === 'FR' ? 'Marie Tremblay' : 'James Okonkwo',
  msg: '', msgkind: ''
});

const SCREENS = [
  {
    name: 'invhome',
    tpl: ['gtinhd', 'invhome.ejs'],
    css: ['gtcommon/gt-theme.css', 'gtinhd/invhome.css'],
    declared: ['action', 'scanval', 'langpref', 'opername', 'msg', 'msgkind',
               'nalerts', 'alrsfl'],
    controls: ['#gt-scan-start', '#gt-scan-manual'],
    minTarget: ['#gt-scan-manual'],
    sticky: { band: '.gt-inv-top', keep: ['#gt-scan-start', '#gt-scan-manual'],
              under: '.gt-alr-card' },
    data: (lang) => Object.assign(COMMON(lang), { nalerts: 51, alrsfl: ALERTS(lang) })
  },
  {
    name: 'invloc',
    tpl: ['gtinld', 'invloc.ejs'],
    css: ['gtcommon/gt-theme.css', 'gtinld/invloc.css'],
    declared: ['action', 'scanval', 'langpref', 'opername', 'msg', 'msgkind',
               'hloc', 'hzone', 'htype', 'hactive', 'haisle', 'hbay', 'hlevel',
               'hitems', 'hqty', 'hcap', 'hpct', 'itmsfl'],
    controls: ['#gt-scan-start', '#gt-scan-manual', '.gt-ilc-repl', '.gt-ilc-move'],
    minTarget: ['.gt-ilc-repl', '.gt-ilc-move'],
    sticky: { band: '.gt-inv-top', keep: ['#gt-scan-start', '#gt-scan-manual'],
              under: '.gt-ilc-row' },
    data: (lang) => Object.assign(COMMON(lang), {
      hloc: 'D01011', hzone: 'D', htype: 'BULK', hactive: 'Y',
      haisle: '01', hbay: '01', hlevel: '1',
      hitems: 26, hqty: 11386, hcap: 1155, hpct: 100,
      itmsfl: LOCITEMS(lang) })
  },
  {
    name: 'invmove',
    tpl: ['gtimvd', 'invmove.ejs'],
    css: ['gtcommon/gt-theme.css', 'gtimvd/invmove.css'],
    declared: ['action', 'scanval', 'langpref', 'opername', 'msg', 'msgkind',
               'mfrom', 'mfromtyp', 'mfromzon', 'msku', 'mdesc', 'muom', 'mimg',
               'mcase', 'mavail', 'monhand', 'mto', 'mtotyp', 'mtozon',
               'mtoqty', 'mtook', 'mqty', 'mreason', 'mdone',
               'mfrombef', 'mfromaft', 'mtobef', 'mtoaft', 'rsnsfl'],
    controls: ['#gt-scan-start', '#gt-scan-manual', '#gt-mv-qty',
               '#gt-mv-reason', '.gt-btn-go', '.gt-mv-all'],
    minTarget: ['#gt-mv-qty', '#gt-mv-reason', '.gt-btn-go', '.gt-mv-all'],
    data: (lang) => Object.assign(COMMON(lang), {
      mfrom: 'D01011', mfromtyp: 'BULK', mfromzon: 'D',
      msku: 'GROC-000004',
      mdesc: lang === 'FR' ? 'Riz blanc à grain long - 1 kg' : 'Long Grain White Rice - 1 kg',
      muom: 'EA', mimg: 0, mcase: 12,
      monhand: 273, mavail: 273,
      /* Destination already confirmed -- the replenish path, and the state
         with the most on screen at once. */
      mto: 'A01041', mtotyp: 'PICK', mtozon: 'A', mtoqty: 4, mtook: 'Y',
      mqty: 112, mreason: 'REPL', mdone: 'N',
      mfrombef: 0, mfromaft: 0, mtobef: 0, mtoaft: 0,
      rsnsfl: REASONS(lang) })
  },
  {
    /* The same screen after the move: the before/after panel replaces the
       form, so it is a different render and gets its own case. */
    name: 'invmove-done',
    tpl: ['gtimvd', 'invmove.ejs'],
    css: ['gtcommon/gt-theme.css', 'gtimvd/invmove.css'],
    declared: ['action', 'scanval', 'langpref', 'opername', 'msg', 'msgkind',
               'mfrom', 'mfromtyp', 'mfromzon', 'msku', 'mdesc', 'muom', 'mimg',
               'mcase', 'mavail', 'monhand', 'mto', 'mtotyp', 'mtozon',
               'mtoqty', 'mtook', 'mqty', 'mreason', 'mdone',
               'mfrombef', 'mfromaft', 'mtobef', 'mtoaft', 'rsnsfl'],
    controls: ['.gt-btn-primary'],
    data: (lang) => Object.assign(COMMON(lang), {
      msg: lang === 'FR' ? '112 déplacé de D01011 vers A01041.'
                         : '112 moved from D01011 to A01041.',
      msgkind: 'GOOD',
      mfrom: 'D01011', mfromtyp: 'BULK', mfromzon: 'D',
      msku: 'GROC-000004',
      mdesc: lang === 'FR' ? 'Riz blanc à grain long - 1 kg' : 'Long Grain White Rice - 1 kg',
      muom: 'EA', mimg: 0, mcase: 12,
      monhand: 161, mavail: 161,
      mto: 'A01041', mtotyp: 'PICK', mtozon: 'A', mtoqty: 116, mtook: 'Y',
      mqty: 112, mreason: 'REPL', mdone: 'Y',
      mfrombef: 273, mfromaft: 161, mtobef: 4, mtoaft: 116,
      rsnsfl: REASONS(lang) })
  }
];

const CASES = [{ lang: 'EN', w: 412, h: 915 }, { lang: 'FR', w: 412, h: 915 },
                { lang: 'FR', w: 360, h: 800 }, { lang: 'EN', w: 1280, h: 900 }];

(async () => {
  fs.mkdirSync(SHOT, { recursive: true });
  const browser = await chromium.launch({ executablePath: CH, args: ['--no-sandbox'] });
  let fail = 0;

  for (const sc of SCREENS) {
    const src = fs.readFileSync(path.join(UI, sc.tpl[0], sc.tpl[1]), 'utf8');
    const css = sc.css.map((f) => fs.readFileSync(path.join(UI, f), 'utf8')).join('\n');

    /* Locals the template declares itself are not display-file fields. */
    const locals = new Set();
    [...src.matchAll(/\b(?:var|function)\s+([A-Za-z_$][\w$]*)/g)].forEach((m) => locals.add(m[1]));
    [...src.matchAll(/function\s*\(([^)]*)\)/g)].forEach((m) =>
      m[1].split(',').map((x) => x.trim()).filter(Boolean).forEach((x) => locals.add(x)));
    const refs = new Set([...src.matchAll(/<%[=-]\s*([A-Za-z_$][\w$]*)/g)].map((m) => m[1]));
    const unknown = [...refs].filter((r) =>
      !sc.declared.includes(r) && !locals.has(r) &&
      ['typeof', 'Number', 'String', 'Math', 'JSON'].indexOf(r) === -1);
    console.log('\n' + sc.name + '  identifier audit: ' +
                (unknown.length ? 'UNKNOWN -> ' + unknown.join(', ') : 'clean'));
    if (unknown.length) fail++;

    for (const c of CASES) {
      let html;
      try { html = ejs.render(src, sc.data(c.lang)); }
      catch (e) { console.log('  RENDER FAILED', c.lang, c.w, e.message); fail++; continue; }

      const page = await browser.newPage({ viewport: { width: c.w, height: c.h } });
      await page.setContent('<!doctype html><meta charset="utf-8">' +
        '<style>html,body{margin:0;padding:0}div{white-space:nowrap;z-index:10;padding:1px}</style>' +
        '<style>' + css + '</style>' + html, { waitUntil: 'load' });

      const o = await page.evaluate(() => {
        const de = document.documentElement; const wide = [];
        document.querySelectorAll('.gt-app *').forEach((el) => {
          const r = el.getBoundingClientRect();
          if (r.right > de.clientWidth + 1) {
            wide.push((typeof el.className === 'string' ? el.className : el.tagName) +
                      ' r=' + Math.round(r.right));
          }
        });
        return { sw: de.scrollWidth, cw: de.clientWidth, wide: wide.slice(0, 3) };
      });
      const bad = o.sw > o.cw + 1;
      if (bad) fail++;
      console.log('  ' + (c.lang + ' ' + c.w + 'px').padEnd(11) +
                  ' sw=' + o.sw + ' cw=' + o.cw + ' ' +
                  (bad ? 'OVERFLOW -> ' + o.wide.join(' | ') : 'no overflow'));

      /* Controls that must be present, sized and actually hit-testable. A
       * visible-but-covered button screenshots perfectly and does nothing. */
      const ctl = await page.evaluate((sels) => sels.map((sel) => {
        const el = document.querySelector(sel);
        if (!el) return sel + ': MISSING';
        const r = el.getBoundingClientRect();
        if (!r.width || !r.height) return sel + ': HIDDEN';
        if (r.bottom < 0 || r.top > innerHeight) return null;   /* below the fold is fine */
        const hitEl = document.elementFromPoint(r.left + r.width / 2, r.top + r.height / 2);
        if (!hitEl || !(hitEl === el || el.contains(hitEl))) {
          return sel + ': COVERED by ' +
                 (hitEl && typeof hitEl.className === 'string' && hitEl.className
                   ? hitEl.className : hitEl ? hitEl.tagName : 'nothing');
        }
        return null;
      }).filter(Boolean), sc.controls || []);
      /* Two failures the earlier checks let through, so they are asserted now:
         a quantity box squeezed so narrow its own value is invisible, and a
         label spilling outside its button's border. Both looked "fine" to an
         overflow check bounded on the viewport, because neither crossed it. */
      const squashed = await page.evaluate(() => {
        const bad = [];
        document.querySelectorAll('.gt-lin-qty').forEach((b, i) => {
          const w = b.getBoundingClientRect().width;
          if (w < 70) bad.push('.gt-lin-qty[' + i + '] only ' + Math.round(w) + 'px wide');
          /* An <input type="number"> silently EMPTIES itself when handed a
             value it cannot parse -- which is what a localised "24,00" is.
             The French screen came up with every box blank and looked fine. */
          if (b.value === '') {
            bad.push('.gt-lin-qty[' + i + '] value rejected by the browser: ' +
                     JSON.stringify(b.getAttribute('value')));
          }
        });
        /* Any control whose content is wider than the control itself. */
        document.querySelectorAll('.gt-step, .gt-lin-full, .gt-lin-chip, .gt-btn')
          .forEach((el) => {
            if (el.scrollWidth > el.clientWidth + 1) {
              bad.push((el.className || el.tagName) + ' text overflows by ' +
                       (el.scrollWidth - el.clientWidth) + 'px');
            }
          });
        return bad.slice(0, 4);
      });
      if (squashed.length) { fail++; console.log('               SQUASHED -> ' + squashed.join(' | ')); }

      /* A control can be visible, hit-testable and still too small to hit.
         The spinners this replaced were exactly that. */
      const small = await page.evaluate((sels) => sels.map((sel) => {
        const el = document.querySelector(sel);
        if (!el) return null;
        const r = el.getBoundingClientRect();
        return (r.height < 44 || r.width < 40)
          ? sel + ': ' + Math.round(r.width) + 'x' + Math.round(r.height)
          : null;
      }).filter(Boolean), sc.minTarget || []);
      if (small.length) { fail++; console.log('               TOO SMALL -> ' + small.join(' | ')); }

      if (ctl.length) { fail++; console.log('               CONTROLS -> ' + ctl.join(' | ')); }
      else if ((sc.controls || []).length) {
        console.log('               controls ok (' + sc.controls.length + ' checked)');
      }

      await page.screenshot({
        path: SHOT + '/rcv-' + sc.name + '-' + c.lang.toLowerCase() + '-' + c.w + '.png',
        fullPage: true
      });
      await page.close();
    }
  }

  await browser.close();
  console.log(fail ? '\n' + fail + ' case(s) failed' : '\nAll cases clean');
  process.exit(fail ? 1 : 0);
})();
