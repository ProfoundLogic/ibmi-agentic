/* ==================================================================
 * Offline pre-flight for the three Cycle Count screens.
 *
 * Renders the real templates with the real CSS and the display files' declared
 * field lists, under Genie's hostile globals, at 360/412/1280 in both
 * languages. Audits every <%= identifier %> against the declared fields --
 * one undefined reference aborts the whole render and gives a blank white
 * screen with no error anywhere.
 *
 * Also hit-tests the controls that must survive the narrowest phone.
 *
 * Run: node gtwms/tools/preflight-cnt.js
 * ================================================================== */

const ejs = require('ejs');
const fs = require('fs');
const path = require('path');
const { chromium } = require('playwright-core');

const UI = '/workspace/workspace/ibmi-agentic/docs/htdocs/profoundui/userdata/ui';
const CH = '/home/coder/.cache/ms-playwright/chromium-1228/chrome-linux64/chrome';
const SHOT = '/tmp/ejspre';

const COMMON = (lang) => ({
  action: '', scanval: '', langpref: lang,
  opername: lang === 'FR' ? 'Marie Tremblay' : 'James Okonkwo',
  msg: '', msgkind: ''
});

const COUNTS = (lang) => [
  { _rrn: 1, cseq: 1, ccnt: 'CN00000001', cloc: 'D01011', czone: 'D', ctype: 'BULK',
    cstat: 'OPEN', casgn: 'GT001', caname: 'Marie Tremblay',
    clines: 26, cdone: 0, cvar: 0, cphoto: 0 },
  { _rrn: 2, cseq: 2, ccnt: 'CN00000004', cloc: 'A01061', czone: 'A', ctype: 'PICK',
    cstat: 'CNTD', casgn: 'GT004', caname: 'Jean-Philippe Bourassa',
    clines: 1, cdone: 1, cvar: 1, cphoto: 1 }
];

/* Blind entry: note there is no expected quantity in this data, because the
   display file has no field for one. */
const ENTRY = (lang) => [
  { _rrn: 1, eseq: 1, eline: 1, esku: 'GROC-000004',
    edesc: lang === 'FR' ? 'Riz blanc à grain long - 1 kg' : 'Long Grain White Rice - 1 kg',
    edept: 'GROC', euom: 'EA', eimg: 0, ecnt: 0, eflag: 'N', ecase: 12, eqty: 0 },
  { _rrn: 2, eseq: 2, eline: 2, esku: 'SEAS-000512',
    edesc: lang === 'FR'
      ? 'Ensemble de meubles de patio quatre pièces avec coussins'
      : 'Four Piece Patio Furniture Set with Weather Resistant Cushions',
    edept: 'SEAS', euom: 'EA', eimg: 0, ecnt: 312, eflag: 'Y', ecase: 4, eqty: 312 },
  { _rrn: 3, eseq: 3, eline: 3, esku: 'HLTH-000141',
    edesc: lang === 'FR' ? 'Dentifrice - paquet de 12' : 'Toothpaste - 12 Pack',
    edept: 'HLTH', euom: 'EA', eimg: 0, ecnt: 0, eflag: 'Y', ecase: 24, eqty: 0 }
];

/* One matching line, one over, one short needing a photo it does not have. */
const VARS = (lang) => [
  { _rrn: 1, vseq: 1, vline: 1, vsku: 'GROC-000004',
    vdesc: lang === 'FR' ? 'Riz blanc à grain long - 1 kg' : 'Long Grain White Rice - 1 kg',
    vuom: 'EA', vimg: 0, vexp: 273, vcnt: 261, vvar: -12,
    vreason: 'CVAR', vphoto: 0, vneed: 'Y', vflag: 'Y' },
  { _rrn: 2, vseq: 2, vline: 2, vsku: 'SEAS-000512',
    vdesc: lang === 'FR'
      ? 'Ensemble de meubles de patio quatre pièces avec coussins'
      : 'Four Piece Patio Furniture Set with Weather Resistant Cushions',
    vuom: 'EA', vimg: 0, vexp: 300, vcnt: 312, vvar: 12,
    vreason: 'CEXT', vphoto: 1, vneed: 'Y', vflag: 'Y' },
  { _rrn: 3, vseq: 3, vline: 3, vsku: 'HLTH-000141',
    vdesc: lang === 'FR' ? 'Dentifrice - paquet de 12' : 'Toothpaste - 12 Pack',
    vuom: 'EA', vimg: 0, vexp: 1073, vcnt: 1073, vvar: 0,
    vreason: '', vphoto: 0, vneed: 'N', vflag: 'Y' }
];

const REASONS_JSON = JSON.stringify([
  { c: 'CVAR', d: 'Count variance', p: 'Y' },
  { c: 'CMIS', d: 'Stock not found', p: 'Y' },
  { c: 'CEXT', d: 'Extra stock found', p: 'Y' },
  { c: 'CREC', d: 'Recount required', p: 'N' },
  { c: 'COK',  d: 'Count matches', p: 'N' }
]);

const SCREENS = [
  {
    name: 'cnthome',
    tpl: ['gtcnhd', 'cnthome.ejs'],
    css: ['gtcommon/gt-theme.css', 'gtcnhd/cnthome.css'],
    declared: ['action', 'scanval', 'langpref', 'opername', 'msg', 'msgkind',
               'ncounts', 'cntsfl'],
    controls: ['#gt-scan-start', '#gt-scan-manual'],
    minTarget: ['#gt-scan-manual'],
    sticky: { band: '.gt-inv-top', keep: ['#gt-scan-start', '#gt-scan-manual'],
              under: '.gt-cnt-card' },
    data: (lang) => Object.assign(COMMON(lang), { ncounts: 4, cntsfl: COUNTS(lang) })
  },
  {
    name: 'cntentry',
    tpl: ['gtcned', 'cntentry.ejs'],
    css: ['gtcommon/gt-theme.css', 'gtcned/cntentry.css'],
    declared: ['action', 'scanval', 'langpref', 'opername', 'msg', 'msgkind',
               'hcnt', 'hloc', 'hzone', 'htype', 'hstat', 'nlines', 'ndone',
               'hitline', 'cntedits', 'cesfl'],
    controls: ['#gt-scan-start', '#gt-scan-manual', '.gt-ce-box', '.gt-ce-zero'],
    minTarget: ['.gt-ce-box', '.gt-ce-zero'],
    data: (lang) => Object.assign(COMMON(lang), {
      hcnt: 'CN00000001', hloc: 'D01011', hzone: 'D', htype: 'BULK', hstat: 'CNTD',
      nlines: 26, ndone: 2, hitline: 2, cntedits: '', cesfl: ENTRY(lang) })
  },
  {
    name: 'cntvar',
    tpl: ['gtcnvd', 'cntvar.ejs'],
    css: ['gtcommon/gt-theme.css', 'gtcnvd/cntvar.css'],
    declared: ['action', 'scanval', 'langpref', 'opername', 'msg', 'msgkind',
               'hcnt', 'hloc', 'hzone', 'htype', 'hstat', 'nlines', 'ndone',
               'nvar', 'nphoto', 'nblock', 'vedits', 'imgline', 'imgdata',
               'posted', 'reasons', 'vasfl'],
    controls: ['.gt-cv-select', '.gt-cv-cam', '.gt-btn-go'],
    minTarget: ['.gt-cv-select', '.gt-cv-cam', '.gt-btn-go'],
    data: (lang) => Object.assign(COMMON(lang), {
      hcnt: 'CN00000001', hloc: 'D01011', hzone: 'D', htype: 'BULK', hstat: 'CNTD',
      nlines: 3, ndone: 3, nvar: 2, nphoto: 1, nblock: 1,
      vedits: '', imgline: 0, imgdata: '', posted: 'N',
      reasons: REASONS_JSON, vasfl: VARS(lang) })
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
