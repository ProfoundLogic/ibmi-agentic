/* ==================================================================
 * Offline pre-flight for the two Receiving screens.
 *
 * Renders the real templates with the real CSS and the display files' declared
 * field lists, under Genie's hostile globals, at 360/412/1280 in both
 * languages. Audits every <%= identifier %> against the declared fields --
 * one undefined reference aborts the whole render and gives a blank white
 * screen with no error anywhere.
 *
 * Also hit-tests the controls that must survive the narrowest phone.
 *
 * Run: node gtwms/tools/preflight-rcv.js
 * ================================================================== */

const ejs = require('ejs');
const fs = require('fs');
const path = require('path');
const { chromium } = require('playwright-core');

const UI = '/workspace/workspace/ibmi-agentic/docs/htdocs/profoundui/userdata/ui';
const CH = '/home/coder/.cache/ms-playwright/chromium-1228/chrome-linux64/chrome';
const SHOT = '/tmp/ejspre';

const SCREENS = [
  {
    name: 'rcvhome',
    tpl: ['gtrchd', 'rcvhome.ejs'],
    css: ['gtcommon/gt-theme.css', 'gtrchd/rcvhome.css'],
    declared: ['action', 'scanval', 'langpref', 'opername', 'msg', 'nopen', 'rcpsfl'],
    controls: ['#gt-scan-start', '#gt-scan-manual'],
    data: (lang) => ({
      action: '', scanval: '', langpref: lang,
      opername: lang === 'FR' ? 'Marie Tremblay' : 'James Okonkwo',
      msg: '', nopen: 3,
      rcpsfl: [
        { _rrn: 1, rseq: 1, rrcpt: 'RC00000001', rpo: 'PO-2026-0481',
          rvend: 'Maple Leaf Distribution', rdoor: 'D04', rstat: 'PART',
          rsscc: '006141410000000185', rlines: 34, rdone: 12, rvar: 2,
          rqtye: 1840, rqtyr: 690 },
        { _rrn: 2, rseq: 2, rrcpt: 'RC00000002', rpo: 'PO-2026-0482',
          rvend: 'Groupe Alimentaire Québec', rdoor: 'D01', rstat: 'OPEN',
          rsscc: '006141410000000192', rlines: 18, rdone: 0, rvar: 0,
          rqtye: 720, rqtyr: 0 },
        /* Long vendor name and no PO: the two things that break a card. */
        { _rrn: 3, rseq: 3, rrcpt: 'RC00000003', rpo: '',
          rvend: 'Northern Ontario Wholesale Grocers and Distribution Partners',
          rdoor: '', rstat: 'OPEN', rsscc: '', rlines: 6, rdone: 0, rvar: 0,
          rqtye: 96, rqtyr: 0 }
      ]
    })
  },
  {
    name: 'rcvlines',
    tpl: ['gtrcld', 'rcvlines.ejs'],
    css: ['gtcommon/gt-theme.css', 'gtrcld/rcvlines.css'],
    declared: ['action', 'scanval', 'langpref', 'opername', 'msg', 'msgkind',
               'hrcpt', 'hpo', 'hvend', 'hdoor', 'hstat', 'hsscc',
               'nlines', 'ndone', 'nvar', 'nphoto', 'hitline', 'qtyedits', 'linsfl'],
    /* The stepper is the reason this list exists: the native number-input
       spinners it replaced were present and visible and still unusable, so
       these are checked for a real 44px+ touch target as well as being
       hit-testable. */
    controls: ['#gt-scan-start', '#gt-scan-manual', '.gt-lin-qty', '.gt-btn-post',
               '.gt-step-down', '.gt-step-up', '.gt-lin-full'],
    minTarget: ['.gt-step-down', '.gt-step-up', '.gt-lin-qty', '.gt-lin-full'],
    data: (lang) => ({
      action: '', scanval: '', langpref: lang,
      opername: lang === 'FR' ? 'Marie Tremblay' : 'James Okonkwo',
      msg: lang === 'FR' ? 'HLTH-000141 : 24 sur 24 reçus.'
                         : 'HLTH-000141: 24 of 24 received.',
      msgkind: 'GOOD',
      hrcpt: 'RC00000001', hpo: 'PO-2026-0481',
      hvend: 'Maple Leaf Distribution', hdoor: 'D04', hstat: 'PART',
      hsscc: '006141410000000185',
      nlines: 5, ndone: 3, nvar: 2, nphoto: 2,
      /* A scan just landed on line 2 -- exercises the flash and the scroll. */
      hitline: 2,
      linsfl: [
        { _rrn: 1, lseq: 1, lline: 1, lsku: 'HLTH-000141',
          ldesc: lang === 'FR' ? 'Dentifrice - paquet de 12' : 'Toothpaste - 12 Pack',
          ldept: 'HLTH', luom: 'EA', limg: 149,
          lexp: 24, lrcv: 24, lvar: 0, lstat: 'MTCH', lreason: '', lphoto: 0, lcase: 24 },
        { _rrn: 2, lseq: 2, lline: 2, lsku: 'GROC-000004',
          ldesc: lang === 'FR' ? 'Riz blanc à grain long - 1 kg' : 'Long Grain White Rice - 1 kg',
          ldept: 'GROC', luom: 'EA', limg: 103,
          lexp: 48, lrcv: 36, lvar: -12, lstat: 'SHRT', lreason: 'SHRT', lphoto: 1, lcase: 12 },
        { _rrn: 3, lseq: 3, lline: 3, lsku: 'CLEA-000210',
          ldesc: lang === 'FR' ? 'Détergent à lessive - 2,5 L' : 'Laundry Detergent - 2.5 L',
          ldept: 'CLEA', luom: 'EA', limg: 0,
          lexp: 12, lrcv: 14, lvar: 2, lstat: 'OVER', lreason: '', lphoto: 0, lcase: 6 },
        { _rrn: 4, lseq: 4, lline: 4, lsku: 'HOME-000077',
          ldesc: lang === 'FR' ? 'Ensemble de draps - très grand lit' : 'Bed Sheet Set - King',
          ldept: 'HOME', luom: 'EA', limg: 0,
          lexp: 6, lrcv: 0, lvar: 0, lstat: 'OPEN', lreason: '', lphoto: 0, lcase: 1 },
        /* Damaged, photographed, and a description long enough to wrap. */
        { _rrn: 5, lseq: 5, lline: 5, lsku: 'SEAS-000512',
          ldesc: lang === 'FR'
            ? 'Ensemble de meubles de patio quatre pièces avec coussins'
            : 'Four Piece Patio Furniture Set with Weather Resistant Cushions',
          ldept: 'SEAS', luom: 'EA', limg: 0,
          lexp: 4, lrcv: 3, lvar: -1, lstat: 'DAMG', lreason: 'DAMG', lphoto: 1, lcase: 1 }
      ]
    })
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
