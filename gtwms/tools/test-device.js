#!/usr/bin/env node
/*
 * GT Warehouse Mobile -- does a real phone lay these screens out at phone size?
 *
 *   node gtwms/tools/test-device.js
 *
 * WHY THIS EXISTS
 * ---------------
 * The application looked wrong on the operator's Samsung S24 Ultra: correct, but
 * shrunken, and using a desktop layout on a phone. Nothing was wrong with the
 * CSS. Genie's start.html declares NO <meta name="viewport">, and a mobile
 * browser with no viewport meta lays the page out in a ~980px LAYOUT VIEWPORT
 * and then zooms the finished page out to fit the screen. Measured:
 *
 *     no meta    layout 980px  ->  the >=835px desktop layout at ~42% scale
 *     with meta  layout 412px  ->  the phone layout, full size
 *
 * So every media query in the application was reading a desktop width. The
 * screens were never told the device existed.
 *
 * WHY NO EXISTING TEST CAUGHT IT. Every pre-flight injected its own
 * `<meta name="viewport" content="width=412">` before measuring -- it told the
 * browser the answer. A harness that supplies what the real page is missing
 * cannot find out that the real page is missing it. This is the third time in
 * this project that a green suite has been green about the wrong document.
 *
 * WHAT THIS CHECKS
 *   1  the deployed shim, in EVERY skin, actually carries the viewport logic --
 *      the fix has to be on the server, not just in the repo;
 *   2  with the shim's meta, the layout viewport IS the device width and the
 *      PHONE branch of the CSS is the one in effect;
 *   3  THE NEGATIVE CASE: with no meta the layout is ~980px and the desktop
 *      branch takes over. Without this the test proves nothing -- it would pass
 *      just as happily on a browser that ignored viewports entirely;
 *   4  type is still readable at phone size, since "responsive" that produces
 *      9px text is not responsive.
 */

'use strict';

const ejs = require('ejs');
const fs = require('fs');
const path = require('path');
const { execFileSync } = require('child_process');
const { chromium } = require('playwright-core');

const UI = path.resolve(__dirname, '../../htdocs/profoundui/userdata/ui');
const CH = '/home/coder/.cache/ms-playwright/chromium-1228/chrome-linux64/chrome';
const SKINS_DIR = '/home/drusso/puidist/htdocs/profoundui/userdata/genie skins';
const GENIE = 'html,body{margin:0;padding:0}' +
              'div{white-space:nowrap;z-index:10;padding:1px}';

/* The S24 Ultra as the browser reports it. */
const DEVICE = { width: 412, height: 915, dsf: 3.5 };

let pass = 0, fail = 0;
const ok  = (m, d) => { pass++; console.log('  PASS  ' + m + (d ? '  -- ' + d : '')); };
const bad = (m, d) => { fail++; console.log('  FAIL  ' + m + (d ? '  -- ' + d : '')); };

/* ------------------------------------------------------------------
   1. Is the fix actually deployed, in every skin?
   ------------------------------------------------------------------ */
function checkDeployedShim(skins) {
  for (const skin of skins) {
    let html;
    try {
      html = execFileSync('ssh', ['dev', `cat "${SKINS_DIR}/${skin}/start.html"`],
                          { maxBuffer: 64 * 1024 * 1024 }).toString();
    } catch (e) {
      bad(skin + ': start.html unreadable', e.message.slice(0, 80));
      continue;
    }

    if (html.indexOf('width=device-width') < 0) {
      bad(skin + ': the deployed shim has no viewport logic',
          'screens will lay out at 980px and be scaled down');
    } else if (html.indexOf('syncViewport') < 0) {
      bad(skin + ': viewport string present but not the sync logic',
          'it would never be applied or restored');
    } else {
      ok(skin + ': deployed shim carries the viewport logic');
    }
  }
}

/* ------------------------------------------------------------------
   Metric tokens used where a colour belongs.

   --gt-tap is 48px and --gt-action is 64px. `background: var(--gt-action)` is
   invalid CSS: the browser drops the declaration silently, and paired with
   `color: #fff` it produced a chip that was white text on a white card --
   present in the DOM, hit-testable, screenshot-perfect, invisible. Six of them
   had shipped across three screens.

   A static grep, because no rendering check can catch this reliably: the
   element is there and correctly sized, it just has no colour.
   ------------------------------------------------------------------ */
const METRICS = ['--gt-tap', '--gt-action', '--gt-radius', '--gt-gap',
                 '--gt-pad', '--gt-logo-guard'];
const COLOUR_PROPS = /(^|[^-\w])(color|background|background-color|border|border-color|border-left|border-right|border-top|border-bottom|outline|fill|stroke)\s*:/;

function checkColourTokens() {
  const dirs = fs.readdirSync(UI).filter((d) => d.startsWith('gt'));
  const hits = [];
  for (const dir of dirs) {
    const full = path.join(UI, dir);
    if (!fs.statSync(full).isDirectory()) continue;
    for (const f of fs.readdirSync(full).filter((x) => x.endsWith('.css'))) {
      const text = fs.readFileSync(path.join(full, f), 'utf8');
      text.split('\n').forEach((line, i) => {
        const code = line.split('/*')[0];
        if (!COLOUR_PROPS.test(code)) return;
        for (const m of METRICS) {
          if (code.indexOf('var(' + m + ')') >= 0) {
            hits.push(dir + '/' + f + ':' + (i + 1) + ' uses ' + m + ' as a colour');
          }
        }
      });
    }
  }
  if (hits.length) hits.slice(0, 6).forEach((h) => bad('metric token as a colour', h));
  else ok('no metric token is used where a colour belongs',
          METRICS.length + ' tokens checked across every gt* stylesheet');
}

/* ------------------------------------------------------------------
   Two representative screens: the densest dashboard, and a subfile screen
   with a pinned band. Data kept minimal on purpose -- this test is about the
   viewport, and the pre-flights already cover content at width.
   ------------------------------------------------------------------ */
const SCREENS = [
  {
    name: 'supv',
    tpl: 'gtsvd/supv.ejs',
    css: ['gtcommon/gt-theme.css', 'gtsvd/supv.css'],
    /* .gt-sv-stats is 2 columns on a phone and 3+ on a desk, so its computed
     * column count is a direct read-out of which branch is in effect. */
    probe: '.gt-sv-stats',
    phoneCols: 2,
    text: '.gt-sv-stat small',
    data: {
      action: '', langpref: 'EN', opername: 'Marie Tremblay', operrole: 'SUPV',
      msg: '', msgkind: '',
      nrcpts: 7, nrlines: 165, nrdone: 20, nrvar: 3,
      ncounts: 4, nclines: 76, ncdone: 0, ncvar: 0,
      nalerts: 51, nstaged: 6024, nmoves: 32, nwork: 2,
      zonestat: '[{"z":"A","l":144,"s":144,"q":8772,"a":18},' +
                '{"z":"D","l":288,"s":278,"q":149245,"a":0}]',
      wrksfl: [
        { _rrn: 1, wseq: 1, wtype: 'RECV', wdoc: 'RC00000001',
          wwhere: 'Door D001 - Northbridge Wholesale Ltd.', wwho: 'PO0000031884',
          wstat: 'OPEN', wlines: 60, wdone: 0, wvar: 0, wphoto: 0 },
        { _rrn: 2, wseq: 2, wtype: 'CNT', wdoc: 'CN00000001', wwhere: 'D01011',
          wwho: 'Marie Tremblay', wstat: 'CNTD', wlines: 26, wdone: 26,
          wvar: 3, wphoto: 0 }
      ]
    }
  },
  {
    name: 'puthome',
    tpl: 'gtpud/puthome.ejs',
    css: ['gtcommon/gt-theme.css', 'gtpud/puthome.css'],
    /* The putaway card stacks its route onto a second row on a phone and puts
     * it inline at >=835px, so the grid-template-areas string identifies the
     * branch unambiguously. */
    probe: '.gt-put-hit',
    phoneAreas: 'route',
    text: '.gt-put-sku',
    data: {
      action: '', scanval: '', langpref: 'EN', opername: 'Marie Tremblay',
      msg: '', msgkind: '', nrows: 2, nunits: 1240, hitline: 0,
      stgsfl: [1, 2].map((n) => ({
        _rrn: n, sseq: n, ssku: 'GROC-00001' + n,
        sdesc: 'Long Grain White Rice - 2 kg resealable bag',
        sdept: 'GROC', suom: 'EA', simg: 0, sqty: 120 * n, scase: 12,
        sloc: 'STAGE01', ssugg: 'D0304' + n, ssugtyp: 'BULK',
        ssugrm: 1227, ssugrnk: 2, ssscc: '306141410000000044'
      }))
    }
  }
];

const SHIM_VIEWPORT = 'width=device-width, initial-scale=1, viewport-fit=cover';

async function measure(browser, sc, withMeta) {
  const src = fs.readFileSync(path.join(UI, sc.tpl), 'utf8');
  const css = sc.css.map((f) => fs.readFileSync(path.join(UI, f), 'utf8')).join('\n');
  const body = ejs.render(src, sc.data);

  const ctx = await browser.newContext({
    viewport: { width: DEVICE.width, height: DEVICE.height },
    deviceScaleFactor: DEVICE.dsf,
    isMobile: true,
    hasTouch: true
  });
  const page = await ctx.newPage();
  await page.setContent(
    '<!doctype html><html><head><meta charset="utf-8">' +
    '<meta http-equiv="X-UA-Compatible" content="IE=edge">' +
    (withMeta ? '<meta name="viewport" content="' + SHIM_VIEWPORT + '">' : '') +
    '<style>' + GENIE + '</style><style>' + css + '</style></head><body>' +
    body + '</body></html>', { waitUntil: 'load' });

  const out = await page.evaluate((cfg) => {
    const probe = document.querySelector(cfg.probe);
    const text = document.querySelector(cfg.text);
    const cs = probe ? getComputedStyle(probe) : null;
    return {
      layout: document.documentElement.clientWidth,
      scrollW: document.documentElement.scrollWidth,
      cols: cs ? cs.gridTemplateColumns.split(/\s+/).filter(Boolean).length : 0,
      areas: cs ? cs.gridTemplateAreas : '',
      fontPx: text ? parseFloat(getComputedStyle(text).fontSize) : 0
    };
  }, { probe: sc.probe, text: sc.text });

  await page.screenshot({
    path: '/tmp/ejspre/device-' + sc.name + (withMeta ? '-fixed' : '-broken') + '.png',
    fullPage: false
  });
  await ctx.close();
  return out;
}

(async () => {
  fs.mkdirSync('/tmp/ejspre', { recursive: true });
  const skins = (process.argv[2] || 'pls,Classic').split(',').filter(Boolean);

  console.log('=== 1. is the viewport fix actually deployed? ===');
  checkDeployedShim(skins);

  console.log('\n=== 1b. colour tokens where colours belong ===');
  checkColourTokens();

  const browser = await chromium.launch({ executablePath: CH, args: ['--no-sandbox'] });

  console.log('\n=== 2. with the shim: phone layout at phone size ===');
  for (const sc of SCREENS) {
    const r = await measure(browser, sc, true);

    if (r.layout !== DEVICE.width) {
      bad(sc.name + ': layout viewport', r.layout + 'px, expected ' + DEVICE.width);
    } else {
      ok(sc.name + ': layout viewport is the device width', r.layout + 'px');
    }

    if (sc.phoneCols !== undefined) {
      if (r.cols === sc.phoneCols) {
        ok(sc.name + ': phone breakpoint in effect', sc.probe + ' has ' + r.cols + ' columns');
      } else {
        bad(sc.name + ': wrong breakpoint', sc.probe + ' has ' + r.cols +
            ' columns, phone layout is ' + sc.phoneCols);
      }
    }
    if (sc.phoneAreas !== undefined) {
      /* The phone layout stacks the route onto its own grid row; the desk
       * layout puts it inline. Counting rows in grid-template-areas tells them
       * apart without depending on the exact string. */
      const rows = (r.areas.match(/"/g) || []).length / 2;
      if (rows >= 2) {
        ok(sc.name + ': phone breakpoint in effect', 'card stacks into ' + rows + ' rows');
      } else {
        bad(sc.name + ': wrong breakpoint', 'card is one row -- that is the desk layout');
      }
    }

    if (r.scrollW > r.layout + 1) {
      bad(sc.name + ': overflows the phone', r.scrollW + ' > ' + r.layout);
    } else {
      ok(sc.name + ': no horizontal overflow at phone width');
    }

    /* Responsive that yields unreadable type is not responsive. */
    if (r.fontPx >= 10) {
      ok(sc.name + ': supporting text is readable', r.fontPx + 'px CSS');
    } else {
      bad(sc.name + ': supporting text too small', r.fontPx + 'px CSS');
    }
  }

  console.log('\n=== 3. the negative case: no meta must NOT look like a phone ===');
  console.log('    (if these pass, every check above is meaningless)');
  for (const sc of SCREENS) {
    const r = await measure(browser, sc, false);
    if (r.layout > DEVICE.width + 100) {
      ok(sc.name + ': without the meta the layout really is desktop-wide',
         r.layout + 'px -- so the check above is measuring something real');
    } else {
      bad(sc.name + ': without the meta the layout was still ' + r.layout + 'px',
          'this browser is not honouring viewports, so nothing here is proven');
    }
  }

  await browser.close();
  console.log('\n' + (fail ? 'FAILED' : 'All checks passed') +
              ': ' + pass + ' passed, ' + fail + ' failed');
  console.log('screenshots: /tmp/ejspre/device-*-{fixed,broken}.png');
  process.exit(fail ? 1 : 0);
})();
