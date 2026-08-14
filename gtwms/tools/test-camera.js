#!/usr/bin/env node
/*
 * GT Warehouse Mobile -- does the camera button work on EVERY screen that has one?
 *
 *   node gtwms/tools/test-camera.js
 *
 * WHY THIS EXISTS
 * ---------------
 * The camera button was dead on NINE of the eleven screens that have one.
 * `gt-scan.js` bound relative to `.gt-scan-screen`, a class that only Scan Lab
 * and Item Lookup's scan screen were ever given, so on Receiving, Inventory,
 * Cycle Count and Putaway `bind()` returned false and nothing was wired.
 *
 * IT HID BECAUSE THE FAILURE IS PARTIAL AND SILENT. The keyed box beside the
 * camera works with no JavaScript whatsoever -- its Go button is an inline
 * `pui.submit` -- so every one of those screens still scanned perfectly well
 * as long as you typed. Every test in this repo typed:
 *
 *   - the pre-flights assert the button is PRESENT, sized and hit-testable,
 *     which it was;
 *   - the shim test asserts the JS is DELIVERED, which it was;
 *   - the live session verification fed SCANVAL straight into the field, which
 *     is the path the keyed box uses, not the path the camera uses.
 *
 * Present, delivered, and never once clicked. So this test clicks it.
 *
 * WHAT IT DOES
 *   For every template containing #gt-scan-start -- discovered by scanning the
 *   directory, never a hardcoded list -- it renders the screen with the real
 *   gt-scan.js, stubs getUserMedia, CLICKS the button, and asserts the camera
 *   was actually requested. Then it re-renders a second screen in the same page
 *   to prove rebinding works, because Genie is a single page and the real
 *   failure mode is the SECOND screen you visit.
 */

'use strict';

const ejs = require('ejs');
const fs = require('fs');
const path = require('path');
const http = require('http');
const { chromium } = require('playwright-core');

const { execFileSync } = require('child_process');

const UI = path.resolve(__dirname, '../../htdocs/profoundui/userdata/ui');
const SKINS_DIR = '/home/drusso/puidist/htdocs/profoundui/userdata/genie skins';
const CH = '/home/coder/.cache/ms-playwright/chromium-1228/chrome-linux64/chrome';
const GENIE = 'html,body{margin:0;padding:0}div{white-space:nowrap;z-index:10;padding:1px}';

let pass = 0, fail = 0;
const ok = (m, d) => { pass++; console.log('  PASS  ' + m + (d ? '  -- ' + d : '')); };
const bad = (m, d) => { fail++; console.log('  FAIL  ' + m + (d ? '  -- ' + d : '')); };

/* Minimal data per screen: enough for the template to render without throwing.
 * The scan band does not depend on the business data, so this stays small on
 * purpose -- the pre-flights already cover content. */
const COMMON = {
  action: '', scanval: '', langpref: 'EN', opername: 'Marie Tremblay',
  msg: '', msgkind: '', sflclear: '0', hitline: 0
};

const DATA = {
  'gtscnd/scnhome.ejs': { nscans: 0, hasres: 'N', rkind: '', rsymb: '', rraw: '',
    rrawlen: 0, risgs1: 'N', rcheck: 'N', rsscc: '', rgtin: '', rlot: '',
    rexpiry: '', rserial: '', rqty: 0, rsku: '', rdesc: '', rloc: '', rrcpt: '',
    roper: '', ropername: '', rpackqty: 0, rlevel: '', hist: [] },
  'gtitmd/itmscan.ejs': { search: '', nfound: 0, srchsfl: [] },
  'gtrchd/rcvhome.ejs': { nopen: 7, rcpsfl: [] },
  'gtrcld/rcvlines.ejs': { hrcpt: 'RC00000007', hpo: 'PO1', hvend: 'V', hdoor: 'D007',
    hstat: 'OPEN', hsscc: '3061', nlines: 9, ndone: 0, nvar: 0, nphoto: 0,
    qtyedits: '', msgkind: '', linsfl: [] },
  'gtinhd/invhome.ejs': { nalerts: 51, alrsfl: [] },
  'gtinld/invloc.ejs': { hloc: 'D01012', hzone: 'D', htype: 'BULK', hactive: 'Y',
    haisle: '01', hbay: '02', hlevel: '1', hitems: 25, hqty: 12303, hcap: 1155,
    hpct: 90, itmsfl: [] },
  'gtimvd/invmove.ejs': { mfrom: 'D01012', mfromtyp: 'BULK', mfromzon: 'D',
    msku: 'GROC-000004', mdesc: 'Rice', muom: 'EA', mimg: 0, mcase: 12,
    monhand: 273, mavail: 273, mto: '', mtotyp: '', mtozon: '', mtoqty: 0,
    mtook: 'N', mqty: 12, mreason: 'REPL', mdone: 'N', mfrombef: 0, mfromaft: 0,
    mtobef: 0, mtoaft: 0, rsnsfl: [] },
  'gtcnhd/cnthome.ejs': { ncounts: 4, cntsfl: [] },
  'gtcned/cntentry.ejs': { hcnt: 'CN00000001', hloc: 'D01011', hzone: 'D',
    htype: 'BULK', hstat: 'OPEN', nlines: 26, ndone: 0, cntedits: '', cesfl: [] },
  'gtpud/puthome.ejs': { nrows: 2, nunits: 240, stgsfl: [] },
  'gtcnvd/cntvar.ejs': { hcnt: 'CN00000001', hloc: 'D01011', hzone: 'D',
    htype: 'BULK', hstat: 'CNTD', nlines: 2, ndone: 2, nvar: 1, nphoto: 0,
    nblock: 1, vedits: '', imgline: 0, imgdata: '', posted: 'N',
    reasons: '[{"c":"CVAR","d":"Count variance","p":"Y"},' +
             '{"c":"COK","d":"Count agrees","p":"N"}]',
    /* TWENTY-SIX rows, which is what a real bulk-bay count has. The page is
       then thousands of pixels tall -- the condition under which the photo
       overlay opened halfway down the document. A two-row fixture cannot see
       that bug, which is why the earlier photo test never did. */
    vasfl: Array.from({ length: 26 }, (_, i) => ({
      _rrn: i + 1, vseq: i + 1, vline: i + 1,
      vsku: 'CLEA-0000' + (66 + i),
      vdesc: 'Liquid Laundry Detergent - 500 mL',
      vuom: 'EA', vimg: 0, vexp: 833 - i * 10, vcnt: i === 0 ? 700 : 0,
      vvar: i === 0 ? -133 : -(833 - i * 10), vreason: '',
      vphoto: 0, vneed: 'N', vflag: i === 0 ? 'SHRT' : 'N'
    })) },
  'gtpdd/putdest.ejs': { psku: 'APPL-000237', pdesc: 'Hoodie', pdept: 'APPL',
    puom: 'EA', pcase: 12, pimg: 0, psscc: '3061', pfrom: 'STAGE01', pavail: 107,
    pto: 'D05102', ptotyp: 'BULK', ptozone: 'D', ptoroom: 214, ptoqty: 258,
    prank: 2, ptook: 'N', pqty: 107, pdone: 'N', pfrombef: 0, pfromaft: 0,
    ptobef: 0, ptoaft: 0, palts: '[]' }
};

/* CSS for a screen: the theme plus whatever sits beside the template. */
function cssFor(tpl) {
  const dir = path.dirname(tpl);
  const files = [path.join(UI, 'gtcommon/gt-theme.css')];
  for (const f of fs.readdirSync(path.join(UI, dir))) {
    if (f.endsWith('.css')) files.push(path.join(UI, dir, f));
  }
  return files.map((f) => fs.readFileSync(f, 'utf8')).join('\n');
}

/* Every template with a camera button, discovered rather than listed. A
 * hardcoded list is how a test comes to cover fewer screens than exist. */
function screensWithCamera() {
  const out = [];
  for (const dir of fs.readdirSync(UI).filter((d) => d.startsWith('gt'))) {
    const full = path.join(UI, dir);
    if (!fs.statSync(full).isDirectory()) continue;
    for (const f of fs.readdirSync(full).filter((x) => x.endsWith('.ejs'))) {
      const rel = dir + '/' + f;
      if (fs.readFileSync(path.join(UI, rel), 'utf8').indexOf('gt-scan-start') >= 0) {
        out.push(rel);
      }
    }
  }
  return out.sort();
}

function render(tpl) {
  const src = fs.readFileSync(path.join(UI, tpl), 'utf8');
  const data = Object.assign({}, COMMON, DATA[tpl] || {});
  return ejs.render(src, data);
}

/* The page harness.
 *
 * SERVED OVER http://127.0.0.1, NOT setContent. gt-scan.js checks for
 * `navigator.mediaDevices.getUserMedia` before doing anything, and on an
 * insecure origin that property does not exist -- so the button "did nothing"
 * for a reason that had nothing to do with the product. Stubbing it does not
 * help either: the assignment does not stick. A loopback origin is a secure
 * context, so the real API is present, and Chromium's fake-device flags make it
 * resolve without hardware.
 *
 * So getUserMedia is WRAPPED rather than replaced -- the real one still runs,
 * and the count proves the click reached it. */
const HARNESS = `
  window.__gum = 0;
  (function () {
    var md = navigator.mediaDevices;
    if (!md || !md.getUserMedia) { window.__noSecureContext = true; return; }
    var real = md.getUserMedia.bind(md);
    md.getUserMedia = function (c) { window.__gum++; return real(c); };
  })();
  /* Chromium on Linux has no BarcodeDetector. Stub it so gt-scan takes the same
     path it takes on the phone instead of warning and stopping. */
  if (typeof window.BarcodeDetector === 'undefined') {
    window.BarcodeDetector = function () {
      return { detect: function () { return Promise.resolve([]); } };
    };
    window.BarcodeDetector.getSupportedFormats =
      function () { return Promise.resolve(['code_128']); };
  }
  window.pui = { submit: function (o) { window.__submitted = o; } };
`;

/* ------------------------------------------------------------------
   Is the fix on the SERVER, not just in the repo?

   The shim carries gt-scan.js as a base64 snapshot inside each skin's
   start.html, so copying the file to the docroot changes nothing on its own --
   the browser reads the snapshot. Plain grep cannot see it either, being
   base64. So: decode every scan screen's snapshot and check the anchor.
   ------------------------------------------------------------------ */
function checkDeployedScanJs(skins) {
  for (const skin of skins) {
    let html;
    try {
      html = execFileSync('ssh', ['dev', `cat "${SKINS_DIR}/${skin}/start.html"`],
                          { maxBuffer: 128 * 1024 * 1024 }).toString();
    } catch (e) {
      bad(skin + ': start.html unreadable', String(e.message).slice(0, 70));
      continue;
    }
    const b = html.indexOf('GTWMS EJS shim BEGIN');
    const e2 = html.indexOf('GTWMS EJS shim END');
    if (b < 0 || e2 < 0) { bad(skin + ': no GTWMS shim block'); continue; }
    const blk = html.slice(b, e2);

    const re = /pattern:\s*"([^"]+)"[\s\S]*?jsB64:\s*"([A-Za-z0-9+/=]*)"/g;
    let m, checked = 0, wrong = [];
    while ((m = re.exec(blk)) !== null) {
      if (!m[2]) continue;
      const js = Buffer.from(m[2], 'base64').toString('utf8');
      if (js.indexOf('gt-scan-start') < 0) continue;     /* not a scan screen */
      checked++;
      const fixed = js.indexOf("$('#gt-scan-camera')") >= 0;
      const old = js.indexOf("$('.gt-scan-screen')") >= 0;
      if (!fixed || old) wrong.push(m[1]);
    }
    if (!checked) bad(skin + ': no scan screens found in the snapshot');
    else if (wrong.length) bad(skin + ': stale gt-scan.js snapshot', wrong.join(', '));
    else ok(skin + ': all ' + checked + ' deployed scan screens are band-anchored');
  }
}

/* ------------------------------------------------------------------
   Is the Safari decoder reachable from the REAL server?

   It is fetched by a <script src> at runtime, so shipping gt-scan.js without
   the decoder beside it leaves the iPad exactly where it started -- and the
   shim test cannot see this, because the decoder is not part of any snapshot.
   ------------------------------------------------------------------ */
function checkDeployedDecoder() {
  const url = 'https://idev.profoundlogic.com:8103' +
              '/profoundui/userdata/ui/gtcommon/gt-zxing.min.js';
  let out;
  try {
    out = execFileSync('curl',
      ['-s', '-o', '/tmp/.gtzx.js', '-w', '%{http_code} %{size_download}', '-k', url],
      { timeout: 30000 }).toString().trim().split(/\s+/);
  } catch (e) {
    bad('the Safari decoder is served', 'curl failed'); return;
  }
  const code = Number(out[0]), size = Number(out[1]);
  if (code !== 200) bad('the Safari decoder is served', 'HTTP ' + code);
  else if (size < 100000) bad('the Safari decoder is served', 'only ' + size + ' bytes');
  else ok('the Safari decoder is served', size + ' bytes over HTTPS');
}

function page(bodyHtml, css) {
  return '<!doctype html><html><head><meta charset="utf-8">' +
    '<meta name="viewport" content="width=device-width, initial-scale=1">' +
    '<style>' + GENIE + '</style><style>' + css + '</style></head><body>' +
    bodyHtml + '</body></html>';
}

(async () => {
  const list = screensWithCamera();
  const scanJs = fs.readFileSync(path.join(UI, 'gtcommon/gt-scan.js'), 'utf8');

  /* One server, one route per screen. */
  const routes = {};
  const server = http.createServer((req, res) => {
    const url = req.url.split('?')[0];

    /* The decoder is loaded lazily by a <script src> to the real userdata path,
       so that path has to resolve here too or the Safari test would be testing
       the 404 handler. */
    if (url.indexOf('/profoundui/userdata/ui/') === 0) {
      const rel = url.replace('/profoundui/userdata/ui/', '');
      const file = path.join(UI, rel);
      if (fs.existsSync(file)) {
        res.writeHead(200, { 'content-type': 'application/javascript' });
        return res.end(fs.readFileSync(file));
      }
      res.writeHead(404); return res.end();
    }

    const body = routes[url];
    if (body === undefined) { res.writeHead(404); return res.end(); }
    res.writeHead(200, { 'content-type': 'text/html; charset=utf-8' });
    res.end(body);
  });
  const port = await new Promise((resolve) =>
    server.listen(0, '127.0.0.1', () => resolve(server.address().port)));
  const base = 'http://127.0.0.1:' + port;

  console.log('=== 0. is the fix deployed, in every skin? ===');
  checkDeployedScanJs((process.argv[2] || 'pls,Classic').split(',').filter(Boolean));

  console.log('\n=== 0b. the Safari decoder, on the server ===');
  checkDeployedDecoder();

  console.log('\n=== the camera button, on every screen that has one ===');
  console.log('    ' + list.length + ' screens found by scanning the templates\n');

  const browser = await chromium.launch({
    executablePath: CH,
    args: ['--no-sandbox',
           '--use-fake-device-for-media-stream',   /* synthetic camera */
           '--use-fake-ui-for-media-stream']       /* auto-grant, no prompt */
  });

  for (const tpl of list) {
    let html;
    try { html = render(tpl); }
    catch (e) { bad(tpl + ' render', e.message.slice(0, 70)); continue; }
    routes['/' + tpl] = page(html, cssFor(tpl));

    const pg = await browser.newPage({ viewport: { width: 412, height: 915 },
                                       isMobile: true, hasTouch: true });
    await pg.addInitScript(HARNESS);
    await pg.goto(base + '/' + tpl, { waitUntil: 'load' });
    await pg.addScriptTag({ content: scanJs });
    await pg.waitForTimeout(150);           /* gt-scan polls for its root */

    /* Did it actually bind? The band carries the marker. */
    const bound = await pg.evaluate(() =>
      document.querySelector('#gt-scan-camera') &&
      document.querySelector('#gt-scan-camera').getAttribute('data-gt-scan-bound') === '1');

    /* The real question: does clicking the button ask for the camera? */
    await pg.click('#gt-scan-start').catch(() => {});
    await pg.waitForTimeout(400);
    const gum = await pg.evaluate(() => window.__gum || 0);
    const insecure = await pg.evaluate(() => !!window.__noSecureContext);
    const status = await pg.evaluate(() => {
      const el = document.querySelector('#gt-scan-status');
      return el ? el.textContent.trim() : '';
    });

    if (insecure) {
      bad(tpl.padEnd(22) + ' harness', 'no getUserMedia -- not a secure context');
    } else if (!bound) {
      bad(tpl.padEnd(22) + ' camera button', 'gt-scan.js never bound to this screen');
    } else if (gum < 1) {
      bad(tpl.padEnd(22) + ' camera button',
          'bound, but the click did not request the camera' +
          (status ? ' (status: "' + status + '")' : ''));
    } else {
      ok(tpl.padEnd(22) + ' camera button', 'click -> getUserMedia');
    }

    await pg.close();
  }

  /* ---- The second-screen case ---------------------------------------
     Genie is a single page: the DOM is replaced per screen while the window
     lives on. gt-scan.js guards re-entry, so the interesting failure is the
     SECOND screen -- exactly the one the operator reported. Prove a rebind. */
  console.log('\n=== rebinding: a second screen in the same page ===');
  if (list.length >= 2) {
    const first = 'gtscnd/scnhome.ejs';
    const second = list.find((t) => t !== first && t.indexOf('rcvhome') >= 0) ||
                   list.find((t) => t !== first);
    routes['/pair'] = page('<div id="host">' + render(first) + '</div>',
                           cssFor(first) + '\n' + cssFor(second));
    const pg = await browser.newPage({ viewport: { width: 412, height: 915 },
                                       isMobile: true, hasTouch: true });
    await pg.addInitScript(HARNESS);
    await pg.goto(base + '/pair', { waitUntil: 'load' });
    await pg.addScriptTag({ content: scanJs });
    await pg.waitForTimeout(150);
    await pg.click('#gt-scan-start').catch(() => {});
    await pg.waitForTimeout(400);
    const firstGum = await pg.evaluate(() => window.__gum || 0);

    /* Swap the DOM the way Genie does, then re-run the snapshot JS the way the
       shim does. */
    await pg.evaluate((h) => { document.getElementById('host').innerHTML = h; },
                      render(second));
    await pg.addScriptTag({ content: scanJs });
    await pg.waitForTimeout(150);
    await pg.click('#gt-scan-start').catch(() => {});
    await pg.waitForTimeout(400);
    const secondGum = await pg.evaluate(() => window.__gum || 0);

    if (firstGum >= 1 && secondGum > firstGum) {
      ok(first + ' then ' + second, 'both screens requested the camera');
    } else {
      bad(first + ' then ' + second,
          'first=' + firstGum + ' second=' + secondGum +
          ' -- the second screen did not rebind');
    }
    await pg.close();
  }

  /* ---- The PHOTO camera ---------------------------------------------
     A second kind of camera button, on a different code path: an inline
     onclick reaching gtPhoto rather than a bound listener.

     Item Detail is already covered end to end by test-photo-capture.js. The gap
     was the Cycle Count variance photo -- the one the reason-code table can make
     MANDATORY before a count will post -- which had never been clicked by any
     test. Its trigger goes through gt-cntvar.js first, so this exercises the
     whole chain: row button -> gtCntVar.photo() -> gtPhoto.open() -> camera. */
  console.log('\n=== the photo camera, on the screens that capture evidence ===');
  {
    const photoScreens = [
      { tpl: 'gtcnvd/cntvar.ejs',
        js: ['gtcommon/gt-photo.js', 'gtcommon/gt-edits.js', 'gtcommon/gt-cntvar.js'],
        click: '[onclick*="gtCntVar.photo"]',
        note: 'variance evidence photo' }
    ];

    for (const ps of photoScreens) {
      let html;
      try { html = render(ps.tpl); }
      catch (e) { bad(ps.tpl + ' render', e.message.slice(0, 70)); continue; }
      routes['/photo/' + ps.tpl] = page(html, cssFor(ps.tpl));

      const pg = await browser.newPage({ viewport: { width: 412, height: 915 },
                                         isMobile: true, hasTouch: true });
      await pg.addInitScript(HARNESS);
      await pg.goto(base + '/photo/' + ps.tpl, { waitUntil: 'load' });
      for (const j of ps.js) {
        await pg.addScriptTag({ content: fs.readFileSync(path.join(UI, j), 'utf8') });
      }
      await pg.waitForTimeout(150);

      const present = await pg.$(ps.click);
      if (!present) {
        bad(ps.tpl.padEnd(22) + ' photo button', 'no element matching ' + ps.click);
        await pg.close();
        continue;
      }

      /* SCROLL TO THE BOTTOM FIRST. The overlay is position:absolute, so on a
         tall page its containing block is the whole document -- opening it from
         the top of the page hides the bug completely. The operator taps a photo
         button on a row they have scrolled down to. */
      await pg.evaluate(() => window.scrollTo(0, document.body.scrollHeight));
      await pg.waitForTimeout(120);
      const scrolledTo = await pg.evaluate(() => Math.round(window.scrollY));

      /* Click the LAST photo button, the one furthest down the list. */
      const buttons = await pg.$$(ps.click);
      const target = buttons[buttons.length - 1];
      await target.click().catch(() => {});
      await pg.waitForTimeout(500);
      const gum = await pg.evaluate(() => window.__gum || 0);

      /* IS THE CAMERA ACTUALLY IN VIEW? */
      const view = await pg.evaluate(() => {
        const panel = document.querySelector('.gt-photo-panel');
        if (!panel) return { err: 'no .gt-photo-panel' };
        const r = panel.getBoundingClientRect();
        return { top: Math.round(r.top), bottom: Math.round(r.bottom),
                 vh: window.innerHeight, h: Math.round(r.height) };
      });
      const overlay = await pg.evaluate(() => {
        const el = document.querySelector('#gt-photo-overlay');
        if (!el) return 'missing';
        return getComputedStyle(el).display === 'none' ? 'hidden' : 'open';
      });

      if (gum >= 1) {
        ok(ps.tpl.padEnd(22) + ' photo button', ps.note + ': click -> getUserMedia');
      } else {
        bad(ps.tpl.padEnd(22) + ' photo button',
            'clicked, no camera requested (overlay ' + overlay + ')');
      }

      /* The panel must be ON SCREEN after scrolling ' + scrolledTo + 'px down.
         Being present is not the same as being findable. */
      if (view.err) {
        bad(ps.tpl.padEnd(22) + ' camera in view', view.err);
      } else if (view.bottom <= 0 || view.top >= view.vh) {
        bad(ps.tpl.padEnd(22) + ' camera in view',
            'OFF SCREEN after scrolling ' + scrolledTo + 'px: panel top=' +
            view.top + ' bottom=' + view.bottom + ' viewport=' + view.vh);
      } else if (view.top < -4) {
        bad(ps.tpl.padEnd(22) + ' camera in view',
            'panel starts above the viewport (top=' + view.top + ')');
      } else {
        ok(ps.tpl.padEnd(22) + ' camera in view',
           'scrolled ' + scrolledTo + 'px, panel at top=' + view.top +
           ' within ' + view.vh + 'px viewport');
      }

      await pg.close();
    }
  }

  /* ---- The iPad -----------------------------------------------------
     BarcodeDetector is a Chromium API; WebKit does not implement it, so on
     Safari the camera opened and then reported "this browser cannot decode
     in-camera". Simulated by DELETING BarcodeDetector, which is exactly the
     condition, and checked all the way through to a decoded value -- a fallback
     that loads but cannot read a barcode would be no better than the message. */
  /* ---- Every way the native detector can fail -----------------------
     Checking that BarcodeDetector EXISTS is not enough. On Chrome for Android
     the API is present but backed by a Play Services module downloaded on
     demand, so it can exist and still be useless -- and the code took the
     native branch and reported "cannot decode" without trying the fallback
     beside it. That is what survived a hard refresh on a Chrome browser.

     Each case below is a real device state, and each must end up on ZXing. */
  console.log('\n=== every native-detector failure must reach ZXing ===');
  {
    const tpl = 'gtscnd/scnhome.ejs';
    routes['/broken/' + tpl] = page(render(tpl), cssFor(tpl));

    const CASES = [
      { name: 'absent (Safari, desktop Chrome)',
        js: '(function(){ try { delete window.BarcodeDetector; } catch(e){} })()' },
      { name: 'constructor throws (Play module missing)',
        js: 'window.BarcodeDetector = function(){ throw new Error("NotSupportedError"); };' +
            'window.BarcodeDetector.getSupportedFormats = function(){ return Promise.resolve(["code_128"]); };' },
      { name: 'supports ZERO formats (module present but empty)',
        js: 'window.BarcodeDetector = function(){ return { detect: function(){ return Promise.resolve([]); } }; };' +
            'window.BarcodeDetector.getSupportedFormats = function(){ return Promise.resolve([]); };' },
      { name: 'detect() keeps rejecting (module broken at runtime)',
        js: 'window.BarcodeDetector = function(){ return { detect: function(){ ' +
            'return Promise.reject(new Error("broken")); } }; };' +
            'window.BarcodeDetector.getSupportedFormats = function(){ return Promise.resolve(["code_128"]); };' }
    ];

    for (const c of CASES) {
      const pg = await browser.newPage({ viewport: { width: 412, height: 915 },
                                         isMobile: true, hasTouch: true });
      await pg.addInitScript(HARNESS);
      await pg.addInitScript(c.js);
      await pg.goto(base + '/broken/' + tpl, { waitUntil: 'load' });
      await pg.addScriptTag({ content: scanJs });
      await pg.waitForTimeout(150);
      await pg.click('#gt-scan-start').catch(() => {});

      let loaded = false;
      for (let i = 0; i < 50 && !loaded; i++) {
        await pg.waitForTimeout(150);
        loaded = await pg.evaluate(() =>
          !!(window.ZXing && window.ZXing.BrowserMultiFormatReader));
      }
      const status = await pg.evaluate(() => {
        const el = document.querySelector('#gt-scan-status');
        return el ? el.textContent.trim() : '';
      });

      if (loaded && !/cannot decode/i.test(status)) {
        ok('fell back to ZXing: ' + c.name, status ? '"' + status + '"' : '');
      } else {
        bad('did NOT fall back: ' + c.name,
            'zxing=' + loaded + ' status="' + status + '"');
      }
      await pg.close();
    }
  }

  /* ---- A stream that never reports dimensions ------------------------
     video.play() resolving does NOT mean the video has a size yet. Start
     decoding then and both decoders get a 0x0 frame and read nothing for ever:
     "the camera opens but will not scan". A fake device stream has dimensions
     immediately, which is why no desk test could see it. */
  console.log('\n=== a camera that never produces a picture ===');
  {
    const tpl = 'gtscnd/scnhome.ejs';
    routes['/nopic/' + tpl] = page(render(tpl), cssFor(tpl));
    const pg = await browser.newPage({ viewport: { width: 412, height: 915 },
                                       isMobile: true, hasTouch: true });
    await pg.addInitScript(HARNESS);
    /* Pin videoWidth/videoHeight to 0 for every video element. */
    await pg.addInitScript(() => {
      Object.defineProperty(HTMLVideoElement.prototype, 'videoWidth',
        { get: function () { return 0; }, configurable: true });
      Object.defineProperty(HTMLVideoElement.prototype, 'videoHeight',
        { get: function () { return 0; }, configurable: true });
    });
    await pg.goto(base + '/nopic/' + tpl, { waitUntil: 'load' });
    await pg.addScriptTag({ content: scanJs });
    await pg.waitForTimeout(150);
    await pg.click('#gt-scan-start').catch(() => {});
    /* The gate allows ~6s before giving up, so wait past it. */
    await pg.waitForTimeout(8000);

    const st = await pg.evaluate(() => {
      const el = document.querySelector('#gt-scan-status');
      const dg = (window.gtScan && window.gtScan.diag) ? window.gtScan.diag() : {};
      return { status: el ? el.textContent.trim() : '', path: dg.path,
               reason: dg.reason, tries: dg.nativeTries };
    });

    if (/no picture/i.test(st.status)) {
      ok('says so instead of pretending to scan', '"' + st.status + '"');
    } else {
      bad('a picture-less camera was not reported',
          'status="' + st.status + '" path=' + st.path + ' reason=' + st.reason);
    }
    /* And it must not have burned the CPU decoding blank frames. */
    if (!st.tries) ok('did not decode against a 0x0 frame', 'no attempts made');
    else bad('decoded against a 0x0 frame', st.tries + ' attempts');
    await pg.close();
  }

  console.log('\n=== the Safari path: decoding a real barcode ===');
  {
    const tpl = 'gtscnd/scnhome.ejs';
    routes['/ipad/' + tpl] = page(render(tpl), cssFor(tpl));

    const pg = await browser.newPage({ viewport: { width: 810, height: 1080 },
                                       isMobile: true, hasTouch: true });
    /* No BarcodeDetector at all, and a canvas that returns a REAL barcode image
       so the decoder has something to decode. */
    await pg.addInitScript(HARNESS);
    await pg.addInitScript(() => { try { delete window.BarcodeDetector; } catch (e) {} });
    await pg.goto(base + '/ipad/' + tpl, { waitUntil: 'load' });

    const gone = await pg.evaluate(() => typeof window.BarcodeDetector === 'undefined');
    if (gone) ok('BarcodeDetector really is absent', 'the iPad condition');
    else bad('BarcodeDetector still present', 'this is not testing Safari');

    await pg.addScriptTag({ content: scanJs });
    await pg.waitForTimeout(150);
    await pg.click('#gt-scan-start').catch(() => {});

    /* The decoder is 353 KB and loads lazily, so allow for it. */
    let loaded = false;
    for (let i = 0; i < 40 && !loaded; i++) {
      await pg.waitForTimeout(150);
      loaded = await pg.evaluate(() => !!(window.ZXing && window.ZXing.BrowserMultiFormatReader));
    }
    if (loaded) ok('ZXing fallback loaded from userdata', 'via a <script src> tag');
    else bad('ZXing fallback did not load', 'the iPad still cannot decode');

    const status = await pg.evaluate(() => {
      const el = document.querySelector('#gt-scan-status');
      return el ? el.textContent.trim() : '';
    });
    if (/cannot decode/i.test(status)) {
      bad('status after fallback', 'still says "' + status + '"');
    } else {
      ok('status after fallback', status ? '"' + status + '"' : '(no warning)');
    }

    /* Does it actually READ one? Decode a rendered UPC-A straight through the
       library the page just loaded -- same code path the video frames take. */
    if (loaded) {
      const bwip = require('bwip-js');
      const png = await bwip.toBuffer({ bcid: 'upca', text: '061414102377',
                                        scale: 4, height: 20, includetext: false,
                                        paddingwidth: 8, paddingheight: 8 });
      const decoded = await pg.evaluate(async (b64) => {
        const img = new Image();
        await new Promise((res, rej) => {
          img.onload = res; img.onerror = rej;
          img.src = 'data:image/png;base64,' + b64;
        });
        const r = new window.ZXing.BrowserMultiFormatReader();
        try {
          const out = await r.decodeFromImageElement(img);
          return out ? out.getText() : null;
        } catch (e) { return 'ERR:' + (e && e.message ? e.message : e); }
      }, png.toString('base64'));

      if (decoded === '061414102377' || decoded === '0061414102377') {
        ok('ZXing decodes a real UPC-A', 'got ' + decoded);
      } else {
        bad('ZXing decode failed', 'got ' + JSON.stringify(decoded));
      }
    }
    await pg.close();
  }

  /* ---- THE END-TO-END DECODE ----------------------------------------
     A real <video> playing REAL barcode frames, via canvas.captureStream(),
     with BarcodeDetector deleted so the ZXing path runs. Nothing is stubbed
     downstream: if this passes, a picture of a barcode went in one end and
     pui.submit came out the other.

     This is the test that was missing every round. Chromium's fake device emits
     a rolling colour pattern -- a camera with no barcode in it -- so every
     earlier check could only ever prove that the decoder STARTED, never that it
     read anything. The device reported exactly that gap: zxLoaded=true,
     zxTries=0. */
  console.log('\n=== the cheat sheet must not print an undecodable barcode ===');
  {
    const gen = fs.readFileSync(path.resolve(__dirname, 'gen-demo-sheet.js'), 'utf8');
    if (/bcid === 'itf14'[^\n]*borderwidth = 0/.test(gen)) {
      ok('gen-demo-sheet suppresses the ITF-14 bearer bar',
         'a bearer bar closes the quiet zone and kills the camera read');
    } else {
      bad('gen-demo-sheet may print an ITF-14 with a bearer bar',
          'measured: 40 frames no read with it, first frame without it');
    }
  }

  console.log('\n=== end to end: a real barcode on camera must reach pui.submit ===');
  {
    const bwip = require('bwip-js');

    /* Every symbology the cheat sheet tells the audience to hold up. A decoder
       that reads Code 128 and quietly misses ITF would fail in the demo on the
       one card nobody tested. */
    /* Exactly the symbologies gen-demo-sheet.js renders onto the cards, so this
       test covers what the audience will physically hold up -- nothing more,
       nothing less. Derived by grepping the generator's svg() calls. */
    /* GS1 mod-10: weight the digits 3,1,3,1... from the RIGHT, and the check
       digit is what brings the total to a multiple of ten. Computed rather than
       hardcoded because bwip-js rejects a wrong one outright -- which is how the
       first version of this fixture failed. */
    const gtin = (body) => {
      let sum = 0;
      for (let i = 0; i < body.length; i++) {
        sum += Number(body[body.length - 1 - i]) * (i % 2 === 0 ? 3 : 1);
      }
      return body + String((10 - (sum % 10)) % 10);
    };

    const SYMBOLOGIES = [
      { bcid: 'upca',       text: '061414102377',   label: 'UPC-A (item cards)' },
      { bcid: 'ean13',      text: '4006381333931',  label: 'EAN-13' },
      { bcid: 'ean8',       text: '96385074',       label: 'EAN-8' },
      { bcid: 'code128',    text: 'GT-A12-03-B',    label: 'Code 128 (locations)' },
      { bcid: 'code39',     text: 'PO4471',         label: 'Code 39' },
      /* borderwidth 0 mirrors gen-demo-sheet.js, which suppresses the bearer bar
         for exactly the reason documented there: with it, this symbol does not
         decode through a camera at all. */
      { bcid: 'itf14', text: gtin('1061414102377'), label: 'ITF-14 (cases)',
        extra: { borderwidth: 0 } },
      { bcid: 'qrcode',     text: 'GT|CNT|4471',    label: 'QR (2D, best at distance)', twoD: true },
      { bcid: 'datamatrix', text: 'GT-DM-9910',     label: 'Data Matrix', twoD: true },
      { bcid: 'pdf417',     text: 'GT-PDF-4471',    label: 'PDF417', twoD: true },
      { bcid: 'azteccode',  text: 'GT-AZ-4471',     label: 'Aztec', twoD: true }
    ];

    const tpl = 'gtscnd/scnhome.ejs';
    routes['/e2e/' + tpl] = page(render(tpl), cssFor(tpl));

    for (const sym of SYMBOLOGIES) {
      /* height is a 1D-only option: passing it as undefined is not the same as
         omitting it, and bwip-js throws on the former. */
      const opts = { bcid: sym.bcid, text: sym.text, scale: 5, includetext: false,
                     paddingwidth: 20, paddingheight: 20 };
      if (!sym.twoD) opts.height = 24;
      Object.assign(opts, sym.extra || {});
      const png = await bwip.toBuffer(opts);
      const dataUrl = 'data:image/png;base64,' + png.toString('base64');

      const pg = await browser.newPage({ viewport: { width: 412, height: 915 },
                                         isMobile: true, hasTouch: true });
      await pg.addInitScript(HARNESS);
      await pg.addInitScript((url) => {
        /* No BarcodeDetector: force the ZXing path, which is the iOS path and the
           one that was broken. */
        try { delete window.BarcodeDetector; } catch (e) {}
        navigator.mediaDevices.getUserMedia = function () {
          return new Promise(function (resolve, reject) {
            var img = new Image();
            img.onload = function () {
              /* 1280x720, because that is what gt-scan.js asks the camera for
                 (width: { ideal: 1280 }) and frame resolution is not cosmetic
                 here: at 640x480 a 14-digit ITF-14 spread across the frame puts
                 its narrow bars under a pixel wide and NOTHING decodes it, while
                 the same symbol as a still image decodes fine. A fixture at the
                 wrong resolution reports a decoder bug that does not exist --
                 and hides the real-world advice, which is that wide 1D symbols
                 need the full frame width. */
              var c = document.createElement('canvas');
              c.width = 1280; c.height = 720;
              var ctx = c.getContext('2d');
              /* Repainted on a timer: a canvas that never changes stops
                 producing frames, and a frozen stream would pass for the wrong
                 reason. Letterboxed to preserve aspect, because squashing a 1D
                 barcode horizontally destroys the narrow bars. */
              var portion = window.__gtBarcodePortion || 1;
              var scale = Math.min(1200 * portion / img.width, 600 * portion / img.height);
              var w = img.width * scale, h = img.height * scale;
              var paint = function () {
                ctx.fillStyle = '#fff'; ctx.fillRect(0, 0, c.width, c.height);
                ctx.drawImage(img, (c.width - w) / 2, (c.height - h) / 2, w, h);
              };
              paint();
              setInterval(paint, 100);
              resolve(c.captureStream(10));
            };
            img.onerror = function () { reject(new Error('fixture image failed')); };
            img.src = url;
          });
        };
      }, dataUrl);

      await pg.goto(base + '/e2e/' + tpl, { waitUntil: 'load' });
      await pg.addScriptTag({ content: scanJs });
      await pg.waitForTimeout(200);
      await pg.click('#gt-scan-start').catch(() => {});

      let submitted = null;
      for (let i = 0; i < 40 && !submitted; i++) {
        await pg.waitForTimeout(250);
        submitted = await pg.evaluate(() => window.__submitted || null);
      }
      const dg = await pg.evaluate(() =>
        (window.gtScan && window.gtScan.diag) ? window.gtScan.diag() : {});

      const want = sym.expect || sym.text;
      const got = submitted ? String(submitted.scanval) : '';
      if (got && (got === want || got.indexOf(want) >= 0)) {
        ok(sym.label + ' decoded off the camera and submitted',
           dg.zxTries + ' frame(s), path=' + dg.path);
      } else {
        bad(sym.label + ' never reached pui.submit',
            'got=' + JSON.stringify(submitted) + ' path=' + dg.path +
            ' zxLoaded=' + dg.zxLoaded + ' zxTries=' + dg.zxTries +
            ' lastErr=' + dg.lastErr);
      }

      /* Separate assertion on purpose: zxLoaded=true with zxTries=0 is exactly
         what the iPad reported, and it means the loop never started rather than
         the barcode being unreadable. Worth naming so the next failure says
         which of the two it is. */
      if (dg.zxTries > 0) ok('  decode loop ran for ' + sym.bcid, dg.zxTries + ' frame(s) examined');
      else bad('  decode loop never ran for ' + sym.bcid,
               'zxLoaded=' + dg.zxLoaded + ' -- the reported device symptom');

      await pg.close();
    }
  }

  /* ---- a SMALL barcode in a big frame -------------------------------
     The reason the loop now crops to the reticle. A barcode occupying a fifth of
     a 1280x720 frame is a handful of pixels per bar by the time a full-frame
     binarizer sees it. This is the realistic case -- nobody fills the viewfinder
     with the barcode -- and it is the one no previous test represented. */
  console.log('\n=== a barcode that does not fill the frame ===');
  {
    const bwip = require('bwip-js');
    const png = await bwip.toBuffer({ bcid: 'code128', text: 'GT-A12-03-B', scale: 5,
                                      height: 24, includetext: false,
                                      paddingwidth: 20, paddingheight: 20 });
    const dataUrl = 'data:image/png;base64,' + png.toString('base64');
    const tpl = 'gtscnd/scnhome.ejs';
    routes['/e2e/' + tpl] = page(render(tpl), cssFor(tpl));

    const pg = await browser.newPage({ viewport: { width: 412, height: 915 },
                                       isMobile: true, hasTouch: true });
    await pg.addInitScript(HARNESS);
    await pg.addInitScript(() => { window.__gtBarcodePortion = 0.42; });
    await pg.addInitScript((url) => {
      try { delete window.BarcodeDetector; } catch (e) {}
      navigator.mediaDevices.getUserMedia = function () {
        return new Promise(function (resolve, reject) {
          var img = new Image();
          img.onload = function () {
            var c = document.createElement('canvas');
            c.width = 1280; c.height = 720;
            var ctx = c.getContext('2d');
            var portion = window.__gtBarcodePortion || 1;
            var scale = Math.min(1200 * portion / img.width, 600 * portion / img.height);
            var w = img.width * scale, h = img.height * scale;
            var paint = function () {
              ctx.fillStyle = '#fff'; ctx.fillRect(0, 0, c.width, c.height);
              ctx.drawImage(img, (c.width - w) / 2, (c.height - h) / 2, w, h);
            };
            paint(); setInterval(paint, 100);
            resolve(c.captureStream(10));
          };
          img.onerror = function () { reject(new Error('fixture failed')); };
          img.src = url;
        });
      };
    }, dataUrl);

    await pg.goto(base + '/e2e/' + tpl, { waitUntil: 'load' });
    await pg.addScriptTag({ content: scanJs });
    await pg.waitForTimeout(200);
    await pg.click('#gt-scan-start').catch(() => {});

    let submitted = null;
    for (let i = 0; i < 40 && !submitted; i++) {
      await pg.waitForTimeout(250);
      submitted = await pg.evaluate(() => window.__submitted || null);
    }
    const dg = await pg.evaluate(() => window.gtScan.diag());
    if (submitted && submitted.scanval === 'GT-A12-03-B') {
      ok('a barcode filling 42% of the frame still decoded',
         dg.zxTries + ' frame(s), analysed at ' + dg.frameW + 'x' + dg.frameH);
    } else {
      bad('a barcode filling 42% of the frame did not decode',
          'got=' + JSON.stringify(submitted) + ' frames=' + dg.zxTries + ' err=' + dg.lastErr);
    }
    /* The analysed frame must be the CROP, not the whole picture, or the crop is
       not actually doing anything. */
    if (dg.frameW && dg.frameW <= 1024) ok('the analysed frame is capped and cropped', dg.frameW + 'x' + dg.frameH);
    else bad('the analysed frame is not cropped/capped', 'frameW=' + dg.frameW);
    await pg.close();
  }

  /* ---- the photo path -----------------------------------------------
     The escape hatch for a stream that will not focus, which is the iOS case.
     Feeds a FILE through the real <input type=file> the code creates, so the
     wiring is tested and not just the decode. */
  console.log('\n=== scan from a photo (the iOS focus escape hatch) ===');
  {
    const bwip = require('bwip-js');
    const png = await bwip.toBuffer({ bcid: 'upca', text: '061414102377', scale: 6,
                                      height: 28, includetext: false,
                                      paddingwidth: 24, paddingheight: 24 });
    const file = '/tmp/gt-photo-fixture.png';
    fs.writeFileSync(file, png);

    const tpl = 'gtscnd/scnhome.ejs';
    const pg = await browser.newPage({ viewport: { width: 412, height: 915 },
                                       isMobile: true, hasTouch: true });
    await pg.addInitScript(HARNESS);
    await pg.addInitScript(() => { try { delete window.BarcodeDetector; } catch (e) {} });
    await pg.goto(base + '/e2e/' + tpl, { waitUntil: 'load' });
    await pg.addScriptTag({ content: scanJs });
    await pg.waitForTimeout(300);

    /* The button and the input must exist WITHOUT any template change -- they are
       created by this file, which is the point. */
    const built = await pg.evaluate(() => ({
      btn: !!document.querySelector('#gt-scan-photo-btn'),
      input: !!document.querySelector('#gt-scan-photo'),
      capture: (document.querySelector('#gt-scan-photo') || {}).getAttribute
        ? document.querySelector('#gt-scan-photo').getAttribute('capture') : null,
      accept: (document.querySelector('#gt-scan-photo') || {}).accept
    }));
    if (built.btn && built.input) ok('the photo control is built on a screen whose template knows nothing about it');
    else bad('the photo control was not built', JSON.stringify(built));
    if (built.capture === 'environment' && built.accept === 'image/*')
      ok('it asks for the rear camera', 'capture=environment accept=image/*');
    else bad('it will not open the camera', JSON.stringify(built));

    await pg.setInputFiles('#gt-scan-photo', file);
    let submitted = null;
    for (let i = 0; i < 40 && !submitted; i++) {
      await pg.waitForTimeout(250);
      submitted = await pg.evaluate(() => window.__submitted || null);
    }
    const dg = await pg.evaluate(() => window.gtScan.diag());
    if (submitted && submitted.scanval === '061414102377')
      ok('a photographed barcode decoded and submitted', 'photos=' + dg.photoTries + ' @' + dg.photoW + 'px');
    else
      bad('the photographed barcode did not decode',
          'got=' + JSON.stringify(submitted) + ' photos=' + dg.photoTries + ' err=' + dg.lastErr);
    await pg.close();
  }

  /* ---- the diagnostic line ------------------------------------------
     Five rounds of this symptom were diagnosed from a description. The line has
     to actually render, and it has to name the version -- a report that does not
     say which version produced it is what made rounds two and three ambiguous. */
  console.log('\n=== the in-app diagnostic line ===');
  {
    const tpl = 'gtscnd/scnhome.ejs';
    const pg = await browser.newPage({ viewport: { width: 412, height: 915 },
                                       isMobile: true, hasTouch: true });
    await pg.addInitScript(HARNESS);
    await pg.goto(base + '/e2e/' + tpl, { waitUntil: 'load' });
    await pg.addScriptTag({ content: scanJs });
    await pg.waitForTimeout(300);
    const before = await pg.evaluate(() => {
      const el = document.querySelector('#gt-scan-diag');
      return el ? getComputedStyle(el).display : 'missing';
    });
    if (before === 'none') ok('it stays out of the way until asked', 'display:none');
    else bad('the diagnostic line is visible when nothing is wrong', 'display=' + before);

    const btns = await pg.$$('#gt-scan-camera .gt-scan-photo');
    await btns[1].click();
    await pg.waitForTimeout(150);
    const shown = await pg.evaluate(() => {
      const el = document.querySelector('#gt-scan-diag');
      return { display: getComputedStyle(el).display, text: el.textContent };
    });
    const stamp = scanJs.match(/VERSION = '([0-9a-z]+)'/)[1];
    if (shown.display !== 'none' && shown.text.indexOf('v' + stamp) === 0)
      ok('one tap reports the running version', shown.text.slice(0, 60));
    else
      bad('the diagnostic line does not name the running version', JSON.stringify(shown));
    await pg.close();
  }

  /* ---- THE SELF-UPGRADE ----------------------------------------------
     The access log proved the device fetched v...f at 15:14, the fix shipped at
     15:26, and the device made no request at all afterwards. The operator
     refreshed and nothing was re-fetched, so the old code kept running. A stamp
     bump is invisible to a page that never asks the server anything.

     This asserts the recovery: a page running an OLDER copy notices, pulls the
     current one, AND ENDS UP WIRED TO IT. The wiring half is the trap -- bind()
     marks the band and returns early when it sees the mark, so a newer copy would
     install itself, attach nothing, and leave Start calling the old code:
     upgraded and inert. */
  console.log('\n=== a stale page must upgrade itself and be wired to the new copy ===');
  {
    const stamp = scanJs.match(/VERSION = '([0-9a-z]+)'/)[1];
    /* An "old" build: same file, older version, and a marker we can look for. */
    const oldJs = scanJs.replace(/VERSION = '[0-9a-z]+'/, "VERSION = '20260101a'")
                        .replace(/window\.gtScan = \{/, 'window.__gtOldRan = true; window.gtScan = {');

    /* gt-version.js and gt-scan.js are served from the real userdata directory by
       the handler above, which is what we want here: this exercises the generated
       file rather than a convenient stand-in, so a missing or stale gt-version.js
       fails the test instead of passing on a fixture.

       (Registering short-form routes is how the first run of this test "proved"
       the upgrade was broken -- the probe 404'd and the harness was at fault.) */

    const tpl = 'gtscnd/scnhome.ejs';
    const pg = await browser.newPage({ viewport: { width: 412, height: 915 },
                                       isMobile: true, hasTouch: true });
    await pg.addInitScript(HARNESS);
    /* Count getUserMedia calls: two live copies would open two streams. */
    await pg.addInitScript(() => {
      window.__gumCalls = 0;
      const orig = navigator.mediaDevices && navigator.mediaDevices.getUserMedia;
      if (orig) {
        navigator.mediaDevices.getUserMedia = function () {
          window.__gumCalls++;
          return orig.apply(navigator.mediaDevices, arguments);
        };
      }
    });
    await pg.goto(base + '/e2e/' + tpl, { waitUntil: 'load' });

    /* The stale copy installs first, exactly as a cached file would. */
    await pg.addScriptTag({ content: oldJs });
    await pg.waitForTimeout(250);
    /* Assert the stale copy RAN, not that it is still in charge: the upgrade can
       land inside 250ms, and racing the fix is not a failure of the fix. */
    const before = await pg.evaluate(() => ({
      version: window.gtScan.__version, oldRan: !!window.__gtOldRan
    }));
    if (before.oldRan) ok('the stale copy installed first, as a cached file would', 'v' + before.version + ' at first look');
    else bad('the fixture never ran the stale copy', JSON.stringify(before));

    /* Its own freshness check should now pull the current build in. */
    let after = null;
    for (let i = 0; i < 40; i++) {
      await pg.waitForTimeout(250);
      after = await pg.evaluate(() => window.gtScan.__version);
      if (after === stamp) break;
    }
    if (after === stamp) ok('it noticed it was stale and upgraded itself', '20260101a -> ' + after);
    else bad('a stale page did not upgrade', 'still v' + after + ' -- the reported failure mode');

    /* And the new copy must actually be listening. */
    const wired = await pg.evaluate(() => ({
      bound: document.querySelector('#gt-scan-camera').getAttribute('data-gt-scan-bound'),
      extras: document.querySelectorAll('#gt-scan-extra').length,
      diags: document.querySelectorAll('#gt-scan-diag').length,
      ver: (document.querySelector('#gt-scan-ver') || {}).textContent
    }));
    if (wired.extras === 1 && wired.diags === 1) ok('the controls were rebuilt exactly once', JSON.stringify(wired));
    else bad('the controls were duplicated or lost', JSON.stringify(wired));
    if (wired.ver === 'v' + stamp) ok('the version chip shows the UPGRADED version', wired.ver);
    else bad('the chip still names the old version', String(wired.ver));

    await pg.click('#gt-scan-start').catch(() => {});
    await pg.waitForTimeout(600);
    const calls = await pg.evaluate(() => window.__gumCalls);
    if (calls === 1) ok('one click opens exactly one camera', calls + ' getUserMedia call');
    else bad('the old listener is still attached', calls + ' getUserMedia calls from one click');

    await pg.close();
  }

  /* ---- the access-log ping -------------------------------------------
     The channel that replaces "please send me a screenshot". The query string is
     the payload, so this asserts the request is made and carries what is needed to
     identify a device and its build. */
  console.log('\n=== the app must report itself to the access log ===');
  {
    const stamp = scanJs.match(/VERSION = '([0-9a-z]+)'/)[1];
    const tpl = 'gtscnd/scnhome.ejs';
    const pg = await browser.newPage({ viewport: { width: 412, height: 915 },
                                       isMobile: true, hasTouch: true });
    const pings = [];
    pg.on('request', (r) => {
      if (r.url().indexOf('gt-ping.js') >= 0) pings.push(r.url());
    });
    await pg.addInitScript(HARNESS);
    await pg.goto(base + '/e2e/' + tpl, { waitUntil: 'load' });
    await pg.addScriptTag({ content: scanJs });
    await pg.waitForTimeout(600);

    /* One install ping, sent by up to TWO mechanisms on purpose: sendBeacon and an
       image. The redundancy is the point -- the script-tag version fired correctly
       in Chromium and produced no request whatsoever from the iPad, so relying on a
       single mechanism is what got us here. What must not happen is a flood. */
    if (pings.length >= 1 && pings.length <= 2) ok('one install ping, via ' + pings.length + ' mechanism(s)', 'not a flood');
    else bad('the install ping fired the wrong number of times', pings.length + ' requests');

    const q = pings[0] ? new URL(pings[0]).searchParams : new URLSearchParams();
    const need = { v: stamp, via: 'tag', why: 'install' };
    for (const k of Object.keys(need)) {
      if (q.get(k) === need[k]) ok('  ping carries ' + k, q.get(k));
      else bad('  ping is missing ' + k, 'got ' + q.get(k) + ', wanted ' + need[k]);
    }
    if ((q.get('ua') || '').length > 10) ok('  ping carries the user agent', (q.get('ua') || '').slice(0, 40) + '…');
    else bad('  ping has no user agent', String(q.get('ua')));

    /* The file it fetches has to be SERVED, or every ping reads as a 404. */
    const pres = await pg.evaluate(async () => {
      const r = await fetch('/profoundui/userdata/ui/gtcommon/gt-ping.js?probe=1');
      return r.status;
    });
    if (pres === 200) ok('  gt-ping.js is served, so pings log as 200', 'HTTP ' + pres);
    else bad('  gt-ping.js is not served', 'HTTP ' + pres);

    await pg.close();
  }

  /* ---- the screen channel --------------------------------------------
     The only channel that has reached us every single round. Two outbound
     mechanisms failed silently on the iPad; the screen state never has. */
  console.log('\n=== the diagnostics must travel through the screen ===');
  {
    const stamp = scanJs.match(/VERSION = '([0-9a-z]+)'/)[1];
    const tpl = 'gtscnd/scnhome.ejs';
    const pg = await browser.newPage({ viewport: { width: 412, height: 915 },
                                       isMobile: true, hasTouch: true });
    await pg.addInitScript(HARNESS);
    await pg.goto(base + '/e2e/' + tpl, { waitUntil: 'load' });
    /* Run it the way the SHIM does -- new Function, not a script tag. Every camera
       test until now used the tag route, which is not what the device runs. */
    await pg.evaluate((src) => { window.__gtVia = 'snapshot'; (new Function(src))(); }, scanJs);
    await pg.waitForTimeout(400);

    const btns = await pg.$$('#gt-scan-camera .gt-scan-photo');
    await btns[1].click();
    await pg.waitForTimeout(250);
    const sub = await pg.evaluate(() => window.__submitted || null);

    if (sub && String(sub.scanval || '').indexOf('GTDIAG v=' + stamp) === 0) {
      ok('one tap sends the report through the screen', sub.scanval);
    } else {
      bad('the report did not reach the screen', JSON.stringify(sub));
    }
    /* SCANVAL is char(120): a longer payload is silently truncated by the display
       file, and the error code -- the part worth having -- lives at the end. */
    if (sub && String(sub.scanval).length <= 118) ok('  it fits SCANVAL char(120)', String(sub.scanval).length + ' chars');
    else bad('  the report will be truncated', sub ? String(sub.scanval).length + ' chars' : 'none');
    if (sub && /via=(snap|tag)/.test(sub.scanval)) ok('  it names the delivery route', sub.scanval.match(/via=\w+/)[0]);
    else bad('  it does not say which route delivered the code', String(sub && sub.scanval));

    await pg.close();
  }

  /* ---- THE DEVICE'S ACTUAL CONDITION ---------------------------------
     A device on which a dynamically appended <script src> produces NO REQUEST.
     That is not hypothetical: the access log proved the iPad never once fetched
     gt-zxing.min.js from inside Genie, while fetching it fine from a standalone
     page. iOS has no BarcodeDetector, so no decoder means a camera that opens and
     can never read -- the symptom reported seven times, and no test represented it
     because every test let the fetch succeed.

     Here every network route to the decoder is BLOCKED, exactly as on the device,
     and a real barcode must still decode through the copy the shim inlines. */
  console.log('\n=== scanning must work when the decoder cannot be fetched ===');
  {
    const bwip = require('bwip-js');
    const png = await bwip.toBuffer({ bcid: 'upca', text: '061414102377', scale: 5,
                                      height: 24, includetext: false,
                                      paddingwidth: 20, paddingheight: 20 });
    const dataUrl = 'data:image/png;base64,' + png.toString('base64');
    const zxB64 = fs.readFileSync(path.join(UI, 'gtcommon', 'gt-zxing.min.js')).toString('base64');

    const tpl = 'gtscnd/scnhome.ejs';
    const pg = await browser.newPage({ viewport: { width: 412, height: 915 },
                                       isMobile: true, hasTouch: true });

    /* Kill every network path to the decoder. */
    let blocked = 0;
    await pg.route('**/gt-zxing.min.js*', (r) => { blocked++; return r.abort(); });

    await pg.addInitScript(HARNESS);
    await pg.addInitScript((z) => {
      try { delete window.BarcodeDetector; } catch (e) {}
      /* This is what the shim provides -- the page carries the decoder. */
      window.__gtLoadZxing = function () {
        if (window.ZXing && window.ZXing.BrowserMultiFormatReader) return true;
        var bin = atob(z), bytes = new Uint8Array(bin.length);
        for (var i = 0; i < bin.length; i++) bytes[i] = bin.charCodeAt(i);
        (new Function(new TextDecoder('utf-8').decode(bytes)))();
        return !!(window.ZXing && window.ZXing.BrowserMultiFormatReader);
      };
    }, zxB64);
    await pg.addInitScript((url) => {
      navigator.mediaDevices.getUserMedia = function () {
        return new Promise(function (resolve, reject) {
          var img = new Image();
          img.onload = function () {
            var c = document.createElement('canvas');
            c.width = 1280; c.height = 720;
            var ctx = c.getContext('2d');
            var sc = Math.min(1200 / img.width, 600 / img.height);
            var w = img.width * sc, h = img.height * sc;
            var paint = function () {
              ctx.fillStyle = '#fff'; ctx.fillRect(0, 0, c.width, c.height);
              ctx.drawImage(img, (c.width - w) / 2, (c.height - h) / 2, w, h);
            };
            paint(); setInterval(paint, 100);
            resolve(c.captureStream(10));
          };
          img.onerror = function () { reject(new Error('fixture failed')); };
          img.src = url;
        });
      };
    }, dataUrl);

    await pg.goto(base + '/e2e/' + tpl, { waitUntil: 'load' });
    /* Delivered the way the shim delivers it, not as a tag. */
    await pg.evaluate((src) => { window.__gtVia = 'snapshot'; (new Function(src))(); }, scanJs);
    await pg.waitForTimeout(300);
    await pg.click('#gt-scan-start').catch(() => {});

    let submitted = null;
    for (let i = 0; i < 60 && !submitted; i++) {
      await pg.waitForTimeout(250);
      submitted = await pg.evaluate(() => window.__submitted || null);
    }
    const dg = await pg.evaluate(() => window.gtScan.diag());

    if (submitted && submitted.scanval === '061414102377') {
      ok('a barcode decoded with every decoder fetch blocked',
         'zxVia=' + dg.zxVia + ' after ' + dg.zxTries + ' frame(s)');
    } else {
      bad('scanning failed when the decoder could not be fetched',
          'got=' + JSON.stringify(submitted) + ' zxVia=' + dg.zxVia +
          ' zxLoaded=' + dg.zxLoaded + ' -- THE DEVICE FAILURE');
    }
    if (dg.zxVia === 'inlined') ok('  it used the copy carried in the page', dg.zxVia);
    else bad('  it did not use the inlined copy', 'zxVia=' + dg.zxVia);

    await pg.close();
  }

  /* And the generated shim must actually carry the decoder, or the above is
     testing a fixture and nothing else. */
  console.log('\n=== the deployed shim must carry the decoder ===');
  {
    for (const skin of (process.argv[2] || 'pls,Classic').split(',').filter(Boolean)) {
      /* The skins live on the IBM i, not locally -- read them the way every other
         deployed-state check in this file does. */
      let h;
      try {
        h = execFileSync('ssh', ['dev', `cat "${SKINS_DIR}/${skin}/start.html"`],
                         { maxBuffer: 128 * 1024 * 1024 }).toString();
      } catch (e) { bad(skin + ': start.html unreadable', String(e.message).slice(0, 60)); continue; }
      const m = h.match(/var ZXING_B64 = "([A-Za-z0-9+/=]+)"/);
      if (!m) { bad(skin + ': the shim carries no inlined decoder', 'iOS cannot scan without it'); continue; }
      const src = Buffer.from(m[1], 'base64').toString('utf8');
      if (src.indexOf('BrowserMultiFormatReader') >= 0 && src.length > 300000)
        ok(skin + ': the shim carries the decoder', Math.round(src.length / 1024) + ' KB inlined');
      else
        bad(skin + ': the inlined decoder looks wrong', src.length + ' bytes');
    }
  }

  await browser.close();
  server.close();
  console.log('\n' + (fail ? 'FAILED' : 'The camera button works on every screen that has one') +
              ': ' + pass + ' passed, ' + fail + ' failed');
  process.exit(fail ? 1 : 0);
})();
