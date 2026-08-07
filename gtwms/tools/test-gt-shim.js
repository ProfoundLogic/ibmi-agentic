#!/usr/bin/env node
/*
 * GT Warehouse Mobile -- Genie shim functional test
 *
 *   node gtwms/tools/test-gt-shim.js [skin[,skin...]] [https://host:port]
 *
 * Defaults to every skin the shim is deployed to. Each skin carries its own
 * independent copy of the snapshot, so each has to be tested separately -- it
 * is entirely possible for one to be current and another stale.
 *
 * Fetches the DEPLOYED start.html, extracts the GTWMS shim block, runs it in a
 * real browser, and then does what Profound UI does: issues an XHR for the
 * template URL. The response must be our template, byte for byte.
 *
 * This is the test that matters, because the shim's whole job is to make that
 * one XHR succeed. It also catches the stale-snapshot trap: if someone edits
 * the .ejs and redeploys it without regenerating the shim, the bytes served to
 * the browser are the OLD template and nothing on screen changes. Here that
 * shows up as a mismatch rather than as a confusing demo.
 */

'use strict';

const fs = require('fs');
const path = require('path');
const https = require('https');
const { chromium } = require('playwright-core');

const args = process.argv.slice(2).filter((a) => a !== '--check');
const SKINS = (args.find((a) => !a.startsWith('http')) || 'pls,Classic').split(',');
const HOST = args.find((a) => a.startsWith('http')) || 'https://idev.profoundlogic.com:8103';
const skinUrl = (skin) =>
  HOST + '/profoundui/userdata/genie%20skins/' + encodeURIComponent(skin) + '/start.html';
const UI = path.join(__dirname, '..', '..', 'htdocs', 'profoundui', 'userdata', 'ui');
const CHROME = '/home/coder/.cache/ms-playwright/chromium-1228/chrome-linux64/chrome';

/* Every screen the shim claims. Each is fetched exactly the way Profound UI
 * would, including cache-busting, and compared against the file on disk. */
const SCREENS = [
  { name: 'gtmnud/menu.ejs',    url: '/profoundui/userdata/ui/gtmnud/menu.ejs?v=99',
    file: ['gtmnud', 'menu.ejs'] },
  { name: 'gtscnd/scnhome.ejs', url: '/profoundui/userdata/ui/gtscnd/scnhome.ejs?v=99',
    file: ['gtscnd', 'scnhome.ejs'] },
  { name: 'gtitmd/itmscan.ejs',  url: '/profoundui/userdata/ui/gtitmd/itmscan.ejs?v=99',
    file: ['gtitmd', 'itmscan.ejs'] },
  { name: 'gtitdd/itmdetl.ejs',  url: '/profoundui/userdata/ui/gtitdd/itmdetl.ejs?v=99',
    file: ['gtitdd', 'itmdetl.ejs'] },
];

function fetchText(url) {
  return new Promise((resolve, reject) => {
    https.get(url, { rejectUnauthorized: false, timeout: 45000 }, (res) => {
      let d = '';
      res.setEncoding('utf8');
      res.on('data', (c) => (d += c));
      res.on('end', () => resolve({ status: res.statusCode, body: d }));
    }).on('error', reject);
  });
}

(async () => {
  let fail = 0;
  const check = (label, ok, detail) => {
    if (!ok) fail++;
    console.log((ok ? '  PASS  ' : '  FAIL  ') + label + (detail ? '  -- ' + detail : ''));
  };

  const browser = await chromium.launch({ executablePath: CHROME, args: ['--no-sandbox'] });

for (const SKIN of SKINS) {
  console.log('\nskin: ' + SKIN);
  const SKIN_URL = skinUrl(SKIN);
  const res = await fetchText(SKIN_URL);
  check('start.html served', res.status === 200, 'http ' + res.status + ', ' + res.body.length + ' bytes');

  const b = res.body.indexOf('<!-- GTWMS EJS shim BEGIN');
  const e = res.body.indexOf('<!-- GTWMS EJS shim END -->');
  check('GTWMS shim block present', b !== -1 && e !== -1);
  if (b === -1 || e === -1) continue;

  /* The other projects share this file. Breaking them would be worse than
   * our own screen not working, so assert they survived. */
  check('FIFA shim still intact', res.body.includes('FIFA EJS shim END'));
  check('VWCUST shim still intact', res.body.includes('VWCUST EJS shim END'));

  const block = res.body.slice(b, e);
  const js = block.slice(
    block.indexOf('<script type="text/javascript">') + '<script type="text/javascript">'.length,
    block.lastIndexOf('</script>')
  );

  const page = await browser.newPage();
  const logs = [];
  page.on('console', (m) => logs.push(m.text()));

  await page.setContent('<!doctype html><meta charset="utf-8"><body></body>');
  await page.addScriptTag({ content: js });

  const armed = await page.evaluate(() => window.__gtwmsEjsShimLoaded === true);
  check('shim armed', armed, logs.filter((l) => l.indexOf('GTWMS') !== -1)[0] || '');

  /* The real test: ask for each template the way Profound UI does, including
   * a cache-busting query string, and compare what comes back. */
  for (const sc of SCREENS) {
    const srcTpl = fs.readFileSync(path.join(UI, sc.file[0], sc.file[1]), 'utf8');
    const got = await page.evaluate((url) => new Promise((resolve) => {
      const x = new XMLHttpRequest();
      x.open('GET', url, true);
      x.onreadystatechange = function () {
        if (x.readyState === 4) resolve({ status: x.status, text: x.responseText });
      };
      x.send();
      setTimeout(() => resolve({ status: -1, text: '' }), 5000);
    }), sc.url);

    check(sc.name + ' served from snapshot', got.status === 200, 'status ' + got.status);
    check(sc.name + ' matches source byte for byte', got.text === srcTpl,
          got.text === srcTpl ? '' : 'got ' + got.text.length + ', expected ' + srcTpl.length);
  }

  /* Screen JS is snapshotted too, and going stale there is just as silent as
   * a stale template -- the camera or the scanner would simply stop working
   * while the screen still looked perfectly fine.
   *
   * Each entry's snapshot is the concatenation of its js[] files in order, so
   * the expected text is built the same way the generator builds it. */
  var JS_SNAPSHOTS = [
    { match: 'scnhome', files: ['gt-scan.js'] },
    { match: 'itmdetl', files: ['gt-carousel.js', 'gt-photo.js'] },
  ];

  for (const snap of JS_SNAPSHOTS) {
    const expected = snap.files
      .map((f) => fs.readFileSync(path.join(UI, 'gtcommon', f), 'utf8'))
      .join('\n;\n');
    const jsOk = await page.evaluate(function (a) {
      var e = (window.__gtwmsEntries || []).filter(function (x) {
        return x.pattern.indexOf(a.match) !== -1;
      })[0];
      if (!e || !e.jsB64) return 'no js snapshot';
      var bin = atob(e.jsB64);
      var bytes = new Uint8Array(bin.length);
      for (var i = 0; i < bin.length; i++) bytes[i] = bin.charCodeAt(i);
      return new TextDecoder('utf-8').decode(bytes) === a.expected ? true : 'mismatch';
    }, { match: snap.match, expected: expected });
    check(snap.match + ' js snapshot matches ' + snap.files.join(' + '),
          jsOk === true, jsOk === true ? '' : String(jsOk));
  }

  /* The capture path has a hard dependency the shim cannot see: gt-photo.js
   * must actually define the globals the template's inline onclick handlers
   * call. A rename on either side is silent -- the button just does nothing.
   * Run the snapshot and assert the contract. */
  const photoApi = await page.evaluate(() => {
    var e = (window.__gtwmsEntries || []).filter(function (x) {
      return x.pattern.indexOf('itmdetl') !== -1;
    })[0];
    if (!e || !e.jsB64) return 'no js snapshot';
    var bin = atob(e.jsB64);
    var bytes = new Uint8Array(bin.length);
    for (var i = 0; i < bin.length; i++) bytes[i] = bin.charCodeAt(i);
    try { (new Function(new TextDecoder('utf-8').decode(bytes)))(); }
    catch (err) { return 'threw: ' + err.message; }
    if (!window.gtPhoto) return 'gtPhoto not defined';
    var missing = ['pick', 'chosen', 'use', 'close'].filter(function (fn) {
      return typeof window.gtPhoto[fn] !== 'function';
    });
    return missing.length ? 'missing: ' + missing.join(', ') : true;
  });
  check('gtPhoto exposes the handlers the template calls', photoApi === true,
        photoApi === true ? '' : String(photoApi));

  /* And the other direction: every gtPhoto.<fn> the template invokes from an
   * inline attribute must exist on that API. */
  const tplSrc = fs.readFileSync(path.join(UI, 'gtitdd', 'itmdetl.ejs'), 'utf8');
  const called = [...new Set(
    [...tplSrc.matchAll(/gtPhoto\.([A-Za-z_$][\w$]*)\s*\(/g)].map((m) => m[1])
  )];
  const known = ['pick', 'chosen', 'use', 'close'];
  const unknownCalls = called.filter((c) => known.indexOf(c) === -1);
  check('template calls only handlers gt-photo.js defines',
        called.length > 0 && unknownCalls.length === 0,
        unknownCalls.length ? 'unknown: ' + unknownCalls.join(', ')
                            : 'calls: ' + called.join(', '));

  /* A non-matching URL must still go to the real network, or we would have
   * broken every other request on the page -- including the other projects'
   * screens that share this start.html.
   *
   * The URL has to be absolute: the test page is about:blank, so a relative
   * one cannot be resolved against a base and open() throws for reasons that
   * have nothing to do with the shim. */
  const passthru = await page.evaluate((base) => {
    try {
      const x = new XMLHttpRequest();
      x.open('GET', base + '/nothing-of-ours.txt', true);
      /* If the shim had wrongly claimed it, readyState would be 1 from the
       * fake object; the real XHR also reports 1, so assert on identity of
       * behaviour instead: a claimed URL never reaches the network layer, and
       * an unclaimed one accepts a request header without throwing. */
      x.setRequestHeader('X-Gt-Passthru', '1');
      return true;
    } catch (err) { return 'threw: ' + err.name; }
  }, HOST);
  check('non-matching URL falls through to real XHR', passthru === true,
        passthru === true ? '' : String(passthru));

  const styled = await page.evaluate(() => {
    const s = document.getElementById('gtwms-gtmnud-menu-style');
    return s ? s.textContent.indexOf('--gt-red:        #E31837') !== -1 : false;
  });
  check('brand CSS injected', styled);

  await page.close();
}

  await browser.close();
  console.log(fail ? `\n${fail} check(s) FAILED` : `\nAll checks passed across ${SKINS.length} skin(s)`);
  process.exit(fail ? 1 : 0);
})();
