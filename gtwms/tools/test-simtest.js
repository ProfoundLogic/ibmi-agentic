#!/usr/bin/env node
/*
 * Simple camera test -- the whole screen, checked offline.
 *
 *   node gtwms/tools/test-simtest.js
 *
 * This screen exists to be a measuring instrument: it is what we will believe
 * when it tells us what the iPad is doing. So it has to be right itself, and
 * the checks here are the ones that were missing for seven rounds on the main
 * application:
 *
 *   - a REAL barcode, in a REAL <video>, decoded end to end. Chromium's fake
 *     camera emits a rolling colour pattern -- a camera with no barcode in it --
 *     so every earlier check could only prove the decoder STARTED.
 *
 *   - with EVERY NETWORK FETCH OF THE DECODER BLOCKED. That was the actual root
 *     cause: on the iPad inside Genie a dynamically appended <script src>
 *     produced no request at all, so the decoder never arrived and the camera
 *     could never read anything. A test that is granted a capability the device
 *     withholds cannot fail the way the device fails.
 *
 *   - with NO screen JavaScript at all, because a screen that goes blank when
 *     its JS fails cannot tell us anything about why its JS failed.
 *
 * Every field name and every handler name is derived from the source, never
 * restated here: a hand-kept copy is a check that quietly stops covering the
 * thing it names.
 */

'use strict';

const fs = require('fs');
const path = require('path');
const http = require('http');
const ejs = require('ejs');
const bwip = require('bwip-js');
const { chromium } = require('playwright-core');

const ROOT = path.resolve(__dirname, '../..');
const UI = path.join(ROOT, 'htdocs/profoundui/userdata/ui');
const CHROME = '/home/coder/.cache/ms-playwright/chromium-1228/chrome-linux64/chrome';

const TPL_REL = 'gtsimd/simtest.ejs';
const JS_REL = 'gtsimd/simtest.js';
const CSS_REL = ['gtcommon/gt-theme.css', 'gtsimd/simtest.css'];
const JSON_SRC = path.join(ROOT, 'gtwms/qddssrc/gtsimd.json');

let pass = 0, fail = 0;
const ok = (m, d) => { pass++; console.log('  PASS  ' + m + (d ? '  -- ' + d : '')); };
const bad = (m, d) => { fail++; console.log('  FAIL  ' + m + (d ? '  -- ' + d : '')); };

const tplSrc = fs.readFileSync(path.join(UI, TPL_REL), 'utf8');
const jsSrc = fs.readFileSync(path.join(UI, JS_REL), 'utf8');
const cssSrc = CSS_REL.map((f) => fs.readFileSync(path.join(UI, f), 'utf8')).join('\n');

/* The one format in the display file, read from the source of truth. */
const fmt = JSON.parse(fs.readFileSync(JSON_SRC, 'utf8')).formats.simtest;
const FIELDS = Object.keys(fmt.fields);
const SUBFILES = Object.keys(fmt.subfiles || {});
const SFL = SUBFILES[0];
const SFL_FIELDS = Object.keys(fmt.subfiles[SFL].fields);
const SFL_CLEAR = fmt.subfiles[SFL].clear;

/* Genie's own stylesheet has three rules that trap EJS layouts: a z-index that
 * captures stacking contexts, nowrap that stops all wrapping, and padding on
 * every div. Reproduce them, or the offline render is kinder than the device. */
const GENIE = 'html,body{margin:0;padding:0}div{white-space:nowrap;z-index:10;padding:1px}';

function render(data) {
  return ejs.render(tplSrc, data);
}

function pageHtml(body) {
  return '<!doctype html><html><head><meta charset="utf-8">' +
    '<meta name="viewport" content="width=device-width, initial-scale=1">' +
    '<style>' + GENIE + '</style><style>' + cssSrc + '</style></head><body>' +
    body + '</body></html>';
}

/* Exactly the fields the display file declares, and nothing else -- so a field
 * added to the template but not to the JSON shows up as a render failure here
 * rather than as a blank screen on the device. */
function data(opts) {
  const o = opts || {};
  const d = {
    action: '', scanval: '', langpref: o.lang || 'EN',
    msg: o.msg || '',
    lastval: o.val || '', lastsymb: o.val ? 'CODE_128' : '',
    lastlen: o.val ? o.val.length : 0, lasttime: o.val ? '14:32:07' : '',
    diag: o.diag || '',
  };
  d[SFL] = [];
  for (let i = 1; i <= (o.rows || 0); i++) {
    d[SFL].push({
      _rrn: i, sseq: i,
      sval: '0614141000' + String(100 + i),
      ssymb: i % 2 ? 'UPC_A' : 'QR_CODE',
      stime: '14:3' + (i % 10) + ':01',
    });
  }
  return d;
}

/* A barcode drawn small in a big frame, played into a real <video> through
 * canvas.captureStream(). `portion` is how much of the frame width the barcode
 * fills -- nobody fills a viewfinder with a barcode, and the realistic case is
 * the one no earlier test represented. */
async function fixture(text, bcid, portion) {
  const png = await bwip.toBuffer({
    bcid: bcid || 'code128', text: text, scale: 5, height: 24,
    includetext: false, paddingwidth: 20, paddingheight: 20,
  });
  return 'data:image/png;base64,' + png.toString('base64');
}

const CAMERA_STUB = (a) => {
  var url = a.url, portion = a.portion;
  /* No BarcodeDetector: the iOS condition, where a JavaScript decoder is the
     only thing that can read anything at all. */
  try { delete window.BarcodeDetector; } catch (e) {}

  window.__gum = 0;
  navigator.mediaDevices = navigator.mediaDevices || {};
  navigator.mediaDevices.getUserMedia = function () {
    window.__gum++;
    return new Promise(function (resolve, reject) {
      var img = new Image();
      img.onload = function () {
        var c = document.createElement('canvas');
        c.width = 1280; c.height = 720;
        var ctx = c.getContext('2d');
        var draw = function () {
          ctx.fillStyle = '#8a8a8a';
          ctx.fillRect(0, 0, c.width, c.height);
          var w = c.width * portion;
          var h = w * (img.height / img.width);
          ctx.drawImage(img, (c.width - w) / 2, (c.height - h) / 2, w, h);
          requestAnimationFrame(draw);
        };
        draw();
        window.__stubTracks = 0;
        var stream = c.captureStream(10);
        var tracks = stream.getVideoTracks();
        for (var i = 0; i < tracks.length; i++) {
          var t = tracks[i];
          window.__stubTracks++;
          var realStop = t.stop.bind(t);
          t.stop = function () { window.__stopped = (window.__stopped || 0) + 1; realStop(); };
        }
        resolve(stream);
      };
      img.onerror = function () { reject(new Error('fixture failed')); };
      img.src = url;
    });
  };
};

/* pui does not exist offline. Record what the screen tried to send instead. */
const PUI_STUB = () => {
  window.__submitted = null;
  window.pui = {
    submit: function (o) { window.__submitted = o; },
  };
};

(async () => {
  console.log('=== the template only names fields the display file declares ===');
  {
    const locals = new Set(
      [...tplSrc.matchAll(/\b(?:var|function)\s+([A-Za-z_$][\w$]*)|function\s*\(\s*([A-Za-z_$][\w$]*)/g)]
        .flatMap((m) => [m[1], m[2]]).filter(Boolean));
    const refs = new Set([...tplSrc.matchAll(/<%[=-]\s*([A-Za-z_$][\w$]*)/g)].map((m) => m[1]));
    const known = new Set([...FIELDS, ...SUBFILES, SFL_CLEAR,
      'typeof', 'Number', 'String', 'Math', 'JSON']);
    const unknown = [...refs].filter((r) => !known.has(r) && !locals.has(r));

    /* An identifier the format does not carry is not a blank value -- EJS
       throws on it and the WHOLE screen renders as nothing. */
    if (unknown.length) bad('unknown identifiers in the template', unknown.join(', '));
    else ok('every identifier resolves', refs.size + ' referenced, ' + FIELDS.length + ' fields declared');

    /* The other direction: a field declared and then never rendered is dead
       weight in a screen whose whole virtue is being small. */
    const rendered = [...refs].filter((r) => FIELDS.includes(r));
    const unused = FIELDS.filter((f) => !tplSrc.includes(f));
    if (unused.length) bad('declared fields the template never uses', unused.join(', '));
    else ok('no dead fields', rendered.length + ' of ' + FIELDS.length + ' rendered directly');
  }

  console.log('\n=== the template and its JavaScript agree on handler names ===');
  {
    /* A rename on either side is silent: the button simply does nothing. */
    const called = [...new Set([...tplSrc.matchAll(/window\.(gts[A-Za-z]*)/g)].map((m) => m[1]))];
    const defined = [...new Set([...jsSrc.matchAll(/window\.(gts[A-Za-z]*)\s*=/g)].map((m) => m[1]))];

    const missing = called.filter((c) => !defined.includes(c));
    if (missing.length) bad('the template calls handlers the JS never defines', missing.join(', '));
    else ok('every handler the template calls exists', called.join(', '));

    const orphan = defined.filter((d) => !called.includes(d));
    if (orphan.length) bad('the JS defines handlers nothing calls', orphan.join(', '));
    else ok('no unreachable handlers', defined.length + ' defined, all called');

    /* addEventListener here would bind to nothing: this file runs before the
       template renders. Every control must be an inline attribute. */
    if (/addEventListener\s*\(\s*['"]click/.test(jsSrc)) {
      bad('simtest.js binds clicks with addEventListener',
          'it runs BEFORE the template renders, so it would bind to nothing');
    } else {
      ok('controls are wired with inline handlers, not addEventListener');
    }
  }

  const browser = await chromium.launch({ executablePath: CHROME, args: ['--no-sandbox'] });

  console.log('\n=== it renders, and stays inside the screen, with no JavaScript ===');
  {
    const CASES = [
      { name: 'en-360-empty', w: 360, h: 800, d: { lang: 'EN' } },
      { name: 'fr-412-result', w: 412, h: 915, d: { lang: 'FR', val: '061414102377', rows: 6, msg: 'Lu : 061414102377' } },
      { name: 'en-412-long', w: 412, h: 915, d: { lang: 'EN', val: '00306141410000000013011061414100001910L2026A1717270131', rows: 3, diag: 'v=20260811k dec=inline routes=fetch:ok,tag:no vid=1280x720 crop=1203x324 frames=41 focus=not adjustable' } },
      { name: 'en-768-result', w: 768, h: 1024, d: { lang: 'EN', val: '061414102377', rows: 12 } },
      { name: 'en-1280-result', w: 1280, h: 900, d: { lang: 'EN', val: '061414102377', rows: 12 } },
    ];

    for (const c of CASES) {
      let html;
      try { html = render(data(c.d)); }
      catch (e) { bad(c.name + ' render failed', e.message.slice(0, 80)); continue; }

      const pg = await browser.newPage({ viewport: { width: c.w, height: c.h } });
      await pg.setContent(pageHtml(html), { waitUntil: 'load' });

      const o = await pg.evaluate(() => {
        const de = document.documentElement;
        const wide = [];
        document.querySelectorAll('.gt-app *').forEach((el) => {
          const r = el.getBoundingClientRect();
          if (r.right > de.clientWidth + 1) {
            wide.push((typeof el.className === 'string' ? el.className : el.tagName) +
                      '@' + Math.round(r.right));
          }
        });
        const vis = (sel) => {
          const el = document.querySelector(sel);
          if (!el) return false;
          const r = el.getBoundingClientRect();
          return r.width > 0 && r.height > 0;
        };
        return {
          sw: de.scrollWidth, cw: de.clientWidth, wide: wide.slice(0, 3),
          keypad: vis('#gts-manual'),
          result: vis('.gts-result'),
          openBtn: vis('#gts-open'),
          /* The stage must stay collapsed with no JS: a dead black rectangle
             reads as a broken camera rather than as an unopened one. */
          stage: vis('#gts-stage'),
          status: (document.querySelector('#gts-status') || {}).textContent || '',
        };
      });

      const overflow = o.sw > o.cw + 1;
      if (overflow) bad(c.name + ' overflows sideways', 'sw=' + o.sw + ' cw=' + o.cw + ' ' + o.wide.join(' | '));
      else ok(c.name + ' fits', c.w + 'px, sw=' + o.sw);

      if (!o.keypad || !o.result || !o.openBtn) {
        bad('  ' + c.name + ' is not usable without JavaScript',
            'keypad=' + o.keypad + ' result=' + o.result + ' camera=' + o.openBtn);
      }
      if (o.stage) bad('  ' + c.name + ' shows a dead camera stage with no JS');
      if (!/JavaScript|JavaScript n/.test(o.status) && !/Javascript/i.test(o.status)) {
        bad('  ' + c.name + ' does not say why the camera is unavailable', o.status.slice(0, 50));
      }

      await pg.screenshot({ path: '/tmp/simtest-' + c.name + '.png', fullPage: true });
      await pg.close();
    }
  }

  /* ---- the server, for the routes the screen may reach for ------------ */
  const routes = {};
  let zxingRequests = 0;
  const server = http.createServer((req, res) => {
    const url = req.url.split('?')[0];
    if (url.indexOf('/profoundui/userdata/ui/') === 0) {
      const rel = url.replace('/profoundui/userdata/ui/', '');
      if (rel.indexOf('gt-zxing') !== -1) zxingRequests++;
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
  const port = await new Promise((r) => server.listen(0, '127.0.0.1', () => r(server.address().port)));
  const base = 'http://127.0.0.1:' + port;
  routes['/screen'] = pageHtml(render(data({ lang: 'EN' })));

  console.log('\n=== a real barcode reaches RPG with every decoder fetch blocked ===');
  {
    /* THE DEVICE'S ACTUAL CONDITION. No BarcodeDetector, and no network route to
       the decoder either -- exactly what the access log showed the iPad doing.
       The only copy available is the base64 one the Genie shim carries in the
       page, so this proves the screen works through the single delivery route
       that is known to reach the device. */
    const dataUrl = await fixture('061414102377', 'code128', 0.42);
    const zxing = fs.readFileSync(path.join(UI, 'gtcommon/gt-zxing.min.js'), 'utf8');

    const pg = await browser.newPage({ viewport: { width: 412, height: 915 },
                                       isMobile: true, hasTouch: true });
    /* Make every fetch of the decoder impossible, by any mechanism. */
    await pg.route('**/gt-zxing.min.js*', (r) => r.abort());
    await pg.addInitScript(PUI_STUB);
    await pg.addInitScript(CAMERA_STUB, { url: dataUrl, portion: 0.42 });
    /* Stand in for the shim's inlined copy, the same way start.html does. */
    await pg.addInitScript((src) => {
      window.__gtLoadZxing = function () {
        if (window.ZXing && window.ZXing.BrowserMultiFormatReader) return true;
        (new Function(src))();
        return !!(window.ZXing && window.ZXing.BrowserMultiFormatReader);
      };
    }, zxing);

    await pg.goto(base + '/screen', { waitUntil: 'load' });
    await pg.addScriptTag({ content: jsSrc });
    await pg.waitForTimeout(150);

    const ready = await pg.evaluate(() => (document.querySelector('#gts-status') || {}).textContent || '');
    if (/Ready/.test(ready)) ok('the no-JavaScript notice is replaced once the JS runs', ready.slice(0, 40));
    else bad('the JS did not take over the status line', ready.slice(0, 60));

    await pg.click('#gts-open');

    let submitted = null;
    for (let i = 0; i < 40 && !submitted; i++) {
      await pg.waitForTimeout(250);
      submitted = await pg.evaluate(() => window.__submitted);
    }

    const diag = await pg.evaluate(() => (document.querySelector('#gts-diagline') || {}).textContent || '');

    if (submitted && submitted.scanval === '061414102377') {
      ok('a barcode filling 42% of the frame decoded and reached RPG',
         'action=' + submitted.action + ' symb=' + submitted.lastsymb);
    } else {
      bad('the barcode never reached RPG', 'submitted=' + JSON.stringify(submitted) + ' | ' + diag);
    }

    if (/dec=inline/.test(diag)) ok('  it used the copy carried in the page', 'dec=inline');
    else bad('  it did not use the inlined decoder', diag.slice(0, 90));

    /* The line on screen and the line sent to RPG are built by different code
       paths, and the on-screen one went stale after a read while the sent one
       was correct. Check both, or half the instrument can lie undetected. */
    if (submitted && /dec=inline/.test(submitted.diag || '')) {
      ok('  the sent report agrees with the screen', 'dec=inline in DIAG');
    } else {
      bad('  the sent report disagrees with the screen', (submitted && submitted.diag || '').slice(0, 90));
    }
    if (/frames=[1-9]/.test(diag)) ok('  the frame count is live after a read', diag.match(/frames=\d+/)[0]);
    else bad('  the details line went stale after a read', diag.slice(0, 90));

    if (submitted && submitted.diag) ok('  the report travels with the scan', 'DIAG is populated on SCAN');
    else bad('  no diagnostics were sent with the scan');

    /* The value must be visible before the round trip repaints, and there must
       be exactly one element named scanval or which value reaches RPG is
       undefined. */
    const mirror = await pg.evaluate(() => ({
      value: (document.querySelector('#gts-manual') || {}).value,
      named: document.querySelectorAll('[name="scanval"]').length,
    }));
    if (mirror.value === '061414102377') ok('  the value is mirrored into the visible field');
    else bad('  the decoded value is not shown before the round trip', String(mirror.value));
    if (mirror.named === 1) ok('  exactly one element is named scanval', '1');
    else bad('  scanval is not unique in the DOM', mirror.named + ' elements');

    /* Closing must actually release the camera: a track left running keeps the
       lens busy and the next screen gets nothing. */
    await pg.evaluate(() => window.gtsClose());
    const stopped = await pg.evaluate(() => ({ stopped: window.__stopped || 0,
                                               tracks: window.__stubTracks || 0,
                                               gum: window.__gum }));
    if (stopped.stopped >= stopped.tracks && stopped.tracks > 0) {
      ok('closing the camera stops every track', stopped.stopped + '/' + stopped.tracks);
    } else {
      bad('closing the camera left a track running', JSON.stringify(stopped));
    }
    if (stopped.gum === 1) ok('one tap opened exactly one camera', '1 getUserMedia call');
    else bad('the camera was opened more than once', stopped.gum + ' calls');

    await pg.close();
  }

  console.log('\n=== the delivery-route probe reports what actually happened ===');
  {
    /* The measurement the main application's bug turned on. Blocking the script
       tag while leaving fetch alone must show up as tag:no,fetch:ok -- if the
       probe cannot tell those apart it is worthless on the device. */
    const dataUrl = await fixture('GT-PROBE-1', 'code128', 0.42);
    const zxing = fs.readFileSync(path.join(UI, 'gtcommon/gt-zxing.min.js'), 'utf8');
    const pg = await browser.newPage({ viewport: { width: 412, height: 915 } });
    await pg.route('**/gt-zxing.min.js*', (r) => r.abort());
    await pg.addInitScript(PUI_STUB);
    await pg.addInitScript(CAMERA_STUB, { url: dataUrl, portion: 0.42 });
    await pg.addInitScript((src) => {
      window.__gtLoadZxing = function () {
        if (window.ZXing && window.ZXing.BrowserMultiFormatReader) return true;
        (new Function(src))();
        return !!(window.ZXing && window.ZXing.BrowserMultiFormatReader);
      };
    }, zxing);
    await pg.route('**/gt-version.js*', (route) => {
      /* Let fetch through, refuse the script tag -- distinguished by the marker
         the probe appends to the tag URL. */
      if (route.request().url().indexOf('-tag') !== -1) return route.abort();
      return route.fulfill({ status: 200, contentType: 'application/javascript',
                             body: "window.__gtStamp='probe';" });
    });
    await pg.goto(base + '/screen', { waitUntil: 'load' });
    await pg.addScriptTag({ content: jsSrc });
    await pg.click('#gts-open').catch(() => {});
    await pg.waitForTimeout(1500);

    const diag = await pg.evaluate(() => (document.querySelector('#gts-diagline') || {}).textContent || '');
    if (/fetch:ok/.test(diag)) ok('the probe reports a working fetch', 'fetch:ok');
    else bad('the probe did not detect the working route', diag.slice(0, 90));
    if (/tag:no/.test(diag)) ok('the probe reports the blocked script tag', 'tag:no -- the iPad symptom');
    else bad('the probe did not detect the blocked route', diag.slice(0, 90));

    await pg.close();
  }

  console.log('\n=== a camera that gives no picture says so, instead of pretending ===');
  {
    const pg = await browser.newPage({ viewport: { width: 412, height: 915 } });
    await pg.addInitScript(PUI_STUB);
    await pg.addInitScript(() => {
      try { delete window.BarcodeDetector; } catch (e) {}
      /* video.play() resolving does NOT mean the video has a picture: metadata
         can arrive later, leaving videoWidth at 0. A 0x0 frame makes every
         decoder fail silently for ever -- a live picture that never reads. */
      Object.defineProperty(HTMLVideoElement.prototype, 'videoWidth', { get: () => 0 });
      Object.defineProperty(HTMLVideoElement.prototype, 'videoHeight', { get: () => 0 });
      navigator.mediaDevices = navigator.mediaDevices || {};
      navigator.mediaDevices.getUserMedia = function () {
        var c = document.createElement('canvas');
        c.width = 320; c.height = 240;
        c.getContext('2d').fillRect(0, 0, 320, 240);
        return Promise.resolve(c.captureStream(5));
      };
    });
    await pg.goto(base + '/screen', { waitUntil: 'load' });
    await pg.addScriptTag({ content: jsSrc });
    await pg.click('#gts-open');
    await pg.waitForTimeout(7500);

    const st = await pg.evaluate(() => ({
      status: (document.querySelector('#gts-status') || {}).textContent || '',
      diag: (document.querySelector('#gts-diagline') || {}).textContent || '',
    }));
    if (/no picture/i.test(st.status)) ok('it reports the empty picture', st.status.slice(0, 55));
    else bad('it did not report an empty picture', st.status.slice(0, 70));
    if (/frames=0/.test(st.diag)) ok('  and did not decode against a 0x0 frame', 'frames=0');
    else bad('  it decoded against a 0x0 frame', st.diag.slice(0, 70));

    await pg.close();
  }

  console.log('\n=== a photo decodes through the real file input ===');
  {
    /* The path that does not depend on the live stream at all. On iOS the native
       camera focuses and exposes where a getUserMedia stream may not, so if the
       live camera shows a picture and never reads, this is the fallback. */
    const png = await bwip.toBuffer({ bcid: 'qrcode', text: 'GT|SIMPLE|TEST',
                                      scale: 6, includetext: false,
                                      paddingwidth: 10, paddingheight: 10 });
    const file = '/tmp/simtest-photo.png';
    fs.writeFileSync(file, png);
    const zxing = fs.readFileSync(path.join(UI, 'gtcommon/gt-zxing.min.js'), 'utf8');

    const pg = await browser.newPage({ viewport: { width: 412, height: 915 } });
    await pg.route('**/gt-zxing.min.js*', (r) => r.abort());
    await pg.addInitScript(PUI_STUB);
    await pg.addInitScript(() => { try { delete window.BarcodeDetector; } catch (e) {} });
    await pg.addInitScript((src) => {
      window.__gtLoadZxing = function () {
        if (window.ZXing && window.ZXing.BrowserMultiFormatReader) return true;
        (new Function(src))();
        return !!(window.ZXing && window.ZXing.BrowserMultiFormatReader);
      };
    }, zxing);
    await pg.goto(base + '/screen', { waitUntil: 'load' });
    await pg.addScriptTag({ content: jsSrc });
    await pg.waitForTimeout(150);

    const wrapVisible = await pg.evaluate(() => {
      const w = document.querySelector('#gts-photowrap');
      return !!w && !w.hidden;
    });
    if (wrapVisible) ok('the photo control appears once the JS is running');
    else bad('the photo control stayed hidden with the JS loaded');

    await pg.setInputFiles('#gts-photo', file);

    let submitted = null;
    for (let i = 0; i < 30 && !submitted; i++) {
      await pg.waitForTimeout(200);
      submitted = await pg.evaluate(() => window.__submitted);
    }
    if (submitted && submitted.scanval === 'GT|SIMPLE|TEST') {
      ok('a photographed barcode reached RPG', 'symb=' + submitted.lastsymb);
    } else {
      bad('the photo never decoded', JSON.stringify(submitted));
    }

    await pg.close();
  }

  console.log('\n=== the decoder was never fetched over the network ===');
  if (zxingRequests === 0) {
    ok('no test relied on a decoder fetch', 'the device cannot make one either');
  } else {
    bad('a decoder fetch succeeded during the run', zxingRequests + ' request(s)');
  }

  await browser.close();
  server.close();

  console.log('\n' + (fail
    ? fail + ' failed, ' + pass + ' passed'
    : 'The simple camera test holds up: ' + pass + ' passed, 0 failed'));
  process.exit(fail ? 1 : 0);
})();
