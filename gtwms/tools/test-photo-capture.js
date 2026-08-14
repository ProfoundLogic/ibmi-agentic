/* ==================================================================
 * test-photo-capture -- drive the camera capture flow for real.
 *
 * The camera button used to hand off to <input type="file">, which on a phone
 * showed a "camera or gallery?" chooser and on a desktop a file dialog. It now
 * opens a live getUserMedia stream with its own shutter. That is a behaviour
 * change no screenshot can confirm, so this exercises it end to end:
 *
 *   1. the button opens a LIVE camera, and NO file chooser is ever raised
 *   2. the shutter produces a still that fits IMGDATA's char(24000)
 *   3. with no camera present it degrades to the file fallback -- and still
 *      does not pop a file dialog on its own
 *
 * Chromium supplies a synthetic camera via --use-fake-device-for-media-stream,
 * so this runs headless with no hardware. The page is served over
 * http://127.0.0.1 rather than set as content, because getUserMedia is refused
 * outside a secure context and about:blank is not reliably one.
 *
 * The REAL gt-photo.js and gt-carousel.js are served and executed here -- this
 * tests the shipped files, not a copy of their logic.
 *
 * Run: node gtwms/tools/test-photo-capture.js
 * ================================================================== */

const ejs = require('ejs');
const fs = require('fs');
const http = require('http');
const path = require('path');
const { chromium } = require('playwright-core');

const UI = '/workspace/workspace/ibmi-agentic/docs/htdocs/profoundui/userdata/ui';
const CH = '/home/coder/.cache/ms-playwright/chromium-1228/chrome-linux64/chrome';
const SHOT = '/tmp/ejspre';

/* Same shape the display file hands the template. Two catalogue images so the
 * carousel has a strip and its arrows are live. */
const DATA = {
  action: '', langpref: 'EN', opername: 'Marie Tremblay', msg: '',
  dsku: 'HLTH-000138', ddesc: 'Toothpaste - 4 Pack', ddept: 'HLTH', duom: 'EA',
  dcasepk: 24, dweight: 0.349, dlen: 9.5, dwid: 10.4, dhgt: 9.6, dretail: 3.25,
  dupc: '0614141001385', dcase: '10614141001382',
  dtotoh: 770, dtotav: 770, dbulk: 665, dnlocs: 3,
  dimg1: 149, dimg2: 150, dimg3: 0, dimg4: 0,
  dimg5: 0, dimg6: 0, dimg7: 0, dimg8: 0,
  dcap1: 'Toothpaste - Front', dcap2: 'Toothpaste - Detail',
  dcap3: '', dcap4: '', dcap5: '', dcap6: '', dcap7: '', dcap8: '',
  dsrc1: 'C', dsrc2: 'C', dsrc3: '', dsrc4: '',
  dsrc5: '', dsrc6: '', dsrc7: '', dsrc8: '',
  imgdata: '',
  locsfl: [
    { _rrn: 1, lseq: 1, lloc: 'D01012', ltype: 'BULK', lzone: 'D', lqty: 312, lalloc: 0 },
    { _rrn: 2, lseq: 2, lloc: 'E03074', ltype: 'BULK', lzone: 'E', lqty: 353, lalloc: 0 },
    { _rrn: 3, lseq: 3, lloc: 'A08121', ltype: 'PICK', lzone: 'A', lqty: 105, lalloc: 0 }
  ]
};

/* IMGDATA is char(24000); gt-photo.js keeps base64 under 23000. */
const MAX_B64 = 23000;

let fail = 0;
function check(name, ok, detail) {
  if (!ok) fail++;
  console.log('  ' + (ok ? 'PASS' : 'FAIL') + '  ' + name + (detail ? '  -- ' + detail : ''));
}

/* Serve the rendered screen plus its real assets. Genie's hostile
 * `div{white-space:nowrap;z-index:10;padding:1px}` is injected first, exactly
 * as the skin does, so the button's stacking is tested under it. */
function serve(html) {
  const files = {
    '/gt-theme.css': [path.join(UI, 'gtcommon/gt-theme.css'), 'text/css'],
    '/itmdetl.css': [path.join(UI, 'gtitdd/itmdetl.css'), 'text/css'],
    '/gt-carousel.js': [path.join(UI, 'gtcommon/gt-carousel.js'), 'text/javascript'],
    '/gt-photo.js': [path.join(UI, 'gtcommon/gt-photo.js'), 'text/javascript']
  };
  const page = '<!doctype html><meta charset="utf-8">' +
    '<style>html,body{margin:0;padding:0}div{white-space:nowrap;z-index:10;padding:1px}</style>' +
    '<link rel="stylesheet" href="/gt-theme.css"><link rel="stylesheet" href="/itmdetl.css">' +
    html +
    '<script src="/gt-carousel.js"></script><script src="/gt-photo.js"></script>';

  const server = http.createServer((req, res) => {
    const url = req.url.split('?')[0];
    if (url === '/' ) {
      res.writeHead(200, { 'content-type': 'text/html; charset=utf-8' });
      return res.end(page);
    }
    if (files[url]) {
      res.writeHead(200, { 'content-type': files[url][1] + '; charset=utf-8' });
      return res.end(fs.readFileSync(files[url][0]));
    }
    /* Product imagery is not served here; a broken <img> is fine. */
    res.writeHead(404); res.end();
  });
  return new Promise((resolve) => {
    server.listen(0, '127.0.0.1', () => resolve({ server, port: server.address().port }));
  });
}

(async () => {
  fs.mkdirSync(SHOT, { recursive: true });
  const src = fs.readFileSync(path.join(UI, 'gtitdd', 'itmdetl.ejs'), 'utf8');
  const html = ejs.render(src, DATA);
  const { server, port } = await serve(html);
  const base = 'http://127.0.0.1:' + port + '/';

  const browser = await chromium.launch({
    executablePath: CH,
    args: ['--no-sandbox',
           '--use-fake-device-for-media-stream',   /* synthetic camera */
           '--use-fake-ui-for-media-stream']       /* auto-grant, no prompt */
  });

  /* ---- Case 1: live camera ------------------------------------- */
  console.log('\ncase: camera available (S24 Ultra viewport)');
  {
    const page = await browser.newPage({ viewport: { width: 412, height: 915 } });

    /* The whole point of the change: no file dialog, ever. */
    let chooserFired = false;
    page.on('filechooser', () => { chooserFired = true; });

    await page.goto(base, { waitUntil: 'load' });

    const addBox = await page.locator('.gt-car-add').boundingBox();
    check('camera button is in the carousel bottom-LEFT corner',
          !!addBox && addBox.x < 60,
          addBox ? 'x=' + Math.round(addBox.x) + ' y=' + Math.round(addBox.y) : 'not found');

    /* SCROLL TO THE BOTTOM FIRST.
     *
     * The overlay is position:absolute, so its containing block is the whole
     * document. Opening it from the top of a short page cannot see the bug the
     * operator hit on Cycle Count: the panel is centred in an overlay as tall as
     * the page, so it lands halfway down the DOCUMENT rather than in the
     * viewport. Item Detail shares the same overlay, so it must be checked here
     * too. */
    await page.evaluate(() => window.scrollTo(0, document.body.scrollHeight));
    await page.waitForTimeout(120);
    const scrolledTo = await page.evaluate(() => Math.round(window.scrollY));

    await page.click('.gt-car-add');
    let live = true;
    try {
      await page.waitForSelector('.gt-photo-overlay.is-open.is-live', { timeout: 8000 });
    } catch (e) { live = false; }
    check('camera button opens a LIVE camera', live,
          live ? await page.locator('#gt-photo-status').textContent() : 'never went live');
    /* Present is not the same as findable. */
    const inView = await page.evaluate(() => {
      const panel = document.querySelector('.gt-photo-panel');
      if (!panel) return { err: 'no .gt-photo-panel' };
      const r = panel.getBoundingClientRect();
      return { top: Math.round(r.top), bottom: Math.round(r.bottom),
               vh: window.innerHeight };
    });
    check('camera opens IN VIEW, not down the page',
          !inView.err && inView.bottom > 0 && inView.top < inView.vh &&
          inView.top > -5,
          inView.err ||
            ('scrolled ' + scrolledTo + 'px, panel top=' + inView.top +
             ' bottom=' + inView.bottom + ' viewport=' + inView.vh));

    check('no file chooser was raised', !chooserFired,
          chooserFired ? 'a file dialog opened' : '');

    const vid = await page.evaluate(() => {
      const v = document.getElementById('gt-photo-video');
      return { w: v.videoWidth, h: v.videoHeight, playing: !v.paused,
               shown: getComputedStyle(v).display !== 'none' };
    });
    check('live video is visible and streaming',
          vid.shown && vid.playing && vid.w > 0,
          vid.w + '×' + vid.h + ' playing=' + vid.playing);
    check('shutter and close-camera controls are visible',
          await page.locator('#gt-photo-shutter').isVisible() &&
          await page.locator('#gt-photo-stop').isVisible());
    /* Retake means nothing until there is a shot to retake. This caught a real
     * bug: the CSS targeted .gt-photo-retake but the button carried only the
     * matching id, so the rule never applied and Retake sat there while the
     * camera was still live. */
    check('Retake is hidden while the camera is live',
          !(await page.locator('#gt-photo-retake').isVisible()));
    await page.screenshot({ path: SHOT + '/photo-1-live.png' });

    /* ---- shutter ---- */
    await page.click('#gt-photo-shutter');
    let shot = true;
    try {
      await page.waitForSelector('.gt-photo-overlay.is-shot', { timeout: 8000 });
    } catch (e) { shot = false; }
    check('shutter captures a still', shot);

    const out = await page.evaluate(() => {
      const img = document.getElementById('gt-photo-preview');
      const v = document.getElementById('gt-photo-video');
      return {
        src: (img.getAttribute('src') || '').slice(0, 23),
        b64len: (img.getAttribute('src') || '').split(',')[1] ?
                (img.getAttribute('src') || '').split(',')[1].length : 0,
        imgShown: getComputedStyle(img).display !== 'none',
        vidShown: getComputedStyle(v).display !== 'none',
        useEnabled: !document.getElementById('gt-photo-use').disabled,
        status: document.getElementById('gt-photo-status').textContent
      };
    });
    check('still is a JPEG data URL', out.src.indexOf('data:image/jpeg') === 0, out.src + '…');
    check('payload fits IMGDATA char(24000)', out.b64len > 0 && out.b64len <= MAX_B64,
          out.b64len + ' base64 chars (limit ' + MAX_B64 + ')');
    check('still replaces the live view', out.imgShown && !out.vidShown);
    check('"Use this photo" becomes enabled', out.useEnabled, out.status);
    check('Retake appears once there is a still',
          await page.locator('#gt-photo-retake').isVisible());
    await page.screenshot({ path: SHOT + '/photo-2-captured.png' });

    /* Camera must be released once the shot is taken. */
    const released = await page.evaluate(() =>
      !document.getElementById('gt-photo-video').srcObject);
    check('camera is released after capture', released);

    /* ---- retake goes back to live ---- */
    await page.click('#gt-photo-retake');
    let back = true;
    try { await page.waitForSelector('.gt-photo-overlay.is-live', { timeout: 8000 }); }
    catch (e) { back = false; }
    check('retake returns to the live camera', back);
    check('retake raised no file chooser', !chooserFired);

    /* ---- close stops the camera ---- */
    await page.click('#gt-photo-stop');
    const closed = await page.evaluate(() => {
      const o = document.getElementById('gt-photo-overlay');
      return { open: o.classList.contains('is-open'),
               stream: !!document.getElementById('gt-photo-video').srcObject };
    });
    check('close-camera closes the panel and stops the stream',
          !closed.open && !closed.stream);
    await page.close();
  }

  /* ---- Case 2: no camera on the device ------------------------- */
  console.log('\ncase: no camera present (desktop demo machine)');
  {
    const page = await browser.newPage({ viewport: { width: 1280, height: 900 } });
    let chooserFired = false;
    page.on('filechooser', () => { chooserFired = true; });

    /* Exercise the real branch: navigator.mediaDevices absent. */
    await page.addInitScript(() => {
      try { Object.defineProperty(navigator, 'mediaDevices', { get: () => undefined }); }
      catch (e) { delete navigator.mediaDevices; }
    });
    await page.goto(base, { waitUntil: 'load' });
    /* SCROLL TO THE BOTTOM FIRST.
     *
     * The overlay is position:absolute, so its containing block is the whole
     * document. Opening it from the top of a short page cannot see the bug the
     * operator hit on Cycle Count: the panel is centred in an overlay as tall as
     * the page, so it lands halfway down the DOCUMENT rather than in the
     * viewport. Item Detail shares the same overlay, so it must be checked here
     * too. */
    await page.evaluate(() => window.scrollTo(0, document.body.scrollHeight));
    await page.waitForTimeout(120);
    const scrolledTo = await page.evaluate(() => Math.round(window.scrollY));

    await page.click('.gt-car-add');
    await page.waitForSelector('.gt-photo-overlay.is-open.is-fallback', { timeout: 8000 })
      .catch(() => {});

    const fb = await page.evaluate(() => {
      const o = document.getElementById('gt-photo-overlay');
      return { open: o.classList.contains('is-open'),
               fallback: o.classList.contains('is-fallback'),
               live: o.classList.contains('is-live'),
               chooseShown: getComputedStyle(document.getElementById('gt-photo-choose')).display !== 'none',
               status: document.getElementById('gt-photo-status').textContent };
    });
    check('falls back when there is no camera', fb.open && fb.fallback && !fb.live, fb.status);
    check('offers the file fallback explicitly', fb.chooseShown);
    check('does NOT open a file dialog by itself', !chooserFired,
          chooserFired ? 'a file dialog opened unasked' : '');
    await page.screenshot({ path: SHOT + '/photo-3-fallback.png' });
    await page.close();
  }

  await browser.close();
  server.close();
  console.log(fail ? '\n' + fail + ' check(s) FAILED' : '\nAll checks passed');
  process.exit(fail ? 1 : 0);
})();
