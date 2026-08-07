/* ==================================================================
 * test-rcv-stepper -- click the receiving quantity controls for real.
 *
 * The native number-input spinners were replaced with a 48px minus, a 48px
 * plus, and a per-line "receive in full". Whether they LOOK right is the
 * pre-flight's job; whether they DO the right thing is this file's, and it
 * cannot be answered by a screenshot:
 *
 *   - the step is the item's case pack, not 1
 *   - minus stops at zero and never goes negative
 *   - plus is NOT clamped at expected, because an overage is real
 *   - "in full" jumps straight to the expected quantity
 *   - none of them round-trips to RPG (48 cases must not be 48 submits)
 *
 * The REAL gt-rcvlines.js is served and executed, not a copy of its logic.
 *
 * Run: node gtwms/tools/test-rcv-stepper.js
 * ================================================================== */

const ejs = require('ejs');
const fs = require('fs');
const http = require('http');
const path = require('path');
const { chromium } = require('playwright-core');

const UI = '/workspace/workspace/ibmi-agentic/docs/htdocs/profoundui/userdata/ui';
const CH = '/home/coder/.cache/ms-playwright/chromium-1228/chrome-linux64/chrome';

/* Line 1 has a case pack of 24 and 24 expected; line 2 a pack of 12, 48
 * expected and 36 already received; line 4 a pack of 1 and nothing received. */
const DATA = {
  action: '', scanval: '', langpref: 'EN', opername: 'James Okonkwo',
  msg: '', msgkind: '',
  hrcpt: 'RC00000001', hpo: 'PO-2026-0481', hvend: 'Maple Leaf Distribution',
  hdoor: 'D04', hstat: 'PART', hsscc: '006141410000000185',
  nlines: 3, ndone: 2, nvar: 1, nphoto: 0, hitline: 0,
  linsfl: [
    { _rrn: 1, lseq: 1, lline: 1, lsku: 'HLTH-000141', ldesc: 'Toothpaste - 12 Pack',
      ldept: 'HLTH', luom: 'EA', limg: 0, lexp: 24, lrcv: 24, lvar: 0,
      lstat: 'MTCH', lreason: '', lphoto: 0, lcase: 24 },
    { _rrn: 2, lseq: 2, lline: 2, lsku: 'GROC-000004', ldesc: 'Rice - 1 kg',
      ldept: 'GROC', luom: 'EA', limg: 0, lexp: 48, lrcv: 36, lvar: -12,
      lstat: 'SHRT', lreason: '', lphoto: 0, lcase: 12 },
    { _rrn: 3, lseq: 3, lline: 3, lsku: 'HOME-000077', ldesc: 'Bed Sheet Set',
      ldept: 'HOME', luom: 'EA', limg: 0, lexp: 6, lrcv: 0, lvar: 0,
      lstat: 'OPEN', lreason: '', lphoto: 0, lcase: 1 }
  ]
};

let fail = 0;
function check(name, ok, detail) {
  if (!ok) fail++;
  console.log('  ' + (ok ? 'PASS' : 'FAIL') + '  ' + name + (detail ? '  -- ' + detail : ''));
}

function serve(html) {
  const files = {
    '/gt-theme.css': [path.join(UI, 'gtcommon/gt-theme.css'), 'text/css'],
    '/rcvlines.css': [path.join(UI, 'gtrcld/rcvlines.css'), 'text/css'],
    '/gt-rcvlines.js': [path.join(UI, 'gtcommon/gt-rcvlines.js'), 'text/javascript']
  };
  /* Genie's hostile globals first, exactly as the skin applies them. */
  const page = '<!doctype html><meta charset="utf-8">' +
    '<style>html,body{margin:0;padding:0}div{white-space:nowrap;z-index:10;padding:1px}</style>' +
    '<link rel="stylesheet" href="/gt-theme.css"><link rel="stylesheet" href="/rcvlines.css">' +
    html + '<script src="/gt-rcvlines.js"></script>';

  const server = http.createServer((req, res) => {
    const url = req.url.split('?')[0];
    if (url === '/') {
      res.writeHead(200, { 'content-type': 'text/html; charset=utf-8' });
      return res.end(page);
    }
    if (files[url]) {
      res.writeHead(200, { 'content-type': files[url][1] + '; charset=utf-8' });
      return res.end(fs.readFileSync(files[url][0]));
    }
    res.writeHead(404); res.end();
  });
  return new Promise((resolve) => {
    server.listen(0, '127.0.0.1', () => resolve({ server, port: server.address().port }));
  });
}

const qty = (page, n) => page.evaluate((i) =>
  document.querySelectorAll('.gt-lin-row')[i].querySelector('.gt-lin-qty').value, n);

(async () => {
  const src = fs.readFileSync(path.join(UI, 'gtrcld', 'rcvlines.ejs'), 'utf8');
  const { server, port } = await serve(ejs.render(src, DATA));
  const browser = await chromium.launch({ executablePath: CH, args: ['--no-sandbox'] });
  const page = await browser.newPage({ viewport: { width: 412, height: 915 } });

  /* Nothing here may submit. pui.submit is stubbed to count calls rather than
   * being absent, so a stray submit is caught instead of throwing. */
  await page.addInitScript(() => {
    window.__submits = 0;
    window.pui = { submit: function () { window.__submits++; } };
  });
  await page.goto('http://127.0.0.1:' + port + '/', { waitUntil: 'load' });

  check('screen JS installed', await page.evaluate(() =>
    !!(window.gtRcvLines && window.gtRcvLines.__installed)));

  /* ---- the native spinners are gone ---- */
  const spin = await page.evaluate(() => {
    const box = document.querySelector('.gt-lin-qty');
    return { appearance: getComputedStyle(box).appearance,
             h: Math.round(box.getBoundingClientRect().height) };
  });
  check('native spinner suppressed', spin.appearance === 'textfield',
        'appearance=' + spin.appearance);
  check('quantity box is a 48px target', spin.h >= 48, spin.h + 'px');

  /* ---- step sizes come from the case pack ---- */
  const steps = await page.evaluate(() =>
    [...document.querySelectorAll('.gt-lin-qty')].map((b) => b.getAttribute('data-gt-step')));
  check('step is the case pack per line', steps.join(',') === '24,12,1', steps.join(','));

  /* ---- plus steps by a case, and is NOT clamped at expected ---- */
  const rows = page.locator('.gt-lin-row');
  await rows.nth(0).locator('.gt-step-up').click();
  check('plus adds one case', (await qty(page, 0)) === '48.00', await qty(page, 0));
  check('plus is not clamped at expected (overage is real)',
        Number(await qty(page, 0)) > 24);

  /* ---- minus steps by a case ---- */
  await rows.nth(1).locator('.gt-step-down').click();
  check('minus subtracts one case', (await qty(page, 1)) === '24.00', await qty(page, 1));

  /* ---- minus stops at zero ---- */
  for (let i = 0; i < 4; i++) await rows.nth(2).locator('.gt-step-down').click();
  check('minus never goes negative', (await qty(page, 2)) === '0.00', await qty(page, 2));

  /* ---- receive in full ---- */
  await rows.nth(2).locator('.gt-lin-full').click();
  check('"in full" jumps to expected', (await qty(page, 2)) === '6.00', await qty(page, 2));
  await rows.nth(1).locator('.gt-lin-full').click();
  check('"in full" works on a part-received line', (await qty(page, 1)) === '48.00',
        await qty(page, 1));

  /* ---- and none of it went to the server ---- */
  const submits = await page.evaluate(() => window.__submits);
  check('no round trip for any of the 9 taps', submits === 0, submits + ' submits');

  /* ---- THE PAYLOAD -------------------------------------------------
   * The bug this covers: in an EJS screen the subfile rows are rendered by the
   * template, so every box carries the same name with no record number and the
   * runtime cannot build its {SUBFILE}.rrn marker. READC saw nothing and Save
   * answered "no quantities were changed" for every edit. The changed rows now
   * travel in one field as "seq:qty;".
   *
   * State at this point: row 1 stepped 24 -> 48, row 2 in-full to 48 (from 36),
   * row 3 stepped to 0 then in-full to 6.
   * ---------------------------------------------------------------- */
  const payload = await page.evaluate(() => window.gtRcvLines.collect());
  check('payload carries every changed row, and the row index',
        payload === '1:48.00;2:48.00;3:6.00;', JSON.stringify(payload));

  /* Only what changed. Reset row 2 to exactly what the server rendered and it
   * must drop out -- this is what keeps updated_by off untouched lines. */
  await page.evaluate(() => {
    const b = document.querySelectorAll('.gt-lin-qty')[1];
    b.value = b.getAttribute('data-gt-orig');
  });
  check('a box put back to its original drops out of the payload',
        (await page.evaluate(() => window.gtRcvLines.collect())) === '1:48.00;3:6.00;',
        await page.evaluate(() => window.gtRcvLines.collect()));

  /* An empty box is somebody mid-edit, not a zero. Sending 0 would silently
   * receive nothing for that line. */
  await page.evaluate(() => { document.querySelectorAll('.gt-lin-qty')[0].value = ''; });
  check('an empty box is skipped rather than sent as zero',
        (await page.evaluate(() => window.gtRcvLines.collect())) === '3:6.00;',
        await page.evaluate(() => window.gtRcvLines.collect()));

  /* Save submits once, with the payload on the action and in the hidden field. */
  await page.evaluate(() => { window.__last = null;
    window.pui.submit = function (o) { window.__submits++; window.__last = o; }; });
  /* .gt-lin-acts, not just .gt-btn-primary -- the scan row's "Go" is the first
   * primary button on the screen and this test clicked that instead. */
  await page.click('.gt-lin-acts .gt-btn-primary');
  const sub = await page.evaluate(() => ({
    last: window.__last, n: window.__submits,
    hidden: document.getElementById('gt-qtyedits').value
  }));
  check('Save submits APPLY once with the payload', sub.n === 1 &&
        sub.last && sub.last.action === 'APPLY' && sub.last.qtyedits === '3:6.00;',
        JSON.stringify(sub.last));
  check('the hidden field carries the same payload', sub.hidden === '3:6.00;',
        JSON.stringify(sub.hidden));

  /* Nothing changed -> an empty payload, so RPG can say so rather than
   * writing a receipt's worth of rows for no reason. */
  await page.evaluate(() => {
    document.querySelectorAll('.gt-lin-qty').forEach((b) => {
      b.value = b.getAttribute('data-gt-orig');
    });
  });
  check('no edits produces an empty payload',
        (await page.evaluate(() => window.gtRcvLines.collect())) === '',
        JSON.stringify(await page.evaluate(() => window.gtRcvLines.collect())));

  /* ---- THE PINNED BAND ---------------------------------------------
   * The camera button and the scan box must stay in frame while the lines
   * scroll underneath. position:sticky fails SILENTLY -- one ancestor with
   * `overflow` other than visible and it simply does not stick, with no
   * warning anywhere -- so this scrolls the page for real and measures.
   * ---------------------------------------------------------------- */
  const before = await page.evaluate(() => {
    const r = (sel) => document.querySelector(sel).getBoundingClientRect();
    return { cam: Math.round(r('#gt-scan-start').top),
             box: Math.round(r('#gt-scan-manual').top),
             bandH: Math.round(r('.gt-rcv-top').height) };
  });

  await page.evaluate(() => window.scrollTo(0, document.body.scrollHeight));
  await page.waitForTimeout(120);

  const after = await page.evaluate(() => {
    const de = document.documentElement;
    const r = (sel) => document.querySelector(sel).getBoundingClientRect();
    const cam = r('#gt-scan-start');
    const box = r('#gt-scan-manual');
    const band = r('.gt-rcv-top');
    /* Hit-test just below the band: whatever is there must be a LINE, proving
     * the cards pass underneath rather than over. */
    const under = document.elementFromPoint(de.clientWidth / 2, band.bottom + 6);
    return {
      scrolled: Math.round(window.scrollY),
      camTop: Math.round(cam.top), camVisible: cam.top >= 0 && cam.bottom <= innerHeight,
      boxTop: Math.round(box.top), boxVisible: box.top >= 0 && box.bottom <= innerHeight,
      bandTop: Math.round(band.top),
      /* Anything drawn OVER the band would be found at its own midpoint. */
      onTop: document.querySelector('.gt-rcv-top')
               .contains(document.elementFromPoint(de.clientWidth / 2, band.bottom - 8)),
      underneath: under ? (under.closest('.gt-lin-row') ? 'a line card' : under.className) : 'nothing'
    };
  });

  check('the page actually scrolled', after.scrolled > 200, after.scrolled + 'px');
  check('camera button stays in frame at the top', after.camVisible && after.camTop < 200,
        'top ' + before.cam + 'px -> ' + after.camTop + 'px');
  check('scan box stays in frame at the top', after.boxVisible && after.boxTop < 200,
        'top ' + before.box + 'px -> ' + after.boxTop + 'px');
  check('the band is pinned to the top', after.bandTop <= 1, 'top ' + after.bandTop + 'px');
  check('nothing is drawn over the band', after.onTop === true);
  check('the lines scroll underneath it', after.underneath === 'a line card',
        after.underneath);
  check('the band leaves the list room', before.bandH < 200, before.bandH + 'px tall');

  await page.evaluate(() => window.scrollTo(0, 0));

  /* ---- the buttons must not be reachable on a posted receipt ---- */
  await page.close();
  const posted = await browser.newPage({ viewport: { width: 412, height: 915 } });
  const { server: s2, port: p2 } = await serve(
    ejs.render(src, Object.assign({}, DATA, { hstat: 'POST' })));
  await posted.goto('http://127.0.0.1:' + p2 + '/', { waitUntil: 'load' });
  const n = await posted.evaluate(() => ({
    steps: document.querySelectorAll('.gt-step').length,
    boxes: document.querySelectorAll('.gt-lin-qty').length,
    full: document.querySelectorAll('.gt-lin-full').length
  }));
  check('posted receipt offers no editing controls',
        n.steps === 0 && n.boxes === 0 && n.full === 0,
        JSON.stringify(n));
  await posted.close();
  s2.close();

  await browser.close();
  server.close();
  console.log(fail ? '\n' + fail + ' check(s) FAILED' : '\nAll checks passed');
  process.exit(fail ? 1 : 0);
})();
