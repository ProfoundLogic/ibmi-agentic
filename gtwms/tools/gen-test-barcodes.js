#!/usr/bin/env node
/*
 * SUPERSEDED by gen-demo-sheet.js / verify-demo-sheet.js.
 *
 * This pair needs a hand-supplied `aitool sql` dump at /tmp/barcodes.json, which
 * is ephemeral -- so on any fresh container it simply crashes with ENOENT rather
 * than telling you what it wants. The demo-sheet pair queries the database
 * itself, covers 32 barcodes across every act of the script, and the verifier
 * derives its work list from the generator so it cannot silently cover less.
 *
 * Kept because the layout work here is still useful reference. Neither
 * rebuild-all.sh nor the playbook's test list runs it. Pass an input file
 * explicitly if you want it:
 *     node gtwms/tools/gen-test-barcodes.js rows.json out.pdf
 */
/*
 * GT Warehouse Mobile -- printable barcode test sheet.
 *
 *   node gtwms/tools/gen-test-barcodes.js <rows.json> <out.pdf>
 *
 * Produces a PDF of REAL, scannable barcodes for testing Scan Lab and every
 * later screen on a physical device.
 *
 * The values are pulled from the seeded database rather than invented, so
 * scanning one produces a meaningful result in the app instead of a shrug.
 * The input JSON is the output of an `aitool sql` query -- see the header of
 * the accompanying summary for the statement.
 *
 * Two things matter for a sheet that actually scans:
 *
 *   1. Module width and quiet zone. Phone cameras need a wider bar and more
 *      white space around it than a laser scanner does. Everything here is
 *      rendered generously rather than at minimum spec.
 *   2. Real check digits. Every UPC-A, EAN-13, ITF-14 and SSCC in the seed is
 *      computed, so a scanner accepts them. The one deliberately corrupted
 *      barcode is labelled as such -- it is the negative test.
 */

'use strict';

const fs = require('fs');
const path = require('path');
const bwipjs = require('bwip-js');
const { chromium } = require('playwright-core');

const CHROME = '/home/coder/.cache/ms-playwright/chromium-1228/chrome-linux64/chrome';

const inFile = process.argv[2] || '/tmp/barcodes.json';
const outFile = process.argv[3] || '/task-output/GT-barcode-test-sheet.pdf';

const raw = JSON.parse(fs.readFileSync(inFile, 'utf8'));
const rows = (raw.data && raw.data.rows) || raw.rows || [];
const byCat = (c) => rows.filter((r) => r.CAT === c);

/* bwip-js emits a viewBox but NO width/height attributes. Inline in HTML that
 * collapses to zero height and the barcode prints as an empty box -- which is
 * exactly what happened the first time, and is invisible until you try to scan
 * the sheet. Derive explicit dimensions from the viewBox so `height: auto`
 * has an aspect ratio to work from. */
function sizeSvg(markup) {
  const m = markup.match(/viewBox="0 0 ([0-9.]+) ([0-9.]+)"/);
  if (!m) return markup;
  return markup.replace('<svg ', `<svg width="${m[1]}" height="${m[2]}" `);
}

/* ---- GS1 helpers -------------------------------------------------- */
function gs1Check(payload) {
  let sum = 0;
  for (let i = 0; i < payload.length; i++) {
    const d = Number(payload[payload.length - 1 - i]);
    sum += d * (i % 2 === 0 ? 3 : 1);
  }
  return String((10 - (sum % 10)) % 10);
}

/* ---- Barcode rendering -------------------------------------------- */
const MATRIX = ['qrcode', 'datamatrix', 'azteccode'];

async function svg(bcid, text, opts) {
  const cfg = Object.assign({
    bcid: bcid,
    text: text,
    /* Generous scale: a phone camera at arm's length needs a wider module
     * than a contact laser scanner. */
    scale: 3,
    height: 16,
    includetext: false,
    paddingwidth: 4,
    paddingheight: 2,
  }, opts || {});

  /* Matrix symbologies size themselves from `scale`; passing a linear bar
   * height (or a zero width) makes bwip-js reject the render outright. */
  if (MATRIX.indexOf(bcid) !== -1) {
    delete cfg.height;
    delete cfg.width;
  }
  return sizeSvg(bwipjs.toSVG(cfg));
}

function card(title, sub, expect, svgMarkup, human, note) {
  return `
    <div class="card">
      <div class="card-head">
        <span class="card-title">${esc(title)}</span>
        ${sub ? `<span class="card-sub">${esc(sub)}</span>` : ''}
      </div>
      <div class="bc">${svgMarkup}</div>
      <div class="human">${esc(human)}</div>
      <div class="expect"><strong>App should say:</strong> ${esc(expect)}</div>
      ${note ? `<div class="note">${esc(note)}</div>` : ''}
    </div>`;
}

function esc(s) {
  return String(s == null ? '' : s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
}

(async () => {
  const sections = [];

  /* ---- 1. Item barcodes: UPC-A ------------------------------------ */
  {
    const items = byCat('UPC').slice(0, 6);
    const cards = [];
    for (const r of items) {
      cards.push(card('UPC-A', 'Consumer unit',
        'An item — ' + r.LBL1.trim(),
        await svg('upca', r.CODE.trim()),
        r.CODE.trim(), 'SKU ' + r.LBL2.trim()));
    }
    sections.push({
      title: '1 · Item barcodes (UPC-A)',
      blurb: 'The everyday retail barcode. Scanning one in Scan Lab resolves to an item; in Item Lookup it will open the product.',
      cards
    });
  }

  /* ---- 2. Case codes: ITF-14 -------------------------------------- */
  {
    const items = byCat('ITF').slice(0, 4);
    const cards = [];
    for (const r of items) {
      cards.push(card('ITF-14', 'Case / carton',
        'An item — ' + r.LBL1.trim(),
        await svg('itf14', r.CODE.trim(), { height: 18 }),
        r.CODE.trim(), r.LBL2.trim()));
    }
    sections.push({
      title: '2 · Case codes (ITF-14)',
      blurb: 'What receiving actually scans off a carton. Resolves to the same item as its UPC, but reports pack quantity and level "Case".',
      cards
    });
  }

  /* ---- 3. EAN-13 --------------------------------------------------- */
  {
    const items = byCat('EAN').slice(0, 2);
    const cards = [];
    for (const r of items) {
      cards.push(card('EAN-13', 'Imported goods',
        'An item — ' + r.LBL1.trim(),
        await svg('ean13', r.CODE.trim()),
        r.CODE.trim(), 'SKU ' + r.LBL2.trim()));
    }
    sections.push({ title: '3 · International (EAN-13)', blurb: 'Same resolution path, different symbology.', cards });
  }

  /* ---- 4. Locations: Code 128 and QR ------------------------------ */
  {
    const locs = byCat('LOC').slice(0, 4);
    const cards = [];
    for (const r of locs) {
      cards.push(card('Code 128', 'Location label',
        'A location — ' + r.LBL1.trim(),
        await svg('code128', r.CODE.trim()),
        r.CODE.trim(), r.LBL2.trim()));
    }
    /* One location also as QR, for the deep-link idea in the design. */
    if (locs[0]) {
      cards.push(card('QR', 'Location label',
        'A location — ' + locs[0].LBL1.trim(),
        await svg('qrcode', locs[0].CODE.trim(), { scale: 4 }),
        locs[0].CODE.trim(), 'Same value, QR encoded'));
    }
    sections.push({
      title: '4 · Locations (Code 128 and QR)',
      blurb: 'Rack and pick-face labels. In Inventory these open "what is in this location"; the counts are against the bulk locations listed.',
      cards
    });
  }

  /* ---- 5. Operator badges: Code 39 -------------------------------- */
  {
    const badges = byCat('BADGE').slice(0, 4);
    const cards = [];
    for (const r of badges) {
      cards.push(card('Code 39', 'Operator badge',
        'An operator badge — ' + r.LBL1.trim(),
        await svg('code39', r.CODE.trim()),
        r.CODE.trim(), r.LBL2.trim()));
    }
    sections.push({
      title: '5 · Operator badges (Code 39)',
      blurb: 'Badge-in identity for shared devices. Note two of these are French-preference operators — badging in as one will flip the app to French.',
      cards
    });
  }

  /* ---- 6. Pallet labels: GS1-128 ---------------------------------- */
  {
    const sscc = byCat('SSCC').slice(0, 3);
    const itf = byCat('ITF');
    const cards = [];
    for (let i = 0; i < sscc.length; i++) {
      const r = sscc[i];
      const gtin = (itf[i] && itf[i].CODE.trim()) || (itf[0] && itf[0].CODE.trim());
      const lot = 'L2026A' + String(17 + i);
      /* Real dates. bwip-js validates AI 17 as a calendar date and rejects
       * day 32, which is a useful reminder that GS1 expiries are not just
       * six digits. */
      const expiry = ['270131', '270228', '270331'][i % 3];

      /* bwip-js takes GS1 element strings in bracketed AI notation and
       * encodes the FNC1 separators itself. */
      const data = `(00)${r.CODE.trim()}(01)${gtin}(10)${lot}(17)${expiry}`;

      cards.push(card('GS1-128', 'Pallet label',
        'A pallet on an open receipt — ' + r.LBL2.trim(),
        await svg('gs1-128', data, { height: 20, scale: 3 }),
        `(00)${r.CODE.trim()}\n(01)${gtin}  (10)${lot}  (17)${expiry}`,
        'Four fields from one scan: SSCC, GTIN, lot, expiry ' +
        '20' + expiry.substr(0, 2) + '-' + expiry.substr(2, 2) + '-' + expiry.substr(4, 2)));
    }
    sections.push({
      title: '6 · Pallet labels (GS1-128)',
      blurb: 'THE ONE TO SHOW. A single scan yields the pallet licence plate, the product GTIN, the batch and the expiry date — parsed in RPG. In Receiving this opens the receipt directly.',
      cards,
      highlight: true
    });
  }

  /* ---- 7. Negative tests ------------------------------------------ */
  {
    const cards = [];

    /* A foreign but perfectly valid EAN-13. */
    cards.push(card('EAN-13', 'Not in our database',
      'Not one of ours — decoded, check digit valid, no match',
      await svg('ean13', '5901234123457'),
      '5901234123457',
      'This is the correct answer, not a failure. Decoding a stranger’s barcode and saying so is the point of Scan Lab.'));

    /* A UPC with a deliberately wrong check digit. */
    const good = (byCat('UPC')[0] || {}).CODE;
    if (good) {
      const body = good.trim().slice(0, 11);
      const rightDigit = gs1Check(body);
      const wrongDigit = String((Number(rightDigit) + 1) % 10);
      /* Rendered as Code 128 on purpose: a UPC-A renderer would refuse to
       * draw an invalid check digit, which is exactly the protection we are
       * trying to test around. */
      cards.push(card('Code 128', 'Corrupted check digit',
        'Not one of ours — check digit reported "Not valid"',
        await svg('code128', body + wrongDigit),
        body + wrongDigit,
        'Correct digit is ' + rightDigit + '. The app must refuse this rather than resolve it to the wrong item.'));
    }

    sections.push({ title: '7 · Negative tests', blurb: 'These SHOULD fail to match. Verifying the failures matters as much as verifying the successes.', cards });
  }

  /* ---- Build the document ---------------------------------------- */
  const html = `<!doctype html><html><head><meta charset="utf-8"><style>
    @page { size: A4; margin: 12mm 10mm; }
    * { box-sizing: border-box; }
    body { font-family: -apple-system, "Segoe UI", Roboto, Helvetica, Arial, sans-serif;
           color: #2B2B2B; margin: 0; }
    .cover { border-bottom: 5px solid #FFCD00; padding-bottom: 10px; margin-bottom: 14px; }
    .brand { display:flex; align-items:center; gap:10px; }
    .mark { background:#FFCD00; color:#000; font-weight:800; font-size:20px;
            padding:6px 10px; border-radius:6px; }
    h1 { font-size: 20px; margin: 0; }
    .sub { color:#6E6E69; font-size:12px; margin-top:2px; }
    .intro { font-size:11.5px; line-height:1.5; margin: 10px 0 0; }
    .intro strong { color:#E31837; }
    h2 { font-size: 14px; margin: 16px 0 2px; padding-top: 8px;
         border-top: 2px solid #E4E4E1; page-break-after: avoid; }
    h2.hl { color:#E31837; border-top-color:#E31837; }
    .blurb { font-size: 11px; color:#6E6E69; margin: 0 0 8px; line-height:1.45;
             page-break-after: avoid; }
    .grid { display:grid; grid-template-columns: 1fr 1fr; gap: 8px; }
    .card { border:1px solid #E4E4E1; border-radius:8px; padding:8px 10px;
            page-break-inside: avoid; break-inside: avoid; background:#fff; }
    .card-head { display:flex; justify-content:space-between; align-items:baseline;
                 border-bottom:1px solid #F0F0EE; padding-bottom:3px; margin-bottom:5px; }
    .card-title { font-weight:800; font-size:11px; letter-spacing:.4px; text-transform:uppercase; }
    .card-sub { font-size:10px; color:#6E6E69; }
    .bc { text-align:center; padding: 2px 0; }
    .bc svg { max-width: 100%; height: auto; }
    .human { font-family: ui-monospace, Menlo, Consolas, monospace; font-size:10px;
             text-align:center; margin-top:2px; white-space:pre-line; word-break:break-all; }
    .expect { font-size:10px; margin-top:5px; line-height:1.35; }
    .note { font-size:9.5px; color:#6E6E69; margin-top:3px; line-height:1.35; }
    .foot { margin-top:16px; border-top:2px solid #E4E4E1; padding-top:8px;
            font-size:10px; color:#6E6E69; line-height:1.5; }
  </style></head><body>

  <div class="cover">
    <div class="brand">
      <span class="mark">GT</span>
      <div>
        <h1>GT Warehouse Mobile — Barcode Test Sheet</h1>
        <div class="sub">Scan Lab and beyond · every value below is real, seeded data from TIGERPOC</div>
      </div>
    </div>
    <p class="intro">
      Print at <strong>100% scale — do not "fit to page"</strong>, which shrinks the bars below what a
      phone camera resolves. Scanning from a screen works too, at full brightness.
      Open <strong>option 4 → Scan Lab → Open camera</strong> and point at any code below.
      Each card states what the application should report; anything else is a bug worth telling me about.
    </p>
  </div>

  ${sections.map((sec) => `
    <h2 class="${sec.highlight ? 'hl' : ''}">${esc(sec.title)}</h2>
    <p class="blurb">${esc(sec.blurb)}</p>
    <div class="grid">${sec.cards.join('')}</div>
  `).join('')}

  <div class="foot">
    Generated by <code>gtwms/tools/gen-test-barcodes.js</code> from live TIGERPOC data.
    Regenerate after reseeding the database — the SKUs, locations and SSCCs change.
    Every check digit here is computed, except the one on the corrupted card in section 7,
    which is wrong on purpose.
  </div>
  </body></html>`;

  fs.writeFileSync('/tmp/barcode-sheet.html', html, 'utf8');

  const browser = await chromium.launch({ executablePath: CHROME, args: ['--no-sandbox'] });
  const page = await browser.newPage();
  await page.setContent(html, { waitUntil: 'load' });
  await page.pdf({ path: outFile, format: 'A4', printBackground: true,
                   margin: { top: '12mm', bottom: '12mm', left: '10mm', right: '10mm' } });
  await browser.close();

  const kb = Math.round(fs.statSync(outFile).size / 1024);
  const total = sections.reduce((n, s) => n + s.cards.length, 0);
  console.log('Wrote ' + outFile + '  (' + kb + ' KB, ' + total + ' barcodes, ' + sections.length + ' sections)');
  sections.forEach((s) => console.log('  ' + String(s.cards.length).padStart(2) + '  ' + s.title));
})();
