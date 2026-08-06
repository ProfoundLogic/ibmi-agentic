#!/usr/bin/env node
/*
 * Verify the generated barcode test sheet actually decodes.
 *
 *   node gtwms/tools/verify-test-barcodes.js <rows.json>
 *
 * Renders every barcode the sheet contains exactly as the PDF does, then reads
 * it back with an independent decoder (ZXing) and compares against what was
 * encoded. A sheet that does not scan would waste a physical test session, and
 * "it looked like a barcode" is not verification.
 *
 * GS1-128 needs care: the decoder returns the element string with FNC1 as a
 * group separator, so the comparison is against the AI values rather than the
 * bracketed notation that was fed to the encoder.
 */

'use strict';

const fs = require('fs');
const path = require('path');
const bwipjs = require('bwip-js');
const { chromium } = require('playwright-core');
const zxing = require('zxing-wasm');

const CHROME = '/home/coder/.cache/ms-playwright/chromium-1228/chrome-linux64/chrome';
const inFile = process.argv[2] || '/tmp/barcodes.json';

const raw = JSON.parse(fs.readFileSync(inFile, 'utf8'));
const rows = (raw.data && raw.data.rows) || raw.rows || [];
const byCat = (c) => rows.filter((r) => r.CAT === c);

/* bwip-js emits a viewBox but NO width/height attributes. Inline in HTML that
 * collapses to zero height and the barcode renders as an empty box -- which is
 * exactly what happened first time, and is invisible until you try to scan it.
 * Derive explicit dimensions so `height: auto` has an aspect ratio to use. */
function sizeSvg(markup) {
  const m = markup.match(/viewBox="0 0 ([0-9.]+) ([0-9.]+)"/);
  if (!m) return markup;
  return markup.replace('<svg ', `<svg width="${m[1]}" height="${m[2]}" `);
}

function gs1Check(payload) {
  let sum = 0;
  for (let i = 0; i < payload.length; i++) {
    const d = Number(payload[payload.length - 1 - i]);
    sum += d * (i % 2 === 0 ? 3 : 1);
  }
  return String((10 - (sum % 10)) % 10);
}

(async () => {
  /* Build the same list of barcodes the sheet renders. */
  const list = [];
  byCat('UPC').slice(0, 6).forEach((r) => list.push({ bcid: 'upca', data: r.CODE.trim(), expect: r.CODE.trim(), label: 'UPC-A ' + r.CODE.trim() }));
  byCat('ITF').slice(0, 4).forEach((r) => list.push({ bcid: 'itf14', data: r.CODE.trim(), expect: r.CODE.trim(), label: 'ITF-14 ' + r.CODE.trim() }));
  byCat('EAN').slice(0, 2).forEach((r) => list.push({ bcid: 'ean13', data: r.CODE.trim(), expect: r.CODE.trim(), label: 'EAN-13 ' + r.CODE.trim() }));
  byCat('LOC').slice(0, 4).forEach((r) => list.push({ bcid: 'code128', data: r.CODE.trim(), expect: r.CODE.trim(), label: 'Code128 ' + r.CODE.trim() }));

  const loc0 = byCat('LOC')[0];
  if (loc0) list.push({ bcid: 'qrcode', data: loc0.CODE.trim(), expect: loc0.CODE.trim(), label: 'QR ' + loc0.CODE.trim(), matrix: true });

  /* Code 39 decodes without the delimiting asterisks. */
  byCat('BADGE').slice(0, 4).forEach((r) => list.push({ bcid: 'code39', data: r.CODE.trim(), expect: r.CODE.trim(), label: 'Code39 ' + r.CODE.trim() }));

  const itf = byCat('ITF');
  byCat('SSCC').slice(0, 3).forEach((r, i) => {
    const gtin = (itf[i] && itf[i].CODE.trim()) || (itf[0] && itf[0].CODE.trim());
    const lot = 'L2026A' + String(17 + i);
    const expiry = ['270131', '270228', '270331'][i % 3];
    list.push({
      bcid: 'gs1-128',
      data: `(00)${r.CODE.trim()}(01)${gtin}(10)${lot}(17)${expiry}`,
      /* What the SCANNER emits: digits only, separators stripped. */
      expectDigits: ['00' + r.CODE.trim(), '01' + gtin, '10' + lot, '17' + expiry],
      label: 'GS1-128 pallet ' + r.LBL2.trim(),
      gs1: true,
    });
  });

  list.push({ bcid: 'ean13', data: '5901234123457', expect: '5901234123457', label: 'EAN-13 foreign (negative test)' });

  const good = (byCat('UPC')[0] || {}).CODE;
  if (good) {
    const body = good.trim().slice(0, 11);
    const wrong = body + String((Number(gs1Check(body)) + 1) % 10);
    list.push({ bcid: 'code128', data: wrong, expect: wrong, label: 'Code128 corrupted check digit (negative test)' });
  }

  /* Render each one to PNG through the same path the PDF uses. */
  const browser = await chromium.launch({ executablePath: CHROME, args: ['--no-sandbox'] });
  const page = await browser.newPage({ viewport: { width: 900, height: 400 } });
  const reader = zxing;

  let fail = 0;
  for (const b of list) {
    const cfg = { bcid: b.bcid, text: b.data, scale: 4, height: 20,
                  includetext: false, paddingwidth: 6, paddingheight: 4 };
    if (b.matrix) { delete cfg.height; cfg.scale = 6; }

    let svg;
    try { svg = sizeSvg(bwipjs.toSVG(cfg)); }
    catch (e) { console.log('  FAIL  ' + b.label + '  -- encode: ' + e.message); fail++; continue; }

    await page.setContent('<body style="margin:0;background:#fff">' +
      '<div id="w" style="display:inline-block;background:#fff;padding:10px">' + svg + '</div></body>');
    const buf = await page.locator('#w').screenshot({ type: 'png' });

    let results;
    try {
      results = await reader.readBarcodesFromImageData
        ? null
        : null;
    } catch (e) { /* fall through to blob path */ }

    /* zxing-wasm reads from a Blob/ArrayBuffer of an image file. */
    const blob = new Blob([buf], { type: 'image/png' });
    let out;
    try {
      out = await reader.readBarcodes(blob, { tryHarder: true, formats: [] });
    } catch (e) {
      console.log('  FAIL  ' + b.label + '  -- decode threw: ' + e.message);
      fail++;
      continue;
    }

    if (!out || !out.length) { console.log('  FAIL  ' + b.label + '  -- nothing decoded'); fail++; continue; }

    const text = out[0].text || '';
    if (b.gs1) {
      /* Compare AI by AI: the decoder strips FNC1 to a separator we do not
       * want to depend on the exact byte of. */
      const digitsOnly = text.replace(/[^0-9A-Za-z]/g, '');
      const missing = b.expectDigits.filter((d) => digitsOnly.indexOf(d.replace(/[^0-9A-Za-z]/g, '')) === -1);
      if (missing.length) {
        console.log('  FAIL  ' + b.label + '  -- missing AI data: ' + missing.join(' '));
        console.log('        decoded: ' + text);
        fail++;
      } else {
        console.log('  PASS  ' + b.label + '  [' + out[0].format + '] all 4 AIs present');
      }
    } else {
      let clean = text.replace(/^\*|\*$/g, '');   /* Code 39 delimiters */
      /* ZXing -- like most phone scanners -- returns a UPC-A as a 13-digit
       * EAN-13 with a leading zero. That is correct scanner behaviour, and
       * GTBAR now normalises it, so accept it here rather than flagging it. */
      if (b.bcid === 'upca' && clean.length === 13 && clean[0] === '0') {
        clean = clean.slice(1);
      }
      if (clean !== b.expect) {
        console.log('  FAIL  ' + b.label + '  -- decoded "' + clean + '" expected "' + b.expect + '"');
        fail++;
      } else {
        console.log('  PASS  ' + b.label + '  [' + out[0].format + ']');
      }
    }
  }

  await browser.close();
  console.log(fail ? `\n${fail} of ${list.length} FAILED` : `\nAll ${list.length} barcodes decode correctly`);
  process.exit(fail ? 1 : 0);
})();
