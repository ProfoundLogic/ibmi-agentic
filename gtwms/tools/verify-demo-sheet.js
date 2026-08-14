#!/usr/bin/env node
/*
 * Does the demo cheat sheet actually scan?
 *
 *   node gtwms/tools/verify-demo-sheet.js
 *
 * Renders every barcode the sheet contains, then reads each one back with an
 * INDEPENDENT decoder (ZXing) and compares it to what was encoded. A sheet that
 * does not scan wastes the demo, and "it looks like a barcode" is not
 * verification -- the first version of the earlier test sheet rendered every
 * barcode at zero height and looked perfectly fine as a PDF.
 *
 * THE LIST COMES FROM THE GENERATOR. gen-demo-sheet.js records every barcode it
 * renders in RENDERED, and this reads that. A hardcoded copy has already caused
 * a false pass in this project -- a test reporting success while covering
 * neither of two new screens -- so the work is derived, never restated.
 *
 * Two decoding facts to expect rather than treat as failures:
 *
 *   1. ZXing returns a UPC-A as a 13-digit EAN-13 with a leading zero. That is
 *      correct scanner behaviour and what a phone does too; GTBAR normalises it.
 *   2. A GS1-128 comes back as an element string with FNC1 rendered as some
 *      separator byte, so the comparison is AI by AI, not string equality.
 */

'use strict';

const { chromium } = require('playwright-core');
const bwipjs = require('bwip-js');
const zxing = require('zxing-wasm');
const gen = require('./gen-demo-sheet.js');

const CHROME = '/home/coder/.cache/ms-playwright/chromium-1228/chrome-linux64/chrome';
const MATRIX = ['qrcode', 'datamatrix', 'azteccode', 'pdf417'];

let pass = 0, fail = 0;

/* The AI values inside a bracketed GS1 element string, as digits/letters only,
 * which is what survives every scanner's separator handling. */
function gs1Parts(text) {
  const parts = [];
  const re = /\((\d{2,4})\)([^(]*)/g;
  let m;
  while ((m = re.exec(text)) !== null) parts.push(m[1] + m[2]);
  return parts;
}

(async () => {
  console.log('building the sheet from live data...');
  const d = gen.loadDemoData();
  gen.buildSections(d);                /* populates gen.RENDERED as a side effect */
  const list = gen.RENDERED;
  console.log('  ' + list.length + ' barcodes to verify\n');

  const browser = await chromium.launch({ executablePath: CHROME, args: ['--no-sandbox'] });
  const page = await browser.newPage({ viewport: { width: 1000, height: 500 } });

  for (const b of list) {
    const label = (b.bcid + ' ' + b.text).slice(0, 62);

    /* Rendered a little larger than the sheet on purpose: this is testing that
     * the SYMBOL is valid and decodable, not the print size. Print size is
     * covered by the 100%-scale note on the sheet itself. */
    const cfg = { bcid: b.bcid, text: b.text, scale: 4, height: 20,
                  includetext: false, paddingwidth: 6, paddingheight: 4 };
    if (MATRIX.indexOf(b.bcid) !== -1) { delete cfg.height; cfg.scale = 6; }

    let svg;
    try { svg = gen.sizeSvg(bwipjs.toSVG(cfg)); }
    catch (e) { console.log('  FAIL  ' + label + '  -- encode: ' + e.message); fail++; continue; }

    await page.setContent('<body style="margin:0;background:#fff">' +
      '<div id="w" style="display:inline-block;background:#fff;padding:12px">' +
      svg + '</div></body>');
    const buf = await page.locator('#w').screenshot({ type: 'png' });

    let out;
    try {
      out = await zxing.readBarcodes(new Blob([buf], { type: 'image/png' }),
                                     { tryHarder: true, formats: [] });
    } catch (e) {
      console.log('  FAIL  ' + label + '  -- decode threw: ' + e.message); fail++; continue;
    }
    if (!out || !out.length) {
      console.log('  FAIL  ' + label + '  -- nothing decoded'); fail++; continue;
    }

    const text = out[0].text || '';
    const fmt = out[0].format || '?';

    if (b.text.indexOf('(') === 0) {
      /* GS1: every AI value must survive the round trip. */
      const want = gs1Parts(b.text);
      const got = text.replace(/[^0-9A-Za-z]/g, '');
      const missing = want.filter((w) => got.indexOf(w.replace(/[^0-9A-Za-z]/g, '')) === -1);
      if (missing.length) {
        console.log('  FAIL  ' + label + '  -- missing AI data: ' + missing.join(' '));
        console.log('        decoded: ' + text);
        fail++;
      } else {
        console.log('  PASS  ' + label + '  [' + fmt + '] ' + want.length + ' AIs intact');
        pass++;
      }
      continue;
    }

    const clean = text.replace(/^\*|\*$/g, '');          /* Code 39 delimiters */
    const asEan13 = '0' + b.text;                        /* UPC-A widened */
    if (clean === b.text || clean === asEan13) {
      console.log('  PASS  ' + label + '  [' + fmt + ']' +
                  (clean === asEan13 ? ' (returned as EAN-13, expected)' : ''));
      pass++;
    } else {
      console.log('  FAIL  ' + label + '  -- decoded "' + clean + '" [' + fmt + ']');
      fail++;
    }
  }

  await browser.close();
  console.log('\n' + (fail ? 'FAILED' : 'Every barcode on the sheet decodes') +
              ': ' + pass + ' passed, ' + fail + ' failed');
  process.exit(fail ? 1 : 0);
})();
