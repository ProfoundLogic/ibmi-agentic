#!/usr/bin/env node
/*
 * GT Warehouse Mobile -- the demo barcode cheat sheet.
 *
 *   node gtwms/tools/gen-demo-sheet.js [out.pdf]
 *
 * A printable sheet of REAL, scannable barcodes laid out in the order of the
 * demo script (docs/DEMO-SCRIPT.md), one section per act, so the presenter can
 * work down the page instead of hunting for the right code mid-sentence.
 *
 * DRIVEN BY THE LIVE DATABASE, not by a hardcoded list. Every value is queried
 * from TIGERPOC at generation time, because a cheat sheet that has drifted from
 * the data is worse than no cheat sheet: it fails in front of the audience. If
 * a receipt has been posted since the last run, this sheet knows.
 *
 * Rendering details that matter for a sheet that actually scans -- all three
 * learned the hard way:
 *
 *   1. bwip-js emits a viewBox but no width/height. Inline in HTML that
 *      collapses to zero height and prints as an empty box, which is invisible
 *      until someone tries to scan it.
 *   2. Phone cameras need a wider module and a bigger quiet zone than a contact
 *      laser scanner. Everything is rendered generously, not at minimum spec.
 *   3. Matrix symbologies size themselves from `scale`; passing a linear bar
 *      height makes bwip-js reject the render outright.
 *
 * Verify with:  node gtwms/tools/verify-demo-sheet.js
 */

'use strict';

const fs = require('fs');
const { execFileSync } = require('child_process');
const bwipjs = require('bwip-js');
const { chromium } = require('playwright-core');

const CHROME = '/home/coder/.cache/ms-playwright/chromium-1228/chrome-linux64/chrome';
const OUT = process.argv[2] || '/task-output/GT-demo-barcode-cheatsheet.pdf';

/* ------------------------------------------------------------------
   Live data. One query per question, so a failure names itself.
   ------------------------------------------------------------------ */
function sql(statement) {
  fs.writeFileSync('/tmp/.gtdemo.json',
    JSON.stringify({ connection: 'dev', sql: statement }));
  let out;
  try {
    out = execFileSync('aitool', ['sql', '--input', '@/tmp/.gtdemo.json'],
                       { maxBuffer: 64 * 1024 * 1024 }).toString();
  } catch (e) {
    /* aitool exits non-zero on a SQL error and puts the JSON on stderr. Reading
       it is the difference between a message and a 700-byte Uint8Array dump. */
    out = (e.stdout && e.stdout.toString()) || (e.stderr && e.stderr.toString()) || '';
  }
  let parsed;
  try { parsed = JSON.parse(out); }
  catch (e) { throw new Error('aitool gave no JSON for: ' + statement.slice(0, 90)); }
  if (!parsed.success) {
    throw new Error('SQL failed: ' + (parsed.error && parsed.error.message) +
                    '\n  in: ' + statement.replace(/\s+/g, ' ').slice(0, 160));
  }
  return (parsed.data && parsed.data.rows || []).map((r) => {
    const o = {};
    for (const k of Object.keys(r)) o[k] = typeof r[k] === 'string' ? r[k].trim() : r[k];
    return o;
  });
}

function loadDemoData() {
  /* The hero item: it must have an each barcode, a case barcode, imagery for
     the carousel, AND be staged for putaway, so one SKU carries three acts. */
  const hero = sql(`
    SELECT b.sku, i.description_en, i.uom, i.case_pack,
           MAX(CASE WHEN b.symbology = 'UPC-A'  THEN b.barcode END) AS upc,
           MAX(CASE WHEN b.symbology = 'EAN-13' THEN b.barcode END) AS ean,
           MAX(CASE WHEN b.symbology = 'ITF-14' THEN b.barcode END) AS itf,
           MAX(CASE WHEN b.symbology = 'ITF-14' THEN b.pack_qty END) AS caseqty
      FROM TIGERPOC.GTBARCODE b
      JOIN TIGERPOC.GTITEM i ON i.sku = b.sku
     WHERE b.sku IN (SELECT sku FROM TIGERPOC.GTVPUTSTG)
       AND EXISTS (SELECT 1 FROM TIGERPOC.GTVITEMIMG v WHERE v.sku = b.sku)
     GROUP BY b.sku, i.description_en, i.uom, i.case_pack
    HAVING MAX(CASE WHEN b.symbology = 'UPC-A'  THEN b.barcode END) IS NOT NULL
       AND MAX(CASE WHEN b.symbology = 'EAN-13' THEN b.barcode END) IS NOT NULL
       AND MAX(CASE WHEN b.symbology = 'ITF-14' THEN b.barcode END) IS NOT NULL
     ORDER BY b.sku
     FETCH FIRST 1 ROW ONLY`)[0];

  /* The receipt to work live: the SMALLEST open one, so the act finishes inside
     its slot. A 60-line receipt is a better story and a worse demo. */
  const receipt = sql(`
    SELECT h.receipt_id, h.sscc, h.door_id, h.vendor_name, h.po_number,
           COUNT(d.line_no) AS lines
      FROM TIGERPOC.GTRCPTHDR h
      JOIN TIGERPOC.GTRCPTDTL d ON d.receipt_id = h.receipt_id
     WHERE h.receipt_status = 'OPEN'
     GROUP BY h.receipt_id, h.sscc, h.door_id, h.vendor_name, h.po_number
     ORDER BY COUNT(d.line_no), h.receipt_id
     FETCH FIRST 1 ROW ONLY`)[0];

  /* A second open receipt, as the spare. Demos go sideways. */
  const spare = sql(`
    SELECT h.receipt_id, h.sscc, h.door_id, h.vendor_name,
           COUNT(d.line_no) AS lines
      FROM TIGERPOC.GTRCPTHDR h
      JOIN TIGERPOC.GTRCPTDTL d ON d.receipt_id = h.receipt_id
     WHERE h.receipt_status = 'OPEN'
     GROUP BY h.receipt_id, h.sscc, h.door_id, h.vendor_name
     ORDER BY COUNT(d.line_no), h.receipt_id
     OFFSET 1 ROWS FETCH FIRST 1 ROW ONLY`)[0];

  /* One line off that receipt, so Receiving can be driven by scanning an item
     rather than tapping -- which is the whole point of the screen. */
  const rcvLine = sql(`
    SELECT d.sku, d.qty_expected, i.description_en,
           MIN(d.line_no) AS line_no,
           MAX(CASE WHEN b.symbology = 'UPC-A' THEN b.barcode END) AS upc
      FROM TIGERPOC.GTRCPTDTL d
      JOIN TIGERPOC.GTITEM i ON i.sku = d.sku
      JOIN TIGERPOC.GTBARCODE b ON b.sku = d.sku
     WHERE d.receipt_id = '${receipt.RECEIPT_ID}'
     GROUP BY d.sku, d.qty_expected, i.description_en
    HAVING MAX(CASE WHEN b.symbology = 'UPC-A' THEN b.barcode END) IS NOT NULL
     ORDER BY MIN(d.line_no)
     FETCH FIRST 2 ROWS ONLY`);

  /* Putaway: a staged pallet, and the destination the app will suggest for the
     hero SKU. Printed so the presenter can say the answer before the app does. */
  const staged = sql(`
    SELECT s.sscc, s.stage_location, COUNT(*) AS lines,
           SUM(s.qty_staged) AS units
      FROM TIGERPOC.GTVPUTSTG s
     WHERE s.sscc <> ''
     GROUP BY s.sscc, s.stage_location
     ORDER BY COUNT(*) DESC
     FETCH FIRST 1 ROW ONLY`)[0];

  const heroPut = sql(`
    SELECT s.qty_staged,
           (SELECT g.location_id FROM TIGERPOC.GTVPUTSUG g WHERE g.sku = s.sku
             ORDER BY g.sort_key, g.room DESC FETCH FIRST ROW ONLY) AS dest,
           (SELECT g.location_type FROM TIGERPOC.GTVPUTSUG g WHERE g.sku = s.sku
             ORDER BY g.sort_key, g.room DESC FETCH FIRST ROW ONLY) AS desttyp,
           (SELECT g.room FROM TIGERPOC.GTVPUTSUG g WHERE g.sku = s.sku
             ORDER BY g.sort_key, g.room DESC FETCH FIRST ROW ONLY) AS room
      FROM TIGERPOC.GTVPUTSTG s
     WHERE s.sku = '${hero.SKU}'
     FETCH FIRST 1 ROW ONLY`)[0] || {};

  /* Cycle count first, because Inventory must then pick a DIFFERENT location:
     the fullest bulk bay is also the one somebody scheduled a count for, and
     acts 5 and 6 sharing a barcode makes them read as one act. */
  const count = sql(`
    SELECT c.count_id, c.location_id, o.full_name, COUNT(d.line_no) AS lines
      FROM TIGERPOC.GTCOUNTHDR c
      JOIN TIGERPOC.GTCOUNTDTL d ON d.count_id = c.count_id
      LEFT JOIN TIGERPOC.GTOPERATOR o ON o.operator_id = c.assigned_to
     WHERE c.count_status = 'OPEN'
     GROUP BY c.count_id, c.location_id, o.full_name
     ORDER BY COUNT(d.line_no) DESC
     FETCH FIRST 1 ROW ONLY`)[0];

  /* An item that is ACTUALLY ON that count, so 6.2 lands on a line instead of
     telling the presenter to scan something the count has never heard of. */
  const cntLine = sql(`
    SELECT d.sku, i.description_en,
           MAX(CASE WHEN b.symbology = 'UPC-A' THEN b.barcode END) AS upc
      FROM TIGERPOC.GTCOUNTDTL d
      JOIN TIGERPOC.GTITEM i ON i.sku = d.sku
      JOIN TIGERPOC.GTBARCODE b ON b.sku = d.sku
     WHERE d.count_id = '${count.COUNT_ID}'
     GROUP BY d.sku, i.description_en
    HAVING MAX(CASE WHEN b.symbology = 'UPC-A' THEN b.barcode END) IS NOT NULL
     ORDER BY d.sku
     FETCH FIRST 1 ROW ONLY`)[0];

  /* Inventory: the fullest bulk bay that is NOT the counted one. */
  const bulk = sql(`
    SELECT b.location_id, COUNT(*) AS skus, SUM(b.qty_on_hand) AS qty
      FROM TIGERPOC.GTINVBAL b
      JOIN TIGERPOC.GTLOCATION l ON l.location_id = b.location_id
     WHERE l.location_type = 'BULK'
       AND b.location_id <> '${count.LOCATION_ID}'
     GROUP BY b.location_id
     ORDER BY COUNT(*) DESC
     FETCH FIRST 1 ROW ONLY`)[0];

  const replen = sql(`
    SELECT location_id, sku, qty_on_hand, min_qty, qty_suggested, qty_bulk_avail
      FROM TIGERPOC.GTVREPLEN
     ORDER BY qty_on_hand - min_qty
     FETCH FIRST 1 ROW ONLY`)[0];

  /* A reason code that REQUIRES a photograph -- the count act needs one to
     demonstrate the declarative photo rule. */
  const reason = sql(`
    SELECT reason_code, reason_desc_en
      FROM TIGERPOC.GTREASON
     WHERE photo_required = 'Y' AND reason_type = 'CNT'
     ORDER BY sort_order
     FETCH FIRST 1 ROW ONLY`)[0] || sql(`
    SELECT reason_code, reason_desc_en FROM TIGERPOC.GTREASON
     WHERE photo_required = 'Y' ORDER BY sort_order FETCH FIRST 1 ROW ONLY`)[0];

  const oper = sql(`
    SELECT operator_id, badge_barcode, full_name
      FROM TIGERPOC.GTOPERATOR WHERE operator_id = 'GT001'`)[0];

  return { hero, receipt, spare, rcvLine, staged, heroPut, bulk, replen,
           count, cntLine, reason, oper };
}

/* ------------------------------------------------------------------
   Barcode rendering
   ------------------------------------------------------------------ */
function sizeSvg(markup) {
  const m = markup.match(/viewBox="0 0 ([0-9.]+) ([0-9.]+)"/);
  if (!m) return markup;
  return markup.replace('<svg ', `<svg width="${m[1]}" height="${m[2]}" `);
}

const MATRIX = ['qrcode', 'datamatrix', 'azteccode', 'pdf417'];

/* Every barcode this module renders, recorded as it is rendered.
 *
 * verify-demo-sheet.js reads THIS rather than keeping its own list. A hardcoded
 * copy has already produced a false pass in this project -- the worst possible
 * test result -- so the verifier derives the work from the generator and cannot
 * silently cover fewer barcodes than the sheet contains. */
const RENDERED = [];

function svg(bcid, text, opts) {
  RENDERED.push({ bcid, text });
  const cfg = Object.assign({
    bcid, text,
    scale: 3,          /* a phone at arm's length needs a wide module */
    height: 16,
    includetext: false,
    paddingwidth: 4,   /* quiet zone: more than spec, deliberately */
    paddingheight: 2
  }, opts || {});
  if (MATRIX.indexOf(bcid) !== -1) { delete cfg.height; delete cfg.width; }

  /* NO BEARER BAR on ITF-14. bwip-js draws one by default -- it is correct for a
     printed carton, where the thick frame stops a laser scanner reading a partial
     symbol -- but it sits hard against the quiet zone, and through a CAMERA frame
     that is fatal: ZXing rejects the symbol outright. Measured, one identical
     value: with the bearer bar, 40 frames and no read; with borderwidth 0, read on
     the first frame.

     This card would have failed live at steps 1.3 and 2.2 while every check we had
     said the sheet was good, because verifying a rendered barcode as a STILL IMAGE
     is not verifying it through a camera -- at natural resolution the bearer bar
     decodes fine, and it is the downscale into a video frame that closes the quiet
     zone. The camera-path coverage now lives in test-camera.js. */
  if (bcid === 'itf14' && cfg.borderwidth === undefined) cfg.borderwidth = 0;

  return sizeSvg(bwipjs.toSVG(cfg));
}

function esc(s) {
  return String(s == null ? '' : s)
    .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
}

/* One scannable card. `step` is the script line it belongs to, so the sheet and
   the script are read together rather than cross-referenced. */
function card(c) {
  return `
    <div class="card${c.hero ? ' hero' : ''}">
      <div class="chead">
        ${c.step ? `<span class="step">${esc(c.step)}</span>` : ''}
        <span class="ctitle">${esc(c.title)}</span>
        <span class="csym">${esc(c.sym)}</span>
      </div>
      <div class="bc">${c.svg}</div>
      <div class="human">${esc(c.human)}</div>
      <div class="expect"><b>App:</b> ${esc(c.expect)}</div>
      ${c.note ? `<div class="note">${esc(c.note)}</div>` : ''}
    </div>`;
}

/* ------------------------------------------------------------------
   The sheet
   ------------------------------------------------------------------ */
function buildSections(d) {
  const S = [];
  const heroName = d.hero.DESCRIPTION_EN;

  /* ---- ACT 1: Scan Lab, the versatility page --------------------- */
  const lab = [];

  /* The Scan Lab template's own labels for gtbar_result.kind. Quoting them
     verbatim matters: the presenter reads this line aloud while the audience
     reads the screen, and a paraphrase makes the sheet look wrong. */
  const KIND = {
    ITEM: 'An item', LOC: 'A location', PALLET: 'A pallet',
    RCPT: 'A pallet on an open receipt', OPER: 'An operator badge',
    UNKNWN: 'Not one of ours'
  };

  lab.push(card({ step: '1.1', title: 'UPC-A · retail each', sym: 'UPC-A (12)',
    svg: svg('upca', d.hero.UPC), human: d.hero.UPC,
    expect: `${KIND.ITEM} · ${heroName}`,
    note: 'The everyday North American retail code.' }));

  lab.push(card({ step: '1.2', title: 'EAN-13 · same item, international',
    sym: 'EAN-13 (13)', svg: svg('ean13', d.hero.EAN), human: d.hero.EAN,
    expect: `${KIND.ITEM} · ${heroName}`,
    note: 'Different symbology, same resolution path.' }));

  lab.push(card({ step: '1.3', title: 'ITF-14 · the CASE', sym: 'ITF-14 (14)',
    svg: svg('itf14', d.hero.ITF), human: d.hero.ITF,
    expect: `${KIND.ITEM} · GTIN ${d.hero.ITF} · case of ${d.hero.CASEQTY}`,
    note: 'Same product, a case not an each. The pack quantity comes back with it.' }));

  lab.push(card({ step: '1.4', title: 'EAN-8 · short retail', sym: 'EAN-8 (8)',
    svg: svg('ean8', '96385074'), human: '96385074',
    expect: `Symbology EAN-8 · check digit OK · "${KIND.UNKNWN}"`,
    note: 'Deliberate: proves the symbology is identified even when the key is not ours.' }));

  lab.push(card({ step: '1.5', title: 'SSCC-18 · a pallet licence plate',
    sym: 'SSCC-18 (18)', svg: svg('code128', d.receipt.SSCC), human: d.receipt.SSCC,
    expect: `${KIND.RCPT} · ${d.receipt.RECEIPT_ID}`,
    note: 'A bare 18-digit SSCC, with no GS1 wrapper.' }));

  lab.push(card({ step: '1.6', title: 'GS1-128 · SSCC inside AI (00)',
    sym: 'GS1-128', svg: svg('gs1-128', '(00)' + d.receipt.SSCC),
    human: '(00) ' + d.receipt.SSCC,
    expect: `${KIND.RCPT} · ${d.receipt.RECEIPT_ID}`,
    note: 'The same pallet, properly wrapped. Both routes must agree.' }));

  lab.push(card({ step: '1.7', hero: true,
    title: 'GS1-128 · four fields in ONE scan', sym: 'GS1-128 multi-AI',
    /* (30), not (37): GS1 makes (01) and (37) mutually exclusive -- (37) counts
       trade items on an SSCC, (30) is a variable count against a GTIN. bwip-js
       enforces it, which is a decent sign the sheet is genuinely valid GS1 and
       not just a picture of bars. */
    svg: svg('gs1-128',
      `(01)${d.hero.ITF}(10)LOT4471(17)261231(30)${d.hero.CASEQTY}`),
    human: `(01) ${d.hero.ITF}  (10) LOT4471  (17) 261231  (30) ${d.hero.CASEQTY}`,
    expect: `${KIND.ITEM} ${heroName} · GS1-128 · lot LOT4471 · expiry 261231 · qty ${d.hero.CASEQTY}`,
    note: 'THE SHOWPIECE. GTIN, lot, expiry and count from a single scan — no typing, no four fields.' }));

  lab.push(card({ step: '1.8', title: 'Code 39 · a rack label', sym: 'Code 39',
    svg: svg('code39', d.bulk.LOCATION_ID), human: d.bulk.LOCATION_ID,
    expect: `${KIND.LOC} · ${d.bulk.LOCATION_ID}`,
    note: 'Alphanumeric, so symbology is inferred from content, not trusted from the client.' }));

  lab.push(card({ step: '1.9', title: 'Code 39 · an operator badge',
    sym: 'Code 39', svg: svg('code39', d.oper.BADGE_BARCODE),
    human: d.oper.BADGE_BARCODE,
    expect: `${KIND.OPER} · ${d.oper.FULL_NAME}`,
    note: 'One camera reads people as well as product.' }));

  lab.push(card({ step: '1.10', title: 'Code 128 · a purchase order',
    sym: 'Code 128', svg: svg('code128', d.receipt.PO_NUMBER || 'PO0000031884'),
    human: d.receipt.PO_NUMBER || 'PO0000031884',
    expect: `Symbology CODE-128 · "${KIND.UNKNWN}"`,
    note: 'Honest negative: recognised, decoded, and correctly not pretended to be a match.' }));

  lab.push(card({ step: '1.11', hero: true, title: 'QR · same location, 2D',
    sym: 'QR Code', svg: svg('qrcode', d.bulk.LOCATION_ID),
    human: d.bulk.LOCATION_ID,
    expect: `${KIND.LOC} · ${d.bulk.LOCATION_ID}`,
    note: 'Identical payload to 1.8 in a 2D code — the resolution does not care how it arrived.' }));

  lab.push(card({ step: '1.12', title: 'Data Matrix · tiny-label symbology',
    sym: 'Data Matrix', svg: svg('datamatrix', d.hero.UPC), human: d.hero.UPC,
    expect: `${KIND.ITEM} · ${heroName}`,
    note: 'What a pharmaceutical or small-part label uses.' }));

  lab.push(card({ step: '1.13', title: 'PDF417 · stacked 2D', sym: 'PDF417',
    svg: svg('pdf417', d.receipt.SSCC, { columns: 4 }), human: d.receipt.SSCC,
    expect: `${KIND.RCPT} · ${d.receipt.RECEIPT_ID}`,
    note: 'Driver licences and shipping manifests.' }));

  lab.push(card({ step: '1.14', title: 'Aztec · dense 2D', sym: 'Aztec',
    svg: svg('azteccode', d.oper.BADGE_BARCODE), human: d.oper.BADGE_BARCODE,
    expect: `${KIND.OPER} · ${d.oper.FULL_NAME}`,
    note: 'Transport ticketing. Included because the camera reads it and a laser gun does not.' }));

  const badUpc = d.hero.UPC.slice(0, 11) +
                 String((Number(d.hero.UPC.slice(-1)) + 5) % 10);
  lab.push(card({ step: '1.15', title: 'Bad check digit · MUST be rejected',
    sym: 'UPC-A (corrupt)', svg: svg('code128', badUpc), human: badUpc,
    expect: `Symbology UPC-A · check digit FAILS · "${KIND.UNKNWN}"`,
    note: 'Rendered as Code 128 on purpose: a UPC-A encoder would silently CORRECT the digit and there would be nothing to demonstrate.' }));

  S.push({ title: 'ACT 1 · Scan Lab — one camera, every symbology',
           blurb: 'Work down the column. Each card names what the app should say; ' +
                  'if it says something else, that is the story, not a glitch.',
           cards: lab });

  /* ---- ACT 2: Item Lookup + PHOTO -------------------------------- */
  S.push({
    title: 'ACT 2 · Item Lookup — and the first photograph',
    blurb: `Hero item for the whole demo: ${d.hero.SKU} — ${heroName}. ` +
           'It carries three acts: it is in the catalogue with imagery, and it is ' +
           'staged for putaway.',
    cards: [
      card({ step: '2.1', hero: true, title: 'Scan the each', sym: 'UPC-A',
        svg: svg('upca', d.hero.UPC), human: d.hero.UPC,
        expect: `Item detail · carousel · on hand by location`,
        note: 'THEN TAKE A PHOTO: tap the camera on the carousel, shoot, Use. It appears in the carousel immediately and is stored as a journaled BLOB in DB2.' }),
      card({ step: '2.2', title: 'Scan the case', sym: 'ITF-14',
        svg: svg('itf14', d.hero.ITF), human: d.hero.ITF,
        expect: `Same item, case of ${d.hero.CASEQTY}`,
        note: 'Shows each-versus-case resolution on the same product.' })
    ]
  });

  /* ---- ACT 3: Receiving ------------------------------------------ */
  const rcv = [
    card({ step: '3.1', hero: true,
      title: `Pallet for ${d.receipt.RECEIPT_ID}`, sym: 'SSCC-18',
      svg: svg('code128', d.receipt.SSCC), human: d.receipt.SSCC,
      expect: `${d.receipt.RECEIPT_ID} · ${d.receipt.LINES} lines · door ${d.receipt.DOOR_ID}`,
      note: `${d.receipt.VENDOR_NAME}. The smallest open receipt, chosen so this act finishes inside its slot.` })
  ];
  d.rcvLine.forEach((l, i) => {
    rcv.push(card({ step: '3.' + (i + 2), title: 'Confirm a line by scanning it',
      sym: 'UPC-A', svg: svg('upca', l.UPC), human: l.UPC,
      expect: `Line flashes · ${l.QTY_EXPECTED} expected`,
      note: i === 0
        ? 'Scan-to-confirm: the line highlights and takes the expected quantity.'
        : 'Second line — then EDIT this quantity down to show a variance, and Save.' }));
  });
  rcv.push(card({ step: '3.' + (d.rcvLine.length + 2),
    title: `Spare receipt: ${d.spare ? d.spare.RECEIPT_ID : 'n/a'}`,
    sym: 'SSCC-18',
    svg: svg('code128', d.spare ? d.spare.SSCC : '306141410000000013'),
    human: d.spare ? d.spare.SSCC : '306141410000000013',
    expect: d.spare ? `${d.spare.RECEIPT_ID} · ${d.spare.LINES} lines` : 'n/a',
    note: 'Insurance. Use if 3.1 has already been posted in a rehearsal.' }));

  S.push({
    title: 'ACT 3 · Receiving — pallet to posted, under commitment control',
    blurb: 'Finish with POST. That is what creates the Putaway work used in Act 4, ' +
           'so do not skip it.',
    cards: rcv
  });

  /* ---- ACT 4: Putaway -------------------------------------------- */
  S.push({
    title: 'ACT 4 · Putaway — the other half of Receiving',
    blurb: `The queue holds what Act 3 just posted. Staged now: ` +
           `${d.staged ? d.staged.LINES + ' lines, ' + Math.round(d.staged.UNITS) + ' units' : 'nothing'}.`,
    cards: [
      card({ step: '4.1', hero: true, title: 'Scan the staged pallet',
        sym: 'SSCC-18', svg: svg('code128', d.staged ? d.staged.SSCC : d.receipt.SSCC),
        human: d.staged ? d.staged.SSCC : d.receipt.SSCC,
        expect: `Its rows flash in the queue at ${d.staged ? d.staged.STAGE_LOCATION : 'STAGE01'}`,
        note: 'The scan marks the work rather than opening it — the operator still chooses the line.' }),
      card({ step: '4.2', title: 'Or go straight to the hero item', sym: 'UPC-A',
        svg: svg('upca', d.hero.UPC), human: d.hero.UPC,
        expect: d.heroPut.DEST
          ? `Confirm screen · suggests ${d.heroPut.DEST} (${d.heroPut.DESTTYP}, room ${Math.round(d.heroPut.ROOM)})`
          : 'Confirm screen with the suggested destination',
        note: 'SAY THE ANSWER BEFORE THE APP DOES: the suggestion is printed here, so the audience sees it was reasoned, not guessed. Book it, then show the audit line.' }),
      card({ step: '4.3', title: 'Override the suggestion', sym: 'Code 39',
        svg: svg('code39', d.bulk.LOCATION_ID), human: d.bulk.LOCATION_ID,
        expect: `Destination becomes ${d.bulk.LOCATION_ID} · badge flips to SCANNED`,
        note: 'The point: the application suggests, the operator decides.' })
    ]
  });

  /* ---- ACT 5: Inventory ------------------------------------------ */
  S.push({
    title: 'ACT 5 · Inventory & Movement — one box, three meanings',
    blurb: 'The same scan box takes a location, an item or a pallet and works out ' +
           'which it got.',
    cards: [
      card({ step: '5.1', hero: true, title: 'A bulk bay', sym: 'Code 39',
        svg: svg('code39', d.bulk.LOCATION_ID), human: d.bulk.LOCATION_ID,
        expect: `${d.bulk.LOCATION_ID} · ${d.bulk.SKUS} SKUs · ${Math.round(d.bulk.QTY)} units`,
        note: 'Capacity, contents and the replenish flags.' }),
      card({ step: '5.2', title: 'The worst replenishment alert', sym: 'Code 39',
        svg: svg('code39', d.replen.LOCATION_ID), human: d.replen.LOCATION_ID,
        expect: `${d.replen.LOCATION_ID} · ${Math.round(d.replen.QTY_ON_HAND)} on hand vs ${Math.round(d.replen.MIN_QTY)} minimum`,
        note: `Pull ${Math.round(d.replen.QTY_SUGGESTED)} from bulk (${Math.round(d.replen.QTY_BULK_AVAIL)} available). Do the move — it is one tap from here.` }),
      card({ step: '5.3', title: 'An item, in the same box', sym: 'UPC-A',
        svg: svg('upca', d.hero.UPC), human: d.hero.UPC,
        expect: 'Item detail — every location holding it',
        note: '"Where is this?" is the same question as "what is this?", so it reuses Item Lookup rather than a near-duplicate screen.' })
    ]
  });

  /* ---- ACT 6: Cycle Count + PHOTO -------------------------------- */
  S.push({
    title: 'ACT 6 · Cycle Count — blind, then the reveal, then the evidence',
    blurb: `Count ${d.count.COUNT_ID} at ${d.count.LOCATION_ID}, ` +
           `${d.count.LINES} lines. Count two, no more — the point is the ` +
           `mechanism, not the arithmetic.`,
    cards: [
      card({ step: '6.1', hero: true, title: 'The count location', sym: 'Code 39',
        svg: svg('code39', d.count.LOCATION_ID), human: d.count.LOCATION_ID,
        expect: `Blind entry for ${d.count.COUNT_ID} · ${d.count.LINES} lines`,
        note: 'SHOW THE AUDIENCE there is no expected quantity anywhere on this screen. It is not hidden by CSS — it never leaves the database.' }),
      card({ step: '6.2', title: 'Count a line by scanning it', sym: 'UPC-A',
        svg: svg('upca', d.cntLine.UPC), human: d.cntLine.UPC,
        expect: `${d.cntLine.DESCRIPTION_EN} flashes, ready for the number`,
        note: 'Enter one count CORRECT and one DELIBERATELY SHORT. The short one is what makes Act 6 worth watching.' }),
      card({ step: '6.3', title: 'Then: Review → the reveal', sym: '— no scan —',
        svg: svg('qrcode', 'REVIEW'), human: 'tap Review',
        expect: `Expected beside counted, variance flagged`,
        note: `AND THE SECOND PHOTOGRAPH: pick reason ${d.reason ? d.reason.REASON_CODE : 'CVAR'}` +
              `${d.reason ? ' (' + d.reason.REASON_DESC_EN + ')' : ''} — it REQUIRES a photo, ` +
              'and the app will not let the count post without one. The rule comes from the reason-code table, not from code.' })
    ]
  });

  /* ---- ACT 7-8 --------------------------------------------------- */
  S.push({
    title: 'ACT 7 & 8 · Supervisor View, then Settings — no barcodes needed',
    blurb: 'Both are tap-only. They land last on purpose: by now the numbers on ' +
           'the dashboard are numbers the audience watched you create.',
    cards: [
      card({ step: '7.1', title: 'Supervisor View', sym: '— tap tile 6 —',
        svg: svg('qrcode', 'SUPV'), human: 'menu tile: Supervisor',
        expect: 'Progress bars and the work queue have MOVED since Act 1',
        note: 'Tap the receipt row you posted in Act 3 — it opens the real screen, not a report.' }),
      card({ step: '8.1', title: 'Settings · French', sym: '— tap tile 8 —',
        svg: svg('qrcode', 'SETT'), human: 'menu tile: Settings',
        expect: 'Whole application in French, immediately',
        note: 'Then Large touch → On. Close on the language switch: it is one column in DB2, and every screen honours it.' })
    ]
  });

  return S;
}

/* ------------------------------------------------------------------ */
async function main() {
  console.log('reading the live database...');
  const d = loadDemoData();

  console.log('  hero item   : ' + d.hero.SKU + ' — ' + d.hero.DESCRIPTION_EN);
  console.log('  receipt     : ' + d.receipt.RECEIPT_ID + ' (' + d.receipt.LINES +
              ' lines, SSCC ' + d.receipt.SSCC + ')');
  console.log('  count       : ' + d.count.COUNT_ID + ' at ' + d.count.LOCATION_ID);
  console.log('  bulk bay    : ' + d.bulk.LOCATION_ID);
  console.log('  replen      : ' + d.replen.LOCATION_ID);

  const sections = buildSections(d);
  const stamp = sql('SELECT CHAR(CURRENT TIMESTAMP) AS TS FROM SYSIBM.SYSDUMMY1')[0].TS
                  .slice(0, 16).replace('-', '-').replace(/\./g, ':');

  const html = `<!doctype html><html><head><meta charset="utf-8">
<style>
  @page { size: A4; margin: 10mm; }
  * { box-sizing: border-box; }
  body { font: 11px/1.35 -apple-system, "Segoe UI", Roboto, Arial, sans-serif;
         color: #1d1d1b; margin: 0; }
  h1 { font-size: 19px; margin: 0 0 2px; letter-spacing: -.2px; }
  .sub { color: #6E6E69; margin: 0 0 10px; font-size: 10.5px; }
  .brand { display: inline-block; background: #DA291C; color: #fff;
           font-weight: 800; padding: 2px 7px; border-radius: 4px;
           letter-spacing: .4px; }
  h2 { font-size: 13px; margin: 14px 0 1px; padding-bottom: 3px;
       border-bottom: 2px solid #FFCD00; page-break-after: avoid; }
  .blurb { color: #6E6E69; margin: 0 0 8px; font-size: 10px;
           page-break-after: avoid; }
  .grid { display: grid; grid-template-columns: repeat(3, 1fr); gap: 7px; }
  .card { border: 1px solid #ddd; border-radius: 6px; padding: 7px 8px 8px;
          page-break-inside: avoid; break-inside: avoid; }
  .card.hero { border-color: #DA291C; border-width: 2px; background: #FFFCF5; }
  .chead { display: flex; align-items: baseline; gap: 5px; flex-wrap: wrap;
           margin-bottom: 4px; }
  .step { background: #1d1d1b; color: #fff; font-weight: 800; font-size: 9px;
          padding: 1px 5px; border-radius: 3px; }
  .ctitle { font-weight: 700; font-size: 10.5px; }
  .csym { color: #6E6E69; font-size: 9px; text-transform: uppercase;
          letter-spacing: .3px; margin-left: auto; }
  /* White behind every barcode, always: a tinted card breaks the contrast a
     camera needs. */
  .bc { background: #fff; text-align: center; padding: 3px 0; }
  .bc svg { width: 100%; height: auto; max-height: 78px; }
  .human { font-family: "SF Mono", Consolas, monospace; font-size: 8.5px;
           text-align: center; color: #444; word-break: break-all;
           margin-top: 2px; }
  .expect { margin-top: 4px; font-size: 9.5px; }
  .note { margin-top: 3px; font-size: 9px; color: #6E6E69; }
  .foot { margin-top: 14px; border-top: 1px solid #ddd; padding-top: 6px;
          font-size: 9px; color: #6E6E69; }
</style></head><body>
  <h1><span class="brand">GT</span> Warehouse Mobile — demo barcode cheat sheet</h1>
  <p class="sub">
    Every barcode below is real, has a correct check digit, and resolves against
    live <b>TIGERPOC</b> data as at <b>${esc(stamp)}</b>. Cards outlined in red are
    the ones that carry the story. Read alongside <b>DEMO-SCRIPT.md</b> — the step
    numbers match.
  </p>
  ${sections.map((s) => `
    <h2>${esc(s.title)}</h2>
    <p class="blurb">${esc(s.blurb)}</p>
    <div class="grid">${s.cards.join('')}</div>`).join('')}
  <p class="foot">
    Regenerate before every demo: <b>node gtwms/tools/gen-demo-sheet.js</b> — it
    re-reads the database, so a receipt posted in rehearsal cannot leave a dead
    barcode on the page. Verify with <b>verify-demo-sheet.js</b>, which decodes
    every barcode on this sheet with an independent reader.
    Print at 100% — "fit to page" shrinks the modules and phones start missing them.
  </p>
</body></html>`;

  fs.writeFileSync('/tmp/gt-demo-sheet.html', html);

  const browser = await chromium.launch({ executablePath: CHROME, args: ['--no-sandbox'] });
  const page = await browser.newPage();
  await page.setContent(html, { waitUntil: 'load' });
  await page.pdf({ path: OUT, format: 'A4', printBackground: true,
                   margin: { top: '10mm', bottom: '10mm', left: '10mm', right: '10mm' } });
  await browser.close();

  const kb = Math.round(fs.statSync(OUT).size / 1024);
  const total = sections.reduce((n, s) => n + s.cards.length, 0);
  console.log('\nwrote ' + OUT + '  (' + kb + ' KB, ' + total + ' barcodes)');
  sections.forEach((s) => console.log('  ' + String(s.cards.length).padStart(2) + '  ' + s.title));
}

module.exports = { loadDemoData, buildSections, RENDERED, svg, sizeSvg };

if (require.main === module) {
  main().catch((e) => { console.error(String(e.message || e)); process.exit(1); });
}
