#!/usr/bin/env node
/*
 * GT Warehouse Mobile -- demo product imagery.
 *
 *   node gtwms/tools/gen-demo-images.js
 *
 * Renders two images per product family, writes them to /tmp/gtimg as JPEGs,
 * and emits SQL that loads them into GTIMAGE as BLOBs.
 *
 * WHY GENERATED RATHER THAN SOURCED
 * ---------------------------------
 * Stock photography would put a licensing question in the middle of a customer
 * meeting. These are obviously-synthetic product cards: clean, on-brand, and
 * clearly demo assets. Real Giant Tiger photography drops straight in later --
 * the pipeline does not care what the pixels are.
 *
 * WHY TWO ANGLES
 * --------------
 * The carousel is the point of Item Lookup. One image per item makes it look
 * broken; two makes the swipe meaningful without doubling the seed size.
 *
 * SIZE DISCIPLINE
 * ---------------
 * Every image is loaded as a hex literal in SQL, which doubles its size on the
 * way in, so the images are deliberately modest: 480px full, 160px thumbnail.
 * That is ample for a phone carousel and keeps the generated SQL manageable.
 * The output is split per department so no single RUNSQLSTM source is huge.
 */

'use strict';

const fs = require('fs');
const path = require('path');
const { chromium } = require('playwright-core');

const CHROME = '/home/coder/.cache/ms-playwright/chromium-1228/chrome-linux64/chrome';
const OUTDIR = '/tmp/gtimg';
const SQLDIR = path.join(__dirname, '..', 'qsqlsrc');

/* Department palettes. Distinct enough that a carousel of different items
 * reads as different products at a glance. */
const DEPTS = {
  GROC: { name: 'Grocery',         a: '#E31837', b: '#8C0F22' },
  CLEA: { name: 'Cleaning',        a: '#1D4ED8', b: '#0B2A78' },
  HLTH: { name: 'Health & Beauty', a: '#6D28D9', b: '#3B157A' },
  HOME: { name: 'Home',            a: '#0F766E', b: '#08403B' },
  APPL: { name: 'Apparel',         a: '#B45309', b: '#6A320A' },
  SEAS: { name: 'Seasonal',        a: '#127C3B', b: '#0A4622' },
  PETS: { name: 'Pet Supplies',    a: '#BE185D', b: '#6D0E36' },
  TOYS: { name: 'Toys',            a: '#FFCD00', b: '#B38F00' },
};

/* Two views per family. The second is deliberately different in composition,
 * not just rotated, so a swipe obviously changes something. */
const VIEWS = [
  { seq: 1, label: 'Front',  labelFr: 'Avant' },
  { seq: 2, label: 'Detail', labelFr: 'Détail' },
];

function esc(s) {
  return String(s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
}

/* A product card. Kept deliberately flat and graphic -- a fake photograph
 * would look worse than an honest illustration. */
function cardHtml(family, dept, view, size) {
  const d = DEPTS[dept] || DEPTS.GROC;
  const big = size >= 400;
  const initials = family.split(/\s+/).slice(0, 2).map((w) => w[0]).join('').toUpperCase();

  return `<!doctype html><meta charset="utf-8"><style>
    *{margin:0;padding:0;box-sizing:border-box}
    body{width:${size}px;height:${size}px;overflow:hidden;
         font-family:-apple-system,"Segoe UI",Roboto,Helvetica,Arial,sans-serif}
    .w{width:100%;height:100%;position:relative;
       background:linear-gradient(150deg,${d.a} 0%,${d.b} 100%);
       display:flex;flex-direction:column;align-items:center;justify-content:center;
       padding:${big ? 28 : 12}px;text-align:center;color:#fff}
    .box{background:rgba(255,255,255,.14);border:${big ? 3 : 2}px solid rgba(255,255,255,.5);
         border-radius:${big ? 18 : 8}px;padding:${big ? '22px 26px' : '8px 10px'};
         backdrop-filter:blur(2px);max-width:92%}
    .ini{font-weight:800;font-size:${big ? 68 : 26}px;line-height:1;letter-spacing:-2px;
         text-shadow:0 2px 8px rgba(0,0,0,.3)}
    .nm{font-weight:700;font-size:${big ? 19 : 9}px;line-height:1.25;margin-top:${big ? 12 : 5}px;
        text-shadow:0 1px 4px rgba(0,0,0,.35)}
    .dp{position:absolute;top:${big ? 16 : 6}px;left:${big ? 16 : 6}px;
        background:#FFCD00;color:#000;font-weight:800;font-size:${big ? 12 : 7}px;
        letter-spacing:.6px;padding:${big ? '5px 10px' : '2px 5px'};border-radius:999px}
    .vw{position:absolute;bottom:${big ? 16 : 6}px;right:${big ? 16 : 6}px;
        background:rgba(0,0,0,.4);font-weight:700;font-size:${big ? 12 : 7}px;
        letter-spacing:.6px;padding:${big ? '5px 10px' : '2px 5px'};border-radius:999px}
    ${view.seq === 2 ? `.w{background:linear-gradient(30deg,${d.b} 0%,${d.a} 100%)}
       .box{transform:rotate(-3deg)}` : ''}
  </style><div class="w">
    <span class="dp">${esc(d.name)}</span>
    <div class="box"><div class="ini">${esc(initials)}</div>
      <div class="nm">${esc(family)}</div></div>
    <span class="vw">${esc(view.label)}</span>
  </div>`;
}

(async () => {
  /* Families come from the demo-data generator so the two stay in step. */
  const gen = fs.readFileSync(path.join(__dirname, 'gen-demo-data.js'), 'utf8');
  /* Bound the slice on the next const, not on a comment: the comment text
   * drifted and the regex silently swallowed the REASONS array as well,
   * producing 17 bogus "departments" named after reason codes. */
  const famStart = gen.indexOf('const FAMILIES = [');
  const famEnd = gen.indexOf('const REASONS', famStart);
  if (famStart < 0 || famEnd < 0) throw new Error('could not bound FAMILIES in gen-demo-data.js');
  const famBlock = gen.slice(famStart, famEnd);
  const families = [...famBlock.matchAll(/\[\s*['"]([A-Z]{4})['"],\s*(['"])((?:\\.|(?!\2).)*)\2/g)]
    .map((m) => ({ dept: m[1], en: m[3].replace(/\\'/g, "'") }));

  if (!families.length) throw new Error('could not read FAMILIES from gen-demo-data.js');
  /* Guard against the slice drifting again: the departments must be the
   * eight real ones, not whatever else happens to match the pattern. */
  const known = Object.keys(DEPTS);
  const stray = [...new Set(families.map((f) => f.dept))].filter((d) => known.indexOf(d) === -1);
  if (stray.length) throw new Error('unexpected departments parsed: ' + stray.join(', '));
  console.log('families:', families.length, 'across', new Set(families.map(f => f.dept)).size, 'departments');

  fs.rmSync(OUTDIR, { recursive: true, force: true });
  fs.mkdirSync(OUTDIR, { recursive: true });

  const browser = await chromium.launch({ executablePath: CHROME, args: ['--no-sandbox'] });
  const rows = [];

  for (let f = 0; f < families.length; f++) {
    const fam = families[f];
    for (const view of VIEWS) {
      const shot = async (size) => {
        const page = await browser.newPage({ viewport: { width: size, height: size } });
        await page.setContent(cardHtml(fam.en, fam.dept, view, size), { waitUntil: 'load' });
        const buf = await page.screenshot({ type: 'jpeg', quality: size >= 400 ? 72 : 68 });
        await page.close();
        return buf;
      };
      const full = await shot(480);
      const thumb = await shot(160);

      /* ref_key is the family index, so every item variant in that family
       * shares the same imagery -- 400 items, 102 images. */
      const refKey = 'FAM' + String(f + 1).padStart(4, '0');
      fs.writeFileSync(path.join(OUTDIR, refKey + '-' + view.seq + '.jpg'), full);

      rows.push({
        dept: fam.dept, refKey, seq: view.seq,
        capEn: fam.en + ' - ' + view.label,
        capFr: fam.en + ' - ' + view.labelFr,
        full: full.toString('hex').toUpperCase(),
        thumb: thumb.toString('hex').toUpperCase(),
        bytes: full.length,
      });
    }
    if ((f + 1) % 10 === 0) console.log('  rendered', f + 1, 'families');
  }
  await browser.close();

  /* Split per department so no single RUNSQLSTM source gets unwieldy. */
  const byDept = {};
  rows.forEach((r) => { (byDept[r.dept] = byDept[r.dept] || []).push(r); });

  const targets = [];
  Object.keys(byDept).sort().forEach((dept) => {
    const chunk = byDept[dept];
    const name = 'gtimg' + dept.toLowerCase();
    const out = [];
    out.push('--  GT Warehouse Mobile -- generated product imagery: ' + dept);
    out.push('--  GENERATED FILE. Produced by gtwms/tools/gen-demo-images.js.');
    out.push('--  Synthetic on purpose: real photography would raise a licensing');
    out.push('--  question in a customer meeting, and drops in later unchanged.');
    out.push('');
    /* Scoped to this file's own ref_keys. A blanket "LIKE 'FAM%'" delete
     * would have each department file wipe the ones loaded before it, so only
     * the last department would survive -- and nothing would look wrong until
     * most items showed no image. */
    const keys = [...new Set(chunk.map((r) => "'" + r.refKey + "'"))].join(', ');
    out.push("DELETE FROM GTIMAGE WHERE ref_type = 'ITEM' AND ref_key IN (" + keys + ');');
    out.push('');
    chunk.forEach((r) => {
      out.push('INSERT INTO GTIMAGE (ref_type, ref_key, seq_no, mime_type, width_px,');
      out.push('       height_px, byte_size, image_data, thumb_data, caption_en,');
      out.push('       caption_fr, committed_flag)');
      out.push("VALUES ('ITEM', '" + r.refKey + "', " + r.seq + ", 'image/jpeg', 480, 480, " +
               r.bytes + ',');
      out.push("        BX'" + r.full + "',");
      out.push("        BX'" + r.thumb + "',");
      out.push("        '" + r.capEn.replace(/'/g, "''") + "', '" +
               r.capFr.replace(/'/g, "''") + "', 'Y');");
      out.push('');
    });
    const file = path.join(SQLDIR, name + '.table.sql');
    fs.writeFileSync(file, out.join('\n'));
    targets.push({ name, file, count: chunk.length, kb: Math.round(fs.statSync(file).size / 1024) });
  });

  console.log('\nimages rendered :', rows.length, '(' + families.length + ' families x ' + VIEWS.length + ')');
  console.log('jpegs written   :', OUTDIR);
  targets.forEach((t) => console.log('  ' + t.name.padEnd(12) + String(t.count).padStart(3) +
                                     ' images  ' + String(t.kb).padStart(5) + ' KB sql'));
  console.log('\nAdd to Rules.mk / rebuild:');
  console.log('  ' + targets.map((t) => t.name + '.file').join(' '));
})();
