#!/usr/bin/env node
/* Drive the VW web screens like a user and assert what happens.

   node prfdemolib/web/tools/drive.js

   Same page as render.js (real templates, CSS and JS; Genie skin rules
   reproduced; every network request refused), fed with fixtures captured
   from live TS0110W / INV502W / INV046W sessions. Each check states what a
   user would expect; the run exits 1 if any fails. Includes negative
   controls: checks that must fail when the behaviour is broken on purpose. */
'use strict';
const fs = require('fs');
const path = require('path');
const SKILL = path.join(process.env.HOME, '.claude/skills/dspf-to-ejs/node_modules');
const { chromium } = require(path.join(SKILL, 'playwright-core'));
const REPO = path.resolve(__dirname, '../../..');
const HT = path.join(REPO, 'htdocs');
const FX = path.join(__dirname, '..', 'fixtures');
const EJS = fs.readFileSync(path.join(SKILL, 'ejs/ejs.min.js'), 'utf8');
/* both forms of Genie's blanket rule: Classic's bare div, and the Skyline
   lineage's [id="5250"] div (0,1,1) - the one that outranks a one-class reset */
const GENIE = 'div{padding:1px;z-index:10;white-space:nowrap}[id="5250"] div{z-index:10;padding:1px;white-space:nowrap}img{z-index:10}INPUT{font-family:Monospace;font-size:13px;z-index:20}SELECT{z-index:20}BUTTON{z-index:20}body{margin:0}';
const file = (u) => path.join(HT, u.split('?')[0]);

function dataOf(fx) {
  const d = {};
  for (const [k, v] of Object.entries(fx.data || {})) { d[k.toLowerCase()] = v; d[k.toUpperCase()] = v; }
  for (const [n, sf] of Object.entries(fx.subfiles || {})) {
    d[n.toLowerCase()] = sf.rows.map((r, i) => { const o = { _rrn: i + 1 }; sf.fields.forEach((f, j) => { o[f.toLowerCase()] = r[j]; }); return o; });
  }
  d.flags = [];
  return d;
}

let pass = 0, fail = 0;
const ok = (cond, msg) => { if (cond) { pass++; console.log('  ok   ' + msg); } else { fail++; console.log('  FAIL ' + msg); } };

async function open(browser, name, opts = {}) {
  const fx = JSON.parse(fs.readFileSync(path.join(FX, name + '.json'), 'utf8'));
  const page = await browser.newPage({ viewport: { width: opts.width || 1280, height: 900 } });
  page.errors = [];
  page.on('pageerror', (e) => page.errors.push(e.message));
  await page.route('**/*', (r) => r.request().url().startsWith('http://vw.test/')
    ? r.fulfill({ contentType: 'text/html', body: `<!doctype html><html><head><style>${GENIE}</style></head><body><div id="5250"><div id="g"></div></div></body></html>` })
    : r.abort());
  await page.goto('http://vw.test/');
  if (opts.storage) await page.evaluate((s) => { for (const [k, v] of Object.entries(s)) (k.startsWith('ss:') ? sessionStorage : localStorage).setItem(k.replace(/^ss:/, ''), v); }, opts.storage);
  for (const c of fx.css) await page.addStyleTag({ content: fs.readFileSync(file(c), 'utf8') });
  await page.addScriptTag({ content: 'window.__posts=[];window.pui={submit:function(o){window.__posts.push(JSON.parse(JSON.stringify(o)));setTimeout(function(){var r=document.querySelector(\'.vw-app\');if(r)r.classList.remove(\'is-busy\');},0);}};' + EJS });
  const tpl = fs.readFileSync(file(fx.template), 'utf8');
  const res = await page.evaluate(([t, d]) => { try { document.getElementById('g').innerHTML = window.ejs.render(t, d); return 'ok'; } catch (e) { return e.message; } }, [tpl, dataOf(fx)]);
  if (res !== 'ok') page.errors.push('EJS: ' + res);
  if (!opts.nojs) for (const j of fx.js) await page.addScriptTag({ content: fs.readFileSync(file(j), 'utf8') });
  await page.waitForTimeout(200);
  return page;
}
const posts = (p) => p.evaluate(() => window.__posts);
const text = (p, sel) => p.$eval(sel, (e) => e.textContent.trim());

(async () => {
  const exe = process.env.CHROMIUM_PATH || path.join(process.env.HOME, '.cache/ms-playwright/chromium_headless_shell-1228/chrome-headless-shell-linux64/chrome-headless-shell');
  const browser = await chromium.launch({ executablePath: exe, args: ['--no-sandbox', '--disable-dev-shm-usage'] });

  console.log('TS0110W SKU list (store 205, dept 86)');
  let p = await open(browser, 'ts-skuctl-205-86');
  ok(p.errors.length === 0, 'renders with no page errors ' + p.errors.join('; '));
  ok(await p.$$eval('tr[data-sku-row]', (r) => r.length) === 10, '10 SKU rows, matching SKUCNT 10');
  ok(await p.$$eval('tr.vw-grp', (r) => r.length) === 6 && await p.$$eval('tr.vw-tot', (r) => r.length) === 6 && await p.$$eval('tfoot tr', (r) => r.length) === 1,
     'SFIND decoded: 6 headings (2 subdept, 4 class), 6 subtotals, final total in the footer');
  // stepper + live totals
  await p.click('tr[data-rrn="3"] .vw-step button:last-child');
  await p.click('tr[data-rrn="3"] .vw-step button:last-child');
  ok(await p.$eval('tr[data-rrn="3"] .vw-qty', (i) => i.value) === '2', '+ twice on SKU 880110 keys 2 packs');
  ok(await text(p, '[data-dist="units"]') === '24' && await text(p, '[data-dist="retail"]') === '$48.00', 'summary: 2 packs x 12 = 24 units, $48.00 retail ($2.00 each)');
  await p.focus('tr[data-rrn="3"] .vw-qty'); await p.keyboard.press('Minus');
  ok(await p.$eval('tr[data-rrn="3"] .vw-qty', (i) => i.value) === '1', 'the - key removes a pack');
  await p.keyboard.press('ArrowDown');
  ok(await p.evaluate(() => document.activeElement.name) === 'skusfl.sfqty.4', 'Arrow Down moves to the next SKU pack field');
  // over-allocation warning
  await p.fill('tr[data-rrn="4"] .vw-qty', '999'); await p.dispatchEvent('tr[data-rrn="4"] .vw-qty', 'input');
  ok(await p.$eval('tr[data-rrn="4"] .vw-qty', (i) => i.classList.contains('is-over')), '999 packs on 880111 (468 available) is flagged as over-allocated');
  ok(/more packs than the warehouse/.test(await text(p, '[data-dist="detail"]')), 'summary explains the over-allocation');
  await p.fill('tr[data-rrn="4"] .vw-qty', ''); await p.dispatchEvent('tr[data-rrn="4"] .vw-qty', 'input');
  // smart fill
  await p.click('text=Smart fill');
  const sf = await p.$$eval('tr[data-sku-row]', (rows) => rows.map((r) => [+r.getAttribute('data-pct'), +r.getAttribute('data-avl'), r.querySelector('.vw-qty').value, r.getAttribute('data-rrn')]));
  const expectSf = sf.filter(([pct, avl, , rrn]) => rrn !== '3' && pct >= 85 && avl >= 1);
  ok(expectSf.every(([pct, avl, v]) => +v === Math.min(avl, pct >= 95 ? 2 : 1)), 'Smart fill keys 1 pack at 85%+ and 2 at 95%+ (' + expectSf.length + ' SKUs)');
  ok(sf.find((r) => r[3] === '3')[2] === '1', 'Smart fill leaves an already keyed SKU alone');
  ok(sf.filter(([pct]) => pct < 85).every(([, , v]) => v === ''), 'Smart fill does not touch slower sellers');
  await p.focus('#vw-dist .vw-btn-ghost');
  const dz = await p.evaluate(() => { const d = document.querySelector('#vw-dist').getBoundingClientRect(); const t = document.querySelector('.vw-tablebar').getBoundingClientRect(); return { gap: Math.round(t.top - d.bottom), pos: getComputedStyle(document.querySelector('#vw-dist')).position }; });
  ok(dz.pos === 'sticky' && dz.gap >= 0, 'focusing inside the distribution bar keeps it sticky and off the filter bar (gap ' + dz.gap + 'px)');
  // chips (scroll to the top first, as a user sees them: Playwright would park the
  // chip flush under the sticky top bar + distribution bar)
  await p.evaluate(() => window.scrollTo(0, 0));
  await p.click('.vw-chip[data-chip="qty"]');
  const vis = await p.$$eval('tr[data-sku-row]', (rows) => rows.filter((r) => r.offsetParent !== null).length);
  const keyed = await p.$$eval('tr[data-sku-row]', (rows) => rows.filter((r) => r.querySelector('.vw-qty').value).length);
  ok(vis === keyed && vis > 0, '"In distribution" chip shows only the ' + keyed + ' keyed SKUs');
  ok(await p.$$eval('tr.vw-grp-sub', (r) => r.filter((x) => x.offsetParent !== null).length) >= 1, 'group headings with visible SKUs stay visible');
  await p.evaluate(() => window.scrollTo(0, 0));
  await p.click('.vw-chip[data-chip="qty"]');
  // keyboard navigation keeps the focused pack field clear of the sticky bars
  await p.focus('tr[data-rrn="3"] .vw-qty');
  for (let k = 0; k < 6; k++) await p.keyboard.press('ArrowDown');
  const clear = await p.evaluate(() => {
    const f = document.activeElement.getBoundingClientRect();
    const bars = ['.vw-topbar', '#vw-dist'].map((q) => document.querySelector(q).getBoundingClientRect().bottom);
    const hit = document.elementFromPoint(f.left + f.width / 2, f.top + f.height / 2);
    return { ok: hit === document.activeElement || document.activeElement.contains(hit), top: Math.round(f.top), bars: bars.map(Math.round) };
  });
  ok(clear.ok, 'Arrow Down to a lower SKU keeps its pack field visible below the sticky bars (field top ' + clear.top + ', bars end ' + clear.bars.join('/') + ')');
  // F10 handoff
  await p.click('text=Create distribution');
  const p10 = await posts(p);
  ok(p10.length === 1 && p10[0].action === 'F10', 'Create distribution posts ACTION=F10 (the green screen F10)');
  const handoff = JSON.parse(await p.evaluate(() => sessionStorage.getItem('vw.dist')));
  ok(handoff && handoff.store === '205' && handoff.lines.length === keyed, 'F10 hands the ' + keyed + '-line summary to the confirmation screen');
  // F7/F8 stay in the browser
  await p.evaluate(() => { window.__posts = []; });
  await p.click('button[data-fkey="F8"]');
  ok((await posts(p)).length === 0, 'F8 (bottom) scrolls in the browser - no round trip');
  ok(p.errors.length === 0, 'no page errors after interaction ' + p.errors.join('; '));
  await p.close();

  console.log('TS0110W confirmation (DISTWIN) with the hand-off');
  p = await open(browser, 'ts-distwin', { storage: { 'ss:vw.dist': JSON.stringify(handoff) } });
  ok(new RegExp(handoff.packs + ' pack').test(await text(p, '#vw-dist-h')), 'heading states the packs (' + handoff.packs + ') and store');
  ok(await p.$$eval('.vw-confirm-table tbody tr', (r) => r.length) === handoff.lines.length, 'one line per keyed SKU');
  await p.click('button[data-fkey="Enter"]');
  ok((await posts(p))[0].action === 'ENTER', 'Create posts ACTION=ENTER (the window Enter)');
  ok(/Distribution submitted/.test(await p.evaluate(() => localStorage.getItem('vw.notice'))), 'leaves a toast for the next screen');
  await p.close();
  p = await open(browser, 'ts-distwin');
  ok(/Create a distribution\?/.test(await text(p, '#vw-dist-h')), 'without a hand-off it still renders a plain confirmation');
  await p.close();

  console.log('TS0110W prompt');
  p = await open(browser, 'ts-prompt');
  await p.fill('#vw-dfstor', 'laurel'); await p.dispatchEvent('#vw-dfstor', 'input');
  ok(await p.$$eval('.vw-picker-list li', (l) => l.length) === 1 && await p.$eval('.vw-picker-list li b', (b) => b.textContent) === '205', 'typing "laurel" in Store suggests store 205');
  await p.keyboard.press('Enter');
  ok(await p.$eval('#vw-dfstor', (i) => i.value) === '205', 'Enter on a typed name picks the suggestion instead of submitting text');
  await p.fill('#vw-dfdept', '86');
  await p.click('.vw-ts-switches label:nth-child(1)');
  await p.click('button[data-fkey="Enter"]');
  const pp = (await posts(p))[0];
  ok(pp && pp.action === 'ENTER' && pp.dfstor === '205' && pp.dfdept === '86' && pp.tckyn === 'N' && pp.avlyn === 'Y' && pp.dfseas === 'ALL',
     'Show SKUs posts store, dept, season and the three switches explicitly ' + JSON.stringify(pp));
  await p.close();
  p = await open(browser, 'ts-prompt-badstore');
  ok(await p.$eval('#vw-dfstor', (i) => i.classList.contains('is-error')) && /Invalid store/.test(await text(p, '.vw-field-err')), '*IN90 (invalid store) outlines the field and says why');
  await p.close();

  console.log('INV502W SKU prompt and detail');
  p = await open(browser, 'inv502-screen1');
  await p.fill('#vw-sku', 'sherpa'); await p.dispatchEvent('#vw-sku', 'input');
  ok(await p.$eval('.vw-picker-list li b', (b) => b.textContent) === '880209', 'SKU finder: "sherpa" finds 880209 (replaces the F3 search program)');
  await p.keyboard.press('Enter');
  await p.click('button[data-fkey="Enter"]');
  ok(JSON.stringify((await posts(p))[0]) === JSON.stringify({ inumbr: '880209', action: 'ENTER' }), 'Look up posts INUMBR=880209 ACTION=ENTER');
  await p.close();
  p = await open(browser, 'inv502-screen2c-262657');
  ok(await text(p, '.vw-kpi:nth-child(1) .vw-kpi-value') === '183,802', 'network on hand = 92,250 + 91,552 = 183,802');
  ok(/73\.6% margin/.test(await text(p, '.vw-kpi:nth-child(5) .vw-kpi-note')), 'margin (5.99 - 1.58) / 5.99 = 73.6%');
  await p.click('[data-vw-next]');
  ok((await posts(p))[0].action === 'PAGEDOWN', 'Next posts PAGEDOWN (Page Down = next SKU, as on the 5250)');
  await p.evaluate(() => { window.__posts = []; });
  await p.keyboard.press('Control+k');
  ok(await p.$eval('#vw-palette', (d) => !d.hidden), 'Ctrl+K opens the command palette');
  await p.keyboard.type('export');
  ok(/Export warehouse balances/.test(await text(p, '#vw-palette-list')), 'palette finds "Export warehouse balances to CSV"');
  await p.keyboard.press('Escape');
  ok(await p.$eval('#vw-palette', (d) => d.hidden), 'Esc closes it');
  ok(JSON.parse(await p.evaluate(() => localStorage.getItem('vw.recentSkus')))[0].v === '262657', 'detail screen records 262657 in Recently viewed');
  await p.close();
  p = await open(browser, 'inv502-screen2c-262657-badsku');
  ok(!(JSON.parse(await p.evaluate(() => localStorage.getItem('vw.recentSkus')) || '[]')).length, 'an invalid SKU (INV0297) is not recorded as recently viewed');
  ok(/INV0297/.test(await text(p, '.vw-alert.is-bad')), 'INV0297 message shown from SndMsgW');
  await p.close();

  console.log('INV046W search');
  p = await open(browser, 'inv046-grp740');
  await p.click('.vw-f-find .vw-seg-btn[data-v="I"]');
  ok(await p.$eval('#vw-inrds', (i) => i.value) === 'I' && await p.$eval('#vw-instr', (i) => i.getAttribute('data-kind')) === 'I', 'Search for = Inv group sets INRDS and retargets the number picker');
  ok(await text(p, '#vw-instr-l') === 'Inventory group', 'the number field relabels itself');
  await p.fill('#vw-instr', ''); await p.dispatchEvent('#vw-instr', 'input');
  ok(await p.$$eval('.vw-f-num .vw-picker-list li', (l) => l.length) === 4, 'picker lists the 4 inventory groups');
  await p.click('.vw-f-num .vw-picker-list li:nth-child(3)', { force: true });
  await p.click('.vw-f-type .vw-seg-btn[data-v="S"]');
  await p.click('button[data-fkey="Enter"]');
  const ip = (await posts(p))[0];
  ok(ip.action === 'ENTER' && ip.inumbr === '262657' && ip.intype === 'S' && ip.inrds === 'I' && ip.instr === '30', 'Search posts INUMBR/INTYPE/INRDS/INSTR explicitly ' + JSON.stringify(ip));
  await p.evaluate(() => { window.__posts = []; });
  await p.fill('.vw-tablebar input', 'pearl'); await p.dispatchEvent('.vw-tablebar input', 'input');
  ok(await p.$$eval('#vw-stores tbody tr', (r) => r.filter((x) => x.offsetParent !== null).length) === 1, 'filter "pearl" (a city, not on the 5250) leaves store 262');
  ok((await posts(p)).length === 0, 'filtering never round-trips');
  await p.fill('.vw-tablebar input', ''); await p.dispatchEvent('.vw-tablebar input', 'input');
  await p.click('th:nth-child(10) .vw-sort'); await p.click('th:nth-child(10) .vw-sort');
  ok(await p.$eval('#vw-stores tbody tr td.vw-cell-head', (c) => c.getAttribute('data-v')) === '215', 'sorting Supply high-to-low puts store 215 (90 wk) first');
  await p.click('.vw-period .vw-seg-btn:nth-child(2)');
  ok((await posts(p))[0].action === 'F05', 'Year to date posts F05, as the green screen F5');
  await p.close();


  console.log('Dropdowns are on top and wide enough (inside #5250 with the real Genie skin rule)');
  const occl = async (pg, inputSel, typed, label) => {
    // close whatever list is open, as a user would (Esc / click away)
    await pg.evaluate(() => { if (document.activeElement) document.activeElement.blur(); });
    await pg.waitForTimeout(200);
    await pg.click(inputSel);
    await pg.fill(inputSel, typed); await pg.dispatchEvent(inputSel, 'input');
    await pg.waitForTimeout(100);
    const r = await pg.evaluate((sel) => {
      const inp = document.querySelector(sel);
      const list = inp.closest('.vw-picker').querySelector('.vw-picker-list');
      if (!list || list.hidden) return { err: 'list not open' };
      const lb = list.getBoundingClientRect();
      const items = Array.from(list.querySelectorAll('li')).filter((li) => { const b = li.getBoundingClientRect(); return b.bottom <= lb.bottom && b.top >= lb.top; });
      let covered = 0, n = 0, cut = 0, over = [];
      for (const li of items) {
        const b = li.getBoundingClientRect();
        for (const fx of [0.1, 0.5, 0.9]) {
          const x = b.left + b.width * fx, y = b.top + b.height / 2;
          if (y < 0 || y > innerHeight) continue;
          n++;
          const hit = document.elementFromPoint(x, y);
          if (!hit || !list.contains(hit)) { covered++; if (over.length < 2) over.push(hit ? (hit.className || hit.tagName) : 'none'); }
        }
        const sp = li.querySelector('span');
        if (sp && sp.scrollWidth > sp.clientWidth + 1) cut++;
      }
      let clip = null;
      for (let a = list.parentElement; a && a !== document.body; a = a.parentElement) {
        const cs = getComputedStyle(a);
        if (cs.overflowX !== 'visible' || cs.overflowY !== 'visible') { const ab = a.getBoundingClientRect(); if (lb.bottom > ab.bottom + 1 || lb.right > ab.right + 1) { clip = a.className || a.tagName; break; } }
      }
      return { n, covered, over, cut, items: items.length, width: Math.round(lb.width), right: Math.round(lb.right), vw: innerWidth, clip };
    }, inputSel);
    ok(!r.err && r.items > 0, label + ': list opens (' + (r.items || 0) + ' items, ' + r.width + 'px wide)');
    ok(!r.err && r.covered === 0, label + ': nothing paints over it (' + (r.covered || 0) + '/' + (r.n || 0) + ' probes covered' + (r.over && r.over.length ? ' by ' + r.over.join(', ') : '') + ')');
    ok(!r.err && r.cut === 0, label + ': descriptions shown in full (' + (r.cut || 0) + ' cut off)');
    ok(!r.err && !r.clip && r.right <= r.vw, label + ': not clipped by a container or the window' + (r.clip ? ' (clipped by ' + r.clip + ')' : ''));
  };
  for (const w of [1440, 1024, 390]) {
    p = await open(browser, 'inv046-grp740', { width: w });
    await occl(p, '#vw-instr', '', 'INV046 @' + w + ' store-group/number picker (narrow field)');
    await p.click('.vw-f-find .vw-seg-btn[data-v="D"]');
    ok(await p.$eval('#vw-instr', (i) => i.value) === '' && await p.$$eval('.vw-f-num .vw-picker-list li', (l) => l.length) === 10,
       'INV046 @' + w + ' switching Search for to District clears the old number and lists all 10 districts');
    await occl(p, '#vw-instr', '', 'INV046 @' + w + ' district picker');
    await occl(p, '#vw-inumbr', 'a', 'INV046 @' + w + ' SKU finder');
    await p.close();
    p = await open(browser, 'inv502-screen2c-262657', { width: w });
    await occl(p, '#vw-newsku', 'bra', 'INV502 detail @' + w + ' SKU finder (above the KPI tiles)');
    await p.close();
    p = await open(browser, 'inv502-screen1', { width: w });
    await occl(p, '#vw-sku', 'a', 'INV502 prompt @' + w + ' SKU finder (above the recents cards)');
    await p.close();
    p = await open(browser, 'ts-prompt', { width: w });
    await occl(p, '#vw-dfstor', '', 'TS0110 prompt @' + w + ' store picker');
    await occl(p, '#vw-dfdept', '', 'TS0110 prompt @' + w + ' department picker (above the quick picks)');
    await p.close();
  }
  p = await open(browser, 'inv502-screen2c-262657', { width: 1280 });
  await p.evaluate(() => window.scrollTo(0, 1400));
  await p.keyboard.press('Control+k');
  const pal = await p.evaluate(() => { const d = document.querySelector('#vw-palette .vw-dialog').getBoundingClientRect(); return { top: Math.round(d.top), bottom: Math.round(d.bottom), h: innerHeight }; });
  ok(pal.top >= 0 && pal.top < pal.h * 0.5, 'command palette opens in view even when scrolled down (top=' + pal.top + ')');
  await p.close();

  console.log('Render without JS (the screen must not depend on app.js)');
  for (const f of ['ts-skuctl-205-86', 'inv502-screen2c-262657', 'inv046-grp740', 'ts-prompt']) {
    p = await open(browser, f, { nojs: true });
    ok(p.errors.length === 0 && await p.$('.vw-app') !== null, f + ' renders with no JS');
    await p.click('button[data-fkey="Enter"], button[data-fkey="F10"]').catch(() => {});
    ok((await posts(p)).length === 1, f + ' main button still submits through the template bootstrap');
    await p.close();
  }

  console.log('Negative control');
  p = await open(browser, 'ts-skuctl-205-86');
  await p.evaluate(() => { window.VWts.recalc = function () {}; });
  await p.click('tr[data-rrn="3"] .vw-step button:last-child');
  ok(await text(p, '[data-dist="units"]') === '0', 'with recalc broken on purpose the summary stays 0 (so the live-total check above is real)');
  await p.close();

  await browser.close();
  console.log(`\n${pass} passed, ${fail} failed`);
  process.exit(fail ? 1 : 0);
})();
