#!/usr/bin/env node
/* Render a VW web screen locally, exactly as Profound UI would, and check it.

   node prfdemolib/web/tools/render.js <fixture.json> [--out dir] [--widths 1440,1024,768,390]
                                       [--theme light|dark] [--nojs] [--click selector]...

   A fixture is the active format of a live RDF screen captured with the
   ibmi-interactive-session skill (prfdemolib/web/tools/fixture.js makes one):
     { "format": "SCREEN2C", "template": "/profoundui/userdata/ui/...ejs?v=1",
       "css": [...], "js": [...], "data": {FIELD: value}, "subfiles": {NAME: {fields:[], rows:[[]]}} }

   The template is rendered with Profound UI's data contract (every field as
   lower AND upper case, each subfile as a top-level lower-case array of row
   objects with _rrn). It runs inside a page that reproduces the Genie skin
   rules that break EJS screens (div{padding:1px;z-index:10;white-space:nowrap},
   INPUT{font-family:Monospace;font-size:13px}). Fails (exit 1) on any page
   error, console error, horizontal overflow, or with --nojs a screen that
   does not render. */
'use strict';
const fs = require('fs');
const path = require('path');
const SKILL = path.join(process.env.HOME, '.claude/skills/dspf-to-ejs/node_modules');
const ejs = require(path.join(SKILL, 'ejs'));
const { chromium } = require(path.join(SKILL, 'playwright-core'));

const REPO = path.resolve(__dirname, '../../..');
const HT = path.join(REPO, 'htdocs');
const argv = process.argv.slice(2);
const fx = JSON.parse(fs.readFileSync(argv[0], 'utf8'));
const opt = (k, d) => { const i = argv.indexOf(k); return i >= 0 ? argv[i + 1] : d; };
const all = (k) => argv.reduce((a, v, i) => (v === k ? a.concat(argv[i + 1]) : a), []);
const outDir = opt('--out', '/tmp/vwrender');
const widths = opt('--widths', '1440,1024,768,390').split(',').map(Number);
const theme = opt('--theme', 'light');
const nojs = argv.includes('--nojs');
const clicks = all('--click');
const evals = all('--eval');
const tag = opt('--tag', path.basename(argv[0], '.json'));
fs.mkdirSync(outDir, { recursive: true });

const file = (u) => path.join(HT, u.split('?')[0]);
const data = {};
for (const [k, v] of Object.entries(fx.data || {})) { data[k.toLowerCase()] = v; data[k.toUpperCase()] = v; }
for (const [name, sf] of Object.entries(fx.subfiles || {})) {
  data[name.toLowerCase()] = sf.rows.map((r, i) => {
    const o = { _rrn: i + 1 };
    sf.fields.forEach((f, j) => { o[f.toLowerCase()] = r[j]; o[f.toUpperCase()] = r[j]; });
    return o;
  });
}
data.flags = [];
const tpl = fs.readFileSync(file(fx.template), 'utf8');

const GENIE = `div{padding:1px;z-index:10;white-space:nowrap}[id="5250"] div{z-index:10;padding:1px;white-space:nowrap}img{z-index:10}INPUT{font-family:Monospace;font-size:13px;z-index:20}SELECT{z-index:20}BUTTON{z-index:20}body{margin:0}#genie{position:relative}`;

(async () => {
  const exe = process.env.CHROMIUM_PATH ||
    path.join(process.env.HOME, '.cache/ms-playwright/chromium_headless_shell-1228/chrome-headless-shell-linux64/chrome-headless-shell');
  const browser = await chromium.launch({ executablePath: exe, args: ['--no-sandbox', '--disable-dev-shm-usage'] });
  let failures = 0;
  const report = [];
  for (const w of widths) {
    const page = await browser.newPage({ viewport: { width: w, height: 900 }, deviceScaleFactor: 1 });
    const errors = [];
    page.on('pageerror', (e) => errors.push('pageerror: ' + e.message));
    page.on('console', (m) => { if (m.type() === 'error') errors.push('console: ' + m.text()); });
    // a real origin so localStorage works; every other request is refused -
    // nothing on the screen may depend on a second fetch
    await page.route('**/*', (r) => r.request().url().startsWith('http://vw.test/')
      ? r.fulfill({ contentType: 'text/html', body: `<!doctype html><html><head><style>${GENIE}</style></head><body><div id="5250"><div id="genie"></div></div></body></html>` })
      : r.abort());
    await page.goto('http://vw.test/');
    await page.evaluate((t) => { try { localStorage.setItem('vw.theme', JSON.stringify(t)); } catch (e) {} }, theme);
    for (const c of fx.css) await page.addStyleTag({ content: fs.readFileSync(file(c), 'utf8') });
    await page.addScriptTag({ content: 'window.pui={submit:function(o){window.__submitted=o;}};' });
    // render client-side like Profound UI does (localStorage etc. available)
    await page.addScriptTag({ content: fs.readFileSync(path.join(SKILL, 'ejs/ejs.min.js'), 'utf8') });
    const rendered = await page.evaluate(([t, d]) => {
      try { document.getElementById('genie').innerHTML = window.ejs.render(t, d); return 'ok'; }
      catch (e) { return 'EJS ERROR: ' + e.message; }
    }, [tpl, data]);
    if (rendered !== 'ok') errors.push(rendered);
    if (!nojs) for (const j of fx.js) await page.addScriptTag({ content: fs.readFileSync(file(j), 'utf8') });
    await page.waitForTimeout(250);
    for (const c of clicks) { try { await page.click(c, { timeout: 2000 }); await page.waitForTimeout(200); } catch (e) { errors.push('click ' + c + ': ' + e.message.split('\n')[0]); } }
    for (const e of evals) { try { await page.evaluate(e); await page.waitForTimeout(250); } catch (er) { errors.push('eval: ' + er.message.split('\n')[0]); } }
    const m = await page.evaluate(() => {
      const r = document.querySelector('.vw-app, .vp-app');
      const de = document.documentElement;
      const wide = [];
      if (r) r.querySelectorAll('*').forEach((el) => {
        const b = el.getBoundingClientRect();
        if (b.right > de.clientWidth + 1 && b.width > 0 && !el.closest('.vw-tablewrap, .vp-tablewrap') && getComputedStyle(el).position !== 'absolute') wide.push((el.className || el.tagName).toString().slice(0, 60) + ' right=' + Math.round(b.right));
      });
      return { app: !!r, h: r ? Math.round(r.getBoundingClientRect().height) : 0, overflow: de.scrollWidth > de.clientWidth + 1, sw: de.scrollWidth, cw: de.clientWidth, wide: wide.slice(0, 5),
               submitted: window.__submitted || null };
    });
    if (!m.app) errors.push('no .vw-app or .vp-app rendered');
    if (m.overflow) errors.push(`horizontal overflow ${m.sw}px > ${m.cw}px: ${m.wide.join('; ')}`);
    const shot = path.join(outDir, `${tag}-${theme}${nojs ? '-nojs' : ''}-${w}.png`);
    await page.screenshot({ path: shot, fullPage: true });
    report.push({ width: w, height: m.h, shot, errors, submitted: m.submitted });
    if (errors.length) failures++;
    await page.close();
  }
  await browser.close();
  for (const r of report) console.log(`${r.errors.length ? 'FAIL' : 'ok  '} ${String(r.width).padStart(4)}px h=${r.height} ${r.shot}${r.submitted ? ' submitted=' + JSON.stringify(r.submitted) : ''}${r.errors.length ? '\n      ' + r.errors.join('\n      ') : ''}`);
  process.exit(failures ? 1 : 0);
})();
