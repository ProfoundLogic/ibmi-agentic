const http = require('http');
const fs = require('fs');
const path = require('path');
const { chromium } = require('playwright-core');

/* Offline preview harness for Genie skins.
 *
 * Serves a skin folder over HTTP with a stand-in genie.js (mock/genie.js) that
 * renders synthetic 5250 screens using real Genie field ids, then screenshots
 * the result and probes the DOM. Lets a skin be checked without an IBM i, a
 * Profound UI instance, or a browser session.
 *
 *   npm i playwright-core && npx playwright install chromium --only-shell
 *   node tools/genie-skin-preview/preview.js [skinName] [outDir]
 *
 * Screens are defined in mock/genie.js; add one there for any screen shape you
 * want to check.
 */

const SKIN_NAME = process.argv[2] || 'bdmedical';
const REPO = path.resolve(__dirname, '..', '..');
const SKIN = path.join(REPO, 'htdocs', 'profoundui', 'userdata', 'genie skins', SKIN_NAME);
const MOCK = path.join(__dirname, 'mock');
const OUT = process.argv[3] || path.join(__dirname, 'out');
const URL_BASE = '/profoundui/userdata/genie skins/' + SKIN_NAME + '/';
fs.mkdirSync(OUT, { recursive: true });
if (!fs.existsSync(SKIN)) { console.error('no such skin: ' + SKIN); process.exit(1); }

let VARIANT = 'standard';
const TYPES = { '.html': 'text/html', '.css': 'text/css', '.js': 'application/javascript',
                '.svg': 'image/svg+xml', '.woff2': 'font/woff2', '.woff': 'font/woff', '.ttf': 'font/ttf' };

function resolve(url) {
  const p = decodeURIComponent(url.split('?')[0]);
  // must come before the skin-folder catch-all below
  if (p.endsWith('/custom.js') && VARIANT === 'adjusted') return path.join(SKIN, 'adjusted columns custom.js');
  if (p.startsWith(URL_BASE)) return path.join(SKIN, p.replace(URL_BASE, ''));
  if (p === '/profoundui/proddata/js/genie.js') return path.join(MOCK, 'genie.js');
  if (p === '/profoundui/proddata/css/profoundui.css') return path.join(MOCK, 'profoundui.css');
  if (p === '/' || p === '/index.html') return path.join(SKIN, 'start.html');
  return null;
}

const server = http.createServer((req, res) => {
  const file = resolve(req.url);
  if (file == null || !fs.existsSync(file)) { res.writeHead(404); res.end('not found ' + req.url); return; }
  res.writeHead(200, { 'Content-Type': TYPES[path.extname(file)] || 'text/plain', 'Cache-Control': 'no-store' });
  res.end(fs.readFileSync(file));
});

(async () => {
  await new Promise(r => server.listen(8799, r));
  const browser = await chromium.launch();
  const page = await browser.newPage({ viewport: { width: 1280, height: 900 }, deviceScaleFactor: 2 });

  const errors = [];
  page.on('pageerror', e => errors.push('PAGEERROR: ' + e.message));
  page.on('console', m => { if (m.type() === 'error') errors.push('CONSOLE: ' + m.text()); });
  page.on('requestfailed', r => errors.push('REQFAILED: ' + r.url() + ' ' + (r.failure() || {}).errorText));
  page.on('response', r => { if (r.status() >= 400) errors.push('HTTP ' + r.status() + ' ' + r.url()); });

  const cases = [
    { screen: 'signon',    variant: 'standard', vp: { width: 1280, height: 900 }, name: 'signon' },
    { screen: 'app',       variant: 'standard', vp: { width: 1280, height: 900 }, name: 'app' },
    { screen: 'signon128', variant: 'standard', vp: { width: 1280, height: 900 }, name: 'signon128' },
    { screen: 'signonadj', variant: 'adjusted', vp: { width: 1280, height: 900 }, name: 'signon-adjusted' },
    { screen: 'mainmenu',  variant: 'standard', vp: { width: 1280, height: 900 }, name: 'mainmenu' },
    { screen: 'app',       variant: 'standard', vp: { width: 760,  height: 900 }, name: 'app-narrow' }
  ];
  for (const tc of cases) {
    const screen = tc.screen;
    VARIANT = tc.variant;
    await page.setViewportSize(tc.vp);
    await page.goto(`http://127.0.0.1:8799/?screen=${screen}`, { waitUntil: 'networkidle' });
    await page.waitForTimeout(350);
    await page.screenshot({ path: path.join(OUT, `${tc.name}.png`), fullPage: true });

    const probe = await page.evaluate(() => {
      const q = id => document.getElementById(id);
      const card = q('bd_signon_card');
      const bar = q('bdFkeyBar');
      const cs = el => el ? getComputedStyle(el) : null;
      return {
        cardPresent: !!card,
        cardRect: card ? card.getBoundingClientRect() : null,
        cardZ: card ? card.style.zIndex : null,
        loginRect: q('login_button') ? q('login_button').getBoundingClientRect() : null,
        quitRect: q('quit_button') ? q('quit_button').getBoundingClientRect() : null,
        barPresent: !!bar,
        barButtons: bar ? Array.from(bar.querySelectorAll('input')).map(b => b.value) : [],
        barRect: bar ? bar.getBoundingClientRect() : null,
        hiddenOriginals: Array.from(document.querySelectorAll('[id="5250"] > input[type=button]'))
          .filter(b => b.style.visibility === 'hidden').length,
        headerMeta: q('bdSessionInfo') ? q('bdSessionInfo').textContent : null,
        menuToggleVisible: q('bdMenuToggle') ? !q('bdMenuToggle').hidden : null,
        // The Actions button should sit just above the terminal and share its
        // right edge, not float off at the edge of a full-width header.
        menuVsTerminal: (() => {
          const t = q('bdMenuToggle'), five = q('5250');
          if (!t || t.hidden || !five) return null;
          const tr = t.getBoundingClientRect(), fr = five.getBoundingClientRect();
          return { rightOffset: Math.round(fr.right - tr.right), gapAbove: Math.round(fr.top - tr.bottom) };
        })(),
        menuCount: q('bdMenuCount') ? q('bdMenuCount').textContent : null,
        menuItems: q('bdMenuPanel')
          ? Array.from(q('bdMenuPanel').querySelectorAll('.bd-menu-item'))
              .map(i => i.querySelector('.bd-menu-item__key').textContent + '|' +
                        i.querySelector('.bd-menu-item__text').textContent)
          : null,
        spinnerAnim: (() => { const d = document.createElement('div'); d.className = 'pui-bd-animation';
          document.body.appendChild(d); const s = getComputedStyle(d, ':before').animationName; d.remove(); return s; })(),
        sampleColors: ['A20', 'A22', 'A28', 'A3A', 'A30', 'A32', 'A38'].map(c => {
          const el = document.querySelector('[id="5250"] div.' + c);
          return c + '=' + (el ? cs(el).color : 'n/a');
        }),
        // The rebuilt function-key bar must not sit on top of any visible
        // 5250 field - on screens like the IBM i Main Menu, real text shares
        // the rows the original fkey prompts occupied.
        barOverlaps: (() => {
          const b = q('bdFkeyBar'); if (!b) return [];
          const br = b.getBoundingClientRect();
          const hits = [];
          Array.from(q('5250').children).forEach(el => {
            if (el === b) return;
            if (el.style.visibility === 'hidden' || el.style.display === 'none') return;
            const r = el.getBoundingClientRect();
            if (r.width === 0 && r.height === 0) return;
            if (r.left < br.right && r.right > br.left && r.top < br.bottom && r.bottom > br.top) {
              hits.push((el.id || el.tagName) + ':' + JSON.stringify(el.textContent || el.value).slice(0, 40));
            }
          });
          return hits;
        })(),
        barBottomVsMiddle: (() => {
          const b = q('bdFkeyBar'); const m = document.getElementById('middle');
          if (!b || !m) return null;
          return Math.round(b.getBoundingClientRect().bottom - m.getBoundingClientRect().bottom);
        })(),
        // Terminal typeface: it must actually load, must fit inside Genie's
        // 8px character cell, and must not ligate - a coding font that
        // ligates "===>" or "!=" would collapse several cells into one glyph
        // and break the fixed grid.
        terminalFont: (() => {
          const five = q('5250');
          if (!five) return null;
          const cs = getComputedStyle(five);
          const probe = document.createElement('span');
          probe.style.cssText = 'position:absolute;visibility:hidden;white-space:pre;';
          probe.style.font = cs.fontSize + '/' + cs.lineHeight + ' ' + cs.fontFamily;
          probe.style.fontVariantLigatures = cs.fontVariantLigatures;
          probe.style.fontFeatureSettings = cs.fontFeatureSettings;
          five.appendChild(probe);
          const measure = t => { probe.textContent = t; return probe.getBoundingClientRect().width; };
          const one = measure('M');
          const eighty = measure('M'.repeat(80));
          const arrow = measure('===>');
          const dots = measure('. . . .');
          probe.remove();
          return {
            family: cs.fontFamily.split(',')[0].replace(/"/g, ''),
            size: cs.fontSize,
            loaded: document.fonts ? document.fonts.check('13px "JetBrains Mono"') : null,
            advance: Math.round(one * 100) / 100,
            eightyCols: Math.round(eighty * 10) / 10,
            fitsGrid: eighty <= 640,
            ligatureFree: Math.abs(arrow - one * 4) < 0.5 && Math.abs(dots - one * 7) < 0.5
          };
        })(),
        docScrollWidth: document.documentElement.scrollWidth,
        pageCenter: document.documentElement.clientWidth / 2,
        cardCenter: card ? (card.getBoundingClientRect().left + card.getBoundingClientRect().width / 2) : null,
        buttonLabels: ['login_button', 'quit_button'].map(id => {
          const b = q(id); if (!b) return id + '=absent';
          // scrollWidth > clientWidth means the label is being clipped
          return id + ' w=' + Math.round(b.getBoundingClientRect().width) +
                 ' inlineW=' + (b.style.width || 'unset') +
                 ' clipped=' + (b.scrollWidth > b.clientWidth + 1);
        }),
        fiveRect: q('5250').getBoundingClientRect()
      };
    });
    console.log('=== ' + tc.name + ' ===');
    console.log(JSON.stringify(probe, null, 1));

    if (probe.menuToggleVisible) {
      await page.click('#bdMenuToggle');
      await page.waitForTimeout(200);
      await page.screenshot({ path: path.join(OUT, `${tc.name}-menu-open.png`), fullPage: true });

      const open = await page.evaluate(() => {
        const panel = document.getElementById('bdMenuPanel');
        const toggle = document.getElementById('bdMenuToggle');
        const r = panel.getBoundingClientRect();
        // Is the panel actually the topmost thing at its own centre? If a 5250
        // field paints over it, elementFromPoint returns that field instead.
        const hit = document.elementFromPoint(r.left + r.width / 2, r.top + 24);
        return {
          panelVisible: !panel.hidden && r.height > 0,
          ariaExpanded: toggle.getAttribute('aria-expanded'),
          focusedItem: document.activeElement && document.activeElement.className.indexOf('bd-menu-item') >= 0,
          topmostAtPanel: hit ? (hit.closest('#bdMenuPanel') ? 'panel' : (hit.id || hit.className || hit.tagName)) : 'none',
          withinViewport: r.right <= document.documentElement.clientWidth + 1 && r.left >= -1
        };
      });
      console.log('  menu-open: ' + JSON.stringify(open));

      // Escape must close it and hand focus back to the toggle.
      await page.keyboard.press('Escape');
      await page.waitForTimeout(120);
      console.log('  after-escape: ' + JSON.stringify(await page.evaluate(() => ({
        closed: document.getElementById('bdMenuPanel').hidden,
        focusBackOnToggle: document.activeElement === document.getElementById('bdMenuToggle')
      }))));

      // Clicking an item must fire pressKey with the right key.
      await page.evaluate(() => { window.__pressed = []; const o = window.pressKey;
        window.pressKey = function(k) { window.__pressed.push(k); return o.apply(this, arguments); }; });
      await page.click('#bdMenuToggle');
      await page.waitForTimeout(120);
      await page.click('#bdMenuPanel .bd-menu-item:nth-child(2)');
      await page.waitForTimeout(120);
      console.log('  item-click: ' + JSON.stringify(await page.evaluate(() => ({
        pressed: window.__pressed,
        closedAfterClick: document.getElementById('bdMenuPanel').hidden
      }))));
    }
  }

  console.log('=== errors ===');
  console.log(errors.length ? errors.join('\n') : '(none)');
  await browser.close();
  server.close();
})();
