// Confirm the settings.js helpers exist after the screen loads, and then actually
// fire the grid's onrowclick - the handler that was failing in the browser.
const path = process.env.PLAYWRIGHT_CORE || '/home/coder/.claude/skills/dspf-to-ejs/node_modules/playwright-core';
const { chromium } = require(path);
(async () => {
  const url = process.argv[3] || process.argv[2];
  const b = await chromium.launch({ executablePath:'/home/coder/.cache/ms-playwright/chromium-1228/chrome-linux64/chrome',
    args:['--no-sandbox','--disable-dev-shm-usage'] });
  const p = await b.newPage({ viewport:{width:1440,height:760}, colorScheme: process.env.PUI_COLOR_SCHEME || 'light' });
  const errs = [], missing = [];
  p.on('pageerror', e => errs.push(e.message));
  p.on('dialog', async d => { errs.push('DIALOG: ' + d.message()); await d.dismiss(); });
  p.on('response', r => { if (r.status() >= 400) missing.push(r.status() + ' ' + r.url().split('/').pop()); });
  await p.goto(url,{waitUntil:'networkidle'}); await p.waitForTimeout(2500);
  const out = await p.evaluate(() => {
    const names = ['applyPropertyCSS','restoreSearch','sanitizeFilename','pressWait',
                   'serverReady','sleep','applyPropertyCSSfromPUI'];
    const defined = {}; names.forEach(n => defined[n] = typeof window[n]);
    let rowclick = 'not attempted';
    try {
      const g = getObj('SFLFMT');
      if (g && g.grid) { g.grid.selectRow ? g.grid.selectRow(2) : null;
        const h = g.pui && g.pui.properties && g.pui.properties.onrowclick;
        if (h) { const row = 2; eval(h); rowclick = 'ran clean'; } else rowclick = 'no handler';
      } else rowclick = 'no grid';
    } catch (e) { rowclick = 'THREW: ' + e.message; }
    return { defined, rowclick };
  });
  console.log(JSON.stringify({ ...out, pageErrors: errs, failedAssets: missing }, null, 1));
  await b.close();
})();
