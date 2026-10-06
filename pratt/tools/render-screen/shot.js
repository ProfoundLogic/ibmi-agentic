const path = process.env.PLAYWRIGHT_CORE || '/home/coder/.claude/skills/dspf-to-ejs/node_modules/playwright-core';
const { chromium } = require(path);
(async () => {
  const out = process.argv[2] || '/tmp/pratt/harness/shot.png';
  const url = process.argv[3] || 'http://localhost:8777/render.html';
  const browser = await chromium.launch({
    executablePath: '/home/coder/.cache/ms-playwright/chromium-1228/chrome-linux64/chrome',
    args: ['--no-sandbox', '--disable-dev-shm-usage']
  });
  const page = await browser.newPage({ viewport: { width: 1440, height: 760 },
    colorScheme: process.env.PUI_COLOR_SCHEME || 'light' });
  const logs = [];
  page.on('console', m => logs.push(m.type() + ': ' + m.text()));
  page.on('pageerror', e => logs.push('pageerror: ' + e.message));
  page.on('requestfailed', r => logs.push('404/fail: ' + r.url()));
  await page.goto(url, { waitUntil: 'networkidle' });
  await page.waitForTimeout(2500);
  const info = await page.evaluate(() => ({
    errors: window.__errors || [],
    rendered: !!window.__rendered,
    bodyChildren: document.getElementById('pui') ? document.getElementById('pui').children.length : -1,
    text: (document.body.innerText || '').slice(0, 400)
  }));
  await page.screenshot({ path: out, fullPage: false });
  console.log(JSON.stringify({ info, logs: logs.slice(0, 25) }, null, 1));
  await browser.close();
})();
