// Dump every text element's class + computed colour, keyed by a stable path, so
// light and dark renders can be diffed.  Anything whose colour differs between
// the two is being driven by the skin's dark palette rather than by the screen.
const path = process.env.PLAYWRIGHT_CORE || '/home/coder/.claude/skills/dspf-to-ejs/node_modules/playwright-core';
const { chromium } = require(path);
(async () => {
  const url = process.argv[3] || process.argv[2];
  const browser = await chromium.launch({
    executablePath: '/home/coder/.cache/ms-playwright/chromium-1228/chrome-linux64/chrome',
    args: ['--no-sandbox', '--disable-dev-shm-usage'] });
  const page = await browser.newPage({ viewport: { width: 1440, height: 760 },
    colorScheme: process.env.PUI_COLOR_SCHEME || 'light' });
  await page.goto(url, { waitUntil: 'networkidle' });
  await page.waitForTimeout(2500);
  const out = await page.evaluate(() => {
    const res = {};
    document.querySelectorAll('*').forEach(el => {
      const txt = Array.from(el.childNodes).filter(n => n.nodeType === 3)
        .map(n => n.textContent.trim()).join(' ').trim();
      if (!txt) return;
      const r = el.getBoundingClientRect(); if (r.width < 2 || r.height < 2) return;
      const key = (el.id || '') + '#' + (el.className || '(none)') + '#' + txt.slice(0, 20);
      res[key] = getComputedStyle(el).color;
    });
    return res;
  });
  console.log(JSON.stringify(out));
  await browser.close();
})();
