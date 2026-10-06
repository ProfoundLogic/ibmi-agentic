const path = process.env.PLAYWRIGHT_CORE || '/home/coder/.claude/skills/dspf-to-ejs/node_modules/playwright-core';
const { chromium } = require(path);
(async () => {
  const url = process.argv[3] || process.argv[2];
  const browser = await chromium.launch({ executablePath: '/home/coder/.cache/ms-playwright/chromium-1228/chrome-linux64/chrome',
    args: ['--no-sandbox','--disable-dev-shm-usage'] });
  const page = await browser.newPage({ viewport:{width:1440,height:760},
    colorScheme: process.env.PUI_COLOR_SCHEME || 'light' });
  await page.goto(url,{waitUntil:'networkidle'}); await page.waitForTimeout(2200);
  console.log(JSON.stringify(await page.evaluate(() => {
    const seen = {};
    document.querySelectorAll('input, textarea, select').forEach(el => {
      const cs = getComputedStyle(el);
      const k = el.tagName.toLowerCase() + '.' + (el.className || '(none)');
      if (!seen[k]) seen[k] = { color: cs.color, background: cs.backgroundColor, n: 0 };
      seen[k].n++;
    });
    return seen;
  }), null, 1));
  await browser.close();
})();
