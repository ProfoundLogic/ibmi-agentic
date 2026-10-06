// Report the computed colour of representative widgets, so a dark-mode fix can
// reproduce light-mode appearance rather than invent one.
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
    const want = ['label','outputField','hybrid-constant','hybrid-link','input','search'];
    const res = {};
    want.forEach(c => {
      const el = document.querySelector('.' + CSS.escape(c));
      if (el) res[c] = { color: getComputedStyle(el).color, tag: el.tagName.toLowerCase() };
    });
    res['--color--text--primary'] = getComputedStyle(document.documentElement)
      .getPropertyValue('--color--text--primary').trim();
    return res;
  });
  console.log(JSON.stringify(out, null, 1));
  await browser.close();
})();
