const path = process.env.PLAYWRIGHT_CORE || '/home/coder/.claude/skills/dspf-to-ejs/node_modules/playwright-core';
const { chromium } = require(path);
(async () => {
  const url = process.argv[3] || process.argv[2];
  const b = await chromium.launch({ executablePath:'/home/coder/.cache/ms-playwright/chromium-1228/chrome-linux64/chrome',
    args:['--no-sandbox','--disable-dev-shm-usage'] });
  const p = await b.newPage({ viewport:{width:1440,height:760}, colorScheme: process.env.PUI_COLOR_SCHEME || 'light' });
  const missing = [];
  p.on('requestfailed', r => missing.push(r.url()));
  p.on('response', r => { if (r.status() >= 400) missing.push(r.status()+' '+r.url()); });
  await p.goto(url,{waitUntil:'networkidle'}); await p.waitForTimeout(2200);
  console.log(JSON.stringify(await p.evaluate(() => {
    const els = Array.from(document.querySelectorAll('.label')).slice(0,4);
    return { labels: els.map(e => ({ text:(e.textContent||'').trim().slice(0,16),
      inlineStyle: e.getAttribute('style') || '', computed: getComputedStyle(e).color })) };
  }), null, 1));
  console.log('failed/4xx requests:', JSON.stringify(missing.filter(m => /pratt_theme_fix/.test(m))));
  await b.close();
})();
