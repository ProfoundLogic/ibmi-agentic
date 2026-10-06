const path = process.env.PLAYWRIGHT_CORE || '/home/coder/.claude/skills/dspf-to-ejs/node_modules/playwright-core';
const { chromium } = require(path);
(async () => {
  const url = process.argv[3] || process.argv[2];
  const b = await chromium.launch({ executablePath:'/home/coder/.cache/ms-playwright/chromium-1228/chrome-linux64/chrome',
    args:['--no-sandbox','--disable-dev-shm-usage'] });
  const p = await b.newPage({ viewport:{width:1440,height:760} });
  await p.goto(url,{waitUntil:'networkidle'}); await p.waitForTimeout(2500);
  console.log(JSON.stringify(await p.evaluate(() => {
    const out = {};
    document.querySelectorAll('select').forEach(s => {
      const id = s.id || '(no id)';
      out[id] = { options: s.options.length,
                  sample: Array.from(s.options).slice(0,3).map(o => o.text.trim()) };
    });
    return out;
  }), null, 1));
  await b.close();
})();
