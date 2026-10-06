const path = process.env.PLAYWRIGHT_CORE || '/home/coder/.claude/skills/dspf-to-ejs/node_modules/playwright-core';
const { chromium } = require(path);
(async () => {
  const url = process.argv[3] || process.argv[2];
  const browser = await chromium.launch({ executablePath: '/home/coder/.cache/ms-playwright/chromium-1228/chrome-linux64/chrome',
    args: ['--no-sandbox','--disable-dev-shm-usage'] });
  const page = await browser.newPage({ viewport:{width:1440,height:760}, colorScheme: process.env.PUI_COLOR_SCHEME || 'light' });
  await page.goto(url,{waitUntil:'networkidle'}); await page.waitForTimeout(2200);
  console.log(JSON.stringify(await page.evaluate(() => {
    const parse = s => { const m=(s||'').match(/rgba?\(([^)]+)\)/); if(!m) return null;
      const p=m[1].split(',').map(Number); return {rgb:p.slice(0,3), a:p.length>3?p[3]:1}; };
    const bgOf = el => { let n=el; while(n && n!==document.documentElement){ const b=parse(getComputedStyle(n).backgroundColor);
      if(b && b.a>0.1) return b.rgb; n=n.parentElement; } return [255,255,255]; };
    const out={};
    document.querySelectorAll('.label').forEach(el => {
      const bg = bgOf(el).join(',');
      const lum = bgOf(el).reduce((a,v)=>a+v,0)/3;
      out[bg] = out[bg] || { n:0, light: lum>140, sample:[] };
      out[bg].n++;
      const t=(el.textContent||'').trim(); if(t && out[bg].sample.length<4) out[bg].sample.push(t.slice(0,18));
    });
    return out;
  }), null, 1));
  await browser.close();
})();
