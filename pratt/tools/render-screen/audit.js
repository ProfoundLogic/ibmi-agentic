// Report every text element whose contrast against its effective background
// falls below the WCAG AA threshold for normal text.  Used to find washed out
// widgets instead of eyeballing a screenshot.
const path = process.env.PLAYWRIGHT_CORE || '/home/coder/.claude/skills/dspf-to-ejs/node_modules/playwright-core';
const { chromium } = require(path);
(async () => {
  const url = process.argv[3] || process.argv[2];  // render.sh passes <out> <url>
  const browser = await chromium.launch({
    executablePath: '/home/coder/.cache/ms-playwright/chromium-1228/chrome-linux64/chrome',
    args: ['--no-sandbox', '--disable-dev-shm-usage']
  });
  const page = await browser.newPage({ viewport: { width: 1440, height: 760 },
    colorScheme: process.env.PUI_COLOR_SCHEME || 'light' });
  await page.goto(url, { waitUntil: 'networkidle' });
  await page.waitForTimeout(2500);
  const out = await page.evaluate(() => {
    const lum = c => { const s = c.map(v => { v /= 255; return v <= 0.03928 ? v/12.92 : Math.pow((v+0.055)/1.055, 2.4); });
      return 0.2126*s[0] + 0.7152*s[1] + 0.0722*s[2]; };
    const parse = s => { const m = (s||'').match(/rgba?\(([^)]+)\)/); if (!m) return null;
      const p = m[1].split(',').map(Number); return { rgb: p.slice(0,3), a: p.length > 3 ? p[3] : 1 }; };
    const bgOf = el => { let n = el;
      while (n && n !== document.documentElement) { const b = parse(getComputedStyle(n).backgroundColor);
        if (b && b.a > 0.1) return b.rgb; n = n.parentElement; } return [255,255,255]; };
    const ratio = (a,b) => { const l1 = lum(a), l2 = lum(b);
      return (Math.max(l1,l2)+0.05)/(Math.min(l1,l2)+0.05); };
    const bad = [], seen = new Set();
    document.querySelectorAll('*').forEach(el => {
      const txt = Array.from(el.childNodes).filter(n => n.nodeType === 3)
        .map(n => n.textContent.trim()).join(' ').trim();
      if (!txt) return;
      const cs = getComputedStyle(el);
      if (cs.visibility === 'hidden' || cs.display === 'none') return;
      const r = el.getBoundingClientRect(); if (r.width < 2 || r.height < 2) return;
      const fg = parse(cs.color); if (!fg || fg.a < 0.1) return;
      const cr = ratio(fg.rgb, bgOf(el));
      if (cr < 4.5) {
        const key = (el.className||'') + '|' + Math.round(cr*10);
        if (seen.has(key)) return; seen.add(key);
        bad.push({ cls: el.className || '(none)', tag: el.tagName.toLowerCase(),
                   contrast: Math.round(cr*100)/100, color: cs.color, bg: 'rgb('+bgOf(el).join(',')+')',
                   text: txt.slice(0,28) });
      }
    });
    return bad.sort((a,b) => a.contrast - b.contrast);
  });
  console.log(JSON.stringify(out, null, 1));
  await browser.close();
})();
