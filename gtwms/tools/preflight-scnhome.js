const ejs = require('ejs'), fs = require('fs'), path = require('path');
const { chromium } = require('playwright-core');

const UI = '/workspace/workspace/ibmi-agentic/docs/htdocs/profoundui/userdata/ui';
const TPL = path.join(UI, 'gtscnd', 'scnhome.ejs');
const CSS = [path.join(UI, 'gtcommon', 'gt-theme.css'), path.join(UI, 'gtscnd', 'scnhome.css')];

/* Exactly the fields declared in gtscnd.json. Note the subfile arrives as a
 * TOP-LEVEL array named scnsfl -- it is not nested under a `subfiles` object,
 * which is the single most common way an EJS subfile screen renders empty. */
function data(lang, withResult) {
  const rows = [
    { _rrn: 1, sseq: 1, skind: 'RCPT',   ssymb: 'GS1-128', sraw: '00306141410000000013106141', sdesc: '' },
    { _rrn: 2, sseq: 2, skind: 'ITEM',   ssymb: 'UPC-A',   sraw: '061414100015', sdesc: 'Long Grain White Rice - 250 g' },
    { _rrn: 3, sseq: 3, skind: 'UNKNWN', ssymb: 'EAN-13',  sraw: '5901234123457', sdesc: '' },
  ];
  return {
    action: '', langpref: lang, scanval: '', msg: '', nscans: 3,
    opername: lang === 'FR' ? 'Marie Tremblay' : 'James Okonkwo',
    hasres: withResult ? 'Y' : 'N',
    rkind: withResult ? 'RCPT' : '',
    rsymb: withResult ? 'GS1-128' : '',
    risgs1: withResult ? 'Y' : 'N',
    rcheck: withResult ? 'Y' : 'N',
    rraw: withResult ? '00306141410000000013011061414100001910L2026A1717270131' : '',
    rrawlen: withResult ? 54 : 0,
    rsku: '', rdesc: '', rloc: '',
    rsscc: withResult ? '306141410000000013' : '',
    rrcpt: withResult ? 'RC00000001' : '',
    rgtin: withResult ? '10614141000019' : '',
    rlot: withResult ? 'L2026A17' : '',
    rexpiry: withResult ? '270131' : '',
    rqty: 0, rlevel: '', rpack: 0,
    scnsfl: rows,
  };
}

const DECLARED = ['action', 'scanval', 'langpref', 'opername', 'msg', 'hasres', 'rkind',
  'rsymb', 'risgs1', 'rcheck', 'rraw', 'rrawlen', 'rsku', 'rdesc', 'rloc', 'rsscc',
  'rrcpt', 'rgtin', 'rlot', 'rexpiry', 'rqty', 'rlevel', 'rpack', 'nscans', 'scnsfl'];

(async () => {
  const src = fs.readFileSync(TPL, 'utf8');
  const css = CSS.map((f) => fs.readFileSync(f, 'utf8')).join('\n');

  const locals = new Set([...src.matchAll(/\b(?:var|function)\s+([A-Za-z_$][\w$]*)|function\s*\(\s*([A-Za-z_$][\w$]*)/g)]
    .flatMap((m) => [m[1], m[2]]).filter(Boolean));
  const refs = new Set([...src.matchAll(/<%[=-]\s*([A-Za-z_$][\w$]*)/g)].map((m) => m[1]));
  const unknown = [...refs].filter((r) => !DECLARED.includes(r) && !locals.has(r) &&
    !['typeof', 'Number', 'String', 'Math', 'JSON'].includes(r));
  console.log('identifier audit :', unknown.length ? 'UNKNOWN -> ' + unknown.join(', ') : 'clean');

  const browser = await chromium.launch({
    executablePath: '/home/coder/.cache/ms-playwright/chromium-1228/chrome-linux64/chrome',
    args: ['--no-sandbox'],
  });

  const CASES = [
    { lang: 'EN', res: true,  w: 412,  h: 915, name: 'scan-en-412-result' },
    { lang: 'FR', res: true,  w: 412,  h: 915, name: 'scan-fr-412-result' },
    { lang: 'FR', res: false, w: 360,  h: 800, name: 'scan-fr-360-empty' },
    { lang: 'EN', res: true,  w: 1280, h: 900, name: 'scan-en-1280' },
  ];

  let fail = 0;
  for (const c of CASES) {
    let html;
    try {
      html = ejs.render(src, data(c.lang, c.res));
    } catch (e) {
      console.log(c.name.padEnd(22) + ' RENDER FAILED: ' + e.message);
      fail++;
      continue;
    }
    const page = await browser.newPage({ viewport: { width: c.w, height: c.h } });
    await page.setContent(
      '<!doctype html><meta charset="utf-8"><style>html,body{margin:0;padding:0}' +
      'div{white-space:nowrap;z-index:10;padding:1px}</style>' +
      '<style>' + css + '</style>' + html, { waitUntil: 'load' });

    const o = await page.evaluate(() => {
      const de = document.documentElement;
      const wide = [];
      document.querySelectorAll('.gt-app *').forEach((el) => {
        const r = el.getBoundingClientRect();
        if (r.right > de.clientWidth + 1) {
          wide.push((typeof el.className === 'string' ? el.className : el.tagName) + ' r=' + Math.round(r.right));
        }
      });
      return { sw: de.scrollWidth, cw: de.clientWidth, wide: wide.slice(0, 3) };
    });

    const bad = o.sw > o.cw + 1;
    if (bad) fail++;
    console.log(c.name.padEnd(22) + ' ' + String(c.w).padStart(4) + 'px sw=' + o.sw + ' cw=' + o.cw +
      ' ' + (bad ? 'OVERFLOW -> ' + o.wide.join(' | ') : 'no overflow'));

    await page.screenshot({ path: '/tmp/ejspre/' + c.name + '.png', fullPage: true });
    await page.close();
  }

  await browser.close();
  console.log(fail ? '\n' + fail + ' case(s) failed' : '\nAll cases clean');
})();
