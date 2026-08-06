/*
 * Offline pre-flight for a Profound UI EJS screen.
 *
 * EJS templates render client-side, so a syntax error or an undefined field
 * reference is invisible to the build and shows up as a blank white screen in
 * front of the customer. This renders the template with representative field
 * data exactly as the runtime would, then loads the result in headless
 * Chromium to catch layout overflow at the real device width.
 */
const ejs = require('ejs');
const fs = require('fs');
const path = require('path');
const { chromium } = require('playwright-core');

const UI = '/workspace/workspace/ibmi-agentic/docs/htdocs/profoundui/userdata/ui';
const TEMPLATE = path.join(UI, 'gtmnud', 'menu.ejs');   // TODO: parameterise per screen
const CSS = [
  path.join(UI, 'gtcommon', 'gt-theme.css'),
  path.join(UI, 'gtmnud', 'menu.css'),
];

/* Exactly the fields declared in gtmnud.json, lowercased the way the
 * Profound UI runtime hands them to the template. Values match what GTMNUR
 * actually selects from DB2. */
function data(lang) {
  return {
    action: '', operid: 'GT001',
    opername: lang === 'FR' ? 'Marie Tremblay' : 'James Okonkwo',
    operrole: 'OPER', langpref: lang,
    msg: lang === 'FR'
      ? "Cette application n'est pas encore construite : RECV"
      : 'That application is not built yet: RECV',
    nrcpts: 8, ncounts: 4, nreplen: 51, nitems: 408,
  };
}

/* Guards against the failure mode where a template references a field that is
 * not declared in that format's JSON -- the whole render aborts. */
const DECLARED = ['action', 'operid', 'opername', 'operrole', 'langpref',
                  'msg', 'nrcpts', 'ncounts', 'nreplen', 'nitems'];

(async () => {
  const src = fs.readFileSync(TEMPLATE, 'utf8');
  const css = CSS.map((f) => fs.readFileSync(f, 'utf8')).join('\n');

  /* Identifier audit: every bare <%= x %> must be a declared field or a
   * local declared inside the template itself. */
  const locals = new Set([...src.matchAll(/\b(?:var|function)\s+([A-Za-z_$][\w$]*)|function\s*\(\s*([A-Za-z_$][\w$]*)/g)].flatMap((m) => [m[1], m[2]]).filter(Boolean));
  const refs = new Set([...src.matchAll(/<%[=-]\s*([A-Za-z_$][\w$]*)/g)].map((m) => m[1]));
  const unknown = [...refs].filter(
    (r) => !DECLARED.includes(r) && !locals.has(r) &&
           !['typeof', 'Number', 'String', 'Math', 'JSON'].includes(r));
  console.log('identifier audit :', unknown.length ? 'UNKNOWN -> ' + unknown.join(', ') : 'clean');

  const browser = await chromium.launch({
    executablePath: '/home/coder/.cache/ms-playwright/chromium-1228/chrome-linux64/chrome',
    args: ['--no-sandbox'],
  });

  const CASES = [
    { lang: 'EN', w: 412,  h: 915,  name: 'en-phone-s24ultra' },
    { lang: 'FR', w: 412,  h: 915,  name: 'fr-phone-s24ultra' },
    { lang: 'FR', w: 360,  h: 800,  name: 'fr-phone-360-floor' },
    { lang: 'EN', w: 1280, h: 900,  name: 'en-desktop-1280' },
  ];

  let failures = 0;

  for (const c of CASES) {
    let html;
    try {
      html = ejs.render(src, data(c.lang));
    } catch (e) {
      console.log(`${c.name.padEnd(22)} RENDER FAILED: ${e.message}`);
      failures++;
      continue;
    }

    const page = await browser.newPage({ viewport: { width: c.w, height: c.h } });
    await page.setContent(
      `<!doctype html><meta charset="utf-8"><style>
         html,body{margin:0;padding:0}
         /* Reproduce the hostile Genie globals this theme has to survive. */
         div{white-space:nowrap;z-index:10;padding:1px}
       </style><style>${css}</style>${html}`,
      { waitUntil: 'load' });

    const overflow = await page.evaluate(() => {
      const de = document.documentElement;
      const wide = [];
      document.querySelectorAll('.gt-app *').forEach((el) => {
        const r = el.getBoundingClientRect();
        if (r.right > de.clientWidth + 1) {
          wide.push((el.className || el.tagName) + ' right=' + Math.round(r.right));
        }
      });
      return { scrollW: de.scrollWidth, clientW: de.clientWidth, wide: wide.slice(0, 4) };
    });

    const bad = overflow.scrollW > overflow.clientW + 1;
    if (bad) failures++;
    console.log(
      `${c.name.padEnd(22)} ${String(c.w).padStart(4)}px  ` +
      `scrollW=${overflow.scrollW} clientW=${overflow.clientW}  ` +
      (bad ? 'OVERFLOW -> ' + overflow.wide.join(' | ') : 'no overflow'));

    await page.screenshot({ path: `/tmp/ejspre/${c.name}.png`, fullPage: true });
    await page.close();
  }

  await browser.close();
  console.log(failures ? `\n${failures} case(s) failed` : '\nAll cases clean');
})();
