/* ==================================================================
 * The offline pre-flight engine, extracted.
 *
 * preflight-item / -scnhome / -rcv / -inv / -cnt each carry their own copy of
 * this code, which is how one of them ended up with a check the others lacked.
 * The three screens added after it use this instead; the older five are left
 * alone deliberately -- rewriting five passing tests to prove a refactor is a
 * bad trade, and a divergence that is written down is cheaper than a migration
 * that breaks a working check.
 *
 * WHAT IT ACTUALLY CATCHES, in the order the failures were first met:
 *
 *   1  identifier audit -- an <%= name %> for a field the display file does not
 *      declare throws inside EJS and aborts the WHOLE render. The screen is
 *      blank and white, with no error in the browser, in the joblog or on the
 *      5250 side. This is the single most valuable check here.
 *   2  overflow -- scrollWidth against clientWidth, under Genie's hostile
 *      `div { white-space: nowrap; z-index: 10; padding: 1px }`, in French,
 *      which is longer than English and is where spills show up first.
 *   3  hit-testing -- a visible-but-covered control screenshots perfectly and
 *      does nothing. elementFromPoint on the control's own centre is the only
 *      check that notices.
 *   4  target size -- a control can be present, visible and hit-testable and
 *      still be too small for a gloved thumb.
 *   5  squashed content -- scrollWidth inside a control, and the empty-value
 *      trap: <input type="number"> SILENTLY empties itself when handed a
 *      localised "24,00", so the French screen comes up with blank boxes and
 *      looks fine to every other check.
 *   6  sticky bands -- asserted by actually SCROLLING, because a pinned band
 *      that has quietly stopped pinning looks identical until you try.
 * ================================================================== */

'use strict';

const ejs = require('ejs');
const fs = require('fs');
const path = require('path');
const { chromium } = require('playwright-core');

const UI = path.resolve(__dirname, '../../htdocs/profoundui/userdata/ui');
const CH = '/home/coder/.cache/ms-playwright/chromium-1228/chrome-linux64/chrome';
const SHOT = '/tmp/ejspre';

/* Genie's globals, injected exactly as the skin applies them. Testing without
 * these passes screens that break the moment they are deployed. */
const GENIE = 'html,body{margin:0;padding:0}' +
              'div{white-space:nowrap;z-index:10;padding:1px}';

/* THE VIEWPORT THE SHIM APPLIES, character for character.
 *
 * This used to be `width=412` -- the test told the browser the answer. Genie's
 * start.html declares no viewport meta at all, so a real phone lays the page out
 * at ~980px and scales it down: every media query read a desktop width and every
 * screen was a shrunken desktop, and this harness could not see it because it
 * had already supplied a viewport the real page did not have.
 *
 * Now it injects exactly what the deployed shim injects, and asserts the layout
 * viewport really is the device width. test-device.js covers the mechanism -- and
 * the negative case, that with no meta the layout is 980. */
const SHIM_VIEWPORT = 'width=device-width, initial-scale=1, viewport-fit=cover';

const DEFAULT_CASES = [
  /* The target device, emulated as a phone: 412x915 CSS px at a 3.5x device
   * pixel ratio, isMobile on. */
  { lang: 'EN', w: 412, h: 915, mobile: true, dsf: 3.5 },
  { lang: 'FR', w: 412, h: 915, mobile: true, dsf: 3.5 },
  /* The narrowest realistic phone, in the longer language: worst case. */
  { lang: 'FR', w: 360, h: 800, mobile: true, dsf: 3 },
  /* A desk browser: more columns, not wider ones. */
  { lang: 'EN', w: 1280, h: 900 }
];

/* Identifiers that are JavaScript, not display-file fields. */
const GLOBALS = ['typeof', 'Number', 'String', 'Math', 'JSON', 'Array',
                 'Object', 'isFinite', 'parseInt', 'parseFloat'];

function auditIdentifiers(src, declared) {
  /* Anything the template declares itself is a local, not a field. */
  const locals = new Set();
  [...src.matchAll(/\b(?:var|function)\s+([A-Za-z_$][\w$]*)/g)]
    .forEach((m) => locals.add(m[1]));
  [...src.matchAll(/function\s*\(([^)]*)\)/g)].forEach((m) =>
    m[1].split(',').map((x) => x.trim()).filter(Boolean).forEach((x) => locals.add(x)));
  /* forEach/catch bindings are locals too. */
  [...src.matchAll(/catch\s*\(\s*([A-Za-z_$][\w$]*)/g)].forEach((m) => locals.add(m[1]));

  const refs = new Set([...src.matchAll(/<%[=-]\s*([A-Za-z_$][\w$]*)/g)].map((m) => m[1]));
  return [...refs].filter((r) =>
    !declared.includes(r) && !locals.has(r) && GLOBALS.indexOf(r) === -1);
}

async function checkScreen(browser, sc, c, report) {
  const src = fs.readFileSync(path.join(UI, sc.tpl[0], sc.tpl[1]), 'utf8');
  const css = sc.css.map((f) => fs.readFileSync(path.join(UI, f), 'utf8')).join('\n');

  let html;
  try {
    html = ejs.render(src, sc.data(c.lang));
  } catch (e) {
    report.fail('  RENDER FAILED ' + c.lang + ' ' + c.w + 'px: ' + e.message);
    return;
  }

  /* isMobile/hasTouch matter: they are what make the browser honour (or miss)
   * the viewport meta the way a phone does. Without them this is a desktop
   * window that happens to be 412px wide, which is a different thing. */
  const page = await browser.newPage({
    viewport: { width: c.w, height: c.h },
    deviceScaleFactor: c.dsf || 1,
    isMobile: !!c.mobile,
    hasTouch: !!c.mobile
  });
  await page.setContent(
    '<!doctype html><html><head><meta charset="utf-8">' +
    '<meta name="viewport" content="' + SHIM_VIEWPORT + '">' +
    '<style>' + GENIE + '</style><style>' + css + '</style></head><body>' +
    html + '</body></html>', { waitUntil: 'load' });

  const label = (c.lang + ' ' + c.w + 'px').padEnd(11);

  /* The layout viewport must BE the device width. If this fails, nothing below
   * it means anything -- every measurement would be taken against a phantom
   * desktop. */
  const lw = await page.evaluate(() => document.documentElement.clientWidth);
  if (lw !== c.w) {
    report.fail('  ' + label + ' LAYOUT VIEWPORT is ' + lw + 'px, not ' + c.w +
                'px -- the page is being laid out for a different screen');
  }

  /* ---- 2. overflow -------------------------------------------------- */
  const o = await page.evaluate(() => {
    const de = document.documentElement;
    const wide = [];
    document.querySelectorAll('.gt-app *').forEach((el) => {
      const r = el.getBoundingClientRect();
      if (r.right > de.clientWidth + 1) {
        wide.push((typeof el.className === 'string' && el.className
                    ? el.className : el.tagName) + ' r=' + Math.round(r.right));
      }
    });
    return { sw: de.scrollWidth, cw: de.clientWidth, wide: wide.slice(0, 3) };
  });
  if (o.sw > o.cw + 1) {
    report.fail('  ' + label + ' OVERFLOW -> ' + o.wide.join(' | '));
  } else {
    report.ok('  ' + label + ' sw=' + o.sw + ' cw=' + o.cw + ' no overflow');
  }

  /* ---- 3. controls present, and answering for themselves ------------ */
  const ctl = await page.evaluate((sels) => sels.map((sel) => {
    const el = document.querySelector(sel);
    if (!el) return sel + ': MISSING';
    const r = el.getBoundingClientRect();
    if (!r.width || !r.height) return sel + ': HIDDEN';
    if (r.bottom < 0 || r.top > innerHeight) return null;   /* below the fold is fine */

    /* Probe a point inside BOTH the element and the viewport.
     *
     * The element's own centre is not good enough: a footer straddling the
     * bottom edge has its centre below innerHeight, elementFromPoint returns
     * null there, and the check reported "COVERED by nothing" -- a harness
     * fault that reads exactly like a product bug. This has now happened twice
     * in this project, in both directions, so the point is clamped. */
    const x = Math.min(Math.max(r.left + r.width / 2, 1), innerWidth - 1);
    const top = Math.max(r.top, 0);
    const bottom = Math.min(r.bottom, innerHeight);
    if (bottom - top < 2) return null;                      /* nothing visible to probe */
    const y = (top + bottom) / 2;

    const hit = document.elementFromPoint(x, y);
    if (!hit || !(hit === el || el.contains(hit))) {
      return sel + ': COVERED by ' +
             (hit && typeof hit.className === 'string' && hit.className
               ? hit.className : hit ? hit.tagName : 'nothing');
    }
    return null;
  }).filter(Boolean), sc.controls || []);
  if (ctl.length) report.fail('  ' + label + ' CONTROLS -> ' + ctl.join(' | '));

  /* ---- 4. big enough for a gloved thumb ----------------------------- */
  const small = await page.evaluate((sels) => sels.map((sel) => {
    const el = document.querySelector(sel);
    if (!el) return null;
    const r = el.getBoundingClientRect();
    return (r.height < 44 || r.width < 40)
      ? sel + ': ' + Math.round(r.width) + 'x' + Math.round(r.height) : null;
  }).filter(Boolean), sc.minTarget || []);
  if (small.length) report.fail('  ' + label + ' TOO SMALL -> ' + small.join(' | '));

  /* ---- 5. squashed content, and the number-input trap --------------- */
  const squashed = await page.evaluate(() => {
    const bad = [];
    document.querySelectorAll('input[type=number]').forEach((b, i) => {
      /* An <input type="number"> silently EMPTIES itself when handed a value
       * it cannot parse -- which is what a localised "24,00" is. */
      if (b.value === '') {
        bad.push('number input[' + i + '] value rejected by the browser: ' +
                 JSON.stringify(b.getAttribute('value')));
      }
    });
    document.querySelectorAll('.gt-btn, .gt-chip, .gt-st-opt, .gt-numbox')
      .forEach((el) => {
        if (el.scrollWidth > el.clientWidth + 1) {
          bad.push((el.className || el.tagName) + ' text overflows by ' +
                   (el.scrollWidth - el.clientWidth) + 'px');
        }
      });
    return bad.slice(0, 4);
  });
  if (squashed.length) report.fail('  ' + label + ' SQUASHED -> ' + squashed.join(' | '));

  /* ---- 6. sticky bands, proven by scrolling ------------------------- */
  if (sc.sticky) {
    const st = await page.evaluate(async (cfg) => {
      const band = document.querySelector(cfg.band);
      if (!band) return { err: 'band ' + cfg.band + ' MISSING' };
      const before = band.getBoundingClientRect().top;
      window.scrollTo(0, 400);
      await new Promise((r) => requestAnimationFrame(() => requestAnimationFrame(r)));
      const after = band.getBoundingClientRect().top;
      const scrolled = window.scrollY;
      const covered = (cfg.keep || []).filter((sel) => {
        const el = document.querySelector(sel);
        if (!el) return true;
        const r = el.getBoundingClientRect();
        if (!r.width || !r.height) return true;
        const hit = document.elementFromPoint(r.left + r.width / 2,
                                              r.top + r.height / 2);
        return !hit || !(hit === el || el.contains(hit));
      });
      return { before, after, scrolled, covered };
    }, sc.sticky);

    if (st.err) {
      report.fail('  ' + label + ' STICKY -> ' + st.err);
    } else if (st.scrolled < 50) {
      /* Nothing to prove: the page is not tall enough to scroll at this size. */
      report.ok('  ' + label + ' sticky not exercised (page fits)');
    } else if (st.after > st.before + 4) {
      report.fail('  ' + label + ' STICKY -> ' + sc.sticky.band +
                  ' scrolled away (top ' + Math.round(st.before) + ' -> ' +
                  Math.round(st.after) + ')');
    } else if (st.covered.length) {
      report.fail('  ' + label + ' STICKY -> pinned but covered: ' +
                  st.covered.join(', '));
    } else {
      report.ok('  ' + label + ' sticky holds after scrolling ' + st.scrolled + 'px');
    }
  }

  await page.screenshot({
    path: SHOT + '/' + sc.name + '-' + c.lang.toLowerCase() + '-' + c.w + '.png',
    fullPage: true
  });
  await page.close();
}

/* Run a set of screens. Returns the number of failures and exits non-zero, so
 * a caller can just `await run(...)`. */
async function run(title, screens, cases) {
  fs.mkdirSync(SHOT, { recursive: true });

  let failures = 0;
  const report = {
    ok: (m) => console.log(m),
    fail: (m) => { failures++; console.log(m); }
  };

  console.log('=== ' + title + ' ===');

  const browser = await chromium.launch({ executablePath: CH, args: ['--no-sandbox'] });

  for (const sc of screens) {
    const src = fs.readFileSync(path.join(UI, sc.tpl[0], sc.tpl[1]), 'utf8');

    /* ---- 1. the identifier audit ------------------------------------ */
    const unknown = auditIdentifiers(src, sc.declared);
    console.log('\n' + sc.name + '  identifier audit: ' +
                (unknown.length ? 'UNKNOWN -> ' + unknown.join(', ') : 'clean'));
    if (unknown.length) failures++;

    for (const c of (cases || DEFAULT_CASES)) {
      await checkScreen(browser, sc, c, report);
    }
  }

  await browser.close();
  console.log(failures ? '\n' + failures + ' case(s) failed' : '\nAll cases clean');
  process.exit(failures ? 1 : 0);
}

module.exports = { run, UI, DEFAULT_CASES };
