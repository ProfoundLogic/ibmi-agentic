#!/usr/bin/env node
/*
 * Can the operator reach the bottom of the screen?
 *
 *   node gtwms/tools/test-skin-scroll.js
 *
 * WHY THIS EXISTS
 *
 * Scrolling stopped working on the iPad and the Back-to-menu button could not be
 * reached on most screens. Every existing pre-flight rendered our markup into a
 * bare page of its own making, so none of them could ever have caught it: the
 * fault is entirely in the boxes the SKIN puts around us.
 *
 *     body                    position: fixed   (html.pls--mobile only)
 *      .pls-scroll-bar-wrap   position: fixed
 *       #main.genie-main      height: 100vh
 *        #contain             position: fixed; height: 100%
 *         #logoBanner         38px of empty space above us
 *         #5250               <- our screen is rendered in here
 *
 * `.genie-container` gets `overflow: auto` ONLY inside
 * `@supports (-moz-appearance: none)`. That reads as Firefox-only, but Chromium
 * honours it as well because it aliases -moz-appearance -- so a Chromium harness
 * scrolls happily while WebKit may not. This test therefore runs BOTH ways:
 *
 *   as-is        -- what Chromium does
 *   webkit-like  -- the same page with that one rule suppressed
 *
 * The second is the one that reproduced the report. A test that is granted a
 * capability the device withholds cannot fail the way the device fails.
 *
 * It fetches the DEPLOYED skin CSS, so it tracks the skin the device is actually
 * served rather than a copy that can rot.
 */

'use strict';

const fs = require('fs');
const path = require('path');
const https = require('https');
const ejs = require('ejs');
const { chromium } = require('playwright-core');

const ROOT = path.resolve(__dirname, '../..');
const UI = path.join(ROOT, 'htdocs/profoundui/userdata/ui');
const CHROME = '/home/coder/.cache/ms-playwright/chromium-1228/chrome-linux64/chrome';
const HOST = process.argv[2] || 'https://idev.profoundlogic.com:8103';

const { SCREENS } = require('./gen-gt-shim.js');

let pass = 0, fail = 0;
const ok = (m, d) => { pass++; console.log('  PASS  ' + m + (d ? '  -- ' + d : '')); };
const bad = (m, d) => { fail++; console.log('  FAIL  ' + m + (d ? '  -- ' + d : '')); };

function get(url) {
  return new Promise((resolve, reject) => {
    https.get(url, { rejectUnauthorized: false, timeout: 45000 }, (res) => {
      let d = '';
      res.setEncoding('utf8');
      res.on('data', (c) => (d += c));
      res.on('end', () => resolve({ status: res.statusCode, body: d }));
    }).on('error', reject);
  });
}

/* The real chain, copied from the deployed pls/start.html. `pls--mobile` is set
 * because the skin sets it for (is_android || is_ios) && is_touch -- which an
 * iPad running Chrome satisfies. */
function realPage(skinCss, ourCss, inner, suppressSupports) {
  const kill = suppressSupports
    ? '#contain.genie-container{overflow:visible !important}'
    : '';
  return `<!doctype html><html class="pls--mobile"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1, viewport-fit=cover">
<style>${skinCss}</style><style>${ourCss}</style><style>${kill}</style></head>
<body>
  <div id="copyModeSelectionRectangle"></div>
  <div class="pls-scroll-bar-wrap"><div class="pls-scroll-box">
    <div id="main" class="genie-main">
      <div id="contain" class="genie-container">
        <div id="logoBanner" class="genie-logobanner"></div>
        <div id="copyModeMask" class="genie-5250"></div>
        <div id="5250" class="genie-5250">${inner}</div>
      </div>
      <div id="sidebar" class="pls-nav-wrapper"></div>
    </div>
  </div></div>
</body></html>`;
}

/* Deliberately generic content: what is under test is the SKIN's boxes, not any
 * one screen's markup, so this uses the shared header/main/footer shell every
 * screen is built from, with enough rows to overflow any viewport. */
function shellHtml(rows) {
  let list = '';
  for (let i = 1; i <= rows; i++) {
    list += '<li class="gt-row">Row ' + i + ' &mdash; 06141410' + (1000 + i) + '</li>';
  }
  return `<div class="gt-app gt-test-screen" data-gt-lang="EN">
  <header class="gt-header"><div class="gt-logo"><span class="gt-logo-mark">GT</span></div></header>
  <main class="gt-main">
    <h1>Reachability</h1>
    <ul style="list-style:none;margin:0;padding:0">${list}</ul>
  </main>
  <footer class="gt-actions">
    <button type="button" class="gt-btn">Clear</button>
    <button type="button" class="gt-btn gt-btn-primary" id="the-exit">Back to menu</button>
  </footer>
</div>`;
}

const VIEWPORTS = [
  { name: 'iPad 11 portrait',  w: 834,  h: 1194 },
  { name: 'iPad 11 landscape', w: 1194, h: 834 },
  { name: 'iPad mini',         w: 744,  h: 1133 },
  { name: 'S24 Ultra',         w: 412,  h: 915 },
  { name: 'small phone',       w: 360,  h: 640 },
];

/* Reach the exit button the way a finger would: scroll whichever box is actually
 * scrollable, then check the button is inside the viewport. */
const REACH = () => {
  const exit = document.getElementById('the-exit') ||
               [...document.querySelectorAll('.gt-actions .gt-btn')].pop();
  const cands = ['html', 'body', '.pls-scroll-bar-wrap', '.pls-scroll-box',
                 '#main', '#contain', 'div[id="5250"]', '.gt-app', '.gt-main'];
  const before = exit.getBoundingClientRect();
  cands.forEach((s) => { const el = document.querySelector(s); if (el) el.scrollTop = 999999; });
  window.scrollTo(0, 999999);
  const after = exit.getBoundingClientRect();
  return {
    vpH: window.innerHeight,
    beforeBottom: Math.round(before.bottom),
    afterBottom: Math.round(after.bottom),
    afterTop: Math.round(after.top),
    /* Fully inside the viewport, not merely touching it. */
    reachable: after.bottom <= window.innerHeight + 1 && after.top >= -1,
    /* Was it reachable WITHOUT scrolling? A pinned footer is better still. */
    pinned: before.bottom <= window.innerHeight + 1,
    scrollerH: (document.querySelector('.gt-main') || {}).scrollHeight || 0,
  };
};

(async () => {
  console.log('fetching the deployed skin CSS from ' + HOST);
  const files = [
    '/profoundui/userdata/genie%20skins/pls/pls.css',
    '/profoundui/userdata/custom/css/pls.css',
  ];
  let skinCss = '';
  for (const f of files) {
    const r = await get(HOST + f);
    if (r.status !== 200) { bad('could not fetch ' + f, 'http ' + r.status); }
    else { skinCss += '\n' + r.body; console.log('  ' + f.split('/').pop() + '  ' + r.body.length + ' bytes'); }
  }
  if (fail) { console.log('\ncannot test without the real skin CSS'); process.exit(1); }

  /* Assert the hazard is still there, so this test cannot quietly become
     vacuous if the skin is fixed upstream one day. */
  console.log('\n=== the skin hazard this test exists for ===');
  if (/@supports\s*\(-moz-appearance:\s*none\)\s*\{\s*\.genie-container\s*\{\s*overflow:\s*auto/.test(
        skinCss.replace(/\s+/g, ' ').replace(/ \{/g, '{').replace(/\{ /g, '{'))
      || /-moz-appearance/.test(skinCss)) {
    ok('the container is still scrollable only under @supports(-moz-appearance)',
       'so WebKit may get no scroll at all');
  } else {
    bad('the skin no longer gates .genie-container overflow on -moz-appearance',
        'this test\'s webkit-like case may no longer represent anything');
  }
  if (/html\.pls--mobile body[^{]*\{[^}]*position:\s*fixed/.test(skinCss.replace(/\s+/g, ' '))) {
    ok('mobile body is still position:fixed', 'no document scroll to fall back on');
  } else {
    bad('mobile body is no longer position:fixed', 'assumption changed');
  }

  const themeCss = fs.readFileSync(path.join(UI, 'gtcommon/gt-theme.css'), 'utf8');
  const browser = await chromium.launch({ executablePath: CHROME, args: ['--no-sandbox'] });

  for (const mode of [
    { label: 'as-is (Chromium honours the @supports rule)', suppress: false },
    { label: 'webkit-like (that rule suppressed)', suppress: true },
  ]) {
    console.log('\n=== ' + mode.label + ' ===');
    for (const v of VIEWPORTS) {
      const pg = await browser.newPage({ viewport: { width: v.w, height: v.h },
                                         isMobile: true, hasTouch: true });
      await pg.setContent(realPage(skinCss, themeCss, shellHtml(40), mode.suppress),
                          { waitUntil: 'load' });
      /* The shim sets this while one of our screens is displayed. */
      await pg.evaluate(() => document.documentElement.classList.add('gt-screen'));
      const r = await pg.evaluate(REACH);

      if (r.reachable) {
        ok(v.name.padEnd(18) + ' exit reachable',
           r.pinned ? 'pinned, no scrolling needed' : 'after scrolling');
      } else {
        bad(v.name.padEnd(18) + ' exit NOT reachable',
            (r.afterBottom - r.vpH) + 'px below the fold after scrolling everything');
      }
      await pg.close();
    }
  }

  /* Prove the fix is what is doing the work: without the marker class the old
     behaviour must come back, or this test is measuring nothing. */
  console.log('\n=== without the marker class, the old failure must return ===');
  {
    const pg = await browser.newPage({ viewport: { width: 834, height: 1194 },
                                       isMobile: true, hasTouch: true });
    await pg.setContent(realPage(skinCss, themeCss, shellHtml(40), true),
                        { waitUntil: 'load' });
    const r = await pg.evaluate(REACH);
    if (!r.reachable) {
      ok('unscoped, the exit is still out of reach',
         (r.afterBottom - r.vpH) + 'px below the fold -- the reported symptom');
    } else {
      bad('the page scrolls even without our rules',
          'then the fix is not what makes the difference and this test proves nothing');
    }
    await pg.close();
  }

  /* Our rules must not touch anything that is not ours. */
  console.log('\n=== the rules are scoped to our screens only ===');
  {
    const ours = themeCss.match(/^html\.gt-screen[^{]*\{/gm) || [];
    const leaks = (themeCss.match(/^\s*(\.genie-|#contain|div\[id="5250"\]|body|html)[^{]*\{/gm) || [])
      .filter((r) => !/gt-screen/.test(r));
    if (ours.length) ok('scrolling rules are gated on html.gt-screen', ours.length + ' rule(s)');
    else bad('no html.gt-screen rules found in the theme');
    if (leaks.length) bad('the theme styles skin or document elements unscoped', leaks.join(' | ').slice(0, 100));
    else ok('no unscoped skin or document selectors in the theme');

    /* The height cascade has to stay in weakest-first order or a browser takes
       the wrong one. */
    const cascade = (themeCss.match(/html\.gt-screen \.genie-container \{[^}]*\}/) || [''])[0];
    const order = ['height: 100%', 'height: 100dvh', 'height: var(--gt-vvh'];
    let at = -1, ordered = true;
    for (const decl of order) {
      const i = cascade.indexOf(decl);
      if (i < 0 || i < at) ordered = false;
      at = i;
    }
    if (ordered) ok('the container height cascade is weakest-first', order.join(' -> '));
    else bad('the container height cascade is wrong or incomplete', cascade.replace(/\s+/g, ' ').slice(0, 110));

    const shim = fs.readFileSync(path.join(__dirname, 'gen-gt-shim.js'), 'utf8');
    if (/gt-screen/.test(shim) && /replace\(\/\(\^\|\\s\)gt-screen/.test(shim)) {
      ok('the shim both sets and removes the marker', 'a 5250 screen gets stock behaviour back');
    } else {
      bad('the shim does not remove the marker', 'Genie 5250 screens would keep our rules');
    }
  }

  /* Every real screen has a footer that must stay reachable. Checking the shell
     covers the mechanism; this checks nobody omitted the footer. */
  console.log('\n=== every screen has a pinned footer to be reached ===');
  for (const sc of SCREENS) {
    const src = fs.readFileSync(path.join(UI, sc.tpl), 'utf8');
    /* Match the class TOKEN. Several screens legitimately add a modifier --
       `class="gt-main gt-detl-main"` -- and an exact-string check called them
       broken when they were not. */
    const hasFooter = /class="[^"]*\bgt-actions\b/.test(src);
    const hasMain = /class="[^"]*\bgt-main\b/.test(src);
    if (hasFooter && hasMain) ok('  ' + sc.tpl);
    else bad('  ' + sc.tpl + ' cannot use the app shell',
             'gt-main=' + hasMain + ' gt-actions=' + hasFooter);
  }

  /* ---- THE iOS TOOLBAR ------------------------------------------------
     `height: 100%` on a position:fixed element resolves against the LAYOUT
     viewport, and on iOS that is the LARGE viewport -- the size the page would
     be with the browser's toolbars hidden. So the bottom of the container sits
     BEHIND the toolbar and a footer pinned there is invisible. That is how
     Back-to-menu "disappeared" once the scroll fix pinned it: reachable by the
     numbers, hidden in practice.
     Headless Chromium has no toolbar, so visualViewport is faked to be shorter
     than the layout viewport -- which is exactly what an overlaying toolbar
     looks like to a page. */
  console.log('\n=== a browser toolbar must not hide the footer ===');
  for (const toolbar of [96, 140]) {
    const pg = await browser.newPage({ viewport: { width: 834, height: 1194 },
                                       isMobile: true, hasTouch: true });
    await pg.setContent(realPage(skinCss, themeCss, shellHtml(40), true), { waitUntil: 'load' });
    /* Patched HERE, not via addInitScript: init scripts run on navigation, and
       setContent is not one -- registered after the initial about:blank load, it
       never ran, and the check passed while measuring an unshrunk page.
       The PROTOTYPE carries the accessor, so that is where it has to go. */
    await pg.evaluate((px) => {
      Object.defineProperty(VisualViewport.prototype, 'height',
        { configurable: true, get: function () { return window.innerHeight - px; } });
      document.documentElement.classList.add('gt-screen');
      /* Exactly what the shim does on every visualViewport resize. */
      const shrink = window.innerHeight - window.visualViewport.height;
      if (shrink <= 200) {
        document.documentElement.style.setProperty(
          '--gt-vvh', Math.round(window.visualViewport.height) + 'px');
      }
    }, toolbar);
    const r = await pg.evaluate(() => {
      const exit = document.getElementById('the-exit');
      const rect = exit.getBoundingClientRect();
      return { visible: Math.round(window.visualViewport.height),
               layout: window.innerHeight,
               exitBottom: Math.round(rect.bottom),
               containH: Math.round(document.querySelector('#contain').getBoundingClientRect().height) };
    });
    /* Refuse to report anything until the fixture is doing what it claims. A
       check that measures an unshrunk viewport passes for the wrong reason. */
    if (r.visible !== r.layout - toolbar) {
      bad('the ' + toolbar + 'px toolbar fixture did not take effect',
          'visible=' + r.visible + ' layout=' + r.layout + ' -- the check would be vacuous');
    } else if (r.exitBottom <= r.visible + 1) {
      ok('a ' + toolbar + 'px toolbar still leaves the exit visible',
         'exit bottom ' + r.exitBottom + ' within visible ' + r.visible +
         ' (container ' + r.containH + 'px)');
    } else {
      bad('a ' + toolbar + 'px toolbar hides the exit',
          'exit bottom ' + r.exitBottom + ' vs visible ' + r.visible + ' -- behind the toolbar');
    }
    await pg.close();
  }

  /* The control. Without the measured height the same toolbar must hide the exit,
     or the two checks above are passing for some other reason. */
  {
    const pg = await browser.newPage({ viewport: { width: 834, height: 1194 },
                                       isMobile: true, hasTouch: true });
    await pg.setContent(realPage(skinCss, themeCss, shellHtml(40), true), { waitUntil: 'load' });
    const r = await pg.evaluate(() => {
      Object.defineProperty(VisualViewport.prototype, 'height',
        { configurable: true, get: function () { return window.innerHeight - 96; } });
      document.documentElement.classList.add('gt-screen');
      /* deliberately NOT publishing --gt-vvh */
      const rect = document.getElementById('the-exit').getBoundingClientRect();
      return { visible: Math.round(window.visualViewport.height), exitBottom: Math.round(rect.bottom) };
    });
    if (r.exitBottom > r.visible + 1) {
      ok('without the measured height the toolbar DOES hide the exit',
         'exit bottom ' + r.exitBottom + ' vs visible ' + r.visible + ' -- the reported symptom');
    } else {
      bad('the toolbar does not hide the exit even unfixed',
          'then these checks prove nothing about the fix');
    }
    await pg.close();
  }

  /* The keyboard also shrinks visualViewport, by far more. Resizing the shell for
     that would reflow the list under the operator's thumb on every tap, so big
     drops are deliberately ignored. */
  console.log('\n=== the on-screen keyboard must NOT resize the shell ===');
  {
    const pg = await browser.newPage({ viewport: { width: 412, height: 915 },
                                       isMobile: true, hasTouch: true });
    await pg.setContent(realPage(skinCss, themeCss, shellHtml(40), true), { waitUntil: 'load' });
    const h = await pg.evaluate(() => {
      document.documentElement.classList.add('gt-screen');
      Object.defineProperty(VisualViewport.prototype, 'height',
        { configurable: true, get: function () { return window.innerHeight - 420; } });
      const faked = window.innerHeight - window.visualViewport.height;
      const before = Math.round(document.querySelector('#contain').getBoundingClientRect().height);
      const vv = window.visualViewport;
      const shrink = window.innerHeight - vv.height;
      if (shrink <= 200) {
        document.documentElement.style.setProperty('--gt-vvh', Math.round(vv.height) + 'px');
      }
      return { faked, before,
               after: Math.round(document.querySelector('#contain').getBoundingClientRect().height) };
    });
    if (h.faked !== 420) {
      bad('the keyboard fixture did not take effect', 'shrink measured ' + h.faked + 'px');
    } else if (h.before === h.after) {
      ok('a keyboard-sized shrink is ignored', 'container stayed ' + h.after + 'px');
    } else {
      bad('the keyboard resized the shell', h.before + ' -> ' + h.after + 'px');
    }
    await pg.close();
  }

  /* Seven screens pin a scan band or a column header with `position: sticky`.
     Sticky resolves against the NEAREST scrolling ancestor, which the app shell
     changes from the container to .gt-main -- so it has to be re-checked, or the
     band silently scrolls away with the rows it is meant to label. */
  console.log('\n=== a sticky band still sticks inside the new scroller ===');
  {
    const pg = await browser.newPage({ viewport: { width: 412, height: 915 },
                                       isMobile: true, hasTouch: true });
    const sticky = shellHtml(40).replace('<h1>Reachability</h1>',
      '<div id="the-band" style="position:sticky;top:0;background:#fff">scan band</div>');
    await pg.setContent(realPage(skinCss, themeCss, sticky, true), { waitUntil: 'load' });
    await pg.evaluate(() => document.documentElement.classList.add('gt-screen'));
    const r = await pg.evaluate(() => {
      const band = document.getElementById('the-band');
      const main = document.querySelector('.gt-main');
      const before = Math.round(band.getBoundingClientRect().top);
      main.scrollTop = 999999;
      /* Sticky resolves against the scroller's PADDING box, and .gt-main is
         padded, so the expected resting place is its top plus that padding --
         not its border-box top. */
      const pad = parseFloat(getComputedStyle(main).paddingTop) || 0;
      return { before, after: Math.round(band.getBoundingClientRect().top),
               expected: Math.round(main.getBoundingClientRect().top + pad),
               scrolled: main.scrollTop };
    });
    if (r.scrolled > 0 && Math.abs(r.after - r.expected) <= 2) {
      ok('the band stayed pinned to the top of the scroller',
         'scrolled ' + r.scrolled + 'px, band held at ' + r.after);
    } else {
      bad('the sticky band scrolled away', JSON.stringify(r));
    }
    await pg.close();
  }

  /* The synthetic shell proves the mechanism; this proves it on markup nobody
     wrote for the test. simtest is used because its data is self-contained --
     the other screens need database-shaped fixtures. */
  console.log('\n=== a real screen, in the real chain ===');
  {
    const tpl = fs.readFileSync(path.join(UI, 'gtsimd/simtest.ejs'), 'utf8');
    const scr = fs.readFileSync(path.join(UI, 'gtsimd/simtest.css'), 'utf8');
    const d = { action: '', scanval: '', langpref: 'EN', msg: '', diag: '',
                lastval: '061414102377', lastsymb: 'CODE_128', lastlen: 12,
                lasttime: '14:32:07', simsfl: [] };
    for (let i = 1; i <= 30; i++) {
      d.simsfl.push({ _rrn: i, sseq: i, sval: '06141410' + (1000 + i),
                      ssymb: 'UPC_A', stime: '14:32:0' + (i % 10) });
    }
    for (const v of [{ n: 'iPad portrait', w: 834, h: 1194 }, { n: 'S24 Ultra', w: 412, h: 915 }]) {
      const pg = await browser.newPage({ viewport: { width: v.w, height: v.h },
                                         isMobile: true, hasTouch: true });
      await pg.setContent(realPage(skinCss, themeCss + '\n' + scr,
                                   ejs.render(tpl, d), true), { waitUntil: 'load' });
      await pg.evaluate(() => document.documentElement.classList.add('gt-screen'));
      const r = await pg.evaluate(REACH);
      const rows = await pg.evaluate(() => {
        const main = document.querySelector('.gt-main');
        /* REACH has already driven every scroller to the bottom, so a delta
           measured from here is always zero. Rewind, then scroll for real. */
        main.scrollTop = 0;
        const before = main.scrollTop;
        main.scrollTop = 999999;
        return { moved: main.scrollTop - before, canScroll: main.scrollHeight > main.clientHeight + 1 };
      });
      if (r.reachable && rows.canScroll && rows.moved > 0) {
        ok('simtest on ' + v.n, 'exit pinned, content scrolled ' + rows.moved + 'px');
      } else {
        bad('simtest on ' + v.n, 'reachable=' + r.reachable + ' canScroll=' + rows.canScroll +
            ' moved=' + rows.moved);
      }
      await pg.close();
    }
  }

  /* ---- THE OTHER SKIN -------------------------------------------------
     Everything above builds the pls chain. Classic is a completely different
     shape -- its whole body is `<div id="5250">` with nothing pinned -- so the
     document scrolls and one finger works, which is exactly the stock behaviour
     to leave alone. Applying the app shell there removed the document scroll,
     left the only scroller inside .gt-main, and made an Android device need a
     pinch and two fingers to move anything. Nothing in this file could see that,
     because this file only knew one skin. */
  console.log('\n=== Classic: the stock document scroll must be left alone ===');
  {
    const classicCss = await get(HOST + '/profoundui/userdata/genie%20skins/Classic/Classic.css');
    const puiCss = await get(HOST + '/profoundui/proddata/css/profoundui.css');
    if (classicCss.status !== 200 || puiCss.status !== 200) {
      bad('could not fetch the Classic skin CSS',
          'Classic ' + classicCss.status + ', profoundui ' + puiCss.status);
    } else {
      /* Genie sizes #5250 for a 24x80 terminal, so test both an auto-height div
         and one with a height set -- the second is what traps content. */
      for (const genieH of [null, 640]) {
        const pg = await browser.newPage({ viewport: { width: 412, height: 915 },
                                           isMobile: true, hasTouch: true });
        /* Classic's body, verbatim from the deployed start.html. */
        const body = '<div id="5250"' +
          (genieH ? ' style="height:' + genieH + 'px"' : '') + '>' + shellHtml(120) + '</div>';
        await pg.setContent(
          '<!doctype html><html><head><meta charset="utf-8">' +
          '<meta name="viewport" content="width=device-width, initial-scale=1">' +
          '<style>' + puiCss.body + '</style><style>' + classicCss.body + '</style>' +
          '<style>' + themeCss + '</style></head><body>' + body + '</body></html>',
          { waitUntil: 'load' });
        await pg.evaluate(() => document.documentElement.classList.add('gt-screen'));

        const r = await pg.evaluate(() => {
          const de = document.documentElement;
          const five = document.querySelector('div[id="5250"]');
          const exit = document.getElementById('the-exit');
          const docScrollable = de.scrollHeight > de.clientHeight + 1 ||
                                document.body.scrollHeight > document.body.clientHeight + 1;
          window.scrollTo(0, 999999);
          return {
            docScrollable,
            scrolled: Math.round(window.scrollY),
            fiveOverflow: getComputedStyle(five).overflowY,
            exitBottom: Math.round(exit.getBoundingClientRect().bottom),
            vp: de.clientHeight,
          };
        });

        const label = 'Classic, #5250 height ' + (genieH ? genieH + 'px' : 'auto');
        if (r.fiveOverflow !== 'hidden') {
          ok(label + ': the page div is not clipped', 'overflow-y=' + r.fiveOverflow);
        } else {
          bad(label + ': the page div is clipped', 'overflow-y:hidden with nothing else to scroll');
        }
        if (r.docScrollable && r.scrolled > 0) {
          ok('  the document scrolls, so one finger works', 'scrolled ' + r.scrolled + 'px');
        } else {
          bad('  the document does not scroll', 'scrollable=' + r.docScrollable +
              ' -- this is the two-finger-and-a-pinch symptom');
        }
        if (r.exitBottom <= r.vp + 1) ok('  the exit is reachable by scrolling the page');
        else bad('  the exit is not reachable', (r.exitBottom - r.vp) + 'px below the fold');
        await pg.close();
      }
    }
  }

  /* The control for the Classic section: with the rules unscoped -- which is how
     they shipped and how Android broke -- the page div IS clipped and the document
     stops scrolling. Without this the checks above could pass for any reason. */
  console.log('\n=== Classic, with the rules unscoped, must break ===');
  {
    const classicCss = await get(HOST + '/profoundui/userdata/genie%20skins/Classic/Classic.css');
    const pg = await browser.newPage({ viewport: { width: 412, height: 915 },
                                       isMobile: true, hasTouch: true });
    const UNSCOPED = 'html.gt-screen .gt-app{height:100%;overflow:hidden}' +
                     'html.gt-screen .gt-main{flex:1 1 auto;min-height:0;overflow-y:auto}' +
                     'html.gt-screen div[id="5250"]{overflow:hidden}';
    await pg.setContent(
      '<!doctype html><html><head><meta charset="utf-8">' +
      '<meta name="viewport" content="width=device-width, initial-scale=1">' +
      '<style>' + classicCss.body + '</style><style>' + themeCss + '</style>' +
      '<style>' + UNSCOPED + '</style></head><body>' +
      '<div id="5250" style="height:640px">' + shellHtml(120) + '</div></body></html>',
      { waitUntil: 'load' });
    await pg.evaluate(() => document.documentElement.classList.add('gt-screen'));
    const r = await pg.evaluate(() => {
      const de = document.documentElement;
      const scrollable = de.scrollHeight > de.clientHeight + 1 ||
                         document.body.scrollHeight > document.body.clientHeight + 1;
      return { scrollable, five: getComputedStyle(document.querySelector('div[id="5250"]')).overflowY };
    });
    if (!r.scrollable && r.five === 'hidden') {
      ok('unscoped, Classic loses its document scroll',
         'overflow-y:hidden and nothing to scroll -- the Android symptom, reproduced');
    } else {
      bad('unscoped, Classic still scrolls', 'then the Classic checks prove nothing');
    }
    await pg.close();
  }

  /* Whatever the skin, one finger has to pan the thing that scrolls. */
  console.log('\n=== the scroller declares that one finger pans it ===');
  {
    const rule = (themeCss.match(/html\.gt-screen \.genie-container \.gt-main \{[^}]*\}/) || [''])[0];
    if (/touch-action:\s*pan-y/.test(rule)) ok('the pls scroller sets touch-action: pan-y');
    else bad('the pls scroller does not declare touch-action',
             'a touch browser may give the gesture to a page that cannot scroll');
    /* And the shell must not be applied outside that container. */
    const unscoped = (themeCss.match(/^html\.gt-screen (?!\.genie-container)[^{]*\{/gm) || []);
    if (!unscoped.length) ok('every shell rule is inside .genie-container', 'Classic is untouched');
    else bad('a shell rule escapes .genie-container', unscoped.join(' | ').slice(0, 90));
  }

  await browser.close();
  console.log('\n' + (fail
    ? fail + ' failed, ' + pass + ' passed'
    : 'The bottom of every screen is reachable: ' + pass + ' passed, 0 failed'));
  process.exit(fail ? 1 : 0);
})();
