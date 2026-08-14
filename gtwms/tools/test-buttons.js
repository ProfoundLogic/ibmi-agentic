#!/usr/bin/env node
/*
 * Every button's label must sit in the middle of it.
 *
 *   node gtwms/tools/test-buttons.js
 *
 * WHY THIS EXISTS
 *
 * Retake was reported as "formatting off". It was: its label sat left of centre
 * and hard against the top of a 64px button, while every button beside it was
 * centred.
 *
 * The cause is a rule that reads as harmless. A <button> centres its own label
 * only while its display is block or inline-block. `.gt-photo-retake` and
 * `.gt-photo-choose` are hidden by default and revealed with
 * `display: inline-flex` -- and as flex items that BLOCKIFIES to `flex`, making
 * the button a flex container, which by default puts its content at the start
 * and stretches it. Native centring is gone and nothing says so.
 *
 * Measured before the fix, in the same row:
 *
 *   gt-photo-use     display=block  text left 37, right 37, top 23   centred
 *   gt-photo-retake  display=flex   text left 28, right 46, top  0   NOT centred
 *
 * The fix is at the base -- `.gt-btn` declares its own centring -- so a button
 * renders identically whatever display value a show/hide rule leaves it with.
 * This test holds that, and holds the general rule: no button's label may sit
 * off-centre in either axis.
 */

'use strict';

const fs = require('fs');
const path = require('path');
const ejs = require('ejs');
const { chromium } = require('playwright-core');

const ROOT = path.resolve(__dirname, '../..');
const UI = path.join(ROOT, 'htdocs/profoundui/userdata/ui');
const CHROME = '/home/coder/.cache/ms-playwright/chromium-1228/chrome-linux64/chrome';

let pass = 0, fail = 0;
const ok = (m, d) => { pass++; console.log('  PASS  ' + m + (d ? '  -- ' + d : '')); };
const bad = (m, d) => { fail++; console.log('  FAIL  ' + m + (d ? '  -- ' + d : '')); };

const themeCss = fs.readFileSync(path.join(UI, 'gtcommon/gt-theme.css'), 'utf8');

/* Genie's own rules, reproduced: nowrap, a z-index and padding on every div. */
const GENIE = 'html,body{margin:0;padding:0}div{white-space:nowrap;z-index:10;padding:1px}';

/* Where every label sits inside its button. A Range around the text contents is
 * the only honest way to ask -- the button's own box says nothing about it. */
const MEASURE = () => {
  const out = [];
  document.querySelectorAll('.gt-btn').forEach((el) => {
    const c = getComputedStyle(el);
    if (c.display === 'none' || !el.offsetParent && c.position !== 'fixed') {
      const r0 = el.getBoundingClientRect();
      if (r0.width === 0 && r0.height === 0) return;      /* genuinely hidden */
    }
    const r = el.getBoundingClientRect();
    if (r.width === 0 || r.height === 0) return;
    const range = document.createRange();
    range.selectNodeContents(el);
    const t = range.getBoundingClientRect();
    if (t.width === 0) return;                            /* icon-only */
    out.push({
      id: el.id || (typeof el.className === 'string' ? el.className : 'button'),
      label: (el.textContent || '').trim().slice(0, 24),
      display: c.display,
      dx: Math.round((t.left - r.left) - (r.right - t.right)),
      dy: Math.round((t.top - r.top) - (r.bottom - t.bottom)),
    });
  });
  return out;
};

function itmdetlData() {
  const d = {
    action: '', langpref: 'EN', opername: 'Marie Tremblay', msg: '',
    dsku: 'APPL-000237', ddesc: 'Ladies Fleece Hoodie - X-Large', ddept: 'APPL',
    duom: 'EA', dcasepk: 12, dweight: '.467', dlen: '43.3', dwid: '17.8',
    dhgt: '9.3', dretail: '18.69', dupc: '0614141002375', dcase: '10614141002372',
    dtotoh: '846.00', dtotav: '846.00', dbulk: '791.00', dnlocs: 3, imgdata: '',
    locsfl: [
      { _rrn: 1, lseq: 1, lloc: 'D03101', ltype: 'BULK', lzone: 'D', lqty: '533.00', lalloc: '.00' },
      { _rrn: 2, lseq: 2, lloc: 'B06031', ltype: 'PICK', lzone: 'B', lqty: '55.00', lalloc: '.00' },
    ],
  };
  for (let i = 1; i <= 8; i++) {
    d['dimg' + i] = i <= 3 ? [419, 420, 519][i - 1] : 0;
    d['dcap' + i] = i <= 3 ? 'Photo ' + i : '';
    d['dsrc' + i] = i <= 3 ? (i === 3 ? 'P' : 'C') : '';
  }
  return d;
}

(async () => {
  console.log('=== the base button declares its own centring ===');
  {
    /* The structural half. A button only centres itself while it is block or
       inline-block, and show/hide rules in this project do change display. */
    const base = (themeCss.match(/^\.gt-btn \{[\s\S]*?\n\}/m) || [''])[0];
    const has = (p) => new RegExp(p).test(base);
    if (has('align-items:\\s*center') && has('justify-content:\\s*center')) {
      ok('.gt-btn centres its label in both axes', 'independent of display');
    } else {
      bad('.gt-btn relies on native button centring',
          'any rule that sets display: flex on it silently un-centres the label');
    }

    /* Which rules change a button's display at all -- named, so the next person
       knows this is a live hazard rather than a theoretical one. */
    const changers = (themeCss.match(/^[^\n{]*\.gt-(photo-retake|photo-choose)[^\n{]*\{[^}]*display:[^}]*\}/gm) || []);
    if (changers.length) {
      ok('  rules that do change a button display', changers.length + ' found, all now harmless');
    }
  }

  const browser = await chromium.launch({ executablePath: CHROME, args: ['--no-sandbox'] });

  console.log('\n=== the photo overlay, in every state it can be shown in ===');
  {
    const tpl = fs.readFileSync(path.join(UI, 'gtitdd/itmdetl.ejs'), 'utf8');
    const scr = fs.readFileSync(path.join(UI, 'gtitdd/itmdetl.css'), 'utf8');
    const html = ejs.render(tpl, itmdetlData());

    /* is-shot   -> a photo has been taken, so Retake appears
       is-fallback -> the live camera was refused, so Choose file appears
       both      -> the widest row, and the one nobody looks at */
    for (const state of ['is-open is-shot', 'is-open is-fallback', 'is-open is-shot is-fallback']) {
      for (const vp of [{ n: 'phone', w: 412, h: 915 }, { n: 'tablet', w: 834, h: 1194 }]) {
        const pg = await browser.newPage({ viewport: { width: vp.w, height: vp.h },
                                           isMobile: true, hasTouch: true });
        await pg.setContent('<!doctype html><meta charset="utf-8"><style>' + GENIE + '</style>' +
                            '<style>' + themeCss + '</style><style>' + scr + '</style>' + html,
                            { waitUntil: 'load' });
        await pg.evaluate((cls) => {
          document.getElementById('gt-photo-overlay').className = 'gt-photo-overlay ' + cls;
        }, state);

        const rows = await pg.evaluate(MEASURE);
        const off = rows.filter((r) => Math.abs(r.dx) > 2 || Math.abs(r.dy) > 2);
        const label = state.replace('is-open ', '') + ' @' + vp.n;
        if (!off.length) {
          ok(label, rows.length + ' buttons, every label centred');
        } else {
          bad(label, off.map((r) => r.id + ' dx=' + r.dx + ' dy=' + r.dy +
                                    ' (display:' + r.display + ')').join(' | '));
        }
        await pg.close();
      }
    }
  }

  /* The control. Put the old rule back and the same check must fail, or it is
     measuring nothing. */
  console.log('\n=== without the base centring, Retake must go off-centre again ===');
  {
    const tpl = fs.readFileSync(path.join(UI, 'gtitdd/itmdetl.ejs'), 'utf8');
    const scr = fs.readFileSync(path.join(UI, 'gtitdd/itmdetl.css'), 'utf8');
    const html = ejs.render(tpl, itmdetlData());
    const UNDO = '.gt-btn{display:block;align-items:normal;justify-content:normal;padding:0}' +
                 '.gt-photo-overlay.is-shot .gt-photo-retake{display:inline-flex}';

    const pg = await browser.newPage({ viewport: { width: 412, height: 915 },
                                       isMobile: true, hasTouch: true });
    await pg.setContent('<!doctype html><meta charset="utf-8"><style>' + GENIE + '</style>' +
                        '<style>' + themeCss + '</style><style>' + scr + '</style>' +
                        '<style>' + UNDO + '</style>' + html, { waitUntil: 'load' });
    await pg.evaluate(() => {
      document.getElementById('gt-photo-overlay').className = 'gt-photo-overlay is-open is-shot';
    });
    const rows = await pg.evaluate(MEASURE);
    const retake = rows.filter((r) => r.id === 'gt-photo-retake')[0];
    if (retake && (Math.abs(retake.dx) > 2 || Math.abs(retake.dy) > 2)) {
      ok('unfixed, Retake is off-centre', 'dx=' + retake.dx + ' dy=' + retake.dy +
         ' -- the reported symptom');
    } else {
      bad('unfixed, Retake is still centred',
          'then this test proves nothing about the fix: ' + JSON.stringify(retake));
    }
    await pg.close();
  }

  await browser.close();
  console.log('\n' + (fail
    ? fail + ' failed, ' + pass + ' passed'
    : 'Every button label is centred: ' + pass + ' passed, 0 failed'));
  process.exit(fail ? 1 : 0);
})();
