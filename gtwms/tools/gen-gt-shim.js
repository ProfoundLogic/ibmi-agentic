#!/usr/bin/env node
/*
 * GT Warehouse Mobile -- Genie EJS shim generator
 *
 *   node gtwms/tools/gen-gt-shim.js <path-to-start.html>          # write
 *   node gtwms/tools/gen-gt-shim.js <path-to-start.html> --check   # verify only
 *
 * WHY THIS EXISTS
 * ---------------
 * Profound UI fetches an EJS template over XHR from
 * /profoundui/userdata/ui/..., and in this environment that request does not
 * reach the browser -- the screen renders as a blank white page. start.html
 * demonstrably does reach the browser, so the template and its CSS are
 * base64-snapshotted into a script block there and served from a data: URI.
 *
 * Two independent interception points, because neither is reliable alone:
 *
 *   1. The screen-render hook. Its name changed between Profound UI builds --
 *      older genie.js exposes pui.vu, the build deployed here exposes pui.QD.
 *      Both names are patched if present, rather than assuming either.
 *   2. A patched XMLHttpRequest that answers from the same snapshot. This is
 *      the one that actually carries the load on the current build, and it
 *      chains onto whatever window.XMLHttpRequest already is, so it composes
 *      safely with the other projects' shim blocks in the same file.
 *
 * Screen JS is snapshotted the same way, because <script src="...userdata/ui/...">
 * does not reach the browser here either. Screens are still written to render
 * and be readable with NO JavaScript -- gt-scan only adds the camera on top of
 * a working manual-entry screen -- so a failure here degrades rather than
 * blanks. Screens that need no JS simply omit `js` and pay nothing.
 *
 * THE STALE-SNAPSHOT TRAP
 * -----------------------
 * The base64 below is a COPY of the template. Editing the .ejs and redeploying
 * it changes nothing on screen until this generator is re-run and start.html
 * redeployed. Always run with --check after editing a template.
 */

'use strict';

const fs = require('fs');
const path = require('path');

const UI = path.join(__dirname, '..', '..', 'htdocs', 'profoundui', 'userdata', 'ui');

/* Add a screen here as each application lands. `pattern` is matched against
 * the template URL as a substring, so it survives ?v=N cache-busting. */
const SCREENS = [
  {
    id: 'gtmnud-menu',
    pattern: 'gtmnud/menu.ejs',
    tpl: 'gtmnud/menu.ejs',
    css: ['gtcommon/gt-theme.css', 'gtmnud/menu.css'],
    /* No js: this screen renders and works with zero JavaScript. */
  },
  {
    id: 'gtitmd-itmscan',
    pattern: 'gtitmd/itmscan.ejs',
    tpl: 'gtitmd/itmscan.ejs',
    css: ['gtcommon/gt-theme.css', 'gtitmd/itmscan.css'],
    js: ['gtcommon/gt-scan.js'],
    root: '.gt-item-screen',
  },
  {
    id: 'gtitdd-itmdetl',
    pattern: 'gtitdd/itmdetl.ejs',
    tpl: 'gtitdd/itmdetl.ejs',
    css: ['gtcommon/gt-theme.css', 'gtitdd/itmdetl.css'],
    js: ['gtcommon/gt-carousel.js', 'gtcommon/gt-photo.js'],
    root: '.gt-itemdetl-screen',
  },
  {
    id: 'gtscnd-scnhome',
    pattern: 'gtscnd/scnhome.ejs',
    tpl: 'gtscnd/scnhome.ejs',
    css: ['gtcommon/gt-theme.css', 'gtscnd/scnhome.css'],
    js: ['gtcommon/gt-scan.js'],
    root: '.gt-scan-screen',
  },
  {
    id: 'gtrchd-rcvhome',
    pattern: 'gtrchd/rcvhome.ejs',
    tpl: 'gtrchd/rcvhome.ejs',
    css: ['gtcommon/gt-theme.css', 'gtrchd/rcvhome.css'],
    js: ['gtcommon/gt-scan.js'],
    root: '.gt-rcvhome-screen',
  },
  {
    id: 'gtrcld-rcvlines',
    pattern: 'gtrcld/rcvlines.ejs',
    tpl: 'gtrcld/rcvlines.ejs',
    css: ['gtcommon/gt-theme.css', 'gtrcld/rcvlines.css'],
    js: ['gtcommon/gt-scan.js', 'gtcommon/gt-edits.js', 'gtcommon/gt-rcvlines.js'],
    root: '.gt-rcvlines-screen',
  },
  {
    id: 'gtinhd-invhome',
    pattern: 'gtinhd/invhome.ejs',
    tpl: 'gtinhd/invhome.ejs',
    css: ['gtcommon/gt-theme.css', 'gtinhd/invhome.css'],
    js: ['gtcommon/gt-scan.js'],
    root: '.gt-invhome-screen',
  },
  {
    id: 'gtinld-invloc',
    pattern: 'gtinld/invloc.ejs',
    tpl: 'gtinld/invloc.ejs',
    css: ['gtcommon/gt-theme.css', 'gtinld/invloc.css'],
    js: ['gtcommon/gt-scan.js'],
    root: '.gt-invloc-screen',
  },
  {
    id: 'gtcnhd-cnthome',
    pattern: 'gtcnhd/cnthome.ejs',
    tpl: 'gtcnhd/cnthome.ejs',
    css: ['gtcommon/gt-theme.css', 'gtcnhd/cnthome.css'],
    js: ['gtcommon/gt-scan.js'],
    root: '.gt-cnthome-screen',
  },
  {
    id: 'gtcned-cntentry',
    pattern: 'gtcned/cntentry.ejs',
    tpl: 'gtcned/cntentry.ejs',
    css: ['gtcommon/gt-theme.css', 'gtcned/cntentry.css'],
    js: ['gtcommon/gt-scan.js', 'gtcommon/gt-edits.js', 'gtcommon/gt-cntentry.js'],
    root: '.gt-cntentry-screen',
  },
  {
    id: 'gtcnvd-cntvar',
    pattern: 'gtcnvd/cntvar.ejs',
    tpl: 'gtcnvd/cntvar.ejs',
    css: ['gtcommon/gt-theme.css', 'gtcnvd/cntvar.css'],
    js: ['gtcommon/gt-photo.js', 'gtcommon/gt-edits.js', 'gtcommon/gt-cntvar.js'],
    root: '.gt-cntvar-screen',
  },
  {
    id: 'gtimvd-invmove',
    pattern: 'gtimvd/invmove.ejs',
    tpl: 'gtimvd/invmove.ejs',
    css: ['gtcommon/gt-theme.css', 'gtimvd/invmove.css'],
    js: ['gtcommon/gt-scan.js', 'gtcommon/gt-invmove.js'],
    root: '.gt-invmove-screen',
  },
  {
    /* Supervisor View. No js at all -- it renders and works with zero
     * JavaScript, like the main menu. */
    id: 'gtsvd-supv',
    pattern: 'gtsvd/supv.ejs',
    tpl: 'gtsvd/supv.ejs',
    css: ['gtcommon/gt-theme.css', 'gtsvd/supv.css'],
    root: '.gt-supv-screen',
  },
  {
    id: 'gtpud-puthome',
    pattern: 'gtpud/puthome.ejs',
    tpl: 'gtpud/puthome.ejs',
    css: ['gtcommon/gt-theme.css', 'gtpud/puthome.css'],
    js: ['gtcommon/gt-scan.js'],
    root: '.gt-puthome-screen',
  },
  {
    id: 'gtpdd-putdest',
    pattern: 'gtpdd/putdest.ejs',
    tpl: 'gtpdd/putdest.ejs',
    css: ['gtcommon/gt-theme.css', 'gtpdd/putdest.css'],
    js: ['gtcommon/gt-scan.js'],
    root: '.gt-putdest-screen',
  },
  {
    /* Settings. No js either: every control is an inline pui.submit. */
    id: 'gtstd-settings',
    pattern: 'gtstd/settings.ejs',
    tpl: 'gtstd/settings.ejs',
    css: ['gtcommon/gt-theme.css', 'gtstd/settings.css'],
    root: '.gt-sett-screen',
  },
  {
    /* Simple camera test -- option 5 of the sign-on menu. Deliberately does
       NOT use gtcommon/gt-scan.js: it exists to isolate the iPad camera
       problem from the application's own scan stack, so it carries its own
       client side and shares nothing but the theme. */
    id: 'gtsimd-simtest',
    pattern: 'gtsimd/simtest.ejs',
    tpl: 'gtsimd/simtest.ejs',
    css: ['gtcommon/gt-theme.css', 'gtsimd/simtest.css'],
    js: ['gtsimd/simtest.js'],
    root: '.gts-screen',
  },
];

const BEGIN = '<!-- GTWMS EJS shim BEGIN (generated by gtwms/tools/gen-gt-shim.js - do not hand-edit) -->';
const END = '<!-- GTWMS EJS shim END -->';

function b64(file) {
  return fs.readFileSync(file).toString('base64');
}

function buildEntries() {
  return SCREENS.map((s) => {
    const tplPath = path.join(UI, s.tpl);
    if (!fs.existsSync(tplPath)) throw new Error('missing template: ' + tplPath);
    const cssText = s.css
      .map((c) => {
        const p = path.join(UI, c);
        if (!fs.existsSync(p)) throw new Error('missing css: ' + p);
        return fs.readFileSync(p, 'utf8');
      })
      .join('\n');
    /* Screen JS is snapshotted for the same reason as the template: a
     * <script src="...userdata/ui/..."> does not reach the browser here.
     * Screens that need no JS carry an empty string and are skipped. */
    const jsText = (s.js || [])
      .map((j) => {
        const p = path.join(UI, j);
        if (!fs.existsSync(p)) throw new Error('missing js: ' + p);
        return fs.readFileSync(p, 'utf8');
      })
      .join('\n;\n');

    return {
      pattern: s.pattern,
      styleId: 'gtwms-' + s.id + '-style',
      root: s.root || '',
      tplB64: b64(tplPath),
      cssB64: Buffer.from(cssText, 'utf8').toString('base64'),
      jsB64: jsText ? Buffer.from(jsText, 'utf8').toString('base64') : '',
    };
  });
}

function buildBlock(entries) {
  /* The decoder is read here rather than passed in: it is not per-screen, it is one
     global copy for the whole page. 362 KB becomes ~483 KB of base64, which is real
     but is the only delivery route proven to reach the device -- see the comment on
     ZXING_B64 in the generated block. */
  const zxingFile = path.join(UI, 'gtcommon', 'gt-zxing.min.js');
  if (!fs.existsSync(zxingFile)) {
    console.error('FATAL: ' + zxingFile + ' is missing -- iOS cannot decode without it');
    process.exit(1);
  }
  const zxingB64 = fs.readFileSync(zxingFile).toString('base64');

  const entryJs = entries
    .map(
      (e) =>
        '    { pattern: ' + JSON.stringify(e.pattern) +
        ', styleId: ' + JSON.stringify(e.styleId) +
        ', root: ' + JSON.stringify(e.root) +
        ', tplB64: "' + e.tplB64 + '"' +
        ', cssB64: "' + e.cssB64 + '"' +
        ', jsB64: "' + e.jsB64 + '" }'
    )
    .join(',\n');

  return `${BEGIN}
<script type="text/javascript">
/* GT Warehouse Mobile EJS screens (sign-on menu option 4).

   Independent of the other shim blocks in this file: it chains onto whatever
   window.pui.* and window.XMLHttpRequest already are at load time, so it
   composes safely regardless of script order.

   Regenerate with: node gtwms/tools/gen-gt-shim.js <this start.html>
   Never hand-edit the base64 below -- it is a snapshot of the real template. */
(function () {
  "use strict";

  var ENTRIES = [
${entryJs}
  ];

  /* ------------------------------------------------------------------
     THE DECODER, SHIPPED IN THE PAGE.

     gt-scan.js used to load ZXing by appending <script src=".../gt-zxing.min.js">.
     On the iPad that request WAS NEVER MADE -- the access log shows exactly one
     fetch of that file from the device all day, and it came from the standalone
     self-test page, never from inside Genie. Two other dynamically appended
     scripts (the version probe and the access-log ping) behaved identically:
     correct in Chromium, no request whatsoever from the device.

     On iOS there is no BarcodeDetector, so ZXing is the ONLY thing that can read a
     barcode. No decoder means a camera that opens, shows a live picture and never
     resolves anything -- which is precisely what was reported, seven times.

     I cannot see why WebKit-inside-Genie drops those requests, and I do not need
     to: the base64 route in this shim demonstrably reaches the device, because the
     screen JS it carries is running there. So the decoder travels the same way.

     Decoded LAZILY -- 362 KB of parser is not worth spending on a 5250 screen that
     will never scan -- and gt-scan.js calls this instead of appending a script.
     ------------------------------------------------------------------ */
  var ZXING_B64 = "${zxingB64}";

  window.__gtLoadZxing = function () {
    if (window.ZXing && window.ZXing.BrowserMultiFormatReader) return true;
    try {
      (new Function(b64decode(ZXING_B64)))();
    } catch (err) {
      if (window.console) console.error("[GTWMS] inlined decoder failed:", err);
    }
    return !!(window.ZXing && window.ZXing.BrowserMultiFormatReader);
  };

  /* The screen-render entry point was renamed between Profound UI builds:
     older genie.js exposes pui.vu, this build exposes pui.QD. Patch whichever
     candidates exist rather than assuming one name. */
  var HOOKS = ["QD", "vu"];
  var MAX_TRIES = 200;

  function b64decode(s) {
    try {
      var bin = atob(s);
      var bytes = new Uint8Array(bin.length);
      for (var i = 0; i < bin.length; i++) bytes[i] = bin.charCodeAt(i);
      return new TextDecoder("utf-8").decode(bytes);
    } catch (e) { return ""; }
  }

  for (var e = 0; e < ENTRIES.length; e++) {
    ENTRIES[e]._tplCache = null;
    ENTRIES[e]._dataUrl = "data:text/html;charset=utf-8;base64," + ENTRIES[e].tplB64;
  }

  function findEntry(u) {
    if (!u || typeof u !== "string") return null;
    var i;
    /* Match on the path suffix, not the whole URL, so ?v=N cache-busting
       does not defeat it. */
    if (u.indexOf("data:") !== 0) {
      for (i = 0; i < ENTRIES.length; i++) {
        if (u.indexOf(ENTRIES[i].pattern) !== -1) return ENTRIES[i];
      }
      return null;
    }
    /* Also claim the data: URIs this shim itself produces, so the template is
       served from the cache above rather than relying on the browser's
       XHR-over-data:-URI behaviour. */
    for (i = 0; i < ENTRIES.length; i++) {
      if (u === ENTRIES[i]._dataUrl) return ENTRIES[i];
    }
    return null;
  }

  function decodeTpl(entry) {
    if (entry._tplCache === null) entry._tplCache = b64decode(entry.tplB64);
    return entry._tplCache;
  }

  /* ------------------------------------------------------------------
     Viewport.

     The pls skin ships a viewport meta; the Classic skin ships NONE, so a
     phone falls back to a ~980px virtual viewport and zooms in every time an
     input takes focus. Add one only when it is missing -- never touch a skin
     that already declares its own, and never add user-scalable=0, which
     removes pinch-zoom for everyone.
     ------------------------------------------------------------------ */
  function ensureViewport() {
    if (document.querySelector('meta[name="viewport"]')) return;
    var m = document.createElement('meta');
    m.name = 'viewport';
    m.content = 'width=device-width, initial-scale=1';
    (document.head || document.documentElement).appendChild(m);
    if (window.console) console.log('[GTWMS] added missing viewport meta');
  }
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', ensureViewport);
  } else {
    ensureViewport();
  }

  /* The CSS <link> may or may not be blocked depending on build; inlining
     costs nothing. Every rule in these files is scoped to .gt-app, so a
     permanent <style> tag cannot leak into the surrounding Genie skin. */
  function injectCss() {
    for (var i = 0; i < ENTRIES.length; i++) {
      var entry = ENTRIES[i];
      var existing = document.getElementById(entry.styleId);
      var freshText = b64decode(entry.cssB64);
      if (existing) {
        if (existing.textContent !== freshText) existing.textContent = freshText;
        continue;
      }
      var s = document.createElement("style");
      s.id = entry.styleId;
      s.textContent = freshText;
      (document.head || document.documentElement).appendChild(s);
    }
  }
  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", injectCss);
  } else {
    injectCss();
  }
  var gtCssInterval = setInterval(injectCss, 500);
  setTimeout(function () { clearInterval(gtCssInterval); }, 10000);

  /* ------------------------------------------------------------------
     THE VIEWPORT. Without this every screen is laid out at 980px and then
     scaled down to fit the phone.

     Genie's start.html declares no <meta name="viewport">, and a mobile browser
     with no viewport meta uses a ~980px LAYOUT viewport and zooms the finished
     page out to fit the screen. Measured on a Samsung S24 Ultra profile:

       no meta   layout = 980px  ->  the >=835px desktop layout, scaled to ~42%
       with meta layout = 412px  ->  the phone layout, at full size

     So every media query was reading a desktop width and every screen was a
     shrunken desktop. Nothing was wrong with the CSS; the page was never told
     the device existed. This is invisible to a headless test that injects its
     own viewport meta -- which is exactly what the pre-flights did.

     SCOPED, NOT GLOBAL. start.html is shared with the other projects in this
     skin and with Genie's own 5250 screens, where a 980px layout scaled to fit
     is arguably the right answer for a fixed 80-column terminal. So the meta is
     applied only while one of OUR screens is on the page, and the previous
     state -- including its absence -- is restored when it leaves.
     ------------------------------------------------------------------ */
  var GT_VIEWPORT = "width=device-width, initial-scale=1, viewport-fit=cover";
  var vpPrev = null;          /* previous content, or null when there was none */
  var vpOurs = false;         /* did we create the tag ourselves? */
  var vpOn = false;

  function viewportTag() {
    return document.querySelector('meta[name="viewport"]');
  }

  function applyViewport() {
    if (vpOn) return;
    var tag = viewportTag();
    if (tag) {
      vpPrev = tag.getAttribute("content");
      vpOurs = false;
    } else {
      tag = document.createElement("meta");
      tag.setAttribute("name", "viewport");
      (document.head || document.documentElement).appendChild(tag);
      vpPrev = null;
      vpOurs = true;
    }
    tag.setAttribute("content", GT_VIEWPORT);
    vpOn = true;
  }

  function restoreViewport() {
    if (!vpOn) return;
    var tag = viewportTag();
    if (tag) {
      if (vpOurs) {
        tag.parentNode.removeChild(tag);
      } else if (vpPrev === null) {
        tag.removeAttribute("content");
      } else {
        tag.setAttribute("content", vpPrev);
      }
    }
    vpOn = false;
  }

  /* Driven by whether a .gt-app wrapper is actually in the DOM rather than by a
     render hook: the hook tells us when one of our screens ARRIVES, and nothing
     tells us when it leaves. Polling the DOM covers both directions and cannot
     be wrong about the current state. */
  function syncViewport() {
    if (document.querySelector(".gt-app")) applyViewport();
    else restoreViewport();
  }
  syncViewport();
  setInterval(syncViewport, 300);

  /* ------------------------------------------------------------------
     Screen JS delivery. Each screen's JS guards its own re-entry, so if the
     normal script element DID load, running it again is a no-op.
     ------------------------------------------------------------------ */
  function runScreenJs(entry) {
    if (!entry || !entry.jsB64 || !entry.root) return true;
    var el = document.querySelector(entry.root);
    if (!el) return false;
    try {
      /* Mark the delivery route before running it. The snapshot below and the
         display file's <script src> tag both deliver the same file, and knowing
         WHICH one is running has been the difference between a real fix and a
         wasted round: a cached start.html pins the snapshot, and no stamp bump can
         reach it. gt-scan.js reports this in its access-log ping. */
      window.__gtVia = 'snapshot';
      (new Function(b64decode(entry.jsB64)))();
    } catch (err) {
      if (window.console) console.error("[GTWMS] screen JS failed:", err);
    }
    return true;
  }

  function awaitScreen(entry, attempt) {
    if (runScreenJs(entry) || attempt > 80) return;
    setTimeout(function () { awaitScreen(entry, attempt + 1); }, 50);
  }

  function wrapHook(name) {
    var flag = "__gtwmsPatched_" + name;
    if (window.pui[flag]) return true;
    var orig = window.pui[name];
    window.pui[name] = function (w) {
      var matched = null;
      try {
        if (w && w.metaData && w.metaData.screen) {
          matched = findEntry(w.metaData.screen["ejs template"]);
          if (matched) { w.metaData.screen["ejs template"] = matched._dataUrl; }
        }
      } catch (err) {}
      var out = orig.apply(this, arguments);
      try { if (matched) awaitScreen(matched, 0); } catch (err2) {}
      return out;
    };
    window.pui[flag] = true;
    return true;
  }

  var tries = 0;
  function patchHooks() {
    tries++;
    var patched = 0;
    if (typeof window.pui !== "undefined") {
      for (var i = 0; i < HOOKS.length; i++) {
        if (typeof window.pui[HOOKS[i]] === "function" && wrapHook(HOOKS[i])) patched++;
      }
    }
    /* Keep looking until at least one hook is patched, but never poll forever:
       on a build where none of the candidates exist, the patched XHR below is
       enough on its own. */
    if (patched === 0 && tries < MAX_TRIES) setTimeout(patchHooks, 50);
  }
  patchHooks();

  /* ------------------------------------------------------------------
     Patched XMLHttpRequest. This is the interception that actually carries
     the load: it answers any request for one of our templates from the
     snapshot above, whatever code path issued it.
     ------------------------------------------------------------------ */
  var GtOriginalXHR = window.XMLHttpRequest;

  function GtFakeXHR() {
    this.readyState = 0; this.status = 0; this.statusText = ""; this.responseText = "";
    this.response = ""; this.responseURL = ""; this.onreadystatechange = null;
    this.onload = null; this._url = ""; this._entry = null;
  }
  GtFakeXHR.prototype.open = function (m, url) { this._url = url; this.readyState = 1; };
  GtFakeXHR.prototype.setRequestHeader = function () {};
  GtFakeXHR.prototype.getResponseHeader = function (n) {
    return n && String(n).toLowerCase() === "content-type" ? "text/html; charset=utf-8" : null;
  };
  GtFakeXHR.prototype.getAllResponseHeaders = function () {
    return "content-type: text/html; charset=utf-8\\r\\n";
  };
  GtFakeXHR.prototype.abort = function () { this.readyState = 0; };
  GtFakeXHR.prototype.send = function () {
    var self = this;
    setTimeout(function () {
      var content = self._entry ? decodeTpl(self._entry) : "";
      for (var rs = 2; rs <= 4; rs++) {
        self.readyState = rs; self.status = 200; self.statusText = "OK";
        self.responseURL = self._url;
        if (rs === 4) { self.responseText = content; self.response = content; }
        if (typeof self.onreadystatechange === "function") {
          try { self.onreadystatechange(); } catch (_) {}
        }
      }
      if (typeof self.onload === "function") { try { self.onload(); } catch (_) {} }
      /* The XHR path is what actually serves the template on this build, so
         screen JS has to be armed from here too -- the render hook may not
         exist under the deployed genie.js. */
      try { if (self._entry) awaitScreen(self._entry, 0); } catch (_) {}
    }, 0);
  };

  function GtPatchedXHR() {
    var realXhr = new GtOriginalXHR();
    var fake = null;
    var proxy = {};
    function targetFor() { return fake || realXhr; }
    function defineForward(name) {
      Object.defineProperty(proxy, name, {
        get: function () { return targetFor()[name]; },
        set: function (v) { targetFor()[name] = v; },
        configurable: true, enumerable: true
      });
    }
    var props = ["readyState","status","statusText","responseText","response","responseURL",
      "responseType","timeout","withCredentials","onreadystatechange","onload","onerror",
      "onabort","ontimeout","onprogress","onloadstart","onloadend","upload"];
    for (var i = 0; i < props.length; i++) defineForward(props[i]);
    proxy.open = function (m, url, a, u, p) {
      var entry = findEntry(url);
      if (entry) { fake = new GtFakeXHR(); fake._entry = entry; fake.open(m, url, a, u, p); return; }
      return realXhr.open(m, url, a, u, p);
    };
    proxy.send = function (b) { return targetFor().send(b); };
    proxy.abort = function () { return targetFor().abort(); };
    proxy.setRequestHeader = function (n, v) { return targetFor().setRequestHeader(n, v); };
    proxy.getResponseHeader = function (n) { return targetFor().getResponseHeader(n); };
    proxy.getAllResponseHeaders = function () { return targetFor().getAllResponseHeaders(); };
    proxy.overrideMimeType = function (m) {
      if (realXhr.overrideMimeType) return realXhr.overrideMimeType(m);
    };
    return proxy;
  }
  GtPatchedXHR.UNSENT = 0; GtPatchedXHR.OPENED = 1; GtPatchedXHR.HEADERS_RECEIVED = 2;
  GtPatchedXHR.LOADING = 3; GtPatchedXHR.DONE = 4;
  GtPatchedXHR.prototype = GtOriginalXHR.prototype;
  window.XMLHttpRequest = GtPatchedXHR;

  try {
    /* Exposed so gtwms/tools/test-gt-shim.js can verify the snapshots from
       outside without re-parsing start.html. */
    window.__gtwmsEntries = ENTRIES;
    window.__gtwmsEjsShimLoaded = true;
    if (window.console) console.log("[GTWMS EJS shim armed: " + ENTRIES.length + " screen(s)]");
  } catch (_) {}
})();
</script>
${END}`;
}

/* ------------------------------------------------------------------ */
/* Exported before the CLI runs, and the CLI is guarded, so the test suite can
 * require this file for its SCREENS list without executing the generator. */
module.exports = { SCREENS };

if (require.main !== module) return;

const target = process.argv[2];
const checkOnly = process.argv.includes('--check');

if (!target) {
  console.error('usage: gen-gt-shim.js <path-to-start.html> [--check]');
  process.exit(2);
}

const entries = buildEntries();
const block = buildBlock(entries);
let html = fs.readFileSync(target, 'utf8');

/* Remove any previous GTWMS block so the generator is idempotent. */
const bi = html.indexOf(BEGIN);
const ei = html.indexOf(END);
const hadBlock = bi !== -1 && ei !== -1;
let existing = null;
if (hadBlock) {
  existing = html.slice(bi, ei + END.length);
  html = html.slice(0, bi) + html.slice(ei + END.length);
  html = html.replace(/\n{3,}/g, '\n\n');
}

if (checkOnly) {
  if (!hadBlock) {
    console.error('STALE: no GTWMS shim block present in ' + target);
    process.exit(1);
  }
  if (existing.trim() !== block.trim()) {
    console.error('STALE: the shim snapshot does not match the current templates/CSS.');
    console.error('       Re-run without --check, then redeploy start.html.');
    process.exit(1);
  }
  console.log('shim is current (' + entries.length + ' screen(s))');
  process.exit(0);
}

/* Insert immediately before genie.js loads. */
const anchor = html.search(/<script[^>]+genie\.js/i);
if (anchor === -1) throw new Error('could not find the genie.js script tag in ' + target);

html = html.slice(0, anchor) + block + '\n' + html.slice(anchor);
fs.writeFileSync(target, html, 'utf8');

console.log((hadBlock ? 'Replaced' : 'Inserted') + ' GTWMS shim in ' + target);
entries.forEach((e) => {
  console.log('  ' + e.pattern.padEnd(28) +
              ' tpl ' + String(Math.round(e.tplB64.length / 1024)).padStart(3) + 'KB' +
              '  css ' + String(Math.round(e.cssB64.length / 1024)).padStart(3) + 'KB' +
              '  js ' + String(Math.round(e.jsB64.length / 1024)).padStart(3) + 'KB');
});