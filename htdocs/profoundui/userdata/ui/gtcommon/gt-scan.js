/* ==================================================================
 * gt-scan -- the universal scan input.
 *
 * Shared by every screen in the application that accepts a barcode. Three
 * input paths, in preference order:
 *
 *   1. Camera via BarcodeDetector. Native in Chrome on Android, which is
 *      the live demo device, so decoding happens in the browser engine --
 *      fast, no library, low battery cost.
 *   2. Camera via ZXing-js, lazily loaded, for browsers with no
 *      BarcodeDetector -- which means SAFARI, so every iPhone and iPad.
 *   3. Manual entry, which is plain markup in the template and needs no
 *      JavaScript at all.
 *
 * BarcodeDetector is a Chromium API and WebKit does not implement it, so on iOS
 * the camera opened, showed a live picture and then said "this browser cannot
 * decode in-camera" -- honest, and useless to somebody holding an iPad. Path 2
 * closes that: the decoder is pure JavaScript (no WebAssembly, because the Genie
 * proxy drops XHR to userdata/* and a .wasm could not be fetched, while a
 * <script src> to the same path is served normally), it attaches to the <video>
 * element this file is already streaming into, and it is loaded ONLY on the
 * browsers that need it.
 *
 * DESIGN CONSTRAINTS worth knowing before editing:
 *   - This file runs when the shim injects it, which may be BEFORE or AFTER
 *     the template markup exists. Everything is therefore driven from an
 *     init() that polls for its root element, and guarded so re-entry is a
 *     no-op. Screen JS re-executes on every render, including subfile
 *     reloads.
 *   - Listeners go on elements inside the screen container, never on
 *     document or window, because the container is wiped on re-render and
 *     anything attached higher up would stack.
 *   - The screen must remain fully usable with this file absent. Nothing
 *     here is required to read the screen.
 * ================================================================== */

(function () {
  'use strict';

  /* NEWER WINS, and this is not paranoia -- it is the bug that made the last
     three camera fixes invisible on a real device.
     
     This file arrives by TWO routes at once. The display file lists it in its
     "js" array, so the browser loads it as a <script src=...?v=stamp> tag; and
     the Genie shim also injects a base64 snapshot of it, which is always
     current. Whichever runs FIRST installs window.gtScan, and the old guard made
     the second one a no-op -- so a MONTHS-OLD CACHED TAG beat the fresh
     snapshot, and every fix looked deployed while the device ran August's code.
     Two of the display files had no ?v= stamp at all, so their copy could never
     be evicted.
     
     Comparing versions fixes it from the inside: a stale copy that arrives after
     a newer one steps aside, and a newer copy replaces an older one. It is a
     string compare on a YYYYMMDDx stamp, which orders correctly. */
  var VERSION = '20260811k';
  var prev = window.gtScan;
  if (prev && prev.__installed) {
    if (prev.__version && String(prev.__version) >= VERSION) {
      prev.rebind();          /* same age or newer already in charge */
      return;
    }
    /* We are newer. Shut the old one down before taking over, or its frame loop
       and listeners keep running against a camera we no longer own.

       And then UNWIRE it. bind() marks the band with data-gt-scan-bound and
       returns early when it sees the mark, so a newer copy would install itself,
       find the band already marked, attach nothing -- and the Start button would
       still be calling the OLD startCamera. Installed but not listening: the
       upgrade would look like it worked and change nothing.

       cloneNode drops event listeners, which is the only way to detach handlers
       registered by a copy of this file we no longer have a reference to. Without
       it, one click would open TWO camera streams. */
    try { if (prev.stop) prev.stop(); } catch (e) {}
    try {
      var oldBand = document.querySelector('#gt-scan-camera');
      if (oldBand) {
        oldBand.removeAttribute('data-gt-scan-bound');
        /* One container holds every control this file created, so one removal
           cannot leave a stale fragment behind. */
        var oldExtra = document.querySelector('#gt-scan-extra');
        if (oldExtra && oldExtra.parentNode) oldExtra.parentNode.removeChild(oldExtra);
        var ids = ['#gt-scan-start', '#gt-scan-stop', '#gt-scan-manual'];
        for (var ci = 0; ci < ids.length; ci++) {
          var ctl = document.querySelector(ids[ci]);
          if (ctl && ctl.parentNode) {
            var fresh = ctl.cloneNode(true);
            if ('value' in ctl) fresh.value = ctl.value;
            ctl.parentNode.replaceChild(fresh, ctl);
          }
        }
      }
    } catch (e) {}
  }

  var state = { stream: null, detector: null, raf: 0, running: false, lastCode: '', lastAt: 0,
                canvas: null, ctx: null, reader: null, hints: null, tickN: 0,
                zxTimer: 0, hintTimer: 0, photoInput: null };

  /* Cap the analysed frame. A JS decoder walks every pixel row, so 1920 wide
     costs real time and buys nothing once each bar is a few pixels across. */
  var MAX_EDGE = 1024;

  /* Everything a device can tell us about why a scan is not happening.
     Exposed as gtScan.diag() and read by gt-camtest.html, because four rounds of
     guessing from a description beat one screenshot of facts. */
  var diag = {
    version: VERSION, path: 'none', reason: '', videoW: 0, videoH: 0,
    detectorPresent: false, detectorFormats: null,
    nativeTries: 0, nativeErrs: 0, zxTries: 0, zxLoaded: false,
    lastErr: '', lastCode: '',
    cameraLabel: '', focus: '', frameW: 0, frameH: 0, photoTries: 0, photoW: 0,
    serverStamp: '', fresher: '', zxVia: ''
  };

  function $(sel, root) { return (root || document).querySelector(sel); }

  function supportsDetector() {
    return typeof window.BarcodeDetector !== 'undefined';
  }

  /* The formats we care about, filtered to what this browser admits to
   * supporting -- passing an unsupported format to the constructor throws. */
  function wantedFormats() {
    return ['upc_a', 'upc_e', 'ean_13', 'ean_8', 'itf', 'code_128',
            'code_39', 'codabar', 'qr_code', 'data_matrix', 'pdf417', 'aztec'];
  }

  function setStatus(msg, kind) {
    var el = $('#gt-scan-status');
    if (!el) return;
    el.textContent = msg || '';
    el.className = 'gt-scan-status' + (kind ? ' is-' + kind : '');
  }

  /* Submit is the same whichever path produced the payload. */
  function submitPayload(code) {
    if (!code) return;

    /* Dedupe: continuous decode fires many times for one physical barcode. */
    var now = Date.now();
    if (code === state.lastCode && now - state.lastAt < 2500) return;
    state.lastCode = code;
    state.lastAt = now;
    diag.lastCode = code;

    ping('hit');
    feedback();
    stopCamera();

    /* Mirror into the visible field so the operator sees what was decoded
     * even in the instant before the round trip repaints. There is exactly
     * one element named scanval -- a hidden duplicate would leave which
     * value reaches RPG undefined. */
    var input = $('#gt-scan-manual');
    if (input) input.value = code;

    if (window.pui && typeof pui.submit === 'function') {
      pui.submit({ action: 'SCAN', scanval: code });
    }
  }

  /* A warehouse is loud, bright, and the device may be in a holster, so a
   * good scan is signalled three ways at once. */
  function feedback() {
    try { if (navigator.vibrate) navigator.vibrate(60); } catch (e) {}
    try {
      var Ctx = window.AudioContext || window.webkitAudioContext;
      if (!Ctx) return;
      var ctx = new Ctx();
      var osc = ctx.createOscillator();
      var gain = ctx.createGain();
      osc.frequency.value = 1180;
      gain.gain.value = 0.06;
      osc.connect(gain); gain.connect(ctx.destination);
      osc.start();
      setTimeout(function () { osc.stop(); ctx.close(); }, 110);
    } catch (e) {}
    var frame = $('#gt-scan-frame');
    if (frame) {
      frame.classList.add('is-hit');
      setTimeout(function () { frame.classList.remove('is-hit'); }, 400);
    }
  }

  function stopCamera() {
    state.running = false;
    if (state.raf) { cancelAnimationFrame(state.raf); state.raf = 0; }
    if (state.zxTimer) { clearTimeout(state.zxTimer); state.zxTimer = 0; }
    if (state.hintTimer) { clearTimeout(state.hintTimer); state.hintTimer = 0; }
    if (state.zxing) {
      /* We own the loop now, so clearing the timer above is what actually stops
         decoding. reset() is still called to release the library's capture
         canvas, and is harmless once our own stream teardown follows. */
      try { state.zxing.reset(); } catch (e) {}
      state.zxing = null;
    }
    if (state.stream) {
      state.stream.getTracks().forEach(function (t) { try { t.stop(); } catch (e) {} });
      state.stream = null;
    }
    var wrap = $('#gt-scan-camera');
    if (wrap) wrap.classList.remove('is-live');
  }

  /* Poll rather than trust one event: 'loadedmetadata' has already fired on some
     browsers by the time we attach, and readyState alone can lie about
     dimensions. ~6 seconds is generous; a camera that has not produced a frame
     by then is not going to. */
  function whenVideoReady(video, cb, tries) {
    tries = tries || 0;
    if (video.videoWidth > 0 && video.videoHeight > 0 && video.readyState >= 2) {
      return cb(true);
    }
    if (tries > 60) return cb(false);
    setTimeout(function () { whenVideoReady(video, cb, tries + 1); }, 100);
  }

  function startCamera() {
    var video = $('#gt-scan-video');
    var wrap = $('#gt-scan-camera');
    if (!video || !wrap) return;

    if (!navigator.mediaDevices || !navigator.mediaDevices.getUserMedia) {
      /* Almost always this means the page is not on HTTPS -- every modern
       * browser refuses getUserMedia on an insecure origin. */
      setStatus(txt('noCamera'), 'bad');
      return;
    }

    setStatus(txt('starting'), '');

    navigator.mediaDevices.getUserMedia({
      /* 1920 rather than 1280: a barcode occupies a small part of the frame and a
         1D symbol needs a couple of pixels PER BAR. The browser hands back the
         nearest mode it has, so this costs nothing where it is unavailable. */
      video: { facingMode: { ideal: 'environment' }, width: { ideal: 1920 } },
      audio: false
    }).then(function (stream) {
      state.stream = stream;

      /* TRY to ask for continuous autofocus. A getUserMedia stream is NOT the
         native camera app: on iOS it commonly stays at a fixed focus, so a
         barcode held at reading distance is a blurry mess that no decoder on
         earth can read -- a live picture that never resolves. Chrome on Android
         honours this; WebKit ignores it, which is why the photo path below
         exists. Wrapped because a rejected constraint must not stop the camera. */
      try {
        var track = stream.getVideoTracks()[0];
        if (track) {
          diag.cameraLabel = track.label || '';
          if (track.applyConstraints) {
            track.applyConstraints({ advanced: [{ focusMode: 'continuous' }] })
              .then(function () { diag.focus = 'continuous requested'; })
              .catch(function () { diag.focus = 'not adjustable'; });
          }
        }
      } catch (e) { diag.focus = 'not adjustable'; }

      video.srcObject = stream;
      video.setAttribute('playsinline', 'true');  /* iOS refuses fullscreen-less play without it */
      return video.play();
    }).then(function () {
      wrap.classList.add('is-live');

      /* WAIT FOR A PICTURE BEFORE DECODING ANYTHING.
       *
       * video.play() resolving does NOT mean the video has dimensions yet --
       * metadata can arrive later. Start decoding then and BarcodeDetector gets a
       * 0x0 frame: it rejects (or returns nothing) for ever, ZXing does the same,
       * and the operator sees a live picture that never reads a barcode. That is
       * "the camera opens but will not scan", and it is invisible from a desk
       * because a fake device stream has dimensions immediately. */
      whenVideoReady(video, function (hasPicture) {
        diag.videoW = video.videoWidth || 0;
        diag.videoH = video.videoHeight || 0;
        if (!hasPicture) {
          diag.reason = 'video never reported dimensions';
          setStatus(txt('noFrames'), 'bad');
          return;
        }
        beginDetect(video);
      });
    }).catch(function (err) {
      setStatus(txt('cameraFailed') + ' (' + (err && err.name ? err.name : 'error') + ')', 'bad');
      stopCamera();
    });
  }

  /* ------------------------------------------------------------------
     THE SAFARI PATH.

     BarcodeDetector is a Chromium API. Safari -- so every iPhone and iPad, and
     every browser on iOS, since they are all WebKit -- does not implement it at
     all. getUserMedia works there, so the camera opened, showed a live picture,
     and then said "this browser cannot decode in-camera": honest, and useless to
     somebody holding an iPad in a warehouse.

     So when BarcodeDetector is missing, a pure-JavaScript decoder is loaded and
     given the SAME <video> element this file is already streaming into. ZXing-js
     is 362 KB of plain JS with no WebAssembly, which matters twice over: the
     Genie proxy drops XHR to userdata/* (a .wasm could not be fetched), while a
     <script src> tag to the same path is served normally.

     It is loaded LAZILY and only on the browsers that need it, so Android pays
     nothing for it.
     ------------------------------------------------------------------ */
  var ZXING_SRC = '/profoundui/userdata/ui/gtcommon/gt-zxing.min.js';
  var zxingWaiters = [];

  function loadZxing(cb) {
    if (window.ZXing && window.ZXing.BrowserMultiFormatReader) return cb(true);

    /* THE INLINED COPY FIRST, and this is not an optimisation.

       On the iPad, appending <script src=".../gt-zxing.min.js"> produced NO NETWORK
       REQUEST AT ALL from inside Genie -- the access log shows one fetch of that
       file from the device all day, and it came from the standalone self-test page.
       Two other dynamically appended scripts behaved the same way. Since iOS has no
       BarcodeDetector, no decoder means a camera that opens and can never read
       anything: the symptom reported seven times.

       The shim carries a base64 copy in the page, and that route demonstrably
       reaches the device because the screen JS it delivers is running there. The
       script tag below is kept as the fallback for skins that have no shim. */
    if (typeof window.__gtLoadZxing === 'function') {
      try {
        if (window.__gtLoadZxing()) {
          diag.zxVia = 'inlined';
          return cb(true);
        }
      } catch (e) { diag.lastErr = 'inlined decoder: ' + ((e && e.name) || '?'); }
    }

    diag.zxVia = 'fetch';
    zxingWaiters.push(cb);
    if (state.zxingLoading) return;
    state.zxingLoading = true;

    var done = function (ok) {
      state.zxingLoading = false;
      var list = zxingWaiters.splice(0);
      for (var i = 0; i < list.length; i++) list[i](ok);
    };

    var el = document.createElement('script');
    el.src = ZXING_SRC;
    el.async = true;
    el.onload = function () {
      done(!!(window.ZXing && window.ZXing.BrowserMultiFormatReader));
    };
    el.onerror = function () { done(false); };
    (document.head || document.documentElement).appendChild(el);
  }

  function zxingFormats(Z) {
    var want = ['UPC_A', 'UPC_E', 'EAN_13', 'EAN_8', 'ITF', 'CODE_128',
                'CODE_39', 'CODE_93', 'CODABAR', 'QR_CODE', 'DATA_MATRIX',
                'PDF_417', 'AZTEC'];
    var out = [];
    for (var i = 0; i < want.length; i++) {
      if (Z.BarcodeFormat[want[i]] !== undefined) out.push(Z.BarcodeFormat[want[i]]);
    }
    return out;
  }

  /* ------------------------------------------------------------------
     WE DRAW THE PIXELS OURSELVES.

     Passing the <video> element to the library and letting it manage its own
     capture canvas works on a desk and gives up too much control on a device:

       - its canvas is created ONCE, sized from videoWidth at that instant, and
         reused for ever -- an orientation change leaves it the wrong shape;
       - it always analyses the WHOLE frame. A barcode occupying a fifth of a
         1080-line picture is a handful of pixels per bar by the time the
         binarizer sees it;
       - any failure inside it surfaces as an opaque exception per frame.

     So: draw the region under the reticle into our own canvas at a known size,
     build the luminance source from that, and run the binarizers ourselves. Every
     class used here is one the library itself uses -- HTMLCanvasElementLuminanceSource
     is what its own video path constructs.
     ------------------------------------------------------------------ */
  function ensureCanvas() {
    if (!state.canvas) {
      state.canvas = document.createElement('canvas');
      /* willReadFrequently keeps the surface on the CPU; without it every
         getImageData round-trips from the GPU and the loop crawls. Older WebKit
         throws on the options object rather than ignoring it. */
      try { state.ctx = state.canvas.getContext('2d', { willReadFrequently: true }); }
      catch (e) { state.ctx = state.canvas.getContext('2d'); }
    }
    return state.ctx;
  }

  /* The reticle band, matching what the operator is aiming with. Cropping to it
     roughly triples the pixels per bar at the same cost. */
  function frameRect(w, h, tight) {
    if (!tight) return { x: 0, y: 0, w: w, h: h };
    var cw = Math.round(w * 0.94), ch = Math.round(h * 0.45);
    return { x: Math.round((w - cw) / 2), y: Math.round((h - ch) / 2), w: cw, h: ch };
  }

  /* Draw `src` (a video or an image) into our canvas, cropped and size-capped. */
  function grabFrame(src, sw, sh, tight) {
    var ctx = ensureCanvas();
    if (!ctx) return null;
    var r = frameRect(sw, sh, tight);
    var scale = Math.min(1, MAX_EDGE / Math.max(r.w, r.h));
    var dw = Math.max(1, Math.round(r.w * scale));
    var dh = Math.max(1, Math.round(r.h * scale));
    if (state.canvas.width !== dw || state.canvas.height !== dh) {
      state.canvas.width = dw;
      state.canvas.height = dh;
    }
    try { ctx.drawImage(src, r.x, r.y, r.w, r.h, 0, 0, dw, dh); }
    catch (e) { diag.lastErr = 'drawImage:' + ((e && e.name) || '?'); return null; }
    diag.frameW = dw; diag.frameH = dh;
    return state.canvas;
  }

  function zxReader() {
    var Z = window.ZXing;
    if (!state.reader) {
      state.hints = new Map();
      state.hints.set(Z.DecodeHintType.POSSIBLE_FORMATS, zxingFormats(Z));
      state.hints.set(Z.DecodeHintType.TRY_HARDER, true);
      state.reader = new Z.MultiFormatReader();
      if (state.reader.setHints) state.reader.setHints(state.hints);
    }
    return state.reader;
  }

  /* Three attempts on one frame, cheapest first. They fail differently:
       HybridBinarizer     -- local thresholding; right for an evenly lit photo
       GlobalHistogram     -- one threshold; wins on flat, high-contrast frames
       inverted            -- a light-on-dark label, or a screen photographed
     A read on the third attempt is still a read, and all three together are far
     cheaper than the 120ms we wait between frames. */
  function decodeCanvas(canvas) {
    var Z = window.ZXing;
    var reader = zxReader();
    var lum;
    try { lum = new Z.HTMLCanvasElementLuminanceSource(canvas); }
    catch (e) { diag.lastErr = 'luminance:' + ((e && e.name) || '?'); return null; }

    var attempts = [
      function () { return new Z.BinaryBitmap(new Z.HybridBinarizer(lum)); },
      function () { return new Z.BinaryBitmap(new Z.GlobalHistogramBinarizer(lum)); },
      function () { return new Z.BinaryBitmap(new Z.HybridBinarizer(new Z.InvertedLuminanceSource(lum))); }
    ];

    for (var i = 0; i < attempts.length; i++) {
      try {
        reader.reset();
        var res = reader.decode(attempts[i](), state.hints);
        if (res) {
          var text = null;
          try { text = res.getText(); } catch (e) { text = res.text; }
          if (text) return text;
        }
      } catch (e) {
        /* NotFoundException on a frame with no barcode is the normal case and is
           NOT worth recording. Minified builds rename the class, so match the
           message shape rather than the identity. */
        var n = (e && (e.name || (e.constructor && e.constructor.name))) || '';
        var m = (e && e.message) || '';
        var expected = n.indexOf('NotFound') >= 0 || n.indexOf('Checksum') >= 0 ||
                       n.indexOf('Format') >= 0 || n.length <= 2 ||
                       m.indexOf('able to detect') >= 0 || m.indexOf('No MultiFormat') >= 0;
        if (!expected) diag.lastErr = n + (m ? ':' + m.slice(0, 40) : '');
      }
    }
    return null;
  }

  function beginZxing(video) {
    diag.path = 'zxing';
    setStatus(txt('scanning'), 'good');
    state.running = true;
    state.tickN = 0;
    armNoReadHint();

    /* ---------------------------------------------------------------
       OUR OWN FRAME LOOP, not decodeFromVideoElementContinuously().

       That helper never decoded a single frame on the device, and the reason is
       in the library rather than in us:

           playVideoOnLoad(el, cb) {
             el.addEventListener('playing', cb);   // it waits for 'playing'
             this.tryPlayVideo(el);
           }
           tryPlayVideo(v) {
             if (this.isVideoPlaying(v)) { return; }   // ...and returns early
             yield v.play();                            // the only thing that
           }                                            // would fire 'playing'

       We hand it an ALREADY PLAYING element -- deliberately, because we wait for
       real dimensions before decoding anything -- so tryPlayVideo bails, the
       'playing' event never fires again, and its callback is never invoked. The
       device reported zxLoaded=true with zxTries=0, which is exactly that. It
       also calls reset() on setup, stopping the very stream we opened.

       setTimeout rather than requestAnimationFrame: a JS decoder at 60fps would
       cook the battery for no benefit, and rAF stops entirely when the tab is
       backgrounded.
       --------------------------------------------------------------- */
    var tick = function () {
      if (!state.running) return;

      /* No picture this instant (orientation change, app resumed) -- wait. */
      if (!video.videoWidth || !video.videoHeight) {
        state.zxTimer = setTimeout(tick, 200);
        return;
      }

      state.tickN++;
      diag.zxTries++;

      /* Mostly the reticle crop, every fourth frame the whole picture -- so a
         barcode the operator has not centred still reads, just a little slower. */
      var tight = (state.tickN % 4) !== 0;
      var canvas = grabFrame(video, video.videoWidth, video.videoHeight, tight);
      var text = canvas ? decodeCanvas(canvas) : null;
      if (text) { submitPayload(text); return; }

      state.zxTimer = setTimeout(tick, 120);
    };
    tick();
  }

  /* ==================================================================
     SCAN FROM A PHOTO -- the path that does not depend on the live stream.

     A getUserMedia stream is not the camera app. On iOS it typically will not
     macro-focus, so a barcode held where a person naturally holds it is blurry,
     and blur is unreadable no matter how good the decoder is. `capture` hands the
     job to the NATIVE camera, which focuses, exposes and shoots at full
     resolution -- then we decode the still.

     Everything here is built in JavaScript rather than added to eleven templates:
     the markup, the styling and the wiring. A display file rebuild is not needed
     to give every screen the same escape hatch.
     ================================================================== */
  function decodeImageFile(file) {
    if (!file) return;
    setStatus(txt('readingPhoto'), '');
    diag.photoTries++;

    var url = URL.createObjectURL(file);
    var img = new Image();
    img.onload = function () {
      diag.photoW = img.naturalWidth || 0;
      var done = false;

      /* Full frame FIRST here, unlike the live loop. A still is high resolution
         and the operator framed it deliberately, so the crop is the fallback
         rather than the default. */
      loadZxing(function (ok) {
        diag.zxLoaded = ok;
        if (ok) {
          var tries = [false, true];
          for (var i = 0; i < tries.length && !done; i++) {
            var c = grabFrame(img, img.naturalWidth, img.naturalHeight, tries[i]);
            var text = c ? decodeCanvas(c) : null;
            if (text) { done = true; submitPayload(text); }
          }
        }
        try { URL.revokeObjectURL(url); } catch (e) {}
        if (!done) {
          setStatus(txt('photoNoRead'), 'warn');
          showDiag();
        }
      });
    };
    img.onerror = function () {
      try { URL.revokeObjectURL(url); } catch (e) {}
      setStatus(txt('photoFailed'), 'bad');
    };
    img.src = url;
  }

  /* ------------------------------------------------------------------
     THE DIAGNOSTIC LINE.

     The same symptom has now been reported five times and diagnosed from a
     description each time. A separate self-test page fixed that once, but asking
     somebody on a warehouse floor to open a second URL is a poor answer -- so the
     application reports on itself, in the place where the failure happens.

     Hidden until it earns its place: it appears when a scan does not resolve, or
     on demand. It costs nothing when everything works.
     ------------------------------------------------------------------ */
  function diagText() {
    var d = gtScanDiag();
    return 'v' + d.version + ' · ' + (d.path || 'idle') +
           ' · frames ' + (d.zxTries + d.nativeTries) +
           ' · video ' + d.videoW + 'x' + d.videoH +
           (d.frameW ? ' · read ' + d.frameW + 'x' + d.frameH : '') +
           (d.photoTries ? ' · photos ' + d.photoTries + '@' + d.photoW : '') +
           (d.focus ? ' · focus ' + d.focus : '') +
           (d.cameraLabel ? ' · ' + d.cameraLabel : '') +
           (d.serverStamp && d.serverStamp !== d.version ? ' · server ' + d.serverStamp : '') +
           (d.lastErr ? ' · ERR ' + d.lastErr : '');
  }

  /* ------------------------------------------------------------------
     REPORT THROUGH THE SCREEN, because the screen always gets through.

     Two outbound channels have now failed on the device that matters: the access
     log ping and the version probe both fire correctly in Chromium and produced no
     request at all from the iPad. Whatever the cause -- WebKit, a content blocker,
     Private Relay -- I cannot see it from here and cannot fix it blind.

     But ONE channel is proven, every single round: the screen state. The support
     tooling captures the RDF data on every message, so anything the program puts
     into a screen field arrives without the operator doing anything beyond using
     the application.

     So the diagnostics go where they cannot be dropped: submitted as a scan,
     prefixed GTDIAG. The program does not resolve it -- which is correct, it is not
     a barcode -- and records it as an unresolved scan, and the raw value comes back
     in RRAW and in the scan history. It costs one row of demo data and ends the
     round trip.

     Kept to ~110 characters because SCANVAL is char(120) and a truncated report
     loses the error, which is the part worth having.
     ------------------------------------------------------------------ */
  function compactDiag() {
    var d = gtScanDiag();
    return ('GTDIAG v=' + d.version +
            ' via=' + ((window.__gtVia || 'tag') === 'snapshot' ? 'snap' : 'tag') +
            ' p=' + (d.path || '-') +
            ' f=' + (d.zxTries + d.nativeTries) +
            ' vid=' + d.videoW + 'x' + d.videoH +
            ' rd=' + d.frameW + 'x' + d.frameH +
            ' det=' + (d.detectorPresent ? 1 : 0) +
            ' zx=' + (d.zxLoaded ? 1 : 0) + (d.zxVia ? '/' + d.zxVia.slice(0, 4) : '') +
            ' ph=' + d.photoTries +
            ' e=' + ((d.lastErr || '-').replace(/[^A-Za-z0-9:.-]/g, '').slice(0, 18))
           ).slice(0, 118);
  }

  function sendDiagToScreen() {
    if (window.pui && typeof pui.submit === 'function') {
      pui.submit({ action: 'SCAN', scanval: compactDiag() });
    }
  }

  function showDiag() {
    var el = $('#gt-scan-diag');
    if (!el) return;
    el.textContent = diagText();
    el.className = 'gt-scan-diag is-open';
  }

  /* If nothing has resolved after a while the operator is holding a camera that
     looks like it is working. Say what to try, and offer the photo path. */
  function armNoReadHint() {
    if (state.hintTimer) clearTimeout(state.hintTimer);
    state.hintTimer = setTimeout(function () {
      if (!state.running || diag.lastCode) return;
      setStatus(txt('noReadYet'), 'warn');
      showDiag();
      ping('noread');
      var photo = $('#gt-scan-photo-btn');
      if (photo) photo.classList.add('is-urged');
    }, 7000);
  }

  /* One stylesheet for the controls this file creates. Scoped to the band so it
     cannot leak into the Genie skin or a screen's own CSS. */
  var EXTRA_CSS =
    '#gt-scan-camera .gt-scan-extra{display:flex;gap:8px;flex-wrap:wrap;margin-top:8px}' +
    '#gt-scan-camera .gt-scan-photo{min-height:48px;padding:0 14px;border:1px solid #C8C8C4;' +
      'border-radius:10px;background:#fff;color:#2B2B2B;font-size:15px;font-weight:600;' +
      'white-space:normal;cursor:pointer}' +
    '#gt-scan-camera .gt-scan-photo.is-urged{border-color:#DA291C;color:#DA291C;font-weight:700}' +
    '#gt-scan-camera .gt-scan-diag{display:none;flex:0 0 100%;margin-top:6px;font-size:11px;line-height:1.35;' +
      'color:#6E6E69;white-space:normal;word-break:break-word;font-family:"SF Mono",Consolas,monospace}' +
    '#gt-scan-camera .gt-scan-diag.is-open{display:block}' +
    '#gt-scan-camera .gt-scan-ver{flex:0 0 100%;margin-top:4px;font-size:10px;color:#9A9A95;' +
      'white-space:normal;font-family:"SF Mono",Consolas,monospace}';

  function injectExtras(band) {
    if (!document.getElementById('gt-scan-extra-css')) {
      var st = document.createElement('style');
      st.id = 'gt-scan-extra-css';
      st.textContent = EXTRA_CSS;
      (document.head || document.documentElement).appendChild(st);
    }
    if ($('#gt-scan-extra', band)) return;

    var row = document.createElement('div');
    row.className = 'gt-scan-extra';
    row.id = 'gt-scan-extra';

    var input = document.createElement('input');
    input.type = 'file';
    input.accept = 'image/*';
    /* capture asks for the CAMERA rather than the photo library. Where it is not
       honoured the operator simply gets a picker, which is still useful. */
    input.setAttribute('capture', 'environment');
    input.id = 'gt-scan-photo';
    input.style.display = 'none';
    input.addEventListener('change', function () {
      var f = this.files && this.files[0];
      this.value = '';          /* so the same file can be chosen twice */
      decodeImageFile(f);
    });

    var btn = document.createElement('button');
    btn.type = 'button';
    btn.id = 'gt-scan-photo-btn';
    btn.className = 'gt-scan-photo';
    btn.textContent = txt('photoScan');
    btn.addEventListener('click', function () { input.click(); });

    var info = document.createElement('button');
    info.type = 'button';
    info.className = 'gt-scan-photo';
    info.textContent = txt('showDiag');
    info.addEventListener('click', function () {
      var el = $('#gt-scan-diag');
      if (el && el.className.indexOf('is-open') >= 0) { el.className = 'gt-scan-diag'; return; }
      showDiag();
      sendDiagToScreen();
    });

    var diagEl = document.createElement('div');
    diagEl.className = 'gt-scan-diag';
    diagEl.id = 'gt-scan-diag';

    /* ALWAYS VISIBLE, and deliberately so. "Which version is your device
       running?" has been the pivotal question in five of six rounds on this bug,
       and it was never answerable from a screenshot. Ten grey pixels settle it
       for good. */
    var ver = document.createElement('div');
    ver.className = 'gt-scan-ver';
    ver.id = 'gt-scan-ver';
    ver.textContent = 'v' + VERSION;

    /* Everything this function creates goes INSIDE one container, because the
       takeover path removes it by id when a newer copy arrives. The first version
       of this appended the chip and the diagnostic line to the band separately;
       the takeover removed two of the three, so the OLD chip survived alongside a
       new one -- two elements with the same id, and querySelector returned the
       stale one. The chip then reported the version that was no longer running,
       which is the exact confusion it exists to prevent. */
    row.appendChild(btn);
    row.appendChild(info);
    row.appendChild(input);
    row.appendChild(diagEl);
    row.appendChild(ver);
    band.appendChild(row);
  }

  /* Start ZXing, whatever the reason the native path is unavailable. `why` ends
     up in the status text so a screenshot from the floor says WHICH branch we
     took, instead of one message meaning four different things. */
  function startFallback(video, why) {
    diag.reason = why;
    setStatus(txt('loadingDecoder'), '');
    loadZxing(function (ok) {
      diag.zxLoaded = ok;
      if (ok) beginZxing(video);
      else setStatus(txt('noDetector') + ' [' + why + ' / decoder not loaded]', 'warn');
    });
  }

  /* ------------------------------------------------------------------
     ANY native failure falls through to ZXing. Checking that BarcodeDetector
     EXISTS is not enough.

     On Chrome for Android the API is present but backed by a Google Play
     Services module downloaded ON DEMAND -- so where that module is missing the
     constructor throws, or it constructs and reports no supported formats, or
     every detect() rejects. The previous version treated "exists" as "works",
     took the native branch, and showed "cannot decode" without ever trying the
     fallback sitting right beside it. That is why the message survived a hard
     refresh on a Chrome browser.

     Four ways in to the fallback now:
       1. no BarcodeDetector at all           (Safari, desktop Chrome)
       2. the constructor throws              (module missing)
       3. it reports zero supported formats   (module present but empty)
       4. detect() keeps rejecting            (module broken at runtime)
     ------------------------------------------------------------------ */
  function beginDetect(video) {
    diag.detectorPresent = supportsDetector();
    if (!supportsDetector()) { startFallback(video, 'no BarcodeDetector'); return; }

    var det = null;
    try {
      det = new window.BarcodeDetector({ formats: wantedFormats() });
    } catch (e) {
      try { det = new window.BarcodeDetector(); }
      catch (e2) { startFallback(video, 'detector init threw'); return; }
    }
    if (!det) { startFallback(video, 'detector init gave nothing'); return; }

    /* A detector that supports nothing is a detector that cannot help. */
    var check = (window.BarcodeDetector.getSupportedFormats
                  ? window.BarcodeDetector.getSupportedFormats()
                  : Promise.resolve(['unknown']));

    check.then(function (fmts) {
      diag.detectorFormats = fmts ? fmts.slice(0) : [];
      if (!fmts || !fmts.length) {
        startFallback(video, 'detector supports 0 formats');
        return;
      }
      runNative(video, det);
    }).catch(function () {
      startFallback(video, 'getSupportedFormats failed');
    });
  }

  function runNative(video, det) {
    diag.path = 'native';
    state.detector = det;
    setStatus(txt('scanning'), 'good');
    state.running = true;

    var errs = 0;
    var tick = function () {
      if (!state.running || !state.detector) return;
      diag.nativeTries++;
      state.detector.detect(video).then(function (codes) {
        errs = 0;
        if (codes && codes.length) {
          submitPayload(codes[0].rawValue);
          return;
        }
        state.raf = requestAnimationFrame(tick);
      }).catch(function (e) {
        /* One rejection between frames is normal. A RUN of them means the native
           decoder is broken on this device, so hand over rather than spin for
           ever showing a live picture that never resolves. */
        diag.nativeErrs++;
        diag.lastErr = (e && (e.name || e.message)) ? (e.name || e.message) : 'detect rejected';
        errs++;
        if (errs >= 6) {
          state.detector = null;
          state.running = false;
          startFallback(video, 'detector errored');
          return;
        }
        state.raf = requestAnimationFrame(tick);
      });
    };
    state.raf = requestAnimationFrame(tick);
  }

  /* Strings live here rather than in the template because this file is
   * shared; the template owns everything the user reads when JS is absent. */
  function txt(key) {
    var lang = (document.documentElement.getAttribute('data-gt-lang') || 'EN').toUpperCase();
    var S = {
      EN: {
        starting: 'Starting camera...',
        scanning: 'Camera ready - point at a barcode',
        noCamera: 'Camera unavailable. This usually means the page is not on HTTPS.',
        loadingDecoder: 'Starting the decoder…',
        noFrames: 'The camera gave no picture to read. Close it and open it again.',
        noDetector: 'This browser cannot decode in-camera. Use the keypad below.',
        cameraFailed: 'Camera could not be opened',
        photoScan: 'Scan from a photo',
        showDiag: 'Camera details / send report',
        readingPhoto: 'Reading the photo…',
        photoNoRead: 'No barcode found in that photo. Fill the frame with the barcode and try again.',
        photoFailed: 'That photo could not be opened',
        noReadYet: 'Not reading yet — try 15–20 cm back, avoid glare, or use "Scan from a photo".'
      },
      FR: {
        starting: 'Démarrage de la caméra...',
        scanning: 'Caméra prête - visez un code à barres',
        noCamera: 'Caméra indisponible. La page n’est probablement pas en HTTPS.',
        loadingDecoder: 'Démarrage du décodeur…',
        noFrames: 'La caméra n’a fourni aucune image. Fermez-la et réouvrez-la.',
        noDetector: 'Ce navigateur ne peut pas décoder. Utilisez le clavier ci-dessous.',
        cameraFailed: 'Impossible d’ouvrir la caméra',
        photoScan: 'Scanner depuis une photo',
        showDiag: 'Détails caméra / envoyer',
        readingPhoto: 'Lecture de la photo…',
        photoNoRead: 'Aucun code à barres dans cette photo. Remplissez le cadre et réessayez.',
        photoFailed: 'Impossible d’ouvrir cette photo',
        noReadYet: 'Aucune lecture — reculez de 15 à 20 cm, évitez les reflets, ou utilisez « Scanner depuis une photo ».'
      }
    };
    return (S[lang] || S.EN)[key] || '';
  }

  function bind() {
    /* ANCHOR ON THE SCAN BAND, NOT ON A SCREEN CLASS.
     *
     * This used to look for `.gt-scan-screen`, which exists on exactly two
     * templates -- Scan Lab and Item Lookup's scan screen, the only two ever
     * given that class. Every other screen with a camera carries its own
     * wrapper (`.gt-rcvhome-screen`, `.gt-puthome-screen`, ...), so `bind()`
     * returned false, nothing was wired, and the camera button was DEAD on
     * nine of the eleven screens that have one.
     *
     * It hid for so long because the failure is silent and partial: the keyed
     * box beside the camera still works everywhere, since its Go button is an
     * inline `pui.submit` that needs no JavaScript at all. So every screen
     * appeared to scan -- as long as you typed.
     *
     * `#gt-scan-camera` is the band itself. Anchoring here means "wherever
     * there is a scan band, wire it up", which cannot drift when screen twelve
     * invents a new wrapper class. The bound flag goes on the band rather than
     * the screen, so a re-rendered screen arrives with a fresh, unmarked band
     * and rebinds by itself. */
    var band = $('#gt-scan-camera');
    if (!band) return false;
    if (band.getAttribute('data-gt-scan-bound') === '1') return true;
    band.setAttribute('data-gt-scan-bound', '1');

    /* Scope lookups to the whole screen, not just the band: a future screen may
     * put a control outside it, and `.gt-app` is the one wrapper every screen
     * genuinely shares. */
    var root = (band.closest && band.closest('.gt-app')) || document;

    var startBtn = $('#gt-scan-start', root);
    var stopBtn = $('#gt-scan-stop', root);
    if (startBtn) startBtn.addEventListener('click', startCamera);
    if (stopBtn) stopBtn.addEventListener('click', function () {
      stopCamera();
      setStatus('', '');
    });

    /* A keyboard-wedge scanner types fast and ends with Enter. Costs a few
     * lines and means a paired Bluetooth scanner works, though it is not
     * part of the pitch now that the RF guns are being replaced. */
    var manual = $('#gt-scan-manual', root);
    if (manual) {
      manual.addEventListener('keydown', function (ev) {
        if (ev.key === 'Enter') {
          ev.preventDefault();
          submitPayload(manual.value.trim());
        }
      });
    }

    /* The photo-scan button and the diagnostic line are built here rather than
       in eleven templates, so every screen with a scan band gets both. */
    injectExtras(band);

    /* No warning up front any more: a browser without BarcodeDetector now gets
       the ZXing fallback the moment the camera opens, so telling the operator it
       cannot decode would be false. */
    return true;
  }

  /* ==================================================================
     NOTICE WHEN WE ARE STALE, AND UPGRADE IN PLACE.

     The access log settled an argument that had run for five rounds: the device
     had fetched v...f at 15:14, the fix went out at 15:26, and the device made NO
     REQUEST AT ALL after 15:15. The operator refreshed; nothing was re-fetched;
     the old code kept running. Every stamp bump is invisible to a page that never
     asks the server for anything.

     The ?v= stamp only works on a REAL page load. So this asks, once per install,
     whether something newer exists -- and if so pulls it in and lets the
     newer-wins guard above hand over. A stale page heals itself within a second
     of a screen appearing, with no reload and nothing for the operator to do.

     A <script> tag, NOT XHR: the Genie proxy drops XHR to userdata/*, while tag
     fetches to the same path are served normally. The cache-buster is a timestamp
     rather than the stamp, because the whole point is asking a question whose
     answer we do not already know.
     ================================================================== */
  var VERSION_URL = '/profoundui/userdata/ui/gtcommon/gt-version.js';
  var SCAN_URL = '/profoundui/userdata/ui/gtcommon/gt-scan.js';
  var PING_URL = '/profoundui/userdata/ui/gtcommon/gt-ping.js';

  /* ------------------------------------------------------------------
     REPORT TO THE ACCESS LOG. The URL is the message.

     The server records every request with its query string, so putting the state
     in the query means a device tells us what it is running without anybody
     screenshotting anything. No CGI, no table, no RPG change, no new authority --
     just a static file fetched with a long query string.

     This exists because the pivotal fact in six rounds of one bug -- WHICH BUILD
     IS ON THE DEVICE -- was unknowable from here, while the log recorded every
     request faithfully and had nothing useful to record.

     Sent at most three times per page: on install, on the first successful read,
     and once if a scan has not resolved. User agent is truncated; nothing personal
     is collected beyond what every HTTP request already carries.
     ------------------------------------------------------------------ */
  var pinged = {};

  function ping(why) {
    if (pinged[why]) return;
    pinged[why] = true;
    try {
      var d = gtScanDiag();
      var q = '?v=' + VERSION +
              '&via=' + (window.__gtVia || 'tag') +
              '&why=' + why +
              '&path=' + (d.path || '-') +
              '&frames=' + (d.zxTries + d.nativeTries) +
              '&video=' + d.videoW + 'x' + d.videoH +
              '&read=' + d.frameW + 'x' + d.frameH +
              '&det=' + (d.detectorPresent ? 1 : 0) +
              '&zx=' + (d.zxLoaded ? 1 : 0) +
              '&zxvia=' + (d.zxVia || '-') +
              '&photos=' + d.photoTries +
              '&focus=' + encodeURIComponent(d.focus || '-') +
              '&err=' + encodeURIComponent((d.lastErr || '-').slice(0, 40)) +
              '&ua=' + encodeURIComponent((navigator.userAgent || '').slice(0, 90)) +
              '&t=' + (new Date()).getTime();
      /* An IMAGE beacon, not a <script> tag. Same request, but far fewer things
         refuse it: content blockers, Lockdown Mode and any script-src policy all
         treat an image differently, and we do not need the response -- only the
         log line. Chromium fires both identically; the iPad fired neither, which
         is why this no longer relies on one mechanism. */
      if (navigator.sendBeacon) {
        try { navigator.sendBeacon(PING_URL + q); } catch (e) {}
      }
      var img = new Image();
      img.src = PING_URL + q;
      state.beacon = img;          /* keep a reference or GC can cancel it */
    } catch (e) {}
  }

  function checkFresher() {
    if (window.__gtVersionChecked) return;   /* once per page, not once per render */
    window.__gtVersionChecked = true;
    try {
      var probe = document.createElement('script');
      probe.src = VERSION_URL + '?cb=' + (new Date()).getTime();
      probe.async = true;
      probe.onload = function () {
        var stamp = window.__gtStamp || '';
        diag.serverStamp = stamp;
        if (!stamp || String(stamp) <= VERSION) return;   /* we are current */
        diag.fresher = stamp;
        var el = document.createElement('script');
        el.src = SCAN_URL + '?v=' + stamp;
        el.async = true;
        (document.head || document.documentElement).appendChild(el);
      };
      probe.onerror = function () { diag.serverStamp = 'unreachable'; };
      (document.head || document.documentElement).appendChild(probe);
    } catch (e) {}
  }

  function init(attempt) {
    if (bind()) return;
    if (attempt > 80) return;
    setTimeout(function () { init(attempt + 1); }, 50);
  }

  function gtScanDiag() {
    diag.videoW = (($('#gt-scan-video') || {}).videoWidth) || diag.videoW;
    diag.videoH = (($('#gt-scan-video') || {}).videoHeight) || diag.videoH;
    return diag;
  }

  window.gtScan = {
    __installed: true,
    __version: VERSION,
    diag: gtScanDiag,
    photo: decodeImageFile,
    start: startCamera,
    stop: stopCamera,
    submit: submitPayload,
    rebind: function () { init(0); }
  };

  init(0);
  checkFresher();
  ping('install');
})();
