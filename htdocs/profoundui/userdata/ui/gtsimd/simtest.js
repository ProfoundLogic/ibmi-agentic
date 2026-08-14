/* ======================================================================
 * Simple camera test -- the entire client side, in one file.
 *
 * WHY THIS IS NOT gt-scan.js
 *
 * gt-scan.js serves eleven screens and carries everything seven rounds of
 * iPad debugging added to it: a version chip, a self-upgrade, an access-log
 * beacon, a photo path, a no-read hint, per-screen takeover. Every one of
 * those was added for a reason and together they make it impossible to say
 * which part a device is failing on. This file is the same job with nothing
 * else attached, so a failure here has only a handful of possible causes.
 *
 * It answers three questions, and reports all three on screen:
 *
 *   1. Which decoder can this browser actually get?  ->  D.dec
 *   2. Which delivery routes reach this device?      ->  D.routes
 *   3. Is it getting a picture with pixels in it?    ->  D.vid / D.frames
 *
 * Question 2 is the one that closed the main application's bug: on the iPad
 * inside Genie, a dynamically appended <script src> produced NO network
 * request at all, so the decoder never loaded and the camera could never read
 * anything. That is measured here rather than assumed -- probeRoutes() tries
 * each mechanism against a 249-byte file and records what happened.
 *
 * Contract with the template:
 *   - every button uses an inline onclick calling a window global. A file in
 *     the "js" array runs BEFORE the template renders, so addEventListener
 *     here would bind to nothing. See ejs-screen-js-inline-handlers.
 *   - the screen must be usable with this file absent: the template renders
 *     the keypad, the result frame and the history server-side.
 * ====================================================================== */
(function () {
  'use strict';

  var VERSION   = '20260811q';
  var ZXING_SRC = '/profoundui/userdata/ui/gtcommon/gt-zxing.min.js';
  var PROBE_SRC = '/profoundui/userdata/ui/gtcommon/gt-version.js';

  var TICK_MS  = 120;   /* between frames. 8/s is far more than a hand can aim */
  var MAX_EDGE = 1280;  /* cap the analysed frame; bigger costs time, not reads */

  var T = {
    EN: {
      ready:     'Ready. Tap the camera to start.',
      starting:  'Starting the camera…',
      noCamera:  'This browser will not give a camera to this page.',
      denied:    'Camera permission was refused. Allow it in the browser settings and try again.',
      noPicture: 'The camera gave no picture to read. Close it and open it again.',
      noDecoder: 'No barcode decoder could be loaded, so nothing can be read from the picture. Type a value in below.',
      scanning:  'Scanning — hold the barcode inside the yellow frame.',
      hint:      'No read yet. Try 15–20 cm away, fill the frame with the barcode, and avoid glare.',
      hit:       'Read: ',
      photoBusy: 'Reading the photo…',
      photoNo:   'No barcode found in that photo. Try again, closer and square on.',
      closed:    'Camera closed.'
    },
    FR: {
      ready:     'Prêt. Touchez la caméra pour commencer.',
      starting:  'Démarrage de la caméra…',
      noCamera:  'Ce navigateur refuse de donner une caméra à cette page.',
      denied:    'Accès à la caméra refusé. Autorisez-le dans les réglages du navigateur.',
      noPicture: 'La caméra n’a fourni aucune image. Fermez-la et ouvrez-la de nouveau.',
      noDecoder: 'Aucun décodeur n’a pu être chargé, donc rien ne peut être lu. Saisissez une valeur ci-dessous.',
      scanning:  'Balayage — gardez le code dans le cadre jaune.',
      hint:      'Aucune lecture. Essayez à 15–20 cm, remplissez le cadre et évitez les reflets.',
      hit:       'Lu : ',
      photoBusy: 'Lecture de la photo…',
      photoNo:   'Aucun code trouvé dans cette photo. Réessayez de plus près.',
      closed:    'Caméra fermée.'
    }
  };

  /* Everything we know, in one flat object. It becomes the on-screen line and
     the DIAG field, so keys are short on purpose -- DIAG is char(240). */
  var D = {
    v: VERSION,
    dec: 'none',      /* which decoder is in use */
    routes: '',       /* which delivery mechanisms reached the device */
    vid: '0x0',       /* what the camera actually gave us */
    crop: '0x0',      /* what we handed the decoder */
    frames: 0,        /* frames examined -- 0 with a live picture means the loop */
    focus: '?',
    err: ''
  };

  var state = {
    stream: null, detector: null, reader: null, hints: null,
    canvas: null, ctx: null, timer: 0, hintTimer: 0,
    running: false, probed: false, tight: true,
    lastCode: '', lastAt: 0
  };

  function $(id) { return document.getElementById(id); }

  function root() { return document.querySelector('.gts-screen'); }

  function lang() {
    var el = root();
    var v = el && el.getAttribute('data-gt-lang');
    return (v && v.toUpperCase() === 'FR') ? 'FR' : 'EN';
  }

  function txt(k) { return (T[lang()] || T.EN)[k] || ''; }

  function setStatus(text, kind) {
    var el = $('gts-status');
    if (!el) return;
    el.textContent = text;
    el.className = 'gts-status' + (kind ? ' is-' + kind : '');
  }

  function showDiag() {
    var el = $('gts-diagline');
    if (el) el.textContent = diagText();
  }

  /* One line, short keys, no JSON punctuation to be mangled by a paste. */
  function diagText() {
    return 'v=' + D.v +
           ' dec=' + D.dec +
           ' routes=' + (D.routes || '?') +
           ' vid=' + D.vid +
           ' crop=' + D.crop +
           ' frames=' + D.frames +
           ' focus=' + D.focus +
           (D.err ? ' err=' + D.err : '');
  }

  function note(where, e) {
    D.err = where + ':' + ((e && (e.name || e.message)) || '?');
    showDiag();
  }

  /* ==================================================================
     WHICH DELIVERY ROUTES REACH THIS DEVICE

     Measured, not assumed, and against a 249-byte file so the answer costs
     nothing. On the iPad this is the difference between "the decoder is
     broken" and "the decoder was never delivered".
     ================================================================== */
  function probeRoutes() {
    if (state.probed) return;
    state.probed = true;

    var results = [];
    var bust = '?cb=' + (VERSION + '-' + Math.floor(performance.now()));

    var finish = function (name, ok) {
      results.push(name + (ok ? ':ok' : ':no'));
      D.routes = results.join(',');
      showDiag();
    };

    if (typeof fetch === 'function') {
      try {
        fetch(PROBE_SRC + bust, { cache: 'no-store' })
          .then(function (r) { finish('fetch', r && r.ok); })
          .catch(function () { finish('fetch', false); });
      } catch (e) { finish('fetch', false); }
    } else {
      finish('fetch', false);
    }

    /* The mechanism that silently produced no request at all in the main
       application. If this says :no here too, that is the whole story. */
    try {
      var el = document.createElement('script');
      el.src = PROBE_SRC + bust + '-tag';
      el.async = true;
      el.onload  = function () { finish('tag', true); };
      el.onerror = function () { finish('tag', false); };
      (document.head || document.documentElement).appendChild(el);
    } catch (e) { finish('tag', false); }
  }

  /* ==================================================================
     GETTING A DECODER

     In order of how likely each is to work here, stopping at the first that
     does. `inline` is the base64 copy the Genie shim carries in the page --
     the only route proven to reach the iPad, because the screen JavaScript
     it delivers is what is running right now.
     ================================================================== */
  function nativeDetector() {
    if (typeof window.BarcodeDetector !== 'function') return null;
    try {
      var det = new window.BarcodeDetector({
        formats: ['upc_a', 'upc_e', 'ean_13', 'ean_8', 'itf', 'code_128',
                  'code_39', 'code_93', 'codabar', 'qr_code', 'data_matrix',
                  'pdf417', 'aztec']
      });
      return det && typeof det.detect === 'function' ? det : null;
    } catch (e) {
      /* Present but unusable. On Chrome for Android the API is backed by a
         Play Services module that is downloaded on demand, so the constructor
         throwing is a normal state, not a broken browser. */
      note('detector', e);
      return null;
    }
  }

  function haveZxing() {
    return !!(window.ZXing && window.ZXing.BrowserMultiFormatReader);
  }

  function loadDecoder(cb) {
    var det = nativeDetector();
    if (det) { state.detector = det; D.dec = 'native'; return cb(true); }

    if (haveZxing()) { D.dec = 'page'; return cb(true); }

    if (typeof window.__gtLoadZxing === 'function') {
      try {
        if (window.__gtLoadZxing() && haveZxing()) { D.dec = 'inline'; return cb(true); }
      } catch (e) { note('inline', e); }
    }

    /* fetch + eval rather than a <script src>: the request itself is what the
       iPad appears to drop, and fetch is a different code path in WebKit. */
    if (typeof fetch === 'function') {
      fetch(ZXING_SRC, { cache: 'force-cache' })
        .then(function (r) {
          if (!r || !r.ok) throw new Error('http');
          return r.text();
        })
        .then(function (src) {
          (new Function(src))();
          if (haveZxing()) { D.dec = 'fetch'; cb(true); }
          else tagRoute(cb);
        })
        .catch(function (e) { note('fetch', e); tagRoute(cb); });
      return;
    }

    tagRoute(cb);
  }

  function tagRoute(cb) {
    var el = document.createElement('script');
    el.src = ZXING_SRC;
    el.async = true;
    el.onload = function () {
      if (haveZxing()) { D.dec = 'tag'; cb(true); } else cb(false);
    };
    el.onerror = function () { cb(false); };
    (document.head || document.documentElement).appendChild(el);
  }

  /* ==================================================================
     DECODING A FRAME

     We draw the pixels ourselves. Handing the <video> to the library means it
     analyses the whole picture, so a barcode filling a fifth of a 1080-line
     frame is a few pixels per bar by the time the binarizer sees it. Cropping
     to the reticle roughly triples the pixels per bar at the same cost.
     ================================================================== */
  function ctx2d() {
    if (!state.canvas) {
      state.canvas = document.createElement('canvas');
      /* willReadFrequently keeps the surface on the CPU. Without it every read
         round-trips from the GPU and the loop crawls; older WebKit throws on
         the options object rather than ignoring it. */
      try { state.ctx = state.canvas.getContext('2d', { willReadFrequently: true }); }
      catch (e) { state.ctx = state.canvas.getContext('2d'); }
    }
    return state.ctx;
  }

  /* Alternating tight and full keeps both a barcode held in the reticle and
     one held anywhere in shot readable, without paying for two decodes. */
  function grab(src, sw, sh, tight) {
    var ctx = ctx2d();
    if (!ctx) return null;

    var cw = tight ? Math.round(sw * 0.94) : sw;
    var ch = tight ? Math.round(sh * 0.45) : sh;
    var cx = Math.round((sw - cw) / 2);
    var cy = Math.round((sh - ch) / 2);

    var scale = Math.min(1, MAX_EDGE / Math.max(cw, ch));
    var dw = Math.max(1, Math.round(cw * scale));
    var dh = Math.max(1, Math.round(ch * scale));

    if (state.canvas.width !== dw || state.canvas.height !== dh) {
      state.canvas.width = dw;
      state.canvas.height = dh;
    }
    try { ctx.drawImage(src, cx, cy, cw, ch, 0, 0, dw, dh); }
    catch (e) { note('draw', e); return null; }

    D.crop = dw + 'x' + dh;
    return state.canvas;
  }

  function zxReader() {
    var Z = window.ZXing;
    if (!state.reader) {
      state.hints = new Map();
      var want = ['UPC_A', 'UPC_E', 'EAN_13', 'EAN_8', 'ITF', 'CODE_128',
                  'CODE_39', 'CODE_93', 'CODABAR', 'QR_CODE', 'DATA_MATRIX',
                  'PDF_417', 'AZTEC'];
      var fmts = [];
      for (var i = 0; i < want.length; i++) {
        if (Z.BarcodeFormat[want[i]] !== undefined) fmts.push(Z.BarcodeFormat[want[i]]);
      }
      state.hints.set(Z.DecodeHintType.POSSIBLE_FORMATS, fmts);
      state.hints.set(Z.DecodeHintType.TRY_HARDER, true);
      state.reader = new Z.MultiFormatReader();
      if (state.reader.setHints) state.reader.setHints(state.hints);
    }
    return state.reader;
  }

  /* Three attempts on one frame, cheapest first. They fail differently:
       HybridBinarizer  -- local thresholding, right for an evenly lit photo
       GlobalHistogram  -- one threshold, wins on flat high-contrast frames
       inverted         -- a light-on-dark label, or a screen photographed
     All three together cost far less than the 120ms we wait between frames. */
  function decodeCanvas(canvas) {
    var Z = window.ZXing;
    var reader = zxReader();
    var lum;
    try { lum = new Z.HTMLCanvasElementLuminanceSource(canvas); }
    catch (e) { note('lum', e); return null; }

    var passes = [
      function () { return new Z.BinaryBitmap(new Z.HybridBinarizer(lum)); },
      function () { return new Z.BinaryBitmap(new Z.GlobalHistogramBinarizer(lum)); },
      function () { return new Z.BinaryBitmap(new Z.HybridBinarizer(new Z.InvertedLuminanceSource(lum))); }
    ];

    for (var i = 0; i < passes.length; i++) {
      try {
        reader.reset();
        var res = reader.decode(passes[i](), state.hints);
        if (res) {
          var text = null;
          try { text = res.getText(); } catch (e) { text = res.text; }
          var fmt = '';
          try { fmt = Z.BarcodeFormat[res.getBarcodeFormat()] || ''; } catch (e) { fmt = ''; }
          if (text) return { text: text, format: fmt };
        }
      } catch (e) {
        /* No barcode in this frame is the normal case and is not worth
           recording. Minified builds rename the exception class, so match the
           message shape rather than the identity. */
        var n = (e && (e.name || (e.constructor && e.constructor.name))) || '';
        var m = (e && e.message) || '';
        var expected = n.indexOf('NotFound') >= 0 || n.indexOf('Checksum') >= 0 ||
                       n.indexOf('Format') >= 0 || n.length <= 2 ||
                       m.indexOf('able to detect') >= 0 || m.indexOf('No MultiFormat') >= 0;
        if (!expected) note('decode', e);
      }
    }
    return null;
  }

  /* ==================================================================
     THE FRAME LOOP
     ================================================================== */
  function tick() {
    if (!state.running) return;
    var video = $('gts-video');
    if (!video) return;

    D.frames++;
    state.tight = !state.tight;

    var done = function () {
      if (state.running) state.timer = setTimeout(tick, TICK_MS);
      showDiag();
    };

    if (state.detector) {
      /* The native detector takes the element directly and does its own
         cropping, so there is nothing to draw. */
      state.detector.detect(video).then(function (list) {
        if (list && list.length) {
          return hit(list[0].rawValue, (list[0].format || 'native').toUpperCase());
        }
        done();
      }).catch(function (e) {
        /* Every detect() rejecting means the API is present but not working.
           Fall through to the JavaScript decoder rather than spinning. */
        note('native', e);
        state.detector = null;
        loadDecoder(function (ok) {
          if (!ok) return fail(txt('noDecoder'));
          done();
        });
      });
      return;
    }

    var canvas = grab(video, video.videoWidth, video.videoHeight, state.tight);
    var res = canvas ? decodeCanvas(canvas) : null;
    if (res) return hit(res.text, res.format || 'UNKNOWN');
    done();
  }

  function hit(code, format) {
    if (!code) return;

    /* Continuous decode fires many times for one physical barcode. */
    var now = Date.now();
    if (code === state.lastCode && now - state.lastAt < 2500) return;
    state.lastCode = code;
    state.lastAt = now;

    try { if (navigator.vibrate) navigator.vibrate(60); } catch (e) {}

    setStatus(txt('hit') + code, 'good');
    /* Refresh before stopping. hit() is the ONE path that leaves the loop
       without going through done(), so without this the details line still
       reads dec=none frames=0 after a successful read -- a readout that lies
       exactly when someone is finally looking at it. */
    showDiag();
    stop();

    /* Mirror into the visible field so the value is on screen in the instant
       before the round trip repaints. There is exactly one element named
       scanval -- a hidden duplicate would leave which value reaches RPG
       undefined. */
    var input = $('gts-manual');
    if (input) input.value = code;

    if (window.pui && typeof pui.submit === 'function') {
      pui.submit({ action: 'SCAN', scanval: code, lastsymb: format, diag: diagText() });
    }
  }

  function fail(message) {
    setStatus(message, 'bad');
    showDiag();
  }

  /* ==================================================================
     CAMERA
     ================================================================== */
  function start() {
    if (state.running) return;

    var video = $('gts-video');
    var el = root();
    if (!video || !el) return;

    probeRoutes();

    el.classList.add('is-live');
    var open = $('gts-open'), close = $('gts-close');
    if (open) open.hidden = true;
    if (close) close.hidden = false;
    setStatus(txt('starting'), 'warn');

    /* Scroll the camera into view. Opening a viewfinder half a page below the
       fold means the operator has to hunt for it before they can aim. */
    try { el.scrollIntoView({ block: 'start', behavior: 'smooth' }); } catch (e) {}

    if (!navigator.mediaDevices || !navigator.mediaDevices.getUserMedia) {
      return fail(txt('noCamera'));
    }

    navigator.mediaDevices.getUserMedia({
      /* Ask for the rear camera and plenty of pixels. focusMode is ignored by
         WebKit and honoured by Chrome on Android; asking costs nothing. */
      video: {
        facingMode: { ideal: 'environment' },
        width:  { ideal: 1920 },
        height: { ideal: 1080 },
        focusMode: 'continuous'
      },
      audio: false
    }).then(function (stream) {
      state.stream = stream;
      video.srcObject = stream;

      try {
        var track = stream.getVideoTracks()[0];
        var caps = track && track.getCapabilities ? track.getCapabilities() : null;
        D.focus = (caps && caps.focusMode) ? String(caps.focusMode) : 'not adjustable';
      } catch (e) { D.focus = '?'; }

      return video.play().then(function () { return awaitPicture(video); });
    }).then(function (ok) {
      if (!ok) return fail(txt('noPicture'));

      D.vid = video.videoWidth + 'x' + video.videoHeight;
      showDiag();

      loadDecoder(function (got) {
        if (!got) return fail(txt('noDecoder'));
        state.running = true;
        D.frames = 0;
        showDiag();
        setStatus(txt('scanning'), 'good');
        clearTimeout(state.hintTimer);
        state.hintTimer = setTimeout(function () {
          if (state.running) setStatus(txt('hint'), 'warn');
        }, 8000);
        tick();
      });
    }).catch(function (e) {
      var name = (e && e.name) || '';
      note('camera', e);
      fail(name === 'NotAllowedError' || name === 'SecurityError'
             ? txt('denied') : txt('noCamera'));
    });
  }

  /* video.play() resolving does NOT mean the video has a picture. Metadata can
     arrive later, leaving videoWidth at 0, and a 0x0 frame makes every decoder
     fail silently for ever -- a live picture that never reads anything. */
  function awaitPicture(video) {
    return new Promise(function (resolve) {
      var tries = 0;
      var look = function () {
        if (video.videoWidth > 0 && video.videoHeight > 0) return resolve(true);
        if (++tries > 60) return resolve(false);   /* ~6 seconds */
        setTimeout(look, 100);
      };
      look();
    });
  }

  function stop() {
    state.running = false;
    clearTimeout(state.timer);
    clearTimeout(state.hintTimer);

    if (state.stream) {
      try {
        var tracks = state.stream.getTracks();
        for (var i = 0; i < tracks.length; i++) tracks[i].stop();
      } catch (e) {}
      state.stream = null;
    }

    var video = $('gts-video');
    if (video) { try { video.srcObject = null; } catch (e) {} }

    var el = root();
    if (el) el.classList.remove('is-live');
    var open = $('gts-open'), close = $('gts-close');
    if (open) open.hidden = false;
    if (close) close.hidden = true;
  }

  /* ==================================================================
     A STILL PHOTO -- the path that does not depend on the live stream.

     The native camera app focuses, exposes and shoots at full resolution,
     where a getUserMedia stream on iOS may never focus at all. If the live
     camera shows a picture and never reads, this is the thing to try.
     ================================================================== */
  function fromPhoto(input) {
    var file = input && input.files && input.files[0];
    if (!file) return;
    input.value = '';   /* or picking the same file twice does nothing */

    setStatus(txt('photoBusy'), 'warn');

    loadDecoder(function (got) {
      if (!got) return fail(txt('noDecoder'));

      var url = URL.createObjectURL(file);
      var img = new Image();
      img.onload = function () {
        D.vid = img.naturalWidth + 'x' + img.naturalHeight;

        /* Full frame first: a deliberately taken photo usually has the barcode
           in the middle but framed loosely, and there is no reticle to crop to. */
        var res = null;
        var tries = [false, true];
        for (var i = 0; i < tries.length && !res; i++) {
          var canvas = grab(img, img.naturalWidth, img.naturalHeight, tries[i]);
          if (canvas) res = decodeCanvas(canvas);
        }

        URL.revokeObjectURL(url);
        D.frames++;
        showDiag();

        if (res) hit(res.text, (res.format || 'UNKNOWN') + '/PHOTO');
        else fail(txt('photoNo'));
      };
      img.onerror = function () {
        URL.revokeObjectURL(url);
        fail(txt('photoNo'));
      };
      img.src = url;
    });
  }

  /* ==================================================================
     Globals the template's inline handlers call, and first paint.
     ================================================================== */
  window.gtsOpen  = start;
  window.gtsClose = function () { stop(); setStatus(txt('closed'), ''); };
  window.gtsPhoto = fromPhoto;

  window.gtsSendDiag = function () {
    if (window.pui && typeof pui.submit === 'function') {
      pui.submit({ action: 'DIAG', diag: diagText() });
    }
  };

  /* This file runs before the template renders, so there is nothing to write
     to yet. Wait for the markup, then replace the no-JavaScript notice with
     the real state -- if that notice is still on screen, this file never ran. */
  (function paint(attempt) {
    if (!$('gts-status') || !$('gts-diagline')) {
      if (attempt < 100) setTimeout(function () { paint(attempt + 1); }, 50);
      return;
    }
    setStatus(txt('ready'), '');
    showDiag();

    /* Photo capture only makes sense once this file is running -- otherwise
       the file dialog leads nowhere. */
    var wrap = $('gts-photowrap');
    if (wrap) wrap.hidden = false;
  })(0);
}());
