/* ==================================================================
 * gt-photo -- add a photograph of the actual stock from the device camera.
 *
 * The catalogue image shows what the product is SUPPOSED to look like. This
 * is for what is actually on the pallet: crushed cartons, a relabelled case,
 * a mixed skid, water damage. The operator is standing in front of it with a
 * camera in their hand, so let them use it.
 *
 * HOW IT GETS TO THE PROGRAM
 * --------------------------
 * There is no upload endpoint. No /cgi-bin alias on this server can be
 * pointed at TIGERPOC (see the design doc, section 29), so the photograph
 * travels the only channel that already exists: a field on the display file.
 *
 * That imposes a HARD SIZE LIMIT. IMGDATA is char(24000), and base64 costs
 * 4 bytes for every 3, so the JPEG has to come in under about 17 KB. The
 * ladder below resizes and re-encodes until it does, dropping resolution
 * before it drops quality too far -- a slightly soft 800px photo of a damaged
 * carton is useful; a 340px one at quality 0.3 is not, so that is the floor
 * and past it we refuse rather than store mush.
 *
 * LIVE CAMERA FIRST, FILE INPUT ONLY AS A FALLBACK
 * ------------------------------------------------
 * This used to hand off to <input type="file" capture>, on the reasoning that
 * the platform camera app brings autofocus, flash and HDR for free. True, but
 * it buys those at the cost of the thing that matters here: on a phone the
 * operator got an intermediate "camera or gallery?" chooser, and on a desktop
 * a plain file dialog -- neither of which is "press the button, take the
 * picture" while standing in front of a damaged carton.
 *
 * So the camera button now opens a live stream in the panel, the same
 * getUserMedia path gt-scan.js uses, with an explicit shutter. The file input
 * is still here and still wired, but it is reached ONLY when getUserMedia is
 * refused -- no camera on the machine, or a non-HTTPS origin, which is what
 * makes demoing from a laptop possible. Nothing auto-opens a file dialog.
 *
 * getUserMedia needs a secure origin. It is available here because Genie is
 * served over HTTPS; if that ever changes, every device silently lands on the
 * fallback and the status line says why.
 *
 * NO JAVASCRIPT, NO HARM
 * ----------------------
 * Screen JS is delivered through the Genie shim and can go stale. Everything
 * here is additive: the camera button is rendered by the template and simply
 * does nothing if this file never arrives. The screen itself is unaffected.
 *
 * Handlers are inline attributes calling these globals rather than
 * addEventListener bindings, because screen JS runs BEFORE the template has
 * rendered -- there is nothing to bind to yet, and the failure is silent.
 * ================================================================== */

(function () {
  'use strict';

  /* See the long note in gt-scan.js. This file is delivered TWICE -- as a
     cache-stamped <script src> listed in the display file, and as a fresh
     snapshot injected by the Genie shim -- and there is no guarantee which runs
     last. It had no guard at all, so a months-old cached copy arriving second
     silently replaced the current one. */
  var VERSION = '20260811k';

  /* Display-file field is char(24000). Stay clear of the edge: a truncated
   * base64 string decodes to a corrupt JPEG, not to an error. */
  var MAX_B64 = 23000;

  /* Resolution first, then quality. Each rung is tried in order and the first
   * one that fits is used, so a small photo keeps its full quality and only a
   * large one pays. */
  var LADDER = [
    { max: 1024, q: 0.72 },
    { max: 900,  q: 0.66 },
    { max: 800,  q: 0.60 },
    { max: 700,  q: 0.52 },
    { max: 600,  q: 0.45 },
    { max: 500,  q: 0.40 },
    { max: 420,  q: 0.35 },
    { max: 340,  q: 0.30 }
  ];

  var T = {
    EN: { title: 'Add a photo', use: 'Use this photo', retake: 'Retake',
          cancel: 'Cancel', working: 'Preparing photo…',
          saving: 'Saving…',
          tooBig: 'That photo could not be made small enough to send.',
          notImage: 'That file is not an image.',
          failed: 'The photo could not be read.',
          starting: 'Starting camera…',
          ready: 'Camera ready — press the shutter',
          noCamera: 'No camera available on this device. Choose a file instead.',
          camFailed: 'The camera could not be opened. Choose a file instead.' },
    FR: { title: 'Ajouter une photo', use: 'Utiliser cette photo',
          retake: 'Reprendre', cancel: 'Annuler',
          working: 'Préparation de la photo…',
          saving: 'Enregistrement…',
          tooBig: 'Cette photo n’a pas pu être réduite suffisamment.',
          notImage: 'Ce fichier n’est pas une image.',
          failed: 'La photo n’a pas pu être lue.',
          starting: 'Démarrage de la caméra…',
          ready: 'Caméra prête — appuyez sur le déclencheur',
          noCamera: 'Aucune caméra sur cet appareil. Choisissez un fichier.',
          camFailed: 'Impossible d’ouvrir la caméra. Choisissez un fichier.' }
  };

  function lang() {
    var app = document.querySelector('.gt-app[data-gt-lang]');
    var v = app ? app.getAttribute('data-gt-lang') : 'EN';
    return T[v] ? v : 'EN';
  }
  function t() { return T[lang()]; }

  /* ---------------------------------------------------------------- */
  /* Decode with EXIF orientation honoured. A phone held upright records
   * the sensor's landscape frame plus a rotation tag; drawing the raw
   * bitmap would store every portrait photo on its side. */
  function decode(file) {
    if (window.createImageBitmap) {
      try {
        return createImageBitmap(file, { imageOrientation: 'from-image' })
          .catch(function () { return decodeViaImg(file); });
      } catch (e) { /* older signature rejects the options object */ }
    }
    return decodeViaImg(file);
  }

  function decodeViaImg(file) {
    return new Promise(function (resolve, reject) {
      var url = URL.createObjectURL(file);
      var img = new Image();
      img.onload = function () { URL.revokeObjectURL(url); resolve(img); };
      img.onerror = function () { URL.revokeObjectURL(url); reject(new Error('decode')); };
      img.src = url;
    });
  }

  /* Walk the ladder until the encoded string fits the field. */
  function shrink(bitmap) {
    var w = bitmap.width || bitmap.naturalWidth;
    var h = bitmap.height || bitmap.naturalHeight;
    var canvas = document.createElement('canvas');
    var ctx = canvas.getContext('2d');

    for (var i = 0; i < LADDER.length; i++) {
      var rung = LADDER[i];
      var scale = Math.min(1, rung.max / Math.max(w, h));
      canvas.width = Math.max(1, Math.round(w * scale));
      canvas.height = Math.max(1, Math.round(h * scale));
      ctx.clearRect(0, 0, canvas.width, canvas.height);
      ctx.drawImage(bitmap, 0, 0, canvas.width, canvas.height);

      var url = canvas.toDataURL('image/jpeg', rung.q);
      var b64 = url.slice(url.indexOf(',') + 1);
      if (b64.length <= MAX_B64) {
        return { dataUrl: url, b64: b64, w: canvas.width, h: canvas.height,
                 bytes: Math.round(b64.length * 3 / 4) };
      }
    }
    return null;
  }

  /* ---------------------------------------------------------------- */
  var pending = null;   /* the shrunk photo awaiting confirmation */
  var stream = null;    /* live MediaStream, when the camera is open */

  function overlay() { return document.getElementById('gt-photo-overlay'); }
  function video() { return document.getElementById('gt-photo-video'); }

  function setStatus(text) {
    var el = document.getElementById('gt-photo-status');
    if (el) el.textContent = text || '';
  }

  /* One place decides which of the three states the panel is in, so the
   * controls cannot drift out of step with whether the camera is running. */
  function setState(name) {
    var o = overlay();
    if (!o) return;
    o.classList.remove('is-live', 'is-shot');
    if (name) o.classList.add(name);
  }

  /* ------------------------------------------------------------------
     PIN THE OVERLAY TO THE VIEWPORT.

     The CSS is `position: absolute; inset: 0`, which covers the nearest
     POSITIONED ANCESTOR -- and on the Cycle Count variance screen that ancestor
     is a 26-row page several thousand pixels tall. The panel is centred in it
     with `margin: auto`, so the camera opened literally halfway down the
     document and the operator had to scroll to find it.

     `position: fixed` is the obvious answer and is not available here: Genie
     owns the page and a fixed element anchors to whatever transformed or
     positioned container the skin has wrapped us in, which differs between
     skins. So the viewport rectangle is computed instead, from the overlay's own
     offsetParent, which is correct whatever Genie does:

         parent.getBoundingClientRect().top  is the parent's offset from the
         viewport, so -that puts our top edge exactly at the viewport's top.

     With no positioned ancestor the offsetParent is <body> and its rect.top is
     -scrollY, so this reduces to "top = current scroll position" -- the same
     answer by the same arithmetic.
     ------------------------------------------------------------------ */
  function pinToViewport(o) {
    if (!o) return;
    var pTop = 0, pLeft = 0;
    var parent = o.offsetParent;
    if (parent && parent.getBoundingClientRect) {
      var r = parent.getBoundingClientRect();
      pTop = r.top;
      pLeft = r.left;
    }
    /* right/bottom are cleared because `inset: 0` set them, and they would
       fight an explicit width/height. */
    o.style.top = (-pTop) + 'px';
    o.style.left = (-pLeft) + 'px';
    o.style.right = 'auto';
    o.style.bottom = 'auto';
    o.style.width = (window.innerWidth || 0) + 'px';
    o.style.height = (window.innerHeight || 0) + 'px';
  }

  /* Kept while the overlay is open: a phone can still scroll behind a
     backdrop, and the camera drifting off screen is the bug all over again. */
  function onViewportChange() {
    var o = overlay();
    if (o && o.classList.contains('is-open')) pinToViewport(o);
  }

  function show() {
    var o = overlay();
    if (!o) return;
    o.classList.add('is-open');
    pinToViewport(o);
    window.addEventListener('scroll', onViewportChange, true);
    window.addEventListener('resize', onViewportChange);
    window.addEventListener('orientationchange', onViewportChange);
  }

  function unpin() {
    window.removeEventListener('scroll', onViewportChange, true);
    window.removeEventListener('resize', onViewportChange);
    window.removeEventListener('orientationchange', onViewportChange);
  }

  function stopCamera() {
    if (stream) {
      stream.getTracks().forEach(function (t) { try { t.stop(); } catch (e) {} });
      stream = null;
    }
    var v = video();
    if (v) { try { v.pause(); } catch (e) {} v.srcObject = null; }
  }

  function close() {
    stopCamera();
    unpin();
    var o = overlay();
    if (o) {
      o.classList.remove('is-open', 'is-live', 'is-shot', 'is-fallback');
      /* Hand the geometry back to the stylesheet, so a closed overlay carries
         no stale inline pixels into the next screen. */
      o.style.top = '';
      o.style.left = '';
      o.style.right = '';
      o.style.bottom = '';
      o.style.width = '';
      o.style.height = '';
    }
    pending = null;
    var prev = document.getElementById('gt-photo-preview');
    if (prev) prev.removeAttribute('src');
    var useBtn = document.getElementById('gt-photo-use');
    if (useBtn) useBtn.disabled = true;
    setStatus('');
    var input = document.getElementById('gt-photo-file');
    /* Clearing lets the SAME file be chosen again -- otherwise "retake",
     * then picking the identical shot, fires no change event at all. */
    if (input) input.value = '';
  }

  /* Drop to the file input. Only ever called after the live camera has been
   * refused: it surfaces the button and explains why, and does NOT open a
   * file dialog by itself -- an unexpected file chooser is precisely what
   * this screen was changed to stop doing. */
  function fallback(msg) {
    stopCamera();
    setState(null);
    var o = overlay();
    if (o) o.classList.add('is-fallback');
    setStatus(msg);
  }

  function startCamera() {
    var o = overlay();
    var v = video();
    if (!o || !v) return;

    pending = null;
    var useBtn = document.getElementById('gt-photo-use');
    if (useBtn) useBtn.disabled = true;
    setState(null);
    setStatus(t().starting);

    if (!navigator.mediaDevices || !navigator.mediaDevices.getUserMedia) {
      fallback(t().noCamera);
      return;
    }

    /* Ask for the rear camera and a big frame: this is the master image the
     * shrink ladder works down from, so detail here is detail in the stored
     * photo. `ideal` rather than `exact` so a laptop with only a front camera
     * still gets a stream instead of an OverconstrainedError. */
    navigator.mediaDevices.getUserMedia({
      video: { facingMode: { ideal: 'environment' }, width: { ideal: 1920 } },
      audio: false
    }).then(function (s) {
      stream = s;
      v.srcObject = s;
      v.setAttribute('playsinline', 'true');  /* iOS refuses inline play without it */
      v.muted = true;
      return v.play();
    }).then(function () {
      setState('is-live');
      setStatus(t().ready);
    }).catch(function (err) {
      fallback(t().camFailed + ' (' + (err && err.name ? err.name : 'error') + ')');
    });
  }

  /* Grab the current frame at the sensor's own resolution, then hand it to
   * the same shrink ladder a chosen file goes through -- one encoder path,
   * so the size guarantee holds however the photo arrived. */
  function shoot() {
    var v = video();
    if (!v || !v.videoWidth) return;

    var frame = document.createElement('canvas');
    frame.width = v.videoWidth;
    frame.height = v.videoHeight;
    frame.getContext('2d').drawImage(v, 0, 0, frame.width, frame.height);

    /* The camera is released as soon as the shot is taken. Retake restarts
     * it; leaving it streaming behind a still costs battery and leaves the
     * recording indicator lit for no reason. */
    stopCamera();
    setStatus(t().working);

    var out = shrink(frame);
    if (!out) { fallback(t().tooBig); return; }

    pending = out;
    var prev = document.getElementById('gt-photo-preview');
    if (prev) prev.src = out.dataUrl;
    setState('is-shot');
    setStatus(out.w + '×' + out.h + ' · ' +
              Math.round(out.bytes / 1024 * 10) / 10 + ' KB');
    var useBtn = document.getElementById('gt-photo-use');
    if (useBtn) useBtn.disabled = false;
  }

  /* Open the panel and go straight to a live camera. */
  function open() {
    show();
    var o = overlay();
    if (o) o.classList.remove('is-fallback');
    startCamera();
  }

  /* Retake returns to whichever input actually worked. */
  function retake() {
    var o = overlay();
    if (o && o.classList.contains('is-fallback')) { pick(); return; }
    startCamera();
  }

  function pick() {
    var input = document.getElementById('gt-photo-file');
    if (input) { input.value = ''; input.click(); }
  }

  function chosen(input) {
    var file = input && input.files && input.files[0];
    if (!file) return;

    if (file.type && file.type.indexOf('image/') !== 0) {
      alert(t().notImage);
      return;
    }

    show();
    setStatus(t().working);
    var prev = document.getElementById('gt-photo-preview');
    if (prev) prev.removeAttribute('src');

    decode(file).then(function (bitmap) {
      var out = shrink(bitmap);
      if (bitmap.close) bitmap.close();
      if (!out) { setStatus(t().tooBig); return; }

      pending = out;
      if (prev) prev.src = out.dataUrl;
      setState('is-shot');
      setStatus(out.w + '×' + out.h + ' · ' +
                Math.round(out.bytes / 1024 * 10) / 10 + ' KB');
      var useBtn = document.getElementById('gt-photo-use');
      if (useBtn) useBtn.disabled = false;
    }).catch(function () {
      setStatus(t().failed);
    });
  }

  function send() {
    if (!pending) return;
    setStatus(t().saving);

    var useBtn = document.getElementById('gt-photo-use');
    if (useBtn) useBtn.disabled = true;

    /* Set the named input AND pass the value to pui.submit -- the same
     * belt-and-braces the scan screen uses, because which one Profound UI
     * reads depends on how the field was bound. */
    var hidden = document.getElementById('gt-photo-data');
    if (hidden) hidden.value = pending.b64;

    var payload = pending.b64;
    if (window.pui && typeof pui.submit === 'function') {
      pui.submit({ action: 'ADDIMG', imgdata: payload });
    }
  }

  var prevPhoto = window.gtPhoto;
  if (prevPhoto && prevPhoto.__installed && prevPhoto.__version &&
      String(prevPhoto.__version) > VERSION) {
    return;                     /* a newer copy is already in charge */
  }

  window.gtPhoto = {
    __installed: true,
    __version: VERSION,
    open: open,       /* camera button -- live camera */
    shoot: shoot,     /* shutter */
    retake: retake,
    pick: pick,       /* file-input fallback only */
    chosen: chosen,
    use: send,
    close: close
  };
})();
