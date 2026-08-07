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
 * WHY <input type="file" capture> AND NOT getUserMedia
 * ----------------------------------------------------
 * gt-scan.js uses getUserMedia because a barcode scanner needs a live frame
 * loop. A photograph does not. The file input hands over to the platform's
 * own camera app, which means autofocus, exposure, HDR, the flash, tap to
 * focus, and pinch to zoom -- all of it for free, all of it already familiar,
 * and a much better picture than a <video> frame grab. It also degrades to a
 * file picker on a desktop, which is exactly what you want when demoing from
 * a laptop with no camera.
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
          failed: 'The photo could not be read.' },
    FR: { title: 'Ajouter une photo', use: 'Utiliser cette photo',
          retake: 'Reprendre', cancel: 'Annuler',
          working: 'Préparation de la photo…',
          saving: 'Enregistrement…',
          tooBig: 'Cette photo n’a pas pu être réduite suffisamment.',
          notImage: 'Ce fichier n’est pas une image.',
          failed: 'La photo n’a pas pu être lue.' }
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

  function overlay() { return document.getElementById('gt-photo-overlay'); }

  function setStatus(text) {
    var el = document.getElementById('gt-photo-status');
    if (el) el.textContent = text || '';
  }

  function show() {
    var o = overlay();
    if (o) o.classList.add('is-open');
  }

  function close() {
    var o = overlay();
    if (o) o.classList.remove('is-open');
    pending = null;
    var prev = document.getElementById('gt-photo-preview');
    if (prev) prev.removeAttribute('src');
    setStatus('');
    var input = document.getElementById('gt-photo-file');
    /* Clearing lets the SAME file be chosen again -- otherwise "retake",
     * then picking the identical shot, fires no change event at all. */
    if (input) input.value = '';
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

  window.gtPhoto = {
    __installed: true,
    pick: pick,
    chosen: chosen,
    use: send,
    close: close
  };
})();
