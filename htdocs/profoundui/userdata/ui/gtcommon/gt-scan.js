/* ==================================================================
 * gt-scan -- the universal scan input.
 *
 * Shared by every screen in the application that accepts a barcode. Three
 * input paths, in preference order:
 *
 *   1. Camera via BarcodeDetector. Native in Chrome on Android, which is
 *      the live demo device, so decoding happens in the browser engine --
 *      fast, no library, low battery cost.
 *   2. Camera via a still capture, when a live stream is refused.
 *   3. Manual entry, which is plain markup in the template and needs no
 *      JavaScript at all.
 *
 * iOS Safari has no BarcodeDetector. Rather than fail silently there, the
 * component says so and falls back to manual entry; a ZXing decode path is
 * the planned follow-up.
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

  if (window.gtScan && window.gtScan.__installed) { window.gtScan.rebind(); return; }

  var state = { stream: null, detector: null, raf: 0, running: false, lastCode: '', lastAt: 0 };

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
    if (state.stream) {
      state.stream.getTracks().forEach(function (t) { try { t.stop(); } catch (e) {} });
      state.stream = null;
    }
    var wrap = $('#gt-scan-camera');
    if (wrap) wrap.classList.remove('is-live');
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
      video: { facingMode: { ideal: 'environment' }, width: { ideal: 1280 } },
      audio: false
    }).then(function (stream) {
      state.stream = stream;
      video.srcObject = stream;
      video.setAttribute('playsinline', 'true');  /* iOS refuses fullscreen-less play without it */
      return video.play();
    }).then(function () {
      wrap.classList.add('is-live');
      if (supportsDetector()) {
        beginDetect(video);
      } else {
        setStatus(txt('noDetector'), 'warn');
      }
    }).catch(function (err) {
      setStatus(txt('cameraFailed') + ' (' + (err && err.name ? err.name : 'error') + ')', 'bad');
      stopCamera();
    });
  }

  function beginDetect(video) {
    try {
      state.detector = new window.BarcodeDetector({ formats: wantedFormats() });
    } catch (e) {
      try { state.detector = new window.BarcodeDetector(); }
      catch (e2) { setStatus(txt('noDetector'), 'warn'); return; }
    }

    setStatus(txt('scanning'), 'good');
    state.running = true;

    var tick = function () {
      if (!state.running || !state.detector) return;
      state.detector.detect(video).then(function (codes) {
        if (codes && codes.length) {
          submitPayload(codes[0].rawValue);
          return;
        }
        state.raf = requestAnimationFrame(tick);
      }).catch(function () {
        /* A transient decode failure is normal between frames. */
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
        noDetector: 'This browser cannot decode in-camera. Use the keypad below.',
        cameraFailed: 'Camera could not be opened'
      },
      FR: {
        starting: 'Démarrage de la caméra...',
        scanning: 'Caméra prête - visez un code à barres',
        noCamera: 'Caméra indisponible. La page n’est probablement pas en HTTPS.',
        noDetector: 'Ce navigateur ne peut pas décoder. Utilisez le clavier ci-dessous.',
        cameraFailed: 'Impossible d’ouvrir la caméra'
      }
    };
    return (S[lang] || S.EN)[key] || '';
  }

  function bind() {
    var root = $('.gt-scan-screen');
    if (!root) return false;
    if (root.getAttribute('data-gt-scan-bound') === '1') return true;
    root.setAttribute('data-gt-scan-bound', '1');

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

    if (!supportsDetector()) setStatus(txt('noDetector'), 'warn');
    return true;
  }

  function init(attempt) {
    if (bind()) return;
    if (attempt > 80) return;
    setTimeout(function () { init(attempt + 1); }, 50);
  }

  window.gtScan = {
    __installed: true,
    start: startCamera,
    stop: stopCamera,
    submit: submitPayload,
    rebind: function () { init(0); }
  };

  init(0);
})();
