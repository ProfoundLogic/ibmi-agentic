/* ==================================================================
 * gt-cntentry -- the blind count's quantity controls.
 *
 * Additive: with this file absent the boxes are still number inputs the
 * counter can type into, and Save still submits.
 *
 * The collector is gt-edits.js, shared with receiving. What is specific here is
 * the FORCE flag: an uncounted line whose box already reads 0.00 must still be
 * sent when the counter confirms it, because "I counted it and there are none"
 * is a finding and "nobody has been there yet" is not. Receiving has no
 * equivalent -- an untouched line there is simply untouched.
 * ================================================================== */

(function () {
  'use strict';

  var EDITS = {
    root: '.gt-cntentry-screen', box: '.gt-ce-box', number: true,
    hiddenId: 'gt-cntedits', field: 'cntedits', action: 'APPLY'
  };

  function boxFor(btn) {
    var row = btn.closest ? btn.closest('.gt-ce-row') : null;
    return row ? row.querySelector('.gt-ce-box') : null;
  }

  function setQty(box, value) {
    if (value < 0) value = 0;
    box.value = (Math.round(value * 100) / 100).toFixed(2);
    /* Touching a line counts it, so it must be sent even if the number did not
     * change. */
    box.setAttribute('data-gt-force', '1');
    try { box.dispatchEvent(new Event('input', { bubbles: true })); } catch (e) {}
    try { if (navigator.vibrate) navigator.vibrate(30); } catch (e) {}
  }

  function step(btn, dir) {
    var box = boxFor(btn);
    if (!box) return;
    var by = Number(box.getAttribute('data-gt-step')) || 1;
    setQty(box, (Number(box.value) || 0) + (dir * by));
  }

  /* Counted, and there are none. Not the same as not counted. */
  function zero(btn) {
    var box = boxFor(btn);
    if (box) setQty(box, 0);
  }

  function save() {
    if (window.gtEdits) { gtEdits.submit(EDITS); return; }
    if (window.pui && typeof pui.submit === 'function') {
      pui.submit({ action: 'APPLY' });
    }
  }

  function revealHit(el) {
    var line = el.getAttribute('data-gt-hit');
    if (!line || Number(line) <= 0) return;
    var row = document.getElementById('gt-ce-' + Number(line));
    if (!row) return;
    try { row.scrollIntoView({ behavior: 'smooth', block: 'center' }); }
    catch (e) { row.scrollIntoView(); }
    /* A scan gets the beep as well as the haptic: it is the signal that
     * matters when the counter is not looking at the screen. */
    try { if (navigator.vibrate) navigator.vibrate(60); } catch (e) {}
    try {
      var Ctx = window.AudioContext || window.webkitAudioContext;
      if (!Ctx) return;
      var ctx = new Ctx(), osc = ctx.createOscillator(), g = ctx.createGain();
      osc.frequency.value = 1180; g.gain.value = 0.06;
      osc.connect(g); g.connect(ctx.destination); osc.start();
      setTimeout(function () { osc.stop(); ctx.close(); }, 110);
    } catch (e) {}
  }

  function bind() {
    var el = document.querySelector('.gt-cntentry-screen');
    if (!el) return false;
    if (el.getAttribute('data-gt-cnt-bound') === '1') return true;
    el.setAttribute('data-gt-cnt-bound', '1');

    var boxes = el.querySelectorAll('.gt-ce-box');
    for (var i = 0; i < boxes.length; i++) {
      boxes[i].addEventListener('focus', function () {
        var b = this;
        /* Typing into a line counts it. */
        b.setAttribute('data-gt-force', '1');
        setTimeout(function () { try { b.select(); } catch (e) {} }, 0);
      });
    }
    revealHit(el);
    return true;
  }

  function init(a) {
    if (bind()) return;
    if (a > 80) return;
    setTimeout(function () { init(a + 1); }, 50);
  }

  window.gtCntEntry = {
    __installed: true, step: step, zero: zero, save: save,
    collect: function () { return window.gtEdits ? gtEdits.collect(EDITS) : ''; },
    rebind: function () { init(0); }
  };

  init(0);
})();
