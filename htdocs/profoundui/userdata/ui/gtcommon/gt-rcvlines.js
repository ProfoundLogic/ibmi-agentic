/* ==================================================================
 * gt-rcvlines -- the receiving line list's own behaviour.
 *
 * Everything here is ADDITIVE. The screen is fully usable with this file
 * absent: the cards render, the quantity boxes accept input, Save and Confirm
 * all submit, and the Post button falls back to a plain confirm() in its
 * inline handler. Screen JS arrives through the Genie shim and can go stale,
 * so nothing the operator needs may depend on it.
 *
 * Five jobs CSS and RPG cannot do between them:
 *
 *   1. The - and + stepper either side of each quantity box, and "receive in
 *      full". All three act on the box locally: stepping 48 cases must not be
 *      48 round trips, so the operator adjusts freely and Save quantities
 *      commits the lot in one submit.
 *   1a. Gathering those changes into the qtyedits payload -- see collect()
 *      below for why a READC cannot do this job in an EJS screen.
 *   2. Scroll the line a scan just landed on into view. RPG knows which line
 *      moved and puts it in HITLINE; only the browser can scroll to it.
 *   3. Confirm the post, because posting is irreversible.
 *   4. Feedback on a scan -- beep and haptic -- so a receiver working by feel
 *      knows it landed without reading the screen.
 *
 * Handlers are inline attributes calling these globals rather than
 * addEventListener bindings, because screen JS runs BEFORE the template has
 * rendered: there is nothing to bind to yet and the failure is silent. The
 * post-render work below therefore polls for its root, the same way
 * gt-carousel.js and gt-scan.js do.
 * ================================================================== */

(function () {
  'use strict';

  var T = {
    EN: { postConfirm: 'Post this receipt? Stock goes to staging and the ' +
                       'receipt closes.' },
    FR: { postConfirm: 'Valider cette réception ? Le stock va en transit et ' +
                       'la réception se ferme.' }
  };

  function root() { return document.querySelector('.gt-rcvlines-screen'); }

  function lang() {
    var el = root();
    var v = el ? (el.getAttribute('data-gt-lang') || 'EN').toUpperCase() : 'EN';
    return T[v] ? v : 'EN';
  }
  function t() { return T[lang()]; }

  /* ---- The quantity stepper ---------------------------------------
   * Both of these work on the box next to the button that was pressed and do
   * NOT round-trip to RPG. A tap per case at 48 cases would be 48 round trips;
   * the operator adjusts freely and then presses Save quantities once, which
   * collects every changed box into a single payload -- see collect().
   *
   * Degradation: with this file absent the buttons do nothing, and the box is
   * still a plain number input the operator can type into. The screen stays
   * usable, which is the rule for everything delivered through the shim.
   * ---------------------------------------------------------------- */
  function boxFor(btn) {
    var wrap = btn.closest ? btn.closest('.gt-lin-step') : null;
    return wrap ? wrap.querySelector('.gt-lin-qty') : null;
  }

  function setQty(box, value) {
    /* Never below zero. There is no upper clamp: an overage is a real thing a
     * receiver has to be able to record, and the line goes OVER for it. */
    if (value < 0) value = 0;
    /* Two decimals always, matching the zoned(11:2) subfile field the value
     * is read back into. */
    box.value = (Math.round(value * 100) / 100).toFixed(2);
    /* Let anything listening know, and make the change visible as a change. */
    try { box.dispatchEvent(new Event('input', { bubbles: true })); } catch (e) {}
  }

  function step(btn, dir) {
    var box = boxFor(btn);
    if (!box) return;
    var by = Number(box.getAttribute('data-gt-step')) || 1;
    setQty(box, (Number(box.value) || 0) + (dir * by));
    feedback(30);
  }

  /* Receive this line in full: straight to the expected quantity. */
  function full(btn) {
    var row = btn.closest ? btn.closest('.gt-lin-row') : null;
    var box = row ? row.querySelector('.gt-lin-qty') : null;
    if (!box) return;
    setQty(box, Number(box.getAttribute('data-gt-exp')) || 0);
    feedback(45);
  }

  /* ---- Saving the quantities --------------------------------------
   * WHY THIS IS NOT A READC.
   *
   * In an EJS Rich Display screen the subfile rows are rendered by the
   * TEMPLATE, not by Profound UI grid widgets. Every quantity box therefore
   * carries the same name="lqty" with no record number attached, and the
   * runtime derives its {SUBFILE}.rrn changed-record marker from its own grid
   * widgets -- of which there are none here. So RPG's READC sees nothing,
   * whether the value was typed, stepped or set by "in full", and the screen
   * answered "no quantities were changed" every time.
   *
   * The changed rows travel in one field instead, "seq:qty;seq:qty;", with the
   * row index encoded the same way this application already encodes it in its
   * PICKnn action codes. Only rows that actually differ from what the server
   * rendered are sent, so the payload stays small and RPG still does not stamp
   * updated_by across untouched lines.
   * ---------------------------------------------------------------- */
  function collect() {
    var out = [];
    var boxes = document.querySelectorAll('.gt-rcvlines-screen .gt-lin-qty');
    for (var i = 0; i < boxes.length; i++) {
      var box = boxes[i];
      var seq = box.getAttribute('data-gt-seq');
      var orig = box.getAttribute('data-gt-orig');
      if (!seq) continue;
      /* An empty box is not a zero -- it is somebody mid-edit, or a value the
       * browser rejected. Skipping it is the safe reading; writing 0 would
       * silently receive nothing for that line. */
      if (box.value === '' || box.value === null) continue;
      var now = (Math.round(Number(box.value) * 100) / 100).toFixed(2);
      if (now !== orig) out.push(seq + ':' + now);
    }
    return out.length ? out.join(';') + ';' : '';
  }

  function save() {
    var payload = collect();
    var hidden = document.getElementById('gt-qtyedits');
    /* Set the named input AND pass it to pui.submit -- the same
     * belt-and-braces the photo capture uses, because which one the runtime
     * reads depends on how the field was bound. */
    if (hidden) hidden.value = payload;
    if (window.pui && typeof pui.submit === 'function') {
      pui.submit({ action: 'APPLY', qtyedits: payload });
    }
  }

  /* ---- Posting ---------------------------------------------------- */
  function post() {
    if (!window.confirm(t().postConfirm)) return;
    if (window.pui && typeof pui.submit === 'function') {
      pui.submit({ action: 'POST' });
    }
  }

  /* ---- The line a scan just landed on ----------------------------
   * A warehouse is loud and bright and the device may be held at waist
   * height, so a good scan is signalled three ways: the CSS flash on the row,
   * a short beep, and a haptic tick. Any of the three can be unavailable
   * without the others failing.
   * ---------------------------------------------------------------- */
  /* ms given -> a silent haptic tick only, for a stepper tap. A beep on every
   * one of 48 taps would have the operator turn the sound off, and then the
   * scan beep -- which is the one that matters -- goes with it. */
  function feedback(ms) {
    try { if (navigator.vibrate) navigator.vibrate(ms || 60); } catch (e) {}
    if (ms) return;
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
  }

  function revealHit(el) {
    var line = el.getAttribute('data-gt-hit');
    if (!line || Number(line) <= 0) return;

    var row = document.getElementById('gt-lin-' + Number(line));
    if (!row) return;

    /* 'center' rather than the default: the summary bar is sticky, and
     * scrolling a row to the very top puts it underneath. */
    try { row.scrollIntoView({ behavior: 'smooth', block: 'center' }); }
    catch (e) { row.scrollIntoView(); }

    feedback();
  }

  /* Select the whole value on focus. A receiver correcting 24 to 22 wants to
   * type the new number, not position a caret inside the old one. */
  function bindQtyBoxes(el) {
    var boxes = el.querySelectorAll('.gt-lin-qty');
    for (var i = 0; i < boxes.length; i++) {
      boxes[i].addEventListener('focus', function () {
        var box = this;
        setTimeout(function () { try { box.select(); } catch (e) {} }, 0);
      });
    }
  }

  function bind() {
    var el = root();
    if (!el) return false;
    /* Screen JS re-executes on every render, including subfile reloads, so
     * binding is guarded on the root -- which is itself replaced between
     * renders, so the guard resets exactly when it should. */
    if (el.getAttribute('data-gt-rcv-bound') === '1') return true;
    el.setAttribute('data-gt-rcv-bound', '1');

    bindQtyBoxes(el);
    revealHit(el);
    return true;
  }

  function init(attempt) {
    if (bind()) return;
    if (attempt > 80) return;
    setTimeout(function () { init(attempt + 1); }, 50);
  }

  window.gtRcvLines = {
    __installed: true,
    step: step,     /* the - and + buttons either side of the quantity box */
    full: full,     /* receive this line in full */
    save: save,     /* gather the changed boxes and submit them */
    collect: collect,
    post: post,
    rebind: function () { init(0); }
  };

  init(0);
})();
