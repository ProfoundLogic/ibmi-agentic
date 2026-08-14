/* ==================================================================
 * gt-invmove -- the quantity stepper on the Inventory move screen.
 *
 * Additive, like every other screen file here: with this absent the buttons do
 * nothing and the box is still a number field the operator can type into. The
 * move itself is a plain pui.submit from an inline handler, so it never depends
 * on this file having arrived.
 *
 * There is only ONE quantity on this screen, so unlike the receiving line list
 * there is no row to resolve and no payload to collect -- the box submits under
 * its own field name and RPG reads it directly. What the stepper does need is
 * an UPPER clamp, which receiving deliberately does not have: you cannot move
 * more of something than is in the location you are moving it out of.
 * ================================================================== */

(function () {
  'use strict';

  function box() { return document.getElementById('gt-mv-qty'); }

  function setQty(value) {
    var b = box();
    if (!b) return;
    var max = Number(b.getAttribute('data-gt-max'));

    if (value < 0) value = 0;
    /* The clamp that matters. RPG re-checks it against a fresh read before
     * committing -- somebody else may have moved the stock while this screen
     * sat there -- but clamping here means the operator is never offered a
     * number that is going to be refused. */
    if (!isNaN(max) && max >= 0 && value > max) value = max;

    b.value = (Math.round(value * 100) / 100).toFixed(2);
    try { b.dispatchEvent(new Event('input', { bubbles: true })); } catch (e) {}

    /* A short haptic, no beep: the scan beep is the one that matters and a
     * beep per tap is how an operator ends up turning the sound off. */
    try { if (navigator.vibrate) navigator.vibrate(30); } catch (e) {}
  }

  function step(dir) {
    var b = box();
    if (!b) return;
    var by = Number(b.getAttribute('data-gt-step')) || 1;
    setQty((Number(b.value) || 0) + (dir * by));
  }

  /* Everything available -- the common case for a consolidation or a
   * relocation, where the whole lot goes to the new home. */
  function all() {
    var b = box();
    if (!b) return;
    setQty(Number(b.getAttribute('data-gt-max')) || 0);
  }

  /* Select the whole value on focus: an operator correcting 116 to 100 wants
   * to type the new number, not position a caret inside the old one. */
  function bind() {
    var b = box();
    if (!b) return false;
    if (b.getAttribute('data-gt-bound') === '1') return true;
    b.setAttribute('data-gt-bound', '1');
    b.addEventListener('focus', function () {
      setTimeout(function () { try { b.select(); } catch (e) {} }, 0);
    });
    return true;
  }

  function init(attempt) {
    if (bind()) return;
    if (attempt > 80) return;
    setTimeout(function () { init(attempt + 1); }, 50);
  }

  window.gtInvMove = {
    __installed: true,
    step: step,
    all: all,
    rebind: function () { init(0); }
  };

  init(0);
})();
