/* ==================================================================
 * gt-carousel -- swipeable product images.
 *
 * The swipe itself is CSS scroll-snap, not JavaScript. That is deliberate:
 * native scrolling is smoother than anything hand-rolled, it respects the
 * platform's momentum and rubber-banding, and it means the carousel WORKS
 * WITH NO JAVASCRIPT AT ALL -- which matters here, because screen JS is
 * delivered through the Genie shim and could go stale.
 *
 * This file adds only the parts CSS cannot do:
 *   - keeping the dot indicators in step with the scroll position
 *   - tap-a-dot to jump
 *   - arrow buttons for mouse users on desktop
 *
 * If it never runs, you still get a horizontally swipeable strip of images.
 * You just lose the dots.
 *
 * Re-entrancy: screen JS re-executes on every render, so binding is guarded
 * by a data attribute on the root and all listeners live inside the screen
 * container, which is wiped between renders.
 * ================================================================== */

(function () {
  'use strict';

  if (window.gtCarousel && window.gtCarousel.__installed) {
    window.gtCarousel.rebind();
    return;
  }

  function bindOne(root) {
    if (root.getAttribute('data-gt-car') === '1') return;
    root.setAttribute('data-gt-car', '1');

    var strip = root.querySelector('.gt-car-strip');
    var dots = [].slice.call(root.querySelectorAll('.gt-car-dot'));
    var prev = root.querySelector('.gt-car-prev');
    var next = root.querySelector('.gt-car-next');
    if (!strip) return;

    var slides = [].slice.call(strip.querySelectorAll('.gt-car-slide'));
    if (!slides.length) return;

    function current() {
      /* Nearest slide to the current scroll offset. Simpler and steadier
       * than tracking touch events, and it stays correct if the user flings
       * past several images. */
      var best = 0;
      var bestDist = Infinity;
      for (var i = 0; i < slides.length; i++) {
        var d = Math.abs(slides[i].offsetLeft - strip.scrollLeft);
        if (d < bestDist) { bestDist = d; best = i; }
      }
      return best;
    }

    function paint() {
      var idx = current();
      for (var i = 0; i < dots.length; i++) {
        dots[i].classList.toggle('is-on', i === idx);
      }
      if (prev) prev.disabled = idx === 0;
      if (next) next.disabled = idx === slides.length - 1;
    }

    function goTo(i) {
      var t = Math.max(0, Math.min(slides.length - 1, i));
      strip.scrollTo({ left: slides[t].offsetLeft, behavior: 'smooth' });
    }

    /* rAF-throttled: scroll fires far more often than the dots need updating. */
    var ticking = false;
    strip.addEventListener('scroll', function () {
      if (ticking) return;
      ticking = true;
      requestAnimationFrame(function () { ticking = false; paint(); });
    }, { passive: true });

    dots.forEach(function (dot, i) {
      dot.addEventListener('click', function () { goTo(i); });
    });
    if (prev) prev.addEventListener('click', function () { goTo(current() - 1); });
    if (next) next.addEventListener('click', function () { goTo(current() + 1); });

    paint();
  }

  function bindAll() {
    var roots = document.querySelectorAll('.gt-car');
    if (!roots.length) return false;
    [].forEach.call(roots, bindOne);
    return true;
  }

  function init(attempt) {
    if (bindAll()) return;
    if (attempt > 80) return;
    setTimeout(function () { init(attempt + 1); }, 50);
  }

  window.gtCarousel = { __installed: true, rebind: function () { init(0); } };
  init(0);
})();
