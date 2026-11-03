/* TS0110W - SKU list: live distribution builder.

   Everything here is browser-side help on top of the unchanged RPG: the pack
   quantities still travel to TS0110W as the subfile field SFQTY of each row,
   and F10 still runs the program's own CrtDist routine. */
(function () {
  if (!window.VW || !window.VW.ready) return;   /* shared app.js did not load: the template still works */
  var TS = window.VWts = window.VWts || {};
  var root = function () { return document.querySelector('.vw-app.vw-ts-list'); };
  var rows = function () { var r = root(); return r ? Array.prototype.slice.call(r.querySelectorAll('tr[data-sku-row]')) : []; };
  var qtyOf = function (tr) { var i = tr.querySelector('.vw-qty'); var n = parseInt(i && i.value, 10); return isNaN(n) || n < 0 ? 0 : n; };
  var money = function (n) { return '$' + n.toLocaleString('en-US', { minimumFractionDigits: 2, maximumFractionDigits: 2 }); };
  var touch = function (inp) {           /* let Profound UI's change tracking see programmatic edits */
    inp.dispatchEvent(new Event('input', { bubbles: true }));
    inp.dispatchEvent(new Event('change', { bubbles: true }));
  };

  TS.summary = function () {
    var s = { skus: 0, packs: 0, units: 0, retail: 0, over: 0, lines: [] };
    rows().forEach(function (tr) {
      var q = qtyOf(tr); var inp = tr.querySelector('.vw-qty');
      var avl = +tr.getAttribute('data-avl') || 0;
      var over = q > 0 && q > avl;
      if (inp) { inp.classList.toggle('is-over', over); inp.classList.toggle('is-set', q > 0); }
      tr.classList.toggle('is-picked', q > 0);
      if (!q) return;
      var pack = +tr.getAttribute('data-pack') || 0, price = +tr.getAttribute('data-price') || 0;
      s.skus++; s.packs += q; s.units += q * pack; s.retail += q * pack * price; if (over) s.over++;
      s.lines.push({ sku: tr.querySelector('[data-col="sku"]').getAttribute('data-v'), desc: tr.getAttribute('data-desc'), packs: q, units: q * pack, over: over });
    });
    return s;
  };
  TS.recalc = function () {
    var r = root(); if (!r) return;
    var s = TS.summary();
    var set = function (k, v) { var el = r.querySelector('[data-dist="' + k + '"]'); if (el) el.textContent = v; };
    set('skus', s.skus); set('packs', s.packs.toLocaleString('en-US')); set('units', s.units.toLocaleString('en-US')); set('retail', money(s.retail));
    set('title', s.skus ? 'Distribution to store ' + r.getAttribute('data-store') + ': ' + s.packs + ' pack' + (s.packs === 1 ? '' : 's') : 'Build a distribution');
    set('detail', s.over ? (s.over + ' SKU' + (s.over === 1 ? ' asks' : 's ask') + ' for more packs than the warehouse has available - highlighted in amber.')
      : (s.skus ? s.units.toLocaleString('en-US') + ' units at ' + money(s.retail) + ' retail. F10 sends it to the IBM i.' :
        'Key packs in the Pks column (or use Smart fill), then Create distribution. The totals update as you type.'));
    var card = r.querySelector('#vw-dist'); if (card) { card.classList.toggle('has-picks', s.skus > 0); card.classList.toggle('has-over', s.over > 0); }
    if (r.querySelector('.vw-chip[data-chip="qty"].is-on')) TS.applyChips();
    return s;
  };
  TS.step = function (btn, d) {
    var inp = btn.parentNode.querySelector('.vw-qty');
    var n = Math.max(0, Math.min(999, (parseInt(inp.value, 10) || 0) + d));
    inp.value = n ? String(n) : ''; touch(inp); TS.recalc(); inp.focus();
  };
  TS.key = function (inp, ev) {
    if (ev.key === '+' || ev.key === '=' || ev.key === '-') {
      ev.preventDefault(); TS.step(inp, ev.key === '-' ? -1 : 1); return;
    }
    if (ev.key === 'ArrowDown' || ev.key === 'ArrowUp') {
      ev.preventDefault();
      var all = rows().filter(function (tr) { return tr.offsetParent !== null; }).map(function (tr) { return tr.querySelector('.vw-qty'); });
      var i = all.indexOf(inp), nx = all[i + (ev.key === 'ArrowDown' ? 1 : -1)];
      if (nx) { nx.focus(); nx.select(); nx.closest('tr').scrollIntoView({ block: 'nearest' }); }
    }
  };
  /* Smart fill: a starting point, never a decision. Fast sellers (85%+ of
     what was shipped has sold) that the warehouse can still supply get one
     pack, very fast sellers (95%+) two - capped at the packs available.
     Rows already keyed are left alone. */
  TS.smartFill = function () {
    var n = 0, packs = 0;
    rows().forEach(function (tr) {
      var inp = tr.querySelector('.vw-qty'); if (!inp || qtyOf(tr) || tr.offsetParent === null) return;
      var pct = +tr.getAttribute('data-pct') || 0, avl = +tr.getAttribute('data-avl') || 0;
      if (pct < 85 || avl < 1) return;
      var q = Math.min(avl, pct >= 95 ? 2 : 1);
      inp.value = String(q); touch(inp); n++; packs += q;
      tr.classList.add('vw-flash'); setTimeout(function () { tr.classList.remove('vw-flash'); }, 1400);
    });
    TS.recalc();
    VW.toast(n ? 'Smart fill suggested ' + packs + ' pack' + (packs === 1 ? '' : 's') + ' on ' + n + ' fast-selling SKU' + (n === 1 ? '' : 's') + ' - review before F10.'
      : 'Nothing to suggest: no unkeyed SKU sells 85%+ with packs available.', n ? 'ok' : '');
  };
  TS.clearAll = function () {
    rows().forEach(function (tr) { var inp = tr.querySelector('.vw-qty'); if (inp && inp.value) { inp.value = ''; touch(inp); } });
    TS.recalc(); VW.toast('Pack quantities cleared');
  };
  TS.chip = function (btn) { btn.classList.toggle('is-on'); btn.setAttribute('aria-pressed', btn.classList.contains('is-on')); TS.applyChips(); };
  TS.applyChips = function () {
    var r = root(); if (!r) return;
    var on = Array.prototype.slice.call(r.querySelectorAll('.vw-chip.is-on')).map(function (c) { return c.getAttribute('data-chip'); });
    var shown = {};
    rows().forEach(function (tr) {
      var ok = on.every(function (c) {
        if (c === 'map') return tr.getAttribute('data-map') === '1';
        if (c === 'hot') return (+tr.getAttribute('data-pct') || 0) >= 85;
        if (c === 'avl') return (+tr.getAttribute('data-avl') || 0) > 0;
        if (c === 'qty') return qtyOf(tr) > 0;
        return true;
      });
      tr.classList.toggle('vw-chip-hidden', !ok);
      if (ok && !tr.classList.contains('vw-hidden')) shown[tr.getAttribute('data-grp')] = true;
    });
    var filtering = on.length || (r.querySelector('.vw-tablebar input') || {}).value;
    Array.prototype.slice.call(r.querySelectorAll('tr.vw-grp, tr.vw-tot')).forEach(function (tr) {
      var g = tr.getAttribute('data-grp');
      /* a subdept heading/total covers every class under it */
      var any = Object.keys(shown).some(function (k) { return k === g || (tr.classList.contains('vw-grp-sub') || tr.classList.contains('vw-tot-subtot')) && k.split('-')[0] === g.split('-')[0]; });
      tr.classList.toggle('vw-chip-hidden', !!filtering && !any);
    });
  };
  VW.onFilter = function (id) { if (id === 'vw-skus') TS.applyChips(); };
  TS.go = function (where) {
    var r = root(); if (!r) return;
    var wrap = r.querySelector('#vw-skuwrap'), list = rows().filter(function (tr) { return tr.offsetParent !== null; });
    var target = where === 'top' ? list[0] : list[list.length - 1];
    if (wrap) wrap.scrollTop = where === 'top' ? 0 : wrap.scrollHeight;
    if (target) { target.scrollIntoView({ block: 'center' }); var i = target.querySelector('.vw-qty'); if (i) i.focus(); }
  };
  /* F10: hand the summary to the confirmation screen (DISTWIN) */
  TS.beforeSubmit = function () {
    var r = root(); var s = TS.summary();
    s.store = r.getAttribute('data-store'); s.dept = r.getAttribute('data-dept'); s.label = r.getAttribute('data-label');
    try { sessionStorage.setItem('vw.dist', JSON.stringify(s)); } catch (e) { /* ignore */ }
  };

  VW.ready('vw-ts-list', function (r) {
    VW.remember('vw.recentTs', r.getAttribute('data-store') + '/' + r.getAttribute('data-dept'), (r.getAttribute('data-label') || '').replace(/\s+/g, ' ').trim(), 6);
    TS.recalc();
    var nbr = +r.getAttribute('data-nbr') || 1;
    if (nbr > 1) { var tr = r.querySelector('tr[data-rrn="' + nbr + '"]'); if (tr) tr.scrollIntoView({ block: 'center' }); }
    var first = r.querySelector('.vw-qty'); if (first && nbr <= 1) first.focus();
  });
})();
