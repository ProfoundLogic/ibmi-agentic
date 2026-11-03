/* INV046W - inventory by store: remember the SKU, keep the number picker's
   suggestions in step with "Search for", focus the first empty field. */
(function () {
  if (!window.VW || !window.VW.ready) return;   /* shared app.js did not load: the template still works */
  VW.ready('vw-inv046', function (root) {
    VW.remember('vw.recentSkus', root.getAttribute('data-sku'), (root.getAttribute('data-desc') || '').trim());
    var sku = root.querySelector('#vw-inumbr'), num = root.querySelector('#vw-instr');
    var err = root.querySelector('.vw-input.is-error');
    var target = err || (sku && !sku.value ? sku : (num && !num.value ? num : null));
    if (target) { target.focus(); target.select(); }
    root.querySelectorAll('.vw-picker-list').forEach(function (l) { l.hidden = true; });
  });
})();
