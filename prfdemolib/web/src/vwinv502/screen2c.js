/* INV502W - SKU detail: remember the SKU for the "recently viewed" list,
   select the lookup box so a new number overwrites it. */
(function () {
  if (!window.VW || !window.VW.ready) return;   /* shared app.js did not load: the template still works */
  VW.ready('vw-inv502-s2', function (root) {
    VW.remember('vw.recentSkus', root.getAttribute('data-sku'), (root.getAttribute('data-desc') || '').trim());
    var i = root.querySelector('#vw-newsku');
    if (i) { i.focus(); i.select(); }
  });
})();
