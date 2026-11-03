/* INV502W - SKU prompt: put the cursor in the SKU box, select the text so a
   new number overwrites it, and close the finder until the user types. */
(function () {
  if (!window.VW || !window.VW.ready) return;   /* shared app.js did not load: the template still works */
  VW.ready('vw-inv502-s1', function (root) {
    var i = root.querySelector('#vw-sku');
    if (i) { i.focus(); i.select(); var l = root.querySelector('.vw-picker-list'); if (l) l.hidden = true; }
  });
})();
