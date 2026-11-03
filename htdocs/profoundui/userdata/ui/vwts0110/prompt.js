/* TS0110W - prompt: focus the first field that needs attention. */
(function () {
  if (!window.VW || !window.VW.ready) return;   /* shared app.js did not load: the template still works */
  VW.ready('vw-ts-prompt', function (root) {
    var err = root.querySelector('.vw-input.is-error');
    var st = root.querySelector('#vw-dfstor'), dp = root.querySelector('#vw-dfdept');
    var t = err || (st && !st.value ? st : (dp && !dp.value ? dp : st));
    if (t) { t.focus(); t.select(); }
    root.querySelectorAll('.vw-picker-list').forEach(function (l) { l.hidden = true; });
  });
})();
