(function () {
  function wire() {
    ['fgroup', 'fitem'].forEach(function (id) {
      var el = document.getElementById(id);
      if (!el || el.dataset.hdWired === '1') return;
      el.dataset.hdWired = '1';
      el.addEventListener('keydown', function (ev) {
        if (ev.key === 'Enter') {
          ev.preventDefault();
          if (window.pui && typeof pui.submit === 'function') pui.submit({ action: 'SEARCH' });
        }
      });
    });
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', wire);
  else wire();
}());
