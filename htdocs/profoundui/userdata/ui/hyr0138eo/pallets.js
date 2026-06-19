(function () {
  function wireFilterEnter() {
    var el = document.getElementById('ffilter');
    if (!el || el.dataset.hdWired === '1') return;
    el.dataset.hdWired = '1';
    el.addEventListener('keydown', function (ev) {
      if (ev.key === 'Enter') {
        ev.preventDefault();
        if (window.pui && typeof pui.submit === 'function') pui.submit({ action: 'SEARCH' });
      }
    });
  }
  function wireFilterSelects() {
    var el = document.getElementById('fstatus');
    if (!el || el.dataset.hdWired === '1') return;
    el.dataset.hdWired = '1';
    el.addEventListener('change', function () {
      if (window.pui && typeof pui.submit === 'function') pui.submit({ action: 'SEARCH' });
    });
  }

  function openCreate() {
    var m = document.getElementById('hd-create-modal');
    if (m) m.classList.remove('hd-hidden');
  }
  function closeCreate() {
    var m = document.getElementById('hd-create-modal');
    if (m) m.classList.add('hd-hidden');
  }
  function submitCreate() {
    var shto = Number((document.getElementById('hd-create-shto') || {}).value) || 0;
    var len  = Number((document.getElementById('hd-create-len')  || {}).value) || 48;
    var wid  = Number((document.getElementById('hd-create-wid')  || {}).value) || 40;
    var hgt  = Number((document.getElementById('hd-create-hgt')  || {}).value) || 48;
    var haz  = (document.getElementById('hd-create-haz') || {}).checked ? 'Y' : 'N';
    if (!shto) { alert('Pick a ship-to customer.'); return; }
    if (window.pui && typeof pui.submit === 'function') {
      pui.submit({
        action: 'CREATE',
        newshto: shto, newlen: len, newwid: wid, newhgt: hgt, newhzlb: haz
      });
    }
  }
  function confirmDelete(shto, psrl) {
    if (!window.confirm('Delete pallet ' + psrl + ' for ship-to ' + shto + '?')) return;
    if (window.pui && typeof pui.submit === 'function') {
      pui.submit({ action: 'DELETE', selshto: shto, selpsrl: psrl });
    }
  }

  function wireEscape() {
    if (document.hdEscWired) return;
    document.hdEscWired = true;
    document.addEventListener('keydown', function (ev) {
      if (ev.key !== 'Escape') return;
      var m = document.getElementById('hd-create-modal');
      if (m && !m.classList.contains('hd-hidden')) { ev.preventDefault(); closeCreate(); }
    });
  }

  function init() { wireFilterEnter(); wireFilterSelects(); wireEscape(); }

  window.HD_openCreate    = openCreate;
  window.HD_closeCreate   = closeCreate;
  window.HD_submitCreate  = submitCreate;
  window.HD_confirmDelete = confirmDelete;

  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', init);
  else init();
}());
