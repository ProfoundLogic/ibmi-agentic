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
    ['fstatus', 'floc'].forEach(function (id) {
      var el = document.getElementById(id);
      if (!el || el.dataset.hdWired === '1') return;
      el.dataset.hdWired = '1';
      el.addEventListener('change', function () {
        if (window.pui && typeof pui.submit === 'function') pui.submit({ action: 'SEARCH' });
      });
    });
  }
  function renderDetail() {
    var host = document.querySelector('.hd-detail-rows');
    if (!host) return;
    var json = host.getAttribute('data-rows') || '[]';
    var rows;
    try { rows = JSON.parse(json); }
    catch (e) {
      host.innerHTML = '<div class="hd-detail-empty">Could not parse detail data.</div>';
      return;
    }
    if (!Array.isArray(rows) || rows.length === 0) {
      host.innerHTML = '<div class="hd-detail-empty">No shipment lines for this group.</div>';
      return;
    }
    var html = ['<table>',
      '<thead><tr>',
      '<th>Item</th><th>Description</th>',
      '<th>Order #</th><th>Line</th><th>Lot</th>',
      '<th style="text-align:right">Open</th>',
      '<th style="text-align:right">To Pack</th>',
      '<th style="text-align:right">Picked</th>',
      '<th>Pallet loc</th>',
      '</tr></thead><tbody>'];
    rows.forEach(function (r) {
      html.push('<tr>' +
        '<td class="hd-mono">' + esc(r.item) + '</td>' +
        '<td>' + esc(r.desc) + '</td>' +
        '<td class="hd-mono">' + r.ord + '</td>' +
        '<td class="hd-mono">' + r.ordl + '</td>' +
        '<td class="hd-mono">' + esc(r.lot) + '</td>' +
        '<td class="hd-mono" style="text-align:right">' + r.qopen + '</td>' +
        '<td class="hd-mono" style="text-align:right">' + r.qpack + '</td>' +
        '<td class="hd-mono" style="text-align:right">' + r.qstoc + '</td>' +
        '<td class="hd-mono">' + esc(r.ploc) + '</td>' +
        '</tr>');
    });
    html.push('</tbody></table>');
    host.innerHTML = html.join('');
  }
  function esc(v) {
    return String(v == null ? '' : v).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
  }
  function init() { wireFilterEnter(); wireFilterSelects(); renderDetail(); }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', init);
  else init();
}());
