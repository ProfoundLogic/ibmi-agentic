(function () {
  // ---- filter wiring ----------------------------------------------------
  function wireFilterEnter() {
    var el = document.getElementById('ffilter');
    if (!el || el.dataset.hdWired === '1') return;
    el.dataset.hdWired = '1';
    el.addEventListener('keydown', function (ev) {
      if (ev.key === 'Enter') {
        ev.preventDefault();
        if (window.pui && typeof pui.submit === 'function') {
          pui.submit({ action: 'SEARCH' });
        }
      }
    });
  }
  function wireFilterSelects() {
    ['fstatus', 'floc'].forEach(function (id) {
      var el = document.getElementById(id);
      if (!el || el.dataset.hdWired === '1') return;
      el.dataset.hdWired = '1';
      el.addEventListener('change', function () {
        if (window.pui && typeof pui.submit === 'function') {
          pui.submit({ action: 'SEARCH' });
        }
      });
    });
  }

  // ---- assign-picker modal ---------------------------------------------
  function openAssign(batch, currentPicker) {
    var modal = document.getElementById('hd-assign-modal');
    if (!modal) return;
    modal.dataset.batch = String(batch);
    var batchEl = document.getElementById('hd-assign-batch');
    if (batchEl) batchEl.textContent = '#' + batch;
    var input = document.getElementById('hd-assign-picker');
    if (input) {
      input.value = currentPicker || '';
      modal.classList.remove('hd-hidden');
      setTimeout(function () { input.focus(); input.select(); }, 0);
    } else {
      modal.classList.remove('hd-hidden');
    }
  }
  function closeAssign() {
    var modal = document.getElementById('hd-assign-modal');
    if (modal) modal.classList.add('hd-hidden');
  }
  function submitAssign() {
    var modal = document.getElementById('hd-assign-modal');
    var input = document.getElementById('hd-assign-picker');
    if (!modal || !input) return;
    var batch = Number(modal.dataset.batch) || 0;
    var picker = (input.value || '').trim().toUpperCase();
    if (!picker) {
      input.focus();
      return;
    }
    if (window.pui && typeof pui.submit === 'function') {
      pui.submit({ action: 'ASSIGN', selbatch: batch, newpicker: picker });
    }
  }
  function wireAssignEnter() {
    var input = document.getElementById('hd-assign-picker');
    if (!input || input.dataset.hdWired === '1') return;
    input.dataset.hdWired = '1';
    input.addEventListener('keydown', function (ev) {
      if (ev.key === 'Enter') { ev.preventDefault(); submitAssign(); }
      else if (ev.key === 'Escape') { ev.preventDefault(); closeAssign(); }
    });
  }

  // ---- delete confirm --------------------------------------------------
  function confirmDelete(batch) {
    var ok = window.confirm('Permanently delete batch ' + batch + ' and its lines?');
    if (!ok) return;
    if (window.pui && typeof pui.submit === 'function') {
      pui.submit({ action: 'DELETE', selbatch: batch });
    }
  }

  // ---- mega-batch selection + create ----------------------------------
  function selectedBatches() {
    return Array.prototype.slice
      .call(document.querySelectorAll('.hd-row-check:checked'))
      .map(function (c) { return c.getAttribute('data-batch'); })
      .filter(function (b) { return b && Number(b) > 0; });
  }
  function refreshMegabar() {
    var bar    = document.getElementById('hd-megabar');
    var countS = document.getElementById('hd-megabar-count');
    var btn    = document.getElementById('hd-megabar-create');
    if (!bar) return;
    var sel = selectedBatches();
    var n = sel.length;
    if (n === 0) {
      bar.classList.add('hd-hidden');
      return;
    }
    bar.classList.remove('hd-hidden');
    if (countS) countS.textContent = String(n);
    if (btn) btn.disabled = (n < 2);
  }
  function wireRowChecks() {
    Array.prototype.forEach.call(document.querySelectorAll('.hd-row-check'), function (c) {
      if (c.dataset.hdWired === '1') return;
      c.dataset.hdWired = '1';
      c.addEventListener('change', refreshMegabar);
    });
    var all = document.getElementById('hd-check-all');
    if (all && all.dataset.hdWired !== '1') {
      all.dataset.hdWired = '1';
      all.addEventListener('change', function () {
        Array.prototype.forEach.call(document.querySelectorAll('.hd-row-check'), function (c) {
          c.checked = all.checked;
        });
        refreshMegabar();
      });
    }
  }
  function clearSelections() {
    Array.prototype.forEach.call(document.querySelectorAll('.hd-row-check'), function (c) {
      c.checked = false;
    });
    var all = document.getElementById('hd-check-all');
    if (all) all.checked = false;
    refreshMegabar();
  }
  function createMega() {
    var sel = selectedBatches();
    if (sel.length < 2) return;
    var ok = window.confirm(
      'Bundle ' + sel.length + ' batches into one mega batch?\n\n' +
      'Batches: ' + sel.join(', '));
    if (!ok) return;
    if (window.pui && typeof pui.submit === 'function') {
      pui.submit({ action: 'CREATE_MEGA', selbatches: sel.join(',') });
    }
  }
  function confirmDissolve(megaBatch /*, kidCount */) {
    var ok = window.confirm(
      'Dissolve mega batch ' + megaBatch + '?\n\n' +
      'Component batches are released back to the regular grid. ' +
      'No pick-line data is lost.');
    if (!ok) return;
    if (window.pui && typeof pui.submit === 'function') {
      pui.submit({ action: 'DISSOLVE_MEGA', selbatch: megaBatch });
    }
  }

  // ---- mega-kids: render the children list inside the view modal ------
  function renderMegaKids() {
    var host = document.querySelector('.hd-mega-kids');
    if (!host) return;
    var json = host.getAttribute('data-kids') || '[]';
    var kids;
    try { kids = JSON.parse(json); }
    catch (e) {
      host.innerHTML = '<div class="hd-detail-empty">Could not parse kids.</div>';
      return;
    }
    if (!Array.isArray(kids) || kids.length === 0) {
      host.innerHTML = '<div class="hd-detail-empty">No constituent batches found.</div>';
      return;
    }
    var html = ['<table class="hd-mega-kids-table">',
      '<thead><tr><th>Batch #</th><th>Picker</th><th>DC</th><th>Status</th>',
      '<th style="text-align:right">Items</th>',
      '<th style="text-align:right">Pieces</th></tr></thead>',
      '<tbody>'];
    kids.forEach(function (k) {
      html.push('<tr>' +
        '<td class="hd-mono">' + k.batch + '</td>' +
        '<td class="hd-mono">' + escapeHtml(k.picker) + '</td>' +
        '<td>' + escapeHtml(k.loc) + '</td>' +
        '<td>' + escapeHtml(k.stat) + '</td>' +
        '<td class="hd-mono" style="text-align:right">' + k.items + '</td>' +
        '<td class="hd-mono" style="text-align:right">' + k.pieces + '</td>' +
        '</tr>');
    });
    html.push('</tbody></table>');
    host.innerHTML = html.join('');
  }

  // ---- detail modal: parse JSON payload + render table -----------------
  function renderDetailRows() {
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
      host.innerHTML = '<div class="hd-detail-empty">No pick lines for this batch.</div>';
      return;
    }
    var html = ['<table>',
      '<thead><tr>',
      '<th>Seq</th>',
      '<th>Item</th>',
      '<th>Aisle</th>',
      '<th>Location</th>',
      '<th>Order #</th>',
      '<th style="text-align:right">Need</th>',
      '<th style="text-align:right">Picked</th>',
      '</tr></thead>',
      '<tbody>'];
    rows.forEach(function (r) {
      html.push(
        '<tr>' +
        '<td class="hd-mono">' + r.seq + '</td>' +
        '<td class="hd-mono">' + escapeHtml(r.item) + '</td>' +
        '<td>' + escapeHtml(r.aisle) + '</td>' +
        '<td>' + escapeHtml(r.loc) + '</td>' +
        '<td class="hd-mono">' + r.ord + '</td>' +
        '<td class="hd-mono" style="text-align:right">' + r.need + '</td>' +
        '<td class="hd-mono" style="text-align:right">' + (r.qtyp || 0) + '</td>' +
        '</tr>'
      );
    });
    html.push('</tbody></table>');
    host.innerHTML = html.join('');
  }
  function escapeHtml(v) {
    return String(v == null ? '' : v)
      .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
  }

  // ---- close modal on Escape ------------------------------------------
  function wireEscape() {
    if (document.hdEscWired) return;
    document.hdEscWired = true;
    document.addEventListener('keydown', function (ev) {
      if (ev.key !== 'Escape') return;
      var assignModal = document.getElementById('hd-assign-modal');
      if (assignModal && !assignModal.classList.contains('hd-hidden')) {
        ev.preventDefault();
        closeAssign();
        return;
      }
      var viewModal = document.querySelector('[data-hd-modal="view"]');
      if (viewModal && window.pui && typeof pui.submit === 'function') {
        ev.preventDefault();
        pui.submit({ action: 'CLOSE_VIEW' });
      }
    });
  }

  function init() {
    wireFilterEnter();
    wireFilterSelects();
    wireAssignEnter();
    wireEscape();
    wireRowChecks();
    refreshMegabar();
    renderDetailRows();
    renderMegaKids();
  }

  // Expose for inline onclick handlers.
  window.HD_assign           = openAssign;
  window.HD_closeAssign      = closeAssign;
  window.HD_submitAssign     = submitAssign;
  window.HD_confirmDelete    = confirmDelete;
  window.HD_clearSelections  = clearSelections;
  window.HD_createMega       = createMega;
  window.HD_confirmDissolve  = confirmDissolve;

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init);
  } else {
    init();
  }
}());
