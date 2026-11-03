/* ============================================================================
 * Variety Wholesalers - Merchandise Inquiry shared behaviour (vwshared/app.js)
 *
 * Loaded on every screen of INV046W / INV502W / TS0110W before the screen's
 * own JS. It re-executes on every render, so:
 *   - everything hangs off window.VW and is (re)defined idempotently
 *   - templates call it through inline handler attributes (onclick="VW...")
 *     which resolve at event time - load order cannot break them
 *   - the only document-level listener (keyboard) is installed once, guarded,
 *     and always looks up the *current* screen when a key is pressed
 *   - the screens render completely without this file; it only adds speed
 * ==========================================================================*/
(function () {
  'use strict';
  var VW = window.VW = window.VW || {};
  VW.version = 'vw-1';

  /* ------------------------------------------------------------ helpers */
  VW.root = function () { return document.querySelector('.vw-app'); };
  VW.$ = function (sel, el) { return (el || VW.root() || document).querySelector(sel); };
  VW.$$ = function (sel, el) { return Array.prototype.slice.call((el || VW.root() || document).querySelectorAll(sel)); };
  VW.store = {
    get: function (k, d) { try { var v = localStorage.getItem(k); return v === null ? d : JSON.parse(v); } catch (e) { return d; } },
    set: function (k, v) { try { localStorage.setItem(k, JSON.stringify(v)); } catch (e) { /* private mode */ } }
  };
  VW.esc = function (s) {
    return String(s == null ? '' : s).replace(/[&<>"]/g, function (c) {
      return { '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' }[c];
    });
  };
  VW.num = function (v) { var n = parseFloat(String(v == null ? '' : v).replace(/,/g, '')); return isNaN(n) ? 0 : n; };
  VW.fmt = function (v, d) {
    return VW.num(v).toLocaleString('en-US', { minimumFractionDigits: d || 0, maximumFractionDigits: d || 0 });
  };
  /* Run fn once the screen root is in the DOM (screen JS may run before or
     after Profound UI injects the template). Marks the root so a re-run of
     the same render does not initialise twice. */
  VW.ready = function (cls, fn) {
    var tries = 0;
    (function poll() {
      var r = document.querySelector('.vw-app.' + cls), mark = 'data-ready-' + cls;
      if (r && !r.hasAttribute(mark)) { r.setAttribute(mark, '1'); try { fn(r); } catch (e) { if (window.console) console.error('[VW]', e); } return; }
      if (r || ++tries > 120) return;
      (window.requestAnimationFrame || setTimeout)(poll, 16);
    })();
  };

  /* ------------------------------------------------- talking to the IBM i */
  /* Every button posts an ACTION; the RPG maps it onto the same function key
     indicators / AID the green screen used. Extra fields can ride along. */
  VW.submit = function (action, fields) {
    var r = VW.root();
    if (r) {
      r.classList.add('is-busy');
      /* the next screen replaces this one; if the answer never comes, do not leave the screen locked */
      setTimeout(function () { if (document.body.contains(r)) r.classList.remove('is-busy'); }, 15000);
    }
    var o = fields || {};
    if (action) o.action = action;
    if (window.pui && typeof pui.submit === 'function') pui.submit(o);
    else { window.__vwSubmitted = o; if (r) r.classList.remove('is-busy'); }
    return false;
  };

  /* --------------------------------------------------------------- theme */
  VW.toggleTheme = function () {
    var r = VW.root(); if (!r) return;
    var t = r.getAttribute('data-theme') === 'dark' ? 'light' : 'dark';
    r.setAttribute('data-theme', t); VW.store.set('vw.theme', t);
    VW.toast(t === 'dark' ? 'Dark theme on' : 'Light theme on');
  };
  VW.toggleDensity = function () {
    var r = VW.root(); if (!r) return;
    var d = r.getAttribute('data-density') === 'compact' ? 'comfortable' : 'compact';
    r.setAttribute('data-density', d); VW.store.set('vw.density', d);
    VW.toast(d === 'compact' ? 'Compact rows' : 'Comfortable rows');
  };

  /* --------------------------------------------------------------- toasts */
  VW.toast = function (text, kind, ms) {
    var host = VW.$('.vw-toasts > div'); if (!host) return;
    var t = document.createElement('div');
    t.className = 'vw-toast' + (kind ? ' is-' + kind : '');
    t.setAttribute('role', 'status');
    t.innerHTML = '<svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.4" stroke-linecap="round"><path d="' +
      (kind === 'bad' ? 'M12 8v5M12 16.5v.5M4 20h16L12 4z' : 'M5 12.5l4.5 4.5L19 7.5') + '"/></svg><div>' + VW.esc(text) + '</div>';
    host.appendChild(t);
    setTimeout(function () { if (t.parentNode) t.parentNode.removeChild(t); }, ms || 2600);
  };

  /* ------------------------------------------------------------ clipboard */
  VW.copy = function (text, label) {
    var done = function () { VW.toast((label || 'Copied') + ': ' + text, 'ok'); };
    try {
      if (navigator.clipboard && navigator.clipboard.writeText) { navigator.clipboard.writeText(String(text)).then(done, done); return; }
    } catch (e) { /* fall through */ }
    var ta = document.createElement('textarea'); ta.value = text; document.body.appendChild(ta); ta.select();
    try { document.execCommand('copy'); } catch (e) { /* ignore */ }
    document.body.removeChild(ta); done();
  };

  /* ------------------------------------------------------------- recents */
  VW.remember = function (key, value, label, max) {
    if (!value || String(value) === '0') return;
    var list = VW.store.get(key, []).filter(function (x) { return String(x.v) !== String(value); });
    list.unshift({ v: String(value), l: label || '' });
    VW.store.set(key, list.slice(0, max || 8));
  };

  /* --------------------------------------------------------- table tools */
  VW.sort = function (btn, col, type) {
    var th = btn.closest('th'), table = btn.closest('table'); if (!table) return;
    var dir = th.getAttribute('aria-sort') === 'ascending' ? 'descending' : 'ascending';
    VW.$$('th', table).forEach(function (h) { h.removeAttribute('aria-sort'); });
    th.setAttribute('aria-sort', dir);
    var body = table.tBodies[0]; if (!body) return;
    /* rows sort inside their group (data-group) so headings stay with rows */
    var rows = Array.prototype.slice.call(body.rows).filter(function (r) { return r.hasAttribute('data-sortable'); });
    var key = function (r) {
      var c = r.querySelector('[data-col="' + col + '"]');
      var v = c ? (c.getAttribute('data-v') !== null ? c.getAttribute('data-v') : c.textContent) : '';
      return type === 'num' ? VW.num(v) : String(v).trim().toLowerCase();
    };
    var groups = {};
    rows.forEach(function (r) { var g = r.getAttribute('data-group') || '_'; (groups[g] = groups[g] || []).push(r); });
    Object.keys(groups).forEach(function (g) {
      var list = groups[g], anchor = list[list.length - 1].nextSibling;
      list.sort(function (a, b) { var x = key(a), y = key(b); return (x > y ? 1 : x < y ? -1 : 0) * (dir === 'ascending' ? 1 : -1); });
      list.forEach(function (r) { body.insertBefore(r, anchor); });
    });
    VW.toast('Sorted by ' + (btn.textContent || col).trim() + (dir === 'ascending' ? ' (low to high)' : ' (high to low)'));
  };
  VW.filter = function (input, tableId) {
    var q = String(input.value || '').trim().toLowerCase();
    var table = document.getElementById(tableId); if (!table) return;
    var shown = 0, total = 0;
    VW.$$('tbody tr[data-sortable]', table).forEach(function (r) {
      total++;
      var hit = !q || (r.getAttribute('data-search') || r.textContent).toLowerCase().indexOf(q) !== -1;
      r.classList.toggle('vw-hidden', !hit); if (hit) shown++;
    });
    var c = document.querySelector('[data-count-for="' + tableId + '"]');
    if (c) c.textContent = q ? (shown + ' of ' + total + ' shown') : (total + ' rows');
    if (typeof VW.onFilter === 'function') VW.onFilter(tableId, q);
  };
  VW.exportCsv = function (tableId, name) {
    var table = document.getElementById(tableId); if (!table) return;
    var out = [];
    var cells = function (tr) {
      return VW.$$('th,td', tr).filter(function (c) { return !c.hasAttribute('data-noexport'); }).map(function (c) {
        var v = c.getAttribute('data-v') !== null ? c.getAttribute('data-v') : c.textContent.replace(/\s+/g, ' ').trim();
        return '"' + String(v).replace(/"/g, '""') + '"';
      }).join(',');
    };
    VW.$$('thead tr', table).forEach(function (tr) { out.push(cells(tr)); });
    VW.$$('tbody tr', table).forEach(function (tr) { if (!tr.classList.contains('vw-hidden')) out.push(cells(tr)); });
    VW.$$('tfoot tr', table).forEach(function (tr) { out.push(cells(tr)); });
    var blob = new Blob(['﻿' + out.join('\r\n')], { type: 'text/csv;charset=utf-8' });
    var a = document.createElement('a');
    a.href = URL.createObjectURL(blob); a.download = (name || 'export') + '.csv';
    document.body.appendChild(a); a.click(); document.body.removeChild(a);
    setTimeout(function () { URL.revokeObjectURL(a.href); }, 2000);
    VW.toast('Exported ' + (out.length - 1) + ' rows to ' + a.download, 'ok');
  };

  /* ------------------------------------------- type-ahead lookup pickers */
  /* Replaces the F4 prompt programs that are not part of the demo restore.
     The catalogue (window.VW_LOOKUPS) is generated from the demo tables by
     prfdemolib/web/build.py, so suggestions match what the RPG will find. */
  var L = function () { return window.VW_LOOKUPS || {}; };
  VW.lookup = function (kind) {
    var d = L();
    switch (kind) {
      case 'sku': return (d.skus || []).map(function (s) { return { v: s.n, t: s.d, m: 'Dept ' + s.dept }; });
      case 'store': return (d.stores || []).filter(function (s) { return s.type === 'S'; }).map(function (s) { return { v: s.n, t: s.short + ' - ' + s.city, m: 'WH ' + s.wh }; });
      case 'loc': return (d.stores || []).map(function (s) { return { v: s.n, t: s.name, m: s.type === 'W' ? 'Warehouse' : s.city }; });
      case 'R': return (d.regions || []).map(function (s) { return { v: s.n, t: s.name, m: s.stores + ' stores' }; });
      case 'D': return (d.districts || []).map(function (s) { return { v: s.n, t: s.name, m: 'Region ' + s.reg }; });
      case 'S': return VW.lookup('loc');
      case 'G': return (d.groups || []).map(function (s) { return { v: s.n, t: s.name, m: s.stores + ' stores' }; });
      case 'I': return (d.invgroups || []).map(function (s) { return { v: s.n, t: s.name, m: s.stores + ' stores' }; });
      case 'dept': return (d.depts || []).map(function (s) { return { v: s.n, t: s.name, m: s.skus + ' SKUs' }; });
      default: return [];
    }
  };
  var hl = function (txt, q) {
    txt = VW.esc(txt); if (!q) return txt;
    var i = txt.toLowerCase().indexOf(q.toLowerCase());
    return i < 0 ? txt : txt.slice(0, i) + '<mark>' + txt.slice(i, i + q.length) + '</mark>' + txt.slice(i + q.length);
  };
  VW.pickInput = function (input) {
    var list = (input.closest('.vw-picker') || input.parentNode).querySelector('.vw-picker-list'); if (!list) return;
    var kind = input.getAttribute('data-kind'), q = String(input.value || '').trim();
    var items = VW.lookup(kind);
    if (q && !/^0+$/.test(q)) {
      var ql = q.toLowerCase();
      items = items.filter(function (it) { return String(it.v).indexOf(q) === 0 || it.t.toLowerCase().indexOf(ql) !== -1; });
    }
    items = items.slice(0, 40);
    if (!items.length) { list.innerHTML = '<div class="vw-picker-empty">No matches in the demo data - Enter will ask the IBM i anyway.</div>'; list.hidden = false; VW.pickPlace(list); return; }
    list.innerHTML = items.map(function (it, i) {
      return '<li role="option" aria-selected="' + (i === 0 ? 'true' : 'false') + '" data-v="' + VW.esc(it.v) + '"' +
        ' onmousedown="VW.pickChoose(this, event)"><b>' + hl(String(it.v), q) + '</b><span>' + hl(it.t, q) + '</span><em>' + VW.esc(it.m || '') + '</em></li>';
    }).join('');
    list.hidden = false;
    VW.pickPlace(list);
  };
  /* keep the list inside the window: flip it to the right edge of the field
     when it would run off the right of the screen */
  VW.pickPlace = function (list) {
    list.classList.remove('is-flip');
    var b = list.getBoundingClientRect(), vw = document.documentElement.clientWidth;
    if (b.right > vw - 8) list.classList.add('is-flip');
  };
  VW.pickChoose = function (li, ev) {
    if (ev) ev.preventDefault();
    var wrap = li.closest('.vw-picker'), input = wrap.querySelector('input');
    input.value = li.getAttribute('data-v');
    input.dispatchEvent(new Event('input', { bubbles: true }));
    input.dispatchEvent(new Event('change', { bubbles: true }));
    var list = wrap.querySelector('.vw-picker-list'); list.hidden = true;
    var lbl = wrap.querySelector('[data-pick-label]'); if (lbl) lbl.textContent = li.querySelector('span').textContent;
    if (input.getAttribute('data-autosubmit') === '1') { var o = {}; o[input.name] = input.value; VW.submit('ENTER', o); }
    else input.focus();
  };
  VW.pickKey = function (input, ev) {
    var list = (input.closest('.vw-picker') || input.parentNode).querySelector('.vw-picker-list');
    if (!list || list.hidden) { if (ev.key === 'ArrowDown') { VW.pickInput(input); ev.preventDefault(); } return; }
    var opts = Array.prototype.slice.call(list.querySelectorAll('li')), cur = opts.findIndex(function (o) { return o.getAttribute('aria-selected') === 'true'; });
    if (ev.key === 'ArrowDown' || ev.key === 'ArrowUp') {
      ev.preventDefault();
      var nx = Math.max(0, Math.min(opts.length - 1, cur + (ev.key === 'ArrowDown' ? 1 : -1)));
      opts.forEach(function (o, i) { o.setAttribute('aria-selected', i === nx ? 'true' : 'false'); });
      if (opts[nx]) opts[nx].scrollIntoView({ block: 'nearest' });
    } else if (ev.key === 'Tab' && opts[cur] && input.value.trim() !== opts[cur].getAttribute('data-v')) {
      VW.pickChoose(opts[cur]);
    } else if (ev.key === 'Escape') { list.hidden = true; }
    else if (ev.key === 'Enter' && opts[cur] && input.getAttribute('data-pick-enter') === '1' &&
             input.value.trim() && !/^\d+$/.test(input.value.trim())) {
      /* typed a name: Enter picks the highlighted match instead of submitting text */
      ev.preventDefault(); ev.stopPropagation(); VW.pickChoose(opts[cur]);
    }
  };
  VW.pickBlur = function (input) {
    /* only close if focus really left the field: re-focusing it (e.g. after
       changing "Search for") must not lose the list that just opened */
    setTimeout(function () {
      if (document.activeElement === input) return;
      var l = (input.closest('.vw-picker') || input.parentNode).querySelector('.vw-picker-list'); if (l) l.hidden = true;
    }, 150);
  };

  /* ----------------------------------------- dialogs: palette + shortcuts */
  VW.open = function (id) {
    var d = document.getElementById(id); if (!d) return;
    d.hidden = false;
    var f = d.querySelector('input, button'); if (f) setTimeout(function () { f.focus(); }, 20);
    if (id === 'vw-palette') VW.paletteFilter('');
  };
  VW.close = function (id) { var d = document.getElementById(id); if (d) d.hidden = true; };
  VW.closeAll = function () {
    var any = false;
    VW.$$('.vw-backdrop[data-vw-closable]').forEach(function (d) { if (!d.hidden) { d.hidden = true; any = true; } });
    return any;
  };
  /* Commands: global ones + every element on the screen marked data-cmd */
  VW.commands = function () {
    var cmds = [];
    VW.$$('[data-cmd]').forEach(function (el) {
      if (el.disabled || el.offsetParent === null && !el.hasAttribute('data-cmd-always')) return;
      cmds.push({ g: el.getAttribute('data-cmd-group') || 'This screen', t: el.getAttribute('data-cmd'), k: el.getAttribute('data-cmd-key') || '', run: function () { el.click(); } });
    });
    cmds.push({ g: 'View', t: 'Toggle dark theme', k: 'Alt+T', run: VW.toggleTheme });
    cmds.push({ g: 'View', t: 'Toggle compact rows', k: 'Alt+D', run: VW.toggleDensity });
    cmds.push({ g: 'View', t: 'Print this screen', k: 'Ctrl+P', run: function () { window.print(); } });
    cmds.push({ g: 'Help', t: 'Keyboard shortcuts', k: '?', run: function () { VW.open('vw-keys'); } });
    return cmds;
  };
  VW.paletteFilter = function (q) {
    var ul = document.getElementById('vw-palette-list'); if (!ul) return;
    q = String(q || '').toLowerCase().trim();
    var cmds = VW.commands().filter(function (c) { return !q || (c.t + ' ' + c.g).toLowerCase().indexOf(q) !== -1; });
    VW.__cmds = cmds;
    var html = '', g = null;
    cmds.forEach(function (c, i) {
      if (c.g !== g) { g = c.g; html += '<div class="vw-pal-group">' + VW.esc(g) + '</div>'; }
      html += '<li role="option" data-i="' + i + '" aria-selected="' + (i === 0) + '" onmousedown="VW.paletteRun(' + i + ', event)">' +
        '<span>' + VW.esc(c.t) + '</span>' + (c.k ? '<span class="vw-kbd">' + VW.esc(c.k) + '</span>' : '') + '</li>';
    });
    ul.innerHTML = html || '<div class="vw-picker-empty">No matching command</div>';
  };
  VW.paletteRun = function (i, ev) {
    if (ev) ev.preventDefault();
    var c = (VW.__cmds || [])[i]; VW.close('vw-palette'); if (c) setTimeout(c.run, 10);
  };
  VW.paletteKey = function (input, ev) {
    var ul = document.getElementById('vw-palette-list'); if (!ul) return;
    var opts = Array.prototype.slice.call(ul.querySelectorAll('li')), cur = opts.findIndex(function (o) { return o.getAttribute('aria-selected') === 'true'; });
    if (ev.key === 'ArrowDown' || ev.key === 'ArrowUp') {
      ev.preventDefault();
      var nx = Math.max(0, Math.min(opts.length - 1, cur + (ev.key === 'ArrowDown' ? 1 : -1)));
      opts.forEach(function (o, i) { o.setAttribute('aria-selected', i === nx ? 'true' : 'false'); });
      if (opts[nx]) opts[nx].scrollIntoView({ block: 'nearest' });
    } else if (ev.key === 'Enter') { ev.preventDefault(); ev.stopPropagation(); if (opts[cur]) VW.paletteRun(+opts[cur].getAttribute('data-i')); }
    else if (ev.key === 'Escape') { VW.close('vw-palette'); }
  };

  /* ------------------------------------------- keyboard (installed once) */
  if (!window.__vwKeys) {
    window.__vwKeys = true;
    document.addEventListener('keydown', function (ev) {
      var r = VW.root(); if (!r) return;           /* not on a VW screen */
      var t = ev.target, typing = t && (t.tagName === 'INPUT' || t.tagName === 'TEXTAREA' || t.tagName === 'SELECT');
      if ((ev.ctrlKey || ev.metaKey) && (ev.key === 'k' || ev.key === 'K')) { ev.preventDefault(); VW.open('vw-palette'); return; }
      if (ev.key === 'Escape' && VW.closeAll()) { ev.preventDefault(); return; }
      if (ev.altKey && (ev.key === 't' || ev.key === 'T')) { ev.preventDefault(); VW.toggleTheme(); return; }
      if (ev.altKey && (ev.key === 'd' || ev.key === 'D')) { ev.preventDefault(); VW.toggleDensity(); return; }
      if (typing) return;
      if (ev.key === '?') { ev.preventDefault(); VW.open('vw-keys'); return; }
      if (ev.key === '/') { var s = VW.$('[data-vw-search]'); if (s) { ev.preventDefault(); s.focus(); s.select && s.select(); } return; }
      if (ev.altKey && ev.key === 'ArrowRight') { var n = VW.$('[data-vw-next]'); if (n && !n.disabled) { ev.preventDefault(); n.click(); } return; }
      if (ev.altKey && ev.key === 'ArrowLeft') { var p = VW.$('[data-vw-prev]'); if (p && !p.disabled) { ev.preventDefault(); p.click(); } return; }
    });
  }

  /* -------------------------------------------- shared per-render setup */
  VW.ready('vw-app', function (r) {
    /* the topbar clock follows the browser; the merchandise date comes
       from SYSGET via the program */
    var clk = r.querySelector('[data-vw-clock]');
    if (clk) {
      var tick = function () { if (!document.body.contains(clk)) return; clk.textContent = new Date().toLocaleTimeString('en-US', { hour: 'numeric', minute: '2-digit' }); setTimeout(tick, 15000); };
      tick();
    }
    var n = VW.store.get('vw.notice', null);
    if (n && n.text) { VW.store.set('vw.notice', null); setTimeout(function () { VW.toast(n.text, n.kind, 4200); }, 200); }
  });
})();
