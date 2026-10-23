(function () {
  "use strict";

  var STATES = {
    AL: "Alabama", AK: "Alaska", AZ: "Arizona", AR: "Arkansas", CA: "California", CO: "Colorado",
    CT: "Connecticut", DE: "Delaware", DC: "Washington DC", FL: "Florida", GA: "Georgia", HI: "Hawaii",
    ID: "Idaho", IL: "Illinois", IN: "Indiana", IA: "Iowa", KS: "Kansas", KY: "Kentucky", LA: "Louisiana",
    ME: "Maine", MD: "Maryland", MA: "Massachusetts", MI: "Michigan", MN: "Minnesota", MS: "Mississippi",
    MO: "Missouri", MT: "Montana", NE: "Nebraska", NV: "Nevada", NH: "New Hampshire", NJ: "New Jersey",
    NM: "New Mexico", NY: "New York", NC: "North Carolina", ND: "North Dakota", OH: "Ohio", OK: "Oklahoma",
    OR: "Oregon", PA: "Pennsylvania", RI: "Rhode Island", SC: "South Carolina", SD: "South Dakota",
    TN: "Tennessee", TX: "Texas", UT: "Utah", VT: "Vermont", VA: "Virginia", WA: "Washington",
    WV: "West Virginia", WI: "Wisconsin", WY: "Wyoming"
  };
  var GENERIC_DOMAINS = ["gmail.com", "yahoo.com", "hotmail.com", "outlook.com", "email.com", "icloud.com",
    "aol.com", "live.com", "msn.com", "proton.me", "protonmail.com"];
  var SEGMENT_COLORS = ["#1769E0", "#F7861E", "#1E9E6A", "#6A43D1", "#D9344F", "#6F7D96"];
  var STORE = "cx.customers.";

  function esc(value) {
    return String(value == null ? "" : value).replace(/[&<>"']/g, function (c) {
      return { "&": "&amp;", "<": "&lt;", ">": "&gt;", "\"": "&quot;", "'": "&#39;" }[c];
    });
  }
  function pct(part, total) { return total ? Math.round(part / total * 100) : 0; }
  function plural(n, one, many) { return n + " " + (n === 1 ? one : many); }
  function store(key, value) { try { window.localStorage.setItem(STORE + key, value); } catch (e) { /* storage unavailable */ } }
  function load(key, fallback) {
    try { var v = window.localStorage.getItem(STORE + key); return v === null ? fallback : v; } catch (e) { return fallback; }
  }

  function parseRecord(row, subs) {
    var d = row.dataset;
    var addr = String(d.addr || "").trim();
    var st = /,\s*([A-Z]{2})\s+\d{4,5}/.exec(addr);
    var email = String(d.email || "").trim();
    var domain = email.indexOf("@") > -1 ? email.split("@")[1].toLowerCase() : "";
    var phone = String(d.phone || "").trim();
    var code = /(\d{3})/.exec(phone);
    return {
      row: row,
      sub: subs[d.rrn] || null,
      rrn: d.rrn,
      no: Number(d.no),
      name: String(d.name || "").trim(),
      email: email,
      phone: phone,
      addr: addr,
      state: st ? st[1] : "",
      domain: domain,
      corporate: !!domain && GENERIC_DOMAINS.indexOf(domain) === -1,
      areaCode: code ? code[1] : "",
      haystack: [d.name, d.email, d.phone, addr, d.no].join(" ").toLowerCase()
    };
  }

  function initials(name) {
    var parts = String(name || "").replace(/[^A-Za-z0-9 ]/g, " ").trim().split(/\s+/).filter(Boolean);
    if (!parts.length) { return "?"; }
    return (parts[0].charAt(0) + (parts.length > 1 ? parts[1].charAt(0) : "")).toUpperCase();
  }
  function tone(name) {
    var sum = 0, s = String(name || "");
    for (var i = 0; i < s.length; i++) { sum += s.charCodeAt(i); }
    return sum % 6;
  }

  function init() {
    var root = document.querySelector(".cx-list");
    if (!root || root.dataset.cxReady) { return; }
    root.dataset.cxReady = "1";

    function $(sel) { return root.querySelector(sel); }
    function $$(sel) { return Array.prototype.slice.call(root.querySelectorAll(sel)); }

    var body = $("[data-role=body]");
    var scroll = $("[data-role=scroll]");
    var workspace = $(".cx-workspace");
    var filterInput = $("#sfilter");
    var findInput = $("#sfndcustno");
    var originalFilter = filterInput ? filterInput.value : "";
    var originalFind = findInput ? findInput.value : "";

    var subs = {};
    $$("tr.cx-subrow").forEach(function (tr) { subs[tr.dataset.sub] = tr; });
    var records = $$("tr.cx-row").map(function (tr) { return parseRecord(tr, subs); });

    var state = { sortKey: "name", sortDir: 1, page: 1, pageSize: 25, filtered: [], selected: null };

    /* ---------- row decoration ---------- */
    records.forEach(function (r) {
      var chip = r.row.querySelector("[data-role=statechip]");
      if (chip) {
        chip.textContent = r.state || "–";
        if (r.state) { chip.title = STATES[r.state] || r.state; } else { chip.className += " is-empty"; }
      }
    });

    /* ---------- state dropdown ---------- */
    function fillStates() {
      var sel = $("[data-role=state]");
      var map = {};
      records.forEach(function (r) { if (r.state) { map[r.state] = (map[r.state] || 0) + 1; } });
      Object.keys(map).sort(function (a, b) { return (STATES[a] || a).localeCompare(STATES[b] || b); }).forEach(function (k) {
        var o = document.createElement("option");
        o.value = k;
        o.textContent = (STATES[k] || k) + " (" + map[k] + ")";
        sel.appendChild(o);
      });
    }

    /* ---------- counters ---------- */
    function countTo(el, target, suffix) {
      var from = parseInt(el.getAttribute("data-val") || "0", 10) || 0;
      el.setAttribute("data-val", String(target));
      suffix = suffix || "";
      if (from === target || !window.requestAnimationFrame) { el.textContent = target + suffix; return; }
      var start = null;
      function step(ts) {
        if (start === null) { start = ts; }
        var t = Math.min((ts - start) / 450, 1);
        el.textContent = Math.round(from + (target - from) * (1 - Math.pow(1 - t, 3))) + suffix;
        if (t < 1) { window.requestAnimationFrame(step); }
      }
      window.requestAnimationFrame(step);
    }

    function tally(list, key) {
      var map = {};
      list.forEach(function (r) { var k = r[key]; if (k) { map[k] = (map[k] || 0) + 1; } });
      return Object.keys(map).map(function (k) { return { key: k, n: map[k] }; })
        .sort(function (a, b) { return b.n - a.n || a.key.localeCompare(b.key); });
    }

    /* ---------- overview: stats, donut, bars, insights ---------- */
    function renderStats(list) {
      var corp = list.filter(function (r) { return r.corporate; }).length;
      countTo($("[data-kpi=count]"), list.length);
      countTo($("[data-kpi=states]"), tally(list, "state").length);
      countTo($("[data-kpi=corp]"), pct(corp, list.length), "%");
      countTo($("[data-kpi=codes]"), tally(list, "areaCode").length);
    }

    function renderDonut(list) {
      var donut = $("[data-chart=donut]");
      var legend = $("[data-chart=legend]");
      var states = tally(list, "state");
      var total = states.reduce(function (s, x) { return s + x.n; }, 0);
      $("[data-note=states]").textContent = states.length ? plural(states.length, "state", "states") : "";
      if (!total) {
        donut.innerHTML = '<svg viewBox="0 0 42 42" class="cx-donut-svg"><circle cx="21" cy="21" r="15.9155" fill="none" stroke="#E3E9F3" stroke-width="5"/></svg><div class="cx-donut-center"><strong>0</strong><span>customers</span></div>';
        legend.innerHTML = '<li class="cx-legend-empty">No data to chart</li>';
        return;
      }
      var top = states.slice(0, 5);
      var rest = states.slice(5).reduce(function (s, x) { return s + x.n; }, 0);
      var segs = top.map(function (x) { return { label: STATES[x.key] || x.key, n: x.n }; });
      if (rest) { segs.push({ label: "Other", n: rest }); }
      var acc = 0;
      var circles = segs.map(function (s, i) {
        var share = s.n / total * 100;
        var dash = Math.max(share - 1, 0.01);
        var c = '<circle class="cx-seg" cx="21" cy="21" r="15.9155" fill="none" stroke="' + SEGMENT_COLORS[i % SEGMENT_COLORS.length] +
          '" stroke-width="5" stroke-dasharray="' + dash.toFixed(2) + " " + (100 - dash).toFixed(2) +
          '" stroke-dashoffset="' + (25 - acc + 0.5).toFixed(2) + '" style="--d:' + (i * 80) + 'ms"><title>' + esc(s.label) + ": " + s.n + "</title></circle>";
        acc += share;
        return c;
      }).join("");
      donut.innerHTML = '<svg viewBox="0 0 42 42" class="cx-donut-svg"><circle cx="21" cy="21" r="15.9155" fill="none" stroke="#E3E9F3" stroke-width="5"/>' +
        circles + '</svg><div class="cx-donut-center"><strong>' + total + "</strong><span>customers</span></div>";
      legend.innerHTML = segs.map(function (s, i) {
        return '<li><i style="background:' + SEGMENT_COLORS[i % SEGMENT_COLORS.length] + '"></i><span>' + esc(s.label) +
          "</span><b>" + s.n + "</b><em>" + pct(s.n, total) + "%</em></li>";
      }).join("");
    }

    function renderBars(list) {
      var n = list.length;
      var corp = list.filter(function (r) { return r.corporate; }).length;
      var phone = list.filter(function (r) { return r.phone; }).length;
      var complete = list.filter(function (r) { return r.state && /\d{5}/.test(r.addr); }).length;
      var bars = [
        { label: "Corporate email", v: pct(corp, n), cls: "green" },
        { label: "Personal email", v: n ? 100 - pct(corp, n) : 0, cls: "orange" },
        { label: "Phone on file", v: pct(phone, n), cls: "blue" },
        { label: "Complete address", v: pct(complete, n), cls: "purple" }
      ];
      $("[data-chart=bars]").innerHTML = bars.map(function (b, i) {
        return '<div class="cx-bar"><div class="cx-bar-top"><span>' + b.label + "</span><b>" + b.v +
          '%</b></div><div class="cx-bar-track"><i class="cx-bar-fill cx-fill-' + b.cls + '" style="width:' + b.v + "%;--d:" + (i * 70) + 'ms"></i></div></div>';
      }).join("");
    }

    function renderInsights(list) {
      var ul = $("[data-chart=insights]");
      var n = list.length;
      if (!n) {
        ul.innerHTML = '<li class="cx-insight"><span class="cx-insight-ico cx-tone-rose"><svg class="cx-ico"><use href="#cx-alert"/></svg></span><span>No customers match the current filters.</span></li>';
        return;
      }
      var items = [];
      var states = tally(list, "state");
      if (states.length) {
        var lead = states[0];
        items.push({ t: "blue", i: "pin", h: "<b>" + esc(STATES[lead.key] || lead.key) + "</b> leads with " + plural(lead.n, "customer", "customers") + " (" + pct(lead.n, n) + "%)." });
        if (states.length >= 4) { items.push({ t: "lav", i: "globe", h: "Coverage spans <b>" + states.length + " states</b>." }); }
      }
      var corp = list.filter(function (r) { return r.corporate; }).length;
      if (n >= 2) {
        if (pct(corp, n) >= 60) { items.push({ t: "mint", i: "building", h: "<b>" + pct(corp, n) + "%</b> use corporate email &mdash; good for account-based outreach." }); }
        else if (pct(corp, n) <= 40) { items.push({ t: "peach", i: "mail", h: "<b>" + (100 - pct(corp, n)) + "%</b> use personal email; capturing a business contact could help." }); }
        else { items.push({ t: "mint", i: "building", h: "Balanced mix: <b>" + corp + " corporate</b> vs " + (n - corp) + " personal domains." }); }
      }
      var series = tally(list.map(function (r) { return { s: String(r.no).charAt(0) + "xxxxx" }; }), "s");
      if (series.length > 1) {
        items.push({ t: "blue", i: "hash", h: "Customer numbers use <b>" + series.length + " series</b>: " + series.map(function (x) { return esc(x.key) + " (" + x.n + ")"; }).join(", ") + "." });
      }
      var missing = list.filter(function (r) { return !r.email || !r.phone; }).length;
      items.push(missing
        ? { t: "rose", i: "alert", h: "<b>" + plural(missing, "record", "records") + "</b> missing an email or phone." }
        : { t: "mint", i: "mail", h: "All shown records have both email and phone." });
      ul.innerHTML = items.slice(0, 5).map(function (x, i) {
        return '<li class="cx-insight" style="--d:' + (i * 60) + 'ms"><span class="cx-insight-ico cx-tone-' + x.t + '"><svg class="cx-ico"><use href="#cx-' + x.i + '"/></svg></span><span>' + x.h + "</span></li>";
      }).join("");
    }

    /* ---------- filter / sort / page ---------- */
    function matches(r, terms, st, mt) {
      if (st && r.state !== st) { return false; }
      if (mt === "corp" && !r.corporate) { return false; }
      if (mt === "personal" && r.corporate) { return false; }
      for (var i = 0; i < terms.length; i++) { if (r.haystack.indexOf(terms[i]) === -1) { return false; } }
      return true;
    }

    function comparator() {
      var key = state.sortKey, dir = state.sortDir;
      return function (a, b) {
        var c;
        if (key === "no") { c = a.no - b.no; }
        else { c = String(a[key] || "").toLowerCase().localeCompare(String(b[key] || "").toLowerCase()); }
        if (c === 0) { c = a.no - b.no; }
        return c * dir;
      };
    }

    function pageCount() { return Math.max(1, Math.ceil(state.filtered.length / state.pageSize)); }

    function recompute(resetPage) {
      var q = filterInput ? filterInput.value.trim().toLowerCase() : "";
      var terms = q ? q.split(/\s+/) : [];
      var st = $("[data-role=state]").value;
      var mt = $("[data-role=mailtype]").value;
      state.filtered = records.filter(function (r) { return matches(r, terms, st, mt); }).sort(comparator());
      if (resetPage) { state.page = 1; }
      state.page = Math.min(state.page, pageCount());
      var list = state.filtered;
      renderStats(list);
      renderDonut(list);
      renderBars(list);
      renderInsights(list);
      var clear = $("[data-act=clear]");
      if (clear) { clear.classList.toggle("is-on", !!q); }
      var start = (state.page - 1) * state.pageSize;
      var pageRows = list.slice(start, start + state.pageSize);
      // Keep the selection on the page being shown, otherwise arrow keys would jump pages.
      if (!state.selected || pageRows.indexOf(state.selected) === -1) { select(pageRows[0] || null); }
      renderPage();
    }

    function renderPage() {
      var size = state.pageSize;
      var start = (state.page - 1) * size;
      var slice = state.filtered.slice(start, start + size);
      var inSlice = {};
      slice.forEach(function (r, i) { inSlice[r.rrn] = i; });
      records.forEach(function (r) {
        var show = Object.prototype.hasOwnProperty.call(inSlice, r.rrn);
        r.row.hidden = !show;
        if (r.sub) { r.sub.hidden = !show; }
      });
      slice.forEach(function (r, i) {
        body.appendChild(r.row);
        if (r.sub) { body.appendChild(r.sub); }
        r.row.classList.toggle("is-odd", i % 2 === 1);
      });
      $("[data-role=nomatch]").hidden = state.filtered.length > 0 || records.length === 0;

      var total = state.filtered.length;
      $("[data-role=range]").textContent = total
        ? (start + 1) + "–" + (start + slice.length) + " of " + total
        : "0 results";
      var q = filterInput ? filterInput.value.trim() : "";
      $("[data-role=count]").innerHTML = "<b>" + total + "</b> of " + records.length + " customers" +
        (q ? ' matching <span class="cx-q">“' + esc(q) + "”</span>" : "");
      renderPager();
      updateSortHeaders();
    }

    function renderPager() {
      var nav = $("[data-role=pages]");
      var pages = pageCount();
      var cur = state.page;
      function btn(label, target, opts) {
        opts = opts || {};
        return '<button type="button" class="cx-pg' + (opts.on ? " is-on" : "") + '" data-page="' + target + '"' +
          (opts.disabled ? " disabled" : "") + (opts.aria ? ' aria-label="' + opts.aria + '"' : "") + ">" + label + "</button>";
      }
      var html = btn('<svg class="cx-ico"><use href="#cx-first"/></svg>', 1, { disabled: cur === 1, aria: "First page" }) +
        btn('<svg class="cx-ico"><use href="#cx-chev-left"/></svg>', cur - 1, { disabled: cur === 1, aria: "Previous page" });
      var shown = {};
      [1, pages, cur - 2, cur - 1, cur, cur + 1, cur + 2].forEach(function (p) { if (p >= 1 && p <= pages) { shown[p] = true; } });
      var last = 0;
      Object.keys(shown).map(Number).sort(function (a, b) { return a - b; }).forEach(function (p) {
        if (p - last > 1) { html += '<span class="cx-dots">&hellip;</span>'; }
        html += btn(p, p, { on: p === cur });
        last = p;
      });
      html += btn('<svg class="cx-ico"><use href="#cx-chev-right"/></svg>', cur + 1, { disabled: cur === pages, aria: "Next page" }) +
        btn('<svg class="cx-ico"><use href="#cx-last"/></svg>', pages, { disabled: cur === pages, aria: "Last page" });
      nav.innerHTML = html;
      nav.hidden = pages <= 1;
    }

    function updateSortHeaders() {
      $$("th[data-sort]").forEach(function (th) {
        var on = th.dataset.sort === state.sortKey;
        th.classList.toggle("is-sorted", on);
        th.setAttribute("aria-sort", on ? (state.sortDir === 1 ? "ascending" : "descending") : "none");
        var use = th.querySelector("use");
        if (use) { use.setAttribute("href", on ? (state.sortDir === 1 ? "#cx-sort-up" : "#cx-sort-down") : "#cx-sort"); }
      });
    }

    function goToPage(p) {
      state.page = Math.min(Math.max(p, 1), pageCount());
      renderPage();
      var first = state.filtered[(state.page - 1) * state.pageSize];
      if (first) { select(first); }
      scroll.scrollTop = 0;
    }

    /* ---------- selection + map preview ---------- */
    function select(rec) {
      state.selected = rec;
      records.forEach(function (r) {
        var on = r === rec;
        r.row.classList.toggle("is-selected", on);
        r.row.setAttribute("aria-selected", on ? "true" : "false");
      });
      var empty = $("[data-role=preview-empty]");
      var bodyEl = $("[data-role=preview-body]");
      if (!rec) { empty.hidden = false; bodyEl.hidden = true; return; }
      empty.hidden = true;
      bodyEl.hidden = false;
      var av = $("[data-pv=avatar]");
      av.className = "cx-avatar cx-avatar-lg cx-av-" + tone(rec.name);
      av.textContent = initials(rec.name);
      $("[data-pv=name]").textContent = rec.name;
      $("[data-pv=no]").textContent = "#" + rec.no;
      $("[data-pv=addr]").textContent = rec.addr;
      var q = encodeURIComponent(rec.addr);
      if (workspace.getAttribute("data-map") === "on") { loadMap(q); }
      $("[data-pv=dir]").setAttribute("href", "https://www.google.com/maps/dir/?api=1&destination=" + q);
      var mail = $("[data-pv=mail]");
      var tel = $("[data-pv=tel]");
      mail.setAttribute("href", rec.email ? "mailto:" + rec.email : "#");
      tel.setAttribute("href", rec.phone ? "tel:" + rec.phone.replace(/[^0-9+]/g, "") : "#");
      mail.classList.toggle("is-disabled", !rec.email);
      tel.classList.toggle("is-disabled", !rec.phone);
    }

    function loadMap(q) {
      var map = $("[data-pv=map]");
      var src = "https://maps.google.com/maps?q=" + q + "&z=14&output=embed";
      if (map.getAttribute("src") !== src) { map.setAttribute("src", src); }
    }

    function moveSelection(delta) {
      var list = state.filtered;
      if (!list.length) { return; }
      var idx = list.indexOf(state.selected);
      var next = Math.min(Math.max((idx < 0 ? 0 : idx) + delta, 0), list.length - 1);
      var targetPage = Math.floor(next / state.pageSize) + 1;
      if (targetPage !== state.page) { state.page = targetPage; renderPage(); }
      select(list[next]);
      if (state.selected.row.scrollIntoView) { state.selected.row.scrollIntoView({ block: "nearest" }); }
    }

    /* ---------- actions ---------- */
    function openRecord(rec) {
      if (!rec) { return; }
      $$("[data-role=opt]").forEach(function (input) { input.value = ""; });
      var opt = rec.row.querySelector("[data-role=opt]");
      if (opt) { opt.value = "5"; }
      // Only the option should reach the program, not a half-typed search.
      if (filterInput) { filterInput.value = originalFilter; }
      if (findInput) { findInput.value = originalFind; }
      pui.submit({ action: "VIEW" });
    }

    function serverSearch() { pui.submit({ action: "SEARCH" }); }

    function exportCsv() {
      function cell(v) { return '"' + String(v == null ? "" : v).replace(/"/g, '""') + '"'; }
      var lines = [["Number", "Customer", "Email", "Phone", "State", "Address"].map(cell).join(",")];
      state.filtered.forEach(function (r) {
        lines.push([r.no, r.name, r.email, r.phone, r.state, r.addr].map(cell).join(","));
      });
      var blob = new Blob(["﻿" + lines.join("\r\n")], { type: "text/csv;charset=utf-8" });
      var a = document.createElement("a");
      a.href = URL.createObjectURL(blob);
      a.download = "customers.csv";
      root.appendChild(a);
      a.click();
      root.removeChild(a);
      setTimeout(function () { URL.revokeObjectURL(a.href); }, 1000);
    }

    function setDensity(d) {
      root.setAttribute("data-density", d);
      $("[data-role=density-label]").textContent = d === "compact" ? "Compact" : "Comfortable";
      store("density", d);
    }
    function setHidden(cols) {
      root.setAttribute("data-hide", cols.join(" "));
      $$("[data-col]").forEach(function (cb) { cb.checked = cols.indexOf(cb.getAttribute("data-col")) === -1; });
      store("hide", cols.join(","));
    }
    function setMap(on) {
      workspace.setAttribute("data-map", on ? "on" : "off");
      $(".cx-maptoggle").classList.toggle("is-on", on);
      store("map", on ? "on" : "off");
      if (on && state.selected) { loadMap(encodeURIComponent(state.selected.addr)); }
    }
    function setOverview(open) {
      $("[data-role=overview-body]").hidden = !open;
      var t = $("[data-act=toggle-overview]");
      t.setAttribute("aria-expanded", open ? "true" : "false");
      t.classList.toggle("is-closed", !open);
      $("[data-role=ov-label]").textContent = open ? "Hide insights" : "Show insights";
      store("overview", open ? "open" : "closed");
    }

    function recordFromTarget(target) {
      var host = target.closest("tr.cx-row, tr.cx-subrow");
      if (!host) { return null; }
      var rrn = host.dataset.rrn || host.dataset.sub;
      for (var i = 0; i < records.length; i++) { if (records[i].rrn === rrn) { return records[i]; } }
      return null;
    }

    root.addEventListener("click", function (e) {
      var t = e.target;
      var menu = $("[data-role=cols-menu]");
      if (!t.closest(".cx-menu-wrap")) { menu.hidden = true; }

      var th = t.closest("th[data-sort]");
      if (th) {
        var key = th.dataset.sort;
        if (state.sortKey === key) { state.sortDir *= -1; } else { state.sortKey = key; state.sortDir = 1; }
        recompute(false);
        return;
      }
      var pg = t.closest("[data-page]");
      if (pg && !pg.disabled) { goToPage(parseInt(pg.getAttribute("data-page"), 10)); return; }

      var actEl = t.closest("[data-act]");
      var act = actEl ? actEl.getAttribute("data-act") : "";
      switch (act) {
        case "open": openRecord(recordFromTarget(t)); return;
        case "open-selected": openRecord(state.selected); return;
        case "search": serverSearch(); return;
        case "reset":
          if (filterInput) { filterInput.value = ""; }
          if (findInput) { findInput.value = ""; }
          serverSearch();
          return;
        case "clear":
          if (filterInput) { filterInput.value = ""; filterInput.focus(); }
          if (originalFilter) { serverSearch(); } else { recompute(true); }
          return;
        case "toggle-cols": menu.hidden = !menu.hidden; return;
        case "density": setDensity(root.getAttribute("data-density") === "compact" ? "comfortable" : "compact"); return;
        case "export": exportCsv(); return;
        case "toggle-map": setMap(workspace.getAttribute("data-map") !== "on"); return;
        case "toggle-overview": setOverview($("[data-role=overview-body]").hidden); return;
        case "expand": {
          var rec = recordFromTarget(t);
          if (rec) {
            var open = !rec.row.classList.contains("is-open");
            rec.row.classList.toggle("is-open", open);
            if (rec.sub) { rec.sub.classList.toggle("is-open", open); }
          }
          return;
        }
        default: break;
      }
      var r = recordFromTarget(t);
      if (r) { select(r); }
    });

    root.addEventListener("dblclick", function (e) {
      if (e.target.closest("button, a, input, select")) { return; }
      var rec = recordFromTarget(e.target);
      if (rec) { openRecord(rec); }
    });

    root.addEventListener("keydown", function (e) {
      var t = e.target;
      var typing = t === filterInput || t === findInput || (t.tagName === "SELECT") || (t.tagName === "INPUT" && t.type === "checkbox");
      // Profound UI also listens for Enter on document and calls pui.submit(); stop it so we submit exactly once.
      if (e.key === "Enter" && (t === filterInput || t === findInput)) { e.preventDefault(); e.stopPropagation(); serverSearch(); return; }
      if (typing || (t.tagName === "BUTTON" && e.key === "Enter")) { return; }
      switch (e.key) {
        case "ArrowDown": e.preventDefault(); moveSelection(1); break;
        case "ArrowUp": e.preventDefault(); moveSelection(-1); break;
        case "PageDown": e.preventDefault(); goToPage(state.page + 1); break;
        case "PageUp": e.preventDefault(); goToPage(state.page - 1); break;
        case "Home": e.preventDefault(); moveSelection(-state.filtered.length); break;
        case "End": e.preventDefault(); moveSelection(state.filtered.length); break;
        case "Enter": if (state.selected && t.closest(".cx-scroll")) { e.preventDefault(); e.stopPropagation(); openRecord(state.selected); } break;
        default: break;
      }
    });

    if (filterInput) { filterInput.addEventListener("input", function () { recompute(true); }); }
    $("[data-role=state]").addEventListener("change", function () { recompute(true); });
    $("[data-role=mailtype]").addEventListener("change", function () { recompute(true); });
    $("[data-role=pagesize]").addEventListener("change", function (e) {
      state.pageSize = parseInt(e.target.value, 10) || 25;
      store("pagesize", String(state.pageSize));
      recompute(true);
    });
    $$("[data-col]").forEach(function (cb) {
      cb.addEventListener("change", function () {
        setHidden($$("[data-col]").filter(function (x) { return !x.checked; }).map(function (x) { return x.getAttribute("data-col"); }));
      });
    });

    /* ---------- boot ---------- */
    if (findInput && findInput.value.trim() === "0") { findInput.value = ""; }
    fillStates();
    setDensity(load("density", "comfortable") === "compact" ? "compact" : "comfortable");
    var hidden = load("hide", "");
    setHidden(hidden ? hidden.split(",").filter(Boolean) : []);
    var size = parseInt(load("pagesize", "25"), 10);
    if ([10, 25, 50, 100].indexOf(size) === -1) { size = 25; }
    state.pageSize = size;
    $("[data-role=pagesize]").value = String(size);
    setMap(load("map", "on") === "on" && window.innerWidth >= 1100);
    setOverview(load("overview", "closed") === "open");
    recompute(true);
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", init);
  } else {
    init();
  }
}());
