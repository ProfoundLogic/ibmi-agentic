(function () {
  // ---- login screen helpers --------------------------------------------
  function setEmp(code) {
    var el = document.getElementById('empcode');
    if (el) { el.value = code; el.focus(); }
  }
  function setWh(code) {
    var hidden = document.getElementById('warehouse');
    if (hidden) hidden.value = code;
    Array.prototype.forEach.call(document.querySelectorAll('.pk-whbtn'), function (b) {
      b.classList.toggle('is-active', b.getAttribute('data-wh') === code);
    });
  }
  function submitLogin() {
    var emp = (document.getElementById('empcode') || {}).value || '';
    var wh  = (document.getElementById('warehouse') || {}).value || '';
    emp = emp.trim().toUpperCase();
    if (!emp) {
      var el = document.getElementById('empcode');
      if (el) el.focus();
      return;
    }
    if (!wh) wh = 'WEST';
    if (window.pui && typeof pui.submit === 'function') {
      pui.submit({ action: 'LOGIN', empcode: emp, warehouse: wh });
    }
  }
  function wireLoginEnter() {
    var el = document.getElementById('empcode');
    if (!el || el.dataset.pkWired === '1') return;
    el.dataset.pkWired = '1';
    el.addEventListener('keydown', function (ev) {
      if (ev.key === 'Enter') { ev.preventDefault(); submitLogin(); }
    });
  }

  // ---- detail screen: render lines and wire scan actions ---------------
  // Lines key is "child:seq" so SCAN can disambiguate same-seq lines
  // across constituent batches in a mega.  Tote badges are color-coded
  // per tote slot (T01..T0N).
  function renderLines() {
    var host = document.querySelector('.pk-lines');
    if (!host) return;
    var batch  = host.getAttribute('data-batch') || '0';
    var isMega = host.getAttribute('data-ismega') === '1';
    var json   = host.getAttribute('data-lines') || '[]';
    var lines;
    try { lines = JSON.parse(json); }
    catch (e) {
      host.innerHTML = '<div class="pk-detail-loading">Could not parse lines.</div>';
      return;
    }
    if (!Array.isArray(lines) || lines.length === 0) {
      host.innerHTML = '<div class="pk-detail-loading">No pick lines for this batch.</div>';
      return;
    }

    // Compute walk order from WHLOC coordinates (same DP the map uses)
    // so the on-screen list reads top-to-bottom in the same order the
    // picker physically walks.  Unmapped lines fall to the bottom.
    var mappedLines   = lines.filter(function (l) {
      return Number(l.whRow) > 0 && Number(l.whBay) > 0;
    });
    var unmappedLines = lines.filter(function (l) {
      return !(Number(l.whRow) > 0 && Number(l.whBay) > 0);
    });
    if (mappedLines.length > 0) {
      var walkPlan = planRoute(mappedLines);
      var walkOrdered = flattenPlan(walkPlan);
      walkOrdered.forEach(function (l, i) { l._walkIdx = i + 1; });
      lines = walkOrdered.concat(unmappedLines);
    }

    var html = lines.map(function (r, idx) {
      var need  = Number(r.need) || 0;
      var pic   = Number(r.qtyp) || 0;
      var pct   = need > 0 ? Math.min(100, Math.round((pic / need) * 100)) : 0;
      var done  = (need > 0 && pic >= need);
      var child = Number(r.child) || Number(batch) || 0;
      var tote  = (r.tote || 'T01').toUpperCase();
      var toteN = Number(tote.replace(/^T0?/, '')) || 1;
      // Deterministic colour per tote slot (1..8 supported in css; wraps).
      var toteClass = 'pk-tote pk-tote--' + ((toteN - 1) % 8 + 1);
      var key = child + ':' + r.seq;
      var whRow   = Number(r.whRow)   || 0;
      var whBay   = Number(r.whBay)   || 0;
      var whShelf = Number(r.whShelf) || 0;
      var hasCoord = whRow > 0 && whBay > 0;
      var rbs = hasCoord
        ? '<span class="pk-rbs" title="Row ' + whRow +
              ', Bay ' + whBay + ', Shelf ' + whShelf + '">'
          + '<span class="pk-rbs-pin" aria-hidden="true">'
          +   '<svg viewBox="0 0 12 14" fill="currentColor">'
          +     '<path d="M6 0a4.5 4.5 0 0 0-4.5 4.5C1.5 8 6 14 6 14s4.5-6 4.5-9.5A4.5 4.5 0 0 0 6 0zm0 6a1.6 1.6 0 1 1 0-3.2 1.6 1.6 0 0 1 0 3.2z"/>'
          +   '</svg>'
          + '</span>'
          + 'R<strong>' + pad2(whRow) + '</strong>'
          + '<span class="pk-rbs-dot"></span>'
          + 'B<strong>' + pad2(whBay) + '</strong>'
          + '<span class="pk-rbs-dot"></span>'
          + 'S<strong>' + whShelf + '</strong>'
          + '</span>'
        : '';
      // Walk-order stop badge -- the colored circle matches the dot on
      // the map exactly (same colour palette, same number).  When the
      // picker reads "stop 3" on the map they look for the circle with
      // a 3 down the list -- that line is the item to pick at that map
      // position.  Internal PICKSEQ is preserved on the data-key so
      // SCAN still routes correctly.
      var palette = ['#C8102E', '#1F7A1F', '#B08D57', '#1F4F7A',
                     '#6E1F7A', '#B0571F', '#0A0A0A', '#616264'];
      var stopColor = palette[(toteN - 1) % palette.length];
      var stopBadge = (typeof r._walkIdx === 'number')
        ? '<div class="pk-stop-circle" style="background:' + stopColor +
            ';" title="Walk order">' + r._walkIdx + '</div>'
        : '<div class="pk-stop-circle pk-stop-circle--unmapped" title="No mapped location">?</div>';

      return ''+
        '<div class="pk-line' + (done ? ' is-complete' : '') +
              (isMega ? ' has-tote' : '') + '" ' +
              'data-key="' + key + '">' +
        stopBadge +
        (isMega
          ? '<div class="pk-line-tote"><span class="' + toteClass + '">' + tote +
            '</span><span class="pk-line-child">batch ' + child + '</span></div>'
          : '') +
        '  <div class="pk-line-item">' +
        '    <span class="item-code">' + escapeHtml(r.item) + '</span>' +
        '    <span class="item-meta">Order ' + r.ord + '</span>' +
        '  </div>' +
        '  <div class="pk-line-loc">' +
        '    <span class="loc-aisle">' + escapeHtml(r.aisle || '') + '</span>' +
        '    <span class="loc-bin">' + escapeHtml(r.loc || '') + '</span>' +
        rbs +
        '  </div>' +
        '  <div class="pk-line-qty">' +
        '    <div class="pk-qty-row"><strong>' + pic + '</strong> / ' + need + '</div>' +
        '    <div class="pk-line-bar"><span style="width:' + pct + '%"></span></div>' +
        '  </div>' +
        '  <div class="pk-line-controls">' +
        '    <input type="number" min="1" value="1" class="pk-qty-input"' +
        '           data-qty-for="' + key + '"' + (done ? ' disabled' : '') + '>' +
        '    <button type="button" class="pk-btn pk-btn--ghost pk-btn--small"' +
        (done ? ' disabled' : '') +
        '            onclick="HD_scan(' + child + ',' + r.seq + ')">Scan</button>' +
        '    <button type="button" class="pk-btn pk-btn--success pk-btn--small"' +
        (done ? ' disabled' : '') +
        '            onclick="HD_scanFull(' + child + ',' + r.seq + ')">Scan all</button>' +
        '  </div>' +
        '</div>';
    }).join('');
    host.innerHTML = html;
  }

  function scan(childBatch, seq) {
    var key = childBatch + ':' + seq;
    var input = document.querySelector('.pk-qty-input[data-qty-for="' + key + '"]');
    var q = input ? Number(input.value) : 1;
    if (!q || q < 1) q = 1;
    if (window.pui && typeof pui.submit === 'function') {
      // Pass childBatch as selchild; the umbrella stays in detbatch.
      pui.submit({ action: 'SCAN', selchild: childBatch, selseq: seq, qty: q });
    }
  }
  function scanFull(childBatch, seq) {
    if (window.pui && typeof pui.submit === 'function') {
      pui.submit({ action: 'SCANFULL', selchild: childBatch, selseq: seq });
    }
  }

  function escapeHtml(v) {
    return String(v == null ? '' : v)
      .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
  }

  function pad2(n) {
    var s = String(Math.max(0, Number(n) || 0));
    return s.length < 2 ? '0' + s : s;
  }

  // -------------------------------------------------------------------
  //  Warehouse map (route + heatmap)
  // -------------------------------------------------------------------
  // The 12-row x 60-bay grid is rendered as an SVG.  Rows run horizontally
  // (so a row = a long aisle facing north/south); each row has 60 bays.
  // Aisles separate rows.  Each pick is plotted at (bay, row) and a
  // dashed path connects them in pick order.
  //
  // Heatmap mode bins picks by (row, bay) and colors cells by intensity.

  var MAP_W = 12;      // rows
  var MAP_B = 60;      // bays
  var MAP_S = 4;       // shelves
  var mapState = { mode: 'route' };

  function pickedLines() {
    var host = document.querySelector('.pk-lines');
    if (!host) return [];
    try { return JSON.parse(host.getAttribute('data-lines') || '[]') || []; }
    catch (e) { return []; }
  }

  function openMap() {
    var m = document.getElementById('pk-map-modal');
    if (!m) return;
    m.classList.remove('pk-hidden');
    setMapMode(mapState.mode || 'route');
  }
  function closeMap() {
    var m = document.getElementById('pk-map-modal');
    if (m) m.classList.add('pk-hidden');
  }
  function setMapMode(mode) {
    mapState.mode = (mode === 'heat') ? 'heat' : 'route';
    Array.prototype.forEach.call(document.querySelectorAll('.pk-tab'), function (t) {
      t.classList.toggle('is-active', t.getAttribute('data-mode') === mapState.mode);
    });
    renderMap();
  }

  // ---- Route optimisation (DP over through vs in-and-out per aisle) --
  //
  // Aisles can only be entered/exited at the two warehouse ends (left =
  // bay 1, right = bay 60).  Within an aisle the picker chooses between:
  //   - THROUGH   : walk the full aisle from one end to the other,
  //                 visiting stops along the way.  Cost ~ 60 bays.
  //   - IN-AND-OUT: walk in to the deepest stop and turn back, exiting
  //                 at the entry side.  Cost = 2 x deepest bay.
  // The next aisle's entry side is forced to equal the previous aisle's
  // exit side (cross-aisle at the warehouse end).  The last aisle does
  // not exit -- it stops at its furthest pick.
  //
  // DP state: dp[aisleIdx][entryEdge] = minimum remaining walk cost.
  // We pick the cheaper start entry edge and reconstruct the plan.
  function planRoute(lines) {
    var byAisle = {};
    lines.forEach(function (line) {
      var a = Math.ceil(Number(line.whRow) / 2);
      (byAisle[a] = byAisle[a] || []).push(line);
    });
    var aisles = Object.keys(byAisle).map(Number)
                       .sort(function (a, b) { return a - b; });
    var N = aisles.length;
    if (N === 0) return [];

    var sortedStops = aisles.map(function (a) {
      return byAisle[a].slice().sort(function (p, q) {
        return Number(p.whBay) - Number(q.whBay);
      });
    });
    function mm(i) {
      var s = sortedStops[i];
      return { min: Number(s[0].whBay),
               max: Number(s[s.length - 1].whBay) };
    }
    var BAYS = MAP_B;            // 60
    var FULL = BAYS - 1;         // ~ cost of one full traversal

    // dp[i][e]   = min cost from aisle i onward, entering at edge e
    // pick[i][e] = { traversal, exitEdge } chosen at that state
    var dp = aisles.map(function () { return [Infinity, Infinity]; });
    var pick = aisles.map(function () { return [null, null]; });

    // Last aisle: just walk from the entry edge to the farthest stop.
    var lastMm = mm(N - 1);
    dp[N - 1][0] = lastMm.max - 1;       // enter left,  walk to max bay
    dp[N - 1][1] = BAYS - lastMm.min;    // enter right, walk to min bay
    pick[N - 1][0] = { traversal: 'last', exitEdge: 0 };
    pick[N - 1][1] = { traversal: 'last', exitEdge: 1 };

    for (var i = N - 2; i >= 0; i--) {
      var m = mm(i);
      for (var e = 0; e < 2; e++) {
        var costThrough = FULL;
        var totalThrough = costThrough + dp[i + 1][1 - e];
        var costInOut = (e === 0) ? 2 * (m.max - 1)
                                  : 2 * (BAYS - m.min);
        var totalInOut = costInOut + dp[i + 1][e];
        if (totalThrough <= totalInOut) {
          dp[i][e] = totalThrough;
          pick[i][e] = { traversal: 'through', exitEdge: 1 - e };
        } else {
          dp[i][e] = totalInOut;
          pick[i][e] = { traversal: 'inout', exitEdge: e };
        }
      }
    }

    // The picker always enters the warehouse at the top-left (row 1 /
    // bay 1), so the first aisle is always entered from the LEFT.  The
    // DP still optimises every subsequent through-vs-inout choice; only
    // the starting entry edge is fixed.
    var startEdge = 0;
    var plan = [];
    var cur = startEdge;
    for (var j = 0; j < N; j++) {
      var p = pick[j][cur];
      var stops = sortedStops[j].slice();
      if (cur === 1) stops.reverse(); // walked right-to-left
      plan.push({
        aisle:      aisles[j],
        entryEdge:  cur === 0 ? 'L' : 'R',
        exitEdge:   p.exitEdge === 0 ? 'L' : 'R',
        traversal:  p.traversal,
        stops:      stops
      });
      cur = p.exitEdge;
    }
    return plan;
  }
  // Convenience: flat list of stops in walk order (used for numbering)
  function flattenPlan(plan) {
    var out = [];
    plan.forEach(function (seg) {
      seg.stops.forEach(function (s) { out.push(s); });
    });
    return out;
  }

  function renderMap() {
    var canvas = document.getElementById('pk-map-canvas');
    var footer = document.getElementById('pk-map-foot');
    if (!canvas) return;

    var raw = pickedLines().filter(function (l) {
      return Number(l.whRow) > 0 && Number(l.whBay) > 0;
    });
    if (raw.length === 0) {
      canvas.innerHTML = '<div class="pk-map-empty">No mapped locations for this batch.</div>';
      footer.innerHTML = '';
      return;
    }

    // Route mode walks the picks in DP-optimised order; heatmap mode
    // keeps the raw set (order doesn't matter for density).
    var plan  = planRoute(raw);
    var lines = flattenPlan(plan);

    // SVG sizing -- paired-row layout (rows 1+2 share aisle 1, etc).
    var pad        = { l: 56, t: 30, r: 24, b: 20 };
    var cellW      = 14;   // px per bay
    var cellH      = 24;   // px per row
    var aisleGap   = 16;   // gap BETWEEN paired rows (the aisle the picker walks)
    var interGap   = 16;   // gap BETWEEN aisle pairs (structural separator)
    var STOP_R     = 9;    // dot radius
    var STOP_OFF   = 11;   // distance from rack cell edge to dot centre

    function cellY(row) {
      var y = pad.t;
      for (var i = 1; i < row; i++) {
        y += cellH;
        if (i % 2 === 1) y += aisleGap; // after rows 1, 3, 5, ... lies the aisle
        else             y += interGap; // after rows 2, 4, 6, ... is inter-aisle separator
      }
      return y;
    }
    function aisleLaneY(aisle) {
      // aisle 1 between rows 1 and 2, etc. -- y is centre of the lane.
      var topRow = 2 * aisle - 1;
      return cellY(topRow) + cellH + aisleGap / 2;
    }
    var cellX   = function (bay)  { return pad.l + (bay - 1) * cellW; };
    var centerX = function (bay) { return cellX(bay) + cellW / 2; };
    // Stops sit INSIDE the rack cell, leaning toward the aisle the picker
    // reaches from.  Odd rows face the aisle below (dot near cell bottom),
    // even rows face the aisle above (dot near cell top).  The visual
    // offset tells the picker which side of the aisle the SKU is on.
    function stopY(row) {
      if (row % 2 === 1) return cellY(row) + cellH - STOP_OFF;
      else                return cellY(row) + STOP_OFF;
    }

    // Aisle indices + walk directions (LTR / RTL) for the U-turn path.
    var occupiedAisles = {};
    raw.forEach(function (l) { occupiedAisles[Math.ceil(Number(l.whRow) / 2)] = true; });
    var aisleOrder = Object.keys(occupiedAisles).map(Number)
      .sort(function (a, b) { return a - b; });
    var aisleIdx = {};
    aisleOrder.forEach(function (a, i) { aisleIdx[a] = i; });
    function dirOf(a)    { return (aisleIdx[a] % 2 === 0) ? 'LTR' : 'RTL'; }
    function exitEdgeX(dir) {
      return dir === 'LTR' ? cellX(MAP_B) + cellW + 4 : cellX(1) - 4;
    }

    var H = cellY(MAP_W) + cellH + pad.b;
    var W = pad.l + MAP_B * cellW + pad.r;

    var s = [];
    s.push('<svg viewBox="0 0 ' + W + ' ' + H + '" width="100%" preserveAspectRatio="xMidYMid meet">');

    // Aisle lane shading + label (drawn behind rack cells so cells overlay clean)
    for (var a = 1; a <= 6; a++) {
      var laneY = cellY(2 * a - 1) + cellH;
      s.push('<rect x="' + pad.l + '" y="' + laneY + '" width="' + (MAP_B * cellW) + '" height="' + aisleGap + '" fill="#f0e6cf" opacity="0.55" />');
      s.push('<text x="' + (pad.l - 8) + '" y="' + (laneY + aisleGap / 2 + 3) + '" font-family="Oswald" font-weight="700" font-size="9" fill="#B08D57" letter-spacing="0.04em" text-anchor="end">A' + a + '</text>');
    }

    // Bay number ticks every 10
    for (var b = 0; b <= MAP_B; b += 10) {
      if (b === 0) continue;
      s.push('<text x="' + cellX(b) + '" y="' + (pad.t - 10) + '" font-family="Roboto Mono" font-size="9" fill="#616264" text-anchor="middle">' + b + '</text>');
    }
    // Row labels
    for (var r = 1; r <= MAP_W; r++) {
      s.push('<text x="' + (pad.l - 8) + '" y="' + (cellY(r) + cellH / 2 + 3) + '" font-family="Roboto Mono" font-size="9" fill="#616264" text-anchor="end">R' + pad2(r) + '</text>');
    }

    if (mapState.mode === 'heat') {
      var bin = {};
      var max = 1;
      raw.forEach(function (l) {
        var k = l.whRow + ':' + l.whBay;
        bin[k] = (bin[k] || 0) + (Number(l.need) || 1);
        if (bin[k] > max) max = bin[k];
      });
      for (var r2 = 1; r2 <= MAP_W; r2++) {
        for (var b2 = 1; b2 <= MAP_B; b2++) {
          var key = r2 + ':' + b2;
          var v = bin[key] || 0;
          var fill = '#f4f4f6';
          if (v > 0) {
            var t = v / max;
            var alpha = 0.25 + 0.75 * t;
            fill = 'rgba(200, 16, 46, ' + alpha.toFixed(3) + ')';
          }
          s.push('<rect x="' + cellX(b2) + '" y="' + cellY(r2) + '" width="' + (cellW - 1) + '" height="' + (cellH - 1) + '" fill="' + fill + '" stroke="#E2E2E5" stroke-width="0.5" />');
        }
      }
    } else {
      // Route mode: pale rack cells
      for (var r3 = 1; r3 <= MAP_W; r3++) {
        for (var b3 = 1; b3 <= MAP_B; b3++) {
          s.push('<rect x="' + cellX(b3) + '" y="' + cellY(r3) + '" width="' + (cellW - 1) + '" height="' + (cellH - 1) + '" fill="#fafafb" stroke="#E2E2E5" stroke-width="0.5" />');
        }
      }

      // Build the path from the DP-optimised plan:
      //   - Spine = dashed line ALONG the aisle lane (brass band).
      //     `through` segments extend to the opposite rack-end edge;
      //     `in-and-out` and `last` segments stop at the deepest visited
      //     bay (saving the walk to the far edge when stops cluster).
      //   - Spur  = short line from spine to each pick dot.
      //   - Cross-aisle = vertical segment at the rack-end edge that
      //     connects the exit of one aisle to the entry of the next.
      function edgeX(side) {
        return side === 'L' ? cellX(1) - 4 : cellX(MAP_B) + cellW + 4;
      }
      var pathParts = [];

      // ---- entry leg: from the dock (R01 B01) to the first aisle ----
      // The picker always starts at the top-left corner of the warehouse
      // and walks down the left edge to the first occupied aisle.
      var startDockX = centerX(1);
      var startDockY = stopY(1);                            // sits in R01 cell, aisle-facing edge
      var firstLaneY = aisleLaneY(plan[0].aisle);
      var leftEdgeX  = edgeX('L');
      pathParts.push('M ' + startDockX.toFixed(1) + ' ' + startDockY.toFixed(1) +
                     ' L ' + leftEdgeX.toFixed(1)  + ' ' + startDockY.toFixed(1) +
                     ' L ' + leftEdgeX.toFixed(1)  + ' ' + firstLaneY.toFixed(1));

      plan.forEach(function (seg, segIdx) {
        var laneY = aisleLaneY(seg.aisle);
        var entryX = edgeX(seg.entryEdge);
        var deepBay = Number(seg.stops[seg.stops.length - 1].whBay);

        var spineEndX;
        if (seg.traversal === 'through') {
          spineEndX = edgeX(seg.exitEdge);
        } else {
          // 'inout' or 'last' -- spine stops at the deepest visited bay
          spineEndX = centerX(deepBay);
        }
        pathParts.push('M ' + entryX.toFixed(1)    + ' ' + laneY.toFixed(1) +
                       ' L ' + spineEndX.toFixed(1) + ' ' + laneY.toFixed(1));

        // Cross-aisle: at the EXIT edge, regardless of traversal type
        if (segIdx < plan.length - 1) {
          var nextLaneY = aisleLaneY(plan[segIdx + 1].aisle);
          var crossX = edgeX(seg.exitEdge);
          pathParts.push('M ' + crossX.toFixed(1) + ' ' + laneY.toFixed(1) +
                         ' L ' + crossX.toFixed(1) + ' ' + nextLaneY.toFixed(1));
        }

        // Spurs to each pick dot
        seg.stops.forEach(function (l) {
          var x = centerX(Number(l.whBay));
          var y = stopY(Number(l.whRow));
          pathParts.push('M ' + x.toFixed(1) + ' ' + laneY.toFixed(1) +
                         ' L ' + x.toFixed(1) + ' ' + y.toFixed(1));
        });
      });
      s.push('<path d="' + pathParts.join(' ') + '" fill="none" stroke="#C8102E" stroke-width="1.8" stroke-dasharray="5 3" opacity="0.9" stroke-linecap="round" />');

      // START marker: green dot at R01 / B01 to show where the picker
      // enters the warehouse.  Numbered "S" so it's clearly the origin.
      s.push('<circle cx="' + startDockX + '" cy="' + startDockY + '" r="' + STOP_R + '" fill="#1F7A1F" stroke="#fff" stroke-width="1.5" />');
      s.push('<text x="' + startDockX + '" y="' + (startDockY + 4) + '" font-family="Oswald" font-weight="700" font-size="11" fill="#fff" text-anchor="middle">S</text>');

      // Stop markers in walk order, coloured by tote, sitting inside the
      // rack cell on the aisle-facing edge.
      lines.forEach(function (l, i) {
        var x = centerX(Number(l.whBay));
        var y = stopY(Number(l.whRow));
        var tote = (l.tote || 'T01').toUpperCase();
        var toteN = Number(tote.replace(/^T0?/, '')) || 1;
        var palette = ['#C8102E', '#1F7A1F', '#B08D57', '#1F4F7A',
                       '#6E1F7A', '#B0571F', '#0A0A0A', '#616264'];
        var color = palette[(toteN - 1) % palette.length];
        s.push('<circle cx="' + x + '" cy="' + y + '" r="' + STOP_R + '" fill="' + color + '" stroke="#fff" stroke-width="1.5" />');
        s.push('<text x="' + x + '" y="' + (y + 4) + '" font-family="Oswald" font-weight="700" font-size="11" fill="#fff" text-anchor="middle">' + (i + 1) + '</text>');
      });
    }

    s.push('</svg>');
    canvas.innerHTML = s.join('');

    // Footer summary -- aisle walk order, tote counts
    var byAisle = {}, byTote = {};
    lines.forEach(function (l) {
      var a = Math.ceil(Number(l.whRow) / 2);
      byAisle[a] = (byAisle[a] || 0) + 1;
      var t = (l.tote || 'T01').toUpperCase();
      byTote[t] = (byTote[t] || 0) + 1;
    });
    var aisleOrder = Object.keys(byAisle).map(Number).sort(function (a, b) { return a - b; });
    var aisleSummary = aisleOrder
      .map(function (a) { return 'A' + a + '<small>(' + byAisle[a] + ')</small>'; })
      .join(' &rarr; ');
    var toteSummary = Object.keys(byTote).sort()
      .map(function (t) { return t + '<small>(' + byTote[t] + ')</small>'; }).join(' &middot; ');
    footer.innerHTML =
      '<div><strong>' + lines.length + '</strong> stops &middot; aisles walked: ' + aisleSummary + '</div>' +
      (Object.keys(byTote).length > 1 ? '<div>totes: ' + toteSummary + '</div>' : '');
  }

  function init() {
    wireLoginEnter();
    renderLines();
  }

  window.HD_setEmp      = setEmp;
  window.HD_setWh       = setWh;
  window.HD_submitLogin = submitLogin;
  window.HD_scan        = scan;
  window.HD_scanFull    = scanFull;
  window.HD_openMap     = openMap;
  window.HD_closeMap    = closeMap;
  window.HD_setMapMode  = setMapMode;

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init);
  } else {
    init();
  }
}());
