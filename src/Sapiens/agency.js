/* wtagtcfe / agency.js  —  v2 rebuild
 * Minimal client behavior:
 *   - ESC submits an EXIT action
 *   - Enter on a row submits SELECT with that row's srrn
 *   - selectRow() helper used by row onclick from the EJS
 *   - Suppress the print dialog that PUI key bindings can trigger.
 */
(function () {
  "use strict";

  // Some PUI key-binding paths call window.print() — neutralise it so a
  // stray click/keystroke doesn't open the browser print dialog.
  try { window.print = function () {}; } catch (_) {}
  window.addEventListener("beforeprint", function (e) {
    try { e.preventDefault(); } catch (_) {}
    return false;
  }, true);
  window.addEventListener("keydown", function (e) {
    // Block Ctrl/Cmd+P explicitly
    if ((e.ctrlKey || e.metaKey) && (e.key === "p" || e.key === "P")) {
      e.preventDefault();
      e.stopPropagation();
      return false;
    }
  }, true);

  if (!window.wtagtcfe) window.wtagtcfe = {};

  window.wtagtcfe.selectRow = function (rrn, srrn) {
    var hidden = document.getElementById("ag-selrrn");
    if (hidden) hidden.value = srrn;
    if (typeof pui !== "undefined" && typeof pui.submit === "function") {
      pui.submit({ action: "SELECT", selrrn: srrn });
    }
  };

  // ----- Edit / New modal ----------------------------------------------
  function setVal(id, v) {
    var n = document.getElementById(id);
    if (!n) return;
    var s = (v == null) ? "" : String(v);
    n.value = s;
    // Also poke the attribute and fire input/change so PUI's two-way
    // binding picks the new value up. PUI ignores .value-only mutations
    // for some inputs.
    try { n.setAttribute("value", s); } catch (_) {}
    try { n.dispatchEvent(new Event("input",  { bubbles: true })); } catch (_) {}
    try { n.dispatchEvent(new Event("change", { bubbles: true })); } catch (_) {}
  }
  function getCarrier() { return document.getElementById("ag-detail-data"); }
  function getAttr(src, a, fallback) {
    if (!src) return fallback || "";
    var v = src.getAttribute(a);
    return (v == null || v === "") ? (fallback || "") : v;
  }
  function setMode(mode) {
    var m = document.getElementById("ag-modal");
    if (!m) return;
    m.setAttribute("data-mode", mode);
    var title  = document.getElementById("ag-modal-title");
    var sub    = document.getElementById("ag-modal-sub");
    var save   = document.getElementById("ag-modal-save");
    var newOnly = m.querySelectorAll(".ag-new-only");
    if (mode === "new") {
      if (title) title.textContent = "New agency";
      if (sub)   sub.textContent   = "Create a record in WMAGP";
      if (save)  save.textContent  = "Create agency";
      newOnly.forEach(function (el) { el.removeAttribute("hidden"); });
    } else {
      if (title) title.textContent = "Edit agency";
      if (save)  save.textContent  = "Save changes";
      newOnly.forEach(function (el) { el.setAttribute("hidden", ""); });
    }
  }

  window.wtagtcfe.openEdit = function () {
    var m = document.getElementById("ag-modal");
    if (!m) return;
    var src = getCarrier();
    var selrrn = getAttr(src, "data-selrrn", "0");
    if (!selrrn || selrrn === "0") {
      alert("Select an agency from the list first.");
      return;
    }
    var name = getAttr(src, "data-name", "");
    if (!name) {
      alert("The selected row has no agency name on file. Pick a different row.");
      return;
    }
    setMode("edit");
    setVal("ag-ename",    name);
    setVal("ag-econtact", getAttr(src, "data-contact", ""));
    setVal("ag-eemail",   getAttr(src, "data-email",   ""));
    setVal("ag-ecity",    getAttr(src, "data-city",    ""));
    setVal("ag-estate",   getAttr(src, "data-state",   ""));
    setVal("ag-estatus",  getAttr(src, "data-status",  "A"));
    setVal("ag-efein", "");
    setVal("ag-emod",  "0");
    var sub = document.getElementById("ag-modal-sub");
    if (sub) sub.textContent = name;
    m.removeAttribute("hidden");
    setTimeout(function () { var n = document.getElementById("ag-ename"); if (n) n.focus(); }, 50);
  };

  window.wtagtcfe.openNew = function () {
    var m = document.getElementById("ag-modal");
    if (!m) return;
    setMode("new");
    // Clear all fields for a fresh record
    setVal("ag-efein",    "");
    setVal("ag-emod",     "0");
    setVal("ag-ename",    "");
    setVal("ag-econtact", "");
    setVal("ag-eemail",   "");
    setVal("ag-ecity",    "");
    setVal("ag-estate",   "");
    setVal("ag-estatus",  "A");
    m.removeAttribute("hidden");
    setTimeout(function () { var n = document.getElementById("ag-efein"); if (n) n.focus(); }, 50);
  };

  window.wtagtcfe.closeEdit = function () {
    var m = document.getElementById("ag-modal");
    if (m) m.setAttribute("hidden", "");
  };

  window.wtagtcfe.saveEdit = function (e) {
    if (e && e.preventDefault) e.preventDefault();
    var modal = document.getElementById("ag-modal");
    var mode  = modal && modal.getAttribute("data-mode") === "new" ? "new" : "edit";
    var src   = getCarrier();

    function fld(formId, dataAttr, fallback) {
      var n = document.getElementById(formId);
      var v = n ? (n.value || "") : "";
      if (v && v.trim()) return v;
      return getAttr(src, dataAttr, fallback || "");
    }

    var ename, econtact, eemail, ecity, estate, estatus;
    if (mode === "edit") {
      // For edits, ALWAYS fall back to the carrier so a half-bound form
      // can't blank the record.
      ename    = fld("ag-ename",    "data-name");
      econtact = fld("ag-econtact", "data-contact");
      eemail   = fld("ag-eemail",   "data-email");
      ecity    = fld("ag-ecity",    "data-city");
      estate   = fld("ag-estate",   "data-state").toUpperCase();
      estatus  = fld("ag-estatus",  "data-status", "A");
    } else {
      ename    = (document.getElementById("ag-ename")    || {}).value || "";
      econtact = (document.getElementById("ag-econtact") || {}).value || "";
      eemail   = (document.getElementById("ag-eemail")   || {}).value || "";
      ecity    = (document.getElementById("ag-ecity")    || {}).value || "";
      estate   = ((document.getElementById("ag-estate")  || {}).value || "").toUpperCase();
      estatus  = (document.getElementById("ag-estatus")  || {}).value || "A";
    }

    if (mode === "new") {
      var efein = ((document.getElementById("ag-efein") || {}).value || "").trim();
      var emod  = ((document.getElementById("ag-emod")  || {}).value || "0").trim();
      if (!efein || !/^\d{2}-?\d{7}$/.test(efein)) {
        alert("Please enter a valid Federal EIN (format: XX-XXXXXXX).");
        var f = document.getElementById("ag-efein"); if (f) f.focus();
        return;
      }
      if (!ename) {
        alert("Agency name is required.");
        var n = document.getElementById("ag-ename"); if (n) n.focus();
        return;
      }
      if (typeof pui !== "undefined" && typeof pui.submit === "function") {
        pui.submit({
          action: "CREATE",
          efein: efein, emod: emod,
          ename: ename, econtact: econtact, eemail: eemail,
          ecity: ecity, estate: estate, estatus: estatus
        });
      }
      return;
    }

    if (typeof pui !== "undefined" && typeof pui.submit === "function") {
      var src = document.getElementById("ag-detail-data");
      var selrrn = src ? (src.getAttribute("data-selrrn") || "0") : "0";
      pui.submit({
        action: "SAVE",
        selrrn: selrrn,
        ename: ename, econtact: econtact, eemail: eemail,
        ecity: ecity, estate: estate, estatus: estatus
      });
    }
  };

  document.addEventListener("keydown", function (e) {
    if (e.key === "Escape") {
      if (typeof pui !== "undefined" && typeof pui.submit === "function") {
        pui.submit({ action: "EXIT" });
      }
    }
  });

  // Live-filter the search box client-side (visual hint only — the real
  // filter runs server-side when the user presses Search/Enter).
  document.addEventListener("DOMContentLoaded", function () {
    var input = document.getElementById("ag-sfilter");
    if (!input) return;
    input.addEventListener("keydown", function (e) {
      if (e.key === "Enter") {
        e.preventDefault();
        if (typeof pui !== "undefined" && typeof pui.submit === "function") {
          pui.submit({ action: "SEARCH" });
        }
      }
    });
  });
})();
