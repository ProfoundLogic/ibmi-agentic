/* =========================================================================
   wtagtcfe / agency.js - row interaction + keyboard shortcuts for the
   modern Agency Configuration screen.

   Profound UI loads this once per screen render. Anything that touches
   the DOM must defer until after the EJS template has rendered.
   ========================================================================= */
(function () {
  "use strict";

  // Namespace used by the inline onclick handlers in the EJS template.
  var ns = (window.wtagtcfe = window.wtagtcfe || {});

  // ---- row selection -----------------------------------------------------
  //
  // The EJS template puts onclick="wtagtcfe.selectRow(_rrn, srrn)" on each
  // row. We set the screen's selrrn field, submit a SELECT action, and let
  // the RPG come back with the populated detail pane.
  ns.selectRow = function (sflRrn, srrn) {
    try {
      // Show immediate visual feedback before the round trip.
      document.querySelectorAll(".ag-row.is-selected").forEach(function (el) {
        el.classList.remove("is-selected");
      });
      var row = document.querySelector('.ag-row[data-rrn="' + srrn + '"]');
      if (row) row.classList.add("is-selected");

      pui.submit({ action: "SELECT", selrrn: srrn });
    } catch (e) {
      console && console.warn && console.warn("wtagtcfe.selectRow failed", e);
    }
  };

  // ---- keyboard shortcuts ------------------------------------------------
  function onKey(e) {
    // Ignore if user is typing in a control.
    var t = e.target;
    var typing = t && (t.tagName === "INPUT" || t.tagName === "TEXTAREA" ||
                       t.isContentEditable);

    // ESC -> Exit
    if (e.key === "Escape") {
      e.preventDefault();
      try { pui.submit({ action: "EXIT" }); } catch (_) {}
      return;
    }

    // / -> focus filter (only when not typing)
    if (e.key === "/" && !typing) {
      var box = document.getElementById("ag-sfilter");
      if (box) { e.preventDefault(); box.focus(); box.select(); }
      return;
    }

    // Enter inside the filter box -> submit search
    if (e.key === "Enter" && t && t.id === "ag-sfilter") {
      e.preventDefault();
      try { pui.submit({ action: "SEARCH" }); } catch (_) {}
      return;
    }

    // n -> New
    if ((e.key === "n" || e.key === "N") && !typing && (e.metaKey || e.ctrlKey) === false) {
      e.preventDefault();
      try { pui.submit({ action: "NEW" }); } catch (_) {}
      return;
    }
  }

  function install() {
    // Avoid double-binding when PUI re-runs the script on each response.
    if (window.__wtagtcfeKeyBound) return;
    window.__wtagtcfeKeyBound = true;
    document.addEventListener("keydown", onKey);
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", install);
  } else {
    install();
  }
})();
