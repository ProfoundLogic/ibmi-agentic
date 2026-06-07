/*
 * wtagtcfg-shim.js - Adds an Exit button + ESC keyboard shortcut to
 * the Profound UI render so the user can leave WTAGTCFG cleanly.
 *
 * REVERSE-ENGINEERED. Real Sapiens deployments rely on the action
 * panel (rendered by STACTPNL + LoadActionBar) to surface F3 / F12
 * exit keys. Our stub service program is a no-op for LoadActionBar
 * and our stub STACTPNL has no UI, so the user has no exit path.
 * This shim manufactures one.
 *
 * Loaded via <script> tag in:
 *   /www/profoundui/htdocs/profoundui/userdata/html/start.html
 */
(function () {
  "use strict";

  // Wait until PUI runtime is available and a screen has rendered.
  function ready(cb) {
    if (typeof pui !== "undefined" && pui.runtimeContainer) cb();
    else setTimeout(function () { ready(cb); }, 200);
  }

  function makeExitButton() {
    if (document.getElementById("wtagtcfgShimExit")) return;
    var btn = document.createElement("button");
    btn.id = "wtagtcfgShimExit";
    btn.textContent = "Exit (ESC)";
    btn.title = "Fire BtnCancel response — leaves WTAGTCFG and returns";
    Object.assign(btn.style, {
      position: "fixed",
      top: "12px",
      right: "16px",
      width: "120px",
      height: "32px",
      background: "#c44",
      color: "#fff",
      border: "1px solid #a22",
      borderRadius: "4px",
      fontWeight: "600",
      fontSize: "13px",
      cursor: "pointer",
      zIndex: "10001",
      boxShadow: "0 2px 6px rgba(0,0,0,0.25)",
    });
    btn.addEventListener("click", fireCancel);
    document.body.appendChild(btn);
  }

  // Try the hardest to fire BtnCancel. PUI exposes several APIs that
  // can submit a response indicator on a hidden button.
  function fireCancel() {
    try {
      // First choice: pui.click(id) - calls the button's click handler
      if (typeof pui !== "undefined" && typeof pui.click === "function") {
        pui.click("BtnCancel");
        return;
      }
    } catch (e) {}

    try {
      // Fallback: set the field value directly then submit
      if (typeof pui !== "undefined" && typeof pui.setField === "function") {
        pui.setField("BtnCancel", "1");
      }
      if (typeof pui !== "undefined" && typeof pui.respond === "function") {
        pui.respond();
        return;
      }
    } catch (e) {}

    // Last resort: build the form post that we know the program needs
    // and submit via fetch.
    try {
      var body = "RCDAGTCFG.BTNAGTADD=0&RCDAGTCFG.BTNACAN=0&RCDAGTCFG.BTNAGTSEL=0"
               + "&RCDAGTCFG.SCCHGFEIN=0&RCDAGTCFG.BTNCANCEL=1"
               + "&aid=241&row=1&column=1&toprrn.1=1";
      fetch(location.pathname.replace(/\/[^/]+$/, "/respond"), {
        method: "POST",
        headers: { "Content-Type": "application/x-www-form-urlencoded" },
        body: body,
        credentials: "same-origin",
      }).then(function () { location.reload(); });
    } catch (e) {
      alert("Could not fire BtnCancel response. Close the browser tab to exit.");
    }
  }

  document.addEventListener("keydown", function (e) {
    // ESC fires Cancel
    if (e.key === "Escape") {
      e.preventDefault();
      fireCancel();
    }
  });

  ready(function () {
    // After every screen response, ensure our button still exists
    // (PUI re-renders the body content on every response).
    var orig = pui.respondingNow;
    setInterval(makeExitButton, 1000);
    makeExitButton();
  });
})();
