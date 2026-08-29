/* ==========================================================================
   BD Medical skin - "adjusted columns" custom.js
   --------------------------------------------------------------------------
   Column-adjusted counterpart of custom.js, shipped alongside it exactly as
   the stock Skyline skin this skin was copied from does. It is byte-for-byte
   the same logic; only the 5250 field ids differ, every one of them shifted
   one column left (D_1_23 -> D_1_22, I_6_53 -> I_6_52, and so on), because
   Genie numbers fields differently when column adjustment is in effect.

   Swap this file in for custom.js when config.js has "adjustColumns": true.
   ========================================================================== */

BDGenie.init({
  basePath: "/profoundui/userdata/genie skins/bdmedical/",
  logo: "bd-logo.svg",
  productName: "Enterprise Systems",
  signonHeading: "Sign in",
  signonSubheading: "Advancing the world of health™",
  signonButtonText: "Sign in",
  signonQuitText: "Exit",

  // Function keys are surfaced through the "Actions" menu above the screen
  // only. Set buildFkeyBar back to true to also get the pill bar under the
  // terminal; running both at once just duplicates the same list.
  buildFkeyMenu: true,
  buildFkeyBar: false
});

/** Sign-on screen field map - column-adjusted layout. */
const bdSignonFields = {
  detect: [
    "D_1_22", "             Sign On",
    "D_2_47", "System  . . . . . :",
    "D_3_47", "Subsystem . . . . :",
    "D_4_47", "Display . . . . . :"
  ],
  hide: [
    "D_1_22", "D_2_47", "D_3_47", "D_4_47", "D_2_69", "D_3_69", "D_4_69",
    "I_11_52", "I_12_52", "D_11_16", "D_12_16",
    "D_8_16", "D_9_16", "D_10_16", "I_8_52", "I_9_52", "I_10_52", "D_24_39"
  ],
  userLabel: "D_6_16",
  passwordLabel: "D_7_16",
  userInput: "I_6_52",
  passwordInput: "I_7_52",
  // Present only when the system uses 128-character passwords.
  passwordContinuation: ["I_8_0", "I_9_0"],
  messageField: "D_24_0"
};

// eslint-disable-next-line no-unused-vars
function customize() {
  pui["loading animation"]["css"] = "pui-bd-animation";
  BDGenie.customize(bdSignonFields);
}
