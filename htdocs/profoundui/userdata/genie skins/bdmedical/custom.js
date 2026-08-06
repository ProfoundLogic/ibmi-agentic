/* ==========================================================================
   BD Medical skin - custom.js
   --------------------------------------------------------------------------
   Thin per-skin configuration layer over bd-brandkit.js (loaded ahead of this
   file by start.html). The only things that belong here are values that are
   genuinely specific to THIS skin: its folder name, its wording, and the 5250
   field ids of the sign-on screen at this display width.

   Field ids are position-dependent (D_<row>_<col> / I_<row>_<col>) and do not
   transfer between display widths - the 132-column variant of this same
   screen lives in "adjusted columns custom.js", where every id is shifted by
   one column.
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

/** Sign-on screen field map - standard (non column-adjusted) layout. */
const bdSignonFields = {
  detect: [
    "D_1_23", "             Sign On",
    "D_2_48", "System  . . . . . :",
    "D_3_48", "Subsystem . . . . :",
    "D_4_48", "Display . . . . . :"
  ],
  hide: [
    "D_1_23", "D_2_48", "D_3_48", "D_4_48", "D_2_70", "D_3_70", "D_4_70",
    "I_11_53", "I_12_53", "D_11_17", "D_12_17",
    "D_8_17", "D_9_17", "D_10_17", "I_8_53", "I_9_53", "I_10_53", "D_24_40"
  ],
  userLabel: "D_6_17",
  passwordLabel: "D_7_17",
  userInput: "I_6_53",
  passwordInput: "I_7_53",
  // Present only when the system uses 128-character passwords.
  passwordContinuation: ["I_8_1", "I_9_1"],
  messageField: "D_24_1"
};

// eslint-disable-next-line no-unused-vars
function customize() {
  pui["loading animation"]["css"] = "pui-bd-animation";
  BDGenie.customize(bdSignonFields);
}
