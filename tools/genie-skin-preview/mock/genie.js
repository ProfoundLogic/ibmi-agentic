/* Local test harness only - a stand-in for Profound UI's genie.js.
   Implements just enough of the runtime API that a skin's custom.js exercises,
   plus two synthetic 5250 screens rendered with real Genie-style field ids. */

var multX = 8, multY = 23;

var pui = {
  genie: {
    displaySize: 80,
    config: { useAjax: false },
    middleDiv: null,
    alertMsg: "",
    afterInit: null,
    isCustomized: false
  },
  "loading animation": {},
  "appJob": { user: "MSCHUTT" },
  getWindowSize: function() { return { width: window.innerWidth, height: window.innerHeight }; }
};

function getObj(id) { return document.getElementById(id); }
function get(id) { var o = getObj(id); return o == null ? "" : (o.value != null && o.tagName == "INPUT" ? o.value : o.textContent); }
function pressKey(k) { console.log("pressKey(" + k + ")"); }

function detectScreen() {
  for (var i = 0; i < arguments.length; i += 2) {
    var o = getObj(arguments[i]);
    if (o == null) return false;
    if (o.textContent != arguments[i + 1]) return false;
  }
  return true;
}

function hideElement(id) { var o = getObj(id); if (o) o.style.visibility = "hidden"; }
function hideElements() { for (var i = 0; i < arguments.length; i++) hideElement(arguments[i]); }
function changeElementValue(id, v) { var o = getObj(id); if (o) { if (o.tagName == "INPUT") o.value = v; else o.innerHTML = v; } }
function changeElementClass(id, c) { var o = getObj(id); if (o) o.className = c; }
function setDOMAttribute(id, a, v) { var o = getObj(id); if (o && a == "transparent" && v) o.style.background = "transparent"; }
function moveElement(id, row, col) { var o = getObj(id); if (o) { o.style.top = ((row - 1) * multY) + "px"; o.style.left = ((col - 1) * multX) + "px"; } }

function newElement(a, b, c, d, e) {
  var host = getObj("5250"), el;
  if (typeof a == "string") {                       // newElement(type, content, id)
    el = a == "img" ? document.createElement("img") : document.createElement(a);
    if (a == "img") el.src = b; else el.innerHTML = b == null ? "" : b;
    if (c) el.id = c;
    el.style.position = "absolute";
    host.appendChild(el);
    return el;
  }
  var row = a, col = b, type = c, value = d, id = e; // newElement(row, col, type, value, id)
  if (type == "button") {
    el = document.createElement("input"); el.type = "button"; el.value = value; el.className = "button";
    el.style.width = (String(value).length * multX) + "px";   // Genie writes an inline width
  }
  else if (type == "img") { el = document.createElement("img"); el.src = value; }
  else { el = document.createElement(type); el.innerHTML = value; }
  if (id) el.id = id;
  el.style.position = "absolute";
  el.style.top = ((row - 1) * multY) + "px";
  el.style.left = ((col - 1) * multX) + "px";
  host.appendChild(el);
  return el;
}

/* ---------------------------------------------------------------- screens */

function fld(tag, row, col, text, cls, id, widthChars) {
  var host = getObj("5250");
  var el = document.createElement(tag);
  el.id = id || (tag == "input" ? "I_" : "D_") + row + "_" + col;
  if (tag == "input") { el.type = "text"; el.value = text; el.className = cls || "input A26"; el.style.width = ((widthChars || text.length || 10) * multX) + "px"; el.fieldInfo = { size: widthChars || 10 }; }
  else { el.textContent = text; el.className = cls || "A20"; }
  el.style.position = "absolute";
  el.style.top = ((row - 1) * multY) + "px";
  el.style.left = ((col - 1) * multX) + "px";
  host.appendChild(el);
  return el;
}

function renderSignOn() {
  fld("div", 1, 23, "             Sign On", "A22");
  fld("div", 2, 48, "System  . . . . . :", "A20");
  fld("div", 2, 70, "IDEV", "A22");
  fld("div", 3, 48, "Subsystem . . . . :", "A20");
  fld("div", 3, 70, "QINTER", "A22");
  fld("div", 4, 48, "Display . . . . . :", "A20");
  fld("div", 4, 70, "QPADEV0012", "A22");
  fld("div", 6, 17, "User  . . . . . . . . . . . . . .", "A20");
  fld("input", 6, 53, "", "input A26", "I_6_53", 10);
  fld("div", 7, 17, "Password  . . . . . . . . . . . .", "A20");
  fld("input", 7, 53, "", "input A27", "I_7_53", 10);
  fld("div", 8, 17, "Program/procedure . . . . . . . .", "A20");
  fld("input", 8, 53, "", "input A26", "I_8_53", 10);
  fld("div", 9, 17, "Menu  . . . . . . . . . . . . . .", "A20");
  fld("input", 9, 53, "", "input A26", "I_9_53", 10);
  fld("div", 10, 17, "Current library . . . . . . . . .", "A20");
  fld("input", 10, 53, "", "input A26", "I_10_53", 10);
  fld("div", 11, 17, "", "A20", "D_11_17");
  fld("input", 11, 53, "", "input A26", "I_11_53", 10);
  fld("div", 12, 17, "", "A20", "D_12_17");
  fld("input", 12, 53, "", "input A26", "I_12_53", 10);
  fld("div", 24, 1, "CPF1107 - Password not correct for user profile.", "A28", "D_24_1");
  fld("div", 24, 40, "(C) COPYRIGHT IBM CORP. 1980, 2024.", "A20", "D_24_40");

  var quit = document.createElement("input");
  quit.type = "button"; quit.value = "Quit"; quit.className = "button"; quit.id = "quit_button";
  quit.style.position = "absolute"; quit.style.top = (11 * multY) + "px"; quit.style.left = (49 * multX) + "px";
  quit.style.width = (4 * multX) + "px";                      // Genie writes an inline width
  getObj("5250").appendChild(quit);
}

function renderSignOn128() {
  renderSignOn();
  getObj("I_7_53").fieldInfo.size = 52;
  fld("input", 8, 1, "", "input A27", "I_8_1", 52).fieldInfo.size = 52;
  fld("input", 9, 1, "", "input A27", "I_9_1", 24).fieldInfo.size = 24;
}

/* Same sign-on screen as renderSignOn(), with every field id shifted one
   column left - the layout Genie produces when column adjustment is on. */
function renderSignOnAdj() {
  fld("div", 1, 22, "             Sign On", "A22", "D_1_22");
  fld("div", 2, 47, "System  . . . . . :", "A20", "D_2_47");
  fld("div", 2, 69, "IDEV", "A22", "D_2_69");
  fld("div", 3, 47, "Subsystem . . . . :", "A20", "D_3_47");
  fld("div", 3, 69, "QINTER", "A22", "D_3_69");
  fld("div", 4, 47, "Display . . . . . :", "A20", "D_4_47");
  fld("div", 4, 69, "QPADEV0012", "A22", "D_4_69");
  fld("div", 6, 16, "User  . . . . . . . . . . . . . .", "A20", "D_6_16");
  fld("input", 6, 52, "", "input A26", "I_6_52", 10);
  fld("div", 7, 16, "Password  . . . . . . . . . . . .", "A20", "D_7_16");
  fld("input", 7, 52, "", "input A27", "I_7_52", 10);
  fld("div", 8, 16, "Program/procedure . . . . . . . .", "A20", "D_8_16");
  fld("input", 8, 52, "", "input A26", "I_8_52", 10);
  fld("div", 9, 16, "Menu  . . . . . . . . . . . . . .", "A20", "D_9_16");
  fld("input", 9, 52, "", "input A26", "I_9_52", 10);
  fld("div", 10, 16, "Current library . . . . . . . . .", "A20", "D_10_16");
  fld("input", 10, 52, "", "input A26", "I_10_52", 10);
  fld("div", 11, 16, "", "A20", "D_11_16");
  fld("input", 11, 52, "", "input A26", "I_11_52", 10);
  fld("div", 12, 16, "", "A20", "D_12_16");
  fld("input", 12, 52, "", "input A26", "I_12_52", 10);
  fld("div", 24, 0, "CPF1120 - User profile does not exist.", "A28", "D_24_0");
  fld("div", 24, 39, "(C) COPYRIGHT IBM CORP. 1980, 2024.", "A20", "D_24_39");

  var quit = document.createElement("input");
  quit.type = "button"; quit.value = "Quit"; quit.className = "button"; quit.id = "quit_button";
  quit.style.position = "absolute"; quit.style.top = (11 * multY) + "px"; quit.style.left = (48 * multX) + "px";
  quit.style.width = (4 * multX) + "px";                      // Genie writes an inline width
  getObj("5250").appendChild(quit);
}

function renderAppScreen() {
  fld("div", 1, 2, "WRKCUST", "A20");
  fld("div", 1, 28, "Work with Customers", "A22");
  fld("div", 1, 66, "10/08/26", "A20");
  fld("div", 2, 66, "14:22:07", "A20");
  fld("div", 3, 2, "Type options, press Enter.", "A20");
  fld("div", 4, 4, "2=Change   4=Delete   5=Display   8=Order history", "A3A");

  fld("div", 6, 2, "Opt", "A22");
  fld("div", 6, 7, "Customer", "A22");
  fld("div", 6, 18, "Name", "A22");
  fld("div", 6, 46, "Territory", "A22");
  fld("div", 6, 58, "Balance", "A22");
  fld("div", 6, 70, "Status", "A22");
  fld("div", 7, 2, "___ ________ ___________________________ __________ __________ _________", "A30");

  var rows = [
    ["10041", "Grand River Hospital", "ON-CENTRAL", "48,210.55", "ACTIVE", "A20"],
    ["10058", "CHU de Quebec", "QC-EAST", "12,904.00", "ACTIVE", "A20"],
    ["10063", "Alberta Health Services", "AB-SOUTH", "301,772.18", "HOLD", "A32"],
    ["10077", "Vancouver Coastal Health", "BC-WEST", "0.00", "ACTIVE", "A20"],
    ["10094", "Winnipeg Regional HA", "MB-CENTRAL", "88,145.90", "OVERDUE", "A28"],
    ["10102", "Nova Scotia Health", "NS-EAST", "5,600.25", "ACTIVE", "A20"],
    ["10118", "Saskatchewan Cancer Agency", "SK-NORTH", "27,330.00", "REVIEW", "A30"],
    ["10125", "Horizon Health Network", "NB-EAST", "9,875.40", "ACTIVE", "A20"]
  ];
  for (var i = 0; i < rows.length; i++) {
    var r = 8 + i;
    fld("input", r, 3, "", "input A26", "I_" + r + "_3", 2);
    fld("div", r, 7, rows[i][0], "A20");
    fld("div", r, 18, rows[i][1], "A20");
    fld("div", r, 46, rows[i][2], "A20");
    fld("div", r, 58, rows[i][3].padStart(11, " "), "A20");
    fld("div", r, 70, rows[i][4], rows[i][5]);
  }

  fld("div", 18, 2, "Position to customer  . . . .", "A20");
  fld("input", 18, 32, "", "input A26", "I_18_32", 8);
  fld("div", 19, 2, "Territory . . . . . . . . . .", "A20");
  fld("input", 19, 32, "", "input A26", "I_19_32", 10);
  fld("div", 21, 2, "Customer 10094 is past due 62 days.", "A28");
  fld("div", 22, 2, "More...", "A22");

  // Genie renders each "Fnn=Text" prompt as its own absolutely-positioned
  // button at that prompt's original column - exactly what this reproduces.
  var keys = [
    ["F3", "F3=Exit", 2], ["F5", "F5=Refresh", 12], ["F6", "F6=Create", 26],
    ["F9", "F9=Retrieve", 40], ["F11", "F11=Alt view", 55], ["F12", "F12=Cancel", 70],
    ["F17", "F17=Subset", 2], ["F21", "F21=Print list", 16], ["F24", "F24=More keys", 33]
  ];
  for (var k = 0; k < keys.length; k++) {
    var b = document.createElement("input");
    b.type = "button"; b.value = keys[k][1]; b.className = "button"; b.fkey = keys[k][0];
    b.style.position = "absolute";
    b.style.top = ((k < 6 ? 23 : 24) - 1) * multY + "px";
    b.style.left = (keys[k][2] - 1) * multX + "px";
    getObj("5250").appendChild(b);
  }
}

/* The IBM i Main Menu: function-key prompts on rows 23 AND 24, with the IBM
   copyright line sharing row 24. This is the shape that exposes a rebuilt
   function-key bar overlapping real screen text. */
function renderMainMenu() {
  fld("div", 1, 2, "MAIN", "A3A");
  fld("div", 1, 31, "IBM i Main Menu", "A22");
  fld("div", 2, 57, "System:", "A20");
  fld("div", 2, 68, "PROFND01", "A22");
  fld("div", 3, 2, "Select one of the following:", "A3A");

  var opts = [
    "1. User tasks", "2. Office tasks", "3. General system tasks",
    "4. Files, libraries, and folders", "5. Programming", "6. Communications",
    "7. Define or change the system", "8. Problem handling", "9. Display a menu",
    "10. Information Assistant options", "11. IBM i Access tasks"
  ];
  for (var i = 0; i < opts.length; i++) {
    var el = fld("div", 5 + i, 7, "", "A20", "D_" + (5 + i) + "_7");
    el.innerHTML = '<a href="#">' + opts[i] + '</a>';   // Genie makes menu options clickable
  }
  var so = fld("div", 18, 7, "", "A20", "D_18_7");
  so.innerHTML = '<a href="#">90. Sign off</a>';

  fld("div", 20, 2, "Selection or command", "A20");
  fld("div", 21, 2, "===>", "A20");
  fld("input", 21, 7, "", "input A26", "I_21_7", 74);
  fld("input", 22, 1, "", "input A26", "I_22_1", 80);

  // Row 24 carries BOTH an F23 prompt (col 1) and the copyright (col 40).
  fld("div", 24, 40, "(C) COPYRIGHT IBM CORP. 1980, 2018.", "A20", "D_24_40");

  var keys = [
    ["F3", "F3=Exit", 1, 23], ["F4", "F4=Prompt", 10, 23], ["F9", "F9=Retrieve", 23, 23],
    ["F12", "F12=Cancel", 37, 23], ["F13", "F13=Information Assistant", 51, 23],
    ["F23", "F23=Set initial menu", 1, 24]
  ];
  for (var k = 0; k < keys.length; k++) {
    var b = document.createElement("input");
    b.type = "button"; b.value = keys[k][1]; b.className = "button"; b.fkey = keys[k][0];
    b.style.position = "absolute";
    b.style.top = ((keys[k][3] - 1) * multY) + "px";
    b.style.left = ((keys[k][2] - 1) * multX) + "px";
    b.style.width = (String(keys[k][1]).length * multX) + "px";
    getObj("5250").appendChild(b);
  }
}

window.addEventListener("DOMContentLoaded", function() {
  var screenName = new URLSearchParams(location.search).get("screen") || "signon";
  pui.genie.middleDiv = getObj("middle");
  if (screenName == "signon") renderSignOn();
  else if (screenName == "signon128") renderSignOn128();
  else if (screenName == "signonadj") renderSignOnAdj();
  else if (screenName == "mainmenu") renderMainMenu();
  else renderAppScreen();
  getObj("5250").style.visibility = "visible";   // genie.js reveals it after render
  if (typeof customize == "function") customize();
  if (typeof pui.genie.afterInit == "function") pui.genie.afterInit();
});
