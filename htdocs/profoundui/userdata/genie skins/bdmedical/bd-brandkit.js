/* ==========================================================================
   BD Genie brandkit - reusable skin behaviour module
   --------------------------------------------------------------------------
   Loaded from start.html BETWEEN genie.js and custom.js, so that genie.js's
   helpers exist when this parses and custom.js (loaded last) can configure it.

   Nothing in this file hardcodes a skin folder name, a set of 5250 field ids,
   or any wording - all of that is passed in by the skin's own custom.js via
   BDGenie.init(). The same file can therefore be dropped into another skin
   folder, or relocated to /profoundui/userdata/js/shared/, unchanged.
   ========================================================================== */

var BDGenie = BDGenie || {};

BDGenie.config = {
  // Absolute URL of the folder holding this skin's images. Set by custom.js.
  basePath: "",
  logo: "bd-logo.svg",

  // Sign-on card wording.
  productName: "Enterprise Systems",
  signonHeading: "Sign in",
  signonSubheading: "Advancing the world of health™",
  signonButtonText: "Sign in",
  signonQuitText: "Exit",

  // Feature switches. Both function-key affordances are independent - set
  // buildFkeyBar to false to keep only the masthead menu, or buildFkeyMenu to
  // false to keep only the bar.
  buildFkeyBar: true,
  buildFkeyMenu: true,
  // Gap in px between the last line of 5250 text and the function-key bar,
  // and between the bar and the bottom of the middle container.
  fkeyBarGap: 12,

  // Ids of the menu chrome in start.html.
  menuToggleId: "bdMenuToggle",
  menuPanelId: "bdMenuPanel",
  menuCountId: "bdMenuCount",
  showUserInHeader: true,
  headerMetaId: "bdSessionInfo",

  // 132-column mode geometry (kept identical to the stock skin's numbers so
  // wide-screen sizing behaves exactly as it did before the rebrand).
  wideDisplayWidth: 1060,
  wideDisplayHeight: "640px",

  // Sign-on card padding, in px. cardPadX must match the padding-left of
  // .bd-signon-card__brand / __heading / __sub in the CSS, so the form's label
  // column lines up with the card's own heading.
  cardPadX: 30,
  cardPadBottom: 30,
  // Must clear the card's own brand row + heading + subheading stack.
  cardHeaderSpace: 124,
  cardMinWidth: 360
};

/** Merge skin-specific overrides into the defaults above. */
BDGenie.init = function(cfg) {
  if (cfg == null) return;
  for (const key in cfg) {
    if (Object.prototype.hasOwnProperty.call(cfg, key)) BDGenie.config[key] = cfg[key];
  }
};

/** True while the current screen is the sign-on screen. Reset every screen. */
BDGenie.signonActive = false;

/** Element ids the function-key bar must never absorb. */
BDGenie.fkeyIgnoreIds = { login_button: true, quit_button: true };


/* --------------------------------------------------------------------------
   Sign-on screen
   -------------------------------------------------------------------------- */

/**
 * Rebuild the stock 5250 sign-on screen as a BD card.
 *
 * @param {object} f  Field map for this display width. See custom.js.
 * @returns {boolean} true if this screen was the sign-on screen.
 */
BDGenie.customizeSignon = function(f) {
  if (f == null || !detectScreen.apply(null, f.detect)) return false;
  BDGenie.signonActive = true;

  hideElements.apply(null, f.hide);

  // --- Labels -------------------------------------------------------------
  changeElementValue(f.userLabel, "User:");
  changeElementClass(f.userLabel, "BigText");
  setDOMAttribute(f.userLabel, "transparent", true);
  changeElementValue(f.passwordLabel, "Password:");
  changeElementClass(f.passwordLabel, "BigText");
  setDOMAttribute(f.passwordLabel, "transparent", true);

  // --- Field geometry -----------------------------------------------------
  // Two layouts exist: the normal one, and the one IBM i uses when password
  // level 3/4 gives a 128-character password split over three input lines.
  const pwContinuation = BDGenie.getPasswordContinuation(f);
  const g = pwContinuation == null ? BDGenie.geometry.standard : BDGenie.geometry.longPassword;
  const c = BDGenie.config;

  // Horizontal placement is COMPUTED, not inherited from the stock skin's
  // fixed pixel offsets: the card is centred in the 5250 container and the
  // form is then laid out inside it. The stock offsets put the form well left
  // of centre, and because the container is itself centred on the page, the
  // whole card looked off-centre in the browser.
  const container = getObj("5250");
  const containerWidth = (container != null && container.offsetWidth > 0)
    ? container.offsetWidth : 675;
  const contentWidth = g.labelWidth + g.labelGap + g.inputWidth;
  const cardWidth = Math.max(c.cardMinWidth, contentWidth + (c.cardPadX * 2));
  const cardLeft = Math.max(0, Math.round((containerWidth - cardWidth) / 2));
  const labelLeft = cardLeft + c.cardPadX;
  const inputLeft = labelLeft + g.labelWidth + g.labelGap;

  BDGenie.place(f.userLabel, labelLeft, g.userLabelTop, g.labelWidth);
  BDGenie.place(f.passwordLabel, labelLeft, g.passwordLabelTop, g.labelWidth);
  BDGenie.place(f.userInput, inputLeft, g.userInputTop, g.inputWidth);
  BDGenie.place(f.passwordInput, inputLeft, g.passwordInputTop, g.inputWidth);

  const placed = [f.userLabel, f.passwordLabel, f.userInput, f.passwordInput];
  let lastInputId = f.passwordInput;

  if (pwContinuation != null) {
    for (let i = 0; i < pwContinuation.length; i++) {
      const top = g.passwordInputTop + (g.passwordLineHeight * (i + 1));
      BDGenie.place(pwContinuation[i], inputLeft, top, g.inputWidth);
      placed.push(pwContinuation[i]);
      lastInputId = pwContinuation[i];
    }
  }

  // --- Buttons ------------------------------------------------------------
  // Positioned from the last input's real rendered box rather than from 5250
  // row/column coordinates, so they stay put in either password layout.
  const lastInput = getObj(lastInputId);
  const buttonTop = lastInput == null
    ? g.passwordInputTop + 60
    : lastInput.offsetTop + lastInput.offsetHeight + 26;

  const loginButton = newElement(1, 1, "button", c.signonButtonText, "login_button");
  loginButton.onclick = function() { pressKey("Enter"); };
  BDGenie.sizeButtonToLabel("login_button");
  BDGenie.place("login_button", inputLeft, buttonTop);
  placed.push("login_button");

  if (getObj("quit_button") != null) {
    changeElementValue("quit_button", c.signonQuitText);
    BDGenie.sizeButtonToLabel("quit_button");
    const loginWidth = getObj("login_button").offsetWidth || 96;
    BDGenie.place("quit_button", inputLeft + loginWidth + 12, buttonTop);
    placed.push("quit_button");
  }

  // --- Card ---------------------------------------------------------------
  BDGenie.buildSignonCard(placed, cardLeft, cardWidth);

  // --- Sign-on failure message -------------------------------------------
  // Surface CPF messages through Genie's alert instead of leaving raw 5250
  // text on line 24 (which the card would otherwise sit under).
  const msg = get(f.messageField);
  if (msg != "" && pui.genie.alertMsg == "") {
    pui.genie.alertMsg = msg;
    if (pui.genie.alertMsg.substr(0, 3) == "CPF") pui.genie.alertMsg = pui.genie.alertMsg.substr(8);
    if (pui.genie.alertMsg.substr(0, 1) == "-") pui.genie.alertMsg = pui.genie.alertMsg.substr(1);
  }
  hideElement(f.messageField);

  return true;
};

/**
 * Pixel geometry for the two sign-on layouts.
 *
 * Vertical values carry over from the stock skin (known-good against these
 * field ids). Horizontal values are widths and gaps only - the actual left
 * offsets are computed at runtime so the form centres itself inside the card
 * and the card centres itself in the 5250 container.
 */
BDGenie.geometry = {
  standard: {
    labelWidth: 100,          // must match .BigText's width in the CSS
    labelGap: 24,
    inputWidth: 190,
    userLabelTop: 157,
    passwordLabelTop: 196,
    userInputTop: 154,
    passwordInputTop: 193,
    passwordLineHeight: 26
  },
  longPassword: {
    labelWidth: 100,
    labelGap: 24,
    inputWidth: 190,
    userLabelTop: 137,
    passwordLabelTop: 183,
    userInputTop: 137,
    passwordInputTop: 183,
    passwordLineHeight: 26
  }
};

/**
 * Let a button size itself to its label.
 *
 * Genie writes an inline pixel width onto every button it creates, measured
 * against the 5250 prompt text. Combined with the skin's horizontal padding
 * and border-box sizing that leaves almost no content box, so a label like
 * "Sign in" renders clipped to "Sign". CSS cannot fix this - an inline style
 * wins - so the width has to be cleared in script.
 */
BDGenie.sizeButtonToLabel = function(id) {
  const obj = getObj(id);
  if (obj == null) return;
  obj.style.width = "auto";
};

/**
 * Return the continuation input ids when the screen is in 128-character
 * password mode, otherwise null.
 */
BDGenie.getPasswordContinuation = function(f) {
  if (f.passwordContinuation == null) return null;
  const ids = [f.passwordInput].concat(f.passwordContinuation);
  let total = 0;
  for (let i = 0; i < ids.length; i++) {
    const obj = getObj(ids[i]);
    if (obj == null || obj.fieldInfo == null) return null;
    total += obj.fieldInfo.size;
  }
  return total == 128 ? f.passwordContinuation.slice(0) : null;
};

/** Absolutely position a field by id. Silently ignores missing fields. */
BDGenie.place = function(id, left, top, width) {
  const obj = getObj(id);
  if (obj == null) return null;
  obj.style.left = left + "px";
  obj.style.top = top + "px";
  if (width != null) obj.style.width = width + "px";
  return obj;
};

/**
 * Draw the BD sign-on card behind a set of already-positioned elements.
 *
 * Left/width are passed in by the caller, which computed them in order to lay
 * the form out inside the card. Top/height are derived from the real bounding
 * box of the fields, so the card fits whichever password layout is in play
 * without a second set of hardcoded coordinates.
 */
BDGenie.buildSignonCard = function(ids, left, width) {
  const box = BDGenie.boundingBox(ids);
  if (box == null) return null;

  const c = BDGenie.config;
  const top = Math.max(8, box.top - c.cardHeaderSpace);
  const height = (box.bottom - top) + c.cardPadBottom;

  const card = newElement("div", "", "bd_signon_card");
  card.className = "bd-signon-card";
  card.style.position = "absolute";
  card.style.left = left + "px";
  card.style.top = top + "px";
  card.style.width = width + "px";
  card.style.height = height + "px";
  // Below the 5250 fields (z-index 10) and inputs (z-index 20) so the form
  // that sits on top of it stays interactive.
  card.style.zIndex = 5;

  card.innerHTML =
    '<div class="bd-signon-card__brand">' +
      '<img class="bd-signon-card__logo" src="' + c.basePath + c.logo + '" alt="BD" />' +
      '<span class="bd-signon-card__product">' + c.productName + '</span>' +
    '</div>' +
    '<div class="bd-signon-card__heading">' + c.signonHeading + '</div>' +
    '<div class="bd-signon-card__sub">' + c.signonSubheading + '</div>';

  return card;
};

/** Union of the offset boxes of every element id that currently exists. */
BDGenie.boundingBox = function(ids) {
  let left = null, top = null, right = null, bottom = null;
  for (let i = 0; i < ids.length; i++) {
    const obj = getObj(ids[i]);
    if (obj == null) continue;
    const l = obj.offsetLeft, t = obj.offsetTop;
    const r = l + obj.offsetWidth, b = t + obj.offsetHeight;
    if (left == null || l < left) left = l;
    if (top == null || t < top) top = t;
    if (right == null || r > right) right = r;
    if (bottom == null || b > bottom) bottom = b;
  }
  return left == null ? null : { left: left, top: top, right: right, bottom: bottom };
};


/* --------------------------------------------------------------------------
   Function-key bar
   -------------------------------------------------------------------------- */

/** Sort order for the rebuilt bar: Enter, Help, then F1..F24. */
BDGenie.fkeyRank = function(fkey) {
  if (fkey == "Enter") return -2;
  if (fkey == "Help") return -1;
  const m = /^F(\d{1,2})$/.exec(fkey);
  return m == null ? 999 : parseInt(m[1], 10);
};

/**
 * Split "F13=Information Assistant" into its key and its description.
 * Falls back gracefully when Genie is configured to hide key names, in which
 * case the label carries no "Fnn=" prefix at all.
 */
BDGenie.splitFkeyLabel = function(text, fkey) {
  const m = /^(Enter|Help|F\d{1,2})\s*=\s*(.*)$/.exec(text);
  if (m == null) return { key: fkey, description: text };
  return { key: m[1], description: m[2] == "" ? m[1] : m[2] };
};

/**
 * Find every function-key button Genie generated for this screen.
 *
 * Genie positions each one absolutely at the column its "Fnn=Text" prompt
 * occupied on the green screen, and they do not reflow off one another - so
 * any CSS that makes them wider makes them collide. The only way to give them
 * real spacing (or move them somewhere else entirely) is to hide the
 * originals and render our own.
 *
 * @returns {{keys: Array, topPx: number, leftPx: number}}
 */
BDGenie.collectFunctionKeys = function(screenDiv, hideOriginals) {
  const inputs = screenDiv.getElementsByTagName("input");
  const keys = [];
  const seen = {};
  // Genie does not add these buttons in left-to-right order, so anchor on the
  // minimum top/left across all of them, not on whichever comes first.
  let topPx = null, leftPx = null;

  for (let i = 0; i < inputs.length; i++) {
    const input = inputs[i];
    if (input.type != "button") continue;
    if (BDGenie.fkeyIgnoreIds[input.id] == true) continue;
    if (input.id.indexOf("W") != -1) continue;          // window buttons stay put
    if (input.className.indexOf("hide") >= 0) continue;
    if (input.className.indexOf("bd-fkey") >= 0) continue;   // our own, on a re-run

    // The label text is the reliable source - it always encodes the key.
    // input.fkey is only a fallback (it is absent on some PUI builds, and it
    // is the only source left when Genie is configured to hide key names).
    let fkey = null;
    const m = /^(Enter|Help|F\d{1,2})\s*=/.exec(input.value);
    if (m != null) fkey = m[1];
    else if (input.fkey != null) fkey = input.fkey;
    if (fkey == null) continue;

    const t = parseInt(input.style.top, 10) || 0;
    const l = parseInt(input.style.left, 10) || 0;
    if (topPx == null || t < topPx) topPx = t;
    if (leftPx == null || l < leftPx) leftPx = l;

    if (hideOriginals) input.style.visibility = "hidden";
    if (seen[input.value] == true) continue;            // de-dupe labels
    seen[input.value] = true;

    const parts = BDGenie.splitFkeyLabel(input.value, fkey);
    keys.push({ fkey: fkey, text: input.value, key: parts.key, description: parts.description });
  }

  keys.sort(function(a, b) { return BDGenie.fkeyRank(a.fkey) - BDGenie.fkeyRank(b.fkey); });
  return { keys: keys, topPx: topPx || 0, leftPx: leftPx || 0 };
};

/**
 * Rebuild both function-key affordances for the current screen: the pill bar
 * under the terminal, and the "Actions" menu in the masthead.
 *
 * MUST be called from pui.genie.afterInit. Called from customize() it runs
 * before Genie has created these buttons, finds nothing, and silently does
 * nothing.
 */
BDGenie.rebuildFunctionKeys = function() {
  const c = BDGenie.config;
  const screenDiv = getObj("5250");
  if (screenDiv == null) return;

  const existing = getObj("bdFkeyBar");
  if (existing != null && existing.parentNode != null) existing.parentNode.removeChild(existing);

  if (!c.buildFkeyBar && !c.buildFkeyMenu) return;

  // Originals are hidden whenever we render at least one replacement,
  // otherwise the screen would show both.
  const found = BDGenie.collectFunctionKeys(screenDiv, true);

  if (c.buildFkeyMenu) BDGenie.renderFkeyMenu(found.keys);
  if (c.buildFkeyBar && found.keys.length > 0) {
    BDGenie.renderFkeyBar(screenDiv, found.keys, found.topPx, found.leftPx);
  }
};

/** Lay the collected keys out as a flex-wrapped pill bar under the terminal. */
BDGenie.renderFkeyBar = function(screenDiv, keys, topPx, leftPx) {
  const bar = document.createElement("div");
  bar.id = "bdFkeyBar";
  bar.className = "bd-fkey-bar";
  bar.style.position = "absolute";
  bar.style.left = leftPx + "px";
  bar.style.right = "6px";

  // Anchor BELOW everything still on screen, not at the row the original
  // prompts occupied. Function-key prompts routinely share a row with real
  // text - the IBM i Main Menu puts "F23=Set initial menu" on row 24
  // alongside the IBM copyright line - and the rebuilt bar is taller than the
  // one or two text rows it replaces, so anchoring at the old top drops the
  // second row of pills straight on top of that text.
  const contentBottom = BDGenie.contentBottom(screenDiv);
  bar.style.top = Math.max(topPx, contentBottom + BDGenie.config.fkeyBarGap) + "px";

  keys.forEach(function(f) {
    const btn = document.createElement("input");
    btn.type = "button";
    btn.value = f.text;
    btn.className = "bd-fkey" + BDGenie.fkeyModifier(f.fkey, "bd-fkey");
    btn.onclick = function() { pressKey(f.fkey); };
    bar.appendChild(btn);
  });

  screenDiv.appendChild(bar);
  BDGenie.reserveRoomFor(bar);
};

/** Affirmative keys read as primary, back-out keys as muted. */
BDGenie.fkeyModifier = function(fkey, base) {
  if (fkey == "Enter") return " " + base + "--enter";
  if (fkey == "F3" || fkey == "F12") return " " + base + "--exit";
  return "";
};

/**
 * Bottom edge of the lowest visible field in the 5250 area, in px.
 * Fields already hidden (including the original function-key buttons, hidden
 * just above) and zero-sized fields are ignored.
 */
BDGenie.contentBottom = function(screenDiv) {
  let bottom = 0;
  const kids = screenDiv.children;
  for (let i = 0; i < kids.length; i++) {
    const el = kids[i];
    if (el.id == "bdFkeyBar") continue;
    if (el.style.visibility == "hidden" || el.style.display == "none") continue;
    if (el.offsetWidth == 0 && el.offsetHeight == 0) continue;
    const b = el.offsetTop + el.offsetHeight;
    if (b > bottom) bottom = b;
  }
  return bottom;
};

/**
 * Grow the middle container if the (absolutely positioned) bar now extends
 * past it, so it cannot collide with the page footer.
 *
 * Safe to leave set: customize() runs before afterInit on every screen and
 * clears the height again via applyDisplaySizing().
 */
BDGenie.reserveRoomFor = function(bar) {
  const middle = pui.genie.middleDiv;
  if (middle == null) return;
  const needed = bar.offsetTop + bar.offsetHeight + BDGenie.config.fkeyBarGap;
  if (needed > middle.offsetHeight) middle.style.height = needed + "px";
};


/* --------------------------------------------------------------------------
   Function-key menu ("hamburger") in the masthead
   -------------------------------------------------------------------------- */

BDGenie.menuOpen = false;
BDGenie.menuWired = false;

/** The three chrome elements the menu needs, or null if start.html lacks them. */
BDGenie.menuParts = function() {
  const c = BDGenie.config;
  const toggle = document.getElementById(c.menuToggleId);
  const panel = document.getElementById(c.menuPanelId);
  if (toggle == null || panel == null) return null;
  return { toggle: toggle, panel: panel, count: document.getElementById(c.menuCountId) };
};

/**
 * Repopulate the masthead menu for the current screen.
 *
 * The chrome lives in start.html and survives every screen change; only the
 * items are rebuilt. Screens with no function keys hide the toggle entirely,
 * so the header never advertises an empty menu.
 */
BDGenie.renderFkeyMenu = function(keys) {
  const parts = BDGenie.menuParts();
  if (parts == null) return;
  BDGenie.wireMenu();
  BDGenie.closeMenu();

  parts.panel.innerHTML = "";

  if (keys.length == 0) {
    parts.toggle.hidden = true;
    parts.panel.hidden = true;
    return;
  }
  parts.toggle.hidden = false;
  if (parts.count != null) parts.count.textContent = String(keys.length);

  keys.forEach(function(f) {
    const item = document.createElement("button");
    item.type = "button";
    item.className = "bd-menu-item" + BDGenie.fkeyModifier(f.fkey, "bd-menu-item");
    item.setAttribute("role", "menuitem");
    item.title = f.text;

    const key = document.createElement("span");
    key.className = "bd-menu-item__key";
    key.textContent = f.key;

    const label = document.createElement("span");
    label.className = "bd-menu-item__text";
    label.textContent = f.description;

    item.appendChild(key);
    item.appendChild(label);
    item.onclick = function() {
      // Do not restore focus: pressKey submits and Genie focuses the next
      // screen itself.
      BDGenie.closeMenu();
      pressKey(f.fkey);
    };
    parts.panel.appendChild(item);
  });
};

/** Attach the toggle/outside-click/keyboard handlers exactly once. */
BDGenie.wireMenu = function() {
  if (BDGenie.menuWired) return;
  const parts = BDGenie.menuParts();
  if (parts == null) return;
  BDGenie.menuWired = true;

  parts.toggle.onclick = function(e) {
    e.preventDefault();
    e.stopPropagation();
    if (BDGenie.menuOpen) BDGenie.closeMenu(true); else BDGenie.openMenu();
  };

  document.addEventListener("click", function(e) {
    if (!BDGenie.menuOpen) return;
    if (parts.panel.contains(e.target) || parts.toggle.contains(e.target)) return;
    BDGenie.closeMenu();
  });

  // Capture phase, so Escape and the arrow keys are handled here instead of
  // being swallowed by Genie's own 5250 key handling - but only while the
  // menu is actually open, so normal typing is never affected.
  document.addEventListener("keydown", function(e) {
    if (!BDGenie.menuOpen) return;
    if (e.key == "Escape") {
      BDGenie.closeMenu(true);
      e.preventDefault();
      e.stopPropagation();
      return;
    }
    if (e.key == "ArrowDown" || e.key == "ArrowUp") {
      BDGenie.moveMenuFocus(e.key == "ArrowDown" ? 1 : -1);
      e.preventDefault();
      e.stopPropagation();
    }
  }, true);
};

BDGenie.openMenu = function() {
  const parts = BDGenie.menuParts();
  if (parts == null || parts.toggle.hidden) return;
  BDGenie.menuOpen = true;
  parts.panel.hidden = false;
  parts.toggle.setAttribute("aria-expanded", "true");
  parts.toggle.className = "bd-menu-toggle is-open";
  const first = parts.panel.querySelector(".bd-menu-item");
  if (first != null) first.focus();
};

BDGenie.closeMenu = function(restoreFocus) {
  const parts = BDGenie.menuParts();
  if (parts == null) return;
  BDGenie.menuOpen = false;
  parts.panel.hidden = true;
  parts.toggle.setAttribute("aria-expanded", "false");
  parts.toggle.className = "bd-menu-toggle";
  if (restoreFocus === true) parts.toggle.focus();
};

BDGenie.moveMenuFocus = function(step) {
  const parts = BDGenie.menuParts();
  if (parts == null) return;
  const items = parts.panel.querySelectorAll(".bd-menu-item");
  if (items.length == 0) return;
  let index = -1;
  for (let i = 0; i < items.length; i++) {
    if (items[i] === document.activeElement) { index = i; break; }
  }
  index = (index + step + items.length) % items.length;
  items[index].focus();
};


/* --------------------------------------------------------------------------
   Header session info
   -------------------------------------------------------------------------- */

/** Show the signed-on user and system in the start.html masthead. */
BDGenie.showSessionInfo = function() {
  if (!BDGenie.config.showUserInHeader) return;
  const meta = document.getElementById(BDGenie.config.headerMetaId);
  if (meta == null) return;

  let user = "";
  try {
    if (pui["appJob"] != null && pui["appJob"]["user"] != null) user = pui["appJob"]["user"];
  }
  catch (e) { user = ""; }

  meta.innerHTML = user == ""
    ? ""
    : "Signed in as <strong>" + user + "</strong>";
};


/* --------------------------------------------------------------------------
   Display sizing
   -------------------------------------------------------------------------- */

/**
 * 132x27 mode centring. Same approach (and same pixel width) as the stock
 * skin - deliberately not replaced with a CSS-based scheme, because mixing
 * two centring mechanisms on the same div is how these skins break.
 */
BDGenie.applyDisplaySizing = function() {
  const div5250 = getObj("5250");
  if (div5250 == null) return;

  if (pui.genie.displaySize == 132 && !pui.genie.config.useAjax) {
    pui.genie.middleDiv.style.height = BDGenie.config.wideDisplayHeight;
    div5250.style.position = "absolute";
    let position = (screen.width - BDGenie.config.wideDisplayWidth) / 2;
    if (position < 0) position = 0;
    div5250.style.left = position + "px";
  }
  else if (pui.genie.middleDiv != null) {
    pui.genie.middleDiv.style.height = "";
    pui.genie.middleDiv.style.width = "";
    div5250.style.position = "relative";
    div5250.style.left = "";
    div5250.style.width = "";
  }
};


/* --------------------------------------------------------------------------
   Wiring
   -------------------------------------------------------------------------- */

/**
 * Install the afterInit hook. pui.genie.afterInit is a single global (not a
 * list), so the assignment is guarded - but the assigned function itself runs
 * on every subsequent screen.
 */
BDGenie.installAfterInit = function() {
  if (pui.genie.afterInit != null) return;
  pui.genie.afterInit = function() {
    if (BDGenie.signonActive) BDGenie.renderFkeyMenu([]);   // no menu on sign-on
    else BDGenie.rebuildFunctionKeys();
    BDGenie.showSessionInfo();
  };
};

/**
 * Everything a skin needs on every screen. Called from custom.js's
 * customize(), with that skin's sign-on field map.
 */
BDGenie.customize = function(signonFields) {
  BDGenie.signonActive = false;
  // The masthead survives screen changes, so an open menu would otherwise
  // hang over the screen that replaced the one it belongs to.
  BDGenie.closeMenu();
  BDGenie.installAfterInit();
  BDGenie.customizeSignon(signonFields);
  BDGenie.applyDisplaySizing();
};
