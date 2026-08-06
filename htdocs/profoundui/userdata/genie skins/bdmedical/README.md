# BD Medical — Genie skin

A Profound UI Genie skin branded for **BD (Becton, Dickinson and Company)**, built
for prospect demos. Derived from the stock **Skyline** skin, then re-skinned end to
end against BD's live design system.

## Deploying

Copy this whole folder to the IFS of the Genie instance:

```
/www/<instance>/htdocs/profoundui/userdata/genie skins/bdmedical/
```

Launch with `http://<system>:<port>/profoundui/Genie?skin=bdmedical`, or set
**bdmedical** as the default skin in the Genie Administrator (Global options →
Default Skin).

Every asset URL in `start.html` carries a `?v=` cache-buster (currently `v=6`).
**Bump that number on every edit** — several PUI/Apache instances serve skin
`.css`/`.js` with no cache-control headers, so browsers otherwise keep serving the
previous copy and the edit looks like it did nothing.

## Files

| File | Purpose |
|---|---|
| `start.html` | Page shell: BD masthead, the Actions menu strip above the screen, footer, and the required `<div id="5250">`. |
| `bdmedical.css` | Whole stylesheet: BD palette, typography, 5250 attribute-byte mapping, sign-on card, function-key bar, Actions menu, loading spinner. |
| `bd-brandkit.js` | Reusable, skin-agnostic behaviour module (sign-on rebuild, function-key bar and menu, session info, display sizing). Hardcodes no skin name, no field ids, no wording. |
| `custom.js` | Thin per-skin config: folder path, wording, and the sign-on field ids for the standard layout. |
| `adjusted columns custom.js` | Same, for the column-adjusted layout (every field id shifted one column left). Swap in for `custom.js` when `config.js` has `"adjustColumns": true`. |
| `config.js` | Genie Administrator-managed settings. Not part of the visual skin — see the note below. |
| `bd-logo-white.svg` | Official BD logo lockup, all-white, for the navy masthead. |
| `bd-logo.svg` | Official BD logo, full colour, for the sign-on card. |
| `fonts/JetBrainsMono-latin.woff2` | Self-hosted terminal typeface (31 KB, variable 100–800, latin subset). |
| `fonts/OFL.txt` | SIL Open Font License 1.1 for JetBrains Mono. |

Load order in `start.html` matters and must not change:
`profoundui.css` → `bdmedical.css`, then `genie.js` → `bd-brandkit.js` → `custom.js`.

`bd-brandkit.js` is deliberately free of skin-specific values, so it can be
relocated verbatim to `/profoundui/userdata/js/shared/` and shared with other
skins; only the `<script src>` in `start.html` would change.

## Brand reference

Colours and type are taken from the CSS custom properties published by
`www.bd.com` (`clientlib-base`), not from a third-party colour-picker site:

| Token | Hex | BD name |
|---|---|---|
| `--bd-navy` | `#060A3D` | BD Navy (`--primary-navy`) |
| `--bd-blue` | `#044ED7` | BD Boosted Blue (`--primary-boosted-bd-blue`) |
| `--bd-blue-pressed` | `#05207A` | Interactive pressed |
| `--bd-orange` | `#FF6E00` | BD Boosted Orange (`--primary-boosted-bd-orange`) |
| `--bd-teal` | `#39BEAB` | BD Teal |
| `--bd-purple` / `--bd-magenta` | `#5A398D` / `#8D2A90` | BD Purple / Magenta |
| `--bd-warm-grey` | `#D8D8D7` | `--primary-warm-grey` |
| `--bd-grey-xlight` | `#F8F7F7` | `--extra-light-grey` |
| `--bd-grey-dark` | `#657278` | `--neutral-dark-grey` |

Two typefaces are in play.

**Chrome** (masthead, footer, sign-on card, Actions menu, window titles) uses BD's
own **FS Albert Pro**, with BD's published fallback chain
(`"FS Albert Pro", "Gill Sans", "Gill Sans MT", "Calibri", "Trebuchet MS", …`).
FS Albert Pro is licensed and is **not** redistributed here — BD-managed desktops
that have it installed pick it up automatically; everyone else lands on the same
fallbacks bd.com uses.

**The terminal** uses **JetBrains Mono**, self-hosted from `fonts/`. A monospace is
non-negotiable — Genie lays every field out on a fixed character grid — but a bare
system stack lands on whatever each machine happens to have, frequently Courier New,
which is the dated look this skin exists to remove. Self-hosting means every
workstation renders identically. JetBrains Mono was chosen for a tall x-height (it
reads larger than other faces at the same 13px) and unambiguous `0/O` and `1/l/I`,
which matters when the screen is full of account numbers and amounts. It is SIL
OFL 1.1, so redistribution is permitted; the licence ships in `fonts/OFL.txt`.

The `-dark` tokens (`--bd-orange-dark`, `--bd-teal-dark`, `--bd-amber-dark`) are
derived shades used only where a brand hue is too light to read as body text on
white. The brand hues themselves are never altered.

## Notes on specific decisions

**5250 text is monospaced, chrome is not.** Genie lays fields out on a fixed
character grid (`column × pui.multX`, ~8px). The stock skin only put a monospace
font on `INPUT` fields and left output fields on the proportional body font, which
makes wide subfile columns collide. `[id="5250"]` therefore sets 13px monospace —
the same size the stock skin already used for inputs. Masthead, footer, sign-on
card, function-key bar and window chrome all set the BD brand font explicitly and
so override it.

**Three constraints bind the terminal font.** Measured, not assumed — the preview
harness asserts all three on every screen:

1. *Advance width.* JetBrains Mono's advance is `0.6em`, i.e. 7.8px at 13px, which
   the browser rounds to the 8px cell — an exact grid fit. **13px is the ceiling**:
   `8 / 0.6 = 13.33px`, above which glyphs outgrow the cell and columns collide.
   Any replacement face must be checked the same way.
2. *Bold must not be wider.* Headings are `700`. Regular and bold come from one
   variable file and measure identically; a family whose bold is wider would knock
   every heading out of alignment.
3. *No ligatures.* JetBrains Mono is a coding face, and the IBM i command line is
   literally `===>`. A ligature draws several characters as one glyph and breaks the
   grid. This subset turns out not to ligate `===>` even with features on, so the
   suppression is insurance rather than a fix — but keep it.

**`[id="5250"]`, not `#5250`.** A CSS id selector cannot begin with a digit, so
`#5250` is invalid and any rule using it is discarded silently.

**Function-key buttons are rebuilt, not restyled.** Genie positions each
auto-generated `Fnn=` button absolutely at the column its prompt occupied on the
green screen; they never reflow, so widening them in CSS just makes them overlap.
`bd-brandkit.js` hides the originals and lays out its own flex-wrapped pill bar —
from `pui.genie.afterInit`, which is the only hook that runs after Genie has
created those buttons.

**The bar anchors below the screen, not at the old prompt rows.** Function-key
prompts routinely share a row with real text — the IBM i Main Menu puts
`F23=Set initial menu` on row 24 next to the IBM copyright line — and the rebuilt
bar is taller than the one or two text rows it replaces. Anchored at the original
top it drops its second row of pills straight onto that text. `BDGenie.contentBottom`
measures the lowest still-visible field and the bar goes below it, then
`BDGenie.reserveRoomFor` grows `.middleDiv` so the bar cannot collide with the page
footer either. The gap is `BDGenie.config.fkeyBarGap`.

**The Actions menu is chrome + data, split.** The hamburger button and its panel
are static markup in `start.html`; `bd-brandkit.js` fills the panel from whatever
function keys the current screen has, and hides the toggle entirely on screens with
none (including sign-on). The chrome outlives every screen change, so the menu is
explicitly closed in `customize()` — otherwise an open panel would hang over the
screen that replaced the one it belongs to.

**The menu sits in its own strip, width-matched to the terminal.** `.bd-screenbar`
is `675px` — the width of `.insideDiv`, *not* the wider `.middleDiv` that wraps it —
and centred the same way, so the button's right edge lines up exactly with the right
edge of the screen and the panel opens right over the content. In the masthead it
was correct but far away: the header runs to `1400px` while the terminal is `675px`,
which left the button a long way from the screen the user is actually reading. The
strip also carries a `min-height`, so the terminal does not shift vertically between
screens that have function keys and screens that do not.

**Only the menu is active.** `custom.js` sets `buildFkeyBar: false`. Both
affordances are still implemented and independently switchable — running both just
duplicates the same list:

```js
BDGenie.init({ buildFkeyBar: true });     // also show the pill bar under the screen
BDGenie.init({ buildFkeyMenu: false });   // pill bar only, no menu
```

Note the consequence of hiding Genie's own buttons with the bar off: the
`F3=Exit F4=Prompt …` line no longer appears at the bottom of the screen at all,
because those buttons *are* that text once Genie converts it. Every key is in the
menu with its description, and the physical F-keys still work.

**Attribute colours keep their meaning.** All of `A20`–`A3E` are retained and
still resolve to visibly distinct hues, so an application using colour to encode
state (red = overdue, yellow = on hold, turquoise = review) reads the same as it
did before.

**The sign-on layout is computed, not hardcoded.** The card is centred in the 5250
container and the form is laid out inside it, so the whole thing lands on the page
centre. The stock skin's fixed pixel offsets (labels at x=63) put the form well
left of centre. `BDGenie.config.cardPadX` must stay equal to the `padding-left` of
`.bd-signon-card__brand` / `__heading` / `__sub`, and `geometry.labelWidth` must
stay equal to `.BigText`'s `width`, or the form stops lining up with the card's own
heading.

**`.insideDiv` needs auto margins.** It is 675px inside a 775px `.middleDiv`, and as
a plain block it sits flush left — which pushes the whole terminal 50px left of the
page centre even though `.middleDiv` itself is centred.

**Anything overlaying the terminal needs its own stacking context.** 5250 fields
carry `z-index` 10–40 and live in the *root* stacking context — `.insideDiv` is
`position: relative` with `z-index: auto`, which does not create one. `.bd-screenbar`
therefore takes `z-index: 60` (and `.bd-header` `z-index: 100`); without it the
Actions panel paints *underneath* the terminal text. The panel itself sits at
`z-index: 1000` inside that context.

**Genie writes an inline `width` on every button it creates**, measured against the
5250 prompt text. Combined with the skin's horizontal padding and `border-box`
sizing that leaves almost no content box, so a label like "Sign in" renders clipped
to "Sign". CSS cannot fix it — an inline style wins — so `BDGenie.sizeButtonToLabel`
clears the width in script. Any new button the skin styles with padding needs the
same treatment.

## `config.js` — one suggested change, not applied

`config.js` is Genie Administrator-managed and has been left exactly as it was.
It still carries the Skyline accent colour for subfile highlighting:

```json
"hiColor":"#DD5700"
```

Changing that to a BD colour (`#044ED7`) is worth doing before a demo, but should
be done through the Genie Administrator rather than by hand-editing this file.
