# Genie skin preview harness

Renders a Genie skin offline — no IBM i, no Profound UI instance, no manual
browser session — so a skin can be checked before it is deployed.

It serves the skin folder over `http://127.0.0.1:8799` with a stand-in
`genie.js` that draws synthetic 5250 screens using real Genie field ids
(`D_<row>_<col>` / `I_<row>_<col>`) and real attribute classes (`A20`…`A3E`),
runs the skin's own `customize()` and `pui.genie.afterInit`, then screenshots
each screen and probes the resulting DOM.

## Running

```sh
npm i playwright-core
npx playwright install chromium --only-shell
node tools/genie-skin-preview/preview.js [skinName] [outDir]
```

Defaults: skin `bdmedical`, output `tools/genie-skin-preview/out/`.

## What it checks

Screenshots plus a JSON probe per screen: whether the sign-on card was built and
where, whether the function-key bar was built and how many original buttons it
hid, the header session text, that the loading-spinner class resolves to a real
`@keyframes`, the computed colour of each attribute class, and whether the page
overflows horizontally. Any page error, console error, failed request or HTTP
4xx/5xx is collected and printed at the end.

Cases currently covered: standard sign-on, 128-character-password sign-on,
column-adjusted sign-on (loads `adjusted columns custom.js` in place of
`custom.js`), an application/subfile screen with function keys, and that same
screen at a narrow viewport.

## Limits

`mock/genie.js` is a stand-in, not Profound UI. It implements only the helper
API a `custom.js` actually calls (`detectScreen`, `getObj`, `get`,
`hideElement(s)`, `changeElementValue`, `changeElementClass`,
`setDOMAttribute`, `moveElement`, `newElement`, `pressKey`, and the `pui.genie`
state a skin reads). `mock/profoundui.css` is an empty stub, so widget styling
that comes from the real `profoundui.css` is not represented. Field ids and
`pui.multX`/`multY` are assumed to be the common 80×24 values.

Treat a pass here as "the skin's own HTML/CSS/JS is internally correct and lays
out as intended" — not as a substitute for loading it in a real Genie session.

## Adding screens

Add a `renderXxx()` function in `mock/genie.js`, dispatch it from the
`DOMContentLoaded` handler on a new `?screen=` value, and add a case to the
`cases` array in `preview.js`.
