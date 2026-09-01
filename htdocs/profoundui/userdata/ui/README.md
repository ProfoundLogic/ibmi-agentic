# EJS screen assets

Templates, stylesheets and images for the Profound UI EJS screens. **codermake does
not deploy this tree** — it only builds IBM i objects. Editing a `.ejs` or `.css`
here and rebuilding will appear to change nothing.

## The cycle

```bash
tools/ejs-preflight.sh            # validate templates against their RDF JSON
tools/deploy-ejs.sh               # pre-flight, copy to the PUI docroot, verify
```

`deploy-ejs.sh` runs the pre-flight itself, so in practice just run the deploy.

## Pre-flight

`tools/ejs-preflight.py` builds a data context **from the RDF JSON field list** and
renders each template through `aitool ejs-validate`. That makes it a contract test,
not just a syntax check: a template referencing a field the RDF never declares fails
here, with a file and line number, instead of rendering a blank screen in front of a
customer. It also checks that every `css` and `js` URL in the JSON resolves to a real
file, and strips `?v=` cache-bust strings when mapping URLs to paths.

## Conventions

Learned the hard way from the `wrkcuste` reference screens — see GJA-914.

- **Scope every rule to a wrapper class.** No `html`/`body` rules, no `position:fixed`,
  no `100vh`. All of them fight the Genie skin.
- **Never reuse a generic class name.** `.header`, `.toolbar`, `.btn`, `.message` and
  `.footer` are declared *globally* in both reference stylesheets with conflicting
  rules. The Genie shim injects all snapshotted CSS globally, so they already collide.
  Use a screen prefix: `.flparts-`, `.flfleet-`, `.flmach-`, `.fldup-`.
- **Keep your own reset weaker than your layout rules.** `.x-screen div {padding:0}`
  has specificity (0,1,1) and silently beats `.x-body {padding:20px}` at (0,1,0).
- **Guard every optional value**: `<% if (typeof x !== 'undefined' && x) { %>`. One
  undefined identifier aborts the entire render.
- **Screens must render with no JS.** None of the Fletcher screens ship a `js` array.
  Files in that array load *before* the template renders, so `addEventListener` binds
  to nothing and fails silently — that is why the reference screen's column sorting
  has never worked (GJA-891). Use inline `onclick` calling `pui.submit(...)`.
- **Cache-bust** `template`, `css` and `js` URLs in the JSON whenever you edit an asset.
- Subfile inputs are named `<subfile>.<field>.<%= row._rrn %>`; row properties arrive
  **lowercase**.

## Layout

```
fletcher/   shared theme tokens + logo (brand palette from jhfletcher.com)
flfleet/    Fleet 360 - fleet.ejs/.css, machine.ejs/.css
flparts/    Parts Finder - parts.ejs/.css, dupes.ejs/.css
wrkcuste/   pre-existing cfdemo reference screens (not Fletcher)
```
