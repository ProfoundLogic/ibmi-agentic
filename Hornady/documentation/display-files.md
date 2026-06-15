# Display Files

12 display files. Ten are classic 5250 DDS (with `SFL` subfiles, `SFLCTL`, and `WINDOW*` pop-up records); two are Profound UI variants that embed JSON layout metadata inside `HTML(...)` keywords.

## Naming pairs

For the `HYR0xxx` shipping screens, the program and its display file share the number — `HYR0600.SQLRPGLE` ↔ `HYD0600A.DSPF` (+ `HYD0600B.DSPF` for the second view), and so on.

| Program | Display file(s) | Reference file (REF) |
|---|---|---|
| `HYR0189` | `HYD0189.DSPF` | `*LIBL/FLDREF` |
| `HYR0520` | `HYD0520.DSPF` | (likely `HDCCMT` per program code) |
| `HYR0600` | `HYD0600A.DSPF`, `HYD0600B.DSPF` | `*LIBL/HYPSGCU`, `*LIBL/HYPSGDT` |
| `HYR0602` | `HYD0602A.DSPF`, `HYD0602B.DSPF` | `*LIBL/HYPSGDT` |
| `HYR0604` *(external)* | `HYD0604A.DSPF` | (referenced from HYR0602) |
| `HYR0606` | `HYD0606.DSPF` | `*LIBL/HYPSGLD` |
| `HYR0608` | `HYD0608A.DSPF`, `HYD0608B.DSPF` | `*LIBL/HYPSGTD` |
| `PICKBATR` / `PICKBATDR` / `PICKBATSV` | `PICKBATD.DSPF` (Profound UI) | `PICKBATHP` |
| `PICKERR` | `PICKERD.DSPF` (Profound UI) | n/a (JSON-driven) |

## Record formats per DSPF

Verified record-format names (`A R xxxx`) from each member:

### `HYD0189.DSPF` — Order/customer comments

```
SFL01 / SFLCTL01 / SFLCMD01     -- primary subfile (comments list)
MSF01 / MSFCTL01                -- message subfile
```

- Screen size: 24×80 (`DSPSIZ(24 80 *DS3)`)
- Reference file: `FLDREF`
- Function keys: `CF06 'Print'`, `CF11 'Accept'`, `CA12 'Previous'`, `CA17 'Show all comments'`, `CA18 'Show customer comments only'`

### `HYD0520.DSPF` — Sales ship-via / comments confirmation

```
SFL01 / SFLCTL01 / SFLCMD01     -- primary subfile
MSF01 / MSFCTL01                -- message subfile
```

### `HYD0600A.DSPF` — Shipment Processing main (27×132)

```
SFL01A / SFLCTL01A / SFLNOR01 / SFLCMD01   -- subfile A (main list)
SFL02A / SFLCTL02A / SFLCMD02              -- subfile B
SFL03A / SFLCTL03A / SFLCMD03              -- subfile C
WINDOW01 / WINDOW02 / WINDOW03 / WINDOW04  -- pop-ups
DSPBLANKA                                  -- blank/scroll record
MSF01A / MSFCTL01A                         -- message subfile
```

The main subfile pulls from `HYPSGCU`. Hidden subfile fields like `S1BTNM`, `S1DSHP`, `S1STNM`, `S1STZP`, `S1SVDS`, `S1FRZN`, `S1TSTP2` all `REFFLD()` from `HYPSGCU` columns (`GCBTNM`, `GCDSHP`, …).

### `HYD0600B.DSPF` — Shipment Processing alt view (27×132)

```
DSPFMT2A / DSPFMT3A / DSPFMT3B / DSPFMT4A / DSPFMT5A / DSPFMT6A
WINDOW03B / WINDOW05B
DSPBLANKB
MSF01B / MSFCTL01B
```

### `HYD0602A.DSPF` — Shipment Detail (27×132)

```
SFL01A / SFLCTL01A / SFLNOR01 / SFLCMD01
SFL02A / SFLCTL02A / SFLCMD02
WINDOW01 / WINDOW01A / WINDOW01B / WINDOW01C / WINDOW03
DSPBLANKA
MSF01A / MSFCTL01A
```

Subfile fields reference `HYPSGDT` (`GDRSEQ`, `GDOCTL`, `GDBLN#`, `GDITEM`, `GDQOPN`, `GDQOPK`, `GDQSTC`, …).

### `HYD0602B.DSPF` — Shipment Detail alt view

```
DSPFMT2A / DSPFMT3A / DSPFMT3B / DSPFMT4A / DSPFMT4B
DSPBLANKB
MSF01B / MSFCTL01B
```

### `HYD0604A.DSPF` — Shipment 0604 (consumer of HYR0604, external program)

```
SFL01 / SFLCTL01 / SFLCMD01
SFL02 / SFLCTL02 / SFLCMD02
DSPBLANKB
MSF01A / MSFCTL01A
SCRN3 / SCRN3CMD                -- third pure-record screen
```

### `HYD0606.DSPF` — Shipment Lot Inquiry

```
SFL01 / SFLCTL01 / SFLNOR01 / SFLCMD01
DSPFMT3A
DSPBLANK
MSF01 / MSFCTL01
```

### `HYD0608A.DSPF` — Tote Processing

```
SFL01A / SFLCTL01A
SFL01B / SFLCTL01B               -- second subfile variant
SFLNOR01 / SFLCMD01
SFL02A / SFLCTL02A / SFLCMD02
WINDOW01 / WINDOW02
DSPBLANKA
MSF01A / MSFCTL01A
```

### `HYD0608B.DSPF` — Tote Processing alt view

```
DSPFMT2A / DSPFMT3A
DSPBLANKB
MSF01B / MSFCTL01B
```

### `PICKBATD.DSPF` — Pick Batch Dashboard (Profound UI)

```
SFL                              -- main subfile
QSFLCTL001                       -- subfile control (auto-named by Profound)
CTL                              -- batch-list control
DETAIL                           -- detail panel
DELETE                           -- delete confirmation
MESSAGE                          -- message area
BATCH_MSG                        -- batch-level message
ASSIGN_BAT                       -- assign-batch dialog
COMP_BATCH                       -- complete-batch dialog
SHIP_VIA                         -- ship-via picker
```

References `PICKBATHP` for fields. Hidden fields like `BATCHCRTT`, `CUSTNAME`, `ORDLIST`, `ICONCOLOR` etc. drive the JSON-rendered UI (the dashboard has tooltips, status icons, and delete/edit buttons).

### `PICKERD.DSPF` — Picker Workflow (Profound UI, mobile-scanner)

```
LOGINR                           -- login screen
HOMESFLR / QSFLCTL001 / HOMER    -- home dashboard
PICKDTLR / QSFLCTL002 / PICKSUMR -- pick detail + summary
SCANNEDR / QSFLCTL003 / ITEMDTLR -- scanned-items + item detail
PICKDONER                        -- pick complete
MESSENGER                        -- inline message
SKIPCONF                         -- skip-item confirmation
UPICKCONF                        -- un-pick confirmation
TOTEIDREC                        -- tote ID entry
TOTEDONE                         -- tote complete
```

Records are largely defined via `HTML('{"screen":…JSON…}')` — Profound UI parses the JSON at runtime and presents a mobile-responsive UI. Notable DDS keywords:

```
A*PUI                            CANVASHEIGHT(1920)
A*PUI                            CANVASWIDTH(1200)
A*PUI                            LONGNAMEALIASES
```

`LONGNAMEALIASES` lets the program use long field names that aren't valid in pure DDS.

## Patterns

- **Window records** (`WINDOW01`, `WINDOW02`, …) on the classic screens are 5250 pop-up overlays. `HYR0600`/`HYR0602`/`HYR0608` use several to confirm complete-shipment actions, prompt for tracking numbers, etc. Header comments in `HYR0600` mention `WINDOW05B` was added in April 2022 as a "generic" window for new use cases — these screens are still being actively extended in 2024.
- **Two-DSPF pairs (A / B)** — most shipment screens (`HYD0600`, `HYD0602`, `HYD0608`) split a primary view (A) and an alternate "extra-fields" view (B) so the program can toggle between them. The A views are the data-entry side; the B views show pick-related context (added Sep 2022 per comment).
- **`MSF01*` / `MSFCTL01*`** — every classic screen has a 1-line message subfile in the same DDS member, fed by `QMHSNDPM` from the RPG. This is the standard MCA pattern for inline error messaging.
- **Modern vs classic split** — the Profound UI screens reference `PICKBATHP.PF` directly (it's a small new table). The classic shipping screens reference `HYPSGCU/HYPSGDT/HYPSGLD/HYPSGTD` (external work files). The two UI styles co-exist; only the picking workflow has been modernized to Profound UI so far.
