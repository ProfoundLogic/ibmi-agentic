# `HYR0600` — what it would take to compile

Audit of `HYR0600.SQLRPGLE` (7,580 source lines) against what we have in the package.

## TL;DR

| | In package | Missing |
|---|---|---|
| Display files | `HYD0600A`, `HYD0600B` | — |
| Logical files (F-spec) | `HYLSGCU1` `HYLSGCU2` `HYLSGCU3` `HYLSGCU4` `HYLSGCU12` `HYLSGCU13` `HYLSGCU14` `HYLSGDT3` | `HYLSGCU11` |
| Parent PFs | (none) | `HYPSGCU` `HYPSGHD` `HYPSGDT` `HYPSGLD` `HYPSGTC` `HYPSGTS` `HYPSCWK` `HYPSSCC` `HYPPCLS` `HYPRCLT` `HYPSTRK` `HYPSVCT` `HYPTDTA` `HYPIDTA` `HYPMILG` `HYPINTR` |
| SQL-referenced external tables | (none) | `HDCUST` `HDCCMT` `HDDSHP` `HDIMST` `HDIWHS` `HDSHPV` `OEORHD` `OEOCMT` `OECMWK` `OEDTWK` `OEHDWK` `GUPTDAT` `HREMPL` `HYW0189` `HYW0610A` |
| Field reference file | — | `FLDREF` |
| `/COPY` members | source as `HYR0610` / `HYR0614` / `HYR9930` | needs to land in `QPRPSRC` as separate members |
| Service-procedure calls | `HYR0189` `HYR0520` `HYR0602` `HYR0608` `VPRBLDP` `HYC0138` (source-available, but each has same data-dep issue) | `HHDCSS` `HHDSVI` `HOEDIP` |
| IBM APIs | `QCMDEXC` `QMHRMVPM` `QMHSNDPM` | — (system) |

What I produced this turn:

- `documentation/ddl/fldref.pf` — skeleton field-reference file.
- `documentation/ddl/hypsgcu.sql` — skeleton DDL for `HYPSGCU`.
- `documentation/ddl/hypsghd.sql` — skeleton DDL for `HYPSGHD`.
- `documentation/ddl/hypsgdt.sql` — skeleton DDL for `HYPSGDT`.
- `documentation/ddl/hypsgld.sql` — skeleton DDL for `HYPSGLD`.
- `documentation/ddl/hypsgtd.sql` — skeleton DDL for `HYPSGTD`.
- `documentation/ddl/hypsgsd.sql` — skeleton DDL for `HYPSGSD`.
- `documentation/ddl/hypsghd_create.sql` — combined drop+create wrapper.
- `documentation/ddl/README.md` — how the drafts were derived, type-inference rules, promotion plan.

These are skeletons — they cover the keys + field references I can defend from in-package evidence. They are NOT promoted into `ibmi-agentic/src/` yet on purpose; promoting them is a deliberate next step.

## What still blocks `HYR0600` from compiling, in detail

### 1. Eleven more `HYP*` PFs (the rest of the shipment family)

Beyond the six I drafted, `HYR0600` directly opens or SQL-references these — all `HYP*` work files for which we have no LF keys and no DSPF references:

| File | First-pass purpose (inferred from naming) | Tractability |
|---|---|---|
| `HYPSGTC` | Shipment tote-content header | Low — no LF keys |
| `HYPSGTS` | Shipment tote-stamp | Low |
| `HYPSCWK` | Shipment carton work-file | Medium — referenced in HYR0150 |
| `HYPSSCC` | SSCC master | Medium — `HYLSSCC8.LF` keys give a starting point |
| `HYPPCLS` | Pallet content / count summary | Low |
| `HYPRCLT` | "RC LOT" — recall/lot? | Very low |
| `HYPSTRK` | Shipment tracking | Low |
| `HYPSVCT` | Ship-via container | Low |
| `HYPTDTA` | Top-of-ship data / TID? | Low |
| `HYPIDTA` | "ID data" | Very low |
| `HYPMILG` | Mileage log | Low |
| `HYPINTR` | Shipment in-transit | Low — opened `IF A E K` so has standard PF shape |

Without seeing how each is *written* (and we mostly don't — most usage is reads in `HYR0600`), inventing fields beyond the SQL-referenced ones would be largely guesswork.

### 2. Order / customer / inventory tables

These are in the SQL JOINs and DELETEs of `HYR0600` (and many other Hornady programs). The full schemas are huge — for `OEORHD` alone the program references at least these columns: `OHORD#`, `OHCUST`, `OHSHTO`, `OHSHPV`, `OHSTS`, plus many more in the JOINs. Without the real source, every column would be a guess.

| File | Best evidence we have |
|---|---|
| `OEORHD` (Order Header) | SQL `OEORHD JOIN HDCUST` references — column-by-column extraction across HYR0xxx programs would yield ~25–35 columns |
| `OEORDP` (Order in-process detail) | Only the keys from `OEORDP01.INDEX` (`IDTURN, IDORD#, IDORL#, IDBLN#`) |
| `OEOCMT` (Order comments) | SQL `from HYPSGDT a join OEOCMT b on a.GDORD#=b.OCORD#` shows `OCORD#` |
| `OECMWK` / `OEDTWK` / `OEHDWK` | Aliased columns `O4OCTL`, `O1OCTL`, `H1OCTL` give the `OCTL` key |
| `HDCUST` | SQL `from HDCUST` — column refs in HYR0600 alone include name/address fields; full extraction across all HYR programs needed |
| `HDCCMT` | SQL `from HDCCMT` — comments file |
| `HDDSHP` | Drop-ship master |
| `HDIMST` | Item master — pervasive in many HYR programs |
| `HDIWHS` | Item-warehouse cross |
| `HDSHPV` | Ship-via master — only keys from `HDSHPV01.INDEX` (`SVSVDS, SVSVSV`) |
| `GUPTDAT` | "GU PT Data" — likely shared general-purpose table |
| `HREMPL` | Human-resources employee |
| `HYW0189` / `HYW0610A` | Work files defined by their respective driver programs |

Recommended approach for these: build them only as far as `HYR0600`'s F-specs and SQL columns require — same evidence-based approach as the shipment work-file drafts, but the field count goes up sharply because real master files have 40–80 columns each.

### 3. `/COPY` members

`HYR0600` has these three `/COPY` directives:

```
/COPY QPRPSRC,HYR0610
/COPY QPRPSRC,HYR0614
/COPY QPRPSRC,HYR9930
```

These pull D-spec (prototype) blocks from the named members. The package includes `HYR0610.SQLRPGLE`, `HYR0614.SQLRPGLE`, and `HYR9930.RPGLE` as full programs — but `/COPY` expects a source-PF member named exactly `HYR0610` etc. in a source file `QPRPSRC` on IBM i (with the prototype block alone, not the whole program).

Two ways to handle:

- **Best**: extract the `DCL-PR` / D-spec block from each program into a separate source member named `HYR0610` (etc.) inside an `AITSK00030/QPRPSRC` source PF. Codermake supports this via the standard `addpfm` + `Rfile -Qw` pattern used by the existing `qddssrc` rules.
- **Quick & dirty**: replace each `/COPY` with the prototype-only excerpt inlined in `HYR0600` itself.

### 4. Three missing service programs

`HYR0600` calls these via `CALLP(E)`:

| Procedure | What it presumably does | Stub strategy |
|---|---|---|
| `HHDCSS` | "HDS Customer Shipment Service" — likely the Agilx credit-card / freight-quote bridge based on the program comments | RPG service program exporting `HHDCSS` (and any sub-procs it uses) that returns success / zero values. |
| `HHDSVI` | "HDS Shipment Via" — ship-via validation | Same — stub returns success. |
| `HOEDIP` | "OE Distribution / Pick"? | Same. |

The signatures aren't visible from `HYR0600` calls because they use named-parm syntax we'd have to reconstruct. Easiest path is to extract the prototype block from any caller's `/COPY` member once it's available.

### 5. Sibling programs in the call chain

When `HYR0600` is launched it interactively chains into:

- `HYR0602` (Shipment Detail Processing) — same data-dep problem.
- `HYR0608` (Shipment Tote Processing) — same.
- `HYR0606TV` (Tote-verify variant) — same.
- `HYR0626` (Mixed/Partial label print) — same.
- `HYR0500`, `HYR0520`, `HYR0189` — service procs, smaller surface; `HYR0520` is the most likely to compile on the basis of just the data-layer described here, since it only needs `HDCCMT` and the message file.

## Recommended next phase to make this actually go

If the goal is *runnable* (not just compilable) `HYR0600`:

1. **Reconcile the DDL skeletons against a real Hornady IBM i.** Even an old dev box would have these files. `DSPFFD FILE(HYPSGCU)` etc. → field types and sizes. That eliminates the type-inference guess entirely.
2. **Land the `FLDREF`.** Build it first, then every PF compiles cleanly.
3. **Build the six shipment-family DDL members** as `.table.sql` in `ibmi-agentic/src/` and add to `Rules.mk`.
4. **Stub `HHDCSS`, `HHDSVI`, `HOEDIP`** as no-op service programs returning success codes.
5. **Extract `/COPY` members into `QPRPSRC`.**
6. **Promote the real `HYR0600.SQLRPGLE`** in place of the stub. Compile and iterate — every undefined column / file / procedure will be a fix list of one more thing to skeleton or stub.
7. **Seed data.** Load a handful of rows into `HYPSGCU` / `HYPSGDT` / `HYPSGLD` (and the customer / order files) so the subfile has something to display when you `GO HORNADY` and pick option 1.

## What I did *not* do this turn

- I did not promote any of the DDL drafts into `ibmi-agentic/src/`. The build is still green; you can codermake the menu/stubs without touching the new files.
- I did not attempt to draft DDL for the eleven additional `HYP*` files or any of the `OE*` / `HD*` masters. The evidence base is thin and I'd be inventing more than deriving — felt better to stop at the defensible Phase 1 line and let you decide.
- I did not extract the `/COPY` members or write stubs for `HHDCSS` / `HHDSVI` / `HOEDIP`.
