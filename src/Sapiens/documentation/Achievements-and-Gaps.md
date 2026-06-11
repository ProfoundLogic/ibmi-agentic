# Sapiens Agency Demo — Achievements & Missing Artefacts

_A status snapshot of what we received from Sapiens, what we built around the gaps, and what would still be valuable to get from them._

**Task library:** `AITSK00030` &nbsp;•&nbsp; **Source root:** `/workspace/ibmi-agentic/src/Sapiens/` &nbsp;•&nbsp; **Companion docs:** `Sapiens-Analysis.md`, `Rebuild.md`

---

## 1. What we achieved

### 1.1 A fully-running Agency Configuration screen on the demo menu

| Option | Title | Driver | Display | Status |
|--------|-------|--------|---------|--------|
| 4 | Agency Configuration | `WTAGTCFG` (Sapiens SQLRPGLE) | `WTAGTCFG` 5250 DDS + PUI Rich Display | Runs end-to-end. Grid loads 35 sample agencies. Search filters. Detail panel populates. |
| 5 | Agency Configuration (EJS) | `WTAGTCFE` (new, free-form RPG) | EJS Rich Display File (`agency.html` + CSS + JS) | Runs end-to-end with Sapiens-branded UX (navy/orange/cream, DM Sans/Inter). Template fetch works via inline-shim workaround (see §3.3). |

Both options are reachable from `MENU` (`USR0004` and `USR0005` message IDs). Cancel/Exit work. Sample data is rich enough to demonstrate filtering and selection.

### 1.2 Sapiens artefacts brought up cleanly

Every Sapiens-supplied artefact that referenced **only** existing primitives now builds:

- **Driver programs:** `WTAGTCFG.sqlrpgle` (the option-4 program) compiles and binds.
- **Service-program /COPY modules:** All `SPR*` and `SCOPY*` copybooks load from the new `AITSK00030/QRPGLESRC` source physical file.
- **DDS:** `STACTPNL.dspf`, `WTAGTCFG.dspf`, plus the read-only DDS shipped for the other Agency screens (`WTAGTASN`, `WTAGTCON`, `WTAGTPRI`, `WTAGTPROD`, `WTAGTRTFND`, `WTAGTSRVRT`, `WTVSTTRK`).

### 1.3 Documentation, sample data, and a clean rebuild path

- `Sapiens-Analysis.md` — exhaustive technical reading of the source drop, including mermaid diagrams of program flow, dependency maps, and a list of unresolved external symbols.
- `Rebuild.md` — step-by-step guide to recreate the entire stub stack from scratch.
- `sqlScripts/` — Python generator + SQL `INSERT` packages that seed at least 30 representative rows into each reverse-engineered table; `runall.sh` / `runall.sql` to load everything in one go.
- `documentation/Sapiens-Analysis.docx` — Word version of the technical analysis for non-IBM-i stakeholders.

### 1.4 Process learnings captured as memory

Persistent memory records now cover four PUI/EJS gotchas surfaced during this work (Apache cache headers, inline `<script>` non-execution, proxy/XHR limitation, deployment paths). Future tasks pick these up without re-discovering them.

---

## 2. What we received from Sapiens

**Two artefacts, total:**

| File | Type | Contents |
|------|------|----------|
| `agencysrc.zip` | Source bundle | 7 driver RPG/SQLRPG programs, 9 DDS members, 21 `/COPY` service modules. **No PFs, LFs, binding directories, or runtime program objects.** |
| `Agency Info updated.docx` | Word doc | High-level prose describing the Agency module purpose and screen flow. No table layouts, no field-level specs, no record-format documentation. |

---

## 3. Missing artefacts and how we worked around them

### 3.1 Physical files (DDS/PF source)

| Missing | Why it mattered | How we mitigated | Risk |
|---------|-----------------|------------------|------|
| `WMAGP` (Agency master), `SMCOP`, `WMAAP`, `WDF2P`, `WMCZP`, `WDELP` | Every driver opens these. Without them, nothing compiles. | Reverse-engineered 19 PFs (`wmagp.pf`, `smcop.pf`, `wmaap.pf`, `wdf2p.pf`, `wmczp.pf`, `wdelp.pf`, plus 13 trigger-related: `szq1p`, `wmahp`, `wmcmp`, `wmcdp`, `wdpap`, `wamcp`, `wdehp`, `wmshp`, `wxelp`, `wmemp`, `sdcmp`, `wmeap`, `wibifp`) by inferring fields, lengths, and decimals from RPG `extName` references, F-spec reads, and `/COPY` data structure templates. | Field layouts are best-guess. If Sapiens' production schemas differ in length, decimal places, or null-capability, the stub PFs will need to be regenerated. |

> **Ask Sapiens for:** the DDS source (`*.pf`/`*.lf`) for every file referenced by `extName(...)` or `F` specs in the shipped drivers. Even just `DSPFFD` output for each table would let us verify field-level fidelity.

### 3.2 Logical files

| Missing | Why it mattered | How we mitigated | Risk |
|---------|-----------------|------------------|------|
| `WMAGL`, `WMAGL1`, `SMCOL`, `WMAAL`, `WDF2L`, `WMCZL` | The drivers read via keyed LFs, not the PFs directly. Without LFs the `KEYED` open fails and `CHAIN`/`READ` semantics are different. | Built six LFs over the reverse-engineered PFs, picking keys based on how each driver uses them (`SETLL`/`CHAIN` argument patterns). | If the production LFs include `SELECT/OMIT`, alternate key sequences, or join LFs we don't know about, runtime filtering will diverge. |

> **Ask Sapiens for:** the LF DDS source. LFs are the only way to know the real index ordering and any built-in record selection.

### 3.3 EJS Rich Display File integration through the demo proxy

| Missing | Why it mattered | How we mitigated | Risk |
|---------|-----------------|------------------|------|
| A working pattern for EJS templates on this demo environment | The CoderFlow proxy at `coderdemo:3000` does not forward `XMLHttpRequest` fetches to `/profoundui/userdata/*`, but PUI's runtime fetches EJS templates via XHR. Result: every template URL returned `failed to fetch template`. | Inlined the agency template content + CSS into a `<script>` block inside each Genie skin's `start.html` and patched `XMLHttpRequest` to synthesize a 200 response for the template URL. | Demo-grade. The "real" fix is server-side (teach the proxy to forward XHR or mount `userdata/` differently). The shim lives outside the source repo and would need to be re-applied on a fresh IBM i environment. |

> **Action item (PL side, not Sapiens):** patch the proxy so PUI's XHR template fetches reach Apache like `<link>`/`<script>` fetches do.

### 3.4 Service-program runtime objects

| Missing | Why it mattered | How we mitigated | Risk |
|---------|-----------------|------------------|------|
| Compiled `*MODULE`/`*SRVPGM`/`*BNDDIR` for the `SPR*` service stack | `WTAGTCFG` references procedures like `runErrorWithBranding`, `getAtriumUser`, `qcmdexc_run`, `runSql`, etc. Without bound implementations the binder errors out. | Built `sapstubs.rpgle` with one no-op or value-returning procedure for every exported prototype, packaged into `SAPSTUBS *SRVPGM` and registered in `STBNDDIR` / `WTGRPCFG` binding directories. | All business logic inside those procedures is absent. Anything that **depends** on real branding, real Atrium auth, real SQL via the helper, etc., will silently return `*BLANKS` / `0`. The current screens don't depend on the real behaviour, but extending the demo will hit these stubs quickly. |

> **Ask Sapiens for:** the source for the `SPR*` service programs (or at least exported names and intended return semantics). Even pseudo-code per procedure is enough to flesh out the stubs.

### 3.5 EXTPGM runtime callees

| Missing | Why it mattered | How we mitigated | Risk |
|---------|-----------------|------------------|------|
| Compiled programs `W4020R`, `SRCHKPGM`, `RTVCLTID`, `STRTVDTA`, `STWRTDTA` | `WTAGTCFG` and other drivers `CALL` these by name. Missing-program-object errors at run time. | Wrote one-line RPGLE stub modules for each, returning blanks/zeros via the same parameter signature inferred from the caller. | Behavior of these programs is unknown. They appear to be cross-module lookups (client ID retrieval, search dispatch, generic read/write). For a real production port we need them. |

> **Ask Sapiens for:** the source (or at least call-spec documentation) for `W4020R`, `SRCHKPGM`, `RTVCLTID`, `STRTVDTA`, `STWRTDTA`.

### 3.6 Sample data

| Missing | Why it mattered | How we mitigated | Risk |
|---------|-----------------|------------------|------|
| Production-shaped seed rows | Empty tables can't demo filtering, status badges, or counts. | Wrote `sqlScripts/gen_sample_data.py` and per-table `*.sql` packages; loaded ≥30 rows into every reverse-engineered PF with realistic agency names, FEINs, contacts, phone numbers, cities/states, and status mixes (A/I/H/P). | Synthetic, not anonymized real data. Distribution may not match what Sapiens' QA scenarios assume. |

> **Ask Sapiens for:** a dev-environment data extract (even 50 obfuscated rows per table) so the demo shows real data shapes.

### 3.7 The `WTAGTPRI`, `WTAGTPROD`, `WTAGTSRVRT`, `WTAGTRTFND`, `WTAGTCON`, `WTAGTASN`, `WTVSTTRK` drivers

| Missing | Why it mattered | How we mitigated | Risk |
|---------|-----------------|------------------|------|
| Compiled objects for the other Agency-module screens. Their source IS shipped but they depend on the same missing PFs/LFs/services as `WTAGTCFG`. | Without these, the only working menu items are `4` and `5`. The Word doc references the full screen flow but we can't demo it. | **Not yet mitigated.** Listed as the next phase if/when the user wants more than the configuration screen. The Sapiens-shipped RPG source is in the repo and ready, the dependency surface is mapped in `Sapiens-Analysis.md` §3, and the same stubbing pattern would apply. | Effort. Each additional driver pulls in 1-3 more PFs and a handful of `/COPY` procedures to stub. |

> **Ask Sapiens for:** clarification on whether these drivers share the same data files as `WTAGTCFG` (likely yes for `WMAGP`) or open additional tables not yet on our list.

### 3.8 Other smaller gaps (not blocking the current demo)

| Missing | Mitigation |
|---------|------------|
| Trigger-buffer DS layouts referenced by `SPRtrigger`/`SPRtrig` (`SZQ1P`, `WMAHP`, `WMCMP`, `WMCDP`, `WDPAP`, `WAMCP`, `WDEHP`, `WMSHP`, `WXELP`, `WMEMP`, `SDCMP`, `WMEAP`, `WIBIFP`) | Placeholder PFs created so the `extName(...)` data structures resolve. No trigger logic ever runs in the demo. |
| `WDEAP_HLD` table | Created as a SQL-defined table (`wdeap_hld.table.sql`) since the original was likely SQL-created in production. |
| Sapiens branding (logos / official color codes) | Pulled colors and font choices from Sapiens.com (navy `#0D256F`, orange `#FF5900`, cream `#F6ECDB`, DM Sans + Inter). No official brand asset bundle. |

> **Ask Sapiens for:** an official brand kit if the demo will be shown to their team.

---

## 4. Suggested ask to Sapiens

A consolidated request — single email, one prioritised list:

1. **DDS for every PF and LF referenced by the shipped drivers.** This is by far the highest-value ask; everything else is downstream of correct table layouts.
2. **Source (or build artefacts) for the `SPR*` service program stack.** Even `SCAN`-able RPG with bodies removed would tell us return-value semantics.
3. **Source for the EXTPGM callees** (`W4020R`, `SRCHKPGM`, `RTVCLTID`, `STRTVDTA`, `STWRTDTA`).
4. **A small data extract** (50 rows per table is fine, obfuscated PII).
5. **Official brand assets** (logo SVG/PNG and the canonical hex codes Sapiens uses internally).
6. **Confirmation** that the other Agency screens (`WTAGTASN`, `WTAGTCON`, `WTAGTPRI`, `WTAGTPROD`, `WTAGTRTFND`, `WTAGTSRVRT`, `WTVSTTRK`) share the same data file set or, if not, the missing tables they open.

A draft of this email already lives at `/task-output/email-to-sapiens.md` from the earlier turn — worth refreshing with the items above before sending.

---

## 5. Risk summary

The current demo runs cleanly **as a demo**. It is not a substitute for an integration build:

- Every `SPR*` procedure call returns a placeholder. Real business logic is absent.
- Field layouts are inferred. A column-width mismatch could cause subtle data-truncation bugs once real data lands.
- Triggers, error-popup behaviour, branding logic, and Atrium auth are all stubbed to no-ops.
- The EJS proxy workaround (option 5) is a client-side shim, not a server-side fix.

For a production port of the Agency module, treat this repo as a scaffold and replace the reverse-engineered pieces in lock-step with whatever Sapiens delivers.
