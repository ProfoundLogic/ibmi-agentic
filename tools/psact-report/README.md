# PSACT daily activity report

Generates the presales team's morning report from Jira project **PSACT**
(plus any rollup projects it points at) and renders three artefacts.

## Usage

```bash
python3 psact_report.py                      # today, mode chosen from weekday
python3 psact_report.py --date 2026-08-10    # back-dated run
python3 psact_report.py --mode weekly        # force weekly recap
python3 psact_report.py --outdir /task-output
python3 psact_report.py --force              # run at a weekend
```

Exits without producing anything on Saturday and Sunday unless `--force` is
given.

## Modes

| Weekday | Mode | Window |
| --- | --- | --- |
| Monday | `weekly` | previous 7 days — recap of the week just ended |
| Tue–Fri | `daily` | the prior day only |
| Sat/Sun | skipped | — |

The window is **half-open**: `[start, today)`. Work done this morning is not
reported under yesterday's heading. Day boundaries are computed in
`America/New_York`, not UTC — in UTC a 9pm EDT comment falls on the following
calendar day and would be filed under the wrong date.

## Outputs

| File | Purpose |
| --- | --- |
| `psact-report-<date>.html` | Rich version — animated, expandable sections. Open in a browser. |
| `psact-report-<date>.email.html` | Email-safe — no JS, table layout, inline styles. Survives Gmail/Outlook. |
| `psact-report-<date>.md` | Markdown digest for the archive. |

Two HTML files exist because **email clients strip `<script>` and most modern
CSS**. Gmail drops `<details>`/`<summary>` entirely, so an expandable report
cannot be delivered inline. The email carries the flat summary and attaches the
rich file; opening the attachment gives the full interactive version.

## Links

Every Jira link carries `target="_blank" rel="noopener noreferrer"` — set once in
the `LINK_ATTRS` constant and used by all four anchor-generating sites (the rich
row links, the rollup key list, and the email's row and "Next up" links).

The report is something people keep open and scan through, so following a ticket
in place would discard their position. In the emailed attachment it would navigate
away from the report entirely. If you add a new link, use `LINK_ATTRS`.

## Theming

The rich report has a **light/dark toggle** in the top-right of the header.

- The default follows the reader's OS preference (`prefers-color-scheme`).
- Clicking the toggle overrides that and persists the choice in `localStorage`
  under `psact-theme`, so it survives across days and across reports.
- The saved choice is applied by a small inline script in `<head>`, before first
  paint, so a dark reader does not get a flash of light.
- With JavaScript unavailable the page still themes correctly from the OS
  preference — only the toggle stops responding.

Dark is a **selected palette, not an inverted one**. Every colour lives in
`VARS_LIGHT` / `VARS_DARK` as a CSS custom property; a colour written inline is a
colour frozen in light mode, which is why the status pills are class-based rather
than inline-styled.

Contrasts were computed rather than eyeballed. All pill foreground/background
pairs clear 4.5:1 in both modes except the dark `critical` pill at **3.84:1** —
reaching 4.5 would mean altering the fixed status hex, so it relies on the
sanctioned mitigation instead: it always ships with an icon and the word
"Critical", so colour never carries the meaning alone.

**The email body stays light.** Inline styles are required for email client
compatibility and cannot be overridden by a `prefers-color-scheme` block, so a
dark email would mean restructuring to classes that many clients strip
inconsistently. The attachment is where dark mode lives.

## Report sections

This is **one document for the whole team**, not three personal reports. Each
person sees what the others are carrying and where someone is slipping, so the
team can offer help rather than each person only reading their own list.

1. **Today** — anything due today (demos, deadlines). If today is clear, a single
   "Next up" line points at the next scheduled item. The week ahead is **not**
   listed here — it sits on each person's plate, where it also says whose it is,
   so a team-wide block would list the same tickets twice in one document.
2. **A plate per person** — one block each for Brian, Gary and Roger, in
   `TEAM` order (Brian leads the team, so he comes first; reorder `TEAM` to
   change it — the header name list derives from the same constant). Each opens
   with **what is falling behind for that person**,
   under a banner naming them and inviting the others to step in, then their
   work scheduled this week, in progress, waiting, queued, and completed. The
   card heading carries a one-line count (`2 in progress · 2 behind`) so the
   state of someone's plate is readable without expanding it.
3. **Activity** — what moved in the window. Deliberately **terse**: a rollup
   renders as one summary line plus a row of linked ticket keys
   (*"12 tickets moved in WAT — 7 done, 5 in progress"*). The child project's
   comment bodies are its own narrative; reproducing them here buried the report
   in detail. Comments written directly on PSACT tickets — the team's own
   end-of-day notes — are still shown, since there are few of them and they are
   the point of the daily log.
4. **Falling behind — elsewhere** — stale tickets *not* on a team member's
   plate: other people's, and unowned work. Capped at 5 with a count of the
   remainder. Each person's own stale items already head their plate, so
   repeating them here would just be noise. Section is hidden when empty.
5. **Completed elsewhere** — closed by someone outside the team.
6. **No owner** — open tickets with no assignee (only shown when non-empty).

Members of `TEAM` always get a plate, even when empty — a person with nothing on
their plate is information, not a section to hide.

**Plate buckets are disjoint.** A ticket lands in exactly one of *falling
behind → scheduled this week → in progress → waiting → queued*, in that
precedence order. Without this, a demo that is both due this week and still To Do
appears twice on the same plate. The heading chips (`2 in progress · 2 behind`)
are counted from **true status** instead, since health and status are different
axes and overlap there is correct.

## Staleness

Keyed on `updated`, **not** on comment presence. `updated` moves on transitions
and edits, so it picks up real activity for free; comment-absence would flag
tickets created that morning.

| Status | Threshold |
| --- | --- |
| In Progress | 7 days |
| Waiting | 14 days |
| To Do | 21 days |

Exempt: a **future due date** (the ticket is scheduled, not stalled) or the
**`parked`** label (deliberately deferred). Severity escalates at 1×, 2× and 3×
the threshold, rendered with the reserved status palette — always paired with a
text label, never colour alone.

## Rollup tickets

A PSACT ticket labelled `rollup:<PROJECT>` is a placeholder: it holds no work
of its own and the real decomposition lives in another Jira project.

| Ticket | Labels | Resolves to |
| --- | --- | --- |
| PSACT-10 | `rollup:perp` | all of PERP — the demo ERP build |
| PSACT-12 | `rollup:wat` + `rollup-mine` | Gary's slice of WAT — Watsons discovery |

Adding **`rollup-mine`** alongside the `rollup:` label narrows the rollup to the
umbrella ticket's own assignee. Use it whenever the target project is shared
with people outside this team: WAT is worked by Mike Lamere and Ajay Gomez as
well as Gary, so an unscoped rollup would report their work as ours. PERP is
wholly unassigned, so it stays project-wide.

For these tickets the generator:

- resolves **freshness** as the newest update across the target project, so the
  placeholder is never falsely stale — but still flags when that project itself
  goes quiet;
- treats the ticket as **active** when the child project moved, even if the
  placeholder was never touched;
- pulls the child project's **recent tickets and their comments** into the
  activity section, since that is where the narrative actually lives;
- counts **tickets only**, excluding the child project's epics (they are
  containers, and counting them double-counts the same work).

## Configuration

Edit the constants at the top of `psact_report.py`:

- `PROJECT` — the Jira project key.
- `TEAM` — display names, and the order people appear in.
- `STALE_DAYS` — the staleness ladder.
- `TOK` — design tokens (validated status palette and surfaces).

## Authentication

Reuses the environment's existing Jira variables: `JIRA_BASE_URL`,
`JIRA_EMAIL`, `JIRA_API_TOKEN`. Credentials are read from the environment and
are never logged, echoed, or passed on a command line.

## Notes

The bundled `jira.sh` helper is deliberately **not** used. Two reasons:

- Its fixed field list omits `parent`, `labels` and `duedate`, all of which this
  report depends on.
- **It does not paginate.** Its `maxResults` argument is passed straight through,
  but the `/search/jql` endpoint caps a single response at 100 regardless — so a
  project with more issues than that silently reports exactly 100. This produced
  wrong figures for WAT during development (100 instead of the true 133) before
  it was spotted. Any round-number total from that helper should be treated as
  suspect.

The script calls `/rest/api/3/search/jql` directly and pages via
`nextPageToken` until the result set is exhausted.

The entrance animation is purely additive: every element is fully visible
without it, and the animation only adds movement. An earlier version started at
`opacity:0` and relied on the animation to reveal content, which rendered a
near-blank page anywhere the animation did not execute — print, PDF export,
screenshots, reduced-motion.
