# PSACT Kanban

A personal kanban board over Jira project **PSACT**: your assigned tickets plus
the unassigned pool, with local drag-and-drop prioritization that is never
pushed to Jira until you explicitly say so.

## What it does

- Pulls PSACT tickets that are either assigned to you or unassigned (never
  Cancelled). Done tickets drop off the board automatically 7 days after
  their last update.
- Board columns: **Unassigned | To Do | In Progress | Waiting | Done**.
- Drag a card between columns, or use the ▲▼ buttons / "Move to…" dropdown.
  Reordering within a column is a **local priority order only** - Jira has no
  field for it, so it never gets pushed.
- Moving a card out of **Unassigned** into any of your columns marks it as a
  pending "assign to me (+ transition)" change. Moving one back into
  **Unassigned** marks it as a pending "unassign" change. Moving between your
  own columns marks a pending status transition.
- Nothing touches Jira until you click **Review & Push**, which shows every
  pending change (with a diff and per-action pills) so you can select exactly
  what to push, then reports success/failure per ticket.
- **Refresh from Jira** re-pulls current data. Tickets with no pending local
  change silently follow whatever Jira says. Tickets with a pending change
  are left alone, but flagged with a conflict warning if Jira's side also
  moved since your last sync, so you can decide whether to push your version
  or discard it.
- Clicking a card (outside its buttons) opens the ticket in Jira in a new
  tab.

## Persistence

All board state (columns, per-ticket local order, the baseline used to
compute the diff, and a short change log) lives in `data/board-state.json`,
committed to the repo. Cloning this branch into a fresh container and running
`npm start` picks up exactly where you left off - nothing is stored outside
this directory.

## Running it

See `RUNBOOK.md` for the full step-by-step (prerequisites, startup, getting
a browser URL, troubleshooting) — this is the short version.

Requires `JIRA_BASE_URL`, `JIRA_EMAIL` and `JIRA_API_TOKEN` in the environment
(already provisioned in this environment for the `jira` skill/tool).

```bash
cd tools/psact-kanban
npm install
npm start
```

The server listens on `127.0.0.1:${PORT:-4287}` only (not on all interfaces).
In a CoderFlow task container, reach it through the code-server proxy:

```
https://coderdemo.profoundlogic.com:3000/tasks/<TASK_ID>/vscode/proxy/4287/
```

Open the task's VS Code tab at least once first - code-server (and therefore
this proxy path) only starts on demand, not automatically at container boot.
This URL only works for the lifetime of the container it was started in; a
freshly re-pulled container needs its own `npm start` and will get a new
`<TASK_ID>` in the link.

## Layout

```
lib/
  config.js      column/status constants, Done retention window
  jiraClient.js   thin Jira REST v3 client (search, transitions, assign)
  boardStore.js   loads/saves data/board-state.json, column placement helpers
  reconcile.js    merges fresh Jira results into local state, conflict detection
  diff.js         computes pending-change list + the Jira actions each implies
  push.js         executes the pending actions against Jira
server.js         Express app + API routes
public/           vanilla HTML/CSS/JS frontend (no build step)
```
