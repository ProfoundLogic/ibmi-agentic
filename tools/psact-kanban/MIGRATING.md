# Moving the PSACT Kanban to another CoderFlow environment

The board is a self-contained Node/Express app in this folder. Moving it means
carrying four things across. Only the first one travels with the code.

| What | Where it lives | How it moves |
|---|---|---|
| App code + board state | this folder (`data/board-state.json` holds your priority order and sync baseline) | git branch, or the export bundle |
| Jira credentials | environment secrets `JIRA_BASE_URL`, `JIRA_EMAIL`, `JIRA_API_TOKEN` | set by hand in the target environment's settings |
| Agent skills | CoderFlow shared skill library: **PSACT PreSales Tracker**, **jira** (optionally **Confluence Page Manager**) | assign them to the target environment |
| Agent memories | per-environment memory store | re-written in the target via `/memory-management` |

Jira itself is not moved. Both environments talk to the same live PSACT
project, so every ticket, status and comment is already "there".

## 1. Get the latest state onto the source side

Approve any open task in the source environment that touched this folder.
CoderFlow commits and pushes on approval, and until then the newest RUNBOOK
edits and board state exist only in that task's working tree.

The board also commits and pushes `data/board-state.json` by itself after
every successful **Review & Push** (`lib/gitSync.js`). Refreshing from Jira
does not commit anything.

## 2. Prepare the target environment (CoderFlow settings, not a task)

1. **Secrets:** add `JIRA_BASE_URL` (`https://plogic.atlassian.net`),
   `JIRA_EMAIL` and `JIRA_API_TOKEN` as environment secrets. Use your own API
   token from id.atlassian.com. Never paste a token into a task prompt or a
   file. The board resolves "me" from the token's account at runtime, so
   nothing else is user-specific.
2. **Skills:** assign **PSACT PreSales Tracker** and **jira** to the
   environment (plus **Confluence Page Manager** if you want the Content to
   Review page workflow, which also needs `CONFLUENCE_EMAIL` and
   `CONFLUENCE_API_TOKEN`).
3. **Image:** the container needs Node >= 20 and npm. Browser access uses
   CoderFlow's code-server proxy (`/usr/local/bin/start-code-server.sh`). On
   an edition without browser VS Code, the board still runs but is reachable
   only inside the container.

## 3. Bring the code in (pick one)

**A. The target environment includes the `ibmi-agentic` repo.** Start a task
on branch `psact-kanban`, or merge `origin/psact-kanban` into the
environment's branch. Nothing to copy.

**B. The target environment uses a different repo.** Copy this folder (minus
`node_modules/`) into that repo, e.g. as `tools/psact-kanban/`, from the
export bundle or with `git archive origin/psact-kanban tools/psact-kanban`.
Leave it uncommitted for the normal approve flow.

> In route B, `lib/gitSync.js` commits and pushes `data/board-state.json` to
> **whatever branch is checked out in that repo**, after every Review & Push.
> If that branch is shared or protected, decide before the first push whether
> you want that. The commit is pathspec-scoped, so it never sweeps up other
> work, but it does land on that branch.

## 4. First start in the target

```bash
bash tools/psact-kanban/start.sh
```

Then click **Open VS Code** in the task UI once (CoderFlow-side gate, see
RUNBOOK §2), load the printed URL, and press **Refresh from Jira**. A clean
move shows `0` on Review & Push and no conflict badges. Your column order
comes from the carried-over `board-state.json`.

If the refresh errors, the secrets from step 2 are missing or wrong. The
server still starts without them, but every Jira call fails.

## 5. Re-create the memories

Memories are per environment and do not follow the code. The export bundle
has a `memories/` folder of the relevant files. In a target task, ask the
agent to write each one with `/memory-management write <filename>`, then
update the index. Don't copy them into `~/.claude/` by hand, because local
writes do not persist.

## 6. Retire the source copy

Once the target board is working, stop using the source one. Two live
boards pushing `board-state.json` to two branches will drift: each one's
local order and baseline are its own. Jira stays consistent either way,
because the board has no write path that bypasses Review & Push.
