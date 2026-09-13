# PSACT Kanban — deployment runbook

What it takes to get this app running from scratch in a brand-new container
(after this branch has been re-pulled), start to finish. Written so someone
(human or agent) with zero memory of building this can follow it top to
bottom.

## 0. What you're starting with

A fresh container that has this branch checked out gives you:

- `tools/psact-kanban/{server.js,lib/,public/,package.json,data/board-state.json}`
  — all committed, all present. **`node_modules/` is not committed** (it's
  `.gitignore`'d) and `data/board-state.json` is the *last committed* board
  state, not live — it only updates as you use the running app.
- Nothing running. No node process, no port bound, no URL.

Everything below is about turning that into a reachable, working board.

## 1. Prerequisites — confirm before starting anything

These should already be true of the environment; this step is "verify," not
"set up."

```bash
node -v        # need >= 20 (developed/tested against v24)
npm -v
[ -n "$JIRA_BASE_URL" ] && echo "JIRA_BASE_URL set" || echo "MISSING"
[ -n "$JIRA_EMAIL" ] && echo "JIRA_EMAIL set" || echo "MISSING"
[ -n "$JIRA_API_TOKEN" ] && echo "JIRA_API_TOKEN set" || echo "MISSING"
[ -n "$TASK_ID" ] && echo "TASK_ID set" || echo "MISSING"
[ -n "$CODERFLOW_SERVER_URL" ] && echo "CODERFLOW_SERVER_URL set" || echo "MISSING"
```

**Never print the actual values of `JIRA_API_TOKEN`** (or any credential) —
these presence-only checks are enough. If `JIRA_BASE_URL`/`JIRA_EMAIL`/
`JIRA_API_TOKEN` are missing, the server will still start and bind its port,
but every Jira call will fail with a JSON `{"error": "..."}` body — this is
provisioned per-environment already, so a miss here means something is wrong
with the environment itself, not this app; stop and report it rather than
inventing credentials.

## 2. Find the repo and install dependencies

```bash
find /workspace -maxdepth 4 -name .git   # confirms the repo root; layout has varied across containers
cd <that repo root>/tools/psact-kanban
npm install
```

`npm install` is required every time — `node_modules/` never survives a
re-pull.

## 3. Start the server, detached, and verify it's actually up

The server must outlive the current shell/turn, so start it detached and
confirm it's listening before moving on — don't just trust the log line.

```bash
nohup node server.js > /tmp/psact-kanban.log 2>&1 < /dev/null &
disown
sleep 2
curl -s -o /dev/null -w "http_status=%{http_code}\n" http://127.0.0.1:4287/api/board
```

Expect `http_status=200`. If you get `000`, the process died — check
`/tmp/psact-kanban.log` for a stack trace (missing Jira env vars is the most
common cause, per step 1).

Default port is `4287`. Override with `PORT=<n> nohup node server.js ...` if
that port is already in use in this container (see Troubleshooting).

**Do not restart the server by pattern-killing "server.js" or "node" and
re-launching in the same combined command.** In this environment that
combination reliably fails to actually stop the old process (it survives in
the background, still holding its in-memory state, and just re-saves over
whatever you deleted) while reporting success. If you need to restart it,
stop it by **port**, not by process name/pattern:

```bash
fuser -k 4287/tcp   # stops whatever is actually listening on the port
sleep 1
# then start fresh per the block above
```

## 4. Get a browser-reachable URL

This container's ports aren't exposed to the internet directly. Reach it
through the CoderFlow code-server proxy chain:

```bash
echo "${CODERFLOW_SERVER_URL}/tasks/${TASK_ID}/vscode/proxy/4287/"
```

Two things that will bite you if skipped:

- **Open the task's VS Code tab in the browser at least once first.**
  code-server (and therefore this proxy path) only starts on demand, not
  automatically at container boot. If the URL 404s or hangs, this is almost
  always why.
- **This URL is only valid for this container's lifetime.** A fresh re-pull
  into another container gets its own `TASK_ID` and needs the whole sequence
  above run again — there is no way to pre-generate a stable, permanent link.

## 5. First load / what happens automatically

- If `data/board-state.json`'s `lastSyncedAt` is `null` (a genuinely fresh
  board, or one that was reset per step 6), the frontend automatically fires
  a "refresh from Jira" the moment the page loads — no manual click needed.
- If `data/board-state.json` already carries synced data (committed from a
  prior session), the board renders that immediately; use the **Refresh from
  Jira** button to pull anything that changed since.
- Either way, nothing is pushed to Jira until you explicitly use **Review &
  Push**. A freshly-started server with no interaction is 100% read-only
  toward Jira.

## 6. Resetting board state (rare — only if asked, or the file looks corrupt)

```bash
fuser -k 4287/tcp   # stop it first
rm tools/psact-kanban/data/board-state.json
# then restart per step 3 — next load repopulates fresh from Jira, zero local edits
```

Only do this if the user asks for a clean slate or the JSON file is
genuinely broken (fails to parse) — it discards every local column/order
edit that hasn't been pushed to Jira.

## 7. Verifying without a browser

Useful when you can't (or don't want to) drive an actual browser:

```bash
curl -s http://127.0.0.1:4287/api/board | python3 -m json.tool          # current board + pending diff
curl -s -X POST http://127.0.0.1:4287/api/refresh | python3 -m json.tool # force a live Jira pull
```

A non-empty `diff` array in the response means there are local changes not
yet pushed to Jira — normal mid-session, but worth noting if you're handing
the board back to the user.

## 8. What persists across containers vs. what doesn't

| Persists (committed to git)              | Does not persist (redo every container) |
|-------------------------------------------|------------------------------------------|
| All code (`server.js`, `lib/`, `public/`) | `node_modules/` — run `npm install`      |
| `data/board-state.json` (columns, local order, sync baseline, change log) | The running server process — run `npm start`/`node server.js` |
|                                            | The access URL — new `TASK_ID` each container |

The whole point of committing `data/board-state.json` is that your priority
order and any not-yet-pushed pending changes survive a container swap. If
that file is ever `.gitignore`'d instead (see the note in `README.md`), this
table changes — the board reverts to a fresh Jira pull on every new
container.

## 9. Troubleshooting quick reference

| Symptom | Likely cause | Fix |
|---|---|---|
| `curl` gives `http_status=000` | server not running / crashed on start | check `/tmp/psact-kanban.log`; usually missing `JIRA_*` env vars |
| `EADDRINUSE` on start | something already bound to the port | `fuser 4287/tcp` to find it, `fuser -k 4287/tcp` to free it, or `PORT=<other>` |
| Proxy URL 404s or hangs | code-server not started for this task yet | open the task's VS Code tab once, retry |
| `/api/refresh` returns `{"error": "..."}` | Jira credentials missing/invalid, or PSACT unreachable | check the presence-only env checks in step 1; don't invent credentials |
| Restarted the server but old data/behavior still shows | old process wasn't actually killed (see step 3's warning) | stop by port (`fuser -k <port>/tcp`), not by name/pattern match |

**Security note:** avoid `pgrep -af`, `ps aux` with full-command output, or
similar broad process-line dumps in this environment when looking for this
(or any) server process — in this environment they can print the
container's own entrypoint command line, which embeds a plaintext SSH
private key. Use `fuser <port>/tcp` for port-based lookups instead; it only
ever prints a PID.
