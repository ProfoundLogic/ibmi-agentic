#!/usr/bin/env bash
#
# One-step launch for the PSACT Kanban board.
#
# Brings up everything the browser URL depends on, in order, and prints the
# URL. Safe to re-run: every step is a no-op if it is already done.
#
#   bash tools/psact-kanban/start.sh
#
# Why code-server is in here, and what it does NOT do
# ---------------------------------------------------
# Container ports are not exposed to the internet. The only route in is
# CoderFlow's reverse proxy at /tasks/<TASK_ID>/vscode/, which forwards to
# code-server inside the container, whose own /proxy/<port>/ then forwards to
# an app on localhost.
#
# This script starts code-server via CoderFlow's own launcher,
# /usr/local/bin/start-code-server.sh, which is documented there as safe for a
# task to invoke directly. Same binary, same flags the platform uses, so
# nothing new is exposed.
#
# IMPORTANT: that is NOT sufficient to make the browser URL work. CoderFlow
# gates the proxy on its own server-side record of whether VS Code has been
# opened for this task, and only the "Open VS Code" action in the CoderFlow UI
# sets it. With code-server running and serving the board perfectly well on
# localhost:8080/proxy/<port>/, the external URL still answers:
#
#     Code-server not started for this task. Please click "Open VS Code" first.
#
# So the first browser visit in a NEW container still needs that one click.
# Running code-server here is harmless and means the click has less to do, but
# it does not replace it. Removing the click for good needs a CoderFlow-side
# change (auto-start for tasks, or an API to register it), not a change here.

set -uo pipefail

APP_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PORT="${PORT:-4287}"
LOG="${LOG:-/tmp/psact-kanban.log}"
CODE_SERVER_HELPER="/usr/local/bin/start-code-server.sh"

step() { printf '\n== %s\n' "$1"; }
warn() { printf '   !! %s\n' "$1" >&2; }

code_server_ok=0

step "code-server (necessary for the browser URL, but not sufficient - see header)"
if curl -sf -m 5 http://localhost:8080 >/dev/null 2>&1; then
  echo "   already running"
  code_server_ok=1
elif [ ! -x "$CODE_SERVER_HELPER" ]; then
  warn "$CODE_SERVER_HELPER not found - cannot start code-server."
  warn "Open the task's VS Code tab once instead, then re-run this script."
else
  out="$("$CODE_SERVER_HELPER" 2>&1)"; rc=$?
  if [ "$rc" -eq 0 ]; then
    echo "   started"
    code_server_ok=1
  elif [ "$rc" -eq 77 ]; then
    # The helper's own edition gate.
    warn "Browser VS Code is disabled on this CoderFlow edition, so the"
    warn "proxy URL below will not work. The board still runs locally on"
    warn "port $PORT inside the container."
  else
    warn "code-server failed to start (exit $rc):"
    printf '%s\n' "$out" | sed 's/^/      /' >&2
  fi
fi

step "dependencies"
if [ -d "$APP_DIR/node_modules" ]; then
  echo "   node_modules present"
else
  echo "   installing (node_modules never survives a re-pull)..."
  ( cd "$APP_DIR" && npm install --silent ) || { warn "npm install failed"; exit 1; }
fi

step "board server on port $PORT"
if curl -sf -m 5 "http://127.0.0.1:$PORT/api/board" >/dev/null 2>&1; then
  echo "   already running"
else
  # Stop by port, never by process name: pattern kills are unreliable here and
  # a broad process listing can print the container entrypoint, which carries
  # a private key.
  fuser -k "$PORT/tcp" >/dev/null 2>&1 && sleep 1
  # Three things here are deliberate, and each was needed to make this work:
  #
  #   * setsid - the server must outlive this script AND the shell that ran
  #     it. `& disown` only detaches from job control; the process stays in
  #     the caller's process group, so a group-directed kill (`timeout`,
  #     Ctrl-C, a harness tearing down a command) takes it with it.
  #
  #   * backgrounding a SIMPLE command, not `cd ... && node ... &`. Bash
  #     parses `A && B &` as `{ A && B } &`, forking a subshell that then
  #     *waits* on node. That subshell inherits this script's stdout, so
  #     `start.sh | tail` printed everything and then hung forever waiting
  #     for an EOF that only arrived when the server died. The `cd` is a
  #     separate statement so that only `node` is backgrounded.
  #
  #   * `>/dev/null` on the group - belt and braces, so nothing forked in
  #     here can hold this script's stdout open even transiently.
  (
    cd "$APP_DIR" || exit 1
    PORT="$PORT" setsid nohup node server.js >"$LOG" 2>&1 </dev/null &
  ) >/dev/null 2>&1
  for _ in $(seq 1 15); do
    curl -sf -m 3 "http://127.0.0.1:$PORT/api/board" >/dev/null 2>&1 && break
    sleep 1
  done
  if curl -sf -m 5 "http://127.0.0.1:$PORT/api/board" >/dev/null 2>&1; then
    echo "   started"
  else
    warn "server did not come up - last lines of $LOG:"
    tail -n 20 "$LOG" 2>/dev/null | sed 's/^/      /' >&2
    exit 1
  fi
fi

step "ready"
if [ -n "${CODERFLOW_SERVER_URL:-}" ] && [ -n "${TASK_ID:-}" ]; then
  url="${CODERFLOW_SERVER_URL}/tasks/${TASK_ID}/vscode/proxy/${PORT}/"
  if [ "$code_server_ok" -eq 1 ]; then
    echo "   $url"
    echo
    echo "   If that shows \"Code-server not started for this task\", click"
    echo "   \"Open VS Code\" in the CoderFlow task UI once, then reload. That"
    echo "   gate is CoderFlow-side state this script cannot set."
  else
    echo "   (unreachable until code-server is up) $url"
  fi
else
  warn "CODERFLOW_SERVER_URL / TASK_ID not set - cannot build the browser URL."
  echo "   Local only: http://127.0.0.1:$PORT/"
fi
echo
