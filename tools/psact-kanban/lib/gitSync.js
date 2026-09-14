'use strict';

const { execFile } = require('child_process');
const path = require('path');

const REPO_DIR = path.join(__dirname, '..');
const BOARD_STATE_REL = path.join('data', 'board-state.json');

function run(cmd, args) {
  return new Promise((resolve, reject) => {
    execFile(cmd, args, { cwd: REPO_DIR }, (err, stdout, stderr) => {
      if (err) {
        err.stdout = stdout;
        err.stderr = stderr;
        return reject(err);
      }
      resolve({ stdout, stderr });
    });
  });
}

// Commits and pushes ONLY data/board-state.json to whatever branch is
// currently checked out, so the board's priority order and sync baseline
// survive a container swap without waiting on a full task approval. Scoped
// strictly to this one file via `git add <path>` (never `-A`/`.`) so it never
// sweeps up other in-progress, uncommitted repo changes - those stay
// uncommitted for the normal CoderFlow review/approve flow, same as always.
async function commitAndPushBoardState(summary) {
  try {
    const status = await run('git', ['status', '--porcelain', '--', BOARD_STATE_REL]);
    if (!status.stdout.trim()) return { attempted: false };

    await run('git', ['add', '--', BOARD_STATE_REL]);
    await run('git', ['commit', '-m', `psact-kanban: ${summary}`]);
    await run('git', ['push']);
    return { attempted: true, success: true };
  } catch (err) {
    return {
      attempted: true,
      success: false,
      error: (err.stderr && err.stderr.trim()) || err.message,
    };
  }
}

module.exports = { commitAndPushBoardState };
