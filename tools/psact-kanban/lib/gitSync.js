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
// survive a container swap without waiting on a full task approval. Other
// in-progress repo changes must stay uncommitted for the normal CoderFlow
// review/approve flow.
//
// Scoping `git add` to the one path is NOT enough on its own, and assuming it
// was is how this function once swallowed a whole task's worth of
// work-in-progress into a board-sync commit: a bare `git commit` commits
// everything already in the index, not just what this function staged. If
// anything else had been staged beforehand - by a tool, an editor, or a
// half-finished `git add` - it went along for the ride, and once pushed it
// was no longer visible to CoderFlow as pending work. Both the add AND the
// commit are therefore pathspec-scoped; the commit pathspec is the one that
// actually guarantees it.
// How many commits are sitting locally that the remote has not got. Used to
// tell the user how much is waiting when a push fails.
async function unpushedCount() {
  try {
    const r = await run('git', ['rev-list', '--count', '@{u}..HEAD']);
    return Number(r.stdout.trim()) || 0;
  } catch {
    return 0; // no upstream configured, or the ref is unreadable - not worth failing over
  }
}

// CoderFlow's credential helper mints a short-lived Git token per container.
// On a long-lived container it expires, and from then on commits still work
// but pushes do not. The helper says so in its own words, so match on that.
function isExpiredCredential(text) {
  return /container_token_expired|credential has expired|could not read Username/i.test(text || '');
}

async function commitAndPushBoardState(summary) {
  // Which step we are on, so a failure can say whether anything was saved.
  let step = 'check';
  try {
    const status = await run('git', ['status', '--porcelain', '--', BOARD_STATE_REL]);
    if (!status.stdout.trim()) return { attempted: false };

    step = 'commit';
    await run('git', ['add', '--', BOARD_STATE_REL]);
    // The trailing pathspec is load-bearing - see the note above.
    await run('git', ['commit', '-m', `psact-kanban: ${summary}`, '--', BOARD_STATE_REL]);

    step = 'push';
    await run('git', ['push']);
    return { attempted: true, success: true };
  } catch (err) {
    const error = (err.stderr && err.stderr.trim()) || err.message;
    return {
      attempted: true,
      success: false,
      // 'push' means the commit landed and only the mirror to the remote
      // failed - the board state is not lost, and `git push` sends the whole
      // backlog, so the next successful sync clears it.
      step,
      committed: step === 'push',
      unpushed: step === 'push' ? await unpushedCount() : 0,
      expiredCredential: isExpiredCredential(error),
      error,
    };
  }
}

module.exports = { commitAndPushBoardState };
