'use strict';

const { computeDiff, actionsFor } = require('./diff');

// Pushes pending local changes to Jira. If `onlyKeys` is given (array),
// restricts the push to those ticket keys; otherwise pushes every pending
// change. Returns a per-ticket result list. Saves progress after each
// ticket so a mid-run failure doesn't lose already-applied changes.
async function pushChanges(store, jiraClient, myAccountId, onlyKeys) {
  const s = store.getState();
  const diff = computeDiff(s, store.findColumn).filter(
    (d) => !onlyKeys || onlyKeys.includes(d.key)
  );

  const results = [];
  for (const entry of diff) {
    const ticket = s.tickets[entry.key];
    const actions = actionsFor(ticket.baselineColumn, store.findColumn(entry.key));
    try {
      for (const action of actions) {
        if (action.type === 'assign') {
          await jiraClient.assignIssue(entry.key, myAccountId);
        } else if (action.type === 'unassign') {
          await jiraClient.assignIssue(entry.key, null);
        } else if (action.type === 'status') {
          await jiraClient.transitionIssue(entry.key, action.to);
        }
      }
      ticket.baselineColumn = store.findColumn(entry.key);
      ticket.conflict = false;
      ticket.conflictInfo = null;
      store.logChange('push', entry.key, `${entry.from} -> ${entry.to}`);
      results.push({ key: entry.key, success: true, from: entry.from, to: entry.to });
    } catch (err) {
      results.push({ key: entry.key, success: false, from: entry.from, to: entry.to, error: err.message });
    }
    store.save();
  }
  return results;
}

module.exports = { pushChanges };
