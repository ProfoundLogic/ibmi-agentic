'use strict';

const { UNASSIGNED_COLUMN } = require('./config');

// Computes the pending Jira actions for one ticket given where it started
// (baselineColumn, i.e. Jira truth as of last sync) and where it sits now
// (currentColumn, i.e. local board state). Order-only changes (same column)
// never produce actions - there's nothing on the Jira side to push for pure
// re-ranking.
function actionsFor(baselineColumn, currentColumn) {
  if (baselineColumn === currentColumn) return [];
  const actions = [];
  if (baselineColumn === UNASSIGNED_COLUMN) {
    actions.push({ type: 'assign', to: 'me' });
    actions.push({ type: 'status', to: currentColumn });
  } else if (currentColumn === UNASSIGNED_COLUMN) {
    actions.push({ type: 'unassign' });
  } else {
    actions.push({ type: 'status', to: currentColumn });
  }
  return actions;
}

// Builds the full list of pending changes across the board, for display in
// the review panel and for the push endpoint to act on.
function computeDiff(state, findColumnFn) {
  const diff = [];
  for (const [key, ticket] of Object.entries(state.tickets)) {
    const currentColumn = findColumnFn(key);
    if (currentColumn === ticket.baselineColumn) continue;
    diff.push({
      key,
      summary: ticket.data && ticket.data.summary,
      from: ticket.baselineColumn,
      to: currentColumn,
      conflict: !!ticket.conflict,
      conflictInfo: ticket.conflictInfo || null,
      actions: actionsFor(ticket.baselineColumn, currentColumn),
    });
  }
  return diff;
}

module.exports = { actionsFor, computeDiff };
