'use strict';

// The five statuses that exist as real Jira workflow statuses for PSACT.
// These map 1:1 to board columns of the same name. Order here is the order
// the columns appear on the board, left to right.
const STATUS_COLUMNS = ['To Do', 'In Progress', 'On Hold', 'Waiting', 'Done'];

// "Unassigned" is not a Jira status - it's a local pool for tickets with no
// assignee. A ticket's presence there is derived from assignee=null, not from
// its Jira status. It's always the leftmost column.
const UNASSIGNED_COLUMN = 'Unassigned';

const ALL_COLUMNS = [UNASSIGNED_COLUMN, ...STATUS_COLUMNS];

// Done tickets older than this (by Jira `updated`) are dropped from the board
// on refresh, so the column doesn't grow forever. Only applies to tickets
// with no pending local change.
const DONE_RETENTION_DAYS = 7;

const PROJECT_KEY = 'PSACT';

// The column the "parked" label tracks: entering it adds the label, leaving
// it removes the label.
//
// This is deliberately still "Waiting", even though PSACT gained a real
// "On Hold" status in Sep 2026. The binding predates that status - back then
// Waiting was the closest thing PSACT had to "on hold", so the label was
// pinned there. Whether "parked" should follow the new On Hold column instead
// (or apply to both) is a convention decision for the team, not something to
// infer: changing it silently would start adding and removing real Jira
// labels on a different set of tickets. The constant is named for the label
// rather than the concept precisely so it isn't mistaken for the new column.
const PARKED_COLUMN = 'Waiting';
const PARKED_LABEL = 'parked';

module.exports = {
  STATUS_COLUMNS,
  UNASSIGNED_COLUMN,
  ALL_COLUMNS,
  DONE_RETENTION_DAYS,
  PROJECT_KEY,
  PARKED_COLUMN,
  PARKED_LABEL,
};
