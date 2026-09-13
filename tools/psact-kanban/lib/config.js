'use strict';

// The four statuses that exist as real Jira workflow statuses for PSACT.
// These map 1:1 to board columns of the same name.
const STATUS_COLUMNS = ['To Do', 'In Progress', 'Waiting', 'Done'];

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

module.exports = {
  STATUS_COLUMNS,
  UNASSIGNED_COLUMN,
  ALL_COLUMNS,
  DONE_RETENTION_DAYS,
  PROJECT_KEY,
};
