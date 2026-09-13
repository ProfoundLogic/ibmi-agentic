'use strict';

const { UNASSIGNED_COLUMN, STATUS_COLUMNS, DONE_RETENTION_DAYS } = require('./config');

function statusColumn(statusName) {
  return STATUS_COLUMNS.includes(statusName) ? statusName : 'To Do';
}

function daysSince(isoString) {
  if (!isoString) return Infinity;
  return (Date.now() - new Date(isoString).getTime()) / 86400000;
}

// Merges freshly-pulled Jira issues into the persisted board state:
//  - new tickets are appended to the column matching Jira's current truth
//  - tickets with no pending local change silently follow Jira's truth
//  - tickets with a pending local change are left alone, but flagged as a
//    conflict if Jira's truth diverged from what the pending edit assumed
//  - tickets that fell out of the Jira query (reassigned elsewhere,
//    cancelled) are dropped if there's no pending change, else flagged
//  - Done tickets older than the retention window are dropped, if clean
function reconcile(store, issues, myAccountId, jiraBaseUrl) {
  const s = store.getState();
  const freshByKey = new Map(issues.map((i) => [i.key, i]));

  for (const issue of issues) {
    const authoritativeColumn =
      issue.assigneeAccountId === myAccountId
        ? statusColumn(issue.status)
        : UNASSIGNED_COLUMN;

    const data = { ...issue, url: `${jiraBaseUrl}/browse/${issue.key}` };
    let ticket = s.tickets[issue.key];

    if (!ticket) {
      ticket = { baselineColumn: authoritativeColumn, conflict: false, conflictInfo: null, data };
      s.tickets[issue.key] = ticket;
      store.placeTicket(issue.key, authoritativeColumn);
      continue;
    }

    ticket.data = data;
    const currentColumn = store.findColumn(issue.key);
    const pending = currentColumn !== ticket.baselineColumn;

    if (!pending) {
      if (authoritativeColumn !== currentColumn) {
        store.placeTicket(issue.key, authoritativeColumn);
        ticket.baselineColumn = authoritativeColumn;
      }
      ticket.conflict = false;
      ticket.conflictInfo = null;
    } else if (authoritativeColumn !== ticket.baselineColumn) {
      ticket.conflict = true;
      ticket.conflictInfo = {
        reason: 'Jira status/assignee changed since your edit',
        jiraColumn: authoritativeColumn,
        notedAt: new Date().toISOString(),
      };
    } else {
      ticket.conflict = false;
      ticket.conflictInfo = null;
    }
  }

  // Evict tickets that dropped out of the "mine or unassigned, not
  // cancelled" query, or that are stale Done tickets - but only when clean.
  for (const key of Object.keys(s.tickets)) {
    const ticket = s.tickets[key];
    const currentColumn = store.findColumn(key);
    const pending = currentColumn !== ticket.baselineColumn;

    if (!freshByKey.has(key)) {
      if (!pending) {
        removeTicket(s, key);
      } else {
        ticket.conflict = true;
        ticket.conflictInfo = {
          reason: 'Ticket no longer matches your board (reassigned elsewhere or cancelled in Jira)',
          jiraColumn: null,
          notedAt: new Date().toISOString(),
        };
      }
      continue;
    }

    if (!pending && currentColumn === 'Done' && daysSince(ticket.data.updated) > DONE_RETENTION_DAYS) {
      removeTicket(s, key);
    }
  }

  s.currentUser = s.currentUser; // unchanged here, set by caller
  s.lastSyncedAt = new Date().toISOString();
  store.save();
  return s;
}

function removeTicket(s, key) {
  for (const col of Object.keys(s.columns)) {
    const i = s.columns[col].indexOf(key);
    if (i !== -1) s.columns[col].splice(i, 1);
  }
  delete s.tickets[key];
}

module.exports = { reconcile, statusColumn };
