'use strict';

const path = require('path');
const express = require('express');

const store = require('./lib/boardStore');
const jiraClient = require('./lib/jiraClient');
const { reconcile } = require('./lib/reconcile');
const { computeDiff } = require('./lib/diff');
const { pushChanges } = require('./lib/push');
const { cleanupComment } = require('./lib/cleanup');
const { ALL_COLUMNS } = require('./lib/config');

const PORT = process.env.PORT || 4287;
const app = express();
app.use(express.json());
app.use(express.static(path.join(__dirname, 'public')));

let myself = null; // { accountId, displayName, avatarUrl, email }

async function ensureMyself() {
  if (myself) return myself;
  myself = await jiraClient.getMyself();
  const s = store.getState();
  s.currentUser = myself;
  store.save();
  return myself;
}

function boardPayload() {
  const s = store.getState();
  return {
    currentUser: s.currentUser,
    lastSyncedAt: s.lastSyncedAt,
    columns: ALL_COLUMNS,
    board: Object.fromEntries(
      ALL_COLUMNS.map((col) => [
        col,
        s.columns[col].map((key) => ({ key, ...s.tickets[key] })),
      ])
    ),
    diff: computeDiff(s, store.findColumn),
    changeLog: s.changeLog.slice(0, 20),
  };
}

app.get('/api/board', async (req, res) => {
  try {
    await ensureMyself();
    res.json(boardPayload());
  } catch (err) {
    res.status(500).json({ error: err.message });
  }
});

app.post('/api/refresh', async (req, res) => {
  try {
    const me = await ensureMyself();
    const issues = await jiraClient.searchMineAndUnassigned(me.accountId);
    reconcile(store, issues, me.accountId, jiraBaseUrl());
    res.json(boardPayload());
  } catch (err) {
    res.status(500).json({ error: err.message });
  }
});

app.post('/api/move', (req, res) => {
  try {
    const { key, toColumn, toIndex } = req.body || {};
    if (!key || !ALL_COLUMNS.includes(toColumn)) {
      return res.status(400).json({ error: 'key and a valid toColumn are required' });
    }
    const s = store.getState();
    if (!s.tickets[key]) return res.status(404).json({ error: `Unknown ticket ${key}` });
    store.placeTicket(key, toColumn, toIndex);
    store.save();
    res.json(boardPayload());
  } catch (err) {
    res.status(500).json({ error: err.message });
  }
});

app.post('/api/discard', (req, res) => {
  try {
    const { key, all } = req.body || {};
    const s = store.getState();
    if (all) {
      for (const [k, ticket] of Object.entries(s.tickets)) {
        if (store.findColumn(k) === ticket.baselineColumn) continue; // no pending change - leave local order alone
        store.placeTicket(k, ticket.baselineColumn);
        ticket.conflict = false;
        ticket.conflictInfo = null;
      }
    } else if (key) {
      const ticket = s.tickets[key];
      if (!ticket) return res.status(404).json({ error: `Unknown ticket ${key}` });
      store.placeTicket(key, ticket.baselineColumn);
      ticket.conflict = false;
      ticket.conflictInfo = null;
    } else {
      return res.status(400).json({ error: 'key or all is required' });
    }
    store.save();
    res.json(boardPayload());
  } catch (err) {
    res.status(500).json({ error: err.message });
  }
});

// Comments bypass the review/push queue entirely - they post to Jira the
// moment this is called, per the user's explicit "push immediately" request.
app.post('/api/comment', async (req, res) => {
  try {
    const { key, text } = req.body || {};
    if (!key || !text || !text.trim()) {
      return res.status(400).json({ error: 'key and non-empty text are required' });
    }
    const s = store.getState();
    if (!s.tickets[key]) return res.status(404).json({ error: `Unknown ticket ${key}` });

    const { adf, preview } = cleanupComment(text);
    await jiraClient.addComment(key, adf);
    store.logChange('comment', key, preview);
    store.save();
    res.json({ success: true, key, posted: preview, ...boardPayload() });
  } catch (err) {
    res.status(500).json({ error: err.message });
  }
});

app.post('/api/push', async (req, res) => {
  try {
    const me = await ensureMyself();
    const { keys } = req.body || {};
    const results = await pushChanges(store, jiraClient, me.accountId, keys);
    res.json({ results, ...boardPayload() });
  } catch (err) {
    res.status(500).json({ error: err.message });
  }
});

function jiraBaseUrl() {
  return (process.env.JIRA_BASE_URL || '').replace(/\/$/, '');
}

app.listen(PORT, '127.0.0.1', () => {
  console.log(`psact-kanban listening on http://127.0.0.1:${PORT}`);
});
