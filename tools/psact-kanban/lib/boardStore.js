'use strict';

const fs = require('fs');
const path = require('path');
const { ALL_COLUMNS } = require('./config');

const DATA_FILE = path.join(__dirname, '..', 'data', 'board-state.json');

function emptyState() {
  const columns = {};
  for (const col of ALL_COLUMNS) columns[col] = [];
  return {
    version: 1,
    currentUser: null,
    lastSyncedAt: null,
    columns,
    tickets: {}, // key -> { baselineColumn, conflict, data: {...jira fields} }
    changeLog: [], // { at, key, action, detail }
  };
}

let state = null;

function load() {
  if (state) return state;
  try {
    const raw = fs.readFileSync(DATA_FILE, 'utf8');
    state = JSON.parse(raw);
    // Guard against a hand-edited or older-shaped file missing a column.
    for (const col of ALL_COLUMNS) {
      if (!Array.isArray(state.columns[col])) state.columns[col] = [];
    }
  } catch (err) {
    if (err.code !== 'ENOENT') throw err;
    state = emptyState();
  }
  return state;
}

function save() {
  const tmp = DATA_FILE + '.tmp';
  fs.writeFileSync(tmp, JSON.stringify(state, null, 2));
  fs.renameSync(tmp, DATA_FILE);
}

function getState() {
  return load();
}

function findColumn(key) {
  const s = load();
  for (const col of ALL_COLUMNS) {
    if (s.columns[col].includes(key)) return col;
  }
  return null;
}

// Move (or insert) a ticket key into a column at a given index, removing it
// from wherever it currently lives first. index defaults to end of list.
function placeTicket(key, toColumn, toIndex) {
  const s = load();
  for (const col of ALL_COLUMNS) {
    const i = s.columns[col].indexOf(key);
    if (i !== -1) s.columns[col].splice(i, 1);
  }
  const list = s.columns[toColumn];
  const idx = toIndex === undefined || toIndex === null ? list.length : Math.max(0, Math.min(toIndex, list.length));
  list.splice(idx, 0, key);
}

function logChange(action, key, detail) {
  const s = load();
  s.changeLog.unshift({ at: new Date().toISOString(), key, action, detail });
  s.changeLog = s.changeLog.slice(0, 200);
}

module.exports = {
  DATA_FILE,
  getState,
  save,
  findColumn,
  placeTicket,
  logChange,
};
