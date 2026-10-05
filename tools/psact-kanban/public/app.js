'use strict';

/* ---------- state ---------- */
let state = null; // full board payload from the server
let filterText = '';
const columnEls = {}; // col name -> { root, list, count }
const COLUMN_COLOR = {
  Unassigned: '#6b778c',
  'To Do': '#0052cc',
  'In Progress': '#ff991f',
  Waiting: '#6554c0',
  // Deliberately a darker shade of Waiting's violet: On Hold sits next to it
  // and the two are sibling "not being worked right now" states, so reading
  // as a pair is the point. Distinct enough in lightness to tell apart.
  'On Hold': '#403294',
  Done: '#00875a',
};

const DONE_COLUMN = 'Done';

// Whether the Done column is on screen. This is a *view* preference, not board
// data, so it lives in localStorage rather than in data/board-state.json -
// that file is committed to git, and writing it on every toggle would produce
// a diff for a UI choice. The proxy path changes with each container but the
// origin doesn't, so the preference still survives a container swap.
//
// Default is hidden: Done is only needed when moving cards in or out of it,
// and the other five columns want the room.
// Epics the board is filtered to. Empty = show everything. Several at once is
// additive (a ticket matches if it is in ANY selected epic), which is what the
// filter strip's multi-select implies.
const selectedEpics = new Set();
// Stand-in key for "has no epic at all". Without it, a ticket with no parent -
// PSACT-85 is one right now - becomes unreachable the moment any epic filter
// is on, with no pill that can bring it back.
const NO_EPIC = '__no_epic__';

const SHOW_DONE_KEY = 'psact.showDone';
let showDone = localStorage.getItem(SHOW_DONE_KEY) === '1';

const boardEl = document.getElementById('board');
const toastsEl = document.getElementById('toasts');
const cardTemplate = document.getElementById('cardTemplate');

/* ---------- tiny fetch helper ---------- */
async function api(path, opts = {}) {
  const res = await fetch(path, {
    headers: { 'Content-Type': 'application/json' },
    ...opts,
  });
  const body = await res.json().catch(() => ({}));
  if (!res.ok) throw new Error(body.error || `HTTP ${res.status}`);
  return body;
}

/* ---------- FLIP animation helpers ---------- */
function flipCapture() {
  const rects = new Map();
  document.querySelectorAll('.card[data-key]').forEach((el) => {
    rects.set(el.dataset.key, el.getBoundingClientRect());
  });
  return rects;
}

function flipPlay(prevRects) {
  document.querySelectorAll('.card[data-key]').forEach((el) => {
    const prev = prevRects.get(el.dataset.key);
    if (!prev) {
      el.classList.add('entering');
      requestAnimationFrame(() => requestAnimationFrame(() => el.classList.remove('entering')));
      return;
    }
    const next = el.getBoundingClientRect();
    const dx = prev.left - next.left;
    const dy = prev.top - next.top;
    if (dx || dy) {
      el.style.transition = 'none';
      el.style.transform = `translate(${dx}px, ${dy}px)`;
      requestAnimationFrame(() => {
        el.style.transition = '';
        el.style.transform = '';
      });
    }
  });
}

/* ---------- misc formatting helpers ---------- */
function initials(name) {
  if (!name) return '?';
  return name
    .split(/\s+/)
    .map((p) => p[0])
    .join('')
    .slice(0, 2)
    .toUpperCase();
}

function colorFromString(str) {
  let hash = 0;
  for (let i = 0; i < str.length; i++) hash = str.charCodeAt(i) + ((hash << 5) - hash);
  const hue = Math.abs(hash) % 360;
  return `hsl(${hue}, 55%, 42%)`;
}

function formatDate(iso) {
  if (!iso) return '';
  const d = new Date(iso.length === 10 ? iso + 'T00:00:00' : iso);
  return d.toLocaleDateString(undefined, { month: 'short', day: 'numeric' });
}

function dueUrgency(duedate) {
  if (!duedate) return null;
  const today = new Date();
  today.setHours(0, 0, 0, 0);
  const due = new Date(duedate + 'T00:00:00');
  const days = Math.round((due - today) / 86400000);
  if (days < 0) return 'overdue';
  if (days <= 3) return 'soon';
  return null;
}

function priorityClass(name) {
  if (!name) return '';
  const n = name.toLowerCase();
  if (n.includes('high')) return 'overdue';
  if (n.includes('medium')) return 'soon';
  return '';
}

function pendingActionsFor(key) {
  const entry = (state.diff || []).find((d) => d.key === key);
  return entry ? entry.actions : [];
}

function matchesFilter(ticket) {
  // Epic filter and text filter are AND-ed: narrowing by epic and typing a
  // word should do both, not either.
  if (selectedEpics.size && !selectedEpics.has(ticket.data.epicKey || NO_EPIC)) {
    return false;
  }
  if (!filterText) return true;
  const needle = filterText.toLowerCase();
  const haystack = [
    ticket.key,
    ticket.data.summary,
    ticket.data.epicKey,
    ticket.data.epicName,
    ...(ticket.data.labels || []),
  ]
    .filter(Boolean)
    .join(' ')
    .toLowerCase();
  return haystack.includes(needle);
}

/* ---------- toasts ---------- */
function toast(type, message, ttl = 4500) {
  const el = document.createElement('div');
  el.className = `toast ${type}`;
  el.textContent = message;
  toastsEl.appendChild(el);
  setTimeout(() => {
    el.classList.add('leaving');
    setTimeout(() => el.remove(), 200);
  }, ttl);
}

/* ---------- board scaffolding ---------- */
function buildColumnsScaffold() {
  boardEl.innerHTML = '';
  for (const col of state.columns) {
    const root = document.createElement('section');
    root.className = 'column';
    root.style.setProperty('--col-color', COLUMN_COLOR[col] || '#0052cc');

    const header = document.createElement('div');
    header.className = 'column-header';
    const title = document.createElement('span');
    title.textContent = col;
    const count = document.createElement('span');
    count.className = 'column-count';
    header.append(title, count);

    const list = document.createElement('div');
    list.className = 'column-list';
    list.dataset.column = col;
    attachColumnDnD(list, col);

    root.append(header, list);
    boardEl.append(root);
    columnEls[col] = { root, list, count };
  }
}

// Done is built and rendered like any other column even while hidden, so
// toggling it back on is a pure CSS flip with no re-fetch and no re-render.
function applyDoneVisibility() {
  const entry = columnEls[DONE_COLUMN];
  if (entry) entry.root.classList.toggle('is-hidden', !showDone);
  const btn = document.getElementById('doneToggle');
  document.getElementById('doneToggleLabel').textContent =
    showDone ? 'Hide Done' : 'Show Done';
  btn.setAttribute('aria-pressed', String(showDone));
}

function attachColumnDnD(list, col) {
  list.addEventListener('dragover', (e) => {
    e.preventDefault();
    e.dataTransfer.dropEffect = 'move';
    list.classList.add('drag-over');
    const placeholder = ensurePlaceholder();
    const after = getDragAfterElement(list, e.clientY);
    if (after == null) list.appendChild(placeholder);
    else list.insertBefore(placeholder, after);
  });

  list.addEventListener('dragleave', (e) => {
    if (e.target === list) list.classList.remove('drag-over');
  });

  list.addEventListener('drop', (e) => {
    e.preventDefault();
    list.classList.remove('drag-over');
    const key = e.dataTransfer.getData('text/plain');
    const placeholder = document.getElementById('dropPlaceholder');
    let toIndex = list.children.length;
    if (placeholder) {
      toIndex = Array.from(list.children).indexOf(placeholder);
      placeholder.remove();
    }
    if (key) moveTicket(key, col, toIndex);
  });
}

function ensurePlaceholder() {
  let el = document.getElementById('dropPlaceholder');
  if (!el) {
    el = document.createElement('div');
    el.id = 'dropPlaceholder';
    el.className = 'drop-placeholder';
  }
  return el;
}

function getDragAfterElement(container, y) {
  const cards = [...container.querySelectorAll('.card:not(.dragging)')];
  return cards.reduce(
    (closest, child) => {
      const box = child.getBoundingClientRect();
      const offset = y - box.top - box.height / 2;
      if (offset < 0 && offset > closest.offset) return { offset, element: child };
      return closest;
    },
    { offset: Number.NEGATIVE_INFINITY, element: null }
  ).element;
}

document.addEventListener('dragend', () => {
  const p = document.getElementById('dropPlaceholder');
  if (p) p.remove();
  document.querySelectorAll('.column-list.drag-over').forEach((l) => l.classList.remove('drag-over'));
});

/* ---------- epic filter strip ---------- */
const epicFilterEl = document.getElementById('epicFilter');

// "PSACT-11" must sort after "PSACT-4". A plain string sort gives
// 1, 11, 21, 4, 5 - which is exactly what this board produces.
function epicNumber(key) {
  const m = /-(\d+)\s*$/.exec(key || '');
  return m ? Number(m[1]) : Number.MAX_SAFE_INTEGER;
}

// Built from the columns actually on screen, so hiding Done also drops any
// epic that only lives there - a pill that filters to nothing is worse than
// no pill.
function epicsOnBoard() {
  const names = new Map();
  let noEpic = 0;
  for (const col of state.columns) {
    if (col === DONE_COLUMN && !showDone) continue;
    for (const t of state.board[col] || []) {
      const key = t.data.epicKey;
      if (key) {
        if (!names.has(key)) names.set(key, t.data.epicName || '');
      } else {
        noEpic += 1;
      }
    }
  }
  const list = [...names.entries()].sort((a, b) => epicNumber(a[0]) - epicNumber(b[0]));
  return { list, noEpic };
}

// Publish the real height of everything above the board, so the board's and
// columns' viewport-based heights can subtract it instead of guessing.
//
// Both parts have to be measured rather than hardcoded because both wrap: the
// filter strip goes to a second row on a narrow window, and the topbar's
// controls do the same (52px -> 92px at ~1100px). The old hardcoded 60px for
// the topbar was already wrong at that width - the page carried a 32px
// scrollbar before this strip existed - so measuring fixes that too.
function syncChromeHeight() {
  const topbar = document.querySelector('.topbar');
  const h = Math.round(
    (topbar ? topbar.getBoundingClientRect().height : 0) +
    epicFilterEl.getBoundingClientRect().height
  );
  document.documentElement.style.setProperty('--chrome-h', `${h}px`);
}
if (typeof ResizeObserver === 'function') {
  const ro = new ResizeObserver(syncChromeHeight);
  ro.observe(epicFilterEl);
  const tb = document.querySelector('.topbar');
  if (tb) ro.observe(tb);
}

function renderEpicFilter() {
  const { list, noEpic } = epicsOnBoard();

  // Drop selections for epics that are no longer on the board (a refresh moved
  // the last ticket out, or Done got hidden). Leaving them selected would
  // silently filter the board down to nothing.
  const live = new Set(list.map(([k]) => k));
  if (noEpic) live.add(NO_EPIC);
  for (const k of [...selectedEpics]) if (!live.has(k)) selectedEpics.delete(k);

  epicFilterEl.innerHTML = '';

  const chips = list.map(([key, name]) => ({
    key,
    text: name ? `${key} \u00b7 ${name}` : key,
  }));
  if (noEpic) chips.push({ key: NO_EPIC, text: `No epic (${noEpic})` });

  for (const c of chips) {
    const b = document.createElement('button');
    b.type = 'button';
    // Same `pill` class the cards use, so the chip and the pill on the ticket
    // are visually the same object.
    b.className = 'pill epic-chip';
    b.textContent = c.text;
    const on = selectedEpics.has(c.key);
    b.classList.toggle('is-on', on);
    b.setAttribute('aria-pressed', String(on));
    b.addEventListener('click', () => {
      if (selectedEpics.has(c.key)) selectedEpics.delete(c.key);
      else selectedEpics.add(c.key);
      render();
    });
    epicFilterEl.appendChild(b);
  }

  const clear = document.createElement('button');
  clear.type = 'button';
  clear.className = 'pill epic-chip epic-chip-clear';
  clear.textContent = selectedEpics.size
    ? `Clear filters (${selectedEpics.size})`
    : 'Clear filters';
  clear.disabled = selectedEpics.size === 0;
  clear.addEventListener('click', () => {
    selectedEpics.clear();
    render();
  });
  epicFilterEl.appendChild(clear);

  syncChromeHeight();
}

/* ---------- rendering ---------- */
function render() {
  document.getElementById('userChip').textContent = state.currentUser
    ? state.currentUser.displayName
    : '…';
  document.getElementById('syncedAt').textContent = state.lastSyncedAt
    ? `synced ${new Date(state.lastSyncedAt).toLocaleString()}`
    : 'never synced';

  const diffBadge = document.getElementById('diffCount');
  diffBadge.textContent = state.diff.length;
  diffBadge.classList.toggle('zero', state.diff.length === 0);

  const doneTotal = (state.board[DONE_COLUMN] || []).length;
  const doneBadge = document.getElementById('doneCount');
  doneBadge.textContent = doneTotal;
  doneBadge.classList.toggle('zero', doneTotal === 0);

  renderEpicFilter();
  for (const col of state.columns) renderColumn(col);
  applyDoneVisibility();
}

function renderColumn(col) {
  const { list, count } = columnEls[col];
  const all = state.board[col] || [];
  const entries = all.filter(matchesFilter);
  const filtering = Boolean(filterText) || selectedEpics.size > 0;
  count.textContent = filtering ? `${entries.length}/${all.length}` : all.length;

  list.innerHTML = '';
  if (entries.length === 0) {
    const hint = document.createElement('div');
    hint.className = 'column-empty-hint';
    hint.textContent = all.length === 0 ? 'Drop tickets here' : 'No matches';
    list.appendChild(hint);
    return;
  }
  entries.forEach((ticket, idx) => list.appendChild(buildCard(ticket, col, idx, entries.length)));
}

function buildCard(ticket, col, idx, total) {
  const node = cardTemplate.content.firstElementChild.cloneNode(true);
  const data = ticket.data || {};
  const pending = ticket.baselineColumn !== col;

  node.dataset.key = ticket.key;
  node.classList.toggle('pending', pending);
  node.classList.toggle('conflict', !!ticket.conflict);

  const keyLink = node.querySelector('.card-key');
  keyLink.textContent = ticket.key;
  keyLink.href = data.url || '#';

  const conflictFlag = node.querySelector('.conflict-flag');
  if (ticket.conflict) {
    conflictFlag.hidden = false;
    conflictFlag.title = ticket.conflictInfo ? ticket.conflictInfo.reason : 'Changed in Jira since your edit';
  }

  node.querySelector('.comment-btn').addEventListener('click', (e) => {
    e.stopPropagation();
    openCommentModal(ticket.key, data.summary);
  });

  const viewBtn = node.querySelector('.view-comments-btn');
  const nComments = (data.comments || []).length;
  viewBtn.querySelector('.vc-count').textContent = nComments;
  viewBtn.classList.toggle('is-empty', nComments === 0);
  viewBtn.title = nComments
    ? `View ${nComments} comment${nComments === 1 ? '' : 's'}`
    : 'No comments yet';
  viewBtn.addEventListener('click', (e) => {
    e.stopPropagation();
    openViewComments(ticket.key);
  });

  const discardBtn = node.querySelector('.discard-btn');
  if (pending) {
    discardBtn.hidden = false;
    discardBtn.addEventListener('click', (e) => {
      e.stopPropagation();
      discardTicket(ticket.key);
    });
  }

  node.querySelector('.card-summary').textContent = data.summary || '(no summary)';

  const epicPill = node.querySelector('.epic-pill');
  if (data.epicKey) {
    epicPill.hidden = false;
    epicPill.textContent = data.epicName ? `${data.epicKey} · ${data.epicName}` : data.epicKey;
  }

  const priorityPill = node.querySelector('.priority-pill');
  if (data.priority) {
    priorityPill.hidden = false;
    priorityPill.textContent = data.priority;
  }

  const duePill = node.querySelector('.due-pill');
  if (data.duedate) {
    duePill.hidden = false;
    duePill.textContent = formatDate(data.duedate);
    const urgency = dueUrgency(data.duedate);
    if (urgency) duePill.classList.add(urgency);
  }

  const labelsEl = node.querySelector('.card-labels');
  const pendingActions = pending ? pendingActionsFor(ticket.key) : [];
  const pendingAdds = pendingActions.filter((a) => a.type === 'addLabel').map((a) => a.label);
  const pendingRemoves = pendingActions.filter((a) => a.type === 'removeLabel').map((a) => a.label);
  (data.labels || []).forEach((label) => {
    const tag = document.createElement('span');
    tag.className = 'label-tag' + (pendingRemoves.includes(label) ? ' label-pending-remove' : '');
    tag.textContent = label;
    if (pendingRemoves.includes(label)) tag.title = 'Will be removed on push';
    labelsEl.appendChild(tag);
  });
  pendingAdds
    .filter((label) => !(data.labels || []).includes(label))
    .forEach((label) => {
      const tag = document.createElement('span');
      tag.className = 'label-tag label-pending-add';
      tag.textContent = label;
      tag.title = 'Will be added on push';
      labelsEl.appendChild(tag);
    });

  const avatar = node.querySelector('.avatar');
  if (col === 'Unassigned') {
    avatar.classList.add('unassigned');
    avatar.textContent = '?';
    avatar.title = 'Unassigned';
  } else {
    const name = (state.currentUser && state.currentUser.displayName) || 'Me';
    avatar.textContent = initials(name);
    avatar.style.background = colorFromString(name);
    avatar.title = name;
  }

  const upBtn = node.querySelector('.move-up');
  const downBtn = node.querySelector('.move-down');
  upBtn.disabled = idx === 0;
  downBtn.disabled = idx === total - 1;
  upBtn.addEventListener('click', (e) => {
    e.stopPropagation();
    moveTicket(ticket.key, col, idx - 1);
  });
  downBtn.addEventListener('click', (e) => {
    e.stopPropagation();
    moveTicket(ticket.key, col, idx + 1);
  });

  const select = node.querySelector('.move-select');
  const placeholderOpt = document.createElement('option');
  placeholderOpt.textContent = 'Move to…';
  placeholderOpt.value = '';
  placeholderOpt.disabled = true;
  placeholderOpt.selected = true;
  select.appendChild(placeholderOpt);
  state.columns
    .filter((c) => c !== col)
    .forEach((c) => {
      const opt = document.createElement('option');
      opt.value = c;
      opt.textContent = c;
      select.appendChild(opt);
    });
  select.addEventListener('click', (e) => e.stopPropagation());
  select.addEventListener('change', (e) => {
    const target = e.target.value;
    e.target.value = '';
    if (target) moveTicket(ticket.key, target, undefined);
  });

  node.addEventListener('click', (e) => {
    if (e.target.closest('.card-controls') || e.target.closest('.discard-btn') || e.target.closest('.card-key')) return;
    if (data.url) window.open(data.url, '_blank', 'noopener');
  });

  node.addEventListener('dragstart', (e) => {
    e.dataTransfer.setData('text/plain', ticket.key);
    e.dataTransfer.effectAllowed = 'move';
    requestAnimationFrame(() => node.classList.add('dragging'));
  });
  node.addEventListener('dragend', () => node.classList.remove('dragging'));

  return node;
}

/* ---------- local optimistic mutation ---------- */
function applyLocalMove(key, toColumn, toIndex) {
  let ticket = null;
  for (const col of state.columns) {
    const list = state.board[col];
    const i = list.findIndex((t) => t.key === key);
    if (i !== -1) {
      ticket = list.splice(i, 1)[0];
      break;
    }
  }
  if (!ticket) return;
  const dest = state.board[toColumn];
  const idx = toIndex === undefined || toIndex === null ? dest.length : Math.max(0, Math.min(toIndex, dest.length));
  dest.splice(idx, 0, ticket);
}

/* ---------- actions ---------- */
async function moveTicket(key, toColumn, toIndex) {
  const prevRects = flipCapture();
  applyLocalMove(key, toColumn, toIndex);
  render();
  flipPlay(prevRects);

  try {
    const fresh = await api('api/move', {
      method: 'POST',
      body: JSON.stringify({ key, toColumn, toIndex }),
    });
    const rects = flipCapture();
    state = fresh;
    render();
    flipPlay(rects);
    // Without this the card simply vanishes - the move worked, but the column
    // it landed in isn't on screen.
    if (toColumn === DONE_COLUMN && !showDone) {
      toast('info', `${key} moved to Done - that column is hidden`);
    }
  } catch (err) {
    toast('error', `Couldn't save move for ${key}: ${err.message}`);
    await refreshFromServer(false);
  }
}

async function discardTicket(key) {
  try {
    const rects = flipCapture();
    const fresh = await api('api/discard', { method: 'POST', body: JSON.stringify({ key }) });
    state = fresh;
    render();
    flipPlay(rects);
    toast('success', `Discarded local change on ${key}`);
  } catch (err) {
    toast('error', err.message);
  }
}

async function discardAll() {
  try {
    const rects = flipCapture();
    const fresh = await api('api/discard', { method: 'POST', body: JSON.stringify({ all: true }) });
    state = fresh;
    render();
    flipPlay(rects);
    toast('success', 'Discarded all pending changes');
  } catch (err) {
    toast('error', err.message);
  }
}

async function refreshFromServer(showToast = true) {
  const spinner = document.getElementById('refreshSpinner');
  spinner.hidden = false;
  try {
    const rects = flipCapture();
    const fresh = await api('api/refresh', { method: 'POST' });
    state = fresh;
    render();
    flipPlay(rects);
    if (showToast) toast('success', 'Board refreshed from Jira');
  } catch (err) {
    toast('error', `Refresh failed: ${err.message}`);
  } finally {
    spinner.hidden = true;
  }
}

/* ---------- review & push modal ---------- */
const overlay = document.getElementById('modalOverlay');
const modalBody = document.getElementById('modalBody');
const pushBtn = document.getElementById('pushSelectedBtn');
let selectedKeys = new Set();

function openReviewModal() {
  selectedKeys = new Set(state.diff.filter((d) => !d.conflict).map((d) => d.key));
  renderModalBody();
  overlay.hidden = false;
}

function closeReviewModal() {
  overlay.hidden = true;
}

function renderModalBody() {
  modalBody.innerHTML = '';
  pushBtn.hidden = false;
  pushBtn.disabled = false;
  pushBtn.textContent = `Push selected (${selectedKeys.size})`;

  if (state.diff.length === 0) {
    const empty = document.createElement('div');
    empty.className = 'diff-empty';
    empty.textContent = 'No pending changes. Move some cards around first.';
    modalBody.appendChild(empty);
    pushBtn.hidden = true;
    return;
  }

  for (const entry of state.diff) {
    const row = document.createElement('div');
    row.className = 'diff-row' + (entry.conflict ? ' conflict' : '');

    const checkbox = document.createElement('input');
    checkbox.type = 'checkbox';
    checkbox.checked = selectedKeys.has(entry.key);
    checkbox.addEventListener('change', () => {
      if (checkbox.checked) selectedKeys.add(entry.key);
      else selectedKeys.delete(entry.key);
      pushBtn.textContent = `Push selected (${selectedKeys.size})`;
    });

    const body = document.createElement('div');
    body.className = 'diff-row-body';

    const title = document.createElement('div');
    title.className = 'diff-row-title';
    const link = document.createElement('a');
    link.href = entry.key && state.board
      ? Object.values(state.board).flat().find((t) => t.key === entry.key)?.data?.url || '#'
      : '#';
    link.target = '_blank';
    link.rel = 'noopener';
    link.textContent = entry.key;
    title.append(link, document.createTextNode(`  ${entry.from} → ${entry.to}`));

    const summary = document.createElement('div');
    summary.className = 'diff-row-summary';
    summary.textContent = entry.summary || '';

    const actions = document.createElement('div');
    actions.className = 'diff-actions';
    entry.actions.forEach((a) => {
      const pill = document.createElement('span');
      pill.className = 'action-pill' + (a.type === 'unassign' || a.type === 'removeLabel' ? ' unassign' : '');
      pill.textContent =
        a.type === 'assign' ? 'Assign to you'
        : a.type === 'unassign' ? 'Unassign'
        : a.type === 'addLabel' ? `+ ${a.label} label`
        : a.type === 'removeLabel' ? `− ${a.label} label`
        : `Status → ${a.to}`;
      actions.appendChild(pill);
    });

    body.append(title, summary, actions);

    if (entry.conflict) {
      const note = document.createElement('div');
      note.className = 'conflict-note';
      note.textContent = `⚠ ${entry.conflictInfo ? entry.conflictInfo.reason : 'Changed in Jira since your edit'} — review before pushing.`;
      body.appendChild(note);
    }

    row.append(checkbox, body);
    modalBody.appendChild(row);
  }
}

async function pushSelected() {
  if (selectedKeys.size === 0) {
    toast('error', 'Nothing selected to push');
    return;
  }
  pushBtn.disabled = true;
  pushBtn.textContent = 'Pushing…';
  try {
    const resp = await api('api/push', {
      method: 'POST',
      body: JSON.stringify({ keys: Array.from(selectedKeys) }),
    });
    const rects = flipCapture();
    state = resp;
    render();
    flipPlay(rects);

    const ok = resp.results.filter((r) => r.success).length;
    const fail = resp.results.filter((r) => !r.success);
    if (fail.length === 0) {
      toast('success', `Pushed ${ok} change${ok === 1 ? '' : 's'} to Jira`);
      closeReviewModal();
    } else {
      toast('error', `${ok} pushed, ${fail.length} failed: ${fail.map((f) => `${f.key} (${f.error})`).join('; ')}`, 8000);
      renderModalBody();
    }

    if (resp.gitSync && resp.gitSync.attempted) {
      const g = resp.gitSync;
      if (g.success) {
        toast('success', 'Board state committed and pushed to git');
      } else if (g.committed) {
        // The Jira push already succeeded and the board state is committed
        // locally - only the mirror to the remote failed. That is a warning,
        // not an error, and the old red toast full of raw git stderr made it
        // look as though the whole operation had failed.
        const waiting = g.unpushed
          ? `${g.unpushed} commit${g.unpushed === 1 ? '' : 's'} waiting`
          : 'waiting to push';
        const why = g.expiredCredential
          ? "this container's git credential has expired - fork the task for a fresh one"
          : g.error.split('\n')[0];
        toast('warn',
          `Jira updated. Board state saved and committed locally but not mirrored to git (${waiting}): ${why}. The next successful sync pushes the backlog.`,
          9000);
      } else {
        toast('error', `Board state could not be saved to git: ${g.error}`, 9000);
      }
    }
  } catch (err) {
    toast('error', `Push failed: ${err.message}`);
    pushBtn.disabled = false;
    pushBtn.textContent = `Push selected (${selectedKeys.size})`;
  }
}

/* ---------- comment modal ---------- */
const commentOverlay = document.getElementById('commentModalOverlay');
const commentTitle = document.getElementById('commentModalTitle');
const commentTextarea = document.getElementById('commentTextarea');
const commentPostBtn = document.getElementById('commentPostBtn');
let commentTargetKey = null;

function openCommentModal(key, summary) {
  commentTargetKey = key;
  commentTitle.textContent = `Comment on ${key}${summary ? ' — ' + summary : ''}`;
  commentTextarea.value = '';
  commentPostBtn.disabled = false;
  commentPostBtn.textContent = 'Post to Jira';
  commentOverlay.hidden = false;
  commentTextarea.focus();
}

function closeCommentModal() {
  commentOverlay.hidden = true;
  commentTargetKey = null;
}

async function postComment() {
  const text = commentTextarea.value.trim();
  if (!text) {
    toast('error', 'Type something first');
    return;
  }
  const key = commentTargetKey;
  commentPostBtn.disabled = true;
  commentPostBtn.textContent = 'Posting…';
  try {
    const resp = await api('api/comment', {
      method: 'POST',
      body: JSON.stringify({ key, text }),
    });
    const rects = flipCapture();
    state = resp;
    render();
    flipPlay(rects);
    closeCommentModal();
    toast('success', `Posted to ${key}: “${resp.posted.split('\n')[0]}${resp.posted.includes('\n') ? '…' : ''}”`);
  } catch (err) {
    toast('error', `Couldn't post comment: ${err.message}`);
    commentPostBtn.disabled = false;
    commentPostBtn.textContent = 'Post to Jira';
  }
}

/* ---------- comment viewer ---------- */
const viewOverlay = document.getElementById('viewCommentsOverlay');
const viewBody = document.getElementById('viewCommentsBody');
const viewTitle = document.getElementById('viewCommentsTitle');
const viewNote = document.getElementById('viewCommentsNote');
const viewJiraLink = document.getElementById('viewCommentsJiraLink');
let viewTargetKey = null;

function findTicket(key) {
  for (const col of state.columns) {
    const hit = (state.board[col] || []).find((t) => t.key === key);
    if (hit) return hit;
  }
  return null;
}

// Jira hands back an ISO timestamp; show it in the reader's own locale rather
// than raw, but keep the full value in a tooltip.
function commentStamp(iso) {
  const d = new Date(iso);
  return isNaN(d) ? iso : d.toLocaleString();
}

function openViewComments(key) {
  const ticket = findTicket(key);
  if (!ticket) return;
  const data = ticket.data || {};
  const comments = data.comments || [];

  viewTargetKey = key;
  viewTitle.textContent = `${key} — ${comments.length} comment${comments.length === 1 ? '' : 's'}`;
  viewJiraLink.href = data.url || '#';

  const shortfall = (data.commentTotal || comments.length) - comments.length;
  viewNote.textContent = shortfall > 0
    ? `Jira returned the ${comments.length} most recent of ${data.commentTotal}. Open in Jira for the full thread.`
    : '';

  viewBody.innerHTML = '';

  if (!comments.length) {
    const empty = document.createElement('div');
    empty.className = 'diff-empty';
    empty.textContent = 'No comments on this ticket yet.';
    viewBody.appendChild(empty);
  } else {
    // Oldest first - Jira already returns them that way, but sort rather than
    // trust it, so the order is right regardless of what the API does.
    const ordered = comments
      .slice()
      .sort((a, b) => new Date(a.created) - new Date(b.created));

    for (const c of ordered) {
      const item = document.createElement('article');
      item.className = 'vc-item';

      const head = document.createElement('div');
      head.className = 'vc-head';
      const who = document.createElement('span');
      who.className = 'vc-author';
      who.textContent = c.author;
      const when = document.createElement('time');
      when.className = 'vc-when';
      when.textContent = commentStamp(c.created);
      when.title = c.created;
      head.append(who, when);
      if (c.updated) {
        const edited = document.createElement('span');
        edited.className = 'vc-edited';
        edited.textContent = 'edited';
        edited.title = `Last edited ${commentStamp(c.updated)}`;
        head.appendChild(edited);
      }

      const body = document.createElement('div');
      body.className = 'vc-text';
      // textContent, never innerHTML: comment text is other people's input.
      // white-space: pre-wrap in the CSS preserves the line breaks.
      body.textContent = c.text || '(empty comment)';

      item.append(head, body);
      viewBody.appendChild(item);
    }
  }

  viewOverlay.hidden = false;
  // Long threads: show the newest first on screen by scrolling to the bottom.
  viewBody.scrollTop = viewBody.scrollHeight;
}

function closeViewComments() {
  viewOverlay.hidden = true;
  viewTargetKey = null;
}

/* ---------- wiring ---------- */
document.getElementById('refreshBtn').addEventListener('click', () => refreshFromServer(true));
document.getElementById('reviewBtn').addEventListener('click', openReviewModal);
document.getElementById('modalClose').addEventListener('click', closeReviewModal);
document.getElementById('modalCancel').addEventListener('click', closeReviewModal);
document.getElementById('pushSelectedBtn').addEventListener('click', pushSelected);
document.getElementById('discardAllBtn').addEventListener('click', () => {
  if (confirm('Discard every pending local change and revert to Jira truth?')) {
    discardAll();
    closeReviewModal();
  }
});
overlay.addEventListener('click', (e) => {
  if (e.target === overlay) closeReviewModal();
});
document.getElementById('commentModalClose').addEventListener('click', closeCommentModal);
document.getElementById('commentCancel').addEventListener('click', closeCommentModal);
document.getElementById('commentPostBtn').addEventListener('click', postComment);
commentOverlay.addEventListener('click', (e) => {
  if (e.target === commentOverlay) closeCommentModal();
});
commentTextarea.addEventListener('keydown', (e) => {
  if ((e.metaKey || e.ctrlKey) && e.key === 'Enter') postComment();
});
document.getElementById('viewCommentsClose').addEventListener('click', closeViewComments);
viewOverlay.addEventListener('click', (e) => {
  if (e.target === viewOverlay) closeViewComments();
});
document.getElementById('viewCommentsAdd').addEventListener('click', () => {
  const key = viewTargetKey;
  const ticket = key && findTicket(key);
  closeViewComments();
  if (ticket) openCommentModal(key, (ticket.data || {}).summary);
});
document.addEventListener('keydown', (e) => {
  if (e.key === 'Escape' && !viewOverlay.hidden) closeViewComments();
});
document.getElementById('doneToggle').addEventListener('click', () => {
  showDone = !showDone;
  localStorage.setItem(SHOW_DONE_KEY, showDone ? '1' : '0');
  applyDoneVisibility();
  renderEpicFilter();
});
document.getElementById('searchBox').addEventListener('input', (e) => {
  filterText = e.target.value.trim();
  render();
});

/* ---------- boot ---------- */
(async function init() {
  try {
    state = await api('api/board');
    buildColumnsScaffold();
    render();
    if (!state.lastSyncedAt) {
      await refreshFromServer(true);
    }
  } catch (err) {
    toast('error', `Failed to load board: ${err.message}`, 10000);
  }
})();
