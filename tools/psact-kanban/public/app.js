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
  Done: '#00875a',
};

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

  for (const col of state.columns) renderColumn(col);
}

function renderColumn(col) {
  const { list, count } = columnEls[col];
  const all = state.board[col] || [];
  const entries = all.filter(matchesFilter);
  count.textContent = filterText ? `${entries.length}/${all.length}` : all.length;

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
      if (resp.gitSync.success) {
        toast('success', 'Board state committed and pushed to git');
      } else {
        toast('error', `Board state git sync failed: ${resp.gitSync.error}`, 8000);
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
