'use strict';

const { PROJECT_KEY } = require('./config');

const FIELDS = [
  'summary',
  'status',
  'assignee',
  'priority',
  'duedate',
  'labels',
  'parent',
  'issuetype',
  'updated',
  'created',
];

function requireEnv() {
  const { JIRA_BASE_URL, JIRA_EMAIL, JIRA_API_TOKEN } = process.env;
  if (!JIRA_BASE_URL || !JIRA_EMAIL || !JIRA_API_TOKEN) {
    throw new Error(
      'JIRA_BASE_URL, JIRA_EMAIL and JIRA_API_TOKEN must be set in the environment.'
    );
  }
  return {
    baseUrl: JIRA_BASE_URL.replace(/\/$/, ''),
    email: JIRA_EMAIL,
    token: JIRA_API_TOKEN,
  };
}

function authHeader() {
  const { email, token } = requireEnv();
  return 'Basic ' + Buffer.from(`${email}:${token}`).toString('base64');
}

async function jiraFetch(path, options = {}) {
  const { baseUrl } = requireEnv();
  const res = await fetch(`${baseUrl}${path}`, {
    ...options,
    headers: {
      Authorization: authHeader(),
      'Content-Type': 'application/json',
      Accept: 'application/json',
      ...(options.headers || {}),
    },
  });

  const text = await res.text();
  let body;
  try {
    body = text ? JSON.parse(text) : {};
  } catch {
    body = { raw: text };
  }

  if (!res.ok) {
    const message =
      (body.errorMessages && body.errorMessages.join('; ')) ||
      (body.errors && JSON.stringify(body.errors)) ||
      text ||
      `HTTP ${res.status}`;
    const err = new Error(`Jira ${options.method || 'GET'} ${path} failed: ${message}`);
    err.status = res.status;
    err.body = body;
    throw err;
  }

  return body;
}

async function getMyself() {
  const me = await jiraFetch('/rest/api/3/myself');
  return {
    accountId: me.accountId,
    displayName: me.displayName,
    avatarUrl: me.avatarUrls && (me.avatarUrls['32x32'] || me.avatarUrls['48x48']),
    email: me.emailAddress,
  };
}

// Pulls every ticket in PSACT that is either assigned to `accountId` or has
// no assignee at all, excluding Cancelled. Pagination via the enhanced
// search endpoint's nextPageToken.
async function searchMineAndUnassigned(accountId) {
  const jql =
    `project = ${PROJECT_KEY} AND status != Cancelled AND issuetype != Epic AND ` +
    `(assignee = "${accountId}" OR assignee is EMPTY) ` +
    `ORDER BY updated DESC`;

  const issues = [];
  let nextPageToken;
  do {
    const body = await jiraFetch('/rest/api/3/search/jql', {
      method: 'POST',
      body: JSON.stringify({
        jql,
        maxResults: 100,
        fields: FIELDS,
        ...(nextPageToken ? { nextPageToken } : {}),
      }),
    });
    issues.push(...(body.issues || []));
    nextPageToken = body.isLast ? undefined : body.nextPageToken;
  } while (nextPageToken);

  return issues.map(normalizeIssue);
}

function normalizeIssue(issue) {
  const f = issue.fields || {};
  return {
    key: issue.key,
    url: null, // filled in by caller (needs baseUrl, which is server-side only)
    summary: f.summary || '',
    status: f.status && f.status.name,
    assigneeAccountId: f.assignee ? f.assignee.accountId : null,
    assigneeName: f.assignee ? f.assignee.displayName : null,
    priority: f.priority ? f.priority.name : null,
    priorityIcon: f.priority ? f.priority.iconUrl : null,
    duedate: f.duedate || null,
    labels: f.labels || [],
    epicKey: f.parent ? f.parent.key : null,
    epicName: f.parent && f.parent.fields ? f.parent.fields.summary : null,
    issuetype: f.issuetype ? f.issuetype.name : null,
    updated: f.updated,
    created: f.created,
  };
}

async function getTransitions(key) {
  const body = await jiraFetch(`/rest/api/3/issue/${key}/transitions`);
  return body.transitions || [];
}

async function transitionIssue(key, statusName) {
  const transitions = await getTransitions(key);
  const match = transitions.find(
    (t) => t.name.toLowerCase() === statusName.toLowerCase()
  );
  if (!match) {
    throw new Error(
      `No transition to "${statusName}" available for ${key} (available: ${transitions
        .map((t) => t.name)
        .join(', ') || 'none'})`
    );
  }
  await jiraFetch(`/rest/api/3/issue/${key}/transitions`, {
    method: 'POST',
    body: JSON.stringify({ transition: { id: match.id } }),
  });
}

async function assignIssue(key, accountId) {
  await jiraFetch(`/rest/api/3/issue/${key}/assignee`, {
    method: 'PUT',
    body: JSON.stringify({ accountId: accountId || null }),
  });
}

module.exports = {
  getMyself,
  searchMineAndUnassigned,
  getTransitions,
  transitionIssue,
  assignIssue,
};
