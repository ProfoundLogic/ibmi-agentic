'use strict';

// Lightweight, deterministic comment cleanup - no LLM involved. Expands a
// small fixed set of common shorthand, fixes sentence casing/punctuation, and
// turns multi-line notes into a bullet list. Longer keys are checked first so
// e.g. "w/o" doesn't get half-eaten by the "w/" pattern.
const SHORTHAND = [
  ['w/o', 'without'],
  ['w/', 'with'],
  ['asap', 'as soon as possible'],
  ['imho', 'in my honest opinion'],
  ['imo', 'in my opinion'],
  ['btw', 'by the way'],
  ['thx', 'thanks'],
  ['ty', 'thanks'],
  ['pls', 'please'],
  ['plz', 'please'],
  ['wip', 'work in progress'],
  ['np', 'no problem'],
  ['tbd', 'to be determined'],
  ['tbc', 'to be confirmed'],
  ['wrt', 'with respect to'],
  ['mtg', 'meeting'],
  ['eod', 'end of day'],
  ['eow', 'end of week'],
];

function escapeRegExp(s) {
  return s.replace(/[.*+?^${}()|[\]\\/]/g, '\\$&');
}

function expandShorthand(line) {
  let out = line;
  for (const [token, expansion] of SHORTHAND) {
    const pattern = new RegExp(`(?<![a-zA-Z0-9])${escapeRegExp(token)}(?![a-zA-Z0-9])`, 'gi');
    out = out.replace(pattern, expansion);
  }
  return out;
}

function capitalizeAndPunctuate(line) {
  let out = line.trim().replace(/\s+/g, ' ');
  if (!out) return out;
  out = out[0].toUpperCase() + out.slice(1);
  if (!/[.!?:;,-]$/.test(out)) out += '.';
  return out;
}

function cleanLine(line) {
  return capitalizeAndPunctuate(expandShorthand(line));
}

// Returns { adf, preview } - adf is a Jira Atlassian Document Format comment
// body, preview is a plain-text rendering for toasts/change-log display.
function cleanupComment(raw) {
  const lines = String(raw || '')
    .split('\n')
    .map((l) => l.trim())
    .filter(Boolean)
    .map(cleanLine);

  if (lines.length === 0) {
    throw new Error('Comment text is empty');
  }

  if (lines.length === 1) {
    return {
      preview: lines[0],
      adf: {
        type: 'doc',
        version: 1,
        content: [{ type: 'paragraph', content: [{ type: 'text', text: lines[0] }] }],
      },
    };
  }

  return {
    preview: lines.map((l) => `- ${l}`).join('\n'),
    adf: {
      type: 'doc',
      version: 1,
      content: [
        {
          type: 'bulletList',
          content: lines.map((l) => ({
            type: 'listItem',
            content: [{ type: 'paragraph', content: [{ type: 'text', text: l }] }],
          })),
        },
      ],
    },
  };
}

module.exports = { cleanupComment };
