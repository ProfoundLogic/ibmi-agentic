'use strict';

// Flattens a Jira ADF (Atlassian Document Format) document to plain text.
//
// Comments are flattened server-side rather than shipped to the browser as
// ADF, for three reasons: the board only needs to *read* them, plain text is
// a fraction of the size in data/board-state.json (which is committed), and
// text that is escaped on render can't carry markup into the page.
//
// Nodes we can't render meaningfully become a visible placeholder rather than
// vanishing - a comment that is just a screenshot should not look empty.

function textOf(node) {
  if (!node || typeof node !== 'object') return '';
  const kids = () => (node.content || []).map(textOf).join('');

  switch (node.type) {
    case 'doc':
      return (node.content || []).map(textOf).join('\n').replace(/\n{3,}/g, '\n\n').trim();

    case 'paragraph':
    case 'heading':
      return kids() + '\n';

    case 'text': {
      const link = (node.marks || []).find((m) => m.type === 'link');
      const t = node.text || '';
      // Only append the href when it isn't already the visible text.
      if (link && link.attrs && link.attrs.href && link.attrs.href !== t) {
        return `${t} <${link.attrs.href}>`;
      }
      return t;
    }

    case 'hardBreak':
      return '\n';

    case 'bulletList':
    case 'orderedList':
      return (node.content || [])
        .map((li, i) => {
          const marker = node.type === 'orderedList' ? `${i + 1}. ` : '- ';
          return marker + textOf(li).trim().replace(/\n/g, '\n  ');
        })
        .join('\n') + '\n';

    case 'taskList':
      return (node.content || [])
        .map((ti) => {
          const done = ti.attrs && ti.attrs.state === 'DONE';
          return `[${done ? 'x' : ' '}] ` + textOf(ti).trim();
        })
        .join('\n') + '\n';

    case 'listItem':
    case 'taskItem':
    case 'blockquote':
    case 'tableCell':
    case 'tableHeader':
      return kids();

    case 'tableRow':
      return (node.content || []).map((c) => textOf(c).trim()).join(' | ') + '\n';

    case 'table':
      return kids();

    case 'codeBlock':
      return kids() + '\n';

    case 'rule':
      return '---\n';

    case 'mention':
      return '@' + ((node.attrs && node.attrs.text) || 'unknown').replace(/^@/, '');

    case 'emoji':
      return (node.attrs && (node.attrs.text || node.attrs.shortName)) || '';

    case 'inlineCard':
    case 'blockCard':
      return (node.attrs && node.attrs.url) || '[card]';

    case 'media':
      return `[attachment: ${(node.attrs && node.attrs.alt) || (node.attrs && node.attrs.type) || 'file'}]`;

    case 'mediaSingle':
    case 'mediaGroup':
      return kids() + '\n';

    default:
      // Unknown block: keep any text inside it rather than dropping it.
      return kids();
  }
}

function adfToText(doc) {
  if (!doc) return '';
  if (typeof doc === 'string') return doc; // very old comments can be plain strings
  try {
    return textOf(doc);
  } catch {
    return '[comment could not be rendered]';
  }
}

module.exports = { adfToText };
