/* ==================================================================
 * gt-cntvar -- the variance review's behaviour.
 *
 * Three jobs:
 *   1. Collect the reason codes into one field (gt-edits.js does the work --
 *      READC cannot see a template-rendered subfile's inputs).
 *   2. Warn the INSTANT a reason that demands a photograph is chosen, rather
 *      than letting the operator find out when the post is refused. The
 *      photo_required flag rides along on each <option>, so this is data from
 *      GTREASON rather than a rule written twice.
 *   3. Tell gt-photo.js which LINE a photograph belongs to before opening the
 *      camera, so the evidence attaches to the finding rather than to the item.
 *
 * Additive: without this file the selects still submit through Save reasons and
 * the post still works; only the early warning is lost.
 * ================================================================== */

(function () {
  'use strict';

  var EDITS = {
    root: '.gt-cntvar-screen', box: '.gt-cv-select', number: false,
    hiddenId: 'gt-vedits', field: 'vedits', action: 'APPLY'
  };

  var T = {
    EN: { needsPhoto: 'That reason needs a photograph before this count can post.',
          postBlocked: 'Some lines still need a photograph.' },
    FR: { needsPhoto: 'Ce motif exige une photo avant la validation.',
          postBlocked: 'Certaines lignes exigent encore une photo.' }
  };
  function t() {
    var el = document.querySelector('.gt-cntvar-screen');
    var v = el ? (el.getAttribute('data-gt-lang') || 'EN').toUpperCase() : 'EN';
    return T[v] || T.EN;
  }

  /* The chosen option carries its own photo_required flag. */
  function reasonChanged(sel) {
    var opt = sel.options[sel.selectedIndex];
    var needs = opt && opt.getAttribute('data-gt-photo') === 'Y';
    var row = sel.closest ? sel.closest('.gt-cv-row') : null;
    if (!row) return;

    var has = !!row.querySelector('.gt-cv-has');
    row.classList.toggle('is-blocked', needs && !has);

    var note = row.querySelector('.gt-cv-hint');
    if (needs && !has) {
      if (!note) {
        note = document.createElement('span');
        note.className = 'gt-cv-hint';
        var ph = row.querySelector('.gt-cv-photo');
        if (ph) ph.appendChild(note);
      }
      note.textContent = t().needsPhoto;
    } else if (note) {
      note.remove();
    }
  }

  /* gt-photo.js does not know about count lines, so the line number is set
   * here -- in the hidden field RPG reads AND on the element it submits. */
  function photo(lineNo) {
    var h = document.getElementById('gt-imgline');
    if (h) h.value = String(lineNo);
    if (window.gtPhoto) gtPhoto.open();
  }

  function save() {
    if (window.gtEdits) { gtEdits.submit(EDITS); return; }
    if (window.pui && typeof pui.submit === 'function') pui.submit({ action: 'APPLY' });
  }

  function post() {
    /* RPG refuses and explains anyway; this only saves the round trip. */
    if (document.querySelector('.gt-cntvar-screen .gt-cv-row.is-blocked')) {
      window.alert(t().postBlocked);
      return;
    }
    if (window.pui && typeof pui.submit === 'function') pui.submit({ action: 'POST' });
  }

  window.gtCntVar = {
    __installed: true,
    reasonChanged: reasonChanged,
    photo: photo,
    save: save,
    post: post,
    collect: function () { return window.gtEdits ? gtEdits.collect(EDITS) : ''; }
  };
})();
