/* ==================================================================
 * gt-edits -- collect changed subfile inputs into one field.
 *
 * WHY THIS EXISTS. In an EJS Rich Display screen the subfile rows are rendered
 * by the TEMPLATE, not by Profound UI grid widgets. Every input in every row
 * therefore carries the same name with no record number attached, and the
 * runtime derives its {SUBFILE}.rrn changed-record marker from its own grid
 * widgets -- of which there are none. RPG's READC sees nothing, whatever the
 * operator did.
 *
 * So the changed rows travel in ONE field as "seq:value;seq:value;", the row
 * index encoded the same way this application already encodes it in its PICKnn
 * action codes. Only rows whose value differs from what the server rendered are
 * sent, so an unchanged save costs nothing and RPG does not stamp an updated-by
 * across untouched lines.
 *
 * Three screens use this now -- receiving quantities, count quantities and
 * count reason codes -- which is why it is here rather than copied into each.
 * Reference: design doc §34.7.
 * ================================================================== */

(function () {
  'use strict';

  /* Everything a caller can vary:
   *   root   -- the screen wrapper, so two screens on one page cannot cross
   *   box    -- the inputs to read
   *   number -- true rounds and fixes to 2dp; false sends the string as-is
   */
  function collect(opt) {
    var out = [];
    var boxes = document.querySelectorAll(opt.root + ' ' + opt.box);

    for (var i = 0; i < boxes.length; i++) {
      var b = boxes[i];
      var seq = b.getAttribute('data-gt-seq');
      var orig = b.getAttribute('data-gt-orig');
      if (!seq) continue;

      var now = b.value;
      /* An empty box is somebody mid-edit, not a zero. Sending 0 would
       * silently record nothing for that row. */
      if (now === '' || now === null) continue;

      if (opt.number) now = (Math.round(Number(now) * 100) / 100).toFixed(2);

      /* An explicit `force` attribute means "send this row even if the value
       * matches" -- a blind count needs it, because counting a line and
       * finding exactly what was showing is still a count. */
      if (now !== orig || b.getAttribute('data-gt-force') === '1') {
        out.push(seq + ':' + now);
      }
    }
    return out.length ? out.join(';') + ';' : '';
  }

  /* Set the named hidden field AND pass the value to pui.submit. Which one the
   * runtime reads depends on how the field was bound, so both are set -- the
   * same belt-and-braces the photo capture uses. */
  function submit(opt) {
    var payload = collect(opt);
    var hidden = document.getElementById(opt.hiddenId);
    if (hidden) hidden.value = payload;

    var fields = { action: opt.action };
    fields[opt.field] = payload;
    if (window.pui && typeof pui.submit === 'function') pui.submit(fields);
    return payload;
  }

  window.gtEdits = { __installed: true, collect: collect, submit: submit };
})();
