#!/usr/bin/env node
/* Turn a live RDF screen captured by the ibmi-interactive-session skill into a
   render fixture (real field values and subfile rows from the IBM i).

   node prfdemolib/web/tools/fixture.js /tmp/genie-sessions/S/history/screen-004.json out.json */
'use strict';
const fs = require('fs');
const s = JSON.parse(fs.readFileSync(process.argv[2], 'utf8'));
const layer = s.layers[s.layers.length - 1];
const f = layer.formats.find((x) => x.active) || layer.formats[0];
const scr = f.metaData.screen;
const out = { format: f.name, template: scr['ejs template'], css: scr['ejs css'] || [], js: scr['ejs js'] || [], data: {}, subfiles: {} };
for (const [k, v] of Object.entries(f.data || {})) if (!/^QPUI/.test(k)) out.data[k] = v;
for (const [k, v] of Object.entries(f.subfiles || {})) out.subfiles[k] = { fields: v['field names'], rows: v.data };
fs.writeFileSync(process.argv[3], JSON.stringify(out, null, 1));
console.log(`${out.format}: ${Object.keys(out.data).length} fields, ` +
  Object.entries(out.subfiles).map(([k, v]) => `${k} ${v.rows.length} rows`).join(', '));
