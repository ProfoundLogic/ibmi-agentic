#!/usr/bin/env node
/*
 * GT Warehouse Mobile -- export image BLOBs to the static serving path.
 *
 *   node gtwms/tools/export-images.js [outdir]
 *
 * DB2 is the system of record: GTIMAGE holds every image as a journaled BLOB.
 * This reads them back out and writes them as files, because no /cgi-bin alias
 * on this server can be pointed at TIGERPOC (see design section 29) and we are
 * not putting our objects in somebody else's library to get one.
 *
 * The output directory is a CACHE, not a second source of truth. Delete it and
 * re-run this; nothing is lost. That is the same relationship gtseed.table.sql
 * has to its generator.
 *
 * Naming is by image_id so the URL is derivable from a single integer that the
 * RPG program already has:
 *     /profoundui/userdata/ui/gtimg/<image_id>.jpg      full
 *     /profoundui/userdata/ui/gtimg/<image_id>t.jpg     thumbnail
 *
 * Reads in pages: HEX() doubles the payload, so pulling 100+ images in one
 * result set is needlessly large.
 */

'use strict';

const fs = require('fs');
const path = require('path');
const { execFileSync } = require('child_process');

const OUT = process.argv[2] || '/tmp/gtimg-export';
/* Two rows per query. HEX() doubles the payload and the SQL tool truncates
 * its JSON output somewhere above ~140 KB, which surfaces as an unterminated
 * JSON string rather than as an error -- so keep each response small rather
 * than discover the ceiling in production. */
const PAGE = 2;

function sql(statement) {
  const input = JSON.stringify({ connection: 'dev', sql: statement });
  const tmp = '/tmp/.gtexp.json';
  fs.writeFileSync(tmp, input);
  const out = execFileSync('aitool', ['sql', '--input', '@' + tmp],
                           { maxBuffer: 256 * 1024 * 1024 }).toString();
  const parsed = JSON.parse(out);
  if (!parsed.success) {
    throw new Error('SQL failed: ' + JSON.stringify(parsed.error || parsed).slice(0, 300));
  }
  return (parsed.data && parsed.data.rows) || [];
}

fs.rmSync(OUT, { recursive: true, force: true });
fs.mkdirSync(OUT, { recursive: true });

const total = sql('SELECT COUNT(*) AS N FROM TIGERPOC.GTIMAGE')[0].N;
console.log('images in GTIMAGE:', total);

let written = 0;
let bytes = 0;

for (let offset = 0; offset < total; offset += PAGE) {
  const rows = sql(
    'SELECT IMAGE_ID, HEX(IMAGE_DATA) AS FULLHEX, HEX(THUMB_DATA) AS THUMBHEX ' +
    'FROM TIGERPOC.GTIMAGE ORDER BY IMAGE_ID ' +
    'OFFSET ' + offset + ' ROWS FETCH FIRST ' + PAGE + ' ROWS ONLY');

  for (const r of rows) {
    const id = r.IMAGE_ID;
    const full = Buffer.from(r.FULLHEX, 'hex');
    const thumb = Buffer.from(r.THUMBHEX, 'hex');

    /* A JPEG starts FF D8 FF. Anything else means the round trip through
     * HEX() and back has gone wrong, and it is far better to fail here than
     * to ship a directory of unreadable files. */
    if (full.length && full.slice(0, 3).toString('hex') !== 'ffd8ff') {
      throw new Error('image ' + id + ' is not a JPEG after decode');
    }

    fs.writeFileSync(path.join(OUT, id + '.jpg'), full);
    fs.writeFileSync(path.join(OUT, id + 't.jpg'), thumb);
    written += 2;
    bytes += full.length + thumb.length;
  }
  process.stdout.write('  exported ' + Math.min(offset + PAGE, total) + '/' + total + '\r');
}

console.log('\nfiles written :', written, 'in', OUT);
console.log('total bytes   :', Math.round(bytes / 1024), 'KB');
console.log('\nDeploy with:');
console.log('  scp ' + OUT + '/*.jpg dev:/home/drusso/puidist/htdocs/profoundui/userdata/ui/gtimg/');
