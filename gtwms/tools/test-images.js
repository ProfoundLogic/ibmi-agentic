#!/usr/bin/env node
/*
 * GT Warehouse Mobile -- do the images the screens ask for actually serve?
 *
 *   node gtwms/tools/test-images.js
 *
 * WHY THIS EXISTS
 * ---------------
 * Every screen builds its image URL from an integer the RPG program already
 * has:
 *
 *     /profoundui/userdata/ui/gtimg/<image_id>.jpg
 *
 * and GTIMAGE.image_id is GENERATED ALWAYS AS IDENTITY. A --full rebuild drops
 * and recreates the table, so the reload issues a NEW block of ids -- one
 * generation ran 103..204, the next 205..306. The exported files on the server
 * are named for whichever generation exported them.
 *
 * When those two fall out of step EVERY image on EVERY screen 404s, and nothing
 * in the build reports it:
 *
 *   - codermake succeeds: the objects are all fine.
 *   - test-gt-shim passes: the templates are byte-perfect.
 *   - the pre-flights pass: they serve their own local fixtures, so their
 *     <img> tags resolve against something that is not the real server.
 *
 * It is visible only by looking at a screen, which is how it was found -- by
 * the user, after a rebuild that reported "everything ok". This test closes
 * that hole: it asks DB2 for the ids the screens will really ask for, and
 * fetches them over HTTP from the server that really serves them.
 *
 * THE NEGATIVE CASE IS NOT OPTIONAL. A server misconfigured to answer 200 with
 * an error page for everything would sail through a positive-only check, and
 * that is precisely the failure this test is meant to catch. So it also asserts
 * that a deliberately absent id 404s. A check you have not seen fail proves
 * nothing.
 */

'use strict';

const { execFileSync } = require('child_process');
const fs = require('fs');

const BASE = 'https://idev.profoundlogic.com:8103/profoundui/userdata/ui/gtimg';

let pass = 0, fail = 0;
const ok  = (m, d) => { pass++; console.log('  PASS  ' + m + (d ? '  -- ' + d : '')); };
const bad = (m, d) => { fail++; console.log('  FAIL  ' + m + (d ? '  -- ' + d : '')); };

function sql(statement) {
  fs.writeFileSync('/tmp/.gtimgtest.json',
    JSON.stringify({ connection: 'dev', sql: statement }));
  const out = execFileSync('aitool', ['sql', '--input', '@/tmp/.gtimgtest.json'],
                           { maxBuffer: 64 * 1024 * 1024 }).toString();
  const parsed = JSON.parse(out);
  if (!parsed.success) throw new Error('SQL failed: ' + JSON.stringify(parsed.error).slice(0, 200));
  return (parsed.data && parsed.data.rows) || [];
}

/* curl rather than node https: the dev box serves a self-signed certificate and
 * -k keeps this a test of the image, not of the trust store. */
function fetchHead(url) {
  try {
    const out = execFileSync('curl',
      ['-s', '-o', '/tmp/.gtimg.bin', '-w', '%{http_code} %{size_download}', '-k', url],
      { timeout: 30000 }).toString().trim().split(/\s+/);
    return { code: Number(out[0]), size: Number(out[1]) };
  } catch (e) {
    return { code: 0, size: 0 };
  }
}

function isJpeg(file) {
  try {
    const fd = fs.openSync(file, 'r');
    const b = Buffer.alloc(3);
    fs.readSync(fd, b, 0, 3, 0);
    fs.closeSync(fd);
    return b.toString('hex') === 'ffd8ff';
  } catch (e) { return false; }
}

console.log('=== images the screens will ask for ===');

/* Sample the ends and the middle rather than all 102: an id block goes stale
 * as a whole, so three points across it finds it, and 102 HTTPS round trips
 * to prove the same thing is just slower. */
const ids = sql(
  'WITH n AS (SELECT image_id, ROW_NUMBER() OVER (ORDER BY image_id) AS rn, ' +
  '                  COUNT(*) OVER () AS tot FROM TIGERPOC.GTIMAGE) ' +
  'SELECT image_id FROM n WHERE rn IN (1, tot / 2, tot) ORDER BY image_id'
).map(r => r.IMAGE_ID);

const total = sql('SELECT COUNT(*) AS N FROM TIGERPOC.GTIMAGE')[0].N;
console.log(`  GTIMAGE holds ${total} images, ids ${ids[0]}..${ids[ids.length - 1]}`);

if (!ids.length) bad('GTIMAGE is empty', 'nothing to serve -- was the seed run after the imagery?');

for (const id of ids) {
  for (const name of [id + '.jpg', id + 't.jpg']) {
    const r = fetchHead(BASE + '/' + name);
    if (r.code !== 200)          bad(name + ' serves', 'HTTP ' + (r.code || 'no response'));
    else if (r.size < 500)       bad(name + ' serves', 'HTTP 200 but only ' + r.size + ' bytes');
    else if (!isJpeg('/tmp/.gtimg.bin')) bad(name + ' serves', '200 but not a JPEG');
    else                         ok(name + ' serves', r.size + ' bytes');
  }
}

/* The whole point: an id that cannot exist must NOT answer 200. */
const ghost = (ids[ids.length - 1] || 0) + 100000;
const g = fetchHead(BASE + '/' + ghost + '.jpg');
if (g.code === 404) ok('an absent id 404s', 'id ' + ghost + ' -- so a 200 above means something');
else bad('an absent id 404s',
         'id ' + ghost + ' answered HTTP ' + g.code + ' -- every check above is meaningless');

console.log(`\n${fail ? 'FAILED' : 'All checks passed'}: ${pass} passed, ${fail} failed`);
if (fail) {
  console.log('\nIf the ids 404: the exported files are a different generation than');
  console.log('GTIMAGE. Re-export and upload:');
  console.log('  node gtwms/tools/export-images.js /tmp/gtimg-export');
  console.log('  scp /tmp/gtimg-export/*.jpg dev:' +
              '/home/drusso/puidist/htdocs/profoundui/userdata/ui/gtimg/');
  process.exit(1);
}
