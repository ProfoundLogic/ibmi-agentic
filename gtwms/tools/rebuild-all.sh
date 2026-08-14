#!/usr/bin/env bash
#
# GT Warehouse Mobile -- rebuild the whole application, in the order that works.
#
# WHY THIS SCRIPT EXISTS
# ----------------------
# docs/BUILD-PLAYBOOK.md §2 records the build order in prose, and prose is not
# executable. Every ordering hazard below has cost real time at least once:
#
#   1. gtjrn.file FIRST. A codermake-created library has no QSQJRN, so SQL
#      tables are not auto-journaled and DB2 refuses DML under commitment
#      control (SQL7008). SQL7905 at CREATE time is expected.
#   2. tables -> indexes -> views -> gtseed.file.
#   3. IMAGERY AFTER gtseed.file. gtseed.table.sql opens with
#      DELETE FROM GTIMAGE, so loading imagery first silently wipes it. This is
#      invisible on an incremental build and only bites on a full one -- which
#      is exactly what this script does.
#   4. service programs -> display files -> programs.
#   5. the MENU launcher (rebuild-menu.sh, both libraries).
#   6. htdocs assets -- codermake does NOT deploy these, and the shim carries a
#      COPY of every template, so an scp without a re-shim changes nothing.
#
# THE STAMP TRAP. build/<target> records THAT a target built, not WHICH library
# it went to. So a rebuild after the target library changes is a silent no-op
# unless the stamps go first. Only the gt* stamps are removed, NEVER the whole
# directory: build/custp.file and friends are order-only prerequisites of the
# other projects in this repo, and recreating those CRTPFs an EMPTY copy over
# populated cfdemo demo data.
#
# WHAT --full DESTROYS. gttables.table.sql drops and recreates every table, and
# gtseed regenerates the demo data deterministically. Anything captured on a
# device since the last rebuild -- operator photographs, posted receipts, moves,
# counts -- is gone. That is the intent of a full rebuild, not a surprise, but
# it is the reason it is not the default.
#
# Usage:
#   gtwms/tools/rebuild-all.sh                 objects only, data left intact
#   gtwms/tools/rebuild-all.sh --full          + drop/recreate/reseed the database
#   gtwms/tools/rebuild-all.sh --no-tests      skip the offline test suite
#   gtwms/tools/rebuild-all.sh --no-deploy     skip htdocs + shim
#
set -uo pipefail

cd "$(dirname "$0")/../.."

APPLIB="TIGERPOC"
FULL=0 TESTS=1 DEPLOY=1
for arg in "$@"; do
  case "$arg" in
    --full)      FULL=1 ;;
    --no-tests)  TESTS=0 ;;
    --no-deploy) DEPLOY=0 ;;
    *) echo "unknown option: $arg" >&2; exit 2 ;;
  esac
done

#  NOT under tmp/logs -- codermake clears that directory at the start of every
#  invocation, which deletes this script's own log as it runs.
LOGDIR=tmp/rebuild-logs
mkdir -p "$LOGDIR"
FAILED=()

# ---------------------------------------------------------------------
#  One codermake target, into APPLIB, stamp cleared first so it really runs.
# ---------------------------------------------------------------------
build() {
  local target="$1" t0 rc
  mkdir -p "$LOGDIR"
  rm -f "build/$target"
  printf '  %-16s ' "$target"
  t0=$SECONDS
  if IBMI_BUILD_LIBRARY="$APPLIB" codermake "$target" > "$LOGDIR/$target.log" 2>&1; then
    printf 'ok    %3ds\n' "$((SECONDS - t0))"
  else
    rc=$?
    printf 'FAIL  %3ds   %s\n' "$((SECONDS - t0))" "$LOGDIR/$target.log"
    FAILED+=("$target")
    return $rc
  fi
}

# Build one target after clearing the stamps of everything underneath it too --
# a .PHONY alias has no stamp of its own, and a .bnddir stamp says nothing about
# the .module and .srvpgm it was built from.
build_group() {
  local group="$1"; shift
  local m
  mkdir -p "$LOGDIR"
  for m in "$@"; do rm -f "build/$m"; done
  printf '  %-16s ' "$group"
  local t0=$SECONDS
  if IBMI_BUILD_LIBRARY="$APPLIB" codermake "$group" > "$LOGDIR/$group.log" 2>&1; then
    printf 'ok    %3ds\n' "$((SECONDS - t0))"
  else
    printf 'FAIL  %3ds   %s\n' "$((SECONDS - t0))" "$LOGDIR/$group.log"
    FAILED+=("$group")
    return 1
  fi
}

stage() { echo; echo "=== $* ==="; }

# ---------------------------------------------------------------------
#  Export the image BLOBs to the static serving path.
#
#  WHY THIS IS PART OF THE BUILD AND NOT AN AFTERTHOUGHT. GTIMAGE.image_id is
#  GENERATED ALWAYS AS IDENTITY and the table is dropped and recreated by
#  --full, so every imagery reload issues a NEW BLOCK OF IDS -- 103..204 one
#  generation, 205..306 the next. Every template builds its URL from that
#  integer:
#
#      <img src="/profoundui/userdata/ui/gtimg/<image_id>.jpg">
#
#  so a rebuild that skips this step leaves the server holding the PREVIOUS
#  generation's filenames. Result: a 404 for every image on every screen that
#  shows one, and nothing reports it -- not the build, not the shim test, not
#  the pre-flights, which serve their own fixtures. It is only visible by
#  looking at a screen. That is exactly how it was found.
#
#  The directory is a cache, not a source of truth: DB2 holds the journaled
#  BLOBs. So it is emptied rather than merged, otherwise every generation's
#  dead files accumulate there forever.
# ---------------------------------------------------------------------
IMGDIR=/home/drusso/puidist/htdocs/profoundui/userdata/ui/gtimg

export_images() {
  local work=/tmp/gtimg-export expected actual
  mkdir -p "$LOGDIR"

  printf '  %-16s ' "export BLOBs"
  if ! node gtwms/tools/export-images.js "$work" > "$LOGDIR/export-images.log" 2>&1; then
    echo "FAIL   $LOGDIR/export-images.log"; return 1
  fi
  expected=$(ls "$work"/*.jpg 2>/dev/null | wc -l)
  echo "ok    $expected files"

  printf '  %-16s ' "upload"
  ssh dev "mkdir -p '$IMGDIR' && rm -f '$IMGDIR'/*.jpg" >/dev/null 2>&1
  if ! scp -q "$work"/*.jpg "dev:$IMGDIR/"; then echo "FAIL"; return 1; fi
  actual=$(ssh dev "ls '$IMGDIR'/*.jpg 2>/dev/null | wc -l" | tr -d ' ')
  if [ "$actual" = "$expected" ]; then
    echo "ok    $actual on the server"
  else
    echo "FAIL  $actual on the server, expected $expected"; return 1
  fi

  #  Prove one CURRENT id actually serves. Counting files would pass happily
  #  with a directory full of the wrong generation's names.
  printf '  %-16s ' "serves"
  local id
  id=$(node -e "
    const {execFileSync}=require('child_process'),fs=require('fs');
    fs.writeFileSync('/tmp/.gtid.json', JSON.stringify({connection:'dev',
      sql:'select min(image_id) as lo from tigerpoc.gtimage'}));
    const o=JSON.parse(execFileSync('aitool',['sql','--input','@/tmp/.gtid.json']).toString());
    process.stdout.write(String(o.data.rows[0].LO));
  " 2>/dev/null)
  if ssh dev "test -s '$IMGDIR/$id.jpg' && test -s '$IMGDIR/${id}t.jpg'"; then
    echo "ok    image_id $id and its thumbnail are present"
  else
    echo "FAIL  image_id $id is not on the server"; return 1
  fi
}

# ---------------------------------------------------------------------
#  1-3. Database. Destructive, so opt-in.
# ---------------------------------------------------------------------
if [ "$FULL" = 1 ]; then
  stage "1. journal (before any table -- SQL7008)"
  build gtjrn.file || exit 1

  stage "2. tables -> indexes -> views -> seed"
  build gttables.file  || exit 1
  build gtindexes.file || exit 1
  build gtviews.file   || exit 1
  build gtvrcv.file    || exit 1
  build gtvinv.file    || exit 1
  build gtvcnt.file    || exit 1
  build gtseed.file    || exit 1

  stage "3. imagery (AFTER the seed -- it opens with DELETE FROM GTIMAGE)"
  build_group gtimages gtimgappl.file gtimgclea.file gtimggroc.file \
              gtimghlth.file gtimghome.file gtimgpets.file \
              gtimgseas.file gtimgtoys.file || exit 1
  build gtvimg.file   || exit 1
  build gtimgstg.file || exit 1
  build gtvput.file   || exit 1

else
  stage "1-3. database SKIPPED (--full to drop, recreate and reseed)"
fi

# ---------------------------------------------------------------------
#  4. Service programs, then display files, then programs.
# ---------------------------------------------------------------------
stage "4a. service programs (module -> srvpgm -> bnddir)"
build_group gtbar.bnddir gtbar.module gtbar.srvpgm gtbar.bnddir || exit 1
build_group gtimg.bnddir gtimg.module gtimg.srvpgm gtimg.bnddir || exit 1

stage "4b. display files"
for f in gtmnud gtscnd gtitmd gtitdd gtrchd gtrcld gtinhd gtinld gtimvd \
         gtcnhd gtcned gtcnvd gtsvd gtpud gtpdd gtstd gtbartst; do
  build "$f.file" || exit 1
done

stage "4c. programs"
#  gtitmr calls gtitdr, gtrchr calls gtrclr, gtinhr calls gtinlr/gtimvr,
#  gtcnhr calls gtcner/gtcnvr -- callees first so a bind never chases a
#  program that is not there yet.
for p in gtstart gtitdr gtitmr gtscnr gtrclr gtrchr gtinlr gtimvr gtinhr \
         gtcner gtcnvr gtcnhr gtsvr gtpdr gtpur gtstr gtmnur; do
  build "$p.pgm" || exit 1
done

stage "4d. test fixtures (not application objects)"
build gtbartst.pgm || true
build gtimgtst.pgm || true
build gtcmtst.pgm  || true

# ---------------------------------------------------------------------
#  5. The launcher, into both libraries.
# ---------------------------------------------------------------------
stage "5. sign-on menu launcher"
gtwms/tools/rebuild-menu.sh || FAILED+=(menu)

# ---------------------------------------------------------------------
#  6. htdocs + the shim. codermake does neither.
# ---------------------------------------------------------------------
if [ "$DEPLOY" = 1 ]; then
  #  UNCONDITIONAL, not just under --full.
  #
  #  It looked like a --full-only concern -- only a reseed reloads the imagery,
  #  surely. It is not. gtseed.file lists gtviews.file as a NORMAL prerequisite
  #  and the eight imagery targets list gtseed.file, so rebuilding ANY of the
  #  shared views cascades into a reseed and a full imagery reload, and every
  #  image_id moves. That is how the ids went 205..306 -> 307..408 on a build
  #  whose only source change was one column added to a view.
  #
  #  Cheap (about 100 SQL reads) and idempotent, so it runs every time rather
  #  than depending on anyone correctly predicting the cascade.
  stage "6a. image BLOBs -> static files (any view rebuild can move the ids)"
  export_images || FAILED+=(export-images)

  stage "6b. htdocs assets -> Profound UI docroot"
  #  Delegated to deploy-assets.sh so that the standalone-file upload has exactly
  #  ONE implementation. It used to live inline here, which meant the documented
  #  "just re-run deploy-shim.sh" shortcut silently skipped it.
  gtwms/tools/deploy-assets.sh || FAILED+=(deploy-assets)

  stage "6c. the shim -- EVERY skin (each holds its own template copy)"
  gtwms/tools/deploy-shim.sh || FAILED+=(shim)
else
  stage "6. htdocs + shim SKIPPED"
fi

# ---------------------------------------------------------------------
#  Offline verification. Cheap, and each catches what the next cannot.
# ---------------------------------------------------------------------
if [ "$TESTS" = 1 ]; then
  stage "7. offline tests"
  [ -d node_modules/ejs ] || npm i --no-save --silent ejs playwright-core
  #  test-images LAST and always: it is the only check that fetches from the
  #  server the operator's browser fetches from. The pre-flights serve their own
  #  fixtures, so their <img> tags resolve against something that is not real.
  for t in preflight-item preflight-scnhome preflight-rcv preflight-inv \
           preflight-cnt preflight-supv preflight-put preflight-sett \
           test-photo-capture test-rcv-stepper test-gt-shim \
           test-images; do
    printf '  %-20s ' "$t"
    if node "gtwms/tools/$t.js" > "$LOGDIR/$t.log" 2>&1; then
      echo "ok"
    else
      echo "FAIL   $LOGDIR/$t.log"; FAILED+=("$t")
    fi
  done
fi

# ---------------------------------------------------------------------
echo
if [ ${#FAILED[@]} -eq 0 ]; then
  echo "=== rebuild complete: everything ok ==="
else
  echo "=== rebuild finished with ${#FAILED[@]} failure(s): ${FAILED[*]} ==="
  exit 1
fi
