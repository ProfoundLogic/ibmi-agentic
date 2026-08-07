# =====================================================================
#  GT Warehouse Mobile -- Giant Tiger warehouse floor POC
#
#  Build target library is TIGERPOC. Pass it as a per-invocation override:
#      IBMI_BUILD_LIBRARY=TIGERPOC codermake <target>
#
#  Never run a bare `codermake` with no target from this repo -- it would
#  build every other project's objects into TIGERPOC as well, including
#  empty copies of populated data files.
# =====================================================================

# ---------------------------------------------------------------------
#  Database -- journals first, then tables.
#
#  TIGERPOC is a plain library with no QSQJRN, so tables are not journaled
#  automatically and DB2 refuses DML on an unjournaled table (SQL7008).
#  gtjrn.file must therefore build before gttables.file, and gttables.sql
#  starts journalling on each table as it creates it.
# ---------------------------------------------------------------------
gtjrn.file:     qsqlsrc/gtjrn.table.sql
gttables.file:  qsqlsrc/gttables.table.sql gtjrn.file
gtindexes.file: qsqlsrc/gtindexes.index.sql gttables.file
gtviews.file:   qsqlsrc/gtviews.view.sql gttables.file

#  Demo data. gtseed.table.sql is GENERATED -- edit tools/gen-demo-data.js and
#  re-run it, never the .sql. It is committed so the build does not depend on
#  node being present.
gtseed.file:    qsqlsrc/gtseed.table.sql gtviews.file gtindexes.file

#  Everything the database needs, in order.
.PHONY: gtdb
gtdb: gtseed.file

# ---------------------------------------------------------------------
#  Wave 0 spike -- commitment control proof. Not part of the application.
# ---------------------------------------------------------------------
gtcmtst.pgm: qrpglesrc/gtcmtst.sqlrpgle | gttables.file

# ---------------------------------------------------------------------
#  Application -- main menu and entry point
#
#  HOW THE OPERATOR REACHES THIS APP, and the one chore it costs per task:
#
#  Sign-on resolves its initial menu through *LIBL/MENU, and PUISETENV puts the
#  CoderFlow TASK library ahead of AIDEMOBASE. So the sign-on menu carrying
#  "4. GT Warehouse Mobile" is a shadow MENU built from ../cfdemo into the TASK
#  library -- NOT into TIGERPOC, which only joins the library list later, when
#  GTSTART does its ADDLIBLE. Option 4 runs `call tigerpoc/gtstart`, fully
#  qualified, so it works whatever the library list holds.
#
#  The task library changes every task, so rebuild the shadow each time -- and
#  delete the stamps first, because codermake stamps record THAT a target was
#  built, not WHICH library it went to, so a stale stamp makes this a silent
#  no-op and option 4 just isn't there:
#
#      rm -f build/menu.file build/menu.msgf build/menu.menu
#      codermake menu.menu        # default IBMI_BUILD_LIBRARY = task library
#
#  Verify by signing on and reading the menu. See §25.3 of the design doc.
# ---------------------------------------------------------------------
gtmnud.file: qddssrc/gtmnud.json
gtmnur.pgm:  qrpglesrc/gtmnur.sqlrpgle qddssrc/gtmnud.json | gtmnud.file gttables.file
gtstart.pgm: qclsrc/gtstart.clle | gtmnur.pgm

#  ---------------------------------------------------------------------
#  WEB ASSETS -- codermake does not deploy htdocs. Two steps, and BOTH are
#  required after editing any .ejs or .css:
#
#    1. Copy the asset to the Profound UI document root:
#         scp htdocs/profoundui/userdata/ui/gtmnud/* \
#             dev:/home/drusso/puidist/htdocs/profoundui/userdata/ui/gtmnud/
#
#    2. Regenerate and redeploy the Genie shim, for EVERY skin:
#         gtwms/tools/deploy-shim.sh              # pls and Classic
#         gtwms/tools/deploy-shim.sh Skyline      # or a named skin
#
#       Profound UI fetches the EJS template over XHR and that request does not
#       reach the browser here, so the template is snapshotted into each Genie
#       skin's start.html. deploy-shim.sh backs up, regenerates, syntax-checks
#       the JavaScript, uploads, and verifies against the deployed file.
#
#  STEP 2 IS NOT OPTIONAL. The shim holds a COPY of the template, so step 1
#  alone changes nothing on screen -- the browser keeps getting the old
#  snapshot. Each skin holds its OWN copy, so testing under one skin proves
#  nothing about another. `test-gt-shim.js` checks all deployed skins.
#  ---------------------------------------------------------------------

# ---------------------------------------------------------------------
#  GTBAR -- barcode identification and GS1 parsing service program.
#  Every scan in the application goes through it.
# ---------------------------------------------------------------------
gtbar.module: qrpglesrc/gtbar.sqlrpgle qrpglesrc/gtbar_pr.rpgle | gttables.file
gtbar.srvpgm: gtbar.module qsrvsrc/gtbar.bnd
gtbar.bnddir: gtbar.bnddir | gtbar.srvpgm

#  Unit test for the barcode engine. Not part of the application.
gtbartst.file: qsqlsrc/gtbartst.table.sql gttables.file
gtbartst.pgm:  qrpglesrc/gtbartst.sqlrpgle qrpglesrc/gtbar_pr.rpgle gtbar.srvpgm | gtbar.bnddir gtbartst.file

# ---------------------------------------------------------------------
#  Scan Lab -- identify any barcode. Proves gt-scan, which every other
#  screen depends on.
# ---------------------------------------------------------------------
gtscnd.file: qddssrc/gtscnd.json
gtscnr.pgm:  qrpglesrc/gtscnr.sqlrpgle qrpglesrc/gtbar_pr.rpgle qddssrc/gtscnd.json gtbar.srvpgm | gtscnd.file gtbar.bnddir gttables.file

# ---------------------------------------------------------------------
#  Image pipeline.
#
#  NO CGI. Every /cgi-bin ScriptAlias on this server is hard-wired to a
#  specific library (drpuidev -> DRUSSO, puidev -> PUICGI, ...) and none can
#  point at TIGERPOC without editing a shared httpd.conf. Rather than put our
#  objects in someone else's library, images are served as static files.
#
#  DB2 remains the system of record: GTIMAGE holds the BLOBs, they are
#  journaled, and they travel with a database save. The files under
#  htdocs/.../gtimg/ are a GENERATED CACHE, exported from the BLOBs by
#  tools/export-images.js and safe to delete at any time.
# ---------------------------------------------------------------------
#  Generated product imagery, one target per department. Regenerate with
#  tools/gen-demo-images.js; the .sql files are committed so the build
#  never needs node or a browser.
#
#  THESE DEPEND ON gtseed.file, NOT ON gttables.file. gtseed.table.sql opens
#  with DELETE FROM GTIMAGE, so if the imagery loads first the seed wipes it
#  and the carousel goes blank on every item. It is invisible on an
#  incremental build -- the stamps are already current and neither target
#  re-runs -- and only bites on a fresh clone or a fresh container, which is
#  exactly when nobody is looking for it.
gtimgappl.file: qsqlsrc/gtimgappl.table.sql gtseed.file
gtimgclea.file: qsqlsrc/gtimgclea.table.sql gtseed.file
gtimggroc.file: qsqlsrc/gtimggroc.table.sql gtseed.file
gtimghlth.file: qsqlsrc/gtimghlth.table.sql gtseed.file
gtimghome.file: qsqlsrc/gtimghome.table.sql gtseed.file
gtimgpets.file: qsqlsrc/gtimgpets.table.sql gtseed.file
gtimgseas.file: qsqlsrc/gtimgseas.table.sql gtseed.file
gtimgtoys.file: qsqlsrc/gtimgtoys.table.sql gtseed.file

.PHONY: gtimages
gtimages: gtimgappl.file gtimgclea.file gtimggroc.file gtimghlth.file gtimghome.file gtimgpets.file gtimgseas.file gtimgtoys.file

#  Item-to-image mapping. Depends on the real image targets, NOT the .PHONY
#  alias -- codermake tries to ship a prerequisite as a file, so a phony one
#  fails with "tar: gtimages: Cannot stat".
gtvimg.file: qsqlsrc/gtvimg.view.sql gtimgappl.file gtimgclea.file gtimggroc.file \
             gtimghlth.file gtimghome.file gtimgpets.file gtimgseas.file gtimgtoys.file

# ---------------------------------------------------------------------
#  Item Lookup -- the demo's opener and the brand showpiece.
# ---------------------------------------------------------------------
#  ONE FORMAT PER DISPLAY FILE, and a called program for the next screen.
#  Two formats in one display file both stay rendered at once, so the second
#  covers the first and only F-keys reach the program. This is the pattern the
#  existing cfdemo screens already use.
gtitmd.file: qddssrc/gtitmd.json
gtitdd.file: qddssrc/gtitdd.json

#  The detail screen also takes camera photos, so it binds GTIMG. Its display
#  file carries a char(24000) IMGDATA field -- the payload channel, since no
#  upload endpoint is reachable from TIGERPOC.
gtitdr.pgm:  qrpglesrc/gtitdr.sqlrpgle qrpglesrc/gtimg_pr.rpgle qddssrc/gtitdd.json \
             gtimg.srvpgm | gtitdd.file gtimg.bnddir gtvimg.file
gtitmr.pgm:  qrpglesrc/gtitmr.sqlrpgle qrpglesrc/gtbar_pr.rpgle qddssrc/gtitmd.json \
             gtbar.srvpgm gtitdr.pgm | gtitmd.file gtbar.bnddir gtvimg.file

# ---------------------------------------------------------------------
#  GTIMG -- camera capture. Stores the BLOB and writes the servable file
#  itself, so a photo appears the instant it is taken.
# ---------------------------------------------------------------------
gtimg.module: qrpglesrc/gtimg.sqlrpgle qrpglesrc/gtimg_pr.rpgle | gttables.file
gtimg.srvpgm: gtimg.module qsrvsrc/gtimg.bnd
gtimg.bnddir: gtimg.bnddir | gtimg.srvpgm

#  Proves decode -> BLOB -> IFS -> HTTP before any screen depends on it.
#  Test fixtures, not application objects. GTIMGSTG is where the base64 is
#  staged, because 17,000 characters cannot be typed at a green screen and a
#  CL literal is not padded to the parameter's declared size.
gtimgstg.file: qsqlsrc/gtimgstg.table.sql gttables.file
gtimgtst.pgm:  qrpglesrc/gtimgtst.sqlrpgle qrpglesrc/gtimg_pr.rpgle gtimg.srvpgm \
               | gtimg.bnddir gtimgstg.file

# ---------------------------------------------------------------------
#  Receiving -- the flagship application. Four screens, four display files,
#  four programs, per the one-format-per-display-file rule (design doc §32).
#
#  Views first: GTVRCVLIN and GTVRCVOPEN derive everything the screens show,
#  including the line thumbnail, so the subfile needs no per-row query.
# ---------------------------------------------------------------------
gtvrcv.file: qsqlsrc/gtvrcv.view.sql gtvimg.file gtseed.file

gtrchd.file: qddssrc/gtrchd.json
gtrcld.file: qddssrc/gtrcld.json

#  RCVLINES is the scan-to-confirm subfile; it binds GTBAR to match a scanned
#  carton against a line on the receipt.
gtrclr.pgm:  qrpglesrc/gtrclr.sqlrpgle qrpglesrc/gtbar_pr.rpgle qddssrc/gtrcld.json \
             gtbar.srvpgm | gtrcld.file gtbar.bnddir gtvrcv.file

#  RCVHOME scans the pallet label and calls RCVLINES.
gtrchr.pgm:  qrpglesrc/gtrchr.sqlrpgle qrpglesrc/gtbar_pr.rpgle qddssrc/gtrchd.json \
             gtbar.srvpgm gtrclr.pgm | gtrchd.file gtbar.bnddir gtvrcv.file
