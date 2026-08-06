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
gtimgspk.file: qsqlsrc/gtimgspk.table.sql gttables.file

#  Generated product imagery, one target per department. Regenerate with
#  tools/gen-demo-images.js; the .sql files are committed so the build
#  never needs node or a browser.
gtimgappl.file: qsqlsrc/gtimgappl.table.sql gttables.file
gtimgclea.file: qsqlsrc/gtimgclea.table.sql gttables.file
gtimggroc.file: qsqlsrc/gtimggroc.table.sql gttables.file
gtimghlth.file: qsqlsrc/gtimghlth.table.sql gttables.file
gtimghome.file: qsqlsrc/gtimghome.table.sql gttables.file
gtimgpets.file: qsqlsrc/gtimgpets.table.sql gttables.file
gtimgseas.file: qsqlsrc/gtimgseas.table.sql gttables.file
gtimgtoys.file: qsqlsrc/gtimgtoys.table.sql gttables.file

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
gtitdr.pgm:  qrpglesrc/gtitdr.sqlrpgle qddssrc/gtitdd.json | gtitdd.file gtvimg.file
gtitmr.pgm:  qrpglesrc/gtitmr.sqlrpgle qrpglesrc/gtbar_pr.rpgle qddssrc/gtitmd.json \
             gtbar.srvpgm gtitdr.pgm | gtitmd.file gtbar.bnddir gtvimg.file
