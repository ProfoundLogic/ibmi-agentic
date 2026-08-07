--  GT Warehouse Mobile -- capture test staging.
--
--  TEST FIXTURE. Not part of the application; nothing in GTMNUR, GTITMR or
--  GTITDR reads it.
--
--  It exists to solve one problem: proving the camera path from the outside.
--  The real payload arrives in a 24 KB display-file field from a browser, and
--  there is no way to type a 17,000-character base64 string at a green screen.
--  Passing it as a CL literal does not work either -- CL does not pad a
--  literal to the receiver's declared size, so a char(30000) parameter reads
--  30,000 bytes out of a 4,560-byte buffer and the program silently stores
--  nothing. That failure cost a full debugging round; hence this table.
--
--  So the payload is staged here with SQL, and GTIMGTST reads it and hands it
--  to the same GTIMG_ADD the screen calls. Everything downstream of that call
--  -- BASE64_DECODE, the journaled BLOB, the IFS publish, the view -- is then
--  exercised for real.

CREATE OR REPLACE TABLE GTIMGSTG (
  STG_ID     INTEGER       NOT NULL GENERATED ALWAYS AS IDENTITY
                           PRIMARY KEY,
  TARGET_SKU CHAR(15)      NOT NULL,
  PAYLOAD    VARCHAR(32000) NOT NULL,
  CREATED_AT TIMESTAMP     NOT NULL DEFAULT CURRENT TIMESTAMP
)
RCDFMT GTIMGSTGR;

CALL QSYS2.QCMDEXC('STRJRNPF FILE(*LIBL/GTIMGSTG) JRN(*LIBL/GTJRN) IMAGES(*BOTH)');
