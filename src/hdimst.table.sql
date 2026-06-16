-- HDIMST - Item master (IM-prefix)
-- Updated 2026-06: original had IMITEM/IMDESC/IMUOM/IMWGT/IMSTS but the
-- actual Hornady program field names differ.  Corrected and extended based
-- on HYR0600 / PICKBATR / PICKERR references:
--   IMITEM   -> CHAR(15)    item number  (was already correct)
--   IMIMDS   -> CHAR(30)    item description  (was IMDESC)
--   IMIMWG   -> DECIMAL     item weight  (was IMWGT)
--   IMUOMS   -> CHAR(3)     unit-of-measure string  (was IMUOM)
--   IMPCLS   -> CHAR(3)     product class
--   IMUDN4   -> CHAR(30)    user-defined name 4
--   IMST     -> CHAR(1)     status  (was IMSTS)
CREATE TABLE HDIMST (
  IMITEM   CHAR(15)      NOT NULL DEFAULT '',
  IMIMDS   CHAR(30)      NOT NULL DEFAULT '',
  IMIMWG   DECIMAL(9,3)  NOT NULL DEFAULT 0,
  IMUOMS   CHAR(3)       NOT NULL DEFAULT '',
  IMPCLS   CHAR(3)       NOT NULL DEFAULT '',
  IMUDN4   CHAR(30)      NOT NULL DEFAULT '',
  IMST     CHAR(1)       NOT NULL DEFAULT '',
  PRIMARY KEY (IMITEM)
) RCDFMT HDRIMST;
