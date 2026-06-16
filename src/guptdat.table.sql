-- GUPTDAT - Generic table data (key/value lookup, TD-prefix fields)
-- Original CHAR(30)/CHAR(60) GUKEY/GUVAL shape did not match the actual
-- Hornady program references.  Both HYR0600 and PICKBATR use the chain
-- pattern:
--   chain ('SHIPUSER':userId) GUPTDAT  -->  TDDESC = user's full name
--   chain ('SHIPVIA':code)    GUPTDAT  -->  TDDESC = ship-via description
-- HYPTDTA shares the same schema and is used the same way; both
-- "table-data" files together cover Hornady's generic enumeration / lookup
-- mechanism.  TDTABL is the table-name selector, TDKEY1..TDKEY3 are the
-- composite key, TDDESC is the rendered description.
CREATE TABLE GUPTDAT (
  TDTABL   CHAR(10)      NOT NULL DEFAULT '',
  TDKEY1   CHAR(30)      NOT NULL DEFAULT '',
  TDKEY2   CHAR(30)      NOT NULL DEFAULT '',
  TDKEY3   CHAR(30)      NOT NULL DEFAULT '',
  TDDESC   CHAR(60)      NOT NULL DEFAULT '',
  TDVAL    CHAR(60)      NOT NULL DEFAULT '',
  TDCF01   CHAR(30)      NOT NULL DEFAULT '',
  TDDATA   CHAR(256)     NOT NULL DEFAULT '',
  TDDAT    DECIMAL(7,0)  NOT NULL DEFAULT 0,
  TDWN     CHAR(30)      NOT NULL DEFAULT '',
  TDAT     CHAR(30)      NOT NULL DEFAULT '',
  TDTSTP1  TIMESTAMP     NOT NULL DEFAULT '0001-01-01-00.00.00.000000',
  PRIMARY KEY (TDTABL, TDKEY1, TDKEY2, TDKEY3)
) RCDFMT GUPTDATR;
