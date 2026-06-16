-- HYPTDTA - Generic table data (Hornady-specific key/value lookup)
-- Same TD-prefix shape as GUPTDAT.  HYR0600 chains/queries patterns like:
--   where TDTABL='SHIPVIA'    and TDKEY1=:wk_ShipVia
--   where TDTABL='BXLLINDFTS' and TDKEY1=:ps_User
--   where TDTABL='ALDAWKSTN'  and TDKEY1=:ps_Job
-- PICKBATR additionally writes a TDBUSY claim flag (set to 'DIGIPICK' while
-- a turn is busy, cleared on delete).  TDTRK# / TDSHPV remain from the
-- earlier (non-table-data) usage as picker-order/shipper-specific extras.
CREATE TABLE HYPTDTA (
  TDTABL   CHAR(10)      NOT NULL DEFAULT '',
  TDKEY1   CHAR(30)      NOT NULL DEFAULT '',
  TDKEY2   CHAR(30)      NOT NULL DEFAULT '',
  TDKEY3   CHAR(30)      NOT NULL DEFAULT '',
  TDDESC   CHAR(60)      NOT NULL DEFAULT '',
  TDVAL    CHAR(60)      NOT NULL DEFAULT '',
  TDORD#   DECIMAL(8,0)  NOT NULL DEFAULT 0,
  TDTURN   DECIMAL(4,0)  NOT NULL DEFAULT 0,
  TDTRK#   CHAR(30)      NOT NULL DEFAULT '',
  TDSHPV   CHAR(2)       NOT NULL DEFAULT '',
  TDBUSY   CHAR(10)      NOT NULL DEFAULT '',
  TDTSTP1  TIMESTAMP     NOT NULL DEFAULT '0001-01-01-00.00.00.000000',
  PRIMARY KEY (TDTABL, TDKEY1, TDKEY2, TDKEY3)
) RCDFMT HYRTDTA;
