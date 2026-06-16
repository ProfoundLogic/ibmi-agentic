-- HDCUST - Customer master (CM-prefix)
-- Updated 2026-06: original promotion used CU-prefix which doesn't match the
-- actual Hornady program references.  Both HYR0600 and PICKBATR reference
-- CMCUST / CMCNA1 / CMALPH / CMCCLS etc.  Prefix corrected to CM.
-- Field types inferred from naming + fldref:
--   CMCUST  -> DECIMAL(7,0)   (cust#, fldref CUST)
--   CMCNA1  -> CHAR(30)       (customer name)
--   CMCCLS  -> CHAR(3)        (customer class)
--   CMLOC#  -> DECIMAL(7,0)   (location/store#)
--   CMD01/02-> CHAR(30)       (address lines)
--   CMTS    -> TIMESTAMP      (last update)
--   CMSHPV  -> CHAR(2)        (default ship-via)
--   CMSTS   -> CHAR(1)        (status)
CREATE TABLE HDCUST (
  CMCUST   DECIMAL(7,0)  NOT NULL DEFAULT 0,
  CMCNA1   CHAR(30)      NOT NULL DEFAULT '',
  CMALPH   CHAR(30)      NOT NULL DEFAULT '',
  CMCCLS   CHAR(3)       NOT NULL DEFAULT '',
  CMLOC#   DECIMAL(7,0)  NOT NULL DEFAULT 0,
  CMD01    CHAR(30)      NOT NULL DEFAULT '',
  CMD02    CHAR(30)      NOT NULL DEFAULT '',
  CMCITY   CHAR(20)      NOT NULL DEFAULT '',
  CMSTAT   CHAR(2)       NOT NULL DEFAULT '',
  CMZIP    CHAR(9)       NOT NULL DEFAULT '',
  CMCNTRY  CHAR(3)       NOT NULL DEFAULT '',
  CMSHPV   CHAR(2)       NOT NULL DEFAULT '',
  CMSTS    CHAR(1)       NOT NULL DEFAULT '',
  CMTS     TIMESTAMP     NOT NULL DEFAULT '0001-01-01-00.00.00.000000',
  PRIMARY KEY (CMCUST)
) RCDFMT HDRCUST;
