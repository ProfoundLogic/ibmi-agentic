-- ============================================================================
-- HYPSGHD - Shipment Group Header
-- ----------------------------------------------------------------------------
-- Skeleton. Evidence base is thinnest of any here — HYPSGHD has no LFs in
-- the package. Derived only from:
--   * HYR0600.SQLRPGLE F-spec FHYPSGHD IF E K
--   * HYR0600 SQL: exists (select * from hypsghd b where b.GHGRP#=a.GCGRP#)
--   * HYR0600 GH* token usage (GHTSTP1, GHTSTP2, GHSTS, GHUSER, GHDCLOC)
-- Verify carefully against the live Hornady system.
-- ============================================================================
CREATE TABLE HYPSGHD (
  -- Key
  GHGRP#   DECIMAL(9,0)  NOT NULL DEFAULT 0,   -- Shipment group number (key)

  -- Status
  GHSTS    CHAR(1)       NOT NULL DEFAULT '',  -- Status code
  GHUSER   CHAR(10)      NOT NULL DEFAULT '',  -- Last user

  -- Location
  GHDCLOC  CHAR(4)       NOT NULL DEFAULT '',  -- DC / inventory location

  -- Counts
  GHSCUS   DECIMAL(5,0)  NOT NULL DEFAULT 0,   -- # customers in shipment
  GHSCWS   DECIMAL(5,0)  NOT NULL DEFAULT 0,   -- # customers w/ shipment

  -- Timestamps
  GHTSTP1  TIMESTAMP     NOT NULL DEFAULT CURRENT TIMESTAMP, -- Create stamp
  GHTSTP2  TIMESTAMP     NOT NULL DEFAULT CURRENT TIMESTAMP, -- Last update

  PRIMARY KEY ( GHGRP# )
) RCDFMT HYRSGHD;

LABEL ON TABLE HYPSGHD IS 'Shipment Group Header';
