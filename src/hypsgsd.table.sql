-- ============================================================================
-- HYPSGSD - Shipment Sequence Detail
-- ----------------------------------------------------------------------------
-- Keyed by CDTURN per HYLSGSD1.LF (CDTURN is the turnaround id).  Additional
-- ship-via fields (CDDS#, CDSTYP, CDSHPV, CDBLTO) come from the PICKBATR
-- combine-batches flow:
--   update HYPSGSD set CDDS#=:..., CDSTYP=:..., CDSHPV=:..., CDBLTO=:...
--     where CDTURN in (:baseTurn,:moveTurn)
-- Field widths chosen to align with the host variables PICKBATR binds (DS#
-- shaped like %char shipping doc number, SHPV/STYP align with HDSHPV /
-- HYPSVCT's CHAR(2) / CHAR(2)-ish flags, BLTO sized as 7,0 cust#).
-- ============================================================================
CREATE TABLE HYPSGSD (
  CDTURN   DECIMAL(4,0)  NOT NULL DEFAULT 0,   -- Turnaround (key)
  CDSEQ    DECIMAL(5,0)  NOT NULL DEFAULT 0,   -- Sequence
  CDDS#    DECIMAL(9,0)  NOT NULL DEFAULT 0,   -- Shipping doc number
  CDSTYP   CHAR(2)       NOT NULL DEFAULT '',  -- Shipment type
  CDSHPV   CHAR(2)       NOT NULL DEFAULT '',  -- Ship-via code
  CDBLTO   DECIMAL(7,0)  NOT NULL DEFAULT 0,   -- Bill-to cust#
  CDTSTP1  TIMESTAMP     NOT NULL DEFAULT CURRENT_TIMESTAMP,

  PRIMARY KEY ( CDTURN )
) RCDFMT HYRSGSD;

LABEL ON TABLE HYPSGSD IS 'Shipment Sequence Detail';
