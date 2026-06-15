-- ============================================================================
-- HYPSGDT - Shipment Group Detail
-- ----------------------------------------------------------------------------
-- Skeleton. Field list derived from:
--   * LF keys in HYLSGDT1..5 (GDGRP#, GDBLTO, GDSHTO, GDDSHP, GDTURN,
--     GDITEM, GDORD#, GDORL#, GDBLN#, GDRSEQ, GDSSCCBC)
--   * DSPF REFFLD() in HYD0602A / HYD0602B / HYD0606 / HYD0608B
--   * HYR0600.SQLRPGLE token usage of GD* names
--   * SQL ops: HYR0600 has "delete from HYPSGDT" and multiple SELECTs
-- ============================================================================
CREATE TABLE HYPSGDT (
  -- Keys (from LFs)
  GDGRP#  DECIMAL(9,0)  NOT NULL DEFAULT 0,   -- Shipment group  (key)
  GDBLTO  DECIMAL(7,0)  NOT NULL DEFAULT 0,   -- Bill-to customer
  GDSHTO  DECIMAL(7,0)  NOT NULL DEFAULT 0,   -- Ship-to customer
  GDDSHP  DECIMAL(7,0)  NOT NULL DEFAULT 0,   -- Drop-ship customer
  GDTURN  DECIMAL(9,0)  NOT NULL DEFAULT 0,   -- Turnaround
  GDITEM  CHAR(15)      NOT NULL DEFAULT '',  -- Item
  GDORD#  DECIMAL(8,0)  NOT NULL DEFAULT 0,   -- Order number
  GDORL#  DECIMAL(4,0)  NOT NULL DEFAULT 0,   -- Order line
  GDBLN#  DECIMAL(5,0)  NOT NULL DEFAULT 0,   -- Bin number
  GDRSEQ  DECIMAL(5,0)  NOT NULL DEFAULT 0,   -- Record sequence
  GDSSCCBC CHAR(20)     NOT NULL DEFAULT '',  -- SSCC barcode

  -- Order / item ref
  GDOCTL  CHAR(8)       NOT NULL DEFAULT '',  -- Order control number
  GDIMDS  CHAR(25)      NOT NULL DEFAULT '',  -- Item description
  GDSHPV  CHAR(2)       NOT NULL DEFAULT '',  -- Ship-via
  GDSVDS  CHAR(30)      NOT NULL DEFAULT '',  -- Ship-via description

  -- Quantities (DSPF REFFLDs and HYR0600 references)
  GDQOPN  DECIMAL(9,0)  NOT NULL DEFAULT 0,   -- Quantity open
  GDQOPK  DECIMAL(9,0)  NOT NULL DEFAULT 0,   -- Quantity to-be-packed
  GDQSTC  DECIMAL(9,0)  NOT NULL DEFAULT 0,   -- Quantity stocked / picked

  -- Lot / pallet / SSCC
  GDSSCC  DECIMAL(20,0) NOT NULL DEFAULT 0,    -- SSCC
  GDLOT   CHAR(10)      NOT NULL DEFAULT '',  -- Lot number
  GDMXLT  CHAR(1)       NOT NULL DEFAULT '',  -- Mixed-lot flag
  GDMXPO  CHAR(1)       NOT NULL DEFAULT '',  -- Mixed-PO flag
  GDPLOC  CHAR(14)      NOT NULL DEFAULT '',  -- Pallet location
  GDNAME  CHAR(30)      NOT NULL DEFAULT '',  -- Customer/ship-to name (HYR0606)
  GDDVRT  DECIMAL(3,0)  NOT NULL DEFAULT 0,      -- Shipping lane / divert

  -- Tote / conveyor
  GDCNVY  CHAR(1)       NOT NULL DEFAULT '',  -- Conveyable flag
  GDTOTE  CHAR(20)      NOT NULL DEFAULT '',  -- Tote ID

  -- Misc
  GDPO#   CHAR(25)      NOT NULL DEFAULT '',  -- PO number
  GDPIKO  CHAR(1)       NOT NULL DEFAULT '',  -- Pick-only flag
  GDSPPK  CHAR(1)       NOT NULL DEFAULT '',  -- Special-pack flag
  GDTSEQ  DECIMAL(3,0)  NOT NULL DEFAULT 0,   -- Tote sequence

  -- Labels (HYR0600 GDLBL1 referenced)
  GDLBL1  CHAR(40)      NOT NULL DEFAULT '',  -- Label line 1

  PRIMARY KEY ( GDGRP#, GDTURN, GDTSEQ, GDRSEQ, GDITEM, GDORD#, GDORL#, GDBLN#, GDSSCCBC )
) RCDFMT HYRSGDT;

LABEL ON TABLE HYPSGDT IS 'Shipment Group Detail';
