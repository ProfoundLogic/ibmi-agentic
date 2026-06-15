-- ============================================================================
-- HYPSGLD - Shipment Group Lot Detail
-- ----------------------------------------------------------------------------
-- Skeleton. Field list derived from:
--   * LF keys in HYLSGLD1 (LDGRP#, LDTURN, LDTSEQ, LDRSEQ, LDLOT#, LDSSCC)
--   * DSPF REFFLD() in HYD0606 (LDLOT#, LDQSTC, LDSSCC plus GD* refs)
--   * HYR0600.SQLRPGLE LD* token usage
--   * HYR0606 LF F-spec FHYLSGLD1
-- ============================================================================
CREATE TABLE AITSK00030/HYPSGLD (
  -- Keys (from LF)
  LDGRP#  DECIMAL(7,0)  NOT NULL DEFAULT 0,   -- Shipment group (key)
  LDTURN  DECIMAL(4,0)  NOT NULL DEFAULT 0,   -- Turnaround
  LDTSEQ  DECIMAL(5,0)  NOT NULL DEFAULT 0,   -- Tote sequence
  LDRSEQ  DECIMAL(5,0)  NOT NULL DEFAULT 0,   -- Record sequence
  LDLOT#  CHAR(10)      NOT NULL DEFAULT '',  -- Lot number
  LDSSCC  CHAR(20)      NOT NULL DEFAULT '',  -- SSCC barcode

  -- Joined from HYPSGDT (HYD0606 REFFLDs against GD* fields too —
  -- carried here for the inquiry screen's convenience)
  LDBLN#  DECIMAL(5,0)  NOT NULL DEFAULT 0,   -- Bin number
  LDBLTO  DECIMAL(7,0)  NOT NULL DEFAULT 0,   -- Bill-to customer
  LDDSHP  DECIMAL(7,0)  NOT NULL DEFAULT 0,   -- Drop-ship customer
  LDSHTO  DECIMAL(7,0)  NOT NULL DEFAULT 0,   -- Ship-to customer
  LDITEM  CHAR(15)      NOT NULL DEFAULT '',  -- Item
  LDORD#  DECIMAL(8,0)  NOT NULL DEFAULT 0,   -- Order
  LDORL#  DECIMAL(4,0)  NOT NULL DEFAULT 0,   -- Order line
  LDOCTL  CHAR(8)       NOT NULL DEFAULT '',  -- Order control

  -- Quantities
  LDQSTC  DECIMAL(9,0)  NOT NULL DEFAULT 0,   -- Quantity stocked / picked

  PRIMARY KEY ( LDGRP#, LDTURN, LDTSEQ, LDRSEQ, LDLOT#, LDSSCC )
) RCDFMT HYRSGLD;

LABEL ON TABLE AITSK00030/HYPSGLD IS 'Shipment Group Lot Detail';
