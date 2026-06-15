-- ============================================================================
-- HYPSGTD - Shipment Group Tote Detail
-- ----------------------------------------------------------------------------
-- Skeleton. Field list derived from:
--   * LF keys in HYLSGTD1, HYLSGTD2 (TDTOTE, TDGRP#, TDBLTO, TDSHTO, TDDSHP,
--     TDITEM, TDTURN, TDTSEQ, TDRSEQ)
--   * HYR0608.SQLRPGLE F-spec FHYLSGTD1/2
--   * Token usage of TD* names in HYR0600 / HYR0608
-- ============================================================================
CREATE TABLE AITSK00030/HYPSGTD (
  -- Keys (from LFs)
  TDTOTE  CHAR(20)      NOT NULL DEFAULT '',  -- Tote ID (key)
  TDGRP#  DECIMAL(7,0)  NOT NULL DEFAULT 0,   -- Shipment group
  TDBLTO  DECIMAL(7,0)  NOT NULL DEFAULT 0,   -- Bill-to customer
  TDSHTO  DECIMAL(7,0)  NOT NULL DEFAULT 0,   -- Ship-to customer
  TDDSHP  DECIMAL(7,0)  NOT NULL DEFAULT 0,   -- Drop-ship customer
  TDITEM  CHAR(15)      NOT NULL DEFAULT '',  -- Item
  TDTURN  DECIMAL(4,0)  NOT NULL DEFAULT 0,   -- Turnaround
  TDTSEQ  DECIMAL(5,0)  NOT NULL DEFAULT 0,   -- Tote sequence
  TDRSEQ  DECIMAL(5,0)  NOT NULL DEFAULT 0,   -- Record sequence

  -- Order / line refs
  TDORD#  DECIMAL(8,0)  NOT NULL DEFAULT 0,   -- Order
  TDORL#  DECIMAL(4,0)  NOT NULL DEFAULT 0,   -- Order line
  TDBLN#  DECIMAL(5,0)  NOT NULL DEFAULT 0,   -- Bin number
  TDOCTL  CHAR(8)       NOT NULL DEFAULT '',  -- Order control

  -- Quantities / status
  TDQSTC  DECIMAL(9,0)  NOT NULL DEFAULT 0,   -- Quantity stocked
  TDQSTCB DECIMAL(9,0)  NOT NULL DEFAULT 0,   -- Quantity stocked (before)
  TDQSTCH DECIMAL(9,0)  NOT NULL DEFAULT 0,   -- Quantity stocked (hold)

  -- Tracking
  TDTRK#  CHAR(30)      NOT NULL DEFAULT '',  -- Tracking number
  TDPKL#  DECIMAL(8,0)  NOT NULL DEFAULT 0,   -- Packing list number

  -- Misc
  TDESC   CHAR(30)      NOT NULL DEFAULT '',  -- Description
  TDBUSY  CHAR(1)       NOT NULL DEFAULT '',  -- Busy flag
  TDOWN   CHAR(10)      NOT NULL DEFAULT '',  -- Owner

  -- Timestamps
  TDTSTP2 TIMESTAMP     NOT NULL DEFAULT CURRENT TIMESTAMP, -- Last update

  PRIMARY KEY ( TDTOTE, TDGRP#, TDBLTO, TDSHTO, TDDSHP, TDITEM, TDTURN, TDTSEQ, TDRSEQ )
) RCDFMT HYRSGTD;

LABEL ON TABLE AITSK00030/HYPSGTD IS 'Shipment Group Tote Detail';
