-- ============================================================================
-- HYPSGSD - Shipment Sequence Detail
-- ----------------------------------------------------------------------------
-- Smallest table. Only evidence is HYLSGSD1.LF keyed on CDTURN (the field
-- name suggests it's a shared-counter for turnaround sequencing).
-- Add fields as needed once the program that reads this turns up.
-- ============================================================================
CREATE TABLE HYPSGSD (
  CDTURN   DECIMAL(4,0)  NOT NULL DEFAULT 0,   -- Turnaround (key)
  CDSEQ    DECIMAL(5,0)  NOT NULL DEFAULT 0,   -- Sequence
  CDTSTP1  TIMESTAMP     NOT NULL DEFAULT CURRENT TIMESTAMP,

  PRIMARY KEY ( CDTURN )
) RCDFMT HYRSGSD;

LABEL ON TABLE HYPSGSD IS 'Shipment Sequence Detail';
