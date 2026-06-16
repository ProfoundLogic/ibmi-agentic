-- OEORDT - Order Detail Trade (OD-prefix)
-- Stub schema derived from PICKBATLR2 references.  Only ODORD# / ODORL#
-- columns are referenced today (join key against PICKBATDP).  Add columns
-- as new program references emerge.
CREATE TABLE OEORDT (
  ODORD#   DECIMAL(8,0)  NOT NULL DEFAULT 0,
  ODORL#   DECIMAL(4,0)  NOT NULL DEFAULT 0,
  PRIMARY KEY (ODORD#, ODORL#)
) RCDFMT OEORDTR;
