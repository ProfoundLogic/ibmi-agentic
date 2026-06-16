-- OEORDP - Order Detail (ID-prefix)
-- Stub schema derived from PICKBATR / PICKBATLR2 / PICKERR field references.
-- Original PF source was not in the HornadyDemo package; types inferred:
--   IDORD#  -> DECIMAL(8,0)   (matches PICKORD, ORD#)
--   IDTURN  -> DECIMAL(4,0)   (matches PICKTURN, TURN)
--   IDORL#  -> DECIMAL(4,0)   (matches PICKORDL, ORL#)
--   IDBLN#  -> DECIMAL(5,0)   (bin number, fldref BLN#)
--   IDSEQ#  -> DECIMAL(3,0)   (sequence)
--   IDQOPK  -> DECIMAL(11,2)  (quantity-to-pick, fldref IMQT shape)
--   IDEMP   -> CHAR(10)       (employee, %like(EMEMPL) shape)
-- Add columns as new program references emerge.
CREATE TABLE OEORDP (
  IDORD#   DECIMAL(8,0)  NOT NULL DEFAULT 0,
  IDTURN   DECIMAL(4,0)  NOT NULL DEFAULT 0,
  IDORL#   DECIMAL(4,0)  NOT NULL DEFAULT 0,
  IDBLN#   DECIMAL(5,0)  NOT NULL DEFAULT 0,
  IDSEQ#   DECIMAL(3,0)  NOT NULL DEFAULT 0,
  IDQOPK   DECIMAL(11,2) NOT NULL DEFAULT 0,
  IDEMP    CHAR(10)      NOT NULL DEFAULT '',
  PRIMARY KEY (IDORD#, IDTURN, IDORL#, IDBLN#)
) RCDFMT OEORDPR;
