-- OEORHP - Order Header (IH-prefix)
-- Stub schema derived from PICKBATR / PICKBATLR2 field references.  The
-- HornadyDemo package didn't ship the original PF source, so the column
-- list and types are inferred from RPGLE host-variable %LIKE() targets:
--   IHTURN ~ PICKTURN -> DECIMAL(4,0)
--   IHORD# ~ PICKORD  -> DECIMAL(8,0)
--   IHSHTO/IHBLTO/IHDSHP follow ship-to / bill-to / drop-ship cust# size 7,0.
-- Add more columns as new program references appear.
CREATE TABLE OEORHP (
  IHORD#   DECIMAL(8,0) NOT NULL DEFAULT 0,
  IHTURN   DECIMAL(4,0) NOT NULL DEFAULT 0,
  IHSHTO   DECIMAL(7,0) NOT NULL DEFAULT 0,
  IHBLTO   DECIMAL(7,0) NOT NULL DEFAULT 0,
  IHDSHP   DECIMAL(7,0) NOT NULL DEFAULT 0,
  IHORRF   CHAR(20)     NOT NULL DEFAULT '',
  IHORTY   CHAR(2)      NOT NULL DEFAULT '',
  PRIMARY KEY (IHORD#, IHTURN)
) RCDFMT OEORHPR;
