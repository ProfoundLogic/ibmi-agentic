-- HDCCMT - Customer / Order comments
-- Schema rebuilt 2026-06: original CXCUST/CXCTYP/CXORD#/CXSEQ key list did
-- not match the actual program usage.  PICKERR's `setll (hld_CXCVF :
-- hld_IHBLTO : hld_PICCMT) HDCCMT` calls into HDCCMT with three keys whose
-- types are CHAR(1) / DECIMAL(7,0) / CHAR(3):
--   CXCVF   -> 'C' = Customer-Comment View Flag
--   CXBLTO  -> bill-to customer number (matches OEORHP.IHBLTO)
--   CXTYPE  -> 'PIC' = picking-comment category
-- CXSEQ stays as the within-key sequence number so multiple comment lines
-- can live under the same key tuple.
CREATE TABLE HDCCMT (
  CXCVF    CHAR(1)       NOT NULL DEFAULT '',
  CXBLTO   DECIMAL(7,0)  NOT NULL DEFAULT 0,
  CXTYPE   CHAR(3)       NOT NULL DEFAULT '',
  CXSEQ    DECIMAL(5,0)  NOT NULL DEFAULT 0,
  CXCMNT   CHAR(60)      NOT NULL DEFAULT '',
  PRIMARY KEY (CXCVF, CXBLTO, CXTYPE, CXSEQ)
) RCDFMT HDRCCMT;
