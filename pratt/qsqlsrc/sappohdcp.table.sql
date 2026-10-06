-- SAPPOHDCP : SAP purchase order header interface staging
CREATE OR REPLACE TABLE SAPPOHDCP (
  SPHPO#   CHAR(12)       NOT NULL DEFAULT ''        , -- SAP PO number, 'PRyynnnnnn' (key)
  SPHSTS   CHAR(1)        NOT NULL DEFAULT ''        , -- Interface status
  SPHYY    CHAR(2)        NOT NULL DEFAULT ''        , -- Source PO year
  SPHOH#   DECIMAL(6,0)   NOT NULL DEFAULT 0         , -- Source PO number
  PRIMARY KEY (SPHPO#)
)
RCDFMT SAPHFMT;

LABEL ON TABLE SAPPOHDCP IS 'SAP PO Header Interface';
