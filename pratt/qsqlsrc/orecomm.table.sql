-- ORECOMM : E-commerce order cross reference (read as ORECOMM and as ORECT)
CREATE OR REPLACE TABLE ORECOMM (
  ORORDID  CHAR(20)       NOT NULL DEFAULT ''        , -- Ecom order id (key) -> ORHDR.OHORDID
  ORYY     CHAR(2)        NOT NULL DEFAULT ''        , -- PO year
  ORPP     CHAR(2)        NOT NULL DEFAULT ''        , -- Business area
  OR#      NUMERIC(6,0)   NOT NULL DEFAULT 0         , -- PO number
  ORSORDR  CHAR(25)       NOT NULL DEFAULT ''        , -- Source/ecom sales order number
  PRIMARY KEY (ORORDID)
)
RCDFMT ORFMT;

LABEL ON TABLE ORECOMM IS 'E-Commerce Order Cross Reference';
