-- WAREHOUS : Warehouse stocking rules per item
CREATE OR REPLACE TABLE WAREHOUS (
  WHDC     CHAR(2)        NOT NULL DEFAULT ''        , -- Warehouse (key 1)
  WHNN     CHAR(2)        NOT NULL DEFAULT ''        , -- Plant (key 2)
  WHID     DECIMAL(5,0)   NOT NULL DEFAULT 0         , -- Customer id (key 3)
  WHIT     CHAR(15)       NOT NULL DEFAULT ''        , -- Item (key 4)
  WHORDQ   DECIMAL(9,3)   NOT NULL DEFAULT 0         , -- Standard order quantity
  PRIMARY KEY (WHDC, WHNN, WHID, WHIT)
)
RCDFMT WHFMT;

LABEL ON TABLE WAREHOUS IS 'Warehouse Item Rules';
