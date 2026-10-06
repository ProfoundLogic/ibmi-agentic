-- ORHTX : Purchase Order Header Text ("Text & Instructions" on the detail screen)
CREATE OR REPLACE TABLE ORHTX (
  OHTYY    CHAR(2)        NOT NULL DEFAULT ''        , -- PO year (key 1)
  OHTPP    CHAR(2)        NOT NULL DEFAULT ''        , -- Business area (key 2)
  OHT#     NUMERIC(6,0)   NOT NULL DEFAULT 0         , -- PO number (key 3)
  OHTPKEY  CHAR(8)        NOT NULL DEFAULT ''        , -- Partner key, e.g. 'RTL2nnnn' (key 4)
  OHTXT1   CHAR(250)      NOT NULL DEFAULT ''        , -- Header text
  OHTXP    CHAR(10)       NOT NULL DEFAULT ''        , -- Text profile
  OHTUSE   CHAR(10)       NOT NULL DEFAULT ''        , -- Last maintained by
  OHTDAT   DATE           NOT NULL DEFAULT '0001-01-01', -- Last maintained date
  OHTCX    CHAR(1)        NOT NULL DEFAULT ''        , -- 'X' = cancelled
  OHTDEL   CHAR(1)        NOT NULL DEFAULT ''        , -- 'D' = deleted
  PRIMARY KEY (OHTYY, OHTPP, OHT#, OHTPKEY)
)
RCDFMT OTFMT;

LABEL ON TABLE ORHTX IS 'Purchase Order Header Text';
