-- ORITX : Purchase Order Line Text (8 printed lines + 8 internal lines per PO line)
CREATE OR REPLACE TABLE ORITX (
  OITYY    CHAR(2)        NOT NULL DEFAULT ''        , -- PO year (key 1)
  OITPP    CHAR(2)        NOT NULL DEFAULT ''        , -- Business area (key 2)
  OIT#     NUMERIC(6,0)   NOT NULL DEFAULT 0         , -- PO number (key 3)
  OITLN#   NUMERIC(3,0)   NOT NULL DEFAULT 0         , -- PO line number (key 4)
  OITPRT   CHAR(25)       NOT NULL DEFAULT ''        , -- Vendor part number (key 5)
  OITSUB   CHAR(2)        NOT NULL DEFAULT ''        , -- Sub-sequence (key 6)
  OITXT1   CHAR(60)       NOT NULL DEFAULT ''        , -- Printed note line 1
  OITXT2   CHAR(60)       NOT NULL DEFAULT ''        ,
  OITXT3   CHAR(60)       NOT NULL DEFAULT ''        ,
  OITXT4   CHAR(60)       NOT NULL DEFAULT ''        ,
  OITXT5   CHAR(60)       NOT NULL DEFAULT ''        ,
  OITXT6   CHAR(60)       NOT NULL DEFAULT ''        ,
  OITXT7   CHAR(60)       NOT NULL DEFAULT ''        ,
  OITXT8   CHAR(60)       NOT NULL DEFAULT ''        ,
  OITXTU1  CHAR(60)       NOT NULL DEFAULT ''        , -- Internal note line 1
  OITXTU2  CHAR(60)       NOT NULL DEFAULT ''        ,
  OITXTU3  CHAR(60)       NOT NULL DEFAULT ''        ,
  OITXTU4  CHAR(60)       NOT NULL DEFAULT ''        ,
  OITXTU5  CHAR(60)       NOT NULL DEFAULT ''        ,
  OITXTU6  CHAR(60)       NOT NULL DEFAULT ''        ,
  OITXTU7  CHAR(60)       NOT NULL DEFAULT ''        ,
  OITXTU8  CHAR(60)       NOT NULL DEFAULT ''        ,
  OITUSE   CHAR(10)       NOT NULL DEFAULT ''        , -- Last maintained by
  OITDAT   DATE           NOT NULL DEFAULT '0001-01-01', -- Last maintained date
  OITCX    CHAR(1)        NOT NULL DEFAULT ''        , -- 'X' = cancelled
  PRIMARY KEY (OITYY, OITPP, OIT#, OITLN#, OITPRT, OITSUB)
)
RCDFMT OITFMT;

LABEL ON TABLE ORITX IS 'Purchase Order Line Text';
