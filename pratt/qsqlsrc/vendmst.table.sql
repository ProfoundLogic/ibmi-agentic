-- VENDMST : Vendor Master
CREATE OR REPLACE TABLE VENDMST (
  VNUMB    DECIMAL(7,0)   NOT NULL DEFAULT 0         , -- Vendor number (key)
  VNNAME   CHAR(30)       NOT NULL DEFAULT ''        , -- Vendor name
  VNCODE   CHAR(1)        NOT NULL DEFAULT ''        , -- ' ' = active, any other value = inactive
  VNEMAL   CHAR(50)       NOT NULL DEFAULT ''        , -- PO transmission e-mail address
  PRIMARY KEY (VNUMB)
)
RCDFMT VENDFMT;

LABEL ON TABLE VENDMST IS 'Vendor Master';
