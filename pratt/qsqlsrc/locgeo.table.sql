-- LOCGEO : Warehouse geography. Joined to COADDRES on L2WH = COAPLT so the warehouse
-- drop-downs can be filtered to the plant (PRTM) the user is working in.
CREATE OR REPLACE TABLE LOCGEO (
  L2WH     CHAR(2)        NOT NULL DEFAULT ''        , -- Warehouse code (key) -> COADDRES.COAPLT
  PRTM     CHAR(2)        NOT NULL DEFAULT ''        , -- Parent plant / business area
  L2REG    CHAR(15)       NOT NULL DEFAULT ''        , -- Region
  L2DEL    CHAR(1)        NOT NULL DEFAULT ''        , -- 'D' = deleted
  PRIMARY KEY (L2WH)
)
RCDFMT L2FMT;

LABEL ON TABLE LOCGEO IS 'Warehouse Geography';
