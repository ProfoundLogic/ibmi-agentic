-- SAPINCO : Freight terms / incoterms, keyed on the code held in ORHDR.OHFRT
CREATE OR REPLACE TABLE SAPINCO (
  INCOCDE  CHAR(3)        NOT NULL DEFAULT ''        , -- Freight terms code (key)
  INCODES  CHAR(30)       NOT NULL DEFAULT ''        , -- Description
  PRIMARY KEY (INCOCDE)
)
RCDFMT INCOFMT;

LABEL ON TABLE SAPINCO IS 'Freight Terms (Incoterms)';
