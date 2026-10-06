-- ZCCITY : ZIP code to city/state table (read with READE over the 5 digit ZIP)
CREATE OR REPLACE TABLE ZCCITY (
  ZZIP     CHAR(5)        NOT NULL DEFAULT ''        , -- ZIP5 (key)
  ZCITY    CHAR(20)       NOT NULL DEFAULT ''        , -- City
  ZSTATE   CHAR(2)        NOT NULL DEFAULT ''        , -- State
  ZPREF    CHAR(1)        NOT NULL DEFAULT ''        , -- 'P' = preferred city name for this ZIP
  ZMIND    CHAR(1)        NOT NULL DEFAULT ''        , -- Military indicator
  PRIMARY KEY (ZZIP, ZCITY)
)
RCDFMT ZCITYR;

LABEL ON TABLE ZCCITY IS 'ZIP Code to City/State';
