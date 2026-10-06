-- MASTSAZZ : Generic code/description table, keyed on type + code (e.g. 'COUNTRY' + 'USA')
CREATE OR REPLACE TABLE MASTSAZZ (
  QZZKEY   CHAR(16)       NOT NULL DEFAULT ''        , -- Composite key, e.g. 'COUNTRYUSA'
  QZZDTA   CHAR(30)       NOT NULL DEFAULT ''        , -- Description
  PRIMARY KEY (QZZKEY)
)
RCDFMT QZZFMT;

LABEL ON TABLE MASTSAZZ IS 'Generic Code / Description Table';
