-- XLSLIST : Spreadsheet download request queue used by the SQL2XLSXD helper
CREATE OR REPLACE TABLE XLSLIST (
  XLSUSER  CHAR(10)       NOT NULL DEFAULT ''        , -- Requesting user (key 1)
  XLSNBR   CHAR(14)       NOT NULL DEFAULT ''        , -- Request number (key 2)
  XLSFILE  CHAR(50)       NOT NULL DEFAULT ''        , -- Generated file name
  XLSSTS   CHAR(1)        NOT NULL DEFAULT ''        , -- Status
  PRIMARY KEY (XLSUSER, XLSNBR)
)
RCDFMT XLSFMT;

LABEL ON TABLE XLSLIST IS 'Spreadsheet Download Queue';
