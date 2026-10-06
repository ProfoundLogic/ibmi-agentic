-- USERIDS : Application user profile settings
CREATE OR REPLACE TABLE USERIDS (
  UIUSER   CHAR(10)       NOT NULL DEFAULT ''        , -- User profile (key)
  UINN     CHAR(2)        NOT NULL DEFAULT ''        , -- Default plant
  UISAP    CHAR(10)       NOT NULL DEFAULT ''        , -- SAP user id; blank = restricted mode
  PRIMARY KEY (UIUSER)
)
RCDFMT UIFMT;

LABEL ON TABLE USERIDS IS 'Application User Settings';
