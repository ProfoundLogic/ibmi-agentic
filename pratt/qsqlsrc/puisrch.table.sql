-- PUISRCH : Per-user saved searches, maintained by PUISRCHUI
CREATE OR REPLACE TABLE PUISRCH (
  PSRAPP   CHAR(10)       NOT NULL DEFAULT ''        , -- Owning application (POSELUI / PMSELUI)
  PSRUSR   CHAR(10)       NOT NULL DEFAULT ''        , -- User profile
  PSRNAM   CHAR(30)       NOT NULL DEFAULT ''        , -- Search name as typed by the user
  PSRDFT   CHAR(1)        NOT NULL DEFAULT 'N'       , -- 'Y' = load this one automatically
  PSRVAL   VARCHAR(2000)  NOT NULL DEFAULT ''        , -- JSON blob of the screen's .search fields
  PRIMARY KEY (PSRAPP, PSRUSR, PSRNAM)
)
RCDFMT PSRFMT;

LABEL ON TABLE PUISRCH IS 'Profound UI Saved Searches';
