-- USRLEVEL : Authority level per user per application, read by the USRRTV helper.
-- Levels understood by the applications: NOV, INT, EXP, PGMR (anything else = no access).
CREATE OR REPLACE TABLE USRLEVEL (
  ULUSER   CHAR(10)       NOT NULL DEFAULT ''        , -- User profile (key 1)
  ULPGM    CHAR(10)       NOT NULL DEFAULT ''        , -- Application, e.g. OHMGR (key 2)
  ULLVL    CHAR(10)       NOT NULL DEFAULT ''        , -- NOV / INT / EXP / PGMR
  PRIMARY KEY (ULUSER, ULPGM)
)
RCDFMT ULFMT;

LABEL ON TABLE USRLEVEL IS 'User Authority Levels';
