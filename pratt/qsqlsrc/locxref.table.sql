-- LOCXREF : Location / trading partner cross reference
-- Read twice: by (L0LOC, L0LAKT) as LOCXREF, and by (L0LAKT, L0LAK) as LOCXREFLF.
CREATE OR REPLACE TABLE LOCXREF (
  L0LOC    CHAR(3)        NOT NULL DEFAULT ''        , -- Location code (key 1)
  L0LAKT   CHAR(4)        NOT NULL DEFAULT ''        , -- Lookup type (key 2)
  L0LAK    CHAR(20)       NOT NULL DEFAULT ''        , -- Lookup value / partner key
  L0DEL    CHAR(1)        NOT NULL DEFAULT ''        , -- 'D' = deleted
  PRIMARY KEY (L0LOC, L0LAKT)
)
RCDFMT L0FMT;

LABEL ON TABLE LOCXREF IS 'Location Cross Reference';
