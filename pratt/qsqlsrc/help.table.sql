-- HELP : Generic application pick-list table.
-- The sort-order drop-downs self-join this table: T1 (HLPSUB='SORTT') supplies the
-- text the user sees, T2 (HLPSUB='SORTV') supplies the column name handed to SQLORDER.
CREATE OR REPLACE TABLE HELP (
  HLPAPP   CHAR(10)       NOT NULL DEFAULT ''        , -- Application, e.g. 'POSELUI' (key 1)
  HLPSUB   CHAR(10)       NOT NULL DEFAULT ''        , -- Sub-list, e.g. 'SORTT'/'SORTV' (key 2)
  HLPVAL   CHAR(10)       NOT NULL DEFAULT ''        , -- Join value pairing text with column (key 3)
  HLPSEQ   DECIMAL(3,0)   NOT NULL DEFAULT 0         , -- Display sequence
  HLPTXT   CHAR(50)       NOT NULL DEFAULT ''        , -- Text, or column name for SORTV rows
  PRIMARY KEY (HLPAPP, HLPSUB, HLPVAL)
)
RCDFMT HLPFMT;

LABEL ON TABLE HELP IS 'Application Pick Lists';
