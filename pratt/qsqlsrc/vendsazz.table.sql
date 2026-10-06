-- VENDSAZZ : Vendor generic code table.
-- WARNING - byte positions are load bearing.  PODTLUI overlays
--   VZZ CHAR(15) DIM(26) POS(21) : 26 value slots immediately after the 20 byte key.
CREATE OR REPLACE TABLE VENDSAZZ (
  VZZKEY   CHAR(20)       NOT NULL DEFAULT ''            , -- 1-20   Composite key, e.g. 'BASEUOFM' (key)
  VZZ1     CHAR(15)       NOT NULL DEFAULT ''            , -- 21-410 Value slot 1
  VZZ2     CHAR(15)       NOT NULL DEFAULT ''            , -- 21-410 Value slot 2
  VZZ3     CHAR(15)       NOT NULL DEFAULT ''            , -- 21-410 Value slot 3
  VZZ4     CHAR(15)       NOT NULL DEFAULT ''            , -- 21-410 Value slot 4
  VZZ5     CHAR(15)       NOT NULL DEFAULT ''            , -- 21-410 Value slot 5
  VZZ6     CHAR(15)       NOT NULL DEFAULT ''            , -- 21-410 Value slot 6
  VZZ7     CHAR(15)       NOT NULL DEFAULT ''            , -- 21-410 Value slot 7
  VZZ8     CHAR(15)       NOT NULL DEFAULT ''            , -- 21-410 Value slot 8
  VZZ9     CHAR(15)       NOT NULL DEFAULT ''            , -- 21-410 Value slot 9
  VZZ10    CHAR(15)       NOT NULL DEFAULT ''            , -- 21-410 Value slot 10
  VZZ11    CHAR(15)       NOT NULL DEFAULT ''            , -- 21-410 Value slot 11
  VZZ12    CHAR(15)       NOT NULL DEFAULT ''            , -- 21-410 Value slot 12
  VZZ13    CHAR(15)       NOT NULL DEFAULT ''            , -- 21-410 Value slot 13
  VZZ14    CHAR(15)       NOT NULL DEFAULT ''            , -- 21-410 Value slot 14
  VZZ15    CHAR(15)       NOT NULL DEFAULT ''            , -- 21-410 Value slot 15
  VZZ16    CHAR(15)       NOT NULL DEFAULT ''            , -- 21-410 Value slot 16
  VZZ17    CHAR(15)       NOT NULL DEFAULT ''            , -- 21-410 Value slot 17
  VZZ18    CHAR(15)       NOT NULL DEFAULT ''            , -- 21-410 Value slot 18
  VZZ19    CHAR(15)       NOT NULL DEFAULT ''            , -- 21-410 Value slot 19
  VZZ20    CHAR(15)       NOT NULL DEFAULT ''            , -- 21-410 Value slot 20
  VZZ21    CHAR(15)       NOT NULL DEFAULT ''            , -- 21-410 Value slot 21
  VZZ22    CHAR(15)       NOT NULL DEFAULT ''            , -- 21-410 Value slot 22
  VZZ23    CHAR(15)       NOT NULL DEFAULT ''            , -- 21-410 Value slot 23
  VZZ24    CHAR(15)       NOT NULL DEFAULT ''            , -- 21-410 Value slot 24
  VZZ25    CHAR(15)       NOT NULL DEFAULT ''            , -- 21-410 Value slot 25
  VZZ26    CHAR(15)       NOT NULL DEFAULT ''            , -- 21-410 Value slot 26
  PRIMARY KEY (VZZKEY)
)
RCDFMT VZZFMT;

LABEL ON TABLE VENDSAZZ IS 'Vendor Generic Code Table';
