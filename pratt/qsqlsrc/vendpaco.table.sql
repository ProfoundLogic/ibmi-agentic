-- VENDPACO : Vendor Part Cost by organisation / ship-to.
-- WARNING - byte positions are load bearing.  PODTLUI overlays:
--   VOQ PACKED(7) DIM(5) POS(68), VOC PACKED(11:2) DIM(5) POS(88),
--   VOF POS(133), VOA POS(163).
CREATE OR REPLACE TABLE VENDPACO (
  VOPART   CHAR(25)       NOT NULL DEFAULT ''            , -- 1-25   Vendor part number (key 1)
  VOOO     CHAR(4)        NOT NULL DEFAULT ''            , -- 26-29  Organisation (key 2)
  VONN     CHAR(2)        NOT NULL DEFAULT ''            , -- 30-31  Plant (key 3)
  VOID     DECIMAL(5,0)   NOT NULL DEFAULT 0             , -- 32-34  Customer id (key 4)
  VOORAX   CHAR(3)        NOT NULL DEFAULT ''            , -- 35-37  Ship-to axis (key 5)
  VOLOC    CHAR(3)        NOT NULL DEFAULT ''            , -- 38-40  Location
  VOOVHD   DECIMAL(15,5)  NOT NULL DEFAULT 0             , -- 41-48  Overhead multiplier
  VOCCOD   DECIMAL(2,0)   NOT NULL DEFAULT 0             , -- 49-50  Cost code (numeric)
  VOFIL0   CHAR(1)        NOT NULL DEFAULT ''            , -- 51     Reserved
  VOSRCL   DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 52-57  Source freight (per load)
  VOSRCU   DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 58-63  Source surcharge (per unit)
  VOEXPD   DECIMAL(7,0)   NOT NULL DEFAULT 0             , -- 64-67  Expiry CYMD
  VOQ1     DECIMAL(7,0)   NOT NULL DEFAULT 0             , -- 68-87  Price break quantity 1
  VOQ2     DECIMAL(7,0)   NOT NULL DEFAULT 0             , -- 68-87  Price break quantity 2
  VOQ3     DECIMAL(7,0)   NOT NULL DEFAULT 0             , -- 68-87  Price break quantity 3
  VOQ4     DECIMAL(7,0)   NOT NULL DEFAULT 0             , -- 68-87  Price break quantity 4
  VOQ5     DECIMAL(7,0)   NOT NULL DEFAULT 0             , -- 68-87  Price break quantity 5
  VOC1     DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 88-117 Price break unit cost 1
  VOC2     DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 88-117 Price break unit cost 2
  VOC3     DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 88-117 Price break unit cost 3
  VOC4     DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 88-117 Price break unit cost 4
  VOC5     DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 88-117 Price break unit cost 5
  VOCYMD   DECIMAL(7,0)   NOT NULL DEFAULT 0             , -- 118-121 Last change CYMD
  VOUSER   CHAR(10)       NOT NULL DEFAULT ''            , -- 122-131 Last changed by
  VODEL    CHAR(1)        NOT NULL DEFAULT ''            , -- 132     'D' = deleted
  VOF1     DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 133-162 Price break freight 1
  VOF2     DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 133-162 Price break freight 2
  VOF3     DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 133-162 Price break freight 3
  VOF4     DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 133-162 Price break freight 4
  VOF5     DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 133-162 Price break freight 5
  VOA1     DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 163-192 Price break all-in cost 1
  VOA2     DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 163-192 Price break all-in cost 2
  VOA3     DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 163-192 Price break all-in cost 3
  VOA4     DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 163-192 Price break all-in cost 4
  VOA5     DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 163-192 Price break all-in cost 5
  VOX      CHAR(1)        NOT NULL DEFAULT ''            , -- 193     'X' = cancelled
  PRIMARY KEY (VOPART, VOOO, VONN, VOID, VOORAX)
)
RCDFMT VOFMT;

LABEL ON TABLE VENDPACO IS 'Vendor Part Cost by Organisation';
