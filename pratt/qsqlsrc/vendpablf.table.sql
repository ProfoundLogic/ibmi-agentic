-- VENDPABLF : Vendor Part Cost by SKU / warehouse / vendor.
-- WARNING - byte positions are load bearing.  PODTLUI overlays (via VENDPABL6):
--   VYXT CHAR(50) DIM(4) POS(152), VYQ PACKED(7) DIM(5) POS(411),
--   VYC PACKED(11:2) DIM(5) POS(431), VYF POS(476), VYA POS(506).
CREATE OR REPLACE TABLE VENDPABLF (
  VYSKU    CHAR(15)       NOT NULL DEFAULT ''            , -- 1-15   Internal SKU (key 1)
  VYDC     CHAR(2)        NOT NULL DEFAULT ''            , -- 16-17  Warehouse (key 2)
  VYVEND   DECIMAL(7,0)   NOT NULL DEFAULT 0             , -- 18-21  Vendor number (key 3)
  VYPART   CHAR(25)       NOT NULL DEFAULT ''            , -- 22-46  Vendor part number
  VYDESC   CHAR(30)       NOT NULL DEFAULT ''            , -- 47-76  Description line 1
  VYDESC2  CHAR(30)       NOT NULL DEFAULT ''            , -- 77-106 Description line 2
  VYUOM    CHAR(3)        NOT NULL DEFAULT ''            , -- 107-109 Unit of measure
  VYOVHD   DECIMAL(15,5)  NOT NULL DEFAULT 0             , -- 110-117 Overhead multiplier
  VYCCOD   DECIMAL(2,0)   NOT NULL DEFAULT 0             , -- 118-119 Cost code (numeric)
  VYSRCL   DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 121-126 Source freight (per load)
  VYSRCU   DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 127-132 Source surcharge (per unit)
  VYEXPD   DECIMAL(7,0)   NOT NULL DEFAULT 0             , -- 133-136 Expiry CYMD
  VYCHGD   DATE           NOT NULL DEFAULT '0001-01-01'  , -- 137-146 Last changed date
  VYDEL    CHAR(1)        NOT NULL DEFAULT ''            , -- 147     'D' = deleted
  VYFIL0   CHAR(5)        NOT NULL DEFAULT ''            , -- 147-151 Reserved
  VYXT1    CHAR(50)       NOT NULL DEFAULT ''            , -- 152-351 Extended text line 1
  VYXT2    CHAR(50)       NOT NULL DEFAULT ''            , -- 152-351 Extended text line 2
  VYXT3    CHAR(50)       NOT NULL DEFAULT ''            , -- 152-351 Extended text line 3
  VYXT4    CHAR(50)       NOT NULL DEFAULT ''            , -- 152-351 Extended text line 4
  VYUSER   CHAR(10)       NOT NULL DEFAULT ''            , -- 352-361 Last changed by
  VYFIL1   CHAR(49)       NOT NULL DEFAULT ''            , -- 362-410 Reserved
  VYQ1     DECIMAL(7,0)   NOT NULL DEFAULT 0             , -- 411-430 Price break quantity 1
  VYQ2     DECIMAL(7,0)   NOT NULL DEFAULT 0             , -- 411-430 Price break quantity 2
  VYQ3     DECIMAL(7,0)   NOT NULL DEFAULT 0             , -- 411-430 Price break quantity 3
  VYQ4     DECIMAL(7,0)   NOT NULL DEFAULT 0             , -- 411-430 Price break quantity 4
  VYQ5     DECIMAL(7,0)   NOT NULL DEFAULT 0             , -- 411-430 Price break quantity 5
  VYC1     DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 431-460 Price break unit cost 1
  VYC2     DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 431-460 Price break unit cost 2
  VYC3     DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 431-460 Price break unit cost 3
  VYC4     DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 431-460 Price break unit cost 4
  VYC5     DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 431-460 Price break unit cost 5
  VYFIL2   CHAR(15)       NOT NULL DEFAULT ''            , -- 461-475 Reserved
  VYF1     DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 476-505 Price break freight 1
  VYF2     DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 476-505 Price break freight 2
  VYF3     DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 476-505 Price break freight 3
  VYF4     DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 476-505 Price break freight 4
  VYF5     DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 476-505 Price break freight 5
  VYA1     DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 506-535 Price break all-in cost 1
  VYA2     DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 506-535 Price break all-in cost 2
  VYA3     DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 506-535 Price break all-in cost 3
  VYA4     DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 506-535 Price break all-in cost 4
  VYA5     DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 506-535 Price break all-in cost 5
  PRIMARY KEY (VYSKU, VYDC, VYVEND)
)
RCDFMT VYFMT;

LABEL ON TABLE VENDPABLF IS 'Vendor Part Cost by SKU';
