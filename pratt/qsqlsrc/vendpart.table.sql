-- VENDPART : Vendor Part Master.
-- WARNING - byte positions are load bearing.  PODTLUI overlays this record with
--   MAJMIN CHAR(6) POS(103), UOM CHAR(4) DIM(5) POS(310), QTY PACKED(7) DIM(5) POS(372),
--   CST PACKED(11:2) DIM(5) POS(392), VRF PACKED(11:2) DIM(5) POS(437), VRA POS(467).
-- Buffer sizes: CHAR(n)=n, DATE=10, DECIMAL(p,s)=CEIL((p+1)/2).  Verified by DSPFFD.
CREATE OR REPLACE TABLE VENDPART (
  VRPART   CHAR(25)       NOT NULL DEFAULT ''            , -- 1-25   Vendor part number (key)
  VRNN     CHAR(2)        NOT NULL DEFAULT ''            , -- 26-27  Owning plant
  VRID     DECIMAL(5,0)   NOT NULL DEFAULT 0             , -- 28-30  Customer / owner id
  VRIT     CHAR(15)       NOT NULL DEFAULT ''            , -- 31-45  Internal item / SKU
  VQDESC   CHAR(30)       NOT NULL DEFAULT ''            , -- 46-75  Description (drop-down text)
  VRDES2   CHAR(27)       NOT NULL DEFAULT ''            , -- 76-102 Description line 2
  VRMAJ    CHAR(3)        NOT NULL DEFAULT ''            , -- 103-105 Major code  } overlaid as MAJMIN CHAR(6)
  VRMIN    CHAR(3)        NOT NULL DEFAULT ''            , -- 106-108 Minor code  }
  VRUNIQ   DECIMAL(9,0)   NOT NULL DEFAULT 0             , -- 109-113 Units per package
  VRLEAD   DECIMAL(3,0)   NOT NULL DEFAULT 0             , -- 114-115 Lead time in days
  VRPCT    DECIMAL(5,2)   NOT NULL DEFAULT 0             , -- 116-118 Allowed over-receipt percent
  VRPER1   CHAR(4)        NOT NULL DEFAULT ''            , -- 119-122 Default unit of measure
  VROVHD   DECIMAL(15,5)  NOT NULL DEFAULT 0             , -- 123-130 Overhead multiplier
  VRUPTL   DECIMAL(15,5)  NOT NULL DEFAULT 0             , -- 131-138 Unit price total
  VRCCOD   DECIMAL(2,0)   NOT NULL DEFAULT 0             , -- 139-140 Cost code (numeric)
  VRSRCL   DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 142-147 Source freight (per load)
  VRSRCU   DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 148-153 Source surcharge (per unit)
  VRORIG   CHAR(3)        NOT NULL DEFAULT ''            , -- 154-156 Country of origin
  VREFFD   DATE           NOT NULL DEFAULT '0001-01-01'  , -- 157-166 Effective date
  VREXPD   DECIMAL(7,0)   NOT NULL DEFAULT 0             , -- 167-170 Expiry CYMD
  VRCYMD   DECIMAL(7,0)   NOT NULL DEFAULT 0             , -- 171-174 Last change CYMD
  VRUSER   CHAR(10)       NOT NULL DEFAULT ''            , -- 175-184 Last changed by
  VRDEL    CHAR(1)        NOT NULL DEFAULT ''            , -- 185     'D' = deleted
  VRFILL1  CHAR(125)      NOT NULL DEFAULT ''            , -- 185-309 Reserved
  VRUOM1   CHAR(4)        NOT NULL DEFAULT ''            , -- 310-329 Price break UOM 1
  VRUOM2   CHAR(4)        NOT NULL DEFAULT ''            , -- 310-329 Price break UOM 2
  VRUOM3   CHAR(4)        NOT NULL DEFAULT ''            , -- 310-329 Price break UOM 3
  VRUOM4   CHAR(4)        NOT NULL DEFAULT ''            , -- 310-329 Price break UOM 4
  VRUOM5   CHAR(4)        NOT NULL DEFAULT ''            , -- 310-329 Price break UOM 5
  VRFILL2  CHAR(42)       NOT NULL DEFAULT ''            , -- 330-371 Reserved
  VRQTY1   DECIMAL(7,0)   NOT NULL DEFAULT 0             , -- 372-391 Price break quantity 1
  VRQTY2   DECIMAL(7,0)   NOT NULL DEFAULT 0             , -- 372-391 Price break quantity 2
  VRQTY3   DECIMAL(7,0)   NOT NULL DEFAULT 0             , -- 372-391 Price break quantity 3
  VRQTY4   DECIMAL(7,0)   NOT NULL DEFAULT 0             , -- 372-391 Price break quantity 4
  VRQTY5   DECIMAL(7,0)   NOT NULL DEFAULT 0             , -- 372-391 Price break quantity 5
  VRCST1   DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 392-421 Price break unit cost 1
  VRCST2   DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 392-421 Price break unit cost 2
  VRCST3   DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 392-421 Price break unit cost 3
  VRCST4   DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 392-421 Price break unit cost 4
  VRCST5   DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 392-421 Price break unit cost 5
  VRFILL3  CHAR(15)       NOT NULL DEFAULT ''            , -- 422-436 Reserved
  VRFRT1   DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 437-466 Price break freight 1
  VRFRT2   DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 437-466 Price break freight 2
  VRFRT3   DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 437-466 Price break freight 3
  VRFRT4   DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 437-466 Price break freight 4
  VRFRT5   DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 437-466 Price break freight 5
  VRADD1   DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 467-496 Price break all-in cost 1
  VRADD2   DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 467-496 Price break all-in cost 2
  VRADD3   DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 467-496 Price break all-in cost 3
  VRADD4   DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 467-496 Price break all-in cost 4
  VRADD5   DECIMAL(11,2)  NOT NULL DEFAULT 0             , -- 467-496 Price break all-in cost 5
  PRIMARY KEY (VRPART)
)
RCDFMT VRFMT;

LABEL ON TABLE VENDPART IS 'Vendor Part Master';
