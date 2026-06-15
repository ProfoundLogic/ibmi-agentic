-- ============================================================================
-- HYPSGCU - Shipment Group / Customer
-- ----------------------------------------------------------------------------
-- Skeleton. Field list derived from:
--   * LF keys in HYLSGCU1..14 (GCGRP#, GCCPTY, GCBLTO, GCSHTO, GCDSHP, GCSTNM)
--   * DSPF REFFLD() in HYD0600A / HYD0600B
--   * HYR0600.SQLRPGLE token usage of GC* names
-- Record format must be HYRSGCU (LFs rename to it). All char fields are
-- CCSID 37; types inferred — verify against the live Hornady IBM i.
-- ============================================================================
CREATE TABLE AITSK00030/HYPSGCU (
  -- Keys (from LFs)
  GCGRP#  DECIMAL(7,0)  NOT NULL DEFAULT 0,   -- Shipment group number  (key)
  GCCPTY  DECIMAL(7,0)  NOT NULL DEFAULT 0,   -- Counter-party customer
  GCBLTO  DECIMAL(7,0)  NOT NULL DEFAULT 0,   -- Bill-to customer
  GCSHTO  DECIMAL(7,0)  NOT NULL DEFAULT 0,   -- Ship-to customer
  GCDSHP  DECIMAL(7,0)  NOT NULL DEFAULT 0,   -- Drop-ship customer

  -- Customer / address (DSPF REFFLDs in HYD0600A)
  GCBTNM  CHAR(30)      NOT NULL DEFAULT '',  -- Bill-to name
  GCSTNM  CHAR(30)      NOT NULL DEFAULT '',  -- Ship-to name
  GCSTZP  CHAR(9)       NOT NULL DEFAULT '',  -- Ship-to zip

  -- Ship-via / freight (HYR0600 GC* usage)
  GCSHPV  CHAR(6)       NOT NULL DEFAULT '',  -- Ship-via code
  GCSVDS  CHAR(30)      NOT NULL DEFAULT '',  -- Ship-via description
  GCFRZN  CHAR(1)       NOT NULL DEFAULT '',  -- Freight zone flag
  GCFRTA  DECIMAL(11,2) NOT NULL DEFAULT 0,   -- Freight amount
  GCFTRM  CHAR(6)       NOT NULL DEFAULT '',  -- Freight terms
  GCCHGA  DECIMAL(11,2) NOT NULL DEFAULT 0,   -- Charges amount

  -- Shipment metrics
  GCITCT  DECIMAL(7,0)  NOT NULL DEFAULT 0,   -- Item count
  GCSTCT  DECIMAL(7,0)  NOT NULL DEFAULT 0,   -- Sub-total count
  GCSHWT  DECIMAL(11,2) NOT NULL DEFAULT 0,   -- Shipment weight

  -- Status / type
  GCSTS   CHAR(1)       NOT NULL DEFAULT '',  -- Status code
  GCSTST  CHAR(1)       NOT NULL DEFAULT '',  -- Sub-status
  GCSTYP  CHAR(1)       NOT NULL DEFAULT '',  -- Shipment type
  GCPIKO  CHAR(1)       NOT NULL DEFAULT '',  -- Pick-only flag
  GCPANS  CHAR(1)       NOT NULL DEFAULT '',  -- Pallet-ANS flag
  GCSPPK  CHAR(1)       NOT NULL DEFAULT '',  -- Special-pack flag

  -- Tracking
  GCPRO#  CHAR(20)      NOT NULL DEFAULT '',  -- PRO number (LTL)
  GCBOL#  DECIMAL(8,0)  NOT NULL DEFAULT 0,   -- Bill-of-lading number

  -- Lanes (HYR0600 GCLANE1/2/3/S)
  GCLANE1 CHAR(2)       NOT NULL DEFAULT '',  -- Lane 1
  GCLANE2 CHAR(2)       NOT NULL DEFAULT '',  -- Lane 2
  GCLANE3 CHAR(2)       NOT NULL DEFAULT '',  -- Lane 3
  GCLANES CHAR(6)       NOT NULL DEFAULT '',  -- Lane summary

  -- Misc
  GCMLLC  CHAR(1)       NOT NULL DEFAULT '',  -- Multi-LL carrier flag
  GCMLLI  CHAR(1)       NOT NULL DEFAULT '',  -- Multi-LL inbound flag
  GCHCFLD CHAR(1)       NOT NULL DEFAULT '',  -- Hardcoded freight flag

  -- Timestamps
  GCTSTP1 TIMESTAMP     NOT NULL DEFAULT CURRENT TIMESTAMP, -- Create stamp
  GCTSTP2 TIMESTAMP     NOT NULL DEFAULT CURRENT TIMESTAMP, -- Last update

  PRIMARY KEY ( GCGRP#, GCCPTY, GCBLTO, GCSHTO, GCDSHP )
) RCDFMT HYRSGCU;

LABEL ON TABLE AITSK00030/HYPSGCU IS 'Shipment Group / Customer';
