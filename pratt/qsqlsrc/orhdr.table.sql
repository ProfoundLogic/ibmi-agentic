-- ORHDR : Purchase Order Header
-- Reconstructed from field usage in PODTLUI/POSELUI (Pratt "puitests" bundle).
-- Record format OHFMT is pinned so native RPG I/O in PODTLUI resolves unchanged.
CREATE OR REPLACE TABLE ORHDR (
  OHYY     CHAR(2)        NOT NULL DEFAULT ''        , -- PO year (key 1)
  OH#      NUMERIC(6,0)   NOT NULL DEFAULT 0         , -- PO number (key 2)
  OHPP     CHAR(2)        NOT NULL DEFAULT ''        , -- Business area / plant
  OHDC     CHAR(2)        NOT NULL DEFAULT ''        , -- Destination warehouse
  OHSTAT   CHAR(1)        NOT NULL DEFAULT ''        , -- ' '=Open H=Hold C=Closed X=Cancelled
  OHCODE   CHAR(1)        NOT NULL DEFAULT ''        , -- I=Inventory D=Drop ship E=Ecom
  OHMETH   CHAR(1)        NOT NULL DEFAULT ''        , -- Y=Sent N=Not sent A=Auto
  OHVEND   DECIMAL(7,0)   NOT NULL DEFAULT 0         , -- Vendor number -> VENDMST.VNUMB
  OHFRT    CHAR(3)        NOT NULL DEFAULT ''        , -- Freight terms -> SAPINCO
  OHREQ    CHAR(10)       NOT NULL DEFAULT ''        , -- Created by (user profile)
  OHRDTE   DATE           NOT NULL DEFAULT '0001-01-01', -- Created date
  OHCHBY   CHAR(10)       NOT NULL DEFAULT ''        , -- Last changed by
  OHCHDT   DATE           NOT NULL DEFAULT '0001-01-01', -- Last changed date
  OHCHTM   TIME           NOT NULL DEFAULT '00.00.00', -- Last changed time
  OHNN     CHAR(2)        NOT NULL DEFAULT ''        , -- Ship-to customer plant
  OHCUID   DECIMAL(5,0)   NOT NULL DEFAULT 0         , -- Ship-to customer id
  OHLOC    CHAR(3)        NOT NULL DEFAULT ''        , -- Ship-to location code
  OHSHIP   CHAR(30)       NOT NULL DEFAULT ''        , -- Ship-to name
  OHSATN   CHAR(30)       NOT NULL DEFAULT ''        , -- Ship-to attention
  OHSADR   CHAR(30)       NOT NULL DEFAULT ''        , -- Ship-to street
  OHSCTY   CHAR(20)       NOT NULL DEFAULT ''        , -- Ship-to city
  OHSST    CHAR(2)        NOT NULL DEFAULT ''        , -- Ship-to state
  OHSZP5   DECIMAL(5,0)   NOT NULL DEFAULT 0         , -- Ship-to ZIP5
  OHSZP4   DECIMAL(4,0)   NOT NULL DEFAULT 0         , -- Ship-to ZIP+4
  OHSTCTRY CHAR(3)        NOT NULL DEFAULT ''        , -- Ship-to country
  OHSTPCD  CHAR(10)       NOT NULL DEFAULT ''        , -- Ship-to postal code (non-US)
  OHORDID  CHAR(20)       NOT NULL DEFAULT ''        , -- Ecom order id -> ORECOMM
  OHEDOC   DECIMAL(9,0)   NOT NULL DEFAULT 0         , -- EDI document number
  OHESTS   CHAR(1)        NOT NULL DEFAULT ''        , -- EDI status
  OHOBACT  CHAR(10)       NOT NULL DEFAULT ''        , -- Outbound account
  OHOBGRP  CHAR(10)       NOT NULL DEFAULT ''        , -- Outbound group
  OHX      CHAR(1)        NOT NULL DEFAULT ''        , -- audit flag, set to 'X' on save (NOT a delete flag)
  PRIMARY KEY (OHYY, OH#)
)
RCDFMT OHFMT;

LABEL ON TABLE ORHDR IS 'Purchase Order Header';
