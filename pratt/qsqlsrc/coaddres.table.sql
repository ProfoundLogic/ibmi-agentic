-- COADDRES : Company / plant / warehouse master, keyed on the 2 character plant code.
-- COADEL carries a type as well as a delete flag: 'S'/'W' = selectable warehouse, 'D' = deleted.
CREATE OR REPLACE TABLE COADDRES (
  COAPLT   CHAR(2)        NOT NULL DEFAULT ''        , -- Plant / warehouse code (key)
  COANAM   CHAR(30)       NOT NULL DEFAULT ''        , -- Plant name
  SHORTNAME CHAR(15)      NOT NULL DEFAULT ''        , -- Short name used in drop-downs
  STREET   CHAR(30)       NOT NULL DEFAULT ''        , -- Street address
  STCTST   CHAR(40)       NOT NULL DEFAULT ''        , -- 'City, ST  99999-9999' parsed by PODTLUI
  ORG      CHAR(4)        NOT NULL DEFAULT ''        , -- Purchasing organisation -> VENDPACO.VOOO
  LCACCT   DECIMAL(5,0)   NOT NULL DEFAULT 0         , -- Location account -> WIPJOBS.WPID
  PAYOTQ   CHAR(10)       NOT NULL DEFAULT ''        , -- Cost centre, numeric 700000-810999
  COAJOB   DECIMAL(8,0)   NOT NULL DEFAULT 0         , -- Default job number
  COAPO#   DECIMAL(6,0)   NOT NULL DEFAULT 0         , -- Last PO number issued
  COADEL   CHAR(1)        NOT NULL DEFAULT ''        , -- 'S'/'W' = warehouse, 'D' = deleted
  PRIMARY KEY (COAPLT)
)
RCDFMT COAFMT;

LABEL ON TABLE COADDRES IS 'Company / Plant / Warehouse Master';
