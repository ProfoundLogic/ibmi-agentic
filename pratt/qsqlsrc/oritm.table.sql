-- ORITM : Purchase Order Line Item
CREATE OR REPLACE TABLE ORITM (
  OIYY     CHAR(2)        NOT NULL DEFAULT ''        , -- PO year (key 1)
  OI#      NUMERIC(6,0)   NOT NULL DEFAULT 0         , -- PO number (key 2)
  OIPP     CHAR(2)        NOT NULL DEFAULT ''        , -- Business area (key 3)
  OILIN#   NUMERIC(3,0)   NOT NULL DEFAULT 0         , -- PO line number (key 4)
  OIPART   CHAR(25)       NOT NULL DEFAULT ''        , -- Vendor part number (key 5)
  OIDESC   CHAR(30)       NOT NULL DEFAULT ''        , -- Line description
  OISKU    CHAR(15)       NOT NULL DEFAULT ''        , -- Internal SKU / item
  OIUOM    CHAR(3)        NOT NULL DEFAULT ''        , -- Unit of measure
  OIQTY    DECIMAL(9,3)   NOT NULL DEFAULT 0         , -- Ordered quantity
  OIRQTY   DECIMAL(9,3)   NOT NULL DEFAULT 0         , -- Received quantity
  OIBQTY   DECIMAL(9,3)   NOT NULL DEFAULT 0         , -- Balance quantity
  OIAKQY   DECIMAL(9,3)   NOT NULL DEFAULT 0         , -- Acknowledged quantity
  OIAKDT   DATE           NOT NULL DEFAULT '0001-01-01', -- Acknowledged date
  OIUNIT   DECIMAL(15,5)  NOT NULL DEFAULT 0         , -- Unit cost
  OIDDAT   DATE           NOT NULL DEFAULT '0001-01-01', -- Due date at Pratt
  OISTAT   CHAR(1)        NOT NULL DEFAULT ''        , -- ' '=Open C=Closed D=Deleted X=Cancelled
  OIREQ    DECIMAL(6,0)   NOT NULL DEFAULT 0         , -- Source requisition -> ORMSR.OMREQ
  OICSTC   CHAR(7)        NOT NULL DEFAULT ''        , -- Cost centre
  OIGLACT  NUMERIC(6,0)   NOT NULL DEFAULT 0         , -- G/L account
  OIMTLG   CHAR(7)        NOT NULL DEFAULT ''        , -- Material group
  OIMTLN   CHAR(9)        NOT NULL DEFAULT ''        , -- Material number
  OIPURG   NUMERIC(3,0)   NOT NULL DEFAULT 0         , -- Purchasing group
  OIWSNN   CHAR(2)        NOT NULL DEFAULT ''        , -- WIP ship plant
  OIWSJOB  DECIMAL(8,0)   NOT NULL DEFAULT 0         , -- WIP job number
  OIWSLOC  CHAR(3)        NOT NULL DEFAULT ''        , -- WIP ship location
  OIWSYMD  DECIMAL(7,0)   NOT NULL DEFAULT 0         , -- WIP ship CYMD
  OIWSTIME DECIMAL(5,0)   NOT NULL DEFAULT 0         , -- WIP ship time
  OICHBY   CHAR(10)       NOT NULL DEFAULT ''        , -- Last changed by
  OICHDT   DATE           NOT NULL DEFAULT '0001-01-01', -- Last changed date
  OICHTM   TIME           NOT NULL DEFAULT '00.00.00', -- Last changed time
  OIX      CHAR(1)        NOT NULL DEFAULT ''        , -- audit flag, set to 'X' on save (NOT a delete flag)
  PRIMARY KEY (OIYY, OI#, OIPP, OILIN#, OIPART)
)
RCDFMT OIFMT;

LABEL ON TABLE ORITM IS 'Purchase Order Line Item';
