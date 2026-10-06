-- ORMSR : Material / Service Requisition  (the file behind the OMMGR green screen)
CREATE OR REPLACE TABLE ORMSR (
  OMYY     CHAR(2)        NOT NULL DEFAULT ''        , -- Requisition year (key 1)
  OMREQ    DECIMAL(6,0)   NOT NULL DEFAULT 0         , -- Requisition number (key 2)
  OMPP     CHAR(2)        NOT NULL DEFAULT ''        , -- Business area / plant
  OMDC     CHAR(2)        NOT NULL DEFAULT ''        , -- Destination warehouse
  OMCODE   CHAR(1)        NOT NULL DEFAULT ''        , -- ' '=Sales E=Ecom I=Inventory D=Drop ship F=Freight
  OMSTAT   CHAR(1)        NOT NULL DEFAULT ''        , -- ' '=Approved A=Audit E=EDI audit C=PO created R=Rate set D=Deleted
  OMPART   CHAR(25)       NOT NULL DEFAULT ''        , -- Vendor part number
  OMDESC   CHAR(30)       NOT NULL DEFAULT ''        , -- Description
  OMQTY    DECIMAL(9,3)   NOT NULL DEFAULT 0         , -- Requested quantity
  OMUOM    CHAR(3)        NOT NULL DEFAULT ''        , -- Unit of measure
  OMEST$   DECIMAL(15,5)  NOT NULL DEFAULT 0         , -- Estimated unit cost
  OMVN#    DECIMAL(7,0)   NOT NULL DEFAULT 0         , -- Suggested vendor -> VENDMST.VNUMB
  OMCUPO   CHAR(25)       NOT NULL DEFAULT ''        , -- Customer PO reference
  OMBY     CHAR(10)       NOT NULL DEFAULT ''        , -- Requested by
  OMRQDT   DATE           NOT NULL DEFAULT '0001-01-01', -- Required date
  OMUSER   CHAR(10)       NOT NULL DEFAULT ''        , -- Entered by
  OMCDAT   DATE           NOT NULL DEFAULT '0001-01-01', -- Created date
  OMAPP    CHAR(10)       NOT NULL DEFAULT ''        , -- Approved by
  OMWO#    DECIMAL(10,0)  NOT NULL DEFAULT 0         , -- Work order: plant(2) + job(8)
  OMPOQR   DECIMAL(7,0)   NOT NULL DEFAULT 0         , -- WIP ship CYMD  -> WIPSHIP.WSYMD
  OMWSLC   CHAR(3)        NOT NULL DEFAULT ''        , -- WIP ship location -> WIPSHIP.WSLOC
  OMMSD#   NUMERIC(5,0)   NOT NULL DEFAULT 0         , -- WIP ship time -> WIPSHIP.WSTIME
  OMPOYY   CHAR(2)        NOT NULL DEFAULT ''        , -- Resulting PO year
  OMPO#    DECIMAL(6,0)   NOT NULL DEFAULT 0         , -- Resulting PO number
  OMPOPP   CHAR(2)        NOT NULL DEFAULT ''        , -- Resulting PO business area
  OMPOIT   CHAR(10)       NOT NULL DEFAULT ''        , -- Resulting PO line reference
  OMPOQ    DECIMAL(9,3)   NOT NULL DEFAULT 0         , -- Quantity placed on PO
  OMPOUM   CHAR(3)        NOT NULL DEFAULT ''        , -- PO unit of measure
  OMPOBY   CHAR(10)       NOT NULL DEFAULT ''        , -- PO created by
  OMPODT   DATE           NOT NULL DEFAULT '0001-01-01', -- PO created date
  OMPODD   DATE           NOT NULL DEFAULT '0001-01-01', -- PO due date
  PRIMARY KEY (OMYY, OMREQ)
)
RCDFMT OMFMT;

LABEL ON TABLE ORMSR IS 'Material/Service Requisition';
