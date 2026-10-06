-- WIPSHIP : Work-in-process shipments
CREATE OR REPLACE TABLE WIPSHIP (
  WSNN     CHAR(2)        NOT NULL DEFAULT ''        , -- Plant (key 1)
  WSJOB    DECIMAL(8,0)   NOT NULL DEFAULT 0         , -- Job number (key 2)
  WSWH     CHAR(2)        NOT NULL DEFAULT ''        , -- Warehouse (key 3)
  WSYMD    DECIMAL(7,0)   NOT NULL DEFAULT 0         , -- Ship CYMD (key 4)
  WSLOC    CHAR(3)        NOT NULL DEFAULT ''        , -- Ship location (key 5)
  WSTIME   NUMERIC(5,0)   NOT NULL DEFAULT 0         , -- Ship time (key 6)
  WSID     DECIMAL(5,0)   NOT NULL DEFAULT 0         , -- Customer id
  WSIT     CHAR(15)       NOT NULL DEFAULT ''        , -- Item
  WSLIN#   CHAR(3)        NOT NULL DEFAULT ''        , -- Line number (edited)
  WSLOCV   CHAR(3)        NOT NULL DEFAULT ''        , -- Verified location
  WSPO     CHAR(25)       NOT NULL DEFAULT ''        , -- Customer PO
  WSPCOD   DECIMAL(2,0)   NOT NULL DEFAULT 0         , -- Pack code
  WSQTY    DECIMAL(9,3)   NOT NULL DEFAULT 0         , -- Shipped quantity
  WSPRIQ   DECIMAL(9,3)   NOT NULL DEFAULT 0         , -- Prior quantity
  WSQ1     DECIMAL(9,3)   NOT NULL DEFAULT 0         , -- Quantity bucket 1
  WSP1     DECIMAL(9,3)   NOT NULL DEFAULT 0         , -- Pallet bucket 1
  WSSCH2   CHAR(1)        NOT NULL DEFAULT ''        , -- 'F' = firm schedule
  WSTO     CHAR(10)       NOT NULL DEFAULT ''        , -- Ship to reference
  WSCYMD   DECIMAL(7,0)   NOT NULL DEFAULT 0         , -- Last change CYMD
  WSCHMS   DECIMAL(6,0)   NOT NULL DEFAULT 0         , -- Last change HHMMSS
  WSUSER   CHAR(10)       NOT NULL DEFAULT ''        , -- Last changed by
  WSDEL    CHAR(1)        NOT NULL DEFAULT ''        , -- 'D' = deleted
  WSX      CHAR(1)        NOT NULL DEFAULT ''        , -- 'X' = cancelled
  PRIMARY KEY (WSNN, WSJOB, WSWH, WSYMD, WSLOC, WSTIME)
)
RCDFMT WSFMT;

LABEL ON TABLE WIPSHIP IS 'Work In Process Shipments';
