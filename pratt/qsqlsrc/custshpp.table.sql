-- CUSTSHPP : Customer ship-to master
CREATE OR REPLACE TABLE CUSTSHPP (
  CSNN     CHAR(2)        NOT NULL DEFAULT ''        , -- Plant (key 1)
  CSID     DECIMAL(5,0)   NOT NULL DEFAULT 0         , -- Customer id (key 2)
  CSLOC    CHAR(3)        NOT NULL DEFAULT ''        , -- Location (key 3)
  CSNAME   CHAR(30)       NOT NULL DEFAULT ''        , -- Ship-to name
  CSADDR   CHAR(30)       NOT NULL DEFAULT ''        , -- Ship-to street
  CSCITY   CHAR(20)       NOT NULL DEFAULT ''        , -- Ship-to city
  CSST     CHAR(2)        NOT NULL DEFAULT ''        , -- Ship-to state
  CSZIP5   DECIMAL(5,0)   NOT NULL DEFAULT 0         , -- Ship-to ZIP5
  CSZIP4   DECIMAL(4,0)   NOT NULL DEFAULT 0         , -- Ship-to ZIP+4
  CSCTRY   CHAR(3)        NOT NULL DEFAULT ''        , -- Country
  CSPOCD   CHAR(10)       NOT NULL DEFAULT ''        , -- Non-US postal code
  CSORAX   CHAR(3)        NOT NULL DEFAULT ''        , -- Ship-to axis -> VENDPACO.VOORAX
  CSR01    CHAR(2)        NOT NULL DEFAULT ''        , -- Related warehouse -> ORHDR.OHDC
  CSMAXO   DECIMAL(9,0)   NOT NULL DEFAULT 0         , -- Maximum order value
  CSX01    CHAR(1)        NOT NULL DEFAULT ''        , -- 'T' = third party / transfer
  CSDEL    CHAR(1)        NOT NULL DEFAULT ''        , -- 'D' = deleted
  PRIMARY KEY (CSNN, CSID, CSLOC)
)
RCDFMT CSFMT;

LABEL ON TABLE CUSTSHPP IS 'Customer Ship-To Master';
