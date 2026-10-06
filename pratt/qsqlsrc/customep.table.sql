-- CUSTOMEP : Customer master (drop-down source for the PO ship-to customer)
CREATE OR REPLACE TABLE CUSTOMEP (
  CUNN     CHAR(2)        NOT NULL DEFAULT ''        , -- Plant (key 1)
  CUID     DECIMAL(5,0)   NOT NULL DEFAULT 0         , -- Customer id (key 2)
  CUNAME   CHAR(30)       NOT NULL DEFAULT ''        , -- Customer name
  CUDEL    CHAR(1)        NOT NULL DEFAULT ''        , -- ' '/'I' = selectable, 'D' = deleted
  PRIMARY KEY (CUNN, CUID)
)
RCDFMT CUFMT;

LABEL ON TABLE CUSTOMEP IS 'Customer Master';
