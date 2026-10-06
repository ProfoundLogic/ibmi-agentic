-- MASTSPEP : Item specification / standard pack
CREATE OR REPLACE TABLE MASTSPEP (
  MQNN     CHAR(2)        NOT NULL DEFAULT ''        , -- Plant (key 1)
  MQID     DECIMAL(5,0)   NOT NULL DEFAULT 0         , -- Customer id (key 2)
  MQIT     CHAR(15)       NOT NULL DEFAULT ''        , -- Item (key 3)
  MQ#OUT   DECIMAL(7,0)   NOT NULL DEFAULT 0         , -- Number out per sheet
  MQ#PCS   DECIMAL(7,0)   NOT NULL DEFAULT 0         , -- Pieces per unit
  MQMLT    DECIMAL(9,3)   NOT NULL DEFAULT 1         , -- Cost multiplier (never 0)
  MQCOST   DECIMAL(15,5)  NOT NULL DEFAULT 0         , -- Standard cost
  MQCCOD   DECIMAL(2,0)   NOT NULL DEFAULT 0         , -- Cost code
  MQSYMD   DECIMAL(7,0)   NOT NULL DEFAULT 0         , -- Last change CYMD
  MQUSER   CHAR(10)       NOT NULL DEFAULT ''        , -- Last changed by
  MQUSEX   DECIMAL(9,3)   NOT NULL DEFAULT 0         , -- Cost multiplier (extra)
  MQX      CHAR(1)        NOT NULL DEFAULT ''        , -- 'X' = cancelled
  PRIMARY KEY (MQNN, MQID, MQIT)
)
RCDFMT MQFMT;

LABEL ON TABLE MASTSPEP IS 'Item Specification / Standard Pack';
