--  AdelWiggins demo dataset - WAREHOUSE / stocking location master
CREATE OR REPLACE TABLE WAREHOUSE (
  WH_CODE                           CHAR(3),
  WH_NAME                           VARCHAR(40),
  LOCATION                          VARCHAR(100),
  ACTIVE_FLAG FOR COLUMN ACTIV00001 CHAR(1) DEFAULT 'Y',
  PRIMARY KEY (WH_CODE)
);

LABEL ON COLUMN WAREHOUSE (
  WH_CODE     IS 'Warehouse Code',
  WH_NAME     IS 'Warehouse Name',
  LOCATION    IS 'Warehouse Location',
  ACTIVE_FLAG IS 'Active (Y/N)'
);
