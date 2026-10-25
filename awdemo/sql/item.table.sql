--  AdelWiggins demo dataset - ITEM master
--  Schema is a faithful clone of library BOMSAMPLE so that any program written
--  against BOMSAMPLE runs unchanged against this library.  System column names
--  are pinned with FOR COLUMN so the short names match BOMSAMPLE exactly.
CREATE OR REPLACE TABLE ITEM (
  ITEM_ID                             CHAR(10),
  ITEM_DESC                           VARCHAR(50),
  ITEM_TYPE                           CHAR(1) NOT NULL,
  UNIT_OF_MEASURE FOR COLUMN UNIT_00001   CHAR(3),
  COST                                DECIMAL(10,2),
  ACTIVE_FLAG     FOR COLUMN ACTIV00001   CHAR(1) DEFAULT 'Y',
  PRIMARY KEY (ITEM_ID)
);

LABEL ON COLUMN ITEM (
  ITEM_ID         IS 'Item Code',
  ITEM_DESC       IS 'Item Description',
  ITEM_TYPE       IS 'P=Purchased, M=Manufactured',
  UNIT_OF_MEASURE IS 'Unit of Measure',
  COST            IS 'Unit Cost',
  ACTIVE_FLAG     IS 'Is Active (Y/N)'
);
