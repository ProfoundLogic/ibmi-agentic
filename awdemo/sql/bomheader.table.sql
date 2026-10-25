--  AdelWiggins demo dataset - BOM_HEADER (one row per parent item / revision)
CREATE OR REPLACE TABLE BOM_HEADER (
  BOM_ID                                CHAR(10),
  PARENT_ITEM_ID FOR COLUMN PAREN00001  CHAR(10) NOT NULL,
  REVISION                              CHAR(2),
  EFFECTIVE_DATE FOR COLUMN EFFEC00001  DATE,
  PRIMARY KEY (BOM_ID),
  CONSTRAINT BOM_HEADER_PARENT_FK FOREIGN KEY (PARENT_ITEM_ID) REFERENCES ITEM (ITEM_ID)
);

LABEL ON COLUMN BOM_HEADER (
  BOM_ID         IS 'BOM Identifier',
  PARENT_ITEM_ID IS 'Parent Item ID',
  REVISION       IS 'BOM Revision',
  EFFECTIVE_DATE IS 'Effective From Date'
);
