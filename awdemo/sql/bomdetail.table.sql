--  AdelWiggins demo dataset - BOM_DETAIL (component lines of a BOM)
CREATE OR REPLACE TABLE BOM_DETAIL (
  BOM_ID                                   CHAR(10),
  LINE_NO                                  INTEGER,
  COMPONENT_ITEM_ID FOR COLUMN COMPO00001  CHAR(10),
  QUANTITY                                 DECIMAL(10,3),
  PRIMARY KEY (BOM_ID, LINE_NO),
  CONSTRAINT BOM_DETAIL_BOM_FK  FOREIGN KEY (BOM_ID)            REFERENCES BOM_HEADER (BOM_ID),
  CONSTRAINT BOM_DETAIL_COMP_FK FOREIGN KEY (COMPONENT_ITEM_ID) REFERENCES ITEM (ITEM_ID)
);

LABEL ON COLUMN BOM_DETAIL (
  BOM_ID            IS 'BOM Identifier',
  LINE_NO           IS 'Line Number',
  COMPONENT_ITEM_ID IS 'Component Item ID',
  QUANTITY          IS 'Component Quantity'
);
