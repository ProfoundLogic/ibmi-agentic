--  AdelWiggins demo dataset - MANUFACTURING_ORDERS (shop orders)
CREATE OR REPLACE TABLE MANUFACTURING_ORDERS FOR SYSTEM NAME MANUF00001 (
  ORDER_ID                                            CHAR(10),
  ITEM_ID                                             CHAR(10) NOT NULL,
  ORDER_DATE                                          DATE NOT NULL,
  EXPECTED_COMPLETION_DATE FOR COLUMN EXPEC00001      DATE DEFAULT NULL,
  ACTUAL_COMPLETION_DATE   FOR COLUMN ACTUA00001      DATE DEFAULT NULL,
  QUANTITY_ORDERED         FOR COLUMN QUANT00001      DECIMAL(10,2),
  QUANTITY_SCRAPPED        FOR COLUMN QUANT00002      DECIMAL(10,2),
  STATUS                                              CHAR(1),
  SHIFT_CODE                                          CHAR(1),
  OPERATOR_ID              FOR COLUMN OPERA00001      CHAR(6),
  NOTES                                               VARCHAR(200),
  PRIMARY KEY (ORDER_ID),
  CONSTRAINT MFG_ORDERS_ITEM_FK FOREIGN KEY (ITEM_ID) REFERENCES ITEM (ITEM_ID)
);

LABEL ON COLUMN MANUFACTURING_ORDERS (
  ORDER_ID                 IS 'Manufacturing Order ID',
  ITEM_ID                  IS 'Manufactured Item',
  ORDER_DATE               IS 'Order Date',
  EXPECTED_COMPLETION_DATE IS 'Expected            Completion          Date',
  ACTUAL_COMPLETION_DATE   IS 'Actual              Completion          Date',
  QUANTITY_ORDERED         IS 'Quantity Ordered',
  QUANTITY_SCRAPPED        IS 'Quantity Scrapped',
  STATUS                   IS 'Order Status (P/R/C)',
  SHIFT_CODE               IS 'Shift Code',
  OPERATOR_ID              IS 'Operator ID',
  NOTES                    IS 'Order Notes'
);
