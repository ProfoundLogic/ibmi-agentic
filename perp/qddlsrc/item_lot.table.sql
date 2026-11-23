-- ---------------------------------------------------------------------------
-- Table:   item_lot  (system name ITMLOT)
-- Module:  perp
-- Purpose: Lot balances for lot-controlled items (Option C — denormalized
--          alongside item.qty_on_hand so drift is possible by design; the
--          reconciliation demo (PERP-24) surfaces and resolves it).
-- Epic:    PERP-3 / PERP-20 (Inventory Master)
-- ---------------------------------------------------------------------------

-- 'item_lot' is itself a valid <=10-char system name; any explicit FOR
-- SYSTEM NAME here raises SQL7029 regardless of value. Real object name
-- (verified via DSPOBJD) is PERPDEMO/ITEM_LOT -- see the note in
-- item_class.table.sql for why the SQL7029 message text is misleading.
CREATE TABLE item_lot (

  -- Composite key -----------------------------------------------------------
  company_code   FOR COLUMN COMPCD  CHAR(3)       NOT NULL,
  item_number    FOR COLUMN ITMNBR  VARCHAR(25)   NOT NULL,
  lot_number     FOR COLUMN LOTNBR  VARCHAR(20)   NOT NULL,

  -- Lot balance and dates ---------------------------------------------------
  qty_on_hand    FOR COLUMN QTYOH   DECIMAL(15,4) NOT NULL DEFAULT 0,
  received_date  FOR COLUMN RCVDT   DATE          NOT NULL DEFAULT CURRENT_DATE,
  expiry_date    FOR COLUMN EXPDT   DATE,

  -- Standard audit block --------------------------------------------------
  created_at     FOR COLUMN CRTAT   TIMESTAMP     NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_by     FOR COLUMN CRTBY   VARCHAR(18)   NOT NULL DEFAULT USER,
  updated_at     FOR COLUMN UPDAT   TIMESTAMP     NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_by     FOR COLUMN UPDBY   VARCHAR(18)   NOT NULL DEFAULT USER,
  is_active      FOR COLUMN ISACT   CHAR(1)       NOT NULL DEFAULT 'Y',

  -- Composite primary key ---------------------------------------------------
  PRIMARY KEY (company_code, item_number, lot_number),

  -- Constraints ---------------------------------------------------------------
  CONSTRAINT itmlot_isact_ck CHECK (is_active IN ('Y','N')),
  CONSTRAINT itmlot_qtyoh_ck CHECK (qty_on_hand >= 0),
  CONSTRAINT itmlot_dt_ck    CHECK (expiry_date IS NULL OR expiry_date >= received_date),

  -- FK to item ------------------------------------------------------------
  CONSTRAINT itmlot_item_fk FOREIGN KEY (company_code, item_number)
    REFERENCES item (company_code, item_number)
    ON DELETE RESTRICT ON UPDATE RESTRICT
);

LABEL ON TABLE item_lot IS
  'PERP item lot balances (reconciliation demo)';

LABEL ON COLUMN item_lot (
  company_code  IS 'Company code (FK to item)',
  item_number   IS 'Item number (FK to item)',
  lot_number    IS 'Lot number',
  qty_on_hand   IS 'Lot quantity on hand',
  received_date IS 'Date lot was received',
  expiry_date   IS 'Lot expiry date (nullable)',
  is_active     IS 'Active flag (Y/N, soft delete)'
);
