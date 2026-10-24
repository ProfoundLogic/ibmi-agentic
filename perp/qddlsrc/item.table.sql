-- ---------------------------------------------------------------------------
-- Table:   item  (system name ITEM)
-- Module:  perp
-- Purpose: Core item master. Balances (Option C), reorder parameters,
--          single location, UOM pair, and the lot_controlled flag that
--          drives item_lot behaviour. Items are not shared across
--          companies.
-- Epic:    PERP-3 / PERP-20 (Inventory Master)
-- ---------------------------------------------------------------------------

-- 'item' auto-derives to system name ITEM; specifying FOR SYSTEM NAME with
-- the same value raises SQL7029, so it's omitted (same rule as company.sql).
CREATE TABLE item (

  -- Composite key (per-company) ---------------------------------------------
  company_code     FOR COLUMN COMPCD  CHAR(3)       NOT NULL,
  item_number      FOR COLUMN ITMNBR  VARCHAR(25)   NOT NULL,

  -- Descriptive columns -------------------------------------------------------
  item_description FOR COLUMN ITMDSC  VARCHAR(60)   NOT NULL,
  short_description FOR COLUMN SHTDSC VARCHAR(20)   NOT NULL DEFAULT '',

  -- Classification (FK to item_class) ------------------------------------
  class_code       FOR COLUMN CLSCD   VARCHAR(10)   NOT NULL,

  -- UOMs (FK to uom) ----------------------------------------------------------
  inventory_uom    FOR COLUMN INVUOM  VARCHAR(5)    NOT NULL,
  stocking_uom     FOR COLUMN STKUOM  VARCHAR(5)    NOT NULL,

  -- Lot control flag — drives item_lot behaviour -----------------------------
  lot_controlled   FOR COLUMN LOTCTL  CHAR(1)       NOT NULL DEFAULT 'N',

  -- Balances — Option C: also denormalized onto item_lot for lot-controlled
  -- items, so drift is possible by design (the reconciliation demo) --------
  qty_on_hand      FOR COLUMN QTYOH   DECIMAL(15,4) NOT NULL DEFAULT 0,
  qty_available    FOR COLUMN QTYAVL  DECIMAL(15,4) NOT NULL DEFAULT 0,
  qty_frozen       FOR COLUMN QTYFRZ  DECIMAL(15,4) NOT NULL DEFAULT 0,
  qty_on_order     FOR COLUMN QTYOO   DECIMAL(15,4) NOT NULL DEFAULT 0,

  -- Reorder parameters ----------------------------------------------------
  reorder_point    FOR COLUMN RORDPT  DECIMAL(15,4) NOT NULL DEFAULT 0,
  critical_level   FOR COLUMN CRITLV  DECIMAL(15,4) NOT NULL DEFAULT 0,
  min_qty          FOR COLUMN MINQTY  DECIMAL(15,4) NOT NULL DEFAULT 0,
  max_qty          FOR COLUMN MAXQTY  DECIMAL(15,4) NOT NULL DEFAULT 0,
  safety_stock     FOR COLUMN SAFSTK  DECIMAL(15,4) NOT NULL DEFAULT 0,
  lead_time_days   FOR COLUMN LEADTM  INTEGER       NOT NULL DEFAULT 0,

  -- Single location (aisle/bay/shelf) --------------------------------------
  aisle_code       FOR COLUMN AISLE   VARCHAR(10)   NOT NULL DEFAULT '',
  bay_code         FOR COLUMN BAY     VARCHAR(10)   NOT NULL DEFAULT '',
  shelf_code       FOR COLUMN SHELF   VARCHAR(10)   NOT NULL DEFAULT '',

  -- Standard audit block --------------------------------------------------
  created_at       FOR COLUMN CRTAT   TIMESTAMP     NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_by       FOR COLUMN CRTBY   VARCHAR(18)   NOT NULL DEFAULT USER,
  updated_at       FOR COLUMN UPDAT   TIMESTAMP     NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_by       FOR COLUMN UPDBY   VARCHAR(18)   NOT NULL DEFAULT USER,
  is_active        FOR COLUMN ISACT   CHAR(1)       NOT NULL DEFAULT 'Y',

  -- Composite primary key ---------------------------------------------------
  PRIMARY KEY (company_code, item_number),

  -- Constraints ---------------------------------------------------------------
  CONSTRAINT item_isact_ck   CHECK (is_active IN ('Y','N')),
  CONSTRAINT item_lotctl_ck  CHECK (lot_controlled IN ('Y','N')),
  CONSTRAINT item_qtyoh_ck   CHECK (qty_on_hand >= 0),
  CONSTRAINT item_qtyavl_ck  CHECK (qty_available >= 0),
  CONSTRAINT item_qtyfrz_ck  CHECK (qty_frozen >= 0),
  CONSTRAINT item_qtyoo_ck   CHECK (qty_on_order >= 0),
  CONSTRAINT item_rordpt_ck  CHECK (reorder_point >= 0),
  CONSTRAINT item_minqty_ck  CHECK (min_qty >= 0),
  CONSTRAINT item_maxqty_ck  CHECK (max_qty >= min_qty),
  CONSTRAINT item_safstk_ck  CHECK (safety_stock >= 0),
  CONSTRAINT item_leadtm_ck  CHECK (lead_time_days >= 0),

  -- FKs -----------------------------------------------------------------------
  CONSTRAINT item_company_fk FOREIGN KEY (company_code)
    REFERENCES company (company_code)
    ON DELETE RESTRICT ON UPDATE RESTRICT,
  CONSTRAINT item_class_fk FOREIGN KEY (company_code, class_code)
    REFERENCES item_class (company_code, class_code)
    ON DELETE RESTRICT ON UPDATE RESTRICT,
  CONSTRAINT item_invuom_fk FOREIGN KEY (inventory_uom)
    REFERENCES uom (uom_code)
    ON DELETE RESTRICT ON UPDATE RESTRICT,
  CONSTRAINT item_stkuom_fk FOREIGN KEY (stocking_uom)
    REFERENCES uom (uom_code)
    ON DELETE RESTRICT ON UPDATE RESTRICT
);

LABEL ON TABLE item IS
  'PERP core item master';

LABEL ON COLUMN item (
  company_code       IS 'Company code (FK to company)',
  item_number        IS 'Item number (natural key within company)',
  item_description   IS 'Item description',
  short_description  IS 'Short description (list / column display)',
  class_code         IS 'Item class (FK to item_class)',
  inventory_uom      IS 'Inventory UOM (FK to uom)',
  stocking_uom       IS 'Stocking UOM (FK to uom)',
  lot_controlled     IS 'Lot controlled flag (Y/N)',
  qty_on_hand        IS 'Quantity on hand',
  qty_available      IS 'Quantity available',
  qty_frozen         IS 'Quantity frozen',
  qty_on_order       IS 'Quantity on order',
  reorder_point      IS 'Reorder point',
  critical_level     IS 'Critical stock level',
  min_qty            IS 'Minimum quantity',
  max_qty            IS 'Maximum quantity',
  safety_stock       IS 'Safety stock quantity',
  lead_time_days     IS 'Lead time in days',
  aisle_code         IS 'Storage aisle',
  bay_code           IS 'Storage bay',
  shelf_code         IS 'Storage shelf',
  is_active          IS 'Active flag (Y/N, soft delete)'
);
