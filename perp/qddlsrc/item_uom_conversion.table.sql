-- ---------------------------------------------------------------------------
-- Table:   item_uom_conversion  (system name ITMUOMCV)
-- Module:  perp
-- Purpose: Per-item UOM conversion factors (e.g. for item WIDGET1:
--          1 CS = 12 EA). Looked up from the per-item conversion screen
--          reached from item maintenance (PERP-21).
-- Epic:    PERP-3 / PERP-20 (Inventory Master)
-- ---------------------------------------------------------------------------

CREATE TABLE item_uom_conversion FOR SYSTEM NAME ITMUOMCV (

  -- Composite key -----------------------------------------------------------
  company_code      FOR COLUMN COMPCD  CHAR(3)       NOT NULL,
  item_number       FOR COLUMN ITMNBR  VARCHAR(25)   NOT NULL,
  from_uom          FOR COLUMN FRMUOM  VARCHAR(5)    NOT NULL,
  to_uom            FOR COLUMN TOUOM   VARCHAR(5)    NOT NULL,

  -- Conversion factor: 1 from_uom = conversion_factor to_uom -----------------
  conversion_factor FOR COLUMN CONVFCT DECIMAL(15,6) NOT NULL,

  -- Standard audit block --------------------------------------------------
  created_at        FOR COLUMN CRTAT   TIMESTAMP     NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_by        FOR COLUMN CRTBY   VARCHAR(18)   NOT NULL DEFAULT USER,
  updated_at        FOR COLUMN UPDAT   TIMESTAMP     NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_by        FOR COLUMN UPDBY   VARCHAR(18)   NOT NULL DEFAULT USER,
  is_active         FOR COLUMN ISACT   CHAR(1)       NOT NULL DEFAULT 'Y',

  -- Composite primary key ---------------------------------------------------
  PRIMARY KEY (company_code, item_number, from_uom, to_uom),

  -- Constraints ---------------------------------------------------------------
  CONSTRAINT itmuomcv_isact_ck CHECK (is_active IN ('Y','N')),
  CONSTRAINT itmuomcv_fct_ck   CHECK (conversion_factor > 0),
  CONSTRAINT itmuomcv_uom_ck   CHECK (from_uom <> to_uom),

  -- FKs -----------------------------------------------------------------------
  CONSTRAINT itmuomcv_item_fk FOREIGN KEY (company_code, item_number)
    REFERENCES item (company_code, item_number)
    ON DELETE RESTRICT ON UPDATE RESTRICT,
  CONSTRAINT itmuomcv_frmuom_fk FOREIGN KEY (from_uom)
    REFERENCES uom (uom_code)
    ON DELETE RESTRICT ON UPDATE RESTRICT,
  CONSTRAINT itmuomcv_touom_fk FOREIGN KEY (to_uom)
    REFERENCES uom (uom_code)
    ON DELETE RESTRICT ON UPDATE RESTRICT
);

LABEL ON TABLE item_uom_conversion IS
  'PERP per-item UOM conversion factors';

LABEL ON COLUMN item_uom_conversion (
  company_code      IS 'Company code (FK to item)',
  item_number       IS 'Item number (FK to item)',
  from_uom          IS 'From UOM (FK to uom)',
  to_uom            IS 'To UOM (FK to uom)',
  conversion_factor IS '1 from_uom = conversion_factor to_uom',
  is_active         IS 'Active flag (Y/N, soft delete)'
);
