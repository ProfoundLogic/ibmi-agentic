-- ---------------------------------------------------------------------------
-- Table:   item_vendor_price  (system name abbreviated, >10 chars)
-- Module:  perp
-- Purpose: Effective-dated pricing per item_vendor. Current row has
--          effective_to IS NULL. Historical rows are immutable -- a new
--          price closes the current row (sets effective_to) and inserts
--          a new row starting effective_from = today (see PERP-31).
-- Epic:    PERP-5 (PERP-28)
-- ---------------------------------------------------------------------------

-- 'item_vendor_price' (18 chars) exceeds the 10-char system-name cap; omit
-- FOR SYSTEM NAME and confirm the real auto-derived name with DSPOBJD
-- after build, per the SQL7029 guidance in DDL_STYLE_GUIDE.md Sec.2.
CREATE TABLE item_vendor_price (

  -- Composite key (effective_from is part of the PK) ------------------------
  company_code   FOR COLUMN COMPCD  CHAR(3)       NOT NULL,
  item_number    FOR COLUMN ITMNBR  VARCHAR(25)   NOT NULL,
  vendor_code    FOR COLUMN VNDCD   VARCHAR(10)   NOT NULL,
  effective_from FOR COLUMN EFFFRM  DATE          NOT NULL DEFAULT CURRENT_DATE,

  -- Price ---------------------------------------------------------------------
  effective_to   FOR COLUMN EFFTO   DATE,
  unit_price     FOR COLUMN UNTPRC  DECIMAL(15,4) NOT NULL,

  -- Currency (constant discriminator + code_master FK) ----------------------
  currency_code  FOR COLUMN CURR    VARCHAR(20)   NOT NULL DEFAULT 'USD',
  currency_type  FOR COLUMN CURTYP  VARCHAR(20)   NOT NULL DEFAULT 'CURRENCY',

  -- Where the price came from --------------------------------------------
  price_source   FOR COLUMN PRCSRC  VARCHAR(20)   NOT NULL DEFAULT 'MANUAL',

  -- Standard audit block --------------------------------------------------
  created_at     FOR COLUMN CRTAT   TIMESTAMP     NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_by     FOR COLUMN CRTBY   VARCHAR(18)   NOT NULL DEFAULT USER,
  updated_at     FOR COLUMN UPDAT   TIMESTAMP     NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_by     FOR COLUMN UPDBY   VARCHAR(18)   NOT NULL DEFAULT USER,
  is_active      FOR COLUMN ISACT   CHAR(1)       NOT NULL DEFAULT 'Y',

  -- Composite primary key ---------------------------------------------------
  PRIMARY KEY (company_code, item_number, vendor_code, effective_from),

  -- Constraints -----------------------------------------------------------
  CONSTRAINT itmvprc_isact_ck  CHECK (is_active IN ('Y','N')),
  CONSTRAINT itmvprc_curtyp_ck CHECK (currency_type = 'CURRENCY'),
  CONSTRAINT itmvprc_prcsrc_ck CHECK (price_source IN ('MANUAL','IMPORT')),
  CONSTRAINT itmvprc_price_ck  CHECK (unit_price >= 0),
  CONSTRAINT itmvprc_efftv_ck  CHECK (effective_to IS NULL
                                      OR effective_to >= effective_from),

  -- FKs (item + vendor referential integrity comes transitively through
  -- item_vendor, which already FKs both) -----------------------------------
  CONSTRAINT itmvprc_itmvnd_fk FOREIGN KEY (company_code, item_number, vendor_code)
    REFERENCES item_vendor (company_code, item_number, vendor_code)
    ON DELETE RESTRICT ON UPDATE RESTRICT,
  CONSTRAINT itmvprc_curr_fk FOREIGN KEY (currency_type, currency_code)
    REFERENCES code_master (code_type, code_value)
    ON DELETE RESTRICT ON UPDATE RESTRICT
);

LABEL ON TABLE item_vendor_price IS
  'PERP effective-dated item-vendor pricing';

LABEL ON COLUMN item_vendor_price (
  company_code   IS 'Company code (FK to item_vendor)',
  item_number    IS 'Item number (FK to item_vendor)',
  vendor_code    IS 'Vendor code (FK to item_vendor)',
  effective_from IS 'Effective from date (PK)',
  effective_to   IS 'Effective to date (null = current)',
  unit_price     IS 'Unit price',
  currency_code  IS 'Currency (FK to code_master)',
  currency_type  IS 'Currency type discriminator (constant)',
  price_source   IS 'Price source (MANUAL/IMPORT)',
  is_active      IS 'Active flag (Y/N, soft delete)'
);
