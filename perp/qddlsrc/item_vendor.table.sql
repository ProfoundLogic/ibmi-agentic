-- ---------------------------------------------------------------------------
-- Table:   item_vendor  (system name ITEM_VENDOR, auto-derived)
-- Module:  perp
-- Purpose: Item <-> vendor sourcing profile. vendor_part_number, lead
--          time, MOQ, pack size, and the is_preferred flag that the
--          partial unique index below enforces (one preferred vendor
--          per item). See item_vendor_preferred_ak.index.sql.
-- Epic:    PERP-5 (PERP-28)
-- ---------------------------------------------------------------------------

-- 'item_vendor' (11 chars) exceeds the 10-char system-name cap, so DB2
-- abbreviates unless we don't ask for a specific value -- omit FOR SYSTEM
-- NAME and confirm the real auto-derived name with DSPOBJD after build
-- (same approach as item_class/item_lot -- see DDL_STYLE_GUIDE.md Sec.2).
CREATE TABLE item_vendor (

  -- Composite key -----------------------------------------------------------
  company_code       FOR COLUMN COMPCD  CHAR(3)       NOT NULL,
  item_number        FOR COLUMN ITMNBR  VARCHAR(25)   NOT NULL,
  vendor_code        FOR COLUMN VNDCD   VARCHAR(10)   NOT NULL,

  -- Sourcing profile ----------------------------------------------------------
  vendor_part_number FOR COLUMN VPARTN  VARCHAR(25)   NOT NULL DEFAULT '',
  lead_time_days     FOR COLUMN LEADTM  INTEGER       NOT NULL DEFAULT 0,
  moq                                   DECIMAL(15,4) NOT NULL DEFAULT 0,
  pack_size          FOR COLUMN PACKSZ  DECIMAL(15,4) NOT NULL DEFAULT 1,
  is_preferred       FOR COLUMN ISPREF  CHAR(1)       NOT NULL DEFAULT 'N',

  -- Standard audit block --------------------------------------------------
  created_at         FOR COLUMN CRTAT   TIMESTAMP     NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_by         FOR COLUMN CRTBY   VARCHAR(18)   NOT NULL DEFAULT USER,
  updated_at         FOR COLUMN UPDAT   TIMESTAMP     NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_by         FOR COLUMN UPDBY   VARCHAR(18)   NOT NULL DEFAULT USER,
  is_active          FOR COLUMN ISACT   CHAR(1)       NOT NULL DEFAULT 'Y',

  -- Composite primary key ---------------------------------------------------
  PRIMARY KEY (company_code, item_number, vendor_code),

  -- Constraints -----------------------------------------------------------
  CONSTRAINT itmvnd_isact_ck  CHECK (is_active IN ('Y','N')),
  CONSTRAINT itmvnd_ispref_ck CHECK (is_preferred IN ('Y','N')),
  CONSTRAINT itmvnd_leadtm_ck CHECK (lead_time_days >= 0),
  CONSTRAINT itmvnd_moq_ck    CHECK (moq >= 0),
  CONSTRAINT itmvnd_packsz_ck CHECK (pack_size > 0),

  -- FKs -----------------------------------------------------------------------
  CONSTRAINT itmvnd_item_fk FOREIGN KEY (company_code, item_number)
    REFERENCES item (company_code, item_number)
    ON DELETE RESTRICT ON UPDATE RESTRICT,
  CONSTRAINT itmvnd_vendor_fk FOREIGN KEY (company_code, vendor_code)
    REFERENCES vendor (company_code, vendor_code)
    ON DELETE RESTRICT ON UPDATE RESTRICT
);

LABEL ON TABLE item_vendor IS
  'PERP item-vendor sourcing profile';

LABEL ON COLUMN item_vendor (
  company_code       IS 'Company code (FK to item, vendor)',
  item_number         IS 'Item number (FK to item)',
  vendor_code         IS 'Vendor code (FK to vendor)',
  vendor_part_number  IS 'Vendor''s part number for this item',
  lead_time_days      IS 'Lead time in days',
  moq                 IS 'Minimum order quantity',
  pack_size           IS 'Pack size (units per pack)',
  is_preferred        IS 'Preferred vendor flag (Y/N, one per item)',
  is_active           IS 'Active flag (Y/N, soft delete)'
);
