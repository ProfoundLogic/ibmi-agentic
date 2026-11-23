-- ---------------------------------------------------------------------------
-- Table:   uom  (system name UOM)
-- Module:  perp
-- Purpose: Unit-of-measure master. Global (not company-scoped) — items in
--          every company reference the same UOM codes. Kept as its own
--          table rather than a code_master row because UOMs carry
--          structural attributes (see item_uom_conversion) that don't fit
--          the generic lookup shape.
-- Epic:    PERP-3 / PERP-20 (Inventory Master)
-- ---------------------------------------------------------------------------

-- 'uom' auto-derives to system name UOM; specifying FOR SYSTEM NAME with
-- the same value raises SQL7029, so it's omitted here (see company.table.sql
-- for the same rule applied to 'company').
CREATE TABLE uom (

  -- Natural key -------------------------------------------------------------
  uom_code      FOR COLUMN UOMCD   VARCHAR(5)   NOT NULL,

  -- Descriptive columns -------------------------------------------------------
  description   FOR COLUMN UOMDSC  VARCHAR(60)  NOT NULL,
  uom_category  FOR COLUMN UOMCAT  VARCHAR(20)  NOT NULL DEFAULT '',

  -- Standard audit block --------------------------------------------------
  created_at    FOR COLUMN CRTAT   TIMESTAMP    NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_by    FOR COLUMN CRTBY   VARCHAR(18)  NOT NULL DEFAULT USER,
  updated_at    FOR COLUMN UPDAT   TIMESTAMP    NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_by    FOR COLUMN UPDBY   VARCHAR(18)  NOT NULL DEFAULT USER,
  is_active     FOR COLUMN ISACT   CHAR(1)      NOT NULL DEFAULT 'Y',

  -- Primary key -------------------------------------------------------------
  PRIMARY KEY (uom_code),

  -- Constraints -------------------------------------------------------------
  CONSTRAINT uom_isact_ck CHECK (is_active IN ('Y','N'))
);

LABEL ON TABLE uom IS
  'PERP unit-of-measure master';

LABEL ON COLUMN uom (
  uom_code     IS 'Unit of measure code (PK)',
  description  IS 'UOM description',
  uom_category IS 'UOM category (e.g. WEIGHT, VOLUME, EACH)',
  is_active    IS 'Active flag (Y/N, soft delete)'
);
