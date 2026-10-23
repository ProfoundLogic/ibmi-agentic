-- ---------------------------------------------------------------------------
-- Table:   item_class  (system name ITMCLS)
-- Module:  perp
-- Purpose: Flat item classification, per company. Items FK to this for
--          their class_code.
-- Epic:    PERP-3 / PERP-20 (Inventory Master)
-- ---------------------------------------------------------------------------

-- 'item_class' is itself a valid <=10-char system name (underscores
-- allowed), so any explicit FOR SYSTEM NAME here raises SQL7029 -- even a
-- value matching the true auto-derived name (ITEM_CLASS). The error message
-- echoes back whatever value *you* specified, not the real auto-derived
-- one -- confirmed here via DSPOBJD (object is PERPDEMO/ITEM_CLASS, not the
-- abbreviated ITMCLS this file originally guessed). Omit the clause and
-- verify the real object name with DSPOBJD/DSPFD rather than trusting the
-- SQL7029 message text.
CREATE TABLE item_class (

  -- Composite key (per-company) ---------------------------------------------
  company_code  FOR COLUMN COMPCD  CHAR(3)      NOT NULL,
  class_code    FOR COLUMN CLSCD   VARCHAR(10)  NOT NULL,

  -- Descriptive columns -------------------------------------------------------
  description   FOR COLUMN CLSDSC  VARCHAR(60)  NOT NULL,

  -- Standard audit block --------------------------------------------------
  created_at    FOR COLUMN CRTAT   TIMESTAMP    NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_by    FOR COLUMN CRTBY   VARCHAR(18)  NOT NULL DEFAULT USER,
  updated_at    FOR COLUMN UPDAT   TIMESTAMP    NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_by    FOR COLUMN UPDBY   VARCHAR(18)  NOT NULL DEFAULT USER,
  is_active     FOR COLUMN ISACT   CHAR(1)      NOT NULL DEFAULT 'Y',

  -- Composite primary key ---------------------------------------------------
  PRIMARY KEY (company_code, class_code),

  -- Constraints ---------------------------------------------------------------
  CONSTRAINT itmcls_isact_ck  CHECK (is_active IN ('Y','N')),

  -- FK to company -------------------------------------------------------------
  CONSTRAINT itmcls_company_fk FOREIGN KEY (company_code)
    REFERENCES company (company_code)
    ON DELETE RESTRICT ON UPDATE RESTRICT
);

LABEL ON TABLE item_class IS
  'PERP item classification (flat, per company)';

LABEL ON COLUMN item_class (
  company_code IS 'Company code (FK to company)',
  class_code   IS 'Item class code',
  description  IS 'Item class description',
  is_active    IS 'Active flag (Y/N, soft delete)'
);
