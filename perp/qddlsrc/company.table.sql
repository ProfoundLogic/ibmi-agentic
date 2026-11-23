-- ---------------------------------------------------------------------------
-- Table:   company  (system name COMPANY)
-- Module:  perp
-- Purpose: Multi-tenant root. Every business table in PERP FKs its
--          company_code column back to this table.
-- Epic:    PERP-2 (Company & System Reference)
-- ---------------------------------------------------------------------------

-- Note on FOR SYSTEM NAME here: 'company' as a SQL table name auto-derives
-- to the 10-char system name COMPANY. On DB2 for i, specifying FOR SYSTEM
-- NAME with a value equal to that auto-derivation raises SQL7029 - so we
-- omit it. Same rule applies to any FOR COLUMN whose value would equal the
-- column's auto-derived short name (see 'city_name' below - renamed from
-- 'city' so an explicit FOR COLUMN CITY is meaningful).
CREATE TABLE company (

  -- Natural / multi-tenant key --------------------------------------------
  company_code    FOR COLUMN COMPCD  CHAR(3)      NOT NULL,

  -- Descriptive columns ---------------------------------------------------
  company_name    FOR COLUMN COMPNM  VARCHAR(60)  NOT NULL,
  address_line1   FOR COLUMN ADDR1   VARCHAR(60)  NOT NULL DEFAULT '',
  address_line2   FOR COLUMN ADDR2   VARCHAR(60)  NOT NULL DEFAULT '',
  city_name       FOR COLUMN CITY    VARCHAR(40)  NOT NULL DEFAULT '',
  state_code      FOR COLUMN STATE   VARCHAR(3)   NOT NULL DEFAULT '',
  postal_code     FOR COLUMN POSTCD  VARCHAR(12)  NOT NULL DEFAULT '',
  country_code    FOR COLUMN CNTRY   VARCHAR(3)   NOT NULL DEFAULT 'US',

  -- Base currency (FK to code_master CURRENCY) ---------------------------
  base_currency   FOR COLUMN BASECUR VARCHAR(20)  NOT NULL DEFAULT 'USD',
  currency_type   FOR COLUMN CURTYP  VARCHAR(20)  NOT NULL DEFAULT 'CURRENCY',

  -- Standard audit block --------------------------------------------------
  created_at      FOR COLUMN CRTAT   TIMESTAMP    NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_by      FOR COLUMN CRTBY   VARCHAR(18)  NOT NULL DEFAULT USER,
  updated_at      FOR COLUMN UPDAT   TIMESTAMP    NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_by      FOR COLUMN UPDBY   VARCHAR(18)  NOT NULL DEFAULT USER,
  is_active       FOR COLUMN ISACT   CHAR(1)      NOT NULL DEFAULT 'Y',

  -- Primary key -----------------------------------------------------------
  PRIMARY KEY (company_code),

  -- Constraints -----------------------------------------------------------
  CONSTRAINT company_isact_ck   CHECK (is_active IN ('Y','N')),
  CONSTRAINT company_curtyp_ck  CHECK (currency_type = 'CURRENCY')
);

LABEL ON TABLE company IS
  'PERP company / tenant root';

LABEL ON COLUMN company (
  company_code   IS 'Company code (multi-tenant PK)',
  company_name   IS 'Company display name',
  address_line1  IS 'Address line 1',
  address_line2  IS 'Address line 2',
  city_name      IS 'City',
  state_code     IS 'State / province code',
  postal_code    IS 'Postal / ZIP code',
  country_code   IS 'ISO country code',
  base_currency  IS 'Base currency (FK to code_master CURRENCY)',
  currency_type  IS 'Currency type discriminator (constant)',
  is_active      IS 'Active flag (Y/N, soft delete)'
);
