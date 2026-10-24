-- ---------------------------------------------------------------------------
-- Table:   example_reference  (system name EXMPREF)
-- Module:  perp
-- Purpose: Reference implementation of every DDL convention in
--          perp/DDL_STYLE_GUIDE.md. Copy this file when creating a new
--          table so that no convention is accidentally dropped.
-- Epic:    PERP-1 (Foundation)
--
-- Notes on DB2 for i syntax:
--   * FOR COLUMN goes BETWEEN column name and data type. Placing it after
--     CHAR/VARCHAR raises SQL0199 because the parser treats "FOR ..." after
--     a character type as the CCSID modifier grammar.
--   * GENERATED ALWAYS AS ('literal') is rejected -- the generation
--     expression must reference a column of the same table. Use DEFAULT +
--     CHECK for the constant-discriminator pattern.
--   * CHECK constraints are declared as table-level CONSTRAINT clauses so
--     they have stable, human-readable names in DB2 catalog messages.
-- ---------------------------------------------------------------------------

CREATE TABLE example_reference FOR SYSTEM NAME EXMPREF (

  -- Multi-tenant key -------------------------------------------------------
  company_code   FOR COLUMN COMPCD  CHAR(3)      NOT NULL,

  -- Natural key ------------------------------------------------------------
  ref_code       FOR COLUMN REFCD   VARCHAR(20)  NOT NULL,

  -- Descriptive columns ----------------------------------------------------
  ref_name       FOR COLUMN REFNM   VARCHAR(60)  NOT NULL,
  ref_type       FOR COLUMN REFTYP  VARCHAR(20)  NOT NULL,

  -- Numeric with CHECK (table-level constraint below) ----------------------
  sort_order     FOR COLUMN SRTORD  INTEGER      NOT NULL DEFAULT 0,

  -- Effective dating example ----------------------------------------------
  effective_from FOR COLUMN EFFFRM  DATE         NOT NULL DEFAULT CURRENT_DATE,
  effective_to   FOR COLUMN EFFTO   DATE,

  -- Constant discriminator + code FK (parent table lands in PERP-2) --------
  status_code    FOR COLUMN STCODE  VARCHAR(20)  NOT NULL DEFAULT 'ACTIVE',
  status_type    FOR COLUMN STTYPE  VARCHAR(20)  NOT NULL DEFAULT 'EXMPSTAT',

  -- Standard audit block --------------------------------------------------
  created_at     FOR COLUMN CRTAT   TIMESTAMP    NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_by     FOR COLUMN CRTBY   VARCHAR(18)  NOT NULL DEFAULT USER,
  updated_at     FOR COLUMN UPDAT   TIMESTAMP    NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_by     FOR COLUMN UPDBY   VARCHAR(18)  NOT NULL DEFAULT USER,
  is_active      FOR COLUMN ISACT   CHAR(1)      NOT NULL DEFAULT 'Y',

  -- Composite PK — company_code leads --------------------------------------
  PRIMARY KEY (company_code, ref_code),

  -- Table-level CHECK constraints -----------------------------------------
  CONSTRAINT exmpref_srtord_ck  CHECK (sort_order >= 0),
  CONSTRAINT exmpref_isact_ck   CHECK (is_active IN ('Y','N')),
  CONSTRAINT exmpref_sttype_ck  CHECK (status_type = 'EXMPSTAT'),
  CONSTRAINT exmpref_efftv_ck   CHECK (effective_to IS NULL
                                       OR effective_to >= effective_from)

  -- Cross-table FKs would go here. For example, once code_master exists:
  --
  --   , FOREIGN KEY (status_type, status_code)
  --       REFERENCES code_master (code_type, code_value)
  --
  -- and once company exists:
  --
  --   , FOREIGN KEY (company_code)
  --       REFERENCES company (company_code)
  --
  -- They are commented out here because those parent tables land in PERP-2.
);

LABEL ON TABLE example_reference IS
  'PERP DDL style-guide reference example table';

LABEL ON COLUMN example_reference (
  company_code   IS 'Company code (multi-tenant key)',
  ref_code       IS 'Reference code (natural key)',
  ref_name       IS 'Reference name',
  ref_type       IS 'Reference type / category',
  sort_order     IS 'Presentation sort order',
  effective_from IS 'Effective from date',
  effective_to   IS 'Effective to date (null = current)',
  status_code    IS 'Status (FK to code_master)',
  status_type    IS 'Status type discriminator (constant)',
  is_active      IS 'Active flag (Y/N, soft delete)'
);
