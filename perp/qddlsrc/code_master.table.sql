-- ---------------------------------------------------------------------------
-- Table:   code_master  (system name CODEMSTR)
-- Module:  perp
-- Purpose: Generic lookup table. Every enum/status/code/role in PERP
--          lives here keyed on (code_type, code_value). Business tables
--          reference it via a composite FK plus a constant-discriminator
--          column (DEFAULT + CHECK — see DDL_STYLE_GUIDE §10).
-- Epic:    PERP-2 (Company & System Reference)
-- ---------------------------------------------------------------------------

CREATE TABLE code_master FOR SYSTEM NAME CODEMSTR (

  -- Composite natural key -------------------------------------------------
  code_type       FOR COLUMN CODETYP VARCHAR(20)  NOT NULL,
  code_value      FOR COLUMN CODEVAL VARCHAR(20)  NOT NULL,

  -- Descriptive columns ---------------------------------------------------
  description     FOR COLUMN CODEDSC VARCHAR(60)  NOT NULL,
  short_desc      FOR COLUMN SHTDSC  VARCHAR(20)  NOT NULL DEFAULT '',
  sort_order      FOR COLUMN SRTORD  INTEGER      NOT NULL DEFAULT 0,

  -- Free-form attributes (JSON-style extras). CLOB, not JSON, to keep the
  -- table portable to older DB2 for i releases.
  attributes      FOR COLUMN ATTRS   CLOB(4K)     DEFAULT NULL,

  -- Standard audit block --------------------------------------------------
  created_at      FOR COLUMN CRTAT   TIMESTAMP    NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_by      FOR COLUMN CRTBY   VARCHAR(18)  NOT NULL DEFAULT USER,
  updated_at      FOR COLUMN UPDAT   TIMESTAMP    NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_by      FOR COLUMN UPDBY   VARCHAR(18)  NOT NULL DEFAULT USER,
  is_active       FOR COLUMN ISACT   CHAR(1)      NOT NULL DEFAULT 'Y',

  -- Composite primary key -------------------------------------------------
  PRIMARY KEY (code_type, code_value),

  -- Constraints -----------------------------------------------------------
  CONSTRAINT codemstr_srtord_ck CHECK (sort_order >= 0),
  CONSTRAINT codemstr_isact_ck  CHECK (is_active IN ('Y','N'))
);

LABEL ON TABLE code_master IS
  'PERP generic lookup (statuses/roles/etc.)';

LABEL ON COLUMN code_master (
  code_type    IS 'Code type / group (e.g. POSTATUS, USERROLE)',
  code_value   IS 'Code value within the type',
  description  IS 'Long description',
  short_desc   IS 'Short description (list / column display)',
  sort_order   IS 'Presentation sort order',
  attributes   IS 'Free-form JSON-style attributes (nullable)',
  is_active    IS 'Active flag (Y/N, soft delete)'
);
