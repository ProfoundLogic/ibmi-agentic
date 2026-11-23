-- ---------------------------------------------------------------------------
-- Table:   company_config  (system name COMPCFG)
-- Module:  perp
-- Purpose: Per-company key/value config. Auto-approval thresholds and
--          other tunables live here rather than being hard-coded.
-- Epic:    PERP-2 (Company & System Reference)
-- ---------------------------------------------------------------------------

CREATE TABLE company_config FOR SYSTEM NAME COMPCFG (

  -- Composite key ---------------------------------------------------------
  company_code    FOR COLUMN COMPCD  CHAR(3)      NOT NULL,
  config_key      FOR COLUMN CFGKEY  VARCHAR(40)  NOT NULL,

  -- Value + description ---------------------------------------------------
  config_value    FOR COLUMN CFGVAL  VARCHAR(256) NOT NULL DEFAULT '',
  description     FOR COLUMN CFGDSC  VARCHAR(60)  NOT NULL DEFAULT '',

  -- Standard audit block --------------------------------------------------
  created_at      FOR COLUMN CRTAT   TIMESTAMP    NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_by      FOR COLUMN CRTBY   VARCHAR(18)  NOT NULL DEFAULT USER,
  updated_at      FOR COLUMN UPDAT   TIMESTAMP    NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_by      FOR COLUMN UPDBY   VARCHAR(18)  NOT NULL DEFAULT USER,
  is_active       FOR COLUMN ISACT   CHAR(1)      NOT NULL DEFAULT 'Y',

  -- Composite PK ----------------------------------------------------------
  PRIMARY KEY (company_code, config_key),

  -- Constraints -----------------------------------------------------------
  CONSTRAINT compcfg_isact_ck  CHECK (is_active IN ('Y','N')),

  -- FK to company ---------------------------------------------------------
  CONSTRAINT compcfg_company_fk FOREIGN KEY (company_code)
    REFERENCES company (company_code)
    ON DELETE RESTRICT ON UPDATE RESTRICT
);

LABEL ON TABLE company_config IS
  'PERP per-company config key/value pairs';

LABEL ON COLUMN company_config (
  company_code IS 'Company code (FK to company)',
  config_key   IS 'Config key (dotted namespace, e.g. approval.auto_threshold)',
  config_value IS 'Config value (string; caller parses)',
  description  IS 'Human-readable description of the key',
  is_active    IS 'Active flag (Y/N, soft delete)'
);
