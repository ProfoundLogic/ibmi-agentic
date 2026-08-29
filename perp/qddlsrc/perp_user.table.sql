-- ---------------------------------------------------------------------------
-- Table:   perp_user  (system name PERPUSR)
-- Module:  perp
-- Purpose: Application-level user directory. Typically maps 1:1 to an
--          IBM i user profile, but carries role + display attributes
--          used throughout PERP (buyer/receiver/approver/requester).
-- Epic:    PERP-2 (Company & System Reference)
-- ---------------------------------------------------------------------------

-- SQL name 'perp_user' is itself a valid ≤10-char system name (underscores
-- allowed), so DB2 for i rejects a different FOR SYSTEM NAME with SQL7029.
-- The system name auto-derives to PERP_USER; RPG references it that way.
CREATE TABLE perp_user (

  -- Natural key -----------------------------------------------------------
  user_code       FOR COLUMN USRCD   CHAR(10)     NOT NULL,

  -- Descriptive columns ---------------------------------------------------
  display_name    FOR COLUMN DSPNM   VARCHAR(60)  NOT NULL DEFAULT '',
  email_address   FOR COLUMN EMAIL   VARCHAR(120) NOT NULL DEFAULT '',

  -- Role (FK to code_master USERROLE) ------------------------------------
  role_code       FOR COLUMN ROLECD  VARCHAR(20)  NOT NULL DEFAULT 'REQUESTER',
  role_type       FOR COLUMN ROLETYP VARCHAR(20)  NOT NULL DEFAULT 'USERROLE',

  -- Standard audit block --------------------------------------------------
  created_at      FOR COLUMN CRTAT   TIMESTAMP    NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_by      FOR COLUMN CRTBY   VARCHAR(18)  NOT NULL DEFAULT USER,
  updated_at      FOR COLUMN UPDAT   TIMESTAMP    NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_by      FOR COLUMN UPDBY   VARCHAR(18)  NOT NULL DEFAULT USER,
  is_active       FOR COLUMN ISACT   CHAR(1)      NOT NULL DEFAULT 'Y',

  -- Primary key -----------------------------------------------------------
  PRIMARY KEY (user_code),

  -- Constraints -----------------------------------------------------------
  CONSTRAINT perpusr_isact_ck   CHECK (is_active IN ('Y','N')),
  CONSTRAINT perpusr_roletyp_ck CHECK (role_type = 'USERROLE'),

  -- FK to code_master (USERROLE) -----------------------------------------
  CONSTRAINT perpusr_role_fk FOREIGN KEY (role_type, role_code)
    REFERENCES code_master (code_type, code_value)
    ON DELETE RESTRICT ON UPDATE RESTRICT
);

LABEL ON TABLE perp_user IS
  'PERP application user directory';

LABEL ON COLUMN perp_user (
  user_code    IS 'User code (typically IBM i user profile, PK)',
  display_name  IS 'Display name',
  email_address IS 'Email address',
  role_code    IS 'Role (FK to code_master USERROLE)',
  role_type    IS 'Role type discriminator (constant)',
  is_active    IS 'Active flag (Y/N, soft delete)'
);
