-- ---------------------------------------------------------------------------
-- Table:   requisition_header  (system name REQUI00001, auto-derived)
-- Module:  perp
-- Purpose: Requisition header. Confidence score and full approval audit
--          (who/when/source/notes) are built in from day one so CoderFlow
--          auto-approval (PERP-36) has somewhere to write its verdict.
--          No back-pointer to PO here -- linkage lives on po_line (a later
--          epic) so one requisition line can be split/consolidated across
--          multiple POs. "Converted" status is derived by the PO epic
--          joining po_line back to this table's PK, not stored here.
-- Epic:    PERP-6 (PERP-33)
-- ---------------------------------------------------------------------------

-- 'requisition_header' (19 chars) exceeds the 10-char system-name cap, so
-- DB2 abbreviates unless we omit FOR SYSTEM NAME -- same approach as
-- item_vendor/item_vendor_price (DDL_STYLE_GUIDE.md Sec.2). Auto-derived to
-- REQUI00001 (sequential counter, confirmed via DSPOBJD after build --
-- not a truncation of the SQL name, per the item_vendor_price precedent).
CREATE TABLE requisition_header (

  -- Composite key (per-company) ---------------------------------------------
  -- No requisition_display computed column here -- DDL_STYLE_GUIDE.md Sec.9's
  -- GENERATED ALWAYS AS (expression) pattern does not build on this target
  -- (confirmed PERP-33; see the style guide for the full finding). Any
  -- display formatting happens in RPG/DSPF instead.
  company_code         FOR COLUMN COMPCD  CHAR(3)       NOT NULL,
  requisition_number   FOR COLUMN REQNBR  BIGINT        NOT NULL,

  -- Requester / dates ---------------------------------------------------------
  requested_by         FOR COLUMN REQBY   CHAR(10)      NOT NULL,
  request_date         FOR COLUMN REQDT   DATE          NOT NULL DEFAULT CURRENT_DATE,
  need_by_date         FOR COLUMN NEEDBY  DATE          NOT NULL,

  -- Priority (FK to code_master PRIORITY) ------------------------------------
  priority_code        FOR COLUMN PRICD   VARCHAR(20)   NOT NULL DEFAULT 'NORMAL',
  priority_type        FOR COLUMN PRITYP  VARCHAR(20)   NOT NULL DEFAULT 'PRIORITY',

  -- Status (FK to code_master REQSTATUS) -------------------------------------
  status_code          FOR COLUMN STCODE  VARCHAR(20)   NOT NULL DEFAULT 'DRAFT',
  status_type          FOR COLUMN STTYPE  VARCHAR(20)   NOT NULL DEFAULT 'REQSTATUS',

  -- Confidence -- NULL until CoderFlow scores it (PERP-36) -------------------
  confidence_pct       FOR COLUMN CONFPCT DECIMAL(5,2),

  -- Approval audit -- all null until the requisition is approved/rejected.
  -- approved_by is NOT FK'd to perp_user: an auto-approval stamps the
  -- literal 'CODERFLOW', which is not a row in the human user directory.
  approved_by          FOR COLUMN APRBY   VARCHAR(18),
  approved_at          FOR COLUMN APRAT   TIMESTAMP,
  approval_source_code FOR COLUMN APRSCD  VARCHAR(20),
  approval_source_type FOR COLUMN APRSTYP VARCHAR(20),
  approval_notes       FOR COLUMN APRNTS  VARCHAR(500),

  -- Cost / currency -----------------------------------------------------------
  total_estimated_cost FOR COLUMN TOTEST  DECIMAL(15,2) NOT NULL DEFAULT 0,
  currency_code        FOR COLUMN CURR    VARCHAR(20)   NOT NULL DEFAULT 'USD',
  currency_type        FOR COLUMN CURTYP  VARCHAR(20)   NOT NULL DEFAULT 'CURRENCY',

  -- Long-form justification. 'notes' (5 chars) is itself a valid system
  -- name, so an explicit FOR COLUMN NOTES matching the auto-derivation
  -- raises SQL0612 "duplicate column name" here (same root cause as the
  -- SQL7029 case in DDL_STYLE_GUIDE.md Sec.2, different DB2 error) -- omit
  -- the clause and let it auto-derive. -------------------------------------
  notes                CLOB(16K),

  -- Standard audit block --------------------------------------------------
  created_at           FOR COLUMN CRTAT   TIMESTAMP     NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_by           FOR COLUMN CRTBY   VARCHAR(18)   NOT NULL DEFAULT USER,
  updated_at           FOR COLUMN UPDAT   TIMESTAMP     NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_by           FOR COLUMN UPDBY   VARCHAR(18)   NOT NULL DEFAULT USER,
  is_active            FOR COLUMN ISACT   CHAR(1)       NOT NULL DEFAULT 'Y',

  -- Composite primary key ---------------------------------------------------
  PRIMARY KEY (company_code, requisition_number),

  -- Constraints -----------------------------------------------------------
  CONSTRAINT reqhdr_isact_ck   CHECK (is_active IN ('Y','N')),
  CONSTRAINT reqhdr_prityp_ck  CHECK (priority_type = 'PRIORITY'),
  CONSTRAINT reqhdr_sttype_ck  CHECK (status_type = 'REQSTATUS'),
  CONSTRAINT reqhdr_curtyp_ck  CHECK (currency_type = 'CURRENCY'),
  CONSTRAINT reqhdr_totest_ck  CHECK (total_estimated_cost >= 0),
  CONSTRAINT reqhdr_confpct_ck CHECK (confidence_pct IS NULL
                                      OR (confidence_pct BETWEEN 0 AND 100)),
  CONSTRAINT reqhdr_aprstyp_ck CHECK (approval_source_type IS NULL
                                      OR approval_source_type = 'APPRSRC'),
  -- Approval audit is all-or-nothing -- either nothing has happened yet, or
  -- who/when/source were all stamped together by the same operation.
  CONSTRAINT reqhdr_aprall_ck  CHECK (
    (approved_by IS NULL AND approved_at IS NULL AND approval_source_code IS NULL)
    OR
    (approved_by IS NOT NULL AND approved_at IS NOT NULL AND approval_source_code IS NOT NULL)
  ),

  -- FKs -----------------------------------------------------------------------
  CONSTRAINT reqhdr_company_fk FOREIGN KEY (company_code)
    REFERENCES company (company_code)
    ON DELETE RESTRICT ON UPDATE RESTRICT,
  CONSTRAINT reqhdr_reqby_fk FOREIGN KEY (requested_by)
    REFERENCES perp_user (user_code)
    ON DELETE RESTRICT ON UPDATE RESTRICT,
  CONSTRAINT reqhdr_pri_fk FOREIGN KEY (priority_type, priority_code)
    REFERENCES code_master (code_type, code_value)
    ON DELETE RESTRICT ON UPDATE RESTRICT,
  CONSTRAINT reqhdr_stat_fk FOREIGN KEY (status_type, status_code)
    REFERENCES code_master (code_type, code_value)
    ON DELETE RESTRICT ON UPDATE RESTRICT,
  CONSTRAINT reqhdr_apr_fk FOREIGN KEY (approval_source_type, approval_source_code)
    REFERENCES code_master (code_type, code_value)
    ON DELETE RESTRICT ON UPDATE RESTRICT,
  CONSTRAINT reqhdr_curr_fk FOREIGN KEY (currency_type, currency_code)
    REFERENCES code_master (code_type, code_value)
    ON DELETE RESTRICT ON UPDATE RESTRICT
);

LABEL ON TABLE requisition_header IS
  'PERP requisition header (confidence + approval)';

LABEL ON COLUMN requisition_header (
  company_code         IS 'Company code (FK to company)',
  requisition_number   IS 'Requisition number (doc_sequence REQ)',
  requested_by         IS 'Requester (FK to perp_user)',
  request_date         IS 'Date requisition was created',
  need_by_date         IS 'Date requisition is needed by',
  priority_code        IS 'Priority (FK to code_master)',
  priority_type        IS 'Priority type discriminator (constant)',
  status_code          IS 'Status (FK to code_master)',
  status_type          IS 'Status type discriminator (constant)',
  confidence_pct       IS 'CoderFlow confidence pct, null until scored',
  approved_by          IS 'Approver user code or CODERFLOW, null until approved',
  approved_at          IS 'Approval/rejection timestamp',
  approval_source_code IS 'Approval source (FK to code_master)',
  approval_source_type IS 'Approval source type discriminator (constant)',
  approval_notes       IS 'Approval/rejection explanation',
  total_estimated_cost IS 'Sum of line est_unit_cost * quantity',
  currency_code        IS 'Currency (FK to code_master)',
  currency_type        IS 'Currency type discriminator (constant)',
  notes                IS 'Long-form justification',
  is_active            IS 'Active flag (Y/N, soft delete)'
);
