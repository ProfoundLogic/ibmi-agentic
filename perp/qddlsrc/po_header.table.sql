-- ---------------------------------------------------------------------------
-- Table:   po_header  (system name PO_HEADER, auto-derived)
-- Module:  perp
-- Purpose: Purchase order header. Buyer is snapshotted from vendor at
--          creation so historical POs keep their original buyer even if
--          the vendor is later reassigned. total_amount is maintained by
--          the PO entry program (UPDATE ... SET total_amount = SUM(...))
--          — same idiom used by requisition_header.total_estimated_cost.
--          No po_display computed column here — DDL_STYLE_GUIDE.md Sec.9's
--          GENERATED ALWAYS AS (expression) pattern does not build on this
--          target (confirmed PERP-33); display formatting happens in RPG.
-- Epic:    PERP-7 (PERP-37)
-- ---------------------------------------------------------------------------

-- 'po_header' (9 chars) is itself a valid system name, so DB2 auto-derives
-- PO_HEADER and FOR SYSTEM NAME would raise SQL7029 — same rule as company
-- and vendor. Omit the clause and let it auto-derive.
CREATE TABLE po_header (

  -- Composite key (per-company) ---------------------------------------------
  company_code       FOR COLUMN COMPCD  CHAR(3)       NOT NULL,
  po_number          FOR COLUMN PONBR   BIGINT        NOT NULL,

  -- Vendor / buyer -----------------------------------------------------------
  vendor_code        FOR COLUMN VNDCD   VARCHAR(10)   NOT NULL,
  -- buyer_code is snapshotted from vendor.buyer_code at PO creation. FK to
  -- perp_user (same rule vendor.buyer_code follows) — a buyer must be a
  -- real named user; CODERFLOW does not create POs (unlike requisition
  -- approvals which can be system-stamped).
  buyer_code         FOR COLUMN BUYCD   CHAR(10)      NOT NULL,

  -- Dates -------------------------------------------------------------------
  order_date         FOR COLUMN ORDDT   DATE          NOT NULL DEFAULT CURRENT_DATE,

  -- Status (FK to code_master POSTATUS) -------------------------------------
  status_code        FOR COLUMN STCODE  VARCHAR(20)   NOT NULL DEFAULT 'DRAFT',
  status_type        FOR COLUMN STTYPE  VARCHAR(20)   NOT NULL DEFAULT 'POSTATUS',

  -- Currency (FK to code_master CURRENCY) -----------------------------------
  currency_code      FOR COLUMN CURR    VARCHAR(20)   NOT NULL DEFAULT 'USD',
  currency_type      FOR COLUMN CURTYP  VARCHAR(20)   NOT NULL DEFAULT 'CURRENCY',

  -- Roll-up total. Maintained by po entry program on line insert/change/
  -- delete via the same UPDATE-with-subquery pattern requisition_header uses.
  total_amount       FOR COLUMN TOTAMT  DECIMAL(15,2) NOT NULL DEFAULT 0,

  -- Long-form notes. 'notes' (5 chars) is itself a valid system name so
  -- explicit FOR COLUMN NOTES raises SQL0612 — omit and let it auto-derive
  -- (same finding as requisition_header, DDL_STYLE_GUIDE.md Sec.2).
  notes              CLOB(16K),

  -- Standard audit block ----------------------------------------------------
  created_at         FOR COLUMN CRTAT   TIMESTAMP     NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_by         FOR COLUMN CRTBY   VARCHAR(18)   NOT NULL DEFAULT USER,
  updated_at         FOR COLUMN UPDAT   TIMESTAMP     NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_by         FOR COLUMN UPDBY   VARCHAR(18)   NOT NULL DEFAULT USER,
  is_active          FOR COLUMN ISACT   CHAR(1)       NOT NULL DEFAULT 'Y',

  -- Composite primary key ---------------------------------------------------
  PRIMARY KEY (company_code, po_number),

  -- Constraints -------------------------------------------------------------
  CONSTRAINT pohdr_isact_ck   CHECK (is_active IN ('Y','N')),
  CONSTRAINT pohdr_sttype_ck  CHECK (status_type = 'POSTATUS'),
  CONSTRAINT pohdr_curtyp_ck  CHECK (currency_type = 'CURRENCY'),
  CONSTRAINT pohdr_totamt_ck  CHECK (total_amount >= 0),

  -- FKs ----------------------------------------------------------------------
  CONSTRAINT pohdr_company_fk FOREIGN KEY (company_code)
    REFERENCES company (company_code)
    ON DELETE RESTRICT ON UPDATE RESTRICT,
  CONSTRAINT pohdr_vendor_fk FOREIGN KEY (company_code, vendor_code)
    REFERENCES vendor (company_code, vendor_code)
    ON DELETE RESTRICT ON UPDATE RESTRICT,
  CONSTRAINT pohdr_buyer_fk FOREIGN KEY (buyer_code)
    REFERENCES perp_user (user_code)
    ON DELETE RESTRICT ON UPDATE RESTRICT,
  CONSTRAINT pohdr_stat_fk FOREIGN KEY (status_type, status_code)
    REFERENCES code_master (code_type, code_value)
    ON DELETE RESTRICT ON UPDATE RESTRICT,
  CONSTRAINT pohdr_curr_fk FOREIGN KEY (currency_type, currency_code)
    REFERENCES code_master (code_type, code_value)
    ON DELETE RESTRICT ON UPDATE RESTRICT
);

LABEL ON TABLE po_header IS
  'PERP purchase order header';

LABEL ON COLUMN po_header (
  company_code       IS 'Company code (FK to company)',
  po_number          IS 'PO number (doc_sequence PO)',
  vendor_code        IS 'Vendor (FK to vendor)',
  buyer_code         IS 'Buyer snapshot from vendor at creation',
  order_date         IS 'Date PO was placed',
  status_code        IS 'Status (FK to code_master POSTATUS)',
  status_type        IS 'Status type discriminator (constant)',
  currency_code      IS 'Currency (FK to code_master CURRENCY)',
  currency_type      IS 'Currency type discriminator (constant)',
  total_amount       IS 'Sum of line ordered_qty * unit_price',
  notes              IS 'Long-form notes',
  is_active          IS 'Active flag (Y/N, soft delete)'
);
