-- ---------------------------------------------------------------------------
-- Table:   requisition_line  (system name REQUI00002, auto-derived)
-- Module:  perp
-- Purpose: Requisition line. No status column here -- a line is
--          considered "converted" once a later epic's po_line FKs back to
--          it; that is derived by joining po_line to this table's PK, not
--          stored redundantly on the line itself.
-- Epic:    PERP-6 (PERP-33)
-- ---------------------------------------------------------------------------

-- 'requisition_line' (17 chars) exceeds the 10-char system-name cap, so DB2
-- abbreviates unless we omit FOR SYSTEM NAME -- same approach as
-- requisition_header above. Auto-derived to REQUI00002 (confirmed via
-- DSPOBJD after build).
CREATE TABLE requisition_line (

  -- Composite key (per-company, per-requisition) -----------------------------
  company_code       FOR COLUMN COMPCD  CHAR(3)       NOT NULL,
  requisition_number FOR COLUMN REQNBR  BIGINT        NOT NULL,
  line_number        FOR COLUMN LINNBR  INTEGER       NOT NULL,

  -- Item / quantity -----------------------------------------------------------
  item_number        FOR COLUMN ITMNBR  VARCHAR(25)   NOT NULL,
  quantity           FOR COLUMN QTY     DECIMAL(15,4) NOT NULL DEFAULT 0,
  uom_code           FOR COLUMN UOMCD   VARCHAR(5)    NOT NULL,

  -- Estimated cost (defaulted from the preferred vendor price on entry,
  -- but editable -- see PERP-34) ----------------------------------------------
  est_unit_cost      FOR COLUMN ESTCST  DECIMAL(15,4) NOT NULL DEFAULT 0,

  -- Optional per-line override of the header's need_by_date; null means
  -- "use the header date" --------------------------------------------------
  need_by_date       FOR COLUMN NEEDBY  DATE,

  -- 'notes' (5 chars) is itself a valid system name -- explicit FOR COLUMN
  -- NOTES raises SQL0612 "duplicate column name" (see requisition_header
  -- for the same finding); omit the clause and let it auto-derive.
  notes              VARCHAR(240)  NOT NULL DEFAULT '',

  -- Standard audit block --------------------------------------------------
  created_at         FOR COLUMN CRTAT   TIMESTAMP     NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_by         FOR COLUMN CRTBY   VARCHAR(18)   NOT NULL DEFAULT USER,
  updated_at         FOR COLUMN UPDAT   TIMESTAMP     NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_by         FOR COLUMN UPDBY   VARCHAR(18)   NOT NULL DEFAULT USER,
  is_active          FOR COLUMN ISACT   CHAR(1)       NOT NULL DEFAULT 'Y',

  -- Composite primary key ---------------------------------------------------
  PRIMARY KEY (company_code, requisition_number, line_number),

  -- Constraints -----------------------------------------------------------
  CONSTRAINT reqln_isact_ck  CHECK (is_active IN ('Y','N')),
  CONSTRAINT reqln_linnbr_ck CHECK (line_number > 0),
  CONSTRAINT reqln_qty_ck    CHECK (quantity > 0),
  CONSTRAINT reqln_estcst_ck CHECK (est_unit_cost >= 0),

  -- FKs -----------------------------------------------------------------------
  CONSTRAINT reqln_hdr_fk FOREIGN KEY (company_code, requisition_number)
    REFERENCES requisition_header (company_code, requisition_number)
    ON DELETE RESTRICT ON UPDATE RESTRICT,
  CONSTRAINT reqln_item_fk FOREIGN KEY (company_code, item_number)
    REFERENCES item (company_code, item_number)
    ON DELETE RESTRICT ON UPDATE RESTRICT,
  CONSTRAINT reqln_uom_fk FOREIGN KEY (uom_code)
    REFERENCES uom (uom_code)
    ON DELETE RESTRICT ON UPDATE RESTRICT
);

LABEL ON TABLE requisition_line IS
  'PERP requisition line';

LABEL ON COLUMN requisition_line (
  company_code       IS 'Company code (FK to requisition_header)',
  requisition_number IS 'Requisition number (FK to requisition_header)',
  line_number        IS 'Line number within requisition (PK)',
  item_number        IS 'Item number (FK to item)',
  quantity           IS 'Requested quantity',
  uom_code           IS 'Unit of measure (FK to uom)',
  est_unit_cost      IS 'Estimated unit cost',
  need_by_date       IS 'Line need-by override, null = use header date',
  notes              IS 'Line notes',
  is_active          IS 'Active flag (Y/N, soft delete)'
);
