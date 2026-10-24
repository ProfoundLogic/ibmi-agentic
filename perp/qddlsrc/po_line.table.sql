-- ---------------------------------------------------------------------------
-- Table:   po_line  (system name PO_LINE, auto-derived)
-- Module:  perp
-- Purpose: Purchase order line. unit_price is snapshotted from
--          item_vendor_price at PO creation so historical POs keep their
--          original price even if the current effective row is later
--          superseded (same snapshot idiom as buyer_code on po_header).
--          source_requisition_number / source_requisition_line_number
--          are nullable — populated only when the line originates from
--          a requisition (PERP-39). This line-level (not header-level)
--          linkage lets one requisition line be split across multiple
--          POs and multiple requisitions be consolidated into one PO.
--          open_qty is NOT a stored column — DDL_STYLE_GUIDE.md Sec.9's
--          GENERATED ALWAYS AS (expression) pattern does not build here.
--          Callers derive it as (ordered_qty - received_qty) in RPG or
--          via the po_line_open view (built in the same epic).
-- Epic:    PERP-7 (PERP-37)
-- ---------------------------------------------------------------------------

-- 'po_line' (7 chars) is itself a valid system name, so DB2 auto-derives
-- PO_LINE and FOR SYSTEM NAME would raise SQL7029. Omit the clause.
CREATE TABLE po_line (

  -- Composite key (per-company, per-PO) -------------------------------------
  company_code                 FOR COLUMN COMPCD  CHAR(3)       NOT NULL,
  po_number                    FOR COLUMN PONBR   BIGINT        NOT NULL,
  line_number                  FOR COLUMN LINNBR  INTEGER       NOT NULL,

  -- Item / quantity ---------------------------------------------------------
  item_number                  FOR COLUMN ITMNBR  VARCHAR(25)   NOT NULL,
  ordered_qty                  FOR COLUMN ORDQTY  DECIMAL(15,4) NOT NULL DEFAULT 0,
  received_qty                 FOR COLUMN RCVQTY  DECIMAL(15,4) NOT NULL DEFAULT 0,
  uom_code                     FOR COLUMN UOMCD   VARCHAR(5)    NOT NULL,

  -- Snapshot at PO creation -------------------------------------------------
  unit_price                   FOR COLUMN UNTPRC  DECIMAL(15,4) NOT NULL DEFAULT 0,

  -- Optional expected receipt date. Blanket-schedule lines with real
  -- delivery dates live in po_line_schedule.
  expected_receipt_date        FOR COLUMN EXPRCV  DATE,

  -- Line status (per-line, e.g. one line CLOSED while other lines OPEN).
  -- Same POSTATUS lookup as the header.
  status_code                  FOR COLUMN STCODE  VARCHAR(20)   NOT NULL DEFAULT 'OPEN',
  status_type                  FOR COLUMN STTYPE  VARCHAR(20)   NOT NULL DEFAULT 'POSTATUS',

  -- Optional back-link to the requisition line this PO line came from.
  -- Both are nullable together — a manual PO carries NULLs. The FK
  -- constraint permits NULLs (RESTRICT on delete/update) — see reqln_ref_fk
  -- at the bottom of the table.
  source_requisition_number    FOR COLUMN SRCRQN  BIGINT,
  source_requisition_line_number FOR COLUMN SRCRQL INTEGER,

  -- Standard audit block ----------------------------------------------------
  created_at         FOR COLUMN CRTAT   TIMESTAMP     NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_by         FOR COLUMN CRTBY   VARCHAR(18)   NOT NULL DEFAULT USER,
  updated_at         FOR COLUMN UPDAT   TIMESTAMP     NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_by         FOR COLUMN UPDBY   VARCHAR(18)   NOT NULL DEFAULT USER,
  is_active          FOR COLUMN ISACT   CHAR(1)       NOT NULL DEFAULT 'Y',

  -- Composite primary key ---------------------------------------------------
  PRIMARY KEY (company_code, po_number, line_number),

  -- Constraints -------------------------------------------------------------
  CONSTRAINT poln_isact_ck   CHECK (is_active IN ('Y','N')),
  CONSTRAINT poln_linnbr_ck  CHECK (line_number > 0),
  CONSTRAINT poln_ordqty_ck  CHECK (ordered_qty > 0),
  CONSTRAINT poln_rcvqty_ck  CHECK (received_qty >= 0
                                    AND received_qty <= ordered_qty),
  CONSTRAINT poln_untprc_ck  CHECK (unit_price >= 0),
  CONSTRAINT poln_sttype_ck  CHECK (status_type = 'POSTATUS'),
  -- Requisition back-link is all-or-nothing.
  CONSTRAINT poln_srcreq_ck  CHECK (
    (source_requisition_number IS NULL AND source_requisition_line_number IS NULL)
    OR
    (source_requisition_number IS NOT NULL AND source_requisition_line_number IS NOT NULL)
  ),

  -- FKs ----------------------------------------------------------------------
  CONSTRAINT poln_hdr_fk FOREIGN KEY (company_code, po_number)
    REFERENCES po_header (company_code, po_number)
    ON DELETE RESTRICT ON UPDATE RESTRICT,
  CONSTRAINT poln_item_fk FOREIGN KEY (company_code, item_number)
    REFERENCES item (company_code, item_number)
    ON DELETE RESTRICT ON UPDATE RESTRICT,
  CONSTRAINT poln_uom_fk FOREIGN KEY (uom_code)
    REFERENCES uom (uom_code)
    ON DELETE RESTRICT ON UPDATE RESTRICT,
  CONSTRAINT poln_stat_fk FOREIGN KEY (status_type, status_code)
    REFERENCES code_master (code_type, code_value)
    ON DELETE RESTRICT ON UPDATE RESTRICT,
  -- Composite FK to requisition_line — DB2 for i treats a composite FK
  -- where any component is NULL as satisfied (MATCH SIMPLE default), so
  -- manual PO lines with both source columns NULL pass without needing
  -- a special "manual" placeholder row.
  CONSTRAINT poln_srcreq_fk FOREIGN KEY
    (company_code, source_requisition_number, source_requisition_line_number)
    REFERENCES requisition_line (company_code, requisition_number, line_number)
    ON DELETE RESTRICT ON UPDATE RESTRICT
);

LABEL ON TABLE po_line IS
  'PERP purchase order line';

LABEL ON COLUMN po_line (
  company_code                 IS 'Company code (FK to po_header)',
  po_number                    IS 'PO number (FK to po_header)',
  line_number                  IS 'Line number within PO (PK)',
  item_number                  IS 'Item number (FK to item)',
  ordered_qty                  IS 'Quantity ordered',
  received_qty                 IS 'Running receipt total (updated by receipts)',
  uom_code                     IS 'Unit of measure (FK to uom)',
  unit_price                   IS 'Unit price snapshot at PO creation',
  expected_receipt_date        IS 'Expected receipt date (null on blanket)',
  status_code                  IS 'Line status (FK to code_master POSTATUS)',
  status_type                  IS 'Status type discriminator (constant)',
  source_requisition_number    IS 'Source requisition number, null on manual PO',
  source_requisition_line_number IS 'Source requisition line, null on manual PO',
  is_active                    IS 'Active flag (Y/N, soft delete)'
);
