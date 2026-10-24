-- ---------------------------------------------------------------------------
-- Table:   po_line_schedule  (auto-derived short name)
-- Module:  perp
-- Purpose: Blanket-PO delivery schedule. Optional per line — a normal PO
--          line has zero schedule rows and relies on
--          po_line.expected_receipt_date; a blanket line has one row per
--          scheduled delivery. scheduled_qty rolls up per line to the
--          same ordered_qty on po_line (soft warning if it doesn't;
--          see PERP-40 — the maintenance program flags but does not
--          block on the mismatch, since demo flows often adjust
--          ordered_qty after schedules are set).
-- Epic:    PERP-7 (PERP-37)
-- ---------------------------------------------------------------------------

-- 'po_line_schedule' is 16 chars — DB2 will auto-derive a short name
-- (same shape as requisition_header/line, confirmed via DSPOBJD after
-- build). FOR SYSTEM NAME omitted per DDL_STYLE_GUIDE.md Sec.2.
CREATE TABLE po_line_schedule (

  -- Composite key (per-company, per-PO, per-line, per-schedule-seq) ---------
  company_code   FOR COLUMN COMPCD  CHAR(3)       NOT NULL,
  po_number      FOR COLUMN PONBR   BIGINT        NOT NULL,
  line_number    FOR COLUMN LINNBR  INTEGER       NOT NULL,
  schedule_seq   FOR COLUMN SCHSEQ  INTEGER       NOT NULL,

  -- Schedule --------------------------------------------------------------
  scheduled_date FOR COLUMN SCHDT   DATE          NOT NULL,
  scheduled_qty  FOR COLUMN SCHQTY  DECIMAL(15,4) NOT NULL DEFAULT 0,
  received_qty   FOR COLUMN RCVQTY  DECIMAL(15,4) NOT NULL DEFAULT 0,

  -- 'notes' auto-derives to a valid ≤10-char system name; explicit
  -- FOR COLUMN NOTES raises SQL0612 (same finding as elsewhere) — omit.
  notes          VARCHAR(240)  NOT NULL DEFAULT '',

  -- Standard audit block --------------------------------------------------
  created_at     FOR COLUMN CRTAT   TIMESTAMP     NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_by     FOR COLUMN CRTBY   VARCHAR(18)   NOT NULL DEFAULT USER,
  updated_at     FOR COLUMN UPDAT   TIMESTAMP     NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_by     FOR COLUMN UPDBY   VARCHAR(18)   NOT NULL DEFAULT USER,
  is_active      FOR COLUMN ISACT   CHAR(1)       NOT NULL DEFAULT 'Y',

  -- Composite primary key --------------------------------------------------
  PRIMARY KEY (company_code, po_number, line_number, schedule_seq),

  -- Constraints -----------------------------------------------------------
  CONSTRAINT posch_isact_ck  CHECK (is_active IN ('Y','N')),
  CONSTRAINT posch_seq_ck    CHECK (schedule_seq > 0),
  CONSTRAINT posch_schqty_ck CHECK (scheduled_qty > 0),
  CONSTRAINT posch_rcvqty_ck CHECK (received_qty >= 0
                                    AND received_qty <= scheduled_qty),

  -- FK to po_line -----------------------------------------------------------
  CONSTRAINT posch_ln_fk FOREIGN KEY (company_code, po_number, line_number)
    REFERENCES po_line (company_code, po_number, line_number)
    ON DELETE RESTRICT ON UPDATE RESTRICT
);

LABEL ON TABLE po_line_schedule IS
  'PERP PO blanket line delivery schedule';

LABEL ON COLUMN po_line_schedule (
  company_code   IS 'Company code (FK to po_line)',
  po_number      IS 'PO number (FK to po_line)',
  line_number    IS 'Line number (FK to po_line)',
  schedule_seq   IS 'Schedule sequence within line (PK)',
  scheduled_date IS 'Scheduled delivery date',
  scheduled_qty  IS 'Quantity scheduled for this delivery',
  received_qty   IS 'Quantity received against this schedule',
  notes          IS 'Schedule notes',
  is_active      IS 'Active flag (Y/N, soft delete)'
);
