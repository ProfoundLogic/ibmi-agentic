-- ---------------------------------------------------------------------------
-- Table:   reconciliation_log  (auto-derived short name)
-- Module:  perp
-- Purpose: Audit trail of lot-vs-item-balance drift detection and repair.
--          Written exclusively by the lotrecon service program (PERP-44)
--          -- no RPG program inserts into this table interactively. Has no
--          natural key (a reconciliation run doesn't identify itself by
--          anything but "when it happened"), so per DDL_STYLE_GUIDE.md
--          Sec.5 this is the one table in the module allowed a surrogate
--          GENERATED ALWAYS AS IDENTITY key.
--
--          reconciled_by is NOT FK'd to perp_user -- an automated run
--          stamps the literal 'CODERFLOW', which is not a row in the human
--          user directory. Same rationale/pattern as
--          requisition_header.approved_by.
-- Epic:    PERP-8 (PERP-42)
-- ---------------------------------------------------------------------------

-- 'reconciliation_log' is 19 chars -- DB2 will auto-derive a short name.
-- FOR SYSTEM NAME omitted per DDL_STYLE_GUIDE.md Sec.2; confirm the real
-- short name via DSPOBJD after build.
CREATE TABLE reconciliation_log (

  -- Composite key -- company_code leads per Sec.3, reconciliation_id is
  -- the IDENTITY surrogate (globally unique on its own; company_code
  -- still leads the PK for multi-tenant consistency with every other
  -- table in the module). ------------------------------------------------
  company_code       FOR COLUMN COMPCD  CHAR(3)       NOT NULL,
  reconciliation_id  FOR COLUMN RECONID INTEGER GENERATED ALWAYS AS IDENTITY
                                         (START WITH 1 INCREMENT BY 1) NOT NULL,

  -- What was reconciled and when --------------------------------------------
  item_number         FOR COLUMN ITMNBR  VARCHAR(25)   NOT NULL,
  run_timestamp        FOR COLUMN RUNTS   TIMESTAMP     NOT NULL DEFAULT CURRENT_TIMESTAMP,

  -- Before / after balances --------------------------------------------------
  item_qty_before       FOR COLUMN QTYBEF  DECIMAL(15,4) NOT NULL,
  lot_sum_before         FOR COLUMN LOTBEF  DECIMAL(15,4) NOT NULL,
  qty_after               FOR COLUMN QTYAFT  DECIMAL(15,4) NOT NULL,

  -- Who/what ran the reconciliation. Not FK'd -- see header note.
  reconciled_by             FOR COLUMN RCNBY   VARCHAR(18)   NOT NULL,

  -- 'notes' auto-derives to a valid <=10-char system name; explicit
  -- FOR COLUMN NOTES raises SQL0612 (same finding as elsewhere) -- omit.
  notes                       VARCHAR(240)  NOT NULL DEFAULT '',

  -- Standard audit block. This table is append-only in practice (no
  -- program updates a row after insert), but every PERP table carries
  -- the same five columns for module consistency. -----------------------
  created_at     FOR COLUMN CRTAT   TIMESTAMP     NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_by     FOR COLUMN CRTBY   VARCHAR(18)   NOT NULL DEFAULT USER,
  updated_at     FOR COLUMN UPDAT   TIMESTAMP     NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_by     FOR COLUMN UPDBY   VARCHAR(18)   NOT NULL DEFAULT USER,
  is_active      FOR COLUMN ISACT   CHAR(1)       NOT NULL DEFAULT 'Y',

  -- Composite primary key ---------------------------------------------------
  PRIMARY KEY (company_code, reconciliation_id),

  -- Constraints ---------------------------------------------------------------
  CONSTRAINT rcnlog_isact_ck  CHECK (is_active IN ('Y','N')),
  CONSTRAINT rcnlog_qtybef_ck CHECK (item_qty_before >= 0),
  CONSTRAINT rcnlog_lotbef_ck CHECK (lot_sum_before >= 0),
  CONSTRAINT rcnlog_qtyaft_ck CHECK (qty_after >= 0),

  -- FK to item ------------------------------------------------------------
  CONSTRAINT rcnlog_item_fk FOREIGN KEY (company_code, item_number)
    REFERENCES item (company_code, item_number)
    ON DELETE RESTRICT ON UPDATE RESTRICT
);

LABEL ON TABLE reconciliation_log IS
  'PERP lot vs item balance reconciliation audit log';

LABEL ON COLUMN reconciliation_log (
  company_code       IS 'Company code (FK to item)',
  reconciliation_id  IS 'Surrogate identity key (no natural key exists)',
  item_number         IS 'Item reconciled (FK to item)',
  run_timestamp        IS 'When the reconciliation ran',
  item_qty_before       IS 'item.qty_on_hand before repair',
  lot_sum_before         IS 'SUM(item_lot.qty_on_hand) before repair',
  qty_after               IS 'item.qty_on_hand after repair (= lot_sum_before)',
  reconciled_by             IS 'User code or CODERFLOW, not FK-enforced',
  notes                       IS 'Reconciliation notes',
  is_active                  IS 'Active flag (Y/N, soft delete)'
);
