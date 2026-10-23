-- ---------------------------------------------------------------------------
-- Table:   po_receipt  (system name PO_RECEIPT, auto-derived)
-- Module:  perp
-- Purpose: Purchase order receipt header. One row per receiving event
--          against a PO (multiple receipts per PO are expected -- scheduled
--          / blanket deliveries and partial shipments both land here).
--          receipt_number is allocated from document_sequence via
--          docseq_next('RCP'), same idiom as po_number/requisition_number.
--          status_code defaults to POSTED -- unlike po_header/requisition
--          there is no DRAFT workflow here: creating a receipt line IS the
--          act of receiving (see rcventr, PERP-43), so the header is
--          POSTED from creation. RCPSTATUS (DRAFT/POSTED/VOIDED) was
--          already seeded in code_master by PERP-2/PERP-15; VOIDED exists
--          for a future void flow but no program sets it in this epic --
--          same "lookup exists, no program uses it yet" deferral pattern
--          as POSTATUS.CANCELLED noted in PERP-7's recap.
-- Epic:    PERP-8 (PERP-42)
-- ---------------------------------------------------------------------------

-- 'po_receipt' (10 chars) is itself a valid system name, so DB2 auto-derives
-- PO_RECEIPT and an explicit FOR SYSTEM NAME would raise SQL7029 -- same
-- rule as po_line/po_header (DDL_STYLE_GUIDE.md Sec.2).
CREATE TABLE po_receipt (

  -- Composite key (per-company) ---------------------------------------------
  company_code   FOR COLUMN COMPCD  CHAR(3)      NOT NULL,
  receipt_number FOR COLUMN RCPNBR  BIGINT       NOT NULL,

  -- Source PO ---------------------------------------------------------------
  po_number      FOR COLUMN PONBR   BIGINT       NOT NULL,

  -- Receipt metadata ----------------------------------------------------------
  receipt_date   FOR COLUMN RCPDT   DATE         NOT NULL DEFAULT CURRENT_DATE,
  -- received_by is NOT FK'd to perp_user's role -- any perp_user may
  -- receive; role-based restriction is out of scope for this epic.
  received_by    FOR COLUMN RCVBY   CHAR(10)     NOT NULL,

  -- Status (FK to code_master RCPSTATUS) -------------------------------------
  status_code    FOR COLUMN STCODE  VARCHAR(20)  NOT NULL DEFAULT 'POSTED',
  status_type    FOR COLUMN STTYPE  VARCHAR(20)  NOT NULL DEFAULT 'RCPSTATUS',

  -- Long-form notes. 'notes' (5 chars) is itself a valid system name so
  -- explicit FOR COLUMN NOTES raises SQL0612 -- omit and let it auto-derive
  -- (same finding as po_header/requisition_header, DDL_STYLE_GUIDE.md Sec.2).
  notes          CLOB(16K),

  -- Standard audit block ----------------------------------------------------
  created_at     FOR COLUMN CRTAT   TIMESTAMP    NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_by     FOR COLUMN CRTBY   VARCHAR(18)  NOT NULL DEFAULT USER,
  updated_at     FOR COLUMN UPDAT   TIMESTAMP    NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_by     FOR COLUMN UPDBY   VARCHAR(18)  NOT NULL DEFAULT USER,
  is_active      FOR COLUMN ISACT   CHAR(1)      NOT NULL DEFAULT 'Y',

  -- Composite primary key ---------------------------------------------------
  PRIMARY KEY (company_code, receipt_number),

  -- Constraints ---------------------------------------------------------------
  CONSTRAINT porcp_isact_ck  CHECK (is_active IN ('Y','N')),
  CONSTRAINT porcp_sttype_ck CHECK (status_type = 'RCPSTATUS'),

  -- FKs -----------------------------------------------------------------------
  CONSTRAINT porcp_company_fk FOREIGN KEY (company_code)
    REFERENCES company (company_code)
    ON DELETE RESTRICT ON UPDATE RESTRICT,
  CONSTRAINT porcp_po_fk FOREIGN KEY (company_code, po_number)
    REFERENCES po_header (company_code, po_number)
    ON DELETE RESTRICT ON UPDATE RESTRICT,
  CONSTRAINT porcp_rcvby_fk FOREIGN KEY (received_by)
    REFERENCES perp_user (user_code)
    ON DELETE RESTRICT ON UPDATE RESTRICT,
  CONSTRAINT porcp_stat_fk FOREIGN KEY (status_type, status_code)
    REFERENCES code_master (code_type, code_value)
    ON DELETE RESTRICT ON UPDATE RESTRICT
);

LABEL ON TABLE po_receipt IS
  'PERP purchase order receipt header';

LABEL ON COLUMN po_receipt (
  company_code   IS 'Company code (FK to po_header)',
  receipt_number IS 'Receipt number (docseq RCP)',
  po_number      IS 'Source PO number (FK to po_header)',
  receipt_date   IS 'Date goods were received',
  received_by    IS 'Receiver (FK to perp_user)',
  status_code    IS 'Status (FK to code_master RCPSTATUS)',
  status_type    IS 'Status type discriminator (constant)',
  notes          IS 'Long-form notes',
  is_active      IS 'Active flag (Y/N, soft delete)'
);
