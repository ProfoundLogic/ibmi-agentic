-- ---------------------------------------------------------------------------
-- Table:   document_sequence  (system name DOCSEQ)
-- Module:  perp
-- Purpose: High-water mark per (company, document_type). Callers
--          atomically bump current_number to get the next doc number
--          for requisitions, POs, receipts.
-- Epic:    PERP-2 (Company & System Reference)
-- ---------------------------------------------------------------------------

CREATE TABLE document_sequence FOR SYSTEM NAME DOCSEQ (

  -- Composite key ---------------------------------------------------------
  company_code    FOR COLUMN COMPCD  CHAR(3)      NOT NULL,
  document_type   FOR COLUMN DOCTYP  VARCHAR(20)  NOT NULL,

  -- Sequence state --------------------------------------------------------
  current_number  FOR COLUMN CURNUM  BIGINT       NOT NULL DEFAULT 0,
  description     FOR COLUMN DOCDSC  VARCHAR(60)  NOT NULL DEFAULT '',

  -- Standard audit block --------------------------------------------------
  created_at      FOR COLUMN CRTAT   TIMESTAMP    NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_by      FOR COLUMN CRTBY   VARCHAR(18)  NOT NULL DEFAULT USER,
  updated_at      FOR COLUMN UPDAT   TIMESTAMP    NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_by      FOR COLUMN UPDBY   VARCHAR(18)  NOT NULL DEFAULT USER,
  is_active       FOR COLUMN ISACT   CHAR(1)      NOT NULL DEFAULT 'Y',

  -- Composite PK ----------------------------------------------------------
  PRIMARY KEY (company_code, document_type),

  -- Constraints -----------------------------------------------------------
  CONSTRAINT docseq_curnum_ck CHECK (current_number >= 0),
  CONSTRAINT docseq_isact_ck  CHECK (is_active IN ('Y','N')),

  -- FK to company ---------------------------------------------------------
  CONSTRAINT docseq_company_fk FOREIGN KEY (company_code)
    REFERENCES company (company_code)
    ON DELETE RESTRICT ON UPDATE RESTRICT
);

LABEL ON TABLE document_sequence IS
  'PERP per-company doc-number high-water marks';

LABEL ON COLUMN document_sequence (
  company_code   IS 'Company code (FK to company)',
  document_type  IS 'Document type key (REQ, PO, RCP, ...)',
  current_number IS 'Last-issued document number (bump before use)',
  description    IS 'Human-readable document-type label',
  is_active      IS 'Active flag (Y/N, soft delete)'
);
