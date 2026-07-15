-- ---------------------------------------------------------------------------
-- Table:   vendor  (system name VENDOR)
-- Module:  perp
-- Purpose: Vendor master. Buyer assignment (FK to perp_user) is later
--          snapshotted onto po_header at PO creation time -- see the
--          "buyer snapshot pattern" on the Design Decisions & Conventions
--          page. Payment terms is a code_master (PAYTERMS) lookup.
-- Epic:    PERP-5 (PERP-28)
-- ---------------------------------------------------------------------------

-- 'vendor' auto-derives to system name VENDOR; specifying FOR SYSTEM NAME
-- with the same value raises SQL7029 (same rule as company/item), so it's
-- omitted here.
CREATE TABLE vendor (

  -- Composite key (per-company) ---------------------------------------------
  company_code       FOR COLUMN COMPCD  CHAR(3)      NOT NULL,
  vendor_code        FOR COLUMN VNDCD   VARCHAR(10)  NOT NULL,

  -- Descriptive columns -----------------------------------------------------
  vendor_name        FOR COLUMN VNDNM   VARCHAR(60)  NOT NULL,

  -- Address block (same shape as company.table.sql) --------------------------
  address_line1      FOR COLUMN ADDR1   VARCHAR(60)  NOT NULL DEFAULT '',
  address_line2      FOR COLUMN ADDR2   VARCHAR(60)  NOT NULL DEFAULT '',
  city_name          FOR COLUMN CITY    VARCHAR(40)  NOT NULL DEFAULT '',
  state_code         FOR COLUMN STATE   VARCHAR(3)   NOT NULL DEFAULT '',
  postal_code        FOR COLUMN POSTCD  VARCHAR(12)  NOT NULL DEFAULT '',
  country_code       FOR COLUMN CNTRY   VARCHAR(3)   NOT NULL DEFAULT 'US',

  -- Contact info --------------------------------------------------------------
  phone_number       FOR COLUMN PHONE   VARCHAR(20)  NOT NULL DEFAULT '',
  email_address      FOR COLUMN EMAIL   VARCHAR(120) NOT NULL DEFAULT '',
  contact_name       FOR COLUMN CNTCNM  VARCHAR(60)  NOT NULL DEFAULT '',

  -- Buyer assignment (FK to perp_user) ---------------------------------------
  buyer_code         FOR COLUMN BUYCD   CHAR(10)     NOT NULL,

  -- Payment terms (constant discriminator + code_master FK) -----------------
  payment_terms_code FOR COLUMN PTCODE  VARCHAR(20)  NOT NULL DEFAULT 'NET30',
  payment_terms_type FOR COLUMN PTTYPE  VARCHAR(20)  NOT NULL DEFAULT 'PAYTERMS',

  tax_id             FOR COLUMN TAXID   VARCHAR(20)  NOT NULL DEFAULT '',

  -- Standard audit block --------------------------------------------------
  created_at         FOR COLUMN CRTAT   TIMESTAMP    NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_by         FOR COLUMN CRTBY   VARCHAR(18)  NOT NULL DEFAULT USER,
  updated_at         FOR COLUMN UPDAT   TIMESTAMP    NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_by         FOR COLUMN UPDBY   VARCHAR(18)  NOT NULL DEFAULT USER,
  is_active          FOR COLUMN ISACT   CHAR(1)      NOT NULL DEFAULT 'Y',

  -- Composite primary key ---------------------------------------------------
  PRIMARY KEY (company_code, vendor_code),

  -- Constraints -----------------------------------------------------------
  CONSTRAINT vendor_isact_ck  CHECK (is_active IN ('Y','N')),
  CONSTRAINT vendor_pttyp_ck  CHECK (payment_terms_type = 'PAYTERMS'),

  -- FKs -----------------------------------------------------------------------
  CONSTRAINT vendor_company_fk FOREIGN KEY (company_code)
    REFERENCES company (company_code)
    ON DELETE RESTRICT ON UPDATE RESTRICT,
  CONSTRAINT vendor_buyer_fk FOREIGN KEY (buyer_code)
    REFERENCES perp_user (user_code)
    ON DELETE RESTRICT ON UPDATE RESTRICT,
  CONSTRAINT vendor_ptcode_fk FOREIGN KEY (payment_terms_type, payment_terms_code)
    REFERENCES code_master (code_type, code_value)
    ON DELETE RESTRICT ON UPDATE RESTRICT
);

LABEL ON TABLE vendor IS
  'PERP vendor master';

LABEL ON COLUMN vendor (
  company_code       IS 'Company code (FK to company)',
  vendor_code         IS 'Vendor code (natural key within company)',
  vendor_name         IS 'Vendor name',
  address_line1       IS 'Address line 1',
  address_line2       IS 'Address line 2',
  city_name           IS 'City',
  state_code          IS 'State / province code',
  postal_code         IS 'Postal / ZIP code',
  country_code        IS 'ISO country code',
  phone_number        IS 'Phone number',
  email_address       IS 'Email address',
  contact_name        IS 'Contact person name',
  buyer_code          IS 'Buyer (FK to perp_user)',
  payment_terms_code  IS 'Payment terms (FK to code_master)',
  payment_terms_type  IS 'Payment terms type discriminator (constant)',
  tax_id              IS 'Tax ID',
  is_active           IS 'Active flag (Y/N, soft delete)'
);
