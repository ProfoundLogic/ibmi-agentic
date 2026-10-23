-- ---------------------------------------------------------------------------
-- Table:   warehouse_layout  (system name WLAYOUT)
-- Module:  perp
-- Purpose: Per-company warehouse grid config -- aisle x bay x shelf counts
--          and bin/aisle dimensions in meters. One row per company; drives
--          the coordinate derivation in the whcoord service (PERP-27).
-- Epic:    PERP-4 (3D Warehouse Map) / PERP-25
--
-- Notes on DB2 for i syntax:
--   * 'notes' is <=10 chars and pure alphanumeric, so it already auto-
--     derives to system name NOTES -- an explicit FOR COLUMN NOTES would
--     raise SQL7029 (same rule as 'item'/'uom'/'company'; see
--     DDL_STYLE_GUIDE.md sec 2). Omitted here for that reason.
-- ---------------------------------------------------------------------------

CREATE TABLE warehouse_layout FOR SYSTEM NAME WLAYOUT (

  -- Primary key (one row per company) --------------------------------------
  company_code    FOR COLUMN COMPCD  CHAR(3)       NOT NULL,

  -- Descriptive ---------------------------------------------------------------
  warehouse_name  FOR COLUMN WHSNM   VARCHAR(60)   NOT NULL DEFAULT '',

  -- Grid dimensions ---------------------------------------------------------
  aisle_count     FOR COLUMN ASLCNT  INTEGER       NOT NULL DEFAULT 1,
  bays_per_aisle  FOR COLUMN BAYSPA  INTEGER       NOT NULL DEFAULT 1,
  shelves_per_bay FOR COLUMN SHLFSB  INTEGER       NOT NULL DEFAULT 1,

  -- Bin dimensions, in meters -------------------------------------------------
  bin_width_m     FOR COLUMN BINWID  DECIMAL(9,4)  NOT NULL DEFAULT 1,
  bin_depth_m     FOR COLUMN BINDEP  DECIMAL(9,4)  NOT NULL DEFAULT 1,
  bin_height_m    FOR COLUMN BINHGT  DECIMAL(9,4)  NOT NULL DEFAULT 1,
  aisle_spacing_m FOR COLUMN ASLSPC  DECIMAL(9,4)  NOT NULL DEFAULT 2,

  -- Free-text notes -- system name auto-derives, see header note ------------
  notes           VARCHAR(60)                     NOT NULL DEFAULT '',

  -- Standard audit block --------------------------------------------------
  created_at      FOR COLUMN CRTAT   TIMESTAMP     NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_by      FOR COLUMN CRTBY   VARCHAR(18)   NOT NULL DEFAULT USER,
  updated_at      FOR COLUMN UPDAT   TIMESTAMP     NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_by      FOR COLUMN UPDBY   VARCHAR(18)   NOT NULL DEFAULT USER,
  is_active       FOR COLUMN ISACT   CHAR(1)       NOT NULL DEFAULT 'Y',

  -- Primary key -------------------------------------------------------------
  PRIMARY KEY (company_code),

  -- Constraints -------------------------------------------------------------
  CONSTRAINT wlayout_aslcnt_ck CHECK (aisle_count >= 1),
  CONSTRAINT wlayout_bayspa_ck CHECK (bays_per_aisle >= 1),
  CONSTRAINT wlayout_shlfsb_ck CHECK (shelves_per_bay >= 1),
  CONSTRAINT wlayout_binwid_ck CHECK (bin_width_m > 0),
  CONSTRAINT wlayout_bindep_ck CHECK (bin_depth_m > 0),
  CONSTRAINT wlayout_binhgt_ck CHECK (bin_height_m > 0),
  CONSTRAINT wlayout_aslspc_ck CHECK (aisle_spacing_m >= 0),
  CONSTRAINT wlayout_isact_ck  CHECK (is_active IN ('Y','N')),

  -- FK ------------------------------------------------------------------------
  CONSTRAINT wlayout_company_fk FOREIGN KEY (company_code)
    REFERENCES company (company_code)
    ON DELETE RESTRICT ON UPDATE RESTRICT
);

LABEL ON TABLE warehouse_layout IS
  'Warehouse layout config (one row per company)';

LABEL ON COLUMN warehouse_layout (
  company_code    IS 'Company code (PK, FK to company)',
  warehouse_name  IS 'Warehouse name',
  aisle_count     IS 'Number of aisles in the grid',
  bays_per_aisle  IS 'Bays per aisle',
  shelves_per_bay IS 'Shelves per bay',
  bin_width_m     IS 'Bin width in meters (x axis)',
  bin_depth_m     IS 'Bin depth in meters',
  bin_height_m    IS 'Bin height in meters (z axis)',
  aisle_spacing_m IS 'Spacing between aisles in meters (y axis)',
  notes           IS 'Free-text notes',
  is_active       IS 'Active flag (Y/N, soft delete)'
);
