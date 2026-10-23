-- ---------------------------------------------------------------------------
-- Index:   item_vendor_preferred_ak
-- Module:  perp
-- Purpose: Partial unique index enforcing at most one preferred vendor
--          (is_preferred = 'Y') per (company_code, item_number). Any
--          number of non-preferred rows are allowed.
-- Epic:    PERP-5 (PERP-28)
-- ---------------------------------------------------------------------------

CREATE UNIQUE INDEX item_vendor_preferred_ak
  ON item_vendor (company_code, item_number)
  WHERE is_preferred = 'Y';
