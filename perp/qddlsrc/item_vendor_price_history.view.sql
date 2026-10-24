-- ---------------------------------------------------------------------------
-- View:    item_vendor_price_history
-- Module:  perp
-- Purpose: Effective-dated price history joined to vendor_name, for the
--          pricing-over-time graph demo. Data source for the itmvprcq
--          service program (PERP-32) and for any future UI that queries
--          it directly. Callers order by effective_from themselves --
--          this view carries no ORDER BY (DB2 for i views don't guarantee
--          storage order without one, and ORDER BY on a view without
--          FETCH FIRST isn't portable).
-- Epic:    PERP-5 (PERP-32)
-- ---------------------------------------------------------------------------

CREATE VIEW item_vendor_price_history
  (company_code, item_number, vendor_code, vendor_name,
   effective_from, effective_to, unit_price, currency_code) AS
  SELECT p.company_code, p.item_number, p.vendor_code, v.vendor_name,
         p.effective_from, p.effective_to, p.unit_price, p.currency_code
    FROM item_vendor_price p
    JOIN vendor v
      ON v.company_code = p.company_code AND v.vendor_code = p.vendor_code;

LABEL ON TABLE item_vendor_price_history IS
  'PERP item-vendor price history (graph source)';
