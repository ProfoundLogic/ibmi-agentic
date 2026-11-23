-- ---------------------------------------------------------------------------
-- Seed:    020_payment_terms
-- Module:  perp
-- Purpose: Populate code_master with the PAYTERMS code_type that
--          vendor.payment_terms_code FKs to. Idempotent -- DELETE by
--          code_type first, then INSERT. Safe to re-run.
-- Epic:    PERP-5 (PERP-28)
-- ---------------------------------------------------------------------------

DELETE FROM code_master
 WHERE code_type = 'PAYTERMS';

-- PAYTERMS — vendor payment terms -----------------------------------------
INSERT INTO code_master
  (code_type, code_value, description, short_desc, sort_order) VALUES
  ('PAYTERMS','COD',   'Cash on delivery', 'COD',    10),
  ('PAYTERMS','NET15', 'Net 15 days',      'Net 15', 20),
  ('PAYTERMS','NET30', 'Net 30 days',      'Net 30', 30),
  ('PAYTERMS','NET60', 'Net 60 days',      'Net 60', 40),
  ('PAYTERMS','PREPAID','Prepaid',         'Prepaid',50);
