-- ---------------------------------------------------------------------------
-- Seed:    025_approval_thresholds
-- Module:  perp
-- Purpose: Populate company_config with the auto-approval thresholds
--          reqauto (PERP-36) reads: a minimum confidence_pct and a maximum
--          total_estimated_cost. Both must pass for auto-approval.
--          Idempotent -- DELETE by config_key first, then INSERT.
--
--          Numbered 025 (not 030) to leave 030 free for PERP-9's planned
--          030_items.sql (see the ACME Seed Dataset Spec Confluence page,
--          2512060417) -- these two seed scripts were written independently
--          and would otherwise collide. Key names here
--          (approval.auto_threshold / approval.auto_max_amount) also differ
--          from that page's originally-planned auto_approve_threshold_pct /
--          auto_approve_max_amount; reconciled on the spec page rather than
--          silently diverging -- see that page for the full note.
-- Epic:    PERP-6 (PERP-36)
-- ---------------------------------------------------------------------------

DELETE FROM company_config
 WHERE config_key IN ('approval.auto_threshold', 'approval.auto_max_amount');

INSERT INTO company_config
  (company_code, config_key, config_value, description) VALUES
  ('ACM', 'approval.auto_threshold',  '85',     'Min confidence_pct for CoderFlow auto-approval'),
  ('ACM', 'approval.auto_max_amount', '500.00', 'Max total_estimated_cost for CoderFlow auto-approval'),
  ('BET', 'approval.auto_threshold',  '85',     'Min confidence_pct for CoderFlow auto-approval'),
  ('BET', 'approval.auto_max_amount', '500.00', 'Max total_estimated_cost for CoderFlow auto-approval');
