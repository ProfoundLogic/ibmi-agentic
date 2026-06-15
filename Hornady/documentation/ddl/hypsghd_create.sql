-- ============================================================================
-- Combined create script — runs in dependency order.
-- Use:
--   system> RUNSQLSTM SRCFILE(...) SRCMBR(HYPSGHD_CREATE) DFTRDBCOL(AITSK00030)
-- or paste into ACS Run SQL Scripts.
--
-- Drops first so it's idempotent.  DROP ignores not-found errors via CONTINUE.
-- ============================================================================
DROP TABLE AITSK00030/HYPSGTD;
DROP TABLE AITSK00030/HYPSGLD;
DROP TABLE AITSK00030/HYPSGDT;
DROP TABLE AITSK00030/HYPSGHD;
DROP TABLE AITSK00030/HYPSGCU;
DROP TABLE AITSK00030/HYPSGSD;

-- HYPSGHD first (lightest), then HYPSGCU (matches GHGRP# foreign-ish ref),
-- then HYPSGDT (depends on the above), then HYPSGLD / HYPSGTD (children of
-- HYPSGDT for the lot / tote layers).  HYPSGSD is independent.

-- (Bodies are in the per-table .sql files; copy them in if running this
--  combined script standalone, or use RUNSQLSTM SRCFILE() for each separately.)
