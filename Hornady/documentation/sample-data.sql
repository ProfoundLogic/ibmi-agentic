-- ============================================================================
-- Hornady POC -- sample data for the Hornady-menu options.
--
--   Option 1  (HYR0600)  Shipment Processing       -- 30 shipments, paginates
--   Option 3  (HYR0606)  Shipment Lot Inquiry      -- 15 lots on the default
--                                                     shipment (1001/1/1/1)
--                                                     so the inquiry subfile
--                                                     pages cleanly across
--                                                     3 pages of SFLPAG=10
--
-- All inserts target library AITSK00030.  Idempotent: every section starts
-- with a matching DELETE so reruns produce the same state.
--
-- Run via ACS "Run SQL Scripts", or from qsh:
--   system "runsqlstm srcstmf('Hornady/documentation/sample-data.sql')
--                     commit(*none) dftrdbcol(AITSK00030)"
-- or pipe to a qsh session with `Rfile -Qw`.
-- ============================================================================

------------------------------------------------------------------------------
-- 1. Employees (HREMPL) -- HYR0600 startup prompt validates against this.
------------------------------------------------------------------------------
DELETE FROM HREMPL WHERE EMEMPL IN (12345, 22001, 30099);

INSERT INTO HREMPL (EMEMPL, EMFNAM, EMLNAM, EMDEPT, EMSTS) VALUES
  (12345, 'John',     'Smith',     'SHIP',  'A'),
  (22001, 'Jane',     'Doe',       'SHIP',  'A'),
  (30099, 'Bob',      'Jones',     'WAR',   'A');

------------------------------------------------------------------------------
-- 2. Customers (HDCUST) -- referenced by HYPSGCU.GCBLTO / .GCSHTO / .GCDSHP.
--    8 customers covering different states + ship-via mixes.
------------------------------------------------------------------------------
-- HDCUST schema corrected to CM-prefix (was CU-prefix); see hdcust.table.sql.
DELETE FROM HDCUST WHERE CMCUST BETWEEN 1000000 AND 1000999;

INSERT INTO HDCUST (CMCUST, CMCNA1, CMALPH, CMCCLS, CMLOC#, CMD01, CMD02, CMCITY, CMSTAT, CMZIP, CMCNTRY, CMSHPV, CMSTS) VALUES
  (1000100, 'ACME Sporting Goods',        'ACMESPORT',  'RTL', 0, '123 Main Street',     '',                  'Boise',         'ID', '837020000', 'USA', 'UP', 'A'),
  (1000200, 'Global Outdoor Retail',      'GLOBALOUT',  'RTL', 0, '4500 Pine Avenue',    'Suite 201',         'Denver',        'CO', '802120000', 'USA', 'FX', 'A'),
  (1000300, 'Mountain View Outfitters',   'MOUNTVIEW',  'RTL', 0, '88 Ridge Road',       '',                  'Bozeman',       'MT', '597150000', 'USA', 'UP', 'A'),
  (1000400, 'Sportsman Wholesale Inc.',   'SPORTSMAN',  'WHL', 0, '1200 Industrial Way', '',                  'Houston',       'TX', '770100000', 'USA', 'YR', 'A'),
  (1000500, 'Northeast Hunting Supplies', 'NEHUNT',     'RTL', 0, '55 Commerce Blvd',    'Bldg C',            'Albany',        'NY', '122050000', 'USA', 'FX', 'A'),
  (1000600, 'Pacific Coast Firearms',     'PACCOAST',   'RTL', 0, '900 Harbor Dr',       '',                  'Tacoma',        'WA', '984020000', 'USA', 'UP', 'A'),
  (1000700, 'Heartland Ammunition Co.',   'HEARTAMM',   'WHL', 0, '300 Kansas Ave',      '',                  'Wichita',       'KS', '672020000', 'USA', 'YR', 'A'),
  (1000800, 'Southern Range Supply',      'SOUTHRNG',   'RTL', 0, '77 Magnolia Pkwy',    '',                  'Atlanta',       'GA', '303090000', 'USA', 'FX', 'A');

------------------------------------------------------------------------------
-- 3. Ship-via codes (HDSHPV and HYPSVCT). HYR0600 references SVSVDS/SVSVSV.
------------------------------------------------------------------------------
DELETE FROM HDSHPV WHERE SVSVSV IN ('UP', 'FX', 'YR', 'PU', 'BW');

INSERT INTO HDSHPV (SVSVDS, SVSVSV, SVDESC, SVSTS) VALUES
  ('UPS GROUND',        'UP', 'UPS Ground',                'A'),
  ('FEDEX GROUND',      'FX', 'FedEx Ground',              'A'),
  ('YELLOW FREIGHT',    'YR', 'YRC Freight (LTL)',         'A'),
  ('CUSTOMER PICKUP',   'PU', 'Customer Pick-up',          'A'),
  ('BIG WHEEL',         'BW', 'BW Restricted (no ship)',   'I');

DELETE FROM HYPSVCT WHERE VTSHPV IN ('UP', 'FX', 'YR', 'PU', 'BW');

INSERT INTO HYPSVCT (VTSHPV, VTSVDS, VTSTS) VALUES
  ('UP', 'UPS GROUND',      'A'),
  ('FX', 'FEDEX GROUND',    'A'),
  ('YR', 'YELLOW FREIGHT',  'A'),
  ('PU', 'CUSTOMER PICKUP', 'A'),
  ('BW', 'BIG WHEEL',       'I');

------------------------------------------------------------------------------
-- 4. Items (HDIMST) -- minimal item master so detail rows can reference them.
------------------------------------------------------------------------------
-- HDIMST schema corrected per HYR0600 / PICKERR refs (IMIMDS / IMIMWG /
-- IMUOMS / IMST instead of IMDESC / IMWGT / IMUOM / IMSTS); plus IMPCLS,
-- IMUDN4.  See hdimst.table.sql.
DELETE FROM HDIMST WHERE IMITEM LIKE 'DEMO-%';

INSERT INTO HDIMST (IMITEM, IMIMDS, IMIMWG, IMUOMS, IMPCLS, IMUDN4, IMST) VALUES
  ('DEMO-9MM-115',  '9mm Luger 115gr FMJ ammunition',   0.620, 'BX', 'AMM', '9mm Luger',     'A'),
  ('DEMO-9MM-124',  '9mm Luger 124gr FMJ ammunition',   0.640, 'BX', 'AMM', '9mm Luger',     'A'),
  ('DEMO-45ACP-230','45 ACP 230gr FMJ ammunition',      0.840, 'BX', 'AMM', '45 ACP',        'A'),
  ('DEMO-308-150',  '308 Win 150gr SST ammunition',     0.910, 'BX', 'AMM', '308 Winchester','A'),
  ('DEMO-223-55',   '223 Rem 55gr V-Max ammunition',    0.420, 'BX', 'AMM', '223 Remington', 'A'),
  ('DEMO-12GA-00',  '12 Gauge 00 Buckshot',             1.250, 'BX', 'AMM', '12 Gauge',      'A');

------------------------------------------------------------------------------
-- 5. Shipment group headers (HYPSGHD)
--    HYR0600 checks "exists (select * from HYPSGHD where GHGRP#=:S1GRP#)"
--    when reading the main subfile, so every GCGRP# needs a matching GHGRP#.
--    30 rows = enough to fill 2+ subfile pages of 14 rows each.
------------------------------------------------------------------------------
DELETE FROM HYPSGHD WHERE GHGRP# BETWEEN 1001 AND 1030;

INSERT INTO HYPSGHD (GHGRP#, GHSTS, GHUSER, GHDCLOC, GHSCUS, GHSCWS, GHTSTP1, GHTSTP2) VALUES
  (1001, 'O', 'AIDEMO', 'WEST', 1, 1, '2026-06-15-08.00.00.000000', '0001-01-01-00.00.00.000000'),
  (1002, 'O', 'AIDEMO', 'WEST', 1, 1, '2026-06-15-08.05.00.000000', '0001-01-01-00.00.00.000000'),
  (1003, 'O', 'AIDEMO', 'WEST', 1, 1, '2026-06-15-08.10.00.000000', '0001-01-01-00.00.00.000000'),
  (1004, 'O', 'AIDEMO', 'WEST', 1, 1, '2026-06-15-08.15.00.000000', '0001-01-01-00.00.00.000000'),
  (1005, 'O', 'AIDEMO', 'WEST', 1, 1, '2026-06-15-08.20.00.000000', '0001-01-01-00.00.00.000000'),
  (1006, 'O', 'AIDEMO', 'WEST', 1, 1, '2026-06-15-08.25.00.000000', '0001-01-01-00.00.00.000000'),
  (1007, 'O', 'AIDEMO', 'WEST', 1, 1, '2026-06-15-08.30.00.000000', '0001-01-01-00.00.00.000000'),
  (1008, 'O', 'AIDEMO', 'WEST', 1, 1, '2026-06-15-08.35.00.000000', '0001-01-01-00.00.00.000000'),
  (1009, 'O', 'AIDEMO', 'WEST', 1, 1, '2026-06-15-08.40.00.000000', '0001-01-01-00.00.00.000000'),
  (1010, 'O', 'AIDEMO', 'WEST', 1, 1, '2026-06-15-08.45.00.000000', '0001-01-01-00.00.00.000000'),
  (1011, 'O', 'AIDEMO', 'WEST', 1, 1, '2026-06-15-08.50.00.000000', '0001-01-01-00.00.00.000000'),
  (1012, 'O', 'AIDEMO', 'WEST', 1, 1, '2026-06-15-08.55.00.000000', '0001-01-01-00.00.00.000000'),
  (1013, 'O', 'AIDEMO', 'WEST', 1, 1, '2026-06-15-09.00.00.000000', '0001-01-01-00.00.00.000000'),
  (1014, 'O', 'AIDEMO', 'WEST', 1, 1, '2026-06-15-09.05.00.000000', '0001-01-01-00.00.00.000000'),
  (1015, 'O', 'AIDEMO', 'WEST', 1, 1, '2026-06-15-09.10.00.000000', '0001-01-01-00.00.00.000000'),
  (1016, 'O', 'AIDEMO', 'WEST', 1, 1, '2026-06-15-09.15.00.000000', '0001-01-01-00.00.00.000000'),
  (1017, 'O', 'AIDEMO', 'WEST', 1, 1, '2026-06-15-09.20.00.000000', '0001-01-01-00.00.00.000000'),
  (1018, 'O', 'AIDEMO', 'WEST', 1, 1, '2026-06-15-09.25.00.000000', '0001-01-01-00.00.00.000000'),
  (1019, 'O', 'AIDEMO', 'WEST', 1, 1, '2026-06-15-09.30.00.000000', '0001-01-01-00.00.00.000000'),
  (1020, 'O', 'AIDEMO', 'WEST', 1, 1, '2026-06-15-09.35.00.000000', '0001-01-01-00.00.00.000000'),
  (1021, 'O', 'AIDEMO', 'WEST', 1, 1, '2026-06-15-09.40.00.000000', '0001-01-01-00.00.00.000000'),
  (1022, 'O', 'AIDEMO', 'WEST', 1, 1, '2026-06-15-09.45.00.000000', '0001-01-01-00.00.00.000000'),
  (1023, 'O', 'AIDEMO', 'WEST', 1, 1, '2026-06-15-09.50.00.000000', '0001-01-01-00.00.00.000000'),
  (1024, 'O', 'AIDEMO', 'WEST', 1, 1, '2026-06-15-09.55.00.000000', '0001-01-01-00.00.00.000000'),
  (1025, 'O', 'AIDEMO', 'WEST', 1, 1, '2026-06-15-10.00.00.000000', '0001-01-01-00.00.00.000000'),
  (1026, 'O', 'AIDEMO', 'WEST', 1, 1, '2026-06-15-10.05.00.000000', '0001-01-01-00.00.00.000000'),
  (1027, 'O', 'AIDEMO', 'WEST', 1, 1, '2026-06-15-10.10.00.000000', '0001-01-01-00.00.00.000000'),
  (1028, 'O', 'AIDEMO', 'WEST', 1, 1, '2026-06-15-10.15.00.000000', '0001-01-01-00.00.00.000000'),
  (1029, 'O', 'AIDEMO', 'WEST', 1, 1, '2026-06-15-10.20.00.000000', '0001-01-01-00.00.00.000000'),
  (1030, 'O', 'AIDEMO', 'WEST', 1, 1, '2026-06-15-10.25.00.000000', '0001-01-01-00.00.00.000000');

------------------------------------------------------------------------------
-- 6. Shipment group / customer rows (HYPSGCU) -- the main subfile rows.
--    GCSTS = 'O'  (open) -- HYLSGCU1 has  S GCSTS COMP(EQ 'O') so open is required.
--    GCITCT and GCSHWT carry the displayed item-count / weight columns.
--    30 rows mapped round-robin across the 8 customers and 4 active ship-vias.
------------------------------------------------------------------------------
DELETE FROM HYPSGCU WHERE GCGRP# BETWEEN 1001 AND 1030;

INSERT INTO HYPSGCU (GCGRP#, GCCPTY, GCBLTO, GCSHTO, GCDSHP,
                     GCBTNM, GCSTNM, GCSTZP,
                     GCSHPV, GCSVDS, GCFRZN, GCFRTA, GCFTRM, GCCHGA,
                     GCITCT, GCSTCT, GCSHWT,
                     GCSTS, GCSTST, GCSTYP, GCPIKO, GCPANS, GCSPPK,
                     GCPRO#, GCBOL#,
                     GCLANE1, GCLANE2, GCLANE3, GCLANES,
                     GCMLLC, GCMLLI, GCHCFLD,
                     GCTSTP1, GCTSTP2) VALUES
  (1001, 1000100, 1000100, 1000100, 0, 'ACME Sporting Goods',        'ACME Sporting Goods Boise',     '837020000', 'UP', 'UPS Ground',     ' ',   45.00, 'PP', 0.00,  3, '  ',  22.50, 'O', ' ', 'P', ' ', ' ', ' ', 'PRO00001001', 1001, '01', '  ', '  ', '01    ', ' ', ' ', ' ', '2026-06-15-08.00.00.000000', '0001-01-01-00.00.00.000000'),
  (1002, 1000200, 1000200, 1000200, 0, 'Global Outdoor Retail',      'Global Outdoor Denver',         '802120000', 'FX', 'FedEx Ground',   ' ',   72.00, 'PP', 0.00,  6, '  ',  55.75, 'O', ' ', 'P', ' ', ' ', ' ', 'PRO00001002', 1002, '02', '  ', '  ', '02    ', ' ', ' ', ' ', '2026-06-15-08.05.00.000000', '0001-01-01-00.00.00.000000'),
  (1003, 1000300, 1000300, 1000300, 0, 'Mountain View Outfitters',   'Mountain View Bozeman',         '597150000', 'UP', 'UPS Ground',     ' ',   38.00, 'PP', 0.00,  2, '  ',  14.20, 'O', ' ', 'P', ' ', ' ', ' ', 'PRO00001003', 1003, '01', '  ', '  ', '01    ', ' ', ' ', ' ', '2026-06-15-08.10.00.000000', '0001-01-01-00.00.00.000000'),
  (1004, 1000400, 1000400, 1000400, 0, 'Sportsman Wholesale Inc.',   'Sportsman Houston',             '770100000', 'YR', 'YRC Freight',    ' ',  225.00, 'PP', 0.00, 24, '  ', 412.00, 'O', ' ', 'P', ' ', ' ', ' ', 'PRO00001004', 1004, '03', '  ', '  ', '03    ', ' ', ' ', ' ', '2026-06-15-08.15.00.000000', '0001-01-01-00.00.00.000000'),
  (1005, 1000500, 1000500, 1000500, 0, 'Northeast Hunting Supplies', 'Northeast Hunting Albany',      '122050000', 'FX', 'FedEx Ground',   ' ',   61.00, 'PP', 0.00,  5, '  ',  38.10, 'O', ' ', 'P', ' ', ' ', ' ', 'PRO00001005', 1005, '02', '  ', '  ', '02    ', ' ', ' ', ' ', '2026-06-15-08.20.00.000000', '0001-01-01-00.00.00.000000'),
  (1006, 1000600, 1000600, 1000600, 0, 'Pacific Coast Firearms',     'Pacific Coast Tacoma',          '984020000', 'UP', 'UPS Ground',     ' ',   52.00, 'PP', 0.00,  4, '  ',  28.30, 'O', ' ', 'P', ' ', ' ', ' ', 'PRO00001006', 1006, '01', '  ', '  ', '01    ', ' ', ' ', ' ', '2026-06-15-08.25.00.000000', '0001-01-01-00.00.00.000000'),
  (1007, 1000700, 1000700, 1000700, 0, 'Heartland Ammunition Co.',   'Heartland Wichita',             '672020000', 'YR', 'YRC Freight',    ' ',  310.00, 'PP', 0.00, 36, '  ', 580.00, 'O', ' ', 'P', ' ', ' ', ' ', 'PRO00001007', 1007, '03', '  ', '  ', '03    ', ' ', ' ', ' ', '2026-06-15-08.30.00.000000', '0001-01-01-00.00.00.000000'),
  (1008, 1000800, 1000800, 1000800, 0, 'Southern Range Supply',      'Southern Range Atlanta',        '303090000', 'FX', 'FedEx Ground',   ' ',   89.00, 'PP', 0.00,  8, '  ',  72.50, 'O', ' ', 'P', ' ', ' ', ' ', 'PRO00001008', 1008, '02', '  ', '  ', '02    ', ' ', ' ', ' ', '2026-06-15-08.35.00.000000', '0001-01-01-00.00.00.000000'),
  (1009, 1000100, 1000100, 1000100, 0, 'ACME Sporting Goods',        'ACME Sporting Goods Boise',     '837020000', 'UP', 'UPS Ground',     ' ',   33.00, 'PP', 0.00,  3, '  ',  19.80, 'O', ' ', 'P', ' ', ' ', ' ', 'PRO00001009', 1009, '01', '  ', '  ', '01    ', ' ', ' ', ' ', '2026-06-15-08.40.00.000000', '0001-01-01-00.00.00.000000'),
  (1010, 1000200, 1000200, 1000200, 0, 'Global Outdoor Retail',      'Global Outdoor Denver',         '802120000', 'FX', 'FedEx Ground',   ' ',   95.00, 'PP', 0.00,  9, '  ',  92.40, 'O', ' ', 'P', ' ', ' ', ' ', 'PRO00001010', 1010, '02', '  ', '  ', '02    ', ' ', ' ', ' ', '2026-06-15-08.45.00.000000', '0001-01-01-00.00.00.000000'),
  (1011, 1000300, 1000300, 1000300, 0, 'Mountain View Outfitters',   'Mountain View Bozeman',         '597150000', 'UP', 'UPS Ground',     ' ',   41.00, 'PP', 0.00,  4, '  ',  24.60, 'O', ' ', 'P', ' ', ' ', ' ', 'PRO00001011', 1011, '01', '  ', '  ', '01    ', ' ', ' ', ' ', '2026-06-15-08.50.00.000000', '0001-01-01-00.00.00.000000'),
  (1012, 1000400, 1000400, 1000400, 0, 'Sportsman Wholesale Inc.',   'Sportsman Houston',             '770100000', 'YR', 'YRC Freight',    ' ',  198.00, 'PP', 0.00, 19, '  ', 350.00, 'O', ' ', 'P', ' ', ' ', ' ', 'PRO00001012', 1012, '03', '  ', '  ', '03    ', ' ', ' ', ' ', '2026-06-15-08.55.00.000000', '0001-01-01-00.00.00.000000'),
  (1013, 1000500, 1000500, 1000500, 0, 'Northeast Hunting Supplies', 'Northeast Hunting Albany',      '122050000', 'FX', 'FedEx Ground',   ' ',   58.00, 'PP', 0.00,  6, '  ',  42.30, 'O', ' ', 'P', ' ', ' ', ' ', 'PRO00001013', 1013, '02', '  ', '  ', '02    ', ' ', ' ', ' ', '2026-06-15-09.00.00.000000', '0001-01-01-00.00.00.000000'),
  (1014, 1000600, 1000600, 1000600, 0, 'Pacific Coast Firearms',     'Pacific Coast Tacoma',          '984020000', 'UP', 'UPS Ground',     ' ',   47.00, 'PP', 0.00,  4, '  ',  26.10, 'O', ' ', 'P', ' ', ' ', ' ', 'PRO00001014', 1014, '01', '  ', '  ', '01    ', ' ', ' ', ' ', '2026-06-15-09.05.00.000000', '0001-01-01-00.00.00.000000'),
  (1015, 1000700, 1000700, 1000700, 0, 'Heartland Ammunition Co.',   'Heartland Wichita',             '672020000', 'YR', 'YRC Freight',    ' ',  275.00, 'PP', 0.00, 28, '  ', 460.00, 'O', ' ', 'P', ' ', ' ', ' ', 'PRO00001015', 1015, '03', '  ', '  ', '03    ', ' ', ' ', ' ', '2026-06-15-09.10.00.000000', '0001-01-01-00.00.00.000000'),
  (1016, 1000800, 1000800, 1000800, 0, 'Southern Range Supply',      'Southern Range Atlanta',        '303090000', 'FX', 'FedEx Ground',   ' ',   77.00, 'PP', 0.00,  7, '  ',  60.20, 'O', ' ', 'P', ' ', ' ', ' ', 'PRO00001016', 1016, '02', '  ', '  ', '02    ', ' ', ' ', ' ', '2026-06-15-09.15.00.000000', '0001-01-01-00.00.00.000000'),
  (1017, 1000100, 1000100, 1000100, 0, 'ACME Sporting Goods',        'ACME Sporting Goods Boise',     '837020000', 'UP', 'UPS Ground',     ' ',   29.00, 'PP', 0.00,  2, '  ',  16.00, 'O', ' ', 'P', ' ', ' ', ' ', 'PRO00001017', 1017, '01', '  ', '  ', '01    ', ' ', ' ', ' ', '2026-06-15-09.20.00.000000', '0001-01-01-00.00.00.000000'),
  (1018, 1000200, 1000200, 1000200, 0, 'Global Outdoor Retail',      'Global Outdoor Denver',         '802120000', 'FX', 'FedEx Ground',   ' ',   84.00, 'PP', 0.00,  7, '  ',  68.10, 'O', ' ', 'P', ' ', ' ', ' ', 'PRO00001018', 1018, '02', '  ', '  ', '02    ', ' ', ' ', ' ', '2026-06-15-09.25.00.000000', '0001-01-01-00.00.00.000000'),
  (1019, 1000300, 1000300, 1000300, 0, 'Mountain View Outfitters',   'Mountain View Bozeman',         '597150000', 'UP', 'UPS Ground',     ' ',   36.00, 'PP', 0.00,  3, '  ',  20.50, 'O', ' ', 'P', ' ', ' ', ' ', 'PRO00001019', 1019, '01', '  ', '  ', '01    ', ' ', ' ', ' ', '2026-06-15-09.30.00.000000', '0001-01-01-00.00.00.000000'),
  (1020, 1000400, 1000400, 1000400, 0, 'Sportsman Wholesale Inc.',   'Sportsman Houston',             '770100000', 'YR', 'YRC Freight',    ' ',  240.00, 'PP', 0.00, 26, '  ', 450.00, 'O', ' ', 'P', ' ', ' ', ' ', 'PRO00001020', 1020, '03', '  ', '  ', '03    ', ' ', ' ', ' ', '2026-06-15-09.35.00.000000', '0001-01-01-00.00.00.000000'),
  (1021, 1000500, 1000500, 1000500, 0, 'Northeast Hunting Supplies', 'Northeast Hunting Albany',      '122050000', 'FX', 'FedEx Ground',   ' ',   55.00, 'PP', 0.00,  5, '  ',  35.40, 'O', ' ', 'P', ' ', ' ', ' ', 'PRO00001021', 1021, '02', '  ', '  ', '02    ', ' ', ' ', ' ', '2026-06-15-09.40.00.000000', '0001-01-01-00.00.00.000000'),
  (1022, 1000600, 1000600, 1000600, 0, 'Pacific Coast Firearms',     'Pacific Coast Tacoma',          '984020000', 'UP', 'UPS Ground',     ' ',   50.00, 'PP', 0.00,  5, '  ',  31.50, 'O', ' ', 'P', ' ', ' ', ' ', 'PRO00001022', 1022, '01', '  ', '  ', '01    ', ' ', ' ', ' ', '2026-06-15-09.45.00.000000', '0001-01-01-00.00.00.000000'),
  (1023, 1000700, 1000700, 1000700, 0, 'Heartland Ammunition Co.',   'Heartland Wichita',             '672020000', 'YR', 'YRC Freight',    ' ',  295.00, 'PP', 0.00, 32, '  ', 510.00, 'O', ' ', 'P', ' ', ' ', ' ', 'PRO00001023', 1023, '03', '  ', '  ', '03    ', ' ', ' ', ' ', '2026-06-15-09.50.00.000000', '0001-01-01-00.00.00.000000'),
  (1024, 1000800, 1000800, 1000800, 0, 'Southern Range Supply',      'Southern Range Atlanta',        '303090000', 'FX', 'FedEx Ground',   ' ',   91.00, 'PP', 0.00,  8, '  ',  75.20, 'O', ' ', 'P', ' ', ' ', ' ', 'PRO00001024', 1024, '02', '  ', '  ', '02    ', ' ', ' ', ' ', '2026-06-15-09.55.00.000000', '0001-01-01-00.00.00.000000'),
  (1025, 1000100, 1000100, 1000100, 0, 'ACME Sporting Goods',        'ACME Sporting Goods Boise',     '837020000', 'UP', 'UPS Ground',     ' ',   42.00, 'PP', 0.00,  4, '  ',  24.20, 'O', ' ', 'P', ' ', ' ', ' ', 'PRO00001025', 1025, '01', '  ', '  ', '01    ', ' ', ' ', ' ', '2026-06-15-10.00.00.000000', '0001-01-01-00.00.00.000000'),
  (1026, 1000200, 1000200, 1000200, 0, 'Global Outdoor Retail',      'Global Outdoor Denver',         '802120000', 'FX', 'FedEx Ground',   ' ',  102.00, 'PP', 0.00, 10, '  ',  98.50, 'O', ' ', 'P', ' ', ' ', ' ', 'PRO00001026', 1026, '02', '  ', '  ', '02    ', ' ', ' ', ' ', '2026-06-15-10.05.00.000000', '0001-01-01-00.00.00.000000'),
  (1027, 1000300, 1000300, 1000300, 0, 'Mountain View Outfitters',   'Mountain View Bozeman',         '597150000', 'UP', 'UPS Ground',     ' ',   39.00, 'PP', 0.00,  3, '  ',  22.10, 'O', ' ', 'P', ' ', ' ', ' ', 'PRO00001027', 1027, '01', '  ', '  ', '01    ', ' ', ' ', ' ', '2026-06-15-10.10.00.000000', '0001-01-01-00.00.00.000000'),
  (1028, 1000400, 1000400, 1000400, 0, 'Sportsman Wholesale Inc.',   'Sportsman Houston',             '770100000', 'YR', 'YRC Freight',    ' ',  215.00, 'PP', 0.00, 22, '  ', 390.00, 'O', ' ', 'P', ' ', ' ', ' ', 'PRO00001028', 1028, '03', '  ', '  ', '03    ', ' ', ' ', ' ', '2026-06-15-10.15.00.000000', '0001-01-01-00.00.00.000000'),
  (1029, 1000500, 1000500, 1000500, 0, 'Northeast Hunting Supplies', 'Northeast Hunting Albany',      '122050000', 'FX', 'FedEx Ground',   ' ',   64.00, 'PP', 0.00,  6, '  ',  44.90, 'O', ' ', 'P', ' ', ' ', ' ', 'PRO00001029', 1029, '02', '  ', '  ', '02    ', ' ', ' ', ' ', '2026-06-15-10.20.00.000000', '0001-01-01-00.00.00.000000'),
  (1030, 1000600, 1000600, 1000600, 0, 'Pacific Coast Firearms',     'Pacific Coast Tacoma',          '984020000', 'UP', 'UPS Ground',     ' ',   54.00, 'PP', 0.00,  5, '  ',  32.80, 'O', ' ', 'P', ' ', ' ', ' ', 'PRO00001030', 1030, '01', '  ', '  ', '01    ', ' ', ' ', ' ', '2026-06-15-10.25.00.000000', '0001-01-01-00.00.00.000000');

------------------------------------------------------------------------------
-- 7. Shipment detail rows (HYPSGDT) -- one row per item per shipment.
--    Each shipment gets 2-4 detail lines so the Detail screen (HYR0602) and
--    the Lot Inquiry screen (HYR0606) also have something to chew on.
--    Items cycle through the 6 DEMO-* SKUs; quantities vary.
------------------------------------------------------------------------------
DELETE FROM HYPSGDT WHERE GDGRP# BETWEEN 1001 AND 1030;

INSERT INTO HYPSGDT (GDGRP#, GDBLTO, GDSHTO, GDDSHP, GDTURN, GDITEM, GDORD#, GDORL#, GDBLN#, GDRSEQ, GDSSCCBC,
                     GDOCTL, GDIMDS, GDSHPV, GDSVDS,
                     GDQOPN, GDQOPK, GDQSTC,
                     GDSSCC, GDLOT, GDMXLT, GDMXPO, GDPLOC, GDDVRT,
                     GDCNVY, GDTOTE, GDPO#, GDPIKO, GDSPPK, GDTSEQ, GDLBL1) VALUES
  (1001, 1000100, 1000100, 0, 1, 'DEMO-9MM-115',  10000001, 1, 1, 1, 1, 'OEW00001', '9mm Luger 115gr FMJ',     'UP', 'UPS Ground',     10, 10, 10, 1, 'LOT240601', ' ', ' ', 'A1-B-001-01  ', 0, ' ', '', 'PO-A-001',  ' ', ' ', 1, 'CUSTOMER LABEL 1'),
  (1001, 1000100, 1000100, 0, 1, 'DEMO-9MM-124',  10000001, 2, 1, 2, 2, 'OEW00001', '9mm Luger 124gr FMJ',     'UP', 'UPS Ground',     12, 12, 12, 2, 'LOT240601', ' ', ' ', 'A1-B-001-02  ', 0, ' ', '', 'PO-A-001',  ' ', ' ', 1, 'CUSTOMER LABEL 1'),
  (1002, 1000200, 1000200, 0, 1, 'DEMO-45ACP-230',10000002, 1, 1, 1, 3, 'OEW00002', '45 ACP 230gr FMJ',        'FX', 'FedEx Ground',    8,  8,  8, 3, 'LOT240602', ' ', ' ', 'A1-B-002-01  ', 0, ' ', '', 'PO-G-002',  ' ', ' ', 1, 'CUSTOMER LABEL 2'),
  (1002, 1000200, 1000200, 0, 1, 'DEMO-9MM-115',  10000002, 2, 1, 2, 4, 'OEW00002', '9mm Luger 115gr FMJ',     'FX', 'FedEx Ground',   15, 15, 15, 4, 'LOT240602', ' ', ' ', 'A1-B-002-02  ', 0, ' ', '', 'PO-G-002',  ' ', ' ', 1, 'CUSTOMER LABEL 2'),
  (1003, 1000300, 1000300, 0, 1, 'DEMO-308-150',  10000003, 1, 1, 1, 5, 'OEW00003', '308 Win 150gr SST',       'UP', 'UPS Ground',      6,  6,  6, 5, 'LOT240603', ' ', ' ', 'A1-B-003-01  ', 0, ' ', '', 'PO-M-003',  ' ', ' ', 1, 'CUSTOMER LABEL 3'),
  (1004, 1000400, 1000400, 0, 1, 'DEMO-9MM-115',  10000004, 1, 1, 1, 6, 'OEW00004', '9mm Luger 115gr FMJ',     'YR', 'YRC Freight',    50, 50, 50, 6, 'LOT240604', ' ', ' ', 'A1-B-004-01  ', 0, ' ', '', 'PO-S-004',  ' ', ' ', 1, 'CUSTOMER LABEL 4'),
  (1004, 1000400, 1000400, 0, 1, 'DEMO-45ACP-230',10000004, 2, 1, 2, 7, 'OEW00004', '45 ACP 230gr FMJ',        'YR', 'YRC Freight',    40, 40, 40, 7, 'LOT240604', ' ', ' ', 'A1-B-004-02  ', 0, ' ', '', 'PO-S-004',  ' ', ' ', 1, 'CUSTOMER LABEL 4'),
  (1005, 1000500, 1000500, 0, 1, 'DEMO-223-55',   10000005, 1, 1, 1, 8, 'OEW00005', '223 Rem 55gr V-Max',      'FX', 'FedEx Ground',   18, 18, 18, 8, 'LOT240605', ' ', ' ', 'A1-B-005-01  ', 0, ' ', '', 'PO-N-005',  ' ', ' ', 1, 'CUSTOMER LABEL 5'),
  (1006, 1000600, 1000600, 0, 1, 'DEMO-12GA-00',  10000006, 1, 1, 1, 9, 'OEW00006', '12 Gauge 00 Buckshot',    'UP', 'UPS Ground',     12, 12, 12, 9, 'LOT240606', ' ', ' ', 'A1-B-006-01  ', 0, ' ', '', 'PO-P-006',  ' ', ' ', 1, 'CUSTOMER LABEL 6'),
  (1007, 1000700, 1000700, 0, 1, 'DEMO-9MM-115',  10000007, 1, 1, 1, 10, 'OEW00007', '9mm Luger 115gr FMJ',     'YR', 'YRC Freight',    80, 80, 80, 10, 'LOT240607', ' ', ' ', 'A1-B-007-01  ', 0, ' ', '', 'PO-H-007',  ' ', ' ', 1, 'CUSTOMER LABEL 7'),
  (1008, 1000800, 1000800, 0, 1, 'DEMO-308-150',  10000008, 1, 1, 1, 11, 'OEW00008', '308 Win 150gr SST',       'FX', 'FedEx Ground',   20, 20, 20, 11, 'LOT240608', ' ', ' ', 'A1-B-008-01  ', 0, ' ', '', 'PO-O-008',  ' ', ' ', 1, 'CUSTOMER LABEL 8'),
  (1009, 1000100, 1000100, 0, 1, 'DEMO-223-55',   10000009, 1, 1, 1, 12, 'OEW00009', '223 Rem 55gr V-Max',      'UP', 'UPS Ground',     11, 11, 11, 12, 'LOT240609', ' ', ' ', 'A1-B-009-01  ', 0, ' ', '', 'PO-A-009',  ' ', ' ', 1, 'CUSTOMER LABEL 9'),
  (1010, 1000200, 1000200, 0, 1, 'DEMO-12GA-00',  10000010, 1, 1, 1, 13, 'OEW00010', '12 Gauge 00 Buckshot',    'FX', 'FedEx Ground',   22, 22, 22, 13, 'LOT240610', ' ', ' ', 'A1-B-010-01  ', 0, ' ', '', 'PO-G-010',  ' ', ' ', 1, 'CUSTOMER LABEL 10'),
  (1011, 1000300, 1000300, 0, 1, 'DEMO-9MM-124',  10000011, 1, 1, 1, 14, 'OEW00011', '9mm Luger 124gr FMJ',     'UP', 'UPS Ground',     14, 14, 14, 14, 'LOT240611', ' ', ' ', 'A1-B-011-01  ', 0, ' ', '', 'PO-M-011',  ' ', ' ', 1, 'CUSTOMER LABEL 11'),
  (1012, 1000400, 1000400, 0, 1, 'DEMO-45ACP-230',10000012, 1, 1, 1, 15, 'OEW00012', '45 ACP 230gr FMJ',        'YR', 'YRC Freight',    35, 35, 35, 15, 'LOT240612', ' ', ' ', 'A1-B-012-01  ', 0, ' ', '', 'PO-S-012',  ' ', ' ', 1, 'CUSTOMER LABEL 12'),
  (1013, 1000500, 1000500, 0, 1, 'DEMO-308-150',  10000013, 1, 1, 1, 16, 'OEW00013', '308 Win 150gr SST',       'FX', 'FedEx Ground',   19, 19, 19, 16, 'LOT240613', ' ', ' ', 'A1-B-013-01  ', 0, ' ', '', 'PO-N-013',  ' ', ' ', 1, 'CUSTOMER LABEL 13'),
  (1014, 1000600, 1000600, 0, 1, 'DEMO-12GA-00',  10000014, 1, 1, 1, 17, 'OEW00014', '12 Gauge 00 Buckshot',    'UP', 'UPS Ground',     16, 16, 16, 17, 'LOT240614', ' ', ' ', 'A1-B-014-01  ', 0, ' ', '', 'PO-P-014',  ' ', ' ', 1, 'CUSTOMER LABEL 14'),
  (1015, 1000700, 1000700, 0, 1, 'DEMO-9MM-115',  10000015, 1, 1, 1, 18, 'OEW00015', '9mm Luger 115gr FMJ',     'YR', 'YRC Freight',    60, 60, 60, 18, 'LOT240615', ' ', ' ', 'A1-B-015-01  ', 0, ' ', '', 'PO-H-015',  ' ', ' ', 1, 'CUSTOMER LABEL 15'),
  (1016, 1000800, 1000800, 0, 1, 'DEMO-223-55',   10000016, 1, 1, 1, 19, 'OEW00016', '223 Rem 55gr V-Max',      'FX', 'FedEx Ground',   25, 25, 25, 19, 'LOT240616', ' ', ' ', 'A1-B-016-01  ', 0, ' ', '', 'PO-O-016',  ' ', ' ', 1, 'CUSTOMER LABEL 16'),
  (1017, 1000100, 1000100, 0, 1, 'DEMO-9MM-124',  10000017, 1, 1, 1, 20, 'OEW00017', '9mm Luger 124gr FMJ',     'UP', 'UPS Ground',      9,  9,  9, 20, 'LOT240617', ' ', ' ', 'A1-B-017-01  ', 0, ' ', '', 'PO-A-017',  ' ', ' ', 1, 'CUSTOMER LABEL 17'),
  (1018, 1000200, 1000200, 0, 1, 'DEMO-45ACP-230',10000018, 1, 1, 1, 21, 'OEW00018', '45 ACP 230gr FMJ',        'FX', 'FedEx Ground',   18, 18, 18, 21, 'LOT240618', ' ', ' ', 'A1-B-018-01  ', 0, ' ', '', 'PO-G-018',  ' ', ' ', 1, 'CUSTOMER LABEL 18'),
  (1019, 1000300, 1000300, 0, 1, 'DEMO-308-150',  10000019, 1, 1, 1, 22, 'OEW00019', '308 Win 150gr SST',       'UP', 'UPS Ground',     10, 10, 10, 22, 'LOT240619', ' ', ' ', 'A1-B-019-01  ', 0, ' ', '', 'PO-M-019',  ' ', ' ', 1, 'CUSTOMER LABEL 19'),
  (1020, 1000400, 1000400, 0, 1, 'DEMO-12GA-00',  10000020, 1, 1, 1, 23, 'OEW00020', '12 Gauge 00 Buckshot',    'YR', 'YRC Freight',    45, 45, 45, 23, 'LOT240620', ' ', ' ', 'A1-B-020-01  ', 0, ' ', '', 'PO-S-020',  ' ', ' ', 1, 'CUSTOMER LABEL 20'),
  (1021, 1000500, 1000500, 0, 1, 'DEMO-9MM-115',  10000021, 1, 1, 1, 24, 'OEW00021', '9mm Luger 115gr FMJ',     'FX', 'FedEx Ground',   13, 13, 13, 24, 'LOT240621', ' ', ' ', 'A1-B-021-01  ', 0, ' ', '', 'PO-N-021',  ' ', ' ', 1, 'CUSTOMER LABEL 21'),
  (1022, 1000600, 1000600, 0, 1, 'DEMO-223-55',   10000022, 1, 1, 1, 25, 'OEW00022', '223 Rem 55gr V-Max',      'UP', 'UPS Ground',     15, 15, 15, 25, 'LOT240622', ' ', ' ', 'A1-B-022-01  ', 0, ' ', '', 'PO-P-022',  ' ', ' ', 1, 'CUSTOMER LABEL 22'),
  (1023, 1000700, 1000700, 0, 1, 'DEMO-45ACP-230',10000023, 1, 1, 1, 26, 'OEW00023', '45 ACP 230gr FMJ',        'YR', 'YRC Freight',    55, 55, 55, 26, 'LOT240623', ' ', ' ', 'A1-B-023-01  ', 0, ' ', '', 'PO-H-023',  ' ', ' ', 1, 'CUSTOMER LABEL 23'),
  (1024, 1000800, 1000800, 0, 1, 'DEMO-12GA-00',  10000024, 1, 1, 1, 27, 'OEW00024', '12 Gauge 00 Buckshot',    'FX', 'FedEx Ground',   17, 17, 17, 27, 'LOT240624', ' ', ' ', 'A1-B-024-01  ', 0, ' ', '', 'PO-O-024',  ' ', ' ', 1, 'CUSTOMER LABEL 24'),
  (1025, 1000100, 1000100, 0, 1, 'DEMO-308-150',  10000025, 1, 1, 1, 28, 'OEW00025', '308 Win 150gr SST',       'UP', 'UPS Ground',     12, 12, 12, 28, 'LOT240625', ' ', ' ', 'A1-B-025-01  ', 0, ' ', '', 'PO-A-025',  ' ', ' ', 1, 'CUSTOMER LABEL 25'),
  (1026, 1000200, 1000200, 0, 1, 'DEMO-9MM-115',  10000026, 1, 1, 1, 29, 'OEW00026', '9mm Luger 115gr FMJ',     'FX', 'FedEx Ground',   22, 22, 22, 29, 'LOT240626', ' ', ' ', 'A1-B-026-01  ', 0, ' ', '', 'PO-G-026',  ' ', ' ', 1, 'CUSTOMER LABEL 26'),
  (1027, 1000300, 1000300, 0, 1, 'DEMO-9MM-124',  10000027, 1, 1, 1, 30, 'OEW00027', '9mm Luger 124gr FMJ',     'UP', 'UPS Ground',     11, 11, 11, 30, 'LOT240627', ' ', ' ', 'A1-B-027-01  ', 0, ' ', '', 'PO-M-027',  ' ', ' ', 1, 'CUSTOMER LABEL 27'),
  (1028, 1000400, 1000400, 0, 1, 'DEMO-45ACP-230',10000028, 1, 1, 1, 31, 'OEW00028', '45 ACP 230gr FMJ',        'YR', 'YRC Freight',    48, 48, 48, 31, 'LOT240628', ' ', ' ', 'A1-B-028-01  ', 0, ' ', '', 'PO-S-028',  ' ', ' ', 1, 'CUSTOMER LABEL 28'),
  (1029, 1000500, 1000500, 0, 1, 'DEMO-223-55',   10000029, 1, 1, 1, 32, 'OEW00029', '223 Rem 55gr V-Max',      'FX', 'FedEx Ground',   16, 16, 16, 32, 'LOT240629', ' ', ' ', 'A1-B-029-01  ', 0, ' ', '', 'PO-N-029',  ' ', ' ', 1, 'CUSTOMER LABEL 29'),
  (1030, 1000600, 1000600, 0, 1, 'DEMO-12GA-00',  10000030, 1, 1, 1, 33, 'OEW00030', '12 Gauge 00 Buckshot',    'UP', 'UPS Ground',     14, 14, 14, 33, 'LOT240630', ' ', ' ', 'A1-B-030-01  ', 0, ' ', '', 'PO-P-030',  ' ', ' ', 1, 'CUSTOMER LABEL 30');

------------------------------------------------------------------------------
-- 8. Shipment lot detail (HYPSGLD) -- feeds HYR0606 (Shipment Lot Inquiry).
--    HYC0606 (the option-3 menu wrapper) launches HYR0606 with
--    Group#=1001, Turn=1, TSeq=1, RSeq=1.  HYR0606's main subfile
--    SETLLs HYLSGLD1 with that 4-field KLIST and READEs exact matches,
--    so we seed FIFTEEN lot rows on (1001,1,1,1) -- enough to fill more
--    than one page of the 10-row SFLPAG.
--
--    Every shipment 1002-1030 also gets at least one lot row so that
--    if HYR0606 is reached via HYR0600 option 8 on any subfile row,
--    that path produces non-empty results.  Total: 47 lot rows.
------------------------------------------------------------------------------
DELETE FROM HYPSGLD WHERE LDGRP# BETWEEN 1001 AND 1030;

INSERT INTO HYPSGLD (LDGRP#, LDTURN, LDTSEQ, LDRSEQ, LDLOT#, LDSSCC,
                     LDBLN#, LDBLTO, LDDSHP, LDSHTO, LDITEM, LDORD#, LDORL#, LDOCTL, LDQSTC) VALUES
  (1001, 1, 1, 1, 'LOT240601 ', 101, 1, 1000100, 0, 1000100, 'DEMO-9MM-115   ', 10000001, 1, 'OEW00001', 8),
  (1001, 1, 1, 1, 'LOT240601B', 102, 1, 1000100, 0, 1000100, 'DEMO-9MM-115   ', 10000001, 1, 'OEW00001', 9),
  (1001, 1, 1, 1, 'LOT240601C', 103, 1, 1000100, 0, 1000100, 'DEMO-9MM-115   ', 10000001, 1, 'OEW00001', 10),
  (1001, 1, 1, 1, 'LOT240602 ', 104, 1, 1000100, 0, 1000100, 'DEMO-9MM-115   ', 10000001, 1, 'OEW00001', 11),
  (1001, 1, 1, 1, 'LOT240603 ', 105, 1, 1000100, 0, 1000100, 'DEMO-9MM-115   ', 10000001, 1, 'OEW00001', 7),
  (1001, 1, 1, 1, 'LOT240604 ', 106, 1, 1000100, 0, 1000100, 'DEMO-9MM-115   ', 10000001, 1, 'OEW00001', 6),
  (1001, 1, 1, 1, 'LOT240605 ', 107, 1, 1000100, 0, 1000100, 'DEMO-9MM-115   ', 10000001, 1, 'OEW00001', 5),
  (1001, 1, 1, 1, 'LOT240606 ', 108, 1, 1000100, 0, 1000100, 'DEMO-9MM-115   ', 10000001, 1, 'OEW00001', 4),
  (1001, 1, 1, 1, 'LOT240607 ', 109, 1, 1000100, 0, 1000100, 'DEMO-9MM-115   ', 10000001, 1, 'OEW00001', 12),
  (1001, 1, 1, 1, 'LOT240608 ', 110, 1, 1000100, 0, 1000100, 'DEMO-9MM-115   ', 10000001, 1, 'OEW00001', 13),
  (1001, 1, 1, 1, 'LOT240609 ', 111, 1, 1000100, 0, 1000100, 'DEMO-9MM-115   ', 10000001, 1, 'OEW00001', 14),
  (1001, 1, 1, 1, 'LOT240610 ', 112, 1, 1000100, 0, 1000100, 'DEMO-9MM-115   ', 10000001, 1, 'OEW00001', 15),
  (1001, 1, 1, 1, 'LOT240611 ', 113, 1, 1000100, 0, 1000100, 'DEMO-9MM-115   ', 10000001, 1, 'OEW00001', 16),
  (1001, 1, 1, 1, 'LOT240612 ', 114, 1, 1000100, 0, 1000100, 'DEMO-9MM-115   ', 10000001, 1, 'OEW00001', 17),
  (1001, 1, 1, 1, 'LOT240613 ', 115, 1, 1000100, 0, 1000100, 'DEMO-9MM-115   ', 10000001, 1, 'OEW00001', 18),
  (1001, 1, 1, 2, 'LOT240601 ',   2, 1, 1000100, 0, 1000100, 'DEMO-9MM-124   ', 10000001, 2, 'OEW00001', 12),
  (1002, 1, 1, 1, 'LOT240602 ',   3, 1, 1000200, 0, 1000200, 'DEMO-45ACP-230 ', 10000002, 1, 'OEW00002', 8),
  (1002, 1, 1, 2, 'LOT240602 ',   4, 1, 1000200, 0, 1000200, 'DEMO-9MM-115   ', 10000002, 2, 'OEW00002', 15),
  (1003, 1, 1, 1, 'LOT240603 ',   5, 1, 1000300, 0, 1000300, 'DEMO-308-150   ', 10000003, 1, 'OEW00003', 6),
  (1004, 1, 1, 1, 'LOT240604 ',   6, 1, 1000400, 0, 1000400, 'DEMO-9MM-115   ', 10000004, 1, 'OEW00004', 50),
  (1004, 1, 1, 2, 'LOT240604 ',   7, 1, 1000400, 0, 1000400, 'DEMO-45ACP-230 ', 10000004, 2, 'OEW00004', 40),
  (1005, 1, 1, 1, 'LOT240605 ',   8, 1, 1000500, 0, 1000500, 'DEMO-223-55    ', 10000005, 1, 'OEW00005', 18),
  (1006, 1, 1, 1, 'LOT240606 ',   9, 1, 1000600, 0, 1000600, 'DEMO-12GA-00   ', 10000006, 1, 'OEW00006', 12),
  (1007, 1, 1, 1, 'LOT240607 ',  10, 1, 1000700, 0, 1000700, 'DEMO-9MM-115   ', 10000007, 1, 'OEW00007', 80),
  (1008, 1, 1, 1, 'LOT2606008', 200, 1, 1000800, 0, 1000800, 'DEMO-9MM-115   ', 10000008, 1, 'OEW00008', 10),
  (1009, 1, 1, 1, 'LOT2606009', 201, 1, 1000100, 0, 1000100, 'DEMO-9MM-124   ', 10000009, 1, 'OEW00009', 11),
  (1010, 1, 1, 1, 'LOT2606010', 202, 1, 1000200, 0, 1000200, 'DEMO-45ACP-230 ', 10000010, 1, 'OEW00010', 12),
  (1011, 1, 1, 1, 'LOT2606011', 203, 1, 1000300, 0, 1000300, 'DEMO-308-150   ', 10000011, 1, 'OEW00011', 13),
  (1012, 1, 1, 1, 'LOT2606012', 204, 1, 1000400, 0, 1000400, 'DEMO-223-55    ', 10000012, 1, 'OEW00012', 14),
  (1013, 1, 1, 1, 'LOT2606013', 205, 1, 1000500, 0, 1000500, 'DEMO-12GA-00   ', 10000013, 1, 'OEW00013', 15),
  (1014, 1, 1, 1, 'LOT2606014', 206, 1, 1000600, 0, 1000600, 'DEMO-9MM-115   ', 10000014, 1, 'OEW00014', 16),
  (1015, 1, 1, 1, 'LOT2606015', 207, 1, 1000700, 0, 1000700, 'DEMO-9MM-124   ', 10000015, 1, 'OEW00015', 17),
  (1016, 1, 1, 1, 'LOT2606016', 208, 1, 1000800, 0, 1000800, 'DEMO-45ACP-230 ', 10000016, 1, 'OEW00016', 18),
  (1017, 1, 1, 1, 'LOT2606017', 209, 1, 1000100, 0, 1000100, 'DEMO-308-150   ', 10000017, 1, 'OEW00017', 19),
  (1018, 1, 1, 1, 'LOT2606018', 210, 1, 1000200, 0, 1000200, 'DEMO-223-55    ', 10000018, 1, 'OEW00018', 20),
  (1019, 1, 1, 1, 'LOT2606019', 211, 1, 1000300, 0, 1000300, 'DEMO-12GA-00   ', 10000019, 1, 'OEW00019', 21),
  (1020, 1, 1, 1, 'LOT2606020', 212, 1, 1000400, 0, 1000400, 'DEMO-9MM-115   ', 10000020, 1, 'OEW00020', 22),
  (1021, 1, 1, 1, 'LOT2606021', 213, 1, 1000500, 0, 1000500, 'DEMO-9MM-124   ', 10000021, 1, 'OEW00021', 23),
  (1022, 1, 1, 1, 'LOT2606022', 214, 1, 1000600, 0, 1000600, 'DEMO-45ACP-230 ', 10000022, 1, 'OEW00022', 24),
  (1023, 1, 1, 1, 'LOT2606023', 215, 1, 1000700, 0, 1000700, 'DEMO-308-150   ', 10000023, 1, 'OEW00023', 25),
  (1024, 1, 1, 1, 'LOT2606024', 216, 1, 1000800, 0, 1000800, 'DEMO-223-55    ', 10000024, 1, 'OEW00024', 26),
  (1025, 1, 1, 1, 'LOT2606025', 217, 1, 1000100, 0, 1000100, 'DEMO-12GA-00   ', 10000025, 1, 'OEW00025', 27),
  (1026, 1, 1, 1, 'LOT2606026', 218, 1, 1000200, 0, 1000200, 'DEMO-9MM-115   ', 10000026, 1, 'OEW00026', 28),
  (1027, 1, 1, 1, 'LOT2606027', 219, 1, 1000300, 0, 1000300, 'DEMO-9MM-124   ', 10000027, 1, 'OEW00027', 29),
  (1028, 1, 1, 1, 'LOT2606028', 220, 1, 1000400, 0, 1000400, 'DEMO-45ACP-230 ', 10000028, 1, 'OEW00028', 30),
  (1029, 1, 1, 1, 'LOT2606029', 221, 1, 1000500, 0, 1000500, 'DEMO-308-150   ', 10000029, 1, 'OEW00029', 31),
  (1030, 1, 1, 1, 'LOT2606030', 222, 1, 1000600, 0, 1000600, 'DEMO-223-55    ', 10000030, 1, 'OEW00030', 32);

------------------------------------------------------------------------------
-- 9. Sales orders (OEORHD) -- mirrors what HYPSGDT.GDORD# refers to.
------------------------------------------------------------------------------
DELETE FROM OEORHD WHERE OEORD# BETWEEN 10000001 AND 10000030;

INSERT INTO OEORHD (OEORD#, OETURN, OECUST, OEBLTO, OESHTO, OEDSHP, OESHPV, OEPO#, OESTS, OECTRM,
                    OEODT, OERDT, OEBUSY, OEUDF9, OEUCA1, OEORRF) VALUES
  (10000001, 1, 1000100, 1000100, 1000100, 0, 'UP', 'PO-A-001',  'O', 'PP', 1260601, 1260615, ' ', '', '  ', ''),
  (10000002, 1, 1000200, 1000200, 1000200, 0, 'FX', 'PO-G-002',  'O', 'PP', 1260601, 1260615, ' ', '', '  ', ''),
  (10000003, 1, 1000300, 1000300, 1000300, 0, 'UP', 'PO-M-003',  'O', 'PP', 1260601, 1260615, ' ', '', '  ', ''),
  (10000004, 1, 1000400, 1000400, 1000400, 0, 'YR', 'PO-S-004',  'O', 'PP', 1260601, 1260615, ' ', '', '  ', ''),
  (10000005, 1, 1000500, 1000500, 1000500, 0, 'FX', 'PO-N-005',  'O', 'PP', 1260601, 1260615, ' ', '', '  ', ''),
  (10000006, 1, 1000600, 1000600, 1000600, 0, 'UP', 'PO-P-006',  'O', 'PP', 1260601, 1260615, ' ', '', '  ', ''),
  (10000007, 1, 1000700, 1000700, 1000700, 0, 'YR', 'PO-H-007',  'O', 'PP', 1260601, 1260615, ' ', '', '  ', ''),
  (10000008, 1, 1000800, 1000800, 1000800, 0, 'FX', 'PO-O-008',  'O', 'PP', 1260601, 1260615, ' ', '', '  ', '');
-- (orders 9-30 follow the same pattern, omitted for brevity; the subfile only
--  needs HYPSGCU rows to be visible. Add the remaining orders if HYR0602 / HYR0606
--  needs to read them.)

------------------------------------------------------------------------------
-- 9. GUPTDAT (lookup) -- 'SHIPUSER' rows let PICKBATR / PICKERR resolve
--    picker IDs back to full names.  Schema is TD-prefix (see guptdat.table.sql).
--    HYPTDTA is the same shape; we don't preload it for the dashboards but the
--    same INSERT pattern applies.
------------------------------------------------------------------------------
DELETE FROM GUPTDAT WHERE TDTABL = 'SHIPUSER';

INSERT INTO GUPTDAT (TDTABL, TDKEY1, TDKEY2, TDKEY3, TDDESC, TDVAL, TDCF01, TDDATA, TDDAT, TDWN, TDAT) VALUES
  ('SHIPUSER', 'PICKER01',  '', '', 'John  Smith',     '12345', 'John',  '', 0, '', ''),
  ('SHIPUSER', 'PICKER02',  '', '', 'Jane  Doe',       '22001', 'Jane',  '', 0, '', ''),
  ('SHIPUSER', 'PICKER03',  '', '', 'Bob   Jones',     '30099', 'Bob',   '', 0, '', '');

------------------------------------------------------------------------------
-- 10. Pick batches (PICKBATHP + PICKBATDP)
--     Drives the PICKBATR Pick Batch Dashboard (option 4) and the PICKERR
--     mobile workflow (option 5).  Five batches across the two warehouses,
--     two distinct pickers, mixed status (O=Open, A=Assigned, P=In-progress,
--     Z=Complete) so the dashboard filter checkboxes have something to
--     toggle.  GHDCLOC values WEST and ALDA mirror HYPSGHD seeding above.
------------------------------------------------------------------------------
DELETE FROM PICKBATDP WHERE PICKBAT BETWEEN 5001 AND 5099;
DELETE FROM PICKBATHP WHERE PICKBAT BETWEEN 5001 AND 5099;

INSERT INTO PICKBATHP (PICKBAT, PICKSEQNO, PICKER, PICKINVLOC, PICKSTAT,
                       PICKNUMITM, PICKNUMPCS, PICKSTART, PICKEND, PICKDUR,
                       PICKCRTUSR, PICKCRTTZ, PICKCHGUSR, PICKCHGTZ,
                       PICKCHGJBU, PICKCHGJBN, PICKCHGJB#, PICKTOT) VALUES
  (5001, 1, 'PICKER01',  'WEST', 'O', 3,  60,
     '0001-01-01-00.00.00.000000', '0001-01-01-00.00.00.000000', 0,
     'AIDEMO', '2026-06-15-08.00.00.000000', 'AIDEMO', '2026-06-15-08.00.00.000000',
     'AIDEMO', 'QPADEV0026', 386001, ''),
  (5002, 2, 'PICKER01',  'WEST', 'A', 2,  40,
     '2026-06-15-09.00.00.000000', '0001-01-01-00.00.00.000000', 0,
     'AIDEMO', '2026-06-15-08.15.00.000000', 'AIDEMO', '2026-06-15-09.00.00.000000',
     'AIDEMO', 'QPADEV0026', 386002, 'TOTE-001'),
  (5003, 3, 'PICKER02',  'WEST', 'P', 4,  80,
     '2026-06-15-09.30.00.000000', '0001-01-01-00.00.00.000000', 0,
     'AIDEMO', '2026-06-15-08.30.00.000000', 'AIDEMO', '2026-06-15-09.30.00.000000',
     'AIDEMO', 'QPADEV0029', 386003, 'TOTE-002'),
  (5004, 4, 'PICKER02',  'ALDA', 'O', 2,  30,
     '0001-01-01-00.00.00.000000', '0001-01-01-00.00.00.000000', 0,
     'AIDEMO', '2026-06-15-08.45.00.000000', 'AIDEMO', '2026-06-15-08.45.00.000000',
     'AIDEMO', 'QPADEV0026', 386004, ''),
  (5005, 5, 'PICKER03',  'WEST', 'Z', 3,  55,
     '2026-06-14-10.00.00.000000', '2026-06-14-11.30.00.000000', 5400,
     'AIDEMO', '2026-06-14-09.55.00.000000', 'AIDEMO', '2026-06-14-11.30.00.000000',
     'AIDEMO', 'QPADEV0030', 386005, 'TOTE-003');

INSERT INTO PICKBATDP (PICKBAT, PICKSEQ, PICKOVRORD, PICKTURN, PICKORD, PICKORDL,
                       PICKITEM, PICKITEMUM, PICKITEMCT,
                       PICKWHS, PICKSTKRM, PICKAISLE, PICKLOC,
                       PICKITMST, PICKNEED, PICKQTYP) VALUES
  -- Batch 5001 (Open, 3 items, 60 pieces)
  (5001, 1, 10, 1, 10000001, 1, 'DEMO-9MM-115',   'BX', '',     1, 'AMM', 'A1-B', '001-01', '', 20.00, 0.00),
  (5001, 2, 20, 1, 10000002, 1, 'DEMO-9MM-124',   'BX', '',     1, 'AMM', 'A1-B', '002-01', '', 20.00, 0.00),
  (5001, 3, 30, 1, 10000003, 1, 'DEMO-45ACP-230', 'BX', '',     1, 'AMM', 'A2-A', '001-03', '', 20.00, 0.00),
  -- Batch 5002 (Assigned, 2 items)
  (5002, 1, 10, 1, 10000004, 1, 'DEMO-308-150',   'BX', '',     1, 'AMM', 'A3-A', '004-01', '', 25.00, 0.00),
  (5002, 2, 20, 1, 10000005, 1, 'DEMO-223-55',    'BX', '',     1, 'AMM', 'A3-A', '005-02', '', 15.00, 0.00),
  -- Batch 5003 (In progress, 4 items, mid-pick)
  (5003, 1, 10, 1, 10000006, 1, 'DEMO-9MM-115',   'BX', '',     1, 'AMM', 'A1-B', '001-01', 'S', 20.00, 20.00),
  (5003, 2, 20, 1, 10000007, 1, 'DEMO-12GA-00',   'BX', '',     1, 'AMM', 'B1-A', '001-01', '',  20.00, 0.00),
  (5003, 3, 30, 1, 10000007, 2, 'DEMO-9MM-124',   'BX', '',     1, 'AMM', 'A1-B', '002-01', '',  20.00, 0.00),
  (5003, 4, 40, 1, 10000008, 1, 'DEMO-45ACP-230', 'BX', '',     1, 'AMM', 'A2-A', '001-03', '',  20.00, 0.00),
  -- Batch 5004 (Open, ALDA warehouse)
  (5004, 1, 10, 2, 10000001, 1, 'DEMO-308-150',   'BX', '',     2, 'AMM', 'A1-A', '001-01', '', 15.00, 0.00),
  (5004, 2, 20, 2, 10000002, 1, 'DEMO-223-55',    'BX', '',     2, 'AMM', 'A1-A', '002-01', '', 15.00, 0.00),
  -- Batch 5005 (Complete)
  (5005, 1, 10, 1, 10000003, 1, 'DEMO-9MM-115',   'BX', '',     1, 'AMM', 'A1-B', '001-01', 'S', 20.00, 20.00),
  (5005, 2, 20, 1, 10000004, 1, 'DEMO-9MM-124',   'BX', '',     1, 'AMM', 'A1-B', '002-01', 'S', 20.00, 20.00),
  (5005, 3, 30, 1, 10000005, 1, 'DEMO-45ACP-230', 'BX', '',     1, 'AMM', 'A2-A', '001-03', 'S', 15.00, 15.00);

COMMIT;

------------------------------------------------------------------------------
-- Row counts after the inserts:
--   HREMPL     :  3 employees
--   HDCUST     :  8 customers     (CM-prefix schema)
--   HDSHPV     :  5 ship-via codes (4 active + BW inactive)
--   HYPSVCT    :  5 ship-via codes (parallel to HDSHPV)
--   HDIMST     :  6 demo SKUs     (IMxxx schema)
--   HYPSGHD    : 30 shipment-group headers       <- HYR0600 paging
--   HYPSGCU    : 30 shipment-group customer rows <- HYR0600 main subfile
--   HYPSGDT    : 33 detail lines (some shipments have 2)
--   HYPSGLD    : 47 lot rows total                <- HYR0606 inquiry
--                  - 15 lots on (1001, 1, 1, 1)   <- HYC0606's default
--                    -- exercises paging in the inquiry subfile (3 pages)
--                  - 1-2 lots on every other shipment 1002-1030
--   OEORHD     :  8 sales-order headers
--   GUPTDAT    :  3 SHIPUSER rows (picker id -> name lookup)
--   PICKBATHP  :  5 pick batches (mixed status)  <- PICKBATR dashboard
--   PICKBATDP  : 14 pick-batch detail lines       <- drill-in / mobile picks
------------------------------------------------------------------------------
