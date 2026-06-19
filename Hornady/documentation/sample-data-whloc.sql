-- ============================================================================
-- WHLOC seed -- warehouse location master.
--
-- 12-row x 60-bay x 4-shelf grid per DC.  Items are clustered by
-- category so the route view shows the picker walking through coherent
-- zones:
--    Rows  1-3   handgun ammo
--    Rows  4-7   rifle ammo
--    Rows  8-9   shotgun ammo
--    Row  10     rimfire ammo
--    Rows 11-12  components (brass, primers)
--
-- Each DC (WEST/ALDA/GI) gets its own coordinate per SKU so a picker
-- working out of ALDA isn't pointed at WEST locations.  Bay numbers
-- vary slightly per DC so the maps look distinct.
--
-- Idempotent: DELETE-then-INSERT wipes the table on every run.
-- ============================================================================
DELETE FROM WHLOC;
INSERT INTO WHLOC (WHITEM, WHDC, WHROW, WHBAY, WHSHELF, WHQOH) VALUES
  -- handgun ammo (rows 1-3)
  ('DEMO-9MM-115',   'WEST',  1,  6, 2, 12000),
  ('DEMO-9MM-115',   'ALDA',  1, 10, 2,  8000),
  ('DEMO-9MM-115',   'GI',    1,  4, 2,  6000),
  ('DEMO-9MM-124',   'WEST',  1, 18, 2,  9000),
  ('DEMO-9MM-124',   'ALDA',  1, 22, 2,  7000),
  ('DEMO-9MM-124',   'GI',    1, 16, 2,  5500),
  ('DEMO-380-95',    'WEST',  1, 30, 1,  4000),
  ('DEMO-380-95',    'ALDA',  1, 34, 1,  3000),
  ('DEMO-380-95',    'GI',    1, 28, 1,  2500),
  ('DEMO-40SW-180',  'WEST',  1, 42, 3,  5500),
  ('DEMO-40SW-180',  'ALDA',  1, 46, 3,  4500),
  ('DEMO-40SW-180',  'GI',    1, 40, 3,  3500),
  ('DEMO-45ACP-230', 'WEST',  2,  8, 2,  7000),
  ('DEMO-45ACP-230', 'ALDA',  2, 12, 2,  5500),
  ('DEMO-45ACP-230', 'GI',    2,  6, 2,  4500),
  ('DEMO-357-158',   'WEST',  2, 22, 1,  3500),
  ('DEMO-357-158',   'ALDA',  2, 26, 1,  2800),
  ('DEMO-357-158',   'GI',    2, 20, 1,  2200),
  ('DEMO-44MAG-240', 'WEST',  3, 14, 4,  2500),
  ('DEMO-44MAG-240', 'ALDA',  3, 18, 4,  2000),
  ('DEMO-44MAG-240', 'GI',    3, 12, 4,  1800),
  -- rifle ammo (rows 4-7)
  ('DEMO-308-150',   'WEST',  4, 12, 2,  9500),
  ('DEMO-308-150',   'ALDA',  4, 16, 2,  7500),
  ('DEMO-308-150',   'GI',    4, 10, 2,  6500),
  ('DEMO-30-06-180', 'WEST',  4, 32, 2,  4500),
  ('DEMO-30-06-180', 'ALDA',  4, 36, 2,  3500),
  ('DEMO-30-06-180', 'GI',    4, 30, 2,  3000),
  ('DEMO-30-30-150', 'WEST',  5, 20, 3,  3000),
  ('DEMO-30-30-150', 'ALDA',  5, 24, 3,  2500),
  ('DEMO-30-30-150', 'GI',    5, 18, 3,  2000),
  ('DEMO-7MM-150',   'WEST',  5, 48, 2,  2200),
  ('DEMO-7MM-150',   'ALDA',  5, 52, 2,  1800),
  ('DEMO-7MM-150',   'GI',    5, 46, 2,  1500),
  ('DEMO-270-130',   'WEST',  6, 26, 2,  2800),
  ('DEMO-270-130',   'ALDA',  6, 30, 2,  2200),
  ('DEMO-270-130',   'GI',    6, 24, 2,  1900),
  ('DEMO-223-55',    'WEST',  7,  6, 3, 11000),
  ('DEMO-223-55',    'ALDA',  7, 10, 3,  9000),
  ('DEMO-223-55',    'GI',    7,  4, 3,  7500),
  -- shotgun ammo (rows 8-9)
  ('DEMO-12GA-00',   'WEST',  8, 16, 2,  4800),
  ('DEMO-12GA-00',   'ALDA',  8, 20, 2,  3800),
  ('DEMO-12GA-00',   'GI',    8, 14, 2,  3200),
  ('DEMO-12GA-SLUG', 'WEST',  8, 40, 3,  2800),
  ('DEMO-12GA-SLUG', 'ALDA',  8, 44, 3,  2200),
  ('DEMO-12GA-SLUG', 'GI',    8, 38, 3,  1800),
  ('DEMO-20GA-275',  'WEST',  9, 22, 2,  2400),
  ('DEMO-20GA-275',  'ALDA',  9, 26, 2,  2000),
  ('DEMO-20GA-275',  'GI',    9, 20, 2,  1700),
  -- rimfire (row 10)
  ('DEMO-22LR-40',   'WEST', 10, 10, 1, 25000),
  ('DEMO-22LR-40',   'ALDA', 10, 14, 1, 18000),
  ('DEMO-22LR-40',   'GI',   10,  8, 1, 14000),
  ('DEMO-22WMR-40',  'WEST', 10, 44, 2,  6500),
  ('DEMO-22WMR-40',  'ALDA', 10, 48, 2,  5500),
  ('DEMO-22WMR-40',  'GI',   10, 42, 2,  4200),
  -- components (rows 11-12)
  ('DEMO-BRASS-9MM', 'WEST', 11, 18, 1,   850),
  ('DEMO-BRASS-9MM', 'ALDA', 11, 22, 1,   650),
  ('DEMO-BRASS-9MM', 'GI',   11, 16, 1,   500),
  ('DEMO-PRIMER-LR', 'WEST', 12, 30, 4,  4200),
  ('DEMO-PRIMER-LR', 'ALDA', 12, 34, 4,  3200),
  ('DEMO-PRIMER-LR', 'GI',   12, 28, 4,  2400);
