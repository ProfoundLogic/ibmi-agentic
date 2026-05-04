-- Sample data loader for PROPRTYP (~30 NYC-flavored property listings).
-- Idempotent: DELETE-then-INSERT so it may be re-run safely.
-- PGUID is GENERATED ALWAYS AS IDENTITY and is omitted from the column list.

DELETE FROM PROPRTYP;

INSERT INTO PROPRTYP
  (PSTRNO, PSTRNAM, PUNIT, PBORO, PCITY, PSTATE, PZIP,
   PBEDS, PBATHS, PSQFT, PRENT,
   PKITCHN, PLAUNDR, PHEAT, PUTILS, PPETS, PAVAILD)
VALUES
  ('245',   'East 86th Street',         'Apt 12B',  'Manhattan',     'New York',      'NY', '10028',
   2, 2.0, 1100, 4850.00, 'PREMIUM',  'IN_UNIT',  'Y', 'N', 'CATS_ONLY',  '2026-06-01'),

  ('1247',  'Grand Concourse',          'Apt 4B',   'Bronx',         'Bronx',         'NY', '10456',
   3, 1.0,  950, 2150.00, 'BASIC',    'BUILDING', 'Y', 'Y', 'NONE',       '2026-05-15'),

  ('425',   'Court Street',             '',         'Brooklyn',      'Brooklyn',      'NY', '11231',
   1, 1.0,  680, 2950.00, 'STANDARD', 'IN_UNIT',  'Y', 'N', 'SMALL_DOGS', '2026-06-15'),

  ('88',    'Greenwich Street',         'Unit 1502','Manhattan',     'New York',      'NY', '10006',
   1, 1.0,  725, 4200.00, 'PREMIUM',  'IN_UNIT',  'Y', 'Y', 'NONE',       '2026-05-20'),

  ('37-21', 'Main Street',              'Apt 7C',   'Queens',        'Flushing',      'NY', '11354',
   2, 1.0,  880, 2650.00, 'STANDARD', 'BUILDING', 'Y', 'N', 'CATS_ONLY',  '2026-07-01'),

  ('2150',  'Adam Clayton Powell Jr Blvd','Apt 11G','Manhattan',     'New York',      'NY', '10027',
   0, 1.0,  475, 2350.00, 'BASIC',    'BUILDING', 'Y', 'Y', 'NONE',       '2026-06-01'),

  ('512',   'Hylan Boulevard',          '',         'Staten Island', 'Staten Island', 'NY', '10305',
   3, 2.0, 1450, 3100.00, 'STANDARD', 'IN_UNIT',  'N', 'N', 'ALL_OK',     '2026-05-10'),

  ('760',   'Eastern Parkway',          'Apt 3D',   'Brooklyn',      'Brooklyn',      'NY', '11213',
   2, 1.0,  825, 2400.00, 'BASIC',    'HOOKUPS',  'Y', 'N', 'SMALL_DOGS', '2026-06-01'),

  ('34-15', '31st Avenue',              'Apt 2A',   'Queens',        'Astoria',       'NY', '11106',
   2, 1.0,  900, 2750.00, 'STANDARD', 'BUILDING', 'Y', 'Y', 'CATS_ONLY',  '2026-05-25'),

  ('215',   'East 68th Street',         'Apt 9F',   'Manhattan',     'New York',      'NY', '10065',
   1, 1.5,  790, 3950.00, 'PREMIUM',  'IN_UNIT',  'Y', 'N', 'NONE',       '2026-06-10'),

  ('1880',  'Webster Avenue',           'Apt 5C',   'Bronx',         'Bronx',         'NY', '10457',
   3, 1.0, 1020, 2050.00, 'BASIC',    'BUILDING', 'Y', 'Y', 'ALL_OK',     '2026-05-15'),

  ('180',   'Bedford Avenue',           'Apt 2L',   'Brooklyn',      'Brooklyn',      'NY', '11211',
   1, 1.0,  650, 3450.00, 'PREMIUM',  'IN_UNIT',  'Y', 'N', 'CATS_ONLY',  '2026-07-01'),

  ('155',   'Bay Street',               'Apt 7',    'Staten Island', 'Staten Island', 'NY', '10301',
   2, 1.0,  925, 2250.00, 'STANDARD', 'HOOKUPS',  'N', 'N', 'SMALL_DOGS', '2026-06-01'),

  ('2954',  'Brighton 4th Street',      'Apt 3B',   'Brooklyn',      'Brooklyn',      'NY', '11235',
   3, 1.5, 1100, 2850.00, 'STANDARD', 'BUILDING', 'Y', 'Y', 'CATS_ONLY',  '2026-05-20'),

  ('350',   'East 79th Street',         'PH 2',     'Manhattan',     'New York',      'NY', '10075',
   3, 2.5, 1875, 8200.00, 'PREMIUM',  'IN_UNIT',  'Y', 'N', 'ALL_OK',     '2026-06-15'),

  ('1675',  'Crotona Avenue',           'Apt 6A',   'Bronx',         'Bronx',         'NY', '10457',
   4, 1.5, 1180, 2400.00, 'BASIC',    'BUILDING', 'Y', 'Y', 'NONE',       '2026-05-10'),

  ('88-22', '178th Street',             '',         'Queens',        'Jamaica',       'NY', '11432',
   3, 2.0, 1320, 2950.00, 'STANDARD', 'IN_UNIT',  'N', 'N', 'ALL_OK',     '2026-06-01'),

  ('110',   'Horatio Street',           'Apt 4D',   'Manhattan',     'New York',      'NY', '10014',
   1, 1.0,  710, 4650.00, 'PREMIUM',  'IN_UNIT',  'Y', 'N', 'CATS_ONLY',  '2026-07-15'),

  ('40-15', '82nd Street',              'Apt 1F',   'Queens',        'Jackson Heights','NY','11372',
   2, 1.0,  860, 2350.00, 'BASIC',    'BUILDING', 'Y', 'Y', 'SMALL_DOGS', '2026-05-20'),

  ('275',   'Greenwich Street',         'Apt 8C',   'Manhattan',     'New York',      'NY', '10007',
   2, 2.0, 1050, 5400.00, 'PREMIUM',  'IN_UNIT',  'Y', 'N', 'NONE',       '2026-06-10'),

  ('545',   'West 45th Street',         'Apt 12J',  'Manhattan',     'New York',      'NY', '10036',
   2, 1.0,  840, 3650.00, 'STANDARD', 'BUILDING', 'Y', 'N', 'CATS_ONLY',  '2026-05-25'),

  ('345',   'Carroll Street',           '',         'Brooklyn',      'Brooklyn',      'NY', '11231',
   2, 1.5,  980, 3850.00, 'STANDARD', 'IN_UNIT',  'Y', 'N', 'SMALL_DOGS', '2026-06-15'),

  ('4502',  'Broadway',                 'Apt 2A',   'Manhattan',     'New York',      'NY', '10040',
   3, 1.0, 1080, 2750.00, 'BASIC',    'HOOKUPS',  'Y', 'Y', 'ALL_OK',     '2026-05-15'),

  ('1100',  'Fulton Street',            'Apt 4G',   'Brooklyn',      'Brooklyn',      'NY', '11238',
   1, 1.0,  620, 2650.00, 'STANDARD', 'BUILDING', 'Y', 'N', 'CATS_ONLY',  '2026-06-01'),

  ('255',   'New Dorp Lane',            '',         'Staten Island', 'Staten Island', 'NY', '10306',
   4, 2.5, 1620, 3450.00, 'STANDARD', 'IN_UNIT',  'N', 'N', 'ALL_OK',     '2026-07-01'),

  ('231',   'Grand Street',             'Apt 5B',   'Manhattan',     'New York',      'NY', '10013',
   2, 1.0,  780, 4150.00, 'PREMIUM',  'IN_UNIT',  'Y', 'N', 'CATS_ONLY',  '2026-06-15'),

  ('1240',  'Boynton Avenue',           'Apt 2D',   'Bronx',         'Bronx',         'NY', '10472',
   2, 1.0,  790, 1950.00, 'BASIC',    'BUILDING', 'Y', 'Y', 'NONE',       '2026-05-10'),

  ('83-15', 'Britton Avenue',           'Apt 6E',   'Queens',        'Elmhurst',      'NY', '11373',
   1, 1.0,  680, 2150.00, 'BASIC',    'BUILDING', 'Y', 'Y', 'CATS_ONLY',  '2026-06-01'),

  ('650',   'West 168th Street',        'Apt 9H',   'Manhattan',     'New York',      'NY', '10032',
   2, 1.0,  870, 2950.00, 'STANDARD', 'BUILDING', 'Y', 'Y', 'SMALL_DOGS', '2026-05-20'),

  ('155',   'Page Avenue',              '',         'Staten Island', 'Staten Island', 'NY', '10307',
   4, 3.0, 2150, 4250.00, 'PREMIUM',  'IN_UNIT',  'N', 'N', 'ALL_OK',     '2026-07-01'),

  ('1320',  'Surf Avenue',              'Apt 3F',   'Brooklyn',      'Brooklyn',      'NY', '11224',
   0, 1.0,  430, 1850.00, 'BASIC',    'BUILDING', 'Y', 'Y', 'NONE',       '2026-06-15');
