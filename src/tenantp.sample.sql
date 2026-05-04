-- Sample data loader for TENANTP (~30 NYC-flavored records).
-- Idempotent: DELETE-then-INSERT so it may be re-run safely.
-- TGUID is GENERATED ALWAYS AS IDENTITY and is omitted from the column list.

DELETE FROM TENANTP;

INSERT INTO TENANTP
  (TFNAME, TMNAME, TLNAME, TDOB, TSSN4,
   TADDR1, TADDR2, TCITY, TSTATE, TZIP,
   TPHONE, TEMAIL, THHSIZE, TINCOME,
   TEMPLOY, TECNAME, TECPHN)
VALUES
  ('Maria',    'Elena', 'Rodriguez',  '1985-03-12', '4821',
   '1247 Grand Concourse',          'Apt 4B',     'Bronx',         'NY', '10456',
   '347-555-0142', 'maria.rodriguez@email.com',  4,  62500.00,
   'NYC Health + Hospitals',        'Carlos Rodriguez',           '347-555-0143'),

  ('James',    'Robert','OConnor',    '1978-07-23', '7392',
   '425 Court Street',              '',           'Brooklyn',      'NY', '11231',
   '718-555-0234', 'jim.oconnor@email.com',      2, 118000.00,
   'Goldman Sachs',                 'Patricia OConnor',           '718-555-0235'),

  ('Aisha',    'N',     'Patel',      '1992-11-04', '2156',
   '88 Greenwich Street',           'Unit 1502',  'New York',      'NY', '10006',
   '212-555-0356', 'aisha.patel@email.com',      1,  95000.00,
   'JPMorgan Chase',                'Raj Patel',                  '212-555-0357'),

  ('David',    'Michael','Chen',      '1980-05-19', '6743',
   '37-21 Main Street',             'Apt 7C',     'Flushing',      'NY', '11354',
   '718-555-0468', 'dchen@email.com',            5,  78000.00,
   'NYC Department of Education',   'Linda Chen',                 '718-555-0469'),

  ('Tiana',    '',      'Williams',   '1995-09-08', '3284',
   '2150 Adam Clayton Powell Jr Blvd','Apt 11G',  'New York',      'NY', '10027',
   '917-555-0571', 'tiana.williams@email.com',   3,  54000.00,
   'Mount Sinai Hospital',          'Janelle Williams',           '917-555-0572'),

  ('Giovanni', 'A',     'Rossi',      '1972-02-28', '9105',
   '512 Hylan Boulevard',           '',           'Staten Island', 'NY', '10305',
   '718-555-0683', 'g.rossi@email.com',          4,  87500.00,
   'NYPD',                          'Sofia Rossi',                '718-555-0684'),

  ('Latoya',   'M',     'Jackson',    '1988-06-14', '1547',
   '760 Eastern Parkway',           'Apt 3D',     'Brooklyn',      'NY', '11213',
   '347-555-0795', 'latoya.jackson@email.com',   3,  47000.00,
   'NYC MTA',                       'Andre Jackson',              '347-555-0796'),

  ('Mohammed', '',      'Al-Hassan',  '1983-12-01', '8264',
   '34-15 31st Avenue',             'Apt 2A',     'Astoria',       'NY', '11106',
   '718-555-0807', 'm.alhassan@email.com',       6,  72000.00,
   'Astoria Bank',                  'Fatima Al-Hassan',           '718-555-0808'),

  ('Rebecca',  'Lynn',  'Goldberg',   '1990-04-22', '5719',
   '215 East 68th Street',          'Apt 9F',     'New York',      'NY', '10065',
   '212-555-0918', 'r.goldberg@email.com',       2, 142000.00,
   'NewYork-Presbyterian Hospital', 'Daniel Goldberg',            '212-555-0919'),

  ('Marcus',   'J',     'Thompson',   '1986-08-30', '4076',
   '1880 Webster Avenue',           'Apt 5C',     'Bronx',         'NY', '10457',
   '347-555-1029', 'marcus.thompson@email.com',  5,  41500.00,
   'NYC Sanitation Dept',           'Denise Thompson',            '347-555-1030'),

  ('Sarah',    'E',     'Kim',        '1994-01-17', '6831',
   '180 Bedford Avenue',            'Apt 2L',     'Brooklyn',      'NY', '11211',
   '917-555-1132', 'sarah.kim@email.com',        1,  68000.00,
   'Etsy',                          'Min-Jun Kim',                '917-555-1133'),

  ('Anthony',  'V',     'DeLuca',     '1965-10-05', '2948',
   '155 Bay Street',                'Apt 7',      'Staten Island', 'NY', '10301',
   '718-555-1244', 'a.deluca@email.com',         3,  91000.00,
   'FDNY',                          'Joanne DeLuca',              '718-555-1245'),

  ('Yelena',   '',      'Volkov',     '1981-03-09', '7155',
   '2954 Brighton 4th Street',      'Apt 3B',     'Brooklyn',      'NY', '11235',
   '347-555-1356', 'y.volkov@email.com',         4,  56500.00,
   'Maimonides Medical Center',     'Boris Volkov',               '347-555-1357'),

  ('Christopher','James','Murphy',    '1976-07-12', '8392',
   '350 East 79th Street',          'PH 2',       'New York',      'NY', '10075',
   '212-555-1467', 'chris.murphy@email.com',     4, 235000.00,
   'Morgan Stanley',                'Kathleen Murphy',            '212-555-1468'),

  ('Ngozi',    '',      'Okafor',     '1991-11-26', '5613',
   '1675 Crotona Avenue',           'Apt 6A',     'Bronx',         'NY', '10457',
   '347-555-1579', 'ngozi.okafor@email.com',     5,  38000.00,
   'Lincoln Hospital',              'Chinedu Okafor',             '347-555-1580'),

  ('Kevin',    'Patrick','Sullivan',  '1969-09-15', '4267',
   '88-22 178th Street',            '',           'Jamaica',       'NY', '11432',
   '718-555-1681', 'k.sullivan@email.com',       2,  76000.00,
   'Con Edison',                    'Maureen Sullivan',           '718-555-1682'),

  ('Priya',    'R',     'Sharma',     '1989-06-03', '3478',
   '110 Horatio Street',            'Apt 4D',     'New York',      'NY', '10014',
   '917-555-1793', 'priya.sharma@email.com',     1, 165000.00,
   'Google NYC',                    'Arjun Sharma',               '917-555-1794'),

  ('Damian',   '',      'Reyes',      '1984-12-19', '9026',
   '40-15 82nd Street',             'Apt 1F',     'Jackson Heights','NY','11372',
   '347-555-1805', 'd.reyes@email.com',          7,  52000.00,
   'NYC Parks Department',          'Lucia Reyes',                '347-555-1806'),

  ('Olivia',   'Grace', 'Bennett',    '1996-02-14', '6541',
   '275 Greenwich Street',          'Apt 8C',     'New York',      'NY', '10007',
   '212-555-1917', 'olivia.bennett@email.com',   1,  88000.00,
   'NYC Department of Finance',     'William Bennett',            '212-555-1918'),

  ('Hassan',   'O',     'Ibrahim',    '1973-04-25', '7283',
   '545 West 45th Street',          'Apt 12J',    'New York',      'NY', '10036',
   '917-555-2029', 'h.ibrahim@email.com',        4,  67500.00,
   'NYC Taxi & Limousine Commission','Amina Ibrahim',             '917-555-2030'),

  ('Rachel',   'Beth',  'Stein',      '1987-08-08', '5934',
   '345 Carroll Street',            '',           'Brooklyn',      'NY', '11231',
   '347-555-2131', 'rachel.stein@email.com',     3, 124000.00,
   'NYU Langone',                   'Michael Stein',              '347-555-2132'),

  ('Tomas',    '',      'Garcia',     '1979-05-30', '4815',
   '4502 Broadway',                 'Apt 2A',     'New York',      'NY', '10040',
   '212-555-2243', 't.garcia@email.com',         5,  44000.00,
   'NYC DOT',                       'Esperanza Garcia',           '212-555-2244'),

  ('Jasmine',  'A',     'Brown',      '1993-10-21', '6172',
   '1100 Fulton Street',            'Apt 4G',     'Brooklyn',      'NY', '11238',
   '347-555-2355', 'jasmine.brown@email.com',    2,  59000.00,
   'Brooklyn Public Library',       'Tyrone Brown',               '347-555-2356'),

  ('Liam',     'Christopher','Walsh', '1982-01-11', '8704',
   '255 New Dorp Lane',             '',           'Staten Island', 'NY', '10306',
   '718-555-2467', 'l.walsh@email.com',          5,  82500.00,
   'Richmond University Medical Center','Bridget Walsh',          '718-555-2468'),

  ('Wei',      '',      'Zhang',      '1971-06-07', '3691',
   '231 Grand Street',              'Apt 5B',     'New York',      'NY', '10013',
   '212-555-2579', 'wei.zhang@email.com',        3,  71000.00,
   'NYC Department of Transportation','Jing Zhang',                '212-555-2580'),

  ('Destiny',  'M',     'Morales',    '1997-09-02', '2438',
   '1240 Boynton Avenue',           'Apt 2D',     'Bronx',         'NY', '10472',
   '347-555-2681', 'd.morales@email.com',        4,  35500.00,
   'Bronx-Lebanon Hospital',        'Hector Morales',              '347-555-2682'),

  ('Robert',   'Eugene','Schmidt',    '1958-11-29', '9357',
   '83-15 Britton Avenue',          'Apt 6E',     'Elmhurst',      'NY', '11373',
   '718-555-2793', 'r.schmidt@email.com',        2,  98000.00,
   'Verizon',                       'Helen Schmidt',                '718-555-2794'),

  ('Amara',    '',      'Diallo',     '1990-03-16', '5028',
   '650 West 168th Street',         'Apt 9H',     'New York',      'NY', '10032',
   '917-555-2805', 'a.diallo@email.com',         3,  61000.00,
   'Columbia University Medical Center','Ousmane Diallo',         '917-555-2806'),

  ('Frank',    'J',     'Russo',      '1962-12-22', '7140',
   '155 Page Avenue',               '',           'Staten Island', 'NY', '10307',
   '718-555-2917', 'frank.russo@email.com',      4, 105000.00,
   'NY Waterway',                   'Theresa Russo',                '718-555-2918'),

  ('Brianna',  'N',     'Carter',     '1998-07-04', '4683',
   '1320 Surf Avenue',              'Apt 3F',     'Brooklyn',      'NY', '11224',
   '347-555-3029', 'b.carter@email.com',         1,  42000.00,
   'NYC Aquarium',                  'Monique Carter',               '347-555-3030');
