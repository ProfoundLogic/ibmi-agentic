"""Reference data for the Pratt Industries purchasing demo.

Content is modelled on Pratt's real business as described on prattindustries.com:
six 100% recycled paper mills (Conyers GA, Staten Island NY, Valparaiso IN,
Shreveport LA, Wapakoneta OH, Henderson KY) feeding corrugating, converting and
retail-display plants, with a recycling division collecting OCC.  The vendor
part catalogue is therefore what a vertically integrated recycled-containerboard
business actually buys: mill chemicals and clothing, corrugator and flexo
consumables, baling and strapping supplies, and plant MRO.

Vendor names are invented. They are deliberately generic industrial names so the
demo does not imply a trading relationship with any real supplier.
"""

# --- Business areas (OHPP / OHNN). The drop-downs filter COAPLT BETWEEN 93 AND 99 ---
PLANTS = [
    # plt, name,                               short,          street,                      city/state/zip,            org,  lcacct, costctr
    ('93', 'Pratt Paper - Valparaiso IN',      'VALPO',        '3050 Anthony Pratt Drive',  'Valparaiso, IN  46383-0000', 'PPIN',  9301, '710300'),
    ('94', 'Pratt Industries - Conyers GA',    'CONYERS',      '1800-A Sarasota Bus Pkwy',  'Conyers, GA  30013-0000',    'PIUS',  9401, '700100'),
    ('95', 'Pratt Paper - Shreveport LA',      'SHREVEPORT',   '10429 Richard Pratt Drive', 'Shreveport, LA  71115-0000', 'PPLA',  9501, '720500'),
    ('96', 'Pratt Paper - Wapakoneta OH',      'WAPAK',        '2860 County Road 25A',      'Wapakoneta, OH  45895-0000', 'PPOH',  9601, '730600'),
    ('97', 'Pratt Paper - Henderson KY',       'HENDERSON',    '6303 Highway 425 West',     'Henderson, KY  42420-0000',  'PPKY',  9701, '740700'),
    ('98', 'Pratt Paper - Staten Island NY',   'STATEN ISL',   '400 Western Avenue',        'Staten Island, NY  10303-0000', 'PPNY', 9801, '750800'),
    ('99', 'Pratt Display - Atlanta GA',       'DISPLAY',      '2050 Westside Parkway',     'Alpharetta, GA  30004-0000', 'PDUS',  9901, '760900'),
]

# --- Warehouses / destinations (OHDC).  COADEL 'S'/'W' makes them selectable ---
WAREHOUSES = [
    # dc,  name,                              short,        parent plant, street,                     city/state/zip,               region
    ('CO', 'Conyers GA Mill Store',           'CONYERS WH', '94', '1800-B Sarasota Bus Pkwy', 'Conyers, GA  30013-0000',    'SOUTHEAST'),
    ('AT', 'Atlanta Display Store',           'ATLANTA WH', '99', '2050 Westside Parkway',    'Alpharetta, GA  30004-0000', 'SOUTHEAST'),
    ('VA', 'Valparaiso IN Mill Store',        'VALPO WH',   '93', '3050 Anthony Pratt Drive', 'Valparaiso, IN  46383-0000', 'MIDWEST'),
    ('SH', 'Shreveport LA Mill Store',        'SHREVE WH',  '95', '10429 Richard Pratt Drive','Shreveport, LA  71115-0000', 'SOUTHCENTRAL'),
    ('WA', 'Wapakoneta OH Mill Store',        'WAPAK WH',   '96', '2860 County Road 25A',     'Wapakoneta, OH  45895-0000', 'MIDWEST'),
    ('HE', 'Henderson KY Mill Store',         'HENDRSN WH', '97', '6303 Highway 425 West',    'Henderson, KY  42420-0000',  'MIDWEST'),
    ('SI', 'Staten Island NY Mill Store',     'STATEN WH',  '98', '400 Western Avenue',       'Staten Island, NY  10303-0000','NORTHEAST'),
    ('MS', 'Mill Direct - no warehouse',      'MILL DIRECT','94', '1800-A Sarasota Bus Pkwy', 'Conyers, GA  30013-0000',    'SOUTHEAST'),
    ('DS', 'Drop Ship to Customer',           'DROP SHIP',  '94', '1800-A Sarasota Bus Pkwy', 'Conyers, GA  30013-0000',    'SOUTHEAST'),
]

# --- Freight terms (SAPINCO) ---
FREIGHT = [
    ('PPA', 'Prepaid and Add'),
    ('PPD', 'Prepaid'),
    ('CPU', 'Customer Pick Up'),
    ('COL', 'Collect'),
    ('FOB', 'FOB Origin'),
    ('DDP', 'Delivered Duty Paid'),
    ('EXW', 'Ex Works'),
]

# --- Vendors.  7750 / 7751 / 7799 / 7805 are special-cased in PODTLUI. ---
VENDORS = [
    (7701, 'MIDLAND STARCH & ADHESIVES',     ' ', 'orders@midlandstarch.example.com'),
    (7705, 'GULF COAST CAUSTIC SUPPLY',      ' ', 'po@gulfcoastcaustic.example.com'),
    (7710, 'APEX PAPER MACHINE CLOTHING',    ' ', 'service@apexclothing.example.com'),
    (7715, 'CLEARWATER PROCESS CHEMICALS',   ' ', 'cs@clearwaterprocess.example.com'),
    (7720, 'NORTHSTAR FLEXO INKS',           ' ', 'orders@northstarflexo.example.com'),
    (7725, 'PIEDMONT PLATE & PREPRESS',      ' ', 'prepress@piedmontplate.example.com'),
    (7730, 'ANILOX ROLL SERVICES INC',       ' ', 'sales@aniloxroll.example.com'),
    (7735, 'CORRUGATING ROLL TECHNOLOGIES',  ' ', 'quotes@corrugroll.example.com'),
    (7740, 'ATLANTIC BALING WIRE CO',        ' ', 'orders@atlanticbaling.example.com'),
    (7745, 'SUMMIT STRAPPING & FILM',        ' ', 'sales@summitstrapping.example.com'),
    (7750, 'EAGLE PALLET SYSTEMS',           ' ', 'orders@eaglepallet.example.com'),
    (7751, 'EAGLE LOGISTICS SERVICES',       ' ', 'dispatch@eaglelogistics.example.com'),
    (7755, 'KEYSTONE HOT MELT ADHESIVES',    ' ', 'cs@keystonehotmelt.example.com'),
    (7760, 'PRECISION BLADE & KNIFE',        ' ', 'orders@precisionblade.example.com'),
    (7765, 'DELTA CONVEYOR BELTING',         ' ', 'sales@deltabelting.example.com'),
    (7770, 'IRONSIDE BEARING & POWER TRANS', ' ', 'orders@ironsidebearing.example.com'),
    (7775, 'LUMEN INDUSTRIAL LIGHTING',      ' ', 'projects@lumenind.example.com'),
    (7780, 'SAFEGUARD PPE DISTRIBUTORS',     ' ', 'orders@safeguardppe.example.com'),
    (7785, 'TRIAD LUBRICANTS & FLUIDS',      ' ', 'cs@triadlubricants.example.com'),
    (7790, 'CAROLINA CORNER & EDGEBOARD',    ' ', 'sales@carolinaedge.example.com'),
    (7795, 'BLUEGRASS STITCHING WIRE',       'D', 'orders@bluegrasswire.example.com'),
    (7799, 'EAGLE PACKAGING GROUP',          ' ', 'eag@eaglepackaging.example.com'),
    (7805, 'EAGLE DISPLAY CONVERTING',       ' ', 'display@eagleconverting.example.com'),
    (7810, 'HARBOR LITHO LABEL',             ' ', 'csr@harborlitho.example.com'),
    (7815, 'CASCADE FOAM & PROTECTIVE',      ' ', 'orders@cascadefoam.example.com'),
    (7820, 'MERIDIAN OCC BROKERS',           ' ', 'trading@meridianocc.example.com'),
]

# --- Vendor part catalogue -------------------------------------------------
# (part, vendor, major, minor, sku, description, desc2, uom, lead days,
#  units/pack, base unit cost, over-receipt tolerance %)
# Major codes double as the SAP material group feed in RTVSAPACCT.
PARTS = [
 # ---- Paper mill: wet end chemicals and starch -------------------------------
 ('STR-CORN-4500',   7701,'110','010','STARCH-CORN',  'CORN STARCH CORRUGATING GRADE','BULK TRUCKLOAD 45,000 LB',    'LB', 10, 45000,   0.2850, 2.00),
 ('STR-ETHYL-2200',  7701,'110','011','STARCH-ETH',   'ETHYLATED STARCH HIGH SPEED',  '2,200 LB SUPER SACK',        'LB', 14,  2200,   0.4150, 2.00),
 ('ADH-BORAX-50',    7701,'110','012','BORAX-50',     'BORAX DECAHYDRATE TECH GRADE', '50 LB BAG',                  'BAG',  7,    50,  28.5000, 0.00),
 ('CHM-CAUSTIC-50',  7705,'120','020','CAUSTIC-50',   'CAUSTIC SODA 50% LIQUID',      'BULK TANKER 4,000 GAL',      'GAL', 12,  4000,   3.9500, 1.00),
 ('CHM-ALUM-SULF',   7705,'120','021','ALUM-SULF',    'ALUMINUM SULFATE LIQUID',      'BULK TANKER',                'GAL', 12,  4000,   1.8700, 1.00),
 ('CHM-AKD-SIZE',    7715,'120','022','AKD-SIZE',     'AKD SIZING EMULSION',          '275 GAL TOTE',               'GAL', 15,   275,   9.4000, 0.00),
 ('CHM-RETENT-AID',  7715,'120','023','RETENT-AID',   'CATIONIC RETENTION AID',       '275 GAL TOTE',               'GAL', 15,   275,  11.2500, 0.00),
 ('CHM-DEFOAM-55',   7715,'120','024','DEFOAM-55',    'SILICONE DEFOAMER',            '55 GAL DRUM',                'DRM', 10,     1, 742.0000, 0.00),
 ('CHM-BIOCIDE-55',  7715,'120','025','BIOCIDE-55',   'BROAD SPECTRUM BIOCIDE',       '55 GAL DRUM',                'DRM', 10,     1, 915.0000, 0.00),
 ('CHM-WETSTR-275',  7715,'120','026','WET-STRENGTH', 'WET STRENGTH RESIN PAE',       '275 GAL TOTE',               'GAL', 18,   275,   7.6500, 0.00),
 # ---- Paper machine clothing and consumables ---------------------------------
 ('PMC-FELT-PRESS1', 7710,'130','030','FELT-P1',      'PRESS FELT NO.1 POSITION',     'CUSTOM WIDTH PM2',           'EA',  60,     1,28400.0000, 0.00),
 ('PMC-FELT-PRESS2', 7710,'130','031','FELT-P2',      'PRESS FELT NO.2 POSITION',     'CUSTOM WIDTH PM2',           'EA',  60,     1,26750.0000, 0.00),
 ('PMC-FORM-FABRIC', 7710,'130','032','FORM-FAB',     'FORMING FABRIC TRIPLE LAYER',  'CUSTOM WIDTH PM1',           'EA',  75,     1,41200.0000, 0.00),
 ('PMC-DRYER-SCRN',  7710,'130','033','DRYER-SCRN',   'DRYER FABRIC SPIRAL LINK',     'CUSTOM WIDTH PM1',           'EA',  75,     1,19800.0000, 0.00),
 ('PMC-DOCTOR-BLD',  7710,'130','034','DOCTOR-BLD',   'DOCTOR BLADE CARBON FIBRE',    'BOX OF 10 - 4.0M',           'BOX', 21,    10, 486.0000, 5.00),
 # ---- Corrugator ------------------------------------------------------------
 ('COR-ROLL-A-FLT',  7735,'140','040','ROLL-A',       'CORRUGATING ROLL A FLUTE',     'REGROUND EXCHANGE',          'EA',  90,     1,38500.0000, 0.00),
 ('COR-ROLL-C-FLT',  7735,'140','041','ROLL-C',       'CORRUGATING ROLL C FLUTE',     'REGROUND EXCHANGE',          'EA',  90,     1,38500.0000, 0.00),
 ('COR-ROLL-E-FLT',  7735,'140','042','ROLL-E',       'CORRUGATING ROLL E FLUTE',     'REGROUND EXCHANGE',          'EA',  90,     1,41000.0000, 0.00),
 ('COR-BELT-DBLBK',  7765,'140','043','BELT-DB',      'DOUBLE BACKER BELT',           'ENDLESS 2.8M X 42M',         'EA',  45,     1,22400.0000, 0.00),
 ('COR-KNIFE-SLIT',  7760,'140','044','KNIFE-SLIT',   'SLITTER KNIFE TUNGSTEN',       'SET OF 12',                  'SET', 20,    12, 1840.0000, 0.00),
 ('COR-BLADE-SCORE', 7760,'140','045','BLADE-SCR',    'SCORING BLADE HARDENED',       'SET OF 12',                  'SET', 20,    12,  965.0000, 0.00),
 # ---- Flexo print and converting --------------------------------------------
 ('INK-FLEXO-CYAN',  7720,'150','050','INK-CYAN',     'WATER BASED FLEXO INK CYAN',   '55 GAL DRUM',                'DRM',  7,     1, 1017.5000, 3.00),
 ('INK-FLEXO-MAG',   7720,'150','051','INK-MAG',      'WATER BASED FLEXO INK MAGENTA','55 GAL DRUM',                'DRM',  7,     1, 1072.5000, 3.00),
 ('INK-FLEXO-YEL',   7720,'150','052','INK-YEL',      'WATER BASED FLEXO INK YELLOW', '55 GAL DRUM',                'DRM',  7,     1,  990.0000, 3.00),
 ('INK-FLEXO-BLK',   7720,'150','053','INK-BLK',      'WATER BASED FLEXO INK BLACK',  '55 GAL DRUM',                'DRM',  7,     1,  907.5000, 3.00),
 ('INK-FLEXO-PMS',   7720,'150','054','INK-PMS',      'FLEXO INK PMS MATCH CUSTOM',   '5 GAL PAIL',                 'PAL', 10,     1,  212.0000, 5.00),
 ('PLT-PHOTOPOLY',   7725,'150','055','PLATE-PP',     'PHOTOPOLYMER PRINT PLATE',     'PER SQUARE INCH',            'SI',   5,     1,    0.3850,10.00),
 ('PLT-MOUNT-TAPE',  7725,'150','056','MNT-TAPE',     'PLATE MOUNTING TAPE',          'CASE OF 12 ROLLS',           'CS',   5,    12,  284.0000, 5.00),
 ('ANX-ROLL-360',    7730,'150','057','ANILOX-360',   'ANILOX ROLL 360 LPI CERAMIC',  'RECHROME EXCHANGE',          'EA',  35,     1, 4850.0000, 0.00),
 ('ANX-ROLL-440',    7730,'150','058','ANILOX-440',   'ANILOX ROLL 440 LPI CERAMIC',  'RECHROME EXCHANGE',          'EA',  35,     1, 5120.0000, 0.00),
 ('ADH-HOTMELT-40',  7755,'150','059','HOTMELT-40',   'HOT MELT ADHESIVE PELLETS',    '40 LB CASE',                 'CS',   7,    40,   96.4000, 3.00),
 ('WIR-STITCH-GAL',  7795,'150','060','STITCH-WIRE',  'GALVANISED STITCHING WIRE',    '100 LB SPOOL',               'SPL', 14,   100,  172.0000, 2.00),
 ('TPE-SEAL-CARTON', 7745,'150','061','TAPE-SEAL',    'CARTON SEALING TAPE 72MM',     'CASE OF 24 ROLLS',           'CS',   5,    24,   68.4000, 5.00),
 # ---- Recycling division -----------------------------------------------------
 ('WIR-BALE-HT',     7740,'160','070','BALE-WIRE',    'BALING WIRE HIGH TENSILE 13GA','2,500 LB BUNDLE',            'LB',  12,  2500,    0.7200, 2.00),
 ('WIR-BALE-TIE',    7740,'160','071','BALE-TIE',     'SINGLE LOOP BALE TIES 12GA',   'BUNDLE OF 125',              'BDL', 12,   125,  118.0000, 2.00),
 ('BLT-SORT-SCRN',   7765,'160','072','SORT-DISC',    'OCC SORTING SCREEN DISC',      'BOX OF 50',                  'BOX', 25,    50,  925.0000, 0.00),
 ('BLT-CONV-RECY',   7765,'160','073','CONV-BELT',    'RECYCLING CONVEYOR BELT',      'PER LINEAR FOOT',            'FT',  30,     1,   62.5000, 2.00),
 ('OCC-BALED-11',    7820,'160','074','OCC-11',       'OCC GRADE 11 BALED',           'PER SHORT TON',              'TON',  3,     1,  118.0000, 5.00),
 ('OCC-DLK-12',      7820,'160','075','DLK-12',       'DOUBLE LINED KRAFT GRADE 12',  'PER SHORT TON',              'TON',  3,     1,  146.0000, 5.00),
 # ---- Shipping and warehouse supplies ---------------------------------------
 ('PAL-48X40-GMA',   7750,'170','080','PALLET-GMA',   'PALLET 48X40 GMA GRADE A',     'TRUCKLOAD 520 EA',           'EA',   7,   520,    8.9500, 3.00),
 ('PAL-48X40-HT',    7750,'170','081','PALLET-HT',    'PALLET 48X40 HEAT TREATED',    'TRUCKLOAD 480 EA',           'EA',  10,   480,   12.4000, 3.00),
 ('FLM-STRETCH-80',  7745,'170','082','STRETCH-80',   'STRETCH FILM 80GA 20IN',       'CASE OF 4 ROLLS',            'CS',   5,     4,   42.0000, 5.00),
 ('STP-PET-12MM',    7745,'170','083','STRAP-PET',    'POLYESTER STRAPPING 12MM',     'COIL 3,000 FT',              'CL',   7,     1,  187.0000, 2.00),
 ('EDG-CORNER-2IN',  7790,'170','084','EDGE-2IN',     'EDGE PROTECTOR 2IN X 48IN',    'BUNDLE OF 200',              'BDL', 10,   200,  146.0000, 3.00),
 ('EDG-CORNER-3IN',  7790,'170','085','EDGE-3IN',     'EDGE PROTECTOR 3IN X 48IN',    'BUNDLE OF 150',              'BDL', 10,   150,  162.0000, 3.00),
 ('FOM-PROTECT-LAM', 7815,'170','086','FOAM-LAM',     'LAMINATED PROTECTIVE FOAM',    'ROLL 48IN X 250FT',          'RL',  12,     1,  238.0000, 2.00),
 # ---- Retail display / high graphics ----------------------------------------
 ('LAM-LITHO-LABEL', 7810,'180','090','LITHO-LBL',    'LITHO LAMINATE LABEL SHEET',   'PER 1,000 SHEETS',           'M',   15,  1000,  412.0000, 4.00),
 ('DSP-CORNER-POST', 7805,'180','091','DISP-POST',    'DISPLAY CORNER POST KRAFT',    'BUNDLE OF 500',              'BDL', 14,   500,  268.0000, 3.00),
 ('DSP-SHELF-TRAY',  7805,'180','092','DISP-TRAY',    'DISPLAY SHELF TRAY BLANK',     'BUNDLE OF 250',              'BDL', 14,   250,  395.0000, 3.00),
 ('DSP-HDR-CARD',    7810,'180','093','HDR-CARD',     'DISPLAY HEADER CARD PRINTED',  'PER 500',                    'C',   18,   500,  186.0000, 4.00),
 # ---- Plant MRO ---------------------------------------------------------------
 ('MRO-BEARING-SPH', 7770,'190','100','BEARING-SPH',  'SPHERICAL ROLLER BEARING',     '22220 CCK/W33',              'EA',  14,     1,  418.0000, 0.00),
 ('MRO-GEARBOX-OIL', 7785,'190','101','GEAR-OIL',     'SYNTHETIC GEAR OIL ISO 320',   '55 GAL DRUM',                'DRM',  7,     1,  1385.0000,0.00),
 ('MRO-HYD-FLUID',   7785,'190','102','HYD-FLUID',    'HYDRAULIC FLUID AW46',         '55 GAL DRUM',                'DRM',  7,     1,   742.0000,0.00),
 ('MRO-LED-HIGHBAY', 7775,'190','103','LED-HIGHBAY',  'LED HIGH BAY FIXTURE 240W',    'CASE OF 4',                  'CS',  21,     4,   986.0000,0.00),
 ('MRO-VFD-75HP',    7770,'190','104','VFD-75',       'VARIABLE FREQUENCY DRIVE 75HP','480V 3PH',                   'EA',  28,     1,  7240.0000,0.00),
 ('PPE-GLOVE-CUT',   7780,'190','110','GLOVE-CUT',    'CUT RESISTANT GLOVE LEVEL A4', 'CASE OF 72 PAIR',            'CS',   5,    72,   324.0000, 5.00),
 ('PPE-GLOVE-NIT',   7780,'190','111','GLOVE-NIT',    'NITRILE DISPOSABLE GLOVE',     'CASE OF 1000',               'CS',   5,  1000,    88.0000, 5.00),
 ('PPE-VEST-HIVIS',  7780,'190','112','VEST-HIVIS',   'HI VIS CLASS 2 SAFETY VEST',   'CASE OF 50',                 'CS',   7,    50,   412.0000, 5.00),
 ('PPE-EARPLUG',     7780,'190','113','EARPLUG',      'FOAM EAR PLUG CORDED',         'CASE OF 2000 PAIR',          'CS',   5,  2000,   196.0000, 5.00),
 ('PPE-BOOT-STEEL',  7780,'190','114','BOOT-ST',      'STEEL TOE WORK BOOT',          'PER PAIR',                   'PR',  10,     1,   118.0000, 0.00),
 # ---- Freight and services (freight lines must be qty 1) ----------------------
 ('SVC-FREIGHT-LTL', 7751,'900','900','FREIGHT',      'LTL FREIGHT CHARGE',           'PER SHIPMENT',               'EA',   1,     1,     0.0000, 0.00),
 ('SVC-FREIGHT-TL',  7751,'900','901','FREIGHT-TL',   'TRUCKLOAD FREIGHT CHARGE',     'PER SHIPMENT',               'EA',   1,     1,     0.0000, 0.00),
 ('SVC-FUEL-SCHG',   7751,'900','902','FUEL-SCHG',    'FUEL SURCHARGE',               'PER SHIPMENT',               'EA',   1,     1,     0.0000, 0.00),
]

# --- Customers (CUSTOMEP) and their ship-to locations (CUSTSHPP) -------------
# Modelled on the seven markets Pratt names: automotive, beverage, cold chain,
# ecommerce, agriculture, pizza and protein.
CUSTOMERS = [
 ('94', 1040, 'SOUTHERN HARVEST PRODUCE',   'AGRICULTURE'),
 ('94', 1185, 'PEACHTREE BEVERAGE CO',      'BEVERAGE'),
 ('94', 1320, 'NAPOLI PIZZA KITCHENS',      'PIZZA'),
 ('94', 1455, 'COLDSTREAM PROTEIN FOODS',   'PROTEIN'),
 ('94', 1610, 'SWIFTCART ECOMMERCE',        'ECOMMERCE'),
 ('93', 2075, 'LAKESHORE AUTO COMPONENTS',  'AUTOMOTIVE'),
 ('93', 2240, 'DUNE RIDGE COLD CHAIN',      'COLD CHAIN'),
 ('95', 3115, 'BAYOU FRESH FARMS',          'AGRICULTURE'),
 ('96', 4080, 'BUCKEYE BOTTLING WORKS',     'BEVERAGE'),
 ('97', 5025, 'OHIO VALLEY MEATS',          'PROTEIN'),
 ('98', 6010, 'HARBOR POINT GROCERS',       'COLD CHAIN'),
 ('99', 3915, 'STOROPACK DISPLAY PARTNERS', 'RETAIL DISPLAY'),
]

SHIPTOS = [
 # plant, custid, loc, name,                         street,                 city,          st, zip5, zip4, axis, rel wh
 ('94', 1040,'001','SOUTHERN HARVEST - FORSYTH',  '1200 Peach Orchard Rd','FORSYTH',     'GA',31029,1420,'SE1','CO'),
 ('94', 1185,'001','PEACHTREE BEVERAGE - DC1',    '455 Bottling Way',     'COLLEGE PARK','GA',30349,2210,'SE1','CO'),
 ('94', 1320,'001','NAPOLI PIZZA - CENTRAL',      '88 Commerce Circle',   'MARIETTA',    'GA',30060,1105,'SE1','CO'),
 ('94', 1320,'002','NAPOLI PIZZA - NORTH',        '2140 Thornton Road',   'LITHIA SPRINGS','GA',30122,3310,'SE1','CO'),
 ('94', 1455,'001','COLDSTREAM - GAINESVILLE',    '700 Poultry Park Dr',  'GAINESVILLE', 'GA',30501,4420,'SE2','CO'),
 ('94', 1610,'001','SWIFTCART FC ATL1',           '5500 Fulfillment Pkwy','UNION CITY',  'GA',30291,1180,'SE2','CO'),
 ('93', 2075,'001','LAKESHORE AUTO - PLANT 2',    '900 Industrial Drive', 'PORTAGE',     'IN',46368,2240,'MW1','VA'),
 ('93', 2240,'001','DUNE RIDGE COLD - DC',        '3300 Lakefront Blvd',  'MICHIGAN CITY','IN',46360,1150,'MW1','VA'),
 ('95', 3115,'001','BAYOU FRESH - PACKHOUSE',     '1750 Delta Road',      'BOSSIER CITY','LA',71111,3320,'SC1','SH'),
 ('96', 4080,'001','BUCKEYE BOTTLING - MAIN',     '450 Glass Plant Road', 'LIMA',        'OH',45804,1170,'MW2','WA'),
 ('97', 5025,'001','OHIO VALLEY MEATS - COLD',    '2200 River Port Drive','OWENSBORO',   'KY',42301,2260,'MW2','HE'),
 ('98', 6010,'001','HARBOR POINT - BROOKLYN DC',  '1100 Dockside Avenue', 'BROOKLYN',    'NY',11232,1140,'NE1','SI'),
 ('99', 3915,'ATL','STOROPACK DISPLAY - ATLANTA', '2050 Westside Parkway','ALPHARETTA',  'GA',30004,1190,'SE1','AT'),
]

# --- Sort drop-down content for the HELP table ------------------------------
SORTS = {
 'POSELUI': [
   ('Last Change Date','OHCHDT'), ('PO Number','OHYY, OH#'), ('Vendor','OHVEND'),
   ('Status','OHSTAT'), ('Type','OHCODE'), ('Warehouse','OHDC'),
   ('Required Date','OHRDTE'), ('Created By','OHREQ'),
 ],
 'PMSELUI': [
   ('Created Date','OMCDAT'), ('Requisition Number','OMYY, OMREQ'), ('Vendor','OMVN#'),
   ('Status','OMSTAT'), ('Type','OMCODE'), ('Warehouse','OMDC'),
   ('Required Date','OMRQDT'), ('Requested By','OMBY'), ('Part Number','OMPART'),
 ],
}
