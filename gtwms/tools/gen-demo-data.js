#!/usr/bin/env node
/*
 * GT Warehouse Mobile -- demo data generator
 *
 *   node gtwms/tools/gen-demo-data.js
 *
 * Writes gtwms/qsqlsrc/gtseed.table.sql. That file is committed so the build
 * never depends on node being installed; regenerate it by editing this script,
 * never by editing the .sql.
 *
 * Two things here are deliberate rather than incidental:
 *
 *   1. Every barcode carries a REAL GS1 mod-10 check digit. A scanner rejects
 *      an invalid one, and on a live demo that failure happens in front of the
 *      audience. UPC-A, EAN-13, ITF-14 and SSCC-18 all go through gs1Check().
 *
 *   2. French descriptions come from a curated bilingual lexicon, composed the
 *      same way on both sides. Product names are exactly where machine
 *      translation reads worst, and composing from a fixed vocabulary gives
 *      genuinely idiomatic Canadian French for free.
 *
 * Output is deterministic -- a seeded PRNG, no Date.now() -- so the demo data
 * is identical on every regeneration and the scripted demo path stays valid.
 */

'use strict';

const fs = require('fs');
const path = require('path');

/* ------------------------------------------------------------------ *
 * Deterministic PRNG (mulberry32)
 * ------------------------------------------------------------------ */
function makeRng(seed) {
  let a = seed >>> 0;
  return function rng() {
    a = (a + 0x6d2b79f5) >>> 0;
    let t = a;
    t = Math.imul(t ^ (t >>> 15), t | 1);
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}
const rng = makeRng(20260806);
const randInt = (lo, hi) => lo + Math.floor(rng() * (hi - lo + 1));
const pick = (arr) => arr[Math.floor(rng() * arr.length)];

/* ------------------------------------------------------------------ *
 * GS1 mod-10 check digit.
 * Weights alternate 3,1 starting from the RIGHTMOST payload digit, which is
 * the same rule for UPC-A (11), EAN-13 (12), ITF-14 (13) and SSCC-18 (17).
 * ------------------------------------------------------------------ */
function gs1Check(payload) {
  let sum = 0;
  for (let i = 0; i < payload.length; i++) {
    const d = Number(payload[payload.length - 1 - i]);
    sum += d * (i % 2 === 0 ? 3 : 1);
  }
  return String((10 - (sum % 10)) % 10);
}
const withCheck = (payload) => payload + gs1Check(payload);

/* Self-test against a known-good UPC-A so a broken edit fails loudly. */
if (withCheck('03600029145') !== '036000291452') {
  throw new Error('gs1Check is wrong -- known UPC-A 036000291452 did not round-trip');
}

const GS1_PREFIX = '0614141';           // demo company prefix

/* ------------------------------------------------------------------ *
 * SQL helpers
 * ------------------------------------------------------------------ */
const q = (s) => "'" + String(s).replace(/'/g, "''") + "'";
const out = [];
const say = (s) => out.push(s);

/* Emits a multi-row INSERT in batches so RUNSQLSTM is not fed 4,000
 * single-row statements. */
function insertRows(table, columns, rows, batchSize = 40) {
  for (let i = 0; i < rows.length; i += batchSize) {
    const chunk = rows.slice(i, i + batchSize);
    say(`INSERT INTO ${table} (${columns.join(', ')}) VALUES`);
    say(chunk.map((r) => '    (' + r.join(', ') + ')').join(',\n') + ';');
    say('');
  }
}

/* ------------------------------------------------------------------ *
 * Bilingual lexicon
 * ------------------------------------------------------------------ */
const DEPARTMENTS = [
  { code: 'GROC', en: 'Grocery',          fr: 'Épicerie' },
  { code: 'CLEA', en: 'Cleaning',         fr: 'Nettoyage' },
  { code: 'HLTH', en: 'Health & Beauty',  fr: 'Santé et beauté' },
  { code: 'HOME', en: 'Home',             fr: 'Maison' },
  { code: 'APPL', en: 'Apparel',          fr: 'Vêtements' },
  { code: 'SEAS', en: 'Seasonal',         fr: 'Saisonnier' },
  { code: 'PETS', en: 'Pet Supplies',     fr: 'Animalerie' },
  { code: 'TOYS', en: 'Toys',             fr: 'Jouets' },
];

/* Variant axes, each side written by hand. */
const V_WEIGHT = [
  { en: '250 g', fr: '250 g' },   { en: '500 g', fr: '500 g' },
  { en: '750 g', fr: '750 g' },   { en: '1 kg', fr: '1 kg' },
  { en: '1.5 kg', fr: '1,5 kg' }, { en: '2 kg', fr: '2 kg' },
  { en: '3 kg', fr: '3 kg' },     { en: '4 kg', fr: '4 kg' },
];
const V_VOLUME = [
  { en: '250 mL', fr: '250 ml' },  { en: '500 mL', fr: '500 ml' },
  { en: '750 mL', fr: '750 ml' },  { en: '1 L', fr: '1 L' },
  { en: '1.5 L', fr: '1,5 L' },    { en: '2 L', fr: '2 L' },
  { en: '3 L', fr: '3 L' },        { en: '4 L', fr: '4 L' },
];
const V_COUNT = [
  { en: '2 Pack', fr: 'Paquet de 2' },   { en: '4 Pack', fr: 'Paquet de 4' },
  { en: '6 Pack', fr: 'Paquet de 6' },   { en: '8 Pack', fr: 'Paquet de 8' },
  { en: '12 Pack', fr: 'Paquet de 12' }, { en: '18 Pack', fr: 'Paquet de 18' },
  { en: '24 Pack', fr: 'Paquet de 24' }, { en: '48 Pack', fr: 'Paquet de 48' },
];
const V_SIZE = [
  { en: 'X-Small', fr: 'Très petit' },   { en: 'Small', fr: 'Petit' },
  { en: 'Medium', fr: 'Moyen' },         { en: 'Large', fr: 'Grand' },
  { en: 'X-Large', fr: 'Très grand' },   { en: '2X-Large', fr: 'Très grand 2X' },
  { en: '3X-Large', fr: 'Très grand 3X' }, { en: 'One Size', fr: 'Taille unique' },
];
const V_COLOUR = [
  { en: 'Black', fr: 'Noir' },   { en: 'Navy', fr: 'Marine' },
  { en: 'Red', fr: 'Rouge' },    { en: 'Grey', fr: 'Gris' },
  { en: 'White', fr: 'Blanc' },  { en: 'Forest Green', fr: 'Vert forêt' },
  { en: 'Burgundy', fr: 'Bordeaux' }, { en: 'Cream', fr: 'Crème' },
];
const V_SCENT = [
  { en: 'Fresh Linen', fr: 'Lin frais' },     { en: 'Lavender', fr: 'Lavande' },
  { en: 'Citrus', fr: 'Agrumes' },            { en: 'Unscented', fr: 'Sans parfum' },
  { en: 'Ocean Breeze', fr: 'Brise marine' }, { en: 'Vanilla', fr: 'Vanille' },
  { en: 'Eucalyptus', fr: 'Eucalyptus' },     { en: 'Spring Rain', fr: 'Pluie printanière' },
];

/* family: [department, english, french, variant axis, uom, casePack, price lo, price hi, kg lo, kg hi] */
const FAMILIES = [
  ['GROC', 'Long Grain White Rice',      'Riz blanc à grain long',              V_WEIGHT, 'EA',  12,  2.99, 12.99, 0.5, 4.0],
  ['GROC', 'Whole Wheat Pasta',          'Pâtes de blé entier',                 V_WEIGHT, 'EA',  12,  1.49,  5.99, 0.5, 2.0],
  ['GROC', 'Canned Diced Tomatoes',      'Tomates en dés en conserve',          V_COUNT,  'EA',  24,  0.99,  8.99, 0.4, 3.2],
  ['GROC', 'Ground Coffee',              'Café moulu',                          V_WEIGHT, 'EA',   6,  6.99, 21.99, 0.5, 2.0],
  ['GROC', 'Breakfast Cereal',           'Céréales à déjeuner',                 V_WEIGHT, 'EA',  10,  3.49, 11.99, 0.5, 2.0],
  ['GROC', 'Vegetable Oil',              'Huile végétale',                      V_VOLUME, 'EA',  12,  3.99, 14.99, 0.5, 4.0],
  ["GROC", 'Peanut Butter',              "Beurre d'arachide",                   V_WEIGHT, 'EA',  12,  3.29, 10.99, 0.5, 2.0],
  ['GROC', 'Granola Bars',               'Barres granola',                      V_COUNT,  'EA',  16,  2.49,  9.99, 0.2, 1.4],

  ['CLEA', 'Liquid Laundry Detergent',   'Détergent à lessive liquide',         V_VOLUME, 'EA',   6,  5.99, 19.99, 0.6, 4.5],
  ['CLEA', 'All-Purpose Cleaner',        'Nettoyant tout usage',                V_SCENT,  'EA',  12,  2.99,  6.49, 0.7, 1.1],
  ['CLEA', 'Dish Soap',                  'Savon à vaisselle',                   V_SCENT,  'EA',  12,  1.99,  4.99, 0.6, 1.0],
  ['CLEA', 'Paper Towels',               'Essuie-tout',                         V_COUNT,  'EA',   4,  4.99, 19.99, 0.8, 5.0],
  ['CLEA', 'Bathroom Tissue',            'Papier hygiénique',                   V_COUNT,  'EA',   4,  5.99, 24.99, 0.9, 6.0],
  ['CLEA', 'Trash Bags',                 'Sacs à ordures',                      V_COUNT,  'EA',  10,  3.99, 13.99, 0.4, 2.2],
  ['CLEA', 'Disinfecting Wipes',         'Lingettes désinfectantes',            V_COUNT,  'EA',  12,  3.49,  9.99, 0.3, 1.5],

  ['HLTH', 'Shampoo',                    'Shampooing',                          V_SCENT,  'EA',  12,  2.99,  7.99, 0.4, 0.8],
  ['HLTH', 'Bar Soap',                   'Savon en barre',                      V_COUNT,  'EA',  24,  1.99,  6.99, 0.2, 1.0],
  ['HLTH', 'Toothpaste',                 'Dentifrice',                          V_COUNT,  'EA',  24,  1.79,  6.49, 0.1, 0.6],
  ['HLTH', 'Body Lotion',                'Lotion pour le corps',                V_SCENT,  'EA',  12,  3.49,  9.49, 0.3, 0.7],
  ['HLTH', 'Deodorant',                  'Déodorant',                           V_SCENT,  'EA',  24,  2.49,  6.99, 0.1, 0.3],
  ['HLTH', 'Facial Tissue',              'Papiers-mouchoirs',                   V_COUNT,  'EA',  12,  1.49,  7.99, 0.2, 1.2],

  ['HOME', 'Bath Towel',                 'Serviette de bain',                   V_COLOUR, 'EA',   6,  6.99, 16.99, 0.4, 0.9],
  ['HOME', 'Bed Sheet Set',              'Ensemble de draps',                   V_COLOUR, 'EA',   4, 19.99, 49.99, 1.2, 2.6],
  ['HOME', 'Storage Bin',                'Bac de rangement',                    V_SIZE,   'EA',   8,  4.99, 19.99, 0.5, 2.4],
  ['HOME', 'Throw Pillow',               'Coussin décoratif',                   V_COLOUR, 'EA',   8,  7.99, 17.99, 0.3, 0.8],
  ['HOME', 'Picture Frame',              'Cadre photo',                         V_SIZE,   'EA',  12,  3.99, 14.99, 0.2, 1.1],
  ['HOME', 'Kitchen Utensil Set',        "Ensemble d'ustensiles de cuisine",    V_COUNT,  'EA',   8,  9.99, 24.99, 0.4, 1.6],
  ['HOME', 'Ceramic Mug',                'Tasse en céramique',                  V_COLOUR, 'EA',  24,  2.99,  7.99, 0.3, 0.5],

  ['APPL', "Men's Crew Neck T-Shirt",    'T-shirt à col rond pour homme',       V_SIZE,   'EA',  24,  6.99, 14.99, 0.15, 0.3],
  ['APPL', 'Ladies Fleece Hoodie',       'Chandail à capuchon pour femme',      V_SIZE,   'EA',  12, 16.99, 34.99, 0.4, 0.8],
  ['APPL', 'Kids Denim Jeans',           'Jeans en denim pour enfant',          V_SIZE,   'EA',  12, 12.99, 24.99, 0.3, 0.6],
  ['APPL', 'Athletic Socks',             'Chaussettes de sport',                V_COUNT,  'EA',  24,  4.99, 14.99, 0.1, 0.4],
  ['APPL', 'Winter Gloves',              "Gants d'hiver",                       V_SIZE,   'EA',  24,  7.99, 19.99, 0.1, 0.3],
  ['APPL', 'Baseball Cap',               'Casquette de baseball',               V_COLOUR, 'EA',  24,  8.99, 16.99, 0.1, 0.2],

  ['SEAS', 'Patio Chair Cushion',        'Coussin de chaise de patio',          V_COLOUR, 'EA',   6, 12.99, 29.99, 0.5, 1.2],
  ['SEAS', 'String Lights',              'Guirlande lumineuse',                 V_COUNT,  'EA',  12,  9.99, 24.99, 0.3, 0.9],
  ['SEAS', 'Garden Hose',                "Boyau d'arrosage",                    V_SIZE,   'EA',   4, 14.99, 39.99, 1.8, 4.5],
  ['SEAS', 'Beach Towel',                'Serviette de plage',                  V_COLOUR, 'EA',   8,  8.99, 18.99, 0.4, 0.7],
  ['SEAS', 'Artificial Wreath',          'Couronne artificielle',               V_SIZE,   'EA',   6, 11.99, 34.99, 0.4, 1.5],
  ['SEAS', 'Snow Shovel',                'Pelle à neige',                       V_SIZE,   'EA',   6, 12.99, 27.99, 0.9, 2.2],

  ['PETS', 'Dry Dog Food',               'Nourriture sèche pour chien',         V_WEIGHT, 'EA',   4,  9.99, 39.99, 2.0, 16.0],
  ['PETS', 'Cat Litter',                 'Litière pour chat',                   V_WEIGHT, 'EA',   4,  6.99, 24.99, 4.0, 18.0],
  ['PETS', 'Pet Bed',                    'Lit pour animal',                     V_SIZE,   'EA',   4, 14.99, 44.99, 0.8, 3.2],
  ['PETS', 'Chew Toy',                   'Jouet à mâcher',                      V_SIZE,   'EA',  24,  3.99, 11.99, 0.1, 0.5],
  ['PETS', 'Dog Leash',                  'Laisse pour chien',                   V_COLOUR, 'EA',  12,  7.99, 18.99, 0.2, 0.4],

  ['TOYS', 'Building Block Set',         'Ensemble de blocs de construction',   V_COUNT,  'EA',   8, 12.99, 39.99, 0.5, 2.4],
  ['TOYS', 'Plush Teddy Bear',           'Ourson en peluche',                   V_SIZE,   'EA',  12,  8.99, 26.99, 0.2, 0.9],
  ['TOYS', 'Puzzle 500 Pieces',          'Casse-tête 500 morceaux',             V_COUNT,  'EA',  12,  6.99, 16.99, 0.5, 0.9],
  ['TOYS', 'Toy Car Pack',               'Ensemble de petites voitures',        V_COUNT,  'EA',  24,  4.99, 14.99, 0.2, 0.8],
  ['TOYS', 'Craft Kit',                  'Trousse de bricolage',                V_SIZE,   'EA',  12,  7.99, 19.99, 0.3, 1.1],
  ['TOYS', 'Board Game',                 'Jeu de société',                      V_SIZE,   'EA',   8, 11.99, 29.99, 0.6, 1.4],
];

/* ------------------------------------------------------------------ *
 * Reason codes
 * ------------------------------------------------------------------ */
const REASONS = [
  ['DMGC', 'RCPT', 'Damaged carton',            'Carton endommagé',              'Y', 'N',  10],
  ['DMGP', 'RCPT', 'Damaged product',           'Produit endommagé',             'Y', 'N',  20],
  ['DMGW', 'RCPT', 'Water damage',              "Dégât d'eau",                   'Y', 'N',  30],
  ['SHRT', 'RCPT', 'Short shipped',             'Quantité manquante',            'N', 'N',  40],
  ['OVER', 'RCPT', 'Over shipped',              'Quantité excédentaire',         'N', 'Y',  50],
  ['WRNG', 'RCPT', 'Wrong item shipped',        'Mauvais article expédié',       'Y', 'Y',  60],
  ['EXPD', 'RCPT', 'Expired or short-dated',    'Périmé ou date trop courte',    'Y', 'Y',  70],
  ['MISS', 'RCPT', 'Missing from pallet',       'Absent de la palette',          'N', 'N',  80],
  ['REFU', 'RCPT', 'Refused at door',           'Refusé à la porte',             'Y', 'Y',  90],

  ['REPL', 'MOVE', 'Replenishment',             'Réapprovisionnement',           'N', 'N',  10],
  ['PUTA', 'MOVE', 'Putaway from staging',      'Rangement depuis la zone',      'N', 'N',  20],
  ['CONS', 'MOVE', 'Consolidation',             'Consolidation',                 'N', 'N',  30],
  ['RELO', 'MOVE', 'Location change',           "Changement d'emplacement",      'N', 'N',  40],
  ['QUAR', 'MOVE', 'Move to quarantine',        'Mise en quarantaine',           'Y', 'Y',  50],
  ['DMGH', 'MOVE', 'Move to damage hold',       'Transfert aux avaries',         'Y', 'N',  60],
  ['RTRN', 'MOVE', 'Customer return',           'Retour de client',              'N', 'N',  70],

  ['CVAR', 'CNT',  'Count variance',            'Écart de dénombrement',         'Y', 'Y',  10],
  ['CMIS', 'CNT',  'Stock not found',           'Stock introuvable',             'Y', 'Y',  20],
  ['CEXT', 'CNT',  'Extra stock found',         'Stock excédentaire trouvé',     'Y', 'Y',  30],
  ['CLBL', 'CNT',  'Label unreadable',          'Étiquette illisible',           'Y', 'N',  40],
  ['CMIX', 'CNT',  'Mixed stock in location',   'Stock mélangé dans un endroit', 'Y', 'Y',  50],
  ['CREC', 'CNT',  'Recount required',          'Nouveau dénombrement requis',   'N', 'N',  60],
  ['COK',  'CNT',  'Count matches',             'Dénombrement conforme',         'N', 'N',  70],

  ['ADJU', 'ADJ',  'Inventory adjustment up',   "Ajustement d'inventaire (+)",   'N', 'Y',  10],
  ['ADJD', 'ADJ',  'Inventory adjustment down', "Ajustement d'inventaire (-)",   'Y', 'Y',  20],
];

/* ------------------------------------------------------------------ *
 * Operators. Split EN/FR so the demo can badge in as a French operator.
 * Badge barcodes are Code 39, which is what employee badges usually carry.
 * ------------------------------------------------------------------ */
const OPERATORS = [
  ['GT001', 'GT-1001', 'Marie Tremblay',     'FR', 'A',   'N', 'OPER'],
  ['GT002', 'GT-1002', 'James Okonkwo',      'EN', 'A',   'N', 'OPER'],
  ['GT003', 'GT-1003', 'Priya Raghavan',     'EN', 'B',   'N', 'OPER'],
  ['GT004', 'GT-1004', 'Luc Bergeron',       'FR', 'B',   'Y', 'OPER'],
  ['GT005', 'GT-1005', 'Sarah Whitecloud',   'EN', 'C',   'N', 'OPER'],
  ['GT006', 'GT-1006', 'Mohamed El-Amrani',  'FR', 'C',   'N', 'OPER'],
  ['GT007', 'GT-1007', 'Chen Wei',           'EN', 'D',   'N', 'OPER'],
  ['GT008', 'GT-1008', 'Josée Lafleur',      'FR', 'D',   'Y', 'OPER'],
  ['GT009', 'GT-1009', 'Daniel Osei',        'EN', 'E',   'N', 'SUPV'],
  ['GT010', 'GT-1010', 'Isabelle Gagnon',    'FR', 'E',   'N', 'SUPV'],
];

const VENDORS = [
  ['VND0001', 'Northbridge Wholesale Ltd.'],
  ['VND0002', 'Laurentide Distribution inc.'],
  ['VND0003', 'Prairie Sun Foods'],
  ['VND0004', 'Maritime Home Goods'],
  ['VND0005', 'Cascade Textiles Co.'],
  ['VND0006', 'Groupe Beauport'],
];

/* ================================================================== *
 * Build the data
 * ================================================================== */

/* ---- Items and barcodes ---- */
const items = [];
const barcodes = [];
let itemSeq = 0;

for (const [dept, famEn, famFr, axis, uom, casePack, priceLo, priceHi, kgLo, kgHi] of FAMILIES) {
  for (const variant of axis) {
    itemSeq += 1;
    const sku = `${dept}-${String(itemSeq).padStart(6, '0')}`;
    const descEn = `${famEn} - ${variant.en}`;
    const descFr = `${famFr} - ${variant.fr}`;

    const price = (priceLo + rng() * (priceHi - priceLo)).toFixed(2);
    const kg = (kgLo + rng() * (kgHi - kgLo)).toFixed(3);

    items.push({
      sku, descEn, descFr, dept, uom, casePack,
      kg, price,
      len: (randInt(80, 450) / 10).toFixed(1),
      wid: (randInt(60, 350) / 10).toFixed(1),
      hgt: (randInt(40, 300) / 10).toFixed(1),
    });

    /* Consumer unit -- UPC-A. 11 payload digits + check. */
    const upc = withCheck(GS1_PREFIX + String(itemSeq).padStart(4, '0'));
    barcodes.push([upc, sku, 'UPC-A', 'EA', 1]);

    /* Case -- ITF-14. Indicator + 12 + check. */
    const itf = withCheck('1' + GS1_PREFIX + String(itemSeq).padStart(5, '0'));
    barcodes.push([itf, sku, 'ITF-14', 'CS', casePack]);

    /* Roughly a third also carry an EAN-13, as imported goods do. */
    if (itemSeq % 3 === 0) {
      const ean = withCheck(GS1_PREFIX + String(itemSeq).padStart(5, '0'));
      barcodes.push([ean, sku, 'EAN-13', 'EA', 1]);
    }
  }
}

/* ---- Locations ---- */
const locations = [];
const pickLocs = [];
const bulkLocs = [];

/* Pick faces: zones A, B, C. One SKU per face, as a real DC works. */
for (const zone of ['A', 'B', 'C']) {
  for (let aisle = 1; aisle <= 8; aisle++) {
    for (let bay = 1; bay <= 18; bay++) {
      const id = `${zone}${String(aisle).padStart(2, '0')}${String(bay).padStart(2, '0')}1`;
      const loc = {
        id, zone, type: 'PICK',
        aisle: String(aisle).padStart(2, '0'),
        bay: String(bay).padStart(2, '0'),
        level: '1',
        cap: randInt(60, 240),
        min: randInt(6, 24),
        max: randInt(48, 180),
      };
      locations.push(loc);
      pickLocs.push(loc);
    }
  }
}

/* Bulk racking: zones D and E, four levels. */
for (const zone of ['D', 'E']) {
  for (let aisle = 1; aisle <= 6; aisle++) {
    for (let bay = 1; bay <= 12; bay++) {
      for (let level = 1; level <= 4; level++) {
        const id = `${zone}${String(aisle).padStart(2, '0')}${String(bay).padStart(2, '0')}${level}`;
        const loc = {
          id, zone, type: 'BULK',
          aisle: String(aisle).padStart(2, '0'),
          bay: String(bay).padStart(2, '0'),
          level: String(level),
          cap: randInt(400, 1600), min: 0, max: 0,
        };
        locations.push(loc);
        bulkLocs.push(loc);
      }
    }
  }
}

/* Staging, returns and damage hold. */
for (let i = 1; i <= 12; i++) {
  locations.push({ id: `STAGE${String(i).padStart(2, '0')}`, zone: 'S', type: 'STAG',
                   aisle: '', bay: '', level: '', cap: 2000, min: 0, max: 0 });
}
for (let i = 1; i <= 6; i++) {
  locations.push({ id: `RETURN${String(i).padStart(2, '0')}`, zone: 'R', type: 'RETN',
                   aisle: '', bay: '', level: '', cap: 800, min: 0, max: 0 });
}
for (let i = 1; i <= 6; i++) {
  locations.push({ id: `DAMAGE${String(i).padStart(2, '0')}`, zone: 'X', type: 'DAMG',
                   aisle: '', bay: '', level: '', cap: 400, min: 0, max: 0 });
}

/* ---- Inventory balances ---- *
 * Each item gets one pick face plus one or two bulk pallets. A handful of
 * bulk locations are deliberately overloaded so that the "what is in this
 * location" screen has something long enough to scroll with a finger.
 */
const balances = [];
const balanceKeys = new Set();
const itemPickFace = new Map();

function addBalance(sku, locId, qty, alloc) {
  const key = `${sku}|${locId}`;
  if (balanceKeys.has(key)) return false;
  balanceKeys.add(key);
  balances.push([sku, locId, qty, alloc]);
  return true;
}

items.forEach((it, idx) => {
  /* Pick face -- deterministic pairing so every item has exactly one. */
  const face = pickLocs[idx % pickLocs.length];
  itemPickFace.set(it.sku, face);

  /* Roughly one pick face in eight is below its minimum, which is what
   * lights up the Replenish shortcut and the Supervisor alert list. */
  const low = idx % 8 === 3;
  const qty = low ? randInt(0, Math.max(1, face.min - 1)) : randInt(face.min + 4, face.max);
  addBalance(it.sku, face.id, qty, idx % 11 === 0 ? randInt(1, 6) : 0);

  /* Bulk pallets. */
  const bulkCount = randInt(1, 2);
  for (let b = 0; b < bulkCount; b++) {
    const loc = bulkLocs[(idx * 3 + b * 97) % bulkLocs.length];
    addBalance(it.sku, loc.id, randInt(48, 640), 0);
  }
});

/* Hero bulk locations: 24 items each, for the long-list demo. */
const heroLocs = [bulkLocs[0], bulkLocs[1], bulkLocs[2]];
heroLocs.forEach((loc, h) => {
  for (let i = 0; i < 24; i++) {
    const it = items[(h * 37 + i * 5) % items.length];
    addBalance(it.sku, loc.id, randInt(60, 900), 0);
  }
});

/* ---- Pallets ---- */
const pallets = [];
for (let i = 1; i <= 30; i++) {
  const sscc = withCheck('3' + GS1_PREFIX + String(i).padStart(9, '0'));
  pallets.push({
    sscc,
    status: i <= 8 ? 'OPEN' : 'CLSD',
    loc: i <= 8 ? `STAGE${String(((i - 1) % 12) + 1).padStart(2, '0')}` : '',
    vendor: VENDORS[i % VENDORS.length][0],
  });
}

/* ---- Receipts ---- *
 * Eight open receipts. Receipt 1 is the demo path and is deliberately the
 * long one -- 60 lines forces genuine touch scrolling on camera.
 */
const receipts = [];
const receiptLines = [];
const RECEIPT_SIZES = [60, 24, 18, 31, 12, 27, 9, 15];

RECEIPT_SIZES.forEach((lineCount, r) => {
  const receiptId = `RC${String(r + 1).padStart(8, '0')}`;
  const pallet = pallets[r];
  const vendor = VENDORS[r % VENDORS.length];

  receipts.push({
    receiptId,
    po: `PO${String(48210 + r * 17).padStart(10, '0')}`,
    vendorId: vendor[0],
    vendorName: vendor[1],
    sscc: pallet.sscc,
    door: `D${String((r % 8) + 1).padStart(3, '0')}`,
    status: 'OPEN',
    expectedOffsetHrs: r * 3,
  });

  const used = new Set();
  for (let l = 1; l <= lineCount; l++) {
    let it = items[(r * 53 + l * 7) % items.length];
    while (used.has(it.sku)) it = items[(items.indexOf(it) + 1) % items.length];
    used.add(it.sku);

    receiptLines.push({
      receiptId,
      lineNo: l,
      sku: it.sku,
      qtyExpected: randInt(2, 40) * it.casePack,
    });
  }
});

/* ---- Cycle counts ---- *
 * Assigned against the hero bulk locations so each count has enough lines
 * to scroll, plus one small pick-face count.
 */
const counts = [];
const countLines = [];
const countTargets = [heroLocs[0], heroLocs[1], heroLocs[2], pickLocs[5]];

countTargets.forEach((loc, c) => {
  const countId = `CN${String(c + 1).padStart(8, '0')}`;
  counts.push({
    countId,
    locationId: loc.id,
    status: 'OPEN',
    assignedTo: OPERATORS[c % 8][0],
  });

  const here = balances.filter((b) => b[1] === loc.id);
  here.forEach((b, i) => {
    countLines.push({
      countId,
      lineNo: i + 1,
      sku: b[0],
      qtyExpected: b[2],
    });
  });
});

/* ================================================================== *
 * Emit SQL
 * ================================================================== */
say('--  GT Warehouse Mobile -- demo data');
say('--');
say('--  GENERATED FILE -- do not edit.');
say('--  Produced by gtwms/tools/gen-demo-data.js. Edit that script and re-run it.');
say('--');
say('--  Every barcode below carries a real GS1 mod-10 check digit, so a live');
say('--  scanner accepts them. Descriptions are bilingual by construction.');
say('');
say('DELETE FROM GTIMAGE;');
say('DELETE FROM GTCOUNTDTL;');
say('DELETE FROM GTCOUNTHDR;');
say('DELETE FROM GTMOVEMENT;');
say('DELETE FROM GTRCPTDTL;');
say('DELETE FROM GTRCPTHDR;');
say('DELETE FROM GTPALLET;');
say('DELETE FROM GTINVBAL;');
say('DELETE FROM GTBARCODE;');
say('DELETE FROM GTLOCATION;');
say('DELETE FROM GTITEM;');
say('DELETE FROM GTOPERATOR;');
say('DELETE FROM GTREASON;');
say('');

insertRows('GTREASON',
  ['reason_code', 'reason_type', 'reason_desc_en', 'reason_desc_fr',
   'photo_required', 'needs_approval', 'sort_order'],
  REASONS.map((r) => [q(r[0]), q(r[1]), q(r[2]), q(r[3]), q(r[4]), q(r[5]), r[6]]));

insertRows('GTOPERATOR',
  ['operator_id', 'badge_barcode', 'full_name', 'lang_pref',
   'default_zone', 'large_touch', 'operator_role'],
  OPERATORS.map((o) => [q(o[0]), q(o[1]), q(o[2]), q(o[3]), q(o[4]), q(o[5]), q(o[6])]));

insertRows('GTITEM',
  ['sku', 'description_en', 'description_fr', 'department', 'uom', 'case_pack',
   'weight_kg', 'length_cm', 'width_cm', 'height_cm', 'retail_price', 'item_status'],
  items.map((i) => [q(i.sku), q(i.descEn), q(i.descFr), q(i.dept), q(i.uom),
                    i.casePack, i.kg, i.len, i.wid, i.hgt, i.price, q('A')]));

insertRows('GTLOCATION',
  ['location_id', 'zone', 'location_type', 'aisle', 'bay', 'level_id',
   'capacity_units', 'min_qty', 'max_qty', 'active_flag'],
  locations.map((l) => [q(l.id), q(l.zone), q(l.type), q(l.aisle), q(l.bay),
                        q(l.level), l.cap, l.min, l.max, q('Y')]));

insertRows('GTBARCODE',
  ['barcode', 'sku', 'symbology', 'barcode_level', 'pack_qty'],
  barcodes.map((b) => [q(b[0]), q(b[1]), q(b[2]), q(b[3]), b[4]]));

insertRows('GTINVBAL',
  ['sku', 'location_id', 'qty_on_hand', 'qty_allocated', 'stock_status'],
  balances.map((b) => [q(b[0]), q(b[1]), b[2], b[3], q('OK')]));

insertRows('GTPALLET',
  ['sscc', 'pallet_status', 'location_id', 'vendor_id'],
  pallets.map((p) => [q(p.sscc), q(p.status), q(p.loc), q(p.vendor)]));

insertRows('GTRCPTHDR',
  ['receipt_id', 'po_number', 'vendor_id', 'vendor_name', 'sscc', 'door_id',
   'receipt_status', 'expected_ts'],
  receipts.map((r) => [q(r.receiptId), q(r.po), q(r.vendorId), q(r.vendorName),
                       q(r.sscc), q(r.door), q(r.status),
                       `CURRENT TIMESTAMP - ${r.expectedOffsetHrs} HOURS`]));

insertRows('GTRCPTDTL',
  ['receipt_id', 'line_no', 'sku', 'qty_expected', 'qty_received', 'line_status'],
  receiptLines.map((l) => [q(l.receiptId), l.lineNo, q(l.sku), l.qtyExpected, 0, q('OPEN')]));

insertRows('GTCOUNTHDR',
  ['count_id', 'location_id', 'count_status', 'assigned_to'],
  counts.map((c) => [q(c.countId), q(c.locationId), q(c.status), q(c.assignedTo)]));

insertRows('GTCOUNTDTL',
  ['count_id', 'line_no', 'sku', 'qty_expected', 'qty_counted', 'counted_flag'],
  countLines.map((l) => [q(l.countId), l.lineNo, q(l.sku), l.qtyExpected, 0, q('N')]));

/* ------------------------------------------------------------------ */
const target = path.join(__dirname, '..', 'qsqlsrc', 'gtseed.table.sql');
fs.writeFileSync(target, out.join('\n') + '\n', 'utf8');

console.log('Wrote ' + target);
console.log(`  reason codes ....... ${REASONS.length}`);
console.log(`  operators .......... ${OPERATORS.length}`);
console.log(`  items .............. ${items.length}`);
console.log(`  barcodes ........... ${barcodes.length}`);
console.log(`  locations .......... ${locations.length}  (pick ${pickLocs.length}, bulk ${bulkLocs.length})`);
console.log(`  inventory balances . ${balances.length}`);
console.log(`  pallets ............ ${pallets.length}`);
console.log(`  receipts ........... ${receipts.length}  (lines ${receiptLines.length}, largest ${Math.max(...RECEIPT_SIZES)})`);
console.log(`  cycle counts ....... ${counts.length}  (lines ${countLines.length})`);

/* A demo dies on details, so assert the things that would embarrass us. */
const bad = barcodes.filter((b) => withCheck(b[0].slice(0, -1)) !== b[0]);
if (bad.length) throw new Error(`${bad.length} barcodes have an invalid check digit`);
const badSscc = pallets.filter((p) => withCheck(p.sscc.slice(0, -1)) !== p.sscc);
if (badSscc.length) throw new Error(`${badSscc.length} SSCCs have an invalid check digit`);
const dupBarcode = barcodes.length !== new Set(barcodes.map((b) => b[0])).size;
if (dupBarcode) throw new Error('duplicate barcode generated -- primary key would fail');
console.log('  check digits ....... all valid, no duplicate barcodes');
