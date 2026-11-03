#!/usr/bin/env python3
"""Generate prfdemolib/data/seed-demo-items.sql - extra demo data that sits on
top of the sample rows copied from PRFDEMOLIB.

Why: the shipped sample is deep in one department (12 LADIES INTIMATE BASICS,
145 SKUs) and has one SKU in each of 17 others, and the inventory-group tables
are empty, so INV046 "Search For: I" can never succeed. This seed adds:

  * dept 86 SEASONAL CANDY  - 12 Christmas / Harvest SKUs (Roses "Seasonal")
  * dept 59 BEDDING         - 12 sheet / comforter / throw SKUs (Roses "Home")
  * 5 new vendors, 10 new sub-department / class rows
  * pack-and-hold rows for six holiday SKUs (release date 12/01/26)
  * 4 inventory groups (by state) over the 10 sample stores

Products are modelled on what Roses sells (vwstores.com / rosesdiscountstores
.com departments: Seasonal, Home, Food & Beverage; Fall/Holiday 2026 banners).
All seeded SKUs are 8801nn (candy) and 8802nn (bedding) and carry
style number DEMOSEED, so they are easy to find and remove.

How the rows are built: every table row is cloned from a real "shape" SKU with
stock at both distribution centres, last-year history and replenishment
profiles (849528 LADIES ASSORTED PANTIES), or from the existing dept 86/59 SKU for the item
master (503589, 104027). Identity, hierarchy, vendor, pack, cost and price
columns are overridden; unit columns are scaled per SKU (and varied per store),
money columns are scaled by the same factor times the price ratio. Column
metadata comes from the reconstructed DDS in qddssrc, so the SQL always
matches the files.

The output uses {LIB} for the target library; load-prfdemo-data.sh fills it in.

Usage:  python3 prfdemolib/tools/gen-seed-data.py > prfdemolib/data/seed-demo-items.sql
"""
import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
DDS = os.path.join(HERE, '..', 'qddssrc')

# ---------------------------------------------------------------------------
# Seed specification
# ---------------------------------------------------------------------------
NEW_VENDORS = [  # (number, name) - cloned from APSUPP 57353 PIEDMONT CANDY CO.
    (22850, 'FERRARA CANDY COMPANY'),
    (68120, 'SPANGLER CANDY COMPANY'),
    (60215, 'PEM AMERICA INC'),
    (59780, 'THE NORTHWEST COMPANY LLC'),
    (31405, 'HOME EXPRESSIONS TEXTILES'),
]

# (dept, subdept, class, name, short) - cloned from an existing row of the dept
NEW_HIERARCHY = [
    (86, 4, 20, 'CANDY CANES', 'CANES'),
    (86, 4, 21, 'CHOCOLATE GIFTS', 'CHOCGIFT'),
    (86, 3, 0, 'HARVEST', 'HARVEST'),
    (86, 3, 30, 'FALL CANDY', 'FALLCNDY'),
    (59, 1, 102, 'SHEET SETS', 'SHTSETS'),
    (59, 2, 0, 'COMFORTERS', 'COMFORT'),
    (59, 2, 201, 'COMFORTER SETS', 'CMFSETS'),
    (59, 3, 0, 'THROWS & BLANKETS', 'THROWS'),
    (59, 3, 301, 'THROWS', 'THROWS'),
    (59, 3, 302, 'BLANKETS', 'BLANKETS'),
]

# sku, description (30), dept, sub, class, vendor, vendor part#, inner pack,
# case pack, cost, retail, unit factor, season, ticketed ('3' = yes, '1' = no;
# TS0110 treats ticket types 1, 3 and 4 as ticketed;
# becomes ticket type 3 tear ticket for candy, 1 hang ticket for bedding, 0
# none), mapped (replenishment profile, shown as Map=Y in TS0110), shape SKU,
# pack-and-hold units (0 = none)
ITEMS = [
    # ---- 86 SEASONAL CANDY / 4 CHRISTMAS -------------------------------
    (880101, 'HOLIDAY SUGAR COOKIES 10OZ', 86, 4, 19, 23296, 'AB-HSC10', 12, 12, 1.05, 2.00, 1.40, '7', '3', True, 849528, 0),
    (880102, 'GINGERBREAD COOKIE TIN 12OZ', 86, 4, 19, 23296, 'AB-GBT12', 12, 12, 2.10, 4.00, 0.70, '7', '3', True, 849528, 0),
    (880103, 'PEPPERMINT CANDY CANES 12CT', 86, 4, 20, 68120, 'SP-PCC12', 24, 24, 0.55, 1.25, 2.20, '7', '3', True, 849528, 7200),
    (880104, 'MINI CANDY CANES 24CT', 86, 4, 20, 68120, 'SP-MCC24', 24, 24, 0.80, 1.50, 1.60, '7', '3', True, 849528, 0),
    (880105, 'FRUIT FLAVOR CANDY CANES 12CT', 86, 4, 20, 68120, 'SP-FCC12', 24, 24, 0.60, 1.25, 1.10, '7', '1', False, 849528, 0),
    (880106, 'MILK CHOCOLATE SANTA 3OZ', 86, 4, 21, 22850, 'FC-MCS3', 18, 36, 0.75, 1.50, 1.30, '7', '3', True, 849528, 4320),
    (880107, 'ASST CHOCOLATE GIFT BOX 7OZ', 86, 4, 21, 22850, 'FC-GBX7', 12, 12, 2.40, 5.00, 0.60, '7', '3', True, 849528, 2880),
    (880108, 'PEPPERMINT BARK 8OZ', 86, 4, 21, 57353, 'PC-PB8', 12, 24, 1.60, 3.00, 0.90, '7', '3', True, 849528, 0),
    (880109, 'CHOC COVERED PRETZELS 8OZ', 86, 4, 21, 57353, 'PC-CCP8', 12, 24, 1.35, 2.50, 0.80, '7', '1', False, 849528, 0),
    # ---- 86 SEASONAL CANDY / 3 HARVEST ---------------------------------
    (880110, 'CANDY CORN 11OZ', 86, 3, 30, 22850, 'FC-CC11', 12, 24, 0.95, 2.00, 1.20, '7', '3', True, 849528, 0),
    (880111, 'AUTUMN MIX CANDY 11OZ', 86, 3, 30, 22850, 'FC-AM11', 12, 24, 0.95, 2.00, 0.90, '7', '3', True, 849528, 0),
    (880112, 'CARAMEL APPLE POPS 12CT', 86, 3, 30, 57353, 'PC-CAP12', 12, 12, 1.10, 2.25, 0.50, '7', '3', False, 849528, 0),
    # ---- 59 BEDDING / 1 SHEETS -----------------------------------------
    (880201, '2PK MICROFIBER PILLOWCASE STD', 59, 1, 101, 71188, 'SL-MPC2S', 12, 24, 1.60, 3.00, 0.90, '0', '3', True, 849528, 0),
    (880202, 'MICROFIBER SHEET SET TWIN', 59, 1, 102, 31405, 'HE-MSS-T', 6, 12, 4.50, 9.00, 0.25, '0', '3', True, 849528, 0),
    (880203, 'MICROFIBER SHEET SET FULL', 59, 1, 102, 31405, 'HE-MSS-F', 6, 12, 5.25, 10.00, 0.2, '0', '3', True, 849528, 0),
    (880204, 'MICROFIBER SHEET SET QUEEN', 59, 1, 102, 31405, 'HE-MSS-Q', 6, 12, 6.00, 12.00, 0.3, '0', '3', True, 849528, 0),
    (880205, 'FLANNEL SHEET SET QUEEN', 59, 1, 102, 60215, 'PA-FSS-Q', 4, 8, 9.50, 18.00, 0.15, '7', '3', True, 849528, 960),
    # ---- 59 BEDDING / 2 COMFORTERS -------------------------------------
    (880206, '7PC COMFORTER SET QUEEN', 59, 2, 201, 60215, 'PA-CS7Q', 2, 4, 16.00, 30.00, 0.06, '0', '3', True, 849528, 0),
    (880207, '5PC COMFORTER SET TWIN', 59, 2, 201, 60215, 'PA-CS5T', 2, 4, 12.50, 25.00, 0.06, '0', '3', False, 849528, 0),
    (880208, 'REVERSIBLE COMFORTER FULL/QN', 59, 2, 201, 71188, 'SL-RCFQ', 4, 8, 9.00, 18.00, 0.1, '0', '1', True, 849528, 0),
    # ---- 59 BEDDING / 3 THROWS & BLANKETS ------------------------------
    (880209, 'SHERPA THROW 50X60', 59, 3, 301, 59780, 'NW-SHR56', 6, 12, 4.75, 10.00, 0.80, '7', '3', True, 849528, 1440),
    (880210, 'PLUSH FLEECE THROW 50X60', 59, 3, 301, 59780, 'NW-PFT56', 6, 12, 2.40, 5.00, 1.10, '7', '3', True, 849528, 0),
    (880211, 'HOLIDAY PRINT THROW 50X60', 59, 3, 301, 59780, 'NW-HPT56', 6, 12, 3.60, 7.00, 0.70, '7', '3', True, 849528, 1800),
    (880212, 'PLUSH BLANKET QUEEN', 59, 3, 302, 71188, 'SL-PBQ', 4, 8, 7.50, 15.00, 0.1, '7', '3', False, 849528, 0),
]

MASTER_TEMPLATE = {86: 503589, 59: 104027}      # item master clone per dept
HIER_TEMPLATE = {86: (86, 4, 19, 0), 59: (59, 1, 101, 0)}
VENDOR_TEMPLATE = 57353
PH_OUT_DATE = 261201                            # YYMMDD - 12/01/26

INV_GROUPS = [  # (number, name, short, stores)
    (10, 'NC & VA STORES', 'NC/VA', [406, 458, 1838]),
    (20, 'GEORGIA STORES', 'GA', [215, 257]),
    (30, 'MISSISSIPPI STORES', 'MS', [205, 262]),
    (40, 'FL KY MD STORES', 'FL/KY/MD', [188, 499, 535]),
]

SKU_LO, SKU_HI = 880100, 880299

# ---------------------------------------------------------------------------
# DDS metadata
# ---------------------------------------------------------------------------
FIELD = re.compile(r'^     A {12}([A-Z0-9#@$]+)\s+(\d+)?([APSBLTZ])?\s*(\d+)?')


def columns(member):
    """[(name, length, type, decimals, text)] from a reconstructed .pf"""
    cols = []
    text_pending = None
    with open(os.path.join(DDS, member + '.pf')) as fh:
        lines = fh.read().split('\n')
    i = 0
    while i < len(lines):
        ln = lines[i]
        if ln.startswith('     A          R ') or ln.startswith('     A          K '):
            i += 1
            continue
        m = FIELD.match(ln) if len(ln) > 18 and ln[18] != ' ' else None
        if m and ln[16] == ' ':
            name = ln[18:28].strip()
            length = ln[29:34].strip()
            dtype = ln[34:35].strip() or 'A'
            dec = ln[35:37].strip()
            cols.append([name, int(length or 0), dtype, int(dec or 0), ''])
        elif 'TEXT(' in ln and cols and not cols[-1][4]:
            # gather a possibly continued TEXT('...') keyword
            buf = ln[44:]
            while buf.endswith('-'):
                i += 1
                buf = buf[:-1] + lines[i][44:]
            t = re.search(r"TEXT\('(.*)'\)", buf)
            cols[-1][4] = t.group(1).replace("''", "'") if t else ''
        i += 1
    return [tuple(c) for c in cols]


UNIT_RE = re.compile(r'units|qty|quantity|sls|sales|on hand|weekly|receipt|#/ordrs|avg inv oh|avg oh', re.I)
AMT_RE = re.compile(r'amt|rtl|cost$|cst', re.I)
NOSCALE_RE = re.compile(r'date|century|cent\b|turns|gmroi|lead|out of stock|vendor|dept|class|style|store', re.I)


def lit(col, value):
    name, length, dtype, dec, _ = col
    if value is None:
        return name
    if isinstance(value, str) and value.startswith('='):  # raw SQL expression
        return value[1:]
    if dtype in 'APSBZ' and dtype == 'A':
        v = str(value)[:length].replace("'", "''")
        return "'%s'" % v
    return repr(value) if isinstance(value, float) else str(value)


def scaled(col, factor_sql):
    name, length, dtype, dec, _ = col
    mx = 10 ** (length - dec) - (10 ** -dec if dec else 1)
    mx = ('%.' + str(dec) + 'f') % mx if dec else str(int(mx))
    # DOUBLE arithmetic: DECIMAL division in the factor (MOD(..)/10.0) yields a
    # huge scale, and large DC quantities then overflow (SQLSTATE 22003)
    # before the clamp is reached.
    return ('CAST(LEAST(GREATEST(ROUND(DOUBLE(%s) * DOUBLE(%s), %d), -%s), %s) AS DECIMAL(%d, %d))'
            % (name, factor_sql, dec, mx, mx, length, dec))


ONHAND = {'IBHBOY', 'IBHBOP', 'IBHAND', 'IBAVOH', 'CBHBOY', 'CBHBOP', 'CBHAND', 'CBAVOH'}


def clone(table, member, where, overrides, unit_f=None, amt_f=None, onhand_f=None):
    cols = columns(member)
    names = [c[0] for c in cols]
    unknown = set(overrides) - set(names)
    assert not unknown, (member, unknown)
    exprs = []
    for c in cols:
        n, length, dtype, dec, text = c
        if n in overrides:
            exprs.append(lit(c, overrides[n]))
        elif dtype in 'PSB' and onhand_f and n in ONHAND:
            exprs.append(scaled(c, onhand_f))
        elif dtype in 'PSB' and unit_f and not NOSCALE_RE.search(text) and UNIT_RE.search(text) and not AMT_RE.search(text):
            exprs.append(scaled(c, unit_f))
        elif dtype in 'PSB' and amt_f and not NOSCALE_RE.search(text) and AMT_RE.search(text):
            exprs.append(scaled(c, amt_f))
        else:
            exprs.append(n)
    return ('INSERT INTO {LIB}.%s (%s)\n  SELECT %s\n  FROM {LIB}.%s WHERE %s;'
            % (table, ', '.join(names), ',\n         '.join(exprs), table, where))


def store_var(store_col, sku):
    """0.6 .. 1.4 per store, 1.0 for distribution centres (>= 9000)"""
    return ('(CASE WHEN %s >= 9000 THEN 1.0E0 ELSE 0.6E0 + DOUBLE(MOD(INT(%s) * 13 + %d, 9)) / 10.0E0 END)'
            % (store_col, store_col, sku % 97))


def onhand_var(store_col, sku):
    """0.4 .. 1.65 per store/SKU on top of the unit factor, so sell-through
    and weeks of supply differ between rows; 1.0 for distribution centres"""
    return ('(CASE WHEN %s >= 9000 THEN 1.0E0 ELSE 0.4E0 + DOUBLE(MOD(INT(%s) * 7 + %d, 11)) / 8.0E0 END)'
            % (store_col, store_col, sku % 89))


def main():
    out = []
    w = out.append
    w('-- GENERATED by prfdemolib/tools/gen-seed-data.py - do not edit by hand.')
    w('-- Extra demo data on top of the PRFDEMOLIB sample. {LIB} = target library.')
    w('-- Re-runnable: everything it adds is deleted first.')
    w('')
    skus = 'BETWEEN %d AND %d' % (SKU_LO, SKU_HI)
    for t, k in [('INVMST', 'INUMBR'), ('INVBAL', 'INUMBR'), ('INVBAL1', 'INUMBR'),
                 ('INVCBL', 'INUMBR'), ('ITMRTL', 'SKU'), ('INVFSD', 'FSSKU'),
                 ('INVVEN', 'INUMBR'), ('SKVAUX', 'IXSKU'), ('RPLPRF', 'INUMBR'),
                 ('PHPF', 'PHSKU'), ('PHPFW2', 'PHSKU')]:
        w('DELETE FROM {LIB}.%s WHERE %s %s;' % (t, k, skus))
    w('DELETE FROM {LIB}.APSUPP WHERE ASNUM IN (%s);' % ', '.join(str(v[0]) for v in NEW_VENDORS))
    for d, s, c, _, _ in NEW_HIERARCHY:
        w('DELETE FROM {LIB}.INVDPT WHERE IDEPT = %d AND ISDEPT = %d AND ICLAS = %d AND ISCLAS = 0;' % (d, s, c))
    grp = ', '.join(str(g[0]) for g in INV_GROUPS)
    w('DELETE FROM {LIB}.TBLIGD WHERE INGNUM IN (%s);' % grp)
    w('DELETE FROM {LIB}.TBLIGH WHERE INGNUM IN (%s);' % grp)
    w('')

    w('-- Vendors')
    for num, name in NEW_VENDORS:
        w(clone('APSUPP', 'apsupp', 'ASNUM = %d' % VENDOR_TEMPLATE,
                {'ASNUM': num, 'ASNAME': name, 'ASALPH': name[:10]}))
    w('')
    w('-- Sub-departments and classes')
    for d, s, c, name, short in NEW_HIERARCHY:
        td, ts, tc, tsc = HIER_TEMPLATE[d]
        w(clone('INVDPT', 'invdpt',
                'IDEPT = %d AND ISDEPT = %d AND ICLAS = %d AND ISCLAS = %d' % (td, ts, tc, tsc),
                {'ISDEPT': s, 'ICLAS': c, 'ISCLAS': 0, 'DPTNAM': name, 'DPTSHT': short}))
    w('')

    for (sku, desc, dept, sub, cls, vnd, vpart, inpk, cspk, cost, rtl, f,
         seas, tkt, mapped, shape, ph) in ITEMS:
        w('-- %d %s' % (sku, desc))
        # shape template price, for scaling money columns
        tprice = {849528: 1.25}[shape]
        pr = round(rtl / tprice, 4)
        hier = {'D': dept, 'S': sub, 'C': cls}
        w(clone('INVMST', 'invmst', 'INUMBR = %d' % MASTER_TEMPLATE[dept], {
            'INUMBR': sku, 'IDESCR': desc, 'ISORT': desc[:10], 'ISTYL#': 'DEMOSEED',
            'ASNUM': vnd, 'IVNDP#': vpart, 'IASNUM': vnd,
            'IDEPT': dept, 'ISDEPT': sub, 'ICLAS': cls, 'ISCLAS': 0,
            'IMINPK': inpk, 'ISTDPK': cspk, 'ISEASN': seas,
            'ITKTTR': ('3' if dept == 86 else '1') if tkt == '3' else '0',
            'IATRB1': 'N', 'IATRB3': 'H' if dept == 86 else 'S',
            'IRPLCD': '4' if mapped else '0',
            'ICUCST': cost, 'ILSCST': cost, 'IFCCST': cost, 'INXCST': cost,
            'INLRTL': rtl, 'INLCST': cost, 'IDSCCD': 'A',
            'ISUBST': 0, 'ICORE': 0, 'IREPL': 0}))
        sv = store_var('ISTORE', sku)
        # all factor arithmetic in DOUBLE (E0 literals): DECIMAL products of
        # the MOD()/n terms overflow (SQL0406 / SQLSTATE 22003)
        uf = '(%sE0 * %s)' % (f, sv)
        af = '(%sE0 * %sE0 * %s)' % (f, pr, sv)
        bal = {'INUMBR': sku, 'IBDEPT': dept, 'IBSDPT': sub, 'IBCLAS': cls, 'IBSCLS': 0,
               'IBVNDR': vnd, 'IBSTYL': 'DEMOSEED', 'IBAVGC': cost, 'IBAVRT': rtl,
               'IBAVCS': cost, 'IBLAVC': cost}
        of = '(%s * %s)' % (uf, onhand_var('ISTORE', sku))
        w(clone('INVBAL', 'invbal', 'INUMBR = %d' % shape, bal, uf, af, of))
        w(clone('INVBAL1', 'invbal1', 'INUMBR = %d' % shape, bal, uf, af, of))
        w(clone('INVCBL', 'invcbl', 'INUMBR = %d' % shape, {
            'INUMBR': sku, 'CBDEPT': dept, 'CBSDPT': sub, 'CBCLAS': cls, 'CBSCLS': 0,
            'CBVNDR': vnd, 'CBSTYL': 'DEMOSEED', 'CBAVGC': cost, 'CBLAVC': cost},
            '%sE0' % f, '(%sE0 * %sE0)' % (f, pr)))
        w('INSERT INTO {LIB}.ITMRTL (SKU, REGMLT, REGPRC, ORIGMLT, ORIGRTL, Z2RTL, Z3RTL, Z1RTL)'
          ' VALUES (%d, 1, %.2f, 1, %.2f, %.2f, %.2f, %.2f);' % (sku, rtl, rtl, rtl, rtl, rtl))
        fsd = {'FSSKU': sku, 'ORIGRT': rtl, 'CURRTL': rtl}
        for p in ('FMD', 'LMD', 'FMU', 'LMU'):
            fsd.update({p + 'CEN': 0, p + 'YR': 0, p + 'MTH': 0, p + 'DAY': 0, p + 'RTL': 0})
        w(clone('INVFSD', 'invfsd', 'FSSKU = %d' % shape, fsd))
        w(clone('INVVEN', 'invven', 'INUMBR = %d' % shape, {
            'INUMBR': sku, 'IVVNUM': vnd, 'IVSORT': desc[:10], 'IVCCST': cost,
            'IVFCST': cost, 'IVVND#': vpart, 'IVSDPK': cspk, 'IVINPK': inpk,
            'IVDEPT': dept, 'IVSDPT': sub, 'IVCLAS': cls, 'IVSCLS': 0}))
        w(clone('SKVAUX', 'skvaux', 'IXSKU = %d' % shape, {'IXSKU': sku, 'IXVNUM': vnd}))
        if mapped:
            w(clone('RPLPRF', 'rplprf', 'INUMBR = %d' % shape, {
                'INUMBR': sku, 'RFANUM': vnd, 'RFREPL': 0, 'RFSSKU': 0,
                'RFDEPT': dept, 'RFSDPT': sub, 'RFCLAS': cls, 'RFSCLS': 0}))
        if ph:
            for table, whse, share in (('PHPF', 9001, 0.45), ('PHPFW2', 9002, 0.55)):
                q = int(round(ph * share / inpk)) * inpk
                w("INSERT INTO {LIB}.%s (PHSKU, PHQTY, PHOUT, PHNEW, PHQTY2, PHOUT2, PHQTY3,"
                  " PHOUT3, PHQTY4, PHOUT4, PHQTY5, PHOUT5, PHCHDT, PHCHTM, PHUSER, PHWHSE)"
                  " VALUES (%d, %d, %d, 0, 0, 0, 0, 0, 0, 0, 0, 0, CURRENT DATE, CURRENT TIME,"
                  " 'DEMOSEED', %d);" % (table, sku, q, PH_OUT_DATE, whse))
        w('')

    w('-- Inventory groups (INV046 Search For = I)')
    for num, name, short, stores in INV_GROUPS:
        w("INSERT INTO {LIB}.TBLIGH (INGNUM, INGNAM, INGSHT) VALUES (%d, '%s', '%s');" % (num, name, short))
        for s in stores:
            w('INSERT INTO {LIB}.TBLIGD (INGNUM, STRNUM) VALUES (%d, %d);' % (num, s))
    sys.stdout.write('\n'.join(out) + '\n')


if __name__ == '__main__':
    main()
