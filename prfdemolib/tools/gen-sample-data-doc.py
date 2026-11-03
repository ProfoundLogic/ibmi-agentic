#!/usr/bin/env python3
"""Write docs/SAMPLE-DATA.md - the "what can I key in?" reference for the
PRFDEMOLIB screens - straight from the data in a loaded library.

    python3 prfdemolib/tools/gen-sample-data-doc.py <LIBRARY> > prfdemolib/docs/SAMPLE-DATA.md

Run it after load-prfdemo-data.sh so the sheet matches what users will see.
Queries go over `ssh dev` (override with PRFDEMO_SSH_HOST) using db2util.
"""
import json
import os
import subprocess
import sys

LIB = sys.argv[1].upper() if len(sys.argv) > 1 else sys.exit(__doc__)
HOST = os.environ.get('PRFDEMO_SSH_HOST', 'dev')
SEED_LO, SEED_HI = 880100, 880299


def q(sql):
    sql = sql.replace('{L}', LIB)
    out = subprocess.run(['ssh', HOST, '/QOpenSys/pkgs/bin/db2util -o json "%s"' % sql],
                         capture_output=True, text=True, check=True).stdout
    try:
        rows = json.loads(out)['records']
    except Exception:
        sys.exit('query failed: %s\n%s' % (sql, out))
    # db2util returns numbers as JSON numbers; normalise everything to text
    return [{k: (v.strip() if isinstance(v, str) else ('' if v is None else str(v))) for k, v in r.items()}
            for r in rows]


def table(head, rows):
    out = ['| ' + ' | '.join(head) + ' |', '|' + '---|' * len(head)]
    for r in rows:
        out.append('| ' + ' | '.join('' if v is None else str(v) for v in r) + ' |')
    return '\n'.join(out)


def num(v, d=0):
    try:
        f = float(v)
    except (TypeError, ValueError):
        return v
    return ('{:,.%df}' % d).format(f)


stores = q("select STRNUM, STSHRT, STCITY, STPVST, REGNUM, STRDST, STRWHS, ZONNUM, STRHDO, STRNAM "
           "from {L}.TBLSTR order by STRNUM")
regions = q("select REGNUM, REGNAM from {L}.TBLREG order by 1")
districts = q("select STRDST, DSTNAM, REGNUM from {L}.TBLDST order by 1")
adis = q("select h.ADINUM, h.ADINAM, d.STRNUM from {L}.TBLADI h join {L}.INVADI d on d.ADINUM = h.ADINUM "
         "order by 1, 3")
igs = q("select h.INGNUM, h.INGNAM, d.STRNUM from {L}.TBLIGH h join {L}.TBLIGD d on d.INGNUM = h.INGNUM "
        "order by 1, 3")
seasons = q("select SEANUM, SEADSC from {L}.TBLSEA order by 1")
depts = q("select m.IDEPT, d.DPTNAM, count(*) N, "
          "sum(case when m.INUMBR between %d and %d then 1 else 0 end) S, "
          "sum(case when m.ITKTTR in ('1','3','4') then 1 else 0 end) T "
          "from {L}.INVMST m left join {L}.INVDPT d on d.IDEPT = m.IDEPT and d.ISDEPT = 0 "
          "and d.ICLAS = 0 and d.ISCLAS = 0 group by m.IDEPT, d.DPTNAM order by 3 desc, 1" % (SEED_LO, SEED_HI))
skus = q("select m.INUMBR, m.IDESCR, m.IDEPT, m.ISDEPT, m.ICLAS, m.ISEASN, m.IMINPK, m.ITKTTR, m.ISET, "
         "m.ASNUM, coalesce(a.ASNAME, '') VN, coalesce(r.REGPRC, 0) PRC, "
         "coalesce(w1.IBHAND, 0) W1, coalesce(w2.IBHAND, 0) W2, "
         "coalesce(p1.PHQTY, 0) + coalesce(p2.PHQTY, 0) PH, "
         "(select count(*) from {L}.INVBAL s where s.INUMBR = m.INUMBR and s.ISTORE < 9000) NS, "
         "(select count(*) from {L}.RPLPRF p where p.INUMBR = m.INUMBR) NM, "
         "(select coalesce(sum(t.TRFALC - t.TRFSHP), 0) from {L}.TRFDTL t where t.INUMBR = m.INUMBR "
         "and t.TRFSTS = 'A') AL "
         "from {L}.INVMST m left join {L}.APSUPP a on a.ASNUM = m.ASNUM "
         "left join {L}.ITMRTL r on r.SKU = m.INUMBR "
         "left join {L}.INVBAL w1 on w1.INUMBR = m.INUMBR and w1.ISTORE = 9001 "
         "left join {L}.INVBAL w2 on w2.INUMBR = m.INUMBR and w2.ISTORE = 9002 "
         "left join {L}.PHPF p1 on p1.PHSKU = m.INUMBR left join {L}.PHPFW2 p2 on p2.PHSKU = m.INUMBR "
         "order by m.IDEPT, m.ISDEPT, m.ICLAS, m.INUMBR")
names = {(r['IDEPT'], r['ISDEPT'], r['ICLAS']): r['DPTNAM'] for r in
         q("select IDEPT, ISDEPT, ICLAS, DPTNAM from {L}.INVDPT where ISCLAS = 0")}
sea = {r['SEANUM']: r['SEADSC'] for r in seasons}
store_of = {r['STRNUM']: r for r in stores}
n_skus = len(skus)
n_seed = sum(1 for s in skus if SEED_LO <= int(s['INUMBR']) <= SEED_HI)

o = []
w = o.append
w('# PRFDEMOLIB sample data: what to key in')
w('')
w('These screens have no working **F4 prompt / search** functions; the lookup programs '
  'are not part of the demo. Use this sheet instead. Every value below was read from '
  'library `%s` by `prfdemolib/tools/gen-sample-data-doc.py`, so it matches what the '
  'screens show. Regenerate it after reloading data.' % LIB)
w('')
w('**Where the data comes from:** %d SKUs in total. %d came from PRFDEMOLIB (a real '
  'sample from Variety Wholesalers / Roses). %d were **seeded for the demo** '
  '(SKUs 8801xx Seasonal Candy and 8802xx Bedding, marked *seed* below), modelled on '
  'what Roses sells, because the shipped sample is concentrated in one department. '
  'The seeding also added the four **inventory groups**, which were empty, and five vendors. '
  'See [How the data was built](#how-the-data-was-built).' % (n_skus, n_skus - n_seed, n_seed))
w('')
w('Contents: [Try these first](#try-these-first) · [Stores and warehouses](#stores-and-warehouses) · '
  '[Regions, districts, groups](#regions-districts-and-groups) · [Seasons](#seasons) · '
  '[Departments](#departments) · [SKU catalog](#sku-catalog) · [Vendors](#vendors)')
w('')

# ---------------------------------------------------------------- scenarios
w('## Try these first')
w('')
w('### INV046: Inventory by Store (menu option 4)')
w('')
w(table(['SKU Number', 'Store Type', 'Search For', 'Number', 'What you see'], [
    ['262657', 'A', 'G', '740', 'Store group SOFA TEST STORES: all 10 stores, group and chain totals'],
    ['880103', 'A', 'I', '30', 'Inventory group MISSISSIPPI STORES (205, 262): peppermint candy canes'],
    ['880209', 'A', 'I', '10', 'NC & VA STORES (406, 458, 1838): sherpa throws'],
    ['100001', 'S', 'R', '3', 'Region 3 (406, 535, 1838): a fast-selling washcloth'],
    ['827412', 'A', 'D', '11', 'District 11 (store 205 only)'],
    ['880101', 'A', 'S', '205', 'Starts at store 205 and lists onward, including the DCs'],
    ['880110', 'W', 'S', '9001', 'Warehouses only: 9001 onward'],
]))
w('')
w('Press **F5** to switch between current period and year to date, and **Page Down** for more stores.')
w('')
w('> **Use Store Type `A` with Search For `G` or `I`.** Every group member is a store, so the '
  'result is the same, and `S`/`W` are unreliable there: INV046 checks the store type of the '
  '*previous* store it read (an original program bug), so on the first inquiry the list can come '
  'back empty.')
w('')
w('### INV502: Item Inquiry (menu option 5)')
w('')
w(table(['SKU', 'What it shows'], [
    ['262657', 'ASST TOPS: large pack-and-hold at both DCs, next out 1/03/27'],
    ['100001', '13x13 WASH CLOTH: allocated quantities at 9001/9002, a busy replenishment item'],
    ['248117', 'Sofa sold as a *Bundle Component* (Set Code 6)'],
    ['166402', 'ASST SWIM SEPARATE TOPS: pack-and-hold'],
    ['880209', 'SHERPA THROW (seed): new vendor, Fall/Winter 2026 season, pack-and-hold releasing 12/01/26'],
    ['880103', 'PEPPERMINT CANDY CANES (seed): holiday candy with pack-and-hold'],
    ['880206', '7PC COMFORTER SET (seed): big-ticket item, $30.00'],
    ['999999', 'Error path: *INV0297: Invalid sku entered.*'],
]))
w('')
w('**Page Down / Page Up** step through every SKU in number order (%d of them).' % n_skus)
w('')
w('### TS0110: Ticket Item Inventory (menu option 6)')
w('')
w(table(['Store', 'Dept', 'Ticketed only', 'Available only', 'What you see'], [
    ['205', '12', 'N', 'N', 'Ladies Intimate Basics, the full department (largest list)'],
    ['205', '12', 'Y', 'Y', 'The same department narrowed to ticketed items the DC can ship'],
    ['205', '86', 'Y', 'Y', 'Seasonal Candy (seed): Christmas and Harvest candy, pack-and-hold on candy canes'],
    ['406', '86', 'N', 'N', 'The same department from warehouse 9001 (store 406)'],
    ['205', '59', 'N', 'N', 'Bedding (seed): sheets, comforters, throws; includes unmapped (Map N) items'],
    ['458', '59', 'Y', 'Y', 'Bedding at a 9001-supplied store'],
    ['205', '40', 'N', 'N', 'A one-item department (ASST TOPS)'],
    ['999', '12', '', '', 'Error path: *Invalid store number - press RESET*'],
]))
w('')
w('Season: leave **ALL**, or key a season code from [Seasons](#seasons), e.g. `7` for '
  'Fall/Winter 2026 only. To try **F10 = create distribution**, key `1`-`3` in the **Pks** column '
  'on one or two lines first.')
w('')

# ---------------------------------------------------------------- stores
w('## Stores and warehouses')
w('')
w('All stores are Roses stores. *Short name* is what INV046 shows in its Store column. '
  '*Warehouse* is the DC that supplies the store; TS0110 shows it top left (e.g. WH9002).')
w('')
w(table(['Store', 'Short name', 'City, state', 'Region', 'District', 'Warehouse', 'Price zone'],
        [[s['STRNUM'], s['STSHRT'], '%s, %s' % (s['STCITY'].title(), s['STPVST']), s['REGNUM'],
          s['STRDST'], s['STRWHS'], s['ZONNUM']] for s in stores if s['STRHDO'] == 'S']))
w('')
w('**Warehouses / distribution centres** (Store Type `W` in INV046):')
w('')
w(table(['Number', 'Name'], [[s['STRNUM'], s['STRNAM']] for s in stores if s['STRHDO'] == 'W']))
w('')
w('There are no offices (`O`) or inventory locations (`I`) in the sample, so those Store Type '
  'codes list nothing.')
w('')

# ---------------------------------------------------------------- groupings
w('## Regions, districts and groups')
w('')
w('For INV046 *Search For*:')
w('')
by = {}
for s in stores:
    if s['STRHDO'] == 'S':
        by.setdefault(('R', s['REGNUM']), []).append(s['STRNUM'])
        by.setdefault(('D', s['STRDST']), []).append(s['STRNUM'])
rows = [['R', r['REGNUM'], r['REGNAM'], ', '.join(by.get(('R', r['REGNUM']), [])) or '(warehouses only)']
        for r in regions]
rows += [['D', d['STRDST'], '%s (region %s)' % (d['DSTNAM'].title(), d['REGNUM']),
          ', '.join(by.get(('D', d['STRDST']), [])) or '(warehouses only)'] for d in districts]
grp = {}
for a in adis:
    grp.setdefault((a['ADINUM'], a['ADINAM']), []).append(a['STRNUM'])
rows += [['G', k[0], k[1], ', '.join(v)] for k, v in grp.items()]
grp = {}
for a in igs:
    grp.setdefault((a['INGNUM'], a['INGNAM']), []).append(a['STRNUM'])
rows += [['I', k[0], k[1] + ' *(seed)*', ', '.join(v)] for k, v in grp.items()]
w(table(['Search For', 'Number', 'Name', 'Stores'], rows))
w('')
w('`S` (store) takes any store or warehouse number from the tables above.')
w('')

# ---------------------------------------------------------------- seasons
w('## Seasons')
w('')
w(table(['Code', 'Season'], [[s['SEANUM'], s['SEADSC']] for s in seasons]))
w('')

# ---------------------------------------------------------------- departments
w('## Departments')
w('')
w('*Ticketed* = SKUs TS0110 keeps when *Ticketed skus only* = Y.')
w('')
w(table(['Dept', 'Name', 'SKUs', 'of which seeded', 'Ticketed'],
        [[d['IDEPT'], d['DPTNAM'], d['N'], d['S'] or '', d['T']] for d in depts]))
w('')

# ---------------------------------------------------------------- catalog
w('## SKU catalog')
w('')
w('Columns: **Sub/Cls** sub-department and class · **Pack** store ship pack · **Retail** chain '
  'price · **Tkt** ticketed (TS0110 filter) · **Map** has a replenishment profile for the stores '
  '(TS0110 Map column) · **DC 9001 / 9002** warehouse on hand · **P&H** pack-and-hold units across '
  'both DCs · **Alloc** units allocated to transfers not yet shipped · **Stores** sample stores with '
  'a balance.')
w('')
cur = None
for s in skus:
    if s['IDEPT'] != cur:
        if cur is not None:
            w('')
        cur = s['IDEPT']
        w('### Dept %s %s' % (cur, names.get((cur, '0', '0'), '')))
        w('')
        w('| SKU | Description | Sub/Cls | Season | Vendor | Pack | Retail | Tkt | Map | DC 9001 | DC 9002 | P&H | Alloc | Stores |')
        w('|---|---|---|---|---|---|---|---|---|---|---|---|---|---|')
    seed = SEED_LO <= int(s['INUMBR']) <= SEED_HI
    cls = names.get((s['IDEPT'], s['ISDEPT'], s['ICLAS']), '')
    w('| %s%s | %s | %s/%s %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s |' % (
        s['INUMBR'], ' *seed*' if seed else '', s['IDESCR'], s['ISDEPT'], s['ICLAS'], cls.title(),
        s['ISEASN'], s['VN'].title() or s['ASNUM'], s['IMINPK'], num(s['PRC'], 2),
        'Y' if s['ITKTTR'] in ('1', '3', '4') else '', 'Y' if int(s['NM']) else '',
        num(s['W1']), num(s['W2']), num(s['PH']) if float(s['PH']) else '',
        num(s['AL']) if float(s['AL']) else '', s['NS']))
w('')

# ---------------------------------------------------------------- vendors
w('## Vendors')
w('')
vend = {}
for s in skus:
    vend.setdefault((s['ASNUM'], s['VN']), 0)
    vend[(s['ASNUM'], s['VN'])] += 1
w(table(['Vendor', 'Name', 'SKUs'], [[k[0], k[1], v] for k, v in sorted(vend.items(), key=lambda x: int(x[0][0]))]))
w('')

w('## How the data was built')
w('')
w('- `prfdemolib/tools/load-prfdemo-data.sh <LIB>` copies the PRFDEMOLIB sample with CPYF, then '
  'runs the seed. Add `--no-seed` for the shipped sample only.')
w('- The seed comes from `prfdemolib/tools/gen-seed-data.py`, which documents every item. Each '
  'seeded row is a copy of a real sample row (SKU 849528 for balances and history; 503589 and '
  '104027 for the item master). Identity, hierarchy, vendor, pack, cost and price are replaced, '
  'and quantities are scaled per item and varied per store, so the screens behave exactly as '
  'they do for the real SKUs.')
w('- Seeded SKUs have style number `DEMOSEED`. To remove them, rerun the loader with `--no-seed`. '
  'Seeded departments 86 and 59 also contain one original PRFDEMOLIB SKU each (503589, 104027).')
w('- Product choices follow Roses departments listed on rosesdiscountstores.com and vwstores.com '
  '(Seasonal, Home, Food & Beverage) and the current Fall/Holiday 2026 season.')
sys.stdout.write('\n'.join(o) + '\n')
