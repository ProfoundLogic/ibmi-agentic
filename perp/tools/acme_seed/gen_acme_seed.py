#!/usr/bin/env python3
"""
Generate the ACME Distribution (company ACM) demo dataset for PERPDEMO.

Writes one SQL script that deletes every ACM-scoped row and reloads a
complete, internally consistent dataset: item classes, items, UOM
conversions, vendors, item-vendor sourcing, price history, lots,
requisitions, purchase orders, delivery schedules, receipts and the
lot reconciliation log, then resets the REQ / PO / RCP document
sequences.

Dates are anchored to the run date (or --as-of), so the data never goes
stale: re-run the generator and reload to refresh it. Content is
deterministic for a given anchor (fixed RNG seed). All transactional
dates fall on weekdays.

The rules follow what the PERP programs themselves write:
  * poreqr creates POs as OPEN, one per preferred vendor, with po_line
    source_requisition_number/line linkage; requisitions stay APPROVED
    (the app never writes CONVERTED).
  * rcventr closes a fully received line as CLOSED, sets the header to
    RECEIVED when every line is CLOSED, else PARTIAL; receipts are POSTED.
  * reqauto approves as CODERFLOW when confidence >= approval.auto_threshold
    and total <= approval.auto_max_amount, with its exact note text.

Deliberate exceptions are planted so every PERP portal report has rows
(see perp/docs/portal-reports/README.md).

Usage:
  python3 gen_acme_seed.py                      # anchor = today
  python3 gen_acme_seed.py --as-of 2026-10-23   # reproduce a snapshot
  python3 gen_acme_seed.py --out path/to/file.sql
"""
import argparse
import datetime as dt
import json
import math
import os
import random
from collections import defaultdict

HERE = os.path.dirname(os.path.abspath(__file__))
DEFAULT_OUT = os.path.normpath(os.path.join(HERE, '..', '..', 'qddlsrc', 'seed', '030_acme_demo_data.sql'))
LIB = 'PERPDEMO'
CO = 'ACM'
SEED = 20261023

# --------------------------------------------------------------------------
# Calendar helpers -- everything transactional lands on a weekday
# --------------------------------------------------------------------------
def is_wd(d):
    return d.weekday() < 5

def on_or_after_wd(d):
    while not is_wd(d):
        d += dt.timedelta(days=1)
    return d

def on_or_before_wd(d):
    while not is_wd(d):
        d -= dt.timedelta(days=1)
    return d

def add_wd(d, n):
    step = 1 if n >= 0 else -1
    while n:
        d += dt.timedelta(days=step)
        if is_wd(d):
            n -= step
    return d

def wd_between(a, b):
    """Working days from a to b (b > a positive)."""
    if a == b:
        return 0
    sign = 1 if b > a else -1
    lo, hi = (a, b) if b > a else (b, a)
    n = 0
    d = lo
    while d < hi:
        d += dt.timedelta(days=1)
        if is_wd(d):
            n += 1
    return n * sign

def add_months(d, m, day=None):
    y, mo = d.year, d.month + m
    while mo > 12:
        y, mo = y + 1, mo - 12
    while mo < 1:
        y, mo = y - 1, mo + 12
    day = day or d.day
    for dd in (day, 28):
        try:
            return dt.date(y, mo, dd)
        except ValueError:
            continue
    return dt.date(y, mo, 28)

TODAY_WINDOW = {'date': None}     # events on the anchor date are timed in the morning, so none lie in the future


def ts(d, rng, lo=(7, 30), hi=(17, 0)):
    if d == TODAY_WINDOW['date']:
        lo, hi = (7, 0), (10, 0)
    mins = rng.randint(lo[0] * 60 + lo[1], hi[0] * 60 + hi[1])
    return dt.datetime(d.year, d.month, d.day, mins // 60, mins % 60, rng.randint(0, 59))

# --------------------------------------------------------------------------
# Master data additions (on top of acme_base_catalogue.json)
# --------------------------------------------------------------------------
NEW_CLASSES = [('PLUMBING', 'Plumbing supplies'), ('JANITORIAL', 'Janitorial and sanitation')]

NEW_USERS = [  # (user, name, email, role)
    ('ERAMIREZ', 'Elena Ramirez', 'eramirez@acmedist.example', 'BUYER'),
    ('JKIM', 'Jason Kim', 'jkim@acmedist.example', 'REQUESTER'),
]

TAX_IDS = {'VND01': '34-1827765', 'VND02': '36-4410298', 'VND03': '61-1199873', 'VND04': '31-0957721',
           'VND05': '34-2290154', 'VND06': '25-1763092', 'VND07': '35-0688214', 'VND08': '38-3305617',
           'VND09': '20-4471580', 'VND10': '27-1160943'}

# code, name, addr, city, st, zip, phone, email, contact, buyer, terms, tax, active
NEW_VENDORS = [
    ('VND11', 'Great Lakes Plumbing Supply', '4410 Grand River Ave', 'Detroit', 'MI', '48208', '(313) 555-2290', 'orders@glplumbing.example', 'Dana Kowalski', 'ERAMIREZ', 'NET30', '38-2219904', 'Y'),
    ('VND12', 'CleanPro Janitorial Distributors', '88 Speedway Dr', 'Indianapolis', 'IN', '46224', '(317) 555-7712', 'sales@cleanpro.example', 'Marcus Hill', 'ERAMIREZ', 'NET30', '35-2087731', 'Y'),
    ('VND13', 'Buckeye Fastener & Bolt', '210 Webster St', 'Dayton', 'OH', '45402', '(937) 555-4108', 'orders@buckeyebolt.example', 'Tom Reilly', 'JMORGAN', 'NET30', '31-1540288', 'Y'),
    ('VND14', 'Keystone Packaging Group', '1500 Smallman St', 'Pittsburgh', 'PA', '15222', '(412) 555-6650', 'cs@keystonepack.example', 'Angela Brooks', 'LCHEN', 'NET60', '25-1998476', 'Y'),
    ('VND15', 'Allied Electrical Wholesale', '777 Joyce Ave', 'Columbus', 'OH', '43219', '(614) 555-3384', 'quotes@alliedelec.example', 'Ray Castillo', 'JMORGAN', 'NET30', '31-1702395', 'Y'),
    ('VND16', 'Shield Safety Supply', '901 Story Ave', 'Louisville', 'KY', '40206', '(502) 555-8823', 'orders@shieldsafety.example', 'Nadia Farouk', 'JMORGAN', 'NET15', '61-1488320', 'Y'),
    ('VND17', 'Pacific Rim Tool Imports', '2400 E Anaheim St', 'Long Beach', 'CA', '90804', '(562) 555-1047', 'export@pacrimtools.example', 'Kenji Watanabe', 'LCHEN', 'NET60', '95-4630817', 'Y'),
    ('VND18', 'Heartland Industrial Lubricants', '3300 Coliseum Blvd', 'Fort Wayne', 'IN', '46805', '(260) 555-9031', 'orders@heartlandlube.example', 'Cheryl Dunn', 'ERAMIREZ', 'NET30', '35-1873364', 'Y'),
    # new vendor, master data not finished yet (no tax id, no email) -- master data health exception
    ('VND19', 'Tri-State MRO Supply', '615 Central Pkwy', 'Cincinnati', 'OH', '45202', '(513) 555-2766', '', 'Lou Bennett', 'LCHEN', 'NET30', '', 'Y'),
    # inactive vendor: history only
    ('VND20', 'Northern Hardware Co.', '42 Lake Ave', 'Toledo', 'OH', '43604', '(419) 555-0185', 'orders@northernhw.example', 'Gail Prentice', 'ERAMIREZ', 'NET30', '34-0711652', 'N'),
]

# item, desc, short, class, inv_uom, stk_uom, lot, rop, crit, ss, minq, maxq, lead, shelf_life_months,
#   preferred(vendor, price, lead, moq, pack), alternates [(vendor, price, lead, moq, pack)]
NEW_ITEMS = [
    ('FAS-008', 'Hex Nut 1/4-20 Zinc', '1/4-20 Hex Nut', 'FASTENERS', 'EA', 'EA', 'N', 60, 10, 20, 40, 400, 7, None, ('VND13', 0.09, 5, 100, 100), [('VND01', 0.095, 5, 100, 100), ('VND19', 0.085, 7, 500, 100)]),
    ('FAS-009', 'Wedge Anchor Bolt 1/2 x 6in', '1/2x6 Wedge Anchor', 'FASTENERS', 'EA', 'EA', 'N', 40, 8, 12, 20, 240, 10, None, ('VND13', 1.85, 7, 25, 25), [('VND01', 1.97, 7, 25, 25)]),
    ('FAS-010', 'Self-Drilling Screw #10 x 3/4in', '#10 Tek Screw', 'FASTENERS', 'EA', 'EA', 'N', 200, 40, 80, 100, 1500, 7, None, ('VND01', 0.06, 5, 500, 500), [('VND13', 0.058, 7, 1000, 500)]),
    ('FAS-011', 'Threaded Rod 3/8-16 x 6ft Zinc', '3/8 Threaded Rod', 'FASTENERS', 'EA', 'EA', 'N', 20, 4, 6, 10, 90, 10, None, ('VND13', 6.40, 7, 10, 10), []),
    ('FAS-012', 'Cotter Pin Assortment Kit', 'Cotter Pin Kit', 'FASTENERS', 'EA', 'EA', 'N', 6, 1, 2, 4, 30, 7, None, ('VND01', 14.75, 7, 1, 1), [('VND19', 13.90, 10, 1, 1)]),
    ('TOL-008', 'Cordless Drill 20V Kit', '20V Drill Kit', 'TOOLS', 'EA', 'EA', 'N', 4, 1, 2, 2, 16, 14, None, ('VND02', 129.00, 10, 1, 1), [('VND08', 124.50, 14, 1, 1), ('VND17', 98.40, 40, 10, 5)]),
    ('TOL-009', 'Socket Set 40pc 3/8in Drive', '40pc Socket Set', 'TOOLS', 'EA', 'EA', 'N', 5, 1, 2, 3, 20, 14, None, ('VND17', 38.20, 40, 10, 5), [('VND02', 52.75, 10, 1, 1)]),
    ('TOL-010', 'Torque Wrench 1/2in 150 ft-lb', 'Torque Wrench', 'TOOLS', 'EA', 'EA', 'N', 3, 1, 1, 2, 12, 14, None, ('VND08', 74.90, 10, 1, 1), [('VND02', 79.60, 10, 1, 1)]),
    ('TOL-011', 'Hacksaw Frame 12in (discontinued)', 'Hacksaw 12in', 'TOOLS', 'EA', 'EA', 'N', 0, 0, 0, 0, 20, 7, None, ('VND20', 11.40, 7, 1, 1), []),
    ('TOL-012', 'Pry Bar 24in Forged', '24in Pry Bar', 'TOOLS', 'EA', 'EA', 'N', 5, 1, 2, 3, 24, 14, None, ('VND17', 9.85, 40, 12, 6), [('VND08', 15.20, 10, 1, 1)]),
    ('SAF-008', 'Hard Hat Type I White', 'Hard Hat White', 'SAFETY', 'EA', 'EA', 'N', 15, 3, 5, 10, 80, 10, None, ('VND16', 12.60, 7, 6, 6), [('VND03', 13.35, 10, 1, 1)]),
    ('SAF-009', 'Foam Ear Plugs (200 pair box)', 'Ear Plugs 200pr', 'SAFETY', 'EA', 'CS', 'N', 10, 2, 4, 6, 60, 7, None, ('VND16', 24.80, 7, 1, 1), [('VND03', 26.10, 7, 1, 1)]),
    ('SAF-010', 'Cut-Resistant Gloves A4 (dozen)', 'A4 Cut Gloves dz', 'SAFETY', 'EA', 'EA', 'N', 20, 4, 8, 12, 120, 10, None, ('VND16', 46.50, 7, 2, 1), [('VND03', 49.90, 10, 1, 1)]),
    ('SAF-011', 'Full Body Safety Harness', 'Safety Harness', 'SAFETY', 'EA', 'EA', 'N', 4, 1, 2, 2, 16, 14, None, ('VND03', 118.00, 14, 1, 1), []),
    ('SAF-012', 'Eyewash Station Refill 32oz', 'Eyewash Refill', 'SAFETY', 'EA', 'EA', 'Y', 12, 2, 4, 6, 60, 10, 24, ('VND16', 18.40, 7, 6, 6), [('VND03', 19.75, 10, 1, 1)]),
    ('CON-008', 'Shop Towels Roll (55 sheets)', 'Shop Towel Roll', 'CONSUMABL', 'EA', 'EA', 'N', 40, 8, 12, 24, 240, 7, None, ('VND04', 3.15, 5, 12, 12), [('VND12', 3.05, 7, 24, 12)]),
    ('CON-009', 'Thread Locker Blue 50ml', 'Threadlock Blue', 'CONSUMABL', 'EA', 'EA', 'Y', 12, 2, 4, 6, 72, 7, 24, ('VND04', 14.20, 5, 6, 6), [('VND19', 13.10, 10, 12, 6)]),
    ('CON-010', 'Epoxy Putty Stick 4oz', 'Epoxy Putty', 'CONSUMABL', 'EA', 'EA', 'Y', 10, 2, 3, 6, 60, 7, 18, ('VND04', 8.75, 5, 6, 6), []),
    ('CON-011', 'Cable Ties 8in Black (1000ct)', 'Cable Ties 8in', 'CONSUMABL', 'EA', 'EA', 'N', 8, 2, 3, 4, 40, 7, None, ('VND15', 21.40, 5, 1, 1), [('VND04', 22.85, 5, 1, 1)]),
    ('CON-012', 'Duct Tape 2in x 60yd Silver', 'Duct Tape 2in', 'CONSUMABL', 'EA', 'EA', 'N', 24, 5, 8, 12, 144, 7, None, ('VND04', 6.40, 5, 12, 12), [('VND14', 6.15, 7, 24, 12)]),
    ('ELE-008', 'Extension Cord 50ft 12/3 SJTW', '50ft Ext Cord', 'ELECTRICAL', 'EA', 'EA', 'N', 6, 1, 2, 4, 30, 10, None, ('VND15', 42.90, 7, 1, 1), [('VND05', 45.10, 7, 1, 1)]),
    ('ELE-009', 'LED Shop Light 4ft 5000K', '4ft LED Light', 'ELECTRICAL', 'EA', 'EA', 'N', 8, 2, 3, 4, 40, 10, None, ('VND15', 36.75, 7, 2, 2), [('VND08', 39.40, 10, 1, 1)]),
    ('ELE-010', 'GFCI Receptacle 20A White', 'GFCI 20A', 'ELECTRICAL', 'EA', 'EA', 'N', 10, 2, 4, 6, 50, 7, None, ('VND05', 17.85, 7, 5, 5), [('VND15', 17.20, 7, 10, 5)]),
    ('ELE-011', 'Heat Shrink Tubing Assortment', 'Heat Shrink Kit', 'ELECTRICAL', 'EA', 'EA', 'N', 6, 1, 2, 3, 30, 7, None, ('VND05', 15.60, 7, 1, 1), []),
    ('ELE-012', 'Lever Wire Connector 3-Port (100ct)', 'Lever Connector', 'ELECTRICAL', 'EA', 'CS', 'N', 5, 1, 2, 3, 25, 7, None, ('VND15', 38.50, 7, 1, 1), [('VND05', 41.25, 7, 1, 1)]),
    ('PKG-008', 'Stretch Wrap 18in x 1500ft', 'Stretch Wrap 18in', 'PACKAGING', 'EA', 'EA', 'N', 24, 4, 8, 12, 120, 7, None, ('VND14', 21.90, 5, 4, 4), [('VND06', 23.40, 5, 4, 4)]),
    ('PKG-009', 'Packing Tape 2in Clear (roll)', 'Packing Tape 2in', 'PACKAGING', 'EA', 'CS', 'N', 72, 12, 24, 36, 432, 5, None, ('VND14', 2.35, 5, 36, 36), [('VND06', 2.49, 5, 36, 36)]),
    ('PKG-010', 'Corrugated Box 24x18x12', 'Box 24x18x12', 'PACKAGING', 'EA', 'EA', 'N', 100, 20, 40, 60, 600, 7, None, ('VND14', 2.95, 5, 50, 25), [('VND06', 3.10, 5, 50, 25)]),
    ('PKG-011', 'Pallet Label 4x6 (roll of 500)', 'Label 4x6 Roll', 'PACKAGING', 'EA', 'EA', 'N', 6, 1, 2, 3, 30, 7, None, ('VND14', 19.80, 7, 2, 1), []),
    ('PKG-012', 'Kraft Void Fill Paper (spool ft)', 'Void Fill Paper', 'PACKAGING', 'FT', 'FT', 'N', 1500, 250, 500, 1000, 9000, 7, None, ('VND06', 0.055, 5, 1000, 500), [('VND14', 0.052, 7, 1500, 500)]),
    ('LUB-009', 'Chain Lube Aerosol 10oz', 'Chain Lube 10oz', 'LUBRICANTS', 'EA', 'EA', 'Y', 12, 2, 4, 6, 72, 10, 36, ('VND18', 9.20, 7, 12, 12), [('VND07', 10.15, 14, 12, 12)]),
    ('LUB-010', 'Food Grade Grease H1 14oz', 'H1 Grease 14oz', 'LUBRICANTS', 'EA', 'EA', 'Y', 20, 4, 8, 10, 120, 14, 24, ('VND18', 11.60, 10, 10, 10), [('VND07', 12.95, 21, 10, 10)]),
    ('LUB-011', 'Anti-Seize Compound 8oz', 'Anti-Seize 8oz', 'LUBRICANTS', 'EA', 'EA', 'Y', 8, 2, 3, 4, 48, 10, 36, ('VND18', 13.40, 7, 6, 6), []),
    ('LUB-012', 'Cutting Oil 1gal', 'Cutting Oil 1gal', 'LUBRICANTS', 'EA', 'EA', 'Y', 6, 1, 2, 4, 36, 10, 36, ('VND07', 24.80, 14, 4, 4), [('VND18', 23.95, 7, 4, 4)]),
    ('LUB-013', 'Dry PTFE Lubricant 11oz', 'PTFE Dry Lube', 'LUBRICANTS', 'EA', 'EA', 'Y', 10, 2, 3, 6, 60, 10, 36, ('VND18', 10.70, 7, 12, 12), [('VND10', 9.90, 7, 12, 12)]),
    ('PLM-001', 'PVC Pipe 1/2in Sch 40 (ft)', '1/2 PVC Pipe', 'PLUMBING', 'FT', 'FT', 'N', 100, 20, 40, 60, 600, 7, None, ('VND11', 0.62, 5, 100, 10), [('VND19', 0.58, 10, 200, 10)]),
    ('PLM-002', 'PVC Elbow 1/2in 90deg Slip', '1/2 PVC Elbow', 'PLUMBING', 'EA', 'EA', 'N', 50, 10, 20, 30, 300, 7, None, ('VND11', 0.48, 5, 50, 25), []),
    ('PLM-003', 'Ball Valve 3/4in Brass FNPT', '3/4 Ball Valve', 'PLUMBING', 'EA', 'EA', 'N', 10, 2, 4, 6, 60, 10, None, ('VND11', 14.30, 7, 5, 1), [('VND19', 13.25, 10, 10, 5)]),
    ('PLM-004', 'PTFE Thread Tape 1/2in x 520in', 'PTFE Tape', 'PLUMBING', 'EA', 'EA', 'N', 36, 6, 12, 24, 240, 5, None, ('VND11', 0.95, 5, 24, 12), []),
    ('PLM-005', 'Pipe Thread Sealant 8oz', 'Thread Sealant', 'PLUMBING', 'EA', 'EA', 'Y', 8, 2, 3, 4, 48, 7, 24, ('VND11', 8.40, 5, 6, 6), []),
    ('PLM-006', 'Hose Clamp Stainless #12 (10pk)', 'Hose Clamp #12', 'PLUMBING', 'EA', 'EA', 'N', 12, 2, 4, 6, 72, 7, None, ('VND11', 7.90, 5, 5, 1), [('VND13', 8.35, 7, 5, 1)]),
    ('PLM-007', 'Copper Pipe 3/4in Type L (ft)', '3/4 Copper Pipe', 'PLUMBING', 'FT', 'FT', 'N', 60, 10, 20, 40, 400, 10, None, ('VND11', 5.85, 7, 20, 10), []),
    ('PLM-008', 'Faucet Supply Line 3/8 x 20in', 'Supply Line 20in', 'PLUMBING', 'EA', 'EA', 'N', 15, 3, 5, 10, 90, 7, None, ('VND11', 6.20, 5, 10, 5), [('VND19', 5.95, 10, 10, 5)]),
    ('JAN-001', 'Trash Can Liner 55gal (case 100)', 'Liner 55gal cs', 'JANITORIAL', 'EA', 'CS', 'N', 8, 2, 3, 4, 48, 5, None, ('VND12', 38.50, 5, 2, 1), [('VND19', 36.90, 7, 4, 1)]),
    ('JAN-002', 'Neutral Floor Cleaner 1gal', 'Floor Cleaner', 'JANITORIAL', 'EA', 'EA', 'Y', 12, 2, 4, 6, 72, 5, 24, ('VND12', 16.75, 5, 4, 4), []),
    ('JAN-003', 'Foam Hand Soap Refill 1000ml', 'Hand Soap 1000ml', 'JANITORIAL', 'EA', 'EA', 'Y', 18, 4, 6, 12, 108, 5, 24, ('VND12', 9.80, 5, 6, 6), []),
    ('JAN-004', 'Paper Towel Roll 800ft (case 6)', 'Towel Roll cs6', 'JANITORIAL', 'EA', 'CS', 'N', 10, 2, 4, 6, 60, 5, None, ('VND12', 42.60, 5, 2, 1), [('VND14', 44.10, 7, 2, 1)]),
    ('JAN-005', 'Wet Mop Head Looped Large', 'Mop Head Large', 'JANITORIAL', 'EA', 'EA', 'N', 6, 1, 2, 4, 36, 7, None, ('VND12', 11.25, 5, 6, 6), []),
    ('JAN-006', 'Disinfectant Spray 19oz', 'Disinfectant 19oz', 'JANITORIAL', 'EA', 'EA', 'Y', 24, 4, 8, 12, 144, 5, 24, ('VND12', 7.40, 5, 12, 12), [('VND19', 7.05, 10, 24, 12)]),
    ('JAN-007', 'Oil Absorbent Granules 25lb', 'Absorbent 25lb', 'JANITORIAL', 'LB', 'LB', 'N', 200, 40, 75, 100, 1200, 7, None, ('VND12', 0.44, 5, 250, 25), [('VND04', 0.47, 5, 250, 25)]),
]

# shelf life (months) for the existing lot-controlled items
BASE_SHELF_LIFE = {'SAF-002': 36, 'SAF-005': 36, 'SAF-006': 24, 'CON-003': 12, 'CON-004': 18, 'CON-005': 24,
                   'ELE-006': 60, 'LUB-001': 36, 'LUB-003': 36, 'LUB-005': 36}

# extra sourcing on existing items (so new vendors have a footprint)
EXTRA_SOURCING = [  # item, vendor, price_factor_vs_preferred, lead, moq, pack, active
    ('FAS-001', 'VND13', 1.03, 7, 100, 50, 'Y'), ('FAS-002', 'VND13', 0.97, 7, 50, 25, 'Y'),
    ('FAS-004', 'VND13', 1.02, 7, 100, 50, 'Y'), ('ELE-003', 'VND15', 1.04, 5, 5, 1, 'Y'),
    ('ELE-007', 'VND15', 0.98, 5, 10, 5, 'Y'), ('SAF-001', 'VND16', 1.05, 7, 12, 12, 'Y'),
    ('SAF-003', 'VND16', 0.99, 7, 6, 6, 'Y'), ('PKG-002', 'VND14', 1.01, 5, 50, 25, 'Y'),
    ('PKG-004', 'VND14', 0.98, 5, 4, 1, 'Y'), ('LUB-002', 'VND18', 1.04, 7, 4, 1, 'Y'),
    ('LUB-004', 'VND18', 0.97, 7, 12, 12, 'Y'), ('CON-001', 'VND12', 1.02, 7, 4, 1, 'Y'),
    ('CON-006', 'VND12', 1.06, 7, 6, 6, 'Y'), ('TOL-003', 'VND19', 0.95, 10, 6, 6, 'Y'),
    ('TOL-006', 'VND19', 0.93, 10, 6, 6, 'Y'), ('CON-002', 'VND19', 0.96, 10, 50, 10, 'Y'),
    ('SAF-004', 'VND19', 0.94, 10, 12, 12, 'Y'),
    ('FAS-005', 'VND20', 1.00, 7, 50, 50, 'N'), ('TOL-002', 'VND20', 0.98, 7, 1, 1, 'N'),
]

AISLE_BY_CLASS = {'FASTENERS': 'A1', 'TOOLS': 'A2', 'SAFETY': 'A3', 'CONSUMABL': 'A4', 'ELECTRICAL': 'A5',
                  'PACKAGING': 'A6', 'LUBRICANTS': 'A7', 'PLUMBING': 'A8', 'JANITORIAL': 'A9'}

# vendor delivery behaviour: list of (weight, working-day delta vs promise)
VENDOR_PROFILE = {
    'VND07': [(30, 0), (20, 3), (20, 8), (15, 15), (15, -1)],     # Apex -- unreliable
    'VND02': [(55, 0), (20, 2), (15, 5), (10, -1)],               # Titan -- sometimes late
    'VND17': [(35, 0), (30, 5), (20, 12), (15, -2)],              # Pacific Rim -- import, slips
    'default': [(60, 0), (18, -1), (14, 2), (8, 4)],
}
SHORT_SHIP = {'VND07': 0.40, 'VND17': 0.25, 'VND02': 0.15, 'default': 0.08}

REQUESTERS = [('MNGUYEN', 24), ('AFOSTER', 24), ('JKIM', 22), ('GJONES', 10), ('BMAY', 10), ('RBETANC', 10)]
REQ_CLASS_AFFINITY = {
    'MNGUYEN': ['PACKAGING', 'CONSUMABL', 'JANITORIAL', 'SAFETY'],
    'AFOSTER': ['TOOLS', 'FASTENERS', 'SAFETY', 'LUBRICANTS'],
    'JKIM': ['ELECTRICAL', 'PLUMBING', 'LUBRICANTS', 'TOOLS'],
}
APPROVERS = ['RPATEL', 'DWALSH']
RECEIVERS = ['TSANTOS', 'KOBRIEN']

APPROVE_NOTES = ['Approved -- routine restock.', 'Approved -- within department budget.',
                 'Approved; please consolidate with the weekly vendor order.', 'Approved -- needed for scheduled maintenance.',
                 'Approved.', 'Approved -- replaces damaged stock.', 'Approved -- tool crib replenishment.']
APPROVE_NOTES_HIGH = ['Approved -- high priority, please expedite.', 'Approved -- line-down risk, order today.']
REJECT_NOTES = ['Rejected -- stock on hand covers this; issue from inventory.',
                'Rejected -- please split by cost center and resubmit.',
                'Rejected -- use the approved substitute item instead.',
                'Rejected -- quantity looks high for one month of use; resubmit with usage detail.']
REQ_LINE_NOTES = ['', '', '', '', 'Restock tool crib', 'For conveyor line maintenance', 'Replace damaged stock',
                  'Dock 3 supplies', 'Monthly shop supplies', 'For the Building B retrofit', 'Safety audit follow-up']


class Gen:
    def __init__(self, anchor):
        self.A = anchor
        self.rng = random.Random(SEED)
        cat = json.load(open(os.path.join(HERE, 'acme_base_catalogue.json')))
        self.cat = cat
        bm = dt.date.fromisoformat(cat['base_month'] + '-01')
        self.month_shift = (anchor.year - bm.year) * 12 + (anchor.month - bm.month)
        self.S = add_wd(on_or_before_wd(anchor), -125)            # start of transactional window
        self.AI_START = add_wd(on_or_before_wd(anchor), -70)      # CoderFlow scoring went live
        self.rows = defaultdict(list)
        self.exceptions = []

    # ---------------------------------------------------------------- master
    def build_master(self):
        r = self.rng
        A = self.A
        # classes
        self.classes = [(c['class_code'], c['description']) for c in self.cat['item_class']] + NEW_CLASSES
        # vendors
        self.vendors = {}
        for v in self.cat['vendors']:
            v = dict(v)
            v['tax_id'] = TAX_IDS.get(v['vendor_code'], v.get('tax_id', ''))
            v['created'] = dt.date(2024, 7, 1) + dt.timedelta(days=r.randint(0, 120))
            self.vendors[v['vendor_code']] = v
        for (code, name, addr, city, st, zp, ph, em, ct, buyer, terms, tax, act) in NEW_VENDORS:
            created = add_months(A, -r.randint(8, 20), 1) if code not in ('VND19',) else add_wd(A, -28)
            if code == 'VND20':
                created = add_months(A, -30, 3)
            self.vendors[code] = dict(vendor_code=code, vendor_name=name, address_line1=addr, address_line2='',
                                      city_name=city, state_code=st, postal_code=zp, country_code='US',
                                      phone_number=ph, email_address=em, contact_name=ct, buyer_code=buyer,
                                      payment_terms_code=terms, tax_id=tax, is_active=act, created=created)
        # items
        self.items = {}
        for it in self.cat['items']:
            it = dict(it)
            it['shelf_life'] = BASE_SHELF_LIFE.get(it['item_number']) if it['lot_controlled'] == 'Y' else None
            self.items[it['item_number']] = it
        for (num, desc, short, cls, iu, su, lot, rop, crit, ss, mn, mx, lead, shelf, pref, alts) in NEW_ITEMS:
            self.items[num] = dict(item_number=num, item_description=desc, short_description=short, class_code=cls,
                                   inventory_uom=iu, stocking_uom=su, lot_controlled=lot, reorder_point=rop,
                                   critical_level=crit, safety_stock=ss, min_qty=mn, max_qty=mx, lead_time_days=lead,
                                   is_active='N' if num == 'TOL-011' else 'Y', shelf_life=shelf)
        for it in self.items.values():
            it['created'] = add_months(A, -r.randint(9, 22), r.choice([1, 15]))
        # sourcing
        self.iv = {}
        for p in self.cat['item_vendor']:
            k = (p['item_number'], p['vendor_code'])
            self.iv[k] = dict(p)
        prices = defaultdict(list)
        for p in self.cat['prices']:
            k = (p['item_number'], p['vendor_code'])
            ef = add_months(dt.date.fromisoformat(p['effective_from']), self.month_shift)
            et = add_months(dt.date.fromisoformat(p['effective_to']), self.month_shift) if p['effective_to'] else None
            prices[k].append(dict(ef=ef, et=et, price=p['unit_price'], src=p['price_source']))
        for (num, desc, short, cls, iu, su, lot, rop, crit, ss, mn, mx, lead, shelf, pref, alts) in NEW_ITEMS:
            pv, pp, pl, pm, pk = pref
            self.iv[(num, pv)] = dict(item_number=num, vendor_code=pv, vendor_part_number=f'{pv[-2:]}-{num}',
                                      lead_time_days=pl, moq=pm, pack_size=pk, is_preferred='Y',
                                      is_active='N' if pv == 'VND20' else 'Y')
            prices[(num, pv)] = self.price_history(pp)
            for (av, ap, al, am, ak) in alts:
                self.iv[(num, av)] = dict(item_number=num, vendor_code=av, vendor_part_number=f'{av[-2:]}-{num}',
                                          lead_time_days=al, moq=am, pack_size=ak, is_preferred='N',
                                          is_active='Y')
                prices[(num, av)] = self.price_history(ap)
        for (num, av, fac, al, am, ak, act) in EXTRA_SOURCING:
            pv = self.preferred(num)
            base = self.price_on(prices[(num, pv)], A)
            self.iv[(num, av)] = dict(item_number=num, vendor_code=av, vendor_part_number=f'{av[-2:]}-{num}',
                                      lead_time_days=al, moq=am, pack_size=ak, is_preferred='N', is_active=act)
            prices[(num, av)] = self.price_history(round(base * fac, 4 if base < 1 else 2),
                                                   end=add_months(A, -4, 1) if act == 'N' else None)
        # VND08 Crestline becomes preferred for the torque wrench (already above); keep VND09/VND10 untapped.
        # Recent and announced (future-dated) price changes
        active_pairs = [k for k, v in self.iv.items() if v['is_active'] == 'Y' and self.vendors[k[1]]['is_active'] == 'Y']
        active_pairs.sort()
        recent = r.sample(active_pairs, 16)
        for k in recent:
            hist = prices[k]
            last = max(hist, key=lambda h: h['ef'])
            ef = add_months(A, -r.randint(0, 1), r.choice([1, 15]))
            if ef >= A - dt.timedelta(days=3):
                ef = add_months(A, -1, 15)
            if ef <= last['ef'] + dt.timedelta(days=60):
                continue
            last['et'] = ef - dt.timedelta(days=1)
            hist.append(dict(ef=ef, et=None, price=self.bump(last['price'], r.uniform(0.02, 0.07)), src='MANUAL'))
        prefs = sorted([k for k in active_pairs if self.iv[k]['is_preferred'] == 'Y'])
        future = r.sample(prefs, 10)
        self.future_price_pairs = []
        for k in future:
            hist = prices[k]
            last = max(hist, key=lambda h: h['ef'])
            ef = add_months(A, r.choice([1, 1, 2, 3]), 1)
            last['et'] = ef - dt.timedelta(days=1)
            hist.append(dict(ef=ef, et=None, price=self.bump(last['price'], r.uniform(0.03, 0.09)),
                             src=r.choice(['IMPORT', 'MANUAL']), announced=add_wd(A, -r.randint(3, 12))))
            self.future_price_pairs.append(k)
        # planted: overlapping open-ended price rows (an import that did not close the old row)
        k = ('ELE-004', 'VND05')
        hist = prices[k]
        last = max(hist, key=lambda h: h['ef'])
        if last['ef'] < add_months(A, -2, 1):
            ef = add_months(A, -2, 1)
            hist.append(dict(ef=ef, et=None, price=self.bump(last['price'], 0.04), src='IMPORT'))
            last['et'] = None
            self.exceptions.append('Overlapping price rows: ELE-004 / VND05')
        # planted: expired quotes with no replacement (approved vendor, no current price)
        for k in (('TOL-006', 'VND19'), ('PLM-003', 'VND19')):
            hist = prices[k]
            hist.sort(key=lambda h: h['ef'])
            cut = add_months(A, -3, 1) - dt.timedelta(days=1)
            hist[:] = [h for h in hist if h['ef'] <= cut]
            if hist:
                hist[-1]['et'] = cut
            self.exceptions.append(f'Expired quote, no current price: {k[0]} / {k[1]}')
        self.prices = prices
        # UOM conversions for the case-stocked items
        self.uomconv = [('SAF-009', 'CS', 'EA', 12), ('PKG-009', 'CS', 'EA', 36), ('JAN-001', 'CS', 'EA', 4),
                        ('JAN-004', 'CS', 'EA', 2)]
        # ELE-012 is stocked in CS with NO conversion on file -- planted master data exception
        self.exceptions.append('ELE-012 stocked in CS with no CS->EA conversion (receipt in CS is blocked)')

    def price_history(self, current, end=None):
        """Steps every 4-8 months, from ~2 years back up to ~5 months ago, ending at `current`."""
        r = self.rng
        A = self.A
        start = add_months(A, -r.randint(18, 26), r.choice([1, 15]))
        dates = [start]
        while True:
            nd = add_months(dates[-1], r.randint(4, 8), r.choice([1, 15]))
            if nd > add_months(A, -5, 1):
                break
            dates.append(nd)
        steps = [r.choice([0.02, 0.03, 0.04, 0.05, 0.06, -0.015]) for _ in dates[1:]]
        p = current
        ps = [p]
        for s in reversed(steps):
            p = p / (1 + s)
            ps.append(p)
        ps = list(reversed(ps))
        hist = []
        for i, d in enumerate(dates):
            et = dates[i + 1] - dt.timedelta(days=1) if i + 1 < len(dates) else end
            pr = round(ps[i], 4) if ps[i] < 1 else round(ps[i], 2)
            hist.append(dict(ef=d, et=et, price=pr, src='MANUAL'))
        if end and hist[-1]['ef'] > end:
            hist = [h for h in hist if h['ef'] <= end]
            hist[-1]['et'] = end
        return hist

    @staticmethod
    def bump(p, pct):
        v = p * (1 + pct)
        return round(v, 4) if v < 1 else round(v, 2)

    @staticmethod
    def price_on(hist, d):
        best = None
        for h in hist:
            if h['ef'] <= d and (h['et'] is None or h['et'] >= d):
                if best is None or h['ef'] > best['ef']:
                    best = h
        return best['price'] if best else None

    def preferred(self, item):
        for (i, v), p in self.iv.items():
            if i == item and p['is_preferred'] == 'Y':
                return v
        raise KeyError(item)

    def vprice(self, item, vendor, d):
        p = self.price_on(self.prices[(item, vendor)], d)
        if p is None:
            p = self.price_on(self.prices[(item, vendor)], max(h['ef'] for h in self.prices[(item, vendor)]))
        return p

    # ---------------------------------------------------------- requisitions
    def build_requisitions(self):
        r = self.rng
        A = on_or_before_wd(self.A)
        cfg_thr, cfg_max = 85.0, 500.0
        days = []
        d = self.S
        while d <= A:
            days.append(d)
            d = add_wd(d, 1)
        active_items = [i for i, it in self.items.items() if it['is_active'] == 'Y']
        n_reqs = 72
        req_days = sorted(r.choice(days) for _ in range(n_reqs - 6))
        # make sure the last few days are populated (approval queue, drafts)
        req_days += [A, A, A, add_wd(A, -1), add_wd(A, -1), add_wd(A, -1), add_wd(A, -2), add_wd(A, -2), add_wd(A, -3)]
        req_days.sort()
        self.reqs = []
        num = 1000
        for rd in req_days:
            num += 1
            who = r.choices([u for u, _ in REQUESTERS], [w for _, w in REQUESTERS])[0]
            pri = r.choices(['LOW', 'NORMAL', 'HIGH', 'CRITICAL'], [15, 60, 20, 5])[0]
            self.reqs.append(dict(num=num, by=who, date=rd, pri=pri, lines=[]))
        self.req_by_num = {q['num']: q for q in self.reqs}
        # lines
        for q in self.reqs:
            nl = r.choices([1, 2, 3, 4], [35, 35, 20, 10])[0]
            aff = REQ_CLASS_AFFINITY.get(q['by'])
            chosen = set()
            first_cls = None
            for ln in range(1, nl + 1):
                for _ in range(20):
                    if first_cls and r.random() < 0.7:     # related items: same class, usually the same vendor
                        pool = [i for i in active_items if self.items[i]['class_code'] == first_cls]
                    else:
                        pool = [i for i in active_items if (not aff or self.items[i]['class_code'] in aff or r.random() < 0.15)]
                    it = r.choice(pool)
                    if it not in chosen and it != 'TOL-011':
                        break
                chosen.add(it)
                first_cls = first_cls or self.items[it]['class_code']
                q['lines'].append(self.make_req_line(q, ln, it))
        # planted: a stale draft (not submitted) and an empty draft
        stale = dict(num=None, by='JKIM', date=add_wd(A, -24), pri='NORMAL', lines=[], force='DRAFT')
        empty = dict(num=None, by='GJONES', date=add_wd(A, -9), pri='NORMAL', lines=[], force='DRAFT')
        stale['lines'].append(None)
        self.reqs += [stale, empty]
        # planted: self approval and approval by a buyer (segregation-of-duties exceptions)
        sod1 = dict(num=None, by='DWALSH', date=add_wd(A, -38), pri='NORMAL', lines=[], force='SELF')
        sod2 = dict(num=None, by='BMAY', date=add_wd(A, -52), pri='HIGH', lines=[], force='BUYERAPPROVED')
        # planted: unrealistic need-by -- one genuine (SUBMITTED, low stock), one coverable from stock
        tight1 = dict(num=None, by='AFOSTER', date=add_wd(A, -1), pri='HIGH', lines=[], force='TIGHT_SUBMITTED')
        tight2 = dict(num=None, by='MNGUYEN', date=add_wd(A, -2), pri='HIGH', lines=[], force='TIGHT_APPROVED')
        # planted: approved long ago and never converted
        stuck1 = dict(num=None, by='JKIM', date=add_wd(A, -19), pri='NORMAL', lines=[], force='STUCK')
        stuck2 = dict(num=None, by='RBETANC', date=add_wd(A, -13), pri='HIGH', lines=[], force='STUCK')
        sod3 = dict(num=None, by='MNGUYEN', date=add_wd(A, -29), pri='NORMAL', lines=[], force='REQUESTERAPPROVED')
        oldai = dict(num=None, by='AFOSTER', date=add_wd(self.AI_START, 4), pri='NORMAL', lines=[], force='OLDPOLICY_AI')
        sla = dict(num=None, by='JKIM', date=add_wd(A, -7), pri='HIGH', lines=[], force='SLA_WAIT')
        nopref = dict(num=None, by='JKIM', date=add_wd(A, -8), pri='NORMAL', lines=[], force='NOPREF_STUCK')
        self.reqs += [sod1, sod2, sod3, tight1, tight2, stuck1, stuck2, oldai, sla, nopref]
        self.forced_stock = {}
        # renumber all in date order so numbering follows time
        self.reqs.sort(key=lambda q: (q['date'], q['num'] or 99999))
        for i, q in enumerate(self.reqs, start=1001):
            q['num'] = i
        for q in self.reqs:
            f = q.get('force')
            if f == 'DRAFT' and q['lines'] == [None]:
                q['lines'] = [self.make_req_line(q, 1, 'ELE-009')]
            elif f == 'DRAFT':
                q['lines'] = []
            elif f == 'SELF':
                q['lines'] = [self.make_req_line(q, 1, 'SAF-008'), self.make_req_line(q, 2, 'SAF-010')]
            elif f == 'BUYERAPPROVED':
                q['lines'] = [self.make_req_line(q, 1, 'PKG-004')]
            elif f == 'TIGHT_SUBMITTED':
                q['lines'] = [self.make_req_line(q, 1, 'SAF-011', qty=2)]
                self.forced_stock['SAF-011'] = 'critical'
                self.forced_stock.update({'CON-008': 'below_safety', 'ELE-008': 'below_rop', 'FAS-005': 'below_rop'})
            elif f == 'TIGHT_APPROVED':
                q['lines'] = [self.make_req_line(q, 1, 'PKG-008', qty=8)]
                self.forced_stock['PKG-008'] = 'healthy'
            elif f == 'REQUESTERAPPROVED':
                q['lines'] = [self.make_req_line(q, 1, 'JAN-003'), self.make_req_line(q, 2, 'JAN-005')]
            elif f == 'OLDPOLICY_AI':
                q['lines'] = [self.make_req_line(q, 1, 'TOL-003', qty=4)]
            elif f == 'SLA_WAIT':
                q['lines'] = [self.make_req_line(q, 1, 'ELE-009', qty=6), self.make_req_line(q, 2, 'ELE-008', qty=4)]
            elif f == 'NOPREF_STUCK':
                q['lines'] = [self.make_req_line(q, 1, 'CON-008', qty=48)]
            elif f == 'STUCK':
                it = 'PLM-003' if q['by'] == 'JKIM' else 'TOL-005'
                q['lines'] = [self.make_req_line(q, 1, it, qty=6 if it == 'PLM-003' else 4)]
        # lifecycle
        for q in self.reqs:
            self.lifecycle(q, A, cfg_thr, cfg_max)

    def make_req_line(self, q, ln, it, qty=None):
        r = self.rng
        item = self.items[it]
        pv = self.preferred(it)
        price = self.vprice(it, pv, q['date'])
        pack = self.iv[(it, pv)]['pack_size'] or 1
        if qty is None:
            target = r.lognormvariate(math.log(140), 0.7)
            qty = max(1, round(target / max(price, 0.01)))
            if pack > 1:
                qty = max(pack, round(qty / pack) * pack)
            qty = min(qty, max(item['max_qty'], pack))
            qty = int(max(1, qty))
        return dict(line=ln, item=it, qty=qty, uom=item['inventory_uom'], cost=price, notes=r.choice(REQ_LINE_NOTES))

    def lifecycle(self, q, A, thr, mx):
        r = self.rng
        q['total'] = round(sum(l['qty'] * l['cost'] for l in q['lines']), 2)
        lead = max([self.iv[(l['item'], self.preferred(l['item']))]['lead_time_days'] for l in q['lines']] or [7])
        buf = {'CRITICAL': r.randint(1, 3), 'HIGH': r.randint(2, 6), 'NORMAL': r.randint(5, 15), 'LOW': r.randint(10, 25)}[q['pri']]
        q['need_by'] = on_or_after_wd(q['date'] + dt.timedelta(days=lead + buf))
        f = q.get('force')
        if f == 'TIGHT_SUBMITTED':
            q['need_by'] = on_or_after_wd(q['date'] + dt.timedelta(days=3))
        elif f == 'TIGHT_APPROVED':
            q['need_by'] = add_wd(q['date'], 1)          # wanted next day, vendor needs a week
        q['created'] = ts(q['date'], r)
        q['status'] = 'DRAFT'
        q['conf'] = None
        q['approved_by'] = q['approved_at'] = q['src'] = q['notes_apr'] = None
        q['updated'] = q['created']
        age = wd_between(q['date'], A)
        if f == 'DRAFT' or (age == 0 and r.random() < 0.45 and not f):
            return
        if not q['lines']:
            return
        # submit
        sub_date = q['date'] if r.random() < 0.8 else add_wd(q['date'], 1)
        if sub_date > A:
            return
        sub = ts(sub_date, r) if sub_date != q['date'] else q['created'] + dt.timedelta(minutes=r.randint(4, 90))
        q['status'] = 'SUBMITTED'
        q['updated'] = sub
        if f == 'OLDPOLICY_AI':                        # approved under the old 80% threshold, before the policy was raised
            q['conf'] = 82.50
            q['status'] = 'APPROVED'
            q['approved_by'] = q['src'] = 'CODERFLOW'
            q['approved_at'] = sub + dt.timedelta(minutes=2, seconds=r.randint(0, 59))
            q['notes_apr'] = (f"Auto-approved by CoderFlow: confidence {q['conf']:.2f}% >= threshold 80.00%, "
                              f"total {q['total']:.2f} <= max {mx:.2f}.")
            q['updated'] = q['approved_at']
            self.exceptions.append(f"AI approval below the current 85% threshold (old 80% policy): requisition {q['num']}")
            return
        if f == 'SLA_WAIT':
            q['conf'] = 71.20
            self.exceptions.append(f"Waiting for approval 7 working days (SLA breach): requisition {q['num']}")
            return
        if sub_date >= self.AI_START and r.random() < 0.85 and f not in ('SELF', 'BUYERAPPROVED', 'REQUESTERAPPROVED', 'NOPREF_STUCK'):
            q['conf'] = round(r.triangular(58, 99, 90), 2)
            if f in ('TIGHT_SUBMITTED',):
                q['conf'] = 78.40
        decide = None
        if q['conf'] is not None and q['conf'] >= thr and q['total'] <= mx and f not in ('TIGHT_SUBMITTED',):
            q['status'] = 'APPROVED'
            q['approved_by'] = 'CODERFLOW'
            q['src'] = 'CODERFLOW'
            q['approved_at'] = sub + dt.timedelta(minutes=r.randint(1, 4), seconds=r.randint(0, 59))
            q['notes_apr'] = (f"Auto-approved by CoderFlow: confidence {q['conf']:.2f}% >= threshold {thr:.2f}%, "
                              f"total {q['total']:.2f} <= max {mx:.2f}.")
            q['updated'] = q['approved_at']
        else:
            delay = 0 if f == 'TIGHT_APPROVED' else r.choices([0, 1, 2, 3, 5], [30, 35, 18, 12, 5])[0]
            if f == 'TIGHT_SUBMITTED':
                return
            decide = add_wd(sub_date, delay)
            if age <= 2 and f not in ('TIGHT_APPROVED',) and r.random() < 0.75:
                return                       # still in the approval queue
            if decide > A:
                return
            q['approved_at'] = ts(decide, r, lo=(8, 0), hi=(16, 30))
            if q['approved_at'] <= sub:
                q['approved_at'] = sub + dt.timedelta(minutes=r.randint(20, 200))
            q['src'] = 'HUMAN'
            q['approved_by'] = r.choice(APPROVERS)
            if f == 'SELF':
                q['approved_by'] = 'DWALSH'
                self.exceptions.append(f"Self-approval: requisition {q['num']} (DWALSH)")
            if f == 'REQUESTERAPPROVED':
                q['approved_by'] = 'GJONES'
                self.exceptions.append(f"Approved by a requester-role user: requisition {q['num']} (GJONES)")
            if f == 'BUYERAPPROVED':
                q['approved_by'] = 'LCHEN'
                self.exceptions.append(f"Approved by a buyer (no approver role): requisition {q['num']} (LCHEN)")
            reject = r.random() < 0.15 and not f
            if reject:
                q['status'] = 'REJECTED'
                q['notes_apr'] = r.choice(REJECT_NOTES)
            else:
                q['status'] = 'APPROVED'
                q['notes_apr'] = r.choice(APPROVE_NOTES_HIGH if q['pri'] in ('HIGH', 'CRITICAL') else APPROVE_NOTES)
            q['updated'] = q['approved_at']

    # ------------------------------------------------------------ purchasing
    def build_pos(self):
        r = self.rng
        A = on_or_before_wd(self.A)
        self.pos = []
        # 1. conversions of approved requisitions (poreqr): buyer converts 1-4 working days after approval
        buckets = defaultdict(list)
        self.awaiting = []
        for q in self.reqs:
            if q['status'] != 'APPROVED':
                continue
            f = q.get('force')
            appr_d = q['approved_at'].date()
            conv = add_wd(appr_d, 1)
            while conv.weekday() != 1:                   # buyers run poreqr weekly, on Tuesdays
                conv = add_wd(conv, 1)
            if q["pri"] in ("HIGH", "CRITICAL") and r.random() < 0.6:
                conv = add_wd(appr_d, 1)                 # urgent ones converted the next day
            if f in ('STUCK', 'TIGHT_APPROVED', 'NOPREF_STUCK') or conv > A:
                self.awaiting.append(q['num'])
                continue
            for l in q['lines']:
                v = self.preferred(l['item'])
                buckets[(v, conv)].append((q, l))
        for (v, conv), lines in sorted(buckets.items(), key=lambda kv: (kv[0][1], kv[0][0])):
            po = self.new_po(v, conv, source='REQ')
            for (q, l) in lines:
                self.add_po_line(po, l['item'], l['qty'], src=(q['num'], l['line']))
            self.pos.append(po)
        # 2. buyer replenishment POs (poentr): DRAFT then released
        days = []
        d = self.S
        while d <= A:
            days.append(d)
            d = add_wd(d, 1)
        vend_items = defaultdict(list)
        for (i, v), p in self.iv.items():
            if p['is_preferred'] == 'Y' and p['is_active'] == 'Y' and self.items[i]['is_active'] == 'Y':
                vend_items[v].append(i)
        recent_days = [d for d in days if wd_between(d, A) <= 15]
        for k in range(26):
            od = r.choice(days) if k < 14 else r.choice(recent_days)
            v = r.choice(sorted(vend_items))
            its = r.sample(vend_items[v], min(len(vend_items[v]), r.randint(2, 4)))
            po = self.new_po(v, od, source='DIRECT')
            for it in its:
                item = self.items[it]
                p = self.iv[(it, v)]
                qty = max(item['max_qty'] - item['reorder_point'], p['moq'] or 1, 1)
                pk = p['pack_size'] or 1
                qty = int(math.ceil(qty / pk) * pk)
                self.add_po_line(po, it, qty)
            self.pos.append(po)
        for (off, its) in ((-22, [('TOL-009', 20), ('TOL-012', 24)]), (-9, [('TOL-008', 10), ('TOL-009', 10)])):
            po = self.new_po('VND17', add_wd(A, off), source='DIRECT')
            po['notes'] = 'Import order -- ocean freight, LCL consolidation'
            for (it, qn) in its:
                self.add_po_line(po, it, qn)
            self.pos.append(po)
        # 3. blanket POs with delivery schedules
        self.blankets = []
        for (v, it, qty_per, n, first_off, step_m) in (('VND07', 'LUB-001', 30, 6, -70, 1),
                                                        ('VND14', 'PKG-010', 200, 4, -10, 0),
                                                        ('VND03', 'SAF-002', 40, 3, -27, 1)):
            od = add_wd(A, first_off - 8)
            po = self.new_po(v, od, source='BLANKET')
            po['notes'] = 'Blanket order -- releases per delivery schedule'
            ln = self.add_po_line(po, it, qty_per * n)
            ln['expected'] = None
            sched = []
            for s in range(n):
                if step_m:
                    sd = on_or_after_wd(add_months(add_wd(A, first_off), s))
                else:
                    sd = add_wd(A, first_off + 10 * s)
                sched.append(dict(seq=s + 1, date=sd, qty=qty_per, rcv=0, notes=''))
            ln['schedule'] = sched
            self.pos.append(po)
            self.blankets.append(po)
        # 4. drafts: an empty shell, a stale draft with lines, one in progress
        for (v, off, lines) in (('VND13', -34, []), ('VND15', -27, [('ELE-010', 20), ('ELE-008', 6)]),
                                ('VND12', -1, [('JAN-003', 24)])):
            po = self.new_po(v, add_wd(A, off), source='DRAFT')
            po['status'] = 'DRAFT'
            for (it, qn) in lines:
                self.add_po_line(po, it, qn)
            self.pos.append(po)
        # planted: an open PO line in CS for ELE-012 with no conversion (cannot be received)
        po = self.new_po('VND15', add_wd(A, -6), source='DIRECT')
        ln = self.add_po_line(po, 'ELE-012', 4)
        ln['uom'] = 'CS'
        ln['no_receive'] = True
        po['notes'] = 'Ordered by the case -- 4 CS'
        self.pos.append(po)
        # number POs in date order
        self.pos.sort(key=lambda p: (p['date'], p['vendor']))
        for i, p in enumerate(self.pos, start=4001):
            p['num'] = i

    def new_po(self, v, d, source):
        r = self.rng
        created = ts(d, r)
        return dict(num=None, vendor=v, buyer=self.vendors[v]['buyer_code'], date=d, created=created,
                    source=source, lines=[], status='OPEN', notes='')

    def add_po_line(self, po, it, qty, src=None):
        r = self.rng
        v = po['vendor']
        price = self.vprice(it, v, po['date'])
        if r.random() < 0.08 and po['source'] == 'DIRECT':
            price = self.bump(price, r.choice([-0.05, -0.03, 0.04]))   # negotiated / quoted price
        lead = self.iv[(it, v)]['lead_time_days'] or self.items[it]['lead_time_days'] or 7
        promise = on_or_after_wd(po['date'] + dt.timedelta(days=lead))
        has_exp = po['source'] in ('DIRECT',) or r.random() < 0.6
        ln = dict(line=len(po['lines']) + 1, item=it, qty=qty, uom=self.items[it]['inventory_uom'], price=price,
                  promise=promise, expected=promise if has_exp else None, src=src, rcv=0, deliveries=[],
                  schedule=None)
        po['lines'].append(ln)
        return ln

    # ------------------------------------------------------------- receiving
    def simulate_deliveries(self):
        r = self.rng
        A = on_or_before_wd(self.A)
        for po in self.pos:
            if po['status'] == 'DRAFT':
                continue
            v = po['vendor']
            prof = VENDOR_PROFILE.get(v, VENDOR_PROFILE['default'])
            for ln in po['lines']:
                if ln.get('no_receive'):
                    continue
                if ln['schedule']:
                    for s in ln['schedule']:
                        delta = r.choices([d for _, d in prof], [w for w, _ in prof])[0]
                        if po['vendor'] == 'VND07' and s['seq'] == 3:
                            s['notes'] = 'Shipped short 20/30'      # screen column is 22 chars wide
                            arr = add_wd(s['date'], 4)
                            if arr <= A:
                                ln['deliveries'].append((arr, s['qty'] - 10, s))
                            continue
                        if po['vendor'] == 'VND07' and s['seq'] == 4:
                            s['notes'] = 'Backordered, no date'
                            continue
                        arr = add_wd(s['date'], max(delta, 0))
                        if arr <= A and s['date'] < A:
                            ln['deliveries'].append((arr, s['qty'], s))
                    continue
                delta = r.choices([d for _, d in prof], [w for w, _ in prof])[0]
                arr = add_wd(ln['promise'], delta)
                if arr > A:
                    continue
                short = r.random() < SHORT_SHIP.get(v, SHORT_SHIP['default'])
                if short and ln['qty'] >= 4:
                    first = max(1, int(ln['qty'] * r.uniform(0.4, 0.8)))
                    ln['deliveries'].append((arr, first, None))
                    bal_d = add_wd(arr, r.randint(3, 15))
                    if bal_d <= A and not (v == 'VND07' and wd_between(arr, A) < 25):   # Apex: recent balances still owed
                        ln['deliveries'].append((bal_d, ln['qty'] - first, None))
                else:
                    ln['deliveries'].append((arr, ln['qty'], None))
        # planted: deliveries the vendor never shipped -> late, spread over the ageing bands
        cands = [(po, ln) for po in self.pos if po['status'] != 'DRAFT' for ln in po['lines']
                 if ln['deliveries'] and not ln['schedule'] and not ln.get('no_receive')
                 and add_wd(A, -45) <= ln['promise'] <= add_wd(A, -2)]
        r.shuffle(cands)
        per_v = defaultdict(int)
        bands = {'31-45': 3, '15-30': 4, '2-14': 5}
        for po, ln in cands:
            age = wd_between(ln['promise'], A)
            band = '31-45' if age > 30 else '15-30' if age >= 15 else '2-14'
            if bands[band] <= 0 or per_v[po['vendor']] >= 3:
                continue
            bands[band] -= 1
            per_v[po['vendor']] += 1
            if len(ln['deliveries']) > 1 or (ln['qty'] >= 10 and r.random() < 0.4):
                d0, q0, s0 = ln['deliveries'][0]
                ln['deliveries'] = [(d0, q0 if q0 < ln['qty'] else max(1, int(ln['qty'] * 0.5)), s0)]
            else:
                ln['deliveries'] = []
            if age > 25 and not po['notes']:
                po['notes'] = 'Vendor confirmed backorder -- revised ship date pending'
            self.exceptions.append(f"Late delivery ({age} working days): PO {po['num'] or '?'} {ln['item']} from {po['vendor']}")
        # make sure the dock has work today and this week
        open_future = [(po, ln) for po in self.pos if po['status'] != 'DRAFT' for ln in po['lines']
                       if not ln['deliveries'] and not ln['schedule'] and not ln.get('no_receive') and ln['promise'] > A]
        open_future.sort(key=lambda x: x[1]['promise'])
        forced = 0
        if is_wd(self.A):
            for po, ln in open_future[:2]:
                ln['expected'] = self.A
                forced += 1
        # receipts
        events = defaultdict(list)
        for po in self.pos:
            for ln in po['lines']:
                for (d, qn, s) in ln['deliveries']:
                    events[(d, po['num'])].append((ln, qn, s))
        self.receipts = []
        rn = 7000
        lotseq = defaultdict(int)
        for (d, ponum) in sorted(events):
            rn += 1
            po = next(p for p in self.pos if p['num'] == ponum)
            rcv_by = r.choice(RECEIVERS)
            rts = ts(d, r, lo=(7, 0), hi=(15, 30))
            rc = dict(num=rn, po=ponum, date=d, by=rcv_by, ts=rts, lines=[], notes='')
            for (ln, qn, s) in events[(d, ponum)]:
                item = self.items[ln['item']]
                lot = None
                if item['lot_controlled'] == 'Y':
                    lotseq[ln['item']] += 1
                    lot = f"{po['vendor'][-2:]}{d:%y%m%d}{chr(64 + lotseq[ln['item']])}"
                ln['rcv'] += qn
                if s:
                    s['rcv'] += qn
                note = ''
                if ln['rcv'] < ln['qty'] and not s:
                    note = 'Short shipped -- balance on backorder'
                rc['lines'].append(dict(line=len(rc['lines']) + 1, ln=ln, qty=qn, lot=lot, notes=note))
                ln['last_rcv'] = rts
                if lot:
                    self.lot_receipts.setdefault(ln['item'], []).append(dict(lot=lot, date=d, qty=qn, ts=rts))
            self.receipts.append(rc)
        # statuses (rcventr rules)
        for po in self.pos:
            if po['status'] == 'DRAFT':
                for ln in po['lines']:
                    ln['status'] = 'OPEN'
                continue
            for ln in po['lines']:
                ln['status'] = 'CLOSED' if ln['rcv'] >= ln['qty'] else ('PARTIAL' if ln['rcv'] > 0 else 'OPEN')
            if all(ln['status'] == 'CLOSED' for ln in po['lines']):
                po['status'] = 'RECEIVED'
            elif any(ln['rcv'] > 0 for ln in po['lines']):
                po['status'] = 'PARTIAL'
            else:
                po['status'] = 'OPEN'

    # ------------------------------------------------------------- inventory
    def build_inventory(self):
        r = self.rng
        A = self.A
        open_qty = defaultdict(float)
        for po in self.pos:
            if po['status'] in ('OPEN', 'PARTIAL'):
                for ln in po['lines']:
                    if ln['uom'] == self.items[ln['item']]['inventory_uom']:
                        open_qty[ln['item']] += ln['qty'] - ln['rcv']
        self.open_qty = open_qty
        single = {i for i in self.items if sum(1 for (ii, v), p in self.iv.items() if ii == i and p['is_active'] == 'Y') == 1}
        items = sorted(i for i in self.items if self.items[i]['is_active'] == 'Y')
        no_open = [i for i in items if open_qty.get(i, 0) == 0 and i not in self.forced_stock]
        with_open = [i for i in items if open_qty.get(i, 0) > 0 and i not in self.forced_stock]
        arch = dict(self.forced_stock)
        low_single = [i for i in no_open if i in single][:]
        r.shuffle(low_single)
        low_other = [i for i in no_open if i not in single]
        r.shuffle(low_other)
        picks = low_single[:3] + low_other[:6]
        for i, a in zip(picks, ['critical', 'below_safety', 'below_rop', 'critical', 'below_safety',
                                'below_rop', 'below_rop', 'below_rop', 'below_rop']):
            arch[i] = a
        ws = sorted(with_open, key=lambda i: -open_qty[i])
        over = [i for i in ws if open_qty[i] >= self.items[i]['max_qty'] * 0.3][:3]
        for k, i in enumerate(over):
            arch[i] = 'over_max_on_hand' if k == 0 else 'overstock'
        for i in items:
            arch.setdefault(i, 'covered' if i in with_open else 'healthy')
        for i, it in self.items.items():
            rop, mx, ss, crit = it['reorder_point'], it['max_qty'], it['safety_stock'], it['critical_level']
            a = arch.get(i, 'inactive')
            oq = open_qty.get(i, 0)
            if a == 'inactive':
                oh = 4
            elif a == 'critical':
                oh = r.randint(0, int(crit))
            elif a == 'below_safety':
                oh = r.randint(int(crit) + 1, max(int(crit) + 1, int(ss)))
            elif a == 'below_rop':
                oh = r.randint(max(int(ss) + 1, int(rop * 0.6)), max(int(ss) + 1, int(rop)))
            elif a == 'covered':
                oh = r.randint(max(int(rop * 0.4), 1), max(int(rop * 1.1), 2))
                if oh + oq > mx:
                    oh = max(0, int(mx - oq - 1))
            elif a == 'over_max_on_hand':
                oh = int(mx * r.uniform(1.05, 1.2)) + 1
            elif a == 'overstock':
                oh = int(max(mx - oq, 0) + mx * r.uniform(0.15, 0.35))
            else:
                oh = r.randint(int(rop * 1.25) + 1, max(int(rop * 1.25) + 2, int(mx * 0.85)))
            it['arch'] = a
            it['qty_on_hand'] = float(oh)
            it['qty_frozen'] = 0.0
        # quality holds (frozen stock)
        for i, fz in (('SAF-010', 12), ('LUB-010', 10)):
            it = self.items[i]
            it['qty_on_hand'] = max(it['qty_on_hand'], fz + it['reorder_point'] + 5)
            it['qty_frozen'] = float(fz)
        for it in self.items.values():
            it['qty_available'] = it['qty_on_hand'] - it['qty_frozen']
            it['qty_on_order'] = open_qty.get(it['item_number'], 0.0)
        # planted: available not updated after the last receipt (rcventr only updates on-hand)
        recent = sorted([(ln['last_rcv'], ln['item'], q['qty']) for rc in self.receipts for q in rc['lines']
                         for ln in [q['ln']] if self.items[ln['item']]['lot_controlled'] == 'N'
                         and wd_between(rc['date'], on_or_before_wd(A)) <= 4], reverse=True)
        if recent:
            _, i, qn = recent[0]
            it = self.items[i]
            it['qty_on_hand'] = max(it['qty_on_hand'], qn + 5)
            it['qty_available'] = it['qty_on_hand'] - it['qty_frozen'] - qn
            self.exceptions.append(f'Available understated after receipt: {i} (by {qn:g})')
        it = self.items['CON-012']
        it['qty_available'] = 0.0
        self.exceptions.append('Stock invisible to orders: CON-012 (available 0)')
        # planted: stale on-order figures
        cands = [i for i in sorted(open_qty) if open_qty[i] > 0 and self.items[i]['lot_controlled'] == 'N'
                 and self.items[i].get('arch') not in ('over_max_on_hand', 'overstock')]
        if cands:
            self.items[cands[0]]['qty_on_order'] = 0.0
            self.exceptions.append(f'On-order not updated: {cands[0]} shows 0, POs have {open_qty[cands[0]]:g}')
        closed = [ln['item'] for po in self.pos for ln in po['lines'] if ln['status'] == 'CLOSED'
                  and open_qty.get(ln['item'], 0) == 0 and self.items[ln['item']]['lot_controlled'] == 'N']
        if closed:
            i = closed[-1]
            self.items[i]['qty_on_order'] = float(next(ln['qty'] for po in self.pos for ln in po['lines']
                                                     if ln['item'] == i and ln['status'] == 'CLOSED'))
            self.exceptions.append(f'On-order not cleared after receipt: {i}')
        # bins: one aisle per class, then a handful of planted shared / mixed-class bins
        per_class = defaultdict(int)
        for i in sorted(self.items):
            it = self.items[i]
            k = per_class[it['class_code']]
            per_class[it['class_code']] += 1
            it['aisle_code'] = AISLE_BY_CLASS[it['class_code']]
            it['bay_code'] = f'B{k // 2 + 1}'
            it['shelf_code'] = f'S{(k % 2) * 2 + 1 if it["class_code"] not in ("LUBRICANTS", "JANITORIAL") else (k % 2) + 1}'
        def put(i, j):
            self.items[i]['aisle_code'] = self.items[j]['aisle_code']
            self.items[i]['bay_code'] = self.items[j]['bay_code']
            self.items[i]['shelf_code'] = self.items[j]['shelf_code']
        put('LUB-008', 'CON-006')       # mixed: lubricant drum in the consumables aisle
        put('PKG-012', 'JAN-007')       # mixed: void fill parked with absorbent
        put('ELE-011', 'CON-011')       # mixed: heat shrink with cable ties
        put('FAS-012', 'FAS-011')       # same class, shared bin
        put('PLM-006', 'PLM-008')       # same class, shared bin
        self.exceptions.append('Shared bins: 5 (3 mixing classes)')

    def plant_sourcing_gaps(self):
        A = self.A
        r = self.rng
        # items set up recently with no approved vendor at all
        for (num, desc, short, cls, rop, crit, ss, mx, oh) in (
                ('PLM-009', 'Water Heater Element 4500W 240V', 'WH Element 4500W', 'PLUMBING', 4, 1, 2, 16, 1),
                ('SAF-013', 'Arc Flash Face Shield 12 cal', 'Arc Flash Shield', 'SAFETY', 3, 1, 1, 10, 0)):
            k = sum(1 for it in self.items.values() if it['class_code'] == cls)
            self.items[num] = dict(item_number=num, item_description=desc, short_description=short, class_code=cls,
                                   inventory_uom='EA', stocking_uom='EA', lot_controlled='N', reorder_point=rop,
                                   critical_level=crit, safety_stock=ss, min_qty=1, max_qty=mx, lead_time_days=14,
                                   is_active='Y', shelf_life=None, created=add_wd(A, -12), arch='no_vendor',
                                   qty_on_hand=float(oh), qty_available=float(oh), qty_frozen=0.0, qty_on_order=0.0,
                                   aisle_code=AISLE_BY_CLASS[cls], bay_code=f'B{k // 2 + 2}', shelf_code='S2')
            self.exceptions.append(f'Item with no approved vendor: {num} (on hand {oh}, reorder point {rop})')
        # preferred vendor removed (item still has approved vendors, but nobody is preferred)
        for i in ('CON-008', 'ELE-008'):
            for (ii, v), p in self.iv.items():
                if ii == i:
                    p['is_preferred'] = 'N'
            self.exceptions.append(f'Item with no preferred vendor: {i}')
        # preferred vendor has been deactivated
        for (ii, v), p in self.iv.items():
            if ii == 'FAS-005':
                p['is_preferred'] = 'Y' if v == 'VND20' else 'N'
        self.exceptions.append('Preferred vendor is inactive: FAS-005 (VND20 Northern Hardware)')
        # vendor master gaps
        self.vendors['VND13']['email_address'] = ''
        self.vendors['VND17']['tax_id'] = ''
        self.exceptions.append('Vendor master gaps: VND13 no email, VND17 no tax ID, VND19 no email or tax ID')

    def build_lots(self):
        r = self.rng
        A = self.A
        self.lots = []
        planted_nolots = ('JAN-006', 'PLM-005')
        planted_short = {'LUB-010': 18, 'SAF-012': 7}
        planted_over = {'CON-009': 9, 'LUB-013': 12}
        for i, it in sorted(self.items.items()):
            if it['lot_controlled'] != 'Y':
                continue
            if i in planted_nolots:
                self.exceptions.append(f'Lot-controlled with no lots on file: {i}')
                continue
            life = it['shelf_life']
            rec = sorted(self.lot_receipts.get(i, []), key=lambda x: x['date'])
            opening_d = on_or_before_wd(self.S - dt.timedelta(days=r.randint(20, 70)))
            opening = dict(lot=f'OB{opening_d:%y%m%d}', date=opening_d, qty=max(it['max_qty'] * 0.6, 10), ts=ts(opening_d, r))
            lots = [opening] + rec
            remaining = it['qty_on_hand']
            if i in planted_short:
                remaining = max(it['qty_on_hand'] - planted_short[i], 0)
                self.exceptions.append(f'Lots short of on-hand: {i} ({planted_short[i]} units)')
            if i in planted_over:
                remaining = it['qty_on_hand'] + planted_over[i]
                self.exceptions.append(f'Lots exceed on-hand: {i} ({planted_over[i]} units)')
            alloc = []
            for L in reversed(lots):                      # FIFO consumption: newest lots still full
                take = min(remaining, L['qty'])
                if take > 0:
                    alloc.append((L, take))
                    remaining -= take
            if remaining > 0 and alloc:
                L, take = alloc[-1]
                alloc[-1] = (L, take + remaining)
            for L, take in alloc:
                exp = add_months(L['date'], life) if life else None
                self.lots.append(dict(item=i, lot=L['lot'], qty=float(take), rcvd=L['date'], exp=exp, ts=L['ts']))
        # planted: lots about to expire, expired lots still holding stock, a lot with no expiry date
        olds = [l for l in self.lots if l['lot'].startswith('OB') and l['exp']]
        for k, days in enumerate((19, 9, 27, 4, -6, -21, -48)):
            if k >= len(olds):
                break
            L = olds[k]
            L['exp'] = A + dt.timedelta(days=days)
            L['rcvd'] = min(L['rcvd'], L['exp'] - dt.timedelta(days=200))
            self.exceptions.append(f"{'Lot expiring in %d days' % days if days > 0 else 'Expired lot with stock (%d days ago)' % -days}: "
                                   f"{L['item']} {L['lot']}")
        nexp = [l for l in self.lots if l['exp'] and not l['lot'].startswith('OB')]
        if nexp:
            nexp[0]['exp'] = None
            self.exceptions.append(f"Lot with no expiry date on a shelf-life item: {nexp[0]['item']} {nexp[0]['lot']}")
        # reconciliation history
        self.recon = []
        lot_items = sorted(i for i, it in self.items.items() if it['lot_controlled'] == 'Y'
                           and i not in planted_nolots and i not in planted_short and i not in planted_over)
        for off, who, note in ((-61, 'CODERFLOW', 'Auto-repaired: lot is source of truth'),
                               (-44, 'KOBRIEN', 'Cycle count adjustment after bin audit'),
                               (-31, 'CODERFLOW', 'Auto-repaired: lot is source of truth'),
                               (-12, 'CODERFLOW', 'Auto-repaired: lot is source of truth'),
                               (-5, 'TSANTOS', 'Cycle count adjustment -- damaged units removed')):
            i = r.choice(lot_items)
            lot_items.remove(i)
            before = float(r.randint(20, 140))
            lotsum = before - r.choice([-6, -4, 3, 5, 8, 12])
            self.recon.append(dict(item=i, ts=ts(add_wd(on_or_before_wd(A), off), r), before=before,
                                   lotsum=max(lotsum, 0), after=max(lotsum, 0), by=who, notes=note))

    # ------------------------------------------------------------------ SQL
    def sql(self):
        A = self.A
        out = []
        w = out.append
        w('-- ---------------------------------------------------------------------------')
        w('-- Seed:    030_acme_demo_data')
        w('-- Module:  perp')
        w('-- Purpose: Full reseed of the ACME Distribution demo company (ACM).')
        w('--          Deletes every ACM-scoped row, then reloads master data,')
        w('--          sourcing, price history, lots, requisitions, POs, schedules,')
        w('--          receipts and the reconciliation log, and resets the REQ / PO /')
        w('--          RCP document sequences. company, company_config,')
        w('--          warehouse_layout, code_master and uom are left untouched;')
        w('--          perp_user only gains rows.')
        w(f'-- Anchor:  {A.isoformat()}  (GENERATED by perp/tools/acme_seed/gen_acme_seed.py')
        w('--          -- do not hand-edit; re-run the generator to refresh the dates)')
        w('-- Run:     perp/tools/acme_seed/load.sh  (RUNSQLSTM COMMIT(*CHG))')
        w('-- ---------------------------------------------------------------------------')
        w('')
        for t in ('reconciliation_log', 'po_receipt_line', 'po_receipt', 'po_line_schedule', 'po_line', 'po_header',
                  'requisition_line', 'requisition_header', 'item_lot', 'item_vendor_price', 'item_vendor',
                  'item_uom_conversion', 'item', 'item_class', 'vendor'):
            w(f"DELETE FROM {LIB}.{t} WHERE company_code = '{CO}';")
        w('')
        for (u, n, e, role) in NEW_USERS:
            w(f"INSERT INTO {LIB}.perp_user (user_code, display_name, email_address, role_code, role_type)")
            w(f"  SELECT {q(u)}, {q(n)}, {q(e)}, {q(role)}, 'USERROLE' FROM SYSIBM.SYSDUMMY1")
            w(f"   WHERE NOT EXISTS (SELECT 1 FROM {LIB}.perp_user WHERE user_code = {q(u)});")
        w('')
        cr = ts(add_months(A, -24, 2), self.rng)
        self.insert(out, 'item_class', ['company_code', 'class_code', 'description', 'created_at', 'created_by', 'updated_at', 'updated_by'],
                    [(CO, c, dsc, cr, 'AIDEMO', cr, 'AIDEMO') for c, dsc in self.classes])
        vrows = []
        for v in sorted(self.vendors.values(), key=lambda v: v['vendor_code']):
            c = ts(v['created'], self.rng)
            u = c if v['is_active'] == 'Y' else ts(add_months(A, -4, 2), self.rng)
            vrows.append((CO, v['vendor_code'], v['vendor_name'], v['address_line1'], v.get('address_line2', ''),
                          v['city_name'], v['state_code'], v['postal_code'], v.get('country_code', 'US'),
                          v['phone_number'], v['email_address'], v['contact_name'], v['buyer_code'],
                          v['payment_terms_code'], 'PAYTERMS', v['tax_id'], c, v['buyer_code'], u, v['buyer_code'], v['is_active']))
        self.insert(out, 'vendor', ['company_code', 'vendor_code', 'vendor_name', 'address_line1', 'address_line2', 'city_name',
                                    'state_code', 'postal_code', 'country_code', 'phone_number', 'email_address', 'contact_name',
                                    'buyer_code', 'payment_terms_code', 'payment_terms_type', 'tax_id', 'created_at', 'created_by',
                                    'updated_at', 'updated_by', 'is_active'], vrows)
        irows = []
        last_rcv = {}
        for rc in self.receipts:
            for l in rc['lines']:
                last_rcv[l['ln']['item']] = rc['ts']
        for i in sorted(self.items):
            it = self.items[i]
            c = ts(it['created'], self.rng)
            u = last_rcv.get(i, c)
            irows.append((CO, i, it['item_description'], it['short_description'], it['class_code'], it['inventory_uom'],
                          it['stocking_uom'], it['lot_controlled'], it['qty_on_hand'], it['qty_available'], it['qty_frozen'],
                          it['qty_on_order'], it['reorder_point'], it['critical_level'], it['min_qty'], it['max_qty'],
                          it['safety_stock'], it['lead_time_days'], it['aisle_code'], it['bay_code'], it['shelf_code'],
                          c, 'LCHEN', u, 'TSANTOS' if i in last_rcv else 'LCHEN', it['is_active']))
        self.insert(out, 'item', ['company_code', 'item_number', 'item_description', 'short_description', 'class_code', 'inventory_uom',
                                  'stocking_uom', 'lot_controlled', 'qty_on_hand', 'qty_available', 'qty_frozen', 'qty_on_order',
                                  'reorder_point', 'critical_level', 'min_qty', 'max_qty', 'safety_stock', 'lead_time_days',
                                  'aisle_code', 'bay_code', 'shelf_code', 'created_at', 'created_by', 'updated_at', 'updated_by',
                                  'is_active'], irows)
        crow = []
        for (i, f, t, fac) in self.uomconv:
            c = ts(self.items[i]['created'], self.rng)
            crow.append((CO, i, f, t, float(fac), c, 'LCHEN', c, 'LCHEN'))
        self.insert(out, 'item_uom_conversion', ['company_code', 'item_number', 'from_uom', 'to_uom', 'conversion_factor',
                                                 'created_at', 'created_by', 'updated_at', 'updated_by'], crow)
        ivrows = []
        for (i, v) in sorted(self.iv):
            p = self.iv[(i, v)]
            c = ts(max(self.items[i]['created'], self.vendors[v]['created']), self.rng)
            ivrows.append((CO, i, v, p.get('vendor_part_number') or f'{i}-{v}', p['lead_time_days'], p['moq'], p['pack_size'],
                           p['is_preferred'], c, self.vendors[v]['buyer_code'], c, self.vendors[v]['buyer_code'], p['is_active']))
        self.insert(out, 'item_vendor', ['company_code', 'item_number', 'vendor_code', 'vendor_part_number', 'lead_time_days', 'moq',
                                         'pack_size', 'is_preferred', 'created_at', 'created_by', 'updated_at', 'updated_by',
                                         'is_active'], ivrows)
        prow = []
        for (i, v) in sorted(self.prices):
            for h in sorted(self.prices[(i, v)], key=lambda h: h['ef']):
                c = ts(on_or_before_wd(h.get('announced') or (h['ef'] - dt.timedelta(days=self.rng.randint(1, 10)))), self.rng)
                prow.append((CO, i, v, h['ef'], h['et'], h['price'], 'USD', 'CURRENCY', h['src'], c,
                             self.vendors[v]['buyer_code'], c, self.vendors[v]['buyer_code']))
        self.insert(out, 'item_vendor_price', ['company_code', 'item_number', 'vendor_code', 'effective_from', 'effective_to',
                                               'unit_price', 'currency_code', 'currency_type', 'price_source', 'created_at',
                                               'created_by', 'updated_at', 'updated_by'], prow)
        lrow = [(CO, l['item'], l['lot'], l['qty'], l['rcvd'], l['exp'], l['ts'], 'TSANTOS', l['ts'], 'TSANTOS') for l in self.lots]
        self.insert(out, 'item_lot', ['company_code', 'item_number', 'lot_number', 'qty_on_hand', 'received_date', 'expiry_date',
                                      'created_at', 'created_by', 'updated_at', 'updated_by'], lrow)
        hrow, rlrow = [], []
        for qd in self.reqs:
            hrow.append((CO, qd['num'], qd['by'], qd['date'], qd['need_by'], qd['pri'], 'PRIORITY', qd['status'], 'REQSTATUS',
                         qd['conf'], qd['approved_by'], qd['approved_at'], qd['src'], 'APPRSRC' if qd['src'] else None,
                         qd['notes_apr'], qd['total'], 'USD', 'CURRENCY', '', qd['created'], qd['by'], qd['updated'],
                         qd['approved_by'] if qd['approved_by'] and qd['approved_by'] != 'CODERFLOW' else qd['by']))
            for l in qd['lines']:
                rlrow.append((CO, qd['num'], l['line'], l['item'], float(l['qty']), l['uom'], l['cost'], None, l['notes'],
                              qd['created'], qd['by'], qd['created'], qd['by']))
        self.insert(out, 'requisition_header', ['company_code', 'requisition_number', 'requested_by', 'request_date', 'need_by_date',
                                                'priority_code', 'priority_type', 'status_code', 'status_type', 'confidence_pct',
                                                'approved_by', 'approved_at', 'approval_source_code', 'approval_source_type',
                                                'approval_notes', 'total_estimated_cost', 'currency_code', 'currency_type', 'notes',
                                                'created_at', 'created_by', 'updated_at', 'updated_by'], hrow)
        self.insert(out, 'requisition_line', ['company_code', 'requisition_number', 'line_number', 'item_number', 'quantity', 'uom_code',
                                              'est_unit_cost', 'need_by_date', 'notes', 'created_at', 'created_by', 'updated_at',
                                              'updated_by'], rlrow)
        phrow, plrow, srow = [], [], []
        for po in self.pos:
            tot = round(sum(ln['qty'] * ln['price'] for ln in po['lines']), 2)
            upd = max([ln.get('last_rcv') or po['created'] for ln in po['lines']] or [po['created']])
            phrow.append((CO, po['num'], po['vendor'], po['buyer'], po['date'], po['status'], 'POSTATUS', 'USD', 'CURRENCY', tot,
                          po['notes'], po['created'], po['buyer'], upd, po['buyer'] if upd == po['created'] else 'TSANTOS'))
            for ln in po['lines']:
                lu = ln.get('last_rcv') or po['created']
                plrow.append((CO, po['num'], ln['line'], ln['item'], float(ln['qty']), float(ln['rcv']), ln['uom'], ln['price'],
                              ln['expected'], ln['status'], 'POSTATUS', ln['src'][0] if ln['src'] else None,
                              ln['src'][1] if ln['src'] else None, po['created'], po['buyer'], lu,
                              po['buyer'] if lu == po['created'] else 'TSANTOS'))
                for s in ln['schedule'] or []:
                    srow.append((CO, po['num'], ln['line'], s['seq'], s['date'], float(s['qty']), float(s['rcv']), s['notes'],
                                 po['created'], po['buyer'], ln.get('last_rcv') or po['created'], po['buyer']))
        self.insert(out, 'po_header', ['company_code', 'po_number', 'vendor_code', 'buyer_code', 'order_date', 'status_code', 'status_type',
                                       'currency_code', 'currency_type', 'total_amount', 'notes', 'created_at', 'created_by',
                                       'updated_at', 'updated_by'], phrow)
        self.insert(out, 'po_line', ['company_code', 'po_number', 'line_number', 'item_number', 'ordered_qty', 'received_qty', 'uom_code',
                                     'unit_price', 'expected_receipt_date', 'status_code', 'status_type', 'source_requisition_number',
                                     'source_requisition_line_number', 'created_at', 'created_by', 'updated_at', 'updated_by'], plrow)
        self.insert(out, 'po_line_schedule', ['company_code', 'po_number', 'line_number', 'schedule_seq', 'scheduled_date', 'scheduled_qty',
                                              'received_qty', 'notes', 'created_at', 'created_by', 'updated_at', 'updated_by'], srow)
        rrow, rlr = [], []
        for rc in self.receipts:
            rrow.append((CO, rc['num'], rc['po'], rc['date'], rc['by'], 'POSTED', 'RCPSTATUS', rc['notes'], rc['ts'], rc['by'],
                         rc['ts'], rc['by']))
            for l in rc['lines']:
                ln = l['ln']
                rlr.append((CO, rc['num'], l['line'], rc['po'], ln['line'], ln['item'], float(l['qty']), float(l['qty']), 1.0,
                            l['lot'], l['notes'], rc['ts'], rc['by'], rc['ts'], rc['by']))
        self.insert(out, 'po_receipt', ['company_code', 'receipt_number', 'po_number', 'receipt_date', 'received_by', 'status_code',
                                        'status_type', 'notes', 'created_at', 'created_by', 'updated_at', 'updated_by'], rrow)
        self.insert(out, 'po_receipt_line', ['company_code', 'receipt_number', 'line_number', 'po_number', 'po_line_number',
                                             'item_number', 'qty_received_vendor_uom', 'qty_received_inventory_uom',
                                             'uom_conversion_factor', 'lot_number', 'notes', 'created_at', 'created_by',
                                             'updated_at', 'updated_by'], rlr)
        self.insert(out, 'reconciliation_log', ['company_code', 'item_number', 'run_timestamp', 'item_qty_before', 'lot_sum_before',
                                                'qty_after', 'reconciled_by', 'notes', 'created_at', 'created_by', 'updated_at',
                                                'updated_by'],
                    [(CO, x['item'], x['ts'], x['before'], x['lotsum'], x['after'], x['by'], x['notes'], x['ts'],
                      'AIDEMO', x['ts'], 'AIDEMO') for x in self.recon])
        w('')
        for dtype, val in (('REQ', max(q_['num'] for q_ in self.reqs)), ('PO', max(p['num'] for p in self.pos)),
                           ('RCP', max([rc['num'] for rc in self.receipts] or [7000]))):
            w(f"UPDATE {LIB}.document_sequence SET current_number = {val}, updated_at = CURRENT_TIMESTAMP, updated_by = USER")
            w(f"  WHERE company_code = '{CO}' AND document_type = '{dtype}';")
        w('')
        return '\n'.join(out) + '\n'

    @staticmethod
    def insert(out, table, cols, rows, batch=40):
        for k in range(0, len(rows), batch):
            chunk = rows[k:k + batch]
            out.append(f"INSERT INTO {LIB}.{table} ({', '.join(cols)}) VALUES")
            out.append(',\n'.join('  (' + ', '.join(sqlv(v) for v in row) + ')' for row in chunk) + ';')
            out.append('')

    # ------------------------------------------------------------- checks
    def validate(self):
        errs = []
        for po in self.pos:
            for ln in po['lines']:
                if ln['rcv'] > ln['qty']:
                    errs.append(f"PO {po['num']}/{ln['line']} over-received")
                for s in ln['schedule'] or []:
                    if s['rcv'] > s['qty']:
                        errs.append(f"PO {po['num']} schedule {s['seq']} over-received")
        for it in self.items.values():
            for k in ('qty_on_hand', 'qty_available', 'qty_frozen', 'qty_on_order'):
                if it[k] < 0:
                    errs.append(f"{it['item_number']} {k} negative")
            if it['max_qty'] < it['min_qty']:
                errs.append(f"{it['item_number']} max<min")
        prefs = defaultdict(int)
        for (i, v), p in self.iv.items():
            if p['is_preferred'] == 'Y':
                prefs[i] += 1
        gaps = {'PLM-009', 'SAF-013', 'CON-008', 'ELE-008'}
        for i in self.items:
            if prefs[i] != 1 and i not in gaps:
                errs.append(f'{i} has {prefs[i]} preferred vendors')
        for (i, v), hist in self.prices.items():
            for h in hist:
                if h['et'] and h['et'] < h['ef']:
                    errs.append(f'{i}/{v} price window inverted')
        for (i, v) in self.iv:                      # alternate prices must be in the same league as the preferred one
            if i in gaps or self.vendors[v]['is_active'] == 'N' or self.iv[(i, v)]['is_preferred'] == 'Y':
                continue
            pv = self.preferred(i)
            mine, base = self.vprice(i, v, self.A), self.vprice(i, pv, self.A)
            if base and not 0.4 <= mine / base <= 2.5:
                errs.append(f'{i}/{v} price {mine} vs preferred {base}')
        dates = [q_['date'] for q_ in self.reqs] + [p['date'] for p in self.pos] + [rc['date'] for rc in self.receipts]
        if any(not is_wd(d) for d in dates):
            errs.append('weekend transaction date')
        if errs:
            raise SystemExit('validation failed:\n  ' + '\n  '.join(errs))

    def summary(self):
        from collections import Counter
        c = dict(items=len(self.items), vendors=len(self.vendors), item_vendor=len(self.iv),
                 prices=sum(len(h) for h in self.prices.values()), lots=len(self.lots), reqs=len(self.reqs),
                 req_lines=sum(len(q_['lines']) for q_ in self.reqs), pos=len(self.pos),
                 po_lines=sum(len(p['lines']) for p in self.pos),
                 schedules=sum(len(ln['schedule'] or []) for p in self.pos for ln in p['lines']),
                 receipts=len(self.receipts), receipt_lines=sum(len(rc['lines']) for rc in self.receipts), recon=len(self.recon))
        c['req_status'] = dict(Counter(q_['status'] for q_ in self.reqs))
        c['po_status'] = dict(Counter(p['status'] for p in self.pos))
        c['awaiting_po'] = self.awaiting
        c['item_archetypes'] = dict(Counter(it.get('arch', 'inactive') for it in self.items.values()))
        c['planted'] = self.exceptions
        return c


def q(s):
    return "'" + str(s).replace("'", "''") + "'"


def sqlv(v):
    if v is None:
        return 'NULL'
    if isinstance(v, dt.datetime):
        return "'" + v.strftime('%Y-%m-%d-%H.%M.%S') + ".000000'"
    if isinstance(v, dt.date):
        return "'" + v.isoformat() + "'"
    if isinstance(v, bool):
        return "'Y'" if v else "'N'"
    if isinstance(v, int):
        return str(v)
    if isinstance(v, float):
        s = f'{v:.4f}'.rstrip('0').rstrip('.')
        return s if s not in ('', '-0') else '0'
    return q(v)


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument('--as-of', help='anchor date YYYY-MM-DD (default: today)')
    ap.add_argument('--out', default=DEFAULT_OUT)
    a = ap.parse_args()
    anchor = dt.date.fromisoformat(a.as_of) if a.as_of else dt.date.today()
    TODAY_WINDOW['date'] = anchor
    g = Gen(anchor)
    g.lot_receipts = {}
    g.build_master()
    g.build_requisitions()
    g.build_pos()
    g.simulate_deliveries()
    g.build_inventory()
    g.plant_sourcing_gaps()
    g.build_lots()
    g.validate()
    with open(a.out, 'w') as f:
        f.write(g.sql())
    print(json.dumps(g.summary(), indent=1, default=str))
    print('wrote', a.out)


if __name__ == '__main__':
    main()
