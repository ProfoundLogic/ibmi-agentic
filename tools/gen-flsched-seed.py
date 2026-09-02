#!/usr/bin/env python3
"""Generate the A4 schedule-board seed data (GJA-917).

Writes cfdemo/seed/flsched.sql: work centres, machine build work orders,
routing operations and material shortages. Deterministic - same input, same
file - so the SQL can be committed and reviewed as source.

Two things about this data are deliberate and worth reading before changing it.

1. It joins to the existing Fletcher demo data by key. Modernization and
   rebuild orders reference serials that really exist in FLMACHP, so a machine
   on the Fleet 360 installed base is the same machine that shows up in the
   shop. New builds take serials above the current maximum.

2. **It is deliberately dirty.** The board carries a data-confidence panel
   whose entire job is to report where the schedule cannot be trusted, which
   is only honest if the defects are real and the panel finds them by
   querying rather than by being told. Every defect planted below is listed
   in DEFECTS with the finding it should produce, and the scan in FLDATA is
   written against the data, not against this list. Fletcher's own words:
   "we're getting into actually using the data in the system to plug into
   scheduling applications and we're finding a lot of database issues."

  tools/gen-flsched-seed.py > cfdemo/seed/flsched.sql
"""
import datetime as dt
import random
import sys

TODAY = dt.date(2026, 9, 1)
RNG = random.Random(20260901)

# ---------------------------------------------------------------------------
# Reference data already seeded by flseed.sql
# ---------------------------------------------------------------------------
MODELS = {
    "CHDDR-4":    ("Four Head Roof Drill",    "RBC"),
    "DDO-15-A":   ("Twin Boom Drill Jumbo",   "DJ"),
    "HDDR":       ("Twin Boom Roof Drill",    "RBC"),
    "J352-LS":    ("Low Seam Roof Bolter",    "RBC"),
    "N3114-AD/E": ("Narrow Vein Roof Bolter", "RBM"),
    "PML-88":     ("Powder Loader",           "PL"),
    "RRII-13":    ("Mobile Roof Support",     "MRS"),
    "SCL-200":    ("Scaling Machine",         "SCL"),
}
# (customer, site) pairs
CUST_SITES = [
    (100001, 200001), (100001, 200002), (100002, 200003), (100002, 200004),
    (100002, 200005), (100003, 200006), (100003, 200007), (100004, 200008),
    (100005, 200009), (100005, 200010), (100006, 200011), (100007, 200012),
    (100008, 200013), (100008, 200014),
]
# Parts used for shortages - real FLPARTP rows, long-lead and high-value,
# which is what actually holds a build up.
SHORT_PARTS = [
    ("BM-CYL-96",       "BOOM CYLINDER 96",           30),
    ("ELB-FTG-1IN",     "ELBOW FITTING 1IN",          30),
    ("MOTOR-TRM-6SPEE", "MOTOR, TRAM, 6 SPEED",       10),
    ("CHUCK-DRL-200",   "CHUCK, DRILL, 200 SER",       5),
    ("ATRSJCKSTANDA",   "ATRS JACK STANDARD",         14),
    ("PUMP-HYD-A2",     "PUMP, HYDRAULIC, A2",         5),
    ("CTLCBL45FT",      "CTL CABLE 45 FOOT",          14),
    ("OPPNLRIGHTH",     "OP PANEL RIGHT HAND",         5),
    ("SOLEN-PLT-24V",   "SOLENOID, PILOT, 24V",       14),
    ("DRV-SPK-24TOOT",  "DRIVE SPROCKET 24 TOOTH",    10),
    ("HP-HOSE-60FT",    "HIGH PRESSURE HOSE 60FT",     5),
    ("THRBRG50MM",      "THR BEARING 50MM",            5),
    ("JMB-RL-8FT",      "JUMBO RAIL 8FT",             21),
    ("SEAL-CNP-8IN",    "SEAL, CANOPY, 8IN",          14),
    ("BRAKE-DSC-9IN",   "BRAKE, DISC, 9IN",           14),
]
VENDORS = [
    "Kanawha Hydraulics Inc", "Tri-State Bearing Supply", "Ohio Valley Steel",
    "Appalachian Fluid Power", "Midland Electric Controls",
    "Guyandotte Machine Works", "Blue Ridge Fasteners",
]

# ---------------------------------------------------------------------------
# Work centres. Capacity is crew x shifts x 8, which the load calculation
# divides remaining routed hours by.
# ---------------------------------------------------------------------------
WCTRS = [
    ("TEAR",  "Teardown & Inspection",     5, 4,  1, "A"),
    ("FAB",   "Fabrication & Weldment",   10, 9,  2, "A"),
    ("WELD2", "Fabrication Bay 2 (idle)", 15, 0,  0, "I"),
    ("MACH",  "Machining",                20, 6,  2, "A"),
    ("BLAST", "Blast & Paint",            30, 4,  1, "A"),
    ("HYD",   "Hydraulic Assembly",       40, 7,  1, "A"),
    ("ELEC",  "Electrical & Controls",    50, 5,  1, "A"),
    ("ASSY",  "Final Assembly",           60, 12, 1, "A"),
    ("TEST",  "Test & Commissioning",     70, 4,  1, "A"),
    ("CERT",  "QA & MSHA Certification",  80, 3,  1, "A"),
    ("SHIP",  "Ship Preparation",         90, 3,  1, "A"),
]

# Routing template per order type: (work centre, operation text, share of the
# order's total standard hours). Shares sum to 1.0 in each template.
ROUTINGS = {
    "N": [("FAB",   "Frame & canopy weldment",        0.21),
          ("MACH",  "Machine frame, boom pin bores",  0.13),
          ("BLAST", "Blast, prime & finish paint",    0.07),
          ("HYD",   "Hydraulic manifold & cylinders", 0.15),
          ("ELEC",  "Wiring harness & control panel", 0.12),
          ("ASSY",  "Final assembly & boom fit",      0.19),
          ("TEST",  "Function & pressure test",       0.06),
          ("CERT",  "MSHA approval & cert tags",      0.04),
          ("SHIP",  "Knock down, crate & load",       0.03)],
    "M": [("TEAR",  "Teardown, clean & inspect",      0.14),
          ("MACH",  "Reman boom & feed components",   0.16),
          ("BLAST", "Blast, prime & finish paint",     0.08),
          ("HYD",   "Rebuild hydraulic circuit",      0.18),
          ("ELEC",  "New harness & control upgrade",  0.14),
          ("ASSY",  "Reassemble to current spec",     0.18),
          ("TEST",  "Function & pressure test",       0.06),
          ("CERT",  "Recertification & new tags",     0.04),
          ("SHIP",  "Crate & load",                   0.02)],
    "R": [("TEAR",  "Core teardown & evaluation",     0.18),
          ("MACH",  "Machine wear surfaces",          0.18),
          ("BLAST", "Blast & paint",                  0.10),
          ("HYD",   "Reseal cylinders & valves",      0.22),
          ("ASSY",  "Reassemble exchange unit",       0.21),
          ("TEST",  "Function test",                  0.08),
          ("SHIP",  "Crate & load",                   0.03)],
    "E": [("FAB",   "Structural fabrication",         0.30),
          ("MACH",  "Machine mounting interfaces",    0.16),
          ("BLAST", "Blast & industrial coating",     0.12),
          ("ASSY",  "Assemble & fit hydraulics",      0.28),
          ("TEST",  "Function test & sign off",       0.09),
          ("SHIP",  "Crate & load",                   0.05)],
}
TYPE_HOURS = {"N": (980, 1680), "M": (420, 760), "R": (240, 460), "E": (300, 620)}
TYPE_DESC = {
    "N": ["New build to customer spec", "New build - low seam configuration",
          "New build - dual boom option", "New build - metal/non-metal package"],
    "M": ["Full modernization", "Partial modernization - drill & boom",
          "Full modernization with cert renewal"],
    "R": ["Exchange unit rebuild", "Core rebuild - trade-in unit",
          "Boom & feed exchange rebuild"],
    "E": ["Engineered Solutions - grit blaster", "Engineered Solutions - sand spreader",
          "Engineered Solutions - ladle platform"],
}
# The scheduler's sticky notes, carried over verbatim from the physical board.
# Scoped by order type: one pool put "Partial mod only - customer declined
# full" on a new build, and a note that contradicts the order it is stuck to is
# exactly what a Fletcher engineer spots in the first ten seconds.
NOTES_ANY = [
    "Customer flying in for FAT - do not slip TEST",
    "Moved ahead of 500118 per Rod, mine section down",
    "Waiting on cert paperwork from engineering",
    "Hold for customer paint spec confirmation",
    "Ship with 500131 on the same truck",
    "Cannon unit - parts numbering still being reconciled",
    "",
    "",
]
NOTES_BY_TYPE = {
    # FAB and export crating only make sense where the routing has them
    "N": ["Second shift approved on FAB through month end",
          "Distributor order, needs export crating",
          "Rush - replaces unit lost in roof fall",
          "Boom came back out of tolerance - remachine"],
    "M": ["Partial mod only - customer declined full",
          "Boom came back out of tolerance - remachine",
          "Customer wants the old cert tags returned"],
    "R": ["Trade-in core arrived short, see shortages",
          "Core teardown found frame cracks - quoting extra",
          "Rush - replaces unit lost in roof fall"],
    "E": ["Second shift approved on FAB through month end",
          "Non-mining unit - no MSHA package required",
          "Distributor order, needs export crating"],
}

# Deliberate defects, keyed by work order index in ORDERS below. Each entry is
# the finding the data-confidence scan is expected to produce from it.
DEFECTS = {}


def wd_add(d, days):
    """Add working days, skipping weekends."""
    n = 0
    while n < days:
        d += dt.timedelta(days=1)
        if d.weekday() < 5:
            n += 1
    return d


def ymd(d):
    return 0 if d is None else int(d.strftime("%Y%m%d"))


def q(s):
    return "'" + str(s).replace("'", "''") + "'"


# Character column widths, from the DDS. DB2 rejects an over-long literal with
# "Value for column or variable X too long" and names no row, so the check
# happens here, where the offending string is still in hand.
WIDTHS = {
    "FLWCTR": 6, "FLWCDESC": 40, "FLWSER": 8, "FLWMODL": 10, "FLWTYPE": 1,
    "FLWSTAT": 1, "FLWPRIO": 1, "FLWWCTR": 6, "FLWDESC": 40, "FLWNOTE": 60,
    "FLWCSTAT": 1, "FLRWCTR": 6, "FLRDESC": 40, "FLRSTAT": 1, "FLSHPART": 15,
    "FLSHSTAT": 1, "FLSHPO": 8, "FLSHVEND": 30,
}
_toolong = []


def qc(col, s):
    """Quote a character value, asserting it fits its column."""
    s = "" if s is None else str(s)
    if len(s) > WIDTHS[col]:
        _toolong.append(f"{col}: {len(s)} > {WIDTHS[col]} chars: {s!r}")
    return q(s)


# ---------------------------------------------------------------------------
# Work orders
# ---------------------------------------------------------------------------
# (type, status, count) - the shape of a shop with a backlog it cannot get to.
MIX = [("N", "C", 5), ("M", "C", 2), ("R", "C", 1),
       ("N", "I", 8), ("M", "I", 3), ("R", "I", 2), ("E", "I", 1),
       ("N", "R", 6), ("M", "R", 3), ("R", "R", 2), ("E", "R", 1),
       ("N", "P", 5), ("M", "P", 2), ("E", "P", 1),
       ("N", "H", 1), ("R", "H", 1)]

orders, routing, shortages = [], [], []
next_serial = 4566
wo_no = 500101
existing_serials = None  # filled by caller via --serials, else generated

# Serials already on the installed base, used by modernization and rebuild
# orders so the shop and the fleet are the same machines.
INSTALLED = ["FL004%03d" % n for n in range(110, 566, 11)]


def pick_model(otype):
    if otype == "E":
        return "SCL-200"          # closest thing to non-mining fabrication
    if otype in ("M", "R"):
        return RNG.choice(["J352-LS", "HDDR", "RRII-13", "PML-88", "CHDDR-4"])
    return RNG.choice(["CHDDR-4", "HDDR", "N3114-AD/E", "DDO-15-A", "SCL-200"])


idx = 0
for otype, status, count in MIX:
    for _ in range(count):
        idx += 1
        cust, site = RNG.choice(CUST_SITES)
        model = pick_model(otype)
        total_hrs = RNG.uniform(*TYPE_HOURS[otype])
        prio = RNG.choices("HML", weights=[2, 5, 3])[0]
        desc = RNG.choice(TYPE_DESC[otype])
        note = RNG.choice(NOTES_ANY + NOTES_BY_TYPE[otype])

        if otype in ("M", "R"):
            serial = INSTALLED[idx % len(INSTALLED)]
        elif status in ("P",):
            serial = ""                      # not assigned until release
        else:
            serial = "FL%06d" % next_serial
            next_serial += 1

        # Dates. Lead time scales with content, so a new build is 5-8 months
        # and a rebuild is 6-10 weeks.
        lead = int(total_hrs / RNG.uniform(7.5, 11.0)) + RNG.randint(10, 30)
        if status == "C":
            comp = TODAY - dt.timedelta(days=RNG.randint(12, 150))
            start = comp - dt.timedelta(days=int(lead * RNG.uniform(0.75, 1.05)))
            open_d = start - dt.timedelta(days=RNG.randint(20, 70))
            prom = comp + dt.timedelta(days=RNG.randint(-9, 14))
            pct_routed_target = 1.0
        elif status == "I":
            start = TODAY - dt.timedelta(days=RNG.randint(15, int(lead * 0.85)))
            open_d = start - dt.timedelta(days=RNG.randint(20, 70))
            prom = start + dt.timedelta(days=lead)
            comp = None
            pct_routed_target = min(0.93, (TODAY - start).days / max(lead, 1))
        elif status == "R":
            open_d = TODAY - dt.timedelta(days=RNG.randint(15, 80))
            start = None
            prom = TODAY + dt.timedelta(days=RNG.randint(45, 210))
            comp = None
            pct_routed_target = 0.0
        elif status == "P":
            open_d = TODAY - dt.timedelta(days=RNG.randint(2, 30))
            start = None
            prom = TODAY + dt.timedelta(days=RNG.randint(150, 300))
            comp = None
            pct_routed_target = 0.0
        else:  # H = hold
            start = TODAY - dt.timedelta(days=RNG.randint(40, 120))
            open_d = start - dt.timedelta(days=RNG.randint(20, 60))
            prom = start + dt.timedelta(days=lead)
            comp = None
            pct_routed_target = RNG.uniform(0.25, 0.6)

        orders.append(dict(
            i=idx, wo=wo_no, serial=serial, model=model, cust=cust, site=site,
            otype=otype, status=status, prio=prio, open_d=open_d, prom=prom,
            start=start, comp=comp, wctr="", pct=0,
            value=round(total_hrs * RNG.uniform(255, 470), 2),
            desc=desc, note=note, total_hrs=total_hrs,
            pct_target=pct_routed_target))
        wo_no += 1

# ---------------------------------------------------------------------------
# Routing operations, walked forward from the order's start date
# ---------------------------------------------------------------------------
for o in orders:
    ops = ROUTINGS[o["otype"]]
    routed = []
    done_hrs = target = 0.0
    total_std = sum(round(o["total_hrs"] * sh, 1) for _, _, sh in ops)
    target = total_std * o["pct_target"]
    cursor = o["start"] or wd_add(TODAY, 5)
    running_set = False
    acc = 0.0
    for seq, (wc, text, share) in enumerate(ops, start=1):
        std = round(o["total_hrs"] * share, 1)
        if acc + std <= target + 0.001:
            st = "C"
            act = round(std * RNG.uniform(0.82, 1.28), 1)
            o_start = cursor
            days = max(1, int(std / 26) + RNG.randint(0, 3))
            cursor = wd_add(cursor, days)
            o_comp = cursor
        elif not running_set and o["status"] in ("I", "H") and acc < target + std:
            st = "H" if o["status"] == "H" else "R"
            act = round(std * RNG.uniform(0.15, 0.75), 1)
            o_start, o_comp = cursor, None
            running_set = True
            o["wctr"] = wc
        else:
            st, act, o_start, o_comp = "W", 0.0, None, None
        acc += std
        if st == "C":
            done_hrs += std
        routed.append(dict(wo=o["wo"], seq=seq * 10, wc=wc, desc=text, std=std,
                           act=act, stat=st, start=o_start, comp=o_comp))
    o["std_total"] = round(total_std, 1)
    o["done_hrs"] = round(done_hrs, 1)
    # Reported percent normally agrees with the routing; where it does not,
    # that disagreement is itself a finding.
    o["pct"] = int(round(100 * done_hrs / total_std)) if total_std else 0
    if o["status"] == "C":
        o["pct"] = 100
    if o["status"] in ("I", "H") and not o["wctr"]:
        o["wctr"] = ops[0][0]
    routing.extend(routed)

# ---------------------------------------------------------------------------
# Material shortages against released and in-process orders
# ---------------------------------------------------------------------------
live = [o for o in orders if o["status"] in ("I", "R", "H")]
for o in RNG.sample(live, 16):
    ops_for = [r for r in routing if r["wo"] == o["wo"] and r["stat"] in ("W", "R", "H")]
    if not ops_for:
        continue
    for _ in range(RNG.randint(1, 3)):
        part, pdesc, plead = RNG.choice(SHORT_PARTS)
        if any(s["wo"] == o["wo"] and s["part"] == part for s in shortages):
            continue
        blocks = RNG.choice(ops_for)
        reqd = RNG.randint(1, 8)
        shortages.append(dict(
            wo=o["wo"], part=part, seq=blocks["seq"], reqd=reqd,
            avail=RNG.randint(0, max(0, reqd - 1)),
            due=TODAY + dt.timedelta(days=RNG.randint(4, plead + 25)),
            stat="O", po="PO%06d" % RNG.randint(30000, 39999),
            vend=RNG.choice(VENDORS)))
# a few already resolved, so the screen is not all red
for o in RNG.sample(live, 5):
    part, pdesc, plead = RNG.choice(SHORT_PARTS)
    if any(s["wo"] == o["wo"] and s["part"] == part for s in shortages):
        continue
    shortages.append(dict(
        wo=o["wo"], part=part, seq=10, reqd=RNG.randint(1, 4), avail=RNG.randint(4, 9),
        due=TODAY - dt.timedelta(days=RNG.randint(5, 40)), stat="R",
        po="PO%06d" % RNG.randint(30000, 39999), vend=RNG.choice(VENDORS)))

# ---------------------------------------------------------------------------
# Deliberate defects. Each one is a real condition a scheduling application
# would trip over, and each is what one row of the data-confidence panel is
# reporting. Applied by position so the file regenerates identically.
# ---------------------------------------------------------------------------
by_status = {s: [o for o in orders if o["status"] == s] for s in "CIRPH"}


def defect(o, what):
    DEFECTS.setdefault(o["wo"], []).append(what)


# 1. promised ship earlier than the order date - impossible, and it silently
#    makes the order look late forever
for o in by_status["R"][:2]:
    o["prom"] = o["open_d"] - dt.timedelta(days=RNG.randint(3, 20))
    defect(o, "promised ship before order date")

# 2. in process with no actual start date - elapsed time is uncomputable
for o in by_status["I"][:3]:
    o["start"] = None
    defect(o, "in process, no start date")

# 3. complete with no completion date
for o in by_status["C"][:2]:
    o["comp"] = None
    defect(o, "complete, no completion date")

# 4. current work centre that does not exist in FLWCP - load rolls up nowhere
for o, bogus in zip(by_status["I"][3:5], ["BAY3", "FINAL"]):
    o["wctr"] = bogus
    defect(o, "current work centre not in FLWCP")

# 5. blank current work centre while in process
for o in by_status["I"][5:8]:
    o["wctr"] = ""
    defect(o, "in process, no current work centre")

# 6. a model code that is not in FLMODLP
by_status["R"][2]["model"] = "J352-XLS"
defect(by_status["R"][2], "model not in FLMODLP")

# 7. reported percent complete well away from the routing
for o, delta in zip(by_status["I"][8:12], [34, -28, 41, -25]):
    o["pct"] = max(0, min(99, o["pct"] + delta))
    defect(o, "reported percent disagrees with routing")

# 8. released with no promised ship date at all
for o in by_status["R"][3:5]:
    o["prom"] = None
    defect(o, "released, no promised ship date")

# 9. two orders with no routing at all - invisible to any capacity plan
noroute = [by_status["R"][5], by_status["P"][0]]
noroute_wos = {o["wo"] for o in noroute}
routing = [r for r in routing if r["wo"] not in noroute_wos]
for o in noroute:
    defect(o, "no routing operations")
    o["std_total"] = 0.0

# 10. operations reported complete with zero actual hours - std vs actual
#     variance at that work centre is unusable
zeroed = 0
for r in routing:
    if r["stat"] == "C" and r["act"] > 0 and zeroed < 6 and r["wc"] in ("FAB", "ASSY", "HYD"):
        r["act"] = 0.0
        zeroed += 1
        defect(next(o for o in orders if o["wo"] == r["wo"]),
               "operation complete with zero actual hours")

# 11. operations carrying no standard hours - the capacity plan counts them free
zerostd = 0
for r in routing:
    if r["stat"] == "W" and zerostd < 4 and r["wc"] in ("CERT", "TEST"):
        r["std"] = 0.0
        zerostd += 1
        defect(next(o for o in orders if o["wo"] == r["wo"]),
               "operation with no standard hours")

# 12. operations routed to the inactive work centre
moved = 0
for r in routing:
    if r["wc"] == "FAB" and r["stat"] == "W" and moved < 2:
        r["wc"] = "WELD2"
        moved += 1
        defect(next(o for o in orders if o["wo"] == r["wo"]),
               "operation at an inactive work centre")

# 13. open shortages whose expected date has already passed - every projected
#     ship date downstream silently assumes the material arrived
past = 0
for s in shortages:
    if s["stat"] == "O" and past < 4:
        s["due"] = TODAY - dt.timedelta(days=RNG.randint(6, 45))
        past += 1
        defect(next(o for o in orders if o["wo"] == s["wo"]),
               "open shortage past its expected date")

# 14. open shortages with no purchase order behind them
nopo = 0
for s in shortages:
    if s["stat"] == "O" and s["po"] and nopo < 3 and s["due"] >= TODAY:
        s["po"] = ""
        nopo += 1
        defect(next(o for o in orders if o["wo"] == s["wo"]),
               "open shortage with no purchase order")

# ---------------------------------------------------------------------------
# Emit
# ---------------------------------------------------------------------------
out = sys.stdout.write
out("-- J.H. Fletcher demo - A4 production schedule board seed data (GJA-917)\n")
out("-- FICTIONAL DATA. Customers, vendors and work orders are invented.\n")
out("-- GENERATED by tools/gen-flsched-seed.py - do not hand-edit.\n")
out("-- Run via tools/seed-fletcher.sh <LIBRARY>\n")
out("--\n")
out("-- This data is deliberately dirty in %d places across %d work orders;\n"
    % (sum(len(v) for v in DEFECTS.values()), len(DEFECTS)))
out("-- the data-confidence panel on the board finds them by query. See the\n")
out("-- DEFECTS section of the generator for the list and the reason.\n")

out("\n-- FLWCP: %d rows\n" % len(WCTRS))
out("DELETE FROM FLWCP;\n")
for code, desc, seq, crew, shifts, st in WCTRS:
    out("INSERT INTO FLWCP (FLWCTR,FLWCDESC,FLWCSEQ,FLWCCAP,FLWCCREW,FLWCSHFT,FLWCSTAT)"
        " VALUES (%s, %s, %d, %.1f, %d, %d, %s);\n"
        % (qc("FLWCTR", code), qc("FLWCDESC", desc), seq, crew * shifts * 8,
           crew, shifts, qc("FLWCSTAT", st)))

out("\n-- FLWOP: %d rows\n" % len(orders))
out("DELETE FROM FLWOP;\n")
for o in orders:
    out("INSERT INTO FLWOP (FLWO,FLWSER,FLWMODL,FLWCUST,FLWSITE,FLWTYPE,FLWSTAT,"
        "FLWPRIO,FLWOPEN,FLWPROM,FLWSTRT,FLWCOMP,FLWWCTR,FLWPCT,FLWVALUE,FLWDESC,"
        "FLWNOTE) VALUES (%d, %s, %s, %d, %d, %s, %s, %s, %d, %d, %d, %d, %s, %d, "
        "%.2f, %s, %s);\n"
        % (o["wo"], qc("FLWSER", o["serial"]), qc("FLWMODL", o["model"]), o["cust"],
           o["site"], qc("FLWTYPE", o["otype"]), qc("FLWSTAT", o["status"]),
           qc("FLWPRIO", o["prio"]), ymd(o["open_d"]), ymd(o["prom"]),
           ymd(o["start"]), ymd(o["comp"]), qc("FLWWCTR", o["wctr"]), o["pct"],
           o["value"], qc("FLWDESC", o["desc"]), qc("FLWNOTE", o["note"])))

out("\n-- FLROUTP: %d rows\n" % len(routing))
out("DELETE FROM FLROUTP;\n")
for r in routing:
    out("INSERT INTO FLROUTP (FLRWO,FLRSEQ,FLRWCTR,FLRDESC,FLRSTHR,FLRACHR,FLRSTAT,"
        "FLRSTRT,FLRCOMP) VALUES (%d, %d, %s, %s, %.1f, %.1f, %s, %d, %d);\n"
        % (r["wo"], r["seq"], qc("FLRWCTR", r["wc"]), qc("FLRDESC", r["desc"]),
           r["std"], r["act"], qc("FLRSTAT", r["stat"]), ymd(r["start"]),
           ymd(r["comp"])))

out("\n-- FLSHRTP: %d rows\n" % len(shortages))
out("DELETE FROM FLSHRTP;\n")
for s in shortages:
    out("INSERT INTO FLSHRTP (FLSHWO,FLSHPART,FLSHSEQ,FLSHQTYR,FLSHQTYA,FLSHDUE,"
        "FLSHSTAT,FLSHPO,FLSHVEND) VALUES (%d, %s, %d, %d, %d, %d, %s, %s, %s);\n"
        % (s["wo"], qc("FLSHPART", s["part"]), s["seq"], s["reqd"], s["avail"],
           ymd(s["due"]), qc("FLSHSTAT", s["stat"]), qc("FLSHPO", s["po"]),
           qc("FLSHVEND", s["vend"])))

if _toolong:
    sys.stderr.write("VALUES TOO LONG FOR THEIR COLUMN:\n  "
                     + "\n  ".join(sorted(set(_toolong))) + "\n")
    sys.exit(1)

sys.stderr.write(
    "work centres %d | work orders %d | routing ops %d | shortages %d\n"
    "planted defects %d across %d work orders\n"
    % (len(WCTRS), len(orders), len(routing), len(shortages),
       sum(len(v) for v in DEFECTS.values()), len(DEFECTS)))
