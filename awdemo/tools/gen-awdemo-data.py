#!/usr/bin/env python3
"""Generate the AdelWiggins (AWDEMO) demo dataset as SQL INSERT scripts.

The dataset mirrors the shape of library BOMSAMPLE but is populated with
AdelWiggins Group content: aerospace fluid-transport couplings, quick
disconnects, dielectric isolators, freeze-protection heaters, Adel clamps
and Wiggins mining fast-fuelling products.

Everything is deterministic (fixed RNG seed), so regenerating produces an
identical dataset.  Table names in the emitted SQL are unqualified; the
loader script qualifies them with the target library.

    python3 awdemo/tools/gen-awdemo-data.py [output-dir]
"""

import datetime as dt
import os
import random
import sys

SEED = 20260908

# The dataset is anchored to a run date rather than a constant, so a demo
# always shows a live shop floor: a year of closed history behind it, work in
# process now, and a forward backlog.  Pinning the anchor made the data go
# stale -- at +60 days every open order was overdue and the backlog was empty.
#
# Override with --as-of YYYY-MM-DD (or $AWDEMO_AS_OF) to reproduce an old
# snapshot.  The RNG seed is fixed either way, so content is deterministic for
# a given anchor date; only the dates move.
_as_of = os.environ.get("AWDEMO_AS_OF")
_args = []
_argv = sys.argv[1:]
while _argv:
    a = _argv.pop(0)
    if a == "--as-of":
        _as_of = _argv.pop(0)
    elif a.startswith("--as-of="):
        _as_of = a.split("=", 1)[1]
    else:
        _args.append(a)
TODAY = dt.date.fromisoformat(_as_of) if _as_of else dt.date.today()

OUT = _args[0] if _args else os.path.join(
    os.path.dirname(os.path.abspath(__file__)), "..", "sql", "data")

rng = random.Random(SEED)


def add_workdays(day, n):
    """Shift `day` by n working days, skipping Saturday and Sunday."""
    step = 1 if n >= 0 else -1
    remaining = abs(n)
    while remaining:
        day += dt.timedelta(days=step)
        if day.weekday() < 5:
            remaining -= 1
    return day


def to_workday(day):
    """Nudge a date off a weekend to the nearest working day."""
    if day.weekday() == 5:
        return day - dt.timedelta(days=1)
    if day.weekday() == 6:
        return day + dt.timedelta(days=1)
    return day

# --------------------------------------------------------------------------
# Warehouses
# --------------------------------------------------------------------------
WAREHOUSES = [
    ("LAX", "Los Angeles Main Plant",   "Los Angeles, CA - AS9100D machining and assembly", "Y"),
    ("VAL", "Valencia Machining Center", "Valencia, CA - CNC detail parts and finishing",   "Y"),
    ("HUN", "Huntington Operations",     "Huntington, WV - clamp fabrication and moulding", "Y"),
    ("MIA", "Aftermarket Spares Depot",  "Miami, FL - MRO and AOG spares distribution",     "Y"),
    ("ELP", "Wiggins Mining Distribution", "El Paso, TX - mining fast-fuel product centre", "Y"),
]

# --------------------------------------------------------------------------
# Suppliers
# --------------------------------------------------------------------------
SUPPLIERS = [
    ("AWS001", "Pacific Aerospace Alloys",     "Dana Whitfield",  "310-555-0142", "dwhitfield@pacaeroalloys.com",  "Y"),
    ("AWS002", "Meridian CRES Products",       "Luis Obregon",    "562-555-0188", "lobregon@meridiancres.com",     "Y"),
    ("AWS003", "Titan Bar & Billet Co.",       "Priya Raman",     "714-555-0113", "praman@titanbarbillet.com",     "Y"),
    ("AWS004", "Elastomer Dynamics Inc.",      "Corey Nakamura",  "323-555-0177", "cnakamura@elastomerdyn.com",    "Y"),
    ("AWS005", "Precision Fastener Group",     "Marta Delgado",   "818-555-0126", "mdelgado@precisionfast.com",    "Y"),
    ("AWS006", "Coastal Anodizing & Finishing","Ray Kowalski",    "562-555-0209", "rkowalski@coastalanodize.com",  "Y"),
    ("AWS007", "ThermElectric Components LLC", "Ingrid Sorensen", "626-555-0154", "isorensen@thermelectric.com",   "Y"),
    ("AWS008", "Composite Tube Technologies",  "Aaron Petrosian", "949-555-0198", "apetrosian@comptubetech.com",   "Y"),
    ("AWS009", "SealTech Polymers",            "Yolanda Briggs",  "909-555-0165", "ybriggs@sealtechpoly.com",      "Y"),
    ("AWS010", "Western Spring & Wire",        "Devon Ashby",     "661-555-0131", "dashby@westernspringwire.com",  "Y"),
    ("AWS011", "Aero Label & Marking Co.",     "Suzanne Vogt",    "213-555-0172", "svogt@aerolabelmark.com",       "Y"),
    ("AWS012", "Summit Packaging Systems",     "Hector Alanis",   "310-555-0119", "halanis@summitpackaging.com",   "Y"),
    ("AWS013", "Redline Metal Treating",       "Gordon Meeks",    "562-555-0244", "gmeeks@redlinemetaltreat.com",  "N"),
]

# --------------------------------------------------------------------------
# Item master:  (item_id, description, type, uom, cost, active)
# --------------------------------------------------------------------------
ITEMS = [
    # ---- Purchased raw material: aluminium -------------------------------
    ("AL2024BAR", "2024-T351 Aluminum Bar Stock 2.00 in",        "P", "LB",   8.75, "Y"),
    ("AL2024SHT", "2024-T3 Aluminum Sheet .050 in",              "P", "SF",   6.40, "Y"),
    ("AL2024STR", "2024-T3 Aluminum Strip .032 x 1.00 in",       "P", "FT",   1.85, "Y"),
    ("AL6061BAR", "6061-T6 Aluminum Bar Stock 1.50 in",          "P", "LB",   5.20, "Y"),
    ("AL7075BAR", "7075-T73 Aluminum Bar Stock 2.50 in",         "P", "LB",  11.30, "Y"),
    # ---- Purchased raw material: CRES / steel ----------------------------
    ("CR304TUB",  "304 CRES Tube 1.00 OD x .035 Wall",           "P", "FT",   9.25, "Y"),
    ("CR316WIR",  "316 CRES Lockwire .032 in",                   "P", "FT",   0.42, "Y"),
    ("CR321BAR",  "321 CRES Bar Stock 1.25 in",                  "P", "LB",  12.40, "Y"),
    ("CR321SHT",  "321 CRES Sheet .025 in",                      "P", "SF",  14.60, "Y"),
    ("CR155BAR",  "15-5 PH CRES Bar Stock 1.00 in",              "P", "LB",  18.90, "Y"),
    ("CRA286BAR", "A286 Alloy Bar Stock .750 in",                "P", "LB",  22.80, "Y"),
    # ---- Purchased raw material: titanium --------------------------------
    ("TI64BAR",   "Ti-6Al-4V Bar Stock 1.00 in",                 "P", "LB",  38.50, "Y"),
    ("TI64SHT",   "Ti-6Al-4V Sheet .040 in",                     "P", "SF",  62.00, "Y"),
    ("TICP2TUB",  "CP Grade 2 Titanium Tube .750 OD",            "P", "FT",  44.75, "Y"),
    # ---- Purchased elastomers and seals ----------------------------------
    ("ELSIL60",   "Silicone Cushion Stock 60 Duro Sheet",        "P", "SF",   9.80, "Y"),
    ("ELFSIL70",  "Fluorosilicone Cushion Stock 70 Duro",        "P", "SF",  16.50, "Y"),
    ("ELPTFESH",  "PTFE Cushion Sheet .062 in",                  "P", "SF",  21.40, "Y"),
    ("ORFS908",   "Fluorosilicone O-Ring MS29513-908",           "P", "EA",   1.45, "Y"),
    ("ORFS912",   "Fluorosilicone O-Ring MS29513-912",           "P", "EA",   1.72, "Y"),
    ("ORVT916",   "Fluorocarbon O-Ring MS28775-916",             "P", "EA",   1.28, "Y"),
    ("ORVT924",   "Fluorocarbon O-Ring MS28775-924",             "P", "EA",   1.96, "Y"),
    ("ORSIL932",  "Silicone O-Ring MS9021-932",                  "P", "EA",   2.15, "Y"),
    ("ORNBR908",  "Nitrile O-Ring MS29513-908 Superseded",       "P", "EA",   0.95, "N"),
    ("SLPTFEBU",  "PTFE Backup Ring Set",                        "P", "EA",   0.88, "Y"),
    ("SLGRAFOL",  "Graphite Foil High-Temp Seal Ring",           "P", "EA",   4.35, "Y"),
    # ---- Purchased hardware ----------------------------------------------
    ("BLTA28610", "A286 Hex Bolt .250-28 x 1.00 in",             "P", "EA",   2.35, "Y"),
    ("BLTA28612", "A286 Hex Bolt .250-28 x 1.25 in",             "P", "EA",   2.55, "Y"),
    ("NUTA286SL", "A286 Self-Locking Nut .250-28",               "P", "EA",   1.15, "Y"),
    ("SCRCAP832", "CRES Socket Cap Screw 8-32 x .500",           "P", "EA",   0.34, "Y"),
    ("WSHCRE025", "CRES Flat Washer .250 in",                    "P", "EA",   0.09, "Y"),
    ("PINSHR188", "CRES Shear Pin .188 in Frangible",            "P", "EA",   3.60, "Y"),
    ("PINROLL12", "CRES Roll Pin .125 x .750 in",                "P", "EA",   0.22, "Y"),
    ("RIVAL44",   "Aluminum Blind Rivet 1/8 x 1/4",              "P", "EA",   0.11, "Y"),
    ("SPRCOMP01", "CRES Compression Spring .500 OD",             "P", "EA",   1.65, "Y"),
    ("SPRTORS02", "CRES Torsion Spring Latch",                   "P", "EA",   2.10, "Y"),
    ("RETRING18", "CRES Retaining Ring .188 in",                 "P", "EA",   0.28, "Y"),
    ("JAMNUT150", "Aluminum Jam Nut 1.500-18",                   "P", "EA",   4.85, "Y"),
    # ---- Purchased heater electrical -------------------------------------
    ("HTFOIL01",  "Etched Foil Heating Element 22 Watt",         "P", "EA",  27.40, "Y"),
    ("HTFOIL02",  "Etched Foil Heating Element 45 Watt",         "P", "EA",  34.90, "Y"),
    ("HTTSTAT1",  "Thermostatic Control Switch 120F",            "P", "EA",  18.25, "Y"),
    ("HTTCO160",  "Thermal Cut-Off Device 160F",                 "P", "EA",  12.60, "Y"),
    ("HTLEAD18",  "PTFE Lead Wire 18 AWG",                       "P", "FT",   1.35, "Y"),
    ("HTCONN01",  "Environmental Circular Connector 2-Pin",      "P", "EA",  22.75, "Y"),
    ("HTEPOXY1",  "Structural Epoxy Compound Heater Grade",      "P", "LB",  46.00, "Y"),
    ("HTSILRUB",  "Fire-Retardant Silicone Rubber Sheet",        "P", "SF",  13.20, "Y"),
    ("HTGLSCLO",  "Fiberglass Insulating Cloth",                 "P", "SY",   7.65, "Y"),
    # ---- Purchased composite ---------------------------------------------
    ("CMPTUBE05", "Fiberglass/Epoxy Dielectric Tube 0.50 in",    "P", "FT",  41.25, "Y"),
    ("CMPTUBE1",  "Fiberglass/Epoxy Dielectric Tube 1.00 in",    "P", "FT",  58.00, "Y"),
    ("CMPTUBE2",  "Fiberglass/Epoxy Dielectric Tube 2.00 in",    "P", "FT",  84.50, "Y"),
    ("CMPADHES",  "Aerospace Structural Adhesive Film",          "P", "SF",  19.80, "Y"),
    # ---- Purchased hose --------------------------------------------------
    ("HSPTFE04",  "PTFE Convoluted Hose Core -04",               "P", "FT",  16.40, "Y"),
    ("HSPTFE08",  "PTFE Smooth Bore Hose Core -08",              "P", "FT",  24.10, "Y"),
    ("HSBRAID1",  "CRES Wire Braid Reinforcement",               "P", "FT",   8.95, "Y"),
    ("HSFITAN4",  "AN Straight Flare End Fitting -04",           "P", "EA",  14.30, "Y"),
    ("HSFIT908",  "AN 90 Degree End Fitting -08",                "P", "EA",  26.75, "Y"),
    # ---- Purchased processes and finishes --------------------------------
    ("FNANODIZ",  "Sulfuric Anodize per MIL-A-8625",             "P", "EA",   3.20, "Y"),
    ("FNCHEMFL",  "Chemical Film per MIL-DTL-5541",              "P", "EA",   1.90, "Y"),
    ("FNPASSIV",  "CRES Passivation per AMS2700",                "P", "EA",   1.40, "Y"),
    ("FNPRIMER",  "Epoxy Primer per MIL-PRF-23377",              "P", "PT",  38.50, "Y"),
    ("FNPAINT1",  "Polyurethane Topcoat per MIL-PRF-85285",      "P", "QT",  44.00, "Y"),
    ("FNDRYLUB",  "Dry Film Lubricant per AS5272",               "P", "PT",  52.00, "Y"),
    ("FNNDTFPI",  "Fluorescent Penetrant Inspection Service",    "P", "EA",   6.75, "Y"),
    # ---- Purchased labels and packaging ----------------------------------
    ("LBLNAME1",  "Aerospace Nameplate Etched CRES",             "P", "EA",   2.85, "Y"),
    ("LBLPART1",  "Part Marking Label Polyimide",                "P", "EA",   0.42, "Y"),
    ("PKGESDBG",  "ESD Barrier Bag Heat Sealable",               "P", "EA",   0.55, "Y"),
    ("PKGCART1",  "Corrugated Shipping Carton 12x12x6",          "P", "EA",   1.30, "Y"),
    ("PKGDESIC",  "Desiccant Pack 1 Unit",                       "P", "EA",   0.28, "Y"),
    ("PKGFOAM1",  "Die-Cut Foam Insert",                         "P", "EA",   1.75, "Y"),

    # ---- Manufactured sub-assemblies -------------------------------------
    ("SACLSH16",  "Clamshell Half Machined -16 Aluminum",        "M", "EA",  22.40, "Y"),
    ("SACLSH24",  "Clamshell Half Machined -24 Aluminum",        "M", "EA",  28.90, "Y"),
    ("SACLSH32",  "Clamshell Half Machined -32 Aluminum",        "M", "EA",  35.60, "Y"),
    ("SACLSH24C", "Clamshell Half Machined -24 CRES",            "M", "EA",  46.20, "Y"),
    ("SACLSH32T", "Clamshell Half Machined -32 Titanium",        "M", "EA", 118.40, "Y"),
    ("SATHBDY16", "Threaded Coupling Body Machined -16",         "M", "EA",  24.80, "Y"),
    ("SATHBDY24", "Threaded Coupling Body Machined -24",         "M", "EA",  31.20, "Y"),
    ("SATHBDY32", "Threaded Coupling Body Machined -32",         "M", "EA",  39.60, "Y"),
    ("SATHRDNUT", "Threaded Coupling Nut Machined Aluminum",     "M", "EA",  19.30, "Y"),
    ("SALATCH1",  "Latch and Hook Assembly Single Fault",        "M", "EA",  31.75, "Y"),
    ("SAVTLI01",  "Visual Tactile Latch Indicator Assembly",     "M", "EA",  17.90, "Y"),
    ("SASEALK16", "Coupling Seal Kit -16",                       "M", "EA",   6.40, "Y"),
    ("SASEALK24", "Coupling Seal Kit -24",                       "M", "EA",   8.15, "Y"),
    ("SASEALK32", "Coupling Seal Kit -32",                       "M", "EA",   9.85, "Y"),
    ("SABODY200", "200 Series QD Body Machined",                 "M", "EA",  41.50, "Y"),
    ("SABODY600", "6000 Series QD Body Machined",                "M", "EA",  47.80, "Y"),
    ("SAPOPPET1", "QD Poppet and Spring Subassembly",            "M", "EA",  16.25, "Y"),
    ("SASLEEVE1", "QD Locking Sleeve Machined",                  "M", "EA",  13.70, "Y"),
    ("SAHTRCORE", "Heater Core Assembly Foil and Substrate",     "M", "EA",  63.20, "Y"),
    ("SAHTRHARN", "Heater Wire Harness Assembly",                "M", "EA",  38.45, "Y"),
    ("SAISOTUB1", "Isolator Tube Subassembly 1.00 in",           "M", "EA", 128.60, "Y"),
    ("SAISOTUB2", "Isolator Tube Subassembly 2.00 in",           "M", "EA", 186.40, "Y"),
    ("SAISOEND1", "Isolator End Fitting Machined Aluminum",      "M", "EA",  54.30, "Y"),
    ("SABANDAL6", "Clamp Band Formed 2024 Aluminum -06",         "M", "EA",   1.95, "Y"),
    ("SABANDCR8", "Clamp Band Formed 321 CRES -08",              "M", "EA",   3.40, "Y"),
    ("SACUSHN06", "Clamp Cushion Molded Silicone -06",           "M", "EA",   1.20, "Y"),
    ("SACUSHNW8", "Clamp Cushion W-Shape Fluorosilicone -08",    "M", "EA",   2.65, "Y"),
    ("SAWEARSH1", "Wear Sleeve Shell Half High Temp",            "M", "EA",   4.80, "Y"),
    ("SAHOSEAS8", "Hose Core and Braid Subassembly -08",         "M", "EA",  57.90, "Y"),

    # ---- Manufactured ferrules and fittings ------------------------------
    ("W902-16",   "W902 Swage Ferrule -16 Aluminum",             "M", "EA",  18.60, "Y"),
    ("W902-24",   "W902 Swage Ferrule -24 Aluminum",             "M", "EA",  23.75, "Y"),
    ("W902-32",   "W902 Swage Ferrule -32 CRES",                 "M", "EA",  41.20, "Y"),
    ("F63052-24", "63052 Variable Cavity Swage Ferrule -24",     "M", "EA",  27.35, "Y"),
    ("F63052-32", "63052 Swage Ferrule -32 Titanium",            "M", "EA",  96.40, "Y"),
    ("W922N-16",  "W922N Bulkhead Fitting -16 Aluminum",         "M", "EA",  86.40, "Y"),

    # ---- Finished goods: flexible clamshell couplings --------------------
    ("AW2010-16", "AW2010HP Clamshell Coupling -16 Aluminum",    "M", "EA", 184.50, "Y"),
    ("AW2010-24", "AW2010HP Clamshell Coupling -24 Aluminum",    "M", "EA", 226.80, "Y"),
    ("AW2010-32", "AW2010HP Clamshell Coupling -32 Titanium",    "M", "EA", 512.40, "Y"),
    ("AW2091-24", "AW2091N Clamshell Coupling -24 Aluminum",     "M", "EA", 241.60, "Y"),
    ("AW2100-24", "AW2100 Clamshell Coupling -24 AS7510",        "M", "EA", 318.75, "Y"),
    ("AW2100-32", "AW2100 Clamshell Coupling -32 AS7510",        "M", "EA", 386.20, "Y"),
    ("W991-16",   "W991 Flexible Clamshell Coupling -16",        "M", "EA", 162.30, "Y"),
    ("W991-24",   "W991 Flexible Clamshell Coupling -24",        "M", "EA", 198.45, "Y"),
    ("W991N-32",  "W991N Flexible Clamshell Coupling -32",       "M", "EA", 274.90, "Y"),
    ("W991F-24",  "W991F High Temp Clamshell Coupling -24",      "M", "EA", 329.50, "Y"),
    ("G620-32",   "G620 Clamshell Tube Connector -32",           "M", "EA", 355.80, "Y"),
    # ---- Finished goods: flexible threaded and rigid couplings -----------
    ("AW971-16",  "AW971 Flexible Threaded Coupling -16",        "M", "EA", 147.20, "Y"),
    ("AW971-24",  "AW971 Flexible Threaded Coupling -24",        "M", "EA", 176.90, "Y"),
    ("W700-32",   "W700 Flexible Threaded Coupling -32",         "M", "EA", 221.40, "Y"),
    ("C3600-16",  "3600 Series Flexible Threaded Coupling -16",  "M", "EA", 158.75, "Y"),
    ("C6300-24",  "6300 Series Flexible Threaded Coupling -24",  "M", "EA", 189.30, "Y"),
    ("AW979-24",  "AW979 Rigid Coupling -24 Self-Locking",       "M", "EA", 132.60, "Y"),
    ("C8300-32",  "8300 Series Rigid Coupling -32 AS1730",       "M", "EA", 151.85, "Y"),
    # ---- Finished goods: quick disconnects, valves, hoses ----------------
    ("QD200-08",  "200 Series Quick Disconnect -08 Aluminum",    "M", "EA",  94.70, "Y"),
    ("QD200-16",  "200 Series Quick Disconnect -16 CRES",        "M", "EA", 168.40, "Y"),
    ("QD200-04",  "200 Series Quick Disconnect -04 Obsolete",    "M", "EA",  88.20, "N"),
    ("QD6000-12", "6000 Series Quick Disconnect -12",            "M", "EA", 121.35, "Y"),
    ("P9100-08",  "Preece 9100 Blind Mate QD -08",               "M", "EA", 138.90, "Y"),
    ("P9400-12",  "Preece 9400 Series Quick Disconnect -12",     "M", "EA", 164.25, "Y"),
    ("P9600-16",  "Preece 9600 Series Quick Disconnect -16",     "M", "EA", 192.80, "Y"),
    ("PVCHK-08",  "Preece Check Valve -08",                      "M", "EA", 108.50, "Y"),
    ("PVFUSE-12", "Preece Fuse Valve -12",                       "M", "EA", 143.70, "Y"),
    ("FRANG-12",  "Frangible Coupling -12 Stainless Steel",      "M", "EA", 212.40, "Y"),
    ("HOSE-04S",  "Hose Assembly -04 Straight AN Ends",          "M", "EA", 118.65, "Y"),
    ("HOSE-08L",  "Hose Assembly -08 90 Degree End",             "M", "EA", 176.20, "Y"),
    # ---- Finished goods: dielectric isolators ----------------------------
    ("ISOF-1000", "Fuel Isolator 1.00 in Dielectric Inline",     "M", "EA", 1245.00, "Y"),
    ("ISOF-2000", "Fuel Isolator 2.00 in Dielectric Inline",     "M", "EA", 1680.50, "Y"),
    ("ISOH-0500", "Hydraulic Isolator 0.50 in Bulkhead",         "M", "EA", 1420.75, "Y"),
    # ---- Finished goods: heaters -----------------------------------------
    ("HTRRB-024", "Ribbon Heater 24 in Thermostatic",            "M", "EA", 386.40, "Y"),
    ("HTRRB-012", "Ribbon Heater 12 in Superseded by 024",       "M", "EA", 341.00, "N"),
    ("HTRBL-112", "Heater Blanket 12 in Duct Freeze Protect",    "M", "EA", 452.90, "Y"),
    ("HTRGK-HG1", "HG Series Gasket Heater Lever Lock",          "M", "EA", 268.35, "Y"),
    ("HTRCT-22W", "Cartridge Heater 22 Watt EB Welded",          "M", "EA", 214.60, "Y"),
    ("HTRCF-201", "Heater Cuff and Manifold Assembly",           "M", "EA", 512.80, "Y"),
    ("HTRHS-06",  "Heated Hose Assembly -06 Potable Water",      "M", "EA", 478.25, "Y"),
    # ---- Finished goods: Adel clamps -------------------------------------
    ("CLPSTD-06", "Standard Loop Clamp -06 Aluminum Cushion",    "M", "EA",   3.85, "Y"),
    ("CLPSTD-04", "Standard Loop Clamp -04 Obsolete Cushion",    "M", "EA",   3.40, "N"),
    ("CLPTL-08",  "Top-Locking Loop Clamp -08 2024 Aluminum",    "M", "EA",   6.40, "Y"),
    ("CLPSL-08",  "Side Locking Loop Clamp -08 321 CRES",        "M", "EA",   8.95, "Y"),
    ("CLPBLK-12", "Block Clamp -12 Double Mount 304 CRES",       "M", "EA",  16.70, "Y"),
    ("CLPWS-10",  "Wear Sleeve Clamp -10 High Temperature",      "M", "EA",  12.30, "Y"),
    ("CLPSPC-06", "Aircraft Specialty Clamp -06 W-Cushion",      "M", "EA",   9.55, "Y"),
    ("GRMAC-04",  "Aircraft Grommet -04 Panel Mount",            "M", "EA",   2.10, "Y"),
    # ---- Finished goods: Wiggins mining products -------------------------
    ("WMFFN-100", "Diesel Fast Fueling Nozzle 1200 LPM",         "M", "EA", 1875.00, "Y"),
    ("WMFFR-100", "Diesel Fast Fueling Receiver 1.5 in",         "M", "EA", 1240.00, "Y"),
    ("WMDEF-050", "DEF Automatic Fill System Heated",            "M", "EA", 2340.00, "Y"),
    ("WMLQD-075", "Lubrication Quick Disconnect .75 Dry Break",  "M", "EA", 186.50, "Y"),
]

# --------------------------------------------------------------------------
# Product structures.  parent -> [(component, qty), ...]
# Sub-assemblies are themselves parents, giving genuine multi-level BOMs.
# --------------------------------------------------------------------------
STRUCT = {
    # --- level 3: machined details and kits -------------------------------
    "SACLSH16":  [("AL2024BAR", 0.85), ("FNCHEMFL", 1), ("FNNDTFPI", 1)],
    "SACLSH24":  [("AL2024BAR", 1.35), ("FNCHEMFL", 1), ("FNNDTFPI", 1)],
    "SACLSH32":  [("AL2024BAR", 1.90), ("FNCHEMFL", 1), ("FNNDTFPI", 1)],
    "SACLSH24C": [("CR321BAR", 2.10), ("FNPASSIV", 1), ("FNPRIMER", 0.02), ("FNNDTFPI", 1)],
    "SACLSH32T": [("TI64BAR", 2.10), ("TI64SHT", 0.20), ("FNPASSIV", 1), ("FNNDTFPI", 1)],
    "SATHBDY16": [("AL2024BAR", 1.05), ("FNANODIZ", 1), ("FNNDTFPI", 1)],
    "SATHBDY24": [("AL2024BAR", 1.45), ("FNANODIZ", 1), ("FNNDTFPI", 1)],
    "SATHBDY32": [("AL2024BAR", 1.95), ("FNANODIZ", 1), ("FNNDTFPI", 1)],
    "SATHRDNUT": [("AL2024BAR", 1.10), ("FNANODIZ", 1)],
    "SALATCH1":  [("CRA286BAR", 0.35), ("SPRTORS02", 1), ("PINROLL12", 2), ("FNPASSIV", 1)],
    "SAVTLI01":  [("ELPTFESH", 0.12), ("PINROLL12", 1), ("SPRCOMP01", 1)],
    "SASEALK16": [("ORFS908", 2), ("SLPTFEBU", 2)],
    "SASEALK24": [("ORFS912", 2), ("SLPTFEBU", 2)],
    "SASEALK32": [("ORVT924", 2), ("SLPTFEBU", 2), ("SLGRAFOL", 1)],
    "SABODY200": [("AL6061BAR", 1.60), ("FNANODIZ", 1), ("FNNDTFPI", 1)],
    "SABODY600": [("AL6061BAR", 1.95), ("FNANODIZ", 1), ("FNNDTFPI", 1)],
    "SAPOPPET1": [("CRA286BAR", 0.22), ("SPRCOMP01", 1), ("ORVT916", 1), ("RETRING18", 1)],
    "SASLEEVE1": [("AL7075BAR", 0.75), ("FNANODIZ", 1), ("FNDRYLUB", 0.01), ("SPRCOMP01", 1)],
    "SAHTRCORE": [("HTFOIL01", 1), ("HTEPOXY1", 0.35), ("HTGLSCLO", 0.50)],
    "SAHTRHARN": [("HTLEAD18", 3.5), ("HTCONN01", 1), ("HTTSTAT1", 1), ("HTTCO160", 1)],
    "SAISOTUB1": [("CMPTUBE1", 1.20), ("CMPADHES", 0.40)],
    "SAISOTUB2": [("CMPTUBE2", 1.50), ("CMPADHES", 0.60)],
    "SAISOEND1": [("AL7075BAR", 1.25), ("FNCHEMFL", 1), ("FNPRIMER", 0.02), ("FNNDTFPI", 1)],
    "SABANDAL6": [("AL2024STR", 0.55), ("FNCHEMFL", 1)],
    "SABANDCR8": [("CR321SHT", 0.08), ("FNPASSIV", 1)],
    "SACUSHN06": [("ELSIL60", 0.05)],
    "SACUSHNW8": [("ELFSIL70", 0.09)],
    "SAWEARSH1": [("CR304TUB", 0.15), ("FNPASSIV", 1)],
    "SAHOSEAS8": [("HSPTFE08", 1.60), ("HSBRAID1", 1.60)],
    # --- ferrules and fittings --------------------------------------------
    "W902-16":   [("AL2024BAR", 0.42), ("FNCHEMFL", 1), ("FNNDTFPI", 1)],
    "W902-24":   [("AL2024BAR", 0.65), ("FNCHEMFL", 1), ("FNNDTFPI", 1)],
    "W902-32":   [("CR321BAR", 1.05), ("FNPASSIV", 1), ("FNNDTFPI", 1)],
    "F63052-24": [("AL2024BAR", 0.70), ("FNCHEMFL", 1), ("FNNDTFPI", 1)],
    "F63052-32": [("TICP2TUB", 0.55), ("FNPASSIV", 1), ("FNNDTFPI", 1)],
    "W922N-16":  [("AL7075BAR", 1.40), ("JAMNUT150", 1), ("ORFS908", 1), ("FNANODIZ", 1)],
    # --- finished goods: clamshell couplings -------------------------------
    "AW2010-16": [("SACLSH16", 2), ("SALATCH1", 1), ("SASEALK16", 1), ("BLTA28610", 2),
                  ("NUTA286SL", 2), ("LBLPART1", 1), ("PKGESDBG", 1)],
    "AW2010-24": [("SACLSH24", 2), ("SALATCH1", 1), ("SASEALK24", 1), ("BLTA28612", 2),
                  ("NUTA286SL", 2), ("LBLPART1", 1), ("PKGESDBG", 1)],
    "AW2010-32": [("SACLSH32T", 2), ("SALATCH1", 1), ("SASEALK32", 1), ("BLTA28612", 2),
                  ("NUTA286SL", 2), ("LBLNAME1", 1), ("PKGESDBG", 1)],
    "AW2091-24": [("SACLSH24", 2), ("SALATCH1", 1), ("SAVTLI01", 1), ("SASEALK24", 1),
                  ("BLTA28612", 2), ("NUTA286SL", 2), ("LBLPART1", 1), ("PKGESDBG", 1)],
    "AW2100-24": [("SACLSH24", 2), ("SALATCH1", 2), ("SAVTLI01", 1), ("SASEALK24", 1),
                  ("BLTA28612", 4), ("NUTA286SL", 4), ("LBLNAME1", 1), ("PKGESDBG", 1),
                  ("PKGFOAM1", 1)],
    "AW2100-32": [("SACLSH32", 2), ("SALATCH1", 2), ("SAVTLI01", 1), ("SASEALK32", 1),
                  ("BLTA28612", 4), ("NUTA286SL", 4), ("LBLNAME1", 1), ("PKGESDBG", 1),
                  ("PKGFOAM1", 1)],
    "W991-16":   [("SACLSH16", 2), ("SASEALK16", 1), ("BLTA28610", 2), ("NUTA286SL", 2),
                  ("SPRTORS02", 1), ("LBLPART1", 1), ("PKGESDBG", 1)],
    "W991-24":   [("SACLSH24", 2), ("SASEALK24", 1), ("BLTA28612", 2), ("NUTA286SL", 2),
                  ("SPRTORS02", 1), ("LBLPART1", 1), ("PKGESDBG", 1)],
    "W991N-32":  [("SACLSH32", 2), ("SASEALK32", 1), ("BLTA28612", 2), ("NUTA286SL", 2),
                  ("SPRTORS02", 1), ("LBLPART1", 1), ("PKGESDBG", 1)],
    "W991F-24":  [("SACLSH24C", 2), ("SLGRAFOL", 2), ("BLTA28612", 2), ("NUTA286SL", 2),
                  ("SPRTORS02", 1), ("LBLNAME1", 1), ("PKGESDBG", 1)],
    "G620-32":   [("SACLSH32", 2), ("SALATCH1", 1), ("SASEALK32", 1), ("BLTA28612", 2),
                  ("NUTA286SL", 2), ("LBLPART1", 1), ("PKGESDBG", 1)],
    # --- finished goods: threaded and rigid couplings ----------------------
    "AW971-16":  [("SATHBDY16", 1), ("SATHRDNUT", 1), ("SASEALK16", 1), ("RETRING18", 1),
                  ("LBLPART1", 1), ("PKGESDBG", 1)],
    "AW971-24":  [("SATHBDY24", 1), ("SATHRDNUT", 1), ("SASEALK24", 1), ("RETRING18", 1),
                  ("LBLPART1", 1), ("PKGESDBG", 1)],
    "W700-32":   [("SATHBDY32", 1), ("SATHRDNUT", 1), ("SASEALK32", 1), ("RETRING18", 1),
                  ("LBLPART1", 1), ("PKGESDBG", 1)],
    "C3600-16":  [("SATHBDY16", 1), ("SATHRDNUT", 1), ("SASEALK16", 1), ("LBLPART1", 1),
                  ("PKGESDBG", 1)],
    "C6300-24":  [("SATHBDY24", 1), ("SATHRDNUT", 1), ("SASEALK24", 1), ("LBLPART1", 1),
                  ("PKGESDBG", 1)],
    "AW979-24":  [("SATHBDY24", 1), ("SATHRDNUT", 1), ("SASEALK24", 1), ("SCRCAP832", 2),
                  ("LBLPART1", 1), ("PKGESDBG", 1)],
    "C8300-32":  [("SATHBDY32", 1), ("SATHRDNUT", 1), ("SASEALK32", 1), ("SCRCAP832", 2),
                  ("CR316WIR", 0.50), ("LBLPART1", 1), ("PKGESDBG", 1)],
    # --- finished goods: quick disconnects, valves, hoses ------------------
    "QD200-08":  [("SABODY200", 1), ("SAPOPPET1", 2), ("SASLEEVE1", 1), ("ORVT916", 2),
                  ("SPRCOMP01", 1), ("LBLPART1", 1), ("PKGESDBG", 1)],
    "QD200-16":  [("SABODY200", 1), ("SAPOPPET1", 2), ("SASLEEVE1", 1), ("ORVT924", 2),
                  ("SPRCOMP01", 1), ("CR316WIR", 0.50), ("LBLNAME1", 1), ("PKGESDBG", 1)],
    "QD200-04":  [("SABODY200", 1), ("SAPOPPET1", 2), ("ORNBR908", 2), ("SPRCOMP01", 1),
                  ("LBLPART1", 1), ("PKGESDBG", 1)],
    "QD6000-12": [("SABODY600", 1), ("SAPOPPET1", 2), ("SASLEEVE1", 1), ("ORVT916", 2),
                  ("SPRCOMP01", 2), ("LBLPART1", 1), ("PKGESDBG", 1)],
    "P9100-08":  [("SABODY200", 1), ("SAPOPPET1", 2), ("ORFS908", 2), ("SPRCOMP01", 1),
                  ("LBLPART1", 1), ("PKGESDBG", 1)],
    "P9400-12":  [("SABODY600", 1), ("SAPOPPET1", 2), ("ORFS912", 2), ("SPRCOMP01", 1),
                  ("LBLPART1", 1), ("PKGESDBG", 1)],
    "P9600-16":  [("SABODY600", 1), ("SAPOPPET1", 2), ("ORFS912", 2), ("SASLEEVE1", 1),
                  ("SPRCOMP01", 1), ("LBLNAME1", 1), ("PKGESDBG", 1)],
    "PVCHK-08":  [("SABODY200", 1), ("SAPOPPET1", 1), ("SPRCOMP01", 1), ("ORVT916", 2),
                  ("LBLPART1", 1), ("PKGESDBG", 1)],
    "PVFUSE-12": [("SABODY600", 1), ("SAPOPPET1", 1), ("SPRCOMP01", 2), ("ORVT916", 2),
                  ("RETRING18", 1), ("LBLPART1", 1), ("PKGESDBG", 1)],
    "FRANG-12":  [("CR155BAR", 1.85), ("PINSHR188", 2), ("ORVT916", 2), ("FNPASSIV", 1),
                  ("FNNDTFPI", 1), ("LBLNAME1", 1), ("PKGESDBG", 1)],
    "HOSE-04S":  [("HSPTFE04", 2.50), ("HSBRAID1", 2.50), ("HSFITAN4", 2), ("LBLPART1", 1),
                  ("PKGESDBG", 1)],
    "HOSE-08L":  [("SAHOSEAS8", 1), ("HSFIT908", 1), ("HSFITAN4", 1), ("LBLPART1", 1),
                  ("PKGESDBG", 1)],
    # --- finished goods: isolators -----------------------------------------
    "ISOF-1000": [("SAISOTUB1", 1), ("SAISOEND1", 2), ("ORFS912", 2), ("BLTA28612", 4),
                  ("NUTA286SL", 4), ("LBLNAME1", 1), ("PKGESDBG", 1), ("PKGDESIC", 1),
                  ("PKGFOAM1", 1), ("PKGCART1", 1)],
    "ISOF-2000": [("SAISOTUB2", 1), ("SAISOEND1", 2), ("ORVT924", 2), ("BLTA28612", 6),
                  ("NUTA286SL", 6), ("LBLNAME1", 1), ("PKGESDBG", 1), ("PKGDESIC", 1),
                  ("PKGFOAM1", 1), ("PKGCART1", 1)],
    "ISOH-0500": [("CMPTUBE05", 1.00), ("SAISOEND1", 2), ("JAMNUT150", 1), ("ORFS908", 2),
                  ("LBLNAME1", 1), ("PKGESDBG", 1), ("PKGFOAM1", 1)],
    # --- finished goods: heaters -------------------------------------------
    "HTRRB-024": [("SAHTRCORE", 1), ("SAHTRHARN", 1), ("HTSILRUB", 1.20), ("LBLNAME1", 1),
                  ("PKGESDBG", 1)],
    "HTRRB-012": [("SAHTRCORE", 1), ("SAHTRHARN", 1), ("HTSILRUB", 0.70), ("LBLNAME1", 1),
                  ("PKGESDBG", 1)],
    "HTRBL-112": [("SAHTRCORE", 1), ("SAHTRHARN", 1), ("HTSILRUB", 2.40), ("HTGLSCLO", 1.00),
                  ("RIVAL44", 4), ("LBLNAME1", 1), ("PKGESDBG", 1)],
    "HTRGK-HG1": [("HTFOIL01", 1), ("HTEPOXY1", 0.22), ("HTLEAD18", 1.50), ("ORSIL932", 2),
                  ("LBLPART1", 1), ("PKGESDBG", 1)],
    "HTRCT-22W": [("HTFOIL01", 1), ("CR304TUB", 0.25), ("HTLEAD18", 1.00), ("FNPASSIV", 1),
                  ("LBLPART1", 1), ("PKGESDBG", 1)],
    "HTRCF-201": [("SAHTRCORE", 1), ("SAHTRHARN", 1), ("HTSILRUB", 1.80), ("AL2024SHT", 0.40),
                  ("LBLNAME1", 1), ("PKGESDBG", 1)],
    "HTRHS-06":  [("HSPTFE04", 3.00), ("HTFOIL02", 1), ("HTLEAD18", 3.50), ("HSFITAN4", 2),
                  ("LBLNAME1", 1), ("PKGESDBG", 1)],
    # --- finished goods: Adel clamps ---------------------------------------
    "CLPSTD-06": [("SABANDAL6", 1), ("SACUSHN06", 1)],
    "CLPTL-08":  [("SABANDAL6", 1), ("SACUSHN06", 1), ("PINROLL12", 1), ("SPRTORS02", 1)],
    "CLPSL-08":  [("SABANDCR8", 1), ("SACUSHNW8", 1), ("PINROLL12", 1), ("SPRTORS02", 1)],
    "CLPBLK-12": [("SABANDCR8", 2), ("SACUSHNW8", 2), ("SCRCAP832", 2), ("WSHCRE025", 2)],
    "CLPWS-10":  [("SABANDCR8", 1), ("SAWEARSH1", 2)],
    "CLPSPC-06": [("SABANDCR8", 1), ("SACUSHNW8", 1)],
    "GRMAC-04":  [("ELSIL60", 0.02)],
    # --- finished goods: Wiggins mining ------------------------------------
    "WMFFN-100": [("SABODY600", 1), ("SAPOPPET1", 2), ("SASLEEVE1", 1), ("ORVT924", 4),
                  ("SPRCOMP01", 2), ("AL7075BAR", 2.40), ("FNANODIZ", 1), ("FNPAINT1", 0.05),
                  ("LBLNAME1", 1), ("PKGDESIC", 1), ("PKGCART1", 1)],
    "WMFFR-100": [("SABODY600", 1), ("SAPOPPET1", 1), ("ORVT924", 2), ("AL7075BAR", 1.80),
                  ("FNANODIZ", 1), ("FNPAINT1", 0.04), ("LBLNAME1", 1), ("PKGCART1", 1)],
    "WMDEF-050": [("SABODY200", 1), ("SAPOPPET1", 1), ("HTFOIL02", 1), ("HTTSTAT1", 1),
                  ("HTLEAD18", 4.00), ("ORVT916", 2), ("LBLNAME1", 1), ("PKGCART1", 1)],
    "WMLQD-075": [("SABODY200", 1), ("SAPOPPET1", 2), ("ORVT916", 2), ("SPRCOMP01", 1),
                  ("LBLPART1", 1), ("PKGESDBG", 1)],
}

# Parents that carry a superseding revision B.  value = (effective date, edits)
# edits is a list of (component, qty) that replace/extend the rev A structure.
REV_B = {
    "AW2100-24": (dt.date(2025, 11, 3),
                  [("SASEALK24", 1), ("SAVTLI01", 2)]),
    "ISOF-1000": (dt.date(2026, 2, 16),
                  [("PKGDESIC", 2), ("CMPADHES", 0.10)]),
    "W991-24":   (dt.date(2026, 5, 4),
                  [("SLGRAFOL", 1)]),
    "HTRBL-112": (dt.date(2026, 1, 12),
                  [("HTTCO160", 1)]),
    "WMFFN-100": (dt.date(2025, 12, 8),
                  [("FNDRYLUB", 0.02)]),
}

# CLPSTD-04 is deliberately left without a BOM: an obsolete manufactured item
# with no product structure, useful for exception reporting in the demo.
NO_BOM = {"CLPSTD-04"}

# --------------------------------------------------------------------------
# Standard cost of a manufactured item is rolled up from its structure rather
# than hand-typed, so a cost roll-up in the demo actually ties out.  The factor
# is the conversion cost (labour, machine time, inspection, NDT, overhead)
# applied on top of rolled-up material.
# --------------------------------------------------------------------------
COST_FACTOR_DEFAULT = 1.80
COST_FACTOR = [
    ("SASEALK", 1.45),     # kitting only
    ("SACUSHN", 1.90),     # moulding
    ("SABAND",  1.75),     # forming
    ("SAWEARSH", 1.85),
    ("SA",      2.25),     # machined details and mechanical sub-assemblies
    ("W902",    2.25),     # machined ferrules
    ("F63052",  2.25),
    ("W922N",   2.25),
    ("ISO",     2.80),     # dielectric isolators: qualification and test heavy
    ("HTR",     2.20),     # heaters
    ("CLP",     1.70),     # clamps: high volume, short cycle
    ("GRM",     1.70),
    ("WM",      2.20),     # mining products
]


def cost_factor(item_id):
    for prefix, factor in COST_FACTOR:
        if item_id.startswith(prefix):
            return factor
    return COST_FACTOR_DEFAULT


def roll_up_costs(items, struct):
    """Replace the cost of every manufactured item with rolled-up material
    times its conversion factor.  Resolved depth-first so a sub-assembly is
    costed before the parent that consumes it."""
    cost = {i[0]: i[4] for i in items}
    itype = {i[0]: i[2] for i in items}
    resolved, in_progress = set(), set()

    def resolve(iid):
        if iid in resolved or itype.get(iid) != "M" or iid not in struct:
            return cost[iid]
        if iid in in_progress:
            raise SystemExit("cyclic product structure at %s" % iid)
        in_progress.add(iid)
        material = sum(resolve(c) * qty for c, qty in struct[iid])
        cost[iid] = round(material * cost_factor(iid), 2)
        in_progress.discard(iid)
        resolved.add(iid)
        return cost[iid]

    for iid in list(cost):
        resolve(iid)
    return [(a, b, c, u, cost[a], f) for a, b, c, u, _k, f in items]

# --------------------------------------------------------------------------
def q(s):
    return "'" + str(s).replace("'", "''") + "'"


def d(x):
    return "DATE('%s')" % x.isoformat() if x else "NULL"


def emit(name, header, cols, rows, chunk=50):
    """Write one INSERT-per-chunk SQL file.  Table name left unqualified."""
    path = os.path.join(OUT, name)
    with open(path, "w") as f:
        f.write("--  %s\n"
                "--  Generated by awdemo/tools/gen-awdemo-data.py - do not edit by hand.\n"
                "--  Dates are anchored to as-of %s; regenerate before a demo.\n\n"
                % (header, TODAY.isoformat()))
        table = header.split()[0]
        for i in range(0, len(rows), chunk):
            f.write("INSERT INTO %s (%s) VALUES\n" % (table, ", ".join(cols)))
            f.write(",\n".join("  (%s)" % ", ".join(r) for r in rows[i:i + chunk]))
            f.write(";\n\n")
    return len(rows)


counts = {}

# ---- WAREHOUSE ------------------------------------------------------------
counts["WAREHOUSE"] = emit(
    "10_warehouse.sql", "WAREHOUSE - AdelWiggins stocking locations",
    ["WH_CODE", "WH_NAME", "LOCATION", "ACTIVE_FLAG"],
    [[q(c), q(n), q(l), q(a)] for c, n, l, a in WAREHOUSES])

# ---- SUPPLIER -------------------------------------------------------------
counts["SUPPLIER"] = emit(
    "20_supplier.sql", "SUPPLIER - approved suppliers",
    ["SUPPLIER_ID", "SUPPLIER_NAME", "CONTACT_NAME", "PHONE", "EMAIL", "ACTIVE_FLAG"],
    [[q(a), q(b), q(c), q(e), q(f), q(g)] for a, b, c, e, f, g in SUPPLIERS])

# ---- ITEM -----------------------------------------------------------------
for it in ITEMS:
    assert len(it[0]) <= 10, "item id too long: %s" % it[0]
    assert len(it[1]) <= 50, "desc too long: %s" % it[1]
ITEM_IDS = {i[0] for i in ITEMS}
assert len(ITEM_IDS) == len(ITEMS), "duplicate item id"

ITEMS = roll_up_costs(ITEMS, STRUCT)

counts["ITEM"] = emit(
    "30_item.sql", "ITEM - AdelWiggins part master",
    ["ITEM_ID", "ITEM_DESC", "ITEM_TYPE", "UNIT_OF_MEASURE", "COST", "ACTIVE_FLAG"],
    [[q(a), q(b), q(c), q(u), "%.2f" % k, q(f)] for a, b, c, u, k, f in ITEMS])

# ---- SUPPLIER_ITEM --------------------------------------------------------
# Purchased items are sourced by commodity family.
FAMILY = [
    (("AL2024BAR", "AL2024SHT", "AL2024STR", "AL6061BAR", "AL7075BAR"), "AWS001", ("AWS002",)),
    (("CR304TUB", "CR316WIR", "CR321BAR", "CR321SHT", "CR155BAR", "CRA286BAR"), "AWS002", ("AWS001",)),
    (("TI64BAR", "TI64SHT", "TICP2TUB"), "AWS003", ("AWS001",)),
    (("ELSIL60", "ELFSIL70", "ELPTFESH", "ORFS908", "ORFS912", "ORVT916", "ORVT924",
      "ORSIL932", "ORNBR908"), "AWS004", ("AWS009",)),
    (("SLPTFEBU", "SLGRAFOL"), "AWS009", ("AWS004",)),
    (("BLTA28610", "BLTA28612", "NUTA286SL", "SCRCAP832", "WSHCRE025", "PINSHR188",
      "PINROLL12", "RIVAL44", "JAMNUT150"), "AWS005", ("AWS002",)),
    (("SPRCOMP01", "SPRTORS02", "RETRING18"), "AWS010", ("AWS005",)),
    (("HTFOIL01", "HTFOIL02", "HTTSTAT1", "HTTCO160", "HTLEAD18", "HTCONN01",
      "HTEPOXY1", "HTSILRUB", "HTGLSCLO"), "AWS007", ()),
    (("CMPTUBE05", "CMPTUBE1", "CMPTUBE2", "CMPADHES"), "AWS008", ()),
    (("HSPTFE04", "HSPTFE08", "HSBRAID1", "HSFITAN4", "HSFIT908"), "AWS009", ("AWS002",)),
    (("FNANODIZ", "FNCHEMFL", "FNPASSIV", "FNNDTFPI"), "AWS006", ("AWS013",)),
    (("FNPRIMER", "FNPAINT1", "FNDRYLUB"), "AWS006", ()),
    (("LBLNAME1", "LBLPART1"), "AWS011", ()),
    (("PKGESDBG", "PKGCART1", "PKGDESIC", "PKGFOAM1"), "AWS012", ()),
]
cost_of = {i[0]: i[4] for i in ITEMS}
si_rows, si_seen = [], set()
for items, primary, alts in FAMILY:
    for iid in items:
        assert iid in ITEM_IDS, iid
        base = cost_of[iid]
        lead = rng.choice([3, 5, 7, 10, 14, 21, 28, 35, 45, 60])
        si_rows.append([q(primary), q(iid), str(lead), "%.2f" % round(base * 0.88, 2), q("Y")])
        si_seen.add((primary, iid))
        for alt in alts:
            if rng.random() < 0.45 and (alt, iid) not in si_seen:
                si_rows.append([q(alt), q(iid), str(lead + rng.choice([5, 10, 14, 21])),
                                "%.2f" % round(base * rng.uniform(0.93, 1.12), 2), q("N")])
                si_seen.add((alt, iid))
si_rows.sort(key=lambda r: (r[0], r[1]))
counts["SUPPLIER_ITEM"] = emit(
    "40_supplier_item.sql", "SUPPLIER_ITEM - approved source list",
    ["SUPPLIER_ID", "ITEM_ID", "LEAD_TIME_DAYS", "PURCHASE_COST", "PRIMARY_SUPPLIER_FLAG"],
    si_rows)

# ---- BOM_HEADER / BOM_DETAIL ---------------------------------------------
mfg_items = [i[0] for i in ITEMS if i[2] == "M"]
for p in STRUCT:
    assert p in ITEM_IDS, "structure for unknown parent %s" % p
    for c, _ in STRUCT[p]:
        assert c in ITEM_IDS, "unknown component %s in %s" % (c, p)
missing = [m for m in mfg_items if m not in STRUCT and m not in NO_BOM]
assert not missing, "manufactured items with no structure: %s" % missing

bh_rows, bd_rows = [], []
seq = 0
bom_of = {}
for parent in mfg_items:
    if parent in NO_BOM:
        continue
    seq += 1
    bom_id = "BM%05d" % seq
    eff = dt.date(2019, 1, 1) + dt.timedelta(days=rng.randrange(0, 2000))
    bh_rows.append([q(bom_id), q(parent), q("A"), d(eff)])
    bom_of.setdefault(parent, []).append(bom_id)
    for n, (comp, qty) in enumerate(STRUCT[parent], start=1):
        bd_rows.append([q(bom_id), str(n * 10), q(comp), "%.3f" % qty])

for parent, (eff, edits) in REV_B.items():
    seq += 1
    bom_id = "BM%05d" % seq
    bh_rows.append([q(bom_id), q(parent), q("B"), d(eff)])
    bom_of.setdefault(parent, []).append(bom_id)
    merged = dict(STRUCT[parent])
    for comp, qty in edits:
        merged[comp] = qty
    for n, (comp, qty) in enumerate(merged.items(), start=1):
        bd_rows.append([q(bom_id), str(n * 10), q(comp), "%.3f" % qty])

counts["BOM_HEADER"] = emit(
    "50_bom_header.sql", "BOM_HEADER - product structure headers",
    ["BOM_ID", "PARENT_ITEM_ID", "REVISION", "EFFECTIVE_DATE"], bh_rows)
counts["BOM_DETAIL"] = emit(
    "60_bom_detail.sql", "BOM_DETAIL - product structure component lines",
    ["BOM_ID", "LINE_NO", "COMPONENT_ITEM_ID", "QUANTITY"], bd_rows, chunk=100)

# ---- INVENTORY ------------------------------------------------------------
FG_PREFIX = ("AW", "W9", "W7", "C3", "C6", "C8", "G6", "QD", "P9", "PV", "FR",
             "HOSE", "ISO", "HTR", "CLP", "GRM", "WM")


def stocking(item_id, itype):
    if item_id.startswith("WM"):
        return ["ELP", "LAX", "MIA"]
    if itype == "P":
        return ["LAX", "HUN"] if rng.random() < 0.7 else ["LAX", "VAL", "HUN"]
    if item_id.startswith("SA"):
        return ["LAX", "VAL"]
    return ["LAX", "MIA"] if rng.random() < 0.6 else ["LAX", "MIA", "HUN"]


inv_rows = []
for iid, desc, itype, uom, cost, active in ITEMS:
    for wh in stocking(iid, itype):
        if itype == "P":
            rop = round(rng.uniform(40, 900), 2)
        elif iid.startswith("SA"):
            rop = round(rng.uniform(25, 350), 2)
        else:
            rop = round(rng.uniform(8, 160), 2)
        r = rng.random()
        if active == "N":
            onhand = round(rng.uniform(0, rop * 0.4), 2)
        elif r < 0.18:                       # below reorder point - shortage
            onhand = round(rng.uniform(0, rop * 0.85), 2)
        elif r < 0.26:                       # excess / slow mover
            onhand = round(rop * rng.uniform(4.0, 7.5), 2)
        else:
            onhand = round(rop * rng.uniform(1.1, 3.4), 2)
        onorder = 0.0 if rng.random() < 0.45 else round(rop * rng.uniform(0.4, 2.2), 2)
        recv = to_workday(TODAY - dt.timedelta(days=rng.randrange(3, 240)))
        issue = to_workday(TODAY - dt.timedelta(days=rng.randrange(1, 200)))
        inv_rows.append([q(iid), q(wh), "%.2f" % onhand, "%.2f" % onorder,
                         "%.2f" % rop, d(recv), d(issue)])
counts["INVENTORY"] = emit(
    "70_inventory.sql", "INVENTORY - on-hand balances by stocking location",
    ["ITEM_ID", "WH_CODE", "QTY_ON_HAND", "QTY_ON_ORDER", "REORDER_POINT",
     "LAST_RECEIPT_DATE", "LAST_ISSUE_DATE"], inv_rows, chunk=100)

# ---- MANUFACTURING_ORDERS -------------------------------------------------
OPERATORS = ["OP1042", "OP1087", "OP1113", "OP1156", "OP1204", "OP1238", "OP1271",
             "OP1319", "OP1364", "OP1402", "OP1448", "OP1490", "OP1536", "OP1578",
             "OP1611", "OP1654", "OP1698", "OP1733", "OP1776", "OP1820", "OP1865",
             "OP1907", "OP1942", "OP1988"]

NOTES_OPEN = [
    "Standard routing, no deviations. Release to LAX cell 4.",
    "Customer AOG pull-in requested - expedite through NDT.",
    "FAI required per AS9102 on this revision before shipment.",
    "Kit shortage: A286 hardware on backorder from Precision Fastener Group.",
    "Anodize line at capacity - routed to outside processor.",
    "Split across shifts 1 and 2 to protect the Friday shipment.",
    "Qualification build supporting AS7510 lightning test article.",
    "Customer source inspection scheduled at final acceptance.",
    "Held for engineering disposition on tolerance deviation request.",
    "Rev B effectivity - verify seal kit callout before kitting.",
    "Long-lead titanium bar released early against forecast.",
    "Spares order for MIA depot replenishment.",
    "Mining channel order - ELP stock replenishment.",
    "Dual-lot build; segregate serial ranges at pack out.",
    "Traveller reprinted after routing update; use rev 4 only.",
]
NOTES_DONE = [
    "Closed complete. All dimensional and NDT results acceptable.",
    "Closed complete. Source inspection passed on first presentation.",
    "Closed short - balance cancelled at customer request.",
    "Closed complete after rework of anodize coverage on two units.",
    "Closed complete. FAI package submitted with shipment.",
    "Closed complete. Proof pressure test recorded at 250 PSIG.",
    "Closed complete, one unit scrapped for bore undersize at op 40.",
    "Closed complete. Lot released from penetrant hold on retest.",
    "Closed complete. Shipped ahead of need date.",
    "Closed complete. Cushion bond verified per process spec.",
]

# Orders come off a *steady release rate* rather than a fixed per-status quota.
# Quotas produced a hockey stick: closed orders spread over a year while every
# open order sat in the last seven weeks, so the shop read as ~10 orders/month
# for eleven months and then 149 in six.  Here a constant number of orders is
# released each working day and the status falls out of where each one sits in
# its own lifecycle, which is what keeps the monthly profile flat.
#
# The planned backlog is generated separately -- it is forward-looking work
# created by the last MRP run, not part of the historical release stream.
HISTORY_WORKDAYS = 260              # ~12 months of working days
RELEASES_PER_WORKDAY = 7.7          # ~150 open orders at a ~19-workday lead
PLANNED_BACKLOG = 120

buildable = [m for m in mfg_items if m in STRUCT]
active = {i[0]: i[5] for i in ITEMS}


def pick_weight(item_id):
    if item_id.startswith(("AW", "W9", "QD", "P9", "CLP")):
        return 3.0
    if item_id.startswith("SA"):
        return 2.2
    if item_id.startswith(("ISO", "WM")):
        return 0.8
    return 1.0


ACTIVE_POOL = [m for m in buildable if active[m] == "Y"]
ACTIVE_WEIGHTS = [pick_weight(m) for m in ACTIVE_POOL]
# Discontinued parts appear only in old closed history.  An obsolete part with
# a live order on the floor reads as a data error, not as an exception.
OBSOLETE_POOL = [m for m in buildable if active[m] == "N"]

mo_rows = []
seq = 0


def add_order(item, odate, exp, actual, status, notes):
    global seq
    seq += 1
    is_fg = not item.startswith("SA")
    qty = round(rng.choice([12, 20, 24, 25, 36, 40, 48, 50, 60, 72, 80, 96, 100,
                            120, 144, 150, 200, 240, 250, 300]) *
                (1.0 if is_fg else rng.choice([2, 2, 3, 4])), 2)
    if status == "P":
        scrap = 0.0                   # nothing built yet, nothing scrapped
    elif status == "R":
        scrap = 0.0 if rng.random() < 0.70 else round(qty * rng.uniform(0.004, 0.030), 2)
    else:
        scrap = 0.0 if rng.random() < 0.50 else round(qty * rng.uniform(0.004, 0.045), 2)
    mo_rows.append([q("MO%06d" % (100000 + seq)), q(item), d(odate), d(exp), d(actual),
                    "%.2f" % qty, "%.2f" % scrap, q(status),
                    q(rng.choice("112233")), q(rng.choice(OPERATORS)), q(notes)])


# --- the release stream: closed history plus what is live on the floor -----
for offset in range(HISTORY_WORKDAYS):
    released_today = int(RELEASES_PER_WORKDAY) + (
        1 if rng.random() < RELEASES_PER_WORKDAY % 1 else 0)
    odate = add_workdays(TODAY, -offset)
    for _ in range(released_today):
        lead = rng.randrange(3, 25)               # lead time in working days
        exp = add_workdays(odate, lead)
        past_due = (TODAY - exp).days

        if past_due < 0:
            status = "R"                          # still inside its window
        elif past_due < 45:
            status = "C" if rng.random() < 0.78 else "R"      # recently late
        else:
            status = "C" if rng.random() < 0.995 else "R"     # a few stuck jobs

        if status == "C":
            actual = add_workdays(exp, rng.randrange(-3, 7))
            if actual > TODAY:
                actual = to_workday(TODAY)
            if actual < odate:
                actual = odate
            notes = rng.choice(NOTES_DONE)
            # Only long-closed orders may be for a discontinued part.
            if past_due > 60 and OBSOLETE_POOL and rng.random() < 0.004:
                item = rng.choice(OBSOLETE_POOL)
            else:
                item = rng.choices(ACTIVE_POOL, weights=ACTIVE_WEIGHTS, k=1)[0]
        else:
            actual = None
            notes = rng.choice(NOTES_OPEN)
            item = rng.choices(ACTIVE_POOL, weights=ACTIVE_WEIGHTS, k=1)[0]

        add_order(item, odate, exp, actual, status, notes)

# --- the planned backlog: firm-planned work not yet on the floor -----------
# Creation dates sit in the last fortnight.  Spreading them wider does not
# help: the current month is only part-elapsed, so everything pushed backwards
# lands on the previous one and makes the tail of the chart lumpier, not less.
for _ in range(PLANNED_BACKLOG):
    odate = add_workdays(TODAY, -rng.randrange(0, 11))
    exp = add_workdays(TODAY, rng.randrange(4, 60))
    item = rng.choices(ACTIVE_POOL, weights=ACTIVE_WEIGHTS, k=1)[0]
    add_order(item, odate, exp, None, "P", rng.choice(NOTES_OPEN))

mo_rows.sort(key=lambda r: r[0])
counts["MANUFACTURING_ORDERS"] = emit(
    "80_mfg_orders.sql", "MANUFACTURING_ORDERS - shop orders",
    ["ORDER_ID", "ITEM_ID", "ORDER_DATE", "EXPECTED_COMPLETION_DATE",
     "ACTUAL_COMPLETION_DATE", "QUANTITY_ORDERED", "QUANTITY_SCRAPPED",
     "STATUS", "SHIFT_CODE", "OPERATOR_ID", "NOTES"], mo_rows, chunk=100)

for t in ("WAREHOUSE", "SUPPLIER", "ITEM", "SUPPLIER_ITEM", "BOM_HEADER",
          "BOM_DETAIL", "INVENTORY", "MANUFACTURING_ORDERS"):
    print("%-22s %5d rows" % (t, counts[t]))
