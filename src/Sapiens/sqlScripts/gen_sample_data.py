#!/usr/bin/env python3
"""Generate INSERT statements for the reverse-engineered Sapiens PFs.

Produces one .sql file per table with >= 30 rows. Uses deterministic but
plausible-looking sample data (insurance agencies in the US Midwest).
"""

import os, random

random.seed(42)
OUT = "/workspace/ibmi-agentic/src/Sapiens/sqlScripts"
os.makedirs(OUT, exist_ok=True)

AGENCIES = [
    "Heritage Insurance Group", "Cardinal Brokers LLC", "Lakeshore Risk Mgmt",
    "Summit Coverage Partners", "Bluegrass Insurance Agency", "Riverbend Underwriters",
    "Northwoods Risk Advisors", "Stonebridge Insurance Group", "Prairie State Agency",
    "Buckeye Coverage Solutions", "Tri-State Risk Brokers", "Liberty Bell Insurance",
    "Iron Mountain Insurance", "Crossroads Insurance Inc", "Lakefront Agency LLC",
    "Capital City Brokers", "Midwest Mutual Brokers", "Crescent Risk Group",
    "Pinnacle Insurance Group", "Cornerstone Coverage", "Rolling Hills Insurance",
    "Falcon Insurance Services", "Eagle Bend Brokers", "Maple Leaf Insurance",
    "Greater Lakes Insurance", "Sunrise Risk Partners", "Watershed Insurance",
    "Anchor Insurance Group", "Beacon Insurance LLC", "Lighthouse Brokers Inc",
    "Cobblestone Insurance", "Lighthouse Risk Co", "Riverstone Insurance",
    "Briarwood Insurance Group",
]

CITIES_MIDWEST = [
    ("Columbus", "OH", "43215"), ("Indianapolis", "IN", "46204"),
    ("Detroit", "MI", "48201"), ("Chicago", "IL", "60601"),
    ("Milwaukee", "WI", "53202"), ("Minneapolis", "MN", "55401"),
    ("St Louis", "MO", "63101"), ("Kansas City", "MO", "64108"),
    ("Cleveland", "OH", "44113"), ("Cincinnati", "OH", "45202"),
    ("Madison", "WI", "53703"), ("Des Moines", "IA", "50309"),
    ("Omaha", "NE", "68102"), ("Louisville", "KY", "40202"),
    ("Lansing", "MI", "48933"), ("Springfield", "IL", "62701"),
    ("Toledo", "OH", "43604"), ("Grand Rapids", "MI", "49503"),
    ("Fort Wayne", "IN", "46802"), ("Akron", "OH", "44308"),
    ("Dayton", "OH", "45402"), ("Naperville", "IL", "60540"),
    ("Rockford", "IL", "61101"), ("Peoria", "IL", "61602"),
    ("Gary", "IN", "46402"), ("Saginaw", "MI", "48601"),
    ("Flint", "MI", "48502"), ("Green Bay", "WI", "54301"),
    ("Bloomington", "IN", "47404"), ("South Bend", "IN", "46601"),
    ("Ann Arbor", "MI", "48104"), ("Battle Creek", "MI", "49014"),
    ("Joliet", "IL", "60432"), ("Champaign", "IL", "61820"),
]

COUNTIES = [
    "Franklin", "Marion", "Wayne", "Cook", "Milwaukee", "Hennepin",
    "St Louis", "Jackson", "Cuyahoga", "Hamilton", "Dane", "Polk",
    "Douglas", "Jefferson", "Ingham", "Sangamon", "Lucas", "Kent",
    "Allen", "Summit", "Montgomery", "DuPage", "Winnebago", "Peoria",
    "Lake", "Genesee", "Saginaw", "Brown", "Monroe", "St Joseph",
    "Washtenaw", "Calhoun", "Will", "Champaign",
]

CARRIER_CODES = ["WC101", "WC202", "WC303", "WC404", "WC505",
                 "GL110", "GL220", "GL330", "GL440", "GL550",
                 "AU150", "AU260", "AU370", "AU480", "AU590",
                 "PL120", "PL230", "PL340", "PL450", "PL560",
                 "BO130", "BO240", "BO350", "BO460", "BO570",
                 "EX140", "EX250", "EX360", "EX470", "EX580"]

CARRIER_NAMES = [
    "WorkComp National", "WorkComp Pacific", "WorkComp Atlantic",
    "WorkComp Midwest", "WorkComp Southern",
    "GenLiab Underwriters", "GenLiab Prime", "GenLiab Direct",
    "GenLiab Select", "GenLiab Allied",
    "Auto Insure One", "Auto Coverage Plus", "Auto Specialty",
    "Auto Select Risk", "Auto Premier",
    "Property One", "Property Coverage", "Property Plus",
    "Property Direct", "Property Premier",
    "Bond One Surety", "Bond Coverage", "Bond Specialty",
    "Bond Direct", "Bond Premier",
    "Excess Liab One", "Excess Coverage", "Excess Specialty",
    "Excess Direct", "Excess Premier",
]

def sql_str(s, n):
    """Pad/truncate s to fit char(n), single-quote it."""
    s = s[:n] if s else ""
    return "'" + s.replace("'", "''") + "'"

def gen_wmagp():
    lines = []
    for i in range(35):
        co_no = (i % 3) + 1  # 1, 2, 3
        fein  = 100000000 + i * 1234567 % 800000000
        mod   = (i % 5) + 1
        name  = AGENCIES[i % len(AGENCIES)]
        shnm  = name.split()[0] + " Ins"
        fdnm  = name + " DBA"
        agt   = 1000 + i
        city, st, zip5 = CITIES_MIDWEST[i % len(CITIES_MIDWEST)]
        mad1  = f"{100 + i*5} {city.split()[-1]} Ave"
        pad1  = f"{200 + i*7} Commerce Pkwy"
        cnty  = COUNTIES[i % len(COUNTIES)]
        phn1  = 5550100 + i
        phn2  = 5559000 + i
        area  = 614 if st == "OH" else 312 if st == "IL" else 313 if st == "MI" else 800
        email = f"contact{i+1}@{name.lower().split()[0]}.example"
        cont  = "Pat Buyer" if i % 2 == 0 else "Sam Risk"
        secy  = "Lee Admin" if i % 3 == 0 else "Robin Asst"
        comm  = f"Sample agency #{i+1} (REVERSE-ENGINEERED demo data)"
        stat  = 'A'
        act   = 'A'
        lines.append(f"""INSERT INTO WMAGP (AGCO#, AGFEIN, AGMOD, AGNAME, AGSHNM, AGFDNM,
  AGAGT#, AGSTAT, AGACT, AGMAD1, AGMCTY, AGMST, AGMZIP, AGPAD1, AGPCTY, AGPST, AGPZIP,
  AGCNTY, AGPHN1, AGPHN2, AGAREA, AGARE1, AGARE2, AGEMAL, AGCONT, AGCOMM)
VALUES ({co_no}, {fein}, {mod}, {sql_str(name,40)}, {sql_str(shnm,40)}, {sql_str(fdnm,40)},
  {agt}, 'A', 'A', {sql_str(mad1,40)}, {sql_str(city,25)}, '{st}', {int(zip5)},
  {sql_str(pad1,40)}, {sql_str(city,25)}, '{st}', {int(zip5)},
  {sql_str(cnty,20)}, {phn1}, {phn2}, {area}, {area}, {area},
  {sql_str(email,60)}, {sql_str(cont,40)}, {sql_str(comm,60)});""")
    with open(f"{OUT}/wmagp.sql","w") as f:
        f.write("-- WMAGP sample data (REVERSE-ENGINEERED)\n")
        f.write("\n".join(lines))
        f.write("\n")

def gen_smcop():
    lines = []
    for i in range(35):
        co_no = i + 1
        name = ["StoneRiver Workers Comp","Sapiens Insurance Corp","Midwest Mutual Carrier",
                "Heritage National Ins","Pinnacle Risk Carrier","Buckeye Group Ins",
                "Lakeshore Risk Pool","Greater Lakes Mutual","Tri-State Carrier","Capital Carriers",
                "Northwoods Mutual","Cardinal Insurance Co","Crossroads Carrier","Liberty Bell Mutual",
                "Stonebridge National","Iron Mountain Risk","Eagle Bend National","Maple Leaf Carrier",
                "Riverbend National","Bluegrass Carrier","Cornerstone National","Summit National",
                "Beacon National Carriers","Lighthouse National","Anchor National Carrier",
                "Cobblestone National","Watershed Carrier","Sunrise National","Falcon National",
                "Briarwood Carrier","Crescent National","Prairie Carrier","Rolling Hills Co",
                "Riverstone National","Cardinal Mutual"][i % 35]
        text = f"Sample insurance carrier #{i+1}"
        lines.append(f"INSERT INTO SMCOP (\"CO#\", CONAME, COTEXT) "
                     f"VALUES ({co_no}, {sql_str(name,40)}, {sql_str(text,60)});")
    with open(f"{OUT}/smcop.sql","w") as f:
        f.write("-- SMCOP sample data (REVERSE-ENGINEERED)\n")
        f.write("\n".join(lines)); f.write("\n")

def gen_wmaap():
    lines = []
    for i in range(35):
        co  = (i % 3) + 1
        fein = 100000000 + i * 1234567 % 800000000
        mod  = (i % 5) + 1
        fnd  = (i % 6) + 1
        agt  = 1000 + i
        name = AGENCIES[i % len(AGENCIES)][:40]
        lines.append(f"INSERT INTO WMAAP (\"AACO#\", AAFEIN, AAMOD, AAFND, \"AAAGT#\", AANAME, AAL)"
                     f" VALUES ({co}, {fein}, {mod}, {fnd}, {agt}, {sql_str(name,40)}, 'A');")
    with open(f"{OUT}/wmaap.sql","w") as f:
        f.write("-- WMAAP sample data (REVERSE-ENGINEERED)\n")
        f.write("\n".join(lines)); f.write("\n")

def gen_wdf2p():
    lines = []
    for i in range(35):
        co  = (i % 3) + 1
        fnd = (i % 6) + 1
        carr = CARRIER_CODES[i % len(CARRIER_CODES)]
        nm   = CARRIER_NAMES[i % len(CARRIER_NAMES)]
        lines.append(f"INSERT INTO WDF2P (\"DFCO#\", DFFND, DFCARR, DFTEXT)"
                     f" VALUES ({co}, {fnd}, {sql_str(carr,10)}, {sql_str(nm,40)});")
    with open(f"{OUT}/wdf2p.sql","w") as f:
        f.write("-- WDF2P sample data (REVERSE-ENGINEERED)\n")
        f.write("\n".join(lines)); f.write("\n")

def gen_wmczp():
    lines = []
    for i in range(35):
        co = (i % 3) + 1
        city, st, zip5 = CITIES_MIDWEST[i % len(CITIES_MIDWEST)]
        cnty = COUNTIES[i % len(COUNTIES)]
        lines.append(f"INSERT INTO WMCZP (\"CZCO#\", CZST, CZZIP, CZCNTY, CZP)"
                     f" VALUES ({co}, '{st}', {int(zip5)}, {sql_str(cnty,20)}, 'P');")
    with open(f"{OUT}/wmczp.sql","w") as f:
        f.write("-- WMCZP sample data (REVERSE-ENGINEERED)\n")
        f.write("\n".join(lines)); f.write("\n")

def gen_wdelp():
    lines = []
    for i in range(35):
        co  = (i % 3) + 1
        fnd = (i % 6) + 1
        emp = 1000000 + i
        div = chr(ord('A') + (i % 26)) + str(i % 10)
        fyr = 2024 + (i % 3)
        lines.append(f"INSERT INTO WDELP (\"ELCO#\", ELFND, \"ELEMP#\", ELDIV, ELFYR)"
                     f" VALUES ({co}, {fnd}, {emp}, {sql_str(div,5)}, {fyr});")
    with open(f"{OUT}/wdelp.sql","w") as f:
        f.write("-- WDELP sample data (REVERSE-ENGINEERED)\n")
        f.write("\n".join(lines)); f.write("\n")

for fn in (gen_wmagp, gen_smcop, gen_wmaap, gen_wdf2p, gen_wmczp, gen_wdelp):
    fn()

print("Generated SQL files in", OUT)
for f in sorted(os.listdir(OUT)):
    p = os.path.join(OUT, f)
    print(f"  {f}: {sum(1 for _ in open(p))} lines")
