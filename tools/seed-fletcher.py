#!/usr/bin/env python3
"""Qualify the Fletcher seed statements with a target library and execute them.

Note: the SQL client executes only the LAST statement of a semicolon-separated
batch, so every statement is sent individually. Consecutive INSERTs into the
same table are folded into multi-row VALUES to keep the call count sane.
"""
import json, re, subprocess, sys

LIB, PATH = sys.argv[1], sys.argv[2]
TABLES = ["FLCUSTP", "FLSITEP", "FLMODLP", "FLMACHP", "FLOPTNP",
          "FLPARTP", "FLBOMP", "FLSVCP", "FLPORDP"]
ROWS_PER_STMT = 100

raw = []
for line in open(PATH):
    line = line.strip()
    if not line or line.startswith("--"):
        continue
    for t in TABLES:
        line = re.sub(rf"\b(INTO|FROM)\s+{t}\b", rf"\1 {LIB}.{t}", line)
    raw.append(line.rstrip(";"))

# fold consecutive same-target INSERTs into multi-row VALUES
INS = re.compile(r"^INSERT INTO (\S+) \(([^)]*)\) VALUES (\(.*\))$", re.I)
stmts, pend_key, pend_vals = [], None, []

def flush():
    global pend_key, pend_vals
    if pend_key:
        tbl, cols = pend_key
        for i in range(0, len(pend_vals), ROWS_PER_STMT):
            chunk = pend_vals[i:i + ROWS_PER_STMT]
            stmts.append(f"INSERT INTO {tbl} ({cols}) VALUES " + ", ".join(chunk))
    pend_key, pend_vals = None, []

for s in raw:
    m = INS.match(s)
    if m:
        k = (m.group(1), m.group(2))
        if k != pend_key:
            flush()
            pend_key = k
        pend_vals.append(m.group(3))
    else:
        flush()
        stmts.append(s)
flush()

ok = bad = 0
for n, s in enumerate(stmts, 1):
    p = subprocess.run(["aitool", "sql", "--input", "-"],
                       input=json.dumps({"connection": "dev", "sql": s}),
                       capture_output=True, text=True)
    try:
        r = json.loads(p.stdout)
    except Exception:
        r = {"success": False, "error": {"message": (p.stdout or p.stderr)[:200]}}
    if r.get("success"):
        ok += 1
    else:
        bad += 1
        print(f"\n  FAILED: {s[:70]}...\n    {r.get('error', {}).get('message', '')[:200]}")
    print(f"\r  {n}/{len(stmts)} statements", end="", flush=True)

print(f"\n  applied {ok}, failed {bad}")
sys.exit(1 if bad else 0)
