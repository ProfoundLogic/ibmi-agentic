#!/usr/bin/env python3
"""Load the AdelWiggins demo dataset into an IBM i library.

    awdemo/tools/load-awdemo-data.py [LIBRARY] [--keep]

LIBRARY defaults to $IBMI_BUILD_LIBRARY.  The tables must already exist; build
them first with codermake (see awdemo/README.md).

Every table is emptied before loading so the script is repeatable; --keep
appends instead.

Table names in sql/data/*.sql are unqualified and are rewritten here to
LIBRARY.TABLE.  That matters: an unqualified INSERT resolves against the job
library list and can silently land in the wrong library.
"""

import json
import os
import re
import subprocess
import sys
import tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
DATA_DIR = os.path.join(HERE, "..", "sql", "data")
CONNECTION = os.environ.get("AWDEMO_SQL_CONNECTION", "dev")

TABLES = ["WAREHOUSE", "SUPPLIER", "ITEM", "SUPPLIER_ITEM", "BOM_HEADER",
          "BOM_DETAIL", "INVENTORY", "MANUFACTURING_ORDERS"]


def run_sql(sql, fmt="json", timeout=180000):
    """Run one statement through the aitool sql skill.

    The payload goes via a temp file: inline --input strings mangle embedded
    single quotes.  aitool's exit code is not reliable, so parse .success.
    """
    with tempfile.NamedTemporaryFile("w", suffix=".json", delete=False) as fh:
        json.dump({"connection": CONNECTION, "quiet": True, "format": fmt,
                   "sql": sql, "timeout": timeout}, fh)
        path = fh.name
    try:
        proc = subprocess.run(["aitool", "sql", "--input", "@" + path],
                              capture_output=True, text=True)
    finally:
        os.unlink(path)
    out = proc.stdout.strip() or proc.stderr.strip()
    if fmt != "json":
        return out
    try:
        result = json.loads(out)
    except ValueError:
        raise SystemExit("unparseable aitool response:\n%s" % out[:2000])
    if not result.get("success"):
        raise SystemExit("SQL failed:\n  %s\n  %s"
                         % (sql[:160], json.dumps(result.get("error"))[:600]))
    return result


def split_statements(text):
    """Split on ';' -- but only outside string literals.

    Order notes legitimately contain semicolons, so a naive text.split(";")
    tears statements in half.
    """
    out, buf, in_str = [], [], False
    i, n = 0, len(text)
    while i < n:
        ch = text[i]
        if in_str:
            if ch == "'":
                if i + 1 < n and text[i + 1] == "'":   # doubled = escaped quote
                    buf.append("''")
                    i += 2
                    continue
                in_str = False
            buf.append(ch)
        elif ch == "'":
            in_str = True
            buf.append(ch)
        elif ch == ";":
            out.append("".join(buf))
            buf = []
        else:
            buf.append(ch)
        i += 1
    out.append("".join(buf))
    return [s.strip() for s in out if s.strip()]


def statements(path, lib):
    text = re.sub(r"^\s*--.*$", "", open(path).read(), flags=re.M)
    for stmt in split_statements(text):
        yield re.sub(r"^INSERT INTO (\w+)", r"INSERT INTO %s.\1" % lib, stmt)


def main():
    args = [a for a in sys.argv[1:] if not a.startswith("--")]
    keep = "--keep" in sys.argv[1:]
    lib = (args[0] if args else os.environ.get("IBMI_BUILD_LIBRARY", "")).upper()
    if not lib:
        raise SystemExit("error: no library given and IBMI_BUILD_LIBRARY is not set")

    print("Target library: %s   (connection: %s)" % (lib, CONNECTION))

    if not keep:
        print("Clearing existing rows...")
        for table in reversed(TABLES):          # reverse order keeps FKs happy
            run_sql("DELETE FROM %s.%s" % (lib, table))

    for name in sorted(os.listdir(DATA_DIR)):
        if not name.endswith(".sql"):
            continue
        path = os.path.join(DATA_DIR, name)
        n = 0
        for stmt in statements(path, lib):
            run_sql(stmt)
            n += 1
        print("  loaded %-24s %d statement(s)" % (name, n))

    counts = " UNION ALL ".join(
        "SELECT '%s' AS TABLE_NAME, COUNT(*) AS ROW_COUNT FROM %s.%s" % (t, lib, t)
        for t in TABLES)
    print(run_sql(counts, fmt="table"))


if __name__ == "__main__":
    main()
