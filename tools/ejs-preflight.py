#!/usr/bin/env python3
"""Pre-flight every EJS template referenced by a Profound UI RDF JSON.

The data context is synthesised FROM the RDF field list, so this is a contract
test between template and RDF, not just a syntax check: a template referencing
a field the RDF never declares fails here rather than rendering a blank screen
in front of a customer.

  tools/ejs-preflight.py <rdf.json> [<rdf.json> ...]

Exit 0 = all templates render. Exit 1 = at least one failed.
"""
import json, os, subprocess, sys

DOCROOT = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "htdocs")
SAMPLE_ROWS = 3


def sample(spec):
    t = (spec.get("type") or "char").lower()
    if t in ("zoned", "packed", "int", "float"):
        return 0 if not spec.get("decimals") else 0.0
    return ""


def context(fmt):
    ctx = {name: sample(spec) for name, spec in (fmt.get("fields") or {}).items()}
    for sflname, sfl in (fmt.get("subfiles") or {}).items():
        row = {n: sample(s) for n, s in (sfl.get("fields") or {}).items()}
        ctx[sflname] = [dict(row, _rrn=i + 1) for i in range(SAMPLE_ROWS)]
    return ctx


def resolve(url):
    """Map a /profoundui/... URL onto the repo htdocs tree.

    Cache-bust query strings (?v=N) are stripped - they matter to the browser
    but not to the filesystem."""
    url = url.split("?", 1)[0].split("#", 1)[0]
    return os.path.normpath(os.path.join(DOCROOT, url.lstrip("/")))


def check(rdf_path):
    rdf = json.load(open(rdf_path))
    failed = 0
    for fname, fmt in (rdf.get("formats") or {}).items():
        tpl_url = fmt.get("template")
        if not tpl_url:
            continue
        tpl = resolve(tpl_url)
        label = f"{os.path.basename(rdf_path)}::{fname}"
        if not os.path.isfile(tpl):
            print(f"  FAIL {label}\n       template not found: {tpl}")
            failed += 1
            continue
        payload = {"template": open(tpl).read(),
                   "data": context(fmt),
                   "filename": os.path.basename(tpl)}
        p = subprocess.run(["aitool", "ejs-validate", "--input", "-"],
                           input=json.dumps(payload), capture_output=True, text=True)
        try:
            r = json.loads(p.stdout or p.stderr)
        except Exception:
            r = {"success": False, "error": {"message": (p.stdout or p.stderr)[:300]}}
        if r.get("success"):
            nf = len(fmt.get("fields") or {})
            ns = len(fmt.get("subfiles") or {})
            print(f"  ok   {label}  ({nf} fields, {ns} subfile(s))")
        else:
            print(f"  FAIL {label}\n       {r.get('error', {}).get('message', '')[:400]}")
            failed += 1
        for kind in ("css", "js"):
            for url in fmt.get(kind) or []:
                if not os.path.isfile(resolve(url)):
                    print(f"  FAIL {label}\n       missing {kind}: {url}")
                    failed += 1
    return failed


if __name__ == "__main__":
    if len(sys.argv) < 2:
        sys.exit(__doc__)
    total = sum(check(p) for p in sys.argv[1:])
    print(f"\n  {'FAILED - ' + str(total) + ' problem(s)' if total else 'all templates rendered'}")
    sys.exit(1 if total else 0)
