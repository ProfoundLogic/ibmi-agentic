#!/usr/bin/env python3
"""Render an EJS screen offline and screenshot it, using real session data.

  tools/ejs-screenshot.py <rdf.json> <session-screen.json> <out.png>

Renders through `aitool ejs-validate` (same engine the pre-flight uses), wraps
the HTML with the stylesheets the RDF declares, and screenshots it in headless
Chromium. Catches blank screens and content spilling off the right edge before
anyone sees them in a browser.
"""
import base64, json, os, subprocess, sys

ROOT = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..")
DOCROOT = os.path.join(ROOT, "htdocs")
CHROME = "/home/coder/.cache/ms-playwright/chromium-1228/chrome-linux64/chrome"


def resolve(url):
    return os.path.normpath(os.path.join(DOCROOT, url.split("?")[0].lstrip("/")))


def ctx_from_session(screen_json):
    """Turn a Genie session screen capture into an EJS data context."""
    s = json.load(open(screen_json))
    fmt = next(f for f in s["layers"][0]["formats"] if f.get("active"))
    ctx = {k.lower(): v for k, v in (fmt.get("data") or {}).items()}
    for name, sfl in (fmt.get("subfiles") or {}).items():
        names = [n.lower() for n in sfl["field names"]]
        rows = []
        for i, rec in enumerate(sfl.get("data") or []):
            row = dict(zip(names, rec))
            row["_rrn"] = i + 1
            rows.append(row)
        ctx[name.lower()] = rows
    return fmt["name"], ctx


def fmt_all_subfiles(fmt):
    return list((fmt.get("subfiles") or {}).keys())


def main(rdf_path, screen_json, out_png):
    rdf = json.load(open(rdf_path))
    fmt_name, ctx = ctx_from_session(screen_json)
    fmt = next(v for k, v in rdf["formats"].items() if k.lower() == fmt_name.lower())

    # `aitool ejs-validate` truncates its stdout at about 64 KB, so a long
    # subfile produces a response that is cut mid-string and will not parse.
    # That is a limit of the renderer, not a fault in the template: halve the
    # rows and retry, and say what was dropped rather than screenshotting a
    # shorter grid as if it were the whole thing.
    tpl = open(resolve(fmt["template"])).read()
    sfl_keys = [k.lower() for k in (fmt_all_subfiles(fmt) or [])]
    rows_used = None
    r = None
    while True:
        payload = {"template": tpl, "data": ctx,
                   "filename": os.path.basename(fmt["template"])}
        p = subprocess.run(["aitool", "ejs-validate", "--input", "-"],
                           input=json.dumps(payload), capture_output=True, text=True)
        try:
            r = json.loads(p.stdout or p.stderr)
            break
        except json.JSONDecodeError:
            longest = max((k for k in sfl_keys if isinstance(ctx.get(k), list)),
                          key=lambda k: len(ctx[k]), default=None)
            if longest is None or len(ctx[longest]) <= 4:
                print("RENDER FAILED: renderer response would not parse even at "
                      "4 subfile rows")
                return 1
            keep = max(4, len(ctx[longest]) // 2)
            print(f"  note: renderer response exceeded its output limit - "
                  f"retrying with {keep} of {len(ctx[longest])} {longest} rows")
            ctx[longest] = ctx[longest][:keep]
            rows_used = (longest, keep)
    if not r.get("success"):
        print("RENDER FAILED:", r.get("error", {}).get("message", "")[:300])
        return 1
    body = r["data"]["html"]
    if len(body.strip()) < 200:
        print(f"WARNING: rendered HTML is only {len(body.strip())} bytes - likely blank")

    css = "\n".join(open(resolve(u)).read() for u in fmt.get("css", [])
                    if os.path.isfile(resolve(u)))
    html = (f"<!DOCTYPE html><html><head><meta charset='utf-8'><style>\n"
            f"body{{margin:0;background:#fff}}\n{css}\n</style></head>"
            f"<body>{body}</body></html>")
    tmp = "/tmp/gen/screen.html"
    os.makedirs("/tmp/gen", exist_ok=True)
    open(tmp, "w", encoding="utf-8").write(html)

    subprocess.run([CHROME, "--headless", "--no-sandbox", "--hide-scrollbars",
                    "--window-size=1400,1000", f"--screenshot={out_png}",
                    "file://" + tmp], capture_output=True)
    ok = os.path.isfile(out_png) and os.path.getsize(out_png) > 2000
    print(f"  {'ok  ' if ok else 'FAIL'} {os.path.basename(out_png)}  "
          f"html={len(body)}b  png={os.path.getsize(out_png) if os.path.isfile(out_png) else 0}b")
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main(*sys.argv[1:4]))
