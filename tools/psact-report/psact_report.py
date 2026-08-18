#!/usr/bin/env python3
"""
PSACT daily activity report generator.

Pulls the presales team's workload out of Jira and renders three artefacts:

  * a rich, animated HTML report (opened in a browser - full interactivity)
  * an email-safe HTML summary (no JS, table layout - survives Gmail/Outlook)
  * a markdown digest for the monthly archive

Run modes are chosen from the weekday unless overridden:

  Monday      "weekly"  - recap of the previous week
  Tue-Fri     "daily"   - prior day only
  Sat/Sun     skipped   (unless --force)

Authentication reuses the Jira environment variables already configured for
this environment (JIRA_BASE_URL, JIRA_EMAIL, JIRA_API_TOKEN). Credentials are
read from the environment and never logged or echoed.

Usage:
    python3 psact_report.py [--date YYYY-MM-DD] [--mode daily|weekly]
                            [--outdir DIR] [--force]
"""

import argparse
import base64
import datetime as dt
import html
import json
import os
import sys
import urllib.parse
import urllib.request
from collections import defaultdict

try:
    from zoneinfo import ZoneInfo
    TZ = ZoneInfo("America/New_York")
except Exception:                                    # pragma: no cover
    TZ = dt.timezone(dt.timedelta(hours=-4), "EDT")

# --------------------------------------------------------------------------
# Configuration
# --------------------------------------------------------------------------

PROJECT = "PSACT"

# Order here drives the order plates appear in the report, and the order names
# are listed in the header. Brian leads the team, so he comes first.
TEAM = [
    ("Brian May", "bmay@profoundlogic.com"),
    ("Gary Jones", "gjones@profoundlogic.com"),
    ("Roger Betancourt", "rbetancourt@profoundlogic.com"),
]

# Staleness ladder: status -> days before it is called out.
# Keyed on `updated`, not on comment presence, so transitions and edits count.
STALE_DAYS = {"In Progress": 7, "Waiting": 14, "To Do": 21}

# Display names for customer: labels whose spelling title-casing gets wrong -
# initialisms, punctuation, internal capitals. Anything not listed falls back to
# title-casing the label ("hal-leonard" -> "Hal Leonard"), which is right most of
# the time but turns "jh-fletcher" into "Jh Fletcher".
CUSTOMER_NAMES = {
    "jh-fletcher": "J.H. Fletcher",
    "taskforce-italy": "TaskForce Italy",
}

# Design tokens (validated status palette + surfaces from the dataviz skill).
TOK = {
    "surface": "#fcfcfb",
    "surface_dark": "#1a1a19",
    "ink": "#0b0b0b",
    "ink2": "#52514e",
    "good": "#0ca30c",
    "warning": "#fab219",
    "serious": "#ec835a",
    "critical": "#d03b3b",
    "blue": "#2a78d6",
    "violet": "#4a3aa7",
    "aqua": "#1baf7a",
}

# Status colour + icon. Status colour never carries meaning alone - every use
# is paired with a text label, per the dataviz status-palette rule.
STALE_TIERS = [
    (3.0, "critical", "●", "Critical"),   # >= 3x the threshold
    (2.0, "serious", "◐", "Serious"),     # >= 2x
    (1.0, "warning", "◑", "Ageing"),      # over threshold
]


# --------------------------------------------------------------------------
# Jira client
# --------------------------------------------------------------------------

class Jira:
    def __init__(self):
        self.base = os.environ["JIRA_BASE_URL"].rstrip("/")
        email = os.environ["JIRA_EMAIL"]
        token = os.environ["JIRA_API_TOKEN"]
        raw = f"{email}:{token}".encode()
        self._auth = "Basic " + base64.b64encode(raw).decode()

    def _call(self, method, path, body=None, params=None):
        url = self.base + path
        if params:
            url += "?" + urllib.parse.urlencode(params)
        data = json.dumps(body).encode() if body is not None else None
        req = urllib.request.Request(url, data=data, method=method)
        req.add_header("Authorization", self._auth)
        req.add_header("Accept", "application/json")
        if data:
            req.add_header("Content-Type", "application/json")
        with urllib.request.urlopen(req, timeout=60) as r:
            return json.loads(r.read().decode() or "{}")

    def search(self, jql, fields, expand=None):
        """Page through /search/jql until exhausted."""
        out, token = [], None
        while True:
            body = {"jql": jql, "maxResults": 100, "fields": fields}
            if expand:
                body["expand"] = expand
            if token:
                body["nextPageToken"] = token
            d = self._call("POST", "/rest/api/3/search/jql", body)
            out.extend(d.get("issues", []))
            token = d.get("nextPageToken")
            if not token or d.get("isLast", True):
                break
        return out

    def comments(self, key, since=None):
        d = self._call("GET", f"/rest/api/3/issue/{key}/comment",
                       params={"maxResults": 100, "orderBy": "-created"})
        out = []
        for c in d.get("comments", []):
            created = parse_ts(c.get("created"))
            if since and created < since:
                continue
            out.append({
                "author": (c.get("author") or {}).get("displayName", "?"),
                "created": created,
                "text": adf_to_text(c.get("body")),
            })
        return out


# --------------------------------------------------------------------------
# Helpers
# --------------------------------------------------------------------------

def parse_ts(s):
    """Jira timestamps: 2026-08-18T04:12:33.000-0400 -> aware datetime."""
    if not s:
        return None
    try:
        return dt.datetime.strptime(s, "%Y-%m-%dT%H:%M:%S.%f%z")
    except ValueError:
        return dt.datetime.fromisoformat(s)


def adf_to_text(node, out=None):
    """Flatten Atlassian Document Format to plain text."""
    if out is None:
        out = []
    if isinstance(node, dict):
        if node.get("type") == "text":
            out.append(node.get("text", ""))
        for c in node.get("content", []) or []:
            adf_to_text(c, out)
        if node.get("type") in ("paragraph", "listItem", "heading"):
            out.append(" ")
    elif isinstance(node, list):
        for c in node:
            adf_to_text(c, out)
    return " ".join("".join(out).split())


def e(s):
    return html.escape(str(s if s is not None else ""))


def stale_tier(days_idle, threshold):
    """Return (token, icon, label, ratio) or None when the ticket is fresh."""
    if not threshold or days_idle < threshold:
        return None
    ratio = days_idle / threshold
    for cut, token, icon, label in STALE_TIERS:
        if ratio >= cut:
            return token, icon, label, ratio
    return None


def plural(n, one, many=None):
    return one if n == 1 else (many or one + "s")


def customer_name(slug):
    return CUSTOMER_NAMES.get(slug, slug.replace("-", " ").title())


# Leading labels stripped from comment text on display. The convention is
# useful when writing a comment but reads as noise in the report, which is
# already in a "what happened" section. A mid-text "Next:" is left alone - there
# it works as a separator between what was done and what is left.
COMMENT_PREFIXES = ("did:", "did -", "did –")

# Every Jira link opens in a new tab. The report is something people keep open
# and scan through - following a ticket in place would throw away their position,
# and in the emailed attachment it would navigate away from the report entirely.
# rel="noopener" is the standard companion to target="_blank".
LINK_ATTRS = 'target="_blank" rel="noopener noreferrer"'


def clean_comment(text):
    t = (text or "").lstrip()
    low = t.lower()
    for pre in COMMENT_PREFIXES:
        if low.startswith(pre):
            return t[len(pre):].lstrip()
    return t


# --------------------------------------------------------------------------
# Data model
# --------------------------------------------------------------------------

def build_model(jira, today, mode):
    if mode == "weekly":
        win_start = today - dt.timedelta(days=7)
        window_label = (f"week of {win_start:%-d %b} – "
                        f"{today - dt.timedelta(days=1):%-d %b %Y}")
    else:
        win_start = today - dt.timedelta(days=1)
        window_label = f"{win_start:%A %-d %B %Y}"

    # Day boundaries are local to the team (America/New_York), not UTC.
    # In UTC a 9pm EDT comment falls on the following calendar day, which would
    # file evening work under the wrong heading.
    win_start_dt = dt.datetime.combine(win_start, dt.time.min, tzinfo=TZ)
    today_dt = dt.datetime.combine(today, dt.time.min, tzinfo=TZ)

    fields = ["summary", "status", "assignee", "duedate", "labels", "parent",
              "updated", "created", "issuetype", "priority"]

    issues = jira.search(f"project = {PROJECT} AND issuetype != Epic", fields)
    epics = {i["key"]: i["fields"]["summary"] for i in
             jira.search(f"project = {PROJECT} AND issuetype = Epic",
                         ["summary"])}

    # ---- rollup resolution -------------------------------------------------
    # A ticket labelled rollup:<proj> is a placeholder; its real activity lives
    # in another project. Its freshness is the newest update over there, so it
    # is never falsely stale - but still flags when that project goes quiet.
    rollup_cache = {}

    def rollup_activity(proj, scope=None):
        # `scope` is the assignee a `rollup-mine` ticket narrows to. WAT is
        # shared with people outside this team, so an unscoped rollup would
        # report their work as ours; PERP is wholly unassigned, so it stays
        # project-wide.
        key = (proj, scope)
        rollup_cache_key = key
        if rollup_cache_key not in rollup_cache:
            jql = f"project = {proj}"
            if scope:
                jql += f' AND assignee = "{scope}"'
            child = jira.search(
                jql,
                ["summary", "status", "updated", "assignee", "issuetype"])
            newest = max((parse_ts(c["fields"]["updated"]) for c in child),
                         default=None)
            # Count tickets only. Epics are containers over there just as they
            # are in PSACT, so including them double-counts the same work.
            tix = [c for c in child
                   if c["fields"]["issuetype"]["name"] != "Epic"]
            done = sum(1 for c in tix
                       if c["fields"]["status"]["statusCategory"]["key"]
                       == "done")
            rollup_cache[rollup_cache_key] = {
                "project": proj, "total": len(tix), "newest": newest,
                "open": len(tix) - done, "done": done, "scope": scope,
            }
        return rollup_cache[rollup_cache_key]

    rows = []
    for i in issues:
        f = i["fields"]
        status = f["status"]["name"]
        cat = f["status"]["statusCategory"]["key"]
        labels = f.get("labels") or []
        assignee = (f.get("assignee") or {}).get("displayName")
        updated = parse_ts(f["updated"])
        parent = (f.get("parent") or {}).get("key")

        roll = None
        for lb in labels:
            if lb.startswith("rollup:"):
                scope = assignee if "rollup-mine" in labels else None
                roll = rollup_activity(lb.split(":", 1)[1].upper(), scope)
                break

        effective = updated
        if roll and roll["newest"] and roll["newest"] > effective:
            effective = roll["newest"]

        due = f.get("duedate")
        due_d = dt.date.fromisoformat(due) if due else None

        # Clamped: a back-dated run (--date in the past) would otherwise report
        # a negative age for tickets touched after that date.
        idle = max(0, (today_dt - effective).days) if effective else 0
        threshold = STALE_DAYS.get(status)
        # A future due date means the ticket is simply scheduled, not stale.
        if due_d and due_d >= today:
            threshold = None
        if "parked" in labels:
            threshold = None
        tier = stale_tier(idle, threshold) if cat != "done" else None

        rows.append({
            "key": i["key"], "summary": f["summary"], "status": status,
            "cat": cat, "assignee": assignee, "labels": labels,
            "epic": epics.get(parent, parent or "—"), "epic_key": parent,
            "updated": updated, "effective": effective, "idle": idle,
            "due": due_d, "rollup": roll, "tier": tier,
            "customer": next((customer_name(l.split(":", 1)[1])
                              for l in labels if l.startswith("customer:")),
                             None),
        })

    # ---- activity in the window -------------------------------------------
    # The window is half-open: [start, today). Without the upper bound, tickets
    # touched this morning would be listed under yesterday's heading.
    # Rollup tickets first: a placeholder holds no narrative of its own, so the
    # detail has to be pulled from the project it points at - including that
    # project's comments, which is where the actual story lives.
    for r in rows:
        if not r["rollup"]:
            continue
        proj = r["rollup"]["project"]
        jql = (f'project = {proj} AND updated >= "{win_start:%Y-%m-%d}"'
               f' AND updated < "{today:%Y-%m-%d}"')
        if r["rollup"].get("scope"):
            jql += f' AND assignee = "{r["rollup"]["scope"]}"'
        child = jira.search(
            jql, ["summary", "status", "updated", "assignee"])
        recent = []
        for c in child:
            recent.append({
                "key": c["key"],
                "summary": c["fields"]["summary"],
                "status": c["fields"]["status"]["name"],
                "assignee": (c["fields"].get("assignee") or {}).get(
                    "displayName"),
                "comments": [x for x in jira.comments(c["key"],
                                                      since=win_start_dt)
                             if x["created"] < today_dt],
            })
        r["rollup"]["recent"] = recent

    activity = []
    for r in rows:
        touched = bool(r["updated"] and win_start_dt <= r["updated"] < today_dt)
        cmts = []
        if touched or (r["updated"] and r["updated"] >= win_start_dt):
            cmts = [c for c in jira.comments(r["key"], since=win_start_dt)
                    if c["created"] < today_dt]
        # A rollup counts as active when its child project moved, even if the
        # placeholder itself was never touched.
        child_moved = bool(r["rollup"] and r["rollup"].get("recent"))
        if touched or cmts or child_moved:
            activity.append({**r, "comments": cmts})

    open_rows = [r for r in rows if r["cat"] != "done"]

    # ---- per-person plates -------------------------------------------------
    # The report is one document for the whole team, so everyone can see what
    # the others are carrying and where someone is falling behind. That only
    # works if each person's work is presented as a block rather than scattered
    # through status-oriented sections.
    week_end = today + dt.timedelta(days=7)
    plates = []
    for name, _email in TEAM:
        mine = [r for r in open_rows if r["assignee"] == name]

        # Display buckets must be DISJOINT - a ticket that is both "due this
        # week" and "To Do" would otherwise be listed twice on the same plate.
        # Precedence: falling behind, then scheduled, then by status.
        placed = set()

        def take(pred, sort=None):
            got = [r for r in mine
                   if r["key"] not in placed and pred(r)]
            for r in got:
                placed.add(r["key"])
            return sorted(got, key=sort) if sort else got

        behind = take(lambda r: bool(r["tier"]), sort=lambda r: -r["idle"])
        scheduled = take(lambda r: r["due"] and today <= r["due"] <= week_end,
                         sort=lambda r: r["due"])
        in_prog = take(lambda r: r["status"] == "In Progress")
        waiting = take(lambda r: r["status"] == "Waiting")
        todo = take(lambda r: r["status"] == "To Do")

        plates.append({
            "name": name,
            # Disjoint display buckets.
            "behind": behind, "scheduled": scheduled,
            "in_progress": in_prog, "waiting": waiting, "todo": todo,
            # True status counts for the heading chips - these describe health
            # and status on different axes, so overlap is correct here.
            "n_in_progress": sum(1 for r in mine
                                 if r["status"] == "In Progress"),
            "n_waiting": sum(1 for r in mine if r["status"] == "Waiting"),
            "closed": [r for r in rows if r["assignee"] == name
                       and r["cat"] == "done" and r["updated"]
                       and win_start_dt <= r["updated"] < today_dt],
            "total": len(mine),
        })

    return {
        "plates": plates,
        "today": today, "mode": mode, "window_label": window_label,
        "win_start": win_start,
        "rows": rows,
        "open": open_rows,
        "in_progress": [r for r in open_rows if r["status"] == "In Progress"],
        "waiting": [r for r in open_rows if r["status"] == "Waiting"],
        "today_events": sorted([r for r in open_rows if r["due"] == today],
                               key=lambda r: r["key"]),
        "upcoming": sorted([r for r in open_rows if r["due"]
                            and today < r["due"] <= today + dt.timedelta(days=7)],
                           key=lambda r: r["due"]),
        "stale": sorted([r for r in open_rows if r["tier"]],
                        key=lambda r: -r["idle"]),
        "unassigned": [r for r in open_rows if not r["assignee"]],
        "activity": sorted(activity, key=lambda r: r["effective"] or
                           dt.datetime.min.replace(tzinfo=dt.timezone.utc),
                           reverse=True),
        "closed_in_window": [
            r for r in rows if r["cat"] == "done" and r["updated"]
            and win_start_dt <= r["updated"] < today_dt],
    }


# --------------------------------------------------------------------------
# Rich animated HTML
# --------------------------------------------------------------------------

# Theme tokens. Every colour the rich report uses is declared here and nowhere
# else - a literal left inline is a literal that cannot follow the theme.
#
# Dark is a *selected* palette, not an inverted light one. The status hexes are
# mode-invariant and all clear 3:1 on the dark surface, so dark mode uses them
# directly as foreground; light mode uses darkened variants because warning and
# serious sit under 3:1 on a light surface (mitigated anyway by icon + label).
#
# Pill contrasts were computed, not eyeballed. Every pair clears 4.5:1 except the
# dark `critical` pill at 3.84 - reaching 4.5 there would mean altering the fixed
# status hex, so it relies on the sanctioned mitigation instead: a status colour
# never carries meaning alone, and this one always ships with an icon and the
# word "Critical".
VARS_LIGHT = """
 --bg:#f4f4f2; --surface:#fcfcfb; --border:#e5e4e0; --hover:#f7f7f5;
 --ink:#0b0b0b; --ink2:#52514e; --rule:#efeeeb; --chip:#ececea;
 --link:#2a78d6; --cmt-bg:#f8f8f6; --cmt-ink:#3a3a37;
 --help-bg:#fdf4e6; --help-ink:#5c4405;
 --p-good-bg:#e7f6e7;    --p-good-fg:#0a7a0a;
 --p-warn-bg:#fef3d9;    --p-warn-fg:#8a6000;
 --p-serious-bg:#fdece4; --p-serious-fg:#a8471f;
 --p-crit-bg:#fae5e5;    --p-crit-fg:#a52222;
 --p-neutral-bg:#ececea; --p-neutral-fg:#52514e;
 --p-blue-bg:#e4eefb;    --p-blue-fg:#1d5aa3;
"""

VARS_DARK = """
 --bg:#121211; --surface:#1f1f1e; --border:#35342f; --hover:#292928;
 --ink:#ffffff; --ink2:#c3c2b7; --rule:#2f2e2b; --chip:#2e2e2c;
 --link:#3987e5; --cmt-bg:#232322; --cmt-ink:#cfcec6;
 --help-bg:#2d2413; --help-ink:#f2d79a;
 --p-good-bg:#0a2109;    --p-good-fg:#0ca30c;
 --p-warn-bg:#33280c;    --p-warn-fg:#fab219;
 --p-serious-bg:#33200f; --p-serious-fg:#ec835a;
 --p-crit-bg:#240d0d;    --p-crit-fg:#d03b3b;
 --p-neutral-bg:#2e2e2c; --p-neutral-fg:#c3c2b7;
 --p-blue-bg:#132339;    --p-blue-fg:#6ea8ec;
"""

# System preference is the default; an explicit choice overrides it and persists.
THEME_CSS = (
    ":root{" + VARS_LIGHT + "}\n"
    "@media (prefers-color-scheme:dark){:root:not([data-theme=\"light\"])"
    "{" + VARS_DARK + "}}\n"
    ":root[data-theme=\"dark\"]{" + VARS_DARK + "}\n"
    ":root[data-theme=\"light\"]{" + VARS_LIGHT + "}\n"
)

CSS = THEME_CSS + """
*{box-sizing:border-box}
body{margin:0;background:var(--bg);color:var(--ink);
 font:15px/1.55 -apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,Helvetica,Arial,sans-serif;
 -webkit-font-smoothing:antialiased}
.wrap{max-width:940px;margin:0 auto;padding:28px 20px 60px}
.hero{background:linear-gradient(135deg,#1f2a44 0%%,#33406b 55%%,#4a3aa7 100%%);
 color:#fff;border-radius:16px;padding:26px 28px;box-shadow:0 12px 30px rgba(31,42,68,.22);
 position:relative}
.hero h1{margin:0 0 4px;font-size:22px;letter-spacing:-.2px}
.hero .sub{opacity:.82;font-size:13.5px}
.badge{display:inline-block;background:rgba(255,255,255,.16);border:1px solid rgba(255,255,255,.28);
 border-radius:999px;padding:3px 11px;font-size:11.5px;font-weight:600;letter-spacing:.4px;
 text-transform:uppercase;margin-bottom:10px}
.tt{position:absolute;top:22px;right:22px;background:rgba(255,255,255,.14);
 border:1px solid rgba(255,255,255,.3);color:#fff;border-radius:999px;
 padding:5px 13px;font-size:12px;font-weight:600;cursor:pointer;font-family:inherit;
 transition:background .18s}
.tt:hover{background:rgba(255,255,255,.26)}
.kpis{display:grid;grid-template-columns:repeat(auto-fit,minmax(150px,1fr));gap:12px;margin:18px 0 6px}
.kpi{background:var(--surface);border:1px solid var(--border);border-radius:12px;padding:14px 16px}
.kpi .n{font-size:27px;font-weight:700;letter-spacing:-.5px;line-height:1.1}
.kpi .l{font-size:12px;color:var(--ink2);margin-top:3px}
.card{background:var(--surface);border:1px solid var(--border);border-radius:12px;margin-top:14px;
 overflow:hidden}
.card>summary{list-style:none;cursor:pointer;padding:15px 18px;display:flex;align-items:center;
 gap:10px;font-weight:650;font-size:15px;user-select:none;transition:background .18s}
.card>summary::-webkit-details-marker{display:none}
.card>summary:hover{background:var(--hover)}
.chev{margin-left:auto;transition:transform .28s cubic-bezier(.2,.7,.3,1);opacity:.5;font-size:13px}
.card[open] .chev{transform:rotate(90deg)}
.count{background:var(--chip);border-radius:999px;padding:2px 9px;font-size:12px;
 font-weight:600;color:var(--ink2)}
.body{padding:0 18px 6px;animation:unfold .32s cubic-bezier(.2,.7,.3,1)}
@keyframes unfold{from{opacity:0;transform:translateY(-6px)}to{opacity:1;transform:none}}
@keyframes rise{from{opacity:0;transform:translateY(12px)}to{opacity:1;transform:none}}
.t{width:100%%;border-collapse:collapse;font-size:13.5px}
.t td{padding:9px 6px;border-top:1px solid var(--rule);vertical-align:top}
.t tr:first-child td{border-top:none}
.k{font-family:ui-monospace,SFMono-Regular,Menlo,monospace;font-size:12.5px;white-space:nowrap}
.k a{color:var(--link);text-decoration:none;font-weight:600}
.k a:hover{text-decoration:underline}
.pill{display:inline-block;border-radius:999px;padding:2px 9px;font-size:11.5px;font-weight:600;
 white-space:nowrap;border:1px solid transparent}
.pill-good{background:var(--p-good-bg);color:var(--p-good-fg)}
.pill-warning{background:var(--p-warn-bg);color:var(--p-warn-fg)}
.pill-serious{background:var(--p-serious-bg);color:var(--p-serious-fg)}
.pill-critical{background:var(--p-crit-bg);color:var(--p-crit-fg)}
.pill-neutral{background:var(--p-neutral-bg);color:var(--p-neutral-fg)}
.pill-blue{background:var(--p-blue-bg);color:var(--p-blue-fg)}
.who{color:var(--ink2);font-size:12.5px;white-space:nowrap}
.meta{color:var(--ink2);font-size:12px}
.sect-h{font-size:12px;text-transform:uppercase;letter-spacing:.7px;color:var(--ink2);
 font-weight:700;margin:16px 0 2px}
.empty{color:var(--ink2);font-size:13.5px;padding:4px 0 14px;font-style:italic}
.help{background:var(--help-bg);border:1px solid %(warning)s55;border-left:4px solid %(warning)s;
 border-radius:0 8px 8px 0;padding:9px 13px;margin:10px 0 2px;font-size:13.5px;
 color:var(--help-ink)}
.cmt{background:var(--cmt-bg);border-left:3px solid var(--link);border-radius:0 7px 7px 0;
 padding:7px 11px;margin:6px 0;font-size:13px;color:var(--cmt-ink)}
.cmt b{color:var(--ink)}
.cmt a{color:var(--link)}
.foot{margin-top:26px;font-size:12px;color:var(--ink2);text-align:center;line-height:1.7}

/* Entrance animation is purely additive: every element above is fully visible
   without it. An earlier version started at opacity:0 and relied on the
   animation to reveal content, which rendered a near-blank page anywhere the
   animation did not execute (print, PDF export, screenshot, reduced motion).
   `backwards` fill keeps the stagger while leaving the resting state visible. */
@media (prefers-reduced-motion:no-preference){
 .hero{animation:rise .5s cubic-bezier(.2,.7,.3,1) backwards}
 .kpi{animation:rise .5s cubic-bezier(.2,.7,.3,1) backwards;
  animation-delay:calc(var(--i)*60ms)}
 .card{animation:rise .5s cubic-bezier(.2,.7,.3,1) backwards;
  animation-delay:calc(var(--i)*60ms)}
}
""" % TOK


def pill(token, label, icon=""):
    """Pills are class-based, not inline-styled, so they follow the theme.

    An inline `style` attribute cannot be overridden by a `[data-theme]` block,
    so any colour set inline is a colour frozen in light mode.
    """
    if token not in ("good", "warning", "serious", "critical", "blue"):
        token = "neutral"
    ic = f"{icon} " if icon else ""
    return f'<span class="pill pill-{token}">{ic}{e(label)}</span>'


def status_pill(r):
    tok = {"In Progress": "blue", "Waiting": "warning",
           "To Do": "neutral", "Done": "good", "Cancelled": "neutral"}
    return pill(tok.get(r["status"], "neutral"), r["status"])


def issue_link(key, base):
    return f'<a href="{base}/browse/{key}" {LINK_ATTRS}>{key}</a>'


def row_html(r, base, show_age=True, show_due=False):
    bits = []
    if r["customer"]:
        bits.append(pill("blue", r["customer"]))
    if show_due and r["due"]:
        bits.append(f'<span class="meta">due {r["due"]:%a %-d %b}</span>')
    if show_age:
        bits.append(f'<span class="meta">{r["idle"]}d idle</span>')
    if r["tier"]:
        tok, icon, label, _ = r["tier"]
        bits.append(pill(tok, label, icon))
    if r["rollup"]:
        ru = r["rollup"]
        bits.append(pill("neutral",
                         f'{ru["project"]}: {ru["open"]} open / {ru["done"]} done'))
    return (
        f'<tr><td class="k">{issue_link(r["key"], base)}</td>'
        f'<td>{e(r["summary"])}'
        f'<div class="meta" style="margin-top:3px">{e(r["epic"])}</div></td>'
        f'<td>{status_pill(r)}</td>'
        f'<td class="who">{e(r["assignee"] or "— unassigned")}</td>'
        f'<td style="text-align:right">{" ".join(bits)}</td></tr>')


def rollup_line(roll, base=None, links=True):
    """One-or-two-line digest of a rollup's movement.

    Deliberately terse: the child project's comment bodies are its own
    narrative, and reproducing them here buried the report in detail. The
    counts plus the ticket keys are enough to see what moved and click through.
    """
    rec = roll.get("recent") or []
    by = defaultdict(list)
    for c in rec:
        by[c["status"]].append(c["key"])

    # Read in a stable, meaningful order rather than dict order.
    order = ["Done", "In Progress", "Waiting", "To Do", "Cancelled"]
    ordered = ([s for s in order if s in by]
               + sorted(s for s in by if s not in order))
    phrases = [f'{len(by[s])} {s.lower()}' for s in ordered]

    head = (f'<b>{len(rec)} {plural(len(rec), "ticket")} moved in '
            f'{e(roll["project"])}</b>')
    if phrases:
        head += f' — {e(", ".join(phrases))}'

    keys = [c["key"] for c in rec][:12]
    more = f' + {len(rec) - len(keys)} more' if len(rec) > len(keys) else ""

    if not (links and base):
        # Plain form for the markdown archive - no tags to strip, and an
        # explicit separator so the counts do not run into the key list.
        plain = head.replace("<b>", "").replace("</b>", "")
        return f'{plain} · {", ".join(keys)}{more}'

    shown = ", ".join(f'<a href="{base}/browse/{k}" {LINK_ATTRS}>{k}</a>'
                      for k in keys)
    return f'{head}<div style="margin-top:4px">{shown}{more}</div>'


def table(rows, base, empty, **kw):
    if not rows:
        return f'<div class="empty">{e(empty)}</div>'
    body = "".join(row_html(r, base, **kw) for r in rows)
    return f'<table class="t">{body}</table>'


def card(idx, title, count, inner, open_=False):
    o = " open" if open_ else ""
    cnt = f'<span class="count">{count}</span>' if count is not None else ""
    return (f'<details class="card" style="--i:{idx}"{o}>'
            f'<summary>{e(title)}{cnt}<span class="chev">▶</span></summary>'
            f'<div class="body">{inner}</div></details>')


def render_rich(m, base):
    t = m["today"]
    weekly = m["mode"] == "weekly"
    title = "Weekly Recap" if weekly else "Daily Report"
    named = {n for n, _ in TEAM}

    kpis = [
        (len(m["in_progress"]), "In progress"),
        (len(m["waiting"]), "Waiting on others"),
        (len(m["today_events"]), "Scheduled today"),
        (len(m["stale"]), "Need attention"),
        (len(m["closed_in_window"]), "Completed " +
         ("last week" if weekly else "yesterday")),
    ]
    kpi_html = "".join(
        f'<div class="kpi" style="--i:{n}"><div class="n">{v}</div>'
        f'<div class="l">{e(l)}</div></div>'
        for n, (v, l) in enumerate(kpis))

    parts = []
    i = 0

    # Today
    ev = m["today_events"]
    # Today only. The week ahead lives on each person's plate, where it also
    # says whose it is - a team-wide "coming up" block just repeated the same
    # tickets further down the page.
    inner = table(ev, base, "Nothing scheduled for today.",
                  show_age=False, show_due=True)
    if not ev and m["upcoming"]:
        nxt = m["upcoming"][0]
        inner += (f'<div class="meta" style="padding:0 0 12px">Next up: '
                  f'{issue_link(nxt["key"], base)} on '
                  f'{nxt["due"]:%A %-d %B} — {e(nxt["assignee"] or "unassigned")}'
                  f'.</div>')
    parts.append(card(i, f"Today — {t:%A %-d %B}", len(ev), inner,
                      open_=True)); i += 1

    # One plate per person, so the whole team can see who is carrying what
    # and where someone could use a hand.
    for pl in m["plates"]:
        inner = ""
        if pl["behind"]:
            n = len(pl["behind"])
            inner += (
                f'<div class="help">'
                f'<b>{e(pl["name"].split()[0])} has {n} '
                f'{plural(n, "item")} falling behind.</b> '
                f'Worth asking how the team can help.</div>')
            inner += table(pl["behind"], base, "")
        if pl["scheduled"]:
            inner += '<div class="sect-h">Scheduled this week</div>'
            inner += table(pl["scheduled"], base, "", show_age=False,
                           show_due=True)
        inner += '<div class="sect-h">In progress</div>'
        inner += table(pl["in_progress"], base, "Nothing in progress.")
        if pl["waiting"]:
            inner += '<div class="sect-h">Waiting on others</div>'
            inner += table(pl["waiting"], base, "")
        if pl["todo"]:
            inner += '<div class="sect-h">Queued</div>'
            inner += table(pl["todo"], base, "")
        if pl["closed"]:
            inner += ('<div class="sect-h">Completed '
                      + ("last week" if weekly else "yesterday") + '</div>')
            inner += table(pl["closed"], base, "", show_age=False)

        chips = [f'{pl["n_in_progress"]} in progress']
        if pl["n_waiting"]:
            chips.append(f'{pl["n_waiting"]} waiting')
        if pl["behind"]:
            chips.append(f'{len(pl["behind"])} behind')
        # Deliberately not named `title` - that holds the report's own heading.
        plate_title = f'{pl["name"]}  —  {" · ".join(chips)}'
        parts.append(card(i, plate_title, pl["total"], inner, open_=True))
        i += 1

    # Activity
    inner = ""
    if not m["activity"]:
        inner = (f'<div class="empty">No ticket activity recorded for '
                 f'{e(m["window_label"])}.</div>')
    for r in m["activity"]:
        inner += f'<table class="t">{row_html(r, base, show_age=False)}</table>'
        for c in r.get("comments", []):
            ct = clean_comment(c["text"])
            txt = ct[:400] + ("…" if len(ct) > 400 else "")
            inner += (f'<div class="cmt"><b>{e(c["author"])}</b> '
                      f'· {c["created"]:%-d %b %H:%M}<br>{e(txt)}</div>')
        if r["rollup"] and r["rollup"].get("recent"):
            inner += (f'<div class="cmt">{rollup_line(r["rollup"], base)}'
                      f'</div>')
    parts.append(card(i, f"Activity — {m['window_label']}",
                      len(m["activity"]), inner, open_=True)); i += 1

    # Completions now sit on each person's plate, so a team-wide list would
    # only repeat them. Anything closed by someone outside the team still needs
    # a home.
    outside_closed = [r for r in m["closed_in_window"]
                      if r["assignee"] not in named]
    if outside_closed:
        parts.append(card(i, "Completed elsewhere", len(outside_closed),
                          table(outside_closed, base, "", show_age=False)))
        i += 1

    # Anything stale that is NOT on a team member's plate - other people's
    # tickets and unowned work. Each person's own stale items already appear
    # at the top of their plate, so repeating them here would be noise.
    other_stale = [r for r in m["stale"] if r["assignee"] not in named]
    if other_stale:
        shown, rest = other_stale[:5], len(other_stale) - 5
        inner = table(shown, base, "")
        if rest > 0:
            inner += (f'<div class="meta" style="padding:8px 0 12px">'
                      f'+ {rest} further {plural(rest, "ticket")} over '
                      f'threshold, not shown.</div>')
        parts.append(card(i, "Falling behind — elsewhere",
                          len(other_stale), inner, open_=True)); i += 1

    if m["unassigned"]:
        parts.append(card(i, "No owner", len(m["unassigned"]),
                          table(m["unassigned"], base, ""))); i += 1

    # Applied before first paint so a saved dark choice does not flash light.
    # Wrapped in try/catch because localStorage throws on a file:// URL in some
    # browsers, and the report is opened from disk as an email attachment.
    theme_init = (
        "<script>try{var t=localStorage.getItem('psact-theme');"
        "if(t)document.documentElement.setAttribute('data-theme',t)}catch(e){}"
        "</script>")

    theme_js = (
        "<script>(function(){"
        "var r=document.documentElement,b=document.getElementById('tt');"
        "function dark(){var a=r.getAttribute('data-theme');"
        "return a?a==='dark':window.matchMedia&&"
        "window.matchMedia('(prefers-color-scheme:dark)').matches}"
        "function label(){if(b)b.textContent=dark()?'\\u2600 Light':'\\u263e Dark'}"
        "if(b)b.addEventListener('click',function(){"
        "var next=dark()?'light':'dark';"
        "r.setAttribute('data-theme',next);"
        "try{localStorage.setItem('psact-theme',next)}catch(e){}"
        "label()});"
        "label();"
        "if(window.matchMedia){var m=window.matchMedia('(prefers-color-scheme:dark)');"
        "if(m.addEventListener)m.addEventListener('change',label)}"
        "})();</script>")

    return f"""<!doctype html><html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<meta name="color-scheme" content="light dark">
<title>PSACT {title} — {t:%d %b %Y}</title><style>{CSS}</style>
{theme_init}</head><body>
<div class="wrap">
  <div class="hero">
    <button class="tt" id="tt" type="button" aria-label="Toggle light or dark theme">&#9790; Dark</button>
    <div class="badge">{e(title)}</div>
    <h1>PreSales Activity — {t:%A %-d %B %Y}</h1>
    <div class="sub">Covering {e(m['window_label'])} &middot;
      {e(" · ".join(n for n, _ in TEAM))}</div>
  </div>
  <div class="kpis">{kpi_html}</div>
  {''.join(parts)}
  <div class="foot">Generated from Jira project PSACT
    (plus rollup projects) &middot; {t:%Y-%m-%d}<br>
    Staleness is measured on last update, not on comments —
    In&nbsp;Progress {STALE_DAYS['In Progress']}d &middot;
    Waiting {STALE_DAYS['Waiting']}d &middot;
    To&nbsp;Do {STALE_DAYS['To Do']}d.
    Tickets with a future due date, or labelled <code>parked</code>, are exempt.
  </div>
</div>
{theme_js}</body></html>"""


# --------------------------------------------------------------------------
# Email-safe HTML (no JS, no <details> - table layout only)
# --------------------------------------------------------------------------

def render_email(m, base, attach_name):
    t = m["today"]
    weekly = m["mode"] == "weekly"
    title = "Weekly Recap" if weekly else "Daily Report"

    def mini(rows, empty, show_due=False):
        if not rows:
            return (f'<p style="margin:4px 0 14px;color:{TOK["ink2"]};'
                    f'font-style:italic;font-size:14px">{e(empty)}</p>')
        out = ('<table cellpadding="0" cellspacing="0" border="0" '
               'width="100%" style="margin:4px 0 16px;font-size:14px">')
        for r in rows:
            extra = []
            if show_due and r["due"]:
                extra.append(f'due {r["due"]:%a %-d %b}')
            if r["customer"]:
                extra.append(r["customer"])
            if r["tier"]:
                extra.append(f'{r["tier"][2]} · {r["idle"]}d idle')
            sub = " · ".join(extra)
            out += (
                f'<tr><td style="padding:6px 0;border-bottom:1px solid #eee;'
                f'vertical-align:top">'
                f'<a href="{base}/browse/{r["key"]}" {LINK_ATTRS} '
                f'style="color:{TOK["blue"]};text-decoration:none;'
                f'font-weight:600;font-family:monospace">{r["key"]}</a>'
                f'&nbsp; {e(r["summary"])}<br>'
                f'<span style="color:{TOK["ink2"]};font-size:12.5px">'
                f'{e(r["status"])} · {e(r["assignee"] or "unassigned")}'
                + (f' · {e(sub)}' if sub else "") +
                '</span></td></tr>')
        return out + "</table>"

    def h(txt):
        return (f'<h2 style="font-size:13px;text-transform:uppercase;'
                f'letter-spacing:.6px;color:{TOK["ink2"]};margin:22px 0 2px;'
                f'border-bottom:2px solid #ececea;padding-bottom:5px">'
                f'{e(txt)}</h2>')

    kpi_cells = "".join(
        f'<td align="center" style="padding:10px 6px;background:#fff;'
        f'border:1px solid #e5e4e0;border-radius:10px">'
        f'<div style="font-size:23px;font-weight:700">{v}</div>'
        f'<div style="font-size:11.5px;color:{TOK["ink2"]}">{e(l)}</div></td>'
        f'<td width="8"></td>'
        for v, l in [
            (len(m["in_progress"]), "In progress"),
            (len(m["waiting"]), "Waiting"),
            (len(m["today_events"]), "Today"),
            (len(m["stale"]), "Attention"),
        ])

    # Today only - the week ahead sits on each person's plate instead, so a
    # team-wide "coming up" block would list the same tickets twice.
    next_up = ""
    if not m["today_events"] and m["upcoming"]:
        nxt = m["upcoming"][0]
        next_up = (f'<p style="margin:-8px 0 14px;font-size:13px;'
                   f'color:{TOK["ink2"]}">Next up: '
                   f'<a href="{base}/browse/{nxt["key"]}" {LINK_ATTRS} '
                   f'style="color:{TOK["blue"]};text-decoration:none;'
                   f'font-weight:600">{nxt["key"]}</a> on '
                   f'{nxt["due"]:%A %-d %B} — '
                   f'{e(nxt["assignee"] or "unassigned")}.</p>')

    # One shared document: a block per person so each of them can see the
    # others' plate and where to offer help.
    named = {n for n, _ in TEAM}
    plates_html = ""
    for pl in m["plates"]:
        chips = [f'{pl["n_in_progress"]} in progress']
        if pl["n_waiting"]:
            chips.append(f'{pl["n_waiting"]} waiting')
        if pl["behind"]:
            chips.append(f'{len(pl["behind"])} behind')
        plates_html += (
            f'<h2 style="font-size:15px;margin:24px 0 2px;padding-bottom:5px;'
            f'border-bottom:2px solid #1f2a44">{e(pl["name"])}'
            f'<span style="font-weight:400;font-size:12.5px;'
            f'color:{TOK["ink2"]}"> — {e(" · ".join(chips))}</span></h2>')
        if pl["behind"]:
            plates_html += (
                f'<div style="background:#fdf4e6;border-left:4px solid '
                f'{TOK["warning"]};padding:9px 12px;margin:10px 0 0;'
                f'font-size:13.5px;color:#5c4405">'
                f'<b>{e(pl["name"].split()[0])} has {len(pl["behind"])} '
                f'{plural(len(pl["behind"]), "item")} falling behind.</b> '
                f'Worth asking how the team can help.</div>')
            plates_html += mini(pl["behind"], "")
        if pl["scheduled"]:
            plates_html += mini(pl["scheduled"], "", show_due=True)
        # Buckets are already disjoint upstream, so concatenating them cannot
        # list the same ticket twice.
        rest_items = pl["in_progress"] + pl["waiting"] + pl["todo"]
        plates_html += mini(rest_items, "Nothing else on the plate.")
        if pl["closed"]:
            plates_html += (
                f'<p style="margin:-6px 0 12px;font-size:12.5px;'
                f'color:{TOK["ink2"]}">Completed: '
                + e(", ".join(r["key"] for r in pl["closed"])) + '</p>')

    other_stale = [r for r in m["stale"] if r["assignee"] not in named]
    stale_shown = other_stale[:5]
    rest = len(other_stale) - 5

    act = ""
    if not m["activity"]:
        act = (f'<p style="color:{TOK["ink2"]};font-style:italic;'
               f'font-size:14px">No ticket activity recorded.</p>')
    for r in m["activity"]:
        act += mini([r], "")
        for c in r.get("comments", [])[:3]:
            ct = clean_comment(c["text"])
            txt = ct[:280] + ("…" if len(ct) > 280 else "")
            act += (f'<div style="background:#f8f8f6;border-left:3px solid '
                    f'{TOK["blue"]};padding:7px 11px;margin:-10px 0 14px;'
                    f'font-size:13px;color:#3a3a37"><b>{e(c["author"])}</b> '
                    f'· {c["created"]:%-d %b}<br>{e(txt)}</div>')
        # Rollup tickets carry no narrative themselves - without this the
        # email would show the placeholder and none of the actual work.
        rec = (r["rollup"] or {}).get("recent") or []
        if rec:
            act += (f'<div style="background:#f8f8f6;border-left:3px solid '
                    f'{TOK["violet"]};padding:9px 11px;margin:-10px 0 14px;'
                    f'font-size:12.5px;color:#3a3a37">'
                    f'{rollup_line(r["rollup"], base)}</div>')

    return f"""<html><body style="margin:0;padding:0;background:#f4f4f2">
<table cellpadding="0" cellspacing="0" border="0" width="100%"
       style="background:#f4f4f2;padding:22px 12px">
<tr><td align="center">
<table cellpadding="0" cellspacing="0" border="0" width="660"
  style="max-width:660px;background:#fcfcfb;border-radius:14px;overflow:hidden;
  font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,Helvetica,Arial,sans-serif;
  color:{TOK['ink']}">
 <tr><td style="background:#1f2a44;padding:22px 26px;color:#fff">
   <div style="font-size:11.5px;letter-spacing:.5px;text-transform:uppercase;
     opacity:.8;font-weight:600">{e(title)}</div>
   <div style="font-size:20px;font-weight:700;margin-top:3px">
     PreSales Activity — {t:%A %-d %B %Y}</div>
   <div style="font-size:13px;opacity:.8;margin-top:3px">
     Covering {e(m['window_label'])}</div>
 </td></tr>
 <tr><td style="padding:18px 26px 26px">
   <table cellpadding="0" cellspacing="0" border="0" width="100%">
     <tr>{kpi_cells}</tr></table>

   {h(f'Today — {t:%A %-d %B}')}
   {mini(m['today_events'], 'Nothing scheduled for today.', show_due=True)}
   {next_up}

   {plates_html}

   {h('Activity — ' + m['window_label'])}
   {act}

   {(h('Falling behind — elsewhere') + mini(stale_shown, '')
     + ((f'<p style="font-size:12.5px;color:{TOK["ink2"]}">+ {rest} further '
         f'{plural(rest, "ticket")} over threshold.</p>') if rest > 0 else ''))
     if other_stale else ''}

   {(h('No owner') + mini(m['unassigned'], '')) if m['unassigned'] else ''}

   <p style="margin-top:26px;padding-top:14px;border-top:1px solid #ececea;
     font-size:12px;color:{TOK['ink2']};line-height:1.6">
     The attached <b>{e(attach_name)}</b> is the interactive version — open it
     in a browser for collapsible sections. Click any ticket key to go
     straight to Jira.<br>
     Staleness is measured on last update, not on comments. Tickets with a
     future due date, or labelled <code>parked</code>, are exempt.
   </p>
 </td></tr>
</table></td></tr></table></body></html>"""


# --------------------------------------------------------------------------
# Markdown archive
# --------------------------------------------------------------------------

def render_markdown(m):
    t, weekly = m["today"], m["mode"] == "weekly"
    L = [f"## {t:%Y-%m-%d} ({t:%A}) — "
         f"{'Weekly recap' if weekly else 'Daily'}",
         "", f"*Covering {m['window_label']}*", ""]

    L.append(f"**Pulse:** {len(m['in_progress'])} in progress · "
             f"{len(m['waiting'])} waiting · "
             f"{len(m['today_events'])} scheduled today · "
             f"{len(m['stale'])} needing attention · "
             f"{len(m['closed_in_window'])} completed")
    L.append("")

    if m["today_events"]:
        L.append("### Scheduled today")
        for r in m["today_events"]:
            cust = f" [{r['customer']}]" if r["customer"] else ""
            L.append(f"- **{r['key']}**{cust} {r['summary']} "
                     f"— {r['assignee'] or 'unassigned'}")
        L.append("")

    # Per person, so the monthly roll-up can answer "what was X working on"
    # without re-querying Jira.
    L.append("### By person")
    for pl in m["plates"]:
        chips = [f'{len(pl["in_progress"])} in progress']
        if pl["waiting"]:
            chips.append(f'{len(pl["waiting"])} waiting')
        if pl["behind"]:
            chips.append(f'{len(pl["behind"])} falling behind')
        L.append(f"#### {pl['name']} — {' · '.join(chips)}")
        if pl["behind"]:
            L.append(f"- _Falling behind ({len(pl['behind'])}):_")
            for r in pl["behind"]:
                L.append(f"    - **{r['key']}** {r['summary']} "
                         f"({r['status']}, idle {r['idle']}d, {r['tier'][2]})")
        for label, items in (("Scheduled this week", pl["scheduled"]),
                             ("In progress", pl["in_progress"]),
                             ("Waiting", pl["waiting"]),
                             ("Queued", pl["todo"]),
                             ("Completed", pl["closed"])):
            if not items:
                continue
            L.append(f"- _{label}:_")
            for r in items:
                cust = f" [{r['customer']}]" if r["customer"] else ""
                due = f", due {r['due']:%d %b}" if r["due"] else ""
                L.append(f"    - **{r['key']}**{cust} {r['summary']}"
                         f" ({r['status']}{due}) · _{r['epic']}_")
        if not pl["total"] and not pl["closed"]:
            L.append("- _Nothing open._")
        L.append("")

    if m["activity"]:
        L.append("### Activity")
        for r in m["activity"]:
            L.append(f"- **{r['key']}** {r['summary']} "
                     f"— {r['assignee'] or 'unassigned'} ({r['status']})")
            for c in r.get("comments", []):
                ct = clean_comment(c["text"])
                txt = ct[:300] + ("…" if len(ct) > 300 else "")
                L.append(f"    - _{c['author']}, {c['created']:%d %b}_: {txt}")
            if r["rollup"] and r["rollup"].get("recent"):
                L.append("    - " + rollup_line(r["rollup"], links=False))
        L.append("")

    named = {n for n, _ in TEAM}
    other_stale = [r for r in m["stale"] if r["assignee"] not in named]
    if other_stale:
        L.append("### Falling behind — elsewhere")
        for r in other_stale[:5]:
            L.append(f"- **{r['key']}** {r['summary']} — "
                     f"{r['assignee'] or 'unassigned'}, {r['status']}, "
                     f"idle {r['idle']}d ({r['tier'][2]})")
        if len(other_stale) > 5:
            L.append(f"- _+{len(other_stale) - 5} more over threshold_")
        L.append("")

    L.append("---")
    L.append("")
    return "\n".join(L)


# --------------------------------------------------------------------------
# Main
# --------------------------------------------------------------------------

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--date")
    ap.add_argument("--mode", choices=["daily", "weekly"])
    ap.add_argument("--outdir", default="/task-output")
    ap.add_argument("--force", action="store_true",
                    help="run even at the weekend")
    a = ap.parse_args()

    today = dt.date.fromisoformat(a.date) if a.date else dt.date.today()
    if today.weekday() >= 5 and not a.force:
        print(f"{today} is a weekend - no report. Use --force to override.")
        return 0

    mode = a.mode or ("weekly" if today.weekday() == 0 else "daily")

    jira = Jira()
    base = os.environ["JIRA_BASE_URL"].rstrip("/")
    m = build_model(jira, today, mode)

    os.makedirs(a.outdir, exist_ok=True)
    stem = f"psact-report-{today:%Y-%m-%d}"
    rich_path = os.path.join(a.outdir, f"{stem}.html")
    mail_path = os.path.join(a.outdir, f"{stem}.email.html")
    md_path = os.path.join(a.outdir, f"{stem}.md")

    with open(rich_path, "w") as f:
        f.write(render_rich(m, base))
    with open(mail_path, "w") as f:
        f.write(render_email(m, base, f"{stem}.html"))
    with open(md_path, "w") as f:
        f.write(render_markdown(m))

    print(json.dumps({
        "date": str(today), "mode": mode, "window": m["window_label"],
        "rich": rich_path, "email": mail_path, "markdown": md_path,
        "counts": {
            "in_progress": len(m["in_progress"]), "waiting": len(m["waiting"]),
            "today": len(m["today_events"]), "stale": len(m["stale"]),
            "activity": len(m["activity"]),
            "completed": len(m["closed_in_window"]),
            "unassigned": len(m["unassigned"]),
        }}, indent=2))
    return 0


if __name__ == "__main__":
    sys.exit(main())
