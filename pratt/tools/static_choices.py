#!/usr/bin/env python3
"""Give the database-driven select boxes a static fallback list.

Why this is needed
------------------
Several select boxes are "database driven": the widget names a table and the
Profound UI runtime queries it **from the browser**, by POSTing to
/profoundui/auth/PUI0009103.PGM.  `pui.getProgramURL` builds that as an absolute
path from the site root, and the task proxy serves the app under
/tasks/<id>/app/proxy/..., so the call does not land where it should.  The result
is empty drop-downs - including Dest and Freight on the new-PO screen, which makes
a purchase order impossible to raise in the browser.

The data is fine; only the callback is.  Running the exact queries against the
task library returns the right rows.

What this does
--------------
Writes a static `choices` / `choice values` pair onto the affected select boxes,
generated from the live tables so it cannot drift from the seeded data.  This is
not a foreign idea in these screens - Pratt already ship static choices on OHCODE,
OITXTU1-8 and ASKRETURN.

The database-choice properties are REMOVED on exactly these widgets.  Leaving them
in does not give a graceful fallback: the runtime puts a single "Loading..." option
in the box while it waits for the query, and when the query never returns, that is
all the user ever sees.  Verified in the render harness.

The removed properties are printed when this runs, so restoring Pratt's original
behaviour is a matter of putting them back once the callback works.

Textboxes get the same treatment where the list is master data worth browsing -
vendor and vendor part.  There the properties drive autocomplete rather than a
drop-down; without them the field is still typable, but you cannot look a vendor
or a part up, which is most of the work of raising a purchase order.

Not covered on purpose:
  - OHCUID / OHLOC (customer and ship-to).  Their criteria depend on another field
    on the screen, so a flat list would offer invalid combinations.
  - SRDESC, which lists every description in ORITM/ORMSR - volatile and long.
  - CTLVAL, the saved-search picker.  Its contents are per-user and change at
    runtime, so a build-time snapshot would be wrong by design.

Re-run after changing the reference data, then rebuild the display files.
"""
import json, os, subprocess, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import rdf_edit

LIB = os.environ.get('IBMI_BUILD_LIBRARY')


def q(sql):
    """Run a query against the task library and return rows as a list of dicts."""
    payload = json.dumps({'connection': 'dev', 'sql': sql})
    out = subprocess.run(['aitool', 'sql', '--input', payload],
                         capture_output=True, text=True).stdout
    d = json.loads(out)
    if not d.get('success'):
        raise SystemExit('query failed: %s\n%s' % (d['error']['message'], sql))
    return d['data']['rows']


def clean(v):
    """`choices` is a comma separated list, so a comma in the text would split it."""
    return str(v).strip().replace(',', ' ')


def pair(rows, opt, val):
    return (','.join(clean(r[opt]) for r in rows),
            ','.join(clean(r[val]) for r in rows))


def build_lists():
    wh = q("SELECT COAPLT || ' ' || COANAM AS OPT, COAPLT AS VAL FROM %s.COADDRES "
           "WHERE COADEL IN ('S','W') ORDER BY COAPLT" % LIB)
    frt = q("SELECT INCOCDE || ' ' || INCODES AS OPT, INCOCDE AS VAL FROM %s.SAPINCO "
            "ORDER BY INCOCDE" % LIB)
    plant = q("SELECT COAPLT || ' ' || SHORTNAME AS OPT, COAPLT AS VAL FROM %s.COADDRNAME "
              "WHERE COADEL <> 'D' AND COAPLT BETWEEN '93' AND '99' ORDER BY COAPLT" % LIB)

    def sorts(app):
        return q("SELECT T1.HLPTXT AS OPT, T2.HLPTXT AS VAL "
                 "FROM %s.HELP T1 JOIN %s.HELP T2 "
                 "  ON T1.HLPAPP = T2.HLPAPP AND T1.HLPVAL = T2.HLPVAL "
                 "WHERE T1.HLPAPP = '%s' AND T1.HLPSUB = 'SORTT' AND T2.HLPSUB = 'SORTV' "
                 "ORDER BY T1.HLPSEQ" % (LIB, LIB, app))

    vend = q("SELECT VNUMB || ' ' || TRIM(VNNAME) AS OPT, VNUMB AS VAL FROM %s.VENDMST "
             "WHERE VNCODE = ' ' ORDER BY VNUMB" % LIB)
    part = q("SELECT TRIM(VRPART) || ' ' || TRIM(VQDESC) AS OPT, VRPART AS VAL "
             "FROM %s.VENDPART WHERE VRDEL <> 'D' ORDER BY VRPART" % LIB)

    return {
        'vendor':    pair(vend, 'OPT', 'VAL'),
        'part':      pair(part, 'OPT', 'VAL'),
        'warehouse': pair(wh, 'OPT', 'VAL'),
        'freight':   pair(frt, 'OPT', 'VAL'),
        'plant':     pair(plant, 'OPT', 'VAL'),
        'sort_po':   pair(sorts('POSELUI'), 'OPT', 'VAL'),
        'sort_req':  pair(sorts('PMSELUI'), 'OPT', 'VAL'),
    }


# widget id -> which list, per source file
TARGETS = {
    'podtlui':   {'OHDC': 'warehouse', 'OHFRT': 'freight',
                  'OHVEND': 'vendor', 'OIPART': 'part'},
    'poselui':   {'SRWH': 'warehouse', 'SRFRT': 'freight', 'SRNN': 'plant',
                  'SRVEND': 'vendor', 'SRPART': 'part',
                  'CTLORDR1': 'sort_po', 'CTLORDR2': 'sort_po', 'CTLORDR3': 'sort_po'},
    'pmselui':   {'SRDC': 'warehouse', 'SRNN': 'plant',
                  'SRVEND': 'vendor', 'SRPART': 'part', 'SRPOIT': 'part',
                  'CTLORDR1': 'sort_req', 'CTLORDR2': 'sort_req', 'CTLORDR3': 'sort_req'},
}


DB_CHOICE_PROPS = ('choices database file', 'choices database file 2',
                   'choices database join', 'choices selection criteria',
                   'choice options field', 'choice values field', 'order by')


def strip_db_choices(item):
    """Remove the database-driven choice properties from one widget."""
    removed = {}
    for k in list(item):
        if k in DB_CHOICE_PROPS or k.startswith('choices parameter value'):
            removed[k] = item.pop(k)
    return removed


def main():
    if not LIB:
        raise SystemExit('IBMI_BUILD_LIBRARY is not set')
    lists = build_lists()
    for name, (opts, vals) in sorted(lists.items()):
        print('  %-10s %d options' % (name, len(opts.split(',')) if opts else 0))
    for src, wanted in TARGETS.items():
        applied, removed = [], {}

        def mutate(obj, wanted=wanted, applied=applied, removed=removed):
            for it in obj.get('items', []):
                key = wanted.get(it.get('id'))
                # textboxes take the same properties - there it drives autocomplete
                if not key or it.get('field type') not in ('select box', 'textbox'):
                    continue
                opts, vals = lists[key]
                gone = strip_db_choices(it)
                if gone:
                    removed.setdefault(it['id'], sorted(gone))
                it['choices'] = opts
                it['choice values'] = vals
                # a blank first entry so a filter can be cleared (select boxes only;
                # on a textbox it would just add an empty suggestion)
                if it.get('field type') == 'select box' and it.get('blank option') != 'true':
                    it['blank option'] = 'true'
                applied.append(it['id'])

        rdf_edit.rewrite('qddssrc/%s.dspf' % src, mutate)
        print('%-24s %s' % ('qddssrc/%s.dspf' % src, ', '.join(applied) or 'nothing matched'))
        for wid, props in sorted(removed.items()):
            print('      %-10s removed db-choice props: %s' % (wid, ', '.join(props)))


if __name__ == '__main__':
    main()
