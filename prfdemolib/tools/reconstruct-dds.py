#!/usr/bin/env python3
"""Reconstruct DDS source for the PRFDEMOLIB data files from their metadata.

PRFDEMOLIB ships its physical and logical files as objects only.  This script
turns the DSPFFD / DSPFD outfile metadata of those files into DDS members so
the files can be rebuilt by codermake into any library with identical record
format level identifiers (no level checks against the shipped objects).

Usage:
    reconstruct-dds.py METADIR OUTDIR

METADIR holds four JSON exports (db2util -o json) of the outfiles produced by

    DSPFFD FILE(PRFDEMOLIB/*ALL) OUTPUT(*OUTFILE) OUTFILE(x/ZFFD)          -> ffd.json
    DSPFD  FILE(PRFDEMOLIB/*ALL) TYPE(*ACCPTH) OUTPUT(*OUTFILE) ...ZFDACCPTH -> acc.json
    DSPFD  FILE(PRFDEMOLIB/*ALL) TYPE(*SELECT) FILEATR(*LF) ...ZFDSELECT    -> sel.json
    DSPFD  FILE(PRFDEMOLIB/*ALL) TYPE(*BASATR) ...ZFDBASATR                 -> bas.json

See prfdemolib/tools/fetch-metadata.sh for the exact queries.  Join logical
files are skipped: INVVENJ2 has its original DDS in qddssrc already.
"""
import json
import os
import sys
from collections import OrderedDict

SKIP = {'INVVENJ2', 'EVFEVENT', 'MMCPY', 'QCLSRC', 'QDDSSRC', 'QRPGLESRC',
        'QRPGSRC'}


def load(path):
    with open(path) as fh:
        return json.load(fh)['records']


def s(v):
    return v.strip() if isinstance(v, str) else v


def quote(text):
    return "'" + text.replace("'", "''") + "'"


def spec(typ='', name='', length='', dtype='', dec='', kw=''):
    """Build one or more DDS A-spec lines.  Keywords start in column 45 and
    are continued with '-' in column 80 when longer than 36 characters."""
    head = ('     A' + ' ' * 10 + typ.ljust(1) + ' ' + name.ljust(10) + ' ' +
            str(length).rjust(5) + dtype.ljust(1) + str(dec).rjust(2) +
            ' ' + ' ' * 6)
    assert len(head) == 44, head
    if len(kw) <= 36:
        return [(head + kw).rstrip()]
    lines = []
    cur = head
    rest = kw
    while len(rest) > 36:
        lines.append(cur + rest[:35] + '-')
        rest = rest[35:]
        cur = '     A' + ' ' * 38
    lines.append(cur + rest)
    return lines


def field_lines(f):
    name = s(f['WHFLDI'])
    t = f['WHFLDT']
    out = []
    if t == 'A':
        out += spec(name=name, length=f['WHFLDB'])
    elif t in 'PSB':
        out += spec(name=name, length=f['WHFLDD'], dtype=t, dec=f['WHFLDP'])
    elif t == 'L':
        out += spec(name=name, dtype='L')
        out += spec(kw='DATFMT(%s)' % s(f['WHFMT']))
    elif t == 'T':
        out += spec(name=name, dtype='T')
        out += spec(kw='TIMFMT(%s)' % s(f['WHFMT']))
    else:
        raise SystemExit('unsupported type %s for %s' % (t, name))
    txt = s(f['WHFTXT'])
    if txt:
        out += spec(kw='TEXT(%s)' % quote(txt))
    hdgs = [s(f[k]) for k in ('WHCHD1', 'WHCHD2', 'WHCHD3')]
    while hdgs and not hdgs[-1]:
        hdgs.pop()
    if hdgs:
        out += spec(kw='COLHDG(%s)' % ' '.join(quote(h) for h in hdgs))
    ecde = s(f['WHECDE'])
    if ecde:
        out += spec(kw='EDTCDE(%s)' % ecde)
    ewrd = s(f['WHEWRD'])
    if ewrd:
        out += spec(kw='EDTWRD(%s)' % ewrd)
    return out


def main(metadir, outdir):
    ffd = load(os.path.join(metadir, 'ffd.json'))
    acc = load(os.path.join(metadir, 'acc.json'))
    sel = load(os.path.join(metadir, 'sel.json'))
    bas = {s(r['ATFILE']): r for r in load(os.path.join(metadir, 'bas.json'))}

    fields = OrderedDict()
    for r in ffd:
        fields.setdefault(s(r['WHFILE']), []).append(r)
    keys = OrderedDict()
    for r in acc:
        keys.setdefault(s(r['APFILE']), []).append(r)
    rules = OrderedDict()
    for r in sel:
        rules.setdefault(s(r['SOFILE']), []).append(r)

    written = []
    for fname, flds in fields.items():
        if fname in SKIP:
            continue
        first = flds[0]
        ftype = first['WHFTYP']
        fmt = s(first['WHNAME'])
        ftxt = s(bas[fname]['ATTXT']) if fname in bas else ''
        k = keys.get(fname, [])
        ext = '.pf' if ftype == 'P' else '.lf'
        lines = ['      * @MBRTEXT: %s' % (ftxt or fname),
                 '     A* Reconstructed from the PRFDEMOLIB/%s object by' % fname,
                 '     A* prfdemolib/tools/reconstruct-dds.py. Format level',
                 '     A* identifier of the shipped object: %s' % first['WHSEQ']]
        if k and k[0]['APUNIQ'] == 'Y':
            lines += spec(kw='UNIQUE')
        if ftype == 'P':
            rk = 'TEXT(%s)' % quote(s(first['WHTEXT'])) if s(first['WHTEXT']) else ''
            lines += spec(typ='R', name=fmt, kw=rk)
            for f in flds:
                lines += field_lines(f)
        else:
            pf = s(k[0]['APBOF'])
            lines += spec(typ='R', name=fmt, kw='PFILE(%s)' % pf)
        for kr in k:
            if s(kr['APKEYF']):
                lines += spec(typ='K', name=s(kr['APKEYF']),
                              kw='DESCEND' if kr['APKSEQ'] == 'D' else '')
        for sr in rules.get(fname, []):
            comp = s(sr['SOCOMP'])
            if comp == 'AL':
                lines += spec(typ=s(sr['SORULE']), kw='ALL')
            elif comp == 'VA':
                lines += spec(typ=s(sr['SORULE']), name=s(sr['SOFLD']),
                              kw='VALUES(%s)' % s(sr['SOVALU']))
            else:
                val = s(sr['SOVALU']).lstrip('+')
                lines += spec(typ=s(sr['SORULE']), name=s(sr['SOFLD']),
                              kw='COMP(%s %s)' % (comp, val))
        path = os.path.join(outdir, fname.lower() + ext)
        with open(path, 'w') as fh:
            fh.write('\n'.join(lines) + '\n')
        for ln in lines:
            assert len(ln) <= 80, (fname, ln)
        written.append(path)
    print('\n'.join(written))


if __name__ == '__main__':
    if len(sys.argv) != 3:
        sys.exit(__doc__)
    main(sys.argv[1], sys.argv[2])
