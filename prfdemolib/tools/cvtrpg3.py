#!/usr/bin/env python3
"""Convert an RPG III (RPG/400, OPM) source member to ILE RPG (RPG IV)
fixed-format source - the same job IBM's CVTRPGSRC does, for the subset of
RPG III that the PRFDEMOLIB programs use.

    cvtrpg3.py prfdemolib/qrpgsrc/inv046.rpg > out.rpgle

It is deliberately mechanical: every calculation keeps its line, order,
indicators and opcode, so the converted program follows exactly the same
flow as the original. What changes is only the column layout and the
handful of names RPG IV spells differently:

  * H/F/E/I specs become H/F/D specs (E-spec arrays -> D DIM(); I-spec data
    structures, named constants, SDS and INFDS -> D specs; an E-spec array
    that a DS also defines becomes a DIM() subfield of that DS)
  * C specs move to the RPG IV columns; half adjust (col 53) becomes (H)
  * opcodes: DEFN->DEFINE, RETRN->RETURN, SELEC->SELECT, UPDAT->UPDATE,
    WHxx->WHENxx, LOKUP->LOOKUP, DELET->DELETE, EXCPT->EXCEPT, CHEKR->CHECKR,
    REDPE->READPE, OCUR->OCCUR, COMIT->COMMIT, *PROGRAM->*PROC
  * comments in cols 60-74 move to cols 81+
  * compile-time data (** sections) is copied unchanged

Anything it does not understand raises an error rather than guessing, so a
construct outside the supported subset can never be silently mistranslated.
"""
import re
import sys

OPMAP = {'DEFN': 'DEFINE', 'RETRN': 'RETURN', 'SELEC': 'SELECT', 'UPDAT': 'UPDATE',
         'LOKUP': 'LOOKUP', 'DELET': 'DELETE', 'EXCPT': 'EXCEPT', 'CHEKR': 'CHECKR',
         'REDPE': 'READPE', 'OCUR': 'OCCUR', 'COMIT': 'COMMIT', 'SETOF': 'SETOFF'}
CMP = ('EQ', 'NE', 'GT', 'LT', 'GE', 'LE')


def col(line, a, b):
    """1-based inclusive column slice"""
    return line[a - 1:b].ljust(b - a + 1)


def pad(s, n, right=False):
    lead = s.startswith(' ') and not right and s.strip()
    s = (' ' + s.strip()) if lead else s.strip()
    if len(s) > n:
        raise ValueError('value %r does not fit %d columns' % (s, n))
    return s.rjust(n) if right else s.ljust(n)


def dspec(name='', ext='', dst='', deft='', frm='', to='', dt='', dec='', kw=''):
    """RPG IV D spec by exact columns: name 7-21, E 22, S/U 23, type 24-25,
    from 26-32, to/length 33-39, data type 40, decimals 41-42, keywords 44+"""
    l = ('     D' + pad(name, 15) + pad(ext, 1) + pad(dst, 1) + pad(deft, 2) + pad(frm, 7, True) +
         pad(to, 7, True) + pad(dt, 1) + pad(dec, 2, True) + ' ' + kw)
    assert len(l) <= 80 or l[43:].startswith(kw), l
    return l.rstrip()


def op4(op):
    op = op.strip().upper()
    if op in OPMAP:
        return OPMAP[op]
    if re.fullmatch(r'WH(%s)' % '|'.join(CMP), op):
        return 'WHEN' + op[2:]
    return op


def c_spec(l):
    lvl, ind = col(l, 7, 8), col(l, 9, 17)
    f1, op, f2, res = col(l, 18, 27), col(l, 28, 32), col(l, 33, 42), col(l, 43, 48)
    ln, dec, hadj, hi = col(l, 49, 51), col(l, 52, 52), col(l, 53, 53), col(l, 54, 59)
    cmt = l[59:74].rstrip() if len(l) > 59 else ''
    inds = re.findall(r'(N?)\s*(\d\d|L\d|LR|H\d|U\d|OA|OF|O[B-G]|OV|K[A-Y]|MR|RT|1P)', ind)
    if len(inds) > 1:
        raise ValueError('multiple conditioning indicators not supported: %r' % l)
    i4 = ''
    if inds:
        n, i = inds[0]
        i4 = (n or ' ') + i
    o = op4(op)
    if hadj.strip() == 'H':
        o += '(H)'
    # array index: RPG III ARR,N -> RPG IV ARR(N)  (also *IN,nn -> *IN(nn))
    def idx(v):
        v = v.strip()
        m = re.fullmatch(r"(\*?[A-Z0-9#@$]+),([A-Z0-9#@$]+)", v, re.I)
        return '%s(%s)' % (m.group(1), m.group(2)) if m and not v.startswith("'") else v
    f1, f2, res = idx(f1), idx(f2), idx(res)
    # RPG III CLEAR/RESET name the target in factor 2; RPG IV uses the result field
    if o in ('CLEAR', 'RESET') and f2 and not res and not f2.startswith('*'):
        f2, res = '', f2
    # resulting indicators keep their HI/LO/EQ positions exactly
    out = ('     C' + pad(lvl, 2) + pad(i4, 3) + pad(f1, 14) + pad(o, 10) + pad(f2, 14) +
           pad(res, 14) + pad(ln, 5, True) + pad(dec, 2, True) + hi.ljust(6)[:6])
    out = out.rstrip()
    if cmt.strip():
        out = out.ljust(80) + cmt.strip()
    return out


def convert(src, file_overrides=None):
    lines = src.split('\n')
    out = []
    arrays = {}       # E-spec arrays: name -> dict
    ds_subfields = set()
    i = 0
    # pre-scan I-spec subfield names so arrays defined by a DS get DIM there
    for l in lines:
        if len(l) > 6 and l[5] in 'iI' and l[6] != '*' and col(l, 53, 58).strip():
            ds_subfields.add(col(l, 53, 58).strip().upper())
    pending_consts = []
    in_ctdata = False
    fspecs = []
    while i < len(lines):
        l = lines[i]
        if in_ctdata or l.startswith('**'):
            in_ctdata = True
            out.append(l)
            i += 1
            continue
        if len(l) < 6 or (len(l) > 6 and l[6] == '*') or not l[5:6].strip():
            out.append(l)
            i += 1
            continue
        t = l[5].upper()
        if t == 'H':
            out.append('     H')       # RPG III H-spec options have no RPG IV equivalent here
        elif t == 'F':
            if col(l, 7, 14).strip():
                fspecs.append({'name': col(l, 7, 14).strip(), 'type': col(l, 15, 15), 'desig': col(l, 16, 16),
                               'fmt': col(l, 19, 19), 'keyed': col(l, 31, 31) == 'K',
                               'dev': col(l, 40, 46).strip(), 'kw': [],
                               'uc': col(l, 71, 72) == 'UC', 'cont': col(l, 53, 53) == 'K',
                               'k': col(l, 54, 59).strip(), 'v': col(l, 60, 67).strip(), 'line': l})
                f = fspecs[-1]
                if f['cont'] and f['k']:
                    f['kw'].append((f['k'], f['v'], '', col(l, 47, 52).strip()))
            else:   # continuation line
                k, v = col(l, 54, 59).strip(), col(l, 60, 67).strip()
                fspecs[-1]['kw'].append((k, v, col(l, 19, 28).strip(), col(l, 47, 52).strip()))
            # flush F specs when the next line is not an F spec
            nxt = lines[i + 1] if i + 1 < len(lines) else ''
            if not (len(nxt) > 6 and nxt[5] in 'fF' and nxt[6] != '*'):
                for f in fspecs:
                    kws = []
                    for k, v, ext, extra in f['kw']:
                        k = k.upper()
                        if k == 'INFDS':
                            kws.append('INFDS(%s)' % v)
                        elif k == 'SFILE':
                            kws.append('SFILE(%s:%s)' % (v, extra))
                        elif k == 'RENAME':
                            kws.append('RENAME(%s:%s)' % (ext, v))
                        else:
                            raise ValueError('F-spec keyword %s not supported' % k)
                    if f['uc']:
                        kws.append('USROPN')
                    name = f['name']
                    if file_overrides and name.upper() in file_overrides:
                        ov = file_overrides[name.upper()]
                        name = ov.get('name', name)
                        kws += ov.get('kw', [])
                    head = ('     F' + pad(name, 10) + f['type'] + f['desig'] + ' ' + ' ' + ' ' + f['fmt'] +
                            ' ' * 5 + ' ' + ' ' * 5 + ('K' if f['keyed'] else ' ') + ' ' + pad(f['dev'], 7) + ' ')
                    first = True
                    for kw in (kws or ['']):
                        if first:
                            out.append((head + kw).rstrip())
                            first = False
                        else:
                            out.append(('     F' + ' ' * 37 + kw).rstrip())
                fspecs = []
        elif t == 'E':
            name = col(l, 27, 32).strip()
            per, num, ln, fmt, dec = (col(l, 33, 35).strip(), col(l, 36, 39).strip(), col(l, 40, 42).strip(),
                                      col(l, 43, 43).strip(), col(l, 44, 44).strip())
            if col(l, 11, 26).strip():
                raise ValueError('E-spec from/to file not supported: %r' % l)
            arrays[name.upper()] = {'per': per, 'num': num, 'len': ln, 'fmt': fmt, 'dec': dec}
            if name.upper() not in ds_subfields:
                kw = 'DIM(%s)' % num
                if per:
                    kw += ' CTDATA PERRCD(%s)' % per
                dt = {'P': 'P', 'B': 'B'}.get(fmt, 'S' if dec else '')
                out.append(dspec(name, deft='S', to=ln, dt=dt, dec=dec, kw=kw))
        elif t == 'I':
            rest = col(l, 7, 20)
            # named constant (literal in 21-42, 'C' in 43, name in 53-58), maybe continued with '-'
            if col(l, 21, 21) == "'" or (col(l, 43, 43) == 'C' and col(l, 53, 58).strip()):
                lit = ''
                name = col(l, 53, 58).strip()
                j = i
                while True:
                    seg = lines[j][20:42].rstrip()
                    if seg.startswith("'"):      # every segment opens with a quote
                        seg = seg[1:]
                    lit += seg
                    if lit.endswith('-'):
                        lit = lit[:-1]
                        j += 1
                        continue
                    break
                if not lit.endswith("'"):
                    raise ValueError('named constant not closed: %r' % l)
                lit = lit[:-1]
                i = j
                # emit with RPG IV '-' continuation at col 44
                body = "CONST('" + lit + "')"
                chunks = []
                while len(body) > 36:
                    chunks.append(body[:35] + '-')
                    body = body[35:]
                chunks.append(body)
                out.append(dspec(name, deft='C', kw=chunks[0]))
                for c in chunks[1:]:
                    out.append(dspec(kw=c))
            elif col(l, 19, 20).upper() == 'DS' or col(l, 18, 20).upper() == 'SDS':
                nm = col(l, 7, 12).strip()
                ext = col(l, 17, 17).upper() == 'E'
                sds = col(l, 18, 20).upper() == 'SDS'
                extname = col(l, 21, 30).strip()
                kw = ('EXTNAME(%s)' % extname) if ext else ''
                out.append(dspec(nm, ext='E' if ext else '', dst='S' if sds else '', deft='DS', kw=kw))
            elif col(l, 53, 58).strip():
                name = col(l, 53, 58).strip()
                frm, to, dec, fmt = col(l, 44, 47).strip(), col(l, 48, 51).strip(), col(l, 52, 52).strip(), col(l, 43, 43).strip()
                kw = ''
                if frm.startswith('*'):          # PSDS keywords *PROGRAM / *PARMS / *STATUS ...
                    kwd = col(l, 44, 51).strip().upper()
                    kwd = {'*PROGRAM': '*PROC'}.get(kwd, kwd)
                    # keyword is left-justified from column 26
                    out.append(('     D' + pad(' ' + name, 15) + ' ' * 4 + kwd).rstrip())
                    i += 1
                    continue
                a = arrays.get(name.upper())
                if a:
                    kw = 'DIM(%s)' % a['num']
                dt = 'B' if fmt == 'B' else ('P' if fmt == 'P' else ('S' if dec else ' '))
                out.append(dspec(' ' + name, frm=frm, to=to, dt=dt.strip(), dec=dec, kw=kw))
            else:
                raise ValueError('I-spec not supported: %r' % l)
        elif t == 'C':
            out.append(c_spec(l))
        else:
            raise ValueError('spec type %s not supported: %r' % (t, l))
        i += 1
    return '\n'.join(out)


if __name__ == '__main__':
    sys.stdout.write(convert(open(sys.argv[1]).read()))
