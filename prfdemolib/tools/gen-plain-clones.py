#!/usr/bin/env python3
"""Generate the plain web clones (menu options 7-9) from the Variety
Wholesalers web clones (options 10-12).

    python3 prfdemolib/tools/gen-plain-clones.py          # write
    python3 prfdemolib/tools/gen-plain-clones.py --check  # fail if stale

The plain programs are the SAME program as the VW web ones - same RPG, same
ejsIn/ejsOut, same field contract - running on a different EJS display file
whose templates are a straight web rendering of the green screen. The only
source differences are the display-file name (same length, so fixed-format
columns never move), the member text and the header comments. Never edit the
*E members by hand: edit the *W member and re-run this.
"""
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, '..'))
CHECK = '--check' in sys.argv

CLONES = [
    ('qrpglesrc/inv046w.rpgle', 'qrpglesrc/inv046e.rpgle', [
        ('INV046WD', 'INV046ED'),
        ('* @MBRTEXT: INQ - INVENTORY BAL ALL STORES - VW web (EJS) version',
         '* @MBRTEXT: INQ - INVENTORY BAL ALL STORES - plain web (EJS)'),
        ('Web clone INV046W (menu option 10)', 'Plain clone INV046E (menu option 7)'),
    ]),
    ('qrpglesrc/inv502w.rpgle', 'qrpglesrc/inv502e.rpgle', [
        ('INV502WD', 'INV502ED'),
        ('@MBRTEXT: Item Inquiry - Variety Wholesalers web (EJS) version',
         '@MBRTEXT: Item Inquiry - plain web (EJS) version'),
        ('Web clone INV502W (menu option 11)', 'Plain clone INV502E (menu option 8)'),
    ]),
    ('qrpglesrc/ts0110w.rpgle', 'qrpglesrc/ts0110e.rpgle', [
        ('TS0110WD', 'TS0110ED'), ('Ts0110wd', 'Ts0110ed'),
        ('@MBRTEXT: Ticket Item Inventory - Variety Wholesalers web (EJS)',
         '@MBRTEXT: Ticket Item Inventory - plain web (EJS)'),
        ('Web clone TS0110W (menu option 12)', 'Plain clone TS0110E (menu option 9)'),
    ]),
    ('qclsrc/inv046clw.cl', 'qclsrc/inv046cle.cl', [
        ('INVENTORY BALANCE INQUIRY - VW WEB (EJS) VERSION', 'INVENTORY BALANCE INQUIRY - PLAIN WEB (EJS) VERSION'),
        ('Web clone of INV046CL for menu option 10: calls INV046W',
         'Plain clone of INV046CL for menu option 7: calls INV046E'),
        ('INV046WD', 'INV046ED'), ('PGM(INV046W)', 'PGM(INV046E)'),
    ]),
]
NOTE = {
    '.rpgle': '      * GENERATED from %s by prfdemolib/tools/gen-plain-clones.py - do not edit\n',
    '.cl': '/* GENERATED from %s by prfdemolib/tools/gen-plain-clones.py */\n',
}

stale = []
for src, dst, subs in CLONES:
    text = open(os.path.join(ROOT, src)).read()
    for old, new in subs:
        if old not in text:
            sys.exit('%s: expected text not found: %r' % (src, old))
        text = text.replace(old, new)
    lines = text.split('\n')
    # keep the leading @MBRTEXT/@MBRTYPE lines first (codermake reads them)
    at = 0
    while at < len(lines) and '@MBR' in lines[at]:
        at += 1
    lines.insert(at, (NOTE[os.path.splitext(dst)[1]] % os.path.basename(src)).rstrip('\n'))
    text = '\n'.join(lines)
    p = os.path.join(ROOT, dst)
    cur = open(p).read() if os.path.exists(p) else None
    if cur != text:
        stale.append(dst)
        if not CHECK:
            open(p, 'w').write(text)
if CHECK and stale:
    sys.exit('out of date: ' + ', '.join(stale))
print('%d clones, %d %s' % (len(CLONES), len(stale), 'stale' if CHECK else 'written'))
