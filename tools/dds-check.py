#!/usr/bin/env python3
"""Assert DDS source cannot overflow column 80.

DDS keywords start in column 45 and the record ends at column 80, so a
TEXT()/COLHDG() string has a 28-character budget. Overflow does not fail
cleanly: CRTPF truncates mid-string and reports `CPD7508 Closing apostrophe
missing`, which points at entirely the wrong problem. It hit 7 of the 9
original Fletcher physical files, so every DDS member gets checked here
before it is handed to codermake.

  tools/dds-check.py cfdemo/qddssrc/*.pf
"""
import sys

def check(path):
    problems = []
    # A quoted constant may legitimately span two lines: the first ends with a
    # continuation "-" and the second carries the closing apostrophe. Without
    # tracking that state, every continued constant in the repo reads as an
    # unclosed string.
    pending = False
    for n, line in enumerate(open(path, encoding="utf-8"), 1):
        line = line.rstrip("\n")
        if len(line.rstrip()) > 80:
            problems.append(f"{path}:{n}: {len(line.rstrip())} chars, past column 80")
        # A DDS comment (A* in columns 6-7) is free-form text.
        body = line[:80]
        if body[5:7] == "A*":
            continue
        odd = bool(body.count("'") % 2)
        if pending:
            pending = not odd          # an odd count here closes the constant
            continue
        if odd:
            if body.rstrip().endswith("-"):
                pending = True
            else:
                problems.append(
                    f"{path}:{n}: odd number of apostrophes (unclosed string?)")
    if pending:
        problems.append(f"{path}: file ends inside a continued constant")
    return problems

def main(argv):
    if not argv:
        print(__doc__)
        return 2
    bad = []
    for p in argv:
        bad += check(p)
    for b in bad:
        print(b)
    print(f"{'FAIL' if bad else 'OK'} - {len(argv)} member(s), {len(bad)} problem(s)")
    return 1 if bad else 0

if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
