#!/usr/bin/env python3
"""Read and rewrite the widget JSON embedded in a Profound UI Rich Display DDS.

A Rich Display File's design source is DDS whose HTML() keywords carry the screen
definition as JSON.  Long screens are split across several HTML() keywords which
the runtime simply concatenates, and each keyword is itself split across DDS
continuation lines (35 content characters per line, '-' in column 80).  Single
quotes inside the JSON are doubled, DDS style.

This module parses that back into Python objects and emits it again byte-for-byte
compatibly, so a screen property can be added without hand editing columns.
"""
import json, re, sys

PREFIX = '     A                                  1  2'   # 44 chars, as emitted by PUI
WIDTH = 80
BODY_START = 44
CONT_LEN = WIDTH - BODY_START - 1                          # 35 chars before the '-'


def _unquote(chunk):
    return chunk.replace("''", "'")


def parse(path):
    """-> (lines, screens) where screens is a list of
    (first_line_idx, last_line_idx, json_obj) for each embedded screen definition."""
    lines = open(path, encoding='utf-8', errors='replace').read().splitlines()

    def content(idx):
        """The keyword text carried by one line, and whether it continues."""
        ln = lines[idx]
        cont = len(ln) >= WIDTH and ln[WIDTH - 1] == '-'
        return (ln[BODY_START:WIDTH - 1] if cont else ln[BODY_START:].rstrip()), cont

    kws, i = [], 0
    while i < len(lines):
        body = lines[i][BODY_START:] if len(lines[i]) > BODY_START else ''
        if body.startswith("HTML('"):
            start = i
            txt, cont = content(i)
            buf = txt[len("HTML('"):]
            while cont:
                i += 1
                txt, cont = content(i)
                buf += txt
            assert buf.endswith("')"), 'unterminated HTML() at line %d' % (start + 1)
            kws.append((start, i, _unquote(buf[:-2])))
        i += 1

    # Consecutive keywords whose concatenation parses as JSON form one screen
    screens, j = [], 0
    while j < len(kws):
        if kws[j][2].lstrip().startswith('{'):
            acc, k, obj = kws[j][2], j + 1, None
            while True:
                try:
                    obj = json.loads(acc)
                    break
                except Exception:
                    if k >= len(kws):
                        break
                    acc += kws[k][2]
                    k += 1
            if obj is not None:
                screens.append((kws[j][0], kws[k - 1][1], obj))
                j = k
                continue
        j += 1
    return lines, screens


def _emit_keyword(value):
    """Render one HTML('...') keyword as DDS lines.

    A continued line carries 35 content characters with '-' in column 80; the
    closing line has all 36 columns but must fit the trailing "')" as well.
    """
    text = value.replace("'", "''")
    out, first, pos = [], True, 0
    while True:
        lead = len("HTML('") if first else 0
        avail_cont = CONT_LEN - lead                      # this line continues
        avail_last = (WIDTH - BODY_START) - lead - 2      # this line closes
        if len(text) - pos <= avail_last:
            chunk, last = text[pos:], True
            pos = len(text)
        else:
            # A continuation line's content is exactly columns 45-79, so the
            # chunk must be full width - padding it would inject real spaces into
            # the string.  PUI's own generator splits doubled quotes across lines
            # here too, and the DDS compiler joins before scanning, so that is safe.
            chunk = text[pos:pos + avail_cont]
            pos += len(chunk)
            last = False
        body = ("HTML('" if first else '') + chunk + ("')" if last else '')
        line = (PREFIX if first else ' ' * BODY_START) + body
        if not last:
            line = line.ljust(WIDTH - 1) + '-'
        out.append(line)
        first = False
        if last:
            return out


def emit(obj, chunk=2000):
    """Render a screen object as the DDS lines for one or more HTML() keywords."""
    text = json.dumps(obj, separators=(',', ':'), ensure_ascii=False)
    lines = []
    for i in range(0, len(text), chunk):
        lines += _emit_keyword(text[i:i + chunk])
    return lines


def rewrite(path, mutate):
    """Apply mutate(screen_obj) to every embedded screen and write the file back."""
    lines, screens = parse(path)
    changed = 0
    for start, end, obj in reversed(screens):          # back to front: indices stay valid
        before = json.dumps(obj, sort_keys=True)
        mutate(obj)
        if json.dumps(obj, sort_keys=True) != before:
            changed += 1
        lines[start:end + 1] = emit(obj)
    open(path, 'w', encoding='utf-8').write('\n'.join(lines) + '\n')
    return changed, len(screens)
