#!/usr/bin/env bash
#
# Rebuild the sign-on menu launcher.
#
# WHY THIS SCRIPT EXISTS
# ----------------------
# The application lives entirely in TIGERPOC. The ONLY thing that cannot is the
# three-object MENU launcher, because sign-on resolves its initial menu through
# *LIBL/MENU and the sign-on library list is:
#
#   AITSK000nn (SYS) · PLSYS · QSYS · QSYS2 · QHLPSYS · QUSRSYS
#   AIDEMOBASE · QGPL · QTEMP · DRPUIDEV                          (USR)
#
# TIGERPOC is not on it, and neither way of putting it there is available to the
# aidemo profile:
#
#   CHGJOBD  JOBD(AIDEMO/AIDEMO) INLLIBL(TIGERPOC ...)  -> CPD1602 not authorized
#   CHGUSRPRF USRPRF(AIDEMO) CURLIB(TIGERPOC)           -> CPF2292 *SECADM required
#
# So the menu is built into BOTH:
#
#   TIGERPOC  -- its permanent home. Travels with the application, so a save or
#                a restore to a box with a normal library list just works.
#   the TASK library -- the copy sign-on actually resolves here, and the reason
#                this has to be re-run whenever the CoderFlow task library
#                changes. That is the whole of the per-task chore.
#
# One command, both copies, both verified. The stamps are deleted first because
# codermake records THAT a target was built, not WHICH library it went to -- so
# without the rm this is a silent no-op and option 4 simply is not there.
#
# Usage:  gtwms/tools/rebuild-menu.sh
#
set -euo pipefail

cd "$(dirname "$0")/../.."

TASKLIB="${IBMI_BUILD_LIBRARY:?IBMI_BUILD_LIBRARY is not set}"
PERMLIB="TIGERPOC"

stamps() { rm -f build/menu.file build/menu.msgf build/menu.menu; }

echo "==> $PERMLIB (permanent home)"
stamps
IBMI_BUILD_LIBRARY="$PERMLIB" codermake menu.menu

echo
echo "==> $TASKLIB (what sign-on resolves)"
stamps
IBMI_BUILD_LIBRARY="$TASKLIB" codermake menu.menu

echo
echo "==> verifying"
for lib in "$PERMLIB" "$TASKLIB"; do
  cat > /tmp/gt-menu-check.json <<JSON
{"connection":"dev","sql":"select count(*) as n from table(qsys2.object_statistics('$lib','*ALL','MENU'))"}
JSON
  n=$(aitool sql --input @/tmp/gt-menu-check.json 2>/dev/null \
        | python3 -c "import json,sys; d=json.load(sys.stdin); print(d['data']['rows'][0]['N'] if d.get('success') else 0)")
  if [ "$n" = "3" ]; then
    echo "    ok    $lib has all three MENU objects"
  else
    echo "    FAIL  $lib has $n of 3 MENU objects"; exit 1
  fi
done

cat <<'NOTE'

Now sign on and look at the menu -- that is the only check that counts.

To retire the per-task step permanently, someone with *SECADM runs ONE of:

    CHGUSRPRF USRPRF(AIDEMO) CURLIB(TIGERPOC)
    CHGJOBD   JOBD(AIDEMO/AIDEMO) INLLIBL(TIGERPOC AIDEMOBASE QGPL QTEMP DRPUIDEV)

Either puts TIGERPOC ahead of AIDEMOBASE for every new AIDEMO session, at which
point TIGERPOC/MENU is found on its own and this script is never needed again.
Both are additive and reversible with one command; the only name TIGERPOC shares
with AIDEMOBASE is QDDSSRC, a build staging source file nothing opens at runtime.
NOTE
