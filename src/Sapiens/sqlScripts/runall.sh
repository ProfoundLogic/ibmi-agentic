#!/QOpenSys/usr/bin/sh
# =====================================================================
# runall.sh - Clear and reload sample data for the reverse-engineered
# Sapiens Agency tables. Run ON the IBM i (PASE) after copying this
# folder there.
#
# Usage:   ssh dev "cd /tmp/seed && ./runall.sh"
#
# REVERSE-ENGINEERED PLACEHOLDER DATA - throw away when Sapiens
# delivers real DDS and a real seed strategy.
# =====================================================================
set -eu

LIB="${IBMI_BUILD_LIBRARY:-AITSK00030}"
DIR="$(cd "$(dirname "$0")" && pwd)"

echo "=========================================================="
echo "Target library: $LIB"
echo "SQL dir:        $DIR"
echo "=========================================================="

run_cl() {
    # Run a CL command and detect CPF/CPD failure messages.
    # The IBM i `system` PASE wrapper returns 0 even when the underlying
    # CL command fails, so we must scan stderr/stdout ourselves.
    local cmd="$1"
    local out
    out=$(/usr/bin/system "$cmd" 2>&1)
    echo "$out"
    if echo "$out" | grep -qE 'CPF[0-9]{4}|CPD[0-9]{4}'; then
        # Treat CPF/CPD as failure - except CPF3142 (member doesn't
        # exist) and CPF7301 (file already in target lib) which are
        # benign for re-runs.
        if echo "$out" | grep -qE 'CPF3142|CPF7301'; then
            return 0
        fi
        echo "  >> ERROR detected, aborting."
        return 1
    fi
}

echo ""
echo "--- Clearing existing data ----------------------------------"
for TBL in WMAGP SMCOP WMAAP WDF2P WMCZP WDELP; do
    echo "[CLRPFM] $LIB/$TBL"
    run_cl "CLRPFM FILE($LIB/$TBL)"
done

echo ""
echo "--- Loading sample data -------------------------------------"
# Order: parents before children (per the comments in runall.sql).
for FILE in smcop.sql wmczp.sql wdf2p.sql wmaap.sql wdelp.sql wmagp.sql; do
    SRC="$DIR/$FILE"
    if [ ! -f "$SRC" ]; then
        echo "[skip ] $FILE not found"
        continue
    fi
    echo "[LOAD ] $FILE"
    run_cl "RUNSQLSTM SRCSTMF('$SRC') COMMIT(*NONE) DFTRDBCOL($LIB)" \
        | tail -3
done

echo ""
echo "--- Row counts ----------------------------------------------"
for TBL in WMAGP SMCOP WMAAP WDF2P WMCZP WDELP; do
    CNT=$(/usr/bin/system "DSPFD FILE($LIB/$TBL) TYPE(*MBR)" 2>/dev/null \
          | awk -F: '/Current number of records/{gsub(/ /,"",$2); print $2}')
    printf "  %-10s %s\n" "$TBL:" "$CNT"
done
echo "=========================================================="
