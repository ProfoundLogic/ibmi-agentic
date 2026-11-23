#!/usr/bin/env bash
# ---------------------------------------------------------------------------
# Load the ACME (ACM) demo dataset into PERPDEMO on the dev IBM i.
#
#   perp/tools/acme_seed/load.sh [path/to/030_acme_demo_data.sql]
#
# Copies the generated script to the IFS and runs it with RUNSQLSTM under
# commitment control, so the whole reseed commits or rolls back as one unit.
# Regenerate the script first (gen_acme_seed.py) to re-anchor the dates.
# ---------------------------------------------------------------------------
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
SQL="${1:-$HERE/../../qddlsrc/seed/030_acme_demo_data.sql}"
REMOTE="/tmp/acme_demo_data.sql"

scp -q "$SQL" "dev:$REMOTE"
ssh dev "system -i \"RUNSQLSTM SRCSTMF('$REMOTE') COMMIT(*CHG) NAMING(*SQL) ERRLVL(10)\"" </dev/null
echo "RUNSQLSTM completed. Check row counts with the queries in README.md."
