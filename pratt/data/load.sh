#!/bin/bash
# Load the generated demo data into $IBMI_BUILD_LIBRARY.
#
# seed.sql deliberately uses UNQUALIFIED table names; the target library is
# supplied at run time through RUNSQLSTM DFTRDBCOL, so nothing in the repo ever
# hardcodes an AITSKxxxxx library name.
set -e
cd "$(dirname "$0")"
LIB="${1:-$IBMI_BUILD_LIBRARY}"
if [ -z "$LIB" ]; then echo "No library given and IBMI_BUILD_LIBRARY is not set." >&2; exit 2; fi

python3 gen_seed.py > seed.sql
echo "Loading $(grep -c '^INSERT' seed.sql) rows into $LIB ..."

REMOTE=/home/aidemo/pratt_seed.sql
scp -q seed.sql dev:$REMOTE
ssh dev "system \"RUNSQLSTM SRCSTMF('$REMOTE') COMMIT(*NONE) NAMING(*SYS) DFTRDBCOL($LIB) ERRLVL(20)\"" \
  | tail -20
