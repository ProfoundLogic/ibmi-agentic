#!/bin/bash
# Export the file metadata reconstruct-dds.py needs from the shipped library.
#
#   prfdemolib/tools/fetch-metadata.sh <SCRATCHLIB> <OUTDIR> [SOURCELIB]
#
# Writes DSPFFD/DSPFD outfiles into SCRATCHLIB (a library you own, e.g. the
# task library), exports them as JSON into OUTDIR, then deletes the outfiles.
# SOURCELIB (default PRFDEMOLIB) is only read.
#
# Then:  python3 prfdemolib/tools/reconstruct-dds.py OUTDIR prfdemolib/qddssrc
set -euo pipefail
LIB=${1:?usage: $0 SCRATCHLIB OUTDIR [SOURCELIB]}; OUT=${2:?usage}; SRC=${3:-PRFDEMOLIB}
LIB=$(echo "$LIB" | tr a-z A-Z); SRC=$(echo "$SRC" | tr a-z A-Z)
SSH_HOST=${PRFDEMO_SSH_HOST:-dev}
DB2=/QOpenSys/pkgs/bin/db2util
mkdir -p "$OUT"
NOTSRC="not like 'Q%' and"   # skip the source physical files
ssh "$SSH_HOST" "system -q 'DSPFFD FILE($SRC/*ALL) OUTPUT(*OUTFILE) OUTFILE($LIB/ZZPRFFFD)';
  system -q 'DSPFD FILE($SRC/*ALL) TYPE(*ACCPTH) OUTPUT(*OUTFILE) FILEATR(*PF *LF) OUTFILE($LIB/ZZPRFACC)';
  system -q 'DSPFD FILE($SRC/*ALL) TYPE(*SELECT) OUTPUT(*OUTFILE) FILEATR(*LF) OUTFILE($LIB/ZZPRFSEL)';
  system -q 'DSPFD FILE($SRC/*ALL) TYPE(*BASATR) OUTPUT(*OUTFILE) FILEATR(*PF *LF) OUTFILE($LIB/ZZPRFBAS)'"
ssh "$SSH_HOST" "$DB2 -o json \"select WHFILE,WHFTYP,WHNAME,WHSEQ,WHTEXT,WHFLDN,WHRLEN,WHFLDI,WHFLDE,WHFOBO,WHFLDB,WHFLDD,WHFLDP,WHFTXT,WHCHD1,WHCHD2,WHCHD3,WHFLDT,WHFIOB,WHECDE,WHEWRD,WHALIS,WHDFT,WHDFTL,WHCSID,WHNULL,WHVARL,WHALLC,WHRFIL,WHRFLD,WHFMT,WHSEP,WHJREF,WHSQLT from $LIB.ZZPRFFFD where WHFILE $NOTSRC WHFILE not in ('MMCPY','EVFEVENT') order by WHFILE, WHFOBO\"" > "$OUT/ffd.json"
ssh "$SSH_HOST" "$DB2 -o json \"select APFILE, APFTYP, APUNIQ, APBOF, APBOLF, APKEYN, APKSEQ, APKEYF, APACCP from $LIB.ZZPRFACC where APFILE $NOTSRC APFILE not in ('MMCPY','EVFEVENT') order by APFILE, APKEYN\"" > "$OUT/acc.json"
ssh "$SSH_HOST" "$DB2 -o json \"select SOFILE, SORFMT, SORULE, SOCOMP, SOVALU, SOFLD, SONRUL from $LIB.ZZPRFSEL where SONRUL>0\"" > "$OUT/sel.json"
ssh "$SSH_HOST" "$DB2 -o json \"select ATFILE, ATTXT, ATFATR from $LIB.ZZPRFBAS where ATFILE $NOTSRC ATFILE not in ('MMCPY','EVFEVENT')\"" > "$OUT/bas.json"
ssh "$SSH_HOST" "for f in ZZPRFFFD ZZPRFACC ZZPRFSEL ZZPRFBAS; do system -q \"DLTF FILE($LIB/\$f)\"; done"
echo "Metadata for $SRC written to $OUT"
