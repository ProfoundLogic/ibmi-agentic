#!/bin/bash
# Build (or inspect) only the pratt/ targets.
#
# Never run bare `codermake` in this repo while the pratt work is in progress:
# the shared build/ stamp directory no longer has stamps for cfdemo and awdemo,
# so a bare run would also (re)create cfdemo's CUSTP in the task library as an
# EMPTY file, which then shadows the populated copy in AIDEMOBASE.
#
#   ./pratt/build.sh                 build everything under pratt/
#   ./pratt/build.sh --list-targets  show what is outstanding, pratt only
#   ./pratt/build.sh -n              dry run
set -e
cd "$(dirname "$0")/.."
TARGETS=$(grep -oE "^[a-z0-9]+\.(file|pgm|menu|msgf|srvpgm|module|bnddir)" pratt/Rules.mk \
          | sort -u | sed 's|^|pratt/|')
exec codermake "$@" $TARGETS
