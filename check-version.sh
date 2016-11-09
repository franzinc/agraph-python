#!/bin/bash

# This script compares the AG version number passed as a parameter
# with the client version stored in src/franz/__init__.py.
# If the client's version does not start with the AG version the 
# script will print an error message and exit with a non-zero code.

BASE_DIR=$(dirname "$0")

AG_VERSION=$1
PY_VERSION=$(python2 -c "execfile('${BASE_DIR}/src/franz/__init__.py'); print(__version__)")

function err() { echo "$@" 1>&2; }

if [[ ! "${PY_VERSION}" == "${AG_VERSION}"* ]]; then
    err "ERROR: Version number mismatch!"
    err "  Client version (in src/franz/__init__.py): ${PY_VERSION}"
    err "  AG version: ${AG_VERSION}"
    exit 1
fi
