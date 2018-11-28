#!/bin/bash

# Usage: conda-install.sh [PYTHON_MAJOR_VERSION [MINICONDA_VERSION [ARCH]]]
# e.g. conda-install.sh 3 4.3.30 x86_64
# Default Python major version: 3 (can be 2 or 3).
# Default miniconda version: 4.3.30 (or 4.3.30.1 on Mac)
# Default arch: x86_64

# First check if miniconda3 is already installed and has the desired
# version.  If that is not the case, try to install from a downloaded
# copy on the san at Franz. If that's not there, then download from
# the web.

set -e

if [ $# -gt 3 ]
then
    echo "ERROR: Wrong number of arguments"
    echo "Usage: "
    echo "  ${0} [PYTHON_MAJOR_VERSION [MINICONDA_VERSION [ARCH]]]"
    exit
fi

# Check OS - this scripts should work on Mac and Linux
if [[ "$OSTYPE" = darwin* ]]; then
    # Looks like a Mac...
    OS=MacOSX
    DEFAULT_VERSION=4.5.11
else
    # Just assume Linux.
    OS=Linux
    DEFAULT_VERSION=4.5.11
fi

# Python major version, can be 2 or 3
PYTHON_VERSION="${1:-3}"
# Miniconda version number (e.g. 4.3.30)
VERSION="${2:-${DEFAULT_VERSION}}"
# Architecture - x86 or x86_64
ARCH="${3:-x86_64}"
VERSION_AND_ARCH="${VERSION}-${ARCH}"
# Path to files stored on the SAN.
LOCAL_DIR="/net/san1/disk1/conda"
# Path to the file we're looking for
LOCAL_ARCHIVE="${LOCAL_DIR}/Miniconda${PYTHON_VERSION}-${VERSION}-${OS}-${ARCH}.sh"
# URL to download from, if file not available locally
URL="https://repo.continuum.io/miniconda/Miniconda${PYTHON_VERSION}-${VERSION}-${OS}-${ARCH}.sh"
# Installation directory
TARGET="miniconda${PYTHON_VERSION}"
# We'll write down the installed version here,
# so that we can check if we need to update later.
VERSION_FILE="${TARGET}/.version"

function install_miniconda () {
    echo "Installing miniconda to ${TARGET}"
    rm -rf "${TARGET}"
    if [ -e "${LOCAL_ARCHIVE}" ]; then
        echo "Installing miniconda from SAN."
        bash -- "${LOCAL_ARCHIVE}" -b -p "${TARGET}"
    else
        echo "Installing miniconda from the web."
        echo "Please download an archive to ${LOCAL_DIR}"
        TMP_FILE=$(mktemp miniconda-installer.XXXXXXXXXX.sh)
        curl "${URL}" > "${TMP_FILE}"
        trap "rm -rf '${TMP_FILE}'" EXIT
        bash -- "${TMP_FILE}" -b -p "${TARGET}"
    fi
    # Save the current version
    echo "${VERSION_AND_ARCH}" > "${VERSION_FILE}"
}

if [ ! -e "${VERSION_FILE}" ]; then
    install_miniconda
else
    CURRENT=$(cat "${VERSION_FILE}")
    # Check if we need to change version...
    if [ ! "${CURRENT}" = "${VERSION_AND_ARCH}" ]
    then
        echo "Upgrading / downgrading miniconda to ${VERSION}"
        install_miniconda
    else
       echo "Miniconda ${VERSION} already installed."
    fi
fi
