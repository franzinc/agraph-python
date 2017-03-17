#!/bin/sh

# Installs miniconda X (where X is either 2 or 3) into './minicondaX'.
# Tries to use the package stored in /net/san1/disk1. If that is not
# available downloads the installer from the internet.
# Tools required for building and uploading packages will be
# installed. These tools currently have to be downloaded from the net.

set -e

function info() {
    echo $@ >&2
}

# A single, required argument: Python version (2 or 3).
if [ ! $# == 1 ]; then
  info "Usage: $0 <2|3>"
  exit 1
fi

# Python version - either 2 or 3
PYTHON_MAJOR_VERSION=$1

# The settings below might need to depend on PYTHON_MAJOR_VERSION...

# Package version to download. Can also be 'latest'.
VERSION=4.3.11

# Architecture: x86_64 or x86.
ARCH=x86_64

# Installer filename as found on https://repo.continuum.io
# We'll use the same name for files stored on SAN1.
INSTALLER_FILE="Miniconda${PYTHON_MAJOR_VERSION}-${VERSION}-Linux-${ARCH}.sh"

# Path to the installer file cached on SAN1
SAN1_PATH="/net/san1/disk1/conda/${INSTALLER_FILE}"

# URL to download the installer from.
INSTALLER_URL="https://repo.continuum.io/miniconda/${INSTALLER_FILE}"

# Local path to the downloaded installer file
LOCAL_PATH="./${INSTALLER_FILE}"

# Target installation path
TARGET="./miniconda${PYTHON_MAJOR_VERSION}"

# Conda packages to be installed
PACKAGES="conda-build anaconda-client"

# First check if we really need to do anything.
if [ -d "${TARGET}" ]; then
    info "${TARGET} already exists."
    exit
fi

# Now try to locate the installer.

if [ -f "${LOCAL_PATH}" ]; then
    info "Using already downloaded installer: ${LOCAL_PATH}"
    INSTALLER=${LOCAL_PATH}
elif [ -f "${SAN1_PATH}" ]; then
    info "Using installer from SAN: ${SAN1_PATH}"
    INSTALLER=${SAN1_PATH}
else
    info "Downloading installer: ${INSTALLER_URL} -> ${LOCAL_PATH}"
    curl -o "${LOCAL_PATH}" "${INSTALLER_URL}"
    INSTALLER=${LOCAL_PATH}
fi

# Run the installer. Options:
#  -b: Batch mode, no prompts, no bashrc changes
#  -f: Overwrite target directory if it exists
#  -p: Target path
bash ${INSTALLER} -bf -p "${TARGET}"

info "Installing required packages"
# Install required tools (-y: don't ask for confirmation).
"${TARGET}/bin/conda" install -y ${PACKAGES}
