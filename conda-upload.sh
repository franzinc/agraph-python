#!/bin/sh

# Builds a package and uploads it to anaconda.org

set -e

# This obviously only works in the internal network.
CONDA2_DIR=/net/san1/disk1/conda2
CONDA2="${CONDA2_DIR}/bin/conda"
PYTHON2="${CONDA2_DIR}/bin/python"
CONDA3_DIR=/net/san1/disk1/conda3
CONDA3="${CONDA3_DIR}/bin/conda"
PYTHON3="${CONDA3_DIR}/bin/python"
ANACONDA="${CONDA3_DIR}/bin/anaconda"

# Anaconda channel to publish to.
# Can be overriden to allow publication to a personal channel.
CONDA_CHANNEL=${CONDA_CHANNEL:-franzinc}

# Check if we're already logged in, to avoid nasty warnings.
USER=$(${ANACONDA} whoami 2> /dev/null | head -n 1 | sed -n 's/\(Username: \(.*\)\)/\2/pi')

if [ -z "${USER}" ] ; then
    echo "Need login credentials for anaconda.org:"
    # Cannot do 2> /dev/null as it hides the password prompt.
    ${ANACONDA} login
    # If we've logged in just for this script, we should log out at exit
    trap "${ANACONDA} logout 2> /dev/null" EXIT
else
    echo "Already logged in to anaconda.org as: ${USER}"
fi

# Create a fresh temp directory
BUILD_DIR=$(mktemp -d)

# And delete it on exit
trap 'rm -rf "${BUILD_DIR}"' EXIT

# Override the build directory used by conda
export CONDA_BLD_PATH="${BUILD_DIR}/conda-bld"

# Use our custom conda settings.
# This mainly ensures that our channel is used when looking for dependencies.
export CONDARC="$(pwd)/condarc"

# Build the package from the local folder
${PYTHON2} setup.py bdist_conda
${PYTHON3} setup.py bdist_conda

# Compute the output paths
PACKAGE2=$(find "${BUILD_DIR}/conda-bld" -name 'agraph-python-*-py2*.tar.bz2')
PACKAGE3=$(find "${BUILD_DIR}/conda-bld" -name 'agraph-python-*-py3*.tar.bz2')

# Generate versions for all archs
mkdir "${BUILD_DIR}/dist"
cd "${BUILD_DIR}/dist"
${CONDA2} convert -f --platform all "${PACKAGE2}"
${CONDA3} convert -f --platform all "${PACKAGE3}"

# Upload to anaconda.org
${ANACONDA} upload -c "${CONDA_CHANNEL}" --user "${CONDA_CHANNEL}" $(find . -name 'agraph-python-*.tar.bz2')
