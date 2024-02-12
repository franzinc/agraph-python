#!/bin/bash

set -e

# Nexus repository URL
export NEXUS_URL="https://san1.franz.com:8443/repository/pypi-group/simple"

# Certificate for the repository (RH hosts lack intermediate certs)
export NEXUS_CERT="../nexus.ca.crt"

# Pip options
export AG_PIP_OPTS="--use-wheel --index-url=${NEXUS_URL} --cert=${NEXUS_CERT} "

function absdir () {
   pushd $(dirname $1) > /dev/null
   local result=$(pwd)
   popd > /dev/null
   echo ${result}
}

STRESS_DIR=$(absdir $0)
ENV_DIR=${STRESS_DIR}/env
BASE_DIR=${STRESS_DIR}/..

PYTHON="${ENV_DIR}/bin/python"
PIP="${ENV_DIR}/bin/pip"

if [ ! -d "${ENV_DIR}" ]
then
    virtualenv --no-site-packages ${ENV_DIR}
    ${PIP} install -U setuptools pip wheel ${AG_PIP_OPTS}
    # Pip sometimes damages itself while updating, this should fix it:
    ${PYTHON} -m pip install -U --force-reinstall pip ${AG_PIP_OPTS}
fi

${PIP} install -r ${BASE_DIR}/requirements.txt ${AG_PIP_OPTS}
${PIP} install -r ${BASE_DIR}/requirements2.txt ${AG_PIP_OPTS}
${PIP} install -e ${BASE_DIR} ${AG_PIP_OPTS}
cd ${STRESS_DIR}/events
${PYTHON} ./events $@
