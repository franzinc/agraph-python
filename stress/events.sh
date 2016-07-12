#!/bin/bash

set -e

# Prebuilt wheels location
WHEELHOUSE ?= file:///net/san1/disk1/wheelhouse/

# Use prebuilt packages ONLY by default
export AG_PIP_OPTS = --use-wheel --find-links=$(WHEELHOUSE) --no-index

function absdir () {
   pushd $(dirname $1) > /dev/null
   local result=$(pwd)
   popd > /dev/null
   echo ${result}
}

STRESS_DIR=$(absdir $0)
ENV_DIR=${STRESS_DIR}/env
BASE_DIR=${STRESS_DIR}/..

if [ ! -d "${ENV_DIR}" ]
then
    virtualenv --no-site-packages ${ENV_DIR}
    source ${ENV_DIR}/bin/activate
    pip install -U setuptools pip wheel ${AG_PIP_OPTS}
else
    source  ${ENV_DIR}/bin/activate
fi
export PYCURL_SSL_LIBRARY=nss
pip install -r ${BASE_DIR}/requirements.txt ${AG_PIP_OPTS}
pip install -r ${BASE_DIR}/requirements2.txt ${AG_PIP_OPTS}
pip install -e ${BASE_DIR} ${AG_PIP_OPTS}
cd ${STRESS_DIR}/events
python ./events $@
