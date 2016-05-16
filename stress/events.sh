#!/bin/bash

set -e

function absdir () {
   pushd $(dirname $1) > /dev/null
   local result=$(pwd)
   popd > /dev/null
   echo ${result}
}

STRESS_DIR=$(absdir $0)
ENV_DIR=${STRESS_DIR}/env
BASE_DIR=${STRESS_DIR}/..

if [ ! -d env ]
then
    virtualenv --no-site-packages ${ENV_DIR}
    source ${ENV_DIR}/bin/activate
    pip install -U setuptools pip wheel
else
    source  ${ENV_DIR}/bin/activate
fi
export PYCURL_SSL_LIBRARY=nss
pip install -r ${BASE_DIR}/requirements.txt
pip install -r ${BASE_DIR}/requirements2.txt
pip install -e ${BASE_DIR}
cd ${STRESS_DIR}/events
python ./events $@
