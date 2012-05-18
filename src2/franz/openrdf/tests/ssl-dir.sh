#!/bin/bash

dir=$( cd "$( dirname "$0" )" && pwd )

cacert="${dir}/ca.cert"
# Point this at the client certificate
testcert="${dir}/test.cert"

echo "${dir}"
db_dir="${dir}/certs.db"
p12=/tmp/$$.p12

mkdir -p $db_dir

certutil -d $db_dir "$@"-A -n FranzTestCA -i $cacert -t TC,,

openssl pkcs12 -export -in $testcert -name "Test Client" -passout pass: \
   -out $p12
pk12util -d $db_dir -i $p12 -K "" -W ""
rm $p12
