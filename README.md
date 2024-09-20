# AllegroGraph Python API

![PyPI - Python Version](https://img.shields.io/pypi/pyversions/agraph-python?logo=python&logoColor=gold)
[![PyPI package](https://img.shields.io/pypi/v/agraph-python.svg?logo=python&logoColor=gold)](https://pypi.python.org/pypi/agraph-python)
[![Anaconda package](https://img.shields.io/conda/v/franzinc/agraph-python.svg?logo=anaconda&logoColor=gold)](https://anaconda.org/franzinc/agraph-python)

The AllegroGraph Python API offers convenient and efficient access to an
AllegroGraph server from a Python-based application. This API provides methods
for creating, querying and maintaining RDF data, and for managing the stored
triples. The AllegroGraph Python API deliberately emulates the Eclipse RDF4J
(formerly Aduna Sesame) API to make it easier to migrate from RDF4J to
AllegroGraph. The AllegroGraph Python API has also been extended in ways that
make it easier and more intuitive than the RDF4J API.

## Requirements

Python versions `>=3.8,<=3.12` are supported. The installation method described
here uses the pip package manager. On some systems this might require installing
an additional package (e.g. `python-pip` on RHEL/CentOS systems). All
third-party libraries used by the Python client will be downloaded automatically
during installation.

## Installation

**Important**: It is highly recommended to perform the install in a
[virtualenv](https://virtualenv.pypa.io/) environment.

The client can be installed from [PyPI](https://pypi.python.org/) using the
`pip` package manager:

``` sh
pip install agraph-python
```

Alternatively, a distribution archive can be obtained from
ftp://ftp.franz.com/pub/agraph/python-client and installed using `pip`:

``` sh
pip install agraph-python-<VERSION>.tar.gz
```

### Offline installation

If it is not possible to access [PyPI](https://pypi.python.org/) from the target
machine, the following steps should be taken:

> -   In a compatible environment with unrestricted network access run:
>
>         pip wheel agraph-python
>
> -   This will generate a number of `.whl` files in the current
>     directory. These files must be transferred to the target machine.
>
> -   On the target machine use this command to install:
>
>         pip install --no-index --find-links=<DIR> agraph-python
>
>     where `<DIR>` is the directory containing the `.whl` files
>     generated in the previous step.

### Install by `conda`

Using `conda` to install `agraph-python` is also supported:

``` sh
conda create -n myenv python=3.10
conda activate myenv
conda install -y -c conda-forge -c franzinc agraph-python
```

## Testing

To validate the installation make sure that you have access to an AllegroGraph
server and run the following Python script:

``` python
from franz.openrdf.connect import ag_connect
    with ag_connect('repo', host='HOST', port='PORT',
                    user='USER', password='PASS') as conn:
    print (conn.size())
```
    
Substitute appropriate values for the HOST/PORT/USER/PASS placeholders. If the
script runs successfully a new repository named `repo` will be created.

## Proxy setup

It is possible to configure the AllegroGraph Python client to use a proxy for
all its connection to the server. This can be achieved by setting the
`AGRAPH_PROXY` environment variable, as in the following example:

``` sh
# Create a SOCKS proxy for tunneling to an internal network
ssh -fN -D 1080 user@gateway.example.com
# Configure agraph-python to use this proxy
export AGRAPH_PROXY=socks://localhost:1080
```

The format of the `AGRAPH_PROXY` value is `TYPE://HOST:PORT`, where `TYPE` can
be either `http`, `socks4`, `socks5` or `socks` (a synonym for `socks5`). Note
that if a SOCKS proxy is used, DNS lookups will be performed by the proxy
server.

### Unit tests

The Python client includes a suite of unit tests that can be run after
installation. The tests are executed using the `pytest` framework and also use a
few utilities from `nose`, so these two packages have to be installed. We also
need the `pytest-mock` plugin:

``` sh
pip install -e ".[test]"
```

The tests require a running AllegroGraph server instance. The configuration of
this server is passed to the tests through environment variables:

``` sh
# Host and port where the server can be reached. These values are the
# default, it is only necessary to define the variables below if your
# setup is different
export AGRAPH_HOST=localhost
export AGRAPH_PORT=10035

# Tests will create repositories in this catalog.
# It must exist on the server. Use "/" for the root catalog.
export AGRAPH_CATALOG=tests

# Login credentials for the AG server.
# The user must have superuser privileges.
export AGRAPH_USER=test

# Use a prompt to read the password
read -s -r -p "Password for user ${AGRAPH_USER}: " AGRAPH_PASSWORD
export AGRAPH_PASSWORD
```

To run the tests, type:

``` sh
pytest --pyargs franz.openrdf.tests.tests --pyargs franz.openrdf.tests.newtests
```
