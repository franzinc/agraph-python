AllegroGraph Python API
=======================
The AllegroGraph Python API offers convenient and efficient access to
an AllegroGraph server from a Python-based application. This API
provides methods for creating, querying and maintaining RDF data, and
for managing the stored triples. The AllegroGraph Python API
deliberately emulates the Aduna Sesame API to make it easier to
migrate from Sesame to AllegroGraph. The AllegroGraph Python API has
also been extended in ways that make it easier and more intuitive than
the Sesame API.

Requirements
------------
Python versions 2.6+ and 3.3+ are supported.

The Python client depends on a few third-party Python modules. These will
be downloaded from PyPI_ during installation and compiled on the target
systems. Since some of the required packages use native extensions, it is
necessary to have a proper development environment set up so that the
compilation may succeed. This environment must include:

   * A C compiler
   * Python development headers
   * libcurl development headers

Below we describe more detailed setup instructions for RHEL-compatible
systems.

RHEL 6/7 and compatible systems
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
On RHEL-like systems the requirements mentioned above can be satisfied by
following these steps (note that root privileges are required):

   * Enable EPEL_ repositories, since some of the required packages
     are only available there. The detailed instructions can be
     found `here <https://fedoraproject.org/wiki/EPEL#How_can_I_use_these_extra_packages.3F>`_.
     On CentOS systems, simply run::

         yum -y install epel-release

   * Install the required packages:

         yum -y install python-devel python-pip libcurl-devel gcc

   * Before installing the client make sure that the following environment
     variable is set::

         export PYCURL_SSL_LIBRARY=nss

   * To use virtual environments (recommended) an additional package is needed::

        yum -y install python-virtualenv

Ubuntu
~~~~~~
Ubuntu offers three variants of curl, built using different SSL libraries. These variants differ
in their licensing and SSL related capabilities (see https://curl.haxx.se/docs/ssl-compared.html
for more details). The list of packages that need to be installed depends on the chosen SSL
implementation.

The following package is required in all cases to use Python 2::

    # This will bring in Python, dev headers and GCC as dependencies
    apt-get install python-pip

For Python 3 this becomes::

    apt-get install python3-pip

To use the OpenSSL backend in curl::

    apt-get install libcurl4-openssl-dev libssl-dev

For GnuTLS::

    apt-get install libcurl4-gnutls-dev libgnutls28-dev

For NSS::

    apt-get install libcurl4-nss-dev libnss3-dev

Arch Linux
~~~~~~~~~~
On Arch the following packages are needed to use the client with Python 2::

    pacman -S gcc python2 python2-pip libcurl

For Python 3 use::

    pacman -S gcc python python-pip libcurl

Installation
------------
A distribution archive can be obtained from http://franz.com/agraph/downloads/clients
and installed using `pip`::

    pip install agraph-<VERSION>-python-client.tar.gz

It is highly recommended to perform the installation in a `virtualenv`_ environment.

Testing
-------
To validate the installation make sure that you have access to an AllegroGraph server
and run the following Python script::

    from franz.openrdf.connect import ag_connect
    with ag_connect('repo', host='HOST', port='PORT',
                    user='USER', password='PASS') as conn:
        print conn.size()

Substitute appropriate values for the HOST/PORT/USER/PASS placeholders. If the script
runs successfully a new repository named `repo` will be created.

Troubleshooting
---------------
If you see an error similar to the following::

    ImportError: pycurl: libcurl link-time ssl backend (nss) is
    different from compile-time ssl backend (none/other)

Perform this procedure (replacing `<VERSION>` with the actual version)::

    # Uninstall pycurl:
    pip uninstall pycurl

    # Set the required compile-time option for pycurl
    export PYCURL_SSL_LIBRARY=nss

    # Reinstall, but ignore cached packages (force recompile)
    pip install --no-cache-dir agraph-<VERSION>-python-client.tar.gz

.. _PyPI: https://pypi.python.org/
.. _EPEL: https://fedoraproject.org/wiki/EPEL
.. _virtualenv: https://virtualenv.pypa.io/
