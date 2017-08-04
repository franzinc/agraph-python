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
Python versions 2.6+ and 3.3+ are supported. The installation method
described here uses the pip package manager. On some systems this
might require installing an additional package (e.g. ``python-pip`` on
RHEL/CentOS systems). All third-party libraries used by the Python
client will be downloaded automatically during installation.

See also the `Optional requirements`_ section below. 

Installation
------------
.. important:: It is highly recommended to perform the install in a
               `virtualenv`_ environment.

The client can be installed from PyPI_ using the ``pip`` package
manager::

   pip install agraph-python
               
Alternatively, a distribution archive can be obtained from
ftp://ftp.franz.com/pub/agraph/python-client/ and installed using `pip`::

    pip install agraph-python-<VERSION>.tar.gz

.. warning::

   Python 2.6 users should consider installing the simplejson package::

      pip install simplejson

   since the built-in JSON module in that version of Python offers
   unsatisfactory performance. The AllegroGraph Python client will
   detect and use simplejson automatically if it is installed.

Offline installation
~~~~~~~~~~~~~~~~~~~~
If it is not possible to access PyPI_ from the target machine, the
following steps should be taken:

   * In a compatible environment with unrestricted network
     access run::

        pip wheel agraph-python

   * If desired do the same for optional dependencies::

        pip wheel pycurl simplejson

   * This will generate a number of ``.whl`` files in the current
     directory. These files must be transferred to the target machine.

   * On the target machine use this command to install::

        pip install --no-index --find-links=<DIR> agraph-python

     where ``<DIR>`` is the directory containing the ``.whl`` files
     generated in the previous step.

Testing
-------
To validate the installation make sure that you have access to an
AllegroGraph server and run the following Python script::

    from franz.openrdf.connect import ag_connect
    with ag_connect('repo', host='HOST', port='PORT',
                    user='USER', password='PASS') as conn:
        print conn.size()

Substitute appropriate values for the HOST/PORT/USER/PASS
placeholders. If the script runs successfully a new repository named
`repo` will be created.

Proxy setup
----------- 
It is possible to configure the AllegroGraph Python client to use a
proxy for all its connection to the server. This can be achieved by
setting the ``AGRAPH_PROXY`` environment variable, as in the following
example::

    # Create a SOCKS proxy for tunneling to an internal network
    ssh -fN -D 1080 user@gateway.example.com
    # Configure agraph-python to use this proxy
    export AGRAPH_PROXY=socks://localhost:1080

The format of the ``AGRAPH_PROXY`` value is ``TYPE://HOST:PORT``,
where ``TYPE`` can be either ``http``, ``socks4``, ``socks5`` or
``socks`` (a synonym for ``socks5``). Note that if a SOCKS proxy is
used, DNS lookups will be performed by the proxy server.

Unit tests
~~~~~~~~~~
The Python client includes a suite of unit tests that can be run after
installation.  The tests are executed using the ``pytest`` framework
and also use a few utilities from ``nose``, so these two packages have
to be installed::

    pip install pytest nose

The tests require a running AllegroGraph server instance. The
configuration of this server is passed to the tests through
environment variables::

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


To run the tests, type::

    pytest --pyargs franz

Optional requirements
---------------------
The Python client can utilize a few additional third-party libraries
if these are available on the host system:

   * pycurl: can be used as the HTTP backend. It might offer
     better performance than ``requests`` (the default backend).
   * simplejson: recommended for Python 2.6 users to significantly
     improve performance. Has negligible impact on other Python
     versions.

These can be downloaded and installed from PyPI_::

   pip install pycurl
   pip install simplejson

Since the packages discussed here use native extensions, it is
necessary to have a proper development environment set up so that the
compilation may succeed. This environment must include:

   * A C compiler
   * Python development headers
   * libcurl development headers

Below we describe more detailed setup instructions for some of the
supported systems.

Windows
-------
The required packages are available in binary form for Windows, so it is not
necessary to install any compilers or headers.

RHEL 6/7 and compatible systems
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
On RHEL-like systems the requirements mentioned above can be satisfied by
following these steps (note that root privileges are required):

   * Enable EPEL_ repositories, since some of the required packages
     are only available there. The detailed instructions can be
     found `here <https://fedoraproject.org/wiki/EPEL#How_can_I_use_these_extra_packages.3F>`_.
     On CentOS systems, simply run::

         yum -y install epel-release

   * Install the required packages::

         yum -y install python-devel python-pip libcurl-devel gcc

   * Before installing the AllegroGraph Python client make sure that the
     following environment variable is set::

         export PYCURL_SSL_LIBRARY=nss

   * To use virtual environments (recommended) an additional package
     is needed::

        yum -y install python-virtualenv

Ubuntu
~~~~~~
The following packages are required to use the client with Python 2::

    apt-get install python-pip libcurl-gnutls libcurl4-gnutls-dev libgnutls28-dev

For Python 3 this becomes::

   apt-get install python3-pip libcurl-gnutls libcurl4-gnutls-dev libgnutls28-dev

.. note:: *Using different SSL backends.*

   Ubuntu offers three variants of curl, built using different SSL
   libraries. These variants differ in their licensing and SSL related
   capabilities (see https://curl.haxx.se/docs/ssl-compared.html for
   more details). The instructions above use the GnuTLS version. In
   most cases this is an acceptable choice, but it is possible to use
   a different SSL implementation by installing appropriate packages
   before installing the AllegroGraph Python client.

   To use the OpenSSL backend in curl::

       apt-get install libcurl4-openssl-dev libssl-dev

   For GnuTLS::

      apt-get install libcurl4-gnutls-dev libgnutls28-dev

   For NSS::

      apt-get install libcurl4-nss-dev libnss3-dev

   Note that if the client has already been installed it is necessary
   to reinstall the ``pycurl`` package in order to switch SSL
   backends::

      # Uninstall old package
      pip uninstall pycurl

      # Reinstall, ignoring PIP cache (to force recompilation)
      pip install --no-cache-dir pycurl

Arch Linux
~~~~~~~~~~
On Arch the following packages are needed to use the client with Python 2::

    pacman -S gcc python2 python2-pip libcurl

For Python 3 use::

    pacman -S gcc python python-pip libcurl
    
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
