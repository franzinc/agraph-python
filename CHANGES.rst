==========================================
AllegroGraph Python client release history
==========================================

Release 102.1.2
================

Update required version of requests to 2.25.0
-------------------------

Change required version of `requests` from 2.23.0 to 2.25.0
This fixes an issue in agraph-python when used on Python 3.10 or 3.11

Release 102.1.1
================

Add TriplesBuffer utility
-------------------------

Add `TriplesBuffer`, a utility class that helps users efficiently add triples in
batches.

Release 102.1.0
================

Support rdf-star
-------------------------

Add support for rdf-star.  Requires Allegrograph 8.0.0 or newer.


Release 102.0.0
================

Drop support for Python 2
-------------------------

Python 2 was created 23 years ago and sunsetted 3 years ago.
Python 3 was created 15 years ago and is still current.

The Python 2 compatible code will still exist in the git repo on github
and can be checked out and built.

Release 101.0.11
================

use getargspec only for Python 2.7
----------------------------------

Import "getargspec" and alias it as "getfullargspec" only if it's Python 2.x;
if it's Python 3.x, we import "getfullargspec" from "inspect" package
directly.

Release 101.0.10
================

rfe16865: support graphql queries in python
-------------------------------------------

New method ``evalGraphqlQuery`` has been added to ``RepositoryConnection``
class. It is a thin wrapper over the HTTP API:
https://franz.com/agraph/support/documentation/current/http-reference.html#backend-post-catalogs-repositories-graphql



Release 101.0.9
===============

Make loading the module six an installation requirement
--------------------------------------------------------


Release 101.0.8
===============

Allow sparql queries inside a Jupyter notebook
--------------------------------------------------------

Also
Allow "root" to denote the root catalog
Update some module versions
Fix the specification of a protocol when creating a
connection.



Release 101.0.7
===============

rfe16555: Add support for handling 204 responses from AG
--------------------------------------------------------

The AllegroGraph server, starting in vesion 7.1.0, may return
a 204 (no content) response when it knows that it will not
be returning any data.  The client will
then create a null cursor, rather than creating an http
cursor which when used immediately returns 'no data left'



Release 101.0.6
===============

rfe16523: query options API
---------------------------

``RepositoryConnection`` class now provides a set of methods for
managing per-user/per-repository SPARQL query options. The methods
utilize a server-side API that is available in AllegroGraph v7.1.0 and
later and are similar to the namespace management methods:

  - ``getQueryOptions()``
  - ``getQueryOption()``
  - ``setQueryOption()``
  - ``removeQueryOption()``
  - ``clearQueryOptions()``


Release 101.0.5
===============

Fix bug in URI's split function
-------------------------------

split() was splitting on the wrong character.
Fix submitted via github.


Release 101.0.4
===============

Dependency updates
------------------

===========  =====================  ==============
Package      Previous version       New version
===========  =====================  ==============
``urllib3``  ``1.24.2``               ``1.25.8``
===========  =====================  ==============


Release 101.0.3
===============

rfe16393: Let SPARQL SELECT queries return metadata
---------------------------------------------------

Extend `TupleQueryResult` with a field named `metadata` that contains
the query execution properties, like time spent and memory used:

   >>> from franz.openrdf.query.query import QueryLanguage
   >>> query = conn.prepareTupleQuery(QueryLanguage.SPARQL, "SELECT * { ?s ?p ?o }")
   >>> result = query.evaluate()
   >>> queryDuration = result.metadata['time']['total']

For now this is only supported on `SELECT` queries.

Security-related dependency updates
-----------------------------------

===========  =====================  ==============
Package      Previous version       New version
===========  =====================  ==============
``numpy``    ``1.15.1``/``1.15.4``  ``1.16.0``
``urllib3``  ``1.23``               ``1.24.2``
===========  =====================  ==============

Multiple-namespace URI factory object
-------------------------------------

Added new method ``ValueFactory.namespaces()`` that takes a dictionary of the
form `{<prefix>: <namespace>, ...}` and returns a factory object that allows to
create URIs from the strings of the form `<prefix>:<name>`, dispatching the
prefix to corresponding namespace:

   >>> prefixes = {'': 'http://this.org#', 'ex': 'http://example.org#'}
   >>> nss = conn.getValueFactory().namespaces(prefixes)
   >>> nss[':resource']
   <http://this.org#resource>
   >>> nss('ex:resource')
   <http://example.org#resource>


Release 101.0.2
===============

Fix interoperability with Python 3.5.3
--------------------------------------

The error "TypeError: 'ContextAttribute' object is not callable" would
occur when using the AllegroGraph Python client with Python 3.5.3.
This has been fixed.

Raise error when attemping to federate certain sessions
-------------------------------------------------------

A session started by AllegroGraphServer.openSession() cannot
be federated because this type of session may not be for a
single repository and may in fact already be a session over
a federation of repositories.  To federate repositories
pass a set of Repository.getConnection() objects.
With this change an exception will be raised when attempting to
federate a RepositoryConnection created by AllegroGraphServer.openSession().


Release 101.0.1
===============

Non-RDF document import
-----------------------

JSON and CSV documents can be imported into AllegroGraph using
the new 'transform' service introduced in AG 6.5.0. This can
now be accessed from the Python client by calling the
``addDocumentFile()`` method of the connection object.
Document data from strings or Python dictionaries can be added
with ``addDocumentData()``.

Update dependencies which have security vulnerabilities
-------------------------------------------------------

Bumped urllib3 from 1.22 to 1.23 and requests from 2.18.4 to 2.20.0
according to recommendations made by Github.

Ensure sessions are closed properly
-----------------------------------

Sessions created via the ``openFederation()`` or ``openSession()``
methods of franz.openrdf.sail.allegrographserver.AllegroGraphServer
were not closed when ``close()`` was called on the resulting
connection.  This has been fixed.

Release 101.0.0
===============

JSON-LD support
---------------

A new RDF format (`RDFFormat.JSONLD`) has been added. When importing
documents from strings or files it is now possible to specify
a few JSON-LD specific arguments:

   - `json_ld_context`: a JSON object defining the mapping between
     JSON-LD terms and triples.
   - `json_ld_store_source`: a flag that can be used to persist
     the whole input document in a single triple.
   - `allow_external_references` a flag that must be explicitly
     set to True to allow the JSON-LD importer to retrieve
     external resources referenced in the input document or in
     the `json_ld_context` argument.
   - `external_reference_timeout`: HTTP timeout that will be
     used by the server when retrieving external resources.

In addition the `addData()` method can import a JSON-LD document
in the form of a dictionary.

URI object changes
------------------

URI objects are now canonicalized by default. This means that
two equal URIs are guaranteed to be the same object. This behavior
can be disabled by passing `canonical=False` when creating a URI.

URI objects no longer carry an attribute dictionary. This means
it is no longer possible to add arbitrary fields to an URI object.

rfe15690: warmup support
------------------------

Add `RepositoryConnection.warmup()` to allow the user to
warm up the string table and/or indices.

User data access
----------------

AllegroGraph allows each user to store arbitrary key-value data
on the server. This storage can now be accessed from Python by
using new ``AllegroGraphServer`` methods:

  - ``listUserData()``
  - ``getUserData()``
  - ``setUserData()``
  - ``deleteUserData()``

Release 100.2.0
===============

Pandas support
--------------

It is now possible to turn a query result into a Pandas DataFrame
by calling the ``toPandas()`` method of the result object. Note that
Pandas must be installed separately for this to work.

Release 100.1.2
===============

bug25281: Proxy settings are ignored
------------------------------------
Proxy settings used to be ignored when the requests backend was used.
This has been corrected.

Thanks to Iván Darío Ramos Vacca for reporting the bug and providing
a fix.

Release 100.1.1
===============

A bugfix release that adds some missing dependencies that are needed
when using Python < 3.5.

Release 100.1.0
===============

Triple attributes
-----------------

Added support for triple attributes (requires AG >= 6.1). Specifically
it is now possible to:

   - Set and retrieve the static attribute filter using
     ``conn.setAttributeFilter()`` and ``conn.getAttributeFilter()``
   - Set and retrieve user attributes (that will be sent with each
     request) using ``conn.setUserAttributes()`` and
     ``conn.getUserAttributes()``.
   - Manage attribute definitions using various methods in the
     connection class..
   - Add triples with attributes - a new keyword parameter named
     'attributes' has been added to methods that add triples,
     such as ``addData()``. It is also possible to pass five-element
     tuples to ``addTriples()``, where the fifth element is
     a dictionary of attribute values.

Distributed transaction settings
--------------------------------

It is now possible to configure distributed transaction parameters
in multiple ways:

   - By passing arguments to the ``commit()`` method
   - By calling ``setTransactionSettings()`` on the connection object.
   - By using a context manager returned by the
     ``temporaryTransactionSettings()`` method.

In all cases the settings can be passed either in a single
``TransactionSettings`` object or as individual keyword arguments.

Enhanced namespace objects
--------------------------

Namespace objects can now create URIs when indexed or called like a
function. This makes it easier to create URIs where the local name is
not a valid attribute name:

   >>> from franz.openrdf.connect import ag_connect
   >>> conn = ag_connect('repo')
   >>> ex = conn.namespace('http://franz.com/example/')
   >>> ex('is')
   <http://franz.com/example/is>
   >>> ex['def']
   <http://franz.com/example/def>

Release 100.0.4
===============

Jupyter-friendly stdout
-----------------------

The ``output_to`` context manager (used internally when writing output
to stdout) has been modified to work better in environments that
hijack the ``sys.stdout`` value, such as Jupyter notebooks or IDLE.

Release 100.0.3
===============

Resolved issues with running unit tests from a wheel
----------------------------------------------------

Some unit tests used to fail when the module was installed
from a binary wheel. This has been corrected.

bug25081: The 'context' argument to addTriples() is broken
----------------------------------------------------------

Using the ``addTriples()`` method with the ``context`` parameter
set to a non-default value used to produce errors::

   >>> conn.addTriples([(s, p, o)], context=g)
   400 MALFORMED DATA: Invalid graph name: (<ex://g>)

This has been corrected. Context can now be set to a single URI
or a list of URIs. Both URI objects and strings are supported.

bug25079: Statement objects not created from strings are broken
---------------------------------------------------------------

Statement objects that were created in user code were not fully
functional. In particular attempts to convert such statements to
strings or to pass them to addTriples() would fail.

This has been corrected.

Namespace objects
-----------------

Namespace objects can be used to create URIs, as in the following
example:

   >>> from franz.openrdf.connect import ag_connect
   >>> conn = ag_connect('repo')
   >>> ex = conn.namespace('http://franz.com/example/')
   >>> ex.foo
   <http://franz.com/example/foo>

Release 100.0.2
===============

New query methods
-----------------

Four new methods have been added to the RepositoryConnection class:

   - executeTupleQuery()
   - executeGraphQuery()
   - executeBooleanQuery()
   - executeUpdate()

These can be used to prepare and evaluate a SPARQL query in a single
call.

New tutorial
------------

The tutorial has been updated and restyled using Sphinx.

Finalizers for query results
----------------------------

All result objects are now closed automatically when garbage collected.
This makes it possible to write simple loops like the one below::

   for stmt in conn.executeTupleQuery('...'):
       ...

without having to use the ``with`` statement, since reference counting
will ensure that the query result is closed at the right time. Note that
this should not be relied upon in more complex scenarios, where circular
references might occur and prevent the result object from being closed.

Connection parameters can now be passed in environment variables
-----------------------------------------------------------------

The following environment variables are now used when connecting
to the server:

   - ``AGRAPH_HOST`` - server address, the default is '127.0.0.1'
   - ``AGRAPH_PORT`` - port number (default: 10035 for HTTP connections,
                       10036 for HTTPS).
   - ``AGRAPH_USER`` - Username, no default.
   - ``AGRAPH_PASSWORD`` - Password, no default.

Note that parameters passed to ``ag_connect()`` or  ``AllegroGraphServer()``
will override these variables.

Various fixes related to data export
------------------------------------

Specifically the following adjustments have been done:

   - Changed the default RDF export format to N-Quads.
   - Fixed a bug where errors returned during export
     caused an encoding error.
   - Provided a default format (CSV) for tuple queries.
   - Value of the output parameter can now be True (stdout)
     or a file descriptor.

Release 100.0.1
===============

bug24892: Time parsing fixes
----------------------------

The Python client used to fail when trying to retrieve a
datetimeValue() of a literal that contained time zone
information. This has been corrected.

All datetime objects created by the Python API are now timezone-aware.

rfe15005: duplicate suppression control API
-------------------------------------------

It is now possible to set and query the duplicate suppression policy of
a repository from Python, using three new methods of the connection
object:

   - getDuplicateSuppressionPolicy()
   - setDuplicateSuppressionPolicy()
   - disableDuplicateSuppression()

New export methods
------------------

A new mechanism for exporting data has been added. It utilizes a new
``output`` parameter that has been added to the following methods:

   - RepositoryConnection.getStatements()
   - RepositoryConnection.getStatementsById()
   - TupleQuery.evaluate()
   - GraphQuery.evaluate()

Setting the new parameter to a file name or a file-like object
will cause the data that would normally be returned by the call
to be saved to the specified file instead. Serialization format
can be controlled by setting another new parameter,
``output_format``.

Release 100.0.0
===============

New versioning scheme
---------------------

Client versions no longer match the server version. Major version
number has been bumped to 100 to avoid confusion.

bug24819: Circular import
-------------------------

Importing com.franz.openrdf.query.query failed due to a circular
import. Thanks to Maximilien de Bayser for reporting this.

bug24826: removeStatement uses context instead of object
--------------------------------------------------------

The removeStatement method of RepositoryConnection was broken.
Patch by Maximilien de Bayser.

Release 6.2.2.0.4
=================

bug24728: Incorrect conversion between boolean literals and Python values
-------------------------------------------------------------------------

The booleanValue() method of the Literal class used to work
incorrectly.  It would return True for any literal that is not empty,
including the "false"^^xsd:boolean literal.  This has been corrected -
the function will now return expected values for literals of type
xsd:boolean.  Result for other types remains undefined.

Release 6.2.2.0.1
=================

bug24680: to_native_string is broken on Python 2
------------------------------------------------

The Python client sometimes failed while processing values with
non-ascii characters, showing the following error message:

UnicodeEncodeError: 'ascii' codec can't encode characters in position ??: ordinal not in range(128)

This has been corrected.

Release 6.2.2.0.0
=================

Released with AllegroGraph 6.2.2. Change log for this and all previous
Python client releases can be found in AllegroGraph release notes:
https://franz.com/agraph/support/documentation/current/release-notes.html
