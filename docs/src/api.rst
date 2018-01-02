AllegroGraph Python API Reference
=================================

This is a description of the Python Application Programmer's Interface
(API) to Franz Inc.'s `AllegroGraph`_.

The Python API offers convenient and efficient access to an
AllegroGraph server from a Python-based application. This API provides
methods for creating, querying and maintaining RDF data, and for
managing the stored triples.

.. note::

   The Python API deliberately emulates the Eclipse RDF4J API
   (formerly Aduna Sesame) to make it easier to migrate from RDF4J to
   AllegroGraph.  The Python API has also been extended in ways that
   make it easier and more intuitive than the RDF4J API.
   
.. toctree::
   :hidden:
   
   _gen/modules
   
.. currentmodule:: franz.openrdf.sail.allegrographserver

.. the "raw" anchors below are needed because :noindex: causes invalid
   links to be generated.
                   
AllegroGraphServer class
------------------------
.. raw:: html

   <a id="franz.openrdf.sail.allegrographserver.AllegroGraphServer"></a>

.. autoclass:: AllegroGraphServer
   :noindex:

   Example:

   .. code:: python

      server = AllegroGraphServer(host="localhost", port=8080,
                                  user="test", password="pw") 
   .. extautosummary::
      :nosignatures:
      
      methods:AllegroGraphServer

Catalog class
-------------
.. raw:: html

   <a id="franz.openrdf.sail.allegrographserver.Catalog"></a>
 
.. autoclass:: Catalog
   :noindex:
     
   Construct catalogs using the server object:

   .. code:: python

      catalog = server.openCatalog('scratch')
      
   .. extautosummary::
      :nosignatures:
      
      methods_without_init:Catalog

Spec module
-----------
      
.. automodule:: franz.openrdf.sail.spec
   
   .. extautosummary::
      :nosignatures:
      
      funcs:franz.openrdf.sail.spec
      
Repository class
----------------
.. raw:: html

   <a id="franz.openrdf.repository.repository.Repository"></a>

.. currentmodule:: franz.openrdf.repository.repository

.. autoclass:: Repository
   :noindex:
               
   Construct instances using :meth:`~franz.openrdf.sail.allegrographserver.Catalog.getRepository`.

   .. code :: python
   
      with catalog.getRepository("agraph_test", Repository.ACCESS) as repo:
          ...

   .. extautosummary::
      :nosignatures:
      
      methods:Repository
          
Utility connection functions
----------------------------

.. currentmodule:: franz.openrdf.connect

Manually constructing server, catalog and repository objects is often
tedious when only a simple connection to a single repository is needed.
In that case the connection may be created directly using
:func:`ag_connect`.
   
.. extautosummary::
   :nosignatures:
      
   ag_connect

RepositoryConnection class
--------------------------
.. raw:: html

   <a id="franz.openrdf.repository.repositoryconnection.RepositoryConnection"></a>

.. currentmodule:: franz.openrdf.repository.repositoryconnection

.. autoclass:: RepositoryConnection
   :noindex:

   |RepositoryConnection| objects should be constructed using
   :meth:`~franz.openrdf.repository.repository.Repository.getConnection`.
   To ensure that repository connections are closed, the best practice
   is to use a ``with`` statement:

   .. code:: python

      with repository.getConnection() as conn:
          ...

   .. extautosummary::
      :nosignatures:   
   
      ~RepositoryConnection.__init__ 
          
General connection methods
~~~~~~~~~~~~~~~~~~~~~~~~~~

This section contains the |RepositoryConnection| methods that create,
maintain, search, and delete triple stores.

.. extautosummary::
   :nosignatures: 

   ~RepositoryConnection.add
   ~RepositoryConnection.addData
   ~RepositoryConnection.addFile
   ~RepositoryConnection.addStatement
   ~RepositoryConnection.addTriple
   ~RepositoryConnection.addTriples
   ~RepositoryConnection.clear
   ~RepositoryConnection.clearNamespaces
   ~RepositoryConnection.close
   ~RepositoryConnection.createBNode
   ~RepositoryConnection.createLiteral
   ~RepositoryConnection.createRange
   ~RepositoryConnection.createStatement
   ~RepositoryConnection.createURI
   ~RepositoryConnection.deleteDuplicates
   ~RepositoryConnection.executeBooleanQuery
   ~RepositoryConnection.executeGraphQuery
   ~RepositoryConnection.executeTupleQuery
   ~RepositoryConnection.executeUpdate
   ~RepositoryConnection.export
   ~RepositoryConnection.exportStatements
   ~RepositoryConnection.getAddCommitSize
   ~RepositoryConnection.getContextIDs
   ~RepositoryConnection.getDuplicateStatements
   ~RepositoryConnection.getNamespace
   ~RepositoryConnection.getNamespaces
   ~RepositoryConnection.getSpec
   ~RepositoryConnection.getStatements
   ~RepositoryConnection.getStatementsById
   ~RepositoryConnection.getValueFactory
   ~RepositoryConnection.isEmpty
   ~RepositoryConnection.namespace
   ~RepositoryConnection.prepareBooleanQuery
   ~RepositoryConnection.prepareGraphQuery
   ~RepositoryConnection.prepareTupleQuery
   ~RepositoryConnection.prepareUpdate
   ~RepositoryConnection.registerDatatypeMapping
   ~RepositoryConnection.remove
   ~RepositoryConnection.removeNamespace
   ~RepositoryConnection.removeQuads
   ~RepositoryConnection.removeQuadsByID
   ~RepositoryConnection.removeStatement
   ~RepositoryConnection.removeTriples
   ~RepositoryConnection.setAddCommitSize
   ~RepositoryConnection.setNamespace
   ~RepositoryConnection.size

   ~RepositoryConnection.add_commit_size

Triple Index Methods
~~~~~~~~~~~~~~~~~~~~

These |RepositoryConnection| methods support user-defined triple indices.
See `AllegroGraph Triple Indices <../triple-index.html>`__ for more
information on this topic.

.. extautosummary::
   :nosignatures: 

   ~RepositoryConnection.listIndices
   ~RepositoryConnection.listValidIndices
   ~RepositoryConnection.addIndex
   ~RepositoryConnection.dropIndex
   ~RepositoryConnection.optimizeIndices

Free Text Indexing Methods
~~~~~~~~~~~~~~~~~~~~~~~~~~

The following |RepositoryConnection| methods support free-text indexing in
AllegroGraph.

.. extautosummary::
   :nosignatures: 

   ~RepositoryConnection.createFreeTextIndex
   ~RepositoryConnection.deleteFreeTextIndex
   ~RepositoryConnection.evalFreeTextSearch
   ~RepositoryConnection.getFreeTextIndexConfiguration
   ~RepositoryConnection.listFreeTextIndices
   ~RepositoryConnection.modifyFreeTextIndex

Note that text search is implemented through a SPARQL query using a
"magic" predicate called ``fti:search``. See the :doc:`AllegroGraph
Python API Tutorial <tutorial>` for an example of how to set up this
search.

Prolog Rule Inference Methods
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

These |RepositoryConnection| methods support the use of Prolog rules in
AllegroGraph. Any use of Prolog rules requires that you create a
:ref:`transaction <transactions>` to run them in.

.. extautosummary::
   :nosignatures: 

   ~RepositoryConnection.addRules
   ~RepositoryConnection.loadRules

Geospatial Reasoning Methods
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

These |RepositoryConnection| methods support geospatial reasoning.

.. extautosummary::
   :nosignatures: 

   ~RepositoryConnection.createBox
   ~RepositoryConnection.createCircle
   ~RepositoryConnection.createCoordinate
   ~RepositoryConnection.createLatLongSystem
   ~RepositoryConnection.createPolygon
   ~RepositoryConnection.createRectangularSystem
   ~RepositoryConnection.getGeoType 
   ~RepositoryConnection.setGeoType

Social Network Analysis Methods
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following |RepositoryConnection| methods support Social Network
Analysis in AllegroGraph. The Python API to the Social Network Analysis
methods of AllegroGraph requires Prolog queries, and therefore must be
run in a :ref:`dedicated session <transactions>`.

.. extautosummary::
   :nosignatures: 

   ~RepositoryConnection.registerNeighborMatrix
   ~RepositoryConnection.registerSNAGenerator

.. _transactions:
   
Transactions
~~~~~~~~~~~~

AllegroGraph lets you set up a special |RepositoryConnection| (a
"session") that supports transaction semantics. You can add statements
to this session until you accumulate all the triples you need for a
specific transaction. Then you can commit the triples in a single act.
Up to that moment the triples will not be visible to other users of the
repository.

If anything interrupts the accumulation of triples building to the
transaction, you can roll back the session. This discards all of the
uncommitted triples and resynchronizes the session with the repository
as a whole.

Closing the session discards all uncommitted triples and all rules,
generators, and matrices that were created in the session. Rules,
generators, and matrices cannot be committed. They persist as long as the
session persists.

.. extautosummary::
   :nosignatures: 

   ~RepositoryConnection.openSession
   ~RepositoryConnection.closeSession
   ~RepositoryConnection.session
   ~RepositoryConnection.commit
   ~RepositoryConnection.rollback

Subject Triples Caching
~~~~~~~~~~~~~~~~~~~~~~~

You can enable subject triple caching to speed up queries where the same
subject URI appears in multiple patterns. The first time AllegroGraph
retrieves triples for a specific resource, it caches the triples in
memory. Subsequent query patterns that ask for the same subject URI can
retrieve the matching triples very quickly from the cache. The cache has
a size limit and automatically discards old entries as that limit is exceeded.


.. extautosummary::
   :nosignatures:

   ~RepositoryConnection.enableSubjectTriplesCache
   ~RepositoryConnection.disableSubjectTriplesCache
   ~RepositoryConnection.getSubjectTriplesCacheSize

Query Class (and Subclasses)
----------------------------

.. currentmodule:: franz.openrdf.query.query

.. note::
   The Query class is non-instantiable. It is an abstract class from which
   the three query subclasses are derived. It is included here because of
   its methods, which are inherited by the subclasses.

A query on a |Repository| that can be formulated
in one of the supported query languages (for example SPARQL). The query can
be parameterized, to allow one to reuse the same query with different parameter
bindings.

The best practice is to allow the |RepositoryConnection| object to
create an instance of one of the Query subclasses
(|TupleQuery|, |GraphQuery|, |BooleanQuery|, |UpdateQuery|).
There is no reason for the Python application programmer to create
such objects directly.

.. code:: python

   tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
   result = tupleQuery.evaluate()

.. extautosummary::
   :nosignatures:
   
   methods_without_init:Query

Subclass TupleQuery
~~~~~~~~~~~~~~~~~~~

This subclass is used with SELECT queries. Use the
:meth:`~.RepositoryConnection.prepareTupleQuery` method to create
a TupleQuery object. The results of the query are returned in a
:class:`.QueryResult` iterator that yields a sequence of
:class:`binding sets <.ListBindingSet>`.

TupleQuery uses all the methods of the |Query| class,
plus two more:

.. extautosummary::
   :nosignatures:

   ~TupleQuery.evaluate
   ~TupleQuery.analyze

Subclass GraphQuery
~~~~~~~~~~~~~~~~~~~

This subclass is used with CONSTRUCT and DESCRIBE queries. Use the
:meth:`~.RepositoryConnection.prepareGraphQuery` method to create a
GraphQuery object. The results of the query are returned in a
:class:`.GraphQueryResult` iterator that yields a
sequence of :class:`statements <franz.openrdf.model.Statement>`.

|GraphQuery| implements all the methods of the |Query| class,
plus one more:

.. extautosummary::
   :nosignatures:

   ~GraphQuery.evaluate

Subclass BooleanQuery
~~~~~~~~~~~~~~~~~~~~~

This subclass is used with ASK queries. Use the
:meth:`~.RepositoryConnection.prepareBooleanQuery` method to create
a |BooleanQuery| object. The results of the query are
``True`` or ``False``.

BooleanQuery implements all the methods of the |Query|
class, plus one more:

.. extautosummary::
   :nosignatures:

   ~BooleanQuery.evaluate

Subclass UpdateQuery
~~~~~~~~~~~~~~~~~~~~

This subclass is used for ``DELETE`` and ``INSERT`` queries.  The
result returned when the query is evaluated is a boolean that can be
used to tell if the store has been modified by the operation. Use the
:meth:`~.RepositoryConnection.prepareUpdate` method to create an
|UpdateQuery| object.

|UpdateQuery| implements all the methods of the |Query|
class, plus one more:

.. extautosummary::
   :nosignatures:

   ~UpdateQuery.evaluate

QueryResult Class
-----------------

.. currentmodule:: franz.openrdf.query.queryresult

A QueryResult object is simply an iterator that also has a
:meth:`~QueryResult.close` method that must be called to free
resources. Such objects are returned as a result of SPARQL and PROLOG
query evaluation and should not be constructed directly. Result
objects are context managers and can be used in the ``with``
statement. The recommended usage looks like this:

.. code:: python

   tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
   with tupleQuery.evaluate() as results:
      for result in results:
          print result

It is also possible to use one of the ``execute*Query`` methods
(e.g. :meth:`executeTupleQuery`) to prepare and execute a query in a
single call:

.. code:: python

   with conn.executeTupleQuery(queryString) as results:
      for result in results:
          print result

.. extautosummary::
   :nosignatures:

   ~QueryResult.close
   ~QueryResult.__next__

Subclass TupleQueryResult
~~~~~~~~~~~~~~~~~~~~~~~~~

A QueryResult subclass used for queries that return tuples.

.. extautosummary::
   :nosignatures:

   ~TupleQueryResult.getBindingNames


Subclass GraphQueryResult
~~~~~~~~~~~~~~~~~~~~~~~~~

A QueryResult subclass used for queries that return statements. Objects
of this class are also |RepositoryResult| instances.


RepositoryResult class
----------------------

.. currentmodule:: franz.openrdf.repository.repositoryresult

A |RepositoryResult| object is a result collection of :class:`statements
<franz.openrdf.model.Statement>` that can be iterated over. It keeps
an open connection to the backend for lazy retrieval of individual
results. Additionally it has some utility methods to fetch all results
and add them to a collection.

By default, a |RepositoryResult| is not necessarily a (mathematical)
set: it may contain duplicate objects. Duplicate filtering can be
switched on, but this should not be used lightly as the filtering
mechanism is potentially memory-intensive.

A |RepositoryResult| must be closed after use to free up any resources
(open connections, read locks, etc.) it has on the underlying
repository. To make this easier it is a context manager and
can be used in the ``with`` statement.

.. code:: python

   graphQuery = conn.prepareGraphQuery(QueryLanguage.SPARQL, queryString)
   with graphQuery.evaluate() as results:
       for result in results:
           print result

.. extautosummary::
   :nosignatures:

   ~RepositoryResult.close
   ~RepositoryResult.__next__
   ~RepositoryResult.enableDuplicateFilter
   ~RepositoryResult.asList
   ~RepositoryResult.addTo
   ~RepositoryResult.rowCount

.. currentmodule:: franz.openrdf.model
   
Statement Class
---------------

A :class:`.Statement` is a client-side triple. It encapsulates the subject,
predicate, object and context (subgraph) values of a single triple and
makes them available.

Best practice is to allow the :meth:`.RepositoryConnection.createStatement`
method to create and return the Statement object. There is no reason for
the Python application programmer to create a Statement object
directly.

.. code:: python

        stmt1 = conn.createStatement(alice, age, fortyTwo)

.. extautosummary::
   :nosignatures:

   ~Statement.getContext
   ~Statement.getObject
   ~Statement.getPredicate
   ~Statement.getSubject

ValueFactory Class
------------------

A ValueFactory is a factory for creating URIs, blank nodes, literals
and :class:`.Statement` objects. In the AllegroGraph Python interface,
the :class:`.ValueFactory` class is regarded as obsolete. Its
functions have been subsumed by the expanded capability of the
|RepositoryConnection| class. It is documented here for the
convenience of users porting an application from Eclipse RDF4J.

.. extautosummary::
   :nosignatures:

   ~ValueFactory.createBNode
   ~ValueFactory.createLiteral
   ~ValueFactory.createStatement
   ~ValueFactory.createURI


.. Links
   
.. _AllegroGraph: http://franz.com/agraph/allegrograph/
