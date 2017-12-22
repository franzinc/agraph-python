.. _example10:

Example 10: Graphs in SPARQL
----------------------------

In :ref:`example6` and :ref:`example7` we've seen how to import data
to a non-default context and run queries against such data. In this
example we'll explore facilities for handling multiple contexts
provided by SPARQL and the AllegroGraph Python client.

We'll start by opening a connection:

.. literalinclude:: doctest_setup.py
   :language: python
   :start-after: BEGIN-CONNECT
   :end-before: END-CONNECT

Now we will create two URIs that will represent named contexts.

.. testcode:: example10

   context1 = conn.createURI("ex://context1")      
   context2 = conn.createURI("ex://context2")

The first context will be filled using the :meth:`addData` method:

.. testcode:: example10

   conn.addData("""
       @prefix : <ex://> .
       :alice a :person ;
              :name "Alice" .""",
       context=context1)

The second context will be filled using :meth:`addTriple`. Notice how
we use a constant defined in the ``RDF`` class to obtain the URI of
the ``type`` predicate:

.. testcode:: example10

   from franz.openrdf.vocabulary.rdf import RDF
   
   bob = conn.createURI('ex://bob')
   bob_name = conn.createLiteral('Bob')
   name = conn.createURI('ex://person')
   person = conn.createURI('ex://person')
   conn.addTriple(bob, RDF.TYPE, person,
                  contexts=[context2])
   conn.addTriple(bob, name, bob_name,
                  contexts=[context2])

Finally we'll add two triples to the default context using
:meth:`addStatement`:

.. testcode:: example10

   from franz.openrdf.model import Statement
   
   ted = conn.createURI('ex://ted')
   ted_name = conn.createLiteral('Ted')
   stmt1 = Statement(ted, name, ted_name)
   stmt2 = Statement(ted, RDF.TYPE, person)
   conn.addStatement(stmt1)
   conn.addStatement(stmt2)

.. warning::

   The :class:`.Statement` object contains a `context` field.
   This field is *ignored* by :meth:`addStatement`. If you
   wish to add a statement object to a specific context, use
   the ``contexts`` parameter.

As we've seen already in :ref:`example7`, a call to
:meth:`getStatements` will return triples from all contexts:

.. testcode:: example10

   with conn.getStatements() as result:
       print('getStatements(): {0}'.format(len(result)))
   print('size(): {0}'.format(conn.size()))

:meth:`size` will also process all contexts by default.
   
.. testoutput:: example10

   getStatements(): 6  
   size(): 6

Both :meth:`getStatements` and :meth:`size` accept a ``contexts``
parameter that can be used to limit processing to a specified list of
graphs:

.. testcode:: example10

   contexts = [context1, context2]
   with conn.getStatements(contexts=contexts) as result:
       print('getStatements(): {0}'.format(len(result)))
   print('size(): {0}'.format(conn.size(contexts=contexts)))

As expected, triples from the default context are not processed:
   
.. testoutput:: example10

   getStatements(): 4
   size(): 4

To include the default graph when using the ``contexts`` parameter use
``None`` as a graph URI:

.. testcode:: example10

   contexts = [context1, None]
   with conn.getStatements(contexts=contexts) as result:
       print('getStatements(): {0}'.format(len(result)))
   print('size(): {0}'.format(conn.size(contexts=contexts)))

Now triples from the default context and from one of our named
contexts are processed:
   
.. testoutput:: example10

   getStatements(): 4
   size(): 4

SPARQL using ``FROM``, ``FROM DEFAULT``, and ``FROM NAMED``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In many of our examples we have used a simple SPARQL query to retrieve
triples from AllegroGraph's default graph. This has been very
convenient but it is also misleading. As soon as we tell SPARQL to
search a specific graph, we lose the ability to search AllegroGraph's
default graph! Triples from the null graph vanish from the search
results. Why is that?

It is important to understand that AllegroGraph and SPARQL use the
phrase "default graph" to identify two very different
things.

   * AllegroGraph's default graph, or null context, is simply the set
     of all triples that have `null` in the fourth field of the
     "triple." The *default graph* is an unnamed subgraph of the
     AllegroGraph triple store.

   * SPARQL uses *default graph* to describe something that is very
     different. In SPARQL, the *default graph* is a temporary pool of
     triples imported from one or more *named* graphs. SPARQL's
     *default graph* is constructed and discarded in the service of a
     single query.  Standard SPARQL was designed for named graphs
     only, and has no syntax to identify a truly unnamed
     graph. AllegroGraph's SPARQL, however, has been extended to allow
     the unnamed graph to participate in multi-graph queries.

We can use AllegroGraph's SPARQL to search specific subgraphs in three
ways.

   * We can create a temporary *default graph* using the ``FROM``
     operator.

   * We can put AllegroGraph's unnamed graph into SPARQL's default
     graph using ``FROM DEFAULT``.

   * Or we can target specific named graphs using the ``FROM NAMED``
     operator.

Here's an example of a query that accesses the unnamed graph explicitly:

.. testcode:: example10

   query = conn.prepareTupleQuery(query="""
       SELECT DISTINCT ?s FROM DEFAULT {
           ?s ?p ?o
       }""")
   query.evaluate(output=True)

This will not process any of the triples in named contexts:

.. testoutput:: example10

   ------------
   | s        |
   ============
   | ex://ted |
   ------------

Here's an example of a query that uses ``FROM``. It instructs SPARQL
to regard ``context1`` as the default graph for the purposes of this
query.

.. testcode:: example10

   query = conn.prepareTupleQuery(query="""
       SELECT DISTINCT ?s FROM <ex://context1> {
           ?s ?p ?o
       }""")
   query.evaluate(output=True)

Now only one context is processed:
   
.. testoutput:: example10

   --------------
   | s          |
   ==============
   | ex://alice |
   --------------

The next example changes ``FROM`` to ``FROM NAMED`` in the same query:

.. testcode:: example10

   query = conn.prepareTupleQuery(query="""
       SELECT DISTINCT ?s FROM NAMED <ex://context1> {
           ?s ?p ?o
       }""")
   query.evaluate(output=True)
   
There are no matches now! The pattern ``{ ?s ?p ?o . }`` only matches
the SPARQL default graph. We declared ``context1`` to be a *named*
graph, so it is no longer the default graph.

.. testoutput:: example10

   -----
   | s |
   =====
   -----

To match triples in named graphs, SPARQL requires a ``GRAPH`` pattern:

.. testcode:: example10

   query = conn.prepareTupleQuery(query="""
       SELECT DISTINCT ?s ?g FROM NAMED <ex://context1> {
           GRAPH ?g { ?s ?p ?o }
       }""")
   query.evaluate(output=True)

This time we'll also print the graph:
   
.. testoutput:: example10

   ------------------------------
   | s          | g             |
   ==============================
   | ex://alice | ex://context1 |
   ------------------------------

We can also combine all the forms presented above:

.. testcode:: example10

   query = conn.prepareTupleQuery(query="""
       SELECT DISTINCT ?s ?g
       FROM DEFAULT
       FROM <ex://context1>
       FROM NAMED <ex://context2> {
           { ?s ?p ?o } UNION { GRAPH ?g { ?s ?p ?o } }
       }""")
   query.evaluate(output=True)

This query puts AllegroGraph's unnamed graph and the ``context1``
graph into SPARQL's default graph, where the triples can be found by
using a simple ``{?s ?p ?o . }`` query.  Then it identifies
``context2`` as a named graph, which can be searched using a ``GRAPH``
pattern.  In the final line, we used a ``UNION`` operator to combine
the matches of the simple and ``GRAPH`` patterns.

This query should find all three subjects:

.. testoutput:: example10
   :options: +SORT

   ------------------------------
   | s          | g             |
   ==============================
   | ex://alice | ---           |
   | ex://ted   | ---           |
   | ex://bob   | ex://context2 |
   ------------------------------


SPARQL with :class:`.Dataset` object
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A :class:`.Dataset` object is a construct that contains two lists of
named graphs. There is one list of graphs that will become the SPARQL
default graph, just like using ``FROM`` in the query. There is a
second list of graphs that will be *named graphs* in the query, just
like using FROM NAMED. To use the dataset, we put the graph URIs into
the dataset object, and then add the dataset to the query object. When
we evaluate the query, the results will be confined to the graphs
listed in the dataset.

.. exttestcode:: example10
   :emphasize-lines: 10

   from franz.openrdf.query.dataset import Dataset
                     
   dataset = Dataset()
   dataset.addDefaultGraph(context1)
   dataset.addNamedGraph(context2)
   query = conn.prepareTupleQuery(query="""
       SELECT DISTINCT ?s ?g {
         { ?s ?p ?o } UNION { GRAPH ?g { ?s ?p ?o } }
       }""")
   query.setDataset(dataset)   
   query.evaluate(output=True)

Note that, since we're explicitly specifying graphs (through a dataset
object), we need a ``GRAPH`` pattern to match triples from the named
graphs. Triples from the unnamed graph are not matched at all, since
that graph is not a part of the dataset.
   
.. testoutput:: example10
   :options: +SORT

   ------------------------------
   | s          | g             |
   ==============================
   | ex://alice | ---           |
   | ex://bob   | ex://context2 |
   ------------------------------
