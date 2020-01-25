.. _example13:

Example 13: SPARQL query forms
------------------------------

SPARQL provides alternatives to the standard SELECT query. This
example exercises these alternatives to show how AllegroGraph Server
and the Python client handle them.

Let's connect to the database:

.. literalinclude:: doctest_setup.py
   :language: python_rdf
   :start-after: BEGIN-CONNECT
   :end-before: END-CONNECT

We'll need some sample data to illustrate all the query types. Our
dataset will contain information about rulers of 17th century England.

.. testcode:: example13

   conn.addData("""
       @prefix : <ex://> .
       @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
       
       :james_i :reigned_from "1603-03-24"^^xsd:date ;
                :reigned_to "1625-03-27"^^xsd:date .
       :charles_i :reigned_from "1625-03-27"^^xsd:date ;
                  :reigned_to "1649-01-30"^^xsd:date ;
                  :child_of :james_i .
       :charles_ii :reigned_from "1649-01-30"^^xsd:date ;
                :reigned_to "1685-02-06"^^xsd:date ;
                :child_of :charles_i .
       :james_ii :reigned_from "1685-02-06"^^xsd:date ;
                :reigned_to "1688-12-11"^^xsd:date ;
                :child_of :charles_i .
       :mary_ii :reigned_from "1689-02-13"^^xsd:date ;
                :reigned_to "1694-12-28"^^xsd:date ;
                :child_of :james_ii .
       :william_iii :reigned_from "1672-07-04"^^xsd:date ;
                    :reigned_to "1702-03-08"^^xsd:date .
       :anne :reigned_from "1707-05-01"^^xsd:date ;
             :reigned_to "1714-08-01"^^xsd:date ;
             :child_of :james_ii .
   """)

``SELECT``
~~~~~~~~~~

This kind of query returns a sequence of tuples, binding variables to
matching elements of a search pattern. ``SELECT`` queries are created
using :meth:`prepareTupleQuery` and return results of type
:class:`.TupleQueryResult`. Query result can also be serialized in a
supported :class:`.TupleFormat` - in previous examples we used
``output=True`` and relied on the default ``TupleFormat.TABLE``.

Here's a sample query which locates all rulers whose grandchildren
inherited the crown:

.. testcode:: example13

   conn.setNamespace('', 'ex://')
   query = conn.prepareTupleQuery(query="""
       SELECT DISTINCT ?name WHERE {
           ?grandchild :child_of/:child_of ?name .
       } ORDER BY ?name """)
       
   with query.evaluate() as result:
       for bindings in result:
           print(bindings.getValue('name'))

Two names are returned:

.. testoutput:: example13

   <ex://charles_i>
   <ex://james_i>

We can also serialize the output instead of processing the result
object. This time let us reverse the query and ask for rulers whose
grandparents are also in the dataset:

.. testcode:: example13

   from franz.openrdf.rio.tupleformat import TupleFormat

   query = conn.prepareTupleQuery(query="""
      SELECT DISTINCT ?name WHERE {
         ?name :child_of/:child_of ?grandparent .
      } ORDER BY ?name """)

   query.evaluate(output=True, output_format=TupleFormat.CSV)

We get four results, serialized as CSV:

.. testoutput:: example13

   name
   "ex://anne"
   "ex://charles_ii"
   "ex://james_ii" 
   "ex://mary_ii"   
   
``ASK``
~~~~~~~

The ``ASK`` query returns a Boolean, depending on whether the triple
pattern matched any triples. Queries of this type are created using
:meth:`prepareBooleanQuery`.

Let's check if there were any co-regencies in the time period
described by our dataset:

.. testcode:: example13

   query = conn.prepareBooleanQuery(query="""

       ASK { ?ruler1 :reigned_from ?r1from ;
                     :reigned_to ?r1to .
             ?ruler2 :reigned_from ?r2from ;
                     :reigned_to ?r2to .
             FILTER (?ruler1 != ?ruler2 &&
                     ?r1from >= ?r2from &&
                     ?r1from < ?r2to)
       }""")

   print(query.evaluate())

There was one (William and Mary):
   
.. testoutput:: example13

   True

``CONSTRUCT``
~~~~~~~~~~~~~

The ``CONSTRUCT`` query creates triples by substantiating provided
templates with values resulting from matching a pattern. Queries of
this kind are created using :meth:`prepareGraphQuery` and return a
:class:`.RepositoryResult` - which is an iterator over the constructed
triples.

.. note::

   Executing a ``CONSTRUCT`` query will *not* add any triples to the
   store. To insert the data we have to iterate over the result and
   add each triple using :meth:`addStatement` (or use an ``INSERT``
   query).

Let us consider a query that calculates a ``:sibling_of``
relationship:
   
.. testcode:: example13

   print('Size before: {0}'.format(conn.size()))
   query = conn.prepareGraphQuery(query="""
      CONSTRUCT {
          ?person1 :sibling_of ?person2 .
      } WHERE {
          ?person1 :child_of ?parent .
          ?person2 :child_of ?parent .
          filter (?person1 != ?person2) .
      } ORDER BY ?person1""")
   for stmt in query.evaluate():
       print('{0} <-> {1}'.format(stmt.getSubject(),
                                  stmt.getObject()))
   print('Size after: {0}'.format(conn.size()))

The returned object is an iterator over |Statement| objects. We can
also see that no data has been added to the repository.

.. testoutput:: example13

   Size before: 19
   <ex://anne> <-> <ex://mary_ii>
   <ex://charles_ii> <-> <ex://james_ii>
   <ex://james_ii> <-> <ex://charles_ii>
   <ex://mary_ii> <-> <ex://anne>
   Size after: 19

We can also serialize the result using any of the supported
:class:`RDFFormats <.RDFFormat>`:

.. testcode:: example13

   from franz.openrdf.rio.rdfformat import RDFFormat

   query.evaluate(output=True,
                  output_format=RDFFormat.NTRIPLES)

Here we use the `N-Triples`_ format. This happens to be the default,
so we could have omitted the ``output_format`` argument.
                  
.. testoutput:: example13

   <ex://anne> <ex://sibling_of> <ex://mary_ii> .
   <ex://charles_ii> <ex://sibling_of> <ex://james_ii> .
   <ex://james_ii> <ex://sibling_of> <ex://charles_ii> .
   <ex://mary_ii> <ex://sibling_of> <ex://anne> .
               
``DESCRIBE``
~~~~~~~~~~~~

The ``DESCRIBE`` query returns triples that 'describe' a given set of
resources. Such queries are created using :meth:`prepareGraphQuery`
and return :class:`.RepositoryResult` objects.

The set of resources to be processed is specified by a query
pattern. The SPARQL standard does not say what triples constitute a
'description' of a particular resource. AllegroGraph will return the
`Concise Bounded Description`_ of the queried resources.

Let's use a ``DESCRIBE`` query to see what data do we have regarding
the children of Charles I:

.. testcode:: example13

   query = conn.prepareGraphQuery(query="""
       DESCRIBE ?child WHERE {
           ?child :child_of :charles_i
       }""")
   for stmt in query.evaluate():
       print(stmt)

In this case AllegroGraph will simply return all triples with subject
in the specified set:
       
.. testoutput:: example13
   :options: +SORT
                
   (<ex://charles_ii>, <ex://reigned_from>, "1649-01-30"^^<http://www.w3.org/2001/XMLSchema#date>)
   (<ex://charles_ii>, <ex://reigned_to>, "1685-02-06"^^<http://www.w3.org/2001/XMLSchema#date>)
   (<ex://charles_ii>, <ex://child_of>, <ex://charles_i>)
   (<ex://james_ii>, <ex://reigned_from>, "1685-02-06"^^<http://www.w3.org/2001/XMLSchema#date>)
   (<ex://james_ii>, <ex://reigned_to>, "1688-12-11"^^<http://www.w3.org/2001/XMLSchema#date>)
   (<ex://james_ii>, <ex://child_of>, <ex://charles_i>)

DESCRIBE queries can be useful for exploring a dataset and learning
what properties a certain object might have. The results of such
queries can be serialized to any supported :class:`.RDFFormat`:

.. testcode:: example13

   query.evaluate(output=True,
                  output_format=RDFFormat.NTRIPLES)

.. testoutput:: example13
   :options: +SORT

   <ex://charles_ii> <ex://reigned_from> "1649-01-30"^^<http://www.w3.org/2001/XMLSchema#date> .
   <ex://charles_ii> <ex://reigned_to> "1685-02-06"^^<http://www.w3.org/2001/XMLSchema#date> .
   <ex://charles_ii> <ex://child_of> <ex://charles_i> .
   <ex://james_ii> <ex://reigned_from> "1685-02-06"^^<http://www.w3.org/2001/XMLSchema#date> .
   <ex://james_ii> <ex://reigned_to> "1688-12-11"^^<http://www.w3.org/2001/XMLSchema#date> .
   <ex://james_ii> <ex://child_of> <ex://charles_i> .


.. _Concise Bounded Description: https://www.w3.org/Submission/CBD/
