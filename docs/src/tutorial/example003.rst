.. _example3:

Example 3: A SPARQL query
-------------------------

SPARQL stands for the `"SPARQL Protocol and RDF Query Language,"
<http://www.w3.org/TR/rdf-sparql-query/>`_ a recommendation of the
`World Wide Web Consortium (W3C) <http://www.w3.org/>`_ . SPARQL is a
query language for retrieving RDF triples.

Our next example illustrates how to evaluate a SPARQL query. This is
the simplest query, the one that returns all triples. Note that we
will use the same triples that were used in :ref:`example2`.

Let's create the connection first:

.. literalinclude:: doctest_setup.py
   :language: python_rdf
   :start-after: BEGIN-CONNECT
   :end-before: END-CONNECT

And now we can add our data and define the query:

.. testcode:: example3

   conn.addData("""
       @base <http://example.org/> .

       <people/alice> a <ontology/Person> ;
                      <ontology/name> "Alice" .
       <people/bob> a <ontology/Person> ;
                    <ontology/name> "Bob" .
   """)
   query_string = "SELECT ?s ?p ?o  WHERE {?s ?p ?o . } ORDER BY ?s ?p ?o"
      
The ``SELECT`` clause returns the variables ``?s``, ``?p`` and ``?o``
in the binding set. The variables are bound to the subject, predicate
and objects values of each triple that satisfies the WHERE clause. In
this case the WHERE clause is unconstrained. The dot (``.``) in the
fourth position signifies the end of the pattern.  We use ORDER BY
to return the results in a consistent order for demonstration purposes.

The connection object's :meth:`prepareTupleQuery` method creates a
query object that can be evaluated one or more times. The results are
returned in an iterator that yields a sequence of binding sets.

.. testcode:: example3

   from franz.openrdf.query.query import QueryLanguage

   tuple_query = conn.prepareTupleQuery(QueryLanguage.SPARQL, query_string)
   result = tuple_query.evaluate()

Below we illustrate one method for extracting the values from a
binding set, indexed by the name of the corresponding column variable
in the ``SELECT`` clause.

.. testcode:: example3

   with result:
      for binding_set in result:
           s = binding_set.getValue("s")
           p = binding_set.getValue("p")
           o = binding_set.getValue("o")              
           print("%s %s %s" % (s, p, o))

.. testoutput:: example3

   <http://example.org/people/alice> <http://example.org/ontology/name> "Alice"
   <http://example.org/people/alice> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/ontology/Person>
   <http://example.org/people/bob> <http://example.org/ontology/name> "Bob"
   <http://example.org/people/bob> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/ontology/Person>

Note that we have wrapped the whole result processing in a ``with``
statement. The reason is that result objects must be closed after
processing to release resources. The most convenient way to ensure
this is the ``with`` statement, but it is also possible to explicitly
call :meth:`~franz.openrdf.query.queryresult.QueryResult.close`
(e.g. in a ``finally`` block).
