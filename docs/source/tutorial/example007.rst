.. _example7:

Example 7: Querying multiple contexts
-------------------------------------

The purpose of this example is to see how data imported into multiple
contexts (like that from :ref:`example6`) behaves when queried using
various methods. This exampe covers only the results of basic
queries. The subject is explored in more detail in :ref:`example10`.

Let us start by creating a connection:

.. literalinclude:: doctest_setup.py
   :language: python
   :start-after: BEGIN-CONNECT
   :end-before: END-CONNECT

and adding a few triples in the default context:

.. testcode:: example7

   from franz.openrdf.query.query import QueryLanguage

   conn.addData("""
      <ex://default1> <ex://p1> 1 .
      <ex://default2> <ex://p2> 2 .
      <ex://default3> <ex://p3> 3 .""")

We can add data to another contect by using the optional ``context``
parameter of :meth:`addData`:

.. testcode:: example7

   context = conn.createURI('ex://context')
   conn.addData("""
      <ex://context1> <ex://p1> 1 .
      <ex://context2> <ex://p2> 2 .
      <ex://context3> <ex://p3> 3 .""",
      context=context)

Let's try a :meth:`getStatements` call first:

.. testcode:: example7

   p1 = conn.createURI('ex://p1')
   with conn.getStatements(None, p1, None, None) as result:
       for row in result:
           print(row.getSubject())

This loop prints out a mix of triples from the default context and
from the named context.
           
.. testoutput:: example7

   <ex://context1>
   <ex://default1>

SPARQL queries behave in a different way. When a graph clause is
present, as in the following code, triples that are not in a named
context will not be examined:
   
.. testcode:: example7
   
   query_string = """
       SELECT DISTINCT ?s WHERE {
         graph ?g { ?s ?p ?o filter(?o > 2).
       }} order by ?s"""
   tuple_query = conn.prepareTupleQuery(
       QueryLanguage.SPARQL, query_string)
   with tuple_query.evaluate() as result:
       for bindings in result:
           print(bindings[0])

Only the ``context3`` triple is printed:
           
.. testoutput:: example7

   <ex://context3>

What happens if we issue a trivial query without mentioning ``graph``?

.. testcode:: example7
   
   query_string = """
       SELECT DISTINCT ?s WHERE {
         ?s ?p ?o .
       } order by ?s"""
   tuple_query = conn.prepareTupleQuery(
       QueryLanguage.SPARQL, query_string)
   with tuple_query.evaluate() as result:
       for bindings in result:
           print(bindings[0])

This prints all triples, just like a :meth:`getStatements` call.
    
.. testoutput:: example7

   <ex://context1>
   <ex://context2>
   <ex://context3>
   <ex://default1>
   <ex://default2>
   <ex://default3>

But this behavior can be altered by setting a query option.
AllegroGraph allows such options to be set by defining
a prefix.

.. testcode:: example7

   query_string = """
       PREFIX franzOption_defaultDatasetBehavior: <franz:rdf>
       SELECT DISTINCT ?s WHERE {
         ?s ?p ?o .
       } order by ?s"""
   tuple_query = conn.prepareTupleQuery(
       QueryLanguage.SPARQL, query_string)
   with tuple_query.evaluate() as result:
       for bindings in result:
           print(bindings[0])

Now only the default context is matched by simple pattern (i.e. ones
not wrapped in ``graph ?g { ... }``)


.. testoutput:: example7

   <ex://default1>
   <ex://default2>
   <ex://default3>
