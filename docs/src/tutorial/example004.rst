.. _example4:

Example 4: Statement matching
-----------------------------

The :meth:`getStatements` method of the connection object provides a
simple way to perform unsophisticated queries. This method lets you
enter a mix of required values and wildcards, and retrieve all
matching triples. (If you need to perform sophisticated tests and
comparisons you should use a SPARQL query instead.)

Below, we illustrate two kinds of :meth:`getStatements` calls. The
first mimics traditional RDF4J syntax, and returns a |Statement|
object at each iteration. We will reuse data from in previous
examples to create a connection object and populate the repository
with four triples describing Bob and Alice. We're going to search for
triples that mention Alice, so we have to create an "Alice" URI to use
in the search pattern:

.. literalinclude:: doctest_setup.py
   :language: python
   :start-after: BEGIN-CONNECT
   :end-before: END-CONNECT

.. testcode:: example4

   conn.addData("""
       @base <http://example.org/> .

       <people/alice> a <ontology/Person> ;
                      <ontology/name> "Alice" .
       <people/bob> a <ontology/Person> ;
                    <ontology/name> "Bob" .
   """)
   alice = conn.createURI("http://example.org/people/alice")

Now we search for triples with Alice's URI in the subject
position. The ``None`` values are wildcards for the predicate and
object positions of the triple.

.. testcode:: example4

   statements = conn.getStatements(alice, None, None)

The :meth:`getStatements` method returns a |RepositoryResult| object
(bound to the variable ``statements`` in this case). This object can
be iterated over, exposing one result statement at a time. It is
sometimes desirable to screen the results for duplicates, using the
:meth:`.enableDuplicateFilter` method. Note, however, that duplicate
filtering can be expensive. Our example does not contain any
duplicates, but it is possible for them to occur.

.. testcode:: example4

   with statements:
       statements.enableDuplicateFilter() 
       for statement in statements:
           print(statement)

This prints out the two matching triples for "Alice."

.. testoutput:: example4

   (<http://example.org/people/alice>, <http://www.w3.org/1999/02/22-rdf-syntax-ns#type>, <http://example.org/ontology/Person>)
   (<http://example.org/people/alice>, <http://example.org/ontology/name>, "Alice") 

Notice how we used the ``with`` keyword to ensure that the
|RepositoryResult| object is closed after the results are
fetched. This is necessary to release resources used during result
retrieval. The same goal could be accomplished by calling the
:meth:`.RepositoryResult.close` method (preferably in a ``finally``
block to ensure exception safety).

