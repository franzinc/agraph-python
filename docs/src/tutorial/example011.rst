.. _example11:

Example 11: Namespaces
----------------------

A *namespace* is that portion of a URI that preceeds the last ``#``,
``/``, or ``:`` character, inclusive. The remainder of a URI is called
the ``localname``. For example, with respect to the URI
``http://example.org/people/alice``, the namespace is
``http://example.org/people/`` and the localname is ``alice``. When
writing SPARQL queries, it is convenient to define prefixes or
nicknames for the namespaces, so that abbreviated URIs can be
specified. For example, if we define ``ex`` to be a nickname for
``http://example.org/people/``, then the string ``ex:alice`` is a
recognized abbreviation for ``http://example.org/people/alice``. This
abbreviation is called a *qname* (qualified name).

In the SPARQL query discussed in this chapter we see two qnames,
``rdf:type`` and ``ex:alice``. Ordinarily, we would expect to see
``PREFIX`` declarations in SPARQL that define namespaces for the
``rdf`` and ``ex`` nicknames. However, the connection and query
machinery can do that job for you. The mapping of prefixes to
namespaces includes the built-in prefixes ``rdf``, ``rdfs``, ``xsd``,
and ``owl``. Hence, we can write ``rdf:type`` in a SPARQL query, and
the system already knows its meaning. In the case of the ``ex``
prefix, we need to instruct it. The :meth:`setNamespace` method of the
connection object registers a new namespace.

.. note:: It is legal, although not recommended, to redefine the
          built-in prefixes (RDF, XSD etc...).

We start by opening a connection

.. literalinclude:: doctest_setup.py
   :language: python_rdf
   :start-after: BEGIN-CONNECT
   :end-before: END-CONNECT

and creating two URIs. Note how :meth:`createURI` allows us to compose
URIs from namespaces and local names.
          
.. testcode:: example11

   exns = "http://example.org/people/"
   alice = conn.createURI(namespace=exns, localname="alice")
   person = conn.createURI(namespace=exns, localname="Person")

Now we can assert Alice's RDF:TYPE triple.
         
.. testcode:: example11
              
   from franz.openrdf.vocabulary.rdf import RDF

   conn.add(alice, RDF.TYPE, person)

Now we register the ``exns`` namespace with the connection object, so
we can use it in a SPARQL query. The query looks for triples that have
``rdf:type`` in the predicate position, and ``ex:Person`` in the
object position.

.. testcode:: example11
 
   conn.setNamespace('ex', exns)
   conn.executeTupleQuery("""
       SELECT ?s ?p ?o WHERE {
           ?s ?p ?o .
           FILTER (?p = rdf:type && ?o = ex:Person)
       }""", output=True)

The output shows the single triple that we expected to find. This
demonstrates that the qnames in the SPARQL query successfully matched
the fully-expanded URIs in the triple. Note that the namespace prefix
is also used in the table below.

.. testoutput:: example11

   -----------------------------------
   | s        | p        | o         |
   ===================================
   | ex:alice | rdf:type | ex:Person |
   -----------------------------------

It should be mentioned here that the prefix of a namespace can be an
empty string. This allows the resulting qnames to be very concise and
readable:

.. testcode:: example11

   conn.setNamespace('', 'http://a-long-and-often-used-namespace/')
   conn.executeUpdate('insert data { :this :looks :nice }')
   conn.executeTupleQuery('select ?s { ?s :looks :nice }',
                          output=True)

.. testoutput:: example11
                          
   ---------
   | s     |
   =========
   | :this |
   ---------
