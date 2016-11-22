.. _example2:

Example 2: Asserting and retracting triples
-------------------------------------------

In this example we show how to create resources describing two people,
Bob and Alice, by asserting triples into the repository. The example
also retracts and replaces a triple. Assertions and retractions to the
triple store are executed by :meth:`~add` and :meth:`~remove` methods
belonging to the connection object, which we obtain by calling the
``connect()`` function from :ref:`example1`.

Before asserting a triple, we have to generate the URI values for the
subject, predicate and object fields. The AllegroGraph Python client
API predefines a number of classes and predicates for the RDF, RDFS,
XSD, and OWL ontologies. ``RDF.TYPE`` is one of the predefined
predicates we will use.

The :meth:`~add` and :meth:`~remove` methods take an optional
``contexts`` argument that specifies one or more subgraphs that are
the target of triple assertions and retractions. When the context is
omitted, triples are asserted/retracted to/from the default graph. In
the example below, facts about Alice and Bob reside in the default
graph.

The second example begins by calling ``connect()`` to create the
appropriate connection object, which is bound to the variable ``conn``.

.. testcode:: example2

   conn = connect()

The next step is to begin assembling the URIs we will need for the new
triples. The :meth:`~createURI` method generates a URI from a string.
These are the subject URIs identifying the resources "Bob" and "Alice":

.. testcode:: example2

   alice = conn.createURI("http://example.org/people/alice")
   bob = conn.createURI("http://example.org/people/bob")

Bob and Alice will be members of the "person" class (rdf type
``person``).

.. testcode:: example2

   person = conn.createURI("http://example.org/ontology/Person")

Both Bob and Alice will have a "name" attribute.

.. testcode:: example2

   name = conn.createURI("http://example.org/ontology/name")

The name attributes will contain literal values. We have to generate the
:class:`~franz.openrdf.model.Literal` objects from strings:

.. testcode:: example2

   bobsName = conn.createLiteral("Bob")
   alicesName = conn.createLiteral("Alice")

The next line prints out the number of triples currently in the
repository - we expect that to be zero, since we have not yet added
any triples and the ``connect`` function should have removed any
existing statements from the repository.

.. testcode:: example2

   print("Triple count before inserts:", conn.size())

.. testoutput:: example2

   Triple count before inserts: 0
    
Now we assert four triples, two for Bob and two more for Alice, using
the connection object's :meth:`~add` method. After
the assertions, we count triples again (there should be four) and print
out the triples for inspection.

.. testcode:: example2

   from franz.openrdf.vocabulary import RDF

   # alice is a person
   conn.add(alice, RDF.TYPE, person)
   # alice's name is "Alice"
   conn.add(alice, name, alicesName)
   # bob is a person
   conn.add(bob, RDF.TYPE, person)
   # bob's name is "Bob":
   conn.add(bob, name, bobsName)
   
   print("Triple count:", conn.size())
   for s in conn.getStatements(None, None, None, None):
       print(s) 

The ``None`` arguments to the :meth:`getStatements` method say that we
don't want to restrict what values may be present in the subject, predicate,
object or context positions. Just print out all the triples.

This is the output at this point. We see four triples, two about Alice
and two about Bob

.. testoutput:: example2

   Triple count: 4
   (<http://example.org/people/alice>, <http://www.w3.org/1999/02/22-rdf-syntax-ns#type>, <http://example.org/ontology/Person>)
   (<http://example.org/people/alice>, <http://example.org/ontology/name>, "Alice")
   (<http://example.org/people/bob>, <http://www.w3.org/1999/02/22-rdf-syntax-ns#type>, <http://example.org/ontology/Person>)
   (<http://example.org/people/bob>, <http://example.org/ontology/name>, "Bob")

We see two resources of type "person," each with a literal name.

The next step is to demonstrate how to remove a triple. Use the
:meth:`~remove` method of the connection object, and supply a triple pattern
that matches the target triple. In this case we want to remove Bob's name
triple from the repository. Then we'll count the triples again to verify
that there are only three remaining.

.. testcode:: example2

   conn.remove(bob, name, bobsName)
   print("Triple count:", conn.size())

.. testoutput:: example2

   Triple count: 3

A potentially less verbose way of adding triples is to use the
:meth:`~addData` method of the connection object with a string
containing triples in `Turtle`_, `N-Triples`_ or another RDF format.

Let us see how the data used in this example could be added using
:meth:`~addData`. We will also wrap the whole process in a function
that we'll use in further examples:

.. literalinclude:: doctest_setup.py
   :language: python
   :start-after: BEGIN-ADD-BOB-AND-ALICE
   :end-before: END-ADD-BOB-AND-ALICE


The string used here is in the `Turtle`_ format. It is also possible
to use other formats by passing the ``rdf_format`` argument to
:meth:`~addData`.
                        
We should check if the new function behaves as expected by creating a
fresh connection (recall that ``connect()`` uses the ``clear``
parameter, which causes all existing triples to be deleted):

.. testcode:: example2

   with connect() as conn:
       add_bob_and_alice(conn)
       print("Triple count:", conn.size())

.. testoutput:: example2

   Triple count: 4
