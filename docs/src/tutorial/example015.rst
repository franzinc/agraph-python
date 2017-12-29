.. _example15:

Example 15: Range queries
-------------------------

In many of the previous examples we have used the
:meth:`getStatements` method to find all triples conforming to a given
pattern. The patterns we have used so far matched each triple
component against a single value. It is possible to use more complex
patterns that can match a range of values for each component. To
illustrate this let us first create a connection:

.. literalinclude:: doctest_setup.py
   :language: python
   :start-after: BEGIN-CONNECT
   :end-before: END-CONNECT

and construct some data:

.. testcode:: example15

   conn.addData("""
       @prefix : <ex://> .

       :mercury a :planet ; :moons 0 .
       :venus a :planet ; :moons 0 .
       :earth a :planet ; :moons 1 .
       :mars a :planet ; :moons 2 .
       :jupiter a :planet ; :moons 67 .
       :saturn a :planet ; :moons 62 .
       :uranus a :planet ; :moons 27 .
       :neptune a :planet ; :moons 14 .
       :pluto a :dwarf_planet ; :moons 5 .
   """)

Suppose that we want to locate all planets that have at least one, but
no more than five moons. To issue such a query we need to create a
:class:`.Range` object:

.. testcode:: example15

   one_to_five = conn.createRange(1, 5)

We can pass the range object to :meth:`getStatements`:

.. testcode:: example15

   moons = conn.createURI('ex://moons')
   with conn.getStatements(
           None, moons, one_to_five) as result:
       for statement in result:
           print(statement.getSubject())

This will find two planets and one dwarf planet, as expected:

.. testoutput:: example15
   :options: +SORT

   <ex://earth>
   <ex://mars>
   <ex://pluto>

The arguments to :meth:`createRange` can be either RDF terms or
regular Python values that will be converted to typed :class:`literals
<Literal>`. In our example we used have used values of type ``int``,
which will be mapped to literals of type
``<http://www.w3.org/2001/XMLSchema#integer>``. Range queries will
only match values of exactly the same type. For instance if we add
another triple to our store:

.. testcode:: example15

   conn.addData("""
       @prefix : <ex://> .
       @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
       
       :coruscant a :planet ; :moons "4"^^xsd:long .
   """)

And then reissue our query:

.. testcode:: example15

   with conn.getStatements(
           None, moons, one_to_five) as result:
       for statement in result:
           print(statement.getSubject())

we will find that the result has not changed:
           
.. testoutput:: example15
   :options: +SORT

   <ex://earth>
   <ex://mars>
   <ex://pluto>

Range queries can also be performed with SPARQL, using ``FILTER``:

.. testcode:: example15

   conn.executeTupleQuery('''
       SELECT ?planet {
           ?planet <ex://moons> ?moons .
           filter (?moons <= 5 && ?moons >= 1)
       }''', output=True)

The result is the same as in the previous example.

.. testoutput:: example15
   :options: +SORT

   ------------------
   | planet         |
   ==================
   | ex://coruscant |
   | ex://earth     |
   | ex://mars      |
   | ex://pluto     |
   ------------------


When the filter expression is a simple set of inequalities, as it is
in this case, the query engine will use indices to optimize the query
execution, similaraly to the way :meth:`getStatements` does for range
queries.
