.. _example5:

Example 5: Literal values
-------------------------

The next example illustrates some variations on what we have seen so
far. The example creates and asserts plain, data-typed, and
language-tagged literals, and then conducts searches for them in three
ways:

   - :meth:`getStatements` search, which is an efficient way to match a
     single triple pattern.
   - SPARQL direct match, for efficient multi-pattern search.
   - SPARQL filter match, for sophisticated filtering such as
     performing range matches.

The :meth:`getStatements` and SPARQL direct searches return exactly
the datatype you ask for. The SPARQL filter queries can sometimes
return multiple datatypes. This behavior will be one focus of this
section.

If you are not explicit about the datatype of a value, either when
asserting the triple or when writing a search pattern, AllegroGraph
will deduce an appropriate datatype and use it. This is another focus
of this section. This helpful behavior can sometimes surprise you with
unanticipated results.

Setup
~~~~~

We begin by obtaining a connection object and remvoing all existing data
from the repository

.. literalinclude:: doctest_setup.py
   :language: python_rdf
   :start-after: BEGIN-CONNECT
   :end-before: END-CONNECT

For the sake of coding efficiency, it is good practice to create variables
for namespace strings. We'll use this namespace again and again in the
following example. We have made the URIs in this example very short to
keep the result displays compact.

.. testcode:: example5

   exns = "ex://"
   conn.setNamespace('ex', exns)

Namespace handling, including the :meth:`setNamespace` method, is
described in :ref:`example11`.

The example will use an artificial data set consisting of eight
statements, each illustrating a different kind of literal. The subject
will describe the nature of the literal used as the object, while the
predicate will always be ``<ex://p>``. The example shows how to enter
a full URI string, or alternately how to combine a namespace with a
local resource name.

.. testcode:: example5

   ex_integer = conn.createURI("ex://integer")
   ex_double = conn.createURI("ex://double")
   ex_int = conn.createURI("ex://int")
   ex_long = conn.createURI(
       namespace=exns, localname="long")
   ex_float = conn.createURI(
       namespace=exns, localname="float")
   ex_decimal = conn.createURI(
       namespace=exns, localname="decimal")
   ex_string = conn.createURI(
       namespace=exns, localname="string")
   ex_plain = conn.createURI(
       namespace=exns, localname="plain")

The predicate for all our statements will be the same.

.. testcode:: example5

   pred = conn.createURI(namespace=exns, localname="p")

Now we construct the objects, illustrating various kinds of RDF
literals.

.. testcode:: example5

   from franz.openrdf.vocabulary.xmlschema import XMLSchema

   # Type will be XMLSchema.INTEGER
   forty_two = conn.createLiteral(42)
   # Type will be XMLSchema.DOUBLE
   forty_two_double = conn.createLiteral(42.0)
   forty_two_int = conn.createLiteral(
       '42', datatype=XMLSchema.INT)
   forty_two_long = conn.createLiteral(
       '42', datatype=XMLSchema.LONG)
   forty_two_float = conn.createLiteral(
       '42', datatype=XMLSchema.FLOAT)
   forty_two_decimal = conn.createLiteral(
       '42', datatype=XMLSchema.DECIMAL)
   forty_two_string = conn.createLiteral(
       '42', datatype=XMLSchema.STRING)
   # Creates a plain (untyped) literal.
   forty_two_plain = conn.createLiteral('42')

In four of these statements, we explicitly identified the datatype of
the value in order to create an INT, a LONG, a FLOAT and a
STRING. This is the best practice.

In three other statements, we just handed AllegroGraph numeric-looking
values to see what it would do with them. As we will see in a moment,
``42`` creates an INTEGER, ``42.0`` becomes a DOUBLE, and ``'42'``
becomes a "plain" (untyped) literal value.

.. warning:: Note that plain literals are not *quite* the same thing
             as typed literal strings. A search for a plain literal
             will not always match a typed string, and *vice versa*.)

Now we will now assemble the URIs and values into :class:`statements
<.Statement>` (which are client-side triples):

.. testcode:: example5

   stmt1 = conn.createStatement(ex_integer, pred, forty_two)
   stmt2 = conn.createStatement(ex_double, pred, forty_two_double)
   stmt3 = conn.createStatement(ex_int, pred, forty_two_int)
   stmt4 = conn.createStatement(ex_long, pred, forty_two_long)
   stmt5 = conn.createStatement(ex_float, pred, forty_two_float)
   stmt6 = conn.createStatement(ex_decimal, pred, forty_two_decimal)
   stmt7 = conn.createStatement(ex_string, pred, forty_two_string)
   stmt8 = conn.createStatement(ex_plain, pred, forty_two_plain)

And then add the statements to the triple store on the AllegroGraph
server. We can use either :meth:`add` or :meth:`addStatement` for this
purpose.

.. testcode:: example5

   conn.add(stmt1)
   conn.add(stmt2)
   conn.add(stmt3)
   conn.addStatement(stmt4)
   conn.addStatement(stmt5)
   conn.addStatement(stmt6)
   conn.addStatement(stmt7)
   conn.addStatement(stmt8)

Now we'll complete the round trip to see what triples we get back from
these assertions. This is where we use :meth:`getStatements` in this
example to retrieve and display triples for us:

.. testcode:: example5

   print("Showing all triples using getStatements(). Eight matches.")
   conn.getStatements(None, pred, None, output=True,)

This code prints out all triples from the store. The ``output``
parameter causes the result to be printed on ``stdout`` (it is also
possible to pass a file name or a file-like object as the value of
this parameter to print to other destinations). Without ``output`` the
result would have been returned as a |RepositoryResult| object.

Note that the retrieved literals are of eight types: an int (a 32-bit
integer), an integer (arbitrary precision), a decimal, a long, a
float, a double, a string, and a "plain literal."

.. testoutput:: example5
   :options: +SORT +NORMALIZE_WHITESPACE

   Showing all triples using getStatements(). Eight matches.
   <ex://plain> <ex://p> "42" .
   <ex://string> <ex://p> "42"^^<http://www.w3.org/2001/XMLSchema#string> .
   <ex://decimal> <ex://p> "42.0"^^<http://www.w3.org/2001/XMLSchema#decimal> .
   <ex://float> <ex://p> "4.2E1"^^<http://www.w3.org/2001/XMLSchema#float> .
   <ex://long> <ex://p> "42"^^<http://www.w3.org/2001/XMLSchema#long> .
   <ex://int> <ex://p> "42"^^<http://www.w3.org/2001/XMLSchema#int> .
   <ex://double> <ex://p> "4.2E1"^^<http://www.w3.org/2001/XMLSchema#double> .
   <ex://integer> <ex://p> "42"^^<http://www.w3.org/2001/XMLSchema#integer> .

If you ask for a specific datatype, you will get it. If you leave the
decision up to AllegroGraph, you might get something unexpected such as
a plain literal value.

Numeric literal values
~~~~~~~~~~~~~~~~~~~~~~

Matching 42 without explicit type
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This section explores :meth:`getStatements` and SPARQL matches against
numeric triples. We ask AllegroGraph to find an untyped number,
``42``.

.. testcode:: example5

   print('getStatements():')
   conn.getStatements(None, pred, 42, output=True)
   print()

   print('SPARQL direct match')
   conn.executeTupleQuery(
       'SELECT ?s WHERE {?s ?p 42 .}',
       output=True)
   print()

   print('SPARQL filter match')
   conn.executeTupleQuery(
       'SELECT ?s ?p ?o WHERE {?s ?p ?o . filter (?o = 42)}',
       output=True)
   print()

We use the :meth:`executeQuery` method to retrieve the result of a
SPARQL query. Like :meth:`getStatements`, it accepts an ``output``
parameter that causes the result to be printed (instead of being
returned as a |TupleQueryResult| object).  Here is what the query
methods discussed in this example would return:

.. Format of decimals has changed in 6.3.0, we need the doctests
   to work with both versions.

.. condtestoutput:: example5
   :options: +SORT +NORMALIZE_WHITESPACE
   :if: ag_version >= (6, 3, 0)

   getStatements():
   <ex://integer> <ex://p> "42"^^<http://www.w3.org/2001/XMLSchema#integer> .

   SPARQL direct match
   --------------
   | s          |
   ==============
   | ex:integer |
   --------------

   SPARQL filter match
   -----------------------------
   | s          | p    | o     |
   =============================
   | ex:integer | ex:p | 42    |
   | ex:double  | ex:p | 4.2E1 |
   | ex:int     | ex:p | 42    |
   | ex:long    | ex:p | 42    |
   | ex:float   | ex:p | 4.2E1 |
   | ex:decimal | ex:p | 42.0  |
   -----------------------------

.. condtestoutput:: example5
   :options: +SORT +NORMALIZE_WHITESPACE
   :if: ag_version < (6, 3, 0)
             
   getStatements():
   <ex://integer> <ex://p> "42"^^<http://www.w3.org/2001/XMLSchema#integer> .

   SPARQL direct match
   --------------
   | s          |
   ==============
   | ex:integer |
   --------------

   SPARQL filter match
   ------------------------------
   | s          | p    | o      |
   ==============================
   | ex:decimal | ex:p | 42.0   |
   | ex:integer | ex:p | 42     |
   | ex:float   | ex:p | 42.0   |
   | ex:double  | ex:p | 42.0d0 |
   | ex:long    | ex:p | 42     |
   | ex:int     | ex:p | 42     |
   ------------------------------
   
The :meth:`getStatements` query returned triples containing longs
only. The SPARQL direct match treated the numeric literal as if it had
the type of ``<http://www.w3.org/2001/XMLSchema#integer>`` (see the
SPARQL `specification
<https://www.w3.org/TR/sparql11-query/#QSynLiterals>`__ for
information on how literals are parsed in queries) and returned only
triples with exactly the same type. The SPARQL filter match, however,
opened the doors to matches of multiple numeric types, and returned
ints, floats, longs and doubles.


Matching 42.0 without explicit type
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Now we will try the same queries using ``42.0``.

.. testcode:: example5

   print('getStatements():')
   conn.getStatements(None, pred, 42.0, output=True)
   print()

   print('SPARQL direct match')
   conn.executeTupleQuery(
       'SELECT ?s WHERE {?s <ex://p> 42.0 .}',
       output=True)
   print()

   print('SPARQL filter match')
   conn.executeTupleQuery(
       'SELECT ?s ?p ?o WHERE {?s ?p ?o . filter (?o = 42.0)}',
       output=True)
   print()

Here is what the query methods discussed in this example would
return:

.. condtestoutput:: example5
   :if: ag_version >= (6, 3, 0)
   :options: +SORT +NORMALIZE_WHITESPACE

   getStatements():
   <ex://double> <ex://p> "4.2E1"^^<http://www.w3.org/2001/XMLSchema#double> .

   SPARQL direct match
   --------------
   | s          |
   ==============
   | ex:decimal |
   --------------

   SPARQL filter match
   -----------------------------
   | s          | p    | o     |
   =============================
   | ex:integer | ex:p | 42    |
   | ex:double  | ex:p | 4.2E1 |
   | ex:int     | ex:p | 42    |
   | ex:long    | ex:p | 42    |
   | ex:float   | ex:p | 4.2E1 |
   | ex:decimal | ex:p | 42.0  |
   -----------------------------

.. condtestoutput:: example5
   :if: ag_version < (6, 3, 0)
   :options: +SORT +NORMALIZE_WHITESPACE

   getStatements():
   <ex://double> <ex://p> "4.2E1"^^<http://www.w3.org/2001/XMLSchema#double> .

   SPARQL direct match
   --------------
   | s          |
   ==============
   | ex:decimal |
   --------------

   SPARQL filter match
   ------------------------------
   | s          | p    | o      |
   ==============================
   | ex:decimal | ex:p | 42.0   |
   | ex:integer | ex:p | 42     |
   | ex:float   | ex:p | 42.0   |
   | ex:double  | ex:p | 42.0d0 |
   | ex:long    | ex:p | 42     |
   | ex:int     | ex:p | 42     |
   ------------------------------

The :meth:`getStatements` search returned a double but not the similar
float. Direct SPARQL match treated ``42.0`` as a decimal (in
accordance with the SPARQL specification). The filter match returned
all numeric types that were equal to 42.0.

Matching "42"^^xsd:int
^^^^^^^^^^^^^^^^^^^^^^

The next section shows the results obtained when querying for a
literal with explicitly specified type. Note that doing this with
:meth:`getStatements` requires passing in a :class:`.Literal` object,
not a raw value.

.. testcode:: example5

   print('getStatements():')
   conn.getStatements(None, pred, forty_two_int, output=True)
   print()

   print('SPARQL direct match')
   conn.executeTupleQuery(
       'SELECT ?s WHERE {?s ?p "42"^^xsd:int .}',
       output=True)
   print()

   print('SPARQL filter match')
   conn.executeTupleQuery('''
       SELECT ?s ?p ?o WHERE {
          ?s ?p ?o .
          filter (?o = "42"^^xsd:int)
       }''',
       output=True)
   print()

Here is what the query methods discussed in this example would
return:

.. condtestoutput:: example5
   :if: ag_version >= (6, 3, 0)
   :options: +SORT +NORMALIZE_WHITESPACE

   getStatements():
   <ex://int> <ex://p> "42"^^<http://www.w3.org/2001/XMLSchema#int> .

   SPARQL direct match
   ----------
   | s      |
   ==========
   | ex:int |
   ----------

   SPARQL filter match
   -----------------------------
   | s          | p    | o     |
   =============================
   | ex:integer | ex:p | 42    |
   | ex:double  | ex:p | 4.2E1 |
   | ex:int     | ex:p | 42    |
   | ex:long    | ex:p | 42    |
   | ex:float   | ex:p | 4.2E1 |
   | ex:decimal | ex:p | 42.0  |
   -----------------------------
   
.. condtestoutput:: example5
   :if: ag_version < (6, 3, 0)
   :options: +SORT +NORMALIZE_WHITESPACE

   getStatements():
   <ex://int> <ex://p> "42"^^<http://www.w3.org/2001/XMLSchema#int> .

   SPARQL direct match
   ----------
   | s      |
   ==========
   | ex:int |
   ----------

   SPARQL filter match
   ------------------------------
   | s          | p    | o      |
   ==============================
   | ex:decimal | ex:p | 42.0   |
   | ex:integer | ex:p | 42     |
   | ex:float   | ex:p | 42.0   |
   | ex:double  | ex:p | 42.0d0 |
   | ex:long    | ex:p | 42     |
   | ex:int     | ex:p | 42     |
   ------------------------------
      
We would get similar results when asking for any other typed literal
(``forty_two_long``, ``forty_two_float``, ...).

Numeric strings and plain literals
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

At this point we are transitioning from tests of numeric matches to
tests of string matches, but there is a gray zone to be explored
first. What do we find if we search for strings that contain numbers?
In particular, what about "plain literal" values that are almost, but
not quite, strings?

Matching "42" as a typed string
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Let's start with a typed string literal.

.. testcode:: example5

   print('getStatements():')
   conn.getStatements(None, pred, forty_two_string, output=True)
   print()

   print('SPARQL direct match')
   conn.executeTupleQuery(
       'SELECT ?s WHERE {?s ?p "42"^^xsd:string .}',
       output=True)
   print()

   print('SPARQL filter match')
   conn.executeTupleQuery('''
       SELECT ?s ?p ?o WHERE {
          ?s ?p ?o .
          filter (?o = "42"^^xsd:string)
       }''',
       output=True)
   print()

Here are the results:

.. testoutput:: example5
   :options: +SORT +NORMALIZE_WHITESPACE

    getStatements():
    <ex://string> <ex://p> "42"^^<http://www.w3.org/2001/XMLSchema#string> .

    SPARQL direct match
    -------------
    | s         |
    =============
    | ex:plain  |
    | ex:string |
    -------------

    SPARQL filter match
    ------------------------------------------------------------------
    | s         | p    | o                                           |
    ==================================================================
    | ex:string | ex:p | 42^^http://www.w3.org/2001/XMLSchema#string |
    | ex:plain  | ex:p | 42                                          |
    ------------------------------------------------------------------

SPARQL matched both plain and literal strings, but a
:meth:`getStatements` search returned only typed matches. In both
cases numeric literals were ignored.

Matching "42" as a plain literal
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If we try to match a plain (untyped) string value

.. testcode:: example5

   print('getStatements():')
   conn.getStatements(None, pred, forty_two_plain, output=True)
   print()

   print('SPARQL direct match')
   conn.executeTupleQuery(
       'SELECT ?s WHERE {?s ?p "42" .}',
       output=True)
   print()

   print('SPARQL filter match')
   conn.executeTupleQuery('''
       SELECT ?s ?p ?o WHERE {
          ?s ?p ?o .
          filter (?o = "42")
       }''',
       output=True)
   print()

We will get results consistent with that we saw in the typed case:

.. testoutput:: example5
   :options: +SORT +NORMALIZE_WHITESPACE

    getStatements():
    <ex://plain> <ex://p> "42" .

    SPARQL direct match
    -------------
    | s         |
    =============
    | ex:plain  |
    | ex:string |
    -------------

    SPARQL filter match
    ------------------------------------------------------------------
    | s         | p    | o                                           |
    ==================================================================
    | ex:string | ex:p | 42^^http://www.w3.org/2001/XMLSchema#string |
    | ex:plain  | ex:p | 42                                          |
    ------------------------------------------------------------------

In SPARQL both kinds of string literals were matched, while
:meth:`getStatements` returned only direct matches.

Matching strings
~~~~~~~~~~~~~~~~

In this section we'll set up a variety of string triples and
experiment with matching them using :meth:`getStatements` and SPARQL.

.. note:: :ref:`example12` is a different topic. In this section we're
          doing simple matches of whole strings.

Sample data
^^^^^^^^^^^

For these examples we will use a different data set.

.. testcode:: example5

   name = conn.createURI('ex://name')
   upper_g = conn.createLiteral('Galadriel')
   lower_g = conn.createLiteral('galadriel')
   typed_g = conn.createLiteral('Galadriel', XMLSchema.STRING)
   lang_g = conn.createLiteral('Galadriel', language='sjn')
   upper_a = conn.createLiteral('Artanis')
   lower_a = conn.createLiteral('artanis')
   typed_a = conn.createLiteral('Artanis', XMLSchema.STRING)
   lang_a = conn.createLiteral('Artanis', language='qya')
   conn.addTriple('<ex://upper_g>', name, upper_g)
   conn.addTriple('<ex://lower_g>', name, lower_g)
   conn.addTriple('<ex://typed_g>', name, typed_g)
   conn.addTriple('<ex://lang_g>', name, lang_g)
   conn.addTriple('<ex://upper_a>', name, upper_a)
   conn.addTriple('<ex://lower_a>', name, lower_a)
   conn.addTriple('<ex://typed_a>', name, typed_a)
   conn.addTriple('<ex://lang_a>', name, lang_a)

We have two literals, each in four variants:

   - Upper case (plain literal)
   - Lower case (plain literal)
   - Typed
   - Tagged with a `BCP47`_ language tag appropriate for its language
     (Quenya or Sindarin) according to the the `registry`_

Matching a plain string
^^^^^^^^^^^^^^^^^^^^^^^

We've seen a similar case when looking at matches for ``"42"``, but
this time we have more similar literals in the store.

.. testcode:: example5

   print('getStatements():')
   conn.getStatements(None, name, upper_g, output=True)
   print()

   print('SPARQL direct match')
   conn.executeTupleQuery(
       'SELECT ?s WHERE {?s <ex://name> "Galadriel" .}',
       output=True)
   print()

   print('SPARQL filter match')
   conn.executeTupleQuery('''
       SELECT ?s ?o WHERE {
          ?s <ex://name> ?o .
          filter (?o = "Galadriel")
       }''',
       output=True)
   print()

Here's the result:

.. testoutput:: example5
   :options: +SORT +NORMALIZE_WHITESPACE

   getStatements():
   <ex://upper_g> <ex://name> "Galadriel" .

   SPARQL direct match
   --------------
   | s          |
   ==============
   | ex:typed_g |
   | ex:upper_g |
   --------------

   SPARQL filter match
   -------------------------------------------------------------------
   | s          | o                                                  |
   ===================================================================
   | ex:typed_g | Galadriel^^http://www.w3.org/2001/XMLSchema#string |
   | ex:upper_g | Galadriel                                          |
   -------------------------------------------------------------------

We can see that the match is case-sensitive and ignores the
language-tagged literal in all cases. As usual :meth:`getStatements`
matches only the exact kind of literal that we've provided, while
SPARQL is more liberal.

Matching a language-tagged string
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

To retrieve the language-tagged variant we can ask for it explicitly:

.. testcode:: example5

   print('getStatements():')
   conn.getStatements(None, name, lang_g, output=True)
   print()

   print('SPARQL direct match')
   conn.executeTupleQuery(
       'SELECT ?s WHERE {?s <ex://name> "Galadriel"@sjn .}',
       output=True)
   print()

   print('SPARQL filter match')
   conn.executeTupleQuery('''
       SELECT ?s ?o WHERE {
          ?s <ex://name> ?o .
          filter (?o = "Galadriel"@sjn)
       }''',
       output=True)
   print()

Unsurprisingly we get exactly what we have asked for

.. testoutput:: example5
   :options: +SORT +NORMALIZE_WHITESPACE

   getStatements():
   <ex://lang_g> <ex://name> "Galadriel"@sjn .

   SPARQL direct match
   -------------
   | s         |
   =============
   | ex:lang_g |
   -------------

   SPARQL filter match
   -----------------------------
   | s         | o             |
   =============================
   | ex:lang_g | Galadriel@sjn |
   -----------------------------

You may be wondering how to perform a string match where language and
capitalization don't matter. You can do that with a SPARQL filter
query using the ``str()`` function, which strips out the string
portion of a literal, leaving behind the datatype or language
tag. Then the ``fn:lower-case()`` function eliminates case issues:

.. testcode:: example5

   conn.executeTupleQuery('''
       SELECT ?s ?o WHERE {
          ?s <ex://name> ?o .
          filter (fn:lower-case(str(?o)) = "artanis")
       }''',
       output=True)

This query returns all variants of the selected literal

.. testoutput:: example5
   :options: +SORT +NORMALIZE_WHITESPACE

   -----------------------------------------------------------------
   | s          | o                                                |
   =================================================================
   | ex:lang_a  | Artanis@qya                                      |
   | ex:typed_a | Artanis^^http://www.w3.org/2001/XMLSchema#string |
   | ex:lower_a | artanis                                          |
   | ex:upper_a | Artanis                                          |
   -----------------------------------------------------------------

Remember that the SPARQL ``filter`` queries are powerful, but they are
also the slowest queries. SPARQL direct queries and getStatements()
queries are faster.

Booleans
~~~~~~~~

Boolean values in SPARQL are represented by literals of type
``<http://www.w3.org/2001/XMLSchema#boolean>``. There are two ways to
create such literals in Python:

   1. From corresponding Python boolean values (``True`` and ``FAlse``):

      .. testcode:: example5

         true1 = conn.createLiteral(True)
         false1 = conn.createLiteral(False)

   2. By creating a typed literal with the value of ``"true"`` or
      ``"false"``. The type must be ``xsd:boolean``:

      .. testcode:: example5

         true2 = conn.createLiteral("true", datatype=XMLSchema.BOOLEAN)
         false2 = conn.createLiteral("false", datatype=XMLSchema.BOOLEAN)

Both ways of creating boolean literals produce equivalent results:

.. testcode:: example5

   print(true1)
   print(true2)

As we can see the literals are identical.

.. testoutput:: example5

   "true"^^<http://www.w3.org/2001/XMLSchema#boolean>
   "true"^^<http://www.w3.org/2001/XMLSchema#boolean>

Let us add some boolean data to the store:

.. testcode:: example5

   conn.addData("""
       <ex://f> <ex://p>
           "false"^^<http://www.w3.org/2001/XMLSchema#boolean> .
       # In Turtle 'true' is the same as '"true"^^xsd:boolean"'
       <ex://t> <ex://p> true .
   """)

When querying for boolean values using SPARQL one can use the literals
``true`` and ``false`` as a shorthand for
``"true"^^<http://www.w3.org/2001/XMLSchema#boolean>`` and
``"false"^^<http://www.w3.org/2001/XMLSchema#boolean>``. The code
below illustrates various ways of querying for boolean values:

.. testcode:: example5

   print('getStatements():')
   conn.getStatements(None, None, true1, output=True)
   print()

   print('SPARQL direct match (true)')
   conn.executeTupleQuery(
       'SELECT ?s WHERE {?s ?p true.}',
       output=True)
   print()

   print('SPARQL direct match ("false"^^xsd:boolean)')
   conn.executeTupleQuery(
       'SELECT ?s WHERE {?s ?p "false"^^xsd:boolean .}',
       output=True)
   print()

   print('SPARQL filter match ("false"^^xsd:boolean)')
   conn.executeTupleQuery('''
       SELECT ?s ?o WHERE {
          ?s ?p ?o .
          filter (?o = "false"^^xsd:boolean)
       }''',
       output=True)
   print()

Here's the output from that script:

.. testoutput:: example5

   getStatements():
   <ex://t> <ex://p> "true"^^<http://www.w3.org/2001/XMLSchema#boolean> .

   SPARQL direct match (true)
   --------
   | s    |
   ========
   | ex:t |
   --------

   SPARQL direct match ("false"^^xsd:boolean)
   --------
   | s    |
   ========
   | ex:f |
   --------

   SPARQL filter match ("false"^^xsd:boolean)
   ----------------
   | s    | o     |
   ================
   | ex:f | false |
   ----------------

Dates and times
~~~~~~~~~~~~~~~

SPARQL represents dates and times using three literal types:
``xsd:date``, ``xsd:time`` and ``xsd:dateTime``. These can be created
either explicitly from strings in the `ISO 8601`_ format or from
Python ``datetime.date``, ``datetime.time`` and ``datetime.datetime``
values.

Let's create a few sample literals:

.. testcode:: example5

   from datetime import date, time, datetime
   import iso8601

   d = conn.createLiteral(date(1944, 8, 1))
   t = conn.createLiteral(time(15, 0, 0))
   dt = conn.createLiteral('1944-08-01T17:00:00+02:00',
                           datatype=XMLSchema.DATETIME)

Creating ``time`` and ``datetime`` literals from Python values can
yield somewhat unexpected results if time zones are involved:

.. testcode:: example5

   surprise = conn.createLiteral(iso8601.parse_date(
       '1944-08-01T17:00:00+02:00'))
   # Should be the same...
   print(dt)
   print(surprise)

The output is

.. testoutput:: example5

   "1944-08-01T17:00:00+02:00"^^<http://www.w3.org/2001/XMLSchema#dateTime>
   "1944-08-01T15:00:00Z"^^<http://www.w3.org/2001/XMLSchema#dateTime>

The time has been converted to UTC. While both ``dt`` and ``surprise``
refer to the same moment in time, this conversion might still lead to
problems if the user is not aware that it takes place.

We will now add the newly created literals to the store:

.. testcode:: example5

   conn.addTriple('<ex://d>', '<ex://p>', d)
   conn.addTriple('<ex://t>', '<ex://p>', t)
   conn.addTriple('<ex://dt>', '<ex://p>', dt)

The following sections illustrate how date and time values behave
during queries.

Matching dates
^^^^^^^^^^^^^^
Let's try the usual mix of query methods and see what is returned:

.. testcode:: example5

   print('getStatements():')
   conn.getStatements(None, None, d, output=True)
   print()

   print('SPARQL direct match')
   conn.executeTupleQuery(
       'SELECT ?s WHERE {?s ?p "1944-08-01"^^xsd:date .}',
       output=True)
   print()

   print('SPARQL filter match')
   conn.executeTupleQuery('''
       SELECT ?s ?o WHERE {
          ?s ?p ?o .
          filter (?o = "1944-08-01"^^xsd:date)
       }''',
       output=True)
   print()

The result is not surprising. It is worth noting that the ``datetime``
value has not been returned, even though it refers to the same date.

.. testoutput:: example5
   :options: +NORMALIZE_WHITESPACE

    getStatements():
    <ex://d> <ex://p> "1944-08-01"^^<http://www.w3.org/2001/XMLSchema#date> .

    SPARQL direct match
    --------
    | s    |
    ========
    | ex:d |
    --------

    SPARQL filter match
    ---------------------
    | s    | o          |
    =====================
    | ex:d | 1944-08-01 |
    ---------------------


Matching times
^^^^^^^^^^^^^^

Times can be queried in a similar fashion.

.. testcode:: example5

   print('getStatements():')
   conn.getStatements(None, None, t, output=True)
   print()

   print('SPARQL direct match')
   conn.executeTupleQuery(
       'SELECT ?s WHERE {?s ?p "15:00:00Z"^^xsd:time .}',
       output=True)
   print()

   print('SPARQL filter match')
   conn.executeTupleQuery('''
       SELECT ?s ?o WHERE {
          ?s ?p ?o .
          filter (?o = "15:00:00Z"^^xsd:time)
       }''',
       output=True)
   print()

Again, only the value of the appropriate type is returned.

.. testoutput:: example5

   getStatements():
   <ex://t> <ex://p> "15:00:00Z"^^<http://www.w3.org/2001/XMLSchema#time> .

   SPARQL direct match
   --------
   | s    |
   ========
   | ex:t |
   --------

   SPARQL filter match
   --------------------
   | s    | o         |
   ====================
   | ex:t | 15:00:00Z |
   --------------------

Matching datetimes
^^^^^^^^^^^^^^^^^^

Datetimes work just like times and dates:

.. testcode:: example5

   print('getStatements():')
   conn.getStatements(None, None, dt, output=True)
   print()

   print('SPARQL direct match')
   conn.executeTupleQuery('''
       SELECT ?s WHERE {
          ?s ?p "1944-08-01T17:00:00+02:00"^^xsd:dateTime .
       }''',
       output=True)
   print()

   print('SPARQL filter match')
   conn.executeTupleQuery('''
       SELECT ?s ?o WHERE {
          ?s ?p ?o .
          filter (?o = "1944-08-01T17:00:00+02:00"^^xsd:dateTime)
       }''',
       output=True)
   print()

The result:

.. testoutput:: example5

   getStatements():
   <ex://dt> <ex://p> "1944-08-01T17:00:00+02:00"^^<http://www.w3.org/2001/XMLSchema#dateTime> .

   SPARQL direct match
   ---------
   | s     |
   =========
   | ex:dt |
   ---------

   SPARQL filter match
   -------------------------------------
   | s     | o                         |
   =====================================
   | ex:dt | 1944-08-01T17:00:00+02:00 |
   -------------------------------------


Matching datetimes with offsets
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We saw that times created from Python values are converted to UTC. So
what happens when we query for Zulu time, while the value in the store
is still in CEST?

.. testcode:: example5

   zulu = conn.createLiteral("1944-08-01T15:00:00Z",
                             datatype=XMLSchema.DATETIME)
   print('getStatements():')
   conn.getStatements(None, None, zulu, output=True)
   print()

   print('SPARQL direct match')
   conn.executeTupleQuery('''
       SELECT ?s WHERE {
          ?s ?p "1944-08-01T15:00:00Z"^^xsd:dateTime .
       }''',
       output=True)
   print()

   print('SPARQL filter match')
   conn.executeTupleQuery('''
       SELECT ?s ?o WHERE {
          ?s ?p ?o .
          filter (?o = "1944-08-01T15:00:00Z"^^xsd:dateTime)
       }''',
       output=True)
   print()

AllegroGraph still finds our value when using SPARQL

.. testoutput:: example5

   getStatements():

   SPARQL direct match
   ---------
   | s     |
   =========
   | ex:dt |
   ---------

   SPARQL filter match
   -------------------------------------
   | s     | o                         |
   =====================================
   | ex:dt | 1944-08-01T17:00:00+02:00 |
   -------------------------------------


When evaluating SPARQL queries AllegroGraph treats ``datetime``
objects that refer to the same point in time as equivalent, regardless
of the timezone used in their representation. :meth:`getStatements`
performs exact matching, so will not return a value with different
timezone.

.. _BCP47: https://tools.ietf.org/html/bcp47
.. _registry: https://www.iana.org/assignments/language-subtag-registry/language-subtag-registry
.. _ISO 8601: https://en.wikipedia.org/wiki/ISO_8601
