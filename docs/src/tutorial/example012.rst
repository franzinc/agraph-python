.. _example12:

Example 12: Free Text indexing
------------------------------

It is common for users to build RDF applications that combine some
form of "keyword search" with their queries. For example, a user might
want to retrieve all triples for which the string "Alice" appears as a
word within the third (object) field of the triple. AllegroGraph
provides a capability for including free text matching within a SPARQL
query, and also by using the :meth:`evalFreeTextSearch` method of the
connection object. It requires, however, that you create and configure
indexes appropriate to the searches you want to pursue.

First let's open a connection

.. literalinclude:: doctest_setup.py
   :language: python_rdf
   :start-after: BEGIN-CONNECT
   :end-before: END-CONNECT

We will start this example by importing some sample data

.. testcode:: example12

   conn.addData("""
       @prefix : <ex://> .
       
       :alice a :Person ;
                :fullname "Alice B. Toklas" .
       :book1 a :Book ;
                :title "Alice in Wonderland" ;
                :author :carroll .

       :carroll a :Person ;
                  :fullname "Lewis Carroll" .""")

We have to create an index. AllegroGraph lets you create any number of
text indexes, each for a specific purpose. In this case we are
indexing the literal values we find in the ``fullname`` predicate,
which we have used in resources that describe people. The
:meth:`createFreeTextIndex` method has many configurable
parameters. Their default settings are appropriate to this
situation. All we have to provide is a name for the index and the URI
of the predicate (or predicates) that contain the text to be indexed.

.. testcode:: example12

   fullname = conn.createURI(namespace='ex://',
                            localname='fullname')
   conn.createFreeTextIndex(
       "index1", predicates=[fullname])

We can view the index configuration using the
:meth:`getFreeTextIndexConfiguration` method:

.. testcode:: example12

   config = conn.getFreeTextIndexConfiguration("index1")
   for key, value in config.items():
       print('{key}: {value}'.format(key=key, value=value))

.. testoutput:: example12
   :options: +ELLIPSIS +SORT

   tokenizer: default
   indexLiterals: True
   minimumWordSize: 3
   indexFields: [u'object']
   stopWords: ...
   innerChars: []
   predicates: [<ex://fullname>]
   wordFilters: []
   indexResources: False
   borderChars: []

This configuration says that ``index1`` will operate on the literal
values it finds in the object position of the ``<ex://fullname>``
predicate. It ignores words smaller than three characters in
length. It will ignore the words in its ``stopWords`` list (elided
from sample output). If it encounters a resource URI in the object
position, it will ignore it. This index doesn't use any
``wordFilters``, which are sometimes used to remove accented letters
and to perform stemming on indexed text and search strings.

The text match occurs through a "magic" predicate called fti:match.
This predicate has two arguments. One is the subject URI of the
resources to search. The other is the string pattern to search for,
such as "Alice". Only full-word matches will be found.

.. testcode:: example12

   query = conn.prepareTupleQuery(query="""
       SELECT ?s WHERE {
           ?s fti:match "Alice" .
       }""")
   query.evaluate(output=True)

There is no need to include a prefix declaration for the ``fti``
namespace. That is because ``fti`` is included among the built-in
namespace mappings in AllegroGraph.

When we execute our SPARQL query, it matches the ``"Alice"`` within the literal ``"Alice B. Toklas"`` because that literal occurs in a triple having the ``fullname`` predicate, but it does not match the "Alice" in the literal ``"Alice in Wonderland"`` because the ``title`` predicate was not included in our index.

.. testoutput:: example12

   --------------
   | s          |
   ==============
   | ex://alice |
   --------------

By default ``fti:match`` searches in all text indexes. It is possible
to specify a single index name when searching. We'll illustrate this
be creating another index, this time on the ``title`` predicate:

.. testcode:: example12

   title = conn.createURI(namespace='ex://',
                          localname='title')
   conn.createFreeTextIndex(
       "index2", predicates=[title])

   query = conn.prepareTupleQuery(query="""
       SELECT ?s WHERE {
           ?s fti:match ( "Alice" "index2" ) .
       }""")
   query.evaluate(output=True)

This time only the book title will match our query

.. testoutput:: example12

   --------------
   | s          |
   ==============
   | ex://book1 |
   --------------

Another way of searching text indexes is the
:meth:`evalFreeTextSearch` method:

.. testcode:: example12

   for triple in conn.evalFreeTextSearch(
           "Alice", index="index1"):
       print(triple[0])

This works just like our first query. Note that
:meth:`evalFreeTextSearch` returns a list of lists of strings (in
N-Triples format), not a list of |Statement| objects.

.. Yay for consistency!

.. testoutput:: example12

   <ex://alice>

The text index supports simple wildcard queries. The asterisk (``*``)
may be appended to the end of the pattern to indicate "any number of
additional characters." For instance, this query looks for whole words
that begin with "Ali":

.. testcode:: example12

   for triple in conn.evalFreeTextSearch("Ali*"):
       print(triple[0])

This search runs across both indexes, so it will find both the
``:title`` and the ``:fullname`` triples.

.. testoutput:: example12
   :options: +SORT
             
   <ex://alice>
   <ex://book1>

There is also a single-character wildcard, the question mark. It will
match any single character. You can add as many question marks as you
need to the string pattern. This query looks for a five-letter word
that has "l" in the second position, and "c" in the fourth position:

.. testcode:: example12

   for triple in conn.evalFreeTextSearch("?l?c?*"):
       print(triple[0])

The result is the same as for the previous query

.. testoutput:: example12
   :options: +SORT

   <ex://alice>
   <ex://book1>

Text indexes are not the only way of matching text values available in
SPARQL. One may also filter results using regular expressions. This
approach is more flexible, but at the price of performance. Regular
expression filters do not use any form of indexing to speed up the
query.

.. testcode:: example12

   query = conn.prepareTupleQuery(query="""
       SELECT ?s ?p ?o WHERE {
           ?s ?p ?o .
           FILTER regex(?o, "lic|oll")
       }""")
   query.evaluate(output=True)

Note how this search matches the provided pattern inside words.

.. testoutput:: example12
   :options: +SORT

   ------------------------------------------------------
   | s            | p             | o                   |
   ======================================================
   | ex://carroll | ex://fullname | Lewis Carroll       |
   | ex://book1   | ex://title    | Alice in Wonderland |
   | ex://alice   | ex://fullname | Alice B. Toklas     |
   ------------------------------------------------------

In addition to indexing literal values, AllegroGraph can also index
resource URIs. ``index3`` is an index that looks for URIs in the
object position of the ``author`` predicate, and then indexes only the
local name of the resource (the characters following the rightmost
``/``, ``#`` or ``:`` in the URI). This lets us avoid indexing
highly-repetitive namespace strings, which would fill the index with
data that would not be very useful.

.. testcode:: example12

   author = conn.createURI(namespace='ex://',
                           localname='author')

   conn.createFreeTextIndex(
       "index3", predicates=[author],
       indexResources="short", indexFields=["object"])

   for triple in conn.evalFreeTextSearch("carroll",
                                         index="index3"):
       print(triple[0])

The text search located the triple that has ``carroll`` in the URI in
the object position:

.. testoutput:: example12

   <ex://book1>
