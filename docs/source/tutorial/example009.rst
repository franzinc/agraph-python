.. _example9:

Example 9: Exporting query results
----------------------------------

The :ref:`previous example <example8>` showed how to serialize
statements to a file or a stream. It is also possible to perform a
similar operation on the result of a query.

As usual, we'll start by opening a connection and importing sample
data - in this case containing birth and (when applicable) coronation
dates of the sons of Henry II.

.. testcode:: example9

   conn = connect()
   conn.addData("""
     @prefix : <ex://> .
     @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

     :Henry :born "1155-02-28"^^xsd:date .
     :Richard :born "1157-09-08"^^xsd:date .
     :Geoffrey :born "1158-09-23"^^xsd:date .
     :John :born "1166-12-24"^^xsd:date .

     :Henry :crowned "1170-06-14"^^xsd:date .  # sort of...
     :Richard :crowned "1189-09-03"^^xsd:date .
     :John :crowned "1199-05-27"^^xsd:date .""")

Query results can be exported by passing a file name or a file-like
object as the ``output`` parameter of the :meth:`.TupleQuery.evaluate`
method of the query object. In this case we'll want to print all kings
born in or after 1156 from our dataset to standard output (we can use
``True`` as the file name to indicate stdout):

.. testcode:: example9

   from franz.openrdf.query.query import QueryLanguage
   from franz.openrdf.rio.tupleformat import TupleFormat

   query = conn.prepareTupleQuery(
       QueryLanguage.SPARQL,
       """
       select ?name ?crowned {
          ?name <ex://born> ?birth .
          ?name <ex://crowned> ?crowned .
          filter(?birth >= "1156-01-01"^^xsd:date) .
       }""")
   query.evaluate(output=True,
                  output_format=TupleFormat.CSV)
       

We can see that results are printed in the specified format:
   
.. testoutput:: example9
   :options: +SORT +NORMALIZE_WHITESPACE

   name,crowned
   "ex://Richard","1189-09-03"
   "ex://John","1199-05-27"

We can export the result of a ``CREATE`` or ``DESCRIBE`` query in a
similar fashion. The difference is that we need to supply an
:class:`.RDFFormat` instead of a :class:`.TupleFormat`, since the
result is a set of triples.

.. testcode:: example9

   from franz.openrdf.rio.rdfformat import RDFFormat

   query = conn.prepareGraphQuery(
       QueryLanguage.SPARQL, "describe <ex://Richard> where {}")
   query.evaluate(output=True,
                  output_format=RDFFormat.NTRIPLES)
                  
As expected, the result contains two triples:
                      
.. testoutput:: example9
   :options: +SORT +NORMALIZE_WHITESPACE

   <ex://Richard> <ex://born> "1157-09-08"^^<http://www.w3.org/2001/XMLSchema#date> .
   <ex://Richard> <ex://crowned> "1189-09-03"^^<http://www.w3.org/2001/XMLSchema#date> .

A file path can also be passed as the ``output`` argument:

.. testcode:: example9

   import os
   import sys
   
   query = conn.prepareTupleQuery(
       QueryLanguage.SPARQL,
       """
       select ?name ?birth ?coronation {
         ?name <ex://born> ?birth ;
               <ex://crowned> ?coronation .
       }""")
   query.evaluate(output='example9.csv',
                  output_format=TupleFormat.CSV)
   with open('example9.csv', 'r') as f:
       sys.stdout.write(f.read())

   os.remove('example9.csv')

This outputs data read from the file:
   
.. testoutput:: example9
   :options: +SORT +NORMALIZE_WHITESPACE

   name,birth,coronation
   "ex://Henry","1155-02-28","1170-06-14"
   "ex://Richard","1157-09-08","1189-09-03"
   "ex://John","1166-12-24","1199-05-27"


