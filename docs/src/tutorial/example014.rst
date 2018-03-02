.. _example14:

Example 14: Parametric queries
------------------------------

In previous examples our SPARQL queries were always fixed strings. In
practice it is often necessary to include some variable elements
(e.g. user input, results from another query, ...) in the query
strings.

To illustrate, let us create a connection

.. literalinclude:: doctest_setup.py
   :language: python_rdf
   :start-after: BEGIN-CONNECT
   :end-before: END-CONNECT

and populate the repository with sample data:

.. testcode:: example14

   conn.addData(r"""
     @prefix : <ex://> .

     :cipher42 :label "RSA" ; :code 1 .
     :hedge1 :label "\\/\\/\\/\\/\\/\\/\\" ; :code 2 .
     :hedge2 :label "/\\/\\/\\/\\/\\/\\//" .  # No code
     :has_no_label :secret "squeamish ossifrage" .
   """)

Suppose that we need a function that will take a search text and find
all subjects that have a label containing a given search pattern. To
make the query a little more interesting we'll also print the value of
the `:code` predicate for all subjects found.
   
.. testcode:: example14

   conn.setNamespace('', 'ex://')
   query = conn.prepareTupleQuery(query="""
       SELECT ?s ?code WHERE {
          ?s :label ?o .
          FILTER contains(?o, ?search)
          OPTIONAL { ?s <ex://code> ?code } .
       }""")
   
   def print_labelled_subjects(search_text):
      query.setBinding('search', search_text)    
      query.evaluate(output=True)

We have created a query object with a variable (``?search``) in place of the search pattern. To use it we have to provide a value for the variable. We do that using the :meth:`.setBinding` method.

Let's check if our function works as expected:

.. testcode:: example14

   print_labelled_subjects(r'\/\/')

This will return the 'hedge' subjects:

.. testoutput:: example14
   :options: +SORT              

   ------------------
   | s       | code |
   ==================
   | :hedge1 | 2    |
   | :hedge2 | ---  |
   ------------------

String formatting
~~~~~~~~~~~~~~~~~

Another way to achieve our goal would be to use formatting or string
concatenation, like this:

.. testcode:: example14

   import sys

   conn.setNamespace('', 'ex://')
   def print_labelled_subjects(search_text):
       query = conn.prepareTupleQuery(query="""
           SELECT ?s ?code WHERE {
               ?s :label ?o .
               FILTER contains(?o, "%s")
               OPTIONAL { ?s <ex://code> ?code } .
           }""" % search_text)
       query.evaluate(output=True)

   print_labelled_subjects('RS')

This seems to work

.. testoutput:: example14

   --------------------
   | s         | code |
   ====================
   | :cipher42 | 1    |
   --------------------
 
But attempting to use a trickier input reveals a problem:

.. testcode:: example14

   print_labelled_subjects(r'\/\/')
 
The query is now invalid

.. testoutput:: example14
   :options: +ELLIPSIS
                
   Traceback (most recent call last):
     ...
   RequestError: Server returned 400: ...

A `devious user <https://xkcd.com/327/>`_ could take advantage of this
bug to access data that is not supposed to be available

.. testcode:: example14

   print_labelled_subjects(
       r'S") optional { ?x <ex://secret> ?code } # ')

It should not be possible to reveal this literal by searching labels,
and yet:
       
.. testoutput:: example14
   
   -----------------------------------
   | s         | code                |
   ===================================
   | :cipher42 | squeamish ossifrage |
   -----------------------------------
                
We can work around this by ensuring proper escaping:

.. testcode:: example14

   def print_labelled_subjects(search_text):
       search_lit = conn.createLiteral(search_text)
       query = conn.prepareTupleQuery(query="""
           SELECT ?s ?code WHERE {
               ?s :label ?o .
               FILTER contains(?o, %s)
               OPTIONAL { ?s <ex://code> ?code } . 
           }""" % search_lit.toNTriples())
       query.evaluate(output=True)

   print_labelled_subjects(r'\/\/')

The function now works as expected:
   
.. testoutput:: example14
   :options: +SORT
 
   ------------------
   | s       | code |
   ==================
   | :hedge1 | 2    |
   | :hedge2 | ---  |
   ------------------
