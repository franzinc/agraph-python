.. _example8:

Example 8: Exporting triples
----------------------------

This example shows how to serialize contents of a repository to a
file. As usual we'll start with obtaining a connection to the
repository:

.. testcode:: example8

   conn = connect()

Now let's import some data:

.. testcode:: example8

   conn.addData("""
     <ex://s> <ex://p1> <ex://o1> , <ex://o2> ;
              <ex://p2> <ex://o3> .""")

Data can be exported by passing a file name or a file-like object as
the ``output`` parameter of :meth:`getStatements`. In this case we'll
want to print all statements to standard output. We can do this by
passign ``True`` as the output file:

.. testcode:: example8

   from franz.openrdf.rio.rdfformat import RDFFormat

   conn.getStatements(output=True, output_format=RDFFormat.NTRIPLES)

We can see that results are printed in the specified format:
   
.. testoutput:: example8
   :options: +SORT
                  
   <ex://s> <ex://p1> <ex://o1> .
   <ex://s> <ex://p1> <ex://o2> .
   <ex://s> <ex://p2> <ex://o3> .     

We can also use other arguments of :meth:`getStatements` to constrain
the set of exported tripes:

.. testcode:: example8

   conn.getStatements(None, conn.createURI('ex://p1'), None,
                      output=True,
                      output_format=RDFFormat.NTRIPLES)

As expected, the result contains only two triples.
                      
.. testoutput:: example8
   :options: +SORT

   <ex://s> <ex://p1> <ex://o1> .
   <ex://s> <ex://p1> <ex://o2> .

A file path can also be passed as the ``output`` argument:

.. testcode:: example8

   import os
   import sys
   
   conn.getStatements(output='example8.nt')
   with open('example8.nt', 'r') as f:
       sys.stdout.write(f.read())

   os.remove('example8.nt')

This outputs data read from the file:
   
.. testoutput:: example8
   :options: +SORT
                
   <ex://s> <ex://p2> <ex://o3> .
   <ex://s> <ex://p1> <ex://o2> .  
   <ex://s> <ex://p1> <ex://o1> .     
