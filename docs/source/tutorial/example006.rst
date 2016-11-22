.. _example6:

Example 6: Importing triples
----------------------------

AllegroGraph can import files in multiple RDF :class:`formats
<.RDFFormat>`, such as `Turtle`_ or `N-Triples`_. The example below
calls the connection object's :meth:`add` method to load an N-Triples
file, and :meth:`addFile` to load an RDF/XML file. Both methods work,
but the best practice is to use :meth:`addFile`.

.. note::

   If you get a 'file not found' error while executing this example, make sure that the `DATA_DIR` setting (described in the :ref:`setup` section of this tutorial).

The RDF/XML file contains a short list of v-cards (virtual business cards), like this one:

.. code-block:: xml

   <rdf:Description rdf:about="http://somewhere/JohnSmith/">
     <vCard:FN>John Smith</vCard:FN>
     <vCard:N rdf:parseType="Resource">
       <vCard:Family>Smith</vCard:Family>
       <vCard:Given>John</vCard:Given>
     </vCard:N>
   </rdf:Description>
  
The N-Triples file contains a graph of resources describing the
Kennedy family, the places where they were each born, their colleges,
and their professions. A typical entry from that file looks like this:

.. code-block:: text

   <http://www.franz.com/simple#person1> <http://www.franz.com/simple#first-name> "Joseph" . 
   <http://www.franz.com/simple#person1> <http://www.franz.com/simple#middle-initial> "Patrick" . 
   <http://www.franz.com/simple#person1> <http://www.franz.com/simple#last-name> "Kennedy" . 
   <http://www.franz.com/simple#person1> <http://www.franz.com/simple#suffix> "none" . 
   <http://www.franz.com/simple#person1> <http://www.franz.com/simple#alma-mater> <http://www.franz.com/simple#Harvard> . 
   <http://www.franz.com/simple#person1> <http://www.franz.com/simple#birth-year> "1888" . 
   <http://www.franz.com/simple#person1> <http://www.franz.com/simple#death-year> "1969" . 
   <http://www.franz.com/simple#person1> <http://www.franz.com/simple#sex> <http://www.franz.com/simple#male> . 
   <http://www.franz.com/simple#person1> <http://www.franz.com/simple#spouse> <http://www.franz.com/simple#person2> . 
   <http://www.franz.com/simple#person1> <http://www.franz.com/simple#has-child> <http://www.franz.com/simple#person3> . 
   <http://www.franz.com/simple#person1> <http://www.franz.com/simple#profession> <http://www.franz.com/simple#banker> . 
   <http://www.franz.com/simple#person1> <http://www.franz.com/simple#birth-place> <http://www.franz.com/simple#place5> . 
   <http://www.franz.com/simple#person1> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.franz.com/simple#person> . 

Note that AllegroGraph can segregate triples into contexts (subgraphs)
by treating them as quads, but the N-Triples and RDF/XML formats
cannot include context information (unlike e.g `N-Quads`_ or
`Trig`_). They deal with triples only, so there is no place to store a
fourth field in those formats. In the case of the :meth:`add` call, we
have omitted the context argument so the triples are loaded into the
default graph (sometimes called the "null context.") The
:meth:`addFile` call includes an explicit context setting, so the
fourth field of each VCard triple will be the context named
``http://example.org#vcards``. The connection :meth:`size` method
takes an optional context argument. With no argument, it returns the
total number of triples in the repository. Below, it returns the
number ``16`` for the ``context`` context argument, and the number
``28`` for the null context (``None``) argument.

.. testcode:: example6

   from franz.openrdf.rio.rdfformat import RDFFormat
   import os.path

   conn = connect()
   
The variables ``path1`` and ``path2`` are bound to the RDF/XML and
N-Triples files, respectively.

.. testcode:: example6

   path1 = os.path.join(DATA_DIR, 'vcards.rdf')    
   path2 = os.path.join(DATA_DIR, 'kennedy.ntriples')

The triples about the VCards will be added to a specific context, so
naturally we need a URI to identify that context.

.. testcode:: example6
   
   context = conn.createURI("http://example.org#vcards")

In the next step we use :meth:`addFile` to load the VCard triples into
the ``#vcards`` context:

.. testcode:: example6

   conn.addFile(path1, None, format=RDFFormat.RDFXML, context=context)
   
Then we use :meth:`add` to load the Kennedy family tree into the
default context:

.. testcode:: example6

   conn.add(path2, base=None, format=RDFFormat.NTRIPLES, contexts=None)

Now we'll ask AllegroGraph to report on how many triples it sees in
the default context and in the `#vcards` context:
   
.. testcode:: example6

   print('VCard triples (in {context}): {count}'.format(
         count=conn.size(context), context=context))
   
   print('Kennedy triples (default graph): {count}'.format(
         count=conn.size('null')))

The output of this report was:
         
.. testoutput:: example6

   VCard triples (in <http://example.org#vcards>): 16
   Kennedy triples (default graph): 1214

         
