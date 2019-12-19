.. _example16:

Example 16: Federated repositories
----------------------------------

AllegroGraph lets you split up your triples among repositories on
multiple servers and then search them all in parallel. To do this we
query a single "federated" repository that automatically distributes
the queries to the secondary repositories and combines the
results. From the point of view of your Python code, it looks like you
are working with a single repository.

To illustrate this, let us first create two repositories and import
some data. The data will represent positive numbers below 15. The
first repository will contain all Fibonacci numbers in that range,
while the second one will contain all other numbers.

.. testcode:: example16

   from franz.openrdf.connect import ag_connect

   with ag_connect('python_fib', create=True, clear=True) as conn:
       conn.addData("""
           @prefix : <ex://> .

           :one :value 1 .
           :two :value 2 .
           :three :value 3 .
           :five :value 5 .
           :eight :value 8 .
           :thirteen :value 13 .
       """)  
   
   with ag_connect('python_boring', create=True, clear=True) as conn:
       conn.addData("""
           @prefix : <ex://> .

           :four :value 4 .
           :six :value 6 .
           :seven :value 7 .
           :nine :value 9 .
           :ten :value 10 .
           :eleven :value 11 .
           :twelve :value 12 .
           :fourteen :value 14 .
           :fifteen :value 15 .
       """)              

To create a federated repository, we first have to connect to the
server that will be used to aggregate results. We do this by creating
an :class:`.AllegroGraphServer` instance.

.. testcode:: example16

   from franz.openrdf.sail.allegrographserver import AllegroGraphServer
              
   server = AllegroGraphServer()

We are using default server address and credentials, as described in
the :ref:`setup` section of the tutorial.

The next step is to use the :meth:`~.AllegroGraphServer.openFederated`
method to create a federated session. We will pass the list of
repositories to federate as an argument. Elements of this list could
be

   * :class:`.Repository` objects
   * :class:`.RepositoryConnection` objects
   * strings (naming a store in the root catalog, or the URL of a
     store)
   * (storename, catalogname) tuples.

We'll use the third option

.. testcode:: example16

   conn = server.openFederated(['python_fib', 'python_boring'])

Now we can query the combined repository.

.. testcode:: example16

   query = conn.prepareTupleQuery(query="""
       select (avg(?v) as ?avg)
              (min(?v) as ?min)
              (max(?v) as ?max) where {
          ?number <ex://value> ?v .
       }""")
   query.evaluate(output=True)

As we can see, data from both repositories has been returned and
aggregates have been correctly computed over the whole dataset.

.. testoutput:: example16

   -------------------
   | avg | min | max |
   ===================
   | 8.0 | 1   | 15  |
   -------------------

Another example of using federated repositories, this time with
multiple server machines, can be found in :ref:`fedex`.

Finally we'll delete the example repositories:

.. testcode:: example16

   root_catalog = server.openCatalog()
   for repo in ['python_fib', 'python_boring']:
       root_catalog.deleteRepository(repo)
