.. _example20:

Example 20: Reasoning
---------------------

AllegroGraph supports the following RDFS and OWL predicates:

- ``rdf:type``;
- ``rdfs:domain``;
- ``rdfs:range``;
- ``rdfs:subClassOf``;
- ``rdfs:subPropertyOf``;
- ``owl:inverseOf``;
- ``owl:sameAs``;
- ``owl:SymmetricProperty``;
- ``owl:TransitiveProperty``.

A more detailed description of reasoning support can be found in the
`Reasoner Tutorial
<https://franz.com/agraph/support/documentation/current/reasoner-tutorial.html>`_
chapter of AllegroGraph LISP documentation and is not repeated here
for brevity. This tutorial only contains Python setup and querying
examples.


Setup
~~~~~

.. currentmodule:: franz.openrdf.sail.spec

In order to enable reasoning, a connection to the server must be
constructed by passing repository spec to a :func:`reason` function
and creating a session from the resulting spec: ::

   conn = server.openSession(reason(repo))

This connection must be used instead of a regular connection as an
entry-point to execute queries on inferred statements. Here is a
complete example of a setup that enables RDFS++ reasoning over a
regular repository ``repo``: ::

   from franz.openrdf.sail.allegrographserver import AllegroGraphServer
   from franz.openrdf.sail.spec import reason

   server = AllegroGraphServer(host='localhost', port=10035, user='test', password='xyzzy')

   # Create repository 'repo' in the root catalog.
   server.openCatalog().createRepository('repo')

   # Open session with reasoning enabled on repository 'repo'.
   conn = server.openSession(reason('<repo>'))


Reasoner examples
~~~~~~~~~~~~~~~~~

The examples below assume that you already have an RDFS++ repository
created as described in the `Setup <#setup>`_ section. Each example
removes all of the triples in the store, adds some new triples, and
makes some queries to illustrate the different sorts of reasoning that
AllegroGraph supports.

In order to demonstrate the results, we will use the following
function to print triples matching a simple pattern: ::

    def ptl(conn, s, p, o):
        """
        Get statements matching subject, predicate and object,
        which can be either entities represented by strings
        of the form '<prefix>:<name>' or wildcard None values.
        """
        namespaces = {
            'rdf': conn.namespace('http://www.w3.org/1999/02/22-rdf-syntax-ns#'),
            'ex': conn.namespace('ex://')
        }

        def part_to_uri(part):
            # Split part string on ':' and get a full URI from a corresponding
            # namespace object:
            if part:
                prefix, name = part.split(':', 2)
                return getattr(namespaces[prefix], name)
            else:
                return None

        us, up, uo = (part_to_uri(part) for part in (s, p, o))
        for rs, rp, ro, _ in conn.getStatements(us, up, uo):
            print("{} {} {}.".format(rs, rp, ro))


.. currentmodule:: franz.openrdf.repository.repositoryconnection.RepositoryConnection

It is also possible to ask for inferred statements without creating a
reasoning session by setting the value of the ``includeInferred``
argument to :meth:`getStatements` to ``True`` as in the following
example: ::

   conn.getStatements(s, p, o, includeInferred=True)


``inverseOf``
^^^^^^^^^^^^^

::

   >>> conn.addData("""
   ...   @prefix owl: <http://www.w3.org/2002/07/owl#>.
   ...   @prefix ex: <ex://>.
   ...
   ...   ex:Jans    ex:owns       ex:Birra.
   ...   ex:ownedBy owl:inverseOf ex:owns.
   ...   ex:has     owl:inverseOf ex:ownedBy.
   ... """)

   >>> ptl(conn, "ex:Birra", "ex:ownedBy", None)
   <ex://Birra> <ex://ownedBy> <ex://Jans>.

   >>> ptl(conn, None, "ex:ownedBy", None)
   <ex://Birra> <ex://ownedBy> <ex://Jans>.

   >>> ptl(conn, None, "ex:ownedBy", "ex:Jans")
   <ex://Birra> <ex://ownedBy> <ex://Jans>.

   >>> ptl(conn, "ex:Jans", "ex:has", None)
   <ex://Jans> <ex://has> <ex://Birra>.

   >>> ptl(conn, None, "ex:has", None)
   <ex://Jans> <ex://has> <ex://Birra>.

   >>> ptl(conn, None, "ex:has", "ex:Birra")
   <ex://Jans> <ex://has> <ex://Birra>.


``subPropertyOf``
^^^^^^^^^^^^^^^^^

::

   >>> conn.addData("""
   ...   @prefix owl: <http://www.w3.org/2002/07/owl#>.
   ...   @prefix ex: <ex://>.
   ...   @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>.
   ...
   ...   ex:Jans   ex:hasPet          ex:Birra.
   ...   ex:Birra  ex:friendOf        ex:Samira.
   ...   ex:hasPet rdfs:subPropertyOf ex:owns.
   ... """)

   >>> ptl(conn, "ex:Jans", "ex:owns", "ex:Birra")
   <ex://Jans> <ex://owns> <ex://Birra>.

   >>> ptl(conn, "ex:Jans", "ex:owns", None)
   <ex://Jans> <ex://owns> <ex://Birra>.

   >>> ptl(conn, None, "ex:owns", "ex:Birra")
   <ex://Jans> <ex://owns> <ex://Birra>.

   >>> ptl(conn, "ex:Jans", "ex:hasPet", "ex:Birra")
   <ex://Jans> <ex://hasPet> <ex://Birra>.

   >>> ptl(conn, "ex:Jans", "ex:hasPet", None)
   <ex://Jans> <ex://hasPet> <ex://Birra>.

   >>> ptl(conn, None, "ex:hasPet", "ex:Birra")
   <ex://Jans> <ex://hasPet> <ex://Birra>.



``inverseOf`` with ``subPropertyOf``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

::

    >>> conn.addData("""
    ...   @prefix owl: <http://www.w3.org/2002/07/owl#>.
    ...   @prefix ex: <ex://>.
    ...   @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>.
    ...
    ...   ex:Jans    ex:hasPet          ex:Birra.
    ...   ex:ownedBy owl:inverseOf      ex:owns.
    ...   ex:has     owl:inverseOf      ex:ownedBy.
    ...   ex:hasPet  rdfs:subPropertyOf ex:owns.
    ...   ex:petOf   owl:inverseOf      ex:hasPet.
    ... """)

    # direct triples
    >>> ptl(conn, "ex:Jans", "ex:hasPet", "ex:Birra")
    <ex://Jans> <ex://hasPet> <ex://Birra>.

    >>> ptl(conn, None, "ex:hasPet", "ex:Birra")
    <ex://Jans> <ex://hasPet> <ex://Birra>.

    >>> ptl(conn, "ex:Jans", "ex:hasPet", None)
    <ex://Jans> <ex://hasPet> <ex://Birra>.

    # inverse of ex:hasPet
    >>> ptl(conn, "ex:Birra", "ex:petOf", "ex:Jans")
    <ex://Birra> <ex://petOf> <ex://Jans>.

    >>> ptl(conn, None, "ex:petOf", "ex:Jans")
    <ex://Birra> <ex://petOf> <ex://Jans>.

    >>> ptl(conn, "ex:Birra", "ex:petOf", None)
    <ex://Birra> <ex://petOf> <ex://Jans>.

    # subproperty
    >>> ptl(conn, "ex:Jans", "ex:owns", "ex:Birra")
    <ex://Jans> <ex://owns> <ex://Birra>.

    >>> ptl(conn, "ex:Jans", "ex:owns", None)
    <ex://Jans> <ex://owns> <ex://Birra>.

    >>> ptl(conn, None, "ex:owns", "ex:Birra")
    <ex://Jans> <ex://owns> <ex://Birra>.

    # inverse of subproperty
    >>> ptl(conn, "ex:Birra", "ex:ownedBy", "ex:Jans")
    <ex://Birra> <ex://ownedBy> <ex://Jans>.

    >>> ptl(conn, None, "ex:ownedBy", "ex:Jans")
    <ex://Birra> <ex://ownedBy> <ex://Jans>.

    >>> ptl(conn, "ex:Birra", "ex:ownedBy", None)
    <ex://Birra> <ex://ownedBy> <ex://Jans>.

    # inverse of inverse
    >>> ptl(conn, "ex:Jans", "ex:has", "ex:Birra")
    <ex://Jans> <ex://has> <ex://Birra>.

    >>> ptl(conn, None, "ex:has", "ex:Birra")
    <ex://Jans> <ex://has> <ex://Birra>.

    >>> ptl(conn, "ex:Jans", "ex:has", None)
    <ex://Jans> <ex://has> <ex://Birra>.



``sameAs``
^^^^^^^^^^

::

    >>> conn.addData("""
      @prefix owl: <http://www.w3.org/2002/07/owl#>.
      @prefix ex: <ex://>.
      @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>.

      ex:Jans   ex:owns    ex:Birra.
      ex:Jans   owl:sameAs ex:Jannes.
      ex:Aasman owl:sameAs ex:Jannes.
      ex:Birra  owl:sameAs ex:SonOfSamira.
    """)

    >>> ptl(conn, "ex:Aasman", "ex:owns", "ex:SonOfSamira")
    <ex://Aasman> <ex://owns> <ex://SonOfSamira>.

    >>> ptl(conn, "ex:Aasman", "ex:owns", None)
    <ex://Aasman> <ex://owns> <ex://Birra>.
    <ex://Aasman> <ex://owns> <ex://SonOfSamira>.

    >>> ptl(conn, None, "ex:owns", "ex:SonOfSamira")
    <ex://Jans> <ex://owns> <ex://SonOfSamira>.
    <ex://Jannes> <ex://owns> <ex://SonOfSamira>.
    <ex://Aasman> <ex://owns> <ex://SonOfSamira>.

    >>> ptl(conn, None, "ex:owns", None)
    <ex://Jans> <ex://owns> <ex://Birra>.
    <ex://Aasman> <ex://owns> <ex://Birra>.
    <ex://Aasman> <ex://owns> <ex://SonOfSamira>.
    <ex://Jannes> <ex://owns> <ex://SonOfSamira>.
    <ex://Jannes> <ex://owns> <ex://Birra>.
    <ex://Jans> <ex://owns> <ex://SonOfSamira>.


``sameAs`` with ``inverseOf`` and ``subPropertyOf``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

::

    conn.addData("""
      @prefix owl: <http://www.w3.org/2002/07/owl#>.
      @prefix ex: <ex://>.
      @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>.

      ex:Jans    ex:hasPet ex:Birra.
      ex:ownedBy owl:inverseOf ex:owns.
      ex:has     owl:inverseOf ex:ownedBy.
      ex:hasPet  rdfs:subPropertyOf ex:owns.
      ex:petOf   owl:inverseOf ex:hasPet.
      ex:Birra   ex:age ex:twelve.

      ex:Jans   owl:sameAs ex:Jannes.
      ex:Aasman owl:sameAs ex:Jannes.
      ex:Birra  owl:sameAs ex:SonOfSamira.
    """)

    # direct triples
    >>> ptl(conn, "ex:Aasman", "ex:hasPet", "ex:SonOfSamira")
    <ex://Aasman> <ex://hasPet> <ex://SonOfSamira>.

    >>> ptl(conn, None, "ex:hasPet", "ex:SonOfSamira")
    <ex://Aasman> <ex://hasPet> <ex://SonOfSamira>.
    <ex://Jannes> <ex://hasPet> <ex://SonOfSamira>.
    <ex://Jans> <ex://hasPet> <ex://SonOfSamira>.

    >>> ptl(conn, "ex:Aasman", "ex:hasPet", None)
    <ex://Aasman> <ex://hasPet> <ex://Birra>.
    <ex://Aasman> <ex://hasPet> <ex://SonOfSamira>.

    # inverse of 'owns'
    >>> ptl(conn, "ex:SonOfSamira", "ex:petOf", "ex:Aasman")
    <ex://SonOfSamira> <ex://petOf> <ex://Aasman>.

    >>> ptl(conn, None, "ex:petOf", "ex:Aasman")
    <ex://SonOfSamira> <ex://petOf> <ex://Aasman>.
    <ex://Birra> <ex://petOf> <ex://Aasman>.

    >>> ptl(conn, "ex:SonOfSamira", "ex:petOf", None)
    <ex://SonOfSamira> <ex://petOf> <ex://Jannes>.
    <ex://SonOfSamira> <ex://petOf> <ex://Aasman>.
    <ex://SonOfSamira> <ex://petOf> <ex://Jans>.

    # inverse of inverse
    >>> ptl(conn, "ex:Aasman", "ex:has", "ex:SonOfSamira")
    <ex://Aasman> <ex://has> <ex://SonOfSamira>.

    >>> ptl(conn, None, "ex:has", "ex:SonOfSamira")
    <ex://Jannes> <ex://has> <ex://SonOfSamira>.
    <ex://Aasman> <ex://has> <ex://SonOfSamira>.
    <ex://Jans> <ex://has> <ex://SonOfSamira>.

    >>> ptl(conn, "ex:Aasman", "ex:has", None)
    <ex://Aasman> <ex://has> <ex://Birra>.
    <ex://Aasman> <ex://has> <ex://SonOfSamira>.

    # subproperty
    >>> ptl(conn, "ex:Aasman", "ex:owns", "ex:SonOfSamira")
    <ex://Aasman> <ex://owns> <ex://SonOfSamira>.

    >>> ptl(conn, "ex:Aasman", "ex:owns", None)
    <ex://Aasman> <ex://owns> <ex://SonOfSamira>.
    <ex://Aasman> <ex://owns> <ex://Birra>.

    >>> ptl(conn, None, "ex:owns", "ex:SonOfSamira")
    <ex://Aasman> <ex://owns> <ex://SonOfSamira>.
    <ex://Jans> <ex://owns> <ex://SonOfSamira>.
    <ex://Jannes> <ex://owns> <ex://SonOfSamira>.

    # inverse of subproperty
    >>> ptl(conn, "ex:SonOfSamira", "ex:ownedBy", "ex:Aasman")
    <ex://SonOfSamira> <ex://ownedBy> <ex://Aasman>.

    >>> ptl(conn, None, "ex:ownedBy", "ex:Aasman")
    <ex://SonOfSamira> <ex://ownedBy> <ex://Aasman>.
    <ex://Birra> <ex://ownedBy> <ex://Aasman>.

    >>> ptl(conn, "ex:SonOfSamira", "ex:ownedBy", None)
    <ex://SonOfSamira> <ex://ownedBy> <ex://Aasman>.
    <ex://SonOfSamira> <ex://ownedBy> <ex://Jans>.
    <ex://SonOfSamira> <ex://ownedBy> <ex://Jannes>.


``type`` with ``subClassOf``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

::

    >>> conn.addData("""
      @prefix owl: <http://www.w3.org/2002/07/owl#>.
      @prefix ex: <ex://>.
      @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>.
      @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>.

      ex:Mammal rdfs:subClassOf ex:Animal.
      ex:Human  rdfs:subClassOf ex:Mammal.
      ex:Man    rdfs:subClassOf ex:Human.
      ex:Jans   rdf:type        ex:Man.
      ex:Jans   owl:sameAs      ex:Jannes.
      ex:Aasman owl:sameAs      ex:Jannes.
    """)

    >>> ptl(conn, "ex:Jans", "rdf:type", "ex:Man")
    <ex://Jans> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <ex://Man>.

    >>> ptl(conn, "ex:Jans", "rdf:type", "ex:Human")
    <ex://Jans> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <ex://Human>.

    >>> ptl(conn, "ex:Jans", "rdf:type", None)
    <ex://Jans> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <ex://Man>.
    <ex://Jans> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <ex://Animal>.
    <ex://Jans> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <ex://Human>.
    <ex://Jans> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <ex://Mammal>.

    >>> ptl(conn, "ex:Aasman", "rdf:type", "ex:Man")
    <ex://Aasman> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <ex://Man>.

    >>> ptl(conn, "ex:Aasman", "rdf:type", "ex:Human")
    <ex://Aasman> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <ex://Human>.

    >>> ptl(conn, "ex:Aasman", "rdf:type", None)
    <ex://Aasman> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <ex://Animal>.
    <ex://Aasman> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <ex://Human>.
    <ex://Aasman> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <ex://Mammal>.
    <ex://Aasman> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <ex://Man>.

    >>> ptl(conn, None, "rdf:type", "ex:Man")
    <ex://Jans> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <ex://Man>.

    >>> ptl(conn, None, "rdf:type", "ex:Human")
    <ex://Aasman> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <ex://Human>.
    <ex://Jannes> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <ex://Human>.
    <ex://Jans> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <ex://Human>.

    >>> ptl(conn, None, "rdf:type", None)
    <ex://Jans> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <ex://Man>.
    <ex://Aasman> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <ex://Human>.
    <ex://Jannes> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <ex://Human>.
    <ex://Jans> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <ex://Human>.
    <ex://Aasman> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <ex://Mammal>.
    <ex://Jannes> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <ex://Mammal>.
    <ex://Jans> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <ex://Mammal>.
    <ex://Aasman> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <ex://Animal>.
    <ex://Jannes> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <ex://Animal>.
    <ex://Jans> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <ex://Animal>.


``type`` with ``range``
^^^^^^^^^^^^^^^^^^^^^^^

::

    >>> conn.addData("""
      @prefix owl: <http://www.w3.org/2002/07/owl#>.
      @prefix ex: <ex://>.
      @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>.

      ex:Jans   ex:hasPet      ex:Birra.
      ex:hasPet rdfs:range      ex:Pet.
      ex:Pet    rdfs:subClassOf ex:Mammal.
      ex:Fatcat owl:sameAs      ex:Birra.
    """)

    >>> ptl(conn, "ex:Birra", "rdf:type", "ex:Pet")
    <ex://Birra> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <ex://Pet>.

    >>> ptl(conn, "ex:Birra", "rdf:type", None)
    <ex://Birra> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <ex://Pet>.
    <ex://Birra> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <ex://Mammal>.

    >>> ptl(conn, None, "rdf:type", "ex:Pet")
    <ex://Fatcat> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <ex://Pet>.
    <ex://Birra> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <ex://Pet>.

    >>> ptl(conn, "ex:Birra", "rdf:type", "ex:Mammal")
    <ex://Birra> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <ex://Mammal>.

    >>> ptl(conn, "ex:Fatcat", "rdf:type", "ex:Mammal")
    <ex://Fatcat> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <ex://Mammal>.


``type`` with ``domain``
^^^^^^^^^^^^^^^^^^^^^^^^

::

    >>> conn.addData("""
      @prefix owl: <http://www.w3.org/2002/07/owl#>.
      @prefix ex: <ex://>.
      @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>.

      ex:Jans   ex:hasPet      ex:Birra.
      ex:hasPet rdfs:domain     ex:Human.
      ex:Human  rdfs:subClassOf ex:Mammal.
      ex:Jans   owl:sameAs      ex:Aasman.
    """)

    >>> ptl(conn, "ex:Jans", "rdf:type", "ex:Human")
    <ex://Jans> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <ex://Human>.

    >>> ptl(conn, "ex:Jans", "rdf:type", None)
    <ex://Jans> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <ex://Human>.
    <ex://Jans> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <ex://Mammal>.

    >>> ptl(conn, None, "rdf:type", "ex:Human")
    <ex://Aasman> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <ex://Human>.
    <ex://Jans> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <ex://Human>.

    >>> ptl(conn, None, "rdf:type", None)
    <ex://Aasman> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <ex://Mammal>.
    <ex://Jans> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <ex://Mammal>.
    <ex://Aasman> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <ex://Human>.
    <ex://Jans> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <ex://Human>.


Transitivity with ``sameAs``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

::

    >>> conn.addData("""
      @prefix owl: <http://www.w3.org/2002/07/owl#>.
      @prefix ex: <ex://>.
      @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>.

      ex:contains    rdf:type    owl:TransitiveProperty.
      ex:USA         ex:contains ex:California.
      ex:GoldenState ex:contains ex:ContraCosta.
      ex:ContraCosta ex:contains ex:Moraga.

      ex:USA        owl:sameAs ex:UncleSam.
      ex:Moraga     owl:sameAs ex:MyTown.
      ex:California owl:sameAs ex:GoldenState.
    """)

    >>> ptl(conn, "ex:USA", "ex:contains", "ex:Moraga")
    <ex://USA> <ex://contains> <ex://Moraga>.

    >>> ptl(conn, "ex:UncleSam", "ex:contains", "ex:MyTown")
    <ex://UncleSam> <ex://contains> <ex://MyTown>.

    >>> ptl(conn, "ex:GoldenState", "ex:contains", "ex:Moraga")
    <ex://GoldenState> <ex://contains> <ex://Moraga>.

    >>> ptl(conn, "ex:California", "ex:contains", "ex:Moraga")
    <ex://California> <ex://contains> <ex://Moraga>.

    >>> ptl(conn, "ex:California", "ex:contains", "ex:MyTown")
    <ex://California> <ex://contains> <ex://MyTown>.

    >>> ptl(conn, "ex:USA", "ex:contains", None)
    <ex://USA> <ex://contains> <ex://California>.
    <ex://USA> <ex://contains> <ex://MyTown>.
    <ex://USA> <ex://contains> <ex://GoldenState>.
    <ex://USA> <ex://contains> <ex://ContraCosta>.
    <ex://USA> <ex://contains> <ex://Moraga>.

    >>> ptl(conn, "ex:UncleSam", "ex:contains", None)
    <ex://UncleSam> <ex://contains> <ex://GoldenState>.
    <ex://UncleSam> <ex://contains> <ex://California>.
    <ex://UncleSam> <ex://contains> <ex://ContraCosta>.
    <ex://UncleSam> <ex://contains> <ex://MyTown>.
    <ex://UncleSam> <ex://contains> <ex://Moraga>.

    >>> ptl(conn, None, "ex:contains", "ex:Moraga")
    <ex://ContraCosta> <ex://contains> <ex://Moraga>.
    <ex://California> <ex://contains> <ex://Moraga>.
    <ex://UncleSam> <ex://contains> <ex://Moraga>.
    <ex://GoldenState> <ex://contains> <ex://Moraga>.
    <ex://USA> <ex://contains> <ex://Moraga>.

    >>> ptl(conn, None, "ex:contains", "ex:MyTown")
    <ex://GoldenState> <ex://contains> <ex://MyTown>.
    <ex://USA> <ex://contains> <ex://MyTown>.
    <ex://ContraCosta> <ex://contains> <ex://MyTown>.
    <ex://UncleSam> <ex://contains> <ex://MyTown>.
    <ex://California> <ex://contains> <ex://MyTown>.
