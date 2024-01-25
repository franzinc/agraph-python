# pylint: disable-msg=C0103

################################################################################
# Copyright (c) 2006-2017 Franz Inc.
# All rights reserved. This program and the accompanying materials are
# made available under the terms of the MIT License which accompanies
# this distribution, and is available at http://opensource.org/licenses/MIT
################################################################################
import re

from franz.openrdf.exceptions import IllegalArgumentException
from franz.openrdf.model import URI, ValueFactory
from franz.openrdf.repository.repositoryconnection import RepositoryConnection
from franz.openrdf.vocabulary.xmlschema import XMLSchema


class Repository:
    """
    A repository contains RDF data that can be queried and updated. Access
    to the repository can be acquired by opening a connection to it. This
    connection can then be used to query and/or update the contents of the
    repository.

    Please note that a repository needs to be initialized before it can be
    used and that it should be shut down before it is discarded/garbage
    collected. Forgetting the latter can result in loss of data (depending
    on the Repository implementation)!
    """

    # Modes for Catalog.getRepository()
    RENEW = "RENEW"
    ACCESS = "ACCESS"
    OPEN = "OPEN"
    CREATE = "CREATE"
    REPLACE = "REPLACE"

    def __init__(self, catalog, database_name, repository):
        """
        Invoke through :meth:`~franz.openrdf.sail.allegrographserver.Catalog.getRepository`.
        """
        self.mini_repository = repository
        self.database_name = database_name
        self.catalog = catalog
        # system state fields:
        self.value_factory = None

    def getDatabaseName(self):
        """
        Return the name of the database (remote triple store) that this repository is
        interfacing with.
        """
        return self.database_name

    def getSpec(self):
        """
        Return a session spec string for this repository.

        See :meth:`~franz.openrdf.sail.allegrographserver.AllegroGraphServer.openSession`.
        :return: A session spec.
        :rtype: string
        """
        mini = self.mini_repository
        urlstart = re.match("^https?://", mini.url).group(0)
        url = "<%s%s:%s@%s>" % (
            urlstart,
            mini.user,
            mini.password,
            mini.url[len(urlstart) :],
        )

        return url

    def initialize(self):
        """
        Initializes this repository. A repository must be initialized before
        it can be used.

        It is recommended to take advantage of the fact that repositories
        are context managers and use the ``with`` statement to ensure that
        :meth:`initialize` and :meth:`shutDown` are called:

        .. code:: python

           with catalog.getRepository() as repo:
               # No need to call initialize or shutDown inside.
               ...

        :return: ``self`` (to allow call chaining).
        """
        # We've only kept this method for RDF4J compatibility, we do not actually
        # need to do anything here.
        return self

    def registerDatatypeMapping(self, predicate=None, datatype=None, nativeType=None):
        """
        Register an inlined datatype.

        This allows some literals to be stored in an optimized form
        on the server.

        .. seealso::

           http://franz.com/agraph/support/documentation/current/lisp-reference.html#ref-type-mapping
              More detailed discussion of type mappings in the Lisp API documentation.

        You must supply ``nativeType`` and either ``predicate`` or ``datatype``.

        If ``predicate`` is supplied, then object arguments to triples with that
        predicate will use an inlined encoding of type `nativeType` in their internal
        representation on the server.

        If ``datatype`` is supplied, then typed literal objects with a datatype matching
        ``datatype`` will use an inlined encoding of type `nativeType`.

        Duplicated in the :class:`.RepositoryConnection` class for Python user convenience.

        :param predicate: The URI of a predicate used in the triple store.
        :param datatype: May be one of: ``XMLSchema.INT``, ``XMLSchema.LONG``,
                         ``XMLSchema.FLOAT``, ``XMLSchema.DATE`` and ``XMLSchema.DATETIME``.
        :param nativeType: may be ``int``, ``datetime``, or ``float``.
        :type nativeType: string|type
        """
        predicate = predicate.getURI() if isinstance(predicate, URI) else predicate
        datatype = datatype.getURI() if isinstance(datatype, URI) else datatype

        if nativeType is not None and not isinstance(nativeType, (str, bytes)):
            nativeType = nativeType.__name__

        def translate_inlined_type(the_type):
            if the_type == "int":
                return XMLSchema.LONG.toNTriples()
            if the_type == "datetime":
                return XMLSchema.DATETIME.toNTriples()
            if the_type == "time":
                return XMLSchema.TIME.toNTriples()
            if the_type == "date":
                return XMLSchema.DATE.toNTriples()
            if the_type == "float":
                return XMLSchema.DOUBLE.toNTriples()
            if the_type == "bool":
                return XMLSchema.BOOLEAN.toNTriples()
            raise IllegalArgumentException(
                "Unknown inlined type '%s'\n.  Legal types are "
                "int, float, bool, datetime, time, and date." % the_type
            )

        if predicate:
            if not nativeType:
                raise IllegalArgumentException(
                    "Missing 'nativeType' parameter in call to 'registerDatatypeMapping'"
                )
            xsdType = translate_inlined_type(nativeType)
            self.mini_repository.addMappedPredicate("<%s>" % predicate, xsdType)
        elif datatype:
            xsdType = translate_inlined_type(nativeType or datatype)
            self.mini_repository.addMappedType("<%s>" % datatype, xsdType)

    def shutDown(self):
        """
        Shuts the store down, releasing any resources that it keeps hold of.
        Once shut down, the store can no longer be used.

        It is recommended to take advantage of the fact that repositories
        are context managers and use the ``with`` statement to ensure that
        :meth:`initialize` and :meth:`shutDown` are called:

        .. code:: python

           with catalog.getRepository() as repo:
               # No need to call initialize or shutDown inside.
               ...
        """
        self.mini_repository = None

    def isWritable(self):
        """
        Checks whether this store is writable, i.e. if the data contained in
        this store can be changed. The writability of the store is
        determined by the writability of the Sail that this store operates
        on.
        """
        # TODO maybe remove this, it's nonsense in 4.0.
        return True

    def getConnection(self):
        """
        Opens a connection to this store that can be used for querying and
        updating the contents of the store. Created connections need to be
        closed to make sure that any resources they keep hold of are released.
        The best way to ensure this is to use a ``with`` statement:

        .. code:: python

           with repo.getConnection() as conn:
               ...

        :return: A :class:`RepositoryConnection` object.
        :rtype: RepositoryConnection
        """
        return RepositoryConnection(self)

    def getValueFactory(self):
        """
        Return a ValueFactory for this store.

        This is present for RDF4J compatibility, but in the Python API all ValueFactory
        functionality has been duplicated or subsumed in the :class:`.RepositoryConnection` class.
        It isn't necessary to manipulate the :class:`.ValueFactory` class at all.

        :return: A ValueFactory instance.
        :rtype: ValueFactory
        """
        if not self.value_factory:
            self.value_factory = ValueFactory(self)
        return self.value_factory

    def _set_bulk_mode(self, on):
        self.mini_repository.setBulkMode(on)

    def _get_bulk_mode(self):
        return self.mini_repository.getBulkMode()

    bulk_mode = property(
        _get_bulk_mode,
        _set_bulk_mode,
        doc="""Turn BulkMode on with True or off with False.

               In bulk mode, all modifications to the triple-store are made without
               writing to the transaction log. There is overhead to switching
               in and out of bulk-mode, and it is a global repository state, so all
               clients are affected.""",
    )

    def __enter__(self):
        self.initialize()
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        del exc_type, exc_val, exc_tb
        self.shutDown()
