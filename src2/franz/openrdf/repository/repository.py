#!/usr/bin/env python
# -*- coding: utf-8 -*-
# pylint: disable-msg=C0103

###############################################################################
# Copyright (c) 2006-2012 Franz Inc.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the Eclipse Public License v1.0
# which accompanies this distribution, and is available at
# http://www.eclipse.org/legal/epl-v10.html
###############################################################################

from __future__ import absolute_import

from franz import miniclient
from ..exceptions import IllegalArgumentException
from ..model import URI, ValueFactory
from .repositoryconnection import RepositoryConnection
from ..vocabulary.xmlschema import XMLSchema

import re, urllib

# * A Sesame repository that contains RDF data that can be queried and updated.
# * Access to the repository can be acquired by opening a connection to it.
# * This connection can then be used to query and/or update the contents of the
# * repository. Depending on the implementation of the repository, it may or may
# * not support multiple concurrent connections.
# * <p>
# * Please note that a repository needs to be initialized before it can be used
# * and that it should be shut down before it is discarded/garbage collected.
# * Forgetting the latter can result in loss of data (depending on the Repository
# * implementation)!
class Repository(object):
    RENEW = 'RENEW'
    ACCESS = 'ACCESS'
    OPEN = 'OPEN'
    CREATE = 'CREATE'
    REPLACE = 'REPLACE'

    def __init__(self, catalog, database_name, repository):
        self.mini_repository = repository
        self.database_name = database_name
        self.catalog = catalog
        ## system state fields:
        self.value_factory = None
         
    def getDatabaseName(self):
        """
        Return the name of the database (remote triple store) that this repository is
        interfacing with.
        """ 
        return self.database_name

    def getSpec(self):
        mini = self.mini_repository
        urlstart = re.match("^https?://", mini.url).group(0)
        url = "<%s%s:%s@%s>" % (urlstart, mini.user, mini.password,
            mini.url[len(urlstart):])

        return url
        
    def initialize(self):
        """
        Initializes this repository. A repository needs to be initialized before
        it can be used.  Return 'self' (so that we can chain this call if we like).
        """
        return self

    def registerDatatypeMapping(self, predicate=None, datatype=None, nativeType=None):
        """
        Register an inlined datatype.  If 'predicate', then object arguments to triples
        with that predicate will use an inlined encoding of type 'nativeType' in their 
        internal representation.
        If 'datatype', then typed literal objects with a datatype matching 'datatype' will
        use an inlined encoding of type 'nativeType'.
        """
        predicate = predicate.getURI() if isinstance(predicate, URI) else predicate
        datatype = datatype.getURI() if isinstance(datatype, URI) else datatype

        if nativeType is not None and not isinstance(nativeType, basestring):
            nativeType=nativeType.__name__

        def translate_inlined_type(the_type):
            if the_type == 'int':
                return XMLSchema.LONG.toNTriples()
            if the_type == 'datetime':
                return XMLSchema.DATETIME.toNTriples()
            if the_type == 'time':
                return XMLSchema.TIME.toNTriples()
            if the_type == 'date':
                return XMLSchema.DATE.toNTriples()
            if the_type == "float":
                return XMLSchema.DOUBLE.toNTriples()
            if the_type == "bool":
                return XMLSchema.BOOLEAN.toNTriples()
            raise IllegalArgumentException("Unknown inlined type '%s'\n.  Legal types are "\
                    "int, float, bool, datetime, time, and date." % the_type)
            
        if predicate:
            if not nativeType:
                raise IllegalArgumentException("Missing 'nativeType' parameter in call to 'registerDatatypeMapping'")
            xsdType = translate_inlined_type(nativeType)
            self.mini_repository.addMappedPredicate("<%s>" % predicate, xsdType)            
        elif datatype: 
            xsdType = translate_inlined_type(nativeType or datatype)
            self.mini_repository.addMappedType("<%s>" % datatype, xsdType)
        
    def shutDown(self):
        """
        Shuts the store down, releasing any resources that it keeps hold of.
        Once shut down, the store can no longer be used.
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
        return True;

    def getConnection(self):
        """
        Opens a connection to this store that can be used for querying and
        updating the contents of the store. Created connections need to be
        closed to make sure that any resources they keep hold of are released. The
        best way to do this is to use a try-finally-block 
        """
        return RepositoryConnection(self)

    def getValueFactory(self):
        """
        Return a ValueFactory for this store
        """
        if not self.value_factory:
            self.value_factory = ValueFactory(self)
        return self.value_factory

    def setBulkMode(self, on):
        return self.mini_repository.setBulkMode(on)

    def getBulkMode(self):
        return self.mini_repository.getBulkMode()

    bulk_mode = property(getBulkMode, setBulkMode,
        "Turn BulkMode on with True or off with False.\n"
        "\n"
        "In bulk mode, all statements are added to the triple-store without flushing\n"
        "disk writes to the transaction log. There is overhead to switching\n"
        "out of bulk-mode, and it is a global repository state, so all clients.\n"
        "are affected.\n")
