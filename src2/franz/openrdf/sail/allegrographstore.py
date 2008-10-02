#!/usr/bin/env python
# -*- coding: utf-8 -*-

##***** BEGIN LICENSE BLOCK *****
##Version: MPL 1.1
##
##The contents of this file are subject to the Mozilla Public License Version
##1.1 (the "License"); you may not use this file except in compliance with
##the License. You may obtain a copy of the License at
##http:##www.mozilla.org/MPL/
##
##Software distributed under the License is distributed on an "AS IS" basis,
##WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
##for the specific language governing rights and limitations under the
##License.
##
##The Original Code is the AllegroGraph Java Client interface.
##
##The Original Code was written by Franz Inc.
##Copyright (C) 2006 Franz Inc.  All Rights Reserved.
##
##***** END LICENSE BLOCK *****


from franz.openrdf.exceptions import *
from franz.openrdf.sail.sail import Sail
from franz.openrdf.model.value import URI
from franz.openrdf.model.valuefactory import ValueFactory
from franz.openrdf.sail.agrepositoryconnection import AllegroGraphRepositoryConnection
from franz.miniclient.repository import AllegroGraphServer
from franz.miniclient.repository import Repository as MiniRepository

READ_ONLY = 'READ_ONLY'

LEGAL_OPTION_TYPES = {READ_ONLY: bool,}

# * A Sesame Sail contains RDF data that can be queried and updated.
# * Access to the Sail can be acquired by opening a connection to it.
# * This connection can then be used to query and/or update the contents of the
# * repository. Depending on the implementation of the repository, it may or may
# * not support multiple concurrent connections.
# * <p>
# * Please note that a Sail needs to be initialized before it can be used
# * and that it should be shut down before it is discarded/garbage collected.
class AllegroGraphStore(Sail):
    """
    Accumulates RDF store parameters, which are then passed to the
    repository where they are used to initialize the startup and 
    server execution.
    """
    RENEW = 'renew'
    ACCESS = 'access'
    OPEN = 'open'
    CREATE = 'create'
    REPLACE = 'replace'

#    EXPECTED_UNIQUE_RESOURCES = "EXPECTED_UNIQUE_RESOURCES"
#    WITH_INDICES = "WITH_INDICES"
#    INCLUDE_STANDARD_PARTS = "INCLUDE_STANDARD_PARTS"
#    INDIRECT_HOST = "INDIRECT_HOST"
#    INDIRECT_PORT = "INDIRECT_PORT"
    
    def __init__(self, accessVerb, host, databaseName, dbDirectory, port=4567, **options):
        self.access_verb = accessVerb
        self.host = host
        self.database_name = databaseName
        self.database_directory = dbDirectory
        self.port = port
        self.options = options
        self.translated_options = None
        ## system state fields:
        self.connection = None
        self.mini_server = None
        self.mini_repository = None
        self.is_closed = False
        self.value_factory = None
        self.inlined_predicates = {}
        self.inlined_datatypes = {}
        
#   def __init__(self, sailStore):
#        self.access_verb = sailStore.getAccessVerb()
#        self.host = sailStore.getHost()
#        self.database_name = sailStore.getDatabaseName()
#        self.database_directory = sailStore.getDatabaseDirectory()
#        self.options = sailStore.getOptions()
#                            
    
    def getAccessVerb(self): return self.access_verb
    def getHost(self): return self.host
    def getDatabaseName(self): return self.database_name
    def getDatabaseDirectory(self): return self.database_directory
    def getOptions(self): return self.options
    
    def _attach_to_mini_repository(self):
        """
        Connect to the mini-server, create a mini-repository, and 
        execute a RENEW, OPEN, CREATE, or ACCESS.
        
        TODO: FIGURE OUT WHAT 'REPLACE' DOES
        """
        address = "%s:%s" % (self.host, self.port)
        conn = AllegroGraphServer(address)
        dbName = self.database_name
        clearIt = False
        if self.access_verb == AllegroGraphStore.RENEW:
            if dbName in conn.listTripleStores():
                ## not nice, since someone else probably has it open:
                clearIt = True
            else:
                try:
                    conn.openTripleStore(dbName, self.database_directory)
                    clearIt = True
                except:
                    conn.createTripleStore(dbName, self.database_directory)                    
        elif self.access_verb == AllegroGraphStore.CREATE:
            if dbName in conn.listTripleStores():
                raise IllegalOptionException(
                    "Can't create triple store named '%s' because a store with that name already exists.",
                    dbName)
            conn.createTripleStore(dbName, self.database_directory)
        elif self.access_verb == AllegroGraphStore.OPEN:
            if not dbName in conn.listTripleStores():
                conn.openTripleStore(dbName, self.database_directory) 
        elif self.access_verb == AllegroGraphStore.ACCESS:
            if not dbName in conn.listTripleStores():
                try:
                    conn.openTripleStore(dbName, self.database_directory)
                except:
                    conn.createTripleStore(dbName, self.database_directory) 
        self.mini_server = conn       
        self.mini_repository = conn.getRepository(dbName)
        ## we are done unless a RENEW requires us to clear the store
        if clearIt:
            self.mini_repository.deleteMatchingStatements(None, None, None, None)
    
    def initialize(self):
        """
        Initialize this store. This will establish a connection to the remote 
        server, or die trying.
        """
        self._attach_to_mini_repository()
        
    def indexTriples(self, all=False, asynchronous=False):
        """
        Index the newly-added triples in the store.  This should be done after every 
        significant-sized load of triples into the store.
        If 'all', re-index all triples in the store.  If 'asynchronous', spawn
        the indexing task as a separate thread, and don't wait for it to complete.
        Note. Upon version 4.0, calling this will no longer be necessary.        
        """
        self.mini_repository.indexStatements(all=all)

    def registerFreeTextPredicate(self,uri=None, namespace=None, localname=None):
        """
        Register a predicate 'uri' (or 'namespace'+'localname'), telling the RDF store to index
        text keywords belonging to strings in object position in the corresponding
        triples/statements.  This is needed to make the  fti:match  operator
        work properly.
        """
        uri = uri or (namespace + localname)
        self.mini_repository.registerFreeTextPredicate("<%s>" % uri)
        
    def _translate_inlined_type(self, type):
        if type == "int": return "int"
        elif type == "datetime": return "date-time"
        elif type == "float": return "float"
        else:
            raise IllegalArgumentException("Unknown inlined type '%s'\n.  Legal types are " +
                    "'int', 'float', and 'datetime'")
        
    def registerInlinedDatatype(self, predicate=None, datatype=None, inlinedType=None):
        """
        Register an inlined datatype.  If 'predicate', then object arguments to triples
        with that predicate will use an inlined encoding of type 'inlinedType' in their 
        internal representation.
        If 'datatype', then typed literal objects with a datatype matching 'datatype' will
        use an inlined encoding of type 'inlinedType'.
        """
        predicate = predicate.getURI() if isinstance(predicate, URI) else predicate
        datatype = datatype.getURI() if isinstance(datatype, URI) else datatype
        if predicate:
            if not inlinedType:
                raise IllegalArgumentException("Missing 'inlinedType' parameter in call to 'registerInlinedDatatype'")
            lispType = self._translate_inlined_type(inlinedType)
            mapping = [predicate, lispType, "predicate"]
            self.inlined_predicates[predicate] = lispType
        elif datatype:
            lispType = self._translate_inlined_type(inlinedType or datatype)
            mapping = [datatype, lispType, "datatype"]
            self.inlined_datatypes[datatype] = lispType
        ##self.internal_ag_store.addDataMapping(mapping)
        raise UnimplementedMethodException("Inlined datatypes not yet implemented.")
        
    def setDataDirectory(self, dataDir):
        """
        Set the directory where data and logging for this store is stored.
        """
        self.databaseDirectory = dataDir

    def getDataDirectory(self):
        """
        Get the directory where data and logging for this store is stored.
        """
        return self.databaseDirectory

    def shutDown(self):
        """
        Shuts the store down, releasing any resources that it keeps hold of.
        Once shut down, the store can no longer be used.
        
        TODO: WE COULD PRESUMABLY ADD SOME LOGIC TO MAKE A RESTART POSSIBLE, ALTHOUGH
        THE ACCESS OPTION MIGHT NOT MAKE SENSE THE SECOND TIME AROUND (KILLING THAT IDEA!)
        """
        try:
            self.mini_server.close()
        finally:
            self.is_closed = True

    def isWritable(self):
        """
        Checks whether this store is writable, i.e. if the data contained in
        this store can be changed. The writability of the store is
        determined by the writability of the Sail that this store operates
        on.
        """
        return self.mini_repository.is_writable()

    def getConnection(self):
        """
        Opens a connection to this store that can be used for querying and
        updating the contents of the store. Created connections need to be
        closed to make sure that any resources they keep hold of are released. The
        best way to do this is to use a try-finally-block 
        """
        if not self.connection:
            self.connection = AllegroGraphRepositoryConnection(self)
        return self.connection

    def getValueFactory(self):
        """
        Return a ValueFactory for this store
        """
        if not self.value_factory:
            self.value_factory = ValueFactory(self)
        return self.value_factory
    

       
        
        