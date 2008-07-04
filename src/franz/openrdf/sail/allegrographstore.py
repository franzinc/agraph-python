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
from franz.openrdf.modelimpl.valuefactoryimpl import ValueFactoryImpl
from franz.openrdf.sail.agrepositoryconnection import AllegroGraphRepositoryConnection
from franz.openrdf.exceptions import *
from franz.allegrograph.startup import StartUp
from franz.allegrograph.open2ag import Term2InternalManager
from franz.transport.agconnection import AllegroGraphConnection as AGInternalConnection

LEGAL_OPTIONS = {}
LEGAL_OPTION_TYPES = {}

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
    RENEW = AGInternalConnection.RENEW
    ACCESS = AGInternalConnection.ACCESS
    OPEN = AGInternalConnection.OPEN
    CREATE = AGInternalConnection.CREATE
    REPLACE = AGInternalConnection.REPLACE
    
    EXPECTED_UNIQUE_RESOURCES = "EXPECTED_UNIQUE_RESOURCES"
    WITH_INDICES = "WITH_INDICES"
    INCLUDE_STANDARD_PARTS = "INCLUDE_STANDARD_PARTS"
    READ_ONLY = "READ_ONLY"
    INDIRECT_HOST = "INDIRECT_HOST"
    INDIRECT_PORT = "INDIRECT_PORT"

    EXPECTED_UNIQUE_RESOURCES_OP = "expected-unique-resources"
    WITH_INDICES_OP = "with-indices"
    INCLUDE_STANDARD_PARTS_OP = "include-standard-parts"
    READ_ONLY_OP = "read-only-p"
    INDIRECT_HOST_OP = "indirect-host"
    INDIRECT_PORT_OP = "indirect-port"
    
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
        self.internal_ag_store = None
        self.is_closed = False
        self.value_factory = None
        self.term2internal = None
        
#   def __init__(self, sailStore):
#        self.access_verb = sailStore.getAccessVerb()
#        self.host = sailStore.getHost()
#        self.database_name = sailStore.getDatabaseName()
#        self.database_directory = sailStore.getDatabaseDirectory()
#        self.options = sailStore.getOptions()
#        
        
    def translated_options(self):
        global LEGAL_OPTION_TYPES
        for key, value in self.options.iteritems():
            valueType = LEGAL_OPTION_TYPES.get(key)
            if not type:
                raise IllegalOptionException("Unrecognized option '%s'.  Legal options are:\n   %s"
                         % (key, [opt for opt in LEGAL_OPTIONS.iterkeys()]))
            if not isinstance(value, valueType):
                raise IllegalOptionException("Invalid option '%s=%s'.  Expected an option of type '%s'" %
                                             (key, value, valueType))
            
    
    def getAccessVerb(self): return self.access_verb
    def getHost(self): return self.host
    def getDatabaseName(self): return self.database_name
    def getDatabaseDirectory(self): return self.database_directory
    def getOptions(self): return self.options
    
    def initialize(self):
        """
        Initialize this store. This will establish a connection to the remote 
        server, or die trying.
        """
        self.internal_ag_store = StartUp.startUpTripleStore(self.access_verb, self.host, self.database_name, 
                                                  self.database_directory, self.options)
        
    def indexTriples(self):
        """
        (Re)index the triples in the store.  This should be done after every 
        significant-sized load of triples into the store.
        Note. Upon version 4.0, calling this will no longer be necessary.        
        """
        self.internal_ag_store.indexTriples()


        
    def getInternalAllegroGraph(self):
        """
        Return the AllegroGraph instance for this store.  This is an internal
        call, not to be called by applications.
        """
        return self.internal_ag_store
    
    def getTerm2InternalManager(self):
        if not self.term2internal:
            if not self.internal_ag_store:
                raise InitializationException("Failed to initialize internal AllegroGraphStore before first use.")
            self.term2internal = Term2InternalManager(self.internal_ag_store, self.getValueFactory())
        return self.term2internal

    def setDataDir(self, dataDir):
        """
        Set the directory where data and logging for this store is stored.
        """
        self.databaseDirectory = dataDir

    def getDataDir(self):
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
            StartUp.shutDownTripleStore(self.internal_ag_store)
        finally:
            self.internal_ag_store = None
            self.is_closed = True

    def isWritable(self):
        """
        Checks whether this store is writable, i.e. if the data contained in
        this store can be changed. The writability of the store is
        determined by the writability of the Sail that this store operates
        on.
        """
        return not self.options.get(AllegroGraphStore.READ_ONLY)

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
            self.value_factory = ValueFactoryImpl(self)
        return self.value_factory
    
###################################################################################
##
###################################################################################

    def get_connector(self):
        """
        Return the connector to the socket.
        """
        return self.internal_ag_store.verifyEnabled()


###################################################################################
## Initialization of static variables takes place AFTER the class
## has been defined:
###################################################################################
   
LEGAL_OPTIONS = {
    AllegroGraphStore.EXPECTED_UNIQUE_RESOURCES: AllegroGraphStore.EXPECTED_UNIQUE_RESOURCES_OP,
    AllegroGraphStore.WITH_INDICES: AllegroGraphStore.WITH_INDICES_OP,
    AllegroGraphStore.INCLUDE_STANDARD_PARTS: AllegroGraphStore.INCLUDE_STANDARD_PARTS_OP,
    AllegroGraphStore.READ_ONLY: AllegroGraphStore.READ_ONLY_OP,
    AllegroGraphStore.INDIRECT_HOST: AllegroGraphStore.INDIRECT_HOST_OP,
    AllegroGraphStore.INDIRECT_PORT: AllegroGraphStore.INDIRECT_PORT_OP,
}

LEGAL_OPTION_TYPES = {
    AllegroGraphStore.EXPECTED_UNIQUE_RESOURCES: long,
    AllegroGraphStore.WITH_INDICES: list,
    AllegroGraphStore.INCLUDE_STANDARD_PARTS: bool,
    AllegroGraphStore.READ_ONLY: bool,
    AllegroGraphStore.INDIRECT_HOST: str,
    AllegroGraphStore.INDIRECT_PORT: int,
}
    
       
        
        