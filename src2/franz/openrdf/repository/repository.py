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


from franz import miniclient
from franz.openrdf.exceptions import *
from franz.openrdf.model.value import URI
from franz.openrdf.model.valuefactory import ValueFactory
from franz.openrdf.repository.repositoryconnection import RepositoryConnection
import franz.miniclient
import threading
import urllib


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
class Repository:
    RENEW = 'RENEW'
    ACCESS = 'ACCESS'
    OPEN = 'OPEN'
    CREATE = 'CREATE'
    REPLACE = 'REPLACE'

    def __init__(self, catalog, database_name, access_verb, multi_threaded_mode=False):
        self.catalog = catalog
        self.mini_catalog = catalog.mini_catalog
        self.mini_repository = None
        self.thread_local_mini_repository = threading.local()
        self.thread_local_mini_repository.value = None
        self.database_name = database_name
        self.access_verb = access_verb.upper()
        self.multi_threaded_mode=multi_threaded_mode
        ## system state fields:
        self.connection = None
        self.value_factory = None
        self.is_initialized = False
        self.federated_triple_stores = None
        self.mapped_predicates = {}
        self.mapped_datatypes = {}
        ## make sure that ValueFactory is initialized:
        #self.getConnection()
        #self.getValueFactory()
         
    def getDatabaseName(self):
        """
        Return the name of the database (remote triple store) that this repository is
        interfacing with.
        """ 
        return self.database_name
    
    def _create_triple_store(self, quotedDbName):
        miniCat = self.mini_catalog
        if self.federated_triple_stores:
            miniCat.federateTripleStores(quotedDbName, [urllib.quote_plus(ts) for ts in self.federated_triple_stores])
        else:
            miniCat.createTripleStore(quotedDbName)
          
    def _attach_to_mini_repository(self):
        """
        Create a mini-repository and execute a RENEW, OPEN, CREATE, or ACCESS.
        
        TODO: FIGURE OUT WHAT 'REPLACE' DOES
        """
        def remove_double_quotes(name):
            if name.startswith('"'):
                name = name[1:-1]
            return name
        #clearIt = False
        quotedDbName = urllib.quote_plus(self.database_name)
        miniCat = self.mini_catalog
        repositoryNames = [remove_double_quotes(name) for name in miniCat.listTripleStores()]
        if self.access_verb == Repository.RENEW:
            if quotedDbName in repositoryNames:
                ## not nice, since someone else probably has it open:
                miniCat.deleteTripleStore(quotedDbName)
            self._create_triple_store(quotedDbName)                    
        elif self.access_verb == Repository.CREATE:
            if quotedDbName in repositoryNames:
                raise ServerException(
                    "Can't create triple store named '%s' because a store with that name already exists.",
                    quotedDbName)
            self._create_triple_store(quotedDbName)
        elif self.access_verb == Repository.OPEN:
            if not quotedDbName in repositoryNames:
                raise ServerException(
                    "Can't open a triple store named '{0}' because there is none.".format(quotedDbName))
        elif self.access_verb == Repository.ACCESS:
            if not quotedDbName in repositoryNames:
                self._create_triple_store(quotedDbName)      
        self.mini_repository = miniCat.getRepository(quotedDbName)
#        ## we are done unless a RENEW requires us to clear the store
#        if clearIt:
#            self.mini_repository.deleteMatchingStatements(None, None, None, None)

    def _get_mini_repository(self):
        import sys
        if self.multi_threaded_mode:
            ## EXPERIMENT -- THIS IS WORKING WELL AT THE MOMENT:
            if True:
                if hasattr(self.thread_local_mini_repository, 'value'):
                    miniRepository = self.thread_local_mini_repository.value
                else:
                    miniRepository = None
                if miniRepository:
                    #print >> sys.stderr, ">>>>>>>>>>REUSING THREAD LOCAL REPOSITORY" 
                    pass           
                if not miniRepository:
                    print >> sys.stderr, ">>>>>>>>>>CREATING NEW THREAD LOCAL REPOSITORY"    
                    miniRepository = miniclient.repository.Repository(self.mini_repository.url)
                    self.thread_local_mini_repository.value = miniRepository
                return miniRepository
            else:
                print >> sys.stderr, ">>>>>>>>>>RESETTING CURL"            
                return miniclient.repository.Repository(self.mini_repository.url)
        else:
            return self.mini_repository
        
    def initialize(self):
        """
        Initializes this repository. A repository needs to be initialized before
        it can be used.  Return 'self' (so that we can chain this call if we like).
        """
        if self.is_initialized:
            raise InitializationException("A repository cannot be initialized twice.")
        self._attach_to_mini_repository()
        ## EXPERIMENTATION WITH INITIALIZING AN ENVIRONMENT.  DIDN'T LOOK RIGHT - RMM
#        self.environment = self.mini_repository.createEnvironment()
#        print "ENV", self.environment
#        self.mini_repository.deleteEnvironment(self.environment)
#        print "ENV AfTER", self.mini_repository.listEnvironments()
        ## tricky: 'Literal' can be called before ValueFactory is initialized, causing
        ## it to break.  Here we make sure its up:
        self.getValueFactory()
        self.is_initialized = True
        return self    
    
    def addFederatedTripleStores(self, tripleStoreNames):
        """
        Make this repository a federated store that includes the stores named in
        'tripleStoreNames'.  This call must precede the call to 'initialize'.  It
        may be called multiple times.        
        """
        if self.is_initialized:
            raise InitializationException("Federated triples stores must be added prior to calling 'initialize'.")
        if not self.access_verb in [Repository.CREATE, Repository.RENEW]:
            raise InitializationException("Adding federated triple stores requires a CREATE or RENEW access option.\n" +
                                          "The current access is set to '%s'." % self.access_verb)
        if not self.federated_triple_stores:
            self.federated_triple_stores = set([])
        for ts in tripleStoreNames:
            self.federated_triple_stores.add(ts)
        return self
        
    def indexTriples(self, all=False, asynchronous=False):
        """
        Index the newly-added triples in the store.  This should be done after every 
        significant-sized load of triples into the store.
        If 'all', re-index all triples in the store.  If 'asynchronous', spawn
        the indexing task as a separate thread, and don't wait for it to complete.
        Note. Upon version 4.0, calling this will no longer be necessary.        
        """
        self._get_mini_repository().indexStatements(all=all)

    def registerFreeTextPredicate(self, uri=None, namespace=None, localname=None):
        """
        Register a predicate 'uri' (or 'namespace'+'localname'), telling the RDF store to index
        text keywords belonging to strings in object position in the corresponding
        triples/statements.  This is needed to make the  fti:match  operator
        work properly.
        """
        uri = uri or (namespace + localname)
        self._get_mini_repository().registerFreeTextPredicate("<%s>" % uri)
        
    def _translate_inlined_type(self, type):
        if type == "int": return "int"
        elif type == "datetime": return "date-time"
        elif type == "float": return "double-float"
        else:
            raise IllegalArgumentException("Unknown inlined type '%s'\n.  Legal types are " +
                    "'int', 'float', and 'datetime'")
        
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
        if predicate:
            if not nativeType:
                raise IllegalArgumentException("Missing 'nativeType' parameter in call to 'registerDatatypeMapping'")
            lispType = self._translate_inlined_type(nativeType)
            #mapping = [predicate, lispType, "predicate"]
            self.mapped_predicates[predicate] = lispType
        elif datatype:
            lispType = self._translate_inlined_type(nativeType or datatype)
            #mapping = [datatype, lispType, "datatype"]
            self.mapped_datatypes[datatype] = lispType
        if predicate:
            self._get_mini_repository().addMappedPredicate("<%s>" % predicate, lispType)            
        else:
            self._get_mini_repository().addMappedType("<%s>" % datatype, lispType)
        
    def shutDown(self):
        """
        Shuts the store down, releasing any resources that it keeps hold of.
        Once shut down, the store can no longer be used.
        
        TODO: WE COULD PRESUMABLY ADD SOME LOGIC TO MAKE A RESTART POSSIBLE, ALTHOUGH
        THE ACCESS OPTION MIGHT NOT MAKE SENSE THE SECOND TIME AROUND (KILLING THAT IDEA!)
        """
        self.mini_catalog = None
        self.mini_repository = None

    def isWritable(self):
        """
        Checks whether this store is writable, i.e. if the data contained in
        this store can be changed. The writability of the store is
        determined by the writability of the Sail that this store operates
        on.
        """
        return self._get_mini_repository().is_writable()

    def getConnection(self):
        """
        Opens a connection to this store that can be used for querying and
        updating the contents of the store. Created connections need to be
        closed to make sure that any resources they keep hold of are released. The
        best way to do this is to use a try-finally-block 
        """
        if not self.connection:
            self.connection = RepositoryConnection(self)
        return self.connection

    def getValueFactory(self):
        """
        Return a ValueFactory for this store
        """
        if not self.value_factory:
            self.value_factory = ValueFactory(self)
        return self.value_factory

    
    
    
