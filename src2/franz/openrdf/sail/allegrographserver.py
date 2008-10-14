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
from franz.openrdf.repository.repository import Repository
from franz.miniclient import repository as miniserver
from franz.miniclient.repository import Catalog

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
class AllegroGraphServer(object):
    """
    Connects to a remote AllegroGraph HTTP Server
    """

#    EXPECTED_UNIQUE_RESOURCES = "EXPECTED_UNIQUE_RESOURCES"
#    WITH_INDICES = "WITH_INDICES"
#    INCLUDE_STANDARD_PARTS = "INCLUDE_STANDARD_PARTS"
#    INDIRECT_HOST = "INDIRECT_HOST"
#    INDIRECT_PORT = "INDIRECT_PORT"
    
    def __init__(self, host, port=4567, **options):
        self.host = host
        self.port = port
        self.username = None
        self.password = None
        self.open_catalogs = []
        self.options = options
        self.translated_options = None
    
    def _get_address(self):
        return "%s:%s" % (self.host, self.port)
    
    def getHost(self): return self.host
    def getOptions(self): return self.options

    def listCatalogs(self):
        catNames = []
        for longName in miniserver.listCatalogs(self._get_address()):
            pos = longName.rfind('/')
            catNames.append(longName[pos + 1:])
        return catNames
    
    def openCatalog(self, shortName):
        """
        Open a catalog named 'catalogName'.
        """
        if not shortName in self.listCatalogs():
            raise ServerException("There is no catalog named '%s'" % shortName)
        for cat in self.open_catalogs:
            if cat.getName() == shortName:
                return cat
        longName = '/catalogs/' + shortName
        miniCatalog = miniserver.openCatalog(self._get_address(), longName, user=self.username, password=self.password)
        catalog = Catalog(shortName, miniCatalog, self)
        return catalog

class Catalog(object):
    """
    Container of multiple repositories (triple stores).
    """
    def __init__(self, short_name, mini_catalog, server):
        self.server = server
        self.mini_catalog = mini_catalog
        self.short_name = short_name
        self.is_closed = False
        
    def getName(self):
        return self.short_name
    
    def listRepositories(self):
        """
        Return a list of names of repositories (triple stores) managed by
        this catalog.
        """
        return self.mini_catalog.listTripleStores()
    
    def getRepository(self, name, access_verb):
        return Repository(self, name, access_verb)
    
    def close(self):
        if self.is_closed: return
        self.server.open_catalogs.remove(self)
        self.is_closed = True
        
       
        
        