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
from franz.miniclient.repository import AllegroGraphServer as MiniServer

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
        self.options = options
        self.translated_options = None
        address = "%s:%s" % (self.host, self.port)
        self.mini_server = MiniServer(address)
    
    def getHost(self): return self.host
    def getOptions(self): return self.options

    def listRepositories(self):
        return self.mini_server.listTripleStores()

       
        
        