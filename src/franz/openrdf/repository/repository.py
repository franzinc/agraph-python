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
from franz.openrdf.model.valuefactory import ValueFactory 

# * A Sesame repository that contains RDF data that can be queried and updated.
# * Access to the repository can be acquired by openening a connection to it.
# * This connection can then be used to query and/or update the contents of the
# * repository. Depending on the implementation of the repository, it may or may
# * not support multiple concurrent connections.
# * <p>
# * Please note that a repository needs to be initialized before it can be used
# * and that it should be shut down before it is discarded/garbage collected.
# * Forgetting the latter can result in loss of data (depending on the Repository
# * implementation)!
class Repository:
    def __init__(self, sail):
        self.sail = sail

    def setDataDir(self, dataDir):
        """
        Set the directory where data and logging for this repository is stored.
        """
        self.sail.setDataDir(dataDir)

    def getDataDir(self):
        """
        Get the directory where data and logging for this repository is stored.
        """
        return self.sail.getDataDir()

    def initialize(self):
        """
        Initializes this repository. A repository needs to be initialized before
        it can be used.
        """
        self.sail.initialize()
        
    def indexTriples(self, all=False, asynchronous=False):
        """
        Index the newly-added triples in the store.  This should be done after every 
        significant-sized load of triples into the store.
        If 'all', re-index all triples in the store.  If 'asynchronous', spawn
        the indexing task as a separate thread, and don't wait for it to complete.
        Note. Upon version 4.0, calling this will no longer be necessary.        
        """
        self.sail.indexTriples(all=all, asynchronous=asynchronous)

    def shutDown(self):
        """
        Shuts the repository down, releasing any resources that it keeps hold of.
        Once shut down, the repository can no longer be used until it is
        re-initialized.
        """
        self.sail.shutDown()

    def isWritable(self):
        """
        Checks whether this repository is writable, i.e. if the data contained in
        this repository can be changed. The writability of the repository is
        determined by the writability of the Sail that this repository operates
        on.
        """
        return self.sail.isWritable()

    def getConnection(self):
        """
        Opens a connection to this repository that can be used for querying and
        updating the contents of the repository. Created connections need to be
        closed to make sure that any resources they keep hold of are released. The
        best way to do this is to use a try-finally-block 
        """
        return self.sail.getConnection()

    def getValueFactory(self):
        """
        Return a ValueFactory for this Repository
        """
        return self.sail.getValueFactory()
    
    
    