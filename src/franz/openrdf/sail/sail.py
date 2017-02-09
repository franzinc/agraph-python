#!/usr/bin/env python
# -*- coding: utf-8 -*-
# pylint: disable-msg=C0103

################################################################################
# Copyright (c) 2006-2017 Franz Inc.  
# All rights reserved. This program and the accompanying materials are
# made available under the terms of the MIT License which accompanies
# this distribution, and is available at http://opensource.org/licenses/MIT
################################################################################

from __future__ import absolute_import
from __future__ import unicode_literals
from builtins import object

from ..repository.repository import Repository

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
class Sail(object):
    """
    Currently, this class is NOT USED!!!
    """

    def setDataDir(self, dataDir):
        """
        Set the directory where data and logging for this repository is stored.
        """
        raise NotImplementedError("setDataDir")

    def getDataDir(self):
        """
        Get the directory where data and logging for this repository is stored.
        """
        raise NotImplementedError("getDataDir")

    def initialize(self):
        """
        Initializes this repository. A repository needs to be initialized before
        it can be used.
        """
        raise NotImplementedError("initialize")

    def shutDown(self):
        """
        Shuts the repository down, releasing any resources that it keeps hold of.
        Once shut down, the repository can no longer be used until it is
        re-initialized.
        """
        raise NotImplementedError("shutDown")

    def isWritable(self):
        """
        Checks whether this repository is writable, i.e. if the data contained in
        this repository can be changed. The writability of the repository is
        determined by the writability of the Sail that this repository operates
        on.
        """
        raise NotImplementedError("isWritable")

    def getConnection(self):
        """
        Opens a connection to this repository that can be used for querying and
        updating the contents of the repository. Created connections need to be
        closed to make sure that any resources they keep hold of are released. The
        best way to do this is to use a try-finally-block 
        """
        raise NotImplementedError("getConnection")

    def getValueFactory(self):
        """
        Return a ValueFactory for this Repository
        """
        raise NotImplementedError("getValueFactory")
    
###################################################################################
##
###################################################################################

## THIS CLASS IS HOPEFULLY OBSOLETE:
class SailRepository(Repository):
    """
    For documentation, see 'Repository'.
    
    The class 'Repository' implements the hand-offs to a SailStore.
    Here, we provide an internal call to find that store, given a repository
    """
    
    ## NOT SURE IF WE NEED THIS OR NOT!!
    def getSail(self): return self.sail

###################################################################################
##
###################################################################################

class StackableSail(Sail):
    """
    An interface for Sails that can be stacked on top of other Sails.
    """
    
    def setBaseSail(self, baseSail):
        """
        Sets the base Sail that this Sail will work on top of. This method
        will be called before the initialize() method is called.
        """
        raise NotImplementedError("setBaseSail")

    def getBaseSail(self):
        """
        Gets the base Sail that this Sail works on top of.
        """
        raise NotImplementedError("getBaseSail")

###################################################################################
##
###################################################################################
    

# * A connection to an RDF Sail object. A SailConnection is active from the
# * moment it is created until it is closed. Care should be taken to properly
# * close SailConnections as they might block concurrent queries and/or updates
# * on the Sail while active, depending on the Sail-implementation that is being
# * used.
class SailConnection(object):
    pass



