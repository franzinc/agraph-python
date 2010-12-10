#!/usr/bin/env python
# -*- coding: utf-8 -*-
# pylint: disable-msg=C0103

###############################################################################
# Copyright (c) 2006-2009 Franz Inc.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the Eclipse Public License v1.0
# which accompanies this distribution, and is available at
# http://www.eclipse.org/legal/epl-v10.html
###############################################################################

from __future__ import absolute_import

from ..exceptions import ServerException
from ..repository.repository import Repository, RepositoryConnection
from ...miniclient import repository as miniserver
import urllib
from . import spec

READ_ONLY = 'READ_ONLY'

LEGAL_OPTION_TYPES = {READ_ONLY: bool,}

class AllegroGraphServer(object):
    """
    Connects to an AllegroGraph HTTP Server
    """
    def __init__(self, host, port=10035, user=None, password=None, **options):
        self._client = miniserver.Client("http://%s:%d" % (host, port), user, password)

    @property
    def url(self):
        """Return the server's URL."""
        return self._client.url

    @property
    def version(self):
        """Return the server's version as a string."""
        return self._client.getVersion()

    def listCatalogs(self):
        catalogs = self._client.listCatalogs()
        return catalogs
    
    def openCatalog(self, name=None):
        """
        Open a catalog by name. Pass None to open the root catalog.
        """       
        # Allow for None and '' (or anything else that evaluates to False) to
        # mean the root catalog.
        if not name:
            name = None
            
        cats = self.listCatalogs()
        if not name in cats:
            raise ServerException("There is no catalog named '%s' (found %s)"
                % (name, cats))

        return Catalog(name, self._client)

    def getInitfile(self):
        """
        Retrieve the contents of the server initialization file.

        The initialization file is a collection of Common Lisp code
        that is executed in every back-end as it is created.
        """
        return self._client.getInitfile()

    def setInitfile(self, content=None, restart=True):
        """
        Replace the current initialization file contents with the
        'content' string or remove if None. `restart`, which defaults
        to true, specifies whether any running shared back-ends should
        be shut down, so that subsequent requests will be handled by
        back-ends that include the new code.
        """
        return self._client.setInitfile(content, restart)

    def openSession(self, spec, autocommit=False, lifetime=None, loadinitfile=False):
        """
        Open a session on a federated, reasoning, or filtered store.
        Use the helper functions in the franz.openrdf.sail.spec module
        to create the spec string.
        """
        minirep = self._client.openSession(spec, autocommit=autocommit, lifetime=lifetime, loadinitfile=loadinitfile)
        return RepositoryConnection(Repository(None, None, minirep))

    def openFederated(self, repositories, autocommit=False, lifetime=None, loadinitfile=False):
        """
        Open a session that federates several repositories. The
        repositories argument should be an array containing store
        designators, which can be Repository or RepositoryConnection
        objects, strings (naming a store in the root catalog, or the
        URL of a store), or (storename, catalogname) tuples.
        """
        def asRepoString(x):
            if isinstance(x, str): return spec.local(x)
            elif isinstance(x, tuple): return spec.local(x[0], x[1])
            elif isinstance(x, Repository): return x.getSpec()
            elif isinstance(x, RepositoryConnection): return x.getSpec()
            else: raise TypeError(str(x) + " is not a valid repository specification.")
        return self.openSession(spec.federate(*map(asRepoString, repositories)), autocommit, lifetime, loadinitfile)

class Catalog(object):
    """
    Container of multiple repositories (triple stores).
    """
    def __init__(self, name, client):
        self.mini_catalog = client.openCatalogByName(name)
        self._name = name

    def getName(self):
        """Return the catalog name."""
        return self._name
    
    name = property(getName)

    def listRepositories(self):
        """
        Return a list of names of repositories (triple stores) managed by
        this catalog.
        """
        return self.mini_catalog.listRepositories()
    
    def deleteRepository(self, name):
        return self.mini_catalog.deleteRepository(name)
    
    def getRepository(self, name, access_verb):
        """
        Create a mini-repository and execute a RENEW, OPEN, CREATE, or ACCESS.
        """
        access_verb = access_verb.upper()
        name = urllib.quote_plus(name)
        exists = name in self.listRepositories();
        if access_verb == Repository.RENEW:
            if exists:
                self.deleteRepository(name)
            return self.createRepository(name)

        if access_verb == Repository.CREATE:
            if exists:
                raise ServerException(
                    "Can't create triple store named '%s' because a store with that name already exists.",
                    name)
            return self.createRepository(name)

        if access_verb == Repository.OPEN:
            if not exists:
                raise ServerException(
                    "Can't open a triple store named '%s' because there is none.", name)

            return Repository(self, name, self.mini_catalog.getRepository(name))

        if access_verb == Repository.ACCESS:
            if not exists:
                return self.createRepository(name)

        return Repository(self, name, self.mini_catalog.getRepository(name))

    def createRepository(self, name):
        return Repository(self, name, self.mini_catalog.createRepository(name))


