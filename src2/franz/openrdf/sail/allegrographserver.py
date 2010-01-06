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
from ..repository.repository import Repository
from ...miniclient import repository as miniserver
import urllib

READ_ONLY = 'READ_ONLY'

LEGAL_OPTION_TYPES = {READ_ONLY: bool,}

class AllegroGraphServer(object):
    """
    Connects to an AllegroGraph HTTP Server
    """
    FEDERATED = 'federated'

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
        catalogs.append(AllegroGraphServer.FEDERATED)
        return catalogs
    
    def openCatalog(self, name=None):
        """
        Open a catalog by name. Pass None to open the root catalog.
        Pass 'federated' to open the virtual catalog of federations.
        """       
        # Allow for None and '' (or anything else that evaluates to False) to
        # mean the root catalog.
        if not name:
            name = None
            
        cats = self.listCatalogs()
        if not name in cats:
            raise ServerException("There is no catalog named '%s' (found %s)"
                % (name, cats))

        if name == AllegroGraphServer.FEDERATED:
            catalog = FederatedCatalog(AllegroGraphServer.FEDERATED, self._client)
        else:
            catalog = Catalog(name, self._client)

        return catalog

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


class FederatedCatalog(object):
    """
    Container of multiple federated repositories (triple stores).
    """
    def __init__(self, name, client):
        self.mini_catalog = client
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
        return self.mini_catalog.listFederations()
    
    def deleteRepository(self, name):
        return self.mini_catalog.deleteFederation(name)
    
    def getRepository(self, name, access_verb=Repository.OPEN):
        access_verb = access_verb.upper()
        assert access_verb == Repository.OPEN, "Only OPEN is allowed on getRepository for a federated Repository."
        return Repository(self, name, self.mini_catalog.getFederation(name))

    def createRepository(self, name, urls=[], repos=[]):
        return Repository(self, name, self.mini_catalog.createFederation(name, urls, repos))


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


