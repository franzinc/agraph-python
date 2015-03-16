#!/usr/bin/env python
# -*- coding: utf-8 -*-
# pylint: disable-msg=C0103

###############################################################################
# Copyright (c) 2006-2015 Franz Inc.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the Eclipse Public License v1.0
# which accompanies this distribution, and is available at
# http://www.eclipse.org/legal/epl-v10.html
###############################################################################

from __future__ import absolute_import

from ..exceptions import ServerException
from ..repository.repository import Repository, RepositoryConnection
from ...miniclient import repository as miniserver
import re, urllib
from . import spec

READ_ONLY = 'READ_ONLY'

LEGAL_OPTION_TYPES = {READ_ONLY: bool,}

class AllegroGraphServer(object):
    """
    Connects to an AllegroGraph HTTP Server
    """
    def __init__(self, host, port=10035, user=None, password=None, cainfo=None, sslcert=None, verifyhost=None, verifypeer=None, **options):
        """
        Defines the connection to the AllegroGraph HTTP server.

        Pass either user & password for Basic Authentication or
        cainfo, sslcert values for client x.509 certificate
        authentication. You can optionally set verifyhost/verifypeer
        to True or False. Default behavior is True.

        See pycurl documentation for the meanings of cainfo, sslcert,
        verifyhost, verifypeer as those values are just passed 
        through to the Curl object's setopt function.
        """
        
        if re.match('^https?://', host):
            uri = host + ':%d'
        elif sslcert != None:
            uri = 'https://%s:%d'
        else:
            uri = 'http://%s:%d'
        
        self._client = miniserver.Client(uri % (host, port), user, password, cainfo, sslcert, verifyhost, verifypeer)

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

    def listScripts(self):
        """
        List the registered scripts.
        """
        return self._client.listScripts()

    def addScript(self, module, code):
        return self._client.addScript(module, code)

    def deleteScript(self, module):
        return self._client.deleteScript(module)

    def getScript(self, module):
        return self._client.getScript(module)

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

    def listUsers(self):
        """
        Returns a list of names of all the users that have been defined.
        """
        return self._client.listUsers()

    def addUser(self, name, password=None):
        """
        Create a new user. Expects a password parameter, which specifies the
        user's password (can be left off when creating the anonymous user).
        """
        assert password is not None or name == 'anonymous'
        self._client.addUser(name, password)

    def deleteUser(self, name):
        """
        Delete a user.
        """
        self._client.deleteUser(name)

    def changeUserPassword(self, name, password):
        """
        Change the password for the given user.
        """
        self._client.changeUserPassword(name, password)

    def listUserAccess(self, name):
        """
        Retrieve the read/write access for a user. This returns a result set,
        each element of which has a read, write, catalog, and repository
        component. The first two are booleans, the latter two strings. For
        permissions granted globally, catalog and repository will have a
        value of "*", for those granted per-catalog, only repository will
        be "*". catalog normally contains the catalog name, but for the rootAs above, but also includes the access granted to roles that this user has.
        catalog "/" is used.
        """
        return self._client.listUserAccess(name)

    def addUserAccess(self, name, read, write, catalog='*', repository='*'):
        """
        This is used to grant read/write access to a user. It takes four parameters:

        read
                Whether to grant read access. A boolean, defaults to false.
        write
                Whether to grant write access. Boolean, defaults to false.
        catalog
                Which catalog to grant the access on. Leave off or pass * to grant
                access on all catalogs. Again, use / for the root catalog.
        repository
                Specifies the repository that access is granted on. Passing *,
                or leaving the parameter off, means all repositories in the
                given catalog.
        """
        self._client.addUserAccess(name, read, write, catalog, repository)

    def deleteUserAccess(self, name, read, write, catalog='*', repository='*'):
        """
        Takes the same parameters as PUT on this URL, but revokes the access instead of granting it.
        """
        self._client.deleteUserAccess(name, read, write, catalog, repository)

    def listUserEffectiveAccess(self, name):
        """
        Like listUserAccess, but also includes the access granted to roles that this user has.
        """
        return self._client.listUserEffectiveAccess(name)

    def listUserPermissions(self, name):
        """
        List the permission flags that have been assigned to a user (any of
        super, eval, session, replication).
        """
        return self._client.listUserPermissions(name)

    def listUserEffectivePermissions(self, name):
        """
        Retrieve the permission flags assigned to the user, or any of its roles.
        """
        return self._client.listUserEffectivePermissions(name)

    def addUserPermission(self, name, _type):
        """
        Assigns the given permission to this user. type should be super, eval,
        replication, or session.
        """
        self._client.addUserPermission(name, _type)

    def deleteUserPermission(self, name, _type):
        """
        Revokes the given permission for this user.
        """
        self._client.deleteUserPermission(name, _type)

    def listRoles(self):
        """
        Returns the names of all defined roles.
        """
        return self._client.listRoles()

    def addRole(self, role):
        """
        Creates a new role.
        """
        self._client.addRole(role)

    def deleteRole(self, role):
        """
        Deletes a role.
        """
        self._client.deleteRole(role)

    def listRolePermissions(self, role):
        """
        Lists the permission flags granted to a role.
        """
        return self._client.listRolePermissions(role)

    def addRolePermission(self, role, _type):
        """
        Grants a role a certain permission. type should be super, eval, or session.
        """
        self._client.addRolePermission(role, _type)

    def deleteRolePermission(self, role, _type):
        """
        Revokes a permission for a role.
        """
        self._client.deleteRolePermission(role, _type)

    def listRoleAccess(self, role):
        """
        Query the access granted to a role. Returns a result in the same
        format as the equivalent service for users.
        """
        return self._client.listRoleAccess(role)

    def addRoleAccess(self, role, read, write, catalog='*', repository='*'):
        """
        Grant read/write access to a role. See here for the parameters
        that are expected.
        """
        return self._client.addRoleAccess(role, read, write, catalog, repository)
        
    def deleteRoleAccess(self, role, read, write, catalog='*', repository='*'):
        """
        Revoke read/write access for a role. Accepts the same parameters as above.
        """
        self._client.deleteRoleAccess(role, read, write, catalog, repository)

    def listUserRoles(self, name):
        """
        Retrieves a list of role names for this user name.
        """
        return self._client.listUserRoles(name)

    def addUserRole(self, name, role):
        """
        Add a role to a user.
        """
        self._client.addUserRole(name, role)

    def deleteUserRole(self, name, role):
        """
        Remove a role from a user.
        """
        self._client.deleteUserRole(name, role)

    def listUserSecurityFilters(self, name, _type):
        """
        List security filters for user.

        _type is one of "allow", "disallow"
        """
        return self._client.listUserSecurityFilters(name, _type)

    def addUserSecurityFilter(self, name, _type, s=None, p=None, o=None, g=None):
        """
        Add a security filter for the user.

        name - user name
        _type - one of 'allow' or 'disallow'
        s - optional subject
        p - optional predicate
        o - optional predicate
        g - optional graph
        """
        self._client.addUserSecurityFilter(name, _type, s, p, o, g)

    def deleteUserSecurityFilter(self, name, _type, s=None, p=None, o=None, g=None):
        """
        Add a security filter for the user.

        name - user name
        _type - one of 'allow' or 'disallow'
        s - optional subject
        p - optional predicate
        o - optional predicate
        g - optional graph
        """
        self._client.deleteUserSecurityFilter(name, _type, s, p, o, g)

    def listRoleSecurityFilters(self, role, _type):
        """
        List security filters for user.

        _type is one of "allow", "disallow"
        """
        return self._client.listRoleSecurityFilters(role, _type)

    def addRoleSecurityFilter(self, role, _type, s=None, p=None, o=None, g=None):
        """
        Add a security filter for the user.

        role - role name
        _type - one of 'allow' or 'disallow'
        s - optional subject
        p - optional predicate
        o - optional predicate
        g - optional graph
        """
        self._client.addRoleSecurityFilter(role, _type, s, p, o, g)

    def deleteRoleSecurityFilter(self, role, _type, s=None, p=None, o=None, g=None):
        """
        Add a security filter for the user.

        role - role name
        _type - one of 'allow' or 'disallow'
        s - optional subject
        p - optional predicate
        o - optional predicate
        g - optional graph
        """
        self._client.deleteRoleSecurityFilter(role, _type, s, p, o, g)


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

    def createRepository(self, name, indices=None):
        """
        createsRepository - makes a new repository with the given name.

        indices - if provided, creates a store with the given indices
        deleteDuplicates - sets behavior for duplicate removal. See
            http protocol documentation for description. None will
            result in using the server's default behavior.
        """
        return Repository(self, name, self.mini_catalog.createRepository(name, indices=indices))


