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
from builtins import str
from builtins import map
from builtins import object
from future import standard_library
standard_library.install_aliases()

import os

from ..exceptions import ServerException
from ..repository.repository import Repository, RepositoryConnection
from ...miniclient import repository as miniserver
import re, urllib.request, urllib.parse, urllib.error
from . import spec
from past.builtins import basestring

READ_ONLY = 'READ_ONLY'

LEGAL_OPTION_TYPES = {READ_ONLY: bool,}

ROOT_CATALOG_NAME = 'root'

class AllegroGraphServer(object):
    """
    The AllegroGraphServer object represents a remote AllegroGraph server on
    the network. It is used to inventory and access the catalogs of that
    server.
    """
    def __init__(self, host=None, port=None, user=None, password=None,
                 cainfo=None, sslcert=None, verifyhost=None, verifypeer=None,
                 protocol=None,
                 proxy=os.environ.get('AGRAPH_PROXY'),
                 **options):
        """
        Define the connection to the AllegroGraph HTTP server.

        Pass either ``user`` and ``password`` for Basic Authentication or
        ``cainfo``, ``sslcert`` values for client X.509 certificate
        authentication.

        :param host: Address of the AllegroGraph server to connect to.
                     This can also include protocol and port
                     (e.g. http://localhost:10035). Can also be specified in
                     the ``AGRAPH_HOST`` environment variable.
                     The default value is ``'localhost'```.
        :type host: string
        :param port: Port on which the server is listening.
                     The default is 10035 if ``protocol`` is ``'http'``
                     and 10036 if it is ``'https'``. Can also be specified in
                     the ``AGRAPH_PORT`` environment variable.
                     If passed explicitly this parameter overrides any value
                     that might have been specified as a part of the ``host`` string.
        :type port: int
        :param protocol: Connection protocol, either ``'http'`` or ``'https'``.
                         The default is ``'http'`` if no SSL parameters are set
                         and `'https'`` otherwise.
                         If passed explicitly this parameter overrides any value
                         that might have been specified as a part of the ``host`` string.
        :type protocol: string
        :param user: Username (when using Basic authentication). Can also be specified in
                     the ``AGRAPH_USER`` environment variable.
        :type user: string
        :param password: Password (when using Basic authentication). Can also be specified in
                         the ``AGRAPH_PASSWORD`` environment variable.
        :type password: string
        :param cainfo: Path to a file or directory containing CA certificates that
                       will be used to validate the server's certificate.
        :type cainfo: string
        :param sslcert: Client certificate path (when using SSL authentication).
        :type sslcert: string
        :param verifyhost: If set to ``0`` it will not be an error if the server's
                           SSL certificate does not match the server's address.
                           The default value is ``2``, meaning that the host name will
                           be validated against the certificate.

                           ..seealso:: https://curl.haxx.se/libcurl/c/CURLOPT_SSL_VERIFYHOST.html
        :type verifyhost: int
        :param verifypeer: If set to ``1`` (the default) the validity of the server's
                           SSL certificate will be checked. Set to ``0`` to disable
                           the validation.

                           ..seealso:: https://curl.haxx.se/libcurl/c/CURLOPT_SSL_VERIFYPEER.html
        :type verifypeer: int
        :param proxy: Proxy specification string. The format is SCHEME://HOST:PORT.
                      Supported schemes are 'http', 'socks4' and 'socks5'.
                      Note that for SOCKS proxies DNS requests are performed by the
                      proxy server.
                      The default value is taken from the AGRAPH_PROXY environment
                      variable.
        :type proxy: string
        :param options: Ignored.
        """
        # Not sure why we accept these, but don't want to change the API at this point.
        del options

        host = host or os.environ.get('AGRAPH_HOST', '127.0.0.1')

        # Check if other arguments were passed as a part of host
        match = re.match(r'^(?:(?P<protocol>https?)://)?'
                         r'(?P<host>[^:]*)(?::(?P<port>[0-9]*))?(?P<tail>.*)$',
                         host)
        if match:
            if protocol is None:
                protocol = match.group('protocol')
            if port is None and match.group('port') is not None:
                port = int(match.group('port'))
            host = match.group('host')
            tail = match.group('tail')
        else:
            tail = ''

        has_https_params = cainfo or sslcert or verifyhost is not None or verifypeer is not None
        if protocol is None:
            protocol = 'https' if has_https_params else 'http'

        if port is None:
            if 'AGRAPH_PORT' in os.environ:
                port = int(os.environ.get('AGRAPH_PORT'))
            else:
                port = 10035 if protocol == 'http' else 10036

        uri = '{protocol}://{host}:{port}{tail}'.format(protocol=protocol, host=host, port=port, tail=tail)

        user = user or os.environ.get('AGRAPH_USER')
        password = password or os.environ.get('AGRAPH_PASSWORD')

        self._client = miniserver.Client(uri, user, password, cainfo, sslcert, verifyhost, verifypeer,
                                         proxy=proxy)

    @property
    def url(self):
        """Return the server's URL."""
        return self._client.url

    @property
    def version(self):
        """Return the server's version as a string."""
        return self._client.getVersion()

    @property
    def versionTuple(self):
        """
        Return the version number as a tuple of integers.

        All non-digit characters from the version string are ignored
        and act only as separators. All trailing zeros are skipped.
        This is compatible with the way in which the Java client
        parses and compares AG version strings.
        """
        components = [int(c) for c in re.findall(r'\d+', self.version)]
        while components and components[-1] == 0:
            components.pop()
        return tuple(components)

    def listCatalogs(self):
        """
        Get the list of catalogs on this server.

        A value of ``None`` will be included in this list to
        represent the root catalog.

        :return: A list of catalog names.
        :rtype: list[string]
        """
        catalogs = self._client.listCatalogs()
        return catalogs

    def openCatalog(self, name=None):
        """
        Open a catalog by name. Pass None to open the root catalog.

        :param name: One of the catalog names from :meth:`listCatalogs`
                     or ``None`` to open the root catalog.
        :return: A catalog object.
        :rtype: Catalog
        """
        # Allow for 'root', None and '' (or anything else that evaluates to
        # False) to mean the root catalog.
        if not name or name == ROOT_CATALOG_NAME:
            name = None

        cats = self.listCatalogs()
        if not name in cats:
            raise ServerException("There is no catalog named '%s' (found %s)"
                % (name, cats))

        return Catalog(name, self, self._client)

    def getInitfile(self):
        """
        Retrieve the contents of the server initialization file.

        The initialization file is a collection of Common Lisp code
        that is executed in every back-end as it is created.
        :return: Init file content.
        :rtype: string
        """
        return self._client.getInitfile()

    def setInitfile(self, content=None, restart=True):
        """
        Replace the current initialization file contents with the
        ``content`` string or remove if None.

        :param content: New init file conent.
        :type content: string
        :param restart: specifies whether any running shared back-ends should
                        be shut down, so that subsequent requests will be handled by
                        back-ends that include the new code. Default is ``True``.
        """
        return self._client.setInitfile(content, restart)

    def openSession(self, spec, autocommit=False, lifetime=None, loadinitfile=False):
        """
        Open a session on a federated, reasoning, or filtered store.

        Use the helper functions in the :mod:`franz.openrdf.sail.spec` module
        to create the spec string.

        :param spec: Session :mod:`specification string <franz.openrdf.sail.spec>`.
        :type spec: string
        :param autocommit: Autocommit mode (default: ``False`` = start a transaction).
        :type autocommit: bool
        :param lifetime: The number of seconds a session can be idle before being terminated.
                         The default value is 300 (5 minutes). The maximum acceptable value
                         is 21600 (6 hours).
        :param loadinitfile: If ``True`` the init file will be loaded into the new session.
                             The default is False.
        :type loadinitfile: bool
        :return: A connection to the new session.
        :rtype: RepositoryConnection
        """
        minirep = self._client.openSession(spec, autocommit=autocommit, lifetime=lifetime, loadinitfile=loadinitfile)
        return RepositoryConnection(Repository(None, None, minirep), is_session=True)

    def listScripts(self):
        """
        Return the list of Sitescripts currently on the server.

        When a user creates a session, they can choose to load one or more
        of these scripts into the session.

        :return: A list of script names.
        :rtype: list[string]
        """
        return self._client.listScripts()

    def addScript(self, module, code):
        """
        Create or replace a sitescript.

        :param module: Script name.
        :type module: string
        :param code: Script content.
        :type code: string
        """
        return self._client.addScript(module, code)

    def deleteScript(self, module):
        """
        Delete a sitescript.
        :param module: Script name.
        :type module: string
        """
        return self._client.deleteScript(module)

    def getScript(self, module):
        """
        Get the body of a sitescript.
        :param module: Script name.
        :type module: string
        :return: Script content.
        :rtype: string
        """
        return self._client.getScript(module)

    def getUserData(self, key):
        """
        Retrieve user data value with given key.

        Users can store arbitrary strings on the server
        using :meth:`setUserData`.

        :param key: Value identifier.
        :type key: str
        :return: Stored string or None if there is no data under specified key.
        :rtype: str|None
        """
        return self._client.getUserData(key)

    def setUserData(self, key, data):
        """
        Set user data with given key.

        This can be used to store arbitrary strings on the server.

        :param key: Value identifier.
        :type key: str
        :param data: Value to be stored.
        :type data: str
        """
        self._client.setUserData(key, data)

    def deleteUserData(self, key):
        """
        Remove user data from the server.

        This removes a value set with :meth:`setUserData`

        :param key: Value identifier.
        :type key: str
        """
        self._client.deleteUserData(key)

    def listUserData(self):
        """
        Get all used keys from the user data store on the server.

        :return: A list of key names.
        :rtype: list[str]
        """
        return self._client.listUserData()

    def openFederated(self, repositories, autocommit=False, lifetime=None, loadinitfile=False):
        """
        Open a session that federates several repositories. The
        repositories argument must be an array containing store
        designators, which can be Repository or RepositoryConnection
        objects, strings (naming a store in the root catalog, or the
        URL of a store), or (storename, catalogname) tuples.

        :param repositories: List of repositories to federate.
        :type repositories: list[string|(string, string)|Repository|RepositoryConnection]
        :param autocommit: Autocommit mode (default: ``False`` = start a transaction).
        :type autocommit: bool
        :param lifetime: The number of seconds a session can be idle before being terminated.
                         The default value is 300 (5 minutes). The maximum acceptable value
                         is 21600 (6 hours).
        :param loadinitfile: If ``True`` the init file will be loaded into the new session.
                             The default is False.
        :type loadinitfile: bool
        :return: A connection to the new session.
        :rtype: RepositoryConnection
        """
        def asRepoString(x):
            if isinstance(x, basestring): return spec.local(x)
            elif isinstance(x, tuple): return spec.local(x[0], x[1])
            elif isinstance(x, Repository): return x.getSpec()
            elif isinstance(x, RepositoryConnection):
                if (x.repository.database_name == None):
                    raise ServerException(str(x) + " is not a RepositoryConnection created by Repository.getConnection() and therefore cannot be federated")
                return x.getSpec()
            else: raise TypeError(str(x) + " is not a valid repository specification.")
        return self.openSession(spec.federate(*list(map(asRepoString, repositories))), autocommit, lifetime, loadinitfile)

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
        Grant read/write access to a user.

        :param read: Whether to grant read access. Defaults to ``False``.
        :type read: bool
        :param write: Whether to grant write access. Defaults to ``False``.
        :type write: bool
        :param catalog: Which catalog to grant the access on. Leave off or pass ``"*"`` to grant
                        access on all catalogs. Use ``"/"`` for the root catalog.
        :type catalog: string
        :param repository: Specifies the repository that access is granted on. Passing ``"*"``,
                           or leaving the parameter off, means all repositories in the
                           given catalog.
        :type repository: string
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
    def __init__(self, name, server, client):
        self.server = server
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
        """
        Delete a repository.
        :param name: Repository name.
        :type name: string
        """
        return self.mini_catalog.deleteRepository(name)

    def getRepository(self, name, access_verb):
        """
        Creates or opens a repository.
        
        :param name: Repository name.
        :type name: string
        :param access_verb: Determines mode of operation. Possible values are:

                                - **Repository.RENEW** clears the contents of an existing
                                   repository before opening. If the indicated repository does not
                                   exist, it creates one.
                                -  **Repository.OPEN** opens an existing repository, or throws an
                                   exception if the repository is not found.
                                -  **Repository.ACCESS** opens an existing repository, or creates a
                                   new one if the repository is not found.
                                -  **Repository.CREATE** creates a new repository, or throws an
                                   exception if one by that name already exists.
        :return: A repository object.
        :rtype: Repository
        """
        access_verb = access_verb.upper()
        name = urllib.parse.quote_plus(name)
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
        Creates a new repository with the given name.

        :param name: Repository name.
        :type name: string
        :param indices: If provided, creates a store with the given indices.
                        (e.g. ``["spogi, "gspoi", ...]``)
        :type indices: list[string]
        :return: A repository object.
        :rtype: Repository
        """
        return Repository(self, name, self.mini_catalog.createRepository(name, indices=indices))
