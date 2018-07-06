################################################################################
# Copyright (c) 2006-2017 Franz Inc.  
# All rights reserved. This program and the accompanying materials are
# made available under the terms of the MIT License which accompanies
# this distribution, and is available at http://opensource.org/licenses/MIT
################################################################################

"""
Test fixtures, moved to a separate file to avoid warnings
about parameter names shadowing global symbols.
"""
import contextlib
import random
from collections import MutableMapping

import pytest
import six

from franz.openrdf.model import URI
from franz.openrdf.repository import Repository
from franz.openrdf.repository.attributes import AttributeDefinition
from franz.openrdf.sail import AllegroGraphServer

from .tests import AG_HOST, AG_PORT, AG_PROXY, STORE, CATALOG, USER, PASSWORD
import os


# noinspection PyShadowingNames
@pytest.fixture
def conn(server):
    """
    Provides a connection to the test repository. The repository is cleared
    before the test, but not after it.
    """
    catalog = server.openCatalog(CATALOG)
    stores = catalog.listRepositories()

    # Instead of renewing the database, clear it.
    mode = Repository.CREATE if STORE not in stores else Repository.OPEN

    repo = catalog.getRepository(STORE, mode)
    repo.initialize()
    connection = repo.getConnection()

    if STORE in stores:
        connection.setUserAttributes({})
        connection.clearAttributeFilter()
        connection.clear()
        connection.clearNamespaces()
        connection.disableDuplicateSuppression()
        for attr in connection.getAttributeDefinitions():
            connection.deleteAttributeDefinition(attr.name)

    with connection:
        yield connection


# noinspection PyShadowingNames
@pytest.fixture
def repo_name(conn):
    """
    Make sure that a repository exists and is empty.

    Return the name of that repository.
    """
    name = conn.repository.database_name
    conn.close()
    return name


@pytest.fixture
def server():
    """
    A fixture that allows easy access to an AllegroGraphServer object.
    """
    yield AllegroGraphServer(AG_HOST, AG_PORT, USER, PASSWORD, proxy=AG_PROXY)


# noinspection PyShadowingNames
@pytest.fixture
def non_existing_repo(server):
    """
    A fixture that makes sure a repository does not exist before the test
    and also deletes it after the test.
    The value returned by this fixture is the name of the store.
    """
    catalog = server.openCatalog(CATALOG)
    store = 'temp-store'
    while store in catalog.listRepositories():
        store = 'temp-store-' + ''.join(random.choice('0123456789') for _ in range(16))
    yield store
    if store in catalog.listRepositories():
        catalog.deleteRepository(store)


# noinspection PyShadowingNames
@pytest.fixture
def session(conn):
    conn.openSession()
    yield conn
    conn.closeSession()


@pytest.fixture
def attr(conn):
    attr = AttributeDefinition(
        name='test', allowed_values=['a', 'b', 'c'],
        minimum_number=0, maximum_number=1)
    yield conn.setAttributeDefinition(attr)


@pytest.fixture
def attr2(conn):
    attr = AttributeDefinition(
        name='test2', allowed_values=['a', 'b', 'c'],
        minimum_number=0, maximum_number=None)
    yield conn.setAttributeDefinition(attr)


@pytest.fixture
def clean_env():
    old = os.environ
    os.environ = {}
    yield os.environ
    os.environ = old


# Injecting common URIs this way is less verbose


def make_uri_fixture(name):
    """
    Create a pytest fixture named NAME that resolves
    to a URI object '<ex://NAME>'.
    """
    # noinspection PyShadowingNames
    def func(conn):
        return conn.createURI('ex://' + name)
    func.__name__ = name
    return pytest.fixture(func, name=name)


def define_uri_fixtures():
    gl = globals()
    for uri in ('s', 'p', 'o', 'g', 'x'):
        for suffix in ('', '1', '2'):
            name = uri + suffix
            gl[name] = make_uri_fixture(name)


define_uri_fixtures()


@pytest.fixture
def ex():
    class Namespace(object):
        def __getattribute__(self, item):
            return URI(namespace='ex://', localname=item)

    return Namespace()


class UserData(MutableMapping):
    """
    A dictionary-like object that stores values in the user data
    service on an AllegroGraph server.

    This object keeps track of the values it changed. All changes
    can be undone by calling 'close'.
    """
    def __init__(self, server_object):
        """
        Initialize user data dictionary.

        :param server_object: AG server handle.
        :type server_object: AllegroGraphServer
        """
        self.server = server_object
        self.old_values = {}

    def __setitem__(self, k, v):
        if k not in self.old_values:
            self.old_values[k] = self.server.getUserData(k)
        self.server.setUserData(k, v)

    def __delitem__(self, k):
        if k not in self.old_values:
            self.old_values[k] = self.server.getUserData(k)
        self.server.deleteUserData(k)

    def __getitem__(self, k):
        return self.server.getUserData(k)

    def __len__(self):
        return len(self.server.listUserData())

    def __iter__(self):
        for k in self.server.listUserData():
            yield self.server.getUserData(k)

    def close(self):
        """
        Undo all changes made through this object.
        """
        for k, v in six.iteritems(self.old_values):
            if v is None:
                self.server.deleteUserData(k)
            else:
                self.server.setUserData(k, v)
        self.old_values.clear()


# noinspection PyShadowingNames
@pytest.fixture
def user_data(server):
    """
    A test fixture providing access to an instance of the UserData class.
    All user data changes are undone during cleanup.
    """
    with contextlib.closing(UserData(server)) as ud:
        yield ud
