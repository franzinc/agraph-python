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
import random

import pytest

from franz.openrdf.repository import Repository
from franz.openrdf.sail import AllegroGraphServer

from .tests import AG_HOST, AG_PORT, AG_PROXY, STORE, CATALOG, USER, PASSWORD


@pytest.fixture
def conn():
    """
    Provides a connection to the test repository. The repository is cleared
    before the test, but not after it.
    """
    server = AllegroGraphServer(AG_HOST, AG_PORT, USER, PASSWORD, proxy=AG_PROXY)
    catalog = server.openCatalog(CATALOG)
    stores = catalog.listRepositories()

    # Instead of renewing the database, clear it.
    mode = Repository.CREATE if STORE not in stores else Repository.OPEN

    repo = catalog.getRepository(STORE, mode)
    repo.initialize()
    connection = repo.getConnection()

    if STORE in stores:
        connection.clear()
        connection.clearNamespaces()

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
def non_existing_repo():
    """
    A fixture that makes sure a repository does not exist before the test
    and also deletes it after the test.
    The value returned by this fixture is the name of the store.
    """
    server = AllegroGraphServer(AG_HOST, AG_PORT, USER, PASSWORD, proxy=AG_PROXY)
    catalog = server.openCatalog(CATALOG)
    store = 'temp-store'
    while store in catalog.listRepositories():
        store = 'temp-store-' + ''.join(random.choice('0123456789') for _ in range(16))
    yield store
    if store in catalog.listRepositories():
        catalog.deleteRepository(store)