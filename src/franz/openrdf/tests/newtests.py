###############################################################################
# Copyright (c) 2006-2016 Franz Inc.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the Eclipse Public License v1.0
# which accompanies this distribution, and is available at
# http://www.eclipse.org/legal/epl-v0.html
###############################################################################
import pytest

from franz.openrdf.connect import ag_connect

from .tests import AG_HOST, AG_PORT, CATALOG, STORE, USER, PASSWORD

common_args = dict(
    host=AG_HOST, port=AG_PORT, catalog=CATALOG, user=USER, password=PASSWORD
)

def test_ag_connect_open(clean_repo):
    del clean_repo
    with ag_connect(STORE, create=False, **common_args) as conn:
        assert conn.size() == 0


def test_ag_connect_create(non_existing_repo):
    with ag_connect(non_existing_repo, create=True, **common_args) as conn:
        assert conn.size() == 0


def test_ag_connect_recreate(conn):
    with conn:
        conn.addTriple('<http://franz.com/s>', '<http://franz.com/p>', '<http://franz.com/o>')
    with ag_connect(STORE, clear=True, **common_args) as conn:
        assert conn.size() == 0


def test_ag_connect_open_no_create(non_existing_repo):
    with pytest.raises(Exception):
        ag_connect(non_existing_repo, create=False, **common_args)


def test_ag_connect_create_exists():
    with pytest.raises(Exception):
        ag_connect(STORE, create=True, fail_if_exists=True, **common_args)


def test_ag_connect_fail_if_exists_but_not_create(clean_repo):
    del clean_repo
    with ag_connect(STORE, create=False, fail_if_exists=True, **common_args) as conn:
        assert conn.size() == 0
