#
# pytest franz/openrdf/tests/fedshard.py
#


import os

import pytest

from franz.openrdf.connect import ag_connect
from franz.openrdf.sail import AllegroGraphServer
from franz.openrdf.tests.conftest import min_version

pytestmark = min_version(8, 0)


@pytest.fixture
def server():
    host = os.environ.get("AGRAPH_HOST", "localhost")
    port = int(os.environ.get("AGRAPH_PORT", "10035"))
    user = os.environ.get("AGRAPH_USER", "test")
    password = os.environ.get("AGRAPH_PASSWORD", "xyzzy")

    server = AllegroGraphServer(host=host, port=port, user=user, password=password)
    yield server


@pytest.fixture
def repositoryconn():
    yield ag_connect("secondtestdef", catalog="fedshard", create=True)


fedshardef = """

fedshard
 repo testdef
 key part
 secondary-key graph
 shards-per-server 3
 scheme http
 port 10035
 user test
 password xyzzy

server
 host 127.1
 catalog tests

fedshard
 repo secondtestdef
 key part
 secondary-key graph
 shards-per-server 3
 scheme http
 port 10035
 user test
 password xyzzy

server
 host 127.1
 catalog tests

"""


def test_define_fedshards(server):
    res = server.defineFedshards(definitions=fedshardef, supersede=True)
    assert set(res) == set(["testdef", "secondtestdef"])


def test_delete_fedshard_definition(server):
    res = server.deleteFedshardDefinition(repo="testdef")
    assert res == "done"


def test_split_fedshard(repositoryconn):
    res = repositoryconn.splitFedshard(1)
    assert res == "done"
