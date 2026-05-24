#
# pytest franz/openrdf/tests/vectordb-test.py
#


import os

import pytest

from franz.openrdf.connect import AllegroGraphServer, Repository, ag_connect
from franz.openrdf.exceptions import RequestError
from franz.openrdf.rio.rdfformat import RDFFormat
from franz.openrdf.tests.conftest import min_version

# vector dbs started in ag 8.0
# but much more working in 8.3 and some of
# these tests won't work in versions earlier than 8.3
# compact_embeddings param supported in 9.0
pytestmark = min_version(8, 3)


globals_dict = {"size0": 0, "triples_per_object": 0}


@pytest.fixture
def globals():
    return globals_dict


@pytest.fixture
def repo_name():
    return "agraph_test_vdb"


@pytest.fixture
def repo_catalog():
    return "tests"


@pytest.fixture
def server():
    host = os.environ.get("AGRAPH_HOST", "localhost")
    port = int(os.environ.get("AGRAPH_PORT", "10035"))
    user = os.environ.get("AGRAPH_USER", "test")
    password = os.environ.get("AGRAPH_PASSWORD", "xyzzy")

    server = AllegroGraphServer(host=host, port=port, user=user, password=password)
    yield server


@pytest.fixture
def conn(server, repo_name, repo_catalog):
    catalog = server.openCatalog(repo_catalog)
    repo = catalog.getRepository(repo_name, Repository.OPEN)
    conn = repo.getConnection()

    yield conn

    conn.close()


#    To be tidy we could delete the repo at this point but if
#    a test reports an error then looking inside the repo will be
#    very helpful
#    catalog.deleteRepository(name)


def test_create_repo(server, repo_name, repo_catalog, globals):
    catalog = server.openCatalog(repo_catalog)
    catalog.createRepository(
        name=repo_name, vector_store=True, embedder="demo", dimensions=500
    )


def test_add_objects(conn, globals):
    # see how many triples per object added

    size0 = globals["size0"] = conn.size()

    conn.add_objects("object1")
    size1 = conn.size()

    triples_per_object = size1 - size0
    globals["triples_per_object"] = triples_per_object

    conn.add_objects(("object2", "object3", "object4"))
    size2 = conn.size()

    # verify we've added four triples total
    assert (size2 - size0) == (4 * triples_per_object)

    # add one object with two properties
    conn.add_objects("object5", properties={"pa": "va", "pb": "vb"})
    size3 = conn.size()

    # test that there are 5 objects and 2 properties
    assert (size3 - size0) == 5 * triples_per_object + 2


def test_nearest_neighbor(conn):
    # just call this to see if there's an error
    conn.nearest_neighbor("foo", minScore=0.1, topN=2)


def test_remove_objects(conn, globals):
    size0 = globals["size0"]
    triples_per_object = globals["triples_per_object"]

    conn.remove_objects("object3")
    size4 = conn.size()

    # verify we now have 4 objects and 2 properties
    assert (size4 - size0) == 4 * triples_per_object + 2

    # remove object with 2 properties
    conn.remove_objects("object5")
    size5 = conn.size()

    # verify we have 3 objects and no properties
    assert (size5 - size0) == 3 * triples_per_object

    # remove all objects
    conn.remove_objects(all=True)
    size6 = conn.size()

    assert size6 == size0


def test_object_text_and_props(conn):
    objecta = conn.add_objects(
        "try-text", properties={"propa": "vala", "propb": "valb"}
    )

    assert conn.object_text(objecta) == "try-text"

    # look for literals returned in ntriple syntax
    assert conn.object_property_value(objecta, "propa") == '"vala"'
    assert conn.object_property_value(objecta, "propb") == '"valb"'


def test_object_embedding(conn):
    objecta = conn.add_objects("fooembedded")

    # demo embedding is 1000 elements long
    assert len(conn.object_embedding(objecta)) == 500


def test_convert(server):
    name = "vdb-convert"
    catalog = server.openCatalog("tests")
    repo = catalog.createRepository(name=name)
    conn = repo.getConnection()

    size0 = conn.size()
    conn.convert_to_vector_store("demo", api_key="not needed", dimensions=600)
    size1 = conn.size()

    assert size1 > size0

    # will fail if not a vector store
    conn.add_objects("foo")
    size2 = conn.size()

    assert size2 > size1

    catalog.deleteRepository(name)


def test_ag_connect(server):
    #
    # verify that ag_connect can be used to create
    # a vector database
    #

    name = "vdb-ag-con"
    catalog = "tests"

    conn = ag_connect(
        name,
        catalog=catalog,
        create=True,
        host=os.environ.get("AGRAPH_HOST", "localhost"),
        port=int(os.environ.get("AGRAPH_PORT", "10035")),
        user=os.environ.get("AGRAPH_USER", "test"),
        password=os.environ.get("AGRAPH_PASSWORD", "xyzzy"),
        vector_store=True,
        embedder="demo",
        dimensions=700,
    )

    assert conn.is_vector_store()

    conn.remove_objects(all=True)
    size1 = conn.size()

    #
    # if this is vector database the this add_objects
    # will work
    #
    conn.add_objects("foo")
    size2 = conn.size()

    assert size2 > size1

    conn.close()

    server.openCatalog(catalog).deleteRepository(name)


def test_add_objects_normal_repo(server, repo_catalog):
    # try to add an object to a normal repo
    # this will fail
    name = "compact-embeddings"
    catalog = server.openCatalog(repo_catalog)
    repo = catalog.createRepository(name=name)

    conn = repo.getConnection()

    with pytest.raises(RequestError):
        conn.add_objects("foo")


@min_version(9, 0)
def test_ag_connect_compact(server):
    #
    # verify that ag_connect can be used to create
    # a vector database
    #

    name = "vdb-ag-con"
    catalog = "tests"

    conn = ag_connect(
        name,
        catalog=catalog,
        create=True,
        host=os.environ.get("AGRAPH_HOST", "localhost"),
        port=int(os.environ.get("AGRAPH_PORT", "10035")),
        user=os.environ.get("AGRAPH_USER", "test"),
        password=os.environ.get("AGRAPH_PASSWORD", "xyzzy"),
        vector_store=True,
        embedder="demo",
        dimensions=700,
        compact_embeddings=True,
    )

    assert conn.is_vector_store()

    conn.remove_objects(all=True)
    size1 = conn.size()

    #
    # if this is vector database the this add_objects
    # will work
    #
    conn.add_objects("foo")
    size2 = conn.size()

    assert size2 > size1

    conn.close()

    server.openCatalog(catalog).deleteRepository(name)


indexdef = """
embed
 embedder "demo"
 vector-database-name *
 include-predicates <urn:label> 
 property fooprop fooval
 property barprop barval
"""


@min_version(9, 0)  # depends on vdb namespace being defined
def test_vector_index(server):
    dbname = "vt-index-sample"

    # this does not cause an exception if the repo does not exist
    AllegroGraphServer(
        host=os.environ.get("AGRAPH_HOST", "localhost"),
        port=int(os.environ.get("AGRAPH_PORT", "10035")),
        user=os.environ.get("AGRAPH_USER", "test"),
        password=os.environ.get("AGRAPH_PASSWORD", "xyzzy"),
    ).openCatalog().deleteRepository(dbname)

    # it's not possible to create an empty repo with just the
    # triples for the vector database in it if the repo already
    # is populated.   clear=True removes the vector db triples
    conn = ag_connect(
        "vt-index-sample",
        host=os.environ.get("AGRAPH_HOST", "localhost"),
        port=int(os.environ.get("AGRAPH_PORT", "10035")),
        user=os.environ.get("AGRAPH_USER", "test"),
        password=os.environ.get("AGRAPH_PASSWORD", "xyzzy"),
        create=True,
        vector_store=True,
        embedder="demo",
    )

    vsize = conn.size()  # get initial size

    conn.addData(
        """
 <urn:a> <urn:label> "fred" .
 <urn:b> <urn:label> "sam" .
 <urn:c> <urn:label> "zach" .
""",
        RDFFormat.NTRIPLES,
    )

    assert vsize + 3 == conn.size()

    assert "done" == conn.llm_index(indexdef)
    assert vsize + 31 == conn.size()

    conn.addData(
        """
 <urn:x> <urn:label> "mary" .
 <urn:y> <urn:label> "jane" .
""",
        RDFFormat.NTRIPLES,
    )

    assert vsize + 33 == conn.size()

    assert "2 items considered for update" == conn.update_index(True)
    assert vsize + 47 == conn.size()

    conn.removeTriples("<urn:b>", None, None)
    assert vsize + 46 == conn.size()

    print("update2: " + conn.update_index(True))
    assert vsize + 39 == conn.size()
