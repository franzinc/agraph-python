#
# pytest franz/openrdf/tests/nlq-test.py
# This current test has a connection to an existing nlq vdb, but in the future
# we need to create our own vdb for testing purposes.
#

import os

import pytest
import requests

from franz.openrdf.connect import AllegroGraphServer, Repository, ag_connect
from franz.openrdf.exceptions import RequestError
from franz.openrdf.tests.conftest import min_version

HOST = os.environ.get("AGRAPH_HOST", "localhost")
PORT = int(os.environ.get("AGRAPH_PORT", "10035"))
USER = os.environ.get("AGRAPH_USER", "test")
PASSWORD = os.environ.get("AGRAPH_PASSWORD", "xyzzy")


###################################################
# This will need to be changed for 8.4.0 release  #
###################################################

if "OPENAI_API_KEY" in os.environ:
    # embed using openai text-embedding-ada-002
    EMBEDDER = os.environ.get("EMBEDDER", "openai")
    EMBEDDING_MODEL = os.environ.get("EMBEDDING_MODEL", "text-embedding-ada-002")
    API_KEY = os.environ.get("OPENAI_API_KEY")

    # first create repo
    conn = ag_connect(
        "test",
        create=True,
        clear=True,
        user=USER,
        password=PASSWORD,
        host=HOST,
        port=PORT,
    )

    # add some data
    conn.addFile("src/franz/openrdf/tests/kennedy.ttl")
    print(conn.size())

    # convert to vector store
    vdb_conn = ag_connect(
        "test_vdb",
        clear=True,
        create=True,
        user=USER,
        password=PASSWORD,
        host=HOST,
        port=PORT,
    )
    vdb_conn.convert_to_vector_store(EMBEDDER, api_key=API_KEY, model=EMBEDDING_MODEL)

    # create a linking repo (just connect if it already exists)
    connect_conn = ag_connect(
        "nlq-to-store-relationship", user=USER, password=PASSWORD, host=HOST, port=PORT
    )

    # add linking data
    connect_conn.executeUpdate(
        """
        insert data {
            <http://franz.com/test_example> <http://franz.com/vdb/gen/catalog> "/" ;
            <http://franz.com/vdb/gen/repository> "test" ;
            <http://franz.com/vdb/gen/nlq-vdb> "test_vdb" . }
        """
    )
    connect_conn.deleteDuplicates(mode="spo")

    # get shacl data
    url = (
        f"http://{USER}:{PASSWORD}@{HOST}:{PORT}/repositories/test/data-generator/shacl"
    )
    response = requests.get(url)

    # add shacl data to conn
    conn.addData(response.json())

    conn.close()
    vdb_conn.close()
    connect_conn.close()

#######END SETUP OF NLQ VDB CONNECTIONS########

pytestmark = min_version(8, 3)


@pytest.fixture
def repo_name():
    return "test"


@pytest.fixture
def nlq_repo():
    return "test_vdb"


@pytest.fixture
def regular_conn():
    conn = ag_connect("test", user=USER, password=PASSWORD, host=HOST, port=PORT)
    yield conn


@pytest.fixture
def nlq_conn():
    conn = ag_connect("test_vdb", user=USER, password=PASSWORD, host=HOST, port=PORT)
    yield conn


@pytest.fixture
def test_prompt():
    return "show me 10 triples"


@pytest.fixture
def test_query():
    return "SELECT * WHERE {?s ?p ?o} LIMIT 10"


# Check for SHACL presence
@pytest.mark.skipif(
    "OPENAI_API_KEY" not in os.environ,
    reason="OPENAI_API_KEY environment variable not set",
)
def test_check_shacl(regular_conn):
    query_string = """
        ASK WHERE {
            {
                ?shape a <http://www.w3.org/ns/shacl#NodeShape> .
            } UNION {
                ?shape a <http://www.w3.org/ns/shacl#PropertyShape> .
            }
            }"""
    result = regular_conn.executeBooleanQuery(query_string)
    regular_conn.close()
    assert result == True


# perform an nlq query


@pytest.mark.skipif(
    "OPENAI_API_KEY" not in os.environ,
    reason="OPENAI_API_KEY environment variable not set",
)
def test_nlq_query(regular_conn, nlq_repo, test_prompt):
    result = regular_conn.execute_nl_query(test_prompt, nlq_repo)
    regular_conn.close()
    assert type(result["query"]) == str
    assert type(result["result"]) == dict


# store an nlq pair


@pytest.mark.skipif(
    "OPENAI_API_KEY" not in os.environ,
    reason="OPENAI_API_KEY environment variable not set",
)
def test_store_nlq_pair(nlq_conn, test_prompt, test_query):
    nlq_conn.store_nl_query_pair(test_prompt, test_query)

    query_string = f"""
        ASK WHERE {{
            ?thing <http://franz.com/vdb/gen/text> "{test_prompt}" .
        }}"""
    result = nlq_conn.executeBooleanQuery(query_string)
    assert result == True

    query_string = f"""
        ASK WHERE {{
            ?thing <http://franz.com/sparql_llm/querySparql> "{test_query}" .
        }}"""
    result = nlq_conn.executeBooleanQuery(query_string)
    nlq_conn.close()
    assert result == True


# get all pairs


@pytest.mark.skipif(
    "OPENAI_API_KEY" not in os.environ,
    reason="OPENAI_API_KEY environment variable not set",
)
def test_get_pairs(nlq_conn):
    pairs = nlq_conn.get_nl_query_pairs()
    nlq_conn.close()
    assert type(pairs) == list

    for pair in pairs:
        assert type(pair) == dict
        assert "id" in pair
        assert "nl-query" in pair
        assert "sparql-query" in pair


# find the pair that was added and delete it


@pytest.mark.skipif(
    "OPENAI_API_KEY" not in os.environ,
    reason="OPENAI_API_KEY environment variable not set",
)
def test_delete_pair(nlq_conn, test_prompt):
    pairs = nlq_conn.get_nl_query_pairs()
    len_pairs = len(pairs)
    assert len(pairs) > 0

    for pair in pairs:
        if pair["nl-query"] == test_prompt:
            id = pair["id"]
            break

    nlq_conn.delete_nl_query_pairs([id])

    pairs = nlq_conn.get_nl_query_pairs()
    nlq_conn.close()
    new_len_pairs = len(pairs)
    assert len_pairs > new_len_pairs


# delete repositories
@pytest.mark.skipif(
    "OPENAI_API_KEY" not in os.environ,
    reason="OPENAI_API_KEY environment variable not set",
)
def test_delete_repositories(repo_name, nlq_repo):
    server = AllegroGraphServer(host=HOST, port=PORT, user=USER, password=PASSWORD)
    catalog = server.openCatalog()
    catalog.deleteRepository(repo_name)
    catalog.deleteRepository(nlq_repo)
