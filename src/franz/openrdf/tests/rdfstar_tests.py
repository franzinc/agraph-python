import itertools
import os
from pathlib import Path

import pytest

from franz.openrdf.connect import AllegroGraphServer, Repository
from franz.openrdf.exceptions import (
    IllegalArgumentException,
    RequestError,
    ServerException,
)
from franz.openrdf.model import BNode, Literal, QuotedTriple
from franz.openrdf.rio.rdfformat import RDFFormat
from franz.openrdf.tests.conftest import ex, min_version

pytestmark = min_version(8, 0)


def get_file_path(name: str) -> str:
    workdir = Path(__file__).parent
    return str(workdir.joinpath(name))


def get_file_content(name: str) -> str:
    workdir = Path(__file__).parent
    with open(workdir.joinpath(name), "r") as f:
        return f.read()


@pytest.fixture
def server():
    host = os.environ.get("AGRAPH_HOST", "localhost")
    port = int(os.environ.get("AGRAPH_PORT", "10035"))
    user = os.environ.get("AGRAPH_USER", "test")
    password = os.environ.get("AGRAPH_PASSWORD", "xyzzy")

    server = AllegroGraphServer(host=host, port=port, user=user, password=password)
    yield server


@pytest.fixture
def conn(server, name="agraph_test_rdfstar"):
    catalog = server.openCatalog("tests")
    repo = catalog.getRepository(name=name, access_verb=Repository.RENEW)
    conn = repo.getConnection()

    yield conn

    conn.close()
    catalog.deleteRepository(name)


def test_enable_rdfstar(conn):
    assert conn.RDFStarEnabled() is False

    conn.enableRDFStar()
    assert conn.RDFStarEnabled() is True


def test_disable_rdfstar(conn):
    assert conn.RDFStarEnabled() is False

    conn.enableRDFStar()
    conn.disableRDFStar()

    assert conn.RDFStarEnabled() is False


def test_createQuotedTriple_without_rdfstar_enabled(conn, ex):
    with pytest.raises(ServerException):
        conn.createQuotedTriple(ex("alice"), ex("name"), Literal("Alice"))


def test_createQuotedTriple(conn, ex):
    conn.enableRDFStar()
    quoted = conn.createQuotedTriple(ex("alice"), ex("name"), Literal("Alice"))

    assert quoted == QuotedTriple(ex("alice"), ex("name"), Literal("Alice"))


def test_createQuotedTriple_with_illegal_arguments(conn, ex):
    conn.enableRDFStar()

    with pytest.raises(IllegalArgumentException):
        conn.createQuotedTriple("alice", ex("name"), Literal("Alice"))


def test_quoted_triple_getters(ex):
    quoted = QuotedTriple(ex("alice"), ex("name"), Literal("Alice"))

    assert quoted.getSubject() == ex("alice")
    assert quoted.getPredicate() == ex("name")
    assert quoted.getObject() == Literal("Alice")


def test_quoted_triple_eq(ex):
    s, p, o = ex("alice"), ex("name"), Literal("Alice")
    quoted1 = QuotedTriple(s, p, o)
    assert quoted1 == QuotedTriple(s, p, o)

    s, p, o = ex("alice"), ex("firstName"), Literal("Alice")
    quoted2 = QuotedTriple(s, p, o)
    assert quoted2 == QuotedTriple(s, p, o)

    s, p, o = ex("bob"), ex("name"), Literal("Bob")
    quoted3 = QuotedTriple(s, p, o)
    assert quoted3 == QuotedTriple(s, p, o)

    s, p, o = BNode(), ex("name"), Literal("Anonymous")
    quoted4 = QuotedTriple(s, p, o)
    assert quoted4 == QuotedTriple(s, p, o)

    s, p, o = quoted1, ex("ontology#"), quoted3
    quoted5 = QuotedTriple(s, p, o)
    assert quoted5 == QuotedTriple(s, p, o)

    for lhs, rhs in itertools.permutations(
        (quoted1, quoted2, quoted3, quoted4, quoted5), r=2
    ):
        assert lhs != rhs


def test_turtle_star_import_without_rdfstar_enabled(conn):
    with pytest.raises(RequestError):
        conn.addFile(get_file_path("turtle-star.ttls"), format=RDFFormat.TURTLE)


def test_turtle_star_import_rdfstar(conn):
    conn.enableRDFStar()
    conn.addFile(get_file_path("turtle-star.ttls"), format=RDFFormat.TURTLE)
    query = """SELECT (COUNT(*) AS ?count) WHERE { ?s ?p ?o . }"""
    with conn.prepareTupleQuery(query=query).evaluate() as res:
        for bindings in res:
            assert bindings.getValue("count") == Literal(2)
            return


def test_turtle_star_import_from_string_without_rdfstar_enabled(conn):
    with pytest.raises(RequestError):
        conn.addData(get_file_content("turtle-star.ttls"), RDFFormat.TURTLE)


def test_turtle_star_import_from_string_rdfstar(conn):
    conn.enableRDFStar()
    conn.addData(get_file_content("turtle-star.ttls"), RDFFormat.TURTLE)
    query = """SELECT (COUNT(*) AS ?count) WHERE { ?s ?p ?o . }"""
    with conn.prepareTupleQuery(query=query).evaluate() as res:
        for bindings in res:
            assert bindings.getValue("count") == Literal(2)
            return


def test_trig_star_import_without_rdfstar_enabled(conn):
    with pytest.raises(RequestError):
        conn.addFile(get_file_path("trig-star.trig"), format=RDFFormat.TRIG)


def test_trig_star_import_rdfstar(conn):
    conn.enableRDFStar()
    conn.addFile(get_file_path("trig-star.trig"), format=RDFFormat.TRIG)
    query = """SELECT (COUNT(*) AS ?count) WHERE { ?s ?p ?o . }"""
    with conn.prepareTupleQuery(query=query).evaluate() as res:
        for bindings in res:
            assert bindings.getValue("count") == Literal(2)
            return


def test_trig_star_import_from_string_without_rdfstar_enabled(conn):
    with pytest.raises(RequestError):
        conn.addData(get_file_content("trig-star.trig"), RDFFormat.TRIG)


def test_trig_star_import_from_string_rdfstar(conn):
    conn.enableRDFStar()
    conn.addData(get_file_content("trig-star.trig"), RDFFormat.TRIG)
    query = """SELECT (COUNT(*) AS ?count) WHERE { ?s ?p ?o . }"""
    with conn.prepareTupleQuery(query=query).evaluate() as res:
        for bindings in res:
            assert bindings.getValue("count") == Literal(2)
            return


def test_addTriple_without_rdfstar_enabled(conn, ex):
    quoted = QuotedTriple(ex("bob"), ex("age"), Literal(23))
    with pytest.raises(RequestError):
        conn.addTriple(quoted, ex("certainty"), Literal(0.9))


def test_addTriple_rdfstar(conn, ex):
    conn.enableRDFStar()
    quoted = QuotedTriple(ex("bob"), ex("age"), Literal(23))
    conn.addTriple(quoted, ex("certainty"), Literal(0.9))

    assert True


def test_disable_rdfstar_when_repository_is_not_empty(conn):
    conn.enableRDFStar()
    conn.addFile(get_file_path("turtle-star.ttls"), format=RDFFormat.TURTLE)
    with pytest.raises(RequestError):
        conn.disableRDFStar()


def test_getStatements_rdfstar(conn, ex):
    conn.enableRDFStar()
    s, p, o = ex("bob"), ex("age"), Literal(23)
    conn.addTriple(s, p, o)
    conn.addTriple(QuotedTriple(s, p, o), ex("certainty"), Literal(0.9))

    assert len(conn.getStatements()) == 2

    res = conn.getStatements(s).asList()
    assert len(res) == 1
    assert res[0] == conn.createStatement(s, p, o)

    res = conn.getStatements(QuotedTriple(s, p, o)).asList()
    statement = conn.createStatement(
        QuotedTriple(s, p, o), ex("certainty"), Literal(0.9)
    )
    assert len(res) == 1
    assert isinstance(res[0].getSubject(), QuotedTriple)
    assert res[0].getSubject() == statement.getSubject()


def test_sparql_star_query(conn, ex):
    conn.enableRDFStar()
    s, p, o = ex("bob"), ex("age"), Literal(23)
    conn.addTriple(s, p, o)
    conn.addTriple(QuotedTriple(s, p, o), ex("certainty"), Literal(0.9))

    query = """PREFIX ex: <ex://>
SELECT ?quoted WHERE { ?quoted ex:certainty 0.9 . }"""
    with conn.prepareTupleQuery(query=query).evaluate() as res:
        for bindings in res:
            quoted = bindings.getValue("quoted")
            assert quoted == QuotedTriple(ex("bob"), ex("age"), Literal(23))

    query = """PREFIX ex: <ex://>
SELECT ?bob ?age ?certainty WHERE { << ?bob ex:age ?age >> ex:certainty ?certainty . }"""
    with conn.prepareTupleQuery(query=query).evaluate() as res:
        for bindings in res:
            assert bindings.getValue("bob") == ex("bob")
            assert bindings.getValue("age") == Literal(23)
            assert bindings.getValue("certainty").toPython() == 0.9
