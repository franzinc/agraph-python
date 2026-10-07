"""Shared test fixtures and configuration for GraphTalker client tests."""

import os

import pytest

from franz.graphtalker import GraphTalkerClient
from franz.openrdf.rio.rdfformat import RDFFormat
from franz.openrdf.sail.allegrographserver import AllegroGraphServer

KENNEDY_FILE = os.path.join(
    os.path.dirname(os.path.abspath(__file__)),
    os.pardir,
    os.pardir,
    "openrdf",
    "tests",
    "kennedy.ntriples",
)

# ---------------------------------------------------------------------------
# Fixtures for mocked tests (test_client.py, test_lisp.py)
# ---------------------------------------------------------------------------


@pytest.fixture
def base_url():
    return "http://localhost:8080"


@pytest.fixture
def client():
    """Create a GraphTalkerClient for mocked tests."""
    c = GraphTalkerClient(port=8080, api_key="test-key")
    yield c
    c.close()


# ---------------------------------------------------------------------------
# Integration test configuration (test_integration.py)
# ---------------------------------------------------------------------------


def pytest_addoption(parser):
    """Add command-line options for integration tests."""
    parser.addoption(
        "--run-integration",
        action="store_true",
        default=False,
        help="Run integration tests against live eval server",
    )
    parser.addoption("--eval-port", default="8080", help="Eval server port")
    parser.addoption("--eval-key", default="", help="Eval server API key")
    parser.addoption("--ag-host", default="localhost", help="AllegroGraph host")
    parser.addoption("--ag-port", default="10035", help="AllegroGraph port")
    parser.addoption("--ag-catalog", default="", help="AllegroGraph catalog")
    parser.addoption("--ag-repo", default="kennedy", help="AllegroGraph repository")
    parser.addoption("--ag-user", default="test", help="AllegroGraph user")
    parser.addoption("--ag-password", default="xyzzy", help="AllegroGraph password")


def pytest_configure(config):
    config.addinivalue_line("markers", "integration: mark test as integration test")


def pytest_collection_modifyitems(config, items):
    if not config.getoption("--run-integration"):
        skip = pytest.mark.skip(reason="Need --run-integration to run")
        for item in items:
            if "integration" in item.keywords:
                item.add_marker(skip)


@pytest.fixture(scope="module")
def kennedy_repository(request):
    """Ensure the data repository exists, loading the Kennedy dataset if needed.

    If the repository named by --ag-repo is missing, it is created, loaded
    with kennedy.ntriples and deleted on teardown. An existing repository
    is used as-is and left in place.
    """
    config = request.config
    name = config.getoption("--ag-repo")
    server = AllegroGraphServer(
        protocol="http",
        host=config.getoption("--ag-host"),
        port=int(config.getoption("--ag-port")),
        user=config.getoption("--ag-user"),
        password=config.getoption("--ag-password"),
    )
    catalog = server.openCatalog(config.getoption("--ag-catalog") or None)
    created = name not in catalog.listRepositories()
    if created:
        repo = catalog.createRepository(name)
        try:
            conn = repo.getConnection()
            try:
                conn.addFile(KENNEDY_FILE, format=RDFFormat.NTRIPLES)
            finally:
                conn.close()
        except Exception:
            catalog.deleteRepository(name)
            raise
    try:
        yield name
    finally:
        if created:
            catalog.deleteRepository(name)


@pytest.fixture(scope="module")
def integration_client(request, kennedy_repository):
    """Create a connected GraphTalkerClient for integration tests."""
    config = request.config
    c = GraphTalkerClient(
        port=int(config.getoption("--eval-port")),
        api_key=config.getoption("--eval-key"),
        default_query_timeout=300.0,
    )
    c.connect(
        "http",
        config.getoption("--ag-host"),
        int(config.getoption("--ag-port")),
        config.getoption("--ag-catalog"),
        kennedy_repository,
        config.getoption("--ag-user"),
        config.getoption("--ag-password"),
    )
    yield c
    c.close()
