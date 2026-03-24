"""Shared test fixtures and configuration for GraphTalker client tests."""

import pytest

from franz.graphtalker import GraphTalkerClient

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
    parser.addoption(
        "--ag-repo", default="hr-analytics", help="AllegroGraph repository"
    )
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
def integration_client(request):
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
        config.getoption("--ag-repo"),
        config.getoption("--ag-user"),
        config.getoption("--ag-password"),
    )
    yield c
    c.close()
