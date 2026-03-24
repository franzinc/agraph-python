"""GraphTalker - Python client for GraphTalker eval server.

A high-level Pythonic API for interacting with AllegroGraph RDF databases
via Claude AI, powered by the GraphTalker Common Lisp system.

Example::

    from graphtalker import GraphTalkerClient

    with GraphTalkerClient(port=8080, api_key="my-key") as client:
        client.connect("http", "localhost", 10035, "", "hr-analytics", "test", "xyzzy")
        result = client.claude_query("Show me all the classes")
        print(result.answer)
"""

from franz.graphtalker.client import GraphTalkerClient
from franz.graphtalker.exceptions import (
    AuthenticationError,
    ConnectionError,
    EvalError,
    GraphTalkerError,
    QueryAbortedError,
    ServerError,
    TimeoutError,
)
from franz.graphtalker.models import (
    EvalResult,
    PendingVisualization,
    QueryResult,
    SessionInfo,
    TokenCostStats,
    Visualization,
)

__version__ = "0.1.0"

__all__ = [
    "GraphTalkerClient",
    "EvalResult",
    "TokenCostStats",
    "QueryResult",
    "PendingVisualization",
    "Visualization",
    "SessionInfo",
    "GraphTalkerError",
    "ConnectionError",
    "AuthenticationError",
    "EvalError",
    "QueryAbortedError",
    "TimeoutError",
    "ServerError",
]
