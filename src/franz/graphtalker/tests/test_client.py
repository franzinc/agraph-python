"""Integration tests for GraphTalkerClient using mocked HTTP responses.

These tests mock the eval server so no running server is needed.
"""

import json

import pytest
import responses

from franz.graphtalker import (
    AuthenticationError,
    ConnectionError,
    EvalError,
    GraphTalkerClient,
    QueryAbortedError,
    ServerError,
    TokenCostStats,
)

EVAL_URL = "http://localhost:8080/eval"
ABORT_URL = "http://localhost:8080/abort"
HEALTH_URL = "http://localhost:8080/health"


def eval_response(result="NIL", stdout="", error=None):
    """Helper to create a standard eval server JSON response."""
    return {"stdout": stdout, "result": result, "error": error}


# ---------------------------------------------------------------------------
# Health check
# ---------------------------------------------------------------------------


class TestHealthCheck:
    @responses.activate
    def test_healthy(self, client):
        responses.add(
            responses.GET,
            HEALTH_URL,
            json={"status": "ok"},
            status=200,
        )
        assert client.health_check() is True

    @responses.activate
    def test_unhealthy(self, client):
        responses.add(responses.GET, HEALTH_URL, status=500)
        assert client.health_check() is False


# ---------------------------------------------------------------------------
# eval()
# ---------------------------------------------------------------------------


class TestEval:
    @responses.activate
    def test_simple_eval(self, client):
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result="3"),
            status=200,
        )
        result = client.eval("(+ 1 2)")
        assert result.parsed == 3
        assert result.result == "3"
        assert result.stdout == ""

    @responses.activate
    def test_eval_with_output(self, client):
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result="42", stdout="Hello!\n"),
            status=200,
        )
        result = client.eval('(progn (format t "Hello!~%") 42)')
        assert result.parsed == 42
        assert result.stdout == "Hello!\n"

    @responses.activate
    def test_eval_error_raises(self, client):
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(error="Division by zero"),
            status=200,
        )
        with pytest.raises(EvalError, match="Division by zero"):
            client.eval("(/ 1 0)")

    @responses.activate
    def test_eval_error_includes_stdout(self, client):
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(error="Some error", stdout="partial output"),
            status=200,
        )
        with pytest.raises(EvalError) as exc_info:
            client.eval("(bad-code)")
        assert exc_info.value.stdout == "partial output"

    @responses.activate
    def test_auth_failure(self, client):
        responses.add(
            responses.POST,
            EVAL_URL,
            json={"error": "Unauthorized"},
            status=401,
        )
        with pytest.raises(AuthenticationError):
            client.eval("(+ 1 2)")

    @responses.activate
    def test_bad_request(self, client):
        responses.add(
            responses.POST,
            EVAL_URL,
            json={"error": "Bad Request", "message": "Invalid JSON"},
            status=400,
        )
        from franz.graphtalker.exceptions import GraphTalkerError

        with pytest.raises(GraphTalkerError, match="Invalid JSON"):
            client.eval("")

    @responses.activate
    def test_server_error(self, client):
        responses.add(
            responses.POST,
            EVAL_URL,
            body="Internal Server Error",
            status=500,
        )
        with pytest.raises(ServerError) as exc_info:
            client.eval("(+ 1 2)")
        assert exc_info.value.status_code == 500

    @responses.activate
    def test_sends_auth_header(self):
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result="3"),
            status=200,
        )
        client = GraphTalkerClient(port=8080, api_key="secret-123")
        client.eval("(+ 1 2)")
        assert (
            responses.calls[0].request.headers["Authorization"] == "Bearer secret-123"
        )
        client.close()

    @responses.activate
    def test_sends_expression(self):
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result="3"),
            status=200,
        )
        client = GraphTalkerClient(port=8080)
        client.eval("(+ 1 2)")
        body = json.loads(responses.calls[0].request.body)
        assert body["expression"] == "(+ 1 2)"
        client.close()


# ---------------------------------------------------------------------------
# Connection & Configuration
# ---------------------------------------------------------------------------


class TestConnection:
    @responses.activate
    def test_connect(self, client):
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(stdout="Connected to http://localhost:10035"),
            status=200,
        )
        msg = client.connect("http", "localhost", 10035, "", "hr", "test", "pass")
        assert "Connected" in msg

        body = json.loads(responses.calls[0].request.body)
        expr = body["expression"]
        assert "initialize-agraph-connection" in expr
        assert '"http"' in expr
        assert '"localhost"' in expr
        assert "10035" in expr

    @responses.activate
    def test_test_connection_success(self, client):
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result="T"),
            status=200,
        )
        assert client.test_connection() is True

    @responses.activate
    def test_test_connection_failure(self, client):
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result="NIL"),
            status=200,
        )
        assert client.test_connection() is False

    @responses.activate
    def test_set_repository(self, client):
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(stdout="Repository set"),
            status=200,
        )
        msg = client.set_repository("demos:northwind")
        assert "Repository set" in msg

        body = json.loads(responses.calls[0].request.body)
        assert "set-repository-from-string" in body["expression"]
        assert "demos:northwind" in body["expression"]

    @responses.activate
    def test_set_query_library(self, client):
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result="T"),
            status=200,
        )
        assert client.set_query_library("/:query-library") is True

    @responses.activate
    def test_set_api_key(self, client):
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result='"sk-ant-123"'),
            status=200,
        )
        client.set_api_key("sk-ant-123")
        body = json.loads(responses.calls[0].request.body)
        assert "setf *anthropic-api-key*" in body["expression"]

    @responses.activate
    def test_set_model(self, client):
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result='"claude-sonnet-4-5-20250929"'),
            status=200,
        )
        client.set_model("claude-sonnet-4-5-20250929")
        body = json.loads(responses.calls[0].request.body)
        assert "setf *claude-model*" in body["expression"]


# ---------------------------------------------------------------------------
# Query functions
# ---------------------------------------------------------------------------


class TestQueries:
    @responses.activate
    def test_claude_query(self, client):
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(
                result='"Here are the classes: Employee, Department"',
                stdout="=== Iteration 1 ===\n",
            ),
            status=200,
        )
        result = client.claude_query("Show me all classes")
        assert "Employee" in result.answer
        assert "Iteration 1" in result.stdout

        body = json.loads(responses.calls[0].request.body)
        assert "claude-query" in body["expression"]
        assert ":continue t" in body["expression"]

    @responses.activate
    def test_claude_query_fresh(self, client):
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result='"Answer"'),
            status=200,
        )
        client.claude_query("Question", continue_conversation=False)

        body = json.loads(responses.calls[0].request.body)
        assert ":continue nil" in body["expression"]

    @responses.activate
    def test_claude_ask(self, client):
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result='"4"'),
            status=200,
        )
        result = client.claude_ask("What is 2+2?")
        assert result.answer == "4"

        body = json.loads(responses.calls[0].request.body)
        assert "claude-ask" in body["expression"]

    @responses.activate
    def test_generate_sparql(self, client):
        sparql = "SELECT ?dept (COUNT(?emp) AS ?count) WHERE { ?emp a :Employee }"
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result=f'"{sparql}"'),
            status=200,
        )
        result = client.generate_sparql("Count employees per department")
        assert "SELECT" in result
        assert "COUNT" in result

        body = json.loads(responses.calls[0].request.body)
        assert "generate-sparql-with-claude" in body["expression"]

    @responses.activate
    def test_claude_query_nil_result(self, client):
        """claude_query returns empty string when Lisp returns NIL."""
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result="NIL"),
            status=200,
        )
        result = client.claude_query("Question")
        assert result.answer == ""

    @responses.activate
    def test_query_custom_iterations(self, client):
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result='"Answer"'),
            status=200,
        )
        client.claude_query("Q", max_iterations=20)
        body = json.loads(responses.calls[0].request.body)
        assert ":max-iterations 20" in body["expression"]


# ---------------------------------------------------------------------------
# Direct SPARQL
# ---------------------------------------------------------------------------


class TestSparql:
    @responses.activate
    def test_sparql_query(self, client):
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result='"[{s: ...}]"'),
            status=200,
        )
        result = client.sparql_query("SELECT ?s WHERE { ?s ?p ?o } LIMIT 10")

        body = json.loads(responses.calls[0].request.body)
        assert 'execute-tool "sparql_query"' in body["expression"]

    @responses.activate
    def test_sparql_with_quotes(self, client):
        """SPARQL containing quotes is properly escaped."""
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result='"results"'),
            status=200,
        )
        client.sparql_query('SELECT ?s WHERE { ?s rdfs:label "Hello" }')

        body = json.loads(responses.calls[0].request.body)
        # Verify the quotes in SPARQL are escaped in the Lisp expression
        assert '\\"Hello\\"' in body["expression"]


# ---------------------------------------------------------------------------
# Query Library
# ---------------------------------------------------------------------------


class TestQueryLibrary:
    @responses.activate
    def test_store_query(self, client):
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result='"Query stored"'),
            status=200,
        )
        result = client.store_query(
            title="My Query",
            description="Finds things",
            sparql_query="SELECT ?s WHERE { ?s ?p ?o }",
        )
        assert "stored" in result

        body = json.loads(responses.calls[0].request.body)
        assert 'execute-tool "store_query"' in body["expression"]
        assert '"title" "My Query"' in body["expression"]

    @responses.activate
    def test_search_queries(self, client):
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result='"Found 3 queries"'),
            status=200,
        )
        result = client.search_queries("employee count")
        assert "Found" in result

    @responses.activate
    def test_list_all_queries(self, client):
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result='"All queries listed"'),
            status=200,
        )
        result = client.list_all_queries(repository="hr-analytics")

        body = json.loads(responses.calls[0].request.body)
        assert '"repository" "hr-analytics"' in body["expression"]

    @responses.activate
    def test_delete_query_by_uri(self, client):
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result='"Deleted"'),
            status=200,
        )
        client.delete_query(query_uri="http://franz.com/ns/query-library#query-123")
        body = json.loads(responses.calls[0].request.body)
        assert '"queryUri"' in body["expression"]

    @responses.activate
    def test_delete_query_by_title(self, client):
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result='"Deleted"'),
            status=200,
        )
        client.delete_query(query_title="My Query", repository="hr")
        body = json.loads(responses.calls[0].request.body)
        assert '"queryTitle" "My Query"' in body["expression"]

    def test_delete_query_no_params(self, client):
        with pytest.raises(ValueError, match="Must provide"):
            client.delete_query()


# ---------------------------------------------------------------------------
# Context Management
# ---------------------------------------------------------------------------


class TestTokenCostAndContext:
    @responses.activate
    def test_get_token_cost_stats(self, client):
        plist = (
            "(:INPUT-TOKENS 5000"
            " :OUTPUT-TOKENS 1200"
            " :CACHE-WRITE-TOKENS 300"
            " :CACHE-READ-TOKENS 1500"
            " :TOTAL-TOKENS 8000"
            " :INPUT-COST 0.015"
            " :OUTPUT-COST 0.018"
            " :CACHE-WRITE-COST 0.001125"
            " :CACHE-READ-COST 0.00045"
            " :TOTAL-COST 0.034575"
            " :COST-WITHOUT-CACHE 0.0381"
            " :CACHE-SAVINGS 0.003525"
            " :CACHE-SAVINGS-PERCENT 9.25"
            " :CONTEXT-TOKENS 4500"
            " :CONTEXT-PERCENTAGE 2.25"
            " :CONTEXT-LIMIT 200000"
            " :MESSAGE-COUNT 8)"
        )
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result=plist),
            status=200,
        )
        stats = client.get_token_cost_stats()
        assert isinstance(stats, TokenCostStats)
        assert stats.input_tokens == 5000
        assert stats.output_tokens == 1200
        assert stats.cache_write_tokens == 300
        assert stats.cache_read_tokens == 1500
        assert stats.total_tokens == 8000
        assert stats.input_cost == pytest.approx(0.015)
        assert stats.total_cost == pytest.approx(0.034575)
        assert stats.cache_savings == pytest.approx(0.003525)
        assert stats.context_percentage == pytest.approx(2.25)
        assert stats.message_count == 8

    @responses.activate
    def test_get_token_cost_stats_empty(self, client):
        """When no API calls have been made, returns zeroed stats."""
        plist = (
            "(:INPUT-TOKENS 0"
            " :OUTPUT-TOKENS 0"
            " :CACHE-WRITE-TOKENS 0"
            " :CACHE-READ-TOKENS 0"
            " :TOTAL-TOKENS 0"
            " :INPUT-COST 0.0"
            " :OUTPUT-COST 0.0"
            " :CACHE-WRITE-COST 0.0"
            " :CACHE-READ-COST 0.0"
            " :TOTAL-COST 0.0"
            " :COST-WITHOUT-CACHE 0.0"
            " :CACHE-SAVINGS 0.0"
            " :CACHE-SAVINGS-PERCENT 0.0"
            " :CONTEXT-TOKENS 0"
            " :CONTEXT-PERCENTAGE 0.0"
            " :CONTEXT-LIMIT 200000"
            " :MESSAGE-COUNT 0)"
        )
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result=plist),
            status=200,
        )
        stats = client.get_token_cost_stats()
        assert stats.total_cost == 0.0
        assert stats.message_count == 0

    @responses.activate
    def test_reset_context_stats(self, client):
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result="T"),
            status=200,
        )
        client.reset_context_stats()
        body = json.loads(responses.calls[0].request.body)
        assert "reset-context-stats" in body["expression"]

    @responses.activate
    def test_clear_conversation(self, client):
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result="T"),
            status=200,
        )
        client.clear_conversation()
        body = json.loads(responses.calls[0].request.body)
        assert "clear-conversation-keep-config" in body["expression"]

    @responses.activate
    def test_condense_conversation(self, client):
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result="12"),
            status=200,
        )
        removed = client.condense_conversation(keep_recent=3)
        assert removed == 12

        body = json.loads(responses.calls[0].request.body)
        assert ":keep-recent 3" in body["expression"]


# ---------------------------------------------------------------------------
# Utilities
# ---------------------------------------------------------------------------


class TestUtilities:
    @responses.activate
    def test_execute_tool(self, client):
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result='"SHACL schema..."'),
            status=200,
        )
        result = client.execute_tool("get_shacl")

        body = json.loads(responses.calls[0].request.body)
        assert 'execute-tool "get_shacl"' in body["expression"]

    @responses.activate
    def test_execute_tool_with_params(self, client):
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result='"results"'),
            status=200,
        )
        client.execute_tool("sparql_query", query="SELECT 1")

        body = json.loads(responses.calls[0].request.body)
        assert '"query" "SELECT 1"' in body["expression"]

    @responses.activate
    def test_set_max_iterations(self, client):
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result="20"),
            status=200,
        )
        result = client.set_max_iterations(20)
        assert result == 20

    @responses.activate
    def test_set_variable(self, client):
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result="32000"),
            status=200,
        )
        client.set_variable("*max-tokens*", 32000)
        body = json.loads(responses.calls[0].request.body)
        assert "setf *max-tokens* 32000" in body["expression"]

    @responses.activate
    def test_set_variable_bool(self, client):
        # bool must be checked before int since bool is subclass of int
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result="T"),
            status=200,
        )
        client.set_variable("*enable-prompt-caching*", True)
        body = json.loads(responses.calls[0].request.body)
        assert "setf *enable-prompt-caching* t" in body["expression"]


# ---------------------------------------------------------------------------
# Context manager
# ---------------------------------------------------------------------------


class TestContextManager:
    @responses.activate
    def test_with_statement(self):
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result="3"),
            status=200,
        )
        with GraphTalkerClient(port=8080) as client:
            result = client.eval("(+ 1 2)")
            assert result.parsed == 3


# ---------------------------------------------------------------------------
# Session Management
# ---------------------------------------------------------------------------


class TestSessionManagement:
    # -- username property --

    def test_username_default_none(self):
        c = GraphTalkerClient(port=8080)
        assert c.username is None
        c.close()

    def test_username_set_in_constructor(self):
        c = GraphTalkerClient(port=8080, username="alice")
        assert c.username == "alice"
        c.close()

    def test_username_property_setter(self, client):
        assert client.username is None
        client.username = "bob"
        assert client.username == "bob"

    # -- save_session --

    @responses.activate
    def test_save_session_new(self, client):
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result='"session-123-456"'),
            status=200,
        )
        sid = client.save_session("My Session")
        assert sid == "session-123-456"

        body = json.loads(responses.calls[0].request.body)
        expr = body["expression"]
        assert "save-session-to-library" in expr
        assert "nil" in expr  # no session-id => nil
        assert '"My Session"' in expr
        # No :username when client.username is None
        assert ":username" not in expr

    @responses.activate
    def test_save_session_with_username(self):
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result='"session-789"'),
            status=200,
        )
        c = GraphTalkerClient(port=8080, api_key="test-key", username="alice")
        sid = c.save_session("Alice's Session")
        assert sid == "session-789"

        body = json.loads(responses.calls[0].request.body)
        expr = body["expression"]
        assert ':username "alice"' in expr
        c.close()

    @responses.activate
    def test_save_session_update_existing(self, client):
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result='"existing-id"'),
            status=200,
        )
        sid = client.save_session("Updated Title", session_id="existing-id")
        assert sid == "existing-id"

        body = json.loads(responses.calls[0].request.body)
        expr = body["expression"]
        assert '"existing-id"' in expr

    # -- list_sessions --

    @responses.activate
    def test_list_sessions_no_username(self, client):
        """Client with no username: list_sessions uses no username filter."""
        session_json = json.dumps(
            [
                {
                    "uri": "http://franz.com/ns/conversation-session#s1",
                    "title": "Session 1",
                    "repository": "hr-analytics",
                    "messageCount": 5,
                    "created": "2026-01-01T00:00:00",
                    "modified": "2026-01-02T00:00:00",
                    "username": None,
                }
            ]
        )
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result=f'"{_escape_json_in_lisp(session_json)}"'),
            status=200,
        )
        sessions = client.list_sessions()
        assert len(sessions) == 1
        assert sessions[0].session_id == "s1"
        assert sessions[0].title == "Session 1"

        body = json.loads(responses.calls[0].request.body)
        expr = body["expression"]
        assert "list-sessions-as-json" in expr
        assert ":username" not in expr

    @responses.activate
    def test_list_sessions_with_client_username(self):
        """Client with username: list_sessions defaults to filtering by it."""
        session_json = json.dumps(
            [
                {
                    "uri": "http://franz.com/ns/conversation-session#s2",
                    "title": "Alice Session",
                    "repository": "hr",
                    "messageCount": 3,
                    "created": None,
                    "modified": None,
                    "username": "alice",
                }
            ]
        )
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result=f'"{_escape_json_in_lisp(session_json)}"'),
            status=200,
        )
        c = GraphTalkerClient(port=8080, api_key="test-key", username="alice")
        sessions = c.list_sessions()
        assert len(sessions) == 1
        assert sessions[0].username == "alice"

        body = json.loads(responses.calls[0].request.body)
        expr = body["expression"]
        assert ':username "alice"' in expr
        c.close()

    @responses.activate
    def test_list_sessions_explicit_none_overrides_client(self):
        """Passing username=None explicitly bypasses client username filter."""
        session_json = json.dumps([])
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result=f'"{_escape_json_in_lisp(session_json)}"'),
            status=200,
        )
        c = GraphTalkerClient(port=8080, api_key="test-key", username="alice")
        sessions = c.list_sessions(username=None)
        assert sessions == []

        body = json.loads(responses.calls[0].request.body)
        expr = body["expression"]
        # Should NOT have :username since we explicitly passed None
        assert ":username" not in expr
        c.close()

    @responses.activate
    def test_list_sessions_explicit_username(self, client):
        """Explicit username parameter overrides client default."""
        session_json = json.dumps([])
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result=f'"{_escape_json_in_lisp(session_json)}"'),
            status=200,
        )
        client.list_sessions(username="bob")

        body = json.loads(responses.calls[0].request.body)
        expr = body["expression"]
        assert ':username "bob"' in expr

    @responses.activate
    def test_list_sessions_with_repository(self, client):
        session_json = json.dumps([])
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result=f'"{_escape_json_in_lisp(session_json)}"'),
            status=200,
        )
        client.list_sessions(repository="hr-analytics")

        body = json.loads(responses.calls[0].request.body)
        expr = body["expression"]
        assert '"hr-analytics"' in expr

    @responses.activate
    def test_list_sessions_empty_list(self, client):
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result='"[]"'),
            status=200,
        )
        sessions = client.list_sessions()
        assert sessions == []

    @responses.activate
    def test_list_sessions_all_fields_parsed(self, client):
        """Verify all SessionInfo fields are populated correctly."""
        session_json = json.dumps(
            [
                {
                    "uri": "http://franz.com/ns/conversation-session#abc-123",
                    "title": "Full Session",
                    "repository": "demo-repo",
                    "messageCount": 42,
                    "created": "2026-02-01T10:00:00",
                    "modified": "2026-02-20T15:30:00",
                    "username": "carol",
                }
            ]
        )
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result=f'"{_escape_json_in_lisp(session_json)}"'),
            status=200,
        )
        from franz.graphtalker import SessionInfo

        sessions = client.list_sessions()
        assert len(sessions) == 1
        s = sessions[0]
        assert isinstance(s, SessionInfo)
        assert s.session_id == "abc-123"
        assert s.title == "Full Session"
        assert s.repository == "demo-repo"
        assert s.message_count == 42
        assert s.created == "2026-02-01T10:00:00"
        assert s.modified == "2026-02-20T15:30:00"
        assert s.username == "carol"

    @responses.activate
    def test_list_sessions_null_username_becomes_none(self, client):
        """JSON null username is mapped to Python None."""
        session_json = json.dumps(
            [
                {
                    "uri": "http://franz.com/ns/conversation-session#x",
                    "title": "No Owner",
                    "repository": "repo",
                    "messageCount": 1,
                    "created": None,
                    "modified": None,
                    "username": None,
                }
            ]
        )
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result=f'"{_escape_json_in_lisp(session_json)}"'),
            status=200,
        )
        sessions = client.list_sessions()
        assert sessions[0].username is None

    # -- restore_session --

    @responses.activate
    def test_restore_session_success(self, client):
        plist = (
            '(:SESSION-ID "my-session"'
            ' :TITLE "Restored"'
            ' :REPOSITORY "hr"'
            " :MESSAGE-COUNT 10"
            ' :USERNAME "alice"'
            ' :CREATED "2026-01-01T00:00:00"'
            ' :MODIFIED "2026-01-02T00:00:00")'
        )
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result=plist),
            status=200,
        )
        from franz.graphtalker import SessionInfo

        info = client.restore_session("my-session")
        assert isinstance(info, SessionInfo)
        assert info.session_id == "my-session"
        assert info.title == "Restored"
        assert info.username == "alice"
        assert info.message_count == 10

        body = json.loads(responses.calls[0].request.body)
        expr = body["expression"]
        assert "restore-session-from-library" in expr
        assert "conversation-session#my-session" in expr

    @responses.activate
    def test_restore_session_not_found(self, client):
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(
                error="Session not found: http://franz.com/ns/conversation-session#gone"
            ),
            status=200,
        )
        with pytest.raises(EvalError, match="Session not found"):
            client.restore_session("gone")

    # -- delete_session --

    @responses.activate
    def test_delete_session(self, client):
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result='""'),
            status=200,
        )
        client.delete_session("my-session")

        body = json.loads(responses.calls[0].request.body)
        expr = body["expression"]
        assert "delete-session-from-library" in expr
        assert "conversation-session#my-session" in expr


# ---------------------------------------------------------------------------
# Abort Query
# ---------------------------------------------------------------------------


class TestAbortQuery:
    @responses.activate
    def test_abort_running_query(self, client):
        responses.add(
            responses.POST,
            ABORT_URL,
            json={"success": True, "message": "Query aborted"},
            status=200,
        )
        result = client.abort_query()
        assert result is True

    @responses.activate
    def test_abort_no_query_running(self, client):
        responses.add(
            responses.POST,
            ABORT_URL,
            json={"success": True, "message": "No query running"},
            status=200,
        )
        result = client.abort_query()
        assert result is False

    @responses.activate
    def test_abort_auth_failure(self, client):
        responses.add(
            responses.POST,
            ABORT_URL,
            json={"error": "Unauthorized"},
            status=401,
        )
        with pytest.raises(AuthenticationError):
            client.abort_query()

    @responses.activate
    def test_abort_error_raises_query_aborted(self, client):
        """When eval returns an abort error, QueryAbortedError is raised."""
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(error="Query aborted by user"),
            status=200,
        )
        with pytest.raises(QueryAbortedError):
            client.eval("(sleep 30)")

    @responses.activate
    def test_query_aborted_is_eval_error(self, client):
        """QueryAbortedError is a subclass of EvalError for backward compat."""
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(error="Query aborted by user"),
            status=200,
        )
        with pytest.raises(EvalError):
            client.eval("(sleep 30)")


# ---------------------------------------------------------------------------
# Visualizations
# ---------------------------------------------------------------------------


class TestGetPendingVisualization:
    @responses.activate
    def test_viz_config_with_metadata(self, client):
        """viz-config-* entries contain JSON with metadata."""
        stored = json.dumps(
            {
                "visualizationType": "pie_chart",
                "visualizationConfig": json.dumps(
                    {
                        "type": "pie",
                        "data": {
                            "labels": ["A", "B"],
                            "datasets": [{"data": [10, 20]}],
                        },
                    }
                ),
                "description": "A pie chart",
                "summary": "Shows A vs B",
            }
        )
        # Lisp returns the stored value as a quoted string
        lisp_result = '"' + _escape_json_in_lisp(stored) + '"'
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result=lisp_result),
            status=200,
        )
        viz = client.get_pending_visualization("viz-config-123-456")
        assert viz.ref_id == "viz-config-123-456"
        assert viz.viz_type == "pie_chart"
        assert viz.config["type"] == "pie"
        assert viz.description == "A pie chart"
        assert viz.summary == "Shows A vs B"

    @responses.activate
    def test_map_config_raw_geojson(self, client):
        """map-config-* entries are raw GeoJSON config strings."""
        config = json.dumps(
            {
                "type": "map",
                "data": {"type": "FeatureCollection", "features": []},
            }
        )
        lisp_result = '"' + _escape_json_in_lisp(config) + '"'
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result=lisp_result),
            status=200,
        )
        viz = client.get_pending_visualization("map-config-123-456")
        assert viz.ref_id == "map-config-123-456"
        assert viz.viz_type == "map"
        assert viz.config["type"] == "map"

    @responses.activate
    def test_not_found_raises(self, client):
        """Missing ref ID raises EvalError."""
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result="NIL"),
            status=200,
        )
        from franz.graphtalker import EvalError

        with pytest.raises(EvalError, match="not found"):
            client.get_pending_visualization("viz-config-nonexistent")


class TestGetVisualizations:
    @responses.activate
    def test_returns_visualization_list(self, client):
        """get_visualizations parses JSON array from tool result."""
        viz_json = json.dumps(
            [
                {
                    "vizId": "http://franz.com/ns/visualization#viz-1",
                    "type": "bar_chart",
                    "config": '{"type":"bar"}',
                    "description": "Bar chart of sales",
                    "summary": "Shows sales by region",
                    "created": "2026-03-14T12:00:00",
                },
                {
                    "vizId": "http://franz.com/ns/visualization#viz-2",
                    "type": "pie_chart",
                    "config": '{"type":"pie"}',
                    "description": "Pie chart of categories",
                    "summary": None,
                    "created": None,
                },
            ]
        )
        result_str = 'Found 2 visualization(s) for query "Sales":\n' + viz_json
        lisp_result = '"' + _escape_json_in_lisp(result_str) + '"'
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result=lisp_result),
            status=200,
        )
        vizs = client.get_visualizations("Sales")
        assert len(vizs) == 2
        assert vizs[0].viz_id == "http://franz.com/ns/visualization#viz-1"
        assert vizs[0].viz_type == "bar_chart"
        assert vizs[0].description == "Bar chart of sales"
        assert vizs[1].viz_type == "pie_chart"

    @responses.activate
    def test_no_visualizations(self, client):
        """Returns empty list when no visualizations found."""
        result_str = 'No visualizations found for query "Missing"'
        lisp_result = '"' + _escape_json_in_lisp(result_str) + '"'
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result=lisp_result),
            status=200,
        )
        vizs = client.get_visualizations("Missing")
        assert vizs == []

    @responses.activate
    def test_sends_repository_filter(self, client):
        """Repository parameter is passed to the tool."""
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result='"No visualizations found"'),
            status=200,
        )
        client.get_visualizations("Sales", repository="my-repo")
        body = json.loads(responses.calls[0].request.body)
        assert "my-repo" in body["expression"]


class TestStoreVisualization:
    @responses.activate
    def test_store_with_ref_id(self, client):
        """Stores visualization using reference ID (minimal args)."""
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result='"Visualization stored successfully!"'),
            status=200,
        )
        result = client.store_visualization(
            "Sales Query", "viz-config-123-456", "my-repo"
        )
        assert "stored" in result.lower()
        body = json.loads(responses.calls[0].request.body)
        expr = body["expression"]
        assert "viz-config-123-456" in expr
        assert "Sales Query" in expr

    @responses.activate
    def test_store_with_explicit_metadata(self, client):
        """Explicit metadata overrides are passed through."""
        responses.add(
            responses.POST,
            EVAL_URL,
            json=eval_response(result='"Visualization stored successfully!"'),
            status=200,
        )
        client.store_visualization(
            "Sales Query",
            '{"type":"bar"}',
            "my-repo",
            viz_type="bar_chart",
            description="A bar chart",
            summary="Summary here",
        )
        body = json.loads(responses.calls[0].request.body)
        expr = body["expression"]
        assert "bar_chart" in expr
        assert "A bar chart" in expr
        assert "Summary here" in expr


def _escape_json_in_lisp(json_str: str) -> str:
    """Escape a JSON string for embedding in a Lisp quoted string result.

    When the eval server returns a JSON string as a Lisp ~S result,
    inner quotes are escaped with backslashes.
    """
    return json_str.replace("\\", "\\\\").replace('"', '\\"')
