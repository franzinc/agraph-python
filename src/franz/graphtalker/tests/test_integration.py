"""Integration tests against a live GraphTalker eval server.

These tests require a running eval server and AllegroGraph instance.
They are skipped by default unless --run-integration is passed to pytest.

Usage:
    # Start eval server first, then:
    pytest tests/test_integration.py --run-integration

    # With custom settings:
    pytest tests/test_integration.py --run-integration \
        --eval-port=8080 \
        --eval-key="" \
        --ag-host=localhost \
        --ag-port=10035 \
        --ag-catalog="" \
        --ag-repo=hr-analytics \
        --ag-user=test \
        --ag-password=xyzzy
"""

import json
import re
import threading
import time

import pytest

from franz.graphtalker import (
    EvalError,
    GraphTalkerClient,
    PendingVisualization,
    QueryAbortedError,
    SessionInfo,
    TokenCostStats,
    Visualization,
)

# All tests in this file use the integration_client fixture from conftest.py
# and are marked with @pytest.mark.integration so they're skipped without --run-integration.


# ---------------------------------------------------------------------------
# Health & Connection
# ---------------------------------------------------------------------------


@pytest.mark.integration
class TestHealthAndConnection:
    def test_health_check(self, integration_client):
        assert integration_client.health_check() is True

    def test_connection(self, integration_client):
        assert integration_client.test_connection() is True

    def test_eval_arithmetic(self, integration_client):
        result = integration_client.eval("(+ 1 2)")
        assert result.parsed == 3

    def test_eval_string(self, integration_client):
        result = integration_client.eval('(format nil "hello ~A" "world")')
        assert result.parsed == "hello world"


# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------


@pytest.mark.integration
class TestConfiguration:
    def test_get_model(self, integration_client):
        model = integration_client.get_variable("*claude-model*")
        assert isinstance(model, str)
        assert "claude" in model.lower()

    def test_get_max_tokens(self, integration_client):
        tokens = integration_client.get_variable("*max-tokens*")
        assert isinstance(tokens, int)
        assert tokens > 0

    def test_set_and_restore_variable(self, integration_client):
        original = integration_client.get_variable("*max-tool-result-length*")
        try:
            integration_client.set_variable("*max-tool-result-length*", 9999)
            assert integration_client.get_variable("*max-tool-result-length*") == 9999
        finally:
            if original is not None:
                integration_client.set_variable("*max-tool-result-length*", original)
            else:
                integration_client.eval("(setf *max-tool-result-length* nil)")

    def test_show_available_tools(self, integration_client):
        tools = integration_client.show_available_tools()
        assert "sparql_query" in tools
        assert "get_shacl" in tools


# ---------------------------------------------------------------------------
# Direct SPARQL
# ---------------------------------------------------------------------------


@pytest.mark.integration
class TestDirectSparql:
    def test_count_triples(self, integration_client):
        result = integration_client.sparql_query(
            "SELECT (COUNT(*) AS ?count) WHERE { ?s ?p ?o }"
        )
        assert "count" in result
        assert "bindings" in result

    def test_select_classes(self, integration_client):
        result = integration_client.sparql_query(
            "SELECT DISTINCT ?class WHERE { ?s a ?class } ORDER BY ?class"
        )
        assert "bindings" in result

    def test_ask_query(self, integration_client):
        result = integration_client.sparql_query("ASK WHERE { ?s ?p ?o }")
        assert "true" in result.lower() or "boolean" in result.lower()

    def test_invalid_sparql_returns_error_string(self, integration_client):
        """Invalid SPARQL returns an error message string (not an exception).

        The execute-tool function catches AllegroGraph errors and returns
        them as result strings so Claude can iterate on the query.
        """
        result = integration_client.sparql_query("NOT VALID SPARQL AT ALL")
        assert "error" in result.lower() or "malformed" in result.lower()

    def test_sparql_with_quotes(self, integration_client):
        """SPARQL containing double quotes is properly escaped."""
        result = integration_client.sparql_query(
            'SELECT ?s WHERE { ?s ?p "nonexistent-value-12345" } LIMIT 1'
        )
        assert "bindings" in result


# ---------------------------------------------------------------------------
# Context Management
# ---------------------------------------------------------------------------


@pytest.mark.integration
class TestTokenCostAndContext:
    def test_get_token_cost_stats(self, integration_client):
        stats = integration_client.get_token_cost_stats()
        assert isinstance(stats, TokenCostStats)
        assert stats.context_limit == 200000
        assert isinstance(stats.context_percentage, (int, float))
        assert isinstance(stats.message_count, int)
        assert isinstance(stats.total_cost, float)

    def test_clear_conversation(self, integration_client):
        integration_client.clear_conversation()
        stats = integration_client.get_token_cost_stats()
        assert stats.message_count == 0

    def test_start_conversation(self, integration_client):
        integration_client.start_conversation()
        stats = integration_client.get_token_cost_stats()
        assert stats.message_count == 0


# ---------------------------------------------------------------------------
# Execute Tool
# ---------------------------------------------------------------------------


@pytest.mark.integration
class TestExecuteTool:
    def test_get_shacl(self, integration_client):
        schema = integration_client.execute_tool("get_shacl")
        assert isinstance(schema, str)
        assert len(schema) > 0

    def test_get_current_repository(self, integration_client):
        result = integration_client.execute_tool("get_current_repository")
        assert isinstance(result, str)

    def test_list_repositories(self, integration_client):
        result = integration_client.execute_tool("list_repositories")
        assert isinstance(result, str)


# ---------------------------------------------------------------------------
# Claude Query (costs real API tokens)
# ---------------------------------------------------------------------------


@pytest.mark.integration
class TestClaudeQuery:
    def test_claude_ask_simple(self, integration_client):
        """Simple question that doesn't require SPARQL."""
        result = integration_client.claude_ask(
            "What is 2+2? Answer with just the number."
        )
        assert result.answer is not None
        assert len(result.answer) > 0
        assert "4" in result.answer

    def test_claude_query_about_data(self, integration_client):
        """Ask about repository data -- Claude should use tools."""
        integration_client.clear_conversation()
        result = integration_client.claude_query(
            "What classes exist in this repository? Be brief."
        )
        assert result.answer is not None
        assert len(result.answer) > 0
        # hr-analytics should have Employee class
        answer_lower = result.answer.lower()
        assert "employee" in answer_lower

    def test_claude_query_follow_up(self, integration_client):
        """Follow-up question should reuse conversation context."""
        result = integration_client.claude_query(
            "How many instances of the first class you mentioned? Just the number."
        )
        assert result.answer is not None
        assert len(result.answer) > 0

    def test_claude_query_context_grows(self, integration_client):
        """After Claude queries, context should have messages."""
        stats = integration_client.get_token_cost_stats()
        assert stats.message_count > 0

    def test_generate_sparql(self, integration_client):
        """Generate SPARQL and verify it's valid by executing it."""
        integration_client.clear_conversation()
        sparql = integration_client.generate_sparql(
            "Count the total number of triples in the repository"
        )
        assert sparql is not None
        assert len(sparql) > 0
        assert "SELECT" in sparql.upper()

        # Execute the generated SPARQL to verify it's valid
        result = integration_client.sparql_query(sparql)
        assert "bindings" in result


# ---------------------------------------------------------------------------
# Query Library
# ---------------------------------------------------------------------------


@pytest.mark.integration
class TestQueryLibrary:
    QUERY_TITLE = "__pytest_graphtalker_temp_query__"
    QUERY_SPARQL = "SELECT (COUNT(*) AS ?count) WHERE { ?s ?p ?o }"

    REPOSITORY = "hr-analytics"

    @staticmethod
    def _cleanup_test_queries(client, title, repository):
        """Remove any leftover test queries from previous runs."""
        for _ in range(20):
            found = client.search_queries(title)
            if title not in found:
                break
            try:
                client.delete_query(query_title=title, repository=repository)
            except Exception:
                break

    def test_store_and_search_and_delete(self, integration_client):
        """Full lifecycle: store, search, find, delete."""
        # Clean up any leftovers from previous failed runs
        self._cleanup_test_queries(
            integration_client, self.QUERY_TITLE, self.REPOSITORY
        )

        # Store
        result = integration_client.store_query(
            title=self.QUERY_TITLE,
            description="Pytest temp query counts all triples",
            sparql_query=self.QUERY_SPARQL,
            repository=self.REPOSITORY,
        )
        assert isinstance(result, str)
        assert "stored" in result.lower() or "query" in result.lower()

        # Search (use a term that is a substring of the title)
        found = integration_client.search_queries(self.QUERY_TITLE)
        assert self.QUERY_TITLE in found

        # List all
        all_queries = integration_client.list_all_queries(repository=self.REPOSITORY)
        assert self.QUERY_TITLE in all_queries

        # Delete by title + repository (both required)
        integration_client.delete_query(
            query_title=self.QUERY_TITLE, repository=self.REPOSITORY
        )

        # Verify deleted
        after = integration_client.search_queries(self.QUERY_TITLE)
        assert self.QUERY_TITLE not in after


# ---------------------------------------------------------------------------
# Session Management
# ---------------------------------------------------------------------------


@pytest.mark.integration
class TestSessionManagement:
    SESSION_TITLE = "__pytest_graphtalker_temp_session__"
    USERNAME = "__pytest_user__"

    def _cleanup_test_sessions(self, client):
        """Remove any leftover test sessions from previous runs."""
        try:
            sessions = client.list_sessions(username=None)
            for s in sessions:
                if s.title == self.SESSION_TITLE:
                    client.delete_session(s.session_id)
        except Exception:
            pass

    def test_session_lifecycle(self, integration_client):
        """Full lifecycle: save → list → restore → delete."""
        self._cleanup_test_sessions(integration_client)

        # Set up a username for this test
        original_username = integration_client.username
        integration_client.username = self.USERNAME

        try:
            # Ensure there's something in conversation history
            integration_client.clear_conversation()
            integration_client.eval('(format t "test output")')

            # Save
            sid = integration_client.save_session(self.SESSION_TITLE)
            assert isinstance(sid, str)
            assert len(sid) > 0

            # List (filtered by username)
            sessions = integration_client.list_sessions()
            titles = [s.title for s in sessions]
            assert self.SESSION_TITLE in titles
            saved = [s for s in sessions if s.title == self.SESSION_TITLE][0]
            assert isinstance(saved, SessionInfo)
            assert saved.username == self.USERNAME
            assert saved.session_id == sid

            # List all (no username filter)
            all_sessions = integration_client.list_sessions(username=None)
            all_titles = [s.title for s in all_sessions]
            assert self.SESSION_TITLE in all_titles

            # Clear conversation and verify it's empty
            integration_client.clear_conversation()
            stats = integration_client.get_token_cost_stats()
            assert stats.message_count == 0

            # Restore
            info = integration_client.restore_session(sid)
            assert isinstance(info, SessionInfo)
            assert info.session_id == sid
            assert info.title == self.SESSION_TITLE
            assert info.username == self.USERNAME

            # Delete
            integration_client.delete_session(sid)

            # Verify deleted
            after = integration_client.list_sessions(username=None)
            remaining_titles = [s.title for s in after]
            assert self.SESSION_TITLE not in remaining_titles

        finally:
            integration_client.username = original_username
            self._cleanup_test_sessions(integration_client)


# ---------------------------------------------------------------------------
# Abort Query
# ---------------------------------------------------------------------------


@pytest.mark.integration
class TestAbortQuery:
    def test_abort_no_query_running(self, integration_client):
        """Aborting when nothing is running returns False (no-op)."""
        result = integration_client.abort_query()
        assert result is False

    def test_abort_long_running_eval(self, integration_client):
        """Abort a (sleep 30) and verify it stops quickly."""
        error_holder = {}

        def long_eval():
            try:
                integration_client.eval("(sleep 30)", timeout=60)
            except QueryAbortedError as e:
                error_holder["aborted"] = e
            except Exception as e:
                error_holder["error"] = e

        thread = threading.Thread(target=long_eval)
        start_time = time.time()
        thread.start()

        # Give the eval request time to reach the server and start sleeping
        time.sleep(2)

        # Abort from the main thread
        abort_result = integration_client.abort_query()
        assert abort_result is True

        # Wait for the eval thread to finish (should be fast now)
        thread.join(timeout=10)
        elapsed = time.time() - start_time

        # Should have finished well before the 30-second sleep
        assert not thread.is_alive(), "Eval thread should have finished"
        assert elapsed < 15, f"Abort took too long: {elapsed:.1f}s"

        # Should have gotten an abort error, not a successful sleep
        assert (
            "aborted" in error_holder
        ), f"Expected QueryAbortedError, got: {error_holder}"

    def test_server_healthy_after_abort(self, integration_client):
        """Server should still work after an abort."""
        assert integration_client.health_check() is True

        # Simple eval should work fine
        result = integration_client.eval("(+ 1 2)")
        assert result.parsed == 3


# ---------------------------------------------------------------------------
# Visualizations
# ---------------------------------------------------------------------------


@pytest.mark.integration
class TestVisualizationEndToEnd:
    """End-to-end visualization tests using Claude to generate charts.

    Tests both paths:
    A) Claude generates viz → retrieve cached → store → retrieve stored
    B) Claude generates viz → retrieve cached (no store)
    """

    REPOSITORY = "northwind-traders"
    QUERY_TITLE = "__pytest_viz_e2e_query__"

    def _cleanup(self, client):
        """Remove test query and its visualizations."""
        for _ in range(5):
            found = client.search_queries(self.QUERY_TITLE)
            if self.QUERY_TITLE not in found:
                break
            try:
                client.delete_query(
                    query_title=self.QUERY_TITLE,
                    repository=self.REPOSITORY,
                )
            except Exception:
                break

    @staticmethod
    def _extract_viz_ref_ids(text):
        """Extract all viz-config-* reference IDs from text."""
        return re.findall(r"viz-config-\d+-\d+", text)

    def test_path_b_retrieve_without_storing(self, integration_client):
        """Claude generates a chart → retrieve cached config (no store).

        This is the primary use case for the Python API: get the
        visualization config that Claude just created without having
        to store it in the query library first.
        """
        # Switch to northwind-traders
        integration_client.set_repository(self.REPOSITORY)
        integration_client.start_conversation()

        try:
            # Ask Claude to create a visualization
            result = integration_client.claude_query(
                "Show me the number of products per category as a bar chart. "
                "Do NOT store anything, just create the visualization."
            )

            # Claude should have called prepare_visualization —
            # look for reference IDs in both answer and stdout
            all_text = result.answer + "\n" + result.stdout
            ref_ids = self._extract_viz_ref_ids(all_text)
            assert len(ref_ids) > 0, (
                f"Expected viz-config-* reference ID in Claude's response.\n"
                f"Answer: {result.answer[:500]}\n"
                f"Stdout (last 1000): {result.stdout[-1000:]}"
            )

            # Retrieve the cached config
            ref_id = ref_ids[0]
            viz = integration_client.get_pending_visualization(ref_id)

            assert isinstance(viz, PendingVisualization)
            assert viz.viz_type is not None
            assert "chart" in viz.viz_type or "bar" in viz.viz_type
            assert isinstance(viz.config, dict)
            assert viz.description is not None
            assert len(viz.description) > 0

            # The config should be valid Chart.js
            assert "data" in viz.config
            assert "type" in viz.config or "datasets" in viz.config.get("data", {})

        finally:
            integration_client.set_repository("hr-analytics")

    def test_path_a_store_and_retrieve(self, integration_client):
        """Claude generates a chart → store with ref ID → retrieve from library.

        Full lifecycle: ask Claude for a chart, cache it, store query +
        visualization, then retrieve the stored visualization via Python API.
        """
        # Switch to northwind-traders
        integration_client.set_repository(self.REPOSITORY)
        integration_client.start_conversation()
        self._cleanup(integration_client)

        try:
            # Ask Claude to create a visualization
            result = integration_client.claude_query(
                "Show me the number of products per category as a pie chart. "
                "Do NOT store anything yet."
            )

            all_text = result.answer + "\n" + result.stdout
            ref_ids = self._extract_viz_ref_ids(all_text)
            assert len(ref_ids) > 0, (
                f"Expected viz-config-* reference ID in Claude's response.\n"
                f"Answer: {result.answer[:500]}"
            )
            ref_id = ref_ids[0]

            # Verify we can fetch it before storing
            viz = integration_client.get_pending_visualization(ref_id)
            assert isinstance(viz.config, dict)

            # Store the query first
            integration_client.store_query(
                title=self.QUERY_TITLE,
                description="Product count by category",
                sparql_query="SELECT ?cat (COUNT(?p) AS ?c) WHERE { ?p a <http://northwind.com/model/Product> ; <http://northwind.com/model/category> ?catNode . ?catNode <http://northwind.com/model/categoryName> ?cat } GROUP BY ?cat",
                repository=self.REPOSITORY,
            )

            # Store the visualization using the reference ID
            store_result = integration_client.store_visualization(
                self.QUERY_TITLE, ref_id, self.REPOSITORY
            )
            assert "stored" in store_result.lower()

            # Retrieve stored visualizations from query library
            vizs = integration_client.get_visualizations(
                self.QUERY_TITLE, repository=self.REPOSITORY
            )
            assert len(vizs) >= 1
            assert isinstance(vizs[0], Visualization)
            assert vizs[0].viz_type is not None

            # Verify the stored config is valid JSON
            config = json.loads(vizs[0].config)
            assert "data" in config

        finally:
            self._cleanup(integration_client)
            integration_client.set_repository("hr-analytics")

    def test_nonexistent_ref_id_raises(self, integration_client):
        """Fetching a bogus reference ID raises EvalError."""
        with pytest.raises(EvalError, match="not found"):
            integration_client.get_pending_visualization("viz-config-0-0")

    def test_get_visualizations_empty(self, integration_client):
        """Querying visualizations for a nonexistent query returns empty."""
        vizs = integration_client.get_visualizations("__nonexistent_query_12345__")
        assert vizs == []
