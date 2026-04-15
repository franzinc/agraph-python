"""GraphTalker Python client for the eval server.

Provides a high-level Pythonic API for interacting with AllegroGraph
via Claude AI, hiding the underlying Lisp expressions completely.
"""

import json
from typing import Any, List, Optional

import requests

from franz.graphtalker import _lisp
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

# Sentinel object used to distinguish "use client's default username"
# from "explicitly pass no username filter".
_USE_CLIENT_USERNAME = object()


class GraphTalkerClient:
    """Python client for GraphTalker eval server.

    Provides a high-level Pythonic API for interacting with AllegroGraph
    via Claude AI, hiding the underlying Lisp expressions completely.

    For advanced users, the eval() method provides direct access to
    evaluate arbitrary Lisp expressions.

    Args:
        host: Eval server hostname. Ignored when ``base_url`` is provided.
        port: Eval server port. Ignored when ``base_url`` is provided.
        api_key: API key for authentication (None for no auth).
        timeout: Default request timeout in seconds.
        default_query_timeout: Default timeout for Claude query methods
            (claude_query, claude_ask, generate_sparql). Defaults to
            300 seconds (5 minutes) since Claude queries involve multiple
            tool-calling iterations.
        username: Optional username tag for session management.
            When set, save_session() and list_sessions() use this
            as the default owner/filter. Not a security boundary.
        base_url: Full base URL for the GraphTalker eval server. When
            provided it is used directly, overriding the ``http://host:port``
            default.  Use this when connecting through an AllegroGraph proxy,
            e.g. ``"http://ag-host:10035/graphtalker/8080"``.
        auth: ``(username, password)`` tuple for HTTP Basic Auth.  Used when
            connecting through an AllegroGraph proxy, which requires the same
            credentials as the AG server itself.

    Example — direct connection::

        client = GraphTalkerClient(port=8080, api_key="my-key")
        client.connect("http", "localhost", 10035, "", "hr-analytics", "test", "xyzzy")
        result = client.claude_query("Show me all the classes")
        print(result.answer)

    Example — AllegroGraph proxy connection::

        client = GraphTalkerClient(
            base_url="http://localhost:10035/graphtalker/8080",
            api_key="my-key",
        )
        client.connect("http", "localhost", 10035, "", "hr-analytics", "test", "xyzzy")
        result = client.claude_query("Show me all the classes")
        print(result.answer)
    """

    def __init__(
        self,
        host: str = "localhost",
        port: int = 8080,
        api_key: Optional[str] = None,
        timeout: float = 30.0,
        default_query_timeout: float = 300.0,
        username: Optional[str] = None,
        base_url: Optional[str] = None,
        auth: Optional[tuple] = None,
    ):
        self._host = host
        self._port = port
        self._api_key = api_key
        self._timeout = timeout
        self._default_query_timeout = default_query_timeout
        self._username = username
        if base_url is not None:
            self._base_url = base_url.rstrip("/")
        else:
            self._base_url = f"http://{host}:{port}"
        self._session = requests.Session()
        self._session.headers["Content-Type"] = "application/json"
        if api_key:
            self._session.headers["Authorization"] = f"Bearer {api_key}"
        if auth is not None:
            self._session.auth = auth

    def close(self):
        """Stop the GraphTalker server and close the underlying HTTP session."""
        try:
            self.stop()
        except Exception:
            pass
        self._session.close()

    def __enter__(self):
        return self

    def __exit__(self, *args):
        self.close()

    # ------------------------------------------------------------------
    # Low-level
    # ------------------------------------------------------------------

    def eval(self, expression: str, *, timeout: Optional[float] = None) -> EvalResult:
        """Evaluate a Lisp expression on the eval server.

        This is the low-level escape hatch. The expression is sent as-is
        and evaluated in the :agraph-claude-tools package.

        Args:
            expression: A Lisp expression as a string.
            timeout: Request timeout in seconds (overrides default).

        Returns:
            EvalResult with stdout, result, error, and parsed fields.

        Raises:
            ConnectionError: Cannot reach the eval server.
            AuthenticationError: Invalid or missing API key.
            EvalError: Lisp evaluation raised an error.
            TimeoutError: Request timed out.
            ServerError: Unexpected HTTP error.
        """
        effective_timeout = timeout if timeout is not None else self._timeout

        try:
            response = self._session.post(
                f"{self._base_url}/eval",
                json={"expression": expression},
                timeout=effective_timeout,
            )
        except requests.exceptions.ConnectionError as e:
            raise ConnectionError(f"Cannot connect to {self._base_url}: {e}") from e
        except requests.exceptions.Timeout as e:
            raise TimeoutError(f"Request timed out after {effective_timeout}s") from e
        except requests.exceptions.RequestException as e:
            raise GraphTalkerError(f"Request failed: {e}") from e

        if response.status_code == 401:
            raise AuthenticationError("Invalid or missing API key")
        if response.status_code == 400:
            data = response.json()
            raise GraphTalkerError(data.get("message", "Bad request"))
        if response.status_code != 200:
            raise ServerError(response.status_code, response.text)

        data = response.json()
        # The eval server returns "error": [] for no-error and
        # "error": "message" for actual errors. Normalize falsy values to None.
        raw_error = data.get("error")
        error = raw_error if raw_error else None
        result = EvalResult(
            stdout=data.get("stdout", ""),
            result=data.get("result", "NIL"),
            error=error,
        )

        if result.error is not None:
            if "aborted" in result.error.lower():
                raise QueryAbortedError(result.error, stdout=result.stdout)
            raise EvalError(result.error, stdout=result.stdout)

        return result

    def health_check(self) -> bool:
        """Check if the eval server is running and healthy.

        Returns:
            True if server is healthy, False otherwise.

        Raises:
            ConnectionError: Cannot reach the server.
        """
        try:
            response = self._session.get(
                f"{self._base_url}/health",
                timeout=5.0,
            )
            return response.status_code == 200
        except requests.exceptions.ConnectionError as e:
            raise ConnectionError(f"Cannot connect to {self._base_url}: {e}") from e

    def stop(self) -> None:
        """Shut down the GraphTalker eval server process.

        Calls ``(stop-eval-server)`` on the server, which unpublishes all
        HTTP endpoints and terminates the process.  After this call the
        client is no longer usable.

        Raises:
            ConnectionError: Cannot reach the eval server.
            AuthenticationError: Invalid or missing API key.
        """
        self.eval("(stop-eval-server)")

    def abort_query(self) -> bool:
        """Abort the currently running query on the eval server.

        Call this from a different thread while claude_query() or
        generate_sparql() is blocking in another thread. The blocked
        call will raise QueryAbortedError.

        Returns:
            True if a query was aborted, False if no query was running.

        Raises:
            ConnectionError: Cannot reach the eval server.

        Example::

            import threading
            # Thread 1: long-running query
            def worker():
                try:
                    client.claude_query("complex question...")
                except QueryAbortedError:
                    print("Query was aborted")

            t = threading.Thread(target=worker)
            t.start()

            # Thread 2: abort after some time
            time.sleep(5)
            client.abort_query()
            t.join()
        """
        try:
            response = self._session.post(
                f"{self._base_url}/abort",
                timeout=5.0,
            )
        except requests.exceptions.ConnectionError as e:
            raise ConnectionError(f"Cannot connect to {self._base_url}: {e}") from e

        if response.status_code == 401:
            raise AuthenticationError("Invalid or missing API key")

        if response.status_code == 200:
            data = response.json()
            return "No query running" not in data.get("message", "")
        return False

    # ------------------------------------------------------------------
    # Connection & Configuration
    # ------------------------------------------------------------------

    def connect(
        self,
        protocol: str,
        host: str,
        port: int,
        catalog: str,
        repository: str,
        user: str,
        password: str,
    ) -> str:
        """Initialize AllegroGraph connection.

        Args:
            protocol: "http" or "https".
            host: AllegroGraph server hostname.
            port: AllegroGraph server port.
            catalog: Catalog name ("" or "/" for root catalog).
            repository: Repository name.
            user: AllegroGraph username.
            password: AllegroGraph password.

        Returns:
            Connection confirmation message.
        """
        expr = _lisp.lisp_call(
            "initialize-agraph-connection",
            _lisp.lisp_string(protocol),
            _lisp.lisp_string(host),
            _lisp.lisp_int(port),
            _lisp.lisp_string(catalog),
            _lisp.lisp_string(repository),
            _lisp.lisp_string(user),
            _lisp.lisp_string(password),
        )
        result = self.eval(expr)
        return result.stdout.strip()

    def test_connection(self) -> bool:
        """Test the AllegroGraph connection.

        Returns:
            True if connected successfully.
        """
        result = self.eval("(test-agraph-connection)")
        return result.parsed is True

    def set_repository(self, repo_string: str) -> str:
        """Switch the active AllegroGraph repository.

        Args:
            repo_string: Repository in "catalog:repository" format,
                or just "repository" for root catalog.

        Returns:
            Confirmation message.
        """
        expr = _lisp.lisp_call(
            "set-repository-from-string", _lisp.lisp_string(repo_string)
        )
        result = self.eval(expr)
        return result.stdout.strip()

    def set_query_library(self, repo_string: str) -> bool:
        """Set the query library repository location.

        Args:
            repo_string: "catalog:repository", "/:repo" for root catalog,
                or just "repo" for current catalog.

        Returns:
            True if repository exists and was set, False otherwise.
        """
        expr = _lisp.lisp_call("set-query-library", _lisp.lisp_string(repo_string))
        result = self.eval(expr)
        return result.parsed is True

    def set_meta_schema_repository(self, repo_string: str) -> bool:
        """Set the meta-schema repository for the semantic layer.

        Args:
            repo_string: "catalog:repository" or just repository name.

        Returns:
            True if repository exists and was set, False otherwise.
        """
        expr = _lisp.lisp_call(
            "set-meta-schema-repository", _lisp.lisp_string(repo_string)
        )
        result = self.eval(expr)
        return result.parsed is True

    def load_config(self, config_path: str) -> None:
        """Load configuration from a JSON file on the server.

        Args:
            config_path: Absolute path to config.json on the server.
        """
        expr = _lisp.lisp_call("load-config", _lisp.lisp_string(config_path))
        self.eval(expr)

    def set_api_key(self, api_key: str) -> None:
        """Set the Anthropic API key on the server.

        Args:
            api_key: Anthropic API key string.
        """
        expr = _lisp.lisp_setf("*anthropic-api-key*", _lisp.lisp_string(api_key))
        self.eval(expr)

    def set_model(self, model: str) -> None:
        """Set the Claude model to use.

        Args:
            model: Model identifier (e.g., "claude-sonnet-4-5-20250929").
        """
        expr = _lisp.lisp_setf("*claude-model*", _lisp.lisp_string(model))
        self.eval(expr)

    def get_variable(self, variable_name: str) -> Any:
        """Get the value of a Lisp configuration variable.

        Args:
            variable_name: Variable name including asterisks
                (e.g., "*claude-model*").

        Returns:
            The parsed Python value.
        """
        result = self.eval(variable_name)
        return result.parsed

    def set_variable(self, variable_name: str, value: Any) -> None:
        """Set a Lisp configuration variable.

        Args:
            variable_name: Variable name including asterisks
                (e.g., "*max-tokens*").
            value: Python value (str, int, float, bool, or None).
        """
        if isinstance(value, str):
            lisp_val = _lisp.lisp_string(value)
        elif isinstance(value, bool):
            lisp_val = _lisp.lisp_bool(value)
        elif isinstance(value, int):
            lisp_val = _lisp.lisp_int(value)
        elif isinstance(value, float):
            lisp_val = str(value)
        elif value is None:
            lisp_val = "nil"
        else:
            raise TypeError(f"Unsupported type: {type(value)}")

        expr = _lisp.lisp_setf(variable_name, lisp_val)
        self.eval(expr)

    # ------------------------------------------------------------------
    # Query Functions
    # ------------------------------------------------------------------

    def claude_query(
        self,
        question: str,
        *,
        continue_conversation: bool = True,
        max_iterations: int = 10,
        timeout: Optional[float] = None,
    ) -> QueryResult:
        """Ask Claude a question with full tool access.

        By default continues the previous conversation so Claude can
        reuse schema information and context from prior questions.

        Args:
            question: Natural language question about your data.
            continue_conversation: If True (default), continue existing
                conversation. If False, start fresh.
            max_iterations: Maximum tool-calling iterations (default: 10).
            timeout: Request timeout in seconds.

        Returns:
            QueryResult with answer, stdout, and raw_result.

        Example::

            result = client.claude_query("Find all employees in engineering")
            print(result.answer)
            # Follow-up (continues conversation by default):
            result = client.claude_query("Now show their salaries")
        """
        effective_timeout = timeout or self._default_query_timeout

        continue_flag = "t" if continue_conversation else "nil"
        expr = (
            f"(claude-query {_lisp.lisp_string(question)}"
            f" :continue {continue_flag}"
            f" :max-iterations {max_iterations})"
        )
        result = self.eval(expr, timeout=effective_timeout)
        return self._make_query_result(result)

    def claude_ask(
        self,
        question: str,
        *,
        max_iterations: int = 10,
        timeout: Optional[float] = None,
    ) -> QueryResult:
        """Ask Claude a fresh question (always starts new conversation).

        Convenience wrapper equivalent to
        claude_query(question, continue_conversation=False).

        Args:
            question: Natural language question.
            max_iterations: Maximum tool-calling iterations.
            timeout: Request timeout in seconds.

        Returns:
            QueryResult with answer, stdout, and raw_result.
        """
        effective_timeout = timeout or self._default_query_timeout

        expr = (
            f"(claude-ask {_lisp.lisp_string(question)}"
            f" :max-iterations {max_iterations})"
        )
        result = self.eval(expr, timeout=effective_timeout)
        return self._make_query_result(result)

    def generate_sparql(
        self,
        question: str,
        *,
        continue_conversation: bool = True,
        max_iterations: int = 10,
        timeout: Optional[float] = None,
    ) -> str:
        """Ask Claude to generate a SPARQL query for a natural language question.

        Claude will fetch the schema, examine example queries, and iteratively
        test the query before returning the final version.

        Args:
            question: Natural language question to convert to SPARQL.
            continue_conversation: If True (default), continue conversation.
            max_iterations: Maximum tool-calling iterations.
            timeout: Request timeout in seconds.

        Returns:
            The generated SPARQL query string.

        Example::

            sparql = client.generate_sparql("How many products were sold last month?")
            print(sparql)
        """
        effective_timeout = timeout or self._default_query_timeout

        continue_flag = "t" if continue_conversation else "nil"
        expr = (
            f"(generate-sparql-with-claude {_lisp.lisp_string(question)}"
            f" :continue {continue_flag}"
            f" :max-iterations {max_iterations})"
        )
        result = self.eval(expr, timeout=effective_timeout)

        sparql = result.parsed
        if sparql is None:
            return ""
        return str(sparql)

    # ------------------------------------------------------------------
    # Direct SPARQL (bypasses Claude)
    # ------------------------------------------------------------------

    def sparql_query(self, query: str) -> str:
        """Execute a SPARQL SELECT/CONSTRUCT/ASK/DESCRIBE query directly.

        Bypasses Claude -- runs the query directly against AllegroGraph.

        Args:
            query: SPARQL query string.

        Returns:
            Query results as a string.
        """
        expr = _lisp.lisp_execute_tool("sparql_query", query=_lisp.lisp_string(query))
        result = self.eval(expr)
        return result.parsed if isinstance(result.parsed, str) else result.result

    def sparql_update(self, update: str) -> str:
        """Execute a SPARQL UPDATE/INSERT/DELETE directly.

        Bypasses Claude -- runs the update directly against AllegroGraph.

        Args:
            update: SPARQL update string.

        Returns:
            Result confirmation string.
        """
        expr = _lisp.lisp_execute_tool(
            "sparql_update", update=_lisp.lisp_string(update)
        )
        result = self.eval(expr)
        return result.parsed if isinstance(result.parsed, str) else result.result

    # ------------------------------------------------------------------
    # Query Library
    # ------------------------------------------------------------------

    def store_query(
        self,
        title: str,
        description: str,
        sparql_query: str,
        repository: Optional[str] = None,
    ) -> str:
        """Store a SPARQL query in the query library.

        Args:
            title: Query title for identification.
            description: Human-readable description for search/discovery.
            sparql_query: The SPARQL query string.
            repository: Repository name override (defaults to current).

        Returns:
            Result confirmation.
        """
        params = {
            "title": _lisp.lisp_string(title),
            "description": _lisp.lisp_string(description),
            "sparqlQuery": _lisp.lisp_string(sparql_query),
        }
        if repository:
            params["repository"] = _lisp.lisp_string(repository)

        expr = _lisp.lisp_execute_tool("store_query", **params)
        result = self.eval(expr)
        return result.parsed if isinstance(result.parsed, str) else result.result

    def search_queries(self, search: str, repository: Optional[str] = None) -> str:
        """Search the query library by natural language description.

        Args:
            search: Search terms.
            repository: Repository name filter (optional).

        Returns:
            Matching queries as a string.
        """
        params = {"search": _lisp.lisp_string(search)}
        if repository:
            params["repository"] = _lisp.lisp_string(repository)

        expr = _lisp.lisp_execute_tool("search_queries", **params)
        result = self.eval(expr)
        return result.parsed if isinstance(result.parsed, str) else result.result

    def list_all_queries(self, repository: Optional[str] = None) -> str:
        """List all queries in the query library.

        Args:
            repository: Repository name filter (optional).

        Returns:
            All stored queries as a string.
        """
        params = {}
        if repository:
            params["repository"] = _lisp.lisp_string(repository)

        expr = _lisp.lisp_execute_tool("list_all_queries", **params)
        result = self.eval(expr)
        return result.parsed if isinstance(result.parsed, str) else result.result

    def delete_query(
        self,
        *,
        query_uri: Optional[str] = None,
        query_title: Optional[str] = None,
        repository: Optional[str] = None,
    ) -> str:
        """Delete a query from the query library.

        Provide either query_uri or query_title (with repository).

        Args:
            query_uri: URI of the query to delete.
            query_title: Title of the query to delete.
            repository: Repository name (used with query_title).

        Returns:
            Deletion confirmation.
        """
        params = {}
        if query_uri:
            params["queryUri"] = _lisp.lisp_string(query_uri)
        if query_title:
            params["queryTitle"] = _lisp.lisp_string(query_title)
        if repository:
            params["repository"] = _lisp.lisp_string(repository)

        if not params:
            raise ValueError("Must provide either query_uri or query_title")

        expr = _lisp.lisp_execute_tool("delete_query", **params)
        result = self.eval(expr)
        return result.parsed if isinstance(result.parsed, str) else result.result

    # ------------------------------------------------------------------
    # Visualizations
    # ------------------------------------------------------------------

    def get_pending_visualization(self, ref_id: str) -> PendingVisualization:
        """Fetch a cached visualization config by its reference ID.

        After Claude generates a chart and calls prepare_visualization,
        the config is cached server-side. Use this method to retrieve it
        for rendering without storing it permanently.

        Also works for map configs from build_map_visualization.

        Args:
            ref_id: Reference ID (e.g. "viz-config-123-456" or "map-config-123-456").

        Returns:
            PendingVisualization with config, type, description, and summary.

        Raises:
            EvalError: If the reference ID is not found (expired or invalid).

        Example::

            result = client.claude_query("Show product distribution as a pie chart")
            # Extract ref_id from result.answer or result.stdout
            viz = client.get_pending_visualization("viz-config-123-456")
            print(viz.viz_type)  # "pie_chart"
            print(viz.config)   # Chart.js config dict
        """
        expr = f"(gethash {_lisp.lisp_string(ref_id)} *pending-map-configs*)"
        result = self.eval(expr)

        raw = result.parsed
        if raw is None:
            raise EvalError(
                f"Pending visualization not found: {ref_id}",
                stdout=result.stdout,
            )

        if ref_id.startswith("viz-config-"):
            # viz-config entries are JSON strings with metadata
            try:
                data = json.loads(raw) if isinstance(raw, str) else raw
            except (json.JSONDecodeError, ValueError):
                return PendingVisualization(ref_id=ref_id, config=None)

            config_str = data.get("visualizationConfig")
            try:
                config = (
                    json.loads(config_str)
                    if isinstance(config_str, str)
                    else config_str
                )
            except (json.JSONDecodeError, ValueError):
                config = config_str

            return PendingVisualization(
                ref_id=ref_id,
                viz_type=data.get("visualizationType"),
                config=config,
                description=data.get("description"),
                summary=data.get("summary"),
            )
        else:
            # map-config entries are raw GeoJSON config strings
            try:
                config = json.loads(raw) if isinstance(raw, str) else raw
            except (json.JSONDecodeError, ValueError):
                config = raw
            return PendingVisualization(
                ref_id=ref_id,
                viz_type="map",
                config=config,
            )

    def get_visualizations(
        self,
        query_title: str,
        *,
        repository: Optional[str] = None,
    ) -> List[Visualization]:
        """Get all stored visualizations for a query from the query library.

        Args:
            query_title: Title of the query to get visualizations for.
            repository: Repository name filter (optional).

        Returns:
            List of Visualization objects.

        Example::

            vizs = client.get_visualizations("Product Distribution")
            for v in vizs:
                print(f"{v.viz_type}: {v.description}")
        """
        params = {"queryTitle": _lisp.lisp_string(query_title)}
        if repository:
            params["repository"] = _lisp.lisp_string(repository)

        expr = _lisp.lisp_execute_tool("get_query_visualizations", **params)
        result = self.eval(expr)

        return self._parse_visualization_list(result)

    def store_visualization(
        self,
        query_title: str,
        viz_config: str,
        repository: str,
        *,
        viz_type: Optional[str] = None,
        description: Optional[str] = None,
        summary: Optional[str] = None,
    ) -> str:
        """Store a visualization for a query in the query library.

        The viz_config can be a reference ID from prepare_visualization
        or build_map_visualization, in which case viz_type, description,
        and summary are auto-filled from the cached metadata.

        Args:
            query_title: Title of the query this visualization belongs to.
            viz_config: Reference ID string (preferred) or raw JSON config.
            repository: Repository name.
            viz_type: Visualization type (auto-filled when using reference ID).
            description: Description (auto-filled when using reference ID).
            summary: Markdown summary (auto-filled when using reference ID).

        Returns:
            Confirmation message with visualization ID.

        Example::

            # Using reference ID (preferred):
            client.store_visualization("Product Distribution",
                                       "viz-config-123-456", "my-repo")

            # Using raw config:
            client.store_visualization("Product Distribution",
                                       '{"type":"pie",...}', "my-repo",
                                       viz_type="pie_chart",
                                       description="Product breakdown")
        """
        params = {
            "queryTitle": _lisp.lisp_string(query_title),
            "visualizationConfig": _lisp.lisp_string(viz_config),
            "repository": _lisp.lisp_string(repository),
        }
        if viz_type:
            params["visualizationType"] = _lisp.lisp_string(viz_type)
        if description:
            params["description"] = _lisp.lisp_string(description)
        if summary:
            params["summary"] = _lisp.lisp_string(summary)

        expr = _lisp.lisp_execute_tool("store_query_visualization", **params)
        result = self.eval(expr)
        return result.parsed if isinstance(result.parsed, str) else result.result

    # ------------------------------------------------------------------
    # Token Usage & Cost
    # ------------------------------------------------------------------

    def get_token_cost_stats(self) -> TokenCostStats:
        """Get structured token usage, cost, and context statistics.

        Returns all token counts, estimated costs (USD), cache savings,
        and context window usage in a single structured object.

        Returns:
            TokenCostStats with all token/cost/context data.

        Example::

            stats = client.get_token_cost_stats()
            print(f"Total cost: ${stats.total_cost:.4f}")
            print(f"Context: {stats.context_percentage:.1f}% used")
            if stats.cache_savings > 0:
                print(f"Cache saved: ${stats.cache_savings:.4f}")

            # Per-query cost tracking:
            before = client.get_token_cost_stats()
            result = client.claude_query("Find all employees...")
            after = client.get_token_cost_stats()
            query_cost = after.total_cost - before.total_cost
        """
        result = self.eval("(get-token-cost-stats)")
        data = result.parsed

        if isinstance(data, dict):
            return TokenCostStats(**data)
        return TokenCostStats()

    def reset_context_stats(self) -> None:
        """Reset all token, cost, and cache statistics counters."""
        self.eval("(reset-context-stats)")

    def clear_conversation(self) -> None:
        """Clear conversation history while preserving all configuration.

        Preserves AllegroGraph connection, API keys, query library settings,
        prompt caching configuration, and SHACL cache.
        """
        self.eval("(clear-conversation-keep-config)")

    def condense_conversation(
        self,
        *,
        keep_recent: int = 2,
        verbose: bool = True,
    ) -> Optional[int]:
        """Condense conversation to reduce context size.

        Keeps recent interactions intact and prunes intermediate tool calls
        from older interactions.

        Args:
            keep_recent: Number of recent interactions to keep intact.
            verbose: Print condensation details.

        Returns:
            Number of messages removed, or None if nothing to condense.
        """
        verbose_flag = "t" if verbose else "nil"
        expr = (
            f"(condense-conversation"
            f" :keep-recent {keep_recent}"
            f" :verbose {verbose_flag})"
        )
        result = self.eval(expr)
        return result.parsed

    def start_conversation(self) -> None:
        """Start a new conversation, clearing previous history."""
        self.eval("(start-conversation)")

    def show_conversation_history(self) -> str:
        """Display the current conversation history.

        Returns:
            Formatted conversation history.
        """
        result = self.eval("(show-conversation-history)")
        return result.stdout

    def set_prompt_caching(self, enabled: bool) -> None:
        """Enable or disable prompt caching.

        Args:
            enabled: True to enable, False to disable.
        """
        self.set_variable("*enable-prompt-caching*", enabled)

    # ------------------------------------------------------------------
    # Session Management
    # ------------------------------------------------------------------

    @property
    def username(self) -> Optional[str]:
        """Get the current username tag used for session management."""
        return self._username

    @username.setter
    def username(self, value: Optional[str]):
        """Set the username tag used for session management."""
        self._username = value

    def save_session(
        self,
        title: str,
        *,
        session_id: Optional[str] = None,
    ) -> str:
        """Save the current conversation as a named session.

        Args:
            title: Human-readable title for the session.
            session_id: Optional ID to update an existing session.
                If None, a new session ID is generated.

        Returns:
            The session ID (can be used for restore/delete).

        Example::

            sid = client.save_session("Employee analysis Q3")
            # Later:
            client.restore_session(sid)
        """
        parts = [
            "(save-session-to-library",
            _lisp.lisp_string(session_id) if session_id else "nil",
            _lisp.lisp_string(title),
        ]
        if self._username:
            parts.append(f":username {_lisp.lisp_string(self._username)}")
        expr = " ".join(parts) + ")"
        result = self.eval(expr)
        return result.parsed if isinstance(result.parsed, str) else result.result

    def list_sessions(
        self,
        *,
        username: Any = _USE_CLIENT_USERNAME,
        repository: Optional[str] = None,
    ) -> List[SessionInfo]:
        """List saved sessions, optionally filtered by username and repository.

        By default, filters to the client's username (if set).
        Pass ``username=None`` explicitly to see all users' sessions.

        Args:
            username: Filter by username. Defaults to ``self.username``.
                Pass ``None`` to list all sessions regardless of owner.
            repository: Filter by repository name (optional).

        Returns:
            List of SessionInfo objects sorted by most-recently modified.

        Example::

            sessions = client.list_sessions()
            for s in sessions:
                print(f"{s.title} ({s.message_count} messages)")
        """
        # Resolve sentinel to client's username
        effective_username = (
            self._username if username is _USE_CLIENT_USERNAME else username
        )

        parts = ["(list-sessions-as-json"]
        if repository:
            parts.append(_lisp.lisp_string(repository))
        else:
            parts.append("nil")
        if effective_username:
            parts.append(f":username {_lisp.lisp_string(effective_username)}")
        expr = " ".join(parts) + ")"
        result = self.eval(expr)
        return self._parse_session_list(result)

    def restore_session(self, session_id: str) -> SessionInfo:
        """Restore a previously saved session into the conversation history.

        Args:
            session_id: The session ID returned by save_session().

        Returns:
            SessionInfo with metadata about the restored session.

        Raises:
            EvalError: If the session is not found.
        """
        uri = f"http://franz.com/ns/conversation-session#{session_id}"
        expr = _lisp.lisp_call("restore-session-from-library", _lisp.lisp_string(uri))
        result = self.eval(expr)
        return self._make_session_info(result.parsed)

    def delete_session(self, session_id: str) -> None:
        """Delete a saved session.

        Args:
            session_id: The session ID to delete.
        """
        uri = f"http://franz.com/ns/conversation-session#{session_id}"
        expr = _lisp.lisp_call("delete-session-from-library", _lisp.lisp_string(uri))
        self.eval(expr)

    # ------------------------------------------------------------------
    # Utilities
    # ------------------------------------------------------------------

    def show_available_tools(self) -> str:
        """List all available tools with descriptions.

        Returns:
            Formatted tool list.
        """
        result = self.eval("(show-available-tools)")
        return result.stdout

    def execute_tool(self, tool_name: str, **params: Any) -> str:
        """Execute a named tool directly (bypasses Claude).

        This is a lower-level API for calling any of the 30+ tools directly.
        Parameter values are automatically converted to Lisp representations.

        Args:
            tool_name: Tool name (e.g., "sparql_query", "get_shacl").
            \**params: Tool parameters as keyword arguments.

        Returns:
            Tool result as a string.

        Example::

            result = client.execute_tool("get_shacl")
            result = client.execute_tool(
                "sparql_query",
                query="SELECT ?s WHERE { ?s ?p ?o } LIMIT 5",
            )
        """
        lisp_params = {}
        for key, value in params.items():
            if isinstance(value, str):
                lisp_params[key] = _lisp.lisp_string(value)
            elif isinstance(value, bool):
                lisp_params[key] = _lisp.lisp_bool(value)
            elif isinstance(value, int):
                lisp_params[key] = _lisp.lisp_int(value)
            elif isinstance(value, float):
                lisp_params[key] = str(value)
            elif value is None:
                lisp_params[key] = ":null"
            else:
                lisp_params[key] = _lisp.lisp_string(str(value))

        expr = _lisp.lisp_execute_tool(tool_name, **lisp_params)
        result = self.eval(expr)
        return result.parsed if isinstance(result.parsed, str) else result.result

    def set_max_iterations(self, n: int) -> int:
        """Set the default maximum iterations for Claude queries.

        Args:
            n: Number of iterations (1-100).

        Returns:
            The new value.
        """
        expr = _lisp.lisp_call("set-max-iterations", _lisp.lisp_int(n))
        result = self.eval(expr)
        return result.parsed

    # ------------------------------------------------------------------
    # Internal helpers
    # ------------------------------------------------------------------

    def _make_query_result(self, result: EvalResult) -> QueryResult:
        """Convert an EvalResult from a Claude query into a QueryResult."""
        answer = result.parsed
        if answer is None:
            answer = ""
        if not isinstance(answer, str):
            answer = str(answer)
        return QueryResult(
            answer=answer,
            stdout=result.stdout,
            raw_result=result.result,
        )

    def _parse_session_list(self, result: EvalResult) -> List[SessionInfo]:
        """Parse a JSON string of sessions into a list of SessionInfo."""
        raw = result.parsed
        if raw is None:
            return []
        # The Lisp wrapper returns a JSON string
        if isinstance(raw, str):
            try:
                data = json.loads(raw)
            except (json.JSONDecodeError, ValueError):
                return []
        elif isinstance(raw, list):
            data = raw
        else:
            return []

        sessions = []
        for item in data:
            uri = item.get("uri", "")
            # Extract session ID from URI fragment
            sid = uri.rsplit("#", 1)[-1] if "#" in uri else uri
            sessions.append(
                SessionInfo(
                    session_id=sid,
                    title=item.get("title", ""),
                    username=item.get("username"),
                    repository=item.get("repository", ""),
                    message_count=item.get("messageCount") or 0,
                    created=item.get("created"),
                    modified=item.get("modified"),
                )
            )
        return sessions

    def _parse_visualization_list(self, result: EvalResult) -> List[Visualization]:
        """Parse get_query_visualizations result into a list of Visualization."""
        raw = result.parsed
        if raw is None or not isinstance(raw, str):
            return []

        # The Lisp tool returns "Found N visualization(s)...\n[JSON array]"
        # or "No visualizations found..." — find the JSON array in the output
        json_start = raw.find("[")
        if json_start == -1:
            return []

        try:
            data = json.loads(raw[json_start:])
        except (json.JSONDecodeError, ValueError):
            return []

        vizs = []
        for item in data:
            vizs.append(
                Visualization(
                    viz_id=item.get("vizId", ""),
                    viz_type=item.get("type", ""),
                    config=item.get("config", ""),
                    description=item.get("description", ""),
                    summary=item.get("summary"),
                    created=item.get("created"),
                )
            )
        return vizs

    @staticmethod
    def _make_session_info(data: Any) -> SessionInfo:
        """Convert a parsed plist dict into a SessionInfo."""
        if not isinstance(data, dict):
            return SessionInfo(session_id="", title="")
        return SessionInfo(
            session_id=data.get("session_id", "") or "",
            title=data.get("title", "") or "",
            username=data.get("username"),
            repository=data.get("repository", "") or "",
            message_count=data.get("message_count") or 0,
            created=data.get("created"),
            modified=data.get("modified"),
        )
