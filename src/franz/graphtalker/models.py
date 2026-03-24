"""Dataclasses for structured results from the GraphTalker eval server."""

from dataclasses import dataclass, field
from typing import Any, Optional


@dataclass
class EvalResult:
    """Raw result from the eval server.

    Attributes:
        stdout: All printed output from the Lisp side.
        result: The return value string (Lisp ~S format).
        error: Error message if evaluation failed, None otherwise.
        parsed: The result string parsed into a Python value.
    """

    stdout: str
    result: str
    error: Optional[str]
    parsed: Any = field(init=False, default=None)

    def __post_init__(self):
        from ._lisp import parse_lisp_result

        if self.error is None:
            self.parsed = parse_lisp_result(self.result)


@dataclass
class QueryResult:
    """Result from a Claude query (claude_query, claude_ask, generate_sparql).

    Attributes:
        answer: Claude's final answer text.
        stdout: Full conversation output (iterations, tool calls, etc.).
        raw_result: The raw Lisp result string.
    """

    answer: str
    stdout: str
    raw_result: str


@dataclass
class TokenCostStats:
    """Token usage, cost, and context statistics from get_token_cost_stats().

    Provides a single structured view of all token counts, estimated costs,
    cache savings, and context window usage for the current session.

    Attributes:
        input_tokens: Total input tokens (cumulative).
        output_tokens: Total output tokens (cumulative).
        cache_write_tokens: Tokens used to create cache entries.
        cache_read_tokens: Tokens read from cache.
        total_tokens: Sum of all token categories.
        input_cost: Estimated cost for input tokens (USD).
        output_cost: Estimated cost for output tokens (USD).
        cache_write_cost: Estimated cost for cache writes (USD).
        cache_read_cost: Estimated cost for cache reads (USD).
        total_cost: Total estimated cost (USD).
        cost_without_cache: What it would have cost without caching (USD).
        cache_savings: Money saved by caching (USD).
        cache_savings_percent: Cache savings as percentage.
        context_tokens: Current conversation size (tokens).
        context_percentage: Current context window usage as percentage.
        context_limit: Anthropic's context limit (200000).
        message_count: Number of messages in conversation.
    """

    input_tokens: int = 0
    output_tokens: int = 0
    cache_write_tokens: int = 0
    cache_read_tokens: int = 0
    total_tokens: int = 0
    input_cost: float = 0.0
    output_cost: float = 0.0
    cache_write_cost: float = 0.0
    cache_read_cost: float = 0.0
    total_cost: float = 0.0
    cost_without_cache: float = 0.0
    cache_savings: float = 0.0
    cache_savings_percent: float = 0.0
    context_tokens: int = 0
    context_percentage: float = 0.0
    context_limit: int = 200000
    message_count: int = 0


@dataclass
class PendingVisualization:
    """A visualization config cached server-side, not yet stored permanently.

    Created by Claude calling prepare_visualization (charts/network graphs)
    or build_map_visualization (maps). Retrievable by reference ID.

    Attributes:
        ref_id: Reference ID (e.g. "viz-config-123-456" or "map-config-123-456").
        viz_type: Visualization type (e.g. "pie_chart", "map", "network_graph").
        config: The visualization config as a parsed dict (Chart.js, GeoJSON, etc.).
        description: Description of what the visualization shows.
        summary: Optional markdown narrative summary.
    """

    ref_id: str
    viz_type: Optional[str] = None
    config: Optional[dict] = None
    description: Optional[str] = None
    summary: Optional[str] = None


@dataclass
class Visualization:
    """A visualization stored permanently in the query library.

    Attributes:
        viz_id: Visualization URI in the query library.
        viz_type: Visualization type (e.g. "bar_chart", "pie_chart").
        config: The visualization config as a string (JSON).
        description: Description of what the visualization shows.
        summary: Optional markdown narrative summary.
        created: ISO 8601 creation timestamp.
    """

    viz_id: str
    viz_type: str
    config: str
    description: str = ""
    summary: Optional[str] = None
    created: Optional[str] = None


@dataclass
class SessionInfo:
    """Information about a saved conversation session.

    Attributes:
        session_id: Unique session identifier (fragment after # in URI).
        title: Human-readable session title.
        username: Owner username tag (None if not set).
        repository: Repository the session was saved against.
        message_count: Number of messages in the saved conversation.
        created: ISO 8601 creation timestamp.
        modified: ISO 8601 last-modified timestamp.
    """

    session_id: str
    title: str
    username: Optional[str] = None
    repository: str = ""
    message_count: int = 0
    created: Optional[str] = None
    modified: Optional[str] = None
