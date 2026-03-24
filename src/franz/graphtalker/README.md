# GraphTalker Python Client

Python interface for GraphTalker -- AI-powered AllegroGraph RDF queries via Claude.

## Quick Start

```python
from franz.graphtalker import GraphTalkerClient

# Connect to eval server
client = GraphTalkerClient(port=8080, api_key="my-secret-key")

# Initialize AllegroGraph connection
client.connect("http", "localhost", 10035, "", "hr-analytics", "test", "xyzzy")
assert client.test_connection()

# Ask Claude about your data
result = client.claude_query("What data is in this repository?")
print(result.answer)

# Follow-up question (continues conversation by default)
result = client.claude_query("Show me all the classes and their instance counts")
print(result.answer)

# Generate SPARQL specifically
sparql = client.generate_sparql("Find employees hired after 2020")
print(sparql)
```

## Query Library

Store and search queries programmatically:

```python
# Store a query
client.store_query(
    title="Department Employee Counts",
    description="Shows each department with the number of employees",
    sparql_query="SELECT ?dept (COUNT(?emp) AS ?count) WHERE { ... }",
)

# Search for queries
results = client.search_queries("department employee")
print(results)

# List all stored queries
all_queries = client.list_all_queries()
print(all_queries)
```

## Direct SPARQL (bypasses Claude)

```python
results = client.sparql_query("SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10")
print(results)
```

## Aborting Queries

Long-running queries can be aborted from a different thread:

```python
import threading, time
from graphtalker import QueryAbortedError

# Thread 1: long query
def worker():
    try:
        result = client.claude_query("Complex question...")
    except QueryAbortedError:
        print("Query was aborted")

thread = threading.Thread(target=worker)
thread.start()

# Thread 2: abort after 10 seconds
time.sleep(10)
client.abort_query()
thread.join()
```

## Token Usage & Cost Tracking

```python
# Get cumulative stats for the session
stats = client.get_token_cost_stats()
print(f"Total cost: ${stats.total_cost:.4f} ({stats.total_tokens:,} tokens)")
print(f"Cache saved: ${stats.cache_savings:.4f}")
print(f"Context: {stats.context_percentage:.1f}% used")

# Track cost of a single query
before = client.get_token_cost_stats()
result = client.claude_query("Find all employees")
after = client.get_token_cost_stats()
print(f"Query cost: ${after.total_cost - before.total_cost:.4f}")

# Reset counters for a new tracking period
client.reset_context_stats()
```

## Context Management

```python
# Condense when context grows large
stats = client.get_token_cost_stats()
if stats.context_percentage > 80:
    client.condense_conversation()

# Start fresh conversation (keeps connection config)
client.clear_conversation()
```

## Session Management

Save and restore conversation sessions, tagged by username:

```python
client = GraphTalkerClient(port=8080, api_key="key", username="alice")
client.connect("http", "localhost", 10035, "", "hr-analytics", "test", "xyzzy")

# Save current conversation
sid = client.save_session("Q3 Employee Analysis")

# List sessions (defaults to current user)
sessions = client.list_sessions()
for s in sessions:
    print(f"{s.title} ({s.message_count} messages)")

# List all users' sessions
all_sessions = client.list_sessions(username=None)

# Restore a session
info = client.restore_session(sid)

# Delete a session
client.delete_session(sid)
```

## Visualizations

Fetch on-the-fly or stored chart/map/network visualizations:

```python
# Ask Claude to create a chart (it caches the config server-side automatically)
result = client.claude_query("Show product distribution as a pie chart")

# Fetch the cached config by reference ID (no permanent storage needed)
viz = client.get_pending_visualization("viz-config-123-456")
print(viz.viz_type)   # "pie_chart"
print(viz.config)     # Chart.js config dict

# Store it permanently in the query library (using the reference ID)
client.store_visualization("Product Distribution", viz.ref_id, "my-repo")

# Retrieve stored visualizations later
vizs = client.get_visualizations("Product Distribution")
for v in vizs:
    print(f"{v.viz_type}: {v.description}")
```

## Low-Level Escape Hatch

For advanced users who need to evaluate arbitrary Lisp expressions:

```python
result = client.eval("(+ 1 2)")
print(result.parsed)  # 3

# Read/write Lisp variables
model = client.get_variable("*claude-model*")
client.set_variable("*max-tokens*", 32000)
```

## API Reference

### Constructor

```python
GraphTalkerClient(
    host="localhost",       # Eval server hostname
    port=8080,              # Eval server port
    api_key=None,           # API key for authentication
    timeout=30.0,           # Default timeout (seconds)
    default_query_timeout=300.0,  # Timeout for Claude queries (seconds)
    username=None,          # Optional username for session management
)
```

Supports context manager:

```python
with GraphTalkerClient(port=8080, api_key="key") as client:
    ...
```

### Query Methods

| Method | Description |
|--------|-------------|
| `claude_query(question, *, continue_conversation=True, max_iterations=10)` | Ask Claude with tool access |
| `claude_ask(question, *, max_iterations=10)` | Always starts fresh conversation |
| `generate_sparql(question, *, continue_conversation=True, max_iterations=10)` | Get SPARQL query string |
| `sparql_query(query)` | Execute SPARQL directly |
| `sparql_update(update)` | Execute SPARQL UPDATE directly |
| `abort_query()` | Abort running query from another thread |

### Token Usage & Cost

| Method | Description |
|--------|-------------|
| `get_token_cost_stats()` | Returns `TokenCostStats` with tokens, costs, cache savings, context usage |
| `reset_context_stats()` | Reset all token/cost counters to zero |

### Query Library

| Method | Description |
|--------|-------------|
| `store_query(title, description, sparql_query, repository=None)` | Store a query |
| `search_queries(search, repository=None)` | Search by description |
| `list_all_queries(repository=None)` | List all queries |
| `delete_query(*, query_uri=None, query_title=None, repository=None)` | Delete a query |

### Session Management

| Method | Description |
|--------|-------------|
| `save_session(title, *, session_id=None)` | Save conversation, returns session ID |
| `list_sessions(*, username=..., repository=None)` | List sessions (defaults to client's username) |
| `restore_session(session_id)` | Restore session, returns `SessionInfo` |
| `delete_session(session_id)` | Delete a session |

### Visualizations

| Method | Description |
|--------|-------------|
| `get_pending_visualization(ref_id)` | Fetch cached visualization by reference ID |
| `get_visualizations(query_title, *, repository=None)` | Get stored visualizations for a query |
| `store_visualization(query_title, viz_config, repository, ...)` | Store visualization in query library |

### Context Management

| Method | Description |
|--------|-------------|
| `clear_conversation()` | Clear context, keep config |
| `condense_conversation(*, keep_recent=2)` | Prune old interactions |
| `start_conversation()` | Start fresh, clear all history |
