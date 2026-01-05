# AllegroGraph MCP Server

An MCP (Model Context Protocol) server that exposes AllegroGraph RDF database
functionality to Claude and other AI applications. This module enables AI models
to query, manage, and analyze RDF data stored in AllegroGraph repositories.

## Installation

The MCP server is included in
[agraph-python](https://github.com/franzinc/agraph-python) as an optional
dependency:

```bash
pip install "agraph-python[mcp]"
```

## Configuration

### Environment Variables

The server connects to AllegroGraph using these environment variables:

| Variable          | Default     | Description                             |
| ----------------- | ----------- | --------------------------------------- |
| `AGRAPH_PROTOCOL` | `http`      | Connection protocol (`http` or `https`) |
| `AGRAPH_HOST`     | `127.0.0.1` | AllegroGraph server hostname            |
| `AGRAPH_PORT`     | `10035`     | AllegroGraph server port                |
| `AGRAPH_USER`     | -           | Authentication username                 |
| `AGRAPH_PASSWORD` | -           | Authentication password                 |

### Claude Desktop Configuration

Add this to your Claude Desktop MCP configuration file:

```json
{
  "mcpServers": {
    "allegrograph": {
      "command": "python",
      "args": ["-m", "franz.mcp.server"],
      "env": {
        "AGRAPH_PROTOCOL": "https",
        "AGRAPH_HOST": "your-server.example.com",
        "AGRAPH_PORT": "10035",
        "AGRAPH_USER": "your-username",
        "AGRAPH_PASSWORD": "your-password"
      }
    }
  }
}
```

## Running the Server

Start the server directly:

```bash
python -m franz.mcp.server
```

## Available Tools

### Repository Management

| Tool                  | Description                                                 |
| --------------------- | ----------------------------------------------------------- |
| `list_repositories`   | List all repositories in a catalog                          |
| `get_repository_info` | Get metadata about a repository (triple count, server info) |
| `get_namespaces`      | Get namespace prefix mappings for SPARQL queries            |

### Schema Discovery

| Tool        | Description                                                                                                                                                                   |
| ----------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `get_shacl` | Get SHACL shapes describing the repository schema. Call this first before writing SPARQL queries. Supports `compressed` (default, token-efficient) and `full` output formats. |

### SPARQL Queries

| Tool                  | Description                                        |
| --------------------- | -------------------------------------------------- |
| `sparql_select_query` | Execute SELECT queries, returns JSON results       |
| `sparql_graph_query`  | Execute CONSTRUCT/DESCRIBE queries, returns Turtle |
| `sparql_ask_query`    | Execute ASK queries, returns boolean               |
| `sparql_update_query` | Execute UPDATE operations (INSERT/DELETE)          |

### Data Management

| Tool          | Description                                                                    |
| ------------- | ------------------------------------------------------------------------------ |
| `add_triples` | Add RDF triples in various formats (RDF/XML, N-Triples, Turtle, JSON-LD, etc.) |

### Full-Text Search

| Tool                        | Description                                   |
| --------------------------- | --------------------------------------------- |
| `list_freetext_indices`     | List all freetext indices in a repository     |
| `get_freetext_index_config` | Get configuration details of a freetext index |

## Recommended Workflow

1. **Discover repositories**: Use `list_repositories` to find available data
2. **Understand the schema**: Call `get_shacl` to learn the data structure before querying
3. **Get namespaces**: Use `get_namespaces` for SPARQL prefix definitions
4. **Query data**: Execute SPARQL queries using the appropriate tool
5. **Modify data**: Use `add_triples` or `sparql_update_query` to update the repository

## Supported RDF Formats

The `add_triples` tool accepts data in these formats:

- RDF/XML
- N-Triples
- N-Quads
- Extended N-Quads
- Turtle
- TriG
- TriX
- Table
- JSON-LD
