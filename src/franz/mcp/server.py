import json
import logging
import os
from contextlib import contextmanager
from functools import lru_cache
from io import BytesIO
from typing import Any, Generator, Literal

from mcp.server.fastmcp import FastMCP

from franz.miniclient.request import jsonRequest
from franz.openrdf.repository import Repository
from franz.openrdf.repository.repositoryconnection import RepositoryConnection
from franz.openrdf.rio.rdfformat import RDFFormat
from franz.openrdf.sail.allegrographserver import AllegroGraphServer

logging.basicConfig(
    level=logging.INFO, format="%(asctime)s - %(name)s - %(levelname)s - %(message)s"
)
logger = logging.getLogger(__name__)


@lru_cache(maxsize=1)
def get_ag_server() -> AllegroGraphServer:
    return AllegroGraphServer(
        protocol=os.environ.get("AGRAPH_PROTOCOL", "http"),
        host=os.environ.get("AGRAPH_HOST", "127.0.0.1"),
        port=int(os.environ.get("AGRAPH_PORT", "10035")),
        user=os.environ.get("AGRAPH_USER"),
        password=os.environ.get("AGRAPH_PASSWORD"),
    )


@contextmanager
def ag_connect(
    repo: str,
    catalog: str,
    access_verb: Literal["RENEW", "ACCESS", "OPEN", "CREATE", "REPLACE"],
) -> Generator[RepositoryConnection, None, None]:
    if not repo:
        raise ValueError("Empty repository name is not allowed!")

    catalog = catalog or "root"

    server = get_ag_server()
    with server.openCatalog(catalog).getRepository(
        repo, access_verb=access_verb
    ) as repo_handle:
        with repo_handle.getConnection() as conn:
            yield conn


mcp = FastMCP("AllegroGraph")


@mcp.tool()
def list_repositories(catalog: str = "root") -> list[str]:
    """List all repositories in a catalog.

    Args:
        catalog: Catalog name (defaults to 'root')

    Returns:
        List of repository names
    """
    if not catalog:
        catalog = "root"

    server = get_ag_server()
    catalog_handle = server.openCatalog(catalog)
    return catalog_handle.listRepositories()


@mcp.tool()
def get_repository_info(repo: str, catalog: str = "root") -> dict[str, str | int]:
    """Get metadata about a repository.

    Args:
        repo: Repository name
        catalog: Catalog name (defaults to 'root')

    Returns:
        Repository metadata including triple count
    """
    with ag_connect(repo, catalog, Repository.OPEN) as conn:
        return {
            "repository": repo,
            "catalog": catalog,
            "server": get_ag_server().url,
            "triplesCount": conn.size(),
        }


@mcp.tool()
def get_namespaces(repo: str, catalog: str = "root") -> dict[str, str]:
    """Get namespace prefix mappings defined in the repository.

    Use this to understand what prefixes are available for SPARQL queries.

    Args:
        repo: Repository name
        catalog: Catalog name (defaults to 'root')

    Returns:
        Dictionary mapping prefixes to namespace URIs.
    """
    with ag_connect(repo, catalog, Repository.OPEN) as conn:
        return conn.getNamespaces()


@mcp.tool()
def add_triples(
    triples: str,
    format: Literal[
        "RDF/XML",
        "N-Triples",
        "N-Quads",
        "Extended N-Quads (with attributes)",
        "Turtle",
        "TriG",
        "TriX",
        "Table",
        "JSON-LD",
    ],
    repo: str,
    catalog: str = "root",
) -> dict[str, str | bool]:
    """Add RDF triples to a repository.

    Args:
        triples: RDF data as a string in the specified format
        format: Serialization format of the input data
        repo: Repository name
        catalog: Catalog name (defaults to 'root')

    Returns:
        Status of the operation with repository info

    Example:
        add_triples(
            triples='<http://example.org/s> <http://example.org/p> "value" .',
            format="N-Triples",
            repo="myrepo"
        )
    """
    with ag_connect(repo, catalog, Repository.ACCESS) as conn:
        format_map = {
            "RDF/XML": RDFFormat.RDFXML,
            "N-Triples": RDFFormat.NTRIPLES,
            "N-Quads": RDFFormat.NQUADS,
            "Extended N-Quads (with attributes)": RDFFormat.NQX,
            "Turtle": RDFFormat.TURTLE,
            "TriG": RDFFormat.TRIG,
            "TriX": RDFFormat.TRIX,
            "Table": RDFFormat.TABLE,
            "JSON-LD": RDFFormat.JSONLD,
        }
        conn.addData(triples, rdf_format=format_map[format])
        return {"success": True, "repo": repo, "catalog": catalog}


@mcp.tool()
def get_shacl(
    repo: str,
    catalog: str = "root",
    format: Literal["full", "compressed"] = "compressed",
) -> dict[str, Any]:
    """Get SHACL shapes describing the repository schema.

    IMPORTANT: Call this FIRST before writing SPARQL queries. Returns SHACL shapes
    that describe all available classes, predicates, and their constraints in the
    repository. This eliminates guessing and prevents errors by providing the exact
    schema.

    Args:
        repo: Repository name
        catalog: Catalog name (defaults to 'root')
        format: Output format:
            - "compressed" (default): Optimized format with prefixes and classes only,
              60-70% smaller, better for token efficiency
            - "full": Raw SHACL JSON-LD with complete shape details

    Returns:
        SHACL shapes describing the repository schema
    """
    with ag_connect(repo, catalog, Repository.OPEN) as conn:
        server = get_ag_server()
        if server.versionTuple >= (8, 5, 0):
            return jsonRequest(
                conn.mini_repository,
                method="GET",
                url="/shacl-shapes/generate",
                accept="application/vnd.franz.terse-cpt-schema+json",
            )
        else:
            shacl_data = jsonRequest(
                conn.mini_repository,
                method="GET",
                url="/data-generator/shacl",
                accept="application/json",
            )

            if format == "full":
                return shacl_data
            return compress_shacl(shacl_data)


def compress_shacl(shacl_data: dict[str, Any]) -> dict[str, Any]:
    """Compress SHACL JSON-LD to a more token-efficient format.

    Extracts the essential information needed for writing SPARQL queries:
    - Prefixes (namespace mappings)
    - Classes with their properties, types, and constraints
    """
    result = {
        "prefixes": {},
        "classes": {},
    }

    # Extract @context as prefixes
    context = shacl_data.get("@context", {})
    if isinstance(context, dict):
        for key, value in context.items():
            if isinstance(value, str) and not key.startswith("@"):
                result["prefixes"][key] = value

    # Extract shapes from @graph
    graph = shacl_data.get("@graph", [])
    if not isinstance(graph, list):
        graph = [graph] if graph else []

    for shape in graph:
        if not isinstance(shape, dict):
            continue

        # Get the target class this shape describes
        target_class = shape.get("sh:targetClass")
        if not target_class:
            continue

        # Normalize target_class to string
        if isinstance(target_class, dict):
            target_class = target_class.get("@id", str(target_class))

        class_info = {"properties": {}}

        # Extract properties from sh:property
        properties = shape.get("sh:property", [])
        if not isinstance(properties, list):
            properties = [properties] if properties else []

        for prop in properties:
            if not isinstance(prop, dict):
                continue

            # Get property path
            path = prop.get("sh:path")
            if not path:
                continue
            if isinstance(path, dict):
                path = path.get("@id", str(path))

            prop_info = {}

            # Extract datatype
            datatype = prop.get("sh:datatype")
            if datatype:
                if isinstance(datatype, dict):
                    datatype = datatype.get("@id", str(datatype))
                prop_info["datatype"] = datatype

            # Extract class constraint (for object properties)
            node_class = prop.get("sh:class")
            if node_class:
                if isinstance(node_class, dict):
                    node_class = node_class.get("@id", str(node_class))
                prop_info["class"] = node_class

            # Extract cardinality
            min_count = prop.get("sh:minCount")
            max_count = prop.get("sh:maxCount")
            if min_count is not None:
                prop_info["minCount"] = min_count
            if max_count is not None:
                prop_info["maxCount"] = max_count

            # Extract nodeKind (IRI, Literal, etc.)
            node_kind = prop.get("sh:nodeKind")
            if node_kind:
                if isinstance(node_kind, dict):
                    node_kind = node_kind.get("@id", str(node_kind))
                prop_info["nodeKind"] = node_kind

            # Only add if we have useful info
            if prop_info:
                class_info["properties"][path] = prop_info
            else:
                # Still record the property exists, just no constraints
                class_info["properties"][path] = {}

        result["classes"][target_class] = class_info

    return result


@mcp.tool()
def sparql_select_query(
    query: str,
    repo: str,
    catalog: str = "root",
    limit: int = 100,
) -> dict[str, Any]:
    """Execute a SPARQL SELECT query.

    Args:
        query: The SPARQL query to execute
        repo: Repository name
        catalog: Catalog name (defaults to 'root')
        limit: Max number of rows of the query result (defaults to 100)

    Returns:
        Query results in SPARQL 1.1 Query Results JSON Format (https://www.w3.org/TR/sparql11-results-json/)
    """
    with ag_connect(repo, catalog, Repository.OPEN) as conn:
        mini_repo = conn.mini_repository
        results = json.loads(
            mini_repo.evalSparqlQuery(query, accept="application/sparql-results+json")
        )

        if len(results["results"]["bindings"]) > limit:
            results["results"]["bindings"] = results["results"]["bindings"][0:limit]

        return results


@mcp.tool()
def sparql_graph_query(query: str, repo: str, catalog: str = "root") -> str:
    """Execute a SPARQL CONSTRUCT or DESCRIBE query.

    Args:
        query: The SPARQL query to execute
        repo: Repository name
        catalog: Catalog name (defaults to 'root')

    Returns:
        Query results in Turtle format (https://www.w3.org/TR/turtle/)
    """
    with ag_connect(repo, catalog, Repository.OPEN) as conn:
        buffer = BytesIO()
        conn.prepareGraphQuery(query=query).evaluate(
            output=buffer, output_format=RDFFormat.TURTLE
        )
        return buffer.getvalue().decode("utf-8")


@mcp.tool()
def sparql_ask_query(query: str, repo: str, catalog: str = "root") -> bool:
    """Execute a SPARQL ASK query that returns a boolean result.

    ASK queries test whether a pattern exists in the data. Use this instead of
    sparql_query when you only need a yes/no answer.

    Args:
        query: The SPARQL ASK query to execute (must be an ASK query)
        repo: Repository name
        catalog: Catalog name (defaults to 'root')

    Returns:
        True if the pattern exists, False otherwise

    Example:
        sparql_ask("ASK { ?s a <http://schema.org/Person> }", repo="myrepo")
    """
    with ag_connect(repo, catalog, Repository.OPEN) as conn:
        return conn.prepareBooleanQuery(query=query).evaluate()


@mcp.tool()
def sparql_update_query(
    query: str, repo: str, catalog: str = "root"
) -> dict[str, bool | str]:
    """Execute a SPARQL UPDATE (INSERT/DELETE) operation.

    Args:
        query: The SPARQL UPDATE query to execute
        repo: Repository name
        catalog: Catalog name (defaults to 'root')

    Returns:
        Status of the update operation
    """
    with ag_connect(repo, catalog, Repository.ACCESS) as conn:
        return {
            "success": conn.prepareUpdate(query=query).evaluate(),
            "repository": repo,
            "catalog": catalog,
        }


@mcp.tool()
def list_freetext_indices(repo: str, catalog: str = "root") -> list[str]:
    """List all freetext indices in a repository.

    Args:
        repo: Repository name
        catalog: Catalog name (defaults to 'root')

    Returns:
        List of freetext index names
    """
    with ag_connect(repo, catalog, Repository.OPEN) as conn:
        return conn.listFreeTextIndices()


@mcp.tool()
def get_freetext_index_config(
    index_name: str, repo: str, catalog: str = "root"
) -> dict[str, Any]:
    """Get configuration details of a freetext index.

    Args:
        index_name: Name of the freetext index
        repo: Repository name
        catalog: Catalog name (defaults to 'root')

    Returns:
        Index configuration including indexed predicates and settings
    """
    with ag_connect(repo, catalog, Repository.OPEN) as conn:
        return conn.getFreeTextIndexConfiguration(index_name)


if __name__ == "__main__":
    mcp.run()
