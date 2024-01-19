from franz.openrdf.rio.formats import Format


class TupleFormat(Format):
    """
    Serialization formats for tuples returned by queries.
    See :class:`.Format`.
    """

    # Map from extensions to formats, used by Format.format_for_file_name
    _ext_map = {}

    # These will be automatically converted to TupleFormat instances

    SPARQL = dict(
        name="SPARQL XML",
        mime_types=["application/sparql-results+xml", "application/xml"],
        file_extensions=["xml", "sx"],
    )

    JSON = dict(
        name="SPARQL JSON",
        mime_types=["application/sparql-results+json", "application/json"],
        file_extensions=["json"],
    )

    CSV = dict(name="SPARQL CSV", mime_types=["text/csv"], file_extensions=["csv"])

    TSV = dict(
        name="SPARQL TSV",
        mime_types=["text/tab-separated-values"],
        file_extensions=["tsv"],
    )

    TABLE = dict(name="Table", mime_types=["text/table"], file_extensions=[])
