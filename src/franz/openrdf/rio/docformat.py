################################################################################
# Copyright (c) 2006-2017 Franz Inc.
# All rights reserved. This program and the accompanying materials are
# made available under the terms of the MIT License which accompanies
# this distribution, and is available at http://opensource.org/licenses/MIT
################################################################################

from franz.openrdf.rio.formats import Format


class DocFormat(Format):
    """
    Represents a document format. Documents are non-RDF files that
    can be converted to triples using the transform service of AG.
    """

    # A global dictionary mapping extensions to formats
    # Used by Format.format_for_file_name
    _ext_map = {}

    def __init__(self, name, mime_types=None, file_extensions=None, register=True):
        """
        Initialize a new document format object.

        :param name: Human-readable name of the format.
        :param mime_types: A list of MIME types used for this format.
                           The first element of this list will be used
                           as the content-type header during uploads.
        :param file_extensions: List of file extensions for this format.
        :param register: If True file extensions will be added to the map
                         used by :meth:`Format.format_for_file_name`.
        """
        Format.__init__(self, name, mime_types, "UTF-8", file_extensions, register)

    # These will be automatically converted to DocFormat instances

    CSV = dict(name="CSV", mime_types=["text/csv"], file_extensions=["csv"])

    JSON = dict(name="JSON", mime_types=["application/json"], file_extensions=["json"])

    JSON_LINES = dict(
        name="JSON lines (http://jsonlines.org/)",
        mime_types=["jsonl"],
        file_extensions=["jsonl"],
    )
