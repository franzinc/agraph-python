# pylint: disable-msg=C0103

################################################################################
# Copyright (c) 2006-2017 Franz Inc.  
# All rights reserved. This program and the accompanying materials are
# made available under the terms of the MIT License which accompanies
# this distribution, and is available at http://opensource.org/licenses/MIT
################################################################################

"""
Classes containing useful URI constants.
"""

from __future__ import absolute_import
from __future__ import unicode_literals

from future.utils import iteritems

from franz.openrdf.model.value import URI
from .owl import OWL
from .rdf import RDF
from .rdfs import RDFS
from .xmlschema import XMLSchema

__all__ = ['OWL', 'RDF', 'RDFS', 'XMLSchema', 'canonical_uri_object']

_uri_map = {}


def _compute_uri_map():
    for cls in OWL, RDF, RDFS, XMLSchema:
        for name, uri in iteritems(cls.__dict__):
            if name.isupper and isinstance(uri, URI):
                _uri_map[uri.uri] = uri

_compute_uri_map()


def canonical_uri_object(uri):
    """
    Check if there is a predefined constant for ``uri`` and return it.

     If there is no such consatnt ``None`` is returned.

    :param uri: URI string.
    :type uri: string
    :return: An URI object or ``None``.
    :rtype: URI
    """
    return _uri_map.get(uri)
