# pylint: disable-msg=C0103

from __future__ import absolute_import

from .owl import OWL
from .rdf import RDF
from .rdfs import RDFS
from .xmlschema import XMLSchema

__all__ = [ 'OWL', 'RDF', 'RDFS', 'XMLSchema' ]
