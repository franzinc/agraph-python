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

__all__ = ['OWL', 'RDF', 'RDFS', 'XMLSchema']
