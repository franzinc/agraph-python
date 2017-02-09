#!/usr/bin/env python
# -*- coding: utf-8 -*-
# pylint: disable-msg=C0103 

################################################################################
# Copyright (c) 2006-2017 Franz Inc.  
# All rights reserved. This program and the accompanying materials are
# made available under the terms of the MIT License which accompanies
# this distribution, and is available at http://opensource.org/licenses/MIT
################################################################################

from __future__ import absolute_import
from __future__ import unicode_literals
from builtins import object

from .value import Value, BNode, URI
from .literal import Literal, CompoundLiteral, RangeLiteral, GeoCoordinate
from .statement import Statement
from ..vocabulary.xmlschema import XMLSchema

class ValueFactory(object):
    """
    A factory for creating URIs, blank nodes, literals and statements.
    """
    BLANK_NODE_AMOUNT = 10    
    def __init__(self, store):
        self.store = store
        self.unusedBNodeIds = []
        
    def getUnusedBNodeId(self):
        if not self.unusedBNodeIds:
            ## retrieve a set of bnode ids (they include leading '_:', which we strip off later:
            self.unusedBNodeIds = self.store.mini_repository.getBlankNodes(amount=ValueFactory.BLANK_NODE_AMOUNT)
        return self.unusedBNodeIds.pop()[2:] ## strip off leading '_:'

    def createBNode(self, nodeID=None):
        """
        Creates a new blank node with the given node identifier.
        """
        if not nodeID:
            nodeID = self.getUnusedBNodeId()
        return BNode(nodeID)
    
    def createLiteral(self, value, datatype=None, language=None):
        """
        Create a new literal with value 'value'.  'datatype' if supplied,
        should be a URI, in which case 'value' should be a string.
        """
        if isinstance(value, (tuple, list)) and len(value) == 2:
            return self.createRange(value[0], value[1])

        return Literal(value, datatype=datatype, language=language)
        

    def createStatement(self, subject, predicate, _object, context=None):
        """
        Create a new statement with the supplied subject, predicate and object
        and associated context.  Arguments have type Resource, URI, Value, and Resource.
        """
        return Statement(subject, predicate, _object, context=context)
    
    def createURI(self, uri=None, namespace=None, localname=None):
        """
        Creates a new URI from the supplied string-representation(s).
        If two non-keyword arguments are passed, assumes they represent a
        namespace/localname pair.
        """
        if namespace and not localname:
            return URI(namespace=uri, localname=namespace)
        else:
            return URI(uri=uri, namespace=namespace, localname=localname)
    

#############################################################################
## Extension to Sesame API
#############################################################################

    def validateRangeConstant(self, term, predicate):
        """Validate an individual range constant"""
        datatype = term.getDatatype()
        if not datatype:
            raise Exception('Illegal term in range expression "%s" needs to '
                            'have a datatype.' % term.getValue())

    def validateCompoundLiteral(self, term, predicate):
        """
        Check to see if range boundaries are mapped.
        TODO: ADD VALIDATION FOR GEO TERMS
        """
        if isinstance(term, RangeLiteral):
            self.validateRangeConstant(term.lowerBound, predicate)
            self.validateRangeConstant(term.upperBound, predicate)
        elif isinstance(term, GeoCoordinate):
            pass
        
    def object_position_term_to_openrdf_term(self, term, predicate=None):
        """
        If 'term' is a string, integer, float, etc, convert it to
        a Literal term.  Otherwise, if its a Value, just pass it through.
        """
        if term is not None:
            if isinstance(term, CompoundLiteral): 
                self.validateCompoundLiteral(term, predicate)
            elif not isinstance(term, Value):
                term = self.createLiteral(term)
        return term
    
    def createRange(self, lowerBound, upperBound):
        """
        Create a compound literal representing a range from 'lowerBound' to 'upperBound'
        """
        lowerBound = self.object_position_term_to_openrdf_term(lowerBound)
        upperBound = self.object_position_term_to_openrdf_term(upperBound)
        return RangeLiteral(lowerBound=lowerBound, upperBound=upperBound)
