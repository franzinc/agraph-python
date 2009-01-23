#!/usr/bin/env python
# -*- coding: utf-8 -*-

##***** BEGIN LICENSE BLOCK *****
##Version: MPL 1.1
##
##The contents of this file are subject to the Mozilla Public License Version
##1.1 (the "License"); you may not use this file except in compliance with
##the License. You may obtain a copy of the License at
##http:##www.mozilla.org/MPL/
##
##Software distributed under the License is distributed on an "AS IS" basis,
##WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
##for the specific language governing rights and limitations under the
##License.
##
##The Original Code is the AllegroGraph Java Client interface.
##
##The Original Code was written by Franz Inc.
##Copyright (C) 2006 Franz Inc.  All Rights Reserved.
##
##***** END LICENSE BLOCK *****

from franz.openrdf.exceptions import *
from franz.openrdf.model.value import Value, BNode, URI
from franz.openrdf.model.literal import Literal, CompoundLiteral
from franz.openrdf.model.statement import Statement
from franz.openrdf.vocabulary.rdf import RDF
from franz.openrdf.vocabulary.rdfs import RDFS
from franz.openrdf.vocabulary.owl import OWL
from franz.openrdf.vocabulary.xmlschema import XMLSchema

import datetime, traceback

class ValueFactory(object):
    """
    A factory for creating URIs, blank nodes, literals and statements.
    """
    BLANK_NODE_AMOUNT = 10    
    def __init__(self, store):
        self.store = store
        RDF.initialize(self)
        RDFS.initialize(self)
        XMLSchema.initialize(self)
        OWL.initialize(self)
        self.store.getConnection().setNamespace("fti", "http://franz.com/ns/allegrograph/2.2/textindex/")
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
    
    @staticmethod
    def _interpret_value(value, datatype):
        """
        If 'self' is not a string, convert it into one, and infer its
        datatype, unless 'datatype' is set (i.e., overrides it).
        """
        if isinstance(value, str):
            return value, datatype
        elif isinstance(value, int):
            return str(value), datatype or XMLSchema.INT
        elif isinstance(value, float):
            return str(value), datatype or XMLSchema.FLOAT
        elif isinstance(value, bool):
            return str(value), datatype or XMLSchema.BOOLEAN
        elif isinstance(value, datetime.datetime):
            return str(value), datatype or XMLSchema.DATETIME
        elif isinstance(value, datetime.time):
            return str(value), datatype or XMLSchema.TIME
        elif isinstance(value, datetime.date):
            return str(value), datatype or XMLSchema.DATE
        else:
            return str(value), datatype
    
    def createLiteral(self, value, datatype=None, language=None):
        """
        Create a new literal with value 'value'.  'datatype' if supplied,
        should be a URI, in which case 'value' should be a string.
        """
        if isinstance(value, (tuple, list)) and len(value) == 2:
            return self.createRange(value[0], value[1])
        value, datatype = ValueFactory._interpret_value(value, datatype)
        return Literal(value, datatype=datatype, language=language)
        

    def createStatement(self, subject, predicate, object, context=None):
        """
        Create a new statement with the supplied subject, predicate and object
        and associated context.  Arguments have type Resource, URI, Value, and Resource.
        """
        return Statement(subject, predicate, object, context=context)
    
    def createURI(self, uri=None, namespace=None, localname=None):
        """
        Creates a new URI from the supplied string-representation(s)
        """
        return URI(uri=uri, namespace=namespace, localname=localname)
    

#############################################################################
## Extension to Sesame API
#############################################################################

    def validateRangeConstant(self, term, predicate):
        datatype = term.getDatatype()
        if not datatype:
            raise Exception("Illegal term in range expression '%s' needs to have a datatype." % term.getValue())
        rep = self.store.getConnection().repository
        if rep.mapped_datatypes.get(datatype): return
        elif predicate and rep.mapped_predicates.get(predicate.getURI()): return
        else:
            raise Exception("Illegal term in range expression '%s' with datatype '%s' does not have a datatype or predicate mapping." %
                             (term.getValue(), datatype))

    def validateCompoundLiteral(self, term, predicate):
        """
        Check to see if range boundaries are mapped.
        """
        if not isinstance(term, CompoundLiteral): return
        if term.choice == CompoundLiteral.RANGE_LITERAL:
            self.validateRangeConstant(term.lowerBound, predicate)
            self.validateRangeConstant(term.upperBound, predicate)            

        
    def object_position_term_to_openrdf_term(self, term, predicate=None):
        """
        If 'term' is a string, integer, float, etc, convert it to
        a Literal term.  Otherwise, if its a Value, just pass it through.
        """
        if term is None: return term
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
        return CompoundLiteral(choice=CompoundLiteral.RANGE_LITERAL, lowerBound=lowerBound, upperBound=upperBound)
        


