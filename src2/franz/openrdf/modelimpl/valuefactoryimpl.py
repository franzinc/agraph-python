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

from franz.allegrograph.encodedliteral import EncodedLiteral
from franz.openrdf.model.value import BNode, URI, Value
from franz.openrdf.model.literal import Literal
from franz.openrdf.model.valuefactory import ValueFactory
from franz.openrdf.modelimpl.valueimpl import URIImpl, BNodeImpl
from franz.openrdf.modelimpl.literalimpl import LiteralImpl, CompoundLiteral
from franz.openrdf.modelimpl.statementimpl import StatementImpl
from franz.openrdf.model.statement import Statement
from franz.openrdf.vocabulary.rdf import RDF
from franz.openrdf.vocabulary.rdfs import RDFS
from franz.openrdf.vocabulary.xmlschema import XMLSchema
from franz.openrdf.vocabulary.owl import OWL

import datetime
    
class ValueFactoryImpl(ValueFactory):
    """
    AllegroGraph-specific implementation of ValueFactory
    """
    def __init__(self, store):
        self.store = store
        RDF.reinitialize(self, store)
        RDFS.reinitialize(self, store)        
        XMLSchema.reinitialize(self, store)
        OWL.reinitialize(self, store)
        Literal.reinitialize()

    def createBNode(self, nodeID=None):
        """
        Creates a new blank node with the given node identifier.
        """
        return BNodeImpl(None, id=nodeID, store=self.store)
    
    def object_position_term_to_openrdf_term(self, term, predicate=None):
        """
        If 'term' is a string, integer, float, etc, convert it to
        a Literal term.  Otherwise, if its a Value, just pass it through.
        """
        if term is None: return term
        if isinstance(term, CompoundLiteral): return term
        if not isinstance(term, Value):
            term = self.createLiteral(term)
        inlinedType = self.store.inlined_predicates.get(predicate.getURI()) if predicate else None            
        if not inlinedType and isinstance(term, Literal) and term.datatype:
            inlinedType = self.store.inlined_datatypes.get(term.datatype)
        if inlinedType:
            return EncodedLiteral(term.getLabel(), encoding=inlinedType, store=self.store.internal_ag_store)
        else:
            return term
    
    def _interpret_value(self, value, datatype):
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
    
    def createLiteral(self, value, datatype=None, language=None, upi=None, store=None):
        """
        Create a new literal with value 'value'.  'datatype' if supplied,
        should be a URI, in which case 'value' should be a string.
        """
        value, datatype = self._interpret_value(value, datatype)
        return LiteralImpl(value, datatype=datatype, language=language, upi=upi, store=store)

    def createStatement(self, subject, predicate, object, context=None):
        """
        Create a new statement with the supplied subject, predicate and object
        and associated context.  Arguments have type Resource, URI, Value, and Resource.
        """
        return StatementImpl(subject, predicate, object, context=context, store=self.store)
    
    def createURI(self, uri=None, namespace=None, localname=None, store=None, upi=None):
        """
        Creates a new URI from the supplied string-representation(s)
        """
        return URIImpl(uri=uri, namespace=namespace, localname=localname, store=self.store, upi=upi)
    
    def createRange(self, lowerBound, upperBound):
        """
        Create a compound literal representing a range from 'lowerBound' to 'upperBound'
        """
        lowerBound = self.object_position_term_to_openrdf_term(lowerBound)
        upperBound = self.object_position_term_to_openrdf_term(upperBound)
        return CompoundLiteral(choice=CompoundLiteral.RANGE_LITERAL, lowerBound=lowerBound, upperBound=upperBound)
        

