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
from franz.openrdf.model.value import BNode, URI
from franz.openrdf.model.literal import Literal
from franz.openrdf.model.statement import Statement
from franz.openrdf.vocabulary.rdf import RDF
from franz.openrdf.vocabulary.xmlschema import XMLSchema

import datetime, traceback

class ValueFactory(object):
    """
    A factory for creating URIs, blank nodes, literals and statements.
    """
    def __init__(self):
        RDF.initialize(self)

    def createBNode(self, nodeID=None):
        """
        Creates a new blank node with the given node identifier.
        """
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
    
    @staticmethod
    def stringTermToTerm(string_term):
        """
        Given a string representing a term in ntriples format, return
        a URI, Literal, or BNode.
        TODO: BNODES NOT YET IMPLEMENTED
        """
        if not string_term: return string_term
        if string_term[0] == '<':
            uri = string_term[1:-1]
            return URI(uri)
        elif string_term[0] == '"':
            ## we have a double-quoted literal with either a data type or a language indicator
            caratPos = string_term.find('^^')
            if caratPos >= 0:
                label = string_term[1:caratPos - 1]
                datatype = string_term[caratPos + 2:]
                return Literal(label, datatype=datatype)
            else:
                atPos = string_term.find('@')
                label = string_term[1:atPos - 1]
                language = string_term[atPos + 1:]
                return Literal(label, language=language)
        else:
            raise UnimplementedMethodException("BNodes not yet implemented by 'stringTermToTerm'")
        

