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

import traceback

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
    
    def createLiteral(self, label, datatype=None, language=None):
        """
        Create a new literal with label 'label'.  'datatype' if supplied,
        should be a URI, in which case 'label' should be a string.
        """
        return Literal(label, datatype=datatype, language=language)
        

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
        
        

