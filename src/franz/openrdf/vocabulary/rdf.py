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

class RDF:
    NAMESPACE = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    TYPE = None
    PROPERTY = None
    XMLLITERAL = None
    SUBJECT = None
    PREDICATE = None
    STATEMENT = None
    BAG = None
    ALT = None
    SEQ = None
    VALUE = None
    LI = None
    LIST = None
    FIRST = None
    REST = None
    NIL = None
    
    ## map of uri strings to URI objects:
    name2URI = {}
    
    @staticmethod
    def initialize(factory):
        """
        Initialize the constant using factory 'factory'
        """
        RDF.TYPE = factory.createURI(namespace=RDF.NAMESPACE, localname="type")
        RDF.PROPERTY = factory.createURI(namespace=RDF.NAMESPACE, localname="Property")
        RDF.XMLLITERAL = factory.createURI(namespace=RDF.NAMESPACE, localname="XMLLiteral")
        RDF.SUBJECT = factory.createURI(namespace=RDF.NAMESPACE, localname="subject")
        RDF.PREDICATE = factory.createURI(namespace=RDF.NAMESPACE, localname="predicate")
        RDF.OBJECT = factory.createURI(namespace=RDF.NAMESPACE, localname="object")
        RDF.STATEMENT = factory.createURI(namespace=RDF.NAMESPACE, localname="Statement")
        RDF.BAG = factory.createURI(namespace=RDF.NAMESPACE, localname="Bag")
        RDF.ALT = factory.createURI(namespace=RDF.NAMESPACE, localname="Alt")
        RDF.SEQ = factory.createURI(namespace=RDF.NAMESPACE, localname="Seq")
        RDF.VALUE = factory.createURI(namespace=RDF.NAMESPACE, localname="value")
        RDF.LI = factory.createURI(namespace=RDF.NAMESPACE, localname="li")
        RDF.LIST = factory.createURI(namespace=RDF.NAMESPACE, localname="List")
        RDF.FIRST = factory.createURI(namespace=RDF.NAMESPACE, localname="first")
        RDF.REST = factory.createURI(namespace=RDF.NAMESPACE, localname="rest")
        RDF.NIL = factory.createURI(namespace=RDF.NAMESPACE, localname="nil")
        ## (re)build 'name2URI' dictionary
        RDF.name2URIMap = {}
        for uri in [RDF.TYPE, RDF.PROPERTY, RDF.XMLLITERAL, RDF.SUBJECT, RDF.PREDICATE, RDF.OBJECT,
                    RDF.STATEMENT, RDF.BAG, RDF.ALT, RDF.SEQ, RDF.VALUE, RDF.LI, RDF.LIST, RDF.FIRST,
                    RDF.REST, RDF.NIL,]:
            RDF.name2URIMap[str(uri)] = uri

            
    @staticmethod
    def reinitialize(factory, store=None):
        """
        Initialize the values in the factory, or
        reinitialize the values in factory with more efficient
        resources and literals (one's that know what store they
        belong to).
        """
        RDF.TYPE = factory.createURI(namespace=RDF.NAMESPACE, localname="type", store=store)
        RDF.PROPERTY = factory.createURI(namespace=RDF.NAMESPACE, localname="Property", store=store)
        RDF.XMLLITERAL = factory.createURI(namespace=RDF.NAMESPACE, localname="XMLLiteral", store=store)
        RDF.SUBJECT = factory.createURI(namespace=RDF.NAMESPACE, localname="subject", store=store)
        RDF.PREDICATE = factory.createURI(namespace=RDF.NAMESPACE, localname="predicate", store=store)
        RDF.OBJECT = factory.createURI(namespace=RDF.NAMESPACE, localname="object", store=store)
        RDF.STATEMENT = factory.createURI(namespace=RDF.NAMESPACE, localname="Statement", store=store)
        RDF.BAG = factory.createURI(namespace=RDF.NAMESPACE, localname="Bag", store=store)
        RDF.ALT = factory.createURI(namespace=RDF.NAMESPACE, localname="Alt", store=store)
        RDF.SEQ = factory.createURI(namespace=RDF.NAMESPACE, localname="Seq", store=store)
        RDF.VALUE = factory.createURI(namespace=RDF.NAMESPACE, localname="value", store=store)
        RDF.LI = factory.createURI(namespace=RDF.NAMESPACE, localname="li", store=store)
        RDF.LIST = factory.createURI(namespace=RDF.NAMESPACE, localname="List", store=store)
        RDF.FIRST = factory.createURI(namespace=RDF.NAMESPACE, localname="first", store=store)
        RDF.REST = factory.createURI(namespace=RDF.NAMESPACE, localname="rest", store=store)
        RDF.NIL = factory.createURI(namespace=RDF.NAMESPACE, localname="nil", store=store)
        ## (re)build 'name2URI' dictionary
        RDF.name2URIMap = {}
        for uri in [RDF.TYPE, RDF.PROPERTY, RDF.XMLLITERAL, RDF.SUBJECT, RDF.PREDICATE, RDF.OBJECT,
                    RDF.STATEMENT, RDF.BAG, RDF.ALT, RDF.SEQ, RDF.VALUE, RDF.LI, RDF.LIST, RDF.FIRST,
                    RDF.REST, RDF.NIL,]:
            RDF.name2URIMap[str(uri)] = uri
        
    
    @staticmethod
    def name2URI (name):
        """
        Given a URI string, return the OpenRDF URI object.
        """
        return RDF.name2URIMap.get(name)
    



