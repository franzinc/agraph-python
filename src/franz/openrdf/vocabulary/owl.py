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

class OWL:
    NAMESPACE = "http://www.w3.org/2002/07/owl#"
#    TYPE = None
#    PROPERTY = None
#    XMLLITERAL = None
     
    ## map of uri strings to URI objects:
    name2URI = {}
    
    @staticmethod
    def initialize(factory):
        """
        Initialize the constant using factory 'factory'
        """
#        RDF.TYPE = factory.createURI(namespace=RDF.NAMESPACE, localname="type")
#        RDF.PROPERTY = factory.createURI(namespace=RDF.NAMESPACE, localname="Property")
#        RDF.XMLLITERAL = factory.createURI(namespace=RDF.NAMESPACE, localname="XMLLiteral")
        
        ## (re)build 'name2URI' dictionary
        OWL.name2URIMap = {}
        for uri in [OWL.TYPE, OWL.PROPERTY,]:
            OWL.name2URIMap[str(uri)] = uri

            
    @staticmethod
    def reinitialize(factory, store=None):
        """
        Initialize the values in the factory, or
        reinitialize the values in factory with more efficient
        resources and literals (one's that know what store they
        belong to).
        """
#        RDF.TYPE = factory.createURI(namespace=RDF.NAMESPACE, localname="type")
#        RDF.PROPERTY = factory.createURI(namespace=RDF.NAMESPACE, localname="Property")
#        RDF.XMLLITERAL = factory.createURI(namespace=RDF.NAMESPACE, localname="XMLLiteral")
        ## (re)build 'name2URI' dictionary
        OWL.name2URIMap = {}
        for uri in [OWL.TYPE, OWL.PROPERTY,]:
            OWL.name2URIMap[str(uri)] = uri
        
    
    @staticmethod
    def name2URI (name, exception_if_failure=True):
        """
        Given a URI string, return the OpenRDF URI object.
        """
        matchingURI = OWL.name2URIMap.get(name)
        if matchingURI: return matchingURI
        elif exception_if_failure:
            raise IllegalArgumentException("Passed a non-OWL URI to 'XMLSchema.name2URI.")
        else: return None
    



