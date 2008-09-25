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
from franz.openrdf.util import uris

import traceback

class Value(object):
    """
    Top class in the org.openrdf.model interfaces.
    """
    
    def __str__(self): raise UnimplementedMethodException()
    def __eq__(self): raise UnimplementedMethodException()
    def __hash__(self): raise UnimplementedMethodException()   
    
class Resource(Value):
    pass

class URI(Resource):
    """
    Lightweight implementation of the class 'URI'.
    """
    def __init__(self, uri=None, namespace=None, localname=None):
        if uri and not isinstance(uri, str):
            print "NON-STRING PASSED TO URI CONSTRUCTOR ", uri , "  ", type(uri) ## WE ARE NOT SURE WHAT THIS MEANS.  NEED TO FIGURE IT OUT - RMM
            traceback.print_stack()
            raise IllegalArgumentException("Object of type %s passed to URI constructor where string expected: %s"
                                           % (type(uri), uri))
        if uri:
            self.uri = uri
        elif namespace and localname:
            self.uri = namespace + localname
        else:
            self.uri = None        
        self.localNameIdx = -1
    
    def __str__(self): return self.getURI()
    
    def getURI(self):
        """
        Return the URI (string) for 'self'.  This method is typically
        overloaded by subclasses, which may use lazy evaluation to
        retrieve the string.
        """
        return self.uri
    
    def getLocalName(self):
        if (self.localNameIdx < 0):
            self.localNameIdx = uris.getLocalNameIndex(self.getURI())
        return self.uri[self.localNameIdx]
    
    def getNamespace(self):
        if (self.localNameIdx < 0):
            self.localNameIdx = uris.getLocalNameIndex(self.getURI())
        return self.uri[0, self.localNameIdx]
    
class BNode(Resource):
    """
    """
    def __init__(self, id=None):
        self.id = id
        
    def getId(self):
        return self.id

class Namespace(object):
    """
    """
    def __init__(self, prefix, name):
        self.setPrefix(prefix)
        self.setName(name)
    
    def getName(self):
        """
        Gets the name of the current namespace (i.e. it's URI).
        """
        return self.name

    def setName(self, name): self.name = name
    
    def getPrefix(self):
        """
        Gets the prefix of the current namespace.
        """
        return self.prefix

    def setPrefix(self, prefix): self.prefix = prefix
    
    def __str__(self):
        """
        Return an odd name (that's what the Sesame code does).
        """
        return self.prefix + " :: " + self.name
