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

class QueryLanguage:
    registered_languages = []
    SPARQL = None
    def __init__(self, name):
        self.name = name
        QueryLanguage.registered_languages.append(self)
        
    def __str__(self): return self.name
    
    def getName(self): return self.name

    @staticmethod
    def values(): return QueryLanguage.registered_languages[:]
    
    @staticmethod
    def valueOf(name): 
        for ql in QueryLanguage.registered_languages:
            if ql.name.lower() == name.lower():
                return ql
        return None
    
QueryLanguage.SPARQL = QueryLanguage('SPARQL')

#############################################################################
##
#############################################################################

class Query(object):
    """
    A query on a {@link Repository} that can be formulated in one of the
    supported query languages (for example SeRQL or SPARQL). It allows one to
    predefine bindings in the query to be able to reuse the same query with
    different bindings.
    """

    def setBinding(self, name, value):
        """
        Binds the specified variable to the supplied value. Any value that was
        previously bound to the specified value will be overwritten.
        """
        raise UnimplementedMethodException("setBinding")

    def removeBinding(self, name):
        """ 
        Removes a previously set binding on the supplied variable. Calling this
        method with an unbound variable name has no effect.
        """ 
        raise UnimplementedMethodException("removeBinding")

    def getBindings(self):
        """
        Retrieves the bindings that have been set on this query. 
        """ 
        raise UnimplementedMethodException("getBindings")

    def setDataset(self, dataset):
        """
        Specifies the dataset against which to evaluate a query, overriding any
        dataset that is specified in the query itself. 
        """ 
        raise UnimplementedMethodException("setDataset")
     
    def getDataset(self):
        """
        Gets the dataset that has been set using {@link #setDataset(Dataset)}, if  any. 
        """ 
        raise UnimplementedMethodException("setBinding")
     
    def setIncludeInferred(self, getDataset):
        """
        Determine whether evaluation results of this query should include inferred
        statements (if any inferred statements are present in the repository). The
        default setting is 'true'. 
        """ 
        raise UnimplementedMethodException("setIncludeInferred")

    def getIncludeInferred(self):
        """
        Returns whether or not this query will return inferred statements (if any
        are present in the repository). 
        """ 
        raise UnimplementedMethodException("getIncludeInferred")
    

#############################################################################
##
#############################################################################

class TupleQuery(Query):
    
    def evaluate(self):
        """
        Execute the embedded query against the RDF store.  Return
        an iterator that produces for each step a tuple of values
        (resources and literals) corresponding to the variables
        or expressions in a 'select' clause (or its equivalent).
        """
        raise UnimplementedMethodException("evaluate")

class GraphQuery(Query):
    
    def evaluate(self):
        """
        Execute the embedded query against the RDF store.  Return
        an iterator that produces for each step a Statement.
        """
        raise UnimplementedMethodException("evaluate")

class BooleanQuery(Query):
    
    def evaluate(self):
        """
        Execute the embedded query against the RDF store.  Return
        true or false
        """
        raise UnimplementedMethodException("evaluate")


