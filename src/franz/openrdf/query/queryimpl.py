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
from franz.openrdf.query.query import *
from franz.openrdf.sail.repositoryresultimpl import RepositoryResultImpl

#############################################################################
##
#############################################################################

class AbstractQuery(Query):
    """
    Abstract super class of all query types
    """
    def __init__(self, queryLanguage, queryString, baseURI=None):
        self.queryLanguage = queryLanguage
        self.queryString = queryString
        self.baseURI = baseURI
        self.dataset = None
        self.includeInferred = False

    def setBinding(self, name, value):
        """
        Binds the specified variable to the supplied value. Any value that was
        previously bound to the specified value will be overwritten.
        """
        self.bindings.addBinding(name, value)

    def removeBinding(self, name):
        """ 
        Removes a previously set binding on the supplied variable. Calling this
        method with an unbound variable name has no effect.
        """ 
        self.bindings.removeBinding(name)

    def getBindings(self):
        """
        Retrieves the bindings that have been set on this query. 
        """ 
        return self.bindings

    def setDataset(self, dataset):
        """
        Specifies the dataset against which to evaluate a query, overriding any
        dataset that is specified in the query itself. 
        """ 
        self.dataset = dataset
     
    def getDataset(self):
        """
        Gets the dataset that has been set using {@link #setDataset(Dataset)}, if  any. 
        """ 
        return self.dataset
     
    def setIncludeInferred(self, includeInferred):
        """
        Determine whether evaluation results of this query should include inferred
        statements (if any inferred statements are present in the repository). The
        default setting is 'true'. 
        """ 
        self.includeInferred = includeInferred

    def getIncludeInferred(self):
        """
        Returns whether or not this query will return inferred statements (if any
        are present in the repository). 
        """ 
        return self.includeInferred

class BooleanQueryImpl(GraphQuery, AbstractQuery):
    def __init__(self, queryLanguage, queryString, baseURI=None):
        if not queryLanguage == QueryLanguage.SPARQL:
            raise IllegalOptionException("Can't evaluate the query language '%s'.  Options are: SPARQL."
                                         % QueryLanguage.SPARQL)
        super(GraphQuery, self).__init__(queryLanguage, queryString, baseURI=baseURI)
        self.direct_caller = None
        
    def setDirectCaller(self, caller):
        """
        Internal call to embed the evaluator context into the query.
        """
        self.direct_caller = caller
    
    ## NOT YET IMPLEMENTED:
#    def evaluate(self):
#        """
#        Execute the embedded query against the RDF store.  Return
#        an iterator that produces for each step a Statement
#        """
#        statementIt = self.direct_caller.twinqlFind(self.queryString, None, 0, 0, includeInferred=self.includeInferred, more=[])
#        return statementIt


class GraphQueryImpl(GraphQuery, AbstractQuery):
    def __init__(self, queryLanguage, queryString, baseURI=None):
        if not queryLanguage == QueryLanguage.SPARQL:
            raise IllegalOptionException("Can't evaluate the query language '%s'.  Options are: SPARQL."
                                         % QueryLanguage.SPARQL)
        super(GraphQuery, self).__init__(queryLanguage, queryString, baseURI=baseURI)
        self.direct_caller = None
        
    def setDirectCaller(self, caller):
        """
        Internal call to embed the evaluator context into the query.
        """
        self.direct_caller = caller
    
    def evaluate(self):
        """
        Execute the embedded query against the RDF store.  Return
        an iterator that produces for each step a Statement
        """
        more = []
        statementIt = self.direct_caller.twinqlFind(self.queryString, 0, 0, includeInferred=self.includeInferred, more=more)
        return statementIt


class TupleQueryImpl(TupleQuery, AbstractQuery):
    def __init__(self, queryLanguage, queryString, baseURI=None):
        if not queryLanguage == QueryLanguage.SPARQL:
            raise IllegalOptionException("Can't evaluate the query language '%s'.  Options are: SPARQL."
                                         % QueryLanguage.SPARQL)
        super(TupleQueryImpl, self).__init__(queryLanguage, queryString, baseURI=baseURI)
        self.direct_caller = None
        self.connection = None
        
    def setDirectCaller(self, caller):
        """
        Internal call to embed the evaluator context into the query.
        """
        self.direct_caller = caller
        
    def setConnection(self, connection):
        """
        Internal call to embed the conneciton into the query.
        """
        self.connection = connection
    
    def evaluate(self):
        """
        Execute the embedded query against the RDF store.  Return
        an iterator that produces for each step at uple of values
        (resources and literals) corresponding to the variables
        or expressions in a 'select' clause (or its equivalent).
        """
        BEHAVIOR = "default-dataset-behavior";
        ## before executing, see if there is a dataset that needs to be incorporated into the
        ## query
        query = self.queryString
        if self.getDataset():
            query = self.spliceDatasetIntoQuery(query, self.getDataset())
            onlyNullContext = False
            for defaultGraph in self.getDataset().getDefaultGraphs():
                if defaultGraph is None:
                    ## when the null context is combined with other contexts
                    ## I have no idea what might happen; probably the null context
                    ## is dropped on the floor   RMM
                    onlyNullContext = True
        if self.connection:
            query = self.splicePrefixesIntoQuery(query, self.connection)
        options = [BEHAVIOR, 'default'] if onlyNullContext else []
        bindingsIt = self.direct_caller.twinqlSelect(query, None, 0, 0, self.includeInferred, options)
        return bindingsIt
    
    def spliceDatasetIntoQuery(self, query, dataset):
        """
        If 'query' has a dataset, splice its declarations into the query.
        """
        substituteFroms = dataset.asQuery(True)
        if not substituteFroms: return query
        if not self.queryLanguage == QueryLanguage.SPARQL: return query
        lcQuery = query.lower()
        fromPos = lcQuery.find('from')
        wherePos = lcQuery.find('where')
        if wherePos < 0:
            wherePos = len(query)        
        if fromPos < 0:
            fromPos = wherePos
        splicedQuery = ''.join([query[:fromPos], substituteFroms, query[wherePos:]])
        return splicedQuery

    def splicePrefixesIntoQuery(self, query, connection):
        """
        Add build-in and registered prefixes to 'query' when needed.
        """
        lcQuery = query.lower()
        referenced = []
        for ns in connection.getNamespaces():
            if lcQuery.find(ns.getPrefix()) >= 0 and lcQuery.find("prefix %s" % ns.getPrefix) < 0:
                referenced.append(ns)
        for ref in referenced:
            query = "PREFIX %s: <%s> %s" % (ref.getPrefix(), ref.getName(), query)
        return query

