#!/usr/bin/env python
# -*- coding: utf-8 -*-
# pylint: disable-msg=C0103

###############################################################################
# Copyright (c) 2006-2009 Franz Inc.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the Eclipse Public License v1.0
# which accompanies this distribution, and is available at
# http://www.eclipse.org/legal/epl-v10.html
###############################################################################

from __future__ import absolute_import

from ..exceptions import IllegalOptionException, QueryMissingFeatureException
from .dataset import ALL_CONTEXTS, Dataset
from .queryresult import GraphQueryResult, TupleQueryResult
import datetime

class QueryLanguage:
    registered_languages = []
    SPARQL = None
    PROLOG = None

    def __init__(self, name):
        self.name = name
        QueryLanguage.registered_languages.append(self)

    def __str__(self):
        return self.name
    
    def getName(self):
        return self.name

    @staticmethod
    def values(): return QueryLanguage.registered_languages[:]
    
    @staticmethod
    def valueOf(name): 
        for ql in QueryLanguage.registered_languages:
            if ql.name.lower() == name.lower():
                return ql
        return None
    
QueryLanguage.SPARQL = QueryLanguage('SPARQL')
QueryLanguage.PROLOG = QueryLanguage('PROLOG')

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
    def __init__(self, queryLanguage, queryString, baseURI=None):
        self.queryLanguage = Query._check_language(queryLanguage)
        self.queryString = queryString
        self.baseURI = baseURI
        self.dataset = None
        self.includeInferred = False
        self.bindings = {}
        self.connection = None
        self.checkVariables = False
        ## CommonLogic parameters:
        self.preferred_execution_language = None
        self.actual_execution_language = None
        self.subject_comes_first = False

    @staticmethod
    def set_trace_query(setting):
        global TRACE_QUERY
        TRACE_QUERY = setting

    def setBinding(self, name, value):
        """
        Binds the specified variable to the supplied value. Any value that was
        previously bound to the specified value will be overwritten.
        """
        if isinstance(value, str):
            value = self._get_connection().createLiteral(value)
        self.bindings[name] = value
        
    def setBindings(self, dict):
        if not dict: return
        for key, value in dict.iteritems():
            self.setBinding(key, value)

    def removeBinding(self, name):
        """ 
        Removes a previously set binding on the supplied variable. Calling this
        method with an unbound variable name has no effect.
        """
        self.bindings.popitem(name, None)

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
        Gets the dataset that has been set.
        """ 
        return self.dataset
    
    def setContexts(self, contexts):
        """
        Assert a set of contexts (named graphs) that filter all triples.
        """
        if not contexts: return
        ds = Dataset()
        for cxt in contexts:
            if isinstance(cxt, str): cxt = self._get_connection().createURI(cxt)
            ds.addNamedGraph(cxt)
        self.setDataset(ds)
     
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
    
    def setCheckVariables(self, setting):
        """
        If true, the presence of variables in the select clause not referenced in a triple
        are flagged.
        """
        self.checkVariables = setting
    
    def setConnection(self, connection):
        """
        Internal call to embed the connection into the query.
        """
        self.connection = connection
        
    def _get_connection(self):
        return self.connection
    
    def evaluate_generic_query(self, count=False, accept=None, analyze=False, analysisTechnique=None, analysisTimeout=None):
        """
        Evaluate a SPARQL or PROLOG query, which may be a 'select', 'construct', 'describe'
        or 'ask' query (in the SPARQL case).  Return an appropriate response.

        If analysis is True it will perform query analysis for SPARQL queries.
        analysisTechnique defaults to "executed", which executes the query to perform dynamic analysis.
            "static" analysis is the other option.
        For analysisTimeout, pass a float of the number of seconds to run the query if executed.
        """
        ##if self.dataset and self.dataset.getDefaultGraphs() and not self.dataset.getDefaultGraphs() == ALL_CONTEXTS:
        ##    raise UnimplementedMethodException("Query datasets not yet implemented for default graphs.")
        conn = self._get_connection()
        namedContexts = conn._contexts_to_ntriple_contexts(
                        self.dataset.getNamedGraphs() if self.dataset else None)
        regularContexts = conn._contexts_to_ntriple_contexts(
                self.dataset.getDefaultGraphs() if self.dataset else ALL_CONTEXTS)
        bindings = None
        if self.bindings:
            bindings = {}
            for vbl, val in self.bindings.items():
                bindings[vbl] = conn._convert_term_to_mini_term(val)
        mini = conn._get_mini_repository()
        if self.queryLanguage == QueryLanguage.SPARQL:  
            response = mini.evalSparqlQuery(self.queryString, context=regularContexts, namedContext=namedContexts, 
                                            infer=self.includeInferred, bindings=bindings,
                                            checkVariables=self.checkVariables, count=count, accept=accept, analyze=analyze,
                                            analysisTechnique=analysisTechnique, analysisTimeout=analysisTimeout)
        elif self.queryLanguage == QueryLanguage.PROLOG:
            if namedContexts:
                raise QueryMissingFeatureException("Prolog queries do not support the datasets (named graphs) option.")
            if analyze:
                raise QueryMissingFeatureException("Prolog queries do not support analysis.")
            response = mini.evalPrologQuery(self.queryString, infer=self.includeInferred, count=count, accept=accept)
        return response

    @staticmethod
    def _check_language(queryLanguage):
        if queryLanguage == 'SPARQL':
            return QueryLanguage.SPARQL

        if queryLanguage == 'PROLOG':
            return QueryLanguage.PROLOG

        if not queryLanguage in [QueryLanguage.SPARQL, QueryLanguage.PROLOG]:
            raise IllegalOptionException("Can't evaluate the query language '%s'.  Options are: SPARQL and PROLOG."
                                         % queryLanguage)
        return queryLanguage
       
  
class TupleQuery(Query):
    def evaluate(self, count=False):
        """
        Execute the embedded query against the RDF store.  Return
        an iterator that produces for each step a tuple of values
        (resources and literals) corresponding to the variables
        or expressions in a 'select' clause (or its equivalent).
        """
        response = self.evaluate_generic_query(count=count)

        if count:
            return response
        
        return TupleQueryResult(response['names'], response['values'])

    def analyze(self, analysisTechnique=None, analysisTimeout=None):
        """
        Analysis is only available for SPARQL queries.

        analysisTechnique defaults to the string "executed", which executes the query to perform dynamic analysis.
            "static" analysis is the other option.
        For analysisTimeout, pass a float of the number of seconds to run the query if executed.
        """
        response = self.evaluate_generic_query(analyze=True, analysisTechnique=analysisTechnique,
            analysisTimeout=analysisTimeout)
        return response

class GraphQuery(Query):
    
    def evaluate(self):
        """
        Execute the embedded query against the RDF store.  Return
        a graph.
        """
        response = self.evaluate_generic_query()
        return GraphQueryResult(response)

class BooleanQuery(Query):
    
    def evaluate(self):
        """
        Execute the embedded query against the RDF store.  Return
        True or False
        """
        return self.evaluate_generic_query()


