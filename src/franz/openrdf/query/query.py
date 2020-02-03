#!/usr/bin/env python
# -*- coding: utf-8 -*-
# pylint: disable-msg=C0103

################################################################################
# Copyright (c) 2006-2017 Franz Inc.  
# All rights reserved. This program and the accompanying materials are
# made available under the terms of the MIT License which accompanies
# this distribution, and is available at http://opensource.org/licenses/MIT
################################################################################

from __future__ import absolute_import
from __future__ import unicode_literals
from future.builtins import object
from past.builtins import basestring

from future.utils import iteritems, python_2_unicode_compatible

from franz.openrdf.rio.rdfformat import RDFFormat
from franz.openrdf.rio.tupleformat import TupleFormat
from franz.openrdf.util.contexts import output_to
from ..exceptions import IllegalOptionException, QueryMissingFeatureException
from .dataset import ALL_CONTEXTS, Dataset
from .queryresult import GraphQueryResult, TupleQueryResult


@python_2_unicode_compatible
class QueryLanguage(object):
    registered_languages = []
    SPARQL = None
    PROLOG = None

    def __init__(self, name):
        self.name = name
        QueryLanguage.registered_languages.append(self)

    def __str__(self):
        return self.name

    def __repr__(self):
        return 'QueryLanguage.' + self.name

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
    A query on a repository that can be formulated in one of the
    supported query languages (for example SeRQL or SPARQL). It allows one to
    predefine bindings in the query to be able to reuse the same query with
    different bindings.
    """
    def __init__(self, queryLanguage=QueryLanguage.SPARQL,
                 query=None, baseURI=None, queryString=None):
        """
        Initialize a query object.

        :param queryLanguage: Query syntax - the default is SPARQL.
        :type queryLanguage: QueryLanguage
        :param query: Query text
        :type query: string
        :param baseURI: Prefix used to relsove relative URIs.
                        Default: chosen by the server.
        :type baseURI: URI
        :param queryString: Legacy name of the 'query' parameter.
        :type queryString: string
        """
        if query is None:
            raise TypeError('query argument is missing.')
        self.queryLanguage = Query._check_language(queryLanguage)
        self.queryString = query or queryString
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

    def setBinding(self, name, value):
        """
        Binds the specified variable to the supplied value. Any value that was
        previously bound to the specified value will be overwritten.

        :param name: Variable name.
        :type name: string
        :param value: New value for the variable. If a string is passed it will
                      be converted to a literal.
        :type value: Value|basestring
        """
        if isinstance(value, basestring):
            value = self._get_connection().createLiteral(value)
        self.bindings[name] = value
        
    def setBindings(self, dict):
        """
        Set multiple variable bindings.

        :param dict: A dictionary of bindings.
        :type dict: dict[string, Value]
        """
        if not dict: return
        for key, value in iteritems(dict):
            self.setBinding(key, value)

    def removeBinding(self, name):
        """ 
        Removes a previously set binding on the supplied variable. Calling this
        method with an unbound variable name has no effect.

        :param name: Variable name.
        :type name: string
        """
        self.bindings.pop(name)

    def getBindings(self):
        """
        Retrieve the bindings that have been set on this query.

        :return: A dictionary of bindings.
        :rtype: dict[string, Value]
        """ 
        return self.bindings

    def setDataset(self, dataset):
        """
        Select the dataset against which to evaluate this query, overriding any
        dataset that is specified in the query itself.

        :param dataset: A dataset object.
        :type dataset: Dataset
        """ 
        self.dataset = dataset
     
    def getDataset(self):
        """
        Get the dataset against which this query will operate.

        :return: Dataset.
        :rtype: Dataset
        """ 
        return self.dataset
    
    def setContexts(self, contexts):
        """
        Assert a set of contexts (named graphs) that filter all triples.

        :param contexts: List of graph URIs.
        :type contexts: list[URI|string]
        """
        if not contexts: return
        ds = Dataset()
        for cxt in contexts:
            if isinstance(cxt, basestring): cxt = self._get_connection().createURI(cxt)
            ds.addNamedGraph(cxt)
        self.setDataset(ds)
     
    def setIncludeInferred(self, includeInferred):
        """
        Determine whether evaluation results of this query should include inferred
        statements.

        The default setting is ``True``.

        :param includeInferred: If ``True`` include inferred triples in query results.
        :type includeInferred: bool
        """
        self.includeInferred = includeInferred

    def getIncludeInferred(self):
        """
        Check whether this query will return inferred statements.
        :return: ``True`` if inferred triples are included, ``False`` otherwise.
        :rtype: bool
        """ 
        return self.includeInferred
    
    def setCheckVariables(self, setting):
        """
        Determine whether the presence of variables in the select clause not referenced
        in a triple is flagged.

        :param setting: ``True`` if variables should be checked, ``False`` otherwise.
        :type setting: bool
        """
        self.checkVariables = setting
    
    def setConnection(self, connection):
        """
        Internal call to embed the connection into the query.
        """
        self.connection = connection
        
    def _get_connection(self):
        return self.connection
    
    def evaluate_generic_query(self, count=False, accept=None, callback=None,
                               analyze=False, analysisTechnique=None, analysisTimeout=None,
                               update=False):
        """
        Evaluate a SPARQL or PROLOG query, which may be a 'select', 'construct', 'describe'
        or 'ask' query (in the SPARQL case). Return an appropriate response.

        :param count: If ``True`` return only the count of the query results.
        :type count: bool
        :param accept: Set to ``'application/sparql-results+xml'`` or ``'application/sparql-results+json'``
                       to retrieve the result as a string in the specified format.
                       The default is to return a :class:`QueryResult` object.
        :type accept: string
        :param analyze: If ``True`` perform query analysis for SPARQL queries.
        :type analyze: bool
        :param analysisTechnique: Control the method of analysis. Can be either `"executed"` (default).
                                  meaning that the query will be executed to perform dynamic analysis,
                                  or ``"static"``.
        :type analysisTechnique: string
        :param analysisTimeout: Number of second to run the query if executed for analysis.
        :type analysisTimeout: float
        :param update: If ``True`` this is an update query and the result will be a boolean.
        :type update: bool
        :return: The result can be either:
                    - A boolean (for update queries).
                    - A dictionary containing the results.
                    - A string (if ``accept`` was passed).
                    - An integer (if ``count=True``).
        :rtype: dict|string|int|bool
        """
        conn = self._get_connection()
        namedContexts = conn._contexts_to_ntriple_contexts(
                        self.dataset.getNamedGraphs() if self.dataset else None)
        regularContexts = conn._contexts_to_ntriple_contexts(
                self.dataset.getDefaultGraphs() if self.dataset else ALL_CONTEXTS)
        bindings = None
        if self.bindings:
            bindings = {}
            for vbl, val in list(self.bindings.items()):
                bindings[vbl] = conn._convert_term_to_mini_term(val)
        mini = conn._get_mini_repository()
        if self.queryLanguage == QueryLanguage.SPARQL:  
            response = mini.evalSparqlQuery(self.queryString, context=regularContexts, namedContext=namedContexts, 
                                            infer=self.includeInferred, bindings=bindings,
                                            checkVariables=self.checkVariables, count=count,
                                            accept=accept, callback=callback,
                                            analyze=analyze, analysisTechnique=analysisTechnique,
                                            analysisTimeout=analysisTimeout, update=update)
        elif self.queryLanguage == QueryLanguage.PROLOG:
            if namedContexts:
                raise QueryMissingFeatureException("Prolog queries do not support the datasets (named graphs) option.")
            if analyze:
                raise QueryMissingFeatureException("Prolog queries do not support analysis.")
            # evalPrologQuery is already always done as if update=True
            response = mini.evalPrologQuery(self.queryString,
                                            infer=self.includeInferred, count=count, accept=accept,
                                            callback=callback)
        else:
            raise ValueError('Unsupported query language: %s' % self.queryLanguage)
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
    """
    A query that returns tuples (i.e. sets of variable bindings).
    """
    def evaluate(self, count=False, output=None,
                 output_format=TupleFormat.TABLE):
        """Execute the embedded query against the RDF store.

        Return an iterator that produces for each step a tuple of values
        (resources and literals) corresponding to the variables or
        expressions in a 'select' clause (or its equivalent).

        :param count: If ``True`` return the number of result rows
                      instead of the usual iterator.
        :type count: bool
        :param output: A file name, file descriptor or a file-like
                       object to save the results to
                       (optional). ``True`` can be used as a synonym
                       for stdout.
        :type output: str|file|int|bool
        :param output_format: Serialization format for ``output``.
                              The default is TABLE.
        :type output_format: RDFFormat
        :return: Either an iterator over results, 
                 an integer (the number of results, if ``count`` is used) 
                 or ``None`` (if ``output`` is used).
        :rtype: TupleQueryResult|int
        """

        with output_to(output) as out:
            callback = None if output is None else out.write
            accept = None if output is None else TupleFormat.mime_type_for_format(output_format)
            response = self.evaluate_generic_query(
                count=count, accept=accept, callback=callback)

            if count:
                return response

            if output is not None:
                return None

            return TupleQueryResult(response['names'], response['values'], response.get('queryInfo'))

    def analyze(self, analysisTechnique=None, analysisTimeout=None):
        """
        Analyze the query.

        Analysis is only available for SPARQL queries.

        :param analysisTechnique: Control the method of analysis. Can be either `"executed"` (default).
                                  meaning that the query will be executed to perform dynamic analysis,
                                  or ``"static"``.
        :type analysisTechnique: string
        :param analysisTimeout: Number of second to run the query if executed for analysis.
        :type analysisTimeout: float
        :return: Analysis result as a dictionary.
        :rtype: dict
        """
        response = self.evaluate_generic_query(analyze=True, analysisTechnique=analysisTechnique,
            analysisTimeout=analysisTimeout)
        return response


class UpdateQuery(Query):
    """
    An update query.
    """
    def evaluate(self):
        """
        Execute the embedded update against the RDF store.

        :return: ``True`` if the query changed the store, ``False`` otherwise.
        :rtype: bool
        """
        return self.evaluate_generic_query(update=True)

class GraphQuery(Query):
    """
    A query that returns statements (see :class:`Statement`).
    """
    def evaluate(self, output=None, output_format=RDFFormat.TABLE):
        """
        Execute the embedded query against the RDF store.

        :param output: A file name, file descriptor or a file-like
                       object to save the results to
                       (optional). ``True`` can be used as a synonym
                       for stdout.
        :type output: str|file|int|bool
        :param output_format: Serialization format for ``output``.
                              The default is TABLE.
        :type output_format: RDFFormat
        :return: An iterator over statements or None (if ``output`` is used)..
        :rtype: GraphQueryResult
        """
        with output_to(output) as out:
            callback = None if output is None else out.write
            accept = None if output is None else RDFFormat.mime_type_for_format(output_format)
            response = self.evaluate_generic_query(accept=accept, callback=callback)
            if output is None:
                return GraphQueryResult(response)
            else:
                return None


class BooleanQuery(Query):
    """
    A query that returns a boolean.
    """
    def evaluate(self):
        """
        Execute the embedded query against the RDF store.

        :return: A boolean.
        :rtype: bool
        """
        return self.evaluate_generic_query()
