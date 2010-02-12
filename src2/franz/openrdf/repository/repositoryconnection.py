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
from __future__ import with_statement

from .repositoryresult import RepositoryResult

from ..exceptions import IllegalOptionException, IllegalArgumentException
from ..model import Statement, Value
from ..model.literal import RangeLiteral, GeoCoordinate, GeoSpatialRegion, GeoBox, GeoCircle, GeoPolygon
from ..query.dataset import ALL_CONTEXTS, MINI_NULL_CONTEXT
from ..query.query import Query, TupleQuery, GraphQuery, BooleanQuery, QueryLanguage
from ..rio.rdfformat import RDFFormat
from ..util import uris
from ..vocabulary import RDF, RDFS, OWL, XMLSchema

import copy, datetime, os
from contextlib import contextmanager

# * Main interface for updating data in and performing queries on a Sesame
# * repository. By default, a RepositoryConnection is in autoCommit mode, meaning
# * that each operation corresponds to a single transaction on the underlying
# * store. autoCommit can be switched off in which case it is up to the user to
# * handle transaction commit/rollback. Note that care should be taking to always
# * properly close a RepositoryConnection after one is finished with it, to free
# * up resources and avoid unnecessary locks.
# * <p>
# * Several methods take a vararg argument that optionally specifies a (set of)
# * context(s) on which the method should operate. Note that a vararg parameter
# * is optional, it can be completely left out of the method call, in which case
# * a method either operates on a provided statements context (if one of the
# * method parameters is a statement or collection of statements), or operates on
# * the repository as a whole, completely ignoring context. A vararg argument may
# * also be 'null' (cast to Resource) meaning that the method operates on those
# * statements which have no associated context only.
# * <p>
# * Examples:
# * 
# * <pre>
# * // Ex 1: this method retrieves all statements that appear in either context1 or context2, or both.
# * RepositoryConnection.getStatements(null, null, null, true, context1, context2);
# * 
# * // Ex 2: this method retrieves all statements that appear in the repository (regardless of context).
# * RepositoryConnection.getStatements(null, null, null, true);
# * 
# * // Ex 3: this method retrieves all statements that have no associated context in the repository.
# * // Observe that this is not equivalent to the previous method call.
# * RepositoryConnection.getStatements(null, null, null, true, (Resource)null);
# * 
# * // Ex 4: this method adds a statement to the store. If the statement object itself has 
# * // a context (i.e. statement.getContext() != null) the statement is added to that context. Otherwise,
# * // it is added without any associated context.
# * RepositoryConnection.add(statement);
# * 
# * // Ex 5: this method adds a statement to context1 in the store. It completely ignores any
# * // context the statement itself has.
# * RepositoryConnection.add(statement, context1);

class RepositoryConnection(object):
    def __init__(self, repository):
        self.repository = repository 
        self.mini_repository = repository.mini_repository
        self.is_closed = False
        self.ruleLanguage = None
        self._add_commit_size = None
        self.setNamespace("fti", "http://franz.com/ns/allegrograph/2.2/textindex/")
        
    def _get_mini_repository(self):
        return self.mini_repository
        
    def getValueFactory(self):
        return self.repository.getValueFactory()
        
    def close(self):
        self.is_closed = True
    
    def setAddCommitSize(self, triple_count):
        if not triple_count or triple_count < 0:
            self._add_commit_size = None
        else:
            self._add_commit_size = int(triple_count)

    def getAddCommitSize(self):
        return self._add_commit_size

    add_commit_size = property(getAddCommitSize, setAddCommitSize,
        "The threshold for commit size during triple add operations.\n"
        "Set to 0 (zero) or None to clear size-based autocommit behavior.\n"
        "When set to an integer triple_count > 0, loads and adds commit each\n"
        "triple_count triples added and at the end of the triples being added.\n")

    def prepareQuery(self, queryLanguage, queryString, baseURI=None):
        """
        Embed 'queryString' into a query object which can be
        executed against the RDF storage.
        """
        query = Query(queryLanguage, queryString, baseURI)
        query.setConnection(self)
        return query

    def prepareTupleQuery(self, queryLanguage, queryString, baseURI=None):
        """
        Embed 'queryString' into a query object which can be
        executed against the RDF storage.  'queryString' must be a SELECT
        query.  The result of query execution is an iterator of tuples.
        """
        query = TupleQuery(queryLanguage, queryString, baseURI=baseURI)
        query.setConnection(self)
        return query

    def prepareGraphQuery(self, queryLanguage, queryString, baseURI=None):
        """
        Parse 'queryString' into a query object which can be
        executed against the RDF storage.  'queryString' must be a CONSTRUCT
        or DESCRIBE query.  The result of query execution is an iterator of
        statements/quads.
        """
        query = GraphQuery(queryLanguage, queryString, baseURI=baseURI)
        query.setConnection(self)        
        return query

    def prepareBooleanQuery(self, queryLanguage, queryString, baseURI=None):
        """
        Parse 'queryString' into a query object which can be
        executed against the RDF storage.  'queryString' must be an ASK
        query.  The result is true or false.
        """
        query = BooleanQuery(queryLanguage, queryString, baseURI=baseURI)
        query.setConnection(self)
        return query
    
    def getContextIDs(self):
        """
        Return a list of context resources, one for each context referenced by a quad in 
        the triple store.  Omit the default context, since no one had the intelligence to
        make it a first-class object.
        """                         
        contexts = []
        for cxt in self._get_mini_repository().listContexts():
            contexts.append(self.createURI(cxt))
        return contexts


#     * Returns the number of (explicit) statements that are in the specified
#     * contexts in this repository.
#     * 
#     * @param contexts
#     *        The context(s) to get the data from. Note that this parameter is a
#     *        vararg and as such is optional. If no contexts are supplied the
#     *        method operates on the entire repository.
#     * @return The number of explicit statements from the specified contexts in
#     *         this repository.
    def size(self, contexts=ALL_CONTEXTS):
        """
        Returns the number of (explicit) statements that are in the specified
        contexts in this repository.
        """
        cxts = self._contexts_to_ntriple_contexts(contexts, False)
        if cxts == ALL_CONTEXTS or not cxts:
            return self._get_mini_repository().getSize()
        elif len(cxts) == 1:
            return self._get_mini_repository().getSize(cxts[0])
        else:
            total = 0
            for cxt in cxts:
                total += self._get_mini_repository().getSize(cxt)
            return total

#     * Returns <tt>true</tt> if this repository does not contain any (explicit)
#     * statements.
#     * 
#     * @return <tt>true</tt> if this repository is empty, <tt>false</tt>
#     *         otherwise.
#     * @throws RepositoryException
#     *         If the repository could not be checked to be empty.
    def isEmpty(self):
        return self.size() == 0
    
    def _context_to_ntriples(self, context, none_is_mini_null=False):
        if context is None:
            return MINI_NULL_CONTEXT if none_is_mini_null else None

        if context == MINI_NULL_CONTEXT:
            return MINI_NULL_CONTEXT

        if context == 'null':
            return MINI_NULL_CONTEXT

        if context:
            return context if isinstance(context, basestring) else context.toNTriples()

        if none_is_mini_null:
            return MINI_NULL_CONTEXT

        return None            
       
    def _contexts_to_ntriple_contexts(self, contexts, none_is_mini_null=False):
        """
        Do three transformations here.  Convert from context object(s) to
        context strings (angle brackets).
        Also, convert singleton context to list of contexts, and convert
        ALL_CONTEXTS to None.
        And, convert None context to 'null'.
        """
        if contexts == ALL_CONTEXTS:  ## or contexts is None:
            ## consistency would dictate that  None => [None], but this would
            ## likely surprise users, so we don't do that:
            cxts = None
        elif contexts is None:
            if none_is_mini_null: cxts = [MINI_NULL_CONTEXT]
            else: cxts = None
        elif contexts == 'null':
            cxts = [MINI_NULL_CONTEXT]
        elif isinstance(contexts, (list, tuple)):
            cxts = [self._context_to_ntriples(c, none_is_mini_null=True) for c in contexts]
        else:
            cxts = [self._context_to_ntriples(contexts, none_is_mini_null=True)]
        return cxts

    def _convert_term_to_mini_term(self, term, predicate_for_object=None):
        """
        If 'term' is a Value, convert it to an ntriples string.  If its a Python
        term, do likewise
        If 'term' is a CompoundLiteral or a list or tuple, separate out the second
        value, ntriplize it, and return a binary tuple.
        TODO: FIGURE OUT HOW COORDINATE PAIRS WILL WORK HERE
        """ 
        if isinstance(term, GeoSpatialRegion): return term
        factory = self.getValueFactory()
        if isinstance(term, GeoCoordinate):
            geoType = term.geoType
            miniGeoType = geoType._getMiniGeoType()
            if geoType.system == GeoType.Cartesian:
                return self._get_mini_repository().createCartesianGeoLiteral(miniGeoType, term.xcoor, term.ycoor)
            elif geoType.system == GeoType.Spherical:
                unit = term.unit or term.geoType.unit
                return self._get_mini_repository().createSphericalGeoLiteral(miniGeoType, term.xcoor, term.ycoor, unit=unit)
            else:
                raise IllegalOptionException("Unsupported geo coordinate system", geoType.system)
        if isinstance(term, RangeLiteral):
            beginTerm = term.getLowerBound()
            endTerm = term.getUpperBound()
            return (self._to_ntriples(beginTerm), self._to_ntriples(endTerm))
        elif isinstance(term, (tuple, list)):
            return [self._convert_term_to_mini_term(t, predicate_for_object=predicate_for_object) for t in term]
        ## OBSOLETE: CONVERT LIST TO RANGE LITERAL:
        elif isinstance(term, (tuple, list)):
            factory = self.getValueFactory()
            beginTerm = factory.object_position_term_to_openrdf_term(term[0])
            factory.validateRangeConstant(beginTerm, predicate_for_object)
            endTerm = factory.object_position_term_to_openrdf_term(term[1])
            factory.validateRangeConstant(endTerm, predicate_for_object)            
            return (self._to_ntriples(beginTerm), self._to_ntriples(endTerm))
        ## END OBSOLETE
        elif predicate_for_object:
            term = factory.object_position_term_to_openrdf_term(term, predicate=predicate_for_object)
            return self._to_ntriples(term)
        else:
            return self._to_ntriples(term)
    
    def getStatements(self, subject, predicate,  object, contexts=ALL_CONTEXTS, includeInferred=False,
                       limit=None, tripleIDs=False):
        """
        Gets all statements with a specific subject, predicate and/or object from
        the repository. The result is optionally restricted to the specified set
        of named contexts.  Returns a RepositoryResult that produces a 'Statement'
        each time that 'next' is called.
        """
        subj = self._convert_term_to_mini_term(subject)
        pred = self._convert_term_to_mini_term(predicate)
        obj = self._convert_term_to_mini_term(object, predicate)
        cxt = self._contexts_to_ntriple_contexts(contexts)
        if isinstance(object, GeoSpatialRegion):
            return self._getStatementsInRegion(subj, pred, obj, cxt, limit=limit)
        else:
            stringTuples = self._get_mini_repository().getStatements(subj, pred, obj, cxt,
                 infer=includeInferred, limit=limit, tripleIDs=tripleIDs)
            return RepositoryResult(stringTuples, tripleIDs=tripleIDs)

    def getStatementsById(self, ids):
        """
        Return all statements whose triple ID matches an ID in the list 'ids'.
        """
        stringTuples = self._get_mini_repository().getStatementsById(ids)
        return RepositoryResult(stringTuples, tripleIDs=False)
    
    def _getStatementsInRegion(self, subject, predicate,  region, contexts, limit=None):
        geoType = region.geoType
        miniGeoType = geoType._getMiniGeoType()
        if isinstance(region, GeoBox):
            if geoType.system == GeoType.Cartesian:
                stringTuples = self._get_mini_repository().getStatementsInsideBox(miniGeoType, predicate,
                                        region.xMin, region.xMax, region.yMin, region.yMax,
                                        limit=limit)
            elif geoType.system == GeoType.Spherical:
                stringTuples = self._get_mini_repository().getStatementsInsideBox(miniGeoType, predicate,
                                        region.yMin, region.yMax, region.xMin, region.xMax,
                                        limit=limit)
        elif isinstance(region, GeoCircle):
            if geoType.system == GeoType.Cartesian:
                stringTuples = self._get_mini_repository().getStatementsInsideCircle(miniGeoType, predicate,
                                        region.x, region.y, region.radius, limit=limit)
            elif geoType.system == GeoType.Spherical:
                stringTuples = self._get_mini_repository().getStatementsHaversine(miniGeoType, predicate,
                                        region.x, region.y, region.radius, unit=region.unit,
                                        limit=limit)
            else: pass ## can't happen
        elif isinstance(region, GeoPolygon):
            stringTuples = self._get_mini_repository().getStatementsInsidePolygon(miniGeoType, predicate,
                                        self._convert_term_to_mini_term(region.getResource()),
                                        limit=limit)
        else: pass ## can't happen
        return RepositoryResult(stringTuples, subjectFilter=subject)            
    
    def getJDBCStatements(self, subject, predicate,  object, contexts=ALL_CONTEXTS, includeInferred=False, 
                          limit=None, tripleIDs=False):        
        """
        Gets all statements with a specific subject, predicate and/or object from
        the repository. The result is optionally restricted to the specified set
        of named contexts.  Returns a JDBCResultSet that enables Values, strings, etc.
        to be selectively extracted from the result, without the bulky overhead
        of the OpenRDF BindingSet protocol.
        """
        object = self.repository.getValueFactory().object_position_term_to_openrdf_term(object, predicate=predicate)
        stringTuples = self._get_mini_repository().getStatements(self._to_ntriples(subject), self._to_ntriples(predicate),
                 self._to_ntriples(object), self._contexts_to_ntriple_contexts(contexts), infer=includeInferred, 
                 limit=limit, tripleIDs=tripleIDs)
        return JDBCStatementResultSet(stringTuples, triple_ids=tripleIDs)

    def add(self, arg0, arg1=None, arg2=None, contexts=None, base=None, format=None, serverSide=False):
        """
        Calls addTriple, addStatement, or addFile.  If 'contexts' is not
        specified, adds to the null context.
        """
        if contexts and not isinstance(contexts, list):
            contexts = [contexts]
        if isinstance(arg0, (str, file)):
            if contexts:
                if len(contexts) > 1:
                    raise IllegalArgumentException("Only one context may be specified when loading from a file.")
                context = contexts[0]
            else:
                context = None
            return self.addFile(arg0, base=base, format=format, context=context, serverSide=serverSide)
        elif isinstance(arg0, Value):
            return self.addTriple(arg0, arg1, arg2, contexts=contexts)
        elif isinstance(arg0, Statement):
            return self.addStatement(arg0, contexts=contexts)
        elif hasattr(arg0, '__iter__'):
            for s in arg0:
                self.addStatement(s, contexts=contexts)
        else:
            raise IllegalArgumentException("Illegal first argument to 'add'.  Expected a Value, Statement, File, or string.")
            
    def addFile(self, filePath, base=None, format=None, context=None, serverSide=False):
        """
        Load the file or file path 'filePath' into the store.  'base' optionally defines a base URI,
        'format' is RDFFormat.NTRIPLES or RDFFormat.RDFXML, and 'context' optionally specifies
        which context the triples will be loaded into.
        """
        if isinstance(context, (list, tuple)):
            if len(context) > 1:
                raise IllegalArgumentException("Multiple contexts passed to 'addFile': %s" % context)
            context = context[0] if context else None
        contextString = self._context_to_ntriples(context, none_is_mini_null=True)

        if isinstance(filePath, file):
            filePath = os.path.abspath(filePath.name)
        elif isinstance(filePath, basestring):
            fileDrive = os.path.splitdrive(filePath)[0]
            if not filePath.startswith('/') and not fileDrive and not filePath[:5].lower() == "http:":
                ## looks like its a relative file path; test to see if there is a local file that matches.
                ## If so, generate an absolute path name to enable AG server to read it:
                testPath = os.path.abspath(os.path.expanduser(filePath))
                if os.path.exists(testPath):
                    filePath = testPath
        fileExt = os.path.splitext(filePath)[1].lower()
        if format == RDFFormat.NTRIPLES or fileExt in ['.nt', '.ntriples']:
            self._get_mini_repository().loadFile(filePath, 'ntriples', context=contextString, serverSide=serverSide,
                commitEvery=self.add_commit_size)
        elif format == RDFFormat.RDFXML or fileExt in ['.rdf', '.owl']:
            self._get_mini_repository().loadFile(filePath, 'rdf/xml', context=contextString, baseURI=base,
                serverSide=serverSide, commitEvery=self.add_commit_size)
        else:
            raise Exception("Failed to specify a format for the file '%s'." % filePath)
        
    def addTriple(self, subject, predicate, object, contexts=None):
        """
        Add the supplied triple of values to this repository, optionally to
        one or more named contexts.        
        """ 
        obj = self.getValueFactory().object_position_term_to_openrdf_term(object, predicate=predicate)
        cxts = self._contexts_to_ntriple_contexts(contexts, none_is_mini_null=True)
        for cxt in cxts:
            self._get_mini_repository().addStatement(self._to_ntriples(subject), self._to_ntriples(predicate),
                        self._convert_term_to_mini_term(obj), cxt)
        
    def _to_ntriples(self, term):
        """
        If 'term' is an OpenRDF term, convert it to a string.  If its already
        a string; assume its in ntriples format, and just pass it through.
        """
        if not term: return term
        elif isinstance(term, str): 
            return term
        else: return term.toNTriples();
        
    def addTriples(self, triples_or_quads, context=ALL_CONTEXTS, ntriples=False):
        """
        Add the supplied triples or quads to this repository.  Each triple can
        be a list or a tuple of Values.   If 'context' is set, then 
        the context is substituted in for each triple.  If 'ntriples' is True,
        then the triples or quads are assumed to contain valid ntriples strings,
        and they are passed to the server with no conversion.        
        """
        ntripleContexts = self._contexts_to_ntriple_contexts(context, none_is_mini_null=True)
        quads = []
        for q in triples_or_quads:
            isQuad = len(q) == 4
            quad = [None] * 4
            if ntriples:
                quad[0] = q[0]
                quad[1] = q[1]
                quad[2] = q[2]
                quad[3] = q[3] if isQuad and q[3] else ntripleContexts
            elif isinstance(quad, (list, tuple)):
                predicate = q[1]
                obj = self.getValueFactory().object_position_term_to_openrdf_term(q[2], predicate=predicate)
                quad[0] = self._to_ntriples(q[0])
                quad[1] = self._to_ntriples(predicate)
                quad[2] = self._to_ntriples(obj)
                quad[3] = self._to_ntriples(q[3]) if isQuad and q[3] else ntripleContexts
            else: # must be a statement
                predicate = q.getPredicate()
                obj = self.getValueFactory().object_position_term_to_openrdf_term(q.getObject(), predicate=predicate)
                quad[0] = self._to_ntriples(q.getSubject())
                quad[1] = self._to_ntriples(predicate)
                quad[2] = self._to_ntriples(obj)
                quad[3] = self._to_ntriples(q.getContext()) if isQuad and q.getContext() else ntripleContexts
            quads.append(quad)
        self._get_mini_repository().addStatements(quads, commitEvery=self.add_commit_size)
                
#     * Adds the supplied statement to this repository, optionally to one or more
#     * named contexts.
#     * 
#     * @param st
#     *        The statement to add.
#     * @param contexts
#     *        The contexts to add the statements to. Note that this parameter is
#     *        a vararg and as such is optional. If no contexts are specified, the
#     *        statement is added to any context specified in each statement, or
#     *        if the statement contains no context, it is added without context.
#     *        If one or more contexts are specified the statement is added to
#     *        these contexts, ignoring any context information in the statement
#     *        itself.
#     * @throws RepositoryException
#     *         If the statement could not be added to the repository, for example
#     *         because the repository is not writable.


    def addStatement(self, statement, contexts=None):
        """
        Add the supplied statement to the specified contexts in the repository.
        """        
        self.addTriple(statement.getSubject(), statement.getPredicate(), statement.getObject(),
                       contexts=contexts)      

    def remove(self, arg0, arg1=None, arg2=None, contexts=None):
        """
        Remove the supplied triple of values from this repository, optionally to
        one or more named contexts.
        """
        if contexts and not isinstance(contexts, list):
            contexts = [contexts]
        if isinstance(arg0, Value) or arg0 is None: self.removeTriples(arg0, arg1, arg2, contexts=contexts)
        elif isinstance(arg0, Statement): self.removeStatement(arg0, contexts=contexts)
        elif hasattr(arg0, '__iter__'):
            for s in arg0:
                self.removeStatement(s, contexts=contexts)
        else:
            raise IllegalArgumentException("Illegal first argument to 'remove'.  Expected a Value, Statement, or iterator.")

    def removeTriples(self, subject, predicate, object, contexts=ALL_CONTEXTS):
        """
        Removes the statement(s) with the specified subject, predicate and object
        from the repository, optionally restricted to the specified contexts.
        """
        subj = self._to_ntriples(subject)
        pred = self._to_ntriples(predicate)
        obj = self._to_ntriples(self.getValueFactory().object_position_term_to_openrdf_term(object))
        ntripleContexts = self._contexts_to_ntriple_contexts(contexts, none_is_mini_null=True)   
        if ntripleContexts is None or len(ntripleContexts) == 0:
            self._get_mini_repository().deleteMatchingStatements(subj, pred, obj, None)
        else:
            for cxt in ntripleContexts:
                self._get_mini_repository().deleteMatchingStatements(subj, pred, obj, cxt)

    def removeQuads(self, quads, ntriples=False):
        """
        Remove enumerated quads from this repository.  Each quad can
        be a list or a tuple of Values.   If 'ntriples' is True,
        then the  quads are assumed to contain valid ntriples strings,
        and they are passed to the server with no conversion.        
        """
        removeQuads = []
        for q in quads:
            quad = [None] * 4
            if ntriples:
                quad[0] = q[0]
                quad[1] = q[1]
                quad[2] = q[2]
                quad[3] = q[3]
            elif isinstance(quad, (list, tuple)):
                predicate = q[1]
                obj = self.getValueFactory().object_position_term_to_openrdf_term(q[2], predicate=predicate)
                quad[0] = self._to_ntriples(q[0])
                quad[1] = self._to_ntriples(predicate)
                quad[2] = self._to_ntriples(obj)
                quad[3] = self._to_ntriples(q[3])
            else: # must be a statement
                predicate = q.getPredicate()
                obj = self.getValueFactory().object_position_term_to_openrdf_term(q.getObject(), predicate=predicate)
                quad[0] = self._to_ntriples(q.getSubject())
                quad[1] = self._to_ntriples(predicate)
                quad[2] = self._to_ntriples(obj)
                quad[3] = self._to_ntriples(q.getContext())
            removeQuads.append(quad)
        self._get_mini_repository().deleteStatements(removeQuads)

    def removeQuadsByID(self, tids):
        """
        'tids' contains a list of triple/tuple IDs (integers).
        Remove all quads with matching IDs.
        """
        self._get_mini_repository().deleteStatementsById(tids)
   
#     * Removes the supplied statement from the specified contexts in the
#     * repository.
#     * @param st
#     *        The statement to remove.
#     * @param contexts
#     *        The context(s) to remove the data from. Note that this parameter is
#     *        is optional. If no contexts are supplied the
#     *        method operates on the contexts associated with the statement
#     *        itself, and if no context is associated with the statement, on the
#     *        entire repository.
#     * @throws RepositoryException
#     *         If the statement could not be removed from the repository, for
#     *         example because the repository is not writable.
    def removeStatement(self, statement, contexts=None):
        """
        Removes the supplied statement(s) from the specified contexts in the repository.
        """
        self.removeTriples(statement.getSubject(), statement.getPredicate(), statement.getContext(), contexts=contexts)

#     * Removes all statements from a specific contexts in the repository.
#     * 
#     * @param contexts
#     *        The context(s) to remove the data from. Note that this parameter is
#     *        a vararg and as such is optional. If no contexts are supplied the
#     *        method operates on the entire repository.
#     * @throws RepositoryException
#     *         If the statements could not be removed from the repository, for
#     *         example because the repository is not writable.
    def clear(self, contexts=ALL_CONTEXTS):
        """
        Removes all statements from designated contexts in the repository.  If
        'contexts' is ALL_CONTEXTS, clears the repository of all statements.
        """
        self.removeTriples(None, None, None, contexts=contexts)
         
    ## Exports all explicit statements in the specified contexts to the supplied
    ## RDFHandler.
    ## 
    ## @param contexts
    ##        The context(s) to get the data from. Note that this parameter is a
    ##        vararg and as such is optional. If no contexts are supplied the
    ##        method operates on the entire repository.
    ## @param handler
    ##        The handler that will handle the RDF data.
    def export(self, handler, contexts=ALL_CONTEXTS):
        self.exportStatements(None, None, None, False, handler, contexts=contexts)

    def exportStatements(self, subj, pred, obj, includeInferred, handler, contexts=ALL_CONTEXTS):
        """
        Exports all statements with a specific subject, predicate and/or object
        from the repository, optionally from the specified contexts.        
        """
        for prefix, name in self.getNamespaces().iteritems():
            handler.handleNamespace(prefix, name)
        statements = self.getStatements(subj, pred, obj, contexts, includeInferred=includeInferred)
        handler.export(statements)

    def getSubjectTriplesCacheSize(self):
        """
        Return the current size of the subject triples cache.
        """
        return self._get_mini_repository().getTripleCacheSize()

    def disableSubjectTriplesCache(self):
        """
        Disable the subject triples cache (see 'enableSubjectTriplesCache').
        """
        self._get_mini_repository().disableTripleCache()

    def enableSubjectTriplesCache(self, size=None):
        """
        Maintain a cache of size 'size' that caches, for each accessed
        resource, quads where the resource appears in subject position.
        This can accelerate the performance of certain types of queries.
        The size is the maximum number of subjects whose triples will be cached.
        Default is 100,000.
        """
        self._get_mini_repository().enableTripleCache(size=size)


    #############################################################################################
    ## ValueFactory methods
    ## Added here because its dumb to have to dispatch from three different objects
    ## (connection, repository, and value factory) when one will do
    #############################################################################################
    
    def registerFreeTextPredicate(self, uri=None, namespace=None, localname=None):
        return self.repository.registerFreeTextPredicate(uri=uri, namespace=namespace, localname=localname)
    
    def registerDatatypeMapping(self, predicate=None, datatype=None, nativeType=None):
        return self.repository.registerDatatypeMapping(predicate=predicate, datatype=datatype, nativeType=nativeType)


    def createLiteral(self, value, datatype=None, language=None):
        """
        Create a new literal with value 'value'.  'datatype' if supplied,
        should be a URI, in which case 'value' should be a string.
        """
        return self.getValueFactory().createLiteral(value, datatype=datatype, language=language)
    
    def createURI(self, uri=None, namespace=None, localname=None):
        """
        Creates a new URI from the supplied string-representation(s).
        If two non-keyword arguments are passed, assumes they represent a
        namespace/localname pair.
        """
        return self.getValueFactory().createURI(uri=uri, namespace=namespace, localname=localname)
    
    def createBNode(self, nodeID=None):
        return self.getValueFactory().createBNode(nodeID=nodeID)

    def createStatement(self, subject, predicate, object, context=None):
        """
        Create a new statement with the supplied subject, predicate and object
        and associated context.  Arguments have type Resource, URI, Value, and Resource.
        """
        return self.getValueFactory().createStatement(subject, predicate, object, context=context)
    
    def createRange(self, lowerBound, upperBound):
        return self.getValueFactory().createRange(lowerBound=lowerBound, upperBound=upperBound)

    def setRuleLanguage(self, queryLanguage):
        self.ruleLanguage = queryLanguage

    def addRules(self, rules, language=None):
        """
        Add a sequence of one or more rules (in ASCII format) to the current environment.
        If the language is Prolog, rule declarations start with '<-' or '<--'.  The 
        former appends a new rule; the latter overwrites any rule with the same predicate.

        For use with an open session.
        """
        language = language or self.ruleLanguage or QueryLanguage.PROLOG
        if language == QueryLanguage.PROLOG:
            self._get_mini_repository().definePrologFunctors(rules)
        else:
            raise Exception("Cannot add a rule because the rule language has not been set.")
        
    def loadRules(self, filename, language=None):
        """
        Load a file of rules into the current environment.
        'file' is assumed to reside on the client machine.
        If the language is Prolog, rule declarations start with '<-' or '<--'.  The 
        former appends a new rule; the latter overwrites any rule with the same predicate.

        For use with an open session.
        """
        with open(filename) as _file:
            body = _file.read()
        self.addRules(body, language)
        
    #############################################################################################
    ## Server-side implementation of namespaces
    #############################################################################################
      
    ## Get all declared prefix/namespace pairs
    def getNamespaces(self):
        namespaces = {}
        for pair in self._get_mini_repository().listNamespaces():
            namespaces[pair['prefix']] = pair['namespace']
        return namespaces        

    ## Gets the namespace that is associated with the specified prefix, if any.
    def getNamespace(self, prefix):
        return self.getNamespace(prefix)

    ## Sets the prefix for a namespace.
    def setNamespace(self, prefix, name):
        self._get_mini_repository().addNamespace(prefix, name)

    ## Removes a namespace declaration by removing the association between a
    ## prefix and a namespace name.
    def removeNamespace(self, prefix):
        self._get_mini_repository().deleteNamespace(prefix)

    ## Removes all namespace declarations from the repository.
    def clearNamespaces(self):
        for prefix in self.getNamespaces().iterkeys():
            self.removeNamespace(prefix)

    #############################################################################################
    ## Geo-spatial
    #############################################################################################
    
    def createRectangularSystem(self, scale=1, unit=None, xMin=0, xMax=None, yMin=0, yMax=None):
        self.geoType = GeoType(GeoType.Cartesian, scale=scale, unit=unit, xMin=xMin, xMax=xMax, yMin=yMin, yMax=yMax)
                               ##,latMin=None, latMax=None, longMin=None, longMax=None)
        self.geoType.setConnection(self)        
        return self.geoType
    
    def createLatLongSystem(self, unit='degree', scale=None, latMin=None, latMax=None, longMin=None, longMax=None):
        self.geoType = GeoType(GeoType.Spherical, unit=unit, scale=scale, latMin=latMin, latMax=latMax, longMin=longMin, longMax=longMax)
        self.geoType.setConnection(self)
        return self.geoType
    
    def getGeoType(self):
        return self.geoType
    
    def setGeoType(self, geoType):
        self.geoType = geoType
        geoType.setConnection(self)
        
    def createCoordinate(self, x=None, y=None, lat=None, long=None):
        """
        Create an x, y  or lat, long  coordinate in the current coordinate system.
        """
        return self.geoType.createCoordinate(x=x, y=y, lat=lat, long=long)
    
    def createBox(self, xMin=None, xMax=None, yMin=None, yMax=None):
        """
        Define a rectangular region for the current coordinate system.
        """
        return self.geoType.createBox(xMin=xMin, xMax=xMax, yMin=yMin, yMax=yMax)
    
    def createCircle(self, x, y, radius, unit=None):
        """
        Define a circular region with vertex x,y and radius "radius".
        The distance unit for the radius is either 'unit' or the unit specified
        for the current system.
        """
        return self.geoType.createCircle(x, y, radius, unit=unit)
    
    def createPolygon(self, vertices, uri=None, geoType=None):
        """
        Define a polygonal region with the specified vertices.  'vertices'
        is a list of x,y pairs.  The 'uri' is optional.
        """
        return self.geoType.createPolygon(vertices, uri=uri)

    #############################################################################################
    ## SNA   Social Network Analysis Methods
    #############################################################################################
    
    def registerSNAGenerator(self, name, subjectOf=None, objectOf=None, undirected=None, generator_query=None):
        """
        Create (and remember) a generator named 'name'.
        If one already exists with the same name; redefine it.
        'subjectOf', 'objectOf' and 'undirected' expect a list of predicate URIs, expressed as 
        fullURIs or qnames, that define the edges traversed by the generator.
        Alternatively, instead of an adjacency map, one may provide a 'generator_query',
        that defines the edges.

        For use with an open session.
        """
        miniRep = self._get_mini_repository()
        miniRep.registerSNAGenerator(name, subjectOf=subjectOf, objectOf=objectOf, undirected=undirected, 
                                     query=generator_query)

    def registerNeighborMatrix(self, name, generator, group_uris, max_depth=2):
        """
        Construct a neighbor matrix named name.  The generator named 'generator' is applied
        to each URI in 'group_uris' (a collection of fullURIs or qnames (strings)),
        computing edges to max depth 'max_depth'.

        For use with an open session.
        """
        miniRep = self._get_mini_repository()
        miniRep.registerNeighborMatrix(name, group_uris, generator, max_depth)

    def evalFreeTextSearch(self, pattern, infer=False, callback=None, limit=None):
        """
        Return an array of statements for the given free-text pattern search.
        """
        miniRep = self._get_mini_repository()
        return miniRep.evalFreeTextSearch(pattern, infer, callback, limit)
        
    def listFreeTextPredicates(self):
        """
        List the predicates that are used for free-text indexing.
        """
        return self.repository.listFreeTextPredicates()
        
    def openSession(self, autocommit=False, lifetime=None, loadinitfile=False):
        """
        Open a session.

        If autocommit is True, commits are done on each request, otherwise
        you will need to call commit() or rollback() as appropriate for your
        application.

        lifetime is an integer specifying the time to live in seconds of 
        the session.

        If loadinitfile is True, then the current initfile will be loaded
        for you when the session starts.
        """
        miniRep = self._get_mini_repository()
        if miniRep == self.repository.mini_repository:
            # Don't use the shared mini_repository for a session
            miniRep = self.mini_repository = copy.copy(self.repository.mini_repository)

        return miniRep.openSession(autocommit, lifetime, loadinitfile)

    def closeSession(self):
        """
        Close a session.
        """
        # Keeping the clone of the mini_repository is fine in case the user
        # calls openSession again.
        miniRep = self._get_mini_repository()
        return miniRep.closeSession()

    @contextmanager
    def session(self,  autocommit=False, lifetime=None, loadinitfile=False):
        """
        A session context manager for use with the 'with' statement:

        with conn.session():
            # Automatically calls openSession at block start
            # Do work
            # Automatically calls closeSession at block end


        If autocommit is True, commits are done on each request, otherwise
        you will need to call commit() or rollback() as appropriate for your
        application.

        lifetime is an integer specifying the time to live in seconds of 
        the session.

        If loadinitfile is True, then the current initfile will be loaded
        for you when the session starts.
        """
        self.openSession(autocommit, lifetime, loadinitfile)
        yield self
        self.closeSession()

    def commit(self):
        """
        Commits changes on an open session.
        """
        return self._get_mini_repository().commit()

    def rollback(self):
        """
        Rolls back changes on open session.
        """
        return self._get_mini_repository().rollback()

    


class GeoType:
    Cartesian = 'CARTESIAN'
    Spherical = 'SPHERICAL'
    def __init__(self, system, scale=None, unit=None, xMin=None, xMax=None, yMin=None, yMax=None, latMin=None, latMax=None, longMin=None, longMax=None):
        self.system = system
        self.connection = None
        self.scale = scale
        self.unit = unit
        self.xMin = xMin
        self.xMax = xMax
        self.yMin = yMin
        self.yMax = yMax
        self.latMin = latMin
        self.latMax = latMax
        self.longMin = longMin
        self.longMax = longMax
        self.miniGeoType = None
        if system == GeoType.Cartesian:
            if unit:
                raise Exception("Units not yet supported for rectangular coordinates.")
        else: pass  ## can't happen
    
    def setConnection(self, connection): self.connection = connection
        
    def _getMiniGeoType(self):
        def stringify(term): return str(term) if term is not None else None
        if not self.miniGeoType:
            if self.system == GeoType.Cartesian:
                self.miniGeoType = self.connection._get_mini_repository().getCartesianGeoType(stringify(self.scale), stringify(self.xMin), stringify(self.xMax),
                                                                                stringify(self.yMin), stringify(self.yMax))
            elif self.system == GeoType.Spherical:
                self.miniGeoType = self.connection._get_mini_repository().getSphericalGeoType(stringify(self.scale), unit=stringify(self.unit), 
                                latMin=stringify(self.latMin), latMax=stringify(self.latMax), longMin=stringify(self.longMin), longMax=stringify(self.longMax))
        return self.miniGeoType

    def createCoordinate(self, x=None, y=None, lat=None, long=None, unit=None):
        """
        Create an x, y  or lat, long  coordinate for the system defined by this geotype. 
        """
        return GeoCoordinate(x=(x or lat), y=y or long, unit=unit, geoType=self)
    
    def createBox(self, xMin=None, xMax=None, yMin=None, yMax=None, unit=None):
        """
        Define a rectangular region for the current coordinate system.
        """
        return GeoBox(xMin, xMax, yMin, yMax, unit=unit, geoType=self)

    def createCircle(self, x, y, radius, unit=None):
        """
        Define a circular region with vertex x,y and radius "radius".
        The distance unit for the radius is either 'unit' or the unit specified
        for this GeoType.  If 'innerRadius' is specified, the region is
        ring-shaped, consisting of the space between the inner and outer circles.
        """
        return GeoCircle(x, y, radius, unit=unit, geoType=self)

    def createPolygon(self, vertices, uri=None):
        """
        Define a polygonal region with the specified vertices.  'vertices'
        is a list of x,y pairs. The 'uri' is optional.
        """
        poly = GeoPolygon(vertices, uri=uri, geoType=self)
        poly.resource = self.connection.createURI(uri) if uri else self.connection.createBNode()
        miniResource = self.connection._convert_term_to_mini_term(poly.resource)
        miniRep = self.connection._get_mini_repository()
        if self.system == GeoType.Cartesian:
            miniVertices = [miniRep.createCartesianGeoLiteral(self._getMiniGeoType(), coord[0], coord[1]) for coord in poly.vertices]
        elif self.system == GeoType.Spherical:
            miniVertices = [miniRep.createSphericalGeoLiteral(self._getMiniGeoType(), coord[0], coord[1]) for coord in poly.vertices]
        miniRep.createPolygon(miniResource, miniVertices)
        return poly
