#!/usr/bin/env python
# -*- coding: utf-8 -*-
# pylint: disable-msg=C0103

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
from franz.openrdf.model.literal import RangeLiteral, GeoCoordinate, GeoSpatialRegion, GeoBox, GeoCircle, GeoPolygon
from franz.openrdf.model.statement import Statement
from franz.openrdf.model.value import Value
from franz.openrdf.query import query as query_module
from franz.openrdf.query.dataset import ALL_CONTEXTS, MINI_NULL_CONTEXT
from franz.openrdf.query.query import Query, TupleQuery, GraphQuery, BooleanQuery, QueryLanguage
from franz.openrdf.repository.jdbcresultset import JDBCResultSet
from franz.openrdf.repository.repositoryresult import RepositoryResult
from franz.openrdf.rio.rdfformat import RDFFormat
from franz.openrdf.util import uris
from franz.openrdf.vocabulary import *
import datetime
import os



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
        #self.mini_repository = repository.mini_repository
        self.is_closed = False
        self.ruleLanguage = None
        
    def _get_mini_repository(self):
        """
        The mini-repository code is not thread-safe.  If in multi-threaded
        mode, we create a new curl object before every HTTP call.  Here,
        we call 'get_mini_repository' repeatedly instead of caching the
        object to insure of the curl hack.
        """
        return self.repository._get_mini_repository()
        
    def getValueFactory(self):
        return self.repository.getValueFactory()
        
    def rollback(self):
        print "PRETENDING TO ROLLBACK"
    
    def close(self):
        self.is_closed = True
    
    def commit(self):
        print "PRETENDING TO COMMIT"
    
    def prepareQuery(self, queryLanguage, queryString, baseURI=None):
        """
        Embed 'queryString' into a query object which can be
        executed against the RDF storage.
        """
        ## THIS IS BOGUS; OR IS IT?  WE DON'T KNOW WHAT KIND OF QUERY IT IS:
        query = Query(queryLanguage, queryString, baseURI)
        query.setConnection(self)
        return query

    def prepareTupleQuery(self, queryLanguage, queryString, baseURI=None):
        """
        Embed 'queryString' into a query object which can be
        executed against the RDF storage.  'queryString' must be a SELECT
        query.  The result of query
        execution is an iterator of tuples.
        """
        query = TupleQuery(queryLanguage, queryString, baseURI=baseURI)
        query.setConnection(self)
        return query

    def prepareGraphQuery(self, queryLanguage, queryString, baseURI=None):
        """
        Parse 'queryString' into a query object which can be
        executed against the RDF storage.  'queryString' must be a CONSTRUCT
        or DESCRIBE query.  The result of query
        execution is an iterator of statements/quads.
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
#        else:
#            print "Computing the size of a context is currently very expensive"
#            resultSet = self.getJDBCStatements(None, None, None, contexts)
#            count = 0
#            while resultSet.next():
#                count += 1
#            return count
                

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
        if context is None: return MINI_NULL_CONTEXT if none_is_mini_null else None
        if context == MINI_NULL_CONTEXT: return MINI_NULL_CONTEXT
        elif context == 'null': return MINI_NULL_CONTEXT
        elif context: return context.toNTriples()
        elif none_is_mini_null: return MINI_NULL_CONTEXT
        else: return None            
       
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
            #MINITIMER = datetime.datetime.now()
            stringTuples = self._get_mini_repository().getStatements(subj, pred, obj, cxt,
                 infer=includeInferred, limit=limit, tripleIDs=tripleIDs)
            #print "mini elapsed time  " +  str(datetime.datetime.now() - MINITIMER)
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
    
    COLUMN_NAMES = ['subject', 'predicate', 'object', 'context']
    
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
        return JDBCResultSet(stringTuples, column_names = RepositoryConnection.COLUMN_NAMES, tripleIDs=tripleIDs)

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
        if isinstance(filePath, file):
            filePath = os.path.abspath(filePath.name)
        elif isinstance(filePath, str):
            if not filePath.startswith('/') and not filePath.lower().startswith('c:') and not filePath.lower().startswith("http:"):
                ## looks like its a relative file path; test to see if there is a local file that matches.
                ## If so, generate an absolute path name to enable AG server to read it:
                if os.path.exists(os.path.abspath(filePath)):
                    filePath = os.path.abspath(filePath)
        if isinstance(context, (list, tuple)):
            if len(context) > 1:
                raise IllegalArgumentException("Multiple contexts passed to 'addFile': %s" % context)
            context = context[0] if context else None
        contextString = self._context_to_ntriples(context, none_is_mini_null=True)
        if format == RDFFormat.NTRIPLES or filePath.lower().endswith('.nt'):
            self._get_mini_repository().loadFile(filePath, 'ntriples', context=contextString, serverSide=serverSide)
        elif format == RDFFormat.RDFXML or filePath.lower().endswith('.rdf') or filePath.lower().endswith('.owl'):
            self._get_mini_repository().loadFile(filePath, 'rdf/xml', context=contextString, baseURI=base, serverSide=serverSide)
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
            #print "MINITERM", obj, self._convert_term_to_mini_term(obj)
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
        self._get_mini_repository().addStatements(quads)
                
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

    def removeTriples(self, subject, predicate, object, contexts=None):
        """
        Removes the statement(s) with the specified subject, predicate and object
        from the repository, optionally restricted to the specified contexts.
        """
        subj = self._to_ntriples(subject)
        pred = self._to_ntriples(predicate)
        obj = self._to_ntriples(self.getValueFactory().object_position_term_to_openrdf_term(object))
        ## NEED TO FIGURE OUT HOW WILDCARD CONTEXT LOOKS HERE!!!
        ## THIS IS BOGUS FOR 'None' CONTEXT???; COMPLETELY AMBIGUOUS:  (NOT SURE IF THIS IS AN OLD STATEMENT)
        ntripleContexts = self._contexts_to_ntriple_contexts(contexts, none_is_mini_null=True)   
        if len(ntripleContexts) == 0:
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
    def clear(self, contexts=None):
        """
        Removes all statements from designated contexts in the repository.  If
        'contexts' is None, clears the repository of all statements.
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

    def listIndices(self):
        """
        List the SPOC indices currently active in the RDF Store.
        """
        indices = self._get_mini_repository().listIndices()
        return [idx[0:4].replace('g', 'c') for idx in indices]

    def addIndex(self, letters):
        """
        Register an SPOC index of type 'letters', where 'letters' is the concatenation
        of the letters 'SPOC' in whichever order is preferred.   An exception
        will be thrown if the combination is not supported by AllegroGraph.
        """
        self._get_mini_repository().addIndex(letters.replace('c', 'g') + 'i')

    def removeIndex(self, letters):
        """
        Drop an SPOC index.
        """
        self._get_mini_repository().deleteIndex(letters[0:4].replace('c', 'g') + 'i')
        
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
    
    def indexTriples(self, all=False, asynchronous=False):
        return self.repository.indexTriples(all=all, asynchronous=asynchronous)
    
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

    #############################################################################################
    ## Extensions
    #############################################################################################
    
    def createEnvironment(self, name):
        if not name in self._get_mini_repository().listEnvironments():
            self._get_mini_repository().createEnvironment(name)
        
    def deleteEnvironment(self, name):
        """
        Delete an environment.  This causes all rule and namespace definitions for this
        environment to be lost.
        """
        if name in self._get_mini_repository().listEnvironments():
            self._get_mini_repository().deleteEnvironment(name)
    
    def setEnvironment(self, name):
        """
        Choose an environment for execution of a Prolog query.  Rules and namespaces defined 
        in this environment persist across user sessions.  Call 'deleteEnvironment' to start
        with a fresh (empty) environment.
        """
        self.createEnvironment(name)
        self._get_mini_repository().setEnvironment(name)
    
    def listEnvironments(self):
        """
        List the names of environments currently maintained by the system.
        """
        return self._get_mini_repository().listEnvironments()
    
    def setRuleLanguage(self, queryLanguage):
        self.ruleLanguage = queryLanguage

    def addRules(self, rules, language=None):
        """
        Add a sequence of one or more rules (in ASCII format) to the current environment.
        If the language is Prolog, rule declarations start with '<-' or '<--'.  The 
        former appends a new rule; the latter overwrites any rule with the same predicate.
        """
        if not self._get_mini_repository().environment:
            raise Exception("Cannot add a rule because an environment has not been set.")
        language = language or self.ruleLanguage or QueryLanguage.PROLOG
        if language == QueryLanguage.PROLOG:
            rules = query_module.expandPrologQueryPrefixes(rules, self)
            self._get_mini_repository().definePrologFunctors(rules)
        else:
            raise Exception("Cannot add a rule because the rule language has not been set.")
        
    def loadRules(self, file, language=None):
        """
        Load a file of rules into the current environment.
        'file' is assumed to reside on the client machine.
        If the language is Prolog, rule declarations start with '<-' or '<--'.  The 
        former appends a new rule; the latter overwrites any rule with the same predicate.
        """
        f = open(file)
        body = f.read()
        f.close()
        self.addRules(body, language)
        
    def deleteRule(self, predicate, language=None):
        """
        Delete rule(s) with predicate named 'predicate'.  If 'predicate' is None, delete
        all rules.
        """
        if not self._get_mini_repository().environment:
            raise Exception("Cannot delete a rule because an environment has not been set.")
        language = language or self.ruleLanguage
        if language == QueryLanguage.PROLOG:
            self._get_mini_repository().deletePrologFunctor(predicate)
        else:
            raise Exception("Cannot add a rule because the rule language has not been set.")

    #############################################################################################
    ## Server-side implementation of namespaces
    #############################################################################################
      
#    ## Get all declared prefix/namespace pairs
#    def getNamespaces(self):
#        dict = {}
#        for pair in self._get_mini_repository().listNamespaces():
#            dict[pair[0]] = pair[1]
#        print "GET NAMESPACES", dict
#        return dict        
#
#    ## Gets the namespace that is associated with the specified prefix, if any.
#    def getNamespace(self, prefix):
#        return self.getNamespaces().get(prefix)
#
#    ## Sets the prefix for a namespace.
#    def setNamespace(self, prefix, name):
#        self._get_mini_repository().addNamespace(prefix, name)
#
#    ## Removes a namespace declaration by removing the association between a
#    ## prefix and a namespace name.
#    def removeNamespace(self, prefix):
#        self._get_mini_repository().deleteNamespace(prefix)
#
#    ## Removes all namespace declarations from the repository.
#    def clearNamespaces(self):
#        for prefix in self.getNamespaces().iterkeys():
#            self.removeNamespace(prefix)

    
    #############################################################################################
    ## In-memory implementation of namespaces
    #############################################################################################  

    NAMESPACES_MAP = {}
    
    def _get_namespaces_map(self):
        map = RepositoryConnection.NAMESPACES_MAP
        ## HMM. THE HTTPD SERVER IS NOW ROBUST WITH NAMESPACE SUBSTITUTIONS, SO WE
        ## DON'T REALLY NEED TO SEED THE MAP ANYMORE:
        if not map:
            map.update({"rdf": RDF.NAMESPACE, 
                        "rdfs": RDFS.NAMESPACE,
                        "xsd": XMLSchema.NAMESPACE,
                        "owl": OWL.NAMESPACE, 
                        "fti": "http://franz.com/ns/allegrograph/2.2/textindex/", 
                        "dc": "http://purl.org/dc/elements/1.1/",
                        "dcterms": "http://purl.org/dc/terms/",                                            
                        })
        return map
    
    def getNamespaces(self):
        """
        Return a dictionary of prefix/namespace pairings.
        """
        return self._get_namespaces_map()

    def getNamespace(self, prefix):
        """
        Return the namespace that is associated with the specified prefix, if any.
        """        
        return self._get_namespaces_map().get(prefix.lower(), None)

    def setNamespace(self, prefix, namespace):
        """
        Define (or redefine) a namespace 'namespace' for 'prefix'
        """
        uris.validateNamespace(namespace, True)
        self._get_namespaces_map()[prefix.lower()] = namespace
        if self._get_mini_repository().environment:
            self._get_mini_repository().addNamespace(prefix, namespace)

    def removeNamespace(self, prefix):
        """
        Remove a namespace association with 'prefix'.
        """
        self._get_namespaces_map().pop(prefix.lower(), None)

    def clearNamespaces(self):
        """
        Remove all namespace declarations.
        """
        self._get_namespaces_map().clear()

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
        """
        miniRep = self._get_mini_repository()
        miniRep.registerSNAGenerator(name, subjectOf=subjectOf, objectOf=objectOf, undirected=undirected, 
                                     query=generator_query)

    def deleteSNAGenerator(self, name):
        """
        Destroy the generator named 'name'.
        """
        miniRep = self._get_mini_repository()
        miniRep.deleteSNAGenerator(name)
    
    def listSNAGenerators(self):
        """
        Return a list of the names of registered SNA generators.
        ALTERNATIVELY, CONSIDER RETURNING A DICT THAT CONTAINS GENERATOR NAMES AS KEYS,
        AND ADJACENCY MAPS AS VALUES
        """
        miniRep = self._get_mini_repository()
        return miniRep.listSNAGenerators()
    
    def registerNeighborMatrix(self, name, generator, group_uris, max_depth=2):
        """
        Construct a neighbor matrix named name.  The generator named 'generator' is applied
        to each URI in 'group_uris' (a collection of fullURIs or qnames (strings)),
        computing edges to max depth 'max_depth'.
        """
        miniRep = self._get_mini_repository()
        miniRep.registerNeighborMatrix(name, group_uris, generator, max_depth)

    def rebuildNeighborMatrix(self, name):
        """
        Recompute the set of edges cached in the neighbor matrix named 'name'.
        """
        miniRep = self._get_mini_repository()
        miniRep.rebuildNeighborMatrix(name)
    
    def deleteNeighborMatrix(self, name):
        """
        Destroy the neighbor matrix named 'name'.
        """
        miniRep = self._get_mini_repository()
        miniRep.deleteNeighborMatrix(name)
    
    def listNeighborMatrices(self):
        """
        Return a list of the names of registered neighbor matrices
        """
        miniRep = self._get_mini_repository()
        return miniRep.listNeighborMatrices()

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
        
    def updateFreeTextIndexing(self):
        """
        Request that the server update the free-text index.
        """
        return self.repository.updateFreeTextIndexing()


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

    
