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
from __future__ import with_statement
from __future__ import unicode_literals
from future.builtins import object
from past.builtins import map, unicode, basestring

from franz.miniclient.request import wrap_callback
from franz.openrdf.util.contexts import output_to
from .repositoryresult import RepositoryResult

from ..exceptions import IllegalOptionException, IllegalArgumentException
from ..model import Statement, Value, URI
from ..model.literal import RangeLiteral, GeoCoordinate, GeoSpatialRegion, GeoBox, GeoCircle, GeoPolygon, Literal
from ..query.dataset import ALL_CONTEXTS, MINI_NULL_CONTEXT
from ..query.query import Query, TupleQuery, UpdateQuery, GraphQuery, BooleanQuery, QueryLanguage
from ..rio.rdfformat import RDFFormat
from ..util import uris

try:
    from collections import namedtuple
except ImportError:
    from ..util.namedtuple import namedtuple

class PrefixFormat(namedtuple('EncodedIdPrefix', 'prefix format')):
    __slots__ = ()

import copy, sys, warnings
from contextlib import contextmanager

if sys.version_info[0] > 2:
    # Hack for isinstance checks
    import io
    file = io.IOBase

# RepositoryConnection is the main interface for updating data in and performing
# queries on a repository.
#
# See http://www.franz.com/agraph/support/documentation/v5/python-tutorial/python-API.html
# or your local installation's tutorial/python-API.html for the API documentation.
#
# See http://www.franz.com/agraph/support/documentation/v5/python-tutorial/python-tutorial.html
# or your local installation's tutorial/python-tutorial.html for the tutorial.

class RepositoryConnection(object):
    def __init__(self, repository, close_repo=False):
        """
        Call through :meth:`~franz.openrdf.repository.repository.Repository.getConnection`.

        :param repository: Repository to connect to.
        :type repository: Repository
        :param close_repo: If ``True`` shutdown the repository when this connection is closed.
                          The default is ``False``.
        :type close_repo: bool
        """
        self.repository = repository
        self.mini_repository = repository.mini_repository
        self.is_closed = False
        self._add_commit_size = None
        self._close_repo = close_repo
        self.is_session_active = False

    def getSpec(self):
        return self.repository.getSpec()

    def _get_mini_repository(self):
        return self.mini_repository

    def getValueFactory(self):
        return self.repository.getValueFactory()

    def close(self):
        """
        Close the connection. This also closes the session if it is active.

        It is safe to call this on a connection that has already been closed.

        Note that using ``with`` is the preferred way to manage connections.
        """
        if not self.is_closed:
            self.closeSession()
            self.is_closed = True
            if self._close_repo:
                self.repository.shutDown()

    def setAddCommitSize(self, triple_count):
        if not triple_count or triple_count < 0:
            self._add_commit_size = None
        else:
            self._add_commit_size = int(triple_count)

    def getAddCommitSize(self):
        return self._add_commit_size

    add_commit_size = property(
        getAddCommitSize, setAddCommitSize,
        doc="""The threshold for commit size during triple add operations.
               Set to 0 (zero) or None to clear size-based autocommit behavior.
               When set to an integer triple_count > 0, a commit will occur every
               triple_count triples added and at the end of the triples being added.""")

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

    def prepareUpdate(self, queryLanguage, queryString, baseURI=None):
        """
        Embed 'queryString' into a query object which can be
        executed against the RDF storage.  'queryString' can be a
	SPARQL 1.1 Update command such as INSERT DATA or DELETE DATA.
        The returned result of execution either True or False.
	"""
        query = UpdateQuery(queryLanguage, queryString, baseURI=baseURI)
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
        Return a list of context resources, one for each context referenced bya quad in
        the triple store.  Omit the default context, since no one had the intelligence to
        make it a first-class object.
        """
        contexts = []
        for cxt in self._get_mini_repository().listContexts():
            contexts.append(self.createURI(cxt))
        return contexts

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

    def isEmpty(self):
        """
        Returns <tt>true</tt> if this repository does not contain any (explicit)
        statements.
        """
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

    def getStatements(self, subject=None, predicate=None,  object=None, contexts=ALL_CONTEXTS, includeInferred=False,
                       limit=None, offset=None, tripleIDs=False, output=None, output_format=RDFFormat.NQX):
        """
        Gets all statements with a specific subject, predicate and/or object from
        the repository. The result is optionally restricted to the specified set
        of named contexts.  Returns a RepositoryResult that produces a 'Statement'
        each time that 'next' is called.  Alternatively the output can be written
        to a file or a file-like object passed in the output parameter. In that case
        None is returned. Set output_format to an RDFormat to control the serialization
        format.
        """
        with output_to(output) as out_file:
            callback = None if output is None else out_file.write
            accept = None if output is None else RDFFormat.mime_type_for_format(output_format)

            subj = self._convert_term_to_mini_term(subject)
            pred = self._convert_term_to_mini_term(predicate)
            obj = self._convert_term_to_mini_term(object, predicate)
            cxt = self._contexts_to_ntriple_contexts(contexts)
            if isinstance(object, GeoSpatialRegion):
                if cxt is not None and cxt != ALL_CONTEXTS:
                    raise ValueError('Geospatial queries cannot be limited to a context.')
                return self._getStatementsInRegion(subj, pred, obj, limit=limit, offset=offset,
                                                   accept=accept, callback=callback)
            else:
                result = self._get_mini_repository().getStatements(
                    subj, pred, obj, cxt,
                    infer=includeInferred, limit=limit, offset=offset, tripleIDs=tripleIDs,
                    accept=accept, callback=callback)

            if output is None:
                return RepositoryResult(result, tripleIDs=tripleIDs)
            else:
                return None

    def getStatementsById(self, ids, output=None, output_format=RDFFormat.NQX):
        """
        Return all statements whose triple ID matches an ID in the list 'ids'.
        """
        with output_to(output) as out_file:
            callback = None if output is None else out_file.write
            accept = None if output is None else RDFFormat.mime_type_for_format(output_format)
            result = self._get_mini_repository().getStatementsById(ids, accept=accept, callback=callback)
        if output is None:
            return RepositoryResult(result, tripleIDs=False)
        else:
            return False

    def _getStatementsInRegion(self, subject, predicate, region, limit=None, offset=None, accept=None, callback=None):
        geoType = region.geoType
        miniGeoType = geoType._getMiniGeoType()
        common_args = {
            'limit': limit,
            'offset': offset,
            'accept': accept,
            'callback': callback
        }

        if isinstance(region, GeoBox):
            if geoType.system == GeoType.Cartesian:
                stringTuples = self._get_mini_repository().getStatementsInsideBox(
                    miniGeoType, predicate, region.xMin, region.xMax, region.yMin, region.yMax,
                    **common_args)
            elif geoType.system == GeoType.Spherical:
                stringTuples = self._get_mini_repository().getStatementsInsideBox(
                    miniGeoType, predicate, region.yMin, region.yMax, region.xMin, region.xMax,
                    **common_args)
            else:
                raise ValueError('Unsupported coordinate system: %s' % geoType.system)

        elif isinstance(region, GeoCircle):
            if geoType.system == GeoType.Cartesian:
                stringTuples = self._get_mini_repository().getStatementsInsideCircle(miniGeoType, predicate,
                                        region.x, region.y, region.radius, **common_args)
            elif geoType.system == GeoType.Spherical:
                stringTuples = self._get_mini_repository().getStatementsHaversine(miniGeoType, predicate,
                                        region.x, region.y, region.radius, unit=region.unit,
                                        **common_args)
            else:
                raise ValueError('Unsupported coordinate system: %s' % geoType.system)
        elif isinstance(region, GeoPolygon):
            stringTuples = self._get_mini_repository().getStatementsInsidePolygon(miniGeoType, predicate,
                                        self._convert_term_to_mini_term(region.getResource()),
                                        **common_args)
        else:
            raise ValueError('Unsupported region type: %s' % type(region))     # can't happen

        if callback is None:
            return RepositoryResult(stringTuples, subjectFilter=subject)
        else:
            return None

    # NOTE: 'format' shadows a built-in symbol but it is too late to change the public API
    def add(self, arg0, arg1=None, arg2=None, contexts=None, base=None, format=None, serverSide=False):
        """
        Calls addTriple, addStatement, or addFile.  If 'contexts' is not
        specified, adds to the null context.
        """
        if contexts and not isinstance(contexts, list):
            contexts = [contexts]
        if isinstance(arg0, (basestring, file)):
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

    # NOTE: 'format' shadows a built-in symbol but it is too late to change the public API
    def addFile(self, filePath, base=None, format=None, context=None, serverSide=False, content_encoding=None):
        """
        Load the file or file path 'filePath' into the store. 
        'base' optionally defines a base URI,
        'format' is an RDFFormat (e.g. RDFFormat.NTRIPLES) or None 
        (will be guessed from the extension of filePath), and 'context'
        optionally specifies which context the triples will be loaded into.
        GZIP-compressed files can be loaded by passing 'gzip' as the value
        of 'content_encoding'. This is only required if the file extension
        is not '.gz'.
        """
        if isinstance(context, (list, tuple)):
            if len(context) > 1:
                raise IllegalArgumentException("Multiple contexts passed to 'addFile': %s" % context)
            context = context[0] if context else None
        contextString = self._context_to_ntriples(context, none_is_mini_null=True)

        fmt, ce = RDFFormat.format_for_file_name(filePath)
        format = format or fmt
        content_encoding = content_encoding or ce
    
        self._get_mini_repository().loadFile(
            filePath, format, context=contextString, serverSide=serverSide,
            commitEvery=self.add_commit_size, baseURI=base,
            content_encoding=content_encoding)

    def addData(self, data, rdf_format=RDFFormat.TURTLE, base_uri=None, context=None):
        """
        Adds data from a string to the repository.

        :param data: Data to be added.
        :param rdf_format: Data format - either a RDFFormat or a MIME type (string).
        :type rdf_format: RDFFormat|str
        :param base_uri: Base for resolving relative URIs.
                         If None (default), the URI will be chosen by the server.
        :param context: Graph to add the data to.
                        If None (default) the default graph will be used..
        """
        self._get_mini_repository().loadData(
            data, rdf_format, base_uri=base_uri, context=context)

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
        If 'term' is an OpenRDF term, convert it to a string.  If it's already
        a string, assume it's in ntriples format, and just pass it through.
        If the term is None, return None.
        Otherwise convert `term` to Literal and make a string from that.
        """
        if term is None or isinstance(term, basestring):
            return term
        elif hasattr(term, 'toNTriples'):
            return term.toNTriples()
        else:
            return Literal(term).toNTriples()

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

    def removeStatement(self, statement, contexts=None):
        """
        Removes the supplied statement(s) from the specified contexts in the repository.
        """
        self.removeTriples(statement.getSubject(), statement.getPredicate(), statement.getObject(), contexts=contexts)

    def clear(self, contexts=ALL_CONTEXTS):
        """
        Removes all statements from designated contexts in the repository.  If
        'contexts' is ALL_CONTEXTS, clears the repository of all statements.
        """
        self.removeTriples(None, None, None, contexts=contexts)

    def export(self, handler, contexts=ALL_CONTEXTS):
        """
        Exports all explicit statements in the specified contexts to the supplied
        RDFHandler.

        .. deprecated:: 4.6
           Use getStatements(output=...) instead.
        """
        warnings.warn("export is deprecated. Use getStatements(output=...) instead.", DeprecationWarning, stacklevel=2)
        self.exportStatements(None, None, None, False, handler, contexts=contexts)

    def exportStatements(self, subj, pred, obj, includeInferred, handler, contexts=ALL_CONTEXTS):
        """
        Exports all statements with a specific subject, predicate and/or object
        from the repository, optionally from the specified contexts.

        .. deprecated:: 4.6
           Use getStatements(output=...) instead.
        """
        warnings.warn("exportStatements is deprecated. Use getStatements(output=...) instead.",
                      DeprecationWarning, stacklevel=2)
        self.getStatements(subj, pred, obj, contexts,
                           output=handler.getFilePath() or sys.stdout,
                           output_format=handler.getRDFFormat(),
                           includeInferred=includeInferred)

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

    ## Indexing control methods

    def listIndices(self):
        return self._get_mini_repository().listIndices()

    def listValidIndices(self):
        return self._get_mini_repository().listValidIndices()

    def addIndex(self, _type):
        return self._get_mini_repository().addIndex(_type)

    def dropIndex(self, _type):
        return self._get_mini_repository().dropIndex(_type)

    def optimizeIndices(self, level=None, wait=None):
        """
        Optimize indices

        Please see documenation for argument values and meanings:

        http://www.franz.com/agraph/support/documentation/v5/triple-index.html#optimize
        """
        return self._get_mini_repository().optimizeIndices(level, wait);

    #############################################################################################
    ## ValueFactory methods
    ## Added here because its inconvenient to have to dispatch from three different objects
    ## (connection, repository, and value factory) when one will do
    #############################################################################################

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

    def addRules(self, rules, language=QueryLanguage.PROLOG):
        """
        Add a sequence of one or more rules (in ASCII format) to the current environment.
        If the language is Prolog, rule declarations start with '<-' or '<--'.  The
        former appends a new rule; the latter overwrites any rule with the same predicate.

        For use with an open session.
        """
        if language == QueryLanguage.PROLOG:
            self._get_mini_repository().definePrologFunctors(rules)
        else:
            raise Exception("Cannot add a rule because the rule language has not been set.")

    def loadRules(self, filename, language=QueryLanguage.PROLOG):
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

    def getNamespaces(self):
        """
        Get all declared prefix/namespace pairs
        """
        namespaces = {}
        for pair in self._get_mini_repository().listNamespaces():
            namespaces[pair['prefix']] = pair['namespace']
        return namespaces

    def getNamespace(self, prefix):
        """
        Gets the namespace that is associated with the specified prefix, if any.
        """
        return self._get_mini_repository().getNamespace(prefix)

    def setNamespace(self, prefix, name):
        """
        Sets the prefix for a namespace.
        """
        self._get_mini_repository().addNamespace(prefix, name)

    def removeNamespace(self, prefix):
        """
        Removes a namespace declaration by removing the association between a
        prefix and a namespace name.
        """
        self._get_mini_repository().deleteNamespace(prefix)

    def clearNamespaces(self, reset=True):
        """
        Deletes all namespaces in this repository for the current user. If a
        `reset` argument of `True` is passed, the user's namespaces are reset
        to the default set of namespaces, otherwise all namespaces are cleared.
        """
        self._get_mini_repository().clearNamespaces(reset)

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

    def createCoordinate(self, x=None, y=None, latitude=None, longitude=None):
        """
        Create an x, y  or lat, long  coordinate in the current coordinate system.
        """
        return self.geoType.createCoordinate(x=x, y=y, latitude=latitude, longitude=longitude)

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

    def listFreeTextIndices(self):
        return self.mini_repository.listFreeTextIndices()

    def createFreeTextIndex(self, name, predicates=None, indexLiterals=None, indexResources=None,
                            indexFields=None, minimumWordSize=None, stopWords=None, wordFilters=None,
                            innerChars=None, borderChars=None, tokenizer=None):
        """
        Create a free-text index with the given parameters.
        If no predicates are given, triples are indexed regardless of
        predicate.
        indexLiterals determines which literals to index. It can be
        True (the default), False, or a list of resources, indicating
        the literal types that should be indexed.
        indexResources determines which resources are indexed. It can
        be True, False (the default), or \"short\", to index only the
        part of resources after the last slash or hash character.
        indexFields can be a list containing any combination of the
        elements \"subject\", \"predicate\", \"object\", and
        \"graph\". The default is [\"object\"].
        minimumWordSize, an integer, and determines the minimum size a
        word must have to be indexed. The default is 3.
        stopWords should hold a list of words that should not be
        indexed. When not given, a list of common English words is
        used.
        wordFilters can be used to apply some normalizing filters to
        words as they are indexed or queried. Can be a list of filter
        names. Currently, only \"drop-accents\" and \"stem.english\"
        are supported.
        innerChars and borderChars can be lists. tokenizer is a string.

        See  http://www.franz.com/agraph/support/documentation/v5/http-protocol.html#put-freetext-index
        """
        if predicates: predicates = list(map(uris.asURIString, predicates))
        if isinstance(indexLiterals, list): indexLiterals = list(map(uris.asURIString, indexLiterals))
        self.mini_repository.createFreeTextIndex(name, predicates=predicates, indexLiterals = indexLiterals,
                                                 indexResources=indexResources, indexFields=indexFields,
                                                 minimumWordSize=minimumWordSize, stopWords=stopWords,
                                                 wordFilters=wordFilters, innerChars=innerChars,
                                                 borderChars=borderChars, tokenizer=tokenizer)

    def modifyFreeTextIndex(self, name, predicates=None, indexLiterals=None, indexResources=None,
                            indexFields=None, minimumWordSize=None, stopWords=None, wordFilters=None,
                            reIndex=None, innerChars=None, borderChars=None, tokenizer=None):
        if predicates: predicates = list(map(uris.asURIString, predicates))
        if isinstance(indexLiterals, list): indexLiterals = list(map(uris.asURIString, indexLiterals))
        self.mini_repository.modifyFreeTextIndex(name, predicates=predicates, indexLiterals = indexLiterals,
                                                 indexResources=indexResources, indexFields=indexFields,
                                                 minimumWordSize=minimumWordSize, stopWords=stopWords,
                                                 wordFilters=wordFilters, reIndex=reIndex, innerChars=innerChars,
                                                 borderChars=borderChars, tokenizer=tokenizer)

    def deleteFreeTextIndex(self, name):
        self.mini_repository.deleteFreeTextIndex(name)

    def getFreeTextIndexConfiguration(self, name):
        value = self.mini_repository.getFreeTextIndexConfiguration(name)
        value["predicates"] = list(map(URI, value["predicates"]))
        if isinstance(value["indexLiterals"], list):
            value["indexLiterals"] = list(map(URI, value["indexLiterals"]))
        return value

    def evalFreeTextSearch(self, pattern, infer=False, callback=None, limit=None, offset=None, index=None):
        """
        Return an array of statements for the given free-text pattern search.
        """
        miniRep = self._get_mini_repository()
        return miniRep.evalFreeTextSearch(pattern, index, infer, wrap_callback(callback), limit, offset=offset)

    def openSession(self, autocommit=False, lifetime=None, loadinitfile=False):
        """
        Open a session.

        It is not an error to call this when a session is already active on this connection,
        but no new session will be created (i.e. nested sessions are not supported).

        .. seealso::

           http://franz.com/agraph/support/documentation/current/http-protocol.html#sessions
              More detailed explanation of session-related concepts in the HTTP API reference.

        :param autocommit: if ``True``, commits are done on each request, otherwise
                           you will need to call :meth:`.commit` or :meth:`.rollback`
                           as appropriate for your application.
                           The default value is ``False``.
        :type autocommit: bool
        :param lifetime: Time (in seconds) before the session expires when idle.
                         Note that the client maintains a thread that pings the
                         session before this happens.
                         The maximum acceptable value is 21600 (6 hours).
                         When the value is ``None`` (the default) the lifetime
                         is set to 300 seconds (5 minutes).
        :type lifetime: int
        :param loadinitfile: if ``True`` then the current initfile will be loaded
                             for you when the session starts. The default is ``False``.
        :type loadinitfile: bool
        """
        if not self.is_session_active:
            miniRep = self._get_mini_repository()
            if miniRep == self.repository.mini_repository:
                # Don't use the shared mini_repository for a session
                miniRep = self.mini_repository = copy.copy(self.repository.mini_repository)

            miniRep.openSession(autocommit, lifetime, loadinitfile)
            self.is_session_active = True

    def closeSession(self):
        """
        Close a session.

        It is not an error to call this when no session is active.
        """
        if self.is_session_active:
            # Not deleting the dedicated mini_repository in case the user
            # calls openSession again.
            self._get_mini_repository().closeSession()
            self.is_session_active = False

    def __enter__(self):
        return self

    def __exit__(self, *args):
        del args
        self.closeSession()
        self.close()

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


    def runAsUser(self, username=None):
        """
        Only for use as a superuser during a session.

        Runs requests on this connection as username.

        None - the default - clears the setting.
        """
        return self._get_mini_repository().runAsUser(username)

    @contextmanager
    def saveResponse(self, fileobj, accept, raiseAll=False):
        """
        Save the server response(s) for the call(s) within the with statement
        to fileobj, using accept for the response type requested.

        .. deprecated:: 100.0.1
           Use getStatements(output=...) or evaluate(output=...) to export data.

        Responses will be uncompressed and saved to fileobj, but not decoded.
        Therefore, the API called will likely error out shortly after the
        response is saved (which is okay because we really only want
        the side-effect of saving the response).

        RequestError is always thrown on errors from the server.
        Other exceptions can be optionally raised with raiseAll=True.

        You will only want to make only one conn call in the with statement
        unless you wrap each call in its own try/except.

        Example:

        with open('out', 'w') as response:
            with conn.saveResponse(response, 'application/rdf+xml'):
                conn.getStatements(None, None, None) # The response is written to response
        """
        with self._get_mini_repository().saveResponse(fileobj, accept, raiseAll):
            yield

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

    def evalInServer(self, code):
        """
        Evaluate the Lisp code in the server.

        You must have "eval" permissions to the store to use this feature.
        """

        return self._get_mini_repository().evalInServer(code)

    def evalJavaScript(self, code):
        """
        Evaluate the JavaScript code in the server.

        You must have "eval" permissions to the store to use this feature.
        """
        return self._get_mini_repository().evalJavaScript(code)

    # NOTE: 'format' shadows a built-in symbol but it is too late to change the public API
    def registerEncodedIdPrefix(self, prefix, format):
        """
        Registers a single encoded prefix.

        See: http://franz.com/agraph/support/documentation/v5/encoded-ids.html
        """
        return self._get_mini_repository().registerEncodedIdPrefix(prefix, format)

    def registerEncodedIdPrefixes(self, registrations):
        """
        Registers multiple encoded prefixes. Any kind of iteratable collection
        of items with a prefix attribute and format attribute, or the prefix
        at index 0 and the format at index 1 will do (e.g. a list of tuples).
        Using PrefixFormat instances also works well.

        See: http://franz.com/agraph/support/documentation/v5/encoded-ids.html
        """
        return self._get_mini_repository().registerEncodedIdPrefixes(registrations)

    def listEncodedIdPrefixes(self):
        """
        Lists all encoded id prefixes.

        See: http://franz.com/agraph/support/documentation/v5/encoded-ids.html
        """
        regs = []

        enc_ids = self._get_mini_repository().listEncodedIdPrefixes()
        for enc_id in enc_ids:
            regs.append(PrefixFormat(enc_id["prefix"], enc_id["format"]))

        return regs

    def unregisterEncodedIdPrefix(self, prefix):
        """
        Unregisters the specified encoded id prefix.

        See: http://franz.com/agraph/support/documentation/v5/encoded-ids.html
        """
        return self._get_mini_repository().unregisterEncodedIdPrefix(prefix)

    def allocateEncodedIds(self, prefix, amount=1):
        """
        allocateEncodedIds allows you to use the server to allocate
        unique ids for a given registered prefix which has a fixed-width
        format specifier.

        See notes on next-encoded-upi-for-prefix in:

        http://franz.com/agraph/support/documentation/v5/encoded-ids.html
        """
        return self._get_mini_repository().allocateEncodedIds(prefix, amount)

    def deleteDuplicates(self, mode):
        """
        Delete all duplicates in the store. Must commit.

        mode - "spog" or "spo"

        For details see:

    http://www.franz.com/agraph/support/documentation/current/http-protocol.html#delete-statements-duplicates
        """
        self._get_mini_repository().deleteDuplicates(mode)

    def getDuplicateStatements(self, mode):
        """
        Return all duplicates in the store. Must commit.

        mode - "spog" or "spo", specifies how duplicates are determined

        For details see:

    http://www.franz.com/agraph/support/documentation/current/http-protocol.html#delete-statements-duplicates
        """
        stringTuples = self._get_mini_repository().getDuplicateStatements(mode)
        return RepositoryResult(stringTuples)

    def getDuplicateSuppressionPolicy(self):
        """
        Return the duplicate suppression policy used by the store.
        See https://franz.com/agraph/support/documentation/current/deleting-duplicate-triples.html

        :return: Duplicate suppression policy: "spo", "spog" or None.
        :rtype: str
        """
        return self._get_mini_repository().getDuplicateSuppressionPolicy()

    def setDuplicateSuppressionPolicy(self, mode):
        """
        Set the duplicate suppression policy used by the store.
        See https://franz.com/agraph/support/documentation/current/deleting-duplicate-triples.html

        :param mode: Duplicate suppression policy: "spo", "spog" or None (disable suppression).
        :type mode: str
        """
        self._get_mini_repository().setDuplicateSuppressionPolicy(mode)

    def disableDuplicateSuppression(self):
        """
        Disable duplicate suppression for this store.
        See https://franz.com/agraph/support/documentation/current/deleting-duplicate-triples.html
        """
        self._get_mini_repository().disableDuplicateSuppression()

    def callStoredProc(self, function, module, *args):
        return self._get_mini_repository().callStoredProc(function, module,
            *args)

    def getSpinFunction(self, uri):
        """
        Gets the string of the function for the given uri.
         uri - Spin function identifier
        """
        return self._get_mini_repository().getSpinFunction(uri)

    def putSpinFunction(self, uri, sparqlQuery, arguments):
        """
        Adds a Spin function.
         uri - Spin function identifier
         sparqlQuery - Spin function query text
         arguments - names of arguments in the sparqlQuery
        """
        self._get_mini_repository().putSpinFunction(uri, sparqlQuery, arguments)

    def deleteSpinFunction(self, uri):
        """
        Deletes the Spin function at the given uri.
         uri - Spin function identifier
        """
        return self._get_mini_repository().deleteSpinFunction(uri)

    def listSpinFunctions(self):
        """
        Returns a list of defined SPIN function.
        """
        return self._get_mini_repository().listSpinFunctions()

    def putSpinMagicProperty(self, uri, sparqlQuery, arguments):
        """
        Add a Spin magic property.
         uri - Spin magic property identifier
         sparqlQuery
         arguments - names of arguments to the sparqlQuery - must contain the leading question mark
        """
        self._get_mini_repository().putSpinMagicProperty(uri, sparqlQuery, arguments)

    def getSpinMagicProperty(self, uri):
        """
        Get the spin magic property for the uri
         uri - spin magic property identifier
        """
        return self._get_mini_repository().getSpinMagicProperty(uri)

    def listSpinMagicProperties(self):
        """
        Returns a list of defined SPIN magic properties function.
        """
        return self._get_mini_repository().listSpinMagicProperties()

    def deleteSpinMagicProperty(self, uri):
        """
        Deletes the Spin magic property at the given uri.
         uri - Spin magic property identifier
        """
        self._get_mini_repository().deleteSpinMagicProperty(uri)

    def materializeEntailed(self, _with=None, without=None, useTypeSubproperty=False, commit=100000):
        """
        Call to materialize entailed triples to enable reasoning queries without the dynamic query-time reasoner.
        Returns the number of triples added.

        _with and without can be either a single string or a list of strings denoting rules beyond rdfs++ you wish to use.
        See the documentation for the current set, but "same-as", "restriction", "values-from", "class", and "property" are examples.

        useTypeSubproperty tells the materializer to prefer using types that are rdfs:subPropertyOf rdf:type rather than rdf:type directly.

        commit indicates the number of triples per commit for the materializer.
        """
        return self._get_mini_repository().materializeEntailed(_with=_with, without=without, useTypeSubproperty=useTypeSubproperty, commit=commit)

    def deleteMaterialized(self):
        """
        Deletes all previously materialized triples.
        Returns the number of triples deleted.
        """
        return self._get_mini_repository().deleteMaterialized()


class GeoType(object):
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
        def stringify(term): return unicode(term) if term is not None else None
        if not self.miniGeoType:
            if self.system == GeoType.Cartesian:
                self.miniGeoType = self.connection._get_mini_repository().getCartesianGeoType(stringify(self.scale), stringify(self.xMin), stringify(self.xMax),
                                                                                stringify(self.yMin), stringify(self.yMax))
            elif self.system == GeoType.Spherical:
                self.miniGeoType = self.connection._get_mini_repository().getSphericalGeoType(stringify(self.scale), unit=stringify(self.unit),
                                latMin=stringify(self.latMin), latMax=stringify(self.latMax), longMin=stringify(self.longMin), longMax=stringify(self.longMax))
        return self.miniGeoType

    def createCoordinate(self, x=None, y=None, latitude=None, longitude=None, unit=None):
        """
        Create an x, y  or lat, long  coordinate for the system defined by this geotype.
        """
        return GeoCoordinate(x=(x or latitude), y=y or longitude, unit=unit, geoType=self)

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
