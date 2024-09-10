# pylint: disable-msg=C0103

################################################################################
# Copyright (c) 2006-2017 Franz Inc.
# All rights reserved. This program and the accompanying materials are
# made available under the terms of the MIT License which accompanies
# this distribution, and is available at http://opensource.org/licenses/MIT
################################################################################

import copy
import csv
import string
import sys
import warnings
from collections import OrderedDict, namedtuple
from contextlib import contextmanager
from io import IOBase as file

from franz.miniclient.agjson import encode_json
from franz.openrdf.exceptions import (
    IllegalArgumentException,
    IllegalOptionException,
    ServerException,
)
from franz.openrdf.model import URI, Statement, Value
from franz.openrdf.model.literal import (
    GeoBox,
    GeoCircle,
    GeoCoordinate,
    GeoPolygon,
    GeoSpatialRegion,
    Literal,
    RangeLiteral,
)
from franz.openrdf.query.dataset import ALL_CONTEXTS, MINI_NULL_CONTEXT
from franz.openrdf.query.query import (
    BooleanQuery,
    GraphQuery,
    Query,
    QueryLanguage,
    TupleQuery,
    UpdateQuery,
)
from franz.openrdf.repository.attributes import AttributeDefinition
from franz.openrdf.repository.repositoryresult import RepositoryResult
from franz.openrdf.repository.transactions import DEFAULT_TRANSACTION_SETTINGS
from franz.openrdf.rio.docformat import DocFormat
from franz.openrdf.rio.rdfformat import RDFFormat
from franz.openrdf.util import uris
from franz.openrdf.util.contexts import output_to


class PrefixFormat(namedtuple("EncodedIdPrefix", "prefix format")):
    __slots__ = ()


# if sys.version_info[0] > 2:
#     # Hack for isinstance checks
#     import io

#     file = io.IOBase

# Used instead of None as the default value of optional parameters
# when we want to distinguish between the caller passing None as
# the actual value and the parameter not being present in the call.
NOT_GIVEN = object()


class RepositoryConnection:
    """
    The RepositoryConnection class is the main interface for updating data
    in and performing queries on a
    :class:`~franz.openrdf.repository.repository.Repository`. By default, a
    RespositoryConnection is in autoCommit mode, meaning that each
    operation corresponds to a single transaction on the underlying triple
    store. autoCommit can be switched off, in which case it is up to the
    user to handle transaction commit/rollback. Note that care should be
    taken to always properly close a RepositoryConnection after one is
    finished with it, to free up resources and avoid unnecessary locks.

    Note that concurrent access to the same connection object is explicitly
    forbidden. The client must perform its own synchronization to ensure
    non-concurrent access.

    Several methods take a *vararg* argument that optionally specifies a
    set of contexts on which the method should operate. (A context is
    the URI of a subgraph.) Note that a *vararg* parameter is optional, it
    can be completely left out of the method call, in which case a method
    either operates on a provided statement's context (if one of the method
    parameters is a statement or collection of statements), or operates on
    the repository as a whole, completely ignoring context. A *vararg*
    argument may also be ``None``, meaning that the method operates on
    those statements which have no associated context only.
    """

    def __init__(self, repository, close_repo=False, is_session=False):
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
        self.has_dedicated_mini_repository = False
        self.is_closed = False
        self._add_commit_size = None
        self._close_repo = close_repo
        self.is_session_active = is_session

    def getSpec(self):
        """
        Get the session specification string for this repository.

        :return: Spec string suitable for use with
                 :meth:`~franz.openrdf.sail.allegrographserver.AllegroGraphServer.openSession`.
        """
        return self.repository.getSpec()

    # By default the mini-client might be shared with other connections to the same repository,
    # but when opening a session or changing connection settings we need to have our own client.
    def _get_mini_repository(self, dedicated=False):
        if dedicated and not self.has_dedicated_mini_repository:
            self.mini_repository = copy.copy(self.mini_repository)
            self.has_dedicated_mini_repository = True
        return self.mini_repository

    def getValueFactory(self):
        """
        Get the :class:`.ValueFactory` associated with this repository.

        Note that it is recommended to use methods defined in :class:`RepositoryConnection`
        instead of a value factory. The latter is only provided for RDF4J compatibility.

        :return: A value factory.
        :rtype: ValueFactory
        """
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
        """
        Set the value of :attr:`.add_commit_size`.
        :param triple_count: Value of :attr:`.add_commit_size`.
        """
        if not triple_count or triple_count < 0:
            self._add_commit_size = None
        else:
            self._add_commit_size = int(triple_count)

    def getAddCommitSize(self):
        """
        Get the current value of :attr:`.add_commit_size`.
        :return: Value of :attr:`.add_commit_size`.
        """
        return self._add_commit_size

    add_commit_size = property(
        getAddCommitSize,
        setAddCommitSize,
        doc="""The threshold for commit size during triple add operations.
               Set to 0 (zero) or None to clear size-based autocommit behavior.
               When set to an integer triple_count > 0, a commit will occur every
               triple_count triples added and at the end of the triples being added.""",
    )

    def prepareQuery(self, queryLanguage, queryString, baseURI=None):
        """
        Embed 'queryString' into a query object which can be
        executed against the RDF storage.
        """
        query = Query(queryLanguage, queryString, baseURI)
        query.setConnection(self)
        return query

    def prepareTupleQuery(
        self,
        queryLanguage=QueryLanguage.SPARQL,
        query=None,
        baseURI=None,
        queryString=None,
    ):
        """
        Parse ``query`` into a tuple query object which can be
        executed against the triple stroe.

        :param queryLanguage: Either ``QueryLanguage.SPARQL`` or ``QueryLanguage.PROLOG``.
        :type queryLanguage: QueryLanguage
        :param query: The query string (must contain a ``SELECT`` query).
        :type query: string
        :param baseURI: An optional base used to resolve relative URIs in the query.
        :type baseURI: string|URI
        :param queryString: Legacy name of the ``query`` parameter.
        :type queryString: string

        :return: A query object.
        :rtype: TupleQuery
        """
        query = TupleQuery(queryLanguage, query or queryString, baseURI=baseURI)
        query.setConnection(self)
        return query

    def prepareUpdate(
        self,
        queryLanguage=QueryLanguage.SPARQL,
        query=None,
        baseURI=None,
        queryString=None,
    ):
        """
        Parse ``query`` into an update query object which can be
        executed against the triple store.

        :param queryLanguage: Either ``QueryLanguage.SPARQL`` or ``QueryLanguage.PROLOG``.
        :type queryLanguage: QueryLanguage
        :param query: The query string (must contain an ``UPDATE`` query).
        :type query: string
        :param baseURI: An optional base used to resolve relative URIs in the query.
        :type baseURI: string|URI
        :param queryString: Legacy name of the ``query`` parameter.
        :type queryString: string

        :return: A query object.
        :rtype: UpdateQuery
        """
        query = UpdateQuery(queryLanguage, query or queryString, baseURI=baseURI)
        query.setConnection(self)
        return query

    def prepareGraphQuery(
        self,
        queryLanguage=QueryLanguage.SPARQL,
        query=None,
        baseURI=None,
        queryString=None,
    ):
        """
        Parse ``query`` into a graph query object which can be
        executed against the triple store.

        :param queryLanguage: Either ``QueryLanguage.SPARQL`` or ``QueryLanguage.PROLOG``.
        :type queryLanguage: QueryLanguage
        :param query: The query string (must be a``CONSTRUCT`` or ``DESCRIBE`` query).
        :type query: string
        :param baseURI: An optional base used to resolve relative URIs in the query.
        :type baseURI: string|URI
        :param queryString: Legacy name of the ``query`` parameter.
        :type queryString: string

        :return: A query object.
        :rtype: GraphQuery
        """
        query = GraphQuery(queryLanguage, query or queryString, baseURI=baseURI)
        query.setConnection(self)
        return query

    def prepareBooleanQuery(
        self,
        queryLanguage=QueryLanguage.SPARQL,
        query=None,
        baseURI=None,
        queryString=None,
    ):
        """
        Parse ``query`` into a boolean query object which can be
        executed against the triple store.

        :param queryLanguage: Either ``QueryLanguage.SPARQL`` or ``QueryLanguage.PROLOG``.
        :type queryLanguage: QueryLanguage
        :param query: The query string (must contain an ``ASK`` query).
        :type query: string
        :param baseURI: An optional base used to resolve relative URIs in the query.
        :type baseURI: string|URI
        :param queryString: Legacy name of the ``query`` parameter.
        :type queryString: string

        :return: A query object.
        :rtype: BooleanQuery
        """
        query = BooleanQuery(queryLanguage, query or queryString, baseURI=baseURI)
        query.setConnection(self)
        return query

    def getContextIDs(self):
        """
        Return a list of context resources, one for each context referenced by a quad in
        the triple store. Note that the default context will not be included in the result.

        :return: A list of contexts (as :class:`URI` objects).
        :rtype: list[URI]
        """
        contexts = []
        for cxt in self._get_mini_repository().listContexts():
            contexts.append(self.createURI(cxt))
        return contexts

    def size(self, contexts=ALL_CONTEXTS):
        """
        Returns the number of (explicit) statements that are in the specified
        contexts in this repository.

        :param contexts: List of contexts (graph URIs) to count the statements in.
                         By default statements in all graphs will be counted.
        :type contexts: Iterable[string|URI]
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
        Return ``True`` if this repository does not contain any (explicit) statements.
        """
        return self.size() == 0

    def _context_to_ntriples(self, context, none_is_mini_null=False):
        if context is None:
            return MINI_NULL_CONTEXT if none_is_mini_null else None

        if context == MINI_NULL_CONTEXT:
            return MINI_NULL_CONTEXT

        if context == "null":
            return MINI_NULL_CONTEXT

        if context:
            return (
                context if isinstance(context, (str, bytes)) else context.toNTriples()
            )

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
        if contexts == ALL_CONTEXTS:
            cxts = None
        elif contexts is None:
            if none_is_mini_null:
                cxts = [MINI_NULL_CONTEXT]
            else:
                cxts = None
        elif contexts == "null":
            cxts = [MINI_NULL_CONTEXT]
        elif isinstance(contexts, (list, tuple)):
            cxts = [
                self._context_to_ntriples(c, none_is_mini_null=True) for c in contexts
            ]
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
        if isinstance(term, GeoSpatialRegion):
            return term
        factory = self.getValueFactory()
        if isinstance(term, GeoCoordinate):
            geoType = term.geoType
            miniGeoType = geoType._getMiniGeoType()
            if geoType.system == GeoType.Cartesian:
                return self._get_mini_repository().createCartesianGeoLiteral(
                    miniGeoType, term.xcoor, term.ycoor
                )
            elif geoType.system == GeoType.Spherical:
                unit = term.unit or term.geoType.unit
                return self._get_mini_repository().createSphericalGeoLiteral(
                    miniGeoType, term.xcoor, term.ycoor, unit=unit
                )
            else:
                raise IllegalOptionException(
                    "Unsupported geo coordinate system", geoType.system
                )
        if isinstance(term, RangeLiteral):
            beginTerm = term.getLowerBound()
            endTerm = term.getUpperBound()
            return (self._to_ntriples(beginTerm), self._to_ntriples(endTerm))
        elif isinstance(term, (tuple, list)):
            return [
                self._convert_term_to_mini_term(
                    t, predicate_for_object=predicate_for_object
                )
                for t in term
            ]
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
            term = factory.object_position_term_to_openrdf_term(
                term, predicate=predicate_for_object
            )
            return self._to_ntriples(term)
        else:
            return self._to_ntriples(term)

    def warmup(self, includeStrings=None, includeTriples=None, indices=None):
        """
        Warm up specified parts of the repository in order to speed up access.

        :param includeStrings: warm up the string table  (default is True).
        :type includeStrings: bool
        :param includeTriples: warm up the triple indices (default is True).
            Use the indices argument to specify the indices to warm up.
        :type includeTriples: bool
        :param indices: list of indices to warm up (default is None meaning all indices).
           indices can be a single string naming an index (e.g. 'spogi') or
           a list of strings (e.g. ['spogi', 'posgi']).
           Note that  includeTriples must True in order for
           any indices be warmed up.
        :type indices: list[str]|str
        """

        return self._get_mini_repository().warmup(
            includeStrings, includeTriples, indices
        )

    def getStatements(
        self,
        subject=None,
        predicate=None,
        object=None,
        contexts=ALL_CONTEXTS,
        includeInferred=False,
        limit=None,
        offset=None,
        tripleIDs=False,
        output=None,
        output_format=RDFFormat.NQX,
    ):
        """
        Get all statements with a specific subject, predicate and/or
        object from the repository. The result is optionally
        restricted to the specified set of named contexts (graphs).

        Return a :class:`RepositoryResult` object that can be used to
        iterate over the resulting statements and filter out
        duplicates if desired. Alternatively one can write the
        results to a file or stream using the ``output`` parameter.

        :param subject: Subject value or ``None`` (no subject filtering).
        :type subject: Value
        :param predicate: Predicate value or ``None`` (no predicatefiltering).
        :type predicate: URI
        :param object: Object value or ``None`` (no object filtering).
        :type object: Value
        :param contexts: An optional list of graphs to retrieve the
                         statements from.
                         By default statements are taken from all graphs.
        :type contexts: URI|string|Iterable[URI|string]
        :param includeInferred: If ``True``, include triples inferred through
                                RDFS++ reasoning.
                                The default is ``False``.
        :type includeInferred: bool
        :param limit: Max number of statements to retrieve (optional).
        :type limit: int
        :param offset: Used in conjunction with ``limit`` to return results
                       starting from the nth statement.
        :type offset: int
        :param tripleIDs: If ``True`` the id field will be filled
                          in the returned statements.
        :param output: File path or a file-like object to write
                       the result to.
        :type output: str|file
        :param output_format: Serialization format for ``output``.
        :type output_format: RDFFormat
        :param include_attributes: If true the returned statements will
            include triple attributes. The default is false.
        :type include_attributes: bool
        :return: An iterator over the resulting statements
                 or ``None`` (if ``output`` is used).
        :rtype: RepositoryResult
        """
        # note: so with tripleIDs we'll get triples that are quads consisting of five elements.
        with output_to(output) as out_file:
            callback = None if output is None else out_file.write
            accept = (
                None
                if output is None
                else RDFFormat.mime_type_for_format(output_format)
            )

            subj = self._convert_term_to_mini_term(subject)
            pred = self._convert_term_to_mini_term(predicate)
            obj = self._convert_term_to_mini_term(object, predicate)
            cxt = self._contexts_to_ntriple_contexts(contexts)
            if isinstance(object, GeoSpatialRegion):
                if cxt is not None and cxt != ALL_CONTEXTS:
                    raise ValueError(
                        "Geospatial queries cannot be limited to a context."
                    )
                return self._getStatementsInRegion(
                    subj,
                    pred,
                    obj,
                    limit=limit,
                    offset=offset,
                    accept=accept,
                    callback=callback,
                )
            else:
                result = self._get_mini_repository().getStatements(
                    subj,
                    pred,
                    obj,
                    cxt,
                    infer=includeInferred,
                    limit=limit,
                    offset=offset,
                    tripleIDs=tripleIDs,
                    accept=accept,
                    callback=callback,
                )

            if output is None:
                return RepositoryResult(result, tripleIDs=tripleIDs)
            else:
                return None

    def getStatementsById(self, ids, output=None, output_format=RDFFormat.NQX):
        """
        Return all statements whose triple ID matches an ID in the list 'ids'.

        :param ids: List of statement ids.
        :type ids: Iterable[int]
        :param output: File path or a file-like object to write
                       the result to.
        :type output: str|file
        :param output_format: Serialization format for ``output``.
        :type output_format: RDFFormat
        :return: An iterator over requested statements
                 or ``None`` (if ``output`` is used).
        :rtype: RepositoryResult
        """
        with output_to(output) as out_file:
            callback = None if output is None else out_file.write
            accept = (
                None
                if output is None
                else RDFFormat.mime_type_for_format(output_format)
            )
            result = self._get_mini_repository().getStatementsById(
                ids, accept=accept, callback=callback
            )
        if output is None:
            return RepositoryResult(result, tripleIDs=False)
        else:
            return False

    def _getStatementsInRegion(
        self,
        subject,
        predicate,
        region,
        limit=None,
        offset=None,
        accept=None,
        callback=None,
    ):
        geoType = region.geoType
        miniGeoType = geoType._getMiniGeoType()
        common_args = {
            "limit": limit,
            "offset": offset,
            "accept": accept,
            "callback": callback,
        }

        if isinstance(region, GeoBox):
            if geoType.system == GeoType.Cartesian:
                stringTuples = self._get_mini_repository().getStatementsInsideBox(
                    miniGeoType,
                    predicate,
                    region.xMin,
                    region.xMax,
                    region.yMin,
                    region.yMax,
                    **common_args,
                )
            elif geoType.system == GeoType.Spherical:
                stringTuples = self._get_mini_repository().getStatementsInsideBox(
                    miniGeoType,
                    predicate,
                    region.yMin,
                    region.yMax,
                    region.xMin,
                    region.xMax,
                    **common_args,
                )
            else:
                raise ValueError("Unsupported coordinate system: %s" % geoType.system)

        elif isinstance(region, GeoCircle):
            if geoType.system == GeoType.Cartesian:
                stringTuples = self._get_mini_repository().getStatementsInsideCircle(
                    miniGeoType,
                    predicate,
                    region.x,
                    region.y,
                    region.radius,
                    **common_args,
                )
            elif geoType.system == GeoType.Spherical:
                stringTuples = self._get_mini_repository().getStatementsHaversine(
                    miniGeoType,
                    predicate,
                    region.x,
                    region.y,
                    region.radius,
                    unit=region.unit,
                    **common_args,
                )
            else:
                raise ValueError("Unsupported coordinate system: %s" % geoType.system)
        elif isinstance(region, GeoPolygon):
            stringTuples = self._get_mini_repository().getStatementsInsidePolygon(
                miniGeoType,
                predicate,
                self._convert_term_to_mini_term(region.getResource()),
                **common_args,
            )
        else:
            raise ValueError(
                "Unsupported region type: %s" % type(region)
            )  # can't happen

        if callback is None:
            return RepositoryResult(stringTuples, subjectFilter=subject)
        else:
            return None

    # NOTE: 'format' shadows a built-in symbol but it is too late to change the public API
    def add(
        self,
        arg0,
        arg1=None,
        arg2=None,
        contexts=None,
        base=None,
        format=None,
        serverSide=False,
        attributes=None,
    ):
        """
        Calls :meth:`.addTriple`, :meth:`.addStatement`, or :meth:`.addFile`.

        Best practice is to avoid ``add()`` and use :meth:`addTriple`, :meth:`addStatement`
        or :meth:`addFile` instead.

        :param arg0: May be a Statement or a filepath. If so, arg1 and arg2 default to None.
                     May also be the subject of a triple (in that case arg1 and arg2 must be
                     the predicate and the object respectively).
        :type arg0: Statement|string|Value
        :param arg1: Predicate of the new triple.
        :type arg1: string|Value
        :param arg2: Object of the new triple.
        :type arg2: string|Value
        :param contexts: Optional list of contexts (subgraph URIs), defaulting to None.
                         A context is the URI of a subgraph. If None, the triple(s) will
                         be added to the null context (the default or background graph).
        :type contexts: Iterable[URI|string]
        :param base: The baseURI to associate with loading a file.  Defaults to ``None``.
                     If ``None`` the baseURI will be chosen by the server.
        :type base: string
        :param format: Format of the uploaded file (e.g. RDFFormat.TURTLE or RDFFormat.NQUADS).
                       If set to ``None`` (default) the format will be selected based on the filename.
        :type format: RDFFormat
        :param serverSide: Indicates whether the filepath refers to a file on the client computer
                           or on the server.  Defaults to ``False`` (i.e. client-side).
        :type serverSide: bool
        :param attributes: Attributes for the new triple (a mapping from attribute names to values).
        :type attributes: dict[str, str]
        """
        if contexts and not isinstance(contexts, list):
            contexts = [contexts]
        if isinstance(arg0, (str, bytes, file)):
            if contexts:
                if len(contexts) > 1:
                    raise IllegalArgumentException(
                        "Only one context may be specified when loading from a file."
                    )
                context = contexts[0]
            else:
                context = None
            return self.addFile(
                arg0,
                base=base,
                format=format,
                context=context,
                serverSide=serverSide,
                attributes=attributes,
            )
        elif isinstance(arg0, Value):
            return self.addTriple(
                arg0, arg1, arg2, contexts=contexts, attributes=attributes
            )
        elif isinstance(arg0, Statement):
            return self.addStatement(arg0, contexts=contexts, attributes=attributes)
        elif hasattr(arg0, "__iter__"):
            for s in arg0:
                self.addStatement(s, contexts=contexts, attributes=attributes)
        else:
            raise IllegalArgumentException(
                "Illegal first argument to 'add'.  Expected a Value, Statement, File, or string."
            )

    # NOTE: 'format' shadows a built-in symbol but it is too late to change the public API
    def addFile(
        self,
        filePath,
        base=None,
        format=None,
        context=None,
        serverSide=False,
        content_encoding=None,
        attributes=None,
        json_ld_store_source=None,
        json_ld_context=None,
        allow_external_references=None,
        external_reference_timeout=None,
    ):
        """
        Loads a file into the triple store. Note that a file can be loaded into only one context.

        :param filepath: Identifies the file to load.
        :type filePath: string
        :param context: An optional context URI (subgraph URI), defaulting to ``None``.
                        If ``None``, the triple(s) will be added to the null context
                        (the default or background graph).
        :type context: URI|string|list[URI|string]
        :param base: The baseURI to associate with loading a file.  Defaults to ``None``.
                     If ``None`` the baseURI will be chosen by the server.
        :type base: string
        :param format: Format of the uploaded file (e.g. RDFFormat.TURTLE or RDFFormat.NQUADS).
                       If set to ``None`` (default) the format will be selected based on the filename.
        :type format: RDFFormat
        :param serverSide: Indicates whether the filepath refers to a file on the client computer
                           or on the server.  Defaults to ``False`` (i.e. client-side).
        :type serverSide: bool
        :param attributes: Attributes for all added triples (a mapping from attribute names to values).
        :type attributes: dict[str, str]
        :param json_ld_store_source: If set to true then a triple containing the whole uploaded
                                     document as a string will also be added to the store.
                                     The default is False.
        :type json_ld_store_source: bool
        :param json_ld_context: A JSON-LD context. This must be either a dictionary describing
                                a JSON object or a string containing the address of an external
                                context. The context can also be stored in the JSON-LD document,
                                either inline or as a reference to an external URI.
                                Note that a JSON-LD context is an object describing
                                the way in which a JSON document should be represented as triples.
                                It should not be confused with RDF contexts (i.e. graphs).
        :type json_ld_context: str|dict
        :param allow_external_references: If False (default) the server will refuse to
                                          retrieve external JSON-LD contexts and references
                                          from XML documents. Note that allowing
                                          untrusted documents to retrieve arbitrary URLs might
                                          have security implications.
        :type allow_external_references: bool
        :param external_reference_timeout: Timeout value (in seconds) used when retrieving
                                           external JSON-LD contexts.  By default there is
                                           no timeout.
        :type external_reference_timeout: int
        """
        if isinstance(context, (list, tuple)):
            if len(context) > 1:
                raise IllegalArgumentException(
                    "Multiple contexts passed to 'addFile': %s" % context
                )
            context = context[0] if context else None
        contextString = self._context_to_ntriples(context, none_is_mini_null=True)

        fmt, ce = RDFFormat.format_for_file_name(filePath)
        format = format or fmt
        content_encoding = content_encoding or ce

        self._get_mini_repository().loadFile(
            filePath,
            format,
            context=contextString,
            serverSide=serverSide,
            commitEvery=self.add_commit_size,
            baseURI=base,
            content_encoding=content_encoding,
            attributes=attributes,
            json_ld_store_source=json_ld_store_source,
            json_ld_context=json_ld_context,
            allow_external_references=allow_external_references,
            external_reference_timeout=external_reference_timeout,
        )

    def addData(
        self,
        data,
        rdf_format=None,
        base_uri=None,
        context=None,
        attributes=None,
        json_ld_store_source=None,
        json_ld_context=None,
        allow_external_references=None,
        external_reference_timeout=None,
    ):
        """
        Adds data from a string to the repository.

        :param data: Data to be added. Can be a string, a dictionary containing a JSON-LD
                     document or a list of such dictionaries. In addition to regular
                     JSON-serializable values a JSON-LD document might contain URI
                     and BNode objects as keys and all kinds of RDF terms (literals, URIs
                     and BNodes) as values.
        :type data: string|dict|list
        :param rdf_format: Data format - either a RDFFormat or a MIME type (string).
                           Defaults to RDFFormat.TURTLE, unless the data is given as
                           a dictionary, in which case the default is RDFFormat.JSONLD.
        :type rdf_format: RDFFormat|str
        :param base_uri: Base for resolving relative URIs.
                         If None (default), the URI will be chosen by the server.
        :type base_uri: string
        :param context: Graph to add the data to.
                        If None (default) the default graph will be used.
        :type context: URI|string
        :param attributes: Attributes for the added triples (a mapping from attribute names to values).
        :type attributes: dict[str, str]
        :param json_ld_store_source: If set to true then a triple containing the whole uploaded
                                     document as a string will also be added to the store.
                                     The default is False.
        :type json_ld_store_source: bool
        :param json_ld_context: A JSON-LD context. This must be either a dictionary describing
                                a JSON object or a string containing the address of an external
                                context. The context can also be stored in the JSON-LD document,
                                either inline or as a reference to an external URI.
                                Note that a JSON-LD context is an object describing
                                the way in which a JSON document should be represented as triples.
                                It should not be confused with RDF contexts (i.e. graphs).
        :type json_ld_context: str|dict
        :param allow_external_references: If False (default) the server will refuse to
                                          retrieve external JSON-LD contexts and references
                                          from XML documents. Note that allowing
                                          untrusted documents to retrieve arbitrary URLs might
                                          have security implications.
        :type allow_external_references: bool
        :param external_reference_timeout: Timeout value (in seconds) used when retrieving
                                           external JSON-LD contexts.  By default there is
                                           no timeout.
        :type external_reference_timeout: int
        """
        if isinstance(data, dict) or isinstance(data, list):
            # Technically dictionaries are only guaranteed to be ordered
            # on Python 3.7+, but in practice this is also true for
            # CPython 3.6.
            sort_keys = json_ld_store_source and sys.version_info < (3, 6)
            data = dump_json_ld(data, sort_keys=sort_keys)
            if rdf_format is None:
                rdf_format = RDFFormat.JSONLD

        if rdf_format is None:
            rdf_format = RDFFormat.TURTLE

        ctx = self._context_to_ntriples(context, none_is_mini_null=True)
        self._get_mini_repository().loadData(
            data,
            rdf_format,
            base_uri=base_uri,
            context=ctx,
            attributes=attributes,
            json_ld_store_source=json_ld_store_source,
            json_ld_context=json_ld_context,
            allow_external_references=allow_external_references,
            external_reference_timeout=external_reference_timeout,
        )

    def addDocumentFile(
        self,
        doc,
        doc_format=None,
        base=None,
        rules=None,
        subject=None,
        keys=None,
        prefix=None,
        rename=None,
        rdf_type=None,
        lang=None,
        skip=None,
        transform=None,
        graph=None,
        json_store_source=None,
        csv_dialect=None,
        csv_columns=None,
        csv_separator=None,
        csv_quote=None,
        csv_whitespace=None,
        csv_double_quote=None,
        csv_escape=None,
        attributes=None,
        commit=None,
        context=None,
        encoding=None,
        content_encoding=None,
    ):
        """
        Convert a JSON or CSV file into triples and add the resulting triples
        into the repository.

        Use :meth:`addDocumentData` instead if the document is stored in
        a string or in a Python list or dictionary.

        The document will be interpreted as a set of key-value pairs and a triple
        will be added to the repository for each such pair. For CSV documents
        the keys are column names. Mapping of JSON documents is somewhat more
        involved:

           - JSON documents must be objects or lists of objects.
           - If the value of a key is itself an object, that object is
             recursively interpreted as a document. The subject of resulting
             triples is used as the value associated with the original key.
           - If a value is a list then a triple is added for each element.
             Note that an RDF list is *not* created.
           - Multiple objects can be passed by using  `DocFormat.JSON_LINES`.
             In that case the input document should contain one JSON
             object per line (see http://jsonlines.org/).

        The subject of added triples is computed from the `subject` parameter,
        which is a template that can refer to other keys. If the parameter
        is not given then the subject will be a fresh blank node.

        There are multiple parameters that control the way in which values
        for a given key are turned into predicates and objects. These can be
        provided in two ways:

           - By passing a dictionary as the `keys` parameter. It must map
             strings (document key names) to :class:`DocumentKey` objects.

           - By passing dictionaries indexed by document key names in the
             `prefix`, `rename`, `rdf_type`, `lang`, `transform` and  `graph`
             parameters. The `skip` parameter may also be used (it must be
             a list of key names).

        If both methods are used to provide a particular setting, the value
        passed in the individual parameter takes precedence.

        :param doc: Path to the document to be imported.
        :type doc: str
        :param doc_format: Format of the document.
                           If not given it is guessed from the extension.
        :type doc_format: DocFormat
        :param base: Base URI that will be prepended to key names to create
                     predicate URIs. If not specified it is chosen by the server.
        :type base: URI|str
        :param rules: Subject of a set of triples that describe transformation rules
                      to be applied during import. See server documentation
                      for more details. This must be a string that will be
                      interpreted as a relative URI in the
                      `http://franz.com/ns/allegrograph/6.4/load-meta#`
                      namespace.
        :type rules: str
        :param subject: Template used to create the subject for added triples.
                        The template is a string that can refer to any
                        value in the document using the `${key}` notation.
                        A URI object can also be used as the subject.
                        URI objects are simply converted to strings, so
                        any dollar signs inside URIs will be interpreted
                        as key references.
        :type subject: URI|str
        :param keys: A dictionary mapping key names to transformation settings.
                     Settings for each key are described using
                     :class:`DocumentKey` objects. Each such object is a simple
                     named tuple in which all fields are optional. The meaning
                     of each field is the same as that of the corresponding
                     `addDocument` parameter.
        :type keys: dict[str,DocumentKey]
        :param prefix: A dictionary mapping key names to prefixes of predicates
                       that will be used to represent those keys. By default
                       the value of `base` is used.
        :type prefix: dict[str,str|URI]
        :param rename: A dictionary mapping key names to local names of
                       predicates that will be used to represent those keys.
                       By default the local name is the same as the key name.
                       It is also possible to pass full names here, as URI
                       objects. In that case any 'prefix' setting will be
                       ignored.
        :type prefix: dict[str,str|URI]
        :param rdf_type: A dictionary mapping key names to RDF datatypes
                         of corresponding values. The datatype can be a
                         URI or a string. A special value `'uri'` can be
                         used to make the object of the produced triple be
                         a URI rather than a literal. Other than that
                         full URIs and qualified names (e.g. xsd:date)
                         can be used as datatypes.
                         The default datatype is string, except that
                         booleans in JSON documents will be turned
                         into xsd:booleans and integers will be xsd:integers.
        :type rdf_type: dict[str,URI|str]
        :param lang: A dictionary mapping key names to RDF language tags
                     that will be associated with objects of the resulting
                     triples.
        :type lang: dict[str,str]
        :param skip: A list of keys for which no triple should be produced.
        :type skip: list[str]
        :param transform: A dictionary mapping key names to ways in which
                          values should be transformed or computed.
                          Values can be either template strings (using `$key`
                          or ${key} to reference values of any key) or names
                          of built-in transformation functions (e.g.
                          `'string-upcase'`). Note that it is possible
                          to use this argument to add a new, computed key
                          to the dataset. In addition, keys can also be
                          URI objects.
        :type rdf_type: dict[str|URI,str]
        :param graph: A dictionary mapping key names to graph URIs.
                      The triple (or triples, since values in a JSON
                      document can be lists) generated for a key will
                      be added to the graph specified here instead of
                      the default graph.
        :type graph: dict[str,URI|str]
        :param json_store_source: If this flag is set to True then
                                  an additional triple which stores the
                                  whole input document as a string is created.
                                  This only works for JSON documents.
        :type json_store_source: bool
        :param csv_dialect: A dialect object that specifies the syntax used
                            to parse CSV documents. Note that all options
                            specified in the dialect object can be overriden
                            by individual `csv_*` arguments.
                            This can be either a csv.Dialect instance
                            or the name of a registered dialect, such as 'excel'.
        :type csv_dialect: str|csv.Dialect
        :param csv_columns: List of column names to be used when parsing
                            a CSV document. If not given the first row of
                            the document will be used instead.
        :type csv_columns: list[str]
        :param csv_separator: Character used to separate CSV fields.
        :type csv_separator: str
        :param csv_quote: Quote character, used around CSV fields that
                          contain the separator character.
        :type csv_quote: str
        :param csv_whitespace: A string or list of characters that should
                               be treated as whitespace when parsing CSV.
                               Whitespace characters are stripped from the start
                               and end of each value.
        :type csv_whitespace: str|list[str]
        :param csv_double_quote: If true (default) quotes can be escaped inside
                                 quoted values by doubling the quote character.
        :type csv_double_quote: bool
        :param csv_escape: Character that can be used in CSV fields to escape
                           the next character. By default there is no such
                           character.
        :type csv_escape: str
        :param context: Graph to add the data to.
                        If None (default) the default graph will be used.
        :type context: URI|string
        :param commit: If specified then a commit will occur every time
                       the specified number of triples has been generated.
                       This prevents excessive memory usage when importing
                       large files, but makes the import non-transactional.
        :type commit: int
        :param attributes: Attributes for the added triples (a mapping from attribute names to values).
        :type attributes: dict[str, str]
        :param encoding: Character encoding used by the input file.
                         By default the file is interpreted as UTF-8.
        :type encoding: str
        :param content_encoding: Compression format, supported values are 'gzip' or 'none'.
                                 By default the format is guessed from file extension.
        :type content_encoding: str
        """
        args = locals()
        del args["self"]
        fmt, ce = DocFormat.format_for_file_name(doc)
        args["doc_format"] = doc_format or fmt
        content_encoding = content_encoding or ce
        args["content_encoding"] = (
            content_encoding if content_encoding != "none" else None
        )
        with open(doc, "rb") as f:
            args["doc"] = f
            self._addDocument(**args)

    def addDocumentData(
        self,
        doc,
        doc_format=DocFormat.JSON,
        base=None,
        rules=None,
        subject=None,
        keys=None,
        prefix=None,
        rename=None,
        rdf_type=None,
        lang=None,
        skip=None,
        transform=None,
        graph=None,
        json_store_source=None,
        csv_dialect=None,
        csv_columns=None,
        csv_separator=None,
        csv_quote=None,
        csv_whitespace=None,
        csv_double_quote=None,
        csv_escape=None,
        attributes=None,
        commit=None,
        context=None,
    ):
        """
        Convert a JSON or CSV document string into triples and add the resulting triples
        into the repository.

        The first argument must be a string, a dict (will be converted to JSON)
        or a list of dicts (converted to JSON lines).
        The second argument describes the document format, the default is JSON.
        Note that if the first argument is a dict or a list the format argument
        will be ignored.

        See :meth:`addDocument` for the description of other parameters.
        """
        # Do not send a request for empty documents
        if not doc:
            return
        args = locals()
        del args["self"]
        if isinstance(doc, dict):
            args["doc"] = encode_json(doc)
            args["doc_format"] = DocFormat.JSON
        elif isinstance(doc, list):
            args["doc"] = "\n".join(encode_json(d) for d in doc)
            args["doc_format"] = DocFormat.JSON_LINES
        self._addDocument(**args)

    # Make sure the arguments here match the two methods above.
    def _addDocument(
        self,
        doc,
        doc_format=None,
        base=None,
        rules=None,
        subject=None,
        keys=None,
        prefix=None,
        rename=None,
        rdf_type=None,
        lang=None,
        skip=None,
        transform=None,
        graph=None,
        json_store_source=None,
        csv_dialect=None,
        csv_columns=None,
        csv_separator=None,
        csv_quote=None,
        csv_whitespace=None,
        csv_double_quote=None,
        csv_escape=None,
        attributes=None,
        commit=None,
        context=None,
        encoding=None,
        content_encoding=None,
    ):
        args = locals()

        # Unpack 'keys' into individual dicts
        augmented = {}
        if keys:
            for name, props in keys.items():
                for prop in (
                    "prefix",
                    "rename",
                    "rdf_type",
                    "lang",
                    "transform",
                    "graph",
                ):
                    value = getattr(props, prop)
                    if value is not None:
                        if prop not in augmented:
                            augmented[prop] = args[prop].copy() if args[prop] else {}
                        # Do not overwrite if we already have a value
                        if name not in augmented[prop]:
                            augmented[prop][name] = value
                if props.skip:
                    if "skip" not in augmented:
                        augmented["skip"] = skip[:] if skip else []
                    augmented["skip"].append(name)

        # Unpack the CSV dialect, but existing csv_* arguments have precedence
        if csv_dialect:
            if isinstance(csv_dialect, str):
                csv_dialect = csv.get_dialect(csv_dialect)
            if csv_separator is None:
                augmented["csv_separator"] = csv_dialect.delimiter
            if csv_quote is None:
                augmented["csv_quote"] = csv_dialect.quotechar
            if csv_whitespace is None:
                if csv_dialect.skipinitialspace:
                    augmented["csv_whitespace"] = string.whitespace
                else:
                    # The server uses tab and space as defaults
                    augmented["csv_whitespace"] = ""
            if csv_double_quote is None:
                augmented["csv_double_quote"] = csv_dialect.doublequote
            if csv_escape is None:
                augmented["csv_escape"] = csv_dialect.escapechar

        args.update(augmented)

        del args["csv_dialect"]
        del args["keys"]
        del args["self"]
        self._get_mini_repository().loadDocument(**args)

    def getGeneration(self):
        """
        Get the current DB generation.

        :return: Generation
        :rtype: int
        """
        return self._get_mini_repository().getGeneration()

    def addTriple(self, subject, predicate, object, contexts=None, attributes=None):
        """
        Add a single triple to the repository.

        :param subject: Subject of the new triple.
        :type subject: Value
        :param predicate: Predicate of the new triple.
        :type predicate: Value
        :param object: Object of the new triple.
        :type object: Value
        :param contexts: List of contexts (graph URIs) to add the triple to.
                         Defaults to ``None``, which adds the statement to the
                         null context (the default or background graph).
        :type contexts: Iterable[string|URI]
        :param attributes: Attributes for the added triple (a mapping from attribute names to values).
        :type attributes: dict[str, str]
        """
        obj = self.getValueFactory().object_position_term_to_openrdf_term(
            object, predicate=predicate
        )
        cxts = self._contexts_to_ntriple_contexts(contexts, none_is_mini_null=True)
        for cxt in cxts:
            self._get_mini_repository().addStatement(
                self._to_ntriples(subject),
                self._to_ntriples(predicate),
                self._convert_term_to_mini_term(obj),
                cxt,
                attributes=attributes,
            )

    def _to_ntriples(self, term):
        """
        If 'term' is an OpenRDF term, convert it to a string.  If it's already
        a string, assume it's in ntriples format, and just pass it through.
        If the term is None, return None.
        Otherwise convert `term` to Literal and make a string from that.
        """
        if term is None or isinstance(term, (str, bytes)):
            return term
        elif hasattr(term, "toNTriples"):
            return term.toNTriples()
        else:
            return Literal(term).toNTriples()

    def addTriples(
        self, triples_or_quads, context=None, ntriples=False, attributes=None
    ):
        """
        Add the supplied triples or quads to this repository.

        Each triple or quad can have between 3 and 5 elements, the last one being
        the dictionary of attributes (or None if the default attributes should be used).

        :param triples_or_quads: List of triples or quads. Each element can be
                                 either a statement or a list or tuple of :class:`Value` objects
                                 or strings.
        :type triples_or_quads: Iterable[list[string|Value]|tuple[string|Value]|Statement]
        :param context: Context (graph) or list of contexts to add the triples to.
                        Defaults to None (the default graph). Note that this will
                        be ignored for all input quads that already specify a context.
        :type context: string|URI|list[string|URI]
        :param ntriples: If ``True``, parts of the triples are assumed to be strings
                         in N-Triples format and are sent to the server without any
                         conversion.
        :type ntriples: bool
        :param attributes: Default attributes for the added triples (a mapping from attribute
                           names to values). These will be used only for statements that do not
                           contain their own attribute dictionaries.
        :type attributes: dict[str, str]
        """
        quads = []
        for q in triples_or_quads:
            # Note: Statement objects will work here, since they have a length
            # and support accessing components by index.
            is_quad = len(q) >= 4
            quad_attributes = q[4] if len(q) >= 5 and q[4] is not None else attributes
            if ntriples:
                s = q[0]
                p = q[1]
                o = q[2]
                gs = q[3] if is_quad and q[3] else context
            else:
                predicate = q[1]
                obj = self.getValueFactory().object_position_term_to_openrdf_term(
                    q[2], predicate=predicate
                )
                s = self._to_ntriples(q[0])
                p = self._to_ntriples(predicate)
                o = self._to_ntriples(obj)
                gs = [self._to_ntriples(q[3])] if is_quad and q[3] else context
            # We try to avoid passing the fifth element, since it might confuse older AG servers.
            if gs is None:
                if quad_attributes:
                    quads.append((s, p, o, None, encode_json(quad_attributes)))
                else:
                    quads.append((s, p, o, None))
            else:
                ntriple_contexts = self._contexts_to_ntriple_contexts(
                    gs, none_is_mini_null=True
                )
                if quad_attributes:
                    for g in ntriple_contexts:
                        quads.append((s, p, o, g, encode_json(quad_attributes)))
                else:
                    for g in ntriple_contexts:
                        quads.append((s, p, o, g))
        self._get_mini_repository().addStatements(
            quads, commitEvery=self.add_commit_size
        )

    def addStatement(self, statement, contexts=NOT_GIVEN, attributes=None):
        """
        Add the supplied statement to the specified contexts in the repository.

        :param statement: The statement to be added. Note that the ``context`` field is
                          ignored (use the ``contexts`` parameters instead).
        :type statement: Statement
        :param contexts: List of contexts (graph URIs) to add the statement to.
                         If present this overrides the context from the statement object.
                         The default context is ``None``, which adds the statement to the
                         null context (the default or background graph).
        :type contexts: Iterable[string|URI]
        :param attributes: Attributes for the added triple (a mapping from attribute names to values).
                           If present this overrides the attributes passed in the statement object.
        :type attributes: dict[str, str]
        """
        if contexts is NOT_GIVEN:
            ctx = statement.getContext()
            contexts = [ctx] if ctx is not None else None
        self.addTriple(
            statement.getSubject(),
            statement.getPredicate(),
            statement.getObject(),
            contexts=contexts,
            attributes=attributes,
        )

    def remove(self, arg0, arg1=None, arg2=None, contexts=None):
        """
        Call :meth:`removeTriples` or :meth:`removeStatement`.

        Best practice is to avoid :meth:`remove` and use :meth:`removeTriples`
        or :meth:`removeStatement` directly.

        Note that ``context`` fields of statements passed to this method are ignored.

        ``arg0`` may be a :class:`Statement`.  If so, then ``arg1`` and ``arg2`` default to None.

        ``arg0``, ``arg1``, and ``arg2`` may be the subject, predicate and object of a triple.

        :param arg0: Either a :class:`Statement`, a list of statements or the subject of a triple.
        :type arg0: Statement|Value|Iterable[Statement]
        :param arg1: Predicate of a triple.
        :type arg1: URI
        :param arg2: Object of a triple.
        :type arg2: Value
        :param contexts: An optional list of graphs to remove triples from.
        :type contexts: Iterable[URI|string]
        """
        if contexts and not isinstance(contexts, list):
            contexts = [contexts]
        if isinstance(arg0, Value) or arg0 is None:
            self.removeTriples(arg0, arg1, arg2, contexts=contexts)
        elif isinstance(arg0, Statement):
            self.removeStatement(arg0, contexts=contexts)
        elif hasattr(arg0, "__iter__"):
            for s in arg0:
                self.removeStatement(s, contexts=contexts)
        else:
            raise IllegalArgumentException(
                "Illegal first argument to 'remove'.  Expected a Value, Statement, or iterator."
            )

    def removeTriples(self, subject, predicate, object, contexts=ALL_CONTEXTS):
        """
        Remove the statement(s) with the specified subject, predicate and object
        from the repository, optionally restricted to the specified contexts.

        :param subject: Subject of the triples to be removed.
        :type subject: Value|None
        :param predicate: Predicate of the triples to be removed.
        :type predicate: URI|None
        :param object: Object of the triples to be removed.
        :type object: Value|None
        :param contexts: An optional list of graphs to remove triples from.
        :type contexts: Iterable[URI|string]|None
        """
        subj = self._to_ntriples(subject)
        pred = self._to_ntriples(predicate)
        obj = self._to_ntriples(
            self.getValueFactory().object_position_term_to_openrdf_term(object)
        )
        ntripleContexts = self._contexts_to_ntriple_contexts(
            contexts, none_is_mini_null=True
        )
        if ntripleContexts is None or len(ntripleContexts) == 0:
            self._get_mini_repository().deleteMatchingStatements(subj, pred, obj, None)
        else:
            for cxt in ntripleContexts:
                self._get_mini_repository().deleteMatchingStatements(
                    subj, pred, obj, cxt
                )

    def removeQuads(self, quads, ntriples=False):
        """
        Remove enumerated quads from this repository.  Each quad can
        be a list or a tuple of :class:`Value` objects.

        :param quads: List of quads. Each element can be
                      either a statement or a list or tuple of :class:`Value` objects
                      or strings.
        :type quads: Iterable[list[string|Value]|tuple[string|Value]]
        :param ntriples: If ``True``, parts of the quads are assumed to be strings
                         in N-Triples format and are sent to the server without any
                         conversion.
        :type ntriples: bool
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
                obj = self.getValueFactory().object_position_term_to_openrdf_term(
                    q[2], predicate=predicate
                )
                quad[0] = self._to_ntriples(q[0])
                quad[1] = self._to_ntriples(predicate)
                quad[2] = self._to_ntriples(obj)
                quad[3] = self._to_ntriples(q[3])
            else:  # must be a statement
                predicate = q.getPredicate()
                obj = self.getValueFactory().object_position_term_to_openrdf_term(
                    q.getObject(), predicate=predicate
                )
                quad[0] = self._to_ntriples(q.getSubject())
                quad[1] = self._to_ntriples(predicate)
                quad[2] = self._to_ntriples(obj)
                quad[3] = self._to_ntriples(q.getContext())
            removeQuads.append(quad)
        self._get_mini_repository().deleteStatements(removeQuads)

    def removeQuadsByID(self, tids):
        """
        Remove all quads with matching IDs.

        :param tids: List of IDs to be removed.
        :type tids: list[int]
        """
        self._get_mini_repository().deleteStatementsById(tids)

    def removeStatement(self, statement, contexts=None):
        """
        Remove the supplied statement from the specified contexts in the repository.

        :param statement: Statement to be removed. Note that the ``context`` field of this object
                          will be ignored.
        :type statement: Statement
        :param contexts: An optional list of graphs to remove the statement from.
        :type contexts: Iterable[URI|string]
        """
        self.removeTriples(
            statement.getSubject(),
            statement.getPredicate(),
            statement.getObject(),
            contexts=contexts,
        )

    def clear(self, contexts=ALL_CONTEXTS):
        """
        Removes all statements from designated contexts in the repository.  If
        ``contexts`` is ALL_CONTEXTS, clears the repository of all statements.

        :param contexts: A context or list of contexts.
        :type contexts: Iterable[string|URI]|string|URI
        """
        self.removeTriples(None, None, None, contexts=contexts)

    def export(self, handler, contexts=ALL_CONTEXTS):
        """
        Export all explicit statements in the specified contexts to a file.

        .. deprecated:: 4.14.1
           Use :meth:`saveResponse` instead.

        :param handler: An RDFWriter instance describing the target file and data format.
        :type handler: RDFWriter
        :param contexts: A context or list of contexts (default: all contexts).
        :type contexts: Iterable[string|URI]|string|URI
        """
        warnings.warn(
            "export is deprecated. Use saveResponse instead.",
            DeprecationWarning,
            stacklevel=2,
        )
        self.exportStatements(None, None, None, False, handler, contexts=contexts)

    def exportStatements(
        self, subj, pred, obj, includeInferred, handler, contexts=ALL_CONTEXTS
    ):
        """
        Export statements to a file.

        .. deprecated:: 4.14.1
           Use :meth:`saveResponse` instead.

        :param subj: Subject or ``None`` (i.e. no filtering on subject).
        :type subj: Value
        :param pred: Predicate or ``None`` (i.e. no filtering on predicate).
        :type pred: URI
        :param obj: Object or ``None`` (i.e. no filtering on object).
        :type obj: Value
        :param includeInferred: If ``True`` inferred triples will be included
                                in the output. The default is ``False``.
        :type includeInferred: bool
        :param handler: An RDFWriter instance describing the target file and data format.
        :type handler: RDFWriter
        :param contexts: A context or list of contexts (default: all contexts).
        :type contexts: Iterable[string|URI]|string|URI
        """
        warnings.warn(
            "exportStatements is deprecated. Use getStatements(output=...) instead.",
            DeprecationWarning,
            stacklevel=2,
        )
        self.getStatements(
            subj,
            pred,
            obj,
            contexts,
            output=handler.getFilePath() or sys.stdout,
            output_format=handler.getRDFFormat(),
            includeInferred=includeInferred,
        )

    def getSubjectTriplesCacheSize(self):
        """
        Return the current size of the subject triples cache.

        .. seealso::

           :meth:`.enablerSubjectTriplesCache`
        """
        return self._get_mini_repository().getTripleCacheSize()

    def disableSubjectTriplesCache(self):
        """
        Disable the subject triples cache (see :meth:`.enableSubjectTriplesCache`).
        """
        self._get_mini_repository().disableTripleCache()

    def enableSubjectTriplesCache(self, size=None):
        """
        Maintain a cache of size ``size`` that caches, for each accessed
        resource, quads where the resource appears in subject position.

        This can accelerate the performance of certain types of queries.

        :param size: The maximum number of subjects whose triples will be cached.
                     Default is 100,000.
        :type size: int
        """
        self._get_mini_repository().enableTripleCache(size=size)

    ## Indexing control methods

    def listIndices(self):
        """
        Return the list of the current set of triple indices.

        Index names are strings such as ``"gospi"``, ``"spogi"`` etc.
        Use :meth:`.listValidIndices` to get the list of index names
        supported by the server.

        :return: List of index names (see :meth:`.listValidIndices`).
        :rtype: list[string]
        """
        return self._get_mini_repository().listIndices()

    def listValidIndices(self):
        """
        Return the list of valid index names.

        :return: List of index names.
        :rtype: list[string]
        """
        return self._get_mini_repository().listValidIndices()

    def addIndex(self, _type):
        """
        Add a specific type of index to the current set of triple indices.

        :param _type: Index name (see :meth:`.listIndices`).
        :type _type: string
        """
        return self._get_mini_repository().addIndex(_type)

    def dropIndex(self, _type):
        """
        Removes a specific type of index to the current set of triple indices.

        :param _type: Index name (see :meth:`.listIndices`).
        :type _type: string
        """
        return self._get_mini_repository().dropIndex(_type)

    def optimizeIndices(self, level=None, wait=None):
        """
        Optimize indices.

        Please see documentation for argument values and meanings:

        https://franz.com/agraph/support/documentation/current/triple-index.html#optimize
        """
        return self._get_mini_repository().optimizeIndices(level, wait)

    #############################################################################################
    ## ValueFactory methods
    ## Added here because its inconvenient to have to dispatch from three different objects
    ## (connection, repository, and value factory) when one will do
    #############################################################################################

    def registerDatatypeMapping(self, predicate=None, datatype=None, nativeType=None):
        """
        Register an inlined datatype.

        See :meth:`.Repository.registerDatatypeMapping`.
        """
        return self.repository.registerDatatypeMapping(
            predicate=predicate, datatype=datatype, nativeType=nativeType
        )

    def createLiteral(self, value, datatype=None, language=None):
        """
        Create a new literal with value ``value``.  ``datatype``, if supplied,
        should be a URI (it can be a string or an :class:`.URI` instance, in which
        case ``value`` must be a string.

        :param value: Literal value - can be a string, a number or a datetime object.
        :type value: int|long|float|string|datetime.datetime|datetime.date|datetime.time
        :param datatype: Optional literal type. Note that if ``value`` is not a string
                         the type will be guessed automatically.
        :type datatype: string|URI
        :param language: Optional language tag.
        :type language: string
        """
        return self.getValueFactory().createLiteral(
            value, datatype=datatype, language=language
        )

    def createURI(self, uri=None, namespace=None, localname=None, canonical=True):
        """
        Creates a new URI from the supplied string-representation(s).
        If two non-keyword arguments are passed, assumes they represent a
        namespace/localname pair.

        :param uri: URI text, unless namespace is passed and localname is not,
                    in which case this becomes the namespace.
        :type uri: string
        :param namespace: Namespace part of the URI if ``localname`` is passed,
                          otherwise this becomes the local name.
        :type namespace: string|URI
        :param localname: Local part of the URI. Should only be used as a
                          keyword argument and together with ``namespace``.
        :param canonical: If true (default) ensure that the same URI object
                          is returned each time when the same string is
                          passed to this method.
        :type canonical: bool
        :return: An URI object.
        :rtype: URI
        """
        return self.getValueFactory().createURI(
            uri=uri, namespace=namespace, localname=localname, canonical=canonical
        )

    def createBNode(self, nodeID=None):
        """
        Create a new blank node.

        :param nodeID: Optional node identifier, if not given a fresh one will be generated.
        :type nodeID: string
        :return: A BNode value.
        :rtype: BNode
        """
        return self.getValueFactory().createBNode(nodeID=nodeID)

    def createQuotedTriple(self, subject, predicate, object):
        """
        Create a new quoted triple (not to be confused with the Statement type).

        :param subject: Subject of the new quoted triple.
        :type subject: Resource
        :param predicate: Predicate of the new quoted triple.
        :type predicate: Resource
        :param object: Object of the new quoted triple.
        :type object: Resource
        :return: A Triple value.
        :rtype: Triple
        """
        if not self.RDFStarEnabled():
            raise ServerException("RDF-Star mode must be enabled from the server side.")
        return self.getValueFactory().createQuotedTriple(subject, predicate, object)

    def createStatement(self, subject, predicate, object, context=None):
        """
        Create a new Statement object.

        Note that this does *not* cause the statement to be added to the repository.

        :param subject: Subject of the new statement.
        :type subject: Resource
        :param predicate: Predicate of the new statement.
        :type predicate: URI
        :param object: Object of the new statement.
        :type object: Value
        :param context: Graph of the new statement (optional).
        :type context: URI
        :return: A new statement.
        :rtype: Statement
        """
        return self.getValueFactory().createStatement(
            subject, predicate, object, context=context
        )

    def createRange(self, lowerBound, upperBound):
        """
        Create a compound literal representing a range from lowerBound to upperBound.

        :param lowerBound: Lower bound of the range.
        :type lowerBound: int
        :param upperBound: Upper bound of the range.
        :type upperBound: int
        :return: A new literal object.
        :rtype: RangeLiteral
        """
        return self.getValueFactory().createRange(
            lowerBound=lowerBound, upperBound=upperBound
        )

    def addRules(self, rules, language=QueryLanguage.PROLOG):
        """
        Add Prolog functors to the current session.

        Note that this only works with a dedicated session (See :meth:`.session`.

        .. seealso::

           http://franz.com/agraph/support/documentation/current/prolog-tutorial.html
              Tutorial describing the syntax of Prolog rules (functors).

        :param rules: A string containing Prolog rule definitions. The definitions
                      are S-expressions using the ``<-`` and ``<--`` operators
                      (to append and overwrite rules respectively).
        :type rules: string
        :param language: Ignored.
        """
        del language
        if not self._get_mini_repository().sessionAlive:
            raise Exception("Prolog rules can only be added in a dedicated session.")
        self._get_mini_repository().definePrologFunctors(rules)

    def loadRules(self, filename, language=QueryLanguage.PROLOG):
        """
        Add Prolog rules from file to the current session.

        Note that this only works with a dedicated session.

        :param filename: Path to the file containing Prolog rules.
        :type filename: string
        :param language: Ignored.
        """
        with open(filename) as _file:
            body = _file.read()
        self.addRules(body, language)

    #############################################################################################
    ## Server-side implementation of namespaces
    #############################################################################################

    def getNamespaces(self):
        """
        Get all declared prefix/namespace pairs.

        The result is a dictionary mapping prefixes to namespace URIs.

        :return: A dictionary of namespaces.
        :rtype: dict[string,string]
        """
        namespaces = {}
        for pair in self._get_mini_repository().listNamespaces():
            namespaces[pair["prefix"]] = pair["namespace"]
        return namespaces

    def getNamespace(self, prefix):
        """
        Get the namespace that is associated with the specified prefix, if any.

        :param prefix: Namespace prefix.
        :return: Namespace URI.
        :raises RequestError: if there is no namespace with given prefix.
        """
        return self._get_mini_repository().getNamespace(prefix)

    def setNamespace(self, prefix, name):
        """
        Define or redefine a namespace mapping in the repository.

        :param prefix: Namespace prefix.
        :type prefix: string
        :param name: Namespace URI.
        :type name: string
        """
        self._get_mini_repository().addNamespace(prefix, name)

    def removeNamespace(self, prefix):
        """
        Remove a namespace declaration by removing the association between a
        prefix and a namespace name.

        :param prefix: Namespace prefix.
        :type prefix: string
        """
        self._get_mini_repository().deleteNamespace(prefix)

    def clearNamespaces(self, reset=True):
        """
        Delete all namespaces in this repository for the current user.

        :param reset: If ``True`` (default) the user's namespaces are reset
                      to the default set of namespaces, otherwise all namespaces
                      are cleared.
        """
        self._get_mini_repository().clearNamespaces(reset)

    #############################################################################################
    ## Server-side implementation of query options
    #############################################################################################

    def getQueryOptions(self):
        """
        Get pairs option/value for all enabled query options.

        The result is a dictionary mapping option names to values.

        :return: A dictionary of query options.
        :rtype: dict[string,string]
        """
        query_options = {}
        for pair in self._get_mini_repository().listQueryOptions():
            query_options[pair["name"]] = pair["value"]
        return query_options

    def getQueryOption(self, name):
        """
        Get the current value for the query option, if any.

        :param name: Query option name.
        :return: Query option value.
        :raises RequestError: if the given query option is not set.
        """
        return self._get_mini_repository().getQueryOption(name)

    def setQueryOption(self, name, value):
        """
        Set a query option value for all queries in this repository for the
        current user.

        :param name: Query option name.
        :type name: string
        :param value: Query option value.
        :type value: string
        :raises RequestError: if given option name is not a defined query option.
        """
        self._get_mini_repository().setQueryOption(name, "{}".format(value))

    def removeQueryOption(self, name):
        """
        Remove a namespace declaration by removing the association between a
        prefix and a namespace name.

        :param prefix: Query option name.
        :type prefix: string
        """
        self._get_mini_repository().deleteQueryOption(name)

    def clearQueryOptions(self):
        """
        Delete all query options in this repository for the current user.
        """
        self._get_mini_repository().clearQueryOptions()

    #############################################################################################
    ## RDF-Star
    #############################################################################################

    def RDFStarEnabled(self):
        """
        Return True if RDF-star semantics have been enabled; return False otherwise.
        """
        return self._get_mini_repository().RDFStarEnabled()

    def enableRDFStar(self):
        """
        Enable the RDF-star semantics for the current Repository.
        """
        self._get_mini_repository().enableRDFStar()

    def disableRDFStar(self):
        """
        Disable the RDF-star semantics for the current Repository.
        """
        self._get_mini_repository().disableRDFStar()

    #############################################################################################
    ## Geo-spatial
    #############################################################################################

    def createRectangularSystem(
        self, scale=1, unit=None, xMin=0, xMax=None, yMin=0, yMax=None
    ):
        """
        Create a Cartesian coordinate system and use it as the current coordinate system.

        :param scale: Estimate of the Y size of a typical search region.
        :type scale: float|int
        :param unit: Must be ``None`` (the default).
        :param xMin: Left edge of the rectangle.
        :type xMin: float|int
        :param xMax: Right edge of the rectangle.
        :type xMax: float|int
        :param yMin: Bottom edge of the rectangle.
        :type yMin: float|int
        :param yMax: Top edge of the rectangle.
        :type yMax: float|int
        :return: The new coordinate system.
        """
        self.geoType = GeoType(
            GeoType.Cartesian,
            scale=scale,
            unit=unit,
            xMin=xMin,
            xMax=xMax,
            yMin=yMin,
            yMax=yMax,
        )
        ##,latMin=None, latMax=None, longMin=None, longMax=None)
        self.geoType.setConnection(self)
        return self.geoType

    def createLatLongSystem(
        self,
        unit="degree",
        scale=None,
        latMin=None,
        latMax=None,
        longMin=None,
        longMax=None,
    ):
        """
        Create a spherical coordinate system and use it as the current coordinate system.

        :param scale: Estimate of the size of a typical search region in the latitudinal direction.
        :type scale: float|int
        :param unit: One of: ``'degree'``, ``'mile'``, ``'radian'``, ``'km'``.
                     The default is ``'degree'``.
        :type unit: string
        :param longMin: Left side of the coordinate system.
        :type longMin: float|int
        :param longMax: Right side of the coordinate system.
        :type longMax: float|int
        :param latMin: Bottom border of the coordinate system.
        :type latMin: float|int
        :param latMax: Top border of the coordinate system.
        :type latMax: float|int
        :return: The new coordinate system.
        """
        self.geoType = GeoType(
            GeoType.Spherical,
            unit=unit,
            scale=scale,
            latMin=latMin,
            latMax=latMax,
            longMin=longMin,
            longMax=longMax,
        )
        self.geoType.setConnection(self)
        return self.geoType

    def getGeoType(self):
        """
        Get the current geospatial coordinate system.

        :return: A coordinate system.
        :rtype: GeoType
        """
        return self.geoType

    def setGeoType(self, geoType):
        """
        Set the current geospatial coordinate system.

        :param geoType: The new coordinate system.
        :type geoType: GeoType
        """
        self.geoType = geoType
        geoType.setConnection(self)

    def createCoordinate(self, x=None, y=None, latitude=None, longitude=None):
        """
        Create an x, y or latitude, longitude  coordinate in the current coordinate system.

        Either ``x``, ``y`` or ``latitude``, ``longitude`` must be given.

        :param x: X coordinate of the created point.
        :param y: Y coordinate of the created point.
        :param latitude: Latitude of the created point.
        :param longitude: Longitude of the created point.
        :return: A literal representing the created point.
        :rtype: Literal
        """
        return self.geoType.createCoordinate(
            x=x, y=y, latitude=latitude, longitude=longitude
        )

    def createBox(self, xMin=None, xMax=None, yMin=None, yMax=None):
        """
        Create a rectangular search region (a box) for geospatial search.
        This method works for both Cartesian and spherical coordinate systems.

        :param xMin: Minimum latitude.
        :type xMin: float|int
        :param xMax: Maximum latitude.
        :type xMax: float|int
        :param yMin: Minimum longitude.
        :type yMin: float|int
        :param yMax: Maximum longitude.
        :type yMax: float|int
        :return: A geospatial literal corresponding to the specified search region.
        :rtype: Literal
        """
        return self.geoType.createBox(xMin=xMin, xMax=xMax, yMin=yMin, yMax=yMax)

    def createCircle(self, x, y, radius, unit=None):
        """
        Create a circular search region for geospatial search.

        This method works for both Cartesian and spherical coordinate systems.

        :param x: Latitude of the center.
        :type x: float|int
        :param y: Longitude of the center.
        :type y: float|int
        :param radius: The radius of the circle expressed in the designated ``unit``.
        :type radius: float|int
        :param unit: Unit in which the ``radius`` is expressed.
                     Defaults to the unit assigned to the coordinate system.
                     Legal values are ``"degree"``, ``"radian"``, ``"km"`` and ``"mile"``.
        :type unit: string
        """
        return self.geoType.createCircle(x, y, radius, unit=unit)

    def createPolygon(self, vertices, uri=None, geoType=None):
        """
        Define a polygonal region with the specified vertices.

        :param vertices: List of x, y pairs.
        :type vertices: list[(float, float)]
        :param uri: URI under which the polygon will be stored in the repository.
                    If not given a blank node will be used.
        :type uri: URI
        :param geoType: unused
        :return: An object representing the newly created polygon.
        :rtype: GeoPolygon
        """
        del geoType
        return self.geoType.createPolygon(vertices, uri=uri)

    #############################################################################################
    ## SNA   Social Network Analysis Methods
    #############################################################################################

    def registerSNAGenerator(
        self, name, subjectOf=None, objectOf=None, undirected=None, generator_query=None
    ):
        """
        Create a new SNA generator named ``name``.
        If one already exists with the same name - redefine it.

        The edges traversed by the new generator can be defined in one of two ways:

           - Using three lists of predicates to define edges between subjects and
             objects of triples (``subjectOf``, ``objectOf``, ``undirected``).
           - Using a Prolog query (``generator_query``).

        Requires a dedicated session.

        :param name: Name of the new/modified generator.
        :type name: string
        :param subjectOf: Create object to subject edges for these predicates.
        :type subjectOf: list[URI|string]
        :param objectOf: Create subject to object edges for these predicates.
        :type objectOf: list[URI|string]
        :param undirected: Create edges in both directions for these predicates.
        :type undirected: list[URI|string]
        :param generator_query: A Prolog query that generates neighbors given
                                a start node ``?node``.
        """
        miniRep = self._get_mini_repository()
        miniRep.registerSNAGenerator(
            name,
            subjectOf=subjectOf,
            objectOf=objectOf,
            undirected=undirected,
            query=generator_query,
        )

    def registerNeighborMatrix(self, name, generator, group_uris, max_depth=2):
        """
        Construct a neighbor matrix named ``name``.

        The generator named ``generator`` is applied to each URI in ``group_uris``,
        computing edges to max depth ``max_depth``.

        Requires a dedicated session.

        :param name: Name that will be used to identify the matrix.
        :type name: string
        :param generator: Name of the SNA generator used to compute the edges.
                          See :meth:`.registerSNAGenerator`.
        :type generator: string
        :param group_uris: Initial list of vertices from which the graph will be created.
        :type group_uris: list[URI|string]
        :param max_depth: Max distance from the initial vertices.
        :type max_depth: int
        """
        miniRep = self._get_mini_repository()
        miniRep.registerNeighborMatrix(name, group_uris, generator, max_depth)

    def listFreeTextIndices(self):
        """
        Get the names of currently defined free-text indices.

        :return: List of index names.
        :rtype: list[string]
        """
        return self.mini_repository.listFreeTextIndices()

    def createFreeTextIndex(
        self,
        name,
        predicates=None,
        indexLiterals=None,
        indexResources=None,
        indexFields=None,
        minimumWordSize=None,
        stopWords=None,
        wordFilters=None,
        innerChars=None,
        borderChars=None,
        tokenizer=None,
    ):
        """
        Create a free-text index with the given parameters.

        .. seealso::

           http://franz.com/agraph/support/documentation/current/http-protocol.html#put-freetext-index
              Documentation of the HTTP endpoint used by this method.

           http://franz.com/agraph/support/documentation/current/text-index.html
              General information about text indexing in AllegroGraph.

        :param name: Name for the new index.
        :type name: string
        :param predicates: A list of predicates to be indexed. If not given all
                           triples will be indexed regardless of predicate.
        :type predicates: list[string|URI]
        :param indexLiterals: Determines which literals to index. It can be``True``
                              (the default), ``False``, or a list of strings,
                              indicating the literal types or langs that should be indexed.
        :type indexLiterals: bool|list[string]
        :param indexResources: Determines which resources are indexed. It can be
                               ``True``, ``False`` (the default), or ``"short"``,
                               to index only the part of resources after the last
                               slash or hash character.
        :type indexResources: bool|string
        :param indexFields: List of triples fields to index. Can be any combination
                            of ``"subject"``, ``"predicate"``, ``"object"``, and
                            ``"graph"``. The default is ``["object"]``.
        :type indexFields: list[string]
        :param minimumWordSize: Determines the minimum size a word must have to be
                                indexed. The default is 3.
        :type minimumWordSize: int
        :param stopWords: List of words that should not be indexed.
                          When not given, a list of common English words is used.
        :type stopWords: list[string]
        :param wordFilters: List of normalizing filters used to process words
                            before indexing. The list if supported filters can
                            be found here:
                            http://franz.com/agraph/support/documentation/current/text-index.html
        :type wordFilters: list[string]
        :param innerChars: The character set to be used as the constituent characters of a word.
                           Should be a list of character classes. Valid classes are:

                               - `"alpha"``: All (unicode) alphabetic characters.
                               - ``"digit"``: All base-10 digits.
                               - ``"alphanumeric"``: All digits and alphabetic characters.
                               - a single character
                               - a range of characters: A single character, followed by a dash (-)
                                 character, followed by another single character.
        :type innerChars: list[string]
        :param borderChars: The character set to be used as the border characters of indexed words.
                            Uses the same syntax as ``innerChars``.
        :type borderChars: list[string]
        :param tokenizer: Controls the way in which the text is plit into tokens.
                          Can be either ``"default"`` or ``"japanese"``.
                          Note that the ``japanese`` tokenizer ignores ``innerChars``
                          and ``borderChars``.
        :type tokenizer: string
        """
        if predicates:
            predicates = [uris.asURIString(p) for p in predicates]
        if isinstance(indexLiterals, list):
            indexLiterals = [uris.asURIString(l) for l in indexLiterals]
        self.mini_repository.createFreeTextIndex(
            name,
            predicates=predicates,
            indexLiterals=indexLiterals,
            indexResources=indexResources,
            indexFields=indexFields,
            minimumWordSize=minimumWordSize,
            stopWords=stopWords,
            wordFilters=wordFilters,
            innerChars=innerChars,
            borderChars=borderChars,
            tokenizer=tokenizer,
        )

    def modifyFreeTextIndex(
        self,
        name,
        predicates=None,
        indexLiterals=None,
        indexResources=None,
        indexFields=None,
        minimumWordSize=None,
        stopWords=None,
        wordFilters=None,
        reIndex=None,
        innerChars=None,
        borderChars=None,
        tokenizer=None,
    ):
        """
        Modify parameters of a free-text index.

        :param name: Name of the index to be modified.
        :type name: string
        :param predicates: A list of predicates to be indexed. If not given all
                           triples will be indexed regardless of predicate.
        :type predicates: list[string|URI]
        :param indexLiterals: Determines which literals to index. It can be``True``
                              (the default), ``False``, or a list of strings,
                              indicating the literal types or langs that should be indexed.
        :type indexLiterals: bool|list[string]
        :param indexResources: Determines which resources are indexed. It can be
                               ``True``, ``False`` (the default), or ``"short"``,
                               to index only the part of resources after the last
                               slash or hash character.
        :type indexResources: bool|string
        :param indexFields: List of triples fields to index. Can be any combination
                            of ``"subject"``, ``"predicate"``, ``"object"``, and
                            ``"graph"``. The default is ``["object"]``.
        :type indexFields: list[string]
        :param minimumWordSize: Determines the minimum size a word must have to be
                                indexed. The default is 3.
        :type minimumWordSize: int
        :param stopWords: List of words that should not be indexed.
                          When not given, a list of common English words is used.
        :type stopWords: list[string]
        :param wordFilters: List of normalizing filters used to process words
                            before indexing. The list if supported filters can
                            be found here:
                            http://franz.com/agraph/support/documentation/current/text-index.html
        :type wordFilters: list[string]
        :param innerChars: The character set to be used as the constituent characters of a word.
                           Should be a list of character classes. Valid classes are:

                               - `"alpha"``: All (unicode) alphabetic characters.
                               - ``"digit"``: All base-10 digits.
                               - ``"alphanumeric"``: All digits and alphabetic characters.
                               - a single character
                               - a range of characters: A single character, followed by a dash (-)
                                 character, followed by another single character.
        :type innerChars: list[string]
        :param borderChars: The character set to be used as the border characters of indexed words.
                            Uses the same syntax as ``innerChars``.
        :type borderChars: list[string]
        :param tokenizer: Controls the way in which the text is plit into tokens.
                          Can be either ``"default"`` or ``"japanese"``.
                          Note that the ``japanese`` tokenizer ignores ``innerChars``
                          and ``borderChars``.
        :type tokenizer: string
        """
        if predicates:
            predicates = [uris.asURIString(p) for p in predicates]
        if isinstance(indexLiterals, list):
            indexLiterals = [uris.asURIString(l) for l in indexLiterals]
        self.mini_repository.modifyFreeTextIndex(
            name,
            predicates=predicates,
            indexLiterals=indexLiterals,
            indexResources=indexResources,
            indexFields=indexFields,
            minimumWordSize=minimumWordSize,
            stopWords=stopWords,
            wordFilters=wordFilters,
            reIndex=reIndex,
            innerChars=innerChars,
            borderChars=borderChars,
            tokenizer=tokenizer,
        )

    def deleteFreeTextIndex(self, name):
        """
        Delete a free-text index from the server.

        :param name: Index name.
        :type name: string
        """
        self.mini_repository.deleteFreeTextIndex(name)

    def getFreeTextIndexConfiguration(self, name):
        """
        Get the current settings of a free-text index.

        The result will be a dictionary where keys are parameter names
        as used in :meth:`.createFreeTextIndex`.

        :param name: Index name.
        :type name: string
        :return: A dictionary with configuration data, keys are argument
                 names from :meth:`.createFreeTextIndex`.
        :rtype: dict
        """
        value = self.mini_repository.getFreeTextIndexConfiguration(name)
        value["predicates"] = [URI(p) for p in value["predicates"]]
        if isinstance(value["indexLiterals"], list):
            value["indexLiterals"] = [URI(l) for l in value["indexLiterals"]]
        return value

    def evalFreeTextSearch(
        self, pattern, infer=False, callback=None, limit=None, offset=None, index=None
    ):
        """
        Return a list of statements for the given free-text pattern search.

        Note that this method operates on unparsed data. Returned statements
        will be lists of strings in N-Triples format, not statement objects.
        The first element of each list will be the triple id
        (an integer, not a string).

        If no index is provided, all indices will be used.

        :param pattern: Search pattern, see
                        http://franz.com/agraph/support/documentation/current/http-protocol.html#get-post-freetext
                        for details.
        :type pattern: string
        :param index: List of indices to query. If not given - query all indices.
        :type index: string|list[string]
        :param callback: A function that will be called for each statement retrieved.
                         If this argument is used then the list of statements is not
                         returned.
        """
        miniRep = self._get_mini_repository()
        result = miniRep.evalFreeTextSearch(pattern, index, infer, limit, offset=offset)
        if callback:
            for row in result:
                callback(row)
            return None
        return result

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
            mini_repo = self._get_mini_repository(dedicated=True)
            mini_repo.openSession(autocommit, lifetime, loadinitfile)
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
        self.close()

    @contextmanager
    def session(self, autocommit=False, lifetime=None, loadinitfile=False):
        """
        A session context manager for use with the ``with`` statement:

        .. code:: python

            with conn.session():
                # Automatically calls openSession at block start
                # Do work
                # Automatically calls closeSession at block end
                # or in case of an exception.


        :param autocommit: if ``True``, commits are done on each request, otherwise
                           you will need to call :meth:`.commit` or :meth:`.rollback`
                           as appropriate for your application.
                           The default value is ``False``.
        :type autocommit: bool
        :param lifetime: Time (in seconds) before the session expires when idle.
                         Note that the client maintains a thread that ping the
                         session before this happens.
        :type lifetime: int
        :param loadinitfile: if ``True`` then the current initfile will be loaded
                             for you when the session starts. The default is ``False``.
        :type loadinitfile: bool
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

    # This is completely insane. Are we really suggesting that to the users!?

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

    def commit(self, settings=None, **kwargs):
        """
        Commit changes on an open session.

        Parameters can be used to control distributed transaction settings
        for the current transaction, as if using :meth:`temporaryTransactionSettings`.
        """
        if settings or kwargs:
            with self.temporaryTransactionSettings(settings, **kwargs):
                return self._get_mini_repository().commit()
        return self._get_mini_repository().commit()

    def rollback(self):
        """
        Roll back changes on open session.
        """
        return self._get_mini_repository().rollback()

    def evalInServer(self, code):
        """
        Evaluate the Lisp code in the server.

        You must have "eval" permissions to the store to use this feature.
        """

        return self._get_mini_repository().evalInServer(code)

    def evalGraphqlQuery(
        self,
        query,
        default_prefix=None,
        infer=False,
        namespaces=None,
        variables=None,
        aliases=None,
    ):
        """
        Evaluate the GraphQL query in the server.
        """
        return self._get_mini_repository().evalGraphqlQuery(
            query, default_prefix, infer, namespaces, variables, aliases
        )

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

        See: https://franz.com/agraph/support/documentation/current/encoded-ids.html
        """
        return self._get_mini_repository().registerEncodedIdPrefix(prefix, format)

    def registerEncodedIdPrefixes(self, registrations):
        """
        Registers multiple encoded prefixes. Any kind of iteratable collection
        of items with a prefix attribute and format attribute, or the prefix
        at index 0 and the format at index 1 will do (e.g. a list of tuples).
        Using PrefixFormat instances also works well.

        See: https://franz.com/agraph/support/documentation/current/encoded-ids.html
        """
        return self._get_mini_repository().registerEncodedIdPrefixes(registrations)

    def listEncodedIdPrefixes(self):
        """
        Lists all encoded id prefixes.

        See: https://franz.com/agraph/support/documentation/current/encoded-ids.html
        """
        regs = []

        enc_ids = self._get_mini_repository().listEncodedIdPrefixes()
        for enc_id in enc_ids:
            regs.append(PrefixFormat(enc_id["prefix"], enc_id["format"]))

        return regs

    def unregisterEncodedIdPrefix(self, prefix):
        """
        Unregisters the specified encoded id prefix.

        See: https://franz.com/agraph/support/documentation/current/encoded-ids.html
        """
        return self._get_mini_repository().unregisterEncodedIdPrefix(prefix)

    def allocateEncodedIds(self, prefix, amount=1):
        """
        allocateEncodedIds allows you to use the server to allocate
        unique ids for a given registered prefix which has a fixed-width
        format specifier.

        See notes on next-encoded-upi-for-prefix in:

        https://franz.com/agraph/support/documentation/current/encoded-ids.html
        """
        return self._get_mini_repository().allocateEncodedIds(prefix, amount)

    def deleteDuplicates(self, mode):
        """
        Delete duplicate triples from the store.

        :param mode: can be `"spo"` (triples are duplicates if they have the same subject,
                    predicate, and object, regardless of the graph) or `"spog"` (triples
                    are duplicates if they have the same subject, predicate, object, and
                    graph).

        .. seealso::

           Method :meth:`getDuplicateStatements`.
        """
        self._get_mini_repository().deleteDuplicates(mode)

    def getDuplicateStatements(self, mode):
        """
        Return all duplicates in the store.

        :param mode: can be `"spo"` (triples are duplicates if they have the same subject,
            predicate, and object, regardless of the graph) or `"spog"` (triples
            are duplicates if they have the same subject, predicate, object, and
            graph).
        :return: An iterator over duplicate statements.
        :rtype: RepositoryResult
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
        return self._get_mini_repository().callStoredProc(function, module, *args)

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

    def materializeEntailed(
        self, _with=None, without=None, useTypeSubproperty=False, commit=100000
    ):
        """
        Call to materialize entailed triples to enable reasoning queries without the dynamic query-time reasoner.
        Returns the number of triples added.

        _with and without can be either a single string or a list of strings denoting rules beyond rdfs++ you wish to use.
        See the documentation for the current set, but "same-as", "restriction", "values-from", "class", and "property" are examples.

        useTypeSubproperty tells the materializer to prefer using types that are rdfs:subPropertyOf rdf:type rather than rdf:type directly.

        commit indicates the number of triples per commit for the materializer.
        """
        return self._get_mini_repository().materializeEntailed(
            _with=_with,
            without=without,
            useTypeSubproperty=useTypeSubproperty,
            commit=commit,
        )

    def deleteMaterialized(self):
        """
        Deletes all previously materialized triples.
        Returns the number of triples deleted.
        """
        return self._get_mini_repository().deleteMaterialized()

    def namespace(self, prefix):
        """
        Creates an object that allows for simple creation of URIs in given namespace.
        Attribute lookups on the returned object will produce URIs with the attribute
        name as localname. Indexing into the object or calling it like a function
        will have the same effect.

        >>> from franz.openrdf.connect import ag_connect
        >>> conn = ag_connect('repo')
        >>> ex = conn.namespace('http://franz.com/example/')
        >>> ex.foo
        <http://franz.com/example/foo>
        >>> ex['bar']
        <http://franz.com/example/bar>
        >>> ex('baz')
        <http://franz.com/example/baz>

        :param prefix: Prefix prepended to URIs created by the returned object.
        :type prefix: str
        :return: An object that can be used to create URIs.
        """
        return self.getValueFactory().namespace(prefix)

    def executeTupleQuery(
        self,
        query,
        language=QueryLanguage.SPARQL,
        output=None,
        output_format=RDFFormat.TABLE,
    ):
        """
        Prepare and immediately evaluate a query that returns tuples.

        :param query: Query text.
        :type query: str
        :param language: Query language, the default is SPARQL.
        :type language: QueryLanguage
        :param output: File path or a file-like object to write
                       the result to.
        :type output: str|file
        :param output_format: Serialization format for ``output``.
        :type output_format: RDFFormat

        :return: Query result, or ``None`` if ``output`` is used.
        :rtype: TupleQueryResult
        """
        q = self.prepareTupleQuery(language, query)
        return q.evaluate(output=output, output_format=output_format)

    def executeGraphQuery(
        self,
        query,
        language=QueryLanguage.SPARQL,
        output=None,
        output_format=RDFFormat.NQX,
    ):
        """
        Prepare and immediately evaluate a query that returns RDF.

        :param query: Query text.
        :type query: str
        :param language: Query language, the default is SPARQL.
        :type language: QueryLanguage
        :param output: File path or a file-like object to write
                       the result to.
        :type output: str|file
        :param output_format: Serialization format for ``output``.
        :type output_format: RDFFormat

        :return: Query result, or ``None`` if ``output`` is used.
        :rtype: RepositoryResult|None
        """
        q = self.prepareGraphQuery(language, query)
        return q.evaluate(output=output, output_format=output_format)

    def executeBooleanQuery(self, query, language=QueryLanguage.SPARQL):
        """
        Prepare and immediately evaluate a query that returns
        a boolean.

        :param query: Query text.
        :type query: str
        :param language: Query language, the default is SPARQL.
        :type language: QueryLanguage

        :return: Query result.
        :rtype: bool
        """
        q = self.prepareBooleanQuery(language, query)
        return q.evaluate()

    def executeUpdate(self, query):
        """
        Prepare and immediately evaluate a SPARQL update query.

        :param query: Query text.
        :type query: str

        :return: Query result (true iff the store has been modified).
        :rtype: bool
        """
        q = self.prepareUpdate(QueryLanguage.SPARQL, query)
        return q.evaluate()

    def setTransactionSettings(self, settings=None, **kwargs):
        """
        Change distributed transaction settings used by this connection.

        The new settings can be described either by a :class:`TransactionSettings`
        object or by passing individual parameters as keyword arguments.
        Argument names must match the fields of the :class:`TransactionSettings`
        class.

        If a settings object is passed all settings will be replaced.
        Keyword arguments will only affect specific parameters, any
        setting for which there is no corresponding keyword argument
        in the call will keep its current value.

        If both types of arguments are passed they will be merged, with keyword
        arguments taking precedence over values from the settings object.

        :param settings: A settings object.
        :type settings: TransactionSettings
        :param kwargs: Individual transaction parameters.
                       See :class:`TransactionSettings` for a list of valid names.
        """
        client = self._get_mini_repository(dedicated=True)
        settings = settings or client.transaction_settings
        if kwargs:
            settings = (settings or DEFAULT_TRANSACTION_SETTINGS)._replace(**kwargs)
        client.transaction_settings = settings

    @contextmanager
    def temporaryTransactionSettings(self, settings=None, **kwargs):
        """
        Create a context in which transaction settings of this connection
        are modified or replaced.

        If a settings object is given as an argument it will replace all
        transaction settings. Keyword arguments may be used to modify
        individual transaction parameters without affecting other settings.

        Here is how this method can be used to temporarily lower durability
        requirements while executing some operations::

            with conn.temporaryTransactionSettings(durability='min'):
                # Durability is now 'min', other settings remain unchanged.
                # Perform some operations
                ...
                conn.commit()
            # At this points durability will be restored to its original value.

        :param settings: A settings object.
        :type settings: TransactionSettings
        :param kwargs: Individual transaction parameters.
                       See :class:`TransactionSettings` for a list of valid names.
        :return: A context manager that takes care of changing and restroing
                 distributed transaction settings.
        """
        client = self._get_mini_repository(dedicated=True)
        old_settings = client.transaction_settings
        self.setTransactionSettings(settings, **kwargs)
        try:
            yield
        finally:
            self.setTransactionSettings(old_settings)

    def getTransactionSettings(self):
        """
        Return distributed transaction settings currently in force.

        :return: A settings object.
        :rtype: TransactionSettings
        """
        client = self._get_mini_repository()
        return client.transaction_settings

    def setUserAttributes(self, attributes):
        """
        Set user attributes (used to filter the store during queries).

        :param attributes: A dictionary mapping attribute names to values.
                           A value is either a string or a list of strings.
        :type attributes: dict[str, str|list[str]]
        """
        client = self._get_mini_repository(dedicated=True)
        client.user_attributes = copy.deepcopy(attributes)

    def getUserAttributes(self):
        """
        Get the current set of user attributes.

        :return: A dictionary mapping attribute names to values
                 (or lists of values).
        :rtype: dict[str, str|list[str]]
        """
        client = self._get_mini_repository()
        return copy.deepcopy(client.user_attributes)

    @contextmanager
    def temporaryUserAttributes(self, attributes):
        """
        Set user attributes for the duration of a code block.

        This method returns a context manager. It can be used like this::

            with conn.temporaryUserAttributes({'access-level': 'low'}):
                conn.do_something()

        :param attributes: A dictionary mapping attribute names to values.
                           A value is either a string or a list of strings.
        :type attributes: dict[str, str|list[str]]
        :rtype: ContextManager[None]
        """
        client = self._get_mini_repository(dedicated=True)
        old = client.user_attributes
        client.user_attributes = attributes
        try:
            yield
        finally:
            client.user_attributes = old

    def setAttributeDefinition(self, attr_def):
        """
        Define or modify an attribute definition.

        :param attr_def: Attribute definition.
        """
        self._get_mini_repository().setAttributeDefinition(attr_def)

    def deleteAttributeDefinition(self, name):
        """
        Delete an attribute definition.

        :param name: Attribute name.
        """
        self._get_mini_repository().deleteAttributeDefinition(name)

    def getAttributeDefinitions(self):
        """
        Get a list of all attribute definitions from the server.

        :return: A list of attribute definition objects.
        :rtype: list[AttributeDefinition]
        """
        raw = self._get_mini_repository().getAttributeDefinitions()
        return [attribute_definition_from_dict(item) for item in raw]

    def getAttributeDefinition(self, name):
        """
        Get the definition of a given attribute.

        Return None if there is no such attribute.

        :param name: Attribute name.
        :return: A definition object.
        :rtype: AttributeDefinition
        """
        raw = self._get_mini_repository().getAttributeDefinition(name)
        if raw:
            return attribute_definition_from_dict(raw[0])
        return None

    def setAttributeFilter(self, attribute_filter):
        """
        Set the static attribute filter.

        :param attribute_filter: The filter - either an AttributeFilter
                                 or a string.
        :type attribute_filter: AttributeFilter|str
        """
        self._get_mini_repository().setAttributeFilter(attribute_filter)

    def getAttributeFilter(self):
        """
        Get the current static attribute filter.

        The result will be a string, not an AttributeFilter object.

        :return: The filter or none (if there is no static filter).
        :rtype: str
        """
        return self._get_mini_repository().getAttributeFilter()

    def clearAttributeFilter(self):
        """
        Remove the static attribute filter (if set).
        """
        self._get_mini_repository().clearAttributeFilter()

    def convert_to_vector_store(
        self, embedder, api_key=None, model=None, supersede=False, dimensions=None
    ):
        """
        Convert an existing normal repo to a vector store by setting the embedder
        and optionally the api-key and model.  Unless supersede is True this
        function will do nothing if the repo is already a vector store
        """
        return self._get_mini_repository().convert_to_vector_store(
            embedder, api_key, model, supersede, dimensions
        )

    def add_objects(self, text, properties={}):
        """
        Compute the embedding for the given text string and add it
        to the repository.  text can be a single string or
        list of strings to add multiple objects.
        Each object added has the same properties added to
        the object.  The params argument looks like
        {"color" : "red", "size" : "large"}.
        Existing objects will not be added again.
        Returns the object identifier of the last object added.
        """
        return self._get_mini_repository().add_objects(text, properties)

    def remove_objects(self, text=None, all=False, property=None, value=None):
        """
        Remove zero or more objects.
        If all is True then all objects are removed.
        otherwise if text is given then it specifies the object to remove.
        otherwise all objects with the given property having the given
        value are removed.
        """
        return self._get_mini_repository().remove_objects(text, all, property, value)

    def nearest_neighbor(self, text, minScore=0.0, topN=5, selector=None):
        """
        An embedding is computed for the given text string and the closest embeddings
        to that embedding are computed.  embeddings with a matching score less
        than minScore are ignored. The best topN matches are returned.
        The return value is a json list of results. Each item in the list
        is a list of: object id, matching score, text of the object, and then two
        fields not used.
        """
        return self._get_mini_repository().nearest_neighbor(
            text, minScore, topN, selector
        )

    def object_property_value(self, object_id, property):
        """
        Return the value of the given property of the given object_id.  object_id
        can be a string like "http://franz.com/vdb/id/5" or it can be a URI object.
        The value is returned as a string in ntriple syntax.  resources (URI) are
        surrounded by < and >.  Literals begin with a double quote and in the case
        of a literal with data type the syntax is "value"^^type
        """
        return self._get_mini_repository().object_property_value(object_id, property)

    def object_text(self, object_id):
        """
        Return the text of the object denoted by object_id.  object_id
        can be a string like "http://franz.com/vdb/id/5" or it can be a URI object
        """
        return self._get_mini_repository().object_text(object_id)

    def object_embedding(self, object_id):
        """
        Return the embedding for the given object_id.  The embedding returned is
        a list of floating point numbers
        """
        return self._get_mini_repository().object_embedding(object_id)


def attribute_definition_from_dict(item):
    return AttributeDefinition(
        name=item.get("name"),
        allowed_values=item.get("allowed-values"),
        ordered=bool(item.get("ordered")),
        minimum_number=item.get("minimum-number"),
        maximum_number=item.get("maximum-number"),
    )


class GeoType:
    Cartesian = "CARTESIAN"
    Spherical = "SPHERICAL"

    def __init__(
        self,
        system,
        scale=None,
        unit=None,
        xMin=None,
        xMax=None,
        yMin=None,
        yMax=None,
        latMin=None,
        latMax=None,
        longMin=None,
        longMax=None,
    ):
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
        else:
            pass  ## can't happen

    def setConnection(self, connection):
        self.connection = connection

    def _getMiniGeoType(self):
        def stringify(term):
            return str(term) if term is not None else None

        if not self.miniGeoType:
            if self.system == GeoType.Cartesian:
                self.miniGeoType = (
                    self.connection._get_mini_repository().getCartesianGeoType(
                        stringify(self.scale),
                        stringify(self.xMin),
                        stringify(self.xMax),
                        stringify(self.yMin),
                        stringify(self.yMax),
                    )
                )
            elif self.system == GeoType.Spherical:
                self.miniGeoType = (
                    self.connection._get_mini_repository().getSphericalGeoType(
                        stringify(self.scale),
                        unit=stringify(self.unit),
                        latMin=stringify(self.latMin),
                        latMax=stringify(self.latMax),
                        longMin=stringify(self.longMin),
                        longMax=stringify(self.longMax),
                    )
                )
        return self.miniGeoType

    def createCoordinate(
        self, x=None, y=None, latitude=None, longitude=None, unit=None
    ):
        """
        Create an x, y  or lat, long  coordinate for the system defined by this geotype.
        """
        return GeoCoordinate(
            x=(x or latitude), y=y or longitude, unit=unit, geoType=self
        )

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
        poly.resource = (
            self.connection.createURI(uri) if uri else self.connection.createBNode()
        )
        miniResource = self.connection._convert_term_to_mini_term(poly.resource)
        miniRep = self.connection._get_mini_repository()
        if self.system == GeoType.Cartesian:
            miniVertices = [
                miniRep.createCartesianGeoLiteral(
                    self._getMiniGeoType(), coord[0], coord[1]
                )
                for coord in poly.vertices
            ]
        elif self.system == GeoType.Spherical:
            miniVertices = [
                miniRep.createSphericalGeoLiteral(
                    self._getMiniGeoType(), coord[0], coord[1]
                )
                for coord in poly.vertices
            ]
        miniRep.createPolygon(miniResource, miniVertices)
        return poly


def dump_json_ld(source, sort_keys=False):
    """
    Convert a dictionary to JSON-LD string.

    This basically dumps the source like a regular JSON document,
    but is a bit smarter about URLs and literals.

    :param source: Dictionary to be serialized.
    :param sort_keys: if True make sure that metadata keys (@context etc) appear first.
    :return: a JSON-serializable dict.
    """

    def fix_value(value):
        if callable(getattr(value, "to_json_ld", None)):
            return value.to_json_ld()
        elif isinstance(value, dict):
            return fix_dict(value)
        elif isinstance(value, list):
            return [fix_value(v) for v in value]
        return value

    def fix_at_value(value):
        if callable(getattr(value, "to_json_ld_key", None)):
            return value.to_json_ld_key()
        else:
            return fix_value(value)

    def key_priority(key):
        if key == "@context":
            return 0
        elif key == "@id":
            return 1
        elif key == "@value":
            return 2
        elif key.startswith("@"):
            return 3
        return 4

    def fix_dict(d):
        result = []
        for key, value in d.items():
            if callable(getattr(key, "to_json_ld_key", None)):
                key = key.to_json_ld_key()
            # Handle @id, @vocab, ...
            if key.startswith("@"):
                value = fix_at_value(value)
            else:
                value = fix_value(value)
            result.append((key, value))
        if sort_keys:
            # Make sure the @context comes first
            return OrderedDict(sorted(result, key=lambda p: key_priority(p[0])))
        return dict(result)

    return encode_json(fix_value(source))


class DocumentKey(
    namedtuple(
        "DocumentKey",
        ("prefix", "rename", "rdf_type", "lang", "skip", "transform", "graph"),
    )
):
    """
    Describes the way in which value of a single key is translated into a triple (or triples,
    if the value is a list) when importing documents into AG.

    See the documentation of :meth:`RepositoryConnection#addDocument` for details.
    """

    __slots__ = ()


DocumentKey.__new__.__defaults__ = (None,) * len(DocumentKey._fields)
