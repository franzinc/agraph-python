################################################################################
# Copyright (c) 2006-2017 Franz Inc.
# All rights reserved. This program and the accompanying materials are
# made available under the terms of the MIT License which accompanies
# this distribution, and is available at http://opensource.org/licenses/MIT
################################################################################
from __future__ import absolute_import, division, with_statement

import copy
import sys

if(sys.version_info.major < 3):
    from inspect import getargspec as getfullargspec
else:
    from inspect import getfullargspec

import math
import re
import threading
from contextlib import contextmanager
from datetime import timedelta

import six
from franz.miniclient.agjson import encode_json
from franz.openrdf.model.value import URI
from franz.openrdf.repository.attributes import AttributeFilter
from franz.openrdf.rio.formats import Format
from past.builtins import basestring
from past.utils import old_div
from six import python_2_unicode_compatible
from six.moves.urllib.parse import quote, urlparse

from ..openrdf.util.contexts import wrap_context
from ..openrdf.util.strings import to_native_string
from .request import (RequestError, decode, deserialize, encode, jsonRequest,
                      nullRequest, serialize, urlenc)


def _split_proxy(proxy):
    """
    Split a proxy definition of the form PROTOCOL://HOST:PORT into components.

    Return 3x None if the argument is None.

    :param proxy: A proxy specification string.
    :return: scheme, host and port as a triple.
    :rtype (string, string, int)
    """
    if proxy:
        url = urlparse(proxy)
        return url.scheme.lower(), url.hostname, url.port
    else:
        return None, None, None


class Service(object):
    def __init__(self, url, user=None, password=None, cainfo=None, sslcert=None,
                 verifyhost=None, verifypeer=None, proxy=None):
        # Note: it is important to save the arguments into fields with identical names.
        # This makes the weird cloning mechanism used by subclasses work.
        # WARNING: The cloning process mentioned above will create a **shallow** copy
        # of each argument.
        self.url = url
        self.user = user
        self.password = password
        self.runAsName = None
        self.cainfo = cainfo
        self.sslcert = sslcert
        self.verifyhost = verifyhost
        self.verifypeer = verifypeer
        self.proxy = proxy
        self.proxy_type, self.proxy_host, self.proxy_port = _split_proxy(proxy)
        self.session = None  # Will be created lazily
        self.transaction_settings = None
        self.user_attributes = None

    def getHeaders(self):
        """
        Return a dictionary of headers that should be added to every request.
        Return ``None`` instead of an empty dictionary if there are no such
        headers.
        """
        s = self.transaction_settings
        values = []
        if s:
            if s.distributed_transaction_timeout is not None:
                values.append(
                    ('distributedTransactionTimeout',
                     time_in_seconds(s.distributed_transaction_timeout)))
            if s.durability is not None:
                values.append(('durability', s.durability))
            if s.transaction_latency_count is not None:
                values.append(('transactionLatencyCount',
                               s.transaction_latency_count))
            if s.transaction_latency_timeout is not None:
                values.append(('transactionLatencyTimeout',
                               time_in_seconds(s.transaction_latency_timeout)))
        result = {}
        if values:
            result['x-repl-settings'] = ' '.join('%s=%s' % value for value in values)
        if self.user_attributes:
            result['x-user-attributes'] = encode_json(self.user_attributes)
        return result or None

    def _instance_from_url(self, subclass, url=None):
        """
        Create a copy of this service object that will:
           - be an instance of the ``subclass`` class.
           - share the values of all attributes except url,
             which is set to the specified value.

        This is used to create Repository and Catalog objects in the code below.
        """
        # Copy authentication and other settings
        kwargs = {}
        for arg_name in getfullargspec(subclass.__init__).args:
            if hasattr(self, arg_name):
                kwargs[arg_name] = copy.copy(getattr(self, arg_name))
        # Override URL
        if url is not None:
            kwargs['url'] = url
        return subclass(**kwargs)

    def copy(self):
        return self._instance_from_url(type(self))


class Catalog(Service):
    def listRepositories(self):
        """Returns the names of repositories in the catalog."""
        repos = jsonRequest(self, "GET", "/repositories")
        return [repo["id"] for repo in repos]

    def createRepository(self, name, indices=None):
        """Ask the server to create a new repository."""
        nullRequest(self, "PUT", "/repositories/" + quote(name) + "?" +
            urlenc(index=indices))
        return self.getRepository(name)

    def deleteRepository(self, name):
        """Delete a repository in this catalog."""
        nullRequest(self, "DELETE", "/repositories/" + quote(name))

    def getRepository(self, name):
        """Create an access object for a triple store."""
        return self._instance_from_url(Repository, to_native_string(self.url) + "/repositories/" + quote(name))


class Client(Service):
    def getVersion(self):
        return jsonRequest(self, "GET", "/version")

    def listCatalogs(self):
        return [cat["id"] if cat["id"] != "/" else None for cat in jsonRequest(self, "GET", "/catalogs")]

    def openCatalog(self, uriOrName):
        if re.match("^https?://", uriOrName):
            return self._instance_from_url(Catalog, uriOrName)
        else:
            return self.openCatalogByName(uriOrName)

    def openCatalogByName(self, name=None):
        url = to_native_string(self.url)
        if name:
            name = to_native_string(name)
            if name != "~":
                url += "/catalogs/" + quote(name)
        return self.openCatalog(url)

    def getInitfile(self):
        return jsonRequest(self, "GET", "/initfile")

    def setInitfile(self, content=None, restart=True):
        if (content is None):
            nullRequest(self, "DELETE", "/initfile")
        else:
            nullRequest(self, "PUT", "/initfile?" + urlenc(restart=restart), content)

    def listScripts(self):
        """
        List the registered scripts.
        """
        return jsonRequest(self, "GET", "/scripts")

    def addScript(self, module, code):
        nullRequest(self, "PUT", "/scripts/" + to_native_string(module),
            body=code)

    def getScript(self, module):
        return jsonRequest(self, "GET", "/scripts/" + to_native_string(module))

    def deleteScript(self, module):
        nullRequest(self, "DELETE", "/scripts/" + to_native_string(module))

    def openSession(self, spec, autocommit=False, lifetime=None, loadinitfile=False):
        url = jsonRequest(self, "POST", "/session?" +
                          urlenc(autoCommit=autocommit, lifetime=lifetime,
                                 loadInitFile=loadinitfile, store=spec))
        rep = self._instance_from_url(Repository, url)
        rep._enableSession(lifetime)
        return rep

    def listUsers(self):
        return jsonRequest(self, "GET", "/users")

    def addUser(self, name, password=None):
        assert password is not None or name == "anonymous"
        nullRequest(self, "PUT", "/users/" + quote(name) + "?" + urlenc(password=password))

    def deleteUser(self, name):
        nullRequest(self, "DELETE", "/users/" + quote(name))

    def changeUserPassword(self, name, password):
        nullRequest(self, "POST", "/users/" + quote(name) + "/password", password)

    def listUserAccess(self, name):
        return jsonRequest(self, "GET", "/users/" + quote(name) + "/access")

    def addUserAccess(self, name, read, write, catalog, repository):
        nullRequest(self, "PUT", "/users/" + quote(name) + "/access?" +
            urlenc(read=read, write=write, catalog=catalog, repository=repository))

    def deleteUserAccess(self, name, read, write, catalog, repository):
        nullRequest(self, "DELETE", "/users/" + quote(name) + "/access?" +
            urlenc(read=read, write=write, catalog=catalog, repository=repository))

    def listUserEffectiveAccess(self, name):
        return jsonRequest(self, "GET", "/users/" + quote(name) + "/effectiveAccess")

    def listUserPermissions(self, name):
        return jsonRequest(self, "GET", "/users/" + quote(name) + "/permissions")

    def listUserEffectivePermissions(self, name):
        return jsonRequest(self, "GET", "/users/" + quote(name) + "/effectivePermissions")

    def addUserPermission(self, name, _type):
        nullRequest(self, "PUT", "/users/" + quote(name) + "/permissions/" + _type)

    def deleteUserPermission(self, name, _type):
        nullRequest(self, "DELETE", "/users/" + quote(name) + "/permissions/" + _type)

    def listRoles(self):
        return jsonRequest(self, "GET", "/roles")

    def addRole(self, role):
        nullRequest(self, "PUT", "/roles/" + quote(role))

    def deleteRole(self, role):
        nullRequest(self, "DELETE", "/roles/" + quote(role))

    def listRolePermissions(self, role):
        return jsonRequest(self, "GET", "/roles/" + quote(role) + "/permissions")

    def addRolePermission(self, role, _type):
        nullRequest(self, "PUT", "/roles/" + quote(role) + "/permissions/" + _type)

    def deleteRolePermission(self, role, _type):
        nullRequest(self, "DELETE", "/roles/" + quote(role) + "/permissions/" + _type)

    def listRoleAccess(self, role):
        return jsonRequest(self, "GET", "/roles/" + quote(role) + "/access")

    def addRoleAccess(self, role, read, write, catalog, repository):
        nullRequest(self, "PUT", "/roles/" + quote(role) + "/access?" +
            urlenc(read=read, write=write, catalog=catalog, repository=repository))

    def deleteRoleAccess(self, role, read, write, catalog, repository):
        nullRequest(self, "DELETE", "/roles/" + quote(role) + "/access?" +
            urlenc(read=read, write=write, catalog=catalog, repository=repository))

    def listUserRoles(self, name):
        return jsonRequest(self, "GET", "/users/" + quote(name) + "/roles")

    def addUserRole(self, name, role):
        nullRequest(self, "PUT", "/users/" + quote(name) + "/roles/" + quote(role))

    def deleteUserRole(self, name, role):
        nullRequest(self, "DELETE", "/users/" + quote(name) + "/roles/" + quote(role))

    def listUserSecurityFilters(self, name, _type):
        return jsonRequest(self, "GET", "/users/" + quote(name) + '/security-filters/' + to_native_string(_type))

    def addUserSecurityFilter(self, name, _type, s=None, p=None, o=None, g=None):
        nullRequest(self, "POST", "/users/" + quote(name) + "/security-filters/" + to_native_string(_type) + "?" +
            urlenc(s=s, p=p, o=o, g=g))

    def deleteUserSecurityFilter(self, name, _type, s=None, p=None, o=None, g=None):
        nullRequest(self, "DELETE", "/users/" + quote(name) + "/security-filters/" + to_native_string(_type) + "?" +
            urlenc(s=s, p=p, o=o, g=g))

    def listRoleSecurityFilters(self, role, _type):
        return jsonRequest(self, "GET", "/roles/" + quote(role) + '/security-filters/' + to_native_string(_type))

    def addRoleSecurityFilter(self, role, _type, s=None, p=None, o=None, g=None):
        nullRequest(self, "POST", "/roles/" + quote(role) + "/security-filters/" + to_native_string(_type) + "?" +
            urlenc(s=s, p=p, o=o, g=g))

    def deleteRoleSecurityFilter(self, role, _type, s=None, p=None, o=None, g=None):
        nullRequest(self, "DELETE", "/roles/" + quote(role) + "/security-filters/" + to_native_string(_type) + "?" +
            urlenc(s=s, p=p, o=o, g=g))

    def getUserData(self, key):
        try:
            return jsonRequest(self, "GET",
                               "/users/%s/data/%s" % (quote(self.user, safe=''),
                                                      quote(key, safe='')),
                               )
        except RequestError as e:
            if e.status == 404:
                return None
            raise

    def setUserData(self, key, data):
        nullRequest(self, "PUT", "/users/%s/data/%s" % (quote(self.user, safe=''),
                                                        quote(key, safe='')),
                    body=data)

    def deleteUserData(self, key):
        nullRequest(self, "DELETE", "/users/%s/data/%s" % (quote(self.user, safe=''),
                                                           quote(key, safe='')))

    def listUserData(self):
        entries = jsonRequest(self, "GET",
                              "/users/%s/data/" % quote(self.user, safe=''))
        return [entry['id'] for entry in entries]


class Repository(Service):
    sessionAlive = None

    def getSize(self, context=None):
        """Returns the amount of triples in the repository."""
        return jsonRequest(self, "GET", "/size", urlenc(context=context))

    def listContexts(self):
        """Lists the contexts (named graphs) that are present in this repository."""
        return [t["contextID"] for t in jsonRequest(self, "GET", "/contexts")]

    def evalSparqlQuery(self, query, infer=False, context=None, namedContext=None, callback=None,
                        bindings=None, planner=None, checkVariables=None, count=False, accept=None, analyze=False,
                        analysisTechnique=None, analysisTimeout=None, update=False):
        """Execute a SPARQL query. Context can be None or a list of
        contexts -- strings in "http://foo.com" form or "null" for the
        default context. Return type depends on the query type. ASK
        gives a boolean, SELECT a {names, values} object containing
        lists of lists of terms. CONSTRUCT and DESCRIBE return a list
        of lists representing statements. Callback WILL NOT work on
        ASK queries."""
        if accept is None:
            accept="text/integer" if count else "application/json"
        if analyze:
            accept="text/plain"
        if bindings is not None:
            bindings = "".join(["&$" + quote(a) + "=" + quote(b.encode("utf-8")) for a, b in list(bindings.items())])
        return jsonRequest(self, "POST" if update else "GET", self.url,
                   urlenc(query=query, infer=infer, context=context, namedContext=namedContext,
                       planner=planner, checkVariables=checkVariables,
                       analyzeIndicesUsed=analyze, queryAnalysisTechnique=analysisTechnique,
                       queryAnalysisTimeout=analysisTimeout,
                       returnQueryMetadata=True) + (bindings or ""),
                       callback=callback,
                       accept=accept)

    def evalPrologQuery(self, query, infer=False, callback=None, limit=None, count=False, accept=None):
        """Execute a Prolog query. Returns a {names, values} object."""
        if accept is None:
            accept="text/integer" if count else "application/json"
        return jsonRequest(self, "POST", self.url,
                           urlenc(query=query, infer=infer, queryLn="prolog", limit=limit),
                           callback=callback,
                           accept=accept)

    def evalGraphqlQuery(self, query, default_prefix, infer, namespaces, variables, aliases):
        """Execute a GraphQL query. Returns a JSON object."""
        api = "/graphql"
        api_args = "&".join([ name + "=" + arg for name, arg in [
            ("default-prefix", default_prefix),
            ("infer", "false" if infer == False else infer),
            ("namespaces", namespaces), ("variables", variables),
            ("aliases", aliases)]
            if arg
        ])
        if api_args:
            api += "?" + api_args
        return jsonRequest(self, method="POST", url=self.url+api, body=query)

    def definePrologFunctors(self, definitions):
        """Add Prolog functors to the environment. Takes a string
        containing Lisp-syntax functor definitions (using the <-- and
        <- operators)."""
        nullRequest(self, "POST", "/functor", definitions)

    def evalJavaScript(self, code):
        """Evaluate JavaScript code in the server."""
        return jsonRequest(self, "POST", "/eval", code, 'text/javascript')

    def evalInServer(self, code):
        """Evaluate Common Lisp code in the server."""
        return jsonRequest(self, "POST", "/eval", code)

    def runAsUser(self, username=None):
        """
        Only for use as a superuser during a session.

        Runs requests on this connection as username.

        None - the default - clears the setting.
        """
        assert self.sessionAlive, "runAsUser can only be used on a session."
        self.runAsName = username

    def commit(self):
        nullRequest(self, "POST", "/commit")

    def rollback(self):
        nullRequest(self, "POST", "/rollback")

    def getStatements(self, subj=None, pred=None, obj=None, context=None, infer=False, callback=None,
                      limit=None, offset=None, accept=None, tripleIDs=False, count=False):
        """Retrieve all statements matching the given constraints.
        Context can be None or a list of contexts, as in
        evalSparqlQuery."""
        if subj == [] or pred == [] or obj == [] or context == []: return []
        subjEnd, predEnd, objEnd = None, None, None
        if isinstance(subj, tuple): subj, subjEnd = subj
        if isinstance(pred, tuple): pred, predEnd = pred
        if isinstance(obj, tuple): obj, objEnd = obj

        if accept is None:
            accept = "application/json"
            if count: accept = "text/integer"
            elif tripleIDs: accept = "application/x-quints+json"
        return jsonRequest(self, "GET", "/statements",
            urlenc(subj=subj, subjEnd=subjEnd, pred=pred, predEnd=predEnd,
                obj=obj, objEnd=objEnd, context=context, infer=infer,
                limit=limit, offset=offset),
            callback=callback, accept=accept)

    def getStatementsById(self, ids, returnIDs=True, accept=None, callback=None):
        if accept is None and returnIDs:
            accept = "application/x-quints+json" if returnIDs else "application/json"
        return jsonRequest(self, "GET", "/statements/id", urlenc(id=ids),
                           accept=accept, callback=callback)

    def addStatement(self, subj, pred, obj, context=None, attributes=None):
        """Add a single statement to the repository."""
        args = {
            'subj': subj,
            'pred': pred,
            'obj': obj,
            'context': context,
            'attributes': attributes and encode_json(attributes)
        }
        nullRequest(self, "PUT", "/statement?" + urlenc(**args))

    def deleteMatchingStatements(self, subj=None, pred=None, obj=None, context=None):
        """Delete all statements matching the constraints from the
        repository. Context can be None or a single graph name."""
        nullRequest(self, "DELETE", "/statements",
                    urlenc(subj=subj, pred=pred, obj=obj, context=context))

    def addStatements(self, quads, commitEvery=None):
        """Add a collection of statements to the repository. Quads
        should be an array of or five four-element arrays, where the fourth
        element, the graph name, may be None. The fifth element, if present,
        must be a string with a JSON-encoded dictionary of attribute values.
        """
        nullRequest(self, "POST", "/statements?" +
                    urlenc(commit=commitEvery),
                    encode_json(quads), content_type="application/json")

    def loadData(self, data, rdf_format, base_uri=None, context=None,
                 commit_every=None, content_encoding=None, attributes=None,
                 json_ld_store_source=None,
                 json_ld_context=None, allow_external_references=None,
                 external_reference_timeout=None):
        nullRequest(self, "POST",
                    "/statements?" + urlenc(
                        context=context,
                        baseURI=base_uri,
                        commit=commit_every,
                        attributes=attributes and encode_json(attributes),
                        jsonLdStoreSource=json_ld_store_source,
                        jsonLdContext=json_ld_context and fix_json_ld_context(json_ld_context),
                        externalReferences=allow_external_references,
                        extermalReferenceTimeout=external_reference_timeout),
                    data,
                    content_type=Format.mime_type_for_format(rdf_format),
                    content_encoding=content_encoding,)

    def loadFile(self, file, rdf_format, baseURI=None, context=None, serverSide=False,
                 commitEvery=None, content_encoding=None, attributes=None,
                 json_ld_store_source=None,
                 json_ld_context=None, allow_external_references=None,
                 external_reference_timeout=None):
        mime = Format.mime_type_for_format(rdf_format)

        if serverSide:
            # file is a server-side path, body should be empty
            body_context = wrap_context(None)
        elif isinstance(file, basestring) and not serverSide:
            # file is a local path, read it and send as body
            body_context = open(file, 'rb')
            file = None
        else:
            # We were passed a file-like object that we must not close,
            # so we'll just pass it as body.
            body_context = wrap_context(file)
            file = None

        with body_context as body:
            params = urlenc(file=file, context=context, baseURI=baseURI, commit=commitEvery,
                            attributes=attributes and encode_json(attributes),
                            jsonLdStoreSource=json_ld_store_source,
                            jsonLdContext=json_ld_context and fix_json_ld_context(json_ld_context),
                            externalReferences=allow_external_references,
                            extermalReferenceTimeout=external_reference_timeout)
            nullRequest(self, "POST", "/statements?" + params, body,
                        content_type=mime, content_encoding=content_encoding)

    def getBlankNodes(self, amount=1):
        return jsonRequest(self, "POST", "/blankNodes", urlenc(amount=amount))

    def deleteStatements(self, quads):
        """Delete a collection of statements from the repository."""
        nullRequest(self, "POST", "/statements/delete", encode_json(quads), content_type="application/json")

    def deleteStatementsById(self, ids):
        nullRequest(self, "POST", "/statements/delete?ids=true", encode_json(ids), content_type="application/json")

    def evalFreeTextSearch(self, pattern, index=None, infer=False, limit=None, offset=None):
        """Use free-text indices to search for the given pattern.
        Returns an array of statements."""
        return jsonRequest(self, "GET", "/freetext",
                           urlenc(pattern=pattern, infer=infer, limit=limit, offset=offset, index=index))

    def listFreeTextIndices(self):
        """List the names of free-text indices defined in this
        repository."""
        return jsonRequest(self, "GET", "/freetext/indices")

    def getFreeTextIndexConfiguration(self, index):
        """Returns a dictionary with fields \"predicates\",
        \"indexLiterals\", \"indexResources\", \"indexFields\",
        \"minimumWordSize\", \"stopWords\", and \"wordFilters\"."""
        return jsonRequest(self, "GET", "/freetext/indices/" + quote(index))

    def createFreeTextIndex(self, index, predicates=None, indexLiterals=None, indexResources=None,
                            indexFields=None, minimumWordSize=None, stopWords=None, wordFilters=None,
                            innerChars=None, borderChars=None, tokenizer=None):
        """Create a free-text index. predicates, if given, should be a
        list of resources. indexLiterals can be True, False, or a list
        of strings, indicating the literal types or langs to index.
        indexResources can be True, False, or \"short\". indexFields
        can be a list containing any combination of the elements
        \"subject\", \"predicate\", \"object\", and \"graph\".
        minimumWordSize is an integer, and stopWords a list of
        strings. wordFilter must be a list of filter names.

        "innerChars" and "borderChars" can be lists. "tokenizer" is
        a string. See documentation:

        http://www.franz.com/agraph/support/documentation/current/http-protocol.html#put-freetext-index"""
        # Not passing these at all causes the defaults to be used. So
        # when they are given, they should be passed with an empty
        # value.
        if stopWords == []: stopWords = ""
        if indexFields == []: indexFields = ""
        nullRequest(self, "PUT", "/freetext/indices/" + quote(index),
                    urlenc(predicate=predicates, indexLiterals=indexLiterals and True,
                           indexLiteralType=indexLiterals if isinstance(indexLiterals, list) else None,
                           indexResources=indexResources, indexField=indexFields,
                           minimumWordSize=minimumWordSize, stopWord=stopWords, wordFilter=wordFilters,
                           innerChars=innerChars, borderChars=borderChars,
                           tokenizer=tokenizer))

    def modifyFreeTextIndex(self, index, predicates=None, indexLiterals=None, indexResources=None,
                            indexFields=None, minimumWordSize=None, stopWords=None, wordFilters=None,
                            reIndex=None, innerChars=None, borderChars=None,
                            tokenizer=None):
        """Reconfigure a free-text index. Most arguments work as in
        createFreeTextIndex, except that here not passing them means
        'leave the old value'. reIndex controls whether old triples
        are re-indexed."""
        if stopWords == []: stopWords = ""
        if predicates == []: predicates = ""
        if indexFields == []: indexFields = ""
        if wordFilters == []: wordFilters = ""
        nullRequest(self, "POST", "/freetext/indices/" + quote(index),
                    urlenc(predicate=predicates, indexLiterals=indexLiterals and True,
                           indexLiteralType=indexLiterals if isinstance(indexLiterals, list) else None,
                           indexResources=indexResources, indexField=indexFields,
                           minimumWordSize=minimumWordSize, stopWord=stopWords, wordFilter=wordFilters,
                           reIndex=reIndex, innerChars=innerChars, borderChars=borderChars,
                           tokenizer=tokenizer))

    def deleteFreeTextIndex(self, index):
        """Delete the named free-text index."""
        nullRequest(self, "DELETE", "/freetext/indices/" + quote(index))

    def listFreeTextPredicates(self):
        """List the predicates that are used for free-text indexing."""
        return jsonRequest(self, "GET", "/freetext/predicates")

    def registerFreeTextPredicate(self, predicate):
        """Add a predicate for free-text indexing."""
        nullRequest(self, "POST", "/freetext/predicates", urlenc(predicate=predicate))

    def clearNamespaces(self, reset=True):
        """
        Deletes all namespaces in this repository for the current user. If a
        `reset` argument of `True` is passed, the user's namespaces are reset
        to the default set of namespaces, otherwise all namespaces are cleared.
        """
        nullRequest(self, "DELETE", "/namespaces?" + urlenc(reset=reset))

    def addNamespace(self, prefix, uri):
        nullRequest(self, "PUT", "/namespaces/" + quote(prefix),
                    uri, content_type="text/plain")

    def deleteNamespace(self, prefix):
        nullRequest(self, "DELETE", "/namespaces/" + quote(prefix))

    def listNamespaces(self):
        return jsonRequest(self, "GET", "/namespaces")

    def getNamespace(self, prefix):
        return jsonRequest(self, "GET", "/namespaces/" + quote(prefix))

    def clearQueryOptions(self):
        """
        Deletes all query options in this repository for the current user.
        """
        nullRequest(self, "DELETE", "/query-options")

    def setQueryOption(self, name, value):
        nullRequest(self, "PUT", "/query-options/" + quote(name),
                    value, content_type="text/plain")

    def deleteQueryOption(self, name):
        nullRequest(self, "DELETE", "/query-options/" + quote(name))

    def listQueryOptions(self):
        return jsonRequest(self, "GET", "/query-options")

    def getQueryOption(self, name):
        return jsonRequest(self, "GET", "/query-options/" + quote(name))

    def listMappedTypes(self):
        return jsonRequest(self, "GET", "/mapping/type")

    def addMappedType(self, type, encoding):
        nullRequest(self, "POST", "/mapping/type", urlenc(type=type, encoding=encoding))

    def deleteMappedType(self, type):
        nullRequest(self, "DELETE", "/mapping/type", urlenc(type=type))

    def listMappedPredicates(self):
        return jsonRequest(self, "GET", "/mapping/predicate")

    def addMappedPredicate(self, predicate, encoding):
        nullRequest(self, "POST", "/mapping/predicate",
                    urlenc(predicate=predicate, encoding=encoding))

    def deleteMappedPredicate(self, predicate):
        nullRequest(self, "DELETE", "/mapping/predicate", urlenc(predicate=predicate))

    def listIndices(self):
        return jsonRequest(self, "GET", "/indices")

    def listValidIndices(self):
        return jsonRequest(self, "GET", "/indices?listValid=true")

    def addIndex(self, _type):
        nullRequest(self, "PUT", "/indices/" + quote(_type))

    def dropIndex(self, _type):
        nullRequest(self, "DELETE", "/indices/" + quote(_type))

    def optimizeIndices(self, level=None, wait=None):
        nullRequest(self, "POST", "/indices/optimize?" + urlenc(
            level=level,wait=wait))

    def getCartesianGeoType(self, stripWidth, xMin, xMax, yMin, yMax):
        """Retrieve a cartesian geo-spatial literal type."""
        return jsonRequest(self, "POST", "/geo/types/cartesian?" +
                           urlenc(stripWidth=stripWidth, xmin=xMin, ymin=yMin, xmax=xMax, ymax=yMax))


    def getSphericalGeoType(self, stripWidth, unit="degree", latMin=None, latMax=None, longMin=None, longMax=None):
        """Retrieve a spherical geo-spatial literal type."""
        return jsonRequest(self, "POST", "/geo/types/spherical?" +
                           urlenc(stripWidth=stripWidth, unit=unit, latmin=latMin, latmax=latMax,
                                  longmin=longMin, longmax=longMax))

    def listGeoTypes(self):
        """List the geo-spatial types registered in the store."""
        return jsonRequest(self, "GET", "/geo/types")

    def createCartesianGeoLiteral(self, type, x, y):
        """Create a geo-spatial literal of the given type."""
        return "\"%+g%+g\"^^%s" % (x, y, type)

    @python_2_unicode_compatible
    class UnsupportedUnitError(Exception):
        def __init__(self, unit): self.unit = unit
        def __str__(self): return "'%s' is not a known unit (use km, mile, degree, or radian)." % self.unit

    def unitDegreeFactor(self, unit):
        if unit == "degree": return 1.0
        elif unit == "radian": return 57.29577951308232
        elif unit == "km": return 0.008998159
        elif unit == "mile": return 0.014481134
        else: raise Repository.UnsupportedUnitError(unit)

    def createSphericalGeoLiteral(self, type, lat, longitude, unit="degree"):
        """Create a geo-spatial latitude/longitude literal of the
        given type. Unit can be 'km', 'mile', 'radian', or 'degree'."""
        def asISO6709(number, digits):
            sign = "+"
            if (number < 0):
                sign= "-"
                number = -number
            fl = math.floor(number)
            return sign + (("%%0%dd" % digits) % fl) + (".%07d" % ((number - fl) * 10000000))

        conv = self.unitDegreeFactor(unit)
        return "\"%s%s\"^^%s" % (asISO6709(lat * conv, 2), asISO6709(longitude * conv, 3), type)

    def getStatementsHaversine(self, type, predicate, lat, longitude, radius, unit="km", limit=None, offset=None,
                               accept=None, callback=None):
        """Get all the triples with a given predicate whose object
        lies within radius units from the given latitude/longitude."""
        return jsonRequest(self, "GET", "/geo/haversine",
                           urlenc(type=type, predicate=predicate, lat=lat, long=longitude, radius=radius, unit=unit,
                                  limit=limit, offset=offset),
                           accept=accept, callback=callback)

    def getStatementsInsideBox(self, type, predicate, xMin, xMax, yMin, yMax, limit=None, offset=None,
                               accept=None, callback=None):
        """Get all the triples with a given predicate whose object
        lies within the specified box."""
        return jsonRequest(self, "GET", "/geo/box",
                           urlenc(type=type, predicate=predicate, xmin=xMin, xmax=xMax, ymin=yMin, ymax=yMax,
                                  limit=limit, offset=offset),
                           accept=accept, callback=callback)

    def getStatementsInsideCircle(self, type, predicate, x, y, radius, limit=None, offset=None,
                                  accept=None, callback=None):
        """Get all the triples with a given predicate whose object
        lies within the specified circle."""
        return jsonRequest(self, "GET", "/geo/circle",
                           urlenc(type=type, predicate=predicate, x=x, y=y, radius=radius, limit=limit, offset=offset),
                           accept=accept, callback=callback)

    def getStatementsInsidePolygon(self, type, predicate, polygon, limit=None, offset=None, accept=None, callback=None):
        """Get all the triples with a given predicate whose object
        lies within the specified polygon (see createPolygon)."""
        return jsonRequest(self, "GET", "/geo/polygon",
                           urlenc(type=type, predicate=predicate, polygon=polygon, limit=limit, offset=offset),
                           accept=accept, callback=callback)

    def createPolygon(self, resource, points):
        """Create a polygon with the given name in the store. points
        should be a list of literals created with createCartesianGeoLiteral."""
        nullRequest(self, "PUT", "/geo/polygon?" + urlenc(resource=resource, point=points))

    def registerSNAGenerator(self, name, subjectOf=None, objectOf=None, undirected=None, query=None):
        """subjectOf, objectOf, and undirected can be either a single
        predicate or a list of predicates. query should be a prolog
        query in the form (select ?x (q- ?node !<mypredicate> ?x)),
        where ?node always returns to the argument passed to the
        generator."""
        nullRequest(self, "PUT", "/snaGenerators/" + quote(name) + "?" +
                    urlenc(subjectOf=subjectOf, objectOf=objectOf, undirected=undirected, query=query))

    def registerNeighborMatrix(self, name, group, generator, depth):
        """group is a list of nodes, generator the name of an SNA generator."""
        nullRequest(self, "PUT", "/neighborMatrices/" + quote(name) + "?" +
                    urlenc(group=group, depth=depth, generator=generator))

    def getTripleCacheSize(self):
        return jsonRequest(self, "GET", "/tripleCache") or False

    def disableTripleCache(self):
        nullRequest(self, "DELETE", "/tripleCache")

    def enableTripleCache(self, size=None):
        nullRequest(self, "PUT", "/tripleCache?" + urlenc(size=size))

    def warmup(self, includeStrings, includeTriples, indices):
        nullRequest(self, "PUT", "/warmup?" + urlenc(includeStrings=includeStrings,
                                                     includeTriples=includeTriples,
                                                     index=indices))

    def openSession(self, autocommit=False, lifetime=None, loadinitfile=False):
        if self.sessionAlive:
            # A session is already active.  Do nothing.
            return
        self.oldUrl = self.url
        self.url = jsonRequest(self, "POST", "/session?" + urlenc(autoCommit=autocommit,
            lifetime=lifetime, loadInitFile=loadinitfile))
        self._enableSession(lifetime)

    def _enableSession(self, lifetime):
        alive = self.sessionAlive = threading.Event()

        def pingSession():
            while True:
                # Block until closeSession is called, or until
                # the polling interval has been reached.
                # NOTE: wait timeout is specified in seconds.
                alive.wait(old_div(lifetime, 2) if lifetime else 250)
                if alive.isSet():
                    # closeSession has been called.  Terminate the loop
                    # (and therefore the thread)
                    return
                try: nullRequest(self, "GET", "/session/ping")
                except Exception:
                    # The ping failed.  Terminate the loop
                    # (and therefore the thread)
                    return

        pinger = threading.Thread(target=pingSession)
        # Mark the pingSession thread as a daemon thread.  This will
        # prevent it from keeping Python alive on shutdown.  This flag
        # must be set before the thread is started.
        pinger.daemon = True
        pinger.start()

    def closeSession(self):
        if not self.sessionAlive: return
        # Notify pingSession() of session close
        self.sessionAlive.set()
        self.sessionAlive = None
        try: nullRequest(self, "POST", "/session/close")
        except Exception: pass
        if hasattr(self, "oldUrl"): self.url = self.oldUrl

    def setAutoCommit(self, on):
        """Only allowed when a session is active."""
        assert self.sessionAlive, "AutoCommit can only be set on a session."
        nullRequest(self, "POST", "/session/autoCommit?" + urlenc(on=on))

    def setBulkMode(self, on):
        nullRequest(self, "PUT" if on else "DELETE", "/bulkMode")

    def getBulkMode(self):
        return jsonRequest(self, "GET", "/bulkMode")

    def __del__(self):
        self.closeSession()

    def registerEncodedIdPrefix(self, prefix, uri_format):
        return nullRequest(self, "POST", "/encodedIds/prefixes?" +
                           urlenc(prefix=prefix, format=uri_format))

    def registerEncodedIdPrefixes(self, registrations):
        body = []
        for reg in registrations:
            body.append({
                "prefix":
                    reg.prefix if hasattr(reg, "prefix") else reg[0],
                "format":
                    reg.format if hasattr(reg, "format") else reg[1]})

        nullRequest(self, "POST", "/encodedIds/prefixes",
                    content_type="application/json", body=encode_json(body))

    def listEncodedIdPrefixes(self):
        return jsonRequest(self, "GET", "/encodedIds/prefixes")

    def unregisterEncodedIdPrefix(self, prefix):
        nullRequest(self, "DELETE", "/encodedIds/prefixes?" + urlenc(prefix=prefix))

    def allocateEncodedIds(self, prefix, amount):
        return jsonRequest(self, "POST", "/encodedIds?" + urlenc(prefix=prefix,
            amount=amount))

    def deleteDuplicates(self, mode):
        nullRequest(self, "DELETE", "/statements/duplicates?" + urlenc(mode=mode))

    def getDuplicateStatements(self, mode):
        accept = "application/json"
        return jsonRequest(self, "GET", "/statements/duplicates?" + urlenc(mode=mode), accept=accept)

    def getDuplicateSuppressionPolicy(self):
        return jsonRequest(self, "GET", "/suppressDuplicates", accept="application/json") or None

    def setDuplicateSuppressionPolicy(self, mode):
        nullRequest(self, "PUT", "/suppressDuplicates?" + urlenc(type=mode or "false"))

    def disableDuplicateSuppression(self):
        nullRequest(self, "DELETE", "/suppressDuplicates")

    def callStoredProc(self, function, module, *args):
        encoded = encode(serialize(args))
        return deserialize(decode(jsonRequest(self, "POST", "/custom/"+function,
            body=urlenc(spargstr=encoded), accept="text/plain",
            headers=["x-scripts: " + module])))

    def getSpinFunction(self, uri):
        """
        Gets the string of the function for the given uri.
         uri - Spin function identifier
        """
        return jsonRequest(self, "GET", "/spin/function/" + quote(uri, ''))

    def putSpinFunction(self, uri, sparqlQuery, arguments):
        """
        Adds a Spin function.
         uri - Spin function identifier
         sparqlQuery - Spin function query text
         arguments - names of arguments in the sparqlQuery
        """
        nullRequest(self, "PUT", "/spin/function/" + quote(uri, '') + "?" + urlenc(query=sparqlQuery, arguments=arguments))

    def deleteSpinFunction(self, uri):
        """
        Deletes the Spin function at the given uri.
         uri - Spin function identifier
        """
        nullRequest(self, "DELETE", "/spin/function/" + quote(uri, ''))

    def listSpinFunctions(self):
        """
        Returns a list of defined SPIN function.
        """
        return jsonRequest(self, "GET", "/spin/function")

    def putSpinMagicProperty(self, uri, sparqlQuery, arguments):
        """
        Add a Spin magic property.
         uri - Spin magic property identifier
         sparqlQuery
         arguments - names of arguments to the sparqlQuery - must contain the leading question mark
        """
        nullRequest(self, "PUT", "/spin/magicproperty/" + quote(uri, '') + "?" + urlenc(query=sparqlQuery, arguments=arguments))

    def getSpinMagicProperty(self, uri):
        """
        Get the spin magic property for the uri
         uri - spin magic property identifier
        """
        return jsonRequest(self, "GET", "/spin/magicproperty/" + quote(uri, ''))

    def listSpinMagicProperties(self):
        """
        Returns a list of defined SPIN magic properties function.
        """
        return jsonRequest(self, "GET", "/spin/magicproperty")

    def deleteSpinMagicProperty(self, uri):
        """
        Deletes the Spin magic property at the given uri.
         uri - Spin magic property identifier
        """
        nullRequest(self, "DELETE", "/spin/magicproperty/" + quote(uri, ''))

    def materializeEntailed(self, _with=None, without=None, useTypeSubproperty=False, commit=100000):
        """
        Call to materialize entailed triples to enable reasoning queries without the dynamic query-time reasoner.
        Returns the number of triples added.

        _with and without can be either a single string or a list of strings denoting rules beyond rdfs++ you wish to use.
        See the documentation for the current set, but "same-as", "restriction", "values-from", "class", and "property" are examples.

        useTypeSubproperty tells the materializer to prefer using types that are rdfs:subPropertyOf rdf:type rather than rdf:type directly.

        commit indicates the number of triples per commit for the materializer.
        """
        args = { "with": _with, "without": without, "useTypeSubproperty": useTypeSubproperty, "commit": commit }
        return jsonRequest(self, "PUT", "/materializeEntailed?" + urlenc(**args))

    def deleteMaterialized(self):
        """
        Deletes all previously materialized triples.
        Returns the number of triples deleted.
        """
        return jsonRequest(self, "DELETE", "/materializeEntailed")

    @contextmanager
    def saveResponse(self, fileobj, accept, raiseAll=False):
        """
        Save the server response(s) for the call(s) within the with statement
        to fileobj, using accept for the response type requested.
        """
        self._saveFile = fileobj
        self._saveAccept = accept
        try:
            yield
        except RequestError:
            raise
        except:
            if raiseAll:
                raise
        finally:
            del self._saveFile
            del self._saveAccept

    def setAttributeFilter(self, attribute_filter):
        if isinstance(attribute_filter, AttributeFilter):
            attribute_filter = attribute_filter.to_expr()
        args = urlenc(filter=attribute_filter)
        return nullRequest(self, 'POST', '/attributes/staticFilter?' + args)

    def getAttributeFilter(self):
        return jsonRequest(self, 'GET', '/attributes/staticFilter')

    def clearAttributeFilter(self):
        return nullRequest(self, 'DELETE', '/attributes/staticFilter')

    def getAttributeDefinitions(self):
        return jsonRequest(self, 'GET', '/attributes/definitions')

    def getAttributeDefinition(self, name):
        return jsonRequest(
            self, 'GET', '/attributes/definitions?' + urlenc(name=name))

    def setAttributeDefinition(self, attr_def):
        args = {
            'name': attr_def.name,
            'allowed-values': attr_def.allowed_values,
            'ordered': attr_def.ordered,
            'minimum-number': attr_def.minimum_number,
            'maximum-number': attr_def.maximum_number
        }
        return nullRequest(
            self, 'POST', '/attributes/definitions?' + urlenc(**args))

    def deleteAttributeDefinition(self, name):
        return jsonRequest(
            self, 'DELETE', '/attributes/definitions?' + urlenc(name=name))

    def loadDocument(self, doc, doc_format, base=None,
                     rules=None, subject=None, prefix=None,
                     rename=None, rdf_type=None, lang=None, skip=None,
                     transform=None, graph=None, json_store_source=None,
                     csv_columns=None, csv_separator=None, csv_quote=None,
                     csv_whitespace=None, csv_double_quote=None, csv_escape=None,
                     attributes=None, commit=None, context=None,
                     encoding=None, content_encoding=None):
        mime = Format.mime_type_for_format(doc_format)
        if encoding:
            mime += ';charset=' + encoding

        if prefix is None:
            prefix = {}
        if rename is not None:
            # Copy the keys, since we will be changing some renames
            for k, v in list(six.iteritems(rename)):
                if isinstance(v, URI):
                    # To provide a full URI for a key we need to use both 'prefix'
                    # and 'rename' options
                    namespace, local_name = v.split()
                    if local_name != k:
                        rename[k] = local_name
                    prefix[k] = namespace

        prefix_list = uri_dict_to_string_list(prefix) or []
        if base is not None:
            prefix_list.append(uri_to_string(base))

        args = {
            'transform-rules': rules,
            'tr-id': uri_to_string(subject),
            'tr-prefix': prefix_list,
            'tr-rename': dict_to_string_list(rename),
            'tr-type': uri_dict_to_string_list(rdf_type),
            'tr-lang':  dict_to_string_list(lang),
            'tr-skip': skip,
            'tr-transform':  dict_to_string_list(transform),
            'tr-graph': ['%s=%s' % (k, URI(str(v)))
                         for k, v in six.iteritems(graph or {})],
            'json-store-source': bool(json_store_source),
            'csv-columns': ','.join(csv_columns) if csv_columns else None,
            'csv-separator': csv_separator,
            'csv-quote': csv_quote,
            'csv-whitespace': ','.join(csv_whitespace) if csv_whitespace is not None else None,
            'csv-double-quote-p': bool(csv_double_quote) if csv_double_quote is not None else None,
            'csv-escape': csv_escape,
            'context': str(URI(str(context))) if context else None,
            'commit': commit,
            'attributes': attributes and encode_json(attributes)
        }
        return nullRequest(self, 'POST', '/statements/transform?' + urlenc(**args),
                           doc, content_type=mime, content_encoding=content_encoding)

    def getGeneration(self):
        return jsonRequest(self, 'GET', '/generation')


def uri_to_string(uri):
    """
    Converts a URI to a string. If the input is None then None is returned.
    If the input is a string it is returned after stripping angle brackets
    (if present).
    :param uri: Either a URI object or a string.
    :type uri: str|URI|None
    :return: str|None
    """
    if uri is None:
        return None
    elif isinstance(uri, URI):
        return uri.uri
    else:
        return URI(uri).uri


def uri_dict_to_string_list(d):
    """
    Convert a dict to a list of strings of the form key=value,
    but make sure that URI objects are serialized without angle brackets.
    """
    if d is None:
        return None
    result = []
    for k, v in six.iteritems(d):
        result.append('%s=%s' % (uri_to_string(k), uri_to_string(v)))
    return result


def dict_to_string_list(d):
    """
    Converts a dict to a list of 'key=value' strings. If the input is None
    then None will be returned.
    """
    return ['%s=%s' % kv for kv in six.iteritems(d)] if d else None


def time_in_seconds(t):
    """
    Try to convert an argument to a number of seconds.

    This works on:

        - integers (which are simply returned)
        - strings (cast to ints, suffixes like s, h, .. are *not* supported)
        - timedelta objects

    :param t: Time in seconds.
    :type t: int|str|timedelta
    :return: Number of seconds.
    :rtype: int
    """
    if t is None:
        return None
    if isinstance(t, timedelta):
        # Can't use total_seconds() in Python 2.6
        return t.seconds + t.days * 24 * 3600
    return int(t)


def fix_json_ld_context(context):
    """
    Take a dict or a string and return a JSON string.

    If the input dict does not contain "@context", wrap it.
    """
    if not isinstance(context, dict) or "@context" not in context:
        context = {"@context": context}
    return encode_json(context)
