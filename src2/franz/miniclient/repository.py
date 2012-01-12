###############################################################################
# Copyright (c) 2006-2009 Franz Inc.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the Eclipse Public License v1.0
# which accompanies this distribution, and is available at
# http://www.eclipse.org/legal/epl-v10.html
###############################################################################

import time, cjson, math, operator, re, threading, urllib
from request import *

class Service(object):
    def __init__(self, url, user=None, password=None):
        self.url = url
        self.user = user
        self.password = password
        self.runAsName = None

    def _instanceFromUrl(self, constructor, url):
        return constructor(url, self.user, self.password)
    
    def toBaseClient(self):
        url = re.match("^https?://[^/]+", self.url).group(0)
        return self._instanceFromUrl(Client, url)


class Catalog(Service):
    def listRepositories(self):
        """Returns the names of repositories in the catalog."""
        repos = jsonRequest(self, "GET", "/repositories")
        return [repo["id"] for repo in repos]

    def createRepository(self, name, indices=None):
        """Ask the server to create a new repository."""
        nullRequest(self, "PUT", "/repositories/" + urllib.quote(name) + "?" +
            urlenc(index=indices))
        return self.getRepository(name)

    def deleteRepository(self, name):
        """Delete a repository in this catalog."""
        nullRequest(self, "DELETE", "/repositories/" + urllib.quote(name))

    def getRepository(self, name):
        """Create an access object for a triple store."""
        return self._instanceFromUrl(Repository, self.url + "/repositories/" + urllib.quote(name))


class Client(Catalog):
    def getVersion(self):
        return jsonRequest(self, "GET", "/version")
    
    def listCatalogs(self):
        return [cat["id"] if cat["id"] != "/" else None for cat in jsonRequest(self, "GET", "/catalogs")]

    def openCatalog(self, uriOrName):
        if (uriOrName.startswith("http://")):
            return self._instanceFromUrl(Catalog, uriOrName)
        else:
            return self.openCatalogByName(uriOrName)

    def openCatalogByName(self, name=None):
        url = self.url
        if name and name != "~":
            url += "/catalogs/" + urllib.quote(name)
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
        nullRequest(self, "PUT", "/scripts/" + module,
            body=code)

    def getScript(self, module):
        return jsonRequest(self, "GET", "/scripts/" + module)

    def deleteScript(self, module):
        nullRequest(self, "DELETE", "/scripts/" + module)

    def openSession(self, spec, autocommit=False, lifetime=None, loadinitfile=False):
        url = jsonRequest(self, "POST", "/session?" +
                          urlenc(autoCommit=autocommit, lifetime=lifetime,
                                 loadInitFile=loadinitfile, store=spec))
        rep = self._instanceFromUrl(Repository, url)
        rep._enableSession(lifetime)
        return rep

    def listUsers(self):
        return jsonRequest(self, "GET", "/users")

    def addUser(self, name, password=None):
        assert password is not None or name == "anonymous"
        nullRequest(self, "PUT", "/users/" + urllib.quote(name) + "?" + urlenc(password=password))

    def deleteUser(self, name):
        nullRequest(self, "DELETE", "/users/" + urllib.quote(name))

    def changeUserPassword(self, name, password):
        nullRequest(self, "POST", "/users/" + urllib.quote(name) + "/password", password) 

    def listUserAccess(self, name):
        return jsonRequest(self, "GET", "/users/" + urllib.quote(name) + "/access")

    def addUserAccess(self, name, read, write, catalog, repository):
        nullRequest(self, "PUT", "/users/" + urllib.quote(name) + "/access?" +
            urlenc(read=read, write=write, catalog=catalog, repository=repository))

    def deleteUserAccess(self, name, read, write, catalog, repository):
        nullRequest(self, "DELETE", "/users/" + urllib.quote(name) + "/access?" +
            urlenc(read=read, write=write, catalog=catalog, repository=repository))

    def listUserEffectiveAccess(self, name):
        return jsonRequest(self, "GET", "/users/" + urllib.quote(name) + "/effectiveAccess")


    def listUserPermissions(self, name):
        return jsonRequest(self, "GET", "/users/" + urllib.quote(name) + "/permissions")

    def listUserEffectivePermissions(self, name):
        return jsonRequest(self, "GET", "/users/" + urllib.quote(name) + "/effectivePermissions")

    def addUserPermission(self, name, _type):
        nullRequest(self, "PUT", "/users/" + urllib.quote(name) + "/permissions/" + _type)

    def deleteUserPermission(self, name, _type):
        nullRequest(self, "DELETE", "/users/" + urllib.quote(name) + "/permissions/" + _type)

    def listRoles(self):
        return jsonRequest(self, "GET", "/roles")

    def addRole(self, role):
        nullRequest(self, "PUT", "/roles/" + urllib.quote(role))

    def deleteRole(self, role):
        nullRequest(self, "DELETE", "/roles/" + urllib.quote(role))

    def listRolePermissions(self, role):
        return jsonRequest(self, "GET", "/roles/" + urllib.quote(role) + "/permissions")

    def addRolePermission(self, role, _type):
        nullRequest(self, "PUT", "/roles/" + urllib.quote(role) + "/permissions/" + _type)

    def deleteRolePermission(self, role, _type):
        nullRequest(self, "DELETE", "/roles/" + urllib.quote(role) + "/permissions/" + _type)

    def listRoleAccess(self, role):
        return jsonRequest(self, "GET", "/roles/" + urllib.quote(role) + "/access")

    def addRoleAccess(self, role, read, write, catalog, repository):
        nullRequest(self, "PUT", "/roles/" + urllib.quote(role) + "/access?" +
            urlenc(read=read, write=write, catalog=catalog, repository=repository))

    def deleteRoleAccess(self, role, read, write, catalog, repository):
        nullRequest(self, "DELETE", "/roles/" + urllib.quote(role) + "/access?" +
            urlenc(read=read, write=write, catalog=catalog, repository=repository))

    def listUserRoles(self, name):
        return jsonRequest(self, "GET", "/users/" + urllib.quote(name) + "/roles")

    def addUserRole(self, name, role):
        nullRequest(self, "PUT", "/users/" + urllib.quote(name) + "/roles/" + urllib.quote(role))

    def deleteUserRole(self, name, role):
        nullRequest(self, "DELETE", "/users/" + urllib.quote(name) + "/roles/" + urllib.quote(role))

    def listUserSecurityFilters(self, name, _type):
        return jsonRequest(self, "GET", "/users/" + urllib.quote(name) + '/security-filters/' + _type)

    def addUserSecurityFilter(self, name, _type, s=None, p=None, o=None, g=None):
        nullRequest(self, "POST", "/users/" + urllib.quote(name) + "/security-filters/" + _type + "?" +
            urlenc(s=s, p=p, o=o, g=g))

    def deleteUserSecurityFilter(self, name, _type, s=None, p=None, o=None, g=None):
        nullRequest(self, "DELETE", "/users/" + urllib.quote(name) + "/security-filters/" + _type + "?" +
            urlenc(s=s, p=p, o=o, g=g))

    def listRoleSecurityFilters(self, role, _type):
        return jsonRequest(self, "GET", "/roles/" + urllib.quote(role) + '/security-filters/' + _type)

    def addRoleSecurityFilter(self, role, _type, s=None, p=None, o=None, g=None):
        nullRequest(self, "POST", "/roles/" + urllib.quote(role) + "/security-filters/" + _type + "?" +
            urlenc(s=s, p=p, o=o, g=g))

    def deleteRoleSecurityFilter(self, role, _type, s=None, p=None, o=None, g=None):
        nullRequest(self, "DELETE", "/roles/" + urllib.quote(role) + "/security-filters/" + _type + "?" +
            urlenc(s=s, p=p, o=o, g=g))


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
                        analysisTechnique=None, analysisTimeout=None):
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
            bindings = "".join(["&$" + urllib.quote(a) + "=" + urllib.quote(b.encode("utf-8")) for a, b in bindings.items()])
        return jsonRequest(self, "GET", self.url,
                   urlenc(query=query, infer=infer, context=context, namedContext=namedContext,
                       planner=planner, checkVariables=checkVariables,
                       analyzeIndicesUsed=analyze, queryAnalysisTechnique=analysisTechnique,
                       queryAnalysisTimeout=analysisTimeout) + (bindings or ""),
                       rowreader=callback and RowReader(callback),
                       accept=accept)

    def evalPrologQuery(self, query, infer=False, callback=None, limit=None, count=False, accept=None):
        """Execute a Prolog query. Returns a {names, values} object."""
        if accept is None:
            accept="text/integer" if count else "application/json"
        return jsonRequest(self, "POST", self.url,
                           urlenc(query=query, infer=infer, queryLn="prolog", limit=limit),
                           rowreader=callback and RowReader(callback),
                           accept=accept)

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
                      limit=None, offset=None, tripleIDs=False, count=False):
        """Retrieve all statements matching the given constraints.
        Context can be None or a list of contexts, as in
        evalSparqlQuery."""
        if subj == [] or pred == [] or obj == [] or context == []: return []
        subjEnd, predEnd, objEnd = None, None, None
        if isinstance(subj, tuple): subj, subjEnd = subj
        if isinstance(pred, tuple): pred, predEnd = pred
        if isinstance(obj, tuple): obj, objEnd = obj

        accept = "application/json"
        if count: accept = "text/integer"
        elif tripleIDs: accept = "application/x-quints+json"
        return jsonRequest(self, "GET", "/statements",
            urlenc(subj=subj, subjEnd=subjEnd, pred=pred, predEnd=predEnd,
                obj=obj, objEnd=objEnd, context=context, infer=infer,
                limit=limit, offset=offset),
            rowreader=callback and RowReader(callback), accept=accept)

    def getStatementsById(self, ids, returnIDs=True):
        return jsonRequest(self, "GET", "/statements/id", urlenc(id=ids),
            accept=(returnIDs and "application/x-quints+json") or "application/json")

    def addStatement(self, subj, pred, obj, context=None):
        """Add a single statement to the repository."""
        nullRequest(self, "POST", "/statements", cjson.encode([[subj, pred, obj, context]]),
                    contentType="application/json")

    def deleteMatchingStatements(self, subj=None, pred=None, obj=None, context=None):
        """Delete all statements matching the constraints from the
        repository. Context can be None or a single graph name."""
        nullRequest(self, "DELETE", "/statements",
                    urlenc(subj=subj, pred=pred, obj=obj, context=context))

    def addStatements(self, quads, commitEvery=None):
        """Add a collection of statements to the repository. Quads
        should be an array of four-element arrays, where the fourth
        element, the graph name, may be None."""
        nullRequest(self, "POST", "/statements?" + urlenc(commit=commitEvery),
            cjson.encode(quads), contentType="application/json")

    class UnsupportedFormatError(Exception):
        def __init__(self, format): self.format = format
        def __str__(self): return "'%s' file format not supported (try 'ntriples' or 'rdf/xml')." % self.format

    def checkFormat(self, format):
        if format == "ntriples": return "text/plain"
        elif format == "rdf/xml": return "application/rdf+xml"
        else: raise Repository.UnsupportedFormatError(format)

    def loadData(self, data, format, baseURI=None, context=None, commitEvery=None):
        nullRequest(self, "POST", "/statements?" + urlenc(context=context, baseURI=baseURI, commit=commitEvery),
                    data.encode("utf-8"), contentType=self.checkFormat(format))

    def loadFile(self, file, format, baseURI=None, context=None, serverSide=False, commitEvery=None):
        mime = self.checkFormat(format)
        body = ""
        if not serverSide:
            f = open(file)
            body = f.read()
            f.close()
            file = None
        params = urlenc(file=file, context=context, baseURI=baseURI, commit=commitEvery)
        nullRequest(self, "POST", "/statements?" + params, body, contentType=mime)

    def getBlankNodes(self, amount=1):
        return jsonRequest(self, "POST", "/blankNodes", urlenc(amount=amount))

    def deleteStatements(self, quads):
        """Delete a collection of statements from the repository."""
        nullRequest(self, "POST", "/statements/delete", cjson.encode(quads), contentType="application/json")

    def deleteStatementsById(self, ids):
        nullRequest(self, "POST", "/statements/delete?ids=true", cjson.encode(ids), contentType="application/json")

    def evalFreeTextSearch(self, pattern, index=None, infer=False, callback=None, limit=None, offset=None):
        """Use free-text indices to search for the given pattern.
        Returns an array of statements."""
        return jsonRequest(self, "GET", "/freetext", urlenc(pattern=pattern, infer=infer, limit=limit, offset=offset, index=index),
                           rowreader=callback and RowReader(callback))

    def listFreeTextIndices(self):
        """List the names of free-text indices defined in this
        repository."""
        return jsonRequest(self, "GET", "/freetext/indices")

    def getFreeTextIndexConfiguration(self, index):
        """Returns a dictionary with fields \"predicates\",
        \"indexLiterals\", \"indexResources\", \"indexFields\",
        \"minimumWordSize\", \"stopWords\", and \"wordFilters\"."""
        return jsonRequest(self, "GET", "/freetext/indices/" + urllib.quote(index))

    def createFreeTextIndex(self, index, predicates=None, indexLiterals=None, indexResources=None,
                            indexFields=None, minimumWordSize=None, stopWords=None, wordFilters=None,
                            innerChars=None, borderChars=None, tokenizer=None):
        """Create a free-text index. predicates, if given, should be a
        list of resources. indexLiterals can be True, False, or a list
        of resources, indicating the literal types to index.
        indexResources can be True, False, or \"short\". indexFields
        can be a list containing any combination of the elements
        \"subject\", \"predicate\", \"object\", and \"graph\".
        minimumWordSize is an integer, and stopWords a list of
        strings. wordFilter must be a list of filter names.

        "innerChars" and "borderChars" can be lists. "tokenizer" is
        a string. See documentation:

        http://www.franz.com/agraph/support/documentation/v4/http-protocol.html#put-freetext-index"""
        # Not passing these at all causes the defaults to be used. So
        # when they are given, they should be passed with an empty
        # value.
        if stopWords == []: stopWords = ""
        if indexFields == []: indexFields = ""
        nullRequest(self, "PUT", "/freetext/indices/" + urllib.quote(index),
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
        nullRequest(self, "POST", "/freetext/indices/" + urllib.quote(index),
                    urlenc(predicate=predicates, indexLiterals=indexLiterals and True,
                           indexLiteralType=indexLiterals if isinstance(indexLiterals, list) else None,
                           indexResources=indexResources, indexField=indexFields,
                           minimumWordSize=minimumWordSize, stopWord=stopWords, wordFilter=wordFilters,
                           reIndex=reIndex, innerChars=innerChars, borderChars=borderChars,
                           tokenizer=tokenizer))

    def deleteFreeTextIndex(self, index):
        """Delete the named free-text index."""
        nullRequest(self, "DELETE", "/freetext/indices/" + urllib.quote(index))

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
        nullRequest(self, "PUT", "/namespaces/" + urllib.quote(prefix),
                    uri, contentType="text/plain")

    def deleteNamespace(self, prefix):
        nullRequest(self, "DELETE", "/namespaces/" + urllib.quote(prefix))

    def listNamespaces(self):
        return jsonRequest(self, "GET", "/namespaces")

    def getNamespace(self, prefix):
        return jsonRequest(self, "GET", "/namespaces/" + urllib.quote(prefix))

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
        nullRequest(self, "PUT", "/indices/" + urllib.quote(_type))

    def dropIndex(self, _type):
        nullRequest(self, "DELETE", "/indices/" + urllib.quote(_type))

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

    class UnsupportedUnitError(Exception):
        def __init__(self, unit): self.unit = unit
        def __str__(self): return "'%s' is not a known unit (use km, mile, degree, or radian)." % self.unit

    def unitDegreeFactor(self, unit):
        if unit == "degree": return 1.0
        elif unit == "radian": return 57.29577951308232
        elif unit == "km": return 0.008998159
        elif unit == "mile": return 0.014481134
        else: raise Repository.UnsupportedUnitError(unit)

    def createSphericalGeoLiteral(self, type, lat, long, unit="degree"):
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
        return "\"%s%s\"^^%s" % (asISO6709(lat * conv, 2), asISO6709(long * conv, 3), type)

    def getStatementsHaversine(self, type, predicate, lat, long, radius, unit="km", limit=None, offset=None):
        """Get all the triples with a given predicate whose object
        lies within radius units from the given latitude/longitude."""
        return jsonRequest(self, "GET", "/geo/haversine",
                           urlenc(type=type, predicate=predicate, lat=lat, long=long, radius=radius, unit=unit, limit=limit, offset=offset))

    def getStatementsInsideBox(self, type, predicate, xMin, xMax, yMin, yMax, limit=None, offset=None):
        """Get all the triples with a given predicate whose object
        lies within the specified box."""
        return jsonRequest(self, "GET", "/geo/box",
                           urlenc(type=type, predicate=predicate, xmin=xMin, xmax=xMax, ymin=yMin, ymax=yMax, limit=limit, offset=offset))

    def getStatementsInsideCircle(self, type, predicate, x, y, radius, limit=None, offset=None):
        """Get all the triples with a given predicate whose object
        lies within the specified circle."""
        return jsonRequest(self, "GET", "/geo/circle",
                           urlenc(type=type, predicate=predicate, x=x, y=y, radius=radius, limit=limit, offset=offset))

    def getStatementsInsidePolygon(self, type, predicate, polygon, limit=None, offset=None):
        """Get all the triples with a given predicate whose object
        lies within the specified polygon (see createPolygon)."""
        return jsonRequest(self, "GET", "/geo/polygon",
                           urlenc(type=type, predicate=predicate, polygon=polygon, limit=limit, offset=offset))

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
        nullRequest(self, "PUT", "/snaGenerators/" + urllib.quote(name) + "?" +
                    urlenc(subjectOf=subjectOf, objectOf=objectOf, undirected=undirected, query=query))

    def registerNeighborMatrix(self, name, group, generator, depth):
        """group is a list of nodes, generator the name of an SNA generator."""
        nullRequest(self, "PUT", "/neighborMatrices/" + urllib.quote(name) + "?" +
                    urlenc(group=group, depth=depth, generator=generator))

    def getTripleCacheSize(self):
        return jsonRequest(self, "GET", "/tripleCache") or False

    def disableTripleCache(self):
        nullRequest(self, "DELETE", "/tripleCache")

    def enableTripleCache(self, size=None):
        nullRequest(self, "PUT", "/tripleCache?" + urlenc(size=size))

    def openSession(self, autocommit=False, lifetime=None, loadinitfile=False):
        if self.sessionAlive: return
        self.oldUrl = self.url
        self.url = jsonRequest(self, "POST", "/session?" + urlenc(autoCommit=autocommit,
            lifetime=lifetime, loadInitFile=loadinitfile))
        self._enableSession(lifetime)

    def _enableSession(self, lifetime):
        alive = self.sessionAlive = threading.Event()
        def pingSession():
            while True:
                stop = alive.wait(max(lifetime - 60, lifetime) if lifetime else 250)
                if alive.isSet(): return
                try: nullRequest(self, "GET", "/session/ping")
                except Exception: return
        threading.Thread(target=pingSession).start()

    def closeSession(self):
        if not self.sessionAlive: return
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

    def registerEncodedIdPrefix(self, prefix, format):
        return nullRequest(self, "POST", "/encodedIds/prefixes?" +
            urlenc(prefix=prefix, format=format))

    def registerEncodedIdPrefixes(self, registrations):
        body = []
        for reg in registrations:
            body.append({
                "prefix":
                    reg.prefix if hasattr(reg, "prefix") else reg[0],
                "format":
                    reg.format if hasattr(reg, "format") else reg[1]})

        nullRequest(self, "POST", "/encodedIds/prefixes",
            contentType="application/json", body=cjson.encode(body))

    def listEncodedIdPrefixes(self):
        return jsonRequest(self, "GET", "/encodedIds/prefixes")

    def unregisterEncodedIdPrefix(self, prefix):
        nullRequest(self, "DELETE", "/encodedIds/prefixes?" + urlenc(prefix=prefix))

    def allocateEncodedIds(self, prefix, amount):
        return jsonRequest(self, "POST", "/encodedIds?" + urlenc(prefix=prefix,
            amount=amount))

    def deleteDuplicates(self, mode):
        nullRequest(self, "DELETE", "/statements/duplicates?" + urlenc(mode=mode))

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
        return jsonRequest(self, "GET", "/spin/function/" + urllib.quote(uri, ''))

    def putSpinFunction(self, uri, sparqlQuery, arguments):
        """
        Adds a Spin function.
         uri - Spin function identifier
         sparqlQuery - Spin function query text
         arguments - names of arguments in the sparqlQuery
        """
        nullRequest(self, "PUT", "/spin/function/" + urllib.quote(uri, '') + "?" + urlenc(query=sparqlQuery, arguments=arguments))

    def deleteSpinFunction(self, uri):
        """
        Deletes the Spin function at the given uri.
         uri - Spin function identifier
        """
        nullRequest(self, "DELETE", "/spin/function/" + urllib.quote(uri, ''))

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
        nullRequest(self, "PUT", "/spin/magicproperty/" + urllib.quote(uri, '') + "?" + urlenc(query=sparqlQuery, arguments=arguments))

    def getSpinMagicProperty(self, uri):
        """
        Get the spin magic property for the uri
         uri - spin magic property identifier
        """
        return jsonRequest(self, "GET", "/spin/magicproperty/" + urllib.quote(uri, ''))

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
        nullRequest(self, "DELETE", "/spin/magicproperty/" + urllib.quote(uri, ''))

