import time, cjson, math
from request import *

def listCatalogs(serverURL):
    return jsonRequest(None, "GET", serverURL + "/catalogs")

def openCatalog(serverURL, catalog, user=None, password=None):
    return Catalog(serverURL + catalog, user, password)

class Catalog:
    def __init__(self, url, user=None, password=None):
        self.url = url
        self.user = user
        self.password = password

    def listTripleStores(self):
        """Returns the names of open stores on the server."""
        repos = jsonRequest(self, "GET", "/repositories")
        return [repo["id"] for repo in repos]

    def createTripleStore(self, name):
        """Ask the server to create a new triple store."""
        nullRequest(self, "PUT", "/repositories/" + urllib.quote(name))

    def federateTripleStores(self, name, storeNames):
        """Create a federated store."""
        nullRequest(self, "PUT", "/repositories/" + urllib.quote(name) +
                    "?" + urlenc(federate=storeNames))

    def openRemoteStore(self, name, host, port, file):
        nullRequest(self, "PUT", "/repositories/" + urllib.quote(name) +
                    "?" + urlenc(host=host, port=port, file=file))

    def deleteTripleStore(self, name):
        """Delete a server-side triple store."""
        nullRequest(self, "DELETE", "/repositories/" + urllib.quote(name))

    def getRepository(self, name):
        """Create an access object for a triple store."""
        return Repository(self.url + "/repositories/" + urllib.quote(name), self.user, self.password)


class Repository:
    def __init__(self, url, user=None, password=None):
        # TODO verify existence of repository at this point?
        self.url = url
        self.user = user
        self.password = password
        self.environment = None

    def getSize(self, context=None):
        """Returns the amount of triples in the repository."""
        return jsonRequest(self, "GET", "/size", urlenc(context=context))

    def listContexts(self):
        """Lists the contexts (named graphs) that are present in this repository."""
        return [t["contextID"] for t in jsonRequest(self, "GET", "/contexts")]

    def isWriteable(self):
        return jsonRequest(self, "GET", "/writeable")

    def evalSparqlQuery(self, query, infer=False, context=None, namedContext=None, callback=None,
                        bindings=None, planner=None, checkVariables=None, count=False):
        """Execute a SPARQL query. Context can be None or a list of
        contexts -- strings in "http://foo.com" form or "null" for the
        default context. Return type depends on the query type. ASK
        gives a boolean, SELECT a {names, values} object containing
        lists of lists of terms. CONSTRUCT and DESCRIBE return a list
        of lists representing statements. Callback WILL NOT work on
        ASK queries."""
        if (bindings is not None):
            bindings = "".join(["&$" + urllib.quote(a) + "=" + urllib.quote(b.encode("utf-8")) for a, b in bindings.items()])
        return jsonRequest(self, "GET", self.url,
                           urlenc(query=query, infer=infer, context=context, namedContext=namedContext,
                                  environment=self.environment, planner=planner,
                                  checkVariables=checkVariables) + (bindings or ""),
                           rowreader=callback and RowReader(callback),
                           accept="text/integer" if count else "application/json")

    def evalPrologQuery(self, query, infer=False, callback=None, limit=None, context=None, count=False):
        """Execute a Prolog query. Returns a {names, values} object."""
        return jsonRequest(self, "POST", self.url,
                           urlenc(query=query, infer=infer, queryLn="prolog", environment=self.environment,
                                  limit=limit, context=context),
                           rowreader=callback and RowReader(callback),
                           accept="text/integer" if count else "application/json")

    def definePrologFunctors(self, definitions):
        """Add Prolog functors to the environment. Takes a string
        containing Lisp-syntax functor definitions (using the <-- and
        <- operators)."""
        nullRequest(self, "PUT", "/functor?" + urlenc(environment=self.environment), definitions)

    def deletePrologFunctor(self, name=None):
        """Delete a Prolog functor from the environment, or all of
        them if name is not given."""
        nullRequest(self, "DELETE", "/functor", urlenc(environment=self.environment, name=name))

    def registerSNAGenerator(self, name, subjectOf=None, objectOf=None, undirected=None, query=None):
        """subjectOf, objectOf, and undirected can be either a single
        predicate or a list of predicates. query should be a prolog
        query in the form (select ?x (q- ?node !<mypredicate> ?x)),
        where ?node always returns to the argument passed to the
        generator."""
        nullRequest(self, "PUT", "/snaGenerators/" + urllib.quote(name) + "?" +
                    urlenc(environment=self.environment, subjectOf=subjectOf, objectOf=objectOf,
                           undirected=undirected, query=query))

    def deleteSNAGenerator(self, name):
        nullRequest(self, "DELETE", "/snaGenerators/" + urllib.quote(name) + "?" +
                    urlenc(environment=self.environment))

    def listSNAGenerators(self):
        return jsonRequest(self, "GET", "/snaGenerators", urlenc(environment=self.environment))

    def registerNeighborMatrix(self, name, group, generator, depth):
        """group is a list of nodes, generator the name of an SNA generator."""
        nullRequest(self, "PUT", "/neighborMatrices/" + urllib.quote(name) + "?" +
                    urlenc(environment=self.environment, group=group, depth=depth, generator=generator))

    def rebuildNeighborMatrix(self, name):
        nullRequest(self, "POST", "/neighborMatrices/" + urllib.quote(name) + "?" +
                    urlenc(environment=self.environment))

    def deleteNeighborMatrix(self, name):
        nullRequest(self, "DELETE", "/neighborMatrices/" + urllib.quote(name) + "?" +
                    urlenc(environment=self.environment))

    def listNeighborMatrices(self):
        return jsonRequest(self, "GET", "/neighborMatrices", urlenc(environment=self.environment))

    def evalInServer(self, code):
        """Evaluate Common Lisp code in the server."""
        return jsonRequest(self, "POST", "/eval?" + urlenc(environment=self.environment), code)

    def getStatements(self, subj=None, pred=None, obj=None, context=None, infer=False, callback=None,
                      limit=None, tripleIDs=False, count=False):
        """Retrieve all statements matching the given constraints.
        Context can be None or a list of contexts, as in
        evalSparqlQuery."""
        if subj == [] or pred == [] or obj == [] or context == []: return []
        subjEnd, predEnd, objEnd = None, None, None
        if isinstance(subj, tuple): subj, subjEnd = subj
        if isinstance(pred, tuple): pred, predEnd = pred
        if isinstance(obj, tuple): obj, objEnd = obj

        accept = "application/json"
        if count: accept = "application/x-quints+json"
        elif tripleIDs: accept = "text/integer"
        return jsonRequest(self, "GET", "/statements",
                           urlenc(subj=subj, subjEnd=subjEnd, pred=pred, predEnd=predEnd,
                                  obj=obj, objEnd=objEnd, context=context, infer=infer, limit=limit),
                           rowreader=callback and RowReader(callback), accept=accept)

    def getStatementsById(self, ids, returnIDs=True):
        return jsonRequest(self, "GET", "/statements/id", urlenc(id=ids),
                           accept=(returnIDs and "application/x-quints+json") or "application/json")

    def addStatement(self, subj, pred, obj, context=None):
        """Add a single statement to the repository."""
        ##print "ADD STATEMENT CONTEXT '%s' " % context, type(context)
        nullRequest(self, "POST", "/statements", cjson.encode([[subj, pred, obj, context]]),
                    contentType="application/json")

    def deleteMatchingStatements(self, subj=None, pred=None, obj=None, context=None):
        """Delete all statements matching the constraints from the
        repository. Context can be None or a single graph name."""
        nullRequest(self, "DELETE", "/statements",
                    urlenc(subj=subj, pred=pred, obj=obj, context=context))

    def addStatements(self, quads):
        """Add a collection of statements to the repository. Quads
        should be an array of four-element arrays, where the fourth
        element, the graph name, may be None."""
        nullRequest(self, "POST", "/statements", cjson.encode(quads), contentType="application/json")

    class UnsupportedFormatError(Exception):
        def __init__(self, format): self.format = format
        def __str__(self): return "'%s' file format not supported (try 'ntriples' or 'rdf/xml')." % self.format

    def checkFormat(self, format):
        if format == "ntriples": return "text/plain"
        elif format == "rdf/xml": return "application/rdf+xml"
        else: raise Repository.UnsupportedFormatError(format)

    def loadData(self, data, format, baseURI=None, context=None):
        nullRequest(self, "POST", "/statements?" + urlenc(context=context, baseURI=baseURI),
                    data.encode("utf-8"), contentType=self.checkFormat(format))

    def loadFile(self, file, format, baseURI=None, context=None, serverSide=False):
        mime = self.checkFormat(format)
        body = ""
        if not serverSide:
            f = open(file)
            body = f.read()
            f.close()
            file = None
        params = urlenc(file=file, context=context, baseURI=baseURI)
        nullRequest(self, "POST", "/statements?" + params, body, contentType=mime)

    def getBlankNodes(self, amount=1):
        return jsonRequest(self, "POST", "/blankNodes", urlenc(amount=amount))

    def deleteStatements(self, quads):
        """Delete a collection of statements from the repository."""
        nullRequest(self, "POST", "/statements/delete", cjson.encode(quads), contentType="application/json")

    def deleteStatementsById(self, ids):
        nullRequest(self, "POST", "/statements/delete?ids=true", cjson.encode(ids), contentType="application/json")

    def listIndices(self):
        """List the SPOGI-indices that are active in the repository."""
        return jsonRequest(self, "GET", "/indices")

    def addIndex(self, type):
        """Register a SPOGI index."""
        nullRequest(self, "PUT", "/indices/" + type)

    def deleteIndex(self, type):
        """Drop a SPOGI index."""
        nullRequest(self, "DELETE", "/indices/" + type)

    def getIndexCoverage(self):
        """Returns the proportion (0-1) of the repository that is indexed."""
        return jsonRequest(self, "GET", "/indexing")

    def indexStatements(self, all=False):
        """Index any unindexed statements in the repository. If all is
        True, the whole repository is re-indexed."""
        nullRequest(self, "POST", "/indexing", urlenc(all=all))

    def setIndexingTripleThreshold(self, size=None):
        nullRequest(self, "PUT", "/indexing/tripleThreshold", "%d" % (size or 0),
                    contentType="text/plain")

    def setIndexingChunkThreshold(self, size=None):
        nullRequest(self, "PUT", "/indexing/chunkThreshold", "%d" % (size or 0),
                    contentType="text/plain")

    def getTripleCacheSize(self):
        return jsonRequest(self, "GET", "/tripleCache") or False

    def disableTripleCache(self):
        nullRequest(self, "DELETE", "/tripleCache")

    def enableTripleCache(self, size=None):
        nullRequest(self, "PUT", "/tripleCache?" + urlenc(size=size))

    def evalFreeTextSearch(self, pattern, infer=False, callback=None, limit=None):
        """Use free-text indices to search for the given pattern.
        Returns an array of statements."""
        return jsonRequest(self, "GET", "/freetext", urlenc(pattern=pattern, infer=infer, limit=limit),
                           rowreader=callback and RowReader(callback))

    def listFreeTextPredicates(self):
        """List the predicates that are used for free-text indexing."""
        return jsonRequest(self, "GET", "/freetextPredicates")

    def registerFreeTextPredicate(self, predicate):
        """Add a predicate for free-text indexing."""
        nullRequest(self, "POST", "/freetextPredicates", urlenc(predicate=predicate))

    def updateFreeTextIndexing(self):
        nullRequest(self, "POST", "/freetextPredicates/update")

    def setEnvironment(self, name):
        """Repositories use a current environment, which are
        containers for namespaces and Prolog predicates. Every
        server-side repository has a default environment that is used
        when no environment is specified."""
        self.environment = name

    def listEnvironments(self):
        return jsonRequest(self, "GET", "/environments")

    def createEnvironment(self, name=None):
        return jsonRequest(self, "POST", "/environments", urlenc(name=name))

    def deleteEnvironment(self, name):
        nullRequest(self, "DELETE", "/environments", urlenc(name=name))

    def listNamespaces(self):
        return jsonRequest(self, "GET", "/namespaces", urlenc(environment=self.environment))

    def clearNamespaces(self):
        nullRequest(self, "DELETE", "/namespaces?" + urlenc(environment=self.environment))

    def addNamespace(self, prefix, uri):
        nullRequest(self, "PUT", "/namespaces/" + urllib.quote(prefix) + "?"
                    + urlenc(environment=self.environment), uri, contentType="text/plain")

    def deleteNamespace(self, prefix):
        nullRequest(self, "DELETE", "/namespaces/" + urllib.quote(prefix) + "?"
                    + urlenc(environment=self.environment))

    def listMappedTypes(self):
        return jsonRequest(self, "GET", "/typeMapping")

    def addMappedType(self, type, primitiveType):
        nullRequest(self, "POST", "/typeMapping", urlenc(type=type, primitiveType=primitiveType))

    def deleteMappedType(self, type):
        nullRequest(self, "DELETE", "/typeMapping", urlenc(type=type))

    def listMappedPredicates(self):
        return jsonRequest(self, "GET", "/predicateMapping")

    def addMappedPredicate(self, predicate, primitiveType):
        nullRequest(self, "POST", "/predicateMapping",
                    urlenc(predicate=predicate, primitiveType=primitiveType))

    def deleteMappedPredicate(self, predicate):
        nullRequest(self, "DELETE", "/predicateMapping", urlenc(predicate=predicate))

    def getCartesianGeoType(self, stripWidth, xMin, xMax, yMin, yMax):
        """Retrieve a cartesian geo-spatial literal type."""
        return jsonRequest(self, "PUT", "/geo/types/cartesian?" +
                           urlenc(stripWidth=stripWidth, xmin=xMin, ymin=yMin, xmax=xMax, ymax=yMax))


    def getSphericalGeoType(self, stripWidth, unit="degree", latMin=None, latMax=None, longMin=None, longMax=None):
        """Retrieve a spherical geo-spatial literal type."""
        return jsonRequest(self, "PUT", "/geo/types/spherical?" +
                           urlenc(stripWidth=stripWidth, unit=unit, latmin=latMin, latmax=latMax,
                                  longmin=longMin, longmax=longMax))

    def listGeoTypes(self):
        """List the geo-spatial types registered in the store."""
        return jsonRequest(self, "GET", "/geo/types")

    def createCartesianGeoLiteral(self, type, x, y):
        """Create a geo-spatial literal of the given type."""
        return "\"%+g%+g\"^^<%s>" % (x, y, type)

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
                number = -number;
            fl = math.floor(number)
            return sign + (("%%0%dd" % digits) % fl) + (".%07d" % ((number - fl) * 10000000))

        conv = self.unitDegreeFactor(unit)
        return "\"%s%s\"^^<%s>" % (asISO6709(lat * conv, 2), asISO6709(long * conv, 3), type)

    def getStatementsHaversine(self, type, predicate, lat, long, radius, unit="km", limit=None):
        """Get all the triples with a given predicate whose object
        lies within radius units from the given latitude/longitude."""
        return jsonRequest(self, "GET", "/geo/haversine",
                           urlenc(type=type, predicate=predicate, lat=lat, long=long, radius=radius, unit=unit, limit=limit))

    def getStatementsInsideBox(self, type, predicate, xMin, xMax, yMin, yMax, limit=None):
        """Get all the triples with a given predicate whose object
        lies within the specified box."""
        return jsonRequest(self, "GET", "/geo/box",
                           urlenc(type=type, predicate=predicate, xmin=xMin, xmax=xMax, ymin=yMin, ymax=yMax, limit=limit))

    def getStatementsInsideCircle(self, type, predicate, x, y, radius, limit=None):
        """Get all the triples with a given predicate whose object
        lies within the specified circle."""
        return jsonRequest(self, "GET", "/geo/circle",
                           urlenc(type=type, predicate=predicate, x=x, y=y, radius=radius, limit=limit))

    def getStatementsInsidePolygon(self, type, predicate, polygon, limit=None):
        """Get all the triples with a given predicate whose object
        lies within the specified polygon (see createPolygon)."""
        return jsonRequest(self, "GET", "/geo/polygon",
                           urlenc(type=type, predicate=predicate, polygon=polygon, limit=limit))

    def createPolygon(self, resource, points):
        """Create a polygon with the given name in the store. points
        should be a list of literals created with createCartesianGeoLiteral."""
        nullRequest(self, "PUT", "/geo/polygon?" + urlenc(resource=resource, point=points))
