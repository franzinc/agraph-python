import time, cjson, math
from request import *

def listCatalogs(serverURL, user=None, password=None):
    curl = pycurl.Curl()
    if user:
        curl.setopt(pycurl.USERPWD, "%s:%s" % (user, password))
        curl.setopt(pycurl.HTTPAUTH, pycurl.HTTPAUTH_BASIC)
    return jsonRequest(curl, "GET", serverURL + "/catalogs")

def openCatalog(serverURL, catalog, user=None, password=None):
    return Catalog(serverURL + catalog, user, password)

def rootCatalog(serverURL, user=None, password=None):
    return Catalog(serverURL, user, password)

class Catalog:
    def __init__(self, url, user=None, password=None):
        self.url = url
        self.curl = pycurl.Curl()
        if user and password: self.setAuth(user, password)

    def listRepositories(self):
        """Returns the names of repositories in the catalog."""
        repos = jsonRequest(self.curl, "GET", self.url + "/repositories")
        return [repo["id"] for repo in repos]

    def createRepository(self, name):
        """Ask the server to create a new repository."""
        nullRequest(self.curl, "PUT", self.url + "/repositories/" + urllib.quote(name))
        return self.getRepository(name)

    def deleteRepository(self, name):
        """Delete a repository in this catalog."""
        nullRequest(self.curl, "DELETE", self.url + "/repositories/" + urllib.quote(name))

    # def federateTripleStores(self, name, storeNames):
    #     """Create a federated store."""
    #     nullRequest(self.curl, "PUT", self.url + "/repositories/" + urllib.quote(name) +
    #                 "?" + urlenc(federate=storeNames))

    def getRepository(self, name):
        """Create an access object for a triple store."""
        return Repository(self.curl, self.url + "/repositories/" + urllib.quote(name))

    def setAuth(self, user, password):
        """Set a username and password to use when talking to this server."""
        self.curl.setopt(pycurl.USERPWD, "%s:%s" % (user, password))
        self.curl.setopt(pycurl.HTTPAUTH, pycurl.HTTPAUTH_BASIC)


class Repository:
    def __init__(self, curl, url):
        # TODO verify existence of repository at this point?
        self.url = url
        self.curl = curl

    def getSize(self, context=None):
        """Returns the amount of triples in the repository."""
        return jsonRequest(self.curl, "GET", self.url + "/size", urlenc(context=context))

    def listContexts(self):
        """Lists the contexts (named graphs) that are present in this repository."""
        return [t["contextID"] for t in jsonRequest(self.curl, "GET", self.url + "/contexts")]

    def evalSparqlQuery(self, query, infer=False, context=None, namedContext=None, callback=None,
                        bindings=None, planner=None, checkVariables=None):
        """Execute a SPARQL query. Context can be None or a list of
        contexts -- strings in "http://foo.com" form or "null" for the
        default context. Return type depends on the query type. ASK
        gives a boolean, SELECT a {names, values} object containing
        lists of lists of terms. CONSTRUCT and DESCRIBE return a list
        of lists representing statements. Callback WILL NOT work on
        ASK queries."""
        if (bindings is not None):
            bindings = "".join(["&$" + urllib.quote(a) + "=" + urllib.quote(b.encode("utf-8")) for a, b in bindings.items()])
        return jsonRequest(self.curl, "GET", self.url,
                           urlenc(query=query, infer=infer, context=context, namedContext=namedContext,
                                  planner=planner, checkVariables=checkVariables) + (bindings or ""),
                           rowreader=callback and RowReader(callback))

    def evalPrologQuery(self, query, infer=False, callback=None, limit=None):
        """Execute a Prolog query. Returns a {names, values} object."""
        return jsonRequest(self.curl, "POST", self.url,
                           urlenc(query=query, infer=infer, queryLn="prolog", limit=limit),
                           rowreader=callback and RowReader(callback))

    def definePrologFunctors(self, definitions):
        """Add Prolog functors to the environment. Takes a string
        containing Lisp-syntax functor definitions (using the <-- and
        <- operators)."""
        nullRequest(self.curl, "PUT", self.url + "/functor", definitions)

    def evalInServer(self, code):
        """Evaluate Common Lisp code in the server."""
        return jsonRequest(self.curl, "POST", self.url + "/eval", code)

    def getStatements(self, subj=None, pred=None, obj=None, context=None, infer=False, callback=None, limit=None, tripleIDs=False):
        """Retrieve all statements matching the given constraints.
        Context can be None or a list of contexts, as in
        evalSparqlQuery."""
        if subj == [] or pred == [] or obj == [] or context == []: return []
        subjEnd, predEnd, objEnd = None, None, None
        if isinstance(subj, tuple): subj, subjEnd = subj
        if isinstance(pred, tuple): pred, predEnd = pred
        if isinstance(obj, tuple): obj, objEnd = obj
        return jsonRequest(self.curl, "GET", self.url + "/statements",
                           urlenc(subj=subj, subjEnd=subjEnd, pred=pred, predEnd=predEnd,
                                  obj=obj, objEnd=objEnd, context=context, infer=infer, limit=limit),
                           rowreader=callback and RowReader(callback),
                           accept=(tripleIDs and "application/x-quints+json") or "application/json")

    def addStatement(self, subj, pred, obj, context=None):
        """Add a single statement to the repository."""
        ##print "ADD STATEMENT CONTEXT '%s' " % context, type(context)
        nullRequest(self.curl, "POST", self.url + "/statements", cjson.encode([[subj, pred, obj, context]]),
                    contentType="application/json")

    def deleteMatchingStatements(self, subj=None, pred=None, obj=None, context=None):
        """Delete all statements matching the constraints from the
        repository. Context can be None or a single graph name."""
        nullRequest(self.curl, "DELETE", self.url + "/statements",
                    urlenc(subj=subj, pred=pred, obj=obj, context=context))

    def addStatements(self, quads):
        """Add a collection of statements to the repository. Quads
        should be an array of four-element arrays, where the fourth
        element, the graph name, may be None."""
        nullRequest(self.curl, "POST", self.url + "/statements", cjson.encode(quads), contentType="application/json")

    class UnsupportedFormatError(Exception):
        def __init__(self, format): self.format = format
        def __str__(self): return "'%s' file format not supported (try 'ntriples' or 'rdf/xml')." % self.format

    def checkFormat(self, format):
        if format == "ntriples": return "text/plain"
        elif format == "rdf/xml": return "application/rdf+xml"
        else: raise Repository.UnsupportedFormatError(format)

    def loadData(self, data, format, baseURI=None, context=None):
        nullRequest(self.curl, "POST", self.url + "/statements?" + urlenc(context=context, baseURI=baseURI),
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
        nullRequest(self.curl, "POST", self.url + "/statements?" + params, body, contentType=mime)

    def getBlankNodes(self, amount=1):
        return jsonRequest(self.curl, "POST", self.url + "/blankNodes", urlenc(amount=amount))

    def deleteStatements(self, quads):
        """Delete a collection of statements from the repository."""
        nullRequest(self.curl, "POST", self.url + "/statements/delete", cjson.encode(quads), contentType="application/json")

    def deleteStatementsById(self, ids):
        nullRequest(self.curl, "POST", self.url + "/statements/delete?ids=true", cjson.encode(ids), contentType="application/json")

    def evalFreeTextSearch(self, pattern, infer=False, callback=None, limit=None):
        """Use free-text indices to search for the given pattern.
        Returns an array of statements."""
        return jsonRequest(self.curl, "GET", self.url + "/freetext", urlenc(pattern=pattern, infer=infer, limit=limit),
                           rowreader=callback and RowReader(callback))

    def listFreeTextPredicates(self):
        """List the predicates that are used for free-text indexing."""
        return jsonRequest(self.curl, "GET", self.url + "/freetextPredicates")

    def registerFreeTextPredicate(self, predicate):
        """Add a predicate for free-text indexing."""
        nullRequest(self.curl, "POST", self.url + "/freetextPredicates", urlenc(predicate=predicate))

    def clearNamespaces(self):
        nullRequest(self.curl, "DELETE", self.url + "/namespaces")

    def addNamespace(self, prefix, uri):
        nullRequest(self.curl, "PUT", self.url + "/namespaces/" + urllib.quote(prefix),
                    uri, contentType="text/plain")

    def deleteNamespace(self, prefix):
        nullRequest(self.curl, "DELETE", self.url + "/namespaces/" + urllib.quote(prefix))

    def listMappedTypes(self):
        return jsonRequest(self.curl, "GET", self.url + "/typeMapping")

    def addMappedType(self, type, primitiveType):
        nullRequest(self.curl, "POST", self.url + "/typeMapping", urlenc(type=type, primitiveType=primitiveType))

    def deleteMappedType(self, type):
        nullRequest(self.curl, "DELETE", self.url + "/typeMapping", urlenc(type=type))

    def listMappedPredicates(self):
        return jsonRequest(self.curl, "GET", self.url + "/predicateMapping")

    def addMappedPredicate(self, predicate, primitiveType):
        nullRequest(self.curl, "POST", self.url + "/predicateMapping",
                    urlenc(predicate=predicate, primitiveType=primitiveType))

    def deleteMappedPredicate(self, predicate):
        nullRequest(self.curl, "DELETE", self.url + "/predicateMapping", urlenc(predicate=predicate))

    def getCartesianGeoType(self, stripWidth, xMin, xMax, yMin, yMax):
        """Retrieve a cartesian geo-spatial literal type."""
        return jsonRequest(self.curl, "PUT", self.url + "/geo/types/cartesian?" +
                           urlenc(stripWidth=stripWidth, xmin=xMin, ymin=yMin, xmax=xMax, ymax=yMax))


    def getSphericalGeoType(self, stripWidth, unit="degree", latMin=None, latMax=None, longMin=None, longMax=None):
        """Retrieve a spherical geo-spatial literal type."""
        return jsonRequest(self.curl, "PUT", self.url + "/geo/types/spherical?" +
                           urlenc(stripWidth=stripWidth, unit=unit, latmin=latMin, latmax=latMax,
                                  longmin=longMin, longmax=longMax))

    def listGeoTypes(self):
        """List the geo-spatial types registered in the store."""
        return jsonRequest(self.curl, "GET", self.url + "/geo/types")

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
        return jsonRequest(self.curl, "GET", self.url + "/geo/haversine",
                           urlenc(type=type, predicate=predicate, lat=lat, long=long, radius=radius, unit=unit, limit=limit))

    def getStatementsInsideBox(self, type, predicate, xMin, xMax, yMin, yMax, limit=None):
        """Get all the triples with a given predicate whose object
        lies within the specified box."""
        return jsonRequest(self.curl, "GET", self.url + "/geo/box",
                           urlenc(type=type, predicate=predicate, xmin=xMin, xmax=xMax, ymin=yMin, ymax=yMax, limit=limit))

    def getStatementsInsideCircle(self, type, predicate, x, y, radius, limit=None):
        """Get all the triples with a given predicate whose object
        lies within the specified circle."""
        return jsonRequest(self.curl, "GET", self.url + "/geo/circle",
                           urlenc(type=type, predicate=predicate, x=x, y=y, radius=radius, limit=limit))

    def getStatementsInsidePolygon(self, type, predicate, polygon, limit=None):
        """Get all the triples with a given predicate whose object
        lies within the specified polygon (see createPolygon)."""
        return jsonRequest(self.curl, "GET", self.url + "/geo/polygon",
                           urlenc(type=type, predicate=predicate, polygon=polygon, limit=limit))

    def createPolygon(self, resource, points):
        """Create a polygon with the given name in the store. points
        should be a list of literals created with createCartesianGeoLiteral."""
        nullRequest(self.curl, "PUT", self.url + "/geo/polygon?" +
                    urlenc(resource=resource, point=points))
