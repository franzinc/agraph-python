# TODO streaming cursors for queries

import time, cjson
from request import *

class AllegroGraphServer:
    def __init__(self, url, user=None, password=None):
        self.url = url
        self.curl = pycurl.Curl()
        if user and password: self.setAuth(user, password)

    def listTripleStores(self):
        """Returns the names of open stores on the server."""
        return jsonRequest(self.curl, "GET", self.url + "/repositories")

    def openTripleStore(self, name, fileName, readOnly=False):
        """Ask the server to open a given triple store."""
        nullRequest(self.curl, "POST", self.url + "/repository/open",
                    urlenc(name=name, file=fileName, readOnly=readOnly))

    def createTripleStore(self, name, fileName):
        """Ask the server to create a new triple store."""
        nullRequest(self.curl, "POST", self.url + "/repository/create",
                    urlenc(name=name, file=fileName))

    def closeTripleStore(self, name):
        """Close a server-side triple store."""
        nullRequest(self.curl, "DELETE", self.url + "/repositories/" + urllib.quote(name))

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
        self.environment = None

    def getSize(self):
        """Returns the amount of triples in the repository."""
        return jsonRequest(self.curl, "GET", self.url + "/size")

    def listContexts(self):
        """Lists the contexts (named graphs) that are present in this repository."""
        return jsonRequest(self.curl, "GET", self.url + "/contexts")

    def isWriteable(self):
        return jsonRequest(self.curl, "GET", self.url + "/writeable")

    def evalSparqlQuery(self, query, infer=False, context=None, callback=None):
        """Execute a SPARQL query. Context can be None or a list of
        contexts -- strings in "http://foo.com" form or "null" for the
        default context. Return type depends on the query type. ASK
        gives a boolean, SELECT a {names, values} object containing
        lists of lists of terms. CONSTRUCT and DESCRIBE return a list
        of lists representing statements. Callback WILL NOT work on
        ASK queries."""
        return jsonRequest(self.curl, "POST", self.url,
                           urlenc(query=query, infer=infer, context=context, environment=self.environment),
                           rowreader=callback and RowReader(callback))

    def evalPrologQuery(self, query, infer=False, callback=None):
        """Execute a Prolog query. Returns a {names, values} object."""
        return jsonRequest(self.curl, "POST", self.url,
                           urlenc(query=query, infer=infer, queryLn="prolog", environment=self.environment),
                           rowreader=callback and RowReader(callback))

    def definePrologFunctor(self, definition):
        nullRequest(self.curl, "POST", self.url + "/functor", urlenc(definition=definition, environment=self.environment))

    def getStatements(self, subj=None, pred=None, obj=None, context=None, infer=False, callback=None):
        """Retrieve all statements matching the given constraints.
        Context can be None or a list of contexts, as in
        evalSparqlQuery."""
        return jsonRequest(self.curl, "GET", self.url + "/statements",
                           urlenc(subj=subj, pred=pred, obj=obj, context=context, infer=infer),
                           rowreader=callback and RowReader(callback))

    def addStatement(self, subj, pred, obj, context=None):
        """Add a single statement to the repository."""
        nullRequest(self.curl, "POST", self.url + "/statements",
                    urlenc(subj=subj, pred=pred, obj=obj, context=context))

    def deleteMatchingStatements(self, subj=None, pred=None, obj=None, context=None):
        """Delete all statements matching the constraints from the
        repository. Context can be None or a single graph name."""
        nullRequest(self.curl, "DELETE", self.url + "/statements",
                    urlenc(subj=subj, pred=pred, obj=obj, context=context))

    def addStatements(self, quads):
        """Add a collection of statements to the repository. Quads
        should be an array of four-element arrays, where the fourth
        element, the graph name, may be None."""
        nullRequest(self.curl, "POST", self.url + "/statements/json", cjson.encode(quads), contentType="application/json")

    def loadFile(self, file, format, baseURI=None, context=None, serverSide=False):
        urlformat = None
        mime = None
        if format == "ntriples":
            urlformat = "ntriples"
            mime = "text/plain"
        else if format == "rdf/xml":
            urlformat = "rdfxml"
            mime = "application/rdf+xml"
        else:
            raise Error("Only 'ntriples' and 'rdf/xml' are supported as formats by loadFile.")

        body = ""
        if not serverSide:
            f = open(file)
            body = f.read()
            f.close()
            file = None
        nullRequest(self.curl, "POST", self.url + "/statements/" + urlformat + "?" +
                    urlenc(file=file, context=context, baseURI=baseURI), body, contentType=mime)

    def deleteStatements(self, quads):
        """Delete a collection of statements from the repository."""
        nullRequest(self.curl, "POST", self.url + "/statements/json/delete", cjson.encode(quads), contentType="application/json")

    def listIndices(self):
        """List the SPOGI-indices that are active in the repository."""
        return jsonRequest(self.curl, "GET", self.url + "/indices")

    def addIndex(self, type):
        """Register a SPOGI index."""
        nullRequest(self.curl, "POST", self.url + "/indices", urlenc(type=type))

    def deleteIndex(self, type):
        """Drop a SPOGI index."""
        nullRequest(self.curl, "DELETE", self.url + "/indices", urlenc(type=type))

    def getIndexCoverage(self):
        """Returns the proportion (0-1) of the repository that is indexed."""
        return jsonRequest(self.curl, "GET", self.url + "/index")

    def indexStatements(self, all=False):
        """Index any unindexed statements in the repository. If all is
        True, the whole repository is re-indexed."""
        nullRequest(self.curl, "POST", self.url + "/index", urlenc(all=all))

    def evalFreeTextSearch(self, pattern, infer=False, callback=None):
        """Use free-text indices to search for the given pattern.
        Returns an array of statements."""
        return jsonRequest(self.curl, "GET", self.url + "/freetext", urlenc(pattern=pattern, infer=infer),
                           rowreader=callback and RowReader(callback))

    def listFreeTextPredicates(self):
        """List the predicates that are used for free-text indexing."""
        return jsonRequest(self.curl, "GET", self.url + "/freetextpredicates")

    def registerFreeTextPredicate(self, predicate):
        """Add a predicate for free-text indexing."""
        nullRequest(self.curl, "POST", self.url + "/freetextpredicates", urlenc(predicate=predicate))

    def setEnvironment(self, name):
        """Repositories use a current environment, which are
        containers for namespaces and Prolog predicates. Every
        server-side repository has a default environment that is used
        when no environment is specified."""
        self.environment = name

    def listEnvironments(self):
        return jsonRequest(self.curl, "GET", self.url + "/environments")

    def createEnvironment(self, name):
        nullRequest(self.curl, "POST", self.url + "/environments", urlenc(name=name))

    def deleteEnvironment(self, name):
        nullRequest(self.curl, "DELETE", self.url + "/environments", urlenc(name=name))
        
    def listNamespaces(self):
        return jsonRequest(self.curl, "GET", self.url + "/namespaces", urlenc(environment=self.environment))

    def addNamespace(self, prefix, uri):
        nullRequest(self.curl, "POST", self.url + "/namespaces",
                    urlenc(prefix=prefix, uri=uri, environment=self.environment))

    def deleteNamespace(self, prefix):
        nullRequest(self.curl, "DELETE", self.url + "/namespaces",
                    urlenc(prefix=prefix, environment=self.environment))


######################################################
## TESTING CODE
######################################################

def timeQuery(rep):
    n = 100
    size = 5
    t = time.time()
    for i in range(n):
        rep.evaluateQuery("select ?x ?y ?z {?x ?y ?z} limit %d" % size)
    print "Did %d %d-row queries in %f seconds." % (n, size, time.time() - t)

    
def test1():
    conn = AllegroGraphServer("http://localhost:8080")
    storeNames = conn.getTripleStores()
    print "Stores", storeNames
    if len(storeNames) > 0:
        print "Found repositories " + repr(storeNames) + ", opening " + storeNames[0]
        rep = conn.getRepository(storeNames[0])
        print "Repository size = %d" % rep.size()
        timeQuery(rep)

def test2():
    conn = AllegroGraphServer("http://localhost:8080")
    dbName = 'testP'
    if not dbName in conn.listTripleStores():
        try:
            conn.openTripleStore(dbName, "/Users/bmacgregor/Desktop/AGFolder/")
        except:
            conn.createTripleStore(dbName, "/Users/bmacgregor/Desktop/AGFolder/")            
    rep = conn.getRepository(dbName)        
    rep.addStatement('<http://www.franz.com/example#ted>', '<http://www.franz.com/example#age>', '"55"^^<http://www.w3.org/2001/XMLSchema#int>', None)
    query = """select ?x ?y ?z {?x ?y ?z} limit 5"""
    answer = rep.evalSparqlQuery(query)
    print answer['names']
    for v in answer['values']:
        print v

def test3():
    conn = AllegroGraphServer("http://localhost:8080")
    rep = conn.getRepository("foo")
    def printrow(row, names): print "%s %s" %(repr(row), repr(names))
    print rep.evalSparqlQuery("select ?x ?y ?z {?x ?y ?z} limit 5", callback=printrow)

if __name__ == '__main__':
    choice = 3
    print "Run test%i" % choice
    if choice == 1: test1()   
    elif choice == 2: test2()       
    elif choice == 3: test3()       
    elif choice == 4: test4()               
   
