# TODO streaming cursors for queries

import time, cjson
from request import *

class AllegroGraphServer:
    def __init__(self, url):
        self.url = url
        self.curl = pycurl.Curl()

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
        nullRequest(self.curl, "POST", self.url + "/repository/close", urlenc(name=name))

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

    def evalSparqlQuery(self, query, infer=False, context=None):
        """Execute a SPARQL query. Context can be None or a list of
        contexts -- strings in "http://foo.com" form or "null" for
        the default context. Return type depends on the query type.
        ASK gives a boolean, SELECT a {names, values} object
        containing arrays of arrays of terms. CONSTRUCT and DESCRIBE
        return an array of arrays representing statements."""
        return jsonRequest(self.curl, "POST", self.url,
                           urlenc(query=query, infer=infer, context=context, environment=self.environment))

    def evalPrologQuery(self, query, infer=False):
        """Execute a Prolog query. Returns a {names, values} object."""
        return jsonRequest(self.curl, "POST", self.url,
                           urlenc(query=query, infer=infer, queryLn="prolog", environment=self.environment))

    def definePrologFunctor(self, definition):
        nullRequest(self.curl, "POST", self.url + "/functor", urlenc(definition=definition, environment=self.environment))

    def getStatements(self, subj=None, pred=None, obj=None, context=None, infer=False):
        """Retrieve all statements matching the given contstaints.
        Context can be None or a list of contexts, as in
        evalSparqlQuery."""
        return jsonRequest(self.curl, "GET", self.url + "/statements",
                           urlenc(subj=subj, pred=pred, obj=obj, context=context, infer=infer))

    def addStatement(self, subj, pred, obj, context=None):
        """Add a single statement to the repository."""
        nullRequest(self.curl, "POST", self.url + "/statements",
                    urlenc(subj=subj, pred=pred, obj=obj, context=context))

    def deleteStatements(self, subj=None, pred=None, obj=None, context=None):
        """Delete all statements matching the constraints from the
        repository. Context can be None or a single graph name."""
        nullRequest(self.curl, "DELETE", self.url + "/statements",
                    urlenc(subj=subj, pred=pred, obj=obj, context=context))

    def addStatements(self, quads):
        """Add a collection of statements to the repository. Quads
        should be an array of four-element arrays, where the fourth
        element, the graph name, may be None."""
        nullRequest(self.curl, "POST", self.url + "/statements/json", cjson.encode(quads))

    def deleteStatements(self, quads):
        """Delete a collection of statements from the repository."""
        nullRequest(self.curl, "POST", self.url + "/statements/json/delete", cjson.encode(quads))

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

    def evalFreeTextSearch(self, pattern, infer=False):
        """Use free-text indices to search for the given pattern.
        Returns an array of statements."""
        return jsonRequest(self.curl, "GET", self.url + "/freetext", urlenc(pattern=pattern, infer=infer))

    def listFreeTextPredicates(self):
        """List the predicates that are used for free-text indexing."""
        return jsonRequest(self.curl, "GET", self.url + "/freetextindices")

    def registerFreeTextPredicate(self, predicate):
        """Add a predicate for free-text indexing."""
        nullRequest(self.curl, "POST", self.url + "/freetextindices", urlenc(predicate=predicate))

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


# Testing stuff

def timeQuery(rep, n, size):
    t = time.time()
    for i in range(n):
        rep.evalSparqlQuery("select ?x ?y ?z {?x ?y ?z} limit %d" % size)
    print "Did %d %d-row queries in %f seconds." % (n, size, time.time() - t)
    
def test():
    server = AllegroGraphServer("http://localhost:8080")
    storeNames = server.listTripleStores()
    if len(storeNames) > 0:
        print "Found repositories " + repr(storeNames) + ", opening " + storeNames[0]
        rep = server.getRepository(storeNames[0])
        print "Repository size = %d" % rep.getSize()
        print "Repository writable = %d" % rep.isWriteable()
        timeQuery(rep, 500, 2)
        timeQuery(rep, 50, 500)
    else:
        print "No repositories found."

if __name__ == '__main__':
    test()
