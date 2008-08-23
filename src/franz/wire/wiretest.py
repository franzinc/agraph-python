


from franz.openrdf.sail.sail import SailRepository
from franz.openrdf.repository.repository import Repository
from franz.openrdf.sail.allegrographstore import AllegroGraphStore
from franz.openrdf.query.query import QueryLanguage
from franz.openrdf.vocabulary.rdf import RDF
from franz.openrdf.vocabulary.xmlschema import XMLSchema
from franz.openrdf.query.dataset import Dataset
from franz.openrdf.rio.rdfformat import RDFFormat
from franz.openrdf.rio.rdfwriter import  NTriplesWriter
from franz.openrdf.rio.rdfxmlwriter import RDFXMLWriter

import urllib, urllib2, datetime

def data_to_get_query(data):
    query = '&'.join(['%s=%s' % (t[0], urllib2.quote(t[1])) for t in data])
    #print "QUERY", query
    return query
                          
def test0():
    data = [('query',"SELECT ?s ?o { ?s a ?o . } LIMIT 5"), ('id', "testP")]
    print "DTOQ", data_to_get_query(data)
    
 
def test1():
    data = [('query',"SELECT ?s ?o { ?s a ?o . } LIMIT 5"), ('id', "testP")]
    if (True):
        request = urllib2.Request("http://localhost:7654/sparql?" + data_to_get_query(data))
        request.add_header('accept', 'application/rdf+xml')
        request.add_header('accept', 'application/x-sparql-compact')
        try:
            response = urllib2.urlopen(request)
            print response.read(), "\n"
        except Exception, e:
            print "HTTP Request failed.", e 
    else:
        request = urllib2.Request("http://localhost:7654/sparql")
        request.add_header('accept', 'application/rdf+xml')
        request.add_header('accept', 'application/x-sparql-compact')
        data = urllib.urlencode(data, 1)
        try:
            response = urllib2.urlopen(request, data)
            print response.read(), "\n"
        except Exception, e:
            print "HTTP Request failed.", e 
        
def test2():
    sesameDir = "/Users/bmacgregor/Desktop/SesameFolder"
    store = AllegroGraphStore(AllegroGraphStore.ACCESS, "localhost", "testP",
                              sesameDir, port=4567)
    myRepository = Repository(store)
    myRepository.initialize()
    conn = Repository(myRepository).getConnection()
    try:
        queryString = """
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        SELECT ?s  ?o  WHERE {?s rdf:first ?o .} LIMIT 5
        """        
        tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
        result = tupleQuery.evaluate();
        try:
            for bindingSet in result:
                s = bindingSet.getValue("s")
                p = None # bindingSet.getValue("p")
                o = bindingSet.getValue(1)              
                print s, "   ", p, "   ", o
        finally:
            result.close();
        result = tupleQuery.evaluate(jdbc=True);
        while result.next():
            print result.getRow()

    finally:
        conn.close();
    print "Done with test2"

def test3():
    sesameDir = "/Users/bmacgregor/Desktop/SesameFolder"
    store = AllegroGraphStore(AllegroGraphStore.ACCESS, "localhost", "testP",
                              sesameDir, port=4567)
    myRepository = Repository(store)
    myRepository.initialize()
    conn = Repository(myRepository).getConnection()
    CYCLES = 100
    queryString = """
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        SELECT ?s  ?o  WHERE {?s rdf:first ?o .} LIMIT 5
        """ 
    beginTime = datetime.datetime.now()
    for i in range(0, CYCLES):
        tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
        result = tupleQuery.evaluate();
        count = 0
        for bindingSet in result:
            s = bindingSet.getValue(0)
            o = bindingSet.getValue(1)
            count += 1
    elapsedTime = datetime.datetime.now() - beginTime
    print "Finished %s queries, each returning %i tuples, in time %s" % (CYCLES, count, elapsedTime)

#        result = tupleQuery.evaluate(jdbc=True);
#        while result.next():
#            print result.getRow()

from franz.wire.doquery import do_compact_query

def test4():
    CYCLES = 10
    queryString = """
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        SELECT ?s  ?o  WHERE {?s rdf:first ?o .} LIMIT 5
        """ 
    beginTime = datetime.datetime.now()
    for i in range(0, CYCLES):
        reader = do_compact_query(queryString)
    elapsedTime = datetime.datetime.now() - beginTime
    print "Finished %s queries with limit %i in time %s" % (CYCLES, 5000, elapsedTime)
    
if __name__ == '__main__':
    choices = [i for i in range(1,17)]
    choices = [4]
    for choice in choices:
        print "\n==========================================================================="
        print "Test Run Number ", choice, "\n"
        if choice == 1: test1()
        elif choice == 2: test2()
        elif choice == 3: test3()        
        elif choice == 4: test4()                
        else:
            print "No such test: ", choice
