
from franz.openrdf.repository.repository import Repository
from franz.openrdf.sail.sail import SailRepository
from franz.openrdf.sail.allegrographstore import AllegroGraphStore
from franz.openrdf.query.query import QueryLanguage
from franz.openrdf.vocabulary.rdf import RDF
from franz.openrdf.rio.rdfformat import RDFFormat
from franz.openrdf.rio.rdfwriter import RDFXMLWriter, NTriplesWriter


def test1():
    sesameDir = "/Users/bmacgregor/Desktop/SesameFolder"
    store = AllegroGraphStore(AllegroGraphStore.RENEW, "localhost", "testP",
                              sesameDir, port=4567)
    myRepository = SailRepository(store)
    myRepository.initialize()
    ## TEMPORARY:
    #store.internal_ag_store.serverTrace(True)
    ## END TEMPORARY
    print "Repository is up!"
    return myRepository
    
def test2():
    myRepository = test1()
    f = myRepository.getValueFactory()
    ## create some resources and literals to make statements out of
    alice = f.createURI("http://example.org/people/alice")
    bob = f.createURI("http://example.org/people/bob")
    name = f.createURI("http://example.org/ontology/name")
    person = f.createURI("http://example.org/ontology/Person")
    bobsName = f.createLiteral("Bob")
    alicesName = f.createLiteral("Alice")

    conn = myRepository.getConnection()
    ## alice is a person
    conn.add(alice, RDF.TYPE, person)
    ## alice's name is "Alice"
    conn.add(alice, name, alicesName)
    ## bob is a person
    conn.add(bob, RDF.TYPE, person)
    ## bob's name is "Bob":
    conn.add(bob, name, bobsName)
    return myRepository

def test3():    
    conn = test2().getConnection()
    try:
        queryString = "SELECT ?s ?p ?o WHERE {?s ?p ?o .}"
        tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
        result = tupleQuery.evaluate();
        try:
            for bindingSet in result:
                s = bindingSet.getValue("s")
                p = bindingSet.getValue("p")
                o = bindingSet.getValue("o")              
                print "%s %s %s" % (s, p, o)
        finally:
            result.close();
    finally:
        conn.close();
        
def test4():
    myRepository = test2()
    conn = myRepository.getConnection()
    alice = myRepository.getValueFactory().createURI("http://example.org/people/alice")
    statements = conn.getStatements(alice, None, None, True, [])
    for s in statements:
        print s
    resultSet = conn.getJDBCStatements(None, None, None, True, [])
    while resultSet.next():
        print resultSet.getRow()
        print "   ", resultSet.getValue(3), "   ", resultSet.getString(3)  
               
import dircache, os

def test5():
    print "ABSPATH ", os.path.abspath(".")
    for f in dircache.listdir("."):
        print f
        
    myRepository = test1()        
    file = open("/Users/bmacgregor/Documents/eclipse-franz-python/agpython/src/test/vc-db-1.rdf")        
    baseURI = "http://example.org/example/local"
    baseURI = None
    try:
        conn = myRepository.getConnection();
        conn.add(file, base=baseURI, format=RDFFormat.RDFXML); 
        print "After loading, repository contains %s triples." % conn.size(None)
        try:
            for s in conn.getStatements(None, None, None, True, []):
                print s
             
            print "\n\nAnd here it is JDBC-style"
            resultSet = conn.getJDBCStatements(None, None, None, True, [])
            while resultSet.next():
                print resultSet.getRow()
                
            print "\n\nAnd here it is without the objects"
            resultSet = conn.getJDBCStatements(None, None, None, True, [])
            while resultSet.next():
                print resultSet.getString(1), resultSet.getString(2), resultSet.getString(3)

        finally:
            pass
    finally:
        conn.close()

def test6():
    myRepository = test1() 
    file = open("/Users/bmacgregor/Documents/eclipse-franz-python/agpython/src/test/vc-db-1.rdf")        
    baseURI = "http://example.org/example/local"
    try:
        conn = myRepository.getConnection();
        conn.add(file, base=baseURI, format=RDFFormat.RDFXML);
        print "After loading, repository contains %s triples." % conn.size(None)         
        queryString = "SELECT ?s ?p ?o WHERE {?s ?p ?o .}"
        tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
        result = tupleQuery.evaluate();
        try:
            for bindingSet in result:
                s = bindingSet.getValue("s")
                p = bindingSet.getValue("p")
                o = bindingSet.getValue("o")              
                print "%s %s %s" % (s, p, o)
        finally:
            result.close();
    finally:
        conn.close();
        
def test7():
    myRepository = test1() 
    file = open("/Users/bmacgregor/Documents/eclipse-franz-python/agpython/src/test/vc-db-1.rdf")        
    baseURI = "http://example.org/example/local"
    try:
        conn = myRepository.getConnection();
        conn.add(file, base=baseURI, format=RDFFormat.RDFXML);
        print "After loading, repository contains %s triples." % conn.size(None)         
        queryString = "CONSTRUCT { ?s ?p ?o } WHERE {?s ?p ?o .}"
        graphQuery = conn.prepareGraphQuery(QueryLanguage.SPARQL, queryString)
        result = graphQuery.evaluate();
        for s in result:          
            print s
    finally:
        conn.close();

import urlparse

def test8():
    location = "/Users/bmacgregor/Documents/eclipse-franz-python/agpython/src/test/vc_db_1_rdf"      
    url = "/Users/bmacgregor/Documents/eclipse-franz-python/agpython/src/test/vc-db-1.rdf"      
    url = "/Users/bmacgregor/Documents/eclipse-franz-python/agpython/src/test/sample-bad.rdf"
    baseURI = location
    myRepository = test1() 
    context = myRepository.getValueFactory().createURI(location)
    ## TEMPORARY:  
    print "NULLIFYING CONTEXT TEMPORARILY"
    context = None
    ## END TEMPORARY  
    conn = myRepository.getConnection();
    ## read the contents of a file into the context:
    conn.add(url, baseURI, format=RDFFormat.RDFXML, contexts=context);
    print "RDF store contains %s triples" % conn.size(None)
    ## Get all statements in the context
    statements = conn.getStatements(None, None, None, False, context)    
    try:
        for s in statements:
            print s
    finally:
        statements.close()
    ## Export all statements in the context to System.out, in NTriples format
    ntriplesWriter = NTriplesWriter(None)
    #ntriplesWriter = NTriplesWriter("/users/bmacgregor/Desktop/temp.nt")
    conn.export(ntriplesWriter, context);    
    ## Remove all statements in the context from the repository
    conn.clear(context)
    ## Verify that the statements have been removed:
    statements = conn.getStatements(None, None, None, False, context)    
    try:
        for s in statements:
            print s
    finally:
        statements.close()
   
def test9():
    sesameDir = "/Users/bmacgregor/Desktop/SesameFolder"
    store = AllegroGraphStore(AllegroGraphStore.OPEN, "localhost", "testP",
                              sesameDir, port=4567)
    myRepository = SailRepository(store)
    myRepository.initialize()
    ## TEMPORARY:
    #store.internal_ag_store.serverTrace(True)
    ## END TEMPORARY
    print "Repository is up!"
    conn = myRepository.getConnection();
    statements = conn.getStatements(None, None, None, False, None)    
    try:
        for s in statements:
            print s
    finally:
        statements.close()
    return myRepository

def test10():
    print "X ", hasattr([], '__iter__')
    
if __name__ == '__main__':
    choice = 9
    if choice == 1: test1()
    elif choice == 2: test2()
    elif choice == 3: test3()
    elif choice == 4: test4()    
    elif choice == 5: test5()        
    elif choice == 6: test6()            
    elif choice == 7: test7()                
    elif choice == 8: test8()                
    elif choice == 9: test9()                        
    elif choice == 10: test10()                            
    