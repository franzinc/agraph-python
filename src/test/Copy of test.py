
from franz.openrdf.repository.repository import Repository
from franz.openrdf.sail.sail import SailRepository
from franz.openrdf.sail.allegrographstore import AllegroGraphStore
from franz.openrdf.query.query import QueryLanguage
from franz.openrdf.vocabulary.rdf import RDF
from franz.openrdf.rio.rdfformat import RDFFormat


def test1():
    sesameDir = "/Users/bmacgregor/Desktop/SesameFolder"
    store = AllegroGraphStore(AllegroGraphStore.RENEW, "localhost", "test",
                              sesameDir, port=4567)
    myRepository = SailRepository(store)
    myRepository.initialize()
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
    statements = conn.getStatements(None, None, None, True, [])
    for s in statements:
        print s
    
        

                
def test5():
    myRepository = test1()
    file = open("/Users/bmacgregor/Documents/eclipse-franz-python/agpython/src/text/vc-db-1.rdf")
    baseURI = "http://example.org/example/local"
    try:
        conn = myRepository.getConnection();
        try:
            conn.add(file, baseURI, RDFFormat.RDFXML);
            
        finally:
            pass
    finally:
        conn.close()


if __name__ == '__main__':
    choice = 4
    if choice == 1: test1()
    elif choice == 2: test2()
    elif choice == 3: test3()
    elif choice == 4: test4()    
    elif choice == 5: test5()        
    