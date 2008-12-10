
from franz.openrdf.sail.allegrographserver import AllegroGraphServer
from franz.openrdf.repository.repository import Repository
from franz.miniclient import repository
from franz.openrdf.query.query import QueryLanguage
from franz.openrdf.vocabulary.rdf import RDF
from franz.openrdf.vocabulary.xmlschema import XMLSchema
from franz.openrdf.query.dataset import Dataset
from franz.openrdf.rio.rdfformat import RDFFormat
from franz.openrdf.rio.rdfwriter import  NTriplesWriter
from franz.openrdf.rio.rdfxmlwriter import RDFXMLWriter

import os, urllib, datetime, time

CURRENT_DIRECTORY = os.getcwd() 

RAISE_EXCEPTION_ON_VERIFY_FAILURE = False

def verify(expressionValue, targetValue, quotedExpression, testNum):
    """
    Verify that 'expressionValue' equals 'targetValue'.  If not,
    raise an exception, or print a message advertising the failure.
    """
    if not expressionValue == targetValue:
        message = ("Diagnostic failure in test %s.  Expression '%s' returns '%s' where '%s' expected." %
                    (testNum, quotedExpression, expressionValue, targetValue))
        if RAISE_EXCEPTION_ON_VERIFY_FAILURE:
            raise Exception(message)
        else:
            print "BWEEP BWEEP BWEEP BWEEP BWEEP BWEEP BWEEP BWEEP BWEEP BWEEP BWEEP BWEEP BWEEP BWEEP BWEEP \n   ", message

def test0():
    for i in range(0, 5):
        print "Hello World"
        time.sleep(5)

def test1(accessMode=Repository.RENEW):
    """
    Tests getting the repository up.  Is called by the other tests to do the startup.
    """
    server = AllegroGraphServer("localhost", port=8080)
    print "Available catalogs", server.listCatalogs()
    catalog = server.openCatalog('scratch')  
    print "Available repositories in catalog '%s':  %s" % (catalog.getName(), catalog.listRepositories())    
    myRepository = catalog.getRepository("agraph_test", accessMode)
    myRepository.initialize()
    print "Repository %s is up!  It contains %i statements." % (
                myRepository.getDatabaseName(), myRepository.getConnection().size())
    return myRepository
    
def test2():
    myRepository = test1()
    f = myRepository.getValueFactory()
    ## create some resources and literals to make statements out of
    alice = f.createURI("http://example.org/people/alice")
    bob = f.createURI("http://example.org/people/bob")
    #bob = f.createBNode()
    name = f.createURI("http://example.org/ontology/name")
    person = f.createURI("http://example.org/ontology/Person")
    bobsName = f.createLiteral("Bob")
    alicesName = f.createLiteral("Alice")

    conn = myRepository.getConnection()
    print "Triple count before inserts: ", conn.size()
    for s in conn.getStatements(None, None, None, None): print s    
    ## alice is a person
    conn.add(alice, RDF.TYPE, person)
    ## alice's name is "Alice"
    conn.add(alice, name, alicesName)
    ## bob is a person
    conn.add(bob, RDF.TYPE, person)
    ## bob's name is "Bob":
    conn.add(bob, f.createURI("http://example.org/ontology/name"), bobsName)
    print "Triple count: ", conn.size()
    verify(conn.size(), 4, 'conn.size()', 2)
    conn.remove(bob, name, bobsName)
    print "Triple count: ", conn.size()
    verify(conn.size(), 3, 'conn.size()', 2)
    conn.add(bob, name, bobsName)    
    return myRepository

def testS3():
    """
    Test optional query; specifically, the null that gets created.
    """
    myRepository = test1()
    f = myRepository.getValueFactory()
    ## create some resources and literals to make statements out of
    alice = f.createURI("http://example.org#alice")
    bob = f.createBNode()
    age = f.createURI("http://example.org#age")
    bobsAge = f.createLiteral(42)
    alicesAge = f.createLiteral(50)
    conn = myRepository.getConnection()
    conn.add(alice, age, alicesAge)
    conn.add(bob, age, bobsAge)
    #queryString = """PREFIX ex: <http://example.org#>
    #SELECT ?s ?v1 ?v2 WHERE {?s ?p ?v1 . { OPTIONAL ?s ?p ?v2 . FILTER (?v1 > 42) } }
    #"""
    queryString = "SELECT ?s ?v1 ?v2 WHERE {?s ?p ?v1 . OPTIONAL { ?s ?p ?v2  FILTER (?v1 > 42) } }"
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate();    
    for bs in result:
        print bs[0], bs[1], bs[2]

    
if __name__ == '__main__':
    choices = [i for i in range(1,14)]
    choices = [3]
    for choice in choices:
        print "\n==========================================================================="
        print "Stress Test Run Number ", choice, "\n"
        if choice == 0: test0()
        elif choice == 1: test1()
        elif choice == 2: test2()
        elif choice == 3: testS3()
        elif choice == 4: test4()    
        elif choice == 5: test5()        
        elif choice == 6: test6()            
        elif choice == 7: test7()                
        elif choice == 8: test8()                
        elif choice == 9: test9()                        
        elif choice == 10: test10()                            
        elif choice == 11: test11()
        elif choice == 12: test12()                                                                                   
        elif choice == 13: test13()  
        elif choice == 14: test14()                                                                                         
        elif choice == 15: test15()     
        elif choice == 16: test16()                                                                                              
        else:
            print "No such test exists."
    
