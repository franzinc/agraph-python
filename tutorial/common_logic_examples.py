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

import os, urllib, time

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

def doQuery(conn, query, prefer='SPARQL', input_language=QueryLanguage.COMMON_LOGIC, bindings=None):
    tupleQuery = conn.prepareTupleQuery(input_language, query)
    tupleQuery.preferred_execution_language = prefer
    if bindings:
        tupleQuery.setBindings(bindings)
    result = tupleQuery.evaluate(); 
    print "Query: " + query   
    print "Executing using " + (tupleQuery.actual_execution_language or '')
    print "Found %i query results" % result.tupleCount    
    count = 0
    for bindingSet in result:
        print bindingSet
        count += 1
        if count > 5: break
    return tupleQuery.actual_execution_language or ''

def loadBobCarolTedAlice(repositoryConnection):
    c = repositoryConnection
    c.clear()
    foafns = "http://xmlns.com/foaf/0.1/"
    c.setNamespace('foaf', foafns)
    exns = "http://example.org/people#"
    c.setNamespace('ex', exns)
    context1 = c.createURI(namespace=exns, localname="cxt1")      
    context2 = c.createURI(namespace=exns, localname="cxt2")          
    alice = c.createURI(exns, "alice")
    bob = c.createURI(exns, "bob")
    carol = c.createURI(exns, "carol")    
    ted = c.createURI(exns, "ted")    
    person = c.createURI(exns, "Person")
    name = c.createURI(exns, "name")      
    age = c.createURI(exns, "age") 
    mbox = c.createURI(foafns, "mbox")
    c.addTriples([
        (alice, RDF.TYPE, person, context1),
        (alice, name, "Alice", context1),
        (alice, age, 42, context1),
        (alice, mbox, "alice@gmail.com"),
        (carol, RDF.TYPE, person, context1),
        (carol, name, "Carol", context1),
        (carol, age, "39"),
        (bob, RDF.TYPE, person, context2),
        (bob, name, "Bob", context2),
        (bob, age, 24),
        (ted, RDF.TYPE, person),
        (ted, name, "Ted"),])

        
def doDualQuery(conn, query):
    print
    lang = doQuery(conn, query, prefer='PROLOG')
    if not lang == 'SPARQL':
        doQuery(conn, query, prefer='SPARQL')


def test201(accessMode=Repository.RENEW, verbose=True):
    """
    Tests getting the repository up.  Is called by the other tests to do the startup.
    """
    server = AllegroGraphServer("localhost", port=8080)
    if verbose: print "Available catalogs", server.listCatalogs()
    catalog = server.openCatalog('scratch')  
    if verbose: print "Available repositories in catalog '%s':  %s" % (catalog.getName(), catalog.listRepositories())    
    myRepository = catalog.getRepository("agraph_test", accessMode)
    myRepository.initialize()
    if verbose: print "Repository %s is up!  It contains %i statements." % (
                myRepository.getDatabaseName(), myRepository.getConnection().size())
    conn = myRepository.getConnection()
    return conn

def test202():
    """
    Basic CommonLogic queries
    """
    conn = test201(verbose=True);
    loadBobCarolTedAlice(conn)
    doDualQuery(conn, """(select (?s ?p ?o) where (triple ?s ?p ?o))""")
    doDualQuery(conn, """(select (?s ?p ?o) where (?p  ?s ?o))""")            
    doDualQuery(conn, """(select (?s ?p ?o) where (triple ?s ?p ?o) limit 2)""")
    doDualQuery(conn, """(select distinct (?s) where (triple ?s ?p ?o))""")    
    doDualQuery(conn, """(select (?s ?age) where (ex:age ?s ?age) (>= ?age 30))""") ## PROLOG BREAKS        
    doDualQuery(conn, """(select (?s ?age) where (?p ?s ?o) (optional (ex:age ?s ?age)))""")            

def test203():
    """ 
    Contexts filter queries
    """
    conn = test201(verbose=False);
    loadBobCarolTedAlice(conn)
    doDualQuery(conn, """(select (?s ?p ?o ?c) where (quad ?s ?p ?o ?c) contexts (ex:cxt1))""")
    doDualQuery(conn, """(select (?s ?p ?o) where (?p ?s ?o) contexts (ex:cxt1))""")
    doDualQuery(conn, """(select (?s ?p ?o) where (?p ?s ?o) contexts (ex:cxt1 ex:cxt2))""")    
    
    #doQuery(conn, """select ?s ?name where { ?s ?p ?name . filter (?name = "Alice") }""", input_language='SPARQL')
    doQuery(conn, """select ?s ?name where { ?s ?p ?name }""", input_language='SPARQL', bindings={'name': "Alice"})
      

def test204():
    conn = test201(verbose=True);
    foafns = "http://xmlns.com/foaf/0.1/"
    conn.setNamespace('foaf', foafns)
    alice = conn.createURI("http://foo#alice")
    query2 = """select ?s ?o
                where { {?s foaf:name ?o}
                      union { optional {?s foaf:name ?o1} .
                      filter (!bound(?o1)) .
                      { {?s foaf:mbox ?o}
                      union { optional {?s foaf:mbox ?o2} .
                      filter (!bound(?o2)) } } } }"""
    query = query2
    print "ZERO TRIPLES"
    doQuery(conn, query, input_language='SPARQL')
    conn.addTriples([(alice, conn.createURI(foafns, "mbox"), "alice@gmail.com")])
    print "BEFORE: one foaf triple: foaf:mbox"
    doQuery(conn, query, input_language='SPARQL')
    print "AFTER: two foaf triples: foaf:mbox and foaf:name "
    conn.addTriples([(alice, conn.createURI(foafns, "name"), "Alice")])
    doQuery(conn, query, input_language='SPARQL')    
                 
        
def test205():
    """
    Prolog declarative optional
    """
    conn = test201(verbose=True);
    foafns = "http://xmlns.com/foaf/0.1/"
    conn.setNamespace('foaf', foafns)
    alice = conn.createURI("http://foo#alice")
    query = """(select (?s ?o)
                 where (or (triple ?s foaf:name ?o)
                           (and (not (triple ?s foaf:name ?o1))
                                (or (triple ?s foaf:mbox ?o)
                                    (not (triple ?s foaf:mbox ?o2))))))"""
    query = """(select (?s ?o) where
            (and (optional (triple ?s foaf:name ?o))
                 (optional (triple ?s foaf:mbox ?o))))"""                

    print "ZERO TRIPLES"
    doQuery(conn, query, prefer='SPARQL')
    conn.addTriples([(alice, conn.createURI(foafns, "mbox"), "alice@gmail.com")])
    print "MBOX TRIPLE"
    doQuery(conn, query, prefer='SPARQL')
    print "NAME AND MBOX TRIPLEs"
    conn.addTriples([(alice, conn.createURI(foafns, "name"), "Alice")])
    doQuery(conn, query, prefer='SPARQL')                     

from datetime import datetime

def test206():
    """

    """
    conn = test201(verbose=True);
    foafns = "http://xmlns.com/foaf/0.1/"
    conn.setNamespace('foaf', foafns)
    conn.setNamespace('ex', "http://foo#")
    alice = conn.createURI("http://foo#alice")
    bob = conn.createURI("http://foo#bob")    
    conn.addTriples([(alice, conn.createURI(foafns, "mbox"), "alice@gmail.com"),
                     (alice, conn.createURI(foafns, "name"), "Alice"),
                     (bob, conn.createURI(foafns, "name"), "Bob"),
                     ])

    query = """(select (?access)
                 where (and (or (triple ?a ?access "alice@gmail.com")
                                (triple ?b ?access "Bob"))
                            (or (= ?access foaf:name) (= ?access foaf:mbox))
                            ))                                                   
    """  
    query1 = """(select (?z) where (and (triple ?x ex:foo ?c)
                                      (triple ?x ex:bar ?y)
                                      (or (= ?z ?x) (= ?z ?y))))
    """
    query = """(select (?z) where (or (triple ?z ex:foo ?c)
                               (and (triple ?x ex:foo ?c)
                                    (triple ?x ex:bar ?z))))
    """
    doQuery(conn, query, prefer='SPARQL')

def test207():
    conn = test201(verbose=True);
    loadBobCarolTedAlice(conn)
    query = """(select (?s ?o) where 
                  (and (or (triple ?s foaf:mbox ?o)
                           (not (triple ?s foaf:mbox ?o)))
                       (or (triple ?s ex:name ?o)
                           (not (triple ?s ex:name ?o)))))"""
    query = """(select (?s ?o) where
                (and (triple ?s ex:age ?age)
                     (optional (triple ?s foaf:mbox ?o))
                     (optional (triple ?s ex:name ?o))
                     ))"""                
    query2 = """(select (?s ?o) where
                (and (optional (triple ?s ex:name ?o))
                     (optional (triple ?s foaf:mbox ?o))))"""                
    doQuery(conn, query, prefer='SPARQL')
    
   
if __name__ == '__main__':
    choices = [i for i in range(1,3)]
    choices = [7]
    for choice in choices:
        print "\n==========================================================================="
        print "Test Run Number 20%i\n" % choice
        if choice == 0: test0()
        elif choice == 1: test201()
        elif choice == 2: test202()
        elif choice == 3: test203()
        elif choice == 4: test204()    
        elif choice == 5: test205()        
        elif choice == 6: test206()            
        elif choice == 7: test207()                
        elif choice == 8: test8()                
        elif choice == 9: test9()                        
        elif choice == 10: test10()                            
        elif choice == 11: test11()
        elif choice == 12: test12()                                                                                   
        elif choice == 13: test13()  
        elif choice == 14: test14()                                                                                         
        elif choice == 15: test15()    
        elif choice == 16: test16()            
        elif choice == 17: test17()                    
        elif choice == 18: test18()                            
         
        elif choice == 26: test26()                                                                                              
        elif choice == 27: test27()                                                                                                      
        else:
            print "No such test exists."
    
