from franz.miniclient import repository
from franz.openrdf.query.dataset import Dataset
from franz.openrdf.query.query import QueryLanguage
from franz.openrdf.repository.repository import Repository
from franz.openrdf.rio.rdfformat import RDFFormat
from franz.openrdf.rio.rdfwriter import NTriplesWriter
from franz.openrdf.rio.rdfxmlwriter import RDFXMLWriter
from franz.openrdf.sail.allegrographserver import AllegroGraphServer
from franz.openrdf.vocabulary.rdf import RDF
from franz.openrdf.vocabulary.xmlschema import XMLSchema
import os
import urllib
import time, datetime


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

def load_kennedy():
    myRepository = test1()
    conn = myRepository.getConnection()
    conn.clear()   
    path1 = "./kennedy.ntriples"                
    baseURI = "http://example.org/example/local"
    context = myRepository.getValueFactory().createURI("http://example.org#kennedys")
    ## read kennedy triples into the null context:
    conn.add(path1, base=baseURI, format=RDFFormat.NTRIPLES, contexts=context)
    myRepository.indexTriples(all=True, asynchronous=False)
    conn = myRepository.getConnection()
    conn.setNamespace("kdy", "http://www.franz.com/simple#")
    return conn

def doTimeIt(conn, query, language='SPARQL', print_limit=0):
    tupleQuery = conn.prepareTupleQuery(language, query)
    tupleQuery.preferred_execution_language='PROLOG'
    timer = datetime.datetime.now()
    result = tupleQuery.evaluate();
    elapsed = datetime.datetime.now() - timer
    print "Elapsed time {0} with {1} results for query {2}".format(elapsed, result.tupleCount, query)
    count = 0 
    if print_limit:
        for row in result:
            print row                
            count += 1
            if count >= print_limit: break

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
    conn = load_kennedy()
    def timeIt(query, language='SPARQL', print_limit=0):
        tupleQuery = conn.prepareTupleQuery(language, query)
        tupleQuery.preferred_execution_language='PROLOG'
        timer = datetime.datetime.now()
        result = tupleQuery.evaluate();
        elapsed = datetime.datetime.now() - timer
        print "Elapsed time {0} with {1} results for query {2}".format(elapsed, result.tupleCount, query)
        count = 0 
        if print_limit:
            for row in result:
                print row                
                count += 1
                if count >= print_limit: break
    def variations(query):
        query1 = query.replace('DISTINCT', ' ').replace('LIMIT', 'LIMIT 1')
        query2 = query.replace('DISTINCT', ' ').replace('LIMIT', '')
        query3 = query.replace('LIMIT', 'LIMIT 1')
        query4 = query.replace('LIMIT', '')                        
        timeIt(query1)
        timeIt(query2)  
        timeIt(query3)
        timeIt(query4)  

    query1 = """select  ?s ?p ?o    where { 
                   { ?s rdf:type kdy:person }                 
                 . { ?s ?p ?o } 
                 }  """
    query2 = """select  ?s ?p ?o ?otype   where { 
                   { ?s rdf:type kdy:person }                 
                 . { ?s ?p ?o } 
                 .  { ?o rdf:type ?otype }
                 }"""
    query3 = """select ?s ?p ?o ?otype   where { 
                   { ?s rdf:type kdy:person }                 
                 . { ?s ?p ?o } 
                 . optional { ?o rdf:type ?otype }
                 }
                 order by ?s ?p ?o
                 """
    timeIt(query1)
    timeIt(query2, print_limit=20)  
    timeIt(query3, print_limit=20)      
    query4="""(select (?s ?p ?o) where 
                      (triple ?s rdf:type kdy:person)
                      (triple ?s ?p ?o))
          """
    query5="""(select (?s ?p ?o ?otype) where 
                      (triple ?s rdf:type kdy:person)
                      (triple ?s ?p ?o ?c)
                      (triple ?o rdf:type ?otype))
          """
    query6="""(select (?s ?p ?o ) where 
                      (triple ?s rdf:type kdy:person)
                      (triple ?s ?p ?o)
                      (optional (triple ?o rdf:type ?otype)))
          """
    timeIt(query4, language='COMMON_LOGIC')           
    timeIt(query5, language='COMMON_LOGIC')               
    timeIt(query6, language='COMMON_LOGIC')  

def test3():
    conn = load_kennedy()
    def timeIt(query, language, print_limit=-1):
        doTimeIt(conn, query, language=language, print_limit=print_limit)
    #timeIt("(select (?s ?c) where (quad ?s ?p ?o ?c) limit 6)", language="COMMON_LOGIC")
    #timeIt("(select-distinct (?s ?p ?o ?c) (q ?s ?p ?o ?c) (q ?s ?p2 ?o2 ?c) (:limit 3))", language="PROLOG", print_limit=5)
    timeIt("(select distinct (?s ?p ?o ?c) where (quad ?s ?p ?o ?c) (quad ?s ?p2 ?o2 ?c) limit 3)", language="COMMON_LOGIC", print_limit=5)    
    return

  
    
if __name__ == '__main__':
    choices = [i for i in range(1,15)]
    choices = [3]
    for choice in choices:
        print "\n==========================================================================="
        print "Test Run Number ", choice, "\n"
        if choice == 0: test0()
        elif choice == 1: test1()
        elif choice == 2: test2()
        elif choice == 3: test3()

                
