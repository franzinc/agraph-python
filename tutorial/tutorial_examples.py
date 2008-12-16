

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

def test3():    
    conn = test2().getConnection()
    try:
        queryString = "SELECT ?s ?p ?o  WHERE {?s ?p ?o .}"
        tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
        result = tupleQuery.evaluate();
        verify(result.rowCount(), 4, 'result.tupleCount', 3)
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
    statements = conn.getStatements(alice, None, None)
    statements.enableDuplicateFilter() ## there are no duplicates, but this exercises the code that checks
    verify(statements.rowCount(), 2, 'statements.rowCount()', 3)
    for s in statements:
        print s
    print "Same thing using JDBC:"
    resultSet = conn.getJDBCStatements(alice, None, None)
    verify(resultSet.rowCount(), 2, 'resultSet.rowCount()', 3)    
    while resultSet.next():
        #print resultSet.getRow()
        print "   ", resultSet.getValue(2), "   ", resultSet.getString(2)  
               
def test5():
    """
    Typed Literals
    """
    myRepository = test1()
    conn = myRepository.getConnection()
    f = myRepository.getValueFactory()
    conn.clear()
    exns = "http://example.org/people/"
    alice = f.createURI("http://example.org/people/alice")
    age = f.createURI(namespace=exns, localname="age")
    weight = f.createURI(namespace=exns, localname="weight")    
    favoriteColor = f.createURI(namespace=exns, localname="favoriteColor")
    birthdate = f.createURI(namespace=exns, localname="birthdate")
    ted = f.createURI(namespace=exns, localname="Ted")
    red = f.createLiteral('Red')
    rouge = f.createLiteral('Rouge', language="fr")
    fortyTwo = f.createLiteral('42', datatype=XMLSchema.INT)
    fortyTwoInteger = f.createLiteral('42', datatype=XMLSchema.LONG)    
    fortyTwoUntyped = f.createLiteral('42')
    date = f.createLiteral('1984-12-06', datatype=XMLSchema.DATE)     
    time = f.createLiteral('1984-12-06', datatype=XMLSchema.DATETIME)         
    stmt1 = f.createStatement(alice, age, fortyTwo)
    stmt2 = f.createStatement(ted, age, fortyTwoUntyped)    
    conn.add(stmt1)
    conn.addStatement(stmt2)
    conn.addTriple(alice, weight, f.createLiteral('20.5'))
    conn.addTriple(ted, weight, f.createLiteral('20.5', datatype=XMLSchema.FLOAT))
    conn.add(alice, favoriteColor, red)
    conn.add(ted, favoriteColor, rouge)
    conn.add(alice, birthdate, date)
    conn.add(ted, birthdate, time)    
    for obj in [None, fortyTwo, fortyTwoUntyped, f.createLiteral('20.5', datatype=XMLSchema.FLOAT), f.createLiteral('20.5'),
                red, rouge]:
        print "Retrieve triples matching '%s'." % obj
        statements = conn.getStatements(None, None, obj)
        for s in statements:
            print s
    for obj in ['42', '"42"', '20.5', '"20.5"', '"20.5"^^xsd:float', '"Rouge"@fr', '"Rouge"', '"1984-12-06"^^xsd:date']:
        print "Query triples matching '%s'." % obj
        queryString = """PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> 
        SELECT ?s ?p ?o WHERE {?s ?p ?o . filter (?o = %s)}
        """ % obj
        tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
        result = tupleQuery.evaluate();    
        for bindingSet in result:
            s = bindingSet[0]
            p = bindingSet[1]
            o = bindingSet[2]
            print "%s %s %s" % (s, p, o)
    fortyTwoInt = f.createLiteral(42)
    print fortyTwoInt.toPython()

def test6():
    myRepository = test1()
    conn = myRepository.getConnection()
    conn.clear()   
    path1 = "./vc-db-1.rdf"    
    path2 = "./football.nt"            
    baseURI = "http://example.org/example/local"
    context = myRepository.getValueFactory().createURI("http://example.org#vcards")
    conn.setNamespace("vcd", "http://www.w3.org/2001/vcard-rdf/3.0#");
    ## read football triples into the null context:
    conn.add(path2, base=baseURI, format=RDFFormat.NTRIPLES, contexts=None)
    ## read vcards triples into the context 'context':
    conn.addFile(path1, baseURI, format=RDFFormat.RDFXML, context=context);
    myRepository.indexTriples(all=True, asynchronous=False)
    print "After loading, repository contains %i vcard triples in context '%s'\n    and   %i football triples in context '%s'." % (
           conn.size(context), context, conn.size('null'), 'null')
    verify(conn.size(context), 16, 'conn.size(context)', 6)
    verify(conn.size('null'), 28, "conn.size('null)", 6)    
    return myRepository
        
def test7():    
    conn = test6().getConnection()
    print "Match all and print subjects and contexts"
    result = conn.getStatements(None, None, None, None)
    for row in result: print row.getSubject(), row.getContext()
    print "Same thing with SPARQL query (can't retrieve triples in the null context)"
    queryString = "SELECT DISTINCT ?s ?c WHERE {graph ?c {?s ?p ?o .} }"
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate();
    for bindingSet in result:
        print bindingSet[0], bindingSet[1]
    conn.close()

import urlparse

def test8():
    myRepository = test6() 
    conn = myRepository.getConnection()
    context = myRepository.getValueFactory().createURI("http://example.org#vcards")
    outputFile = "/tmp/temp.nt"
    #outputFile = None
    if outputFile == None:
        print "Writing RDF to Standard Out instead of to a file"
    ntriplesWriter = NTriplesWriter(outputFile)
    conn.export(ntriplesWriter, context);
    outputFile2 = "/tmp/temp.rdf"
    #outputFile2 = None
    if outputFile2 == None:
        print "Writing NTriples to Standard Out instead of to a file"
    rdfxmlfWriter = RDFXMLWriter(outputFile2)    
    conn.export(rdfxmlfWriter, context)

def test9():
    myRepository = test6()
    conn = myRepository.getConnection()
    conn.exportStatements(None, RDF.TYPE, None, False, RDFXMLWriter(None))

def test10():
    """
    Datasets and multiple contexts
    """
    myRepository = test1();
    conn = myRepository.getConnection()
    f = myRepository.getValueFactory()
    exns = "http://example.org/people/"
    alice = f.createURI(namespace=exns, localname="alice")
    bob = f.createURI(namespace=exns, localname="bob")
    ted = f.createURI(namespace=exns, localname="ted")
    person = f.createURI(namespace=exns, localname="Person")
    name = f.createURI(namespace=exns, localname="name")    
    alicesName = f.createLiteral("Alice")    
    bobsName = f.createLiteral("Bob")
    tedsName = f.createLiteral("Ted")    
    context1 = f.createURI(namespace=exns, localname="cxt1")      
    context2 = f.createURI(namespace=exns, localname="cxt2")          
    conn.add(alice, RDF.TYPE, person, context1)
    conn.add(alice, name, alicesName, context1)
    conn.add(bob, RDF.TYPE, person, context2)
    conn.add(bob, name, bobsName, context2)
    conn.add(ted, RDF.TYPE, person)
    conn.add(ted, name, bobsName)
    statements = conn.getStatements(None, None, None)
    verify(statements.rowCount(), 6, 'statements.rowCount()', 10)
    print "All triples in all contexts:"
    for s in statements:
        print s
    statements = conn.getStatements(None, None, None, [context1, context2])
    verify(statements.rowCount(), 4, 'statements.rowCount()', 10)
    print "Triples in contexts 1 or 2:"
    for s in statements:
        print s
    statements = conn.getStatements(None, None, None, ['null', context2])
    verify(statements.rowCount(), 4, 'statements.rowCount()', 10)
    print "Triples in contexts null or 2:"
    for s in statements:
        print s
    ## testing named graph query:
    queryString = """
    SELECT ?s ?p ?o ?c
    WHERE { GRAPH ?c {?s ?p ?o . } } 
    """
    ds = Dataset()
    ds.addNamedGraph(context1)
    ds.addNamedGraph(context2)
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    tupleQuery.setDataset(ds)
    result = tupleQuery.evaluate(); 
    verify(result.rowCount(), 4, 'result.rowCount()', 10)   
    print "Query over contexts 1 and 2."
    for bindingSet in result:
        print bindingSet.getRow()
    ## testing default graph query:
    queryString = """
    SELECT ?s ?p ?o    
    WHERE {?s ?p ?o . } 
    """
    ds = Dataset()
    ds.addDefaultGraph('null')
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    tupleQuery.setDataset(ds)   
    result = tupleQuery.evaluate(); 
    verify(result.rowCount(), 2, 'result.rowCount()', 10)    
    print "Query over the null context."
    for bindingSet in result:
        print bindingSet.getRow()
    
def test11():
    """
    Namespaces
    """
    myRepository = test1();
    conn = myRepository.getConnection()
    f = myRepository.getValueFactory()
    exns = "http://example.org/people/"
    alice = f.createURI(namespace=exns, localname="alice")
    person = f.createURI(namespace=exns, localname="Person")
    conn.add(alice, RDF.TYPE, person)
    myRepository.indexTriples(all=True, asynchronous=True)
    conn.setNamespace('ex', exns)
    #conn.removeNamespace('ex')
    queryString = """
    SELECT ?s ?p ?o 
    WHERE { ?s ?p ?o . FILTER ((?p = rdf:type) && (?o = ex:Person) ) }
    """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate();  
    print    
    for bindingSet in result:
        print bindingSet[0], bindingSet[1], bindingSet[2]

def test12():
    """
    Text search
    """
    myRepository = test1();
    conn = myRepository.getConnection()
    f = myRepository.getValueFactory()
    exns = "http://example.org/people/"
    conn.setNamespace('ex', exns)
    #myRepository.registerFreeTextPredicate("http://example.org/people/name")    
    myRepository.registerFreeTextPredicate(namespace=exns, localname='fullname')
    alice = f.createURI(namespace=exns, localname="alice1")
    persontype = f.createURI(namespace=exns, localname="Person")
    fullname = f.createURI(namespace=exns, localname="fullname")    
    alicename = f.createLiteral('Alice B. Toklas')
    book =  f.createURI(namespace=exns, localname="book1")
    booktype = f.createURI(namespace=exns, localname="Book")
    booktitle = f.createURI(namespace=exns, localname="title")    
    wonderland = f.createLiteral('Alice in Wonderland')
    conn.clear()    
    conn.add(alice, RDF.TYPE, persontype)
    conn.add(alice, fullname, alicename)
    conn.add(book, RDF.TYPE, booktype)    
    conn.add(book, booktitle, wonderland) 
    ##myRepository.indexTriples(all=True, asynchronous=True)
    conn.setNamespace('ex', exns)
    #conn.setNamespace('fti', "http://franz.com/ns/allegrograph/2.2/textindex/")    
    queryString = """
    SELECT ?s ?p ?o
    WHERE { ?s ?p ?o . ?s fti:match 'Alice' . }
    """
#    queryString=""" 
#    SELECT ?s ?p ?o
#    WHERE { ?s ?p ?o . FILTER regex(?o, "Ali") }
#    """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate(); 
    print "Found %i query results" % len(result.string_tuples)
    print "Found %i query results" % result.tupleCount    
    count = 0
    for bindingSet in result:
        print bindingSet
        count += 1
        if count > 5: break


def test13():
    """
    Ask, Construct, and Describe queries 
    """
    conn = test2().getConnection()
    conn.setNamespace('ex', "http://example.org/people/")
    conn.setNamespace('ont', "http://example.org/ontology/")
    queryString = """select ?s ?p ?o where { ?s ?p ?o} """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate();
    for r in result: print r     
    queryString = """ask { ?s ont:name "Alice" } """
    booleanQuery = conn.prepareBooleanQuery(QueryLanguage.SPARQL, queryString)
    result = booleanQuery.evaluate(); 
    print "Boolean result", result
    queryString = """construct {?s ?p ?o} where { ?s ?p ?o . filter (?o = "Alice") } """
    constructQuery = conn.prepareGraphQuery(QueryLanguage.SPARQL, queryString)
    result = constructQuery.evaluate(); 
    print "Construct result", [st for st in result]
    queryString = """describe ?s where { ?s ?p ?o . filter (?o = "Alice") } """
    describeQuery = conn.prepareGraphQuery(QueryLanguage.SPARQL, queryString)
    result = describeQuery.evaluate(); 
    print "Describe result"
    for st in result: print st 
    
def test14():
    """
    Parametric queries
    """
    conn = test2().getConnection()
    f = conn.getValueFactory()
    alice = f.createURI("http://example.org/people/alice")
    bob = f.createURI("http://example.org/people/bob")
    queryString = """select ?s ?p ?o where { ?s ?p ?o} """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    tupleQuery.setBinding("s", alice)
    result = tupleQuery.evaluate()
    return
    print "Facts about Alice:"
    for r in result: print r  
    tupleQuery.setBinding("s", bob)
    print "Facts about Bob:"    
    result = tupleQuery.evaluate()
    for r in result: print r  
    
def test15():
    """
    Range matches
    """
    myRepository = test1();
    conn = myRepository.getConnection()
    conn.clear()
    f = myRepository.getValueFactory()
    exns = "http://example.org/people/"
    conn.setNamespace('ex', exns)
    alice = f.createURI(namespace=exns, localname="alice")
    bob = f.createURI(namespace=exns, localname="bob")
    carol = f.createURI(namespace=exns, localname="carol")    
    age = f.createURI(namespace=exns, localname="age")    
    range = f.createRange(30, 50)
    if False: myRepository.registerInlinedDatatype(predicate=age, inlinedType="int")
    if True: myRepository.registerInlinedDatatype(datatype=XMLSchema.INT, inlinedType="int")    
    ## THIS IS A TODO:
    #myRepository.inlineStandardDatatypes()
    conn.add(alice, age, 42)
    conn.add(bob, age, 24)    
    conn.add(carol, age, "39")        
    rows = conn.getStatements(None, age, range)
    for r in rows:
        print r 

def test16():
    """
    Federated triple stores.
    """
    def pt(kind, rows):
        print "\n%s Apples:\t" % kind.capitalize(),
        for r in rows: print r[0].getLocalName(),
    
    server = AllegroGraphServer("localhost", port=8080)
    catalog = server.openCatalog('scratch')  
    print "Available repositories in catalog '%s':  %s" % (catalog.getName(), catalog.listRepositories()) 
    redConn = catalog.getRepository("redthings", Repository.RENEW).initialize().getConnection()
    rf = redConn.getValueFactory()
    greenConn = greenRepository = catalog.getRepository("greenthings", Repository.RENEW).initialize().getConnection()
    gf = greenConn.getValueFactory()    
    rainbowConn = (catalog.getRepository("rainbowthings", Repository.RENEW)
                         .addFederatedTripleStores(["redthings", "greenthings"]).initialize().getConnection())
    rbf = rainbowConn.getValueFactory()
    ex = "http://www.demo.com/example"
    redConn.add(rf.createURI(ex+"mcintosh"), RDF.TYPE, rf.createURI(ex+"Apple"))
    redConn.add(rf.createURI(ex+"reddelicious"), RDF.TYPE, rf.createURI(ex+"Apple"))    
    greenConn.add(gf.createURI(ex+"pippin"), RDF.TYPE, gf.createURI(ex+"Apple"))
    greenConn.add(gf.createURI(ex+"kermitthefrog"), RDF.TYPE, gf.createURI(ex+"Frog"))
    redConn.setNamespace('ex', ex)
    greenConn.setNamespace('ex', ex)
    rainbowConn.setNamespace('ex', ex)        
    queryString = "select ?s where { ?s rdf:type ex:Apple }"
    pt("red", redConn.prepareTupleQuery(QueryLanguage.SPARQL, queryString).evaluate())
    pt("green", greenConn.prepareTupleQuery(QueryLanguage.SPARQL, queryString).evaluate())
    pt("federated", rainbowConn.prepareTupleQuery(QueryLanguage.SPARQL, queryString).evaluate()) 

def test26():
    """
    Queries per second.
    """
    myRepository = test6()
    conn = myRepository.getConnection()
    
    reps = 1 #1000
    
    ##TEMPORARY
    context = myRepository.getValueFactory().createURI("http://example.org#vcards")
    ## END TEMPORARY
    
    t = time.time()
    for i in range(reps):
        count = 0
        resultSet = conn.getJDBCStatements(None, None, None, [context, None])
        while resultSet.next(): count += 1
    print "Did %d %d-row matches in %f seconds." % (reps, count, time.time() - t)
 
    t = time.time()
    for i in range(reps):
        count = 0
        statments = conn.getStatements(None, None, None, None)
        for st in statments:
            st.getSubject()
            st.getPredicate()
            st.getObject() 
            count += 1
    print "Did %d %d-row matches in %f seconds." % (reps, count, time.time() - t)
   
    for size in [1, 5, 10, 100]:
        queryString = """select ?x ?y ?z {?x ?y ?z} limit %d""" % size
        tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
        t = time.time()
        for i in range(reps):
            count = 0
            result = tupleQuery.evaluate(); 
            for row in result: count += 1
            #while result.next(): count += 1
        print "Did %d %d-row queries in %f seconds." % (reps, count, time.time() - t)

def test27 ():
    """ CIA FACTBOOK """
    myRepository = test1(Repository.ACCESS)
    conn = myRepository.getConnection()
    f = myRepository.getValueFactory()
    if conn.size() == 0:
        print "Reading CIA Fact Book file."
        path1 = "/FRANZ_CONSULTING/data/ciafactbook.nt"    
        baseURI = "http://example.org/example/local"
        conn.add(path1, base=baseURI, format=RDFFormat.NTRIPLES, serverSide=True)
    myRepository.indexTriples(True);
    t = time.time()
    count = 0
    resultSet = conn.getJDBCStatements(None, None, None, None)
    while resultSet.next(): count += 1
    print "Did %d-row matches in %f seconds." % (count, time.time() - t)
    queryString = "select ?x ?y ?z {?x ?y ?z}"
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    t = time.time()
    count = 0
    result = tupleQuery.evaluate(); 
    for row in result: count += 1
    print "Did %d-row queries in %f seconds." % (count, time.time() - t)
    

    
if __name__ == '__main__':
    choices = [i for i in range(1,15)]
    choices = [16]
    for choice in choices:
        print "\n==========================================================================="
        print "Test Run Number ", choice, "\n"
        if choice == 0: test0()
        elif choice == 1: test1()
        elif choice == 2: test2()
        elif choice == 3: test3()
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
         
        elif choice == 26: test26()                                                                                              
        elif choice == 27: test27()                                                                                                      
        else:
            print "No such test exists."
    
