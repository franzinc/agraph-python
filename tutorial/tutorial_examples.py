

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
    connection = myRepository.getConnection()
    print "Repository %s is up!  It contains %i statements." % (
                myRepository.getDatabaseName(), connection.size())
    return connection
    
def test2():
    conn = test1()
    ## create some resources and literals to make statements out of
    alice = conn.createURI("http://example.org/people/alice")
    bob = conn.createURI("http://example.org/people/bob")
    #bob = conn.createBNode()
    name = conn.createURI("http://example.org/ontology/name")
    person = conn.createURI("http://example.org/ontology/Person")
    bobsName = conn.createLiteral("Bob")
    alicesName = conn.createLiteral("Alice")
    print "Triple count before inserts: ", conn.size()
    for s in conn.getStatements(None, None, None, None): print s    
    ## alice is a person
    conn.add(alice, RDF.TYPE, person)
    ## alice's name is "Alice"
    conn.add(alice, name, alicesName)
    ## bob is a person
    conn.add(bob, RDF.TYPE, person)
    ## bob's name is "Bob":
    conn.add(bob, conn.createURI("http://example.org/ontology/name"), bobsName)
    print "Triple count: ", conn.size()
    verify(conn.size(), 4, 'conn.size()', 2)
    conn.remove(bob, name, bobsName)
    print "Triple count: ", conn.size()
    verify(conn.size(), 3, 'conn.size()', 2)
    conn.add(bob, name, bobsName)    
    return conn

def test3():    
    conn = test2()
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
    conn = test2()
    alice = conn.createURI("http://example.org/people/alice")
    statements = conn.getStatements(alice, None, None)
    statements.enableDuplicateFilter() ## there are no duplicates, but this exercises the code that checks
    verify(statements.rowCount(), 2, 'statements.rowCount()', 3)
    for s in statements:
        print s
    print "Same thing using JDBC:"
    resultSet = conn.getJDBCStatements(alice, None, None)
    verify(resultSet.rowCount(), 2, 'resultSet.rowCount()', 3)    
    while resultSet.next():        
        print "   ", resultSet.getValue(2), "   ", resultSet.getString(2)  
               
def test5():
    """
    Typed Literals
    """
    conn = test1()
    conn.clear()
    exns = "http://example.org/people/"
    alice = conn.createURI("http://example.org/people/alice")
    age = conn.createURI(namespace=exns, localname="age")
    weight = conn.createURI(namespace=exns, localname="weight")    
    favoriteColor = conn.createURI(namespace=exns, localname="favoriteColor")
    birthdate = conn.createURI(namespace=exns, localname="birthdate")
    ted = conn.createURI(namespace=exns, localname="Ted")
    red = conn.createLiteral('Red')
    rouge = conn.createLiteral('Rouge', language="fr")
    fortyTwo = conn.createLiteral('42', datatype=XMLSchema.INT)
    fortyTwoInteger = conn.createLiteral('42', datatype=XMLSchema.LONG)    
    fortyTwoUntyped = conn.createLiteral('42')
    date = conn.createLiteral('1984-12-06', datatype=XMLSchema.DATE)     
    #time = conn.createLiteral('1984-12-06', datatype=XMLSchema.DATETIME)         
    stmt1 = conn.createStatement(alice, age, fortyTwo)
    stmt2 = conn.createStatement(ted, age, fortyTwoUntyped)    
    conn.add(stmt1)
    conn.addStatement(stmt2)
    conn.addTriple(alice, weight, conn.createLiteral('20.5'))
    conn.addTriple(ted, weight, conn.createLiteral('20.5', datatype=XMLSchema.FLOAT))
    conn.add(alice, favoriteColor, red)
    conn.add(ted, favoriteColor, rouge)
    conn.add(alice, birthdate, date)
    conn.add(ted, birthdate, time)    
    for obj in [None, fortyTwo, fortyTwoUntyped, conn.createLiteral('20.5', datatype=XMLSchema.FLOAT), conn.createLiteral('20.5'),
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
    fortyTwoInt = conn.createLiteral(42)
    print fortyTwoInt.toPython()

def test6():
    conn = test1()
    conn.clear()   
    path1 = "./vc-db-1.rdf"    
    path2 = "./kennedy.ntriples"                
    baseURI = "http://example.org/example/local"
    context = conn.createURI("http://example.org#vcards")
    ## read kennedy triples into the null context:
    conn.add(path2, base=baseURI, format=RDFFormat.NTRIPLES, contexts=None)
    ## read vcards triples into the context 'context':
    conn.addFile(path1, baseURI, format=RDFFormat.RDFXML, context=context);
    conn.indexTriples(all=True, asynchronous=False)
    print "After loading, repository contains %i vcard triples in context '%s'\n    and   %i kennedy triples in context '%s'." % (
           conn.size(context), context, conn.size('null'), 'null')
    verify(conn.size(context), 16, 'conn.size(context)', 6)
    verify(conn.size('null'), 1214, "conn.size('null)", 6)    
    return conn
        
def test7():    
    conn = test6()
    print "Match all and print subjects and contexts"
    result = conn.getStatements(None, None, None, None, limit=25)
    for row in result: print row.getSubject(), row.getContext()
    print "\nSame thing with SPARQL query (can't retrieve triples in the null context)"
    queryString = "SELECT DISTINCT ?s ?c WHERE {graph ?c {?s ?p ?o .} }"
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate();
    for i, bindingSet in enumerate(result):
        print bindingSet[0], bindingSet[1]
    conn.close()

import urlparse

def test8():
    conn = test6()
    context = conn.createURI("http://example.org#vcards")
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
    conn = test6()
    conn.exportStatements(None, RDF.TYPE, None, False, RDFXMLWriter(None))

def test10():
    """
    Datasets and multiple contexts
    """
    conn = test1()
    exns = "http://example.org/people/"
    alice = conn.createURI(namespace=exns, localname="alice")
    bob = conn.createURI(namespace=exns, localname="bob")
    ted = conn.createURI(namespace=exns, localname="ted")
    person = conn.createURI(namespace=exns, localname="Person")
    name = conn.createURI(namespace=exns, localname="name")    
    alicesName = conn.createLiteral("Alice")    
    bobsName = conn.createLiteral("Bob")
    tedsName = conn.createLiteral("Ted")    
    context1 = conn.createURI(namespace=exns, localname="cxt1")      
    context2 = conn.createURI(namespace=exns, localname="cxt2")          
    conn.add(alice, RDF.TYPE, person, context1)
    conn.add(alice, name, alicesName, context1)
    conn.add(bob, RDF.TYPE, person, context2)
    conn.add(bob, name, bobsName, context2)
    conn.add(ted, RDF.TYPE, person)
    conn.add(ted, name, tedsName)
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
    conn = test1()
    exns = "http://example.org/people/"
    alice = conn.createURI(namespace=exns, localname="alice")
    person = conn.createURI(namespace=exns, localname="Person")
    conn.add(alice, RDF.TYPE, person)
    conn.indexTriples(all=True, asynchronous=True)
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
    conn = test1()
    exns = "http://example.org/people/"
    conn.setNamespace('ex', exns)
    #myRepository.registerFreeTextPredicate("http://example.org/people/name")    
    conn.registerFreeTextPredicate(namespace=exns, localname='fullname')
    alice = conn.createURI(namespace=exns, localname="alice1")
    persontype = conn.createURI(namespace=exns, localname="Person")
    fullname = conn.createURI(namespace=exns, localname="fullname")    
    alicename = conn.createLiteral('Alice B. Toklas')
    book =  conn.createURI(namespace=exns, localname="book1")
    booktype = conn.createURI(namespace=exns, localname="Book")
    booktitle = conn.createURI(namespace=exns, localname="title")    
    wonderland = conn.createLiteral('Alice in Wonderland')
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
    conn = test2()
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
    conn = test2()
    alice = conn.createURI("http://example.org/people/alice")
    bob = conn.createURI("http://example.org/people/bob")
    queryString = """select ?s ?p ?o where { ?s ?p ?o} """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    tupleQuery.setBinding("s", alice)
    result = tupleQuery.evaluate()    
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
    conn = test1()
    conn.clear()
    exns = "http://example.org/people/"
    conn.setNamespace('ex', exns)
    alice = conn.createURI(namespace=exns, localname="alice")
    bob = conn.createURI(namespace=exns, localname="bob")
    carol = conn.createURI(namespace=exns, localname="carol")    
    age = conn.createURI(namespace=exns, localname="age")    
    range = conn.createRange(30, 50)
    if False: conn.registerDatatypeMapping(predicate=age, nativeType="int")
    if True: conn.registerDatatypeMapping(datatype=XMLSchema.INT, nativeType="float")    
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
    
    catalog = AllegroGraphServer("localhost", port=8080).openCatalog('scratch') 
    ## create two ordinary stores, and one federated store: 
    redConn = catalog.getRepository("redthings", Repository.RENEW).initialize().getConnection()
    greenConn = greenRepository = catalog.getRepository("greenthings", Repository.RENEW).initialize().getConnection()
    rainbowConn = (catalog.getRepository("rainbowthings", Repository.RENEW)
                         .addFederatedTripleStores(["redthings", "greenthings"]).initialize().getConnection())
    ex = "http://www.demo.com/example#"
    ## add a few triples to the red and green stores:
    redConn.add(redConn.createURI(ex+"mcintosh"), RDF.TYPE, redConn.createURI(ex+"Apple"))
    redConn.add(redConn.createURI(ex+"reddelicious"), RDF.TYPE, redConn.createURI(ex+"Apple"))    
    greenConn.add(greenConn.createURI(ex+"pippin"), RDF.TYPE, greenConn.createURI(ex+"Apple"))
    greenConn.add(greenConn.createURI(ex+"kermitthefrog"), RDF.TYPE, greenConn.createURI(ex+"Frog"))
    redConn.setNamespace('ex', ex)
    greenConn.setNamespace('ex', ex)
    rainbowConn.setNamespace('ex', ex)        
    queryString = "select ?s where { ?s rdf:type ex:Apple }"
    ## query each of the stores; observe that the federated one is the union of the other two:
    pt("red", redConn.prepareTupleQuery(QueryLanguage.SPARQL, queryString).evaluate())
    pt("green", greenConn.prepareTupleQuery(QueryLanguage.SPARQL, queryString).evaluate())
    pt("federated", rainbowConn.prepareTupleQuery(QueryLanguage.SPARQL, queryString).evaluate()) 

def test17():
    """
    Prolog queries
    """
    conn = test6()
    conn.deleteEnvironment("kennedys") ## start fresh        
    conn.setEnvironment("kennedys") 
    conn.setNamespace("kdy", "http://www.franz.com/simple#")
    conn.setRuleLanguage(QueryLanguage.PROLOG)   
    rules2 = """
    (<-- (female ?x) ;; IF
         (q ?x !kdy:sex !kdy:female))
    (<-- (male ?x) ;; IF
         (q ?x !kdy:sex !kdy:male))
    """
    conn.addRules(rules2)
    ## This causes a failure(correctly):
    #conn.deleteRule('male')
    queryString2 = """
    (select (?person ?name)
            (q ?person !rdf:type !kdy:person)
            (male ?person)
            (q ?person !kdy:first-name ?name)
            )
    """
    tupleQuery2 = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString2)
    result = tupleQuery2.evaluate();     
    for row in result:
        print row

def test18():
    """
    Loading Prolog rules
    """
    def pq(queryString):
        tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
        result = tupleQuery.evaluate();     
        for row in result:
            print row
            
    conn = test6()
    conn.deleteEnvironment("kennedys") ## start fresh        
    conn.setEnvironment("kennedys") 
    conn.setNamespace("kdy", "http://www.franz.com/simple#")
    conn.setNamespace("rltv", "http://www.franz.com/simple#")  
    conn.setRuleLanguage(QueryLanguage.PROLOG)
    path = "./relative_rules.txt"
    conn.loadRules(path)
    pq("""(select ?x (string-concat ?x "a" "b" "c"))""")
    pq("""(select (?person ?uncle) (uncle ?y ?x)(name ?x ?person)(name ?y ?uncle))""")
        
    
def test26():
    """
    Queries per second.
    """
    conn = test6()
    
    reps = 1 #1000
    
    ##TEMPORARY
    context = conn.createURI("http://example.org#vcards")
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
    conn = test1(Repository.ACCESS)
    if conn.size() == 0:
        print "Reading CIA Fact Book file."
        path1 = "/FRANZ_CONSULTING/data/ciafactbook.nt"    
        baseURI = "http://example.org/example/local"
        conn.add(path1, base=baseURI, format=RDFFormat.NTRIPLES, serverSide=True)
    conn.indexTriples(True);
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
    

def test28():
    """
    Prolog queries
    """
    c = test1()
    qCounter = [0]
    def pq(query):
        qCounter[0] = qCounter[0] + 1
        print "QUERY{0}: ".format(qCounter[0]), query
        tupleQuery = c.prepareTupleQuery(QueryLanguage.PROLOG, query)
        try:
            result = tupleQuery.evaluate();
            print "   Query returns {0} rows".format(result.tupleCount)     
            for row in result:
                print row
        except Exception, ex:
            print "Failure ", ex
#        else:
#            for row in result:
#                print row

    exns = "http://example.org/people/"
    c.setNamespace("ex", exns)    
    alice = c.createURI(exns, "alice")
    person = c.createURI(exns, "Person")
    name = c.createURI(exns, "name")    
    alicesName = c.createLiteral("Alice")    
    context1 = c.createURI(exns, "cxt1")      
    context2 = c.createURI(exns, "cxt2")          
    c.addTriples([(alice, RDF.TYPE, person, context1),
                     (alice, name, alicesName, context2),
                     (alice, c.createURI(exns, "age"), 42, context1),
                     ])
    c.setRuleLanguage(QueryLanguage.PROLOG)   
#    pq("""(select (?s ?p ?o ?c) (q ?s ?p ?o ?c))""")
#    pq("""(select (?s ?p ?o ?c) 
#                  (member ?c (?? (list !ex:cxt1))) 
#                  (q ?s ?p ?o ?c))""")
#    pq("""(select (?s ?p ?o ?c) 
#                  (q ?s ?p ?o ?c)
#                  (member ?c (?? (list !ex:cxt1))))                 
#                  """)
#    pq("""(select (?x) (= ?x 42))""")
#    pq("""(select (?x) (= ?x (literal 42)))""")    
#    pq("""(select (?x) (member ?x (?? (list 42))))""")
#    pq("""(select (?x) (member ?x (?? (list (literal "42")))))""")
    pq("""(select (?x) (member ?x (?? (list (literal "42"))))
                       (lispp (upi= ?x (literal "4"))) )""")  
    
    pq(""" (select (?x) (member ?x (?? (list (literal "42"))))
                       (and (lispp (upi= ?x (literal "42")))) )   """)  
    
    pq(""" (select (?x) (member ?x (?? (list (literal "42"))))
                        (and (lispp t) ))   """)  

  
    pq("""(select (?s ?p ?o ?x) (q ?s ?p ?o))""")
      
#    pq("""(select (?x) (member ?x (?? (list (literal "42"))))
#                       (member ?x (?? (list (literal "43")))                      
#                       )""")    
#    pq("""   (select (?s ?p ?o ?c ?lac ?otype ?c2) 
#               (and (q ?s ?p ?o ?c) 
#                    (lisp* ?v (upi= ?c !<http://www.wildsemantics.com/SystemWorld_context>))) 
#                    (or (q ?o !<http://www.wildsemantics.com/systemworld#lookAheadCapsule> ?lac !<http://www.wildsemantics.com/SystemWorld_context>) 
#                        (not (q ?o !<http://www.wildsemantics.com/systemworld#lookAheadCapsule> ?lac !<http://www.wildsemantics.com/SystemWorld_context>))) 
#                        (and (q ?o !<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> ?otype ?c2) (lispp (upi= ?c2 !<http://www.wildsemantics.com/SystemWorld_context>)))
#                        )
# """)

    
if __name__ == '__main__':
    choices = [i for i in range(1,19)]
    choices = [5]
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
        elif choice == 17: test17()                    
        elif choice == 18: test18()                                                             
         
        elif choice == 26: test26()                                                                                              
        elif choice == 27: test27()                                                                                                      
        elif choice == 28: test28()                                                                                                              
        else:
            print "No such test exists."
    
