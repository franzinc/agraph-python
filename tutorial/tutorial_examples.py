import tempfile

from franz.openrdf.sail.allegrographserver import AllegroGraphServer
from franz.openrdf.repository.repository import Repository
from franz.miniclient import repository
from franz.openrdf.query.query import QueryLanguage
from franz.openrdf.model import URI
from franz.openrdf.vocabulary.rdf import RDF
from franz.openrdf.vocabulary.rdfs import RDFS
from franz.openrdf.vocabulary.owl import OWL
from franz.openrdf.vocabulary.xmlschema import XMLSchema
from franz.openrdf.query.dataset import Dataset
from franz.openrdf.rio.rdfformat import RDFFormat
from franz.openrdf.rio.rdfwriter import  NTriplesWriter
from franz.openrdf.rio.rdfxmlwriter import RDFXMLWriter

import os, urllib, datetime, time, sys

CURRENT_DIRECTORY = os.getcwd()

# Directory containing the data files.
# Use the location of the script file or the current working
# directory if that location is not known.
BASE_DIR = os.path.dirname(os.path.realpath(__file__)) if '__file__' in globals() else os.getcwd()

AG_HOST = os.environ.get('AGRAPH_HOST', 'localhost')
AG_PORT = int(os.environ.get('AGRAPH_PORT', '10035'))
AG_CATALOG = os.environ.get('AGRAPH_CATALOG', '')
# Every example uses this repository except example16(),
# which creates 2 repos: redthings and greenthings, 
AG_REPOSITORY = 'pythontutorial'
AG_USER = os.environ.get('AGRAPH_USER', 'test')
AG_PASSWORD = os.environ.get('AGRAPH_PASSWORD', 'xyzzy')

RAISE_EXCEPTION_ON_VERIFY_FAILURE = False

# Update this when adding new examples
NUMBER_OF_EXAMPLES = 24

def verify(expressionValue, targetValue, quotedExpression, testNum):
    """
    Verify that 'expressionValue' equals 'targetValue'.  If not,
    raise an exception, or print a message advertising the failure.
    """
    if not expressionValue == targetValue:
        message = ("Diagnostic failure in example %s.  Expression '%s' returns '%s' where '%s' expected." %
                    (testNum, quotedExpression, expressionValue, targetValue))
        if RAISE_EXCEPTION_ON_VERIFY_FAILURE:
            raise Exception(message)
        else:
            print "BWEEP BWEEP BWEEP BWEEP BWEEP BWEEP BWEEP BWEEP BWEEP BWEEP BWEEP BWEEP BWEEP BWEEP BWEEP \n   ", message

def example0():
    """
    Can we connect to AG?
    """
    print "Starting example example0()."
    print "Current working directory is '%s'" % (os.getcwd())
    server = AllegroGraphServer(AG_HOST, AG_PORT, AG_USER, AG_PASSWORD)
    print "Available catalogs", server.listCatalogs()


def example1(accessMode=Repository.RENEW):
    """
    Tests getting the repository up.  Is called by the other examples to do the startup.
    """
    print "Starting example1()."
    print "Defining connnection to AllegroGraph server -- host:'%s' port:%s" % (AG_HOST, AG_PORT)
    print "Default working directory is '%s'" % (CURRENT_DIRECTORY)
    server = AllegroGraphServer(AG_HOST, AG_PORT, AG_USER, AG_PASSWORD)
    print "Available catalogs", server.listCatalogs()
    catalog = server.openCatalog(AG_CATALOG)  ## named catalog
    print "Available repositories in catalog '%s':  %s" % (catalog.getName(), catalog.listRepositories())    
    myRepository = catalog.getRepository(AG_REPOSITORY, accessMode)
    myRepository.initialize()
    conn = myRepository.getConnection()
    print "Repository %s is up!  It contains %i statements." % (
                myRepository.getDatabaseName(), conn.size())
    indices = conn.listValidIndices()
    print "All valid triple indices: %s" % (indices)
    indices = conn.listIndices()
    print "Current triple indices: %s" % (indices)
    print "Removing graph indices..."
    conn.dropIndex("gospi")
    conn.dropIndex("gposi")
    conn.dropIndex("gspoi")
    indices = conn.listIndices()
    print "Current triple indices: %s" % (indices)
    print "Adding one graph index back in..."
    conn.addIndex("gspoi")
    indices = conn.listIndices()
    print "Current triple indices: %s" % (indices)
    return conn

    
def example2():
    conn = example1()
    print "Starting example2()."
    print "Default working directory is '%s'" % (CURRENT_DIRECTORY)
    ## create some resources and literals to make statements out of
    alice = conn.createURI("http://example.org/people/alice")
    bob = conn.createURI("http://example.org/people/bob")
    name = conn.createURI("http://example.org/ontology/name")
    person = conn.createURI("http://example.org/ontology/Person")
    bobsName = conn.createLiteral("Bob")
    alicesName = conn.createLiteral("Alice")
    print "Triple count before inserts: ", conn.size()
    for s in conn.getStatements(None, None, None, None, False): print s    
    ## alice is a person
    conn.add(alice, RDF.TYPE, person)
    ## alice's name is "Alice"
    conn.add(alice, name, alicesName)
    ## bob is a person
    conn.add(bob, RDF.TYPE, person)
    ## bob's name is "Bob":
    conn.add(bob, name, bobsName)
    print "Added four triples."
    print "Triple count: ", conn.size()
    for s in conn.getStatements(None, None, None, None, False): print s    
    conn.remove(bob, name, bobsName)
    print "Removed one triple."
    print "Triple count: ", conn.size()
    conn.add(bob, name, bobsName)    
    return conn
    
def example3():    
    conn = example2()
    print "Starting example3()."
    print "Default working directory is '%s'" % (CURRENT_DIRECTORY)
    print "SPARQL query for all triples in repository."
    try:
        queryString = "SELECT ?s ?p ?o  WHERE {?s ?p ?o .}"
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
        myRepository = conn.repository
        myRepository.shutDown()

        
def example4():
    conn = example2()
    print "Starting example4()."
    print "Default working directory is '%s'" % (CURRENT_DIRECTORY)
    alice = conn.createURI("http://example.org/people/alice")
    print "Searching for Alice using getStatements():"
    statements = conn.getStatements(alice, None, None, False)
    # statements.enableDuplicateFilter() ## there are no duplicates, but this exercises the code that checks
    for s in statements:
        print s
    statements.close()
    conn.close();
    myRepository = conn.repository
    myRepository.shutDown()
		
def example5():
    """
    Typed Literals
    """
    print "\nStarting example5()."
    print "Default working directory is '%s'" % (CURRENT_DIRECTORY)
    conn = example1()
    conn.clear()
    exns = "http://people/"
    alice = conn.createURI("http://people/alice")
    bob = conn.createURI(namespace=exns, localname="bob")
    carol = conn.createURI(namespace=exns, localname="carol")
    dave = conn.createURI(namespace=exns, localname="dave")
    eric = conn.createURI(namespace=exns, localname="eric")
    fred = conn.createURI(namespace=exns, localname="fred")
    greg = conn.createURI(namespace=exns, localname="greg")
    # numeric datatypes
    age = conn.createURI(namespace=exns, localname="age")
    fortyTwo = conn.createLiteral(42)          # creates long
    fortyTwoDouble = conn.createLiteral(42.0)  # creates double
    fortyTwoInt = conn.createLiteral('42', datatype=XMLSchema.INT)
    fortyTwoLong = conn.createLiteral('42', datatype=XMLSchema.LONG)  
    fortyTwoFloat = conn.createLiteral('42', datatype=XMLSchema.FLOAT)   
    fortyTwoString = conn.createLiteral('42', datatype=XMLSchema.STRING)
    fortyTwoPlain = conn.createLiteral('42')   # creates untyped string
    stmt1 = conn.createStatement(alice, age, fortyTwo)
    stmt2 = conn.createStatement(bob, age, fortyTwoDouble)    
    stmt3 = conn.createStatement(carol, age, fortyTwoInt)    
    stmt4 = conn.createStatement(dave, age, fortyTwoLong)    
    stmt5 = conn.createStatement(eric, age, fortyTwoFloat)    
    stmt6 = conn.createStatement(fred, age, fortyTwoString) 
    stmt7 = conn.createStatement(greg, age, fortyTwoPlain)   
    conn.add(stmt1)
    conn.add(stmt2)
    conn.add(stmt3)
    conn.addStatement(stmt4)
    conn.addStatement(stmt5)
    conn.addStatement(stmt6)
    conn.addStatement(stmt7)

    # This section retrieves the age triples to see what datatypes are present. 
    print "\nShowing all age triples using getStatements(). Seven matches."
    statements = conn.getStatements(None, age, None)
    for s in statements:
        print s

    print "----------------------------------------------------------------------------"
    print "GARY: The SQARQL 'direct' matches in the following set of six examples produce no results."

    # Matches against 42, undeclared int.
    print "\ngetStatements() request for 42, matches longs."
    statements = conn.getStatements(None, age, 42)
    for s in statements:
        print s

    print "\nSPARQL matches for 42 (filter match) finds multiple numeric types."
    queryString = """SELECT ?s ?p ?o WHERE {?s ?p ?o . filter (?o = 42)}"""
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate();    
    for bindingSet in result:
        s = bindingSet[0]
        p = bindingSet[1]
        o = bindingSet[2]
        print "%s %s %s" % (s, p, o)

    print "\nSPARQL matches for 42 (direct match)."
    queryString = """SELECT ?s ?p WHERE {?s ?p 42 .}"""
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate();    
    for bindingSet in result:
        s = bindingSet[0]
        p = bindingSet[1]
        print "%s %s" % (s, p)

    # Matches against 42.0, undeclared double.
    print "\ngetStatements() request for 42.0 matches double but not float."
#    statements = conn.getStatements(None, age, fortyTwoDouble)
    statements = conn.getStatements(None, age, 42.0)
    for s in statements:
        print s

    # Matches against fortyTwoDouble, Literal double.
    print "\ngetStatements() request for fortyTwoDouble matches double but not float."
    statements = conn.getStatements(None, age, fortyTwoDouble)
#    statements = conn.getStatements(None, age, 42.0)
    for s in statements:
        print s

    print "\nSPARQL matches for 42.0 (filter match) finds multiple numeric types."
    queryString = """SELECT ?s ?p ?o WHERE {?s ?p ?o . filter (?o = 42.0)}"""
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate();    
    for bindingSet in result:
        s = bindingSet[0]
        p = bindingSet[1]
        o = bindingSet[2]
        print "%s %s %s" % (s, p, o)

    print "\nSPARQL matches for 42.0 (direct match)."
    queryString = """SELECT ?s ?p WHERE {?s ?p 42.0 .}"""
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate();    
    for bindingSet in result:
        s = bindingSet[0]
        p = bindingSet[1]
        print "%s %s" % (s, p)
    print "----------------------------------------------------------------------------"

    # Matches against ints. 
    print "\ngetStatements() request for fortyTwoInt: %s" % (fortyTwoInt)
    statements = conn.getStatements(None, age, fortyTwoInt)
#    statements = conn.getStatements(None, age, "42"^^<http://www.w3.org/2001/XMLSchema#int>)
    for s in statements:
        print s

    print "\nSPARQL matches for \"42\"^^<http://www.w3.org/2001/XMLSchema#int> (filter match) finds multple types."
    queryString = """SELECT ?s ?p ?o WHERE {?s ?p ?o . filter (?o = "42"^^<http://www.w3.org/2001/XMLSchema#int>)}"""
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate();    
    for bindingSet in result:
        s = bindingSet[0]
        p = bindingSet[1]
        o = bindingSet[2]
        print "%s %s %s" % (s, p, o)

    print "\nSPARQL matches for \"42\"^^<http://www.w3.org/2001/XMLSchema#int> (direct match) finds ints."
    queryString = """SELECT ?s ?p WHERE {?s ?p "42"^^<http://www.w3.org/2001/XMLSchema#int>}"""
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate();    
    for bindingSet in result:
        s = bindingSet[0]
        p = bindingSet[1]
        print "%s %s" % (s, p)

    print "----------------------------------------------------------------------------"
    # Matches against longs. 
    print "\ngetStatements() request for FortyTwoLong: %s" % (fortyTwoLong)
    statements = conn.getStatements(None, age, fortyTwoLong)
    for s in statements:
        print s

    print "\nSPARQL matches for \"42\"^^<http://www.w3.org/2001/XMLSchema#long> (filter match) finds multiple types."
    queryString = """SELECT ?s ?p ?o WHERE {?s ?p ?o . filter (?o = "42"^^<http://www.w3.org/2001/XMLSchema#long>)}"""
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate();    
    for bindingSet in result:
        s = bindingSet[0]
        p = bindingSet[1]
        o = bindingSet[2]
        print "%s %s %s" % (s, p, o)

    print "\nSPARQL matches for \"42\"^^<http://www.w3.org/2001/XMLSchema#long> (direct match) finds longs."
    queryString = """SELECT ?s ?p WHERE {?s ?p "42"^^<http://www.w3.org/2001/XMLSchema#long>}"""
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate();    
    for bindingSet in result:
        s = bindingSet[0]
        p = bindingSet[1]
        print "%s %s" % (s, p)

    print "----------------------------------------------------------------------------"
    # Matches against doubles. 
    print "\ngetStatements() request for fortyTwoDouble: %s finds a double." % (fortyTwoDouble)
    statements = conn.getStatements(None, age, fortyTwoDouble)
    for s in statements:
        print s

    print "\nSPARQL matches for \"42\"^^<http://www.w3.org/2001/XMLSchema#double> (filter match) finds multiple types."
    queryString = """SELECT ?s ?p ?o WHERE {?s ?p ?o . filter (?o = "42"^^<http://www.w3.org/2001/XMLSchema#double>)}"""
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate();    
    for bindingSet in result:
        s = bindingSet[0]
        p = bindingSet[1]
        o = bindingSet[2]
        print "%s %s %s" % (s, p, o)

    print "\nSPARQL matches for \"42\"^^<http://www.w3.org/2001/XMLSchema#double> (direct match) finds a double."
    queryString = """SELECT ?s ?p WHERE {?s ?p "42"^^<http://www.w3.org/2001/XMLSchema#double>}"""
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate();    
    for bindingSet in result:
        s = bindingSet[0]
        p = bindingSet[1]
        print "%s %s" % (s, p)

    print "----------------------------------------------------------------------------"
    # Matches against declared strings. 
    print "\ngetStatements() request for FortyTwoString: %s finds a string." % (fortyTwoString)
    statements = conn.getStatements(None, age, fortyTwoString)
    for s in statements:
        print s

    print "\nSPARQL matches for \"42\"^^<http://www.w3.org/2001/XMLSchema#string> (filter match)."

    queryString = """SELECT ?s ?p ?o WHERE {?s ?p ?o . filter (?o = "42"^^<http://www.w3.org/2001/XMLSchema#string>)}"""
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate();    
    for bindingSet in result:
        s = bindingSet[0]
        p = bindingSet[1]
        o = bindingSet[2]
        print "%s %s %s" % (s, p, o)

    print "\nSPARQL matches for \"42\"^^<http://www.w3.org/2001/XMLSchema#string> (direct match)."
    queryString = """SELECT ?s ?p WHERE {?s ?p "42"^^<http://www.w3.org/2001/XMLSchema#string>}"""
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate();    
    for bindingSet in result:
        s = bindingSet[0]
        p = bindingSet[1]
        print "%s %s" % (s, p)

    print "----------------------------------------------------------------------------"
    # Matches against plain literals.
    print "\ngetStatements() triples that match plain literal \"42\"." 
    statements = conn.getStatements(None, age, "42")
    for s in statements:
        print s

    print "\nSPARQL matches for plain literal \"42\" (filter match) finds a string."
    queryString = """SELECT ?s ?p ?o WHERE {?s ?p ?o . filter (?o = "42")}"""
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate();    
    for bindingSet in result:
        s = bindingSet[0]
        p = bindingSet[1]
        o = bindingSet[2]
        print "%s %s %s" % (s, p, o)

    print "\nSPARQL matches for plain literal \"42\" (direct match) finds a string."
    queryString = """SELECT ?s ?p WHERE {?s ?p "42"}"""
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate();    
    for bindingSet in result:
        s = bindingSet[0]
        p = bindingSet[1]
        print "%s %s" % (s, p)

    print "----------------------------------------------------------------------------"
    # Let's examine some string matching.
    print "\nTests of string matching..."
    favoriteColor = conn.createURI(namespace=exns, localname="favoriteColor")
    UCred = conn.createLiteral('Red', datatype=XMLSchema.STRING)
    LCred = conn.createLiteral('red', datatype=XMLSchema.STRING)
    RedPlain = conn.createLiteral('Red')                  #plain literal
    rouge = conn.createLiteral('rouge', datatype=XMLSchema.STRING)
    Rouge = conn.createLiteral('Rouge', datatype=XMLSchema.STRING)
    RougePlain = conn.createLiteral('Rouge')              #plain literal
    FrRouge = conn.createLiteral('Rouge', language="fr")  #plain literal with language tag
    conn.addTriples([(alice, favoriteColor, UCred),
                     (bob, favoriteColor, LCred),
                     (carol, favoriteColor, RedPlain),
                     (dave, favoriteColor, rouge),
                     (eric, favoriteColor, Rouge),
                     (fred, favoriteColor, RougePlain),
                     (greg, favoriteColor, FrRouge)])
    print "\nShowing all color triples using getStatements(). Should be seven."
    statements = conn.getStatements(None, favoriteColor, None)
    for s in statements:
        print s

    # Explore matching on these strings.
    # Matches against undeclared strings. These are capitalized Red.
    print "\ngetStatements() triples that match \"Red\". Finds exact match. " 
    statements = conn.getStatements(None, favoriteColor, "Red")
    for s in statements:
        print s

    print "\nSPARQL matches for \"Red\" (filter match) find exact match."
    queryString = """SELECT ?s ?p ?o WHERE {?s ?p ?o . filter (?o = "Red")}"""
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate();    
    for bindingSet in result:
        s = bindingSet[0]
        p = bindingSet[1]
        o = bindingSet[2]
        print "%s %s %s" % (s, p, o)

    print "\nSPARQL matches for \"Red\" (direct match) finds exact match."
    queryString = """SELECT ?s ?p WHERE {?s ?p "Red"}"""
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate();    
    for bindingSet in result:
        s = bindingSet[0]
        p = bindingSet[1]
        print "%s %s" % (s, p)

    print "----------------------------------------------------------------------------"
    # Matches against undeclared strings. These are capital Rouge.
    print "\ngetStatements() triples that match \"Rouge\". Finds exact match. " 
    statements = conn.getStatements(None, favoriteColor, "Rouge")
    for s in statements:
        print s

    print "\nSPARQL matches for \"Rouge\" (filter match) find string and plain."
    queryString = """SELECT ?s ?p ?o WHERE {?s ?p ?o . filter (?o = "Rouge")}"""
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate();    
    for bindingSet in result:
        s = bindingSet[0]
        p = bindingSet[1]
        o = bindingSet[2]
        print "%s %s %s" % (s, p, o)

    print "\nSPARQL matches for \"Rouge\" (direct match) finds string and plain."
    queryString = """SELECT ?s ?p WHERE {?s ?p "Rouge"}"""
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate();    
    for bindingSet in result:
        s = bindingSet[0]
        p = bindingSet[1]
        print "%s %s" % (s, p)

    print "----------------------------------------------------------------------------"
    # Matches against undeclared strings. These are capital Rouge in French.
    print "\ngetStatements() triples that match \"Rouge\"@fr. Finds exact match. " 
    statements = conn.getStatements(None, favoriteColor, FrRouge)
    for s in statements:
        print s

    print "\nSPARQL matches for \"Rouge\"@fr (filter match) find exact match."
    queryString = """SELECT ?s ?p ?o WHERE {?s ?p ?o . filter (?o = "Rouge"@fr)}"""
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate();    
    for bindingSet in result:
        s = bindingSet[0]
        p = bindingSet[1]
        o = bindingSet[2]
        print "%s %s %s" % (s, p, o)

    print "\nSPARQL matches for \"Rouge\"@fr (direct match) finds exact match."
    queryString = """SELECT ?s ?p WHERE {?s ?p "Rouge"@fr}"""
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate();    
    for bindingSet in result:
        s = bindingSet[0]
        p = bindingSet[1]
        print "%s %s" % (s, p)
    print "----------------------------------------------------------------------"
    print "\nSPARQL matches for (fn:lower-case(str(?o)) = \"rouge\") (filter match) finds four."
    queryString = """PREFIX fn: <http://www.w3.org/2005/xpath-functions#> SELECT ?s ?p ?o WHERE {?s ?p ?o . filter (fn:lower-case(str(?o)) = "rouge")}"""
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate();    
    for bindingSet in result:
        s = bindingSet[0]
        p = bindingSet[1]
        o = bindingSet[2]
        print "%s %s %s" % (s, p, o)


    print "----------------------------------------------------------------------------"
    # Let's try some Booleans. A person is, or is not, a "senior."
    senior = conn.createURI(namespace=exns, localname="seniorp")    
    # The values True and False are predefined.
    print "True = %s" % True     # These work. 
    print "False = %s" % False
    # print "true = %s" % true   # These don't work. 
    # print "false = %s" % false
    trueValue = conn.createLiteral("true", datatype=XMLSchema.BOOLEAN)
    falseValue = conn.createLiteral("false", datatype=XMLSchema.BOOLEAN)
    conn.addTriple(alice, senior, trueValue)
    conn.addTriple(bob, senior, falseValue)

    # Let's look at the new triples.
    print "\ngetStatements() all senior triples, should be two. " 
    statements = conn.getStatements(None, senior, None)
    for s in statements:
        print s

    # Try matches for Boolean "True".
    print "\ngetStatements() triples that match Boolean True. No matches." 
    statements = conn.getStatements(None, senior, True)     # no matches
    for s in statements:
        print s

    print "----------------------------------------------------------------------------"
    print "\ngetStatements() triples that match trueValue. One match." 
    statements = conn.getStatements(None, senior, trueValue)
    for s in statements:
        print s

    print "\nSPARQL matches for true (filter match). One match."
    queryString = """SELECT ?s ?p ?o WHERE {?s ?p ?o . filter (?o = true)}"""
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate();    
    for bindingSet in result:
        s = bindingSet[0]
        p = bindingSet[1]
        o = bindingSet[2]
        print "%s %s %s" % (s, p, o)

    print "\nSPARQL matches for true (direct match). One match."
    queryString = """SELECT ?s ?p WHERE {?s ?p true}"""
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate();    
    for bindingSet in result:
        s = bindingSet[0]
        p = bindingSet[1]
        print "%s %s" % (s, p)



    print "\nSPARQL matches for \"true\"^^<http://www.w3.org/2001/XMLSchema#boolean> (filter match). One match."
    queryString = """SELECT ?s ?p ?o WHERE {?s ?p ?o . filter (?o = "true"^^<http://www.w3.org/2001/XMLSchema#boolean>)}"""
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate();    
    for bindingSet in result:
        s = bindingSet[0]
        p = bindingSet[1]
        o = bindingSet[2]
        print "%s %s %s" % (s, p, o)

    print "\nSPARQL matches for \"true\"^^<http://www.w3.org/2001/XMLSchema#boolean> (direct match). One match."
    queryString = """SELECT ?s ?p WHERE {?s ?p "true"^^<http://www.w3.org/2001/XMLSchema#boolean>}"""
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate();    
    for bindingSet in result:
        s = bindingSet[0]
        p = bindingSet[1]
        print "%s %s" % (s, p)

    print "-----------------------------------------------------------------------------------"
    # Dates, times, and datetimes
    birthdate = conn.createURI(namespace=exns, localname="birthdate")
    date = conn.createLiteral('1984-12-06', datatype=XMLSchema.DATE)     
    datetime = conn.createLiteral('1984-12-06T09:00:00', datatype=XMLSchema.DATETIME) 
    time = conn.createLiteral('09:00:00Z', datatype=XMLSchema.TIME) 
    datetimeOffset = conn.createLiteral('1984-12-06T09:00:00+01:00', datatype=XMLSchema.DATETIME)
    print "Printing out Literals for date, datetime, time, and datetime with Zulu offset."
    print date
    print datetime
    print time
    print datetimeOffset

    conn.addTriples([(alice, birthdate, date),
                     (bob, birthdate, datetime),
                     (carol, birthdate, time),
                     (dave, birthdate, datetimeOffset)])

    print "\nShowing all birthday triples using getStatements(). Should be four."
    statements = conn.getStatements(None, birthdate, None)
    for s in statements:
        print s

    print "----------------------------------------------------------------------------"
    print "\ngetStatements() triples that match date: %s One match." % (date) 
    statements = conn.getStatements(None, birthdate, date)
    for s in statements:
        print s

    print "\nSPARQL matches for \'1984-12-06\'^^<http://www.w3.org/2001/XMLSchema#date> (filter match) finds one."
    queryString = """SELECT ?s ?p ?o WHERE {?s ?p ?o . filter (?o = '1984-12-06'^^<http://www.w3.org/2001/XMLSchema#date>)}"""
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate();    
    for bindingSet in result:
        s = bindingSet[0]
        p = bindingSet[1]
        o = bindingSet[2]
        print "%s %s %s" % (s, p, o)

    print "\nSPARQL matches for \'1984-12-06\'^^<http://www.w3.org/2001/XMLSchema#date> (direct match) finds one."
    queryString = """SELECT ?s ?p WHERE {?s ?p '1984-12-06'^^<http://www.w3.org/2001/XMLSchema#date> .}"""
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate();    
    for bindingSet in result:
        s = bindingSet[0]
        p = bindingSet[1]
        print "%s %s" % (s, p)

    print "----------------------------------------------------------------------------"
    print "\ngetStatements() triples that match datetime: %s One match." % (datetime) 
    statements = conn.getStatements(None, birthdate, datetime)
    for s in statements:
        print s

    print "\nSPARQL matches for \"1984-12-06T09:00:00Z\"^^<http://www.w3.org/2001/XMLSchema#dateTime> (filter match) finds one."
    queryString = """SELECT ?s ?p ?o WHERE {?s ?p ?o . filter (?o = '1984-12-06T09:00:00Z'^^<http://www.w3.org/2001/XMLSchema#dateTime>)}"""
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate();    
    for bindingSet in result:
        s = bindingSet[0]
        p = bindingSet[1]
        o = bindingSet[2]
        print "%s %s %s" % (s, p, o)

    print "\nSPARQL matches for \'1984-12-06T09:00:00Z\'^^<http://www.w3.org/2001/XMLSchema#dateTime> (direct match) finds one."
    queryString = """SELECT ?s ?p WHERE {?s ?p '1984-12-06T09:00:00Z'^^<http://www.w3.org/2001/XMLSchema#dateTime> .}"""
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate();    
    for bindingSet in result:
        s = bindingSet[0]
        p = bindingSet[1]
        print "%s %s" % (s, p)

    print "----------------------------------------------------------------------------"
    print "\ngetStatements() triples that match time: %s One match." % (time) 
    statements = conn.getStatements(None, birthdate, time)
    for s in statements:
        print s

    print "\nSPARQL matches for \"09:00:00Z\"^^<http://www.w3.org/2001/XMLSchema#time> (filter match) finds one."
    queryString = """SELECT ?s ?p ?o WHERE {?s ?p ?o . filter (?o = "09:00:00Z"^^<http://www.w3.org/2001/XMLSchema#time>)}"""
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate();    
    for bindingSet in result:
        s = bindingSet[0]
        p = bindingSet[1]
        o = bindingSet[2]
        print "%s %s %s" % (s, p, o)

    print "\nSPARQL matches for \"09:00:00Z\"^^<http://www.w3.org/2001/XMLSchema#time (direct match) finds one."
    queryString = """SELECT ?s ?p WHERE {?s ?p "09:00:00Z"^^<http://www.w3.org/2001/XMLSchema#time> .}"""
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate();    
    for bindingSet in result:
        s = bindingSet[0]
        p = bindingSet[1]
        print "%s %s" % (s, p)

    print "----------------------------------------------------------------------------"
    print "\ngetStatements() triples that match datetimeOffset: %s One match." % (datetimeOffset) 
    statements = conn.getStatements(None, birthdate, datetimeOffset)
    for s in statements:
        print s

    print "\nSPARQL matches for \"1984-12-06T09:00:00+01:00\"^^<http://www.w3.org/2001/XMLSchema#dateTime> (filter match) finds one."
    queryString = """SELECT ?s ?p ?o WHERE {?s ?p ?o . filter (?o = "1984-12-06T09:00:00+01:00"^^<http://www.w3.org/2001/XMLSchema#dateTime>)}"""
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate();    
    for bindingSet in result:
        s = bindingSet[0]
        p = bindingSet[1]
        o = bindingSet[2]
        print "%s %s %s" % (s, p, o)

    print "\nSPARQL matches for \"1984-12-06T09:00:00+01:00\"^^<http://www.w3.org/2001/XMLSchema#dateTime> (direct match) finds one."
    queryString = """SELECT ?s ?p WHERE {?s ?p "1984-12-06T09:00:00+01:00"^^<http://www.w3.org/2001/XMLSchema#dateTime> .}"""
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate();    
    for bindingSet in result:
        s = bindingSet[0]
        p = bindingSet[1]
        print "%s %s" % (s, p)

    conn.close();
    myRepository = conn.repository
    myRepository.shutDown()
               

def example6(close=True):
    print "Starting example6()."
    server = AllegroGraphServer(AG_HOST, AG_PORT, AG_USER, AG_PASSWORD)
    catalog = server.openCatalog(AG_CATALOG)  
    myRepository = catalog.getRepository(AG_REPOSITORY, Repository.RENEW)
    myRepository.initialize()
    conn = myRepository.getConnection()
    conn.clear()
    conn.openSession()  # open dedicated session to support Prolog queries in example17/18
    # The following paths are relative to os.getcwd(), the working directory.
    print "Default working directory is '%s'" % (CURRENT_DIRECTORY)
    # If you get a "file not found" error, use os.chdir("your directory path") to 
    # point to the location of the data files. For AG Free Edition on Windows:
    #os.chdir("C:\Program Files\AllegroGraphFJE32\python")
    print "Current working directory is '%s'" % (os.getcwd())
    path1 = os.path.join(BASE_DIR, "vcards.rdf")    
    path2 = os.path.join(BASE_DIR, "kennedy.ntriples" )               
    context = conn.createURI("http://example.org#vcards")
    conn.setNamespace("vcd", "http://www.w3.org/2001/vcard-rdf/3.0#");
    ## read kennedy triples into the null context:
    print "Load kennedy.ntriples."
    conn.add(path2, base=None, format=RDFFormat.NTRIPLES, contexts=None)
    ## read vcards triples into the context 'context':
    print "Load vcards triples."
    conn.addFile(path1, None, format=RDFFormat.RDFXML, context=context);
    print "After loading, repository contains %i vcard triples in context '%s'\n    and   %i kennedy triples in context '%s'." % (
           conn.size(context), context, conn.size('null'), 'null')
    if close:
        conn.closeSession()
    else:
        return conn # to chain to other examples
        
def example7():    
    conn = example6(False)
    print "Starting example7()."
    print "Match all and print subjects and contexts"
    result = conn.getStatements(None, None, None, None, limit=25)
    for row in result: print row.getSubject(), row.getContext()
    print "\nSame thing with SPARQL query (can't retrieve triples in the null context)"
    queryString = "SELECT DISTINCT ?s ?c WHERE {graph ?c {?s ?p ?o .} }"
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate();
    for i, bindingSet in enumerate(result):
        print bindingSet[0], bindingSet[1]
    conn.closeSession()
    conn.close();
    myRepository = conn.repository
    myRepository.shutDown()


def example8():
    conn = example6(False)
    print "Starting example8()."
    context = conn.createURI("http://example.org#vcards")
    f, outputFile = tempfile.mkstemp('.nt')
    os.close(f)
    # outputFile = None
    if outputFile == None:
        print "Writing RDF to Standard Out instead of to a file"
    ntriplesWriter = NTriplesWriter(outputFile)
    conn.export(ntriplesWriter, context)   # Export vcards to .nt file.
    f, outputFile2 = tempfile.mkstemp('.rdf')
    os.close(f)
    # outputFile2 = None
    if outputFile2 == None:
        print "Writing NTriples to Standard Out instead of to a file"
    rdfxmlfWriter = RDFXMLWriter(outputFile2)    
    conn.export(rdfxmlfWriter, 'null')  # Export kennedy triples to RDF/XML file
    print "Exporting only the Family Name triples from vcards..."
    familyName = conn.createURI("http://www.w3.org/2001/vcard-rdf/3.0#FN")
    conn.exportStatements(None, familyName, None, False, RDFXMLWriter(None), context)
    conn.closeSession()
    conn.close()
    myRepository = conn.repository
    myRepository.shutDown()
    os.remove(outputFile)
    os.remove(outputFile2)

def example9():
    print "Starting example9()."
    conn = example6(False)
    conn.exportStatements(None, RDF.TYPE, None, False, RDFXMLWriter(None))
    conn.closeSession()
    print "About to close session in example 9:"
    conn.close();
    myRepository = conn.repository
    myRepository.shutDown()

def example10():
    """
    Datasets and multiple contexts
    """
    print "Starting example10()."
    conn = example1()
    ## Create URIs for resources, predicates and classes.
    exns = "http://example.org/people/"
    alice = conn.createURI(namespace=exns, localname="alice")
    bob = conn.createURI(namespace=exns, localname="bob")
    ted = conn.createURI(namespace=exns, localname="ted")
    person = conn.createURI(namespace=exns, localname="Person")
    name = conn.createURI(namespace=exns, localname="name")  
    ## Create literal name values.  
    alicesName = conn.createLiteral("Alice")    
    bobsName = conn.createLiteral("Bob")
    tedsName = conn.createLiteral("Ted")    
    ## Create URIs to identify the named contexts. 
    context1 = conn.createURI(namespace=exns, localname="context1")      
    context2 = conn.createURI(namespace=exns, localname="context2")  
    ## Assemble new statements and add them to the contexts.        
    conn.add(alice, RDF.TYPE, person, context1)
    conn.add(alice, name, alicesName, context1)
    conn.add(bob, RDF.TYPE, person, context2)
    conn.add(bob, name, bobsName, context2)
    conn.add(ted, RDF.TYPE, person)   ## Added to null context
    conn.add(ted, name, tedsName)     ## Added to null context
    ## GetStatements() examples.
    print "---------------------------------------------------------------"
    statements = conn.getStatements(None, None, None)
    print "All triples in all contexts: %s" % (conn.size())   
    for s in statements:
        print s
    print "---------------------------------------------------------------"
    statements = conn.getStatements(None, None, None, ['null'])
    print "All triples in null context: %s" % (conn.size(['null']))   
    for s in statements:
        print s
    print "---------------------------------------------------------------"
    statements = conn.getStatements(None, None, None, [context1, context2])
    print "Triples in contexts 1 or 2: %s" % (conn.size([context1, context2]))
    for s in statements:
        print s
    print "---------------------------------------------------------------"
    statements = conn.getStatements(None, None, None, ['null', context2])
    print "Triples in contexts null or 2: %s" % (conn.size(['null', context2]))
    for s in statements:
        print s
    ## SPARQL examples, some using FROM and FROM NAMED
    print "---------------------------------------------------------------"
    queryString = """
    SELECT ?s ?p ?o WHERE {?s ?p ?o . } 
    """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate(); 
    print "No dataset restrictions."
    for bindingSet in result:
        print bindingSet.getRow()
    print "---------------------------------------------------------------"
    queryString = """
    SELECT ?s ?p ?o ?c WHERE {GRAPH ?c {?s ?p ?o . }} 
    """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate()
    print "No dataset. SPARQL graph query only."
    for bindingSet in result:
        print bindingSet.getRow()
    print "---------------------------------------------------------------"
    queryString = """
    SELECT ?s ?p ?o FROM DEFAULT 
    WHERE {?s ?p ?o . } 
    """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate()
    print queryString
    for bindingSet in result:
        print bindingSet.getRow()
    print "---------------------------------------------------------------"
    queryString = """
    SELECT ?s ?p ?o FROM <http://example.org/people/context1> 
    WHERE {?s ?p ?o . } 
    """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate()
    print queryString
    for bindingSet in result:
        print bindingSet.getRow()
    print "---------------------------------------------------------------"
    queryString = """
    SELECT ?s ?p ?o FROM NAMED <http://example.org/people/context1> 
    WHERE {?s ?p ?o . } 
    """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate()
    print queryString
    for bindingSet in result:
        print bindingSet.getRow()
    print "---------------------------------------------------------------"
    queryString = """
    SELECT ?s ?p ?o ?g FROM NAMED <http://example.org/people/context1> 
    WHERE {GRAPH ?g {?s ?p ?o . }} 
    """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate()
    print queryString
    for bindingSet in result:
        print bindingSet.getRow()

    print "---------------------------------------------------------------"
    queryString = """
    SELECT ?s ?p ?o ?g 
	FROM DEFAULT
	FROM <http://example.org/people/context1>
	FROM NAMED <http://example.org/people/context2>  
    WHERE {{GRAPH ?g {?s ?p ?o . }} UNION {?s ?p ?o .}}
    """
	## This query is the test case for bug19681
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate()
    print queryString
    for bindingSet in result:
        print bindingSet.getRow()

    ## Dataset examples
    print "---------------------------------------------------------------"
    ## testing default graph query:
    queryString = """
    SELECT ?s ?p ?o WHERE {?s ?p ?o . } 
    """
    ds = Dataset()
    ds.addDefaultGraph('null')
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    tupleQuery.setDataset(ds)   
    result = tupleQuery.evaluate(); 
    print "SPARQL query over the null context."
    for bindingSet in result:
        print bindingSet.getRow()
    print "---------------------------------------------------------------"
    ## testing named graph query:
    queryString = """
    SELECT ?s ?p ?o WHERE {?s ?p ?o . } 
    """
    ds = Dataset()
    ds.addNamedGraph(context1)
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    tupleQuery.setDataset(ds)
    result = tupleQuery.evaluate(); 
    print "SPARQL query over context1, no GRAPH pattern."
    for bindingSet in result:
        print bindingSet.getRow()
    print "---------------------------------------------------------------"
    ## testing named graph query:
    queryString = """
    SELECT ?s ?p ?o ?c
    WHERE { GRAPH ?c {?s ?p ?o . } } 
    """
    ds = Dataset()
    ds.addNamedGraph(context1)
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    tupleQuery.setDataset(ds)
    result = tupleQuery.evaluate(); 
    print "SPARQL query over context1, with GRAPH pattern."
    for bindingSet in result:
        print bindingSet.getRow()
    print "---------------------------------------------------------------"
    ## testing named graph query:
    queryString = """
    SELECT ?s ?p ?o ?c
    WHERE { GRAPH ?c {?s ?p ?o . } } 
    """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate(); 
    print "SPARQL query with GRAPH pattern, no context constraints."
    for bindingSet in result:
        print bindingSet.getRow()
    conn.close();
    myRepository = conn.repository
    myRepository.shutDown()
    
def example11():
    """
    Namespaces
    """
    print "Starting example11()."
    conn = example1()
    exns = "http://example.org/people/"
    alice = conn.createURI(namespace=exns, localname="alice")
    person = conn.createURI(namespace=exns, localname="Person")
    conn.add(alice, RDF.TYPE, person)
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
    conn.close();
    myRepository = conn.repository
    myRepository.shutDown()

def example12():
    """
    Text search
    """
    print "Starting example12()."
    conn = example1()
    conn.clear()    
    exns = "http://example.org/people/"
    conn.setNamespace('ex', exns)
    fullname = conn.createURI(namespace=exns, localname="fullname")
    # conn.createFreeTextIndex("index1", predicates=[URI(namespace=exns, localname='fullname')])
    conn.createFreeTextIndex("index1", predicates=[fullname])
    config = conn.getFreeTextIndexConfiguration("index1")
    print(config)
    for item in config["predicates"]:
        print(item)

    alice = conn.createURI(namespace=exns, localname="alice")
    carroll = conn.createURI(namespace=exns, localname="carroll")
    persontype = conn.createURI(namespace=exns, localname="Person")
    # fullname = conn.createURI(namespace=exns, localname="fullname")    
    alicename = conn.createLiteral('Alice B. Toklas')
    book =  conn.createURI(namespace=exns, localname="book1")
    booktype = conn.createURI(namespace=exns, localname="Book")
    booktitle = conn.createURI(namespace=exns, localname="title")
    author = conn.createURI(namespace=exns, localname="author")    
    wonderland = conn.createLiteral('Alice in Wonderland')
    lewisCarroll = conn.createLiteral('Lewis Carroll')
    # Creating Alice B. Toklas resource 
    conn.add(alice, RDF.TYPE, persontype)
    conn.add(alice, fullname, alicename)
    # Creating Alice in Wonderland book resource
    conn.add(book, RDF.TYPE, booktype)    
    conn.add(book, booktitle, wonderland) 
    conn.add(book, author, carroll)
    # Creating Lewis Carrol resource
    conn.add(carroll, fullname, lewisCarroll)
    conn.add(carroll, RDF.TYPE, persontype)
    # getStatements search for all triples
    print "\nCurrent content of triple store:"
    for s in conn.getStatements(None, None, None, None, False): print s 
    # Begin fti:match SPARQL queries
    conn.setNamespace('ex', exns)
    print "\nWhole-word match for 'Alice'"
    queryString = """
    SELECT ?s ?p ?o
    WHERE { ?s ?p ?o . 
            ?s fti:match 'Alice' . }
    """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate(); 
    print "\nFound %i query results" % len(result)      
    for bindingSet in result:
        s = bindingSet[0]
        p = bindingSet[1]
        o = bindingSet[2]
        print "%s %s %s" % (s, p, o)

    print("\nEvalFreeTextSearch() match 'Alice' in index1.")
    for triple in conn.evalFreeTextSearch("Alice", index="index1"):
        print " " + str(triple)

    print "\nWildcard match for 'Ali*'"
    queryString = """
    SELECT ?s ?p ?o
    WHERE { ?s ?p ?o . ?s fti:match 'Ali*' . }
    """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate(); 
    print "\nFound %i query results" % len(result)    
    count = 0
    for bindingSet in result:
        print bindingSet
        count += 1
        if count > 5: break

    print("\nEvalFreeTextSearch() match 'Ali*' in index1.")
    for triple in conn.evalFreeTextSearch("Ali*", index="index1"):
        print " " + str(triple)

    print "Wildcard match for '?l?c?'"
    queryString = """
    SELECT ?s ?p ?o
    WHERE { ?s ?p ?o . ?s fti:match '?l?c?' . }
    """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate(); 
    print "\nFound %i query results" % len(result)    
    count = 0
    for bindingSet in result:
        print bindingSet
        count += 1
        if count > 5: break

    print("\nEvalFreeTextSearch() match '?l?c?' in index1.")
    for triple in conn.evalFreeTextSearch("?l?c?", index="index1"):
        print " " + str(triple)

    print "Substring match for 'lic'"
    queryString = """
    SELECT ?s ?p ?o
    WHERE { ?s ?p ?o . FILTER regex(?o, "lic") }
    """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate(); 
    print "\nFound %i query results" % len(result)    
    count = 0
    for bindingSet in result:
        print bindingSet
        count += 1
        if count > 5: break

    print("\nEvalFreeTextSearch() match 'lic' in index1.")
    for triple in conn.evalFreeTextSearch("lic", index="index1"):
        print " " + str(triple)

    print("\nEvalFreeTextSearch() match '*lic*' in index1.")
    for triple in conn.evalFreeTextSearch("*lic*", index="index1"):
        print " " + str(triple)

    conn.createFreeTextIndex("index2", predicates=[URI(namespace=exns, localname='author')],
                             indexResources="short", indexFields=["object"])
    print("\nMatch 'Carroll' in index2.")
    for triple in conn.evalFreeTextSearch("Carroll", index="index2"):
        print " " + str(triple)

    conn.close();
    myRepository = conn.repository
    myRepository.shutDown()


def example13():
    """
    Select, Ask, Construct, and Describe queries 
    """
    print "Starting example13()."
    conn = example6(False)  # kennedy and vcards data
    conn.setNamespace("kdy", "http://www.franz.com/simple#")
    # We don't want the vcards this time. This is how to delete an entire subgraph.
    context = conn.createURI("http://example.org#vcards")
    conn.remove(None, None, None, context)
    print "\nRemoved V-cards"
    queryString = """select ?s where { ?s rdf:type kdy:person} limit 5"""
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate();
    print "\nSELECT some persons"
    for r in result: print r     
	# ASK query
    queryString = """ask { ?s kdy:first-name "John" } """
    booleanQuery = conn.prepareBooleanQuery(QueryLanguage.SPARQL, queryString)
    result = booleanQuery.evaluate(); 
    print "\nASK: Is there anyone named John?", result
    queryString = """ask { ?s kdy:first-name "Alice" } """
    booleanQuery = conn.prepareBooleanQuery(QueryLanguage.SPARQL, queryString)
    result = booleanQuery.evaluate(); 
    print "\nASK: Is there an Alice?", result
    # CONSTRUCT query
    queryString = """
	construct {?a kdy:has-grandchild ?c} 
	where { ?a kdy:has-child ?b . 
	        ?b kdy:has-child ?c . } 
	        """
    constructQuery = conn.prepareGraphQuery(QueryLanguage.SPARQL, queryString)
    result = constructQuery.evaluate(); 
    print "/nConstruct result, creating new has-grandchild triples:"
    for st in result:
        conn.add(st.getSubject(), st.getPredicate(), st.getObject())
    # DESCRIBE query
    queryString = """select ?s ?o where { ?s kdy:has-grandchild ?o} limit 5"""
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate();
    print "\nShow the has-grandchild triples:"
    for r in result: print r     
    queryString = """describe ?s ?o where { ?s kdy:has-grandchild ?o . } limit 1"""
    describeQuery = conn.prepareGraphQuery(QueryLanguage.SPARQL, queryString)
    result = describeQuery.evaluate(); 
    print "\nDescribe one grandparent and one grandchild:"
    for st in result: print st 
    conn.close();
    myRepository = conn.repository
    myRepository.shutDown()
    
def example14():
    """
    Parametric queries
    """
    print "Starting example14()."
    conn = example2()
    alice = conn.createURI("http://example.org/people/alice")
    bob = conn.createURI("http://example.org/people/bob")
    queryString = """select ?s ?p ?o where { ?s ?p ?o} """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    tupleQuery.setBinding("s", alice)
    result = tupleQuery.evaluate()    
    print "Facts about Alice:"
    for r in result: print r  
    tupleQuery.setBinding("p", RDF.TYPE)
    result = tupleQuery.evaluate()
    print "Alice's RDF type triple:"    
    for r in result: print r  
    conn.close();
    myRepository = conn.repository
    myRepository.shutDown()
    
def example15():
    """
    Range matches
    """
    print "Starting example15()."
    conn = example1()
    conn.clear()
    exns = "http://example.org/people/"
    conn.setNamespace('ex', exns)
    alice = conn.createURI(namespace=exns, localname="alice")
    bob = conn.createURI(namespace=exns, localname="bob")
    carol = conn.createURI(namespace=exns, localname="carol")    
    age = conn.createURI(namespace=exns, localname="age")    
    range = conn.createRange(30, 50)
    # range = conn.createRange(24, 42)  #this setting demonstrates that the limits are inclusive.
    conn.registerDatatypeMapping(predicate=age, nativeType="int")
    conn.add(alice, age, 42)
    conn.add(bob, age, 24) 
    conn.add(carol, age, "39") 
    print "Persons with ages between 30 and 50."
    statements = conn.getStatements(None, age, range)
    for s in statements:
        print s 
    conn.close();
    myRepository = conn.repository
    myRepository.shutDown()

def example16():
    """
    Federated triple stores.
    """
    def pt(kind, rows, expected):
        print "\n%s Apples:  " % kind.capitalize(),
        for r in rows: print r[0].getLocalName(),
    
    server = AllegroGraphServer(AG_HOST, AG_PORT, AG_USER, AG_PASSWORD)
    catalog = server.openCatalog(AG_CATALOG)
    ## create two ordinary stores, and one federated store: 
    redConn = catalog.getRepository("redthings", Repository.RENEW).initialize().getConnection()
    greenConn = catalog.getRepository("greenthings", Repository.RENEW).initialize().getConnection()
    # Can pass strings to name local stores in root catalog, (name,
    # catalog) pairs, URLs, Repository objects, and
    # RepositoryConnection objects
    rainbowThings = server.openFederated([redConn, greenConn], True)

    try:
        print "\nEmpty federation: " + str(rainbowThings.size())

        ex = "http://www.demo.com/example#"
        redConn.setNamespace('ex', ex)
        greenConn.setNamespace('ex', ex)
        rainbowThings.setNamespace('ex', ex)        
        redConn.add(redConn.createURI(ex+"mcintosh"), RDF.TYPE, redConn.createURI(ex+"Apple"))
        redConn.add(redConn.createURI(ex+"reddelicious"), RDF.TYPE, redConn.createURI(ex+"Apple"))    
        greenConn.add(greenConn.createURI(ex+"pippin"), RDF.TYPE, greenConn.createURI(ex+"Apple"))
        greenConn.add(greenConn.createURI(ex+"kermitthefrog"), RDF.TYPE, greenConn.createURI(ex+"Frog"))

        print "\nFederated size: " + str(rainbowThings.size())

        queryString = "select ?s where { ?s rdf:type ex:Apple }"
        ## query each of the stores; observe that the federated one is the union of the other two:
        pt("red", redConn.prepareTupleQuery(QueryLanguage.SPARQL, queryString).evaluate(), 2)
        pt("green", greenConn.prepareTupleQuery(QueryLanguage.SPARQL, queryString).evaluate(), 1)
        pt("federated", rainbowThings.prepareTupleQuery(QueryLanguage.SPARQL, queryString).evaluate(), 3) 

    finally:
        rainbowThings.closeSession()

def example17():
    """
    Prolog queries
    """
    print "Starting example17()."
    conn = example6(False)  # Obtain dedicated connection from example6()
    # end of example6()
    conn.setNamespace("kdy", "http://www.franz.com/simple#")
    rules1 = """
    (<-- (woman ?person) ;; IF
         (q ?person !kdy:sex !kdy:female)
         (q ?person !rdf:type !kdy:person))
    (<-- (man ?person) ;; IF
         (q ?person !kdy:sex !kdy:male)
         (q ?person !rdf:type !kdy:person))"""

    conn.addRules(rules1)

    queryString = """
    (select (?first ?last)
            (man ?person)
            (q ?person !kdy:first-name ?first)
            (q ?person !kdy:last-name ?last)
            )"""
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
    tupleQuery.setIncludeInferred(False)
    result = tupleQuery.evaluate()    
    for bindingSet in result:
        f = bindingSet.getValue("first")
        l = bindingSet.getValue("last")
        print "%s %s" % (f.toPython(), l.toPython())
    conn.closeSession()
    conn.close();
    myRepository = conn.repository
    myRepository.shutDown()

def example18():
    """
    Loading Prolog rules
    """
    print "Starting example18()."
    conn = example6(False)  # loads data and return dedicated session
    conn.setNamespace("kdy", "http://www.franz.com/simple#")
    conn.setNamespace("rltv", "http://www.franz.com/simple#")  
    path = os.path.join(BASE_DIR, "python-rules.txt")
    conn.loadRules(path)
    queryString = """(select (?ufirst ?ulast ?cfirst ?clast)
                             (uncle ?uncle ?child)
                             (name ?uncle ?ufirst ?ulast)
                             (name ?child ?cfirst ?clast))"""
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
    result = tupleQuery.evaluate();     
    for bindingSet in result:
        u1 = bindingSet.getValue("ufirst")
        u2 = bindingSet.getValue("ulast")
        ufull = u1.toPython() + " " + u2.toPython()
        c1 = bindingSet.getValue("cfirst")
        c2 = bindingSet.getValue("clast")
        cfull = c1.toPython() + " " + c2.toPython()
        print "%s is the uncle of %s." % (ufull, cfull)
    conn.closeSession()
    conn.close();
    myRepository = conn.repository
    myRepository.shutDown()
        
def example19():
    ## Examples of RDFS++ inference.  Was originally example 2A.
    conn = example1()
    print "Starting example19()."
    ## Create URIs for Bob and Robert (and kids) 
    robert = conn.createURI("http://example.org/people/robert")
    roberta = conn.createURI("http://example.org/people/roberta")
    bob = conn.createURI("http://example.org/people/bob")
    bobby = conn.createURI("http://example.org/people/bobby")
    ## create name and child predicates, and Person class.
    name = conn.createURI("http://example.org/ontology/name")
    fatherOf = conn.createURI("http://example.org/ontology/fatherOf")
    person = conn.createURI("http://example.org/ontology/Person")
    ## create literal values for names    
    bobsName = conn.createLiteral("Bob")
    bobbysName = conn.createLiteral("Bobby")
    robertsName = conn.createLiteral("Robert")
    robertasName = conn.createLiteral("Roberta")
    ## Bob is the same person as Robert
    conn.add(bob, OWL.SAMEAS, robert)
    ## Robert, Bob, and children are people
    conn.add(robert, RDF.TYPE, person)
    conn.add(roberta, RDF.TYPE, person)
    conn.add(bob, RDF.TYPE, person)
    conn.add(bobby, RDF.TYPE, person)
    ## They all have names.
    conn.add(robert, name, robertsName)
    conn.add(roberta, name, robertasName)
    conn.add(bob, name, bobsName)
    conn.add(bobby, name, bobbysName)
    ## robert has a child
    conn.add(robert, fatherOf, roberta)
    ## bob has a child
    conn.add(bob, fatherOf, bobby)
    ## List the children of Robert, with inference OFF.
    queryString = "SELECT ?child WHERE {?robert ?fatherOf ?child .}"
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    tupleQuery.setIncludeInferred(False) # Turn off inference
    tupleQuery.setBinding("robert", robert)
    tupleQuery.setBinding("fatherOf", fatherOf)
    result = tupleQuery.evaluate()    
    print "Children of Robert, inference OFF:"
    for r in result: print r  
    ## List the children of Robert, with inference ON.
    tupleQuery.setIncludeInferred(True)   # Turn on inference
    result = tupleQuery.evaluate()    
    print "Children of Robert, inference ON:"
    for r in result: print r  
    ## Remove the owl:sameAs link so we can try the next example. 
    conn.remove(bob, OWL.SAMEAS, robert)
    ## Define new predicate, hasFather, as the inverse of fatherOf.
    hasFather = conn.createURI("http://example.org/ontology/hasFather")
    conn.add(hasFather, OWL.INVERSEOF, fatherOf)
    ## Search for people who have fathers, even though there are no hasFather triples.
    ## With inference OFF.
    print "People with fathers, inference OFF"
    for s in conn.getStatements(None, hasFather, None, None, False): print s    
    ## With inference ON. The owl:inverseOf link allows AllegroGraph to 
    ## deduce the inverse links.
    print "People with fathers, inference ON"
    for s in conn.getStatements(None, hasFather, None, None, True): print s  
    ## Remove owl:inverseOf property.
    conn.remove(hasFather, OWL.INVERSEOF, fatherOf)
      ## Next 12 lines were for owl:inverseFunctionalProperty, but that isn't
      ## supported yet in AG.  Commenting them out. 
      ## Add fatherOf link from Robert to Bobby, giving Bobby two fathers. 
      #conn.add(robert, fatherOf, bobby)
      ## Now make fatherOf a 'reverse functional property'
      #conn.add(fatherOf, RDF.TYPE, OWL.INVERSEFUNCTIONALPROPERTY)
      ## Bob has how many children? 
      ## With inference OFF.
      #print "Who is Bob the father of, inference OFF"
      #for s in conn.getStatements(bob, fatherOf, None, None, False): print s    
      ## With inference ON. AllegroGraph knows that Bob and Robert must
      ## be the same person.
      #print "Who is Bob the father of, inference ON"
      #for s in conn.getStatements(bob, fatherOf, None, None, True): print s  
    ## Subproperty example.  We'll make fatherOf an rdfs:subpropertyOf parentOf.
    parentOf = conn.createURI("http://example.org/ontology/parentOf")
    conn.add(fatherOf, RDFS.SUBPROPERTYOF, parentOf)
    ## Now search for inferred parentOf links.
    ## Search for parentOf links, even though there are no parentOf triples.
    ## With inference OFF.
    print "People with parents, inference OFF"
    for s in conn.getStatements(None, parentOf, None, None, False): print s    
    ## With inference ON. The rdfs:subpropertyOf link allows AllegroGraph to 
    ## deduce that fatherOf links imply parentOf links.
    print "People with parents, inference ON"
    for s in conn.getStatements(None, parentOf, None, None, True): print s  
    conn.remove(fatherOf, RDFS.SUBPROPERTYOF, parentOf)
    ## The next example shows rdfs:range and rdfs:domain in action.
    ## We'll create two new rdf:type classes.  Note that classes are capitalized.
    parent = conn.createURI("http://example.org/ontology/Parent")
    child = conn.createURI("http://exmaple.org/ontology/Child")
    ## The following triples say that a fatherOf link points from a parent to a child.
    conn.add(fatherOf, RDFS.DOMAIN, parent)
    conn.add(fatherOf, RDFS.RANGE, child)
    ## Now we can search for rdf:type parent.
    print "Who are the parents?  Inference ON."
    for s in conn.getStatements(None, RDF.TYPE, parent, None, True): print s
    ## And we can search for rdf:type child.
    print "Who are the children?  Inference ON."
    for s in conn.getStatements(None, RDF.TYPE, child, None, True): print s
    conn.close();
    myRepository = conn.repository
    myRepository.shutDown()
    
def example20():
    """
    GeoSpatial Reasoning
    """
    conn = example1();
    conn.clear()
    print "Starting example20()."
    exns = "http://example.org/people/"
    conn.setNamespace('ex', exns)
    alice = conn.createURI(exns, "alice")
    bob = conn.createURI(exns, "bob")
    carol = conn.createURI(exns, "carol")
    print "\nCARTESIAN COORDINATE SYSTEM"
    conn.createRectangularSystem(scale=1, xMax=100, yMax=100)
    location = conn.createURI(exns, "location")
    #conn.registerDatatypeMapping(predicate=location, nativeType="int")   
    #conn.registerDatatypeMapping(predicate=location, nativeType="float")       
    conn.add(alice, location, conn.createCoordinate(30,30))
    conn.add(bob, location, conn.createCoordinate(40, 40))
    conn.add(carol, location, conn.createCoordinate(50, 50)) 
    box1 = conn.createBox(20, 40, 20, 40) 
    #print box1
    print "\nFind people located within box1."
    for r in conn.getStatements(None, location, box1) : print r
    circle1 = conn.createCircle(35, 35, radius=10)  
    #print circle1
    print "\nFind people located within circle1."
    for r in conn.getStatements(None, location, circle1) : print r 
    polygon1 = conn.createPolygon([(10,40), (50,10), (35,40), (50,70)])
    #print polygon1
    print "\nFind people located within polygon1."
    for r in conn.getStatements(None, location, polygon1) : print r
    # now we switch to a LatLong (spherical) coordinate system
    print "\nSPHERICAL COORDINATE SYSTEM"
    #latLongGeoType = conn.createLatLongSystem(scale=5) #, unit='km')
    latLongGeoType = conn.createLatLongSystem(scale=5, unit='degree')
    amsterdam = conn.createURI(exns, "amsterdam")
    london = conn.createURI(exns, "london")
    sanfrancisto = conn.createURI(exns, "sanfrancisco")
    salvador = conn.createURI(exns, "salvador")    
    location = conn.createURI(exns, "geolocation")
    #conn.registerDatatypeMapping(predicate=location, nativeType="float")  
    conn.add(amsterdam, location, conn.createCoordinate(52.366665, 4.883333))
    conn.add(london, location, conn.createCoordinate(51.533333, -0.08333333))
    conn.add(sanfrancisto, location, conn.createCoordinate(37.783333, -122.433334)) 
    conn.add(salvador, location, conn.createCoordinate(13.783333, -88.45))   
    box2 = conn.createBox( 25.0, 50.0, -130.0, -70.0) 
    #print box2
    print "\nLocate entities within box2."
    for r in conn.getStatements(None, location, box2) : print r    
    circle2 = conn.createCircle(19.3994, -99.08, 2000, unit='km')
    #print circle2
    print "\nLocate entities within circle2."
    for r in conn.getStatements(None, location, circle2) : print r
    polygon2 = conn.createPolygon([(51.0, 2.00),(60.0, -5.0),(48.0,-12.5)])
    #print polygon2
    print "\nLocate entities within polygon2."
    for r in conn.getStatements(None, location, polygon2) : print r
    conn.close();
    myRepository = conn.repository
    myRepository.shutDown()


def example21():
    """
    Social Network Analysis Reasoning
    This example is commented out 1/25/2010 relative to rfe9149.  BDC
    """
    print "Starting example21()."
    print "Current working directory is '%s'" % (os.getcwd())

    server = AllegroGraphServer(AG_HOST, AG_PORT, AG_USER, AG_PASSWORD)
    catalog = server.openCatalog(AG_CATALOG)  
    myRepository = catalog.getRepository(AG_REPOSITORY, Repository.RENEW)
    myRepository.initialize()
    conn = myRepository.getConnection()
    conn.openSession() # SNA requires dedicated session.

    path1 = os.path.join(BASE_DIR, "lesmis.rdf")
    print "\nLoad Les Miserables triples."
    conn.addFile(path1, None, format=RDFFormat.RDFXML);
    print "After loading, repository contains %i Les Miserables triples in context '%s'." % (
           conn.size('null'), 'null')

    # Create URIs for relationship predicates.
    lmns = "http://www.franz.com/lesmis#"
    conn.setNamespace('lm', lmns)
    knows = conn.createURI(lmns, "knows")
    barely_knows = conn.createURI(lmns, "barely_knows")
    knows_well = conn.createURI(lmns, "knows_well")

    # Create URIs for some characters.
    valjean = conn.createURI(lmns, "character11")
    bossuet = conn.createURI(lmns, "character64")

    # Create some generators
    #print "\nSNA generators known (should be none): '%s'" % (conn.listSNAGenerators())
    conn.registerSNAGenerator("intimates", subjectOf=None, objectOf=None, 
        undirected=knows_well, generator_query=None)
    conn.registerSNAGenerator("associates", subjectOf=None, objectOf=None, 
        undirected=[knows, knows_well], generator_query=None)
    conn.registerSNAGenerator("everyone", subjectOf=None, objectOf=None, 
        undirected=[knows, knows_well, barely_knows], 
        generator_query=None)
    print "Created three generators."

    # Create neighbor matrix.
    conn.registerNeighborMatrix("matrix1", "intimates", valjean, max_depth=2)
    conn.registerNeighborMatrix("matrix2", "associates", valjean, max_depth=5)
    conn.registerNeighborMatrix("matrix3", "everyone", valjean, max_depth=2)
    print "Created three matrices."

    # Explore Valjean's ego group.
    print "\nValjean's ego group members (using associates)."
    queryString = """
    (select (?member ?name)
      (ego-group-member !lm:character11 1 associates ?member)
      (q ?member !dc:title ?name))
      """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
    result = tupleQuery.evaluate();
    print "Found %i query results" % len(result)      
    for bindingSet in result:
        p = bindingSet.getValue("member")
        n = bindingSet.getValue("name")
        print "%s %s" % (p, n)

    # Valjean's ego group using neighbor matrix.
    print "\nValjean's ego group (using associates matrix)."
    queryString = """
    (select (?member ?name)
      (ego-group-member !lm:character11 1 matrix2 ?member)
      (q ?member !dc:title ?name))
      """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
    result = tupleQuery.evaluate();
    print "Found %i query results" % len(result)      
    for bindingSet in result:
        p = bindingSet.getValue("member")
        n = bindingSet.getValue("name")
        print "%s %s" % (p, n)

    print "\nValjean's ego group in one list depth 1 (using associates)."
    queryString = """
    (select (?member)
      (ego-group !lm:character11 1 associates ?group)
      (member ?member ?group))
      """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
    result = tupleQuery.evaluate();
    print "Found %i query results" % len(result)      
    for bindingSet in result:
        p = bindingSet.getValue("member")
        print "%s" %(p)

	
    print "\nValjean's ego group in one list depth 1 (using associates)."
    queryString = """
    (select (?group)
      (ego-group !lm:character11 1 associates ?group))
      """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
    result = tupleQuery.evaluate();
    print "Found %i query results" % len(result)      
    for bindingSet in result:
        p = bindingSet.getValue("group")
        print "[",
        for item in p:
            print "%s" %(item),
        print "]"

    print "\nValjean's ego group in one list depth 2 (using associates)."
    queryString = """
    (select (?group)
      (ego-group !lm:character11 2 associates ?group))
      """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
    result = tupleQuery.evaluate();
    print "Found %i query results" % len(result)      
    for bindingSet in result:
        p = bindingSet.getValue("group")
        print "[",
        for item in p:
            print "%s" %(item),
        print "]"

    print "\nValjean's ego group in one list depth 3 (using associates)."
    queryString = """
    (select (?group)
      (ego-group !lm:character11 3 associates ?group))
      """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
    result = tupleQuery.evaluate();
    print "Found %i query results" % len(result)      
    for bindingSet in result:
        p = bindingSet.getValue("group")
        print "[",
        for item in p:
            print "%s" %(item),
        print "]"

    print "\nShortest breadth-first path connecting Valjean to Bossuet using intimates."
    queryString = """
    (select (?path)
      (breadth-first-search-path !lm:character11 !lm:character64 intimates 10 ?path))
      """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
    result = tupleQuery.evaluate();
    print "Found %i query results" % len(result)      
    for bindingSet in result:
        p = bindingSet.getValue("path")
        print "[",
        for item in p:
            print "%s" %(item),
        print "]"

    print "\nShortest breadth-first path connecting Valjean to Bossuet using associates."
    queryString = """
    (select (?path)
      (breadth-first-search-path !lm:character11 !lm:character64 associates 10 ?path))
      """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
    result = tupleQuery.evaluate();
    print "Found %i query results" % len(result)      
    for bindingSet in result:
        p = bindingSet.getValue("path")
        print "[",
        for item in p:
            print "%s" %(item),
        print "]"

    print "\nShortest breadth-first path connecting Valjean to Bossuet using everyone."
    queryString = """
    (select (?path)
      (breadth-first-search-path !lm:character11 !lm:character64 everyone 10 ?path))
      """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
    result = tupleQuery.evaluate();
    print "Found %i query results" % len(result)      
    for bindingSet in result:
        p = bindingSet.getValue("path")
        print "[",
        for item in p:
            print "%s" %(item),
        print "]"

    print "\nShortest breadth-first path connecting Valjean to Bossuet? with associates (should be two)."
    queryString = """
    (select (?path)
      (breadth-first-search-paths !lm:character11 !lm:character64 associates ?path))
      """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
    result = tupleQuery.evaluate();
    print "Found %i query results" % len(result)      
    for bindingSet in result:
        p = bindingSet.getValue("path")
        print "[",
        for item in p:
            print "%s" %(item),
        print "]"

    # Note that depth-first-search-paths are not guaranteed to be "the shortest path."
    print "\nReturn depth-first path connecting Valjean to Bossuet with associates (should be one)."
    queryString = """
    (select (?path)
      (depth-first-search-path !lm:character11 !lm:character64 associates 10 ?path))
      """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
    result = tupleQuery.evaluate();
    print "Found %i query results" % len(result)      
    for bindingSet in result:
        p = bindingSet.getValue("path")
        print "[",
        for item in p:
            print "%s" %(item),
        print "]"

    print "\nShortest bidirectional paths connecting Valjean to Bossuet with associates (should be two)."
    queryString = """
    (select (?path)
      (bidirectional-search-paths !lm:character11 !lm:character64 associates ?path))
      """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
    result = tupleQuery.evaluate();
    print "Found %i query results" % len(result)      
    for bindingSet in result:
        p = bindingSet.getValue("path")
        print "[",
        for item in p:
            print "%s" %(item),
        print "]"

    print "\nNodal degree of Valjean (should be seven)."
    queryString = """
    (select ?degree
      (nodal-degree !lm:character11 associates ?degree))
      """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
    result = tupleQuery.evaluate();
    for bindingSet in result:
        p = bindingSet.getValue("degree")
        print "%s" % (p)
        print "%s" % p.toPython()

    print "\nHow many neighbors are around Valjean? (should be 36)."
    queryString = """
    (select ?neighbors
      (nodal-degree !lm:character11 everyone ?neighbors))
      """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
    result = tupleQuery.evaluate();
    for bindingSet in result:
        p = bindingSet.getValue("neighbors")
        print "%s" % (p)
        print "%s" % p.toPython()

    print "\nWho are Valjean's neighbors? (using everyone)."
    queryString = """
    (select (?name)
      (nodal-neighbors !lm:character11 everyone ?member)
      (q ?member !dc:title ?name))
      """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
    result = tupleQuery.evaluate();
    count = 0
    for bindingSet in result:
        count = count + 1
        n = bindingSet.getValue("name")
        print "%s. %s " % (count,n.toPython())

    print "\nGraph density of Valjean's ego group? (using associates)."
    queryString = """
    (select ?density
      (ego-group !lm:character11 1 associates ?group)
      (graph-density ?group associates ?density))
      """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
    result = tupleQuery.evaluate();
    for bindingSet in result:
        p = bindingSet.getValue("density")
        print "%s" % (p)
        print "%s" % (p.toPython())

    print "\nValjean's cliques? Should be two (using associates)."
    queryString = """
    (select (?clique)
      (clique !lm:character11 associates ?clique))
      """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
    result = tupleQuery.evaluate();
    for bindingSet in result:
        p = bindingSet.getValue("clique")
        print "[",
        for item in p:
            print "%s" %(item),
        print "]"

    # Valjean's actor-degree-centrality using a depth of 1.
    print "\nValjean's actor-degree-centrality to his ego group at depth 1 (using associates)."
    queryString = """
    (select (?centrality)
      (ego-group !lm:character11 1 associates ?group)
      (actor-degree-centrality !lm:character11 ?group associates ?centrality))
      """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
    result = tupleQuery.evaluate();
    for bindingSet in result:
        p = bindingSet.getValue("centrality")
        print "%s" % (p)
        print "%s" % (p.toPython())

    # Valjean's actor-degree-centrality using a depth of 2.
    print "\nValjean's actor-degree-centrality to his ego group at depth 2 (using associates)."
    queryString = """
    (select (?centrality)
      (ego-group !lm:character11 2 associates ?group)
      (actor-degree-centrality !lm:character11 ?group associates ?centrality))
      """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
    result = tupleQuery.evaluate();
    for bindingSet in result:
        p = bindingSet.getValue("centrality")
        print "%s" % (p)
        print "%s" % (p.toPython())

    # Valjean's actor-closeness-centrality using a depth of 1.
    print "\nValjean's actor-closeness-centrality to his ego group at depth 1 (using associates)."
    queryString = """
    (select (?centrality)
      (ego-group !lm:character11 1 associates ?group)
      (actor-closeness-centrality !lm:character11 ?group associates ?centrality))
      """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
    result = tupleQuery.evaluate();
    for bindingSet in result:
        p = bindingSet.getValue("centrality")
        print "%s" % (p)
        print "%s" % (p.toPython())

    # Valjean's actor-closeness-centrality using a depth of 2.
    print "\nValjean's actor-closeness-centrality to his ego group at depth 2 (using associates)."
    queryString = """
    (select (?centrality)
      (ego-group !lm:character11 2 associates ?group)
      (actor-closeness-centrality !lm:character11 ?group associates ?centrality))
      """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
    result = tupleQuery.evaluate();
    for bindingSet in result:
        p = bindingSet.getValue("centrality")
        print "%s" % (p)
        print "%s" % (p.toPython())

    # Valjean's actor-betweenness-centrality using a depth of 2.
    print "\nValjean's actor-betweenness-centrality to his ego group at depth 2 (using associates)."
    queryString = """
    (select (?centrality)
      (ego-group !lm:character11 2 associates ?group)
      (actor-betweenness-centrality !lm:character11 ?group associates ?centrality))
      """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
    result = tupleQuery.evaluate();
    for bindingSet in result:
        p = bindingSet.getValue("centrality")
        print "%s" % (p)
        print "%s" % (p.toPython())

    #  "Group centrality measures the cohesion a group relative to
    #  some measure of actor-centrality. `group-degree-centrality measures
    #  group cohesion by finding the maximum actor centrality in the group,
    #  summing the difference between this and each other actor's degree
    #  centrality and then normalizing. It ranges from 0 (when all actors have
    #  equal degree) to 1 (when one actor is connected to every other and no
    #  other actors have connections."
    
    print "\nGroup-degree-centrality of Valjean's ego group at depth 1 (using associates)."
    queryString = """
    (select (?centrality)
      (ego-group !lm:character11 1 associates ?group)
      (group-degree-centrality ?group associates ?centrality))
      """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
    result = tupleQuery.evaluate();
    for bindingSet in result:
        p = bindingSet.getValue("centrality")
        print "Centrality: %s" % (p.toPython())

    print "\nGroup-degree-centrality of Valjean's ego group at depth 2 (using associates)."
    queryString = """
    (select (?centrality)
      (ego-group !lm:character11 2 associates ?group)
      (group-degree-centrality ?group associates ?centrality))
      """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
    result = tupleQuery.evaluate();
    for bindingSet in result:
        p = bindingSet.getValue("centrality")
        print "Centrality: %s" % (p.toPython())

    #  "Group centrality measures the cohesion a group relative to
    #  some measure of actor-centrality. `group-closeness-centrality` is
    #  measured by first finding the actor whose `closeness-centrality`
    #  is maximized and then summing the difference between this maximum
    #  value and the [actor-closeness-centrality][] of all other actors.
    #  This value is then normalized so that it ranges between 0 and 1."
    print "\nGroup-closeness-centrality of Valjean's ego group at depth 1 (using associates)."
    queryString = """
    (select (?centrality)
      (ego-group !lm:character11 1 associates ?group)
      (group-closeness-centrality ?group associates ?centrality))
      """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
    result = tupleQuery.evaluate();
    for bindingSet in result:
        p = bindingSet.getValue("centrality")
        print "Centrality: %s" % (p.toPython())

    print "\nGroup-closeness-centrality of Valjean's ego group at depth 2 (using associates)."
    queryString = """
    (select (?centrality)
      (ego-group !lm:character11 2 associates ?group)
      (group-closeness-centrality ?group associates ?centrality))
      """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
    result = tupleQuery.evaluate();
    for bindingSet in result:
        p = bindingSet.getValue("centrality")
        print "Centrality: %s" % (p.toPython())

    #  "Group centrality measures the cohesion a group relative to
    #  some measure of actor-centrality. `group-betweenness-centrality` is
    #  measured by first finding the actor whose `betweenness-centrality`
    #  is maximized and then summing the difference between this maximum
    #  value and the [actor-betweenness-centrality][] of all other actors.
    #  This value is then normalized so that it ranges between 0 and 1.

    print "\nGroup-betweenness-centrality of Valjean's ego group at depth 1 (using associates)."
    queryString = """
    (select (?centrality)
      (ego-group !lm:character11 1 associates ?group)
      (group-betweenness-centrality ?group associates ?centrality))
      """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
    result = tupleQuery.evaluate();
    for bindingSet in result:
        p = bindingSet.getValue("centrality")
        print "Centrality: %s" % (p.toPython())

    print "\nGroup-betweenness-centrality of Valjean's ego group at depth 2 (using associates)."
    queryString = """
    (select (?centrality)
      (ego-group !lm:character11 2 associates ?group)
      (group-betweenness-centrality ?group associates ?centrality))
      """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
    result = tupleQuery.evaluate();
    for bindingSet in result:
        p = bindingSet.getValue("centrality")
        print "Centrality: %s" % (p.toPython())
		




    conn.closeSession()
    conn.close();
    myRepository = conn.repository
    myRepository.shutDown()

def example22():
    """
    Test of dedicated session Commit/Rollback
    """
    """	
	Create common session and dedicated session.
    """
    server = AllegroGraphServer(AG_HOST, AG_PORT, AG_USER, AG_PASSWORD)
    catalog = server.openCatalog(AG_CATALOG)  
    myRepository = catalog.getRepository(AG_REPOSITORY, Repository.RENEW)
    myRepository.initialize()
    common = myRepository.getConnection()
    dedicated = myRepository.getConnection()
    dedicated.openSession()  # open dedicated session 
    # The following paths are relative to os.getcwd(), the working directory.
    print "Default working directory is '%s'" % (CURRENT_DIRECTORY)
    print "Current working directory is '%s'" % (os.getcwd())
    """
	Load LesMis into common session, Kennedy into dedicated session.
    """
    path1 = os.path.join(BASE_DIR, "kennedy.ntriples")
    path2 = os.path.join(BASE_DIR, "lesmis.rdf")                
    baseURI = "http://example.org/example/local"
    ## read kennedy triples into the dedicated session:
    print "Load 1214 kennedy.ntriples into dedicated session."
    dedicated.add(path1, base=baseURI, format=RDFFormat.NTRIPLES, contexts=None)
    ## read lesmis triples into the common session:
    print "Load 916 lesmis triples into the common session."
    common.addFile(path2, baseURI, format=RDFFormat.RDFXML, context=None);

    print "\nSince we have done neither a commit nor a rollback, queries directed"
    print "to one back end should not be able to retreive triples from the other connection."
    print "\nAfter loading, there are:";
    print "%i kennedy triples in context '%s' of the dedicated session;" % (dedicated.size('null'), 'null');
    print "%i lesmis triples in context '%s' of the common session." % (common.size('null'), 'null');
    print "The answers should be 1214, and 916. "
    """
	Check for partitioning:
		Look for Valjean in common session, should find it.
		Look for Kennedy in common session, should not find it.
		Look for Kennedy in dedicated session, should find it.
		Look for Valjean in dedicated session, should not find it.
    """
    print "\nUsing getStatements() on common session; should find Valjean:"
    Valjean = common.createLiteral("Valjean")
    statements = common.getStatements(None, None, Valjean, None, False, 2)
    print "Number of results: %s" % len(statements)
    for s in statements:
        print s
    print "\nUsing getStatements() on common session; should not find Kennedy:"
    Kennedy = dedicated.createLiteral("Kennedy")
    statements = common.getStatements(None, None, Kennedy,  None, False, 2)
    print "Number of results: %s" % len(statements)
    for s in statements:
        print s
    print "\nUsing getStatements() on dedicated session; should find Kennedys:"
    statements = dedicated.getStatements(None, None, Kennedy,  None, False, 2)
    print "Number of results: %s" % len(statements)
    for s in statements:
        print s
    print "\nUsing getStatements() on dedicated session; should not find Valjean:"
    statements = dedicated.getStatements(None, None, Valjean,  None, False, 2)
    print "Number of results: %s" % len(statements)
    for s in statements:
        print s
    """
	Rollback
	Check for partitioning:
		Look for LesMis in common session, should find it.
		Look for Kennedy in common session, should not find it.
		Look for Kennedy in dedicated session, should not find it.
		Look for LesMis in dedicated session, should find it.
    """
    print "\nRolling back contents of dedicated session."
    dedicated.rollback()
    print "\nUsing getStatements() on common session; should find Valjean:"
    Valjean = common.createLiteral("Valjean")
    statements = common.getStatements(None, None, Valjean,  None, False, 2)
    print "Number of results: %s" % len(statements)
    for s in statements:
        print s
    print "\nUsing getStatements() on common session; should not find Kennedys:"
    Kennedy = dedicated.createLiteral("Kennedy")
    statements = common.getStatements(None, None, Kennedy,  None, False, 2)
    print "Number of results: %s" % len(statements)
    for s in statements:
        print s
    print "\nUsing getStatements() on dedicated session; should not find Kennedys:"
    statements = dedicated.getStatements(None, None, Kennedy,  None, False, 2)
    print "Number of results: %s" % len(statements)
    for s in statements:
        print s
    print "\nUsing getStatements() on dedicated session; should find Valjean:"
    statements = dedicated.getStatements(None, None, Valjean,  None, False, 2)
    print "Number of results: %s" % len(statements)
    for s in statements:
        print s
    """
    Reload the Kennedy data into the dedicated session.
	Commit dedicated session.
	Check for partitioning:
		Look for LesMis in common session, should find it.
		Look for Kennedy in common session, should find it.
		Look for Kennedy in dedicated session, should find it.
		Look for LesMis in dedicated session, should find it.
    """
    ## read kennedy triples into the dedicated session:
    print "\nReload 1214 kennedy.ntriples into dedicated session."
    dedicated.add(path1, base=baseURI, format=RDFFormat.NTRIPLES, contexts=None)
    print "\nCommitting contents of dedicated session."
    dedicated.commit()
    print "\nUsing getStatements() on common session; should find Valjean:"
    Valjean = common.createLiteral("Valjean")
    statements = common.getStatements(None, None, Valjean,  None, False, 2)
    print "Number of results: %s" % len(statements)
    for s in statements:
        print s
    print "\nUsing getStatements() on common session; should find Kennedys:"
    Kennedy = dedicated.createLiteral("Kennedy")
    statements = common.getStatements(None, None, Kennedy,  None, False, 2)
    print "Number of results: %s" % len(statements)
    for s in statements:
        print s
    print "\nUsing getStatements() on dedicated session; should find Kennedys:"
    statements = dedicated.getStatements(None, None, Kennedy,  None, False, 2)
    print "Number of results: %s" % len(statements)
    for s in statements:
        print s
    print "\nUsing getStatements() on dedicated session; should find Valjean:"
    statements = dedicated.getStatements(None, None, Valjean,  None, False, 2)
    print "Number of results: %s" % len(statements)
    for s in statements:
        print s
    dedicated.closeSession()
    dedicated.close()
    common.close()
    repository = dedicated.repository
    repository.shutDown()


def example23():
    """
    Generating duplicate triples and duplicate results
    """
    server = AllegroGraphServer(AG_HOST, AG_PORT, AG_USER, AG_PASSWORD)
    catalog = server.openCatalog(AG_CATALOG)  
    myRepository = catalog.getRepository(AG_REPOSITORY, Repository.RENEW)
    myRepository.initialize()
    conn = myRepository.getConnection()
    # The following paths are relative to os.getcwd(), the working directory.
    print "Default working directory is '%s'" % (CURRENT_DIRECTORY)
    print "Current working directory is '%s'" % (os.getcwd())

    """
    Demonstrate Blank Node Behavior
    """
    path = os.path.join(BASE_DIR, "blankNodes1.rdf")
    baseURI = "http://www.franz.com/simple#"
    print "\nLoad blankNodes1.rdf"
    conn.add(path, base=baseURI, format=RDFFormat.RDFXML, contexts=None)
    statements = conn.getStatements(None, None, None, 'null', limit=10000)
    print "Two books, with one author as blank node in each book."
    print "Number of results: %s" % len(statements)
    for s in statements:
        print s
    conn.remove(None, None, None)

# --------------------------------------------------------------
    path = os.path.join(BASE_DIR, "blankNodes2.rdf")
    baseURI = "http://www.franz.com/simple#"
    print "\nLoad blankNodes2.rdf"
    conn.add(path, base=baseURI, format=RDFFormat.RDFXML, contexts=None)
    statements = conn.getStatements(None, None, None, 'null', limit=10000)
    print "Two books, with one author identified by URI but in striped syntax in each book."
    print "Number of results: %s" % len(statements)
    for s in statements:
        print s
    conn.remove(None, None, None)
	
# Following example is for rfe8610
#    path = os.path.join(BASE_DIR, "blankNodes2.rdf")
#    baseURI = "http://www.franz.com/simple#"
#    conn.openSession()
#    print "\nLoad blankNodes2.rdf"
#    conn.add(path, base=baseURI, format=RDFFormat.RDFXML, contexts=None)
#    conn.commit()
#    statements = conn.getStatements(None, None, None, 'null', limit=10000)
#    conn.closeSession()
#    print "Same as previous example but using a transaction (dedicated session)."
#    print "Number of results: %s" % len(statements)
#    for s in statements:
#        print s
#    print "Try it with SPARQL instead of getStatements()."
#    queryString = """SELECT ?s ?p ?o 
#                     WHERE {?s ?p ?o .}"""
#    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
#    result = tupleQuery.evaluate();    
#    for bindingSet in result:
#        s = bindingSet[0]
#        p = bindingSet[1]
#        o = bindingSet[2]
#        print "%s %s %s" % (s, p, o)	
#    conn.remove(None, None, None)

# --------------------------------------------------------------

    path = os.path.join(BASE_DIR, "blankNodes3.rdf")
    baseURI = "http://www.franz.com/simple#"
    print "\nLoad blankNodes3.rdf"
    conn.add(path, base=baseURI, format=RDFFormat.RDFXML, contexts=None)
    statements = conn.getStatements(None, None, None, 'null', limit=10000)
    print "Two books, with one author linked by a URI."
    print "Number of results: %s" % len(statements)
    for s in statements:
        print s
    conn.remove(None, None, None)
	
    path = os.path.join(BASE_DIR, "blankNodes4.rdf")
    baseURI = "http://www.franz.com/simple#"
    print "\nLoad blankNodes4.rdf"
    conn.add(path, base=baseURI, format=RDFFormat.RDFXML, contexts=None)
    statements = conn.getStatements(None, None, None, 'null', limit=10000)
    print "Two books, with one author as a literal value."
    print "Number of results: %s" % len(statements)
    for s in statements:
        print s
    conn.remove(None, None, None)

    """
    Load Kennedy file 
    """
    path = os.path.join(BASE_DIR, "kennedy.ntriples")
    baseURI = "http://www.franz.com/simple#"
    print "Load 1214 kennedy.ntriples."
    conn.add(path, base=baseURI, format=RDFFormat.NTRIPLES, contexts=None)
    print "\nAfter loading, there are:";
    print "%i kennedy triples in context '%s';" % (conn.size('null'), 'null');
    conn.setNamespace("kdy", "http://www.franz.com/simple#")
    exns = "http://www.franz.com/simple#"
    TedKennedy = conn.createURI(namespace=exns, localname="person17")
    hasChild = conn.createURI("http://www.franz.com/simple#has-child")
    print "\nUsing getStatements() find children of Ted Kennedy: three children."
    statements = conn.getStatements(TedKennedy, hasChild, None, 'null', limit=10000)
    print "Number of results: %s" % len(statements)
    for s in statements:
        print s
    # Write inept query to retrieve two children of Ted Kennedy.
    print "\nSPARQL matches for two children of Ted Kennedy, inept pattern."
    queryString = """SELECT ?o1 ?o2 
                     WHERE {kdy:person17 kdy:has-child ?o1 .
                            kdy:person17 kdy:has-child ?o2 .}"""
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate();    
    for bindingSet in result:
        o1 = bindingSet[0]
        o2 = bindingSet[1]
        print "%s and %s" % (o1, o2)	
		
    print "\nSPARQL matches for two children of Ted Kennedy, better pattern."
    queryString = """SELECT ?o1 ?o2 
                     WHERE {kdy:person17 kdy:has-child ?o1 .
                            kdy:person17 kdy:has-child ?o2 .
                            filter (?o1 < ?o2)}"""
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate();    
    for bindingSet in result:
        o1 = bindingSet[0]
        o2 = bindingSet[1]
        print "%s and %s" % (o1, o2)	

    print "\nProlog SELECT query to parallel the previous SPARQL query."
    queryString = """
    (select (?o1 ?o2)
            (q !kdy:person17 !kdy:has-child ?o1)
            (q !kdy:person17 !kdy:has-child ?o2) 
			(lispp (upi< ?o1 ?o2)))"""
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
    result = tupleQuery.evaluate();     
    for bindingSet in result:
        o1 = bindingSet.getValue("o1")
        o2 = bindingSet.getValue("o2")
        print "%s %s" % (o1, o2)

    print "\nSPARQL matches for two children of Ted Kennedy, even better pattern."
    queryString = """SELECT ?o1 ?o2 
                     WHERE {kdy:person17 kdy:has-child ?o1 .
                            kdy:person17 kdy:has-child ?o2 .
                            filter (?o1 < ?o2)}
                            LIMIT 1"""
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate();    
    for bindingSet in result:
        o1 = bindingSet[0]
        o2 = bindingSet[1]
        print "%s and %s" % (o1, o2)	

    ## load kennedy triples again:
    print "\nReload 1214 kennedy.ntriples."
    conn.add(path, base=baseURI, format=RDFFormat.NTRIPLES, contexts=None)
    print "\nAfter loading, there are:";
    print "%i kennedy triples in context '%s';" % (conn.size('null'), 'null');

    print "\nUsing getStatements(); children of Ted Kennedy: duplicate triples present."
    statements = conn.getStatements(TedKennedy, hasChild, None, 'null', limit=10000)
    print "Number of results: %s" % len(statements)
    for s in statements:
        print s

    print "\nSPARQL matches for children of Ted Kennedy."
    queryString = """SELECT ?o WHERE {kdy:person17 kdy:has-child ?o} ORDER BY ?o"""
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate();    
    for bindingSet in result:
        o = bindingSet[0]
        print "%s" % (o)

    print "\nSPARQL DISTINCT matches for children of Ted Kennedy."
    queryString = """SELECT DISTINCT ?o WHERE {kdy:person17 kdy:has-child ?o} ORDER BY ?o"""
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate();    
    for bindingSet in result:
        o = bindingSet[0]
        print "%s" % (o)

    print "\nSPARQL REDUCED matches for children of Ted Kennedy."
    queryString = """SELECT REDUCED ?o WHERE {kdy:person17 kdy:has-child ?o} ORDER BY ?o"""
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate();    
    for bindingSet in result:
        o = bindingSet[0]
        print "%s" % (o)

    print "\nSPARQL matches for children of Ted Kennedy, limit 2."
    queryString = """SELECT ?o WHERE {kdy:person17 kdy:has-child ?o} LIMIT 2"""
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate();    
    for bindingSet in result:
        o = bindingSet[0]
        print "%s" % (o)

    # Test to see if triple is present before asserting it.
    newParent = conn.createURI(namespace=exns, localname="person100")
    newChild = conn.createURI(namespace=exns, localname="person101")
    print "\nTest before adding triple, first trial: "
    if conn.getStatements(newParent, hasChild, newChild):
        print "Did not add new triple."
    else:
        conn.add(newParent, hasChild, newChild)
        print "Added new triple."
	
    print "\nTest before adding triple, second trial: "
    if conn.getStatements(newParent, hasChild, newChild):
        print "Did not add new triple."
    else:
        conn.add(newParent, hasChild, newChild)
        print "Added new triple."
	
    print "=========================================================================="
    
    conn.closeSession()
    conn.close()
    repository = conn.repository
    repository.shutDown()

def example24():
    """
    Free-text indexing and querying.
    """
    server = AllegroGraphServer(AG_HOST, AG_PORT, AG_USER, AG_PASSWORD)
    catalog = server.openCatalog(AG_CATALOG)  
    myRepository = catalog.getRepository(AG_REPOSITORY, Repository.RENEW)
    myRepository.initialize()
    conn = myRepository.getConnection()

    def ex(name):
        return URI(namespace="http://example.com/", localname=name)
    def lit(text):
        return conn.createLiteral(text)

    # Create a free-text index that indexes only triples whose
    # predicate is ex:description or ex:name.
    conn.createFreeTextIndex("index1", predicates=[ex("description"), ex("name")])
    # Get the names of all free-text indices in this store.
    print "Indices: " + str(conn.listFreeTextIndices())

    # Add some example data
    conn.addTriple(ex("pipe"), ex("name"), lit("steel pipes"))
    conn.addTriple(ex("pipe"), ex("description"), lit("80mm inner diameter, 77mm outer. extra heavy."))
    conn.addTriple(ex("pipe"), ex("price"), lit("11 euro per meter."))
    conn.addTriple(ex("hammer"), ex("name"), lit("hammer"))
    conn.addTriple(ex("hammer"), ex("description"), lit("1500g, black plastic grip, heavy claw hammer."))

    print "Searching for 'heavy' in 'index1':"
    for triple in conn.evalFreeTextSearch("heavy", index="index1"):
        print " " + str(triple)

    # Create an index that indexes all triples, indexing both the
    # subject and the object of the triple, and indexing resources
    # using the 'short' option, meaning only the part after the last
    # slash or hash character gets indexed.
    conn.createFreeTextIndex("index2", indexResources="short", indexFields=["subject", "object"])

    print "Searching for 'pipe' in 'index2':"
    # (When not providing an index= argument, all indices are used.)
    for triple in conn.evalFreeTextSearch("pipe", index="index2"):
        print " " + str(triple)

    # Inspect the index's configuration
    print "Configuration object: " + str(conn.getFreeTextIndexConfiguration("index2"))
    # Delete the indices again
    conn.deleteFreeTextIndex("index1")
    conn.deleteFreeTextIndex("index2")

def main(*args):
    starttime = time.time()
    module = sys.modules[__name__]
    
    if not args or args[0] == "all":
        choices = range(1, 1 + NUMBER_OF_EXAMPLES)
    else:
        choices = args   
    for choice in choices:
        choice = int(choice)
        print "\n==========================================================================="
        print "Example Run Number %d\n" % choice

        if hasattr(module, "example" + str(choice)):
            getattr(module, "example" + str(choice))()
        else:
            print "This example is not available in the current release."
    print "\nElapsed time: %.3f seconds." % (time.time() - starttime)

if __name__ == '__main__':
    main(*sys.argv[1:])
