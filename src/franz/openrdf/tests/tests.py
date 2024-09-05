################################################################################
# Copyright (c) 2006-2017 Franz Inc.
# All rights reserved. This program and the accompanying materials are
# made available under the terms of the MIT License which accompanies
# this distribution, and is available at http://opensource.org/licenses/MIT
################################################################################
import datetime
import io
import locale
import os
import subprocess
import sys
import warnings

import pytest

from franz.openrdf.exceptions import RequestError, ServerException
from franz.openrdf.model import URI, BNode, Literal, Statement, ValueFactory
from franz.openrdf.query.dataset import Dataset
from franz.openrdf.query.query import QueryLanguage
from franz.openrdf.repository.repository import Repository
from franz.openrdf.rio.rdfformat import RDFFormat
from franz.openrdf.rio.rdfwriter import NTriplesWriter
from franz.openrdf.rio.rdfxmlwriter import RDFXMLWriter
from franz.openrdf.sail.allegrographserver import AllegroGraphServer
from franz.openrdf.vocabulary.owl import OWL
from franz.openrdf.vocabulary.rdf import RDF
from franz.openrdf.vocabulary.rdfs import RDFS
from franz.openrdf.vocabulary.xmlschema import XMLSchema

locale.setlocale(locale.LC_ALL, "")


def trace(formatter, values=None, stamp=False):
    prefix = "\ntests [%s]:" % (datetime.datetime.now()) if stamp else "\n"
    if values:
        formatter = locale.format_string(formatter, values, grouping=True)
    print(prefix, formatter)


CURRENT_DIRECTORY = os.path.dirname(__file__)

LOCALHOST = "localhost"
AG_HOST = os.environ.get("AGRAPH_HOST", LOCALHOST)
AG_PORT = int(os.environ.get("AGRAPH_PORT", "10035"))
AG_SSLPORT = int(os.environ.get("AGRAPH_SSL_PORT", "10036"))
AG_PROXY = os.environ.get("AGRAPH_PROXY")
AG_ONSERVER = AG_HOST == LOCALHOST
USER = os.environ.get("AGRAPH_USER", "test")
PASSWORD = os.environ.get("AGRAPH_PASSWORD", "xyzzy")

CATALOG = os.environ.get("AGRAPH_CATALOG", "tests")

# Support "/" as an alias for the root catalog.
if CATALOG == "/":
    CATALOG = None

STORE = "agraph_test"


def teardown_module():
    """Module level teardown function."""
    server = AllegroGraphServer(AG_HOST, AG_PORT, USER, PASSWORD, proxy=AG_PROXY)
    catalog = server.openCatalog(CATALOG)

    for repo in catalog.listRepositories():
        catalog.deleteRepository(repo)


RAISE_EXCEPTION_ON_VERIFY_FAILURE = False


def verify(expressionValue, targetValue, quotedExpression, testNum):
    """
    Verify that 'expressionValue' equals 'targetValue'.  If not,
    raise an exception, or print a message advertising the failure.
    """
    if not expressionValue == targetValue:
        message = (
            "Diagnostic failure in test %s.  Expression '%s' returns '%s' where '%s' expected."
            % (testNum, quotedExpression, expressionValue, targetValue)
        )
        if RAISE_EXCEPTION_ON_VERIFY_FAILURE:
            raise Exception(message)
        else:
            print(
                "BWEEP BWEEP BWEEP BWEEP BWEEP BWEEP BWEEP BWEEP BWEEP BWEEP BWEEP BWEEP BWEEP BWEEP BWEEP \n   ",
                message,
            )


def test0():
    server = AllegroGraphServer(AG_HOST, AG_PORT, USER, PASSWORD, proxy=AG_PROXY)
    print("Available catalogs", server.listCatalogs())


def connect(accessMode=Repository.RENEW):
    """
    Connect is called by the other tests to startup the connection to the test database.
    """
    print("Default working directory is '%s'" % CURRENT_DIRECTORY)
    server = AllegroGraphServer(AG_HOST, AG_PORT, USER, PASSWORD, proxy=AG_PROXY)
    print("Available catalogs", server.listCatalogs())
    catalog = server.openCatalog(CATALOG)
    stores = catalog.listRepositories()
    print(
        "Available repositories in catalog '%s':  %s"
        % (catalog.getName(), catalog.listRepositories())
    )

    # Instead of renewing the database, clear it.
    if accessMode == Repository.RENEW:
        mode = Repository.CREATE if STORE not in stores else Repository.OPEN
    else:
        mode = accessMode

    myRepository = catalog.getRepository(STORE, mode)
    myRepository.initialize()
    connection = myRepository.getConnection()
    connection.disableDuplicateSuppression()

    if accessMode == Repository.RENEW:
        connection.clear()
        connection.clearNamespaces()

    print(
        "Repository %s is up!  It contains %i statements."
        % (myRepository.getDatabaseName(), connection.size())
    )
    return connection


def test1(accessMode=Repository.RENEW):
    """
    Tests getting the repository up.
    """
    return connect(accessMode)


def test2():
    conn = connect()
    ## create some resources and literals to make statements out of
    alice = conn.createURI("http://example.org/people/alice")
    bob = conn.createURI("http://example.org/people/bob")
    # bob = conn.createBNode()
    name = conn.createURI("http://example.org/ontology/name")
    person = conn.createURI("http://example.org/ontology/Person")
    bobsName = conn.createLiteral("Bob")
    alicesName = conn.createLiteral("Alice")
    print("Triple count before inserts: ", conn.size())
    for s in conn.getStatements(None, None, None, None):
        print(s)
    ## alice is a person
    conn.add(alice, RDF.TYPE, person)
    ## alice's name is "Alice"
    conn.add(alice, name, alicesName)
    ## bob is a person
    conn.add(bob, RDF.TYPE, person)
    ## bob's name is "Bob":
    conn.add(bob, name, bobsName)
    print("Triple count: ", conn.size())
    verify(conn.size(), 4, "conn.size()", 2)
    for s in conn.getStatements(None, None, None, None):
        print(s)
    conn.remove(bob, name, bobsName)
    print("Triple count: ", conn.size())
    verify(conn.size(), 3, "conn.size()", 2)
    conn.add(bob, name, bobsName)
    return conn


def test3():
    conn = test2()
    try:
        queryString = "SELECT ?s ?p ?o  WHERE {?s ?p ?o .}"
        tupleQuery = conn.prepareTupleQuery("SPARQL", queryString)
        result = tupleQuery.evaluate()
        verify(result.rowCount(), 4, "len(result)", 3)
        try:
            for bindingSet in result:
                s = bindingSet.getValue("s")
                p = bindingSet.getValue("p")
                o = bindingSet.getValue("o")
                print("%s %s %s" % (s, p, o))
        finally:
            result.close()
    finally:
        conn.close()


def test4():
    conn = test2()
    alice = conn.createURI("http://example.org/people/alice")
    #    statements = conn.getStatements(alice, None, None, tripleIDs=True)
    print("Searching for Alice using getStatements():")
    statements = conn.getStatements(alice, None, None)
    statements.enableDuplicateFilter()  ## there are no duplicates, but this exercises the code that checks
    verify(statements.rowCount(), 2, "statements.rowCount()", 3)
    for s in statements:
        print(s)
    statements.close()


def test5():
    """
    Typed Literals
    """
    conn = connect()
    exns = "http://example.org/people/"
    alice = conn.createURI("http://example.org/people/alice")
    age = conn.createURI(namespace=exns, localname="age")
    weight = conn.createURI(namespace=exns, localname="weight")
    favoriteColor = conn.createURI(namespace=exns, localname="favoriteColor")
    birthdate = conn.createURI(namespace=exns, localname="birthdate")
    ted = conn.createURI(namespace=exns, localname="Ted")
    red = conn.createLiteral("Red")
    rouge = conn.createLiteral("Rouge", language="fr")
    fortyTwo = conn.createLiteral("42", datatype=XMLSchema.INT)
    fortyTwoInteger = conn.createLiteral("42", datatype=XMLSchema.LONG)
    fortyTwoUntyped = conn.createLiteral("42")
    date = conn.createLiteral("1984-12-06", datatype=XMLSchema.DATE)
    time = conn.createLiteral("1984-12-06T09:00:00", datatype=XMLSchema.DATETIME)
    weightFloat = conn.createLiteral("20.5", datatype=XMLSchema.FLOAT)
    weightUntyped = conn.createLiteral("20.5")
    stmt1 = conn.createStatement(alice, age, fortyTwo)
    stmt2 = conn.createStatement(ted, age, fortyTwoUntyped)
    conn.add(stmt1)
    conn.addStatement(stmt2)
    conn.addTriple(alice, weight, weightUntyped)
    conn.addTriple(ted, weight, weightFloat)
    conn.addTriples(
        [
            (alice, favoriteColor, red),
            (ted, favoriteColor, rouge),
            (alice, birthdate, date),
            (ted, birthdate, time),
        ]
    )
    for obj in [
        None,
        fortyTwo,
        fortyTwoUntyped,
        conn.createLiteral("20.5", datatype=XMLSchema.FLOAT),
        conn.createLiteral("20.5"),
        red,
        rouge,
    ]:
        print("Retrieve triples matching '%s'." % obj)
        statements = conn.getStatements(None, None, obj)
        for s in statements:
            print(s)
    for obj in [
        "42",
        '"42"',
        "20.5",
        '"20.5"',
        '"20.5"^^xsd:float',
        '"Rouge"@fr',
        '"Rouge"',
        '"1984-12-06"^^xsd:date',
    ]:
        print("Query triples matching '%s'." % obj)
        queryString = (
            """PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
        SELECT ?s ?p ?o WHERE {?s ?p ?o . filter (?o = %s)}"""
            % obj
        )
        tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
        result = tupleQuery.evaluate()
        for bindingSet in result:
            s = bindingSet[0]
            p = bindingSet[1]
            o = bindingSet[2]
            print("%s %s %s" % (s, p, o))
    ## Search for date using date object in triple pattern.
    print("Retrieve triples matching DATE object.")
    statements = conn.getStatements(None, None, date)
    for s in statements:
        print(s)
    ## Search for datetime using datetime object in triple pattern.
    print("Retrieve triples matching DATETIME object.")
    statements = conn.getStatements(None, None, time)
    for s in statements:
        print(s)
    ## Search for specific date value.
    print("Match triples having specific DATE value.")
    statements = conn.getStatements(
        None, None, '"1984-12-06"^^<http://www.w3.org/2001/XMLSchema#date>'
    )
    for s in statements:
        print(s)
    ## Search for specific datetime value.
    print("Match triples having specific DATETIME value.")
    statements = conn.getStatements(
        None, None, '"1984-12-06T09:00:00"^^<http://www.w3.org/2001/XMLSchema#dateTime>'
    )
    for s in statements:
        print(s)
    ## Search for triples of type xsd:date using SPARQL query.
    print("Use SPARQL to find triples where the value matches a specific xsd:date.")
    queryString = """SELECT ?s ?p WHERE {?s ?p "1984-12-06"^^<http://www.w3.org/2001/XMLSchema#date> }"""
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate()
    for bindingSet in result:
        s = bindingSet[0]
        p = bindingSet[1]
        print("%s %s" % (s, p))
    ## Search for triples of type xsd:datetime using SPARQL query.
    print("Use SPARQL to find triples where the value matches a specific xsd:dateTime.")
    queryString = """SELECT ?s ?p WHERE {?s ?p "1984-12-06T09:00:00"^^<http://www.w3.org/2001/XMLSchema#dateTime> }"""
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate()
    for bindingSet in result:
        s = bindingSet[0]
        p = bindingSet[1]
        print("%s %s" % (s, p))


def test6(conn=None):
    if conn is None:
        conn = connect()
    else:
        conn.clear()
    print("Starting example test6().")
    # The following paths are relative to os.getcwd(), the working directory.
    print("Default working directory is '%s'" % (CURRENT_DIRECTORY))
    # If you get a "file not found" error, use os.chdir("your directory path") to
    # point to the location of the data files. For AG Free Edition on Windows:
    # os.chdir("C:\Program Files\AllegroGraphFJE32\python")
    print("Current working directory is '%s'" % (os.getcwd()))
    path1 = os.path.join(CURRENT_DIRECTORY, "vc-db-1.rdf")
    path2 = os.path.join(CURRENT_DIRECTORY, "kennedy.ntriples")
    baseURI = "http://example.org/example/local"
    context = conn.createURI("http://example.org#vcards")
    conn.setNamespace("vcd", "http://www.w3.org/2001/vcard-rdf/3.0#")
    ## read kennedy triples into the null context:
    print("Load kennedy.ntriples.")
    # conn.add(path2, base=baseURI, format=RDFFormat.NTRIPLES, contexts=None)
    conn.add(path2, base=baseURI, format=RDFFormat.NTRIPLES)
    ## read vcards triples into the context 'context':
    print("Load vcards triples.")
    conn.addFile(path1, baseURI, format=RDFFormat.RDFXML, context=context)
    print(
        "After loading, repository contains %i vcard triples in context '%s'\n    and   %i kennedy triples in context '%s'."
        % (conn.size(context), context, conn.size("null"), "null")
    )
    verify(conn.size(context), 16, "conn.size(context)", 6)
    verify(conn.size("null"), 1214, "conn.size('null)", 6)
    return conn


def test7():
    conn = test6()
    print("Match all and print subjects and contexts")
    result = conn.getStatements(None, None, None, None, limit=25, tripleIDs=True)
    assert len(result) == 25
    first_ids = set()
    for row in result:
        print(row.getSubject(), row.getContext())
        first_ids.add(row.getTripleID())
    # Test limit/offset
    result = conn.getStatements(
        None, None, None, None, limit=25, offset=25, tripleIDs=True
    )
    assert len(result) == 25
    second_ids = set()
    for row in result:
        second_ids.add(row.getTripleID())
    assert not first_ids.intersection(second_ids)
    print("\nSame thing with SPARQL query (can't retrieve triples in the null context)")
    queryString = "SELECT DISTINCT ?s ?c WHERE {graph ?c {?s ?p ?o .} }"
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate()
    for i, bindingSet in enumerate(result):
        print(bindingSet[0], bindingSet[1])
    conn.close()


def test8():
    conn = test6()
    context = conn.createURI("http://example.org#vcards")
    warnings.simplefilter("ignore")
    try:
        conn.export(NTriplesWriter(None), context)
        conn.export(RDFXMLWriter(None), context)
    finally:
        warnings.resetwarnings()


def test9():
    conn = test6()
    warnings.simplefilter("ignore")
    try:
        conn.exportStatements(None, RDF.TYPE, None, False, RDFXMLWriter(None))
    finally:
        warnings.resetwarnings()


def test10():
    """
    Datasets and multiple contexts
    """
    conn = connect()
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
    verify(statements.rowCount(), 6, "statements.rowCount()", 10)
    print("All triples in all contexts:")
    for s in statements:
        print(s)
    statements = conn.getStatements(None, None, None, [context1, context2])
    verify(statements.rowCount(), 4, "statements.rowCount()", 10)
    print("Triples in contexts 1 or 2:")
    for s in statements:
        print(s)
    statements = conn.getStatements(None, None, None, ["null", context2])
    verify(statements.rowCount(), 4, "statements.rowCount()", 10)
    print("Triples in contexts null or 2:")
    for s in statements:
        print(s)
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
    result = tupleQuery.evaluate()
    verify(result.rowCount(), 4, "result.rowCount()", 10)
    print("Query over contexts 1 and 2.")
    for bindingSet in result:
        print(bindingSet.getRow())
    ## testing default graph query:
    queryString = """
    SELECT ?s ?p ?o
    WHERE {?s ?p ?o . }
    """
    ds = Dataset()
    ds.addDefaultGraph("null")
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    tupleQuery.setDataset(ds)
    result = tupleQuery.evaluate()
    verify(result.rowCount(), 2, "result.rowCount()", 10)
    print("Query over the null context.")
    for bindingSet in result:
        print(bindingSet.getRow())


def test11():
    """
    Namespaces
    """
    conn = connect()
    exns = "http://example.org/people/"
    alice = conn.createURI(namespace=exns, localname="alice")
    person = conn.createURI(namespace=exns, localname="Person")
    conn.add(alice, RDF.TYPE, person)
    conn.setNamespace("ex", exns)
    # conn.removeNamespace('ex')
    queryString = """
    SELECT ?s ?p ?o
    WHERE { ?s ?p ?o . FILTER ((?p = rdf:type) && (?o = ex:Person) ) }
    """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate()
    print()
    for bindingSet in result:
        print(bindingSet[0], bindingSet[1], bindingSet[2])


def test12():
    """
    Text search
    """
    conn = connect()
    exns = "http://example.org/people/"
    conn.setNamespace("ex", exns)
    alice = conn.createURI(namespace=exns, localname="alice1")
    persontype = conn.createURI(namespace=exns, localname="Person")
    fullname = conn.createURI(namespace=exns, localname="fullname")
    alicename = conn.createLiteral("Alice B. Toklas")
    book = conn.createURI(namespace=exns, localname="book1")
    booktype = conn.createURI(namespace=exns, localname="Book")
    booktitle = conn.createURI(namespace=exns, localname="title")
    wonderland = conn.createLiteral("Alice in Wonderland")
    conn.clear()
    conn.add(alice, RDF.TYPE, persontype)
    conn.add(alice, fullname, alicename)
    conn.add(book, RDF.TYPE, booktype)
    conn.add(book, booktitle, wonderland)
    conn.setNamespace("ex", exns)
    conn.createFreeTextIndex("index")
    print("Whole-word match for 'Alice'")
    queryString = """
    SELECT ?s ?p ?o
    WHERE { ?s ?p ?o . ?s fti:match 'Alice' . }
    """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate()
    print("Found %i query results" % len(result))
    count = 0
    for bindingSet in result:
        print(bindingSet)
        count += 1
        if count > 5:
            break
    print("Wildcard match for 'Ali*'")
    queryString = """
    SELECT ?s ?p ?o
    WHERE { ?s ?p ?o . ?s fti:match 'Ali*' . }
    """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate()
    print("Found %i query results" % len(result))
    count = 0
    for bindingSet in result:
        print(bindingSet)
        count += 1
        if count > 5:
            break
    print("Wildcard match for '?l?c?'")
    queryString = """
    SELECT ?s ?p ?o
    WHERE { ?s ?p ?o . ?s fti:match '?l?c?' . }
    """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate()
    print("Found %i query results" % len(result))
    count = 0
    for bindingSet in result:
        print(bindingSet)
        count += 1
        if count > 5:
            break
    print("Substring match for 'lic'")
    queryString = """
    SELECT ?s ?p ?o
    WHERE { ?s ?p ?o . FILTER regex(?o, "lic") }
    """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate()
    print("Found %i query results" % len(result))
    count = 0
    for bindingSet in result:
        print(bindingSet)
        count += 1
        if count > 5:
            break


#    queryString="""
#    SELECT ?s ?p ?o
#    WHERE { ?s ?p ?o . FILTER regex(?o, "Ali") }
#    """
def test13():
    """
    Ask, Construct, and Describe queries
    """
    conn = test2()
    conn.setNamespace("ex", "http://example.org/people/")
    conn.setNamespace("ont", "http://example.org/ontology/")
    queryString = """select ?s ?p ?o where { ?s ?p ?o} """
    tupleQuery = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString)
    result = tupleQuery.evaluate()
    print("SELECT result")
    for r in result:
        print(r)
    queryString = """ask { ?s ont:name "Alice" } """
    booleanQuery = conn.prepareBooleanQuery("SPARQL", queryString)
    result = booleanQuery.evaluate()
    print("Boolean result", result)
    queryString = """construct {?s ?p ?o} where { ?s ?p ?o . filter (?o = "Alice") } """
    constructQuery = conn.prepareGraphQuery(QueryLanguage.SPARQL, queryString)
    result = constructQuery.evaluate()
    for st in result:
        print(
            "Construct result, S P O values in statement:",
            st.getSubject(),
            st.getPredicate(),
            st.getObject(),
        )
    # print "Construct result", [st for st in result]
    queryString = """describe ?s where { ?s ?p ?o . filter (?o = "Alice") } """
    describeQuery = conn.prepareGraphQuery(QueryLanguage.SPARQL, queryString)
    result = describeQuery.evaluate()
    print("Describe result")
    for st in result:
        print(st)


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
    print("Facts about Alice:")
    for r in result:
        print(r)
    tupleQuery.setBinding("s", bob)
    print("Facts about Bob:")
    result = tupleQuery.evaluate()
    for r in result:
        print(r)


def test15():
    """
    Range matches
    """
    conn = connect()
    exns = "http://example.org/people/"
    conn.setNamespace("ex", exns)
    alice = conn.createURI(namespace=exns, localname="alice")
    bob = conn.createURI(namespace=exns, localname="bob")
    carol = conn.createURI(namespace=exns, localname="carol")
    age = conn.createURI(namespace=exns, localname="age")
    age_range = conn.createRange(30, 50)
    # range = conn.createRange(24, 42)  #this setting demonstrates that the limits are inclusive.
    if True:
        conn.registerDatatypeMapping(predicate=age, nativeType=int)
    # True, True: Alice Age 42(int) AND Carol Age 39(int)
    # True, False: Alice Age 42(int) AND Carol Age 39(int)
    # False, True: Alice Age 42(int)
    # False, False: Error
    conn.add(alice, age, 42)
    conn.add(bob, age, 24)
    conn.add(carol, age, "39")
    rows = conn.getStatements(None, age, age_range)
    assert len(rows) > 0
    for r in rows:
        print(r)


def test16():
    """
    Federated triple stores.
    """

    def pt(kind, rows, expected):
        print("\n%s Apples:\t" % kind.capitalize(), end=" ")
        for r in rows:
            print(r[0].getLocalName(), end=" ")
        assert len(rows) == expected

    server = AllegroGraphServer(AG_HOST, AG_PORT, USER, PASSWORD, proxy=AG_PROXY)
    catalog = server.openCatalog(CATALOG)
    redConn = (
        catalog.getRepository("redthings", Repository.RENEW)
        .initialize()
        .getConnection()
    )
    greenConn = (
        catalog.getRepository("greenthings", Repository.RENEW)
        .initialize()
        .getConnection()
    )
    rainbowConn = server.openFederated([redConn, greenConn], True)

    ## Verify that the returned connection knows that it is backed by
    ## a session which must be closed when the connection is closed.
    assert rainbowConn.is_session_active == True

    try:
        ## add a few triples to the red and green stores:
        ex = "http://www.demo.com/example#"
        redConn.setNamespace("ex", ex)
        greenConn.setNamespace("ex", ex)
        rainbowConn.setNamespace("ex", ex)
        redConn.add(
            redConn.createURI(ex + "mcintosh"),
            RDF.TYPE,
            redConn.createURI(ex + "Apple"),
        )
        redConn.add(
            redConn.createURI(ex + "reddelicious"),
            RDF.TYPE,
            redConn.createURI(ex + "Apple"),
        )
        greenConn.add(
            greenConn.createURI(ex + "pippin"),
            RDF.TYPE,
            greenConn.createURI(ex + "Apple"),
        )
        greenConn.add(
            greenConn.createURI(ex + "kermitthefrog"),
            RDF.TYPE,
            greenConn.createURI(ex + "Frog"),
        )
        queryString = "select ?s where { ?s rdf:type ex:Apple }"
        ## query each of the stores; observe that the federated one is the union of the other two:
        pt(
            "red",
            redConn.prepareTupleQuery(QueryLanguage.SPARQL, queryString).evaluate(),
            2,
        )
        pt(
            "green",
            greenConn.prepareTupleQuery(QueryLanguage.SPARQL, queryString).evaluate(),
            1,
        )
        pt(
            "federated",
            rainbowConn.prepareTupleQuery(QueryLanguage.SPARQL, queryString).evaluate(),
            3,
        )
    finally:
        rainbowConn.close()


def test16a():
    """
    Attempt to federate server.openSession()'s which isn't allowed
    """
    server = AllegroGraphServer(AG_HOST, AG_PORT, USER, PASSWORD, proxy=AG_PROXY)
    # open in the root catalog so that we can reference the repo by just
    # its name in openSession()
    catalog = server.openCatalog()

    dbname1 = "test16asess1"
    dbname2 = "test16asess2"

    catalog.createRepository(dbname1)
    catalog.createRepository(dbname2)

    with server.openSession(dbname1) as sess1:
        with server.openSession(dbname2) as sess2:
            message = "did not fail"

            try:
                server.openFederated([sess1, sess2], True)
            except ServerException as e:
                message = e.__str__()

    catalog.deleteRepository(dbname1)
    catalog.deleteRepository(dbname2)

    assert (
        "is not a RepositoryConnection created by Repository.getConnection()" in message
    )


def kennedy_male_names(conn=None):
    conn = test6(conn)
    conn.setNamespace("kdy", "http://www.franz.com/simple#")
    rules1 = """
    (<-- (woman ?person) ;; IF
         (q ?person !kdy:sex !kdy:female)
         (q ?person !rdf:type !kdy:person))
    (<-- (man ?person) ;; IF
         (q ?person !kdy:sex !kdy:male)
         (q ?person !rdf:type !kdy:person))"""
    conn.addRules(rules1)
    queryString2 = """
    (select (?first ?last)
            (man ?person)
            (q ?person !kdy:first-name ?first)
            (q ?person !kdy:last-name ?last)
            )"""
    tupleQuery2 = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString2)
    return tupleQuery2.evaluate()


def test17():
    """
    Prolog queries
    """
    with connect().session() as conn:
        result = kennedy_male_names(conn)
        for bindingSet in result:
            f = bindingSet.getValue("first")
            l = bindingSet.getValue("last")
            print("%s %s" % (f, l))


def test18():
    """
    Loading Prolog rules
    """
    #    def pq(queryString):
    #        tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
    #        result = tupleQuery.evaluate();
    #        for row in result:
    #            print row
    with connect().session() as conn:
        test6(conn)
        conn.setNamespace("kdy", "http://www.franz.com/simple#")
        conn.setNamespace("rltv", "http://www.franz.com/simple#")
        path = os.path.join(CURRENT_DIRECTORY, "relative_rules.txt")
        conn.loadRules(path)
        #    pq("""(select ?x (string-concat ?x "a" "b" "c"))""")
        #    pq("""(select (?person ?uncle) (uncle ?y ?x)(name ?x ?person)(name ?y ?uncle))""")
        queryString = """(select (?person ?uncle) (uncle ?y ?x)(name ?x ?person)(name ?y ?uncle))"""
        tupleQuery = conn.prepareTupleQuery("PROLOG", queryString)
        result = tupleQuery.evaluate()
        for bindingSet in result:
            p = bindingSet.getValue("person")
            u = bindingSet.getValue("uncle")
            print("%s is the uncle of %s." % (u, p))


def test19():
    ## Examples of RDFS++ inference.  Was originally example 2A.
    conn = connect()
    print("Beginning test19()...")
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
    print("Children of Robert, inference OFF")
    for s in conn.getStatements(robert, fatherOf, None, None):
        print(s)
    ## List the children of Robert with inference ON. The owl:sameAs
    ## link combines the children of Bob with those of Robert.
    print("Children of Robert, inference ON")
    for s in conn.getStatements(robert, fatherOf, None, None, True):
        print(s)
    ## Remove the owl:sameAs link so we can try the next example.
    conn.remove(bob, OWL.SAMEAS, robert)
    ## Define new predicate, hasFather, as the inverse of fatherOf.
    hasFather = conn.createURI("http://example.org/ontology/hasFather")
    conn.add(hasFather, OWL.INVERSEOF, fatherOf)
    ## Search for people who have fathers, even though there are no hasFather triples.
    ## With inference OFF.
    print("People with fathers, inference OFF")
    for s in conn.getStatements(None, hasFather, None, None):
        print(s)
    ## With inference ON. The owl:inverseOf link allows AllegroGraph to
    ## deduce the inverse links.
    print("People with fathers, inference ON")
    for s in conn.getStatements(None, hasFather, None, None, True):
        print(s)
    ## Remove owl:inverseOf property.
    conn.remove(hasFather, OWL.INVERSEOF, fatherOf)
    ## Next 12 lines were for owl:inverseFunctionalProperty, but that isn't
    ## supported yet in AG.  Commenting them out.
    ## Add fatherOf link from Robert to Bobby, giving Bobby two fathers.
    # conn.add(robert, fatherOf, bobby)
    ## Now make fatherOf a 'reverse functional property'
    # conn.add(fatherOf, RDF.TYPE, OWL.INVERSEFUNCTIONALPROPERTY)
    ## Bob has how many children?
    ## With inference OFF.
    # print "Who is Bob the father of, inference OFF"
    # for s in conn.getStatements(bob, fatherOf, None, None): print s
    ## With inference ON. AllegroGraph knows that Bob and Robert must
    ## be the same person.
    # print "Who is Bob the father of, inference ON"
    # for s in conn.getStatements(bob, fatherOf, None, None, True): print s
    ## Subproperty example.  We'll make fatherOf an rdfs:subpropertyOf parentOf.
    parentOf = conn.createURI("http://example.org/ontology/parentOf")
    conn.add(fatherOf, RDFS.SUBPROPERTYOF, parentOf)
    ## Now search for inferred parentOf links.
    ## Search for parentOf links, even though there are no parentOf triples.
    ## With inference OFF.
    print("People with parents, inference OFF")
    for s in conn.getStatements(None, parentOf, None, None):
        print(s)
    ## With inference ON. The rdfs:subpropertyOf link allows AllegroGraph to
    ## deduce that fatherOf links imply parentOf links.
    print("People with parents, inference ON")
    for s in conn.getStatements(None, parentOf, None, None, True):
        print(s)
    conn.remove(fatherOf, RDFS.SUBPROPERTYOF, parentOf)
    ## The next example shows rdfs:range and rdfs:domain in action.
    ## We'll create two new rdf:type classes.  Note that classes are capitalized.
    parent = conn.createURI("http://example.org/ontology/Parent")
    child = conn.createURI("http://exmaple.org/ontology/Child")
    ## The following triples say that a fatherOf link points from a parent to a child.
    conn.add(fatherOf, RDFS.DOMAIN, parent)
    conn.add(fatherOf, RDFS.RANGE, child)
    ## Now we can search for rdf:type parent.
    print("Who are the parents?  Inference ON.")
    for s in conn.getStatements(None, RDF.TYPE, parent, None, True):
        print(s)
    ## And we can search for rdf:type child.
    print("Who are the children?  Inference ON.")
    for s in conn.getStatements(None, RDF.TYPE, child, None, True):
        print(s)


def test20():
    """
    GeoSpatial Reasoning
    """
    conn = connect()
    print("Starting example test20().")
    exns = "http://example.org/people/"
    conn.setNamespace("ex", exns)
    alice = conn.createURI(exns, "alice")
    bob = conn.createURI(exns, "bob")
    carol = conn.createURI(exns, "carol")
    conn.createRectangularSystem(scale=1, xMax=100, yMax=100)
    location = conn.createURI(exns, "location")
    conn.add(alice, location, conn.createCoordinate(30, 30))
    conn.add(bob, location, conn.createCoordinate(40, 40))
    conn.add(carol, location, conn.createCoordinate(50, 50))
    box1 = conn.createBox(20, 40, 20, 40)
    print(box1)
    print("Find people located within box1.")
    results = conn.getStatements(None, location, box1)
    assert len(results) == 2
    for r in results:
        print(r)
    # Test limit/offset of same
    results = conn.getStatements(None, location, box1, limit=1, offset=0)
    assert len(results) == 1
    results = conn.getStatements(None, location, box1, limit=1, offset=1)
    assert len(results) == 1
    results = conn.getStatements(None, location, box1, limit=1, offset=2)
    assert len(results) == 0

    circle1 = conn.createCircle(35, 35, radius=10)
    print(circle1)
    print("Find people located within circle1.")
    results = conn.getStatements(None, location, circle1)
    assert len(results) == 2
    for r in results:
        print(r)
    # Test limit/offset of same
    results = conn.getStatements(None, location, circle1, limit=1, offset=0)
    assert len(results) == 1
    results = conn.getStatements(None, location, circle1, limit=1, offset=1)
    assert len(results) == 1
    results = conn.getStatements(None, location, circle1, limit=1, offset=2)
    assert len(results) == 0

    polygon1 = conn.createPolygon([(10, 40), (50, 10), (35, 40), (50, 70)])
    print(polygon1)
    print("Find people located within polygon1.")
    for r in conn.getStatements(None, location, polygon1):
        print(r)
    # now we switch to a LatLong (spherical) coordinate system
    # latLongGeoType = conn.createLatLongSystem(scale=5) #, unit='km')
    latLongGeoType = conn.createLatLongSystem(scale=5, unit="degree")
    amsterdam = conn.createURI(exns, "amsterdam")
    london = conn.createURI(exns, "london")
    sanfrancisto = conn.createURI(exns, "sanfrancisco")
    salvador = conn.createURI(exns, "salvador")
    location = conn.createURI(exns, "geolocation")
    conn.add(amsterdam, location, conn.createCoordinate(52.366665, 4.883333))
    conn.add(london, location, conn.createCoordinate(51.533333, -0.08333333))
    conn.add(sanfrancisto, location, conn.createCoordinate(37.783333, -122.433334))
    conn.add(salvador, location, conn.createCoordinate(13.783333, -88.45))
    box2 = conn.createBox(25.0, 50.0, -130.0, -70.0)
    print(box2)
    print("Locate entities within box2.")
    for r in conn.getStatements(None, location, box2):
        print(r)
    circle2 = conn.createCircle(19.3994, -99.08, 2000, unit="km")
    print(circle2)
    print("Locate entities within circle2.")
    for r in conn.getStatements(None, location, circle2):
        print(r)
    polygon2 = conn.createPolygon([(51.0, 2.00), (60.0, -5.0), (48.0, -12.5)])
    print(polygon2)
    print("Locate entities within polygon2.")
    for r in conn.getStatements(None, location, polygon2):
        print(r)
    # experiments in units for lat/long type
    print("km")
    latLongGeoType1 = conn.createLatLongSystem(scale=5, unit="km")
    print(latLongGeoType1)
    print("degree")
    latLongGeoType2 = conn.createLatLongSystem(scale=5, unit="degree")
    print(latLongGeoType2)
    print("mile")
    latLongGeoType3 = conn.createLatLongSystem(scale=5, unit="miles")
    print(latLongGeoType3)
    print("radian")
    latLongGeoType4 = conn.createLatLongSystem(scale=5, unit="radian")
    print(latLongGeoType4)
    print("megaton")
    latLongGeoType5 = conn.createLatLongSystem(scale=5, unit="megaton")
    print(latLongGeoType5)


def test21():
    """
    Social Network Analysis Reasoning
    """
    with connect().session() as conn:
        print("Starting example test21().")
        print("Current working directory is '%s'" % (os.getcwd()))
        path1 = os.path.join(CURRENT_DIRECTORY, "lesmis.rdf")
        print("Load Les Miserables triples.")
        conn.addFile(path1, None, format=RDFFormat.RDFXML)
        print(
            "After loading, repository contains %i Les Miserables triples in context '%s'."
            % (conn.size("null"), "null")
        )
        genName = "LesMiserables"
        lmns = "http://www.franz.com/lesmis#"
        conn.setNamespace("lm", lmns)
        knows = conn.createURI(lmns, "knows")
        # Create some generators
        conn.registerSNAGenerator(
            "LesMiserables1",
            subjectOf=None,
            objectOf=None,
            undirected=knows.toNTriples(),
            generator_query=None,
        )
        conn.registerSNAGenerator(
            "LesMiserables2",
            subjectOf=None,
            objectOf=None,
            undirected=None,
            generator_query=None,
        )
        valjean = conn.createURI(lmns, "character11")
        conn.registerNeighborMatrix(
            "LM_Matrix1", "LesMiserables1", valjean.toNTriples(), max_depth=2
        )
        conn.registerNeighborMatrix(
            "LM_Matrix2", "LesMiserables1", valjean.toNTriples(), max_depth=2
        )

        # To rebuild, just call registerNeighborMatrix again
        conn.registerNeighborMatrix(
            "LM_Matrix1", "LesMiserables1", valjean.toNTriples(), max_depth=2
        )


def test22():
    """
    More Social Network Analysis Reasoning
    """
    with connect().session() as conn:
        path1 = os.path.join(CURRENT_DIRECTORY, "lesmis.rdf")
        conn.addFile(path1, None, format=RDFFormat.RDFXML)
        print(
            "After loading, repository contains %i Les Miserables triples in context '%s'."
            % (conn.size("null"), "null")
        )

        # Create URIs for relationship predicates.
        lmns = "http://www.franz.com/lesmis#"
        conn.setNamespace("lm", lmns)
        knows = conn.createURI(lmns, "knows")
        barely_knows = conn.createURI(lmns, "barely_knows")
        knows_well = conn.createURI(lmns, "knows_well")

        # Create URIs for some characters.
        valjean = conn.createURI(lmns, "character11")
        bossuet = conn.createURI(lmns, "character64")

        # Create some generators
        # print "\nSNA generators known (should be none): '%s'" % (conn.listSNAGenerators())
        conn.registerSNAGenerator(
            "intimates",
            subjectOf=None,
            objectOf=None,
            undirected=knows_well,
            generator_query=None,
        )
        conn.registerSNAGenerator(
            "associates",
            subjectOf=None,
            objectOf=None,
            undirected=[knows, knows_well],
            generator_query=None,
        )
        conn.registerSNAGenerator(
            "everyone",
            subjectOf=None,
            objectOf=None,
            undirected=[knows, knows_well, barely_knows],
            generator_query=None,
        )
        print("Created three generators.")

        # Create neighbor matrix.
        conn.registerNeighborMatrix("matrix1", "intimates", valjean, max_depth=2)
        conn.registerNeighborMatrix("matrix2", "associates", valjean, max_depth=5)
        conn.registerNeighborMatrix("matrix3", "everyone", valjean, max_depth=2)
        print("Created three matrices.")

        # Explore Valjean's ego group.
        print("\nValjean's ego group members (using associates).")
        queryString = """
        (select (?member ?name)
        (ego-group-member !lm:character11 1 associates ?member)
        (q ?member !dc:title ?name))
        """
        tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
        result = tupleQuery.evaluate()
        print("Found %i query results" % len(result))
        for bindingSet in result:
            p = bindingSet.getValue("member")
            n = bindingSet.getValue("name")
            print("%s %s" % (p, n))

        # Valjean's ego group using neighbor matrix.
        print("\nValjean's ego group (using associates matrix).")
        queryString = """
        (select (?member ?name)
          (ego-group-member !lm:character11 1 matrix2 ?member)
          (q ?member !dc:title ?name))
          """
        tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
        result = tupleQuery.evaluate()
        print("Found %i query results" % len(result))
        for bindingSet in result:
            p = bindingSet.getValue("member")
            n = bindingSet.getValue("name")
            print("%s %s" % (p, n))

        print("\nValjean's ego group in one list depth 1 (using associates).")
        queryString = """
        (select (?member)
          (ego-group !lm:character11 1 associates ?group)
          (member ?member ?group))
          """
        tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
        result = tupleQuery.evaluate()
        print("Found %i query results" % len(result))
        for bindingSet in result:
            p = bindingSet.getValue("member")
            print("%s" % (p))

        print("\nValjean's ego group in one list depth 1 (using associates).")
        queryString = """
        (select (?group)
          (ego-group !lm:character11 1 associates ?group))
          """
        tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
        result = tupleQuery.evaluate()
        print("Found %i query results" % len(result))
        for bindingSet in result:
            p = bindingSet.getValue("group")
            print("[", end=" ")
            for item in p:
                print("%s" % (item), end=" ")
            print("]")

        print("\nValjean's ego group in one list depth 2 (using associates).")
        queryString = """
        (select (?group)
          (ego-group !lm:character11 2 associates ?group))
          """
        tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
        result = tupleQuery.evaluate()
        print("Found %i query results" % len(result))
        for bindingSet in result:
            p = bindingSet.getValue("group")
            print("[", end=" ")
            for item in p:
                print("%s" % (item), end=" ")
            print("]")

        print("\nValjean's ego group in one list depth 3 (using associates).")
        queryString = """
        (select (?group)
          (ego-group !lm:character11 3 associates ?group))
          """
        tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
        result = tupleQuery.evaluate()
        print("Found %i query results" % len(result))
        for bindingSet in result:
            p = bindingSet.getValue("group")
            print("[", end=" ")
            for item in p:
                print("%s" % (item), end=" ")
            print("]")

        print(
            "\nShortest breadth-first path connecting Valjean to Bossuet using intimates."
        )
        queryString = """
        (select (?path)
          (breadth-first-search-path !lm:character11 !lm:character64 intimates 10 ?path))
          """
        tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
        result = tupleQuery.evaluate()
        print("Found %i query results" % len(result))
        for bindingSet in result:
            p = bindingSet.getValue("path")
            print("[", end=" ")
            for item in p:
                print("%s" % (item), end=" ")
            print("]")

        print(
            "\nShortest breadth-first path connecting Valjean to Bossuet using associates."
        )
        queryString = """
        (select (?path)
          (breadth-first-search-path !lm:character11 !lm:character64 associates 10 ?path))
          """
        tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
        result = tupleQuery.evaluate()
        print("Found %i query results" % len(result))
        for bindingSet in result:
            p = bindingSet.getValue("path")
            print("[", end=" ")
            for item in p:
                print("%s" % (item), end=" ")
            print("]")

        print(
            "\nShortest breadth-first path connecting Valjean to Bossuet using everyone."
        )
        queryString = """
        (select (?path)
          (breadth-first-search-path !lm:character11 !lm:character64 everyone 10 ?path))
          """
        tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
        result = tupleQuery.evaluate()
        print("Found %i query results" % len(result))
        for bindingSet in result:
            p = bindingSet.getValue("path")
            print("[", end=" ")
            for item in p:
                print("%s" % (item), end=" ")
            print("]")

        print(
            "\nShortest breadth-first path connecting Valjean to Bossuet? with associates (should be two)."
        )
        queryString = """
        (select (?path)
          (breadth-first-search-paths !lm:character11 !lm:character64 associates ?path))
          """
        tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
        result = tupleQuery.evaluate()
        print("Found %i query results" % len(result))
        for bindingSet in result:
            p = bindingSet.getValue("path")
            print("[", end=" ")
            for item in p:
                print("%s" % (item), end=" ")
            print("]")

        # Note that depth-first-search-paths are not guaranteed to be "the shortest path."
        print(
            "\nReturn depth-first path connecting Valjean to Bossuet with associates (should be one)."
        )
        queryString = """
        (select (?path)
          (depth-first-search-path !lm:character11 !lm:character64 associates 10 ?path))
          """
        tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
        result = tupleQuery.evaluate()
        print("Found %i query results" % len(result))
        for bindingSet in result:
            p = bindingSet.getValue("path")
            print("[", end=" ")
            for item in p:
                print("%s" % (item), end=" ")
            print("]")

        print(
            "\nShortest bidirectional paths connecting Valjean to Bossuet with associates (should be two)."
        )
        queryString = """
        (select (?path)
          (bidirectional-search-paths !lm:character11 !lm:character64 associates ?path))
          """
        tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
        result = tupleQuery.evaluate()
        print("Found %i query results" % len(result))
        for bindingSet in result:
            p = bindingSet.getValue("path")
            print("[", end=" ")
            for item in p:
                print("%s" % (item), end=" ")
            print("]")

        print("\nNodal degree of Valjean (should be seven).")
        queryString = """
        (select ?degree
          (nodal-degree !lm:character11 associates ?degree))
          """
        tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
        result = tupleQuery.evaluate()
        for bindingSet in result:
            p = bindingSet.getValue("degree")
            print("%s" % (p))
            print("%s" % p.toPython())

        print("\nHow many neighbors are around Valjean? (should be 36).")
        queryString = """
        (select ?neighbors
          (nodal-degree !lm:character11 everyone ?neighbors))
          """
        tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
        result = tupleQuery.evaluate()
        for bindingSet in result:
            p = bindingSet.getValue("neighbors")
            print("%s" % (p))
            print("%s" % p.toPython())

        print("\nWho are Valjean's neighbors? (using everyone).")
        queryString = """
        (select (?name)
          (nodal-neighbors !lm:character11 everyone ?member)
          (q ?member !dc:title ?name))
          """
        tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
        result = tupleQuery.evaluate()
        count = 0
        for bindingSet in result:
            count = count + 1
            n = bindingSet.getValue("name")
            print("%s. %s " % (count, n.toPython()))

        print("\nGraph density of Valjean's ego group? (using associates).")
        queryString = """
        (select ?density
          (ego-group !lm:character11 1 associates ?group)
          (graph-density ?group associates ?density))
          """
        tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
        result = tupleQuery.evaluate()
        for bindingSet in result:
            p = bindingSet.getValue("density")
            print("%s" % (p))
            print("%s" % (p.toPython()))

        print("\nValjean's cliques? Should be two (using associates).")
        queryString = """
        (select (?clique)
          (clique !lm:character11 associates ?clique))
          """
        tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
        result = tupleQuery.evaluate()
        for bindingSet in result:
            p = bindingSet.getValue("clique")
            print("[", end=" ")
            for item in p:
                print("%s" % (item), end=" ")
            print("]")

        # Valjean's actor-degree-centrality using a depth of 1.
        print(
            "\nValjean's actor-degree-centrality to his ego group at depth 1 (using associates)."
        )
        queryString = """
        (select (?centrality)
          (ego-group !lm:character11 1 associates ?group)
          (actor-degree-centrality !lm:character11 ?group associates ?centrality))
          """
        tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
        result = tupleQuery.evaluate()
        for bindingSet in result:
            p = bindingSet.getValue("centrality")
            print("%s" % (p))
            print("%s" % (p.toPython()))

        # Valjean's actor-degree-centrality using a depth of 2.
        print(
            "\nValjean's actor-degree-centrality to his ego group at depth 2 (using associates)."
        )
        queryString = """
        (select (?centrality)
          (ego-group !lm:character11 2 associates ?group)
          (actor-degree-centrality !lm:character11 ?group associates ?centrality))
          """
        tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
        result = tupleQuery.evaluate()
        for bindingSet in result:
            p = bindingSet.getValue("centrality")
            print("%s" % (p))
            print("%s" % (p.toPython()))

        # Valjean's actor-closeness-centrality using a depth of 1.
        print(
            "\nValjean's actor-closeness-centrality to his ego group at depth 1 (using associates)."
        )
        queryString = """
        (select (?centrality)
          (ego-group !lm:character11 1 associates ?group)
          (actor-closeness-centrality !lm:character11 ?group associates ?centrality))
          """
        tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
        result = tupleQuery.evaluate()
        for bindingSet in result:
            p = bindingSet.getValue("centrality")
            print("%s" % (p))
            print("%s" % (p.toPython()))

        # Valjean's actor-closeness-centrality using a depth of 2.
        print(
            "\nValjean's actor-closeness-centrality to his ego group at depth 2 (using associates)."
        )
        queryString = """
        (select (?centrality)
          (ego-group !lm:character11 2 associates ?group)
          (actor-closeness-centrality !lm:character11 ?group associates ?centrality))
          """
        tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
        result = tupleQuery.evaluate()
        for bindingSet in result:
            p = bindingSet.getValue("centrality")
            print("%s" % (p))
            print("%s" % (p.toPython()))

        # Valjean's actor-betweenness-centrality using a depth of 2.
        print(
            "\nValjean's actor-betweenness-centrality to his ego group at depth 2 (using associates)."
        )
        queryString = """
        (select (?centrality)
          (ego-group !lm:character11 2 associates ?group)
          (actor-betweenness-centrality !lm:character11 ?group associates ?centrality))
          """
        tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
        result = tupleQuery.evaluate()
        for bindingSet in result:
            p = bindingSet.getValue("centrality")
            print("%s" % (p))
            print("%s" % (p.toPython()))

        #  "Group centrality measures the cohesion a group relative to
        #  some measure of actor-centrality. `group-degree-centrality measures
        #  group cohesion by finding the maximum actor centrality in the group,
        #  summing the difference between this and each other actor's degree
        #  centrality and then normalizing. It ranges from 0 (when all actors have
        #  equal degree) to 1 (when one actor is connected to every other and no
        #  other actors have connections."

        print(
            "\nGroup-degree-centrality of Valjean's ego group at depth 1 (using associates)."
        )
        queryString = """
        (select (?centrality)
          (ego-group !lm:character11 1 associates ?group)
          (group-degree-centrality ?group associates ?centrality))
          """
        tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
        result = tupleQuery.evaluate()
        for bindingSet in result:
            p = bindingSet.getValue("centrality")
            print("Centrality: %s" % (p.toPython()))

        print(
            "\nGroup-degree-centrality of Valjean's ego group at depth 2 (using associates)."
        )
        queryString = """
        (select (?centrality)
          (ego-group !lm:character11 2 associates ?group)
          (group-degree-centrality ?group associates ?centrality))
          """
        tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
        result = tupleQuery.evaluate()
        for bindingSet in result:
            p = bindingSet.getValue("centrality")
            print("Centrality: %s" % (p.toPython()))

        #  "Group centrality measures the cohesion a group relative to
        #  some measure of actor-centrality. `group-closeness-centrality` is
        #  measured by first finding the actor whose `closeness-centrality`
        #  is maximized and then summing the difference between this maximum
        #  value and the [actor-closeness-centrality][] of all other actors.
        #  This value is then normalized so that it ranges between 0 and 1."
        print(
            "\nGroup-closeness-centrality of Valjean's ego group at depth 1 (using associates)."
        )
        queryString = """
        (select (?centrality)
          (ego-group !lm:character11 1 associates ?group)
          (group-closeness-centrality ?group associates ?centrality))
          """
        tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
        result = tupleQuery.evaluate()
        for bindingSet in result:
            p = bindingSet.getValue("centrality")
            print("Centrality: %s" % (p.toPython()))

        print(
            "\nGroup-closeness-centrality of Valjean's ego group at depth 2 (using associates)."
        )
        queryString = """
        (select (?centrality)
          (ego-group !lm:character11 2 associates ?group)
          (group-closeness-centrality ?group associates ?centrality))
          """
        tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
        result = tupleQuery.evaluate()
        for bindingSet in result:
            p = bindingSet.getValue("centrality")
            print("Centrality: %s" % (p.toPython()))

        #  "Group centrality measures the cohesion a group relative to
        #  some measure of actor-centrality. `group-betweenness-centrality` is
        #  measured by first finding the actor whose `betweenness-centrality`
        #  is maximized and then summing the difference between this maximum
        #  value and the [actor-betweenness-centrality][] of all other actors.
        #  This value is then normalized so that it ranges between 0 and 1.

        print(
            "\nGroup-betweenness-centrality of Valjean's ego group at depth 1 (using associates)."
        )
        queryString = """
        (select (?centrality)
          (ego-group !lm:character11 1 associates ?group)
          (group-betweenness-centrality ?group associates ?centrality))
          """
        tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
        result = tupleQuery.evaluate()
        for bindingSet in result:
            p = bindingSet.getValue("centrality")
            print("Centrality: %s" % (p.toPython()))

        print(
            "\nGroup-betweenness-centrality of Valjean's ego group at depth 2 (using associates)."
        )
        queryString = """
        (select (?centrality)
          (ego-group !lm:character11 2 associates ?group)
          (group-betweenness-centrality ?group associates ?centrality))
          """
        tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
        result = tupleQuery.evaluate()
        for bindingSet in result:
            p = bindingSet.getValue("centrality")
            print("Centrality: %s" % (p.toPython()))


def testSNA_bug23323():
    """
    Social Network Analysis test for bug23323
    """
    with connect().session() as conn:
        print("Starting example testSNA_bug23323().")
        path1 = os.path.join(CURRENT_DIRECTORY, "lesmis.rdf")
        print("Load Les Miserables triples.")
        conn.addFile(path1, None, format=RDFFormat.RDFXML)
        genName = "LesMiserables"
        lmns = "http://www.franz.com/lesmis#"
        conn.setNamespace("lm", lmns)

        prologQuery = """
        (select (?m)
          (q ?node !lm:barely_knows ?m))
"""
        # Register the generator
        conn.registerSNAGenerator(
            genName,
            subjectOf=None,
            objectOf=None,
            undirected=None,
            generator_query=prologQuery,
        )

        valjean = conn.createURI(lmns, "character11")

        # Create the matrix from the generator
        conn.registerNeighborMatrix("LesMiserablesNM", genName, valjean, max_depth=2)

        print(
            "\nValjean's ego group in one list depth 2 (using barely_knows neighbor matrix)."
        )
        queryString = """
        (select (?group)
          (ego-group !lm:character11 2 LesMiserablesNM ?group))
          """
        tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
        result = tupleQuery.evaluate()
        for bindingSet in result:
            p = bindingSet.getValue("group")
            group_count_nm = len(p)
            print("Group has %i member(s)" % group_count_nm)
            print("Found %i members" % len(p))
            print("[", end=" ")
            for item in p:
                print("%s ; " % (item), end=" ")
            print("]")

        # re-register the generator
        # there is caching involved so if we don't re-register, we'll get the same count
        # regardless of whether or not bug23323 is fixed.
        conn.registerSNAGenerator(
            genName,
            subjectOf=None,
            objectOf=None,
            undirected=None,
            generator_query=prologQuery,
        )

        print("\nValjean's ego group in one list depth 2 (using barely_knows).")
        queryString = """
        (select (?group)
          (ego-group !lm:character11 2 LesMiserables ?group))
          """
        tupleQuery = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString)
        result = tupleQuery.evaluate()
        for bindingSet in result:
            p = bindingSet.getValue("group")
            group_count = len(p)
            print("Found %i members" % group_count)
            print("[", end=" ")
            for item in p:
                print("%s ; " % (item), end=" ")
            print("]")

        print("Make sure we have the same counts")
        assert group_count == group_count_nm


def test_getStatements():
    conn = test6()
    rows = conn.getStatements(None, None, None, tripleIDs=False)

    for row in rows:
        print("%s %s %s %s" % (row[0], row[1], row[2], row[3]))

    rows = conn.getStatements(None, None, None, tripleIDs=True)

    for row in rows:
        print("%s %s %s %s %s" % (row[0], row[1], row[2], row[3], row.getTripleID()))


def test_result_as_list():
    conn = test6()
    rows = conn.getStatements(None, None, None)
    triples = rows.asList()
    assert triples is not None
    assert len(triples) == 1230


def test_session():
    """
    Test of Session Commit/Rollback
    """
    server = AllegroGraphServer(AG_HOST, AG_PORT, USER, PASSWORD, proxy=AG_PROXY)
    catalog = server.openCatalog(CATALOG)
    myRepository = catalog.getRepository(STORE, Repository.OPEN)
    myRepository.initialize()
    common = myRepository.getConnection()
    common.clear()

    with myRepository.getConnection().session() as dedicated:
        path1 = os.path.join(CURRENT_DIRECTORY, "vc-db-1.rdf")
        path2 = os.path.join(CURRENT_DIRECTORY, "kennedy.ntriples")
        path3 = os.path.join(CURRENT_DIRECTORY, "lesmis.rdf")
        baseURI = "http://example.org/example/local"
        context = dedicated.createURI("http://example.org#vcards")
        dedicated.setNamespace("vcd", "http://www.w3.org/2001/vcard-rdf/3.0#")
        ## read vcards triples into the context 'context' of back end:
        # Load 16 vcards triples into named context in dedicated back end.
        dedicated.addFile(path1, baseURI, format=RDFFormat.RDFXML, context=context)
        ## read kennedy triples into the dedicated null context:
        # Load 1214 kennedy.ntriples into null context of dedicated back end.
        dedicated.add(path2, base=baseURI, format=RDFFormat.NTRIPLES, contexts=None)
        ## read lesmis triples into the null context of the common back end:
        # Load 916 lesmis triples into null context of common back end.
        common.addFile(path3, baseURI, format=RDFFormat.RDFXML, context=None)
        assert 16 == dedicated.size(context)
        assert 1214 == dedicated.size("null")
        assert 916 == common.size("null")


def test_temporal():
    """
    Test of temporal range queries.
    """
    server = AllegroGraphServer(AG_HOST, AG_PORT, USER, PASSWORD, proxy=AG_PROXY)
    catalog = server.openCatalog(CATALOG)
    myRepository = catalog.getRepository(STORE, Repository.RENEW)
    myRepository.initialize()
    conn = myRepository.getConnection()
    conn.clear()

    ns = "http://www.example.org#"

    pred = URI(namespace=ns, localname="happened")
    x = URI(namespace=ns, localname="x")
    y = URI(namespace=ns, localname="y")
    z = URI(namespace=ns, localname="z")

    x_dt = Literal(datetime.datetime(2009, 9, 28, 17, 41, 39))
    y_dt = Literal(datetime.datetime(2009, 9, 28, 18, 22))
    z_dt = Literal(datetime.datetime(2009, 9, 28, 17, 2, 41))

    conn.addStatement(Statement(x, pred, x_dt))
    conn.addStatement(Statement(y, pred, y_dt))
    conn.addStatement(Statement(z, pred, z_dt))

    start_dt = datetime.datetime(2009, 9, 28, 17)
    end_dt = datetime.datetime(2009, 9, 28, 18)

    the_range = conn.createRange(start_dt, end_dt)

    results = conn.getStatements(None, None, the_range)
    assert len(results) == 2

    def results_str():
        return "\n".join([str(result) for result in results])

    # Try the same query with Prolog and SPARQL
    lower = the_range.getLowerBound().toNTriples()
    upper = the_range.getUpperBound().toNTriples()
    queryString = """
       (select0 (?subject)
         (:limit 10)
         (:count-only t)
         (q- ?subject ? (? !%s !%s)))""" % (
        lower,
        upper,
    )

    print(queryString)
    count = conn.prepareTupleQuery(QueryLanguage.PROLOG, queryString).evaluate(
        count=True
    )
    assert count == 2, "%s\nResult was %d, should be 2." % (queryString, count)

    lower = the_range.getLowerBound().toNTriples()
    upper = the_range.getUpperBound().toNTriples()

    queryString = """
        SELECT ?event ?time WHERE {
          ?event %s ?time .
          FILTER (?time <= %s) }""" % (
        pred.toNTriples(),
        upper,
    )

    results = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString).evaluate()

    assert len(results) == 2, "%s\n%s\nResult count %s, should be 2." % (
        queryString,
        results_str(),
        len(results),
    )

    queryString = """
        SELECT ?event ?time WHERE {
          ?event %s ?time .
          FILTER (?time >= %s) }""" % (
        pred.toNTriples(),
        lower,
    )

    results = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString).evaluate()

    assert len(results) == 3, "%s\n%s\nResult count %s, should be 3." % (
        queryString,
        results_str(),
        len(results),
    )

    queryString = """
        SELECT ?event {
          ?event %s ?time .
          FILTER (?time >= %s && ?time <= %s) }""" % (
        pred.toNTriples(),
        lower,
        upper,
    )

    results = conn.prepareTupleQuery(QueryLanguage.SPARQL, queryString).evaluate()

    assert len(results) == 2, "%s\n%s\nResult count %s, should be 2." % (
        queryString,
        results_str(),
        len(results),
    )


def test_blanknodes():
    """
    Test Blank Node Creation
    """
    ValueFactory.BLANK_NODE_AMOUNT = 50
    conn = connect()
    node = conn.createBNode()
    assert node
    value_factory = conn.getValueFactory()
    assert len(value_factory.unusedBNodeIds) + 1 == ValueFactory.BLANK_NODE_AMOUNT


def test_uri_factory():
    """
    Test prefixed URI generation
    """
    conn = connect()
    value_factory = conn.getValueFactory()
    this_prefix, example_prefix = "http://this.org", "http://example.org"
    # Single-prefix:
    ns = value_factory.namespace(example_prefix)
    for r in [ns.resource, getattr(ns, "resource"), ns["resource"]]:
        assert r == value_factory.createURI(
            namespace=example_prefix, localname="resource"
        )
    # Multi-prefix:
    nss = value_factory.namespaces({"": this_prefix, "ex": example_prefix})
    assert nss[":resource"] == value_factory.createURI(
        namespace=this_prefix, localname="resource"
    )
    assert nss["ex:resource"] == value_factory.createURI(
        namespace=example_prefix, localname="resource"
    )
    assert nss["ex"] == value_factory.createURI(namespace=example_prefix, localname="")
    assert nss["ex:"] == value_factory.createURI(namespace=example_prefix, localname="")
    with pytest.raises(AssertionError):
        nss("unknown:unknown")


def test_delete_repository():
    """
    Test deleting a Repository.
    """
    server = AllegroGraphServer(AG_HOST, AG_PORT, USER, PASSWORD, proxy=AG_PROXY)
    store = "for_deletion"
    catalog = server.openCatalog(CATALOG)
    myRepository = catalog.getRepository(store, Repository.RENEW)
    myRepository.initialize()
    myRepository.shutDown()
    catalog.deleteRepository(store)
    assert store not in catalog.listRepositories()


def test_enable_spogi_cache():
    """
    Test enabling/disabling the store's spogi cache.
    """
    conn = connect()
    conn.enableSubjectTriplesCache(1000)
    assert conn.getSubjectTriplesCacheSize() == 1000
    conn.disableSubjectTriplesCache()
    assert not conn.getSubjectTriplesCacheSize()


def test_delete_mapping():
    conn = test1()
    conn.repository.mini_repository.deleteMappedType(XMLSchema.DATETIME)
    a_time = conn.createLiteral("1984-12-06T09:00:00.250", datatype=XMLSchema.DATETIME)
    conn.addTriple(
        "<http://franz.com/example#this>", "<http://franz.com/example#hasDate>", a_time
    )


class URIs:
    ## Create URIs for Bob and Robert (and kids)
    robert = URI("http://example.org/people/robert")
    roberta = URI("http://example.org/people/roberta")
    bob = URI("http://example.org/people/bob")
    bobby = URI("http://example.org/people/bobby")

    ## create name and child predicates, and Person class.
    name = URI("http://example.org/ontology/name")
    fatherOf = URI("http://example.org/ontology/fatherOf")
    person = URI("http://example.org/ontology/Person")

    hasChild = URI("http://example.org/ontology/hasChild")

    pointBlank = URI("http://example.org/ontology/pointBlank")


def setup():
    conn = test1()

    ## Bob is the same person as Robert
    conn.add(URIs.bob, OWL.SAMEAS, URIs.robert)

    ## Robert, Bob, and children are people
    conn.add(URIs.robert, RDF.TYPE, URIs.person)
    conn.add(URIs.roberta, RDF.TYPE, URIs.person)
    conn.add(URIs.bob, RDF.TYPE, URIs.person)
    conn.add(URIs.bobby, RDF.TYPE, URIs.person)

    ## They all have names.
    conn.add(URIs.robert, URIs.name, Literal("Robert"))
    conn.add(URIs.roberta, URIs.name, Literal("Roberta"))
    conn.add(URIs.bob, URIs.name, Literal("Bob"))
    conn.add(URIs.bobby, URIs.name, Literal("Bobby"))

    ## robert has a child
    conn.add(URIs.robert, URIs.fatherOf, URIs.roberta)
    ## bob has a child
    conn.add(URIs.bob, URIs.fatherOf, URIs.bobby)

    # Add some blank nodes
    conn.add(conn.createBNode(), URIs.pointBlank, conn.createBNode())
    conn.add(conn.createBNode(), URIs.pointBlank, conn.createBNode())
    conn.add(conn.createBNode(), URIs.pointBlank, conn.createBNode())

    return conn


def test_uri_split():
    x = URI(uri="http://foo.com/bar", namespace="http://foo.com/")
    nspace, localname = x.split()
    assert nspace == "http://foo.com/"
    assert localname == "bar"


def test_query_blank_node():
    """Try using blank node from one query in a second query."""
    conn = setup()
    trace("Finding blank node subjects...")
    query = conn.prepareTupleQuery(
        QueryLanguage.SPARQL,
        "SELECT * WHERE { ?subject %s _:object . }" % (URIs.pointBlank),
    )

    results = query.evaluate()

    # Pick one to use
    result = next(results)["subject"]

    trace(str(result))

    trace("Attempting to query using SPARQL with binding...")
    query = conn.prepareTupleQuery(
        QueryLanguage.SPARQL,
        "SELECT * WHERE { ?subject %s ?object . }" % (URIs.pointBlank),
    )
    query.setBinding("subject", result)
    output = " ".join([str(result) for result in query.evaluate()])
    trace(output)
    assert len(output)


def test_setInferencing():
    """prepareTupleQuery/setInferencing usage"""
    conn = setup()
    query = conn.prepareTupleQuery(
        QueryLanguage.SPARQL,
        "SELECT * WHERE { %s %s ?child . }" % (URIs.robert, URIs.fatherOf.toNTriples()),
    )

    ## List the children of Robert with inference ON. The owl:sameAs
    ## link combines the children of Bob with those of Robert.
    trace("Querying children of Robert, inference ON")
    query.setIncludeInferred(True)
    results = query.evaluate()
    on_len = len(results)
    trace(" ".join([str(result) for result in results]))

    ## List the children of Robert with inference OFF.
    query.setIncludeInferred(False)
    trace("Querying children of Robert, inferencing OFF")
    results = query.evaluate()
    off_len = len(results)
    trace(" ".join([str(result) for result in results]))
    assert on_len > off_len


def test_json_xml_response():
    """Test JSON and other result formats from SPARQL"""
    conn = setup()
    query = conn.prepareQuery(
        QueryLanguage.SPARQL,
        "SELECT * WHERE { %s %s ?child . }" % (URIs.robert, URIs.fatherOf.toNTriples()),
    )

    ## List the children of Robert with inference ON. The owl:sameAs
    ## link combines the children of Bob with those of Robert.
    trace(
        "Querying children of Robert w/ SPARQL, inference ON, application/sparql-results+xml"
    )
    query.setIncludeInferred(True)
    trace(query.evaluate_generic_query(accept="application/sparql-results+xml"))

    trace(
        "Querying children of Robert w/ SPARQL, inference ON, application/sparql-results+json"
    )
    trace(query.evaluate_generic_query(accept="application/sparql-results+json"))

    query = conn.prepareQuery(
        QueryLanguage.PROLOG,
        "(select (?child) (q !%s !%s ?child))"
        % (URIs.robert, URIs.fatherOf.toNTriples()),
    )

    trace(
        "Querying children of Robert w/ PROLOG, inference ON, application/sparql-results+xml"
    )
    query.setIncludeInferred(True)
    trace(query.evaluate_generic_query(accept="application/sparql-results+xml"))

    trace(
        "Querying children of Robert w/ PROLOG, inference ON, application/sparql-results+json"
    )
    trace(query.evaluate_generic_query(accept="application/sparql-results+json"))


def test_construct_query():
    """Test whether Construct Query"""
    conn = setup()
    query = conn.prepareGraphQuery(
        "SPARQL",
        "CONSTRUCT { ?p %s ?child . } WHERE { ?p ?relationship ?child . }"
        % URIs.hasChild,
    )
    query.setBinding("relationship", URIs.fatherOf)
    query.setIncludeInferred(True)

    trace("Trying a CONSTRUCT query with inferred ON")
    results = query.evaluate()
    assert len(results)
    trace("\n".join([str(result) for result in results]))

    query.setIncludeInferred(False)

    trace("Trying a CONSTRUCT query with inferred OFF")
    results = query.evaluate()
    assert len(results)
    trace("\n".join([str(result) for result in results]))


def test_session_loadinitfile():
    """
    Test starting a session with loadinitfile True.
    """
    # Basically ripped off from the miniclient tests.
    server = AllegroGraphServer(AG_HOST, AG_PORT, USER, PASSWORD, proxy=AG_PROXY)
    conn = connect()
    for x in range(0, 10):
        conn.mini_repository.addStatement(
            "<http:%d>" % x, "<http:before>", "<http:%d>" % (x + 1)
        )
        conn.mini_repository.addStatement(
            "<http:%d>" % (x + 1), "<http:after>", "<http:%d>" % x
        )
    [["<http:2>"]] == conn.mini_repository.evalPrologQuery(
        "(select (?x) (q- ?x !<http:before> !<http:3>))"
    )["values"]
    server.setInitfile(
        "(<-- (after-after ?a ?b) (q- ?a !<http:after> ?x) (q- ?x !<http:after> ?b))"
    )
    print(server.getInitfile())
    assert [["<http:5>"]] == conn.mini_repository.evalPrologQuery(
        "(select (?x) (after-after ?x !<http:3>))"
    )["values"]

    with conn.session(autocommit=True, loadinitfile=True) as session:
        [["<http:5>"]] == session.mini_repository.evalPrologQuery(
            "(select (?x) (after-after ?x !<http:3>))"
        )["values"]
    with conn.session(autocommit=True, loadinitfile=False) as session:
        with pytest.raises(RequestError):
            session.mini_repository.evalPrologQuery(
                "(select (?x) (after-after ?x !<http:3>))"
            )

    server.setInitfile(None)


def test_freetext():
    """
    Test registering a free text predicate, then doing a SPARQL query on it.
    """
    conn = connect()
    pred = URI("http://www.franz.com/has_name")
    conn.createFreeTextIndex("index1", predicates=[pred])
    conn.createFreeTextIndex(
        "index2",
        indexFields=["predicate", "object"],
        indexResources="short",
        minimumWordSize=4,
        innerChars="alpha",
        tokenizer="default",
    )
    conn.modifyFreeTextIndex(
        "index2",
        indexFields=["predicate", "object"],
        indexResources="short",
        minimumWordSize=2,
        innerChars="alphanumeric",
        tokenizer="default",
    )

    # config parameter fetching
    preds = conn.getFreeTextIndexConfiguration("index1")["predicates"]
    assert 1 == len(preds)
    assert str(pred) == str(preds[0])
    config = conn.getFreeTextIndexConfiguration("index2")
    assert ["object", "predicate"] == sorted(config["indexFields"])
    assert 2 == config["minimumWordSize"]
    assert "short" == config["indexResources"]
    assert ["alphanumeric"] == config["innerChars"]
    assert [] == config["borderChars"]
    assert "default" == config["tokenizer"]
    assert len(config["stopWords"])

    def contractor(i):
        return URI("http://www.franz.com/contractor#" + str(i))

    conn.addTriple(contractor(0), pred, "Betty Ross")
    conn.addTriple(contractor(1), pred, "Ross Jekel")
    conn.addTriple(contractor(2), pred, "Marijn Haverbeke")
    conn.addTriple(contractor(2), URI("http://www.franz.com/lives_in"), "Berlin")
    conn.addTriple(contractor(3), pred, "Ed")

    search1 = conn.evalFreeTextSearch("Ross", index="index1")
    assert 2 == len(search1)
    assert set([str(contractor(1)), str(contractor(0))]) == set(
        [str(search1[0][0]), str(search1[1][0])]
    )
    assert 2 == len(conn.evalFreeTextSearch("Ross"))

    # Test with limit/offset
    search1 = conn.evalFreeTextSearch("Ross", index="index1", limit=1, offset=0)
    search2 = conn.evalFreeTextSearch("Ross", index="index1", limit=1, offset=1)
    assert len(search1) == 1
    assert len(search2) == 1
    assert search1[0][0] != search2[0][0]

    # min word size
    assert 0 == len(conn.evalFreeTextSearch("Ed", index="index1"))
    assert 1 == len(conn.evalFreeTextSearch("Ed", index="index2"))

    # indexing of predicates
    assert 0 == len(conn.evalFreeTextSearch("has_name", index="index1"))
    assert 4 == len(conn.evalFreeTextSearch("has_name", index="index2"))

    # sparql
    results = conn.prepareTupleQuery(
        QueryLanguage.SPARQL,
        'SELECT ?something WHERE { ?something fti:match "Ross Jekel". }',
    ).evaluate()
    assert len(results)


def test_graphql_basic():
    server = AllegroGraphServer(AG_HOST, AG_PORT, USER, PASSWORD, proxy=AG_PROXY)
    if server.version >= "7.3.0":
        conn = connect()
        path = os.path.join(CURRENT_DIRECTORY, "nsw.ttl")
        conn.addFile(path, format=RDFFormat.TURTLE)
        actual = conn.evalGraphqlQuery(
            query="{ Hero { name } }  { Human { name } }",
            default_prefix="http://example.com/",
        )
        import json

        expected = json.loads(
            """[
    {
        "Hero": [
        {
            "name": "Lando Calrissian"
        },
        {
            "name": "Luke Skywalker"
        },
        {
            "name": "R2-D2"
        }
        ]
    },
    {
        "Human": [
        {
            "name": "Lando Calrissian"
        },
        {
            "name": "Luke Skywalker"
        }
        ]
    }
    ]"""
        )
        assert len(actual) == len(expected)
        assert len(actual[0]["Hero"]) == len(expected[0]["Hero"])
        assert len(actual[1]["Human"]) == len(expected[1]["Human"])


def test_graphql_namespaces():
    server = AllegroGraphServer(AG_HOST, AG_PORT, USER, PASSWORD, proxy=AG_PROXY)
    if server.version >= "7.3.0":
        conn = connect()
        path = os.path.join(CURRENT_DIRECTORY, "nsw.ttl")
        conn.addFile(path, format=RDFFormat.TURTLE)
        actual = conn.evalGraphqlQuery(
            query="{ Hero { ex:name } }  { Human { name } }",
            default_prefix="http://example.com/",
            namespaces="ex http://example.org/,exs https://example.org/",
        )
        import json

        expected = json.loads(
            """[
    {
        "Hero": [
        {
            "name": "Lando Calrissian"
        },
        {
            "name": "Luke Skywalker"
        },
        {
            "name": "R2-D2"
        }
        ]
    },
    {
        "Human": [
        {
            "name": "Lando Calrissian"
        },
        {
            "name": "Luke Skywalker"
        }
        ]
    }
    ]"""
        )
        assert len(actual) == len(expected)
        assert len(actual[0]["Hero"]) == len(expected[0]["Hero"])
        assert len(actual[1]["Human"]) == len(expected[1]["Human"])


def test_javascript():
    conn = connect()
    assert conn.evalJavaScript("1+1") == 2
    assert (
        conn.evalJavaScript(
            "store.addTriple('<a>', '<b>', '\"blah blah\"'); store.size"
        )
        == 1
    )
    assert conn.evalJavaScript("store.getTriplesArray()[0].subject.value") == "a"
    assert conn.evalJavaScript("store.getTriplesArray()[1]") is None
    assert (
        conn.evalJavaScript("store.getTriples().next().object.toString()")
        == '"blah blah"'
    )
    assert conn.evalJavaScript("store.indices.length > 2") is True
    assert (
        conn.evalJavaScript(
            "store.createTextIndex('foo'); store.textSearch('blah', 'foo').next().predicate.value"
        )
        == "b"
    )
    assert conn.evalJavaScript("namespaces.collect().length > 1") is True
    assert (
        conn.evalJavaScript(
            "namespaces.register('blah', 'http://blah.com'); x = namespaces.lookup('blah'); "
            "namespaces.unregister('blah'); x + namespaces.lookup('blah')"
        )
        == "http://blah.comnull"
    )

    with conn.session():
        conn.evalJavaScript("var x = 100;")
        assert conn.evalJavaScript("x") == 100


def test_roundtrips():
    """
    Test round-tripping of Python values.
    """
    conn = connect()
    now = datetime.datetime.now()
    conn.addTriple("<http:object>", "<http:bool>", True)
    conn.addTriple("<http:object>", "<http:str>", "Me")
    conn.addTriple("<http:object>", "<http:int>", 1234)
    conn.addTriple("<http:object>", "<http:date>", now.date())
    conn.addTriple("<http:object>", "<http:datetime>", now)
    conn.addTriple("<http:object>", "<http:time>", now.time())
    then = datetime.time(11, 34, 16, 386672)
    conn.addTriple("<http:objecT>", "<http:time2>", then)

    def checkit(name, the_type, value):
        obj = (
            next(conn.getStatements(None, "<http:%s>" % name, None))
            .getObject()
            .toPython()
        )
        assert isinstance(obj, the_type)
        assert obj == value

    def time_check(name, the_type, value):
        obj = (
            next(conn.getStatements(None, "<http:%s>" % name, None))
            .getObject()
            .toPython()
        )
        assert isinstance(obj, the_type)
        # Ignore time zone - 'value' is going to be naive
        obj = obj.replace(tzinfo=None)
        # Microseconds can have floating point roundoff...
        print("Original:", value, "Store:", obj)
        assert obj == value or abs(obj.microsecond - value.microsecond) < 300

    checkit("bool", bool, True)
    checkit("str", str, "Me")
    checkit("int", int, 1234)
    checkit("date", datetime.date, now.date())
    time_check("datetime", datetime.datetime, now)
    time_check("time", datetime.time, now.time())
    time_check("time2", datetime.time, then)


def test_add_commit_size():
    """
    Test the add_commit_size setting.
    """
    conn = connect()
    path = os.path.join(CURRENT_DIRECTORY, "kennedy-error.nt")
    baseURI = "http://example.org/example/local"

    assert conn.add_commit_size is None
    conn.add_commit_size = 10
    assert conn.add_commit_size == 10
    assert conn.getAddCommitSize() == 10

    try:
        conn.add(path, base=baseURI, format=RDFFormat.NTRIPLES)
    except RequestError:
        pass

    assert conn.size() == 20

    conn.clear()
    conn.setAddCommitSize(0)
    assert conn.getAddCommitSize() is None

    try:
        conn.add(path, base=baseURI, format=RDFFormat.NTRIPLES)
    except RequestError:
        pass

    assert conn.size() == 0


def test_script_management():
    server = AllegroGraphServer(AG_HOST, AG_PORT, USER, PASSWORD, proxy=AG_PROXY)
    scripts = len(server.listScripts())

    server.addScript("script.cl", test_script_management.code)
    assert len(server.listScripts()) == (scripts + 1)
    assert server.getScript("script.cl") == test_script_management.code

    conn = connect()
    result = conn.callStoredProc("add-two-ints", "script.cl", 1, 2)
    print(result)
    assert int(result) == 3

    server.deleteScript("script.cl")
    assert len(server.listScripts()) == scripts


test_script_management.code = """
;; ag 4.x style where we let the def-stored-proc code generate code
;; to check if the correct number of arguments were passed in the
;; argument vector and if not to return an error indication

(def-stored-proc add-two-ints (a b)
      ;; takes two values and adds them
      (+ a b))
"""


def test_namespace_management():
    """
    Test namespace management features
    """
    conn = connect()

    conn.clearNamespaces()
    namespaces = conn.getNamespaces()
    count = len(namespaces)

    # assert that all namepaces returned by getNamespaces can be gotten individually
    print(namespaces)
    for namespace, value in namespaces.items():
        assert value == conn.getNamespace(namespace)

    test_spaces = {
        "kdy": "http://www.franz.com/simple#",
        "vcd": "http://www.w3.org/2001/vcard-rdf/3.0#",
        "ex": "http://example.org/people/",
        "ont": "http://example.org/ontology/",
        "rltv": "http://www.franz.com/simple#",
    }

    for namespace, value in test_spaces.items():
        print("calling setNamespace", namespace, value)
        conn.setNamespace(namespace, value)

    assert count + len(test_spaces) == len(conn.getNamespaces())

    for namespace, value in test_spaces.items():
        assert value == conn.getNamespace(namespace)

    # Try adding a namespace that already exists
    for namespace, value in test_spaces.items():
        conn.setNamespace(namespace, value)

    assert count + len(test_spaces) == len(conn.getNamespaces())

    # Remove the original namespaces
    for namespace in namespaces.keys():
        conn.removeNamespace(namespace)

    # Assert they are gone
    assert len(test_spaces) == len(conn.getNamespaces())

    for namespace in namespaces.keys():
        with pytest.raises(RequestError):
            conn.getNamespace(namespace)

    # Test clearing all namespaces
    conn.clearNamespaces(reset=False)

    assert len(conn.getNamespaces()) == 0

    # Add a bunch back and clear with reset
    for namespace, value in test_spaces.items():
        conn.setNamespace(namespace, value)

    assert len(test_spaces) == len(conn.getNamespaces())

    conn.clearNamespaces(reset=True)

    assert namespaces == conn.getNamespaces()


def test_indices():
    """
    Test creating and deleting indices.
    """
    conn = connect()
    assert "spogi" in conn.listValidIndices()
    assert "spogi" in conn.listIndices()
    assert len(conn.listValidIndices()) > len(conn.listIndices())
    try:
        conn.addIndex("i")
        assert "i" in conn.listIndices()
    finally:
        conn.dropIndex("i")
    assert not ("i" in conn.listIndices())


def test_indices_on_create():
    """
    Test passing indices to createRepository.
    """
    server = AllegroGraphServer(AG_HOST, AG_PORT, USER, PASSWORD, proxy=AG_PROXY)
    catalog = server.openCatalog(CATALOG)
    if "optimal" in catalog.listRepositories():
        catalog.deleteRepository("optimal")

    indices = ["posgi", "gspoi"]
    myRepository = catalog.createRepository("optimal", indices=indices)
    myRepository.initialize()
    conn = myRepository.getConnection()
    assert set(indices) == set(conn.listIndices())


def test_optimize_indices():
    conn = test6()
    # Need a bigger store to test for real, just test the call for now
    conn.optimizeIndices(wait=True)


def test_delete_duplicates():
    graph1 = URI("http://www.example.com/graph#1")
    graph2 = URI("http://www.example.com/graph#2")

    with connect().session() as conn:
        conn.add(URIs.robert, URIs.hasChild, URIs.roberta)
        conn.add(URIs.robert, URIs.hasChild, URIs.roberta, graph1)
        conn.add(URIs.robert, URIs.hasChild, URIs.roberta, graph2)
        conn.add(URIs.robert, URIs.hasChild, URIs.roberta, graph2)

        conn.add(URIs.bob, URIs.hasChild, URIs.bobby)
        conn.add(URIs.bob, URIs.hasChild, URIs.bobby, graph1)
        conn.add(URIs.bob, URIs.hasChild, URIs.bobby, graph2)
        conn.add(URIs.bob, URIs.hasChild, URIs.bobby, graph2)

        conn.commit()

        assert conn.size() == 8

        assert conn.getDuplicateStatements("spog").rowCount() == 2

        conn.deleteDuplicates("spog")
        conn.commit()

        assert conn.size() == 6

        assert conn.getDuplicateStatements("spo").rowCount() == 4

        conn.deleteDuplicates("spo")
        conn.commit()

        assert conn.size() == 2


class URIs:
    ## Create URIs for Bob and Robert (and kids)
    robert = URI("http://example.org/people/robert")
    roberta = URI("http://example.org/people/roberta")
    bob = URI("http://example.org/people/bob")
    bobby = URI("http://example.org/people/bobby")

    ## create name and child predicates, and Person class.
    name = URI("http://example.org/ontology/name")
    fatherOf = URI("http://example.org/ontology/fatherOf")
    person = URI("http://example.org/ontology/Person")

    hasChild = URI("http://example.org/ontology/hasChild")

    pointBlank = URI("http://example.org/ontology/pointBlank")


def test_bulkmode():
    """
    Test the setting and getting of bulk mode.
    """
    conn = connect()
    repo = conn.repository

    repo.bulk_mode = True
    assert repo.bulk_mode, "BulkMode should be on."
    repo.bulk_mode = False
    assert not repo.bulk_mode, "BulkMode should be off."


def test_analyze_query():
    """
    Tests query analysis on a SPARQL Tuple Query.
    """
    conn = test2()
    try:
        queryString = "SELECT ?s ?p ?o  WHERE {?s ?p ?o .}"
        tupleQuery = conn.prepareTupleQuery("SPARQL", queryString)
        result = tupleQuery.analyze()
        assert "desired" in result and "actual" in result
        result = tupleQuery.evaluate()
        verify(result.rowCount(), 4, "len(result)", 3)
    finally:
        conn.close()


def test_users_roles_filters():
    """
    Test user/role/filter management.
    """
    server = AllegroGraphServer(AG_HOST, AG_PORT, USER, PASSWORD, proxy=AG_PROXY)
    default_users = server.listUsers()
    assert "test" in default_users
    server.addUser("user-test", "xyzzy")
    assert "user-test" in server.listUsers()
    server.changeUserPassword("user-test", "xyzzy-new")

    server_user = AllegroGraphServer(AG_HOST, AG_PORT, "user-test", "xyzzy-new")

    # FIXME: uncomment this after change 22700 gets merged in.
    # assert [] == server_user.listCatalogs()

    server.addUserAccess("user-test", True, True)

    assert None in server_user.listCatalogs()
    if CATALOG and CATALOG != "/":
        assert CATALOG in server_user.listCatalogs()

    access = server.listUserAccess("user-test")
    assert len(access) == 1
    access = access[0]
    assert (
        access["read"]
        and access["write"]
        and access["catalog"] == "*"
        and access["repository"] == "*"
    )

    access = server.listUserEffectiveAccess("user-test")
    assert len(access) == 1
    access = access[0]
    assert (
        access["read"]
        and access["write"]
        and access["catalog"] == "*"
        and access["repository"] == "*"
    )

    # Beginning in v7.4.0 new users get the session permission when created
    # thus to make this test work with all versions of agraph we'll
    # add the session permission (just in case it's not present)

    server.addUserPermission("user-test", "session")
    permission = server.listUserPermissions("user-test")
    assert len(permission) == 1

    permission = server.listUserEffectivePermissions("user-test")
    assert len(permission) == 1

    server.addUserPermission("user-test", "eval")
    server.addUserPermission("user-test", "replication")
    server.deleteUserPermission("user-test", "replication")

    permission = server.listUserPermissions("user-test")
    assert len(permission) == 2 and "eval" in permission
    assert len(permission) == 2 and "session" in permission

    roles = server.listRoles()
    assert len(roles) == 0

    server.addRole("role-test")
    server.addRole("role-to-delete")
    roles = server.listRoles()
    assert len(roles) == 2

    server.addRoleAccess("role-to-delete", True, True, "/", "*")
    access = server.listRoleAccess("role-to-delete")
    assert len(access) == 1
    access = access[0]
    assert (
        access["read"]
        and access["write"]
        and access["catalog"] == "/"
        and access["repository"] == "*"
    )
    server.addRoleSecurityFilter("role-to-delete", "allow", s=URI("<allowed>"))
    filters = server.listRoleSecurityFilters("role-to-delete", "allow")
    assert len(filters) == 1
    filters = filters[0]
    assert (
        filters["s"] == "<allowed>"
        and not filters["p"]
        and not filters["o"]
        and not filters["g"]
    )
    server.deleteRoleSecurityFilter("role-to-delete", "allow", s=URI("<allowed>"))
    filters = server.listRoleSecurityFilters("role-to-delete", "allow")
    assert len(filters) == 0
    server.addUserRole("user-test", "role-to-delete")
    access = server.listUserEffectiveAccess("user-test")
    assert len(access) == 2

    server.addUserSecurityFilter("user-test", "allow", s=URI("<allowed>"))
    filters = server.listUserSecurityFilters("user-test", "allow")
    assert len(filters) == 1
    filters = filters[0]
    assert (
        filters["s"] == "<allowed>"
        and not filters["p"]
        and not filters["o"]
        and not filters["g"]
    )

    with connect().session() as conn:
        conn.addTriple(URI("<allowed>"), URI("<p>"), URI("<o>"), URI("<there>"))
        conn.addTriple(URI("<secret>"), URI("<p>"), URI("<o>"), URI("<gone>"))
        assert conn.size() == 2

        conn.runAsUser("user-test")
        assert conn.size() == 1

    server.deleteUserSecurityFilter("user-test", "allow", s=URI("<allowed>"))
    server.deleteUserRole("user-test", "role-to-delete")
    server.deleteRoleAccess("role-to-delete", True, True, "/", "*")
    access = server.listRoleAccess("role-to-delete")
    assert len(access) == 0

    server.deleteRole("role-to-delete")
    roles = server.listRoles()
    assert len(roles) == 1
    server.addRolePermission("role-test", "eval")
    server.addRolePermission("role-test", "session")
    server.deleteRolePermission("role-test", "eval")
    permission = server.listRolePermissions("role-test")
    assert len(permission) == 1 and "session" in permission

    server.addUserRole("user-test", "role-test")
    roles = server.listUserRoles("user-test")
    assert len(roles) == 1 and "role-test" in roles

    permission = server.listUserEffectivePermissions("user-test")
    assert len(permission) == 2 and "eval" in permission and "session" in permission

    roles = server.listRoles()

    server.deleteUserRole("user-test", "role-test")
    roles = server.listUserRoles("user-test")
    assert len(roles) == 0

    server.deleteRole("role-test")
    roles = server.listRoles()
    assert len(roles) == 0

    server.deleteUserAccess("user-test", True, True)
    access = server.listUserAccess("user-test")
    assert len(access) == 0

    server.deleteUser("user-test")
    assert set(server.listUsers()) == set(default_users)
    assert "test" in server.listUsers()


@pytest.mark.skipif(
    not os.environ.get("AG_RUN_SSL_TEST"),
    reason="Too difficult to set up if you do not have AG sources.",
)
def test_ssl():
    """
    Test the SSL port and certificate functionality via a series of
    server calls to connect to and empty a store.
    """

    # This assumes that tests were invoked from the Makefile at the top
    # of the agraph-python module.
    agraph_ssl_dir = "../agraph/lisp/ssl"

    server = AllegroGraphServer(
        AG_HOST,
        AG_SSLPORT,
        cainfo=os.path.join(agraph_ssl_dir, "ca.cert"),
        sslcert=os.path.join(agraph_ssl_dir, "test.cert"),
        # Test files setup for localhost; if running elsewhere don't verify
        verifyhost=2 if AG_HOST == LOCALHOST else None,
    )
    catalogs = server.listCatalogs()

    print("Available catalogs", catalogs)

    catalog = server.openCatalog(CATALOG)
    stores = catalog.listRepositories()
    print(
        "Available repositories in catalog '%s':  %s"
        % (catalog.getName(), catalog.listRepositories())
    )

    # Instead of renewing the database, clear it.
    mode = Repository.CREATE if STORE not in stores else Repository.OPEN

    my_repository = catalog.getRepository(STORE, mode)
    my_repository.initialize()
    connection = my_repository.getConnection()
    connection.clear()
    connection.clearNamespaces()

    assert connection.size() == 0


def test_save_response():
    """
    Tests saving the response object.
    """
    conn = test2()
    buf = io.BytesIO()
    queryString = "SELECT ?s ?p ?o  WHERE {?s ?p ?o .}"
    tupleQuery = conn.prepareTupleQuery("SPARQL", queryString)
    with conn.saveResponse(buf, "application/sparql-results+xml"):
        tupleQuery.evaluate()

    print(buf.getvalue())
    assert len(buf.getvalue()) > 0

    buf = io.BytesIO()
    with conn.saveResponse(buf, "application/rdf+xml"):
        conn.getStatements(subject=None, predicate=None, object=None)

    print(buf.getvalue())
    assert len(buf.getvalue()) > 0


def test_encoded_ids():
    """
    Tests encoded ids.
    """
    with connect().session() as conn:
        prefixes = [
            "http://www.franz.com/customer",
            "http://www.franz.com/employee",
            "http://www.franz.com/company",
            "http://www.franz.com/remove",
        ]
        formats = ["[0-9]{1,10}", "[0-9]{1,11}", "[0-9]{12}", "[0-9]{1,5}"]
        conn.registerEncodedIdPrefix(prefixes[0], formats[0])

        regs = conn.listEncodedIdPrefixes()

        assert (
            len(regs) == 1
            and regs[0].prefix == prefixes[0]
            and regs[0].format == formats[0]
        )

        regs = [(prefixes[i], formats[i]) for i in range(1, len(prefixes))]
        conn.registerEncodedIdPrefixes(regs)

        regs = conn.listEncodedIdPrefixes()
        assert len(regs) == 4

        for reg in regs:
            index = prefixes.index(reg.prefix)
            assert index >= 0 and formats[index] == reg.format

        ids = conn.allocateEncodedIds(prefixes[2], 100)

        assert len(ids) == 100
        assert ids == ["<%s@@%012d>" % (prefixes[2], i) for i in range(0, 100)]

        conn.addTriple(
            "<%s@@0>" % prefixes[0],
            "<%s@@0>" % prefixes[1],
            conn.createURI(ids[0]),
            "<%s@@0>" % prefixes[0],
        )

        assert conn.evalInServer(
            "(= (upi-type-code (subject (first (get-triples-list)))) 45)"
        )
        assert conn.evalInServer(
            "(= (upi-type-code (predicate (first (get-triples-list)))) 45)"
        )
        assert conn.evalInServer(
            "(= (upi-type-code (object (first (get-triples-list)))) 45)"
        )
        assert conn.evalInServer(
            "(= (upi-type-code (graph (first (get-triples-list)))) 45)"
        )

        conn.unregisterEncodedIdPrefix(prefixes[3])

        regs = conn.listEncodedIdPrefixes()
        assert len(regs) == 3

        for reg in regs:
            index = prefixes.index(reg.prefix)
            assert index < 4 and formats[index] == reg.format


def test_spin():
    "Testing spin functions."
    baseuri = "http://ex.org#"
    age_fn = baseuri + "age"
    parents_mp = baseuri + "parents"

    with connect().session() as conn:
        conn.setNamespace("ex", baseuri)
        test6(conn)

        def check_function():
            try:
                conn.getSpinFunction(age_fn)
            except RequestError as e:
                if e.message.find("is not a registered SPIN function") >= 0:
                    print("Correct: no age function is defined.")
                else:
                    assert False, age_fn + " should not exist!"

        check_function()

        ageFnSparql = (
            "prefix kennedy: <http://www.franz.com/simple#>\n"
            + "prefix xs: <http://www.w3.org/2001/XMLSchema#>\n"
            + "select ( (2011 - xs:int(?birthYear)) as ?age ) { ?who kennedy:birth-year ?birthYear . }"
        )
        conn.putSpinFunction(age_fn, ageFnSparql, ["?who"])
        assert ageFnSparql == conn.getSpinFunction(age_fn)

        results = conn.prepareTupleQuery(
            "SPARQL",
            "prefix ex: <"
            + baseuri
            + ">\n"
            + "prefix kennedy: <http://www.franz.com/simple#>\n"
            + "select ?first ?last ?age ?birthYear {\n"
            + "?person kennedy:first-name ?first .\n"
            + "?person kennedy:last-name ?last .\n"
            + "?person kennedy:birth-year ?birthYear .\n"
            + "bind( ex:age( ?person ) as ?age ) .\n"
            + "} order by ?age limit 2",
        ).evaluate()

        assert next(results)["age"] == conn.createLiteral(39, XMLSchema.INTEGER)
        assert next(results)["age"] == conn.createLiteral(43, XMLSchema.INTEGER)
        results = conn.listSpinFunctions()
        assert results[0]["uri"] == "<" + age_fn + ">"
        assert results[0]["query"] == ageFnSparql
        assert results[0]["arguments"] == "?who"

        conn.deleteSpinFunction(age_fn)

        check_function()

        def check_property():
            try:
                conn.getSpinMagicProperty(parents_mp)
            except RequestError as e:
                if e.message.find("is not a registered SPIN magic property"):
                    print("Correct: no parents magic property is defined.")
                else:
                    assert False, parents_mp + " should not exist"

        check_property()

        parents_fn = (
            "prefix kennedy: <http://www.franz.com/simple#>\n"
            + "select ?parent { ?parent kennedy:has-child ?child . }"
        )
        conn.putSpinMagicProperty(parents_mp, parents_fn, ["?child"])
        assert parents_fn == conn.getSpinMagicProperty(parents_mp)

        query = (
            "prefix ex: <"
            + baseuri
            + ">\n"
            + "prefix kennedy: <http://www.franz.com/simple#>\n"
            + "select ?person ?parentFirst {\n"
            + "?person kennedy:first-name 'Joseph' .\n"
            + "?person kennedy:birth-year '1915' .\n"
            + "?person ex:parents ?parent .\n"
            + "?parent kennedy:first-name ?parentFirst .\n}"
        )
        results = conn.prepareTupleQuery("SPARQL", query).evaluate()

        seen_joseph = False
        seen_rose = False

        for bindingSet in results:
            if str(bindingSet["parentFirst"]) == '"Joseph"':
                seen_joseph = True
            if str(bindingSet["parentFirst"]) == '"Rose"':
                seen_rose = True

        assert seen_joseph
        assert seen_rose

        results = conn.listSpinMagicProperties()
        assert results[0]["uri"] == "<" + parents_mp + ">"
        print(results[0]["query"] == parents_fn)
        assert results[0]["arguments"] == "?child"

        conn.deleteSpinMagicProperty(parents_mp)

        check_property()


def test_sparql_update():
    conn = connect()
    ## create some resources and literals to make statements out of
    alice = conn.createURI("http://example.org/people/alice")
    bob = conn.createURI("http://example.org/people/bob")
    name = conn.createURI("http://example.org/ontology/name")
    person = conn.createURI("http://example.org/ontology/Person")
    bobsName = conn.createLiteral("Bob")
    alicesName = conn.createLiteral("Alice")
    print("Triple count before inserts: ", conn.size())
    for s in conn.getStatements(None, None, None, None):
        print(s)
    query = "INSERT DATA { %s %s %s. %s %s %s. %s %s %s. %s %s %s. }" % (
        alice.toNTriples(),
        RDF.TYPE.toNTriples(),
        person.toNTriples(),
        alice.toNTriples(),
        name.toNTriples(),
        alicesName.toNTriples(),
        bob.toNTriples(),
        RDF.TYPE.toNTriples(),
        person.toNTriples(),
        bob.toNTriples(),
        name.toNTriples(),
        bobsName.toNTriples(),
    )
    conn.prepareUpdate(QueryLanguage.SPARQL, query).evaluate()
    print("Triple count: ", conn.size())
    verify(conn.size(), 4, "conn.size()", "test_sparql_update")
    for s in conn.getStatements(None, None, None, None):
        print(s)
    query = "DELETE DATA { %s %s %s. }" % (
        bob.toNTriples(),
        name.toNTriples(),
        bobsName.toNTriples(),
    )
    conn.prepareUpdate(QueryLanguage.SPARQL, query).evaluate()
    print("Triple count: ", conn.size())
    verify(conn.size(), 3, "conn.size()", "test_sparql_update")
    for s in conn.getStatements(None, None, None, None):
        print(s)


def test_materializer():
    conn = connect()
    data = """
        <http://www.franz.com/simple#birra> <http://www.w3.org/2002/07/owl#sameAs> <http://www.franz.com/simple#son-of-samira> .
        <http://www.franz.com/simple#aasman> <http://www.w3.org/2002/07/owl#sameAs> <http://www.franz.com/simple#jannes> .
        <http://www.franz.com/simple#jans> <http://www.w3.org/2002/07/owl#sameAs> <http://www.franz.com/simple#jannes> .
        <http://www.franz.com/simple#birra> <http://www.franz.com/simple#age> <http://www.franz.com/simple#twelve> .
        <http://www.franz.com/simple#pet-of> <http://www.w3.org/2002/07/owl#inverseOf> <http://www.franz.com/simple#has-pet> .
        <http://www.franz.com/simple#has-pet> <http://www.w3.org/2000/01/rdf-schema#subPropertyOf> <http://www.franz.com/simple#owns> .
        <http://www.franz.com/simple#has> <http://www.w3.org/2002/07/owl#inverseOf> <http://www.franz.com/simple#owned-by> .
        <http://www.franz.com/simple#owned-by> <http://www.w3.org/2002/07/owl#inverseOf> <http://www.franz.com/simple#owns> .
        <http://www.franz.com/simple#jans> <http://www.franz.com/simple#has-pet> <http://www.franz.com/simple#birra> .
        """
    conn.mini_repository.loadData(data, RDFFormat.NTRIPLES)

    entailed = conn.materializeEntailed(_with="all")
    print("There were", entailed, "entailed triples")
    assert conn.materializeEntailed(_with="all") >= 40
    assert conn.deleteMaterialized() >= 40


def test_escaping():
    conn = connect()
    s = conn.createURI('http://franz.com/ex#"Zażółć gęślą jaźń"')
    p = conn.createURI(
        'http://franz.com/ex#<langle{lcurl|or^caret"dblquote\\bslash}rcurl>rangle'
    )
    t = conn.createURI('http://franz.com/ex#"quoted"')
    o = conn.createLiteral('"Zażółć gęślą jaźń"', t)
    conn.addTriple(s, p, o)
    results = list(conn.getStatements(None, None, None))
    assert len(results) == 1
    assert results[0].getSubject() == s
    assert results[0].getPredicate() == p
    assert results[0].getObject() == o


def test_uri_eq():
    x = URI("http://www.franz.com/test")
    y = URI("http://www.franz.com/test")
    z = URI("http://www.franz.com/test2")
    assert x == y
    assert not x != y
    assert x != z
    assert not x == z


def test_literal_eq():
    test = Literal("test")
    test_eq = Literal("test")
    test_str = Literal("test", datatype=XMLSchema.STRING)
    test_str_eq = Literal("test", datatype=XMLSchema.STRING)
    test_en = Literal("test", language="en_US")
    test_en_eq = Literal("test", language="en_US")
    not_test = Literal("not_test")

    assert test == test_eq
    assert not test != test_eq
    assert test_str == test_str_eq
    assert not test_str != test_str_eq
    assert test_en == test_en_eq
    assert not test_en != test_en_eq

    assert not test == not_test
    assert test != not_test

    assert not test == test_str
    assert test != test_str
    assert not test == test_en
    assert test != test_en
    assert not test_en == test_str
    assert test_en != test_str


def test_value_eq_diffrent_types():
    uri = URI("http://www.franz.com/test")
    literal = Literal("http://www.franz.com/test")
    literal2 = Literal("<http://www.franz.com/test>")
    text = "<http://www.franz.com/test>"

    assert uri != literal
    assert not uri == literal
    assert uri != literal2
    assert not uri == literal2
    assert uri != text
    assert not uri == text

    assert literal2 != text


def test_bnode_eq():
    b42 = BNode(42)
    b42_eq = BNode(42)
    b44 = BNode(44)

    assert b42 == b42_eq
    assert not b42 != b42_eq

    assert b42 != b44
    assert not b42 == b44


def test_value_ordering():
    literals = [Literal("urn:x-test:%d" % x) for x in [1, 2, 3]]
    uris = [URI("urn:x-test:%d" % x) for x in [1, 2, 3]]
    values = [literals[1], uris[1], uris[0], literals[2], literals[0], uris[2]]
    values.sort()

    # Literal < URI
    assert values == literals + uris


def test_turtle_import():
    conn = connect()
    path = os.path.join(CURRENT_DIRECTORY, "kennedy.ttl")
    base_uri = "http://example.org/example/local"
    conn.addFile(path, base_uri, format=RDFFormat.TURTLE)
    assert conn.size() == 1214


def test_turtle_import_compressed():
    conn = connect()
    path = os.path.join(CURRENT_DIRECTORY, "kennedy.ttl.gz")
    base_uri = "http://example.org/example/local"
    conn.addFile(path, base_uri, format=RDFFormat.TURTLE)
    assert conn.size() == 1214


def test_turtle_import_from_string():
    conn = connect()
    base_uri = "http://example.org/example/local"
    data = """
        <jans> <age> 58 ; <livesIn> <place100> .
        <place100> <name> 'Moraga'; <population> 16000 .
    """
    conn.addData(data, RDFFormat.TURTLE, base_uri=base_uri)
    assert conn.size() == 4


def test_root_catalog():
    "'root' denotes the root catalog."
    server = AllegroGraphServer(AG_HOST, AG_PORT, USER, PASSWORD, proxy=AG_PROXY)
    catalog = server.openCatalog("root")
    assert catalog.name is None
