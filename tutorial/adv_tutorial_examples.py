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

def test101(accessMode=Repository.RENEW):
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
    conn = myRepository.getConnection()
    return conn

def test102():
    conn = test101()
    conn.clear()   
    path1 = "./vc-db-1.rdf"    
    path2 = "./kennedy.ntriples"                
    baseURI = "http://example.org/example/local"
    context = conn.createURI("http://example.org#vcards")
    conn.setNamespace("vcd", "http://www.w3.org/2001/vcard-rdf/3.0#");
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

def test103():
    """
    Range matches (same as test15)
    """
    conn = test101();
    conn.clear()
    exns = "http://example.org/people/"
    conn.setNamespace('ex', exns)
    alice = conn.createURI(exns, "alice")
    bob = conn.createURI(exns, "bob")
    carol = conn.createURI(exns, "carol")    
    age = conn.createURI(exns, "age")    
    range = conn.createRange(30, 50)
    if False: conn.registerDatatypeMapping(predicate=age, nativeType="int")
    if True: conn.registerDatatypeMapping(datatype=XMLSchema.INT, nativeType="int")    
    conn.add(alice, age, 42)
    conn.add(bob, age, 24) 
    conn.add(carol, age, "39") 
    rows = conn.getStatements(None, age, range)    
    for r in rows:
        print r 
        
def test104():
    """
    """
    conn = test101();
    conn.clear()
    exns = "http://example.org/people/"
    conn.setNamespace('ex', exns)
    alice = conn.createURI(exns, "alice")
    bob = conn.createURI(exns, "bob")
    carol = conn.createURI(exns, "carol")
    conn.createRectangularSystem(scale=1, xMax=100, yMax=100)
    location = conn.createURI(exns, "location")
    #conn.registerDatatypeMapping(predicate=location, nativeType="int")   
    conn.registerDatatypeMapping(predicate=location, nativeType="float")       
    conn.add(alice, location, conn.createCoordinate(30,30))
    conn.add(bob, location, conn.createCoordinate(40, 40))
    conn.add(carol, location, conn.createCoordinate(50, 50)) 
    #box1 = conn.createBox(20, 40, 20, 40) 
    box1 = conn.createBox(51.0, 52.0, 0.08, 0.09) 
    print box1
    for r in conn.getStatements(None, location, box1) : print r
    box2 = conn.createBox(30, 30, 30, 30) 
    print box2
    for r in conn.getStatements(None, location, box2) : print r
    circle1 = conn.createCircle(35, 35, radius=10)
    #circle1 = conn.createCircle(0, 0, radius=100)    
    print circle1
    for r in conn.getStatements(None, location, circle1) : print r 
    polygon1 = conn.createPolygon([(35,40), (40,45),(45,40), (40,35)])
    print polygon1
    for r in conn.getStatements(None, location, polygon1) : print r
    # now we switch to a LatLong (spherical) coordinate system
    amsterdam = conn.createURI(exns, "amsterdam")
    london = conn.createURI(exns, "london")
    sanfrancisto = conn.createURI(exns, "sanfrancisto")
    salvador = conn.createURI(exns, "salvador")    
    latLongGeoType = conn.createLatLongSystem(scale=5) #, unit='km')
    location = conn.createURI(exns, "geolocation")
    conn.registerDatatypeMapping(predicate=location, nativeType="float")   
    conn.add(amsterdam, location, conn.createCoordinate(52.366665, 4.883333))
    conn.add(london, location, conn.createCoordinate(51.533333, 0.08333333))
    conn.add(sanfrancisto, location, conn.createCoordinate(37.783333, -122.433334)) 
    conn.add(salvador, location, conn.createCoordinate(-13.083333, -38.45))   
    circle2 = conn.createCircle(50, 0, 1000, unit='km')
    print circle2
    for r in conn.getStatements(None, location, circle2) : print r
    polygon2 = conn.createPolygon([(52.0, 4.88),(52.36, 5.0),(53.0,4.88),(52.36,4.6)])
    print polygon2
    for r in conn.getStatements(None, location, polygon2) : print r
    box3 = conn.createBox(51.0, 52.0, 0.08, 0.09) 
    print box3
    for r in conn.getStatements(None, location, box3) : print r


    
        
if __name__ == '__main__':
    choices = [i for i in range(1,3)]
    choices = [4]
    for choice in choices:
        print "\n==========================================================================="
        print "Test Run Number 10%i\n" % choice
        if choice == 0: test0()
        elif choice == 1: test101()
        elif choice == 2: test102()
        elif choice == 3: test103()
        elif choice == 4: test104()    
        elif choice == 5: test105()        
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
        else:
            print "No such test exists."
    
