
import os
import struct
from franz.util import integer_to_binary
#from franz.startup import StartUp
#from franz.agconnector import AGConnector

from franz.util import *
from franz.allegrograph.startup import StartUp
from franz.transport.agconnection import AllegroGraphConnection


def test1():
    x = 3.3
    print "SIZE " , struct.calcsize('d')
    print "FLT: " , float2raw_int(x)
    print "DBL: ", float2raw_long(x)
    print "ORIGINAL ", long_bits_to_float(float2raw_long(x))
    print "ENVIRON " + str(os.environ)
    print "UNAME " + str(os.uname())
    print "ROOT " + os.path.abspath('/')
    #print "GLOBAL DICT" + 

def test2():
    print "ENVIRON ", os.environ
    print "LISP ", os.getenv("com.franz.ag.exec")
    print "LEN ", len("abcd")

def test3():
    StartUp.startUpTripleStore(
                AllegroGraphConnection.RENEW,
                "localhost", "/temp/test", "/Users/bmacgregor/Desktop/AGFolder", [])

def test4():
    agStore = StartUp.startUpTripleStore(
                AllegroGraphConnection.RENEW,
                "localhost", "test", "/Users/bmacgregor/Desktop/AGFolder", [])
    s = agStore.createURI("<http://www.franz.com/demo#jans>");
    p = agStore.createURI("rdf:type");
    o = agStore.createURI("http://www.franz.com/demo#Person");        
    agStore.addStatement(s, p, o)
    print "Triple count ", agStore.getUnindexedTripleCount()
    query = """select ?s ?p ?o where {?s ?p ?o .}"""
    rows = agStore.twinqlSelect(False, query, None, 0, 0, [])
    for r in rows:
        print r[0], r[1], r[2]

def test5():
    print character_to_integer('c'), " ", ord('c')
    print character_to_integer(' '), " ", ord(' ')
    
    

def test6():
    pass

if __name__ == '__main__':
    test = 5
    if test == 1: test1()
    elif test == 2: test2()
    elif test == 3: test3()
    elif test == 4: test4()
    elif test == 5: test5()
    elif test == 6: test6()    
