# This file is a helper file for Example019 'Using JSON-LD' of
# The AllegroGraph Python tutorial. In this file, there are
# definitions of functions used in the tutorial and also definitions
# of objects used for a dictionary example. The functions and
# objects are only used by Example019. This file is not required for
# any other example.
#

from franz.openrdf.sail.allegrographserver import AllegroGraphServer
from franz.openrdf.connect import ag_connect
from franz.openrdf.vocabulary.xmlschema import XMLSchema
from franz.openrdf.rio.rdfformat import RDFFormat
from franz.openrdf.repository.repository import Repository
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

import os, urllib, datetime, time, sys, json, requests, copy
from pprint import pprint # will let you use the python pretty printer pprint(el)

# This function defines an abbreviation for a prefix
def addNamespace(conn,abbr,namespace):
    conn.setNamespace(abbr,namespace)
    PREFIXES=allPrefixes(conn)

# creating prefixes for SPARQL

# This function creates a string of default and user-defined
# prefixes suitable for prepending to a SPARQL query
def allPrefixes(conn):
    prefs=''
    for key,value in conn.getNamespaces().items():
        prefs = prefs + "prefix " + key + ":<" + value + ">" + os.linesep
    return prefs


PREFIXES = None

# SPARQL returns binding sets. This function
# turns such sets into lists
def bindingSetToList(bs,bindingnames,format='terse'):
    list = []
    for n in bindingnames:
       val = bs.getValue(n)
       list.append(partToString(val,format))
    return list

# SPARQL returns binding sets. This function
# turns such a set into a dictionary
def bindingSetToDict (bs,bindingnames,format='terse'):
    d = {}
    for n in bindingnames:
       val = bs.getValue(n)
       d[n] = partToString(val,format)
    return d

# This function turns return values from SPARQL into a terse
# string or a full N-Triple string
def partToString(var,format='terse'):
   typ = var.__class__.__name__
   if typ == 'Literal' : return var.toPython()
   if typ == 'URI' :
      if format=='terse': 
         return var.localname
      elif format =='ntriples':
         return str(var.toNTriples())
      else:
         print ("ERROR" )
            
# This function runs a SPARQL command and returns
# the results as a dictionary
def runSparql (conn,str,limit=10,format='terse',resultsFormat='dict'):
    global PREFIXES
    if PREFIXES: str = PREFIXES + str
    else: 
        PREFIXES = allPrefixes(conn)
        str = PREFIXES + str          
    query = conn.prepareTupleQuery(QueryLanguage.SPARQL, str)
    list = []
    start = True
    if resultsFormat=='dict': 
        fun = bindingSetToDict
    else: fun = bindingSetToList
    with query.evaluate() as result:
        for bindingSet in result:
            if start:
                bindingnames = bindingSet.getBindingNames()
                start = False
            list.append(fun(bindingSet,bindingnames,format))
    return list

# This function stores a dictionary as a JSON-LD object.
# in AllegroGraph. If object is a list, then each element is stored.
def store(conn,obj, commit=True, fresh=False):
    if fresh: conn.clear(); conn.commit()
    conn.addData(obj, allow_external_references=True, json_ld_store_source=True)
    if commit: conn.commit()

# The function STORE stores JSON-LD objects both as triples and as a blob
# connected to the root. In this function, the source for each matching 
# ?this is retrieved and converted into a corresponding Python object
# (a dictionary or an arrays of dictionaries). An array of these 
# converted objects is returned.
#
# The 'where' argument should be the QUERY block of a SPARQL query.
# The query block must contain the variable ?this.
def retrieve(conn,where,printResult=False):
    thisString = """select distinct ?this ?source {
                     ?this <http://franz.com/ns/allegrograph/6.4/load-meta#source> ?source . """
    pos = where.find('{')
    assert ( pos != -1 ), """The query block needs a '{' in the pattern"""
    assert ( where.find('?this') != -1 ), """The query block needs to include the variable ?this"""
    pos = pos + 1
    where = thisString + where[pos:]
    result = runSparql(conn,where)
    result = [json.loads(el['source']) for el in result]
    if printResult:
       for el in result: pprint(el)
    return result 



## We now define a collection of different objects, 
## mostly from the json-ld.org website.



obs = {}
contexts = {}

# event

obs[1] = {
  "@context": {
    "ical": "http://www.w3.org/2002/12/cal/ical#",
    "xsd": "http://www.w3.org/2001/XMLSchema#",
    "ical:dtstart": {"@type": "xsd:dateTime" }
      },
    "@id": "ical:event1",
    "@type": "ical:Event",
    "ical:summary": "Lady Gaga Concert",
    "ical:location": "New Orleans Arena, New Orleans, Louisiana, USA",
    "ical:dtstart": "2011-04-09T20:00:00Z"
}

# person

obs[2] = {
  "@context": "http://schema.org/",
  "@type": "Person",
  "@id": "foaf:person-1000",
  "name": "Jane Doe",
  "jobTitle": "Professor",
  "telephone": "(425) 123-4567",
  "url": "http://www.janedoe.com"
}


contexts[3] = { "foaf:child": {"@type":"@id"},
            "foaf:brotherOf": {"@type":"@id"},
            "foaf:birthdate": {"@type": "xsd:dateTime" } }

# a graph of three persons.

obs[4] = {
    "@context": contexts[3],
    "@type":"foaf:Person",
    "@id":"foaf:person-1",
    "foaf:birthdate": "1958-04-09T20:00:00Z",
    "foaf:child": ['foaf:person-2', 'foaf:person-3']
}

obs[5] = {
    "@context": contexts[3],
    "@type":"foaf:Person",
    "@id":"foaf:person-2",
    "foaf:brotherOf": "foaf:person-3",
    "foaf:birthdate": "1992-04-09T20:00:00Z",
}

obs[6] = {"@context": contexts[3],
    "@type":"foaf:Person",
    "@id":"foaf:person-3",
    "foaf:birthdate": "1994-04-09T20:00:00Z",
}

# hippie oil

obs[7] = {"@context":"http://schema.org",
 "@type":"Product",
 "@id":"http://franz.com/hippieoil",
 "aggregateRating": 
    {"@type":"AggregateRating",
     "ratingValue":4.6,
     "reviewCount":66},
     "description":"""Make peace with your inner hippie while hydrating & protecting against photoaging....Mad Hippie's preservative-free Antioxidant Facial Oil is truly the most natural way to moisturize.""",
     "brand":"Mad Hippie",
     "name":"Antioxidant Facial Oil",
     "image":"https://images.ulta.com/is/image/Ulta/2530018",
     "productID":"2530018",
     "offers": 
        {"@type":"Offer",
         "availability":"http://schema.org/InStock",
         "price":"24.99",
         "priceCurrency":"USD"}}

# a place

contexts[8] = {
        "name": "http://schema.org/name",
        "description": "http://schema.org/description",
        "image": {
            "@id": "http://schema.org/image", "@type": "@id" },
        "geo": "http://schema.org/geo",
        "latitude": {
            "@id": "http://schema.org/latitude", "@type": "xsd:float" },
        "longitude": {
            "@id": "http://schema.org/longitude",  "@type": "xsd:float" },
        "xsd": "http://www.w3.org/2001/XMLSchema#"
    }

obs[9] = {
    "@context": contexts[8],
    "@id": "http://franz.com/place1", 
    "@graph": {
        "@id": "http://franz.com/place1", 
        "@type": "http://franz.com/Place",
        "name": "The Empire State Building",
        "description": "The Empire State Building is a 102-story landmark in New York City.",
        "image": "http://www.civil.usherbrooke.ca/cours/gci215a/empire-state-building.jpg",
        "geo": {
               "latitude": "40.75",
               "longitude": "73.98" }
        }}

# a product

obs[10] = {
  "@context": {
    "gr": "http://purl.org/goodrelations/v1#",
    "pto": "http://www.productontology.org/id/",
    "foaf": "http://xmlns.com/foaf/0.1/",
    "xsd": "http://www.w3.org/2001/XMLSchema#",
    "foaf:page": {
      "@type": "@id"
    },
    "gr:acceptedPaymentMethods": {
      "@type": "@id"
    },
    "gr:hasBusinessFunction": {
      "@type": "@id"
    },
    "gr:hasCurrencyValue": {
      "@type": "xsd:float"
    }
  },
  "@id": "http://example.org/cars/for-sale#tesla",
  "@type": "gr:Offering",
  "gr:name": "Used Tesla Roadster",
  "gr:description": "Need to sell fast and furiously",
  "gr:hasBusinessFunction": "gr:Sell",
  "gr:acceptedPaymentMethods": "gr:Cash",
  "gr:hasPriceSpecification": {
    "gr:hasCurrencyValue": "85000",
    "gr:hasCurrency": "USD"
  },
  "gr:includes": {
    "@type": [
      "gr:Individual",
      "pto:Vehicle"
    ],
    "gr:name": "Tesla Roadster",
    "foaf:page": "http://www.teslamotors.com/roadster"
  }
}

# a recipe

obs[11] = {
  "@context": {
    "name": "http://rdf.data-vocabulary.org/#name",
    "ingredient": "http://rdf.data-vocabulary.org/#ingredients",
    "yield": "http://rdf.data-vocabulary.org/#yield",
    "instructions": "http://rdf.data-vocabulary.org/#instructions",
    "step": {
      "@id": "http://rdf.data-vocabulary.org/#step",
      "@type": "xsd:integer"
    },
    "description": "http://rdf.data-vocabulary.org/#description",
    "xsd": "http://www.w3.org/2001/XMLSchema#"
  },
 # added in type and id
    "@id": "http://franz.com/cocktail",
    "@type": "http://franz.com/Cocktail",
    "name": "Mojito",
    "ingredient": [
        "12 fresh mint leaves",
        "1/2 lime, juiced with pulp",
        "1 tablespoons white sugar",
        "1 cup ice cubes",
        "2 fluid ounces white rum",
        "1/2 cup club soda"
        ],
    "yield": "1 cocktail",
    "instructions": [
      {
        "step": 1,
        "description": "Crush lime juice, mint and sugar together in glass."
      },
      {
        "step": 2,
        "description": "Fill glass to top with ice cubes."
      },
      {
        "step": 3,
        "description": "Pour white rum over ice."
      },
      {
        "step": 4,
        "description": "Fill the rest of glass with club soda, stir."
      },
      {
        "step": 5,
        "description": "Garnish with a lime wedge."
      }
    ]
}































