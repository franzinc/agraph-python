###############################################################################
# Copyright (c) 2006-2009 Franz Inc.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the Eclipse Public License v1.0
# which accompanies this distribution, and is available at
# http://www.eclipse.org/legal/epl-v10.html
###############################################################################

import repository, re
from os import environ
from request import RequestError, encode, decode, serialize, deserialize

from nose.tools import with_setup, eq_ as eq

url = "http://%s:%d" % (environ.get('AGRAPH_HOST', 'localhost'),
                        int(environ.get('AGRAPH_PORT', '10035')))

client = repository.Client(url, "test", "xyzzy")
cat = client.openCatalogByName("tests")

try:
    rep = cat.createRepository('foo')
except RequestError:
    rep = cat.getRepository('foo')

def testPreconditions():
  assert 'tests' in client.listCatalogs()
  if "foo" in cat.listRepositories():
      cat.deleteRepository("foo")
  cat.createRepository("foo")

def cleanup():
  rep.deleteMatchingStatements()

@with_setup(cleanup)
def testBasics():
  rep.addStatement("<a>", "<p>", '"a"', "<c1>")
  rep.addStatement("<b>", "<p>", '"b"', "<c2>")
  eq(["<c1>", "<c2>"], sorted(rep.listContexts()))
  eq(1, len(rep.getStatements(context="<c1>")))
  eq(2, len(rep.getStatements(pred="<p>")))
  eq(2, len(rep.getStatements(subj=["<a>", "<b>"])))
  eq(0, len(rep.getStatements(subj=["<x>", "<y>"])))
  rep.deleteMatchingStatements(obj='"a"')
  eq(1, rep.getSize())

@with_setup(cleanup)
def testSparql():
  for x in range(0, 20):
    rep.addStatement("<http:a>", "<http:p>", '"%d"' % x, "<http:c1>")
    if x % 2 == 0:
      rep.addStatement("<http:b>", "<http:p>", '"%d"' % x, "<http:c2>")
  eq([["<http:a>"]], rep.evalSparqlQuery("select ?x {?x <http:p> \"1\"}")["values"])
  eq(2, len(rep.evalSparqlQuery("select ?x {?x ?y \"2\"}")["values"]))
  eq(10, len(rep.evalSparqlQuery("select ?x {?x <http:p> ?y}", context="<http:c2>")["values"]))
  eq(True, rep.evalSparqlQuery("ask {<http:a> ?x \"19\"}"))
  eq(False, rep.evalSparqlQuery("ask {<http:a> ?x \"20\"}"))

def cleanup_prolog():
    client.setInitfile(None)

@with_setup(cleanup, cleanup_prolog)
def testProlog():
  for x in range(0, 10):
    rep.addStatement("<http:%d>" % x, "<http:before>", "<http:%d>" % (x + 1))
    rep.addStatement("<http:%d>" % (x + 1), "<http:after>", "<http:%d>" % x)
  eq([["<http:2>"]], rep.evalPrologQuery("(select (?x) (q- ?x !<http:before> !<http:3>))")["values"])
  client.setInitfile("(<-- (after-after ?a ?b) (q- ?a !<http:after> ?x) (q- ?x !<http:after> ?b))")
  eq([["<http:5>"]], rep.evalPrologQuery("(select (?x) (after-after ?x !<http:3>))")["values"])

@with_setup(cleanup)
def testGeo():
  typ = rep.getCartesianGeoType(1, -100, 100, -100, 100)
  def pt(x, y): return rep.createCartesianGeoLiteral(typ, x, y)
  rep.addStatement('"foo"', '"at"', pt(1, 1))
  rep.addStatement('"bar"', '"at"', pt(-2.5, 3.4))
  rep.addStatement('"baz"', '"at"', pt(-1, 1))
  rep.addStatement('"bug"', '"at"', pt(10, -2.421553215))
  eq(['"bar"', '"baz"'], sorted([x[0] for x in rep.getStatementsInsideBox(typ, '"at"', -10, 0, 0, 10)]))
  eq(['"baz"', '"foo"'], sorted([x[0] for x in rep.getStatementsInsideCircle(typ, '"at"', 0, 0, 2)]))
  rep.createPolygon('"right"', [pt(0, -100), pt(0, 100), pt(100, 100), pt(100, -100)])
  eq(['"bug"', '"foo"'], sorted([x[0] for x in rep.getStatementsInsidePolygon(typ, '"at"', '"right"')]))
  typ2 = rep.getSphericalGeoType(5)
  def pp(lat, lon): return rep.createSphericalGeoLiteral(typ2, lat, lon)
  rep.addStatement('"Amsterdam"', '"loc"', pp(52.366665, 4.883333))
  rep.addStatement('"London"', '"loc"', pp(51.533333, 0.08333333))
  rep.addStatement('"San Francisco"', '"loc"', pp(37.783333, -122.433334))
  rep.addStatement('"Salvador"', '"loc"', pp(-13.083333, -38.45))
  eq(['"Amsterdam"', '"London"'], sorted([x[0] for x in rep.getStatementsHaversine(typ2, '"loc"', 50, 0, 1000)]))
  eq(['"London"'], [x[0] for x in rep.getStatementsInsideBox(typ2, '"loc"', 0.08, 0.09, 51.0, 52.0)])

@with_setup(cleanup)
def testSession():
    eq(0, rep.getSize())
    rep.openSession()
    rep.addStatement("<a>", "<b>", "<c>")
    eq(1, rep.getSize())
    rep.closeSession()
    eq(0, rep.getSize())
    rep.openSession()
    rep.addStatement("<a>", "<b>", "<c>")
    rep.definePrologFunctors("(<-- (b ?a ?b) (q- ?a !<b> ?b))")
    rep.evalPrologQuery("(select (?x) (b ?x !<c>))")
    rep.commit()
    rep.closeSession()
    eq(1, rep.getSize())

@with_setup(cleanup)
def testTemporal():
    rep.addMappedType("<time>", "<http://www.w3.org/2001/XMLSchema#dateTime>")
    rep.addStatement("<x>", "<happened>", "\"2009-09-28T17:41:39\"^^<time>")
    rep.addStatement("<y>", "<happened>", "\"2009-09-28T18:22:00\"^^<time>")
    rep.addStatement("<z>", "<happened>", "\"2009-09-28T17:02:41\"^^<time>")
    eq(2, len(rep.getStatements(obj=("\"2009-09-28T17:00:00\"^^<time>", "\"2009-09-28T18:00:00\"^^<time>"))))

@with_setup(cleanup)
def testFreeText():
    rep.addStatement("<x>", "<p>", "\"foo bar quux rhubarb\"")
    rep.addStatement("<y>", "<q>", "\"foo bar quux rhubarb\"")
    rep.addStatement("<z>", "<p>", "\"foo bar quux rhubarb\"^^<type1>")
    eq(len(rep.evalFreeTextSearch("quux")), 0)
    rep.createFreeTextIndex("index", predicates=["<p>"], minimumWordSize=4)
    eq(rep.listFreeTextIndices(), ["index"])
    eq(len(rep.evalFreeTextSearch("foo")), 0)
    eq(len(rep.evalFreeTextSearch("quux")), 2)
    rep.modifyFreeTextIndex("index", indexLiterals=["<type1>"])
    eq(len(rep.evalFreeTextSearch("rhubarb")), 1)
    rep.deleteFreeTextIndex("index")
    eq(rep.listFreeTextIndices(), [])

def test_stored_proc_args():
    orig = [1, 2, "3", "4", "5", [1, [1, "2", 3], "3"]]
    serial = serialize(orig)
    enc = encode(serial)
    assert serial == decode(enc)
    assert orig == deserialize(serial)
    
