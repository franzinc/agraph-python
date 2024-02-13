################################################################################
# Copyright (c) 2006-2017 Franz Inc.
# All rights reserved. This program and the accompanying materials are
# made available under the terms of the MIT License which accompanies
# this distribution, and is available at http://opensource.org/licenses/MIT
################################################################################
from franz.miniclient import repository
from franz.miniclient.request import (
    RequestError,
    decode,
    deserialize,
    encode,
    serialize,
)
from franz.openrdf.tests.conftest import min_version
from franz.openrdf.tests.tests import (
    AG_HOST,
    AG_PORT,
    AG_PROXY,
    CATALOG,
    PASSWORD,
    USER,
)

url = "http://%s:%d" % (AG_HOST, AG_PORT)

client = repository.Client(url, USER, PASSWORD, proxy=AG_PROXY)
cat = client.openCatalogByName(CATALOG)

try:
    rep = cat.createRepository("foo")
except RequestError:
    rep = cat.getRepository("foo")


def testPreconditions():
    assert CATALOG in client.listCatalogs()
    if "foo" in cat.listRepositories():
        cat.deleteRepository("foo")
    cat.createRepository("foo")


def testBasics():
    rep.deleteMatchingStatements()
    rep.addStatement("<a>", "<p>", '"a"', "<c1>")
    rep.addStatement("<b>", "<p>", '"b"', "<c2>")
    assert ["<c1>", "<c2>"] == sorted(rep.listContexts())
    assert 1 == len(rep.getStatements(context="<c1>"))
    assert 2 == len(rep.getStatements(pred="<p>"))
    assert 2 == len(rep.getStatements(subj=["<a>", "<b>"]))
    assert 0 == len(rep.getStatements(subj=["<x>", "<y>"]))
    rep.deleteMatchingStatements(obj='"a"')
    assert 1 == rep.getSize()


def testSparql():
    rep.deleteMatchingStatements()
    for x in range(0, 20):
        rep.addStatement("<http:a>", "<http:p>", '"%d"' % x, "<http:c1>")
        if x % 2 == 0:
            rep.addStatement("<http:b>", "<http:p>", '"%d"' % x, "<http:c2>")
    assert [["<http:a>"]] == rep.evalSparqlQuery('select ?x {?x <http:p> "1"}')[
        "values"
    ]
    assert 2 == len(rep.evalSparqlQuery('select ?x {?x ?y "2"}')["values"])
    assert 10 == len(
        rep.evalSparqlQuery("select ?x {?x <http:p> ?y}", context="<http:c2>")["values"]
    )
    assert True == rep.evalSparqlQuery('ask {<http:a> ?x "19"}')
    assert False == rep.evalSparqlQuery('ask {<http:a> ?x "20"}')


def testProlog():
    rep.deleteMatchingStatements()
    client.setInitfile(None)
    for x in range(0, 10):
        rep.addStatement("<http:%d>" % x, "<http:before>", "<http:%d>" % (x + 1))
        rep.addStatement("<http:%d>" % (x + 1), "<http:after>", "<http:%d>" % x)
    assert [["<http:2>"]] == rep.evalPrologQuery(
        "(select (?x) (q- ?x !<http:before> !<http:3>))"
    )["values"]
    client.setInitfile(
        "(<-- (after-after ?a ?b) (q- ?a !<http:after> ?x) (q- ?x !<http:after> ?b))"
    )
    assert [["<http:5>"]] == rep.evalPrologQuery(
        "(select (?x) (after-after ?x !<http:3>))"
    )["values"]


def testGeo():
    rep.deleteMatchingStatements()
    typ = rep.getCartesianGeoType(1, -100, 100, -100, 100)

    def pt(x, y):
        return rep.createCartesianGeoLiteral(typ, x, y)

    rep.addStatement('"foo"', '"at"', pt(1, 1))
    rep.addStatement('"bar"', '"at"', pt(-2.5, 3.4))
    rep.addStatement('"baz"', '"at"', pt(-1, 1))
    rep.addStatement('"bug"', '"at"', pt(10, -2.421553215))
    assert ['"bar"', '"baz"'] == sorted(
        [x[0] for x in rep.getStatementsInsideBox(typ, '"at"', -10, 0, 0, 10)]
    )
    assert ['"baz"', '"foo"'] == sorted(
        [x[0] for x in rep.getStatementsInsideCircle(typ, '"at"', 0, 0, 2)]
    )
    rep.createPolygon('"right"', [pt(0, -100), pt(0, 100), pt(100, 100), pt(100, -100)])
    assert ['"bug"', '"foo"'] == sorted(
        [x[0] for x in rep.getStatementsInsidePolygon(typ, '"at"', '"right"')]
    )
    typ2 = rep.getSphericalGeoType(5)

    def pp(lat, lon):
        return rep.createSphericalGeoLiteral(typ2, lat, lon)

    rep.addStatement('"Amsterdam"', '"loc"', pp(52.366665, 4.883333))
    rep.addStatement('"London"', '"loc"', pp(51.533333, 0.08333333))
    rep.addStatement('"San Francisco"', '"loc"', pp(37.783333, -122.433334))
    rep.addStatement('"Salvador"', '"loc"', pp(-13.083333, -38.45))
    assert ['"Amsterdam"', '"London"'] == sorted(
        [x[0] for x in rep.getStatementsHaversine(typ2, '"loc"', 50, 0, 1000)]
    )
    assert ['"London"'] == [
        x[0] for x in rep.getStatementsInsideBox(typ2, '"loc"', 0.08, 0.09, 51.0, 52.0)
    ]


def testSession():
    rep.deleteMatchingStatements()
    assert 0 == rep.getSize()
    rep.openSession()
    rep.addStatement("<a>", "<b>", "<c>")
    assert 1 == rep.getSize()
    rep.closeSession()
    assert 0 == rep.getSize()
    rep.openSession()
    rep.addStatement("<a>", "<b>", "<c>")
    rep.definePrologFunctors("(<-- (b ?a ?b) (q- ?a !<b> ?b))")
    rep.evalPrologQuery("(select (?x) (b ?x !<c>))")
    rep.commit()
    rep.closeSession()
    assert 1 == rep.getSize()


def testTemporal():
    rep.deleteMatchingStatements()
    rep.addMappedType("<time>", "<http://www.w3.org/2001/XMLSchema#dateTime>")
    rep.addStatement("<x>", "<happened>", '"2009-09-28T17:41:39"^^<time>')
    rep.addStatement("<y>", "<happened>", '"2009-09-28T18:22:00"^^<time>')
    rep.addStatement("<z>", "<happened>", '"2009-09-28T17:02:41"^^<time>')
    assert 2 == len(
        rep.getStatements(
            obj=('"2009-09-28T17:00:00"^^<time>', '"2009-09-28T18:00:00"^^<time>')
        )
    )


@min_version(6, 7)
def testFreeText():
    rep.deleteMatchingStatements()
    rep.addStatement("<x>", "<p>", '"foo bar quux rhubarb"')
    rep.addStatement("<y>", "<q>", '"foo bar quux rhubarb"')
    rep.addStatement("<z>", "<p>", '"foo bar quux rhubarb"^^<type1>')
    rep.createFreeTextIndex("index", predicates=["<p>"], minimumWordSize=4)
    assert rep.listFreeTextIndices() == ["index"]
    assert len(rep.evalFreeTextSearch("foo")) == 0
    assert len(rep.evalFreeTextSearch("quux")) == 2
    rep.modifyFreeTextIndex("index", indexLiterals=["type1"])
    assert len(rep.evalFreeTextSearch("rhubarb")) == 1
    rep.deleteFreeTextIndex("index")
    assert rep.listFreeTextIndices() == []


def test_stored_proc_args():
    orig = [1, 2, "3", "4", "5", [1, [1, "2", 3], "3"]]
    serial = serialize(orig)
    enc = encode(serial)
    assert serial == decode(enc)
    assert orig == deserialize(serial)
