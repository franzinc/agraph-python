import repository, re

# Preconditions: HTTP server running on port 10034, catalog named
# testcatalog present, store named foo in it. User test:xyzzy has full
# access to this store.

url = "http://localhost:10035"
cat = repository.openCatalog(url, "/catalogs/testcatalog", "test", "xyzzy")
rep = cat.getRepository("foo")

def cleanup(f):
  def fn():
    try: return f()
    finally: rep.deleteMatchingStatements()
  return fn

def testPreconditions():
  checkIf("http://localhost:10035/catalogs/testcatalog" in repository.listCatalogs(url, "test", "xyzzy"))
  checkIf("foo" in cat.listRepositories())
  check(0, rep.getSize())

@cleanup
def testBasics():
  rep.addStatement("<a>", "<p>", '"a"', "<c1>")
  rep.addStatement("<b>", "<p>", '"b"', "<c2>")
  check(2, rep.getSize())
  check(["<c1>", "<c2>"], sorted(rep.listContexts()))
  check(1, len(rep.getStatements(context="<c1>")))
  check(2, len(rep.getStatements(pred="<p>")))
  check(2, len(rep.getStatements(subj=["<a>", "<b>"])))
  check(0, len(rep.getStatements(subj=["<x>", "<y>"])))
  rep.deleteMatchingStatements(obj='"a"')
  check(1, rep.getSize())

@cleanup
def testSparql():
  for x in range(0, 20):
    rep.addStatement("<http:a>", "<http:p>", '"%d"' % x, "<http:c1>")
    if x % 2 == 0:
      rep.addStatement("<http:b>", "<http:p>", '"%d"' % x, "<http:c2>")
  check([["<http:a>"]], rep.evalSparqlQuery("select ?x {?x <http:p> \"1\"}")["values"])
  check(2, len(rep.evalSparqlQuery("select ?x {?x ?y \"2\"}")["values"]))
  check(10, len(rep.evalSparqlQuery("select ?x {?x <http:p> ?y}", context="<http:c2>")["values"]))
  check(True, rep.evalSparqlQuery("ask {<http:a> ?x \"19\"}"))
  check(False, rep.evalSparqlQuery("ask {<http:a> ?x \"20\"}"))

@cleanup
def testProlog():
  for x in range(0, 10):
    rep.addStatement("<http:%d>" % x, "<http:before>", "<http:%d>" % (x + 1))
    rep.addStatement("<http:%d>" % (x + 1), "<http:after>", "<http:%d>" % x)
  check([["<http:2>"]], rep.evalPrologQuery("(select ?x (q- ?x !<http:before> !<http:3>))")["values"])
  rep.definePrologFunctors("(<-- (after-after ?a ?b) (q- ?a !<http:after> ?x) (q- ?x !<http:after> ?b))")
  check([["<http:5>"]], rep.evalPrologQuery("(select ?x (after-after ?x !<http:3>))")["values"])

@cleanup
def testGeo():
  typ = rep.getCartesianGeoType(1, -100, 100, -100, 100)
  def pt(x, y): return rep.createCartesianGeoLiteral(typ, x, y)
  rep.addStatement('"foo"', '"at"', pt(1, 1))
  rep.addStatement('"bar"', '"at"', pt(-2.5, 3.4))
  rep.addStatement('"baz"', '"at"', pt(-1, 1))
  rep.addStatement('"bug"', '"at"', pt(10, -2.421553215))
  check(['"bar"', '"baz"'], sorted([x[0] for x in rep.getStatementsInsideBox(typ, '"at"', -10, 0, 0, 10)]))
  check(['"baz"', '"foo"'], sorted([x[0] for x in rep.getStatementsInsideCircle(typ, '"at"', 0, 0, 2)]))
  rep.createPolygon('"right"', [pt(0, -100), pt(0, 100), pt(100, 100), pt(100, -100)])
  check(['"bug"', '"foo"'], sorted([x[0] for x in rep.getStatementsInsidePolygon(typ, '"at"', '"right"')]))
  typ2 = rep.getSphericalGeoType(5)
  def pp(lat, lon): return rep.createSphericalGeoLiteral(typ2, lat, lon)
  rep.addStatement('"Amsterdam"', '"loc"', pp(52.366665, 4.883333))
  rep.addStatement('"London"', '"loc"', pp(51.533333, 0.08333333))
  rep.addStatement('"San Francisco"', '"loc"', pp(37.783333, -122.433334))
  rep.addStatement('"Salvador"', '"loc"', pp(-13.083333, -38.45))
  check(['"Amsterdam"', '"London"'], sorted([x[0] for x in rep.getStatementsHaversine(typ2, '"loc"', 50, 0, 1000)]))
  check(['"London"'], [x[0] for x in rep.getStatementsInsideBox(typ2, '"loc"', 0.08, 0.09, 51.0, 52.0)])

# Framework

curTestName = None
fail = None

def check(wanted, got):
  global fail
  if wanted != got:
    print "%s: %s != %s" % (curTestName, wanted, got)
    fail = True

def checkIf(val):
  global fail
  if not val:
    print "%s: assertion failed" % curTestName
    fail = True

def runTests():
  global curTestName, fail
  succs = 0
  fails = 0
  print "Running tests..."
  for (name, val) in globals().items():
    curTestName = name
    fail = False
    if re.match("^test", name):
      try: val()
      except Exception, e:
        print "%s: raised %s" % (name, str(e))
        fail = True
      if fail: fails += 1
      else: succs += 1
  print "Finished running.\n %d success\n %d failures." % (succs, fails)

runTests()
