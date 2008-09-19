# TODO creating stores, streaming cursors for queries, mass-delete/insert, contexts

import StringIO, pycurl, time, urllib, cjson

class ProtocolError(Exception):
    def __init__(self, status, message):
        self.status = status
        self.message = message
    def __str__(self):
        return "Server returned %d: %s" % (self.status, self.message)

def urlenc(**args):
    buf = []
    def enc(name, val):
        buf.append(urllib.quote(name) + "=" + urllib.quote(val))
    def encval(name, val):
        if val is None: pass
        elif val == True: enc(name, "true")
        elif val == False: enc(name, "false")
        elif hasattr(val, "append"):
            for elt in val: encval(name, elt)
        else: enc(name, val.encode("utf-8"))
    for name, val in args.iteritems():
        encval(name, val)
    return "&".join(buf)

def makeRequest(curl, method, url, body=None, accept="*/*", contentType=None):
    postbody = method == "POST" or method == "PUT"
    curl.setopt(pycurl.POSTFIELDS, "")
    if body:
        if postbody:
            curl.setopt(pycurl.POSTFIELDS, body)
        else:
            url = url + "?" + body

    curl.setopt(pycurl.POST, (postbody and 1) or 0)
    curl.setopt(pycurl.CUSTOMREQUEST, method)
    curl.setopt(pycurl.URL, url)

    headers = ["Connection: Keep-Alive", "Accept: " + accept]
    if contentType and postbody: headers.append("Content-Type: " + contentType)
    curl.setopt(pycurl.HTTPHEADER, headers)
    curl.setopt(pycurl.ENCODING, "") # which means 'any encoding that curl supports'
    buf = StringIO.StringIO()
    curl.setopt(pycurl.WRITEFUNCTION, buf.write)
    curl.perform()
    body = buf.getvalue().decode("utf-8")
    buf.close()
    return (curl.getinfo(pycurl.RESPONSE_CODE), body)

def jsonRequest(curl, method, url, body=None, contentType="application/x-www-form-urlencoded"):
    status, body = makeRequest(curl, method, url, body, "application/json", contentType)
    if (status == 200): return cjson.decode(body)
    else: raise ProtocolError(status, body)

def nullRequest(curl, method, url, body=None, contentType="application/x-www-form-urlencoded"):
    status, body = makeRequest(curl, method, url, body, "application/json", contentType)
    if (status != 200): raise ProtocolError(status, body)

class RepositoryServer:
    def __init__(self, url):
        self.url = url
        self.curl = pycurl.Curl()

    def repositories(self):
        return jsonRequest(self.curl, "GET", self.url + "/repositories")

    def getRepository(self, name):
        return Repository(self.curl, self.url + "/repositories/" + name)

class Repository:
    def __init__(self, curl, url):
        self.url = url
        self.curl = curl

    def size(self):
        return jsonRequest(self.curl, "GET", self.url + "/size")

    def contexts(self):
        return jsonRequest(self.curl, "GET", self.url + "/contexts")

    def sparqlQuery(self, query, infer=False, context=None):
        return jsonRequest(self.curl, "POST", self.url, urlenc(query=query, infer=infer, context=context))

    def prologQuery(self, query, infer=False):
        return jsonRequest(self.curl, "POST", self.url, urlenc(query=query, infer=infer, queryLn="prolog"))

    def textQuery(self, pattern, infer=False):
        return jsonRequest(self.curl, "GET", self.url + "/freetext", urlenc(pattern=pattern, infer=infer))

    def getStatements(self, subj=None, pred=None, obj=None, context=None, infer=False):
        return jsonRequest(self.curl, "GET", self.url + "/statements",
                           urlenc(subj=subj, pred=pred, obj=obj, context=context, infer=infer))

    def addStatement(self, subj, pred, obj, context=None):
        nullRequest(self.curl, "POST", self.url + "/statements",
                    urlenc(subj=subj, pred=pred, obj=obj, context=context))

    def delStatement(self, subj, pred, obj, context=None):
        nullRequest(self.curl, "DELETE", self.url + "/statements",
                    urlenc(subj=subj, pred=pred, obj=obj, context=context))

    # expects [["subj", "pred", "obj", "graph"]] data, where graph may be None
    def addStatements(self, quads):
        nullRequest(self.curl, "POST", self.url + "/statements/json", cjson.encode(quads));

    def delStatements(self, quads):
        nullRequest(self.curl, "POST", self.url + "/statements/json/delete", cjson.encode(quads));

    def listIndices(self):
        return jsonRequest(self.curl, "GET", self.url + "/indices")

    def addIndex(self, type):
        nullRequest(self.curl, "POST", self.url + "/indices", urlenc(type=type))

    def delIndex(self, type):
        nullRequest(self.curl, "DELETE", self.url + "/indices", urlenc(type=type))

    def listFreetextIndices(self):
        return jsonRequest(self.curl, "GET", self.url + "/freetextindices")

    def addFreetextIndex(self, predicate):
        nullRequest(self.curl, "POST", self.url + "/freetextindices", urlenc(predicate=predicate))

def timeQuery(rep):
    n = 100
    size = 500
    t = time.time()
    for i in range(n):
        rep.sparqlQuery("select ?x ?y ?z {?x ?y ?z} limit %d" % size)
    print "Did %d %d-row queries in %f seconds." % (n, size, time.time() - t)

server = RepositoryServer("http://localhost:8080")
reps = server.repositories()
if len(reps) > 0:
    print "Found repositories " + repr(reps) + ", opening " + reps[0]
    rep = server.getRepository(reps[0])
    print "Repository size = %d" % rep.size()
    timeQuery(rep)
