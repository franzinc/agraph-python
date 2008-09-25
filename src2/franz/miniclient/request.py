import StringIO, pycurl, urllib, cjson

class RequestError(Exception):
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
    else: raise RequestError(status, body)

def nullRequest(curl, method, url, body=None, contentType="application/x-www-form-urlencoded"):
    status, body = makeRequest(curl, method, url, body, "application/json", contentType)
    if (status != 200): raise RequestError(status, body)

