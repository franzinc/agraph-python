import StringIO, pycurl, urllib, cjson, locale

class RequestError(Exception):
    def __init__(self, status, message):
        print status, message
        self.status = status
        self.message = message
    def __str__(self):
        return "Server returned %s: %s" % (self.status, self.message)

def urlenc(**args):
    buf = []
    def enc(name, val):
        buf.append(urllib.quote(name) + "=" + urllib.quote(val))
    def encval(name, val):
        if val is None: pass
        elif val == True: enc(name, "true")
        elif val == False: enc(name, "false")
        elif isinstance(val, int): enc(name, "%d" % val)
        elif isinstance(val, list):
            for elt in val: encval(name, elt)
        else: enc(name, val.encode("utf-8"))
    for name, val in args.iteritems():
        encval(name, val)
    return "&".join(buf)

def makeRequest(curl, method, url, body=None, accept="*/*", contentType=None, callback=None, errCallback=None):
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

    if callback:
        status = [None]
        error = []
        def headerfunc(string):
            if status[0] is None:
                status[0] = locale.atoi(string.split(" ")[1])
            return len(string)
        def writefunc(string):
            if status[0] == 200: callback(string)
            else: error.append(string.decode("utf-8"))
        curl.setopt(pycurl.WRITEFUNCTION, writefunc)
        curl.setopt(pycurl.HEADERFUNCTION, headerfunc)
        curl.perform()
        if status[0] != 200:
            errCallback(curl.getinfo(pycurl.RESPONSE_CODE), "".join(error))
    else:
        buf = StringIO.StringIO()
        curl.setopt(pycurl.WRITEFUNCTION, buf.write)
        curl.perform()
        response = buf.getvalue().decode("utf-8")
        buf.close()
        return (curl.getinfo(pycurl.RESPONSE_CODE), response)

def jsonRequest(curl, method, url, body=None, contentType="application/x-www-form-urlencoded", rowreader=None):
    if rowreader is None:
        status, body = makeRequest(curl, method, url, body, "application/json", contentType)
        if (status == 200): return cjson.decode(body)
        else: raise RequestError(status, body)
    else:
        def raiseErr(status, message): raise RequestError(status, message)
        makeRequest(curl, method, url, body, "application/json", contentType, callback=rowreader.process, errCallback=raiseErr)

def nullRequest(curl, method, url, body=None, contentType="application/x-www-form-urlencoded"):
    status, body = makeRequest(curl, method, url, body, "application/json", contentType)
    if (status != 200): raise RequestError(status, body)

class RowReader:
    def __init__(self, callback):
        self.hasNames = None
        self.names = None
        self.skipNextBracket = False
        self.callback = callback
        self.backlog = None

    def process(self, string):
        if self.hasNames is None: self.hasNames = string[0] == "{"
        ln = len(string)
        if self.backlog: string = self.backlog + string
        pos = [0]

        def useArray(arr):
            if self.hasNames:
                if self.names:
                    self.callback(arr, self.names)
                else:
                    self.names = arr
                    self.skipNextBracket = True
            else:
                self.callback(arr, None)

        def takeArrayAt(start):
            scanned = start + 1
            while True:
                end = string.find("]", scanned)
                if end == -1: return False
                try:
                    useArray(cjson.decode(string[start : end + 1].decode("utf-8")))
                    pos[0] = end + 1
                    return True
                except cjson.DecodeError:
                    scanned = end + 1

        while True:
            start = string.find("[", pos[0])
            if self.skipNextBracket:
                self.skipNextBracket = False
                pos[0] = start + 1
            elif start == -1 or not takeArrayAt(start):
                break

        if pos[0] == 0:
            self.backlog = string
            return ln
        else:
            self.backlog = None
            return pos[0]
