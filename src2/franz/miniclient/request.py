import StringIO, pycurl, urllib, cjson, locale
from threading import Lock

class Pool:
    def __init__(self, create):
        self.create = create
        self.lock = Lock()
        self.pool = []
    def get(self):
        self.lock.acquire()
        try:
            if len(self.pool): return self.pool.pop()
            else: return self.create()
        finally:
            self.lock.release()
    def put(self, value):
        self.lock.acquire()
        try:
            self.pool.append(value)
        finally:
            self.lock.release()

curlPool = Pool(pycurl.Curl)

from threading import Lock

class Pool:
    def __init__(self, create):
        self.create = create
        self.lock = Lock()
        self.pool = []
    def get(self):
        self.lock.acquire()
        try:
            if len(self.pool): return self.pool.pop()
            else: return self.create()
        finally:
            self.lock.release()
    def put(self, value):
        self.lock.acquire()
        try:
            self.pool.append(value)
        finally:
            self.lock.release()

curlPool = Pool(pycurl.Curl)

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
        elif isinstance(val, bool): enc(name, (val and "true") or "false")
        elif isinstance(val, int): enc(name, "%d" % val)
        elif isinstance(val, float): enc(name, "%g" % val)
        elif isinstance(val, list) or isinstance(val, tuple):
            for elt in val: encval(name, elt)
        elif isinstance(val, basestring):
            enc(name, val.encode("utf-8"))
        else:
            enc(name, unicode(val).encode("utf-8"))
    for name, val in args.iteritems():
        encval(name, val)
    return "&".join(buf)

def makeRequest(obj, method, url, body=None, accept="*/*", contentType=None, callback=None, errCallback=None):
    curl = curlPool.get()

    # Uncomment these 5 lines to see pycurl debug output
    ## def report(debug_type, debug_msg):
    ##     if debug_type != 3:
    ##         print "debug(%d): %s" % (debug_type, debug_msg)
    ## curl.setopt(pycurl.VERBOSE, 1)
    ## curl.setopt(pycurl.DEBUGFUNCTION, report)

    if obj.user and obj.password:
        curl.setopt(pycurl.USERPWD, "%s:%s" % (obj.user, obj.password))
        curl.setopt(pycurl.HTTPAUTH, pycurl.HTTPAUTH_BASIC)
    if not url.startswith("http:"): url = obj.url + url

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

    # The "Expect:" is there to suppress "Expect: 100-continue"
    # behaviour that is the default in libcurl when posting large
    # bodies.
    headers = ["Connection: Keep-Alive", "Accept: " + accept, "Expect:"]
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
        result = (curl.getinfo(pycurl.RESPONSE_CODE), response)
        curlPool.put(curl)
        return result

def jsonRequest(obj, method, url, body=None, contentType="application/x-www-form-urlencoded", rowreader=None, accept="application/json"):
    if rowreader is None:
        status, body = makeRequest(obj, method, url, body, accept, contentType)
        if (status == 200): return cjson.decode(body)
        else: raise RequestError(status, body)
    else:
        def raiseErr(status, message): raise RequestError(status, message)
        makeRequest(obj, method, url, body, accept, contentType, callback=rowreader.process, errCallback=raiseErr)

def nullRequest(obj, method, url, body=None, contentType="application/x-www-form-urlencoded"):
    status, body = makeRequest(obj, method, url, body, "application/json", contentType)
    if (status < 200 or status > 204): raise RequestError(status, body)

class RowReader:
    def __init__(self, callback):
        self.hasNames = None
        self.names = None
        self.skipNextBracket = False
        self.callback = callback
        self.backlog = None

    def process(self, string):
        if self.hasNames is None:
            self.hasNames = string[0] == "{"
            if not self.hasNames: self.skipNextBracket = True

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
