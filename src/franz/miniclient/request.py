###############################################################################
# Copyright (c) 2006-2016 Franz Inc.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the Eclipse Public License v1.0
# which accompanies this distribution, and is available at
# http://www.eclipse.org/legal/epl-v10.html
###############################################################################
from __future__ import print_function

from itertools import islice

from future.builtins import bytes, next, object, range

from future.utils import iteritems, python_2_unicode_compatible, bchr
from past.builtins import unicode
from past.builtins import str as old_str
from future.utils import native_str
import io, errno, pycurl, locale, re, os, sys, time
from threading import Lock

from franz.openrdf.util.strings import to_native_string

if sys.version_info[0] > 2:
    from urllib.parse import quote
    from io import StringIO
else:
    from urllib import quote
    from cStringIO import StringIO


class JsonDecodeError(Exception):
    pass


if sys.version_info[0] == 2:
    import cjson

    def encode_json(value):
        return cjson.encode(value)


    def decode_json(text):
        try:
            # True = Output Unicode instead of bytestrings
            return cjson.decode(text, True)
        except cjson.DecodeError:
            raise JsonDecodeError()

    # Workaround for a bug in python-future
    def ibytes(x):
        """
        Construct a bytes object from a sequence or iterator over integers.
        In Python 3, bytes() can do that, but python-future does not have
        that capability.
        """
        if not hasattr(x, '__len__'):
            return bytes(list(x))
        return bytes(x)

else:
    import json

    def encode_json(value):
        return json.dumps(value)

    def decode_json(text):
        try:
            return json.loads(text)
        except ValueError:
            raise JsonDecodeError

    ibytes = bytes


def mk_unicode(text):
    if not isinstance(text, unicode):
        return unicode(text, 'utf-8')
    return text

curlPool = None

class Pool(object):
    @staticmethod
    def instance():
        global curlPool
        pid = os.getpid()

        # There might be a race to create the pool, but
        # it probably isn't worth a lock. If two are created
        # the first one assigned to curlPool will just
        # lose a reference and be deleted - not a big deal.
        if curlPool is None or curlPool.pid != pid:
            curlPool = Pool(pycurl.Curl, pid)

        return curlPool
        
    def __init__(self, create, pid):
        self.create = create
        self.pid = pid
        self.lock = Lock()
        self.pool = []

    def get(self):
        self.lock.acquire()
        try:
            value = self.pool.pop()
        except IndexError:
            value = None
        finally:
            self.lock.release()

        # Create new ones outside the lock
        return value or self.create()

    def put(self, value):
        # We could call value.reset() here before returning the curl object
        # to the pool for pycurl version >= 7.19.0 if the C code for reset
        # actually incremented the reference on the returned None object.
        # As the C code is now, if called, the refcount on None eventually
        # goes to zero after enough requests and the Python interpretor
        # dies a quick death.
        self.lock.acquire()
        try:
            self.pool.append(value)
        finally:
            self.lock.release()


@python_2_unicode_compatible
class RequestError(Exception):
    code = None
    
    def __init__(self, status, message):
        print(status, message)
        self.status = status
        if status == 400:
            match = re.match("([A-Z ]+): (.*)", message)
            if match:
                self.code = match.group(1)
                message = match.group(2)
        self.message = message
            
    def __str__(self):
        return "Server returned %s: %s" % (self.status, self.message)


def urlenc(**args):
    buf = StringIO()

    def enc(name, val):
        if buf.tell():
            buf.write('&')
        buf.write(quote(to_native_string(name)))
        buf.write("=")
        buf.write(quote(to_native_string(val)))

    def encval(name, val):
        if val is None: pass
        elif isinstance(val, bool): enc(name, (val and "true") or "false")
        elif isinstance(val, int): encval(name, "%d" % val)
        elif isinstance(val, float): encval(name, "%g" % val)
        elif isinstance(val, list) or isinstance(val, tuple):
            for elt in val: encval(name, elt)
        elif isinstance(val, native_str):
            enc(name, val)
        elif isinstance(val, (old_str, unicode)):
            enc(name, to_native_string(val))
        else:
            enc(name, to_native_string(str(val)))
    for arg_name, value in iteritems(args):
        encval(arg_name, value)
    return buf.getvalue()

def makeRequest(obj, method, url, body=None, accept="*/*", contentType=None, callback=None, errCallback=None, headers=None):
    curl = Pool.instance().get()

    # Uncomment these 5 lines to see pycurl debug output
    ## def report(debug_type, debug_msg):
    ##     if debug_type != 3:
    ##         print "debug(%d): %s" % (debug_type, debug_msg)
    ##curl.setopt(pycurl.VERBOSE, 1)
    ##curl.setopt(pycurl.DEBUGFUNCTION, report)

    #curl.setopt(pycurl.TIMEOUT, 45)

    # Use a proxy:
    #curl.setopt(pycurl.PROXY, '127.0.0.1')
    #curl.setopt(pycurl.PROXYPORT, 8888)
    #curl.setopt(pycurl.PROXYTYPE, pycurl.PROXYTYPE_HTTP)

    if obj.user is not None and obj.password is not None:
        curl.setopt(pycurl.USERPWD, "%s:%s" % (to_native_string(obj.user),
                                               to_native_string(obj.password)))
        curl.setopt(pycurl.HTTPAUTH, pycurl.HTTPAUTH_BASIC)
    else:
        curl.unsetopt(pycurl.USERPWD)
    if obj.cainfo is not None:
        curl.setopt(pycurl.CAINFO, obj.cainfo)
    if obj.sslcert is not None:
        curl.setopt(pycurl.SSLCERT, obj.sslcert)
    if obj.verifyhost is not None:
        curl.setopt(pycurl.SSL_VERIFYHOST, obj.verifyhost)
    if obj.verifypeer is not None:
        curl.setopt(pycurl.SSL_VERIFYPEER, obj.verifypeer)

    url = to_native_string(url)
    if not url.startswith("http:") and not url.startswith("https:"):
        url = to_native_string(obj.url) + to_native_string(to_native_string(url))

    method = to_native_string(method)
    postbody = method in ("POST", "PUT")
    curl.setopt(pycurl.POSTFIELDS, "")
    if body:
        if postbody:
            curl.setopt(pycurl.POSTFIELDS, body)
        else:
            url = url + "?" + to_native_string(body)

    curl.setopt(pycurl.POST, (postbody and 1) or 0)
    curl.setopt(pycurl.CUSTOMREQUEST, method)
    curl.setopt(pycurl.URL, url)

    # The "Expect:" is there to suppress "Expect: 100-continue"
    # behaviour that is the default in libcurl when posting large
    # bodies.
    if headers is None:
        headers = []
    for i in range(len(headers)):
        headers[i] = to_native_string(headers[i])
    headers.extend(["Connection: keep-alive", "Accept: " + to_native_string(accept), "Expect:"])
    if callback: headers.append("Connection: close")
    if contentType and postbody: headers.append("Content-Type: " + to_native_string(contentType))
    if obj.runAsName: headers.append("x-masquerade-as-user: " + to_native_string(obj.runAsName))
    curl.setopt(pycurl.HTTPHEADER, headers)
    curl.setopt(pycurl.ENCODING, "")  # which means 'any encoding that curl supports'

    def retrying_perform():
        retry = 0.1
        while retry < 2.0:
            try:
                curl.perform()
                break
            except pycurl.error as error:
                if (error.args[0] == 7 and
                    curl.getinfo(pycurl.OS_ERRNO) == errno.ECONNRESET): 
                    # Retry
                    time.sleep(retry)
                    retry *= 2
                    continue
   
                raise

    if callback:
        status = [None]
        error = []
        def headerfunc(string):
            if status[0] is None:
                status[0] = locale.atoi(unicode(string, 'utf-8').split(" ")[1])
            return len(string)
        def writefunc(string):
            if status[0] == 200: callback(unicode(string, 'utf-8'))
            else: error.append(unicode(string, 'utf-8'))
        curl.setopt(pycurl.WRITEFUNCTION, writefunc)
        curl.setopt(pycurl.HEADERFUNCTION, headerfunc)
        retrying_perform()
        if status[0] != 200:
            errCallback(curl.getinfo(pycurl.RESPONSE_CODE), "".join(error))
    else:
        buf = io.BytesIO()
        curl.setopt(pycurl.WRITEFUNCTION, buf.write)
        retrying_perform()
        response = unicode(buf.getvalue(), "utf-8")
        buf.close()
        result = (curl.getinfo(pycurl.RESPONSE_CODE), response)
        Pool.instance().put(curl)
        return result

def jsonRequest(obj, method, url, body=None, contentType="application/x-www-form-urlencoded", rowreader=None, accept="application/json", headers=None):
    callback = None if rowreader is None else rowreader.process
    # If there is a _saveFile and _saveAccept, they override the arguments
    if hasattr(obj, '_saveFile') and hasattr(obj, '_saveAccept'):
        accept = obj._saveAccept
        callback = obj._saveFile.write

    if callback is None:
        status, body = makeRequest(obj, method, url, body, accept, contentType, headers=headers)
        if (status == 200):
            if accept in ('application/json', 'text/integer', "application/x-quints+json"):
                body = decode_json(body)
            return body
        else: raise RequestError(status, body)
    else:
        def raiseErr(status, message): raise RequestError(status, message)
        makeRequest(obj, method, url, body, accept, contentType, callback=callback, errCallback=raiseErr, headers=headers)

def nullRequest(obj, method, url, body=None, contentType="application/x-www-form-urlencoded", content_encoding=None):
    headers = []
    if content_encoding is not None:
        headers.append('Content-Encoding: ' + content_encoding)
    status, body = makeRequest(obj, method, url, body, "application/json", contentType, headers=headers)
    if (status < 200 or status > 204): raise RequestError(status, body)

class RowReader(object):
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
                    useArray(decode_json(string[start : end + 1].decode("utf-8")))
                    pos[0] = end + 1
                    return True
                except JsonDecodeError:
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

class SerialConstants(object):
    SO_VECTOR = 1
    SO_STRING = 5
    SO_NULL = 7
    SO_LIST = 8
    SO_POS_INTEGER = 9
    SO_END_OF_ITEMS = 10
    SO_NEG_INTEGER = 11
    SO_BYTEVECTOR = 15

def serialize(obj):
    def serialize_int(i):
        # make sure i is non negative
        i = abs(i)
        def int_bytes(i):
            rest = True
            while rest:
                lower = i & 0x7f
                rest = i >> 7
                yield lower | (0x80 if rest else 0)
                i = rest

        return ibytes(int_bytes(i))

    if obj is None:
        return bchr(SerialConstants.SO_NULL)

    if isinstance(obj, unicode):
        return b''.join([bchr(SerialConstants.SO_STRING), serialize_int(len(obj)),
            bytes(obj, 'utf-8')])

    if isinstance(obj, int):
        return b''.join([bchr(SerialConstants.SO_POS_INTEGER) if obj >= 0 else
            bchr(SerialConstants.SO_NEG_INTEGER), serialize_int(obj)])

    try:
        # Byte vector
        if obj.typecode == b'b':
            return b''.join([bchr(SerialConstants.SO_BYTEVECTOR),
                serialize_int(len(obj)), obj.tostring()])
    except:
        pass
            
    try:
        iobj = iter(obj)
        return b''.join([bchr(SerialConstants.SO_VECTOR),
            serialize_int(len(obj)),
            b''.join([serialize(elem) for elem in iobj])])
    except:
        pass

    raise TypeError("cannot serialize object of type %s" % type(obj))

def deserialize(string):
    def posInteger(chars):
        result = shift = 0
        
        # Set value to get into the loop the first time
        value = 0x80
        while value & 0x80:
            value = next(chars)
            result += ((value & 0x7f) << shift)
            shift += 7
    
        return result

    if isinstance(string, old_str):
        string = bytes(string)
    chars = iter(string)
    value = next(chars)
    
    if value == SerialConstants.SO_BYTEVECTOR:
        length = posInteger(chars)
        import array
        return array.array(b'b', [ord(next(chars)) for i in range(length)])
    
    if (value == SerialConstants.SO_VECTOR or
        value == SerialConstants.SO_LIST):
        length = posInteger(chars)
        return [deserialize(chars) for i in range(length)]
    
    if value == SerialConstants.SO_STRING:
        length = posInteger(chars)
        return unicode(ibytes(islice(chars, 0, length)), 'utf-8')

    if value == SerialConstants.SO_POS_INTEGER:
        return posInteger(chars)
    
    if value == SerialConstants.SO_NEG_INTEGER:
        return - posInteger(chars)
    
    if value == SerialConstants.SO_NULL:
        return None
        
    if value == SerialConstants.SO_END_OF_ITEMS:
        return None
        
    raise ValueError("bad code found by deserializer: %d" % value)

def encode(string):
    def convert(string):
        codes = encode.codes
        state = rem = 0

        for byte in bytes(string):
            if state == 0:
                yield codes[byte & 0x3f]
                rem = (byte >> 6) & 0x3
                state = 1
            elif state == 1:
                yield codes[((byte & 0xf) << 2) | rem]
                rem = (byte >> 4) & 0xf
                state = 2
            else:
                yield codes[((byte & 0x3) << 4) | rem]
                yield codes[((byte >> 2) & 0x3f)]
                state = 0

        if state:
            yield codes[rem]

    return ibytes(convert(string))


encode.codes = bytes(b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789*+")


def decode(string):
    def convert(string):
        codes = decode.codes
        state = rem = 0

        if isinstance(string, unicode):
            string = bytes(string, 'utf-8')
        else:
            string = bytes(string)

        for byte in string:
            byte = codes[byte]

            if state == 0:
                rem = byte
                state = 1
            elif state == 1:
                yield rem | ((byte & 0x3) << 6)
                rem = byte >> 2
                state = 2
            elif state == 2:
                yield rem | ((byte & 0xf) << 4)
                rem = byte >> 4
                state = 3
            else:
                yield rem | (byte << 2)
                state = 0

    return ibytes(convert(string))

decode.codes = [
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 62, 63,  0,  0,  0,  0, 
    52, 53, 54, 55, 56, 57, 58, 59, 60, 61,  0,  0,  0,  0,  0,  0, 
     0,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 
    15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,  0,  0,  0,  0,  0, 
     0, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 
    41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51]

