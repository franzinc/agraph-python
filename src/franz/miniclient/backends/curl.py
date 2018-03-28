###############################################################################
# Copyright (c) 2006-2016 Franz Inc.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the Eclipse Public License v1.0
# which accompanies this distribution, and is available at
# http://www.eclipse.org/legal/epl-v10.html
###############################################################################

"""
A pycurl-based implementation of HTTP backend.
"""

from __future__ import print_function

import pycurl

import os
import threading
import time
import io

import errno

from past.builtins import unicode, basestring
from future.utils import iteritems

from franz.openrdf.util.strings import to_native_string, to_bytes

# Public symbols
__all__ = ['makeRequest']

# A simple way of checking if we're on pycurl>=7.19.3
if hasattr(pycurl, 'NOPROXY'):
    copt = lambda x: x
else:
    def copt(value):
        """
        Convert a value to a type suitable for a curl option.
        This simply means that unicode strings must be encoded
        to byte strings in older versions of curl.
        """ 
        if isinstance(value, unicode):
            return value.encode('ascii')
        return value          
    

curlPool = None

class Pool(object):
    """
    A pool of curl objects.

    Curl instances are not thread-safe, so we maintain this
    pool to avoid the need of creating a fresh object at each request.

    A new connection is created only when there are no connections left
    in the pool.
    """
    @staticmethod
    def instance():
        """
        Retrieve or construct a pool object.

        There is a global pool that is lazily constructed
        and returned by this method.
        """
        global curlPool
        # Reusing curl object after forking could cause problems
        # (e.g. shared sockets...), so we make sure that each
        # process has its own pool.
        pid = os.getpid()

        # There might be a race to create the pool, but
        # it probably isn't worth a lock. If two are created
        # the first one assigned to curlPool will just
        # lose a reference and be deleted - not a big deal.
        if curlPool is None or curlPool.pid != pid:
            curlPool = Pool(pycurl.Curl, pid)

        return curlPool

    def __init__(self, create, pid):
        """
        Initialize a pool.

        :param create: Function used to create new connections.
        :param pid: PID of the process creating the pool.
        """
        self.create = create
        self.pid = pid
        self.lock = threading.Lock()
        self.pool = []

    def get(self):
        """
        Retrieve a connection from the pool.

        The result should be returned after use (see :meth:`.put`).
        """
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
        """
        Return a connection to the pool.
        """
        # We could call value.reset() here before returning the curl object
        # to the pool for pycurl version >= 7.19.0 if the C code for reset
        # actually incremented the reference on the returned None object.
        # As the C code is now, if called, the refcount on None eventually
        # goes to zero after enough requests and the Python interpreter
        # dies a quick death.
        # Note that this is fixed in pycurl 7.19.0.1 and patched in the RPM
        # package for pycurl 7.19.0 for CentOS 6.
        self.lock.acquire()
        try:
            self.pool.append(value)
        finally:
            self.lock.release()


def normalize_headers(headers):
    """
    Create a list of headers from:
       * A list of curl-style headers (return a copy)
       * None
       * a dictionary.

    The result is a list of curl-style headers.

    :param headers: List or dict of header (may also be None).
    :rtype headers: list[string] | dict[string, string] | None
    :return: A list of headers suitable for pycurl.
    :rtype: list[string]
    """
    if headers is None:
        result = []
    elif isinstance(headers, dict):
        result = ['%s: %s' % entry for entry in iteritems(headers)]
    else:
        result = headers[:]
    return [to_native_string(h) for h in result]


def retrying_perform(curl):
    """
    Perform a curl request, retrying in case of connection errors,

    We want to be able to resend the request if the current connection
    is unusable due to econnreset (e.g. because a proxy or a load balancer
    has just killed a keep-alive connection).

    :param curl: Curl object storing request parameters.
    :type curl: pycurl.Curl
    """
    # Delay before next retry (in seconds).
    retry = 0.1

    while True:
        try:
            curl.perform()
            break
        except pycurl.error as error:
            # Only retry in case of an ECONNRESET happening when
            # the connection is created.
            if (error.args[0] == pycurl.E_COULDNT_CONNECT and
                    curl.getinfo(pycurl.OS_ERRNO) == errno.ECONNRESET and
                    # Keep retrying until the delay reaches two seconds.
                    # This gives us 5 sleeps (and thus 6 requests,
                    # including the initial one) with the total sleep
                    # time of at most ~3.1 seconds.
                    retry < 2.0):
                # Retry
                time.sleep(retry)
                # The retry period will grow exponentially.
                retry *= 2
                continue
            # Not ECONNRESET or retry limit exceeded
            raise

# These constants are missing from pycurl <= 7.19.5, 
# some systems we want to support have only 7.19.0
# Note that libcurl itself must be at least 7.19.0 
# for this to work.
PROXYTYPE_SOCKS5_HOSTNAME = 7
PROXYTYPE_SOCKS4A = 6
PROXYTYPE_HTTP = 0

# Maps proxy type names to curl constants
# TODO: HTTPS proxies (since curl 7.52.0)?
_proxy_types = {
    'http': PROXYTYPE_HTTP,
    'socks': PROXYTYPE_SOCKS5_HOSTNAME,
    'socks4': PROXYTYPE_SOCKS4A,
    'socks5': PROXYTYPE_SOCKS5_HOSTNAME,
}


def makeRequest(obj, method, url, body=None, accept=None, contentType=None, callback=None, errCallback=None, headers=None):
    """
    Send a request to the server.

    See :func:`franz.miniclient.backends.requests.makeRequest` for documentation.
    """
    curl = Pool.instance().get()

    # TODO: Just make this an option
    # Uncomment these 5 lines to see pycurl debug output
    ## def report(debug_type, debug_msg):
    ##     if debug_type != 3:
    ##         print "debug(%d): %s" % (debug_type, debug_msg)
    ##curl.setopt(pycurl.VERBOSE, 1)
    ##curl.setopt(pycurl.DEBUGFUNCTION, report)

    #curl.setopt(pycurl.TIMEOUT, 45)

    if accept is None:
        accept = "*/*"

    # Proxy support
    if obj.proxy is not None:
        curl.setopt(pycurl.PROXY, obj.proxy_host)
        curl.setopt(pycurl.PROXYPORT, obj.proxy_port)
        curl.setopt(pycurl.PROXYTYPE, _proxy_types[obj.proxy_type])
    else:
        # Unsetopt doesn't work. As usual.
        curl.setopt(pycurl.PROXY, '')

    if obj.user is not None and obj.password is not None:
        curl.setopt(pycurl.USERPWD, "%s:%s" % (to_native_string(obj.user),
                                               to_native_string(obj.password)))
        curl.setopt(pycurl.HTTPAUTH, pycurl.HTTPAUTH_BASIC)
    else:
        curl.unsetopt(pycurl.USERPWD)
    if obj.cainfo is not None:
        curl.setopt(pycurl.CAINFO, copt(obj.cainfo))
    if obj.sslcert is not None:
        curl.setopt(pycurl.SSLCERT, copt(obj.sslcert))
    if obj.verifyhost is not None:
        curl.setopt(pycurl.SSL_VERIFYHOST, obj.verifyhost)
    if obj.verifypeer is not None:
        curl.setopt(pycurl.SSL_VERIFYPEER, obj.verifypeer)

    url = to_native_string(url)
    if not url.startswith("http:") and not url.startswith("https:"):
        url = to_native_string(obj.url) + to_native_string(to_native_string(url))

    method = to_native_string(method)

    # Usually POST and UPLOAD correspond to POST and PUT requests respectively.
    # In our case we will override the method using CUSTOMREQUEST and the
    # settings here will tell us if we're uploading using a READFUNCTION
    # or posting a string with POSTFIELDS.
    curl.setopt(pycurl.POST, 0)
    curl.setopt(pycurl.UPLOAD, 0)
    if body:
        if method in ("POST", "PUT"):
            if isinstance(body, basestring):
                # String
                body = to_bytes(body)
                curl.setopt(pycurl.POSTFIELDS, body)
                curl.setopt(pycurl.POST, 1)
            else:
                # File - can be passed as READDATA, but not as POSTFIELDS.
                curl.setopt(pycurl.READDATA, body)
                curl.setopt(pycurl.UPLOAD, 1)
        else:
            contentType = None
            url = url + "?" + to_native_string(body)
    else:
        contentType = None

    curl.setopt(pycurl.CUSTOMREQUEST, method)
    curl.setopt(pycurl.URL, url)

    # The "Expect:" is there to suppress "Expect: 100-continue"
    # behavior that is the default in libcurl when posting large
    # bodies.
    headers = normalize_headers(headers)
    if headers is None:
        headers = []
    headers.extend(["Connection: keep-alive", "Accept: " + to_native_string(accept), "Expect:"])
    if callback: headers.append("Connection: close")
    if contentType: headers.append("Content-Type: " + to_native_string(contentType))
    if obj.runAsName: headers.append("x-masquerade-as-user: " + to_native_string(obj.runAsName))
    curl.setopt(pycurl.HTTPHEADER, headers)
    curl.setopt(pycurl.ENCODING, "")  # which means 'any encoding that curl supports'

    if callback:
        status = [None]
        error = []

        # Called by curl for each header line.
        # The first line will contain the status code and that is
        # the only part we're interested in.
        def headerfunc(string):
            if status[0] is None:
                # Parse the status code if this is the first line.
                status[0] = int(unicode(string, 'utf-8').split(" ")[1])
            # return input length to indicate "no errors".
            return len(string)

        # Called by curl for each block of data received.
        # The argument will be a bytestring.
        def writefunc(string):
            if status[0] == 200: callback(string)
            else: error.append(unicode(string, 'utf-8'))

        curl.setopt(pycurl.WRITEFUNCTION, writefunc)
        curl.setopt(pycurl.HEADERFUNCTION, headerfunc)
        retrying_perform(curl)
        if status[0] != 200:
            errCallback(curl.getinfo(pycurl.RESPONSE_CODE), "".join(error))
    else:
        buf = io.BytesIO()
        curl.setopt(pycurl.WRITEFUNCTION, buf.write)
        retrying_perform(curl)
        response = to_native_string(buf.getvalue())
        buf.close()
        result = (curl.getinfo(pycurl.RESPONSE_CODE), response)
        Pool.instance().put(curl)
        return result
