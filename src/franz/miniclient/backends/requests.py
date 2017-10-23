###############################################################################
# Copyright (c) 2006-2016 Franz Inc.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the Eclipse Public License v1.0
# which accompanies this distribution, and is available at
# http://www.eclipse.org/legal/epl-v10.html
###############################################################################

"""
A requests-based implementation of HTTP backend.
"""

from __future__ import absolute_import

import atexit
import contextlib
import sys

import requests
import requests.packages.urllib3 as urllib3
from requests.adapters import DEFAULT_POOLBLOCK, HTTPAdapter
from requests.packages.urllib3.poolmanager import PoolManager
from requests.packages.urllib3.util.retry import Retry
from pkg_resources import parse_version

from franz.openrdf.util.strings import to_native_string

# Public symbols
__all__ = ['makeRequest']

# size of the buffer used to read responses
BUFFER_SIZE = 4096

# Configure a retry strategy similar to what the curl backend does
retries = Retry(backoff_factor=0.1,
                connect=10,   # 10 retries for connection-level errors
                status_forcelist=(),  # Retry only on connection errors
                method_whitelist=False)  # Retry on all methods, even POST and PUT

# We'll want to know if something contains unicode
if sys.version_info >= (3, 0):
    unicode_type = str
else:
    unicode_type = unicode

# Never check any hostnames
class HostNameIgnoringAdapter(HTTPAdapter):
    """
    A simple transport adapter that disables hostname verification for SSL.
    """
    def init_poolmanager(self, connections, maxsize, block=DEFAULT_POOLBLOCK, **pool_kwargs):
        self.poolmanager = PoolManager(num_pools=connections,
                                       maxsize=maxsize,
                                       block=block,
                                       assert_hostname=False, **pool_kwargs)
        # Setup the retry strategy
        self.max_retries = retries


def translate_proxy_scheme(scheme):
    """
    Translate proxy type form the format AG uses to the one used by requests.

    :param scheme: Proxy type in AG format.
    :return: Proxy type in requests format.
    """
    if scheme == 'socks':
        scheme = 'socks5'

    # In urllib3 1.20 (released 2017-01-19) DNS behavior has changed
    # To make the proxy server do the lookup you now have to use
    # either 'socks4a' or 'socks5h' as the protocol.
    # But older versions naturally neither need nor support these values.
    # The updated version of urllib3 is bundled with requests since
    # version 2.13.0 (released 2017-01-24).
    v1_20 = parse_version('1.20')
    urllib3_version = parse_version(urllib3.__version__)
    if urllib3_version >= v1_20:
        if scheme == 'socks5':
            scheme = 'socks5h'
        if scheme == 'socks4':
            scheme = 'socks4a'
    return scheme


def create_session(obj):
    """
    Create a session object for a service.

    :param obj: A service object containing auth and config information.
    :type obj: franz.miniclient.repository.Service
    :return: A new requests session object with configuration taken from the service.
    :rtype requests.Session:
    """
    session = requests.Session()
    if obj.user is not None and obj.password is not None:
        session.auth = (obj.user, obj.password)

    # Proxy setup
    if obj.proxy is not None:
        session.proxy = '%s://%s:%s' % (translate_proxy_scheme(obj.proxy_type),
                                        obj.proxy_host, obj.proxy_port)

    # Emulate curl's way of handling SSL
    if obj.cainfo is not None:
        # CA certificates
        session.verify = obj.cainfo
    if obj.sslcert is not None:
        # Client certificate
        session.cert = obj.sslcert
    if obj.verifypeer is not None and not obj.verifypeer:
        # Disable certificate validation
        session.verify = False
    if obj.verifyhost is not None and not obj.verifyhost:
        # Check the certificate, but do not verify that the hostname matches it.
        session.mount('https://', HostNameIgnoringAdapter())
    else:
        # Setup the retry strategy
        session.mount('https://', HTTPAdapter(max_retries=retries))
    # setup retry strategy for http connections
    session.mount('http://', HTTPAdapter(max_retries=retries))

    return session


def normalize_headers(headers):
    """
    Create a dictionary of headers from:
       * A list of curl-style headers
       * None
       * a dictionary (return a *copy*).

    :param headers: List or dict of header (may also be None).
    :type headers: Iterable[string] | dict[string, string] | None
    :return: A dictionary of headers suitable for requests.
    :rtype: dict[string,string]
    """
    if headers is None:
        return {}
    elif isinstance(headers, dict):
        return headers.copy()
    else:
        # Assume curl-style sequence of strings
        result = {}
        for line in headers:
            key, sep, value = line.partition(':')
            if sep is None:
                raise Exception("Internal error - invalid header line (%s)" % line)
            result[key.strip().lower()] = value.strip()
        return result


def makeRequest(obj, method, url, body=None, accept=None, contentType=None, callback=None, errCallback=None, headers=None):
    """
    Send an HTTP request to given URL.

    :param obj: A service object containing auth and config information.
    :type obj: franz.miniclient.repository.Service
    :param method: Request method ("GET", "POST", ...).
    :type method: string
    :param url: Target address
    :type url: string
    :param body: Request body (for PUT/POST requests) or query string, optional.
    :type body: basestring|file
    :param accept: Value of the accept header (default: */*)
    :type accept: string
    :param contentType: MIME type of the request body, optional.
    :type contentType: string
    :param callback: Function that will receive the response data.
                     It will be called multiple times per request.
                     The return value should be either None or the number of bytes
                     received, anything else will cause the request to be aborted.
    :type callback: (bytestring) -> int
    :param errCallback: Invoked if the server returned an error.
                        Used only if `callback` is not `None`.
                        The arguments are the response code and
                        the message returned by the server.
                        Unlike normal callback, this is invoked at most once
                        and receives the complete response body.
    :type errCallback: (int, string) -> None
    :param headers: Either a dictionary mapping headers to values or
                    a list of strings that will be included in the request's headers.
    :type headers: Iterable[string] | dict[string, string] | None
    :return: Status code and response body, unless callback is specified (in that case None is returned).
    :rtype: (int, string) | None
    """
    if accept is None:
        accept = "*/*"
    # We create a session object lazily, so we do not have any requests-specific stuff
    # in the implementation of the Service class.
    if obj.session is None:
        obj.session = create_session(obj)
        # Unfortunately our current API does not seem to have a good place 
        # to close that explicitly.
        atexit.register(obj.session.close)

    # Encode data as utf-8 if required - requests tries to use ascii now.
    if isinstance(body, unicode_type):
        body = body.encode('utf-8')

    method = method.upper()
    if method in ('PUT', 'POST'):
        data = body
        params = None
    else:
        data = None
        params = body

    # Get the full url
    url = to_native_string(url)
    if not url.startswith("http:") and not url.startswith("https:"):
        url = to_native_string(obj.url) + to_native_string(url)

    # Note that this will create a copy if necessary, so we're not changing the argument
    headers = normalize_headers(headers)
    headers['accept'] = accept
    if contentType:
        headers['content-type'] = contentType
    if obj.runAsName:
        headers['x-masquerade-as-user'] = obj.runAsName

    response = obj.session.request(method, url, params=params, data=data, headers=headers, stream=True)
    with contextlib.closing(response):
        if callback is not None:
            if 200 <= response.status_code < 300:
                for chunk in response.iter_content(BUFFER_SIZE):
                    callback_result = callback(chunk)
                    # Simulate curl's behavior
                    if callback_result is not None and callback_result != len(chunk):
                        break
            else:
                if errCallback is None:
                    response.raise_for_status()
                else:
                    errCallback(response.status_code, 
                                to_native_string(response.raw.read(
                                    decode_content=True)))
        else:
            # Note: no error callback in this case
            return response.status_code, to_native_string(response.content)

