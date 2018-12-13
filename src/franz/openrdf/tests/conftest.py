################################################################################
# Copyright (c) 2006-2017 Franz Inc.  
# All rights reserved. This program and the accompanying materials are
# made available under the terms of the MIT License which accompanies
# this distribution, and is available at http://opensource.org/licenses/MIT
################################################################################

"""
Test fixtures, moved to a separate file to avoid warnings
about parameter names shadowing global symbols.
"""
import random
from collections import MutableMapping
import uuid

import pytest
import requests

from franz.openrdf.model import URI
from franz.openrdf.repository import Repository
from franz.openrdf.repository.attributes import AttributeDefinition
from franz.openrdf.sail import AllegroGraphServer

from .tests import AG_HOST, AG_PORT, AG_PROXY, STORE, CATALOG, USER, PASSWORD
import contextlib, os, threading
import six
from six.moves import BaseHTTPServer


# noinspection PyShadowingNames
@pytest.fixture
def conn(server):
    """
    Provides a connection to the test repository. The repository is cleared
    before the test, but not after it.
    """
    catalog = server.openCatalog(CATALOG)
    stores = catalog.listRepositories()

    # Instead of renewing the database, clear it.
    mode = Repository.CREATE if STORE not in stores else Repository.OPEN

    repo = catalog.getRepository(STORE, mode)
    repo.initialize()
    connection = repo.getConnection()

    if STORE in stores:
        connection.setUserAttributes({})
        connection.clearAttributeFilter()
        connection.clear()
        connection.clearNamespaces()
        connection.disableDuplicateSuppression()
        for attr in connection.getAttributeDefinitions():
            connection.deleteAttributeDefinition(attr.name)

    with connection:
        yield connection


# noinspection PyShadowingNames
@pytest.fixture
def repo_name(conn):
    """
    Make sure that a repository exists and is empty.

    Return the name of that repository.
    """
    name = conn.repository.database_name
    conn.close()
    return name


@pytest.fixture
def server():
    """
    A fixture that allows easy access to an AllegroGraphServer object.
    """
    yield AllegroGraphServer(AG_HOST, AG_PORT, USER, PASSWORD, proxy=AG_PROXY)


# noinspection PyShadowingNames
@pytest.fixture
def non_existing_repo(server):
    """
    A fixture that makes sure a repository does not exist before the test
    and also deletes it after the test.
    The value returned by this fixture is the name of the store.
    """
    catalog = server.openCatalog(CATALOG)
    store = 'temp-store'
    while store in catalog.listRepositories():
        store = 'temp-store-' + ''.join(random.choice('0123456789') for _ in range(16))
    yield store
    if store in catalog.listRepositories():
        catalog.deleteRepository(store)


# noinspection PyShadowingNames
@pytest.fixture
def session(conn):
    conn.openSession()
    yield conn
    conn.closeSession()


@pytest.fixture
def attr(conn):
    attr = AttributeDefinition(
        name='test', allowed_values=['a', 'b', 'c'],
        minimum_number=0, maximum_number=1)
    yield conn.setAttributeDefinition(attr)


@pytest.fixture
def attr2(conn):
    attr = AttributeDefinition(
        name='test2', allowed_values=['a', 'b', 'c'],
        minimum_number=0, maximum_number=None)
    yield conn.setAttributeDefinition(attr)


@pytest.fixture
def clean_env():
    old = os.environ
    os.environ = {}
    yield os.environ
    os.environ = old


# Injecting common URIs this way is less verbose


def make_uri_fixture(name):
    """
    Create a pytest fixture named NAME that resolves
    to a URI object '<ex://NAME>'.
    """
    # noinspection PyShadowingNames
    def func(conn):
        return conn.createURI('ex://' + name)
    func.__name__ = name
    return pytest.fixture(func, name=name)


def define_uri_fixtures():
    gl = globals()
    for uri in ('s', 'p', 'o', 'g', 'x'):
        for suffix in ('', '1', '2'):
            name = uri + suffix
            gl[name] = make_uri_fixture(name)


define_uri_fixtures()


@pytest.fixture
def ex(conn):
    return conn.namespace('ex://')


@pytest.fixture
def example(conn):
    return conn.namespace('http://franz.com/example/')


class UserData(MutableMapping):
    """
    A dictionary-like object that stores values in the user data
    service on an AllegroGraph server.

    This object keeps track of the values it changed. All changes
    can be undone by calling 'close'.
    """
    def __init__(self, server_object):
        """
        Initialize user data dictionary.

        :param server_object: AG server handle.
        :type server_object: AllegroGraphServer
        """
        self.server = server_object
        self.old_values = {}

    def __setitem__(self, k, v):
        if k not in self.old_values:
            self.old_values[k] = self.server.getUserData(k)
        self.server.setUserData(k, v)

    def __delitem__(self, k):
        if k not in self.old_values:
            self.old_values[k] = self.server.getUserData(k)
        self.server.deleteUserData(k)

    def __getitem__(self, k):
        return self.server.getUserData(k)

    def __len__(self):
        return len(self.server.listUserData())

    def __iter__(self):
        for k in self.server.listUserData():
            yield self.server.getUserData(k)

    def close(self):
        """
        Undo all changes made through this object.
        """
        for k, v in six.iteritems(self.old_values):
            if v is None:
                self.server.deleteUserData(k)
            else:
                self.server.setUserData(k, v)
        self.old_values.clear()


# noinspection PyShadowingNames
@pytest.fixture
def user_data(server):
    """
    A test fixture providing access to an instance of the UserData class.
    All user data changes are undone during cleanup.
    """
    with contextlib.closing(UserData(server)) as ud:
        yield ud


class HTTPServer(object):
    """
    A trivial HTTP server used during tests.

    After creation handlers for specific paths can be added with `add_handler()`.
    Calling `start()` will cause the server to listen for connections in
    a separate thread, calling `close()` will stop the server.

    The server only handles GET requests.

    This is very basic, features should be added as needed by tests.
    """
    def __init__(self):
        class Handler(BaseHTTPServer.BaseHTTPRequestHandler):
            def do_DELETE(self):
                self.send_response(200, 'OK')
                self.send_header('Content-Type', 'text/plain')
                self.send_header('Content-Length', '0')
                self.end_headers()
                server_self.keep_running = False

            def do_GET(self):
                path = ensure_slash(self.path)

                if path not in handlers:
                    self.send_response(404, 'Not found')
                    self.send_header('Content-Type', 'text/plain')
                    self.end_headers()
                    self.wfile.write(b'Oops: ' + path.encode('utf-8'))
                else:
                    self.send_response(200, 'OK')
                    self.send_header('Content-Type', 'text/plain')
                    self.end_headers()
                    handler = handlers[self.path]
                    result = handler() if callable(handler) else handler
                    self.wfile.write(result)

        server_self = self
        self.handlers = handlers = {}
        self.server = BaseHTTPServer.HTTPServer(('127.0.0.1', 0), Handler)
        self.keep_running = True
        self.thread = threading.Thread(target=self._run)
        # Do not hang if a server thread refuses to die
        self.thread.daemon = True

    def _run(self):
        while self.keep_running:
            self.server.handle_request()
        self.server.server_close()

    def start(self):
        """
        Start listening for connections.
        """
        self.thread.start()

    def close(self):
        """
        Stop the server.
        """
        requests.delete(self.url('anything'))
        self.thread.join(5.0)
        if self.thread.is_alive():
            raise Exception('Embedded HTTP server refused to terminate!')

    def publish(self, path, handler):
        """
        Add a handler for a specific path.

        :param path: Path to be handled.
        :param handler: Content to return, either a bytestring or a function
                        that returns one.
        """
        path = ensure_slash(path)
        self.handlers[path] = handler

    def url(self, path):
        """
        Get the full URL for a specific path.

        :param path: Path to be accessed.
        :return: A full URL, including the port number.
        """
        path = ensure_slash(path)
        return 'http://127.0.0.1:%d%s' % (self.server.server_port, path)


@pytest.fixture
def http_server():
    """
    A fixture wrapping the HTTP server, takes care of starting and stopping it.
    :return:
    """
    with contextlib.closing(HTTPServer()) as server:
        server.start()
        yield server


# Lisp code used to run an HTTP server.
# Note that it is a .format() string and expects a security key
# to be passed in the {key} keyword argument.
server_code = """
(use-package :net.aserve)

(defun fixed-handler (body)
  (lambda (req ent)
    (with-http-response (req ent)
      (with-http-body (req ent)
        (write-string body (request-reply-stream req))))))

(defun pub (req ent)
  (when (string= (header-slot-value req :authorization) "test {key}")
    (with-http-response (req ent)
      (with-http-body (req ent)
        (let ((body (get-request-body req))
              (path (request-query-value "path" req))
              (type (or (request-query-value "type" req) "text/plain")))
          (publish :path path
                   :content-type type 
                   :function (fixed-handler body)
                   :server (request-wserver req)))))))
  
(defun stop (req ent)
  (with-http-response (req ent)
    (with-http-body (req ent)))
  (when (string= (header-slot-value req :authorization) "test {key}")
    (shutdown :server (request-wserver req))))
  
(let ((server (start :port 0 :host "127.0.0.1" :server :new)))
  (publish :path "/__pub"
           :content-type "text/plain"
           :server server
           :function #'pub)
  (publish :path "/__stop" :function #'stop :server server)
  (socket:local-port (wserver-socket server)))
"""


def lisp_string(python_string):
    """
    Convert a string to a Lisp string literal.
    """
    return '"%s"' % python_string.replace('\\', '\\\\').replace('"', '\\"')


class RemoteHTTPServer(object):
    """
    Used to make things available through HTTP from the AG server.

    The server is started by calling `start()` and stopped by
    calling `close()`. New documents (strings) can be published
    by using `publish()`. Full URIs to documents (including
    the port number) can be obtained by calling `uri()`.

    Note that documents published here are not necessarily accessible
    from the machine on which the tests are run. Any requests for such
    data must be executed on the AG server. The `send_request()` method
    can be used to send a request through the AG server and return
    the response.

    Very basic, works only with text, can only serve simple strings.
    Should be extended when needed.
    """
    def __init__(self, connection):
        self.conn = connection
        self.key = uuid.uuid4()
        self.port = 0  # Will be set later

    def send_request(self, path, body=None, params=None):
        """
        Execute a HTTP request *from the AG server*.

        This will be a GET request, unless the `body` parameter is used,
        in which case a POST request will be sent.

        :param path: Document path (as given to `publish()`.
        :param body: Request body - if specified this will be a POST request.
        :param params: Request parameters, passed in the URI.
        :return: Response body.
        """
        alist = None
        if params:
            pairs = ' '.join('(%s . %s)' % (lisp_string(k), lisp_string(v))
                             for k, v in six.iteritems(params))
            alist = "'(%s)" % pairs

        uri = self.uri(path)

        return self.conn.evalInServer('''
            (prog1 (net.aserve.client:do-http-request 
             {uri} 
             :method {method}
             :query {query}
             :headers '((:authorization . "test {key}"))
             :content-type "text/plain"
             :content {body}))
        '''.format(uri=lisp_string(uri),
                   query=alist or 'nil',
                   key=self.key,
                   method=':post' if body else ':get',
                   body=lisp_string(body) if body else 'nil'))

    def uri(self, path):
        """
        Retrieve full URI to a document published with `publish`.

        Note that the resulting URI is not necessarily accessible
        from the machine on which the tests are executed.

        :param path: Document path.
        :return: Full URI.
        """
        path = ensure_slash(path)
        return 'http://127.0.0.1:%d%s' % (self.port, path)

    def start(self):
        """
        Start listening for connections.
        """
        self.port = self.conn.evalInServer(server_code.format(key=self.key))

    def close(self):
        """
        Shutdown the server.
        """
        self.send_request('__stop')

    def publish(self, path, document, content_type='text/plain'):
        """
        Publish a document (string).

        :param path: Target path on the server.
        :param document: Document text.
        :param content_type: MIME type, the default is 'text/plain'.
        :return: Full URI to the published document.
        """
        path = ensure_slash(path)
        self.send_request('__pub', document, {
            'path': path,
            'type': content_type
        })
        return self.uri(path)


# noinspection PyShadowingNames
@pytest.fixture
def remote_http_server(conn):
    """
    A fixture wrapping the remote (i.e. AG-side) HTTP server,
    takes care of starting and stopping the server.
    """
    with contextlib.closing(RemoteHTTPServer(conn)) as server:
        server.start()
        yield server


def ensure_slash(text):
    """
    Prepend a slash to a string, unless it starts with one already.
    """
    if text.startswith('/'):
        return text
    return '/' + text


# Cache the version number
_version = None


def get_version():
    global _version
    if _version is None:
        server = AllegroGraphServer(AG_HOST, AG_PORT, USER, PASSWORD, proxy=AG_PROXY)
        _version = server.versionTuple
    return _version

@pytest.fixture(scope='module')
def version():
    return get_version()


# Decorator used to skip tests
def min_version(*args):
    version = get_version()
    if version < args:
        expected = '.'.join(str(c) for c in args)
        actual = '.'.join(str(c) for c in version)
        return pytest.mark.skip("AG server version: %s < %s" % (actual, expected))
    return lambda x: x


# noinspection PyShadowingNames
@pytest.fixture
def ex(conn):
    return conn.namespace('ex://')


@pytest.fixture
def after():
    """
    A fixture that resolves to a mutable list of functions that will
    be called after the test.
    """
    to_call = []
    exceptions = []
    yield to_call
    for o in to_call:
        try:
            o()
        except Exception as e:
            exceptions.append(e)
    if exceptions:

        raise Exception('Exception(s) raised when cleaning up' +
                        ('\n'.join(str(e) for e in exceptions)))
