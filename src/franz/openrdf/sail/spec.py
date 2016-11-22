################################################################################
# Copyright (c) 2006-2017 Franz Inc.  
# All rights reserved. This program and the accompanying materials are
# made available under the terms of the MIT License which accompanies
# this distribution, and is available at http://opensource.org/licenses/MIT
################################################################################

"""
Helper functions for creating session specification strings.
See :meth:`~franz.openrdf.sail.allegrographserver.AllegroGraphServer.openSession`
"""

from __future__ import unicode_literals
from past.builtins import map, unicode
from future import standard_library
standard_library.install_aliases()

import urllib.request, urllib.parse, urllib.error


def local(name, catalog=None):
    """
    Create a session spec for connecting to a store on the local server.

    :param name: Store name.
    :type name: string
    :param catalog: Catalog name (None = root catalog).
    :type catalog: string
    :return: A session spec string.
    :rtype: string
    """
    if catalog: return "<%s:%s>" % (catalog, name)
    else: return "<%s>" % name


def remote(name, catalog=None, host="localhost", port=None, protocol="http"):
    """
    Create a session spec for connecting to a store on another server.

    :param name: Store name.
    :type name: string
    :param catalog: Catalog name (None = root catalog).
    :type catalog: string
    :param host: Remote host address.
    :type host: string
    :param port: Port number on the remote host (default: 10035 for http, 10036 for https).
    :type port: int
    :param protocol: Protocol - `"http"` or `"https"`, the default is `"http"`.
    :type protocol: string
    :return: A session spec string.
    :rtype: string
    """
    if port is None:
        port = 10035 if protocol == "http" else 10036
    if catalog: catalog = "/catalogs/" + urllib.parse.quote(catalog)
    return "<%s://%s:%d%s/repositories/%s>" % (protocol, host, port, catalog or "", urllib.parse.quote(name))

def url(url):
    """
    Create a session spec for connecting to a remote store with known URL.
    :param url: Remote store's address.
    :type url: string
    :return: A session spec string.
    :rtype: string
    """
    return "<%s>" % url


def federate(*stores):
    """
    Create a session spec for connecting to a federated store.

    :param stores: List of session specs to federate.
    :type stores: tuple[string]
    :return: A session spec string.
    :rtype: string
    """
    return " + ".join(stores)


def reason(store, reasoner="rdfs++"):
    """
    Create a session spec that adds reasoning support to another session.

    :param store: Base session spec.
    :type store: string
    :param reasoner: Reasoning type (e.g. `"rdfs++"`or `"restriction"`).
    :type reasoner: string
    :return: A session spec string.
    :rtype: string
    """
    return "%s[%s]" % (store, reasoner)


def graphFilter(store, graphs):
    """
    Create a graph-filtered session spec.

    :param store: Base session spec.
    :type store: string
    :param graphs: List of graph names. `None` means the default graph.
    :type graphs: list[string]
    :return: A session spec string.
    :rtype: string
    """
    def asGraph(x):
        if x is None: return "null"
        else: return unicode(x)
    return "%s{%s}" % (store, " ".join(map(asGraph, graphs)))
