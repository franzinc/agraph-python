################################################################################
# Copyright (c) 2006-2017 Franz Inc.  
# All rights reserved. This program and the accompanying materials are
# made available under the terms of the MIT License which accompanies
# this distribution, and is available at http://opensource.org/licenses/MIT
################################################################################

from __future__ import unicode_literals
from past.builtins import map, unicode
from future import standard_library
standard_library.install_aliases()

import urllib.request, urllib.parse, urllib.error

def local(name, catalog=None):
  if catalog: return "<%s:%s>" % (catalog, name)
  else: return "<%s>" % name

def remote(name, catalog=None, host="localhost", port=10035, protocol="http"):
  if catalog: catalog = "/catalogs/" + urllib.parse.quote(catalog)
  return "<%s://%s:%d%s/repositories/%s>" % (protocol, host, port, catalog or "", urllib.parse.quote(name))

def url(url):
  return "<%s>" % url

def federate(*stores):
  return " + ".join(stores)

def reason(store, reasoner="rdfs++"):
  return "%s[%s]" % (store, reasoner)

def graphFilter(store, graphs):
  def asGraph(x):
    if x is None: return "null"
    else: return unicode(x)
  return "%s{%s}" % (store, " ".join(map(asGraph, graphs)))
