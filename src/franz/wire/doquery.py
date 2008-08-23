#!/usr/bin/env python
# -*- coding: utf-8 -*-

##***** BEGIN LICENSE BLOCK *****
##Version: MPL 1.1
##
##The contents of this file are subject to the Mozilla Public License Version
##1.1 (the "License"); you may not use this file except in compliance with
##the License. You may obtain a copy of the License at
##http:##www.mozilla.org/MPL/
##
##Software distributed under the License is distributed on an "AS IS" basis,
##WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
##for the specific language governing rights and limitations under the
##License.
##
##The Original Code is the AllegroGraph Java Client interface.
##
##The Original Code was written by Franz Inc.
##Copyright (C) 2006 Franz Inc.  All Rights Reserved.
##
##***** END LICENSE BLOCK *****


import urllib, urllib2, httplib

##class MyHTTPConnection(httplib.HTTPConnection):
    
def connect(self):
    """Connect to the host and port specified in __init__."""
    msg = "getaddrinfo returns an empty list"
    for res in socket.getaddrinfo(self.host, self.port, 0,
                                  socket.SOCK_STREAM):
        af, socktype, proto, canonname, sa = res
        try:
            self.sock = socket.socket(af, socktype, proto)
            self.sock.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)
            self.sock.settimeout(10)
            print "SOCKOPT ", self.sock.getsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY), "TIMEOUT", self.sock.gettimeout()
            if self.debuglevel > 0:
                print "connect: (%s, %s)" % (self.host, self.port)
            self.sock.connect(sa)
        except socket.error, msg:
            if self.debuglevel > 0:
                print 'connect fail:', (self.host, self.port)
            if self.sock:
                self.sock.close()
            self.sock = None
            continue
        break
    if not self.sock:
        raise socket.error, msg

httplib.HTTPConnection.connect = connect

import socket

realsocket = socket.socket
def socketwrap(family=socket.AF_INET, type=socket.SOCK_STREAM, proto=0):
    """
    Socket wrapper to enable socket.TCP_NODELAY
    """
    sockobj = realsocket(family, type, proto)
    sockobj.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, True)
    #print "A",
    return sockobj
# Enable TCP_NODELAY
socket.socket = socketwrap



from franz.openrdf.exceptions import *
from franz.wire.resultreader import ResultReader

def do_compact_post_query(query):
    """
    Execute a remove SPARQL query 'query'.
    Return a ResultReader that converts the compact format returned into
    StringTerms.
    """
    request = urllib2.Request("http://localhost:7654/sparql")
    request.add_header('accept', 'application/x-sparql-compact')
    data = [('query', query), ('id', "testP")]
    data = urllib.urlencode(data, 1)
    try:
        response = urllib2.urlopen(request, data)
        # CONSIDER GET INSTEAD OF POST?
        #response = urllib2.urlopen(request)
        return ResultReader(response)
    except Exception, e:
        print "HTTP Request failed.", e  
        return None     

def data_to_get_query(data):
    query = '&'.join(['%s=%s' % (t[0], urllib2.quote(t[1])) for t in data])
    #print "QUERY", query
    return query    
    
import socket

OPENER = urllib2.build_opener()

def do_compact_query(query):
    """
    Execute a remove SPARQL query 'query'.
    Return a ResultReader that converts the compact format returned into
    StringTerms.
    """
    data = [('query',"SELECT ?s ?o { ?s a ?o . } LIMIT 5"), ('id', "testP")]
    if (False): # GET
        request = urllib2.Request("http://localhost:7654/sparql?" + data_to_get_query(data))
        request.add_header('accept', 'application/rdf+xml')
        request.add_header('accept', 'application/x-sparql-compact')
        try:
            response = OPENER.open(request)
            return ResultReader(response)
        except Exception, e:
            print "HTTP Request failed.", e 
    else: # POST
        request = urllib2.Request("http://localhost:7654/sparql")
        request.add_header('accept', 'application/rdf+xml')
        request.add_header('accept', 'application/x-sparql-compact')
        data = urllib.urlencode(data, 1)
        try:
            response = urllib2.urlopen(request, data)
            return ResultReader(response)
        except Exception, e:
            print "HTTP Request failed.", e 
   
    
    
    
    
    
    
     
