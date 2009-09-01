#!/usr/bin/env python
# -*- coding: utf-8 -*-
# pylint: disable-msg=C0103

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

from __future__ import absolute_import

from ..model.value import URI

NS = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"

class RDF:
    """
    A 'static' class containing useful RDF URIs.
    """

    NAMESPACE = NS
    TYPE = URI(namespace=NS, localname="type")
    PROPERTY = URI(namespace=NS, localname="Property")
    XMLLITERAL = URI(namespace=NS, localname="XMLLiteral")
    SUBJECT = URI(namespace=NS, localname="subject")
    PREDICATE = URI(namespace=NS, localname="predicate")
    OBJECT = URI(namespace=NS, localname="object")
    STATEMENT = URI(namespace=NS, localname="Statement")
    BAG = URI(namespace=NS, localname="Bag")
    ALT = URI(namespace=NS, localname="Alt")
    SEQ = URI(namespace=NS, localname="Seq")
    VALUE = URI(namespace=NS, localname="value")
    LI = URI(namespace=NS, localname="li")
    LIST = URI(namespace=NS, localname="List")
    FIRST = URI(namespace=NS, localname="first")
    REST = URI(namespace=NS, localname="rest")
    NIL = URI(namespace=NS, localname="nil")
    
    ## map of uri strings to URI objects:
    uristr2obj = {}

for name, uri in RDF.__dict__.iteritems():
    if name.upper() == name:
        RDF.uristr2obj[str(uri)] = uri

del RDF.uristr2obj[NS]
