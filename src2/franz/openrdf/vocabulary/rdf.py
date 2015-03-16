#!/usr/bin/env python
# -*- coding: utf-8 -*-
# pylint: disable-msg=C0103

###############################################################################
# Copyright (c) 2006-2015 Franz Inc.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the Eclipse Public License v1.0
# which accompanies this distribution, and is available at
# http://www.eclipse.org/legal/epl-v10.html
###############################################################################

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
