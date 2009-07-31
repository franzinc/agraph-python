#!/usr/bin/env python
# -*- coding: utf-8 -*-
# pylint: disable-msg=C0103

##***** BEGIN LICENSE BLOCK *****
##Version: MPL 1.1
##
##The contents of this file are subject to the Mozilla Public License Version
##1.1 (the "License") you may not use this file except in compliance with
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


from franz.openrdf.model.value import URI 

NS = "http://www.w3.org/2000/01/rdf-schema#"

class RDFS:
    """
    A 'static' class containing useful RDFS URIs.
    """

    NAMESPACE = NS
    RESOURCE = URI(namespace=NS, localname="Resource")
    LITERAL = URI(namespace=NS, localname="Literal")
    CLASS = URI(namespace=NS, localname="Class")
    SUBCLASSOF = URI(namespace=NS, localname="subClassOf")
    SUBPROPERTYOF = URI(namespace=NS, localname="subPropertyOf")
    DOMAIN = URI(namespace=NS, localname="domain")
    RANGE = URI(namespace=NS, localname="range")
    COMMENT = URI(namespace=NS, localname="comment")
    LABEL = URI(namespace=NS, localname="label")
    DATATYPE = URI(namespace=NS, localname="Datatype")
    CONTAINER = URI(namespace=NS, localname="Container")
    MEMBER = URI(namespace=NS, localname="member")
    ISDEFINEDBY = URI(namespace=NS, localname="isDefinedBy")
    SEEALSO = URI(namespace=NS, localname="seeAlso")
    CONTAINERMEMBERSHIPPROPERTY = URI(namespace=NS,
	localname="ContainerMembershipProperty")
       
    ## map of uri strings to URI objects:
    uristr2obj = {}

for name, uri in RDFS.__dict__.iteritems():
    if name.upper() == name:
        RDFS.uristr2obj[str(uri)] = uri

del RDFS.uristr2obj[NS]





