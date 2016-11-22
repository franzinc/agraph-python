#!/usr/bin/env python
# -*- coding: utf-8 -*-
# pylint: disable-msg=C0103

################################################################################
# Copyright (c) 2006-2017 Franz Inc.  
# All rights reserved. This program and the accompanying materials are
# made available under the terms of the MIT License which accompanies
# this distribution, and is available at http://opensource.org/licenses/MIT
################################################################################

from __future__ import absolute_import
from __future__ import unicode_literals
from future.builtins import object
from past.builtins import unicode

from future.utils import iteritems

from ..model.value import URI 

NS = "http://www.w3.org/2000/01/rdf-schema#"

class RDFS(object):
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





