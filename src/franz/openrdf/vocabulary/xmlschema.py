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

NS = "http://www.w3.org/2001/XMLSchema#"

class XMLSchema(object):
    """
    A 'static' class containing useful XMLSchema URIs.
    """

    NAMESPACE = NS
    DURATION = URI(namespace=NS, localname="duration")
    DATETIME = URI(namespace=NS, localname="dateTime")
    TIME = URI(namespace=NS, localname="time")
    DATE = URI(namespace=NS, localname="date")
    GYEARMONTH = URI(namespace=NS, localname="gYearMonth")
    GYEAR = URI(namespace=NS, localname="gYear")
    GMONTHDAY = URI(namespace=NS, localname="gMonthDay")
    GDAY = URI(namespace=NS, localname="gDay")
    GMONTH = URI(namespace=NS, localname="gMonth")
    STRING = URI(namespace=NS, localname="string")
    BOOLEAN = URI(namespace=NS, localname="boolean")
    BASE64BINARY = URI(namespace=NS, localname="base64Binary")
    HEXBINARY = URI(namespace=NS, localname="hexBinary")
    FLOAT = URI(namespace=NS, localname="float")
    DECIMAL = URI(namespace=NS, localname="decimal")
    DOUBLE = URI(namespace=NS, localname="double")
    ANYURI = URI(namespace=NS, localname="anyURI")
    QNAME = URI(namespace=NS, localname="QName")
    NOTATION = URI(namespace=NS, localname="NOTATION")
    NORMALIZEDSTRING = URI(namespace=NS, localname="normalizedString")
    TOKEN = URI(namespace=NS, localname="token")
    LANGUAGE = URI(namespace=NS, localname="language")
    NMTOKEN = URI(namespace=NS, localname="NMTOKEN")
    NMTOKENS = URI(namespace=NS, localname="NMTOKENS")
    NAME = URI(namespace=NS, localname="Name")
    NCNAME = URI(namespace=NS, localname="NCName")
    ID = URI(namespace=NS, localname="ID")
    IDREF = URI(namespace=NS, localname="IDREF")
    IDREFS = URI(namespace=NS, localname="IDREFS")
    ENTITY = URI(namespace=NS, localname="ENTITY")
    ENTITIES = URI(namespace=NS, localname="ENTITIES")
    INTEGER = URI(namespace=NS, localname="integer")
    LONG = URI(namespace=NS, localname="long")
    INT = URI(namespace=NS, localname="int")
    SHORT = URI(namespace=NS, localname="short")
    NUMBER = URI(namespace=NS, localname="number")        
    BYTE = URI(namespace=NS, localname="byte")
    NON_POSITIVE_INTEGER = URI(namespace=NS, localname="nonPositiveInteger")
    NEGATIVE_INTEGER = URI(namespace=NS, localname="negativeInteger")
    NON_NEGATIVE_INTEGER = URI(namespace=NS, localname="nonNegativeInteger")
    POSITIVE_INTEGER = URI(namespace=NS, localname="positiveInteger")
    UNSIGNED_LONG = URI(namespace=NS, localname="unsignedLong")
    UNSIGNED_INT = URI(namespace=NS, localname="unsignedInt")
    UNSIGNED_SHORT = URI(namespace=NS, localname="unsignedShort")
    UNSIGNED_BYTE = URI(namespace=NS, localname="unsignedByte")        

