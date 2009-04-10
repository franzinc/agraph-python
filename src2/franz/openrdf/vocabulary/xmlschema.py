#!/usr/bin/env python
# -*- coding: utf-8 -*-

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


from franz.openrdf.exceptions import *

class XMLSchema:
    NAMESPACE = "http://www.w3.org/2001/XMLSchema#"
    DURATION = None 
    DATETIME = None 
    TIME = None 
    DATE = None 
    GYEARMONTH = None 
    GYEAR = None 
    GMONTHDAY = None 
    GDAY = None 
    GMONTH = None 
    STRING = None 
    BOOLEAN = None 
    BASE64BINARY = None 
    HEXBINARY = None 
    FLOAT = None 
    DECIMAL = None 
    DOUBLE = None 
    ANYURI = None 
    QNAME = None 
    NOTATION = None 
    NORMALIZEDSTRING = None 
    TOKEN = None 
    LANGUAGE = None 
    NMTOKEN = None 
    NMTOKENS = None 
    NAME = None 
    NCNAME = None 
    ID = None 
    IDREF = None 
    IDREFS = None 
    ENTITY = None 
    ENTITIES = None 
    INTEGER = None 
    LONG = None 
    INT = None 
    SHORT = None
    NUMBER = None 
    BYTE = None 
    NON_POSITIVE_INTEGER = None 
    NEGATIVE_INTEGER = None 
    NON_NEGATIVE_INTEGER = None 
    POSITIVE_INTEGER = None 
    UNSIGNED_LONG = None 
    UNSIGNED_INT = None 
    UNSIGNED_SHORT = None 
    UNSIGNED_BYTE = None 
    
    ## map of uri strings to URI objects:
    #name2URI = {}
    
    @staticmethod
    def initialize(factory):
        """
        Initialize the constant using factory 'factory'
        """
        XMLSchema.DURATION = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="duration")
        XMLSchema.DATETIME = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="dateTime")
        XMLSchema.TIME = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="time")
        XMLSchema.DATE = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="date")
        XMLSchema.GYEARMONTH = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="gYearMonth")
        XMLSchema.GYEAR = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="gYear")
        XMLSchema.GMONTHDAY = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="gMonthDay")
        XMLSchema.GDAY = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="gDay")
        XMLSchema.GMONTH = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="gMonth")
        XMLSchema.STRING = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="string")
        XMLSchema.BOOLEAN = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="boolean")
        XMLSchema.BASE64BINARY = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="base64Binary")
        XMLSchema.HEXBINARY = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="hexBinary")
        XMLSchema.FLOAT = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="float")
        XMLSchema.DECIMAL = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="decimal")
        XMLSchema.DOUBLE = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="double")
        XMLSchema.ANYURI = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="anyURI")
        XMLSchema.QNAME = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="QName")
        XMLSchema.NOTATION = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="NOTATION")
        XMLSchema.NORMALIZEDSTRING = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="normalizedString")
        XMLSchema.TOKEN = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="token")
        XMLSchema.LANGUAGE = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="language")
        XMLSchema.NMTOKEN = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="NMTOKEN")
        XMLSchema.NMTOKENS = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="NMTOKENS")
        XMLSchema.NAME = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="Name")
        XMLSchema.NCNAME = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="NCName")
        XMLSchema.ID = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="ID")
        XMLSchema.IDREF = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="IDREF")
        XMLSchema.IDREFS = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="IDREFS")
        XMLSchema.ENTITY = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="ENTITY")
        XMLSchema.ENTITIES = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="ENTITIES")
        XMLSchema.INTEGER = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="integer")
        XMLSchema.LONG = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="long")
        XMLSchema.INT = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="int")
        XMLSchema.SHORT = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="short")
        XMLSchema.NUMBER = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="number")        
        XMLSchema.BYTE = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="byte")
        XMLSchema.NON_POSITIVE_INTEGER = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="nonPositiveInteger")
        XMLSchema.NEGATIVE_INTEGER = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="negativeInteger")
        XMLSchema.NON_NEGATIVE_INTEGER = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="nonNegativeInteger")
        XMLSchema.POSITIVE_INTEGER = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="positiveInteger")
        XMLSchema.UNSIGNED_LONG = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="unsignedLong")
        XMLSchema.UNSIGNED_INT = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="unsignedInt")
        XMLSchema.UNSIGNED_SHORT = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="unsignedShort")
        XMLSchema.UNSIGNED_BYTE = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="unsignedByte")        
        
        ## (re)build 'name2URI' dictionary
        XMLSchema.name2URIMap = {}
        for uri in [XMLSchema.DURATION, XMLSchema.DATETIME, XMLSchema.TIME, XMLSchema.DATE,  XMLSchema.GYEARMONTH, 
                XMLSchema.GYEAR, XMLSchema.GMONTHDAY, XMLSchema.GDAY, XMLSchema.GMONTH, XMLSchema.STRING, 
                XMLSchema.BOOLEAN, XMLSchema.BASE64BINARY, XMLSchema.HEXBINARY, XMLSchema.FLOAT, XMLSchema.DECIMAL, 
                XMLSchema.DOUBLE, XMLSchema.ANYURI, XMLSchema.QNAME, XMLSchema.NOTATION, XMLSchema.NORMALIZEDSTRING, 
                XMLSchema.TOKEN, XMLSchema.LANGUAGE, XMLSchema.NMTOKEN, XMLSchema.NMTOKENS, XMLSchema.NAME, 
                XMLSchema.NCNAME, XMLSchema.ID, XMLSchema.IDREF, XMLSchema.IDREFS, XMLSchema.ENTITY, XMLSchema.ENTITIES, 
                XMLSchema.INTEGER, XMLSchema.LONG, XMLSchema.INT, XMLSchema.SHORT, XMLSchema.BYTE, XMLSchema.NON_POSITIVE_INTEGER, 
                XMLSchema.NEGATIVE_INTEGER, XMLSchema.NON_NEGATIVE_INTEGER, XMLSchema.POSITIVE_INTEGER, XMLSchema.UNSIGNED_LONG, 
                XMLSchema.UNSIGNED_INT, XMLSchema.UNSIGNED_SHORT, XMLSchema.UNSIGNED_BYTE,]:
            XMLSchema.name2URIMap[str(uri)] = uri

    @staticmethod
    def name2URI (name, exception_if_failure=True):
        """
        Given a URI string, return the OpenRDF URI object.
        """
        matchingURI = XMLSchema.name2URIMap.get(name)
        if matchingURI: return matchingURI
        elif exception_if_failure:
            raise IllegalArgumentException("Passed a non-XSD URI to 'XMLSchema.name2URI.")
        else: return None

#    @staticmethod
#    def reinitialize(factory, store=None):
#        """
#        Initialize the values in the factory, or
#        reinitialize the values in factory with more efficient
#        resources and literals (one's that know what store they
#        belong to).
#        """
#        XMLSchema.DURATION = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="duration")
#        XMLSchema.DATETIME = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="dateTime")
#        XMLSchema.TIME = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="time")
#        XMLSchema.DATE = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="date")
#        XMLSchema.GYEARMONTH = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="gYearMonth")
#        XMLSchema.GYEAR = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="gYear")
#        XMLSchema.GMONTHDAY = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="gMonthDay")
#        XMLSchema.GDAY = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="gDay")
#        XMLSchema.GMONTH = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="gMonth")
#        XMLSchema.STRING = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="string")
#        XMLSchema.BOOLEAN = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="boolean")
#        XMLSchema.BASE64BINARY = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="base64Binary")
#        XMLSchema.HEXBINARY = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="hexBinary")
#        XMLSchema.FLOAT = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="float")
#        XMLSchema.DECIMAL = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="decimal")
#        XMLSchema.DOUBLE = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="double")
#        XMLSchema.ANYURI = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="anyURI")
#        XMLSchema.QNAME = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="QName")
#        XMLSchema.NOTATION = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="NOTATION")
#        XMLSchema.NORMALIZEDSTRING = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="normalizedString")
#        XMLSchema.TOKEN = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="token")
#        XMLSchema.LANGUAGE = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="language")
#        XMLSchema.NMTOKEN = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="NMTOKEN")
#        XMLSchema.NMTOKENS = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="NMTOKENS")
#        XMLSchema.NAME = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="Name")
#        XMLSchema.NCNAME = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="NCName")
#        XMLSchema.ID = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="ID")
#        XMLSchema.IDREF = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="IDREF")
#        XMLSchema.IDREFS = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="IDREFS")
#        XMLSchema.ENTITY = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="ENTITY")
#        XMLSchema.ENTITIES = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="ENTITIES")
#        XMLSchema.INTEGER = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="integer")
#        XMLSchema.LONG = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="long")
#        XMLSchema.INT = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="int")
#        XMLSchema.SHORT = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="short")
#        XMLSchema.BYTE = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="byte")
#        XMLSchema.NON_POSITIVE_INTEGER = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="nonPositiveInteger")
#        XMLSchema.NEGATIVE_INTEGER = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="negativeInteger")
#        XMLSchema.NON_NEGATIVE_INTEGER = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="nonNegativeInteger")
#        XMLSchema.POSITIVE_INTEGER = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="positiveInteger")
#        XMLSchema.UNSIGNED_LONG = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="unsignedLong")
#        XMLSchema.UNSIGNED_INT = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="unsignedInt")
#        XMLSchema.UNSIGNED_SHORT = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="unsignedShort")
#        XMLSchema.UNSIGNED_BYTE = factory.createURI(namespace=XMLSchema.NAMESPACE, localname="unsignedByte")
#        ## (re)build 'name2URI' dictionary
#        XMLSchema.name2URIMap = {}
#        for uri in [XMLSchema.DURATION, XMLSchema.DATETIME, XMLSchema.TIME, XMLSchema.DATE,  XMLSchema.GYEARMONTH, 
#                XMLSchema.GYEAR, XMLSchema.GMONTHDAY, XMLSchema.GDAY, XMLSchema.GMONTH, XMLSchema.STRING, 
#                XMLSchema.BOOLEAN, XMLSchema.BASE64BINARY, XMLSchema.HEXBINARY, XMLSchema.FLOAT, XMLSchema.DECIMAL, 
#                XMLSchema.DOUBLE, XMLSchema.ANYURI, XMLSchema.QNAME, XMLSchema.NOTATION, XMLSchema.NORMALIZEDSTRING, 
#                XMLSchema.TOKEN, XMLSchema.LANGUAGE, XMLSchema.NMTOKEN, XMLSchema.NMTOKENS, XMLSchema.NAME, 
#                XMLSchema.NCNAME, XMLSchema.ID, XMLSchema.IDREF, XMLSchema.IDREFS, XMLSchema.ENTITY, XMLSchema.ENTITIES, 
#                XMLSchema.INTEGER, XMLSchema.LONG, XMLSchema.INT, XMLSchema.SHORT, XMLSchema.BYTE, XMLSchema.NON_POSITIVE_INTEGER, 
#                XMLSchema.NEGATIVE_INTEGER, XMLSchema.NON_NEGATIVE_INTEGER, XMLSchema.POSITIVE_INTEGER, XMLSchema.UNSIGNED_LONG, 
#                XMLSchema.UNSIGNED_INT, XMLSchema.UNSIGNED_SHORT, XMLSchema.UNSIGNED_BYTE,]:
#            XMLSchema.name2URIMap[str(uri)] = uri
    
    



