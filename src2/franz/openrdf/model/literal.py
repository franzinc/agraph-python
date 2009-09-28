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

from .value import Value, URI
from ..exceptions import IllegalArgumentException
from ..util import strings
from ..vocabulary import XMLSchema

import datetime
from collections import defaultdict


def datatype_from_python(value, datatype):
    """
    If 'value' is not a string, convert it into one, and infer its
    datatype, unless 'datatype' is set (i.e., overrides it).
    """
    if isinstance(value, str):
        return value, datatype

    ## careful: test for 'bool' must precede test for 'int':
    if isinstance(value, bool):
        return str(value), datatype or XMLSchema.BOOLEAN

    if isinstance(value, int):
        return str(value), datatype or XMLSchema.INT

    if isinstance(value, float):
        return str(value), datatype or XMLSchema.FLOAT

    if isinstance(value, datetime.datetime):
        ## TODO: NEED TO ADD TIMEZONE VALUE??:
        value = value.strftime(Literal.ISO_FORMAT_WITH_T) ## truncate microseconds  
        return value, datatype or XMLSchema.DATETIME

    if isinstance(value, datetime.time):
        value = value.strftime(Literal.ISO_FORMAT_WITH_T) ## UNTESTED
        return str(value), datatype or XMLSchema.TIME

    if isinstance(value, datetime.date):
        value = value.strftime(Literal.ISO_FORMAT_WITH_T) ## UNTESTED
        return str(value), datatype or XMLSchema.DATE

    return str(value), datatype


class Literal(Value):
    """
    Lightweight implementation of the Literal class.  Think 'LiteralBase'.
    
    Implementer note: If this is still too heavyweight, subclass but don't
    call super.__init__.  That's why Python is so cool!
    """
    def __init__(self, label, datatype=None, language=None):
        Value.__init__(self)
        
        # Uses the properties to set the real values
        self.label, self.datatype = datatype_from_python(label, datatype)
        self.language = language

    ISO_FORMAT_WITH_T = "%Y-%m-%dT%H:%M:%S"

    def getDatatype(self):
        """The URI representing the datatype for this literal, if there is one""" 
        return self._datatype
    
    def setDatatype(self, datatype):
        """Sets the datatype of the value"""
        if isinstance(datatype, str):
            if datatype[0] == '<':
                datatype = datatype[1:-1]
            datatype = XMLSchema.uristr2obj.get(datatype, None) or URI(datatype)
        elif datatype is not None:
            if not isinstance(datatype, URI):
                datatype = URI(datatype)
            elif datatype.uri is None:
                datatype = None

        self._datatype = datatype # pylint: disable-msg=W0201

    datatype = property(getDatatype, setDatatype)

    def getLanguage(self):
        """The language for this Literal"""
        return self._language
    
    def setLanguage(self, language):
        """Set the language for this Literal"""
        self._language = language.lower() if language else None # pylint: disable-msg=W0201

    language = property(getLanguage, setLanguage)

    def getLabel(self):
        """The label/value for this Literal"""
        return self._label
    
    def setLabel(self, label):
        """Set the label for this Literal"""
        self._label = label # pylint: disable-msg=W0201
    
    def getValue(self):
        """The label/value"""
        return self.label

    label = property(getLabel, setLabel)
    
    def __eq__(self, other):
        if not isinstance(other, Literal):
            return NotImplemented

        return (self.label == other.label and 
                self.datatype == other.datatype and
                self.language == other.language)
    
    def __hash__(self):
        return hash(self._label)
    
    def intValue(self):
        """Convert to int"""
        return int(self._label)
    
    def longValue(self):
        """Convert to long"""
        return long(self._label)
    
    def floatValue(self):
        """Convert to float"""
        return float(self._label)
    
    def booleanValue(self):
        """Convert to bool"""
        return bool(self._label)
    
    def dateValue(self):
        """Convert to date"""
        return datetime.datetime.strptime(self._label, "%Y-%m-%d").date()

    def datetimeValue(self):
        """Convert to datetime"""
        return datetime.datetime.strptime(self._label, Literal.ISO_FORMAT_WITH_T)

    def timeValue(self):
        """Convert to time"""
        ## THIS IS GOING TO BREAK:
        return datetime.time(self._label)

    
    ## Returns the {@link XMLGregorianCalendar} value of this literal. A calendar
    ## representation can be given for literals whose label conforms to the
    ## syntax of the following <a href="http://www.w3.org/TR/xmlschema-2/">XML
    ## Schema datatypes</a>: <tt>dateTime</tt>, <tt>time</tt>,
    ## <tt>date</tt>, <tt>gYearMonth</tt>, <tt>gMonthDay</tt>,
    ## <tt>gYear</tt>, <tt>gMonth</tt> or <tt>gDay</tt>.
    def calendarValue(self):
        """calendarValue not useful for Python."""
        raise NotImplementedError("calendarValue")

    def toNTriples(self):
        """Return an ntriples syntax for this Literal"""
        sb = []
        sb.append('"')
        sb.append(strings.escape_double_quotes(self.getLabel()))
        sb.append('"')
        if self.language:
            sb.append('@')
            sb.append(self.language)
        if self.datatype:
            sb.append("^^")
            sb.append(self.datatype.toNTriples())
        return ''.join(sb)

###############################################################################
## Automatic conversion from Literal to Python object
###############################################################################
    def toPython(self):
        """
        Return a Python object representation of this literal.   
        Slightly silly implementation because we implement a conversion table
        and then don't use the conversion functions.     
        """
        return XSDToPython[getattr(self.datatype, "uri", None)](self)


XSDToPython = defaultdict(lambda: Literal.getValue, [
                (XMLSchema.INT.uri, Literal.intValue),
                (XMLSchema.FLOAT.uri, Literal.floatValue), 
                (XMLSchema.LONG.uri, Literal.longValue),
                (XMLSchema.BOOLEAN.uri, Literal.booleanValue),
                (XMLSchema.DATETIME.uri, Literal.datetimeValue),
                (XMLSchema.DATE.uri, Literal.dateValue),
                (XMLSchema.TIME.uri, Literal.timeValue)])


###############################################################################
# Extension to Sesame API
###############################################################################

class CompoundLiteral(Literal):
    """
    A compound literal represents a range, a geospatial coordinate,
    or other useful compound structure.
    TODO: FIGURE OUT SYNTAX FOR OTHER TYPES. INSURE THAT
    THE SYNTAX FOR A RANGE DOESN'T CONFLICT/OVERLAP
    """
    RANGE_LITERAL = 'rangeLiteral'
    def __init__(self, choice, lowerBound=None, upperBound=None):
        self.choice = choice
        if choice == CompoundLiteral.RANGE_LITERAL:
            self.lowerBound = lowerBound # should be a LiteralImpl
            self.upperBound = upperBound # should be a LiteralImpl
        ## other compound types go here.
        else:
            raise IllegalArgumentException("Can't interpret the choice '%s' of a compound literal." % choice)
    
    def isRangeLiteral(self):
        return self.choice == CompoundLiteral.RANGE_LITERAL
    
    def getLowerBound(self):
        return self.lowerBound
    
    def getUpperBound(self):
        return self.upperBound
    
class RangeLiteral(CompoundLiteral):
    """
    A range represents an interval between to scalar values.
    """
    def __init__(self, lowerBound=None, upperBound=None):
        self.lowerBound = lowerBound # should be a LiteralImpl
        self.upperBound = upperBound # should be a LiteralImpl
    
    def getLowerBound(self):
        return self.lowerBound
    
    def getUpperBound(self):
        return self.upperBound

class GeoCoordinate(CompoundLiteral):
    """
    Define either a cartesian coordinate or a spherical coordinate.  For the
    latter, nit can be 'km', 'mile', 'radian', or 'degree'
    """
    def __init__(self, x, y, unit=None, geoType=None):
        self.xcoor = x
        self.ycoor = y
        self.unit = unit
        self.geoType = geoType
    
    def __str__(self): return "|COOR|(%i, %i)" % (self.xcoor, self.ycoor)
    
class GeoSpatialRegion(CompoundLiteral):
    pass

class GeoBox(GeoSpatialRegion):
    def __init__(self, xMin, xMax, yMin, yMax, unit=None, geoType=None):
        self.xMin = xMin
        self.xMax = xMax
        self.yMin = yMin
        self.yMax = yMax
        self.unit = unit
        self.geoType = geoType
    
    def __str__(self): return "|Box|%s,%s %s,%s" % (self.xMin, self.xMax, self.yMin, self.yMax)
        
class GeoCircle(GeoSpatialRegion):
    def __init__(self, x, y, radius, unit=None, geoType=None):
        self.x = x
        self.y = y
        self.radius = radius
        self.unit = unit
        self.geoType=geoType
        
    def __str__(self): return "|Circle|%i,%i, radius=%i" % (self.x, self.y, self.radius)

class GeoPolygon(GeoSpatialRegion):
    def __init__(self, vertices, uri=None, geoType=None):
        self.vertices = vertices
        self.geoType = geoType
        self.uri = uri
        self.resource = None
        self.miniPolygon = None
        
    def getVertices(self): return self.vertices
    
    def getResource(self): return self.resource
    
    def __str__(self): return "|Polygon|%s" % self.vertices
    
