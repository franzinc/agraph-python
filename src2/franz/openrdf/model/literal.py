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


from franz.openrdf.exceptions import *
from franz.openrdf.model.value import Value, URI
from franz.openrdf.query.commonlogic import XSD
from franz.openrdf.util import strings
from franz.openrdf.vocabulary import XMLSchema
import datetime
import time
from collections import defaultdict

class Literal(Value):
    """
    Lightweight implementation of the Literal class.  Think 'LiteralBase'.
    
    Implementer note: If this is still too heavyweight, subclass but don't
    call super.__init__.  That's why Python is so cool!
    """
    def __init__(self, label, datatype=None, language=None):
        self.datatype = datatype
        self.language = language
        self.label = label

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

        self._datatype = datatype

    datatype = property(getDatatype, setDatatype)

    def getLanguage(self):
        """The language for this Literal"""
        return self._language
    
    def setLanguage(self, language):
        """Set the language for this Literal"""
        ## THE SESAME CODE HAS AN INCONSISTENCY HERE; IT DOESN'T CONVERT TO LOWER CASE, BUT
        ## THE CONSTRUCTOR DOES CONVERT. WE ARE NOT SURE WHICH IS INTENDED  - RMM
        self._language = language.lower() if language else None    

    language = property(getLanguage, setLanguage)

    def getLabel(self):
        return self._label
    
    def setLabel(self, label):
        self._label = label    
    
    def getValue(self):
        return self.label

    label = property(getLabel, setLabel)
    
    def __eq__(self, other):
        if not isinstance(other, Literal): return False
        if not self.label == other.label: return False
        if not self.datatype:
            if other.datatype: return False
        elif not self.datatype == other.datatype: return False
        if not self.language:
            if other.language: return False
        elif not self.language == other.language: return False
        return True
    
    def __hash__(self):
        return hash(self.getLabel())
    
    def intValue(self):
        return int(self.getLabel())
    
    def longValue(self):
        return long(self.getLabel())
    
    def floatValue(self):
        return float(self.getLabel())
    
    def booleanValue(self):
        return bool(self.getLabel())
    
    def dateValue(self):
        return datetime.datetime.strptime(self.getLabel(), "%Y-%m-%d").date()

    def datetimeValue(self):
        return datetime.datetime.strptime(self.getLabel(), Literal.ISO_FORMAT_WITH_T)

    def timeValue(self):
        ## THIS IS GOING TO BREAK:
        return datetime.time(self.getLabel())

    
    ## Returns the {@link XMLGregorianCalendar} value of this literal. A calendar
    ## representation can be given for literals whose label conforms to the
    ## syntax of the following <a href="http://www.w3.org/TR/xmlschema-2/">XML
    ## Schema datatypes</a>: <tt>dateTime</tt>, <tt>time</tt>,
    ## <tt>date</tt>, <tt>gYearMonth</tt>, <tt>gMonthDay</tt>,
    ## <tt>gYear</tt>, <tt>gMonth</tt> or <tt>gDay</tt>.
    def calendarValue(self):
        raise NotImplementedError("calendarValue")

    def __str__(self):
        """
        Display an ntriples syntax for this literal.
        """
        sb = []
        sb.append('"')
        sb.append(strings.escape_double_quotes(self.getLabel()))
        sb.append('"')
        if self.language:
            sb.append('@')
            sb.append(self.language)
        if self.datatype:
            sb.append("^^")
            sb.append(str(self.datatype))
        return ''.join(sb)

    def toNTriples(self):
        """
        Return an NTriples representation of a resource, in this case, wrap
        it in angle brackets.
        """
        return str(self)

###############################################################################
## Automatic conversion from Literal to Python object
###############################################################################
    def toPython(self):
        """
        Return a Python object representation of this literal.   
        Slightly silly implementation because we implement a conversion table
        and then don't use the conversion functions.     
        """
        uri = getattr(self.getDatatype(), 'uri', None)
        conversion = XSDToPython[uri]
        return conversion(self)


XSDToPython = defaultdict(lambda: Literal.getValue, [
                (str(XMLSchema.INT), Literal.intValue),
                (str(XMLSchema.FLOAT), Literal.floatValue), 
                (str(XMLSchema.LONG), Literal.longValue),
                (str(XMLSchema.BOOLEAN), Literal.booleanValue),
                (str(XMLSchema.DATETIME), Literal.datetimeValue),
                (str(XMLSchema.DATE), Literal.dateValue),
                (str(XMLSchema.TIME), Literal.timeValue)])

        
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
        
        
