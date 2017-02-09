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
from __future__ import division
from __future__ import unicode_literals

import math
from decimal import Decimal

from future.utils import python_2_unicode_compatible
from past.utils import old_div

from past.builtins import long, unicode
from .value import Value, URI
from ..exceptions import IllegalArgumentException
from ..vocabulary.xmlschema import XMLSchema
from ..util import strings

import datetime
from collections import defaultdict

# Needed to temporarily convert times to datetimes to do arithmetic.
RANDOM_DAY = datetime.date(1984, 8, 26)


def datatype_from_python(value, datatype):
    """
    If 'value' is not a string, convert it into one, and infer its
    datatype, unless 'datatype' is set (i.e., overrides it).
    """
    if isinstance(value, unicode):
        return value, datatype

    if isinstance(value, bytes):
        return unicode(value, "utf-8"), datatype

    ## careful: test for 'bool' must precede test for 'int':
    if isinstance(value, bool):
        return unicode(value).lower(), datatype or XMLSchema.BOOLEAN

    if isinstance(value, int):
        return unicode(value), datatype or XMLSchema.INTEGER

    if isinstance(value, long):
        return unicode(value), datatype or XMLSchema.LONG

    if isinstance(value, float):
        return unicode(value), datatype or XMLSchema.DOUBLE

    if isinstance(value, datetime.datetime):
        # There is an ambiguity for times that occur twice due to
        # DST switches, but that is a problem with Python's standard
        # library and there is nothing we can do about it here.
        if value.utcoffset() is not None:
            value = value.replace(tzinfo=None) - value.utcoffset()
        str_value = value.isoformat() + 'Z'
        return str_value, datatype or XMLSchema.DATETIME

    if isinstance(value, datetime.time):
        if value.utcoffset() is not None:
            dt = datetime.datetime.combine(RANDOM_DAY, value)
            dt -= value.utcoffset()
            # Note: this will strip TZ
            value = dt.time()
        str_value = value.isoformat() + 'Z'
        return str_value, datatype or XMLSchema.TIME

    if isinstance(value, datetime.date):
        return value.isoformat(), datatype or XMLSchema.DATE

    if isinstance(value, Decimal):
        return unicode(value), datatype or XMLSchema.DECIMAL

    return unicode(value), datatype


class Literal(Value):
    """
    Implementation of the Literal class.
    """
    
    def __init__(self, label, datatype=None, language=None):
        Value.__init__(self)
        
        # Uses the properties to set the real values
        self.label, self.datatype = datatype_from_python(label, datatype)
        self.language = language

    def getDatatype(self):
        """The URI representing the datatype for this literal, if there is one""" 
        return self._datatype
    
    def setDatatype(self, datatype):
        """Sets the datatype of the value"""
        if isinstance(datatype, bytes):
            datatype = unicode(datatype, "utf-8")
        if isinstance(datatype, unicode):
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

    def get_cmp_key(self):
        return self.label, self.datatype, self.language
    
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
        return bool(self._label.capitalize())
    
    def dateValue(self):
        """Convert to date"""
        label = self._label
        if label.endswith('Z'):
            label = label[:-1]
        return datetime.datetime.strptime(label, "%Y-%m-%d").date()

    def datetimeValue(self):
        """Convert to datetime"""
        return _parse_iso(self._label)

    def timeValue(self):
        """Convert to time"""
        # Making this easy by picking a someone arbitrary date (that had a leap second just in case)
        # and reusing _parse_iso
        return _parse_iso('2008-12-31T' + self._label).time()
    
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
        """
        Return an NTriples representation for this Literal.
        """
        sb = []
        sb.append('"')
        sb.append(strings.encode_ntriple_string(self.getLabel()))
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
                (XMLSchema.DOUBLE.uri, Literal.floatValue), 
                (XMLSchema.LONG.uri, Literal.longValue),
                (XMLSchema.INTEGER.uri, Literal.intValue),
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

@python_2_unicode_compatible
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
    
    def __str__(self):
        return "|COOR|(%i, %i)" % (self.xcoor, self.ycoor)
    

class GeoSpatialRegion(CompoundLiteral):
    pass


@python_2_unicode_compatible
class GeoBox(GeoSpatialRegion):
    def __init__(self, xMin, xMax, yMin, yMax, unit=None, geoType=None):
        self.xMin = xMin
        self.xMax = xMax
        self.yMin = yMin
        self.yMax = yMax
        self.unit = unit
        self.geoType = geoType
    
    def __str__(self): return "|Box|%s,%s %s,%s" % (self.xMin, self.xMax, self.yMin, self.yMax)


@python_2_unicode_compatible
class GeoCircle(GeoSpatialRegion):
    def __init__(self, x, y, radius, unit=None, geoType=None):
        self.x = x
        self.y = y
        self.radius = radius
        self.unit = unit
        self.geoType=geoType
        
    def __str__(self): return "|Circle|%i,%i, radius=%i" % (self.x, self.y, self.radius)


@python_2_unicode_compatible
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
    
# The code below this line is modifed from the fixed_datetime.py file
# which can be found here:
#
# http://blog.twinapex.fi/2008/06/30/relativity-of-time-shortcomings-in-python-datetime-and-workaround/
#
# We don't wish to be dependant on pytz or monkeypatch datetime, so only
# portions were used.

# Copyright (c) 2008, Red Innovation Ltd., Finland
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#     * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
#     * Neither the name of Red Innovation nor the names of its contributors 
#       may be used to endorse or promote products derived from this software 
#       without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY RED INNOVATION ``AS IS'' AND ANY
# EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL RED INNOVATION BE LIABLE FOR ANY
# DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
# ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

def _parse_iso(timestamp):
    """Parses the given ISO 8601 compatible timestamp string 
    and converts it to fixed_datetime.datetime. The timestamp
    must conform to following formats:

         - the format is DATE SEP TIME TIMEZONE without
           any intervening spaces.

         - the date must be in format YYYY-MM-DD

         - the time may be either
             * HH:MM:SS,FFFF
             * HH:MM,FFFF
             * HH,FFFF
           FFFF is the fractional part. Decimal point can be
           used too.

         - the time zone must be either Z, -HH:MM or +HH:MM

         - the date and time must be separated either by
           whitespace or single T letter

         - the separators - and : may all be omitted, or
           must all be present.

         Examples (Unix Epoch):

             1970-01-01T00:00:00Z 
             1970-01-01T00Z 
             1969-12-31 19,5-04:30
             19700101T030000+0300
    """
    timestamp = timestamp.strip()
    
    m = _parse_iso.parser.match(timestamp)
    if not m:
        raise ValueError("%s: Not a proper ISO 8601 timestamp!" % timestamp)

    year  = int(m.group('year'))
    month = int(m.group('month'))
    day   = int(m.group('day'))
    
    h, min, s, us = None, None, None, 0
    frac = 0
    if m.group('tzempty') == None and m.group('tzh') == None:
        raise ValueError("Not a proper ISO 8601 timestamp: " +
                "missing timezone (Z or +hh[:mm])!")

    if m.group('frac'):
        frac = m.group('frac')
        power = len(frac)
        frac  = old_div(int(frac), 10.0 ** power)

    if m.group('hour'):
        h = int(m.group('hour'))

    if m.group('minute'):
        min = int(m.group('minute'))

    if m.group('second'):
        s = int(m.group('second'))

    if frac != None:
        # ok, fractions of hour?
        if min == None:
           frac, min = math.modf(frac * 60.0)
           min = int(min)

        # fractions of second?
        if s == None:
           frac, s = math.modf(frac * 60.0)
           s = int(s)

        # and extract microseconds...
        us = int(frac * 1000000)

    if m.group('tzempty') == 'Z':
        offsetmins = 0
    else:
        # timezone: hour diff with sign
        offsetmins = int(m.group('tzh')) * 60
        tzm = m.group('tzm')
      
        # add optional minutes
        if tzm != None:
            tzm = int(tzm)
            offsetmins += tzm if offsetmins > 0 else -tzm

    # For our use here, we should not be given non-zero offsets
    assert offsetmins == 0

    return datetime.datetime(year, month, day, h, min, s, us)

import re

_parse_iso.parser = re.compile("""
    ^
    (?P<year> [0-9]{4})(?P<ymdsep>-?)
    (?P<month>[0-9]{2})(?P=ymdsep)
    (?P<day>  [0-9]{2})

    (?: # time part... optional... at least hour must be specified
	(?:T|\s+)
        (?P<hour>[0-9]{2})
        (?:
            # minutes, separated with :, or none, from hours
            (?P<hmssep>[:]?)
            (?P<minute>[0-9]{2})
            (?:
                # same for seconds, separated with :, or none, from hours
                (?P=hmssep)
                (?P<second>[0-9]{2})
            )?
        )?
        
        # fractions
        (?: [,.] (?P<frac>[0-9]{1,10}))?

        # timezone, Z, +-hh or +-hh:?mm. MUST BE, but complain if not there.
        (
            (?P<tzempty>Z) 
        | 
            (?P<tzh>[+-][0-9]{2}) 
            (?: :? # optional separator 
                (?P<tzm>[0-9]{2})
            )?
        )?
    )?
    $
""", re.X) # """
