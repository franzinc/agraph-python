# pylint: disable-msg=C0103

################################################################################
# Copyright (c) 2006-2017 Franz Inc.
# All rights reserved. This program and the accompanying materials are
# made available under the terms of the MIT License which accompanies
# this distribution, and is available at http://opensource.org/licenses/MIT
################################################################################


import datetime
from collections import OrderedDict, defaultdict
from decimal import Decimal

import iso8601

from franz.openrdf.exceptions import IllegalArgumentException
from franz.openrdf.model.value import LITERAL_CMP_KEY, URI, Value
from franz.openrdf.util import strings
from franz.openrdf.vocabulary.xmlschema import XMLSchema

# Needed to temporarily convert times to datetimes to do arithmetic.
# Python time objects do not support adding/subtracting time deltas,
# but datetimes do. We need that operation to normalize times to UTC.
RANDOM_DAY = datetime.date(1984, 8, 26)
RANDOM_DAY_STRING = RANDOM_DAY.isoformat()


def datatype_from_python(value, datatype):
    """
    If 'value' is not a string, convert it into one, and infer its
    datatype, unless 'datatype' is set (i.e., overrides it).
    """
    if isinstance(value, str):
        return value, datatype

    if isinstance(value, bytes):
        return str(value, "utf-8"), datatype

    ## careful: test for 'bool' must precede test for 'int':
    if isinstance(value, bool):
        return str(value).lower(), datatype or XMLSchema.BOOLEAN

    if isinstance(value, int):
        return str(value), datatype or XMLSchema.INTEGER

    if isinstance(value, float):
        return str(value), datatype or XMLSchema.DOUBLE

    if isinstance(value, datetime.datetime):
        # There is an ambiguity for times that occur twice due to
        # DST switches, but that is a problem with Python's standard
        # library and there is nothing we can do about it here.
        if value.utcoffset() is not None:
            value = value.replace(tzinfo=None) - value.utcoffset()
        str_value = value.isoformat() + "Z"
        return str_value, datatype or XMLSchema.DATETIME

    if isinstance(value, datetime.time):
        if value.utcoffset() is not None:
            dt = datetime.datetime.combine(RANDOM_DAY, value)
            dt -= value.utcoffset()
            # Note: this will strip TZ
            value = dt.time()
        str_value = value.isoformat() + "Z"
        return str_value, datatype or XMLSchema.TIME

    if isinstance(value, datetime.date):
        return value.isoformat(), datatype or XMLSchema.DATE

    if isinstance(value, Decimal):
        return str(value), datatype or XMLSchema.DECIMAL

    return str(value), datatype


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
            datatype = str(datatype, "utf-8")
        if isinstance(datatype, str):
            if datatype[0] == "<":
                datatype = datatype[1:-1]
            datatype = URI(datatype)
        elif datatype is not None:
            if not isinstance(datatype, URI):
                datatype = URI(datatype)
            elif datatype.uri is None:
                datatype = None

        self._datatype = datatype  # pylint: disable-msg=W0201

    datatype = property(getDatatype, setDatatype)

    def getLanguage(self):
        """The language for this Literal"""
        return self._language

    def setLanguage(self, language):
        """Set the language for this Literal"""
        self._language = (
            language.lower() if language else None
        )  # pylint: disable-msg=W0201

    language = property(getLanguage, setLanguage)

    def getLabel(self):
        """The label/value for this Literal"""
        return self._label

    def setLabel(self, label):
        """Set the label for this Literal"""
        self._label = label  # pylint: disable-msg=W0201

    def getValue(self):
        """The label/value"""
        return self.label

    label = property(getLabel, setLabel)

    def get_cmp_key(self):
        return LITERAL_CMP_KEY, self.label, self.datatype, self.language

    def intValue(self):
        """Convert to int"""
        return int(self._label)

    def longValue(self):
        """Convert to long"""
        return int(self._label)

    def floatValue(self):
        """Convert to float"""
        return float(self._label)

    def booleanValue(self):
        """Convert to bool"""
        return self._label == "true"

    def decimalValue(self):
        """Convert to a decimal"""
        return Decimal(self._label)

    def dateValue(self):
        """Convert to date"""
        # iso8601 can parse a date, but it will still be
        # returned as a datetime.datetime object. So we have
        # to extract the date (as a datetime.date object) from it.
        return iso8601.parse_date(self._label).date()

    def datetimeValue(self):
        """Convert to datetime"""
        return iso8601.parse_date(self._label, default_timezone=None)

    def timeValue(self):
        """Convert to time"""
        # iso8601 can't parse a time
        # We trick it by prepending a random date,
        # parsing the result as a datetime and extracting
        # the time from that.
        label = RANDOM_DAY_STRING + "T" + self._label
        # Return a naive time if there is no timezone.
        # The default is UTC.
        dt = iso8601.parse_date(label, default_timezone=None)
        # Note that time() strips the time zone
        return dt.time().replace(tzinfo=dt.tzinfo)

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
            sb.append("@")
            sb.append(self.language)
        if self.datatype:
            sb.append("^^")
            sb.append(self.datatype.toNTriples())
        return "".join(sb)

    def to_json_ld(self):
        """Converts to an object to be used as a JSON-LD value."""
        if self.language:
            return OrderedDict([("@value", self.label), ("@language", self.language)])

        if self.datatype is None or self.datatype == XMLSchema.STRING:
            return self.label

        if self.datatype == XMLSchema.INTEGER:
            return int(self.label)

        return OrderedDict(
            [("@value", self.label), ("@type", self.datatype.to_json_ld_key())]
        )

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


XSDToPython = defaultdict(
    lambda: Literal.getValue,
    [
        (XMLSchema.INT.uri, Literal.intValue),
        (XMLSchema.FLOAT.uri, Literal.floatValue),
        (XMLSchema.DOUBLE.uri, Literal.floatValue),
        (XMLSchema.LONG.uri, Literal.longValue),
        (XMLSchema.INTEGER.uri, Literal.intValue),
        (XMLSchema.BOOLEAN.uri, Literal.booleanValue),
        (XMLSchema.DATETIME.uri, Literal.datetimeValue),
        (XMLSchema.DATE.uri, Literal.dateValue),
        (XMLSchema.TIME.uri, Literal.timeValue),
        (XMLSchema.DECIMAL.uri, Literal.decimalValue),
    ],
)


###############################################################################
# Extension to RDF4J API
###############################################################################


class CompoundLiteral(Literal):
    """
    A compound literal represents a range, a geospatial coordinate,
    or other useful compound structure.
    TODO: FIGURE OUT SYNTAX FOR OTHER TYPES. INSURE THAT
    THE SYNTAX FOR A RANGE DOESN'T CONFLICT/OVERLAP
    """

    RANGE_LITERAL = "rangeLiteral"

    def __init__(self, choice, lowerBound=None, upperBound=None):
        self.choice = choice
        if choice == CompoundLiteral.RANGE_LITERAL:
            self.lowerBound = lowerBound  # should be a LiteralImpl
            self.upperBound = upperBound  # should be a LiteralImpl
        ## other compound types go here.
        else:
            raise IllegalArgumentException(
                "Can't interpret the choice '%s' of a compound literal." % choice
            )

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
        self.lowerBound = lowerBound  # should be a LiteralImpl
        self.upperBound = upperBound  # should be a LiteralImpl

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

    def __str__(self):
        return "|COOR|(%i, %i)" % (self.xcoor, self.ycoor)


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

    def __str__(self):
        return "|Box|%s,%s %s,%s" % (self.xMin, self.xMax, self.yMin, self.yMax)


class GeoCircle(GeoSpatialRegion):
    def __init__(self, x, y, radius, unit=None, geoType=None):
        self.x = x
        self.y = y
        self.radius = radius
        self.unit = unit
        self.geoType = geoType

    def __str__(self):
        return "|Circle|%i,%i, radius=%i" % (self.x, self.y, self.radius)


class GeoPolygon(GeoSpatialRegion):
    def __init__(self, vertices, uri=None, geoType=None):
        self.vertices = vertices
        self.geoType = geoType
        self.uri = uri
        self.resource = None
        self.miniPolygon = None

    def getVertices(self):
        return self.vertices

    def getResource(self):
        return self.resource

    def __str__(self):
        return "|Polygon|%s" % self.vertices
