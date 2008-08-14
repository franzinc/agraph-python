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
from franz.openrdf.model.value import Value
from franz.openrdf.vocabulary.xmlschema import XMLSchema

class Literal(Value):
    """
    Lightweight implementation of the Literal class.  Think 'LiteralBase'.
    
    Implementer note: If this is still too heavyweight, subclass but don't
    call super.__init__.  That's why Python is so cool!
    """
    XSDToPython = {}
    def __init__(self, label, datatype=None, language=None):
        self.datatype = datatype
        self.language = language.lower() if language else None
        self.label = label

    def getLabel(self): return self.label

    def setLabel(self, label): self.label = label    
    
    def getLanguage(self): return self.language
    
    def setLanguage(self, language):
        ## THE SESAME CODE HAS AN INCONSISTENCY HERE; IT DOESN'T CONVERT TO LOWER CASE, BUT
        ## THE CONSTRUCTOR DOES CONVERT. WE ARE NOT SURE WHICH IS INTENDED  - RMM
        self.language = language.lower() if language else None    
    
    def getDatatype(self):
        """
        Return a URI representing the datatype for this literal, if there is one
        """ 
        return self.datatype
    
    def setDatatype(self, datatype):
        self.datatype = datatype

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
        return self.getLabel().__hash__()
    
    def intValue(self):
        return int(self.getLabel())
    
    def longValue(self):
        return long(self.getLabel())
    
    def floatValue(self):
        return float(self.getLabel())
    
    def booleanValue(self):
        return bool(self.getLabel())
    
    ## Returns the {@link XMLGregorianCalendar} value of this literal. A calendar
    ## representation can be given for literals whose label conforms to the
    ## syntax of the following <a href="http://www.w3.org/TR/xmlschema-2/">XML
    ## Schema datatypes</a>: <tt>dateTime</tt>, <tt>time</tt>,
    ## <tt>date</tt>, <tt>gYearMonth</tt>, <tt>gMonthDay</tt>,
    ## <tt>gYear</tt>, <tt>gMonth</tt> or <tt>gDay</tt>.
    def calendarValue(self):
        raise UnimplementedMethodException("calendarValue")

    def __str__(self):
        sb = []
        sb.append('"')
        sb.append(self.getLabel())
        sb.append('"')
        if self.language:
            sb.append('@')
            sb.append(self.language)
        if self.datatype:
            sb.append("^^")
            sb.append(str(self.datatype))
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
        dt = self.getDatatype()
        if dt is None: return self.getLabel()
        else:
            conversion = Literal.XSDToPython.get(dt.getURI())
            if conversion:
                if conversion == int:
                    return self.intValue()
                elif conversion == long:
                    return self.longValue()
                elif conversion == float:
                    return self.floatValue()
                elif conversion == bool:
                    return self.booleanValue()
                else:
                    return conversion(self.label)
            else:
                return self.label
    
    @staticmethod
    def reinitialize():        
        Literal.XSDToPython = {
            str(XMLSchema.DURATION) : None, 
        #    str(XMLSchema.DATETIME) : parseDateTime, 
        #    str(XMLSchema.TIME) : parseTime, 
        #    str(XMLSchema.DATE) : parseDate, 
            str(XMLSchema.GYEARMONTH) : None, 
            str(XMLSchema.GYEAR) : None, 
            str(XMLSchema.GMONTHDAY) : None, 
            str(XMLSchema.GDAY) : None, 
            str(XMLSchema.GMONTH) : None, 
            str(XMLSchema.STRING) : None, 
            str(XMLSchema.BOOLEAN) : bool, 
        #    str(XMLSchema.BASE64BINARY) : base64.decodeString, 
            str(XMLSchema.HEXBINARY) : None, 
            str(XMLSchema.FLOAT) : float, 
            str(XMLSchema.DECIMAL) : float, 
            str(XMLSchema.DOUBLE) : float, 
            str(XMLSchema.ANYURI) : None, 
            str(XMLSchema.QNAME) : None, 
            str(XMLSchema.NOTATION) : None, 
            str(XMLSchema.NORMALIZEDSTRING) : None, 
            str(XMLSchema.TOKEN) : None, 
            str(XMLSchema.LANGUAGE) : None, 
            str(XMLSchema.NMTOKEN) : None, 
            str(XMLSchema.NMTOKENS) : None, 
            str(XMLSchema.NAME) : None, 
            str(XMLSchema.NCNAME) : None, 
            str(XMLSchema.ID) : None, 
            str(XMLSchema.IDREF) : None, 
            str(XMLSchema.IDREFS) : None, 
            str(XMLSchema.ENTITY) : None, 
            str(XMLSchema.ENTITIES) : None, 
            str(XMLSchema.INTEGER) : long, 
            str(XMLSchema.LONG) : long, 
            str(XMLSchema.INT) : long, 
            str(XMLSchema.SHORT) : int, 
            str(XMLSchema.BYTE) : int, 
            str(XMLSchema.NON_POSITIVE_INTEGER) : int, 
            str(XMLSchema.NEGATIVE_INTEGER) : int, 
            str(XMLSchema.NON_NEGATIVE_INTEGER) : int, 
            str(XMLSchema.POSITIVE_INTEGER) : int, 
            str(XMLSchema.UNSIGNED_LONG) : long, 
            str(XMLSchema.UNSIGNED_INT) : long, 
            str(XMLSchema.UNSIGNED_SHORT) : int, 
            str(XMLSchema.UNSIGNED_BYTE) : int, 
            }
        
