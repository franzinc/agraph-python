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

class Literal(Value):
    """
    Lightweight implementation of the Literal class.  Think 'LiteralBase'.
    
    Implementer note: If this is still too heavyweight, subclass but don't
    call super.__init__.  That's why Python is so cool!
    """
    def __init__(self, value, datatype=None, language=None):
        self.value = value
        self.datatype = datatype
        self.language = language
        self.label = None

    def getLabel(self): 
        if not self.label:
            self.label = str(self.value)
        return self.label
    
    def getLanguage(self): return self.language
    def getDatatype(self):
        """
        Return a URI representing the datatype for this literal, if there is one
        """ 
        return self.datatype

    def __eq__(self, other):
        if not isinstance(other, Literal): return False
        if not self.value == other.value: return False
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
