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


from franz.exceptions import IllegalArgumentException, IllegalStateException
from franz import util
from franz.transport.agc import *
 
class UPI:
    """
    This class represents instances of Universal Part Identifiers.
    UPIs are used to denote resources and literals in AllegroGraph 3.0.
    They are returned as values of queries or accessors, and may be used
    as arguments to queries and accessors.  In general, UPI references 
    are more efficient than string references.        
    """
    CODE_LOWEST = -14
    NULL_CONTEXT = None
    WIDTH = UPI_WIDTH ## width in bytes
    WILD = None
    def __init__(self, long_or_byte_array=None):
        if long_or_byte_array == -5:
            print "NEGATIVE FIVE RETRIEVED FROM STORE!"
        self.upi = None
        self.setWidth = 0
        self.code = UPI.CODE_LOWEST
        self.width = UPI.WIDTH
        if isinstance(long_or_byte_array, (long, int)):
            self.code = long(long_or_byte_array)
            width = 0
        elif isinstance(long_or_byte_array, list): ## byte string?
            self.upi = [0 for i in range(self.WIDTH)]
            for i in range(len(long_or_byte_array)):
                self.upi[i] = long_or_byte_array[i]
        else:
            self.upi = [0 for i in range(self.WIDTH)]

    @staticmethod    
    def wildUPI ():
        return UPI.WILD
    
    @staticmethod
    def wildString ():
        return None
    
    @staticmethod
    def nullString(allAllowed):
        if allAllowed: return ""
        return None

    @staticmethod
    def allString():
        return None

    @staticmethod
    def refNull(x):
        if x is None: return UPI.NULL_CONTEXT
        else: return x

    def getCode(self):
        return self.code

    def withLabel(self):
        return (None != self.upi)

    def addByte(self, b):
        if self.upi is None:
            raise IllegalStateException("Cannot update marker UPI.")
        if (self.setWidth == self.WIDTH):
            raise IllegalStateException("UPI bytes are all there.")
        self.upi[self.setWidth] = b
        self.setWidth += 1

    def getByte(self, i):
        if i < self.WIDTH:
            return 0xff & self.upi[i]
        return -1

    def blankNodeID(self):
        return self.getByte(0) + 256 * self.getByte(1) + 256 * 256 * self.getByte(2) + 256 * 256 * 256 * self.getByte(3)

    def __cmp__(self, y):
        if self.upi is None and y.upi is None:
            if (self.code == y.code):
                return 0
            if self.code < y.code:
                return -1
            return 1
        if self.upi is None:
            return -1
        if y.upi is None:
            return 1
        xb = self.upi
        xlen = len(xb)
        yb = y.upi
        ylen = len(yb)
        for i in range(xlen):
            if not i < ylen:
                return 1
            if xb[i] < yb[i]:
                return -1
            if xb[i] > yb[i]:
                return 1
        return 0

    def asHex(self):
        b = []
        for i in range(self.WIDTH):
            hi = (0xf0 & self.upi[i]) >> 4
            lo = 0x0f & self.upi[i]
            b.append("0123456789ABCDEF"[hi])
            b.append("0123456789ABCDEF"[lo])
        return ''.join(b)

    def upiByteAsChar(self, i):
        v = 1 + (0xff & self.upi[i])
        return util.integer_to_character(v)

    def __eq__(self, other):
        if not isinstance(other, UPI):
            return False
        if self.upi is None and other.upi is None:
            return (self.getCode() == other.getCode())
        if self.upi is None or other.upi is None:
            return False
        ## for-while
        i = 0
        while i < len(self.upi):
            if (self.upi[i] != other.upi[i]):
                return False
            i += 1
        return True

    def __hash__(self):
        h = 0
        if self.upi is None:
            return self.getCode()
        ## for-while
        i = 0
        while i < len(self.upi):
            h = h + self.upi[i] << 8 * i % 4
            i += 1
        return h

    def __str__(self):
        if self.upi is not None:
            return "<UPI " + self.asHex() + ">"
        return "<UPI code=" + str(self.code) + ">"

    def getStoreBytes(self):
        b = []
        for i in range(4,8):
            hi = 0xf0 & self.upi[i] >> 4
            lo = 0x0f & self.upi[i]
            b.append("0123456789ABCDEF"[hi])
            b.append("0123456789ABCDEF"[lo])
        return ''.join(b)

    def canReference(self):
        if self.upi is not None:
            return True
        elif self.isNullContext():
            return True
        return False

    @staticmethod
    def can_reference(x):
        if x is None:
            return False
        if x.upi is not None:
            return True
        if UPI.isNullContext(x):
            return True
        return False

    def isWild(self):
        if (self == self.WILD):
            return True
        if self.upi is not None:
            return False
        return (self.WILD.code == self.code)

    @staticmethod
    def is_wild(x):
        return True if x is None else x.isWild()

    @staticmethod
    def getNullContextUPI ():
        return UPI.NULL_CONTEXT
    

    def isNullContext(self):
        return UPI.NULL_CONTEXT.code == self.code
#        if self.upi is not None:
#            return False
#        return (self.NULL_CONTEXT.code == self.code)

    @staticmethod
    def is_null_context(x):
        if x is None:
            return False
        if (x == UPI.NULL_CONTEXT):
            return True
#        if x.upi is not None:
#            return False
        return x.isNullContext()

    def asChars(self, prefix=None):
        if not prefix:
            b = []
            for i in range(self.WIDTH):
                b.append(self.upiByteAsChar(i))            
        else:
            b = []
            for i in range(len(prefix)):
                b.append(prefix[i])
            for i in range(self.WIDTH):
                b.append(self.upiByteAsChar(i))
        return ''.join(b)

    def hasBody(self):
        return (None != self.upi)

    @staticmethod
    def has_body(x):
        return (None != x.upi)

UPI.WILD = UPI(AGU_WILD)
UPI.NULL_CONTEXT = UPI(AGU_NULL_CONTEXT)

