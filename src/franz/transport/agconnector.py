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
from franz.allegrograph.upi import UPI

class AGConnector(object):
    """ 
    Generated source for AGConnector
    """
    def __init__(self):
        self.connected = False
        self.port = 4567  # 3456
        self.jport = -4567 # -3457
        self.host = ""
        self.pollCount = 3
        self.pollInterval = 1000
        self.timeout = 5000
        self.debug = 0
        self.currentServerLevel = -1

    def initialize(self, port, port2, host, pollCount, pollInterval, debug, timeout):
        self.port = self.port
        if port2 < 0:
            self.jport = port2
        elif port2 > 0:
            self.jport = -port2
        self.host = host
        self.pollCount = pollCount
        self.pollInterval = pollInterval
        self.debug = debug
        self.timeout = timeout

def copy(a, b):
    """
    Copy integer values of 'a' into 'b', destructively modifying 'b'.
    UNLESS 'a' contains strings, in which case the strings are copied into
    'b'.  The term 'copy' is poorly-chosen, since this is not a copy.
    """
    if not a: return
    if isinstance(a[0], long):
        for i in range(0, len(a)):
            b[i] = int(a[i])  ## cast long to int
    else:
        for i in range(0, len(a)):            
            b[i] = a[i]  

def testIndex(ag):
    if ag.tsx < 0:
        raise IllegalStateException("Attempting to access closed AllegroGraph database.")

def toInt(v):
    if isinstance(v, long):  return int(v)
    elif isinstance(v, int):  return v
    elif isinstance(v, str) and len(v) == 1: return ord(v[0]) ## bytes??
    else: return 0;
    
def longValue(x):
    if isinstance(x, long): return x
    elif isinstance(x, int): return long(x) ## cast int to long
    elif x is None: return 0
    elif isinstance(x, UPI):  return x.getCode()
    raise IllegalArgumentException("Cannot get a long value from " + x)

def hasLongValue(x):
    if x is None: return True
    if isinstance(x, long):  return True
    if isinstance(x, UPI) and not x.withLabel(): return True
    return False

def doubleValue(x):
    if x is None: return 0
    if isinstance(x, float): return x
    raise IllegalArgumentException("Cannot get a double value from " + x)

def isInteger(x):
    if x is None: return True
    elif isinstance(x, long): return True  ## that's what the Java code says!
    elif isinstance(x, int): return True
    return False

def canBeUPI(x):
    if x is None: return False
    elif isInteger(x): return True
    elif isinstance(x, UPI): return True
    return False

def toUPI(x):
    if x is None:  return None
    elif isInteger(x): return UPI(longValue(x))
    elif isinstance(x, UPI): return x
    raise IllegalArgumentException("Cannot get a UPI value from " + x)

def toUPIArray(x):
    if x is None:  return []
    elif canBeUPI(x):  return [toUPI(x)]
    elif isinstance(x, list):  
        return [toUPI(y) for y in x]
    raise IllegalArgumentException("Cannot get a UPI[] value from " + x)

def longArray(x):
    if x is None: return []
    elif isinstance(x, list): return copyToLongArray(x)
    raise IllegalArgumentException("Cannot get a long[] value from " + x)

def copyToLongArray(x):
    if x is None:  return [long(0)]
    elif not x: return []
    elif isinstance(x[0], long): return x
    return [longValue(item) for item in x]



if __name__ == '__main__':
    pass
