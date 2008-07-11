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


def is_null_string(s):
    return s is None or s == ''

########################################################################################
## Wrapper classes for ones that Python doesn't have
########################################################################################

class Byte(object):
    def __init__(self, byte):
        self.byte = byte ## an int, or perhaps a packed character?
    
    def get_value(self): return self.byte

class Character(object):
    def __init__(self, char):
        self.char = char ## a one-character string
        
    def get_value(self): return self.char

class Short(object):
    def __init__(self, short):
        self.short = short ## an int

    def get_value(self): return self.short
    
class Float(object):
    def __init__(self, float):
        self.float = float ## a C double (a Python float)

    def get_value(self): return self.float

########################################################################################
##
########################################################################################


import struct

def integer_to_hex(i):
    """Convert integer to hex string."""
    #print "HEX ", i
    hexValue = "%X" % i
    return hexValue

def help_integer_to_binary(n):
    ## BUG: INFINITE RECURSION IF n < 0:
    if n == 0: return ['0']
    else: 
        bits = help_integer_to_binary(n>>1)
        bits.append(str(n&1))
        return bits    
    
def integer_to_binary(n):
    """
    Convert integer to binary string.
    For now, only handles non-negative integers
    """
    if n < 0: ## hack seems OK because this is called rarely, and only for debugging:
        return "NEGATIVE: " + integer_to_binary(-n)
    return (''.join(help_integer_to_binary(n))).lstrip('0')

def float2raw_int(f):
    """
    Pack a double-precision float into a 4-byte string representing an integer
    representation.  Loss of precision results.
    """
    p = struct.pack('f', f)
    quad = (ord(p[0]), ord(p[1]), ord(p[2]), ord(p[3]))
    i = (quad[0]) | (quad[1]<<8) | (quad[2]<<16) | (quad[3]<<24)
    return i

def float2raw_long(f):
    """
    Pack a double-precision float into an 8-byte long integer (changing the type, but not the bits). 
    Strategy, pack the float into an 8-byte string, and then unpack into a long.
    """
    p = struct.pack('d', f)
    longInt = struct.unpack('Q', p)[0]
    ##print "UNPACKED TYPE ", type(longInt), "  ", str(longInt)
    return longInt

def long_bits_to_float(longInt):
    """
    Pack an 8-byte long integer into a double-precision float (changing the type, but not the bits). 
    Strategy, pack the long into an 8-byte string, and then unpack into a double.
    """
    p = struct.pack('Q', longInt)
    f = struct.unpack('d', p)[0]
    return f
    
def long_bits_to_character(longInt):
    """
    Return a character (really a length-one string) containing the first
    8 bits of 'longInt'.
    """
    p = struct.pack('l', longInt)
    return p[0]
    
def integer_to_character(intValue):
    if isinstance(intValue, int):
        p = struct.pack('B', intValue)
    else:
        p = struct.pack('Q', intValue)
    return p[0]

def integer_to_string(intValue):
    if isinstance(intValue, int):
        p = struct.pack('l', intValue)
    else:
        p = struct.pack('Q', intValue)
    return p

def character_to_integer(chr):
    #print "CHR ", chr
    p = struct.pack('c', chr[0])
    i = struct.unpack('B', p)[0]
    #print "CHR_TO_INT ", i
    return i

## THIS IS THE SAME RESULT AS 'character_to_integer?
def packed_character_to_integer(pc):
    i = struct.unpack('B', pc)[0]
    return i

    
if __name__ == '__main__':
    print "TO HEX %s  %s" % (3, integer_to_hex(3))
    print "TO HEX %s  %s" % (42, integer_to_hex(42))
    print "TO HEX %s  %s" % (512, integer_to_hex(512))
    print "TO BINARY %s  %s" % (0, integer_to_binary(0))
    print "TO BINARY %s  %s" % (5, integer_to_binary(5))
    print "TO BINARY %s  %s" % (42, integer_to_binary(42))  
    print "TO BINARY %s  %s" % (-3, integer_to_binary(-3))      
    
    x = 3.3
    print "SIZE " , struct.calcsize('d')
    print "FLT: " , float2raw_int(x)
    print "DBL: ", float2raw_long(x)
    print "ORIGINAL ", long_bits_to_float(float2raw_long(x))
    
    print "FORTY_TWO_CHAR '%s'" % integer_to_character(42)
    print "BLANK_TO_INTEGER '%s'" % character_to_integer(' ');
    
      