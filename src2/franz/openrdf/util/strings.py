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

"""
A strings utility module for helper functions.
"""

import re

PATTERN = re.compile('.?"')

def escape_double_quotes(string):
    """
    Place a backslash in front of any double quote in 'string' not already
    preceded by a backslash.
    """
    def handle_quote(matchobj):
        """Replace matches with the appropriate escaped character sequence."""
        match = matchobj.group(0)

        if match == '"':
            return '\\"'

        if match == '\\"':
            return match

        if match == '""':
            return '\\"\\"'

        return match[0] + '\\"'

    return re.sub(PATTERN, handle_quote, string)


###############################################################################
## NTriples 7-bit ASCII Encoding
###############################################################################

def hex2int(hex):
    return int(hex, 16)

def int2hex(n):
    return "%X" % n

def ord2HHHH(n):
    digits = int2hex(n)
    length = min(len(digits), 8)
    return ord2HHHH.prefixes[length] + digits

ord2HHHH.prefixes = ['\U', '\u000', '\u00', '\u0', '\u', '\U000', '\U00', '\U0', '\U']

def encode_ntriple_string(string):
    """
    Return a unicode string encoded in 7-bit ASCII containing the
    NTRIPLES escape sequences for non-ascii and other characters.
    """

    # Access these at local variable speeds since they are in a loop
    HEX_MAP = encode_ntriple_string.HEX_MAP
    LOWER_ASCII = encode_ntriple_string.LOWER_ASCII
    QUOTE = encode_ntriple_string.QUOTE
    UPPER_ASCII = encode_ntriple_string.UPPER_ASCII
    
    bytes = []
    if not isinstance(string, unicode):
        string = unicode(string)

    for c in string:
        ordl = ord(c)
        if ordl >= LOWER_ASCII and ordl <= UPPER_ASCII and not ordl == QUOTE:
            bytes.append(c)
        else:
            bytes.append(HEX_MAP.get(ordl) or ord2HHHH(ordl))
    return ''.join(bytes)

encode_ntriple_string.HEX_MAP = {
    hex2int('9'): r'\t',
    hex2int('A'): r'\n',
    hex2int('D'): r'\r',
    hex2int('20'): chr(hex2int('20')), #blank
    hex2int('21'): chr(hex2int('21')),
    hex2int('22'): r'\"',    
    hex2int('5C'): r'\\',    
    }

encode_ntriple_string.LOWER_ASCII = hex2int('23')
encode_ntriple_string.QUOTE = hex2int('5C')
encode_ntriple_string.UPPER_ASCII = hex2int('7E')

##===========================================================================
## Test code
##===========================================================================

def test_escape_double_quotes():
    assert escape_double_quotes(r'abc') == r'abc'
    assert escape_double_quotes(r'ab"cd\"ef') == r'ab\"cd\"ef'
    assert escape_double_quotes(r'"abc"') == r'\"abc\"'
    assert escape_double_quotes(r'""abc"') == r'\"\"abc\"'
    assert escape_double_quotes(r'""\"""\"\""abc"') == r'\"\"\"\"\"\"\"\"abc\"'
    assert escape_double_quotes(r'\"abc\"') == r'\"abc\"'
    assert escape_double_quotes(r'"""') == r'\"\"\"'
    
