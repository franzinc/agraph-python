#!/usr/bin/env python
# -*- coding: utf-8 -*-
# pylint: disable-msg=C0103

###############################################################################
# Copyright (c) 2006-2015 Franz Inc.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the Eclipse Public License v1.0
# which accompanies this distribution, and is available at
# http://www.eclipse.org/legal/epl-v10.html
###############################################################################

from __future__ import absolute_import

"""
A strings utility module for helper functions.
"""

import re

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

def uriref(string): 
  uri = None
  if string[0] == '<': 
    match = uriref.pattern.match(string)
    assert match, "%s is not a valid URI." % string
    uri = match.group(1).decode('unicode-escape')
  return uri

uri_pattern = r'<([^:]+:[^\s"<>]+)>'
uriref.pattern = re.compile(uri_pattern + '$')

def nodeid(string): 
  bnode = None
  if string[0] == '_': 
     bnode = nodeid.pattern.match(string).group(1)
  return bnode

nodeid.pattern = re.compile(r'_:([A-Za-z][A-Za-z0-9]*)$')

def literal(string): 
  lit = None
  if string[0] == '"': 
     label, lang, dtype = literal.pattern.match(string).groups()
     lit = (label.decode('unicode-escape'), dtype, lang)
  return lit

litvalue = r'"([^"\\]*(?:\\.[^"\\]*)*)"'
litinfo = r'(?:@([a-z]+(?:-[a-z0-9]+)*)|\^\^' + uri_pattern + r')?'
literal.pattern = re.compile(litvalue + litinfo + '$')
