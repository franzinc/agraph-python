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
from __future__ import unicode_literals
from future.builtins import chr
from future.types import newbytes
from future.utils import native_str, isnewbytes
from past.builtins import unicode
import ast
import sys

"""
A strings utility module for helper functions.
"""

import re

###############################################################################
## Canonical NTriples encoding
###############################################################################


def encode_ntriple_string(string):
    """
    Return a unicode string escaped according to N-Triples
    canonical encoding rules.
    """
    if not isinstance(string, unicode):
        string = unicode(string, 'utf-8')

    for char, replacement in ESCAPES:
        string = string.replace(char, replacement)
    return string


ESCAPES = [
    # Replacements will be performed sequentially, so backslash 
    # must be the first character on the list
    (chr(0x5C), r'\\'),  
    (chr(0x0A), r'\n'),
    (chr(0x0D), r'\r'),
    (chr(0x22), r'\"'),
]

uri_escaped_chars = re.compile(r'[\x00-\x20<>"{}|^`\\]')


def uri_escape_match(match):
    """
    Converts a Match object representing a single character
    into an ntriple escape sequence.
    """
    code = ord(match.group())
    if code <= 0xffff:
        return '\\u%04x' % code
    else:
        return '\\U%08x' % code


def encode_ntriple_uri(uri):
    """
    Converts a string URI to ntriples by adding angle brackets
    and escaping special characters.  
    """
    return '<' + uri_escaped_chars.sub(uri_escape_match, uri) + '>'


def ntriples_unescape(text):
    """
    Decodes ntriples escape sequences in a string.

    Actually decodes a superset of said sequences.
    """
    if text is None:
        return None
    return ast.literal_eval(u'u"' + text + u'"')


def uriref(string):
    """
    If `string` is a valid NTriples URI reference, extract and return the URI (as a string).
    Otherwise return `None`.
    """
    match = uriref.pattern.match(string)
    if not match:
        return None
    return ntriples_unescape(match.group(1))

uri_pattern = r'<(.*)>'
uriref.pattern = re.compile(uri_pattern + '$')


def nodeid(string):
    """
    If `string` is a valid NTriples BNode reference, extract and return the node id.
    Otherwise return `None`.
    """
    match = nodeid.pattern.match(string)
    if not match:
        return None
    return match.group(1)

nodeid.pattern = re.compile(r'_:([A-Za-z][A-Za-z0-9]*)$')


def literal(string):
    """
    If `string` is a valid literal in NTriples syntax, return its value, lang tag and type.
    Use `None` if there is no language tag or no datatype.
    If `string` is not a valid literal return `None`.
    """
    match = literal.pattern.match(string)
    if not match:
        return None
    label, lang, dtype = match.groups()
    return ntriples_unescape(label), ntriples_unescape(dtype), lang

litvalue = r'"([^"\\]*(?:\\.[^"\\]*)*)"'
litinfo = r'(?:@([a-z]+(?:-[a-z0-9]+)*)|\^\^' + uri_pattern + r')?'
literal.pattern = re.compile(litvalue + litinfo + '$')


def to_bytes(text):
    """
    If TEXT is a Unicode string, return a byte string in utf-8.
    Otherwise simply return TEXT.

    :param text: Text to be converted.
    :type text: str|bytes|unicode
    :rtype: bytes
    """
    if isinstance(text, unicode):
        return text.encode('utf-8')
    return text


if sys.version_info[0] > 2:
    def to_native_string(text):
        """
        Converts text to the native string type of the Python version used.
        UTF-8 encoding is used if the text needs to be encoded or decoded.

        :param text: Text to be converted (either Unicode or bytes).
        :type text: str|bytes|unicode
        :rtype: str
        """
        if isinstance(text, bytes):
            return str(text, 'utf-8')
        return text
else:
    def to_native_string(text):
        if isnewbytes(text):
            return bytes.__str__(text)
        if isinstance(text, native_str):
            return text
        # Must be Unicode...
        return text.encode('utf-8')
