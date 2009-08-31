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
    
