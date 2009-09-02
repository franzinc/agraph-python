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

"""
Tests for the package.
"""

from strings import escape_double_quotes

def test_escape_double_quotes():
    assert escape_double_quotes(r'abc') == r'abc'
    assert escape_double_quotes(r'ab"cd\"ef') == r'ab\"cd\"ef'
    assert escape_double_quotes(r'"abc"') == r'\"abc\"'
    assert escape_double_quotes(r'""abc"') == r'\"\"abc\"'
    assert escape_double_quotes(r'""\"""\"\""abc"') == r'\"\"\"\"\"\"\"\"abc\"'
    assert escape_double_quotes(r'\"abc\"') == r'\"abc\"'
    assert escape_double_quotes(r'"""') == r'\"\"\"'
