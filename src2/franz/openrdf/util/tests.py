#!/usr/bin/env python
# -*- coding: utf-8 -*-
# pylint: disable-msg=C0103

###############################################################################
# Copyright (c) 2006-2009 Franz Inc.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the Eclipse Public License v1.0
# which accompanies this distribution, and is available at
# http://www.eclipse.org/legal/epl-v10.html
###############################################################################

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
