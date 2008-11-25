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

def escape_double_quotes(string):
    """
    Place a backslash in front of any double quote in 'string' not already
    preceded by a backslash.
    """
    if string.find('"') < 0: return string
    index = 0
    pieces = []
    for i, c in enumerate(string):
        if c == '"' and (i == 0 or not string[i] == '\\'):
            pieces.append(string[index:i])
            pieces.append('\\"')
            index = i + 1
    ## add last piece:
    pieces.append(string[index:i])
    return ''.join(pieces)



##===========================================================================
## Test code
##===========================================================================

def _test(s):
    print s, "   ", escape_double_quotes(s)
    
if __name__ == '__main__':
    _test("abc")
    _test('ab"cd\"ef')
    _test('"abc"')
    _test('\"abc\"')
    