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
from past.builtins import unicode

from ..exceptions import IllegalArgumentException

## Finds the index of the first local name character in an (non-relative)
## URI. This index is determined by the following the following steps:
## <ul>
## <li>Find the <em>first</em> occurrence of the '#' character,
## <li>If this fails, find the <em>last</em> occurrence of the '/'
## character,
## <li>If this fails, find the <em>last</em> occurrence of the ':'
## character.
## <li>Add <tt>1<tt> to the found index and return this value.
## </ul>
## Note that the third step should never fail as every legal (non-relative)
## URI contains at least one ':' character to seperate the scheme from the
## rest of the URI. If this fails anyway, the method will throw an
## {@link IllegalArgumentException}.
## 
## @param uri
##        A URI string.
## @return The index of the first local name character in the URI string. Note that
## this index does not reference an actual character if the algorithm determines
## that there is not local name. In that case, the return index is equal to the
## length of the URI string.
## @throws IllegalArgumentException
##         If the supplied URI string doesn't contain any of the separator
##         characters. Every legal (non-relative) URI contains at least one
##         ':' character to separate the scheme from the rest of the URI.
def getLocalNameIndex(uri):
    idx = uri.rfind('#')
    if (idx < 0):
        idx = uri.rfind('/')
    if (idx < 0):
        idx = uri.rfind(':')
    if (idx < 0):
        raise IllegalArgumentException("No separator character found in URI: " + uri)
    return idx + 1

def asURIString(value):
    value = unicode(value)
    if value.startswith('<'): return value
    else: return "<%s>" % value
