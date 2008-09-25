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
    separatorIdx = uri.find('#')
    if (separatorIdx < 0):
        separatorIdx = uri.rfind('/')
    if (separatorIdx < 0):
        separatorIdx = uri.rfind(':')
    if (separatorIdx < 0):
        raise IllegalArgumentException("No separator character founds in URI: " + uri)
    return separatorIdx + 1

## Checks whether the URI consisting of the specified namespace and local
## name has been split correctly according to the URI splitting rules
## specified in {@link URI}.
## 
## @param namespace
##        The URI's namespace, must not be <tt>null</tt>.
## @param localName
##        The URI's local name, must not be <tt>null</tt>.
## @return <tt>true</tt> if the specified URI has been correctly split into
##         a namespace and local name, <tt>false</tt> otherwise.
## @see URI
## @see #getLocalNameIndex(String)
def isCorrectURISplit(namespace, localName):
    """
    THIS HAS NOT BEEN USED/DEBUGGED.  IS HERE IN CASE WE NEED IT LATER - RMM
    """
    if (len(namespace) == 0):
        return False        
    nsLength = len(namespace)
    lastNsChar = namespace[:-1]
    if (lastNsChar == '#' and namespace.rfind('#', nsLength - 2) == -1):
        ## namespace ends with a '#' and does not contain any futher '#'
        ## characters
        return True
    elif (localName.indexOf('#') == -1 and localName.find('/') == -1):
        if (lastNsChar == '/'):
            ## URI does not contain any '#' characters and the namespace ends
            ## with the last '/' character
            return True
        elif (lastNsChar == ':' and localName.find(':') == -1):
            ## URI does not contain any '#' or '/' characters and the namespace
            ## ends with the last ':' character
            return True
    return False
