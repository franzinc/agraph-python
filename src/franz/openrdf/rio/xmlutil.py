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

from franz.openrdf.util.uris import getLocalNameIndex

def escapeCharacterData(text):
    """
    Escapes any special characters in the supplied text so that it can be included as character data in an XML document.
    The characters that are escaped are &, <, > and carriage return (\r).
    Hmm. We don't know the replacement for '\r' (unless its '<br.').
    """
    return text.replace('&', '&amp;').replace('<', '&lt;').replace('>', '&gt;')
    
def escapeDoubleQuotedAttValue(value):
    """
    Escapes any special characters in the supplied value so that it can be used as an double-quoted attribute value in an XML document.
    The characters that are escaped are &, <, >, tab (\t), carriage return (\r), line feed (\n) and ".
    Hmm. We don't know the replacement for '\r', '\t', and '\n'.
    """
    return value.replace('&', '&amp;').replace('<', '&lt;').replace('>', '&gt;').replace('"', '&quot;')


def findURISplitIndex(uri):
    """
    Tries to find a point in the supplied URI where this URI can be safely split into a namespace part and a local name.
    The local name must be an NCName.
    """
    startPos = getLocalNameIndex(uri)
    for i in range(startPos, len(uri)):
        if isNCName(uri[i:]):
            return i
    return startPos    
    
def isNCName(name):
    """
    Checks whether the supplied String is an NCName (Namespace Classified Name) as specified at http://www.w3.org/TR/REC-xml-names/#NT-NCName.
    
    Note: The spec contains inscrutable hex chars that we have not tried to decypher, so we use something
    simpler here.
    """
    firstChar = name[:1]
    if not (firstChar == '_' or firstChar.isalpha()): return False
    ## look for quick win:
    if name[1:].isalnum(): return True
    for i in range(1, len(name)):
        if not isNCChar(name[i:i]):
            return False
    return True 

def isNCChar(c):
    """
    Note: The spec contains inscrutable hex chars that we have not tried to decypher, so we use something
    simpler (but wrong) here.
    Need to find some code somewhere to borrow.
    """
    if c.isalnum(): return True
    
    
    
    
    
    
    