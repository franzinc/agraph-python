#!/usr/bin/env python
# -*- coding: utf-8 -*-
# pylint: disable-msg=C0103

###############################################################################
# Copyright (c) 2006-2012 Franz Inc.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the Eclipse Public License v1.0
# which accompanies this distribution, and is available at
# http://www.eclipse.org/legal/epl-v10.html
###############################################################################

from __future__ import absolute_import

from ..util.uris import getLocalNameIndex

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
    
    
    
    
    
    
    
