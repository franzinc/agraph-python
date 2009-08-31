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


ALL_CONTEXTS = 'ALL_CONTEXTS'
MINI_NULL_CONTEXT = 'null'

class Dataset:
    """
    Records a set of default and named graphs that can restrict 
    the scope of a query.    
    """
    def __init__(self, contexts=None):
        self.defaultGraphs = set([])
        self.namedGraphs = set([])
        if contexts:
            self.namedGraphs = set([cxt for cxt in contexts])

    def getDefaultGraphs(self):
        if not self.defaultGraphs: return ALL_CONTEXTS 
        else: return [g for g in self.defaultGraphs]
    
    def addDefaultGraph(self, uri): self.defaultGraphs.add(uri)
    
    def removeDefaultGraph(self, uri): self.defaultGraphs.remove(uri) 
    
    def getNamedGraphs(self): 
        if not self.namedGraphs: return ALL_CONTEXTS
        else: return [g for g in self.namedGraphs]
    
    def addNamedGraph(self, uri): self.namedGraphs.add(uri)
    
    def removeNamedGraph(self, uri): self.namedGraphs.remove(uri) 
 
    def clear(self):
        self.defaultGraphs = set([])
        self.namedGraphs = set([])

    def asQuery(self, excludeNullContext):
        if not self.defaultGraphs and not self.namedGraphs:
            if excludeNullContext: return ''
            else: return "## empty dataset ##"
        sb = []
        for uri in self.defaultGraphs:
            if uri is None and excludeNullContext: continue
            sb.append("FROM ")
            self._append_uri(sb, uri)
            sb.append(" ")
        for uri in self.namedGraphs:
            if uri is None and excludeNullContext: continue  ## null context should not appear here
            sb.append("FROM NAMED ")
            self._append_uri(sb, uri)
            sb.append(" ")            
        return ''.join(sb)
    
    def __str__(self):
        self.asQuery(False)

    def _append_uri(self, sb, uri):
        try:
            uriString = uri.uri
        except AttributeError:
            uriString = str(uri)

        if len(uriString) > 50:
            sb.append("<" + uriString[:19] + "..")
            sb.append(uriString[len(uriString) - 29:] + ">/n")
        else:
            sb.append("<")
            sb.append(uriString)
            sb.append(">") 
