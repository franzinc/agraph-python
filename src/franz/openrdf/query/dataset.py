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

class Dataset:
    """
    Records a set of default and named graphs that can restrict 
    the scope of a query.    
    """
    def __init__(self):
        self.defaultGraphs = set([])
        self.namedGraphs = set([])

    def getDefaultGraphs(self): return [g for g in self.defaultGraphs]
    
    def addDefaultGraph(self, uri): self.defaultGraphs.add(uri)
    
    def removeDefaultGraph(self, uri): self.defaultGraphs.remove(uri) 
 
    def getNamedGraphs(self): return [g for g in self.namedGraphs]
    
    def addNamedGraph(self, uri): self.namedGraphs.add(uri)
    
    def removeNamedGraph(self, uri): self.namedGraphs.remove(uri) 
 
    def clear(self):
        self.defaultGraphs = set([])
        self.namedGraphs = set([])

    def __str__(self):
        if not self.defaultGraphs and not self.namedGraphs:
            return "## empty dataset ##"
        sb = []
        for uri in self.defaultGraphs:
            sb.append("FROM")
            self._append_uri(sb, uri)
        for uri in self.namedGraphs:
            sb.append("FROM NAMED ")
            self._append_uri(sb, uri)
        return ''.join(sb)

    def _append_uri(self, sb, uri):
        uriString = str(uri)
        if len(uriString) > 50:
            sb.append("<" + uriString[:19] + "..")
            sb.append(uriString[len(uriString) - 29:] + ">/n")
        else:
            sb.append("<")
            sb.append(uriString)
            sb.append(">") 
