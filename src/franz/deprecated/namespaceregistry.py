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

from franz.allegrograph.exceptions import NiceException

class NamespaceRegistry(object):
    """ generated source for NamespaceRegistry

    """
    RDFandOwl = None
    preDefs = ["rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
        "rdfs", "http://www.w3.org/2000/01/rdf-schema#",
        "owl", "http://www.w3.org/2002/07/owl#",]
    
    def __init__(self, ns_or_defs):
        self.regs = []
        if ns_or_defs:
            self.register(ns_or_defs)
        
    def register_prefix(self, prefix, uri):
        found = False
        ## for-while
        i = 0
        while i < len(self.regs):
            e = self.regs[i]
            if e == prefix:
                if uri is None:
                    self.regs.remove(i)
                    self.regs.remove(i)
                else:
                    self.regs.set(i + 1, uri)
                found = True
                break
            i = i + 2
        if not found:
            self.regs.append(prefix)
            self.regs.append(uri)

    def register(self, prefix_or_more_defs, uri=None):
        if uri:
            return self.register_prefix(prefix_or_more_defs, uri)
        moreDefs = prefix_or_more_defs
        if isinstance(moreDefs, NamespaceRegistry):
            defs = moreDefs.regs
            ## for-while
            i = 0
            while i < len(defs):
                self.register(defs[i], defs[i + 1])
                i = i + 2
        else:
            ## for-while
            i = 0
            while i < len(moreDefs):
                self.register(moreDefs[i], moreDefs[i + 1])
                i = i + 2

    def getURI(self, prefix):
        ## for-while
        i = 0
        while i < len(self.regs):
            e = self.regs[i]
            if e == prefix:
                return self.regs[i + 1]
            i = i + 2
        raise NiceException("The prefix '%s' has not been registered" % prefix)

    def getPrefixes(self):
        prefixes = ['' for i in range(len(self.regs) / 2)]
        ## for-while
        i = 0
        while i < len(prefixes):
            prefixes[i] = self.regs[2 * i]
            i += 1
        return prefixes

    def clear(self):
        self.regs = []

    def stringArray(self):
        out = ['' for i in range(len(self.regs))]
        ## for-while
        i = 0
        while i < len(out):
            out[i] = self.regs[i]
            i += 1
        return out

NamespaceRegistry.RDFandOwl = NamespaceRegistry(NamespaceRegistry.preDefs)


