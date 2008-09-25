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
from franz.openrdf.model.valuefactory import ValueFactory

class Statement:
    """
    Lightweight implementation of 'Statement'
    """
    def __init__(self, subject, predicate, object, context=None):
        self.subject = subject
        self.predicate = predicate
        self.object = object
        self.context = context
        self.string_tuple = None

    def __eq__(self, other):
        if not isinstance(other, Statement): return False
            ## The object is potentially the cheapest to check, as types
            ## of these references might be different.
            ## In general the number of different predicates in sets of
            ## statements is the smallest, so predicate equality is checked
            ## last.
        spoEqual = self.object.__eq__(other.getObject()) and self.subject.__eq__(other.getSubject()) \
                and self.predicate.__eq__(other.getPredicate())
        if self.context:
            return spoEqual and self.context.__eq__(other.getContext())
        else:
            return spoEqual

    def __hash__(self):
        return 961 * self.subject.__hash__() + 31 * self.predicate.__hash__() + self.object.__hash__();

    def __str__(self):
        sb= []
        sb.append("(")
        sb.append(self.string_tuple[0])
        sb.append(", ")
        sb.append(self.string_tuple[1])
        sb.append(", ")
        sb.append(self.string_tuple[2])
        cxt = self.self.string_tuple[3]
        if cxt:
            sb.append(", ")        
            sb.append(self.string_tuple[3])        
        sb.append(")")
        return ''.join(sb)

    def __len__(self):
        return len(self.string_tuple)
    
    def __getitem__(self, index):
        if index == 0: return self.getSubject()
        elif index == 1: return self.getPredicate()
        elif index == 2: return self.getObject()
        elif index == 3: return self.getContext()
        else:
            ## I don't know what the official Python exception is here :(  - RMM
            raise IllegalArgumentException("Illegal index %s passed to StatementImpl.\n" +
                    "  Legal arguments are integers 0-3")
                                              
    def setQuad(self, string_tuple):
        self.string_tuple = string_tuple

    def getSubject(self):         
        if not self.subject:
            self.subject = ValueFactory.stringTermToTerm(self.string_tuple[0])
        return self.subject
    
    def setSubject(self, subject): self.subject = subject
    
    def getPredicate(self):
        if not self.predicate:
            self.predicate = ValueFactory.stringTermToTerm(self.string_tuple[1])
        return self.predicate
     
    def setPredicate(self, predicate):self.predicate = predicate
    
    def getObject(self):
        if not self.object:
            self.object = ValueFactory.stringTermToTerm(self.string_tuple[2])
        return self.object
    
    def setObject(self, object): self.object = object
    
    def getContext(self): 
        if not self.context:
            if len(self.string_tuple == 3): return None
            self.subject = ValueFactory.stringTermToTerm(self.string_tuple[4])
        return self.context
    
    def setContext(self, context): self.context = context
