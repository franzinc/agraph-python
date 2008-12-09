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
from franz.openrdf.model.value import Value, URI, BNode
from franz.openrdf.model.literal import Literal

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
        spoEqual = self.getObject().__eq__(other.getObject()) and self.getSubject().__eq__(other.getSubject()) \
                and self.getPredicate().__eq__(other.getPredicate())
        if self.context:
            return spoEqual and self.getContext().__eq__(other.getContext())
        else:
            return spoEqual

    def __hash__(self):
        return 961 * self.getSubject().__hash__() + 31 * self.getPredicate().__hash__() + self.getObject().__hash__();

    def __str__(self):
        sb= []
        sb.append("(")
        sb.append(self.string_tuple[0])
        sb.append(", ")
        sb.append(self.string_tuple[1])
        sb.append(", ")
        sb.append(self.string_tuple[2])
        if len(self.string_tuple) > 3:
            cxt = self.string_tuple[3]
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
            self.subject = Statement.stringTermToTerm(self.string_tuple[0])
        return self.subject
    
    def setSubject(self, subject): self.subject = subject
    
    def getPredicate(self):
        if not self.predicate:
            self.predicate = Statement.stringTermToTerm(self.string_tuple[1])
        return self.predicate
     
    def setPredicate(self, predicate):self.predicate = predicate
    
    def getObject(self):
        if not self.object:
            self.object = Statement.stringTermToTerm(self.string_tuple[2])
        return self.object
    
    def setObject(self, object): self.object = object
    
    def getContext(self): 
        if not self.context:
            if len(self.string_tuple) == 3: return None
            self.context = Statement.stringTermToTerm(self.string_tuple[3])
        return self.context
    
    def setContext(self, context): self.context = context
    
    @staticmethod
    def stringTermToTerm(string_term):
        """
        Given a string representing a term in ntriples format, return
        a URI, Literal, or BNode.
        """
        if not string_term: return string_term
        if string_term[0] == '<':
            uri = string_term[1:-1]
            return URI(uri)
        elif string_term[0] == '"':
            lastPos = string_term.rfind('"')
            if lastPos == len(string_term) - 1:
                return Literal(string_term[1:-1])
            ## we have a double-quoted literal with either a data type or a language indicator
            caratPos = string_term.find('^^')
            if caratPos >= 0:
                label = string_term[1:caratPos - 1]
                datatype = string_term[caratPos + 2:]
                return Literal(label, datatype=datatype)
            atPos = string_term.find('@')
            if atPos >=0:
                label = string_term[1:atPos - 1]
                language = string_term[atPos + 1:]
                return Literal(label, language=language)
            else:
                return Literal(string_term[1:-1])
        elif string_term[0] == '_' and string_term[1] == ':':
            return BNode(string_term[2:])
        else:
            raise BadFormatException("Cannot translate '%s' into an OpenRDF term." % string_term)
        
    @staticmethod
    def ntriples_string_to_value(string_term):
        """
        Given a string representing a term in ntriples format, return
        a URI or the label portion of a literal (a string minus the double quotes).
        TODO: IMPLEMENT BNODES
        """
        if not string_term: return string_term
        if string_term[0] == '<':
            uri = string_term[1:-1]
            return uri
        elif string_term[0] == '"':
            ## look for the trailing double quote:
            lastPos = string_term.rfind('"')
            print "STRING TERM", string_term, lastPos
            if lastPos == len(string_term) - 1:
                print "   RETURNING", string_term[1:-1]
                return string_term[1:-1]
            ## we have a double-quoted literal with either a data type or a language indicator
            caratPos = string_term.find('^^')
            if caratPos >= 0:
                label = string_term[1:caratPos - 1]
                return label
            atPos = string_term.find('@')
            if atPos >=0:
                label = string_term[1:atPos - 1]
                return label
            else:
                return string_term[1:-1]
        else:
            raise UnimplementedMethodException("BNodes not yet implemented by 'stringTermToTerm'")
                

