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
from builtins import object
from future.utils import python_2_unicode_compatible

from .value import Value, URI, BNode
from .literal import Literal
from ..util import strings


@python_2_unicode_compatible
class Statement(object):
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
        if not isinstance(other, Statement):
            return NotImplemented

        ## The object is potentially the cheapest to check, as types
        ## of these references might be different.
        ## In general the number of different predicates in sets of
        ## statements is the smallest, so predicate equality is checked
        ## last.
        spoEqual = self.getObject() == other.getObject() and self.getSubject() == other.getSubject() \
                and self.getPredicate() == other.getPredicate()
        if self.context:
            return spoEqual and self.getContext() == other.getContext()
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
            elif len(self.string_tuple) > 4:
                sb.append(", None")
        if len(self.string_tuple) > 4:
            sb.append(", ")        
            sb.append(self.string_tuple[4])       
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
            raise IndexError("Illegal index %s passed to StatementImpl.\n" +
                    "  Legal arguments are integers 0-3")
                                              
    def setQuad(self, string_tuple):
        self.string_tuple = string_tuple

    def getSubject(self):         
        if not self.subject:
            self.subject = Statement.stringTermToTerm(self.string_tuple[0])
        return self.subject
    
    def setSubject(self, subject):
        self.subject = subject
    
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

    def getTripleID(self):
        if len(self.string_tuple) == 5:
            id = int(self.string_tuple[4])
        else:
            id = -1
            
        return id
    
    @staticmethod
    def stringTermToTerm(string_term):
        """
        Given a string representing a term in ntriples format, return
        a URI, Literal, or BNode.
        """
        if not string_term:
            return string_term
        
        parsed = strings.uriref(string_term)
        if parsed:
            return URI(parsed)

        parsed = strings.literal(string_term)
        if parsed:
            return Literal(*parsed)

        parsed = strings.nodeid(string_term)
        if parsed:
            return BNode(parsed)

        return Literal(string_term)
