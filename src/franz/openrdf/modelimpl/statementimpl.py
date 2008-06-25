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
from franz.openrdf.model.statement import Statement

class StatementImpl(Statement):
    """
    AllegroGraph implementation of 'Statement'
    """
    def __init__(self, subject, predicate, object, context=None):
        self.subject = subject
        self.predicate = predicate
        self.object = object
        self.context = context
        self.quad = None
    
    def setQuad(self, quad):
        self.quad = quad

    def getSubject(self):         
        if not self.subject:
            if self.quad: self.subject = self.quad.getSubject()
        return self.subject
    
    #def setSubject(self, subject): self.subject = subject
    def getPredicate(self):
        if not self.predicate:
            if self.quad: self.predicate = self.quad.getPredicate()
        return self.predicate
     
    #def setPredicate(self, predicate):self.predicate = predicate
    def getObject(self): 
        if not self.object:
            if self.quad: self.object = self.quad.getObject()
        return self.object
    
    #def setObject(self, object): self.object = object
    def getContext(self): 
        if not self.context:
            if self.quad: self.context = self.quad.getContext()
        return self.context  
    
    #def setContext(self, context): self.context = context
    
    
      


