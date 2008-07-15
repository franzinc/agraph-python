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
    def __init__(self, subject, predicate, object, context=None, store=None):
        ## I HAVE NO IDEA WHY ITS COMPLAINING HERE:
        #super(StatementImpl, self).__init__(subject, predicate, object, context=context)
        self.subject = subject
        self.predicate = predicate
        self.object = object
        self.context = context
        self.quad = None
        if store:
            internalStore = store.internal_ag_store
            self.internal_ag_store = internalStore
            if subject: subject.internal_ag_store = internalStore
            if predicate: predicate.internal_ag_store = internalStore
            if object: object.internal_ag_store = internalStore
            if context: context.internal_ag_store = internalStore
            
    def __len__(self):
        return 4 if self.getContext() else 3
    
    def __getitem__(self, index):
        if index == 0: return self.getSubject()
        elif index == 1: return self.getPredicate()
        elif index == 2: return self.getObject()
        elif index == 3: return self.getContext()
        else:
            ## I don't know what the official Python exception is here :(  - RMM
            raise IllegalArgumentException("Illegal index %s passed to StatementImpl.\n" +
                    "  Legal arguments are integers 0-3")
                                          
    
    def setQuad(self, quad):
        self.quad = quad

    def getSubject(self):         
        if not self.subject:
            if self.quad: 
                self.subject = self.quad.getSubject()
        return self.subject
    
    #def setSubject(self, subject): self.subject = subject
    def getPredicate(self):
        if not self.predicate:
            if self.quad: 
                self.predicate = self.quad.getPredicate()
        return self.predicate
     
    #def setPredicate(self, predicate):self.predicate = predicate
    def getObject(self):
        if not self.object:
            if self.quad: 
                self.object = self.quad.getObject()
        return self.object
    
    #def setObject(self, object): self.object = object
    def getContext(self): 
        if not self.context:
            if self.quad: self.context = self.quad.getContext()
        return self.context  
    
    #def setContext(self, context): self.context = context
    
    
      


