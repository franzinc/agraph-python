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

from  __future__ import with_statement
import threading
from franz.allegrograph.exceptions import IllegalArgumentException

class NamedAttributeList:
    """
    This appears to be a baroque datastructure fronting for a standard one.
    It would be nice to be able to toss it, but I don't know exactly what its for.  - RMM
    """    
    def __init__(self, defs):
        self.attributeDefs = {}        
        for att, type in defs.iteritems():
            self.attributeDefs[att.lower()] = type
        self.attributeStates = {} 
        self.setAttributeLock = threading.RLock()
    
    def attributeIndex (self, name):
        index = -1;
        for i in range(0, len(self.attributeDefs), 2):
            if name.lower() == self.attributeDefs[i]:
                index = i;
        if index == -1:
            raise IllegalArgumentException( "Not an attribute name: " + name )
        return index
    
    def getAttribute(self, name):
        return self.attributeStates.get(name.lower())
    
    def queryAttribute(self, name):
        return self.getAttribute() is not None
    
    def setAttribute(self, name, value):
        with self.setAttributeLock:
            name = name.lower()
            with self.setAttributeLock:
                vclass = type(value)
                if not vclass == self.attributeDefs[name]:
                    raise IllegalArgumentException("Value of attribute " + name + " is not of class " +
                            str(self.attributeDefs[name]) + ": " + str(value));
                self.attributeDefs[name] = value

    def getList (self):
        attList = []
        for name, state in self.attributeStates:
            attList.append(name)
            attList.append(state)
        return attList
    
    

