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


class Statement:
    """
    Lightweight implementation of 'Statement'
    """
    def __init__(self, subject, predicate, object, context=None):
        self.subject = subject
        self.predicate = predicate
        self.object = object
        self.context = context

    def getSubject(self): return self.subject
    def getPredicate(self): return self.predicate
    def getObject(self): return self.object
    def getContext(self): return self.context    

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
        sb.append(str(self.getSubject()))
        sb.append(", ")
        sb.append(str(self.getPredicate()))
        sb.append(", ")
        sb.append(str(self.getObject()))
        sb.append(")")
        return ''.join(sb)

