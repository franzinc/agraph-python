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


from franz.exceptions import AllegroGraphException, IllegalArgumentException, IllegalStateException
from franz.valueobject import ValueObject
from franz.allegrograph.upi import UPI
from franz.openrdf.model.value import Value

class ValueNode(ValueObject, Value):
    """ 

    """

    def __init__(self):
        super(ValueNode, self).__init__()
        self.nodeUPI = None
    def queryAGId(self):
        return self.nodeUPI

    def getAGId(self):
        if self.canReference():
            return self.nodeUPI
        else: raise AllegroGraphException("This node does not have a valid id.")

    def sameAGId(self, x):
        if x is None:
            return 0
        if type(self) == type(x):
            xid = x.nodeUPI
            if UPI.canReference(self.nodeUPI) and UPI.canReference(xid):
                if self.nodeUPI == xid:
                    return 1
                return 0 ## zero apparently means "I don't know"
        return -1

    def canReference(self):
        return UPI.canReference(self.nodeUPI)

    def getObjectStatements(self):
        return self.owner.getStatements(None, None, self)

    def compareTo(self, to):
        return UPI.compare(self.nodeUPI, to.nodeUPI)


