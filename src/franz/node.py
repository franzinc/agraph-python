#!/usr/bin/env python
# -*- coding: utf-8 -*-

from franz.exceptions import IllegalStateException
from franz.resourcenode import ResourceNode
from franz.allegrograph.upi import UPI
from franz.openrdf.model.value import URI


class Node(ResourceNode):
    """ generated source for Node

    """
    AGnullContext = None
    
    def __init__(self, ts, i, u):        
        self.owner = ts
        self.nodeUPI = i
        self.uri = u


    def getAGId(self):
        n = self.queryAGId()
        if UPI.can_reference(n):
            return n
        if self.uri is None:
            raise IllegalStateException("Cannot realize a node without a URI")
        n = self.owner.verifyEnabled().newResource(self.owner, self.uri)
        self.nodeUPI = n
        return n

    def queryURI(self):
        return self.uri

    def getLocalName(self):
        self.uri = self.owner.getText(self.nodeUPI, self.uri)
        p = self.uri.indexOf("#")
        if p < 0:
            return ""
        return self.uri.substring(p + 1)

    def getNamespace(self):
        self.uri = self.owner.getText(self.nodeUPI, self.uri)
        p = self.uri.indexOf("#")
        if p < 0:
            return self.uri
        return self.uri.substring(0, p + 1)

    def __str__(self):
        self.uri = self.owner.getText(self.nodeUPI, self.uri)
        return "|Node|" + self.uri

    def __eq__(self, other):
        if self.sameAGId(other) == 1:
            return True
        elif self.sameAGId(other) == 0:
            return False
        if isinstance(other, URI):
            return str(self) == str(other)
        return False

    def __hash__(self):
        ## THIS IS REALLY EXPENSIVE:
        return str(self).hashCode()

    def getPredicateStatements(self):
        return self.owner.getStatements(None, self, None)

    def getURI(self):
        return str(self)

    def add(self):
        self.getAGId()

Node.AGnullContext = Node(None, UPI.getNullContextUPI(), None)
    

