#!/usr/bin/env python
# -*- coding: utf-8 -*-

from franz.valuenode import ValueNode

class ResourceNode(ValueNode):
    """ 

    """
    def __init__(self):
        super(self, ResourceNode)

    def addProperty(self, property, value):
        self.owner.addStatement(self, property, value)

    def getSubjectStatements(self):
        return self.owner.getStatements(self, None, None)


