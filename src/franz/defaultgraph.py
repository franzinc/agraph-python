#!/usr/bin/env python
# -*- coding: utf-8 -*-

from franz.resourcenode import ResourceNode

class DefaultGraph(ResourceNode):
    """

    """

    def __init__(self, ag, id):
        super(self, ResourceNode)
        self.nodeUPI = id
        self.owner = ag
        self.str = None

    def __str__(self):
        if self.strval is not None:
            return self.strval
        agx = ""
        if self.owner is None:
            agx = ""
        else:
            agx = "" + self.owner.tsx
        if (None != self.nodeUPI):
            agx = agx + " " + self.nodeUPI.getStoreBytes()
        self.strval = "<DefaultGraph " + agx + ">"
        return self.strval


