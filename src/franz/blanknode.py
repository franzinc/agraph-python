#!/usr/bin/env python
# -*- coding: utf-8 -*-

from franz.resourcenode import ResourceNode

class BlankNode(ResourceNode):
    """ generated source for BlankNode

    """

    def __init__(self, ts, upi, ss):
        self.idString = ""
        self.owner = ts
        self.nodeUPI = upi
        if ss is None or ss == "":
            self.idString = "_:blank" + upi.blankNodeID()
        else:
            self.idString = "_:" + ss

    def getUPI(self):
        return self.nodeUPI

    def getID(self):
        return self.idString

    def __str__(self):
        return "<" + self.idString + ">"

    def __eq__(self, other):
        if self.sameAGId(other) == 1:
            return True
        elif self.sameAGId(other) == 0:
            return False
        if isinstance(other, BNode):
            return str(self) == str(other)
        return False

    def __hash__(self):
        return str(self).hashCode()


