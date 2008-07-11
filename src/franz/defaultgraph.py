#!/usr/bin/env python
# -*- coding: utf-8 -*-

from franz.openrdf.model.value import Resource

class DefaultGraph(Resource):
    """
    Having this class at all is a really horrible idea.  In Sesame, there is no denotation
    for the null context, so this should be superfluous.  Need to figure out how
    to eliminate it.  - RMM
    """
    def __init__(self, ag, id):
        super(self, Resource)
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


