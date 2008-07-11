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


from franz.allegrograph.exceptions import AllegroGraphException, IllegalArgumentException, IllegalStateException
from franz.transport.agc import *
from franz.valueobject import ValueObject
from franz.allegrograph.upi import UPI

class Triple(ValueObject):
    """ generated source for Triple

    """
    NO_TRIPLE = -1
    AGId = long()
    inferred = False

    def queryAGId(self):
        return self.AGId

    def getAGId(self):
        if self.canReference():
            return self.AGId
        raise AllegroGraphException("This object does not have a valid id.")

    def add(self):
        if self.canReference():
            return False
        if (None == self.owner):
            raise IllegalStateException("Cannot assert Triple with null triple store.")
        self.owner.addStatement(self.subject, self.predicate, self.object, self.context)
        return True

    def sameAGId(self, x):
        if x is None:
            return 0
        if isinstance(x, (Triple)):
            xid = x.AGId
            if self.canReference(self.AGId) and self.canReference(xid):
                if (self.AGId == xid):
                    return 1
                return 0
        return -1

    def canReference(self):
        return self.canReference(self.AGId)

    serialVersionUID = -235560515765409236L
    s = UPI()
    p = UPI()
    o = UPI()
    c = UPI()
    subject = None
    predicate = None
    object = None
    context = None
    sType = 0
    pType = 0
    oType = 0
    cType = 0
    sTypeName = None
    pTypeName = None
    oTypeName = None
    cxTypeName = None
    subjMod = None
    predMod = None
    objMod = None
    cxMod = None
    subjInstance = None
    predInstance = None
    objInstance = None
    cxInstance = None
    emptyArray = [None for i in range(0)]

    def __init___(self, i, ts, ss, pp, oo=None, cc=None):
        if isinstance(i, long):
            self.AGId = i
        else:
            self.AGId = self.NO_TRIPLE
            ## 'i' is really 'ts'; shift all arg values by one:
            cc = oo;
            oo = pp;
            pp = ss;
            ss = ts;
            ts = i;
        self.owner = ts
        self.AGId = self.NO_TRIPLE
        self.s = ss
        self.p = pp
        self.o = oo
        self.c = UPI.refNull(cc) if cc else UPI.getNullContextUPI()

    def queryS(self):
        return self.s

    def getS(self):
        if self.s is None:
            self.getParts()
        return self.s

    def querySubject(self):
        return self.subject

    def getSubjectLabel(self):
        if self.subject is None:
            self.getParts()
        return self.subject

    def getSubject(self):
        return self.getSubjectInstance()

    def getSubjectInstance(self):
        if (None != self.subjInstance):
            return self.subjInstance
        if self.sType > 0 and (None != self.subject):
            self.subjInstance = self.owner.newValue(self.s, self.sType, self.subject, self.subjMod)
        else:
            self.subjInstance = self.owner.newValue(self.getS())
        return self.subjInstance

    def queryP(self):
        return self.p

    def getP(self):
        if self.p is None:
            self.getParts()
        return self.p

    def queryPredicate(self):
        return self.predicate

    def getPredicateLabel(self):
        if self.predicate is None:
            self.getParts()
        return self.predicate

    def getPredicate(self):
        return self.getPredicateInstance()

    def getPredicateInstance(self):
        if (None != self.predInstance):
            return self.predInstance
        if self.pType > 0 and (None != self.predicate):
            self.predInstance = self.owner.newValue(self.p, self.pType, self.predicate, self.predMod)
        else:
            self.predInstance = self.owner.newValue(self.getP())
        return self.predInstance

    def queryO(self):
        return self.o

    def getO(self):
        if self.o is None:
            self.getParts()
        return self.o

    def queryObject(self):
        return self.object

    def getObjectLabel(self):
        if self.object is None:
            self.getParts()
        return self.object

    def getObject(self):
        if (None != self.objInstance):
            return self.objInstance
        if self.oType > 0 and (None != self.object):
            self.objInstance = self.owner.newValue(self.o, self.oType, self.object, self.objMod)
        else:
            self.objInstance = self.owner.newValue(self.getO())
        return self.objInstance

    def getC(self):
        return self.c

    def queryContext(self):
        return self.context

    def getContextLabel(self):
        if self.context is None:
            self.getParts()
        return self.context

    def getContext(self):
        if (None != self.cxInstance):
            return self.cxInstance
        if self.cType > 0 and (None != self.context):
            self.cxInstance = self.owner.newValue(self.c, self.cType, self.context, self.cxMod)
        else:
            self.cxInstance = self.owner.newValue(self.getC())
        return self.cxInstance

    def getParts(self):
        try:
            if not UPI.can_reference(self.s) or not UPI.can_reference(self.p) or not UPI.can_reference(self.o):
                if ValueObject.canReference(self.AGId):
                    v = self.owner.getTripleParts(self.AGId)
                    if v is None:
                        raise IllegalStateException("Cannot reference this triple id " + self.AGId)
                    self.s = v[0]
                    self.p = v[1]
                    self.o = v[2]
                    self.c = v[3]
                else:
                    raise IllegalStateException("Cannot reference this triple id " + self.AGId)
        except (AllegroGraphException, ), e:
            raise IllegalStateException("Cannot reference this triple id " + self.AGId)
        ids = UPI()
        types = [0 for i in range(4)]
        labels = ['' for i in range(4)]
        mods = ['' for i in range(4)]
        try:
            self.owner.getPartsInternal(ids, types, labels, mods)
        except (AllegroGraphException, ), e:
            raise IllegalStateException("Failed to get all the parts of a triple " + e)
        self.subject = labels[0]
        self.predicate = labels[1]
        self.object = labels[2]
        self.context = labels[3]
        self.sType = types[0]
        self.pType = types[1]
        self.oType = types[2]
        self.cType = types[3]
        self.subjMod = mods[0]
        self.predMod = mods[1]
        self.objMod = mods[2]
        self.cxMod = mods[3]

    def showPart(self, id,
                       label,
                       type,
                       mod,
                       val):
        if val is not None:
            return str(val)
        if label is not None:
            if type == AGU_ANON:
                return "_:" + label
            elif type == AGU_NODE:
                return label
            elif type == AGU_LITERAL_LANG:
                return "\"" + label + "\"@" + mod
            elif type == AGU_TYPED_LITERAL:
                return "\"" + label + "\"^^" + mod
            else:
                return "\"" + label + "\""
        if id is not None:
            return str(id)
        return "null"

    def __str__(self):
        cp = ""
        if self.owner.isDefaultGraph(self.c) | self.owner.isDefaultGraph(self.cType):
            cp = ""
        else:
            if self.c is None:
                cp = " null"
            else:
                cp = " " + self.showPart(self.c, self.context, self.cType, self.cxMod, self.cxInstance)
        return "<Triple " + self.AGId + ": " + self.showPart(self.s, self.subject, self.sType, self.subjMod, self.subjInstance) + " " + self.showPart(self.p, self.predicate, self.pType, self.predMod, self.predInstance) + " " + self.showPart(self.o, self.object, self.oType, self.objMod, self.objInstance) + cp + ">"

    def __eq__(self, other):
        if self.sameAGId(other) == 1:
            return True
        elif self.sameAGId(other) == 0:
            return False
        if isinstance(other, Statement):
            os = other
            return self.getSubject() == os.getSubject() and self.getPredicate() == os.getPredicate() and self.getObject() == os.getObject()
        return False

    def __hash__(self):
        return 961 * self.getSubject().hashCode() + 31 * self.getPredicate().hashCode() + self.getObject().hashCode()

    def __cmp__(self, to):
        if self.AGId < to.AGId:
            return -1
        if self.AGId > to.AGId:
            return +1
        return 0


