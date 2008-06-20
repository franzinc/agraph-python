#!/usr/bin/env python
# -*- coding: utf-8 -*-

from franz.exceptions import IllegalStateException, AllegroGraphException
from franz.valuenode import ValueNode
from franz.allegrograph.upi import UPI


LANG_IS_KNOWN = 'known'
LANG_NOT_KNOWN = 'unknown'

class Literal(ValueNode):
    """ 
     * This class represents an instance of a literal node in AllegroGraph.
     * <p>
     * The AllegroGraph object defines two slots, id and label.  Both slots are copied 
     * to the Java instance.
     * <p>
     * The label member may be a lazy value in the Java instance.  If queryLabel returns
     * null, getLabel() must make a round-trip to the triple store for the actual value.
     * <p>
     * There is no public constructor.  Literal instances are created by calls
     * to TripleStore methods.
    """
    def __init__(self, ts, i, newLabel, newTypeId, newType, newLangSlot, newLanguage):
        super(Literal, self)
        self.typeId = None
        self.type = ""
        self.langSlot = None
        self.language = ""
        self.owner = ts
        self.nodeUPI = i
        self.label = newLabel
        self.type = newType
        self.language = newLanguage
        self.typeId = newTypeId
        self.langSlot = newLangSlot

    def queryLabel(self):
        return self.label

    def getLabel(self):
        self.label = self.owner.getText(self.nodeUPI, self.label)
        return self.label

    def queryType(self):
        return self.type

    def getDatatype(self):
        tp = self.getType()
        if tp is None:
            return
        return self.owner.createURI(tp)

    def getType(self):
        if self.type is not None:
            return self.type
        if self.typeId is None:
            return
        if UPI.canReference(self.typeId):
            self.type = self.owner.getText(self.typeId, None)
        else:
            try:
                self.type = self.owner.getTypePart(self.nodeUPI)
                self.typeId = None
            except (AllegroGraphException, ), e:
                pass
        return self.type

    def queryLanguage(self):
        return self.language

    def getLanguage(self):
        if self.language is not None:
            return self.language
        if (self.LANG_KNOWN == self.langSlot):
            return self.language
        if (self.LANG_NONE == self.langSlot):
            return
        try:
            self.language = self.owner.getLangPart(self.nodeUPI)
            self.langSlot = self.LANG_KNOWN
        except (AllegroGraphException, ), e:
            pass
        return self.language

    def __str__(self):
        tail = ""
        if self.typeId is None and self.type is None:
            if (self.langSlot == self.LANG_NONE):
                tail = ""
            else:
                if self.langSlot is None:
                    tail = "@?"
                else:
                    if (self.langSlot != self.LANG_KNOWN):
                        tail = "@<err>"
                    else:
                        if self.language is None:
                            tail = ""
                        else:
                            tail = "@" + self.language
        else:
            if self.type is not None:
                tail = "^^" + self.type
            else:
                if UPI.canReference(self.typeId):
                    tail = "^^<" + self.typeId + ">"
                else:
                    tail = "^^?"
        return "<Literal " + self.nodeUPI + ": " + self.label + tail + ">"

    def stringRefEx(self):
        raise AllegroGraphException("Cannot make string ref")

    def __eq__(self, other):
        if self.sameAGId(other) == 1:
            return True
        elif self.sameAGId(other) == 0:
            return False
        if isinstance(other, (Literal)):
            return str(self) == str(other)
        return False

    def hashCode(self):
        return str(self).hashCode()

    def add(self):
        if self.canReference():
            return
        if self.label is None:
            raise IllegalStateException("Cannot add Literal with null label.")
        if self.type is not None:
            nodeUPI = self.owner.verifyEnabled().newLiteral(self.owner, self.label, self.type, None)
            return
        if UPI.canReference(self.typeId):
            nodeUPI = self.owner.verifyEnabled().newLiteral(self.owner, self.label, self.typeId, None)
            return
        if self.typeId is not None:
            raise IllegalStateException("Cannot add Literal with unknown type.")
        if (self.langSlot == self.LANG_KNOWN) and self.language is not None:
            nodeUPI = self.owner.verifyEnabled().newLiteral(self.owner, self.label, self.type, self.language)
            return
        if (self.langSlot == self.LANG_NONE):
            nodeUPI = self.owner.verifyEnabled().newLiteral(self.owner, self.label, self.type, self.language)
            return
        raise IllegalStateException("Cannot add uninitialized Literal.")

    def getAGId(self):
        x = self.queryAGId()
        if (None != x):
            return x
        self.add()
        return super.getAGId()


