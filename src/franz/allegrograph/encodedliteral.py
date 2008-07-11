#!/usr/bin/env python
# -*- coding: utf-8 -*-

from franz.allegrograph.exceptions import IllegalStateException
from franz.openrdf.modelimpl.literalimpl import LiteralImpl

class EncodedLiteral(LiteralImpl):
    """ generated source for EncodedLiteral

    """
    #def __init__(self, ts, value, newEncoding):
    def __init__(self, value=None, encoding=None, store=None):
        super(EncodedLiteral, self).__init__(store=store)
        self.encoding = encoding
        if isinstance(value, (int, long)):
            self.longValue = value
            self.rawType = 0  # 0-long  1-double  2-string
        elif isinstance(value, float):
            self.doubleValue = value
            self.rawType = 1  # 0-long  1-double  2-string
        else:
            self.stringValue = value
            self.rawType = 2

    def getEncoding(self):
        "Get the encoding used for this literal value."
        return self.encoding

    def getValue(self):
        "Get the object that represents the encoded value of the literal"
        if self.rawType == 0:
            return long(self.longValue)
        elif self.rawType == 1:
            return float(self.doubleValue)  ## guessing that 'float' is OK; was 'Double' in Java
        else:
            return self.stringValue

    def stringValue(self):
        if self.rawType == 0:
            return "N" + self.longValue
        elif self.rawType == 1:
            return "D" + self.doubleValue
        elif self.rawType == 2:
            return "S" + self.stringValue
        raise IllegalStateException("bad rawType " + self.rawType)

    def add(self):
        if self.canReference():
            return
        v = self.owner.verifyEnabled().addPart(self.owner, self.owner.refEncToString(self))
        self.upi = v[0]

    def __str__(self):
        return "<Literal " + self.upi + " " + self.label + " :" + self.encoding + ">"

    def getLanguage(self):
        return


