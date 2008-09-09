#!/usr/bin/env python
# -*- coding: utf-8 -*-

from franz.allegrograph.exceptions import IllegalStateException
from franz.openrdf.modelimpl.literalimpl import LiteralImpl

class EncodedLiteral(LiteralImpl):
    """ 
    Create an inlined Literal.
    """
    #def __init__(self, ts, value, newEncoding):
    def __init__(self, value=None, encoding=None, store=None):
        super(EncodedLiteral, self).__init__(value, store=store)
        self.encoding = encoding
        if encoding:
            if encoding == 'int' or encoding == 'long':
                self.longValue = int(value)
                self.rawType = 0  # 0-long  1-double  2-string
            elif encoding == 'float':
                self.doubleValue = float(value)
                self.rawType = 1  # 0-long  1-double  2-string
            if encoding == 'string':
                self.storedStringValue = str(value)
                self.rawType = 2  # 0-long  1-double  2-string
        else:
            if isinstance(value, (int, long)):
                self.longValue = value
                self.rawType = 0  # 0-long  1-double  2-string
            elif isinstance(value, float):
                self.doubleValue = value
                self.rawType = 1  # 0-long  1-double  2-string
            else:
                self.storedStringValue = value
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
            return self.storedStringValue

    def stringValue(self):
        if self.rawType == 0:
            return "N" + str(self.longValue)
        elif self.rawType == 1:
            return "D" + str(self.doubleValue)
        elif self.rawType == 2:
            return "S" + self.storedStringValue
        raise IllegalStateException("bad rawType " + self.rawType)

    def add(self):
        if self.canReference():
            return
        v = self.owner.verifyEnabled().addPart(self.owner, self.owner.refEncToString(self))
        self.upi = v[0]

    def __str__(self):
        ##return "<Literal " + " " + self.label + " :" + str(self.encoding) + ">"
        return '"%s"' % self.label if self.encoding == 'string' else self.label

    def getLanguage(self):
        return
    
    @staticmethod
    def literal_to_inlined_literal(literal, inlinedType):
        inLit = EncodedLiteral(literal.getLabel(), encoding=inlinedType)
        return inLit


