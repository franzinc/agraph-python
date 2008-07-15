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


from franz.allegrograph.upi import UPI
from franz.openrdf.exceptions import *
from franz.openrdf.model.literal import Literal
from franz.openrdf.vocabulary.xmlschema import XMLSchema


class LiteralImpl(Literal):
    """
    Implementation of 'Literal' customized for AllegroGraph
    """
    LANG_IS_KNOWN = 'known'
    LANG_NOT_KNOWN = 'unknown'
    def __init__(self, label, datatype=None, upi=None, stringtype=None, language=None, store=None, langslot=None):
        super(LiteralImpl, self).__init__(label, datatype=datatype, language=language)
        self.internal_ag_store = store
        self.setStringType(stringtype)
        self.upi = upi
        self.typeId = None
        self.langSlot = langslot or LiteralImpl.LANG_NOT_KNOWN
        
    def setStringType(self, stringtype):
        self.stringtype = stringtype
        if stringtype and not self.datatype:
            self.datatype = XMLSchema.name2URI(stringtype)

    def assign_literal_pieces(self, store, upi, newLabel, newTypeId, newStringType, newLanguage):
        """
        """
        self.internal_ag_store = store
        self.upi = upi
        self.label = newLabel        
        self.setStringType(newStringType)
        self.language = newLanguage
        if newLanguage:
            self.langSlot = LiteralImpl.LANG_IS_KNOWN
        self.typeId = newTypeId

    def getLabel(self):
        if not self.label:
            self.label = self.internal_ag_store.getText(self.upi, self.label)
        return self.label
    
    def getDatatype(self):
        if self.datatype: return self.datatype
        sType = self._get_string_type()
        if self.datatype: return self.datatype
        elif sType: return self.internal_ag_store.createURI(sType)
        else: return None

    def _get_string_type(self):
        if self.stringtype: return self.stringtype
        if self.typeId is None:
            return None
        if UPI.can_reference(self.typeId):
            stringtype = self.internal_ag_store.getText(self.typeId, None)
        else:
            try:
                stringtype = self.internal_ag_store.getTypePart(self.upi)
                self.typeId = None
            except (AllegroGraphException, ), e:
                pass
        self.setStringType(stringtype)
        return self.stringtype

    def getLanguage(self):
        if self.language is not None:
            return self.language
        if (LiteralImpl.LANG_IS_KNOWN == self.langSlot):
            return self.language
        if (LiteralImpl.LANG_NOT_KNOWN == self.langSlot):
            return None
        try:
            self.language = self.owner.getLangPart(self.nodeUPI)
            self.langSlot = self.LANG_IS_KNOWN
        except AllegroGraphException, e:
            pass
        return self.language
