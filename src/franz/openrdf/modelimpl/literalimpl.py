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


from franz.openrdf.exceptions import *
from franz.openrdf.model.literal import Literal


LANG_IS_KNOWN = 'known'
LANG_NOT_KNOWN = 'unknown'

class LiteralImpl(Literal):
    """
    Implementation of 'Literal' customized for AllegroGraph
    """
    def __init__(self, value, datatype=None, language=None, store=None, upi=None):
        super(LiteralImpl, self).__init__(value, datatype=datatype, language=language)
        self.internal_store = None
        self.upi = upi
        self.typeId = None
        self.type = ""
        self.langSlot = None
        self.language = ""
        self.upi = None
        self.label = None
        self.type = None
        self.language = None
        self.typeId = None
        self.langSlot = LANG_NOT_KNOWN

    def assign_literal_pieces(self, store, upi, newLabel, newTypeId, newType, newLanguage):
        """
        """
        self.internal_store = store
        self.upi = upi
        self.label = newLabel
        self.type = newType
        self.language = newLanguage
        if newLanguage:
            self.langSlot = LANG_IS_KNOWN
        self.typeId = newType

