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

from  __future__ import with_statement
import threading

from franz.exceptions import IllegalArgumentException, IllegalStateException, AllegroGraphException
from franz.transport.agc import *
from franz.allegrograph.upi import UPI
from franz.openrdf.modelimpl.valueimpl import BNodeImpl, URIImpl
from franz.openrdf.modelimpl.literalimpl import LiteralImpl

## Value to return when an SPO value is missing:
NO_TRIPLE = -1

class Quad:
    """
    Historical note: Carves out logic that was previously replicated in Cursor and Triple
    """
    COMPLAIN_BITTERLY = True
    def __init__(self, internal_store):
        self.internal_store = internal_store
        self.id = NO_TRIPLE
        self.s = None
        self.p = None
        self.o = None
        self.c = None
        self.sVal = None
        self.sType = 0
        self.sMod = None
        self.pVal = None
        self.pType = 0
        self.pMod = None
        self.oVal = None
        self.oType = 0
        self.oMod = None
        self.cVal = None
        self.cType = 0
        self.cMod = None
        self.nextp = False
        self.truncated = False
        self.cacheIndex = -1
        self.cTypes = None
        self.cVals = None
        self.cMods = None        

    ##################################################################################
    ## Component Assembly from pieces
    ##################################################################################
    
    def getS(self):
        return self.s

    def querySubject(self):
        return self.sVal

    def queryObject(self):
        return self.oVal

    def queryPredicate(self):
        return self.pVal

    def queryContext(self):
        return self.cVal

    ##@synchronized(mlock)
    def getSubjectLabel(self):
        self.sVal = self.getPartLabel(self.s, 1, self.sType, self.sVal)
        return self.sVal

     ##@synchronized(mlock)
    def getP(self):
        return self.p

    ##@synchronized(mlock)
    def getPredicateLabel(self):
        self.pVal = self.getPartLabel(self.p, 2, self.pType, self.pVal)
        return self.pVal

    ##@synchronized(mlock)
    def getO(self):
        return self.o

    ##@synchronized(mlock)
    def getC(self):
        return self.c

    ##@synchronized(mlock)
    def getObjectLabel(self):
        self.oVal = self.getPartLabel(self.o, 3, self.oType, self.oVal)
        return self.oVal

    ##@synchronized(mlock)
    def getContextLabel(self):
        self.cVal = self.getPartLabel(self.c, 4, self.cType, self.cVal)
        return self.cVal
    
    def getUPI(self, index):
        if index == 1: return self.s
        elif index == 2: return self.p
        elif index == 3: return self.o
        elif index == 4: return self.c                

    def queryPartType(self, px):
        if px == 1:
            return self.sType
        elif px == 2:
            return self.pType
        elif px == 3:
            return self.oType
        elif px == 4:
            return self.cType
        return 0

    def queryPartLabel(self, px):
        if px == 1:
            return self.sVal
        elif px == 2:
            return self.pVal
        elif px == 3:
            return self.oVal
        elif px == 4:
            return self.cVal
        return

    def queryPartMod(self, px):
        if px == 1:
            return self.sMod
        elif px == 2:
            return self.pMod
        elif px == 3:
            return self.oMod
        elif px == 4:
            return self.cMod
        return

    ##@synchronized(mlock)
    def getPartLabel_FromIndex(self, spocIndex):
        if spocIndex == 1:
            self.sVal = self.getPartLabel(self.s, spocIndex, self.sType, self.sVal)
            return self.sVal
        elif spocIndex == 2:
            self.pVal = self.getPartLabel(self.p, spocIndex, self.pType, self.pVal)
            return self.pVal
        elif spocIndex == 3:
            self.oVal = self.getPartLabel(self.o, spocIndex, self.oType, self.oVal)
            return self.oVal
        elif spocIndex == 4:
            self.cVal = self.getPartLabel(self.c, spocIndex, self.cType, self.cVal)
            return self.cVal
        raise IllegalArgumentException("Not a triple part index: " + spocIndex)

    ##@synchronized(mlock)
    def getPartLabel(self, part, spocIndex, type, label):
        if part is None:
            return
        if (type == 0):
            type = self.queryPartType(spocIndex)
            label = self.queryPartLabel(spocIndex)
        if (type == 0):
            v = self.getAll(part, spocIndex)
            label = v[1]
        return label

    ##@synchronized(mlock)
    def getPartMod_FromIndex(self, spocIndex):
        if spocIndex == 1:
            self.sMod = self.getPartMod(self.s, spocIndex, self.sType, self.sMod)
            return self.sMod
        elif spocIndex == 2:
            self.pMod = self.getPartMod(self.p, spocIndex, self.pType, self.pMod)
            return self.pMod
        elif spocIndex == 3:
            self.oMod = self.getPartMod(self.o, spocIndex, self.oType, self.oMod)
            return self.oMod
        elif spocIndex == 4:
            self.cMod = self.getPartMod(self.c, spocIndex, self.cType, self.cMod)
            return self.cMod
        raise IllegalArgumentException("Not a triple part index: " + spocIndex)

    ##@synchronized(mlock)
    def getPartMod(self, part, spocIndex, type, mod):
        if part is None:
            return
        if mod is not None:
            return mod
        if (type == 0):
            type = self.queryPartType(spocIndex)
            mod = self.queryPartMod(spocIndex)
        if (type == 0):
            v = self.getAll(part, spocIndex)
            mod = v[2]
        return mod

    def getPartType_FromIndex(self, spocIndex):
        if spocIndex == 1:
            self.sType = self.getPartType(self.s, spocIndex, self.sType)
            return self.sType
        elif spocIndex == 2:
            self.pType = self.getPartType(self.p, spocIndex, self.pType)
            return self.pType
        elif spocIndex == 3:
            self.oType = self.getPartType(self.o, spocIndex, self.oType)
            return self.oType
        elif spocIndex == 4:
            self.cType = self.getPartType(self.c, spocIndex, self.cType)
            return self.cType
        raise IllegalArgumentException("Not a triple part index: " + spocIndex)

    def getPartType(self, part, spocIndex, type):
        if part is None:
            return 0
        if (type == 0):
            ## see if the piece we want is already stored in the cursor:
            type = self.queryPartType(spocIndex)
        if (type == 0):
            ## bad new, we need to do DB access to get the pieces:
            v = self.getAll(part, spocIndex)
            type = int(v[0])
        return type
   
    def getAll(self, part, partIndex):
        """
        Database access to get stuff; sounds expensive
        """
        if Quad.COMPLAIN_BITTERLY:
            print "ENTIRE ACCESS FOR SINGLE SPOC COMPONENT"
        v = self.internal_store.agConnection.getServer().getParts(self.internal_store, part)
        type = int(v[0])
        if partIndex == 1:
            self.sType = type
            self.sVal = v[1]
            self.sMod = v[2]
        elif partIndex == 2:
            self.pType = type
            self.pVal = v[1]
            self.pMod = v[2]
        elif partIndex == 3:
            self.oType = type
            self.oVal = v[1]
            self.oMod = v[2]
        elif partIndex == 4:
            self.cType = type
            self.cVal = v[1]
            self.cMod = v[2]
        return v

    ##################################################################################
    ## SPOC Component Assembly
    ##################################################################################
    
    def failCreate(self, what, e):
        raise IllegalStateException("Cannot create " + what + " -- " + e)

    def newValue_with_upi(self, upi):
        v = None
        if not UPI.canReference(upi):
            raise IllegalStateException("AllegroGraph Id cannot be registered:" + upi)
        try:
            r = self.internal_store.agConnection.getParts(self, upi)
            v = self.newValue(upi, r[0], r[1], r[2])
        except (AllegroGraphException, ), e:
            self.failCreate("ValueObject", e)
        if v is None:
            self.failCreate("ValueObject", None)
        return v
    
    def newValue(self, upi, component_type, label, mod):
        if component_type is None: return self.newValue_with_upi(upi)
        if component_type == AGU_ANON:
            return BNodeImpl(upi, id=label, store=self.internal_store)
        elif component_type == AGU_NODE:
            return URIImpl(upi=upi, uri=label, store=self.internal_store)
        elif component_type == AGU_LITERAL:
            lit = LiteralImpl(None)
            lit.assign_literal_pieces(self.internal_store, upi, label, None, None, None)
            return lit
        elif component_type == AGU_LITERAL_LANG:
            lit = LiteralImpl(None)
            lit.assign_literal_pieces(self.internal_store, upi, label, None, None, mod)
            return lit
        elif component_type == AGU_TYPED_LITERAL:
            lit = LiteralImpl(None)
            lit.assign_literal_pieces(self.internal_store, upi, label, None, mod, None)
            return lit
        elif component_type == AGU_DEFAULT_GRAPH:
            return URIImpl.NULL_CONTEXT_URI
        elif component_type == AGU_TRIPLE:
            idn = upi.getCode()
            if idn > -1:
                return StatementImpl(self, idn, None, None, None)
        elif component_type == AGU_ENCODED_STRING:
            newInstance = EncodedLiteral(self, label, mod)
            newInstance.nodeUPI = upi
            return newInstance
        elif component_type == AGU_ENCODED_INTEGER:
            newInstance = EncodedLiteral(self, long(label), mod)
            newInstance.nodeUPI = upi
            return newInstance
        elif component_type == AGU_ENCODED_FLOAT:
            newInstance = EncodedLiteral(self, float(label), mod)
            newInstance.nodeUPI = upi
            return newInstance
        else:
            raise IllegalArgumentException("Unknown component component_type " + component_type + "  " + label + "  " + mod)

    def getTripleComponent(self, upi, spocIndex):         
        if not upi:
            return None
        componentType = self.getPartType_FromIndex(spocIndex)
        label = self.getPartLabel_FromIndex(spocIndex)
        mod = self.getPartMod_FromIndex(spocIndex)
        return self.newValue(upi, componentType, label, mod)

    ##################################################################################
    ##  Assembly
    ##################################################################################
    
    def get_id(self):
        return self.id

    def getSubject(self):
        return self.getTripleComponent(self.s, 1)
    
    def getPredicate(self):
        return self.getTripleComponent(self.p, 2)

    def getObject(self):
        return self.getTripleComponent(self.o, 3)

    def getContext(self):
        return self.getTripleComponent(self.s, 4)

    ##################################################################################
    ##  Debugging
    ##################################################################################

    ##@synchronized(mlock)
    def __str__(self):
        triple = "empty"
        ns = ""
        if self.atTriple():
            triple = "" + self.id + ": " + self.showPart(self.sVal, self.sType, self.sMod, self.s) + " " + self.showPart(self.pVal, self.pType, self.pMod, self.p) + " " + self.showPart(self.oVal, self.oType, self.oMod, self.o) + " " + self.showPart(self.cVal, self.cType, self.cMod, self.c)
        if self.nextp:
            ns = ", with"
        else:
            ns = ", no"
        return "<Cursor " + triple + " " + ns + " next>"

    def showPart(self, val, type, mod, upi):
        if type == 1:
            return "_:blank" + val
        elif type == 2:
            return "<" + val + ">"
        elif type == 3:
            return "\"" + val + "\""
        elif type == 4:
            return "\"" + val + "@" + mod + "\""
        elif type == 5:
            return "\"" + val + "^^<" + mod + ">\""
        else:
            return "" + upi
   
    
    
    
    