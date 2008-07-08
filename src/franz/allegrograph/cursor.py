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
from franz.allegrograph.quad import Quad
from franz.openrdf.modelimpl.valueimpl import BNodeImpl, URIImpl
from franz.openrdf.modelimpl.literalimpl import LiteralImpl
 

## Value to return when an SPO value is missing:
NO_TRIPLE = -1
DEFAULT_LOOKAHEAD = 1000
    
class Cursor:
    """ 
    This class implements a generator for multiple Triple instances.
    Many triple store search operations may generate an indeterminate number of
    results. These operations return a Cursor instance which may be used to
    iterate through the available results.
    """
    defaultLookAhead = DEFAULT_LOOKAHEAD
    emptyCursor = None     
    NO_VALUE = -1
    def __init__(self, internal_store, cursor, newts=None, newdefs=None):
        self.lookAhead = self.initLookAhead(internal_store)
        self.source = cursor
        self.agStore = internal_store
        self.withParts = True if newdefs else False
        self.nextp = False
        ## NOT SURE WHERE THIS GOES:
        self.id = NO_TRIPLE
        if newts:
            if newdefs:
                self.setCache(newts, newdefs=newdefs)
            else:
                self.setCache(newts, setNext=True)
        self.current_quad = Quad(internal_store)

    def getStore(self):
        return self.agStore

    def pop_current_quad(self):
        """
        Remove and return the current quad.  Replace it with a fresh
        one.  Called by the statements iterator to extract cursor
        variables into a stand-alone statement object. 
        """
        quad = self.current_quad
        self.current_quad = Quad(self.agStore)
        return quad

    @staticmethod
    def initLookAhead(rts):
        if rts is None:
            return 0
        if (0 == rts.defaultLookAhead):
            return Cursor.defaultLookAhead
        return rts.defaultLookAhead

    @staticmethod
    def getDefaultLookAhead():
        return Cursor.defaultLookAhead

    @staticmethod
    def setDefaultLookAhead(lh):
        if lh < 1:
            Cursor.defaultLookAhead = DEFAULT_LOOKAHEAD
        else:
            Cursor.defaultLookAhead = lh

    def getLookAhead(self):
        """
        The number of triples to transfer from AG to Java whenever a
        Cursor is advanced. The built-in initial value is 1000.        
        """
        return self.lookAhead

    def setLookAhead(self, lh):
        if lh < 1:
            self.lookAhead = self.defaultLookAhead
        else:
            self.lookAhead = lh
            
    #######################################################################################
    ## Cache array is null or a sequence of ids triple-upi subject-upi
    ## predicate-upi object-upi context-upi tr-upi s-upi p-upi o-upi c-upi tr-upi s-upi
    ## p-upi o-upi c-upi ... continued-p triple-upi and continued-p are stored in
    ## code field of UPI instances.
    ## 
    ## continued-p is >0 if more, 0 if no more, -n if n entries were left
    ## behind.
    ## 
    ## Cache has shadow arrays cTypes cVals cMods that hold triple parts info
    ## when that info has been fetched.
    ## 
    ## In cache array, a negative value for triple or triple part is a relative
    ## index to where the real data is for a duplicate triple or part.
    #######################################################################################

    def setCache(self, newts, newdefs=None, setNext=False):
        if newdefs:
            self.setCache_WithDefs(newts, newdefs)
        else:
            self.setCache_WithBoolean(newts, setNext)
    
    def setCache_WithBoolean(self, newts, setNext):
        if setNext:
            self.cacheIndex = -1
        if newts is None:
            if setNext:
                self.nextp = False
            return
        ## Set these to null only if we are storing a new cache array
        ## otherwise, keep the old cache available.
        self.cacheIndex = -1
        self.cTypes = None
        self.cVals = None
        self.cMods = None
        if 0 == len(newts):
            if setNext:
                self.nextp = False
            return
        if 1 == len(newts):
            if 0 < newts[0].getCode():
                self.nextp = True
            return
        ## If we received only the triples, and not the part strings, then
        ##  there are no negative back indexes in the cache.        
        self.cache = newts[:]
        self.cacheIndex = 0
        self.nextp = True

    def setCache_WithDefs(self, newts, newdefs):
        self.cacheIndex = -1
        self.cTypes = None
        self.cVals = None
        self.cMods = None
        if 0 == len(newts):
            self.nextp = False
            return
        if 1 == len(newts):
            self.cacheIndex = 0
            if 0 < newts[0].getCode():
                self.nextp = True
            return
        self.cache = newts[:]
        self.cacheIndex = 0
        self.nextp = True
        clen = len(self.cache)
        self.cTypes = [0 for i in range(clen)]
        self.cVals = [None for i in range(clen)]
        self.cMods = [None for i in range(clen)]
        sx = 0
        cy = 0
        for i in range(clen):
            ce = self.cache[i]
            cen = ce.getCode()
            if ce.upi is None and cen < 0:
                cx = i + cen
                self.cache[i] = self.cache[cx]
                self.cTypes[i] = self.cTypes[cx]
                self.cVals[i] = self.cVals[cx]
                self.cMods[i] = self.cMods[cx]
            else:
                if cy == 0:
                    self.cTypes[i] = 6
                    self.cVals[i] = None
                    self.cMods[i] = None
                else:
                    sx = self.decodeDef(newdefs, sx, i, self.cTypes, self.cVals, self.cMods)
            if (cy == 4):
                cy = 0
            else:
                cy += 1

    @staticmethod
    def decodeDef(defs, sx, i, types, vals, mods):
        defn = defs[sx]
        dl = len(defn)
        if not defn.startswith("%") or dl < 2:
            raise IllegalArgumentException("Ill-formed node ref(a) " + defn)
        if Cursor.regionMatchesOne(defn, 1, "P", "p"):
            return Cursor.decodeDef(defs, sx + 1, i, types, vals, mods)
        if Cursor.regionMatchesOne(defn, 1, "B", "b"):
            types[i] = AGU_ANON
            vals[i] = defn[2:]
            mods[i] = None
        elif Cursor.regionMatchesOne(defn, 1, "N", "n"):
                types[i] = AGU_NODE
                vals[i] = defn[2:]
                mods[i] = None
        elif Cursor.regionMatchesOne(defn, 1, "L", "l"):
                types[i] = AGU_LITERAL
                vals[i] = defn[2:]
                mods[i] = None
        elif Cursor.regionMatchesOne(defn, 1, "G", "g"):
                types[i] = AGU_LITERAL_LANG
                Cursor.decodeBaseFifty(defn, i, mods, vals)
        elif Cursor.regionMatchesOne(defn, 1, "T", "t"):
                types[i] = AGU_TYPED_LITERAL
                Cursor.decodeBaseFifty(defn, i, vals, mods)
        elif Cursor.regionMatchesOne(defn, 1, "M", "m"):
                types[i] = AGU_NODE
                Cursor.decodeNodeWithPrefix(defs, sx, i, vals, mods)
        elif (Cursor.regionMatchesOne(defn, 1, "X", "x") and
              Cursor.regionMatchesOne(defn, 2, "D", "d")):
                types[i] = AGU_DEFAULT_GRAPH
                vals[i] = "default graph"
                mods[i] = None
        elif Cursor.regionMatchesOne(defn, 1, "E", "e"):
                br = defn.indexOf(";", 2)
                if br < 4:
                    raise IllegalArgumentException("Ill-formed node ref(c) " + defn)
                mods[i] = defn[2:br]
                vals[i] = defn[br + 2:]
                if Cursor.regionMatchesOne(defn, br + 1, "S", "s"):
                    types[i] = AGU_ENCODED_STRING
                elif Cursor.regionMatchesOne(defn, br + 1, "N", "n"):
                        types[i] = AGU_ENCODED_INTEGER
                elif Cursor.regionMatchesOne(defn, br + 1, "D", "d"):
                        types[i] = AGU_ENCODED_FLOAT
                else:
                    raise IllegalArgumentException("Ill-formed node ref(d) " + defn)
        else:
            raise IllegalArgumentException("Ill-formed node ref(e) " + defn)
        return sx + 1
    
    @staticmethod
    def regionMatchesOne(string, offset, chr1, chr2):
        "Return true is the character in 'string' at offset 'offset' matches either chr1 or chr2."
        return string[offset] == chr1 or string[offset] == chr2 
    
    @staticmethod
    def baseFiftyDigit(defn, j, d):
        digitChar = defn[j]
        digit = "0123456789abcdefghijklmnopqrstABCDEFGHIJKLMNOPQRST".find(digitChar)
        return d * 50 + digit

    @staticmethod
    def decodeBaseFifty(defn, i, s1, s2):
        d = 0
        ## for-while
        j = 2
        while j < len(defn):
            if Cursor.regionMatchesOne(defn, j, "X", "x"):
                s1[i] = defn[j + 1:j + 1 + d]
                s2[i] = defn[j + 1 + d:]
                return
            d = Cursor.baseFiftyDigit(defn, j, d)
            j += 1
        raise IllegalArgumentException("Ill-formed node ref(b) " + defn)

    @staticmethod
    def decodeNodeWithPrefix(defs, cx, i, s1, s2):
        defn = defs[cx]
        d = 0
        ## for-while
        j = 2
        while j < len(defn):
            if Cursor.regionMatchesOne(defn, j, "X", "x"):
                s1[i] = defs[d][2:] + defn[j + 1:]
                s2[i] = None
                return
            d = Cursor.baseFiftyDigit(defn, j, d)
            j += 1
        raise IllegalArgumentException("Ill-formed node ref(d) " + defn)
    
    #######################################################################################
    ##
    #######################################################################################


    ## CURRENTLY NOT USED:
    def getTriple(self):
        if not self.atTriple():
            return
        tr = Triple(self.agStore, self.id, self.s, self.p, self.o, self.c)
        tr.subject = self.sVal
        tr.sType = self.sType
        tr.subjMod = self.sMod
        tr.object = self.oVal
        tr.oType = self.oType
        tr.objMod = self.oMod
        tr.predicate = self.pVal
        tr.pType = self.pType
        tr.predMod = self.pMod
        tr.context = self.cVal
        tr.cType = self.cType
        tr.cxMod = self.cMod
        return tr
    
    def __iter__(self): return self

    def next(self):
        if self.nextp:
            try:
                self.step()
            except (AllegroGraphException, ), e:
                raise IllegalStateException("Cursor.next " + e)
        return self.getTriple()

    def atTriple(self):
        return (self.id != NO_TRIPLE)

    def hasNext(self):
        return self.nextp

    def limitReached(self):
        return self.truncated

    def close(self):
        if self.source is None:
            return
        if self.agStore is None:
            return
        try:
            self.agStore.discardCursor(self.source)
        except (AllegroGraphException, ), e:
            raise IllegalStateException("Cursor.close " + e)
        finally:
            self.source = None

    def finalize(self):
        if self.agStore is None:
            return
        if (None == self.agStore.agConnection):
            return
        if (None != self.source):
            self.agStore.agConnection.oldTokens.add(self.source)

    def isCacheAvailable(self):
        return self.nextp and self.cache is not None and 0 <= self.cacheIndex and self.cacheIndex < len(self.cache) - 1

    def stepVal(self):
        if self.cVals:
            return self.cVals[self.cacheIndex]
        return None

    def stepType(self):
        if self.cTypes is not None:
            return self.cTypes[self.cacheIndex]
        return 0

    def stepMod(self):
        if self.cMods:
            return self.cMods[self.cacheIndex]
        return None

    def erase_currenet_quad(self):
        quad = self.current_quad
        quad.id = NO_TRIPLE
        quad.s = None
        quad.p = None
        quad.o = None
        quad.c = None
        quad.sVal = None
        quad.pVal = None
        quad.oVal = None
        quad.cVal = None
        quad.sType = 0
        quad.pType = 0
        quad.oType = 0
        quad.cType = 0
        quad.sMod = None
        quad.pMod = None
        quad.oMod = None
        quad.cMod = None

    def step(self):
        if self.isCacheAvailable():
            return self.stepCache()
        if not self.nextp:
            return False
        if self.source is None:
            return False
        if self.withParts:
            v = self.agStore.agConnection.getServer().nextCursorAndParts(self.agStore, self.source, self.lookAhead)
            r = v[0]
            d = v[1]
            if r is None:
                self.setCache(None, setNext=True)
                return False
            self.setCache(r, newdefs=d)
        else:
            r = self.agStore.agConnection.getServer().nextCursor(self.agStore, self.source, self.lookAhead)
            if r is None:
                self.setCache(None, setNext=True)
                return False
            self.setCache(r, setNext=True)
        return self.stepCache()
    
    def stepCache(self):
        ## if the current quad is being re-used, clear the
        ## previous contents; otherwise, it has been shipped with
        ## a statement, so we need to allocate a new one:
        if self.current_quad:
            self.erase_currenet_quad()
        else:
            self.current_quad = Quad(self.internal_ag_store)
        self.id = self.getCache().getCode()
        self.cacheIndex += 1
        quad = self.current_quad
        quad.s = self.getCache()
        quad.sVal = self.stepVal()
        quad.sType = self.stepType()
        quad.sMod = self.stepMod()
        self.cacheIndex += 1
        quad.p = self.getCache()
        quad.pVal = self.stepVal()
        quad.pType = self.stepType()
        quad.pMod = self.stepMod()
        self.cacheIndex += 1
        quad.o = self.getCache()
        quad.oVal = self.stepVal()
        quad.oType = self.stepType()
        quad.oMod = self.stepMod()
        self.cacheIndex += 1
        quad.c = self.getCache()
        quad.cVal = self.stepVal()
        quad.cType = self.stepType()
        quad.cMod = self.stepMod()
        self.cacheIndex += 1
        ## WE BELIEVE THIS CALL TO BE REDUNDANT:
        ##self.extractCachedPieces()
        if (self.cacheIndex == len(self.cache) - 1):
            ce = self.cache[self.cacheIndex]
            cx = ce.getCode()
            if cx > 0 and self.source is not None:
                self.nextp = True
            else:
                self.nextp = False
                if cx < 0:
                    self.truncated = True
            self.setCache(None, setNext=False)
        else:
            self.nextp = True
        return True

    def getCache(self):
        cupi = self.cache[self.cacheIndex]
        if (None != cupi.upi):
            return cupi
        idnum = cupi.getCode()
        if idnum < 0:
            return self.cache[self.cacheIndex + idnum]
        return cupi
    
    ############################################################################################
    ## IT APPEARS THAT THE LOGIC BELOW, WHICH HINGES ON CALLING 'getCachedAll', IS 
    ## REDUNDANT, I.E., IT IS SUPERCEDED BY THE stepCache LOGIC.  WE'LL SEE  - RMM
    ############################################################################################
    
#    def extractCachedPieces(self):
#        """
#        If there are more pieces that need to be extracted
#        from 'self' into the quad, do that 
#        """
#        quad = self.current_quad
#        if quad.sType == 0:
#            self.getCachedAll(1)
#        if quad.pType == 0:
#            self.getCachedAll(2)
#        if quad.oType == 0:
#            self.getCachedAll(3)
#        if quad.cType == 0:
#            self.getCachedAll(4)
#            
#    def getCachedAll(self, spocIndex):
#        quad = self.current_quad
#        if spocIndex == 1:
#            quad.sType = self.getCachedType(spocIndex)
#            quad.sVal = self.getCachedValue(spocIndex)
#            quad.sMod = self.getCachedModifier(spocIndex)
#        elif spocIndex == 2:
#            quad.pType = self.getCachedType(spocIndex)
#            quad.pVal = self.getCachedValue(spocIndex)
#            self.pMod = self.getCachedModifier(spocIndex)
#        elif spocIndex == 3:
#            quad.oType = self.getCachedType(spocIndex)
#            quad.oVal = self.getCachedValue(spocIndex)
#            quad.oMod = self.getCachedModifier(spocIndex)
#        elif spocIndex == 4:
#            quad.cType = self.getCachedType(spocIndex)
#            quad.cVal = self.getCachedValue(spocIndex)
#            quad.cMod = self.getCachedModifier(spocIndex)
#            
#    def getCachedType(self, tpart):
#        if self.cache is None:
#            return 0
#        if self.cacheIndex < 0:
#            return 0
#        if self.cTypes is None:
#            self.getTripleComponents()
#        return self.cTypes[self.cacheIndex - 5 + tpart]
#
#    def getCachedValue(self, tpart):
#        if self.cache is None:
#            return
#        if self.cacheIndex < 0:
#            return
#        if self.cTypes is None:
#            self.getTripleComponents()
#        return self.cVals[self.cacheIndex - 5 + tpart]
#
#    def getCachedModifier(self, tpart):
#        if self.cache is None:
#            return
#        if self.cacheIndex < 0:
#            return
#        if self.cTypes is None:
#            self.getTripleComponents()
#        return self.cMods[self.cacheIndex - 5 + tpart]

 
    ##@synchronized(mlock)
#    def remove(self):
#        if not self.atTriple():
#            raise IllegalStateException("Nothing to remove")
#        self.setTriple()



    ##@synchronized(mlock)
    def getTripleComponents(self):
        ## This actually never gets called if all fetching is done with
        ## nextCursorAndParts().
        if self.cache is None:
            return
        if self.cTypes is not None:
            return
        ln = len(self.cache)
        sln = 0
        cacheCycle = 5
        cc = 0
        ## for-while
        i = 0
        while i < ln - 1:
            if (cc == 0):
                cc = cacheCycle
            else:
                cu = self.cache[i]
                if cu.withLabel():
                    sln += 1
            cc -= 1
            i += 1
        shortCache = [None for i in range(sln)]
        cachePos = [0 for i in range(ln)]
        cc = 0
        sx = 0
        ## for-while
        i = 0
        while i < ln - 1:
            if (cc == 0):
                cc = cacheCycle
                cachePos[i] = -1
            else:
                cu = self.cache[i]
                if cu.withLabel():
                    shortCache[sx] = cu
                    sx += 1
                    cachePos[i] = -2
                else:
                    cachePos[i] = -1
            cc -= 1
            i += 1
        stp = [0 for i in range(sln)]
        svl = [None for i in range(sln)]
        smd = [None for i in range(sln)]
        self.agStore.verifyEnabled().getParts(self.agStore, shortCache, stp, svl, smd)
        tp = [0 for i in range(ln)]
        vl = [None for i in range(ln)]
        md = [None for i in range(ln)]
        sx = 0
        ## for-while
        i = 0
        while i < ln - 1:
            if cachePos[i] == -1:
                break
            elif cachePos[i] == -2:
                tp[i] = stp[sx]
                vl[i] = svl[sx]
                md[i] = smd[sx]
                sx += 1
                break
            i += 1
        self.cTypes = tp
        self.cVals = vl
        self.cMods = md


    ##@synchronized(mlock)
#    def set_step(self, n):
#        have = 0
#        if self.isCacheAvailable():
#            have = len(self.cache) - self.cacheIndex - 1 / 4
#        newa = None
#        newl = 0
#        newd = None
#        if have < n and self.cache is not None and self.cache[len(self.cache) - 1].getCode() > 0 and self.source is not None:
#            ra = None
#            if self.withParts:
#                v = self.agStore.verifyEnabled().nextCursorAndParts(self.agStore, self.source, n - have)
#                ra = v[0]
#                newd = v[1]
#            else:
#                ra = self.agStore.verifyEnabled().nextCursor(self.agStore, self.source, n - have)
#            if ra is not None:
#                newa = ra
#                newl = len(newa) - 1 / 4
#        r = [None for i in range(have + newl)]
#        ## for-while
#        i = 0
#        while i < have:
#            self.stepCache()
#            r[i] = Triple(self.agStore, self.upi, self.s, self.p, self.o, self.c)
#            i += 1
#        if newa is None:
#            self.setCache(None, setNext=True)
#            return r
#        self.setCache(newa, newdefs=newd)
#        ## for-while
#        i = 0
#        while i < newl:
#            self.stepCache()
#            r[have + i] = Triple(self.agStore, self.upi, self.s, self.p, self.o, self.c)
#            i += 1
#        return r
#
#    step = property(get_step, set_step)

Cursor.emptyCursor = Cursor(None, None)  
