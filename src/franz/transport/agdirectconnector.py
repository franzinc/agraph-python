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

import datetime

from franz.allegrograph.exceptions import IllegalArgumentException, IllegalStateException, IOException, AllegroGraphException
from franz.transport.agc import *
from franz.transport.agconnector import *
from franz.transport.agdirectlinkdebug import AGDirectLinkDebug
from franz.transport.agdirectlink import AGDirectLink
from franz.allegrograph.cursor import Cursor
from franz.allegrograph.allegrograph import AllegroGraph
from franz.openrdf.model.value import Value


class AGDirectConnector(AGConnector):
    """ 
    xxx
    """
    def __init__(self):
        super(AGDirectConnector, self).__init__()
        self.trs = None
        
    @staticmethod
    def createConnector(mode):
        if mode is None:
            mode = ""
        if "direct" == mode.lower(): 
            return AGDirectConnector()
        ## mode must be "jlinker"
        ## return new AGJLinkerConnector();
        raise IllegalStateException("Unknown mode: " + mode)    
        
    def transportVersion(self): return AG_DIRECT_LEVEL

    def verifyLink(self):
        if self.trs is None:
            raise IllegalStateException("Cannot use disabled link.")
        return self.trs

    def tsApply0(self, ag, fn, args):
        testIndex(ag)
        try:
            return self.verifyLink().sendOp3n(OP_CALL, 1, 0, AG_APPLY, ag.tsx, fn, args)
        except (IOException, ), e:
            raise AllegroGraphException(e)

    ## Call a function and return all the values
    ## @param fn function name
    ## @param args an array of argument
    ## @return the result array (includes 2extra leading entries)
    def tsApplyA(self, ag, fn, args):
        testIndex(ag)
        #try:
        return self.verifyLink().sendOp3n(OP_CALL, 1, -1, AG_APPLY, ag.tsx, fn, args)
        #except IOException, e:
        #    raise AllegroGraphException(e)        

    def applyA(self, fn, args):
        try:
            return self.verifyLink().sendOp1n(OP_CALL, 1, -1, fn, args)
        except Exception, e:
            raise AllegroGraphException(e)

    def intValue(self, r):
        if isinstance(r, long):
            return int(r)
        if isinstance(r, int):
            return r
        raise IllegalArgumentException("Cannot convert to int " + r)

    def access(self, createCode, name, dir, more=[]):
        args = [None for i in range(3 + len(more))]
        args[0] = createCode
        args[1] = name
        args[2] = dir
        if 0 < len(more) and not self.serverLevel(1):
            raise IllegalStateException("Server level < 1")
        ## for-while
        i = 0
        while i < len(more):
            args[3 + i] = more[i]
            i += 1
        try:
            r = self.verifyLink().sendOp1n(OP_CALL, 1, 0, AG_ACCESS_TRIPLE_STORE, args)
        except (IOException, ), e:
            m = e.getMessage()
            p = m.indexOf("AGErr")
            if p < 0:
                raise AllegroGraphException(e)
            else:
                raise AllegroGraphException(m.substring(p))
        return int(longValue(r))

    def addTriple(self, ag, s, p, o, c):
        r = self.tsApplyA(ag, AG_ADD_TRIPLE, [s, p, o, c, "with-parts", 1, "sync", ('' if ag.sync else None)])
        v = [None] * 5
        v[0] = longValue(r[2])
        ## for-while
        i = 1
        while i < 5:
            v[i] = r[i + 2]
            i += 1
        return v

    def addTriples(self, ag, s, p, o, c):
        v = self.tsApplyA(ag, AG_ADD_TRIPLES, [s, p, o, c, "with-parts", 1, "sync", ('' if ag.sync else None)])
        ids = longArray(v[2])
        rr = [ids, None, None, None, None]
        rr[1] = v[3]
        rr[2] = v[4]
        rr[3] = v[5]
        rr[4] = v[6]
        return rr

    def closeTripleStore(self, ag, doClose):
        return (1 == longValue(self.tsApply0(ag, AG_CLOSE, [('' if doClose else None)])))

    def delete(self, ag, s, p, o, c, wildOk):
        self.tsApply0(ag, AG_DELETE, [s, p, o, c, (1 if wildOk else 0)])

    def disable(self):
        try:
            self.verifyLink().disconnect()
            self.trs = None
        except (IOException, ), e:
            pass

    def discardCursor(self, ag, ref):
        self.tsApply0(ag, AG_DISCARD_CURSOR, [ref])

    def discardCursors(self, ag, refs):
        if 16000 < len(refs):
            raise AllegroGraphException("Too many arguments in call.")
        self.tsApply0(ag, AG_DISCARD_CURSOR, refs)

    def enable_socket_connection(self):
        if self.trs is not None:
            return
        AGDirectLink.debug(self.debug)
        if self.debug > 0:
            self.trs = AGDirectLinkDebug(self.host, self.port, self.pollCount, self.pollInterval, self.timeout)
        else:
            self.trs = AGDirectLink(self.host, self.port, self.pollCount, self.pollInterval, self.timeout)

    def exists(self, name, directory):
        try:
            r = self.verifyLink().sendOp1n(OP_CALL, 1, 0, AG_EXISTS_P, [name, directory])
        except (IOException, ), e:
            raise AllegroGraphException(e)
        if (1 == longValue(r)):
            return True
        return False

    def getLangPart(self, ag, id):
        v = self.tsApplyA(ag, AG_GET_NODE_PARTS, [id])
        if int(longValue(v[2])) == 4:
            if 4 < len(v):
                return v[4]
        return
    
    def getParts(self, ag, idz, types=None, vals=None, mods=None):
        """
        If 'idz' is a UPI, execute a 'get_node_parts' action and
        return some parts. 
        Otherwise, execute the action and copy the parts into the
        last three arguments (arrays)
        """
        if isinstance(idz, UPI):
            v = self.tsApplyA(ag, AG_GET_NODE_PARTS, [idz])
            return [int(longValue(v[2])), str(v[3]), str(v[4])]
        else: 
            v = self.tsApplyA(ag, AG_GET_NODE_PARTS, [idz])
            copy(self.intArray(v[2]), types)
            copy(self.stringArray(v[3]), vals)
            copy(self.stringArray(v[4]), mods)

    def getTextPart(self, ag, id):
        v = self.tsApplyA(ag, AG_GET_NODE_PARTS, [id])
        v2 = longValue(v[2]) 
        if v2 in [2, 3, 4, 5, 8]:
            if 3 < len(v):
                return str(v[3])
        return None

    def getTriples(self, ag, s, p, o, c, subend=None, predend=None, obend=None, cxend=None, lh=None):
        if lh:
            return self.getTriplesInRange(ag, s, p, o, c, subend, predend, obend, cxend, lh);
        ## else do regular 'getTriples':
        ## fix argument overloading:
            lh = subend
            subend = None
        if lh < 1:
            lh = Cursor.defaultLookAhead
        v = self.tsApplyA(ag, AG_GET_TRIPLES, [s, p, o, c, lh, 1])
        if 4 > len(v):
            return Cursor.emptyCursor
        if (4 == len(v)):
            return Cursor(ag, v[2], toUPIArray(v[3]))
        else:
            return Cursor(ag, v[2], toUPIArray(v[3]), v[4])


    def getTriplesInRange(self, ag, s, p, o, c, subend, predend, obend, cxend, lh):
        if lh < 1:
            lh = Cursor.defaultLookAhead
        v = self.tsApplyA(ag, AG_GET_TRIPLE_RANGE, [s, p, o, c, subend, predend, obend, cxend, lh, 1])
        if len(v) < 4:
            return Cursor.emptyCursor
        if (4 == len(v)):
            return Cursor(ag, v[2], toUPIArray(v[3]))
        return Cursor(ag, v[2], toUPIArray(v[3]), v[4])

    def getInfTriples(self, ag, s, p, o, c, subend=None, predend=None, obend=None, cxend=None, lh=None, infer=None):
        if lh:
            return self.getInfTriplesInRange(self, ag, s, p, o, c, subend, predend, obend, cxend, lh, infer);
        ## else do regular 'getInfTriples':
        if lh < 1:
            lh = Cursor.defaultLookAhead
        v = self.tsApplyA(ag, AG_INFER_TRIPLES, [s, p, o, c, lh, "infer", (1 if infer else 0)])
        if len(v) < 4:
            return Cursor.emptyCursor
        return Cursor(ag, v[3], toUPIArray(v[2]))
            
    def getInfTriplesInRange(self, ag, s, p, o, c, subend, predend, obend, cxend, lh, infer):
        if lh < 1:
            lh = Cursor.defaultLookAhead
        v = self.tsApplyA(ag, AG_INFER_TRIPLE_RANGE, [s, p, o, c, subend, predend, obend, cxend, lh, "infer", (1 if infer else 0)])
        if 4 > len(v):
            return Cursor.emptyCursor
        else:
            return Cursor(ag, v[3], toUPIArray(v[2]))

    def getTypePart(self, ag, id):
        v = self.tsApplyA(ag, AG_GET_NODE_PARTS, [id])
        if longValue(v[2]) == 5:
            if 4 < len(v):
                return str(v[4])
        return

    def hasTriple(self, ag, s,  p, o, c):
        v = self.tsApplyA(ag, AG_GET_TRIPLES, [s, p, o, c, -1])
        if 3 > len(v): return False
        else: return True

    def hasInfTriple(self, ag, s,  p, o, c, infer):
        v = self.tsApplyA(ag, AG_INFER_TRIPLES, [s, p, o, c, -1, "infer", (1 if infer else 0)])
        if 3 > len(v): return False
        else: return True

    def indexAll(self, ag, wait):
        self.tsApply0(ag, AG_ALL, [1 if wait else 0])

    def indexTriples(self, ag, wait):
        self.tsApply0(ag, AG_INDEX, [1 if wait else 0])

    def loadNTriples(self, ag, name, context, source, place, save, ext):
        gt = None
        if (None != save):
            if save.booleanValue():
                gt = 8
            else:
                gt = 4
        lng = 4
        if (None != ext):
            lng = lng + 2
        args = [None for i in range(lng)]
        args[0] = name
        args[1] = context
        args[2] = source
        args[3] = gt
        j = 4
        if (None != ext):
            args[j] = "external-format"
            j += 1
            args[j] = ext
            j += 1
        r = self.tsApplyA(ag, AG_LOAD, args)
        if r is None:
            return 0
        if len(r) < 4:
            return longValue(r[2])
        if place is not None and 0 < len(place):
            place[0] = r[3]
        return longValue(r[2])

    def loadRDF(self, ag, filePath, context, baseURI, useRapper):
        gt = 1 if useRapper else None
        if not baseURI:
            args = [filePath, context, gt]
        else:
            args = [filePath, context, gt, "base-uri", baseURI]
        r = self.tsApplyA(ag, AG_RDF, args)
        if r is None:
            return 0
        elif len(r) > 2:
            return longValue(r[2])
        else: return -1

    def newBlankNode(self, ag, name=None):
        names = [name] if name else []
        v = self.tsApply0(ag, AG_NEW_NODE, names)
        return toUPI(v)

    def newBlankNodes(self, ag, n):
        v = self.tsApply0(ag, AG_NEW_NODE, [n])
        return toUPIArray(v)

    def newLiteral(self, ag, text, type, lang):
        v = self.tsApply0(ag, AG_INTERN_LIT, [text, type, lang])
        return toUPI(v)

    def newResource(self, ag, uri):
        v = self.tsApply0(ag, AG_INTERN_RES, [uri])
        return toUPI(v)

    def newResources(self, ag, uri):
        v = self.tsApply0(ag, AG_INTERN_RES, [uri])
        return toUPIArray(v)

    def nextCursor(self, ag, source, lh):
        v = self.tsApplyA(ag, AG_NEXT, [source, lh])
        if 3 > len(v): return None
        else: return toUPIArray(v[2])

    def nextCursorAndParts(self, ag, source, lh):
        v = self.tsApplyA(ag, AG_NEXT_WITH_PARTS, [source, lh])
        if 4 > len(v): return None
        else: return [v[2], v[3]]

    def numberOfTriples(self, ag):
        v = self.tsApply0(ag, AG_NUMBER, [])
        return longValue(v)

    def query(self):
        if self.trs is None:
            return -1
        if (None == self.trs.softLock):
            return 0
        return 1

    def syncTripleStore(self, ag):
        self.tsApplyA(ag, AG_SYNC, [])

    @staticmethod
    def stringArray(x):
        if x is None: return []
        elif isinstance(x, str): return [x]
        sample = x[0]
        if isinstance(sample, str): return x
        elif isinstance(x, list):
            y = x  ## cast in original Java
            r = [None for i in range(len(x))]
            for i in range(0, len(y)):
                e = y[i]
                if e is None:
                    r[i] = None
                else:
                    if isinstance(e, str):
                        r[i] = e
                    else:
                        r[i] = "Coerced " + str(e) + " to String"
            return r        
        else: return ["Coerced " + str(e) + " to String"]

    @staticmethod
    def intArray(x):
        return [int(i) for i in x]

    def selectTriples(self, ag, query, presetz, pvar, limit, infer, distinct):
        v = self.tsApplyA(ag, AG_SELECT_TRIPLES, [query, limit, "presets", presetz, "pvars", pvar,
                            "use-reasoner", infer, "distinct", distinct])
        return Cursor(ag, v[3], toUPIArray(v[2]))

    def selectValues(self, ag, query, presets, pvars, infer, distinct):
        a = [None for x in range(0, 8)]
        a[0] = pvars
        if presets is None:
            a[1] = []
        else:
            if (0 == len(presets)):
                a[1] = []
            else:
                if isinstance(presets[0], long):
                    iv = [int() for i in range(len(presets))]
                    ## for-while
                    i = 0
                    while i < len(iv):
                        iv[i] = presets[i]
                        i += 1
                    a[1] = iv
                else:
                    if isinstance(presets[0], str):
                        iv = [None for i in range(len(presets))]
                        ## for-while
                        i = 0
                        while i < len(iv):
                            inval = presets[i]
                            iv[i] = ag.refNtripleString(inval)
                            i += 1
                        a[1] = iv
                    else:
                        if isinstance(presets[0], Value):
                            iv = [UPI() for __idx0 in range(len(presets))]
                            ## for-while
                            i = 0
                            while i < len(iv):
                                inval = presets[i]
                                iv[i] = inval.getAGId()
                                i += 1
                            a[1] = iv
                        else:
                            if isinstance(presets[0], Triple):
                                iv = [int() for i in range(len(presets))]
                                ## for-while
                                i = 0
                                while i < len(iv):
                                    inval = presets[i]
                                    iv[i] = inval.getAGId()
                                    i += 1
                                a[1] = iv
                            else:
                                raise IllegalArgumentException("presets object contains unsuitable type")
        a[2] = self.query
        a[3] = ag.selectLimit
        a[4] = "use-reasoner"
        a[5] = infer
        a[6] = "distinct"
        a[7] = distinct
        r = self.tsApplyA(ag, AG_SELECT_VALUES, a)
        return self.valuesResults(r)

    def nextValuesArray(self, ag, source, lh):
        r = self.tsApplyA(ag, AG_NEXT, [source, lh])
        return self.valuesResults(r)

    def valuesResults(self, r):
        if 10 > len(r):
            return
        ex = None
        if 10 < len(r):
            ex = r[10]
        ids = toUPIArray(r[2])
        types = self.intArray(r[3])
        labels = self.stringArray(r[4])
        mods = self.stringArray(r[5])
        more = int(longValue(r[6]))
        width = int(longValue(r[7]))
        token = r[8]
        plimit = int(longValue(r[9]))
        return [ids, types, labels, mods, more, width, token, plimit, ex]

    def traceServer_withoutAG(self, onoff, outFile=None):
        arg = outFile if outFile is not None else (1 if onoff else 0)
        try:
            self.verifyLink().sendOp1n(OP_CALL, 1, -1, AGJ_TRACE_INT, [arg])
        except (IOException, ), e:
            raise AllegroGraphException(e)

    def traceServer(self, ag, onoff, outFile=None):
        if not isinstance(ag, AllegroGraph):
            return self.traceServer_withoutAG(ag, onoff)
        ## do trace with 'ag':
        arg = outFile if outFile is not None else 1 if onoff else 0
        self.tsApply0(ag, AGJ_TRACE_INT_A, [arg])
        
    def serverId(self):
        try:
            r = self.verifyLink().sendOp1n(OP_CALL, 1, 0, AGJ_TRACE_INT, [-2])
            return longValue(r)
        except (IOException, ), e:
            raise AllegroGraphException(e)

    def interruptServer(self, id):
        if id < 101:
            return -4
        try:
            r = self.verifyLink().sendOp1n(OP_CALL, 1, 0, AGJ_TRACE_INT, [long(id)])
            return AGConnector.toInt(r)
        except (IOException, ), e:
            raise AllegroGraphException(e)

    def serverLevel(self, level):
        if self.currentServerLevel < 0:
            try:
                r = self.verifyLink().sendOp1n(OP_CALL, 1, 0, AGJ_TRACE_INT, [100])
                currentServerLevel = int(longValue(r))
            except (IOException, ), e:
                raise AllegroGraphException(e)
        return level <= currentServerLevel

    def valuesOnly(self, r):
        s = [None for i in range(len(r) - 2)]
        for i in range(len(s)):
            s[i] = r[i + 2]
        return s
    
    def evalInServer(self, ag, expression=None):
        if not isinstance(ag, AllegroGraph):
            return self.evalInServer_2(expression) ## shift variables one left
        else:
            r = self.tsApplyA(ag, AGJ_EVAL_A, [expression, -1])
            return self.valuesOnly(r)

    def evalInServer_2(self, expression):
        v = self.verifyLink().sendOp1n(OP_CALL, 1, -1, AGJ_EVAL, [expression])
        return self.valuesOnly(v)

    def getTripleParts(self, ag, id):
        r = self.tsApplyA(ag, AG_GET_TRIPLE_PARTS, [long(id)])
        if 6 > len(r):
            return
        return UPI()

    def twinqlAsk(self, ag, query, infer, more=None):
        ml = 0 if (None == more) else len(more)
        args = [None for i in range(3 + ml)]
        args[0] = self.query
        args[1] = "use-reasoner"
        args[2] = infer
        for i in range(ml):
            args[3 + i] = more[i]
            i += 1
        r = self.tsApply0(ag, AG_TWINQL_ASK, [query])
        if (1 == longValue(r)):
            return True
        return False

    def twinqlSelect(self, ag, query, vars, limit, offset, slimit, infer, more=None):
        args = [query, vars, limit, offset, "slimit", slimit, "use-reasoner", infer]
        if more:
            args.extend(more)
#        ml = len(more) if more else 0
#        args = [None] * (8 + ml)
#        args[0] = query
#        args[1] = vars
#        args[2] = limit
#        args[3] = offset
#        args[4] = "slimit"
#        args[5] = slimit
#        args[6] = "use-reasoner"
#        args[7] = infer
#        for i in range(0, ml):
#            args[8 + i] = more[i]
        #print "Begin twinql select"
        #beginTime = datetime.datetime.now()  
        r = self.tsApplyA(ag, AG_TWINQL_SELECT, args)
        #print "\n   Return from twinql select; elapsed time ", (datetime.datetime.now() - beginTime)
        if (slimit == -1):
            return self.valuesOnly(r)
        return self.valuesResults(r)

    def twinqlFind(self, ag, query, limit, offset, slimit, infer=False, more=None):
        ml = 0 if not more else len(more)
        args = [None for i in range(7 + ml)]
        args[0] = query
        args[1] = limit
        args[2] = offset
        args[3] = "slimit"
        args[4] = slimit
        args[5] = "use-reasoner"
        args[6] = infer
        for i in range(0, ml):
            args[7 + i] = more[i]
        v = self.tsApplyA(ag, AG_TWINQL_FIND, args)
        token = None
        if 3 < len(v):
            token = v[3]
        return Cursor(ag, token, toUPIArray(v[2]))

    def twinqlQuery(self, ag, query, format, limit, offset, infer, more=None):
        ml = 0 if (None == more) else len(more)
        args = [None for i in range(6 + ml)]
        args[0] = self.query
        args[1] = format
        args[2] = limit
        args[3] = offset
        args[4] = "use-reasoner"
        args[5] = infer
        for i in range(0, ml):
            args[6 + i] = more[i]
        r = self.tsApply0(ag, AG_TWINQL_QUERY, args)
        return r

    def indexing(self, ag, mode, value, flavors):
        return self.tsApply0(ag, AG_INDEXING, [mode, value, flavors])

    def mapping(self, ag, mode, map):
        return self.tsApply0(ag, AG_MAPPING, [mode, map])

    def namespaces(self, ag, map):
        r = self.tsApply0(ag, AGJ_NAMESPACES_A, [map])
        rs = self.stringArray(r)
        return rs

    def addPart(self, ag, part):
        if self.serverLevel(2):
            v = self.tsApplyA(ag, AG_ADD_PART, [part])
            return self.valuesOnly(v)
        raise IllegalStateException("Server does not support addPart()")

    def freetextPredicates(self, ag, defs):
        v = self.tsApply0(ag, AG_FREETEXT_PREDICATES, [defs])
        if isinstance(v, str): return v
        else: return []

    def getFreetextStatements(self, ag, pattern, lh):
        if (lh == 0):
            lh = Cursor.defaultLookAhead
        v = self.tsApplyA(ag, AG_FREETEXT_STATEMENTS, [pattern, lh, 1])
        if lh < 0:
            if (2 == len(v)):
                return
            return Cursor.emptyCursor
        if 4 > len(v):
            return Cursor.emptyCursor
        if (4 == len(v)):
            return Cursor(ag, v[2], toUPIArray(v[3]))
        return Cursor(ag, v[2], toUPIArray(v[3]), v[4])

    def getFreetextSubjects(self, ag, pattern, limit):
        r = self.tsApplyA(ag, AG_FREETEXT_SUBJECTS, [pattern, limit])
        return self.valuesResults(r)

    def federate(self, name, parts, supersede):
        try:
            r = self.verifyLink().sendOp1n(OP_CALL, 1, 0, AG_FEDERATE, [name, parts, "if-exists", ("supersede" if supersede else None)])
        except (IOException, ), e:
            raise AllegroGraphException(e)
        return int(longValue(r))

    def findStore(self, name, directory, delete):
        try:
            r = self.verifyLink().sendOp1n(OP_CALL, 1, -1, AG_FIND_STORE, [name, directory, None, ("delete" if delete else "ignore")])
        except (IOException, ), e:
            raise AllegroGraphException(e)
        if 5 > len(r): return None
        return [int(longValue(r)), r[3], r[4]]

    def getStores(self, ag):
        r = self.tsApplyA(ag, AG_GET_STORES, [])
        if (3 == len(r)):
            return
        ra = [None for i in range(len(r) - 2)]
        for i in range(len(ra)):
            if (0 == i % 3):
                ra[i] = int(longValue(r[i + 2]))
            else:
                ra[i] = r[i + 2]
        return ra

    def serializeTriples(self, ag, args):
        r = self.tsApplyA(ag, AG_SERIALIZE_TRIPLES, args)
        if 2 < len(r): return r[2]
        return None

    def applyAGFn(self, ag, fn, args):
        v = self.tsApplyA(ag, fn, args)
        if (None == v): return []
        if 3 > len(v):  return []
        w = [None for i in range(len(v) - 2)]
        for i in range(len(w)):
            w[i] = v[i + 2]
        return w

    def applyFn(self, fn, args):
        v = self.applyA(fn, args)
        if v is None: return 0
        if 3 > len(v): return 0
        w = len(v) - 2
        for i in range(len(w)):
            w[i] = v[i+2]
        return w    
    
    def serverOption(self, name, val):
        v = self.applyA(AGJ_SERVER_OPTIONS, [name, val])
        if v is None: return None
        if 3 > len(v): return None
        return v[2]

