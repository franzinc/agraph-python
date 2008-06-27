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
import traceback

from time import sleep
from franz.exceptions import IllegalArgumentException, IllegalStateException, AllegroGraphException
from franz.rdfconstants import XSD
from franz.transport.agc import *
from franz.namespaceregistry import NamespaceRegistry
from franz.transport.agdirectconnector import AGDirectLink
from franz.allegrograph.upi import UPI
from franz.blanknode import BlankNode
from franz.literal import Literal
from franz.encodedliteral import EncodedLiteral
from franz.triple import Triple
from franz.openrdf.model.value import *
from franz.node import Node
from franz.valuenode import ValueNode
from franz.resourcenode import ResourceNode
from franz.defaultgraph import DefaultGraph
from franz.transport import agconnector
from franz.namedattributelist import NamedAttributeList


class valueMapEntry(object):
    """ 
    xxx
    """
    def __init__(self, ag, token, more, pl, sv):
        self.savedToken = token
        self.savedMore = more
        self.savedAG = ag
        self.savedPlimit = pl
        self.savedVal = sv

    def finalize(self):
        with self.finalizeLock:
            if self.savedAG.agConnection is None:
                return
            if (None != self.savedToken):
                self.savedAG.agConnection.oldTokens.append(self.savedToken)

class V3_0_0(object):
    """ 
     * The name of this class identifies the version of the AllegroGraph
     * Java implementation.
     * This name is also visible in the list of members in a jar file
     * when it is inspected with Emacs or WinZip.
    """
    version = "3.0.0"

# * Each instance of this class implements access to one AllegroGraph triple newStore.
# * <p>
# * There is no public constructor.  Instances of this class are
# * created by calls to methods in the AllegroGraphConnection class:
# *  <ul>
# *     <li>open() to open an existing triple newStore
# *     <li>create() to create a new triple newStore when
# *        one did not exist before
# *     <li>access() to open or create a databas
# *     <li>renew() to open a new triple newStore or
# *         replace an existing one with a new empty triple newStore
# *     <li>replace() to replace an existing triple newStore
# *        with a new empty one
# *     <li>federate() to create a federated newStore composed of several
# *        open triple stores.
# *  </ul>
# * <h4>Triple Component Specification</h4>
# * The components of triples in update and search methods are usually 
# * declared as Object.  In that case the argument may be:
# *   <ul>
# *     <li>A string in NTriples notation or a !-notation.
# *     <li>An instance of a class that implements the ValueNode interface.
# *     <li>A UPI instance.
# *   </ul>
# *   When a method argument may be an array, it must be an array of one
# *   of the above types.
# *  
# * <h4>The Context or Graph Component</h4>
# * AllegroGraph is a triple newStore implementations that allows triples to be
# * identified by an additional component that may be 
# * a literal, a URI or a blank node.
# * This component is sometimes called a context and sometimes a graph
# * specifier.  If this component is not specifed, triples are added to the
# * null context, or the default graph. 
# * The terms <i>null context</i> and <i>default graph</i> are used interchangeably
# * in the method descriptions.
# * <p>
# * In search operations, if the context is not specifed, then the search
# * examines only the null context.  If the context is specifed,
# * then a null value is wild and matches any context, the empty string
# * denotes the null context, any other value must be a valid Value reference. 
# * <p>
# * The components of triples may be specfied as strings in ntriple notation,
# * Value instances, and UPI instances.
# * In some cases null and the empty string may be used to denote wild
# * card or the null context. 
# * 
# * @author mm@Franz.com

class AllegroGraph(object):
    """ 
    """
    version = "3.0.0"
    accessOptionsDefinitions = {"expected-unique-resouces": long,
        "with-indices": str,
        "include-standard-parts": bool,
        "read-only-p": bool,
        "indirect-host": str,
        "indirect-port": int,
        }
    ## When this is not null, the prdb method is active:
    dbf = None ## "t:/tmp/agdb.out";
    dbg = None
    prdbLock = threading.RLock()                                        
    
    def initialize_local_fields(self):
        self.versionLabels = ""
        self.agConnection = None
        self.defaultLookAhead = 0
        self.ags = None
        self.tsx = -1
        self.storeName = None
        self.storeDirectory = None
        self.nsregs = None
        self.dgraph = None
        self.dgraphs = None
        self.sync = True
        self.sna = None
        self.geo = None
        self.accessOptions = NamedAttributeList(AllegroGraph.accessOptionsDefinitions)
        self.selectLimit = 1000
        ## method synchronization locks:
        self.finalizeLock = threading.RLock()
        self.isDefaultGraphLock = threading.RLock()
        self.getDefaultGraphLock = threading.RLock()
        self.closeTripleStoreLock = threading.RLock()                                                
    
    def __init__(self, conn, access, name, directory=None):
        """
        The initialization here is a complete mess.  There are 3-4 different combinations of arguments
        that do 3-4 different kinds of initialization, all mixed into one constructor.  The correct
        approach would be to define multiple constructors with different names, with each name describing
        what kind of creation is happening.
        """
        self.initialize_local_fields()
        self.agConnection = conn
        self.storeName = name
        self.storeDirectory = directory
        if access:
            self.connect(access)
            
    @staticmethod            
    def findNamedTripleStore(conn, name, directory):
        newStore = AllegroGraph(conn, None, name, directory)
        newStore.storeDirectory = None
        remoteAGStore = newStore.verifyEnabled().findStore(name, directory, None)
        if not remoteAGStore:
            raise AllegroGraphException("Could not find triple newStore.")
        newStore.tsx = remoteAGStore[0].intValue()
        newStore.storeName = remoteAGStore[1]
        newStore.storeDirectory = remoteAGStore[2]
        newStore.registerLocalTripleStore()
    
    def registerLocalTripleStore(self):
        self.agConnection.addTS(self)

    def connect(self, key, conn=None):
        if conn:
            if self.agConnection is not None:
                raise IllegalStateException("Already connected")
            self.agConnection = conn
            self.connect(key)
        else:
            if self.tsx > -1:
                raise IllegalStateException("Already connected")
            if self.tsx < -1:
                raise IllegalStateException("Closed triple newStore")
            self.tsx = self.verifyEnabled().access(key, self.storeName, self.storeDirectory, self.accessOptions.getList())
            self.registerLocalTripleStore()
            self.initNamespaces()

    def setAttribute(self, name, value):
        """
         * Set attributes that affect the creation of new triple stores 
         * or the accessing of existing ones.  
         * These attributes must be set before one of the connection 
         * methods is called since they only take effect during the creation 
         * or opening of a triple newStore.
         * @param name The name of the attribute.
         * @param value The value of the attribute.
         * 
         * Attribute names are listed below as well as the required datatype
         * of the value:
         * 
         * <ul>
         *     <li><code>"expected-unique-resouces"</code>  -- Long --  The initial
         *           size of the resource table in a new triple newStore.
         *     <li><code>"with-indices"</code>  -- String[] -- The desired indices
         *           for a new triple newStore.
         *     <li><code>"include-standard-parts"</code>  -- Boolean -- When true,
         *           create some commonly used RDF and Owl resources in 
         *           a new triple newStore.
         *     <li><code>"read-only-p"</code>  -- Boolean --  When true, open an
         *           existing triple newStore in read-only mode.
         *     <li><code>"indirect-host"</code>  -- String --  A hostname where the server
         *           will find a remote triple newStore.  When an inirect-host is specified, the name
         *           and directory arguments are relative to the location of the remote host.
         *     <li><code>"indirect-port"</code>  -- Integer --  The port number where the
         *           indirect-host is listening.
         * </ul>
        """
        if self.tsx > -1:
            raise IllegalStateException("Already connected")
        if self.tsx < -1:
            raise IllegalStateException("Closed triple newStore")
        self.accessOptions.setAttribute(name, value)

    @staticmethod
    def version():
        return AllegroGraph.version

    @staticmethod
    def versions():
        """
        Query the current AllegroGraph and component versions.
         * @return An array of strings for the AllegroGraph version, 
         *    the server protocol level, and the socket protocol level.
        """
        return [AllegroGraph.version, str(AGU_PROTOCOL_LEVEL), AG_DIRECT_LEVEL,]

    @staticmethod
    def versionLabels():
        return ["AllegroGraph Version ", "      protocol level ", "   direct link level "]
    
    def getConnection(self):
        return self.agConnection

    def verifyEnabled(self):
        if self.agConnection is None:
            raise IllegalStateException("AllegroGraph server is not set.")
        return self.agConnection.getServer()

    def setLookAhead(self, n):
        """
        * Set the look-ahead value for subsequent Statement search operations.
         * @param n an integer look-ahead value - must be positive.
         *    A zero value specifies that the defaultLookAhead value in the 
         *    Cursor class should be used.
         * <p>
         * The look-ahead value determines how many Statements will be
         * pre-loaded into a Cursor instance when it is created or advanced.
         * The pre-loaded Statements in the Cursor are retrieved 
         * immediately in Java without a round-trip to the 
         * AllegroGraph server. 
        """
        if n > -1:
            self.defaultLookAhead = n
        else:
            raise IllegalArgumentException("setLookAhead cannot be negative " + n)

    def getLookAhead(self):
        return self.defaultLookAhead

    def nsregsInit(self):
        if self.nsregs is None:
            self.nsregs = NamespaceRegistry()
        return self.nsregs

    def isDefaultGraph(self, x):
        with self.isDefaultGraphLock:
            if isinstance(x, Value):
                return self.isDefaultGraph(x.nodeUPI)
            elif isinstance(x, int):
                return (type == AGU_DEFAULT_GRAPH)                
            elif isinstance(x, UPI):
                temp = DefaultGraph()
                if (None == self.dgraphs):
                    if (None == self.dgraph):
                        return False
                    if id == self.dgraph.nodeUPI:
                        return True
                    return False
                ## for-while
                i = 0
                while i < len(self.dgraphs):
                    temp = self.dgraphs[i]
                    if id == temp.nodeUPI:
                        return True
                    i += 1
                return False
            else: return False


    def getDefaultGraph(self, id):
        with self.getDefaultGraphLock:
            if id is None:
                return None
            temp = DefaultGraph()
            if (None == self.dgraphs):
                if (None == self.dgraph):
                    self.dgraph = DefaultGraph(self, id)
                    return self.dgraph
                if id == self.dgraph.nodeUPI:
                    return self.dgraph
                self.dgraphs = []
                self.dgraphs.append(self.dgraph)
                self.dgraph = None
                temp = DefaultGraph(self, id)
                self.dgraphs.append(temp)
                return temp
            ## for-while
            i = 0
            while i < len(self.dgraphs):
                temp = self.dgraphs[i]
                if id == temp.nodeUPI:
                    return temp
                i += 1
            temp = DefaultGraph(self, id)
            self.dgraphs.append(temp)
            return temp

    def initNamespaces(self):
        if self.nsregs is None:
            self.nsregs = self.agConnection.nsregs
        if self.nsregs is not None:
            self.verifyEnabled().namespaces(self, self.nsregs.stringArray())

    @staticmethod
    def stringArray(in_array):
        out = ['' for i in range(len(in_array))]
        for i in range(len(out)):
            out[i] = in_array[i]
        return out

    def syncDatabase(self):
        self.syncTripleStore()

    def syncTripleStore(self):
        self.verifyEnabled().syncTripleStore(self)

    def closeDatabase(self):
        self.closeTripleStore()

    def ntripleContext(self, c):
        if not c: return None
        elif isinstance(c, str) and "source".lower() == c.lower(): return "source"
        else: return self.anyContextRef(c, 1)

    def loadNTriples(self, names, context, save, ext, place):
        if context:  print "NAMES: " + names + "  CONTEXT: " + context + "   EXT " + ext
        return self.verifyEnabled().loadNTriples(self, names, self.ntripleContext(context), None, place, save, ext)

    def parseNTriples(self, caller, context=None, save=None, place=None):
        return self.verifyEnabled().loadNTriples(self, None, self.ntripleContext(context), caller, place, save, None)

    def loadRDF(self, filePath, context=None, baseURI=None):
        return self.verifyEnabled().loadRDF(self, filePath, self.ntripleContext(context), baseURI, False)   

    def numberOfTriples(self):
        """
         * Query the number of triples in the triple newStore.
         * 
         * @return the number of triples in the triple newStore.
        """
        return self.verifyEnabled().numberOfTriples(self)

    def indexTriples(self):
        self.indexNewTriples()

    def indexNewTriples(self, wait=True):
        """
         * Index new triples.
         * 
         * @throws AllegroGraphException if an error occurs during indexing.
         * <p>
         * If the current indexing chunk size is too small, it can cause
         * too many files to be opened during indexing; in that case the 
         * AllegroGraphException exception with the string
         * 
         *    "too-many-index-chunks-error:  will create nnn files"
         *    
         * is thrown; nnn is the number of chunks.  At this point,
         * the triple newStore has not been indexed, but the server is in
         * a stable state.  The Application can adjust the chunk size
         * with AllegroGraphConnection.setChunkSize() and try the indexing call again.
         * <p>
         * If the current indexing chunk size is too large, the server may run out
         * of memory during indexing; in that case, the thrown exception will
         * depend on where and when the out-of-memory condition occurred.
         * The triple newStore may be in a partially indexed state.
        """
        try:
            self.verifyEnabled().indexTriples(self, wait)
        except (IllegalArgumentException, ), e:
            self.throwIndexError(e)
        raise e

    def throwIndexError(self, e):
        m = str(e)
        if (-1 == m.indexOf("too-many-index-chunks-error ")):
            return
        w = " will create "
        ch = m.indexOf(w)
        if (ch == -1):
            return
        ch2 = m.indexOf(" ", ch + len(w))
        if (ch2 == -1):
            return
        raise AllegroGraphException("too-many-index-chunks-error: " + m.substring(ch, ch2) + " files")

    def indexAll(self):
        """
         * Index all the triples in the triple newStore.
         * @throws AllegroGraphException if an error occurs during indexing.
         * <p>
         * See the discussion of chunk size at indexTriples().
         * @deprecated Use {@link #indexAllTriples()} instead
        """
        self.indexAllTriples()

    def stringElt(self, a, i):
        if a is None:
            return
        if i < len(a):
            return a[i]
        return a[len(a) - 1]

    def typeToString(self, type):
        if type == 1:
            return "anon"
        elif type == 2:
            return "node"
        elif type == 3:
            return "literal"
        elif type == 4:
            return "literal/lang"
        elif type == 5:
            return "typed-literal"
        elif type == 6:
            return "triple"
        elif type == 7:
            return "default-graph"
        elif type == 8:
            return "encoded-string"
        elif type == 9:
            return "encoded-integer"
        elif type == 10:
            return "encoded-float"
        elif type == 11:
            return "encoded-triple-id"
        else:
            return "unknown"

    def getText(self, id, lit):
        if lit is not None:
            return lit
        if not UPI.canReference(id):
            raise IllegalStateException("getText " + id)
        try:
            return self.getTextPart(id)
        except (AllegroGraphException, ), e:
            raise IllegalStateException("getText " + e)

    def getTextEx(self, id, lit):
        if lit is not None:
            return lit
        if not UPI.canReference(id):
            raise IllegalStateException("getTextEx " + id)
        return self.getTextPart(id)

    def getTextPart(self, id):
        if not UPI.canReference(id):
            raise IllegalStateException("getTextPart " + id)
        return self.verifyEnabled().getTextPart(self, id)

    def getTypePart(self, id):
        if not UPI.canReference(id):
            raise IllegalStateException("getTypePart " + id)
        return self.verifyEnabled().getTypePart(self, id)

    def getLangPart(self, id):
        if not UPI.canReference(id):
            raise IllegalStateException("getLangPart " + id)
        return self.verifyEnabled().getLangPart(self, id)

    def newValue(self, id, type=None, val=None, mod=None):
        if type is None: return self.newValue_with_id(id)
        if type == AGU_ANON:
            return BlankNode(self, id, val)
        elif type == AGU_NODE:
            return Node(self, id, val)
        elif type == AGU_LITERAL:
            return Literal(self, id, val, None, None, Literal.LANG_NOT_KNOWN, None)
        elif type == AGU_LITERAL_LANG:
            return Literal(self, id, val, None, None, Literal.LANG_IS_KNOWN, mod)
        elif type == AGU_TYPED_LITERAL:
            return Literal(self, id, val, None, mod, Literal.LANG_NOT_KNOWN, None)
        elif type == AGU_TRIPLE:
            idn = id.getCode()
            if idn > -1:
                return Triple(self, idn, None, None, None)
        elif type == AGU_DEFAULT_GRAPH:
            return self.getDefaultGraph(id)
        elif type == AGU_ENCODED_STRING:
            newInstance = EncodedLiteral(self, val, mod)
            newInstance.nodeUPI = id
            return newInstance
        elif type == AGU_ENCODED_INTEGER:
            newInstance = EncodedLiteral(self, long(val), mod)
            newInstance.nodeUPI = id
            return newInstance
        elif type == AGU_ENCODED_FLOAT:
            newInstance = EncodedLiteral(self, float(val), mod)
            newInstance.nodeUPI = id
            return newInstance
        else:
            raise IllegalArgumentException("Unknown node type " + type + "  " + val + "  " + mod)

    def newValue_with_id(self, id):
        v = None
        if not UPI.canReference(id):
            raise IllegalStateException("AllegroGraph Id cannot be registered:" + id)
        try:
            r = self.verifyEnabled().getParts(self, id)
            v = self.newValue(id, r[0].intValue(), r[1], r[2])
        except (AllegroGraphException, ), e:
            self.failCreate("ValueObject", e)
        if v is None:
            self.failCreate("ValueObject", None)
        return v

    def validID(self, id):
        if UPI.canReference(id):
            return id
        raise IllegalArgumentException("Id number may not be negative " + id)

    def refUPIToString(self, n):
        if n.isNullContext():
            return "" + str(n.getCode())
        if UPI.canReference(n):
            return "" + n.asChars("%Z")
        return "" + str(n.getCode())

    def agjRef(self, node, ifnull=None):
        if not Node:
            if ifnull: return self.agjRef(ifnull)
            else: return None
        if isinstance(node, UPI):
            return self.refUPIToString(node)
        if isinstance(node, EncodedLiteral):
            return self.refEncToString(node)
        if isinstance(node, Value):
            return self.agjRefValue(node)
        if isinstance(node, str):
            return self.refNtripleString(node)
        raise IllegalArgumentException("Cannot map this object to a Value ref" + node)

    def agjRefValue(self, node, ifnull=None):
        if node is None:
            if ifnull: return self.refUPIToString(ifnull)
            else: return None
        if isinstance(node, ValueNode):
            nd = node
            n = nd.queryAGId()
            if UPI.can_reference(n):
                if UPI.isNullContext(n):
                    return "" + AGU_NULL_CONTEXT
                if n.isWild():
                    return "" + AGU_WILD
                return "" + n.asChars("%Z")
            if isinstance(nd, Node):
                return self.uriToAGStringTerm(nd.queryURI())
            else:
                if isinstance(nd, (BlankNode)):
                    return self.refAnonToString(nd.getID())
                else:
                    if isinstance(node, Literal):
                        lt = nd
                        label = lt.queryLabel()
                        if label is None:
                            self.notValRef(node)
                        lang = lt.queryLanguage()
                        if lang is not None:
                            return self.literalToAGStringTerm(label, lang, None)
                        else:
                            type = lt.queryType()
                            if type is not None:
                                return self.literalToAGStringTerm(label, None, type)
                            else:
                                tid = lt.typeId
                                if tid is None:
                                    return self.literalToAGStringTerm(label, None, None)
                                if UPI.canReference(tid):
                                    return self.refLitToString(label, tid)
                                self.notValRef(node)
                    else:
                        self.notValRef(node)
        if isinstance(node, URI):
            return str(self.uriToAGStringTerm(node))
        if isinstance(node, BNode):
            return self.refAnonToString(node.getID())
        if isinstance(node, Literal):
            lt = node
            label = lt.getLabel()
            lang = lt.getLanguage()
            if lang is not None:
                return self.literalToAGStringTerm(label, lang, None)
            dt = lt.getDatatype()
            if dt is None:
                return self.literalToAGStringTerm(label, None, None)
            return self.literalToAGStringTerm(label, None, str(dt))
        self.notValRef(node)
        return ""

    def refEncToString(self, v):
        u = v.queryAGId()
        if (None != u) and u.canReference():
            try:
                return self.refUPIToString(u)
            except (AllegroGraphException, ), e:
                pass
        return "%E" + v.encoding + ";" + v.stringValue()

    def uriToAGStringTerm(self, uri):
        return "%N" + str(uri)

    def refAnonToString(self, label):
        return "%B" + label

    def refLitToString(self, value, type):
        return self.refLitInternal("%U", value, type.asChars())

    def literalToAGStringTerm(self, label, lang, type):
        if type is None:
            if not lang:
                return self.refLitInternal("%L", label, None)
            else:
                return self.refLitInternal("%G", lang, label)
        elif lang:
            raise IllegalArgumentException("Cannot specify language tag and type URI on one literal: " + label + " " + lang + " " + type)
        if (0 == len(type)):
            raise IllegalArgumentException("Empty string is not a valid type URI: " + label)
        return self.refLitInternal("%T", label, type)

    def refLitInternal(self, prefix, part1, part2):
        if part2 is None:
            return prefix + part1
        len = len(part1)
        dd = ""
        digits = "0123456789abcdefghijklmnopqrstABCDEFGHIJKLMNOPQRST"
        while len > 0:
            digit = len % 50
            len = len / 50
            dd = digits[digit] + dd
        if (0 == len(dd)):
            dd = "0"
        return prefix + dd + "X" + part1 + part2

    def notValRef(self, node):
        raise IllegalArgumentException("Cannot convert to AG reference string " + node)

    def validRefs(self, nodes):
        if isinstance(nodes, str):
            return self.agjRef(nodes)
        if isinstance(nodes, Value):
            return self.agjRef(nodes)
        if isinstance(nodes, UPI):
            return nodes
        if isinstance(nodes, list):
            if not nodes: return []
            sample = nodes[0]
            if isinstance(sample, UPI): return nodes
            elif isinstance(sample, str): return self.validRefStrings(nodes)
            else: return self.validRefValues(nodes)
        raise IllegalArgumentException("Cannot map this object to a Value ref or array " + nodes)

    def validRefValues(self, nodes, ifnull=None):
        v = ['' for i in range(len(nodes))]
        ## for-while
        i = 0
        while i < len(v):
            v[i] = self.agjRef(nodes[i], ifnull)
            i += 1
        return v

    def validRefStrings(self, nodes):
        ## for-while
        i = 0
        while i < len(nodes):
            nodes[i] = self.refNtripleString(nodes[i])
            i += 1
        return nodes

    def failCreate(self, what, e):
        raise IllegalStateException("Cannot create " + what + " -- " + e)

    def discardCursor(self, ref):
        self.verifyEnabled().discardCursor(self, ref)

    def discardCursors(self, refs):
        self.verifyEnabled().discardCursors(self, refs)

    def getPartsForUPI(self, id):
        """
        Access the server to retrieve pieces for a single UPI,
        """
        r = self.verifyEnabled().getParts(self, id)
        return [self.typeToString(r[0]), str(r[1]), str([2])]

    def getParts(self, ids, types=None, vals=None, mods=None):
        """
        Access the server to retrieve pieces for a single UPI, or if 'ids'
        is an array, retrieve all pieces for all of the UPIs in the array. 
        """
        if isinstance(ids, UPI): return self.getPartsForUPI(ids)
        ## 
        tnums = [0 for i in range(len(ids))]
        self.verifyEnabled().getParts(self, ids, tnums, vals, mods)
        ## for-while
        for i in range(len(ids)):
            types[i] = self.typeToString(tnums[i])

    def getPartsInternalForUPI(self, id):
        return self.verifyEnabled().getParts(self, id)

    def getPartsInternal(self, ids, tnums=None, vals=None, mods=None):
        if isinstance(ids, UPI): return self.getPartsInternalForUPI(ids)
        self.verifyEnabled().getParts(self, ids, tnums, vals, mods)

    def getTripleParts(self, id):
        v = self.verifyEnabled().getTripleParts(self, id)
        if v is None:
            raise AllegroGraphException("Id is not a triple: " + id)
        return v

    def getSelectLimit(self):
        return self.selectLimit

    def setSelectLimit(self, v):
        if v < 0:
            raise IllegalArgumentException("setSelectLimit argument must be non-negative.")
        self.selectLimit = v

    def createBNodes(self, n):
        v = self.verifyEnabled().newBlankNodes(self, n)
        r = [BlankNode() for i in range(n)]
        ## for-while
        i = 0
        while i < n:
            r[i] = BlankNode(self, v[i], None)
            i += 1
        return r

    def createEncodedLiteral(self, v, encoding):
        return EncodedLiteral(self, v, encoding)

    def createTypedLiteral(self, text, type):
        if isinstance(type, UPI):
            return Literal(self, None, text, type, None, Literal.LANG_NOT_KNOWN, None)
        else:
            return Literal(self, None, text, None, type, Literal.LANG_NOT_KNOWN, None)

    def addTypedLiteral(self, text, type):
        if isinstance(type, URI): return self.addTypedLiteral_for_URI(text, type)
        b = None
        ee = None
        try:
            id = self.verifyEnabled().newLiteral(self, text, type, None)
            if isinstance(type, UPI):
                b = Literal(self, id, text, None, type, Literal.LANG_NOT_KNOWN, None)
            else:
                b = Literal(self, id, text, type, None, Literal.LANG_NOT_KNOWN, None)
        except (AllegroGraphException, ), e:
            ee = e
        if b is None:
            self.failCreate("typedLiteral", ee)
        return b

    def addTypedLiteral_for_URI(self, text, type):
        if isinstance(type, (Node)):
            id = type.queryAGId()
            if id is not None:
                return self.addTypedLiteral(text, id)
        return self.addTypedLiteral(text, str(type))

    def createURI(self, uri):
        return Node(self, None, uri)

    def addURI(self, uri):
        return Node(self, self.verifyEnabled().newResource(self, uri), uri)

    def addURIIds(self, uri):
        return self.verifyEnabled().newResources(self, uri)

    def addURIs(self, uri):
        v = [URI() for i in range(len(uri))]
        ids = self.verifyEnabled().newResources(self, uri)
        ## for-while
        i = 0
        while i < len(uri):
            v[i] = Node(self, ids[i], uri[i])
            i += 1
        return v

    def validRangeRef(self, ref):
        v = self.minMaxRef(ref)
        if v is not None:
            return v
        return self.validRefOrWild(ref)

    def minMaxRef(self, ref):
        if ref is None:
            return
        if isinstance(ref, str):
            s = ref
            if (s.indexOf("m") == 0) or (s.indexOf("M") == 0):
                if (s.indexOf("i") == 1) or (s.indexOf("I") == 1) and (s.indexOf("n") == 2) or (s.indexOf("N") == 2):
                    return "min"
                if (s.indexOf("a") == 1) or (s.indexOf("A") == 1) and (s.indexOf("x") == 2) or (s.indexOf("X") == 2):
                    return "max"
        return

    def getStatements_without_boolean(self, subject, predicate, object, context=None):
        cxt = self.anyContextRef(context, 3) if context else UPI.getNullContextUPI()
        return self.verifyEnabled().getTriples(self, self.validRefOrWild(subject), self.validRefOrWild(predicate), self.validRefOrWild(object), cxt, self.defaultLookAhead)

    def getStatements(self, includeInferred, subject, predicate, object=None, context=None):
        cxt = self.anyContextRef(context, 3) if context else UPI.getNullContextUPI()
        if isinstance(includeInferred, bool):
            return self.verifyEnabled().getInfTriples(self, self.validRefOrWild(subject), self.validRefOrWild(predicate), self.validRefOrWild(object), cxt, lh=self.defaultLookAhead, infer=includeInferred)
        else:
            return self.getStatements_without_boolean(subject, predicate, object=object, context=context)

    def getStatements_with_range(self, includeInferred, subject, subEnd, predicate, predEnd, object, obEnd, context, contextEnd):
        return self.verifyEnabled().getInfTriples(self, self.validRangeRef(subject), self.validRangeRef(predicate), self.validRangeRef(object), self.anyContextRef(context, 5), subend=self.validRangeRef(subEnd), predend=self.validRangeRef(predEnd), obend=self.validRangeRef(obEnd), cxend=self.anyContextRef(contextEnd, 6), lh=self.defaultLookAhead, infer=includeInferred)

    def hasStatement_without_boolean(self, subject, predicate, object, context=None):
        cxt = self.anyContextRef(context, 3) if context else Node.nullContext
        return self.verifyEnabled().hasTriple(self, self.validRefOrWild(subject), self.validRefOrWild(predicate), self.validRefOrWild(object), cxt)

    def hasStatement(self, includeInferred, subject, predicate, object, context=None):
        if isinstance(includeInferred, bool):
            cxt = self.anyContextRef(context, 3) if context else Node.nullContext        
            return self.verifyEnabled().hasInfTriple(self, self.validRefOrWild(subject), self.validRefOrWild(predicate), self.validRefOrWild(object), self.anyContextRef(context, 3), includeInferred)
        else:
            return self.hasStatement_without_boolean(subject, predicate, object, context=context)

    def addStatement(self, subject, predicate, object, context=None):
        self.verifyEnabled().addTriple(self, self.validRef(subject), self.validRef(predicate), self.validRef(object), self.anyContextRef(context, 1))

    def addStatements(self, subject, predicate, object, contexts=None):
        self.verifyEnabled().addTriples(self, self.validRefs(subject), self.validRefs(predicate), self.validRefs(object), self.anyContextRefs(contexts, 1))

    def clear(self):
        self.verifyEnabled().delete(self, UPI.wildUPI(), UPI.wildUPI(), UPI.wildUPI(), UPI.wildUPI(), True)


#    ## we are being lazy and not synchronizing here.  How bad could it be? - RMM
#    def getDataType(self, uri):        
#        if not self.xsiCache.get(uri):
#            self.xsiCache[uri] = self.createURI(uri)
#        return self.xsiCache[uri]
    
    python_type_to_uri = {
        int: XSD.INT,
        long: XSD.LONG,
        float: XSD.FLOAT, ## should we return XSD.DOUBLE here???
        bool: XSD.BOOLEAN,
        str: XSD.STRING,
        }
    
    def createLiteral(self, value, language=None, datatype=None):
        """
         * Create a literal instance with a language tag
         * but do not modify the triple newStore.
         * @return a value that can be safely cast to {@link com.franz.ag.Literal}
         * <p>
         * The literal instance will have a null UPI.
        """
        if isinstance(datatype, Node):
            nd = datatype
            nid = nd.queryAGId()
            if UPI.canReference(nid):
                datatype = nd.queryURI()
            else:
                datatype = str(datatype)
        if not datatype:
            uri = AllegroGraph.python_type_to_uri[type(value)]
        return Literal(self, None, str(value), None, uri, Literal.LANG_IS_KNOWN if language else Literal.LANG_NOT_KNOWN, language)

    def addLiteral(self, value, language=None, datatype=None):
        """
         * Create a literal instance with a language tag 
         * and add the Literal to the triple newStore registry.
         * 
         * @return a Literal instance that can be safely cast to com.franz.ag.Literal        
        """
        if isinstance(datatype, Node):
            nd = datatype
            nid = nd.queryAGId()
            if UPI.canReference(nid):
                v = self.addTypedLiteral(value, nid)
                v.type = str(datatype)
                return v
            else:
                datatype = str(datatype)
        if not datatype:
            uri = AllegroGraph.python_type_to_uri[type(value)]
        if not datatype == XSD.STRING:
            return self.addTypedLiteral(value, datatype)
        try:
            id = self.verifyEnabled().newLiteral(self, str(value), None, language)
            b = Literal(self, None, str(value), None, uri, Literal.LANG_IS_KNOWN if language else Literal.LANG_NOT_KNOWN, language)
        except (AllegroGraphException, ), e:
            ee = e
        if b is None:
            self.failCreate("Literal", ee)
        return b


   
    def addLiterals(self, values, datatypes, languages):        
        ids = self.verifyEnabled().newLiteral(self, values, datatypes, languages)
        v = [Literal() for i in range(len(values))]
        ## for-while
        i = 0
        while i < len(values):
            ts = self.stringElt(datatypes, i)
            ls = self.stringElt(languages, i)
            v[i] = Literal(self, ids[i], values[i], None, ts, Literal.LANG_IS_KNOWN, ls)
            i += 1
        return v

    def createStatement(self, subject, predicate, object, context=None):
        b = Triple()
        s = None
        if isinstance(subject, (ResourceNode)):
            s = self.queryAGId(subject)
        p = None
        if isinstance(predicate, (Node)):
            p = self.queryAGId(predicate)
        o = None
        if isinstance(object, ValueNode):
            o = self.queryAGId(object)
        c = UPI.getNullContextUPI()
        if isinstance(context, (Node)):
            c = self.queryAGId(context)
        b = Triple(self, s, p, o, c)
        b.subjInstance = subject
        b.predInstance = predicate
        b.objInstance = object
        return b

    def select(self, query, arg, svar=None):
        return self.selectStatements(query, arg, svar)

    def refNtripleString(self, s):
        if s is None:
            return s
        if (0 == len(s)):
            return s
        if s.startsWith("<"):
            if s.endsWith(">"):
                return s
        else:
            if s.startsWith("\""):
                return s
            else:
                if s.startsWith("!"):
                    return s
        raise IllegalArgumentException("String does not seem to be in ntriples format: " + s)

    def ntripleCheck(self, s):
        ## for-while
        i = 0
        while i < len(s):
            self.refNtripleString(s[i])
            i += 1
        return s

    def newTriple(self, s, p, o, c=None):
        tra = self.verifyEnabled().addTriple(self, self.validRef(s), self.validRef(p), self.validRef(o), self.anyContextRef(c, 1))
        tr = Triple(self, tra[0].longValue(), tra[1], tra[2], tra[3], tra[4])
        return tr

    def newTripleId(self, s, p, o, c=None):
        tra = self.verifyEnabled().addTriple(self, self.validRef(s), self.validRef(p), self.validRef(o), self.anyContextRef(c, 1))
        return tra[0].longValue()

    def newTriples(self, s, p, o, c=None):
        r = self.verifyEnabled().addTriples(self, self.validRefs(s), self.validRefs(p), self.validRefs(o), self.anyContextRefs(c, 1))
        ri = r[0]
        rs = r[1]
        rp = r[2]
        ro = r[3]
        rt = [Triple() for i in range(len(ri))]
        ## for-while
        i = 0
        while i < len(ri):
            tr = Triple(self, ri[i], rs[i], rp[i], ro[i])
            rt[i] = tr
            i += 1
        return rt

    def newTripleIds(self, s, p, o, c=None):
        r = self.verifyEnabled().addTriples(self, self.validRefs(s), self.validRefs(p), self.validRefs(o), self.anyContextRefs(c, 1))
        return r[0]

    def removeStatements(self, s, p, o, c=None):
        self.verifyEnabled().delete(self, self.validRefOrWild(s), self.validRefOrWild(p), self.validRefOrWild(o), self.anyContextRef(c, 3), True)

    def selectStatements(self, query, arg=None, svars=None, arg3=None, arg4=None, arg5=None):
        if isinstance(query, bool):
            return self.selectStatements_with_boolean(query, arg, svars, arg3, arg4, arg5)
        refs = self.validRefs(arg) if arg else None
        return self.verifyEnabled().selectTriples(self, query, self.validRefs(arg), svars, self.selectLimit, False, False)

    def selectStatements_with_boolean(self, includeInferred, distinct, query, arg, svars):
        return self.verifyEnabled().selectTriples(self, query, self.validRefs(arg), svars, self.selectLimit, includeInferred, distinct)

    def createBNodeIds(self, n):
        return self.verifyEnabled().newBlankNodes(self, n)

    def removeStatement(self, s, p, o, c=None):
        c = c if c else UPI.getNullContextUPI() 
        self.verifyEnabled().delete(self, self.validRef(s), self.validRef(p), self.validRef(o), self.anyContextRef(c, 1), False)

    def validRef(self, x, ifnull=None):
        if not x: return ifnull
        elif isinstance(x, UPI) and x.isWild():
            return ifnull
        if isinstance(x, UPI):
            return self.validID(x)
        if isinstance(x, EncodedLiteral):
            return self.validRefEnc(x)
        if isinstance(x, Value):
            return self.agjRefValue(x)
        if isinstance(x, str):
            return self.refNtripleString(x)
        raise IllegalArgumentException("Not a valid part reference: " + str(x))

    def validRefEnc(self, x):
        u = x.queryAGId()
        if (None != u) and UPI.canReference(u):
            return self.validID(u)
        return self.refEncToString(x)

    def validRefOrWild(self, x):
        return self.validRef(x, UPI.wildUPI())

    ## Verify a context reference
    ## @param x any thing
    ## @param wildok 0 - only explicit context allowed,
    ##               1 - null or "" denotes the null context
    ##               2 - null is wild, "" not allowed
    ##               3 - null is wild, "" is null context
    ##               4 - null is null, "" not allowed
    ##               5 - like 3 + min/max
    ##               6 - like 4 + min/max
    def anyContextRef(self, x, wildok):
        if x is None:
            if wildok == 1:
                return UPI.getNullContextUPI()
            elif wildok == 2 or wildok == 3 or wildok == 5:
                return UPI.wildUPI()
            elif wildok == 4 or wildok == 6:
                return None
            else:
                raise AllegroGraphException("Null is not a valid context")
        if "" == x:
            if wildok == 1 or wildok == 3 or wildok == 5:
                return UPI.getNullContextUPI()
            else:
                raise AllegroGraphException("\"\" is not a valid context")
        if wildok == 5 or wildok == 6:
            v = self.minMaxRef(x)
            if v:
                return v
        return self.validRef(x)

    def anyContextRefs(self, x, wildok):
        if x and isinstance(x, list):
            sample = x[0]           
            if isinstance(sample, UPI):
                return self.anyContextUPIs(x, wildok)
            if isinstance(sample, str):
                return self.anyContextStrings(x, wildok)
            if isinstance(sample, Value):
                return self.anyContextValues(x, wildok)
        return self.anyContextRef(x, wildok)

    def anyContextUPIs(self, x, wildok):
        y = [None for i in range(len(x))]
        ## for-while
        i = 0
        while i < len(x):
            y[i] = self.anyContextRef(x[i], wildok)
            i += 1
        return y

    def anyContextStrings(self, x, wildok):
        y = [None for i in range(len(x))]
        ## for-while
        i = 0
        while i < len(x):
            y[i] = self.anyContextRef(x[i], wildok)
            i += 1
        return y

    def anyContextValues(self, x, wildok):
        y = [None for i in range(len(x))]
        ## for-while
        i = 0
        while i < len(x):
            y[i] = self.anyContextRef(x[i], wildok)
            i += 1
        return y

    def anyContextObjects(self, x, wildok):
        y = [None for i in range(len(x))]
        ## for-while
        i = 0
        while i < len(x):
            z = self.anyContextRef(x[i], wildok)
            if isinstance(z, str):
                y[i] = z
            else:
                y[i] = self.agjRef(z)
            i += 1
        return y

    def queryAGId(self, x):
        if x is None:
            raise IllegalArgumentException("Null node reference not allowed")
        return x.queryAGId()

    def addLiteralIds(self, values, datatypes, languages):
        return self.verifyEnabled().newLiteral(self, values, datatypes, languages)

    def selectValues(self, query, invals, invars, arg3=None, arg4=None):
        if isinstance(query, bool):
            return self.selectValues_with_boolean(query, invals, invars, arg3, arg4)
        v = self.verifyEnabled().selectValues(self, query, invals, invars, False, False)
        return self.selectValuesArray(False, v)

    def selectValues_with_boolean(self, includeInferred, distinct, query, invals, invars):
        v = self.verifyEnabled().selectValues(self, query, invals, invars, includeInferred, distinct)
        return self.selectValuesArray(False, v)

    def selectSingleValues(self, query, invals, invars, arg3=None, arg4=None):
        if isinstance(query, bool):
            return self.selectSingleValues_with_boolean(query, invals, invars, arg3, arg4)
        v = self.verifyEnabled().selectValues(self, query, invals, invars, False, False)
        return self.selectValuesArray(True, v)

    def selectSingleValues_with_boolean(self, includeInferred, distinct, query, invals, invars):
        v = self.verifyEnabled().selectValues(self, query, invals, invars, includeInferred, distinct)
        return self.selectValuesArray(True, v)

    def getFreetextUniqueSubjects(self, pattern):
        v = self.verifyEnabled().getFreetextSubjects(self, pattern, self.selectLimit)
        return self.selectValuesArray(True, v)

    def getFreetextStatements(self, pattern):
        return self.verifyEnabled().getFreetextStatements(self, pattern, self.defaultLookAhead)

    def getFreetextPredicates(self):
        return self.verifyEnabled().freetextPredicates(self, None)

    def registerFreetextPredicate(self, predicate):
        self.verifyEnabled().freetextPredicates(self, self.validRefs(predicate))

    def selectValuesArray(self, one, v):
        if v is None:
            if one:
                return []
            return []
        ids = v[0]
        if ids is None:
            if one:
                return []
            return []
        types = v[1]
        labels = v[2]
        mods = v[3]
        more = v[4].intValue()
        width = v[5].intValue()
        if one and (width != 1):
            raise IllegalArgumentException("Asked for single result but received " + width)
        token = v[6]
        plimit = v[7].intValue()
        all = len(ids)
        if one:
            s = [None for i in range(all)]
            for i in range(all):
                s[i] = self.newValue(ids[i], types[i], labels[i], mods[i])
            return self.registerValues(s, token, more, plimit, None)
        if (width == 0):
            r = []
            return self.registerValues(r, token, more, plimit, None)
        n = all / width
        i = 0
        j = 0
        r = [None for i in range(width)]
        while i < all:
            ## for-while
            k = 0
            while k < width:
                r[j][k] = self.newValue(ids[i], types[i], labels[i], mods[i])
                i += 1
                k += 1
            j += 1
        return self.registerValues(r, token, more, plimit, None)

    def registerValues(self, r, token, more, plimit, sv):
        print "SKIPPING REGISTER VALUES FOR NOW"
        return r
        if more > 0 or sv is not None:
            self.agConnection.addValueMapItem(r, valueMapEntry(self, token, more, plimit, sv))
        self.discardOldTokens(False)
        return r

    def discardOldTokens(self, force):
        print "SKIPPING DISCARD OLD TOKENS FOR NOW "
        return
        with self.discardOldTokensLock:
            if self.agConnection is None:
                return
            if len(self.agConnection.oldTokens) < self.agConnection.oldBatch:
                return
            n = len(self.agConnection.oldTokens)
            if n >= self.agConnection.oldBatch:
                n = self.agConnection.oldBatch
            else:
                if not force:
                    return
            r = [None for i in range(n)]
            ## for-while
            i = 0
            while i < n:
                r[i] = self.agConnection.oldTokens.remove(0)
                i += 1
            self.verifyEnabled().discardCursors(self, r)

    def valuesLength(self, x):
        return len(x)

    def moreValues(self, x):
        e = self.agConnection.valueMap[x]
        if e is None:
            return 0
        if e.savedMore > e.savedPlimit:
            return -e.savedMore
        return e.savedMore

    def valueNames(self, x):
        e = self.agConnection.valueMap[x]
        if e is None:
            return []
        v = e.savedVal
        if v is None:
            return []
        if isinstance(v, list): return v
        return []

    def setSavedVal(self, x, val):
        e = self.agConnection.valueMap[x]
        if e is None:
            return
        e.savedVal = val

    def selectMore(self, sr):
        listOfList = sr and isinstance(sr[0], list)
        return self.selectMoreInternal(listOfList, sr)

    def selectNull(self, one):
        if one:
            return []
        return []

    def selectMoreInternal(self, one, sr):
        e = self.agConnection.valueMap[sr]
        if e is None:
            return self.selectNull(one)
        savedToken = e.savedToken
        v = self.verifyEnabled().nextValuesArray(self, savedToken, self.selectLimit)
        if v is None:
            return self.selectNull(one)
        ids = v[0]
        types = v[1]
        labels = v[2]
        mods = v[3]
        more = v[4].intValue()
        width = v[5].intValue()
        token = v[6]
        plimit = v[7].intValue()
        all = len(ids)
        i = 0
        if more > 0:
            e.savedToken = token
            e.savedMore = more
            e.savedPlimit = plimit
        else:
            self.agConnection.valueMap.remove(sr)
        if one and (width == 1):            
            xs = sr
            if (all == len(xs)):
                ## for-while
                j = 0
                while j < all:
                    xs[j] = self.newValue(ids[j], types[j], labels[j], mods[j])
                    j += 1
                return xs
            n = all / width
            i = 0
            rs = [None for i in range(n)]
            while i < all:
                rs[i] = self.newValue(ids[i], types[i], labels[i], mods[i])
                i += 1
            if more > 0 and not savedToken == token:
                self.agConnection.valueMap.remove(sr)
                self.agConnection.valueMap.put(rs, e)
            self.discardOldTokens(False)
            return rs
        x = sr
        if (all == len(x)):
            ## for-while
            j = 0
            while j < all:
                row = [None for i in range(width)]
                ## for-while
                k = 0
                while k < width:
                    row[k] = self.newValue(ids[i], types[i], labels[i], mods[i])
                    i += 1
                    k += 1
                x[j] = row
                j += 1
            return x
        if (width == 0):
            r = []
        else:
            n = 0
            n = all / width
            i = 0
            j = 0
            r = [None for i in range(width)]
            while i < all:
                ## for-while
                k = 0
                while k < width:
                    r[j][k] = self.newValue(ids[i], types[i], labels[i], mods[i])
                    i += 1
                    k += 1
                j += 1
        if more > 0 and not savedToken == token:
            self.agConnection.valueMap.remove(x)
            self.agConnection.valueMap.put(r, e)
        self.discardOldTokens(False)
        return r

    def twinqlAsk(self, query):
        return self.verifyEnabled().twinqlAsk(self, query, False)

#     * Send a SPARQL SELECT query to AllegroGraph.
#     * 
#     * @param query A string containing a well-formed SPARQL SELECT query.
#     *   If the query string is not a SELECT query. the result is undefined.
#     * @param vars A String containing the names of the SPARQL variables 
#     *    separated by spaces, to be returned from the query.
#     *    The order of the names in this list is the order of the 
#     *    corresponding values in the result array.
#     *    This argument may be null or "" to use the variables listed
#     *    in the query.  Note that in the case of a "SELECT *" query, the
#     *    application must call {@link #valueNames(Object)} to 
#     *    determine the names of the variables for which the bindings are returned
#     *    in each result set.
#     * @param limit Overrides a LIMIT specification in the query string.
#     *    The LIMIT specification in the SPARQL language places a limit on 
#     *    the number of results collected by the query.  This limit is 
#     *    separate and distinct from the setSelectLimit() value.
#     *    A zero or negative value is interpreted as unlimited.
#     * @param offset Overrides an OFFSET specification in the query string. 
#     *    A zero or negative value is interpreted as unlimited.
#     * @return An array of n-tuples containing the bindings of the query 
#     *    variables.  The length of the array is the number of successful
#     *    matches of the query.  Each n-tuple contains as many elements as
#     *    there were variables in the vars argument.  The elements in the
#     *    n-tuple are the bindings of the corresponding variable in the vars
#     *    argument.  If a variable does not have a binding in some n-tuple,
#     *    the element is null. 
    def twinqlSelect(self, includeInferred, query, vars, limit, offset, moreArgs):
        moreArgs = moreArgs or []
        v = self.verifyEnabled().twinqlSelect(self, query, vars, limit, offset, self.selectLimit, includeInferred, moreArgs)
        if v is None:
            return []
        ids = v[0]
        types = v[1]
        labels = v[2]
        mods = v[3]
        more = v[4]
        width = v[5]
        token = None
        if 6 < len(v):
            token = v[6]
        plimit = more
        if 7 < len(v):
            plimit = v[7]
        all = len(ids)
        if (width == 0):
            r = [[] for i in range(all)]
        else:
            n = all / width
            r = [[None for i in range(width)] for j in range(n)]
            i = 0
            j = 0 
            while i < len(ids):
                ## for-while
                k = 0
                while k < width:
                    if (-1 == types[i]):
                        r[j][k] = None
                    else:
                        r[j][k] = self.newValue(ids[i], types[i], labels[i], mods[i])
                    i += 1
                    k += 1
                j += 1
            sv = None
            if 8 < len(v):
                sv = v[8]
            self.registerValues(r, token, more, plimit, sv)
        return r

    def twinqlCount(self, includeInferred, query,  vars, limit, offset, more):
        v = self.verifyEnabled().twinqlSelect(self, query, vars, limit, offset, -1, includeInferred, more)
        return agconnector.longValue(v[0])

    def twinqlFind(self, query, limit, offset):
        return self.verifyEnabled().twinqlFind(self, query, limit, offset, self.selectLimit, False)

    def twinqlQuery(self, query, format, limit, offset):
        return self.verifyEnabled().twinqlQuery(self, query, format, limit, offset, False)

    @staticmethod    
    def prdb(caller, msg):
        """
        Internal debug print method.
        """
        with AllegroGraph.prdbLock:
            if AllegroGraph.dbf is None:
                return
            if AllegroGraph.dbg is None:
                try:
                    AllegroGraph.dbg = open(AllegroGraph.dbf, 'w')
                except Exception, e:
                    traceback.print_stack("Failed to open file '%s' because %s" % (AllegroGraph.dbf, e))
            AllegroGraph.dbg.println(caller + ": " + msg)

    def intResult(self, r):
        return agconnector.toInt(r)

    def getUnmergedCount(self):
        r = self.verifyEnabled().indexing(self, AGU_IQ_CHUNKS, 0, None)
        return self.intResult(r)

    def getUnindexedTripleCount(self):
        r = self.verifyEnabled().indexing(self, AGU_IQ_COUNT, 0, None)
        return self.intResult(r)

    def getUnindexedThreshold(self):
        r = self.verifyEnabled().indexing(self, AGU_IQ_UNTHRESH, 0, None)
        return self.intResult(r)

    def setUnindexedThreshold(self, val):
        self.verifyEnabled().indexing(self, AGU_IS_UNTHRESH, val, None)

    def getUnmergedThreshold(self):
        r = self.verifyEnabled().indexing(self, AGU_IQ_CHTHRESH, 0, None)
        return self.intResult(r)

    def setUnmergedThreshold(self, val):
        self.verifyEnabled().indexing(self, AGU_IS_CHTHRESH, val, None)

    def getIndexFlavors(self):
        r = self.verifyEnabled().indexing(self, AGU_IQ_FLAVORS, 0, None)
        if r is None:
            return []
        return r

    def setIndexFlavors(self, flavors):
        self.verifyEnabled().indexing(self, AGU_IS_FLAVORS, 0, flavors)

    def addIndexFlavors(self, flavors):
        self.verifyEnabled().indexing(self, AGU_IA_FLAVORS, 0, flavors)

    def dropIndexFlavors(self, flavors):
        self.verifyEnabled().indexing(self, AGU_ID_FLAVORS, 0, flavors)

    def addDataMapping(self, map):
        self.verifyEnabled().mapping(self, AGU_MAP_ADD, map)

    def setDataMapping(self, map):
        self.verifyEnabled().mapping(self, AGU_MAP_REP, map)

    def getDataMapping(self):
        r = self.verifyEnabled().mapping(self, AGU_MAP_QUERY, None)
        if r is None:
            return []
        if isinstance(r, list):
            return r
        return []

    def evalInServer(self, expression):
        return self.verifyEnabled().evalInServer(self, expression)

    def serverTrace_with_boolean(self, onoff):
        try:
            self.verifyEnabled().traceServer(self, onoff, None)
            return 0
        except (AllegroGraphException, ), e:
            return -1

    def serverTrace(self, outFile):
        if isinstance(outFile, bool): return self.serverTrace_with_boolean(outFile)
        try:
            self.verifyEnabled().traceServer(self, True, outFile)
            return 0
        except (AllegroGraphException, ), e:
            return -1

    def getNamespaces(self):
        if self.nsregs is None:
            return []
        return self.nsregs.stringArray()

    def getNamespaceRegistry(self):
        if (None == self.nsregs):
            return
        return NamespaceRegistry(self.nsregs)

    def registerNamespace(self, prefix, full):
        self.nsregsInit().register(prefix, full)
        self.verifyEnabled().namespaces(self, self.nsregs.stringArray())

    def registerNamespaces(self, defs):
        if isinstance(defs, NamespaceRegistry): return self.registerNamespaces_with_registry(defs)
        if defs is None:
            return
        if (0 == len(defs)):
            return
        self.nsregsInit()
        ## for-while
        i = 0
        while i < len(defs):
            self.nsregs.register(defs[i], defs[i + 1])
            i = i + 2
        self.verifyEnabled().namespaces(self, self.nsregs.stringArray())

    def registerNamespaces_with_registry(self, ns):
        self.nsregsInit().register(ns)
        self.verifyEnabled().namespaces(self, self.nsregs.stringArray())

    def addPart(self, part):
        if part.startsWith("_:"):
            return self.createBNode(part)
        r = self.verifyEnabled().addPart(self, self.refNtripleString(part))
        upi = r[0] if isinstance(r[0], (UPI)) else None
        type = 0
        if 1 < len(r):
            type = agconnector.longValue(r[1])
        val = None
        mod = None
        if 2 < len(r):
            val = r[2]
        if 3 < len(r):
            val = r[3]
        return self.newValue(upi, type, val, mod)
    
    @staticmethod
    def createFederatedTripleStoreComponent(conn, name, parts, supersede):
        """
        """
        newStore = AllegroGraph(conn, None, name)
        iparts = [int() for i in range(len(parts))]
        for i in range(len(iparts)):
            if (parts[i].agConnection != conn):
                raise AllegroGraphException("Component of federated triple newStore must be opened on same connection.")
            if 0 > parts[i].tsx:
                raise AllegroGraphException("Components of federated triple newStore must be open triple stores.")
            iparts[i] = parts[i].tsx
            newStore.tsx = newStore.verifyEnabled().federate(name, iparts, supersede)
        newStore.registerLocalTripleStore()
        newStore.initNamespaces()        

    def createFederatedStoreComponent(self, caller, ix, name, directory):
        """
        Called by 'getStores'
        """
        newStore = AllegroGraph(caller.agConnection, None, name, directory)
        newStore.tsx = ix
        newStore.registerLocalTripleStore()
        return newStore
     
#     * Retrieve the components of a federated triple store.
#     * @return null if this is not a federated triple store;
#     *    otherwise, return an array of the component stores.
#     *    The result array is composed of new AllegroGraph instances, 
#     *    one for each component.
    def getStores(self):
        r = self.verifyEnabled().getStores(self)
        if not r: return None
        ra = [None for i in range(len(r) / 3)]
        ## for-while
        i = 0
        while i < len(ra):
            ra[i] = self.createFederatedStoreComponent(self, r[i * 3].intValue(), r[1 + i * 3], r[2 + i * 3])
            i += 1
        return ra

    def ServerLevel(self, level):
        return self.agConnection.serverLevel(level)

    def getSyncEveryTime(self):
        return self.sync

    def setSyncEveryTime(self, s):
        self.sync = s

    def __str__(self):
        return str(type(self)) + "<" + self.tsx + " " + self.storeName + ">"

    def getSNAExtension(self):
        if (None == self.sna):
            self.sna = self.SNAExtension()
        return self.sna

    def getGeoExtension(self):
        if (None == self.geo):
            self.geo = self.GeoExtension()
        return self.geo

    def get_createBNode(self):
        b = None
        ee = None
        try:
            b = BlankNode(self, self.verifyEnabled().newBlankNode(self), None)
        except (AllegroGraphException, ), e:
            ee = e
        if b is None:
            self.failCreate("BlankNode", ee)
        return b

    def set_createBNode(self, nodeId):
        b = None
        ee = None
        try:
            b = BlankNode(self, self.verifyEnabled().newBlankNode(self, nodeId), nodeId)
        except (AllegroGraphException, ), e:
            ee = e
        if b is None:
            self.failCreate("BlankNode", ee)
        return b

    def get_indexAllTriples(self):
        try:
            self.verifyEnabled().indexAll(self, True)
        except (IllegalArgumentException, ), e:
            self.throwIndexError(e)
            raise e

    def set_indexAllTriples(self, wait):
        try:
            self.verifyEnabled().indexAll(self, wait)
        except (IllegalArgumentException, ), e:
            self.throwIndexError(e)
            raise e

    def get_setNamespaceRegistry(self):
        if self.agConnection is None:
            self.nsregs = None
        else:
            if (None == self.agConnection.nsregs):
                self.nsregs = None
            else:
                self.nsregs = NamespaceRegistry(self.agConnection.nsregs)
        self.verifyEnabled().namespaces(self, None if self.nsregs is None else self.nsregs.stringArray())

    def set_setNamespaceRegistry(self, ns):
        self.nsregs = NamespaceRegistry(ns)
        self.verifyEnabled().namespaces(self, None if self.nsregs is None else self.nsregs.stringArray())

    def closeTripleStore(self, doClose=True):
        with self.closeTripleStoreLock:
            if self.tsx < 1:
                return False
            r = self.verifyEnabled().closeTripleStore(self, doClose)
            self.tsx = -2
            self.agConnection.removeTS(self)
            return r

    @staticmethod
    def main(args):
        v = AllegroGraph.versions()
        ## for-while
        i = 0
        while i < len(v):
            print AllegroGraph.versionLabels[i] + v[i]
            i += 1


if __name__ == '__main__':
    import sys
    AllegroGraph.main(sys.argv)

