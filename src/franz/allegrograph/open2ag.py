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
from franz.openrdf.model.value import BNode, URI
from franz.openrdf.model.literal import Literal
from franz.openrdf.vocabulary.rdf import RDF
from franz.openrdf.vocabulary.xmlschema import XMLSchema
from franz.allegrograph.upi import UPI

class Term2InternalManager(object):
    """
    Handle translations to/from OpenRDF Terms and AllegroGraph equivalents. 
    """
    def __init__(self, internal_store, value_factory):
        self.internal_ag_store = internal_store
        self.value_factory = value_factory
        ## SHOULD BE USING LRU CACHES HERE:
        self.blankNodeToUPIMap = {}
        self.UPIToBlankNodeMap = {}
    
    def termToInternalTerm(self, value):
        """
        """
        pass
    
    def lookupUPIForBlankNode(self, blankNode, create_if_new=False):
        """
        Map a blank (anonymous) node from OpenRDF to AllegroGraph.        
        """
        upi = self.blankNodeToUPIMap.get(blankNode)
        if upi: return upi
        if not create_if_new: return None
        blankNodeId = blankNode.getBlankNodeId().getLabelString()
        ## now, maybe 'blankNodeId' encodes a UPI:
        ## unfortunately, I can't figure out how to create an AllegroGraph
        ## BNode from a string (or if its even possible).  For now, drop
        ## 'blankNodeId' on the floor:
        ## THIS IS A BUG IF WE SWITCH TO LRU CACHE:
        upi = self.internal_ag_store.createBNodeIds(1)[0]
        self.UPIToBlankNodeMap[upi] = blankNode
        self.blankNodeToUPIMap[blankNode] = upi
        return upi

    def lookupBlankNodeForUPI (self, upi, createIfNew):
        """
        Map a blank (anonymous) node from AllegroGraph to OpenRDF.
        """
        bnode = self.UPIToBlankNodeMap.get(upi)
        if bnode: return bnode
        if not createIfNew: return None
        ## POSSIBLE BUG? BNODES MIGHT CLASH WHEN THEY SHOULDN'T???
        bnode = self.value_factory.createBNode(str(upi))        
        self.UPIToBlankNodeMap[upi] = bnode
        self.blankNodeToUPIMap[bnode] = upi
        return bnode
    
    def openTermToAGTerm(self, term, noneOK=False):
        """
        Convert an OpenRDF Value to a suitable AllegroGraph reference.
        This may be a URI or a tagged string (having a '%x' prefix).
        """
        if isinstance(term, BNode):
            ## lookup Jena id in Jena/AG map
            ## if found, return new Long(AGid)
            ## otherwise make a new BlankNode, add to map, return new Long(AGid)
            return self.lookupUPIForBlankNode(term, True)
        elif isinstance(term, URI):
            ## return string prefixed with URI marker
            return self.internal_ag_store.uriToAGStringTerm(str(term))
        elif isinstance(term, Literal):
            lang = term.getLanguage() or None ## convert empty string to None
            type = str(term.getDatatype()) if term.getDatatype() else None
            return self.internal_ag_store.literalToAGStringTerm(term.getLabel(), lang, type)
        elif term is None and noneOK: return None
        else: raise IllegalArgumentException("Null term passed to 'openTermToAGTerm")

    def openTermToAGTermOrWild(self, term, noneOK=False):
        """
        Convert an OpenRDF Value to a suitable AllegroGraph reference.
        This may be a URI or a tagged string (having a '%x' prefix). If 'term'
        is None, return UPI.WILD.  Careful, don't pass the null
        context null here.
        """
        return UPI.WILD if term is None else self.openTermToAGTerm(term)
        
    def openTermToInternalStringTerm (self, term, noneOK=False):
        """
        Convert an OpenRDF Value to an AllegroGraph string encoding
        at the level understood by  AGDirectConnector
        """
        p = self.openTermToAGTerm(term, noneOK=noneOK)
        if isinstance(p, UPI):
            return self.internal_ag_store.refUPIToString(p)
        elif term is None: 
            if noneOK: return None
            else: raise BadnessException("Null passed to 'openTermToInternalStringTerm' when its not OK")
        else:
            return str(p)

    def openTermToInternalStringTermOrWild (self, term):
        """
        Convert an OpenRDF Value to an AllegroGraph string encoding
        at the level understood by  AGDirectConnector.  If 'term'
        is None, return UPI.WILD.  Careful, don't pass the null
        context null here.
        """
        return UPI.WILD if term is None else self.openTermToInternalStringTerm(term)

    def assembleOpenRDFValueTerm(self, id, type, val, mod ):
        """
        Create an OpenRDF Value instance from AllegroGraph parts.
        Called by 'assembleNodeFromParts' and 'twinqlSelect'
        """
        ## types:  1     2     3        4             5
        ##        anon  term  literal  literal/lang  typed-literal
        if type == 1: return self.lookupBlankNodeForUPI(id, True)
        elif type == 2:  return self.value_factory.createURI(val)
        elif type == 3:  return self.value_factory.createLiteral(val)
        elif type == 4:  return self.value_factory.createLiteral(val, language=mod)    
        elif type == 5:
            datatype = XMLSchema.name2URI(mod, exception_if_failure=False)
            if not datatype:
                ## NOT SURE IF THERE ARE MATCHES HERE OR NOT.  IF SO, WHAT ABOUT RDFS MATCHES??? - RMM
                datatype = RDF.name2URI(mod)
            return self.value_factory.createLiteral(val, datatype=datatype)
        elif type == -1: return None  ## THIS IS AN EXPERIMENT; GAVE MUCH NICER RESULTS IN A SPARQL QUERY - RMM
        else:
            ## This produced a result that I found completely unintelligible.  It made me think there was a bug
            ## in the code.  I believe it imitates the Java.  If so, bad idea.  - RMM 
            raise IllegalArgumentException("Cannot convert AG " 
                + str(type) + "=" + self.internal_ag_store.typeToString(type) +
                "/" + str(val) + "/" + str(mod) + " to OpenRDF Value")

        
    def nullContextObject(self):
        return UPI.getNullContextUPI()
    
#    def nullContextString(self):
#        return str(UPI.getNullContextUPI())

    def wildValue(self): return UPI.WILD

