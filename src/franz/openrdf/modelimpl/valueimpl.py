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
from franz.openrdf.util import uris
from franz.openrdf.model.literal import Literal
from franz.openrdf.model.value import Value, URI, BNode

class ValueImpl(Value):
    
    @staticmethod
    def identicalUPIs(x, y):
        """
        Return 1 if 'x' and 'y' have identical UPIs.  Return -1 if
        they are incomparable.  Otherwise, return 0, leaving it to something
        else to do the comparison.
        """
        if y is None:
            return 0
        if type(x) == type(y):
            xid = x.upi
            yid = y.upi
            if UPI.can_reference(xid) and UPI.can_reference(yid):
                if xid == yid:
                    return 1
            return 0 ## zero apparently means "I don't know"
        return -1


class URIImpl(URI):
    """
    AllegroGraph-specific implementation of URI.
    """
    nullContext = None
    NULL_CONTEXT_URI = None
    def __init__(self, uri=None, upi=None, store=None, namespace=None, localname=None):
        super(URIImpl, self).__init__(uri=uri, namespace=namespace, localname=localname)
        self.upi = upi
        self.store = store
    
    def getURI(self):
        """
        Return the URI for 'self'.  On occasions, this means a database access,
        which is expensive.
        """
        if not self.uri:
            if self.store:
                self.uri = self.store.getText(self.upi, None)
            if not self.uri:
                raise BadnessException("Can't materialize a URI for some reason.")
        return self.uri

    def __eq__(self, other):
        compare = ValueImpl.identicalUPIs(self, other)
        if compare == 1: return True
        elif compare == -1: return False
        else: return str(self) == str(other)

    def __hash__(self):
        ## WOW: THIS IS REALLY EXPENSIVE WHEN THE STRING IS NOT AVAILABLE:
        return str(self).__hash__()

URIImpl.NULL_CONTEXT_URI = URIImpl(upi=UPI.NULL_CONTEXT, uri="http://openrdf.org#NuLlCoNtExT")
URIImpl.nullContext = URIImpl(upi=UPI.getNullContextUPI(), uri=URIImpl.NULL_CONTEXT_URI.getURI())
       
class BNodeImpl(BNode):
    """
    """
    def __init__(self, upi=None, id=None, store=None):
        self.upi = upi
        firstChar = id[:1]
            
        self.id = id if firstChar.isalpha() else "A%s" % id 
        self.store = store
        ## QUESTION: WHAT THE HECK IS AN 'idString'.  Is it different than an ID???
        if id:
            self.idString = "_:A%s" % id
        else:
            self.idString = "_:blank" + upi.blankNodeID()
            
    def getID(self):
        ## THIS IS MESSED UP, BECAUSE THE AG CODE WANTS TO USE 'idString' EVERYWHERE
        ## AND SESAME DOESN'T.
        return self.id or self.idString
    
    def __str__(self):
        return "_:" + self.id
          
    def __eq__(self, other):
        return isinstance(other, BNode) and self.getId() == other.getId()
    
    def __hash__(self):
        return self.idString.__hash__()

            

