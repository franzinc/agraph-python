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
from franz.openrdf.query.queryresultimpl import TupleQueryResultImpl, GraphQueryResultImpl
from franz.allegrograph.upi import UPI
from franz.openrdf.model.value import Value

#class SelectTupleResultsIterator(object):
#    def __init__(self, ids, types, labels, mods, more, tupleWidth, token, plimit, sv):
#        self.ids = ids
#        self.types = types,
#        self.labels = labels,
#        self.mods = mods
#        self.tupleWidth = tupleWidth
#        self.totalBindingsCount = len(ids)
#        self.tupleCount = self.totalBindingsCount / tupleWidth
#        self.reusableRow = [None for k in range(tupleWidth)]
#        self.cursor = 0
#        self.term2InternalMgr = None
#    
#    def __iter__(self): return self
#    
#    def next(self):
#        if self.cursor >= self.totalBindingsCount:
#            raise StopIteration()
#        for i in range(self.tupleWidth):
#            offset = self.cursor
#            term = self.term2InternalMgr.assembleOpenRDFValueTerm(self.ids[offset], 
#                                    self.types[0][offset], self.labels[0][offset], self.mods[offset])
#            self.reusableRow[i] = term
#            self.cursor += 1
#        return tuple(self.reusableRow)
#    
#    def setTerm2InternalManager(self, term2InternalMgr):
#        self.term2InternalMgr = term2InternalMgr

class DirectCaller(object):
    """
    Handles calls from Sesame code direct to AGDirectConnector, by-passing
    AllegroGraph, which inserts baggage that we don't want, e.g., ValueObject objects
    """
    ALL_CONTEXTS = []
    def __init__(self, agDirectConnector, term2InternalMgr):
        self.agDirectConnector = agDirectConnector
        self.term2InternalMgr = term2InternalMgr
        self.internal_ag_store = term2InternalMgr.internal_ag_store
    
    def canonicalize_context_argument(self, context):
        if isinstance(context, Value): return self.internal_ag_store.agjRefValue(context)
        elif context == []:
            return UPI.WILD
        elif context is None:
            return UPI.NULL_CONTEXT
        else:
            raise IllegalArgumentException("Expected context argument %s to be a Resource or empty list or None" % context)

    def verifyEnabled(self):
        return self.agDirectConnector
    
    def numberOfTriples(self):
        """
        Return the number of triples in the store
        """
        return self.agDirectConnector.numberOfTriples(self.internal_ag_store)
    
    def getTriples(self, subject, predicate, object, contexts, includeInferred=False):
        """
        """
        if contexts is None:
            theContext = None
        elif contexts == []:
            theContext = self.ALL_CONTEXTS
        elif len(contexts) == 1:
            theContext = contexts[0]
        else:
            ## THE CALLERS SHOULD BE DOING EXPLICIT (BUT SLOW) ITERATION TO COMPENSATE.  IF
            ## NOT, ITS A BUG IN THE CODE:
            raise Exception("'getStatements' over multiple contexts is not yet implemented")
        mgr = self.term2InternalMgr
        if includeInferred:
            return self.agDirectConnector.getInfTriples(self.internal_ag_store,
                               mgr.openTermToInternalStringTermOrWild(subject),
                               mgr.openTermToInternalStringTermOrWild(predicate),
                               mgr.openTermToInternalStringTermOrWild(object),
                               self.canonicalize_context_argument(theContext),
                               lh = 0,
                               infer=True
                               )
        else:
            return self.agDirectConnector.getTriples(self.internal_ag_store,
                               mgr.openTermToAGTermOrWild(subject),
                               mgr.openTermToAGTermOrWild(predicate),
                               mgr.openTermToAGTermOrWild(object),
                               self.canonicalize_context_argument(theContext),
                               lh = 0,
                               )
        

        
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
#     * @return An list of n-tuples containing the bindings of the query 
#     *    variables.  The length of the array is the number of successful
#     *    matches of the query.  Each n-tuple contains as many elements as
#     *    there were variables in the vars argument.  The elements in the
#     *    n-tuple are the bindings of the corresponding variable in the vars
#     *    argument.  If a variable does not have a binding in some n-tuple,
#     *    the element is null. 
    def twinqlSelect(self, query, vars, limit, offset, useInference, moreArgs):
        moreArgs = moreArgs or []
        slimit = 0 ## I HAVE NO IDEA WHAT THIS IS, BUT IF IT WASN'T SUCH A STUPID NAME, I MIGHT BE ABLE TO GUESS - RMM
        v = self.verifyEnabled().twinqlSelect(self.internal_ag_store, query, vars, limit, offset, slimit, useInference, moreArgs)
        if v is None:
            return TupleQueryResultImpl([], [], [], [], moreArgs, 1, None, None, None)
        ids = v[0]
        types = v[1]
        labels = v[2]
        mods = v[3]
        more = v[4]
        tupleWidth = v[5]
        token = v[6] if len(v) > 6 else None
        plimit = v[7] if len(v) > 7 else None
        variableNames = v[8] if len(v) > 8 else None
        iterator = TupleQueryResultImpl(ids, types, labels, mods, more, tupleWidth, token, plimit, variableNames)
        iterator.setTerm2InternalManager(self.term2InternalMgr)
        return iterator
    
        ## THIS IS HERE IF FOR SOME REASON WE DON'T WANT TO RETURN AN ITERATOR (UNLIKELY):
#        totalBindingsCount = len(ids)
#        tupleCount = totalBindingsCount / tupleWidth
#        if (tupleWidth == 0):
#            return [() for i in range(tupleCount)]
#        rows = []
#        reusableRow = [None for k in range(tupleWidth)]
#        for i in range(0, totalBindingsCount, tupleWidth):            
#            for j in range(tupleWidth):
#                offset = i + j
#                reusableRow[j] = self.term2InternalMgr.assembleOpenRDFValueTerm(ids[offset], types[offset], labels[offset], mods[offset])
#            rows.append(tuple(reusableRow))                
#        self.registerValues(rows, token, more, plimit, variableNames)
#        return rows

    def twinqlFind(self, query, limit, offset, includeInferred=False, more=[]):
        cursor = self.verifyEnabled().twinqlFind(self.internal_ag_store, query, limit, offset, self.internal_ag_store.selectLimit, infer=includeInferred, more=more)
        return GraphQueryResultImpl(cursor)

    def registerValues(self, rows, token, more, plimit, sv):
        print "SKIPPING REGISTER VALUES FOR NOW"
        return
        if more > 0 or sv is not None:
            self.agConnection.addValueMapItem(rows, self.valueMapEntry(self, token, more, plimit, sv))
        self.discardOldTokens(False)

