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
from franz.openrdf.model.value import Value
from franz.openrdf.model.statement import Statement

# * A RepositoryResult is a result collection of objects (for example
# * {@link org.openrdf.model.Statement}, {@link org.openrdf.model.Namespace},
# * or {@link org.openrdf.model.Resource} objects) that can be iterated over. It
# * keeps an open connection to the backend for lazy retrieval of individual
# * results. Additionally it has some utility methods to fetch all results and
# * add them to a collection.
# * <p>
# * By default, a RepositoryResult is not necessarily a (mathematical) set: it
# * may contain duplicate objects. Duplicate filtering can be {{@link #enableDuplicateFilter() switched on},
# * but this should not be used lightly as the filtering mechanism is potentially
# * memory-intensive.
# * <p>
# * A RepositoryResult needs to be {@link #close() closed} after use to free up
# * any resources (open connections, read locks, etc.) it has on the underlying
# * repository.
# * 
# * @see RepositoryConnection#getStatements(org.openrdf.model.Resource,
# *      org.openrdf.model.URI, org.openrdf.model.Value, boolean,
# *      org.openrdf.model.Resource[])
# * @see RepositoryConnection#getNamespaces()
# * @see RepositoryConnection#getContextIDs()

class RepositoryResult(object):  ## inherits IterationWrapper
    def __init__(self, string_tuples, limit=None):
        self.string_tuples = string_tuples
        self.cursor = 0
        self.nonDuplicateSet = None
        self.limit = limit
        
    def _createStatement(self, string_tuple):
        """
        Allocate a Statement and fill it in from 'string_tuple'.
        """
        stmt = Statement(None, None, None, None)
        stmt.setQuad(string_tuple)
        return stmt
    
    def __iter__(self): return self
    
    def close(self):
        """
        Shut down the iterator, to insure that resources are free'd up.
        """
        pass
        
    def next(self):
        """
        Return the next Statement in the answer, if there is one.  Otherwise,
        raise StopIteration exception.
        TODO: WHOOOA.  WHAT IF WE HAVE TUPLES INSTEAD OF STATEMENTS; HOW DOES THAT WORK???
        """
        if not self.nonDuplicateSet is None:
            try:
                savedNonDuplicateSet = self.nonDuplicateSet
                self.nonDuplicateSet = None
                while (True):
                    stmt = self.next()
                    if not stmt in savedNonDuplicateSet:
                        savedNonDuplicateSet.add(stmt)
                        return stmt                        
            finally:
                self.nonDuplicateSet = savedNonDuplicateSet
        elif self.limit and self.cursor >= self.limit:
            raise StopIteration
        elif self.cursor < len(self.string_tuples):
            stringTuple = self.string_tuples[self.cursor]
            self.cursor += 1
            return self._createStatement(stringTuple);
        else:
            raise StopIteration

#     * Switches on duplicate filtering while iterating over objects. The
#     * RepositoryResult will keep track of the previously returned objects in a
#     * {@link java.util.Set} and on calling next()  will ignore any
#     * objects that already occur in this Set.
#     * <P>
#     * Caution: use of this filtering mechanism is potentially memory-intensive.
    def enableDuplicateFilter(self):
        self.nonDuplicateSet = set([])


    def asList(self):
        """
        Returns a list containing all objects of this RepositoryResult in
        order of iteration. The RepositoryResult is fully consumed and
        automatically closed by this operation.
        """
        return self.addTo([])

    def addTo(self, collection):
        """
        Adds all objects of this RepositoryResult to the supplied collection. The
        RepositoryResult is fully consumed and automatically closed by this
        operation.
        """
        isList = isinstance(collection, list)
        for stmt in self:
            if isList: collection.append(stmt)
            else: collection.add(stmt)        

    def rowCount(self): return len(self.string_tuples)
