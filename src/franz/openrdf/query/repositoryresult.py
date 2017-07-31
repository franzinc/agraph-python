#!/usr/bin/env python
# -*- coding: utf-8 -*-
# pylint: disable-msg=C0103

################################################################################
# Copyright (c) 2006-2017 Franz Inc.  
# All rights reserved. This program and the accompanying materials are
# made available under the terms of the MIT License which accompanies
# this distribution, and is available at http://opensource.org/licenses/MIT
################################################################################

from __future__ import absolute_import
from __future__ import unicode_literals
from future.builtins import next, object
from past.builtins import unicode

from ..model import Statement, Value

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
    def __init__(self, string_tuples, subjectFilter=None, tripleIDs=False):
        self.string_tuples = string_tuples
        self.cursor = 0
        self.nonDuplicateSet = None
        #self.limit = limit
        self.subjectFilter = subjectFilter
        self.triple_ids = tripleIDs  
        
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

    def __next__(self):
        """
        Return the next Statement in the answer, if there is one.  Otherwise,
        raise StopIteration exception.
        TODO: WHOOOA.  WHAT IF WE HAVE TUPLES INSTEAD OF STATEMENTS; HOW DOES THAT WORK???
        """
        if self.nonDuplicateSet is not None:
            try:
                savedNonDuplicateSet = self.nonDuplicateSet
                self.nonDuplicateSet = None
                while (True):
                    stmt = next(self)
                    if not stmt in savedNonDuplicateSet:
                        savedNonDuplicateSet.add(stmt)
                        return stmt                        
            finally:
                self.nonDuplicateSet = savedNonDuplicateSet
#        elif self.limit and self.cursor >= self.limit:
#            raise StopIteration
        elif self.cursor < len(self.string_tuples):
            stringTuple = self.string_tuples[self.cursor]
            if self.triple_ids:
                stringTuple = RepositoryResult.normalize_quint(stringTuple)
            self.cursor += 1
            if self.subjectFilter and not stringTuple[0] == self.subjectFilter:
                return next(self)
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
        result = []
        self.addTo(result)
        return result

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

    def __len__(self):
        return len(self.string_tuples)

    # Python-future breaks truth testing - length is not checked...
    def __bool__(self):
        return len(self) > 0
    
    def rowCount(self):
        return len(self)
    
    @staticmethod
    def normalize_quint(stringTuple):
        st = stringTuple
        return (st[1], st[2], st[3], None if len(st) == 4 else st[4], unicode(st[0]))

    @staticmethod
    def normalize_quad(stringTuple):
        st = stringTuple
        if len(st) == 3:
            return (st[0], st[1], st[2], None)
        
        return st
