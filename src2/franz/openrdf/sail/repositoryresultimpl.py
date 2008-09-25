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
from franz.openrdf.modelimpl.statementimpl import StatementImpl
from franz.openrdf.repository.repositoryresult import RepositoryResult


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

class RepositoryResultImpl(RepositoryResult):  ## inherits IterationWrapper
    def __init__(self, cursor):
        self.socket_cursor = cursor
        
    def _createStatement(self):
        """
        Allocate a StatementImpl and fill it in from the socket_cursor.
        """
        stmt = StatementImpl(None, None, None, None)
        stmt.setQuad(self.socket_cursor.pop_current_quad())
        return stmt
    
    def __iter__(self): return self
    
    def close(self):
        """
        Shut down the iterator, to insure that resources are free'd up.
        """
        if self.socket_cursor:
            self.socket_cursor.close()
            self.socket_cursor = None
        
    def next(self):
        """
        Return the next Statement in the answer, if there is one.  Otherwise,
        raise StopIteration exception.
        """
        if self.socket_cursor.hasNext():
            self.socket_cursor.step()
            return self._createStatement();
        else:
            raise StopIteration

#     * Switches on duplicate filtering while iterating over objects. The
#     * RepositoryResult will keep track of the previously returned objects in a
#     * {@link java.util.Set} and on calling next() or hasNext() will ignore any
#     * objects that already occur in this Set.
#     * <P>
#     * Caution: use of this filtering mechanism is potentially memory-intensive.
    def enableDuplicateFilterself(self):
        raise UnimplementedMethodException()


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

class CompoundRepositoryResultImpl(RepositoryResultImpl):  ## inherits IterationWrapper
    """
    Combines multiple cursors into one.  Overcomes temporary inability of AG to 
    handle multiple contexts in a getTriples.
    """
    def __init__(self, cursors):
        self.cursors = [RepositoryResultImpl(crsr) for crsr in cursors]
        self.current_cursor = self.cursors[0]
        self.cursor_index = 0
            
    def __iter__(self): return self
    
    def close(self):
        """
        Shut down the iterator, to insure that resources are free'd up.
        """
        if self.current_cursor:
            for i in range(self.cursor_index, len(self.cursors)):
                self.cursors[i].close()
            self.current_cursor = None
            self.cursors = None
        
    def next(self):
        """
        Return the next Statement in the answer, if there is one.  Otherwise,
        raise StopIteration exception.
        """
        if not self.current_cursor:
            raise StopIteration
        try:
            stmt = self.current_cursor.next()
            return stmt
        except StopIteration:
            self.cursor_index += 1
            if self.cursor_index < len(self.cursors):
                self.current_cursor = self.cursors[self.cursor_index]
                return self.next()
            else:
                self.current_cursor = None
                self.cursors = None
                raise StopIteration

#     * Switches on duplicate filtering while iterating over objects. The
#     * RepositoryResult will keep track of the previously returned objects in a
#     * {@link java.util.Set} and on calling next() or hasNext() will ignore any
#     * objects that already occur in this Set.
#     * <P>
#     * Caution: use of this filtering mechanism is potentially memory-intensive.
    def enableDuplicateFilterself(self):
        raise UnimplementedMethodException()


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
