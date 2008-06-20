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
from franz.openrdf.repository.jdbcresultset import JDBCResultSet


## A JDBSResultSet is a JDBC ResultSet iterator over a collection of tuple results
## The results may be rows resulting from a SELECT query or triples/quads resulting
## from a getStatements query.  JDBC-style access is provided as a more efficient
## alternative to the bulky RepositoryResult iterator.  It permits extraction of
## values or strings without the superfluous BindingSet wrapper.

STATEMENTS_CURSOR = 'StatementsCursor'
SELECT_ITERATOR = 'SelectIterator'

class JDBCResultSetImpl(JDBCResultSet):
    """
    """
    def __init__(self, cursor=None):
        self.cursor = cursor
        
    def _validate_cursor_index(self, index):
        """
        Verify correctness of inputs to a Cursor/Statements iterator
        """
        if index is None:
            raise JDBCException("Failed to include 'index' argument to JDBC Statements iterator")
        if not type(index) == int:
            raise JDBCException("Passed non-int argument to 'index'")
        if index < 1 or index > 4:
            raise JDBCException("'index' argument should be between 1 and 4, inclusive.")    

    def getInt(self, index):
        """
        Return an integer value for the column denoted by 'index'.
        Throw an exception if the column value is not either an integer, or
        something readily convertable to an integer (e.g., a float).
        'index' may be an integer or a column name.
        Note: JDBC-style column numbering begins at one (1), not zero.
        If the tuple denotes a Statement (a quad), then 1 = subject, 2 = predicate
        3 = object, and 4 = context. 
        """
        raise UnimplementedMethodException("getInt")

    def getMetaData(self):
        """
        Return a ResultSetMetaData object that provides the name and datatype of each
        column in the ResultSet.
        """
        raise UnimplementedMethodException("getMetaData")
        
    def getRow(self):
        """
        Return a Python tuple of OpenRDF Value objects.  For a SELECT query, the 
        row contains a Value for each SELECT column.  For a getStatements query,
        the row contains subject, predicate, object, and context values.
        Note: This call does NOT advance the iterator to the next row.
        """
        if self.cursor:
            quad = self.cursor.current_quad
            return (quad.getSubject(), quad.getPredicate(), quad.getObject(), quad.getContext())
        
    def getString(self, index):
        """
        Return a string value for the column denoted by 'index'.
        For a resource-valued column, this is a URI label.  For a literal-valued
        column, this is the string representation of the literal value.
        'index' may be an integer or a column name.
        Note: JDBC-style column numbering begins at one (1), not zero.
        If the tuple denotes a Statement (a quad), then 1 = subject, 2 = predicate
        3 = object, and 4 = context.  Alternatively, callers may use the constants
        JDBCResultSet.SUBJECT, JDBCResultSet.PREDICATE, JDBCResultSet.OBJECT, JDBCResultSet.CONTEXT
        as values for the 'index' argument. 
        """
        if self.cursor:
            self._validate_cursor_index(index)
            quad = self.cursor.current_quad   
            return quad.queryPartLabel(index)         
        raise JDBCException("JDBC-style SELECT results not yet implemented")
    
    def getValue(self, index=None, columnName=None):
        """
        Return an OpenRDF Value (a Resource or Literal) for the column denoted by 'index'
        or named 'columnName'. 'index' is an integer greater than zero.  'columnName' is
        a string matching a variable name.
        Note: JDBC-style column numbering begins at one (1), not zero.
        If the tuple denotes a Statement (a quad), then 1 = subject, 2 = predicate
        3 = object, and 4 = context. Alternatively, callers may use the constants
        JDBCResultSet.SUBJECT, JDBCResultSet.PREDICATE, JDBCResultSet.OBJECT, JDBCResultSet.CONTEXT
        as values for the 'index' argument. 
        """
        if self.cursor:
            self._validate_cursor_index(index)
            quad = self.cursor.current_quad            
            if index == 1: return quad.getSubject()
            elif index == 2: return quad.getPredicate()
            elif index == 3: return quad.getObject()
            elif index == 4: return quad.getContext()
        raise JDBCException("JDBC-style SELECT results not yet implemented")

    def next(self):
        """
        Advance to the next tuple.  Return True if there is one, and False
        if the iterator has been exhausted.
        """
        if self.cursor:
            if self.cursor.hasNext():
                self.cursor.step()
                return True
            else:
                return False
        else:
            raise JDBCException("Failed to properly initialize JDBC ResultSet")
    
    
   