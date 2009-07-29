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
from franz.openrdf.model.statement import Statement
from franz.openrdf.model.value import Value
from franz.openrdf.repository.repositoryresult import RepositoryResult


## A JDBSResultSet is a JDBC ResultSet iterator over a collection of tuple results
## The results may be rows resulting from a SELECT query or triples/quads resulting
## from a getStatements query.  JDBC-style access is provided as a more efficient
## alternative to the bulky RepositoryResult iterator.  It permits extraction of
## values or strings without the superfluous BindingSet wrapper.

STATEMENTS_CURSOR = 'StatementsCursor'
SELECT_ITERATOR = 'SelectIterator'

class JDBCResultSet(object):
    """
    """
    COUNTING_STARTS_AT = 0   # real JDBC starts at one; we could change this to 1 if users want
    def __init__(self, string_tuples, column_names=None, tripleIDs=False):
        ## NOT SURE WHY WE CALL 'lower' HERE:
        self.column_names = [v.lower() for v in column_names]
        self.string_tuples = string_tuples
        self.cursor = 0
        self.tuple_width = None
        self.low_index = -1
        self.high_index = -1
        self.current_strings_row = None
        self.current_terms_row = None 
        self.triple_ids = tripleIDs       
    
    def initialize_tuple_width(self):
        self.tuple_width = len(self.current_strings_row)
        self.low_index = JDBCResultSet.COUNTING_STARTS_AT
        self.high_index = JDBCResultSet.COUNTING_STARTS_AT + self.tuple_width
        
    def _get_valid_cursor_index(self, index):
        """
        Verify that 'index' is in bounds.  Convert from column name to integer
        if necessary.  Return valid index.
        """
        if self.tuple_width is None:
            self.initialize_tuple_width()
        if index is None:
            raise JDBCException("Failed to include 'index' argument to JDBC Statements iterator")
        if isinstance(index, int):
            if index >= self.low_index and index <= self.high_index:
                return index - JDBCResultSet.COUNTING_STARTS_AT
            else:
                raise JDBCException("'index' argument should be between %i and %i, inclusive." % (self.low_index, self.high_index))    
        elif isinstance(index, str):
            i = 0
            name = index.lower()
            while i < len(self.column_names):
                if name == self.column_names[i]:
                    return i
                i += 1
            raise JDBCException("Unmatched column name '%s'" % index)                
        else:
            raise JDBCException("Passed non-integer, non-string argument '%s' to JDBC call." % str(index))

    def _get_terms_row(self):
        """
        Return a terms row (a list) for 'self'.  It may or may not be partially or completely
        filled in with terms.  Call 'get_row' to return a fully-filled-in row.
        """
        row = self.current_terms_row
        if not row:
            if self.tuple_width is None:
                self.initialize_tuple_width()
            row = [None] * self.tuple_width
            self.current_terms_row = row
        return row
        
    def getInt(self, index):
        """
        Return an integer value for the column denoted by 'index'.
        Throw an exception if the column value is not either an integer, or
        something readily convertible to an integer (e.g., a float).
        'index' may be an integer or a column name.
        If the tuple denotes a Statement (a quad), then 2 = object position. 
        Note: Column numbering begins at zero.  True JDBC-style numbering begins at
        one (1).  Evaluate:
             JDBCResultSet.COUNTING_STARTS_AT = 1
        to make counting start at one.
        """
        index = self._get_valid_cursor_index(index)
        try:
            value =  int(Statement.ntriples_string_to_value(self.current_strings_row[index]))
            return value
        except:
            raise JDBCException("Cannot convert value '%s' to an integer." 
                                % Statement.ntriples_string_to_value(self.current_strings_row[index]))

    def getMetaData(self):
        """
        Return a ResultSetMetaData object that provides the name and datatype of each
        column in the ResultSet.
        """
        raise NotImplementedError("getMetaData")
        
    def getRow(self):
        """
        Return a Python tuple of OpenRDF Value objects.  For a SELECT query, the 
        row contains a Value for each SELECT column.  For a getStatements query,
        the row contains subject, predicate, object, and context values.
        Note: This call does NOT advance the iterator to the next row.
        """
        i = 0
        while i < self.tuple_width:
            self.getValue(i)
            i += 1;
        return self._get_terms_row()
        
    def getString(self, index):
        """
        Return a string value for the column denoted by 'index'.
        For a resource-valued column, this is a URI label.  For a literal-valued
        column, this is the string representation of the literal value.
        'index' may be an integer or a column name.
        If the tuple denotes a Statement (a quad), then 0 = subject, 1 = predicate
        2 = object, and 3 = context. 
        Note: Column numbering begins at zero.  True JDBC-style numbering begins at
        one (1).  Evaluate:
             JDBCResultSet.COUNTING_STARTS_AT = 1
        to make counting start at one.
        """
        index = self._get_valid_cursor_index(index)
        return Statement.ntriples_string_to_value(self.current_strings_row[index])
    
    def getValue(self, index=None, columnName=None):
        """
        Return an OpenRDF Value (a Resource or Literal) for the column denoted by 'index'
        or named 'columnName'. 'index' is an integer greater than zero.  'columnName' is
        a string matching a variable name.
        If the tuple denotes a Statement (a quad), then 0 = subject, 1 = predicate
        2 = object, and 3 = context. 
        Note: Column numbering begins at zero.  True JDBC-style numbering begins at
        one (1).  Evaluate:
             JDBCResultSet.COUNTING_STARTS_AT = 1
        to make counting start at one.
        """
        row = self._get_terms_row()
        index = self._get_valid_cursor_index(index or columnName)
        term = row[index]
        if not term:
            term = Statement.stringTermToTerm(self.current_strings_row[index])
            row[index] = term
        return term

    def next(self):
        """
        Advance to the next tuple.  Return True if there is one, and False
        if the iterator has been exhausted.
        """
        if self.string_tuples is None:
            raise JDBCException("Failed to properly initialize JDBC ResultSet")
        if self.cursor < len(self.string_tuples):
            stringTuple = self.string_tuples[self.cursor]
            if self.triple_ids:
                stringTuple = RepositoryResult.normalize_quint(stringTuple)
            self.current_strings_row = stringTuple
            self.current_terms_row = None
            self.cursor += 1
            return True
        else:
            return False
    
    def close(self):
        """
        Currently a no-op.  When we do streaming, this may do something
        """
        pass
  

    def __len__(self):
        return len(self.string_tuples)
    
    def rowCount(self):
        return len(self)
