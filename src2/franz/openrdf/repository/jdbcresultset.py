#!/usr/bin/env python
# -*- coding: utf-8 -*-
# pylint: disable-msg=C0103

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

from __future__ import absolute_import

from ..exceptions import JDBCException
from ..model import Statement, Value
from .repositoryresult import RepositoryResult

class JDBCResultSet(object):
    """
    A JDBSResultSet is the base class for a JDBC-like ResultSet iterator over
    a collection of tuple results. JDBC-style access is provided as an
    alternative to the RepositoryResult iterator.  It permits extraction of
    integers, strings, or OpenRDF values.

    Note: Column numbering begins at zero.  True JDBC-style numbering begins at
    one (1).  Evaluate:
         JDBCResultSet.COUNTING_STARTS_AT = 1
    at your client program's initialization time to make counting start at one.
    """

    COUNTING_STARTS_AT = 0   # JDBC starts at one; we could change this to 1 if users want
    def __init__(self, string_tuples, column_names):
        self.tuple_width = len(column_names)
        if string_tuples is None or self.tuple_width == 0:
            raise JDBCException('Failed to properly initialize JDBC ResultSet')
        self.string_tuples = string_tuples
        self.column_names = [v.lower() for v in column_names]
        self.cursor = 0
        self.current_strings_row = None
        self.current_terms_row = None 
        self.iter_called = False
    
    def _tuple_index(self, index):
        """
        Verify that 'index' is in bounds.  Convert from column name to integer
        if necessary.  Returns a valid index.
        """
        if isinstance(index, int):
            index -= self.COUNTING_STARTS_AT

            if index < 0 or index >= self.tuple_width:
                raise JDBCException('"index" argument should in [%d, %d).' %
                    (self.COUNTING_STARTS_AT,
                     self.COUNTING_STARTS_AT + self.tuple_width))
        elif isinstance(index, basestring):
            try:
                index = self.column_names.index(index.lower())
            except ValueError:
                raise JDBCException('Unmatched column name "%s"' % index)
        else:
            raise JDBCException('Passed non-integer, non-string argument '
                                '"%s" to JDBC call.' % str(index))

        return index

    def _terms_row(self):
        """
        Return terms row storage (a list) for 'self'.
        """
        row = self.current_terms_row
        if row is None:
            self.current_terms_row = row = [None] * self.tuple_width
        return row
        
    def getInt(self, indexOrName):
        """
        Return an integer value for the column denoted by 'indexOrName'.

        'indexOrName' may be an integer or a column name.
        """
        index = self._tuple_index(indexOrName)
        value = Statement.ntriples_string_to_value(
            self.current_strings_row[index])

        try:
            value =  int(value)
        except ValueError:
            raise JDBCException('Cannot convert value "%s" to an integer.' 
                % value)

        return value
    
    def getMetaData(self):
        """
        Return a ResultSetMetaData object that provides the name and datatype of each
        column in the ResultSet.
        """
        raise NotImplementedError('getMetaData')
        
    def getRow(self):
        """
        Return a Python tuple of OpenRDF Value objects.  For a SELECT query, the 
        row contains a Value for each SELECT column.  For a getStatements query,
        the row contains subject, predicate, object, and context values.

        Note: This call does NOT advance the iterator to the next row.
        """
        for i in range(self.tuple_width):
            self.getValue(i)
        return self._terms_row()
        
    def getString(self, indexOrName):
        """
        Return a string value for the column denoted by 'indexOrName'.
        For a resource-valued column, this is a URI label.  For a literal-valued
        column, this is the string representation of the literal value.

        'indexOrName' may be an integer or a column name.
        """
        index = self._tuple_index(indexOrName)
        return Statement.ntriples_string_to_value(
            self.current_strings_row[index])
    
    def getValue(self, indexOrName):
        """
        Return an OpenRDF Value (a Resource or Literal) for the column denoted by
        'indexOrName'.

        'indexOrName' may be an integer or a column name.
        """
        row = self._terms_row()
        index = self._tuple_index(indexOrName)
        term = row[index]
        if term is None:
            term = row[index] = Statement.stringTermToTerm(
                self.current_strings_row[index])
        return term

    def __iter__(self):
        self.iter_called = True
        return self

    def next(self):
        """
        Advance to the next tuple.

        If called as Python iterator, it uses the proper exceptions.
        If called directly, the function returns True if there is a next,
        and False if the iterator has been exhausted.
        """
        if self.cursor < len(self.string_tuples):
            stringTuple = self.string_tuples[self.cursor]
            self.current_strings_row = stringTuple
            self.current_terms_row = None
            self.cursor += 1
            return self if self.iter_called else True

        if self.iter_called:
            raise StopIteration()

        return False
    
    def close(self):
        """
        Currently a no-op.
        """
        pass
    
    def __len__(self):
        return len(self.string_tuples)
    
    def rowCount(self):
        return len(self)


class JDBCStatementResultSet(JDBCResultSet):
    """
    This result set may be used for quads or quints resulting from a
    getStatements call.

    If the tuple denotes a Statement (a quad), then column order is
    subject, predicate, object, context, and (optionally) id. 
    """
    QUAD = ['subject', 'predicate', 'object', 'context']
    QUINT = ['subject', 'predicate', 'object', 'context', 'id'] 

    def __init__(self, string_tuples, triple_ids=False):
        JDBCResultSet.__init__(self, string_tuples,
            self.QUINT if triple_ids else self.QUAD)
        self.normalizer = RepositoryResult.normalize_quint if triple_ids \
            else RepositoryResult.normalize_quad

    def next(self):
        retValue = JDBCResultSet.next(self)
        if retValue:
            self.current_strings_row = self.normalizer(self.current_strings_row)

        return retValue


class JDBCQueryResultSet(JDBCResultSet):
    """
    This result set may be used for rows resulting from a query.
    """
    pass
