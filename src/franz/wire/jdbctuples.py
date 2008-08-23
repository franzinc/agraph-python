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
from franz.openrdf.query.queryresultimpl import extractSelectVariableNames, strings_term_to_openrdf_term

class JDBCTuples(JDBCResultSet):
    """
    """
    def __init__(self, result_reader, query, numbering_starts_at_one=False):
        self.result_reader = result_reader
        self.query = query
        self.is_exhausted = False
        self.first_column_offset= 1 if numbering_starts_at_one else 0
        self.column_count = None
        self.column_names = None
        
    def _get_column_count(self):
        """
        Assumes that there are the same number of columns in each row.
        """
        if self.column_count is None:
            self.column_count = self.result_reader.get_term_count()
        return self.column_count
    
    def next(self):
        """
        Advance to the next tuple.  Return True if there is one, and False
        if the iterator has been exhausted.
        """
        success = self.result_reader.next_row()
        if not success:
            self.is_exhausted = True
        return success
    
    def close(self):
        """
        Shut down the iterator, to insure that resources are free'd up.
        """
        if self.socket_cursor:
            self.socket_cursor.close()
            self.socket_cursor = None

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
        if self.is_exhausted: return None
        reader = self.result_reader
        return [self.getValue(i) for i in range(0, self.getColumnCount())]
    
    def _get_index(self, key):
        """
        Convert 'key' into an integer offset if its not one already.
        Assumes that 'key' is a string or an integer.
        """
        if isinstance(key, str):
            index = None 
            names = self.getColumnNames()
            for i in range(len(names)):
                if key == names[i]:
                    index = i
                    break
            if index is None:
                raise IllegalArgumentException(("Illegal key '%s' passed to binding set." +
                            "\n   Legal keys are %s") % (key, str(names)))
        else:
            index = key - self.first_column_offset
        if index >= 0 and index < self.column_count: return index
        else:
            raise IllegalArgumentException("Out-of-bounds index passed to BindingSet." +
                                           "  Index must be between 1 and %s, inclusive." % self._get_column_count()) 

    def _get_string_term(self, key):
        index = self._get_index(key)
        return self.result_reader.get_ith_term(index)
        
    def getString(self, index):
        """
        Return a string value for the column denoted by 'index'.
        For a resource-valued column, this is a URI label.  For a literal-valued
        column, this is the string representation of the literal value.
        'index' may be an integer or a column name.
        
        Note: JOfficial DBC-style column numbering begins at one (1), not zero.
        If the tuple denotes a Statement (a quad), then 1 = subject, 2 = predicate
        3 = object, and 4 = context.  Alternatively, callers may use the constants
        JDBCResultSet.SUBJECT, JDBCResultSet.PREDICATE, JDBCResultSet.OBJECT, JDBCResultSet.CONTEXT
        as values for the 'index' argument. 
        """
        term = self._get_string_term(index)
        return term.getString()

    def getLabel(self, index):
        """
        Return the short form of a string value for the 'index'd term
        URIs return full URIs, literals return a label (i.e., they skip any
        datatype or language gloss).  Blank nodes return IDs
        """
        term = self._get_string_term(index)
        return term.getLabel()        
    
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
        term = self._get_string_term(columnName if index is None else index)
        return strings_term_to_openrdf_term(term)
    
    def getColumnNames(self):
        """
        Return the names of the columns in this query.
        """
        if self.column_names is None:
            self.column_names = extractSelectVariableNames(self.query)
        return self.column_names

    def getColumnCount(self):
        """
        Return the number of terms in the current row.
        """
        return self._get_column_count()
    
    
################################################################################################
## Debugging
################################################################################################

import datetime

from franz.wire.resultreader import *

def test_read_file(path):
    print "FIELD_DELIM", ord(FIELD_DELIMITER) #, FIELD_DELIMITER    
    print "RECORD_DELIM", ord(RECORD_DELIMITER) #, RECORD_DELIMITER
    #file = codecs.open(path, 'r', 'utf-8')
    file = open(path) 
    rr = ResultReader(file)
    tuples = JDBCTuples(rr)
    tupleCount = 0
    beginTime = datetime.datetime.now()
    while True:
        more = tuples.next()
        if not more: break
        tupleCount += 1
        i = 0
        while i < tuples.getColumnCount():
            v = tuples.getString(i)
            i += 1
        #print tuples.getRow()
    elapsedTime = datetime.datetime.now() - beginTime
    print "Retrieved %s tuples in time %s" % (tupleCount, elapsedTime)

def test1():
    paths = ["/Users/bmacgregor/Desktop/gary/swp-2e2.out",
             "/Users/bmacgregor/Desktop/gary/swp-2e3.out",
             "/Users/bmacgregor/Desktop/gary/swp-2e4.out",
#             "/Users/bmacgregor/Desktop/gary/swp-1e5.out",
#             "/Users/bmacgregor/Desktop/gary/swp-2e6.out",
             ]
    for p in paths:
        test_read_file(p)

def test2():
    ## CHANGE ENCODING TO BELL, BACKUP DELIMITERS
    for i in [2,6]:
        input = "/Users/bmacgregor/Desktop/gary/swp-1e%i.out" % i
        output = "/Users/bmacgregor/Desktop/gary/swp-2e%i.out" % i
        infile = open(input)
        outfile = open(output, 'w')
        contents = infile.read()
        bytes = []
        for c in contents:
            b = c
            if ord(b) == ord(C1_RECORD_DELIMITER):
                b = RECORD_DELIMITER
            elif ord(b) == ord(C0_FIELD_DELIMITER):
                b = FIELD_DELIMITER
            #bytes.append(b)
            outfile.write(b)
#        newContents = ''.join(bytes)
#        outfile.write(newContents)
        outfile.flush()
        outfile.close()
        print "Done translating to ", output

def test3():
    ## CREATE CUT_DOWN FILES
    input = "/Users/bmacgregor/Desktop/gary/swp-2e6.out"
    infile = open(input)
    contents = infile.read(810900)
    for pair in [(3,1000),  (5, 100000)]:
        output = "/Users/bmacgregor/Desktop/gary/swp-2e%i.out" % pair[0]
        outfile = open(output, 'w')
        pos = 0
        print "Begin", len(contents)
        for i in range(0, pair[1]):
            nextPos = contents.find(RECORD_DELIMITER, pos)
            #print "I", i, nextPos
            pos = nextPos + 1
        print "Copy to position", pos
        outfile.write(contents[0:pos])
        outfile.flush()
        outfile.close()
        print "Done creating", output

    
if __name__ == '__main__':
    choices = [i for i in range(1,17)]
    choices = [1]
    for choice in choices:
        print "\n==========================================================================="
        print "Test Run Number ", choice, "\n"
        if choice == 1: test1()
        elif choice == 2: test2()
        elif choice == 3: test3()

