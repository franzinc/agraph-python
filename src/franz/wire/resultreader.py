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
from franz.wire.jdbctuples import JDBCTuples

import codecs

class StringsDictionary:
    """
    """
    def __init__(self):
        self.namespace_dict = {}
        self.localname_dict = {}
        self.literal_dict = {}
        
    def decode_namespace(self, ns):
        """
        Return the namespace (including trailing delimiter) for
        the two-character encoding 'ns'.
        """
        namespace = self.namespace_dict.get(ns)
        return namespace or "www.nasa.gov/miasma#"

    def decode_localname(self, lcl):
        """
        Return the local name corresponding to the encoding 'lcl'
        """
        name = self.localname_dict.get(lcl)
        return name
    
    def decode_literal(self, lit):
        """
        Return the literal value corresponding to the encoding 'lit'.
        """
        lv = self.literal_dict.get(lit)
        return lv
    
    def add_namespace(self, ns, namespace):
        self.namespace_dict[ns] = namespace
        
    def add_localname(self, nm, localname):
        self.localname_dict[nm] = localname
        
    def add_literal(self, lt, literal):
        self.literal_dict[lt] = literal
        

class StringsTerm:
    """
    A strings term contains pieces of a resource or literal extracted
    from a strings message.  They can be combined, supplemented with
    dictionary lookup, to yield a complete resource or literal.
    """
    URI_TYPE = 0
    LITERAL_TYPE = 1
    TYPED_LITERAL_TYPE = 2
    LANGUAGE_LITERAL_TYPE = 3
    def __init__(self, dictionary):
        self.term_type = -1
        self.encoded_namespace = None
        self.encoded_localname = None
        self.namespace = None
        self.localname = None
        self.uri = None
        self.literal_value = None
        self.encoded_literal_value = None
        self.encoded_datatype = None
        self.language = None
        self.dictionary = dictionary
        
    def getNamespace(self):
        ns = self.namespace
        if ns: return ns
        ns = self.dictionary.decode_namespace(self.encoded_namespace)
        self.namespace = ns
        return ns
    
    def getLocalname(self):
        ln = self.localname
        if ln: return ln
        ln = self.dictionary.decode_localname(self.encoded_localname)        
        
    def getString(self):
        type = self.term_type
        if type == StringsTerm.URI_TYPE:
            uri = self.uri
            if uri: return uri
            ns = self.getNamespace()
            ln = self.getLocalname()
            ## TEMPORARY TO HELP DIAGNOSE BUG:
            if not ln:
                return None
            ## END TEMPORARY
            uri = ns + ln
            self.uri = uri
            return uri                
        elif type == StringsTerm.LITERAL_TYPE:
            lit = self.literal_value
            if lit: return lit
            lit = self.dictionary.decode_literal(self.encoded_literal_value)
            self.literal_value = lit
            return lit
        elif type == StringsTerm.TYPED_LITERAL_TYPE:
            pass
        elif type == StringsTerm.LANGUAGE_LITERAL_TYPE:
            pass        

    def __str__(self):
        if self.term_type == StringsTerm.URI_TYPE: return '|URI|' + self.getString()
        else: return '|LIT|' + self.getString()
        
    def __repr__(self):
        return self.__str__()
        
FIELD_DELIMITER = u'\uC0C1' 
FIELD_DELIMITER = u'``'
RECORD_DELIMITER = u'\uFEFF'
RECORD_DELIMITER = u'\177\177'
FIELD_DELIMITER_LENGTH = len(FIELD_DELIMITER)
RECORD_DELIMITER_LENGTH = len(RECORD_DELIMITER)

class ResultReader:
    """
    """
    def __init__(self, socket):
        self.socket = socket
        self.strings_data = None
        self.cursor = 0
        self.row_terms = []
        self.term_count = -1
        self.is_exhausted = False
        self.dictionary = StringsDictionary()
        self.socket_is_exhausted = False
        ## read in the first chunk of data
        self.read_chunk()
        
    def get_ith_term(self, column):
        """
        Return the 'column'th term in 'self.row_terms'.  Counting starts at zero.
        Extend the row if necessary.  The 'term' returned may contain
        data from a previous usage, i.e., it needs to be overwritten.
        """
        r = self.row_terms
        if len(r) <= column:
            for i in range(len(r), column + 1):
                newTerm = StringsTerm(self.dictionary)
                r.append(newTerm)
        return r[column]
    
    def get_term_count(self):
        return self.term_count
    
    def open_stream(self):
        pass
    
    def read_chunk(self):
        chunkSize = 8192
#        chunkSize = 4096
#        chunkSize = 2048        
#        chunkSize = 1024
#        chunkSize = 512
#        chunkSize = 256        
#        chunkSize = 128                
#        chunkSize = 64        
#        chunkSize = 32        
#        chunkSize = 16  
#        chunkSize = 8        
        bytes = self.socket.read(chunkSize)        
        if not bytes:
            self.strings_data = None
            self.socket_is_exhausted = True
            return
        if not isinstance(bytes, unicode):
            bytes = unicode(bytes, 'utf-8')
        self.strings_data = bytes
#        if len(bytes) < 8192:
#            print "READ CHUNK ", len(bytes)
        
    def get_straddle_string(self, partial_row):
        """
        'partial_row' needs to be concatenated with the beginning
        string in the next chuck, up to the first record delimiter.
        Also, the cursor needs to be set appropriately.
        Also, there are several corner cases.
        """
        straddleString = partial_row
        while True:
            self.read_chunk()
            nextStrings = self.strings_data
            if not nextStrings:
                return straddleString
            previousStraddleLen = len(straddleString)
            nextStringPos = nextStrings.find(RECORD_DELIMITER)
            if nextStringPos >= 0:
                ## tricky: include the delimiter so that 'straddlePos' is guaranteed to succeed:
                straddleString = straddleString + nextStrings[0:nextStringPos + RECORD_DELIMITER_LENGTH]
            else:
                nextStringPos = len(nextStrings)
                straddleString = straddleString + nextStrings
            ## need to catch the case when the RECORD_DELIMITER spans the chunk:
            straddlePos = straddleString.find(RECORD_DELIMITER)
            if straddlePos > 0:
                if straddlePos - previousStraddleLen  < nextStringPos:
                    ## tricky: it must be the case that the delimiter was split between
                    ## two chunks, or we would have picked it up on the previous iteration:
                    ## THIS IS A BUG IF THE DELIMITER LENGTH IS GREATER THAN 2:
                    self.cursor = 1
                else:
                    self.cursor = nextStringPos + RECORD_DELIMITER_LENGTH
                self.strings_data = nextStrings                    
                return straddleString[:straddlePos]
        
    def get_strings_row(self):
        """
        Return a string that contains all of the substrings for the next
        row.  Return None, if there is no next row.  If a buffer boundary
        is encountered, handle it transparently.
        """
        #if self.is_exhausted: return None
        strings = self.strings_data
        index = self.cursor
        if index >= len(strings):
            pos = -1
        else:
            pos = strings.find(RECORD_DELIMITER, index)
        if pos >= 0:
            self.cursor = pos + RECORD_DELIMITER_LENGTH
            return strings[index:pos]
        ## at this point, we usually have a partial string, with the
        ## remainder belonging to the next chunk to be read in:
        partialString = strings[index:] if index < len(strings) else ''
        strings = self.get_straddle_string(partialString)
        if strings:
            return strings
        else:
            self.is_exhausted = True
            return None
    
    def next_row(self):
        """
        Extract string pieces from 'self.strings_data' to construct the next
        row of terms.  Return 'True' if a next row of terms exists.
        """
        stringsRow = self.get_strings_row()
        if not stringsRow:
            return False
        ## extract term pieces from the fields in 'stringsRow' into
        ## a freshly-initialized list of terms:
        rowCursor = 0
        termCounter = 0
        while True:
            fieldEndPos = stringsRow.find(FIELD_DELIMITER, rowCursor)
            if fieldEndPos < 0: ## no delimiter after the last field:
                fieldEndPos = len(stringsRow) + 1 
            stringsField = stringsRow[rowCursor:fieldEndPos]
            term = self.get_ith_term(termCounter)
            termCounter += 1
            ## PHONY UNTIL GARY ADDS TO THE PROTOCOL:
            if termCounter < 3:
                term.term_type = StringsTerm.URI_TYPE
                term.encoded_namespace = 'NS'
                term.namespace = None
                term.localname = stringsField[0:len(stringsField)]
                ## TEMPORARY
                if not term.localname:
                    print "BREAK ON BUG"
                ## END TEMPORARY 
                term.uri = None
            else:
                term.term_type = StringsTerm.LITERAL_TYPE 
                term.literal_value = stringsField[0:len(stringsField)]
            rowCursor = fieldEndPos + FIELD_DELIMITER_LENGTH
            if rowCursor >= len(stringsRow):
                break
        self.term_count = termCounter
        return True
        
    
    
    
    
    
################################################################################################
## Debugging
################################################################################################

import datetime

def test_read_file(path):
    file = codecs.open(path, 'r', 'utf-8')
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
    paths = ["/Users/bmacgregor/Desktop/gary/swp-1e2.out",
             "/Users/bmacgregor/Desktop/gary/swp-1e3.out",
             "/Users/bmacgregor/Desktop/gary/swp-1e4.out",
             "/Users/bmacgregor/Desktop/gary/swp-1e5.out",
             #"/Users/bmacgregor/Desktop/gary/swp-1e6.out",
             ]
    for p in paths:
        test_read_file(p)
    
if __name__ == '__main__':
    choices = [i for i in range(1,17)]
    choices = [1]
    for choice in choices:
        print "\n==========================================================================="
        print "Test Run Number ", choice, "\n"
        if choice == 1: test1()
#        elif choice == 2: test2()
#        elif choice == 3: test3()
