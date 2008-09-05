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

import codecs

def bytes_find(bytes, target, start=None):
    """
    Like 'find', but for bytes instead of characters.  Assumes
    that 'target' is a single character wide.
    Appears to run FASTER than the build-in 'find'!!!
    """
    i = start or 0;
    target = ord(target)
    end = len(bytes)
    while i < end:
        if ord(bytes[i]) == target:
            return i
        i += 1
    return -1


NEW_NAMESPACE_ENTRY = u'!"'
NEW_LANGUAGE_ENTRY = u'!#'
NEW_DATATYPE_ENTRY = u'!$'
BARE_LITERAL_ENTRY = u'"!'
NULL_VALUE_ENTRY = u'!n'

NAMESPACE_ENTRY_TYPE = 11
DATATYPE_ENTRY_TYPE = 12
LANGUAGE_ENTRY_TYPE = 13


class CompressionDictionary(dict):
    """
    Records pairs consisting of a term type and usually, a string representing
    an uncompressed namespace, datatype or language.
    """
    def __init__(self):
        self[NEW_NAMESPACE_ENTRY] = (NAMESPACE_ENTRY_TYPE, None)
        self[NEW_LANGUAGE_ENTRY] = (DATATYPE_ENTRY_TYPE, None)
        self[NEW_DATATYPE_ENTRY] = (LANGUAGE_ENTRY_TYPE, None)    
        self[BARE_LITERAL_ENTRY] = (LITERAL_TYPE, None)
        self[NULL_VALUE_ENTRY] = (NULL_VALUE_TYPE, None)

#    def decode_localname(self, lcl):
#        """
#        Return the local name corresponding to the encoding 'lcl'
#        """
#        name = self.localname_dict.get(lcl)
#        return name
#    
         
#    def decode_namespace(self, ns):
#        """
#        Return the namespace (including trailing delimiter) for
#        the two-character encoding 'ns'.
#        """
#        namespace = self.namespace_dict.get(ns)
#        return namespace or "www.nasa.gov/miasma#"
#
#    def decode_literal(self, lit):
#        """
#        Return the literal value corresponding to the encoding 'lit'.
#        """
#        lv = self.literal_dict.get(lit)
#        return lv

        
    def process_dictionary_field(self, entry_type, string_field):
        """
        Decode 'string_field' and do what it says.
        """
        #print "PROCESS DICTIONARY FIELD", entry_type, string_field
        key = string_field[0:2]
        value = string_field[2:]
        if NAMESPACE_ENTRY_TYPE == entry_type:
            self[key] = (URI_TYPE, value)
        elif DATATYPE_ENTRY_TYPE == entry_type:
            self[key] = (TYPED_LITERAL_TYPE, value)
        elif LANGUAGE_ENTRY_TYPE == entry_type:
            self[key] = (LANGUAGE_LITERAL_TYPE, value)
        else:
            print "ERROR: Unrecognized dictionary code for string '%s'" % string_field

URI_TYPE = 1
BLANK_NODE_TYPE = 2
LITERAL_TYPE = 3
TYPED_LITERAL_TYPE = 4
LANGUAGE_LITERAL_TYPE = 5
NULL_VALUE_TYPE = 6

class StringsTerm:
    """
    A strings term contains pieces of a resource or literal extracted
    from a strings message.  They can be combined, supplemented with
    dictionary lookup, to yield a complete resource or literal.
    """
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
        self.datatype = None
        self.encoded_language = None
        self.language = None
        self.anon_id = None
        self.dictionary = dictionary
        
#    def getNamespace(self):
#        ns = self.namespace
#        if ns: return ns
#        ns = self.dictionary.decode_namespace(self.encoded_namespace)
#        self.namespace = ns
#        return ns
    
    def getLocalName(self):
        ln = self.localname
        if not ln is None: return ln
        ln = self.dictionary.decode_localname(self.encoded_localname) 

    def getString(self):
        type = self.term_type
        if type == URI_TYPE:
            uri = self.uri
            if uri: return uri
            ns = self.namespace
            ln = self.getLocalName()
            ## TEMPORARY TO HELP DIAGNOSE BUG:
            if ln is None:
                return None
            ## END TEMPORARY
            uri = ns + ln
            self.uri = uri
            return uri                
        elif type == LITERAL_TYPE:
            lit = self.literal_value
            if lit: return lit
            lit = self.dictionary.decode_literal(self.encoded_literal_value)
            self.literal_value = lit
            return lit
        elif type == TYPED_LITERAL_TYPE:
            lit = '"%s"^^<%s>' % (self.literal_value, self.datatype)
            return lit
        elif type == LANGUAGE_LITERAL_TYPE:
            return '"%s"@%s' % (self.literal_value, self.language)
        
    def getLabel(self):
        type = self.term_type
        if type == URI_TYPE:
            return self.getString()     
        elif type == LITERAL_TYPE:
            return self.getString()
        elif type == TYPED_LITERAL_TYPE:
            return self.literal_value
        elif type == LANGUAGE_LITERAL_TYPE:
            return self.literal_value
        elif type == BLANK_NODE_TYPE:
            return self.anon_id  
#        elif type == NULL_TYPE:
#            return ''

    def __str__(self):
        if self.term_type == URI_TYPE: return '|URI|' + self.getString()
        elif self.term_type == BLANK_NODE_TYPE: return '|BLANK|' + self.getString()
        elif self.term_type == NULL_VALUE_TYPE: return ''     
        else: return '|LIT|' + self.getString()
        
    def __repr__(self):
        return self.__str__()
        
C0_FIELD_DELIMITER = u'\xC0' 
#FIELD_DELIMITER = u'``'
FIELD_DELIMITER = u'\010'
C1_RECORD_DELIMITER = u'\xC1'
#RECORD_DELIMITER = u'\177\177'
RECORD_DELIMITER = u'\007'
FIELD_DELIMITER_LENGTH = len(FIELD_DELIMITER)
RECORD_DELIMITER_LENGTH = len(RECORD_DELIMITER)

class ResultReader:
    """
    """
    def __init__(self, socket, data=None):
        self.socket = socket
        self.strings_data = None
        self.cursor = 0
        self.row_terms = []
        self.term_count = -1
        self.dictionary = CompressionDictionary()
        self.socket_is_exhausted = False
        if data:
            self.strings_data = data
            self.socket_is_exhausted = True   ## tell next call to 'read_chunk' to fail
        else:
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
        if self.socket_is_exhausted:
            ## handle the 'date' parameter case 
            self.strings_data = None
            return
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
#        if not isinstance(bytes, unicode):
#            bytes = unicode(bytes, 'utf-8')
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
            nextStringPos = bytes_find(nextStrings, RECORD_DELIMITER)
            if nextStringPos >= 0:
                ## tricky: include the delimiter so that 'straddlePos' is guaranteed to succeed:
                straddleString = straddleString + nextStrings[0:nextStringPos + RECORD_DELIMITER_LENGTH]
            else:
                nextStringPos = len(nextStrings)
                straddleString = straddleString + nextStrings
            ## need to catch the case when the RECORD_DELIMITER spans the chunk:
            straddlePos = bytes_find(straddleString, RECORD_DELIMITER)
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
        row, starting at position 'self.cursor'.  Return None, if there is 
        no next row.  If a buffer boundary is encountered, handle it transparently.
        """
        strings = self.strings_data
        index = self.cursor
        if index >= len(strings):
            pos = -1
        else:
            pos = bytes_find(strings, RECORD_DELIMITER, index)
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
            self.socket_is_exhausted = True
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
            fieldEndPos = bytes_find(stringsRow, FIELD_DELIMITER, rowCursor)
            if fieldEndPos < 0: ## no delimiter after the last field:
                fieldEndPos = len(stringsRow) + 1
            rowCursorPlus2 = rowCursor + 2
            dictCode = stringsRow[rowCursor:rowCursorPlus2]
            valueField = stringsRow[rowCursorPlus2:fieldEndPos]
            dictEntry = self.dictionary.get(dictCode)
            if not dictEntry:
                print "Error: Unrecognized dictionary code in field '%s'" % dictCode + valueField
                for key, value in self.dictionary.iteritems():
                    print "DICT ENTRY", key, value
            termType = dictEntry[0]
            uncompressedString = dictEntry[1]
            term = self.get_ith_term(termCounter)
            term.term_type = termType
            termCounter += 1
            if termType == URI_TYPE:
                term.namespace = uncompressedString
                term.localname = valueField
                ## TEMPORARY
                if term.localname is None:
                    print "BREAK ON BUG"
                ## END TEMPORARY 
                term.uri = None
            elif termType == LITERAL_TYPE:
                term.literal_value = valueField
            elif termType == TYPED_LITERAL_TYPE:
                term.datatype = uncompressedString
                term.literal_value = valueField
            elif termType == LANGUAGE_LITERAL_TYPE:
                term.language = uncompressedString
                term.literal_value = valueField
            elif termType == NULL_VALUE_TYPE:
                term.literal_value = ''
            else:  ## must be a dictionary entry:
                termCounter -= 1
                self.dictionary.process_dictionary_field(termType, valueField)
            rowCursor = fieldEndPos + FIELD_DELIMITER_LENGTH
            if rowCursor >= len(stringsRow):
                break
        self.term_count = termCounter
        return True
        
    
    
    
    
    
