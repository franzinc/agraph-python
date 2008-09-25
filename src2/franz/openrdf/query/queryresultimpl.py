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
from franz.openrdf.modelimpl.valueimpl import URIImpl, BNodeImpl
from franz.openrdf.modelimpl.literalimpl import LiteralImpl
from franz.openrdf.query.query import *
from franz.openrdf.query.queryresult import *
from franz.openrdf.sail.repositoryresultimpl import RepositoryResultImpl


class GraphQueryResultImpl(RepositoryResultImpl, GraphQueryResult):
    """
    A representation of a variable-binding query result as a sequence of
    BindingSet objects. Each query result consists of zero or more
    solutions, each of which represents a single query solution as a set of
    bindings. Note: take care to always close a TupleQueryResult after use to
    free any resources it keeps hold of.
    """
    ## Method logic 100% inherited from 'RepositoryResultImpl', except for the method
    ## 'getNamespaces', which is not yet implemented.


class TupleQueryResultImpl(TupleQueryResult):
    """
    A representation of a variable-binding query result as a sequence of
    BindingSet objects. Each query result consists of zero or more
    solutions, each of which represents a single query solution as a set of
    bindings. Note: take care to always close a TupleQueryResult after use to
    free any resources it keeps hold of.
    """
    def __init__(self, upis, types, labels, mods, more, tupleWidth, token, plimit, variableNames):
        self.upis = upis
        self.types = types,
        self.labels = labels,
        self.mods = mods
        self.tupleWidth = tupleWidth
        self.variableNames = variableNames
        self.totalBindingsCount = len(upis)
        self.tupleCount = self.totalBindingsCount / tupleWidth
        self.reusableRow = [None for k in range(tupleWidth)]
        self.socket_cursor = 0
        self.term2InternalMgr = None
    
    def __iter__(self): return self
    
    def next(self):
        if self.socket_cursor >= self.totalBindingsCount:
            raise StopIteration()
        for i in range(self.tupleWidth):
            offset = self.socket_cursor
            term = self.term2InternalMgr.assembleOpenRDFValueTerm(self.upis[offset], 
                                    self.types[0][offset], self.labels[0][offset], self.mods[offset])
            self.reusableRow[i] = term
            self.socket_cursor += 1
        return DictBindingSet(self.variableNames, self.reusableRow)
    
    def setTerm2InternalManager(self, term2InternalMgr):
        self.term2InternalMgr = term2InternalMgr

    def close(self):
        pass    

    def getBindingNames(self):
        """
        Get the names of the bindings, in order of projection.
        """
        return self.variableNames

def extractSelectVariableNames(query):
    """
    Return a list of variable names from the 'select' clause in 'query'.
    """
    lcQuery = query.lower()
    selectPos = lcQuery.find("select") + len("select")
    fromPos = lcQuery.find("from")
    wherePos = lcQuery.find("where")
    endPos = fromPos if fromPos > 0 else wherePos
    selectTerms = query[selectPos:endPos].replace(' ', '').replace('\n', '')[1:] # remove leading '?'
    return selectTerms.split('?')

from franz.wire.resultreader import URI_TYPE, BLANK_NODE_TYPE, LITERAL_TYPE, TYPED_LITERAL_TYPE, LANGUAGE_LITERAL_TYPE

def strings_term_to_openrdf_term(strings_term):
    """
    Convert a string term to an OpenRDF term
    """
    sType = strings_term.term_type
    if sType == URI_TYPE:
        uri = strings_term.getString()
        return URIImpl(uri)
    elif sType == LITERAL_TYPE:
        label = strings_term.getString()
        return LiteralImpl(label)
    elif sType == TYPED_LITERAL_TYPE:
        label = strings_term.getLabel()
        return LiteralImpl(label, datatype=strings_term.datatype)
    elif sType == LANGUAGE_LITERAL_TYPE:
        label = strings_term.getLabel()
        return LiteralImpl(label, language=strings_term.language)
    elif sType == BLANK_NODE_TYPE:
        id = strings_term.getLabel()
        return BNodeImpl(id=id)
    else:
        raise Exception("Internal error: Unrecognized term_type %s in strings term %s" % (sType, strings_term))    

class CompactTupleQueryResultImpl(TupleQueryResult):
    """
    A representation of a variable-binding query result as a sequence of
    BindingSet objects. Each query result consists of zero or more
    solutions, each of which represents a single query solution as a set of
    bindings. Note: take care to always close a TupleQueryResult after use to
    free any resources it keeps hold of.
    """
    def __init__(self, result_reader, query):
        self.result_reader = result_reader
        self.is_exhausted = False
        self.variable_names = extractSelectVariableNames(query)
    
    def __iter__(self): return self
    
    def next(self):
        """
        Python-style iterator that returns a binding set on each
        successive call.
        """
        if self.is_exhausted:  ## probably should never occur 
            raise StopIteration()
        success = self.result_reader.next_row()
        if not success:
            self.is_exhausted = True
            raise StopIteration()
        return CompactDictBindingSet(self.variable_names, self.result_reader)    

    def close(self):
        pass    

    def getBindingNames(self):
        """
        Get the names of the bindings, in order of projection.
        """
        return self.variableNames
    


class DictBindingSet(dict):
    """
    A BindingSet is a set of named value bindings, which is used to
    represent a single query solution. Values are indexed by name of the binding
    which typically corresponds to the names of the variables used in the
    projection of the original query.
    
    DictBindingSet emulates a Sesame BindingSet, a Python dictionary and a list simultaneously.
    The internal datastructure is a pair of lists.  
    """
    def __init__(self, variableNames, values):
        self.variable_names = variableNames
        self.values = values
#        for i in range(len(values)):
#            self.addBindingPair(variableNames[i], values[i])
    
    def _validate_index(self, index):
        if index >= 0 and index < len(self.values): return index
        else:
            raise IllegalArgumentException("Out-of-bounds index passed to BindingSet." +
                                           "  Index must be between 1 and %s, inclusive." % len(self.values)) 
        
    def __getitem__(self, key):
        if isinstance(key, int): return self.values[self._validate_index(key)]
        else:
            for i in range(len(self.variable_names)):
                if key == self.variable_names[i]:
                    return self.values[i]
        raise IllegalArgumentException(("Illegal key '%s' passed to binding set." +
                            "\n   Legal keys are %s") % (key, str(self.variable_names)))
 
    def __len__(self):
        return len(self.values)
    
    def get(self, key):
        try:
            return self.__getitem__(key)
        except IllegalArgumentException:
            return None
        
    def addBindingPair(self, name, value):
        if value:
            self[name] = value
        
    def addBinding(self, binding):
        self.addBindingPair(binding.getName(),  binding.getValue())
        
    def removeBinding(self, name):
        try:
            del self[name]
        except:
            pass
    
    def iterator(self):
        """
        Creates an iterator over the bindings in this BindingSet. This only
        returns bindings with non-null values. An implementation is free to return
        the bindings in arbitrary order.
        
        Currently, we only support Python-style iteration over BindingSet dictionaries,
        so this (Java-style) iterator method is not implemented.
        """
        raise UnimplementedMethodException("iterator")        
    
    def getBindingNames(self):
        """
        Gets the names of the bindings in this BindingSet.

        """
        return self.iterkeys()

    def getBinding(self, bindingName):
        """
        Gets the binding with the specified name from this BindingSet.
        """
        value = self.get(bindingName)
        return BindingImpl(bindingName, value) if value else None
        
    def hasBinding(self, bindingName):
        """
        Checks whether this BindingSet has a binding with the specified name.
        """
        return self.get(bindingName) or False

    def getValue(self, bindingName):
        """
        Gets the value of the binding with the specified name from this BindingSet.
        Throws exception if 'bindingName' is not legal.
        """
        return self.__getitem__(bindingName)
    
    def getRow(self):
        """
        Return a list of strings representing the values of the current row.
        """
        return [str(self[i]) if self[i] else None for i in range(0, self.size())]

    def size(self):
        """
        Returns the number of bindings in this BindingSet.
        """
        return len(self)
        
#    def __eq__(self, other):
#        """
#        Compares a BindingSet object to another object.
#        """
#        raise UnimplementedMethodException("__eq__")

#    def __hash__(self):
#        """
#        The hash code of a binding is defined as the bit-wise XOR of the hash
#        codes of its bindings:
#        """
#        raise UnimplementedMethodException("__hash__")
    
#    def __str__(self):
#        sb = []
#        sb.append('[')
#        isFirst = False
#        for key, value in self.iteritems:
#            if isFirst: isFirst = False
#            else: sb.append(';') 
#            sb.append(key + "=" + str(value))
#        sb.append(']');
#        return ''.join(sb)

    def __dict__(self, strings_dict=False):
        """
        Return a Python dictionary representation of this binding set.
        This exists to be called by '__str__'.
        I"M NOT SURE HOW PYTHON WILL USE IT - RMM
        """
        d = {}
        for i in range(len(self.values)):
            v = self.values[i]
            if strings_dict: v = str(v)
            d[self.variable_names[i]] = v
        return d
        
    def __str__(self): return str(self.__dict__(strings_dict=True))


class CompactDictBindingSet(DictBindingSet):
    """
    A BindingSet is a set of named value bindings, which is used to
    represent a single query solution. Values are indexed by name of the binding
    which typically corresponds to the names of the variables used in the
    projection of the original query.
    
    DictBindingSet emulates a Sesame BindingSet, a Python dictionary and a list simultaneously.
    The internal datastructure is a pair of lists.  
    """
    def __init__(self, variableNames, result_reader):
        self.variable_names = variableNames
        self.result_reader = result_reader
        self.string_terms =  [None] * len(self.variable_names)
        
    def _get_ith_string_term(self, index):
        term = self.string_terms[index]
        if not term:
            term = self.result_reader.get_ith_term(index)
            self.string_terms[index] = term
        return term
                
    def _get_index(self, key):
        """
        Convert 'key' into an integer offset if its not one already.
        Assumes that 'key' is a string or an integer.
        """
        if isinstance(key, str):
            index = None 
            for i in range(len(self.variable_names)):
                if key == self.variable_names[i]:
                    index = i
                    break
            if index is None:
                raise IllegalArgumentException(("Illegal key '%s' passed to binding set." +
                            "\n   Legal keys are %s") % (key, str(self.variable_names)))
        else:
            index = key
        if index >= 0 and index < len(self.string_terms): return index
        else:
            raise IllegalArgumentException("Out-of-bounds index passed to BindingSet." +
                                           "  Index must be between 1 and %s, inclusive." % len(self.string_terms)) 
                    
    def __getitem__(self, key):
        index = self._get_index(key)
        return self._get_ith_string_term(index)
 
    def __len__(self):
        return len(self.string_terms)
    
    def get(self, key):
        try:
            return self.__getitem__(key)
        except IllegalArgumentException:
            return None
        
    def addBindingPair(self, name, value):
        if value:
            self[name] = value
        
    def addBinding(self, binding):
        self.addBindingPair(binding.getName(),  binding.getValue())
        
    def removeBinding(self, name):
        try:
            del self[name]
        except:
            pass
    
    def iterator(self):
        """
        Creates an iterator over the bindings in this BindingSet. This only
        returns bindings with non-null values. An implementation is free to return
        the bindings in arbitrary order.
        
        Currently, we only support Python-style iteration over BindingSet dictionaries,
        so this (Java-style) iterator method is not implemented.
        """
        raise UnimplementedMethodException("iterator")        
    
    def getBindingNames(self):
        """
        Gets the names of the bindings in this BindingSet.

        """
        return self.iterkeys()

    def getBinding(self, bindingName):
        """
        Gets the binding with the specified name from this BindingSet.
        """
        value = self.get(bindingName)
        return BindingImpl(bindingName, value) if value else None
        
    def hasBinding(self, bindingName):
        """
        Checks whether this BindingSet has a binding with the specified name.
        """
        return self.get(bindingName) or False
    
    def getString(self, bindingName):
        return self.__getitem__(bindingName).getString()

    def getValue(self, bindingName):
        """
        Gets the value of the binding with the specified name from this BindingSet.
        Throws exception if 'bindingName' is not legal.
        """
        stringsTerm = self.__getitem__(bindingName)
        return strings_term_to_openrdf_term(stringsTerm)
    
    def getRow(self):
        """
        Return a list of strings representing the values of the current row.
        """
        return [str(self[i]) if self[i] else None for i in range(0, self.size())]

    def size(self):
        """
        Returns the number of bindings in this BindingSet.
        """
        return len(self)
        
#    def __eq__(self, other):
#        """
#        Compares a BindingSet object to another object.
#        """
#        raise UnimplementedMethodException("__eq__")

#    def __hash__(self):
#        """
#        The hash code of a binding is defined as the bit-wise XOR of the hash
#        codes of its bindings:
#        """
#        raise UnimplementedMethodException("__hash__")
    
#    def __str__(self):
#        sb = []
#        sb.append('[')
#        isFirst = False
#        for key, value in self.iteritems:
#            if isFirst: isFirst = False
#            else: sb.append(';') 
#            sb.append(key + "=" + str(value))
#        sb.append(']');
#        return ''.join(sb)

    def __dict__(self, strings_dict=False):
        """
        Return a Python dictionary representation of this binding set.
        This exists to be called by '__str__'.
        I"M NOT SURE HOW PYTHON WILL USE IT - RMM
        """
        d = {}
        for i in range(len(self.string_terms)):
            v = self._get_ith_string_term(i)
            if strings_dict: v = str(v)
            d[self.variable_names[i]] = v
        return d
        
    def __str__(self): return str(self.__dict__(strings_dict=True))

#############################################################################
##
#############################################################################

class BindingImpl:
    """
    An implementation of 'Binding'
    """
    def __init__(self, name, value):
        self.name = name
        self.value = value
 
    def getName(self):
        """
        Gets the name of the binding (e.g. the variable name).
        """
        return self.name

    def getValue(self):
        """
        Gets the value of the binding. The returned value is never equal to
        None, such a "binding" is considered to be unbound.
        """
        return self.value
    
    def __str__(self): return self.name + "=" + str(self.value)
    
    def __eq__(self, other):
        """
        Compares a binding object to another object.
        """
        return isinstance(other, Binding) and self.name == other.getName() and self.value == other.getValue()

    def __hash__(self):
        """
        The hash code of a binding is defined as the bit-wise XOR of the hash
        codes of its name and value:
        """
        raise UnimplementedMethodException("__hash__")
    

#############################################################################
##
#############################################################################
